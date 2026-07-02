{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Command.Publish;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  PasBuild.Types,
  PasBuild.Command;

type
  { Raised for publish-specific configuration problems (module lookup,
    unresolvable module dependencies, ...) }
  EPublishError = class(Exception);

  TPublishCommand = class(TBuildCommand)
  private
    FArchiveUrl: string;
    FRegistryId: string;
    FDryRun: Boolean;
    FModuleRegistry: TModuleRegistry; // borrowed; nil for single-module
    function GetSourceArchivePath: string;
    function GetRegistryUrlForPublish: string;
    function WriteManifestFile(const ADir, AFileName, AContent: string): string;
  protected
    function GetName: string; override;
  public
    constructor Create(AConfig: TProjectConfig; AProfileIds: TStringList;
      const AArchiveUrl, ARegistryId: string; ADryRun: Boolean);
    { Publish a library module of a multi-module project. Changes the
      current directory to the module directory so the source-package
      dependency and all relative output paths operate on the module. }
    constructor CreateForModule(AProfileIds: TStringList;
      ARegistry: TModuleRegistry;
      const AModuleName, AArchiveUrl, ARegistryId: string; ADryRun: Boolean);

    function Execute: Integer; override;
    function GetDependencies: TBuildCommandList; override;

    class function ValidateForPublish(AConfig: TProjectConfig): TStringList;
    class function GenerateVersionManifest(AConfig: TProjectConfig;
      const AArchiveUrl, ASha256: string; ASize: Int64;
      ARegistry: TModuleRegistry = nil): string;
    class function GeneratePackageManifest(AConfig: TProjectConfig): string;
    class function GetRegistryManifestPath(const AName, AVersion: string): string;

    property ArchiveUrl: string read FArchiveUrl write FArchiveUrl;
    property RegistryId: string read FRegistryId write FRegistryId;
    property DryRun: Boolean read FDryRun write FDryRun;
  end;

implementation

uses
  PasBuild.Utils,
  PasBuild.Crypto.SHA256,
  PasBuild.Remote.Registry,
  PasBuild.Remote.Forge,
  PasBuild.Command.SourcePackage;

{ TPublishCommand }

constructor TPublishCommand.Create(AConfig: TProjectConfig;
  AProfileIds: TStringList; const AArchiveUrl, ARegistryId: string;
  ADryRun: Boolean);
begin
  inherited Create(AConfig, AProfileIds);
  FArchiveUrl := AArchiveUrl;
  FRegistryId := ARegistryId;
  FDryRun := ADryRun;
  FModuleRegistry := nil;
end;

constructor TPublishCommand.CreateForModule(AProfileIds: TStringList;
  ARegistry: TModuleRegistry;
  const AModuleName, AArchiveUrl, ARegistryId: string; ADryRun: Boolean);
var
  Module: TModuleInfo;
begin
  Module := ARegistry.FindModuleByName(AModuleName);
  if not Assigned(Module) then
    raise EPublishError.CreateFmt('Module not found: %s', [AModuleName]);

  if Module.Config.BuildConfig.ProjectType <> ptLibrary then
  begin
    case Module.Config.BuildConfig.ProjectType of
      ptApplication: raise EPublishError.CreateFmt(
        'Only library modules can be published. Module "%s" has packaging: application.',
        [AModuleName]);
      ptPom: raise EPublishError.CreateFmt(
        'Only library modules can be published. Module "%s" has packaging: pom.',
        [AModuleName]);
    else
      raise EPublishError.CreateFmt(
        'Only library modules can be published. Module "%s" is not a library.',
        [AModuleName]);
    end;
  end;

  // The source-package dependency and every relative path (target/,
  // target/publish/) resolve against the current directory; without this
  // the aggregator's tree would be archived under the module's name
  if not SetCurrentDir(Module.Path) then
    raise EPublishError.CreateFmt(
      'Cannot change to module directory: %s', [Module.Path]);
  TUtils.LogInfo('Publishing module ' + AModuleName + ' from ' + Module.Path);

  inherited Create(Module.Config, AProfileIds);
  FArchiveUrl := AArchiveUrl;
  FRegistryId := ARegistryId;
  FDryRun := ADryRun;
  FModuleRegistry := ARegistry;
end;

function TPublishCommand.GetName: string;
begin
  Result := 'publish';
end;

function TPublishCommand.GetDependencies: TBuildCommandList;
begin
  Result := TBuildCommandList.Create(False);
  try
    Result.Add(TSourcePackageCommand.Create(Config, ProfileIds));
  except
    Result.Free;
    raise;
  end;
end;

class function TPublishCommand.ValidateForPublish(
  AConfig: TProjectConfig): TStringList;
begin
  Result := TStringList.Create;

  if AConfig.BuildConfig.ProjectType <> ptLibrary then
  begin
    case AConfig.BuildConfig.ProjectType of
      ptApplication: Result.Add('Only library projects can be published. Current packaging: application.');
      ptPom: Result.Add('Only library projects can be published. Current packaging: pom.');
    else
      Result.Add('Only library projects can be published.');
    end;
  end;

  if AConfig.Name = '' then
    Result.Add('Project name is required for publishing.');

  if AConfig.Version = '' then
    Result.Add('Project version is required for publishing.');

  if AConfig.Author = '' then
    Result.Add('Project author is required for publishing.');

  if AConfig.License = '' then
    Result.Add('Project license is required for publishing.');
end;

class function TPublishCommand.GetRegistryManifestPath(
  const AName, AVersion: string): string;
begin
  Result := 'packages/' + TRegistryManager.GetNamePrefix(AName) + '/' +
    AName + '/' + AVersion + '.xml';
end;

class function TPublishCommand.GenerateVersionManifest(
  AConfig: TProjectConfig; const AArchiveUrl, ASha256: string;
  ASize: Int64; ARegistry: TModuleRegistry): string;
var
  Lines: TStringList;
  DepNames, DepVersions: TStringList;
  Module: TModuleInfo;
  I: Integer;
  Timestamp: string;
begin
  Timestamp := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', Now);

  // Collect the full dependency list up front: external registry
  // dependencies plus inter-module dependencies translated to their
  // published package names and versions
  DepNames := TStringList.Create;
  DepVersions := TStringList.Create;
  try
    for I := 0 to AConfig.Dependencies.Count - 1 do
    begin
      DepNames.Add(AConfig.Dependencies[I].Name);
      DepVersions.Add(AConfig.Dependencies[I].Version);
    end;

    for I := 0 to AConfig.ModuleDependencies.Count - 1 do
    begin
      if not Assigned(ARegistry) then
        raise EPublishError.CreateFmt(
          'Cannot publish "%s": it declares module dependency "%s" but no ' +
          'module registry is available to resolve it.',
          [AConfig.Name, AConfig.ModuleDependencies[I]]);

      Module := ARegistry.FindModuleByName(AConfig.ModuleDependencies[I]);
      if not Assigned(Module) then
        raise EPublishError.CreateFmt(
          'Cannot publish "%s": module dependency "%s" was not found in ' +
          'this project.',
          [AConfig.Name, AConfig.ModuleDependencies[I]]);

      if Module.Config.BuildConfig.ProjectType <> ptLibrary then
        raise EPublishError.CreateFmt(
          'Cannot publish "%s": module dependency "%s" is not a library ' +
          'module, so consumers could never resolve it from a registry.',
          [AConfig.Name, AConfig.ModuleDependencies[I]]);

      DepNames.Add(Module.Config.Name);
      DepVersions.Add(Module.Config.Version);
    end;

    Lines := TStringList.Create;
    try
      Lines.Add('<?xml version="1.0" encoding="UTF-8"?>');
      Lines.Add('<release>');
      Lines.Add('  <name>' + TUtils.XmlEscapeText(AConfig.Name) + '</name>');
      Lines.Add('  <version>' + TUtils.XmlEscapeText(AConfig.Version) + '</version>');
      Lines.Add('  <author>' + TUtils.XmlEscapeText(AConfig.Author) + '</author>');
      Lines.Add('  <license>' + TUtils.XmlEscapeText(AConfig.License) + '</license>');

      if AConfig.Description <> '' then
        Lines.Add('  <description>' + TUtils.XmlEscapeText(AConfig.Description) + '</description>');

      Lines.Add('');
      Lines.Add('  <origin>');
      Lines.Add('    <url>' + TUtils.XmlEscapeText(AArchiveUrl) + '</url>');
      Lines.Add('    <sha256>' + ASha256 + '</sha256>');
      Lines.Add('    <size>' + IntToStr(ASize) + '</size>');
      Lines.Add('  </origin>');

      Lines.Add('');
      Lines.Add('  <compilers>');
      Lines.Add('    <compiler>');
      Lines.Add('      <name>' + TUtils.DetectCompilerName + '</name>');
      Lines.Add('      <minimumVersion>' + TUtils.DetectCompilerVersion + '</minimumVersion>');
      Lines.Add('    </compiler>');
      Lines.Add('  </compilers>');

      Lines.Add('');
      if DepNames.Count > 0 then
      begin
        Lines.Add('  <dependencies>');
        for I := 0 to DepNames.Count - 1 do
        begin
          Lines.Add('    <dependency>');
          Lines.Add('      <name>' + TUtils.XmlEscapeText(DepNames[I]) + '</name>');
          Lines.Add('      <version>' + TUtils.XmlEscapeText(DepVersions[I]) + '</version>');
          Lines.Add('    </dependency>');
        end;
        Lines.Add('  </dependencies>');
      end
      else
        Lines.Add('  <dependencies/>');

      Lines.Add('');
      Lines.Add('  <publishedAt>' + Timestamp + '</publishedAt>');
      Lines.Add('</release>');

      Result := Lines.Text;
    finally
      Lines.Free;
    end;
  finally
    DepVersions.Free;
    DepNames.Free;
  end;
end;

class function TPublishCommand.GeneratePackageManifest(
  AConfig: TProjectConfig): string;
var
  Lines: TStringList;
  I: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('<?xml version="1.0" encoding="UTF-8"?>');
    Lines.Add('<package>');
    Lines.Add('  <name>' + TUtils.XmlEscapeText(AConfig.Name) + '</name>');

    if AConfig.Description <> '' then
      Lines.Add('  <description>' + TUtils.XmlEscapeText(AConfig.Description) + '</description>');

    Lines.Add('  <author>' + TUtils.XmlEscapeText(AConfig.Author) + '</author>');
    Lines.Add('  <license>' + TUtils.XmlEscapeText(AConfig.License) + '</license>');

    if AConfig.Keywords.Count > 0 then
    begin
      Lines.Add('');
      Lines.Add('  <keywords>');
      for I := 0 to AConfig.Keywords.Count - 1 do
        Lines.Add('    <keyword>' + TUtils.XmlEscapeText(AConfig.Keywords[I]) + '</keyword>');
      Lines.Add('  </keywords>');
    end;

    Lines.Add('');
    Lines.Add('  <versions>');
    Lines.Add('    <version>' + TUtils.XmlEscapeText(AConfig.Version) + '</version>');
    Lines.Add('  </versions>');
    Lines.Add('</package>');

    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function TPublishCommand.GetSourceArchivePath: string;
var
  BaseName, OutputDir: string;
begin
  OutputDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory);
  BaseName := Config.Name;
  if BaseName = '' then
    BaseName := 'project';
  Result := OutputDir + DirectorySeparator + BaseName + '-' +
    Config.Version + '-src.zip';
end;

function TPublishCommand.GetRegistryUrlForPublish: string;
var
  I: Integer;
begin
  if (FRegistryId <> '') and (Config.Repositories.Count > 0) then
  begin
    for I := 0 to Config.Repositories.Count - 1 do
      if Config.Repositories[I].Id = FRegistryId then
        Exit(Config.Repositories[I].Url);
  end;

  if Config.Repositories.Count > 0 then
    Exit(Config.Repositories[0].Url);

  Result := GetEnvironmentVariable('PASBUILD_REGISTRY_URL');
  if Result = '' then
    Result := DEFAULT_REGISTRY_URL;
end;

function TPublishCommand.WriteManifestFile(
  const ADir, AFileName, AContent: string): string;
var
  F: TextFile;
begin
  Result := IncludeTrailingPathDelimiter(ADir) + AFileName;
  ForceDirectories(ExtractFileDir(Result));
  AssignFile(F, Result);
  Rewrite(F);
  try
    Write(F, AContent);
  finally
    CloseFile(F);
  end;
end;

function GetFileByteSize(const AFilePath: string): Int64;
var
  FS: TFileStream;
begin
  try
    FS := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyNone);
    try
      Result := FS.Size;
    finally
      FS.Free;
    end;
  except
    Result := 0;
  end;
end;

function TPublishCommand.Execute: Integer;
var
  Errors: TStringList;
  I: Integer;
  ArchivePath, EffectiveUrl, Sha256: string;
  ArchiveSize: Int64;
  VersionManifest, PackageManifest: string;
  OutputDir, ManifestPath, PackagePath: string;
  VersionFilePath, PackageFilePath: string;
  Forge: TForgeType;
  RegistryUrl, Token, BranchName, PrUrl: string;
  Owner, Repo: string;
  GitHub: TGitHubForge;
begin
  Result := 0;

  TUtils.LogInfo('Publishing package...');

  Errors := ValidateForPublish(Config);
  try
    if Errors.Count > 0 then
    begin
      for I := 0 to Errors.Count - 1 do
        TUtils.LogError(Errors[I]);
      Result := 1;
      Exit;
    end;
  finally
    Errors.Free;
  end;

  ArchivePath := GetSourceArchivePath;
  if not FileExists(ArchivePath) then
  begin
    TUtils.LogError('Source archive not found: ' + ArchivePath);
    TUtils.LogError('Run "pasbuild source-package" first.');
    Result := 1;
    Exit;
  end;

  TUtils.LogInfo('Source archive: ' + ArchivePath);

  Sha256 := SHA256File(ArchivePath);
  TUtils.LogInfo('SHA-256: ' + Sha256);

  ArchiveSize := GetFileByteSize(ArchivePath);

  if FArchiveUrl <> '' then
    EffectiveUrl := FArchiveUrl
  else
  begin
    TUtils.LogError('No --archive-url provided.');
    TUtils.LogError('Upload your source archive and re-run with:');
    TUtils.LogError('  pasbuild publish --archive-url <url-to-archive>');
    Result := 1;
    Exit;
  end;

  try
    VersionManifest := GenerateVersionManifest(Config, EffectiveUrl, Sha256,
      ArchiveSize, FModuleRegistry);
  except
    on E: EPublishError do
    begin
      TUtils.LogError(E.Message);
      Result := 1;
      Exit;
    end;
  end;
  PackageManifest := GeneratePackageManifest(Config);

  ManifestPath := GetRegistryManifestPath(Config.Name, Config.Version);
  PackagePath := 'packages/' + TRegistryManager.GetNamePrefix(Config.Name) +
    '/' + Config.Name + '/package.xml';

  TUtils.LogInfo('');
  TUtils.LogInfo('Generated version manifest (' + ManifestPath + '):');
  TUtils.LogInfo('');
  WriteLn(VersionManifest);

  TUtils.LogInfo('Generated package manifest (' + PackagePath + '):');
  TUtils.LogInfo('');
  WriteLn(PackageManifest);

  OutputDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory) +
    DirectorySeparator + 'publish';

  VersionFilePath := WriteManifestFile(OutputDir, ManifestPath,
    VersionManifest);
  PackageFilePath := WriteManifestFile(OutputDir, PackagePath,
    PackageManifest);

  TUtils.LogInfo('Manifest files written to: ' + OutputDir);
  TUtils.LogInfo('  ' + VersionFilePath);
  TUtils.LogInfo('  ' + PackageFilePath);

  if FDryRun then
  begin
    TUtils.LogInfo('');
    TUtils.LogInfo('Dry-run complete. Manifest files generated but not submitted.');
    TUtils.LogInfo('To submit, copy the manifest files to your registry repository');
    TUtils.LogInfo('and open a pull request.');
    Exit;
  end;

  RegistryUrl := GetRegistryUrlForPublish;
  Forge := TForgeDetector.DetectForge(RegistryUrl);
  Token := TForgeDetector.LookupToken(Forge);
  BranchName := TForgeDetector.GetPublishBranchName(Config.Name,
    Config.Version);

  if (Forge = ftGitHub) and (Token <> '') then
  begin
    TForgeDetector.ParseGitHubRepo(RegistryUrl, Owner, Repo);
    if (Owner = '') or (Repo = '') then
    begin
      TUtils.LogError('Cannot parse owner/repo from registry URL: ' +
        RegistryUrl);
      Result := 1;
      Exit;
    end;

    TUtils.LogInfo('');
    TUtils.LogInfo('Submitting to GitHub registry: ' + Owner + '/' + Repo);

    GitHub := TGitHubForge.Create(Token, Owner, Repo);
    try
      TUtils.LogInfo('  Forking repository...');
      if not GitHub.ForkRepository then
      begin
        TUtils.LogError('Could not fork the registry repository.');
        Result := 1;
        Exit;
      end;

      TUtils.LogInfo('  Creating branch: ' + BranchName);
      if not GitHub.CreateBranch(BranchName) then
      begin
        Result := 1;
        Exit;
      end;

      TUtils.LogInfo('  Uploading version manifest...');
      if not GitHub.CreateOrUpdateFile(BranchName, ManifestPath,
        VersionManifest,
        'publish: ' + Config.Name + ' ' + Config.Version) then
      begin
        Result := 1;
        Exit;
      end;

      TUtils.LogInfo('  Uploading package manifest...');
      if not GitHub.CreateOrUpdateFile(BranchName, PackagePath,
        PackageManifest,
        'publish: update package.xml for ' + Config.Name) then
      begin
        Result := 1;
        Exit;
      end;

      TUtils.LogInfo('  Creating pull request...');
      PrUrl := GitHub.CreatePullRequest(BranchName,
        'publish: ' + Config.Name + ' ' + Config.Version,
        'Add ' + Config.Name + ' version ' + Config.Version +
        ' to the registry.' + LineEnding + LineEnding +
        'Generated by `pasbuild publish`.');

      if PrUrl <> '' then
      begin
        TUtils.LogInfo('');
        TUtils.LogInfo('Pull request created: ' + PrUrl);
      end
      else
      begin
        TUtils.LogError('Could not create the pull request. The manifest ' +
          'files are on branch "' + BranchName + '" of your fork; you can ' +
          'open the pull request manually.');
        Result := 1;
        Exit;
      end;
    finally
      GitHub.Free;
    end;
  end
  else
  begin
    TUtils.LogInfo('');
    if (Forge <> ftUnknown) and (Token = '') then
    begin
      TUtils.LogInfo('No authentication token found.');
      TUtils.LogInfo('Set ' + TForgeDetector.GetTokenEnvVar(Forge) +
        ' to enable automated submission.');
      TUtils.LogInfo('');
    end;
    TUtils.LogInfo('Manifest files generated. To submit to the registry:');
    TUtils.LogInfo('  1. Fork the registry repository');
    TUtils.LogInfo('  2. Copy the files from ' + OutputDir + ' to the fork');
    TUtils.LogInfo('  3. Create branch: ' + BranchName);
    TUtils.LogInfo('  4. Commit and open a pull request');
  end;
end;

end.
