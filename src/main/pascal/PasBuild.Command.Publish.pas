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
    FSha256Override: string;
    FModuleRegistry: TModuleRegistry; // borrowed; nil for single-module
    function GetSourceArchivePath: string;
    function GetRegistryUrlForPublish: string;
    function WriteManifestFile(const ADir, AFileName, AContent: string): string;
    function ResolveOriginHash(const AArchivePath: string;
      out ASha256: string; out ASize: Int64): Boolean;
    function FetchExistingVersions(const ARegistryUrl: string): TStringList;
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
    class function GeneratePackageManifest(AConfig: TProjectConfig;
      AExistingVersions: TStringList = nil): string;
    class function GetRegistryManifestPath(const AName, AVersion: string): string;
    { Parse the <versions> list out of an existing registry package.xml.
      Returns an empty list (never raises) for invalid or non-XML input. }
    class function ExtractVersionsFromPackageXml(const AXml: string): TStringList;

    property ArchiveUrl: string read FArchiveUrl write FArchiveUrl;
    property RegistryId: string read FRegistryId write FRegistryId;
    property DryRun: Boolean read FDryRun write FDryRun;
    property Sha256Override: string read FSha256Override write FSha256Override;
  end;

implementation

uses
  DateUtils, DOM, XMLRead,
  PasBuild.Utils,
  PasBuild.Crypto.SHA256,
  PasBuild.Settings,
  PasBuild.Remote,
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
var
  Errors: TStringList;
begin
  Result := TBuildCommandList.Create(False);
  try
    // Skip the source-package build when validation will reject the
    // project anyway; Execute reports the validation errors
    Errors := ValidateForPublish(Config);
    try
      if Errors.Count = 0 then
        Result.Add(TSourcePackageCommand.Create(Config, ProfileIds));
    finally
      Errors.Free;
    end;
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
  Timestamp := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"',
    LocalTimeToUniversal(Now));

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

class function TPublishCommand.ExtractVersionsFromPackageXml(
  const AXml: string): TStringList;
var
  Stream: TStringStream;
  Doc: TXMLDocument;
  VersionsNode, Node: TDOMNode;
begin
  Result := TStringList.Create;
  if Trim(AXml) = '' then
    Exit;

  Doc := nil;
  Stream := TStringStream.Create(AXml);
  try
    try
      ReadXMLFile(Doc, Stream);
      if not Assigned(Doc.DocumentElement) then
        Exit;
      VersionsNode := Doc.DocumentElement.FindNode('versions');
      if not Assigned(VersionsNode) then
        Exit;
      Node := VersionsNode.FirstChild;
      while Assigned(Node) do
      begin
        if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'version') then
          Result.Add(Trim(string(Node.TextContent)));
        Node := Node.NextSibling;
      end;
    except
      // Invalid XML (e.g. an HTML error page): behave as "no versions"
    end;
  finally
    Doc.Free;
    Stream.Free;
  end;
end;

class function TPublishCommand.GeneratePackageManifest(
  AConfig: TProjectConfig; AExistingVersions: TStringList): string;
var
  Lines: TStringList;
  Versions: TStringList;
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

    // Merge the previously published versions with the current one so a
    // new release never erases the registry's version history
    Versions := TStringList.Create;
    try
      if Assigned(AExistingVersions) then
        Versions.AddStrings(AExistingVersions);
      if Versions.IndexOf(AConfig.Version) < 0 then
        Versions.Add(AConfig.Version);

      Lines.Add('');
      Lines.Add('  <versions>');
      for I := 0 to Versions.Count - 1 do
        Lines.Add('    <version>' + TUtils.XmlEscapeText(Versions[I]) + '</version>');
      Lines.Add('  </versions>');
      Lines.Add('</package>');
    finally
      Versions.Free;
    end;

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
  Settings: TSettings;
begin
  if FRegistryId <> '' then
  begin
    for I := 0 to Config.Repositories.Count - 1 do
      if Config.Repositories[I].Id = FRegistryId then
        Exit(Config.Repositories[I].Url);

    Settings := TSettingsLoader.LoadSettings(TSettingsLoader.GetDefaultPath);
    try
      for I := 0 to Settings.Repositories.Count - 1 do
        if Settings.Repositories[I].Id = FRegistryId then
          Exit(Settings.Repositories[I].Url);
    finally
      Settings.Free;
    end;

    raise EPublishError.CreateFmt(
      'Unknown registry id "%s": no matching <repository> in project.xml ' +
      'or ~/.pasbuild/settings.xml.', [FRegistryId]);
  end;

  if Config.Repositories.Count > 0 then
    Exit(Config.Repositories[0].Url);

  Settings := TSettingsLoader.LoadSettings(TSettingsLoader.GetDefaultPath);
  try
    if Settings.Repositories.Count > 0 then
      Exit(Settings.Repositories[0].Url);
  finally
    Settings.Free;
  end;

  Result := GetEnvironmentVariable('PASBUILD_REGISTRY_URL');
  if Result = '' then
    Result := DEFAULT_REGISTRY_URL;
end;

function TPublishCommand.WriteManifestFile(
  const ADir, AFileName, AContent: string): string;
var
  FS: TFileStream;
begin
  Result := IncludeTrailingPathDelimiter(ADir) + AFileName;
  ForceDirectories(ExtractFileDir(Result));
  FS := TFileStream.Create(Result, fmCreate);
  try
    if AContent <> '' then
      FS.WriteBuffer(AContent[1], Length(AContent));
  finally
    FS.Free;
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

function IsSha256Hex(const AValue: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Length(AValue) <> 64 then
    Exit;
  for I := 1 to 64 do
    if not (AValue[I] in ['0'..'9', 'a'..'f', 'A'..'F']) then
      Exit;
  Result := True;
end;

function TPublishCommand.ResolveOriginHash(const AArchivePath: string;
  out ASha256: string; out ASize: Int64): Boolean;
var
  Client: TRemoteClient;
  TempPath, LocalSha: string;
begin
  Result := False;
  LocalSha := SHA256File(AArchivePath);
  ASize := GetFileByteSize(AArchivePath);
  ASha256 := '';

  if FSha256Override <> '' then
  begin
    if not IsSha256Hex(FSha256Override) then
    begin
      TUtils.LogError('--sha256 must be 64 hexadecimal characters.');
      Exit;
    end;
    ASha256 := LowerCase(FSha256Override);
    if ASha256 <> LowerCase(LocalSha) then
      TUtils.LogWarning('--sha256 differs from the locally generated ' +
        'archive; the manifest size reflects the local archive.');
    Exit(True);
  end;

  // Consumers verify the hash against the bytes they download from the
  // origin URL, so hash the remote file - not the locally generated one,
  // which may legitimately differ (forge-generated archives, re-zips)
  TempPath := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'pasbuild-publish-verify-' + IntToStr(GetProcessID) + '.zip';
  Client := TRemoteClient.Create;
  try
    try
      TUtils.LogInfo('Downloading release archive to compute its checksum...');
      Client.DownloadFile(FArchiveUrl, TempPath);
      ASha256 := SHA256File(TempPath);
      ASize := GetFileByteSize(TempPath);
      if LowerCase(ASha256) <> LowerCase(LocalSha) then
      begin
        TUtils.LogWarning('The remote archive does not match the locally ' +
          'generated source package:');
        TUtils.LogWarning('  remote sha256: ' + LowerCase(ASha256));
        TUtils.LogWarning('  local  sha256: ' + LowerCase(LocalSha));
        TUtils.LogWarning('The manifest uses the remote hash. Verify the ' +
          'URL points at the intended release archive.');
      end;
      Result := True;
    except
      on E: Exception do
      begin
        if FDryRun then
        begin
          TUtils.LogWarning('Cannot download ' + FArchiveUrl + ': ' + E.Message);
          TUtils.LogWarning('Dry-run continues with the local archive hash; ' +
            'a real publish must be able to fetch the release archive.');
          ASha256 := LocalSha;
          Result := True;
        end
        else
        begin
          TUtils.LogError('Cannot download the release archive: ' + E.Message);
          TUtils.LogError('Fix --archive-url, or pass --sha256 <hex> if the ' +
            'file is not reachable from this machine.');
        end;
      end;
    end;
  finally
    if FileExists(TempPath) then
      DeleteFile(TempPath);
    Client.Free;
  end;
end;

function TPublishCommand.FetchExistingVersions(
  const ARegistryUrl: string): TStringList;
var
  Client: TRemoteClient;
  Base, Url, Xml: string;
begin
  Result := nil;
  Base := ARegistryUrl;
  if (Base <> '') and (Base[Length(Base)] <> '/') then
    Base := Base + '/';
  Url := Base + 'packages/' + TRegistryManager.GetNamePrefix(Config.Name) +
    '/' + Config.Name + '/package.xml';

  Client := TRemoteClient.Create;
  try
    try
      Xml := Client.FetchText(Url);
      Result := ExtractVersionsFromPackageXml(Xml);
    except
      // Not found or unreachable: treated as the first publish
      on ENetworkError do
        Result := nil;
    end;
  finally
    Client.Free;
  end;
end;

function TPublishCommand.Execute: Integer;
var
  Errors: TStringList;
  ExistingVersions: TStringList;
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

  if FArchiveUrl <> '' then
    EffectiveUrl := FArchiveUrl
  else if FDryRun then
  begin
    TUtils.LogWarning('No --archive-url given; the dry-run manifest uses a ' +
      'placeholder URL and the local archive hash.');
    EffectiveUrl := 'https://example.invalid/' + Config.Name + '-' +
      Config.Version + '-src.zip';
  end
  else
  begin
    TUtils.LogError('No --archive-url provided.');
    TUtils.LogError('Upload your source archive and re-run with:');
    TUtils.LogError('  pasbuild publish --archive-url <url-to-archive>');
    Result := 1;
    Exit;
  end;

  if FArchiveUrl = '' then
  begin
    // Dry-run placeholder: nothing to download
    Sha256 := SHA256File(ArchivePath);
    ArchiveSize := GetFileByteSize(ArchivePath);
  end
  else if not ResolveOriginHash(ArchivePath, Sha256, ArchiveSize) then
  begin
    Result := 1;
    Exit;
  end;
  TUtils.LogInfo('SHA-256: ' + Sha256);

  try
    RegistryUrl := GetRegistryUrlForPublish;
  except
    on E: EPublishError do
    begin
      TUtils.LogError(E.Message);
      Result := 1;
      Exit;
    end;
  end;

  // Merge with the registry's existing version history so publishing a
  // new release never removes earlier versions from package.xml
  ExistingVersions := FetchExistingVersions(RegistryUrl);
  try
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
    PackageManifest := GeneratePackageManifest(Config, ExistingVersions);
  finally
    ExistingVersions.Free;
  end;

  ManifestPath := GetRegistryManifestPath(Config.Name, Config.Version);
  PackagePath := 'packages/' + TRegistryManager.GetNamePrefix(Config.Name) +
    '/' + Config.Name + '/package.xml';

  // Write through the same Text handle the logger uses (StdOut, not
  // Output) so manifest bodies cannot interleave with [INFO] lines
  TUtils.LogInfo('');
  TUtils.LogInfo('Generated version manifest (' + ManifestPath + '):');
  TUtils.LogInfo('');
  WriteLn(StdOut, VersionManifest);
  Flush(StdOut);

  TUtils.LogInfo('Generated package manifest (' + PackagePath + '):');
  TUtils.LogInfo('');
  WriteLn(StdOut, PackageManifest);
  Flush(StdOut);

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
