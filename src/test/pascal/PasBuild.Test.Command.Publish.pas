{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Command.Publish;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Types,
  PasBuild.CLI,
  PasBuild.Command.Publish;

type
  TTestPublishGoalEnum = class(TTestCase)
  published
    procedure TestBgPublishExists;
    procedure TestGoalFromStringPublish;
  end;

  TTestPublishCLIFlags = class(TTestCase)
  published
    procedure TestArchiveUrlFieldExists;
    procedure TestRegistryIdFieldExists;
    procedure TestDryRunFieldExists;
    procedure TestArchiveUrlDefaultEmpty;
    procedure TestRegistryIdDefaultEmpty;
    procedure TestDryRunDefaultFalse;
  end;

  TTestPublishValidation = class(TTestCase)
  published
    procedure TestRejectsApplicationPackaging;
    procedure TestRejectsPomPackaging;
    procedure TestRejectsMissingName;
    procedure TestRejectsMissingVersion;
    procedure TestRejectsMissingAuthor;
    procedure TestRejectsMissingLicense;
    procedure TestAcceptsLibraryPackaging;
  end;

  TTestPublishManifestGeneration = class(TTestCase)
  private
    FTempDir: string;
    function CreateLibraryConfig: TProjectConfig;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateVersionManifestXml;
    procedure TestGenerateVersionManifestIncludesDependencies;
    procedure TestGeneratePackageManifestXml;
    procedure TestGeneratePackageManifestIncludesKeywords;
    procedure TestManifestPathUsesNamePrefix;
  end;

  { Publishing a module out of a multi-module aggregator must operate in
    the module's own directory and translate its moduleDependencies into
    registry dependencies }
  TTestPublishForModule = class(TTestCase)
  private
    FTempDir: string;
    FSavedDir: string;
    FRegistry: TModuleRegistry;
    function AddModule(const AModuleName, APackageName, AVersion: string;
      AType: TProjectType): TModuleInfo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMissingModuleRaises;
    procedure TestNonLibraryModuleRaises;
    procedure TestUsesModuleConfig;
    procedure TestChangesToModuleDirectory;
    procedure TestVersionManifestIncludesModuleDependencies;
    procedure TestVersionManifestModuleDependencyMissingRaises;
    procedure TestVersionManifestNonLibraryModuleDependencyRaises;
  end;

implementation

function DeleteDirRecursive(const ADirName: string): Boolean;
var
  SearchRec: TSearchRec;
  FilePath: string;
begin
  Result := True;
  if FindFirst(IncludeTrailingPathDelimiter(ADirName) + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          FilePath := IncludeTrailingPathDelimiter(ADirName) + SearchRec.Name;
          if (SearchRec.Attr and faDirectory) = faDirectory then
          begin
            if not DeleteDirRecursive(FilePath) then
              Result := False;
          end
          else
          begin
            if not DeleteFile(FilePath) then
              Result := False;
          end;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
  if Result then
    Result := RemoveDir(ADirName);
end;

{ TTestPublishGoalEnum }

procedure TTestPublishGoalEnum.TestBgPublishExists;
var
  G: TBuildGoal;
begin
  G := bgPublish;
  AssertTrue('bgPublish should be a valid goal', G = bgPublish);
end;

procedure TTestPublishGoalEnum.TestGoalFromStringPublish;
begin
  AssertTrue('publish string maps to bgPublish',
    TArgumentParser.GoalFromString('publish') = bgPublish);
end;

{ TTestPublishCLIFlags }

procedure TTestPublishCLIFlags.TestArchiveUrlFieldExists;
var
  Args: TCommandLineArgs;
begin
  Args.ArchiveUrl := 'https://example.com/archive.zip';
  AssertEquals('ArchiveUrl field', 'https://example.com/archive.zip',
    Args.ArchiveUrl);
end;

procedure TTestPublishCLIFlags.TestRegistryIdFieldExists;
var
  Args: TCommandLineArgs;
begin
  Args.RegistryId := 'myrepo';
  AssertEquals('RegistryId field', 'myrepo', Args.RegistryId);
end;

procedure TTestPublishCLIFlags.TestDryRunFieldExists;
var
  Args: TCommandLineArgs;
begin
  Args.DryRun := True;
  AssertTrue('DryRun field', Args.DryRun);
end;

procedure TTestPublishCLIFlags.TestArchiveUrlDefaultEmpty;
var
  Args: TCommandLineArgs;
begin
  Args := TArgumentParser.ParseArguments;
  try
    AssertEquals('ArchiveUrl default empty', '', Args.ArchiveUrl);
  finally
    Args.ProfileIds.Free;
  end;
end;

procedure TTestPublishCLIFlags.TestRegistryIdDefaultEmpty;
var
  Args: TCommandLineArgs;
begin
  Args := TArgumentParser.ParseArguments;
  try
    AssertEquals('RegistryId default empty', '', Args.RegistryId);
  finally
    Args.ProfileIds.Free;
  end;
end;

procedure TTestPublishCLIFlags.TestDryRunDefaultFalse;
var
  Args: TCommandLineArgs;
begin
  Args := TArgumentParser.ParseArguments;
  try
    AssertFalse('DryRun default false', Args.DryRun);
  finally
    Args.ProfileIds.Free;
  end;
end;

{ TTestPublishValidation }

procedure TTestPublishValidation.TestRejectsApplicationPackaging;
var
  Config: TProjectConfig;
  Errors: TStringList;
begin
  Config := TProjectConfig.Create;
  try
    Config.Name := 'myapp';
    Config.Version := '1.0.0';
    Config.Author := 'Test';
    Config.License := 'MIT';
    Config.BuildConfig.ProjectType := ptApplication;

    Errors := TPublishCommand.ValidateForPublish(Config);
    try
      AssertTrue('Should have errors', Errors.Count > 0);
      AssertTrue('Error mentions library',
        Pos('library', Errors[0]) > 0);
    finally
      Errors.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestPublishValidation.TestRejectsPomPackaging;
var
  Config: TProjectConfig;
  Errors: TStringList;
begin
  Config := TProjectConfig.Create;
  try
    Config.Name := 'myaggregator';
    Config.Version := '1.0.0';
    Config.Author := 'Test';
    Config.License := 'MIT';
    Config.BuildConfig.ProjectType := ptPom;

    Errors := TPublishCommand.ValidateForPublish(Config);
    try
      AssertTrue('Should have errors', Errors.Count > 0);
      AssertTrue('Error mentions library',
        Pos('library', Errors[0]) > 0);
    finally
      Errors.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestPublishValidation.TestRejectsMissingName;
var
  Config: TProjectConfig;
  Errors: TStringList;
begin
  Config := TProjectConfig.Create;
  try
    Config.Version := '1.0.0';
    Config.Author := 'Test';
    Config.License := 'MIT';
    Config.BuildConfig.ProjectType := ptLibrary;

    Errors := TPublishCommand.ValidateForPublish(Config);
    try
      AssertTrue('Should have errors', Errors.Count > 0);
      AssertTrue('Error mentions name',
        Pos('name', LowerCase(Errors.Text)) > 0);
    finally
      Errors.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestPublishValidation.TestRejectsMissingVersion;
var
  Config: TProjectConfig;
  Errors: TStringList;
begin
  Config := TProjectConfig.Create;
  try
    Config.Name := 'mylib';
    Config.Author := 'Test';
    Config.License := 'MIT';
    Config.BuildConfig.ProjectType := ptLibrary;

    Errors := TPublishCommand.ValidateForPublish(Config);
    try
      AssertTrue('Should have errors', Errors.Count > 0);
      AssertTrue('Error mentions version',
        Pos('version', LowerCase(Errors.Text)) > 0);
    finally
      Errors.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestPublishValidation.TestRejectsMissingAuthor;
var
  Config: TProjectConfig;
  Errors: TStringList;
begin
  Config := TProjectConfig.Create;
  try
    Config.Name := 'mylib';
    Config.Version := '1.0.0';
    Config.Author := '';
    Config.License := 'MIT';
    Config.BuildConfig.ProjectType := ptLibrary;

    Errors := TPublishCommand.ValidateForPublish(Config);
    try
      AssertTrue('Should have errors', Errors.Count > 0);
      AssertTrue('Error mentions author',
        Pos('author', LowerCase(Errors.Text)) > 0);
    finally
      Errors.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestPublishValidation.TestRejectsMissingLicense;
var
  Config: TProjectConfig;
  Errors: TStringList;
begin
  Config := TProjectConfig.Create;
  try
    Config.Name := 'mylib';
    Config.Version := '1.0.0';
    Config.Author := 'Test';
    Config.License := '';
    Config.BuildConfig.ProjectType := ptLibrary;

    Errors := TPublishCommand.ValidateForPublish(Config);
    try
      AssertTrue('Should have errors', Errors.Count > 0);
      AssertTrue('Error mentions license',
        Pos('license', LowerCase(Errors.Text)) > 0);
    finally
      Errors.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestPublishValidation.TestAcceptsLibraryPackaging;
var
  Config: TProjectConfig;
  Errors: TStringList;
begin
  Config := TProjectConfig.Create;
  try
    Config.Name := 'mylib';
    Config.Version := '1.0.0';
    Config.Author := 'Test Author';
    Config.License := 'MIT';
    Config.BuildConfig.ProjectType := ptLibrary;

    Errors := TPublishCommand.ValidateForPublish(Config);
    try
      AssertEquals('Should have no errors', 0, Errors.Count);
    finally
      Errors.Free;
    end;
  finally
    Config.Free;
  end;
end;

{ TTestPublishManifestGeneration }

procedure TTestPublishManifestGeneration.SetUp;
begin
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'pasbuild-test-publish-' + IntToStr(GetProcessID);
  ForceDirectories(FTempDir);
end;

procedure TTestPublishManifestGeneration.TearDown;
begin
  if DirectoryExists(FTempDir) then
    DeleteDirRecursive(FTempDir);
end;

function TTestPublishManifestGeneration.CreateLibraryConfig: TProjectConfig;
begin
  Result := TProjectConfig.Create;
  Result.Name := 'string-utils';
  Result.Version := '1.0.0';
  Result.Author := 'Test Author';
  Result.License := 'MIT';
  Result.Description := 'Common string utilities';
  Result.BuildConfig.ProjectType := ptLibrary;
end;

procedure TTestPublishManifestGeneration.TestGenerateVersionManifestXml;
var
  Config: TProjectConfig;
  Xml: string;
begin
  Config := CreateLibraryConfig;
  try
    Xml := TPublishCommand.GenerateVersionManifest(Config,
      'https://example.com/string-utils-1.0.0-src.zip',
      'abc123def456', 12480);

    AssertTrue('Contains release root', Pos('<release>', Xml) > 0);
    AssertTrue('Contains name', Pos('<name>string-utils</name>', Xml) > 0);
    AssertTrue('Contains version', Pos('<version>1.0.0</version>', Xml) > 0);
    AssertTrue('Contains author', Pos('<author>Test Author</author>', Xml) > 0);
    AssertTrue('Contains license', Pos('<license>MIT</license>', Xml) > 0);
    AssertTrue('Contains origin url', Pos('<url>https://example.com/string-utils-1.0.0-src.zip</url>', Xml) > 0);
    AssertTrue('Contains sha256', Pos('<sha256>abc123def456</sha256>', Xml) > 0);
    AssertTrue('Contains size', Pos('<size>12480</size>', Xml) > 0);
    AssertTrue('Contains publishedAt', Pos('<publishedAt>', Xml) > 0);
  finally
    Config.Free;
  end;
end;

procedure TTestPublishManifestGeneration.TestGenerateVersionManifestIncludesDependencies;
var
  Config: TProjectConfig;
  Dep: TDependencyInfo;
  Xml: string;
begin
  Config := CreateLibraryConfig;
  try
    Dep := TDependencyInfo.Create('base-lib', '2.0.0');
    Config.Dependencies.Add(Dep);

    Xml := TPublishCommand.GenerateVersionManifest(Config,
      'https://example.com/archive.zip', 'sha256hash', 1000);

    AssertTrue('Contains dependencies section',
      Pos('<dependencies>', Xml) > 0);
    AssertTrue('Contains dep name',
      Pos('<name>base-lib</name>', Xml) > 0);
    AssertTrue('Contains dep version',
      Pos('<version>2.0.0</version>', Xml) > 0);
  finally
    Config.Free;
  end;
end;

procedure TTestPublishManifestGeneration.TestGeneratePackageManifestXml;
var
  Config: TProjectConfig;
  Xml: string;
begin
  Config := CreateLibraryConfig;
  try
    Xml := TPublishCommand.GeneratePackageManifest(Config);

    AssertTrue('Contains package root', Pos('<package>', Xml) > 0);
    AssertTrue('Contains name',
      Pos('<name>string-utils</name>', Xml) > 0);
    AssertTrue('Contains description',
      Pos('<description>Common string utilities</description>', Xml) > 0);
    AssertTrue('Contains author',
      Pos('<author>Test Author</author>', Xml) > 0);
    AssertTrue('Contains license',
      Pos('<license>MIT</license>', Xml) > 0);
    AssertTrue('Contains versions section',
      Pos('<versions>', Xml) > 0);
    AssertTrue('Contains version entry',
      Pos('<version>1.0.0</version>', Xml) > 0);
  finally
    Config.Free;
  end;
end;

procedure TTestPublishManifestGeneration.TestGeneratePackageManifestIncludesKeywords;
var
  Config: TProjectConfig;
  Xml: string;
begin
  Config := CreateLibraryConfig;
  try
    Config.Keywords.Add('string');
    Config.Keywords.Add('utilities');

    Xml := TPublishCommand.GeneratePackageManifest(Config);

    AssertTrue('Contains keywords section',
      Pos('<keywords>', Xml) > 0);
    AssertTrue('Contains keyword string',
      Pos('<keyword>string</keyword>', Xml) > 0);
    AssertTrue('Contains keyword utilities',
      Pos('<keyword>utilities</keyword>', Xml) > 0);
  finally
    Config.Free;
  end;
end;

procedure TTestPublishManifestGeneration.TestManifestPathUsesNamePrefix;
var
  Path: string;
begin
  Path := TPublishCommand.GetRegistryManifestPath('string-utils', '1.0.0');
  AssertEquals('Manifest path with prefix',
    'packages/st/string-utils/1.0.0.xml', Path);
end;

{ TTestPublishForModule }

function TTestPublishForModule.AddModule(const AModuleName, APackageName,
  AVersion: string; AType: TProjectType): TModuleInfo;
var
  Config: TProjectConfig;
begin
  Result := TModuleInfo.Create;
  Result.Name := AModuleName;
  Result.Path := IncludeTrailingPathDelimiter(FTempDir) + AModuleName;
  ForceDirectories(Result.Path);

  Config := TProjectConfig.Create;
  Config.Name := APackageName;
  Config.Version := AVersion;
  Config.BuildConfig.ProjectType := AType;
  Result.Config := Config;

  FRegistry.RegisterModule(Result);
end;

procedure TTestPublishForModule.SetUp;
begin
  FSavedDir := GetCurrentDir;
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'pasbuild-test-pubmod-' + IntToStr(GetProcessID);
  ForceDirectories(FTempDir);
  FRegistry := TModuleRegistry.Create;
end;

procedure TTestPublishForModule.TearDown;
begin
  SetCurrentDir(FSavedDir);
  FRegistry.Free;
  if DirectoryExists(FTempDir) then
    DeleteDirRecursive(FTempDir);
end;

procedure TTestPublishForModule.TestMissingModuleRaises;
var
  Raised: Boolean;
  Cmd: TPublishCommand;
begin
  Raised := False;
  Cmd := nil;
  try
    Cmd := TPublishCommand.CreateForModule(nil, FRegistry,
      'does-not-exist', '', '', True);
  except
    on E: Exception do
    begin
      Raised := True;
      AssertTrue('Error should name the module',
        Pos('does-not-exist', E.Message) > 0);
    end;
  end;
  Cmd.Free;
  AssertTrue('Missing module should raise', Raised);
end;

procedure TTestPublishForModule.TestNonLibraryModuleRaises;
var
  Raised: Boolean;
  Cmd: TPublishCommand;
begin
  AddModule('my-app', 'my-app', '1.0.0', ptApplication);

  Raised := False;
  Cmd := nil;
  try
    Cmd := TPublishCommand.CreateForModule(nil, FRegistry,
      'my-app', '', '', True);
  except
    on E: Exception do
    begin
      Raised := True;
      AssertTrue('Error should mention packaging',
        Pos('application', LowerCase(E.Message)) > 0);
    end;
  end;
  Cmd.Free;
  AssertTrue('Non-library module should raise', Raised);
end;

procedure TTestPublishForModule.TestUsesModuleConfig;
var
  Module: TModuleInfo;
  Cmd: TPublishCommand;
begin
  Module := AddModule('my-lib', 'my-lib-pkg', '2.5.0', ptLibrary);

  Cmd := TPublishCommand.CreateForModule(nil, FRegistry,
    'my-lib', 'https://example.org/a.zip', '', True);
  try
    AssertSame('Command must use the module''s own config',
      Module.Config, Cmd.Config);
  finally
    Cmd.Free;
  end;
end;

procedure TTestPublishForModule.TestChangesToModuleDirectory;
var
  Module: TModuleInfo;
  Cmd: TPublishCommand;
begin
  // The source-package dependency and all relative output paths resolve
  // against the current directory, so publish must run in the module dir
  Module := AddModule('my-lib', 'my-lib-pkg', '2.5.0', ptLibrary);

  Cmd := TPublishCommand.CreateForModule(nil, FRegistry,
    'my-lib', 'https://example.org/a.zip', '', True);
  try
    AssertEquals('Working directory must be the module directory',
      ExcludeTrailingPathDelimiter(Module.Path),
      ExcludeTrailingPathDelimiter(GetCurrentDir));
  finally
    Cmd.Free;
  end;
end;

procedure TTestPublishForModule.TestVersionManifestIncludesModuleDependencies;
var
  LibModule: TModuleInfo;
  Manifest: string;
begin
  AddModule('lib-core', 'core-pkg', '3.1.0', ptLibrary);
  LibModule := AddModule('lib-gui', 'gui-pkg', '3.1.0', ptLibrary);
  LibModule.Config.ModuleDependencies.Add('lib-core');

  Manifest := TPublishCommand.GenerateVersionManifest(LibModule.Config,
    'https://example.org/gui.zip', 'abc123', 100, FRegistry);

  AssertTrue('Manifest should declare the sibling package by name',
    Pos('<name>core-pkg</name>', Manifest) > 0);
  AssertTrue('Manifest should declare the sibling package version',
    Pos('<version>3.1.0</version>', Manifest) > 0);
end;

procedure TTestPublishForModule.TestVersionManifestModuleDependencyMissingRaises;
var
  LibModule: TModuleInfo;
  Raised: Boolean;
begin
  LibModule := AddModule('lib-gui', 'gui-pkg', '3.1.0', ptLibrary);
  LibModule.Config.ModuleDependencies.Add('lib-vanished');

  Raised := False;
  try
    TPublishCommand.GenerateVersionManifest(LibModule.Config,
      'https://example.org/gui.zip', 'abc123', 100, FRegistry);
  except
    on E: Exception do
    begin
      Raised := True;
      AssertTrue('Error should name the missing module',
        Pos('lib-vanished', E.Message) > 0);
    end;
  end;
  AssertTrue('Unresolvable module dependency should raise', Raised);
end;

procedure TTestPublishForModule.TestVersionManifestNonLibraryModuleDependencyRaises;
var
  LibModule: TModuleInfo;
  Raised: Boolean;
begin
  // An application sibling can never be resolved from a registry, so a
  // manifest referencing it would be unresolvable for consumers
  AddModule('some-app', 'some-app', '3.1.0', ptApplication);
  LibModule := AddModule('lib-gui', 'gui-pkg', '3.1.0', ptLibrary);
  LibModule.Config.ModuleDependencies.Add('some-app');

  Raised := False;
  try
    TPublishCommand.GenerateVersionManifest(LibModule.Config,
      'https://example.org/gui.zip', 'abc123', 100, FRegistry);
  except
    on E: Exception do
      Raised := True;
  end;
  AssertTrue('Non-library module dependency should raise', Raised);
end;

initialization
  RegisterTest(TTestPublishGoalEnum);
  RegisterTest(TTestPublishCLIFlags);
  RegisterTest(TTestPublishValidation);
  RegisterTest(TTestPublishManifestGeneration);
  RegisterTest(TTestPublishForModule);

end.
