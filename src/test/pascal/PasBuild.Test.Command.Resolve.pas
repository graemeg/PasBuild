{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Command.Resolve;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
  PasBuild.Types,
  PasBuild.Command,
  PasBuild.Command.Resolve,
  PasBuild.Repository;

type
  { TTestableResolveCommand - Exposes JSON output for test assertions }
  TTestableResolveCommand = class(TResolveCommand)
  public
    function BuildJSON: TJSONObject;
  end;

  { Tests for TResolveCommand — single-module projects }
  TTestResolveCommandSingle = class(TTestCase)
  published
    procedure TestGetNameReturnsResolve;
    procedure TestNoDependencies;
    procedure TestSingleModuleBasicFields;
    procedure TestSingleModuleWithProfiles;
    procedure TestSingleModuleWithDefines;
    procedure TestSingleModuleWithUnitPaths;
    procedure TestAutoScanModeOutOfTreeUnitPath;
    procedure TestSingleModuleWithExternalDeps;
    procedure TestSingleModuleOutputSection;
    procedure TestSingleModuleTestSection;
    procedure TestSingleModuleDirectoriesSection;
    procedure TestSingleModuleDirectoriesCustom;
    procedure TestExecuteReturnsZero;
  end;

  { Tests for TResolveCommand — multi-module aggregator projects }
  TTestResolveCommandMulti = class(TTestCase)
  private
    FTargetId: string;
    FRepo: TLocalRepository;
    function BuildThreeModuleRegistry: TModuleRegistry;
    procedure InstallMockArtifact(const AName, AVersion: string);
    procedure CleanupMockArtifact(const AName, AVersion: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMultiModuleProjectType;
    procedure TestMultiModuleBuildOrder;
    procedure TestMultiModuleModulesArray;
    procedure TestMultiModuleSelectedModule;
    procedure TestMultiModuleInvalidModule;
    procedure TestMultiModuleNoDependenciesOnAggregator;
  end;


implementation

uses
  PasBuild.Utils;

{ TTestableResolveCommand }

function TTestableResolveCommand.BuildJSON: TJSONObject;
begin
  Result := BuildResolveJSON;
end;


{ TTestResolveCommandSingle }

procedure TTestResolveCommandSingle.TestGetNameReturnsResolve;
var
  Config: TProjectConfig;
  Command: TResolveCommand;
begin
  Config := TProjectConfig.Create;
  try
    Command := TResolveCommand.Create(Config, nil);
    try
      AssertEquals('Command name should be resolve', 'resolve', Command.Name);
    finally
      Command.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestResolveCommandSingle.TestNoDependencies;
var
  Config: TProjectConfig;
  Command: TResolveCommand;
  Deps: TBuildCommandList;
begin
  Config := TProjectConfig.Create;
  try
    Command := TResolveCommand.Create(Config, nil);
    try
      Deps := Command.GetDependencies;
      try
        AssertEquals('Resolve should have no dependencies', 0, Deps.Count);
      finally
        Deps.Free;
      end;
    finally
      Command.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestResolveCommandSingle.TestSingleModuleBasicFields;
var
  Config: TProjectConfig;
  Command: TTestableResolveCommand;
  JSON: TJSONObject;
  Project: TJSONObject;
begin
  Config := TProjectConfig.Create;
  try
    Config.Name := 'my-app';
    Config.Version := '1.0.0';
    Config.BuildConfig.ProjectType := ptApplication;
    Config.BuildConfig.MainSource := 'myapp.pas';
    Config.BuildConfig.ExecutableName := 'myapp';

    Command := TTestableResolveCommand.Create(Config, nil);
    try
      JSON := Command.BuildJSON;
      try
        AssertTrue('JSON should have project object', JSON.IndexOfName('project') >= 0);
        Project := JSON.Objects['project'];
        AssertEquals('name', 'my-app', Project.Strings['name']);
        AssertEquals('version', '1.0.0', Project.Strings['version']);
        AssertEquals('projectType', 'application', Project.Strings['projectType']);
        AssertEquals('mainSource', 'myapp.pas', Project.Strings['mainSource']);
        AssertEquals('executableName', 'myapp', Project.Strings['executableName']);
      finally
        JSON.Free;
      end;
    finally
      Command.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestResolveCommandSingle.TestSingleModuleWithProfiles;
var
  Config: TProjectConfig;
  Profiles: TStringList;
  Command: TTestableResolveCommand;
  JSON: TJSONObject;
  ActiveArr, AvailArr: TJSONArray;
  Profile: TProfile;
begin
  Config := TProjectConfig.Create;
  try
    Config.Name := 'test-app';
    Config.BuildConfig.ProjectType := ptApplication;

    { Add available profiles }
    Profile := TProfile.Create;
    Profile.Id := 'debug';
    Config.Profiles.Add(Profile);

    Profile := TProfile.Create;
    Profile.Id := 'release';
    Config.Profiles.Add(Profile);

    Profile := TProfile.Create;
    Profile.Id := 'unix';
    Config.Profiles.Add(Profile);

    { Set active profiles }
    Profiles := TStringList.Create;
    try
      Profiles.Add('unix');
      Profiles.Add('debug');

      Command := TTestableResolveCommand.Create(Config, Profiles);
      try
        JSON := Command.BuildJSON;
        try
          { Check active profiles }
          AssertTrue('Should have activeProfiles', JSON.IndexOfName('activeProfiles') >= 0);
          ActiveArr := JSON.Arrays['activeProfiles'];
          AssertEquals('Two active profiles', 2, ActiveArr.Count);
          AssertEquals('First active profile', 'unix', ActiveArr.Strings[0]);
          AssertEquals('Second active profile', 'debug', ActiveArr.Strings[1]);

          { Check available profiles }
          AssertTrue('Should have availableProfiles', JSON.IndexOfName('availableProfiles') >= 0);
          AvailArr := JSON.Arrays['availableProfiles'];
          AssertEquals('Three available profiles', 3, AvailArr.Count);
        finally
          JSON.Free;
        end;
      finally
        Command.Free;
      end;
    finally
      Profiles.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestResolveCommandSingle.TestSingleModuleWithDefines;
var
  Config: TProjectConfig;
  Profiles: TStringList;
  Command: TTestableResolveCommand;
  JSON: TJSONObject;
  DefinesArr: TJSONArray;
  Profile: TProfile;
  I: Integer;
  HasDebug, HasX11: Boolean;
begin
  Config := TProjectConfig.Create;
  try
    Config.Name := 'test-app';
    Config.BuildConfig.ProjectType := ptApplication;
    Config.BuildConfig.Defines.Add('X11');

    { Add a profile with defines }
    Profile := TProfile.Create;
    Profile.Id := 'debug';
    Profile.Defines.Add('DEBUG');
    Config.Profiles.Add(Profile);

    Profiles := TStringList.Create;
    try
      Profiles.Add('debug');

      Command := TTestableResolveCommand.Create(Config, Profiles);
      try
        JSON := Command.BuildJSON;
        try
          AssertTrue('Should have defines', JSON.IndexOfName('defines') >= 0);
          DefinesArr := JSON.Arrays['defines'];
          AssertTrue('Should have at least 2 defines', DefinesArr.Count >= 2);

          { Check both global and profile defines are present }
          HasDebug := False;
          HasX11 := False;
          for I := 0 to DefinesArr.Count - 1 do
          begin
            if DefinesArr.Strings[I] = 'DEBUG' then HasDebug := True;
            if DefinesArr.Strings[I] = 'X11' then HasX11 := True;
          end;
          AssertTrue('Should contain DEBUG define', HasDebug);
          AssertTrue('Should contain X11 define', HasX11);
        finally
          JSON.Free;
        end;
      finally
        Command.Free;
      end;
    finally
      Profiles.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestResolveCommandSingle.TestSingleModuleWithUnitPaths;
var
  Config: TProjectConfig;
  Command: TTestableResolveCommand;
  JSON: TJSONObject;
  UnitArr: TJSONArray;
begin
  Config := TProjectConfig.Create;
  try
    Config.Name := 'test-app';
    Config.BuildConfig.ProjectType := ptApplication;
    Config.BuildConfig.ManualUnitPaths := True;
    Config.BuildConfig.UnitPaths.Add(TConditionalPath.Create('src/main/pascal'));
    Config.BuildConfig.UnitPaths.Add(TConditionalPath.Create('src/main/pascal/3rdparty'));

    Command := TTestableResolveCommand.Create(Config, nil);
    try
      JSON := Command.BuildJSON;
      try
        AssertTrue('Should have unitPaths', JSON.IndexOfName('unitPaths') >= 0);
        UnitArr := JSON.Arrays['unitPaths'];
        AssertTrue('Should have at least 2 unit paths', UnitArr.Count >= 2);
      finally
        JSON.Free;
      end;
    finally
      Command.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestResolveCommandSingle.TestAutoScanModeOutOfTreeUnitPath;
var
  Config: TProjectConfig;
  Command: TTestableResolveCommand;
  JSON: TJSONObject;
  UnitArr: TJSONArray;
  I: Integer;
  Found: Boolean;
  OutOfTreePath: string;
begin
  OutOfTreePath := '../../framework/src/main/pascal/corelib';

  Config := TProjectConfig.Create;
  try
    Config.Name := 'test-app';
    Config.BuildConfig.ProjectType := ptApplication;
    { Auto-scan mode (ManualUnitPaths = False) with an out-of-tree path }
    Config.BuildConfig.UnitPaths.Add(TConditionalPath.Create(OutOfTreePath));

    Command := TTestableResolveCommand.Create(Config, nil);
    try
      JSON := Command.BuildJSON;
      try
        AssertTrue('Should have unitPaths', JSON.IndexOfName('unitPaths') >= 0);
        UnitArr := JSON.Arrays['unitPaths'];

        Found := False;
        for I := 0 to UnitArr.Count - 1 do
          if UnitArr.Strings[I] = OutOfTreePath then
          begin
            Found := True;
            Break;
          end;

        AssertTrue(
          'Out-of-tree path must appear in unitPaths without sourceDirectory prefix',
          Found
        );
      finally
        JSON.Free;
      end;
    finally
      Command.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestResolveCommandSingle.TestSingleModuleWithExternalDeps;
var
  Config: TProjectConfig;
  Command: TTestableResolveCommand;
  JSON: TJSONObject;
  DepsArr: TJSONArray;
  Dep: TJSONObject;
begin
  Config := TProjectConfig.Create;
  try
    Config.Name := 'test-app';
    Config.BuildConfig.ProjectType := ptApplication;
    Config.Dependencies.Add(TDependencyInfo.Create('lib-alpha', '1.0.0'));
    Config.Dependencies.Add(TDependencyInfo.Create('lib-beta', '3.2.1'));

    Command := TTestableResolveCommand.Create(Config, nil);
    try
      JSON := Command.BuildJSON;
      try
        AssertTrue('Should have dependencies', JSON.IndexOfName('dependencies') >= 0);
        DepsArr := JSON.Arrays['dependencies'];
        AssertEquals('Should have 2 dependencies', 2, DepsArr.Count);

        Dep := DepsArr.Objects[0];
        AssertEquals('First dep name', 'lib-alpha', Dep.Strings['name']);
        AssertEquals('First dep version', '1.0.0', Dep.Strings['version']);

        Dep := DepsArr.Objects[1];
        AssertEquals('Second dep name', 'lib-beta', Dep.Strings['name']);
        AssertEquals('Second dep version', '3.2.1', Dep.Strings['version']);
      finally
        JSON.Free;
      end;
    finally
      Command.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestResolveCommandSingle.TestSingleModuleOutputSection;
var
  Config: TProjectConfig;
  Command: TTestableResolveCommand;
  JSON: TJSONObject;
  Output: TJSONObject;
begin
  Config := TProjectConfig.Create;
  try
    Config.Name := 'test-app';
    Config.BuildConfig.ProjectType := ptApplication;
    Config.BuildConfig.OutputDirectory := 'target';
    Config.BuildConfig.ExecutableName := 'myapp';

    Command := TTestableResolveCommand.Create(Config, nil);
    try
      JSON := Command.BuildJSON;
      try
        AssertTrue('Should have output section', JSON.IndexOfName('output') >= 0);
        Output := JSON.Objects['output'];
        AssertTrue('Output should have directory', Output.IndexOfName('directory') >= 0);
        AssertTrue('Output should have unitDirectory', Output.IndexOfName('unitDirectory') >= 0);
      finally
        JSON.Free;
      end;
    finally
      Command.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestResolveCommandSingle.TestSingleModuleTestSection;
var
  Config: TProjectConfig;
  Command: TTestableResolveCommand;
  JSON: TJSONObject;
  TestObj: TJSONObject;
begin
  Config := TProjectConfig.Create;
  try
    Config.Name := 'test-app';
    Config.BuildConfig.ProjectType := ptApplication;
    Config.TestConfig.TestSource := 'TestRunner.pas';

    Command := TTestableResolveCommand.Create(Config, nil);
    try
      JSON := Command.BuildJSON;
      try
        AssertTrue('Should have test section', JSON.IndexOfName('test') >= 0);
        TestObj := JSON.Objects['test'];
        AssertEquals('Test source', 'TestRunner.pas', TestObj.Strings['testSource']);
      finally
        JSON.Free;
      end;
    finally
      Command.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestResolveCommandSingle.TestSingleModuleDirectoriesSection;
var
  Config: TProjectConfig;
  Command: TTestableResolveCommand;
  JSON: TJSONObject;
  DirsObj: TJSONObject;
begin
  Config := TProjectConfig.Create;
  try
    Config.Name := 'test-app';
    Config.BuildConfig.ProjectType := ptApplication;

    Command := TTestableResolveCommand.Create(Config, nil);
    try
      JSON := Command.BuildJSON;
      try
        AssertTrue('Should have directories section', JSON.IndexOfName('directories') >= 0);
        DirsObj := JSON.Objects['directories'];
        AssertEquals('Default source', 'src/main/pascal', DirsObj.Strings['source']);
        AssertEquals('Default testSource', 'src/test/pascal', DirsObj.Strings['testSource']);
        AssertEquals('Default resources', 'src/main/resources', DirsObj.Strings['resources']);
        AssertEquals('Default testResources', 'src/test/resources', DirsObj.Strings['testResources']);
      finally
        JSON.Free;
      end;
    finally
      Command.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestResolveCommandSingle.TestSingleModuleDirectoriesCustom;
var
  Config: TProjectConfig;
  Command: TTestableResolveCommand;
  JSON: TJSONObject;
  DirsObj: TJSONObject;
begin
  Config := TProjectConfig.Create;
  try
    Config.Name := 'custom-dirs';
    Config.BuildConfig.ProjectType := ptApplication;
    Config.BuildConfig.SourceDirectory := '.';
    Config.TestConfig.SourceDirectory := 'test';
    Config.ResourcesConfig.Directory := 'res';
    Config.TestResourcesConfig.Directory := 'test-res';

    Command := TTestableResolveCommand.Create(Config, nil);
    try
      JSON := Command.BuildJSON;
      try
        AssertTrue('Should have directories section', JSON.IndexOfName('directories') >= 0);
        DirsObj := JSON.Objects['directories'];
        AssertEquals('Custom source', '.', DirsObj.Strings['source']);
        AssertEquals('Custom testSource', 'test', DirsObj.Strings['testSource']);
        AssertEquals('Custom resources', 'res', DirsObj.Strings['resources']);
        AssertEquals('Custom testResources', 'test-res', DirsObj.Strings['testResources']);
      finally
        JSON.Free;
      end;
    finally
      Command.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestResolveCommandSingle.TestExecuteReturnsZero;
var
  Config: TProjectConfig;
  Command: TResolveCommand;
  ExitCode: Integer;
begin
  Config := TProjectConfig.Create;
  try
    Config.Name := 'test-app';
    Config.BuildConfig.ProjectType := ptApplication;

    Command := TResolveCommand.Create(Config, nil);
    try
      ExitCode := Command.Execute;
      AssertEquals('Execute should return 0', 0, ExitCode);
    finally
      Command.Free;
    end;
  finally
    Config.Free;
  end;
end;


{ TTestResolveCommandMulti }

procedure TTestResolveCommandMulti.InstallMockArtifact(const AName, AVersion: string);
var
  Metadata: TArtifactMetadata;
  DepArray: TMetadataDependencyArray;
  UnitsDir, DummyFile: string;
  F: TextFile;
begin
  FRepo.EnsureDirectoryStructure(AName, AVersion, FTargetId);
  UnitsDir := FRepo.GetUnitsPath(AName, AVersion, FTargetId);
  DummyFile := IncludeTrailingPathDelimiter(UnitsDir) + AName + '.ppu';
  AssignFile(F, DummyFile);
  Rewrite(F);
  WriteLn(F, '// dummy ppu');
  CloseFile(F);

  SetLength(DepArray, 0);
  Metadata := TArtifactMetadata.Create;
  try
    Metadata.Name := AName;
    Metadata.Version := AVersion;
    Metadata.Packaging := 'library';
    Metadata.Compiler := 'fpc';
    Metadata.CompilerVersion := '3.2.2';
    Metadata.TargetCPU := 'x86_64';
    Metadata.TargetOS := 'linux';
    Metadata.Timestamp := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now);
    Metadata.Dependencies := DepArray;
    FRepo.WriteMetadata(AName, AVersion, FTargetId, Metadata);
  finally
    Metadata.Free;
  end;
end;

procedure TTestResolveCommandMulti.CleanupMockArtifact(const AName, AVersion: string);
var
  ArtifactDir, UnitsDir: string;
begin
  ArtifactDir := FRepo.GetArtifactPath(AName, AVersion, FTargetId);
  if not DirectoryExists(ArtifactDir) then
    Exit;
  { Remove units directory contents and directory }
  UnitsDir := FRepo.GetUnitsPath(AName, AVersion, FTargetId);
  DeleteFile(IncludeTrailingPathDelimiter(UnitsDir) + AName + '.ppu');
  RemoveDir(UnitsDir);
  { Remove metadata and artifact directory }
  DeleteFile(FRepo.GetMetadataPath(AName, AVersion, FTargetId));
  RemoveDir(ArtifactDir);
end;

procedure TTestResolveCommandMulti.SetUp;
begin
  inherited SetUp;
  FTargetId := TUtils.GetTargetQuadruplet;
  FRepo := TLocalRepository.Create;
  InstallMockArtifact('some-ext', '2.0.0');
end;

procedure TTestResolveCommandMulti.TearDown;
begin
  CleanupMockArtifact('some-ext', '2.0.0');
  FRepo.Free;
  inherited TearDown;
end;

function TTestResolveCommandMulti.BuildThreeModuleRegistry: TModuleRegistry;
var
  Registry: TModuleRegistry;
  Module: TModuleInfo;
  Config: TProjectConfig;
begin
  Registry := TModuleRegistry.Create;

  { lib-core: library, no deps }
  Config := TProjectConfig.Create;
  Config.Name := 'lib-core';
  Config.Version := '1.0.0';
  Config.BuildConfig.ProjectType := ptLibrary;
  Config.BuildConfig.SourceDirectory := 'src/main/pascal';
  Config.BuildConfig.OutputDirectory := 'target';
  Module := TModuleInfo.Create;
  Module.Name := 'lib-core';
  Module.Path := '/tmp/test-lib-core';
  Module.Config := Config;
  Registry.RegisterModule(Module);

  { lib-ui: library, depends on lib-core }
  Config := TProjectConfig.Create;
  Config.Name := 'lib-ui';
  Config.Version := '1.0.0';
  Config.BuildConfig.ProjectType := ptLibrary;
  Config.BuildConfig.SourceDirectory := 'src/main/pascal';
  Config.BuildConfig.OutputDirectory := 'target';
  Module := TModuleInfo.Create;
  Module.Name := 'lib-ui';
  Module.Path := '/tmp/test-lib-ui';
  Module.Config := Config;
  Module.Dependencies.Add('lib-core');
  Registry.RegisterModule(Module);

  { my-app: application, depends on lib-ui + external dep }
  Config := TProjectConfig.Create;
  Config.Name := 'my-app';
  Config.Version := '1.0.0';
  Config.BuildConfig.ProjectType := ptApplication;
  Config.BuildConfig.MainSource := 'myapp.pas';
  Config.BuildConfig.ExecutableName := 'myapp';
  Config.BuildConfig.SourceDirectory := 'src/main/pascal';
  Config.BuildConfig.OutputDirectory := 'target';
  Config.Dependencies.Add(TDependencyInfo.Create('some-ext', '2.0.0'));
  Module := TModuleInfo.Create;
  Module.Name := 'my-app';
  Module.Path := '/tmp/test-my-app';
  Module.Config := Config;
  Module.Dependencies.Add('lib-ui');
  Registry.RegisterModule(Module);

  Result := Registry;
end;

procedure TTestResolveCommandMulti.TestMultiModuleProjectType;
var
  AggConfig: TProjectConfig;
  Registry: TModuleRegistry;
  Command: TTestableResolveCommand;
  JSON: TJSONObject;
begin
  AggConfig := TProjectConfig.Create;
  Registry := BuildThreeModuleRegistry;
  try
    AggConfig.Name := 'my-project';
    AggConfig.Version := '1.0.0';
    AggConfig.BuildConfig.ProjectType := ptPom;

    Command := TTestableResolveCommand.CreateMultiModule(AggConfig, nil, Registry);
    try
      JSON := Command.BuildJSON;
      try
        AssertEquals('projectType should be pom',
          'pom', JSON.Objects['project'].Strings['projectType']);
      finally
        JSON.Free;
      end;
    finally
      Command.Free;
    end;
  finally
    Registry.Free;
    AggConfig.Free;
  end;
end;

procedure TTestResolveCommandMulti.TestMultiModuleBuildOrder;
var
  AggConfig: TProjectConfig;
  Registry: TModuleRegistry;
  Command: TTestableResolveCommand;
  JSON: TJSONObject;
  BuildOrder: TJSONArray;
begin
  AggConfig := TProjectConfig.Create;
  Registry := BuildThreeModuleRegistry;
  try
    AggConfig.Name := 'my-project';
    AggConfig.Version := '1.0.0';
    AggConfig.BuildConfig.ProjectType := ptPom;

    Command := TTestableResolveCommand.CreateMultiModule(AggConfig, nil, Registry);
    try
      JSON := Command.BuildJSON;
      try
        AssertTrue('Should have buildOrder', JSON.IndexOfName('buildOrder') >= 0);
        BuildOrder := JSON.Arrays['buildOrder'];
        AssertEquals('Should have 3 modules in build order', 3, BuildOrder.Count);
        { lib-core first (no deps), then lib-ui, then my-app }
        AssertEquals('First in build order', 'lib-core', BuildOrder.Strings[0]);
        AssertEquals('Second in build order', 'lib-ui', BuildOrder.Strings[1]);
        AssertEquals('Third in build order', 'my-app', BuildOrder.Strings[2]);
      finally
        JSON.Free;
      end;
    finally
      Command.Free;
    end;
  finally
    Registry.Free;
    AggConfig.Free;
  end;
end;

procedure TTestResolveCommandMulti.TestMultiModuleModulesArray;
var
  AggConfig: TProjectConfig;
  Registry: TModuleRegistry;
  Command: TTestableResolveCommand;
  JSON: TJSONObject;
  ModulesArr: TJSONArray;
begin
  AggConfig := TProjectConfig.Create;
  Registry := BuildThreeModuleRegistry;
  try
    AggConfig.Name := 'my-project';
    AggConfig.Version := '1.0.0';
    AggConfig.BuildConfig.ProjectType := ptPom;

    Command := TTestableResolveCommand.CreateMultiModule(AggConfig, nil, Registry);
    try
      JSON := Command.BuildJSON;
      try
        AssertTrue('Should have modules array', JSON.IndexOfName('modules') >= 0);
        ModulesArr := JSON.Arrays['modules'];
        AssertEquals('Should have 3 modules', 3, ModulesArr.Count);

        { Each module should have name and projectType }
        AssertEquals('First module name', 'lib-core',
          ModulesArr.Objects[0].Strings['name']);
        AssertEquals('First module type', 'library',
          ModulesArr.Objects[0].Strings['projectType']);

        AssertEquals('Last module name', 'my-app',
          ModulesArr.Objects[2].Strings['name']);
        AssertEquals('Last module type', 'application',
          ModulesArr.Objects[2].Strings['projectType']);
      finally
        JSON.Free;
      end;
    finally
      Command.Free;
    end;
  finally
    Registry.Free;
    AggConfig.Free;
  end;
end;

procedure TTestResolveCommandMulti.TestMultiModuleSelectedModule;
var
  AggConfig: TProjectConfig;
  Registry: TModuleRegistry;
  Command: TTestableResolveCommand;
  JSON: TJSONObject;
begin
  AggConfig := TProjectConfig.Create;
  Registry := BuildThreeModuleRegistry;
  try
    AggConfig.Name := 'my-project';
    AggConfig.Version := '1.0.0';
    AggConfig.BuildConfig.ProjectType := ptPom;

    Command := TTestableResolveCommand.CreateMultiModule(AggConfig, nil, Registry, 'my-app');
    try
      JSON := Command.BuildJSON;
      try
        { When a specific module is selected, output should be single-module format }
        AssertEquals('Selected module projectType', 'application',
          JSON.Objects['project'].Strings['projectType']);
        AssertEquals('Selected module name', 'my-app',
          JSON.Objects['project'].Strings['name']);
        { Should NOT have modules array in single-module output }
        AssertTrue('Should not have modules array',
          JSON.IndexOfName('modules') < 0);
      finally
        JSON.Free;
      end;
    finally
      Command.Free;
    end;
  finally
    Registry.Free;
    AggConfig.Free;
  end;
end;

procedure TTestResolveCommandMulti.TestMultiModuleInvalidModule;
var
  AggConfig: TProjectConfig;
  Registry: TModuleRegistry;
  Command: TResolveCommand;
  ExitCode: Integer;
begin
  AggConfig := TProjectConfig.Create;
  Registry := BuildThreeModuleRegistry;
  try
    AggConfig.Name := 'my-project';
    AggConfig.Version := '1.0.0';
    AggConfig.BuildConfig.ProjectType := ptPom;

    Command := TResolveCommand.CreateMultiModule(AggConfig, nil, Registry, 'nonexistent');
    try
      ExitCode := Command.Execute;
      AssertEquals('Invalid module should return 1', 1, ExitCode);
    finally
      Command.Free;
    end;
  finally
    Registry.Free;
    AggConfig.Free;
  end;
end;

procedure TTestResolveCommandMulti.TestMultiModuleNoDependenciesOnAggregator;
var
  AggConfig: TProjectConfig;
  Registry: TModuleRegistry;
  Command: TTestableResolveCommand;
  JSON: TJSONObject;
begin
  AggConfig := TProjectConfig.Create;
  Registry := BuildThreeModuleRegistry;
  try
    AggConfig.Name := 'my-project';
    AggConfig.Version := '1.0.0';
    AggConfig.BuildConfig.ProjectType := ptPom;

    Command := TTestableResolveCommand.CreateMultiModule(AggConfig, nil, Registry);
    try
      JSON := Command.BuildJSON;
      try
        { Aggregator itself should not have compiler or unitPaths at top level }
        AssertTrue('Aggregator should not have compiler',
          JSON.IndexOfName('compiler') < 0);
        AssertTrue('Aggregator should not have unitPaths',
          JSON.IndexOfName('unitPaths') < 0);
      finally
        JSON.Free;
      end;
    finally
      Command.Free;
    end;
  finally
    Registry.Free;
    AggConfig.Free;
  end;
end;


initialization
  RegisterTest(TTestResolveCommandSingle);
  RegisterTest(TTestResolveCommandMulti);

end.
