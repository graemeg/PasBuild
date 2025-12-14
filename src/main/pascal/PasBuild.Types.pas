{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Types;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl, contnrs;

type
  { Project type enumeration }
  TProjectType = (ptApplication, ptLibrary, ptPom);

  { Test framework enumeration }
  TTestFramework = (tfAuto, tfFPCUnit, tfFPTest);

  { Forward declarations }
  TProfile = class;
  TBuildConfig = class;
  TTestConfig = class;
  TResourcesConfig = class;
  TSourcePackageConfig = class;
  TProjectConfig = class;
  TConditionalPath = class;
  TModuleInfo = class;
  TModuleRegistry = class;

  { TConditionalPath - Represents a path with an optional condition }
  TConditionalPath = class
  private
    FPath: string;
    FCondition: string;
  public
    constructor Create(const APath: string; const ACondition: string = '');

    property Path: string read FPath write FPath;
    property Condition: string read FCondition write FCondition;
  end;

  { TConditionalPathList - Strongly-typed collection of conditional paths }
  TConditionalPathList = class(specialize TFPGObjectList<TConditionalPath>)
  end;

  { TProfile - Represents a build profile with defines and compiler options }
  TProfile = class
  private
    FId: string;
    FDefines: TStringList;
    FCompilerOptions: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    property Id: string read FId write FId;
    property Defines: TStringList read FDefines;
    property CompilerOptions: TStringList read FCompilerOptions;
  end;

  { TProfileList - Strongly-typed collection of build profiles using generics }
  TProfileList = class(specialize TFPGObjectList<TProfile>)
  public
    function FindById(const AId: string): TProfile;
  end;

  { TBuildConfig - Build configuration section }
  TBuildConfig = class
  private
    FProjectType: TProjectType;
    FMainSource: string;
    FOutputDirectory: string;
    FExecutableName: string;
    FDefines: TStringList;
    FCompilerOptions: TStringList;
    FUnitPaths: TConditionalPathList;
    FIncludePaths: TConditionalPathList;
    FManualUnitPaths: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property ProjectType: TProjectType read FProjectType write FProjectType;
    property MainSource: string read FMainSource write FMainSource;
    property OutputDirectory: string read FOutputDirectory write FOutputDirectory;
    property ExecutableName: string read FExecutableName write FExecutableName;
    property Defines: TStringList read FDefines;
    property CompilerOptions: TStringList read FCompilerOptions;
    property UnitPaths: TConditionalPathList read FUnitPaths;
    property IncludePaths: TConditionalPathList read FIncludePaths;
    property ManualUnitPaths: Boolean read FManualUnitPaths write FManualUnitPaths;
  end;

  { TTestConfig - Test configuration section }
  TTestConfig = class
  private
    FFramework: TTestFramework;
    FTestSource: string;
    FFrameworkOptions: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    property Framework: TTestFramework read FFramework write FFramework;
    property TestSource: string read FTestSource write FTestSource;
    property FrameworkOptions: TStringList read FFrameworkOptions;
  end;

  { TResourcesConfig - Resources configuration section }
  TResourcesConfig = class
  private
    FDirectory: string;
    FFiltering: Boolean;
  public
    constructor Create;

    property Directory: string read FDirectory write FDirectory;
    property Filtering: Boolean read FFiltering write FFiltering;
  end;

  { TSourcePackageConfig - Source package configuration section }
  TSourcePackageConfig = class
  private
    FIncludeDirs: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    property IncludeDirs: TStringList read FIncludeDirs;
  end;

  { TProjectConfig - Complete project configuration }
  TProjectConfig = class
  private
    FName: string;
    FVersion: string;
    FAuthor: string;
    FLicense: string;
    FProjectUrl: string;
    FRepoUrl: string;
    FBuildConfig: TBuildConfig;
    FTestConfig: TTestConfig;
    FResourcesConfig: TResourcesConfig;
    FTestResourcesConfig: TResourcesConfig;
    FSourcePackageConfig: TSourcePackageConfig;
    FProfiles: TProfileList;
    FModules: TStringList;                   // Child modules (for aggregator)
    FModuleDependencies: TStringList;        // Module dependencies (for library/app)
  public
    constructor Create;
    destructor Destroy; override;

    property Name: string read FName write FName;
    property Version: string read FVersion write FVersion;
    property Author: string read FAuthor write FAuthor;
    property License: string read FLicense write FLicense;
    property ProjectUrl: string read FProjectUrl write FProjectUrl;
    property RepoUrl: string read FRepoUrl write FRepoUrl;
    property BuildConfig: TBuildConfig read FBuildConfig;
    property TestConfig: TTestConfig read FTestConfig;
    property ResourcesConfig: TResourcesConfig read FResourcesConfig;
    property TestResourcesConfig: TResourcesConfig read FTestResourcesConfig;
    property SourcePackageConfig: TSourcePackageConfig read FSourcePackageConfig;
    property Profiles: TProfileList read FProfiles;
    property Modules: TStringList read FModules;                           // Child modules list
    property ModuleDependencies: TStringList read FModuleDependencies;    // Module dependencies
  end;

  { TModuleInfo - Module metadata for build ordering }
  TModuleInfo = class
  private
    FName: string;
    FPath: string;
    FConfig: TProjectConfig;
    FDependencies: TStringList;
    FUnitsDirectory: string;
  public
    constructor Create;
    destructor Destroy; override;

    property Name: string read FName write FName;
    property Path: string read FPath write FPath;
    property Config: TProjectConfig read FConfig;
    property Dependencies: TStringList read FDependencies;
    property UnitsDirectory: string read FUnitsDirectory write FUnitsDirectory;
  end;

  { TModuleRegistry - Registry for module resolution and lookup }
  TModuleRegistry = class
  private
    FModules: TObjectList;
    FModulesByName: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterModule(AModule: TModuleInfo);
    function FindModuleByName(const AName: string): TModuleInfo;
    function FindModuleByPath(const APath: string): TModuleInfo;
    function GetBuildOrder: TList;

    property Modules: TObjectList read FModules;
  end;

implementation

{ TConditionalPath }

constructor TConditionalPath.Create(const APath: string; const ACondition: string = '');
begin
  inherited Create;
  FPath := APath;
  FCondition := ACondition;
end;

{ TProfile }

constructor TProfile.Create;
begin
  inherited Create;
  FDefines := TStringList.Create;
  FDefines.Duplicates := dupIgnore;
  FDefines.Sorted := True;

  FCompilerOptions := TStringList.Create;
  FCompilerOptions.Duplicates := dupIgnore;
end;

destructor TProfile.Destroy;
begin
  FDefines.Free;
  FCompilerOptions.Free;
  inherited Destroy;
end;

{ TProfileList }

function TProfileList.FindById(const AId: string): TProfile;
var
  Profile: TProfile;
begin
  Result := nil;
  for Profile in Self do
  begin
    if Profile.Id = AId then
    begin
      Result := Profile;
      Exit;
    end;
  end;
end;

{ TBuildConfig }

constructor TBuildConfig.Create;
begin
  inherited Create;
  FDefines := TStringList.Create;
  FDefines.Duplicates := dupIgnore;
  FDefines.Sorted := True;

  FCompilerOptions := TStringList.Create;
  FCompilerOptions.Duplicates := dupIgnore;

  FUnitPaths := TConditionalPathList.Create;
  FUnitPaths.FreeObjects := True;

  FIncludePaths := TConditionalPathList.Create;
  FIncludePaths.FreeObjects := True;

  // Set defaults
  FProjectType := ptApplication;  // Application by default
  FOutputDirectory := 'target';
  FManualUnitPaths := False;  // Auto-scan by default
end;

destructor TBuildConfig.Destroy;
begin
  FDefines.Free;
  FCompilerOptions.Free;
  FUnitPaths.Free;
  FIncludePaths.Free;
  inherited Destroy;
end;

{ TTestConfig }

constructor TTestConfig.Create;
begin
  inherited Create;
  FFrameworkOptions := TStringList.Create;
  FFrameworkOptions.Duplicates := dupIgnore;

  // Set defaults
  FFramework := tfAuto;  // Auto-detect by default
  FTestSource := 'TestRunner.pas';  // Common default name
end;

destructor TTestConfig.Destroy;
begin
  FFrameworkOptions.Free;
  inherited Destroy;
end;

{ TResourcesConfig }

constructor TResourcesConfig.Create;
begin
  inherited Create;
  FDirectory := 'src/main/resources';  // Default
  FFiltering := False;  // Default: no filtering
end;

{ TSourcePackageConfig }

constructor TSourcePackageConfig.Create;
begin
  inherited Create;
  FIncludeDirs := TStringList.Create;
  FIncludeDirs.Duplicates := dupIgnore;
end;

destructor TSourcePackageConfig.Destroy;
begin
  FIncludeDirs.Free;
  inherited Destroy;
end;

{ TProjectConfig }

constructor TProjectConfig.Create;
begin
  inherited Create;
  FBuildConfig := TBuildConfig.Create;
  FTestConfig := TTestConfig.Create;
  FResourcesConfig := TResourcesConfig.Create;
  FTestResourcesConfig := TResourcesConfig.Create;
  FTestResourcesConfig.Directory := 'src/test/resources';  // Override default for test resources
  FSourcePackageConfig := TSourcePackageConfig.Create;
  FProfiles := TProfileList.Create;
  FProfiles.FreeObjects := True;

  // Multi-module support
  FModules := TStringList.Create;
  FModules.Duplicates := dupIgnore;

  FModuleDependencies := TStringList.Create;
  FModuleDependencies.Duplicates := dupIgnore;

  // Set defaults
  FAuthor := 'Unknown';
  FLicense := 'Proprietary';
end;

destructor TProjectConfig.Destroy;
begin
  FBuildConfig.Free;
  FTestConfig.Free;
  FResourcesConfig.Free;
  FTestResourcesConfig.Free;
  FSourcePackageConfig.Free;
  FProfiles.Free;
  FModules.Free;
  FModuleDependencies.Free;
  inherited Destroy;
end;

{ TModuleInfo implementation }

constructor TModuleInfo.Create;
begin
  inherited Create;
  FDependencies := TStringList.Create;
  FDependencies.Duplicates := dupIgnore;
  FConfig := nil;
end;

destructor TModuleInfo.Destroy;
begin
  FDependencies.Free;
  if Assigned(FConfig) then
    FConfig.Free;
  inherited Destroy;
end;

{ TModuleRegistry implementation }

constructor TModuleRegistry.Create;
begin
  inherited Create;
  FModules := TObjectList.Create(True);  { Owns modules }
  FModulesByName := TStringList.Create;
  FModulesByName.Duplicates := dupError;  { Raise error on duplicate }
end;

destructor TModuleRegistry.Destroy;
begin
  FModulesByName.Free;
  FModules.Free;
  inherited Destroy;
end;

procedure TModuleRegistry.RegisterModule(AModule: TModuleInfo);
begin
  if not Assigned(AModule) then
    raise Exception.Create('Cannot register nil module');

  if FModulesByName.IndexOf(AModule.Name) >= 0 then
    raise Exception.CreateFmt('Duplicate module name: %s', [AModule.Name]);

  FModules.Add(AModule);
  FModulesByName.AddObject(AModule.Name, AModule);
end;

function TModuleRegistry.FindModuleByName(const AName: string): TModuleInfo;
var
  Index: Integer;
begin
  Index := FModulesByName.IndexOf(AName);
  if Index >= 0 then
    Result := TModuleInfo(FModulesByName.Objects[Index])
  else
    Result := nil;
end;

function TModuleRegistry.FindModuleByPath(const APath: string): TModuleInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FModules.Count - 1 do
  begin
    if SameFileName(TModuleInfo(FModules[I]).Path, APath) then
    begin
      Result := TModuleInfo(FModules[I]);
      Exit;
    end;
  end;
end;

function TModuleRegistry.GetBuildOrder: TList;
var
  Visited, RecStack: TStringList;

  procedure Visit(const ModuleName: string);
  var
    Module: TModuleInfo;
    I: Integer;
    DepName: string;
    DepModule: TModuleInfo;
    StackIndex: Integer;
  begin
    Module := FindModuleByName(ModuleName);
    if Module = nil then
      raise Exception.CreateFmt('Module not found: %s', [ModuleName]);

    { Check if already visited }
    if Visited.IndexOf(ModuleName) >= 0 then
      Exit;

    { Check for cycle (module in recursion stack) }
    StackIndex := RecStack.IndexOf(ModuleName);
    if StackIndex >= 0 then
    begin
      { Cyclic dependency detected }
      raise Exception.CreateFmt('Cyclic dependency detected: %s', [ModuleName]);
    end;

    { Add to recursion stack }
    RecStack.Add(ModuleName);

    { Visit all dependencies first (depth-first) }
    for I := 0 to Module.Dependencies.Count - 1 do
    begin
      DepName := Module.Dependencies[I];
      DepModule := FindModuleByName(DepName);
      if DepModule = nil then
        raise Exception.CreateFmt('Dependency not found: %s (required by %s)', [DepName, ModuleName]);

      Visit(DepName);
    end;

    { Remove from recursion stack }
    StackIndex := RecStack.IndexOf(ModuleName);
    if StackIndex >= 0 then
      RecStack.Delete(StackIndex);

    { Mark as visited and add to build order }
    Visited.Add(ModuleName);
    Result.Add(Module);
  end;

var
  I: Integer;
  Module: TModuleInfo;
begin
  Result := TList.Create;
  Visited := TStringList.Create;
  RecStack := TStringList.Create;
  try
    Visited.Duplicates := dupIgnore;
    RecStack.Duplicates := dupIgnore;

    { Visit all modules to build topological order }
    for I := 0 to FModules.Count - 1 do
    begin
      Module := TModuleInfo(FModules[I]);
      if Visited.IndexOf(Module.Name) < 0 then
        Visit(Module.Name);
    end;
  finally
    RecStack.Free;
    Visited.Free;
  end;
end;

end.
