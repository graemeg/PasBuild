{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Command.Resolve;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson,
  PasBuild.Types,
  PasBuild.Command,
  PasBuild.Dependencies,
  PasBuild.Utils;

type
  { TResolveCommand - Outputs resolved build configuration as JSON }
  TResolveCommand = class(TBuildCommand)
  private
    FRegistry: TModuleRegistry;   { nil for single-module projects }
    FSelectedModule: string;      { '' = all modules }
    function ProjectTypeToString(AType: TProjectType): string;
    function CollectActiveDefines(AConfig: TProjectConfig): TStringList;
    function CollectUnitPaths(AConfig: TProjectConfig;
      AActiveDefines: TStringList): TStringList;
    function CollectIncludePaths(AConfig: TProjectConfig;
      AActiveDefines: TStringList): TStringList;
    procedure AddCompilerSection(AResult: TJSONObject; AConfig: TProjectConfig;
      AActiveDefines, AUnitPaths, AIncludePaths: TStringList);
    procedure AddResolveData(AResult: TJSONObject; AConfig: TProjectConfig;
      const AProjectDir: string; AModule: TModuleInfo = nil);
    procedure AddModuleDependencies(ADepsArr: TJSONArray; AModule: TModuleInfo);
    function BuildSingleModuleJSON(AConfig: TProjectConfig;
      const AProjectDir: string; AModule: TModuleInfo = nil): TJSONObject;
    procedure ResolveAndChDir(AModule: TModuleInfo);
  protected
    function GetName: string; override;
    function BuildResolveJSON: TJSONObject; virtual;
  public
    constructor Create(AConfig: TProjectConfig;
      AProfileIds: TStringList); override;
    constructor CreateMultiModule(AConfig: TProjectConfig;
      AProfileIds: TStringList; ARegistry: TModuleRegistry;
      const ASelectedModule: string = ''); reintroduce;

    function Execute: Integer; override;
    function GetDependencies: TBuildCommandList; override;

    property SelectedModule: string read FSelectedModule write FSelectedModule;
  end;

implementation

{ TResolveCommand }

constructor TResolveCommand.Create(AConfig: TProjectConfig;
  AProfileIds: TStringList);
begin
  inherited Create(AConfig, AProfileIds);
  FRegistry := nil;
  FSelectedModule := '';
end;

constructor TResolveCommand.CreateMultiModule(AConfig: TProjectConfig;
  AProfileIds: TStringList; ARegistry: TModuleRegistry;
  const ASelectedModule: string = '');
begin
  inherited Create(AConfig, AProfileIds);
  FRegistry := ARegistry;
  FSelectedModule := ASelectedModule;
end;

function TResolveCommand.GetName: string;
begin
  Result := 'resolve';
end;

function TResolveCommand.GetDependencies: TBuildCommandList;
begin
  { No prerequisite goals — resolve never compiles anything }
  Result := TBuildCommandList.Create;
  Result.FreeObjects := False;
end;

function TResolveCommand.ProjectTypeToString(AType: TProjectType): string;
begin
  case AType of
    ptApplication: Result := 'application';
    ptLibrary:     Result := 'library';
    ptPom:         Result := 'pom';
    else           Result := 'unknown';
  end;
end;

function TResolveCommand.CollectActiveDefines(AConfig: TProjectConfig): TStringList;
var
  ProfileId: string;
  Profile: TProfile;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;
  Result.Sorted := True;

  { Global defines }
  Result.AddStrings(AConfig.BuildConfig.Defines);

  { Profile defines }
  if Assigned(ProfileIds) then
  begin
    for ProfileId in ProfileIds do
    begin
      Profile := AConfig.Profiles.FindById(ProfileId);
      if Assigned(Profile) then
        Result.AddStrings(Profile.Defines);
    end;
  end;
end;

function TResolveCommand.CollectUnitPaths(AConfig: TProjectConfig;
  AActiveDefines: TStringList): TStringList;
var
  BasePath: string;
  I: Integer;
  ConditionalPath: TConditionalPath;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;

  BasePath := TUtils.NormalizePath(AConfig.BuildConfig.SourceDirectory);

  if AConfig.BuildConfig.ManualUnitPaths then
  begin
    { Manual mode: use explicitly listed paths }
    for I := 0 to AConfig.BuildConfig.UnitPaths.Count - 1 do
    begin
      ConditionalPath := AConfig.BuildConfig.UnitPaths[I];
      if TUtils.IsConditionMet(ConditionalPath.Condition, AActiveDefines) then
        Result.Add(TUtils.NormalizePath(ConditionalPath.Path));
    end;
  end
  else
  begin
    { Auto-scan mode: add base source directory first, then scanned subdirs }
    Result.Free;
    Result := TUtils.ScanForUnitPathsFiltered(
      BasePath,
      AConfig.BuildConfig.UnitPaths,
      AActiveDefines
    );
    Result.Sorted := False;
    Result.Insert(0, BasePath);

    { Also add any explicitly listed paths that point outside the source tree
      (start with ../ or ./). These cannot be found by scanning BasePath, so
      they must be added directly. In-tree paths are already found by the scan. }
    for I := 0 to AConfig.BuildConfig.UnitPaths.Count - 1 do
    begin
      ConditionalPath := AConfig.BuildConfig.UnitPaths[I];
      if TUtils.IsConditionMet(ConditionalPath.Condition, AActiveDefines) then
        if TUtils.IsModuleRootRelativePath(ConditionalPath.Path) then
          Result.Add(TUtils.NormalizePath(ConditionalPath.Path));
    end;
  end;

  { Add resolved module dependency paths }
  for I := 0 to AConfig.BuildConfig.ResolvedModulePaths.Count - 1 do
    Result.Add(TUtils.NormalizePath(AConfig.BuildConfig.ResolvedModulePaths[I]));

  { Add unit output directory }
  Result.Add(TUtils.NormalizePath(
    AConfig.BuildConfig.OutputDirectory + DirectorySeparator + 'units'));
end;

function TResolveCommand.CollectIncludePaths(AConfig: TProjectConfig;
  AActiveDefines: TStringList): TStringList;
var
  BasePath: string;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;

  { Always add output directory first (for filtered resource includes) }
  Result.Add(TUtils.NormalizePath(AConfig.BuildConfig.OutputDirectory));

  BasePath := TUtils.NormalizePath(AConfig.BuildConfig.SourceDirectory);

  if AConfig.BuildConfig.ManualUnitPaths then
  begin
    { Manual mode: check base + listed paths for .inc files }
    if TUtils.DirectoryContainsIncludeFiles(BasePath) then
      Result.Add(BasePath);
  end
  else
  begin
    { Auto-scan mode }
    Result.Free;
    if AConfig.BuildConfig.IncludePaths.Count > 0 then
      Result := TUtils.ScanForIncludePathsFiltered(
        BasePath, AConfig.BuildConfig.IncludePaths, AActiveDefines)
    else
      Result := TUtils.ScanForIncludePathsFiltered(
        BasePath, AConfig.BuildConfig.UnitPaths, AActiveDefines);

    { Unsort so we can prepend the output directory }
    Result.Sorted := False;
    Result.Insert(0, TUtils.NormalizePath(AConfig.BuildConfig.OutputDirectory));
  end;
end;

procedure TResolveCommand.AddCompilerSection(AResult: TJSONObject;
  AConfig: TProjectConfig; AActiveDefines, AUnitPaths, AIncludePaths: TStringList);
var
  CompilerObj: TJSONObject;
  CmdLine, OutputDir, ProfileId: string;
  Profile: TProfile;
  I: Integer;
begin
  CompilerObj := TJSONObject.Create;
  CompilerObj.Add('executable', TUtils.GetCompilerBackend.Executable);
  CompilerObj.Add('compilerName', TUtils.GetCompilerBackend.Name);

  { Build the command line — mirrors TCompileCommand.BuildCompilerCommand }
  OutputDir := TUtils.NormalizePath(AConfig.BuildConfig.OutputDirectory);
  CmdLine := TUtils.GetCompilerBackend.BaseCommand;

  if AConfig.BuildConfig.MainSource <> '' then
  begin
    if AConfig.BuildConfig.ProjectType = ptLibrary then
      CmdLine := CmdLine + ' ' + TUtils.GetCompilerBackend.SourceFlag(
        TUtils.NormalizePath(AConfig.BuildConfig.OutputDirectory + '/bootstrap_program.pas'))
    else
      CmdLine := CmdLine + ' ' + TUtils.GetCompilerBackend.SourceFlag(
        TUtils.NormalizePath(AConfig.BuildConfig.SourceDirectory + '/' + AConfig.BuildConfig.MainSource));
  end;

  CmdLine := CmdLine + ' ' + TUtils.GetCompilerBackend.OutputFlags(
    OutputDir, AConfig.BuildConfig.ExecutableName + TUtils.GetPlatformExecutableSuffix);
  CmdLine := CmdLine + ' ' + TUtils.GetCompilerBackend.UnitOutputDirFlag(
    OutputDir + DirectorySeparator + 'units');
  CmdLine := CmdLine + ' ' + TUtils.GetCompilerBackend.IncrementalFlag;

  { Unit paths }
  for I := 0 to AUnitPaths.Count - 1 do
    CmdLine := CmdLine + ' ' + TUtils.GetCompilerBackend.UnitPathFlag(AUnitPaths[I]);

  { Include paths }
  for I := 0 to AIncludePaths.Count - 1 do
    CmdLine := CmdLine + ' ' + TUtils.GetCompilerBackend.IncludePathFlag(AIncludePaths[I]);

  { Global defines }
  for I := 0 to AConfig.BuildConfig.Defines.Count - 1 do
    CmdLine := CmdLine + ' ' + TUtils.GetCompilerBackend.DefineFlag(AConfig.BuildConfig.Defines[I]);

  { Global compiler options }
  for I := 0 to AConfig.BuildConfig.CompilerOptions.Count - 1 do
    CmdLine := CmdLine + ' ' + TUtils.GetCompilerBackend.ExtraOptionFlag(AConfig.BuildConfig.CompilerOptions[I]);

  { Profile-specific defines and options }
  if Assigned(ProfileIds) then
  begin
    for ProfileId in ProfileIds do
    begin
      Profile := AConfig.Profiles.FindById(ProfileId);
      if Assigned(Profile) then
      begin
        for I := 0 to Profile.Defines.Count - 1 do
          CmdLine := CmdLine + ' -d' + Profile.Defines[I];
        for I := 0 to Profile.CompilerOptions.Count - 1 do
          CmdLine := CmdLine + ' ' + Profile.CompilerOptions[I];
      end;
    end;
  end;

  CompilerObj.Add('commandLine', CmdLine);
  AResult.Add('compiler', CompilerObj);
end;

procedure TResolveCommand.AddModuleDependencies(ADepsArr: TJSONArray;
  AModule: TModuleInfo);
var
  I: Integer;
  DepName: string;
  DepModule: TModuleInfo;
  DepObj: TJSONObject;
begin
  if (FRegistry = nil) or (AModule = nil) then
    Exit;

  { Use AModule.Dependencies which contains resolved module names
    (not the raw relative paths from ModuleDependencies) }
  for I := 0 to AModule.Dependencies.Count - 1 do
  begin
    DepName := AModule.Dependencies[I];
    DepModule := FRegistry.FindModuleByName(DepName);
    DepObj := TJSONObject.Create;
    DepObj.Add('name', DepName);
    if Assigned(DepModule) and Assigned(DepModule.Config) then
    begin
      DepObj.Add('version', DepModule.Config.Version);
      DepObj.Add('type', 'local');
      DepObj.Add('projectDir', DepModule.Path);
      DepObj.Add('sourceDir', DepModule.Path + DirectorySeparator +
        TUtils.NormalizePath(DepModule.Config.BuildConfig.SourceDirectory));
      DepObj.Add('unitDir', DepModule.UnitsDirectory);
    end
    else
    begin
      DepObj.Add('type', 'local');
    end;
    ADepsArr.Add(DepObj);
  end;
end;

procedure TResolveCommand.AddResolveData(AResult: TJSONObject;
  AConfig: TProjectConfig; const AProjectDir: string; AModule: TModuleInfo);
var
  ActiveDefines, UnitPathsList, IncludePathsList: TStringList;
  I: Integer;
  Arr: TJSONArray;
  DepObj, OutputObj, TestObj, DirsObj: TJSONObject;
  OutputDir: string;
begin
  ActiveDefines := CollectActiveDefines(AConfig);
  try
    UnitPathsList := CollectUnitPaths(AConfig, ActiveDefines);
    try
      IncludePathsList := CollectIncludePaths(AConfig, ActiveDefines);
      try
        { Compiler section }
        AddCompilerSection(AResult, AConfig, ActiveDefines, UnitPathsList, IncludePathsList);

        { Defines array }
        Arr := TJSONArray.Create;
        for I := 0 to ActiveDefines.Count - 1 do
          Arr.Add(ActiveDefines[I]);
        AResult.Add('defines', Arr);

        { Unit paths array }
        Arr := TJSONArray.Create;
        for I := 0 to UnitPathsList.Count - 1 do
          Arr.Add(UnitPathsList[I]);
        AResult.Add('unitPaths', Arr);

        { Include paths array }
        Arr := TJSONArray.Create;
        for I := 0 to IncludePathsList.Count - 1 do
          Arr.Add(IncludePathsList[I]);
        AResult.Add('includePaths', Arr);
      finally
        IncludePathsList.Free;
      end;
    finally
      UnitPathsList.Free;
    end;
  finally
    ActiveDefines.Free;
  end;

  { Dependencies — module deps first, then external }
  Arr := TJSONArray.Create;
  AddModuleDependencies(Arr, AModule);
  for I := 0 to AConfig.Dependencies.Count - 1 do
  begin
    DepObj := TJSONObject.Create;
    DepObj.Add('name', AConfig.Dependencies[I].Name);
    DepObj.Add('version', AConfig.Dependencies[I].Version);
    DepObj.Add('type', 'external');
    Arr.Add(DepObj);
  end;
  AResult.Add('dependencies', Arr);

  { Test section }
  if AConfig.TestConfig.TestSource <> '' then
  begin
    TestObj := TJSONObject.Create;
    TestObj.Add('testSource', AConfig.TestConfig.TestSource);
    AResult.Add('test', TestObj);
  end;

  { Directories section — resolved conventional directory locations }
  DirsObj := TJSONObject.Create;
  DirsObj.Add('source', AConfig.BuildConfig.SourceDirectory);
  DirsObj.Add('testSource', AConfig.TestConfig.SourceDirectory);
  DirsObj.Add('resources', AConfig.ResourcesConfig.Directory);
  DirsObj.Add('testResources', AConfig.TestResourcesConfig.Directory);
  AResult.Add('directories', DirsObj);

  { Output section }
  OutputObj := TJSONObject.Create;
  OutputDir := TUtils.NormalizePath(AConfig.BuildConfig.OutputDirectory);
  OutputObj.Add('directory', OutputDir);
  OutputObj.Add('unitDirectory', OutputDir + DirectorySeparator + 'units');
  if (AConfig.BuildConfig.ProjectType = ptApplication) and
     (AConfig.BuildConfig.ExecutableName <> '') then
    OutputObj.Add('executable', OutputDir + DirectorySeparator +
      AConfig.BuildConfig.ExecutableName + TUtils.GetPlatformExecutableSuffix);
  AResult.Add('output', OutputObj);
end;

function TResolveCommand.BuildSingleModuleJSON(AConfig: TProjectConfig;
  const AProjectDir: string; AModule: TModuleInfo): TJSONObject;
var
  ProjectObj: TJSONObject;
  Arr: TJSONArray;
  I: Integer;
begin
  Result := TJSONObject.Create;

  { Project section }
  ProjectObj := TJSONObject.Create;
  ProjectObj.Add('name', AConfig.Name);
  if AConfig.Version <> '' then
    ProjectObj.Add('version', AConfig.Version);
  ProjectObj.Add('projectType',
    ProjectTypeToString(AConfig.BuildConfig.ProjectType));
  ProjectObj.Add('projectDir', AProjectDir);
  if AConfig.BuildConfig.MainSource <> '' then
    ProjectObj.Add('mainSource', AConfig.BuildConfig.MainSource);
  if AConfig.BuildConfig.ExecutableName <> '' then
    ProjectObj.Add('executableName', AConfig.BuildConfig.ExecutableName);
  Result.Add('project', ProjectObj);

  { Active profiles }
  Arr := TJSONArray.Create;
  if Assigned(ProfileIds) then
    for I := 0 to ProfileIds.Count - 1 do
      Arr.Add(ProfileIds[I]);
  Result.Add('activeProfiles', Arr);

  { Available profiles }
  Arr := TJSONArray.Create;
  for I := 0 to AConfig.Profiles.Count - 1 do
    Arr.Add(AConfig.Profiles[I].Id);
  Result.Add('availableProfiles', Arr);

  { Add all resolve data (compiler, defines, unitPaths, etc.) }
  AddResolveData(Result, AConfig, AProjectDir, AModule);
end;

procedure TResolveCommand.ResolveAndChDir(AModule: TModuleInfo);
var
  Resolver: TDependencyResolver;
begin
  { Resolve inter-module artifact paths (populates ResolvedModulePaths) }
  FRegistry.ResolveArtifacts(AModule);

  { Resolve external dependencies from local repository }
  if Assigned(AModule.Config) and (AModule.Config.Dependencies.Count > 0) then
  begin
    Resolver := nil;
    try
      try
        Resolver := TDependencyResolver.CreateConfigured(
          AModule.Config.Repositories);
        Resolver.ResolveDependencies(AModule.Config);
      except
        on E: EDependencyError do
          TUtils.LogWarning(E.Message);
      end;
    finally
      Resolver.Free;
    end;
  end;

  { Change to module directory so auto-scan finds source files }
  if DirectoryExists(AModule.Path) then
    ChDir(AModule.Path);
end;

function TResolveCommand.BuildResolveJSON: TJSONObject;
var
  ProjectObj: TJSONObject;
  ModulesArr, BuildOrderArr: TJSONArray;
  ModuleObj: TJSONObject;
  Arr: TJSONArray;
  I: Integer;
  Module: TModuleInfo;
  FoundModule: TModuleInfo;
  BuildOrder: TList;
  SavedDir: string;
begin
  SavedDir := GetCurrentDir;

  if (FRegistry <> nil) and (FSelectedModule = '') then
  begin
    { Multi-module aggregator output }
    Result := TJSONObject.Create;

    ProjectObj := TJSONObject.Create;
    ProjectObj.Add('name', Config.Name);
    if Config.Version <> '' then
      ProjectObj.Add('version', Config.Version);
    ProjectObj.Add('projectType', 'pom');
    ProjectObj.Add('projectDir', SavedDir);
    Result.Add('project', ProjectObj);

    { Active profiles }
    Arr := TJSONArray.Create;
    if Assigned(ProfileIds) then
      for I := 0 to ProfileIds.Count - 1 do
        Arr.Add(ProfileIds[I]);
    Result.Add('activeProfiles', Arr);

    { Available profiles }
    Arr := TJSONArray.Create;
    for I := 0 to Config.Profiles.Count - 1 do
      Arr.Add(Config.Profiles[I].Id);
    Result.Add('availableProfiles', Arr);

    { Build order and modules }
    BuildOrder := FRegistry.GetBuildOrder;
    try
      BuildOrderArr := TJSONArray.Create;
      for I := 0 to BuildOrder.Count - 1 do
      begin
        Module := TModuleInfo(BuildOrder[I]);
        BuildOrderArr.Add(Module.Name);
      end;
      Result.Add('buildOrder', BuildOrderArr);

      { Modules array — each module gets full resolve data }
      ModulesArr := TJSONArray.Create;
      for I := 0 to BuildOrder.Count - 1 do
      begin
        Module := TModuleInfo(BuildOrder[I]);
        if Assigned(Module.Config) then
        begin
          ResolveAndChDir(Module);
          ModuleObj := TJSONObject.Create;
          ModuleObj.Add('name', Module.Config.Name);
          ModuleObj.Add('projectType',
            ProjectTypeToString(Module.Config.BuildConfig.ProjectType));
          ModuleObj.Add('projectDir', Module.Path);
          if Module.Config.BuildConfig.MainSource <> '' then
            ModuleObj.Add('mainSource', Module.Config.BuildConfig.MainSource);
          if Module.Config.BuildConfig.ExecutableName <> '' then
            ModuleObj.Add('executableName', Module.Config.BuildConfig.ExecutableName);
          AddResolveData(ModuleObj, Module.Config, Module.Path, Module);
          ModulesArr.Add(ModuleObj);
          ChDir(SavedDir);
        end;
      end;
      Result.Add('modules', ModulesArr);
    finally
      BuildOrder.Free;
      ChDir(SavedDir);
    end;
  end
  else if (FRegistry <> nil) and (FSelectedModule <> '') then
  begin
    { Multi-module with specific module selected — single-module format }
    FoundModule := nil;
    for I := 0 to FRegistry.Modules.Count - 1 do
    begin
      Module := TModuleInfo(FRegistry.Modules[I]);
      if CompareText(Module.Name, FSelectedModule) = 0 then
      begin
        FoundModule := Module;
        Break;
      end;
    end;

    if FoundModule = nil then
    begin
      Result := nil;
      Exit;
    end;

    ResolveAndChDir(FoundModule);
    try
      Result := BuildSingleModuleJSON(FoundModule.Config, FoundModule.Path, FoundModule);
    finally
      ChDir(SavedDir);
    end;
  end
  else
  begin
    { Single-module output }
    Result := BuildSingleModuleJSON(Config, GetCurrentDir);
  end;
end;

function TResolveCommand.Execute: Integer;
var
  JSON: TJSONObject;
begin
  Result := 0;

  JSON := BuildResolveJSON;
  if JSON = nil then
  begin
    if FSelectedModule <> '' then
      TUtils.LogError('Module not found: ' + FSelectedModule);
    Result := 1;
    Exit;
  end;

  try
    { Write JSON to stdout }
    WriteLn(JSON.FormatJSON);
  finally
    JSON.Free;
  end;
end;

end.
