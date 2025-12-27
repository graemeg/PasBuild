{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Command.Reactor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  PasBuild.Types,
  PasBuild.Command,
  PasBuild.Command.Clean,
  PasBuild.Command.Compile,
  PasBuild.Command.Test,
  PasBuild.Command.Package,
  PasBuild.Utils;

type
  { TReactorCommand - Executes a goal on all modules in build order }
  TReactorCommand = class(TBuildCommand)
  private
    FRegistry: TModuleRegistry;
    FGoalName: string;
    FModulesBuilt: Integer;
    FModulesFailed: Integer;
    FSelectedModule: string;  // Empty = all modules, otherwise build only this module + dependencies
    procedure DisplayDependencyGraph;
    procedure FilterBuildOrderForSelectedModule(var BuildOrder: TList);
  protected
    function GetName: string; override;
  public
    constructor Create(AConfig: TProjectConfig; AProfileIds: TStringList;
      ARegistry: TModuleRegistry; const AGoalName: string; const ASelectedModule: string = ''); reintroduce;
    destructor Destroy; override;

    function Execute: Integer; override;
    function GetDependencies: TBuildCommandList; override;

    property Registry: TModuleRegistry read FRegistry;
    property GoalName: string read FGoalName;
    property ModulesBuilt: Integer read FModulesBuilt;
    property ModulesFailed: Integer read FModulesFailed;
    property SelectedModule: string read FSelectedModule write FSelectedModule;
  end;

implementation

{ TReactorCommand }

constructor TReactorCommand.Create(AConfig: TProjectConfig; AProfileIds: TStringList;
  ARegistry: TModuleRegistry; const AGoalName: string; const ASelectedModule: string = '');
begin
  inherited Create(AConfig, AProfileIds);
  FRegistry := ARegistry;
  FGoalName := AGoalName;
  FSelectedModule := ASelectedModule;
  FModulesBuilt := 0;
  FModulesFailed := 0;
end;

destructor TReactorCommand.Destroy;
begin
  { Registry is not owned by this command - it's passed in }
  inherited Destroy;
end;

function TReactorCommand.GetName: string;
begin
  Result := 'reactor-' + FGoalName;
end;

procedure TReactorCommand.DisplayDependencyGraph;
var
  I: Integer;
  Module: TModuleInfo;
  J: Integer;
begin
  TUtils.LogInfo('Dependency Graph:');
  TUtils.LogInfo('');

  for I := 0 to FRegistry.Modules.Count - 1 do
  begin
    Module := TModuleInfo(FRegistry.Modules[I]);

    if Module.Dependencies.Count = 0 then
      TUtils.LogInfo(Module.Name + ' (no dependencies)')
    else
    begin
      TUtils.LogInfo(Module.Name + ' (depends on: ' + Module.Dependencies.CommaText + ')');

      { Show dependency tree with indentation }
      for J := 0 to Module.Dependencies.Count - 1 do
      begin
        TUtils.LogInfo('  └─ ' + Module.Dependencies[J]);
      end;
    end;
  end;

  TUtils.LogInfo('');
end;

procedure TReactorCommand.FilterBuildOrderForSelectedModule(var BuildOrder: TList);
var
  I: Integer;
  Module: TModuleInfo;
  SelectedModuleInfo: TModuleInfo;
  FilteredOrder: TList;
  IncludeModule: Boolean;
begin
  { Find the selected module by name }
  SelectedModuleInfo := nil;
  for I := 0 to FRegistry.Modules.Count - 1 do
  begin
    Module := TModuleInfo(FRegistry.Modules[I]);
    if CompareText(Module.Name, FSelectedModule) = 0 then
    begin
      SelectedModuleInfo := Module;
      Break;
    end;
  end;

  if SelectedModuleInfo = nil then
  begin
    TUtils.LogError('Module not found: ' + FSelectedModule);
    { Clear build order to signal error }
    BuildOrder.Clear;
    Exit;
  end;

  { Filter: Keep only modules that the selected module depends on (and the selected module itself) }
  { The build order is topologically sorted, so all dependencies come before the selected module }
  FilteredOrder := TList.Create;
  try
    for I := 0 to BuildOrder.Count - 1 do
    begin
      Module := TModuleInfo(BuildOrder[I]);
      IncludeModule := False;

      { Include the selected module itself }
      if CompareText(Module.Name, FSelectedModule) = 0 then
        IncludeModule := True
      { Include if this module is a (direct or transitive) dependency of the selected module }
      { Since build order is sorted, we only include modules up to and including selected }
      else
      begin
        { Check if this module is transitively needed by selected module }
        { For now, include all modules that come before selected in build order }
        IncludeModule := True;
      end;

      if IncludeModule then
        FilteredOrder.Add(Module);

      { Stop once we've added the selected module }
      if CompareText(Module.Name, FSelectedModule) = 0 then
        Break;
    end;

    { Replace build order with filtered order }
    BuildOrder.Clear;
    for I := 0 to FilteredOrder.Count - 1 do
      BuildOrder.Add(FilteredOrder[I]);
  finally
    FilteredOrder.Free;
  end;
end;

function TReactorCommand.Execute: Integer;
var
  BuildOrder: TList;
  I: Integer;
  Module: TModuleInfo;
  ModuleCount: Integer;
  ModuleConfig: TProjectConfig;
  ModuleCommand: TBuildCommand;
  ModuleExecutor: TCommandExecutor;
  CurrentDir: string;
  OriginalDir: string;
begin
  Result := 0;
  FModulesBuilt := 0;
  FModulesFailed := 0;

  { Get build order from registry }
  BuildOrder := FRegistry.GetBuildOrder;
  try
    { Filter to selected module if specified }
    if FSelectedModule <> '' then
    begin
      FilterBuildOrderForSelectedModule(BuildOrder);
    end;

    ModuleCount := BuildOrder.Count;

    if ModuleCount = 0 then
    begin
      if FSelectedModule <> '' then
        TUtils.LogError('Selected module not found or has no dependencies: ' + FSelectedModule);
      Exit(1);
    end;

    if FSelectedModule <> '' then
      TUtils.LogInfo('Building ' + IntToStr(ModuleCount) + ' modules (selected: ' + FSelectedModule + ')')
    else
      TUtils.LogInfo('Building ' + IntToStr(ModuleCount) + ' modules in dependency order');

    { Display dependency graph in verbose mode }
    if FVerbose then
      DisplayDependencyGraph;

    { Save current directory for restoration }
    OriginalDir := GetCurrentDir;

    { Build each module in order }
    for I := 0 to BuildOrder.Count - 1 do
    begin
      Module := TModuleInfo(BuildOrder[I]);

      { Skip aggregators (pom packaging) }
      if (Module.Config <> nil) and (Module.Config.BuildConfig.ProjectType = ptPom) then
      begin
        TUtils.LogInfo('Skipping aggregator module: ' + Module.Name);
        Continue;
      end;

      if Module.Config = nil then
      begin
        TUtils.LogError('Module config not loaded: ' + Module.Name);
        Inc(FModulesFailed);
        Result := 1;
        Continue;
      end;

      { Log module being built }
      TUtils.LogInfo('Building module ' + IntToStr(I + 1) + '/' + IntToStr(ModuleCount) + ': ' + Module.Name);

      { Resolve artifacts for this module (add dependency paths) }
      FRegistry.ResolveArtifacts(Module);

      { Change to module directory }
      CurrentDir := Module.Path;
      try
        ChDir(CurrentDir);
      except
        TUtils.LogError('Failed to change to module directory: ' + CurrentDir);
        Inc(FModulesFailed);
        Result := 1;
        Continue;
      end;

      { Create appropriate command for this module based on goal }
      ModuleConfig := Module.Config;
      ModuleCommand := nil;
      ModuleExecutor := TCommandExecutor.Create;
      try
        case FGoalName of
          'clean':
            ModuleCommand := TCleanCommand.Create(ModuleConfig, FProfileIds);

          'compile':
            ModuleCommand := TCompileCommand.Create(ModuleConfig, FProfileIds);

          'test':
            ModuleCommand := TTestCommand.Create(ModuleConfig, FProfileIds);

          'package':
          begin
            { For aggregated packaging, only compile modules.
              Package aggregation happens at aggregator level via TAggregatedPackageCommand. }
            ModuleCommand := TCompileCommand.Create(ModuleConfig, FProfileIds);
          end;

          else
          begin
            TUtils.LogError('Unsupported goal in reactor: ' + FGoalName);
            Inc(FModulesFailed);
            Result := 1;
            Continue;
          end;
        end;

        { Execute module's command }
        if Assigned(ModuleCommand) then
        begin
          try
            ModuleCommand.Verbose := FVerbose;
            if ModuleExecutor.Execute(ModuleCommand) = 0 then
            begin
              Inc(FModulesBuilt);
              TUtils.LogInfo('Module build successful: ' + Module.Name);
            end
            else
            begin
              Inc(FModulesFailed);
              Result := 1;
              TUtils.LogError('Module build failed: ' + Module.Name);
              { Stop reactor build on first failure (fail-fast) }
              Break;
            end;
          finally
            ModuleCommand.Free;
          end;
        end;

      finally
        ModuleExecutor.Free;
      end;
    end;

    { Restore original directory }
    try
      ChDir(OriginalDir);
    except
      { Ignore errors restoring directory }
    end;

    { Summary }
    TUtils.LogInfo('Reactor build complete: ' + IntToStr(FModulesBuilt) + '/' + IntToStr(ModuleCount) + ' modules built');

    if FModulesFailed > 0 then
      Result := 1;

  finally
    BuildOrder.Free;
  end;
end;

function TReactorCommand.GetDependencies: TBuildCommandList;
begin
  { Reactor builds have no build-goal dependencies (no pre-requisite goals) }
  Result := TBuildCommandList.Create;
  Result.FreeObjects := False;
end;

end.
