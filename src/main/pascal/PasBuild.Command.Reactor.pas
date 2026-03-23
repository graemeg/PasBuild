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
  PasBuild.Command.Install,
  PasBuild.Command.LazarusPackage,
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
    FResults: TList;          { Per-module build results for reactor summary }
    procedure DisplayDependencyGraph;
    procedure FilterBuildOrderForSelectedModule(var BuildOrder: TList);
    procedure PrintReactorSummary(ATotalSecs: Double);
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

type
  TModuleStatus = (msSuccess, msFailure, msSkipped);

  TModuleResult = record
    Name: string;
    Status: TModuleStatus;
    ElapsedSecs: Double;
  end;
  PModuleResult = ^TModuleResult;

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
  FResults := TList.Create;
end;

destructor TReactorCommand.Destroy;
var
  I: Integer;
begin
  for I := 0 to FResults.Count - 1 do
    Dispose(PModuleResult(FResults[I]));
  FResults.Free;
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
begin
  FRegistry.FilterBuildOrder(BuildOrder, FSelectedModule);
  if BuildOrder.Count = 0 then
    TUtils.LogError('Module not found: ' + FSelectedModule);
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
  OverallStartTime: TDateTime;
  ModuleStartTime: TDateTime;
  ElapsedSecs: Double;
  P: PModuleResult;
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

    { Print reactor build order }
    TUtils.LogSeparator;
    if FSelectedModule <> '' then
      TUtils.LogInfo('Reactor Build Order (selected: ' + FSelectedModule + '):')
    else
      TUtils.LogInfo('Reactor Build Order:');
    TUtils.LogInfo('');
    for I := 0 to BuildOrder.Count - 1 do
    begin
      Module := TModuleInfo(BuildOrder[I]);
      TUtils.LogInfo(Module.Name);
    end;
    TUtils.LogSeparator;

    { Display dependency graph in verbose mode }
    if FVerbose then
      DisplayDependencyGraph;

    { Save current directory for restoration }
    OriginalDir := GetCurrentDir;

    { Record overall build start time }
    OverallStartTime := Now;

    { Build each module in order }
    for I := 0 to BuildOrder.Count - 1 do
    begin
      Module := TModuleInfo(BuildOrder[I]);

      { Print Maven-style module header }
      TUtils.LogInfo('');
      TUtils.LogSeparator;
      TUtils.LogInfo('Building ' + Module.Name + ' [' + IntToStr(I + 1) + '/' + IntToStr(ModuleCount) + ']');
      TUtils.LogSeparator;

      { Record module build start time }
      ModuleStartTime := Now;

      { Skip aggregators (pom packaging) }
      if (Module.Config <> nil) and (Module.Config.BuildConfig.ProjectType = ptPom) then
      begin
        TUtils.LogInfo('Skipping aggregator module: ' + Module.Name);
        P := New(PModuleResult);
        P^.Name := Module.Name;
        P^.Status := msSkipped;
        P^.ElapsedSecs := 0;
        FResults.Add(P);
        Continue;
      end;

      if Module.Config = nil then
      begin
        TUtils.LogError('Module config not loaded: ' + Module.Name);
        Inc(FModulesFailed);
        Result := 1;
        P := New(PModuleResult);
        P^.Name := Module.Name;
        P^.Status := msFailure;
        P^.ElapsedSecs := 0;
        FResults.Add(P);
        Continue;
      end;

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
        P := New(PModuleResult);
        P^.Name := Module.Name;
        P^.Status := msFailure;
        P^.ElapsedSecs := 0;
        FResults.Add(P);
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

          'lazarus-package':
            ModuleCommand := TLazarusPackageCommand.Create(ModuleConfig, FProfileIds);

          'test-compile':
            ModuleCommand := TTestCompileCommand.Create(ModuleConfig, FProfileIds);

          'test':
            ModuleCommand := TTestCommand.Create(ModuleConfig, FProfileIds);

          'install':
            ModuleCommand := TInstallCommand.Create(ModuleConfig, FProfileIds);

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
            P := New(PModuleResult);
            P^.Name := Module.Name;
            P^.Status := msFailure;
            P^.ElapsedSecs := 0;
            FResults.Add(P);
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
              ElapsedSecs := (Now - ModuleStartTime) * 86400.0;
              Inc(FModulesBuilt);
              P := New(PModuleResult);
              P^.Name := Module.Name;
              P^.Status := msSuccess;
              P^.ElapsedSecs := ElapsedSecs;
              FResults.Add(P);
              TUtils.LogSeparator;
              TUtils.LogInfo(Format('BUILD SUCCESS [%7.3f s]', [ElapsedSecs]));
              TUtils.LogSeparator;
            end
            else
            begin
              ElapsedSecs := (Now - ModuleStartTime) * 86400.0;
              Inc(FModulesFailed);
              Result := 1;
              P := New(PModuleResult);
              P^.Name := Module.Name;
              P^.Status := msFailure;
              P^.ElapsedSecs := ElapsedSecs;
              FResults.Add(P);
              TUtils.LogSeparator;
              TUtils.LogInfo(Format('BUILD FAILURE [%7.3f s]', [ElapsedSecs]));
              TUtils.LogSeparator;
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

    { Print reactor summary }
    PrintReactorSummary((Now - OverallStartTime) * 86400.0);

    if FModulesFailed > 0 then
      Result := 1;

  finally
    BuildOrder.Free;
  end;
end;

procedure TReactorCommand.PrintReactorSummary(ATotalSecs: Double);
var
  I: Integer;
  P: PModuleResult;
  MaxNameLen, DotCount: Integer;
  ModuleName, Dots, StatusStr, Line: string;
begin
  { Find longest module name for dot alignment }
  MaxNameLen := 0;
  for I := 0 to FResults.Count - 1 do
  begin
    P := PModuleResult(FResults[I]);
    if Length(P^.Name) > MaxNameLen then
      MaxNameLen := Length(P^.Name);
  end;

  TUtils.LogSeparator;
  TUtils.LogInfo('Reactor Summary:');
  TUtils.LogInfo('');

  for I := 0 to FResults.Count - 1 do
  begin
    P := PModuleResult(FResults[I]);
    ModuleName := P^.Name;

    { Pad with dots so all status columns align }
    DotCount := MaxNameLen - Length(ModuleName) + 4;
    Dots := StringOfChar('.', DotCount);

    case P^.Status of
      msSuccess: StatusStr := 'SUCCESS';
      msFailure: StatusStr := 'FAILURE';
      msSkipped: StatusStr := 'SKIPPED';
    end;

    if P^.Status = msSkipped then
      Line := ModuleName + ' ' + Dots + ' ' + StatusStr
    else
      Line := ModuleName + ' ' + Dots + ' ' + StatusStr + Format(' [%7.3f s]', [P^.ElapsedSecs]);

    TUtils.LogInfo(Line);
  end;

  TUtils.LogSeparator;
  if FModulesFailed = 0 then
    TUtils.LogInfo('BUILD SUCCESS')
  else
    TUtils.LogInfo('BUILD FAILURE');
  TUtils.LogSeparator;
  TUtils.LogInfo(Format('Total time: %.3f s', [ATotalSecs]));
  TUtils.LogSeparator;
end;

function TReactorCommand.GetDependencies: TBuildCommandList;
begin
  { Reactor builds have no build-goal dependencies (no pre-requisite goals) }
  Result := TBuildCommandList.Create;
  Result.FreeObjects := False;
end;

end.
