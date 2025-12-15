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
    procedure DisplayDependencyGraph;
  protected
    function GetName: string; override;
  public
    constructor Create(AConfig: TProjectConfig; AProfileIds: TStringList;
      ARegistry: TModuleRegistry; const AGoalName: string); reintroduce;
    destructor Destroy; override;

    function Execute: Integer; override;
    function GetDependencies: TBuildCommandList; override;

    property Registry: TModuleRegistry read FRegistry;
    property GoalName: string read FGoalName;
    property ModulesBuilt: Integer read FModulesBuilt;
    property ModulesFailed: Integer read FModulesFailed;
  end;

implementation

{ TReactorCommand }

constructor TReactorCommand.Create(AConfig: TProjectConfig; AProfileIds: TStringList;
  ARegistry: TModuleRegistry; const AGoalName: string);
begin
  inherited Create(AConfig, AProfileIds);
  FRegistry := ARegistry;
  FGoalName := AGoalName;
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
    ModuleCount := BuildOrder.Count;

    if ModuleCount = 0 then
    begin
      TUtils.LogInfo('No modules to build');
      Exit(0);
    end;

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
