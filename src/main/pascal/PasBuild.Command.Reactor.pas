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
  PasBuild.Utils;

type
  { TReactorCommand - Executes a goal on all modules in build order }
  TReactorCommand = class(TBuildCommand)
  private
    FRegistry: TModuleRegistry;
    FGoalName: string;
    FModulesBuilt: Integer;
    FModulesFailed: Integer;
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

function TReactorCommand.Execute: Integer;
var
  BuildOrder: TList;
  I: Integer;
  Module: TModuleInfo;
  ModuleCount: Integer;
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

      { Log module being built }
      TUtils.LogInfo('Building module ' + IntToStr(I + 1) + '/' + IntToStr(ModuleCount) + ': ' + Module.Name);

      { Resolve artifacts for this module (add dependency paths) }
      if Module.Config <> nil then
        FRegistry.ResolveArtifacts(Module);

      { In a full implementation, would execute the goal here }
      { For now, just count successful builds }
      Inc(FModulesBuilt);
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
