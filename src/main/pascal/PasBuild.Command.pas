{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Command;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  PasBuild.Types;

type
  { Forward declaration }
  TBuildCommand = class;

  { Command list type }
  TBuildCommandList = specialize TFPGObjectList<TBuildCommand>;

  { Abstract base command - Command Pattern }
  TBuildCommand = class
  protected
    FConfig: TProjectConfig;
    FProfileId: string;
    function GetName: string; virtual; abstract;
  public
    constructor Create(AConfig: TProjectConfig; const AProfileId: string); virtual;

    { Execute the command }
    function Execute: Integer; virtual; abstract;

    { Get command dependencies (goals that must run first) }
    function GetDependencies: TBuildCommandList; virtual;

    property Name: string read GetName;
    property Config: TProjectConfig read FConfig;
    property ProfileId: string read FProfileId;
  end;

  { Command executor - executes commands with dependency resolution }
  TCommandExecutor = class
  private
    FExecutedCommands: TStringList;
    function HasExecuted(const ACommandName: string): Boolean;
    procedure MarkExecuted(const ACommandName: string);
  public
    constructor Create;
    destructor Destroy; override;

    { Execute a command and all its dependencies }
    function Execute(ACommand: TBuildCommand): Integer;

    { Execute a list of commands in order }
    function ExecuteAll(ACommands: TBuildCommandList): Integer;
  end;

implementation

uses
  PasBuild.Utils;

{ TBuildCommand }

constructor TBuildCommand.Create(AConfig: TProjectConfig; const AProfileId: string);
begin
  inherited Create;
  FConfig := AConfig;
  FProfileId := AProfileId;
end;

function TBuildCommand.GetDependencies: TBuildCommandList;
begin
  // Default: no dependencies
  Result := TBuildCommandList.Create;
  Result.FreeObjects := False; // Dependencies are owned elsewhere
end;

{ TCommandExecutor }

constructor TCommandExecutor.Create;
begin
  inherited Create;
  FExecutedCommands := TStringList.Create;
  FExecutedCommands.Sorted := True;
  FExecutedCommands.Duplicates := dupIgnore;
end;

destructor TCommandExecutor.Destroy;
begin
  FExecutedCommands.Free;
  inherited Destroy;
end;

function TCommandExecutor.HasExecuted(const ACommandName: string): Boolean;
begin
  Result := FExecutedCommands.IndexOf(ACommandName) >= 0;
end;

procedure TCommandExecutor.MarkExecuted(const ACommandName: string);
begin
  FExecutedCommands.Add(ACommandName);
end;

function TCommandExecutor.Execute(ACommand: TBuildCommand): Integer;
var
  Dependencies: TBuildCommandList;
  Dependency: TBuildCommand;
begin
  Result := 0;

  // Skip if already executed
  if HasExecuted(ACommand.Name) then
  begin
    TUtils.LogInfo('Skipping ' + ACommand.Name + ' (already executed)');
    Exit;
  end;

  // Execute dependencies first
  Dependencies := ACommand.GetDependencies;
  try
    for Dependency in Dependencies do
    begin
      Result := Execute(Dependency);
      if Result <> 0 then
      begin
        TUtils.LogError('Dependency failed: ' + Dependency.Name);
        Exit;
      end;
    end;
  finally
    Dependencies.Free;
  end;

  // Execute the command itself
  TUtils.LogInfo('Executing goal: ' + ACommand.Name);
  Result := ACommand.Execute;

  if Result = 0 then
    MarkExecuted(ACommand.Name)
  else
    TUtils.LogError('Goal failed: ' + ACommand.Name);
end;

function TCommandExecutor.ExecuteAll(ACommands: TBuildCommandList): Integer;
var
  Command: TBuildCommand;
begin
  Result := 0;

  for Command in ACommands do
  begin
    Result := Execute(Command);
    if Result <> 0 then
      Exit;
  end;
end;

end.
