{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

program PasBuild.Test.Command;

{$mode objfpc}{$H+}

uses
  SysUtils,
  PasBuild.Types,
  PasBuild.Config,
  PasBuild.Command,
  PasBuild.Command.Clean,
  PasBuild.Utils;

type
  { Mock command for testing dependency resolution }
  TMockCommand = class(TBuildCommand)
  private
    FCommandName: string;
  protected
    function GetName: string; override;
  public
    constructor Create(AConfig: TProjectConfig; const AProfileId: string; const AName: string);
    function Execute: Integer; override;
  end;

constructor TMockCommand.Create(AConfig: TProjectConfig; const AProfileId: string; const AName: string);
begin
  inherited Create(AConfig, AProfileId);
  FCommandName := AName;
end;

function TMockCommand.GetName: string;
begin
  Result := FCommandName;
end;

function TMockCommand.Execute: Integer;
begin
  TUtils.LogInfo('Executing mock command: ' + FCommandName);
  Result := 0;
end;

var
  Config: TProjectConfig;
  Executor: TCommandExecutor;
  Clean1, Clean2: TCleanCommand;
  Mock1: TMockCommand;
  ExitCode: Integer;

begin
  WriteLn('=== Testing Command Pattern ===');
  WriteLn;

  try
    Config := TConfigLoader.LoadProjectXML('project.xml');
    try
      Executor := TCommandExecutor.Create;
      try
        WriteLn('[Test 1: Execute same command twice - should skip second time]');
        Clean1 := TCleanCommand.Create(Config, '');
        Clean2 := TCleanCommand.Create(Config, '');
        try
          ExitCode := Executor.Execute(Clean1);
          WriteLn;
          ExitCode := Executor.Execute(Clean2);  // Should be skipped
          WriteLn;
        finally
          Clean1.Free;
          Clean2.Free;
        end;

        WriteLn('[Test 2: Execute mock command]');
        Mock1 := TMockCommand.Create(Config, '', 'test-mock');
        try
          ExitCode := Executor.Execute(Mock1);
        finally
          Mock1.Free;
        end;

        WriteLn;
        if ExitCode = 0 then
          WriteLn('[SUCCESS] Command pattern tests passed')
        else
          WriteLn('[FAILURE] Command pattern tests failed');

      finally
        Executor.Free;
      end;
    finally
      Config.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn('[ERROR] ', E.Message);
      Halt(1);
    end;
  end;
end.
