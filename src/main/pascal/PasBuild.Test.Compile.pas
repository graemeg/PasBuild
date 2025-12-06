{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

program PasBuild.Test.Compile;

{$mode objfpc}{$H+}

uses
  SysUtils,
  PasBuild.Types,
  PasBuild.Config,
  PasBuild.Command,
  PasBuild.Command.Compile,
  PasBuild.Utils;

var
  Config: TProjectConfig;
  Executor: TCommandExecutor;
  CompileCmd: TCompileCommand;
  ExitCode: Integer;

begin
  WriteLn('=== Testing Compile Command ===');
  WriteLn;

  try
    // Load project configuration
    Config := TConfigLoader.LoadProjectXML('project.xml');
    try
      WriteLn('[Test 1: Compile without profile]');
      Executor := TCommandExecutor.Create;
      try
        CompileCmd := TCompileCommand.Create(Config, '');
        try
          ExitCode := Executor.Execute(CompileCmd);
        finally
          CompileCmd.Free;
        end;
      finally
        Executor.Free;
      end;

      WriteLn;
      WriteLn('========================================');
      WriteLn;

      WriteLn('[Test 2: Compile with debug profile]');
      Executor := TCommandExecutor.Create;
      try
        CompileCmd := TCompileCommand.Create(Config, 'debug');
        try
          ExitCode := Executor.Execute(CompileCmd);
        finally
          CompileCmd.Free;
        end;
      finally
        Executor.Free;
      end;

      WriteLn;
      if ExitCode = 0 then
        WriteLn('[SUCCESS] Compile command tests passed')
      else
        WriteLn('[FAILURE] Compile command failed with exit code: ', ExitCode);

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
