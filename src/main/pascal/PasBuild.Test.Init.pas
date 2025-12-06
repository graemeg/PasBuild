program PasBuild.Test.Init;

{$mode objfpc}{$H+}

uses
  SysUtils,
  PasBuild.Types,
  PasBuild.Config,
  PasBuild.Command,
  PasBuild.Command.Init,
  PasBuild.Utils;

var
  Config: TProjectConfig;
  Executor: TCommandExecutor;
  InitCmd: TInitCommand;
  ExitCode: Integer;

begin
  WriteLn('=== Testing Init Command ===');
  WriteLn;
  WriteLn('NOTE: This test requires manual input');
  WriteLn('      Press ENTER to accept defaults');
  WriteLn;

  try
    // Create a minimal config (init doesn't use it, but Command base class needs it)
    Config := TProjectConfig.Create;
    try
      Executor := TCommandExecutor.Create;
      try
        InitCmd := TInitCommand.Create(Config, '');
        try
          ExitCode := Executor.Execute(InitCmd);
        finally
          InitCmd.Free;
        end;
      finally
        Executor.Free;
      end;

      WriteLn;
      if ExitCode = 0 then
        WriteLn('[SUCCESS] Init command test passed')
      else
        WriteLn('[FAILURE] Init command failed with exit code: ', ExitCode);

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
