program PasBuild.Test.Clean;

{$mode objfpc}{$H+}

uses
  SysUtils,
  PasBuild.Types,
  PasBuild.Config,
  PasBuild.Command,
  PasBuild.Command.Clean,
  PasBuild.Utils;

var
  Config: TProjectConfig;
  Executor: TCommandExecutor;
  CleanCmd: TCleanCommand;
  ExitCode: Integer;

begin
  WriteLn('=== Testing Clean Command ===');
  WriteLn;

  try
    // Load project configuration
    Config := TConfigLoader.LoadProjectXML('project.xml');
    try
      // Create command executor
      Executor := TCommandExecutor.Create;
      try
        // Create clean command
        CleanCmd := TCleanCommand.Create(Config, '');
        try
          // Execute clean
          ExitCode := Executor.Execute(CleanCmd);

          WriteLn;
          if ExitCode = 0 then
            WriteLn('[SUCCESS] Clean command executed successfully')
          else
            WriteLn('[FAILURE] Clean command failed with exit code: ', ExitCode);

        finally
          CleanCmd.Free;
        end;
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
