program PasBuild.Test.Package;

{$mode objfpc}{$H+}

uses
  SysUtils,
  PasBuild.Types,
  PasBuild.Config,
  PasBuild.Command,
  PasBuild.Command.Package,
  PasBuild.Utils;

var
  Config: TProjectConfig;
  Executor: TCommandExecutor;
  PackageCmd: TPackageCommand;
  ExitCode: Integer;

begin
  WriteLn('=== Testing Package Command ===');
  WriteLn;

  try
    // Load project configuration
    Config := TConfigLoader.LoadProjectXML('project.xml');
    try
      WriteLn('[Test: Package command with clean + compile dependencies]');
      Executor := TCommandExecutor.Create;
      try
        PackageCmd := TPackageCommand.Create(Config, '');
        try
          ExitCode := Executor.Execute(PackageCmd);
        finally
          PackageCmd.Free;
        end;
      finally
        Executor.Free;
      end;

      WriteLn;
      if ExitCode = 0 then
        WriteLn('[SUCCESS] Package command test passed')
      else
        WriteLn('[FAILURE] Package command failed with exit code: ', ExitCode);

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
