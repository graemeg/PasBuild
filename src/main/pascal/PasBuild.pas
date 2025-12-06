program PasBuild;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  PasBuild.Types,
  PasBuild.Config,
  PasBuild.CLI,
  PasBuild.Command,
  PasBuild.Command.Clean,
  PasBuild.Command.Compile,
  PasBuild.Command.Package,
  PasBuild.Command.Init,
  PasBuild.Utils;

var
  Args: TCommandLineArgs;
  Config: TProjectConfig;
  Executor: TCommandExecutor;
  Command: TBuildCommand;

begin
  WriteLn('[INFO] PasBuild ', PASBUILD_VERSION);
  WriteLn;

  // Parse command line arguments
  Args := TArgumentParser.ParseArguments;

  // Handle help
  if Args.ShowHelp then
  begin
    if Args.ErrorMessage <> '' then
    begin
      TUtils.LogError(Args.ErrorMessage);
      WriteLn;
    end;
    TArgumentParser.ShowHelp;
    if Args.ErrorMessage <> '' then
      ExitCode := 1
    else
      ExitCode := 0;
    Exit;
  end;

  // Handle version
  if Args.ShowVersion then
  begin
    TArgumentParser.ShowVersion;
    ExitCode := 0;
    Exit;
  end;

  // Load project configuration (skip for init goal)
  if Args.Goal = bgInit then
  begin
    // Init goal creates project.xml, so create empty config
    Config := TProjectConfig.Create;
  end
  else
  begin
    try
      Config := TConfigLoader.LoadProjectXML('project.xml');
    except
    on E: EProjectConfigError do
    begin
      TUtils.LogError(E.Message);
      ExitCode := 1;
      Exit;
    end;
    on E: Exception do
    begin
      TUtils.LogError('Failed to load project.xml: ' + E.Message);
      ExitCode := 1;
      Exit;
    end;
    end;
  end;

  try
    // Validate configuration (skip for init goal)
    if Args.Goal <> bgInit then
    begin
      try
        TConfigLoader.ValidateConfig(Config);
      except
      on E: EProjectConfigError do
      begin
        TUtils.LogError(E.Message);
        ExitCode := 1;
        Exit;
      end;
      end;
    end;

    // Create command executor
    Executor := TCommandExecutor.Create;
    try
      Command := nil;

      // Create appropriate command based on goal
      case Args.Goal of
        bgClean:
          Command := TCleanCommand.Create(Config, Args.ProfileId);

        bgCompile:
          Command := TCompileCommand.Create(Config, Args.ProfileId);

        bgPackage:
          Command := TPackageCommand.Create(Config, Args.ProfileId);

        bgInit:
          Command := TInitCommand.Create(Config, Args.ProfileId);

        else
        begin
          TUtils.LogError('Unknown goal');
          ExitCode := 1;
          Exit;
        end;
      end;

      // Execute command
      if Assigned(Command) then
      begin
        try
          ExitCode := Executor.Execute(Command);
        finally
          Command.Free;
        end;
      end;

    finally
      Executor.Free;
    end;

  finally
    Config.Free;
  end;
end.
