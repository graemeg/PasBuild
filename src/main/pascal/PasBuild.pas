{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

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
  PasBuild.Command.ProcessResources,
  PasBuild.Command.Compile,
  PasBuild.Command.ProcessTestResources,
  PasBuild.Command.Test,
  PasBuild.Command.Package,
  PasBuild.Command.SourcePackage,
  PasBuild.Command.AggregatedPackage,
  PasBuild.Command.AggregatedSourcePackage,
  PasBuild.Command.Init,
  PasBuild.Command.Reactor,
  PasBuild.ModuleDiscovery,
  PasBuild.Utils;

var
  Args: TCommandLineArgs;
  Config: TProjectConfig;
  Executor: TCommandExecutor;
  Command: TBuildCommand;
  Registry: TModuleRegistry;
  AggregatorDir: string;

begin
  WriteLn('[INFO] PasBuild ', PASBUILD_VERSION);
  WriteLn('[INFO] Copyright (c) 2025 by Graeme Geldenhuys');
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
      Config := TConfigLoader.LoadProjectXML(Args.ProjectFile);
    except
    on E: EProjectConfigError do
    begin
      TUtils.LogError(E.Message);
      ExitCode := 1;
      Exit;
    end;
    on E: Exception do
    begin
      TUtils.LogError('Failed to load ' + Args.ProjectFile + ': ' + E.Message);
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
    Registry := nil;
    try
      Command := nil;

      // Check if this is a multi-module aggregator project (packaging=pom)
      // If so, discover modules and create reactor command
      if (Args.Goal <> bgInit) and (Config.BuildConfig.ProjectType = ptPom) then
      begin
        try
          // Discover all modules in this aggregator project
          // Resolve to absolute path in case relative path was provided
          AggregatorDir := ExpandFileName(Args.ProjectFile);
          if not FileExists(AggregatorDir) then
          begin
            // Maybe just the directory was passed, try appending project.xml
            AggregatorDir := ExpandFileName(IncludeTrailingPathDelimiter(Args.ProjectFile) + 'project.xml');
          end;
          Registry := TModuleDiscoverer.DiscoverModules(AggregatorDir);

          // Create reactor command to build all modules
          case Args.Goal of
            bgClean:
              Command := TReactorCommand.Create(Config, Args.ProfileIds, Registry, 'clean', Args.SelectedModule);

            bgCompile:
              Command := TReactorCommand.Create(Config, Args.ProfileIds, Registry, 'compile', Args.SelectedModule);

            bgTest:
              Command := TReactorCommand.Create(Config, Args.ProfileIds, Registry, 'test', Args.SelectedModule);

            bgPackage:
              Command := TAggregatedPackageCommand.Create(Config, Args.ProfileIds, Registry);

            bgSourcePackage:
              Command := TAggregatedSourcePackageCommand.Create(Config, Args.ProfileIds, Registry);

            else
              // For other goals, use single-module behavior
              Command := nil;
          end;
        except
        on E: EProjectConfigError do
        begin
          TUtils.LogError('Multi-module project discovery failed: ' + E.Message);
          ExitCode := 1;
          Exit;
        end;
        on E: Exception do
        begin
          TUtils.LogError('Error discovering modules: ' + E.Message);
          ExitCode := 1;
          Exit;
        end;
        end;
      end;

      // If not multi-module or unsupported multi-module goal, use single-module commands
      if Command = nil then
      begin
        case Args.Goal of
          bgClean:
            Command := TCleanCommand.Create(Config, Args.ProfileIds);

          bgProcessResources:
            Command := TProcessResourcesCommand.Create(Config, Config.ResourcesConfig, Config.BuildConfig.OutputDirectory);

          bgCompile:
            Command := TCompileCommand.Create(Config, Args.ProfileIds);

          bgProcessTestResources:
            Command := TProcessTestResourcesCommand.Create(Config, Config.TestResourcesConfig, Config.BuildConfig.OutputDirectory);

          bgTestCompile:
            Command := TTestCompileCommand.Create(Config, Args.ProfileIds);

          bgTest:
            Command := TTestCommand.Create(Config, Args.ProfileIds);

          bgPackage:
            Command := TPackageCommand.Create(Config, Args.ProfileIds);

          bgSourcePackage:
            Command := TSourcePackageCommand.Create(Config, Args.ProfileIds);

          bgInit:
            Command := TInitCommand.Create(Config, Args.ProfileIds);

          else
          begin
            TUtils.LogError('Unknown goal');
            ExitCode := 1;
            Exit;
          end;
        end;
      end;

      // Execute command
      if Assigned(Command) then
      begin
        try
          Command.Verbose := Args.Verbose;
          ExitCode := Executor.Execute(Command);
        finally
          Command.Free;
        end;
      end;

    finally
      if Registry <> nil then
        Registry.Free;
      Executor.Free;
    end;

  finally
    Args.ProfileIds.Free;
    Config.Free;
  end;
end.
