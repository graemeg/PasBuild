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
  PasBuild.Command.Install,
  PasBuild.Command.Init,
  PasBuild.Command.Reactor,
  PasBuild.Command.DependencyTree,
  PasBuild.Command.Resolve,
  PasBuild.ModuleDiscovery,
  PasBuild.Repository,
  PasBuild.Dependencies,
  PasBuild.Plugin,
  PasBuild.Utils,
  PasBuild.Compiler.Factory;

var
  Args: TCommandLineArgs;
  Config: TProjectConfig;
  Executor: TCommandExecutor;
  Command: TBuildCommand;
  Registry: TModuleRegistry;
  Resolver: TDependencyResolver;
  AggregatorDir: string;
  ProjectDir, OriginalDir: string;
  PluginPath: string;
  PhaseInfo: TPluginPhaseInfo;
  CompilerPath: string;

begin
  // Parse command line arguments
  Args := TArgumentParser.ParseArguments;

  // Suppress informational output for machine-consumable commands
  TUtils.QuietMode := Args.Goal = bgResolve;

  if not TUtils.QuietMode then
    TUtils.LogInfo('PasBuild ' + PASBUILD_VERSION + ' — Born ' + PASBUILD_BUILD_DATE + '. Raised by Graeme Geldenhuys.');


  // Resolve compiler executable and instantiate backend via factory.
  // Precedence: --compiler flag > PASBUILD_COMPILER env > PASBUILD_FPC env (deprecated) > 'fpc'
  begin
    if Args.CompilerExecutable <> '' then
      CompilerPath := Args.CompilerExecutable
    else if GetEnvironmentVariable('PASBUILD_COMPILER') <> '' then
      CompilerPath := GetEnvironmentVariable('PASBUILD_COMPILER')
    else if GetEnvironmentVariable('PASBUILD_FPC') <> '' then
    begin
      TUtils.LogWarning('PASBUILD_FPC is deprecated; use PASBUILD_COMPILER instead');
      CompilerPath := GetEnvironmentVariable('PASBUILD_FPC');
    end
    else
      CompilerPath := 'fpc';
    TUtils.SetCompilerBackend(TCompilerBackendFactory.CreateFromExecutable(CompilerPath));
    TUtils.LogInfo('Compiler: ' + TUtils.GetCompilerBackend.Name
      + ' (' + CompilerPath + ')');
  end;

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

  // Handle license
  if Args.ShowLicense then
  begin
    TArgumentParser.ShowLicense;
    ExitCode := 0;
    Exit;
  end;

  // Resolve project file path and change to its directory.
  // This allows running pasbuild from any directory using -f flag,
  // e.g.: pasbuild compile -f ../../../project.xml
  if Args.Goal <> bgInit then
  begin
    OriginalDir := GetCurrentDir;
    ProjectDir := ExtractFilePath(ExpandFileName(Args.ProjectFile));
    if ProjectDir <> '' then
    begin
      if not SetCurrentDir(ProjectDir) then
      begin
        TUtils.LogError('Cannot change to project directory: ' + ProjectDir);
        ExitCode := 1;
        Exit;
      end;
      if not SameText(ExcludeTrailingPathDelimiter(ProjectDir),
                      ExcludeTrailingPathDelimiter(OriginalDir)) then
        TUtils.LogInfo('Changing to project directory: ' + ExcludeTrailingPathDelimiter(ProjectDir));
      Args.ProjectFile := ExtractFileName(Args.ProjectFile);
    end;
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

    // Resolve external dependencies (for non-aggregator projects with <dependencies>)
    if (Args.Goal <> bgInit) and (Config.BuildConfig.ProjectType <> ptPom) and
       (Config.Dependencies.Count > 0) then
    begin
      Resolver := TDependencyResolver.Create;
      try
        Resolver.Verbose := Args.Verbose;
        try
          Resolver.ResolveDependencies(Config);
        except
          on E: EDependencyError do
          begin
            TUtils.LogError(E.Message);
            ExitCode := 1;
            Exit;
          end;
        end;
      finally
        Resolver.Free;
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
          Registry := TModuleDiscoverer.DiscoverModules(AggregatorDir,
            Args.ForceAllModules or (Args.SelectedModule <> ''));

          // Create reactor command to build all modules
          case Args.Goal of
            bgClean:
              Command := TReactorCommand.Create(Config, Args.ProfileIds, Registry, 'clean', Args.SelectedModule);

            bgCompile:
              Command := TReactorCommand.Create(Config, Args.ProfileIds, Registry, 'compile', Args.SelectedModule);

            bgTestCompile:
              Command := TReactorCommand.Create(Config, Args.ProfileIds, Registry, 'test-compile', Args.SelectedModule);

            bgTest:
              Command := TReactorCommand.Create(Config, Args.ProfileIds, Registry, 'test', Args.SelectedModule);

            bgPackage:
              Command := TAggregatedPackageCommand.Create(Config, Args.ProfileIds, Registry, Args.SelectedModule);

            bgSourcePackage:
              Command := TAggregatedSourcePackageCommand.Create(Config, Args.ProfileIds, Registry, Args.SelectedModule);

            bgInstall:
              Command := TReactorCommand.Create(Config, Args.ProfileIds, Registry, 'install', Args.SelectedModule);

            bgDependencyTree:
              Command := TDependencyTreeCommand.CreateMultiModule(
                Config, Args.ProfileIds, Registry, Args.SelectedModule);

            bgResolve:
              Command := TResolveCommand.CreateMultiModule(
                Config, Args.ProfileIds, Registry, Args.SelectedModule);

            else
            begin
              // Not a built-in multi-module goal — try plugin resolution
              PluginPath := TPluginDiscovery.DiscoverPlugin(Args.GoalString);
              if PluginPath <> '' then
              begin
                PhaseInfo := TPluginDiscovery.QueryPluginPhase(PluginPath);
                if not PhaseInfo.Valid then
                  TUtils.LogWarning('Plugin ' + Args.GoalString + ' did not declare a valid phase, running without dependencies');
                Command := TPluginCommand.Create(Config, Args.ProfileIds,
                  PluginPath, Args.GoalString, PhaseInfo.AfterGoal, Registry);
              end
              else
                // Fall through to single-module behavior
                Command := nil;
            end;
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

          bgInstall:
            Command := TInstallCommand.Create(Config, Args.ProfileIds);

          bgDependencyTree:
            Command := TDependencyTreeCommand.Create(Config, Args.ProfileIds);

          bgResolve:
            Command := TResolveCommand.Create(Config, Args.ProfileIds);

          bgInit:
            Command := TInitCommand.Create(Config, Args.ProfileIds);

          else
          begin
            // Not a built-in goal — try plugin resolution
            PluginPath := TPluginDiscovery.DiscoverPlugin(Args.GoalString);
            if PluginPath = '' then
            begin
              TUtils.LogError('Unknown goal: ' + Args.GoalString);
              ExitCode := 1;
              Exit;
            end;

            PhaseInfo := TPluginDiscovery.QueryPluginPhase(PluginPath);
            if not PhaseInfo.Valid then
              TUtils.LogWarning('Plugin ' + Args.GoalString + ' did not declare a valid phase, running without dependencies');

            Command := TPluginCommand.Create(Config, Args.ProfileIds,
              PluginPath, Args.GoalString, PhaseInfo.AfterGoal);
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
