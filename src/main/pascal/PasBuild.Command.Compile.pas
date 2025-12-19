{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Command.Compile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  PasBuild.Types,
  PasBuild.Command,
  PasBuild.Utils,
  PasBuild.Bootstrap;

type
  { Compile command - builds the executable }
  TCompileCommand = class(TBuildCommand)
  protected
    function GetName: string; override;
    function BuildCompilerCommand(const ASourcePath: string): string;
  public
    function Execute: Integer; override;
    function GetDependencies: TBuildCommandList; override;
  end;

implementation

uses
  PasBuild.Command.ProcessResources;

{ TCompileCommand }

function TCompileCommand.GetName: string;
begin
  Result := 'compile';
end;

function TCompileCommand.BuildCompilerCommand(const ASourcePath: string): string;
var
  OutputDir, ExeName: string;
  UnitPaths, IncludePaths, ActiveDefines: TStringList;
  UnitPath, IncludePath, Define, Option, ProfileId: string;
  Profile: TProfile;
  ConditionalPath: TConditionalPath;
  BasePath, s: string;
  I: Integer;
begin
  // Base command with default flags
  Result := 'fpc -Mobjfpc -O1';

  // Source file path (passed as parameter)
  Result := Result + ' ' + ASourcePath;

  // Output directory
  OutputDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory);
  Result := Result + ' -FE' + OutputDir;

  // Unit output directory
  Result := Result + ' -FU' + OutputDir + DirectorySeparator + 'units';

  // Executable name
  ExeName := Config.BuildConfig.ExecutableName;
  if ExeName <> '' then
    Result := Result + ' -o' + ExeName + TUtils.GetPlatformExecutableSuffix;

  // Collect all active defines (global + profile)
  ActiveDefines := TStringList.Create;
  try
    ActiveDefines.Duplicates := dupIgnore;
    ActiveDefines.Sorted := True;

    // Add global defines
    ActiveDefines.AddStrings(Config.BuildConfig.Defines);

    // Add profile defines for each active profile (applied in order)
    for ProfileId in ProfileIds do
    begin
      Profile := Config.Profiles.FindById(ProfileId);
      if Assigned(Profile) then
      begin
        TUtils.LogInfo('Activating profile: ' + ProfileId);
        ActiveDefines.AddStrings(Profile.Defines);
      end
      else
        TUtils.LogWarning('Profile not found: ' + ProfileId);
    end;

    // Add unit search paths (-Fu)
    BasePath := TUtils.NormalizePath('src/main/pascal');

    if not Config.BuildConfig.ManualUnitPaths then
      // Always add the base source directory first
      Result := Result + ' -Fu' + BasePath;

    if Config.BuildConfig.ManualUnitPaths then
    begin
      // Manual mode: Only use paths explicitly listed in <unitPaths>
      // Apply conditional filtering to manual paths
      UnitPaths := TStringList.Create;
      try
        UnitPaths.Duplicates := dupIgnore;
        UnitPaths.Sorted := True;

        for I := 0 to Config.BuildConfig.UnitPaths.Count - 1 do
        begin
          ConditionalPath := Config.BuildConfig.UnitPaths[I];
          if TUtils.IsConditionMet(ConditionalPath.Condition, ActiveDefines) then
            UnitPaths.Add(TUtils.NormalizePath(ConditionalPath.Path));
        end;

        for UnitPath in UnitPaths do
          Result := Result + ' -Fu' + UnitPath;
      finally
        UnitPaths.Free;
      end;
    end
    else
    begin
      // Auto-scan mode (default): Scan all directories, apply conditional filtering
      UnitPaths := TUtils.ScanForUnitPathsFiltered(
        BasePath,
        Config.BuildConfig.UnitPaths,
        ActiveDefines
      );

      // If manual paths where listed, add them too
      for I := 0 to Config.BuildConfig.UnitPaths.Count - 1 do
      begin
        ConditionalPath := Config.BuildConfig.UnitPaths[I];
        if TUtils.IsConditionMet(ConditionalPath.Condition, ActiveDefines) then
        begin
          s := TUtils.NormalizePath(ConditionalPath.Path);
          writeln('[DEBUG] user defined UnitPath: ' + s);
          UnitPaths.Add(s);
        end;
      end;

      try
        for UnitPath in UnitPaths do
          Result := Result + ' -Fu' + UnitPath;
      finally
        UnitPaths.Free;
      end;
    end;

    // Add resolved module dependency paths
    for I := 0 to Config.BuildConfig.ResolvedModulePaths.Count - 1 do
    begin
      UnitPath := TUtils.NormalizePath(Config.BuildConfig.ResolvedModulePaths[I]);
      Result := Result + ' -Fu' + UnitPath;
    end;

    // Add include search paths (-Fi)
    // Always add output directory first (for filtered resource includes like version.inc)
    Result := Result + ' -Fi' + OutputDir;

    // Check all unit paths (both base and subdirs) for *.inc files
    IncludePaths := TStringList.Create;
    try
      IncludePaths.Duplicates := dupIgnore;
      IncludePaths.Sorted := True;

      if Config.BuildConfig.ManualUnitPaths then
      begin
        // Manual mode: Check base path + manually listed paths for *.inc files
        // Check base directory
        if TUtils.DirectoryContainsIncludeFiles(BasePath) then
          IncludePaths.Add(BasePath);

        // Check each manually specified unit path
        for I := 0 to Config.BuildConfig.UnitPaths.Count - 1 do
        begin
          ConditionalPath := Config.BuildConfig.UnitPaths[I];
          if TUtils.IsConditionMet(ConditionalPath.Condition, ActiveDefines) then
          begin
            UnitPath := TUtils.NormalizePath(ConditionalPath.Path);
            if TUtils.DirectoryContainsIncludeFiles(UnitPath) then
              IncludePaths.Add(UnitPath);
          end;
        end;
      end
      else
      begin
        // Auto-scan mode: Use full auto-scan with conditional filtering
        IncludePaths.Free;
        // Use IncludePaths if specified, otherwise fall back to UnitPaths
        if Config.BuildConfig.IncludePaths.Count > 0 then
          IncludePaths := TUtils.ScanForIncludePathsFiltered(
            BasePath,
            Config.BuildConfig.IncludePaths,
            ActiveDefines
          )
        else
          IncludePaths := TUtils.ScanForIncludePathsFiltered(
            BasePath,
            Config.BuildConfig.UnitPaths,
            ActiveDefines
          );
      end;

      for IncludePath in IncludePaths do
        Result := Result + ' -Fi' + IncludePath;
    finally
      IncludePaths.Free;
    end;

    // Add global defines to compiler
    for Define in Config.BuildConfig.Defines do
      Result := Result + ' -d' + Define;

    // Add global compiler options (extends defaults)
    for Option in Config.BuildConfig.CompilerOptions do
      Result := Result + ' ' + Option;

    // Add profile-specific defines and compiler options (applied in order)
    for ProfileId in ProfileIds do
    begin
      Profile := Config.Profiles.FindById(ProfileId);
      if Assigned(Profile) then
      begin
        // Profile defines
        for Define in Profile.Defines do
          Result := Result + ' -d' + Define;

        // Profile compiler options (these can override defaults and global options)
        for Option in Profile.CompilerOptions do
          Result := Result + ' ' + Option;
      end;
    end;

  finally
    ActiveDefines.Free;
  end;
end;

function TCompileCommand.Execute: Integer;
var
  MainSourcePath, OutputDir, UnitsDir, BootstrapPath, ProfileId: string;
  Command: string;
  ActiveDefines: TStringList;
  Profile: TProfile;
  StatusDir, LogFile: string;
  SourceFiles, IncludeFiles: TStringList;
begin
  Result := 0;

  TUtils.LogInfo('Compiling project...');

  // Verify directory layout
  if not TUtils.VerifyDirectoryLayout('.') then
  begin
    Result := 1;
    Exit;
  end;

  // Create output directories first (needed for bootstrap generation)
  OutputDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory);
  UnitsDir := OutputDir + DirectorySeparator + 'units';

  if not ForceDirectories(OutputDir) then
  begin
    TUtils.LogError('Failed to create output directory: ' + OutputDir);
    Result := 1;
    Exit;
  end;

  if not ForceDirectories(UnitsDir) then
  begin
    TUtils.LogError('Failed to create units directory: ' + UnitsDir);
    Result := 1;
    Exit;
  end;

  // For library projects, generate bootstrap program
  if Config.BuildConfig.ProjectType = ptLibrary then
  begin
    BootstrapPath := OutputDir + DirectorySeparator + 'bootstrap_program.pas';

    // Collect active defines (same logic as BuildCompilerCommand)
    ActiveDefines := TStringList.Create;
    try
      ActiveDefines.Duplicates := dupIgnore;
      ActiveDefines.Sorted := True;
      ActiveDefines.AddStrings(Config.BuildConfig.Defines);

      // Add defines from each active profile in order
      for ProfileId in ProfileIds do
      begin
        Profile := Config.Profiles.FindById(ProfileId);
        if Assigned(Profile) then
          ActiveDefines.AddStrings(Profile.Defines);
      end;

      if not TBootstrapGenerator.GenerateBootstrapProgram(Config, BootstrapPath, ActiveDefines) then
      begin
        TUtils.LogError('Failed to generate bootstrap program');
        Result := 1;
        Exit;
      end;
    finally
      ActiveDefines.Free;
    end;

    // Use bootstrap program as main source
    MainSourcePath := BootstrapPath;
  end
  else
  begin
    // Application project: Check if main source file exists
    MainSourcePath := TUtils.NormalizePath('src/main/pascal/' + Config.BuildConfig.MainSource);
    if not FileExists(MainSourcePath) then
    begin
      TUtils.LogError('Main source file not found: ' + MainSourcePath);
      Result := 1;
      Exit;
    end;
  end;

  // Check if FPC is available
  if not TUtils.IsFPCAvailable then
  begin
    TUtils.LogError('Free Pascal Compiler (fpc) not found in PATH');
    TUtils.LogError('Please install FPC or add it to your PATH');
    Result := 1;
    Exit;
  end;

  // Create status directory and collect source information
  try
    StatusDir := TUtils.CreateStatusDirectory('compile');

    // Collect and write source files list
    SourceFiles := TUtils.CollectSourceFiles(TUtils.NormalizePath('src/main/pascal'));
    try
      TUtils.LogInfo('Found ' + IntToStr(SourceFiles.Count) + ' source file(s)');
      TUtils.WriteListFile(StatusDir + DirectorySeparator + 'inputUnits.lst', SourceFiles);
    finally
      SourceFiles.Free;
    end;

    // Collect and write include files list
    IncludeFiles := TUtils.CollectIncludeFiles(TUtils.NormalizePath('src/main/pascal'));
    try
      if IncludeFiles.Count > 0 then
        TUtils.LogInfo('Found ' + IntToStr(IncludeFiles.Count) + ' include file(s)');
      TUtils.WriteListFile(StatusDir + DirectorySeparator + 'inputIncludeFiles.lst', IncludeFiles);
    finally
      IncludeFiles.Free;
    end;

    LogFile := StatusDir + DirectorySeparator + 'fpc.log';
  except
    on E: Exception do
    begin
      TUtils.LogWarning('Failed to create status directory: ' + E.Message);
      // Continue without status tracking
      StatusDir := '';
    end;
  end;

  // Build compiler command
  Command := BuildCompilerCommand(MainSourcePath);

  // Execute FPC (verbose mode shows full output, quiet mode logs to file)
  if FVerbose then
  begin
    // Verbose mode: Show full FPC output to console
    TUtils.LogInfo('Build command: ' + Command);
    WriteLn;
    Result := TUtils.ExecuteProcess(Command, True);
    WriteLn;
    if Result = 0 then
      TUtils.LogInfo('Build successful')
    else
      TUtils.LogError('Build failed with exit code: ' + IntToStr(Result));
  end
  else
  begin
    // Quiet mode: Clean console output, full output logged to file
    if StatusDir <> '' then
    begin
      Result := TUtils.ExecuteProcessWithLog(Command, LogFile, True);
      if Result = 0 then
        TUtils.LogInfo('Build successful (see ' + LogFile + ' for details)')
      else
        TUtils.LogError('Build failed with exit code: ' + IntToStr(Result) + ' (see ' + LogFile + ' for details)');
    end
    else
    begin
      // Fallback if status directory creation failed
      TUtils.LogInfo('Build command: ' + Command);
      WriteLn;
      Result := TUtils.ExecuteProcess(Command, True);
      WriteLn;
      if Result = 0 then
        TUtils.LogInfo('Build successful')
      else
        TUtils.LogError('Build failed with exit code: ' + IntToStr(Result));
    end;
  end;
end;

function TCompileCommand.GetDependencies: TBuildCommandList;
begin
  Result := TBuildCommandList.Create(False);
  try
    // compile depends on: process-resources
    Result.Add(TProcessResourcesCommand.Create(Config, Config.ResourcesConfig, Config.BuildConfig.OutputDirectory));
  except
    Result.Free;
    raise;
  end;
end;

end.
