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
    function BuildCompilerCommand: string;
  public
    function Execute: Integer; override;
  end;

implementation

{ TCompileCommand }

function TCompileCommand.GetName: string;
begin
  Result := 'compile';
end;

function TCompileCommand.BuildCompilerCommand: string;
var
  SourcePath, OutputDir, ExeName: string;
  UnitPaths, IncludePaths, ActiveDefines: TStringList;
  UnitPath, IncludePath, Define, Option: string;
  Profile: TProfile;
  ConditionalPath: TConditionalPath;
  BasePath: string;
  I: Integer;
begin
  // Base command with default flags
  Result := 'fpc -Mobjfpc -O1';

  // Source file path (normalized for cross-platform)
  SourcePath := TUtils.NormalizePath('src/main/pascal/' + Config.BuildConfig.MainSource);
  Result := Result + ' ' + SourcePath;

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

    // Add profile defines if profile is active
    if ProfileId <> '' then
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
            UnitPaths.Add(BasePath + DirectorySeparator + TUtils.NormalizePath(ConditionalPath.Path));
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
      try
        for UnitPath in UnitPaths do
          Result := Result + ' -Fu' + UnitPath;
      finally
        UnitPaths.Free;
      end;
    end;

    // Add include search paths (-Fi)
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
            UnitPath := BasePath + DirectorySeparator + TUtils.NormalizePath(ConditionalPath.Path);
            if TUtils.DirectoryContainsIncludeFiles(UnitPath) then
              IncludePaths.Add(UnitPath);
          end;
        end;
      end
      else
      begin
        // Auto-scan mode: Use full auto-scan with conditional filtering
        IncludePaths.Free;
        IncludePaths := TUtils.ScanForIncludePathsFiltered(
          BasePath,
          Config.BuildConfig.IncludePaths,
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

    // Add profile-specific defines and compiler options
    if ProfileId <> '' then
    begin
      Profile := Config.Profiles.FindById(ProfileId);
      if Assigned(Profile) then
      begin
        // Profile defines
        for Define in Profile.Defines do
          Result := Result + ' -d' + Define;

        // Profile compiler options (these can override defaults)
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
  MainSourcePath, OutputDir, UnitsDir, BootstrapPath: string;
  Command: string;
  ActiveDefines: TStringList;
  Profile: TProfile;
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

      if ProfileId <> '' then
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

  // Build compiler command
  Command := BuildCompilerCommand;

  TUtils.LogInfo('Build command: ' + Command);
  WriteLn;

  // Execute FPC with real-time output
  Result := TUtils.ExecuteProcess(Command, True);

  WriteLn;
  if Result = 0 then
    TUtils.LogInfo('Build successful')
  else
    TUtils.LogError('Build failed with exit code: ' + IntToStr(Result));
end;

end.
