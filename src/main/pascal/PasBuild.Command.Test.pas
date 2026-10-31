{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Command.Test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  PasBuild.Types,
  PasBuild.Command,
  PasBuild.Command.Compile,
  PasBuild.Command.ProcessTestResources,
  PasBuild.Utils;

type
  { Test-compile command - compiles test code }
  TTestCompileCommand = class(TBuildCommand)
  protected
    function GetName: string; override;
    function BuildTestCompilerCommand(const ATestSourcePath: string): string;
  public
    function Execute: Integer; override;
    function GetDependencies: TBuildCommandList; override;
  end;

  { Test command - runs compiled tests }
  TTestCommand = class(TBuildCommand)
  protected
    function GetName: string; override;
    function BuildTestRunnerCommand(const ATestExecutable: string): string;
  public
    function Execute: Integer; override;
    function GetDependencies: TBuildCommandList; override;
  end;

implementation

procedure AppendFlag(var ACmd: string; const AFlag: string);
begin
  if AFlag <> '' then
    ACmd := ACmd + ' ' + AFlag;
end;

{ TTestCompileCommand }

function TTestCompileCommand.GetName: string;
begin
  Result := 'test-compile';
end;

function TTestCompileCommand.GetDependencies: TBuildCommandList;
begin
  Result := TBuildCommandList.Create(False);
  try
    // test-compile depends on: compile, process-test-resources
    Result.Add(TCompileCommand.Create(Config, ProfileIds));
    Result.Add(TProcessTestResourcesCommand.Create(Config, Config.TestResourcesConfig, Config.BuildConfig.OutputDirectory));
  except
    Result.Free;
    raise;
  end;
end;

function TTestCompileCommand.BuildTestCompilerCommand(const ATestSourcePath: string): string;
var
  OutputDir: string;
  UnitPaths, IncludePaths: TStringList;
  UnitPath, IncludePath, Define, Option: string;
  Profile: TProfile;
  ProfileId: string;
  ActiveDefines: TStringList;
  TestBaseDir: string;
  I: Integer;
begin
  // Base command with default flags
  OutputDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory);
  TestBaseDir := TUtils.NormalizePath(Config.TestConfig.SourceDirectory);

  Result := TUtils.GetCompilerBackend.BaseCommand;
  AppendFlag(Result, TUtils.GetCompilerBackend.SourceFlag(ATestSourcePath));
  AppendFlag(Result, TUtils.GetCompilerBackend.OutputFlags(
    OutputDir, 'TestRunner' + TUtils.GetPlatformExecutableSuffix));
  AppendFlag(Result, TUtils.GetCompilerBackend.UnitOutputDirFlag(
    OutputDir + DirectorySeparator + 'test-units'));
  AppendFlag(Result, TUtils.GetCompilerBackend.IncrementalFlag);

  // Link to already-compiled main units (compiled by 'compile' goal)
  AppendFlag(Result, TUtils.GetCompilerBackend.UnitPathFlag(
    OutputDir + DirectorySeparator + 'units'));

  // Add resolved module dependency paths (same paths used by the compile goal)
  for I := 0 to Config.BuildConfig.ResolvedModulePaths.Count - 1 do
  begin
    UnitPath := TUtils.NormalizePath(Config.BuildConfig.ResolvedModulePaths[I]);
    AppendFlag(Result, TUtils.GetCompilerBackend.UnitPathFlag(UnitPath));
  end;

  // Module dependency sources, for backends that compile them from source
  if TUtils.GetCompilerBackend.CompilesDependenciesFromSource then
    for I := 0 to Config.BuildConfig.ResolvedModuleSourcePaths.Count - 1 do
    begin
      UnitPath := TUtils.NormalizePath(Config.BuildConfig.ResolvedModuleSourcePaths[I]);
      AppendFlag(Result, TUtils.GetCompilerBackend.UnitPathFlag(UnitPath));
    end;

  // Add main source directory so test code can reference compiler units directly
  AppendFlag(Result, TUtils.GetCompilerBackend.UnitPathFlag(
    TUtils.NormalizePath(Config.BuildConfig.SourceDirectory)));

  // Add explicit <unitPaths> entries from the build config (e.g. RTL sources)
  for I := 0 to Config.BuildConfig.UnitPaths.Count - 1 do
    AppendFlag(Result, TUtils.GetCompilerBackend.UnitPathFlag(
      TUtils.NormalizePath(Config.BuildConfig.UnitPaths[I].Path)));

  // Add test source directory and its subdirectories
  AppendFlag(Result, TUtils.GetCompilerBackend.UnitPathFlag(TestBaseDir));

  // Scan and add test subdirectories (unit paths)
  UnitPaths := TUtils.ScanForUnitPaths(TestBaseDir);
  try
    for UnitPath in UnitPaths do
      AppendFlag(Result, TUtils.GetCompilerBackend.UnitPathFlag(UnitPath));
  finally
    UnitPaths.Free;
  end;

  // Scan and add include paths for test directory
  IncludePaths := TUtils.ScanForIncludePaths(TestBaseDir);
  try
    for IncludePath in IncludePaths do
      AppendFlag(Result, TUtils.GetCompilerBackend.IncludePathFlag(IncludePath));
  finally
    IncludePaths.Free;
  end;

  // Collect active defines (global + profile)
  ActiveDefines := TStringList.Create;
  try
    ActiveDefines.Duplicates := dupIgnore;
    ActiveDefines.Sorted := True;

    // Add global defines
    ActiveDefines.AddStrings(Config.BuildConfig.Defines);

    // Add profile defines
    for ProfileId in ProfileIds do
    begin
      Profile := Config.Profiles.FindById(ProfileId);
      if Assigned(Profile) then
        ActiveDefines.AddStrings(Profile.Defines);
    end;

    // Add defines to compiler command
    for Define in ActiveDefines do
      AppendFlag(Result, TUtils.GetCompilerBackend.DefineFlag(Define));

  finally
    ActiveDefines.Free;
  end;

  // Add global compiler options
  for Option in Config.BuildConfig.CompilerOptions do
    AppendFlag(Result, TUtils.GetCompilerBackend.ExtraOptionFlag(Option));

  // Add profile-specific compiler options
  for ProfileId in ProfileIds do
  begin
    Profile := Config.Profiles.FindById(ProfileId);
    if Assigned(Profile) then
    begin
      for Option in Profile.CompilerOptions do
        AppendFlag(Result, TUtils.GetCompilerBackend.ExtraOptionFlag(Option));
    end;
  end;
end;

function TTestCompileCommand.Execute: Integer;
var
  TestSourcePath, OutputDir: string;
  CompileCommand: string;
  StatusDir, LogFile: string;
  SourceFiles, IncludeFiles: TStringList;
begin
  Result := 0;

  TUtils.LogInfo('Compiling tests...');

  // Check if test directory exists — absence is a skip, not a failure
  // (matches Maven behaviour: no test sources = SUCCESS)
  if not DirectoryExists(Config.TestConfig.SourceDirectory) then
  begin
    TUtils.LogInfo('No test directory found (' + Config.TestConfig.SourceDirectory + '/), skipping');
    Result := 0;
    Exit;
  end;

  // Create output directories
  OutputDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory);

  // Main units directory (should already exist from compile goal)
  if not ForceDirectories(OutputDir + DirectorySeparator + 'units') then
  begin
    TUtils.LogError('Failed to create units directory');
    Result := 1;
    Exit;
  end;

  // Test units directory (separate from main units)
  if not ForceDirectories(OutputDir + DirectorySeparator + 'test-units') then
  begin
    TUtils.LogError('Failed to create test-units directory');
    Result := 1;
    Exit;
  end;

  // Check if test source file exists
  TestSourcePath := TUtils.NormalizePath(Config.TestConfig.SourceDirectory + '/' + Config.TestConfig.TestSource);
  if not FileExists(TestSourcePath) then
  begin
    TUtils.LogError('Test source file not found: ' + TestSourcePath);
    Result := 1;
    Exit;
  end;

  // Create status directory and collect test source information
  try
    StatusDir := TUtils.CreateStatusDirectory('test-compile');

    // Collect and write test source files list
    SourceFiles := TUtils.CollectSourceFiles(TUtils.NormalizePath(Config.TestConfig.SourceDirectory));
    try
      TUtils.LogInfo('Found ' + IntToStr(SourceFiles.Count) + ' test source file(s)');
      TUtils.WriteListFile(StatusDir + DirectorySeparator + 'inputUnits.lst', SourceFiles);
    finally
      SourceFiles.Free;
    end;

    // Collect and write test include files list
    IncludeFiles := TUtils.CollectIncludeFiles(TUtils.NormalizePath(Config.TestConfig.SourceDirectory));
    try
      if IncludeFiles.Count > 0 then
        TUtils.LogInfo('Found ' + IntToStr(IncludeFiles.Count) + ' test include file(s)');
      TUtils.WriteListFile(StatusDir + DirectorySeparator + 'inputIncludeFiles.lst', IncludeFiles);
    finally
      IncludeFiles.Free;
    end;

    LogFile := StatusDir + DirectorySeparator + 'compiler.log';
  except
    on E: Exception do
    begin
      TUtils.LogWarning('Failed to create status directory: ' + E.Message);
      // Continue without status tracking
      StatusDir := '';
    end;
  end;

  // Build compiler command for tests
  CompileCommand := BuildTestCompilerCommand(TestSourcePath);

  // Execute FPC (verbose mode shows full output, quiet mode logs to file)
  if FVerbose then
  begin
    // Verbose mode: Show full FPC output to console
    TUtils.LogInfo('Build command: ' + CompileCommand);
    WriteLn;
    Result := TUtils.ExecuteProcess(CompileCommand, True);
    WriteLn;
    if Result = 0 then
      TUtils.LogInfo('Test compilation successful')
    else
      TUtils.LogError('Test compilation failed with exit code: ' + IntToStr(Result));
  end
  else
  begin
    // Quiet mode: Clean console output, full output logged to file
    if StatusDir <> '' then
    begin
      Result := TUtils.ExecuteProcessWithLog(CompileCommand, LogFile, True);
      if Result = 0 then
        TUtils.LogInfo('Test compilation successful (see ' + LogFile + ' for details)')
      else
        TUtils.LogError('Test compilation failed with exit code: ' + IntToStr(Result) + ' (see ' + LogFile + ' for details)');
    end
    else
    begin
      // Fallback if status directory creation failed
      TUtils.LogInfo('Build command: ' + CompileCommand);
      WriteLn;
      Result := TUtils.ExecuteProcess(CompileCommand, True);
      WriteLn;
      if Result = 0 then
        TUtils.LogInfo('Test compilation successful')
      else
        TUtils.LogError('Test compilation failed with exit code: ' + IntToStr(Result));
    end;
  end;
end;

{ TTestCommand }

function TTestCommand.GetName: string;
begin
  Result := 'test';
end;

function TTestCommand.GetDependencies: TBuildCommandList;
begin
  Result := TBuildCommandList.Create(False);
  try
    // test depends on: test-compile (which depends on compile)
    Result.Add(TTestCompileCommand.Create(Config, ProfileIds));
  except
    Result.Free;
    raise;
  end;
end;

function TTestCommand.BuildTestRunnerCommand(const ATestExecutable: string): string;
var
  Option: string;
begin
  // Start with executable path
  Result := ATestExecutable;

  // Add framework-specific options from configuration
  for Option in Config.TestConfig.FrameworkOptions do
    Result := Result + ' ' + Option;
end;

function TTestCommand.Execute: Integer;
var
  OutputDir, TestExecutable, TestExecutableName: string;
  RunCommand: string;
  OriginalDir: string;
begin
  Result := 0;

  TUtils.LogInfo('Running tests...');

  // Build path to test executable
  OutputDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory);
  TestExecutableName := '.' + DirectorySeparator + 'TestRunner' + TUtils.GetPlatformExecutableSuffix;
  TestExecutable := OutputDir + DirectorySeparator + 'TestRunner' + TUtils.GetPlatformExecutableSuffix;

  // Check if test executable exists.
  // Absence means test-compile was skipped (no test source directory) — skip here too.
  if not FileExists(TestExecutable) then
  begin
    if not DirectoryExists(Config.TestConfig.SourceDirectory) then
    begin
      TUtils.LogInfo('No test directory found (' + Config.TestConfig.SourceDirectory + '/), skipping');
      Result := 0;
    end
    else
    begin
      TUtils.LogError('Test executable not found: ' + TestExecutable);
      TUtils.LogError('Run "pasbuild test-compile" first');
      Result := 1;
    end;
    Exit;
  end;

  // Make executable on Unix systems
  {$IFDEF UNIX}
  TUtils.ExecuteProcess('chmod +x ' + TestExecutable, False);
  {$ENDIF}

  // Change to output directory so test fixtures are found via relative paths
  OriginalDir := GetCurrentDir;
  try
    ChDir(OutputDir);
  except
    TUtils.LogError('Failed to change to output directory: ' + OutputDir);
    Result := 1;
    Exit;
  end;

  try
    // Build test runner command with framework options (use relative path from output dir)
    RunCommand := BuildTestRunnerCommand(TestExecutableName);

    TUtils.LogInfo('Execute command: ' + RunCommand);
    WriteLn;

    // Execute the test runner
    Result := TUtils.ExecuteProcess(RunCommand, True);

    WriteLn;
    if Result = 0 then
      TUtils.LogInfo('All tests passed')
    else
      TUtils.LogError('Tests failed with exit code: ' + IntToStr(Result));
  finally
    // Restore original directory
    ChDir(OriginalDir);
  end;
end;

end.
