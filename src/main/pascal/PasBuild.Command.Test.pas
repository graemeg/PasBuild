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
  PasBuild.Utils;

type
  { Test-compile command - compiles test code }
  TTestCompileCommand = class(TBuildCommand)
  protected
    function GetName: string; override;
    function DetectTestFramework: TTestFramework;
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

{ TTestCompileCommand }

function TTestCompileCommand.GetName: string;
begin
  Result := 'test-compile';
end;

function TTestCompileCommand.GetDependencies: TBuildCommandList;
begin
  Result := TBuildCommandList.Create(False);
  try
    // test-compile depends on: compile
    Result.Add(TCompileCommand.Create(Config, ProfileIds));
  except
    Result.Free;
    raise;
  end;
end;

function TTestCompileCommand.DetectTestFramework: TTestFramework;
var
  TestSourcePath: string;
  SourceLines: TStringList;
  Line: string;
  LineUpper: string;
begin
  Result := tfFPCUnit;  // Default fallback

  TestSourcePath := TUtils.NormalizePath('src/test/pascal/' + Config.TestConfig.TestSource);

  if not FileExists(TestSourcePath) then
  begin
    TUtils.LogWarning('Test source not found: ' + TestSourcePath + ', defaulting to FPCUnit');
    Exit;
  end;

  // Read test source file and scan for framework indicators
  SourceLines := TStringList.Create;
  try
    try
      SourceLines.LoadFromFile(TestSourcePath);

      for Line in SourceLines do
      begin
        LineUpper := UpperCase(Trim(Line));

        // Check for FPCUnit indicator: 'fpcunit' in uses clause
        if (Pos('USES', LineUpper) > 0) and (Pos('FPCUNIT', LineUpper) > 0) then
        begin
          TUtils.LogInfo('Auto-detected test framework: FPCUnit');
          Result := tfFPCUnit;
          Exit;
        end;

        // Check for FPTest indicator: 'TestFramework' in uses clause
        if (Pos('USES', LineUpper) > 0) and (Pos('TESTFRAMEWORK', LineUpper) > 0) then
        begin
          TUtils.LogInfo('Auto-detected test framework: FPTest');
          Result := tfFPTest;
          Exit;
        end;
      end;

      // No framework detected in uses clause, default to FPCUnit
      TUtils.LogWarning('Could not auto-detect test framework, defaulting to FPCUnit');

    except
      on E: Exception do
      begin
        TUtils.LogWarning('Error reading test source: ' + E.Message + ', defaulting to FPCUnit');
      end;
    end;

  finally
    SourceLines.Free;
  end;
end;

function TTestCompileCommand.BuildTestCompilerCommand(const ATestSourcePath: string): string;
var
  OutputDir: string;
  Define, Option: string;
  Profile: TProfile;
  ProfileId: string;
  ActiveDefines: TStringList;
begin
  // Base command with default flags
  Result := 'fpc -Mobjfpc -O1';

  // Test source file path
  Result := Result + ' ' + ATestSourcePath;

  // Output directory
  OutputDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory);
  Result := Result + ' -FE' + OutputDir;

  // Unit output directory
  Result := Result + ' -FU' + OutputDir + DirectorySeparator + 'units';

  // Test executable name
  Result := Result + ' -oTestRunner' + TUtils.GetPlatformExecutableSuffix;

  // Link to already-compiled main units (compiled by 'compile' goal)
  Result := Result + ' -Fu' + OutputDir + DirectorySeparator + 'units';

  // Add test source directory
  Result := Result + ' -Fusrc/test/pascal';

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
      Result := Result + ' -d' + Define;

  finally
    ActiveDefines.Free;
  end;

  // Add global compiler options
  for Option in Config.BuildConfig.CompilerOptions do
    Result := Result + ' ' + Option;

  // Add profile-specific compiler options
  for ProfileId in ProfileIds do
  begin
    Profile := Config.Profiles.FindById(ProfileId);
    if Assigned(Profile) then
    begin
      for Option in Profile.CompilerOptions do
        Result := Result + ' ' + Option;
    end;
  end;
end;

function TTestCompileCommand.Execute: Integer;
var
  TestSourcePath, OutputDir: string;
  CompileCommand: string;
  DetectedFramework: TTestFramework;
begin
  Result := 0;

  TUtils.LogInfo('Compiling tests...');

  // Check if test directory exists
  if not DirectoryExists('src/test/pascal') then
  begin
    TUtils.LogError('Test directory not found: src/test/pascal/');
    TUtils.LogError('Run "pasbuild init" to create the test structure');
    Result := 1;
    Exit;
  end;

  // Determine which framework to use
  if Config.TestConfig.Framework = tfAuto then
    DetectedFramework := DetectTestFramework
  else
    DetectedFramework := Config.TestConfig.Framework;

  // Log which framework is being used
  case DetectedFramework of
    tfFPCUnit: TUtils.LogInfo('Using test framework: FPCUnit');
    tfFPTest: TUtils.LogInfo('Using test framework: FPTest');
  end;

  // Create output directories (should already exist from compile goal)
  OutputDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory);
  if not ForceDirectories(OutputDir + DirectorySeparator + 'units') then
  begin
    TUtils.LogError('Failed to create units directory');
    Result := 1;
    Exit;
  end;

  // Check if test source file exists
  TestSourcePath := TUtils.NormalizePath('src/test/pascal/' + Config.TestConfig.TestSource);
  if not FileExists(TestSourcePath) then
  begin
    TUtils.LogError('Test source file not found: ' + TestSourcePath);
    Result := 1;
    Exit;
  end;

  // Build compiler command for tests
  CompileCommand := BuildTestCompilerCommand(TestSourcePath);

  TUtils.LogInfo('Build command: ' + CompileCommand);
  WriteLn;

  // Compile the test runner
  Result := TUtils.ExecuteProcess(CompileCommand, True);

  WriteLn;
  if Result = 0 then
    TUtils.LogInfo('Test compilation successful')
  else
    TUtils.LogError('Test compilation failed with exit code: ' + IntToStr(Result));
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
  OutputDir, TestExecutable: string;
  RunCommand: string;
begin
  Result := 0;

  TUtils.LogInfo('Running tests...');

  // Build path to test executable
  OutputDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory);
  TestExecutable := OutputDir + DirectorySeparator + 'TestRunner' + TUtils.GetPlatformExecutableSuffix;

  // Check if test executable exists
  if not FileExists(TestExecutable) then
  begin
    TUtils.LogError('Test executable not found: ' + TestExecutable);
    TUtils.LogError('Run "pasbuild test-compile" first');
    Result := 1;
    Exit;
  end;

  // Make executable on Unix systems
  {$IFDEF UNIX}
  TUtils.ExecuteProcess('chmod +x ' + TestExecutable, False);
  {$ENDIF}

  // Build test runner command with framework options
  RunCommand := BuildTestRunnerCommand(TestExecutable);

  TUtils.LogInfo('Execute command: ' + RunCommand);
  WriteLn;

  // Execute the test runner
  Result := TUtils.ExecuteProcess(RunCommand, True);

  WriteLn;
  if Result = 0 then
    TUtils.LogInfo('All tests passed')
  else
    TUtils.LogError('Tests failed with exit code: ' + IntToStr(Result));
end;

end.
