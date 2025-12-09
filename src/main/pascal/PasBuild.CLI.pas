{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.CLI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  PASBUILD_VERSION = '1.0.0';

type
  { Valid build goals }
  TBuildGoal = (bgUnknown, bgClean, bgProcessResources, bgCompile, bgProcessTestResources, bgTestCompile, bgTest, bgPackage, bgSourcePackage, bgInit, bgHelp, bgVersion);

  { Parsed command-line arguments }
  TCommandLineArgs = record
    Goal: TBuildGoal;
    ProfileIds: TStringList;  // Changed from ProfileId to support multiple profiles
    ProjectFile: string;  // Custom project file path (default: project.xml)
    ShowHelp: Boolean;
    ShowVersion: Boolean;
    Verbose: Boolean;
    ErrorMessage: string;
  end;

  { Command-line argument parser }
  TArgumentParser = class
  private
    class function GoalFromString(const AGoalStr: string): TBuildGoal;
    class function GoalToString(AGoal: TBuildGoal): string;
  public
    class function ParseArguments: TCommandLineArgs;
    class procedure ShowHelp;
    class procedure ShowVersion;
  end;

implementation

uses
  PasBuild.Utils;

{ TArgumentParser }

class function TArgumentParser.GoalFromString(const AGoalStr: string): TBuildGoal;
var
  GoalLower: string;
begin
  GoalLower := LowerCase(AGoalStr);

  if GoalLower = 'clean' then
    Result := bgClean
  else if GoalLower = 'process-resources' then
    Result := bgProcessResources
  else if GoalLower = 'compile' then
    Result := bgCompile
  else if GoalLower = 'process-test-resources' then
    Result := bgProcessTestResources
  else if GoalLower = 'test-compile' then
    Result := bgTestCompile
  else if GoalLower = 'test' then
    Result := bgTest
  else if GoalLower = 'package' then
    Result := bgPackage
  else if GoalLower = 'source-package' then
    Result := bgSourcePackage
  else if GoalLower = 'init' then
    Result := bgInit
  else if (GoalLower = '--help') or (GoalLower = '-h') then
    Result := bgHelp
  else if GoalLower = '--version' then
    Result := bgVersion
  else
    Result := bgUnknown;
end;

class function TArgumentParser.GoalToString(AGoal: TBuildGoal): string;
begin
  case AGoal of
    bgClean: Result := 'clean';
    bgProcessResources: Result := 'process-resources';
    bgCompile: Result := 'compile';
    bgProcessTestResources: Result := 'process-test-resources';
    bgTestCompile: Result := 'test-compile';
    bgTest: Result := 'test';
    bgPackage: Result := 'package';
    bgSourcePackage: Result := 'source-package';
    bgInit: Result := 'init';
    bgHelp: Result := '--help';
    bgVersion: Result := '--version';
    else Result := 'unknown';
  end;
end;

class function TArgumentParser.ParseArguments: TCommandLineArgs;
var
  I: Integer;
  Arg: string;
begin
  // Initialize result
  Result.Goal := bgUnknown;
  Result.ProfileIds := TStringList.Create;
  Result.ProfileIds.Delimiter := ',';
  Result.ProfileIds.StrictDelimiter := True;
  Result.ProjectFile := 'project.xml';  // Default
  Result.ShowHelp := False;
  Result.ShowVersion := False;
  Result.Verbose := False;
  Result.ErrorMessage := '';

  // No arguments provided
  if ParamCount = 0 then
  begin
    Result.ErrorMessage := 'No goal specified';
    Result.ShowHelp := True;
    Exit;
  end;

  // First parameter is always the goal
  Result.Goal := GoalFromString(ParamStr(1));

  // Handle help and version flags
  if Result.Goal = bgHelp then
  begin
    Result.ShowHelp := True;
    Exit;
  end;

  if Result.Goal = bgVersion then
  begin
    Result.ShowVersion := True;
    Exit;
  end;

  // Validate goal
  if Result.Goal = bgUnknown then
  begin
    Result.ErrorMessage := 'Unknown goal: ' + ParamStr(1);
    Result.ShowHelp := True;
    Exit;
  end;

  // Parse remaining arguments
  I := 2;
  while I <= ParamCount do
  begin
    Arg := ParamStr(I);

    // Profile flag
    if (Arg = '-p') or (Arg = '--profile') then
    begin
      Inc(I);
      if I > ParamCount then
      begin
        Result.ErrorMessage := 'Option ' + Arg + ' requires a profile ID';
        Exit;
      end;
      // Parse comma-separated profile IDs (e.g., -p debug,logging)
      Result.ProfileIds.DelimitedText := ParamStr(I);
    end
    // Verbose flag
    else if (Arg = '-v') or (Arg = '--verbose') then
    begin
      Result.Verbose := True;
    end
    // Project file flag
    else if (Arg = '-f') or (Arg = '--file') then
    begin
      Inc(I);
      if I > ParamCount then
      begin
        Result.ErrorMessage := 'Option ' + Arg + ' requires a file path';
        Exit;
      end;
      Result.ProjectFile := ParamStr(I);
    end
    else
    begin
      Result.ErrorMessage := 'Unknown option: ' + Arg;
      Exit;
    end;

    Inc(I);
  end;
end;

class procedure TArgumentParser.ShowHelp;
begin
  WriteLn('Usage: pasbuild <goal> [options]');
  WriteLn;
  WriteLn('Goals:');
  WriteLn('  clean                   Delete all build artifacts');
  WriteLn('  process-resources       Copy resources to target directory');
  WriteLn('  compile                 Build the executable (runs: process-resources -> compile)');
  WriteLn('  process-test-resources  Copy test resources to target directory');
  WriteLn('  test-compile            Compile tests (runs: compile -> process-test-resources -> test-compile)');
  WriteLn('  test                    Run tests (runs: compile -> process-test-resources -> test-compile -> test)');
  WriteLn('  package                 Create release archive (runs: clean -> compile -> package)');
  WriteLn('  source-package          Create source archive with src/, docs, and configured files');
  WriteLn('  init                    Create new project structure');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  -p <profile[,profile...]>    Activate build profile(s)');
  WriteLn('  --profile <id>               Activate build profile (same as -p)');
  WriteLn('  -f <file>, --file <file>     Use alternate project file (default: project.xml)');
  WriteLn('  -v, --verbose                Show full compiler output');
  WriteLn('  -h, --help                   Show this help message');
  WriteLn('  --version                    Show version information');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  pasbuild compile                      # Build with default settings');
  WriteLn('  pasbuild compile -p debug             # Build with debug profile');
  WriteLn('  pasbuild compile -p release           # Build with release profile');
  WriteLn('  pasbuild compile -p base,debug        # Build with base + debug profiles');
  WriteLn('  pasbuild compile -v                   # Build with verbose FPC output');
  WriteLn('  pasbuild compile -f custom.xml        # Use custom project file');
  WriteLn('  pasbuild test                         # Run tests');
  WriteLn('  pasbuild package                      # Create release archive');
  WriteLn('  pasbuild init                         # Create new project');
  WriteLn;
end;

class procedure TArgumentParser.ShowVersion;
begin
  WriteLn('PasBuild version ', PASBUILD_VERSION);
  WriteLn('Build automation tool for Free Pascal projects');
  WriteLn;

  // Try to detect FPC version
  WriteLn('FPC version detected (fpc -iV): ', TUtils.DetectFPCVersion());
  WriteLn;
end;

end.
