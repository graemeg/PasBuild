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
  PASBUILD_VERSION = {$I version.inc};
  PASBUILD_BUILD_DATE = {$I %DATE%};

type
  { Valid build goals }
  TBuildGoal = (bgUnknown, bgClean, bgProcessResources, bgCompile, bgProcessTestResources, bgTestCompile, bgTest, bgPackage, bgSourcePackage, bgInstall, bgDependencyTree, bgResolve, bgInit, bgHelp, bgVersion, bgLicense);

  { Parsed command-line arguments }
  TCommandLineArgs = record
    Goal: TBuildGoal;
    ProfileIds: TStringList;  // Changed from ProfileId to support multiple profiles
    ProjectFile: string;  // Custom project file path (default: project.xml)
    SelectedModule: string;  // Module name for multi-module builds (empty = all modules)
    FPCExecutable: string;  // Custom FPC compiler path (empty = default 'fpc')
    ShowHelp: Boolean;
    ShowVersion: Boolean;
    ShowLicense: Boolean;
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
    class procedure ShowLicense;
  end;

implementation

uses
  PasBuild.Utils, TypInfo;

{ TArgumentParser }

class function TArgumentParser.GoalFromString(const AGoalStr: string): TBuildGoal;
var
  GoalLower: string;
  GoalEnumName: string;
begin
  GoalLower := LowerCase(AGoalStr);
  GoalEnumName := 'bg'+StringReplace(GoalLower, '-', '', [rfReplaceAll]);
  if GoalLower = '-h' then
    GoalLower := '--help';
  try
    Result := TBuildGoal(GetEnumValue(TypeInfo(TBuildGoal), GoalEnumName));
    if Ord(Result) = -1 then Result := bgUnknown;
  except
    Result := bgUnknown;
  end;

  if (Result in [bgHelp, bgVersion, bgLicense])
  and (Copy(GoalLower, 1, 2) <> '--') then
    Result := bgUnknown;
end;

class function TArgumentParser.GoalToString(AGoal: TBuildGoal): string;
var
  i: Integer;
begin
  try
    // get Enum name e.g. bgProcessResources and copy just 'ProcessResources';
    Result := Copy(GetEnumName(TypeInfo(TBuildGoal), Ord(AGoal)), 3, Maxint);

    // set 'P' to 'p' for 'processResources' example
    Result[1] := LowerCase(Result[1]); // set first letter lowercase

    i := 2; // start on second letter
    while i <= Length(Result) do
    begin
      // look for capitol letters which indicate a hyphen
      if Result[i] <> LowerCase(Result[i]) then
      begin
        // insert hyphen
        Result[i] := LowerCase(Result[i]);
        Insert('-', Result, i);
        Inc(i);
      end;
      Inc(i);
    end;

    // now some special cases
    case Result of
      'help', 'version', 'license': Result := '--' + Result;
    end;
  except
    Result := 'unknown';
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
  Result.SelectedModule := '';  // Default: all modules
  Result.FPCExecutable := '';  // Default: use 'fpc' from PATH
  Result.ShowHelp := False;
  Result.ShowVersion := False;
  Result.ShowLicense := False;
  Result.Verbose := False;
  Result.ErrorMessage := '';

  // Pre-pass: extract --fpc flag from anywhere in args
  I := 1;
  while I <= ParamCount do
  begin
    if ParamStr(I) = '--fpc' then
    begin
      if I < ParamCount then
        Result.FPCExecutable := ParamStr(I + 1);
      Break;
    end;
    Inc(I);
  end;

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

  if Result.Goal = bgLicense then
  begin
    Result.ShowLicense := True;
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
    // Module selection flag (for multi-module builds)
    else if (Arg = '-m') or (Arg = '--module') then
    begin
      Inc(I);
      if I > ParamCount then
      begin
        Result.ErrorMessage := 'Option ' + Arg + ' requires a module name';
        Exit;
      end;
      Result.SelectedModule := ParamStr(I);
    end
    // FPC executable flag
    else if (Arg = '--fpc') then
    begin
      Inc(I);
      if I > ParamCount then
      begin
        Result.ErrorMessage := 'Option ' + Arg + ' requires a path to the FPC executable';
        Exit;
      end;
      Result.FPCExecutable := ParamStr(I);
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
  WriteLn('  install                 Install compiled units to local repository (~/.pasbuild/repository/)');
  WriteLn('  dependency-tree         Show project dependency tree (no compilation)');
  WriteLn('  resolve                 Output resolved build configuration as JSON (no compilation)');
  WriteLn('  init                    Create new project structure');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  -p <profile[,profile...]>    Activate build profile(s)');
  WriteLn('  --profile <id>               Activate build profile (same as -p)');
  WriteLn('  -m <module>, --module        Build specific module in multi-module project');
  WriteLn('  -f <file>, --file <file>     Use alternate project file (default: project.xml)');
  WriteLn('  --fpc <path>                 Use custom FPC executable (default: fpc)');
  WriteLn('  -v, --verbose                Show full compiler output');
  WriteLn('  -h, --help                   Show this help message');
  WriteLn('  --version                    Show version information');
  WriteLn('  --license                    Show license information');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  pasbuild compile                      # Build with default settings');
  WriteLn('  pasbuild compile -p debug             # Build with debug profile');
  WriteLn('  pasbuild compile -p release           # Build with release profile');
  WriteLn('  pasbuild compile -p base,debug        # Build with base + debug profiles');
  WriteLn('  pasbuild compile -v                   # Build with verbose FPC output');
  WriteLn('  pasbuild compile -f custom.xml        # Use alternate project file');
  WriteLn('  pasbuild compile -f ../../../project.xml  # Build from a subdirectory');
  WriteLn('  pasbuild compile -m mymodule          # Build specific module (multi-module)');
  WriteLn('  pasbuild dependency-tree              # Show full project dependency tree');
  WriteLn('  pasbuild dependency-tree -m mymodule  # Show dependencies for one module');
  WriteLn('  pasbuild resolve -p unix,debug        # Output resolved build config as JSON');
  WriteLn('  pasbuild resolve -m mymodule          # Resolve specific module only');
  WriteLn('  pasbuild compile --fpc /opt/fpc-3.3.1/bin/fpc  # Use custom FPC');
  WriteLn('  pasbuild test                         # Run tests');
  WriteLn('  pasbuild package                      # Create release archive');
  WriteLn('  pasbuild init                         # Create new project');
  WriteLn;
end;

class procedure TArgumentParser.ShowVersion;
begin
  WriteLn('PasBuild version ', PASBUILD_VERSION);
  WriteLn('Build automation tool for Free Pascal projects');
  WriteLn('Built: ', PASBUILD_BUILD_DATE);
  WriteLn('Author: Graeme Geldenhuys');
  WriteLn;

  // Show which FPC is being used and its version
  WriteLn('FPC executable: ', TUtils.GetFPCExecutable);
  WriteLn('FPC version detected: ', TUtils.DetectFPCVersion());
  WriteLn;
end;

class procedure TArgumentParser.ShowLicense;
begin
  WriteLn('BSD 3-Clause License');
  WriteLn;
  WriteLn('Copyright (c) 2025 - Graeme Geldenhuys <graemeg@gmail.com>');
  WriteLn;
  WriteLn('Redistribution and use in source and binary forms, with or without');
  WriteLn('modification, are permitted provided that the following conditions are met:');
  WriteLn;
  WriteLn('1. Redistributions of source code must retain the above copyright notice, this');
  WriteLn('   list of conditions and the following disclaimer.');
  WriteLn;
  WriteLn('2. Redistributions in binary form must reproduce the above copyright notice,');
  WriteLn('   this list of conditions and the following disclaimer in the documentation');
  WriteLn('   and/or other materials provided with the distribution.');
  WriteLn;
  WriteLn('3. Neither the name of the copyright holder nor the names of its');
  WriteLn('   contributors may be used to endorse or promote products derived from');
  WriteLn('   this software without specific prior written permission.');
  WriteLn;
  WriteLn('THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"');
  WriteLn('AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE');
  WriteLn('IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE');
  WriteLn('DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE');
  WriteLn('FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL');
  WriteLn('DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR');
  WriteLn('SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER');
  WriteLn('CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,');
  WriteLn('OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE');
  WriteLn('OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.');
  WriteLn;
end;

end.
