{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

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
  PASBUILD_BUILD_DATE = {$I build-date.inc};

type
  { Valid build goals }
  TBuildGoal = (bgUnknown, bgClean, bgProcessResources, bgCompile, bgProcessTestResources, bgTestCompile, bgTest, bgPackage, bgSourcePackage, bgInstall, bgDependencyTree, bgResolve, bgInit);

  { Parsed command-line arguments }
  TCommandLineArgs = record
    Goal: TBuildGoal;
    GoalString: string;  // Raw goal string (for plugin resolution when Goal = bgUnknown)
    ProfileIds: TStringList;  // Changed from ProfileId to support multiple profiles
    ProjectFile: string;  // Custom project file path (default: project.xml)
    SelectedModule: string;  // Module name for multi-module builds (empty = all modules)
    ForceAllModules: Boolean;  // True when --all is passed; overrides activeByDefault=false
    CompilerExecutable: string;  // Custom Pascal compiler path (empty = default 'fpc')
    ShowHelp: Boolean;
    ShowVersion: Boolean;
    ShowLicense: Boolean;
    Verbose: Boolean;
    ErrorMessage: string;
  end;

  { Command-line argument parser }
  TArgumentParser = class
  private
    class function GoalToString(AGoal: TBuildGoal): string;
  public
    class function GoalFromString(const AGoalStr: string): TBuildGoal;
    class function ParseArguments: TCommandLineArgs;
    class procedure ShowHelp;
    class procedure ShowVersion;
    class procedure ShowLicense;
  end;

implementation

uses
  PasBuild.Utils,
  PasBuild.Plugin;

const
  GoalNames: array[TBuildGoal] of string = (
    'unknown',
    'clean',
    'process-resources',
    'compile',
    'process-test-resources',
    'test-compile',
    'test',
    'package',
    'source-package',
    'install',
    'dependency-tree',
    'resolve',
    'init'
  );

{ TArgumentParser }

class function TArgumentParser.GoalFromString(const AGoalStr: string): TBuildGoal;
var
  GoalLower: string;
  G: TBuildGoal;
begin
  GoalLower := LowerCase(AGoalStr);
  for G := Low(TBuildGoal) to High(TBuildGoal) do
    if GoalNames[G] = GoalLower then
      Exit(G);
  Result := bgUnknown;
end;

class function TArgumentParser.GoalToString(AGoal: TBuildGoal): string;
begin
  Result := GoalNames[AGoal];
end;

class function TArgumentParser.ParseArguments: TCommandLineArgs;
var
  I: Integer;
  Arg: string;
begin
  // Initialize result
  Result.Goal := bgUnknown;
  Result.GoalString := '';
  Result.ProfileIds := TStringList.Create;
  Result.ProfileIds.Delimiter := ',';
  Result.ProfileIds.StrictDelimiter := True;
  Result.ProjectFile := 'project.xml';  // Default
  Result.SelectedModule := '';  // Default: all modules
  Result.ForceAllModules := False;  // Default: respect activeByDefault
  Result.CompilerExecutable := '';  // Default: use 'fpc' from PATH
  Result.ShowHelp := False;
  Result.ShowVersion := False;
  Result.ShowLicense := False;
  Result.Verbose := False;
  Result.ErrorMessage := '';

  // Pre-pass: extract --compiler (or deprecated --fpc) from anywhere in args
  I := 1;
  while I <= ParamCount do
  begin
    if ParamStr(I) = '--compiler' then
    begin
      if I < ParamCount then
        Result.CompilerExecutable := ParamStr(I + 1);
      Break;
    end
    else if (ParamStr(I) = '--fpc') and (Result.CompilerExecutable = '') then
    begin
      if I < ParamCount then
        Result.CompilerExecutable := ParamStr(I + 1);
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

  // Handle flags that are not build goals
  Arg := LowerCase(ParamStr(1));
  if (Arg = '--help') or (Arg = '-h') then
  begin
    Result.ShowHelp := True;
    Exit;
  end;
  if Arg = '--version' then
  begin
    Result.ShowVersion := True;
    Exit;
  end;
  if Arg = '--license' then
  begin
    Result.ShowLicense := True;
    Exit;
  end;

  // First parameter is the goal
  Result.Goal := GoalFromString(ParamStr(1));

  // If not a built-in goal, store the raw string for plugin resolution
  if Result.Goal = bgUnknown then
    Result.GoalString := ParamStr(1);

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
    // Force all modules flag (overrides activeByDefault=false)
    else if (Arg = '--all') then
    begin
      Result.ForceAllModules := True;
    end
    // Compiler executable flag
    else if (Arg = '--compiler') then
    begin
      Inc(I);
      if I > ParamCount then
      begin
        Result.ErrorMessage := 'Option ' + Arg + ' requires a path to a Pascal compiler executable';
        Exit;
      end;
      Result.CompilerExecutable := ParamStr(I);
    end
    // Deprecated alias
    else if (Arg = '--fpc') then
    begin
      TUtils.LogWarning('--fpc is deprecated; use --compiler instead');
      Inc(I);
      if I > ParamCount then
      begin
        Result.ErrorMessage := 'Option ' + Arg + ' requires a path to the compiler executable';
        Exit;
      end;
      Result.CompilerExecutable := ParamStr(I);
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
var
  Plugins: TStringList;
  I: Integer;
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

  // List discovered plugins
  Plugins := TPluginDiscovery.DiscoverAllPlugins;
  try
    if Plugins.Count > 0 then
    begin
      WriteLn;
      WriteLn('Plugins (external):');
      for I := 0 to Plugins.Count - 1 do
        WriteLn('  ', Plugins[I]);
    end;
  finally
    Plugins.Free;
  end;

  WriteLn;
  WriteLn('Options:');
  WriteLn('  -p <profile[,profile...]>    Activate build profile(s)');
  WriteLn('  --profile <id>               Activate build profile (same as -p)');
  WriteLn('  -m <module>, --module        Build specific module in multi-module project');
  WriteLn('  --all                        Build all modules, including those with activeByDefault=false');
  WriteLn('  -f <file>, --file <file>     Use alternate project file (default: project.xml)');
  WriteLn('  --compiler <path>            Use custom Pascal compiler (default: fpc)');
  WriteLn('  --fpc <path>                 Deprecated alias for --compiler');
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
  WriteLn('  pasbuild compile -f ../../project.xml  # Build from a subdirectory');
  WriteLn('  pasbuild compile -m mymodule          # Build specific module (multi-module)');
  WriteLn('  pasbuild compile --all                # Build all modules (including inactive)');
  WriteLn('  pasbuild dependency-tree              # Show full project dependency tree');
  WriteLn('  pasbuild dependency-tree -m mymodule  # Show dependencies for one module');
  WriteLn('  pasbuild resolve -p unix,debug        # Output resolved build config as JSON');
  WriteLn('  pasbuild resolve -m mymodule          # Resolve specific module only');
  WriteLn('  pasbuild compile --compiler /opt/fpc-3.3.1/bin/fpc  # Use custom FPC');
  WriteLn('  pasbuild compile --compiler releases/v0.3.0/blaise  # Use Blaise compiler');
  WriteLn('  pasbuild test                         # Run tests');
  WriteLn('  pasbuild package                      # Create release archive');
  WriteLn('  pasbuild init                         # Create new project');
  WriteLn;
end;

class procedure TArgumentParser.ShowVersion;
begin
  WriteLn('PasBuild version ', PASBUILD_VERSION);
  WriteLn('Build automation tool for Pascal projects');
  WriteLn('Built: ', PASBUILD_BUILD_DATE);
  WriteLn('Author: Graeme Geldenhuys');
  WriteLn;

  // Show which compiler is being used and its version
  WriteLn('Compiler backend : ', TUtils.GetCompilerBackend.Name);
  WriteLn('Compiler path    : ', TUtils.GetCompilerBackend.Executable);
  WriteLn('Compiler version : ', TUtils.DetectFPCVersion());
  WriteLn;
end;

class procedure TArgumentParser.ShowLicense;
begin
  WriteLn('BSD 3-Clause License');
  WriteLn;
  WriteLn('Copyright (c) 2025-2026 by Graeme Geldenhuys <graemeg@gmail.com>');
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
