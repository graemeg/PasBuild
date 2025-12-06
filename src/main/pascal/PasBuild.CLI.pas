unit PasBuild.CLI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  PASBUILD_VERSION = '1.0.0';

type
  { Valid build goals }
  TBuildGoal = (bgUnknown, bgClean, bgCompile, bgPackage, bgInit, bgHelp, bgVersion);

  { Parsed command-line arguments }
  TCommandLineArgs = record
    Goal: TBuildGoal;
    ProfileId: string;
    ShowHelp: Boolean;
    ShowVersion: Boolean;
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

{ TArgumentParser }

class function TArgumentParser.GoalFromString(const AGoalStr: string): TBuildGoal;
var
  GoalLower: string;
begin
  GoalLower := LowerCase(AGoalStr);

  if GoalLower = 'clean' then
    Result := bgClean
  else if GoalLower = 'compile' then
    Result := bgCompile
  else if GoalLower = 'package' then
    Result := bgPackage
  else if GoalLower = 'init' then
    Result := bgInit
  else if (GoalLower = '--help') or (GoalLower = '-h') then
    Result := bgHelp
  else if (GoalLower = '--version') or (GoalLower = '-v') then
    Result := bgVersion
  else
    Result := bgUnknown;
end;

class function TArgumentParser.GoalToString(AGoal: TBuildGoal): string;
begin
  case AGoal of
    bgClean: Result := 'clean';
    bgCompile: Result := 'compile';
    bgPackage: Result := 'package';
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
  Result.ProfileId := '';
  Result.ShowHelp := False;
  Result.ShowVersion := False;
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
      Result.ProfileId := ParamStr(I);
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
  WriteLn('  clean              Delete all build artifacts');
  WriteLn('  compile            Build the executable');
  WriteLn('  package            Create release archive (runs: clean -> compile -> package)');
  WriteLn('  init               Create new project structure');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  -p <profile>       Activate build profile');
  WriteLn('  --profile <id>     Activate build profile (same as -p)');
  WriteLn('  --help, -h         Show this help message');
  WriteLn('  --version, -v      Show version information');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  pasbuild compile              # Build with default settings');
  WriteLn('  pasbuild compile -p debug     # Build with debug profile');
  WriteLn('  pasbuild compile -p release   # Build with release profile');
  WriteLn('  pasbuild package              # Create release archive');
  WriteLn('  pasbuild init                 # Create new project');
  WriteLn;
end;

class procedure TArgumentParser.ShowVersion;
begin
  WriteLn('PasBuild version ', PASBUILD_VERSION);
  WriteLn('Build automation tool for Free Pascal projects');
  WriteLn;

  // Try to detect FPC version
  // Note: This will be implemented in PasBuild.Utils later
  WriteLn('For FPC version detection, run: fpc -iV');
  WriteLn;
end;

end.
