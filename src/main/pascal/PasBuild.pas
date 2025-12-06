program PasBuild;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils;

const
  VERSION = '1.0.0';

procedure ShowVersion;
begin
  WriteLn('PasBuild version ', VERSION);
  WriteLn('Build automation tool for Free Pascal projects');
  WriteLn;
end;

procedure ShowHelp;
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
  WriteLn('  --help             Show this help message');
  WriteLn('  --version          Show version information');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  pasbuild compile              # Build with default settings');
  WriteLn('  pasbuild compile -p debug     # Build with debug profile');
  WriteLn('  pasbuild compile -p release   # Build with release profile');
  WriteLn('  pasbuild package              # Create release archive');
  WriteLn('  pasbuild init                 # Create new project');
  WriteLn;
end;

var
  Goal: string;

begin
  WriteLn('[INFO] PasBuild ', VERSION);
  WriteLn;

  // Parse command line arguments
  if ParamCount = 0 then
  begin
    WriteLn('[ERROR] No goal specified');
    WriteLn;
    ShowHelp;
    ExitCode := 1;
    Exit;
  end;

  Goal := ParamStr(1);

  // Handle special flags
  if (Goal = '--help') or (Goal = '-h') then
  begin
    ShowHelp;
    ExitCode := 0;
    Exit;
  end;

  if (Goal = '--version') or (Goal = '-v') then
  begin
    ShowVersion;
    ExitCode := 0;
    Exit;
  end;

  // TODO: Implement goal dispatch
  // For now, just acknowledge the goal
  WriteLn('[INFO] Goal: ', Goal);
  WriteLn('[ERROR] Goal execution not yet implemented');
  WriteLn('[INFO] This is the Phase 0 skeleton - implementation starts in Phase 1');
  ExitCode := 1;
end.
