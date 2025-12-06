{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

program PasBuild.Test.CLI;

{$mode objfpc}{$H+}

uses
  SysUtils,
  PasBuild.CLI;

procedure TestParseArguments;
var
  Args: TCommandLineArgs;
begin
  WriteLn('=== Testing CLI Argument Parser ===');
  WriteLn;

  Args := TArgumentParser.ParseArguments;

  WriteLn('[Parsed Arguments]');

  if Args.ShowHelp then
  begin
    WriteLn('  Action: Show Help');
    if Args.ErrorMessage <> '' then
      WriteLn('  Error: ', Args.ErrorMessage);
    WriteLn;
    TArgumentParser.ShowHelp;
    Exit;
  end;

  if Args.ShowVersion then
  begin
    WriteLn('  Action: Show Version');
    WriteLn;
    TArgumentParser.ShowVersion;
    Exit;
  end;

  WriteLn('  Goal: ', Ord(Args.Goal));
  WriteLn('  Profile: ', Args.ProfileId);

  if Args.ErrorMessage <> '' then
  begin
    WriteLn;
    WriteLn('[ERROR] ', Args.ErrorMessage);
    ExitCode := 1;
  end
  else
  begin
    WriteLn;
    WriteLn('[SUCCESS] Arguments parsed correctly');
  end;
end;

begin
  TestParseArguments;
end.
