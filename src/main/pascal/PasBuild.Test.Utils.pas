{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

program PasBuild.Test.Utils;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  PasBuild.Types,
  PasBuild.Utils;

var
  UnitPaths, ActiveDefines: TStringList;
  ConditionalPaths: TConditionalPathList;
  Path: string;
  FPCVersion: string;

begin
  WriteLn('=== Testing PasBuild.Utils ===');
  WriteLn;

  // Test path normalization
  WriteLn('[Path Normalization]');
  WriteLn('  Input:  src/main/pascal');
  WriteLn('  Output: ', TUtils.NormalizePath('src/main/pascal'));
  WriteLn('  Input:  src/test/resources');
  WriteLn('  Output: ', TUtils.NormalizePath('src/test/resources'));
  WriteLn;

  // Test directory layout verification
  WriteLn('[Directory Layout Verification]');
  if TUtils.VerifyDirectoryLayout('.') then
    WriteLn('  ✓ Standard layout found')
  else
    WriteLn('  ✗ Standard layout NOT found');
  WriteLn;

  // Test unit path scanning
  WriteLn('[Unit Path Scanning]');
  UnitPaths := TUtils.ScanForUnitPaths('src/main/pascal');
  try
    if UnitPaths.Count > 0 then
    begin
      WriteLn('  Found ', UnitPaths.Count, ' subdirectories:');
      for Path in UnitPaths do
        WriteLn('    ', Path);
    end
    else
      WriteLn('  No subdirectories found');
  finally
    UnitPaths.Free;
  end;
  WriteLn;

  // Test FPC detection
  WriteLn('[FPC Detection]');
  if TUtils.IsFPCAvailable then
  begin
    FPCVersion := TUtils.DetectFPCVersion;
    WriteLn('  ✓ FPC found: version ', FPCVersion);
  end
  else
    WriteLn('  ✗ FPC not found in PATH');
  WriteLn;

  // Test platform utilities
  WriteLn('[Platform Utilities]');
  WriteLn('  Executable suffix: "', TUtils.GetPlatformExecutableSuffix, '"');
  WriteLn('  Directory separator: "', DirectorySeparator, '"');
  WriteLn;

  // Test logging
  WriteLn('[Logging Tests]');
  TUtils.LogInfo('This is an info message');
  TUtils.LogWarning('This is a warning message');
  TUtils.LogError('This is an error message');
  WriteLn;

  // Test conditional path filtering - IsConditionMet
  WriteLn('[Conditional Path - IsConditionMet]');
  ActiveDefines := TStringList.Create;
  try
    ActiveDefines.Add('DEBUG');
    ActiveDefines.Add('MY_CUSTOM_DEFINE');

    // Test built-in platform defines
    {$IFDEF UNIX}
    WriteLn('  ✓ UNIX condition = ', TUtils.IsConditionMet('UNIX', ActiveDefines));
    {$ELSE}
    WriteLn('  ✗ UNIX condition = ', TUtils.IsConditionMet('UNIX', ActiveDefines));
    {$ENDIF}

    {$IFDEF WINDOWS}
    WriteLn('  ✓ WINDOWS condition = ', TUtils.IsConditionMet('WINDOWS', ActiveDefines));
    {$ELSE}
    WriteLn('  ✗ WINDOWS condition = ', TUtils.IsConditionMet('WINDOWS', ActiveDefines));
    {$ENDIF}

    // Test custom defines
    WriteLn('  DEBUG condition = ', TUtils.IsConditionMet('DEBUG', ActiveDefines));
    WriteLn('  MY_CUSTOM_DEFINE condition = ', TUtils.IsConditionMet('MY_CUSTOM_DEFINE', ActiveDefines));
    WriteLn('  NON_EXISTENT condition = ', TUtils.IsConditionMet('NON_EXISTENT', ActiveDefines));

    // Test empty condition (always true)
    WriteLn('  Empty condition = ', TUtils.IsConditionMet('', ActiveDefines));
  finally
    ActiveDefines.Free;
  end;
  WriteLn;

  // Test conditional path filtering - ScanForUnitPathsFiltered
  WriteLn('[Conditional Path - Filtered Unit Scanning]');
  ConditionalPaths := TConditionalPathList.Create;
  ActiveDefines := TStringList.Create;
  try
    ConditionalPaths.FreeObjects := True;

    // Simulate platform-specific paths
    ConditionalPaths.Add(TConditionalPath.Create('platform/x11', 'UNIX'));
    ConditionalPaths.Add(TConditionalPath.Create('platform/gdi', 'WINDOWS'));
    ConditionalPaths.Add(TConditionalPath.Create('platform/cocoa', 'DARWIN'));

    // Test with current platform defines
    UnitPaths := TUtils.ScanForUnitPathsFiltered('src/main/pascal', ConditionalPaths, ActiveDefines);
    try
      WriteLn('  Filtered paths (should exclude wrong platform):');
      if UnitPaths.Count > 0 then
      begin
        for Path in UnitPaths do
          WriteLn('    ', Path);
      end
      else
        WriteLn('    (no subdirectories in src/main/pascal)');
    finally
      UnitPaths.Free;
    end;
  finally
    ConditionalPaths.Free;
    ActiveDefines.Free;
  end;
  WriteLn;

  WriteLn('[SUCCESS] All utility functions tested (including conditional filtering)');
end.
