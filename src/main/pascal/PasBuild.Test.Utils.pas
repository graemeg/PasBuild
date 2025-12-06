program PasBuild.Test.Utils;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  PasBuild.Utils;

var
  UnitPaths: TStringList;
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

  WriteLn('[SUCCESS] All utility functions tested');
end.
