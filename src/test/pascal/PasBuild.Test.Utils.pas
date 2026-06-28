{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.CLI,
  PasBuild.Utils,
  PasBuild.Compiler.Backend,
  PasBuild.Compiler.FPC,
  PasBuild.Compiler.Blaise,
  PasBuild.Compiler.Factory;

type
  { Test TUtils.QuotePath }
  TTestQuotePath = class(TTestCase)
  published
    procedure TestPathWithoutSpaces;
    procedure TestPathWithSpaces;
    procedure TestEmptyPath;
    procedure TestPathWithTrailingSeparator;
    procedure TestPathWithMultipleSpaces;
  end;

  { Test TUtils compiler backend accessors }
  TTestFPCExecutable = class(TTestCase)
  protected
    procedure TearDown; override;
  published
    procedure TestDefaultReturnsFpc;
    procedure TestSetCustomPath;
    procedure TestSetAbsolutePath;
    procedure TestResetToDefault;
    procedure TestCustomFPCUsedByIsCompilerAvailable;
  end;

  { Test TCommandLineArgs.CompilerExecutable field }
  TTestCLIFPCExecutable = class(TTestCase)
  published
    procedure TestFPCExecutableFieldExists;
    procedure TestFPCExecutableDefaultEmpty;
    procedure TestFPCExecutableStoresPath;
  end;

  { Test TUtils.XmlEscapeText }
  TTestXmlEscapeText = class(TTestCase)
  published
    procedure TestPlainText;
    procedure TestAmpersand;
    procedure TestLessThan;
    procedure TestGreaterThan;
    procedure TestAllSpecialChars;
    procedure TestEmptyString;
    procedure TestAuthorWithEmail;
    procedure TestDoubleAmpersand;
  end;

  { Test compiler target detection utilities }
  TTestCompilerTargetDetection = class(TTestCase)
  published
    procedure TestDetectTargetCPU;
    procedure TestDetectTargetOS;
    procedure TestGetTargetQuadruplet;
    procedure TestTargetQuadrupletFormat;
    procedure TestGetPackagePlatformSuffixNotEmpty;
    procedure TestGetPackagePlatformSuffixTwoComponents;
    procedure TestGetPackagePlatformSuffixNoCompilerVersion;
    procedure TestGetPackagePlatformSuffixMatchesQuadruplet;
  end;

  { Test Blaise backend --no-incremental handling }
  TTestBlaiseIncremental = class(TTestCase)
  published
    procedure TestDefaultEmitsUnitCache;
    procedure TestDefaultNoIncrementalFlag;
    procedure TestNoIncrementalSuppressesUnitCache;
    procedure TestNoIncrementalEmitsFlag;
    procedure TestFPCIgnoresNoIncremental;
  end;

implementation

{ TTestQuotePath }

procedure TTestQuotePath.TestPathWithoutSpaces;
begin
  AssertEquals('Path without spaces should be unchanged',
    '/home/user/.pasbuild/repository',
    TUtils.QuotePath('/home/user/.pasbuild/repository'));
end;

procedure TTestQuotePath.TestPathWithSpaces;
var
  Quoted: string;
begin
  Quoted := TUtils.QuotePath('/home/John Smith/.pasbuild/repository');
  AssertEquals('Path with spaces should be wrapped in double quotes',
    '"/home/John Smith/.pasbuild/repository"',
    Quoted);
end;

procedure TTestQuotePath.TestEmptyPath;
begin
  AssertEquals('Empty path should remain empty', '', TUtils.QuotePath(''));
end;

procedure TTestQuotePath.TestPathWithTrailingSeparator;
begin
  AssertEquals('Path with spaces and trailing separator should be quoted',
    '"/home/John Smith/projects/"',
    TUtils.QuotePath('/home/John Smith/projects/'));
end;

procedure TTestQuotePath.TestPathWithMultipleSpaces;
begin
  AssertEquals('Path with multiple spaces should be quoted',
    '"/home/My User/My Projects/build output"',
    TUtils.QuotePath('/home/My User/My Projects/build output'));
end;

{ TTestFPCExecutable }

procedure TTestFPCExecutable.TearDown;
begin
  { Reset to default FPC backend after each test to avoid leaking state }
  TUtils.SetCompilerBackend(TFPCBackend.Create('fpc'));
end;

procedure TTestFPCExecutable.TestDefaultReturnsFpc;
begin
  TUtils.SetCompilerBackend(TFPCBackend.Create('fpc'));
  AssertEquals('Default compiler executable should be fpc',
    'fpc', TUtils.GetCompilerBackend.Executable);
end;

procedure TTestFPCExecutable.TestSetCustomPath;
begin
  TUtils.SetCompilerBackend(TFPCBackend.Create('fpc-ootb'));
  AssertEquals('Custom compiler executable should be returned',
    'fpc-ootb', TUtils.GetCompilerBackend.Executable);
end;

procedure TTestFPCExecutable.TestSetAbsolutePath;
begin
  TUtils.SetCompilerBackend(TFPCBackend.Create('/opt/fpc-3.3.1/bin/fpc'));
  AssertEquals('Absolute path should be returned',
    '/opt/fpc-3.3.1/bin/fpc', TUtils.GetCompilerBackend.Executable);
end;

procedure TTestFPCExecutable.TestResetToDefault;
begin
  TUtils.SetCompilerBackend(TFPCBackend.Create('/custom/fpc'));
  AssertEquals('Custom path should be active',
    '/custom/fpc', TUtils.GetCompilerBackend.Executable);

  TUtils.SetCompilerBackend(TFPCBackend.Create('fpc'));
  AssertEquals('After reset, should return default fpc',
    'fpc', TUtils.GetCompilerBackend.Executable);
end;

procedure TTestFPCExecutable.TestCustomFPCUsedByIsCompilerAvailable;
begin
  TUtils.SetCompilerBackend(TFPCBackend.Create('nonexistent-fpc-binary-that-does-not-exist'));
  AssertFalse('IsCompilerAvailable should return False for nonexistent binary',
    TUtils.IsCompilerAvailable);
end;

{ TTestCLIFPCExecutable }

procedure TTestCLIFPCExecutable.TestFPCExecutableFieldExists;
var
  Args: TCommandLineArgs;
begin
  Args.ProfileIds := TStringList.Create;
  try
    Args.CompilerExecutable := '/usr/bin/fpc';
    AssertEquals('CompilerExecutable field should be settable',
      '/usr/bin/fpc', Args.CompilerExecutable);
  finally
    Args.ProfileIds.Free;
  end;
end;

procedure TTestCLIFPCExecutable.TestFPCExecutableDefaultEmpty;
var
  Args: TCommandLineArgs;
begin
  Args.ProfileIds := TStringList.Create;
  try
    Args.CompilerExecutable := '';
    AssertEquals('CompilerExecutable should default to empty string',
      '', Args.CompilerExecutable);
  finally
    Args.ProfileIds.Free;
  end;
end;

procedure TTestCLIFPCExecutable.TestFPCExecutableStoresPath;
var
  Args: TCommandLineArgs;
begin
  Args.ProfileIds := TStringList.Create;
  try
    Args.CompilerExecutable := '/opt/fpc-3.3.1/bin/fpc';
    AssertEquals('Absolute path should be stored',
      '/opt/fpc-3.3.1/bin/fpc', Args.CompilerExecutable);

    Args.CompilerExecutable := 'fpc-ootb';
    AssertEquals('Simple name should be stored',
      'fpc-ootb', Args.CompilerExecutable);
  finally
    Args.ProfileIds.Free;
  end;
end;

{ TTestCompilerTargetDetection }

procedure TTestCompilerTargetDetection.TestDetectTargetCPU;
var
  CPU: string;
begin
  if not TUtils.IsCompilerAvailable then
  begin
    Ignore('Compiler not available');
    Exit;
  end;
  CPU := TUtils.DetectTargetCPU;
  AssertTrue('Target CPU should not be empty', CPU <> '');
  AssertTrue('Target CPU should not be error value', CPU <> 'Unknown');
end;

procedure TTestCompilerTargetDetection.TestDetectTargetOS;
var
  OS: string;
begin
  if not TUtils.IsCompilerAvailable then
  begin
    Ignore('Compiler not available');
    Exit;
  end;
  OS := TUtils.DetectTargetOS;
  AssertTrue('Target OS should not be empty', OS <> '');
  AssertTrue('Target OS should not be error value', OS <> 'Unknown');
end;

procedure TTestCompilerTargetDetection.TestGetTargetQuadruplet;
var
  Quadruplet: string;
begin
  if not TUtils.IsCompilerAvailable then
  begin
    Ignore('Compiler not available');
    Exit;
  end;
  Quadruplet := TUtils.GetTargetQuadruplet;
  AssertTrue('Target quadruplet should not be empty', Quadruplet <> '');
  // Should contain exactly three hyphens (compiler-cpu-os-version)
  AssertEquals('Target quadruplet should contain exactly three hyphens',
    3, Length(Quadruplet) - Length(StringReplace(Quadruplet, '-', '', [rfReplaceAll])));
end;

procedure TTestCompilerTargetDetection.TestTargetQuadrupletFormat;
var
  Quadruplet: string;
  Parts: TStringList;
begin
  if not TUtils.IsCompilerAvailable then
  begin
    Ignore('Compiler not available');
    Exit;
  end;
  Quadruplet := TUtils.GetTargetQuadruplet;

  Parts := TStringList.Create;
  try
    Parts.Delimiter := '-';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Quadruplet;

    AssertTrue('Should have at least 4 parts', Parts.Count >= 4);
    AssertTrue('Compiler component should not be empty', Parts[0] <> '');
    AssertTrue('CPU component should not be empty', Parts[1] <> '');
    AssertTrue('OS component should not be empty', Parts[2] <> '');
    AssertTrue('Version component should not be empty', Parts[Parts.Count - 1] <> '');

    AssertEquals('Compiler component should match DetectCompilerName',
      TUtils.DetectCompilerName, Parts[0]);
    AssertEquals('CPU component should match DetectTargetCPU',
      TUtils.DetectTargetCPU, Parts[1]);
    AssertEquals('OS component should match DetectTargetOS',
      TUtils.DetectTargetOS, Parts[2]);
  finally
    Parts.Free;
  end;
end;

{ TTestCompilerTargetDetection - GetPackagePlatformSuffix }

procedure TTestCompilerTargetDetection.TestGetPackagePlatformSuffixNotEmpty;
var
  Suffix: string;
begin
  if not TUtils.IsCompilerAvailable then
  begin
    Ignore('Compiler not available');
    Exit;
  end;
  Suffix := TUtils.GetPackagePlatformSuffix;
  AssertTrue('Package platform suffix should not be empty', Suffix <> '');
end;

procedure TTestCompilerTargetDetection.TestGetPackagePlatformSuffixTwoComponents;
var
  Suffix: string;
  DashPos: Integer;
begin
  if not TUtils.IsCompilerAvailable then
  begin
    Ignore('Compiler not available');
    Exit;
  end;
  Suffix := TUtils.GetPackagePlatformSuffix;
  DashPos := Pos('-', Suffix);
  AssertTrue('Suffix should contain a hyphen separating cpu and os', DashPos > 0);
  AssertTrue('cpu component should not be empty', DashPos > 1);
  AssertTrue('os component should not be empty', Length(Suffix) > DashPos);
  AssertEquals('Suffix should have exactly one hyphen (cpu-os, no compiler version)',
    0, Pos('-', Copy(Suffix, DashPos + 1, MaxInt)));
end;

procedure TTestCompilerTargetDetection.TestGetPackagePlatformSuffixNoCompilerVersion;
var
  Suffix, CompilerVersion: string;
begin
  if not TUtils.IsCompilerAvailable then
  begin
    Ignore('Compiler not available');
    Exit;
  end;
  Suffix := TUtils.GetPackagePlatformSuffix;
  CompilerVersion := TUtils.DetectCompilerVersion;
  AssertEquals('Package suffix must not contain the compiler version',
    0, Pos(CompilerVersion, Suffix));
end;

procedure TTestCompilerTargetDetection.TestGetPackagePlatformSuffixMatchesQuadruplet;
var
  Suffix, Quadruplet: string;
  Parts: TStringList;
begin
  if not TUtils.IsCompilerAvailable then
  begin
    Ignore('Compiler not available');
    Exit;
  end;
  Suffix := TUtils.GetPackagePlatformSuffix;
  Quadruplet := TUtils.GetTargetQuadruplet;

  Parts := TStringList.Create;
  try
    Parts.Delimiter := '-';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Quadruplet;

    AssertEquals('Package suffix should equal cpu-os from the quadruplet',
      Parts[1] + '-' + Parts[2], Suffix);
  finally
    Parts.Free;
  end;
end;

{ TTestXmlEscapeText }

procedure TTestXmlEscapeText.TestPlainText;
begin
  AssertEquals('Plain text should be unchanged',
    'Hello World', TUtils.XmlEscapeText('Hello World'));
end;

procedure TTestXmlEscapeText.TestAmpersand;
begin
  AssertEquals('Ampersand should be escaped',
    'Smith &amp; Co', TUtils.XmlEscapeText('Smith & Co'));
end;

procedure TTestXmlEscapeText.TestLessThan;
begin
  AssertEquals('Less-than should be escaped',
    'a &lt; b', TUtils.XmlEscapeText('a < b'));
end;

procedure TTestXmlEscapeText.TestGreaterThan;
begin
  AssertEquals('Greater-than should be escaped',
    'a &gt; b', TUtils.XmlEscapeText('a > b'));
end;

procedure TTestXmlEscapeText.TestAllSpecialChars;
begin
  AssertEquals('All special chars should be escaped',
    '&amp; &lt; &gt;', TUtils.XmlEscapeText('& < >'));
end;

procedure TTestXmlEscapeText.TestEmptyString;
begin
  AssertEquals('Empty string should remain empty',
    '', TUtils.XmlEscapeText(''));
end;

procedure TTestXmlEscapeText.TestAuthorWithEmail;
begin
  AssertEquals('Author with email angle brackets should be escaped',
    'John Doe &lt;john@example.org&gt;',
    TUtils.XmlEscapeText('John Doe <john@example.org>'));
end;

procedure TTestXmlEscapeText.TestDoubleAmpersand;
begin
  AssertEquals('Multiple ampersands should each be escaped',
    'A &amp;&amp; B', TUtils.XmlEscapeText('A && B'));
end;

{ TTestBlaiseIncremental }

procedure TTestBlaiseIncremental.TestDefaultEmitsUnitCache;
var
  Backend: TBlaiseBackend;
begin
  Backend := TBlaiseBackend.Create('blaise');
  try
    AssertEquals('--unit-cache /tmp/units',
      Backend.UnitOutputDirFlag('/tmp/units'));
  finally
    Backend.Free;
  end;
end;

procedure TTestBlaiseIncremental.TestDefaultNoIncrementalFlag;
var
  Backend: TBlaiseBackend;
begin
  Backend := TBlaiseBackend.Create('blaise');
  try
    AssertEquals('', Backend.IncrementalFlag);
  finally
    Backend.Free;
  end;
end;

procedure TTestBlaiseIncremental.TestNoIncrementalSuppressesUnitCache;
var
  Backend: TBlaiseBackend;
begin
  Backend := TBlaiseBackend.Create('blaise');
  try
    Backend.NoIncremental := True;
    AssertEquals('', Backend.UnitOutputDirFlag('/tmp/units'));
  finally
    Backend.Free;
  end;
end;

procedure TTestBlaiseIncremental.TestNoIncrementalEmitsFlag;
var
  Backend: TBlaiseBackend;
begin
  Backend := TBlaiseBackend.Create('blaise');
  try
    Backend.NoIncremental := True;
    AssertEquals('--no-incremental', Backend.IncrementalFlag);
  finally
    Backend.Free;
  end;
end;

procedure TTestBlaiseIncremental.TestFPCIgnoresNoIncremental;
var
  Backend: TFPCBackend;
begin
  { FPC has no incremental concept — the flag is a no-op regardless. }
  Backend := TFPCBackend.Create('fpc');
  try
    Backend.NoIncremental := True;
    AssertEquals('', Backend.IncrementalFlag);
  finally
    Backend.Free;
  end;
end;

initialization
  RegisterTest(TTestQuotePath);
  RegisterTest(TTestFPCExecutable);
  RegisterTest(TTestCLIFPCExecutable);
  RegisterTest(TTestXmlEscapeText);
  RegisterTest(TTestCompilerTargetDetection);
  RegisterTest(TTestBlaiseIncremental);

end.
