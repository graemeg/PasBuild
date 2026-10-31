{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Command.Test;

{$mode objfpc}{$H+}

{ Tests for TTestCompileCommand and TTestCommand behaviour when a module
  has no test directory or test executable. Maven-compatible behaviour:
  a missing src/test/pascal/ directory is a skip (exit 0), not a failure. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Types,
  PasBuild.Utils,
  PasBuild.Compiler.FPC,
  PasBuild.Compiler.Blaise,
  PasBuild.Command.Compile,
  PasBuild.Command.Test;

type
  { Subclass to expose the protected BuildCompilerCommand for unit testing }
  TCompileCommandTestable = class(TCompileCommand)
  public
    function ExposedBuildCommand(const ASourcePath: string): string;
  end;

  { Verify that BuildCompilerCommand handles out-of-tree ../ unit paths correctly }
  TTestCompileCommandUnitPaths = class(TTestCase)
  private
    FConfig: TProjectConfig;
    FProfileIds: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { A path starting with ../ must be used as-is, not prefixed with sourceDirectory }
    procedure TestOutOfTreeUnitPathNotPrefixedWithSourceDir;
    { A plain relative path must still be resolved relative to sourceDirectory }
    procedure TestInTreeUnitPathPrefixedWithSourceDir;
  end;

  { Subclass to expose the protected BuildTestCompilerCommand for unit testing }
  TTestCompileCommandTestable = class(TTestCompileCommand)
  public
    function ExposedBuildCommand(const ATestSourcePath: string): string;
  end;

  { Verify that BuildTestCompilerCommand includes ResolvedModulePaths (-Fu flags)
    so that test compilation can find units from module dependencies. }
  TTestCompileCommandModulePaths = class(TTestCase)
  private
    FConfig: TProjectConfig;
    FProfileIds: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { ResolvedModulePaths must appear as -Fu flags in the test compile command }
    procedure TestModuleDepPathsIncludedInTestCompileCommand;
    { Multiple ResolvedModulePaths must all appear as -Fu flags }
    procedure TestMultipleModuleDepPathsAllIncluded;
  end;

  { Blaise compiles every unit from source, and leaves a dependency's
    target/units empty with --no-incremental, so the compile and
    test-compile commands must also name the dependencies' source
    directories.  FPC links the compiled units and must not get them. }
  TTestModuleSourcePaths = class(TTestCase)
  private
    FConfig: TProjectConfig;
    FProfileIds: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBlaiseCompileIncludesDepSourcePaths;
    procedure TestBlaiseTestCompileIncludesDepSourcePaths;
    procedure TestFPCOmitsDepSourcePaths;
  end;

  { Verify that test-compile and test skip gracefully when no test directory
    exists, matching Maven behaviour (no tests = SUCCESS, not FAILURE). }
  TTestCommandNoTestDir = class(TTestCase)
  private
    FSavedDir: string;
    FTempDir: string;
    FConfig: TProjectConfig;
    FProfileIds: TStringList;
    procedure DeleteDirTree(const ADir: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { test-compile must return 0 (skip) when src/test/pascal/ is absent }
    procedure TestTestCompileSkipsWhenNoTestDir;
    { test must return 0 (skip) when no TestRunner executable exists and
      no test directory exists (i.e. test-compile was previously skipped) }
    procedure TestTestSkipsWhenNoTestExecutable;
  end;

implementation

{ TTestCommandNoTestDir }

procedure TTestCommandNoTestDir.DeleteDirTree(const ADir: string);
var
  SR: TSearchRec;
  Entry: string;
begin
  if FindFirst(ADir + DirectorySeparator + '*', faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') then
      begin
        Entry := ADir + DirectorySeparator + SR.Name;
        if SR.Attr and faDirectory <> 0 then
          DeleteDirTree(Entry)
        else
          DeleteFile(Entry);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  RmDir(ADir);
end;

procedure TTestCommandNoTestDir.SetUp;
begin
  FSavedDir := GetCurrentDir;

  { Create a temporary directory that has no src/test/pascal sub-tree }
  FTempDir := GetTempDir(False) + 'pasbuild_test_' + IntToStr(Random(MaxInt));
  ForceDirectories(FTempDir);
  { Also create the target/ dir so commands don't fail on directory creation,
    but deliberately omit src/test/pascal. }
  ForceDirectories(FTempDir + DirectorySeparator + 'target');

  ChDir(FTempDir);

  FConfig := TProjectConfig.Create;
  FConfig.Name := 'test-module';
  FConfig.Version := '1.0.0';

  FProfileIds := TStringList.Create;
end;

procedure TTestCommandNoTestDir.TearDown;
begin
  ChDir(FSavedDir);

  FProfileIds.Free;
  FConfig.Free;

  if (FTempDir <> '') and DirectoryExists(FTempDir) then
    DeleteDirTree(FTempDir);
end;

procedure TTestCommandNoTestDir.TestTestCompileSkipsWhenNoTestDir;
var
  Cmd: TTestCompileCommand;
  ExitCode: Integer;
begin
  AssertFalse('Precondition: src/test/pascal must not exist',
    DirectoryExists('src/test/pascal'));

  Cmd := TTestCompileCommand.Create(FConfig, FProfileIds);
  try
    ExitCode := Cmd.Execute;
  finally
    Cmd.Free;
  end;

  AssertEquals('test-compile should return 0 (skip) when no test dir exists', 0, ExitCode);
end;

procedure TTestCommandNoTestDir.TestTestSkipsWhenNoTestExecutable;
var
  Cmd: TTestCommand;
  ExitCode: Integer;
begin
  AssertFalse('Precondition: src/test/pascal must not exist',
    DirectoryExists('src/test/pascal'));
  AssertFalse('Precondition: TestRunner must not exist',
    FileExists('target' + DirectorySeparator + 'TestRunner'));

  Cmd := TTestCommand.Create(FConfig, FProfileIds);
  try
    ExitCode := Cmd.Execute;
  finally
    Cmd.Free;
  end;

  AssertEquals('test should return 0 (skip) when no test executable exists', 0, ExitCode);
end;

{ TCompileCommandTestable }

function TCompileCommandTestable.ExposedBuildCommand(const ASourcePath: string): string;
begin
  Result := BuildCompilerCommand(ASourcePath);
end;

{ TTestCompileCommandUnitPaths }

procedure TTestCompileCommandUnitPaths.SetUp;
begin
  FConfig := TProjectConfig.Create;
  FConfig.Name := 'test-module';
  FConfig.Version := '1.0.0';
  FProfileIds := TStringList.Create;
end;

procedure TTestCompileCommandUnitPaths.TearDown;
begin
  FProfileIds.Free;
  FConfig.Free;
end;

procedure TTestCompileCommandUnitPaths.TestOutOfTreeUnitPathNotPrefixedWithSourceDir;
var
  Cmd: TCompileCommandTestable;
  CmdLine: string;
  OutOfTreePath: string;
begin
  OutOfTreePath := '../../framework/src/main/pascal/corelib';
  FConfig.BuildConfig.UnitPaths.Add(TConditionalPath.Create(OutOfTreePath));

  Cmd := TCompileCommandTestable.Create(FConfig, FProfileIds);
  try
    CmdLine := Cmd.ExposedBuildCommand('src/main/pascal/main.pas');
  finally
    Cmd.Free;
  end;

  AssertTrue(
    'Out-of-tree path must appear in -Fu without sourceDirectory prefix',
    Pos('-Fu../../framework/src/main/pascal/corelib', CmdLine) > 0
  );
  AssertEquals(
    'Out-of-tree path must NOT be prefixed with sourceDirectory',
    0, Pos('-Fusrc/main/pascal/../../', CmdLine)
  );
end;

procedure TTestCompileCommandUnitPaths.TestInTreeUnitPathPrefixedWithSourceDir;
var
  Cmd: TCompileCommandTestable;
  CmdLine: string;
begin
  FConfig.BuildConfig.UnitPaths.Add(TConditionalPath.Create('corelib'));

  Cmd := TCompileCommandTestable.Create(FConfig, FProfileIds);
  try
    CmdLine := Cmd.ExposedBuildCommand('src/main/pascal/main.pas');
  finally
    Cmd.Free;
  end;

  AssertTrue(
    'In-tree path must be resolved relative to sourceDirectory',
    Pos('-Fusrc/main/pascal/corelib', CmdLine) > 0
  );
end;

{ TTestModuleSourcePaths }

const
  DepUnitsPath = '/dep/core/target/units';
  DepSourcePath = '/dep/core/src/main/pascal';

procedure TTestModuleSourcePaths.SetUp;
begin
  FConfig := TProjectConfig.Create;
  FConfig.Name := 'test-module';
  FConfig.Version := '1.0.0';
  FConfig.BuildConfig.ResolvedModulePaths.Add(DepUnitsPath);
  FConfig.BuildConfig.ResolvedModuleSourcePaths.Add(DepSourcePath);
  FProfileIds := TStringList.Create;
  TUtils.SetCompilerBackend(TBlaiseBackend.Create('blaise'));
end;

procedure TTestModuleSourcePaths.TearDown;
begin
  TUtils.SetCompilerBackend(TFPCBackend.Create('fpc'));
  FProfileIds.Free;
  FConfig.Free;
end;

procedure TTestModuleSourcePaths.TestBlaiseCompileIncludesDepSourcePaths;
var
  Cmd: TCompileCommandTestable;
  CmdLine: string;
begin
  Cmd := TCompileCommandTestable.Create(FConfig, FProfileIds);
  try
    CmdLine := Cmd.ExposedBuildCommand('src/main/pascal/main.pas');
  finally
    Cmd.Free;
  end;

  AssertTrue('Dependency source directory must be a unit path: ' + CmdLine,
    Pos('--unit-path ' + DepSourcePath, CmdLine) > 0);
  AssertTrue('Cached dependency units must be searched before its sources',
    Pos('--unit-path ' + DepUnitsPath, CmdLine) <
    Pos('--unit-path ' + DepSourcePath, CmdLine));
end;

procedure TTestModuleSourcePaths.TestBlaiseTestCompileIncludesDepSourcePaths;
var
  Cmd: TTestCompileCommandTestable;
  CmdLine: string;
begin
  Cmd := TTestCompileCommandTestable.Create(FConfig, FProfileIds);
  try
    CmdLine := Cmd.ExposedBuildCommand('src/test/pascal/TestRunner.pas');
  finally
    Cmd.Free;
  end;

  AssertTrue('Dependency source directory must be a unit path: ' + CmdLine,
    Pos('--unit-path ' + DepSourcePath, CmdLine) > 0);
end;

procedure TTestModuleSourcePaths.TestFPCOmitsDepSourcePaths;
var
  Cmd: TCompileCommandTestable;
  CmdLine: string;
begin
  TUtils.SetCompilerBackend(TFPCBackend.Create('fpc'));
  Cmd := TCompileCommandTestable.Create(FConfig, FProfileIds);
  try
    CmdLine := Cmd.ExposedBuildCommand('src/main/pascal/main.pas');
  finally
    Cmd.Free;
  end;

  AssertTrue('FPC still gets the dependency units', Pos('-Fu' + DepUnitsPath, CmdLine) > 0);
  AssertEquals('FPC must not get the dependency sources',
    0, Pos(DepSourcePath, CmdLine));
end;

{ TTestCompileCommandTestable }

function TTestCompileCommandTestable.ExposedBuildCommand(const ATestSourcePath: string): string;
begin
  Result := BuildTestCompilerCommand(ATestSourcePath);
end;

{ TTestCompileCommandModulePaths }

procedure TTestCompileCommandModulePaths.SetUp;
begin
  FConfig := TProjectConfig.Create;
  FConfig.Name := 'test-module';
  FConfig.Version := '1.0.0';
  FProfileIds := TStringList.Create;
end;

procedure TTestCompileCommandModulePaths.TearDown;
begin
  FProfileIds.Free;
  FConfig.Free;
end;

procedure TTestCompileCommandModulePaths.TestModuleDepPathsIncludedInTestCompileCommand;
var
  Cmd: TTestCompileCommandTestable;
  CmdLine: string;
  ModPath: string;
begin
  ModPath := '/some/dep/target/units';
  FConfig.BuildConfig.ResolvedModulePaths.Add(ModPath);

  Cmd := TTestCompileCommandTestable.Create(FConfig, FProfileIds);
  try
    CmdLine := Cmd.ExposedBuildCommand('src/test/pascal/TestRunner.pas');
  finally
    Cmd.Free;
  end;

  AssertTrue(
    'test compile command must include -Fu for ResolvedModulePaths: ' + ModPath,
    Pos('-Fu' + ModPath, CmdLine) > 0
  );
end;

procedure TTestCompileCommandModulePaths.TestMultipleModuleDepPathsAllIncluded;
var
  Cmd: TTestCompileCommandTestable;
  CmdLine: string;
  Path1, Path2: string;
begin
  Path1 := '/dep/one/target/units';
  Path2 := '/dep/two/target/units';
  FConfig.BuildConfig.ResolvedModulePaths.Add(Path1);
  FConfig.BuildConfig.ResolvedModulePaths.Add(Path2);

  Cmd := TTestCompileCommandTestable.Create(FConfig, FProfileIds);
  try
    CmdLine := Cmd.ExposedBuildCommand('src/test/pascal/TestRunner.pas');
  finally
    Cmd.Free;
  end;

  AssertTrue(
    'test compile command must include -Fu for first module path: ' + Path1,
    Pos('-Fu' + Path1, CmdLine) > 0
  );
  AssertTrue(
    'test compile command must include -Fu for second module path: ' + Path2,
    Pos('-Fu' + Path2, CmdLine) > 0
  );
end;

initialization
  RegisterTest(TTestCompileCommandUnitPaths);
  RegisterTest(TTestCompileCommandModulePaths);
  RegisterTest(TTestModuleSourcePaths);
  RegisterTest(TTestCommandNoTestDir);

end.
