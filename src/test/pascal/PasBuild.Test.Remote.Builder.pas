{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Remote.Builder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Zipper, fpcunit, testregistry,
  PasBuild.Remote.Builder;

type
  TTestRemoteBuilderCreate = class(TTestCase)
  published
    procedure TestCreateWithDefaults;
    procedure TestCreateWithVerbose;
  end;

  TTestDetectProjectRoot = class(TTestCase)
  private
    FTempDir: string;
    procedure CreateDir(const ARelPath: string);
    procedure CreateFile(const ARelPath, AContent: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProjectXmlAtRoot;
    procedure TestSingleTopLevelDirWithProjectXml;
    procedure TestMultipleTopLevelDirsRaisesError;
    procedure TestNoProjectXmlRaisesError;
    procedure TestEmptyDirectoryRaisesError;
  end;

  TTestCreateArchive = class(TTestCase)
  private
    FTempDir: string;
    procedure CreateTestZip(const AZipPath: string; AEntries: array of string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExtractZipCreatesFiles;
    procedure TestExtractZipWithSubdirectory;
  end;

  TTestBuildCacheFilename = class(TTestCase)
  published
    procedure TestDownloadCachePath;
    procedure TestBuildExtractPath;
  end;

implementation

function DeleteDirectoryRecursive(const ADirName: string): Boolean;
var
  SearchRec: TSearchRec;
  FilePath: string;
begin
  Result := True;
  if FindFirst(IncludeTrailingPathDelimiter(ADirName) + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          FilePath := IncludeTrailingPathDelimiter(ADirName) + SearchRec.Name;
          if (SearchRec.Attr and faDirectory) = faDirectory then
          begin
            if not DeleteDirectoryRecursive(FilePath) then
              Result := False;
          end
          else
          begin
            if not DeleteFile(FilePath) then
              Result := False;
          end;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
  if Result then
    Result := RemoveDir(ADirName);
end;

{ TTestRemoteBuilderCreate }

procedure TTestRemoteBuilderCreate.TestCreateWithDefaults;
var
  Builder: TRemoteBuilder;
begin
  Builder := TRemoteBuilder.Create;
  try
    AssertFalse('Default verbose is false', Builder.Verbose);
  finally
    Builder.Free;
  end;
end;

procedure TTestRemoteBuilderCreate.TestCreateWithVerbose;
var
  Builder: TRemoteBuilder;
begin
  Builder := TRemoteBuilder.Create;
  try
    Builder.Verbose := True;
    AssertTrue('Verbose should be true', Builder.Verbose);
  finally
    Builder.Free;
  end;
end;

{ TTestDetectProjectRoot }

procedure TTestDetectProjectRoot.SetUp;
begin
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'pasbuild-test-builder-' + IntToStr(GetProcessID);
  ForceDirectories(FTempDir);
end;

procedure TTestDetectProjectRoot.TearDown;
begin
  if DirectoryExists(FTempDir) then
    DeleteDirectoryRecursive(FTempDir);
end;

procedure TTestDetectProjectRoot.CreateDir(const ARelPath: string);
begin
  ForceDirectories(IncludeTrailingPathDelimiter(FTempDir) + ARelPath);
end;

procedure TTestDetectProjectRoot.CreateFile(const ARelPath, AContent: string);
var
  F: TextFile;
  FullPath, Dir: string;
begin
  FullPath := IncludeTrailingPathDelimiter(FTempDir) + ARelPath;
  Dir := ExtractFileDir(FullPath);
  if not DirectoryExists(Dir) then
    ForceDirectories(Dir);
  AssignFile(F, FullPath);
  Rewrite(F);
  Write(F, AContent);
  CloseFile(F);
end;

procedure TTestDetectProjectRoot.TestProjectXmlAtRoot;
var
  Result: string;
begin
  CreateFile('project.xml', '<project><name>test</name></project>');
  CreateFile('src/main/pascal/Main.pas', 'program Main; begin end.');

  Result := TRemoteBuilder.DetectProjectRoot(FTempDir);
  AssertEquals('Project root is extraction dir itself',
    ExcludeTrailingPathDelimiter(FTempDir), Result);
end;

procedure TTestDetectProjectRoot.TestSingleTopLevelDirWithProjectXml;
var
  Result: string;
begin
  CreateDir('mylib-1.0.0');
  CreateFile('mylib-1.0.0/project.xml', '<project><name>mylib</name></project>');
  CreateFile('mylib-1.0.0/src/main/pascal/MyLib.pas', 'unit MyLib; interface implementation end.');

  Result := TRemoteBuilder.DetectProjectRoot(FTempDir);
  AssertEquals('Project root is the single subdirectory',
    IncludeTrailingPathDelimiter(FTempDir) + 'mylib-1.0.0', Result);
end;

procedure TTestDetectProjectRoot.TestMultipleTopLevelDirsRaisesError;
var
  ExceptionRaised: Boolean;
begin
  CreateDir('dir1');
  CreateFile('dir1/project.xml', '<project/>');
  CreateDir('dir2');
  CreateFile('dir2/project.xml', '<project/>');

  ExceptionRaised := False;
  try
    TRemoteBuilder.DetectProjectRoot(FTempDir);
  except
    on E: Exception do
    begin
      ExceptionRaised := True;
      AssertTrue('Exception message mentions project.xml',
        Pos('project.xml', E.Message) > 0);
    end;
  end;
  AssertTrue('Should raise exception for ambiguous layout', ExceptionRaised);
end;

procedure TTestDetectProjectRoot.TestNoProjectXmlRaisesError;
var
  ExceptionRaised: Boolean;
begin
  CreateDir('somedir');
  CreateFile('somedir/readme.txt', 'hello');

  ExceptionRaised := False;
  try
    TRemoteBuilder.DetectProjectRoot(FTempDir);
  except
    on E: Exception do
    begin
      ExceptionRaised := True;
      AssertTrue('Exception message mentions project.xml',
        Pos('project.xml', E.Message) > 0);
    end;
  end;
  AssertTrue('Should raise for missing project.xml', ExceptionRaised);
end;

procedure TTestDetectProjectRoot.TestEmptyDirectoryRaisesError;
var
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  try
    TRemoteBuilder.DetectProjectRoot(FTempDir);
  except
    on E: Exception do
    begin
      ExceptionRaised := True;
      AssertTrue('Exception message mentions project.xml',
        Pos('project.xml', E.Message) > 0);
    end;
  end;
  AssertTrue('Should raise for empty dir', ExceptionRaised);
end;

{ TTestCreateArchive }

procedure TTestCreateArchive.SetUp;
begin
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'pasbuild-test-archive-' + IntToStr(GetProcessID);
  ForceDirectories(FTempDir);
end;

procedure TTestCreateArchive.TearDown;
begin
  if DirectoryExists(FTempDir) then
    DeleteDirectoryRecursive(FTempDir);
end;

procedure TTestCreateArchive.CreateTestZip(const AZipPath: string;
  AEntries: array of string);
var
  Zip: TZipper;
  I: Integer;
  EntryPath, Dir: string;
  F: TextFile;
begin
  for I := 0 to High(AEntries) do
  begin
    EntryPath := IncludeTrailingPathDelimiter(FTempDir) + 'stage' +
      DirectorySeparator + AEntries[I];
    Dir := ExtractFileDir(EntryPath);
    if not DirectoryExists(Dir) then
      ForceDirectories(Dir);
    AssignFile(F, EntryPath);
    Rewrite(F);
    Write(F, 'content-' + IntToStr(I));
    CloseFile(F);
  end;

  Zip := TZipper.Create;
  try
    Zip.FileName := AZipPath;
    for I := 0 to High(AEntries) do
    begin
      EntryPath := IncludeTrailingPathDelimiter(FTempDir) + 'stage' +
        DirectorySeparator + AEntries[I];
      Zip.Entries.AddFileEntry(EntryPath, AEntries[I]);
    end;
    Zip.ZipAllFiles;
  finally
    Zip.Free;
  end;
end;

procedure TTestCreateArchive.TestExtractZipCreatesFiles;
var
  ZipPath, ExtractDir: string;
begin
  ZipPath := IncludeTrailingPathDelimiter(FTempDir) + 'test.zip';
  ExtractDir := IncludeTrailingPathDelimiter(FTempDir) + 'extracted';
  ForceDirectories(ExtractDir);

  CreateTestZip(ZipPath, ['project.xml', 'src/main/pascal/Main.pas']);

  TRemoteBuilder.ExtractArchive(ZipPath, ExtractDir);

  AssertTrue('project.xml extracted',
    FileExists(IncludeTrailingPathDelimiter(ExtractDir) + 'project.xml'));
  AssertTrue('Main.pas extracted',
    FileExists(IncludeTrailingPathDelimiter(ExtractDir) +
      'src' + DirectorySeparator + 'main' + DirectorySeparator +
      'pascal' + DirectorySeparator + 'Main.pas'));
end;

procedure TTestCreateArchive.TestExtractZipWithSubdirectory;
var
  ZipPath, ExtractDir: string;
begin
  ZipPath := IncludeTrailingPathDelimiter(FTempDir) + 'test2.zip';
  ExtractDir := IncludeTrailingPathDelimiter(FTempDir) + 'extracted2';
  ForceDirectories(ExtractDir);

  CreateTestZip(ZipPath, ['mylib/project.xml', 'mylib/src/Main.pas']);

  TRemoteBuilder.ExtractArchive(ZipPath, ExtractDir);

  AssertTrue('project.xml extracted in subdir',
    FileExists(IncludeTrailingPathDelimiter(ExtractDir) +
      'mylib' + DirectorySeparator + 'project.xml'));
end;

{ TTestBuildCacheFilename }

procedure TTestBuildCacheFilename.TestDownloadCachePath;
var
  Result: string;
begin
  Result := TRemoteBuilder.GetDownloadCachePath('mylib', '1.0.0');
  AssertTrue('Contains downloads dir',
    Pos('downloads', Result) > 0);
  AssertTrue('Contains name-version-src.zip',
    Pos('mylib-1.0.0-src.zip', Result) > 0);
end;

procedure TTestBuildCacheFilename.TestBuildExtractPath;
var
  Result: string;
begin
  Result := TRemoteBuilder.GetBuildExtractPath('mylib', '1.0.0');
  AssertTrue('Contains builds dir',
    Pos('builds', Result) > 0);
  AssertTrue('Contains name-version',
    Pos('mylib-1.0.0', Result) > 0);
end;

initialization
  RegisterTest(TTestRemoteBuilderCreate);
  RegisterTest(TTestDetectProjectRoot);
  RegisterTest(TTestCreateArchive);
  RegisterTest(TTestBuildCacheFilename);

end.
