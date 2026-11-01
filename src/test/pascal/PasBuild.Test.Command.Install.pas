{
  This file is part of PasBuild.

  Copyright (c) 2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Command.Install;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Types, PasBuild.Repository, PasBuild.Compiler.Blaise;

type
  { A Blaise backend whose version does not depend on a Blaise binary. }
  TFakeBlaiseBackend = class(TBlaiseBackend)
  public
    function GetVersion: string; override;
  end;

  { Installing a module built with a compiler that compiles its
    dependencies from source (Blaise): the artifact holds the module's
    source tree, with any cached objects as a head start. }
  TTestInstallSourceArtifact = class(TTestCase)
  private
    FRoot: string;
    FConfig: TProjectConfig;
    FProfileIds: TStringList;
    FRepo: TLocalRepository;
    FTargetId: string;
    procedure WriteFile(const APath, AText: string);
    procedure RemoveTree(const ADir: string);
    function ArtifactFile(const ARelative: string): string;
    function RunInstall: Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInstallCopiesTheSourceTreeAndObjects;
    procedure TestInstallWithoutCachedObjectsSucceeds;
    procedure TestReinstallDropsDeletedSources;
    procedure TestInstallWithoutSourcesFails;
  end;

implementation

uses
  PasBuild.Utils, PasBuild.Compiler.FPC, PasBuild.Command.Install;

const
  ArtifactName = 'pasbuild-test-blaise-install';
  ArtifactVersion = '1.0.0';

function TFakeBlaiseBackend.GetVersion: string;
begin
  Result := '9.9.9-test';
end;

procedure TTestInstallSourceArtifact.WriteFile(const APath, AText: string);
var
  L: TStringList;
begin
  ForceDirectories(ExtractFileDir(APath));
  L := TStringList.Create;
  try
    L.Text := AText;
    L.SaveToFile(APath);
  finally
    L.Free;
  end;
end;

procedure TTestInstallSourceArtifact.RemoveTree(const ADir: string);
var
  SR: TSearchRec;
  P: string;
begin
  if not DirectoryExists(ADir) then
    Exit;
  if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*', faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then
        Continue;
      P := IncludeTrailingPathDelimiter(ADir) + SR.Name;
      if (SR.Attr and faDirectory) = faDirectory then
        RemoveTree(P)
      else
        DeleteFile(P);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
  RemoveDir(ADir);
end;

function TTestInstallSourceArtifact.ArtifactFile(const ARelative: string): string;
begin
  Result := FRepo.GetArtifactPath(ArtifactName, ArtifactVersion, FTargetId) +
    DirectorySeparator + ARelative;
end;

function TTestInstallSourceArtifact.RunInstall: Integer;
var
  Cmd: TInstallCommand;
begin
  Cmd := TInstallCommand.Create(FConfig, FProfileIds);
  try
    Result := Cmd.Execute;
  finally
    Cmd.Free;
  end;
end;

procedure TTestInstallSourceArtifact.SetUp;
begin
  TUtils.SetCompilerBackend(TFakeBlaiseBackend.Create('blaise'));
  FTargetId := TUtils.GetTargetQuadruplet;
  FRepo := TLocalRepository.Create;
  FRoot := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'pasbuild-install-test';
  RemoveTree(FRoot);
  WriteFile(FRoot + '/src/main/pascal/lib.core.pas', 'unit lib.core;');
  WriteFile(FRoot + '/src/main/pascal/gui/lib.gui.pas', 'unit lib.gui;');
  WriteFile(FRoot + '/src/main/pascal/gui/shader.txt', 'embedded by lib.gui');
  WriteFile(FRoot + '/target/units/lib.core.o', 'object');
  FConfig := TProjectConfig.Create;
  FConfig.Name := ArtifactName;
  FConfig.Version := ArtifactVersion;
  FConfig.BuildConfig.ProjectType := ptLibrary;
  FConfig.BuildConfig.SourceDirectory := FRoot + '/src/main/pascal';
  FConfig.BuildConfig.OutputDirectory := FRoot + '/target';
  FProfileIds := TStringList.Create;
end;

procedure TTestInstallSourceArtifact.TearDown;
begin
  RemoveTree(FRepo.GetArtifactPath(ArtifactName, ArtifactVersion, FTargetId));
  RemoveDir(TLocalRepository.GetDefaultRoot + DirectorySeparator + ArtifactName +
    DirectorySeparator + ArtifactVersion);
  RemoveDir(TLocalRepository.GetDefaultRoot + DirectorySeparator + ArtifactName);
  RemoveTree(FRoot);
  FProfileIds.Free;
  FConfig.Free;
  FRepo.Free;
  TUtils.SetCompilerBackend(TFPCBackend.Create('fpc'));
end;

procedure TTestInstallSourceArtifact.TestInstallCopiesTheSourceTreeAndObjects;
begin
  AssertEquals('install succeeds', 0, RunInstall);
  AssertTrue('unit', FileExists(ArtifactFile('src/lib.core.pas')));
  AssertTrue('unit in a subdirectory', FileExists(ArtifactFile('src/gui/lib.gui.pas')));
  AssertTrue('embedded file beside its unit', FileExists(ArtifactFile('src/gui/shader.txt')));
  AssertTrue('cached object', FileExists(ArtifactFile('units/lib.core.o')));
  AssertTrue('metadata', FileExists(ArtifactFile('metadata.xml')));
  AssertEquals('sources path', ArtifactFile('src'),
    FRepo.GetSourcesPath(ArtifactName, ArtifactVersion, FTargetId));
end;

{ --no-incremental builds leave no objects; the sources are the artifact. }
procedure TTestInstallSourceArtifact.TestInstallWithoutCachedObjectsSucceeds;
begin
  DeleteFile(FRoot + '/target/units/lib.core.o');
  AssertEquals('install succeeds', 0, RunInstall);
  AssertTrue('unit', FileExists(ArtifactFile('src/lib.core.pas')));
end;

procedure TTestInstallSourceArtifact.TestReinstallDropsDeletedSources;
begin
  AssertEquals(0, RunInstall);
  DeleteFile(FRoot + '/src/main/pascal/gui/lib.gui.pas');
  AssertEquals(0, RunInstall);
  AssertFalse('deleted unit gone from the artifact', FileExists(ArtifactFile('src/gui/lib.gui.pas')));
  AssertTrue('remaining unit kept', FileExists(ArtifactFile('src/lib.core.pas')));
end;

procedure TTestInstallSourceArtifact.TestInstallWithoutSourcesFails;
begin
  RemoveTree(FRoot + '/src');
  AssertEquals('nothing to install', 1, RunInstall);
end;

initialization
  RegisterTest(TTestInstallSourceArtifact);

end.
