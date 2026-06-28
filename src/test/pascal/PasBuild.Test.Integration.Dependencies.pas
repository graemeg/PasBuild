{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Integration.Dependencies;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Types,
  PasBuild.Config,
  PasBuild.Repository,
  PasBuild.Dependencies;

type
  { Integration tests for the full dependency management workflow:
    install -> config loading -> dependency resolution -> compiler path injection }
  TTestIntegrationInstallAndResolve = class(TTestCase)
  private
    FTargetId: string;
    FRepo: TLocalRepository;
    function GetFixturePath(const AFileName: string): string;
    procedure InstallMockArtifact(const AName, AVersion: string;
      const ADeps: array of string);
    procedure CleanupMockArtifact(const AName, AVersion: string);
    procedure RemoveDirRecursive(const ADir: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInstallThenResolveConsumer;
    procedure TestConsumerWithoutInstallFails;
    procedure TestTransitiveDependencyResolution;
    procedure TestInstallOverwritesThenResolves;
    procedure TestResolvedPathsInjectedIntoBuildConfig;
    procedure TestVerboseOutputShowsDetails;
  end;

implementation

uses
  PasBuild.Utils;

{ TTestIntegrationInstallAndResolve }

function TTestIntegrationInstallAndResolve.GetFixturePath(const AFileName: string): string;
begin
  Result := 'fixtures/dependency-management/' + AFileName;
end;

procedure TTestIntegrationInstallAndResolve.InstallMockArtifact(
  const AName, AVersion: string; const ADeps: array of string);
var
  Metadata: TArtifactMetadata;
  DepArray: TMetadataDependencyArray;
  I: Integer;
  UnitsDir, DummyFile: string;
  F: TextFile;
begin
  FRepo.EnsureDirectoryStructure(AName, AVersion, FTargetId);

  // Create dummy .ppu and .o files
  UnitsDir := FRepo.GetUnitsPath(AName, AVersion, FTargetId);
  DummyFile := IncludeTrailingPathDelimiter(UnitsDir) + AName + '.ppu';
  AssignFile(F, DummyFile);
  Rewrite(F);
  WriteLn(F, '// dummy ppu');
  CloseFile(F);

  DummyFile := IncludeTrailingPathDelimiter(UnitsDir) + AName + '.o';
  AssignFile(F, DummyFile);
  Rewrite(F);
  WriteLn(F, '// dummy obj');
  CloseFile(F);

  // Build dependencies array (pairs: name, version, name, version, ...)
  SetLength(DepArray, Length(ADeps) div 2);
  I := 0;
  while I < Length(ADeps) do
  begin
    DepArray[I div 2].Name := ADeps[I];
    DepArray[I div 2].Version := ADeps[I + 1];
    Inc(I, 2);
  end;

  Metadata := TArtifactMetadata.Create;
  try
    Metadata.Name := AName;
    Metadata.Version := AVersion;
    Metadata.Packaging := 'library';
    Metadata.Compiler := 'fpc';
    Metadata.CompilerVersion := '3.2.2';
    Metadata.TargetCPU := 'x86_64';
    Metadata.TargetOS := 'linux';
    Metadata.Timestamp := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now);
    Metadata.Dependencies := DepArray;
    FRepo.WriteMetadata(AName, AVersion, FTargetId, Metadata);
  finally
    Metadata.Free;
  end;
end;

procedure TTestIntegrationInstallAndResolve.RemoveDirRecursive(const ADir: string);
var
  SearchRec: TSearchRec;
  FullPath: string;
begin
  if not DirectoryExists(ADir) then
    Exit;
  if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          FullPath := IncludeTrailingPathDelimiter(ADir) + SearchRec.Name;
          if (SearchRec.Attr and faDirectory) = faDirectory then
            RemoveDirRecursive(FullPath)
          else
            DeleteFile(FullPath);
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
  RemoveDir(ADir);
end;

procedure TTestIntegrationInstallAndResolve.CleanupMockArtifact(
  const AName, AVersion: string);
var
  NameDir: string;
begin
  RemoveDirRecursive(FRepo.GetArtifactPath(AName, AVersion, FTargetId));
  // Try to remove parent dirs if empty
  NameDir := TLocalRepository.GetDefaultRoot + DirectorySeparator + AName;
  RemoveDir(NameDir + DirectorySeparator + AVersion);
  RemoveDir(NameDir);
end;

procedure TTestIntegrationInstallAndResolve.SetUp;
begin
  FTargetId := TUtils.GetTargetQuadruplet;
  FRepo := TLocalRepository.Create;
end;

procedure TTestIntegrationInstallAndResolve.TearDown;
begin
  FRepo.Free;
end;

procedure TTestIntegrationInstallAndResolve.TestInstallThenResolveConsumer;
var
  Config: TProjectConfig;
  Resolver: TDependencyResolver;
  ExpectedUnitsPath: string;
begin
  { Workflow: install test-lib, then load consumer project.xml, resolve deps }
  InstallMockArtifact('test-lib', '1.0.0', []);
  try
    // Load consumer project.xml from fixture
    Config := TConfigLoader.LoadProjectXML(GetFixturePath('consumer-project.xml'));
    try
      AssertEquals('Consumer should have 1 dependency', 1, Config.Dependencies.Count);
      AssertEquals('Dependency name', 'test-lib', Config.Dependencies[0].Name);

      // Resolve dependencies
      Resolver := TDependencyResolver.Create;
      try
        Resolver.ResolveDependencies(Config);
      finally
        Resolver.Free;
      end;

      // Verify resolved paths contain the library units path
      AssertEquals('Should have 1 resolved path', 1,
        Config.BuildConfig.ResolvedModulePaths.Count);

      ExpectedUnitsPath := FRepo.GetUnitsPath('test-lib', '1.0.0', FTargetId);
      AssertEquals('Resolved path should match repository units path',
        ExpectedUnitsPath, Config.BuildConfig.ResolvedModulePaths[0]);
    finally
      Config.Free;
    end;
  finally
    CleanupMockArtifact('test-lib', '1.0.0');
  end;
end;

procedure TTestIntegrationInstallAndResolve.TestConsumerWithoutInstallFails;
var
  Config: TProjectConfig;
  Resolver: TDependencyResolver;
  ExceptionRaised: Boolean;
begin
  { Load consumer that references a dependency that's NOT installed }
  ExceptionRaised := False;
  Config := TConfigLoader.LoadProjectXML(
    GetFixturePath('consumer-missing-dep-project.xml'));
  try
    Resolver := TDependencyResolver.Create;
    try
      try
        Resolver.ResolveDependencies(Config);
      except
        on E: EDependencyError do
        begin
          ExceptionRaised := True;
          AssertTrue('Error should mention dependency name',
            Pos('nonexistent-lib', E.Message) > 0);
          AssertTrue('Error should suggest install',
            Pos('install', LowerCase(E.Message)) > 0);
        end;
      end;
    finally
      Resolver.Free;
    end;

    AssertTrue('Should fail when dependency not installed', ExceptionRaised);
  finally
    Config.Free;
  end;
end;

procedure TTestIntegrationInstallAndResolve.TestTransitiveDependencyResolution;
var
  Config: TProjectConfig;
  Resolver: TDependencyResolver;
  BaseUnitsPath, MidUnitsPath: string;
  FoundBase, FoundMid: Boolean;
  I: Integer;
begin
  { Install base-lib (no deps) and mid-lib (depends on base-lib),
    then resolve consumer that only declares mid-lib }
  InstallMockArtifact('test-base-lib', '1.0.0', []);
  InstallMockArtifact('test-mid-lib', '1.0.0', ['test-base-lib', '1.0.0']);
  try
    Config := TConfigLoader.LoadProjectXML(
      GetFixturePath('consumer-transitive-project.xml'));
    try
      AssertEquals('Consumer declares 1 direct dependency', 1,
        Config.Dependencies.Count);

      Resolver := TDependencyResolver.Create;
      try
        Resolver.ResolveDependencies(Config);
      finally
        Resolver.Free;
      end;

      // Should have both mid-lib and base-lib paths (transitive)
      AssertEquals('Should have 2 resolved paths (direct + transitive)', 2,
        Config.BuildConfig.ResolvedModulePaths.Count);

      // Verify both paths are present
      BaseUnitsPath := FRepo.GetUnitsPath('test-base-lib', '1.0.0', FTargetId);
      MidUnitsPath := FRepo.GetUnitsPath('test-mid-lib', '1.0.0', FTargetId);

      FoundBase := False;
      FoundMid := False;
      for I := 0 to Config.BuildConfig.ResolvedModulePaths.Count - 1 do
      begin
        if Config.BuildConfig.ResolvedModulePaths[I] = BaseUnitsPath then
          FoundBase := True;
        if Config.BuildConfig.ResolvedModulePaths[I] = MidUnitsPath then
          FoundMid := True;
      end;

      AssertTrue('Should include transitive dependency (base-lib)', FoundBase);
      AssertTrue('Should include direct dependency (mid-lib)', FoundMid);
    finally
      Config.Free;
    end;
  finally
    CleanupMockArtifact('test-base-lib', '1.0.0');
    CleanupMockArtifact('test-mid-lib', '1.0.0');
  end;
end;

procedure TTestIntegrationInstallAndResolve.TestInstallOverwritesThenResolves;
var
  Config: TProjectConfig;
  Resolver: TDependencyResolver;
  Metadata: TArtifactMetadata;
begin
  { Install test-lib, overwrite with updated metadata, then resolve }
  InstallMockArtifact('test-lib', '1.0.0', []);

  // Overwrite - install again (simulates re-install)
  InstallMockArtifact('test-lib', '1.0.0', []);

  try
    // Verify the artifact still exists and resolves
    AssertTrue('Artifact should exist after overwrite',
      FRepo.ArtifactExists('test-lib', '1.0.0', FTargetId));

    // Read metadata to verify it's valid
    Metadata := FRepo.ReadMetadata('test-lib', '1.0.0', FTargetId);
    try
      AssertEquals('Metadata name should match', 'test-lib', Metadata.Name);
    finally
      Metadata.Free;
    end;

    // Resolve consumer
    Config := TConfigLoader.LoadProjectXML(GetFixturePath('consumer-project.xml'));
    try
      Resolver := TDependencyResolver.Create;
      try
        Resolver.ResolveDependencies(Config);
      finally
        Resolver.Free;
      end;

      AssertEquals('Should resolve after overwrite', 1,
        Config.BuildConfig.ResolvedModulePaths.Count);
    finally
      Config.Free;
    end;
  finally
    CleanupMockArtifact('test-lib', '1.0.0');
  end;
end;

procedure TTestIntegrationInstallAndResolve.TestResolvedPathsInjectedIntoBuildConfig;
var
  Config: TProjectConfig;
  Resolver: TDependencyResolver;
  UnitsPath: string;
begin
  { Verify that resolved paths end up in BuildConfig.ResolvedModulePaths,
    which is what the compile command reads for -Fu flags }
  InstallMockArtifact('test-lib', '1.0.0', []);
  try
    Config := TConfigLoader.LoadProjectXML(GetFixturePath('consumer-project.xml'));
    try
      // Before resolution, no resolved paths
      AssertEquals('No resolved paths before resolution', 0,
        Config.BuildConfig.ResolvedModulePaths.Count);

      Resolver := TDependencyResolver.Create;
      try
        Resolver.ResolveDependencies(Config);
      finally
        Resolver.Free;
      end;

      // After resolution, path should be in ResolvedModulePaths
      AssertEquals('Should have resolved path', 1,
        Config.BuildConfig.ResolvedModulePaths.Count);

      UnitsPath := Config.BuildConfig.ResolvedModulePaths[0];
      AssertTrue('Path should contain artifact name',
        Pos('test-lib', UnitsPath) > 0);
      AssertTrue('Path should contain version',
        Pos('1.0.0', UnitsPath) > 0);
      AssertTrue('Path should end with units',
        Copy(UnitsPath, Length(UnitsPath) - 4, 5) = 'units');
    finally
      Config.Free;
    end;
  finally
    CleanupMockArtifact('test-lib', '1.0.0');
  end;
end;

procedure TTestIntegrationInstallAndResolve.TestVerboseOutputShowsDetails;
var
  Config: TProjectConfig;
  Resolver: TDependencyResolver;
begin
  { Verify verbose mode doesn't crash - details are logged to stdout }
  InstallMockArtifact('test-lib', '1.0.0', []);
  try
    Config := TConfigLoader.LoadProjectXML(GetFixturePath('consumer-project.xml'));
    try
      Resolver := TDependencyResolver.Create;
      try
        Resolver.Verbose := True;
        Resolver.ResolveDependencies(Config);
      finally
        Resolver.Free;
      end;

      // If we get here without exception, verbose mode works
      AssertEquals('Should resolve in verbose mode', 1,
        Config.BuildConfig.ResolvedModulePaths.Count);
    finally
      Config.Free;
    end;
  finally
    CleanupMockArtifact('test-lib', '1.0.0');
  end;
end;

initialization
  RegisterTest(TTestIntegrationInstallAndResolve);

end.
