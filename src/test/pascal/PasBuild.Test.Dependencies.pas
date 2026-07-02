{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Dependencies;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Types,
  PasBuild.Repository,
  PasBuild.Settings,
  PasBuild.Dependencies;

type
  { Test dependency resolution from local repository }
  TTestDependencyResolver = class(TTestCase)
  private
    FRepo: TLocalRepository;
    FTargetId: string;
    procedure InstallMockArtifact(const AName, AVersion: string;
      const ADeps: array of string);
    procedure CleanupMockArtifact(const AName, AVersion: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestResolveSingleDependency;
    procedure TestResolveMultipleDependencies;
    procedure TestResolveTransitiveDependency;
    procedure TestResolveMissingDependency;
    procedure TestResolveTargetMismatch;
    procedure TestResolveNoDependencies;
    procedure TestResolveCyclicDependency;
  end;

  { Test the CreateConfigured factory that wires CLI flags, settings.xml and
    project repositories into new resolver instances }
  TTestResolverConfiguration = class(TTestCase)
  private
    FSettingsPath: string;
    procedure WriteSettingsFile(const AContent: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConfiguredDefaults;
    procedure TestGlobalOfflineMode;
    procedure TestGlobalForceFetch;
    procedure TestSettingsOfflineMode;
    procedure TestSettingsRepositoriesLoaded;
    procedure TestProjectRepositoriesWired;
    procedure TestOfflineMissingDepFailsWithoutNetwork;
  end;

implementation

uses
  PasBuild.Utils;

{ TTestDependencyResolver }

procedure TTestDependencyResolver.InstallMockArtifact(const AName, AVersion: string;
  const ADeps: array of string);
var
  Metadata: TArtifactMetadata;
  DepArray: TMetadataDependencyArray;
  I: Integer;
  UnitsDir, DummyFile: string;
  F: TextFile;
begin
  // Create directory structure
  FRepo.EnsureDirectoryStructure(AName, AVersion, FTargetId);

  // Create a dummy .ppu file so the artifact looks real
  UnitsDir := FRepo.GetUnitsPath(AName, AVersion, FTargetId);
  DummyFile := IncludeTrailingPathDelimiter(UnitsDir) + AName + '.ppu';
  AssignFile(F, DummyFile);
  Rewrite(F);
  WriteLn(F, '// dummy');
  CloseFile(F);

  // Build dependencies array (pairs of name:version)
  SetLength(DepArray, Length(ADeps) div 2);
  I := 0;
  while I < Length(ADeps) do
  begin
    DepArray[I div 2].Name := ADeps[I];
    DepArray[I div 2].Version := ADeps[I + 1];
    Inc(I, 2);
  end;

  // Write metadata
  Metadata := TArtifactMetadata.Create;
  try
    Metadata.Name := AName;
    Metadata.Version := AVersion;
    Metadata.Packaging := 'library';
    Metadata.Compiler := 'fpc';
    Metadata.CompilerVersion := '3.2.2';
    Metadata.TargetCPU := 'x86_64';
    Metadata.TargetOS := 'linux';
    Metadata.Timestamp := '2026-01-01T00:00:00';
    Metadata.Dependencies := DepArray;
    FRepo.WriteMetadata(AName, AVersion, FTargetId, Metadata);
  finally
    Metadata.Free;
  end;
end;

procedure TTestDependencyResolver.CleanupMockArtifact(const AName, AVersion: string);
var
  ArtifactPath: string;

  procedure RemoveDirRecursive(const ADir: string);
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

begin
  ArtifactPath := FRepo.GetArtifactPath(AName, AVersion, FTargetId);
  RemoveDirRecursive(ArtifactPath);
end;

procedure TTestDependencyResolver.SetUp;
begin
  FRepo := TLocalRepository.Create;
  FTargetId := TUtils.GetTargetQuadruplet;
end;

procedure TTestDependencyResolver.TearDown;
begin
  FRepo.Free;
  // Cleanup is done per-test in each test method
end;

procedure TTestDependencyResolver.TestResolveSingleDependency;
var
  Config: TProjectConfig;
  Resolver: TDependencyResolver;
begin
  InstallMockArtifact('test-dep-lib', '1.0.0', []);
  try
    Config := TProjectConfig.Create;
    try
      Config.Dependencies.Add(TDependencyInfo.Create('test-dep-lib', '1.0.0'));

      Resolver := TDependencyResolver.Create;
      try
        Resolver.ResolveDependencies(Config);

        AssertEquals('Should have 1 resolved path', 1,
          Config.BuildConfig.ResolvedModulePaths.Count);
        AssertTrue('Path should point to units dir',
          Pos('test-dep-lib', Config.BuildConfig.ResolvedModulePaths[0]) > 0);
      finally
        Resolver.Free;
      end;
    finally
      Config.Free;
    end;
  finally
    CleanupMockArtifact('test-dep-lib', '1.0.0');
  end;
end;

procedure TTestDependencyResolver.TestResolveMultipleDependencies;
var
  Config: TProjectConfig;
  Resolver: TDependencyResolver;
begin
  InstallMockArtifact('test-dep-a', '1.0.0', []);
  InstallMockArtifact('test-dep-b', '2.0.0', []);
  try
    Config := TProjectConfig.Create;
    try
      Config.Dependencies.Add(TDependencyInfo.Create('test-dep-a', '1.0.0'));
      Config.Dependencies.Add(TDependencyInfo.Create('test-dep-b', '2.0.0'));

      Resolver := TDependencyResolver.Create;
      try
        Resolver.ResolveDependencies(Config);

        AssertEquals('Should have 2 resolved paths', 2,
          Config.BuildConfig.ResolvedModulePaths.Count);
      finally
        Resolver.Free;
      end;
    finally
      Config.Free;
    end;
  finally
    CleanupMockArtifact('test-dep-a', '1.0.0');
    CleanupMockArtifact('test-dep-b', '2.0.0');
  end;
end;

procedure TTestDependencyResolver.TestResolveTransitiveDependency;
var
  Config: TProjectConfig;
  Resolver: TDependencyResolver;
begin
  // dep-base has no deps, dep-mid depends on dep-base
  InstallMockArtifact('test-dep-base', '1.0.0', []);
  InstallMockArtifact('test-dep-mid', '1.0.0', ['test-dep-base', '1.0.0']);
  try
    Config := TProjectConfig.Create;
    try
      // Only declare dep-mid; dep-base should be resolved transitively
      Config.Dependencies.Add(TDependencyInfo.Create('test-dep-mid', '1.0.0'));

      Resolver := TDependencyResolver.Create;
      try
        Resolver.ResolveDependencies(Config);

        AssertEquals('Should have 2 resolved paths (direct + transitive)', 2,
          Config.BuildConfig.ResolvedModulePaths.Count);
      finally
        Resolver.Free;
      end;
    finally
      Config.Free;
    end;
  finally
    CleanupMockArtifact('test-dep-base', '1.0.0');
    CleanupMockArtifact('test-dep-mid', '1.0.0');
  end;
end;

procedure TTestDependencyResolver.TestResolveMissingDependency;
var
  Config: TProjectConfig;
  Resolver: TDependencyResolver;
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  Config := TProjectConfig.Create;
  try
    Config.Dependencies.Add(TDependencyInfo.Create('nonexistent-lib', '1.0.0'));

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
          AssertTrue('Error should mention install',
            Pos('install', LowerCase(E.Message)) > 0);
        end;
      end;
    finally
      Resolver.Free;
    end;

    AssertTrue('Missing dependency should raise exception', ExceptionRaised);
  finally
    Config.Free;
  end;
end;

procedure TTestDependencyResolver.TestResolveTargetMismatch;
var
  Config: TProjectConfig;
  Resolver: TDependencyResolver;
  Metadata: TArtifactMetadata;
  WrongTargetId: string;
  UnitsDir, DummyFile: string;
  F: TextFile;
  ExceptionRaised: Boolean;
begin
  WrongTargetId := 'fpc-arm-darwin-3.2.2';
  FRepo.EnsureDirectoryStructure('test-dep-wrong-target', '1.0.0', WrongTargetId);

  UnitsDir := FRepo.GetUnitsPath('test-dep-wrong-target', '1.0.0', WrongTargetId);
  DummyFile := IncludeTrailingPathDelimiter(UnitsDir) + 'dummy.ppu';
  AssignFile(F, DummyFile);
  Rewrite(F);
  WriteLn(F, '// dummy');
  CloseFile(F);

  Metadata := TArtifactMetadata.Create;
  try
    Metadata.Name := 'test-dep-wrong-target';
    Metadata.Version := '1.0.0';
    Metadata.Packaging := 'library';
    Metadata.Compiler := 'fpc';
    Metadata.CompilerVersion := '3.2.2';
    Metadata.TargetCPU := 'arm';
    Metadata.TargetOS := 'darwin';
    Metadata.Timestamp := '2026-01-01T00:00:00';
    FRepo.WriteMetadata('test-dep-wrong-target', '1.0.0', WrongTargetId, Metadata);
  finally
    Metadata.Free;
  end;

  ExceptionRaised := False;
  Config := TProjectConfig.Create;
  try
    Config.Dependencies.Add(TDependencyInfo.Create('test-dep-wrong-target', '1.0.0'));

    Resolver := TDependencyResolver.Create;
    try
      try
        Resolver.ResolveDependencies(Config);
      except
        on E: EDependencyError do
        begin
          ExceptionRaised := True;
          AssertTrue('Error should mention target',
            Pos('target', LowerCase(E.Message)) > 0);
          AssertTrue('Error should list available targets',
            Pos('fpc-arm-darwin-3.2.2', E.Message) > 0);
        end;
      end;
    finally
      Resolver.Free;
    end;

    AssertTrue('Target mismatch should raise exception', ExceptionRaised);
  finally
    Config.Free;
    DeleteFile(IncludeTrailingPathDelimiter(
      FRepo.GetUnitsPath('test-dep-wrong-target', '1.0.0', WrongTargetId)) + 'dummy.ppu');
    DeleteFile(FRepo.GetMetadataPath('test-dep-wrong-target', '1.0.0', WrongTargetId));
    RemoveDir(FRepo.GetUnitsPath('test-dep-wrong-target', '1.0.0', WrongTargetId));
    RemoveDir(FRepo.GetArtifactPath('test-dep-wrong-target', '1.0.0', WrongTargetId));
    // Remove version and name directories if empty
    RemoveDir(TLocalRepository.GetDefaultRoot + DirectorySeparator +
              'test-dep-wrong-target' + DirectorySeparator + '1.0.0');
    RemoveDir(TLocalRepository.GetDefaultRoot + DirectorySeparator +
              'test-dep-wrong-target');
  end;
end;

procedure TTestDependencyResolver.TestResolveNoDependencies;
var
  Config: TProjectConfig;
  Resolver: TDependencyResolver;
begin
  Config := TProjectConfig.Create;
  try
    // No dependencies declared
    Resolver := TDependencyResolver.Create;
    try
      Resolver.ResolveDependencies(Config);
      AssertEquals('No paths should be resolved', 0,
        Config.BuildConfig.ResolvedModulePaths.Count);
    finally
      Resolver.Free;
    end;
  finally
    Config.Free;
  end;
end;

procedure TTestDependencyResolver.TestResolveCyclicDependency;
var
  Config: TProjectConfig;
  Resolver: TDependencyResolver;
begin
  // Create two artifacts that depend on each other
  InstallMockArtifact('test-cycle-a', '1.0.0', ['test-cycle-b', '1.0.0']);
  InstallMockArtifact('test-cycle-b', '1.0.0', ['test-cycle-a', '1.0.0']);
  try
    Config := TProjectConfig.Create;
    try
      Config.Dependencies.Add(TDependencyInfo.Create('test-cycle-a', '1.0.0'));

      Resolver := TDependencyResolver.Create;
      try
        // Should NOT infinite loop - cycle detection via visited set
        Resolver.ResolveDependencies(Config);

        AssertEquals('Should resolve both (no infinite loop)', 2,
          Config.BuildConfig.ResolvedModulePaths.Count);
      finally
        Resolver.Free;
      end;
    finally
      Config.Free;
    end;
  finally
    CleanupMockArtifact('test-cycle-a', '1.0.0');
    CleanupMockArtifact('test-cycle-b', '1.0.0');
  end;
end;

{ TTestResolverConfiguration }

procedure TTestResolverConfiguration.WriteSettingsFile(const AContent: string);
var
  F: TextFile;
begin
  AssignFile(F, FSettingsPath);
  Rewrite(F);
  Write(F, AContent);
  CloseFile(F);
end;

procedure TTestResolverConfiguration.SetUp;
begin
  FSettingsPath := IncludeTrailingPathDelimiter(GetTempDir) +
    'pasbuild_test_settings_' + IntToStr(GetProcessID) + '.xml';
  TDependencyResolver.ConfigureGlobal(False, False);
end;

procedure TTestResolverConfiguration.TearDown;
begin
  TDependencyResolver.ConfigureGlobal(False, False);
  if FileExists(FSettingsPath) then
    DeleteFile(FSettingsPath);
end;

procedure TTestResolverConfiguration.TestConfiguredDefaults;
var
  Resolver: TDependencyResolver;
begin
  // No settings file, no global flags: everything off, settings defaulted
  Resolver := TDependencyResolver.CreateConfigured(nil, FSettingsPath);
  try
    AssertFalse('OfflineMode should default to off', Resolver.OfflineMode);
    AssertFalse('ForceFetch should default to off', Resolver.ForceFetch);
    AssertNotNull('Settings should always be assigned', Resolver.Settings);
    AssertEquals('Default connect timeout', 30, Resolver.Settings.ConnectTimeout);
    AssertEquals('Default read timeout', 60, Resolver.Settings.ReadTimeout);
  finally
    Resolver.Free;
  end;
end;

procedure TTestResolverConfiguration.TestGlobalOfflineMode;
var
  Resolver: TDependencyResolver;
begin
  TDependencyResolver.ConfigureGlobal(True, False);
  Resolver := TDependencyResolver.CreateConfigured(nil, FSettingsPath);
  try
    AssertTrue('Global offline flag should reach resolver', Resolver.OfflineMode);
    AssertFalse('ForceFetch should stay off', Resolver.ForceFetch);
  finally
    Resolver.Free;
  end;
end;

procedure TTestResolverConfiguration.TestGlobalForceFetch;
var
  Resolver: TDependencyResolver;
begin
  TDependencyResolver.ConfigureGlobal(False, True);
  Resolver := TDependencyResolver.CreateConfigured(nil, FSettingsPath);
  try
    AssertTrue('Global force-fetch flag should reach resolver', Resolver.ForceFetch);
    AssertFalse('OfflineMode should stay off', Resolver.OfflineMode);
  finally
    Resolver.Free;
  end;
end;

procedure TTestResolverConfiguration.TestSettingsOfflineMode;
var
  Resolver: TDependencyResolver;
begin
  WriteSettingsFile(
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<settings>' + LineEnding +
    '  <offlineMode>true</offlineMode>' + LineEnding +
    '</settings>');

  // Global flag off; settings.xml alone should switch offline on
  Resolver := TDependencyResolver.CreateConfigured(nil, FSettingsPath);
  try
    AssertTrue('settings.xml offlineMode should reach resolver',
      Resolver.OfflineMode);
  finally
    Resolver.Free;
  end;
end;

procedure TTestResolverConfiguration.TestSettingsRepositoriesLoaded;
var
  Resolver: TDependencyResolver;
begin
  WriteSettingsFile(
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<settings>' + LineEnding +
    '  <repositories>' + LineEnding +
    '    <repository>' + LineEnding +
    '      <id>mirror</id>' + LineEnding +
    '      <url>https://mirror.example.org/registry/</url>' + LineEnding +
    '    </repository>' + LineEnding +
    '  </repositories>' + LineEnding +
    '</settings>');

  Resolver := TDependencyResolver.CreateConfigured(nil, FSettingsPath);
  try
    AssertEquals('settings.xml repositories should be loaded', 1,
      Resolver.Settings.Repositories.Count);
    AssertEquals('mirror', Resolver.Settings.Repositories[0].Id);
  finally
    Resolver.Free;
  end;
end;

procedure TTestResolverConfiguration.TestProjectRepositoriesWired;
var
  Resolver: TDependencyResolver;
  Repos: TRepositoryList;
begin
  Repos := TRepositoryList.Create;
  try
    Repos.FreeObjects := True;
    Repos.Add(TRepositoryInfo.Create('project-repo', 'https://example.org/reg/'));

    Resolver := TDependencyResolver.CreateConfigured(Repos, FSettingsPath);
    try
      AssertSame('Project repositories should be wired through',
        Repos, Resolver.ProjectRepositories);
    finally
      Resolver.Free;
    end;
  finally
    Repos.Free;
  end;
end;

procedure TTestResolverConfiguration.TestOfflineMissingDepFailsWithoutNetwork;
var
  Config: TProjectConfig;
  Resolver: TDependencyResolver;
  ExceptionRaised: Boolean;
begin
  // In offline mode a missing dependency must fail fast on the local
  // repository check; the remote-resolve path (network) is never reached
  ExceptionRaised := False;
  TDependencyResolver.ConfigureGlobal(True, False);

  Config := TProjectConfig.Create;
  try
    Config.Dependencies.Add(
      TDependencyInfo.Create('nonexistent-offline-lib', '1.0.0'));

    Resolver := TDependencyResolver.CreateConfigured(nil, FSettingsPath);
    try
      try
        Resolver.ResolveDependencies(Config);
      except
        on E: EDependencyError do
        begin
          ExceptionRaised := True;
          AssertTrue('Offline error should mention local repository',
            Pos('local repository', E.Message) > 0);
          AssertTrue('Offline error should not claim a registry was consulted',
            Pos('registry', LowerCase(E.Message)) = 0);
        end;
      end;
    finally
      Resolver.Free;
    end;

    AssertTrue('Missing dependency in offline mode should raise',
      ExceptionRaised);
  finally
    Config.Free;
  end;
end;

initialization
  RegisterTest(TTestDependencyResolver);
  RegisterTest(TTestResolverConfiguration);

end.
