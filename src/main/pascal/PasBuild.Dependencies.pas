{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Dependencies;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  PasBuild.Types,
  PasBuild.Repository,
  PasBuild.Settings,
  PasBuild.Remote.Registry,
  PasBuild.Remote.Manifest;

type
  { Exception raised when dependency resolution fails }
  EDependencyError = class(Exception);

  { Resolves external dependencies from the local repository, with remote fallback }
  TDependencyResolver = class
  private
    FVerbose: Boolean;
    FOfflineMode: Boolean;
    FForceFetch: Boolean;
    FVisited: TStringList;
    FRepository: TLocalRepository;
    FSettings: TSettings;
    FProjectRepositories: TRepositoryList;
    procedure ResolveOne(const AName, AVersion, ATargetId: string;
      AResolvedPaths: TStringList);
    function TryRemoteResolve(const AName, AVersion, ATargetId: string;
      AResolvedPaths: TStringList): Boolean;
    function FetchManifestFromRegistries(const AName, AVersion: string): TReleaseManifest;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResolveDependencies(AConfig: TProjectConfig);

    property Verbose: Boolean read FVerbose write FVerbose;
    property OfflineMode: Boolean read FOfflineMode write FOfflineMode;
    property ForceFetch: Boolean read FForceFetch write FForceFetch;
    property Settings: TSettings read FSettings write FSettings;
    property ProjectRepositories: TRepositoryList read FProjectRepositories write FProjectRepositories;
  end;

implementation

uses
  PasBuild.Utils,
  PasBuild.Remote,
  PasBuild.Remote.Builder;

{ TDependencyResolver }

constructor TDependencyResolver.Create;
begin
  inherited Create;
  FVisited := TStringList.Create;
  FVisited.Sorted := True;
  FVisited.Duplicates := dupIgnore;
  FVerbose := False;
  FOfflineMode := False;
  FForceFetch := False;
  FRepository := TLocalRepository.Create;
  FSettings := nil;
  FProjectRepositories := nil;
end;

destructor TDependencyResolver.Destroy;
begin
  FRepository.Free;
  FVisited.Free;
  inherited Destroy;
end;

function TDependencyResolver.FetchManifestFromRegistries(
  const AName, AVersion: string): TReleaseManifest;
var
  Manager: TRegistryManager;
  Repos: TRepositoryList;
  Client: TRemoteClient;
  ConnTimeout, ReadTimeout: Integer;
  EnvUrl, ManifestUrl, XmlContent: string;
  I: Integer;
begin
  Result := nil;
  EnvUrl := GetEnvironmentVariable('PASBUILD_REGISTRY_URL');

  Manager := TRegistryManager.Create(FProjectRepositories, FSettings, EnvUrl);
  try
    Repos := Manager.GetEffectiveRepositories;
    try
      if Assigned(FSettings) then
      begin
        ConnTimeout := FSettings.ConnectTimeout;
        ReadTimeout := FSettings.ReadTimeout;
      end
      else
      begin
        ConnTimeout := 30;
        ReadTimeout := 60;
      end;

      Client := TRemoteClient.Create(ConnTimeout, ReadTimeout);
      try
        for I := 0 to Repos.Count - 1 do
        begin
          ManifestUrl := TRegistryManager.BuildManifestUrl(
            Repos[I].Url, AName, AVersion);
          try
            if FVerbose then
              TUtils.LogInfo('  Checking registry "' + Repos[I].Id +
                '" for ' + AName + ':' + AVersion + '...');

            XmlContent := Client.FetchText(ManifestUrl);
            Result := TManifestParser.ParseVersionManifestFromString(XmlContent);
            Exit;
          except
            on E: ENetworkError do
            begin
              if FVerbose then
                TUtils.LogInfo('  Not found in "' + Repos[I].Id + '": ' + E.Message);
            end;
          end;
        end;
      finally
        Client.Free;
      end;
    finally
      Repos.Free;
    end;
  finally
    Manager.Free;
  end;
end;

function TDependencyResolver.TryRemoteResolve(const AName, AVersion,
  ATargetId: string; AResolvedPaths: TStringList): Boolean;
var
  Manifest: TReleaseManifest;
  Builder: TRemoteBuilder;
  CompilerName, CompilerVersion, MinVer, UnitsPath: string;
  ConnTimeout, ReadTimeout, I: Integer;
begin
  Result := False;

  Manifest := FetchManifestFromRegistries(AName, AVersion);
  if not Assigned(Manifest) then
    Exit;

  try
    CompilerName := TUtils.DetectCompilerName;
    CompilerVersion := TUtils.DetectCompilerVersion;

    if not Manifest.IsCompatibleWith(CompilerName) then
      raise EDependencyError.CreateFmt(
        'Package "%s:%s" is not compatible with compiler "%s". ' +
        'Compatible compilers: %s',
        [AName, AVersion, CompilerName, 'see registry manifest']);

    MinVer := Manifest.MinVersionFor(CompilerName);
    if (MinVer <> '') and (CompilerVersion < MinVer) then
      raise EDependencyError.CreateFmt(
        'Package "%s:%s" requires %s >= %s, but found %s',
        [AName, AVersion, CompilerName, MinVer, CompilerVersion]);

    for I := 0 to Length(Manifest.Dependencies) - 1 do
      ResolveOne(
        Manifest.Dependencies[I].Name,
        Manifest.Dependencies[I].Version,
        ATargetId,
        AResolvedPaths
      );

    if Assigned(FSettings) then
    begin
      ConnTimeout := FSettings.ConnectTimeout;
      ReadTimeout := FSettings.ReadTimeout;
    end
    else
    begin
      ConnTimeout := 30;
      ReadTimeout := 60;
    end;

    TUtils.LogInfo('  Fetching remote dependency: ' + AName + ':' + AVersion);
    Builder := TRemoteBuilder.Create(ConnTimeout, ReadTimeout);
    try
      Builder.Verbose := FVerbose;
      Builder.BuildAndInstall(Manifest);
    finally
      Builder.Free;
    end;

    UnitsPath := FRepository.GetUnitsPath(AName, AVersion, ATargetId);
    if AResolvedPaths.IndexOf(UnitsPath) < 0 then
      AResolvedPaths.Add(UnitsPath);
    Result := True;
  finally
    Manifest.Free;
  end;
end;

procedure TDependencyResolver.ResolveOne(const AName, AVersion, ATargetId: string;
  AResolvedPaths: TStringList);
var
  Key, UnitsPath: string;
  Metadata: TArtifactMetadata;
  I: Integer;
  Targets: TStringList;
  LocalExists: Boolean;
begin
  Key := AName + ':' + AVersion;

  if FVisited.IndexOf(Key) >= 0 then
    Exit;
  FVisited.Add(Key);

  LocalExists := FRepository.ArtifactExists(AName, AVersion, ATargetId);

  if LocalExists and (not FForceFetch) then
  begin
    UnitsPath := FRepository.GetUnitsPath(AName, AVersion, ATargetId);
    if AResolvedPaths.IndexOf(UnitsPath) < 0 then
      AResolvedPaths.Add(UnitsPath);

    if FVerbose then
      TUtils.LogInfo('  Resolved: ' + AName + ':' + AVersion + ' -> ' + UnitsPath);

    Metadata := FRepository.ReadMetadata(AName, AVersion, ATargetId);
    try
      for I := 0 to Length(Metadata.Dependencies) - 1 do
        ResolveOne(
          Metadata.Dependencies[I].Name,
          Metadata.Dependencies[I].Version,
          ATargetId,
          AResolvedPaths
        );
    finally
      Metadata.Free;
    end;
    Exit;
  end;

  if FOfflineMode then
  begin
    Targets := FRepository.GetAvailableTargets(AName, AVersion);
    try
      if Targets.Count > 0 then
        raise EDependencyError.CreateFmt(
          'Dependency "%s:%s" not found for target "%s". ' +
          'Available targets: %s. Recompile and install with matching compiler target.',
          [AName, AVersion, ATargetId, Targets.CommaText])
      else
        raise EDependencyError.CreateFmt(
          'Dependency "%s:%s" not found in local repository. ' +
          'Run "pasbuild install" in the dependency project first.',
          [AName, AVersion]);
    finally
      Targets.Free;
    end;
  end;

  if TryRemoteResolve(AName, AVersion, ATargetId, AResolvedPaths) then
    Exit;

  Targets := FRepository.GetAvailableTargets(AName, AVersion);
  try
    if Targets.Count > 0 then
      raise EDependencyError.CreateFmt(
        'Dependency "%s:%s" not found for target "%s". ' +
        'Available targets: %s. Recompile and install with matching compiler target.',
        [AName, AVersion, ATargetId, Targets.CommaText])
    else
      raise EDependencyError.CreateFmt(
        'Dependency "%s:%s" not found in local repository or any configured registry. ' +
        'Run "pasbuild install" in the dependency project first.',
        [AName, AVersion]);
  finally
    Targets.Free;
  end;
end;

procedure TDependencyResolver.ResolveDependencies(AConfig: TProjectConfig);
var
  TargetId: string;
  I: Integer;
begin
  if AConfig.Dependencies.Count = 0 then
    Exit;

  if AConfig.Dependencies.Count = 1 then
    TUtils.LogInfo('Resolving 1 external dependency...')
  else
    TUtils.LogInfo('Resolving ' + IntToStr(AConfig.Dependencies.Count) +
                   ' external dependencies...');

  TargetId := TUtils.GetTargetQuadruplet;

  // Clear visited set for this resolution run
  FVisited.Clear;

  // Resolve each declared dependency
  for I := 0 to AConfig.Dependencies.Count - 1 do
  begin
    ResolveOne(
      AConfig.Dependencies[I].Name,
      AConfig.Dependencies[I].Version,
      TargetId,
      AConfig.BuildConfig.ResolvedModulePaths
    );
  end;

  TUtils.LogInfo('Dependencies resolved successfully');
end;

end.
