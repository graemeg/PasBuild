{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Remote.Registry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  PasBuild.Types,
  PasBuild.Settings;

const
  DEFAULT_REGISTRY_ID  = 'central';
  DEFAULT_REGISTRY_URL = 'https://raw.githubusercontent.com/pasbuild/registry/main/';

type
  TRegistryManager = class
  private
    FProjectRepositories: TRepositoryList;
    FSettings: TSettings;
    FEnvRegistryUrl: string;
  public
    constructor Create(AProjectRepositories: TRepositoryList;
      ASettings: TSettings; const AEnvRegistryUrl: string);

    function GetEffectiveRepositories: TRepositoryList;

    class function GetNamePrefix(const AName: string): string;
    class function BuildManifestUrl(const ABaseUrl, AName, AVersion: string): string;
  end;

implementation

{ TRegistryManager }

constructor TRegistryManager.Create(AProjectRepositories: TRepositoryList;
  ASettings: TSettings; const AEnvRegistryUrl: string);
begin
  inherited Create;
  FProjectRepositories := AProjectRepositories;
  FSettings := ASettings;
  FEnvRegistryUrl := AEnvRegistryUrl;
end;

function TRegistryManager.GetEffectiveRepositories: TRepositoryList;
var
  I: Integer;
begin
  Result := TRepositoryList.Create;
  Result.FreeObjects := True;

  if Assigned(FProjectRepositories) and (FProjectRepositories.Count > 0) then
  begin
    for I := 0 to FProjectRepositories.Count - 1 do
      Result.Add(TRepositoryInfo.Create(
        FProjectRepositories[I].Id, FProjectRepositories[I].Url));
    Exit;
  end;

  if Assigned(FSettings) and (FSettings.Repositories.Count > 0) then
  begin
    for I := 0 to FSettings.Repositories.Count - 1 do
      Result.Add(TRepositoryInfo.Create(
        FSettings.Repositories[I].Id, FSettings.Repositories[I].Url));
    Exit;
  end;

  if FEnvRegistryUrl <> '' then
  begin
    Result.Add(TRepositoryInfo.Create('env', FEnvRegistryUrl));
    Exit;
  end;

  Result.Add(TRepositoryInfo.Create(DEFAULT_REGISTRY_ID, DEFAULT_REGISTRY_URL));
end;

class function TRegistryManager.GetNamePrefix(const AName: string): string;
begin
  if Length(AName) >= 2 then
    Result := LowerCase(Copy(AName, 1, 2))
  else if Length(AName) = 1 then
    Result := LowerCase(AName) + '_'
  else
    Result := '__';
end;

class function TRegistryManager.BuildManifestUrl(const ABaseUrl, AName,
  AVersion: string): string;
var
  Base: string;
begin
  Base := ABaseUrl;
  if (Length(Base) > 0) and (Base[Length(Base)] <> '/') then
    Base := Base + '/';
  Result := Base + 'packages/' + GetNamePrefix(AName) + '/' + AName + '/' +
    AVersion + '.xml';
end;

end.
