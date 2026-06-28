{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Remote.Registry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Types,
  PasBuild.Settings,
  PasBuild.Remote.Registry;

type
  TTestNamePrefix = class(TTestCase)
  published
    procedure TestShortName;
    procedure TestLongName;
    procedure TestSingleChar;
    procedure TestTwoChars;
    procedure TestHyphenatedName;
  end;

  TTestManifestUrl = class(TTestCase)
  published
    procedure TestBuildManifestUrl;
    procedure TestBuildManifestUrlTrailingSlash;
    procedure TestBuildManifestUrlNoTrailingSlash;
  end;

  TTestRegistryManager = class(TTestCase)
  published
    procedure TestDefaultRegistryWhenNoConfig;
    procedure TestProjectRepositoriesTakePrecedence;
    procedure TestSettingsRepositoriesUsedWhenNoProject;
    procedure TestEnvVarUsedWhenNoProjectOrSettings;
    procedure TestProjectOverridesEverything;
  end;

implementation

{ TTestNamePrefix }

procedure TTestNamePrefix.TestShortName;
begin
  AssertEquals('fp prefix', 'fp', TRegistryManager.GetNamePrefix('fpgui-framework'));
end;

procedure TTestNamePrefix.TestLongName;
begin
  AssertEquals('my prefix', 'my', TRegistryManager.GetNamePrefix('my-awesome-library'));
end;

procedure TTestNamePrefix.TestSingleChar;
begin
  AssertEquals('Single char padded', 'x_', TRegistryManager.GetNamePrefix('x'));
end;

procedure TTestNamePrefix.TestTwoChars;
begin
  AssertEquals('Two chars exact', 'ab', TRegistryManager.GetNamePrefix('ab'));
end;

procedure TTestNamePrefix.TestHyphenatedName;
begin
  AssertEquals('li prefix', 'li', TRegistryManager.GetNamePrefix('lib-core'));
end;

{ TTestManifestUrl }

procedure TTestManifestUrl.TestBuildManifestUrl;
begin
  AssertEquals('Manifest URL',
    'https://example.com/registry/packages/fp/fpgui-framework/1.0.0.xml',
    TRegistryManager.BuildManifestUrl(
      'https://example.com/registry/', 'fpgui-framework', '1.0.0'));
end;

procedure TTestManifestUrl.TestBuildManifestUrlTrailingSlash;
begin
  AssertEquals('URL with trailing slash',
    'https://example.com/packages/te/test-lib/2.0.0.xml',
    TRegistryManager.BuildManifestUrl(
      'https://example.com/', 'test-lib', '2.0.0'));
end;

procedure TTestManifestUrl.TestBuildManifestUrlNoTrailingSlash;
begin
  AssertEquals('URL without trailing slash gets one',
    'https://example.com/packages/te/test-lib/2.0.0.xml',
    TRegistryManager.BuildManifestUrl(
      'https://example.com', 'test-lib', '2.0.0'));
end;

{ TTestRegistryManager }

procedure TTestRegistryManager.TestDefaultRegistryWhenNoConfig;
var
  Manager: TRegistryManager;
  Repos: TRepositoryList;
begin
  Manager := TRegistryManager.Create(nil, nil, '');
  try
    Repos := Manager.GetEffectiveRepositories;
    try
      AssertEquals('Should have default registry', 1, Repos.Count);
      AssertEquals('Default id', 'central', Repos[0].Id);
      AssertTrue('Default URL is HTTPS',
        Pos('https://', Repos[0].Url) = 1);
    finally
      Repos.Free;
    end;
  finally
    Manager.Free;
  end;
end;

procedure TTestRegistryManager.TestProjectRepositoriesTakePrecedence;
var
  Manager: TRegistryManager;
  ProjectRepos: TRepositoryList;
  Settings: TSettings;
  Repos: TRepositoryList;
begin
  ProjectRepos := TRepositoryList.Create;
  ProjectRepos.FreeObjects := True;
  ProjectRepos.Add(TRepositoryInfo.Create('project-repo', 'https://project.example.com/'));

  Settings := TSettings.Create;
  Settings.Repositories.Add(TRepositoryInfo.Create('settings-repo', 'https://settings.example.com/'));

  Manager := TRegistryManager.Create(ProjectRepos, Settings, 'https://env.example.com/');
  try
    Repos := Manager.GetEffectiveRepositories;
    try
      AssertEquals('Project repos take precedence', 1, Repos.Count);
      AssertEquals('Project repo id', 'project-repo', Repos[0].Id);
    finally
      Repos.Free;
    end;
  finally
    Manager.Free;
    Settings.Free;
    ProjectRepos.Free;
  end;
end;

procedure TTestRegistryManager.TestSettingsRepositoriesUsedWhenNoProject;
var
  Manager: TRegistryManager;
  Settings: TSettings;
  Repos: TRepositoryList;
begin
  Settings := TSettings.Create;
  Settings.Repositories.Add(TRepositoryInfo.Create('settings-repo', 'https://settings.example.com/'));

  Manager := TRegistryManager.Create(nil, Settings, '');
  try
    Repos := Manager.GetEffectiveRepositories;
    try
      AssertEquals('Settings repos used', 1, Repos.Count);
      AssertEquals('Settings repo id', 'settings-repo', Repos[0].Id);
    finally
      Repos.Free;
    end;
  finally
    Manager.Free;
    Settings.Free;
  end;
end;

procedure TTestRegistryManager.TestEnvVarUsedWhenNoProjectOrSettings;
var
  Manager: TRegistryManager;
  Settings: TSettings;
  Repos: TRepositoryList;
begin
  Settings := TSettings.Create;
  Manager := TRegistryManager.Create(nil, Settings, 'https://env.example.com/');
  try
    Repos := Manager.GetEffectiveRepositories;
    try
      AssertEquals('Env var creates one repo', 1, Repos.Count);
      AssertEquals('Env var id', 'env', Repos[0].Id);
      AssertEquals('Env var url', 'https://env.example.com/', Repos[0].Url);
    finally
      Repos.Free;
    end;
  finally
    Manager.Free;
    Settings.Free;
  end;
end;

procedure TTestRegistryManager.TestProjectOverridesEverything;
var
  Manager: TRegistryManager;
  ProjectRepos: TRepositoryList;
  Settings: TSettings;
  Repos: TRepositoryList;
begin
  ProjectRepos := TRepositoryList.Create;
  ProjectRepos.FreeObjects := True;
  ProjectRepos.Add(TRepositoryInfo.Create('custom', 'https://custom.example.com/'));

  Settings := TSettings.Create;
  Settings.Repositories.Add(TRepositoryInfo.Create('settings-repo', 'https://settings.example.com/'));

  Manager := TRegistryManager.Create(ProjectRepos, Settings, 'https://env.example.com/');
  try
    Repos := Manager.GetEffectiveRepositories;
    try
      AssertEquals('Project overrides all', 1, Repos.Count);
      AssertEquals('Custom repo', 'custom', Repos[0].Id);
    finally
      Repos.Free;
    end;
  finally
    Manager.Free;
    Settings.Free;
    ProjectRepos.Free;
  end;
end;

initialization
  RegisterTest(TTestNamePrefix);
  RegisterTest(TTestManifestUrl);
  RegisterTest(TTestRegistryManager);

end.
