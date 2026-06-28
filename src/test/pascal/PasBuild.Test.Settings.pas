{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Types,
  PasBuild.Settings;

type
  TTestSettingsLoader = class(TTestCase)
  private
    FTempDir: string;
    function CreateTempFile(const AContent: string): string;
    procedure CleanupTempDir;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadMissingFileReturnsDefaults;
    procedure TestLoadEmptySettingsElement;
    procedure TestLoadRepositoriesSingle;
    procedure TestLoadRepositoriesMultiple;
    procedure TestLoadRepositoryOrder;
    procedure TestLoadOfflineModeTrue;
    procedure TestLoadOfflineModeFalse;
    procedure TestLoadOfflineModeDefault;
    procedure TestLoadHttpConnectTimeout;
    procedure TestLoadHttpReadTimeout;
    procedure TestLoadHttpTimeoutDefaults;
    procedure TestLoadFullSettings;
    procedure TestLoadMalformedXmlRaisesException;
    procedure TestGetDefaultPathContainsPasbuild;
  end;

implementation

{ TTestSettingsLoader }

procedure TTestSettingsLoader.SetUp;
begin
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'pasbuild-test-settings-' + IntToStr(GetProcessID);
  ForceDirectories(FTempDir);
end;

procedure TTestSettingsLoader.TearDown;
begin
  CleanupTempDir;
end;

function TTestSettingsLoader.CreateTempFile(const AContent: string): string;
var
  F: TextFile;
begin
  Result := IncludeTrailingPathDelimiter(FTempDir) + 'settings.xml';
  AssignFile(F, Result);
  Rewrite(F);
  Write(F, AContent);
  CloseFile(F);
end;

procedure TTestSettingsLoader.CleanupTempDir;
var
  SearchRec: TSearchRec;
  FullPath: string;
begin
  if not DirectoryExists(FTempDir) then
    Exit;
  if FindFirst(IncludeTrailingPathDelimiter(FTempDir) + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          FullPath := IncludeTrailingPathDelimiter(FTempDir) + SearchRec.Name;
          DeleteFile(FullPath);
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
  RemoveDir(FTempDir);
end;

procedure TTestSettingsLoader.TestLoadMissingFileReturnsDefaults;
var
  Settings: TSettings;
begin
  Settings := TSettingsLoader.LoadSettings('/nonexistent/path/settings.xml');
  try
    AssertEquals('No repositories', 0, Settings.Repositories.Count);
    AssertFalse('Offline mode off', Settings.OfflineMode);
    AssertEquals('Default connect timeout', 30, Settings.ConnectTimeout);
    AssertEquals('Default read timeout', 60, Settings.ReadTimeout);
  finally
    Settings.Free;
  end;
end;

procedure TTestSettingsLoader.TestLoadEmptySettingsElement;
var
  Settings: TSettings;
  Path: string;
begin
  Path := CreateTempFile(
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<settings>' + LineEnding +
    '</settings>');
  Settings := TSettingsLoader.LoadSettings(Path);
  try
    AssertEquals('No repositories', 0, Settings.Repositories.Count);
    AssertFalse('Offline mode off', Settings.OfflineMode);
  finally
    Settings.Free;
  end;
end;

procedure TTestSettingsLoader.TestLoadRepositoriesSingle;
var
  Settings: TSettings;
  Path: string;
begin
  Path := CreateTempFile(
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<settings>' + LineEnding +
    '  <repositories>' + LineEnding +
    '    <repository>' + LineEnding +
    '      <id>central</id>' + LineEnding +
    '      <url>https://example.com/registry/</url>' + LineEnding +
    '    </repository>' + LineEnding +
    '  </repositories>' + LineEnding +
    '</settings>');
  Settings := TSettingsLoader.LoadSettings(Path);
  try
    AssertEquals('One repository', 1, Settings.Repositories.Count);
    AssertEquals('Repository id', 'central', Settings.Repositories[0].Id);
    AssertEquals('Repository url', 'https://example.com/registry/',
      Settings.Repositories[0].Url);
  finally
    Settings.Free;
  end;
end;

procedure TTestSettingsLoader.TestLoadRepositoriesMultiple;
var
  Settings: TSettings;
  Path: string;
begin
  Path := CreateTempFile(
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<settings>' + LineEnding +
    '  <repositories>' + LineEnding +
    '    <repository>' + LineEnding +
    '      <id>internal</id>' + LineEnding +
    '      <url>https://git.company.com/registry/</url>' + LineEnding +
    '    </repository>' + LineEnding +
    '    <repository>' + LineEnding +
    '      <id>central</id>' + LineEnding +
    '      <url>https://github.com/registry/</url>' + LineEnding +
    '    </repository>' + LineEnding +
    '  </repositories>' + LineEnding +
    '</settings>');
  Settings := TSettingsLoader.LoadSettings(Path);
  try
    AssertEquals('Two repositories', 2, Settings.Repositories.Count);
  finally
    Settings.Free;
  end;
end;

procedure TTestSettingsLoader.TestLoadRepositoryOrder;
var
  Settings: TSettings;
  Path: string;
begin
  Path := CreateTempFile(
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<settings>' + LineEnding +
    '  <repositories>' + LineEnding +
    '    <repository>' + LineEnding +
    '      <id>first</id>' + LineEnding +
    '      <url>https://first.example.com/</url>' + LineEnding +
    '    </repository>' + LineEnding +
    '    <repository>' + LineEnding +
    '      <id>second</id>' + LineEnding +
    '      <url>https://second.example.com/</url>' + LineEnding +
    '    </repository>' + LineEnding +
    '  </repositories>' + LineEnding +
    '</settings>');
  Settings := TSettingsLoader.LoadSettings(Path);
  try
    AssertEquals('First repository', 'first', Settings.Repositories[0].Id);
    AssertEquals('Second repository', 'second', Settings.Repositories[1].Id);
  finally
    Settings.Free;
  end;
end;

procedure TTestSettingsLoader.TestLoadOfflineModeTrue;
var
  Settings: TSettings;
  Path: string;
begin
  Path := CreateTempFile(
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<settings>' + LineEnding +
    '  <offlineMode>true</offlineMode>' + LineEnding +
    '</settings>');
  Settings := TSettingsLoader.LoadSettings(Path);
  try
    AssertTrue('Offline mode on', Settings.OfflineMode);
  finally
    Settings.Free;
  end;
end;

procedure TTestSettingsLoader.TestLoadOfflineModeFalse;
var
  Settings: TSettings;
  Path: string;
begin
  Path := CreateTempFile(
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<settings>' + LineEnding +
    '  <offlineMode>false</offlineMode>' + LineEnding +
    '</settings>');
  Settings := TSettingsLoader.LoadSettings(Path);
  try
    AssertFalse('Offline mode off', Settings.OfflineMode);
  finally
    Settings.Free;
  end;
end;

procedure TTestSettingsLoader.TestLoadOfflineModeDefault;
var
  Settings: TSettings;
  Path: string;
begin
  Path := CreateTempFile(
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<settings>' + LineEnding +
    '</settings>');
  Settings := TSettingsLoader.LoadSettings(Path);
  try
    AssertFalse('Default offline mode is false', Settings.OfflineMode);
  finally
    Settings.Free;
  end;
end;

procedure TTestSettingsLoader.TestLoadHttpConnectTimeout;
var
  Settings: TSettings;
  Path: string;
begin
  Path := CreateTempFile(
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<settings>' + LineEnding +
    '  <http>' + LineEnding +
    '    <connectTimeout>15</connectTimeout>' + LineEnding +
    '  </http>' + LineEnding +
    '</settings>');
  Settings := TSettingsLoader.LoadSettings(Path);
  try
    AssertEquals('Custom connect timeout', 15, Settings.ConnectTimeout);
    AssertEquals('Default read timeout preserved', 60, Settings.ReadTimeout);
  finally
    Settings.Free;
  end;
end;

procedure TTestSettingsLoader.TestLoadHttpReadTimeout;
var
  Settings: TSettings;
  Path: string;
begin
  Path := CreateTempFile(
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<settings>' + LineEnding +
    '  <http>' + LineEnding +
    '    <readTimeout>120</readTimeout>' + LineEnding +
    '  </http>' + LineEnding +
    '</settings>');
  Settings := TSettingsLoader.LoadSettings(Path);
  try
    AssertEquals('Default connect timeout preserved', 30, Settings.ConnectTimeout);
    AssertEquals('Custom read timeout', 120, Settings.ReadTimeout);
  finally
    Settings.Free;
  end;
end;

procedure TTestSettingsLoader.TestLoadHttpTimeoutDefaults;
var
  Settings: TSettings;
  Path: string;
begin
  Path := CreateTempFile(
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<settings>' + LineEnding +
    '</settings>');
  Settings := TSettingsLoader.LoadSettings(Path);
  try
    AssertEquals('Default connect timeout', 30, Settings.ConnectTimeout);
    AssertEquals('Default read timeout', 60, Settings.ReadTimeout);
  finally
    Settings.Free;
  end;
end;

procedure TTestSettingsLoader.TestLoadFullSettings;
var
  Settings: TSettings;
  Path: string;
begin
  Path := CreateTempFile(
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<settings>' + LineEnding +
    '  <repositories>' + LineEnding +
    '    <repository>' + LineEnding +
    '      <id>central</id>' + LineEnding +
    '      <url>https://example.com/registry/</url>' + LineEnding +
    '    </repository>' + LineEnding +
    '  </repositories>' + LineEnding +
    '  <offlineMode>true</offlineMode>' + LineEnding +
    '  <http>' + LineEnding +
    '    <connectTimeout>10</connectTimeout>' + LineEnding +
    '    <readTimeout>90</readTimeout>' + LineEnding +
    '  </http>' + LineEnding +
    '</settings>');
  Settings := TSettingsLoader.LoadSettings(Path);
  try
    AssertEquals('Repository count', 1, Settings.Repositories.Count);
    AssertTrue('Offline mode', Settings.OfflineMode);
    AssertEquals('Connect timeout', 10, Settings.ConnectTimeout);
    AssertEquals('Read timeout', 90, Settings.ReadTimeout);
  finally
    Settings.Free;
  end;
end;

procedure TTestSettingsLoader.TestLoadMalformedXmlRaisesException;
var
  Path: string;
  ExceptionRaised: Boolean;
begin
  Path := CreateTempFile('not valid xml <><>');
  ExceptionRaised := False;
  try
    TSettingsLoader.LoadSettings(Path).Free;
  except
    ExceptionRaised := True;
  end;
  AssertTrue('Malformed XML should raise exception', ExceptionRaised);
end;

procedure TTestSettingsLoader.TestGetDefaultPathContainsPasbuild;
var
  Path: string;
begin
  Path := TSettingsLoader.GetDefaultPath;
  AssertTrue('Path contains .pasbuild',
    Pos('.pasbuild', Path) > 0);
  AssertTrue('Path ends with settings.xml',
    Copy(Path, Length(Path) - 11, 12) = 'settings.xml');
end;

initialization
  RegisterTest(TTestSettingsLoader);

end.
