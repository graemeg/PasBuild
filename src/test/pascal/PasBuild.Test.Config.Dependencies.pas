{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Config.Dependencies;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Types,
  PasBuild.Config;

type
  { Test parsing of <dependencies> section from project.xml }
  TTestConfigDependencies = class(TTestCase)
  private
    function GetFixturePath(const AFileName: string): string;
  published
    procedure TestParseNoDependencies;
    procedure TestParseSingleDependency;
    procedure TestParseMultipleDependencies;
    procedure TestParseMissingDepName;
    procedure TestParseMissingDepVersion;
    procedure TestParseDuplicateDependency;
    procedure TestParseInvalidDepNameCharacters;
  end;

implementation

{ TTestConfigDependencies }

function TTestConfigDependencies.GetFixturePath(const AFileName: string): string;
begin
  Result := 'fixtures/dependencies/' + AFileName;
end;

procedure TTestConfigDependencies.TestParseNoDependencies;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('no-dependencies.xml'));
  try
    AssertEquals('Dependencies list should be empty', 0, Config.Dependencies.Count);
  finally
    Config.Free;
  end;
end;

procedure TTestConfigDependencies.TestParseSingleDependency;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('single-dependency.xml'));
  try
    AssertEquals('Should have 1 dependency', 1, Config.Dependencies.Count);
    AssertEquals('Dependency name', 'fpgui-framework', Config.Dependencies[0].Name);
    AssertEquals('Dependency version', '1.0.0', Config.Dependencies[0].Version);
  finally
    Config.Free;
  end;
end;

procedure TTestConfigDependencies.TestParseMultipleDependencies;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('multiple-dependencies.xml'));
  try
    AssertEquals('Should have 2 dependencies', 2, Config.Dependencies.Count);
    AssertEquals('First dep name', 'fpgui-framework', Config.Dependencies[0].Name);
    AssertEquals('First dep version', '1.0.0', Config.Dependencies[0].Version);
    AssertEquals('Second dep name', 'my-utils', Config.Dependencies[1].Name);
    AssertEquals('Second dep version', '2.1.0', Config.Dependencies[1].Version);
  finally
    Config.Free;
  end;
end;

procedure TTestConfigDependencies.TestParseMissingDepName;
var
  Config: TProjectConfig;
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  Config := nil;
  try
    Config := TConfigLoader.LoadProjectXML(GetFixturePath('missing-dep-name.xml'));
  except
    on E: EProjectConfigError do
    begin
      ExceptionRaised := True;
      AssertTrue('Error should mention name',
        Pos('name', LowerCase(E.Message)) > 0);
    end;
  end;

  AssertTrue('Missing dep name should raise exception', ExceptionRaised);
  if Assigned(Config) then
    Config.Free;
end;

procedure TTestConfigDependencies.TestParseMissingDepVersion;
var
  Config: TProjectConfig;
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  Config := nil;
  try
    Config := TConfigLoader.LoadProjectXML(GetFixturePath('missing-dep-version.xml'));
  except
    on E: EProjectConfigError do
    begin
      ExceptionRaised := True;
      AssertTrue('Error should mention version',
        Pos('version', LowerCase(E.Message)) > 0);
    end;
  end;

  AssertTrue('Missing dep version should raise exception', ExceptionRaised);
  if Assigned(Config) then
    Config.Free;
end;

procedure TTestConfigDependencies.TestParseDuplicateDependency;
var
  Config: TProjectConfig;
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  Config := nil;
  try
    Config := TConfigLoader.LoadProjectXML(GetFixturePath('duplicate-dependency.xml'));
  except
    on E: EProjectConfigError do
    begin
      ExceptionRaised := True;
      AssertTrue('Error should mention duplicate',
        Pos('duplicate', LowerCase(E.Message)) > 0);
    end;
  end;

  AssertTrue('Duplicate dependency should raise exception', ExceptionRaised);
  if Assigned(Config) then
    Config.Free;
end;

procedure TTestConfigDependencies.TestParseInvalidDepNameCharacters;
var
  Config: TProjectConfig;
  ExceptionRaised: Boolean;
begin
  // A dependency name is used to build filesystem paths and registry URLs,
  // so path separators and traversal sequences must be rejected at parse time
  ExceptionRaised := False;
  Config := nil;
  try
    Config := TConfigLoader.LoadProjectXML(GetFixturePath('invalid-dep-name.xml'));
  except
    on E: EProjectConfigError do
    begin
      ExceptionRaised := True;
      AssertTrue('Error should mention the invalid name',
        Pos('evil-lib', E.Message) > 0);
    end;
  end;

  AssertTrue('Invalid dependency name should raise exception', ExceptionRaised);
  if Assigned(Config) then
    Config.Free;
end;

initialization
  RegisterTest(TTestConfigDependencies);

end.
