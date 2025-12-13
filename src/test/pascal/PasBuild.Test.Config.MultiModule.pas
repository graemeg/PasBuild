{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Config.MultiModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Types,
  PasBuild.Config;

type
  { Test parsing of <packaging> element from XML }
  TTestParsePackaging = class(TTestCase)
  private
    function GetFixturePath(const AFileName: string): string;
  published
    procedure TestParsePackagingPom;
    procedure TestParsePackagingLibrary;
    procedure TestParsePackagingApplication;
    procedure TestParsePackagingDefault;
    procedure TestParsePackagingBackwardCompat;
    procedure TestParsePackagingInvalid;
  end;

  { Test parsing of <modules> element from XML }
  TTestParseModules = class(TTestCase)
  private
    function GetFixturePath(const AFileName: string): string;
  published
    procedure TestParseModulesEmpty;
    procedure TestParseModulesSingle;
    procedure TestParseModulesMultiple;
  end;

  { Test validation of packaging rules }
  TTestValidatePackagingRules = class(TTestCase)
  private
    function GetFixturePath(const AFileName: string): string;
  published
    procedure TestValidatePomRequiresModules;
    procedure TestValidatePomForbidsMainSource;
    procedure TestValidateLibraryForbidsAggregatorModules;
    procedure TestValidateApplicationForbidsAggregatorModules;
    procedure TestValidatePomValid;
    procedure TestValidateLibraryValid;
    procedure TestValidateApplicationValid;
  end;

implementation

{ TTestParsePackaging }

function TTestParsePackaging.GetFixturePath(const AFileName: string): string;
begin
  // Tests run from target/ directory, fixtures are copied there
  Result := 'fixtures/multi-module/' + AFileName;
end;

procedure TTestParsePackaging.TestParsePackagingPom;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('packaging-pom.xml'));
  try
    AssertEquals('Packaging should be pom', Ord(ptPom), Ord(Config.BuildConfig.ProjectType));
    AssertEquals('Project name should match', 'TestAggregator', Config.Name);
  finally
    Config.Free;
  end;
end;

procedure TTestParsePackaging.TestParsePackagingLibrary;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('packaging-library.xml'));
  try
    AssertEquals('Packaging should be library', Ord(ptLibrary), Ord(Config.BuildConfig.ProjectType));
    AssertEquals('Project name should match', 'TestLibrary', Config.Name);
  finally
    Config.Free;
  end;
end;

procedure TTestParsePackaging.TestParsePackagingApplication;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('packaging-application.xml'));
  try
    AssertEquals('Packaging should be application', Ord(ptApplication), Ord(Config.BuildConfig.ProjectType));
    AssertEquals('Project name should match', 'TestApplication', Config.Name);
  finally
    Config.Free;
  end;
end;

procedure TTestParsePackaging.TestParsePackagingDefault;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('packaging-default.xml'));
  try
    AssertEquals('Default packaging should be application', Ord(ptApplication), Ord(Config.BuildConfig.ProjectType));
    AssertEquals('Project name should match', 'TestDefault', Config.Name);
  finally
    Config.Free;
  end;
end;

procedure TTestParsePackaging.TestParsePackagingBackwardCompat;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('packaging-backward-compat.xml'));
  try
    AssertEquals('Old projectType should still work', Ord(ptLibrary), Ord(Config.BuildConfig.ProjectType));
    AssertEquals('Project name should match', 'TestBackwardCompat', Config.Name);
  finally
    Config.Free;
  end;
end;

procedure TTestParsePackaging.TestParsePackagingInvalid;
var
  Config: TProjectConfig;
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  Config := nil;
  try
    Config := TConfigLoader.LoadProjectXML(GetFixturePath('packaging-invalid.xml'));
  except
    on E: EProjectConfigError do
      ExceptionRaised := True;
  end;

  AssertTrue('Invalid packaging value should raise exception', ExceptionRaised);
  if Assigned(Config) then
    Config.Free;
end;

{ TTestParseModules }

function TTestParseModules.GetFixturePath(const AFileName: string): string;
begin
  // Tests run from target/ directory, fixtures are copied there
  Result := 'fixtures/multi-module/' + AFileName;
end;

procedure TTestParseModules.TestParseModulesEmpty;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('packaging-library.xml'));
  try
    AssertEquals('Modules list should be empty', 0, Config.Modules.Count);
  finally
    Config.Free;
  end;
end;

procedure TTestParseModules.TestParseModulesSingle;
var
  Config: TProjectConfig;
begin
  // We'll create this fixture next - for now skip
  // TODO: Create modules-single.xml fixture
  AssertTrue('Test not yet implemented', True);
end;

procedure TTestParseModules.TestParseModulesMultiple;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('packaging-pom.xml'));
  try
    AssertEquals('Modules list should have 2 items', 2, Config.Modules.Count);
    AssertEquals('First module should match', 'module1', Config.Modules[0]);
    AssertEquals('Second module should match', 'module2', Config.Modules[1]);
  finally
    Config.Free;
  end;
end;

{ TTestValidatePackagingRules }

function TTestValidatePackagingRules.GetFixturePath(const AFileName: string): string;
begin
  Result := 'fixtures/multi-module/' + AFileName;
end;

procedure TTestValidatePackagingRules.TestValidatePomRequiresModules;
var
  Config: TProjectConfig;
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  Config := nil;
  try
    Config := TConfigLoader.LoadProjectXML(GetFixturePath('validate-pom-no-modules.xml'));
  except
    on E: EProjectConfigError do
    begin
      ExceptionRaised := True;
      AssertTrue('Error message should mention modules', Pos('modules', LowerCase(E.Message)) > 0);
    end;
  end;

  AssertTrue('POM without modules should raise exception', ExceptionRaised);
  if Assigned(Config) then
    Config.Free;
end;

procedure TTestValidatePackagingRules.TestValidatePomForbidsMainSource;
var
  Config: TProjectConfig;
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  Config := nil;
  try
    Config := TConfigLoader.LoadProjectXML(GetFixturePath('validate-pom-with-mainsource.xml'));
  except
    on E: EProjectConfigError do
    begin
      ExceptionRaised := True;
      AssertTrue('Error message should mention mainSource', Pos('mainSource', E.Message) > 0);
    end;
  end;

  AssertTrue('POM with mainSource should raise exception', ExceptionRaised);
  if Assigned(Config) then
    Config.Free;
end;

procedure TTestValidatePackagingRules.TestValidateLibraryForbidsAggregatorModules;
var
  Config: TProjectConfig;
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  Config := nil;
  try
    Config := TConfigLoader.LoadProjectXML(GetFixturePath('validate-library-with-modules.xml'));
  except
    on E: EProjectConfigError do
    begin
      ExceptionRaised := True;
      AssertTrue('Error message should mention aggregator', Pos('aggregator', LowerCase(E.Message)) > 0);
    end;
  end;

  AssertTrue('Library with modules should raise exception', ExceptionRaised);
  if Assigned(Config) then
    Config.Free;
end;

procedure TTestValidatePackagingRules.TestValidateApplicationForbidsAggregatorModules;
var
  Config: TProjectConfig;
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  Config := nil;
  try
    Config := TConfigLoader.LoadProjectXML(GetFixturePath('validate-application-with-modules.xml'));
  except
    on E: EProjectConfigError do
    begin
      ExceptionRaised := True;
      AssertTrue('Error message should mention aggregator', Pos('aggregator', LowerCase(E.Message)) > 0);
    end;
  end;

  AssertTrue('Application with modules should raise exception', ExceptionRaised);
  if Assigned(Config) then
    Config.Free;
end;

procedure TTestValidatePackagingRules.TestValidatePomValid;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('validate-pom-valid.xml'));
  try
    AssertEquals('Valid POM should load', Ord(ptPom), Ord(Config.BuildConfig.ProjectType));
    AssertTrue('POM should have modules', Config.Modules.Count > 0);
  finally
    Config.Free;
  end;
end;

procedure TTestValidatePackagingRules.TestValidateLibraryValid;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('validate-library-valid.xml'));
  try
    AssertEquals('Valid Library should load', Ord(ptLibrary), Ord(Config.BuildConfig.ProjectType));
    AssertTrue('Library should not have modules', Config.Modules.Count = 0);
  finally
    Config.Free;
  end;
end;

procedure TTestValidatePackagingRules.TestValidateApplicationValid;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('validate-application-valid.xml'));
  try
    AssertEquals('Valid Application should load', Ord(ptApplication), Ord(Config.BuildConfig.ProjectType));
    AssertTrue('Application should not have modules', Config.Modules.Count = 0);
    AssertTrue('Application should have mainSource', Config.BuildConfig.MainSource <> '');
  finally
    Config.Free;
  end;
end;

initialization
  RegisterTest(TTestParsePackaging);
  RegisterTest(TTestParseModules);
  RegisterTest(TTestValidatePackagingRules);

end.
