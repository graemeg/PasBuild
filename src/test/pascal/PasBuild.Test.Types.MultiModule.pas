{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Types.MultiModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Types;

type
  { Test TPackagingType enum and multi-module type support }
  TTestPackagingType = class(TTestCase)
  published
    procedure TestPackagingTypeEnumExists;
    procedure TestPackagingTypeValues;
  end;

  { Test TProjectConfig packaging property }
  TTestProjectConfigPackaging = class(TTestCase)
  private
    FConfig: TProjectConfig;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPackagingDefault;
    procedure TestPackagingSetLibrary;
    procedure TestPackagingSetPom;
    procedure TestPackagingSetApplication;
  end;

  { Test TProjectConfig modules support }
  TTestProjectConfigModules = class(TTestCase)
  private
    FConfig: TProjectConfig;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestModulesListExists;
    procedure TestModulesListEmpty;
    procedure TestModulesListAddSingle;
    procedure TestModulesListAddMultiple;
    procedure TestModuleDependenciesListExists;
  end;

implementation

{ TTestPackagingType }

procedure TTestPackagingType.TestPackagingTypeEnumExists;
var
  PackagingType: TProjectType;
begin
  // Test that we can declare a variable of TProjectType
  // This will fail to compile if the type doesn't exist
  PackagingType := ptApplication;
  AssertTrue('TProjectType enum should exist', True);
end;

procedure TTestPackagingType.TestPackagingTypeValues;
var
  PackagingType: TProjectType;
begin
  // Test that all three packaging types exist
  PackagingType := ptApplication;
  AssertTrue('ptApplication should be valid', Ord(PackagingType) >= 0);

  PackagingType := ptLibrary;
  AssertTrue('ptLibrary should be valid', Ord(PackagingType) >= 0);

  PackagingType := ptPom;
  AssertTrue('ptPom should be valid', Ord(PackagingType) >= 0);

  // Test that they have different values
  AssertFalse('ptApplication and ptLibrary should be different',
    Ord(ptApplication) = Ord(ptLibrary));
  AssertFalse('ptApplication and ptPom should be different',
    Ord(ptApplication) = Ord(ptPom));
  AssertFalse('ptLibrary and ptPom should be different',
    Ord(ptLibrary) = Ord(ptPom));
end;

{ TTestProjectConfigPackaging }

procedure TTestProjectConfigPackaging.SetUp;
begin
  FConfig := TProjectConfig.Create;
end;

procedure TTestProjectConfigPackaging.TearDown;
begin
  FConfig.Free;
end;

procedure TTestProjectConfigPackaging.TestPackagingDefault;
begin
  // Default packaging should be ptApplication for backward compatibility
  AssertEquals('Default packaging should be ptApplication',
    Ord(ptApplication), Ord(FConfig.BuildConfig.ProjectType));
end;

procedure TTestProjectConfigPackaging.TestPackagingSetLibrary;
begin
  FConfig.BuildConfig.ProjectType := ptLibrary;
  AssertEquals('Packaging should be set to ptLibrary',
    Ord(ptLibrary), Ord(FConfig.BuildConfig.ProjectType));
end;

procedure TTestProjectConfigPackaging.TestPackagingSetPom;
begin
  FConfig.BuildConfig.ProjectType := ptPom;
  AssertEquals('Packaging should be set to ptPom',
    Ord(ptPom), Ord(FConfig.BuildConfig.ProjectType));
end;

procedure TTestProjectConfigPackaging.TestPackagingSetApplication;
begin
  FConfig.BuildConfig.ProjectType := ptApplication;
  AssertEquals('Packaging should be set to ptApplication',
    Ord(ptApplication), Ord(FConfig.BuildConfig.ProjectType));
end;

{ TTestProjectConfigModules }

procedure TTestProjectConfigModules.SetUp;
begin
  FConfig := TProjectConfig.Create;
end;

procedure TTestProjectConfigModules.TearDown;
begin
  FConfig.Free;
end;

procedure TTestProjectConfigModules.TestModulesListExists;
begin
  AssertNotNull('Modules list should exist', FConfig.Modules);
end;

procedure TTestProjectConfigModules.TestModulesListEmpty;
begin
  AssertEquals('Modules list should be empty by default', 0, FConfig.Modules.Count);
end;

procedure TTestProjectConfigModules.TestModulesListAddSingle;
begin
  FConfig.Modules.Add('framework-core');
  AssertEquals('Modules list should have 1 item', 1, FConfig.Modules.Count);
  AssertEquals('Module name should match', 'framework-core', FConfig.Modules[0]);
end;

procedure TTestProjectConfigModules.TestModulesListAddMultiple;
begin
  FConfig.Modules.Add('framework-core');
  FConfig.Modules.Add('framework-gui');
  FConfig.Modules.Add('examples/demo');

  AssertEquals('Modules list should have 3 items', 3, FConfig.Modules.Count);
  AssertEquals('First module should match', 'framework-core', FConfig.Modules[0]);
  AssertEquals('Second module should match', 'framework-gui', FConfig.Modules[1]);
  AssertEquals('Third module should match', 'examples/demo', FConfig.Modules[2]);
end;

procedure TTestProjectConfigModules.TestModuleDependenciesListExists;
begin
  AssertNotNull('Module dependencies list should exist', FConfig.ModuleDependencies);
end;

initialization
  RegisterTest(TTestPackagingType);
  RegisterTest(TTestProjectConfigPackaging);
  RegisterTest(TTestProjectConfigModules);

end.
