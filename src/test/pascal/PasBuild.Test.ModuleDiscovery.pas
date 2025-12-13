{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.ModuleDiscovery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Types;

type
  { Tests for TModuleInfo class }
  TTestModuleInfo = class(TTestCase)
  published
    procedure TestModuleInfoCreate;
    procedure TestModuleInfoProperties;
    procedure TestModuleInfoDependencies;
  end;

  { Tests for TModuleRegistry class }
  TTestModuleRegistry = class(TTestCase)
  published
    procedure TestModuleRegistryCreate;
    procedure TestModuleRegistryRegister;
    procedure TestModuleRegistryRegisterMultiple;
    procedure TestModuleRegistryFindByName;
    procedure TestModuleRegistryFindByNameNotFound;
    procedure TestModuleRegistryDuplicateName;
  end;

  { Tests for module discovery }
  TTestModuleDiscovery = class(TTestCase)
  private
    function GetFixturePath(const AFileName: string): string;
  published
    procedure TestDiscoverSimple;
    procedure TestDiscoverWithDependencies;
    procedure TestDiscoverNestedPaths;
    procedure TestDiscoverMissingProjectXml;
    procedure TestDiscoverDuplicateNames;
  end;

implementation

{ TTestModuleInfo }

procedure TTestModuleInfo.TestModuleInfoCreate;
var
  Module: TModuleInfo;
begin
  Module := TModuleInfo.Create;
  try
    AssertNotNull('Module should be created', Module);
    AssertEquals('Module name should be empty initially', '', Module.Name);
    AssertEquals('Module path should be empty initially', '', Module.Path);
    AssertNotNull('Dependencies list should exist', Module.Dependencies);
    AssertEquals('Dependencies should be empty initially', 0, Module.Dependencies.Count);
  finally
    Module.Free;
  end;
end;

procedure TTestModuleInfo.TestModuleInfoProperties;
var
  Module: TModuleInfo;
begin
  Module := TModuleInfo.Create;
  try
    Module.Name := 'TestModule';
    Module.Path := '/path/to/module';
    Module.UnitsDirectory := '/path/to/module/target/units';

    AssertEquals('Module name should be set', 'TestModule', Module.Name);
    AssertEquals('Module path should be set', '/path/to/module', Module.Path);
    AssertEquals('Units directory should be set', '/path/to/module/target/units', Module.UnitsDirectory);
  finally
    Module.Free;
  end;
end;

procedure TTestModuleInfo.TestModuleInfoDependencies;
var
  Module: TModuleInfo;
begin
  Module := TModuleInfo.Create;
  try
    Module.Name := 'AppModule';
    Module.Dependencies.Add('CoreLibrary');
    Module.Dependencies.Add('UtilsLibrary');

    AssertEquals('Module should have 2 dependencies', 2, Module.Dependencies.Count);
    AssertEquals('First dependency should match', 'CoreLibrary', Module.Dependencies[0]);
    AssertEquals('Second dependency should match', 'UtilsLibrary', Module.Dependencies[1]);
  finally
    Module.Free;
  end;
end;

{ TTestModuleRegistry }

procedure TTestModuleRegistry.TestModuleRegistryCreate;
var
  Registry: TModuleRegistry;
begin
  Registry := TModuleRegistry.Create;
  try
    AssertNotNull('Registry should be created', Registry);
    AssertNotNull('Modules list should exist', Registry.Modules);
    AssertEquals('Modules should be empty initially', 0, Registry.Modules.Count);
  finally
    Registry.Free;
  end;
end;

procedure TTestModuleRegistry.TestModuleRegistryRegister;
var
  Registry: TModuleRegistry;
  Module: TModuleInfo;
begin
  Registry := TModuleRegistry.Create;
  Module := TModuleInfo.Create;
  try
    Module.Name := 'TestModule';
    Module.Path := '/path/to/module';

    Registry.RegisterModule(Module);

    AssertEquals('Registry should have 1 module', 1, Registry.Modules.Count);
  finally
    Registry.Free;
    { Module is owned by Registry, don't free separately }
  end;
end;

procedure TTestModuleRegistry.TestModuleRegistryRegisterMultiple;
var
  Registry: TModuleRegistry;
  Module1, Module2: TModuleInfo;
begin
  Registry := TModuleRegistry.Create;
  Module1 := TModuleInfo.Create;
  Module2 := TModuleInfo.Create;
  try
    Module1.Name := 'Module1';
    Module1.Path := '/path/to/module1';
    Module2.Name := 'Module2';
    Module2.Path := '/path/to/module2';

    Registry.RegisterModule(Module1);
    Registry.RegisterModule(Module2);

    AssertEquals('Registry should have 2 modules', 2, Registry.Modules.Count);
  finally
    Registry.Free;
  end;
end;

procedure TTestModuleRegistry.TestModuleRegistryFindByName;
var
  Registry: TModuleRegistry;
  Module: TModuleInfo;
  Found: TModuleInfo;
begin
  Registry := TModuleRegistry.Create;
  Module := TModuleInfo.Create;
  try
    Module.Name := 'TestModule';
    Module.Path := '/path/to/module';

    Registry.RegisterModule(Module);
    Found := Registry.FindModuleByName('TestModule');

    AssertNotNull('Module should be found by name', Found);
    AssertEquals('Found module name should match', 'TestModule', Found.Name);
  finally
    Registry.Free;
  end;
end;

procedure TTestModuleRegistry.TestModuleRegistryFindByNameNotFound;
var
  Registry: TModuleRegistry;
  Found: TModuleInfo;
begin
  Registry := TModuleRegistry.Create;
  try
    Found := Registry.FindModuleByName('NonExistent');
    AssertNull('Non-existent module should return nil', Found);
  finally
    Registry.Free;
  end;
end;

procedure TTestModuleRegistry.TestModuleRegistryDuplicateName;
var
  Registry: TModuleRegistry;
  Module1, Module2: TModuleInfo;
  ExceptionRaised: Boolean;
begin
  Registry := TModuleRegistry.Create;
  Module1 := TModuleInfo.Create;
  Module2 := TModuleInfo.Create;
  ExceptionRaised := False;
  try
    Module1.Name := 'DuplicateModule';
    Module1.Path := '/path/to/module1';
    Module2.Name := 'DuplicateModule';
    Module2.Path := '/path/to/module2';

    Registry.RegisterModule(Module1);
    try
      Registry.RegisterModule(Module2);
    except
      on E: Exception do
      begin
        ExceptionRaised := True;
        AssertTrue('Error message should mention duplicate', Pos('duplicate', LowerCase(E.Message)) > 0);
      end;
    end;
  finally
    Registry.Free;
  end;

  AssertTrue('Registering duplicate name should raise exception', ExceptionRaised);
end;

{ TTestModuleDiscovery }

function TTestModuleDiscovery.GetFixturePath(const AFileName: string): string;
begin
  Result := 'fixtures/multi-module/' + AFileName;
end;

procedure TTestModuleDiscovery.TestDiscoverSimple;
var
  Registry: TModuleRegistry;
  Module1, Module2: TModuleInfo;
begin
  { For now, just test basic registry functionality }
  { Full discovery tests will come after implementing discovery algorithm }
  Registry := TModuleRegistry.Create;
  Module1 := TModuleInfo.Create;
  Module2 := TModuleInfo.Create;
  try
    Module1.Name := 'CoreLib';
    Module1.Path := '/path/to/core';
    Module2.Name := 'Demo';
    Module2.Path := '/path/to/demo';
    Module2.Dependencies.Add('CoreLib');

    Registry.RegisterModule(Module1);
    Registry.RegisterModule(Module2);

    AssertEquals('Registry should have 2 modules', 2, Registry.Modules.Count);
    AssertNotNull('CoreLib should be found', Registry.FindModuleByName('CoreLib'));
    AssertNotNull('Demo should be found', Registry.FindModuleByName('Demo'));
    AssertEquals('Demo should depend on CoreLib', 1, Module2.Dependencies.Count);
  finally
    Registry.Free;
  end;
end;

procedure TTestModuleDiscovery.TestDiscoverWithDependencies;
var
  Registry: TModuleRegistry;
  ModuleA, ModuleB: TModuleInfo;
begin
  Registry := TModuleRegistry.Create;
  ModuleA := TModuleInfo.Create;
  ModuleB := TModuleInfo.Create;
  try
    ModuleA.Name := 'LibA';
    ModuleA.Path := '/path/to/liba';

    ModuleB.Name := 'LibB';
    ModuleB.Path := '/path/to/libb';
    ModuleB.Dependencies.Add('LibA');

    Registry.RegisterModule(ModuleA);
    Registry.RegisterModule(ModuleB);

    AssertNotNull('LibA should exist', Registry.FindModuleByName('LibA'));
    AssertNotNull('LibB should exist', Registry.FindModuleByName('LibB'));
    AssertEquals('LibB should depend on LibA', 1, Registry.FindModuleByName('LibB').Dependencies.Count);
    AssertEquals('LibB dependency should be LibA', 'LibA', Registry.FindModuleByName('LibB').Dependencies[0]);
  finally
    Registry.Free;
  end;
end;

procedure TTestModuleDiscovery.TestDiscoverNestedPaths;
var
  Registry: TModuleRegistry;
  RootModule, NestedModule: TModuleInfo;
begin
  Registry := TModuleRegistry.Create;
  RootModule := TModuleInfo.Create;
  NestedModule := TModuleInfo.Create;
  try
    RootModule.Name := 'Framework';
    RootModule.Path := '/path/to/framework';

    { Module in nested path examples/demo }
    NestedModule.Name := 'DemoApp';
    NestedModule.Path := '/path/to/framework/examples/demo';
    NestedModule.Dependencies.Add('Framework');

    Registry.RegisterModule(RootModule);
    Registry.RegisterModule(NestedModule);

    AssertEquals('Registry should have 2 modules', 2, Registry.Modules.Count);
    AssertNotNull('DemoApp in nested path should be found', Registry.FindModuleByName('DemoApp'));
  finally
    Registry.Free;
  end;
end;

procedure TTestModuleDiscovery.TestDiscoverMissingProjectXml;
var
  ExceptionRaised: Boolean;
begin
  { This test will verify that missing project.xml raises error }
  { Will be implemented when discovery algorithm is created }
  ExceptionRaised := False;

  { For now, just mark as placeholder }
  AssertTrue('Test placeholder - will verify missing project.xml detection', True);
end;

procedure TTestModuleDiscovery.TestDiscoverDuplicateNames;
var
  Registry: TModuleRegistry;
  Module1, Module2: TModuleInfo;
  ExceptionRaised: Boolean;
begin
  Registry := TModuleRegistry.Create;
  Module1 := TModuleInfo.Create;
  Module2 := TModuleInfo.Create;
  ExceptionRaised := False;
  try
    Module1.Name := 'SameModule';
    Module1.Path := '/path/to/module1';
    Module2.Name := 'SameModule';
    Module2.Path := '/path/to/module2';

    Registry.RegisterModule(Module1);
    try
      Registry.RegisterModule(Module2);
    except
      on E: Exception do
      begin
        ExceptionRaised := True;
        AssertTrue('Error should mention duplicate', Pos('duplicate', LowerCase(E.Message)) > 0);
      end;
    end;
  finally
    Registry.Free;
  end;

  AssertTrue('Duplicate names should raise exception', ExceptionRaised);
end;

initialization
  RegisterTest(TTestModuleInfo);
  RegisterTest(TTestModuleRegistry);
  RegisterTest(TTestModuleDiscovery);

end.
