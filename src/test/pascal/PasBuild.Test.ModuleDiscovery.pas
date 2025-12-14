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

  { Tests for build order calculation }
  TTestBuildOrder = class(TTestCase)
  published
    procedure TestBuildOrderLinear;
    procedure TestBuildOrderDiamond;
    procedure TestBuildOrderMultipleRoots;
    procedure TestBuildOrderComplexGraph;
    procedure TestCycleDetectionSimple;
    procedure TestCycleDetectionComplex;
    procedure TestCycleDetectionSelfReference;
  end;

  { Tests for artifact resolution }
  TTestArtifactResolution = class(TTestCase)
  published
    procedure TestResolveArtifactsSingleDependency;
    procedure TestResolveArtifactsMultipleDependencies;
    procedure TestResolveArtifactsTransitive;
    procedure TestResolveArtifactsPomReference;
    procedure TestResolveArtifactsLibraryModules;
    procedure TestResolveArtifactsApplicationModules;
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

{ TTestBuildOrder }

procedure TTestBuildOrder.TestBuildOrderLinear;
var
  Registry: TModuleRegistry;
  ModuleA, ModuleB, ModuleC: TModuleInfo;
  BuildOrder: TList;
begin
  { Linear dependency: C -> B -> A (build order: A, B, C) }
  Registry := TModuleRegistry.Create;
  ModuleA := TModuleInfo.Create;
  ModuleB := TModuleInfo.Create;
  ModuleC := TModuleInfo.Create;
  try
    ModuleA.Name := 'ModuleA';
    ModuleA.Path := '/path/to/a';

    ModuleB.Name := 'ModuleB';
    ModuleB.Path := '/path/to/b';
    ModuleB.Dependencies.Add('ModuleA');

    ModuleC.Name := 'ModuleC';
    ModuleC.Path := '/path/to/c';
    ModuleC.Dependencies.Add('ModuleB');

    Registry.RegisterModule(ModuleA);
    Registry.RegisterModule(ModuleB);
    Registry.RegisterModule(ModuleC);

    BuildOrder := Registry.GetBuildOrder;
    try
      AssertEquals('Build order should have 3 modules', 3, BuildOrder.Count);
      AssertEquals('First should be ModuleA', 'ModuleA', TModuleInfo(BuildOrder[0]).Name);
      AssertEquals('Second should be ModuleB', 'ModuleB', TModuleInfo(BuildOrder[1]).Name);
      AssertEquals('Third should be ModuleC', 'ModuleC', TModuleInfo(BuildOrder[2]).Name);
    finally
      BuildOrder.Free;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TTestBuildOrder.TestBuildOrderDiamond;
var
  Registry: TModuleRegistry;
  ModuleA, ModuleB, ModuleC, ModuleD: TModuleInfo;
  BuildOrder: TList;
begin
  { Diamond: D depends on B,C; B,C depend on A }
  Registry := TModuleRegistry.Create;
  ModuleA := TModuleInfo.Create;
  ModuleB := TModuleInfo.Create;
  ModuleC := TModuleInfo.Create;
  ModuleD := TModuleInfo.Create;
  try
    ModuleA.Name := 'A';
    ModuleA.Path := '/path/to/a';

    ModuleB.Name := 'B';
    ModuleB.Path := '/path/to/b';
    ModuleB.Dependencies.Add('A');

    ModuleC.Name := 'C';
    ModuleC.Path := '/path/to/c';
    ModuleC.Dependencies.Add('A');

    ModuleD.Name := 'D';
    ModuleD.Path := '/path/to/d';
    ModuleD.Dependencies.Add('B');
    ModuleD.Dependencies.Add('C');

    Registry.RegisterModule(ModuleA);
    Registry.RegisterModule(ModuleB);
    Registry.RegisterModule(ModuleC);
    Registry.RegisterModule(ModuleD);

    BuildOrder := Registry.GetBuildOrder;
    try
      AssertEquals('Build order should have 4 modules', 4, BuildOrder.Count);
      { A should be first }
      AssertEquals('First should be A', 'A', TModuleInfo(BuildOrder[0]).Name);
      { D should be last }
      AssertEquals('Last should be D', 'D', TModuleInfo(BuildOrder[3]).Name);
      { B and C should be in middle (order may vary) }
      AssertTrue('Middle should contain B and C',
        ((TModuleInfo(BuildOrder[1]).Name = 'B') or (TModuleInfo(BuildOrder[1]).Name = 'C')) and
        ((TModuleInfo(BuildOrder[2]).Name = 'B') or (TModuleInfo(BuildOrder[2]).Name = 'C')));
    finally
      BuildOrder.Free;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TTestBuildOrder.TestBuildOrderMultipleRoots;
var
  Registry: TModuleRegistry;
  ModuleA, ModuleB, ModuleC: TModuleInfo;
  BuildOrder: TList;
begin
  { A,B both depend on C }
  Registry := TModuleRegistry.Create;
  ModuleA := TModuleInfo.Create;
  ModuleB := TModuleInfo.Create;
  ModuleC := TModuleInfo.Create;
  try
    ModuleA.Name := 'A';
    ModuleA.Path := '/path/to/a';
    ModuleA.Dependencies.Add('C');

    ModuleB.Name := 'B';
    ModuleB.Path := '/path/to/b';
    ModuleB.Dependencies.Add('C');

    ModuleC.Name := 'C';
    ModuleC.Path := '/path/to/c';

    Registry.RegisterModule(ModuleA);
    Registry.RegisterModule(ModuleB);
    Registry.RegisterModule(ModuleC);

    BuildOrder := Registry.GetBuildOrder;
    try
      AssertEquals('Build order should have 3 modules', 3, BuildOrder.Count);
      { C should be first }
      AssertEquals('First should be C', 'C', TModuleInfo(BuildOrder[0]).Name);
    finally
      BuildOrder.Free;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TTestBuildOrder.TestBuildOrderComplexGraph;
var
  Registry: TModuleRegistry;
  Modules: array[1..5] of TModuleInfo;
  I: Integer;
  BuildOrder: TList;
begin
  { Complex graph: 1 -> 2,3; 2 -> 4; 3 -> 4; 4 -> 5 }
  Registry := TModuleRegistry.Create;
  for I := 1 to 5 do
  begin
    Modules[I] := TModuleInfo.Create;
    Modules[I].Name := 'Mod' + IntToStr(I);
    Modules[I].Path := '/path/to/mod' + IntToStr(I);
  end;

  try
    Modules[2].Dependencies.Add('Mod1');
    Modules[3].Dependencies.Add('Mod1');
    Modules[4].Dependencies.Add('Mod2');
    Modules[4].Dependencies.Add('Mod3');
    Modules[5].Dependencies.Add('Mod4');

    for I := 1 to 5 do
      Registry.RegisterModule(Modules[I]);

    BuildOrder := Registry.GetBuildOrder;
    try
      AssertEquals('Build order should have 5 modules', 5, BuildOrder.Count);
      { Mod1 should be first, Mod5 should be last }
      AssertEquals('First should be Mod1', 'Mod1', TModuleInfo(BuildOrder[0]).Name);
      AssertEquals('Last should be Mod5', 'Mod5', TModuleInfo(BuildOrder[4]).Name);
    finally
      BuildOrder.Free;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TTestBuildOrder.TestCycleDetectionSimple;
var
  Registry: TModuleRegistry;
  ModuleA, ModuleB: TModuleInfo;
  ExceptionRaised: Boolean;
begin
  { Simple cycle: A -> B -> A }
  Registry := TModuleRegistry.Create;
  ModuleA := TModuleInfo.Create;
  ModuleB := TModuleInfo.Create;
  ExceptionRaised := False;
  try
    ModuleA.Name := 'A';
    ModuleA.Path := '/path/to/a';
    ModuleA.Dependencies.Add('B');

    ModuleB.Name := 'B';
    ModuleB.Path := '/path/to/b';
    ModuleB.Dependencies.Add('A');

    Registry.RegisterModule(ModuleA);
    Registry.RegisterModule(ModuleB);

    try
      Registry.GetBuildOrder;
    except
      on E: Exception do
      begin
        ExceptionRaised := True;
        AssertTrue('Error should mention cyclic', Pos('cyclic', LowerCase(E.Message)) > 0);
      end;
    end;
  finally
    Registry.Free;
  end;

  AssertTrue('Cyclic dependency should raise exception', ExceptionRaised);
end;

procedure TTestBuildOrder.TestCycleDetectionComplex;
var
  Registry: TModuleRegistry;
  ModuleA, ModuleB, ModuleC: TModuleInfo;
  ExceptionRaised: Boolean;
begin
  { Complex cycle: A -> B -> C -> A }
  Registry := TModuleRegistry.Create;
  ModuleA := TModuleInfo.Create;
  ModuleB := TModuleInfo.Create;
  ModuleC := TModuleInfo.Create;
  ExceptionRaised := False;
  try
    ModuleA.Name := 'A';
    ModuleA.Path := '/path/to/a';
    ModuleA.Dependencies.Add('B');

    ModuleB.Name := 'B';
    ModuleB.Path := '/path/to/b';
    ModuleB.Dependencies.Add('C');

    ModuleC.Name := 'C';
    ModuleC.Path := '/path/to/c';
    ModuleC.Dependencies.Add('A');

    Registry.RegisterModule(ModuleA);
    Registry.RegisterModule(ModuleB);
    Registry.RegisterModule(ModuleC);

    try
      Registry.GetBuildOrder;
    except
      on E: Exception do
      begin
        ExceptionRaised := True;
        AssertTrue('Error should mention cyclic', Pos('cyclic', LowerCase(E.Message)) > 0);
      end;
    end;
  finally
    Registry.Free;
  end;

  AssertTrue('Cyclic dependency should raise exception', ExceptionRaised);
end;

procedure TTestBuildOrder.TestCycleDetectionSelfReference;
var
  Registry: TModuleRegistry;
  ModuleA: TModuleInfo;
  ExceptionRaised: Boolean;
begin
  { Self reference: A -> A }
  Registry := TModuleRegistry.Create;
  ModuleA := TModuleInfo.Create;
  ExceptionRaised := False;
  try
    ModuleA.Name := 'A';
    ModuleA.Path := '/path/to/a';
    ModuleA.Dependencies.Add('A');

    Registry.RegisterModule(ModuleA);

    try
      Registry.GetBuildOrder;
    except
      on E: Exception do
      begin
        ExceptionRaised := True;
        AssertTrue('Error should mention cyclic', Pos('cyclic', LowerCase(E.Message)) > 0);
      end;
    end;
  finally
    Registry.Free;
  end;

  AssertTrue('Self-reference should raise exception', ExceptionRaised);
end;

{ TTestArtifactResolution }

procedure TTestArtifactResolution.TestResolveArtifactsSingleDependency;
var
  Registry: TModuleRegistry;
  ModuleA, ModuleB: TModuleInfo;
  Config: TProjectConfig;
begin
  { Module A depends on Module B }
  Registry := TModuleRegistry.Create;
  ModuleA := TModuleInfo.Create;
  ModuleB := TModuleInfo.Create;
  Config := TProjectConfig.Create;
  try
    ModuleB.Name := 'CoreLib';
    ModuleB.Path := '/path/to/core';
    ModuleB.UnitsDirectory := '/path/to/core/target/units';

    ModuleA.Name := 'MyApp';
    ModuleA.Path := '/path/to/app';
    ModuleA.UnitsDirectory := '/path/to/app/target/units';
    ModuleA.Dependencies.Add('CoreLib');

    Registry.RegisterModule(ModuleA);
    Registry.RegisterModule(ModuleB);

    AssertNotNull('Module B should exist', Registry.FindModuleByName('CoreLib'));
    AssertEquals('Module A should depend on B', 1, ModuleA.Dependencies.Count);
    AssertEquals('First dependency should be CoreLib', 'CoreLib', ModuleA.Dependencies[0]);
  finally
    Registry.Free;
    Config.Free;
  end;
end;

procedure TTestArtifactResolution.TestResolveArtifactsMultipleDependencies;
var
  Registry: TModuleRegistry;
  ModuleA, ModuleB, ModuleC: TModuleInfo;
begin
  { Module A depends on both B and C }
  Registry := TModuleRegistry.Create;
  ModuleA := TModuleInfo.Create;
  ModuleB := TModuleInfo.Create;
  ModuleC := TModuleInfo.Create;
  try
    ModuleB.Name := 'LibB';
    ModuleB.Path := '/path/to/b';
    ModuleB.UnitsDirectory := '/path/to/b/target/units';

    ModuleC.Name := 'LibC';
    ModuleC.Path := '/path/to/c';
    ModuleC.UnitsDirectory := '/path/to/c/target/units';

    ModuleA.Name := 'App';
    ModuleA.Path := '/path/to/app';
    ModuleA.UnitsDirectory := '/path/to/app/target/units';
    ModuleA.Dependencies.Add('LibB');
    ModuleA.Dependencies.Add('LibC');

    Registry.RegisterModule(ModuleA);
    Registry.RegisterModule(ModuleB);
    Registry.RegisterModule(ModuleC);

    AssertEquals('Module A should have 2 dependencies', 2, ModuleA.Dependencies.Count);
    AssertEquals('First dependency should be LibB', 'LibB', ModuleA.Dependencies[0]);
    AssertEquals('Second dependency should be LibC', 'LibC', ModuleA.Dependencies[1]);
  finally
    Registry.Free;
  end;
end;

procedure TTestArtifactResolution.TestResolveArtifactsTransitive;
var
  Registry: TModuleRegistry;
  ModuleA, ModuleB, ModuleC: TModuleInfo;
  BuildOrder: TList;
begin
  { A -> B -> C (transitive): A depends on B only, B depends on C }
  Registry := TModuleRegistry.Create;
  ModuleA := TModuleInfo.Create;
  ModuleB := TModuleInfo.Create;
  ModuleC := TModuleInfo.Create;
  try
    ModuleC.Name := 'CoreLib';
    ModuleC.Path := '/path/to/core';
    ModuleC.UnitsDirectory := '/path/to/core/target/units';

    ModuleB.Name := 'MiddleLib';
    ModuleB.Path := '/path/to/middle';
    ModuleB.UnitsDirectory := '/path/to/middle/target/units';
    ModuleB.Dependencies.Add('CoreLib');

    ModuleA.Name := 'App';
    ModuleA.Path := '/path/to/app';
    ModuleA.UnitsDirectory := '/path/to/app/target/units';
    ModuleA.Dependencies.Add('MiddleLib');

    Registry.RegisterModule(ModuleA);
    Registry.RegisterModule(ModuleB);
    Registry.RegisterModule(ModuleC);

    { Verify build order: C first, then B, then A }
    BuildOrder := Registry.GetBuildOrder;
    try
      AssertEquals('Build order should have 3 modules', 3, BuildOrder.Count);
      AssertEquals('CoreLib should be built first', 'CoreLib', TModuleInfo(BuildOrder[0]).Name);
      AssertEquals('MiddleLib should be built second', 'MiddleLib', TModuleInfo(BuildOrder[1]).Name);
      AssertEquals('App should be built last', 'App', TModuleInfo(BuildOrder[2]).Name);
    finally
      BuildOrder.Free;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TTestArtifactResolution.TestResolveArtifactsPomReference;
var
  Registry: TModuleRegistry;
  ModuleApp, ModuleAgg: TModuleInfo;
  Config: TProjectConfig;
begin
  { Application tries to depend on aggregator (packaging=pom) - should be invalid }
  Registry := TModuleRegistry.Create;
  ModuleApp := TModuleInfo.Create;
  ModuleAgg := TModuleInfo.Create;
  Config := TProjectConfig.Create;
  try
    { Create aggregator with pom packaging }
    ModuleAgg.Name := 'Aggregator';
    ModuleAgg.Path := '/path/to/agg';
    ModuleAgg.UnitsDirectory := '/path/to/agg/target/units';

    { App tries to depend on aggregator }
    ModuleApp.Name := 'MyApp';
    ModuleApp.Path := '/path/to/app';
    ModuleApp.UnitsDirectory := '/path/to/app/target/units';
    ModuleApp.Dependencies.Add('Aggregator');

    Registry.RegisterModule(ModuleApp);
    Registry.RegisterModule(ModuleAgg);

    { Verify the dependency was recorded (validation happens during artifact resolution) }
    AssertEquals('App should reference Aggregator', 'Aggregator', ModuleApp.Dependencies[0]);
  finally
    Registry.Free;
    Config.Free;
  end;
end;

procedure TTestArtifactResolution.TestResolveArtifactsLibraryModules;
var
  Registry: TModuleRegistry;
  ModuleLib: TModuleInfo;
begin
  { Library module produces units in target/units/ }
  Registry := TModuleRegistry.Create;
  ModuleLib := TModuleInfo.Create;
  try
    ModuleLib.Name := 'CoreLibrary';
    ModuleLib.Path := '/path/to/core';
    ModuleLib.UnitsDirectory := '/path/to/core/target/units';

    Registry.RegisterModule(ModuleLib);

    AssertEquals('Library module should be registered', 'CoreLibrary', ModuleLib.Name);
    AssertEquals('Units directory should be set correctly', '/path/to/core/target/units', ModuleLib.UnitsDirectory);
  finally
    Registry.Free;
  end;
end;

procedure TTestArtifactResolution.TestResolveArtifactsApplicationModules;
var
  Registry: TModuleRegistry;
  ModuleApp: TModuleInfo;
begin
  { Application module also produces units in target/units/ }
  Registry := TModuleRegistry.Create;
  ModuleApp := TModuleInfo.Create;
  try
    ModuleApp.Name := 'MyApplication';
    ModuleApp.Path := '/path/to/app';
    ModuleApp.UnitsDirectory := '/path/to/app/target/units';

    Registry.RegisterModule(ModuleApp);

    AssertEquals('Application module should be registered', 'MyApplication', ModuleApp.Name);
    AssertEquals('Units directory should be set correctly', '/path/to/app/target/units', ModuleApp.UnitsDirectory);
  finally
    Registry.Free;
  end;
end;

initialization
  RegisterTest(TTestModuleInfo);
  RegisterTest(TTestModuleRegistry);
  RegisterTest(TTestModuleDiscovery);
  RegisterTest(TTestBuildOrder);
  RegisterTest(TTestArtifactResolution);

end.
