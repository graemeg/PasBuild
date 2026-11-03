{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.FindProjectRoot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Types,
  PasBuild.Config;

type
  { Test FindProjectRoot — walks parent directories to find aggregator }
  TTestFindProjectRoot = class(TTestCase)
  private
    function GetFixturePath(const ASubPath: string): string;
  published
    procedure TestFindsRootFromModuleDirectory;
    procedure TestFindsRootFromNestedModuleDirectory;
    procedure TestReturnsEmptyWhenNoParentAggregator;
    procedure TestReturnsEmptyWhenAlreadyAtRoot;
    procedure TestReturnsEmptyForNonexistentDirectory;
    procedure TestIgnoresAggregatorThatDoesNotListTheDirectory;
  end;

  { Test FindModuleByDirName — resolves directory name to module name }
  TTestFindModuleByDirName = class(TTestCase)
  published
    procedure TestFindsModuleByDirectoryBasename;
    procedure TestReturnsNilForUnknownDirName;
    procedure TestDoesNotMatchModuleName;
  end;

  { Test improved error message for missing version }
  TTestMissingVersionMessage = class(TTestCase)
  private
    function GetFixturePath(const ASubPath: string): string;
  published
    procedure TestErrorMessageHintsAtProjectRoot;
  end;

implementation

{ TTestFindProjectRoot }

function TTestFindProjectRoot.GetFixturePath(const ASubPath: string): string;
begin
  Result := 'fixtures/multi-module/' + ASubPath;
end;

procedure TTestFindProjectRoot.TestFindsRootFromModuleDirectory;
var
  Root: string;
begin
  Root := TConfigLoader.FindProjectRoot(GetFixturePath('find-root/docview'));
  AssertTrue('Should find aggregator root',
    Root <> '');
  AssertTrue('Root should end with find-root',
    Pos('find-root', Root) > 0);
  AssertTrue('Root should not contain docview',
    Pos('docview', Root) = 0);
end;

procedure TTestFindProjectRoot.TestFindsRootFromNestedModuleDirectory;
var
  Root: string;
begin
  Root := TConfigLoader.FindProjectRoot(GetFixturePath('find-root-nested/apps/editor'));
  AssertTrue('Should find top-level aggregator root',
    Root <> '');
  AssertTrue('Root should end with find-root-nested',
    Pos('find-root-nested', Root) > 0);
  AssertTrue('Root should not contain apps/editor',
    Pos('editor', Root) = 0);
end;

procedure TTestFindProjectRoot.TestReturnsEmptyWhenNoParentAggregator;
var
  Root: string;
begin
  Root := TConfigLoader.FindProjectRoot(GetFixturePath('find-root-no-parent'));
  AssertEquals('Should return empty when no parent aggregator found', '', Root);
end;

procedure TTestFindProjectRoot.TestReturnsEmptyWhenAlreadyAtRoot;
var
  Root: string;
begin
  Root := TConfigLoader.FindProjectRoot(GetFixturePath('find-root'));
  AssertEquals('Should return empty when already at an aggregator root', '', Root);
end;

procedure TTestFindProjectRoot.TestReturnsEmptyForNonexistentDirectory;
var
  Root: string;
begin
  Root := TConfigLoader.FindProjectRoot(GetFixturePath('nonexistent-directory'));
  AssertEquals('Should return empty for nonexistent directory', '', Root);
end;

procedure TTestFindProjectRoot.TestIgnoresAggregatorThatDoesNotListTheDirectory;
var
  Root: string;
begin
  { find-root/examples/standalone is a project of its own inside the
    aggregator's directory, not one of its modules: building it must not
    switch to the aggregator. }
  Root := TConfigLoader.FindProjectRoot(GetFixturePath('find-root/examples/standalone'));
  AssertEquals('A directory the aggregator does not list stays put', '', Root);
end;

{ TTestFindModuleByDirName }

procedure TTestFindModuleByDirName.TestFindsModuleByDirectoryBasename;
var
  Registry: TModuleRegistry;
  Module, Found: TModuleInfo;
begin
  Registry := TModuleRegistry.Create;
  try
    Module := TModuleInfo.Create;
    Module.Name := 'fpgui-docview';
    Module.Path := '/projects/fpgui/docview';
    Registry.RegisterModule(Module);

    Found := Registry.FindModuleByDirName('docview');
    AssertNotNull('Should find module by directory basename', Found);
    AssertEquals('Should return correct module', 'fpgui-docview', Found.Name);
  finally
    Registry.Free;
  end;
end;

procedure TTestFindModuleByDirName.TestReturnsNilForUnknownDirName;
var
  Registry: TModuleRegistry;
  Module, Found: TModuleInfo;
begin
  Registry := TModuleRegistry.Create;
  try
    Module := TModuleInfo.Create;
    Module.Name := 'fpgui-docview';
    Module.Path := '/projects/fpgui/docview';
    Registry.RegisterModule(Module);

    Found := Registry.FindModuleByDirName('nonexistent');
    AssertNull('Should return nil for unknown directory name', Found);
  finally
    Registry.Free;
  end;
end;

procedure TTestFindModuleByDirName.TestDoesNotMatchModuleName;
var
  Registry: TModuleRegistry;
  Module, Found: TModuleInfo;
begin
  Registry := TModuleRegistry.Create;
  try
    Module := TModuleInfo.Create;
    Module.Name := 'fpgui-docview';
    Module.Path := '/projects/fpgui/docview';
    Registry.RegisterModule(Module);

    Found := Registry.FindModuleByDirName('fpgui-docview');
    AssertNull('Should not match module name when directory differs', Found);
  finally
    Registry.Free;
  end;
end;

{ TTestMissingVersionMessage }

function TTestMissingVersionMessage.GetFixturePath(const ASubPath: string): string;
begin
  Result := 'fixtures/multi-module/' + ASubPath;
end;

procedure TTestMissingVersionMessage.TestErrorMessageHintsAtProjectRoot;
var
  Config: TProjectConfig;
  ExceptionRaised: Boolean;
  ErrorMsg: string;
begin
  ExceptionRaised := False;
  Config := nil;
  ErrorMsg := '';
  try
    Config := TConfigLoader.LoadProjectXML(
      GetFixturePath('find-root-no-parent/project.xml'));
  except
    on E: EProjectConfigError do
    begin
      ExceptionRaised := True;
      ErrorMsg := E.Message;
    end;
  end;

  AssertTrue('Missing version should raise exception', ExceptionRaised);
  AssertTrue('Error should mention running from project root',
    Pos('root', LowerCase(ErrorMsg)) > 0);
  if Assigned(Config) then
    Config.Free;
end;

initialization
  RegisterTest(TTestFindProjectRoot);
  RegisterTest(TTestFindModuleByDirName);
  RegisterTest(TTestMissingVersionMessage);

end.
