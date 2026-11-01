{
  This file is part of PasBuild.

  Copyright (c) 2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.ProjectFlattener;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Types,
  PasBuild.Config,
  PasBuild.ProjectFlattener,
  PasBuild.Command.SourcePackage;

type
  { A module's project.xml relies on its aggregator (inherited <version>,
    sibling <moduleDependencies>). The flattened form must be buildable on
    its own, as it is when a consumer extracts a published source archive. }
  TTestProjectFlattener = class(TTestCase)
  private
    FRegistry: TModuleRegistry;
    FTempDir: string;
    function AddModule(const AModuleName, APackageName, AVersion: string;
      AType: TProjectType): TModuleInfo;
    function LoadFlattened(const AXml: string): TProjectConfig;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInjectsInheritedVersion;
    procedure TestKeepsExplicitVersion;
    procedure TestTranslatesLibraryModuleDependency;
    procedure TestPreservesExternalDependencies;
    procedure TestDropsNonLibraryModuleDependency;
    procedure TestMissingModuleDependencyRaises;
    procedure TestResolveSkipsNonLibraryModules;
    procedure TestModuleSourceArchiveIsStandalone;
    procedure TestModuleSourceArchiveIsReproducible;
  end;

implementation

uses
  PasBuild.Remote.Builder;

const
  MODULE_XML_NO_VERSION =
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<project>' + LineEnding +
    '  <name>gui-pkg</name>' + LineEnding +
    '  <author>Someone</author>' + LineEnding +
    '  <license>BSD-3-Clause</license>' + LineEnding +
    '  <build>' + LineEnding +
    '    <projectType>library</projectType>' + LineEnding +
    '  </build>' + LineEnding +
    '</project>' + LineEnding;

  MODULE_XML_WITH_DEPS =
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<project>' + LineEnding +
    '  <name>gui-pkg</name>' + LineEnding +
    '  <author>Someone</author>' + LineEnding +
    '  <license>BSD-3-Clause</license>' + LineEnding +
    '  <build>' + LineEnding +
    '    <projectType>library</projectType>' + LineEnding +
    '  </build>' + LineEnding +
    '  <moduleDependencies>' + LineEnding +
    '    <module>../lib-core</module>' + LineEnding +
    '    <module>../some-tool</module>' + LineEnding +
    '  </moduleDependencies>' + LineEnding +
    '  <dependencies>' + LineEnding +
    '    <dependency>' + LineEnding +
    '      <name>ext-lib</name>' + LineEnding +
    '      <version>1.2.3</version>' + LineEnding +
    '    </dependency>' + LineEnding +
    '  </dependencies>' + LineEnding +
    '</project>' + LineEnding;

procedure DeleteDirRecursive(const ADir: string);
var
  SR: TSearchRec;
  Path: string;
begin
  if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*', faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then
        Continue;
      Path := IncludeTrailingPathDelimiter(ADir) + SR.Name;
      if (SR.Attr and faDirectory) <> 0 then
        DeleteDirRecursive(Path)
      else
        DeleteFile(Path);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
  RemoveDir(ADir);
end;

{ TTestProjectFlattener }

procedure TTestProjectFlattener.SetUp;
begin
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'pasbuild-test-flatten-' + IntToStr(GetProcessID);
  ForceDirectories(FTempDir);
  FRegistry := TModuleRegistry.Create;
end;

procedure TTestProjectFlattener.TearDown;
begin
  FRegistry.Free;
  if DirectoryExists(FTempDir) then
    DeleteDirRecursive(FTempDir);
end;

function TTestProjectFlattener.AddModule(const AModuleName, APackageName,
  AVersion: string; AType: TProjectType): TModuleInfo;
var
  Config: TProjectConfig;
begin
  Result := TModuleInfo.Create;
  Result.Name := AModuleName;
  Result.Path := IncludeTrailingPathDelimiter(FTempDir) + AModuleName;

  Config := TProjectConfig.Create;
  Config.Name := APackageName;
  Config.Version := AVersion;
  Config.BuildConfig.ProjectType := AType;
  Result.Config := Config;

  FRegistry.RegisterModule(Result);
end;

function TTestProjectFlattener.LoadFlattened(const AXml: string): TProjectConfig;
var
  Lines: TStringList;
  Path: string;
begin
  // Loading without AAllowEmptyVersion proves the result is standalone
  Path := IncludeTrailingPathDelimiter(FTempDir) + 'flattened.xml';
  Lines := TStringList.Create;
  try
    Lines.Text := AXml;
    Lines.SaveToFile(Path);
  finally
    Lines.Free;
  end;
  Result := TConfigLoader.LoadProjectXML(Path);
end;

procedure TTestProjectFlattener.TestInjectsInheritedVersion;
var
  Module: TModuleInfo;
  Loaded: TProjectConfig;
begin
  Module := AddModule('lib-gui', 'gui-pkg', '2.1.0', ptLibrary);

  Loaded := LoadFlattened(TProjectFlattener.FlattenProjectXml(
    MODULE_XML_NO_VERSION, Module.Config, FRegistry));
  try
    AssertEquals('Name preserved', 'gui-pkg', Loaded.Name);
    AssertEquals('Inherited version written', '2.1.0', Loaded.Version);
    AssertEquals('Author preserved', 'Someone', Loaded.Author);
  finally
    Loaded.Free;
  end;
end;

procedure TTestProjectFlattener.TestKeepsExplicitVersion;
var
  Module: TModuleInfo;
  Xml, Flat: string;
begin
  Module := AddModule('lib-gui', 'gui-pkg', '2.1.0', ptLibrary);
  Xml := StringReplace(MODULE_XML_NO_VERSION, '<name>gui-pkg</name>',
    '<name>gui-pkg</name><version>2.1.0</version>', []);

  Flat := TProjectFlattener.FlattenProjectXml(Xml, Module.Config, FRegistry);

  AssertEquals('Exactly one <version> element', 1,
    (Length(Flat) - Length(StringReplace(Flat, '<version>', '',
      [rfReplaceAll]))) div Length('<version>'));
end;

procedure TTestProjectFlattener.TestTranslatesLibraryModuleDependency;
var
  Module: TModuleInfo;
  Loaded: TProjectConfig;
  I: Integer;
  Found: Boolean;
begin
  AddModule('lib-core', 'core-pkg', '3.1.0', ptLibrary);
  AddModule('some-tool', 'some-tool', '3.1.0', ptApplication);
  Module := AddModule('lib-gui', 'gui-pkg', '3.1.0', ptLibrary);
  Module.Config.ModuleDependencies.Add('lib-core');
  Module.Config.ModuleDependencies.Add('some-tool');

  Loaded := LoadFlattened(TProjectFlattener.FlattenProjectXml(
    MODULE_XML_WITH_DEPS, Module.Config, FRegistry));
  try
    AssertEquals('Module dependencies removed', 0,
      Loaded.ModuleDependencies.Count);
    Found := False;
    for I := 0 to Loaded.Dependencies.Count - 1 do
      if (Loaded.Dependencies[I].Name = 'core-pkg') and
         (Loaded.Dependencies[I].Version = '3.1.0') then
        Found := True;
    AssertTrue('Library sibling becomes a package dependency', Found);
  finally
    Loaded.Free;
  end;
end;

procedure TTestProjectFlattener.TestPreservesExternalDependencies;
var
  Module: TModuleInfo;
  Loaded: TProjectConfig;
  I: Integer;
  Found: Boolean;
begin
  AddModule('lib-core', 'core-pkg', '3.1.0', ptLibrary);
  AddModule('some-tool', 'some-tool', '3.1.0', ptApplication);
  Module := AddModule('lib-gui', 'gui-pkg', '3.1.0', ptLibrary);
  Module.Config.ModuleDependencies.Add('lib-core');
  Module.Config.ModuleDependencies.Add('some-tool');

  Loaded := LoadFlattened(TProjectFlattener.FlattenProjectXml(
    MODULE_XML_WITH_DEPS, Module.Config, FRegistry));
  try
    AssertEquals('External + translated dependency', 2,
      Loaded.Dependencies.Count);
    Found := False;
    for I := 0 to Loaded.Dependencies.Count - 1 do
      if (Loaded.Dependencies[I].Name = 'ext-lib') and
         (Loaded.Dependencies[I].Version = '1.2.3') then
        Found := True;
    AssertTrue('Existing external dependency kept', Found);
  finally
    Loaded.Free;
  end;
end;

procedure TTestProjectFlattener.TestDropsNonLibraryModuleDependency;
var
  Module: TModuleInfo;
  Loaded: TProjectConfig;
  I: Integer;
begin
  AddModule('lib-core', 'core-pkg', '3.1.0', ptLibrary);
  AddModule('some-tool', 'some-tool', '3.1.0', ptApplication);
  Module := AddModule('lib-gui', 'gui-pkg', '3.1.0', ptLibrary);
  Module.Config.ModuleDependencies.Add('lib-core');
  Module.Config.ModuleDependencies.Add('some-tool');

  Loaded := LoadFlattened(TProjectFlattener.FlattenProjectXml(
    MODULE_XML_WITH_DEPS, Module.Config, FRegistry));
  try
    for I := 0 to Loaded.Dependencies.Count - 1 do
      AssertFalse('Application sibling must not become a dependency',
        Loaded.Dependencies[I].Name = 'some-tool');
  finally
    Loaded.Free;
  end;
end;

procedure TTestProjectFlattener.TestMissingModuleDependencyRaises;
var
  Module: TModuleInfo;
  Raised: Boolean;
begin
  Module := AddModule('lib-gui', 'gui-pkg', '3.1.0', ptLibrary);
  Module.Config.ModuleDependencies.Add('lib-vanished');

  Raised := False;
  try
    TProjectFlattener.FlattenProjectXml(MODULE_XML_WITH_DEPS,
      Module.Config, FRegistry);
  except
    on E: EModuleDependencyError do
    begin
      Raised := True;
      AssertTrue('Error names the module', Pos('lib-vanished', E.Message) > 0);
    end;
  end;
  AssertTrue('Unresolvable module dependency raises', Raised);
end;

procedure TTestProjectFlattener.TestResolveSkipsNonLibraryModules;
var
  Module: TModuleInfo;
  Names, Versions: TStringList;
begin
  AddModule('lib-core', 'core-pkg', '3.1.0', ptLibrary);
  AddModule('some-tool', 'some-tool', '3.1.0', ptApplication);
  Module := AddModule('lib-gui', 'gui-pkg', '3.1.0', ptLibrary);
  Module.Config.ModuleDependencies.Add('some-tool');
  Module.Config.ModuleDependencies.Add('lib-core');

  Names := TStringList.Create;
  Versions := TStringList.Create;
  try
    TProjectFlattener.ResolveModuleDependencies(Module.Config, FRegistry,
      Names, Versions);
    AssertEquals('Only the library is resolved', 1, Names.Count);
    AssertEquals('core-pkg', Names[0]);
    AssertEquals('3.1.0', Versions[0]);
  finally
    Versions.Free;
    Names.Free;
  end;
end;

procedure TTestProjectFlattener.TestModuleSourceArchiveIsStandalone;
var
  Module: TModuleInfo;
  Cmd: TSourcePackageCommand;
  SavedDir, ExtractDir: string;
  Lines: TStringList;
  Loaded: TProjectConfig;
begin
  Module := AddModule('lib-gui', 'gui-pkg', '2.1.0', ptLibrary);
  Module.Config.BuildConfig.OutputDirectory := 'target';
  ForceDirectories(Module.Path + '/src/main/pascal');
  Lines := TStringList.Create;
  try
    Lines.Text := MODULE_XML_NO_VERSION;
    Lines.SaveToFile(Module.Path + '/project.xml');
    Lines.Text := 'unit gui; interface implementation end.';
    Lines.SaveToFile(Module.Path + '/src/main/pascal/gui.pas');
  finally
    Lines.Free;
  end;

  SavedDir := GetCurrentDir;
  SetCurrentDir(Module.Path);
  try
    Cmd := TSourcePackageCommand.Create(Module.Config, nil);
    try
      Cmd.ModuleRegistry := FRegistry;
      AssertEquals('source-package succeeds', 0, Cmd.Execute);
    finally
      Cmd.Free;
    end;
  finally
    SetCurrentDir(SavedDir);
  end;

  ExtractDir := IncludeTrailingPathDelimiter(FTempDir) + 'extract';
  ForceDirectories(ExtractDir);
  // Extract exactly as a consumer's remote build would
  TRemoteBuilder.ExtractArchive(Module.Path + '/target/gui-pkg-2.1.0-src.zip',
    ExtractDir);

  Loaded := TConfigLoader.LoadProjectXML(
    ExtractDir + '/gui-pkg-2.1.0/project.xml');
  try
    AssertEquals('Archived project.xml carries the inherited version',
      '2.1.0', Loaded.Version);
  finally
    Loaded.Free;
  end;
end;

procedure TTestProjectFlattener.TestModuleSourceArchiveIsReproducible;
var
  Module: TModuleInfo;
  Lines: TStringList;
  SavedDir, ZipPath: string;
  First, Second: TMemoryStream;

  procedure BuildArchive;
  var
    Cmd: TSourcePackageCommand;
  begin
    Cmd := TSourcePackageCommand.Create(Module.Config, nil);
    try
      Cmd.ModuleRegistry := FRegistry;
      AssertEquals('source-package succeeds', 0, Cmd.Execute);
    finally
      Cmd.Free;
    end;
  end;

begin
  // publish rebuilds the archive and compares it with the uploaded one;
  // unchanged sources must give byte-identical archives
  Module := AddModule('lib-gui', 'gui-pkg', '2.1.0', ptLibrary);
  Module.Config.BuildConfig.OutputDirectory := 'target';
  ForceDirectories(Module.Path + '/src/main/pascal');
  Lines := TStringList.Create;
  try
    Lines.Text := MODULE_XML_NO_VERSION;
    Lines.SaveToFile(Module.Path + '/project.xml');
    Lines.Text := 'unit gui; interface implementation end.';
    Lines.SaveToFile(Module.Path + '/src/main/pascal/gui.pas');
  finally
    Lines.Free;
  end;
  ZipPath := Module.Path + '/target/gui-pkg-2.1.0-src.zip';

  First := TMemoryStream.Create;
  Second := TMemoryStream.Create;
  SavedDir := GetCurrentDir;
  SetCurrentDir(Module.Path);
  try
    BuildArchive;
    First.LoadFromFile(ZipPath);
    Sleep(2100); // zip timestamps have 2-second resolution
    BuildArchive;
    Second.LoadFromFile(ZipPath);
    AssertEquals('Archive size stable', First.Size, Second.Size);
    AssertTrue('Archive bytes identical',
      CompareMem(First.Memory, Second.Memory, First.Size));
  finally
    SetCurrentDir(SavedDir);
    Second.Free;
    First.Free;
  end;
end;

initialization
  RegisterTest(TTestProjectFlattener);

end.
