{
  This file is part of PasBuild.

  Copyright (c) 2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.ProjectFlattener;

{$mode objfpc}{$H+}

{ Produces a self-contained project.xml for a module of a multi-module
  project. A module normally relies on its aggregator: it may omit <version>
  (inherited) and refer to siblings through <moduleDependencies>. Neither
  works once the module is shipped on its own, as in a published source
  archive, so the flattened form:

  - writes the effective (inherited) <version> when the module omits it;
  - replaces <moduleDependencies> with <dependencies> entries naming the
    published package and version of each library sibling;
  - drops dependencies on non-library siblings (applications, tools): those
    only order the reactor build and are never needed by consumers. }

interface

uses
  Classes, SysUtils,
  PasBuild.Types;

type
  { Raised when a module dependency cannot be resolved in the registry }
  EModuleDependencyError = class(Exception);

  TProjectFlattener = class
  public
    { Translate AConfig.ModuleDependencies into published package names and
      versions (appended to ANames/AVersions). Non-library siblings are
      skipped. Raises EModuleDependencyError for unknown modules, or when
      module dependencies exist but ARegistry is nil. }
    class procedure ResolveModuleDependencies(AConfig: TProjectConfig;
      ARegistry: TModuleRegistry; ANames, AVersions: TStrings);

    { Return the flattened form of AXml (the module's project.xml text) }
    class function FlattenProjectXml(const AXml: string;
      AConfig: TProjectConfig; ARegistry: TModuleRegistry): string;
  end;

implementation

uses
  DOM, XMLRead, XMLWrite,
  PasBuild.Utils;

function FindChildElement(AParent: TDOMNode; const AName: string): TDOMElement;
var
  Node: TDOMNode;
begin
  Result := nil;
  Node := AParent.FirstChild;
  while Assigned(Node) do
  begin
    if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = UnicodeString(AName)) then
      Exit(TDOMElement(Node));
    Node := Node.NextSibling;
  end;
end;

function NewTextElement(ADoc: TXMLDocument; const AName, AText: string): TDOMElement;
begin
  Result := ADoc.CreateElement(UnicodeString(AName));
  Result.AppendChild(ADoc.CreateTextNode(UnicodeString(AText)));
end;

{ TProjectFlattener }

class procedure TProjectFlattener.ResolveModuleDependencies(
  AConfig: TProjectConfig; ARegistry: TModuleRegistry;
  ANames, AVersions: TStrings);
var
  I: Integer;
  Module: TModuleInfo;
begin
  for I := 0 to AConfig.ModuleDependencies.Count - 1 do
  begin
    if not Assigned(ARegistry) then
      raise EModuleDependencyError.CreateFmt(
        '"%s" declares module dependency "%s" but no module registry is ' +
        'available to resolve it.',
        [AConfig.Name, AConfig.ModuleDependencies[I]]);

    Module := ARegistry.FindModuleByName(AConfig.ModuleDependencies[I]);
    if not Assigned(Module) then
      raise EModuleDependencyError.CreateFmt(
        '"%s": module dependency "%s" was not found in this project.',
        [AConfig.Name, AConfig.ModuleDependencies[I]]);

    if Module.Config.BuildConfig.ProjectType <> ptLibrary then
    begin
      TUtils.LogInfo('Omitting build-order-only module dependency "' +
        AConfig.ModuleDependencies[I] + '" (not a library)');
      Continue;
    end;

    ANames.Add(Module.Config.Name);
    AVersions.Add(Module.Config.Version);
  end;
end;

class function TProjectFlattener.FlattenProjectXml(const AXml: string;
  AConfig: TProjectConfig; ARegistry: TModuleRegistry): string;
var
  Doc: TXMLDocument;
  InStream: TStringStream;
  OutStream: TStringStream;
  Root, NameEl, VersionEl, ModDeps, Deps, Dep: TDOMElement;
  Names, Versions: TStringList;
  I: Integer;
begin
  Names := TStringList.Create;
  Versions := TStringList.Create;
  InStream := TStringStream.Create(AXml);
  Doc := nil;
  try
    // Resolve first so an unresolvable dependency fails before any output
    ResolveModuleDependencies(AConfig, ARegistry, Names, Versions);

    ReadXMLFile(Doc, InStream);
    Root := Doc.DocumentElement;

    // Inherited version
    VersionEl := FindChildElement(Root, 'version');
    if not Assigned(VersionEl) then
    begin
      VersionEl := NewTextElement(Doc, 'version', AConfig.Version);
      NameEl := FindChildElement(Root, 'name');
      // XMLWrite re-indents element children, so no whitespace nodes needed
      if Assigned(NameEl) and Assigned(NameEl.NextSibling) then
        Root.InsertBefore(VersionEl, NameEl.NextSibling)
      else
        Root.AppendChild(VersionEl);
    end;

    // Module dependencies become package dependencies
    ModDeps := FindChildElement(Root, 'moduleDependencies');
    if Assigned(ModDeps) then
      Root.RemoveChild(ModDeps).Free;

    if Names.Count > 0 then
    begin
      Deps := FindChildElement(Root, 'dependencies');
      if not Assigned(Deps) then
      begin
        Deps := Doc.CreateElement('dependencies');
        Root.AppendChild(Deps);
      end;
      for I := 0 to Names.Count - 1 do
      begin
        Dep := Doc.CreateElement('dependency');
        Dep.AppendChild(NewTextElement(Doc, 'name', Names[I]));
        Dep.AppendChild(NewTextElement(Doc, 'version', Versions[I]));
        Deps.AppendChild(Dep);
      end;
    end;

    OutStream := TStringStream.Create('');
    try
      WriteXMLFile(Doc, OutStream);
      Result := OutStream.DataString;
    finally
      OutStream.Free;
    end;
  finally
    Doc.Free;
    InStream.Free;
    Versions.Free;
    Names.Free;
  end;
end;

end.
