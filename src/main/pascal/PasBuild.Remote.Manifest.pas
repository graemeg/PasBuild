{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Remote.Manifest;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, DOM, XMLRead,
  PasBuild.Repository;

type
  TCompilerRequirement = record
    Name: string;
    MinimumVersion: string;
  end;
  TCompilerRequirementArray = array of TCompilerRequirement;

  TReleaseManifest = class
  private
    FName: string;
    FVersion: string;
    FAuthor: string;
    FLicense: string;
    FDescription: string;
    FOriginUrl: string;
    FOriginSHA256: string;
    FOriginSize: Int64;
    FCompilers: TCompilerRequirementArray;
    FDependencies: TMetadataDependencyArray;
    FPublishedAt: string;
  public
    function IsCompatibleWith(const ACompilerName: string): Boolean;
    function MinVersionFor(const ACompilerName: string): string;

    property Name: string read FName write FName;
    property Version: string read FVersion write FVersion;
    property Author: string read FAuthor write FAuthor;
    property License: string read FLicense write FLicense;
    property Description: string read FDescription write FDescription;
    property OriginUrl: string read FOriginUrl write FOriginUrl;
    property OriginSHA256: string read FOriginSHA256 write FOriginSHA256;
    property OriginSize: Int64 read FOriginSize write FOriginSize;
    property Compilers: TCompilerRequirementArray read FCompilers write FCompilers;
    property Dependencies: TMetadataDependencyArray read FDependencies write FDependencies;
    property PublishedAt: string read FPublishedAt write FPublishedAt;
  end;

  TManifestParser = class
  private
    class function GetNodeText(AParent: TDOMNode; const ATagName: string;
      const ADefault: string = ''): string;
    class procedure ParseOrigin(AOriginNode: TDOMNode; AManifest: TReleaseManifest);
    class procedure ParseCompilers(ACompilersNode: TDOMNode; AManifest: TReleaseManifest);
    class procedure ParseDependencies(ADepsNode: TDOMNode; AManifest: TReleaseManifest);
  public
    class function ParseVersionManifest(const AFilePath: string): TReleaseManifest;
    class function ParseVersionManifestFromString(const AXml: string): TReleaseManifest;
  end;

implementation

{ TReleaseManifest }

function TReleaseManifest.IsCompatibleWith(const ACompilerName: string): Boolean;
var
  I: Integer;
begin
  if Length(FCompilers) = 0 then
    Exit(True);

  for I := 0 to High(FCompilers) do
    if LowerCase(FCompilers[I].Name) = LowerCase(ACompilerName) then
      Exit(True);

  Result := False;
end;

function TReleaseManifest.MinVersionFor(const ACompilerName: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(FCompilers) do
    if LowerCase(FCompilers[I].Name) = LowerCase(ACompilerName) then
      Exit(FCompilers[I].MinimumVersion);
end;

{ TManifestParser }

class function TManifestParser.GetNodeText(AParent: TDOMNode;
  const ATagName: string; const ADefault: string): string;
var
  Node: TDOMNode;
begin
  Result := ADefault;
  if not Assigned(AParent) then
    Exit;
  Node := AParent.FindNode(UnicodeString(ATagName));
  if Assigned(Node) and Assigned(Node.FirstChild) then
    Result := Trim(string(Node.TextContent));
end;

class procedure TManifestParser.ParseOrigin(AOriginNode: TDOMNode;
  AManifest: TReleaseManifest);
var
  SizeStr: string;
begin
  if not Assigned(AOriginNode) then
    Exit;

  AManifest.OriginUrl := GetNodeText(AOriginNode, 'url');
  AManifest.OriginSHA256 := GetNodeText(AOriginNode, 'sha256');

  SizeStr := GetNodeText(AOriginNode, 'size');
  if SizeStr <> '' then
    AManifest.OriginSize := StrToInt64Def(SizeStr, 0)
  else
    AManifest.OriginSize := 0;
end;

class procedure TManifestParser.ParseCompilers(ACompilersNode: TDOMNode;
  AManifest: TReleaseManifest);
var
  Node: TDOMNode;
  Arr: TCompilerRequirementArray;
  Count, I: Integer;
begin
  if not Assigned(ACompilersNode) then
    Exit;

  Count := 0;
  Node := ACompilersNode.FirstChild;
  while Assigned(Node) do
  begin
    if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'compiler') then
      Inc(Count);
    Node := Node.NextSibling;
  end;

  SetLength(Arr, Count);
  I := 0;
  Node := ACompilersNode.FirstChild;
  while Assigned(Node) do
  begin
    if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'compiler') then
    begin
      Arr[I].Name := GetNodeText(Node, 'name');
      Arr[I].MinimumVersion := GetNodeText(Node, 'minimumVersion');
      Inc(I);
    end;
    Node := Node.NextSibling;
  end;
  AManifest.Compilers := Arr;
end;

class procedure TManifestParser.ParseDependencies(ADepsNode: TDOMNode;
  AManifest: TReleaseManifest);
var
  Node: TDOMNode;
  Arr: TMetadataDependencyArray;
  Count, I: Integer;
begin
  if not Assigned(ADepsNode) then
    Exit;

  Count := 0;
  Node := ADepsNode.FirstChild;
  while Assigned(Node) do
  begin
    if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'dependency') then
      Inc(Count);
    Node := Node.NextSibling;
  end;

  SetLength(Arr, Count);
  I := 0;
  Node := ADepsNode.FirstChild;
  while Assigned(Node) do
  begin
    if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'dependency') then
    begin
      Arr[I].Name := GetNodeText(Node, 'name');
      Arr[I].Version := GetNodeText(Node, 'version');
      Inc(I);
    end;
    Node := Node.NextSibling;
  end;
  AManifest.Dependencies := Arr;
end;

class function TManifestParser.ParseVersionManifest(
  const AFilePath: string): TReleaseManifest;
var
  Doc: TXMLDocument;
  RootNode: TDOMNode;
begin
  Doc := nil;
  Result := TReleaseManifest.Create;
  try
    ReadXMLFile(Doc, AFilePath);
    try
      RootNode := Doc.DocumentElement;

      Result.Name := GetNodeText(RootNode, 'name');
      Result.Version := GetNodeText(RootNode, 'version');
      Result.Author := GetNodeText(RootNode, 'author');
      Result.License := GetNodeText(RootNode, 'license');
      Result.Description := GetNodeText(RootNode, 'description');
      Result.PublishedAt := GetNodeText(RootNode, 'publishedAt');

      ParseOrigin(RootNode.FindNode('origin'), Result);
      ParseCompilers(RootNode.FindNode('compilers'), Result);
      ParseDependencies(RootNode.FindNode('dependencies'), Result);
    finally
      Doc.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function TManifestParser.ParseVersionManifestFromString(
  const AXml: string): TReleaseManifest;
var
  Stream: TStringStream;
  Doc: TXMLDocument;
  RootNode: TDOMNode;
begin
  Doc := nil;
  Result := TReleaseManifest.Create;
  Stream := TStringStream.Create(AXml);
  try
    try
      ReadXMLFile(Doc, Stream);
      try
        RootNode := Doc.DocumentElement;

        Result.Name := GetNodeText(RootNode, 'name');
        Result.Version := GetNodeText(RootNode, 'version');
        Result.Author := GetNodeText(RootNode, 'author');
        Result.License := GetNodeText(RootNode, 'license');
        Result.Description := GetNodeText(RootNode, 'description');
        Result.PublishedAt := GetNodeText(RootNode, 'publishedAt');

        ParseOrigin(RootNode.FindNode('origin'), Result);
        ParseCompilers(RootNode.FindNode('compilers'), Result);
        ParseDependencies(RootNode.FindNode('dependencies'), Result);
      finally
        Doc.Free;
      end;
    except
      Result.Free;
      raise;
    end;
  finally
    Stream.Free;
  end;
end;

end.
