{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Repository;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, DOM, XMLRead, XMLWrite;

type
  { Dependency information stored in metadata }
  TMetadataDependency = record
    Name: string;
    Version: string;
  end;
  TMetadataDependencyArray = array of TMetadataDependency;

  { Artifact metadata read from/written to metadata.xml }
  TArtifactMetadata = class
  private
    FName: string;
    FVersion: string;
    FPackaging: string;
    FCompiler: string;
    FCompilerVersion: string;
    FTargetCPU: string;
    FTargetOS: string;
    FTimestamp: string;
    FDependencies: TMetadataDependencyArray;
  public
    property Name: string read FName write FName;
    property Version: string read FVersion write FVersion;
    property Packaging: string read FPackaging write FPackaging;
    property Compiler: string read FCompiler write FCompiler;
    property CompilerVersion: string read FCompilerVersion write FCompilerVersion;
    property TargetCPU: string read FTargetCPU write FTargetCPU;
    property TargetOS: string read FTargetOS write FTargetOS;
    property Timestamp: string read FTimestamp write FTimestamp;
    property Dependencies: TMetadataDependencyArray read FDependencies write FDependencies;
  end;

  { Local repository management }
  TLocalRepository = class
  private
    FRepositoryRoot: string;
  public
    constructor Create; overload;
    constructor Create(const ARepositoryRoot: string); overload;

    { Default repository root directory }
    class function GetDefaultRoot: string;

    property RepositoryRoot: string read FRepositoryRoot;

    { Path construction }
    function GetArtifactPath(const AName, AVersion, ATargetId: string): string;
    function GetUnitsPath(const AName, AVersion, ATargetId: string): string;
    function GetMetadataPath(const AName, AVersion, ATargetId: string): string;

    { Repository queries }
    function ArtifactExists(const AName, AVersion, ATargetId: string): Boolean;
    function GetAvailableTargets(const AName, AVersion: string): TStringList;

    { Directory management }
    procedure EnsureDirectoryStructure(const AName, AVersion, ATargetId: string);

    { Artifact installation }
    procedure CopyUnitFiles(const ASourceUnitsDir, AName, AVersion, ATargetId: string);
    function CountFiles(const ADirectory, APattern: string): Integer;

    { Metadata operations }
    procedure WriteMetadata(const AName, AVersion, ATargetId: string; AMetadata: TArtifactMetadata);
    function ReadMetadata(const AName, AVersion, ATargetId: string): TArtifactMetadata;
  end;

implementation

{ TLocalRepository }

constructor TLocalRepository.Create;
begin
  inherited Create;
  FRepositoryRoot := GetDefaultRoot;
end;

constructor TLocalRepository.Create(const ARepositoryRoot: string);
begin
  inherited Create;
  FRepositoryRoot := ARepositoryRoot;
end;

class function TLocalRepository.GetDefaultRoot: string;
begin
  Result := IncludeTrailingPathDelimiter(GetUserDir) +
            '.pasbuild' + DirectorySeparator + 'repository';
end;

function TLocalRepository.GetArtifactPath(const AName, AVersion, ATargetId: string): string;
begin
  Result := FRepositoryRoot + DirectorySeparator +
            AName + DirectorySeparator +
            AVersion + DirectorySeparator +
            ATargetId;
end;

function TLocalRepository.GetUnitsPath(const AName, AVersion, ATargetId: string): string;
begin
  Result := GetArtifactPath(AName, AVersion, ATargetId) + DirectorySeparator + 'units';
end;

function TLocalRepository.GetMetadataPath(const AName, AVersion, ATargetId: string): string;
begin
  Result := GetArtifactPath(AName, AVersion, ATargetId) + DirectorySeparator + 'metadata.xml';
end;

function TLocalRepository.ArtifactExists(const AName, AVersion, ATargetId: string): Boolean;
var
  ArtifactDir, MetadataFile: string;
begin
  ArtifactDir := GetArtifactPath(AName, AVersion, ATargetId);
  MetadataFile := GetMetadataPath(AName, AVersion, ATargetId);
  Result := DirectoryExists(ArtifactDir) and FileExists(MetadataFile);
end;

function TLocalRepository.GetAvailableTargets(const AName, AVersion: string): TStringList;
var
  SearchRec: TSearchRec;
  VersionDir: string;
begin
  Result := TStringList.Create;
  Result.Sorted := True;

  VersionDir := FRepositoryRoot + DirectorySeparator +
                AName + DirectorySeparator + AVersion;

  if not DirectoryExists(VersionDir) then
    Exit;

  if FindFirst(IncludeTrailingPathDelimiter(VersionDir) + '*', faDirectory, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
           ((SearchRec.Attr and faDirectory) = faDirectory) then
          Result.Add(SearchRec.Name);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

procedure TLocalRepository.EnsureDirectoryStructure(const AName, AVersion, ATargetId: string);
var
  UnitsDir: string;
begin
  UnitsDir := GetUnitsPath(AName, AVersion, ATargetId);
  if not ForceDirectories(UnitsDir) then
    raise Exception.CreateFmt('Failed to create repository directory: %s', [UnitsDir]);
end;

procedure TLocalRepository.CopyUnitFiles(const ASourceUnitsDir, AName, AVersion, ATargetId: string);
var
  SearchRec: TSearchRec;
  SourcePath, DestPath, DestDir: string;
  SourceStream, DestStream: TFileStream;
begin
  if not DirectoryExists(ASourceUnitsDir) then
    raise Exception.CreateFmt('Source units directory not found: %s', [ASourceUnitsDir]);

  DestDir := GetUnitsPath(AName, AVersion, ATargetId);

  if FindFirst(IncludeTrailingPathDelimiter(ASourceUnitsDir) + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Attr and faDirectory) = 0 then
        begin
          // Copy .ppu and .o files
          if AnsiEndsText('.ppu', SearchRec.Name) or
             AnsiEndsText('.o', SearchRec.Name) then
          begin
            SourcePath := IncludeTrailingPathDelimiter(ASourceUnitsDir) + SearchRec.Name;
            DestPath := IncludeTrailingPathDelimiter(DestDir) + SearchRec.Name;

            SourceStream := TFileStream.Create(SourcePath, fmOpenRead or fmShareDenyNone);
            try
              DestStream := TFileStream.Create(DestPath, fmCreate);
              try
                DestStream.CopyFrom(SourceStream, SourceStream.Size);
              finally
                DestStream.Free;
              end;
            finally
              SourceStream.Free;
            end;
          end;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

function TLocalRepository.CountFiles(const ADirectory, APattern: string): Integer;
var
  SearchRec: TSearchRec;
begin
  Result := 0;
  if not DirectoryExists(ADirectory) then
    Exit;

  if FindFirst(IncludeTrailingPathDelimiter(ADirectory) + APattern, faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Attr and faDirectory) = 0 then
          Inc(Result);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

procedure TLocalRepository.WriteMetadata(const AName, AVersion, ATargetId: string;
  AMetadata: TArtifactMetadata);
var
  Doc: TXMLDocument;
  RootNode, BuildNode, DepsNode, DepNode, ElemNode: TDOMNode;
  I: Integer;
  MetadataPath: string;
begin
  MetadataPath := GetMetadataPath(AName, AVersion, ATargetId);

  Doc := TXMLDocument.Create;
  try
    // Create root <artifact> element
    RootNode := Doc.CreateElement('artifact');
    Doc.AppendChild(RootNode);

    // Add identity elements
    ElemNode := Doc.CreateElement('name');
    ElemNode.AppendChild(Doc.CreateTextNode(AMetadata.Name));
    RootNode.AppendChild(ElemNode);

    ElemNode := Doc.CreateElement('version');
    ElemNode.AppendChild(Doc.CreateTextNode(AMetadata.Version));
    RootNode.AppendChild(ElemNode);

    ElemNode := Doc.CreateElement('packaging');
    ElemNode.AppendChild(Doc.CreateTextNode(AMetadata.Packaging));
    RootNode.AppendChild(ElemNode);

    // Add <build> section
    BuildNode := Doc.CreateElement('build');
    RootNode.AppendChild(BuildNode);

    ElemNode := Doc.CreateElement('compiler');
    ElemNode.AppendChild(Doc.CreateTextNode(AMetadata.Compiler));
    BuildNode.AppendChild(ElemNode);

    ElemNode := Doc.CreateElement('compilerVersion');
    ElemNode.AppendChild(Doc.CreateTextNode(AMetadata.CompilerVersion));
    BuildNode.AppendChild(ElemNode);

    ElemNode := Doc.CreateElement('targetCPU');
    ElemNode.AppendChild(Doc.CreateTextNode(AMetadata.TargetCPU));
    BuildNode.AppendChild(ElemNode);

    ElemNode := Doc.CreateElement('targetOS');
    ElemNode.AppendChild(Doc.CreateTextNode(AMetadata.TargetOS));
    BuildNode.AppendChild(ElemNode);

    ElemNode := Doc.CreateElement('timestamp');
    ElemNode.AppendChild(Doc.CreateTextNode(AMetadata.Timestamp));
    BuildNode.AppendChild(ElemNode);

    // Add <dependencies> section
    DepsNode := Doc.CreateElement('dependencies');
    RootNode.AppendChild(DepsNode);

    for I := 0 to Length(AMetadata.Dependencies) - 1 do
    begin
      DepNode := Doc.CreateElement('dependency');
      DepsNode.AppendChild(DepNode);

      ElemNode := Doc.CreateElement('name');
      ElemNode.AppendChild(Doc.CreateTextNode(AMetadata.Dependencies[I].Name));
      DepNode.AppendChild(ElemNode);

      ElemNode := Doc.CreateElement('version');
      ElemNode.AppendChild(Doc.CreateTextNode(AMetadata.Dependencies[I].Version));
      DepNode.AppendChild(ElemNode);
    end;

    // Write to file
    WriteXMLFile(Doc, MetadataPath);
  finally
    Doc.Free;
  end;
end;

function TLocalRepository.ReadMetadata(const AName, AVersion, ATargetId: string): TArtifactMetadata;
var
  Doc: TXMLDocument;
  RootNode, BuildNode, DepsNode, DepNode, ChildNode: TDOMNode;
  MetadataPath: string;
  DepCount, I: Integer;

  function GetChildText(AParent: TDOMNode; const ATagName: string): string;
  var
    Node: TDOMNode;
  begin
    Result := '';
    if AParent = nil then
      Exit;
    Node := AParent.FindNode(ATagName);
    if (Node <> nil) and (Node.FirstChild <> nil) then
      Result := string(Node.FirstChild.NodeValue);
  end;

begin
  MetadataPath := GetMetadataPath(AName, AVersion, ATargetId);

  if not FileExists(MetadataPath) then
    raise Exception.CreateFmt('Metadata file not found: %s', [MetadataPath]);

  Result := TArtifactMetadata.Create;
  // ReadXMLFile's first parameter is 'out' - pre-creating a document here
  // would leak it on every call
  Doc := nil;
  try
    ReadXMLFile(Doc, MetadataPath);
    RootNode := Doc.DocumentElement;

    Result.Name := GetChildText(RootNode, 'name');
    Result.Version := GetChildText(RootNode, 'version');
    Result.Packaging := GetChildText(RootNode, 'packaging');

    // Parse <build> section
    BuildNode := RootNode.FindNode('build');
    if BuildNode <> nil then
    begin
      Result.Compiler := GetChildText(BuildNode, 'compiler');
      Result.CompilerVersion := GetChildText(BuildNode, 'compilerVersion');
      // Backwards compat: old metadata used <fpcVersion> without <compiler>
      if (Result.CompilerVersion = '') and (Result.Compiler = '') then
      begin
        Result.CompilerVersion := GetChildText(BuildNode, 'fpcVersion');
        if Result.CompilerVersion <> '' then
          Result.Compiler := 'fpc';
      end;
      Result.TargetCPU := GetChildText(BuildNode, 'targetCPU');
      Result.TargetOS := GetChildText(BuildNode, 'targetOS');
      Result.Timestamp := GetChildText(BuildNode, 'timestamp');
    end;

    // Parse <dependencies> section
    DepsNode := RootNode.FindNode('dependencies');
    if DepsNode <> nil then
    begin
      // Count dependency elements
      DepCount := 0;
      ChildNode := DepsNode.FirstChild;
      while ChildNode <> nil do
      begin
        if ChildNode.NodeName = 'dependency' then
          Inc(DepCount);
        ChildNode := ChildNode.NextSibling;
      end;

      SetLength(Result.FDependencies, DepCount);

      // Parse each dependency
      I := 0;
      ChildNode := DepsNode.FirstChild;
      while ChildNode <> nil do
      begin
        if ChildNode.NodeName = 'dependency' then
        begin
          Result.FDependencies[I].Name := GetChildText(ChildNode, 'name');
          Result.FDependencies[I].Version := GetChildText(ChildNode, 'version');
          Inc(I);
        end;
        ChildNode := ChildNode.NextSibling;
      end;
    end;
  except
    Doc.Free;
    Result.Free;
    raise;
  end;
  Doc.Free;
end;

end.
