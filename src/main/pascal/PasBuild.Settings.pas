{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead,
  PasBuild.Types;

type
  TSettings = class
  private
    FRepositories: TRepositoryList;
    FOfflineMode: Boolean;
    FConnectTimeout: Integer;
    FReadTimeout: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property Repositories: TRepositoryList read FRepositories;
    property OfflineMode: Boolean read FOfflineMode write FOfflineMode;
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;
    property ReadTimeout: Integer read FReadTimeout write FReadTimeout;
  end;

  TSettingsLoader = class
  private
    class function GetNodeText(AParent: TDOMNode; const ATagName: string;
      const ADefault: string = ''): string;
    class procedure ParseRepositories(ANode: TDOMNode; ASettings: TSettings);
    class procedure ParseHttp(ANode: TDOMNode; ASettings: TSettings);
  public
    class function LoadSettings(const AFilePath: string): TSettings;
    class function GetDefaultPath: string;
  end;

implementation

{ TSettings }

constructor TSettings.Create;
begin
  inherited Create;
  FRepositories := TRepositoryList.Create;
  FRepositories.FreeObjects := True;
  FOfflineMode := False;
  FConnectTimeout := 30;
  FReadTimeout := 60;
end;

destructor TSettings.Destroy;
begin
  FRepositories.Free;
  inherited Destroy;
end;

{ TSettingsLoader }

class function TSettingsLoader.GetNodeText(AParent: TDOMNode;
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

class procedure TSettingsLoader.ParseRepositories(ANode: TDOMNode;
  ASettings: TSettings);
var
  RepoNode: TDOMNode;
  Id, Url: string;
begin
  if not Assigned(ANode) then
    Exit;

  RepoNode := ANode.FirstChild;
  while Assigned(RepoNode) do
  begin
    if RepoNode.NodeName = 'repository' then
    begin
      Id := GetNodeText(RepoNode, 'id');
      Url := GetNodeText(RepoNode, 'url');
      if (Id <> '') and (Url <> '') then
        ASettings.Repositories.Add(TRepositoryInfo.Create(Id, Url));
    end;
    RepoNode := RepoNode.NextSibling;
  end;
end;

class procedure TSettingsLoader.ParseHttp(ANode: TDOMNode;
  ASettings: TSettings);
var
  Value: string;
  IntValue: Integer;
begin
  if not Assigned(ANode) then
    Exit;

  Value := GetNodeText(ANode, 'connectTimeout');
  if (Value <> '') and TryStrToInt(Value, IntValue) then
    ASettings.ConnectTimeout := IntValue;

  Value := GetNodeText(ANode, 'readTimeout');
  if (Value <> '') and TryStrToInt(Value, IntValue) then
    ASettings.ReadTimeout := IntValue;
end;

class function TSettingsLoader.LoadSettings(const AFilePath: string): TSettings;
var
  Doc: TXMLDocument;
  RootNode, Node: TDOMNode;
  OfflineStr: string;
begin
  Result := TSettings.Create;

  if not FileExists(AFilePath) then
    Exit;

  Doc := nil;
  try
    try
      ReadXMLFile(Doc, AFilePath);

      RootNode := Doc.DocumentElement;
      if not Assigned(RootNode) then
        Exit;

      Node := RootNode.FindNode('repositories');
      if Assigned(Node) then
        ParseRepositories(Node, Result);

      OfflineStr := LowerCase(GetNodeText(RootNode, 'offlineMode'));
      if OfflineStr = 'true' then
        Result.OfflineMode := True;

      Node := RootNode.FindNode('http');
      if Assigned(Node) then
        ParseHttp(Node, Result);
    finally
      Doc.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function TSettingsLoader.GetDefaultPath: string;
begin
  Result := IncludeTrailingPathDelimiter(GetUserDir) + '.pasbuild' +
    DirectorySeparator + 'settings.xml';
end;

end.
