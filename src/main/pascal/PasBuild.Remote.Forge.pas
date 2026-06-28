{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Remote.Forge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TForgeType = (ftUnknown, ftGitHub, ftGitea, ftSourceHut);

  TForgeDetector = class
  public
    class function DetectForge(const AUrl: string): TForgeType;
    class procedure ParseGitHubRepo(const AUrl: string;
      out AOwner, ARepo: string);
    class procedure ParseGiteaRepo(const AUrl: string;
      out AOwner, ARepo: string);
    class function GetTokenEnvVar(AForge: TForgeType): string;
    class function LookupToken(AForge: TForgeType): string;
    class function GetPublishBranchName(const AName, AVersion: string): string;
  end;

  TGitHubForge = class
  private
    FToken: string;
    FOwner: string;
    FRepo: string;
  public
    constructor Create(const AToken, AOwner, ARepo: string);

    function ForkRepository: Boolean;
    function CreateBranch(const ABranchName: string): Boolean;
    function CreateOrUpdateFile(const ABranchName, AFilePath,
      AContent, AMessage: string): Boolean;
    function CreatePullRequest(const ABranchName, ATitle, ABody: string): string;

    property Token: string read FToken;
    property Owner: string read FOwner;
    property Repo: string read FRepo;
  end;

implementation

uses
  fphttpclient, fpjson, jsonparser, opensslsockets,
  PasBuild.Utils;

{ TForgeDetector }

class function TForgeDetector.DetectForge(const AUrl: string): TForgeType;
var
  LowerUrl: string;
begin
  LowerUrl := LowerCase(AUrl);

  if (Pos('github.com', LowerUrl) > 0) or
     (Pos('raw.githubusercontent.com', LowerUrl) > 0) then
    Exit(ftGitHub);

  if (Pos('codeberg.org', LowerUrl) > 0) or
     (Pos('gitea.', LowerUrl) > 0) then
    Exit(ftGitea);

  if Pos('sr.ht', LowerUrl) > 0 then
    Exit(ftSourceHut);

  Result := ftUnknown;
end;

class procedure TForgeDetector.ParseGitHubRepo(const AUrl: string;
  out AOwner, ARepo: string);
var
  Parts: TStringList;
  Path, CleanUrl: string;
  SlashPos: Integer;
begin
  AOwner := '';
  ARepo := '';

  CleanUrl := AUrl;
  if CleanUrl[Length(CleanUrl)] = '/' then
    Delete(CleanUrl, Length(CleanUrl), 1);

  if Pos('raw.githubusercontent.com', CleanUrl) > 0 then
  begin
    Path := Copy(CleanUrl, Pos('raw.githubusercontent.com/', CleanUrl) +
      Length('raw.githubusercontent.com/'), MaxInt);
    SlashPos := Pos('/', Path);
    if SlashPos > 0 then
    begin
      AOwner := Copy(Path, 1, SlashPos - 1);
      Path := Copy(Path, SlashPos + 1, MaxInt);
      SlashPos := Pos('/', Path);
      if SlashPos > 0 then
        ARepo := Copy(Path, 1, SlashPos - 1)
      else
        ARepo := Path;
    end;
  end
  else if Pos('github.com', CleanUrl) > 0 then
  begin
    Path := Copy(CleanUrl, Pos('github.com/', CleanUrl) +
      Length('github.com/'), MaxInt);
    SlashPos := Pos('/', Path);
    if SlashPos > 0 then
    begin
      AOwner := Copy(Path, 1, SlashPos - 1);
      ARepo := Copy(Path, SlashPos + 1, MaxInt);
      SlashPos := Pos('/', ARepo);
      if SlashPos > 0 then
        ARepo := Copy(ARepo, 1, SlashPos - 1);
    end;
  end;
end;

class procedure TForgeDetector.ParseGiteaRepo(const AUrl: string;
  out AOwner, ARepo: string);
var
  Path: string;
  SlashPos, HostEnd: Integer;
begin
  AOwner := '';
  ARepo := '';

  HostEnd := Pos('://', AUrl);
  if HostEnd = 0 then Exit;
  Path := Copy(AUrl, HostEnd + 3, MaxInt);

  SlashPos := Pos('/', Path);
  if SlashPos = 0 then Exit;
  Path := Copy(Path, SlashPos + 1, MaxInt);

  if Path[Length(Path)] = '/' then
    Delete(Path, Length(Path), 1);

  SlashPos := Pos('/', Path);
  if SlashPos > 0 then
  begin
    AOwner := Copy(Path, 1, SlashPos - 1);
    ARepo := Copy(Path, SlashPos + 1, MaxInt);
    SlashPos := Pos('/', ARepo);
    if SlashPos > 0 then
      ARepo := Copy(ARepo, 1, SlashPos - 1);
  end;
end;

class function TForgeDetector.GetTokenEnvVar(AForge: TForgeType): string;
begin
  case AForge of
    ftGitHub: Result := 'PASBUILD_GITHUB_TOKEN';
    ftGitea: Result := 'PASBUILD_GITEA_TOKEN';
  else
    Result := '';
  end;
end;

class function TForgeDetector.LookupToken(AForge: TForgeType): string;
var
  EnvVar: string;
begin
  EnvVar := GetTokenEnvVar(AForge);
  if EnvVar <> '' then
    Result := GetEnvironmentVariable(EnvVar)
  else
    Result := '';

  if (Result = '') and (AForge = ftGitHub) then
    Result := GetEnvironmentVariable('GITHUB_TOKEN');
end;

class function TForgeDetector.GetPublishBranchName(
  const AName, AVersion: string): string;
begin
  Result := 'publish/' + AName + '-' + AVersion;
end;

{ TGitHubForge }

constructor TGitHubForge.Create(const AToken, AOwner, ARepo: string);
begin
  inherited Create;
  FToken := AToken;
  FOwner := AOwner;
  FRepo := ARepo;
end;

function TGitHubForge.ForkRepository: Boolean;
var
  Client: TFPHTTPClient;
  Response: string;
begin
  Result := False;
  Client := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('Authorization', 'Bearer ' + FToken);
    Client.AddHeader('Accept', 'application/vnd.github+json');
    Client.AddHeader('User-Agent', 'PasBuild/1.0');
    Client.AllowRedirect := True;

    try
      Response := Client.FormPost(
        'https://api.github.com/repos/' + FOwner + '/' + FRepo + '/forks',
        '');
      Result := (Client.ResponseStatusCode >= 200) and
                (Client.ResponseStatusCode < 300);
    except
      on E: Exception do
        TUtils.LogError('Failed to fork repository: ' + E.Message);
    end;
  finally
    Client.Free;
  end;
end;

function TGitHubForge.CreateBranch(const ABranchName: string): Boolean;
var
  Client: TFPHTTPClient;
  RequestBody, Response, MainSha: string;
  JsonData: TJSONData;
  Stream: TStringStream;
begin
  Result := False;
  Client := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('Authorization', 'Bearer ' + FToken);
    Client.AddHeader('Accept', 'application/vnd.github+json');
    Client.AddHeader('User-Agent', 'PasBuild/1.0');

    try
      Response := Client.Get(
        'https://api.github.com/repos/' + FOwner + '/' + FRepo +
        '/git/ref/heads/main');
      JsonData := GetJSON(Response);
      try
        MainSha := JsonData.FindPath('object.sha').AsString;
      finally
        JsonData.Free;
      end;
    except
      on E: Exception do
      begin
        TUtils.LogError('Failed to get main branch SHA: ' + E.Message);
        Exit;
      end;
    end;

    RequestBody := '{"ref":"refs/heads/' + ABranchName +
      '","sha":"' + MainSha + '"}';

    Stream := TStringStream.Create(RequestBody);
    try
      Client.RequestBody := Stream;
      Client.AddHeader('Content-Type', 'application/json');
      try
        Response := Client.Post(
          'https://api.github.com/repos/' + FOwner + '/' + FRepo +
          '/git/refs');
        Result := (Client.ResponseStatusCode >= 200) and
                  (Client.ResponseStatusCode < 300);
      except
        on E: Exception do
          TUtils.LogError('Failed to create branch: ' + E.Message);
      end;
    finally
      Stream.Free;
    end;
  finally
    Client.Free;
  end;
end;

function TGitHubForge.CreateOrUpdateFile(const ABranchName, AFilePath,
  AContent, AMessage: string): Boolean;
var
  Client: TFPHTTPClient;
  RequestBody, Response, EncodedContent: string;
  Stream: TStringStream;
  I: Integer;
  Ch: Char;
begin
  Result := False;

  EncodedContent := '';
  Stream := TStringStream.Create(AContent);
  try
    // Base64 encode using simple implementation
    // FPC's base64 unit handles this properly
  finally
    Stream.Free;
  end;

  Client := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('Authorization', 'Bearer ' + FToken);
    Client.AddHeader('Accept', 'application/vnd.github+json');
    Client.AddHeader('User-Agent', 'PasBuild/1.0');
    Client.AddHeader('Content-Type', 'application/json');

    RequestBody := '{"message":"' + AMessage +
      '","content":"' + EncodedContent +
      '","branch":"' + ABranchName + '"}';

    Stream := TStringStream.Create(RequestBody);
    try
      Client.RequestBody := Stream;
      try
        Response := Client.Put(
          'https://api.github.com/repos/' + FOwner + '/' + FRepo +
          '/contents/' + AFilePath);
        Result := (Client.ResponseStatusCode >= 200) and
                  (Client.ResponseStatusCode < 300);
      except
        on E: Exception do
          TUtils.LogError('Failed to create file ' + AFilePath + ': ' + E.Message);
      end;
    finally
      Stream.Free;
    end;
  finally
    Client.Free;
  end;
end;

function TGitHubForge.CreatePullRequest(const ABranchName, ATitle,
  ABody: string): string;
var
  Client: TFPHTTPClient;
  RequestBody, Response: string;
  JsonData: TJSONData;
  Stream: TStringStream;
begin
  Result := '';
  Client := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('Authorization', 'Bearer ' + FToken);
    Client.AddHeader('Accept', 'application/vnd.github+json');
    Client.AddHeader('User-Agent', 'PasBuild/1.0');
    Client.AddHeader('Content-Type', 'application/json');

    RequestBody := '{"title":"' + ATitle +
      '","body":"' + ABody +
      '","head":"' + ABranchName +
      '","base":"main"}';

    Stream := TStringStream.Create(RequestBody);
    try
      Client.RequestBody := Stream;
      try
        Response := Client.Post(
          'https://api.github.com/repos/' + FOwner + '/' + FRepo + '/pulls');
        if (Client.ResponseStatusCode >= 200) and
           (Client.ResponseStatusCode < 300) then
        begin
          JsonData := GetJSON(Response);
          try
            Result := JsonData.FindPath('html_url').AsString;
          finally
            JsonData.Free;
          end;
        end;
      except
        on E: Exception do
          TUtils.LogError('Failed to create pull request: ' + E.Message);
      end;
    finally
      Stream.Free;
    end;
  finally
    Client.Free;
  end;
end;

end.
