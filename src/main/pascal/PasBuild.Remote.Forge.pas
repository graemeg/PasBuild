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

  { Automates the fork -> branch -> commit -> pull-request workflow against
    the GitHub REST API. All branch and file operations happen on the
    authenticated user's fork; only the pull request targets the upstream
    registry repository. }
  TGitHubForge = class
  private
    FToken: string;
    FOwner: string;         // upstream registry owner
    FRepo: string;
    FForkOwner: string;     // authenticated user; set by ForkRepository
    FDefaultBranch: string; // upstream default branch, cached
    function ApiRequest(const AMethod, AUrl, ABody: string;
      out AResponse: string): Integer;
  public
    constructor Create(const AToken, AOwner, ARepo: string);

    { Request-body builders, exposed for unit testing. Bodies are built
      with fpjson so quoting/newline escaping is always correct. }
    class function BuildCreateRefBody(const ABranchName, ASha: string): string;
    class function BuildContentsBody(const AMessage, AContent, ABranch,
      AExistingSha: string): string;
    class function BuildPullRequestBody(const ATitle, ABody, AHead,
      ABase: string): string;

    function GetAuthenticatedUser: string;
    function GetDefaultBranch: string;
    { Fork the upstream repo to the authenticated user's account and wait
      until the fork is ready. No-op (but still resolves the user) when
      the authenticated user owns the upstream repository. }
    function ForkRepository: Boolean;
    function CreateBranch(const ABranchName: string): Boolean;
    function CreateOrUpdateFile(const ABranchName, AFilePath,
      AContent, AMessage: string): Boolean;
    function CreatePullRequest(const ABranchName, ATitle, ABody: string): string;

    property Token: string read FToken;
    property Owner: string read FOwner;
    property Repo: string read FRepo;
    property ForkOwner: string read FForkOwner;
  end;

implementation

uses
  fphttpclient, ssockets, fpjson, jsonparser, base64, opensslsockets,
  PasBuild.Utils,
  PasBuild.Remote;

const
  GITHUB_API = 'https://api.github.com';
  FORK_POLL_ATTEMPTS = 10;
  FORK_POLL_DELAY_MS = 1500;

{ Extract the host component of a URL (no scheme, path or port). }
function ExtractUrlHost(const AUrl: string): string;
var
  P: Integer;
begin
  Result := AUrl;
  P := Pos('://', Result);
  if P > 0 then
    Delete(Result, 1, P + 2);
  P := Pos('/', Result);
  if P > 0 then
    Result := Copy(Result, 1, P - 1);
  P := Pos(':', Result);
  if P > 0 then
    Result := Copy(Result, 1, P - 1);
  Result := LowerCase(Result);
end;

function HostMatches(const AHost, ADomain: string): Boolean;
begin
  Result := (AHost = ADomain) or
    ((Length(AHost) > Length(ADomain)) and
     (Copy(AHost, Length(AHost) - Length(ADomain), Length(ADomain) + 1) =
      '.' + ADomain));
end;

{ Parse AJson and return the string at APath. False when the document is
  not valid JSON or the path is absent - never dereferences nil. }
function TryGetJsonString(const AJson, APath: string;
  out AValue: string): Boolean;
var
  Doc, Node: TJSONData;
begin
  Result := False;
  AValue := '';
  if AJson = '' then
    Exit;
  try
    Doc := GetJSON(AJson);
  except
    Exit;
  end;
  try
    Node := Doc.FindPath(APath);
    if Assigned(Node) and not Node.IsNull then
    begin
      AValue := Node.AsString;
      Result := True;
    end;
  finally
    Doc.Free;
  end;
end;

{ TForgeDetector }

class function TForgeDetector.DetectForge(const AUrl: string): TForgeType;
var
  Host: string;
begin
  Host := ExtractUrlHost(AUrl);

  if (Host = 'github.com') or (Host = 'raw.githubusercontent.com') or
     HostMatches(Host, 'github.com') or
     HostMatches(Host, 'githubusercontent.com') then
    Exit(ftGitHub);

  if (Host = 'codeberg.org') or HostMatches(Host, 'codeberg.org') or
     (Copy(Host, 1, 6) = 'gitea.') then
    Exit(ftGitea);

  if (Host = 'sr.ht') or HostMatches(Host, 'sr.ht') then
    Exit(ftSourceHut);

  Result := ftUnknown;
end;

class procedure TForgeDetector.ParseGitHubRepo(const AUrl: string;
  out AOwner, ARepo: string);
var
  Path, CleanUrl: string;
  SlashPos: Integer;
begin
  AOwner := '';
  ARepo := '';

  CleanUrl := AUrl;
  if (CleanUrl <> '') and (CleanUrl[Length(CleanUrl)] = '/') then
    Delete(CleanUrl, Length(CleanUrl), 1);
  if CleanUrl = '' then
    Exit;

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

  if (Path <> '') and (Path[Length(Path)] = '/') then
    Delete(Path, Length(Path), 1);
  if Path = '' then
    Exit;

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
  FForkOwner := '';
  FDefaultBranch := '';
end;

class function TGitHubForge.BuildCreateRefBody(const ABranchName,
  ASha: string): string;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('ref', 'refs/heads/' + ABranchName);
    Obj.Add('sha', ASha);
    Result := Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

class function TGitHubForge.BuildContentsBody(const AMessage, AContent,
  ABranch, AExistingSha: string): string;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('message', AMessage);
    Obj.Add('content', EncodeStringBase64(AContent));
    Obj.Add('branch', ABranch);
    if AExistingSha <> '' then
      Obj.Add('sha', AExistingSha);
    Result := Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

class function TGitHubForge.BuildPullRequestBody(const ATitle, ABody, AHead,
  ABase: string): string;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('title', ATitle);
    Obj.Add('body', ABody);
    Obj.Add('head', AHead);
    Obj.Add('base', ABase);
    Result := Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

function TGitHubForge.ApiRequest(const AMethod, AUrl, ABody: string;
  out AResponse: string): Integer;
var
  Client: TFPHTTPClient;
  BodyStream: TStringStream;
  RespStream: TStringStream;
begin
  Result := 0;
  AResponse := '';

  Client := TFPHTTPClient.Create(nil);
  BodyStream := nil;
  RespStream := TStringStream.Create('');
  try
    Client.AddHeader('Authorization', 'Bearer ' + FToken);
    Client.AddHeader('Accept', 'application/vnd.github+json');
    Client.AddHeader('User-Agent', 'PasBuild/1.0');
    Client.AllowRedirect := True;
    // The Authorization header carries the user's token; without peer
    // verification an on-path attacker could intercept it
    AttachTlsVerification(Client);

    if ABody <> '' then
    begin
      Client.AddHeader('Content-Type', 'application/json');
      BodyStream := TStringStream.Create(ABody);
      Client.RequestBody := BodyStream;
    end;

    try
      // Empty allowed-codes list: never raise on HTTP status, so the
      // caller always gets the real status code and error body
      Client.HTTPMethod(AMethod, AUrl, RespStream, []);
      Result := Client.ResponseStatusCode;
      AResponse := RespStream.DataString;
    except
      on E: ESocketError do
        TUtils.LogError('GitHub API connection failed: ' + E.Message);
      on E: EHTTPClient do
      begin
        Result := Client.ResponseStatusCode;
        AResponse := RespStream.DataString;
        if Result = 0 then
          TUtils.LogError('GitHub API request failed: ' + E.Message);
      end;
    end;
  finally
    RespStream.Free;
    BodyStream.Free;
    Client.Free;
  end;
end;

function TGitHubForge.GetAuthenticatedUser: string;
var
  Status: Integer;
  Response: string;
begin
  Result := '';
  Status := ApiRequest('GET', GITHUB_API + '/user', '', Response);
  if Status <> 200 then
  begin
    TUtils.LogError(Format(
      'Cannot resolve the authenticated GitHub user (HTTP %d). ' +
      'Check that the token is valid.', [Status]));
    Exit;
  end;
  if not TryGetJsonString(Response, 'login', Result) then
    TUtils.LogError('Unexpected response from GitHub /user endpoint.');
end;

function TGitHubForge.GetDefaultBranch: string;
var
  Status: Integer;
  Response: string;
begin
  if FDefaultBranch <> '' then
    Exit(FDefaultBranch);

  Result := '';
  Status := ApiRequest('GET',
    GITHUB_API + '/repos/' + FOwner + '/' + FRepo, '', Response);
  if Status <> 200 then
  begin
    TUtils.LogError(Format(
      'Cannot read repository %s/%s (HTTP %d).', [FOwner, FRepo, Status]));
    Exit;
  end;
  if TryGetJsonString(Response, 'default_branch', FDefaultBranch) then
    Result := FDefaultBranch
  else
    TUtils.LogError('Unexpected repository response from GitHub API.');
end;

function TGitHubForge.ForkRepository: Boolean;
var
  Status, Attempt: Integer;
  Response: string;
begin
  Result := False;

  FForkOwner := GetAuthenticatedUser;
  if FForkOwner = '' then
    Exit;

  // Publishing to a registry the user owns: no fork involved
  if SameText(FForkOwner, FOwner) then
    Exit(True);

  Status := ApiRequest('POST',
    GITHUB_API + '/repos/' + FOwner + '/' + FRepo + '/forks', '', Response);
  // 202 = fork queued (asynchronous); 200/201 possible when it already exists
  if (Status < 200) or (Status >= 300) then
  begin
    TUtils.LogError(Format('Fork request failed (HTTP %d): %s',
      [Status, Response]));
    Exit;
  end;

  // Forking is asynchronous - poll until the fork repository responds
  for Attempt := 1 to FORK_POLL_ATTEMPTS do
  begin
    Status := ApiRequest('GET',
      GITHUB_API + '/repos/' + FForkOwner + '/' + FRepo, '', Response);
    if Status = 200 then
      Exit(True);
    Sleep(FORK_POLL_DELAY_MS);
  end;

  TUtils.LogError(Format(
    'Fork of %s/%s did not become ready in time. Retry in a moment.',
    [FOwner, FRepo]));
end;

function TGitHubForge.CreateBranch(const ABranchName: string): Boolean;
var
  Status: Integer;
  Response, BaseBranch, BaseSha, BranchOwner: string;
begin
  Result := False;

  BranchOwner := FForkOwner;
  if BranchOwner = '' then
    BranchOwner := FOwner;

  BaseBranch := GetDefaultBranch;
  if BaseBranch = '' then
    Exit;

  Status := ApiRequest('GET',
    GITHUB_API + '/repos/' + BranchOwner + '/' + FRepo +
    '/git/ref/heads/' + BaseBranch, '', Response);
  if Status <> 200 then
  begin
    TUtils.LogError(Format(
      'Cannot read %s branch of %s/%s (HTTP %d).',
      [BaseBranch, BranchOwner, FRepo, Status]));
    Exit;
  end;
  if not TryGetJsonString(Response, 'object.sha', BaseSha) then
  begin
    TUtils.LogError('Unexpected git-ref response from GitHub API.');
    Exit;
  end;

  Status := ApiRequest('POST',
    GITHUB_API + '/repos/' + BranchOwner + '/' + FRepo + '/git/refs',
    BuildCreateRefBody(ABranchName, BaseSha), Response);

  if (Status >= 200) and (Status < 300) then
    Exit(True);

  // 422: reference already exists - reuse it (e.g. a retried publish)
  if Status = 422 then
  begin
    Status := ApiRequest('GET',
      GITHUB_API + '/repos/' + BranchOwner + '/' + FRepo +
      '/git/ref/heads/' + ABranchName, '', Response);
    if Status = 200 then
    begin
      TUtils.LogWarning('Branch ' + ABranchName + ' already exists; reusing it.');
      Exit(True);
    end;
  end;

  TUtils.LogError(Format('Cannot create branch %s (HTTP %d): %s',
    [ABranchName, Status, Response]));
end;

function TGitHubForge.CreateOrUpdateFile(const ABranchName, AFilePath,
  AContent, AMessage: string): Boolean;
var
  Status: Integer;
  Response, ExistingSha, FileOwner: string;
begin
  Result := False;

  FileOwner := FForkOwner;
  if FileOwner = '' then
    FileOwner := FOwner;

  // The contents API requires the current blob sha when updating an
  // existing file (404 simply means the file is new)
  ExistingSha := '';
  Status := ApiRequest('GET',
    GITHUB_API + '/repos/' + FileOwner + '/' + FRepo + '/contents/' +
    AFilePath + '?ref=' + ABranchName, '', Response);
  if Status = 200 then
    TryGetJsonString(Response, 'sha', ExistingSha);

  Status := ApiRequest('PUT',
    GITHUB_API + '/repos/' + FileOwner + '/' + FRepo + '/contents/' +
    AFilePath,
    BuildContentsBody(AMessage, AContent, ABranchName, ExistingSha),
    Response);

  if (Status >= 200) and (Status < 300) then
    Exit(True);

  TUtils.LogError(Format('Cannot upload %s (HTTP %d): %s',
    [AFilePath, Status, Response]));
end;

function TGitHubForge.CreatePullRequest(const ABranchName, ATitle,
  ABody: string): string;
var
  Status: Integer;
  Response, Head, BaseBranch: string;
begin
  Result := '';

  BaseBranch := GetDefaultBranch;
  if BaseBranch = '' then
    Exit;

  // Cross-fork pull requests must qualify the head with the fork owner
  if (FForkOwner <> '') and (not SameText(FForkOwner, FOwner)) then
    Head := FForkOwner + ':' + ABranchName
  else
    Head := ABranchName;

  Status := ApiRequest('POST',
    GITHUB_API + '/repos/' + FOwner + '/' + FRepo + '/pulls',
    BuildPullRequestBody(ATitle, ABody, Head, BaseBranch), Response);

  if (Status >= 200) and (Status < 300) then
  begin
    if not TryGetJsonString(Response, 'html_url', Result) then
      TUtils.LogWarning('Pull request created but no URL was returned.');
  end
  else
    TUtils.LogError(Format('Cannot create pull request (HTTP %d): %s',
      [Status, Response]));
end;

end.
