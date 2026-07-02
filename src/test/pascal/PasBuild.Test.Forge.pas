{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Forge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser, base64,
  PasBuild.Remote.Forge;

type
  TTestForgeDetection = class(TTestCase)
  published
    procedure TestDetectGitHubFromUrl;
    procedure TestDetectGitHubRawContent;
    procedure TestDetectGiteaCodeberg;
    procedure TestDetectSourceHut;
    procedure TestDetectUnknown;
    procedure TestDetectEmptyUrl;
  end;

  TTestParseRepoFromUrl = class(TTestCase)
  published
    procedure TestParseGitHubRepoUrl;
    procedure TestParseGitHubRawUrl;
    procedure TestParseGiteaUrl;
    procedure TestParseUnknownReturnsEmpty;
  end;

  TTestForgeTokenLookup = class(TTestCase)
  published
    procedure TestGitHubTokenEnvVarName;
    procedure TestGiteaTokenEnvVarName;
    procedure TestUnknownForgeTokenEmpty;
  end;

  TTestForgeBranchName = class(TTestCase)
  published
    procedure TestBranchNameFormat;
    procedure TestBranchNameWithDots;
  end;

  { Forge detection must match the URL host, not substrings anywhere in
    the URL - github.com.evil.io must not be treated as GitHub }
  TTestForgeHostMatching = class(TTestCase)
  published
    procedure TestLookalikeHostNotGitHub;
    procedure TestForgeNameInPathNotMatched;
    procedure TestSelfHostedGiteaPrefixStillMatches;
    procedure TestParseGitHubEmptyUrlSafe;
    procedure TestParseGiteaEmptyUrlSafe;
  end;

  { GitHub API request bodies must be well-formed JSON with base64 file
    content - built via fpjson, never by string concatenation }
  TTestForgeRequestBodies = class(TTestCase)
  published
    procedure TestCreateRefBody;
    procedure TestContentsBodyEncodesContentAsBase64;
    procedure TestContentsBodyOmitsShaWhenCreating;
    procedure TestContentsBodyIncludesShaWhenUpdating;
    procedure TestPullRequestBodyEscapesNewlines;
    procedure TestPullRequestBodyHeadAndBase;
  end;

implementation

{ TTestForgeDetection }

procedure TTestForgeDetection.TestDetectGitHubFromUrl;
begin
  AssertEquals('GitHub URL',
    Ord(ftGitHub),
    Ord(TForgeDetector.DetectForge('https://github.com/pasbuild/registry')));
end;

procedure TTestForgeDetection.TestDetectGitHubRawContent;
begin
  AssertEquals('GitHub raw URL',
    Ord(ftGitHub),
    Ord(TForgeDetector.DetectForge('https://raw.githubusercontent.com/pasbuild/registry/main/')));
end;

procedure TTestForgeDetection.TestDetectGiteaCodeberg;
begin
  AssertEquals('Codeberg URL',
    Ord(ftGitea),
    Ord(TForgeDetector.DetectForge('https://codeberg.org/pasbuild/registry')));
end;

procedure TTestForgeDetection.TestDetectSourceHut;
begin
  AssertEquals('SourceHut URL',
    Ord(ftSourceHut),
    Ord(TForgeDetector.DetectForge('https://git.sr.ht/~user/registry')));
end;

procedure TTestForgeDetection.TestDetectUnknown;
begin
  AssertEquals('Unknown URL',
    Ord(ftUnknown),
    Ord(TForgeDetector.DetectForge('https://example.com/registry')));
end;

procedure TTestForgeDetection.TestDetectEmptyUrl;
begin
  AssertEquals('Empty URL',
    Ord(ftUnknown),
    Ord(TForgeDetector.DetectForge('')));
end;

{ TTestParseRepoFromUrl }

procedure TTestParseRepoFromUrl.TestParseGitHubRepoUrl;
var
  Owner, Repo: string;
begin
  TForgeDetector.ParseGitHubRepo(
    'https://github.com/pasbuild/registry', Owner, Repo);
  AssertEquals('Owner', 'pasbuild', Owner);
  AssertEquals('Repo', 'registry', Repo);
end;

procedure TTestParseRepoFromUrl.TestParseGitHubRawUrl;
var
  Owner, Repo: string;
begin
  TForgeDetector.ParseGitHubRepo(
    'https://raw.githubusercontent.com/pasbuild/registry/main/', Owner, Repo);
  AssertEquals('Owner', 'pasbuild', Owner);
  AssertEquals('Repo', 'registry', Repo);
end;

procedure TTestParseRepoFromUrl.TestParseGiteaUrl;
var
  Owner, Repo: string;
begin
  TForgeDetector.ParseGiteaRepo(
    'https://codeberg.org/pasbuild/registry', Owner, Repo);
  AssertEquals('Owner', 'pasbuild', Owner);
  AssertEquals('Repo', 'registry', Repo);
end;

procedure TTestParseRepoFromUrl.TestParseUnknownReturnsEmpty;
var
  Owner, Repo: string;
begin
  TForgeDetector.ParseGitHubRepo(
    'https://example.com/something', Owner, Repo);
  AssertEquals('Owner empty', '', Owner);
  AssertEquals('Repo empty', '', Repo);
end;

{ TTestForgeTokenLookup }

procedure TTestForgeTokenLookup.TestGitHubTokenEnvVarName;
begin
  AssertEquals('GitHub token env var',
    'PASBUILD_GITHUB_TOKEN',
    TForgeDetector.GetTokenEnvVar(ftGitHub));
end;

procedure TTestForgeTokenLookup.TestGiteaTokenEnvVarName;
begin
  AssertEquals('Gitea token env var',
    'PASBUILD_GITEA_TOKEN',
    TForgeDetector.GetTokenEnvVar(ftGitea));
end;

procedure TTestForgeTokenLookup.TestUnknownForgeTokenEmpty;
begin
  AssertEquals('Unknown forge token env var',
    '',
    TForgeDetector.GetTokenEnvVar(ftUnknown));
end;

{ TTestForgeBranchName }

procedure TTestForgeBranchName.TestBranchNameFormat;
begin
  AssertEquals('Branch name',
    'publish/mylib-1.0.0',
    TForgeDetector.GetPublishBranchName('mylib', '1.0.0'));
end;

procedure TTestForgeBranchName.TestBranchNameWithDots;
begin
  AssertEquals('Branch name with dots',
    'publish/my.lib-2.3.1',
    TForgeDetector.GetPublishBranchName('my.lib', '2.3.1'));
end;

{ TTestForgeHostMatching }

procedure TTestForgeHostMatching.TestLookalikeHostNotGitHub;
begin
  AssertEquals('Lookalike host must not match GitHub',
    Ord(ftUnknown),
    Ord(TForgeDetector.DetectForge('https://github.com.evil.io/pasbuild/registry')));
end;

procedure TTestForgeHostMatching.TestForgeNameInPathNotMatched;
begin
  AssertEquals('Forge name in the path must not match',
    Ord(ftUnknown),
    Ord(TForgeDetector.DetectForge('https://evil.io/github.com/registry')));
end;

procedure TTestForgeHostMatching.TestSelfHostedGiteaPrefixStillMatches;
begin
  AssertEquals('Self-hosted gitea.* host should match Gitea',
    Ord(ftGitea),
    Ord(TForgeDetector.DetectForge('https://gitea.mycorp.example/pasbuild/registry')));
end;

procedure TTestForgeHostMatching.TestParseGitHubEmptyUrlSafe;
var
  Owner, Repo: string;
begin
  TForgeDetector.ParseGitHubRepo('', Owner, Repo);
  AssertEquals('Owner empty', '', Owner);
  AssertEquals('Repo empty', '', Repo);
end;

procedure TTestForgeHostMatching.TestParseGiteaEmptyUrlSafe;
var
  Owner, Repo: string;
begin
  TForgeDetector.ParseGiteaRepo('', Owner, Repo);
  AssertEquals('Owner empty', '', Owner);
  AssertEquals('Repo empty', '', Repo);
end;

{ TTestForgeRequestBodies }

procedure TTestForgeRequestBodies.TestCreateRefBody;
var
  Json: TJSONData;
begin
  Json := GetJSON(TGitHubForge.BuildCreateRefBody('publish/mylib-1.0.0',
    'abc123def'));
  try
    AssertEquals('ref', 'refs/heads/publish/mylib-1.0.0',
      Json.FindPath('ref').AsString);
    AssertEquals('sha', 'abc123def', Json.FindPath('sha').AsString);
  finally
    Json.Free;
  end;
end;

procedure TTestForgeRequestBodies.TestContentsBodyEncodesContentAsBase64;
var
  Json: TJSONData;
  Content: string;
begin
  Content := '<?xml version="1.0"?>' + LineEnding + '<release/>';
  Json := GetJSON(TGitHubForge.BuildContentsBody(
    'publish: mylib 1.0.0', Content, 'publish/mylib-1.0.0', ''));
  try
    AssertEquals('content must be base64 of the file body',
      EncodeStringBase64(Content), Json.FindPath('content').AsString);
    AssertEquals('message', 'publish: mylib 1.0.0',
      Json.FindPath('message').AsString);
    AssertEquals('branch', 'publish/mylib-1.0.0',
      Json.FindPath('branch').AsString);
  finally
    Json.Free;
  end;
end;

procedure TTestForgeRequestBodies.TestContentsBodyOmitsShaWhenCreating;
var
  Json: TJSONData;
begin
  Json := GetJSON(TGitHubForge.BuildContentsBody('msg', 'data', 'branch', ''));
  try
    AssertNull('sha must be absent when creating a new file',
      Json.FindPath('sha'));
  finally
    Json.Free;
  end;
end;

procedure TTestForgeRequestBodies.TestContentsBodyIncludesShaWhenUpdating;
var
  Json: TJSONData;
begin
  // The GitHub contents API requires the existing blob sha when updating
  Json := GetJSON(TGitHubForge.BuildContentsBody('msg', 'data', 'branch',
    'existingsha123'));
  try
    AssertEquals('sha', 'existingsha123', Json.FindPath('sha').AsString);
  finally
    Json.Free;
  end;
end;

procedure TTestForgeRequestBodies.TestPullRequestBodyEscapesNewlines;
var
  Raw: string;
  Json: TJSONData;
  Body: string;
begin
  Body := 'Line one.' + LineEnding + LineEnding + 'Line two.';
  Raw := TGitHubForge.BuildPullRequestBody('title', Body, 'user:branch', 'main');

  // Raw newlines are illegal inside JSON strings; fpjson must escape them
  AssertEquals('No raw LF in serialized JSON', 0, Pos(#10, Raw));
  AssertEquals('No raw CR in serialized JSON', 0, Pos(#13, Raw));

  Json := GetJSON(Raw);
  try
    AssertEquals('Body must round-trip', Body,
      Json.FindPath('body').AsString);
  finally
    Json.Free;
  end;
end;

procedure TTestForgeRequestBodies.TestPullRequestBodyHeadAndBase;
var
  Json: TJSONData;
begin
  Json := GetJSON(TGitHubForge.BuildPullRequestBody(
    'publish: mylib 1.0.0', 'body', 'someuser:publish/mylib-1.0.0', 'main'));
  try
    AssertEquals('Cross-fork head must be user:branch',
      'someuser:publish/mylib-1.0.0', Json.FindPath('head').AsString);
    AssertEquals('base', 'main', Json.FindPath('base').AsString);
    AssertEquals('title', 'publish: mylib 1.0.0',
      Json.FindPath('title').AsString);
  finally
    Json.Free;
  end;
end;

initialization
  RegisterTest(TTestForgeDetection);
  RegisterTest(TTestParseRepoFromUrl);
  RegisterTest(TTestForgeTokenLookup);
  RegisterTest(TTestForgeBranchName);
  RegisterTest(TTestForgeHostMatching);
  RegisterTest(TTestForgeRequestBodies);

end.
