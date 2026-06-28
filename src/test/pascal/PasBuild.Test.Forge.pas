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
  Classes, SysUtils, fpcunit, testregistry,
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

initialization
  RegisterTest(TTestForgeDetection);
  RegisterTest(TTestParseRepoFromUrl);
  RegisterTest(TTestForgeTokenLookup);
  RegisterTest(TTestForgeBranchName);

end.
