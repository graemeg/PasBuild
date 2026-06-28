{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Config.ProjectMetadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Types,
  PasBuild.Config;

type
  TTestParseDescription = class(TTestCase)
  private
    function GetFixturePath(const AFileName: string): string;
  published
    procedure TestDescriptionParsed;
    procedure TestDescriptionDefaultEmpty;
  end;

  TTestParseKeywords = class(TTestCase)
  private
    function GetFixturePath(const AFileName: string): string;
  published
    procedure TestKeywordsParsed;
    procedure TestKeywordsCount;
    procedure TestKeywordsAbsentReturnsEmpty;
  end;

  TTestParseRepositories = class(TTestCase)
  private
    function GetFixturePath(const AFileName: string): string;
  published
    procedure TestRepositoriesParsed;
    procedure TestRepositoryOrder;
    procedure TestRepositoriesAbsentReturnsEmpty;
  end;

  TTestParseFullMetadata = class(TTestCase)
  private
    function GetFixturePath(const AFileName: string): string;
  published
    procedure TestFullMetadataAllFieldsParsed;
  end;

implementation

{ TTestParseDescription }

function TTestParseDescription.GetFixturePath(const AFileName: string): string;
begin
  Result := 'fixtures/project-metadata/' + AFileName;
end;

procedure TTestParseDescription.TestDescriptionParsed;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('description-only.xml'));
  try
    AssertEquals('Description parsed',
      'Cross-platform GUI toolkit for Free Pascal', Config.Description);
  finally
    Config.Free;
  end;
end;

procedure TTestParseDescription.TestDescriptionDefaultEmpty;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('no-metadata.xml'));
  try
    AssertEquals('Description default empty', '', Config.Description);
  finally
    Config.Free;
  end;
end;

{ TTestParseKeywords }

function TTestParseKeywords.GetFixturePath(const AFileName: string): string;
begin
  Result := 'fixtures/project-metadata/' + AFileName;
end;

procedure TTestParseKeywords.TestKeywordsParsed;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('keywords.xml'));
  try
    AssertTrue('Has gui keyword', Config.Keywords.IndexOf('gui') >= 0);
    AssertTrue('Has toolkit keyword', Config.Keywords.IndexOf('toolkit') >= 0);
    AssertTrue('Has cross-platform keyword',
      Config.Keywords.IndexOf('cross-platform') >= 0);
  finally
    Config.Free;
  end;
end;

procedure TTestParseKeywords.TestKeywordsCount;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('keywords.xml'));
  try
    AssertEquals('Three keywords', 3, Config.Keywords.Count);
  finally
    Config.Free;
  end;
end;

procedure TTestParseKeywords.TestKeywordsAbsentReturnsEmpty;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('no-metadata.xml'));
  try
    AssertEquals('No keywords', 0, Config.Keywords.Count);
  finally
    Config.Free;
  end;
end;

{ TTestParseRepositories }

function TTestParseRepositories.GetFixturePath(const AFileName: string): string;
begin
  Result := 'fixtures/project-metadata/' + AFileName;
end;

procedure TTestParseRepositories.TestRepositoriesParsed;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('repositories.xml'));
  try
    AssertEquals('Two repositories', 2, Config.Repositories.Count);
    AssertEquals('First id', 'company-internal', Config.Repositories[0].Id);
    AssertEquals('First url',
      'https://git.company.com/pasbuild-registry/raw/branch/main/',
      Config.Repositories[0].Url);
    AssertEquals('Second id', 'central', Config.Repositories[1].Id);
  finally
    Config.Free;
  end;
end;

procedure TTestParseRepositories.TestRepositoryOrder;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('repositories.xml'));
  try
    AssertEquals('First repo is company-internal',
      'company-internal', Config.Repositories[0].Id);
    AssertEquals('Second repo is central',
      'central', Config.Repositories[1].Id);
  finally
    Config.Free;
  end;
end;

procedure TTestParseRepositories.TestRepositoriesAbsentReturnsEmpty;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('no-metadata.xml'));
  try
    AssertEquals('No repositories', 0, Config.Repositories.Count);
  finally
    Config.Free;
  end;
end;

{ TTestParseFullMetadata }

function TTestParseFullMetadata.GetFixturePath(const AFileName: string): string;
begin
  Result := 'fixtures/project-metadata/' + AFileName;
end;

procedure TTestParseFullMetadata.TestFullMetadataAllFieldsParsed;
var
  Config: TProjectConfig;
begin
  Config := TConfigLoader.LoadProjectXML(GetFixturePath('full-metadata.xml'));
  try
    AssertEquals('Name', 'fpgui-framework', Config.Name);
    AssertEquals('Version', '2.0.0', Config.Version);
    AssertEquals('Description',
      'Cross-platform GUI toolkit for Free Pascal', Config.Description);
    AssertEquals('Keywords count', 3, Config.Keywords.Count);
    AssertEquals('Repositories count', 1, Config.Repositories.Count);
    AssertEquals('Repository id', 'internal', Config.Repositories[0].Id);
  finally
    Config.Free;
  end;
end;

initialization
  RegisterTest(TTestParseDescription);
  RegisterTest(TTestParseKeywords);
  RegisterTest(TTestParseRepositories);
  RegisterTest(TTestParseFullMetadata);

end.
