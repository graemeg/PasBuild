{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Types.Remote;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Types;

type
  TTestRepositoryInfo = class(TTestCase)
  published
    procedure TestCreateWithIdAndUrl;
    procedure TestPropertiesAreWritable;
  end;

  TTestRepositoryList = class(TTestCase)
  published
    procedure TestCreateEmpty;
    procedure TestAddMultipleRepositories;
    procedure TestFreeObjectsOwnership;
  end;

  TTestProjectConfigRemoteFields = class(TTestCase)
  private
    FConfig: TProjectConfig;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescriptionDefaultEmpty;
    procedure TestDescriptionSetAndGet;
    procedure TestKeywordsListExists;
    procedure TestKeywordsAddMultiple;
    procedure TestKeywordsDuplicatesIgnored;
    procedure TestRepositoriesListExists;
    procedure TestRepositoriesAddRepository;
  end;

implementation

{ TTestRepositoryInfo }

procedure TTestRepositoryInfo.TestCreateWithIdAndUrl;
var
  Info: TRepositoryInfo;
begin
  Info := TRepositoryInfo.Create('central',
    'https://raw.githubusercontent.com/pasbuild/registry/main/');
  try
    AssertEquals('Id', 'central', Info.Id);
    AssertEquals('Url',
      'https://raw.githubusercontent.com/pasbuild/registry/main/',
      Info.Url);
  finally
    Info.Free;
  end;
end;

procedure TTestRepositoryInfo.TestPropertiesAreWritable;
var
  Info: TRepositoryInfo;
begin
  Info := TRepositoryInfo.Create('old-id', 'http://old-url/');
  try
    Info.Id := 'new-id';
    Info.Url := 'https://new-url/';
    AssertEquals('Id updated', 'new-id', Info.Id);
    AssertEquals('Url updated', 'https://new-url/', Info.Url);
  finally
    Info.Free;
  end;
end;

{ TTestRepositoryList }

procedure TTestRepositoryList.TestCreateEmpty;
var
  List: TRepositoryList;
begin
  List := TRepositoryList.Create;
  try
    AssertEquals('Empty list count', 0, List.Count);
  finally
    List.Free;
  end;
end;

procedure TTestRepositoryList.TestAddMultipleRepositories;
var
  List: TRepositoryList;
begin
  List := TRepositoryList.Create;
  List.FreeObjects := True;
  try
    List.Add(TRepositoryInfo.Create('internal', 'https://git.company.com/'));
    List.Add(TRepositoryInfo.Create('central', 'https://github.com/'));
    AssertEquals('Count after adding', 2, List.Count);
    AssertEquals('First id', 'internal', List[0].Id);
    AssertEquals('Second id', 'central', List[1].Id);
  finally
    List.Free;
  end;
end;

procedure TTestRepositoryList.TestFreeObjectsOwnership;
var
  List: TRepositoryList;
begin
  List := TRepositoryList.Create;
  List.FreeObjects := True;
  try
    List.Add(TRepositoryInfo.Create('test', 'https://example.com/'));
    AssertEquals('Has item', 1, List.Count);
  finally
    List.Free;
  end;
end;

{ TTestProjectConfigRemoteFields }

procedure TTestProjectConfigRemoteFields.SetUp;
begin
  FConfig := TProjectConfig.Create;
end;

procedure TTestProjectConfigRemoteFields.TearDown;
begin
  FConfig.Free;
end;

procedure TTestProjectConfigRemoteFields.TestDescriptionDefaultEmpty;
begin
  AssertEquals('Default description empty', '', FConfig.Description);
end;

procedure TTestProjectConfigRemoteFields.TestDescriptionSetAndGet;
begin
  FConfig.Description := 'Cross-platform GUI toolkit for Free Pascal';
  AssertEquals('Description set',
    'Cross-platform GUI toolkit for Free Pascal', FConfig.Description);
end;

procedure TTestProjectConfigRemoteFields.TestKeywordsListExists;
begin
  AssertNotNull('Keywords list created', FConfig.Keywords);
end;

procedure TTestProjectConfigRemoteFields.TestKeywordsAddMultiple;
begin
  FConfig.Keywords.Add('gui');
  FConfig.Keywords.Add('toolkit');
  FConfig.Keywords.Add('cross-platform');
  AssertEquals('Keywords count', 3, FConfig.Keywords.Count);
end;

procedure TTestProjectConfigRemoteFields.TestKeywordsDuplicatesIgnored;
begin
  FConfig.Keywords.Add('gui');
  FConfig.Keywords.Add('gui');
  AssertEquals('Duplicate ignored', 1, FConfig.Keywords.Count);
end;

procedure TTestProjectConfigRemoteFields.TestRepositoriesListExists;
begin
  AssertNotNull('Repositories list created', FConfig.Repositories);
end;

procedure TTestProjectConfigRemoteFields.TestRepositoriesAddRepository;
begin
  FConfig.Repositories.Add(TRepositoryInfo.Create('central',
    'https://raw.githubusercontent.com/pasbuild/registry/main/'));
  AssertEquals('Repository added', 1, FConfig.Repositories.Count);
  AssertEquals('Repository id', 'central', FConfig.Repositories[0].Id);
end;

initialization
  RegisterTest(TTestRepositoryInfo);
  RegisterTest(TTestRepositoryList);
  RegisterTest(TTestProjectConfigRemoteFields);

end.
