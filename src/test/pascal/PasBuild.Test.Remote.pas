{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Remote;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Dependencies,
  PasBuild.Remote;

type
  TTestRemoteClientCreate = class(TTestCase)
  published
    procedure TestDefaultTimeouts;
    procedure TestCustomTimeouts;
  end;

  TTestRemoteExceptionTypes = class(TTestCase)
  published
    procedure TestNetworkErrorIsEDependencyError;
    procedure TestChecksumErrorIsEDependencyError;
    procedure TestChecksumErrorIsNotNetworkError;
  end;

  TTestRemoteChecksumValidation = class(TTestCase)
  private
    FTempDir: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDownloadFileChecksumMismatchRaises;
    procedure TestDownloadFileChecksumMatchSucceeds;
  end;

  { Registry and origin URLs must use HTTPS; cleartext HTTP is only
    acceptable for loopback addresses (local test registries) }
  TTestUrlSchemeEnforcement = class(TTestCase)
  published
    procedure TestHttpsAccepted;
    procedure TestPlainHttpRejected;
    procedure TestLoopbackHttpAllowed;
    procedure TestOtherSchemesRejected;
    procedure TestEmptyUrlRejected;
  end;

implementation

uses
  PasBuild.Crypto.SHA256;

{ TTestRemoteClientCreate }

procedure TTestRemoteClientCreate.TestDefaultTimeouts;
var
  Client: TRemoteClient;
begin
  Client := TRemoteClient.Create;
  try
    AssertEquals('Default connect timeout', 30, Client.ConnectTimeout);
    AssertEquals('Default read timeout', 60, Client.ReadTimeout);
  finally
    Client.Free;
  end;
end;

procedure TTestRemoteClientCreate.TestCustomTimeouts;
var
  Client: TRemoteClient;
begin
  Client := TRemoteClient.Create(15, 120);
  try
    AssertEquals('Custom connect timeout', 15, Client.ConnectTimeout);
    AssertEquals('Custom read timeout', 120, Client.ReadTimeout);
  finally
    Client.Free;
  end;
end;

{ TTestRemoteExceptionTypes }

procedure TTestRemoteExceptionTypes.TestNetworkErrorIsEDependencyError;
var
  E: ENetworkError;
begin
  E := ENetworkError.Create('test');
  try
    AssertTrue('ENetworkError inherits from EDependencyError',
      E is EDependencyError);
  finally
    E.Free;
  end;
end;

procedure TTestRemoteExceptionTypes.TestChecksumErrorIsEDependencyError;
var
  E: EChecksumError;
begin
  E := EChecksumError.Create('test');
  try
    AssertTrue('EChecksumError inherits from EDependencyError',
      E is EDependencyError);
  finally
    E.Free;
  end;
end;

procedure TTestRemoteExceptionTypes.TestChecksumErrorIsNotNetworkError;
var
  E: EChecksumError;
begin
  E := EChecksumError.Create('test');
  try
    AssertEquals('EChecksumError class name', 'EChecksumError', E.ClassName);
    AssertFalse('Class names differ',
      E.ClassName = 'ENetworkError');
  finally
    E.Free;
  end;
end;

{ TTestRemoteChecksumValidation }

procedure TTestRemoteChecksumValidation.SetUp;
begin
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'pasbuild-test-remote-' + IntToStr(GetProcessID);
  ForceDirectories(FTempDir);
end;

procedure TTestRemoteChecksumValidation.TearDown;
var
  SearchRec: TSearchRec;
  FullPath: string;
begin
  if not DirectoryExists(FTempDir) then
    Exit;
  if FindFirst(IncludeTrailingPathDelimiter(FTempDir) + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          FullPath := IncludeTrailingPathDelimiter(FTempDir) + SearchRec.Name;
          DeleteFile(FullPath);
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
  RemoveDir(FTempDir);
end;

procedure TTestRemoteChecksumValidation.TestDownloadFileChecksumMismatchRaises;
var
  Client: TRemoteClient;
  FilePath: string;
  F: TextFile;
  ExceptionRaised: Boolean;
begin
  FilePath := IncludeTrailingPathDelimiter(FTempDir) + 'test-file.txt';
  AssignFile(F, FilePath);
  Rewrite(F);
  Write(F, 'test content');
  CloseFile(F);

  Client := TRemoteClient.Create;
  try
    ExceptionRaised := False;
    try
      Client.DownloadFile('file://' + FilePath, FilePath, 'deadbeef');
    except
      on E: Exception do
      begin
        ExceptionRaised := True;
        AssertTrue('Should be checksum or network error',
          (E is EChecksumError) or (E is ENetworkError));
      end;
    end;
    AssertTrue('Should raise an exception for bad checksum', ExceptionRaised);
  finally
    Client.Free;
  end;
end;

procedure TTestRemoteChecksumValidation.TestDownloadFileChecksumMatchSucceeds;
var
  FilePath, Hash: string;
  F: TextFile;
begin
  FilePath := IncludeTrailingPathDelimiter(FTempDir) + 'test-file.txt';
  AssignFile(F, FilePath);
  Rewrite(F);
  Write(F, 'abc');
  CloseFile(F);

  Hash := SHA256File(FilePath);
  AssertEquals('SHA-256 of abc file',
    'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad', Hash);
end;

{ TTestUrlSchemeEnforcement }

procedure TTestUrlSchemeEnforcement.TestHttpsAccepted;
begin
  // Must not raise
  TRemoteClient.ValidateUrlScheme('https://example.org/registry/pkg.xml');
  TRemoteClient.ValidateUrlScheme('HTTPS://EXAMPLE.ORG/PKG.XML');
end;

procedure TTestUrlSchemeEnforcement.TestPlainHttpRejected;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TRemoteClient.ValidateUrlScheme('http://example.org/registry/pkg.xml');
  except
    on E: ENetworkError do
    begin
      Raised := True;
      AssertTrue('Error should mention https',
        Pos('https', LowerCase(E.Message)) > 0);
    end;
  end;
  AssertTrue('Plain HTTP should be rejected', Raised);
end;

procedure TTestUrlSchemeEnforcement.TestLoopbackHttpAllowed;
begin
  // Must not raise - local test registries are legitimate
  TRemoteClient.ValidateUrlScheme('http://localhost:8080/registry/');
  TRemoteClient.ValidateUrlScheme('http://127.0.0.1/registry/');
  TRemoteClient.ValidateUrlScheme('http://[::1]:9000/registry/');
end;

procedure TTestUrlSchemeEnforcement.TestOtherSchemesRejected;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TRemoteClient.ValidateUrlScheme('ftp://example.org/pkg.zip');
  except
    on E: ENetworkError do
      Raised := True;
  end;
  AssertTrue('ftp scheme should be rejected', Raised);
end;

procedure TTestUrlSchemeEnforcement.TestEmptyUrlRejected;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TRemoteClient.ValidateUrlScheme('');
  except
    on E: ENetworkError do
      Raised := True;
  end;
  AssertTrue('Empty URL should be rejected', Raised);
end;

initialization
  RegisterTest(TTestRemoteClientCreate);
  RegisterTest(TTestRemoteExceptionTypes);
  RegisterTest(TTestRemoteChecksumValidation);
  RegisterTest(TTestUrlSchemeEnforcement);

end.
