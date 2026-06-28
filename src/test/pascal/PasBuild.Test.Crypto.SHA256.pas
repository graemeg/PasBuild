{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Crypto.SHA256;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Crypto.SHA256;

type
  { NIST CAVP test vectors for SHA-256 }
  TTestSHA256 = class(TTestCase)
  published
    procedure TestEmptyString;
    procedure TestABC;
    procedure TestLong448Bit;
    procedure TestLong896Bit;
    procedure TestMillionAs;
    procedure TestStreamHashing;
    procedure TestFileHashing;
    procedure TestIncrementalUpdate;
  end;

implementation

{ TTestSHA256 }

procedure TTestSHA256.TestEmptyString;
begin
  AssertEquals('SHA-256 of empty string',
    'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
    SHA256String(''));
end;

procedure TTestSHA256.TestABC;
begin
  AssertEquals('SHA-256 of "abc"',
    'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad',
    SHA256String('abc'));
end;

procedure TTestSHA256.TestLong448Bit;
begin
  AssertEquals('SHA-256 of 448-bit message',
    '248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1',
    SHA256String('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'));
end;

procedure TTestSHA256.TestLong896Bit;
begin
  AssertEquals('SHA-256 of 896-bit message',
    'cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1',
    SHA256String('abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn' +
                 'hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu'));
end;

procedure TTestSHA256.TestMillionAs;
var
  S: string;
begin
  SetLength(S, 1000000);
  FillChar(S[1], 1000000, Ord('a'));
  AssertEquals('SHA-256 of 1 million "a" characters',
    'cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0',
    SHA256String(S));
end;

procedure TTestSHA256.TestStreamHashing;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('abc');
  try
    AssertEquals('SHA-256 of stream should match string hash',
      'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad',
      SHA256Stream(Stream));
  finally
    Stream.Free;
  end;
end;

procedure TTestSHA256.TestFileHashing;
var
  TempFile: string;
  F: TextFile;
  Hash: string;
begin
  TempFile := IncludeTrailingPathDelimiter(GetCurrentDir) + 'sha256-test-file.tmp';
  AssignFile(F, TempFile);
  Rewrite(F);
  Write(F, 'abc');
  CloseFile(F);
  try
    Hash := SHA256File(TempFile);
    AssertEquals('SHA-256 of file should match string hash',
      'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad',
      Hash);
  finally
    DeleteFile(TempFile);
  end;
end;

procedure TTestSHA256.TestIncrementalUpdate;
var
  Ctx: TSHA256Context;
begin
  SHA256Init(Ctx);
  SHA256Update(Ctx, 'a');
  SHA256Update(Ctx, 'b');
  SHA256Update(Ctx, 'c');
  AssertEquals('Incremental SHA-256 should match single-call',
    'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad',
    SHA256Final(Ctx));
end;

initialization
  RegisterTest(TTestSHA256);

end.
