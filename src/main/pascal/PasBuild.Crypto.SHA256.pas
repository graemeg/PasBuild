{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Crypto.SHA256;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSHA256Digest = array[0..31] of Byte;

  TSHA256Context = record
    State: array[0..7] of DWord;
    Count: QWord;
    Buffer: array[0..63] of Byte;
    BufLen: Integer;
  end;

procedure SHA256Init(out AContext: TSHA256Context);
procedure SHA256Update(var AContext: TSHA256Context; const AData: string);
procedure SHA256UpdateBuffer(var AContext: TSHA256Context; const AData; ALen: Integer);
function SHA256Final(var AContext: TSHA256Context): string;
function SHA256FinalDigest(var AContext: TSHA256Context): TSHA256Digest;

function SHA256String(const AStr: string): string;
function SHA256Stream(AStream: TStream): string;
function SHA256File(const AFileName: string): string;

function SHA256DigestToHex(const ADigest: TSHA256Digest): string;

implementation

const
  K: array[0..63] of DWord = (
    $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5,
    $3956c25b, $59f111f1, $923f82a4, $ab1c5ed5,
    $d807aa98, $12835b01, $243185be, $550c7dc3,
    $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174,
    $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc,
    $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
    $983e5152, $a831c66d, $b00327c8, $bf597fc7,
    $c6e00bf3, $d5a79147, $06ca6351, $14292967,
    $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13,
    $650a7354, $766a0abb, $81c2c92e, $92722c85,
    $a2bfe8a1, $a81a664b, $c24b8b70, $c76c51a3,
    $d192e819, $d6990624, $f40e3585, $106aa070,
    $19a4c116, $1e376c08, $2748774c, $34b0bcb5,
    $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3,
    $748f82ee, $78a5636f, $84c87814, $8cc70208,
    $90befffa, $a4506ceb, $bef9a3f7, $c67178f2
  );

function RightRotate(AValue: DWord; AShift: Integer): DWord; inline;
begin
  Result := (AValue shr AShift) or (AValue shl (32 - AShift));
end;

procedure SHA256Transform(var AState: array of DWord; const ABlock: array of Byte);
var
  W: array[0..63] of DWord;
  A, B, C, D, E, F, G, H: DWord;
  S0, S1, Ch, Maj, Temp1, Temp2: DWord;
  I: Integer;
begin
  for I := 0 to 15 do
    W[I] := (DWord(ABlock[I * 4]) shl 24) or
            (DWord(ABlock[I * 4 + 1]) shl 16) or
            (DWord(ABlock[I * 4 + 2]) shl 8) or
            DWord(ABlock[I * 4 + 3]);

  for I := 16 to 63 do
  begin
    S0 := RightRotate(W[I - 15], 7) xor RightRotate(W[I - 15], 18) xor (W[I - 15] shr 3);
    S1 := RightRotate(W[I - 2], 17) xor RightRotate(W[I - 2], 19) xor (W[I - 2] shr 10);
    W[I] := W[I - 16] + S0 + W[I - 7] + S1;
  end;

  A := AState[0];
  B := AState[1];
  C := AState[2];
  D := AState[3];
  E := AState[4];
  F := AState[5];
  G := AState[6];
  H := AState[7];

  for I := 0 to 63 do
  begin
    S1 := RightRotate(E, 6) xor RightRotate(E, 11) xor RightRotate(E, 25);
    Ch := (E and F) xor ((not E) and G);
    Temp1 := H + S1 + Ch + K[I] + W[I];
    S0 := RightRotate(A, 2) xor RightRotate(A, 13) xor RightRotate(A, 22);
    Maj := (A and B) xor (A and C) xor (B and C);
    Temp2 := S0 + Maj;

    H := G;
    G := F;
    F := E;
    E := D + Temp1;
    D := C;
    C := B;
    B := A;
    A := Temp1 + Temp2;
  end;

  AState[0] := AState[0] + A;
  AState[1] := AState[1] + B;
  AState[2] := AState[2] + C;
  AState[3] := AState[3] + D;
  AState[4] := AState[4] + E;
  AState[5] := AState[5] + F;
  AState[6] := AState[6] + G;
  AState[7] := AState[7] + H;
end;

procedure SHA256Init(out AContext: TSHA256Context);
begin
  AContext.State[0] := $6a09e667;
  AContext.State[1] := $bb67ae85;
  AContext.State[2] := $3c6ef372;
  AContext.State[3] := $a54ff53a;
  AContext.State[4] := $510e527f;
  AContext.State[5] := $9b05688c;
  AContext.State[6] := $1f83d9ab;
  AContext.State[7] := $5be0cd19;
  AContext.Count := 0;
  AContext.BufLen := 0;
  FillChar(AContext.Buffer, SizeOf(AContext.Buffer), 0);
end;

procedure SHA256UpdateBuffer(var AContext: TSHA256Context; const AData; ALen: Integer);
var
  P: PByte;
  Space, CopyLen: Integer;
begin
  if ALen <= 0 then
    Exit;

  P := @AData;
  AContext.Count := AContext.Count + QWord(ALen);

  if AContext.BufLen > 0 then
  begin
    Space := 64 - AContext.BufLen;
    if ALen < Space then
    begin
      Move(P^, AContext.Buffer[AContext.BufLen], ALen);
      Inc(AContext.BufLen, ALen);
      Exit;
    end;
    Move(P^, AContext.Buffer[AContext.BufLen], Space);
    SHA256Transform(AContext.State, AContext.Buffer);
    Inc(P, Space);
    Dec(ALen, Space);
    AContext.BufLen := 0;
  end;

  while ALen >= 64 do
  begin
    SHA256Transform(AContext.State, PByte(P)^);
    Inc(P, 64);
    Dec(ALen, 64);
  end;

  if ALen > 0 then
  begin
    Move(P^, AContext.Buffer[0], ALen);
    AContext.BufLen := ALen;
  end;
end;

procedure SHA256Update(var AContext: TSHA256Context; const AData: string);
begin
  if Length(AData) > 0 then
    SHA256UpdateBuffer(AContext, AData[1], Length(AData));
end;

function SHA256FinalDigest(var AContext: TSHA256Context): TSHA256Digest;
var
  BitCount: QWord;
  PadLen: Integer;
  Padding: array[0..71] of Byte;
  I: Integer;
begin
  BitCount := AContext.Count * 8;

  PadLen := 64 - AContext.BufLen;
  if PadLen <= 8 then
    Inc(PadLen, 64);

  FillChar(Padding, SizeOf(Padding), 0);
  Padding[0] := $80;

  Padding[PadLen - 8] := Byte(BitCount shr 56);
  Padding[PadLen - 7] := Byte(BitCount shr 48);
  Padding[PadLen - 6] := Byte(BitCount shr 40);
  Padding[PadLen - 5] := Byte(BitCount shr 32);
  Padding[PadLen - 4] := Byte(BitCount shr 24);
  Padding[PadLen - 3] := Byte(BitCount shr 16);
  Padding[PadLen - 2] := Byte(BitCount shr 8);
  Padding[PadLen - 1] := Byte(BitCount);

  SHA256UpdateBuffer(AContext, Padding, PadLen);

  for I := 0 to 7 do
  begin
    Result[I * 4]     := Byte(AContext.State[I] shr 24);
    Result[I * 4 + 1] := Byte(AContext.State[I] shr 16);
    Result[I * 4 + 2] := Byte(AContext.State[I] shr 8);
    Result[I * 4 + 3] := Byte(AContext.State[I]);
  end;
end;

function SHA256DigestToHex(const ADigest: TSHA256Digest): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to 31 do
    Result := Result + LowerCase(IntToHex(ADigest[I], 2));
end;

function SHA256Final(var AContext: TSHA256Context): string;
begin
  Result := SHA256DigestToHex(SHA256FinalDigest(AContext));
end;

function SHA256String(const AStr: string): string;
var
  Ctx: TSHA256Context;
begin
  SHA256Init(Ctx);
  SHA256Update(Ctx, AStr);
  Result := SHA256Final(Ctx);
end;

function SHA256Stream(AStream: TStream): string;
var
  Ctx: TSHA256Context;
  Buf: array[0..8191] of Byte;
  BytesRead: Integer;
begin
  SHA256Init(Ctx);
  AStream.Position := 0;
  repeat
    BytesRead := AStream.Read(Buf, SizeOf(Buf));
    if BytesRead > 0 then
      SHA256UpdateBuffer(Ctx, Buf, BytesRead);
  until BytesRead = 0;
  Result := SHA256Final(Ctx);
end;

function SHA256File(const AFileName: string): string;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := SHA256Stream(FS);
  finally
    FS.Free;
  end;
end;

end.
