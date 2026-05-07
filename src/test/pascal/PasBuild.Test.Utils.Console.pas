{
  This file is part of PasBuild.

  Copyright (c) 2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Utils.Console;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Utils.Console;

type

  { TTestUtilsConsoleWriteColor }

  TTestUtilsConsoleWriteColor = class(TTestCase)
  private
    function WriteColorToFakeFile(AColor: TSimpleColor; AText: String): RawByteString;
  published
    procedure TestStdOutIsConsole;
    procedure TestWriteColorRedirected;
  end;


implementation

type
  PStream = ^TStream;

function GetStream(var F: TTextRec): TStream;
begin
  Result := PStream(@F.UserData)^;
end;

function StreamOpen(var F: TTextRec): Integer;
begin
  F.BufPos := 0;
  F.BufEnd := 0;
  Result := 0;
end;

function StreamInOut(var F: TTextRec): Integer;
begin
  Result := 0;
  if F.BufPos > 0 then
  begin
    GetStream(F).Write(F.BufPtr^, F.BufPos);
    F.BufPos := 0;
  end;
end;

function StreamFlush(var F: TTextRec): Integer;
begin
  Result := StreamInOut(F);
end;

function StreamClose(var F: TTextRec): Integer;
begin
  Result := StreamInOut(F);
end;

procedure AssignTextToStream(var TF: Text; S: TStream);
begin
  with TTextRec(TF) do
  begin
    Mode := fmClosed;
    BufSize := SizeOf(Buffer);
    BufPtr := @Buffer;
    OpenFunc := @StreamOpen;
    InOutFunc := @StreamInOut;
    FlushFunc := @StreamFlush;
    CloseFunc := @StreamClose;
    Name[0] := #0;
    PStream(@UserData)^ := S;
  end;
end;

{ TTestUtilsConsoleWriteColor }

function TTestUtilsConsoleWriteColor.WriteColorToFakeFile(AColor: TSimpleColor; AText: String): RawByteString;
var
  F: Text;
  MS: TStringStream;
begin
  Result := '';
  MS := TStringStream.Create;
  try
    AssignTextToStream(F, MS);
    Rewrite(F);
    try
      WriteColor(AColor, AText, F);
    finally
      Close(F);
    end;

    Result := MS.DataString;
  finally
    MS.Free;
  end;
end;

procedure TTestUtilsConsoleWriteColor.TestWriteColorRedirected;
var
  S: RawByteString;
begin
  S := WriteColorToFakeFile(scRed, 'NoControlCodes');
  AssertEquals('Writing to stdout without tty should have no control codes',
    'NoControlCodes', S);
end;

procedure TTestUtilsConsoleWriteColor.TestStdOutIsConsole;
begin
  AssertEquals('Stdout/Stderr run from test process should not be a console/tty',
    StdOutIsConsole, False);

end;

initialization
  RegisterTest(TTestUtilsConsoleWriteColor);

end.

