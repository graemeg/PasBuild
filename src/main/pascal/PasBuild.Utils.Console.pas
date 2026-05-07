{
  This file is part of PasBuild.

  Copyright (c) 2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Utils.Console;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF WINDOWS}
  Windows;
  {$ELSE}
  TermIO;
  {$ENDIF}


type
  TSimpleColor = (
    scResetAll = 0,
    scBlack = 30,
    scRed = 31,
    scGreen = 32,
    scYellow = 33,
    scBlue = 34,
    scMagenta = 35,
    scCyan = 36,
    scWhite = 37,
    scBrightBlack = 90,
    scBrightRed = 91,
    scBrightGreen = 92,
    scBrightYellow = 93,
    scBrightBlue = 94,
    scBrightMagenta = 95,
    scBrightCyan = 96,
    scBrightWhite = 97
  );

procedure WriteColor(AColor: TSimpleColor; const AText: String; var AFile: Text);

function  StdOutIsConsole: Boolean;

implementation

{$IFDEF WINDOWS}

var
  GScreenAttributes: Word;
  GStdOutIsConsole: Integer = MaxInt;

function AnsiToWinColor(Color: Integer): WORD;
begin
  case Color of
    30: Result := 0;                                 // Black
    31: Result := FOREGROUND_RED;                    // Red
    32: Result := FOREGROUND_GREEN;                  // Green
    33: Result := FOREGROUND_RED or FOREGROUND_GREEN;// Yellow
    34: Result := FOREGROUND_BLUE;                   // Blue
    35: Result := FOREGROUND_RED or FOREGROUND_BLUE; // Magenta
    36: Result := FOREGROUND_GREEN or FOREGROUND_BLUE; // Cyan
    37: Result := FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE; // White

    90: Result := FOREGROUND_INTENSITY;              // Bright Black (Gray)
    91: Result := FOREGROUND_RED or FOREGROUND_INTENSITY;
    92: Result := FOREGROUND_GREEN or FOREGROUND_INTENSITY;
    93: Result := FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_INTENSITY;
    94: Result := FOREGROUND_BLUE or FOREGROUND_INTENSITY;
    95: Result := FOREGROUND_RED or FOREGROUND_BLUE or FOREGROUND_INTENSITY;
    96: Result := FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY;
    97: Result := FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY;

    else Result := FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE; // Default (White)
  end;
end;


procedure WriteColor(AColor: TSimpleColor; const AText: String; var AFile: Text);
var
  lColor: Word;
begin
  if not StdOutIsConsole then
  begin
    Write(AFile, AText);
    Exit; // no fun stuff for redirected output
  end;
  lColor := AnsiToWinColor(Ord(AColor));
  SetConsoleTextAttribute(StdOutputHandle, lColor);
  Write(AFile, AText);
  // restore default
  SetConsoleTextAttribute(StdOutputHandle, GScreenAttributes);
end;

function StdOutIsConsole: Boolean;
var
  Mode: DWord;
  lConsoleInfo: TCONSOLESCREENBUFFERINFO;
begin
  if  GStdOutIsConsole = MaxInt then
  begin
    GStdOutIsConsole := Integer(GetConsoleMode(StdOutputHandle, Mode));
    // also get the default colors
    if GStdOutIsConsole <> 0 then
    begin
      GetConsoleScreenBufferInfo(StdOutputHandle, lConsoleInfo);
      GScreenAttributes:=lConsoleInfo.wAttributes;
    end;
  end;

  Result := GStdOutIsConsole = 1;
end;

{$ELSE}

var
  GStdOutIsConsole: Integer = MaxInt;

procedure WriteColor(AColor: TSimpleColor; const AText: String; var AFile: Text);
var
  Col: String;
const
  ESC = #27;
begin
  Col := IntToStr(Ord(AColor));
  if StdOutIsConsole then
      Write(AFile, ESC+'['+Col+'m'+AText +ESC+'[0m')
    else
      Write(AFile, AText);
end;

function StdOutIsConsole: Boolean;
begin
  if GStdOutIsConsole = MaxInt then
  begin
    GStdOutIsConsole := IsATTY(StdOut);
  end;

  Result := GStdOutIsConsole <> 0;
end;

{$ENDIF}

end.

