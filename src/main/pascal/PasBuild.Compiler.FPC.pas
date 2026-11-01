{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Compiler.FPC;

{$mode objfpc}{$H+}

interface

uses
  PasBuild.Compiler.Backend;

type
  TFPCBackend = class(TCompilerBackend)
  public
    function Name: string; override;
    function GetVersion: string; override;
    function GetTargetCPU: string; override;
    function GetTargetOS: string; override;
    function BaseCommand: string; override;
    function SourceFlag(const APath: string): string; override;
    function OutputFlags(const AOutputDir, AExeName: string): string; override;
    function UnitOutputDirFlag(const APath: string): string; override;
    function UnitPathFlag(const APath: string): string; override;
    function IncludePathFlag(const APath: string): string; override;
    function DefineFlag(const ADefine: string): string; override;
    function ExtraOptionFlag(const AOption: string): string; override;
    function SkipLinkFlag: string; override;
  end;

implementation

uses
  SysUtils, PasBuild.Utils;

function TFPCBackend.Name: string;
begin
  Result := 'FPC';
end;

function TFPCBackend.SkipLinkFlag: string;
begin
  // -Cn: omit the linking stage; units (.ppu/.o) are still produced
  Result := '-Cn';
end;

function TFPCBackend.GetVersion: string;
var
  Output: string;
  ExitCode: Integer;
begin
  ExitCode := TUtils.ExecuteProcessWithCapture(Executable + ' -iV', Output);
  if ExitCode = 0 then
    Result := Trim(Output)
  else
    Result := '';
end;

function TFPCBackend.GetTargetCPU: string;
var
  Output: string;
begin
  TUtils.ExecuteProcessWithCapture(Executable + ' -iTP', Output);
  Result := Trim(Output);
end;

function TFPCBackend.GetTargetOS: string;
var
  Output: string;
begin
  TUtils.ExecuteProcessWithCapture(Executable + ' -iTO', Output);
  Result := Trim(Output);
end;

function TFPCBackend.BaseCommand: string;
begin
  Result := Executable + ' -Mobjfpc -O1';
end;

function TFPCBackend.SourceFlag(const APath: string): string;
begin
  Result := APath;
end;

function TFPCBackend.OutputFlags(const AOutputDir, AExeName: string): string;
begin
  Result := '-FE' + TUtils.QuotePath(AOutputDir);
  if AExeName <> '' then
    Result := Result + ' -o' + AExeName;
end;

function TFPCBackend.UnitOutputDirFlag(const APath: string): string;
begin
  Result := '-FU' + TUtils.QuotePath(APath);
end;

function TFPCBackend.UnitPathFlag(const APath: string): string;
begin
  Result := '-Fu' + TUtils.QuotePath(APath);
end;

function TFPCBackend.IncludePathFlag(const APath: string): string;
begin
  Result := '-Fi' + TUtils.QuotePath(APath);
end;

function TFPCBackend.DefineFlag(const ADefine: string): string;
begin
  Result := '-d' + ADefine;
end;

function TFPCBackend.ExtraOptionFlag(const AOption: string): string;
begin
  Result := AOption;
end;

end.
