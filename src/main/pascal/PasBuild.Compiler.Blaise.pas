{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Compiler.Blaise;

{$mode objfpc}{$H+}

interface

uses
  PasBuild.Compiler.Backend;

type
  TBlaiseBackend = class(TCompilerBackend)
  public
    { The version in 'blaise --help''s first line ("Blaise Compiler
      v0.16.0-SNAPSHOT" -> '0.16.0-SNAPSHOT').  It names repository
      artifacts, so a line without a version comes back without spaces. }
    class function VersionFromBanner(const ALine: string): string;
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
    function IncrementalFlag: string; override;
    function CompilesDependenciesFromSource: Boolean; override;
  end;

implementation

uses
  Classes, SysUtils, PasBuild.Utils;

function TBlaiseBackend.Name: string;
begin
  Result := 'Blaise';
end;

function TBlaiseBackend.GetVersion: string;
var
  Output: string;
  Lines: TStringList;
  ExitCode: Integer;
begin
  { 'blaise --help' prints "Blaise Compiler vX.Y.Z" on the first line. }
  ExitCode := TUtils.ExecuteProcessWithCapture(Executable + ' --help', Output);
  if ExitCode <> 0 then
  begin
    Result := '';
    Exit;
  end;
  Lines := TStringList.Create;
  try
    Lines.Text := Output;
    if Lines.Count > 0 then
      Result := VersionFromBanner(Lines[0])
    else
      Result := 'unknown';
  finally
    Lines.Free;
  end;
end;

class function TBlaiseBackend.VersionFromBanner(const ALine: string): string;
var
  S: string;
  P, E: Integer;
begin
  S := Trim(ALine);
  { The version follows ' v' and runs to the next space. }
  P := Pos(' v', S);
  if (P > 0) and (P + 2 <= Length(S)) and (S[P + 2] in ['0'..'9']) then
  begin
    S := Copy(S, P + 2, Length(S));
    E := Pos(' ', S);
    if E > 0 then
      S := Copy(S, 1, E - 1);
    Result := S;
  end
  else
    Result := StringReplace(S, ' ', '-', [rfReplaceAll]);
end;

{ Blaise compiles for the machine it runs on (PasBuild passes no
  --target), which is the machine PasBuild runs on.  The names are FPC's,
  as the FPC backend reports them. }
function TBlaiseBackend.GetTargetCPU: string;
begin
  Result := LowerCase({$I %FPCTARGETCPU%});
end;

function TBlaiseBackend.GetTargetOS: string;
begin
  Result := LowerCase({$I %FPCTARGETOS%});
end;

function TBlaiseBackend.BaseCommand: string;
begin
  Result := Executable;
end;

function TBlaiseBackend.SourceFlag(const APath: string): string;
begin
  Result := '--source ' + APath;
end;

function TBlaiseBackend.OutputFlags(const AOutputDir, AExeName: string): string;
begin
  if AExeName <> '' then
    Result := '--output ' + IncludeTrailingPathDelimiter(AOutputDir) + AExeName
  else
    Result := '';
end;

function TBlaiseBackend.UnitOutputDirFlag(const APath: string): string;
begin
  { No incremental build means no *.o cache is produced, so the cache path is
    meaningless — suppress it. }
  if NoIncremental then
    Result := ''
  else
    Result := '--unit-cache ' + APath;
end;

function TBlaiseBackend.UnitPathFlag(const APath: string): string;
begin
  Result := '--unit-path ' + APath;
end;

function TBlaiseBackend.IncludePathFlag(const APath: string): string;
begin
  Result := '';  { Blaise does not yet support include file search paths. }
end;

function TBlaiseBackend.DefineFlag(const ADefine: string): string;
begin
  Result := '';  { Conditional compilation defines not yet supported. }
end;

function TBlaiseBackend.ExtraOptionFlag(const AOption: string): string;
begin
  { Forward Blaise long options (e.g. '--backend native') verbatim so
    project.xml <compilerOptions> can steer the Blaise compiler.
    Single-dash options are FPC-specific (debug/release profiles) and
    stay swallowed — Blaise does not understand them. }
  if Copy(AOption, 1, 2) = '--' then
    Result := AOption
  else
    Result := '';
end;

function TBlaiseBackend.IncrementalFlag: string;
begin
  { Blaise defaults to incremental builds (per-unit *.o files).  When the user
    opts out, emit --no-incremental so no *.o files are generated. }
  if NoIncremental then
    Result := '--no-incremental'
  else
    Result := '';
end;

function TBlaiseBackend.CompilesDependenciesFromSource: Boolean;
begin
  { Blaise compiles every unit from source.  A dependency's target/units only
    holds cached *.o files, and none at all with --no-incremental, so the
    dependency's sources must be reachable as well. }
  Result := True;
end;

end.
