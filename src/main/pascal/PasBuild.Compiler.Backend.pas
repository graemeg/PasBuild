{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Compiler.Backend;

{$mode objfpc}{$H+}

interface

{ Abstract compiler backend.

  Each concrete backend translates PasBuild's logical build parameters into
  the command-line syntax of a specific Pascal compiler.  Flag methods return
  an empty string when a concept is not applicable to that backend; callers
  skip empty results. }

type
  TCompilerBackend = class
  private
    FExecutable: string;
    FNoIncremental: Boolean;
  public
    constructor Create(const AExecutable: string);
    property Executable: string read FExecutable;

    { Set from the --no-incremental CLI flag.  Backends that have no notion of
      incremental compilation (e.g. FPC) ignore it. }
    property NoIncremental: Boolean read FNoIncremental write FNoIncremental;

    { Flag emitted when NoIncremental is set; '' when not applicable. }
    function IncrementalFlag: string; virtual;

    { True when the compiler builds a module dependency's units from source
      rather than linking its compiled target/units, so the dependency's
      source directories must be on the unit path too.  Default False. }
    function CompilesDependenciesFromSource: Boolean; virtual;

    { Display name reported in version output and log messages. }
    function Name: string; virtual; abstract;

    { Compiler probing — called once at startup. }
    function GetVersion: string; virtual; abstract;
    function GetTargetCPU: string; virtual; abstract;
    function GetTargetOS: string; virtual; abstract;

    { Command construction.
      All path arguments are unquoted; each backend quotes as required.
      Methods return '' when the concept is unsupported by the backend. }

    { Executable plus fixed base flags (e.g. '-Mobjfpc -O1'). }
    function BaseCommand: string; virtual; abstract;

    { Flag for the main source file. }
    function SourceFlag(const APath: string): string; virtual; abstract;

    { Combined output directory and executable name flags. }
    function OutputFlags(const AOutputDir, AExeName: string): string; virtual; abstract;

    { Flag for the compiled-unit output directory (PPU cache). }
    function UnitOutputDirFlag(const APath: string): string; virtual; abstract;

    { Flag for a unit search path (repeatable). }
    function UnitPathFlag(const APath: string): string; virtual; abstract;

    { Flag for an include file search path (repeatable). }
    function IncludePathFlag(const APath: string): string; virtual; abstract;

    { Flag for a conditional compilation define (repeatable). }
    function DefineFlag(const ADefine: string): string; virtual; abstract;

    { Pass-through for a raw compiler option string; return '' to suppress. }
    function ExtraOptionFlag(const AOption: string): string; virtual; abstract;
  end;

implementation

constructor TCompilerBackend.Create(const AExecutable: string);
begin
  inherited Create;
  FExecutable := AExecutable;
end;

function TCompilerBackend.IncrementalFlag: string;
begin
  { Default: the backend has no incremental concept, so --no-incremental is a
    no-op.  Blaise overrides this. }
  Result := '';
end;

function TCompilerBackend.CompilesDependenciesFromSource: Boolean;
begin
  Result := False;
end;

end.
