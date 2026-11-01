{
  This file is part of PasBuild.

  Copyright (c) 2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Compiler.Blaise;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Compiler.Blaise;

type
  { The Blaise backend's compiler identity, which names repository
    artifacts (<compiler>-<cpu>-<os>-<version>). }
  TTestBlaiseBackendIdentity = class(TTestCase)
  published
    procedure TestVersionFromBannerTakesTheVersionNumber;
    procedure TestVersionFromBannerWithoutVersionHasNoSpaces;
    procedure TestTargetIsTheHost;
  end;

implementation

procedure TTestBlaiseBackendIdentity.TestVersionFromBannerTakesTheVersionNumber;
begin
  AssertEquals('0.16.0-SNAPSHOT',
    TBlaiseBackend.VersionFromBanner('Blaise Compiler v0.16.0-SNAPSHOT'));
  AssertEquals('0.15.0', TBlaiseBackend.VersionFromBanner('Blaise Compiler v0.15.0'));
  AssertEquals('trailing text ignored', '1.2.3',
    TBlaiseBackend.VersionFromBanner('  Blaise Compiler v1.2.3 (abc123)  '));
end;

procedure TTestBlaiseBackendIdentity.TestVersionFromBannerWithoutVersionHasNoSpaces;
begin
  AssertEquals('Blaise-Compiler', TBlaiseBackend.VersionFromBanner('Blaise Compiler'));
end;

{ Blaise compiles for the machine it runs on, as PasBuild does; the names
  match FPC's, so artifacts of both compilers sort alike. }
procedure TTestBlaiseBackendIdentity.TestTargetIsTheHost;
var
  B: TBlaiseBackend;
begin
  B := TBlaiseBackend.Create('blaise');
  try
    AssertEquals('cpu', LowerCase({$I %FPCTARGETCPU%}), B.GetTargetCPU);
    AssertEquals('os', LowerCase({$I %FPCTARGETOS%}), B.GetTargetOS);
  finally
    B.Free;
  end;
end;

initialization
  RegisterTest(TTestBlaiseBackendIdentity);

end.
