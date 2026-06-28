{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Test.Remote.Manifest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasBuild.Remote.Manifest;

type
  TTestParseReleaseManifest = class(TTestCase)
  private
    function GetFixturePath(const AFileName: string): string;
  published
    procedure TestParseBasicFields;
    procedure TestParseOrigin;
    procedure TestParseCompilers;
    procedure TestParseDependencies;
    procedure TestParsePublishedAt;
    procedure TestParseNoCompilersSection;
    procedure TestParseNoDependencies;
    procedure TestParseInvalidXmlRaisesException;
  end;

  TTestReleaseManifestCompat = class(TTestCase)
  published
    procedure TestIsCompatibleWithFPC;
    procedure TestIsCompatibleWithBlaise;
    procedure TestIsCompatibleWithUnknown;
    procedure TestIsCompatibleWithNoCompilers;
    procedure TestMinVersionForFPC;
    procedure TestMinVersionForMissing;
  end;

implementation

{ TTestParseReleaseManifest }

function TTestParseReleaseManifest.GetFixturePath(const AFileName: string): string;
begin
  Result := 'fixtures/registry/' + AFileName;
end;

procedure TTestParseReleaseManifest.TestParseBasicFields;
var
  Manifest: TReleaseManifest;
begin
  Manifest := TManifestParser.ParseVersionManifest(GetFixturePath('full-manifest.xml'));
  try
    AssertEquals('Name', 'fpgui-framework', Manifest.Name);
    AssertEquals('Version', '1.0.0', Manifest.Version);
    AssertEquals('Author', 'Graeme Geldenhuys', Manifest.Author);
    AssertEquals('License', 'LGPL-2.1-or-later', Manifest.License);
    AssertEquals('Description', 'Cross-platform GUI toolkit for Free Pascal',
      Manifest.Description);
  finally
    Manifest.Free;
  end;
end;

procedure TTestParseReleaseManifest.TestParseOrigin;
var
  Manifest: TReleaseManifest;
begin
  Manifest := TManifestParser.ParseVersionManifest(GetFixturePath('full-manifest.xml'));
  try
    AssertEquals('Origin URL',
      'https://github.com/graemeg/fpgui/releases/download/v1.0.0/fpgui-framework-1.0.0-src.zip',
      Manifest.OriginUrl);
    AssertEquals('Origin SHA256',
      'a3f2b8c91d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a',
      Manifest.OriginSHA256);
    AssertEquals('Origin size', 245760, Manifest.OriginSize);
  finally
    Manifest.Free;
  end;
end;

procedure TTestParseReleaseManifest.TestParseCompilers;
var
  Manifest: TReleaseManifest;
begin
  Manifest := TManifestParser.ParseVersionManifest(GetFixturePath('full-manifest.xml'));
  try
    AssertEquals('Compiler count', 2, Length(Manifest.Compilers));
    AssertEquals('First compiler', 'fpc', Manifest.Compilers[0].Name);
    AssertEquals('FPC min version', '3.2.2', Manifest.Compilers[0].MinimumVersion);
    AssertEquals('Second compiler', 'blaise', Manifest.Compilers[1].Name);
    AssertEquals('Blaise min version', '0.4.0', Manifest.Compilers[1].MinimumVersion);
  finally
    Manifest.Free;
  end;
end;

procedure TTestParseReleaseManifest.TestParseDependencies;
var
  Manifest: TReleaseManifest;
begin
  Manifest := TManifestParser.ParseVersionManifest(GetFixturePath('full-manifest.xml'));
  try
    AssertEquals('Dependency count', 1, Length(Manifest.Dependencies));
    AssertEquals('Dep name', 'some-base-lib', Manifest.Dependencies[0].Name);
    AssertEquals('Dep version', '2.0.0', Manifest.Dependencies[0].Version);
  finally
    Manifest.Free;
  end;
end;

procedure TTestParseReleaseManifest.TestParsePublishedAt;
var
  Manifest: TReleaseManifest;
begin
  Manifest := TManifestParser.ParseVersionManifest(GetFixturePath('full-manifest.xml'));
  try
    AssertEquals('PublishedAt', '2026-06-15T14:30:00Z', Manifest.PublishedAt);
  finally
    Manifest.Free;
  end;
end;

procedure TTestParseReleaseManifest.TestParseNoCompilersSection;
var
  Manifest: TReleaseManifest;
begin
  Manifest := TManifestParser.ParseVersionManifest(GetFixturePath('minimal-manifest.xml'));
  try
    AssertEquals('No compilers', 0, Length(Manifest.Compilers));
  finally
    Manifest.Free;
  end;
end;

procedure TTestParseReleaseManifest.TestParseNoDependencies;
var
  Manifest: TReleaseManifest;
begin
  Manifest := TManifestParser.ParseVersionManifest(GetFixturePath('minimal-manifest.xml'));
  try
    AssertEquals('No dependencies', 0, Length(Manifest.Dependencies));
  finally
    Manifest.Free;
  end;
end;

procedure TTestParseReleaseManifest.TestParseInvalidXmlRaisesException;
var
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  try
    TManifestParser.ParseVersionManifest(GetFixturePath('invalid-manifest.xml')).Free;
  except
    ExceptionRaised := True;
  end;
  AssertTrue('Invalid XML should raise exception', ExceptionRaised);
end;

{ TTestReleaseManifestCompat }

procedure TTestReleaseManifestCompat.TestIsCompatibleWithFPC;
var
  Manifest: TReleaseManifest;
  Arr: TCompilerRequirementArray;
begin
  Manifest := TReleaseManifest.Create;
  try
    SetLength(Arr, 2);
    Arr[0].Name := 'fpc';
    Arr[0].MinimumVersion := '3.2.2';
    Arr[1].Name := 'blaise';
    Arr[1].MinimumVersion := '0.4.0';
    Manifest.Compilers := Arr;
    AssertTrue('Should be compatible with fpc', Manifest.IsCompatibleWith('fpc'));
  finally
    Manifest.Free;
  end;
end;

procedure TTestReleaseManifestCompat.TestIsCompatibleWithBlaise;
var
  Manifest: TReleaseManifest;
  Arr: TCompilerRequirementArray;
begin
  Manifest := TReleaseManifest.Create;
  try
    SetLength(Arr, 1);
    Arr[0].Name := 'fpc';
    Arr[0].MinimumVersion := '3.2.2';
    Manifest.Compilers := Arr;
    AssertFalse('Should not be compatible with blaise',
      Manifest.IsCompatibleWith('blaise'));
  finally
    Manifest.Free;
  end;
end;

procedure TTestReleaseManifestCompat.TestIsCompatibleWithUnknown;
var
  Manifest: TReleaseManifest;
  Arr: TCompilerRequirementArray;
begin
  Manifest := TReleaseManifest.Create;
  try
    SetLength(Arr, 1);
    Arr[0].Name := 'fpc';
    Arr[0].MinimumVersion := '3.2.2';
    Manifest.Compilers := Arr;
    AssertFalse('Unknown compiler not compatible',
      Manifest.IsCompatibleWith('gcc'));
  finally
    Manifest.Free;
  end;
end;

procedure TTestReleaseManifestCompat.TestIsCompatibleWithNoCompilers;
var
  Manifest: TReleaseManifest;
begin
  Manifest := TReleaseManifest.Create;
  try
    AssertTrue('No compilers section means any compiler',
      Manifest.IsCompatibleWith('fpc'));
    AssertTrue('No compilers section means any compiler (blaise)',
      Manifest.IsCompatibleWith('blaise'));
  finally
    Manifest.Free;
  end;
end;

procedure TTestReleaseManifestCompat.TestMinVersionForFPC;
var
  Manifest: TReleaseManifest;
  Arr: TCompilerRequirementArray;
begin
  Manifest := TReleaseManifest.Create;
  try
    SetLength(Arr, 1);
    Arr[0].Name := 'fpc';
    Arr[0].MinimumVersion := '3.2.2';
    Manifest.Compilers := Arr;
    AssertEquals('Min version for fpc', '3.2.2', Manifest.MinVersionFor('fpc'));
  finally
    Manifest.Free;
  end;
end;

procedure TTestReleaseManifestCompat.TestMinVersionForMissing;
var
  Manifest: TReleaseManifest;
  Arr: TCompilerRequirementArray;
begin
  Manifest := TReleaseManifest.Create;
  try
    SetLength(Arr, 1);
    Arr[0].Name := 'fpc';
    Arr[0].MinimumVersion := '3.2.2';
    Manifest.Compilers := Arr;
    AssertEquals('Min version for missing compiler', '',
      Manifest.MinVersionFor('blaise'));
  finally
    Manifest.Free;
  end;
end;

initialization
  RegisterTest(TTestParseReleaseManifest);
  RegisterTest(TTestReleaseManifestCompat);

end.
