{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Types;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl;

type
  { Project type enumeration }
  TProjectType = (ptApplication, ptLibrary);

  { Test framework enumeration }
  TTestFramework = (tfAuto, tfFPCUnit, tfFPTest);

  { Forward declarations }
  TProfile = class;
  TBuildConfig = class;
  TTestConfig = class;
  TProjectConfig = class;
  TConditionalPath = class;

  { TConditionalPath - Represents a path with an optional condition }
  TConditionalPath = class
  private
    FPath: string;
    FCondition: string;
  public
    constructor Create(const APath: string; const ACondition: string = '');

    property Path: string read FPath write FPath;
    property Condition: string read FCondition write FCondition;
  end;

  { TConditionalPathList - Strongly-typed collection of conditional paths }
  TConditionalPathList = class(specialize TFPGObjectList<TConditionalPath>)
  end;

  { TProfile - Represents a build profile with defines and compiler options }
  TProfile = class
  private
    FId: string;
    FDefines: TStringList;
    FCompilerOptions: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    property Id: string read FId write FId;
    property Defines: TStringList read FDefines;
    property CompilerOptions: TStringList read FCompilerOptions;
  end;

  { TProfileList - Strongly-typed collection of build profiles using generics }
  TProfileList = class(specialize TFPGObjectList<TProfile>)
  public
    function FindById(const AId: string): TProfile;
  end;

  { TBuildConfig - Build configuration section }
  TBuildConfig = class
  private
    FProjectType: TProjectType;
    FMainSource: string;
    FOutputDirectory: string;
    FExecutableName: string;
    FDefines: TStringList;
    FCompilerOptions: TStringList;
    FUnitPaths: TConditionalPathList;
    FIncludePaths: TConditionalPathList;
    FManualUnitPaths: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property ProjectType: TProjectType read FProjectType write FProjectType;
    property MainSource: string read FMainSource write FMainSource;
    property OutputDirectory: string read FOutputDirectory write FOutputDirectory;
    property ExecutableName: string read FExecutableName write FExecutableName;
    property Defines: TStringList read FDefines;
    property CompilerOptions: TStringList read FCompilerOptions;
    property UnitPaths: TConditionalPathList read FUnitPaths;
    property IncludePaths: TConditionalPathList read FIncludePaths;
    property ManualUnitPaths: Boolean read FManualUnitPaths write FManualUnitPaths;
  end;

  { TTestConfig - Test configuration section }
  TTestConfig = class
  private
    FFramework: TTestFramework;
    FTestSource: string;
    FFrameworkOptions: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    property Framework: TTestFramework read FFramework write FFramework;
    property TestSource: string read FTestSource write FTestSource;
    property FrameworkOptions: TStringList read FFrameworkOptions;
  end;

  { TProjectConfig - Complete project configuration }
  TProjectConfig = class
  private
    FName: string;
    FVersion: string;
    FAuthor: string;
    FLicense: string;
    FProjectUrl: string;
    FRepoUrl: string;
    FBuildConfig: TBuildConfig;
    FTestConfig: TTestConfig;
    FProfiles: TProfileList;
  public
    constructor Create;
    destructor Destroy; override;

    property Name: string read FName write FName;
    property Version: string read FVersion write FVersion;
    property Author: string read FAuthor write FAuthor;
    property License: string read FLicense write FLicense;
    property ProjectUrl: string read FProjectUrl write FProjectUrl;
    property RepoUrl: string read FRepoUrl write FRepoUrl;
    property BuildConfig: TBuildConfig read FBuildConfig;
    property TestConfig: TTestConfig read FTestConfig;
    property Profiles: TProfileList read FProfiles;
  end;

implementation

{ TConditionalPath }

constructor TConditionalPath.Create(const APath: string; const ACondition: string = '');
begin
  inherited Create;
  FPath := APath;
  FCondition := ACondition;
end;

{ TProfile }

constructor TProfile.Create;
begin
  inherited Create;
  FDefines := TStringList.Create;
  FDefines.Duplicates := dupIgnore;
  FDefines.Sorted := True;

  FCompilerOptions := TStringList.Create;
  FCompilerOptions.Duplicates := dupIgnore;
end;

destructor TProfile.Destroy;
begin
  FDefines.Free;
  FCompilerOptions.Free;
  inherited Destroy;
end;

{ TProfileList }

function TProfileList.FindById(const AId: string): TProfile;
var
  Profile: TProfile;
begin
  Result := nil;
  for Profile in Self do
  begin
    if Profile.Id = AId then
    begin
      Result := Profile;
      Exit;
    end;
  end;
end;

{ TBuildConfig }

constructor TBuildConfig.Create;
begin
  inherited Create;
  FDefines := TStringList.Create;
  FDefines.Duplicates := dupIgnore;
  FDefines.Sorted := True;

  FCompilerOptions := TStringList.Create;
  FCompilerOptions.Duplicates := dupIgnore;

  FUnitPaths := TConditionalPathList.Create;
  FUnitPaths.FreeObjects := True;

  FIncludePaths := TConditionalPathList.Create;
  FIncludePaths.FreeObjects := True;

  // Set defaults
  FProjectType := ptApplication;  // Application by default
  FOutputDirectory := 'target';
  FManualUnitPaths := False;  // Auto-scan by default
end;

destructor TBuildConfig.Destroy;
begin
  FDefines.Free;
  FCompilerOptions.Free;
  FUnitPaths.Free;
  FIncludePaths.Free;
  inherited Destroy;
end;

{ TTestConfig }

constructor TTestConfig.Create;
begin
  inherited Create;
  FFrameworkOptions := TStringList.Create;
  FFrameworkOptions.Duplicates := dupIgnore;

  // Set defaults
  FFramework := tfAuto;  // Auto-detect by default
  FTestSource := 'TestRunner.pas';  // Common default name
end;

destructor TTestConfig.Destroy;
begin
  FFrameworkOptions.Free;
  inherited Destroy;
end;

{ TProjectConfig }

constructor TProjectConfig.Create;
begin
  inherited Create;
  FBuildConfig := TBuildConfig.Create;
  FTestConfig := TTestConfig.Create;
  FProfiles := TProfileList.Create;
  FProfiles.FreeObjects := True;

  // Set defaults
  FAuthor := 'Unknown';
  FLicense := 'Proprietary';
end;

destructor TProjectConfig.Destroy;
begin
  FBuildConfig.Free;
  FTestConfig.Free;
  FProfiles.Free;
  inherited Destroy;
end;

end.
