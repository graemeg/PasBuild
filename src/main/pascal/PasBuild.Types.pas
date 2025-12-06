unit PasBuild.Types;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl;

type
  { Forward declarations }
  TProfile = class;
  TBuildConfig = class;
  TProjectConfig = class;

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
    FMainSource: string;
    FOutputDirectory: string;
    FExecutableName: string;
    FDefines: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    property MainSource: string read FMainSource write FMainSource;
    property OutputDirectory: string read FOutputDirectory write FOutputDirectory;
    property ExecutableName: string read FExecutableName write FExecutableName;
    property Defines: TStringList read FDefines;
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
    property Profiles: TProfileList read FProfiles;
  end;

implementation

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

  // Set defaults
  FOutputDirectory := 'target';
end;

destructor TBuildConfig.Destroy;
begin
  FDefines.Free;
  inherited Destroy;
end;

{ TProjectConfig }

constructor TProjectConfig.Create;
begin
  inherited Create;
  FBuildConfig := TBuildConfig.Create;
  FProfiles := TProfileList.Create;
  FProfiles.FreeObjects := True;

  // Set defaults
  FAuthor := 'Unknown';
  FLicense := 'Proprietary';
end;

destructor TProjectConfig.Destroy;
begin
  FBuildConfig.Free;
  FProfiles.Free;
  inherited Destroy;
end;

end.
