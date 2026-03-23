
{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}
unit PasBuild.Command.LazarusPackage;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  PasBuild.Types,
  PasBuild.Command,
  PasBuild.Utils,
  PasBuild.LazarusPackage;

type
  { lazarus-package command - builds a lpk and pas }

  { TLazarusPackageCommand }

  TLazarusPackageCommand = class(TBuildCommand)
  protected
    function GetName: string; override;
  public
    function Execute: Integer; override;
    function GetDependencies: TBuildCommandList; override;
  end;

implementation

uses
  PasBuild.Command.ProcessResources;


{ TLazarusPackageCommand }

function TLazarusPackageCommand.GetName: string;
begin
  Result := 'lazarus-package';
end;

function TLazarusPackageCommand.Execute: Integer;
var
  OutputDir, UnitsDir, ProfileId: String;
  ActiveDefines: TStringList;
  Profile: TProfile;
begin
  Result := 0;

  TUtils.LogInfo('Building Lazarus Package...');
  if Config.BuildConfig.ProjectType <> ptLibrary then
  begin
    TUtils.LogError('Goal lazarus-package invalid for project '  + Config.Name);
    Result := 0; // because perhaps it's part of a group of modules.
    Exit;
  end;

  // Verify directory layout
  if not TUtils.VerifyDirectoryLayout('.', Config.BuildConfig.SourceDirectory) then
  begin
    Result := 1;
    Exit;
  end;

  // Create output directories first (needed for bootstrap generation)
  OutputDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory);
  UnitsDir := OutputDir + DirectorySeparator + 'units';

  if not ForceDirectories(OutputDir) then
  begin
    TUtils.LogError('Failed to create output directory: ' + OutputDir);
    Result := 1;
    Exit;
  end;

  if not ForceDirectories(UnitsDir) then
  begin
    TUtils.LogError('Failed to create units directory: ' + UnitsDir);
    Result := 1;
    Exit;
  end;

  // Collect active defines (same logic as BuildCompilerCommand)
  ActiveDefines := TStringList.Create;
  try
    ActiveDefines.Duplicates := dupIgnore;
    ActiveDefines.Sorted := True;
    ActiveDefines.AddStrings(Config.BuildConfig.Defines);

    // Add defines from each active profile in order
    for ProfileId in ProfileIds do
    begin
      Profile := Config.Profiles.FindById(ProfileId);
      if Assigned(Profile) then
        ActiveDefines.AddStrings(Profile.Defines);
    end;
    if not TLazarusPackageGenerator.GenerateLazarusPackage(Config, ActiveDefines) then
    begin
      TUtils.LogError('Failed to generate lazarus package');
      Result := 1;
      Exit;
    end;
  finally
    ActiveDefines.Free;
  end;
end;

function TLazarusPackageCommand.GetDependencies: TBuildCommandList;
begin
  Result := TBuildCommandList.Create(False);
  try
    //
    //Result.Add(TProcessResourcesCommand.Create(Config, Config.ResourcesConfig, Config.BuildConfig.OutputDirectory));
  except
    Result.Free;
    raise;
  end;
end;

end.

