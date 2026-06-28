{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Command.Install;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  PasBuild.Types,
  PasBuild.Command;

type
  { Install command - installs compiled artifacts to local repository }
  TInstallCommand = class(TBuildCommand)
  protected
    function GetName: string; override;
  public
    function Execute: Integer; override;
    function GetDependencies: TBuildCommandList; override;
  end;

implementation

uses
  PasBuild.Utils,
  PasBuild.Repository,
  PasBuild.Command.Compile;

{ TInstallCommand }

function TInstallCommand.GetName: string;
begin
  Result := 'install';
end;

function TInstallCommand.Execute: Integer;
var
  TargetId, UnitsDir: string;
  Repo: TLocalRepository;
  Metadata: TArtifactMetadata;
  DepArray: TMetadataDependencyArray;
  I, PPUCount, OCount: Integer;
  PackagingStr: string;
begin
  Result := 0;

  TUtils.LogInfo('Installing to local repository...');

  if Config.BuildConfig.ProjectType = ptPom then
  begin
    TUtils.LogError('Cannot install aggregator project (packaging=pom). Install individual modules instead.');
    Result := 1;
    Exit;
  end;

  try
    TargetId := TUtils.GetTargetQuadruplet;
  except
    on E: Exception do
    begin
      TUtils.LogError('Failed to detect compiler target: ' + E.Message);
      Result := 1;
      Exit;
    end;
  end;

  UnitsDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory) +
              DirectorySeparator + 'units';
  if not DirectoryExists(UnitsDir) then
  begin
    TUtils.LogError('Units directory not found: ' + UnitsDir);
    TUtils.LogError('Run "pasbuild compile" first.');
    Result := 1;
    Exit;
  end;

  Repo := TLocalRepository.Create;
  try
    PPUCount := Repo.CountFiles(UnitsDir, '*.ppu');
    OCount := Repo.CountFiles(UnitsDir, '*.o');
    if PPUCount = 0 then
    begin
      TUtils.LogError('No compiled units (.ppu) found in: ' + UnitsDir);
      Result := 1;
      Exit;
    end;

    TUtils.LogInfo('Found ' + IntToStr(PPUCount) + ' unit(s) and ' +
                   IntToStr(OCount) + ' object file(s)');

    try
      Repo.EnsureDirectoryStructure(Config.Name, Config.Version, TargetId);
    except
      on E: Exception do
      begin
        TUtils.LogError('Failed to create repository directory: ' + E.Message);
        Result := 1;
        Exit;
      end;
    end;

    try
      Repo.CopyUnitFiles(UnitsDir, Config.Name, Config.Version, TargetId);
      TUtils.LogInfo('Copied unit files to repository');
    except
      on E: Exception do
      begin
        TUtils.LogError('Failed to copy unit files: ' + E.Message);
        Result := 1;
        Exit;
      end;
    end;

    Metadata := TArtifactMetadata.Create;
    try
      Metadata.Name := Config.Name;
      Metadata.Version := Config.Version;

      case Config.BuildConfig.ProjectType of
        ptLibrary: PackagingStr := 'library';
        ptApplication: PackagingStr := 'application';
        else PackagingStr := 'unknown';
      end;
      Metadata.Packaging := PackagingStr;

      Metadata.Compiler := TUtils.DetectCompilerName;
      Metadata.CompilerVersion := TUtils.DetectCompilerVersion;
      Metadata.TargetCPU := TUtils.DetectTargetCPU;
      Metadata.TargetOS := TUtils.DetectTargetOS;
      Metadata.Timestamp := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now);

      SetLength(DepArray, Config.Dependencies.Count + Config.ModuleDependencies.Count);

      for I := 0 to Config.Dependencies.Count - 1 do
      begin
        DepArray[I].Name := Config.Dependencies[I].Name;
        DepArray[I].Version := Config.Dependencies[I].Version;
      end;

      for I := 0 to Config.ModuleDependencies.Count - 1 do
      begin
        DepArray[Config.Dependencies.Count + I].Name := Config.ModuleDependencies[I];
        DepArray[Config.Dependencies.Count + I].Version := Config.Version;
      end;

      Metadata.Dependencies := DepArray;

      try
        Repo.WriteMetadata(Config.Name, Config.Version, TargetId, Metadata);
      except
        on E: Exception do
        begin
          TUtils.LogError('Failed to write metadata: ' + E.Message);
          Result := 1;
          Exit;
        end;
      end;
    finally
      Metadata.Free;
    end;

    TUtils.LogInfo('Installed ' + Config.Name + ':' + Config.Version +
                   ' [' + TargetId + ']');
    TUtils.LogInfo('  -> ' + Repo.GetArtifactPath(
                   Config.Name, Config.Version, TargetId));
  finally
    Repo.Free;
  end;
end;

function TInstallCommand.GetDependencies: TBuildCommandList;
begin
  Result := TBuildCommandList.Create(False);
  try
    // install depends on: compile (which itself depends on process-resources)
    Result.Add(TCompileCommand.Create(Config, ProfileIds));
  except
    Result.Free;
    raise;
  end;
end;

end.
