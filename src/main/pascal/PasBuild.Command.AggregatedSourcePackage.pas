{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Command.AggregatedSourcePackage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Zipper,
  PasBuild.Types,
  PasBuild.Command,
  PasBuild.Utils;

type
  { Aggregated source package command - creates source archive for multi-module projects }
  TAggregatedSourcePackageCommand = class(TBuildCommand)
  private
    FRegistry: TModuleRegistry;
    function GetArchiveFileName: string;
    function FindFileWithVariants(const ABaseName: string): string;
    procedure AddDirectoryToZip(AZip: TZipper; const ASourceDir, AArchivePrefix: string);
    procedure AddModuleToArchive(AZip: TZipper; AModule: TModuleInfo; const AArchivePrefix: string);
    function CreateAggregatedSourceArchive(const AArchiveName: string): Integer;
  protected
    function GetName: string; override;
  public
    constructor Create(AConfig: TProjectConfig; AProfileIds: TStringList;
      ARegistry: TModuleRegistry); reintroduce;
    function Execute: Integer; override;
    function GetDependencies: TBuildCommandList; override;
  end;

implementation

{ TAggregatedSourcePackageCommand }

constructor TAggregatedSourcePackageCommand.Create(AConfig: TProjectConfig; AProfileIds: TStringList;
  ARegistry: TModuleRegistry);
begin
  inherited Create(AConfig, AProfileIds);
  FRegistry := ARegistry;
end;

function TAggregatedSourcePackageCommand.GetName: string;
begin
  Result := 'aggregated-source-package';
end;

function TAggregatedSourcePackageCommand.GetDependencies: TBuildCommandList;
begin
  // Aggregated source package has no build dependencies
  Result := TBuildCommandList.Create;
  Result.FreeObjects := False;
end;

function TAggregatedSourcePackageCommand.GetArchiveFileName: string;
var
  BaseName, OutputDir: string;
begin
  // Archive name: target/<aggregator-name>-<version>-src.zip
  OutputDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory);

  BaseName := Config.Name;
  if BaseName = '' then
    BaseName := 'project';

  Result := OutputDir + DirectorySeparator + BaseName + '-' + Config.Version + '-src.zip';
end;

function TAggregatedSourcePackageCommand.FindFileWithVariants(const ABaseName: string): string;
const
  FileExtensions: array[0..4] of string = (
    '',           // No extension
    '.txt',
    '.md',
    '.adoc',
    '.rst'
  );
var
  Extension, FileName: string;
begin
  Result := '';

  for Extension in FileExtensions do
  begin
    FileName := ABaseName + Extension;
    if FileExists(FileName) then
    begin
      Result := FileName;
      Exit;
    end;
  end;
end;

procedure TAggregatedSourcePackageCommand.AddDirectoryToZip(AZip: TZipper; const ASourceDir, AArchivePrefix: string);
var
  SearchRec: TSearchRec;
  SourcePath, ArchivePath: string;
begin
  if FindFirst(ASourceDir + DirectorySeparator + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        // Skip . and ..
        if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
          Continue;

        SourcePath := ASourceDir + DirectorySeparator + SearchRec.Name;
        ArchivePath := AArchivePrefix + SearchRec.Name;

        if (SearchRec.Attr and faDirectory) <> 0 then
        begin
          // Recursively add subdirectory
          AddDirectoryToZip(AZip, SourcePath, ArchivePath + DirectorySeparator);
        end
        else
        begin
          // Add file
          TUtils.LogInfo('Adding to archive: ' + SourcePath);
          AZip.Entries.AddFileEntry(SourcePath, ArchivePath);
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

procedure TAggregatedSourcePackageCommand.AddModuleToArchive(AZip: TZipper; AModule: TModuleInfo; const AArchivePrefix: string);
var
  ProjectXmlPath, SrcDirPath, ModulePrefix, LicenseFile, ReadmeFile: string;
begin
  // Calculate module's archive path relative to aggregator
  ModulePrefix := AArchivePrefix + ExtractFileName(AModule.Path) + DirectorySeparator;

  // Add module's project.xml
  ProjectXmlPath := AModule.Path + DirectorySeparator + 'project.xml';
  if FileExists(ProjectXmlPath) then
  begin
    TUtils.LogInfo('Adding module config: ' + ProjectXmlPath);
    AZip.Entries.AddFileEntry(ProjectXmlPath, ModulePrefix + 'project.xml');
  end
  else
    TUtils.LogWarning('Module project.xml not found: ' + ProjectXmlPath);

  // Add module's src/ directory recursively
  SrcDirPath := AModule.Path + DirectorySeparator + 'src';
  if DirectoryExists(SrcDirPath) then
  begin
    TUtils.LogInfo('Adding module sources: ' + SrcDirPath);
    AddDirectoryToZip(AZip, SrcDirPath, ModulePrefix + 'src' + DirectorySeparator);
  end
  else
    TUtils.LogWarning('Module source directory not found: ' + SrcDirPath);

  // Add module's LICENSE file (optional)
  LicenseFile := FindFileWithVariants(AModule.Path + DirectorySeparator + 'LICENSE');
  if LicenseFile = '' then
    LicenseFile := FindFileWithVariants(AModule.Path + DirectorySeparator + 'COPYING');

  if LicenseFile <> '' then
  begin
    TUtils.LogInfo('Adding module license: ' + LicenseFile);
    AZip.Entries.AddFileEntry(LicenseFile, ModulePrefix + ExtractFileName(LicenseFile));
  end;

  // Add module's README file (optional)
  ReadmeFile := FindFileWithVariants(AModule.Path + DirectorySeparator + 'README');
  if ReadmeFile <> '' then
  begin
    TUtils.LogInfo('Adding module readme: ' + ReadmeFile);
    AZip.Entries.AddFileEntry(ReadmeFile, ModulePrefix + ExtractFileName(ReadmeFile));
  end;
end;

function TAggregatedSourcePackageCommand.CreateAggregatedSourceArchive(const AArchiveName: string): Integer;
var
  Zip: TZipper;
  I: Integer;
  Module: TModuleInfo;
  ArchivePrefix: string;
  ProjectXmlPath, LicenseFile, ReadmeFile: string;
begin
  Result := 0;

  // Archive prefix: <aggregator-name>-<version>/
  ArchivePrefix := Config.Name + '-' + Config.Version + DirectorySeparator;

  Zip := TZipper.Create;
  try
    Zip.FileName := AArchiveName;

    // Add aggregator's project.xml
    ProjectXmlPath := 'project.xml';
    if FileExists(ProjectXmlPath) then
    begin
      TUtils.LogInfo('Adding aggregator config: ' + ProjectXmlPath);
      Zip.Entries.AddFileEntry(ProjectXmlPath, ArchivePrefix + ProjectXmlPath);
    end
    else
    begin
      TUtils.LogError('Aggregator project.xml not found');
      Result := 1;
      Exit;
    end;

    // Add aggregator's LICENSE file
    LicenseFile := FindFileWithVariants('LICENSE');
    if LicenseFile = '' then
      LicenseFile := FindFileWithVariants('COPYING');

    if LicenseFile <> '' then
    begin
      TUtils.LogInfo('Adding aggregator license: ' + LicenseFile);
      Zip.Entries.AddFileEntry(LicenseFile, ArchivePrefix + ExtractFileName(LicenseFile));
    end
    else
      TUtils.LogInfo('Aggregator LICENSE/COPYING file not found, skipping');

    // Add aggregator's README file
    ReadmeFile := FindFileWithVariants('README');
    if ReadmeFile <> '' then
    begin
      TUtils.LogInfo('Adding aggregator readme: ' + ReadmeFile);
      Zip.Entries.AddFileEntry(ReadmeFile, ArchivePrefix + ExtractFileName(ReadmeFile));
    end
    else
      TUtils.LogInfo('Aggregator README file not found, skipping');

    // Add each module's sources
    for I := 0 to FRegistry.Modules.Count - 1 do
    begin
      Module := TModuleInfo(FRegistry.Modules[I]);

      // Skip nested aggregators (should not happen in MVP, but good for future-proofing)
      if Module.Config.BuildConfig.ProjectType = ptPom then
      begin
        TUtils.LogInfo('Skipping nested aggregator module: ' + Module.Name);
        Continue;
      end;

      AddModuleToArchive(Zip, Module, ArchivePrefix);
    end;

    // Create the archive
    if Zip.Entries.Count = 0 then
    begin
      TUtils.LogError('No files to include in aggregated source archive');
      Result := 1;
      Exit;
    end;

    Zip.ZipAllFiles;
    TUtils.LogInfo('Created aggregated source archive: ' + AArchiveName);

  finally
    Zip.Free;
  end;
end;

function TAggregatedSourcePackageCommand.Execute: Integer;
var
  ArchiveName: string;
  OutputDir: string;
begin
  Result := 0;

  TUtils.LogInfo('Creating aggregated source package for multi-module project...');

  // Ensure target directory exists
  OutputDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory);
  if not ForceDirectories(OutputDir) then
  begin
    TUtils.LogError('Failed to create output directory: ' + OutputDir);
    Result := 1;
    Exit;
  end;

  // Get archive filename
  ArchiveName := GetArchiveFileName;

  // Delete existing archive if present
  if FileExists(ArchiveName) then
  begin
    TUtils.LogInfo('Removing existing archive: ' + ArchiveName);
    if not DeleteFile(ArchiveName) then
    begin
      TUtils.LogError('Failed to delete existing archive: ' + ArchiveName);
      Result := 1;
      Exit;
    end;
  end;

  // Create the aggregated source archive
  Result := CreateAggregatedSourceArchive(ArchiveName);

  if Result = 0 then
    TUtils.LogInfo('Aggregated source package created successfully: ' + ArchiveName)
  else
    TUtils.LogError('Aggregated source package creation failed');
end;

end.
