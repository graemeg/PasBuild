{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Command.SourcePackage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Zipper,
  PasBuild.Types,
  PasBuild.Command,
  PasBuild.Utils;

type
  { SourcePackage command - creates source archive }
  TSourcePackageCommand = class(TBuildCommand)
  private
    function GetArchiveFileName: string;
    function FindFileWithVariants(const ABaseName: string): string;
    function IsPathSafe(const APath: string): Boolean;
    procedure AddDirectoryToZip(AZip: TZipper; const ASourceDir, AArchivePrefix: string);
    function CreateSourceArchive(const AArchiveName: string): Integer;
  protected
    function GetName: string; override;
  public
    function Execute: Integer; override;
  end;

implementation

{ TSourcePackageCommand }

function TSourcePackageCommand.GetName: string;
begin
  Result := 'source-package';
end;

function TSourcePackageCommand.GetArchiveFileName: string;
var
  BaseName, OutputDir: string;
begin
  // Archive name: target/<projectName>-<version>-src.zip
  OutputDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory);

  BaseName := Config.Name;
  if BaseName = '' then
    BaseName := 'project';

  Result := OutputDir + DirectorySeparator + BaseName + '-' + Config.Version + '-src.zip';
end;

function TSourcePackageCommand.FindFileWithVariants(const ABaseName: string): string;
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

function TSourcePackageCommand.IsPathSafe(const APath: string): Boolean;
var
  NormalizedPath: string;
begin
  // Security: Only allow paths within project root (no .. or absolute paths)
  NormalizedPath := TUtils.NormalizePath(APath);

  // Reject absolute paths
  if (Length(NormalizedPath) > 0) and (NormalizedPath[1] = DirectorySeparator) then
  begin
    Result := False;
    Exit;
  end;

  {$IFDEF WINDOWS}
  // Reject Windows absolute paths (C:\, D:\, etc.)
  if (Length(NormalizedPath) > 1) and (NormalizedPath[2] = ':') then
  begin
    Result := False;
    Exit;
  end;
  {$ENDIF}

  // Reject paths with .. (parent directory)
  if Pos('..', NormalizedPath) > 0 then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

procedure TSourcePackageCommand.AddDirectoryToZip(AZip: TZipper; const ASourceDir, AArchivePrefix: string);
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

function TSourcePackageCommand.CreateSourceArchive(const AArchiveName: string): Integer;
var
  Zip: TZipper;
  LicenseFile, ReadmeFile, BootstrapFile, InstallFile: string;
  IncludeDir: string;
  ArchivePrefix: string;
begin
  Result := 0;

  // Archive prefix: <projectName>-<version>/
  ArchivePrefix := Config.Name + '-' + Config.Version + DirectorySeparator;

  Zip := TZipper.Create;
  try
    Zip.FileName := AArchiveName;

    // DEFAULT INCLUDES

    // 1. src/ directory (all source code)
    if DirectoryExists('src') then
    begin
      TUtils.LogInfo('Adding src/ directory');
      AddDirectoryToZip(Zip, 'src', ArchivePrefix + 'src' + DirectorySeparator);
    end
    else
      TUtils.LogWarning('src/ directory not found');

    // 2. project.xml
    if FileExists('project.xml') then
    begin
      TUtils.LogInfo('Adding to archive: project.xml');
      Zip.Entries.AddFileEntry('project.xml', ArchivePrefix + 'project.xml');
    end
    else
      TUtils.LogWarning('project.xml not found');

    // 3. LICENSE file variants
    LicenseFile := FindFileWithVariants('LICENSE');
    if LicenseFile = '' then
      LicenseFile := FindFileWithVariants('COPYING');

    if LicenseFile <> '' then
    begin
      TUtils.LogInfo('Adding to archive: ' + LicenseFile);
      Zip.Entries.AddFileEntry(LicenseFile, ArchivePrefix + ExtractFileName(LicenseFile));
    end
    else
      TUtils.LogInfo('LICENSE/COPYING file not found, skipping');

    // 4. README file variants
    ReadmeFile := FindFileWithVariants('README');
    if ReadmeFile <> '' then
    begin
      TUtils.LogInfo('Adding to archive: ' + ReadmeFile);
      Zip.Entries.AddFileEntry(ReadmeFile, ArchivePrefix + ExtractFileName(ReadmeFile));
    end
    else
      TUtils.LogInfo('README file not found, skipping');

    // 5. BOOTSTRAP file variants
    BootstrapFile := FindFileWithVariants('BOOTSTRAP');
    if BootstrapFile <> '' then
    begin
      TUtils.LogInfo('Adding to archive: ' + BootstrapFile);
      Zip.Entries.AddFileEntry(BootstrapFile, ArchivePrefix + ExtractFileName(BootstrapFile));
    end;

    // 6. INSTALL file variants
    InstallFile := FindFileWithVariants('INSTALL');
    if InstallFile <> '' then
    begin
      TUtils.LogInfo('Adding to archive: ' + InstallFile);
      Zip.Entries.AddFileEntry(InstallFile, ArchivePrefix + ExtractFileName(InstallFile));
    end;

    // OPTIONAL INCLUDES (from configuration)

    for IncludeDir in Config.SourcePackageConfig.IncludeDirs do
    begin
      // Security check: Only allow safe paths
      if not IsPathSafe(IncludeDir) then
      begin
        TUtils.LogError('Security violation: Unsafe path in sourcePackage config: ' + IncludeDir);
        TUtils.LogError('Paths must be relative and within project root (no .., no absolute paths)');
        Result := 1;
        Exit;
      end;

      if DirectoryExists(IncludeDir) then
      begin
        TUtils.LogInfo('Adding configured directory: ' + IncludeDir);
        AddDirectoryToZip(Zip, IncludeDir, ArchivePrefix + IncludeDir + DirectorySeparator);
      end
      else if FileExists(IncludeDir) then
      begin
        TUtils.LogInfo('Adding configured file: ' + IncludeDir);
        Zip.Entries.AddFileEntry(IncludeDir, ArchivePrefix + IncludeDir);
      end
      else
        TUtils.LogWarning('Configured include not found: ' + IncludeDir);
    end;

    // Create the archive
    if Zip.Entries.Count = 0 then
    begin
      TUtils.LogError('No files to include in source archive');
      Result := 1;
      Exit;
    end;

    Zip.ZipAllFiles;
    TUtils.LogInfo('Created source archive: ' + AArchiveName);

  finally
    Zip.Free;
  end;
end;

function TSourcePackageCommand.Execute: Integer;
var
  ArchiveName: string;
  OutputDir: string;
begin
  Result := 0;

  TUtils.LogInfo('Creating source package...');

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

  // Create the source archive
  Result := CreateSourceArchive(ArchiveName);

  if Result = 0 then
    TUtils.LogInfo('Source package created successfully: ' + ArchiveName)
  else
    TUtils.LogError('Source package creation failed');
end;

end.
