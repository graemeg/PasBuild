unit PasBuild.Command.Package;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Zipper,
  PasBuild.Types,
  PasBuild.Command,
  PasBuild.Command.Clean,
  PasBuild.Command.Compile,
  PasBuild.Utils;

type
  { Package command - creates release archive }
  TPackageCommand = class(TBuildCommand)
  private
    function GetArchiveFileName: string;
    function FindFileWithVariants(const ABaseName: string): string;
    function CreateArchive(const AArchiveName: string): Integer;
  protected
    function GetName: string; override;
  public
    function Execute: Integer; override;
    function GetDependencies: TBuildCommandList; override;
  end;

implementation

{ TPackageCommand }

function TPackageCommand.GetName: string;
begin
  Result := 'package';
end;

function TPackageCommand.GetDependencies: TBuildCommandList;
begin
  Result := TBuildCommandList.Create(False); // Don't own the objects
  try
    // Package depends on: clean → compile
    Result.Add(TCleanCommand.Create(Config, ProfileId));
    Result.Add(TCompileCommand.Create(Config, ProfileId));
  except
    Result.Free;
    raise;
  end;
end;

function TPackageCommand.GetArchiveFileName: string;
var
  BaseName, OutputDir: string;
begin
  // Archive name: target/<executableName>-<version>.zip
  // Following Maven convention: all build artifacts go in target/
  OutputDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory);

  BaseName := Config.BuildConfig.ExecutableName;
  if BaseName = '' then
    BaseName := 'app';

  Result := OutputDir + DirectorySeparator + BaseName + '-' + Config.Version + '.zip';
end;

function TPackageCommand.FindFileWithVariants(const ABaseName: string): string;
const
  // Common documentation file extensions (plus no extension)
  FileExtensions: array[0..4] of string = (
    '',           // No extension (e.g., README, LICENSE)
    '.txt',
    '.md',
    '.adoc',
    '.rst'
  );
var
  Extension, FileName: string;
begin
  Result := '';

  // Try each extension variant
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

function TPackageCommand.CreateArchive(const AArchiveName: string): Integer;
var
  Zip: TZipper;
  OutputDir, ExeName, ExePath: string;
  LicenseFile, ReadmeFile: string;
  FilesToAdd: TStringList;
  FileToAdd: string;
begin
  Result := 0;

  // Get executable path
  OutputDir := TUtils.NormalizePath(Config.BuildConfig.OutputDirectory);
  ExeName := Config.BuildConfig.ExecutableName;
  if ExeName = '' then
    ExeName := 'app';
  ExeName := ExeName + TUtils.GetPlatformExecutableSuffix;
  ExePath := OutputDir + DirectorySeparator + ExeName;

  // Check if executable exists
  if not FileExists(ExePath) then
  begin
    TUtils.LogError('Executable not found: ' + ExePath);
    Result := 1;
    Exit;
  end;

  // Build list of files to include
  FilesToAdd := TStringList.Create;
  try
    // Always include the executable
    FilesToAdd.Add(ExePath);

    // Include LICENSE if it exists (try LICENSE and COPYING)
    LicenseFile := FindFileWithVariants('LICENSE');
    if LicenseFile = '' then
      LicenseFile := FindFileWithVariants('COPYING');

    if LicenseFile <> '' then
      FilesToAdd.Add(LicenseFile)
    else
      TUtils.LogInfo('LICENSE/COPYING file not found, skipping');

    // Include README if it exists
    ReadmeFile := FindFileWithVariants('README');
    if ReadmeFile <> '' then
      FilesToAdd.Add(ReadmeFile)
    else
      TUtils.LogInfo('README file not found, skipping');

    // Create the ZIP archive
    Zip := TZipper.Create;
    try
      Zip.FileName := AArchiveName;

      // Add files with flat structure (no directories)
      for FileToAdd in FilesToAdd do
      begin
        TUtils.LogInfo('Adding to archive: ' + FileToAdd);
        Zip.Entries.AddFileEntry(FileToAdd, ExtractFileName(FileToAdd));
      end;

      // Create the archive
      Zip.ZipAllFiles;

      TUtils.LogInfo('Created archive: ' + AArchiveName);

    finally
      Zip.Free;
    end;

  finally
    FilesToAdd.Free;
  end;
end;

function TPackageCommand.Execute: Integer;
var
  ArchiveName: string;
begin
  Result := 0;

  TUtils.LogInfo('Creating release package...');

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

  // Create the archive
  Result := CreateArchive(ArchiveName);

  if Result = 0 then
    TUtils.LogInfo('Package created successfully: ' + ArchiveName)
  else
    TUtils.LogError('Package creation failed');
end;

end.
