{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Remote.Builder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  PasBuild.Dependencies,
  PasBuild.Remote.Manifest;

type
  EBuildPipelineError = class(EDependencyError);

  TRemoteBuilder = class
  private
    FVerbose: Boolean;
    FConnectTimeout: Integer;
    FReadTimeout: Integer;
  public
    constructor Create(AConnectTimeout: Integer = 30;
      AReadTimeout: Integer = 60);

    function BuildAndInstall(AManifest: TReleaseManifest): string;

    class function DetectProjectRoot(const AExtractDir: string): string;
    class procedure ExtractArchive(const AZipPath, ADestDir: string);
    class function GetDownloadCachePath(const AName, AVersion: string): string;
    class function GetBuildExtractPath(const AName, AVersion: string): string;

    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

uses
  Zipper,
  PasBuild.Utils,
  PasBuild.Remote,
  PasBuild.Crypto.SHA256;

function DeleteDirectoryRecursive(const ADirName: string): Boolean;
var
  SearchRec: TSearchRec;
  FilePath: string;
begin
  Result := True;
  if FindFirst(IncludeTrailingPathDelimiter(ADirName) + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          FilePath := IncludeTrailingPathDelimiter(ADirName) + SearchRec.Name;
          if (SearchRec.Attr and faDirectory) = faDirectory then
          begin
            if not DeleteDirectoryRecursive(FilePath) then
              Result := False;
          end
          else
          begin
            if not DeleteFile(FilePath) then
              Result := False;
          end;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
  if Result then
    Result := RemoveDir(ADirName);
end;

{ TRemoteBuilder }

constructor TRemoteBuilder.Create(AConnectTimeout: Integer;
  AReadTimeout: Integer);
begin
  inherited Create;
  FVerbose := False;
  FConnectTimeout := AConnectTimeout;
  FReadTimeout := AReadTimeout;
end;

class function TRemoteBuilder.GetDownloadCachePath(
  const AName, AVersion: string): string;
begin
  Result := TUtils.GetDownloadCacheDir + DirectorySeparator +
    AName + '-' + AVersion + '-src.zip';
end;

class function TRemoteBuilder.GetBuildExtractPath(
  const AName, AVersion: string): string;
begin
  Result := TUtils.GetBuildCacheDir + DirectorySeparator +
    AName + '-' + AVersion;
end;

class procedure TRemoteBuilder.ExtractArchive(const AZipPath, ADestDir: string);
var
  Unzip: TUnZipper;
begin
  Unzip := TUnZipper.Create;
  try
    Unzip.FileName := AZipPath;
    Unzip.OutputPath := ADestDir;
    Unzip.Examine;
    Unzip.UnZipAllFiles;
  finally
    Unzip.Free;
  end;
end;

class function TRemoteBuilder.DetectProjectRoot(
  const AExtractDir: string): string;
var
  SearchRec: TSearchRec;
  SubDirs: TStringList;
  ProjectXmlPath: string;
begin
  ProjectXmlPath := IncludeTrailingPathDelimiter(AExtractDir) + 'project.xml';
  if FileExists(ProjectXmlPath) then
    Exit(ExcludeTrailingPathDelimiter(AExtractDir));

  SubDirs := TStringList.Create;
  try
    if FindFirst(IncludeTrailingPathDelimiter(AExtractDir) + '*',
      faDirectory, SearchRec) = 0 then
    begin
      try
        repeat
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
             ((SearchRec.Attr and faDirectory) = faDirectory) then
            SubDirs.Add(SearchRec.Name);
        until FindNext(SearchRec) <> 0;
      finally
        FindClose(SearchRec);
      end;
    end;

    if SubDirs.Count = 1 then
    begin
      ProjectXmlPath := IncludeTrailingPathDelimiter(AExtractDir) +
        SubDirs[0] + DirectorySeparator + 'project.xml';
      if FileExists(ProjectXmlPath) then
        Exit(IncludeTrailingPathDelimiter(AExtractDir) + SubDirs[0]);
    end;
  finally
    SubDirs.Free;
  end;

  raise EBuildPipelineError.CreateFmt(
    'Cannot find project.xml in extracted archive at "%s". ' +
    'The archive must contain project.xml at the root or inside a single ' +
    'top-level directory.', [AExtractDir]);
end;

function TRemoteBuilder.BuildAndInstall(
  AManifest: TReleaseManifest): string;
var
  DownloadPath, ExtractDir, ProjectRoot: string;
  Client: TRemoteClient;
  PasBuildExe, CompilerExe, Output: string;
  SubArgs: array of string;
  ExitCode: Integer;

  procedure BuildSubprocessArgs(const AGoal: string);
  begin
    if CompilerExe <> '' then
    begin
      SetLength(SubArgs, 3);
      SubArgs[0] := AGoal;
      SubArgs[1] := '--compiler';
      SubArgs[2] := CompilerExe;
    end
    else
    begin
      SetLength(SubArgs, 1);
      SubArgs[0] := AGoal;
    end;
  end;

begin
  // Defence in depth: the manifest parser validates these already, but the
  // values are about to become filesystem paths and a recursive delete target
  if not TUtils.IsValidPackageName(AManifest.Name) then
    raise EBuildPipelineError.CreateFmt(
      'Refusing to build package with invalid name: "%s"', [AManifest.Name]);
  if not TUtils.IsValidPackageVersion(AManifest.Version) then
    raise EBuildPipelineError.CreateFmt(
      'Refusing to build package "%s" with invalid version: "%s"',
      [AManifest.Name, AManifest.Version]);

  DownloadPath := GetDownloadCachePath(AManifest.Name, AManifest.Version);
  ExtractDir := GetBuildExtractPath(AManifest.Name, AManifest.Version);

  ForceDirectories(ExtractFileDir(DownloadPath));

  if not FileExists(DownloadPath) then
  begin
    TUtils.LogInfo('  Downloading ' + AManifest.Name + ':' +
      AManifest.Version + '...');

    Client := TRemoteClient.Create(FConnectTimeout, FReadTimeout);
    try
      Client.DownloadFile(AManifest.OriginUrl, DownloadPath,
        AManifest.OriginSHA256);
    finally
      Client.Free;
    end;

    TUtils.LogInfo('  Download complete, checksum verified');
  end
  else
  begin
    TUtils.LogInfo('  Using cached archive: ' + DownloadPath);
    if AManifest.OriginSHA256 <> '' then
    begin
      if LowerCase(SHA256File(DownloadPath)) <>
         LowerCase(AManifest.OriginSHA256) then
      begin
        DeleteFile(DownloadPath);
        raise EBuildPipelineError.CreateFmt(
          'Cached archive for "%s:%s" failed checksum verification. ' +
          'The cached file has been removed. Retry to re-download.',
          [AManifest.Name, AManifest.Version]);
      end;
    end;
  end;

  if DirectoryExists(ExtractDir) then
    DeleteDirectoryRecursive(ExtractDir);
  ForceDirectories(ExtractDir);

  TUtils.LogInfo('  Extracting archive...');
  ExtractArchive(DownloadPath, ExtractDir);

  ProjectRoot := DetectProjectRoot(ExtractDir);
  TUtils.LogInfo('  Project root: ' + ProjectRoot);

  PasBuildExe := TUtils.GetSelfExePath;

  // Forward the resolved compiler so the subprocess installs under the
  // same target quadruplet this process will look the artifact up with
  CompilerExe := '';
  if Assigned(TUtils.GetCompilerBackend) then
    CompilerExe := TUtils.GetCompilerBackend.Executable;

  TUtils.LogInfo('  Compiling ' + AManifest.Name + ':' +
    AManifest.Version + '...');
  BuildSubprocessArgs('compile');
  ExitCode := TUtils.ExecuteProcessInDirWithCapture(
    PasBuildExe, SubArgs, ProjectRoot, Output);
  if ExitCode <> 0 then
  begin
    TUtils.LogError('  Compile output:');
    TUtils.LogError(Output);
    raise EBuildPipelineError.CreateFmt(
      'Failed to compile remote dependency "%s:%s" (exit code %d)',
      [AManifest.Name, AManifest.Version, ExitCode]);
  end;

  if FVerbose then
    TUtils.LogInfo('  Compile output: ' + Output);

  TUtils.LogInfo('  Installing ' + AManifest.Name + ':' +
    AManifest.Version + '...');
  BuildSubprocessArgs('install');
  ExitCode := TUtils.ExecuteProcessInDirWithCapture(
    PasBuildExe, SubArgs, ProjectRoot, Output);
  if ExitCode <> 0 then
  begin
    TUtils.LogError('  Install output:');
    TUtils.LogError(Output);
    raise EBuildPipelineError.CreateFmt(
      'Failed to install remote dependency "%s:%s" (exit code %d)',
      [AManifest.Name, AManifest.Version, ExitCode]);
  end;

  if FVerbose then
    TUtils.LogInfo('  Install output: ' + Output);

  TUtils.LogInfo('  Cleaning up build directory...');
  DeleteDirectoryRecursive(ExtractDir);

  Result := TUtils.GetDownloadCacheDir;
  TUtils.LogInfo('  Installed ' + AManifest.Name + ':' +
    AManifest.Version + ' successfully');
end;

end.
