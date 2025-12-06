{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Command.Clean;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  PasBuild.Types,
  PasBuild.Command,
  PasBuild.Utils;

type
  { Clean command - deletes build artifacts }
  TCleanCommand = class(TBuildCommand)
  protected
    function GetName: string; override;
  public
    function Execute: Integer; override;
  end;

implementation

{ Helper function to recursively delete directory }
function DeleteDirectory(const DirName: string): Boolean;
var
  SearchRec: TSearchRec;
  FilePath: string;
begin
  Result := True;

  if FindFirst(IncludeTrailingPathDelimiter(DirName) + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          FilePath := IncludeTrailingPathDelimiter(DirName) + SearchRec.Name;

          if (SearchRec.Attr and faDirectory) = faDirectory then
          begin
            // Recursively delete subdirectory
            if not DeleteDirectory(FilePath) then
              Result := False;
          end
          else
          begin
            // Delete file
            if not DeleteFile(FilePath) then
              Result := False;
          end;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;

  // Remove the directory itself
  if Result then
    Result := RemoveDir(DirName);
end;

{ TCleanCommand }

function TCleanCommand.GetName: string;
begin
  Result := 'clean';
end;

function TCleanCommand.Execute: Integer;
var
  OutputDir: string;
  ProjectRoot: string;
begin
  Result := 0;

  TUtils.LogInfo('Cleaning project...');

  // Get output directory from config
  OutputDir := Config.BuildConfig.OutputDirectory;

  // Normalize path for cross-platform compatibility
  OutputDir := TUtils.NormalizePath(OutputDir);

  // Get project root (current directory)
  ProjectRoot := GetCurrentDir;

  // Build full path to output directory (if relative)
  // Check if path is relative (doesn't start with / or drive letter)
  if not ((Length(OutputDir) > 0) and
          ((OutputDir[1] = DirectorySeparator) or
           ((Length(OutputDir) > 1) and (OutputDir[2] = ':')))) then
    OutputDir := IncludeTrailingPathDelimiter(ProjectRoot) + OutputDir;

  // Safety check: only delete if it's the configured output directory
  // and it's under the project root
  if not AnsiStartsText(ProjectRoot, OutputDir) then
  begin
    TUtils.LogError('Safety check failed: Output directory is outside project root');
    TUtils.LogError('Project root: ' + ProjectRoot);
    TUtils.LogError('Output directory: ' + OutputDir);
    Result := 1;
    Exit;
  end;

  // Additional safety: don't delete project root itself
  if SameText(OutputDir, ProjectRoot) or
     SameText(OutputDir, IncludeTrailingPathDelimiter(ProjectRoot)) then
  begin
    TUtils.LogError('Safety check failed: Cannot delete project root directory');
    Result := 1;
    Exit;
  end;

  // Check if directory exists
  if not DirectoryExists(OutputDir) then
  begin
    TUtils.LogInfo('Output directory does not exist: ' + OutputDir);
    TUtils.LogInfo('Nothing to clean');
    Exit;
  end;

  TUtils.LogInfo('Deleting: ' + OutputDir);

  // Delete the directory
  try
    if not DeleteDirectory(OutputDir) then
    begin
      TUtils.LogError('Failed to delete directory: ' + OutputDir);
      Result := 1;
      Exit;
    end;
  except
    on E: Exception do
    begin
      TUtils.LogError('Error deleting directory: ' + E.Message);
      Result := 1;
      Exit;
    end;
  end;

  TUtils.LogInfo('Clean complete');
end;

end.
