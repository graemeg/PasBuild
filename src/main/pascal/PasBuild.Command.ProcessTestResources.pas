{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Command.ProcessTestResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  PasBuild.Types,
  PasBuild.Command;

type
  { Command to process test resources (copy + optional filtering) }
  TProcessTestResourcesCommand = class(TBuildCommand)
  private
    FResourcesConfig: TResourcesConfig;
    FTargetDir: string;

    function ProcessFile(const ASourceFile, ARelativePath: string): Boolean;
    function ApplyFiltering(const AContent: string): string;
    procedure CopyResources(const ASourceDir, ATargetDir: string);
  public
    constructor Create(AConfig: TProjectConfig; AResourcesConfig: TResourcesConfig; const ATargetDir: string);

    function GetName: string; override;
    function Execute: Integer; override;
  end;

implementation

uses
  PasBuild.Utils,
  StrUtils;

{ TProcessTestResourcesCommand }

constructor TProcessTestResourcesCommand.Create(AConfig: TProjectConfig; AResourcesConfig: TResourcesConfig; const ATargetDir: string);
begin
  inherited Create(AConfig, nil);
  FResourcesConfig := AResourcesConfig;
  FTargetDir := ATargetDir;
end;

function TProcessTestResourcesCommand.GetName: string;
begin
  Result := 'process-test-resources';
end;

function TProcessTestResourcesCommand.ApplyFiltering(const AContent: string): string;
begin
  Result := AContent;

  // Replace project variables
  Result := StringReplace(Result, '${project.name}', Config.Name, [rfReplaceAll]);
  Result := StringReplace(Result, '${project.version}', Config.Version, [rfReplaceAll]);
  Result := StringReplace(Result, '${project.author}', Config.Author, [rfReplaceAll]);
  Result := StringReplace(Result, '${project.license}', Config.License, [rfReplaceAll]);
end;

function TProcessTestResourcesCommand.ProcessFile(const ASourceFile, ARelativePath: string): Boolean;
var
  TargetFile: string;
  SourceContent, FilteredContent: string;
  SourceStream: TFileStream;
  TargetStream: TFileStream;
  TargetDir: string;
begin
  Result := False;

  // Build target file path
  TargetFile := IncludeTrailingPathDelimiter(FTargetDir) + ARelativePath;

  // Create target directory if needed
  TargetDir := ExtractFilePath(TargetFile);
  if not DirectoryExists(TargetDir) then
  begin
    if not ForceDirectories(TargetDir) then
    begin
      TUtils.LogError('Failed to create directory: ' + TargetDir);
      Exit;
    end;
  end;

  try
    if FResourcesConfig.Filtering then
    begin
      // Read file, apply filtering, write to target
      SourceStream := TFileStream.Create(ASourceFile, fmOpenRead or fmShareDenyWrite);
      try
        SetLength(SourceContent, SourceStream.Size);
        if SourceStream.Size > 0 then
          SourceStream.ReadBuffer(SourceContent[1], SourceStream.Size);
      finally
        SourceStream.Free;
      end;

      // Apply variable substitution
      FilteredContent := ApplyFiltering(SourceContent);

      // Write filtered content
      TargetStream := TFileStream.Create(TargetFile, fmCreate);
      try
        if Length(FilteredContent) > 0 then
          TargetStream.WriteBuffer(FilteredContent[1], Length(FilteredContent));
      finally
        TargetStream.Free;
      end;
    end
    else
    begin
      // Simple copy without filtering
      SourceStream := TFileStream.Create(ASourceFile, fmOpenRead or fmShareDenyWrite);
      try
        TargetStream := TFileStream.Create(TargetFile, fmCreate);
        try
          TargetStream.CopyFrom(SourceStream, SourceStream.Size);
        finally
          TargetStream.Free;
        end;
      finally
        SourceStream.Free;
      end;
    end;

    Result := True;
  except
    on E: Exception do
    begin
      TUtils.LogError('Failed to process test resource ' + ARelativePath + ': ' + E.Message);
      Exit;
    end;
  end;
end;

procedure TProcessTestResourcesCommand.CopyResources(const ASourceDir, ATargetDir: string);
var
  SearchRec: TSearchRec;
  SourcePath, RelativePath: string;
begin
  SourcePath := IncludeTrailingPathDelimiter(ASourceDir);

  // Process all files in current directory
  if FindFirst(SourcePath + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        // Skip . and ..
        if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
          Continue;

        RelativePath := SearchRec.Name;

        if (SearchRec.Attr and faDirectory) = faDirectory then
        begin
          // Recursively process subdirectory
          CopyResources(SourcePath + SearchRec.Name, ATargetDir);
        end
        else
        begin
          // Calculate relative path from source root
          RelativePath := Copy(SourcePath + SearchRec.Name, Length(FResourcesConfig.Directory) + 2, MaxInt);

          // Process file
          if ProcessFile(SourcePath + SearchRec.Name, RelativePath) then
          begin
            if FResourcesConfig.Filtering then
              TUtils.LogInfo('  Filtered: ' + RelativePath)
            else
              TUtils.LogInfo('  Copied: ' + RelativePath);
          end;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

function TProcessTestResourcesCommand.Execute: Integer;
var
  SourceDir: string;
begin
  Result := 1; // Assume failure

  SourceDir := FResourcesConfig.Directory;

  // Check if source directory exists
  if not DirectoryExists(SourceDir) then
  begin
    TUtils.LogInfo('No test resources directory found (' + SourceDir + '), skipping');
    Result := 0; // Not an error, just nothing to do
    Exit;
  end;

  TUtils.LogInfo('Processing test resources from ' + SourceDir + '...');
  if FResourcesConfig.Filtering then
    TUtils.LogInfo('  Filtering enabled');

  // Copy resources
  CopyResources(SourceDir, FTargetDir);

  TUtils.LogInfo('Test resources processed successfully');
  Result := 0;
end;

end.
