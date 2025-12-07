{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Bootstrap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  PasBuild.Types,
  PasBuild.Utils;

type
  { Bootstrap program generator for library projects }
  TBootstrapGenerator = class
  public
    class function GenerateBootstrapProgram(
      AConfig: TProjectConfig;
      const AOutputPath: string;
      AActiveDefines: TStringList
    ): Boolean;
  private
    class function DiscoverUnits(
      const ABasePath: string;
      AUnitPaths: TConditionalPathList;
      AManualMode: Boolean;
      AActiveDefines: TStringList
    ): TStringList;
    class function ParseUnitName(const AFilePath: string): string;
  end;

implementation

{ TBootstrapGenerator }

class function TBootstrapGenerator.ParseUnitName(const AFilePath: string): string;
var
  F: TextFile;
  Line: string;
  UnitPos: Integer;
  InBraceComment: Boolean;
  InParenComment: Boolean;
  CommentEndPos: Integer;
begin
  Result := '';

  if not FileExists(AFilePath) then
    Exit;

  try
    AssignFile(F, AFilePath);
    Reset(F);
    try
      InBraceComment := False;
      InParenComment := False;

      // Read first few lines looking for "unit <name>;"
      while not EOF(F) and (Result = '') do
      begin
        ReadLn(F, Line);
        Line := Trim(Line);

        // Skip empty lines
        if Line = '' then
          Continue;

        // Track multi-line comment state for { ... }
        if InBraceComment then
        begin
          CommentEndPos := Pos('}', Line);
          if CommentEndPos > 0 then
          begin
            Delete(Line, 1, CommentEndPos);  // Remove everything up to and including }
            Line := Trim(Line);
            InBraceComment := False;
            if Line = '' then
              Continue;
          end
          else
            Continue;  // Still inside comment block
        end;

        // Track multi-line comment state for (* ... *)
        if InParenComment then
        begin
          CommentEndPos := Pos('*)', Line);
          if CommentEndPos > 0 then
          begin
            Delete(Line, 1, CommentEndPos + 1);  // Remove everything up to and including *)
            Line := Trim(Line);
            InParenComment := False;
            if Line = '' then
              Continue;
          end
          else
            Continue;  // Still inside comment block
        end;

        // Check if line starts a multi-line comment
        if AnsiStartsStr('{', Line) then
        begin
          CommentEndPos := Pos('}', Line);
          if CommentEndPos > 0 then
          begin
            // Single-line comment, remove it
            Delete(Line, 1, CommentEndPos);
            Line := Trim(Line);
            if Line = '' then
              Continue;
          end
          else
          begin
            InBraceComment := True;
            Continue;
          end;
        end;

        if AnsiStartsStr('(*', Line) then
        begin
          CommentEndPos := Pos('*)', Line);
          if CommentEndPos > 0 then
          begin
            // Single-line comment, remove it
            Delete(Line, 1, CommentEndPos + 1);
            Line := Trim(Line);
            if Line = '' then
              Continue;
          end
          else
          begin
            InParenComment := True;
            Continue;
          end;
        end;

        // Skip single-line // comments
        if AnsiStartsStr('//', Line) then
          Continue;

        // Look for "unit <name>;"
        if AnsiStartsStr('unit ', LowerCase(Line)) then
        begin
          UnitPos := Pos('unit ', LowerCase(Line));
          if UnitPos > 0 then
          begin
            Delete(Line, 1, UnitPos + 4);  // Remove "unit "
            Line := Trim(Line);

            // Remove trailing semicolon
            if AnsiEndsStr(';', Line) then
              Delete(Line, Length(Line), 1);

            Result := Trim(Line);
          end;
        end;
      end;
    finally
      CloseFile(F);
    end;
  except
    // If we can't read the file, skip it
    Result := '';
  end;
end;

class function TBootstrapGenerator.DiscoverUnits(
  const ABasePath: string;
  AUnitPaths: TConditionalPathList;
  AManualMode: Boolean;
  AActiveDefines: TStringList
): TStringList;
var
  AllDirs, Units: TStringList;
  Dir, UnitFile, DiscoveredUnit: string;
  SearchRec: TSearchRec;
  ConditionalPath: TConditionalPath;
  I: Integer;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;
  Result.Sorted := True;

  Units := TStringList.Create;
  try
    if AManualMode then
    begin
      // Manual mode: Only check explicitly listed paths
      Units.Add(ExcludeTrailingPathDelimiter(ABasePath));  // Base directory

      for I := 0 to AUnitPaths.Count - 1 do
      begin
        ConditionalPath := AUnitPaths[I];
        if TUtils.IsConditionMet(ConditionalPath.Condition, AActiveDefines) then
          Units.Add(ABasePath + DirectorySeparator + TUtils.NormalizePath(ConditionalPath.Path));
      end;
    end
    else
    begin
      // Auto-scan mode: Scan all directories with conditional filtering
      AllDirs := TUtils.ScanForUnitPathsFiltered(ABasePath, AUnitPaths, AActiveDefines);
      try
        Units.Add(ExcludeTrailingPathDelimiter(ABasePath));  // Base directory
        Units.AddStrings(AllDirs);
      finally
        AllDirs.Free;
      end;
    end;

    // Scan each directory for .pas files
    for Dir in Units do
    begin
      if FindFirst(IncludeTrailingPathDelimiter(Dir) + '*.pas', faAnyFile and not faDirectory, SearchRec) = 0 then
      begin
        try
          repeat
            UnitFile := IncludeTrailingPathDelimiter(Dir) + SearchRec.Name;
            DiscoveredUnit := ParseUnitName(UnitFile);

            if DiscoveredUnit <> '' then
              Result.Add(DiscoveredUnit);
          until FindNext(SearchRec) <> 0;
        finally
          FindClose(SearchRec);
        end;
      end;
    end;
  finally
    Units.Free;
  end;
end;

class function TBootstrapGenerator.GenerateBootstrapProgram(
  AConfig: TProjectConfig;
  const AOutputPath: string;
  AActiveDefines: TStringList
): Boolean;
var
  Units: TStringList;
  BootstrapCode: string;
  F: TextFile;
  CurrentUnitName: string;
  I: Integer;
  BasePath: string;
begin
  Result := False;

  TUtils.LogInfo('Generating bootstrap program for library...');

  // Discover all units
  BasePath := TUtils.NormalizePath('src/main/pascal');
  Units := DiscoverUnits(
    BasePath,
    AConfig.BuildConfig.UnitPaths,
    AConfig.BuildConfig.ManualUnitPaths,
    AActiveDefines
  );

  try
    if Units.Count = 0 then
    begin
      TUtils.LogWarning('No units found for bootstrap program');
      Exit;
    end;

    TUtils.LogInfo('Discovered ' + IntToStr(Units.Count) + ' units');

    // Generate bootstrap program code
    BootstrapCode := 'program bootstrap_program;' + LineEnding +
                     LineEnding +
                     '{$mode objfpc}{$H+}' + LineEnding +
                     LineEnding +
                     '{ Auto-generated bootstrap program for library compilation }' + LineEnding +
                     '{ Generated by PasBuild - DO NOT EDIT }' + LineEnding +
                     LineEnding +
                     'uses' + LineEnding;

    for I := 0 to Units.Count - 1 do
    begin
      CurrentUnitName := Units[I];
      if I < Units.Count - 1 then
        BootstrapCode := BootstrapCode + '  ' + CurrentUnitName + ',' + LineEnding
      else
        BootstrapCode := BootstrapCode + '  ' + CurrentUnitName + ';' + LineEnding;
    end;

    BootstrapCode := BootstrapCode + LineEnding +
                     'begin' + LineEnding +
                     '  WriteLn(''Library bootstrap compilation successful: ' + AConfig.Name + ''');' + LineEnding +
                     'end.' + LineEnding;

    // Write to file
    try
      AssignFile(F, AOutputPath);
      Rewrite(F);
      try
        Write(F, BootstrapCode);
      finally
        CloseFile(F);
      end;

      TUtils.LogInfo('Bootstrap program generated: ' + AOutputPath);
      Result := True;
    except
      on E: Exception do
      begin
        TUtils.LogError('Failed to write bootstrap program: ' + E.Message);
        Result := False;
      end;
    end;

  finally
    Units.Free;
  end;
end;

end.
