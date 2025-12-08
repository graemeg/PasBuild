{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Process,
  PasBuild.Types;

type
  { Utility functions for file operations and process execution }
  TUtils = class
  public
    { Path normalization }
    class function NormalizePath(const APath: string): string;

    { Directory and file operations }
    class function VerifyDirectoryLayout(const AProjectRoot: string): Boolean;
    class function ScanForUnitPaths(const ABaseDir: string): TStringList;
    class function ScanForIncludePaths(const ABaseDir: string): TStringList;
    class function ScanForUnitPathsFiltered(const ABaseDir: string; AConditionalPaths: TConditionalPathList; AActiveDefines: TStringList): TStringList;
    class function ScanForIncludePathsFiltered(const ABaseDir: string; AConditionalPaths: TConditionalPathList; AActiveDefines: TStringList): TStringList;
    class function DirectoryContainsIncludeFiles(const ADirectory: string): Boolean;

    { Condition evaluation }
    class function IsConditionMet(const ACondition: string; AActiveDefines: TStringList): Boolean;

    { Process execution }
    class function ExecuteProcess(const ACommand: string; AShowOutput: Boolean): Integer;
    class function ExecuteProcessWithCapture(const ACommand: string; out AOutput: string): Integer;
    class function ExecuteProcessWithLog(const ACommand: string; const ALogFile: string; AShowErrors: Boolean): Integer;

    { FPC detection }
    class function DetectFPCVersion: string;
    class function IsFPCAvailable: Boolean;

    { Platform utilities }
    class function GetPlatformExecutableSuffix: string;

    { Logging helpers }
    class procedure LogInfo(const AMessage: string);
    class procedure LogError(const AMessage: string);
    class procedure LogWarning(const AMessage: string);

    { Build status tracking }
    class function CreateStatusDirectory(const AGoalName: string): string;
    class function CollectSourceFiles(const ABaseDir: string): TStringList;
    class function CollectIncludeFiles(const ABaseDir: string): TStringList;
    class procedure WriteListFile(const AFilePath: string; AList: TStringList);
  end;

implementation

{ Forward declaration for recursive helper }
procedure RecursiveScanDirs(const ADir: string; AList: TStringList); forward;

{ TUtils }

class function TUtils.NormalizePath(const APath: string): string;
var
  I: Integer;
begin
  Result := APath;

  // Convert forward slashes to platform-specific directory separator
  // This allows project.xml to always use / (Maven convention)
  // while working correctly on Windows (\) and Unix (/)
  for I := 1 to Length(Result) do
  begin
    if Result[I] = '/' then
      Result[I] := DirectorySeparator;
  end;
end;

class function TUtils.VerifyDirectoryLayout(const AProjectRoot: string): Boolean;
var
  MainPascalDir: string;
begin
  Result := False;

  // Check if src/main/pascal directory exists
  // Use normalized paths for cross-platform compatibility
  MainPascalDir := IncludeTrailingPathDelimiter(AProjectRoot) +
                   NormalizePath('src/main/pascal');

  if not DirectoryExists(MainPascalDir) then
  begin
    LogError('Standard layout not found. Expected: src/main/pascal/');
    Exit;
  end;

  Result := True;
end;

class function TUtils.ScanForUnitPaths(const ABaseDir: string): TStringList;
var
  SearchRec: TSearchRec;
  BasePath, SubDir: string;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;
  Result.Sorted := True;

  BasePath := IncludeTrailingPathDelimiter(ABaseDir);

  // Find all subdirectories under ABaseDir
  if FindFirst(BasePath + '*', faDirectory, SearchRec) = 0 then
  begin
    try
      repeat
        // Skip . and .. and hidden directories
        if (SearchRec.Name <> '.') and
           (SearchRec.Name <> '..') and
           (SearchRec.Name <> '.git') and
           (SearchRec.Name <> '.svn') and
           (not AnsiStartsStr('backup', SearchRec.Name)) and
           ((SearchRec.Attr and faDirectory) = faDirectory) then
        begin
          SubDir := BasePath + SearchRec.Name;
          Result.Add(SubDir);

          // Recursively scan subdirectories
          RecursiveScanDirs(SubDir, Result);
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

class function TUtils.ScanForIncludePaths(const ABaseDir: string): TStringList;
var
  UnitPaths: TStringList;
  Dir: string;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;
  Result.Sorted := True;

  // Check base directory for .inc files
  if DirectoryContainsIncludeFiles(ABaseDir) then
    Result.Add(ABaseDir);

  // Check all subdirectories for .inc files
  UnitPaths := ScanForUnitPaths(ABaseDir);
  try
    for Dir in UnitPaths do
    begin
      if DirectoryContainsIncludeFiles(Dir) then
        Result.Add(Dir);
    end;
  finally
    UnitPaths.Free;
  end;
end;

procedure RecursiveScanDirs(const ADir: string; AList: TStringList);
var
  SearchRec: TSearchRec;
  DirPath, SubDir: string;
begin
  DirPath := IncludeTrailingPathDelimiter(ADir);

  if FindFirst(DirPath + '*', faDirectory, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name <> '.') and
           (SearchRec.Name <> '..') and
           (SearchRec.Name <> '.git') and
           (SearchRec.Name <> '.svn') and
           (not AnsiStartsStr('backup', SearchRec.Name)) and
           ((SearchRec.Attr and faDirectory) = faDirectory) then
        begin
          SubDir := DirPath + SearchRec.Name;
          AList.Add(SubDir);
          RecursiveScanDirs(SubDir, AList);
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

class function TUtils.ExecuteProcess(const ACommand: string; AShowOutput: Boolean): Integer;
var
  AProcess: TProcess;
  Buffer: array[0..4095] of Char;
  BytesRead: Integer;
  OutputLine: string;
begin
  AProcess := TProcess.Create(nil);
  try
    {$IFDEF UNIX}
    AProcess.Executable := '/bin/sh';
    AProcess.Parameters.Add('-c');
    AProcess.Parameters.Add(ACommand);
    {$ELSE}
    AProcess.Executable := 'cmd.exe';
    AProcess.Parameters.Add('/c');
    AProcess.Parameters.Add(ACommand);
    {$ENDIF}
    AProcess.Options := [poUsePipes, poStderrToOutPut];

    try
      AProcess.Execute;

      // Read output in real-time if requested
      if AShowOutput then
      begin
        while AProcess.Running or (AProcess.Output.NumBytesAvailable > 0) do
        begin
          if AProcess.Output.NumBytesAvailable > 0 then
          begin
            BytesRead := AProcess.Output.Read(Buffer, SizeOf(Buffer) - 1);
            if BytesRead > 0 then
            begin
              Buffer[BytesRead] := #0;
              OutputLine := StrPas(Buffer);
              Write(OutputLine);
            end;
          end
          else
            Sleep(10);
        end;
      end
      else
      begin
        // Just wait for completion
        AProcess.WaitOnExit;
      end;

      Result := AProcess.ExitStatus;
    except
      on E: Exception do
      begin
        LogError('Failed to execute: ' + ACommand);
        LogError('Error: ' + E.Message);
        Result := 1;
      end;
    end;
  finally
    AProcess.Free;
  end;
end;

class function TUtils.ExecuteProcessWithCapture(const ACommand: string; out AOutput: string): Integer;
var
  AProcess: TProcess;
  OutputList: TStringList;
begin
  AOutput := '';
  OutputList := TStringList.Create;
  AProcess := TProcess.Create(nil);
  try
    {$IFDEF UNIX}
    AProcess.Executable := '/bin/sh';
    AProcess.Parameters.Add('-c');
    AProcess.Parameters.Add(ACommand);
    {$ELSE}
    AProcess.Executable := 'cmd.exe';
    AProcess.Parameters.Add('/c');
    AProcess.Parameters.Add(ACommand);
    {$ENDIF}
    AProcess.Options := [poUsePipes, poStderrToOutPut, poWaitOnExit];

    try
      AProcess.Execute;
      OutputList.LoadFromStream(AProcess.Output);
      AOutput := OutputList.Text;
      Result := AProcess.ExitStatus;
    except
      on E: Exception do
      begin
        LogError('Failed to execute: ' + ACommand);
        LogError('Error: ' + E.Message);
        Result := 1;
      end;
    end;
  finally
    AProcess.Free;
    OutputList.Free;
  end;
end;

class function TUtils.ExecuteProcessWithLog(const ACommand: string; const ALogFile: string; AShowErrors: Boolean): Integer;
var
  AProcess: TProcess;
  Buffer: array[0..4095] of Char;
  BytesRead: Integer;
  LogStream: TFileStream;
  Line: string;
  OutputLine: string;
begin
  LogStream := TFileStream.Create(ALogFile, fmCreate);
  AProcess := TProcess.Create(nil);
  try
    {$IFDEF UNIX}
    AProcess.Executable := '/bin/sh';
    AProcess.Parameters.Add('-c');
    AProcess.Parameters.Add(ACommand);
    {$ELSE}
    AProcess.Executable := 'cmd.exe';
    AProcess.Parameters.Add('/c');
    AProcess.Parameters.Add(ACommand);
    {$ENDIF}
    AProcess.Options := [poUsePipes, poStderrToOutPut];

    try
      AProcess.Execute;

      // Read output and write to log file
      OutputLine := '';
      while AProcess.Running or (AProcess.Output.NumBytesAvailable > 0) do
      begin
        if AProcess.Output.NumBytesAvailable > 0 then
        begin
          BytesRead := AProcess.Output.Read(Buffer, SizeOf(Buffer) - 1);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;
            Line := StrPas(Buffer);

            // Write to log file
            LogStream.WriteBuffer(Buffer, BytesRead);

            // Accumulate output for error detection
            OutputLine := OutputLine + Line;

            // Show errors on console if requested
            if AShowErrors and (Pos('Error:', OutputLine) > 0) then
            begin
              // Extract and show error lines
              while Pos(LineEnding, OutputLine) > 0 do
              begin
                Line := Copy(OutputLine, 1, Pos(LineEnding, OutputLine) - 1);
                Delete(OutputLine, 1, Pos(LineEnding, OutputLine) + Length(LineEnding) - 1);

                if (Pos('Error:', Line) > 0) or (Pos('Fatal:', Line) > 0) then
                  LogError(Line);
              end;
            end;
          end;
        end;
      end;

      Result := AProcess.ExitStatus;
    except
      on E: Exception do
      begin
        LogError('Failed to execute: ' + ACommand);
        LogError('Error: ' + E.Message);
        Result := 1;
      end;
    end;
  finally
    AProcess.Free;
    LogStream.Free;
  end;
end;

class function TUtils.DetectFPCVersion: string;
var
  Output: string;
  ExitCode: Integer;
begin
  Result := 'Unknown';

  ExitCode := ExecuteProcessWithCapture('fpc -iV', Output);
  if ExitCode = 0 then
    Result := Trim(Output)
  else
    Result := 'Not found';
end;

class function TUtils.IsFPCAvailable: Boolean;
var
  ExitCode: Integer;
  Output: string;
begin
  ExitCode := ExecuteProcessWithCapture('fpc -iV', Output);
  Result := (ExitCode = 0);
end;

class function TUtils.GetPlatformExecutableSuffix: string;
begin
  {$IFDEF WINDOWS}
  Result := '.exe';
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

class procedure TUtils.LogInfo(const AMessage: string);
begin
  WriteLn('[INFO] ', AMessage);
end;

class procedure TUtils.LogError(const AMessage: string);
begin
  WriteLn(StdErr, '[ERROR] ', AMessage);
end;

class procedure TUtils.LogWarning(const AMessage: string);
begin
  WriteLn('[WARNING] ', AMessage);
end;

class function TUtils.DirectoryContainsIncludeFiles(const ADirectory: string): Boolean;
var
  SearchRec: TSearchRec;
begin
  Result := False;
  if not DirectoryExists(ADirectory) then
    Exit;

  if FindFirst(IncludeTrailingPathDelimiter(ADirectory) + '*.inc', faAnyFile and not faDirectory, SearchRec) = 0 then
  begin
    Result := True;
    FindClose(SearchRec);
  end;
end;

class function TUtils.IsConditionMet(const ACondition: string; AActiveDefines: TStringList): Boolean;
var
  Condition: string;
begin
  // Empty condition means unconditional (always true)
  if ACondition = '' then
    Exit(True);

  Condition := UpperCase(ACondition);

  // Check built-in FPC platform defines
  case Condition of
    'UNIX':    Result := {$IFDEF UNIX}True{$ELSE}False{$ENDIF};
    'LINUX':   Result := {$IFDEF LINUX}True{$ELSE}False{$ENDIF};
    'FREEBSD': Result := {$IFDEF FREEBSD}True{$ELSE}False{$ENDIF};
    'DARWIN':  Result := {$IFDEF DARWIN}True{$ELSE}False{$ENDIF};
    'WINDOWS': Result := {$IFDEF WINDOWS}True{$ELSE}False{$ENDIF};
    'WIN32':   Result := {$IFDEF WIN32}True{$ELSE}False{$ENDIF};
    'WIN64':   Result := {$IFDEF WIN64}True{$ELSE}False{$ENDIF};
  else
    // Unknown built-in define - check against active defines (global + profile)
    Result := (AActiveDefines <> nil) and (AActiveDefines.IndexOf(ACondition) >= 0);
  end;
end;

class function TUtils.ScanForUnitPathsFiltered(const ABaseDir: string; AConditionalPaths: TConditionalPathList; AActiveDefines: TStringList): TStringList;
var
  AllPaths: TStringList;
  Path, RelativePath: string;
  ConditionalPath: TConditionalPath;
  IsConditional, ConditionMet: Boolean;
  I: Integer;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;
  Result.Sorted := True;

  // 1. Auto-scan all directories
  AllPaths := ScanForUnitPaths(ABaseDir);
  try
    for I := 0 to AllPaths.Count - 1 do
    begin
      Path := AllPaths[I];

      // Get path relative to base directory
      RelativePath := ExtractRelativePath(IncludeTrailingPathDelimiter(ABaseDir), Path);

      // 2. Check if this path matches any conditional path
      IsConditional := False;
      ConditionMet := False;

      if AConditionalPaths <> nil then
      begin
        for ConditionalPath in AConditionalPaths do
        begin
          // Check if path starts with conditional path (prefix matching)
          // Normalize both paths to use forward slashes for comparison
          if AnsiStartsStr(
               StringReplace(ConditionalPath.Path, '\', '/', [rfReplaceAll]),
               StringReplace(RelativePath, '\', '/', [rfReplaceAll])
             ) then
          begin
            IsConditional := True;
            ConditionMet := IsConditionMet(ConditionalPath.Condition, AActiveDefines);
            Break;  // First match wins
          end;
        end;
      end;

      // 3. Include path based on condition
      if IsConditional then
      begin
        // Conditional path - include only if condition is TRUE
        if ConditionMet then
          Result.Add(Path);
      end
      else
      begin
        // Non-conditional path - always include
        Result.Add(Path);
      end;
    end;
  finally
    AllPaths.Free;
  end;
end;

class function TUtils.ScanForIncludePathsFiltered(const ABaseDir: string; AConditionalPaths: TConditionalPathList; AActiveDefines: TStringList): TStringList;
var
  DirList, UnitDirs: TStringList;
  Dir, RelativePath: string;
  SearchRec: TSearchRec;
  ConditionalPath: TConditionalPath;
  IsConditional, ConditionMet: Boolean;
  I: Integer;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;
  Result.Sorted := True;

  // 1. Get all directories (including base directory itself)
  DirList := TStringList.Create;
  try
    DirList.Add(ExcludeTrailingPathDelimiter(ABaseDir));  // Include base directory

    // Get subdirectories (need to free the temporary list)
    UnitDirs := ScanForUnitPaths(ABaseDir);
    try
      DirList.AddStrings(UnitDirs);
    finally
      UnitDirs.Free;
    end;

    for I := 0 to DirList.Count - 1 do
    begin
      Dir := DirList[I];

      // 2. Check if directory contains *.inc files
      if FindFirst(IncludeTrailingPathDelimiter(Dir) + '*.inc', faAnyFile and not faDirectory, SearchRec) = 0 then
      begin
        FindClose(SearchRec);

        // 3. Apply conditional filtering (same logic as unit paths)
        RelativePath := ExtractRelativePath(IncludeTrailingPathDelimiter(ABaseDir), Dir);
        IsConditional := False;
        ConditionMet := False;

        if AConditionalPaths <> nil then
        begin
          for ConditionalPath in AConditionalPaths do
          begin
            if AnsiStartsStr(
                 StringReplace(ConditionalPath.Path, '\', '/', [rfReplaceAll]),
                 StringReplace(RelativePath, '\', '/', [rfReplaceAll])
               ) then
            begin
              IsConditional := True;
              ConditionMet := IsConditionMet(ConditionalPath.Condition, AActiveDefines);
              Break;
            end;
          end;
        end;

        // Include if non-conditional OR condition is met
        if (not IsConditional) or ConditionMet then
          Result.Add(Dir);
      end;
    end;
  finally
    DirList.Free;
  end;
end;

class function TUtils.CreateStatusDirectory(const AGoalName: string): string;
var
  StatusBaseDir: string;
begin
  StatusBaseDir := NormalizePath('target' + DirectorySeparator + 'pasbuild-status');
  Result := StatusBaseDir + DirectorySeparator + AGoalName;

  if not ForceDirectories(Result) then
    raise Exception.CreateFmt('Failed to create status directory: %s', [Result]);
end;

class function TUtils.CollectSourceFiles(const ABaseDir: string): TStringList;
var
  SearchRec: TSearchRec;
  BasePath, FileName: string;
  SubDirs: TStringList;
  SubDir: string;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;
  Result.Sorted := True;

  if not DirectoryExists(ABaseDir) then
    Exit;

  BasePath := IncludeTrailingPathDelimiter(ABaseDir);

  // Find all source files in current directory
  if FindFirst(BasePath + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Attr and faDirectory) = 0 then
        begin
          FileName := LowerCase(SearchRec.Name);
          // Check for Pascal source file extensions
          if AnsiEndsStr('.pas', FileName) or
             AnsiEndsStr('.pp', FileName) or
             AnsiEndsStr('.lpr', FileName) then
          begin
            Result.Add(BasePath + SearchRec.Name);
          end;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;

  // Recursively scan subdirectories
  SubDirs := ScanForUnitPaths(ABaseDir);
  try
    for SubDir in SubDirs do
    begin
      if FindFirst(IncludeTrailingPathDelimiter(SubDir) + '*', faAnyFile, SearchRec) = 0 then
      begin
        try
          repeat
            if (SearchRec.Attr and faDirectory) = 0 then
            begin
              FileName := LowerCase(SearchRec.Name);
              if AnsiEndsStr('.pas', FileName) or
                 AnsiEndsStr('.pp', FileName) or
                 AnsiEndsStr('.lpr', FileName) then
              begin
                Result.Add(IncludeTrailingPathDelimiter(SubDir) + SearchRec.Name);
              end;
            end;
          until FindNext(SearchRec) <> 0;
        finally
          FindClose(SearchRec);
        end;
      end;
    end;
  finally
    SubDirs.Free;
  end;
end;

class function TUtils.CollectIncludeFiles(const ABaseDir: string): TStringList;
var
  SearchRec: TSearchRec;
  BasePath, FileName: string;
  SubDirs: TStringList;
  SubDir: string;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;
  Result.Sorted := True;

  if not DirectoryExists(ABaseDir) then
    Exit;

  BasePath := IncludeTrailingPathDelimiter(ABaseDir);

  // Find all .inc files in current directory
  if FindFirst(BasePath + '*.inc', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Attr and faDirectory) = 0 then
          Result.Add(BasePath + SearchRec.Name);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;

  // Recursively scan subdirectories
  SubDirs := ScanForUnitPaths(ABaseDir);
  try
    for SubDir in SubDirs do
    begin
      if FindFirst(IncludeTrailingPathDelimiter(SubDir) + '*.inc', faAnyFile, SearchRec) = 0 then
      begin
        try
          repeat
            if (SearchRec.Attr and faDirectory) = 0 then
              Result.Add(IncludeTrailingPathDelimiter(SubDir) + SearchRec.Name);
          until FindNext(SearchRec) <> 0;
        finally
          FindClose(SearchRec);
        end;
      end;
    end;
  finally
    SubDirs.Free;
  end;
end;

class procedure TUtils.WriteListFile(const AFilePath: string; AList: TStringList);
var
  F: TextFile;
  Item: string;
begin
  AssignFile(F, AFilePath);
  try
    Rewrite(F);
    try
      for Item in AList do
        WriteLn(F, Item);
    finally
      CloseFile(F);
    end;
  except
    on E: Exception do
      LogWarning('Failed to write list file ' + AFilePath + ': ' + E.Message);
  end;
end;

end.
