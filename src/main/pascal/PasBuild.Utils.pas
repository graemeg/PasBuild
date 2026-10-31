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
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  PasBuild.Types,
  PasBuild.Compiler.Backend,
  PasBuild.Utils.Console;

type
  { Utility functions for file operations and process execution }
  TUtils = class
  private
    class var FFPCExecutable: string;
    class var FQuietMode: Boolean;
    class var FBackend: TCompilerBackend;
  public
    class property QuietMode: Boolean read FQuietMode write FQuietMode;
    { Compiler backend — set once at startup by PasBuild.pas via the factory. }
    class procedure SetCompilerBackend(ABackend: TCompilerBackend);
    class function GetCompilerBackend: TCompilerBackend;
    { Legacy accessors — delegate to the active backend. }
    class procedure SetFPCExecutable(const APath: string);
    class function GetFPCExecutable: string;
    { Path normalization }
    class function NormalizePath(const APath: string): string;
    class function IsModuleRootRelativePath(const APath: string): Boolean;

    { Directory and file operations }
    class function VerifyDirectoryLayout(const AProjectRoot: string): Boolean;
    class function VerifyDirectoryLayout(const AProjectRoot: string; const ASourceDirectory: string): Boolean;
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
    { Run an executable with an argument array in a given working directory,
      capturing combined stdout/stderr. No shell is involved, so arguments
      and paths need no quoting and cannot be used for command injection.
      AExtraEnv ('NAME=value' entries) overrides/extends the inherited
      environment. }
    class function ExecuteProcessInDirWithCapture(const AExecutable: string;
      const AArgs: array of string; const AWorkDir: string;
      out AOutput: string; AExtraEnv: TStringList = nil): Integer;
    { Wait for AProcess to finish and return its exit code.  TProcess.ExitCode
      is unreliable on Unix: it reports 0 for a child killed by a signal (a
      crashing compiler), and 0 for any exit code once WaitOnExit has run
      (WEXITSTATUS is applied twice).  This reaps the child through Running,
      decodes the raw waitpid status itself and maps signal termination to
      128+signal, the shell convention. }
    class function ProcessExitCode(AProcess: TProcess): Integer;
    { The last ACount non-blank lines of AText; caller frees the list. }
    class function TailLines(const AText: string; ACount: Integer): TStringList;
    { Absolute path of the running pasbuild binary (safe against relative
      argv[0] and a later working-directory change). }
    class function GetSelfExePath: string;

    { Package identifier validation — names/versions feed filesystem paths,
      registry URLs and subprocess arguments }
    class function IsValidPackageName(const AName: string): Boolean;
    class function IsValidPackageVersion(const AVersion: string): Boolean;

    { Numeric component-wise version comparison: negative when A < B,
      zero when equal, positive when A > B. Missing components count as 0,
      non-numeric suffixes are ignored. }
    class function CompareVersions(const AVersionA, AVersionB: string): Integer;

    { Cross-process remote-resolve stack (comma-separated name:version
      entries carried in the PASBUILD_RESOLVE_STACK environment variable)
      used to detect dependency cycles across pasbuild subprocesses }
    class function ResolveStackContains(const AStack, AEntry: string): Boolean;
    class function AppendToResolveStack(const AStack, AEntry: string): string;

    { Compiler detection — delegate to the active backend. }
    class function DetectCompilerVersion: string;
    class function DetectCompilerName: string;
    class function DetectTargetCPU: string;
    class function DetectTargetOS: string;
    class function GetTargetQuadruplet: string;
    class function GetPackagePlatformSuffix: string;
    class function IsCompilerAvailable: Boolean;

    { Path utilities }
    class function QuotePath(const APath: string): string;

    { XML utilities }
    class function XmlEscapeText(const AText: string): string;

    { Platform utilities }
    class function GetPlatformExecutableSuffix: string;

    { Logging helpers }
    class procedure LogInfo(const AMessage: string);
    class procedure LogError(const AMessage: string);
    class procedure LogWarning(const AMessage: string);
    class procedure LogTag(const ATag: String; const AMessage: String);
    class procedure LogTag(const ATag: String; const AMessage: String; var LogTo: Text);
    class procedure LogSeparator;

    { Cache directory helpers }
    class function GetCacheRoot: string;
    class function GetDownloadCacheDir: string;
    class function GetBuildCacheDir: string;
    class function EnsureCacheDir(const ASubDir: string): string;

    { Build status tracking }
    class function CreateStatusDirectory(const AGoalName: string): string;
    class function CollectSourceFiles(const ABaseDir: string): TStringList;
    class function CollectIncludeFiles(const ABaseDir: string): TStringList;
    class procedure WriteListFile(const AFilePath: string; AList: TStringList);

    { Build date utilities }
    class function GetBuildDateTime(): TDateTime;
  end;

implementation

uses
  DateUtils;

{ Forward declaration for recursive helper }
procedure RecursiveScanDirs(const ADir: string; AList: TStringList); forward;

{ TUtils }

class procedure TUtils.SetCompilerBackend(ABackend: TCompilerBackend);
begin
  FreeAndNil(FBackend);
  FBackend := ABackend;
end;

class function TUtils.GetCompilerBackend: TCompilerBackend;
begin
  if FBackend = nil then
    raise Exception.Create('No compiler backend configured');
  Result := FBackend;
end;

class procedure TUtils.SetFPCExecutable(const APath: string);
begin
  FFPCExecutable := APath;
end;

class function TUtils.GetFPCExecutable: string;
begin
  if FBackend <> nil then
    Result := FBackend.Executable
  else if FFPCExecutable <> '' then
    Result := FFPCExecutable
  else
    Result := 'fpc';
end;

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

class function TUtils.IsModuleRootRelativePath(const APath: string): Boolean;
begin
  // Paths starting with ../ or ./ are relative to the module root (the directory
  // containing project.xml), not to the source directory. This convention mirrors
  // how shells interpret relative paths: anything escaping the current tree via
  // ../ cannot logically be inside <sourceDirectory>.
  // Note: project.xml always uses / (Maven convention), so we check / only here;
  // NormalizePath is applied after this check to convert to platform separators.
  Result := ((Length(APath) >= 3) and (Copy(APath, 1, 3) = '../'))
         or ((Length(APath) >= 2) and (Copy(APath, 1, 2) = './'));
end;

class function TUtils.VerifyDirectoryLayout(const AProjectRoot: string): Boolean;
begin
  Result := VerifyDirectoryLayout(AProjectRoot, 'src/main/pascal');
end;

class function TUtils.VerifyDirectoryLayout(const AProjectRoot: string; const ASourceDirectory: string): Boolean;
var
  SourceDir: string;
begin
  Result := False;

  // Check if source directory exists
  // Use normalized paths for cross-platform compatibility
  SourceDir := IncludeTrailingPathDelimiter(AProjectRoot) +
               NormalizePath(ASourceDirectory);

  if not DirectoryExists(SourceDir) then
  begin
    LogError('Source directory not found: ' + ASourceDirectory + '/');
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
      end;

      // ProcessExitCode also waits when output was not shown
      Result := ProcessExitCode(AProcess);
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
  Buffer: array[0..4095] of Char;
  BytesRead: Integer;
  OutputStr: string;
begin
  AOutput := '';
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

      // Drain output while the process is running. LoadFromStream and
      // poWaitOnExit must NOT be used: the while-Running drain makes TProcess
      // keep the raw waitpid status, which ProcessExitCode decodes.
      OutputStr := '';
      while AProcess.Running or (AProcess.Output.NumBytesAvailable > 0) do
      begin
        if AProcess.Output.NumBytesAvailable > 0 then
        begin
          BytesRead := AProcess.Output.Read(Buffer, SizeOf(Buffer) - 1);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;
            OutputStr := OutputStr + StrPas(Buffer);
          end;
        end
        else
          Sleep(10);
      end;
      AOutput := OutputStr;
      Result := ProcessExitCode(AProcess);
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

class function TUtils.ExecuteProcessInDirWithCapture(const AExecutable: string;
  const AArgs: array of string; const AWorkDir: string;
  out AOutput: string; AExtraEnv: TStringList): Integer;
var
  AProcess: TProcess;
  Buffer: array[0..4095] of Char;
  BytesRead, I, J: Integer;
  OutputStr, ExePath, EnvStr, VarName: string;
  Keep: Boolean;
begin
  AOutput := '';

  // TProcess does not search PATH for bare executable names on all
  // platforms; resolve explicitly so 'pwd'-style names work uniformly
  ExePath := AExecutable;
  if ExtractFilePath(ExePath) = '' then
  begin
    ExePath := ExeSearch(AExecutable, GetEnvironmentVariable('PATH'));
    if ExePath = '' then
      ExePath := AExecutable;
  end;

  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := ExePath;
    for I := Low(AArgs) to High(AArgs) do
      AProcess.Parameters.Add(AArgs[I]);
    AProcess.CurrentDirectory := AWorkDir;
    AProcess.Options := [poUsePipes, poStderrToOutPut];

    if Assigned(AExtraEnv) and (AExtraEnv.Count > 0) then
    begin
      // Copy the inherited environment, dropping variables that the
      // extras override (getenv returns the FIRST match, so appending
      // a duplicate name would not take effect)
      for I := 1 to GetEnvironmentVariableCount do
      begin
        EnvStr := GetEnvironmentString(I);
        Keep := True;
        for J := 0 to AExtraEnv.Count - 1 do
        begin
          VarName := Copy(AExtraEnv[J], 1, Pos('=', AExtraEnv[J]));
          if (VarName <> '') and (Copy(EnvStr, 1, Length(VarName)) = VarName) then
          begin
            Keep := False;
            Break;
          end;
        end;
        if Keep then
          AProcess.Environment.Add(EnvStr);
      end;
      for J := 0 to AExtraEnv.Count - 1 do
        AProcess.Environment.Add(AExtraEnv[J]);
    end;

    try
      AProcess.Execute;

      // Drain output while the process is running (see ProcessExitCode for
      // why poWaitOnExit is avoided)
      OutputStr := '';
      while AProcess.Running or (AProcess.Output.NumBytesAvailable > 0) do
      begin
        if AProcess.Output.NumBytesAvailable > 0 then
        begin
          BytesRead := AProcess.Output.Read(Buffer, SizeOf(Buffer) - 1);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;
            OutputStr := OutputStr + StrPas(Buffer);
          end;
        end
        else
          Sleep(10);
      end;
      AOutput := OutputStr;
      Result := ProcessExitCode(AProcess);
    except
      on E: Exception do
      begin
        LogError('Failed to execute: ' + AExecutable);
        LogError('Error: ' + E.Message);
        Result := 1;
      end;
    end;
  finally
    AProcess.Free;
  end;
end;

class function TUtils.GetSelfExePath: string;
{$IFDEF LINUX}
var
  LinkTarget: string;
begin
  LinkTarget := fpReadLink('/proc/self/exe');
  if LinkTarget <> '' then
    Result := LinkTarget
  else
    Result := ExpandFileName(ParamStr(0));
end;
{$ELSE}
begin
  Result := ExpandFileName(ParamStr(0));
end;
{$ENDIF}

class function TUtils.IsValidPackageName(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AName = '' then
    Exit;
  // First character must be alphanumeric: forbids leading '.', '-'
  // (hidden files, traversal, option injection)
  if not (AName[1] in ['a'..'z', 'A'..'Z', '0'..'9']) then
    Exit;
  for I := 2 to Length(AName) do
    if not (AName[I] in ['a'..'z', 'A'..'Z', '0'..'9', '.', '-', '_']) then
      Exit;
  Result := True;
end;

class function TUtils.IsValidPackageVersion(const AVersion: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AVersion = '' then
    Exit;
  if not (AVersion[1] in ['0'..'9']) then
    Exit;
  for I := 2 to Length(AVersion) do
    if not (AVersion[I] in ['a'..'z', 'A'..'Z', '0'..'9', '.', '-', '_', '+']) then
      Exit;
  Result := True;
end;

class function TUtils.CompareVersions(const AVersionA,
  AVersionB: string): Integer;

  { Next numeric component from AVersion starting at APos (1-based).
    Stops at '.' or any non-digit; advances APos past the separator. }
  function NextComponent(const AVersion: string; var APos: Integer): Integer;
  begin
    Result := 0;
    while (APos <= Length(AVersion)) and (AVersion[APos] in ['0'..'9']) do
    begin
      Result := Result * 10 + (Ord(AVersion[APos]) - Ord('0'));
      Inc(APos);
    end;
    // Skip everything up to and including the next dot (ignores
    // pre-release/build suffixes like '-rc.1')
    while (APos <= Length(AVersion)) and (AVersion[APos] <> '.') do
      Inc(APos);
    if APos <= Length(AVersion) then
      Inc(APos);
  end;

var
  PosA, PosB, CompA, CompB: Integer;
begin
  PosA := 1;
  PosB := 1;
  while (PosA <= Length(AVersionA)) or (PosB <= Length(AVersionB)) do
  begin
    CompA := NextComponent(AVersionA, PosA);
    CompB := NextComponent(AVersionB, PosB);
    if CompA <> CompB then
      Exit(CompA - CompB);
  end;
  Result := 0;
end;

class function TUtils.ResolveStackContains(const AStack,
  AEntry: string): Boolean;
var
  Parts: TStringList;
begin
  Result := False;
  if (AStack = '') or (AEntry = '') then
    Exit;
  Parts := TStringList.Create;
  try
    Parts.StrictDelimiter := True;
    Parts.CommaText := AStack;
    Result := Parts.IndexOf(AEntry) >= 0;
  finally
    Parts.Free;
  end;
end;

class function TUtils.AppendToResolveStack(const AStack,
  AEntry: string): string;
begin
  if AStack = '' then
    Result := AEntry
  else
    Result := AStack + ',' + AEntry;
end;

class function TUtils.ProcessExitCode(AProcess: TProcess): Integer;
{$IFDEF UNIX}
var
  Status: cint;
{$ENDIF}
begin
  {$IFDEF UNIX}
  while AProcess.Running do
    Sleep(10);
  Status := AProcess.ExitStatus;
  if wifexited(Status) then
    Result := wexitstatus(Status)
  else if wifsignaled(Status) then
    Result := 128 + wtermsig(Status)
  else
    Result := 1;
  {$ELSE}
  AProcess.WaitOnExit;
  Result := AProcess.ExitCode;
  {$ENDIF}
end;

class function TUtils.TailLines(const AText: string; ACount: Integer): TStringList;
var
  AllLines: TStringList;
  I: Integer;
begin
  Result := TStringList.Create;
  AllLines := TStringList.Create;
  try
    AllLines.Text := AText;
    I := AllLines.Count - 1;
    while (I >= 0) and (Result.Count < ACount) do
    begin
      if Trim(AllLines[I]) <> '' then
        Result.Insert(0, TrimRight(AllLines[I]));
      Dec(I);
    end;
  finally
    AllLines.Free;
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
  FullOutput: string;
  Header: string;
  ErrorsShown: Boolean;
  TailList: TStringList;
const
  FailureTailLines = 20;
begin
  LogStream := TFileStream.Create(ALogFile, fmCreate);
  AProcess := TProcess.Create(nil);
  try
    // Write build command header to log file
    Header := '# Build command:' + LineEnding + ACommand + LineEnding + LineEnding;
    LogStream.WriteBuffer(Header[1], Length(Header));

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
      FullOutput := '';
      ErrorsShown := False;
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
            FullOutput := FullOutput + Line;

            // Show errors on console if requested
            if AShowErrors and (Pos('Error:', OutputLine) > 0) then
            begin
              // Extract and show error lines
              while Pos(LineEnding, OutputLine) > 0 do
              begin
                Line := Copy(OutputLine, 1, Pos(LineEnding, OutputLine) - 1);
                Delete(OutputLine, 1, Pos(LineEnding, OutputLine) + Length(LineEnding) - 1);

                if (Pos('Error:', Line) > 0) or (Pos('Fatal:', Line) > 0) then
                begin
                  LogError(Line);
                  ErrorsShown := True;
                end;
              end;
            end;
          end;
        end
        else
          Sleep(10);
      end;

      Result := ProcessExitCode(AProcess);

      // The failure produced no line the filter above recognises (Blaise
      // diagnostics have no 'Error:' prefix), so show the end of the output
      // rather than leaving only an exit code on the console
      if AShowErrors and (Result <> 0) and not ErrorsShown then
      begin
        TailList := TailLines(FullOutput, FailureTailLines);
        try
          if TailList.Count > 0 then
          begin
            LogError('Compiler output (last ' + IntToStr(TailList.Count) + ' lines):');
            for Line in TailList do
              LogError('  ' + Line);
          end;
        finally
          TailList.Free;
        end;
      end;
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

class function TUtils.DetectCompilerVersion: string;
begin
  Result := GetCompilerBackend.GetVersion;
end;

class function TUtils.DetectCompilerName: string;
begin
  Result := LowerCase(GetCompilerBackend.Name);
end;

class function TUtils.DetectTargetCPU: string;
begin
  Result := GetCompilerBackend.GetTargetCPU;
end;

class function TUtils.DetectTargetOS: string;
begin
  Result := GetCompilerBackend.GetTargetOS;
end;

class function TUtils.GetTargetQuadruplet: string;
begin
  Result := DetectCompilerName + '-' + DetectTargetCPU + '-' +
            DetectTargetOS + '-' + DetectCompilerVersion;
end;

class function TUtils.GetPackagePlatformSuffix: string;
begin
  Result := DetectTargetCPU + '-' + DetectTargetOS;
end;

class function TUtils.IsCompilerAvailable: Boolean;
begin
  Result := GetCompilerBackend.GetVersion <> '';
end;

class function TUtils.QuotePath(const APath: string): string;
begin
  if (APath <> '') and (Pos(' ', APath) > 0) then
    Result := '"' + APath + '"'
  else
    Result := APath;
end;

class function TUtils.XmlEscapeText(const AText: string): string;
begin
  Result := StringReplace(AText, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
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
  if not FQuietMode then
    LogTag('[INFO]', AMessage);
end;

class procedure TUtils.LogError(const AMessage: string);
begin
  LogTag('[ERROR]', AMessage, StdErr);
end;

class procedure TUtils.LogWarning(const AMessage: string);
begin
  if not FQuietMode then
    LogTag('[WARNING]', AMessage);
end;

class procedure TUtils.LogTag(const ATag: String; const AMessage: String);
begin
  LogTag(ATag, AMessage, StdOut);
end;

class procedure TUtils.LogTag(const ATag: String; const AMessage: String; var LogTo: Text);
var
  Color: TSimpleColor;
begin
  case ATag of
    '[INFO]'    : Color := scBlue;
    '[WARNING]' : Color := scYellow;
    '[ERROR]'   : Color := scRed;
  else
    Color := scWhite;
  end;
  WriteColor(Color, ATag+' ', LogTo);
  WriteLn(LogTo, AMessage);
  // StdOut/StdErr/Output are independently buffered Text handles on the
  // same file descriptors; without a flush, log lines interleave with
  // plain WriteLn output in buffer-fill order rather than program order
  Flush(LogTo);
end;

class procedure TUtils.LogSeparator;
begin
  if not FQuietMode then
    LogInfo(StringOfChar('-', 72));
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

class function TUtils.GetCacheRoot: string;
begin
  Result := IncludeTrailingPathDelimiter(GetUserDir) + '.pasbuild' +
    DirectorySeparator + 'cache';
end;

class function TUtils.GetDownloadCacheDir: string;
begin
  Result := GetCacheRoot + DirectorySeparator + 'downloads';
end;

class function TUtils.GetBuildCacheDir: string;
begin
  Result := GetCacheRoot + DirectorySeparator + 'builds';
end;

class function TUtils.EnsureCacheDir(const ASubDir: string): string;
begin
  Result := GetCacheRoot + DirectorySeparator + ASubDir;
  if not ForceDirectories(Result) then
    raise Exception.CreateFmt('Failed to create cache directory: %s', [Result]);
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
  BasePath: string;
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

class function TUtils.GetBuildDateTime(): TDateTime;
var
  SourceDateEpoch: string;
begin
  SourceDateEpoch := GetEnvironmentVariable('SOURCE_DATE_EPOCH');
  if Length(SourceDateEpoch) > 0 then
    Result := UnixToDateTime(StrToInt64(SourceDateEpoch))
  else
    Result := SysUtils.Now()
end;

end.
