unit PasBuild.Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Process;

type
  { Utility functions for file operations and process execution }
  TUtils = class
  public
    { Path normalization }
    class function NormalizePath(const APath: string): string;

    { Directory and file operations }
    class function VerifyDirectoryLayout(const AProjectRoot: string): Boolean;
    class function ScanForUnitPaths(const ABaseDir: string): TStringList;

    { Process execution }
    class function ExecuteProcess(const ACommand: string; AShowOutput: Boolean): Integer;
    class function ExecuteProcessWithCapture(const ACommand: string; out AOutput: string): Integer;

    { FPC detection }
    class function DetectFPCVersion: string;
    class function IsFPCAvailable: Boolean;

    { Platform utilities }
    class function GetPlatformExecutableSuffix: string;

    { Logging helpers }
    class procedure LogInfo(const AMessage: string);
    class procedure LogError(const AMessage: string);
    class procedure LogWarning(const AMessage: string);
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

end.
