{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Plugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  PasBuild.Types,
  PasBuild.CLI,
  PasBuild.Command;

type
  { Result of querying a plugin for its phase }
  TPluginPhaseInfo = record
    Valid: Boolean;
    AfterGoal: TBuildGoal;
    Description: string;
  end;

  { Discovers and queries external plugins }
  TPluginDiscovery = class
  public
    { Search for a plugin executable named pasbuild-<AGoalName>.
      Search order: <project-dir>/plugins/ -> ~/.pasbuild/plugins/ -> PATH.
      Returns full path or empty string if not found. }
    class function DiscoverPlugin(const AGoalName: string): string;

    { Scan all plugin locations and return deduplicated list of goal names. }
    class function DiscoverAllPlugins: TStringList;

    { Run <AExecutablePath> --pasbuild-phase and parse the response. }
    class function QueryPluginPhase(const AExecutablePath: string): TPluginPhaseInfo;
  private
    class procedure ScanDirectory(const ADir: string; AList: TStringList);
    class function FindExecutable(const ADir, AName: string): string;
  end;

  { Command wrapper for external plugin execution }
  TPluginCommand = class(TBuildCommand)
  private
    FExecutablePath: string;
    FGoalName: string;
    FAfterGoal: TBuildGoal;
    FRegistry: TModuleRegistry;
  protected
    function GetName: string; override;
  public
    constructor Create(AConfig: TProjectConfig; AProfileIds: TStringList;
      const AExecutablePath, AGoalName: string; AAfterGoal: TBuildGoal;
      ARegistry: TModuleRegistry = nil);
    function Execute: Integer; override;
    function GetDependencies: TBuildCommandList; override;
  end;

implementation

uses
  Process,
  PasBuild.Utils,
  PasBuild.Command.Clean,
  PasBuild.Command.ProcessResources,
  PasBuild.Command.Compile,
  PasBuild.Command.ProcessTestResources,
  PasBuild.Command.Test,
  PasBuild.Command.Package,
  PasBuild.Command.SourcePackage,
  PasBuild.Command.Install,
  PasBuild.Command.Reactor;

const
  PluginPrefix = 'pasbuild-';

{ TPluginDiscovery }

class function TPluginDiscovery.FindExecutable(const ADir, AName: string): string;
var
  FullPath: string;
begin
  Result := '';
  if not DirectoryExists(ADir) then
    Exit;

  // Check with platform suffix (.exe on Windows)
  FullPath := IncludeTrailingPathDelimiter(ADir) + AName + TUtils.GetPlatformExecutableSuffix;
  if FileExists(FullPath) then
  begin
    Result := FullPath;
    Exit;
  end;

  {$IFDEF UNIX}
  // On Unix, also check without suffix (shell scripts)
  FullPath := IncludeTrailingPathDelimiter(ADir) + AName;
  if FileExists(FullPath) then
    Result := FullPath;
  {$ENDIF}
end;

class function TPluginDiscovery.DiscoverPlugin(const AGoalName: string): string;
var
  ExeName: string;
  UserPluginDir: string;
begin
  ExeName := PluginPrefix + AGoalName;

  // 1. Project-local plugins directory
  Result := FindExecutable(GetCurrentDir + DirectorySeparator + 'plugins', ExeName);
  if Result <> '' then
    Exit;

  // 2. User-global plugins directory
  UserPluginDir := IncludeTrailingPathDelimiter(GetUserDir) + '.pasbuild'
    + DirectorySeparator + 'plugins';
  Result := FindExecutable(UserPluginDir, ExeName);
  if Result <> '' then
    Exit;

  // 3. Search PATH
  {$IFDEF UNIX}
  Result := ExeSearch(ExeName);
  {$ELSE}
  Result := ExeSearch(ExeName + TUtils.GetPlatformExecutableSuffix);
  {$ENDIF}
end;

class procedure TPluginDiscovery.ScanDirectory(const ADir: string; AList: TStringList);
var
  SearchRec: TSearchRec;
  Name: string;
  PrefixLen: Integer;
  {$IFDEF WINDOWS}
  SuffixLen: Integer;
  {$ENDIF}
begin
  if not DirectoryExists(ADir) then
    Exit;

  PrefixLen := Length(PluginPrefix);
  {$IFDEF WINDOWS}
  SuffixLen := Length(TUtils.GetPlatformExecutableSuffix);
  {$ENDIF}

  if FindFirst(IncludeTrailingPathDelimiter(ADir) + PluginPrefix + '*',
    faAnyFile and not faDirectory, SearchRec) = 0 then
  begin
    try
      repeat
        Name := SearchRec.Name;
        // Strip the pasbuild- prefix
        Delete(Name, 1, PrefixLen);
        {$IFDEF WINDOWS}
        // Strip .exe suffix on Windows
        if (SuffixLen > 0) and (Length(Name) > SuffixLen) and
           (CompareText(RightStr(Name, SuffixLen), TUtils.GetPlatformExecutableSuffix) = 0) then
          Delete(Name, Length(Name) - SuffixLen + 1, SuffixLen);
        {$ENDIF}
        if Name <> '' then
          AList.Add(Name);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

class function TPluginDiscovery.DiscoverAllPlugins: TStringList;
var
  UserPluginDir: string;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;
  Result.Sorted := True;

  // Scan project-local plugins
  ScanDirectory(GetCurrentDir + DirectorySeparator + 'plugins', Result);

  // Scan user-global plugins
  UserPluginDir := IncludeTrailingPathDelimiter(GetUserDir) + '.pasbuild'
    + DirectorySeparator + 'plugins';
  ScanDirectory(UserPluginDir, Result);

  // Note: We don't scan entire PATH — that would be expensive.
  // PATH plugins are discoverable by name but not listed in --help.
end;

class function TPluginDiscovery.QueryPluginPhase(const AExecutablePath: string): TPluginPhaseInfo;
var
  Output: string;
  ExitCode: Integer;
  PhaseLine: string;
begin
  Result.Valid := False;
  Result.AfterGoal := bgUnknown;
  Result.Description := '';

  ExitCode := TUtils.ExecuteProcessWithCapture(
    TUtils.QuotePath(AExecutablePath) + ' --pasbuild-phase', Output);

  if ExitCode <> 0 then
    Exit;

  PhaseLine := Trim(Output);

  // Handle "none" — no dependencies
  if LowerCase(PhaseLine) = 'none' then
  begin
    Result.Valid := True;
    Result.AfterGoal := bgUnknown;
    Exit;
  end;

  // Handle "after:<goal>" format
  if Pos('after:', LowerCase(PhaseLine)) = 1 then
    Delete(PhaseLine, 1, Length('after:'));

  // Parse the goal name
  Result.AfterGoal := TArgumentParser.GoalFromString(PhaseLine);
  Result.Valid := Result.AfterGoal <> bgUnknown;
end;

{ TPluginCommand }

constructor TPluginCommand.Create(AConfig: TProjectConfig; AProfileIds: TStringList;
  const AExecutablePath, AGoalName: string; AAfterGoal: TBuildGoal;
  ARegistry: TModuleRegistry = nil);
begin
  inherited Create(AConfig, AProfileIds);
  FExecutablePath := AExecutablePath;
  FGoalName := AGoalName;
  FAfterGoal := AAfterGoal;
  FRegistry := ARegistry;
end;

function TPluginCommand.GetName: string;
begin
  Result := FGoalName;
end;

function TPluginCommand.GetDependencies: TBuildCommandList;
var
  Dep: TBuildCommand;
begin
  Result := TBuildCommandList.Create(False);
  try
    Dep := nil;
    case FAfterGoal of
      bgClean:
        if FRegistry <> nil then
          Dep := TReactorCommand.Create(Config, ProfileIds, FRegistry, 'clean')
        else
          Dep := TCleanCommand.Create(Config, ProfileIds);
      bgProcessResources:
        Dep := TProcessResourcesCommand.Create(Config, Config.ResourcesConfig,
          Config.BuildConfig.OutputDirectory);
      bgCompile:
        if FRegistry <> nil then
          Dep := TReactorCommand.Create(Config, ProfileIds, FRegistry, 'compile')
        else
          Dep := TCompileCommand.Create(Config, ProfileIds);
      bgProcessTestResources:
        Dep := TProcessTestResourcesCommand.Create(Config, Config.TestResourcesConfig,
          Config.BuildConfig.OutputDirectory);
      bgTestCompile:
        if FRegistry <> nil then
          Dep := TReactorCommand.Create(Config, ProfileIds, FRegistry, 'test-compile')
        else
          Dep := TTestCompileCommand.Create(Config, ProfileIds);
      bgTest:
        if FRegistry <> nil then
          Dep := TReactorCommand.Create(Config, ProfileIds, FRegistry, 'test')
        else
          Dep := TTestCommand.Create(Config, ProfileIds);
      bgPackage:
        if FRegistry <> nil then
          Dep := TReactorCommand.Create(Config, ProfileIds, FRegistry, 'package')
        else
          Dep := TPackageCommand.Create(Config, ProfileIds);
      bgSourcePackage:
        if FRegistry <> nil then
          Dep := TReactorCommand.Create(Config, ProfileIds, FRegistry, 'source-package')
        else
          Dep := TSourcePackageCommand.Create(Config, ProfileIds);
      bgInstall:
        if FRegistry <> nil then
          Dep := TReactorCommand.Create(Config, ProfileIds, FRegistry, 'install')
        else
          Dep := TInstallCommand.Create(Config, ProfileIds);
    end;
    if Dep <> nil then
      Result.Add(Dep);
  except
    Result.Free;
    raise;
  end;
end;

function TPluginCommand.Execute: Integer;
var
  AProcess: TProcess;
  ProfileStr: string;
  Buffer: array[0..4095] of Char;
  BytesRead: Integer;
  I: Integer;
begin
  TUtils.LogInfo('Executing plugin: ' + FGoalName);

  // Build comma-separated profiles string
  ProfileStr := '';
  for I := 0 to ProfileIds.Count - 1 do
  begin
    if I > 0 then
      ProfileStr := ProfileStr + ',';
    ProfileStr := ProfileStr + ProfileIds[I];
  end;

  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := FExecutablePath;
    AProcess.Parameters.Add(GetCurrentDir);

    // Inherit parent environment then add plugin-specific vars.
    // TProcess only inherits the environment when Environment.Count = 0,
    // so we must copy it explicitly before adding our own variables.
    for I := 1 to GetEnvironmentVariableCount do
      AProcess.Environment.Add(GetEnvironmentString(I));
    AProcess.Environment.Add('PASBUILD_PROJECT_DIR=' + GetCurrentDir);
    AProcess.Environment.Add('PASBUILD_PROJECT_FILE=project.xml');
    AProcess.Environment.Add('PASBUILD_PROFILES=' + ProfileStr);
    if Verbose then
      AProcess.Environment.Add('PASBUILD_VERBOSE=1')
    else
      AProcess.Environment.Add('PASBUILD_VERBOSE=0');

    AProcess.Options := [poUsePipes, poStderrToOutPut];

    try
      AProcess.Execute;

      // Stream plugin output to console
      repeat
        BytesRead := AProcess.Output.Read(Buffer, SizeOf(Buffer));
        if BytesRead > 0 then
          System.Write(Copy(Buffer, 1, BytesRead));
      until (BytesRead = 0) and not AProcess.Running;

      // Read any remaining output
      repeat
        BytesRead := AProcess.Output.Read(Buffer, SizeOf(Buffer));
        if BytesRead > 0 then
          System.Write(Copy(Buffer, 1, BytesRead));
      until BytesRead = 0;

      Result := TUtils.ProcessExitCode(AProcess);
    except
      on E: Exception do
      begin
        TUtils.LogError('Failed to execute plugin: ' + E.Message);
        Result := 1;
      end;
    end;
  finally
    AProcess.Free;
  end;

  if Result = 0 then
    TUtils.LogInfo('Plugin ' + FGoalName + ' completed successfully')
  else
    TUtils.LogError('Plugin ' + FGoalName + ' failed with exit code ' + IntToStr(Result));
end;

end.
