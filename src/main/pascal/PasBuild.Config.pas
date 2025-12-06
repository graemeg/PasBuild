{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, DOM, XMLRead,
  PasBuild.Types;

type
  { Exception raised when project.xml is invalid }
  EProjectConfigError = class(Exception);

  { Configuration loader and validator }
  TConfigLoader = class
  private
    class function GetNodeText(AParent: TDOMNode; const ATagName: string; const ADefault: string = ''): string;
    class procedure ParseDefines(AParent: TDOMNode; ADefines: TStringList);
    class procedure ParseCompilerOptions(AParent: TDOMNode; AOptions: TStringList);
    class procedure ParseBuildSection(ABuildNode: TDOMNode; AConfig: TProjectConfig);
    class procedure ParseProfile(AProfileNode: TDOMNode; AProfile: TProfile);
    class procedure ParseProfiles(AProfilesNode: TDOMNode; AConfig: TProjectConfig);
  public
    class function LoadProjectXML(const AFilePath: string): TProjectConfig;
    class function ValidateConfig(AConfig: TProjectConfig): Boolean;
    class function ValidateSemanticVersion(const AVersion: string): Boolean;
  end;

implementation

uses
  RegExpr;

{ TConfigLoader }

class function TConfigLoader.GetNodeText(AParent: TDOMNode; const ATagName: string; const ADefault: string): string;
var
  Node: TDOMNode;
begin
  Result := ADefault;
  if not Assigned(AParent) then
    Exit;

  Node := AParent.FindNode(ATagName);
  if Assigned(Node) and Assigned(Node.FirstChild) then
    Result := Trim(Node.TextContent);
end;

class procedure TConfigLoader.ParseDefines(AParent: TDOMNode; ADefines: TStringList);
var
  DefinesNode, DefineNode: TDOMNode;
  I: Integer;
begin
  if not Assigned(AParent) then
    Exit;

  DefinesNode := AParent.FindNode('defines');
  if not Assigned(DefinesNode) then
    Exit;

  // Iterate through <define> children
  for I := 0 to DefinesNode.ChildNodes.Count - 1 do
  begin
    DefineNode := DefinesNode.ChildNodes[I];
    if (DefineNode.NodeType = ELEMENT_NODE) and (DefineNode.NodeName = 'define') then
    begin
      if Assigned(DefineNode.FirstChild) then
        ADefines.Add(Trim(DefineNode.TextContent));
    end;
  end;
end;

class procedure TConfigLoader.ParseCompilerOptions(AParent: TDOMNode; AOptions: TStringList);
var
  OptionsNode, OptionNode: TDOMNode;
  I: Integer;
begin
  if not Assigned(AParent) then
    Exit;

  OptionsNode := AParent.FindNode('compilerOptions');
  if not Assigned(OptionsNode) then
    Exit;

  // Iterate through <option> children
  for I := 0 to OptionsNode.ChildNodes.Count - 1 do
  begin
    OptionNode := OptionsNode.ChildNodes[I];
    if (OptionNode.NodeType = ELEMENT_NODE) and (OptionNode.NodeName = 'option') then
    begin
      if Assigned(OptionNode.FirstChild) then
        AOptions.Add(Trim(OptionNode.TextContent));
    end;
  end;
end;

class procedure TConfigLoader.ParseBuildSection(ABuildNode: TDOMNode; AConfig: TProjectConfig);
begin
  if not Assigned(ABuildNode) then
    raise EProjectConfigError.Create('Missing required <build> section');

  // Parse build configuration
  AConfig.BuildConfig.MainSource := GetNodeText(ABuildNode, 'mainSource');
  AConfig.BuildConfig.OutputDirectory := GetNodeText(ABuildNode, 'outputDirectory', 'target');
  AConfig.BuildConfig.ExecutableName := GetNodeText(ABuildNode, 'executableName');

  // Parse global defines
  ParseDefines(ABuildNode, AConfig.BuildConfig.Defines);
end;

class procedure TConfigLoader.ParseProfile(AProfileNode: TDOMNode; AProfile: TProfile);
begin
  // Parse profile ID (required)
  AProfile.Id := GetNodeText(AProfileNode, 'id');
  if AProfile.Id = '' then
    raise EProjectConfigError.Create('Profile missing required <id>');

  // Parse profile defines
  ParseDefines(AProfileNode, AProfile.Defines);

  // Parse profile compiler options
  ParseCompilerOptions(AProfileNode, AProfile.CompilerOptions);
end;

class procedure TConfigLoader.ParseProfiles(AProfilesNode: TDOMNode; AConfig: TProjectConfig);
var
  ProfileNode: TDOMNode;
  Profile: TProfile;
  I: Integer;
begin
  if not Assigned(AProfilesNode) then
    Exit; // Profiles are optional

  // Iterate through <profile> children
  for I := 0 to AProfilesNode.ChildNodes.Count - 1 do
  begin
    ProfileNode := AProfilesNode.ChildNodes[I];
    if (ProfileNode.NodeType = ELEMENT_NODE) and (ProfileNode.NodeName = 'profile') then
    begin
      Profile := TProfile.Create;
      try
        ParseProfile(ProfileNode, Profile);
        AConfig.Profiles.Add(Profile);
      except
        Profile.Free;
        raise;
      end;
    end;
  end;
end;

class function TConfigLoader.LoadProjectXML(const AFilePath: string): TProjectConfig;
var
  Doc: TXMLDocument;
  RootNode, BuildNode, ProfilesNode: TDOMNode;
begin
  Result := nil;

  // Check if file exists
  if not FileExists(AFilePath) then
    raise EProjectConfigError.CreateFmt('project.xml not found: %s', [AFilePath]);

  try
    // Parse XML document
    try
      ReadXMLFile(Doc, AFilePath);
    except
      on E: Exception do
        raise EProjectConfigError.CreateFmt('Invalid XML in project.xml: %s', [E.Message]);
    end;

    try
      Result := TProjectConfig.Create;

      // Get root <project> element
      RootNode := Doc.DocumentElement;
      if RootNode.NodeName <> 'project' then
        raise EProjectConfigError.Create('Root element must be <project>');

      // Parse required metadata
      Result.Name := GetNodeText(RootNode, 'name');
      if Result.Name = '' then
        raise EProjectConfigError.Create('Missing required field: <name>');

      Result.Version := GetNodeText(RootNode, 'version');
      if Result.Version = '' then
        raise EProjectConfigError.Create('Missing required field: <version>');

      // Parse optional metadata (defaults already set in TProjectConfig.Create)
      Result.Author := GetNodeText(RootNode, 'author', 'Unknown');
      Result.License := GetNodeText(RootNode, 'license', 'Proprietary');
      Result.ProjectUrl := GetNodeText(RootNode, 'projectUrl');
      Result.RepoUrl := GetNodeText(RootNode, 'repoUrl');

      // Parse <build> section (required)
      BuildNode := RootNode.FindNode('build');
      ParseBuildSection(BuildNode, Result);

      // Validate required build fields
      if Result.BuildConfig.MainSource = '' then
        raise EProjectConfigError.Create('Missing required field: <build><mainSource>');

      // Set default executable name if not specified
      if Result.BuildConfig.ExecutableName = '' then
        Result.BuildConfig.ExecutableName := LowerCase(Result.Name);

      // Parse <profiles> section (optional)
      ProfilesNode := RootNode.FindNode('profiles');
      ParseProfiles(ProfilesNode, Result);

    finally
      Doc.Free;
    end;

  except
    on E: Exception do
    begin
      if Assigned(Result) then
        Result.Free;
      raise;
    end;
  end;
end;

class function TConfigLoader.ValidateConfig(AConfig: TProjectConfig): Boolean;
var
  RegEx: TRegExpr;
begin
  Result := False;

  if not Assigned(AConfig) then
    raise EProjectConfigError.Create('Config object is nil');

  // Validate semantic version
  if not ValidateSemanticVersion(AConfig.Version) then
    raise EProjectConfigError.CreateFmt('Invalid version format: %s (expected: MAJOR.MINOR.PATCH)', [AConfig.Version]);

  // Validate name contains only valid characters
  RegEx := TRegExpr.Create('^[a-zA-Z0-9_-]+$');
  try
    if not RegEx.Exec(AConfig.Name) then
      raise EProjectConfigError.CreateFmt('Invalid project name: %s (only alphanumeric, hyphens, underscores allowed)', [AConfig.Name]);
  finally
    RegEx.Free;
  end;

  // Validate main source file has valid extension
  if not (AnsiEndsStr('.pas', AConfig.BuildConfig.MainSource) or
          AnsiEndsStr('.pp', AConfig.BuildConfig.MainSource) or
          AnsiEndsStr('.lpr', AConfig.BuildConfig.MainSource)) then
    raise EProjectConfigError.CreateFmt('Invalid main source extension: %s (expected: .pas, .pp, or .lpr)', [AConfig.BuildConfig.MainSource]);

  Result := True;
end;

class function TConfigLoader.ValidateSemanticVersion(const AVersion: string): Boolean;
var
  RegEx: TRegExpr;
begin
  RegEx := TRegExpr.Create('^\d+\.\d+\.\d+$');
  try
    Result := RegEx.Exec(AVersion);
  finally
    RegEx.Free;
  end;
end;

end.
