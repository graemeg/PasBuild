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
    class procedure ParseConditionalPaths(AParent: TDOMNode; const ATagName: string; APaths: TConditionalPathList);
    class procedure ParseBuildSection(ABuildNode: TDOMNode; AConfig: TProjectConfig);
    class procedure ParseTestSection(ATestNode: TDOMNode; AConfig: TProjectConfig);
    class procedure ParseResourcesSection(AResourcesNode: TDOMNode; AResourcesConfig: TResourcesConfig);
    class procedure ParseSourcePackageSection(ASourcePackageNode: TDOMNode; AConfig: TProjectConfig);
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

class procedure TConfigLoader.ParseConditionalPaths(AParent: TDOMNode; const ATagName: string; APaths: TConditionalPathList);
var
  PathsNode, PathNode: TDOMNode;
  Element: TDOMElement;
  ConditionalPath: TConditionalPath;
  PathValue, Condition: string;
  I: Integer;
begin
  if not Assigned(AParent) then
    Exit;

  PathsNode := AParent.FindNode(ATagName);
  if not Assigned(PathsNode) then
    Exit;

  // Iterate through <path> children
  for I := 0 to PathsNode.ChildNodes.Count - 1 do
  begin
    PathNode := PathsNode.ChildNodes[I];
    if (PathNode.NodeType = ELEMENT_NODE) and (PathNode.NodeName = 'path') then
    begin
      if Assigned(PathNode.FirstChild) then
      begin
        PathValue := Trim(PathNode.TextContent);
        if PathValue <> '' then
        begin
          // Get optional condition attribute
          Condition := '';
          if PathNode is TDOMElement then
          begin
            Element := TDOMElement(PathNode);
            if Element.HasAttribute('condition') then
              Condition := Trim(Element.GetAttribute('condition'));
          end;

          ConditionalPath := TConditionalPath.Create(PathValue, Condition);
          APaths.Add(ConditionalPath);
        end;
      end;
    end;
  end;
end;

class procedure TConfigLoader.ParseBuildSection(ABuildNode: TDOMNode; AConfig: TProjectConfig);
var
  ProjectTypeStr: string;
begin
  if not Assigned(ABuildNode) then
    raise EProjectConfigError.Create('Missing required <build> section');

  // Parse project type (default: application)
  ProjectTypeStr := LowerCase(GetNodeText(ABuildNode, 'projectType', 'application'));
  if ProjectTypeStr = 'library' then
    AConfig.BuildConfig.ProjectType := ptLibrary
  else if ProjectTypeStr = 'application' then
    AConfig.BuildConfig.ProjectType := ptApplication
  else
    raise EProjectConfigError.CreateFmt('Invalid project type: %s (expected: application or library)', [ProjectTypeStr]);

  // Parse build configuration
  AConfig.BuildConfig.MainSource := GetNodeText(ABuildNode, 'mainSource');
  AConfig.BuildConfig.OutputDirectory := GetNodeText(ABuildNode, 'outputDirectory', 'target');
  AConfig.BuildConfig.ExecutableName := GetNodeText(ABuildNode, 'executableName');

  // Parse manualUnitPaths flag (default: false = auto-scan enabled)
  AConfig.BuildConfig.ManualUnitPaths := LowerCase(GetNodeText(ABuildNode, 'manualUnitPaths', 'false')) = 'true';

  // Parse global defines
  ParseDefines(ABuildNode, AConfig.BuildConfig.Defines);

  // Parse global compiler options
  ParseCompilerOptions(ABuildNode, AConfig.BuildConfig.CompilerOptions);

  // Parse conditional unit paths
  ParseConditionalPaths(ABuildNode, 'unitPaths', AConfig.BuildConfig.UnitPaths);

  // Parse conditional include paths
  ParseConditionalPaths(ABuildNode, 'includePaths', AConfig.BuildConfig.IncludePaths);
end;

class procedure TConfigLoader.ParseTestSection(ATestNode: TDOMNode; AConfig: TProjectConfig);
var
  FrameworkStr: string;
  OptionsNode, OptionNode: TDOMNode;
  I: Integer;
begin
  if not Assigned(ATestNode) then
    Exit; // Test section is optional

  // Parse framework (optional, defaults to auto)
  FrameworkStr := LowerCase(GetNodeText(ATestNode, 'framework', 'auto'));
  if FrameworkStr = 'fpcunit' then
    AConfig.TestConfig.Framework := tfFPCUnit
  else if FrameworkStr = 'fptest' then
    AConfig.TestConfig.Framework := tfFPTest
  else if FrameworkStr = 'auto' then
    AConfig.TestConfig.Framework := tfAuto
  else
    raise EProjectConfigError.CreateFmt('Invalid test framework: %s (expected: auto, fpcunit, or fptest)', [FrameworkStr]);

  // Parse test source (optional)
  AConfig.TestConfig.TestSource := GetNodeText(ATestNode, 'testSource', 'TestRunner.pas');

  // Parse framework-specific options
  OptionsNode := ATestNode.FindNode('frameworkOptions');
  if Assigned(OptionsNode) then
  begin
    for I := 0 to OptionsNode.ChildNodes.Count - 1 do
    begin
      OptionNode := OptionsNode.ChildNodes[I];
      if (OptionNode.NodeType = ELEMENT_NODE) and (OptionNode.NodeName = 'option') then
      begin
        if Assigned(OptionNode.FirstChild) then
          AConfig.TestConfig.FrameworkOptions.Add(Trim(OptionNode.TextContent));
      end;
    end;
  end;
end;

class procedure TConfigLoader.ParseResourcesSection(AResourcesNode: TDOMNode; AResourcesConfig: TResourcesConfig);
var
  FilteringStr: string;
begin
  if not Assigned(AResourcesNode) then
    Exit; // Resources section is optional

  // Parse directory (optional)
  AResourcesConfig.Directory := GetNodeText(AResourcesNode, 'directory', AResourcesConfig.Directory);

  // Parse filtering (optional)
  FilteringStr := LowerCase(GetNodeText(AResourcesNode, 'filtering', 'false'));
  AResourcesConfig.Filtering := (FilteringStr = 'true') or (FilteringStr = '1') or (FilteringStr = 'yes');
end;

class procedure TConfigLoader.ParseSourcePackageSection(ASourcePackageNode: TDOMNode; AConfig: TProjectConfig);
var
  IncludeNode: TDOMNode;
  I: Integer;
  IncludeDir: string;
begin
  if not Assigned(ASourcePackageNode) then
    Exit; // Source package section is optional

  // Parse <include> children
  for I := 0 to ASourcePackageNode.ChildNodes.Count - 1 do
  begin
    IncludeNode := ASourcePackageNode.ChildNodes[I];
    if (IncludeNode.NodeType = ELEMENT_NODE) and (IncludeNode.NodeName = 'include') then
    begin
      if Assigned(IncludeNode.FirstChild) then
      begin
        IncludeDir := Trim(IncludeNode.TextContent);
        if IncludeDir <> '' then
          AConfig.SourcePackageConfig.IncludeDirs.Add(IncludeDir);
      end;
    end;
  end;
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
  RootNode, BuildNode, TestNode, ProfilesNode: TDOMNode;
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
      // MainSource is required for applications, optional for libraries (auto-generated bootstrap)
      if (Result.BuildConfig.ProjectType = ptApplication) and (Result.BuildConfig.MainSource = '') then
        raise EProjectConfigError.Create('Missing required field: <build><mainSource> for application projects');

      // Set default executable name if not specified
      if Result.BuildConfig.ExecutableName = '' then
        Result.BuildConfig.ExecutableName := LowerCase(Result.Name);

      // Parse <test> section (optional)
      TestNode := RootNode.FindNode('test');
      ParseTestSection(TestNode, Result);

      // Parse <resources> section under <build> (optional)
      ParseResourcesSection(BuildNode.FindNode('resources'), Result.ResourcesConfig);

      // Parse <resources> section under <test> (optional)
      if Assigned(TestNode) then
        ParseResourcesSection(TestNode.FindNode('resources'), Result.TestResourcesConfig);

      // Parse <sourcePackage> section (optional)
      ParseSourcePackageSection(BuildNode.FindNode('sourcePackage'), Result);

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

  // Validate main source file has valid extension (only for applications with mainSource)
  if (AConfig.BuildConfig.ProjectType = ptApplication) and (AConfig.BuildConfig.MainSource <> '') then
  begin
    if not (AnsiEndsStr('.pas', AConfig.BuildConfig.MainSource) or
            AnsiEndsStr('.pp', AConfig.BuildConfig.MainSource) or
            AnsiEndsStr('.lpr', AConfig.BuildConfig.MainSource)) then
      raise EProjectConfigError.CreateFmt('Invalid main source extension: %s (expected: .pas, .pp, or .lpr)', [AConfig.BuildConfig.MainSource]);
  end;

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
