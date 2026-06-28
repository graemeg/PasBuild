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
    class procedure ParseBootstrapExclude(AParent: TDOMNode; AExclude: TStringList);
    class procedure ParseBuildSection(ABuildNode: TDOMNode; AConfig: TProjectConfig);
    class procedure ParseTestSection(ATestNode: TDOMNode; AConfig: TProjectConfig);
    class procedure ParseResourcesSection(AResourcesNode: TDOMNode; AResourcesConfig: TResourcesConfig);
    class procedure ParseSourcePackageSection(ASourcePackageNode: TDOMNode; AConfig: TProjectConfig);
    class procedure ParseModules(AModulesNode: TDOMNode; AModuleList: TStringList;
      AInactiveModuleList: TStringList = nil);
    class procedure ParseDependencies(ADepsNode: TDOMNode; AConfig: TProjectConfig);
    class procedure ParseProfile(AProfileNode: TDOMNode; AProfile: TProfile);
    class procedure ParseProfiles(AProfilesNode: TDOMNode; AConfig: TProjectConfig);
    class procedure ParseKeywords(AKeywordsNode: TDOMNode; AKeywords: TStringList);
    class procedure ParseRepositories(AReposNode: TDOMNode; AConfig: TProjectConfig);
    class procedure ValidatePackagingRules(AConfig: TProjectConfig);
  public
    class function LoadProjectXML(const AFilePath: string;
      AAllowEmptyVersion: Boolean = False): TProjectConfig;
    class function ValidateConfig(AConfig: TProjectConfig): Boolean;
    class function ValidateSemanticVersion(const AVersion: string): Boolean;
    class function FindProjectRoot(const AStartDir: string): string;
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

  Node := AParent.FindNode(UnicodeString(ATagName));
  if Assigned(Node) and Assigned(Node.FirstChild) then
    Result := Trim(string(Node.TextContent));
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
        ADefines.Add(Trim(string(DefineNode.TextContent)));
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
        AOptions.Add(Trim(string(OptionNode.TextContent)));
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

  PathsNode := AParent.FindNode(UnicodeString(ATagName));
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
        PathValue := Trim(string(PathNode.TextContent));
        if PathValue <> '' then
        begin
          // Get optional condition attribute
          Condition := '';
          if PathNode is TDOMElement then
          begin
            Element := TDOMElement(PathNode);
            if Element.HasAttribute('condition') then
              Condition := Trim(string(Element.GetAttribute('condition')));
          end;

          ConditionalPath := TConditionalPath.Create(PathValue, Condition);
          APaths.Add(ConditionalPath);
        end;
      end;
    end;
  end;
end;

class procedure TConfigLoader.ParseBootstrapExclude(AParent: TDOMNode; AExclude: TStringList);
var
  ExcludeNode, ExcUnitNode: TDOMNode;
  ExcUnitName: string;
  I: Integer;
begin
  if not Assigned(AParent) then
    Exit;

  ExcludeNode := AParent.FindNode('bootstrapExclude');
  if not Assigned(ExcludeNode) then
    Exit;

  for I := 0 to ExcludeNode.ChildNodes.Count - 1 do
  begin
    ExcUnitNode := ExcludeNode.ChildNodes[I];
    if (ExcUnitNode.NodeType = ELEMENT_NODE) and (ExcUnitNode.NodeName = 'unit') then
    begin
      ExcUnitName := Trim(string(ExcUnitNode.TextContent));
      if ExcUnitName <> '' then
        AExclude.Add(ExcUnitName);
    end;
  end;
end;

class procedure TConfigLoader.ParseBuildSection(ABuildNode: TDOMNode; AConfig: TProjectConfig);
var
  ProjectTypeStr: string;
  PackagingStr: string;
begin
  if not Assigned(ABuildNode) then
    raise EProjectConfigError.Create('Missing required <build> section');

  // Parse packaging type (prefer <packaging>, fallback to <projectType> for backward compat)
  PackagingStr := GetNodeText(ABuildNode, 'packaging', '');
  if PackagingStr <> '' then
  begin
    // New <packaging> element
    ProjectTypeStr := LowerCase(PackagingStr);
  end
  else
  begin
    // Backward compatibility: <projectType>
    ProjectTypeStr := LowerCase(GetNodeText(ABuildNode, 'projectType', 'application'));
  end;

  // Parse and validate packaging type
  if ProjectTypeStr = 'library' then
    AConfig.BuildConfig.ProjectType := ptLibrary
  else if ProjectTypeStr = 'application' then
    AConfig.BuildConfig.ProjectType := ptApplication
  else if ProjectTypeStr = 'pom' then
    AConfig.BuildConfig.ProjectType := ptPom
  else
    raise EProjectConfigError.CreateFmt('Invalid packaging type: %s (expected: application, library, or pom)', [ProjectTypeStr]);

  // Parse build configuration
  AConfig.BuildConfig.MainSource := GetNodeText(ABuildNode, 'mainSource');
  AConfig.BuildConfig.OutputDirectory := GetNodeText(ABuildNode, 'outputDirectory', 'target');
  AConfig.BuildConfig.ExecutableName := GetNodeText(ABuildNode, 'executableName');
  AConfig.BuildConfig.SourceDirectory := GetNodeText(ABuildNode, 'sourceDirectory', 'src/main/pascal');

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

  // Parse bootstrapExclude unit list (library projects only, but safe to parse for all)
  ParseBootstrapExclude(ABuildNode, AConfig.BuildConfig.BootstrapExclude);
end;

class procedure TConfigLoader.ParseTestSection(ATestNode: TDOMNode; AConfig: TProjectConfig);
var
  OptionsNode, OptionNode: TDOMNode;
  I: Integer;
begin
  if not Assigned(ATestNode) then
    Exit; // Test section is optional

  // Parse test source (optional)
  AConfig.TestConfig.TestSource := GetNodeText(ATestNode, 'testSource', 'TestRunner.pas');

  // Parse test source directory (optional, defaults to src/test/pascal)
  AConfig.TestConfig.SourceDirectory := GetNodeText(ATestNode, 'testSourceDirectory', 'src/test/pascal');

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
          AConfig.TestConfig.FrameworkOptions.Add(Trim(string(OptionNode.TextContent)));
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
        IncludeDir := Trim(string(IncludeNode.TextContent));
        if IncludeDir <> '' then
          AConfig.SourcePackageConfig.IncludeDirs.Add(IncludeDir);
      end;
    end;
  end;
end;

class procedure TConfigLoader.ParseModules(AModulesNode: TDOMNode; AModuleList: TStringList;
  AInactiveModuleList: TStringList = nil);
var
  ModuleNode: TDOMNode;
  Element: TDOMElement;
  ModulePath: string;
  ActiveByDefault: Boolean;
  I: Integer;
begin
  if not Assigned(AModulesNode) then
    Exit; // Modules are optional

  // Iterate through <module> children
  for I := 0 to AModulesNode.ChildNodes.Count - 1 do
  begin
    ModuleNode := AModulesNode.ChildNodes[I];
    if (ModuleNode.NodeType = ELEMENT_NODE) and (ModuleNode.NodeName = 'module') then
    begin
      if Assigned(ModuleNode.FirstChild) then
      begin
        ModulePath := Trim(string(ModuleNode.TextContent));
        if ModulePath <> '' then
        begin
          AModuleList.Add(ModulePath);

          // Read optional activeByDefault attribute (default: true)
          ActiveByDefault := True;
          if ModuleNode is TDOMElement then
          begin
            Element := TDOMElement(ModuleNode);
            if Element.HasAttribute('activeByDefault') then
              ActiveByDefault := LowerCase(Trim(string(Element.GetAttribute('activeByDefault')))) <> 'false';
          end;

          if (not ActiveByDefault) and Assigned(AInactiveModuleList) then
            AInactiveModuleList.Add(ModulePath);
        end;
      end;
    end;
  end;
end;

class procedure TConfigLoader.ParseDependencies(ADepsNode: TDOMNode; AConfig: TProjectConfig);
var
  DepNode: TDOMNode;
  DepName, DepVersion: string;
  I, J: Integer;
begin
  if not Assigned(ADepsNode) then
    Exit; // Dependencies section is optional

  for I := 0 to ADepsNode.ChildNodes.Count - 1 do
  begin
    DepNode := ADepsNode.ChildNodes[I];
    if (DepNode.NodeType = ELEMENT_NODE) and (DepNode.NodeName = 'dependency') then
    begin
      DepName := GetNodeText(DepNode, 'name');
      DepVersion := GetNodeText(DepNode, 'version');

      if DepName = '' then
        raise EProjectConfigError.Create('Dependency missing required <name>');
      if DepVersion = '' then
        raise EProjectConfigError.CreateFmt('Dependency "%s" missing required <version>', [DepName]);
      if not ValidateSemanticVersion(DepVersion) then
        raise EProjectConfigError.CreateFmt('Dependency "%s" has invalid version: %s', [DepName, DepVersion]);

      // Check for duplicate dependency names
      for J := 0 to AConfig.Dependencies.Count - 1 do
      begin
        if AConfig.Dependencies[J].Name = DepName then
          raise EProjectConfigError.CreateFmt('Duplicate dependency: "%s"', [DepName]);
      end;

      AConfig.Dependencies.Add(TDependencyInfo.Create(DepName, DepVersion));
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

class procedure TConfigLoader.ParseKeywords(AKeywordsNode: TDOMNode; AKeywords: TStringList);
var
  Node: TDOMNode;
  Keyword: string;
  I: Integer;
begin
  if not Assigned(AKeywordsNode) then
    Exit;

  for I := 0 to AKeywordsNode.ChildNodes.Count - 1 do
  begin
    Node := AKeywordsNode.ChildNodes[I];
    if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'keyword') then
    begin
      if Assigned(Node.FirstChild) then
      begin
        Keyword := Trim(string(Node.TextContent));
        if Keyword <> '' then
          AKeywords.Add(Keyword);
      end;
    end;
  end;
end;

class procedure TConfigLoader.ParseRepositories(AReposNode: TDOMNode; AConfig: TProjectConfig);
var
  RepoNode: TDOMNode;
  Id, Url: string;
  I: Integer;
begin
  if not Assigned(AReposNode) then
    Exit;

  for I := 0 to AReposNode.ChildNodes.Count - 1 do
  begin
    RepoNode := AReposNode.ChildNodes[I];
    if (RepoNode.NodeType = ELEMENT_NODE) and (RepoNode.NodeName = 'repository') then
    begin
      Id := GetNodeText(RepoNode, 'id');
      Url := GetNodeText(RepoNode, 'url');
      if (Id <> '') and (Url <> '') then
        AConfig.Repositories.Add(TRepositoryInfo.Create(Id, Url));
    end;
  end;
end;

class function TConfigLoader.LoadProjectXML(const AFilePath: string;
  AAllowEmptyVersion: Boolean = False): TProjectConfig;
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
      if (Result.Version = '') and (not AAllowEmptyVersion) then
        raise EProjectConfigError.Create('Missing required field: <version>. ' +
          'If this is a child module, run pasbuild from the project root directory instead');

      // Parse optional metadata (defaults already set in TProjectConfig.Create)
      Result.Author := GetNodeText(RootNode, 'author', 'Unknown');
      Result.License := GetNodeText(RootNode, 'license', 'Proprietary');
      Result.ProjectUrl := GetNodeText(RootNode, 'projectUrl');
      Result.RepoUrl := GetNodeText(RootNode, 'repoUrl');
      Result.Description := GetNodeText(RootNode, 'description');

      // Parse <keywords> section (optional)
      ParseKeywords(RootNode.FindNode('keywords'), Result.Keywords);

      // Parse <repositories> section (optional)
      ParseRepositories(RootNode.FindNode('repositories'), Result);

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

      // Parse <modules> section (optional, aggregators only)
      // Lists child modules for reactor builds
      ParseModules(RootNode.FindNode('modules'), Result.Modules, Result.InactiveModules);

      // Parse <moduleDependencies> section (optional, libraries and applications only)
      // Lists module dependencies for linking
      ParseModules(RootNode.FindNode('moduleDependencies'), Result.ModuleDependencies);

      // Parse <dependencies> section (optional, external repository dependencies)
      ParseDependencies(RootNode.FindNode('dependencies'), Result);

      // Validate packaging rules (multi-module constraints)
      ValidatePackagingRules(Result);

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

  // Validate semantic version (skip if empty - version may be inherited from aggregator)
  if AConfig.Version <> '' then
  begin
    if not ValidateSemanticVersion(AConfig.Version) then
      raise EProjectConfigError.CreateFmt('Invalid version format: %s (expected: MAJOR.MINOR.PATCH or MAJOR.MINOR.PATCH-PRERELEASE)', [AConfig.Version]);
  end;

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
  // Support Semantic Versioning 2.0.0: MAJOR.MINOR.PATCH[-PRERELEASE]
  // Examples: 1.0.0, 1.0.0-alpha, 1.0.0-beta.2, 1.0.0-rc.1, 1.0.0-SNAPSHOT
  RegEx := TRegExpr.Create('^\d+\.\d+\.\d+(-[a-zA-Z0-9.-]+)?$');
  try
    Result := RegEx.Exec(AVersion);
  finally
    RegEx.Free;
  end;
end;

class procedure TConfigLoader.ValidatePackagingRules(AConfig: TProjectConfig);
begin
  if not Assigned(AConfig) then
    raise EProjectConfigError.Create('Config object is nil');

  case AConfig.BuildConfig.ProjectType of
    ptPom:
      begin
        // Rule: Aggregator must have child modules
        if AConfig.Modules.Count = 0 then
          raise EProjectConfigError.Create('POM aggregator requires <modules> list (at least one child module)');

        // Rule: Aggregator forbids MainSource
        if AConfig.BuildConfig.MainSource <> '' then
          raise EProjectConfigError.Create('POM aggregator cannot have <mainSource> (aggregators do not compile)');

        // Rule: Aggregator forbids module dependencies (conceptually different from child modules)
        if AConfig.ModuleDependencies.Count > 0 then
          raise EProjectConfigError.Create('POM aggregator cannot have <moduleDependencies> (only child modules via <modules>)');
      end;

    ptLibrary:
      begin
        // Rule: Library forbids child modules (cannot be an aggregator)
        if AConfig.Modules.Count > 0 then
          raise EProjectConfigError.Create('Library cannot have child <modules> (only aggregator projects can list modules)');

        // Module dependencies are allowed for libraries (optional)
      end;

    ptApplication:
      begin
        // Rule: Application forbids child modules (cannot be an aggregator)
        if AConfig.Modules.Count > 0 then
          raise EProjectConfigError.Create('Application cannot have child <modules> (only aggregator projects can list modules)');

        // Module dependencies are allowed for applications (optional)
      end;
  end;
end;

class function TConfigLoader.FindProjectRoot(const AStartDir: string): string;
var
  Dir, ParentDir, ProjectFile: string;
  Doc: TXMLDocument;
  RootNode, BuildNode, PackagingNode: TDOMNode;
  Packaging: string;
begin
  Result := '';
  Dir := ExcludeTrailingPathDelimiter(ExpandFileName(AStartDir));

  if not DirectoryExists(Dir) then
    Exit;

  repeat
    ParentDir := ExcludeTrailingPathDelimiter(ExtractFilePath(Dir));
    if ParentDir = Dir then
      Break;
    Dir := ParentDir;

    ProjectFile := IncludeTrailingPathDelimiter(Dir) + 'project.xml';
    if not FileExists(ProjectFile) then
      Continue;

    try
      ReadXMLFile(Doc, ProjectFile);
      try
        RootNode := Doc.DocumentElement;
        if (RootNode = nil) or (RootNode.NodeName <> 'project') then
          Continue;

        Packaging := '';
        BuildNode := RootNode.FindNode('build');
        if Assigned(BuildNode) then
        begin
          PackagingNode := BuildNode.FindNode('packaging');
          if Assigned(PackagingNode) then
            Packaging := Trim(PackagingNode.TextContent);
        end;

        if SameText(Packaging, 'pom') then
          Result := Dir;
      finally
        Doc.Free;
      end;
    except
      // Malformed XML — skip this directory
    end;
  until False;
end;

end.
