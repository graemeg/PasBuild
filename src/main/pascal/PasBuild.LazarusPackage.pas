unit PasBuild.LazarusPackage;
interface

uses
  Classes, SysUtils, DOM, XMLWrite,
  PasBuild.Types,
  PasBuild.Utils,
  PasBuild.Bootstrap;

type
  { Lazarus package generator for library projects }

  { TLazarusPackageGenerator }

  TLazarusPackageGenerator = class(TBootstrapGenerator)
  public
    class function GenerateLazarusPackage(AConfig: TProjectConfig;
      AActiveDefines: TStringList): Boolean;
  private
    class function CreatePackageDOM(AConfig: TProjectConfig; AUnits,
      AActiveDefines: TStringList): TXMLDocument;
    class function GetOutputPackageDirectory(AConfig: TProjectConfig): String;
    class function GetOutputUnitDirectory(AConfig: TProjectConfig; RelativePath: Boolean): String;
    class function CreateSearchPathNode(AConfig: TProjectConfig; Doc: TXMLDocument; AUnits, AActiveDefines: TStringList): TDOMElement;
    class function CreateFilesNode(AConfig: TProjectConfig; Doc: TXMLDocument; AUnits: TStringList): TDOMElement;
    class function CreateRequiredPackagesNode(AConfig: TProjectConfig; Doc: TXMLDocument): TDOMElement;

  end;

implementation

{ TLazarusPackageGenerator }

class function TLazarusPackageGenerator.GenerateLazarusPackage(
  AConfig: TProjectConfig; AActiveDefines: TStringList): Boolean;
var
  Units: TStringList;
  BasePath, PackageName, lPackageXMLFile: string;
  PackageXML: TXMLDocument;
  lExisting: TStringStream = nil;
  lNewFile: TStringStream;
begin
  Result := False;

  TUtils.LogInfo('Generating Lazarus package file for library...');

  // Discover all units
  BasePath := TUtils.NormalizePath(AConfig.BuildConfig.SourceDirectory);
  Units := DiscoverUnits(
    BasePath,
    AConfig.BuildConfig.UnitPaths,
    AConfig.BuildConfig.ManualUnitPaths,
    AActiveDefines,
    False
  );

  try
    if Units.Count = 0 then
    begin
      TUtils.LogWarning('No units found for Lazarus Package');
      Exit;
    end;

    PackageName := StringReplace(AConfig.Name, '-','_', [rfReplaceAll]);

    TUtils.LogInfo('Discovered ' + IntToStr(Units.Count) + ' units');

    // Write to file
    lPackageXMLFile := IncludeTrailingPathDelimiter(GetOutputPackageDirectory(AConfig))+PackageName+'.lpk';

    ForceDirectories(ExtractFilePath(lPackageXMLFile));

    try
      try

        PackageXML := CreatePackageDOM(AConfig, Units, AActiveDefines);
        lNewFile := TStringStream.Create;
        WriteXMLFile(PackageXML, lNewFile);
        while true do  // lets us use break
        begin
          if FileExists(lPackageXMLFile) then
          begin
            lExisting := TStringStream.Create;
            lExisting.LoadFromFile(lPackageXMLFile);
            // prevent overwriting an identical file
            if lNewFile.DataString = lExisting.DataString then
              Break; //
          end;
          lNewFile.SaveToFile(lPackageXMLFile);
          Break;
        end;

        TUtils.LogInfo('Package File (.lpk) Generated: ' + ChangeFileExt(lPackageXMLFile, '.lpk'));
      finally
        FreeAndNil(lNewFile);
        FreeAndNil(lExisting);
        FreeAndNil(PackageXML);
      end;

      Result := True;
    except
      on E: Exception do
      begin
        TUtils.LogError('Failed to write lazarus package files: ' + E.Message);
        Result := False;
      end;
    end;

  finally
    Units.Free;
  end;
end;

class function TLazarusPackageGenerator.CreatePackageDOM(
  AConfig: TProjectConfig; AUnits, AActiveDefines: TStringList): TXMLDocument;
var
  RootNode: TDOMNode;
  NameNode, PackageNode, CompilerOptionsNode, VersionNode,
  UsageOptionsNode, UnitPathNode,
  PublishOptionsNode, UseFileFiltersNode: TDOMElement;
begin
  Result := TXMLDocument.Create;
  RootNode := Result.AppendChild(Result.CreateElement('CONFIG'));
  PackageNode := RootNode.AppendChild(Result.CreateElement('Package')) as TDOMElement;
    PackageNode.AttribStrings['Version'] := '5';
    NameNode := PackageNode.AppendChild(Result.CreateElement('Name')) as TDOMElement;
      NameNode.AttribStrings['Value'] := UnicodeString(StringReplace(AConfig.Name, '-', '_', [rfReplaceAll]));

    CompilerOptionsNode := PackageNode.AppendChild(Result.CreateElement('CompilerOptions')) as TDOMElement;
      VersionNode := CompilerOptionsNode.AppendChild(Result.CreateElement('Version')) as TDOMElement;
        VersionNode.AttribStrings['Value'] := '11';

       // Collects paths of used units and adds the paths and include files
      CompilerOptionsNode.AppendChild(CreateSearchPathNode(AConfig, Result, AUnits, AActiveDefines));

    // Add File list
    {FilesNode := }PackageNode.AppendChild(CreateFilesNode(AConfig, Result, AUnits));
    // Add Dependancies
    {RequirePkgsNode := }PackageNode.AppendChild(CreateRequiredPackagesNode(AConfig, Result));

    UsageOptionsNode := PackageNode.AppendChild(Result.CreateElement('UsageOptions')) as TDOMElement;
      {IncludePathNode := UsageOptionsNode.AppendChild(Result.CreateElement('IncludePath')) as TDOMElement;
        IncludePathNode.Attributes['Value'] := } // this is for other libraries and projects to search. not for compilation
      UnitPathNode := UsageOptionsNode.AppendChild(Result.CreateElement('UnitPath')) as TDOMElement;
        UnitPathNode.AttribStrings['Value'] := '$(PkgOutDir)';

    PublishOptionsNode := PackageNode.AppendChild(Result.CreateElement('PublishOptions')) as TDOMElement;
      VersionNode := PublishOptionsNode.AppendChild(Result.CreateElement('Version')) as TDOMElement;
        VersionNode.AttribStrings['Value'] := '2';
      UseFileFiltersNode := PublishOptionsNode.AppendChild(Result.CreateElement('UseFileFilters')) as TDOMElement;
        UseFileFiltersNode.AttribStrings['Value'] := 'True';

    // the end

end;

class function TLazarusPackageGenerator.GetOutputPackageDirectory(
  AConfig: TProjectConfig): String;
var
  OutputDir: String;
begin
  OutputDir := TUtils.NormalizePath(AConfig.BuildConfig.OutputDirectory);
  Result := OutputDir + DirectorySeparator + 'lazpkg';
end;

class function TLazarusPackageGenerator.GetOutputUnitDirectory(
  AConfig: TProjectConfig; RelativePath: Boolean): String;
var
  OutputDir, PackageDir, lPrefix: String;
begin
  if RelativePath then
    lPrefix := GetCurrentDir
  else
    lPrefix := '';
  OutputDir := lPrefix + IncludeTrailingPathDelimiter(TUtils.NormalizePath(AConfig.BuildConfig.OutputDirectory));
  PackageDir := lPrefix + IncludeTrailingPathDelimiter(GetOutputPackageDirectory(AConfig));
  Result := OutputDir + 'units';
  if RelativePath then
    Result := ExtractRelativePath(PackageDir, Result);
end;

class function TLazarusPackageGenerator.CreateSearchPathNode(
  AConfig: TProjectConfig; Doc: TXMLDocument; AUnits,
  AActiveDefines: TStringList): TDOMElement;
var
  UnitOutputDirectoryNode, OtherUnitFilesNode, IncludeFilesNode: TDOMElement;
  lPaths, lIncludeFiles, lIncludePaths, IncludePaths: TStringList;
  lFile, lPackageDir, BasePath, lRelativePath: String;
  i: Integer;
begin
  // Result is <SearchPaths> node
  Result := Doc.CreateElement('SearchPaths');
  UnitOutputDirectoryNode := Result.AppendChild(Doc.CreateElement('UnitOutputDirectory')) as TDOMElement;
  UnitOutputDirectoryNode.AttribStrings['Value'] := UnicodeString(GetOutputUnitDirectory(AConfig, True));

  try
    lPaths := TStringList.Create;
    lPaths.Sorted:=True;
    lPaths.Duplicates:=TDuplicates.dupIgnore;
    lPaths.Delimiter:=';';
    lPackageDir := IncludeTrailingPathDelimiter(GetCurrentDir)
       + IncludeTrailingPathDelimiter(GetOutputPackageDirectory(AConfig));




    for lFile in AUnits do
    begin
      lPaths.Add(ExcludeTrailingPathDelimiter(ExtractRelativePath(lPackageDir, IncludeTrailingPathDelimiter(GetCurrentDir) + IncludeTrailingPathDelimiter(ExtractFileDir(lFile)))));
    end;
    try
      lIncludePaths := TStringList.Create;
      lIncludePaths.Sorted:=True;
      lIncludePaths.Duplicates:=TDuplicates.dupIgnore;
      lIncludePaths.Delimiter:=';';

      BasePath := TUtils.NormalizePath(AConfig.BuildConfig.SourceDirectory);


      IncludePaths := nil;

      if AConfig.BuildConfig.IncludePaths.Count > 0 then
          IncludePaths := TUtils.ScanForIncludePathsFiltered(
            BasePath,
            AConfig.BuildConfig.IncludePaths,
            AActiveDefines
          );

      if Assigned(IncludePaths) then
      begin
        for i := 0 to IncludePaths.Count-1 do
        begin

          lFile := IncludePaths[i];

          lRelativePath := ExtractRelativePath(
            lPackageDir,
            IncludeTrailingPathDelimiter(GetCurrentDir) + IncludeTrailingPathDelimiter(ExtractFileDir(lFile)));
          lIncludePaths.Add(ExcludeTrailingPathDelimiter(lRelativePath));
        end;

        // no need for duplicate paths if it's already on the other unit files
        for i := lIncludePaths.Count-1 downto 0 do
        begin
          if lPaths.Contains(lIncludePaths.Strings[i]) then
            lIncludePaths.Delete(i);
        end;

        if lIncludePaths.Count > 0 then
        begin
          IncludeFilesNode := Result.AppendChild(Doc.CreateElement('IncludeFiles')) as TDOMElement;
          IncludeFilesNode.AttribStrings['Value'] := UnicodeString(lIncludePaths.DelimitedText);
        end;
      end;   // if Assigned(IncludePaths);

      OtherUnitFilesNode := Result.AppendChild(Doc.CreateElement('OtherUnitFiles')) as TDOMElement;
      OtherUnitFilesNode.AttribStrings['Value'] := UnicodeString(lPaths.DelimitedText);

    finally
      lIncludePaths.Free;
      IncludePaths.Free;
    end;

  finally
    lPaths.Free;
  end;
end;

class function TLazarusPackageGenerator.CreateFilesNode(
  AConfig: TProjectConfig; Doc: TXMLDocument; AUnits: TStringList): TDOMElement;
var
  ItemNode, FileNode, UnitNameNode, TypeNode: TDOMElement;
  lPackagePath, lFile, lExt, lType, lPrefix, lUnitName: String;
begin
  lPackagePath := IncludeTrailingPathDelimiter(GetOutputPackageDirectory(AConfig));
  Result := Doc.CreateElement('Files') as TDOMElement;

  lPrefix := IncludeTrailingPathDelimiter(GetCurrentDir);

  for lFile in AUnits do
  begin
    lUnitName := ParseUnitName(lFile);
    if lUnitName = '' then
    begin
      TUtils.LogWarning('Skipped '+ lFile +  ' because unit name not found.');
      Continue;
    end;
    ItemNode := Result.AppendChild(Doc.CreateElement('Item')) as TDOMElement;
    FileNode := ItemNode.AppendChild(Doc.CreateElement('Filename')) as TDOMElement;
    FileNode.AttribStrings['Value'] := UnicodeString(ExtractRelativePath(lPrefix+lPackagePath, lPrefix+lFile));

    lExt := lowercase(ExtractFileExt(lFile));
    case lExt of
      '.pas', '.lpr', '.pp' : lType := '';
      '.inc', '.include': lType := 'Include';
      '.txt': lType := 'Text';
      '.md': lType := 'Text'; // Markdown?
    else
      lType := 'Binary'; // can also include text. the ide doesn't seem to care
    end;
    if lType = '' then // unit
    begin
      UnitNameNode := ItemNode.AppendChild(Doc.CreateElement('UnitName')) as TDOMElement;
      UnitNameNode.AttribStrings['Value'] := UnicodeString(lUnitName);
    end
    else begin
      TypeNode := ItemNode.AppendChild(Doc.CreateElement('Type')) as TDOMElement;
      TypeNode.AttribStrings['Value'] := UnicodeString(lType);
    end;

  end;

end;

class function TLazarusPackageGenerator.CreateRequiredPackagesNode(
  AConfig: TProjectConfig; Doc: TXMLDocument): TDOMElement;
var
  ItemNode, PackageNameNode: TDOMElement;
begin
  // This is basically empty but it should probably be expanded
  Result := Doc.CreateElement('RequiredPkgs') as TDOMElement;
  ItemNode := Result.AppendChild(Doc.CreateElement('Item')) as TDOMElement;
  PackageNameNode := ItemNode.AppendChild(Doc.CreateElement('PackageName')) as TDOMElement;
  PackageNameNode.AttribStrings['Value'] := 'FCL';
end;

end.
