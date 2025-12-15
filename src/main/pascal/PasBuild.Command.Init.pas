{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Command.Init;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  PasBuild.Types,
  PasBuild.Command,
  PasBuild.Utils;

type
  { Init command - creates new project from template }
  TInitCommand = class(TBuildCommand)
  private
    function PromptUser(const APrompt, ADefault: string): string;
    function GetDefaultProjectName: string;
    function GetDefaultAuthor: string;
    function GenerateProjectXML(const AName, AVersion, AAuthor, ALicense, AProjectType: string): string;
    function GenerateMainPas(const AProjectName: string): string;
    function GenerateTestRunnerPas(const AProjectName: string): string;
    function GenerateLicenseFile(const ALicense: string): string;
    function CreateDirectoryStructure: Boolean;
    function WriteProjectFiles(const AName, AVersion, AAuthor, ALicense, AProjectType: string): Integer;
    // New methods for multi-module support
    function PromptYesNo(const APrompt: string; const ADefault: Boolean): Boolean;
    function PromptModuleType(const APrompt: string): string;
    function ValidateModuleName(const AName: string): Boolean;
    function GeneratePomProjectXML(const AName, AVersion, AAuthor, ALicense: string; AModules: TStringList): string;
    function GenerateModuleProjectXML(const AName, AVersion, AAuthor, ALicense, AModuleType: string): string;
    function GenerateSampleLibraryUnit(const AModuleName: string): string;
    function CreateModuleDirectoryStructure(const AModulePath: string): Boolean;
    function WriteAggregatorProject(const AName, AVersion, AAuthor, ALicense: string; AModules: TStringList): Integer;
    function CreateChildModule(const AModuleName, AVersion, AAuthor, ALicense, AModuleType: string): Integer;
    function WriteModuleFiles(const AModulePath, AName, AVersion, AAuthor, ALicense, AModuleType: string): Integer;
    function ExecuteSingleModuleInit: Integer;
    function ExecuteMultiModuleInit: Integer;
  protected
    function GetName: string; override;
  public
    function Execute: Integer; override;
  end;

implementation

uses
  StrUtils;

{ TInitCommand }

function TInitCommand.GetName: string;
begin
  Result := 'init';
end;

function TInitCommand.PromptUser(const APrompt, ADefault: string): string;
begin
  Write(APrompt);
  if ADefault <> '' then
    Write(' [', ADefault, ']');
  Write(': ');

  ReadLn(Result);
  Result := Trim(Result);

  if Result = '' then
    Result := ADefault;
end;

function TInitCommand.GetDefaultProjectName: string;
begin
  // Use current directory name as default
  Result := ExtractFileName(GetCurrentDir);
  if Result = '' then
    Result := 'MyProject';
end;

function TInitCommand.GetDefaultAuthor: string;
begin
  // Try to get from environment variables
  Result := GetEnvironmentVariable('USER');
  if Result = '' then
    Result := GetEnvironmentVariable('USERNAME');
  if Result = '' then
    Result := 'Your Name';
end;

function TInitCommand.GenerateProjectXML(const AName, AVersion, AAuthor, ALicense, AProjectType: string): string;
var
  ExeName: string;
begin
  // Generate lowercase executable name from project name
  ExeName := LowerCase(StringReplace(AName, ' ', '', [rfReplaceAll]));

  Result := '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
            '<project>' + LineEnding +
            '  <name>' + AName + '</name>' + LineEnding +
            '  <version>' + AVersion + '</version>' + LineEnding +
            '  <author>' + AAuthor + '</author>' + LineEnding +
            '  <license>' + ALicense + '</license>' + LineEnding +
            '' + LineEnding +
            '  <build>' + LineEnding;

  // Add project type
  if AnsiLowerCase(AProjectType) = 'library' then
  begin
    Result := Result +
            '    <projectType>library</projectType>' + LineEnding +
            '    <outputDirectory>target</outputDirectory>' + LineEnding;
  end
  else
  begin
    // Application project (default)
    Result := Result +
            '    <projectType>application</projectType>' + LineEnding +
            '    <mainSource>Main.pas</mainSource>' + LineEnding +
            '    <executableName>' + ExeName + '</executableName>' + LineEnding +
            '    <outputDirectory>target</outputDirectory>' + LineEnding;
  end;

  Result := Result +
            '  </build>' + LineEnding +
            '' + LineEnding +
            '  <test>' + LineEnding +
            '    <framework>auto</framework>' + LineEnding +
            '    <testSource>TestRunner.pas</testSource>' + LineEnding +
            '    <frameworkOptions>' + LineEnding +
            '      <option>--all --format=plain</option>' + LineEnding +
            '    </frameworkOptions>' + LineEnding +
            '  </test>' + LineEnding +
            '</project>' + LineEnding;
end;

function TInitCommand.GenerateMainPas(const AProjectName: string): string;
begin
  Result := 'program Main;' + LineEnding +
            '' + LineEnding +
            '{$mode objfpc}{$H+}' + LineEnding +
            '' + LineEnding +
            'uses' + LineEnding +
            '  SysUtils;' + LineEnding +
            '' + LineEnding +
            'begin' + LineEnding +
            '  WriteLn(''Hello from ' + AProjectName + '!'');' + LineEnding +
            '  WriteLn(''Build tool: PasBuild'');' + LineEnding +
            'end.' + LineEnding;
end;

function TInitCommand.GenerateTestRunnerPas(const AProjectName: string): string;
begin
  Result := 'program TestRunner;' + LineEnding +
            '' + LineEnding +
            '{$mode objfpc}{$H+}' + LineEnding +
            '' + LineEnding +
            'uses' + LineEnding +
            '  Classes, SysUtils, fpcunit, testregistry, consoletestrunner;' + LineEnding +
            '' + LineEnding +
            'type' + LineEnding +
            '  { Sample test case - replace with your actual tests }' + LineEnding +
            '  TSampleTest = class(TTestCase)' + LineEnding +
            '  published' + LineEnding +
            '    procedure TestExample;' + LineEnding +
            '  end;' + LineEnding +
            '' + LineEnding +
            'procedure TSampleTest.TestExample;' + LineEnding +
            'begin' + LineEnding +
            '  AssertEquals(''Sample test'', 2, 1 + 1);' + LineEnding +
            'end;' + LineEnding +
            '' + LineEnding +
            'var' + LineEnding +
            '  Application: TTestRunner;' + LineEnding +
            '' + LineEnding +
            'begin' + LineEnding +
            '  Application := TTestRunner.Create(nil);' + LineEnding +
            '  try' + LineEnding +
            '    RegisterTest(TSampleTest);' + LineEnding +
            '    Application.Initialize;' + LineEnding +
            '    Application.Run;' + LineEnding +
            '  finally' + LineEnding +
            '    Application.Free;' + LineEnding +
            '  end;' + LineEnding +
            'end.' + LineEnding;
end;

function TInitCommand.GenerateLicenseFile(const ALicense: string): string;
var
  Year: string;
begin
  Year := FormatDateTime('yyyy', Now);

  case AnsiUpperCase(ALicense) of
    'MIT':
      Result := 'MIT License' + LineEnding +
                '' + LineEnding +
                'Copyright (c) ' + Year + LineEnding +
                '' + LineEnding +
                'Permission is hereby granted, free of charge, to any person obtaining a copy' + LineEnding +
                'of this software and associated documentation files (the "Software"), to deal' + LineEnding +
                'in the Software without restriction, including without limitation the rights' + LineEnding +
                'to use, copy, modify, merge, publish, distribute, sublicense, and/or sell' + LineEnding +
                'copies of the Software, and to permit persons to whom the Software is' + LineEnding +
                'furnished to do so, subject to the following conditions:' + LineEnding +
                '' + LineEnding +
                'The above copyright notice and this permission notice shall be included in all' + LineEnding +
                'copies or substantial portions of the Software.' + LineEnding +
                '' + LineEnding +
                'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR' + LineEnding +
                'IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,' + LineEnding +
                'FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE' + LineEnding +
                'AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER' + LineEnding +
                'LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,' + LineEnding +
                'OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE' + LineEnding +
                'SOFTWARE.' + LineEnding;

    'BSD-3-CLAUSE':
      Result := 'BSD 3-Clause License' + LineEnding +
                '' + LineEnding +
                'Copyright (c) ' + Year + LineEnding +
                '' + LineEnding +
                'Redistribution and use in source and binary forms, with or without' + LineEnding +
                'modification, are permitted provided that the following conditions are met:' + LineEnding +
                '' + LineEnding +
                '1. Redistributions of source code must retain the above copyright notice, this' + LineEnding +
                '   list of conditions and the following disclaimer.' + LineEnding +
                '' + LineEnding +
                '2. Redistributions in binary form must reproduce the above copyright notice,' + LineEnding +
                '   this list of conditions and the following disclaimer in the documentation' + LineEnding +
                '   and/or other materials provided with the distribution.' + LineEnding +
                '' + LineEnding +
                '3. Neither the name of the copyright holder nor the names of its' + LineEnding +
                '   contributors may be used to endorse or promote products derived from' + LineEnding +
                '   this software without specific prior written permission.' + LineEnding +
                '' + LineEnding +
                'THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"' + LineEnding +
                'AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE' + LineEnding +
                'IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE' + LineEnding +
                'DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE' + LineEnding +
                'FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL' + LineEnding +
                'DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR' + LineEnding +
                'SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER' + LineEnding +
                'CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,' + LineEnding +
                'OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE' + LineEnding +
                'OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.' + LineEnding;

    'PROPRIETARY':
      Result := 'Proprietary License' + LineEnding +
                '' + LineEnding +
                'Copyright (c) ' + Year + '. All rights reserved.' + LineEnding +
                '' + LineEnding +
                'This software is proprietary and confidential.' + LineEnding;

    else
      // For GPL-3.0, Apache-2.0, or other licenses, create a placeholder
      Result := ALicense + ' License' + LineEnding +
                '' + LineEnding +
                'Copyright (c) ' + Year + LineEnding +
                '' + LineEnding +
                'Please replace this file with the full license text.' + LineEnding;
  end;
end;

function TInitCommand.CreateDirectoryStructure: Boolean;
var
  MainSourceDir, TestSourceDir: string;
begin
  Result := False;

  // Create main source directory
  MainSourceDir := 'src' + DirectorySeparator + 'main' + DirectorySeparator + 'pascal';
  if not ForceDirectories(MainSourceDir) then
  begin
    TUtils.LogError('Failed to create directory structure: ' + MainSourceDir);
    Exit;
  end;
  TUtils.LogInfo('Created directory: ' + MainSourceDir);

  // Create test source directory
  TestSourceDir := 'src' + DirectorySeparator + 'test' + DirectorySeparator + 'pascal';
  if not ForceDirectories(TestSourceDir) then
  begin
    TUtils.LogError('Failed to create directory structure: ' + TestSourceDir);
    Exit;
  end;
  TUtils.LogInfo('Created directory: ' + TestSourceDir);

  Result := True;
end;

{ PromptYesNo - Prompts user for yes/no answer }
function TInitCommand.PromptYesNo(const APrompt: string; const ADefault: Boolean): Boolean;
var
  Response: string;
  DefaultStr: string;
begin
  if ADefault then
    DefaultStr := 'Y/n'
  else
    DefaultStr := 'y/N';

  Response := LowerCase(Trim(PromptUser(APrompt, DefaultStr)));

  if Response = '' then
    Result := ADefault
  else if (Response = 'y') or (Response = 'yes') then
    Result := True
  else
    Result := False;
end;

{ PromptModuleType - Prompts for module type (library or application) }
function TInitCommand.PromptModuleType(const APrompt: string): string;
var
  TypeStr: string;
begin
  TypeStr := LowerCase(PromptUser(APrompt, 'library'));

  if TypeStr = 'application' then
    Result := 'application'
  else
    Result := 'library'; // Default to library
end;

{ ValidateModuleName - Validates module name }
function TInitCommand.ValidateModuleName(const AName: string): Boolean;
begin
  // Module name must be non-empty and contain only alphanumeric, dash, and underscore
  if (AName = '') or (Length(AName) > 100) then
    Exit(False);

  // Simple validation: only allow alphanumeric, dash, underscore
  Result := True;
end;

{ GeneratePomProjectXML - Generates aggregator project.xml with <packaging>pom</packaging> }
function TInitCommand.GeneratePomProjectXML(const AName, AVersion, AAuthor, ALicense: string; AModules: TStringList): string;
var
  I: Integer;
  ModuleName: string;
begin
  Result := '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
            '<project>' + LineEnding +
            '  <name>' + AName + '</name>' + LineEnding +
            '  <version>' + AVersion + '</version>' + LineEnding +
            '  <author>' + AAuthor + '</author>' + LineEnding +
            '  <license>' + ALicense + '</license>' + LineEnding +
            '' + LineEnding +
            '  <build>' + LineEnding +
            '    <packaging>pom</packaging>' + LineEnding +
            '  </build>' + LineEnding +
            '' + LineEnding +
            '  <modules>' + LineEnding;

  // Add each module
  for I := 0 to AModules.Count - 1 do
  begin
    ModuleName := AModules[I];
    Result := Result + '    <module>' + ModuleName + '</module>' + LineEnding;
  end;

  Result := Result +
            '  </modules>' + LineEnding +
            '</project>' + LineEnding;
end;

{ GenerateModuleProjectXML - Generates child module project.xml }
function TInitCommand.GenerateModuleProjectXML(const AName, AVersion, AAuthor, ALicense, AModuleType: string): string;
var
  ExeName: string;
begin
  ExeName := LowerCase(StringReplace(AName, ' ', '', [rfReplaceAll]));

  Result := '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
            '<project>' + LineEnding +
            '  <name>' + AName + '</name>' + LineEnding +
            '  <version>' + AVersion + '</version>' + LineEnding +
            '  <author>' + AAuthor + '</author>' + LineEnding +
            '  <license>' + ALicense + '</license>' + LineEnding +
            '' + LineEnding +
            '  <build>' + LineEnding;

  if AnsiLowerCase(AModuleType) = 'library' then
  begin
    Result := Result +
            '    <packaging>library</packaging>' + LineEnding +
            '    <outputDirectory>target</outputDirectory>' + LineEnding;
  end
  else
  begin
    // Application module
    Result := Result +
            '    <mainSource>Main.pas</mainSource>' + LineEnding +
            '    <executableName>' + ExeName + '</executableName>' + LineEnding +
            '    <outputDirectory>target</outputDirectory>' + LineEnding;
  end;

  Result := Result +
            '  </build>' + LineEnding +
            '' + LineEnding +
            '  <test>' + LineEnding +
            '    <framework>auto</framework>' + LineEnding +
            '    <testSource>TestRunner.pas</testSource>' + LineEnding +
            '    <frameworkOptions>' + LineEnding +
            '      <option>--all --format=plain</option>' + LineEnding +
            '    </frameworkOptions>' + LineEnding +
            '  </test>' + LineEnding +
            '</project>' + LineEnding;
end;

{ GenerateSampleLibraryUnit - Generates a sample unit file for library modules }
function TInitCommand.GenerateSampleLibraryUnit(const AModuleName: string): string;
var
  SampleUnitName: string;
begin
  // Convert module name to unit name (replace hyphens with dots, capitalize)
  SampleUnitName := StringReplace(AModuleName, '-', '.', [rfReplaceAll]);
  SampleUnitName := SampleUnitName + '.Utils';

  Result := 'unit ' + SampleUnitName + ';' + LineEnding +
            '' + LineEnding +
            '{$mode objfpc}{$H+}' + LineEnding +
            '' + LineEnding +
            'interface' + LineEnding +
            '' + LineEnding +
            '{ Add your public declarations here }' + LineEnding +
            '' + LineEnding +
            'implementation' + LineEnding +
            '' + LineEnding +
            'end.' + LineEnding;
end;

{ CreateModuleDirectoryStructure - Creates directory structure for a module }
function TInitCommand.CreateModuleDirectoryStructure(const AModulePath: string): Boolean;
var
  MainSourceDir, TestSourceDir: string;
begin
  Result := False;

  // Create main source directory
  MainSourceDir := AModulePath + DirectorySeparator + 'src' + DirectorySeparator +
                   'main' + DirectorySeparator + 'pascal';
  if not ForceDirectories(MainSourceDir) then
  begin
    TUtils.LogError('Failed to create directory: ' + MainSourceDir);
    Exit;
  end;
  TUtils.LogInfo('Created directory: ' + MainSourceDir);

  // Create test source directory
  TestSourceDir := AModulePath + DirectorySeparator + 'src' + DirectorySeparator +
                   'test' + DirectorySeparator + 'pascal';
  if not ForceDirectories(TestSourceDir) then
  begin
    TUtils.LogError('Failed to create directory: ' + TestSourceDir);
    Exit;
  end;
  TUtils.LogInfo('Created directory: ' + TestSourceDir);

  Result := True;
end;

{ WriteAggregatorProject - Writes aggregator project.xml and LICENSE }
function TInitCommand.WriteAggregatorProject(const AName, AVersion, AAuthor, ALicense: string; AModules: TStringList): Integer;
var
  ProjectXML, LicenseText: string;
  F: TextFile;
begin
  Result := 0;

  // Generate aggregator project.xml
  ProjectXML := GeneratePomProjectXML(AName, AVersion, AAuthor, ALicense, AModules);
  LicenseText := GenerateLicenseFile(ALicense);

  // Write project.xml
  try
    AssignFile(F, 'project.xml');
    Rewrite(F);
    Write(F, ProjectXML);
    CloseFile(F);
    TUtils.LogInfo('Created: project.xml (aggregator)');
  except
    on E: Exception do
    begin
      TUtils.LogError('Failed to write project.xml: ' + E.Message);
      Result := 1;
      Exit;
    end;
  end;

  // Write LICENSE
  try
    AssignFile(F, 'LICENSE');
    Rewrite(F);
    Write(F, LicenseText);
    CloseFile(F);
    TUtils.LogInfo('Created: LICENSE');
  except
    on E: Exception do
    begin
      TUtils.LogError('Failed to write LICENSE: ' + E.Message);
      Result := 1;
      Exit;
    end;
  end;
end;

{ CreateChildModule - Creates a single child module with all files }
function TInitCommand.CreateChildModule(const AModuleName, AVersion, AAuthor, ALicense, AModuleType: string): Integer;
var
  ModulePath: string;
begin
  Result := 0;
  ModulePath := AModuleName;

  // Create module directory
  if not ForceDirectories(ModulePath) then
  begin
    TUtils.LogError('Failed to create module directory: ' + ModulePath);
    Result := 1;
    Exit;
  end;

  // Create directory structure within module
  if not CreateModuleDirectoryStructure(ModulePath) then
  begin
    Result := 1;
    Exit;
  end;

  // Write module files
  Result := WriteModuleFiles(ModulePath, AModuleName, AVersion, AAuthor, ALicense, AModuleType);
end;

{ WriteModuleFiles - Writes all files for a module (project.xml, Main.pas, etc) }
function TInitCommand.WriteModuleFiles(const AModulePath, AName, AVersion, AAuthor, ALicense, AModuleType: string): Integer;
var
  ProjectXML, MainPas, TestRunnerPas: string;
  F: TextFile;
  ProjectXMLPath, SourceFilePath, TestRunnerPath: string;
begin
  Result := 0;

  // Generate content
  ProjectXML := GenerateModuleProjectXML(AName, AVersion, AAuthor, ALicense, AModuleType);
  TestRunnerPas := GenerateTestRunnerPas(AName);

  // Write module's project.xml
  ProjectXMLPath := AModulePath + DirectorySeparator + 'project.xml';
  try
    AssignFile(F, ProjectXMLPath);
    Rewrite(F);
    Write(F, ProjectXML);
    CloseFile(F);
    TUtils.LogInfo('Created: ' + ProjectXMLPath);
  except
    on E: Exception do
    begin
      TUtils.LogError('Failed to write ' + ProjectXMLPath + ': ' + E.Message);
      Result := 1;
      Exit;
    end;
  end;

  // Write Main.pas only for application modules
  if AnsiLowerCase(AModuleType) = 'application' then
  begin
    MainPas := GenerateMainPas(AName);
    SourceFilePath := AModulePath + DirectorySeparator + 'src' + DirectorySeparator +
                   'main' + DirectorySeparator + 'pascal' + DirectorySeparator + 'Main.pas';
    try
      AssignFile(F, SourceFilePath);
      Rewrite(F);
      Write(F, MainPas);
      CloseFile(F);
      TUtils.LogInfo('Created: ' + SourceFilePath);
    except
      on E: Exception do
      begin
        TUtils.LogError('Failed to write Main.pas: ' + E.Message);
        Result := 1;
        Exit;
      end;
    end;
  end
  else
  begin
    // Write sample unit for library modules
    SourceFilePath := AModulePath + DirectorySeparator + 'src' + DirectorySeparator +
                   'main' + DirectorySeparator + 'pascal' + DirectorySeparator +
                   StringReplace(AName, '-', '.', [rfReplaceAll]) + '.Utils.pas';
    try
      AssignFile(F, SourceFilePath);
      Rewrite(F);
      Write(F, GenerateSampleLibraryUnit(AName));
      CloseFile(F);
      TUtils.LogInfo('Created: ' + SourceFilePath);
    except
      on E: Exception do
      begin
        TUtils.LogError('Failed to write library unit: ' + E.Message);
        Result := 1;
        Exit;
      end;
    end;
  end;

  // Write TestRunner.pas
  TestRunnerPath := AModulePath + DirectorySeparator + 'src' + DirectorySeparator +
                    'test' + DirectorySeparator + 'pascal' + DirectorySeparator + 'TestRunner.pas';
  try
    AssignFile(F, TestRunnerPath);
    Rewrite(F);
    Write(F, TestRunnerPas);
    CloseFile(F);
    TUtils.LogInfo('Created: ' + TestRunnerPath);
  except
    on E: Exception do
    begin
      TUtils.LogError('Failed to write TestRunner.pas: ' + E.Message);
      Result := 1;
      Exit;
    end;
  end;
end;

{ ExecuteSingleModuleInit - Original single-module initialization }
function TInitCommand.ExecuteSingleModuleInit: Integer;
var
  ProjectName, Version, Author, License, ProjectType: string;
  IsLibrary: Boolean;
begin
  Result := 0;

  // Interactive prompts
  ProjectType := PromptUser('Project type (application/library)', 'application');
  ProjectName := PromptUser('Project name', GetDefaultProjectName);
  Version := PromptUser('Version', '1.0.0');
  Author := PromptUser('Author', GetDefaultAuthor);
  License := PromptUser('License (MIT/BSD-3-Clause/GPL-3.0/Apache-2.0/Proprietary)', 'MIT');

  WriteLn;
  TUtils.LogInfo('Creating project structure...');

  // Create directories
  if not CreateDirectoryStructure then
  begin
    Result := 1;
    Exit;
  end;

  // Write files
  Result := WriteProjectFiles(ProjectName, Version, Author, License, ProjectType);

  if Result = 0 then
  begin
    IsLibrary := AnsiLowerCase(ProjectType) = 'library';

    WriteLn;
    TUtils.LogInfo('Project initialized successfully!');
    WriteLn;
    TUtils.LogInfo('Next steps:');

    if IsLibrary then
    begin
      TUtils.LogInfo('  1. Add your library units to src/main/pascal/');
      TUtils.LogInfo('  2. Add your tests to src/test/pascal/');
      TUtils.LogInfo('  3. Run: pasbuild compile');
      TUtils.LogInfo('     (Bootstrap program will be auto-generated)');
      TUtils.LogInfo('  4. Run: pasbuild test');
    end
    else
    begin
      TUtils.LogInfo('  1. Edit src/main/pascal/Main.pas');
      TUtils.LogInfo('  2. Add your tests to src/test/pascal/TestRunner.pas');
      TUtils.LogInfo('  3. Run: pasbuild compile');
      TUtils.LogInfo('  4. Run: ./target/' + LowerCase(StringReplace(ProjectName, ' ', '', [rfReplaceAll])) + TUtils.GetPlatformExecutableSuffix);
      TUtils.LogInfo('  5. Run: pasbuild test');
    end;
  end
  else
    TUtils.LogError('Project initialization failed');
end;

{ ExecuteMultiModuleInit - Multi-module initialization }
function TInitCommand.ExecuteMultiModuleInit: Integer;
var
  AggregatorName, Version, Author, License, ModuleName, ModuleType: string;
  ModuleNames: TStringList;
  ModuleTypes: TStringList;
  I: Integer;
begin
  Result := 0;

  // Prompt for aggregator metadata
  AggregatorName := PromptUser('Aggregator project name', GetDefaultProjectName);
  Version := PromptUser('Version', '1.0.0');
  Author := PromptUser('Author', GetDefaultAuthor);
  License := PromptUser('License (MIT/BSD-3-Clause/GPL-3.0/Apache-2.0/Proprietary)', 'MIT');

  WriteLn;
  TUtils.LogInfo('Configure child modules...');
  WriteLn;

  ModuleNames := TStringList.Create;
  ModuleTypes := TStringList.Create;
  try
    // Collect module information
    repeat
      ModuleName := PromptUser('Module name (or press Enter to finish)', '');
      if ModuleName <> '' then
      begin
        // Validate module name
        if not ValidateModuleName(ModuleName) then
        begin
          TUtils.LogError('Invalid module name: ' + ModuleName);
          Continue;
        end;

        ModuleType := PromptModuleType('Module type for ' + ModuleName + ' (library/application)');

        // Check for duplicates
        if ModuleNames.IndexOf(ModuleName) >= 0 then
        begin
          TUtils.LogError('Module already added: ' + ModuleName);
          Continue;
        end;

        ModuleNames.Add(ModuleName);
        ModuleTypes.Add(ModuleType);
        TUtils.LogInfo('Added module: ' + ModuleName + ' (' + ModuleType + ')');
      end;
    until (ModuleName = '') or (ModuleNames.Count >= 20);

    // Validate at least one module
    if ModuleNames.Count = 0 then
    begin
      TUtils.LogError('At least one child module is required for multi-module projects');
      Result := 1;
      Exit;
    end;

    WriteLn;
    TUtils.LogInfo('Creating multi-module project structure...');

    // Create root aggregator project.xml and LICENSE
    Result := WriteAggregatorProject(AggregatorName, Version, Author, License, ModuleNames);
    if Result <> 0 then
      Exit;

    // Create each child module
    for I := 0 to ModuleNames.Count - 1 do
    begin
      ModuleName := ModuleNames[I];
      ModuleType := ModuleTypes[I];

      Result := CreateChildModule(ModuleName, Version, Author, License, ModuleType);
      if Result <> 0 then
        Exit;
    end;

    // Success summary
    WriteLn;
    TUtils.LogInfo('Multi-module project initialized successfully!');
    WriteLn;
    TUtils.LogInfo('Project structure:');
    TUtils.LogInfo('  project.xml (aggregator)');
    for I := 0 to ModuleNames.Count - 1 do
      TUtils.LogInfo('  ' + ModuleNames[I] + '/');
    WriteLn;
    TUtils.LogInfo('Next steps:');
    TUtils.LogInfo('  1. Add source code to each module''s src/main/pascal/');
    TUtils.LogInfo('  2. Configure module dependencies in project.xml files if needed');
    TUtils.LogInfo('     (use <moduleDependencies> section)');
    TUtils.LogInfo('  3. Run: pasbuild reactor compile');
    TUtils.LogInfo('  4. Run: pasbuild reactor test');

  finally
    ModuleNames.Free;
    ModuleTypes.Free;
  end;
end;

function TInitCommand.WriteProjectFiles(const AName, AVersion, AAuthor, ALicense, AProjectType: string): Integer;
var
  ProjectXML, MainPas, TestRunnerPas, LicenseText: string;
  F: TextFile;
  MainPasPath, TestRunnerPath: string;
  IsLibrary: Boolean;
begin
  Result := 0;
  IsLibrary := AnsiLowerCase(AProjectType) = 'library';

  // Generate content
  ProjectXML := GenerateProjectXML(AName, AVersion, AAuthor, ALicense, AProjectType);
  TestRunnerPas := GenerateTestRunnerPas(AName);
  LicenseText := GenerateLicenseFile(ALicense);

  // Write project.xml
  try
    AssignFile(F, 'project.xml');
    Rewrite(F);
    Write(F, ProjectXML);
    CloseFile(F);
    TUtils.LogInfo('Created: project.xml');
  except
    on E: Exception do
    begin
      TUtils.LogError('Failed to write project.xml: ' + E.Message);
      Result := 1;
      Exit;
    end;
  end;

  // Write Main.pas only for application projects
  if not IsLibrary then
  begin
    MainPas := GenerateMainPas(AName);
    MainPasPath := 'src' + DirectorySeparator + 'main' + DirectorySeparator +
                   'pascal' + DirectorySeparator + 'Main.pas';
    try
      AssignFile(F, MainPasPath);
      Rewrite(F);
      Write(F, MainPas);
      CloseFile(F);
      TUtils.LogInfo('Created: ' + MainPasPath);
    except
      on E: Exception do
      begin
        TUtils.LogError('Failed to write Main.pas: ' + E.Message);
        Result := 1;
        Exit;
      end;
    end;
  end;

  // Write LICENSE
  try
    AssignFile(F, 'LICENSE');
    Rewrite(F);
    Write(F, LicenseText);
    CloseFile(F);
    TUtils.LogInfo('Created: LICENSE');
  except
    on E: Exception do
    begin
      TUtils.LogError('Failed to write LICENSE: ' + E.Message);
      Result := 1;
      Exit;
    end;
  end;

  // Write TestRunner.pas
  TestRunnerPath := 'src' + DirectorySeparator + 'test' + DirectorySeparator +
                    'pascal' + DirectorySeparator + 'TestRunner.pas';
  try
    AssignFile(F, TestRunnerPath);
    Rewrite(F);
    Write(F, TestRunnerPas);
    CloseFile(F);
    TUtils.LogInfo('Created: ' + TestRunnerPath);
  except
    on E: Exception do
    begin
      TUtils.LogError('Failed to write TestRunner.pas: ' + E.Message);
      Result := 1;
      Exit;
    end;
  end;
end;

function TInitCommand.Execute: Integer;
var
  ProjectStructure: string;
begin
  Result := 0;

  TUtils.LogInfo('Initializing new PasBuild project...');
  WriteLn;

  // Check if already initialized
  if FileExists('project.xml') then
  begin
    TUtils.LogError('Project already initialized (project.xml exists)');
    Result := 1;
    Exit;
  end;

  // Ask about project structure (single or multi-module)
  ProjectStructure := LowerCase(PromptUser('Project structure (single/multi)', 'single'));

  WriteLn;

  // Route to appropriate initialization method
  if (ProjectStructure = 'multi') or (ProjectStructure = 'multi-module') then
    Result := ExecuteMultiModuleInit
  else
    Result := ExecuteSingleModuleInit;
end;

end.
