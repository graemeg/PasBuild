{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.ModuleDiscovery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  PasBuild.Types,
  PasBuild.Config;

type
  { Module discovery and resolution }
  TModuleDiscoverer = class
  private
    class function ResolvePath(const ABaseDir, ARelativePath: string): string;
    class function IsPathWithinTree(const APath, ATreeRoot: string): Boolean;
  public
    class function DiscoverModules(const AAggregatorPath: string): TModuleRegistry;
  end;

implementation

{ TModuleDiscoverer }

class function TModuleDiscoverer.ResolvePath(const ABaseDir, ARelativePath: string): string;
var
  NormalizedPath: string;
begin
  { Normalize path separators to platform-specific }
  NormalizedPath := StringReplace(ARelativePath, '/', PathDelim, [rfReplaceAll]);

  { Expand relative path from base directory }
  Result := ExpandFileName(IncludeTrailingPathDelimiter(ABaseDir) + NormalizedPath);

  { Ensure we have a normalized path }
  Result := ExpandFileName(Result);
end;

class function TModuleDiscoverer.IsPathWithinTree(const APath, ATreeRoot: string): Boolean;
var
  NormalizedPath, NormalizedRoot: string;
begin
  { Normalize both paths for comparison }
  NormalizedPath := ExpandFileName(APath);
  NormalizedRoot := IncludeTrailingPathDelimiter(ExpandFileName(ATreeRoot));

  { Check if path starts with root }
  Result := Pos(NormalizedRoot, NormalizedPath) = 1;
end;

class function TModuleDiscoverer.DiscoverModules(const AAggregatorPath: string): TModuleRegistry;
var
  AggregatorConfig: TProjectConfig;
  AggregatorDir: string;
  I: Integer;
  ModulePath: string;
  AbsoluteModulePath: string;
  ModuleProjectXml: string;
  ModuleConfig: TProjectConfig;
  ModuleInfo: TModuleInfo;
  J: Integer;
  DependencyPath: string;
  AbsoluteDependencyPath: string;
  DependencyModule: TModuleInfo;
  DependencyModuleInfo: TModuleInfo;
begin
  Result := TModuleRegistry.Create;

  try
    { Load aggregator project.xml }
    AggregatorConfig := TConfigLoader.LoadProjectXML(AAggregatorPath);
    try
      { Validate it's an aggregator }
      if AggregatorConfig.BuildConfig.ProjectType <> ptPom then
        raise Exception.Create('Aggregator project must have packaging=pom');

      { Get aggregator directory }
      AggregatorDir := ExtractFilePath(AAggregatorPath);
      if AggregatorDir = '' then
        AggregatorDir := GetCurrentDir;
      AggregatorDir := ExpandFileName(AggregatorDir);

      { First pass: Load all modules }
      for I := 0 to AggregatorConfig.Modules.Count - 1 do
      begin
        ModulePath := AggregatorConfig.Modules[I];
        AbsoluteModulePath := ResolvePath(AggregatorDir, ModulePath);

        { Validate path is within project tree }
        if not IsPathWithinTree(AbsoluteModulePath, AggregatorDir) then
          raise Exception.CreateFmt('Module path outside project tree: %s', [ModulePath]);

        { Check if project.xml exists }
        ModuleProjectXml := IncludeTrailingPathDelimiter(AbsoluteModulePath) + 'project.xml';
        if not FileExists(ModuleProjectXml) then
          raise Exception.CreateFmt('Module project.xml not found: %s', [ModuleProjectXml]);

        { Load module configuration }
        ModuleConfig := TConfigLoader.LoadProjectXML(ModuleProjectXml);

        { Create module info }
        ModuleInfo := TModuleInfo.Create;
        ModuleInfo.Name := ModuleConfig.Name;
        ModuleInfo.Path := AbsoluteModulePath;
        ModuleInfo.FConfig := ModuleConfig;  { Assign config (owned by ModuleInfo) }
        ModuleInfo.UnitsDirectory := IncludeTrailingPathDelimiter(AbsoluteModulePath) + 'target' + PathDelim + 'units';

        { Register in registry }
        Result.RegisterModule(ModuleInfo);
      end;

      { Second pass: Resolve dependencies }
      for I := 0 to Result.Modules.Count - 1 do
      begin
        ModuleInfo := TModuleInfo(Result.Modules[I]);

        { Process module dependencies }
        for J := 0 to ModuleInfo.Config.ModuleDependencies.Count - 1 do
        begin
          DependencyPath := ModuleInfo.Config.ModuleDependencies[J];

          { Resolve relative to module's directory }
          AbsoluteDependencyPath := ResolvePath(ModuleInfo.Path, DependencyPath);

          { Validate path is within project tree }
          if not IsPathWithinTree(AbsoluteDependencyPath, AggregatorDir) then
            raise Exception.CreateFmt('Dependency path outside project tree: %s', [DependencyPath]);

          { Find module by path }
          DependencyModuleInfo := Result.FindModuleByPath(AbsoluteDependencyPath);
          if DependencyModuleInfo = nil then
            raise Exception.CreateFmt('Dependency module not found: %s (resolved to %s)', [DependencyPath, AbsoluteDependencyPath]);

          { Add dependency name to module's dependencies }
          if ModuleInfo.Dependencies.IndexOf(DependencyModuleInfo.Name) < 0 then
            ModuleInfo.Dependencies.Add(DependencyModuleInfo.Name);
        end;
      end;

    finally
      AggregatorConfig.Free;
    end;

  except
    Result.Free;
    raise;
  end;
end;

end.
