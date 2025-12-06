{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

program TestConfigLoader;

{$mode objfpc}{$H+}

uses
  SysUtils,
  PasBuild.Types,
  PasBuild.Config;

var
  Config: TProjectConfig;
  Profile: TProfile;
  Define, Option: string;

begin
  WriteLn('Testing ConfigLoader with project.xml...');
  WriteLn;

  try
    // Load the configuration
    Config := TConfigLoader.LoadProjectXML('project.xml');
    try
      WriteLn('[Metadata]');
      WriteLn('  Name: ', Config.Name);
      WriteLn('  Version: ', Config.Version);
      WriteLn('  Author: ', Config.Author);
      WriteLn('  License: ', Config.License);
      WriteLn;

      WriteLn('[Build Configuration]');
      WriteLn('  Main Source: ', Config.BuildConfig.MainSource);
      WriteLn('  Output Directory: ', Config.BuildConfig.OutputDirectory);
      WriteLn('  Executable Name: ', Config.BuildConfig.ExecutableName);
      WriteLn;

      if Config.BuildConfig.Defines.Count > 0 then
      begin
        WriteLn('  Global Defines:');
        for Define in Config.BuildConfig.Defines do
          WriteLn('    -d', Define);
        WriteLn;
      end;

      if Config.Profiles.Count > 0 then
      begin
        WriteLn('[Profiles]');
        for Profile in Config.Profiles do
        begin
          WriteLn('  Profile: ', Profile.Id);
          if Profile.Defines.Count > 0 then
          begin
            WriteLn('    Defines:');
            for Define in Profile.Defines do
              WriteLn('      -d', Define);
          end;
          if Profile.CompilerOptions.Count > 0 then
          begin
            WriteLn('    Compiler Options:');
            for Option in Profile.CompilerOptions do
              WriteLn('      ', Option);
          end;
          WriteLn;
        end;
      end;

      // Validate the configuration
      WriteLn('[Validation]');
      if TConfigLoader.ValidateConfig(Config) then
        WriteLn('  ✓ Configuration is valid')
      else
        WriteLn('  ✗ Configuration is invalid');

    finally
      Config.Free;
    end;

  except
    on E: EProjectConfigError do
    begin
      WriteLn('[ERROR] ', E.Message);
      ExitCode := 1;
    end;
    on E: Exception do
    begin
      WriteLn('[ERROR] Unexpected error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
