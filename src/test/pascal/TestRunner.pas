{
  This file is part of PasBuild.

  Copyright (c) 2025 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

program TestRunner;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry, consoletestrunner,
  PasBuild.Test.Types.MultiModule,
  PasBuild.Test.Config.MultiModule,
  PasBuild.Test.ModuleDiscovery,
  PasBuild.Test.Utils,
  PasBuild.Test.Utils.Console,
  PasBuild.Test.Repository,
  PasBuild.Test.Config.Dependencies,
  PasBuild.Test.Dependencies,
  PasBuild.Test.Integration.Dependencies,
  PasBuild.Test.Command.DependencyTree,
  PasBuild.Test.Command.Resolve,
  PasBuild.Test.Command.Test,
  PasBuild.Test.NestedAggregator,
  PasBuild.Test.Plugin,
  PasBuild.Test.Bootstrap,
  PasBuild.Test.ProcessResources;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  try
    Application.Initialize;
    Application.Title := 'PasBuild FPCUnit Test Runner';
    Application.Run;
  finally
    Application.Free;
  end;
end.
