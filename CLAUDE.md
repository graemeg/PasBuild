# PasBuild — Claude Code Instructions

## Project Summary

PasBuild is a Maven-inspired build automation tool for Free Pascal (FPC) projects.
It is written in Object Pascal, self-hosting (builds itself), and follows
convention-over-configuration with a goal-based lifecycle.

- **Language**: Object Pascal (FPC, `{$mode objfpc}{$H+}`)
- **Testing**: FPCUnit (`fpcunit`, `testregistry`, `consoletestrunner`)
- **License**: BSD-3-Clause
- **Author**: Graeme Geldenhuys
- **Current version**: 1.5.0-dev (targeting v1.5.0 release)

---

## Build and Test Commands

**Compiler override:** PasBuild itself is built with FPC. The shell environment
sets `PASBUILD_COMPILER` to the Blaise compiler by default, so always prefix
pasbuild commands with an empty `PASBUILD_COMPILER=` to fall back to `fpc`
(e.g. `PASBUILD_COMPILER= pasbuild compile`). The commands below omit the
prefix for brevity; add it every time.

```bash
# Build PasBuild itself
pasbuild compile

# Compile tests only
pasbuild test-compile

# Run tests (compiled binary lives in target/)
cd target && ./TestRunner --all --format=plain

# Build with a profile
pasbuild compile -p release
pasbuild compile -p debug

# Multi-module build
pasbuild compile          # all modules
pasbuild compile -m mylib # selected module + its dependencies
```

### Bootstrap (first-time, before pasbuild exists)

```bash
mkdir -p target/units
sed 's/\${project.version}/1.0.0/g' src/main/resources/version.inc > target/version.inc
fpc -Mobjfpc -O1 -FEtarget -FUtarget/units -Fitarget -Fusrc/main/pascal src/main/pascal/PasBuild.pas
```

---

## Standard Directory Layout

```
pasbuild/
├── project.xml                  # Project config (POM equivalent)
├── src/
│   ├── main/
│   │   ├── pascal/              # All production source files (*.pas)
│   │   └── resources/           # Filtered resources (version.inc template)
│   └── test/
│       ├── pascal/              # Test source files and TestRunner.pas
│       └── resources/           # Test fixtures (copied to target/ at test-compile)
├── target/                      # Build output (generated; not in VCS)
│   ├── pasbuild                 # Compiled executable
│   ├── TestRunner               # Compiled test runner
│   ├── units/                   # Compiled units (.ppu, .o)
│   └── test-units/              # Compiled test units
├── docs/                        # AsciiDoc design and guide documents
└── samples/                     # Example projects (single, multi-module, dependency-management)
```

---

## Key Source Files

### Production Units (`src/main/pascal/`)

| File | Purpose |
|------|---------|
| `PasBuild.pas` | Program entry point — argument parsing, config loading, command dispatch |
| `PasBuild.CLI.pas` | `TArgumentParser`, `TCommandLineArgs`, goal enum `TBuildGoal`, version/date constants |
| `PasBuild.Types.pas` | Core types: `TProjectConfig`, `TBuildConfig`, `TTestConfig`, `TModuleInfo`, `TModuleRegistry`, `TProfile` |
| `PasBuild.Config.pas` | `TConfigLoader` — XML parser/validator for `project.xml`; `EProjectConfigError` exception |
| `PasBuild.Command.pas` | Abstract `TBuildCommand` base class; `TCommandExecutor` (dependency resolution, deduplication) |
| `PasBuild.Command.Reactor.pas` | `TReactorCommand` — multi-module build orchestration with Maven-style reactor summary |
| `PasBuild.ModuleDiscovery.pas` | `TModuleDiscoverer.DiscoverModules()` — walks aggregator `<modules>` list |
| `PasBuild.Dependencies.pas` | `TDependencyResolver` — resolves `<dependencies>` from local repository |
| `PasBuild.Repository.pas` | Local artifact repository support |
| `PasBuild.Utils.pas` | `TUtils` — logging (`LogInfo`/`LogError`), FPC executable resolution |
| `PasBuild.Command.Clean.pas` | `TCleanCommand` |
| `PasBuild.Command.Compile.pas` | `TCompileCommand`, `TTestCompileCommand` |
| `PasBuild.Command.Test.pas` | `TTestCommand` |
| `PasBuild.Command.Package.pas` | `TPackageCommand` (ZIP archive) |
| `PasBuild.Command.Install.pas` | `TInstallCommand` (install to local repo) |
| `PasBuild.Command.Init.pas` | `TInitCommand` (scaffold new project) |

### Test Units (`src/test/pascal/`)

All test units follow the naming pattern `PasBuild.Test.*.pas` and register
FPCUnit test cases. `TestRunner.pas` wires them all together.

---

## Architecture Patterns

### Command Pattern
- `TBuildCommand` is abstract with `Execute: Integer` and `GetDependencies`.
- `TCommandExecutor` resolves and deduplicates dependencies before executing.
- Each goal maps to a concrete command class.

### Goal Lifecycle (Maven-style)
`clean → process-resources → compile → process-test-resources → test-compile → test → package → source-package → install`

### Project Types (`TProjectType`)
- `ptApplication` — standard executable project (default)
- `ptLibrary` — library project; produces unit artifact for dependents
- `ptPom` — aggregator-only; triggers multi-module reactor build; no artifact

### Multi-Module Support
- Aggregator `project.xml` has `<packaging>pom</packaging>` and a `<modules>` list.
- `TModuleDiscoverer.DiscoverModules()` returns a `TModuleRegistry`.
- `TModuleRegistry.GetBuildOrder()` performs topological sort (DFS) with cycle detection.
- `TReactorCommand` iterates modules in build order, printing a Maven-style reactor summary.
- Module selection: `-m <name>` builds only the named module plus its dependencies.

### Version Inheritance
Child modules inherit `<version>` from the aggregator's `project.xml` when not explicitly set.

### Dependency Resolution
- `<moduleDependencies>` — inter-module deps resolved at build time via registry.
- `<dependencies>` — external deps fetched from local repository (`~/.pasbuild/repository`).

---

## project.xml Structure

```xml
<project>
  <name>MyProject</name>
  <version>1.0.0</version>          <!-- semver; child modules may omit (inherit from aggregator) -->
  <author>Name</author>
  <license>BSD-3-Clause</license>

  <packaging>pom</packaging>         <!-- omit for app; 'pom' = aggregator; 'library' = lib -->

  <modules>                          <!-- aggregator only -->
    <module>lib-core</module>
  </modules>

  <build>
    <mainSource>Main.pas</mainSource>
    <outputDirectory>target</outputDirectory>
    <executableName>myapp</executableName>
    <defines><define>UseCThreads</define></defines>
    <compilerOptions><option>-g</option></compilerOptions>
  </build>

  <test>
    <testSource>TestRunner.pas</testSource>
    <framework>fpcunit</framework>   <!-- auto | fpcunit | fptest -->
    <frameworkOptions><option>--all</option></frameworkOptions>
  </test>

  <moduleDependencies>               <!-- inter-module deps -->
    <module>lib-core</module>
  </moduleDependencies>

  <dependencies>                     <!-- external repo deps -->
    <dependency>
      <name>some-lib</name>
      <version>1.0.0</version>
    </dependency>
  </dependencies>

  <profiles>
    <profile>
      <id>release</id>
      <defines><define>RELEASE</define></defines>
      <compilerOptions><option>-O3</option></compilerOptions>
    </profile>
  </profiles>
</project>
```

---

## Development Conventions

- **TDD**: Write failing tests first, then implement until tests pass.
- **No memory leaks**: Verified with FPC's `-gh` heap trace flag.
- **FPCUnit**: All test classes inherit from `TTestCase`; registered via `RegisterTest`.
- **Generics**: Uses `specialize TFPGObjectList<T>` for typed collections.
- **Error handling**: Structured exceptions (`EProjectConfigError`, `EDependencyError`).
- **Known test failures**: 2 pre-existing failures in `TTestValidatePackagingRules`
  (error messages checked for "aggregator" substring — messages changed).

---

## Known State (as of 2026-03)

- Version: 1.5.0-dev (v1.4.0 released)
- Multi-module support complete (phases 1–7)
- Dependency management implemented
- Maven-style reactor output implemented
- Main branch: `main`; active development on `master`
