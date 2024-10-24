## Table of Contents

1. [Config File](#config-file)
    1. [name](#name)
    2. [source-files](#source-files)
    3. [source-root](#source-root)
    4. [data-files](#data-files)
    5. [dependencies](#dependencies)
    6. [fetch](#fetch)
    7. [scripts](#scripts)
    8. [aliases](#aliases)
    9. [targets](#targets)
2. [Build Targets](#build-targets)
    1. [Eggs](#eggs)
    2. [Executables](#executables)

## Config File

The top-level fields you can define in your `henhen.yaml` config file are:

### `name`

The name of your project. This name is used in log messages and error messages, but does not currently have much value otherwise.

### `source-files`

A list of patterns for globbing source files, such as:

- `*.scm`    - All .scm files in the current directory.
- `src/**`   - All files inside the `src` directory.
- `main.scm` - A `main.scm` file in the current directory.

All the source files in your project should be locatable through the patterns you define in this list.

```yaml
source-files:
- '*.scm'
- '*.egg'
```

### `source-root`

A directory where all source files are located. When you define this field in your config, HenHen will look for source files inside of that directory.

All patterns defined in `source-files` are interpreted as relative to the source root.
```yaml
source-root: 'src' # Look for source files inside 'src'.
```
When this field isn't specified, HenHen uses the current directory as the source root.

### `data-files`

A list of patterns for globbing data files: static assets that are relevant to your project.

It works exactly like `source-files`, but is **not affected** by `source-root`. Thus, you can safely define a source root and still easily glob static files from the current directory without indirections.

### `dependencies`

A list of dependencies for your project. A dependency can be: 

- An egg available in the official egg index and installable through `chicken-install <name>`.
- An egg with a source URL defined in the `fetch` field of your config file.

When you build your project, all dependencies will be installed locally in the `.henhen` directory.

```yaml
dependencies:
- srfi-1
- srfi-13
```

### `fetch`

A map associating custom dependency keys with their respective source URLs. All custom dependency URLs should point to Git repositories.

Any URL that can be cloned with `git clone <url>` can be used!
```yaml
fetch:
  foo: '<url here>'
  bar: '<url here>'
  baz: '<url here>'
```
Any dependencies defined in that map can then be listed as a dependency either on the top level `dependencies` field or in an individual target. All dependencies are installed when you build your project.

### `scripts`

A map where you can define custom shell script one-liners to use with `henhen run`.

```yaml
scripts:
  test: 'henhen interpret tests/run.scm'
```
```bash
# You can run scripts like you'd run a normal target:
henhen run test
```
**Note:** A script and a target should not have the same name. When there's a name conflict between a script and a target, HenHen will choose to run the script.

### `aliases`

A map where you can define **aliases** for the various CHICKEN commands/binaries.

This is useful if your installation of CHICKEN has different names for the commands/binaries. Some distribution packages will install the interpreter as `chicken-csi` instead of `csi`, for example.

The available fields are:

| Field       | Description                                         |
|-------------|-----------------------------------------------------|
| installer   | The installer command. (Default: `chicken-install`) |
| compiler    | The compiler command. (Default: `csc`)              |
| interpreter | The interpreter command. (Default: `csi`)           |

```yaml
aliases:
  compiler: chicken-csc
  interpreter: chicken-csi
```

### `targets`

A map where you can define the **build targets** for your project.

A **build target** can be:

- A CHICKEN egg defined somewhere within the project directory (with its own `.egg` file).
- A source file to be compiled into a static binary executable.

Targets can have their own list of dependencies, and they can depend on other targets. HenHen will always build a target's dependencies first before building the target itself.

[See the section on build targets](#build-targets) to learn how to define them.

```yaml
targets:
  hello-world:
    type: executable
    source: './hello-world.scm'
  example-egg:
    type: egg
    directory: './example-egg/'
```

## Build Targets

A **build target** represents one component of a project that should be built when the project is built.

Targets can depend on each other: a target's dependencies are always built before the target itself. 

A target is defined as a map. The `type` field defines what type of build target it is, and what additional fields it supports. HenHen supports two types of targets: `egg` targets and `executable` targets.

### Eggs

Egg targets can be defined with `type: egg`, and accept the following fields:

| Field           | Description                                                              |
|-----------------|--------------------------------------------------------------------------|
| `directory`     | The egg's root directory (where the `.egg` file is located).             |
| `dependencies`  | The egg's dependencies. (See [this section](#dependencies)!)             |
| `extra-options` | Extra options to be passed to `chicken-install` when installing the egg. |

When an egg target is built, it's locally installed in the `.henhen` directory.

```yaml
targets:
  my-egg:
    type: egg
    directory: './my-egg/'
```

### Executables

Static binary executable targets can be defined with `type: executable`, and accept the following fields:

| Field           | Description                                                          |
|-----------------|----------------------------------------------------------------------|
| `source`        | The path to the source file to be compiled into an executable.       |
| `dependencies`  | The source file's dependencies. (See [this section](#dependencies)!) |
| `extra-options` | Extra options to be passed to `csc` when compiling the executable.   |

When an executable target is built, it's locally installed in the `.henhen` directory, and it can be run with `henhen run <name>`. The generated file can be copied to a specified directory with `henhen copy`.

```yaml
targets:
  my-executable:
    type: executable
    source: './my-executable.scm'
```
