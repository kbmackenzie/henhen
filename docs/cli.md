## Command-Line Interface

HenHen has a nice, simple command-line interface for building and managing projects. All available commands are documented below!

1. [build](#henhen-build)
2. [run](#henhen-run)
3. [init](#henhen-init)
4. [install](#henhen-install)
5. [uninstall](#henhen-uninstall)
6. [interpret](#henhen-interpret)
7. [repl](#henhen-repl)
8. [copy](#henhen-copy)
9. [clean](#henhen-clean)

```
henhen - a build tool for CHICKEN Scheme

Usage: henhen COMMAND [(-v|--verbose) | (-q|--quiet)]

Available options:
  -v,--verbose             Enable verbose mode
  -q,--quiet               Silence log messages
  -h,--help                Show this help text

Available commands:
  build                    Build project
  run                      Run a target or config script in virtual environment
  init                     Initialize project
  install                  Install dependency
  uninstall                Remove dependency
  interpret                Interpret a Scheme script in virtual environment
  repl                     Run the CHICKEN REPL in virtual environment
  copy                     Copy executable target
  clean                    Clean project directory
```

### `henhen build`

Build the project in the current directory.

```
Usage: henhen build 

  Build project

Available options:
  -h,--help                Show this help text
```

### `henhen run`

Run something inside the virtual environment. That 'something' can be...

- ... a [binary executable target defined in your configuration file](./config.md#build-targets).
- ... an [executable component of an installed egg][1].
- ... a script defined in the [`scripts` field of your configuration file.](./config.md#scripts)

```
Usage: henhen run NAME [ARGS]

  Run a target or config script in virtual environment

Available options:
  -h,--help                Show this help text
```

### `henhen init`

Initialize a Henhen project in the current directory.

This creates a `henhen.yaml` file and a `.gitignore` file.

```
Usage: henhen init [NAME]

  Initialize project

Available options:
  -h,--help                Show this help text
```

### `henhen install`

Add a new dependency to your project.

It accepts two arguments:
- The dependency egg's name.
- An optional [git][2] repository URL to fetch from.

When the second argument is provided, the repository URL is added to the `fetch` field in the configuration file.

You can also specify the **version** of an egg by using a colon (`:`) after the egg's name (in the same way you would do with `chicken-install`).

```
Usage: henhen install NAME [SOURCE]

  Install dependency

Available options:
  -h,--help                Show this help text
```

### `henhen uninstall`

Remove a dependency from your project configuration.

This command **does not invoke `chicken-uninstall`**, contrary to what you might expect. All it does is politely remove that dependency from your configuration file. If you want to truly purge all unused dependencies from your virtual environment, it's best to do spring cleaning: run `henhen clean --purge && henhen build` to erase the virtual environment and start fresh, building your project all over again.

**Note:** This command does not affect build targets' `dependencies` field.

```
Usage: henhen uninstall NAME [-f|--fetch]

  Remove dependency

Available options:
  -f,--fetch               Remove key from fetch map
  -h,--help                Show this help text
```

### `henhen interpret`

Interpret a CHICKEN Scheme script in the virtual environment.

```
Usage: henhen interpret PATH

  Interpret a Scheme script in virtual environment

Available options:
  -h,--help                Show this help text
```

### `henhen repl`

Run the CHICKEN REPL in the virtual environment.

```
Usage: henhen repl 

  Run the CHICKEN REPL in virtual environment

Available options:
  -h,--help                Show this help text
```

### `henhen copy`

When [build targets of `type: executable`](./config.md#executables) are built, static binaries are generated in `.henhen/_build` and symlinks are created in `.henhen/bin`.

This utility command copies a generated binary from `.henhen/_build` to a specified directory. It accepts two arguments:

- The name of a build target.
- An optional second argument specifying the directory to copy the generated binary to.

When no second argument is given, this command simply copies the binary to the project's root directory.

```
Usage: henhen copy NAME [DESTINATION]

  Copy executable target

Available options:
  -h,--help                Show this help text
```

### `henhen clean`

Clean the build cache. Next time you build the project, all build targets will be re-built from scratch.

When the `--purge` option is given, this command becomes much more aggressive: it removes the `.henhen` directory entirely, erasing the virtual environment. When you want to start fresh (i.e. re-install all dependencies, re-build everything), this is the option you want to use.

```
Usage: henhen clean [-p|--purge]

  Clean project directory

Available options:
  -p,--purge               Purge virtual environment entirely
  -h,--help                Show this help text
```

[1]: https://wiki.call-cc.org/man/5/Egg%20specification%20format#program
[2]: https://git-scm.com/
