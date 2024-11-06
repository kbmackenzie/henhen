## Command-Line Interface

### `henhen --help`

```
henhen - a build tool for CHICKEN Scheme

Usage: henhen COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  build                    Build project
  run                      Run binary or script in virtual environment
  init                     Initialize project
  install                  Install dependency
  interpret                Interpret script in virtual environment
  repl                     Run repl in virtual environment
  copy                     Copy executable target
  clean                    Clean project directory
```

### `henhen build --help`

```
Usage: henhen build [-q|--quiet] [-v|--verbose]

  Build project

Available options:
  -q,--quiet               Silence log messages
  -v,--verbose             Enable verbose mode
  -h,--help                Show this help text
```

### `henhen run --help`

```
Usage: henhen run NAME [ARGS] [-q|--quiet] [-v|--verbose]

  Run binary or script in virtual environment

Available options:
  -q,--quiet               Silence log messages
  -v,--verbose             Enable verbose mode
  -h,--help                Show this help text
```

### `henhen init --help`

```
Usage: henhen init [NAME] [-q|--quiet] [-v|--verbose]

  Initialize project

Available options:
  -q,--quiet               Silence log messages
  -v,--verbose             Enable verbose mode
  -h,--help                Show this help text
```

### `henhen install --help`

```
Usage: henhen install NAME [SOURCE] [-q|--quiet] [-v|--verbose]

  Install dependency

Available options:
  -q,--quiet               Silence log messages
  -v,--verbose             Enable verbose mode
  -h,--help                Show this help text
```

### `henhen interpret --help`

```
Usage: henhen interpret PATH [-q|--quiet] [-v|--verbose]

  Interpret script in virtual environment

Available options:
  -q,--quiet               Silence log messages
  -v,--verbose             Enable verbose mode
  -h,--help                Show this help text
```

### `henhen repl --help`

```
Usage: henhen repl [-q|--quiet] [-v|--verbose]

  Run repl in virtual environment

Available options:
  -q,--quiet               Silence log messages
  -v,--verbose             Enable verbose mode
  -h,--help                Show this help text
```

### `henhen copy --help`

```
Usage: henhen copy NAME [DESTINATION] [-q|--quiet] [-v|--verbose]

  Copy executable target

Available options:
  -q,--quiet               Silence log messages
  -v,--verbose             Enable verbose mode
  -h,--help                Show this help text
```

### `henhen clean --help`

```
Usage: henhen clean [-p|--purge] [-q|--quiet] [-v|--verbose]

  Clean project directory

Available options:
  -p,--purge               Purge virtual environment entirely
  -q,--quiet               Silence log messages
  -v,--verbose             Enable verbose mode
  -h,--help                Show this help text
```
