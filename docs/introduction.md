## Table of Contents

1. [Documentation](#documentation)
3. [Quick Start](#quick-start)
2. [Configuration](#configuration)

## Documentation

To learn more in-depth about HenHen, see the documentation:

1. [Configuration File](./config.md)
2. [Command-Line Interface](./cli.md)
3. [Defining Aliases For Tools](./aliases.md)
4. [Frequently Asked Questions](./faq.md)

## Quick Start

HenHen's command-line interface is quite simple:

```bash
# Create a new project:
henhen init "project name"

# Build a project:
henhen build

# Run an executable target (or a config script):
henhen run "target-name"

# Interpret a script in the virtual environment:
henhen interpret "path-to-script"

# Run a REPL in the virtual environment:
henhen repl

# Install an egg, appending it to the 'dependencies' field in your config file:
henhen install "dependency-name"

# Erase virtual environment entirely; start with a clean slate:
henhen clean --purge
```

To learn more, see the [command-line interface documentation](./config.md)!

As a simple example, let's create a new project, install the monad egg and run a REPL in the virtual environment:

```bash
henhen init "lolcat"
henhen install shlolcat
henhen repl
```

## Configuration

### Simple Egg

The simplest example would be building a simple, regular egg.

With a folder structure like this:

```tree
.
├── henhen.yaml
├── simple.egg
└── simple.scm
```

Let's write our `henhen.yaml` config file to build that simple egg in our isolated environment:

```yaml
name: simple
source-files:
- '*.scm'
- '*.egg'
dependencies:
- srfi-1
targets:
  simple:
    type: egg
    directory: '.'
```

### Multiple Eggs

In this example, we'll build a local egg `foo`, a local egg `bar` that depends on `foo`, and a static executable that uses both `foo` and `bar`. That's simple with HenHen!

With a folder structure like this:

```tree
.
├── bar
│   ├── bar.egg
│   └── bar.scm
├── foo
│   ├── foo.egg
│   └── foo.scm
├── foobar.scm
└── henhen.yaml
```

Let's write our `henhen.yaml` config file to build our project:

```yaml
name: foobar
source-files:
- 'foo/**'
- 'bar/**'
- 'foobar.scm'
dependencies:
- srfi-1
- srfi-13
- srfi-5
targets:
  foo:
    type: egg
    directory: './foo'
  bar:
    type: egg
    directory: './bar'
    dependencies:
    - foo
  foobar:
    type: executable
    source: './foobar.scm'
    dependencies:
    - foo
    - bar
```
