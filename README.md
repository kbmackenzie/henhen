HenHen is a build tool for CHICKEN Scheme, designed to work alongside `chicken-install`. It can:

- Install dependencies in an **isolated virtual environment**. No more system-wide installs!
- Manage multiple local eggs seamlessly, and build them together.
- Fetch and install dependencies from **anywhere** by specifying a [git][3] repository URL for each one. No need to rely entirely on the [official egg index][1]!

It creates an isolated egg repository for each project inside a `.henhen` folder.

With HenHen, you can manage dependencies more easily and reliably, installing them on a *per-project* basis—and you can experiment with eggs all you want without ever polluting your global CHICKEN installation.

And if anything goes wrong along the way, all you need to start fresh with a clean slate is run `henhen clean --purge`!

## Table of Contents

1. [Philosophy](#philosophy)
2. [Installation](#installation)
3. [Documentation](#documentation)
4. [Quick Start](#quick-start)

## Philosophy

HenHen is designed for convenience. I wrote this to get around some of the behavior of `chicken-install` that I disliked; namely:

- By default, `chicken-install` installs every egg **globally**, system-wide.
- Because of the *default location of the egg repository*, `chicken-install` requires superuser privilleges to install eggs (see [this section][2]). To change the repository location, you must either *build CHICKEN from source* or *set specific environment variables*.

HenHen is designed to circumvent **both of these issues** with as little hassle as possible, creating a virtual environment for each project.

## Installation

HenHen is a single binary executable. A comprehensive installation guide can be found [here](./INSTALL.md).

The guide linked above also includes steps to build HenHen from source!

## Documentation

An introduction + a **tutorial** for HenHen can be found on [here](./docs/introduction.md)!

To learn more about HenHen, read the documentation:

1. [Configuration File](./docs/config.md)
2. [Command-Line Interface](./docs/cli.md)
3. [Defining Aliases For Tools](./docs/aliases.md)
4. [Frequently Asked Questions](./docs/faq.md)

## Quick Start

To initialize HenHen in the current directory, you can use:

```bash
henhen init "your project name"
```

This will create a `henhen.yaml` file populated with basic fields. This is the **configuration file** for your project, where you can...

- ... list dependencies to be installed.
- ... define custom URLs to fetch certain dependencies from.
- ... define **targets** to build. A **target** can be an **egg** or a static binary **executable**.

A simple config file to build a simple egg should look like this:

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

We can do a lot more, however. As a more complex example, let's build a local egg `foo`, a local egg `bar` that depends on `foo`, and a static executable that uses both `foo` and `bar`. That's simple with HenHen!

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

[1]: http://wiki.call-cc.org/releasing-your-egg#publishing-your-egg
[2]: http://wiki.call-cc.org/man/5/Extension%20tools#security
[3]: https://git-scm.com/
