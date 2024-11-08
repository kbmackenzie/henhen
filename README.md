HenHen is a build tool for CHICKEN Scheme, designed to work alongside `chicken-install`. It can:

- Install dependencies in an **isolated virtual environment**. No more system-wide installs!
- Manage multiple local eggs seamlessly, and build them together.
- Fetch and install dependencies from **anywhere** by specifying an URL. No need to rely entirely on the [official egg index][1]!

It creates an isolated egg repository for each project inside a `.henhen` folder.

With HenHen, you can experiment with eggs all you want without polluting your global CHICKEN installation—and if anything ever goes wrong on the way, all you need to do to get a fresh start with a clean slate is run `henhen clean --purge`.

## Table of Contents

1. [Philosophy](#philosophy)
2. [Installation](#installation)
3. [B

## Philosophy

HenHen is designed for convenience. I wrote this to get around some of the behavior of `chicken-install` that I disliked; namely:

- By default, `chicken-install` installs every egg **globally**, system-wide.
- Because of the *default location of the egg repository*, `chicken-install` requires superuser privilleges to install eggs (see [this section][2]). To change the repository location, you must either *build CHICKEN from source* or *set specific environment variables*.

HenHen is designed to circumvent **both of these issues** with as little hassle as possible, creating a virtual environment for each project.

## Installation

HenHen is a single binary executable. A comprehensive installation guide can be found [here](./INSTALL.md).

The guide linked above also includes steps to build HenHen from source!

[1]: http://wiki.call-cc.org/releasing-your-egg#publishing-your-egg
[2]: http://wiki.call-cc.org/man/5/Extension%20tools#security
