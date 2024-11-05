HenHen is a build tool for CHICKEN Scheme, designed to work alongside `chicken-install`. It can:

- Install dependencies in an **isolated virtual environment**. No more system-wide installs!
- Manage multiple local eggs seamlessly, and build them together.
- Fetch and install dependencies from **anywhere** by specifying an URL. No need to rely entirely on the [official egg index][1]!

It creates an isolated egg repository for each project inside a `.henhen` folder.

With HenHen, you can experiment with eggs all you want without polluting your global CHICKEN installation. And if anything ever goes wrong on the way, all you need to do to get a fresh start is run `henhen clean --purge`!

## "When should I use this?"

HenHen is designed for convenience. I wrote this to get around some of the odd behavior of `chicken-install` that I disliked; namely:

- By default, `chicken-install` will install **every egg globally**.
- By default, due to the *default location of the egg repository*, `chicken-install` requires superuser privilleges to install any egg (see [this section][7]). The only way to change the egg repository location is by either *passing a specific option when building CHICKEN from source* or by *setting environment variables*, which feels sorta awkward.

HenHen is designed to circumvent **both of these issues** with as little hassle as possible, in a project-by-project basis.

HenHen's use cases are:

1. When you want to install and manage eggs for a project in an isolated environment, without polluting your system-wide CHICKEN installation.
2. When you want to want to install eggs by URL (directly from a Github repo, for example), without relying on the official egg index.

## Installation

HenHen is a single binary executable. Pre-compiled binaries can be found [here][6].

**Note:** Make sure you have [git][4] and the [CHICKEN binaries][5] installed too! HenHen depends on both.

## Building From Source

1. Install [stack][3].
2. Clone the [henhen][2] repository:
```bash
git clone https://github.com/kbmackenzie/henhen
cd henhen
```
3. Build the `henhen` binary and install it to `~/.local/bin` with Stack:
```bash
stack setup
stack install
```
4. Add `~/.local/bin` to your PATH if it hasn't already been added.

[1]: http://wiki.call-cc.org/releasing-your-egg#publishing-your-egg
[2]: https://github.com/kbmackenzie/henhen
[3]: https://docs.haskellstack.org/en/stable/
[4]: https://git-scm.com/
[5]: https://code.call-cc.org/
[6]: https://github.com/kbmackenzie/henhen/releases/latest
[7]: http://wiki.call-cc.org/man/5/Extension%20tools#security
