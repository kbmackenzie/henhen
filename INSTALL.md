## Installation Guide

The easiest way to install HenHen is to download **pre-compiled binaries** found [here][1]!

You're encouraged to take a look at the [dependencies](#dependencies) section before installing.

### Linux

1. Download the `henhen` binary executable.
2. Copy it to `~/.local/bin`:
```bash
chmod +x henhen
cp henhen ~/.local/bin/
```
3. Add `~/.local/bin` to your PATH if it hasn't already been added.

### Windows

The simplest way to install/use Mewlix on Windows is: 

1. Download the `henhen.exe` executable.
2. Place it in a new directory.
3. Add that directory to your PATH. See [this StackOverflow answer][4] for some help with that.

If you wish to temporarily add the directory to your PATH for a running `cmd` instance, you can do:
```cmd
set "PATH=%PATH%;C:\your-mewlix-folder\"
```
These changes will only apply to that instance of `cmd`, and will not affect the system as a whole.

> "Wait, can't I just use SETX to permanently change my PATH variable with Batch/CMD?"

You *can*, but I don't think you *should*. [This StackOverflow answer explains why][5].

## Dependencies

HenHen depends on [git][6] and the [CHICKEN tools][7]—more specifically, HenHen will invoke those tools through the command-line in a spawned shell process.

**Note:** HenHen makes assumptions about the names of CHICKEN tools. To specify names, see the [documentation on global aliases](./docs/aliases.md).

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

[1]: https://github.com/kbmackenzie/henhen/releases/latest
[2]: https://github.com/kbmackenzie/henhen
[3]: https://docs.haskellstack.org/en/stable/
[4]: https://stackoverflow.com/a/44272417/19764270
[5]: https://stackoverflow.com/a/69239861/19764270
[6]: https://git-scm.com/
[7]: https://code.call-cc.org/
