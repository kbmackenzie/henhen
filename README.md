HenHen is a build tool for CHICKEN Scheme, designed to work alongside `chicken-install`. It can:

- Install dependencies in an **isolated virtual environment**—no more system-wide installs!
- Manage local eggs seamlessly and generate static binaries.
- Fetch and install dependencies from **anywhere** by specifying a URL. No need to rely on the [official egg index][1] anymore.

## Installation

HenHen is a single binary executable. Pre-compiled binaries can be found [here][6].

Make sure you have [git][4] and the [CHICKEN binaries][5] installed too! HenHen depends on both.

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
