HenHen invokes CHICKEN tools through the command-line in a spawned shell process in order to install eggs, build static binaries and run the CHICKEN interpreter.

To do so, HenHen makes assumptions about the *names* of CHICKEN tools in the current environment. For example, it expects `csc` to always refer to the [CHICKEN compiler][1].

However, depending on how you've installed CHICKEN, tools and utilities may have different names on your system entirely. For example, when installing CHICKEN from the [official Arch repositories][3], the compiler and interpreter will be named `chicken-csc` and `chicken-csi` respectively. As you might imagine, this is a problem.

To solve this, HenHen supports defining **global aliases** for each CHICKEN tool: alternate names for HenHen to use when invoking those tools through the command-line.

## Defining Global Aliases

To define your global aliases, create a `.henhen-alias.yaml` file in your home directory:

- On Unix, you should create the file inside `$HOME`.
- On Windows, you should create the file inside `C:/Users/<your-user>`.

Inside `.henhen-alias.yaml`, you can define aliases for each command, structured as [YAML][2] data.

The available fields are:

| Field       | Description                                               |
|-------------|-----------------------------------------------------------|
| installer   | The CHICKEN package manager. (Default: `chicken-install`) |
| compiler    | The CHICKEN compiler. (Default: `csc`)                    |
| interpreter | The CHICKEN interpreter. (Default: `csi`)                 |

As an example, for an user who has installed CHICKEN through the [official Arch repositories][3], their global alias configuration should look like this:

```yaml
installer: chicken-install
compiler: chicken-csc
interpreter: chicken-csi
```

[1]: http://wiki.call-cc.org/man/5/Using%20the%20compiler
[2]: https://yaml.org/
[3]: https://archlinux.org/packages/extra/x86_64/chicken/
