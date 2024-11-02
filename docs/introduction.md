

## Examples

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
