# Alexandria

A package and library manager for C++

## Usage

Define your dependencies on a `package.yaml` file. Local and Git repositories are supported:

```yaml
dependencies:
- name: Foo
  local: /home/user/projects/foo
- name: Bar
  git: git@github.com:user/bar
```

References can be overriden locally by `.package-overrides.yaml` file, whilst editing dependencies locally:

```yaml
dependencies:
- name: Bar
  local: /home/user/projects/bar
```

To download dependencies:

```
$ alexandria fetch
```

To configure, define your configuration in `.alexandria.yaml`, in the local director or in your home directory

```yaml
alexandria:
  dependencyDir: ".dependencies"
```
