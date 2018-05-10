### How to install opam-check-all:

```
$ opam pin add opam-check-all .
```

### How to use opam-check-all locally:

For opam-check-all to work you need to start the server like so:
```
$ opam-serve-all <a new clean path or a path to an existing work directory>
```
For instance:
```
$ opam-serve-all /tmp/opam-check-all
```

Now simply use the `opam-check-all` command. First we need to initialize it like so:
```
$ opam-check-all --from-local-workdir /tmp/opam-check-all
```
or used any custom path given to the server.

Now you can send any command to the server using the `opam-check-all` command.

For example to lauch a task:
```
$ opam-check-all check <name of the compiler> <a dockerfile>
```

For an example of a compatible Dockerfile see the `dockerfiles` directory.
All subcommands are listed with `opam-check-all --help`

### How to use opam-check-all remotely:

**TODO**
