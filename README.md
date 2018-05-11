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

For example to launch a task:
```
$ opam-check-all check <name of the compiler> <a dockerfile>
```

For an example of a compatible Dockerfile see the `dockerfiles` directory.

All subcommands are listed with `opam-check-all --help`

### How to use opam-check-all remotely:

As with local opam-check-all you need to have a server started somewhere and accessible.
Don't forget to open the admin and http ports. Default ports are respectively 6666 and 8080.
You can change them by modifying the yaml config file at the root of the work directory and
restarting the server.

During the first run the server creates an admin user and its key.
To connect to the server remotely just you first need to retreive the `admin.key` file located
in `<workdir>/keys/admin.key` and do `opam-check-all init`.
From there, answer all the questions (hostname, admin-port (default: 6666), username (admin)
and the path to the user key you just retreived).
You now have your client tool configured with an admin user !

To add new users, just use the `opam-check-all add-user <username>` command as the admin and
give the key to your new user. She now just need to do the same procedure but with her username.

Side note: every users have the same rights and can add new users.

Enjoy.
