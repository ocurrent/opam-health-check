### How to install opam-check-all:

```
$ opam pin add opam-check-all .
```

### How to use opam-check-all:
```
$ opam-check-all <your log directory>/<the name of the compiler> <a dockerfile>
```
For instance, to check all packages available on 4.06.1:
```
$ opam-check-all /tmp/logs/4.06.1 dockerfiles/Dockerfile.4.06.1
```
Examples of expected dockerfiles are in the ```dockerfiles``` directory.

### How to get the result from opam-check-all:

```
$ opam-serve-all <your log directory> 8080
```
For instance, to get the result from the check using 4.06.1 as done above:
```
$ opam-serve-all /tmp/logs 8080
```
The result is now available on http://localhost:8080
