Based on [MinCaml](https://github.com/esumii/min-caml/)

# How to build
```
$ make
```
Now you have `./min-caml`.

# How to run
Once you have `./min-caml` generated,
```
$ ./min-caml foo
```
will compile `foo.ml` to `foo.s`. Note that there is no extension `.ml` in the command line.

# How to run test
The following command executes the tests.
```
$ make test
```
