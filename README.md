Based on [MinCaml](https://github.com/esumii/min-caml/)

# How to build
```
$ ./to_risc-v
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
The following command executes the test for `SUB_TESTS` in Makefile.
```
$ ./to_risc-v
$ make sub_tests
```
Run `$ make all_tests` if you want to run all the tests in `test/` directory.
