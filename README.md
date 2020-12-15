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

# How to run raytracer
The following command executes the raytracer program.
```
$ make raytrace
$ cat raytracer/sld | SIMULATOR_EXEC raytracer/minrt.s > out.ppm # this sld file is generated via `python3 raytracer/cvt.py raytracer/contest.sld > raytracer/sld`
```
Where SIMULATOR_EXEC is the path to your simulator executable.
After editting the `out.ppm` file, you will have a valid ppm file!
