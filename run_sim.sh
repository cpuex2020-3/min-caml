#!/bin/sh

sim_path="../simulator/2nd"

make
./min-caml test/$1
cp lib.s tmp.s
cat tri.s >> tmp.s
cat test/$1.s >> tmp.s
cd $sim_path
make
./god_fib ../../min-caml/tmp.s | xargs printf "%d"
cd -
