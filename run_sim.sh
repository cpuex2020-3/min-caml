#!/bin/sh

sim_path="../simulator/2nd"

make
./min-caml test/$1
cd $sim_path
make
./god_fib ../../min-caml/test/$1.s | xargs printf "%d"
cd -
