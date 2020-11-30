#!/bin/sh

sim_path="../simulator/3rd"

make
./min-caml test/$1
cd $sim_path
make
./god_float ../../min-caml/test/$1.s | xargs printf "%d"
cd -
