#!/bin/sh

sim_path="../simulator/3rd"

make
./min-caml test/$1
cp tri.s tmp.s
cat test/$1.s >> tmp.s
cd $sim_path
make
./god_float ../../min-caml/tmp.s | xargs printf "%d"
cd -
