#!/bin/sh
./to_risc-v
riscv64-unknown-elf-gcc -o $1 test/$1.s stub.c libmincaml.S
spike pk $1
