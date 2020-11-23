riscv64-unknown-elf-gcc -g -O2 -Wall test/$1.s stub.c libmincaml.S lib.s -lm -o test/$1
spike pk test/$1
