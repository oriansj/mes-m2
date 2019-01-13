#!/usr/bin/env bash
# Most minimal build command for mes.c
set -eux
rm -rf out-glibc/
mkdir out-glibc
gcc -ggdb -D WITH_GLIBC=1 -D VERSION=\"0.19\" -D MODULEDIR=\"module\" -D PREFIX=\"/usr/local\" -I lib  libmes.c mes.c -o out-glibc/mes
./out-glibc/mes --help
