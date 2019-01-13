#!/usr/bin/env bash
# Most minimal build command for mes.c
set -eux
rm -rf out-glibc/
mkdir out-glibc
gcc -ggdb -D WITH_GLIBC=1 -D VERSION=\"0.19\" -D MODULEDIR=\"module\" -D PREFIX=\"/usr/local\" mes_vector.c mes_hash.c mes_struct.c mes_math.c mes_strings.c mes_module.c mes_gc.c mes_lib.c mes_reader.c libmes.c mes.c -o out-glibc/mes

# Simplest of tests
echo '(display "hello\n")' | MES_BOOT=boot-01.scm out-glibc/mes

# Basic test.  Once this works there are ~30 tests in mes/test/*.test
MES_DEBUG=4 MES_PREFIX=mes MES=out-glibc/mes tests/base.test

# If we ever need to get into hairy debugging, a scaffold with ~50
# bootstrap tests is available in mes: scaffold/boot/*.scm
