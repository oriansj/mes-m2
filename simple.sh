#!/usr/bin/env bash
# Most minimal build command for mes.c
set -eux
rm -rf out-glibc/
mkdir out-glibc
gcc -ggdb -D WITH_GLIBC=1 -D VERSION=\"0.19\" -D MODULEDIR=\"module\" -D PREFIX=\"/usr/local\" -I lib  libmes.c mes.c -o out-glibc/mes

echo '(display "hello\n")' | MES_BOOT=boot-01.scm out-glibc/mes

# For a better test, we need some SCM files;
# have a recent mes @wip worktree checked-out in ../mes
test -d ../mes && test -f ../mes/simple.sh

# A somewhat more elaborate test: running from ../mes
( cd ../mes && MES_DEBUG=4 MES_PREFIX=mes MES=../mes-m2/out-glibc/mes tests/base.test )

# Or run from here
rm -rf mes module tests
ln -sf ../mes/{mes,module,tests} .
MES_DEBUG=4 MES_PREFIX=mes MES=out-glibc/mes tests/base.test

# Or copy over the necessary files (and commit them here?)
rm -rf mes module tests
tar -cf- -C ../mes mes/module/mes/{boot-03.scm,{type-0,module,base,quasiquote,let,scm}.mes} | tar -xf-
tar -cf- -C ../mes module/mes/quasiquote.mes module/mes/test.scm tests/base.test tests/data | tar -xf-
MES_DEBUG=4 MES_PREFIX=mes MES=out-glibc/mes tests/base.test
