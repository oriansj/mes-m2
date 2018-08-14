#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of GNU Mes.
#
# GNU Mes is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# GNU Mes is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

set -e

. build-aux/trace.sh

# We need a 32bit gcc, here's what I do:
# guix environment --system=i686-linux --ad-hoc gcc-toolchain@5
CC64=${CC64-gcc}
MES_PREFIX=${MES_PREFIX-../mes}
MES_SEED=${MES_SEED-../mes-seed}

export MES_PREFIX
unset C_INCLUDE_PATH LIBRARY_PATH

# Function and symbol snarfing was used by mes.c; copied here as a
# temporary hack in the transition to mes.M2
trace "MES.SNARF  src/gc.c" sh $MES_PREFIX/build-aux/mes-snarf.scm --mes src/gc.c
trace "MES.SNARF  src/lib.c" sh $MES_PREFIX/build-aux/mes-snarf.scm --mes src/lib.c
trace "MES.SNARF  src/math.c" sh $MES_PREFIX/build-aux/mes-snarf.scm --mes src/math.c
trace "MES.SNARF  src/mes.c" sh $MES_PREFIX/build-aux/mes-snarf.scm --mes src/mes.c
trace "MES.SNARF  src/posix.c" sh $MES_PREFIX/build-aux/mes-snarf.scm --mes src/posix.c
trace "MES.SNARF  src/reader.c" sh $MES_PREFIX/build-aux/mes-snarf.scm --mes src/reader.c
trace "MES.SNARF  src/vector.c" sh $MES_PREFIX/build-aux/mes-snarf.scm --mes src/vector.c

# The focus is on scaffold/*.c, building up to src/mes.c.
# 
# Therefore we start by cheating about the small Mes Lib C; part of
# this effort is defining the minimal M2 library we need.  Also, it
# will probably use bits from mescc-tools/m2-planet.
mkdir -p lib/x86-mes-gcc
trace "CC         crt1.c" $CC64\
    -c\
    -o lib/x86_64-mes-gcc/crt1.o\
    lib/linux/x86_64-mes-gcc/crt1.c

trace "CC         libc.c" $CC64\
    -I include\
    -I include\
    -I lib\
    -c\
    -o lib/x86_64-mes-gcc/libc.o\
    lib/libc.c

C_FILES="
scaffold/main
scaffold/hello
scaffold/m
scaffold/argv
scaffold/micro-mes
scaffold/milli-mes
scaffold/tiny-mes
scaffold/cons-mes
scaffold/load-mes
broken-scaffold/mini-mes
src/mes
"

set +e
for t in $C_FILES; do
    if ! test -f $t.c; then
        echo $t: skip
        continue
    fi
    trace "CC64       $t.c" $CC64\
        -std=gnu99\
        -O0\
        -fno-builtin\
        -fno-stack-protector\
        -g\
        -m64\
        -nostdinc\
        -nostdlib\
        -Wno-discarded-qualifiers\
        -Wno-int-to-pointer-cast\
        -Wno-pointer-to-int-cast\
        -Wno-pointer-sign\
        -Wno-int-conversion\
        -Wno-incompatible-pointer-types\
        -I include\
        -I src\
        -D 'PREFIX=\"$MES_PREFIX\"'\
        -D 'MODULEDIR=\"$MES_PREFIX/module\"'\
        -D 'VERSION=\"git\"'\
        -L lib/x86_64-mes-gcc\
        -o $t.x86_64-mes-gcc-out\
        lib/x86_64-mes-gcc/crt1.o\
        lib/x86_64-mes-gcc/libc.o\
        $t.c
    # FIXME: to find boot-0.scm; rename to MES_DATADIR
    trace "TEST       $t.x86_64-mes-gcc-out"
    echo '(exit 42)' | MES_PREFIX=../mes/mes ./$t.x86_64-mes-gcc-out 2> $t.x86_64-mes-gcc-log
    r=$?
    e=0
    [ -f "$t".exit ] && e=$(cat "$t".exit)
    if [ "$e" != "$r" ]; then
        echo "fail: $t: expected exit: $e, got: $r"
        exit 1
    fi
    echo "$t: pass"
done
