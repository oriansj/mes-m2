#! /bin/sh

# Mes --- Maxwell Equations of Software
# Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of Mes.
#
# Mes is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Mes is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Mes.  If not, see <http://www.gnu.org/licenses/>.

set -e

if [ -n "$BUILD_DEBUG" ]; then
    set -x
fi

# We need a 32bit gcc, here's what I do:
# guix environment --system=i686-linux --ad-hoc gcc-toolchain@5
GCC32=${GCC32-i686-unknown-linux-gnu-gcc}
MES_PREFIX=${MES_PREFIX-../mes}
MES_SEED=${MES_SEED-../mes-seed}

unset C_INCLUDE_PATH LIBRARY_PATH

# Function and symbol snarfing was used by mes.c; copied here as a
# temporary hack in the transition to mes.M2
sh $MES_PREFIX/build-aux/mes-snarf.scm --mes src/gc.c
sh $MES_PREFIX/build-aux/mes-snarf.scm --mes src/lib.c
sh $MES_PREFIX/build-aux/mes-snarf.scm --mes src/math.c
sh $MES_PREFIX/build-aux/mes-snarf.scm --mes src/mes.c
sh $MES_PREFIX/build-aux/mes-snarf.scm --mes src/posix.c
sh $MES_PREFIX/build-aux/mes-snarf.scm --mes src/reader.c
sh $MES_PREFIX/build-aux/mes-snarf.scm --mes src/vector.c

# The focus is on scaffold/*.c, building up to src/mes.c.
# 
# Therefore we start by cheating about the small Mes Lib C; part of
# this effort is defining the minimal M2 library we need.  Also, it
# will probably use bits from mescc-tools/m2-planet.
mkdir -p lib/x86-mes-gcc
$GCC32\
    -c\
    -o lib/x86-mes-gcc/crt1.o\
    $MES_PREFIX/lib/crt1.c

$GCC32\
    -I include\
    -I $MES_PREFIX/include\
    -I $MES_PREFIX/lib\
    -c\
    -o lib/x86-mes-gcc/libc.o\
    $MES_PREFIX/lib/libc.c

C_FILES="scaffold/main
broken-scaffold/hello
scaffold/milli-mes
scaffold/micro-mes
scaffold/tiny-mes
scaffold/cons-mes
broken-scaffold/mini-mes
broken-src/mes
"

set +e
for t in $C_FILES; do
    if ! test -f $t.c; then
        echo $t: skip
        continue
    fi
    $GCC32\
        -nostdinc\
        -nostdlib\
        -I include\
        -I $MES_PREFIX/include\
        -I src\
        -D PREFIX=\"$MES_PREFIX\"\
        -D MODULEDIR=\"$MES_PREFIX/module\"\
        -D VERSION=\"git\"\
        -L lib/x86-mes-gcc\
        -o $t.x86-mes-gcc-out\
        lib/x86-mes-gcc/crt1.o\
        lib/x86-mes-gcc/libc.o\
        $t.c
    ./$t.x86-mes-gcc-out
    r=$?
    e=0
    [ -f "$t".exit ] && e=$(cat "$t".exit)
    if [ "$e" != "$r" ]; then
        echo "fail: $t: expected exit: $e, got: $r"
        exit 1
    fi
    echo "$t: pass"
done
