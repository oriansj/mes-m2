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

HEX2=${HEX2-hex2}
M1=${M1-M1}
M2_PLANET=${M2_PLANET-M2-Planet}
MES_PREFIX=${MES_PREFIX-../mes}
MES_SEED=${MES_SEED-../mes-seed}

export BUILD_DEBUG HEX2 M1 M2_PLANET MES_PREFIX MES_SEED

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
mkdir -p lib/x86-mes
$M1\
    --LittleEndian\
    --Architecture 1\
    -f $MES_PREFIX/lib/x86-mes/x86.M1\
    -f $MES_SEED/x86-mes/crt1.S\
    -o lib/x86-mes/crt1.o
    
$M1\
    --LittleEndian\
    --Architecture 1\
    -f $MES_PREFIX/lib/x86-mes/x86.M1\
    -f $MES_SEED/x86-mes/libc.S\
    -o lib/x86-mes/libc.o


BROKEN="
scaffold/hello
"

C_FILES="scaffold/main
broken-scaffold/hello
scaffold/milli-mes
scaffold/micro-mes
broken-scaffold/tiny-mes
broken-scaffold/cons-mes
broken-scaffold/mini-mes
broken-src/mes
"

set +e
for t in $C_FILES; do
    if ! test -f $t.c; then
        echo $t: skip
        continue
    fi
    sh boot.sh $t;
    r=$?
    e=0
    [ -f "$t".exit ] && e=$(cat "$t".exit)
    if [ "$e" != "$r" ]; then
        echo "fail: $t: expected exit: $e, got: $r"
        exit 1
    fi
    echo "$t: pass"
done
