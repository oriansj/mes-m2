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

. build-aux/trace.sh

HEX2=${HEX2-hex2}
M1=${M1-M1}
M2_PLANET=${M2_PLANET-M2-Planet}
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

# Mes C Lib seed now included here
mkdir -p lib/x86-mes
trace "M1         crt1.S" $M1\
    --LittleEndian\
    --Architecture 1\
    -f lib/x86-mes/x86.M1\
    -f lib/x86-mes/crt1.S\
    -o lib/x86-mes/crt1.o
    
trace "M1         libc.S" $M1\
    --LittleEndian\
    --Architecture 1\
    -f lib/x86-mes/x86.M1\
    -f lib/x86-mes/libc.S\
    -o lib/x86-mes/libc.o


C_FILES="
scaffold/main
scaffold/hello
broken-scaffold/m
broken-scaffold/argv
scaffold/micro-mes
scaffold/milli-mes
scaffold/tiny-mes
scaffold/cons-mes
scaffold/load-mes
scaffold/mini-mes
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

# TODO:
NOLINK=1 sh boot.sh lib/libc
