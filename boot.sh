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

MES_PREFIX=${MES_PREFIX-../mes}
HEX2=${HEX2-hex2}
M1=${M1-M1}
M2_PLANET=${M2_PLANET-M2-Planet}
MES_SEED=${MES_SEED-../mes-seed}
TINYCC_SEED=${TINYCC_SEED-../tinycc-seed}
BLOOD_ELF=${BLOOD_ELF-blood-elf}
CRT0_M1=../m2-planet/test/functions/libc-core.M1
X86_M1=../m2-planet/test/common_x86/x86_defs.M1
X86_ELF=../m2-planet/test/common_x86/ELF-i386.hex2
PREFIX=
MODULEDIR=module
VERSION=git

mes=${1-src/mes}

if false; then
    # verify mes.c stays working with gcc
    i686-unknown-linux-gnu-gcc\
    -nostdlib\
    -I src\
    -I include\
    -I lib\
    -o $mes.x86-mes-gcc\
    --include=lib/libc-gcc.c\
    -D MODULEDIR=\"$MODULEDIR/\"\
    -D PREFIX=\"$PREFIX\"\
    -D VERSION=\"$VERSION\"\
    $TINYCC_SEED/x86-mes-gcc/crt1.o\
    $mes.c
fi

rm -f $mes.S
rm -f $mes.o

cpp -E \
    -U __GNUC__\
    -D const=\
    -D enum=\
    -D type_t=int\
    -D size_t=int\
    -D long=int\
    -D unsigned=\
    -D FILE=int\
    -D SCM=int\
    -D __MESC__\
    -D __M2_PLANET__\
    -D TCHAR=0\
    -D TCLOSURE=1\
    -D TCONTINUATION=2\
    -D TFUNCTION=3\
    -D TKEYWORD=4\
    -D TMACRO=5\
    -D TNUMBER=6\
    -D TPAIR=7\
    -D TPORT=8\
    -D TREF=9\
    -D TSPECIAL=10\
    -D TSTRING=11\
    -D TSYMBOL=12\
    -D TVALUES=13\
    -D TVARIABLE=14\
    -D TVECTOR=15\
    -D TBROKEN_HEART=16\
    -D MODULEDIR=\"$MODULEDIR/\"\
    -D PREFIX=\"$PREFIX\"\
    -D VERSION=\"$VERSION\"\
    -I src\
    -I include\
    $mes.c\
    | grep -v typedef\
    | grep -v '\.\.\.'\
    | sed \
          -e 's,void qsort,#void qsort,g'\
          -e 's,size_t,int,g'\
          -e 's,int int,int,g'\
    > $mes.M2

if [ -n "$BUILD_DEBUG" ]; then
    cat $mes.M2
fi

$M2_PLANET\
    -f $mes.M2 \
    -o $mes.S

test -s $mes.S || exit 101

$M1\
    --LittleEndian\
    --Architecture 1\
    -f $MES_PREFIX/lib/x86-mes/x86.M1\
    -f $X86_M1\
    -f $mes.S\
    -o $mes.o
sed -i -e s,FUNCTION_,, $mes.o

$HEX2\
    --LittleEndian\
    --Architecture 1\
    --BaseAddress 0x1000000\
      -f $X86_ELF\
      -f lib/x86-mes/crt1.o\
      -f lib/x86-mes/libc.o\
      -f $mes.o\
      --exec_enable\
      -o $mes.m2-out

$mes.m2-out 
