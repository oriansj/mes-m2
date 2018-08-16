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

mes=${1-scaffold/cons-mes}

rm -f $mes.M2
rm -f $mes.S
rm -f $mes.o
rm -f $mes.m2-out

trace "CPP        $mes.c" true
i686-unknown-linux-gnu-cpp -E \
    -U __GNUC__\
    -D const=\
    -D enum=\
    -D functionn_t=FUNCTION\
    -D gid_t=int\
    -D intptr_t=int\
    -D off_t=int\
    -D pid_t=int\
    -D ssize_t=int\
    -D size_t=int\
    -D type_t=int\
    -D uid_t=int\
    -D long=int\
    -D unsigned=\
    -D FILE=int\
    -D SCM=int\
    -D __MESC__\
    -D __M2_PLANET__\
    -D MODULEDIR=\"$MODULEDIR/\"\
    -D PREFIX=\"$PREFIX\"\
    -D VERSION=\"$VERSION\"\
    -I lib\
    -I src\
    -I include\
    $mes.c\
    | grep -v double\
    | grep -v float\
    | grep -v typedef\
    | grep -v '\.\.\.'\
    | sed \
          -e 's,[*] *argv[][]],**argv,g'\
          -e 's,[*] *env[][]],**env,g'\
          -e 's,int atexit,#int atexit,g'\
          -e 's,(char[*]),,g'\
          -e 's,void [*] bsearch,#void * bsearch,g'\
          -e 's,void qsort,#void qsort,g'\
          -e 's,int int,int,g'\
          -e 's,intptr_t,int,g'\
          -e 's,ssize_t,int,g'\
          -e 's,size_t,int,g'\
          -e 's,pid_t,int,g'\
          -e 's,off_t,int,g'\
          -e 's,uid_t,int,g'\
    > $mes.M2

if [ "$V" = 2 ]; then
    cat $mes.M2
fi

trace "M2-Planet  $mes.M2" $M2_PLANET\
    -f $mes.M2 \
    -o $mes.S

test -s $mes.S || exit 101

trace "M1         $mes.M1" $M1\
    --LittleEndian\
    --Architecture 1\
    -f lib/x86-mes/x86.M1\
    -f $X86_M1\
    -f $mes.S\
    -o $mes.o
sed -i -e s,FUNCTION_,, $mes.o

trace "HEX2       $mes.hex2" $HEX2\
    --LittleEndian\
    --Architecture 1\
    --BaseAddress 0x1000000\
      -f $X86_ELF\
      -f lib/x86-mes/crt1.o\
      -f lib/x86-mes/libc.o\
      -f $mes.o\
      --exec_enable\
      -o $mes.m2-out

# FIXME: to find boot-0.scm; rename to MES_DATADIR
trace "TEST       $mes.m2-out"
echo '(exit 42)' | MES_PREFIX=../mes/mes $mes.m2-out 2> $mes.m2-log
