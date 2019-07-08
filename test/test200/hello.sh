#! /bin/bash
## Copyright (C) 2017 Jeremiah Orians
## This file is part of Gnu Mes.
##
## Gnu Mes is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Gnu Mes is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Gnu Mes.  If not, see <http://www.gnu.org/licenses/>.

set -ux
export MES_DEBUG=2
export MES_ARENA=${MES_ARENA-100000000}
export MES_STACK=${MES_STACK-500000}
export MES_PREFIX=mes
export MES=bin/mes-m2

${SCHEME-$MES} \
	--no-auto-compile\
	-e main\
	-L /usr/local/share/guile/site/2.2\
	-C /usr/local/lib/guile/2.2/site-ccache\
	scripts/mescc.scm test/test200/hello.c -m 32 -o test/results/test200-binary &> test/test200/log
r=$?
[ $r = 0 ] || exit 1

out=$(./test/results/test200-binary 2>&1)
r=$?
[ "$out" = "Hello, Mescc!" ] || exit 2
[ $r = 42 ] || exit 3
exit 0
