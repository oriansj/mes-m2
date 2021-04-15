#! /bin/sh
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

set -x
out=$(MES_BOOT=test/test002/display_error.scm ./bin/mes-m2 2>&1)
r=$?
[ $r = 42 ] || exit 1
[ "$out" = "Hello, failure!!!" ] || exit 2
exit 0
