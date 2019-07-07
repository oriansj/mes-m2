## Copyright (C) 2019 Jeremiah Orians
## This file is part of Gnu Mes
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

# Prevent rebuilding
VPATH = bin:test:test/results

CC?=gcc
CFLAGS:=$(CFLAGS) -D_GNU_SOURCE -std=c99 -ggdb -D WITH_GLIBC=1

mes-m2: mes_vector.c mes_hash.c mes_struct.c mes_math.c mes_strings.c mes_module.c mes_gc.c mes_lib.c mes_printer.c mes_reader.c mes_posix.c mes_builtins.c mes_eval.c mes.c temp.c functions/in_set.c functions/numerate.c functions/file_print.c | bin
	$(CC) $(CFLAGS) mes_vector.c \
	mes_hash.c \
	mes_struct.c \
	mes_math.c \
	mes_strings.c \
	mes_module.c \
	mes_gc.c \
	mes_lib.c \
	mes_printer.c \
	mes_reader.c \
	mes_posix.c \
	mes_builtins.c \
	mes_eval.c \
	mes.c \
	temp.c \
	functions/in_set.c \
	functions/numerate.c \
	functions/file_print.c \
	-o bin/mes-m2

# Clean up after ourselves
.PHONY: clean
clean:
	rm -rf bin/ test/results/
#	./test/test000/cleanup.sh

# Directories
bin:
	mkdir -p bin

results:
	mkdir -p test/results

# tests
test: test000-binary \
	test001-binary | results
#	sha256sum -c test/test.answers

test000-binary: results mes-m2
	test/test000/hello.sh

test001-binary: results mes-m2
	test/test001/hello.sh

DESTDIR:=
PREFIX:=/usr/local
bindir:=$(DESTDIR)$(PREFIX)/bin
.PHONY: install
install: mes-m2
	mkdir -p $(bindir)
	cp $^ $(bindir)
