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

mes-m2: mes_vector.c mes_hash.c mes_struct.c mes_math.c mes_strings.c mes_module.c mes_gc.c mes_lib.c mes_printer.c mes_reader.c mes_posix.c mes_builtins.c mes_eval.c mes.c functions/in_set.c functions/numerate.c functions/file_print.c | bin
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
	mes_init.c \
	mes.c \
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
test: test000.answer \
	test001.answer \
	test100.answer \
	test101.answer \
	test102.answer \
	test103.answer \
	test105.answer \
	test106.answer \
	test109.answer \
	test133.answer \
	test200-binary | results
	sha256sum -c test/test.answers
#	test104.answer
#	test107.answer
#	test108.answer
#	test110.answer
#	test111.answer
#	test112.answer
#	test113.answer
#	test114.answer
#	test115.answer
#	test116.answer
#	test117.answer
#	test118.answer
#	test119.answer
#	test120.answer
#	test121.answer
#	test122.answer
#	test123.answer
#	test124.answer
#	test125.answer
#	test126.answer
#	test127.answer
#	test128.answer
#	test129.answer
#	test130.answer
#	test131.answer
#	test132.answer

test000.answer: results mes-m2
	test/test000/hello.sh

test001.answer: results mes-m2
	test/test001/hello.sh

test100.answer: results mes-m2
	test/test100/hello.sh

test101.answer: results mes-m2
	test/test101/hello.sh

test102.answer: results mes-m2
	test/test102/hello.sh

test103.answer: results mes-m2
	test/test103/hello.sh

test104.answer: results mes-m2
	test/test104/hello.sh

test105.answer: results mes-m2
	test/test105/hello.sh

test106.answer: results mes-m2
	test/test106/hello.sh

test107.answer: results mes-m2
	test/test107/hello.sh

test108.answer: results mes-m2
	test/test108/hello.sh

test109.answer: results mes-m2
	test/test109/hello.sh

test110.answer: results mes-m2
	test/test110/hello.sh

test111.answer: results mes-m2
	test/test111/hello.sh

test112.answer: results mes-m2
	test/test112/hello.sh

test113.answer: results mes-m2
	test/test113/hello.sh

test114.answer: results mes-m2
	test/test114/hello.sh

test115.answer: results mes-m2
	test/test115/hello.sh

test116.answer: results mes-m2
	test/test116/hello.sh

test117.answer: results mes-m2
	test/test117/hello.sh

test118.answer: results mes-m2
	test/test118/hello.sh

test119.answer: results mes-m2
	test/test119/hello.sh

test120.answer: results mes-m2
	test/test120/hello.sh

test121.answer: results mes-m2
	test/test121/hello.sh

test122.answer: results mes-m2
	test/test122/hello.sh

test123.answer: results mes-m2
	test/test123/hello.sh

test124.answer: results mes-m2
	test/test124/hello.sh

test125.answer: results mes-m2
	test/test125/hello.sh

test126.answer: results mes-m2
	test/test126/hello.sh

test127.answer: results mes-m2
	test/test127/hello.sh

test128.answer: results mes-m2
	test/test128/hello.sh

test129.answer: results mes-m2
	test/test129/hello.sh

test130.answer: results mes-m2
	test/test130/hello.sh

test131.answer: results mes-m2
	test/test131/hello.sh

test132.answer: results mes-m2
	test/test132/hello.sh

test133.answer: results mes-m2
	test/test133/hello.sh

test200-binary: results mes-m2
	test/test200/hello.sh

# Generate test answers
.PHONY: Generate-test-answers
Generate-test-answers:
	sha256sum test/results/* >| test/test.answers

DESTDIR:=
PREFIX:=/usr/local
bindir:=$(DESTDIR)$(PREFIX)/bin
.PHONY: install
install: mes-m2
	mkdir -p $(bindir)
	cp $^ $(bindir)
