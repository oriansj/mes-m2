## GNU Mes --- Maxwell Equations of Software
## Copyright © 2016,2017,2018,2021 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
## Copyright © 2019 Jeremiah Orians
##
## This file is part of GNU Mes.
##
## GNU Mes is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## GNU Mes is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

# Prevent rebuilding
VPATH = src:bin:test:test/results

CC?=gcc
CFLAGS:=$(CFLAGS) -D_GNU_SOURCE -std=c99 -ggdb -D WITH_GLIBC=1 -O0
KAEM = kaem
GUILD = guild

default: mes-m2-boot

mes-m2: builtins.c cc.c core.c display.c eval-apply.c gc.c hash.c lib.c apply.c math.c mes.c module.c posix.c reader.c stack.c string.c struct.c symbol.c vector.c | bin
	$(CC) $(CFLAGS)     \
	-DMES_VERSION="0"   \
	src/builtins.c      \
	src/cc.c            \
	src/core.c          \
	src/display.c       \
	src/eval-apply.c    \
	src/gc.c            \
	src/hash.c          \
	src/lib.c           \
	src/apply.c         \
	src/math.c          \
	src/mes.c           \
	src/module.c        \
	src/reader.c        \
	src/stack.c         \
	src/string.c        \
	src/struct.c        \
	src/symbol.c        \
	src/vector.c        \
	-o bin/mes-m2

mes: builtins.c cc.c core.c display.c eval-apply.c gc.c hash.c lib.c m2.c math.c mes.c module.c posix.c reader.c stack.c string.c struct.c symbol.c vector.c | bin
	kaem --verbose --strict

mes-m2-boot:
	rm -rf m2
	mkdir -p m2
	$(KAEM) --strict --verbose

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

# For development using Guile
MODULE_SCM_FILES = $(shell find module -name '*.scm')
GO_FILES = $(MODULE_SCM_FILES:%.scm=%.go)
GUILD_OPTIMIZE = -O1
%.go: %.scm
	GUILE_LOAD_PATH=module:$(GUILE_LOAD_PATH)			\
	GUILE_LOAD_COMPILED_PATH=module:$(GUILE_LOAD_COMPILED_PATH)	\
		$(GUILD) compile $(GUILD_OPTIMIZE) -o $@ $^

all-go: $(GO_FILES)
clean-go:
	rm -f $(GO_FILES)

# tests
test: test000.answer \
	test001.answer \
	test002.answer \
	test003.answer \
	test004.answer \
	test005.answer \
	test006.answer \
	test007.answer \
	test008.answer \
	test009.answer \
	test010.answer \
	test011.answer \
	test012.answer \
	test013.answer \
	test014.answer \
	test015.answer \
	test016.answer \
	test017.answer \
	test018.answer \
	test019.answer \
	test020.answer \
	test021.answer \
	test022.answer \
	test023.answer \
	test024.answer \
	test025.answer \
	test026.answer \
	test027.answer \
	test028.answer \
	test029.answer \
	test030.answer \
	test031.answer \
	test032.answer \
	test033.answer \
	test034.answer \
	test035.answer \
	test036.answer \
	test037.answer \
	test038.answer \
	test040.answer \
	test041.answer \
	test042.answer \
	test043.answer \
	test044.answer \
	test045.answer \
	test046.answer \
	test047.answer \
	test048.answer \
	test049.answer \
	test050.answer \
	test051.answer \
	test052.answer \
	test053.answer \
	test054.answer \
	test055.answer \
	test056.answer \
	test057.answer \
	test058.answer \
	test059.answer \
	test060.answer \
	test061.answer \
	test062.answer \
	test063.answer \
	test064.answer \
	test065.answer \
	test066.answer \
	test067.answer \
	test068.answer \
	test101.answer
#	test039.answer \
#	test100.answer \
#	test102.answer \
#	test103.answer \
#	test105.answer \
#	test106.answer \
#	test109.answer \
#	test133.answer \
#	test200-binary | results
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

test002.answer: results mes-m2
	test/test002/hello.sh

test003.answer: results mes-m2
	test/test003/hello.sh

test004.answer: results mes-m2
	test/test004/hello.sh

test005.answer: results mes-m2
	test/test005/hello.sh

test006.answer: results mes-m2
	test/test006/hello.sh

test007.answer: results mes-m2
	test/test007/hello.sh

test008.answer: results mes-m2
	test/test008/hello.sh

test009.answer: results mes-m2
	test/test009/hello.sh

test010.answer: results mes-m2
	test/test010/hello.sh

test011.answer: results mes-m2
	test/test011/hello.sh

test012.answer: results mes-m2
	test/test012/hello.sh

test013.answer: results mes-m2
	test/test013/hello.sh

test014.answer: results mes-m2
	test/test014/hello.sh

test015.answer: results mes-m2
	test/test015/hello.sh

test016.answer: results mes-m2
	test/test016/hello.sh

test017.answer: results mes-m2
	test/test017/hello.sh

test018.answer: results mes-m2
	test/test018/hello.sh

test019.answer: results mes-m2
	test/test019/hello.sh

test020.answer: results mes-m2
	test/test020/hello.sh

test021.answer: results mes-m2
	test/test021/hello.sh

test022.answer: results mes-m2
	test/test022/hello.sh

test023.answer: results mes-m2
	test/test023/hello.sh

test024.answer: results mes-m2
	test/test024/hello.sh

test025.answer: results mes-m2
	test/test025/hello.sh

test026.answer: results mes-m2
	test/test026/hello.sh

test027.answer: results mes-m2
	test/test027/hello.sh

test028.answer: results mes-m2
	test/test028/hello.sh

test029.answer: results mes-m2
	test/test029/hello.sh

test030.answer: results mes-m2
	test/test030/hello.sh

test031.answer: results mes-m2
	test/test031/hello.sh

test032.answer: results mes-m2
	test/test032/hello.sh

test033.answer: results mes-m2
	test/test033/hello.sh

test034.answer: results mes-m2
	test/test034/hello.sh

test035.answer: results mes-m2
	test/test035/hello.sh

test036.answer: results mes-m2
	test/test036/hello.sh

test037.answer: results mes-m2
	test/test037/hello.sh

test038.answer: results mes-m2
	test/test038/hello.sh

test039.answer: results mes-m2
	test/test039/hello.sh

test040.answer: results mes-m2
	test/test040/hello.sh

test041.answer: results mes-m2
	test/test041/hello.sh

test042.answer: results mes-m2
	test/test042/hello.sh

test043.answer: results mes-m2
	test/test043/hello.sh

test044.answer: results mes-m2
	test/test044/hello.sh

test045.answer: results mes-m2
	test/test045/hello.sh

test046.answer: results mes-m2
	test/test046/hello.sh

test047.answer: results mes-m2
	test/test047/hello.sh

test048.answer: results mes-m2
	test/test048/hello.sh

test049.answer: results mes-m2
	test/test049/hello.sh

test050.answer: results mes-m2
	test/test050/hello.sh

test051.answer: results mes-m2
	test/test051/hello.sh

test052.answer: results mes-m2
	test/test052/hello.sh

test053.answer: results mes-m2
	test/test053/hello.sh

test054.answer: results mes-m2
	test/test054/hello.sh

test055.answer: results mes-m2
	test/test055/hello.sh

test056.answer: results mes-m2
	test/test056/hello.sh

test057.answer: results mes-m2
	test/test057/hello.sh

test058.answer: results mes-m2
	test/test058/hello.sh

test059.answer: results mes-m2
	test/test059/hello.sh

test060.answer: results mes-m2
	test/test060/hello.sh

test061.answer: results mes-m2
	test/test061/hello.sh

test062.answer: results mes-m2
	test/test062/hello.sh

test063.answer: results mes-m2
	test/test063/hello.sh

test064.answer: results mes-m2
	test/test064/hello.sh

test065.answer: results mes-m2
	test/test065/hello.sh

test066.answer: results mes-m2
	test/test066/hello.sh

test067.answer: results mes-m2
	test/test067/hello.sh

test068.answer: results mes-m2
	test/test068/hello.sh

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
install: mes-m2-boot
	mkdir -p $(bindir)
	cp bin/mes $(bindir)
