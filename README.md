# mes-m2
Making Mes.c M2-Planet friendly

C sanity test build (you need to be on x86 or have i686-unknown-linux-gnu-gcc*)

    ./build-x86-gcc.sh

Experimental 64-bit build (much less objdump mes-x86-compatible friendly...)

    ./build-gcc.sh

M2 build

    ./build.sh

or

    V=2 ./build.sh

The scaffold:

    scaffold/main.c
    scaffold/hello.c
    scaffold/malloc.c
    scaffold/micro-mes.c
    scaffold/milli-mes.c
    scaffold/tiny-mes.c
    scaffold/cons-mes.c
    scaffold/load-mes.c

these work, todo:

    scaffold/m.c
    scaffold/argv.c
    scaffold/mini-mes.c

Try:

    ./boot.sh scaffold/cons-mes
    V=2 ./boot.sh scaffold/mini-mes

Then, onto the target:

    src/mes.c

which includes

    src/*.c

and uses

    lib/libc.c

*) Use: cd mes && guix environment -l .guix.scm
   or guix environment --system=i686-linux --ad-hoc gcc-toolchain@5
