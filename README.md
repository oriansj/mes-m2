# mes-m2
Making Mes.c M2-Planet friendly

build with make
test with make test
or build with M2-Planet via: kaem --verbose --strict

run: export MES_CORE=0 to interact with C primitive version only

as MesCC doesn't currently run on mes-m2; you will need to use a command like:
guile --no-auto-compile -L module -e main scripts/mescc.scm -I include -S scaffold/exit-42.c -o foo.S

to compile using the MesCC and nyacc included here.

*HOWEVER* at this time, despite the fact that mes-m2 can be built by M2-Planet and that MesCC enables one to build GCC.

MesCC is not able at this time to build mes-m2.
