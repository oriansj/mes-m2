mes-m2
======

Making Mes.c M2-Planet friendly

Goal
----

The goal of this project is to provide a minimally-adapted GNU Mes which can be compiled by M2-Planet
and is able to compile the real GNU Mes (and thereby run mescc).

Status
------

Completed for x86 but porting to M2libc to accelerate architecture porting is not done.

Just run:

$ make mes-m2-boot

You can try running mescc like this

```ShellSession
$ ./bin/mes --no-auto-compile -L module -e main scripts/mescc.scm -I include -v -S scaffold/exit-42.c -o foo.S
```

You can also compare the execution with guile's:

```ShellSession
$ guile --no-auto-compile -L module -e main scripts/mescc.scm -I include -v -S scaffold/exit-42.c -o foo.S
```

to compile using the MesCC and nyacc included here.
