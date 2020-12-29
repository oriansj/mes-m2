mes-m2
======

Making Mes.c M2-Planet friendly

Goal
----

The goal of this project is to create a re-implementation of GNU Mes, which can be compiled by M2-Planet
and extends the feature set of GNU mes to provide a replacement guile to be used for the bootstrap.

The goal of this project is not to provide a minimally-adapted GNU Mes which can be compiled by M2-Planet
and is able to compile the real GNU Mes (and thereby run mescc). If you are interested in this project,
have a look at the [wip-m2 branch](https://git.savannah.gnu.org/cgit/mes.git/log/?h=wip-m2) in
Savannah's GNU Mes repository. That branch is work in progress and can get rebased without further notice.

Status
------

At the moment, the project is quite far away from its goal.

It is possible to load a minimal Scheme environment and REPL by running

```ShellSession
$ make
$ bin/m2
````

Which can be used to test the Scheme implementation (you will most likely run into bugs when doing so).

Tests can be run by `make test` and the M2-Planet build can be tested via `kaem --verbose --strict`.
Note that this only works when you are on amd64 architecture, and mescc-tools are in your `$PATH`.
If you are e.g. on x86, you can use the scripts from the [mescc-tools-seed](../mescc-tools-seed) repo to build mes-m2 with M2-Planet.

MesCC from the original GNU Mes project is also able to compile mes-m2.

The "original" Mes scheme environment is available if you set `MES_PREFIX=mes`:

```ShellSession
$ MES_DEBUG=100 MES_PREFIX=mes bin/m2
```

It is able to load modules, and will auto-load many modules (which you see loading when `MES_DEBUG` is set),
but will eventually end with either a segfault or an undefined symbol. (Currently it ends with an undefined symbol).

Note that the macro implementation and quasiquote expansion is known to be buggy, so an undefined symbol does
not necessarily mean an error in the Scheme code. If you take a look at the file and the code looks just fine,
probably check if you can create a minimal example that uses the same constructs and also shows the same behaviour.
(Replacing quasiquotes by their "expanded form" may also help pinpoint the issue).

For really low-level debugging you can export `MES_CORE=0` to interact with C primitive version only (and if you don't
build with M2-Planet, you can debug with `gdb`/`cgdb`).

Once you got the full MES REPL working, you can try running mescc like this

```ShellSession
$ MES_PREFIX=mes bin/mes-m2 -s scripts/mescc.scm -- -I include -S scaffold/exit-42.c -o foo.S
```

You can also compare the execution with guile's:

```ShellSession
$guile --no-auto-compile -L module -e main scripts/mescc.scm -I include -S scaffold/exit-42.c -o foo.S
```

to compile using the MesCC and nyacc included here.
