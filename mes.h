/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of GNU Mes.
 *
 * GNU Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * GNU Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <assert.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// CONSTANT STDIN 0
#define STDIN 0
// CONSTANT STDOUT 1
#define STDOUT 1

// CONSTANT STDERR 2
#define STDERR 2

char **environ;
int __stdin;
int __stdout;
int __stderr;

typedef long SCM;
SCM g_continuations;
SCM g_symbols;
SCM g_stack;
SCM *g_stack_array;
#define FRAME_SIZE 5
#define FRAME_PROCEDURE 4
// a/env
SCM r0;
// param 1
SCM r1;
// save 2
SCM r2;
// continuation
SCM r3;
// current-module
SCM m0;
// macro
SCM g_macros;
SCM g_ports;

typedef SCM (*function0_t) (void);
typedef SCM (*function1_t) (SCM);
typedef SCM (*function2_t) (SCM, SCM);
typedef SCM (*function3_t) (SCM, SCM, SCM);
typedef SCM (*functionn_t) (SCM);

struct scm
{
	long type;
	SCM car;
	SCM cdr;
};

struct scm *g_cells;
struct scm *g_news;
SCM vector_entry(SCM x);
long MAX_STRING;
char *g_buf;
long g_free;
int g_debug;
long GC_SAFETY;
long ARENA_SIZE;
long MAX_ARENA_SIZE;
long JAM_SIZE;
long STACK_SIZE;
SCM g_symbol_max;
