/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019,2020,2021 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#ifndef __MES_MES_H
#define __MES_MES_H

#include "cc.h"
#include "globals.h"
#include <stdlib.h>
#include <stdio.h>

struct scm
{
  long type;
  union
  {
    struct scm *car;
    long car_value;
    char *bytes;
    long length;
    struct scm *ref;
    struct scm *variable;
    struct scm *macro;
    long port;
    FILE* name_car;
  };
  union
  {
    struct scm *cdr;
    long cdr_value;
    struct scm *closure;
    struct scm *continuation;
    char *name;
    struct scm *string;
    struct scm *structure;
    long value;
    FUNCTION function;
    struct scm *vector;
    FILE* name_cdr;
  };
};

/* mes */
char *g_datadir;
int g_debug;
char *g_buf;
int g_continuations;
struct scm *g_symbols;
struct scm *g_symbol_max;
int g_mini;

/* a/env */
struct scm *R0;
/* param 1 */
struct scm *R1;
/* save 2 */
struct scm *R2;
/* continuation */
struct scm *R3;
/* current-module */
struct scm *M0;
/* macro */
struct scm *g_macros;
struct scm *g_ports;

/* gc */
size_t ARENA_SIZE;
size_t MAX_ARENA_SIZE;
size_t STACK_SIZE;
size_t JAM_SIZE;
size_t GC_SAFETY;
size_t MAX_STRING;
char *g_arena;
struct scm *cell_arena;
struct scm *cell_zero;

struct scm *g_free;
struct scm *g_symbol;

struct scm **g_stack_array;
struct scm *g_cells;
struct scm *g_news;
long g_stack;
size_t gc_count;
struct timespec *gc_start_time;
struct timespec *gc_end_time;
size_t gc_time;

char **__execl_c_argv;
char *__open_boot_buf;
char *__open_boot_file_name;
char *__setenv_buf;
char *__reader_read_char_buf;
struct timespec *g_start_time;
struct timeval *__gettimeofday_time;
struct timespec *__get_internal_run_time_ts;

struct scm *cast_charp_to_scmp (char const *i);
struct scm **cast_charp_to_scmpp (char const *i);
char *cast_voidp_to_charp (void const *i);
long cast_scmp_to_long (struct scm *i);
char *cast_scmp_to_charp (struct scm *i);

struct scm *alloc (long n);
struct scm *apply (struct scm *f, struct scm *x, struct scm *a);
struct scm *apply_builtin (struct scm *fn, struct scm *x);
struct scm *apply_builtin0 (struct scm *fn);
struct scm *apply_builtin1 (struct scm *fn, struct scm *x);
struct scm *apply_builtin2 (struct scm *fn, struct scm *x, struct scm *y);
struct scm *apply_builtin3 (struct scm *fn, struct scm *x, struct scm *y, struct scm *z);
struct scm *builtin_name (struct scm *builtin);
struct scm *cstring_to_list (char const *s);
struct scm *cstring_to_symbol (char const *s);
struct scm *cell_ref (struct scm *cell, long index);
struct scm *fdisplay_ (struct scm *, FILE*, int);
struct scm *init_symbols ();
struct scm *init_time (struct scm *a);
struct scm *make_builtin_type ();
struct scm *make_bytes (char const *s, size_t length);
struct scm *make_cell (long type, struct scm *car, struct scm *cdr);
struct scm *make_pointer_cell (long type, long car, void *cdr);
struct scm *make_value_cell (long type, long car, long cdr);
struct scm *make_char (int n);
struct scm *make_continuation (long n);
struct scm *make_file (FILE* f);
struct scm *make_function (void* n);
struct scm *make_hash_table_ (long size);
struct scm *make_hashq_type ();
struct scm *make_initial_module (struct scm *a);
struct scm *make_macro (struct scm *name, struct scm *x);
struct scm *make_number (long n);
struct scm *make_ref (struct scm *x);
struct scm *make_string (char const *s, size_t length);
struct scm *make_string0 (char const *s);
struct scm *make_string_port (struct scm *x);
struct scm *make_vector_ (long k, struct scm *e);
struct scm *mes_builtins (struct scm *a);
int in_set(int c, char* s);
int peek(FILE* f);
struct scm *push_cc (struct scm *p1, struct scm *p2, struct scm *a, struct scm *c);
struct scm *struct_ref_ (struct scm *x, long i);
struct scm *struct_set_x_ (struct scm *x, long i, struct scm *e);
struct scm *vector_ref_ (struct scm *x, long i);
struct scm *vector_set_x_ (struct scm *x, long i, struct scm *e);
FUNCTION builtin_function (struct scm *builtin);
char *cell_bytes (struct scm *x);
char *news_bytes (struct scm *x);
long gc_free ();
long length__ (struct scm *x);
size_t bytes_cells (size_t length);
void assert_max_string (size_t i, char const *msg, char const *string);
void assert_msg (int check, char *msg);
void assert_number (char const *name, struct scm *x);
void copy_cell (struct scm *to, struct scm *from);
void gc_ ();
void gc_dump_arena (struct scm *cells, long size);
void gc_init ();
void gc_peek_frame ();
void gc_pop_frame ();
void gc_push_frame ();
void gc_stats_ (char const* where);
void init_symbols_ ();
long seconds_and_nanoseconds_to_long (long s, long ns);

#include "builtins.h"
#include "constants.h"
#include "symbols.h"

#endif /* __MES_MES_H */
