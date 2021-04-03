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

#include "mes/lib.h"
#include "mes/mes.h"

#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>

int g_dump_filedes;

#define M2_CELL_SIZE 1U
// CONSTANT M2_CELL_SIZE 12

char *
cell_bytes (struct scm *x)
{
  char *p = cast_voidp_to_charp (x);
  return p + (2 * sizeof (long));
}

char *
news_bytes (struct scm *x)
{
  char *p = cast_voidp_to_charp (x);
  return p + (2 * sizeof (long));
}

#define U10 10U
// CONSTANT U10 10
#define U100 100U
// CONSTANT U100 100
void
gc_init ()
{
#if SYSTEM_LIBC
  ARENA_SIZE = 100000000;       /* 2.3GiB */
#elif ! __M2_PLANET__
  ARENA_SIZE = 300000;          /* 32b: 3MiB, 64b: 6 MiB */
#else
  ARENA_SIZE = 20000000;
#endif
  STACK_SIZE = 20000;

  JAM_SIZE = 10;
  MAX_ARENA_SIZE = 10000000;
  GC_SAFETY = 2000;
  MAX_STRING = 524288;

  char *p;
  p = getenv ("MES_MAX_ARENA");
  if (p != 0)
    MAX_ARENA_SIZE = atoi (p);
  p = getenv ("MES_ARENA");
  if (p != 0)
    ARENA_SIZE = atoi (p);
  JAM_SIZE = ARENA_SIZE / U10;
  p = getenv ("MES_JAM");
  if (p != 0)
    JAM_SIZE = atoi (p);
  GC_SAFETY = ARENA_SIZE / U100;
  p = getenv ("MES_SAFETY");
  if (p != 0)
    GC_SAFETY = atoi (p);
  p = getenv ("MES_STACK");
  if (p != 0)
    STACK_SIZE = atoi (p);
  p = getenv ("MES_MAX_STRING");
  if (p != 0)
    MAX_STRING = atoi (p);

  long arena_bytes = (ARENA_SIZE + JAM_SIZE) * sizeof (struct scm);
  long alloc_bytes = arena_bytes + (STACK_SIZE * sizeof (struct scm));

  g_arena = malloc (alloc_bytes);
  g_cells = cast_charp_to_scmp (g_arena);
  g_stack_array = cast_charp_to_scmpp (g_arena + arena_bytes);

  /* The vector that holds the arenea. */
  cell_arena = g_cells;

  cell_zero = cell_arena + M2_CELL_SIZE;

  g_cells = g_cells + M2_CELL_SIZE; /* Hmm? */

  cell_arena->type = TVECTOR;
  cell_arena->length = 1000;
  cell_arena->vector = cell_zero;

  cell_zero->type = TCHAR;
  cell_zero->value = 'c';

  g_free = g_cells + M2_CELL_SIZE;

  /* FIXME: remove MES_MAX_STRING, grow dynamically. */
  g_buf = malloc (MAX_STRING);
}

long
gc_free ()
{
  return (g_free - g_cells) / M2_CELL_SIZE;
}

void
gc_stats_ (char const* where)
{
  size_t i = g_free - g_cells;
  i = i / M2_CELL_SIZE;
  if (where)
    {
      eputs (where);
      eputs (": ");
    }
  eputs ("[");
  eputs (ltoa (i));
  eputs ("]\n");
}

struct scm *
gc_stats ()
{
  gc_stats_ (0);
  size_t arena_used = g_free - g_cells;
  arena_used = arena_used / M2_CELL_SIZE;
  size_t arena_free = ARENA_SIZE - arena_used;
  struct scm *r = cell_nil;
  r =  acons (cstring_to_symbol ("gc-count"), make_number (gc_count), r);
  r =  acons (cstring_to_symbol ("gc-time"), make_number (gc_time), r);
  r =  acons (cstring_to_symbol ("arena-free"), make_number (arena_free), r);
  r =  acons (cstring_to_symbol ("arena-size"), make_number (ARENA_SIZE), r);
  return r;
}

struct scm *
alloc (long n)
{
  struct scm *x = g_free;
  g_free = g_free + (n * M2_CELL_SIZE);
  long i = g_free - g_cells;
  i = i / M2_CELL_SIZE;

  if (i > ARENA_SIZE)
    assert_msg (0, "alloc: out of memory");
  return x;
}

struct scm *
make_cell (long type, struct scm *car, struct scm *cdr)
{
  struct scm *x = g_free;
  g_free = g_free + M2_CELL_SIZE;
  long i = g_free - g_cells;
  i = i / M2_CELL_SIZE;
  if (i > ARENA_SIZE)
    assert_msg (0, "make_cell: out of memory");
  x->type = type;
  x->car = car;
  x->cdr = cdr;
  return x;
}

struct scm *
make_pointer_cell (long type, long car, void *cdr)
{
  struct scm *x = g_free;
  g_free = g_free + M2_CELL_SIZE;
  long i = g_free - g_cells;
  i = i / M2_CELL_SIZE;
  if (i > ARENA_SIZE)
    assert_msg (0, "make_pointer_cell: out of memory");
  x->type = type;
  x->length = car;
  x->cdr = cdr;
  return x;
}

struct scm *
make_value_cell (long type, long car, long cdr)
{
  struct scm *x = g_free;
  g_free = g_free + M2_CELL_SIZE;
  long i = g_free - g_cells;
  i = i / M2_CELL_SIZE;
  if (i > ARENA_SIZE)
    assert_msg (0, "make_value_cell: out of memory");
  x->type = type;
  x->length = car;
  x->value = cdr;
  return x;
}

void
copy_cell (struct scm *to, struct scm *from)
{
  to->type = from->type;
  to->car = from->car;
  to->cdr = from->cdr;
}

void
copy_news (struct scm *to, struct scm *from)
{
  to->type = from->type;
  to->car = from->car;
  to->cdr = from->cdr;
}

void
copy_stack (long index, struct scm *from)
{
  g_stack_array[index] = from;
}

struct scm *
cell_ref (struct scm *cell, long index)
{
  return cell + (index * M2_CELL_SIZE);
}

struct scm *
cons (struct scm *x, struct scm *y)
{
  return make_cell (TPAIR, x, y);
}

size_t
bytes_cells (size_t length)
{
  return (sizeof (long) + sizeof (long) + length - 1 + sizeof (struct scm *)) / sizeof (struct scm *);
}

struct scm *
make_bytes (char const *s, size_t length)
{
  size_t size = bytes_cells (length);
  struct scm *x = alloc (size);
  x->type = TBYTES;
  x->length = length;
  char *p = cell_bytes (x);
  if (length == 0)
    p[0] = 0;
  else
    memcpy (p, s, length);

  return x;
}

struct scm *
make_char (int n)
{
  return make_value_cell (TCHAR, 0, n);
}

struct scm *
make_continuation (long n)
{
  return make_value_cell (TCONTINUATION, n, g_stack);
}

struct scm *
make_macro (struct scm *name, struct scm *x)    /*:((internal)) */
{
  return make_cell (TMACRO, x, name->string);
}

struct scm *
make_number (long n)
{
  return make_value_cell (TNUMBER, 0, n);
}

struct scm *
make_ref (struct scm *x)                /*:((internal)) */
{
  return make_cell (TREF, x, 0);
}

struct scm *
make_string (char const *s, size_t length)
{
  if (length > MAX_STRING)
    assert_max_string (length, "make_string", s);
  struct scm *x = make_pointer_cell (TSTRING, length, 0);
  struct scm *v = make_bytes (s, length + 1);
  x->cdr = v;
  return x;
}

struct scm *
make_string0 (char const *s)
{
  return make_string (s, strlen (s));
}

struct scm *
make_string_port (struct scm *x)        /*:((internal)) */
{
  return make_pointer_cell (TPORT, -length__ (g_ports) - 2, x);
}

void
gc_init_news ()
{
  g_news = g_free;
  struct scm *ncell_arena = g_news;
  struct scm *ncell_zero = ncell_arena + M2_CELL_SIZE;

  g_news = g_news + M2_CELL_SIZE;

  ncell_arena->type = TVECTOR;
  ncell_arena->length = cell_arena->length;
  ncell_arena->vector = g_news;

  ncell_zero->type = TCHAR;
  ncell_zero->value = 'n';
}

void
gc_up_arena ()
{
  long old_arena_bytes = (ARENA_SIZE + JAM_SIZE) * sizeof (struct scm);
  if (ARENA_SIZE >> 1 < MAX_ARENA_SIZE >> 2)
    {
      ARENA_SIZE = ARENA_SIZE << 1;
      JAM_SIZE = JAM_SIZE << 1;
      GC_SAFETY = GC_SAFETY << 1;
    }
  else
    ARENA_SIZE = MAX_ARENA_SIZE - JAM_SIZE;
  long arena_bytes = (ARENA_SIZE + JAM_SIZE) * sizeof (struct scm);
  long stack_offset = (arena_bytes * 2);
  long realloc_bytes = (arena_bytes * 2) + (STACK_SIZE * sizeof (struct scm));
  void *p = realloc (g_cells - M2_CELL_SIZE, realloc_bytes);
  if (p == 0)
    {
      eputs ("realloc failed, g_free=");
      eputs (ltoa (cast_voidp_to_long (g_free)));
      eputs (":");
      long i = g_free - g_cells;
      i = i / M2_CELL_SIZE;
      eputs (ltoa (ARENA_SIZE - i));
      eputs ("\n");
      assert_msg (0, "0");
      exit (1);
    }
  g_cells = p;
  memcpy (p + stack_offset, p + old_arena_bytes, STACK_SIZE * sizeof (struct scm *));
  g_cells = g_cells + M2_CELL_SIZE;
}

/* A pointer relocating memcpy for pointer cells to avoid using only
   half of the allocated cells.

   For number based cells a simply memcpy could be used, as number
   references are relative.

   A simple stop and copy (SICP 5.3) garbage collector allocates twice
   the cell arena size only for the garbage collector.  The garbage
   collector switches back and forth between cells and news, thus
   utilizing only half the allocated memory. */
void
gc_cellcpy (struct scm *dest, struct scm *src, size_t n)
{
  void *p = src;
  void *q = dest;
  long dist = p - q;
  long t;
  long a;
  long d;
  int i;
  int c;
  while (n != 0)
    {
      t = src->type;
      a = src->car_value;
      d = src->cdr_value;
      dest->type = t;
      if (t == TBROKEN_HEART)
        assert_msg (0, "gc_cellcpy: broken heart");
      if (t == TMACRO
          || t == TPAIR
          || t == TREF
          || t == TVARIABLE)
        dest->car_value = a - dist;
      else
        dest->car_value = a;
      if (t == TBYTES
          || t == TCLOSURE
          || t == TCONTINUATION
          || t == TKEYWORD
          || t == TMACRO
          || t == TPAIR
          || t == TPORT
          || t == TSPECIAL
          || t == TSTRING
          || t == TSTRUCT
          || t == TSYMBOL
          || t == TVALUES
          || t == TVECTOR)
        dest->cdr_value = d - dist;
      else
        dest->cdr_value = d;
      if (t == TBYTES)
        {
          if (g_debug > 5)
            {
              eputs ("copying bytes[");
              eputs (ntoab (cast_voidp_to_long (cell_bytes (src)), 16, 0));
              eputs (", ");
              eputs (ntoab (a, 10, 0));
              eputs ("]: ");
              eputs (cell_bytes (src));
              eputs ("\n to [");
              eputs (ntoab (cast_voidp_to_long (cell_bytes (dest)), 16, 0));
            }
          memcpy (cell_bytes (dest), cell_bytes (src), a);
          if (g_debug > 5)
            {
              eputs ("]: ");
              eputs (cell_bytes (dest));
              eputs ("\n");
            }
          i = bytes_cells (a);
          n = n - i;
          c = i * M2_CELL_SIZE;
          dest = dest + c;
          src = src + c;
        }
      else
        {
          n = n - 1;
          dest = dest + M2_CELL_SIZE;
          src = src + M2_CELL_SIZE;
        }
    }
}

/* We do not actually flip cells and news, instead we move news back to
   cells. */
void
gc_flip ()
{
  if (g_free - g_news > JAM_SIZE)
    JAM_SIZE = (g_free - g_news) + ((g_free - g_news) / 2);

  cell_arena = g_cells - M2_CELL_SIZE; /* For debugging. */
  gc_cellcpy (g_cells, g_news, (g_free - g_news) / M2_CELL_SIZE);

  long dist = g_news - g_cells;
  g_free = g_free - dist;
  g_symbols = g_symbols - dist;
  g_macros = g_macros - dist;
  g_ports = g_ports - dist;
  M0 = M0 - dist;

  long i;
  for (i = g_stack; i < STACK_SIZE; i = i + 1)
    g_stack_array[i] = g_stack_array[i] - dist;

  if (g_debug > 2)
    gc_stats_ (";;; => jam");
}

struct scm *
gc_copy (struct scm *old)               /*:((internal)) */
{
  if (old->type == TBROKEN_HEART)
    return old->car;
  struct scm *new = g_free;
  g_free = g_free + M2_CELL_SIZE;
  copy_news (new, old);
  if (new->type == TSTRUCT || new->type == TVECTOR)
    {
      new->vector = g_free;
      long i;
      for (i = 0; i < old->length; i = i + 1)
        {
          copy_news (g_free, cell_ref (old->vector, i));
          g_free = g_free + M2_CELL_SIZE;
        }
    }
  else if (new->type == TBYTES)
    {
      char const *src = cell_bytes (old);
      char *dest = news_bytes (new);
      size_t length = new->length;
      memcpy (dest, src, length);
      g_free = g_free + ((bytes_cells (length) - 1) * M2_CELL_SIZE);

      if (g_debug > 4)
        {
          eputs ("gc copy bytes: ");
          eputs (src);
          eputs ("\n");
          eputs ("    length: ");
          eputs (ltoa (old->length));
          eputs ("\n");
          eputs ("    nlength: ");
          eputs (ltoa (new->length));
          eputs ("\n");
          eputs ("        ==> ");
          eputs (dest);
          eputs ("\n");
        }
    }
  old->type = TBROKEN_HEART;
  old->car = new;
  return new;
}

struct scm *
gc_relocate_car (struct scm *new, struct scm *car)      /*:((internal)) */
{
  new->car = car;
  return cell_unspecified;
}

struct scm *
gc_relocate_cdr (struct scm *new, struct scm *cdr)      /*:((internal)) */
{
  new->cdr = cdr;
  return cell_unspecified;
}

void
gc_loop (struct scm *scan)
{
  struct scm *car;
  struct scm *cdr;
  long t;
  while (scan < g_free)
    {
      t = scan->type;
      if (t == TBROKEN_HEART)
        assert_msg (0, "gc_loop: broken heart");
      /* *INDENT-OFF* */
      if (t == TMACRO
          || t == TPAIR
          || t == TREF
          || t == TVARIABLE)
        /* *INDENT-ON* */
        {
          car = gc_copy (scan->car);
          gc_relocate_car (scan, car);
        }
      /* *INDENT-OFF* */
      if (t == TCLOSURE
          || t == TCONTINUATION
          || t == TKEYWORD
          || t == TMACRO
          || t == TPAIR
          || t == TPORT
          || t == TSPECIAL
          || t == TSTRING
          /*|| t == TSTRUCT handled by gc_copy */
          || t == TSYMBOL
          || t == TVALUES
          /*|| t == TVECTOR handled by gc_copy */
          )
        /* *INDENT-ON* */
        {
          cdr = gc_copy (scan->cdr);
          gc_relocate_cdr (scan, cdr);
        }
      if (t == TBYTES)
        scan = scan + (bytes_cells (scan->length) * M2_CELL_SIZE);
      else
        scan = scan + M2_CELL_SIZE;
    }
  gc_flip ();
}

struct scm *
gc_check ()
{
  long used = ((g_free - g_cells) / M2_CELL_SIZE) + GC_SAFETY;
  if (used >= ARENA_SIZE)
    return gc ();
  return cell_unspecified;
}

void
gc_ ()
{
  gc_init_news ();
  if (g_debug == 2)
    eputs (".");
  if (g_debug > 2)
    {
      gc_stats_ (";;; gc");
      eputs (";;; free: [");
      eputs (ltoa (ARENA_SIZE - gc_free ()));
      eputs ("]...");
    }
  g_free = g_news + M2_CELL_SIZE;

  if (ARENA_SIZE < MAX_ARENA_SIZE
      && cast_voidp_to_charp (g_cells) == g_arena + M2_CELL_SIZE)
    {
      if (g_debug == 2)
        eputs ("+");
      if (g_debug > 2)
        {
          eputs (" up[");
          eputs (ltoa (cast_voidp_to_long (g_cells)));
          eputs (",");
          eputs (ltoa (cast_voidp_to_long (g_news)));
          eputs (":");
          eputs (ltoa (ARENA_SIZE));
          eputs (",");
          eputs (ltoa (MAX_ARENA_SIZE));
          eputs ("]...");
        }
      gc_up_arena ();
    }

  struct scm *new_cell_nil = g_free;
  struct scm *s;
  for (s = cell_nil; s < g_symbol_max; s = s + M2_CELL_SIZE)
    gc_copy (s);

  g_symbols = gc_copy (g_symbols);
  g_macros = gc_copy (g_macros);
  g_ports = gc_copy (g_ports);
  M0 = gc_copy (M0);

  long i;
  for (i = g_stack; i < STACK_SIZE; i = i + 1)
    copy_stack (i, gc_copy (g_stack_array[i]));

  gc_loop (new_cell_nil);
}

struct scm *
gc ()
{
  if (getenv ("MES_DUMP") != 0)
    gc_dump_arena (g_cells, gc_free ());
  if (g_debug > 5)
    {
      eputs ("symbols: ");
      write_error_ (g_symbols);
      eputs ("\n");
      eputs ("R0: ");
      write_error_ (R0);
      eputs ("\n");
    }
  clock_gettime (CLOCK_PROCESS_CPUTIME_ID, gc_start_time);
  gc_push_frame ();
  gc_ ();
  gc_pop_frame ();
  clock_gettime (CLOCK_PROCESS_CPUTIME_ID, gc_end_time);
  long time = seconds_and_nanoseconds_to_long
    (gc_end_time->tv_sec - gc_start_time->tv_sec,
     gc_end_time->tv_nsec - gc_start_time->tv_nsec);
  gc_time = gc_time + time;
  gc_count = gc_count + 1;
  if (g_debug > 5)
    {
      eputs ("symbols: ");
      write_error_ (g_symbols);
      eputs ("\n");
      eputs ("R0: ");
      write_error_ (R0);
      eputs ("\n");
    }
  if (getenv ("MES_DUMP") != 0)
    gc_dump_arena (g_cells, gc_free ());
  return cell_unspecified;
}

void
gc_push_frame ()
{
  if (g_stack < FRAME_SIZE)
    assert_msg (0, "STACK FULL");
  g_stack_array[g_stack - 1] = cell_f;
  g_stack_array[g_stack - 2] = R0;
  g_stack_array[g_stack - 3] = R1;
  g_stack_array[g_stack - 4] = R2;
  g_stack_array[g_stack - 5] = R3;
  g_stack = g_stack - FRAME_SIZE;
}

void
gc_peek_frame ()
{
  R3 = g_stack_array[g_stack];
  R2 = g_stack_array[g_stack + 1];
  R1 = g_stack_array[g_stack + 2];
  R0 = g_stack_array[g_stack + 3];
  g_stack_array[g_stack + FRAME_PROCEDURE];
}

void
gc_pop_frame ()
{
  gc_peek_frame ();
  g_stack = g_stack + FRAME_SIZE;
}

void
dumpc (char c)
{
  fdputc (c, g_dump_filedes);
}

void
dumps (char const *s)
{
  fdputs (s, g_dump_filedes);
}

void
gc_dump_register (char const* n, struct scm *r)
{
  dumps (n); dumps (": ");
  long i = cast_scmp_to_long (r);
  long a = cast_charp_to_long (g_arena);
  i = i - a;
  i = i / M2_CELL_SIZE;
  dumps (ltoa (i));
  dumps ("\n");
}

void
gc_dump_state ()
{
  gc_dump_register ("R0", R0);
  gc_dump_register ("R1", R1);
  gc_dump_register ("R2", R2);
  gc_dump_register ("R3", R3);
  gc_dump_register ("M0", M0);
  gc_dump_register ("g_symbols", g_symbols);
  gc_dump_register ("g_symbol_max", g_symbol_max);
  gc_dump_register ("g_macros", g_macros);
  gc_dump_register ("g_ports", g_ports);
  gc_dump_register ("cell_zero", cell_zero);
  gc_dump_register ("cell_nil", cell_nil);
}

void
gc_dump_stack ()
{
  long i = g_stack;
  while (i < STACK_SIZE)
    {
      gc_dump_register (itoa (i), g_stack_array[i]);
      i = i + 1;
    }
}

void
gc_dump_arena (struct scm *cells, long size)
{
  struct scm *end = g_cells + (size * M2_CELL_SIZE);
  struct scm *dist = cells;
  int i;
  long t;
  long a;
  long d;
  int c;
  char* p;
  if (g_dump_filedes == 0)
    g_dump_filedes = mes_open ("dump.mo", O_CREAT|O_WRONLY, 0644);
  dumps ("stack="); dumps (ltoa (g_stack)); dumpc ('\n');
  dumps ("size="); dumps (ltoa (size)); dumpc ('\n');
  gc_dump_state ();
  gc_dump_stack ();
  while (end->type == 0 && end->car == 0 && end->cdr == 0)
    {
      end = end - M2_CELL_SIZE;
      size = size - 1;
    }
  while (size > 0)
    {
      for (i=0; i < 16; i = i + 1)
        {
          t = cells->type;
          a = cells->car_value;
          d = cells->cdr_value;
          if (size == 0)
            dumps ("0 0 0");
          else
            {
              dumps (ltoa (t));
              dumpc (' ');
              if (t == TMACRO
                  || t == TPAIR
                  || t == TREF
                  || t == TVARIABLE)
                {
                  dumps (ltoa ((cells->car - dist) / M2_CELL_SIZE));
                  /* dumps ("["); dumps (ltoa (a)); dumps ("]"); */
                }
              else
                dumps (ltoa (a));
              dumpc (' ');
              if (t != TBYTES)
                {
                  if (t == TCLOSURE
                      || t == TCONTINUATION
                      || t == TKEYWORD
                      || t == TMACRO
                      || t == TPAIR
                      || t == TPORT
                      || t == TSPECIAL
                      || t == TSTRING
                      || t == TSTRUCT
                      || t == TSYMBOL
                      || t == TVALUES
                      || t == TVECTOR)
                    {
                      dumps (ltoa ((cells->cdr - dist) / M2_CELL_SIZE));
                      /* dumps ("["); dumps (ltoa (d)); dumps ("]"); */
                    }
                  else if (t == TNUMBER && d > 1000)
                    dumps (ltoa (1001));
                  else
                    dumps (ltoa (d));
                }
              if (t == TBYTES)
                {
                  c = bytes_cells (a);
                  p = cell_bytes (cells);
                  size = size - c;
                  dumpc ('"');
                  while (a > 0)
                    {
                      if (p[0] != 0)
                        dumpc (p[0]);
                      p = p + 1;
                      a = a - 1;
                    }
                  dumpc ('"');
                  cells = cells + c * M2_CELL_SIZE;
                  size = size - c;
                }
              else
                {
                  cells = cells + M2_CELL_SIZE;
                  size = size - 1;
                }
            }
          if (i != 15)
            dumps ("  ");
          else
            dumpc ('\n');
        }
      dumpc ('\n');
    }
}
