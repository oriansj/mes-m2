/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <stdlib.h>
#include <string.h>

#define M2_CELL_SIZE 1
// CONSTANT M2_CELL_SIZE 12

int g_debug;

void
test_setup ()
{
  cell_arena = g_arena;
  g_cells = cell_arena + M2_CELL_SIZE;
  cell_zero = g_cells;

  cell_nil = cell_zero + M2_CELL_SIZE;
  cell_f = cell_nil + M2_CELL_SIZE;
  g_symbols = cell_zero;
  g_symbol_max = cell_zero + (9 * M2_CELL_SIZE);
  g_ports = cell_zero;
  g_macros = cell_zero;
  g_stack = STACK_SIZE;
  M0 = cell_zero;

  memset (g_arena + sizeof (struct scm), 0, ARENA_SIZE * sizeof (struct scm));
  cell_zero->type = TCHAR;
  cell_zero->value = 'c';
  g_free = cell_f;
}

void
print_arena (long length)
{
  struct scm *v = cell_arena;
  v->type = TVECTOR;
  v->length = length;
  eputs ("arena["); eputs (ntoab (g_cells, 16, 0)); eputs ("]: "); write_ (v); eputs ("\n");
}

void
test_gc (char const *name)
{
  eputs ("TEST: "); eputs (name); eputs ("\n");
  print_arena (gc_free () - 1);
  gc_stats_ ("0");
  gc_ ();
  print_arena (gc_free () - 1);
  gc_stats_ ("1");
  eputs ("\n");

  gc_ ();
  cell_zero->value = 'd';
  print_arena (gc_free () - 1);
  gc_stats_ ("2");
  eputs ("\n");

  gc_ ();
  cell_zero->value = 'e';
  print_arena (gc_free () - 1);
  gc_stats_ ("3");
  eputs ("\n");

  gc_ ();
  cell_zero->value = 'f';
  print_arena (gc_free () - 1);
  gc_stats_ ("3");
  eputs ("\n");
}

void
test_empty ()
{
  test_setup ();

  g_free = g_symbol_max;
  test_gc ("empty");
}

void
test_number ()
{
  test_setup ();
  make_number (42);

  g_free = g_symbol_max + M2_CELL_SIZE;
  test_gc ("number");
}

void
test_cons ()
{
  test_setup ();
  struct scm *a = make_number (42);
  struct scm *d = make_number (101);
  cons (a, d);

  g_free = g_symbol_max + M2_CELL_SIZE;
  test_gc ("cons");
}

void
test_list ()
{
  test_setup ();
  struct scm *a = make_number (42);
  struct scm *d = make_number (101);
  struct scm *lst = cons (d, cell_nil);
  cons (a, lst);

  g_free = g_symbol_max + M2_CELL_SIZE;
  test_gc ("cons");
}

void
test_string ()
{
  test_setup ();
  struct scm *s = make_string0 ("hello");

  g_free = g_symbol_max + M2_CELL_SIZE;
  test_gc ("string");
}

void
test_vector ()
{
  test_setup ();
  struct scm *v = make_vector_ (4, cell_zero);
  struct scm *one = make_number (1);
  struct scm *two = make_number (2);
  vector_set_x_ (v, 1, one);
  vector_set_x_ (v, 2, two);

  g_free = g_symbol_max + M2_CELL_SIZE;
  test_gc ("vector");
}

void
test_struct ()
{
  test_setup ();
  struct scm *type = make_char ('t');
  struct scm *printer = make_char ('p');
  struct scm *fields = cons (make_char ('f'), cell_nil);
  make_struct (type, fields, printer);

  g_free = g_symbol_max + M2_CELL_SIZE;
  test_gc ("struct");
}

int
main (int argc, char **argv, char **envp)
{
  setenv ("MES_ARENA", "100", 1);
  setenv ("MES_MAX_ARENA", "100", 1);
  gc_init ();
  cell_zero = g_cells;
  cell_nil = cell_zero + M2_CELL_SIZE;
  cell_f = cell_nil + M2_CELL_SIZE;
  g_symbols = cell_zero;
  g_symbol_max = cell_zero + (9 * M2_CELL_SIZE);
  g_ports = cell_zero;
  g_macros = cell_zero;
  g_stack = STACK_SIZE;
  M0 = cell_zero;

  test_empty ();
  test_number ();
  test_cons ();
  test_list ();
  test_string ();
  test_vector ();
  test_struct ();

  return 0;
}
