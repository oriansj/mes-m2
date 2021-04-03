/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

/** Commentary:
    Essential functions, used by the eval/apply core.
 */

/** Code: */

#include "mes/lib.h"
#include "mes/mes.h"

#include <stdlib.h>

struct scm *
assoc_string (struct scm *x, struct scm *a)     /*:((internal)) */
{
  struct scm *b;
  while (a != cell_nil)
    {
      b = a->car;
      if (b->car->type == TSTRING)
        if (string_equal_p (x, b->car) == cell_t)
          return b;
      a = a->cdr;
    }
  if (a != cell_nil)
    return a->car;
  return cell_f;
}

struct scm *
car (struct scm *x)
{
#if !__MESC_MES__
  if (x->type != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cell_symbol_car));
#endif
  return x->car;
}

struct scm *
cdr (struct scm *x)
{
#if !__MESC_MES__
  if (x->type != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cell_symbol_cdr));
#endif
  return x->cdr;
}

struct scm *
list (struct scm *x)                    /*:((arity . n)) */
{
  return x;
}

struct scm *
null_p (struct scm *x)
{
  if (x == cell_nil)
    return cell_t;
  return cell_f;
}

struct scm *
eq_p (struct scm *x, struct scm *y)
{
  if (x == y)
    return cell_t;
  int t = x->type;
  if (t == TKEYWORD)
    {
      if (y->type == TKEYWORD)
        return string_equal_p (x, y);
      return cell_f;
    }
  if (t == TCHAR)
    {
      if (y->type != TCHAR)
        return cell_f;
      if (x->value == y->value)
        return cell_t;
      return cell_f;
    }
  if (t == TNUMBER)
    {
      if (y->type != TNUMBER)
        return cell_f;
      if (x->value == y->value)
        return cell_t;
      return cell_f;
    }
  return cell_f;
}

struct scm *
values (struct scm *x)                  /*:((arity . n)) */
{
  struct scm *v = cons (0, x);
  v->type = TVALUES;
  return v;
}

struct scm *
acons (struct scm *key, struct scm *value, struct scm *alist)
{
  return cons (cons (key, value), alist);
}

long
length__ (struct scm *x)                /*:((internal)) */
{
  long n = 0;
  while (x != cell_nil)
    {
      n = n + 1;
      if (x->type != TPAIR)
        return -1;
      x = x->cdr;
    }
  return n;
}

struct scm *
length (struct scm *x)
{
  return make_number (length__ (x));
}

struct scm *
error (struct scm *key, struct scm *x)
{
#if !__MESC_MES__ && !__M2_PLANET__
  struct scm *throw = module_ref (R0, cell_symbol_throw);
  if (throw != cell_undefined)
    return apply (throw, cons (key, cons (x, cell_nil)), R0);
#endif
  display_error_ (key);
  eputs (": ");
  write_error_ (x);
  eputs ("\n");
  assert_msg (0, "ERROR");
  exit (1);
}

struct scm *
append2 (struct scm *x, struct scm *y)
{
  if (x == cell_nil)
    return y;
  if (x->type != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cstring_to_symbol ("append2")));
  struct scm *r = cell_nil;
  while (x != cell_nil)
    {
      r = cons (x->car, r);
      x = x->cdr;
    }
  return reverse_x_ (r, y);
}

struct scm *
append_reverse (struct scm *x, struct scm *y)
{
  if (x == cell_nil)
    return y;
  if (x->type != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cstring_to_symbol ("append-reverse")));
  while (x != cell_nil)
    {
      y = cons (x->car, y);
      x = x->cdr;
    }
  return y;
}

struct scm *
reverse_x_ (struct scm *x, struct scm *t)
{
  if (x != cell_nil && x->type != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cstring_to_symbol ("core:reverse!")));
  struct scm *r = t;
  while (x != cell_nil)
    {
      t = x->cdr;
      x->cdr = r;
      r = x;
      x = t;
    }
  return r;
}

struct scm *
assq (struct scm *x, struct scm *a)
{
  if (a->type != TPAIR)
    return cell_f;
  int t = x->type;

  if (t == TSYMBOL || t == TSPECIAL)
    while (a != cell_nil)
      {
        if (x == a->car->car)
          return a->car;
        a = a->cdr;
      }
  else if (t == TCHAR || t == TNUMBER)
    {
      long v = x->value;
      while (a != cell_nil)
        {
          if (v == a->car->car->value)
            return a->car;
          a = a->cdr;
        }
    }
  else if (t == TKEYWORD)
    {
      while (a != cell_nil)
        {
          if (string_equal_p (x, a->car->car) == cell_t)
            return a->car;
          a = a->cdr;
        }
    }
  else
    /* pointer equality, e.g. on strings. */
    while (a != cell_nil)
      {
        if (x == a->car->car)
          return a->car;
        a = a->cdr;
      }
  return cell_f;
}

struct scm *
assoc (struct scm *x, struct scm *a)
{
  if (x->type == TSTRING)
    return assoc_string (x, a);
  while (a != cell_nil)
    {
      if (equal2_p (x, a->car->car) == cell_t)
        return a->car;
      a = a->cdr;
    }
  return cell_f;
}
