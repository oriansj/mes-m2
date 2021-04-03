/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
    Scheme library functions not used by the eval/apply core.
 */

/** Code: */

#include "mes/lib.h"
#include "mes/mes.h"

#include <stdlib.h>

struct scm *
type_ (struct scm *x)
{
  return make_number (x->type);
}

struct scm *
car_ (struct scm *x)
{
  struct scm *a = x->car;
  if (x->type == TPAIR)
    return a;
  return make_number (cast_scmp_to_long (a));
}

struct scm *
cdr_ (struct scm *x)
{
  struct scm *d = x->cdr;
  if (x->type == TPAIR || x->type == TCLOSURE)
    return d;
  return make_number (cast_scmp_to_long (d));
}

struct scm *
xassq (struct scm *x, struct scm *a)            /* For speed in core. */
{
  while (a != cell_nil)
    {
      if (x == a->car->cdr)
        return a->car;
      a = a->cdr;
    }
  return cell_f;
}

struct scm *
memq (struct scm *x, struct scm *a)
{
  int t = x->type;
  if (t == TCHAR || t == TNUMBER)
    {
      long v = x->value;
      while (a != cell_nil)
        {
          if (v == a->car->value)
            return a;
          a = a->cdr;
        }
      return cell_f;
    }
  if (t == TKEYWORD)
    {
      while (a != cell_nil)
        {
          if (a->car->type == TKEYWORD)
            if (string_equal_p (x, a->car) == cell_t)
              return a;
          a = a->cdr;
        }
      return cell_f;
    }
  while (a != cell_nil)
    {
      if (x == a->car)
        return a;
      a = a->cdr;
    }
  return cell_f;
}

struct scm *
equal2_p (struct scm *a, struct scm *b)
{
  long i;
  struct scm *ai;
  struct scm *bi;

equal2:
  if (a == b)
    return cell_t;
  if (a->type == TPAIR && b->type == TPAIR)
    {
      if (equal2_p (a->car, b->car) == cell_t)
        {
          a = a->cdr;
          b = b->cdr;
          goto equal2;
        }
      return cell_f;
    }
  if (a->type == TSTRING && b->type == TSTRING)
    return string_equal_p (a, b);
  if (a->type == TVECTOR && b->type == TVECTOR)
    {
      if (a->length != b->length)
        return cell_f;
      for (i = 0; i < a->length; i = i + 1)
        {
          ai = cell_ref (a->vector, i);
          bi = cell_ref (b->vector, i);
          if (ai->type == TREF)
            ai = ai->ref;
          if (bi->type == TREF)
            bi = bi->ref;
          if (equal2_p (ai, bi) == cell_f)
            return cell_f;
        }
      return cell_t;
    }
  return eq_p (a, b);
}

struct scm *
last_pair (struct scm *x)
{
  while (x != cell_nil)
    {
      if (x->cdr == cell_nil)
        return x;
      x = x->cdr;
    }
  return x;
}

struct scm *
pair_p (struct scm *x)
{
  if (x->type == TPAIR)
    return cell_t;
  return cell_f;
}

struct scm *
char_to_integer (struct scm *x)
{
  return make_number (x->value);
}

struct scm *
integer_to_char (struct scm *x)
{
  return make_char (x->value);
}
