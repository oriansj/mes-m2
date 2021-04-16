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

#include "mes/lib.h"
#include "mes/mes.h"

#if __M2_PLANET__
#define M2_CELL_SIZE 12
// CONSTANT M2_CELL_SIZE 12
#else
#define M2_CELL_SIZE 1
// CONSTANT M2_CELL_SIZE 12
#endif

struct scm *
vector_p (struct scm *n)
{
  if(TVECTOR == n->type) return cell_t;
  return cell_f;
}

struct scm *
make_vector_ (long k, struct scm *e)
{
  struct scm *x = alloc (1);
  struct scm *v = alloc (k);
  x->type = TVECTOR;
  x->length = k;
  x->vector = v;
  long i;
  for (i = 0; i < k; i = i + 1)
    copy_cell (cell_ref (v, i), vector_entry (e));

  return x;
}

struct scm *
make_vector (struct scm *x)               /*:((arity . n)) */
{
  struct scm *k = x->car;
  assert_number ("make-vector", k);
  long n = k->value;
  struct scm *e = cell_unspecified;
  if (x->cdr != cell_nil)
    e = x->cdr->car;

  return make_vector_ (n, e);
}

struct scm *
vector_length (struct scm *x)
{
  assert_msg (x->type == TVECTOR, "x->type == TVECTOR");
  return make_number (x->length);
}

struct scm *
vector_ref_ (struct scm *x, long i)
{
  assert_msg (x->type == TVECTOR, "x->type == TVECTOR");
  assert_msg (i < x->length, "i < x->length");
  struct scm *e = cell_ref (x->vector, i);
  if (e->type == TREF)
    e = e->ref;
  if (e->type == TCHAR)
    e = make_char (e->value);
  if (e->type == TNUMBER)
    e = make_number (e->value);
  return e;
}

struct scm *
vector_ref (struct scm *x, struct scm *i)
{
  return vector_ref_ (x, i->value);
}

struct scm *
vector_entry (struct scm *x)
{
  if (x->type != TCHAR && x->type != TNUMBER)
    x = make_ref (x);
  return x;
}

struct scm *
vector_set_x_ (struct scm *x, long i, struct scm *e)
{
  assert_msg (x->type == TVECTOR, "x->type == TVECTOR");
  assert_msg (i < x->length, "i < x->length");
  copy_cell (cell_ref (x->vector, i), vector_entry (e));
  return cell_unspecified;
}

struct scm *
vector_set_x (struct scm *x, struct scm *i, struct scm *e)
{
  return vector_set_x_ (x, i->value, e);
}

struct scm *
list_to_vector (struct scm *x)
{
  struct scm *v = make_vector_ (length__ (x), cell_unspecified);
  struct scm *p = v->vector;
  while (x != cell_nil)
    {
      copy_cell (p, vector_entry (car (x)));
      p = p + M2_CELL_SIZE;
      x = cdr (x);
    }
  return v;
}

struct scm *
vector_to_list (struct scm *v)
{
  struct scm *x = cell_nil;
  long i;
  struct scm *e;
  for (i = v->length; i; i = i - 1)
    {
      e = cell_ref (v->vector, i - 1);
      if (e->type == TREF)
        e = e->ref;
      x = cons (e, x);
    }
  return x;
}
