/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018,2019,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

struct scm *
make_struct (struct scm *type, struct scm *fields, struct scm *printer)
{
  long size = 2 + length__ (fields);
  struct scm *x = alloc (1);
  struct scm *v = alloc (size);
  x->type = TSTRUCT;
  x->length = size;
  x->structure = v;
  copy_cell (v, vector_entry (type));
  copy_cell (cell_ref (v, 1), vector_entry (printer));
  long i;
  struct scm *e;
  for (i = 2; i < size; i = i + 1)
    {
      e = cell_unspecified;
      if (fields != cell_nil)
        {
          e = fields->car;
          fields = fields->cdr;
        }
      copy_cell (cell_ref (v, i), vector_entry (e));
    }
  return x;
}

struct scm *
struct_length (struct scm *x)
{
  assert_msg (x->type == TSTRUCT, "x->type == TSTRUCT");
  return make_number (x->length);
}

struct scm *
struct_ref_ (struct scm *x, long i)
{
  assert_msg (x->type == TSTRUCT, "x->type == TSTRUCT");
  assert_msg (i < x->length, "i < x->length");
  struct scm *e = cell_ref (x->structure, i);
  if (e->type == TREF)
    e = e->ref;
  if (e->type == TCHAR)
    e = make_char (e->value);
  if (e->type == TNUMBER)
    e = make_number (e->value);
  return e;
}

struct scm *
struct_set_x_ (struct scm *x, long i, struct scm *e)
{
  assert_msg (x->type == TSTRUCT, "x->type == TSTRUCT");
  assert_msg (i < x->length, "i < x->length");
  copy_cell (cell_ref (x->structure, i), vector_entry (e));
  return cell_unspecified;
}

struct scm *
struct_ref (struct scm *x, struct scm *i)
{
  return struct_ref_ (x, i->value);
}

struct scm *
struct_set_x (struct scm *x, struct scm *i, struct scm *e)
{
  return struct_set_x_ (x, i->value, e);
}
