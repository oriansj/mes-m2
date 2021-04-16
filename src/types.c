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

struct scm *
char_p (struct scm *n)
{
  if(TCHAR == n->type) return cell_t;
  return cell_f;
}

void
assert_char (char const *name, struct scm *x)
{
  if (x->type != TCHAR)
    {
      eputs (name);
      error (cell_symbol_not_a_char, x);
    }
}

struct scm *
char_equal_p (struct scm *x)               /*:((name . "char=?") (arity . n)) */
{
  if (x == cell_nil)
    return cell_t;
  assert_char ("char_equal_p", x->car);
  long n = x->car->value;
  x = x->cdr;
  struct scm *i;
  long v;
  while (x != cell_nil)
    {
      assert_char ("char_equal_p", x->car);
      i = car (x);
      v = i->value;
      if (v != n)
        return cell_f;
      n = v;
      x = cdr (x);
    }
  return cell_t;
}

struct scm *
list_p (struct scm *n)
{
  struct scm* i = n;
  do
  {
    if(cell_nil == i) return cell_t;
    if(TPAIR != i->type) return cell_f;
    i = i->cdr;
  } while(NULL != i);
  error (cell_symbol_null_list, n);
  exit(EXIT_FAILURE);
}

struct scm *
symbol_p (struct scm *n)
{
  if(TSYMBOL == n->type) return cell_t;
  return cell_f;
}

struct scm *
number_p (struct scm *n)
{
  if(TNUMBER == n->type) return cell_t;
  return cell_f;
}

struct scm *
procedure_p (struct scm *n)
{
  if(TCLOSURE == n->type) return cell_t;
  return cell_f;
}

struct scm *
defined_p (struct scm *n)
{
  if(TREF == n->type) return cell_t;
  return cell_f;
}
