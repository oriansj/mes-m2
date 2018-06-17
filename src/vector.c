/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of Mes.
 *
 * Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

SCM
make_vector__ (int k)
{
  SCM v = alloc (k);
  SCM x = make_cell__ (TVECTOR, k, v);
  for (int i=0; i<k; i++)
    g_cells[v+i] = g_cells[vector_entry (cell_unspecified)];
  return x;
}

SCM
make_vector_ (SCM n)
{
  return make_vector__ (VALUE (n));
}

SCM
vector_length (SCM x)
{
  assert (TYPE (x) == TVECTOR);
  return MAKE_NUMBER (LENGTH (x));
}

SCM
vector_ref (SCM x, SCM i)
{
  assert (TYPE (x) == TVECTOR);
  assert (VALUE (i) < LENGTH (x));
  SCM e = VECTOR (x) + VALUE (i);
  if (TYPE (e) == TREF)
    e = REF (e);
  if (TYPE (e) == TCHAR)
    e = MAKE_CHAR (VALUE (e));
  if (TYPE (e) == TNUMBER)
    e = MAKE_NUMBER (VALUE (e));
  return e;
}

SCM
vector_entry (SCM x)
{
  if (TYPE (x) != TCHAR && TYPE (x) != TNUMBER)
    x = MAKE_REF (x);
  return x;
}

SCM
vector_set_x (SCM x, SCM i, SCM e)
{
  assert (TYPE (x) == TVECTOR);
  assert (VALUE (i) < LENGTH (x));
  g_cells[VECTOR (x)+VALUE (i)] = g_cells[vector_entry (e)];
  return cell_unspecified;
}

SCM
list_to_vector (SCM x)
{

  SCM v = make_vector__ (length__ (x));
  SCM p = VECTOR (v);
  while (x != cell_nil)
    {
      g_cells[p++] = g_cells[vector_entry (car (x))];
      x = cdr (x);
    }
  return v;
}

SCM
vector_to_list (SCM v)
{
  SCM x = cell_nil;
  for (int i = LENGTH (v); i; i--)
    {
      SCM e = VECTOR (v)+i-1;
      if (TYPE (e) == TREF)
        e = REF (e);
      x = cons (e, x);
    }
  return x;
}
