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

#include "mes/lib.h"
#include "mes/mes.h"

struct scm *
apply_builtin0 (struct scm *fn)
{
  FUNCTION fp = builtin_function (fn);
  return fp ();
}

struct scm *
apply_builtin1 (struct scm *fn, struct scm *x)
{
  FUNCTION fp = builtin_function (fn);
  return fp (x);
}

struct scm *
apply_builtin2 (struct scm *fn, struct scm *x, struct scm *y)
{
  FUNCTION fp = builtin_function (fn);
  return fp (x, y);
}

struct scm *
apply_builtin3 (struct scm *fn, struct scm *x, struct scm *y, struct scm *z)
{
  FUNCTION fp = builtin_function (fn);
  return fp (x, y, z);
}

#undef cast_charp_to_scmp
#undef cast_charp_to_scmpp
#undef cast_voidp_to_charp
#undef cast_scmp_to_long
#undef cast_scmp_to_charp

struct scm *
cast_charp_to_scmp (char const *i)
{
  return i;
}

struct scm **
cast_charp_to_scmpp (char const *i)
{
  return i;
}

char*
cast_voidp_to_charp (void const *i)
{
  return i;
}

long
cast_scmp_to_long (struct scm *i)
{
  return i;
}

char*
cast_scmp_to_charp (struct scm *i)
{
  return i;
}
