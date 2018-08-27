/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <libmes.h>

long
abtol (char const **p, int base)
{
  char const *s = p[0];
  int i = 0;
  int sign = 1;
  if (!base) base = 10;
  if (s[0] == '-')
    {
      sign = -1;
      s = s + 1;
    }
  while (isnumber (s[0], base))
    {
      i = i * base;
      int m = '0';
      if (s[0] > '9')
        m = 'a' - 10;
      i = i + s[0] - m;
      s = s + 1;
    }
  p[0] = s;
  return i * sign;
}
