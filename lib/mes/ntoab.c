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

#include <mes/lib.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

size_t
__mesabi_uldiv (size_t a, size_t b, size_t *remainder)
{
  remainder[0] = a % b;
  return a / b;
}

char *__itoa_buf;

char *
ntoab (long x, unsigned base, int signed_p)
{
  if (__itoa_buf == 0)
    __itoa_buf = malloc (20);
  char *p = __itoa_buf + 11;

  p[0] = 0;
  p = p - 1;
  assert_msg (base > 0, "base > 0");

  int sign_p = 0;
  size_t i;
  size_t u;
  size_t b = base;
  if (signed_p != 0 && x < 0)
    {
      sign_p = 1;
      /* Avoid LONG_MIN */
      u = (-(x + 1));
      u = u + 1;
    }
  else
    u = x;

  do
    {
      u = __mesabi_uldiv (u, b, &i);
      if (i > 9)
        p[0] = 'a' + i - 10;
      else
        p[0] = '0' + i;
      p = p - 1;
    }
  while (u != 0);

  if (sign_p && p[1] != '0')
    {
      p[0] = '-';
      p = p - 1;
    }

  return p + 1;
}
