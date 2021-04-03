/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <limits.h>
#include <stdio.h>
#include <string.h>

#include <mes/lib.h>

int
main ()
{
  int i;

  oputs ("\n");

  oputs ("t: i = INT_MAX\n");
  i = INT_MAX;

  if (strcmp ("2147483647", itoa (i)))
    return 1;

  oputs ("t: i = 2147483646\n");
  i = INT_MIN;

  if (strcmp ("-2147483648", itoa (i)))
    return 2;

  if (strcmp ("-80000000", ntoab (i, 16, 1)))
    return 3;

  return 0;
}
