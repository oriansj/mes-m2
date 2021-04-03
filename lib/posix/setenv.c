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
#include <string.h>
#include <stdlib.h>

// CONSTANT M2_PTR_SIZE 4
#define M2_PTR_SIZE 1

int
setenv (char const *s, char const *v, int overwrite_p)
{
  char **p = environ;
  int length = strlen (s);
  char* q;

  while (p[0] != 0)
    {
      if (strncmp (s, p[0], length) == 0)
        {
          q = p[0] + length;
          if (q[0] == '=')
            break;
        }
      p = p + M2_PTR_SIZE;
    }
  char *entry = malloc (length + strlen (v) + 2);
  int end_p = p[0] == 0;
  p[0] = entry;
  strcpy (entry, s);
  strcpy (entry + length, "=");
  strcpy (entry + length + 1, v);
  entry[length + strlen (v) + 2] = 0;
  if (end_p != 0)
    p[1] = 0;

  return 0;
}
