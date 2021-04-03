/*
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

char *
getenv (char const *s)
{
  /* eputs ("\ngetenv s="); eputs (s); eputs ("\n"); */
  char **p = environ;
  char *q;
  int length = strlen (s);

  while (p[0] != 0)
    {
      /* eputs ("getenv p[0]="); eputs (p[0]); eputs ("\n"); */
      if (strncmp (s, p[0], length) == 0)
        {
          /* eputs ("found!\n"); */
          q = p[0] + length;
          if (q[0] == '=')
            return q + 1;
        }
      /* else */
      /*   eputs ("not found!\n"); */
      p = p + M2_PTR_SIZE;
    }

  return 0;
}
