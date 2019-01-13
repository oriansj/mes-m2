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

int
fdungetc (int c, int fd)
{
  __ungetc_init ();
  if (c == -1)
    return c;
  else if (__ungetc_buf[fd] != -1)
    {
      eputs (" ***MES C LIB*** fdungetc ungetc buffer overflow fd=");
      eputs (itoa (fd));
      eputs ("\n");
      exit (1);
    }
  __ungetc_buf[fd] = c;
  return c;
}

int
_fdungetc_p (int fd)
{
  return __ungetc_buf[fd] >= 0;
}
