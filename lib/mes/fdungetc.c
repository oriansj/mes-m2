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
  if (c == -1)
    return c;
  if (_ungetc_pos == -1)
    _ungetc_fd = fd;
  else if (_ungetc_fd != fd)
    {
      eputs (" ***MES LIB C*** fdungetc ungetc conflict unget-fd=");
      eputs (itoa (_ungetc_fd));
      eputs (", fdungetc-fd=");
      eputs (itoa (fd));
      eputs ("\n");
      exit (1);
    }
  _ungetc_pos++;
  _ungetc_buf[_ungetc_pos] = c;
  return c;
}

int
_fdungetc_p (int fd)
{
  return _ungetc_pos > -1;
}
