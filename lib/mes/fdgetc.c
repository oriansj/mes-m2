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

int _ungetc_pos = -1;
int _ungetc_fd = -1;
char _ungetc_buf[10];

int
fdgetc (int fd)
{
  char c;
  int i;
  if (_ungetc_pos == -1)
    {
      int r = read (fd, &c, 1);
      if (r < 1)
        return -1;
      i = c;
   }
  else
    {
      i = _ungetc_buf[_ungetc_pos];
      if (_ungetc_fd != fd && i == 10)
        {
          // FIXME: Nyacc's ungetc exposes harmless libmec.c bug
          // we need one unget position per FD
          _ungetc_pos = -1;
          _ungetc_fd = -1;
          return fdgetc (fd);
        }
      else if (_ungetc_fd != fd)
        {
          eputs (" ***MES C LIB*** fdgetc ungetc conflict unget-fd=");
          eputs (itoa (_ungetc_fd));
          eputs (", fdgetc-fd=");
          eputs (itoa (fd));
          eputs (", c=");
          eputs (itoa ( _ungetc_buf[_ungetc_pos]));
          eputs ("\n");
          exit (1);
        }
      i = _ungetc_buf[_ungetc_pos];
      _ungetc_pos -= 1;
      if (_ungetc_pos == -1)
        _ungetc_fd = -1;
     }
  if (i < 0)
    i += 256;

  return i;
}
