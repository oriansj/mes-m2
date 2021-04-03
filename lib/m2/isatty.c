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
#include <sys/ioctl.h>
#include <stdlib.h>
#include <string.h>
#include <termio.h>

// CONSTANT TCGETS 0x5401
#define TCGETS 0x5401

struct ktermios
{
  unsigned c_iflag;
  unsigned c_oflag;
  unsigned c_cflag;
  unsigned c_lflag;
  char c_line;
  char c_cc[19];
  unsigned c_ispeed;
  unsigned c_ospeed;
};

struct ktermios *__isatty_kernel_termios;

int
isatty (int filedes)
{
  if (__isatty_kernel_termios == 0)
    __isatty_kernel_termios = malloc (sizeof (struct ktermios));
  int r = ioctl3 (filedes, TCGETS, __isatty_kernel_termios);
  return r == 0;
}
