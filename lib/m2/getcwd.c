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
#include <mes/mes.h>
#include <limits.h>
#include <stdlib.h>
#include <sys/types.h>

#define PATH_MAX 1024

char *__getcwd_buf;

char *
getcwd (char *buffer, int size)
{
  if (buffer == 0)
    buffer = __getcwd_buf;
  if (buffer == 0)
    {
      __getcwd_buf = malloc (PATH_MAX);
      buffer = __getcwd_buf;
    }
  return _getcwd (buffer, size);
}
