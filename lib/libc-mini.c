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

#include <errno.h>

#ifndef _SIZE_T
#define _SIZE_T
#ifndef __SIZE_T
#define __SIZE_T
#ifndef __MES_SIZE_T
#define __MES_SIZE_T
#undef size_t
typedef unsigned long size_t;
#endif
#endif
#endif

#ifndef _SSIZE_T
#define _SSIZE_T
#ifndef __SSIZE_T
#define __SSIZE_T
#ifndef __MES_SSIZE_T
#define __MES_SSIZE_T
#undef ssize_t
typedef long ssize_t;
#endif
#endif
#endif

ssize_t write (int filedes, void const *buffer, size_t size);

size_t
strlen (char const* s)
{
  int i = 0;
  while (s[i]) i++;
  return i;
}

int
eputs (char const* s)
{
  int i = strlen (s);
  write (2, s, i);
  return 0;
}

int
oputs (char const* s)
{
  int i = strlen (s);
  write (1, s, i);
  return 0;
}

int
puts (char const* s)
{
  oputs (s);
  return oputs ("\n");
}

#if __GNU__
#include <hurd/libc-mini.c>
#elif __linux__
#include <linux/libc-mini.c>
#else
#error both __GNU__ and _linux__ are undefined, choose one
#endif

void (*__call_at_exit) (void);

void
exit (int code)
{
  if (__call_at_exit)
    (*__call_at_exit) ();
  _exit (code);
}
