/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <linux/syscall.h>
#include <syscall.h>
#include <time.h>
#include <stdlib.h>

/* Linux' SYS_time syscall is a compatibility shim for SYS_gettimeofday.
Therefore, prefer SYS_gettimeofday. */

#if defined (SYS_gettimeofday)

#include <sys/time.h>

time_t
time (time_t * result)
{
  struct timeval tv;
  struct timezone tz;
  if (gettimeofday (&tv, &tz) != 0)
    return (time_t) - 1;
  if (result)
    *result = tv.tv_sec;
  return tv.tv_sec;
}

#elif defined (SYS_time)

time_t
time (time_t * result)
{
  return _sys_call1 (SYS_time, (long) result);
}

#else

#warning there is no time

#include <mes/lib.h>

time_t
time (time_t * result)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("time stub\n");
  stub = 1;
  if (result)
    *result = 0;
  return 0;
}

#endif
