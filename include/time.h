/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#ifndef __MES_TIME_H
#define __MES_TIME_H 1

#if WITH_GLIBC
#undef __MES_TIME_H
#include_next <time.h>
#else // ! WITH_GLIBC

#ifndef __MES_TIME_T
#define __MES_TIME_T 1
typedef int time_t;
#endif

struct tm {
  int tm_sec;
  int tm_min;
  int tm_hour;
  int tm_mday;
  int tm_mon;
  int tm_year;
  int tm_wday;
  int tm_yday;
  int tm_isdst;
};

struct tm *localtime (time_t const *timep);
struct tm *gmtime (time_t const *time);

time_t time (time_t *tloc);

#ifndef __MES_STRUCT_TIMESPEC
#define __MES_STRUCT_TIMESPEC

struct timespec
{
  long tv_sec;
  long tv_nsec;
};

#endif // __MES_STRUCT_TIMESPEC

#endif // ! WITH_GLIBC

#endif // __MES_TIME_H
