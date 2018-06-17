/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of Mes.
 *
 * Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Mes.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef __MES_SYS_RESOURCE_H
#define __MES_SYS_RESOURCE_H 1

#if WITH_GLIBC
#undef __MES_SYS_RESOURCE_H
#include_next <sys/resource.h>

#else // ! WITH_GLIBC

#include <sys/time.h>

struct rusage
{
  struct timeval ru_utime;
  struct timeval ru_stime;
  long int ru_maxrss;
  long int ru_ixrss;
  long int ru_idrss;
  long int ru_isrss;
  long int ru_minflt;
  long int ru_majflt;
  long int ru_nswap;
  long int ru_inblock;
  long int ru_oublock;
  long int ru_msgsnd;
  long int ru_msgrcv;
  long int ru_nsignals;
  long int ru_nvcsw;
  long int ru_nivcsw;
};

#define RUSAGE_SELF 0
#define RUSAGE_CHILDREN -1

int getrusage (int processes, struct rusage *rusage);

#endif // ! WITH_GLIBC

#endif // __MES_SYS_RESOURCE_H
