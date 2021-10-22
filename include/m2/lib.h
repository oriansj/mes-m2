/*
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#if defined(__M2_LIB_H)
#else
#define __M2_LIB_H 1

char **environ;
int __stdin;
int __stdout;
int __stderr;
int errno;

typedef SCM ulong;
typedef ulong size_t;
typedef long ssize_t;

#define EOF 0xffffffff
#define __FILEDES_MAX 512
#define AT_FDCWD            -100

char* cast_intp_to_charp (int *i);
char* cast_long_to_charp (long i);
long cast_charp_to_long (char const *);
long cast_int_to_long (int i);
long cast_voidp_to_long (void const *);

char *itoa (int number);
char *ltoa (long number);
int __ungetc_p (int filedes);
int eputs (char *s);
int oputs (char *s);
int puts (char *s);
size_t strlen (char *s);
ssize_t _write ();
ssize_t write (int filedes, void *buffer, size_t size);
void __ungetc_clear (int filedes);
void __ungetc_init ();
void __ungetc_set (int filedes, int c);

struct timezone
{
  int tz_minuteswest;
  int tz_dsttime;
};

struct timespec
{
  long tv_sec;
  long tv_nsec;
};

struct timeval
{
  long tv_sec;
  long tv_usec;
};

#endif /* __M2_LIB_H */
