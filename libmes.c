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

#ifndef __MES_LIBMES_H
#define __MES_LIBMES_H

#ifndef __MES_LIBMES_MINI_H
#define __MES_LIBMES_MINI_H

#if !WITH_GLIBC

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
#if __i386__
typedef int ssize_t;
#else
typedef long ssize_t;
#endif
#endif
#endif
#endif

#ifndef __MES_ERRNO_T
#define __MES_ERRNO_T 1
typedef int error_t;
int errno;
#endif // !__MES_ERRNO_T

#endif //!WITH_LIBC

// CONSTANT STDIN 0
#ifndef STDIN
#define STDIN 0
#endif

// CONSTANT STDOUT 1
#ifndef STDOUT
#define STDOUT 1
#endif

// CONSTANT STDERR 2
#ifndef STDERR
#define STDERR 2
#endif

char **environ;
int __stdin;
int __stdout;
int __stderr;

int eputs (char const* s);
int puts (char const* s);
int oputs (char const* s);

#if !WITH_GLIBC
size_t strlen (char const* s);
ssize_t write (int filedes, void const *buffer, size_t size);
#endif // !WITH_GLIBC

#endif //__MES_LIBMES_MINI_H

#if WITH_GLIBC
int mes_open (char const *file_name, int flags, ...);
#define open mes_open
#endif

int __mes_debug ();
double abtod (char const** p, int base);
long abtol (char const** p, int base);
char *dtoab (double number, int base, int signed_p);
char *itoa (int number);
char *ltoa (long number);
char *ltoab (long x, int base);
char *ntoab (long number, int base, int signed_p);
char *ultoa (unsigned long number);
char *utoa (unsigned number);
int atoi (char const *s);
int eputc (int c);
int fdgetc (int fd);
int fdputc (int c, int fd);
int fdputs (char const* s, int fd);
int fdungetc (int c, int fd);
int _fdungetc_p (int fd);
int isdigit (int c);
int isspace (int c);
int isxdigit (int c);
int _open3 (char const *file_name, int flags, int mask);
int _open2 (char const *file_name, int flags);
int oputc (int c);
int oputs (char const* s);
char *search_path (char const *file_name);

#endif //__MES_LIBMES_H

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "lib/ctype/isdigit.c"
#include "lib/ctype/isxdigit.c"
#include "lib/ctype/isspace.c"
#include "lib/ctype/isnumber.c"

#include "lib/mes/abtol.c"
#include "lib/stdlib/atoi.c"
#include "lib/mes/ntoab.c"
#include "lib/mes/ltoab.c"
#include "lib/mes/itoa.c"
#include "lib/mes/ltoa.c"
#include "lib/mes/ultoa.c"
#include "lib/mes/utoa.c"
#include "lib/mes/fdgetc.c"
#include "lib/mes/fdputc.c"
#include "lib/mes/fdputs.c"
#include "lib/mes/fdungetc.c"

#if WITH_GLIBC
#undef open
#include <fcntl.h>
#include <stdarg.h>
// The Mes C Library defines and initializes these in crt1
int __stdin = STDIN;
int __stdout = STDOUT;
int __stderr = STDERR;

int mes_open(char const *file_name, int flags, ...)
{
	va_list ap;
	va_start(ap, flags);
	int mask = va_arg(ap, int);
	__ungetc_init();
	int r = open(file_name, flags, mask);

	if(r > 2)
	{
		__ungetc_buf[r] = -1;
	}

	va_end(ap);
	return r;
}

#include "lib/mes/eputs.c"
#include "lib/mes/oputs.c"
#endif // WITH_GLIBC

#include "lib/mes/eputc.c"
#include "lib/mes/oputc.c"
