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
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <ctype/isdigit.c>
#include <ctype/isxdigit.c>
#include <ctype/isspace.c>
#include <ctype/isnumber.c>

#include <mes/abtol.c>
#include <stdlib/atoi.c>
#include <mes/ntoab.c>
#include <mes/ltoab.c>
#include <mes/itoa.c>
#include <mes/ltoa.c>
#include <mes/ultoa.c>
#include <mes/utoa.c>
#include <mes/fdgetc.c>
#include <mes/fdputc.c>
#include <mes/fdputs.c>
#include <mes/fdungetc.c>

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

#include <mes/eputs.c>
#include <mes/oputs.c>
#endif // WITH_GLIBC

#include <mes/eputc.c>
#include <mes/oputc.c>
