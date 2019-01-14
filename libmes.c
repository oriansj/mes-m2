/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2019 Jeremiah Orians
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

#include "mes.h"
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <sys/resource.h>

char* ntoab(long x, int base, int signed_p)
{
	static char itoa_buf[20];
	char *p = itoa_buf + 11;
	*p-- = 0;
	int sign_p = 0;
	unsigned long u = x;

	if(signed_p && x < 0)
	{
		sign_p = 1;
		u = -x;
	}

	do
	{
		long i = u % base;
		*p-- = i > 9 ? 'a' + i - 10 : '0' + i;
		u = u / base;
	} while(u);

	if(sign_p && *(p + 1) != '0')
	{
		*p-- = '-';
	}

	return p + 1;
}

char* itoa(int x)
{
	return ntoab(x, 10, 1);
}

int __ungetc_buf[RLIMIT_NOFILE + 1] = {0};

void __ungetc_init()
{
	if(__ungetc_buf[RLIMIT_NOFILE] == 0)
	{
		memset(__ungetc_buf, -1, (RLIMIT_NOFILE + 1)*sizeof(int));
	}
}

int fdgetc(int fd)
{
	__ungetc_init();
	char c;
	int i = __ungetc_buf[fd];

	if(i >= 0)
	{
		__ungetc_buf[fd] = -1;
	}
	else
	{
		int r = read(fd, &c, 1);

		if(r < 1)
		{
			return -1;
		}

		i = c;
	}

	if(i < 0)
	{
		i += 256;
	}

	return i;
}

int fdputc(int c, int fd)
{
	write(fd, (char*)&c, 1);
	return 0;
}

int fdputs(char const* s, int fd)
{
	int i = strlen(s);
	write(fd, s, i);
	return 0;
}

void __ungetc_init();
int eputs(char const* s);
char* itoa(int i);
extern int __ungetc_buf[];

int fdungetc(int c, int fd)
{
	__ungetc_init();

	if(c == -1)
	{
		return c;
	}
	else if(__ungetc_buf[fd] != -1)
	{
		eputs(" ***MES C LIB*** fdungetc ungetc buffer overflow fd=");
		eputs(itoa(fd));
		eputs("\n");
		exit(1);
	}

	__ungetc_buf[fd] = c;
	return c;
}

int _fdungetc_p(int fd)
{
	return __ungetc_buf[fd] >= 0;
}

int eputs(char const* s)
{
	int i = strlen(s);
	write(__stderr, s, i);
	return 0;
}

int mes_open (char const *file_name, int flags, ...);
int __mes_debug ();

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
