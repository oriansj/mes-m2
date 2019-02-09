/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2018 Jeremiah Orians <jeremiah@pdp10.guru>
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

#include <stdio.h>
#include <unistd.h>

void file_print(char* s, FILE* f)
{
	while(0 != s[0])
	{
		fputc(s[0], f);
		s = s + 1;
	}
}

int fdputc(int c, int fd)
{
	write(fd, (char*)&c, 1);
	return 0;
}

void fd_print(char* s, int f)
{
	while(0 != s[0])
	{
		fdputc(s[0], f);
		s = s + 1;
	}
}

int fdputs(char const* s, int fd)
{
	fd_print((char*)s, fd);
	return 0;
}

void raw_print(char* s, int fd)
{
	long v = s[0];
	while(0 != v)
	{
		if(v == '\0') fd_print("\\0", fd);
		else if(v == '\a') fd_print("\\a", fd);
		else if(v == '\b') fd_print("\\b", fd);
		else if(v == '\t') fd_print("\\t", fd);
		else if(v == '\v') fd_print("\\v", fd);
		else if(v == '\n') fd_print("\\n", fd);
		else if(v == '\f') fd_print("\\f", fd);
		else if(v == '\r') fd_print("\\r", fd);
		else if(v == '\e') fd_print("\\e", fd);
		else if(v == '\\') fd_print("\\\\", fd);
		else if(v == '"') fd_print("\\\"", fd);
		else fdputc(v, fd);
		s = s + 1;
		v = s[0];
	}
}
