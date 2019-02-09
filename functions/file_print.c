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

// CONSTANT TRUE 1
#define TRUE 1
// CONSTANT FALSE 0
#define FALSE 0

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

int char2hex(int c);
char block[4];
char* char_lookup(int c, int type)
{
	static char* s = block;
	s[0] = '\\';
	if(type)
	{
		if(c == '\0') return "\\nul";
		else if(c == '\a') return "\\alarm";
		else if(c == '\b') return "\\backspace";
		else if(c == '\t') return "\\tab";
		else if(c == '\n') return "\\newline";
		else if(c == '\v') return "\\vtab";
		else if(c == '\f') return "\\page";
		else if(c == '\r') return "\\return";
		else if(c == ' ') return "\\space";
		else
		{
			s[1] = char2hex((c & 0xF0) >> 4);
			s[2] = char2hex(c & 0xF);
		}
	}
	else
	{
		s[2] = 0;
		if(c == '\0') s[1] = '0';
		else if(c == '\a') s[1] = 'a';
		else if(c == '\b') s[1] = 'b';
		else if(c == '\t') s[1] = 't';
		else if(c == '\v') s[1] = 'v';
		else if(c == '\n') s[1] = 'n';
		else if(c == '\f') s[1] = 'f';
		else if(c == '\r') s[1] = 'r';
		else if(c == '\e') s[1] = 'e';
		else if(c == '\\') s[1] = '\\';
		else if(c == '"') s[1] = '"';
		else
		{
			s[0] = c;
			s[1] = 0;
		}
	}
	return s;
}

void raw_print(char* s, int fd)
{
	while(0 != s[0])
	{
		fd_print(char_lookup(s[0], FALSE), fd);
		s = s + 1;
	}
}
