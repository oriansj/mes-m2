/* Copyright (C) 2016 Jeremiah Orians
 * This file is part of M2-Planet.
 *
 * M2-Planet is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * M2-Planet is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with M2-Planet.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <stdio.h>
#include <stdlib.h>
// void fputc(char s, FILE* f);
#define TRUE 1
#define FALSE 0

int in_set(int c, char* s);
void file_print(char* s, FILE* f);


char char_lookup(int c)
{
	if(c == '\a') return 'a';
	else if(c == '\b') return 'b';
	else if(c == '\t') return 't';
	else if(c == '\v') return 'v';
	else if(c == '\n') return 'n';
	else if(c == '\f') return 'f';
	else if(c == '\r') return 'r';
	else if(c == '\033') return 'e';
	else if(c == '\\') return '\\';
	else if(c == '"') return '"';
	return c;
}

int char2hex(int c);
int hexify(int c, int high)
{
	int i = char2hex(c);

	if(0 > i)
	{
		file_print("Tried to print non-hex number\n", stderr);
		exit(EXIT_FAILURE);
	}

	if(high)
	{
		i = i << 4;
	}
	return i;
}


void file_print(char* s, FILE* f)
{
	while(0 != s[0])
	{
		fputc(s[0], f);
		s = s + 1;
	}
}


int escape_lookup(char* c)
{
	if('\\' != c[0]) return c[0];

	if(c[1] == 'x')
	{
		int t1 = hexify(c[2], TRUE);
		int t2 = hexify(c[3], FALSE);
		return t1 + t2;
	}
	else if(c[1] == '0') return 0;
	else if(c[1] == 'a') return 7;
	else if(c[1] == 'b') return 8;
	else if(c[1] == 't') return 9;
	else if(c[1] == 'n') return 10;
	else if(c[1] == 'v') return 11;
	else if(c[1] == 'f') return 12;
	else if(c[1] == 'r') return 13;
	else if(c[1] == 'e') return 27;
	else if(c[1] == '"') return 34;
	else if(c[1] == '\'') return 39;
	else if(c[1] == '\\') return 92;

	file_print("Unknown escape received: ", stderr);
	file_print(c, stderr);
	file_print(" Unable to process\n", stderr);
	exit(EXIT_FAILURE);
}


void raw_print(char* s, FILE* f)
{
	char c;
	while(0 != s[0])
	{
		c = s[0];
		if(in_set(c, "\a\b\t\b\v\f\n\r\033\"\\"))
		{
			fputc('\\', f);
			c = char_lookup(c);
		}
		fputc(c, f);
		s = s + 1;
	}
}


void ugly_print(char* s, FILE* f, int length)
{
	int c;
	int tmp;
	char* table = "0123456789ABCDEF";

	while(length > 0)
	{
		c = s[0];
		if((c < 32) || (c > 126))
		{
			fputc('\\', f);
			fputc('x', f);
			tmp = (c >> 4) & 0xF;
			fputc(table[tmp], f);
			tmp = c & 0xF;
			fputc(table[tmp], f);
		}
		else
		{
			fputc(c, f);
		}
		length = length - 1;
		s = s + 1;
	}
}
