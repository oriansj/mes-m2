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

// CONSTANT TRUE 1
#define TRUE 1
// CONSTANT FALSE 0
#define FALSE 0
int in_set(int c, char* s);

int char2hex(int c)
{
	if (c >= '0' && c <= '9') return (c - 48);
	else if (c >= 'a' && c <= 'f') return (c - 87);
	else if (c >= 'A' && c <= 'F') return (c - 55);
	else return -1;
}

int index_number(char* s, char c)
{
	int i = 0;
	while(s[i] != c)
	{
		i = i + 1;
		if(0 == s[i]) return -1;
	}
	return i;
}

int toupper(int c)
{
	if(in_set(c, "abcdefghijklmnopqrstuvwxyz")) return (c & 0xDF);
	return c;
}

int set_reader(char* set, int mult, char* input)
{
	int n = 0;
	int i = 0;
	int hold;
	int negative_p = 0;

	if(input[0] == '-')
	{
		negative_p = 1;
		i = i + 1;
	}

	while(in_set(input[i], set))
	{
		n = n * mult;
		hold = index_number(set, toupper(input[i]));
		if(-1 == hold) return 0;
		n = n + hold;
		i = i + 1;
	}

	if(0 != input[i]) return 0;

	if(negative_p)
	{
		n = 0 - n;
	}

	return n;
}

int numerate_string(char *a)
{
	/* If NULL string */
	if(0 == a[0])
	{
		return 0;
	}
	/* Deal with binary*/
	else if ('0' == a[0] && 'b' == a[1])
	{
		return set_reader("01", 2, a+2);
	}
	/* Deal with hex */
	else if ('0' == a[0] &&  'x' == a[1])
	{
		return set_reader("0123456789ABCDEFabcdef", 16, a+2);
	}
	/* Deal with ocal */
	else if('0' == a[0])
	{
		return set_reader("01234567", 8, a+1);
	}
	/* Deal with decimal */
	else
	{
		return set_reader("0123456789", 10, a);
	}
}
