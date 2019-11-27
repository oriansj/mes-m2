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

/* Imported functions */
struct cell* make_int(int a);
int string_size(char* a);

struct cell* string_length(struct cell* a)
{
	require(a->type == STRING, "Wrong type recieved\n");
	return make_int(string_size(a->string));
}

struct cell* string_eq(struct cell* a, struct cell* b)
{
	require(a->type == STRING, "Wrong type recieved\n");
	require(b->type == STRING, "Wrong type recieved\n");
	if(match(a->string, b->string)) return cell_t;
	return cell_f;
}

/****************************************************************
 *           Functions for reducing wasted memory               *
 ****************************************************************/
void reset_block(char* a)
{
	int c;
	do
	{
		c = a[0];
		a[0] = 0;
		a = a + 1;
	} while(0 != c);
}

char* copy_string(char* target, char* source)
{
	while(0 != source[0])
	{
		target[0] = source[0];
		target = target + 1;
		source = source + 1;
	}
	return target;
}

char* string_append(char* a, char* b)
{
	if(NULL == a) return b;
	if(NULL == b) return a;
	int a_size = string_size(a);
	int buffer_size = a_size + string_size(b) + 1;
	char* buffer = calloc(buffer_size, sizeof(char));
	copy_string(buffer, a);
	copy_string(buffer + a_size, b);
	return buffer;
}
