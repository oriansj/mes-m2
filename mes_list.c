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
struct cell* make_char(int a);

struct cell* string_to_list(char* string)
{
	if(NULL == string) return nil;
	if(0 == string[0]) return nil;
	struct cell* result = make_char(string[0]);
	struct cell* tail = string_to_list(string + 1);
	return make_cons(result, tail);
}

int list_length(struct cell* args)
{
	if(nil == args) return 0;
	int size = 0;
	struct cell* i;
	for(i = args->car; nil != i; i = i->cdr)
	{
		size = size + 1;
	}
	return size;
}

char* list_to_string(struct cell* args)
{
	char* string = calloc(list_length(args) + 1, sizeof(char));
	int index = 0;
	struct cell* i;
	for(i = args->car; nil != i; i = i->cdr)
	{
		if(CHAR == i->car->type)
		{
			string[index] = i->car->value;
			index = index + 1;
		}
		else
		{
			file_print("Wrong type recieved\n", stdout);
			exit(EXIT_FAILURE);
		}
	}
	return string;
}

struct cell* append(struct cell* a, struct cell* b)
{
	if(nil == a) return b;
	return make_cons(a->car, append(a->cdr, b));
}
