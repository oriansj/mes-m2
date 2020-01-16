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
struct cell* equal(struct cell* a, struct cell* b);
struct cell* make_char(int a);
struct cell* make_int(int a);
struct cell* make_string(char* a, int length);
struct cell* make_sym(char* name);

struct cell* string_to_list(char* string, int length)
{
	if(NULL == string) return nil;
	if(0 == length) return nil;
	struct cell* result = make_char(string[0]);
	struct cell* tail = string_to_list(string + 1, length - 1);
	return make_cons(result, tail);
}

int list_length(struct cell* args)
{
	require(CONS == args->type, "mes_list.c: list_length recieved wrong type\n");
	if(nil == args) return 0;
	int size = 0;
	struct cell* i;
	for(i = args->car; nil != i; i = i->cdr)
	{
		require(CONS == i->type, "mes_list.c: list_length recieved non-pure list\n");
		size = size + 1;
	}
	return size;
}

struct cell* list_to_string(struct cell* args)
{
	require(CONS == args->type, "mes_list.c: list_to_string recieved wrong type\n");
	char* string = calloc(list_length(args) + 1, sizeof(char));
	int index = 0;
	struct cell* i;
	for(i = args->car; nil != i; i = i->cdr)
	{
		require(CONS == args->type, "mes_list.c: list_to_string recieved non-pure list\n");
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

	return make_string(string, list_length(args));
}

struct cell* append(struct cell* a, struct cell* b)
{
	if(nil == a) return b;
	require(CONS == a->type, "Did not recieve a proper list in append\n");
	return make_cons(a->car, append(a->cdr, b));
}

struct cell* list_equal(struct cell* a, struct cell* b)
{
	while((nil != a) && (nil != b))
	{
		require(CONS == a->type, "mes_list.c: list_equal received non-list\n");
		require(CONS == b->type, "mes_list.c: list_equal received non-list\n");

		if(cell_t != equal(a->car, b->car)) return cell_f;

		a = a->cdr;
		b = b->cdr;
	}

	if(a != b) return cell_f;
	return cell_t;
}

struct cell* reverse(struct cell* a, struct cell* b)
{
	require(CONS == a->type, "reverse did not receive a list\n");
	if(nil == a->cdr) return make_cons(a->car, b);
	require(CONS == a->cdr->type, "reverse did not receive a true list\n");
	return reverse(a->cdr, make_cons(a->car, b));
}


/* Exposed primitives */
struct cell* builtin_listp(struct cell* args)
{
	require(nil != args, "list? requires arguments\n");
	require(nil == args->cdr, "list? recieved too many arguments\n");

	struct cell* i = args->car;
	while(nil != i)
	{
		if(CONS != i->type) return cell_f;
		i = i->cdr;
	}

	return cell_t;
}

struct cell* builtin_append(struct cell* args)
{
	if(nil == args) return nil;
	require(((nil == args->car) || (CONS == args->car->type)), "append requires a list\n");
	if(nil == args->cdr) return args->car;
	require(((nil == args->car) || (CONS == args->car->type)), "append requires a list argument\n");
	return append(args->car, args->cdr->car);
}

struct cell* builtin_listeq(struct cell* args)
{
	require(nil != args, "list=? requires arguments\n");

	require(CONS == args->car->type, "list=? received non-list\n");
	struct cell* temp = args->car;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(CONS == args->car->type, "list=? received non-list\n");
		if(cell_t != list_equal(temp, args->car))
		{
			return cell_f;
		}
	}
	return cell_t;
}

struct cell* builtin_string_to_list(struct cell* args)
{
	require(nil != args, "string->list requires arguments\n");
	require(STRING == args->car->type, "mes-builtin.c: string->list did not receive a string\n");

	struct cell* r = string_to_list(args->car->string, args->car->length);
	if(nil == args->cdr)
	{
		return r;
	}

	require(INT == args->cdr->car->type, "string->list only accepts integers\n");
	int i = args->cdr->car->value;
	require(0 <= i, "string->list invalid index\n");
	while(0 != i)
	{
		require(nil != r, "string->list index too large\n");
		r = r->cdr;
		i = i - 1;
	}
	return r;
}

struct cell* builtin_list_length(struct cell* args)
{
	require(nil != args, "list-length requires arguments\n");
	return make_int(list_length(args));
}

struct cell* builtin_list_to_string(struct cell* args)
{
	require(nil != args, "list->string requires an argument\n");
	require(nil == args->cdr, "list->string only allows a single argument\n");
	return list_to_string(args);
}

struct cell* builtin_list_to_symbol(struct cell* args)
{
	require(nil != args, "list->symbol requires an argument\n");
	require(nil == args->cdr, "list->symbol only allows a single argument\n");
	struct cell* r = list_to_string(args);
	r->type = SYM;
	return r;
}

struct cell* builtin_list(struct cell* args)
{
	/* List is stupid, just return */
	return args;
}

struct cell* builtin_reverse(struct cell* args)
{
	require(nil != args, "reverse requires arguments\n");
	require(nil == args->cdr, "reverse recieved too many arguments\n");
	return reverse(args->car, nil);
}
