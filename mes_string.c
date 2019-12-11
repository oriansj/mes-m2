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
char* ntoab(SCM x, int base, int signed_p);
int string_size(char* a);
struct cell* make_int(int a);
struct cell* make_string(char* a);
struct cell* make_sym(char* name);

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


/* Exposed primitives */
struct cell* builtin_stringp(struct cell* args)
{
	require(nil != args, "string? requires arguments\n");
	require(nil == args->cdr, "string? recieved too many arguments\n");
	if(STRING == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_stringeq(struct cell* args)
{
	require(nil != args, "string=? requires arguments\n");

	require(STRING == args->car->type, "string=? received non-string\n");
	struct cell* temp = args->car;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(STRING == args->car->type, "string=? received non-string\n");
		if(cell_t != string_eq(temp, args->car))
		{
			return cell_f;
		}
	}
	return cell_t;
}

struct cell* builtin_string_size(struct cell* args)
{
	require(nil != args, "string-length requires an argument\n");
	require(nil == args->cdr, "string-length only allows a single argument\n");
	return string_length(args->car);
}

struct cell* builtin_string_to_number(struct cell* args)
{
	require(nil != args, "string->number requires an argument\n");
	require(nil == args->cdr, "string->number only supports a single argument (currently)\n");
	require(STRING == args->car->type, "string->number requires a string\n");
	int i = numerate_string(args->car->string);
	if(0 != i) return make_int(i);
	if('0' == args->car->string[0]) return make_int(i);
	return cell_f;
}

struct cell* builtin_string_to_symbol(struct cell* args)
{
	require(nil != args, "string->symbol requires an argument\n");
	require(nil == args->cdr, "string->symbol only supports a single argument\n");
	require(STRING == args->car->type, "string->symbol requires a string\n");
	return make_sym(args->car->string);
}

struct cell* builtin_symbol_to_string(struct cell* args)
{
	require(nil != args, "symbol->string requires an argument\n");
	require(nil == args->cdr, "symbol->string only supports a single argument\n");
	require(SYM == args->car->type, "symbol->string requires a symbol\n");
	return make_string(args->car->string);
}

struct cell* builtin_number_to_string(struct cell* args)
{
	require(nil != args, "number->string requires an argument\n");
	require(INT == args->car->type, "number->string requires an integer\n");
	if(nil == args->cdr) return make_string(ntoab(args->car->value, 10, TRUE));
	require(INT == args->cdr->car->type, "number->string only accepts integer ranges\n");
	require(2 <= args->cdr->car->value, "number->string Value out of range 2 to 36\n");
	require(36 >= args->cdr->car->value, "number->string Value out of range 2 to 36\n");
	require(nil == args->cdr->cdr, "number->string does not support more than 2 arguments\n");
	return make_string(ntoab(args->car->value, args->cdr->car->value, TRUE));
}
