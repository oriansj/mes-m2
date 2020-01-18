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
struct cell* findsym(char *name);
struct cell* make_char(int a);
struct cell* make_int(int a);
struct cell* make_string(char* a, int length);
struct cell* make_sym(char* name);

int string_size(char* a)
{
	int i = 0;
	while(0 != a[i]) i = i + 1;
	return i;
}

struct cell* string_length(struct cell* a)
{
	require(a->type == STRING, "Wrong type recieved\n");
	return make_int(a->length);
}

struct cell* string_eq(struct cell* a, struct cell* b)
{
	require(a->type == STRING, "Wrong type recieved\n");
	require(b->type == STRING, "Wrong type recieved\n");
	if(a->length != b->length) return cell_f;
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


char* substring(char* s, int start, int end)
{
	char* r = calloc((end - start) + 1, sizeof(char));
	int i = 0;
	while(start <= end)
	{
		r[i] = s[start];
		start = start + 1;
		i = i + 1;
	}

	return r;
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

struct cell* builtin_string_index(struct cell* args)
{
	int i = 0;
	int length;
	char* s;
	char find;
	require(nil != args, "string-index requires arguments\n");
	require(STRING == args->car->type, "string-index requires a string\n");
	s = args->car->string;
	length = args->car->length;
	require(nil != args->cdr, "string-index requires more argument(s)\n");
	require(CHAR == args->cdr->car->type, "string-index requires a char\n");
	find = args->cdr->car->value;

	/* Deal with (string-index "abcde" #\c) case */
	if(nil == args->cdr->cdr)
	{
		while(i <= length)
		{
			if(find == s[i]) return make_int(i);
			i = i + 1;
		}

		/* Deal with (string-index "abcde" #\z) case */
		return cell_f;
	}

	require(INT == args->cdr->cdr->car->type, "string-index requires an INT\n");
	i = args->cdr->cdr->car->value;
	require(length >= i, "string-index recieved an int greater than length of string");
	/* Deal with (string-index "abcde" #\c 0) case */
	if(nil == args->cdr->cdr->cdr)
	{
		while(i <= length)
		{
			if(find == s[i]) return make_int(i);
			i = i + 1;
		}

		/* Deal with (string-index "abcde" #\c 3) case */
		return cell_f;
	}

	require(INT == args->cdr->cdr->cdr->car->type, "string-index requires an INT\n");
	require(length >= args->cdr->cdr->cdr->car->value, "string-index received and int greater than length of string");
	length = args->cdr->cdr->cdr->car->value;

	if(nil == args->cdr->cdr->cdr->cdr)
	{
		while(i < length)
		{
			if(find == s[i]) return make_int(i);
			i = i + 1;
		}

		/* Deal with (string-index "abcde" #\c 0 2) case */
		return cell_f;
	}

	require(FALSE, "string-index recieved too many arguments\n");
	exit(EXIT_FAILURE);
}

struct cell* builtin_string_ref(struct cell* args)
{
	require(nil != args, "string-ref requires an argument\n");
	require(STRING == args->car->type, "string-ref requires a string\n");
	char* s = args->car->string;
	require(nil != args->cdr, "string-ref requires another argument\n");
	require(INT == args->cdr->car->type, "string-ref requires an integer\n");
	require(nil == args->cdr->cdr, "string-ref recieved too many arguments\n");
	int index = args->cdr->car->value;
	require(args->car->length >= index, "string-ref value longer than string\n");
	require(index >= 0, "string-ref value is negative\n");
	return make_char(s[index]);
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
	struct cell* r = findsym(args->car->string);
	if(nil != r) return r->car;
	return make_sym(args->car->string);
}

struct cell* builtin_symbol_to_string(struct cell* args)
{
	require(nil != args, "symbol->string requires an argument\n");
	require(nil == args->cdr, "symbol->string only supports a single argument\n");
	require(SYM == args->car->type, "symbol->string requires a symbol\n");
	return make_string(args->car->string, args->car->length);
}

struct cell* builtin_number_to_string(struct cell* args)
{
	require(nil != args, "number->string requires an argument\n");
	require(INT == args->car->type, "number->string requires an integer\n");
	char* r;
	if(nil == args->cdr)
	{
		r = ntoab(args->car->value, 10, TRUE);
		return make_string(r, string_size(r));
	}
	require(INT == args->cdr->car->type, "number->string only accepts integer ranges\n");
	require(2 <= args->cdr->car->value, "number->string Value out of range 2 to 36\n");
	require(36 >= args->cdr->car->value, "number->string Value out of range 2 to 36\n");
	require(nil == args->cdr->cdr, "number->string does not support more than 2 arguments\n");
	r = ntoab(args->car->value, args->cdr->car->value, TRUE);
	return make_string(r, string_size(r));
}


struct cell* builtin_substring(struct cell* args)
{
	require(nil != args, "substring requires arguments\n");
	require(STRING == args->car->type, "substring only works on strings\n");
	require(nil != args->cdr, "substring requires a starting index\n");
	require(INT == args->cdr->car->type, "substring's starting index must be an integer\n");
	int start = args->cdr->car->value;
	require(((start >= 0) && (start <= args->car->length )), "substring's starting index must be between 0 and the length of the string\n");

	if(nil == args->cdr->cdr)
	{
		return make_string(substring(args->car->string, start, args->car->length), (args->car->length - start));
	}

	require(INT == args->cdr->cdr->car->type, "substring's ending index must be an integer\n");
	int end = args->cdr->cdr->car->value;
	require(((end >= start) && (end <= args->car->length)), "substring's ending index must be between the starting index and the length of the string\n");
	return make_string(substring(args->car->string, start, end), (end - start));
}
