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
FILE* open_file(char* name, char* mode);
struct cell* assoc(struct cell* key, struct cell* alist);
struct cell* extend(struct cell* env, struct cell* symbol, struct cell* value);
struct cell* load_file(char* s);
struct cell* make_cell(int type, struct cell* a, struct cell* b, struct cell* env);
struct cell* make_char(int a);
struct cell* make_eof();
struct cell* make_file(FILE* a, char* name);
struct cell* make_int(int a);
struct cell* make_sym(char* name);
struct cell* string_eq(struct cell* a, struct cell* b);
struct cell* vector_equal(struct cell* a, struct cell* b);
void garbage_collect();

/*** Primitives ***/
struct cell* nullp(struct cell* args)
{
	require(nil != args, "null? requires arguments\n");
	require(nil == args->cdr, "null? recieved too many arguments\n");
	if(nil == args->car) return cell_t;
	return cell_f;
}

struct cell* pairp(struct cell* args)
{
	require(nil != args, "pair? requires arguments\n");
	require(nil == args->cdr, "pair? recieved too many arguments\n");
	if(CONS == args->car->type) return cell_t;
	return cell_f;
}

struct cell* portp(struct cell* args)
{
	require(nil != args, "port? requires arguments\n");
	require(nil == args->cdr, "port? recieved too many arguments\n");
	if(FILE_PORT == args->car->type) return cell_t;
	return cell_f;
}

struct cell* symbolp(struct cell* args)
{
	require(nil != args, "symbol? requires arguments\n");
	require(nil == args->cdr, "symbol? recieved too many arguments\n");

	if(nil == args->car) return cell_f;
	if(SYM == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_charp(struct cell* args)
{
	require(nil != args, "char? requires arguments\n");
	require(nil == args->cdr, "char? recieved too many arguments\n");
	if(CHAR == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_intp(struct cell* args)
{
	require(nil != args, "number? requires arguments\n");
	require(nil == args->cdr, "number? recieved too many arguments\n");
	if(INT == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_primitivep(struct cell* args)
{
	require(nil != args, "primitive? requires arguments\n");
	require(nil == args->cdr, "primitive? recieved too many arguments\n");
	if(PRIMOP == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_procedurep(struct cell* args)
{
	require(nil != args, "procedure? requires arguments\n");
	require(nil == args->cdr, "procedure? recieved too many arguments\n");
	if(LAMBDA == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_eofp (struct cell* args)
{
	require(nil != args, "eof? requires arguments\n");
	require(nil == args->cdr, "eof? recieved too many arguments\n");

	if(EOF_object == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_definedp(struct cell* args)
{
	require(nil != args, "defined? requires arguments\n");
	require(nil == args->cdr, "defined? recieved too many arguments\n");
	require(SYM == args->car->type, "defined? did not receive a symbol\n");

	struct cell* hold = assoc(args->car, g_env);
	if(NULL != hold->cdr) return cell_t;
	return cell_f;
}

struct cell* builtin_sum(struct cell* args)
{
	if(nil == args) return make_int(0);

	int sum;
	for(sum = 0; nil != args; args = args->cdr)
	{
		require(INT == args->car->type, "builtin_sum require integers\n");
		sum = sum + args->car->value;
	}
	return make_int(sum);
}

struct cell* builtin_sub(struct cell* args)
{
	require(nil != args, "builtin_sub requires arguments\n");

	require(INT == args->car->type, "builtin_sub require integers\n");
	int sum = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(INT == args->car->type, "builtin_sub require integers\n");
		sum = sum - args->car->value;
	}
	return make_int(sum);
}

struct cell* builtin_prod(struct cell* args)
{
	if(nil == args) make_int(1);

	int prod;
	for(prod = 1; nil != args; args = args->cdr)
	{
		require(INT == args->car->type, "builtin_prod require integers\n");
		prod = prod * args->car->value;
	}
	return make_int(prod);
}

struct cell* builtin_div(struct cell* args)
{
	require(nil != args, "builtin_div requires arguments\n");

	require(INT == args->car->type, "builtin_div require integers\n");
	SCM div = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(INT == args->car->type, "builtin_div require integers\n");
		div = div / args->car->value;
	}
	return make_int(div);
}

struct cell* builtin_mod(struct cell* args)
{
	require(nil != args, "modulo requires 2 arguments\n");
	require(INT == args->car->type, "modulo first argument not an integer\n");
	require(nil != args->cdr, "modulo did not recieve a second argument\n");
	require(INT == args->cdr->car->type, "modulo second argument not an integer\n");

	SCM mod = args->car->value;
	mod = mod % args->cdr->car->value;
	if((0 > args->car->value) ^ (0 > args->cdr->car->value))
	{
		mod = mod + args->cdr->car->value;
	}

	require(nil == args->cdr->cdr, "wrong number of arguments to modulo\n");

	return make_int(mod);
}

struct cell* builtin_rem(struct cell* args)
{
	require(nil != args, "remainder requires 2 arguments\n");
	require(INT == args->car->type, "remainder first argument not an integer\n");
	require(nil != args->cdr, "remainder did not recieve a second argument\n");
	require(INT == args->cdr->car->type, "remainder second argument not an integer\n");

	SCM rem = args->car->value;
	rem = rem % args->cdr->car->value;
	if(0 > args->cdr->car->value)
	{
		rem = rem + args->cdr->car->value;
	}

	require(nil == args->cdr->cdr, "wrong number of arguments to remainder\n");
	return make_int(rem);
}

struct cell* builtin_ash(struct cell* args)
{
	require(nil != args, "ash requires 2 arguments\n");
	require(INT == args->car->type, "ash first argument not an integer\n");
	require(nil != args->cdr, "ash did not recieve a second argument\n");
	require(INT == args->cdr->car->type, "ash second argument not an integer\n");
	require(nil == args->cdr->cdr, "wrong number of arguments to ash\n");

	long ash = args->car->value;
	int count = args->cdr->car->value;
	if(count < 0)
	{
		count = -count;
		ash = ash >> count;
	}
	else
	{
		ash = ash << count;
	}
	return make_int(ash);
}

struct cell* builtin_logand(struct cell* args)
{
	long n = -1;

	while(nil != args)
	{
		require(INT == args->car->type, "builtin_logand require integers\n");
		n = n & args->car->value;
		args = args->cdr;
	}
	return make_int(n);
}

struct cell* builtin_logor(struct cell* args)
{
	long n = 0;

	while(nil != args)
	{
		require(INT == args->car->type, "builtin_logior require integers\n");
		n = n | args->car->value;
		args = args->cdr;
	}
	return make_int(n);
}

struct cell* builtin_xor(struct cell* args)
{
	long n = 0;
	while(nil != args)
	{
		require(INT == args->car->type, "builtin_logxor require integers\n");
		n = n ^ args->car->value;
		args = args->cdr;
	}
	return make_int(n);
}

struct cell* builtin_lognot(struct cell* args)
{
	require(nil != args, "lognot requires 1 argument\n");
	require(INT == args->car->type, "lognot first argument not an integer\n");
	require(nil == args->cdr, "lognot recieved wrong number of arguments\n");

	return make_int(~args->car->value);
}

struct cell* builtin_not(struct cell* args)
{
	require(nil != args, "not requires 1 argument\n");
	require(nil == args->cdr, "not recieved wrong number of arguments\n");

	if(cell_f == args->car) return cell_t;
	return cell_f;
}

struct cell* builtin_and(struct cell* args)
{
	require(nil != args, "and requires arguments\n");
	while(nil != args)
	{
		if(cell_t != args->car) return cell_f;
		args = args->cdr;
	}
	return cell_t;
}

struct cell* builtin_or(struct cell* args)
{
	require(nil != args, "or requires arguments\n");
	while(nil != args)
	{
		if(cell_t == args->car) return cell_t;
		args = args->cdr;
	}
	return cell_f;
}

struct cell* builtin_numgt(struct cell* args)
{
	require(nil != args, "builtin_numgt requires arguments\n");
	require(INT == args->car->type, "builtin_numgt require integers\n");

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(INT == args->car->type, "builtin_numgt require integers\n");
		if(temp <= args->car->value)
		{
			return cell_f;
		}
		temp = args->car->value;
	}
	return cell_t;
}

struct cell* builtin_numge(struct cell* args)
{
	require(nil != args, "builtin_numge requires arguments\n");
	require(INT == args->car->type, "builtin_numge require integers\n");

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(INT == args->car->type, "builtin_numge require integers\n");
		if(temp < args->car->value)
		{
			return cell_f;
		}
		temp = args->car->value;
	}
	return cell_t;
}

struct cell* builtin_numle(struct cell* args)
{
	require(nil != args, "builtin_numle requires arguments\n");
	require(INT == args->car->type, "builtin_numle require integers\n");

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(INT == args->car->type, "builtin_numle require integers\n");
		if(temp > args->car->value)
		{
			return cell_f;
		}
		temp = args->car->value;
	}
	return cell_t;
}

struct cell* builtin_numlt(struct cell* args)
{
	require(nil != args, "builtin_numlt requires arguments\n");
	require(INT == args->car->type, "builtin_numlt require integers\n");

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(INT == args->car->type, "builtin_numlt require integers\n");
		if(temp >= args->car->value)
		{
			return cell_f;
		}
		temp = args->car->value;
	}
	return cell_t;
}

struct cell* builtin_chareq(struct cell* args)
{
	require(nil != args, "char=? requires arguments\n");

	require(CHAR == args->car->type, "char=? received non-char\n");
	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(CHAR == args->car->type, "char=? received non-char\n");
		if(temp != args->car->value) return cell_f;
	}
	return cell_t;
}

struct cell* builtin_numeq(struct cell* args)
{
	require(nil != args, "= requires arguments\n");

	require(INT == args->car->type, "= received non-integer\n");
	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(INT == args->car->type, "= received non-integer\n");
		if(temp != args->car->value) return cell_f;
	}
	return cell_t;
}

struct cell* builtin_eq(struct cell* args)
{
	if(nil == args) return cell_t;
	if(nil == args->cdr) return cell_t;
	struct cell* temp = args->car;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		if(temp == args->car) continue;
		else if(temp->type != args->car->type) return cell_f;
		else if((INT == temp->type) || (CHAR == temp->type))
		{
			if(temp->value != args->car->value) return cell_f;
		}
		else if(STRING == temp->type)
		{
			if(temp != args->car) return cell_f;
		}
		else if(CONS == temp->type)
		{
			if(temp != args->car) return cell_f;
		}
		else return cell_f;
	}

	return cell_t;
}

struct cell* equal(struct cell* a, struct cell* b)
{
	if(a == b) return cell_t;
	if(NULL == a) return cell_f;
	if(NULL == b) return cell_f;

	if(a->type != b->type) return cell_f;
	if((INT == a->type) || (CHAR == a->type))
	{
		if(a->value != b->value) return cell_f;
		return cell_t;
	}
	else if(STRING == a->type)
	{
		return string_eq(a, b);
	}
	else if(VECTOR == a->type)
	{
		return vector_equal(a, b);
	}
	else if(CONS == a->type)
	{
		if(cell_t != equal(a->car, b->car)) return cell_f;
		if(cell_t != equal(a->cdr, b->cdr)) return cell_f;
		return cell_t;
	}

	return cell_f;
}

struct cell* builtin_equal(struct cell* args)
{
	require(nil != args, "equal? requires arguments\n");

	struct cell* temp = args->car;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		if(cell_t != equal(temp, args->car))
		{
			return cell_f;
		}
	}
	return cell_t;
}

struct cell* builtin_freecell(struct cell* args)
{
	if(nil == args)
	{
		file_print("Remaining Cells: ", stdout);
		file_print(numerate_number(left_to_take), stdout);
		return nil;
	}
	return make_int(left_to_take);
}

struct cell* builtin_number_to_char(struct cell* args)
{
	require(nil != args, "integer->char requires an argument\n");
	require(nil == args->cdr, "integer->char only supports a single argument\n");
	require(INT == args->car->type, "integer->char requires an integer\n");
	return make_char(args->car->value);
}

struct cell* builtin_char_to_number(struct cell* args)
{
	require(nil != args, "char->integer requires an argument\n");
	require(nil == args->cdr, "char->integer only supports a single argument\n");
	require(CHAR == args->car->type, "char->integer requires a char\n");
	return make_int(args->car->value);
}

struct cell* builtin_primitive_load(struct cell* args)
{
	require(nil != args, "primitive-load requires an argument\n");
	require(STRING == args->car->type, "primitive-load requires a string\n");
	require(nil == args->cdr, "primitive-load only accepts one argument\n");

	struct cell* r = load_file(args->car->string);
	require(cell_t == r, "primitive-load failed to open file\n");
	return cell_unspecified;
}

struct cell* builtin_read_byte(struct cell* args)
{
	if(nil == args) return make_char(fgetc(__stdin->file));
	else if(FILE_PORT == args->car->type)
	{
		int c = fgetc(args->car->file);
		if(EOF == c) return make_eof();
		return make_char(c);
	}
	return nil;
}

struct cell* builtin_halt(struct cell* args)
{
	exit(args->car->value);
}

struct cell* builtin_cons(struct cell* args)
{
	require(nil != args, "cons requires arguments\n");
	require(nil != args->cdr, "cons requires 2 arguments\n");
	require(nil == args->cdr->cdr, "cons recieved too many arguments\n");
	return make_cons(args->car, args->cdr->car);
}

struct cell* builtin_car(struct cell* args)
{
	require(nil != args, "car requires arguments\n");
	require(CONS == args->car->type, "car expects a pair\n");
	require(nil == args->cdr, "car expects only a single argument\n");
	return args->car->car;
}

struct cell* builtin_cdr(struct cell* args)
{
	require(nil != args, "cdr requires arguments\n");
	require(CONS == args->car->type, "cdr expects a pair\n");
	require(nil == args->cdr, "cdr expects only a single argument\n");
	return args->car->cdr;
}

struct cell* builtin_setcar(struct cell* args)
{
	require(nil != args, "set-car! requires arguments\n");
	require(CONS == args->car->type, "set-car! requires a mutable pair\n");
	require(nil != args->cdr, "set-car! requires something to set car to\n");
	args->car->car = args->cdr->car;
	require(nil == args->cdr->cdr, "set-car! received too many arguements\n");
	return NULL;
}

struct cell* builtin_setcdr(struct cell* args)
{
	require(nil != args, "set-cdr! requires arguments\n");
	require(CONS == args->car->type, "set-cdr! requires a mutable pair\n");
	require(nil != args->cdr, "set-cdr! requires something to set cdr to\n");
	args->car->cdr = args->cdr->car;
	require(nil == args->cdr->cdr, "set-cdr! received too many arguements\n");
	return NULL;
}
