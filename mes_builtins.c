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
char* list_to_string(struct cell* args);
char* ntoab(SCM x, int base, int signed_p);
int REPL();
int list_length(struct cell* args);
struct cell* append(struct cell* a, struct cell* b);
struct cell* apply(struct cell* proc, struct cell* vals);
struct cell* assoc(struct cell* key, struct cell* alist);
struct cell* extend(struct cell* env, struct cell* symbol, struct cell* value);
struct cell* list_equal(struct cell* a, struct cell* b);
struct cell* list_to_vector(struct cell* i);
struct cell* make_cell(int type, struct cell* a, struct cell* b, struct cell* env);
struct cell* make_char(int a);
struct cell* make_eof();
struct cell* make_file(FILE* a, char* name);
struct cell* make_int(int a);
struct cell* make_record(struct cell* type, struct cell* vector);
struct cell* make_record_type(char* name, struct cell* list);
struct cell* make_string(char* a);
struct cell* make_sym(char* name);
struct cell* make_vector(int count, struct cell* init);
struct cell* record_construct(struct cell* type, struct cell* list_args, struct cell* list_vals);
struct cell* record_ref(struct cell* type, char* name, struct cell* record);
struct cell* record_set(struct cell* type, char* name, struct cell* record, struct cell* value);
struct cell* string_eq(struct cell* a, struct cell* b);
struct cell* string_length(struct cell* a);
struct cell* string_to_list(char* string);
struct cell* vector_equal(struct cell* a, struct cell* b);
struct cell* vector_ref(struct cell* a, int i);
struct cell* vector_set(struct cell* v, int i, struct cell* e);
struct cell* vector_to_list(struct cell* a);
void garbage_collect();

/*** Primitives ***/
struct cell* builtin_apply(struct cell* args)
{
	require(nil != args, "mes_builtin.c: apply requires arguments\n");
	require(nil != args->cdr, "mes_builtin.c: apply recieved insufficient arguments\n");
	require(CONS == args->cdr->car->type, "mes_builtin.c: apply did not recieve a list\n");
	return apply(args->car, args->cdr->car);
}

struct cell* builtin_make_record_type(struct cell* args)
{
	require(nil != args, "mes_builtin.c: make-record-type requires arguments\n");
	require(nil != args->cdr, "mes_builtin.c: make-record-type received insufficient arguments\n");
	require(STRING == args->car->type, "mes_builtin.c: make-record-type did not receive a string\n");
	require(CONS == args->cdr->car->type, "mes_builtin.c: make-record-type did not receive a list\n");
	return make_record_type(args->car->string, args->cdr->car);
}

struct cell* builtin_make_record(struct cell* args)
{
	require(nil != args, "mes_builtin.c: make-record requires arguments\n");
	require(nil != args->cdr, "mes_builtin.c: make-record received insufficient arguments\n");
	require(RECORD_TYPE == args->car->type, "mes_builtin.c: make-record did not receive a string\n");
	require(VECTOR == args->cdr->car->type, "mes_builtin.c: make-record did not receive a vector\n");
	return make_record(args->car, args->cdr->car);
}

struct cell* builtin_record_type_name(struct cell* args)
{
	require(nil != args, "mes_builtin.c: record-type-name requires an argument\n");
	require(nil == args->cdr, "mes_builtin.c: record-type-name received too many arguments\n");
	require(RECORD_TYPE == args->car->type, "mes_builtin.c: record-type-name did not receive a record-type\n");
	return make_string(args->car->string);
}

struct cell* builtin_record_type_fields(struct cell* args)
{
	require(nil != args, "mes_builtin.c: record-type-fields requires an argument\n");
	require(nil == args->cdr, "mes_builtin.c: record-type-fields received too many arguments\n");
	require(RECORD_TYPE == args->car->type, "mes_builtin.c: record-type-fields did not receive a record-type\n");
	return args->car->cdr->cdr;
}

struct cell* builtin_record_typep(struct cell* args)
{
	require(nil != args, "mes_builtin.c: record-type? requires an argument\n");
	require(nil == args->cdr, "mes_builtin.c: record-type? received too many arguments\n");
	if(RECORD_TYPE == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_recordp(struct cell* args)
{
	require(nil != args, "mes_builtin.c: record? requires an argument\n");
	require(nil == args->cdr, "mes_builtin.c: record? received too many arguments\n");
	if(RECORD == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_record_type_descriptor(struct cell* args)
{
	require(nil != args, "mes_builtin.c: record-type-descriptor requires an argument\n");
	require(nil == args->cdr, "mes_builtin.c: record-type-descriptor received too many arguments\n");
	require(RECORD == args->car->type, "mes_builtin.c: record-type-descriptor did not receive a record\n");
	return args->car->car;
}

struct cell* builtin_record_predicate(struct cell* args)
{
	require(nil != args, "mes_builtin.c: core:record-predicate requires an argument\n");
	require(nil != args->cdr, "mes_builtin.c: core:record-predicate received insufficient arguments\n");
	if(RECORD_TYPE == args->car->type)
	{
		if(RECORD == args->cdr->car->type)
		{
			if(args->cdr->car->car == args->car) return cell_t;
		}
	}
	return cell_f;
}

struct cell* builtin_record_accessor(struct cell* args)
{
	require(nil != args, "mes_builtin.c: core:record-accessor requires arguments\n");
	require(nil != args->cdr, "mes_builtin.c: core:record-accessor requires more arguments\n");
	require(nil != args->cdr->cdr, "mes_builtin.c: core:record-accessor requires more arguments\n");
	require(RECORD_TYPE == args->car->type, "mes_builtin.c: core:record-accessor did not receive RECORD-TYPE\n");
	require(SYM == args->cdr->car->type, "mes_builtin.c: core:record-accessor did not receive SYMBOL\n");
	require(RECORD == args->cdr->cdr->car->type, "mes_builtin.c: core:record-accessor did not receive RECORD\n");
	require(args->cdr->cdr->car->car == args->car, "mes_builtin.c: core:record-accessor got a record of a type different than record-type\n");
	return record_ref(args->car, args->cdr->car->string, args->cdr->cdr->car);
}

struct cell* builtin_record_modifier(struct cell* args)
{
	require(nil != args, "mes_builtin.c: core:record-modifier requires arguments\n");
	require(nil != args->cdr, "mes_builtin.c: core:record-modifier requires more arguments\n");
	require(nil != args->cdr->cdr, "mes_builtin.c: core:record-modifier requires more arguments\n");
	require(nil != args->cdr->cdr->cdr, "mes_builtin.c: core:record-modifier requires more arguments\n");
	require(RECORD_TYPE == args->car->type, "mes_builtin.c: core:record-modifier did not receive RECORD-TYPE\n");
	require(SYM == args->cdr->car->type, "mes_builtin.c: core:record-modifier did not receive SYMBOL\n");
	require(RECORD == args->cdr->cdr->car->type, "mes_builtin.c: core:record-modifier did not receive RECORD\n");
	require(args->cdr->cdr->car->car == args->car, "mes_builtin.c: core:record-modifier got a record of a type different than record-type\n");
	return record_set(args->car, args->cdr->car->string, args->cdr->cdr->car, args->cdr->cdr->cdr->car);
}

struct cell* builtin_record_constructor(struct cell* args)
{
	require(nil != args, "mes_builtin.c: core:record-constructor requires arguments\n");
	require(nil != args->cdr, "mes_builtin.c: core:record-constructor requires more arguments\n");
	require(nil != args->cdr->cdr, "mes_builtin.c: core:record-constructor requires more arguments\n");
	require(RECORD_TYPE == args->car->type, "mes_builtin.c: core:record-constructor did not receive RECORD-TYPE\n");
	require(CONS == args->cdr->car->type, "mes_builtin.c: core:record-constructor did not receive argument list\n");
	require(CONS == args->cdr->cdr->car->type, "mes_builtin.c: core:record-constructor did not receive argument list\n");
	return record_construct(args->car, args->cdr->car, args->cdr->cdr->car);
}

struct cell* nullp(struct cell* args)
{
	require(nil != args, "mes_builtin.c: null? requires arguments\n");
	require(nil == args->cdr, "mes_builtin.c: null? recieved too many arguments\n");
	if(nil == args->car) return cell_t;
	return cell_f;
}

struct cell* pairp(struct cell* args)
{
	require(nil != args, "mes_builtin.c: pair? requires arguments\n");
	require(nil == args->cdr, "mes_builtin.c: pair? recieved too many arguments\n");
	if(CONS == args->car->type) return cell_t;
	return cell_f;
}

struct cell* portp(struct cell* args)
{
	require(nil != args, "mes_builtin.c: port? requires arguments\n");
	require(nil == args->cdr, "mes_builtin.c: port? recieved too many arguments\n");
	if(FILE_PORT == args->car->type) return cell_t;
	return cell_f;
}

struct cell* symbolp(struct cell* args)
{
	require(nil != args, "mes_builtin.c: symbol? requires arguments\n");
	require(nil == args->cdr, "mes_builtin.c: symbol? recieved too many arguments\n");

	if(nil == args->car) return cell_f;
	if(SYM == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_stringp(struct cell* args)
{
	require(nil != args, "mes_builtin.c: string? requires arguments\n");
	require(nil == args->cdr, "mes_builtin.c: string? recieved too many arguments\n");
	if(STRING == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_charp(struct cell* args)
{
	require(nil != args, "mes_builtin.c: char? requires arguments\n");
	require(nil == args->cdr, "mes_builtin.c: char? recieved too many arguments\n");
	if(CHAR == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_listp(struct cell* args)
{
	require(nil != args, "mes_builtin.c: list? requires arguments\n");
	require(nil == args->cdr, "mes_builtin.c: list? recieved too many arguments\n");

	struct cell* i = args->car;
	while(nil != i)
	{
		if(CONS != i->type) return cell_f;
		i = i->cdr;
	}

	return cell_t;
}

struct cell* builtin_intp(struct cell* args)
{
	require(nil != args, "mes_builtin.c: number? requires arguments\n");
	require(nil == args->cdr, "mes_builtin.c: number? recieved too many arguments\n");
	if(INT == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_vectorp(struct cell* args)
{
	require(nil != args, "mes_builtin.c: vector? requires arguments\n");
	require(nil == args->cdr, "mes_builtin.c: vector? recieved too many arguments\n");
	if(VECTOR == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_primitivep(struct cell* args)
{
	require(nil != args, "mes_builtin.c: primitive? requires arguments\n");
	require(nil == args->cdr, "mes_builtin.c: primitive? recieved too many arguments\n");
	if(PRIMOP == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_procedurep(struct cell* args)
{
	require(nil != args, "mes_builtin.c: procedure? requires arguments\n");
	require(nil == args->cdr, "mes_builtin.c: procedure? recieved too many arguments\n");
	if(LAMBDA == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_eofp (struct cell* args)
{
	require(nil != args, "mes_builtin.c: eof? requires arguments\n");
	require(nil == args->cdr, "mes_builtin.c: eof? recieved too many arguments\n");

	if(EOF_object == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_definedp(struct cell* args)
{
	require(nil != args, "mes_builtin.c: defined? requires arguments\n");
	require(nil == args->cdr, "mes_builtin.c: defined? recieved too many arguments\n");
	require(SYM == args->car->type, "mes_builtin.c: defined? did not receive a symbol\n");

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
		require(INT == args->car->type, "mes_builtin.c: builtin_sum require integers\n");
		sum = sum + args->car->value;
	}
	return make_int(sum);
}

struct cell* builtin_sub(struct cell* args)
{
	require(nil != args, "mes_builtin.c: builtin_sub requires arguments\n");

	require(INT == args->car->type, "mes_builtin.c: builtin_sub require integers\n");
	int sum = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(INT == args->car->type, "mes_builtin.c: builtin_sub require integers\n");
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
		require(INT == args->car->type, "mes_builtin.c: builtin_prod require integers\n");
		prod = prod * args->car->value;
	}
	return make_int(prod);
}

struct cell* builtin_div(struct cell* args)
{
	require(nil != args, "mes_builtin.c: builtin_div requires arguments\n");

	require(INT == args->car->type, "mes_builtin.c: builtin_div require integers\n");
	SCM div = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(INT == args->car->type, "mes_builtin.c: builtin_div require integers\n");
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
		require(INT == args->car->type, "mes_builtin.c: builtin_logand require integers\n");
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
		require(INT == args->car->type, "mes_builtin.c: builtin_logior require integers\n");
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
		require(INT == args->car->type, "mes_builtin.c: builtin_logxor require integers\n");
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
	require(nil != args, "mes_builtin.c: builtin_numgt requires arguments\n");
	require(INT == args->car->type, "mes_builtin.c: builtin_numgt require integers\n");

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(INT == args->car->type, "mes_builtin.c: builtin_numgt require integers\n");
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
	require(nil != args, "mes_builtin.c: builtin_numge requires arguments\n");
	require(INT == args->car->type, "mes_builtin.c: builtin_numge require integers\n");

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(INT == args->car->type, "mes_builtin.c: builtin_numge require integers\n");
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
	require(nil != args, "mes_builtin.c: builtin_numle requires arguments\n");
	require(INT == args->car->type, "mes_builtin.c: builtin_numle require integers\n");

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(INT == args->car->type, "mes_builtin.c: builtin_numle require integers\n");
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
	require(nil != args, "mes_builtin.c: builtin_numlt requires arguments\n");
	require(INT == args->car->type, "mes_builtin.c: builtin_numlt require integers\n");

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(INT == args->car->type, "mes_builtin.c: builtin_numlt require integers\n");
		if(temp >= args->car->value)
		{
			return cell_f;
		}
		temp = args->car->value;
	}
	return cell_t;
}

struct cell* builtin_append(struct cell* args)
{
	if(nil == args) return nil;
	require(((nil == args->car) || (CONS == args->car->type)), "mes_builtin.c: builtin_append require list\n");
	if(nil == args->cdr) return args->car;
	require(((nil == args->car) || (CONS == args->car->type)), "mes_builtin.c: builtin_append require list\n");
	return append(args->car, args->cdr->car);
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

struct cell* builtin_vectoreq(struct cell* args)
{
	require(nil != args, "vector=? requires arguments\n");

	require(VECTOR == args->car->type, "vector=? received non-vector\n");
	struct cell* temp = args->car;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(VECTOR == args->car->type, "vector=? received non-vector\n");
		if(cell_t != vector_equal(temp, args->car))
		{
			return cell_f;
		}
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

struct cell* builtin_make_vector(struct cell* args)
{
	require(nil != args, "make-vector requires arguments\n");
	require(INT == args->car->type, "make-vector requires a numerical argument\n");
	require(0 <= args->car->value, "make-vector requires a number >= 0\n");
	if(nil == args->cdr) return make_vector(args->car->value, cell_unspecified);

	require(nil == args->cdr->cdr, "make-vector recieved too many arguments\n");
	return make_vector(args->car->value, args->cdr->car);
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

struct cell* builtin_string_to_list(struct cell* args)
{
	require(nil != args, "mes_builtin.c: string->list requires arguments\n");
	require(STRING == args->car->type, "mes-builtin.c: string->list did not receive a string\n");

	struct cell* r = string_to_list(args->car->string);
	if(nil == args->cdr)
	{
		return r;
	}

	require(INT == args->cdr->car->type, "mes_builtin.c: string->list only accepts integers\n");
	int i = args->cdr->car->value;
	require(0 <= i, "mes_builtin.c: string->list invalid index\n");
	while(0 != i)
	{
		require(nil != r, "mes_builtin.c: string->list index too large\n");
		r = r->cdr;
		i = i - 1;
	}
	return r;
}

struct cell* builtin_list_length(struct cell* args)
{
	require(nil != args, "mes_builtins.c: list-length requires arguments\n");
	return make_int(list_length(args));
}

struct cell* builtin_list_to_string(struct cell* args)
{
	require(nil != args, "mes_builtins.c: list->string requires an argument\n");
	require(nil == args->cdr, "mes_builtins.c: list->string only allows a single argument\n");
	return make_string(list_to_string(args));
}

struct cell* builtin_list_to_symbol(struct cell* args)
{
	require(nil != args, "mes_builtins.c: list->symbol requires an argument\n");
	require(nil == args->cdr, "mes_builtins.c: list->symbol only allows a single argument\n");
	return make_sym(list_to_string(args));
}

struct cell* builtin_vector_length(struct cell* args)
{
	require(nil != args, "mes_builtins.c: vector-length requires an argument\n");
	return make_int(args->car->value);
}

struct cell* builtin_vector_ref(struct cell* args)
{
	require(nil != args, "mes_builtins.c: vector-ref requires an argument\n");
	require(nil != args->cdr, "mes_builtins.c: vector-ref requires an argument\n");
	require(nil == args->cdr->cdr, "mes_builtins.c: vector-ref received too many arguments\n");
	require(VECTOR == args->car->type, "mes_builtins.c: vector-ref did not receive vector\n");
	require(INT == args->cdr->car->type, "mes_builtins.c: vector-ref did not receive index\n");
	return vector_ref(args->car, args->cdr->car->value);
}

struct cell* builtin_vector_set(struct cell* args)
{
	require(nil != args, "mes_builtins.c: vector-set! requires an argument\n");
	require(nil != args->cdr, "mes_builtins.c: vector-set! requires a second argument\n");
	require(nil != args->cdr->cdr, "mes_builtins.c: vector-set! requires a third argument\n");
	require(nil == args->cdr->cdr->cdr, "mes_builtins.c: vector-set! recieved too many argument\n");
	require(VECTOR == args->car->type, "mes_builtins.c: vector-set! did not receive a vector\n");
	require(INT == args->cdr->car->type, "mes_builtins.c: vector-set! did not receive an index\n");
	return vector_set(args->car, args->cdr->car->value, args->cdr->cdr->car);
}

struct cell* builtin_vector_to_list(struct cell* args)
{
	require(nil != args, "mes_builtins.c: vector->list! requires an argument\n");
	require(VECTOR == args->car->type, "mes_builtins.c: vector->list! ");
	require(nil == args->cdr, "mes_builtins.c: vector-set! too many arguments\n");
	return vector_to_list(args->car);
}

struct cell* builtin_list_to_vector(struct cell* args)
{
	require(nil != args, "list->vector requires an argument\n");
	require(nil == args->cdr, "list->vector only allows a single argument\n");
	return list_to_vector(args->car);
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

	int Reached_EOF= FALSE;
	struct cell* hold =make_file(__stdin->file, __stdin->string);

	__stdin->string = args->car->string;
	__stdin->file = open_file(args->car->string, "r");
	require(NULL != __stdin->file, "primitive-load failed to open file\n");

	while(!Reached_EOF)
	{
		garbage_collect();
		Reached_EOF = REPL();
	}

	__stdin->file = hold->file;
	__stdin->string = hold->string;
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

struct cell* builtin_list(struct cell* args)
{
	/* List is stupid, just return */
	return args;
}

struct cell* reverse(struct cell* a, struct cell* b)
{
	require(CONS == a->type, "reverse did not receive a list\n");
	if(nil == a->cdr) return make_cons(a->car, b);
	require(CONS == a->cdr->type, "reverse did not receive a true list\n");
	return reverse(a->cdr, make_cons(a->car, b));
}

struct cell* builtin_reverse(struct cell* args)
{
	require(nil != args, "reverse requires arguments\n");
	require(nil == args->cdr, "reverse recieved too many arguments\n");
	return reverse(args->car, nil);
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
