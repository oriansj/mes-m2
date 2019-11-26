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
int list_length(struct cell* args);
struct cell* append(struct cell* a, struct cell* b);
struct cell* apply(struct cell* proc, struct cell* vals);
struct cell* extend(struct cell* env, struct cell* symbol, struct cell* value);
struct cell* list_equal(struct cell* a, struct cell* b);
struct cell* list_to_vector(struct cell* i);
struct cell* make_cell(int type, struct cell* a, struct cell* b, struct cell* env);
struct cell* make_char(int a);
struct cell* make_eof();
struct cell* make_file(FILE* a);
struct cell* make_int(int a);
struct cell* make_prim(void* fun);
struct cell* make_proc(struct cell* a, struct cell* b, struct cell* env);
struct cell* make_string(char* a);
struct cell* make_sym(char* name);
struct cell* make_sym(char* name);
struct cell* make_vector(int count, struct cell* init);
struct cell* prim_display(struct cell* args, FILE* out);
struct cell* prim_write(struct cell* args, FILE* out);
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
	if(PROC == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_eofp (struct cell* args)
{
	require(nil != args, "mes_builtin.c: eof? requires arguments\n");
	require(nil == args->cdr, "mes_builtin.c: eof? recieved too many arguments\n");

	if(EOF_object == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_sum(struct cell* args)
{
	if(nil == args) return nil;

	int sum;
	for(sum = 0; nil != args; args = args->cdr)
	{
		sum = sum + args->car->value;
	}
	return make_int(sum);
}

struct cell* builtin_sub(struct cell* args)
{
	if(nil == args) return nil;

	int sum = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		 sum = sum - args->car->value;
	}
	return make_int(sum);
}

struct cell* builtin_prod(struct cell* args)
{
	if(nil == args) return nil;

	int prod;
	for(prod = 1; nil != args; args = args->cdr)
	{
		prod = prod * args->car->value;
	}
	return make_int(prod);
}

struct cell* builtin_div(struct cell* args)
{
	if(nil == args) return make_int(1);

	int div = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
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

	int mod = args->car->value % args->cdr->car->value;
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

	int rem = args->car->value % args->cdr->car->value;
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
	if(nil == args) return nil;

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
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
	if(nil == args) return nil;

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
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
	if(nil == args) return nil;

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
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
	if(nil == args) return nil;

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
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

struct cell* builtin_display(struct cell* args)
{
	if(nil == args->cdr)
	{
		prim_display(args, __stdout);
		return NULL;
	}
	else if(FILE_PORT == args->cdr->car->type)
	{
		prim_display(args, args->cdr->car->file);
		return NULL;
	}

	file_print("You passed something that isn't a file pointer to write in position 2\n", stderr);
	exit(EXIT_FAILURE);
}

struct cell* builtin_display_error(struct cell* args)
{
	if(nil == args->cdr)
	{
		prim_display(args, __stderr);
		return NULL;
	}
	else if(FILE_PORT == args->cdr->car->type)
	{
		prim_display(args, args->cdr->car->file);
		return NULL;
	}

	file_print("You passed something that isn't a file pointer to write in position 2\n", stderr);
	exit(EXIT_FAILURE);
}

struct cell* builtin_write(struct cell* args)
{
	if(nil == args->cdr)
	{
		prim_write(args, __stdout);
		return NULL;
	}
	else if(FILE_PORT == args->cdr->car->type)
	{
		prim_write(args, args->cdr->car->file);
		return NULL;
	}

	file_print("You passed something that isn't a file pointer to write in position 2\n", stderr);
	exit(EXIT_FAILURE);
}

struct cell* builtin_write_error(struct cell* args)
{
	if(nil == args->cdr)
	{
		return prim_write(args, __stderr);
	}
	else if(FILE_PORT == args->cdr->car->type)
	{
		return prim_write(args, args->cdr->car->file);
	}

	file_print("You passed something that isn't a file pointer to write in position 2\n", stderr);
	exit(EXIT_FAILURE);
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
	if(nil == args) return nil;

	if(STRING == args->car->type)
	{
		return string_to_list(args->car->string);
	}
	return nil;
}

struct cell* builtin_list_length(struct cell* args)
{
	if(nil == args) return nil;
	return make_int(list_length(args));
}

struct cell* builtin_list_to_string(struct cell* args)
{
	if(nil == args) return nil;
	return make_string(list_to_string(args));
}

struct cell* builtin_vector_length(struct cell* args)
{
	return make_int(args->car->value);
}

struct cell* builtin_vector_ref(struct cell* args)
{
	if(nil == args) return nil;
	return vector_ref(args->car, args->cdr->car->value);
}

struct cell* builtin_vector_set(struct cell* args)
{
	if(nil == args) return nil;
	return vector_set(args->car, args->cdr->car->value, args->cdr->cdr->car);
}

struct cell* builtin_vector_to_list(struct cell* args)
{
	if(nil == args) return nil;
	return vector_to_list(args->car);
}

struct cell* builtin_list_to_vector(struct cell* args)
{
	if(nil == args) return nil;
	return list_to_vector(args->car);
}

struct cell* builtin_string_size(struct cell* args)
{
	if(nil == args) return nil;
	return string_length(args->car);
}

struct cell* builtin_open(struct cell* args, char* mode)
{
	require(nil != args, "Did not recieve a file name\n");
	require(STRING == args->car->type, "File name must be a string\n");

	return make_file(open_file(args->car->string, mode));
}


struct cell* builtin_open_read(struct cell* args)
{
	return builtin_open(args, "r");
}

struct cell* builtin_open_write(struct cell* args)
{
	return builtin_open(args, "w");
}

struct cell* builtin_set_current_output_port(struct cell* args)
{
	__stdout = args->car->file;
	return NULL;
}

struct cell* builtin_read_byte(struct cell* args)
{
	if(nil == args) return make_char(fgetc(__stdin));
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

void spinup(struct cell* sym, struct cell* prim)
{
	all_symbols = make_cons(sym, all_symbols);
	top_env = extend(top_env, sym, prim);
}

/*** Initialization ***/
void init_sl3()
{
	/* Special symbols */
	nil = make_sym("()");
	cell_t = make_sym("#t");
	cell_f = make_sym("#f");
	cell_dot = make_sym(".");
	quote = make_sym("quote");
	quasiquote = make_sym("quasiquote");
	unquote = make_sym("unquote");
	unquote_splicing = make_sym("unquote-splicing");
	cell_unspecified = make_sym("#<unspecified>");
	s_if = make_sym("if");
	s_cond = make_sym("cond");
	s_lambda = make_sym("lambda");
	s_define = make_sym("define");
	s_setb = make_sym("set!");
	s_begin = make_sym("begin");
	s_let = make_sym("let");
	s_while = make_sym("while");

	/* Globals of interest */
	all_symbols = make_cons(nil, nil);
	top_env = extend(nil, nil, nil);

	/* Add Eval Specials */
	spinup(cell_t, cell_t);
	spinup(cell_f, cell_f);
	spinup(cell_dot, cell_dot);
	spinup(quote, quote);
	spinup(quasiquote, quasiquote);
	spinup(unquote, unquote);
	spinup(unquote_splicing, unquote_splicing);
	spinup(cell_unspecified, cell_unspecified);
	spinup(s_if, s_if);
	spinup(s_cond, s_cond);
	spinup(s_lambda, s_lambda);
	spinup(s_define, s_define);
	spinup(s_setb, s_setb);
	spinup(s_begin, s_begin);
	spinup(s_let, s_let);
	spinup(s_while, s_while);

	/* Add Primitive Specials */
	/* checking type */
	spinup(make_sym("char?"), make_prim(builtin_charp));
	spinup(make_sym("eof-object?"), make_prim(builtin_eofp));
	spinup(make_sym("list?"), make_prim(builtin_listp));
	spinup(make_sym("null?"), make_prim(nullp));
	spinup(make_sym("pair?"), make_prim(pairp));
	spinup(make_sym("port?"), make_prim(portp));
	spinup(make_sym("primitive?"), make_prim(builtin_primitivep));
	spinup(make_sym("procedure?"), make_prim(builtin_procedurep));
	spinup(make_sym("string?"), make_prim(builtin_stringp));
	spinup(make_sym("symbol?"), make_prim(symbolp));
	spinup(make_sym("vector?"), make_prim(builtin_vectorp));

	/* Comparisions */
	spinup(make_sym("<"), make_prim(builtin_numlt));
	spinup(make_sym("<="), make_prim(builtin_numle));
	spinup(make_sym("="), make_prim(builtin_numeq));
	spinup(make_sym(">"), make_prim(builtin_numgt));
	spinup(make_sym(">="), make_prim(builtin_numge));
	spinup(make_sym("char=?"), make_prim(builtin_chareq));
	spinup(make_sym("string=?"), make_prim(builtin_stringeq));
	spinup(make_sym("eq?"), make_prim(builtin_eq));
	spinup(make_sym("equal?"), make_prim(builtin_equal));

	/* Math */
	spinup(make_sym("*"), make_prim(builtin_prod));
	spinup(make_sym("+"), make_prim(builtin_sum));
	spinup(make_sym("-"), make_prim(builtin_sub));
	spinup(make_sym("ash"), make_prim(builtin_ash));
	spinup(make_sym("logand"), make_prim(builtin_logand));
	spinup(make_sym("logior"), make_prim(builtin_logor));
	spinup(make_sym("lognot"), make_prim(builtin_lognot));
	spinup(make_sym("logxor"), make_prim(builtin_xor));
	spinup(make_sym("modulo"), make_prim(builtin_mod));
	spinup(make_sym("quotient"), make_prim(builtin_div));
	spinup(make_sym("remainder"), make_prim(builtin_rem));

	/* Files */
	spinup(make_sym("open-input-file"), make_prim(builtin_open_read));
	spinup(make_sym("open-output-file"), make_prim(builtin_open_write));
	spinup(make_sym("set-current-output-port"), make_prim(builtin_set_current_output_port));
	spinup(make_sym("display"), make_prim(builtin_display));
	spinup(make_sym("display-error"), make_prim(builtin_display_error));
	spinup(make_sym("write"), make_prim(builtin_write));
	spinup(make_sym("read-char"), make_prim(builtin_read_byte));

	/* Dealing with Lists */
	spinup(make_sym("list"), make_prim(builtin_list));
	spinup(make_sym("append"), make_prim(builtin_append));
	spinup(make_sym("list-length"), make_prim(builtin_list_length));
	spinup(make_sym("list->string"), make_prim(builtin_list_to_string));
	spinup(make_sym("list->vector"), make_prim(builtin_list_to_vector));

	/* Deal with Vectors */
	spinup(make_sym("make-vector"), make_prim(builtin_make_vector));
	spinup(make_sym("vector-length"), make_prim(builtin_vector_length));
	spinup(make_sym("vector-set!"), make_prim(builtin_vector_set));
	spinup(make_sym("vector-ref"), make_prim(builtin_vector_ref));
	spinup(make_sym("vector->list"), make_prim(builtin_vector_to_list));

	/* Deal with Strings */
	spinup(make_sym("string->list"), make_prim(builtin_string_to_list));
	spinup(make_sym("string-length"), make_prim(builtin_string_size));

	/* Deal with logicals */
	spinup(make_sym("not"), make_prim(builtin_not));
	spinup(make_sym("and"), make_prim(builtin_and));
	spinup(make_sym("or"), make_prim(builtin_or));

	/* Lisp classics */
	spinup(make_sym("cons"), make_prim(builtin_cons));
	spinup(make_sym("car"), make_prim(builtin_car));
	spinup(make_sym("cdr"), make_prim(builtin_cdr));
	spinup(make_sym("set-car!"), make_prim(builtin_setcar));
	spinup(make_sym("set-cdr!"), make_prim(builtin_setcdr));
	spinup(make_sym("apply"), make_prim(builtin_apply));
	spinup(make_sym("exit"), make_prim(builtin_halt));

	/* MES unique */
	spinup(make_sym("free_mem"), make_prim(builtin_freecell));
	spinup(make_sym("%version"), make_string("0.19"));
	spinup(make_sym("vector=?"), make_prim(builtin_vectoreq));
	spinup(make_sym("list=?"), make_prim(builtin_listeq));
}
