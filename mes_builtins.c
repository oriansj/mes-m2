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
struct cell* make_vector(int count);
struct cell* prim_display(struct cell* args, FILE* out);
struct cell* prim_write(struct cell* args, FILE* out);
struct cell* string_to_list(char* string);
struct cell* vector_ref(struct cell* a, int i);
struct cell* vector_set(struct cell* v, int i, struct cell* e);
struct cell* vector_to_list(struct cell* a);
void garbage_collect();

/*** Primitives ***/
struct cell* builtin_apply(struct cell* args)
{
	return apply(args->car, args->cdr->car);
}

struct cell* builtin_atom(struct cell* args)
{
	if(CONS == args->car->type) return nil;
	return cell_t;
}

struct cell* nullp(struct cell* args)
{
	if(nil == args->car) return cell_t;
	return nil;
}

struct cell* builtin_eofp (struct cell* args)
{
	if(EOF_object == args->car->type) return cell_t;
	return nil;
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
	if(nil == args) return nil;

	int mod = args->car->value % args->cdr->car->value;
	if((0 > args->car->value) ^ (0 > args->cdr->car->value))
	{
		mod = mod + args->cdr->car->value;
	}

	if(nil != args->cdr->cdr)
	{
		file_print("wrong number of arguments to mod\n", stderr);
		exit(EXIT_FAILURE);
	}
	return make_int(mod);
}

struct cell* builtin_rem(struct cell* args)
{
	if(nil == args) return nil;

	int rem = args->car->value % args->cdr->car->value;
	if(0 > args->cdr->car->value)
	{
		rem = rem + args->cdr->car->value;
	}

	if(nil != args->cdr->cdr)
	{
		file_print("wrong number of arguments to mod\n", stderr);
		exit(EXIT_FAILURE);
	}
	return make_int(rem);
}

struct cell* builtin_ash(struct cell* args)
{
	if(nil == args) return nil;

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

struct cell* builtin_and(struct cell* args)
{
	if(nil == args) return nil;
	long n = -1;

	while(nil != args)
	{
		n = n & args->car->value;
		args = args->cdr;
	}
	return make_int(n);
}

struct cell* builtin_or(struct cell* args)
{
	if(nil == args) return nil;
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
	if(nil == args) return nil;

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
	if(nil == args) return nil;

	return make_int(~args->car->value);
}

struct cell* builtin_not(struct cell* args)
{
	if(nil == args) return nil;

	if(cell_f == args->car) return cell_t;
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

struct cell* builtin_numeq(struct cell* args)
{
	if(nil == args) return nil;

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		if(temp != args->car->value)
		{
			return cell_f;
		}
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

struct cell* builtin_listp(struct cell* args)
{
	if(nil == args) return nil;

	if(CONS == args->car->type)
	{
		return cell_t;
	}
	return cell_f;
}

struct cell* builtin_append(struct cell* args)
{
	if(nil == args) return nil;
	return append(args->car, args->cdr->car);
}

struct cell* builtin_get_type(struct cell* args)
{
	if(nil == args) return nil;
	return make_int(args->car->type);
}

struct cell* builtin_set_type(struct cell* args)
{
	if(nil == args) return nil;
	return make_cell(args->cdr->car->value, args->car->car, args->car->cdr, args->car->env);
}

struct cell* builtin_stringeq(struct cell* args)
{
	if(nil == args) return nil;

	char* temp = args->car->string;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		if(!match(temp, args->car->string))
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
	return make_vector(args->car->value);
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
	if(args->car->type != STRING)
	{
		file_print("Wrong type recieved\n", stderr);
		exit(EXIT_FAILURE);
	}
	return make_int(args->car->size);
}

struct cell* builtin_open(struct cell* args, char* mode)
{
	if(nil == args)
	{
		file_print("Did not recieve a file name\n", stderr);
		exit(EXIT_FAILURE);
	}

	if(STRING != args->car->type)
	{
		file_print("File name must be a string\n", stderr);
		exit(EXIT_FAILURE);
	}

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

struct cell* builtin_list(struct cell* args) {return args;}
struct cell* builtin_cons(struct cell* args) { return make_cons(args->car, args->cdr->car); }
struct cell* builtin_car(struct cell* args) { return args->car->car; }
struct cell* builtin_cdr(struct cell* args) { return args->car->cdr; }

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
	spinup(make_sym("apply"), make_prim(builtin_apply));
	spinup(make_sym("null?"), make_prim(nullp));
	spinup(make_sym("atom?"), make_prim(builtin_atom));
	spinup(make_sym("eof-object?"), make_prim(builtin_eofp));
	spinup(make_sym("+"), make_prim(builtin_sum));
	spinup(make_sym("-"), make_prim(builtin_sub));
	spinup(make_sym("*"), make_prim(builtin_prod));
	spinup(make_sym("quotient"), make_prim(builtin_div));
	spinup(make_sym("modulo"), make_prim(builtin_mod));
	spinup(make_sym("remainder"), make_prim(builtin_rem));
	spinup(make_sym("logand"), make_prim(builtin_and));
	spinup(make_sym("logior"), make_prim(builtin_or));
	spinup(make_sym("lognot"), make_prim(builtin_lognot));
	spinup(make_sym("logxor"), make_prim(builtin_xor));
	spinup(make_sym("not"), make_prim(builtin_not));
	spinup(make_sym("ash"), make_prim(builtin_ash));
	spinup(make_sym(">"), make_prim(builtin_numgt));
	spinup(make_sym(">="), make_prim(builtin_numge));
	spinup(make_sym("="), make_prim(builtin_numeq));
	spinup(make_sym("<="), make_prim(builtin_numle));
	spinup(make_sym("<"), make_prim(builtin_numlt));
	spinup(make_sym("open-input-file"), make_prim(builtin_open_read));
	spinup(make_sym("open-output-file"), make_prim(builtin_open_write));
	spinup(make_sym("set-current-output-port"), make_prim(builtin_set_current_output_port));
	spinup(make_sym("display"), make_prim(builtin_display));
	spinup(make_sym("display-error"), make_prim(builtin_display_error));
	spinup(make_sym("write"), make_prim(builtin_write));
	spinup(make_sym("get-type"), make_prim(builtin_get_type));
	spinup(make_sym("set-type!"), make_prim(builtin_set_type));
	spinup(make_sym("list?"), make_prim(builtin_listp));
	spinup(make_sym("list"), make_prim(builtin_list));
	spinup(make_sym("append"), make_prim(builtin_append));
	spinup(make_sym("list-length"), make_prim(builtin_list_length));
	spinup(make_sym("list->string"), make_prim(builtin_list_to_string));
	spinup(make_sym("string->list"), make_prim(builtin_string_to_list));
	spinup(make_sym("string-length"), make_prim(builtin_string_size));
	spinup(make_sym("string=?"), make_prim(builtin_stringeq));
	spinup(make_sym("cons"), make_prim(builtin_cons));
	spinup(make_sym("car"), make_prim(builtin_car));
	spinup(make_sym("cdr"), make_prim(builtin_cdr));
	spinup(make_sym("read-char"), make_prim(builtin_read_byte));
	spinup(make_sym("make-vector"), make_prim(builtin_make_vector));
	spinup(make_sym("vector-length"), make_prim(builtin_vector_length));
	spinup(make_sym("vector-set!"), make_prim(builtin_vector_set));
	spinup(make_sym("vector-ref"), make_prim(builtin_vector_ref));
	spinup(make_sym("vector->list"), make_prim(builtin_vector_to_list));
	spinup(make_sym("list->vector"), make_prim(builtin_list_to_vector));
	spinup(make_sym("exit"), make_prim(builtin_halt));

	/* MES unique */
	spinup(make_sym("free_mem"), make_prim(builtin_freecell));
}
