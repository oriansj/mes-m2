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
struct cell* reverse_list(struct cell* head);
void garbage_collect();

/* Support functions */
struct cell* findsym(char *name)
{
	struct cell* symlist;
	for(symlist = all_symbols; nil != symlist; symlist = symlist->cdr)
	{
		if(match(name, symlist->car->string))
		{
			return symlist;
		}
	}
	return nil;
}

struct cell* intern(char *name)
{
	struct cell* op = findsym(name);
	if(nil != op) return op->car;
	op = make_sym(name);
	all_symbols = make_cons(op, all_symbols);
	return op;
}

/*** Environment ***/
struct cell* extend(struct cell* env, struct cell* symbol, struct cell* value)
{
	return make_cons(make_cons(symbol, value), env);
}

struct cell* multiple_extend(struct cell* env, struct cell* syms, struct cell* vals)
{
	if(nil == syms)
	{
		return env;
	}
	return multiple_extend(extend(env, syms->car, vals->car), syms->cdr, vals->cdr);
}

struct cell* extend_env(struct cell* sym, struct cell* val, struct cell* env)
{
	env->cdr = make_cons(env->car, env->cdr);
	env->car = make_cons(sym, val);
	return NULL;
}

struct cell* assoc(struct cell* key, struct cell* alist)
{
	if(nil == alist) return nil;
	for(; nil != alist; alist = alist->cdr)
	{
		if(alist->car->car->string == key->string) return alist->car;
	}
	return nil;
}


/*** Stack for passing of arguments ***/
void push_cell(struct cell* a)
{
	g_stack[stack_pointer] = a;
	stack_pointer = stack_pointer + 1;
}

struct cell* pop_cell()
{
	stack_pointer = stack_pointer - 1;
	struct cell* r = g_stack[stack_pointer];
	g_stack[stack_pointer] = 0;
	return r;
}

/*** Evaluator (Eval/Apply) ***/
void eval(struct cell* exp, struct cell* env);
struct cell* evlis(struct cell* exps, struct cell* env)
{
	if(exps == nil) return nil;

	eval(exps->car, env);
	struct cell* i = R0;
	struct cell* j = evlis(exps->cdr, env);
	return make_cons(i, j);
}

struct cell* progn(struct cell* exps, struct cell* env)
{
	if(exps == nil) return nil;

	struct cell* result;
progn_reset:
	eval(exps->car, env);
	result = R0;
	if(exps->cdr == nil) return result;
	exps = exps->cdr;
	goto progn_reset;
}

struct cell* apply(struct cell* proc, struct cell* vals)
{
	struct cell* temp = nil;
	if(proc->type == PRIMOP)
	{
		FUNCTION* fp = proc->function;
		temp = fp(vals);
	}
	else if(proc->type == PROC)
	{
		struct cell* env = make_cons(proc->env->car, proc->env->cdr);
		temp = progn(proc->cdr, multiple_extend(env, proc->car, vals));
	}
	else
	{
		file_print("Bad argument to apply\n", stderr);
		exit(EXIT_FAILURE);
	}
	return temp;
}

struct cell* evcond(struct cell* exp, struct cell* env)
{
	/* Return nil but the result is technically undefined per the standard */
	if(nil == exp)
	{
		return nil;
	}

	eval(exp->car->car, env);
	if(cell_t == R0)
	{
		eval(exp->car->cdr->car, env);
		return R0;
	}

	return evcond(exp->cdr, env);
}

struct cell* evwhile(struct cell* exp, struct cell* env)
{
	if(nil == exp) return nil;

	eval(exp->cdr->car, env);

	while(cell_t == R0)
	{
		eval(exp->cdr->cdr->car, env);
		eval(exp->cdr->car, env);
		if((cell_t == exp->cdr->car) && (left_to_take < 1000)) garbage_collect();
	}

	return R0;
}

struct cell* process_sym(struct cell* exp, struct cell* env);
struct cell* process_cons(struct cell* exp, struct cell* env);

void eval(struct cell* exp, struct cell* env)
{
	if(exp == nil)
	{
		R0 = nil;
		return;
	}
	if(SYM == exp->type)
	{
		R0 = process_sym(exp, env);
		return;
	}
	if(CONS == exp->type)
	{
		R0 = process_cons(exp, env);
		return;
	}
	R0 = exp;
}

struct cell* process_sym(struct cell* exp, struct cell* env)
{
	struct cell* tmp = assoc(exp, env);
	if(tmp == nil)
	{
		file_print("Unbound symbol:", stderr);
		file_print(exp->string, stderr);
		fputc('\n', stderr);
		exit(EXIT_FAILURE);
	}
	return tmp->cdr;
}

struct cell* process_if(struct cell* exp, struct cell* env)
{
	eval(exp->cdr->car, env);
	if(R0 != cell_f)
	{
		eval(exp->cdr->cdr->car, env);
		return R0;
	}

	eval(exp->cdr->cdr->cdr->car, env);
	return R0;

}

struct cell* process_setb(struct cell* exp, struct cell* env)
{
	eval(exp->cdr->cdr->car, env);
	struct cell* newval = R0;
	struct cell* pair = assoc(exp->cdr->car, env);
	pair->cdr = newval;
	return NULL;
}

struct cell* process_let(struct cell* exp, struct cell* env)
{
	struct cell* lets;
	for(lets = exp->cdr->car; lets != nil; lets = lets->cdr)
	{
		eval(lets->car->cdr->car, env);
		env = make_cons(make_cons(lets->car->car, R0), env);
	}
	return progn(exp->cdr->cdr, env);
}

struct cell* process_quasiquote(struct cell* exp, struct cell* env)
{
	struct cell* i = exp;
	struct cell* f = NULL;
	struct cell* h;
	while(nil != i)
	{
		h = i->car;
		if(CONS == i->car->type)
		{
			if(unquote == i->car->car)
			{
				eval(i->car->cdr->car, env);
				h = R0;
			}
		}
		f = make_cons(h, f);
		i = i->cdr;
	}
	i = f;
	f = reverse_list(f);
	i->cdr = nil;
	return f;
}

struct cell* process_define(struct cell* exp, struct cell* env)
{
	if(CONS == exp->cdr->car->type)
	{
		struct cell* fun = exp->cdr->cdr;
		struct cell* arguments = exp->cdr->car->cdr;
		struct cell* name = exp->cdr->car->car;
		exp->cdr = make_cons(name, make_cons(make_cons(s_lambda, make_cons(arguments, fun)), nil));
	}

	eval(exp->cdr->cdr->car, env);
	return(extend_env(exp->cdr->car, R0, env));
}

struct cell* process_cons(struct cell* exp, struct cell* env)
{
	if(exp->car == s_if) return process_if(exp, env);
	if(exp->car == s_cond) return evcond(exp->cdr, env);
	if(exp->car == s_begin) return progn(exp->cdr, env);
	if(exp->car == s_lambda) return make_proc(exp->cdr->car, exp->cdr->cdr, env);
	if(exp->car == quote) return exp->cdr->car;
	if(exp->car == quasiquote) return process_quasiquote(exp->cdr->car, env);
	if(exp->car == s_define) return process_define(exp, env);
	if(exp->car == s_setb) return process_setb(exp, env);
	if(exp->car == s_let) return process_let(exp, env);
	if(exp->car == s_while) return evwhile(exp, env);

	eval(exp->car, env);
	push_cell(R0);
	R1 = evlis(exp->cdr, env);
	R0 = pop_cell();
	return apply(R0, R1);
}
