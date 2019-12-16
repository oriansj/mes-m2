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
struct cell* macro_apply(struct cell* exp, struct cell* vals);
struct cell* make_cell(int type, struct cell* a, struct cell* b, struct cell* env);
struct cell* make_char(int a);
struct cell* make_eof();
struct cell* make_file(FILE* a);
struct cell* make_int(int a);
struct cell* make_prim(void* fun);
struct cell* make_proc(struct cell* a, struct cell* b, struct cell* env);
struct cell* make_macro(struct cell* a, struct cell* b, struct cell* env);
struct cell* make_string(char* a);
struct cell* make_sym(char* name);
struct cell* make_sym(char* name);
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

	if(cell_dot == syms->car)
	{
		return multiple_extend(extend(env, syms->cdr->car, vals), syms->cdr->cdr, vals->cdr);
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
	struct cell* s = g_stack[stack_pointer];

	if(NULL == s)
	{
		s = make_cell(0, NULL, NULL, NULL);
		g_stack[stack_pointer] = s;
	}

	stack_pointer = stack_pointer + 1;

	/* Copy Over values */
	s->type = a->type;
	s->car = a->car;
	s->cdr = a->cdr;
	s->env = a->env;
}

struct cell* pop_cell()
{
	stack_pointer = stack_pointer - 1;
	struct cell* r = g_stack[stack_pointer];
	g_stack[stack_pointer] = NULL;

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

	R0 = exps;
	struct cell* result;
progn_reset:
	push_cell(R0);
	eval(R0->car, env);
	result = R0;
	R0 = pop_cell();
	if(R0->cdr == nil) return result;
	R0 = R0->cdr;
	goto progn_reset;
}

struct cell* apply(struct cell* proc, struct cell* vals)
{
	struct cell* temp;
	if(proc->type == PRIMOP)
	{
		FUNCTION* fp = proc->function;
		temp = fp(vals);
	}
	else if(proc->type == LAMBDA)
	{
		struct cell* env = make_cons(proc->env->car, proc->env->cdr);
		temp = progn(proc->cdr, multiple_extend(env, proc->car, vals));
	}
	else if(proc->type == MACRO)
	{
		temp = macro_apply(proc->cdr, vals);
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
	if(nil == exp) return cell_unspecified;

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
	if(NULL == exp)
	{
		R0 = NULL;
		return;
	}
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

	if(nil == exp->cdr->cdr->cdr) return cell_unspecified;
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
			if(unquote_splicing == i->car->car)
			{
				eval(i->car->cdr->car, env);
				while((NULL != R0) && (nil != R0))
				{
					/* Unsure if correct behavior is to revert to unquote behavior (what guile does) */
					/* Or restrict to just proper lists as the spec (r7rs) requires */
					/* eg. `(foo bar ,@(+ 4 5)) */
					require(CONS == R0->type, "unquote-splicing requires argument of type <proper list>\n");
					f = make_cons(R0->car, f);
					/* Simply convert require to if and the above */
					/* else f = make_cons(R0, f); */
					R0 = R0->cdr;
				}
				goto restart_quasiquote;
			}
		}
		f = make_cons(h, f);
restart_quasiquote:
		i = i->cdr;
	}
	i = f;
	f = reverse_list(f);
	require(NULL != i, "Impossible quasiquote processed?\n");
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
	if(exp->car == s_macro) return make_macro(exp->cdr->car, exp->cdr->cdr, env);
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


/* Exposed primitives */
struct cell* builtin_apply(struct cell* args)
{
	require(nil != args, "apply requires arguments\n");
	require(nil != args->cdr, "apply recieved insufficient arguments\n");
	require(CONS == args->cdr->car->type, "apply did not recieve a list\n");
	return apply(args->car, args->cdr->car);
}

struct cell* builtin_primitive_eval(struct cell* args)
{
	require(nil != args, "primitive-eval requires an argument\n");
	require(nil == args->cdr, "primitive-eval received too many arguments\n");

	push_cell(R0);
	push_cell(R1);
	push_cell(g_env);
	eval(args->car, primitive_env);
	struct cell* r = R0;
	g_env = pop_cell();
	R1 = pop_cell();
	R0 = pop_cell();
	return r;
}
