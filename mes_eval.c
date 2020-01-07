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
	struct cell* i = R1;
	struct cell* j = evlis(exps->cdr, env);
	return make_cons(i, j);
}

void eval(struct cell* exp, struct cell* env)
{
	struct cell* tmp;
	struct cell* proc;
	struct cell* vals;
	struct cell* pair;
	struct cell* fun;
	struct cell* arguments;
	struct cell* name;
	FUNCTION* fp;
	struct cell* i;
	struct cell* f;
	struct cell* h;

eval_start:
	if(exp == nil)
	{
		R1 = nil;
		return;
	}

	if(INT == exp->type)
	{
		R1 = exp;
		return;
	}
	else if(SYM == exp->type)
	{
		tmp = assoc(exp, env);
		require(tmp != nil, "Unbound symbol");
		R1 = tmp->cdr;
		return;
	}
	else if(CONS == exp->type)
	{
		if(exp->car == s_if)
		{
			eval(exp->cdr->car, env);
			if(R1 != cell_f)
			{
				exp = exp->cdr->cdr->car;
				goto eval_start;
			}

			if(nil == exp->cdr->cdr->cdr)
			{
				R1 = cell_unspecified;
				return;
			}

			exp = exp->cdr->cdr->cdr->car;
			goto eval_start;
		}
		else if(exp->car == s_cond)
		{
restart_cond:
			if(nil == exp)
			{
				R1 = cell_unspecified;
				return;
			}
			eval(exp->car->car, env);
			if(cell_t == R1)
			{
				exp = exp->car->cdr->car;
				goto eval_start;
			}
			exp = exp->cdr;
			goto restart_cond;
		}
		else if(exp->car == s_lambda)
		{
			R1 = make_proc(exp->cdr->car, exp->cdr->cdr, env);
			return;
		}
		else if(exp->car == quote)
		{
			R1 = exp->cdr->car;
			return;
		}
		else if(exp->car == quasiquote)
		{
			i = exp->cdr->car;
			f = NULL;
			while(nil != i)
			{
				h = i->car;
				if(CONS == i->car->type)
				{
					if(unquote == i->car->car)
					{
						eval(i->car->cdr->car, env);
						h = R1;
					}
					if(unquote_splicing == i->car->car)
					{
						eval(i->car->cdr->car, env);
						while((NULL != R1) && (nil != R1))
						{
							/* Unsure if correct behavior is to revert to unquote behavior (what guile does) */
							/* Or restrict to just proper lists as the spec (r7rs) requires */
							/* eg. `(foo bar ,@(+ 4 5)) */
							require(CONS == R1->type, "unquote-splicing requires argument of type <proper list>\n");
							f = make_cons(R1->car, f);
							/* Simply convert require to if and the above */
							/* else f = make_cons(R1, f); */
							R1 = R1->cdr;
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
			R1 = f;
			return;
		}
		else if(exp->car == s_define)
		{
			if(CONS == exp->cdr->car->type)
			{
				fun = exp->cdr->cdr;
				arguments = exp->cdr->car->cdr;
				name = exp->cdr->car->car;
				exp->cdr = make_cons(name, make_cons(make_cons(s_lambda, make_cons(arguments, fun)), nil));
			}
			eval(exp->cdr->cdr->car, env);
			R1 = extend_env(exp->cdr->car, R1, env);
			return;
		}
		else if(exp->car == s_setb)
		{
			pair = assoc(exp->cdr->car, env);
			eval(exp->cdr->cdr->car, env);
			pair->cdr = R1;
			return;
		}
		else if(exp->car == s_let)
		{
			for(tmp = exp->cdr->car; tmp != nil; tmp = tmp->cdr)
			{
				push_cell(tmp);
				eval(tmp->car->cdr->car, env);
				tmp = pop_cell();
				env = make_cons(make_cons(tmp->car->car, R1), env);
			}

			exp = make_cons(s_begin, exp->cdr->cdr);
			goto eval_start;
		}
		else if(exp->car == s_begin)
		{
			exp = exp->cdr;
			if(exp == nil)
			{
				R1 = nil;
				return;
			}
			while(TRUE)
			{
				if(exp->cdr == nil)
				{
					exp = exp->car;
					goto eval_start;
				}
				eval(exp->car, env);
				exp = exp->cdr;
			}
		}

		eval(exp->car, env);
		push_cell(R1);
		vals = evlis(exp->cdr, env);
		proc = pop_cell();

		if(proc->type == PRIMOP)
		{
			fp = proc->function;
			R1 = fp(vals);
			return;
		}
		else if(proc->type == LAMBDA)
		{
			env = make_cons(proc->env->car, proc->env->cdr);
			env = multiple_extend(env, proc->car, vals);
			exp = make_cons(s_begin, proc->cdr);
			goto eval_start;
		}
		require(FALSE, "Bad argument to apply\n");
	}
	else if(PRIMOP == exp->type)
	{
		R1 = exp;
		return;
	}
	else if(LAMBDA == exp->type)
	{
		R1 = exp;
		return;
	}

	/* Fall through case */
	R1 = exp;
}


/* Exposed primitives */
struct cell* builtin_apply(struct cell* args)
{
	require(nil != args, "apply requires arguments\n");
	require(nil != args->cdr, "apply recieved insufficient arguments\n");
	require(CONS == args->cdr->car->type, "apply did not recieve a list\n");
/*	return apply(args->car, args->cdr->car); */
	return cell_unspecified;
}

struct cell* builtin_primitive_eval(struct cell* args)
{
	require(nil != args, "primitive-eval requires an argument\n");
	require(nil == args->cdr, "primitive-eval received too many arguments\n");

	push_cell(R0);
	push_cell(R1);
	push_cell(g_env);
/*	eval(args->car, primitive_env); */
	struct cell* r = R0;
	g_env = pop_cell();
	R1 = pop_cell();
	R0 = pop_cell();
	return r;
}
