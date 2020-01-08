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
	g_stack[stack_pointer] = a;
	stack_pointer = stack_pointer + 1;
}

struct cell* pop_cell()
{
	stack_pointer = stack_pointer - 1;
	struct cell* r = g_stack[stack_pointer];
	g_stack[stack_pointer] = NULL;

	return r;
}

/*** Evaluator (Eval/Apply) ***/
void eval(struct cell* env);
struct cell* evlis(struct cell* env)
{
	if(R0 == nil) return nil;

	push_cell(R0->cdr);
	R0 = R0->car;
	eval(env);
	R0 = pop_cell();
	struct cell* i = R1;
	struct cell* j = evlis(env);
	return make_cons(i, j);
}

void eval(struct cell* env)
{
	struct cell* tmp;
	struct cell* proc;
	struct cell* vals;
	struct cell* fun;
	struct cell* arguments;
	struct cell* name;
	FUNCTION* fp;
	struct cell* i;
	struct cell* f;
	struct cell* h;

eval_start:
	if(R0 == nil)
	{
		R1 = nil;
		return;
	}

	if(INT == R0->type)
	{
		R1 = R0;
		return;
	}
	else if(SYM == R0->type)
	{
		tmp = assoc(R0, env);
		require(tmp != nil, "Unbound symbol");
		R1 = tmp->cdr;
		return;
	}
	else if(CONS == R0->type)
	{
		if(R0->car == s_if)
		{
			push_cell(R0);
			R0 = R0->cdr->car;
			eval(env);
			R0 = pop_cell();
			if(R1 != cell_f)
			{
				R0 = R0->cdr->cdr->car;
				goto eval_start;
			}

			if(nil == R0->cdr->cdr->cdr)
			{
				R1 = cell_unspecified;
				return;
			}

			R0 = R0->cdr->cdr->cdr->car;
			goto eval_start;
		}
		else if(R0->car == s_cond)
		{
			R0 = R0->cdr;
			while(nil != R0)
			{
				push_cell(R0);
				R0 = R0->car->car;
				eval(env);
				R0 = pop_cell();
				if(cell_t == R1)
				{
					R0 = R0->car->cdr->car;
					eval(env);
					return;
				}
				R0 = R0->cdr;
			}
			R1 = cell_unspecified;
			return;
		}
		else if(R0->car == s_lambda)
		{
			R1 = make_proc(R0->cdr->car, R0->cdr->cdr, env);
			return;
		}
		else if(R0->car == quote)
		{
			R1 = R0->cdr->car;
			return;
		}
		else if(R0->car == quasiquote)
		{
			i = R0->cdr->car;
			f = NULL;
			while(nil != i)
			{
				h = i->car;
				if(CONS == i->car->type)
				{
					if(unquote == i->car->car)
					{
						R0 = i->car->cdr->car;
						eval(env);
						h = R1;
					}
					if(unquote_splicing == i->car->car)
					{
						R0 = i->car->cdr->car;
						eval(env);
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
		else if(R0->car == s_define)
		{
			if(CONS == R0->cdr->car->type)
			{
				fun = R0->cdr->cdr;
				arguments = R0->cdr->car->cdr;
				name = R0->cdr->car->car;
				R0->cdr = make_cons(name, make_cons(make_cons(s_lambda, make_cons(arguments, fun)), nil));
			}
			push_cell(R0->cdr->car);
			R0 = R0->cdr->cdr->car;
			eval(env);
			R0 = pop_cell();
			R1 = extend_env(R0, R1, env);
			return;
		}
		else if(R0->car == s_setb)
		{
			R2 = assoc(R0->cdr->car, env);
			if(nil == R2)
			{
				file_print("Assigning value to unbound variable: ", stderr);
				file_print(R0->cdr->car->string, stderr);
				file_print("\nAborting to prevent problems\n", stderr);
				exit(EXIT_FAILURE);
			}
			R0 = R0->cdr->cdr->car;
			eval(env);
			R2->cdr = R1;
			return;
		}
		else if(R0->car == s_let)
		{
			push_cell(R0);
			for(tmp = R0->cdr->car; tmp != nil; tmp = tmp->cdr)
			{
				push_cell(tmp);
				R0 = tmp->car->cdr->car;
				eval(env);
				tmp = pop_cell();
				env = make_cons(make_cons(tmp->car->car, R1), env);
			}

			R0 = pop_cell();
			R0 = make_cons(s_begin, R0->cdr->cdr);
			goto eval_start;
		}
		else if(R0->car == s_begin)
		{
			R0 = R0->cdr;
			while(R0 != nil)
			{
				push_cell(R0->cdr);
				R0 = R0->car;
				eval(env);
				R0 = pop_cell();
			}
			return;
		}

		push_cell(R0->cdr);
		R0 = R0->car;
		eval(env);
		R0 = pop_cell();
		push_cell(R1);
		push_cell(R0);
		vals = evlis(env);
		R0 = pop_cell();
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
			R0 = make_cons(s_begin, proc->cdr);
			goto eval_start;
		}
		require(FALSE, "Bad argument to apply\n");
	}
	else if(PRIMOP == R0->type)
	{
		R1 = R0;
		return;
	}
	else if(LAMBDA == R0->type)
	{
		R1 = R0;
		return;
	}

	/* Fall through case */
	R1 = R0;
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
