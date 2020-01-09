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
		return extend(env, syms->cdr->car, vals);
	}

	return multiple_extend(extend(env, syms->car, vals->car), syms->cdr, vals->cdr);
}

struct cell* extend_env(struct cell* sym, struct cell* val, struct cell* env)
{
	struct cell* r = make_cons(make_cons(sym, val), env);
	return r;
}

struct cell* assoc(struct cell* key, struct cell* alist)
{
	if(nil == alist) return nil;
	struct cell* i;
	for(i = alist; nil != i; i = i->cdr)
	{
		if(i->car->car->string == key->string) return i->car;
	}

	if(SYM != key->type) return nil;

	/* Last ditch effort (REALLY SLOW) */
	for(i = alist; nil != i; i = i->cdr)
	{
		if(match(i->car->car->string, key->string)) return i->car;
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
void eval();
void evlis()
{
	if(R0 == nil)
	{
		R1 = nil;
		return;
	}

	push_cell(g_env);
	push_cell(R0->cdr);
	R0 = R0->car;
	eval();
	R0 = pop_cell();
	struct cell* i = R1;
	g_env = pop_cell();
	evlis();
	struct cell* j = R1;
	R1 = make_cons(i, j);
}

void apply(struct cell* proc, struct cell* vals)
{
	FUNCTION* fp;
	if(proc->type == PRIMOP)
	{
		fp = proc->function;
		R1 = fp(vals);
		return;
	}
	else if(proc->type == LAMBDA)
	{
		push_cell(g_env);
		g_env = multiple_extend(proc->env, proc->car, vals);
		proc->env = g_env;
		R0 = make_cons(s_begin, proc->cdr);
		eval();
		g_env = pop_cell();
		return;
	}
	file_print("Bad argument to apply: ", stderr);
	file_print(proc->string, stderr);
	file_print("\nAborting to avoid problems\n", stderr);
	exit(EXIT_FAILURE);
}

void eval()
{
	struct cell* tmp;
	struct cell* fun;
	struct cell* arguments;
	struct cell* name;
	struct cell* i;
	struct cell* f;
	struct cell* h;

	if(SYM == R0->type)
	{
		R1 = assoc(R0, g_env);
		if(R1 == nil)
		{
			file_print("Unbound symbol: ", stderr);
			file_print(R0->string, stderr);
			file_print("\nAborting before problems can occur\n", stderr);
			exit(EXIT_FAILURE);
		}
		R1 = R1->cdr;
		return;
	}
	else if(CONS == R0->type)
	{
		if(R0->car == s_if)
		{
			push_cell(R0);
			R0 = R0->cdr->car;
			eval();
			R0 = pop_cell();
			if(R1 != cell_f)
			{
				R0 = R0->cdr->cdr->car;
				eval();
				return;
			}

			if(nil == R0->cdr->cdr->cdr)
			{
				R1 = cell_unspecified;
				return;
			}

			R0 = R0->cdr->cdr->cdr->car;
			eval();
			return;
		}
		else if(R0->car == s_cond)
		{
			R0 = R0->cdr;
			while(nil != R0)
			{
				push_cell(R0);
				R1 = cell_unspecified;
				R0 = R0->car->car;
				eval();
				R0 = pop_cell();
				if(cell_t == R1)
				{
					R0 = R0->car->cdr->car;
					eval();
					return;
				}
				R0 = R0->cdr;
			}
			return;
		}
		else if(R0->car == s_lambda)
		{
			R1 = make_proc(R0->cdr->car, R0->cdr->cdr, make_cons(g_env->car, g_env->cdr));
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
						eval();
						h = R1;
					}
					if(unquote_splicing == i->car->car)
					{
						R0 = i->car->cdr->car;
						eval();
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
			eval();
			R0 = pop_cell();
			if(LAMBDA == R1->type)
			{
				R1->env = make_cons(make_cons(R0, R1), R1->env);
			}
			g_env = extend_env(R0, R1, g_env);
			R1 = cell_unspecified;
			return;
		}
		else if(R0->car == s_setb)
		{
			R2 = assoc(R0->cdr->car, g_env);
			if(nil == R2)
			{
				file_print("Assigning value to unbound variable: ", stderr);
				file_print(R0->cdr->car->string, stderr);
				file_print("\nAborting to prevent problems\n", stderr);
				exit(EXIT_FAILURE);
			}
			R0 = R0->cdr->cdr->car;
			eval();
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
				eval();
				tmp = pop_cell();
				g_env = make_cons(make_cons(tmp->car->car, R1), g_env);
			}

			R0 = pop_cell();
			R0 = make_cons(s_begin, R0->cdr->cdr);
			eval();
			return;
		}
		else if(R0->car == s_begin)
		{
			R0 = R0->cdr;
			while(R0 != nil)
			{
				push_cell(R0->cdr);
				R0 = R0->car;
				eval();
				R0 = pop_cell();
			}
			return;
		}

		push_cell(R0->cdr);
		R0 = R0->car;
		eval();
		R0 = pop_cell();
		push_cell(R1);
		evlis();
		R0 = pop_cell();
		apply(R0, R1);
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
	push_cell(R0);
	push_cell(R1);
	push_cell(g_env);
	R0 = args;
	struct cell* r;
	apply(args->car, args->cdr->car);
	r = R1;
	g_env = pop_cell();
	R1 = pop_cell();
	R0 = pop_cell();
	return r;
}

struct cell* builtin_primitive_eval(struct cell* args)
{
	require(nil != args, "primitive-eval requires an argument\n");
	require(nil == args->cdr, "primitive-eval received too many arguments\n");

	push_cell(R0);
	push_cell(R1);
	push_cell(g_env);
	R0 = args->car;
	g_env = primitive_env;
	eval();
	struct cell* r = R1;
	g_env = pop_cell();
	R1 = pop_cell();
	R0 = pop_cell();
	return r;
}
