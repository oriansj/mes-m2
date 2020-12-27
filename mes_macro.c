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
struct cell* macro_progn(struct cell* exps, struct cell* env);
struct cell* make_macro(struct cell* a, struct cell* b, struct cell* env);
struct cell* make_proc(struct cell* a, struct cell* b, struct cell* env);
struct cell* pop_cell();
struct cell* reverse_list(struct cell* head);
void push_cell(struct cell* a);
struct cell* cell_invoke_function(struct cell* cell, struct cell* vals);
void apply(struct cell* proc, struct cell* vals);

struct cell* macro_extend_env(struct cell* sym, struct cell* val, struct cell* env)
{
	env->cdr = make_cons(env->car, env->cdr);
	env->car = make_cons(sym, val);
	return NULL;
}

struct cell* define_macro(struct cell* exp, struct cell* env)
{
	if(CONS == exp->cdr->car->type)
	{
		struct cell* fun = exp->cdr->cdr;
		struct cell* arguments = exp->cdr->car->cdr;
		struct cell* name = exp->cdr->car->car;
		exp->cdr = make_cons(name, make_cons(make_cons(s_macro, make_cons(arguments, fun)), nil));
	}

	return(macro_extend_env(exp->cdr->car, exp->cdr->cdr->car, env));
}

struct cell* macro_apply(struct cell* exps, struct cell* vals);
struct cell* macro_eval(struct cell* exps, struct cell* env);
struct cell* expand_quasiquote()
{
	/* Protect the s-expression during the entire evaluation */
	push_cell(R0);
	/* R2 is the s-expression we are quasiquoting */
	push_cell(R2);
	/* R3 is the resulting s-expression, built backwards and reversed at the end */
	push_cell(R3);
	/* R4 is just a temp holder of each unquote */
	push_cell(R4);

	/* (quasiquote (...)) */
	R2 = R0->cdr->car;
	R3 = NULL;
	while(nil != R2)
	{
		require(NULL != R2, "Null in quasiquote expression reached\n");
		require(CONS == R2->type, "Not a cons list in quasiquote reached\n");
		R4 = R2->car;
		if(CONS == R2->car->type)
		{
			if(unquote == R2->car->car)
			{
				R0 = R2->car->cdr->car;
				R4 = NULL; /* So that assoc doesn't mistake this for a lambda */
				push_cell(R3);
				push_cell(R2);
				macro_eval(R0, R1);
				R2 = pop_cell();
				R3 = pop_cell();
				R4 = R1;
			}
			if(unquote_splicing == R2->car->car)
			{
				R0 = R2->car->cdr->car;
				push_cell(R4);
				push_cell(R3);
				push_cell(R2);
				R4 = NULL; /* So that assoc doesn't mistake this for a lambda */
				macro_eval(R0, R1);
				R2 = pop_cell();
				R3 = pop_cell();
				R4 = pop_cell();
				while((NULL != R1) && (nil != R1))
				{
					/* Unsure if correct behavior is to revert to unquote behavior (what guile does) */
					/* Or restrict to just proper lists as the spec (r7rs) requires */
					/* eg. `(foo bar ,@(+ 4 5)) */
					require(CONS == R1->type, "unquote-splicing requires argument of type <proper list>\n");
					R3 = make_cons(R1->car, R3);
					/* Simply convert require to if and the above */
					/* else R3 = make_cons(R1, R3); */
					R1 = R1->cdr;
				}

				/* we really don't want to add that cons after what we just did */
				goto macro_restart_quasiquote;
			}
		}
		R3 = make_cons(R4, R3);
macro_restart_quasiquote:
		/* keep walking down the list of s-expressions */
		R2 = R2->cdr;
	}

	/* We created the list backwards because it was simpler, now we have to put it into correct order */
	R2 = R3;
	R3 = reverse_list(R3);
	require(NULL != R2, "Impossible quasiquote processed?\n");
	R2->cdr = nil;
	R1 = R3;

	/* We are finally done with the s-expression, we don't need it back */
	R4 = pop_cell();
	R3 = pop_cell();
	R2 = pop_cell();
	pop_cell();
	return R0;
}

struct cell* macro_list(struct cell* exps, struct cell* env)
{
	if(exps == nil) return nil;

	struct cell* i = macro_eval(exps->car, env);
	struct cell* j = macro_list(exps->cdr, env);
	return make_cons(i, j);
}

struct cell* expand_if(struct cell* exp, struct cell* env)
{
	R0 = macro_eval(exp->cdr->car, env);
	if(R0 != cell_f)
	{
		R0 = macro_eval(exp->cdr->cdr->car, env);
		return R0;
	}

	if(nil == exp->cdr->cdr->cdr) return cell_unspecified;
	R0 = macro_eval(exp->cdr->cdr->cdr->car, env);
	return R0;
}

struct cell* expand_cond()
{
	/* Get past the COND */
	R0 = R0->cdr;

	/* Provide a way to flag no fields in cond */
	R1 = NULL;

	/* Loop until end of list of s-expressions */
	while(nil != R0)
	{
		/* Protect remaining list of s-expressions from garbage collection */
		push_cell(R0);

		/* Evaluate the conditional */
		R0 = R0->car->car;
		macro_eval(R0, R1);
		R0 = pop_cell();

		/* Execute if not false because that is what guile does (believe everything not #f is true) */
		if(cell_f != R1)
		{
			R0 = make_cons(s_begin, R0->car->cdr);
			macro_eval(R0, R1);
			return R0;
		}

		/* Iterate to the next in the list of s-expressions */
		R0 = R0->cdr;

		/* The default return in guile if it hits nil */
		R1 = cell_unspecified;
	}

	require(NULL != R1, "a naked cond is not supported\n");
	return R0;
}

struct cell* expand_let()
{
	/* Clean up locals after let completes */
	push_cell(g_env);

	/* Protect the s-expression from garbage collection */
	push_cell(R0->cdr->cdr);

	/* Deal with the (let ((pieces)) ..) */
	for(R0 = R0->cdr->car; R0 != nil; R0 = R0->cdr)
	{
		push_cell(R0);
		R0 = R0->car->cdr->car;
		macro_eval(R0, R1);
		R0 = pop_cell();
		if(NULL != R4) R4 = make_cons(make_cons(R0->car->car, R1), R4);
		else g_env = make_cons(make_cons(R0->car->car, R1), g_env);
	}

	/* Lets execute the pieces of the of (let ((..)) pieces) */
	R0 = pop_cell();
	R0 = make_cons(s_begin, R0);
	macro_eval(R0, R1);

	/* Actual clean up */
	g_env = pop_cell();
	return R0;
}

struct cell* expand_define()
{
	require(nil != R0->cdr, "naked (define) not supported\n");
	/* To support (define (foo a b .. N) (s-expression)) form */
	if(CONS == R0->cdr->car->type)
	{
		/* R2 is to get the actual function*/
		push_cell(R2);
		/* R3 is to get the function arguments */
		push_cell(R3);
		/* R4 is to get the function's name */
		push_cell(R4);
		R2 = R0->cdr->cdr;
		R3 = R0->cdr->car->cdr;
		R4 = R0->cdr->car->car;
		/* by converting it into (define foo (lambda (a b .. N) (s-expression))) form */
		R0->cdr = make_cons(R4, make_cons(make_cons(s_lambda, make_cons(R3, R2)), nil));
		R4 = pop_cell();
		R3 = pop_cell();
		R2 = pop_cell();
	}

	/* Protect the name from garbage collection */
	push_cell(R0->cdr->car);

	/* Evaluate the s-expression which the name is supposed to equal */
	require(nil != R0->cdr->cdr, "naked (define foo) not supported\n");
	R0 = R0->cdr->cdr->car;
	push_cell(R4);
	push_cell(R3);
	push_cell(R2);
	macro_eval(R0, R1);
	R2 = pop_cell();
	R3 = pop_cell();
	R4 = pop_cell();
	R0 = pop_cell();

	/* If we define a LAMBDA/MACRO, we need to extend its environment otherwise it can not call itself recursively */
	if((LAMBDA == R1->type) || (MACRO == R1->type))
	{
		R1->env = make_cons(make_cons(R0, R1), R1->env);
	}

	/* We now need to extend the environment with our new name */
	g_env = make_cons(make_cons(R0, R1), g_env);
	R1 = cell_unspecified;
	return R0;
}

struct cell* expand_cons(struct cell* exp, struct cell* env)
{
	if(exp->car == s_if) return expand_if(exp, env);
	if(exp->car == s_cond) return expand_cond(exp->cdr, env);
	if(exp->car == s_lambda) return make_proc(exp->cdr->car, exp->cdr->cdr, env);
	if(exp->car == quote) return exp->cdr->car;
	if(exp->car == s_macro) return make_macro(exp->cdr->car, exp->cdr->cdr, env);
	if(exp->car == s_define) return expand_define(exp, env);
	if(exp->car == s_let) return expand_let(exp, env);
	if(exp->car == quasiquote) return expand_quasiquote(exp->cdr->car, env);

	R0 = macro_eval(exp->car, env);
	push_cell(R0);
	R1 = macro_list(exp->cdr, env);
	R0 = pop_cell();
	return macro_apply(R0, R1);
}

struct cell* macro_assoc(struct cell* key, struct cell* alist)
{
	if(nil == alist) return nil;
	struct cell* i;
	for(i = alist; nil != i; i = i->cdr)
	{
		if(i->car->car->string == key->string) return i->car;
	}
	return nil;
}


struct cell* macro_eval(struct cell* exps, struct cell* env)
{
	if(CONS == exps->type) return expand_cons(exps, env);
	if(SYM == exps->type)
	{
		struct cell* tmp = macro_assoc(exps, env);
		if(nil == tmp) return exps;
		return tmp->cdr;
	}
	return exps;
}

struct cell* macro_progn(struct cell* exps, struct cell* env)
{
	if(exps == nil) return nil;

	struct cell* result;
macro_progn_reset:
	result = macro_eval(exps->car, env);
	if(exps->cdr == nil) return result;
	exps = exps->cdr;
	goto macro_progn_reset;
}

struct cell* macro_extend(struct cell* env, struct cell* syms, struct cell* vals)
{
	if(nil == syms)
	{
		return env;
	}

	if(cell_dot == syms->car)
	{
		return make_cons(make_cons(syms->cdr->car, vals), env);
	}

	return macro_extend(make_cons(make_cons(syms->car, vals->car), env), syms->cdr, vals->cdr);
}

struct cell* macro_apply(struct cell* proc, struct cell* vals)
{
	struct cell* temp;
	if(proc->type == PRIMOP)
	{
		temp = cell_invoke_function(proc, vals);
	}
	else if(proc->type == LAMBDA)
	{
		push_cell(R0);
		push_cell(R1);
		apply(proc, vals);
		temp = R1;
		R1 = pop_cell();
		R0 = pop_cell();
	}
	else if(proc->type == MACRO)
	{
		struct cell* env = make_cons(proc->env->car, proc->env->cdr);
		temp = macro_progn(proc->cdr, macro_extend(env, proc->car, vals));
	}
	else
	{
		temp = macro_eval(proc, g_env);
	}
	return temp;
}

struct cell* expand_macros(struct cell* exp)
{
	R0 = exp;
	struct cell* hold;

 	if(NULL == R0) return exp;
	if(CONS != R0->type) return exp;
	else if(R0->car == s_define_macro)
	{
		define_macro(R0, g_env);
		return cell_unspecified;
	}
	hold = R0;
	hold->car = expand_macros(R0->car);
	R0 = hold;

	hold = macro_assoc(R0->car, g_env);
	if(CONS == hold->type)
	{
		if(s_macro == hold->cdr->car)
		{
			R0 = macro_apply(make_macro(hold->cdr->cdr->car, hold->cdr->cdr->cdr, g_env), R0->cdr);
			return expand_macros(R0);
		}
	}

	hold = R0;
	hold->cdr = expand_macros(R0->cdr);
	R0 = hold;
	return R0;
}
