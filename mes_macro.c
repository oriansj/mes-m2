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

struct cell* extend_env(struct cell* sym, struct cell* val, struct cell* env);
void push_cell(struct cell* a);
struct cell* pop_cell();
struct cell* assoc(struct cell* key, struct cell* alist);
struct cell* multiple_extend(struct cell* env, struct cell* syms, struct cell* vals);
struct cell* make_proc(struct cell* a, struct cell* b, struct cell* env);
struct cell* make_macro(struct cell* a, struct cell* b, struct cell* env);

struct cell* define_macro(struct cell* exp, struct cell* env)
{
	if(CONS == exp->cdr->car->type)
	{
		struct cell* fun = exp->cdr->cdr;
		struct cell* arguments = exp->cdr->car->cdr;
		struct cell* name = exp->cdr->car->car;
		exp->cdr = make_cons(name, make_cons(make_cons(s_macro, make_cons(arguments, fun)), nil));
	}

	return(extend_env(exp->cdr->car, exp->cdr->cdr->car, env));
}

struct cell* macro_apply(struct cell* exps, struct cell* vals);
struct cell* macro_eval(struct cell* exps, struct cell* env);

struct cell* macro_list(struct cell* exps, struct cell* env)
{
	if(exps == nil) return nil;

	struct cell* i = macro_eval(exps->car, env);
	struct cell* j = macro_list(exps->cdr, env);
	return make_cons(i, j);
}

struct cell* expand_cons(struct cell* exp, struct cell* env)
{
//	if(exp->car == s_if) return expand_if(exp, env);
//	if(exp->car == s_cond) return expand_cond(exp->cdr, env);
//	if(exp->car == s_lambda) return expand_proc(exp->cdr->car, exp->cdr->cdr, env);
	if(exp->car == quote) return exp->cdr->car;
	if(exp->car == s_macro) return make_macro(exp->cdr->car, exp->cdr->cdr, env);
//	if(exp->car == s_define) return expand_define(exp, env);
//	if(exp->car == s_setb) return expand_setb(exp, env);

	R0 = macro_eval(exp->car, env);
	push_cell(R0);
	R1 = macro_list(exp->cdr, env);
	R0 = pop_cell();
	return macro_apply(R0, R1);
}

struct cell* macro_eval(struct cell* exps, struct cell* env)
{
	if(CONS == exps->type) return expand_cons(exps, env);
	if(SYM == exps->type)
	{
		struct cell* tmp = assoc(exps, env);
		if(nil == tmp) return exps;
		return tmp->cdr;
	}
	return exps;
}

struct cell* macro_progn(struct cell* exps, struct cell* env)
{
	if(exps == nil) return nil;

	struct cell* result;
progn_reset:
	result = macro_eval(exps->car, env);
	if(exps->cdr == nil) return result;
	exps = exps->cdr;
	goto progn_reset;
}

struct cell* macro_apply(struct cell* proc, struct cell* vals)
{
	struct cell* temp;
	if(proc->type == PRIMOP)
	{
		FUNCTION* fp = proc->function;
		temp = fp(vals);
	}
	else if(proc->type == LAMBDA)
	{
		temp = proc;
	}
	else if(proc->type == MACRO)
	{
		struct cell* env = make_cons(proc->env->car, proc->env->cdr);
		temp = macro_progn(proc->cdr, multiple_extend(env, proc->car, vals));
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
expand_reset:
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

	hold = assoc(R0->car, g_env);
	if(CONS == hold->type)
	{
		if(s_macro == hold->cdr->car)
		{
			return macro_apply(make_macro(hold->cdr->cdr->car, hold->cdr->cdr->cdr, g_env), R0->cdr);
		}
	}

	R0 = R0->cdr;
	goto expand_reset;
}
