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
#include "mes_constants.h"

extern SCM STACK_SIZE;

SCM append2(SCM x, SCM y);
SCM gc_push_frame();
SCM assert_defined(SCM x, SCM e);
SCM get_macro(SCM name);
SCM cdr(SCM x);
SCM set_cdr_x(SCM x, SCM e);
SCM expand_variable(SCM x, SCM formals);
SCM macro_get_handle(SCM name);
SCM assq(SCM x, SCM a);
SCM macro_set_x(SCM name, SCM value);
SCM make_closure_(SCM args, SCM body, SCM a);
SCM set_env_x(SCM x, SCM e, SCM a);
SCM gc_pop_frame();
SCM check_apply(SCM f, SCM e);
SCM pairlis(SCM x, SCM y, SCM a);
SCM call_lambda(SCM e, SCM x);
struct scm* make_string(char const* s, int length);
SCM cons_(SCM x, SCM y);
SCM check_formals(SCM f, SCM formals, SCM args);
char *itoa (int number);
SCM error(SCM key, SCM x);
struct scm* mes_builtins(struct scm* a);
struct scm* apply_builtin(SCM fn, SCM x);
struct scm* cstring_to_symbol(char const *s);
struct scm* make_hash_table_(SCM size);
struct scm* gc_check ();
SCM gc ();
SCM push_cc(SCM p1, SCM p2, SCM a, SCM c);
struct scm* hashq_get_handle (SCM table, SCM key, SCM dflt);
struct scm* hash_set_x (SCM table, SCM key, SCM value);
struct scm* display_ (SCM x);
struct scm* write_error_ (SCM x);
SCM equal2_p (SCM a, SCM b);
SCM reverse_x_ (SCM x, SCM t);
struct scm* builtin_arity (SCM builtin);
SCM builtin_p (SCM x);
struct scm* module_printer (SCM module);
struct scm* module_variable (SCM module, SCM name);
struct scm* module_ref (SCM module, SCM name);
struct scm* module_define_x (SCM module, SCM name, SCM value);
SCM open_input_file (SCM file_name);
SCM set_current_input_port (SCM port);
SCM read_input_file_env ();
SCM init_time(SCM a);
SCM make_cell__(SCM type, SCM car, SCM cdr);
struct scm* vector_ref_(SCM x, SCM i);
struct scm* make_vector__(SCM k);
struct scm* vector_set_x_(SCM x, SCM i, SCM e);

SCM eval_apply()
{
	struct scm* AA;
	struct scm* ARGS;
	struct scm* BODY;
	struct scm* CL;
	struct scm* ENTRY;
	struct scm* EXPANDERS;
	struct scm* FORMALS;
	struct scm* INPUT;
	struct scm* NAME;
	struct scm* MACRO;
	struct scm* P;
	struct scm* PROGRAM;
	struct scm* SC_EXPAND;
	struct scm* V;
	struct scm* X;
	int global_p;
	int macro_p;
	int t;
	struct scm* C;
eval_apply:

	if(GetSCM2(bad2good(R3)) == cell_vm_evlis2)
	{
		goto evlis2;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_evlis3)
	{
		goto evlis3;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_eval_check_func)
	{
		goto eval_check_func;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_eval2)
	{
		goto eval2;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_apply2)
	{
		goto apply2;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_if_expr)
	{
		goto if_expr;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_begin_eval)
	{
		goto begin_eval;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_eval_set_x)
	{
		goto eval_set_x;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_macro_expand_car)
	{
		goto macro_expand_car;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_return)
	{
		goto vm_return;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_macro_expand_cdr)
	{
		goto macro_expand_cdr;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_eval_define)
	{
		goto eval_define;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_macro_expand)
	{
		goto macro_expand;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_macro_expand_lambda)
	{
		goto macro_expand_lambda;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_eval_pmatch_car)
	{
		goto eval_pmatch_car;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_begin_expand_macro)
	{
		goto begin_expand_macro;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_macro_expand_define)
	{
		goto macro_expand_define;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_begin_expand_eval)
	{
		goto begin_expand_eval;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_call_with_current_continuation2)
	{
		goto call_with_current_continuation2;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_macro_expand_set_x)
	{
		goto macro_expand_set_x;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_eval_pmatch_cdr)
	{
		goto eval_pmatch_cdr;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_macro_expand_define_macro)
	{
		goto macro_expand_define_macro;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_begin_primitive_load)
	{
		goto begin_primitive_load;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_evlis)
	{
		goto evlis;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_apply)
	{
		goto apply;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_eval)
	{
		goto eval;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_eval_macro_expand_eval)
	{
		goto eval_macro_expand_eval;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_eval_macro_expand_expand)
	{
		goto eval_macro_expand_expand;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_begin)
	{
		goto begin;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_begin_expand)
	{
		goto begin_expand;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_begin_expand_primitive_load)
	{
		goto begin_expand_primitive_load;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_if)
	{
		goto vm_if;
	}
	else if(GetSCM2(bad2good(R3)) == cell_vm_call_with_values2)
	{
		goto call_with_values2;
	}
	else if(GetSCM2(bad2good(R3)) == cell_unspecified)
	{
		return GetSCM2(bad2good(R1));
	}
	else
	{
		error(cell_symbol_system_error, GetSCM2(bad2good(make_string ("eval/apply unknown continuation", strlen("eval/apply unknown continuation")))));
	}

evlis:

	if(GetSCM2(bad2good(R1)) == cell_nil)
	{
		goto vm_return;
	}

	if(bad2good(R1)->type != TPAIR)
	{
		goto eval;
	}

	push_cc(bad2good(R1)->rac, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_evlis2);
	goto eval;
evlis2:
	push_cc(bad2good(R2)->rdc, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_evlis3);
	goto evlis;
evlis3:
	R1 = good2bad(Getstructscm2(cons_(GetSCM2(bad2good(R2)), GetSCM2(bad2good(R1)))));
	goto vm_return;
apply:
	g_stack_array[g_stack + FRAME_PROCEDURE] = bad2good(R1)->car;
	t = bad2good(bad2good(R1)->car)->type;

	if(t == TSTRUCT && builtin_p(bad2good(R1)->rac) == cell_t)
	{
		check_formals(bad2good(R1)->rac, GetSCM2(bad2good(builtin_arity(bad2good(R1)->rac))), bad2good(R1)->rdc);
		R1 = good2bad(apply_builtin(bad2good(R1)->rac, bad2good(R1)->rdc));    /// FIXME: move into eval_apply
		goto vm_return;
	}
	else if(t == TCLOSURE)
	{
		CL = bad2good(bad2good(R1)->car)->cdr;
		BODY = bad2good(bad2good(CL)->cdr)->cdr;
		FORMALS = bad2good(bad2good(CL)->cdr)->car;
		ARGS = bad2good(R1)->cdr;
		AA = bad2good(bad2good(CL)->car)->cdr;
		AA = bad2good(AA)->cdr;
		check_formals(bad2good(R1)->rac, GetSCM2(bad2good(FORMALS)), bad2good(R1)->rdc);
		P = good2bad(Getstructscm2(pairlis(GetSCM2(bad2good(FORMALS)), GetSCM2(bad2good(ARGS)), GetSCM2(bad2good(AA)))));
		call_lambda(GetSCM2(bad2good(BODY)), GetSCM2(bad2good(P)));
		goto begin;
	}
	else if(t == TCONTINUATION)
	{
		V = bad2good(bad2good(R1)->car)->cdr;

		if(bad2good(V)->length)
		{
			for(t = 0; t < bad2good(V)->length; t++)
			{
				g_stack_array[STACK_SIZE - bad2good(V)->length + t] = good2bad(vector_ref_(GetSCM2(bad2good(V)), t));
			}

			g_stack = STACK_SIZE - bad2good(V)->length;
		}

		X = R1;
		gc_pop_frame();
		R1 = bad2good(bad2good(X)->cdr)->car;
		goto eval_apply;
	}
	else if(t == TSPECIAL)
	{
		C = bad2good(R1)->car;

		if(GetSCM2(bad2good(C)) == cell_vm_apply)
		{
			push_cc(cons_(bad2good(bad2good(R1)->cdr)->rac, bad2good(bad2good(bad2good(R1)->cdr)->cdr)->rac), GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_return);
			goto apply;
		}
		else if(GetSCM2(bad2good(C)) ==  cell_vm_eval)
		{
			push_cc(bad2good(bad2good(R1)->cdr)->rac, GetSCM2(bad2good(R1)), bad2good(bad2good(bad2good(R1)->cdr)->cdr)->rac, cell_vm_return);
			goto eval;
		}
		else if(GetSCM2(bad2good(C)) ==  cell_vm_begin_expand)
		{
			push_cc(cons_(bad2good(bad2good(R1)->cdr)->rac, cell_nil), GetSCM2(bad2good(R1)), bad2good(bad2good(bad2good(R1)->cdr)->cdr)->rac, cell_vm_return);
			goto begin_expand;
		}
		else if(GetSCM2(bad2good(C)) ==  cell_call_with_current_continuation)
		{
			R1 = bad2good(R1)->cdr;
			goto call_with_current_continuation;
		}
		else
		{
			check_apply(cell_f, bad2good(R1)->rac);
		}
	}
	else if(t == TSYMBOL)
	{
		if(bad2good(R1)->rac == cell_symbol_call_with_values)
		{
			R1 = bad2good(R1)->cdr;
			goto call_with_values;
		}

		if(bad2good(R1)->rac == cell_symbol_current_module)
		{
			R1 = R0;
			goto vm_return;
		}

		if(bad2good(R1)->rac == cell_symbol_boot_module)
		{
			R1 = M0;
			goto vm_return;
		}
	}
	else if(t == TPAIR)
	{
		if(bad2good(bad2good(R1)->car)->rac == cell_symbol_lambda)
		{
			FORMALS = bad2good(bad2good(bad2good(R1)->car)->cdr)->car;
			ARGS = bad2good(R1)->cdr;
			BODY = bad2good(bad2good(bad2good(R1)->car)->cdr)->cdr;
			P = good2bad(Getstructscm2(pairlis(GetSCM2(bad2good(FORMALS)), bad2good(R1)->rdc, GetSCM2(bad2good(R0)))));
			check_formals(GetSCM2(bad2good(R1)), GetSCM2(bad2good(FORMALS)), GetSCM2(bad2good(ARGS)));
			call_lambda(GetSCM2(bad2good(BODY)), GetSCM2(bad2good(P)));
			goto begin;
		}
	}

	// write_error_ (R1->car);
	// eputs ("\n");
	push_cc(bad2good(R1)->rac, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_apply2);
	goto eval;
apply2:
	check_apply(GetSCM2(bad2good(R1)), bad2good(R2)->rac);
	R1 = good2bad(Getstructscm2(cons_(GetSCM2(bad2good(R1)), bad2good(R2)->rdc)));
	goto apply;
eval:
	t = bad2good(R1)->type;

	if(t == TPAIR)
	{
		C = bad2good(R1)->car;

		if(GetSCM2(bad2good(C)) ==  cell_symbol_pmatch_car)
		{
			push_cc(bad2good(bad2good(R1)->cdr)->rac, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_eval_pmatch_car);
			goto eval;
eval_pmatch_car:
			X = R1;
			gc_pop_frame();
			R1 = bad2good(X)->car;
			goto eval_apply;
		}
		else if(GetSCM2(bad2good(C)) ==  cell_symbol_pmatch_cdr)
		{
			push_cc(bad2good(bad2good(R1)->cdr)->rac, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_eval_pmatch_cdr);
			goto eval;
eval_pmatch_cdr:
			X = R1;
			gc_pop_frame();
			R1 = bad2good(X)->cdr;
			goto eval_apply;
		}
		else if(GetSCM2(bad2good(C)) ==  cell_symbol_quote)
		{
			X = R1;
			gc_pop_frame();
			R1 = bad2good(bad2good(X)->cdr)->car;
			goto eval_apply;
		}
		else if(GetSCM2(bad2good(C)) ==  cell_symbol_begin)
		{
			goto begin;
		}
		else if(GetSCM2(bad2good(C)) ==  cell_symbol_lambda)
		{
			R1 = good2bad(Getstructscm2(make_closure_(bad2good(bad2good(R1)->cdr)->rac, bad2good(bad2good(R1)->cdr)->rdc, GetSCM2(bad2good(R0)))));
			goto vm_return;
		}
		else if(GetSCM2(bad2good(C)) ==  cell_symbol_if)
		{
			R1 = bad2good(R1)->cdr;
			goto vm_if;
		}
		else if(GetSCM2(bad2good(C)) ==  cell_symbol_set_x)
		{
			push_cc(bad2good(bad2good(bad2good(R1)->cdr)->cdr)->rac, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_eval_set_x);
			goto eval;
eval_set_x:
			R1 = good2bad(Getstructscm2(set_env_x(bad2good(bad2good(R2)->cdr)->rac, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)))));
			goto vm_return;
		}
		else if(GetSCM2(bad2good(C)) == cell_vm_macro_expand)
		{
			push_cc(bad2good(bad2good(R1)->cdr)->rac, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_eval_macro_expand_eval);
			goto eval;
eval_macro_expand_eval:
			push_cc(GetSCM2(bad2good(R1)), GetSCM2(bad2good(R2)), GetSCM2(bad2good(R0)), cell_vm_eval_macro_expand_expand);
			goto macro_expand;
eval_macro_expand_expand:
			goto vm_return;
		}
		else
		{
			if(bad2good(R1)->type == TPAIR && (bad2good(R1)->rac == cell_symbol_define || bad2good(R1)->rac == cell_symbol_define_macro))
			{
				global_p = bad2good(bad2good(R0)->car)->rac != cell_closure;
				macro_p = bad2good(R1)->rac == cell_symbol_define_macro;

				if(global_p)
				{
					NAME = bad2good(bad2good(R1)->cdr)->car;

					if(bad2good(bad2good(bad2good(R1)->cdr)->car)->type == TPAIR)
					{
						NAME = bad2good(NAME)->car;
					}

					if(macro_p)
					{
						ENTRY = good2bad(Getstructscm2(assq(GetSCM2(bad2good(NAME)), g_macros)));

						if(GetSCM2(bad2good(ENTRY)) == cell_f)
						{
							macro_set_x(GetSCM2(bad2good(NAME)), cell_f);
						}
					}
					else
					{
						ENTRY = good2bad(module_variable(GetSCM2(bad2good(R0)), GetSCM2(bad2good(NAME))));

						if(GetSCM2(bad2good(ENTRY)) == cell_f)
						{
							module_define_x(GetSCM2(bad2good(M0)), GetSCM2(bad2good(NAME)), cell_f);
						}
					}
				}

				R2 = R1;

				if(bad2good(bad2good(bad2good(R1)->cdr)->car)->type != TPAIR)
				{
					push_cc(bad2good(bad2good(bad2good(R1)->cdr)->cdr)->rac, GetSCM2(bad2good(R2)), cons_(cons_(bad2good(bad2good(R1)->cdr)->rac, bad2good(bad2good(R1)->cdr)->rac), GetSCM2(bad2good(R0))), cell_vm_eval_define);
					goto eval;
				}
				else
				{
					P = good2bad(Getstructscm2(pairlis(bad2good(bad2good(R1)->cdr)->rac, bad2good(bad2good(R1)->cdr)->rac, GetSCM2(bad2good(R0)))));
					FORMALS = bad2good(bad2good(bad2good(R1)->cdr)->car)->cdr;
					BODY = bad2good(bad2good(R1)->cdr)->cdr;

					if(macro_p || global_p)
					{
						expand_variable(GetSCM2(bad2good(BODY)), GetSCM2(bad2good(FORMALS)));
					}

					R1 = good2bad(Getstructscm2(cons_(cell_symbol_lambda, cons_(GetSCM2(bad2good(FORMALS)), GetSCM2(bad2good(BODY))))));
					push_cc(GetSCM2(bad2good(R1)), GetSCM2(bad2good(R2)), GetSCM2(bad2good(P)), cell_vm_eval_define);
					goto eval;
				}

eval_define:
				NAME = bad2good(bad2good(R2)->cdr)->car;

				if(bad2good(bad2good(bad2good(R2)->cdr)->car)->type == TPAIR)
				{
					NAME = bad2good(NAME)->car;
				}

				if(macro_p)
				{
					ENTRY = good2bad(Getstructscm2(macro_get_handle(GetSCM2(bad2good(NAME)))));
					R1 = good2bad(Getstructscm2(make_cell__ (TMACRO, GetSCM2(bad2good(R1)), bad2good(NAME)->rdc)));
					set_cdr_x(GetSCM2(bad2good(ENTRY)), GetSCM2(bad2good(R1)));
				}
				else if(global_p)
				{
					ENTRY = good2bad(module_variable(GetSCM2(bad2good(R0)), GetSCM2(bad2good(NAME))));
					set_cdr_x(GetSCM2(bad2good(ENTRY)), GetSCM2(bad2good(R1)));
				}
				else
				{
					ENTRY = good2bad(Getstructscm2(cons_(GetSCM2(bad2good(NAME)), GetSCM2(bad2good(R1)))));
					AA = good2bad(Getstructscm2(cons_(GetSCM2(bad2good(ENTRY)), cell_nil)));
					set_cdr_x(GetSCM2(bad2good(AA)), cdr(GetSCM2(bad2good(R0))));
					set_cdr_x(GetSCM2(bad2good(R0)), GetSCM2(bad2good(AA)));
					CL = good2bad(module_variable(GetSCM2(bad2good(R0)), cell_closure));
					set_cdr_x(GetSCM2(bad2good(CL)), GetSCM2(bad2good(AA)));
				}

				R1 = good2bad(Getstructscm2(cell_unspecified));
				goto vm_return;
			}

			push_cc(bad2good(R1)->rac, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_eval_check_func);
			gc_check();
			goto eval;
eval_check_func:
			push_cc(bad2good(R2)->rdc, GetSCM2(bad2good(R2)), GetSCM2(bad2good(R0)), cell_vm_eval2);
			goto evlis;
eval2:
			R1 = good2bad(Getstructscm2(cons_(bad2good(R2)->rac, GetSCM2(bad2good(R1)))));
			goto apply;
		}
	}
	else if(t == TSYMBOL)
	{
		if(GetSCM2(bad2good(R1)) == cell_symbol_boot_module)
		{
			goto vm_return;
		}

		if(GetSCM2(bad2good(R1)) == cell_symbol_current_module)
		{
			goto vm_return;
		}

		if(GetSCM2(bad2good(R1)) == cell_symbol_begin)  // FIXME
		{
			R1 = good2bad(Getstructscm2(cell_begin));
			goto vm_return;
		}

		R1 = good2bad(Getstructscm2(assert_defined(GetSCM2(bad2good(R1)), GetSCM2(module_ref(GetSCM2(bad2good(R0)), GetSCM2(bad2good(R1)))))));
		goto vm_return;
	}
	else if(t == TVARIABLE)
	{
		R1 = bad2good(bad2good(R1)->car)->cdr;
		goto vm_return;
	}
	else if(t == TBROKEN_HEART)
	{
		error(cell_symbol_system_error,  GetSCM2(bad2good(R1)));
	}
	else
	{
		goto vm_return;
	}

macro_expand:
	if(bad2good(R1)->type != TPAIR || bad2good(R1)->rac == cell_symbol_quote)
	{
		goto vm_return;
	}

	if(bad2good(R1)->rac == cell_symbol_lambda)
	{
		push_cc(bad2good(bad2good(R1)->cdr)->rdc, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_macro_expand_lambda);
		goto macro_expand;

macro_expand_lambda:
		bad2good(bad2good(R2)->cdr)->rdc = GetSCM2(bad2good(R1));
		R1 = R2;
		goto vm_return;
	}

	if(bad2good(R1)->type == TPAIR && GetSCM2(bad2good((MACRO = good2bad(Getstructscm2(get_macro(bad2good(R1)->rac)))))) != cell_f)
	{
		R1 = good2bad(Getstructscm2(cons_(GetSCM2(bad2good(MACRO)), bad2good(R1)->rdc)));
		push_cc(GetSCM2(bad2good(R1)), cell_nil, GetSCM2(bad2good(R0)), cell_vm_macro_expand);
		goto apply;
	}

	if(bad2good(R1)->rac == cell_symbol_define || bad2good(R1)->rac == cell_symbol_define_macro)
	{
		push_cc(bad2good(bad2good(R1)->cdr)->rdc, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_macro_expand_define);
		goto macro_expand;

macro_expand_define:
		bad2good(bad2good(R2)->cdr)->rdc = GetSCM2(bad2good(R1));
		R1 = R2;

		if(bad2good(R1)->rac == cell_symbol_define_macro)
		{
			push_cc(GetSCM2(bad2good(R1)), GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_macro_expand_define_macro);
			goto eval;

macro_expand_define_macro:
			R1 = R2;
		}

		goto vm_return;
	}

	if(bad2good(R1)->rac == cell_symbol_set_x)
	{
		push_cc(bad2good(bad2good(R1)->cdr)->rdc, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_macro_expand_set_x);
		goto macro_expand;

macro_expand_set_x:
		bad2good(bad2good(R2)->cdr)->rdc = GetSCM2(bad2good(R1));
		R1 = R2;
		goto vm_return;
	}

	if(bad2good(R1)->type == TPAIR && bad2good(bad2good(R1)->car)->type == TSYMBOL)
	{
		MACRO = good2bad(Getstructscm2(macro_get_handle(cell_symbol_portable_macro_expand)));
		EXPANDERS = good2bad(module_ref(GetSCM2(bad2good(R0)), cell_symbol_sc_expander_alist));
		if((bad2good(R1)->rac != cell_symbol_begin) && (GetSCM2(bad2good(MACRO)) != cell_f) && (GetSCM2(bad2good(EXPANDERS)) != cell_undefined))
		{
			MACRO = good2bad(Getstructscm2(assq(bad2good(R1)->rac, GetSCM2(bad2good(EXPANDERS)))));
			if(GetSCM2(bad2good(MACRO)) != cell_f)
			{
				SC_EXPAND = good2bad(module_ref(GetSCM2(bad2good(R0)), cell_symbol_macro_expand));
				R2 = R1;

				if(GetSCM2(bad2good(SC_EXPAND)) != cell_undefined && GetSCM2(bad2good(SC_EXPAND)) != cell_f)
				{
					R1 = good2bad(Getstructscm2(cons_(GetSCM2(bad2good(SC_EXPAND)), cons_(GetSCM2(bad2good(R1)), cell_nil))));
					goto apply;
				}
			}
		}
	}

	push_cc(bad2good(R1)->rac, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_macro_expand_car);
	goto macro_expand;

macro_expand_car:
	bad2good(R2)->car = R1;
	R1 = R2;

	if(bad2good(R1)->rdc == cell_nil)
	{
		goto vm_return;
	}

	push_cc(bad2good(R1)->rdc, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_macro_expand_cdr);
	goto macro_expand;

macro_expand_cdr:
	bad2good(R2)->cdr = R1;
	R1 = R2;
	goto vm_return;

begin:
	X = good2bad(Getstructscm2(cell_unspecified));

	while(GetSCM2(bad2good(R1)) != cell_nil)
	{
		gc_check();

		if(bad2good(R1)->type == TPAIR)
		{
			if(bad2good(bad2good(R1)->car)->rac == cell_symbol_primitive_load)
			{
				PROGRAM = good2bad(Getstructscm2(cons_(bad2good(R1)->rac, cell_nil)));
				push_cc(GetSCM2(bad2good(PROGRAM)), GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_begin_primitive_load);
				goto begin_expand;
begin_primitive_load:
				bad2good(R2)->car = R1;
				R1 = R2;
			}
		}

		if(bad2good(R1)->type == TPAIR && bad2good(bad2good(R1)->car)->type == TPAIR)
		{
			if(bad2good(bad2good(R1)->car)->rac == cell_symbol_begin)
			{
				R1 = good2bad(Getstructscm2(append2(bad2good(bad2good(R1)->car)->rdc, bad2good(R1)->rdc)));
			}
		}

		if(bad2good(R1)->rdc == cell_nil)
		{
			R1 = bad2good(R1)->car;
			goto eval;
		}

		push_cc(bad2good(R1)->rac, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_begin_eval);
		goto eval;
begin_eval:
		X = R1;
		R1 = bad2good(R2)->cdr;
	}

	R1 = X;
	goto vm_return;
begin_expand:
	X = good2bad(Getstructscm2(cell_unspecified));

	while(GetSCM2(bad2good(R1)) != cell_nil)
	{
		gc_check();

		if(bad2good(R1)->type == TPAIR)
		{
			if(bad2good(bad2good(R1)->car)->type == TPAIR && bad2good(bad2good(R1)->car)->rac == cell_symbol_begin)
			{
				R1 = good2bad(Getstructscm2(append2(bad2good(bad2good(R1)->car)->rdc, bad2good(R1)->rdc)));
			}

			if(bad2good(bad2good(R1)->car)->rac == cell_symbol_primitive_load)
			{
				push_cc(bad2good(bad2good(bad2good(R1)->car)->cdr)->rac, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_begin_expand_primitive_load);
				goto eval; // FIXME: expand too?!
begin_expand_primitive_load:

				if(bad2good(R1)->type == TNUMBER && bad2good(R1)->value == 0)
				{
					;
				}
				else if(bad2good(R1)->type == TSTRING)
				{
					INPUT = good2bad(Getstructscm2(set_current_input_port(open_input_file(GetSCM2(bad2good(R1))))));
				}
				else if(bad2good(R1)->type == TPORT)
				{
					INPUT = good2bad(Getstructscm2(set_current_input_port(GetSCM2(bad2good(R1)))));
				}
				else
				{
					assert(0);
				}

				push_cc(GetSCM2(bad2good(INPUT)), GetSCM2(bad2good(R2)), GetSCM2(bad2good(R0)), cell_vm_return);
				//X = good2bad(Getstructscm2(read_input_file_env()));

				if(g_debug > 4)
				{
					module_printer(GetSCM2(bad2good(M0)));
				}

				gc_pop_frame();
				INPUT = R1;
				R1 = X;
				set_current_input_port(GetSCM2(bad2good(INPUT)));
				R1 = good2bad(Getstructscm2(cons_(cell_symbol_begin, GetSCM2(bad2good(R1)))));
				bad2good(R2)->car = R1;
				R1 = R2;
				continue;
			}
		}

		push_cc(bad2good(R1)->rac, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_begin_expand_macro);
		goto macro_expand;
begin_expand_macro:

		if(GetSCM2(bad2good(R1)) != bad2good(R2)->rac)
		{
			bad2good(R2)->car = R1;
			R1 = R2;
			continue;
		}

		R1 = R2;
		expand_variable(bad2good(R1)->rac, cell_nil);
		push_cc(bad2good(R1)->rac, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_begin_expand_eval);
		goto eval;
begin_expand_eval:
		X = R1;
		R1 = bad2good(R2)->cdr;
	}

	R1 = X;
	goto vm_return;
vm_if:
	push_cc(bad2good(R1)->rac, GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_if_expr);
	goto eval;
if_expr:
	X = R1;
	R1 = R2;

	if(GetSCM2(bad2good(X)) != cell_f)
	{
		R1 = bad2good(bad2good(R1)->cdr)->car;
		goto eval;
	}

	if(bad2good(bad2good(R1)->cdr)->rdc != cell_nil)
	{
		R1 = bad2good(bad2good(bad2good(R1)->cdr)->cdr)->car;
		goto eval;
	}

	R1 = good2bad(Getstructscm2(cell_unspecified));
	goto vm_return;
call_with_current_continuation:
	gc_push_frame();
	X = good2bad(Getstructscm2(make_cell__ (TCONTINUATION, g_continuations++, g_stack)));
	V = good2bad(make_vector__(STACK_SIZE - g_stack));

	for(t = g_stack; t < STACK_SIZE; t++)
	{
		vector_set_x_(GetSCM2(bad2good(V)), t - g_stack, (SCM) g_stack_array[t]);
	}

	bad2good(X)->continuation = GetSCM2(bad2good(V));
	gc_pop_frame();
	push_cc(cons_(bad2good(R1)->rac, cons_(GetSCM2(bad2good(X)), cell_nil)), GetSCM2(bad2good(X)), GetSCM2(bad2good(R0)), cell_vm_call_with_current_continuation2);
	goto apply;
call_with_current_continuation2:
	V = good2bad(make_vector__(STACK_SIZE - g_stack));

	for(t = g_stack; t < STACK_SIZE; t++)
	{
		vector_set_x_(GetSCM2(bad2good(V)), t - g_stack, (SCM)g_stack_array[t]);
	}

	bad2good(R2)->continuation = GetSCM2(bad2good(V));
	goto vm_return;
call_with_values:
	push_cc(cons_(bad2good(R1)->rac, cell_nil), GetSCM2(bad2good(R1)), GetSCM2(bad2good(R0)), cell_vm_call_with_values2);
	goto apply;
call_with_values2:

	if(bad2good(R1)->type == TVALUES)
	{
		R1 = bad2good(R1)->cdr;
	}

	R1 = good2bad(Getstructscm2(cons_(bad2good(bad2good(R2)->cdr)->rac, GetSCM2(bad2good(R1)))));
	goto apply;
vm_return:
	X = R1;
	gc_pop_frame();
	R1 = X;
	goto eval_apply;
}
