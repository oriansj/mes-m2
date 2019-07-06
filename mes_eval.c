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
int fdputc (int c, int fd);
int fdputs (char const* s, int fd);
int eputs (char const* s);
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
struct scm* display_error_ (SCM x);
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
SCM make_cell__(long type, SCM car, SCM cdr);
struct scm* vector_ref_(SCM x, SCM i);
struct scm* make_vector__(SCM k);
struct scm* vector_set_x_(SCM x, SCM i, SCM e);

SCM eval_apply()
{
	SCM aa;
	struct scm* AA;
	SCM args;
	struct scm* ARGS;
	SCM body;
	struct scm* BODY;
	SCM cl;
	struct scm* CL;
	SCM entry;
	struct scm* ENTRY;
	SCM expanders;
	struct scm* EXPANDERS;
	SCM formals;
	struct scm* FORMALS;
	SCM input;
	struct scm* INPUT;
	SCM name;
	struct scm* NAME;
	SCM macro;
	struct scm* MACRO;
	SCM p;
	struct scm* P;
	SCM program;
	struct scm* PROGRAM;
	SCM sc_expand;
	struct scm* SC_EXPAND;
	SCM v;
	struct scm* V;
	SCM x;
	struct scm* X;
	int global_p;
	int macro_p;
	int t;
	SCM c;
	SCM r3;
	struct scm* C;
eval_apply:

	r3 = GetSCM2(bad2good(R3));
	if(r3 == cell_vm_evlis2)
	{
		goto evlis2;
	}
	else if(r3 == cell_vm_evlis3)
	{
		goto evlis3;
	}
	else if(r3 == cell_vm_eval_check_func)
	{
		goto eval_check_func;
	}
	else if(r3 == cell_vm_eval2)
	{
		goto eval2;
	}
	else if(r3 == cell_vm_apply2)
	{
		goto apply2;
	}
	else if(r3 == cell_vm_if_expr)
	{
		goto if_expr;
	}
	else if(r3 == cell_vm_begin_eval)
	{
		goto begin_eval;
	}
	else if(r3 == cell_vm_eval_set_x)
	{
		goto eval_set_x;
	}
	else if(r3 == cell_vm_macro_expand_car)
	{
		goto macro_expand_car;
	}
	else if(r3 == cell_vm_return)
	{
		goto vm_return;
	}
	else if(r3 == cell_vm_macro_expand_cdr)
	{
		goto macro_expand_cdr;
	}
	else if(r3 == cell_vm_eval_define)
	{
		goto eval_define;
	}
	else if(r3 == cell_vm_macro_expand)
	{
		goto macro_expand;
	}
	else if(r3 == cell_vm_macro_expand_lambda)
	{
		goto macro_expand_lambda;
	}
	else if(r3 == cell_vm_eval_pmatch_car)
	{
		goto eval_pmatch_car;
	}
	else if(r3 == cell_vm_begin_expand_macro)
	{
		goto begin_expand_macro;
	}
	else if(r3 == cell_vm_macro_expand_define)
	{
		goto macro_expand_define;
	}
	else if(r3 == cell_vm_begin_expand_eval)
	{
		goto begin_expand_eval;
	}
	else if(r3 == cell_vm_call_with_current_continuation2)
	{
		goto call_with_current_continuation2;
	}
	else if(r3 == cell_vm_macro_expand_set_x)
	{
		goto macro_expand_set_x;
	}
	else if(r3 == cell_vm_eval_pmatch_cdr)
	{
		goto eval_pmatch_cdr;
	}
	else if(r3 == cell_vm_macro_expand_define_macro)
	{
		goto macro_expand_define_macro;
	}
	else if(r3 == cell_vm_begin_primitive_load)
	{
		goto begin_primitive_load;
	}
	else if(r3 == cell_vm_evlis)
	{
		goto evlis;
	}
	else if(r3 == cell_vm_apply)
	{
		goto apply;
	}
	else if(r3 == cell_vm_eval)
	{
		goto eval;
	}
	else if(r3 == cell_vm_eval_macro_expand_eval)
	{
		goto eval_macro_expand_eval;
	}
	else if(r3 == cell_vm_eval_macro_expand_expand)
	{
		goto eval_macro_expand_expand;
	}
	else if(r3 == cell_vm_begin)
	{
		goto begin;
	}
	else if(r3 == cell_vm_begin_expand)
	{
		goto begin_expand;
	}
	else if(r3 == cell_vm_begin_expand_primitive_load)
	{
		goto begin_expand_primitive_load;
	}
	else if(r3 == cell_vm_if)
	{
		goto vm_if;
	}
	else if(r3 == cell_vm_call_with_values2)
	{
		goto call_with_values2;
	}
	else if(r3 == cell_unspecified)
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

	push_cc(bad2good(R1)->rac, GetSCM2(bad2good(R1)), r0, cell_vm_evlis2);
	goto eval;
evlis2:
	push_cc(g_cells[r2].rdc, GetSCM2(bad2good(R1)), r0, cell_vm_evlis3);
	goto evlis;
evlis3:
	R1 = good2bad(Getstructscm2(cons_(r2, GetSCM2(bad2good(R1)))));
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
		cl = g_cells[bad2good(R1)->rac].closure;
		body = g_cells[g_cells[cl].rdc].rdc;
		formals = g_cells[g_cells[cl].rdc].rac;
		args = bad2good(R1)->rdc;
		aa = g_cells[g_cells[cl].rac].rdc;
		aa = g_cells[aa].rdc;
		check_formals(bad2good(R1)->rac, formals, bad2good(R1)->rdc);
		p = pairlis(formals, args, aa);
		call_lambda(body, p);
		goto begin;
	}
	else if(t == TCONTINUATION)
	{
		v = g_cells[bad2good(R1)->rac].continuation;

		if(g_cells[v].length)
		{
			for(t = 0; t < g_cells[v].length; t++)
			{
				g_stack_array[STACK_SIZE - g_cells[v].length + t] = good2bad(vector_ref_(v, t));
			}

			g_stack = STACK_SIZE - g_cells[v].length;
		}

		x = GetSCM2(bad2good(R1));
		gc_pop_frame();
		R1 = g_cells[g_cells[x].rdc].car;
		goto eval_apply;
	}
	else if(t == TSPECIAL)
	{
		c = bad2good(R1)->rac;

		if(c == cell_vm_apply)
		{
			push_cc(cons_(g_cells[bad2good(R1)->rdc].rac, g_cells[g_cells[bad2good(R1)->rdc].rdc].rac), GetSCM2(bad2good(R1)), r0, cell_vm_return);
			goto apply;
		}
		else if(c ==  cell_vm_eval)
		{
			push_cc(g_cells[bad2good(R1)->rdc].rac, GetSCM2(bad2good(R1)), g_cells[g_cells[bad2good(R1)->rdc].rdc].rac, cell_vm_return);
			goto eval;
		}
		else if(c ==  cell_vm_begin_expand)
		{
			push_cc(cons_(g_cells[bad2good(R1)->rdc].rac, cell_nil), GetSCM2(bad2good(R1)), g_cells[g_cells[bad2good(R1)->rdc].rdc].rac, cell_vm_return);
			goto begin_expand;
		}
		else if(c ==  cell_call_with_current_continuation)
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
			R1 = good2bad(Getstructscm2(r0));
			goto vm_return;
		}

		if(bad2good(R1)->rac == cell_symbol_boot_module)
		{
			R1 = good2bad(Getstructscm2(m0));
			goto vm_return;
		}
	}
	else if(t == TPAIR)
	{
		if(g_cells[bad2good(R1)->rac].rac == cell_symbol_lambda)
		{
			formals = g_cells[g_cells[bad2good(R1)->rac].rdc].rac;
			args = bad2good(R1)->rdc;
			body = g_cells[g_cells[bad2good(R1)->rac].rdc].rdc;
			p = pairlis(formals, bad2good(R1)->rdc, r0);
			check_formals(GetSCM2(bad2good(R1)), formals, args);
			call_lambda(body, p);
			goto begin;
		}
	}

	// write_error_ (R1->car);
	// eputs ("\n");
	push_cc(bad2good(R1)->rac, GetSCM2(bad2good(R1)), r0, cell_vm_apply2);
	goto eval;
apply2:
	check_apply(GetSCM2(bad2good(R1)), g_cells[r2].rac);
	R1 = good2bad(Getstructscm2(cons_(GetSCM2(bad2good(R1)), g_cells[r2].rdc)));
	goto apply;
eval:
	t = bad2good(R1)->type;

	if(t == TPAIR)
	{
		c = bad2good(R1)->rac;

		if(c ==  cell_symbol_pmatch_car)
		{
			push_cc(g_cells[bad2good(R1)->rdc].rac, GetSCM2(bad2good(R1)), r0, cell_vm_eval_pmatch_car);
			goto eval;
eval_pmatch_car:
			x = GetSCM2(bad2good(R1));
			gc_pop_frame();
			R1 = g_cells[x].car;
			goto eval_apply;
		}
		else if(c ==  cell_symbol_pmatch_cdr)
		{
			push_cc(g_cells[bad2good(R1)->rdc].rac, GetSCM2(bad2good(R1)), r0, cell_vm_eval_pmatch_cdr);
			goto eval;
eval_pmatch_cdr:
			x = GetSCM2(bad2good(R1));
			gc_pop_frame();
			R1 = g_cells[x].cdr;
			goto eval_apply;
		}
		else if(c ==  cell_symbol_quote)
		{
			x = GetSCM2(bad2good(R1));
			gc_pop_frame();
			R1 = g_cells[g_cells[x].rdc].car;
			goto eval_apply;
		}
		else if(c ==  cell_symbol_begin)
		{
			goto begin;
		}
		else if(c ==  cell_symbol_lambda)
		{
			R1 = good2bad(Getstructscm2(make_closure_(g_cells[bad2good(R1)->rdc].rac, g_cells[bad2good(R1)->rdc].rdc, r0)));
			goto vm_return;
		}
		else if(c ==  cell_symbol_if)
		{
			R1 = bad2good(R1)->cdr;
			goto vm_if;
		}
		else if(c ==  cell_symbol_set_x)
		{
			push_cc(g_cells[g_cells[bad2good(R1)->rdc].rdc].rac, GetSCM2(bad2good(R1)), r0, cell_vm_eval_set_x);
			goto eval;
eval_set_x:
			R1 = good2bad(Getstructscm2(set_env_x(g_cells[g_cells[r2].rdc].rac, GetSCM2(bad2good(R1)), r0)));
			goto vm_return;
		}
		else if(c == cell_vm_macro_expand)
		{
			push_cc(g_cells[bad2good(R1)->rdc].rac, GetSCM2(bad2good(R1)), r0, cell_vm_eval_macro_expand_eval);
			goto eval;
eval_macro_expand_eval:
			push_cc(GetSCM2(bad2good(R1)), r2, r0, cell_vm_eval_macro_expand_expand);
			goto macro_expand;
eval_macro_expand_expand:
			goto vm_return;
		}
		else
		{
			if(bad2good(R1)->type == TPAIR && (bad2good(R1)->rac == cell_symbol_define || bad2good(R1)->rac == cell_symbol_define_macro))
			{
				global_p = g_cells[g_cells[r0].rac].rac != cell_closure;
				macro_p = bad2good(R1)->rac == cell_symbol_define_macro;

				if(global_p)
				{
					name = g_cells[bad2good(R1)->rdc].rac;

					if(g_cells[g_cells[bad2good(R1)->rdc].rac].type == TPAIR)
					{
						name = g_cells[name].rac;
					}

					if(macro_p)
					{
						entry = assq(name, g_macros);

						if(entry == cell_f)
						{
							macro_set_x(name, cell_f);
						}
					}
					else
					{
						entry = GetSCM2(module_variable(r0, name));

						if(entry == cell_f)
						{
							module_define_x(m0, name, cell_f);
						}
					}
				}

				r2 = GetSCM2(bad2good(R1));

				if(g_cells[g_cells[bad2good(R1)->rdc].rac].type != TPAIR)
				{
					push_cc(g_cells[g_cells[bad2good(R1)->rdc].rdc].rac, r2, cons_(cons_(g_cells[bad2good(R1)->rdc].rac, g_cells[bad2good(R1)->rdc].rac), r0), cell_vm_eval_define);
					goto eval;
				}
				else
				{
					p = pairlis(g_cells[bad2good(R1)->rdc].rac, g_cells[bad2good(R1)->rdc].rac, r0);
					formals = g_cells[g_cells[bad2good(R1)->rdc].rac].rdc;
					body = g_cells[bad2good(R1)->rdc].rdc;

					if(macro_p || global_p)
					{
						expand_variable(body, formals);
					}

					R1 = good2bad(Getstructscm2(cons_(cell_symbol_lambda, cons_(formals, body))));
					push_cc(GetSCM2(bad2good(R1)), r2, p, cell_vm_eval_define);
					goto eval;
				}

eval_define:
				name = g_cells[g_cells[r2].rdc].rac;

				if(g_cells[g_cells[g_cells[r2].rdc].rac].type == TPAIR)
				{
					name = g_cells[name].rac;
				}

				if(macro_p)
				{
					entry = macro_get_handle(name);
					R1 = good2bad(Getstructscm2(make_cell__ (TMACRO, GetSCM2(bad2good(R1)), g_cells[name].rdc)));
					set_cdr_x(entry, GetSCM2(bad2good(R1)));
				}
				else if(global_p)
				{
					entry = GetSCM2(module_variable(r0, name));
					set_cdr_x(entry, GetSCM2(bad2good(R1)));
				}
				else
				{
					entry = cons_(name, GetSCM2(bad2good(R1)));
					aa = cons_(entry, cell_nil);
					set_cdr_x(aa, cdr(r0));
					set_cdr_x(r0, aa);
					cl = GetSCM2(module_variable(r0, cell_closure));
					set_cdr_x(cl, aa);
				}

				R1 = good2bad(Getstructscm2(cell_unspecified));
				goto vm_return;
			}

			push_cc(bad2good(R1)->rac, GetSCM2(bad2good(R1)), r0, cell_vm_eval_check_func);
			gc_check();
			goto eval;
eval_check_func:
			push_cc(g_cells[r2].rdc, r2, r0, cell_vm_eval2);
			goto evlis;
eval2:
			R1 = good2bad(Getstructscm2(cons_(g_cells[r2].rac, GetSCM2(bad2good(R1)))));
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

		R1 = good2bad(Getstructscm2(assert_defined(GetSCM2(bad2good(R1)), GetSCM2(module_ref(r0, GetSCM2(bad2good(R1)))))));
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
		push_cc(g_cells[bad2good(R1)->rdc].rdc, GetSCM2(bad2good(R1)), r0, cell_vm_macro_expand_lambda);
		goto macro_expand;

macro_expand_lambda:
		g_cells[g_cells[r2].rdc].rdc = GetSCM2(bad2good(R1));
		R1 = good2bad(Getstructscm2(r2));
		goto vm_return;
	}

	if(bad2good(R1)->type == TPAIR && (macro = get_macro(bad2good(R1)->rac)) != cell_f)
	{
		R1 = good2bad(Getstructscm2(cons_(macro, bad2good(R1)->rdc)));
		push_cc(GetSCM2(bad2good(R1)), cell_nil, r0, cell_vm_macro_expand);
		goto apply;
	}

	if(bad2good(R1)->rac == cell_symbol_define || bad2good(R1)->rac == cell_symbol_define_macro)
	{
		push_cc(g_cells[bad2good(R1)->rdc].rdc, GetSCM2(bad2good(R1)), r0, cell_vm_macro_expand_define);
		goto macro_expand;

macro_expand_define:
		g_cells[g_cells[r2].rdc].rdc = GetSCM2(bad2good(R1));
		R1 = good2bad(Getstructscm2(r2));

		if(bad2good(R1)->rac == cell_symbol_define_macro)
		{
			push_cc(GetSCM2(bad2good(R1)), GetSCM2(bad2good(R1)), r0, cell_vm_macro_expand_define_macro);
			goto eval;

macro_expand_define_macro:
			R1 = good2bad(Getstructscm2(r2));
		}

		goto vm_return;
	}

	if(bad2good(R1)->rac == cell_symbol_set_x)
	{
		push_cc(g_cells[bad2good(R1)->rdc].rdc, GetSCM2(bad2good(R1)), r0, cell_vm_macro_expand_set_x);
		goto macro_expand;

macro_expand_set_x:
		g_cells[g_cells[r2].rdc].rdc = GetSCM2(bad2good(R1));
		R1 = good2bad(Getstructscm2(r2));
		goto vm_return;
	}

	if(bad2good(R1)->type == TPAIR && g_cells[bad2good(R1)->rac].type == TSYMBOL)
	{
		macro = macro_get_handle(cell_symbol_portable_macro_expand);
		expanders = GetSCM2(module_ref(r0, cell_symbol_sc_expander_alist));
		if((bad2good(R1)->rac != cell_symbol_begin) && (macro != cell_f) && (expanders != cell_undefined))
		{
			macro = assq(bad2good(R1)->rac, expanders);
			if(macro != cell_f)
			{
				sc_expand = GetSCM2(module_ref(r0, cell_symbol_macro_expand));
				r2 = GetSCM2(bad2good(R1));

				if(sc_expand != cell_undefined && sc_expand != cell_f)
				{
					R1 = good2bad(Getstructscm2(cons_(sc_expand, cons_(GetSCM2(bad2good(R1)), cell_nil))));
					goto apply;
				}
			}
		}
	}

	push_cc(bad2good(R1)->rac, GetSCM2(bad2good(R1)), r0, cell_vm_macro_expand_car);
	goto macro_expand;

macro_expand_car:
	g_cells[r2].rac = GetSCM2(bad2good(R1));
	R1 = good2bad(Getstructscm2(r2));

	if(bad2good(R1)->rdc == cell_nil)
	{
		goto vm_return;
	}

	push_cc(bad2good(R1)->rdc, GetSCM2(bad2good(R1)), r0, cell_vm_macro_expand_cdr);
	goto macro_expand;

macro_expand_cdr:
	g_cells[r2].rdc = GetSCM2(bad2good(R1));
	R1 = good2bad(Getstructscm2(r2));
	goto vm_return;

begin:
	x = cell_unspecified;

	while(GetSCM2(bad2good(R1)) != cell_nil)
	{
		gc_check();

		if(bad2good(R1)->type == TPAIR)
		{
			if(g_cells[bad2good(R1)->rac].rac == cell_symbol_primitive_load)
			{
				program = cons_(bad2good(R1)->rac, cell_nil);
				push_cc(program, GetSCM2(bad2good(R1)), r0, cell_vm_begin_primitive_load);
				goto begin_expand;
begin_primitive_load:
				g_cells[r2].rac = GetSCM2(bad2good(R1));
				R1 = good2bad(Getstructscm2(r2));
			}
		}

		if(bad2good(R1)->type == TPAIR && g_cells[bad2good(R1)->rac].type == TPAIR)
		{
			if(g_cells[bad2good(R1)->rac].rac == cell_symbol_begin)
			{
				R1 = good2bad(Getstructscm2(append2(g_cells[bad2good(R1)->rac].rdc, bad2good(R1)->rdc)));
			}
		}

		if(bad2good(R1)->rdc == cell_nil)
		{
			R1 = bad2good(R1)->car;
			goto eval;
		}

		push_cc(bad2good(R1)->rac, GetSCM2(bad2good(R1)), r0, cell_vm_begin_eval);
		goto eval;
begin_eval:
		x = GetSCM2(bad2good(R1));
		R1 = g_cells[r2].cdr;
	}

	R1 = good2bad(Getstructscm2(x));
	goto vm_return;
begin_expand:
	x = cell_unspecified;

	while(GetSCM2(bad2good(R1)) != cell_nil)
	{
		gc_check();

		if(bad2good(R1)->type == TPAIR)
		{
			if(g_cells[bad2good(R1)->rac].type == TPAIR && g_cells[bad2good(R1)->rac].rac == cell_symbol_begin)
			{
				R1 = good2bad(Getstructscm2(append2(g_cells[bad2good(R1)->rac].rdc, bad2good(R1)->rdc)));
			}

			if(g_cells[bad2good(R1)->rac].rac == cell_symbol_primitive_load)
			{
				push_cc(g_cells[g_cells[bad2good(R1)->rac].rdc].rac, GetSCM2(bad2good(R1)), r0, cell_vm_begin_expand_primitive_load);
				goto eval; // FIXME: expand too?!
begin_expand_primitive_load:

				if(bad2good(R1)->type == TNUMBER && bad2good(R1)->value == 0)
				{
					;
				}
				else if(bad2good(R1)->type == TSTRING)
				{
					input = set_current_input_port(open_input_file(GetSCM2(bad2good(R1))));
				}
				else if(bad2good(R1)->type == TPORT)
				{
					input = set_current_input_port(GetSCM2(bad2good(R1)));
				}
				else
				{
					assert(0);
				}

				push_cc(input, r2, r0, cell_vm_return);
				x = read_input_file_env();

				if(g_debug > 4)
				{
					module_printer(m0);
				}

				gc_pop_frame();
				input = GetSCM2(bad2good(R1));
				R1 = good2bad(Getstructscm2(x));
				set_current_input_port(input);
				R1 = good2bad(Getstructscm2(cons_(cell_symbol_begin, GetSCM2(bad2good(R1)))));
				g_cells[r2].rac = GetSCM2(bad2good(R1));
				R1 = good2bad(Getstructscm2(r2));
				continue;
			}
		}

		push_cc(bad2good(R1)->rac, GetSCM2(bad2good(R1)), r0, cell_vm_begin_expand_macro);
		goto macro_expand;
begin_expand_macro:

		if(GetSCM2(bad2good(R1)) != g_cells[r2].rac)
		{
			g_cells[r2].rac = GetSCM2(bad2good(R1));
			R1 = good2bad(Getstructscm2(r2));
			continue;
		}

		R1 = good2bad(Getstructscm2(r2));
		expand_variable(bad2good(R1)->rac, cell_nil);
		push_cc(bad2good(R1)->rac, GetSCM2(bad2good(R1)), r0, cell_vm_begin_expand_eval);
		goto eval;
begin_expand_eval:
		x = GetSCM2(bad2good(R1));
		R1 = g_cells[r2].cdr;
	}

	R1 = good2bad(Getstructscm2(x));
	goto vm_return;
vm_if:
	push_cc(bad2good(R1)->rac, GetSCM2(bad2good(R1)), r0, cell_vm_if_expr);
	goto eval;
if_expr:
	x = GetSCM2(bad2good(R1));
	R1 = good2bad(Getstructscm2(r2));

	if(x != cell_f)
	{
		R1 = bad2good(bad2good(R1)->cdr)->car;
		goto eval;
	}

	if(g_cells[bad2good(R1)->rdc].rdc != cell_nil)
	{
		R1 = bad2good(bad2good(bad2good(R1)->cdr)->cdr)->car;
		goto eval;
	}

	R1 = good2bad(Getstructscm2(cell_unspecified));
	goto vm_return;
call_with_current_continuation:
	gc_push_frame();
	x = make_cell__ (TCONTINUATION, g_continuations++, g_stack);
	v = GetSCM2(make_vector__(STACK_SIZE - g_stack));

	for(t = g_stack; t < STACK_SIZE; t++)
	{
		vector_set_x_(v, t - g_stack, (SCM) g_stack_array[t]);
	}

	g_cells[x].continuation = v;
	gc_pop_frame();
	push_cc(cons_(bad2good(R1)->rac, cons_(x, cell_nil)), x, r0, cell_vm_call_with_current_continuation2);
	goto apply;
call_with_current_continuation2:
	v = GetSCM2(make_vector__(STACK_SIZE - g_stack));

	for(t = g_stack; t < STACK_SIZE; t++)
	{
		vector_set_x_(v, t - g_stack, (SCM)g_stack_array[t]);
	}

	g_cells[r2].continuation = v;
	goto vm_return;
call_with_values:
	push_cc(cons_(bad2good(R1)->rac, cell_nil), GetSCM2(bad2good(R1)), r0, cell_vm_call_with_values2);
	goto apply;
call_with_values2:

	if(bad2good(R1)->type == TVALUES)
	{
		R1 = bad2good(R1)->cdr;
	}

	R1 = good2bad(Getstructscm2(cons_(g_cells[g_cells[r2].rdc].rac, GetSCM2(bad2good(R1)))));
	goto apply;
vm_return:
	x = GetSCM2(bad2good(R1));
	gc_pop_frame();
	R1 = good2bad(Getstructscm2(x));
	goto eval_apply;
}
