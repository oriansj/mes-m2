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
	SCM args;
	SCM body;
	SCM cl;
	SCM entry;
	SCM expanders;
	SCM formals;
	SCM input;
	SCM name;
	SCM macro;
	SCM p;
	SCM program;
	SCM sc_expand;
	SCM v;
	SCM x;
	int global_p;
	int macro_p;
	int t;
	SCM c;
eval_apply:

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
		return r1;
	}
	else
	{
		error(cell_symbol_system_error, GetSCM2(bad2good(make_string ("eval/apply unknown continuation", strlen("eval/apply unknown continuation")), g_cells), g_cells));
	}

evlis:

	if(r1 == cell_nil)
	{
		goto vm_return;
	}

	if(g_cells[r1].type != TPAIR)
	{
		goto eval;
	}

	push_cc(g_cells[r1].rac, r1, r0, cell_vm_evlis2);
	goto eval;
evlis2:
	push_cc(g_cells[r2].rdc, r1, r0, cell_vm_evlis3);
	goto evlis;
evlis3:
	r1 = cons_(r2, r1);
	goto vm_return;
apply:
	g_stack_array[g_stack + FRAME_PROCEDURE] = g_cells[r1].car;
	t = g_cells[g_cells[r1].rac].type;

	if(t == TSTRUCT && builtin_p(g_cells[r1].rac) == cell_t)
	{
		check_formals(g_cells[r1].rac, GetSCM2(bad2good(builtin_arity(g_cells[r1].rac), g_cells), g_cells), g_cells[r1].rdc);
		r1 = GetSCM2(apply_builtin(g_cells[r1].rac, g_cells[r1].rdc), g_cells);    /// FIXME: move into eval_apply
		goto vm_return;
	}
	else if(t == TCLOSURE)
	{
		cl = g_cells[g_cells[r1].rac].closure;
		body = g_cells[g_cells[cl].rdc].rdc;
		formals = g_cells[g_cells[cl].rdc].rac;
		args = g_cells[r1].rdc;
		aa = g_cells[g_cells[cl].rac].rdc;
		aa = g_cells[aa].rdc;
		check_formals(g_cells[r1].rac, formals, g_cells[r1].rdc);
		p = pairlis(formals, args, aa);
		call_lambda(body, p);
		goto begin;
	}
	else if(t == TCONTINUATION)
	{
		v = g_cells[g_cells[r1].rac].continuation;

		if(g_cells[v].length)
		{
			for(t = 0; t < g_cells[v].length; t++)
			{
				g_stack_array[STACK_SIZE - g_cells[v].length + t] = good2bad(vector_ref_(v, t), g_cells);
			}

			g_stack = STACK_SIZE - g_cells[v].length;
		}

		x = r1;
		gc_pop_frame();
		r1 = g_cells[g_cells[x].rdc].rac;
		goto eval_apply;
	}
	else if(t == TSPECIAL)
	{
		c = g_cells[r1].rac;

		if(c == cell_vm_apply)
		{
			push_cc(cons_(g_cells[g_cells[r1].rdc].rac, g_cells[g_cells[g_cells[r1].rdc].rdc].rac), r1, r0, cell_vm_return);
			goto apply;
		}
		else if(c ==  cell_vm_eval)
		{
			push_cc(g_cells[g_cells[r1].rdc].rac, r1, g_cells[g_cells[g_cells[r1].rdc].rdc].rac, cell_vm_return);
			goto eval;
		}
		else if(c ==  cell_vm_begin_expand)
		{
			push_cc(cons_(g_cells[g_cells[r1].rdc].rac, cell_nil), r1, g_cells[g_cells[g_cells[r1].rdc].rdc].rac, cell_vm_return);
			goto begin_expand;
		}
		else if(c ==  cell_call_with_current_continuation)
		{
			r1 = g_cells[r1].rdc;
			goto call_with_current_continuation;
		}
		else
		{
			check_apply(cell_f, g_cells[r1].rac);
		}
	}
	else if(t == TSYMBOL)
	{
		if(g_cells[r1].rac == cell_symbol_call_with_values)
		{
			r1 = g_cells[r1].rdc;
			goto call_with_values;
		}

		if(g_cells[r1].rac == cell_symbol_current_module)
		{
			r1 = r0;
			goto vm_return;
		}

		if(g_cells[r1].rac == cell_symbol_boot_module)
		{
			r1 = m0;
			goto vm_return;
		}
	}
	else if(t == TPAIR)
	{
		if(g_cells[g_cells[r1].rac].rac == cell_symbol_lambda)
		{
			formals = g_cells[g_cells[g_cells[r1].rac].rdc].rac;
			args = g_cells[r1].rdc;
			body = g_cells[g_cells[g_cells[r1].rac].rdc].rdc;
			p = pairlis(formals, g_cells[r1].rdc, r0);
			check_formals(r1, formals, args);
			call_lambda(body, p);
			goto begin;
		}
	}

	// write_error_ (CAR (r1));
	// eputs ("\n");
	push_cc(g_cells[r1].rac, r1, r0, cell_vm_apply2);
	goto eval;
apply2:
	check_apply(r1, g_cells[r2].rac);
	r1 = cons_(r1, g_cells[r2].rdc);
	goto apply;
eval:
	t = g_cells[r1].type;

	if(t == TPAIR)
	{
		c = g_cells[r1].rac;

		if(c ==  cell_symbol_pmatch_car)
		{
			push_cc(g_cells[g_cells[r1].rdc].rac, r1, r0, cell_vm_eval_pmatch_car);
			goto eval;
eval_pmatch_car:
			x = r1;
			gc_pop_frame();
			r1 = g_cells[x].rac;
			goto eval_apply;
		}
		else if(c ==  cell_symbol_pmatch_cdr)
		{
			push_cc(g_cells[g_cells[r1].rdc].rac, r1, r0, cell_vm_eval_pmatch_cdr);
			goto eval;
eval_pmatch_cdr:
			x = r1;
			gc_pop_frame();
			r1 = g_cells[x].rdc;
			goto eval_apply;
		}
		else if(c ==  cell_symbol_quote)
		{
			x = r1;
			gc_pop_frame();
			r1 = g_cells[g_cells[x].rdc].rac;
			goto eval_apply;
		}
		else if(c ==  cell_symbol_begin)
		{
			goto begin;
		}
		else if(c ==  cell_symbol_lambda)
		{
			r1 = make_closure_(g_cells[g_cells[r1].rdc].rac, g_cells[g_cells[r1].rdc].rdc, r0);
			goto vm_return;
		}
		else if(c ==  cell_symbol_if)
		{
			r1 = g_cells[r1].rdc;
			goto vm_if;
		}
		else if(c ==  cell_symbol_set_x)
		{
			push_cc(g_cells[g_cells[g_cells[r1].rdc].rdc].rac, r1, r0, cell_vm_eval_set_x);
			goto eval;
eval_set_x:
			r1 = set_env_x(g_cells[g_cells[r2].rdc].rac, r1, r0);
			goto vm_return;
		}
		else if(c == cell_vm_macro_expand)
		{
			push_cc(g_cells[g_cells[r1].rdc].rac, r1, r0, cell_vm_eval_macro_expand_eval);
			goto eval;
eval_macro_expand_eval:
			push_cc(r1, r2, r0, cell_vm_eval_macro_expand_expand);
			goto macro_expand;
eval_macro_expand_expand:
			goto vm_return;
		}
		else
		{
			if(g_cells[r1].type == TPAIR && (g_cells[r1].rac == cell_symbol_define || g_cells[r1].rac == cell_symbol_define_macro))
			{
				global_p = g_cells[g_cells[r0].rac].rac != cell_closure;
				macro_p = g_cells[r1].rac == cell_symbol_define_macro;

				if(global_p)
				{
					name = g_cells[g_cells[r1].rdc].rac;

					if(g_cells[g_cells[g_cells[r1].rdc].rac].type == TPAIR)
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
						entry = GetSCM2(module_variable(r0, name), g_cells);

						if(entry == cell_f)
						{
							module_define_x(m0, name, cell_f);
						}
					}
				}

				r2 = r1;

				if(g_cells[g_cells[g_cells[r1].rdc].rac].type != TPAIR)
				{
					push_cc(g_cells[g_cells[g_cells[r1].rdc].rdc].rac, r2, cons_(cons_(g_cells[g_cells[r1].rdc].rac, g_cells[g_cells[r1].rdc].rac), r0), cell_vm_eval_define);
					goto eval;
				}
				else
				{
					p = pairlis(g_cells[g_cells[r1].rdc].rac, g_cells[g_cells[r1].rdc].rac, r0);
					formals = g_cells[g_cells[g_cells[r1].rdc].rac].rdc;
					body = g_cells[g_cells[r1].rdc].rdc;

					if(macro_p || global_p)
					{
						expand_variable(body, formals);
					}

					r1 = cons_(cell_symbol_lambda, cons_(formals, body));
					push_cc(r1, r2, p, cell_vm_eval_define);
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
					r1 = make_cell__ (TMACRO, r1, g_cells[name].rdc);
					set_cdr_x(entry, r1);
				}
				else if(global_p)
				{
					entry = GetSCM2(module_variable(r0, name), g_cells);
					set_cdr_x(entry, r1);
				}
				else
				{
					entry = cons_(name, r1);
					aa = cons_(entry, cell_nil);
					set_cdr_x(aa, cdr(r0));
					set_cdr_x(r0, aa);
					cl = GetSCM2(module_variable(r0, cell_closure), g_cells);
					set_cdr_x(cl, aa);
				}

				r1 = cell_unspecified;
				goto vm_return;
			}

			push_cc(g_cells[r1].rac, r1, r0, cell_vm_eval_check_func);
			gc_check();
			goto eval;
eval_check_func:
			push_cc(g_cells[r2].rdc, r2, r0, cell_vm_eval2);
			goto evlis;
eval2:
			r1 = cons_(g_cells[r2].rac, r1);
			goto apply;
		}
	}
	else if(t == TSYMBOL)
	{
		if(r1 == cell_symbol_boot_module)
		{
			goto vm_return;
		}

		if(r1 == cell_symbol_current_module)
		{
			goto vm_return;
		}

		if(r1 == cell_symbol_begin)  // FIXME
		{
			r1 = cell_begin;
			goto vm_return;
		}

		r1 = assert_defined(r1, GetSCM2(module_ref(r0, r1), g_cells));
		goto vm_return;
	}
	else if(t == TVARIABLE)
	{
		r1 = g_cells[g_cells[r1].rac].rdc;
		goto vm_return;
	}
	else if(t == TBROKEN_HEART)
	{
		error(cell_symbol_system_error,  r1);
	}
	else
	{
		goto vm_return;
	}

macro_expand:
	if(g_cells[r1].type != TPAIR || g_cells[r1].rac == cell_symbol_quote)
	{
		goto vm_return;
	}

	if(g_cells[r1].rac == cell_symbol_lambda)
	{
		push_cc(g_cells[g_cells[r1].rdc].rdc, r1, r0, cell_vm_macro_expand_lambda);
		goto macro_expand;

macro_expand_lambda:
		g_cells[g_cells[r2].rdc].rdc = r1;
		r1 = r2;
		goto vm_return;
	}

	if(g_cells[r1].type == TPAIR && (macro = get_macro(g_cells[r1].rac)) != cell_f)
	{
		r1 = cons_(macro, g_cells[r1].rdc);
		push_cc(r1, cell_nil, r0, cell_vm_macro_expand);
		goto apply;
	}

	if(g_cells[r1].rac == cell_symbol_define || g_cells[r1].rac == cell_symbol_define_macro)
	{
		push_cc(g_cells[g_cells[r1].rdc].rdc, r1, r0, cell_vm_macro_expand_define);
		goto macro_expand;

macro_expand_define:
		g_cells[g_cells[r2].rdc].rdc = r1;
		r1 = r2;

		if(g_cells[r1].rac == cell_symbol_define_macro)
		{
			push_cc(r1, r1, r0, cell_vm_macro_expand_define_macro);
			goto eval;

macro_expand_define_macro:
			r1 = r2;
		}

		goto vm_return;
	}

	if(g_cells[r1].rac == cell_symbol_set_x)
	{
		push_cc(g_cells[g_cells[r1].rdc].rdc, r1, r0, cell_vm_macro_expand_set_x);
		goto macro_expand;

macro_expand_set_x:
		g_cells[g_cells[r2].rdc].rdc = r1;
		r1 = r2;
		goto vm_return;
	}

	if(g_cells[r1].type == TPAIR && g_cells[g_cells[r1].rac].type == TSYMBOL)
	{
		macro = macro_get_handle(cell_symbol_portable_macro_expand);
		expanders = GetSCM2(module_ref(r0, cell_symbol_sc_expander_alist), g_cells);
		if((g_cells[r1].rac != cell_symbol_begin) && (macro != cell_f) && (expanders != cell_undefined))
		{
			macro = assq(g_cells[r1].rac, expanders);
			if(macro != cell_f)
			{
				sc_expand = GetSCM2(module_ref(r0, cell_symbol_macro_expand), g_cells);
				r2 = r1;

				if(sc_expand != cell_undefined && sc_expand != cell_f)
				{
					r1 = cons_(sc_expand, cons_(r1, cell_nil));
					goto apply;
				}
			}
		}
	}

	push_cc(g_cells[r1].rac, r1, r0, cell_vm_macro_expand_car);
	goto macro_expand;

macro_expand_car:
	g_cells[r2].rac = r1;
	r1 = r2;

	if(g_cells[r1].rdc == cell_nil)
	{
		goto vm_return;
	}

	push_cc(g_cells[r1].rdc, r1, r0, cell_vm_macro_expand_cdr);
	goto macro_expand;

macro_expand_cdr:
	g_cells[r2].rdc = r1;
	r1 = r2;
	goto vm_return;

begin:
	x = cell_unspecified;

	while(r1 != cell_nil)
	{
		gc_check();

		if(g_cells[r1].type == TPAIR)
		{
			if(g_cells[g_cells[r1].rac].rac == cell_symbol_primitive_load)
			{
				program = cons_(g_cells[r1].rac, cell_nil);
				push_cc(program, r1, r0, cell_vm_begin_primitive_load);
				goto begin_expand;
begin_primitive_load:
				g_cells[r2].rac = r1;
				r1 = r2;
			}
		}

		if(g_cells[r1].type == TPAIR && g_cells[g_cells[r1].rac].type == TPAIR)
		{
			if(g_cells[g_cells[r1].rac].rac == cell_symbol_begin)
			{
				r1 = append2(g_cells[g_cells[r1].rac].rdc, g_cells[r1].rdc);
			}
		}

		if(g_cells[r1].rdc == cell_nil)
		{
			r1 = g_cells[r1].rac;
			goto eval;
		}

		push_cc(g_cells[r1].rac, r1, r0, cell_vm_begin_eval);
		goto eval;
begin_eval:
		x = r1;
		r1 = g_cells[r2].rdc;
	}

	r1 = x;
	goto vm_return;
begin_expand:
	x = cell_unspecified;

	while(r1 != cell_nil)
	{
		gc_check();

		if(g_cells[r1].type == TPAIR)
		{
			if(g_cells[g_cells[r1].rac].type == TPAIR && g_cells[g_cells[r1].rac].rac == cell_symbol_begin)
			{
				r1 = append2(g_cells[g_cells[r1].rac].rdc, g_cells[r1].rdc);
			}

			if(g_cells[g_cells[r1].rac].rac == cell_symbol_primitive_load)
			{
				push_cc(g_cells[g_cells[g_cells[r1].rac].rdc].rac, r1, r0, cell_vm_begin_expand_primitive_load);
				goto eval; // FIXME: expand too?!
begin_expand_primitive_load:

				if(g_cells[r1].type == TNUMBER && g_cells[r1].value == 0)
				{
					;
				}
				else if(g_cells[r1].type == TSTRING)
				{
					input = set_current_input_port(open_input_file(r1));
				}
				else if(g_cells[r1].type == TPORT)
				{
					input = set_current_input_port(r1);
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
				input = r1;
				r1 = x;
				set_current_input_port(input);
				r1 = cons_(cell_symbol_begin, r1);
				g_cells[r2].rac = r1;
				r1 = r2;
				continue;
			}
		}

		push_cc(g_cells[r1].rac, r1, r0, cell_vm_begin_expand_macro);
		goto macro_expand;
begin_expand_macro:

		if(r1 != g_cells[r2].rac)
		{
			g_cells[r2].rac = r1;
			r1 = r2;
			continue;
		}

		r1 = r2;
		expand_variable(g_cells[r1].rac, cell_nil);
		push_cc(g_cells[r1].rac, r1, r0, cell_vm_begin_expand_eval);
		goto eval;
begin_expand_eval:
		x = r1;
		r1 = g_cells[r2].rdc;
	}

	r1 = x;
	goto vm_return;
vm_if:
	push_cc(g_cells[r1].rac, r1, r0, cell_vm_if_expr);
	goto eval;
if_expr:
	x = r1;
	r1 = r2;

	if(x != cell_f)
	{
		r1 = g_cells[g_cells[r1].rdc].rac;
		goto eval;
	}

	if(g_cells[g_cells[r1].rdc].rdc != cell_nil)
	{
		r1 = g_cells[g_cells[g_cells[r1].rdc].rdc].rac;
		goto eval;
	}

	r1 = cell_unspecified;
	goto vm_return;
call_with_current_continuation:
	gc_push_frame();
	x = make_cell__ (TCONTINUATION, g_continuations++, g_stack);
	v = GetSCM2(make_vector__(STACK_SIZE - g_stack), g_cells);

	for(t = g_stack; t < STACK_SIZE; t++)
	{
		vector_set_x_(v, t - g_stack, (SCM) g_stack_array[t]);
	}

	g_cells[x].continuation = v;
	gc_pop_frame();
	push_cc(cons_(g_cells[r1].rac, cons_(x, cell_nil)), x, r0, cell_vm_call_with_current_continuation2);
	goto apply;
call_with_current_continuation2:
	v = GetSCM2(make_vector__(STACK_SIZE - g_stack), g_cells);

	for(t = g_stack; t < STACK_SIZE; t++)
	{
		vector_set_x_(v, t - g_stack, (SCM)g_stack_array[t]);
	}

	g_cells[r2].continuation = v;
	goto vm_return;
call_with_values:
	push_cc(cons_(g_cells[r1].rac, cell_nil), r1, r0, cell_vm_call_with_values2);
	goto apply;
call_with_values2:

	if(g_cells[r1].type == TVALUES)
	{
		r1 = g_cells[r1].rdc;
	}

	r1 = cons_(g_cells[g_cells[r2].rdc].rac, r1);
	goto apply;
vm_return:
	x = r1;
	gc_pop_frame();
	r1 = x;
	goto eval_apply;
}
