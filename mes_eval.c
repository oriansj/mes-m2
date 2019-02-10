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

#define TYPE(x) g_cells[x].type
#define CAR(x) g_cells[x].rac
#define CDR(x) g_cells[x].rdc
#define VALUE(x) g_cells[x].rdc
#define VARIABLE(x) g_cells[x].rac
#define STRING(x) g_cells[x].rdc
#define LENGTH(x) g_cells[x].rac
#define VECTOR(x) g_cells[x].rdc
#define MAKE_NUMBER(n) make_cell__ (TNUMBER, 0, (long)n)
#define MAKE_STRING0(x) make_string (x, strlen (x))
#define MAKE_MACRO(name, x) make_cell__ (TMACRO, x, STRING (name))
#define MAKE_CHAR(n) make_cell__ (TCHAR, 0, n)
#define MAKE_CONTINUATION(n) make_cell__ (TCONTINUATION, n, g_stack)
#define CAAR(x) CAR (CAR (x))
#define CADR(x) CAR (CDR (x))
#define CDAR(x) CDR (CAR (x))
#define CDDR(x) CDR (CDR (x))
#define CADAR(x) CAR (CDR (CAR (x)))
#define CADDR(x) CAR (CDR (CDR (x)))
#define CDADAR(x) CAR (CDR (CAR (CDR (x))))
#define MACRO(x) g_cells[x].rac
#define CLOSURE(x) g_cells[x].rdc
#define CONTINUATION(x) g_cells[x].rdc


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
SCM cons(SCM x, SCM y);
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
		error(cell_symbol_system_error, GetSCM2(bad2good(MAKE_STRING0("eval/apply unknown continuation"), g_cells), g_cells));
	}

evlis:

	if(r1 == cell_nil)
	{
		goto vm_return;
	}

	if(TYPE(r1) != TPAIR)
	{
		goto eval;
	}

	push_cc(CAR(r1), r1, r0, cell_vm_evlis2);
	goto eval;
evlis2:
	push_cc(CDR(r2), r1, r0, cell_vm_evlis3);
	goto evlis;
evlis3:
	r1 = cons(r2, r1);
	goto vm_return;
apply:
	g_stack_array[g_stack + FRAME_PROCEDURE] = (struct scm*)CAR(r1);
	t = TYPE(CAR(r1));

	if(t == TSTRUCT && builtin_p(CAR(r1)) == cell_t)
	{
		check_formals(CAR(r1), GetSCM2(bad2good(builtin_arity(CAR(r1)), g_cells), g_cells), CDR(r1));
		r1 = GetSCM2(apply_builtin(CAR(r1), CDR(r1)), g_cells);    /// FIXME: move into eval_apply
		goto vm_return;
	}
	else if(t == TCLOSURE)
	{
		cl = CLOSURE(CAR(r1));
		body = CDDR(cl);
		formals = CADR(cl);
		args = CDR(r1);
		aa = CDAR(cl);
		aa = CDR(aa);
		check_formals(CAR(r1), formals, CDR(r1));
		p = pairlis(formals, args, aa);
		call_lambda(body, p);
		goto begin;
	}
	else if(t == TCONTINUATION)
	{
		v = CONTINUATION(CAR(r1));

		if(LENGTH(v))
		{
			for(t = 0; t < LENGTH(v); t++)
			{
				g_stack_array[STACK_SIZE - LENGTH(v) + t] = good2bad(vector_ref_(v, t), g_cells);
			}

			g_stack = STACK_SIZE - LENGTH(v);
		}

		x = r1;
		gc_pop_frame();
		r1 = CADR(x);
		goto eval_apply;
	}
	else if(t == TSPECIAL)
	{
		c = CAR(r1);

		if(c == cell_vm_apply)
		{
			push_cc(cons(CADR(r1), CADDR(r1)), r1, r0, cell_vm_return);
			goto apply;
		}
		else if(c ==  cell_vm_eval)
		{
			push_cc(CADR(r1), r1, CADDR(r1), cell_vm_return);
			goto eval;
		}
		else if(c ==  cell_vm_begin_expand)
		{
			push_cc(cons(CADR(r1), cell_nil), r1, CADDR(r1), cell_vm_return);
			goto begin_expand;
		}
		else if(c ==  cell_call_with_current_continuation)
		{
			r1 = CDR(r1);
			goto call_with_current_continuation;
		}
		else
		{
			check_apply(cell_f, CAR(r1));
		}
	}
	else if(t == TSYMBOL)
	{
		if(CAR(r1) == cell_symbol_call_with_values)
		{
			r1 = CDR(r1);
			goto call_with_values;
		}

		if(CAR(r1) == cell_symbol_current_module)
		{
			r1 = r0;
			goto vm_return;
		}

		if(CAR(r1) == cell_symbol_boot_module)
		{
			r1 = m0;
			goto vm_return;
		}
	}
	else if(t == TPAIR)
	{
		if(CAAR(r1) == cell_symbol_lambda)
		{
			formals = CADR(CAR(r1));
			args = CDR(r1);
			body = CDDR(CAR(r1));
			p = pairlis(formals, CDR(r1), r0);
			check_formals(r1, formals, args);
			call_lambda(body, p);
			goto begin;
		}
	}

	// write_error_ (CAR (r1));
	// eputs ("\n");
	push_cc(CAR(r1), r1, r0, cell_vm_apply2);
	goto eval;
apply2:
	check_apply(r1, CAR(r2));
	r1 = cons(r1, CDR(r2));
	goto apply;
eval:
	t = TYPE(r1);

	if(t == TPAIR)
	{
		c = CAR(r1);

		if(c ==  cell_symbol_pmatch_car)
		{
			push_cc(CADR(r1), r1, r0, cell_vm_eval_pmatch_car);
			goto eval;
eval_pmatch_car:
			x = r1;
			gc_pop_frame();
			r1 = CAR(x);
			goto eval_apply;
		}
		else if(c ==  cell_symbol_pmatch_cdr)
		{
			push_cc(CADR(r1), r1, r0, cell_vm_eval_pmatch_cdr);
			goto eval;
eval_pmatch_cdr:
			x = r1;
			gc_pop_frame();
			r1 = CDR(x);
			goto eval_apply;
		}
		else if(c ==  cell_symbol_quote)
		{
			x = r1;
			gc_pop_frame();
			r1 = CADR(x);
			goto eval_apply;
		}
		else if(c ==  cell_symbol_begin)
		{
			goto begin;
		}
		else if(c ==  cell_symbol_lambda)
		{
			r1 = make_closure_(CADR(r1), CDDR(r1), r0);
			goto vm_return;
		}
		else if(c ==  cell_symbol_if)
		{
			r1 = CDR(r1);
			goto vm_if;
		}
		else if(c ==  cell_symbol_set_x)
		{
			push_cc(CAR(CDDR(r1)), r1, r0, cell_vm_eval_set_x);
			goto eval;
eval_set_x:
			r1 = set_env_x(CADR(r2), r1, r0);
			goto vm_return;
		}
		else if(c == cell_vm_macro_expand)
		{
			push_cc(CADR(r1), r1, r0, cell_vm_eval_macro_expand_eval);
			goto eval;
eval_macro_expand_eval:
			push_cc(r1, r2, r0, cell_vm_eval_macro_expand_expand);
			goto macro_expand;
eval_macro_expand_expand:
			goto vm_return;
		}
		else
		{
			if(TYPE(r1) == TPAIR && (CAR(r1) == cell_symbol_define || CAR(r1) == cell_symbol_define_macro))
			{
				global_p = CAAR(r0) != cell_closure;
				macro_p = CAR(r1) == cell_symbol_define_macro;

				if(global_p)
				{
					name = CADR(r1);

					if(TYPE(CADR(r1)) == TPAIR)
					{
						name = CAR(name);
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

				if(TYPE(CADR(r1)) != TPAIR)
				{
					push_cc(CAR(CDDR(r1)), r2, cons(cons(CADR(r1), CADR(r1)), r0), cell_vm_eval_define);
					goto eval;
				}
				else
				{
					p = pairlis(CADR(r1), CADR(r1), r0);
					formals = CDR(CADR(r1));
					body = CDDR(r1);

					if(macro_p || global_p)
					{
						expand_variable(body, formals);
					}

					r1 = cons(cell_symbol_lambda, cons(formals, body));
					push_cc(r1, r2, p, cell_vm_eval_define);
					goto eval;
				}

eval_define:
				name = CADR(r2);

				if(TYPE(CADR(r2)) == TPAIR)
				{
					name = CAR(name);
				}

				if(macro_p)
				{
					entry = macro_get_handle(name);
					r1 = MAKE_MACRO(name, r1);
					set_cdr_x(entry, r1);
				}
				else if(global_p)
				{
					entry = GetSCM2(module_variable(r0, name), g_cells);
					set_cdr_x(entry, r1);
				}
				else
				{
					entry = cons(name, r1);
					aa = cons(entry, cell_nil);
					set_cdr_x(aa, cdr(r0));
					set_cdr_x(r0, aa);
					cl = GetSCM2(module_variable(r0, cell_closure), g_cells);
					set_cdr_x(cl, aa);
				}

				r1 = cell_unspecified;
				goto vm_return;
			}

			push_cc(CAR(r1), r1, r0, cell_vm_eval_check_func);
			gc_check();
			goto eval;
eval_check_func:
			push_cc(CDR(r2), r2, r0, cell_vm_eval2);
			goto evlis;
eval2:
			r1 = cons(CAR(r2), r1);
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
		r1 = CDR(VARIABLE(r1));
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
	if(TYPE(r1) != TPAIR || CAR(r1) == cell_symbol_quote)
	{
		goto vm_return;
	}

	if(CAR(r1) == cell_symbol_lambda)
	{
		push_cc(CDDR(r1), r1, r0, cell_vm_macro_expand_lambda);
		goto macro_expand;

macro_expand_lambda:
		CDDR(r2) = r1;
		r1 = r2;
		goto vm_return;
	}

	if(TYPE(r1) == TPAIR && (macro = get_macro(CAR(r1))) != cell_f)
	{
		r1 = cons(macro, CDR(r1));
		push_cc(r1, cell_nil, r0, cell_vm_macro_expand);
		goto apply;
	}

	if(CAR(r1) == cell_symbol_define || CAR(r1) == cell_symbol_define_macro)
	{
		push_cc(CDDR(r1), r1, r0, cell_vm_macro_expand_define);
		goto macro_expand;

macro_expand_define:
		CDDR(r2) = r1;
		r1 = r2;

		if(CAR(r1) == cell_symbol_define_macro)
		{
			push_cc(r1, r1, r0, cell_vm_macro_expand_define_macro);
			goto eval;

macro_expand_define_macro:
			r1 = r2;
		}

		goto vm_return;
	}

	if(CAR(r1) == cell_symbol_set_x)
	{
		push_cc(CDDR(r1), r1, r0, cell_vm_macro_expand_set_x);
		goto macro_expand;

macro_expand_set_x:
		CDDR(r2) = r1;
		r1 = r2;
		goto vm_return;
	}

	if(TYPE(r1) == TPAIR && TYPE(CAR(r1)) == TSYMBOL)
	{
		macro = macro_get_handle(cell_symbol_portable_macro_expand);
		expanders = GetSCM2(module_ref(r0, cell_symbol_sc_expander_alist), g_cells);
		if((CAR(r1) != cell_symbol_begin) && (macro != cell_f) && (expanders != cell_undefined))
		{
			macro = assq(CAR(r1), expanders);
			if(macro != cell_f)
			{
				sc_expand = GetSCM2(module_ref(r0, cell_symbol_macro_expand), g_cells);
				r2 = r1;

				if(sc_expand != cell_undefined && sc_expand != cell_f)
				{
					r1 = cons(sc_expand, cons(r1, cell_nil));
					goto apply;
				}
			}
		}
	}

	push_cc(CAR(r1), r1, r0, cell_vm_macro_expand_car);
	goto macro_expand;

macro_expand_car:
	CAR(r2) = r1;
	r1 = r2;

	if(CDR(r1) == cell_nil)
	{
		goto vm_return;
	}

	push_cc(CDR(r1), r1, r0, cell_vm_macro_expand_cdr);
	goto macro_expand;

macro_expand_cdr:
	CDR(r2) = r1;
	r1 = r2;
	goto vm_return;

begin:
	x = cell_unspecified;

	while(r1 != cell_nil)
	{
		gc_check();

		if(TYPE(r1) == TPAIR)
		{
			if(CAAR(r1) == cell_symbol_primitive_load)
			{
				program = cons(CAR(r1), cell_nil);
				push_cc(program, r1, r0, cell_vm_begin_primitive_load);
				goto begin_expand;
begin_primitive_load:
				CAR(r2) = r1;
				r1 = r2;
			}
		}

		if(TYPE(r1) == TPAIR && TYPE(CAR(r1)) == TPAIR)
		{
			if(CAAR(r1) == cell_symbol_begin)
			{
				r1 = append2(CDAR(r1), CDR(r1));
			}
		}

		if(CDR(r1) == cell_nil)
		{
			r1 = CAR(r1);
			goto eval;
		}

		push_cc(CAR(r1), r1, r0, cell_vm_begin_eval);
		goto eval;
begin_eval:
		x = r1;
		r1 = CDR(r2);
	}

	r1 = x;
	goto vm_return;
begin_expand:
	x = cell_unspecified;

	while(r1 != cell_nil)
	{
		gc_check();

		if(TYPE(r1) == TPAIR)
		{
			if(TYPE(CAR(r1)) == TPAIR && CAAR(r1) == cell_symbol_begin)
			{
				r1 = append2(CDAR(r1), CDR(r1));
			}

			if(CAAR(r1) == cell_symbol_primitive_load)
			{
				push_cc(CADR(CAR(r1)), r1, r0, cell_vm_begin_expand_primitive_load);
				goto eval; // FIXME: expand too?!
begin_expand_primitive_load:

				if(TYPE(r1) == TNUMBER && VALUE(r1) == 0)
					;
				else if(TYPE(r1) == TSTRING)
				{
					input = set_current_input_port(open_input_file(r1));
				}
				else if(TYPE(r1) == TPORT)
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
				r1 = cons(cell_symbol_begin, r1);
				CAR(r2) = r1;
				r1 = r2;
				continue;
			}
		}

		push_cc(CAR(r1), r1, r0, cell_vm_begin_expand_macro);
		goto macro_expand;
begin_expand_macro:

		if(r1 != CAR(r2))
		{
			CAR(r2) = r1;
			r1 = r2;
			continue;
		}

		r1 = r2;
		expand_variable(CAR(r1), cell_nil);
		push_cc(CAR(r1), r1, r0, cell_vm_begin_expand_eval);
		goto eval;
begin_expand_eval:
		x = r1;
		r1 = CDR(r2);
	}

	r1 = x;
	goto vm_return;
vm_if:
	push_cc(CAR(r1), r1, r0, cell_vm_if_expr);
	goto eval;
if_expr:
	x = r1;
	r1 = r2;

	if(x != cell_f)
	{
		r1 = CADR(r1);
		goto eval;
	}

	if(CDDR(r1) != cell_nil)
	{
		r1 = CAR(CDDR(r1));
		goto eval;
	}

	r1 = cell_unspecified;
	goto vm_return;
call_with_current_continuation:
	gc_push_frame();
	x = MAKE_CONTINUATION(g_continuations++);
	v = GetSCM2(make_vector__(STACK_SIZE - g_stack), g_cells);

	for(t = g_stack; t < STACK_SIZE; t++)
	{
		vector_set_x_(v, t - g_stack, (SCM) g_stack_array[t]);
	}

	CONTINUATION(x) = v;
	gc_pop_frame();
	push_cc(cons(CAR(r1), cons(x, cell_nil)), x, r0, cell_vm_call_with_current_continuation2);
	goto apply;
call_with_current_continuation2:
	v = GetSCM2(make_vector__(STACK_SIZE - g_stack), g_cells);

	for(t = g_stack; t < STACK_SIZE; t++)
	{
		vector_set_x_(v, t - g_stack, (SCM)g_stack_array[t]);
	}

	CONTINUATION(r2) = v;
	goto vm_return;
call_with_values:
	push_cc(cons(CAR(r1), cell_nil), r1, r0, cell_vm_call_with_values2);
	goto apply;
call_with_values2:

	if(TYPE(r1) == TVALUES)
	{
		r1 = CDR(r1);
	}

	r1 = cons(CADR(r2), r1);
	goto apply;
vm_return:
	x = r1;
	gc_pop_frame();
	r1 = x;
	goto eval_apply;
}
