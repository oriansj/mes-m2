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

/* struct scm* builtin_arity_(struct scm* builtin); */
/* struct scm* check_formals(struct scm* f, struct scm* formals, struct scm* args); */
char* itoa(int number);
int string_len(char* a);
struct scm* append2_(struct scm* x, struct scm* y);
struct scm* apply_builtin(struct scm* fn, struct scm* x);
struct scm* assert_defined(struct scm* x, struct scm* e);
struct scm* assq_(struct scm* x, struct scm* a);
struct scm* builtin_p_(struct scm* x);
struct scm* call_lambda(struct scm* e, struct scm* x);
struct scm* check_apply(struct scm* f, struct scm* e);
struct scm* cstring_to_symbol(char* s);
struct scm* display_(struct scm* x);
struct scm* error_(struct scm* key, struct scm* x);
struct scm* expand_variable(struct scm* x, struct scm* formals);
struct scm* gc();
struct scm* gc_check_();
struct scm* gc_pop_frame();
struct scm* get_macro(struct scm* name);
struct scm* hashq_get_handle(struct scm* table, struct scm* key, struct scm* dflt);
struct scm* macro_get_handle_(struct scm* name);
struct scm* macro_set_x(struct scm* name, struct scm* value);
struct scm* make_closure_(struct scm* args, struct scm* body, struct scm* a);
struct scm* make_hash_table_(struct scm* size);
struct scm* make_string(char* s, int length);
struct scm* make_tcontinuation(SCM a, SCM b);
struct scm* make_tmacro(struct scm* a, struct scm* b);
struct scm* make_tpair(struct scm* a, struct scm* b);
struct scm* make_vector__(SCM k);
struct scm* mes_builtins(struct scm* a);
struct scm* module_define_x_(struct scm* module, struct scm* name, struct scm* value);
struct scm* module_printer_(struct scm* module);
struct scm* module_ref_(struct scm* module, struct scm* name);
struct scm* module_variable_(struct scm* module, struct scm* name);
struct scm* open_input_file_(struct scm* file_name);
struct scm* pairlis_(struct scm* x, struct scm* y, struct scm* a);
struct scm* push_cc(struct scm* p1, struct scm* p2, struct scm* a, struct scm* c);
struct scm* read_input_file_env__();
struct scm* reverse_x_(struct scm* x, struct scm* t);
struct scm* set_current_input_port_(struct scm* port);
struct scm* set_env_x_(struct scm* x, struct scm* e, struct scm* a);
struct scm* vector_ref_(struct scm* x, SCM i);
struct scm* vector_set_x_(struct scm* x, SCM i, struct scm* e);
struct scm* write_error_(struct scm* x);
void gc_push_frame();
void require(int bool, char* error);

struct scm* eval_apply_()
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
eval_apply__:

	if(R3 == cell_vm_evlis2)
	{
		goto evlis2;
	}
	else if(R3 == cell_vm_evlis3)
	{
		goto evlis3;
	}
	else if(R3 == cell_vm_eval_check_func)
	{
		goto eval_check_func;
	}
	else if(R3 == cell_vm_eval2)
	{
		goto eval2;
	}
	else if(R3 == cell_vm_apply2)
	{
		goto apply2;
	}
	else if(R3 == cell_vm_if_expr)
	{
		goto if_expr;
	}
	else if(R3 == cell_vm_begin_eval)
	{
		goto begin_eval;
	}
	else if(R3 == cell_vm_eval_set_x)
	{
		goto eval_set_x;
	}
	else if(R3 == cell_vm_macro_expand_car)
	{
		goto macro_expand_car;
	}
	else if(R3 == cell_vm_return)
	{
		goto vm_return;
	}
	else if(R3 == cell_vm_macro_expand_cdr)
	{
		goto macro_expand_cdr;
	}
	else if(R3 == cell_vm_eval_define)
	{
		goto eval_define;
	}
	else if(R3 == cell_vm_macro_expand)
	{
		goto macro_expand;
	}
	else if(R3 == cell_vm_macro_expand_lambda)
	{
		goto macro_expand_lambda;
	}
	else if(R3 == cell_vm_eval_pmatch_car)
	{
		goto eval_pmatch_car;
	}
	else if(R3 == cell_vm_begin_expand_macro)
	{
		goto begin_expand_macro;
	}
	else if(R3 == cell_vm_macro_expand_define)
	{
		goto macro_expand_define;
	}
	else if(R3 == cell_vm_begin_expand_eval)
	{
		goto begin_expand_eval;
	}
	else if(R3 == cell_vm_call_with_current_continuation2)
	{
		goto call_with_current_continuation2;
	}
	else if(R3 == cell_vm_macro_expand_set_x)
	{
		goto macro_expand_set_x;
	}
	else if(R3 == cell_vm_eval_pmatch_cdr)
	{
		goto eval_pmatch_cdr;
	}
	else if(R3 == cell_vm_macro_expand_define_macro)
	{
		goto macro_expand_define_macro;
	}
	else if(R3 == cell_vm_begin_primitive_load)
	{
		goto begin_primitive_load;
	}
	else if(R3 == cell_vm_evlis)
	{
		goto evlis;
	}
	else if(R3 == cell_vm_apply)
	{
		goto apply;
	}
	else if(R3 == cell_vm_eval)
	{
		goto eval;
	}
	else if(R3 == cell_vm_eval_macro_expand_eval)
	{
		goto eval_macro_expand_eval;
	}
	else if(R3 == cell_vm_eval_macro_expand_expand)
	{
		goto eval_macro_expand_expand;
	}
	else if(R3 == cell_vm_begin)
	{
		goto begin;
	}
	else if(R3 == cell_vm_begin_expand)
	{
		goto begin_expand;
	}
	else if(R3 == cell_vm_begin_expand_primitive_load)
	{
		goto begin_expand_primitive_load;
	}
	else if(R3 == cell_vm_if)
	{
		goto vm_if;
	}
	else if(R3 == cell_vm_call_with_values2)
	{
		goto call_with_values2;
	}
	else if(R3 == cell_unspecified)
	{
		return R1;
	}
	else
	{
		error_(cell_symbol_system_error, make_string("eval/apply unknown continuation", string_len("eval/apply unknown continuation")));
	}

evlis:

	if(R1 == cell_nil)
	{
		goto vm_return;
	}

	if(R1->type != TPAIR)
	{
		goto eval;
	}

	push_cc(R1->car, R1, R0, cell_vm_evlis2);
	goto eval;
evlis2:
	push_cc(R2->cdr, R1, R0, cell_vm_evlis3);
	goto evlis;
evlis3:
	R1 = make_tpair(R2, R1);
	goto vm_return;
apply:
	g_stack_array[g_stack + FRAME_PROCEDURE] = R1->car;
	t = R1->car->type;

	if(t == TSTRUCT && builtin_p_(R1->car) == cell_t)
	{
		/* check_formals(R1->car, builtin_arity_(R1->car), R1->cdr); */
		R1 = apply_builtin(R1->car, R1->cdr);    /* FIXME: move into eval_apply */
		goto vm_return;
	}
	else if(t == TCLOSURE)
	{
		CL = R1->car->cdr;
		BODY = CL->cdr->cdr;
		FORMALS = CL->cdr->car;
		ARGS = R1->cdr;
		AA = CL->car->cdr;
		AA = AA->cdr;
		/* check_formals(R1->car, FORMALS, R1->cdr); */
		P = pairlis_(FORMALS, ARGS, AA);
		call_lambda(BODY, P);
		goto begin;
	}
	else if(t == TCONTINUATION)
	{
		V = R1->car->cdr;

		if(V->length)
		{
			for(t = 0; t < V->length; t = t + 1)
			{
				g_stack_array[STACK_SIZE - V->length + t] = vector_ref_(V, t);
			}

			g_stack = STACK_SIZE - V->length;
		}

		X = R1;
		gc_pop_frame();
		R1 = X->cdr->car;
		goto eval_apply__;
	}
	else if(t == TSPECIAL)
	{
		C = R1->car;

		if(C == cell_vm_apply)
		{
			push_cc(make_tpair(R1->cdr->car, R1->cdr->cdr->car), R1, R0, cell_vm_return);
			goto apply;
		}
		else if(C ==  cell_vm_eval)
		{
			push_cc(R1->cdr->car, R1, R1->cdr->cdr->car, cell_vm_return);
			goto eval;
		}
		else if(C ==  cell_vm_begin_expand)
		{
			push_cc(make_tpair(R1->cdr->car, cell_nil), R1, R1->cdr->cdr->car, cell_vm_return);
			goto begin_expand;
		}
		else if(C ==  cell_call_with_current_continuation)
		{
			R1 = R1->cdr;
			goto call_with_current_continuation;
		}
		else
		{
			check_apply(cell_f, R1->car);
		}
	}
	else if(t == TSYMBOL)
	{
		if(R1->car == cell_symbol_call_with_values)
		{
			R1 = R1->cdr;
			goto call_with_values;
		}

		if(R1->car == cell_symbol_current_module)
		{
			R1 = R0;
			goto vm_return;
		}

		if(R1->car == cell_symbol_boot_module)
		{
			R1 = M0;
			goto vm_return;
		}
	}
	else if(t == TPAIR)
	{
		if(R1->car->car == cell_symbol_lambda)
		{
			FORMALS = R1->car->cdr->car;
			ARGS = R1->cdr;
			BODY = R1->car->cdr->cdr;
			P = pairlis_(FORMALS, R1->cdr, R0);
			/* check_formals(R1, FORMALS, ARGS); */
			call_lambda(BODY, P);
			goto begin;
		}
	}

	/* write_error_(R1->car); */
	/* eputs("\n"); */
	push_cc(R1->car, R1, R0, cell_vm_apply2);
	goto eval;
apply2:
	check_apply(R1, R2->car);
	R1 = make_tpair(R1, R2->cdr);
	goto apply;
eval:
	t = R1->type;

	if(t == TPAIR)
	{
		C = R1->car;

		if(C ==  cell_symbol_pmatch_car)
		{
			push_cc(R1->cdr->car, R1, R0, cell_vm_eval_pmatch_car);
			goto eval;
eval_pmatch_car:
			X = R1;
			gc_pop_frame();
			R1 = X->car;
			goto eval_apply__;
		}
		else if(C ==  cell_symbol_pmatch_cdr)
		{
			push_cc(R1->cdr->car, R1, R0, cell_vm_eval_pmatch_cdr);
			goto eval;
eval_pmatch_cdr:
			X = R1;
			gc_pop_frame();
			R1 = X->cdr;
			goto eval_apply__;
		}
		else if(C ==  cell_symbol_quote)
		{
			X = R1;
			gc_pop_frame();
			R1 = X->cdr->car;
			goto eval_apply__;
		}
		else if(C ==  cell_symbol_begin)
		{
			goto begin;
		}
		else if(C ==  cell_symbol_lambda)
		{
			R1 = make_closure_(R1->cdr->car, R1->cdr->cdr, R0);
			goto vm_return;
		}
		else if(C ==  cell_symbol_if)
		{
			R1 = R1->cdr;
			goto vm_if;
		}
		else if(C ==  cell_symbol_set_x)
		{
			push_cc(R1->cdr->cdr->car, R1, R0, cell_vm_eval_set_x);
			goto eval;
eval_set_x:
			R1 = set_env_x_(R2->cdr->car, R1, R0);
			goto vm_return;
		}
		else if(C == cell_vm_macro_expand)
		{
			push_cc(R1->cdr->car, R1, R0, cell_vm_eval_macro_expand_eval);
			goto eval;
eval_macro_expand_eval:
			push_cc(R1, R2, R0, cell_vm_eval_macro_expand_expand);
			goto macro_expand;
eval_macro_expand_expand:
			goto vm_return;
		}
		else
		{
			if(R1->type == TPAIR && (R1->car == cell_symbol_define || R1->car == cell_symbol_define_macro))
			{
				global_p = R0->car->car != cell_closure;
				macro_p = R1->car == cell_symbol_define_macro;

				if(global_p)
				{
					NAME = R1->cdr->car;

					if(R1->cdr->car->type == TPAIR)
					{
						NAME = NAME->car;
					}

					if(macro_p)
					{
						ENTRY = assq_(NAME, g_macros);

						if(ENTRY == cell_f)
						{
							macro_set_x(NAME, cell_f);
						}
					}
					else
					{
						ENTRY = module_variable_(R0, NAME);

						if(ENTRY == cell_f)
						{
							module_define_x_(M0, NAME, cell_f);
						}
					}
				}

				R2 = R1;

				if(R1->cdr->car->type != TPAIR)
				{
					push_cc(R1->cdr->cdr->car, R2, make_tpair(make_tpair(R1->cdr->car, R1->cdr->car), R0), cell_vm_eval_define);
					goto eval;
				}
				else
				{
					P = pairlis_(R1->cdr->car, R1->cdr->car, R0);
					FORMALS = R1->cdr->car->cdr;
					BODY = R1->cdr->cdr;

					if(macro_p || global_p)
					{
						expand_variable(BODY, FORMALS);
					}

					R1 = make_tpair(cell_symbol_lambda, make_tpair(FORMALS, BODY));
					push_cc(R1, R2, P, cell_vm_eval_define);
					goto eval;
				}

eval_define:
				NAME = R2->cdr->car;

				if(R2->cdr->car->type == TPAIR)
				{
					NAME = NAME->car;
				}

				if(macro_p)
				{
					ENTRY = macro_get_handle_(NAME);
					R1 = make_tmacro(R1, NAME->cdr);
					ENTRY->cdr = R1;
				}
				else if(global_p)
				{
					ENTRY = module_variable_(R0, NAME);
					ENTRY->cdr = R1;
				}
				else
				{
					ENTRY = make_tpair(NAME, R1);
					AA = make_tpair(ENTRY, cell_nil);
					AA->cdr =  R0->cdr;
					R0->cdr = AA;
					CL = module_variable_(R0, cell_closure);
					CL->cdr = AA;
				}

				R1 = cell_unspecified;
				goto vm_return;
			}

			push_cc(R1->car, R1, R0, cell_vm_eval_check_func);
			gc_check_();
			goto eval;
eval_check_func:
			push_cc(R2->cdr, R2, R0, cell_vm_eval2);
			goto evlis;
eval2:
			R1 = make_tpair(R2->car, R1);
			goto apply;
		}
	}
	else if(t == TSYMBOL)
	{
		if(R1 == cell_symbol_boot_module)
		{
			goto vm_return;
		}

		if(R1 == cell_symbol_current_module)
		{
			goto vm_return;
		}

		if(R1 == cell_symbol_begin)  /* FIXME */
		{
			R1 = cell_begin;
			goto vm_return;
		}

		R1 = assert_defined(R1, module_ref_(R0, R1));
		goto vm_return;
	}
	else if(t == TVARIABLE)
	{
		R1 = R1->car->cdr;
		goto vm_return;
	}
	else if(t == TBROKEN_HEART)
	{
		error_(cell_symbol_system_error,  R1);
	}
	else
	{
		goto vm_return;
	}

macro_expand:
	if(R1->type != TPAIR || R1->car == cell_symbol_quote)
	{
		goto vm_return;
	}

	if(R1->car == cell_symbol_lambda)
	{
		push_cc(R1->cdr->cdr, R1, R0, cell_vm_macro_expand_lambda);
		goto macro_expand;

macro_expand_lambda:
		R2->cdr->cdr = R1;
		R1 = R2;
		goto vm_return;
	}

	if(R1->type == TPAIR && (MACRO = get_macro(R1->car)) != cell_f)
	{
		R1 = make_tpair(MACRO, R1->cdr);
		push_cc(R1, cell_nil, R0, cell_vm_macro_expand);
		goto apply;
	}

	if(R1->car == cell_symbol_define || R1->car == cell_symbol_define_macro)
	{
		push_cc(R1->cdr->cdr, R1, R0, cell_vm_macro_expand_define);
		goto macro_expand;

macro_expand_define:
		R2->cdr->cdr = R1;
		R1 = R2;

		if(R1->car == cell_symbol_define_macro)
		{
			push_cc(R1, R1, R0, cell_vm_macro_expand_define_macro);
			goto eval;

macro_expand_define_macro:
			R1 = R2;
		}

		goto vm_return;
	}

	if(R1->car == cell_symbol_set_x)
	{
		push_cc(R1->cdr->cdr, R1, R0, cell_vm_macro_expand_set_x);
		goto macro_expand;

macro_expand_set_x:
		R2->cdr->cdr = R1;
		R1 = R2;
		goto vm_return;
	}

	if(R1->type == TPAIR && R1->car->type == TSYMBOL)
	{
		MACRO = macro_get_handle_(cell_symbol_portable_macro_expand);
		EXPANDERS = module_ref_(R0, cell_symbol_sc_expander_alist);
		if((R1->car != cell_symbol_begin) && (MACRO != cell_f) && (EXPANDERS != cell_undefined))
		{
			MACRO = assq_(R1->car, EXPANDERS);
			if(MACRO != cell_f)
			{
				SC_EXPAND = module_ref_(R0, cell_symbol_macro_expand);
				R2 = R1;

				if(SC_EXPAND != cell_undefined && SC_EXPAND != cell_f)
				{
					R1 = make_tpair(SC_EXPAND, make_tpair(R1, cell_nil));
					goto apply;
				}
			}
		}
	}

	push_cc(R1->car, R1, R0, cell_vm_macro_expand_car);
	goto macro_expand;

macro_expand_car:
	R2->car = R1;
	R1 = R2;

	if(R1->cdr == cell_nil)
	{
		goto vm_return;
	}

	push_cc(R1->cdr, R1, R0, cell_vm_macro_expand_cdr);
	goto macro_expand;

macro_expand_cdr:
	R2->cdr = R1;
	R1 = R2;
	goto vm_return;

begin:
	X = cell_unspecified;

	while(R1 != cell_nil)
	{
		gc_check_();

		if(R1->type == TPAIR)
		{
			if(R1->car->car == cell_symbol_primitive_load)
			{
				PROGRAM = make_tpair(R1->car, cell_nil);
				push_cc(PROGRAM, R1, R0, cell_vm_begin_primitive_load);
				goto begin_expand;
begin_primitive_load:
				R2->car = R1;
				R1 = R2;
			}
		}

		if(R1->type == TPAIR && R1->car->type == TPAIR)
		{
			if(R1->car->car == cell_symbol_begin)
			{
				R1 = append2_(R1->car->cdr, R1->cdr);
			}
		}

		if(R1->cdr == cell_nil)
		{
			R1 = R1->car;
			goto eval;
		}

		push_cc(R1->car, R1, R0, cell_vm_begin_eval);
		goto eval;
begin_eval:
		X = R1;
		R1 = R2->cdr;
	}

	R1 = X;
	goto vm_return;
begin_expand:
	X = cell_unspecified;

	while(R1 != cell_nil)
	{
		gc_check_();

		if(R1->type == TPAIR)
		{
			if(R1->car->type == TPAIR && R1->car->car == cell_symbol_begin)
			{
				R1 = append2_(R1->car->cdr, R1->cdr);
			}

			if(R1->car->car == cell_symbol_primitive_load)
			{
				push_cc(R1->car->cdr->car, R1, R0, cell_vm_begin_expand_primitive_load);
				goto eval; /* FIXME: expand too?! */
begin_expand_primitive_load:

				if(R1->type == TNUMBER && R1->value == 0)
				{
					R1->value = 0; /* Not needed but haven't cleaned this block up yet */
				}
				else if(R1->type == TSTRING)
				{
					INPUT = set_current_input_port_(open_input_file_(R1));
				}
				else if(R1->type == TPORT)
				{
					INPUT = set_current_input_port_(R1);
				}
				else
				{
					require(FALSE, "Error in mes_eval.c: begin_expand_primitive_load");
				}

				push_cc(INPUT, R2, R0, cell_vm_return);
				X = read_input_file_env__();

				if(g_debug > 4)
				{
					module_printer_(M0);
				}

				gc_pop_frame();
				INPUT = R1;
				R1 = X;
				set_current_input_port_(INPUT);
				R1 = make_tpair(cell_symbol_begin, R1);
				R2->car = R1;
				R1 = R2;
				continue;
			}
		}

		push_cc(R1->car, R1, R0, cell_vm_begin_expand_macro);
		goto macro_expand;
begin_expand_macro:

		if(R1 != R2->car)
		{
			R2->car = R1;
			R1 = R2;
			continue;
		}

		R1 = R2;
		expand_variable(R1->car, cell_nil);
		push_cc(R1->car, R1, R0, cell_vm_begin_expand_eval);
		goto eval;
begin_expand_eval:
		X = R1;
		R1 = R2->cdr;
	}

	R1 = X;
	goto vm_return;
vm_if:
	push_cc(R1->car, R1, R0, cell_vm_if_expr);
	goto eval;
if_expr:
	X = R1;
	R1 = R2;

	if(X != cell_f)
	{
		R1 = R1->cdr->car;
		goto eval;
	}

	if(R1->cdr->cdr != cell_nil)
	{
		R1 = R1->cdr->cdr->car;
		goto eval;
	}

	R1 = cell_unspecified;
	goto vm_return;
call_with_current_continuation:
	gc_push_frame();
	X = make_tcontinuation(g_continuations, g_stack);
	g_continuations = g_continuations + 1;
	V = make_vector__(5);

	for(t = 0; t < 5; t = t + 1)
	{
		vector_set_x_(V, t, g_stack_array[g_stack + t]);
	}

	X->continuation = V;
	gc_pop_frame();
	push_cc(make_tpair(R1->car, make_tpair(X, cell_nil)), X, R0, cell_vm_call_with_current_continuation2);
	goto apply;
call_with_current_continuation2:
	V = make_vector__(5);

	for(t = 0; t < 5; t = t + 1)
	{
		vector_set_x_(V, t , g_stack_array[g_stack + t]);
	}

	R2->continuation = V;
	goto vm_return;
call_with_values:
	push_cc(make_tpair(R1->car, cell_nil), R1, R0, cell_vm_call_with_values2);
	goto apply;
call_with_values2:

	if(R1->type == TVALUES)
	{
		R1 = R1->cdr;
	}

	R1 = make_tpair(R2->cdr->car, R1);
	goto apply;
vm_return:
	X = R1;
	gc_pop_frame();
	R1 = X;
	goto eval_apply__;
}

struct scm* eval_apply(struct scm* x) /* External */
{
	require(cell_nil == x, "mes_eval.c: mes_eval recieved arguments with eval_apply\n");
	return eval_apply_();
}
