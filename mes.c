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
#include <fcntl.h>
int numerate_string(char *a);

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

SCM ARENA_SIZE;
SCM MAX_ARENA_SIZE;
SCM STACK_SIZE;

SCM JAM_SIZE;
SCM GC_SAFETY;

int MAX_STRING;
char *g_buf = 0;

char *g_arena = 0;
int g_debug;
SCM g_free = 0;

/* Imported Functions */
char *itoa (int number);
int fdputc (int c, int fd);
int fdputs (char const* s, int fd);
int eputs (char const* s);
struct scm* mes_builtins(struct scm* a);
struct scm* apply_builtin(SCM fn, SCM x);
struct scm* cstring_to_symbol(char const *s);
struct scm* make_hash_table_(SCM size);
struct scm* make_initial_module(SCM a);
struct scm* gc_check ();
SCM gc ();
struct scm* hashq_get_handle (SCM table, SCM key, SCM dflt);
struct scm* hashq_set_x (SCM table, SCM key, SCM value);
struct scm* hash_set_x (SCM table, SCM key, SCM value);
struct scm* display_ (SCM x);
struct scm* display_error_ (SCM x);
struct scm* write_error_ (SCM x);
SCM equal2_p (SCM a, SCM b);
SCM reverse_x_ (SCM x, SCM t);
SCM make_builtin (SCM builtin_type, SCM name, SCM arity, SCM function);
SCM builtin_arity (SCM builtin);
SCM builtin_p (SCM x);
struct scm* module_printer (SCM module);
struct scm* module_variable (SCM module, SCM name);
struct scm* module_ref (SCM module, SCM name);
struct scm* module_define_x (SCM module, SCM name, SCM value);
SCM open_input_file (SCM file_name);
SCM set_current_input_port (SCM port);
SCM read_input_file_env ();
struct scm* string_equal_p (SCM a, SCM b);
struct scm* symbol_to_string (SCM symbol);
struct scm* make_struct (SCM type, SCM fields, SCM printer);
SCM init_time(SCM a);

SCM alloc(SCM n)
{
	SCM x = g_free;
	g_free += n;

	if(g_free > ARENA_SIZE)
	{
		assert(!"alloc: out of memory");
	}

	return x;
}

SCM make_cell__(SCM type, SCM car, SCM cdr)
{
	SCM x = alloc(1);
	TYPE(x) = type;
	CAR(x) = car;
	CDR(x) = cdr;
	return x;
}

SCM make_cell_(SCM type, SCM car, SCM cdr)
{
	assert(TYPE(type) == TNUMBER);
	SCM t = VALUE(type);

	if(t == TCHAR || t == TNUMBER)
	{
		return make_cell__(t, car ? CAR(car) : 0, cdr ? CDR(cdr) : 0);
	}

	return make_cell__(t, car, cdr);
}

SCM assoc_string(SCM x, SCM a)  ///((internal))
{
	while(a != cell_nil && (TYPE(CAAR(a)) != TSTRING || GetSCM(string_equal_p(x, CAAR(a))) == cell_f))
	{
		a = CDR(a);
	}

	return a != cell_nil ? CAR(a) : cell_f;
}

SCM type_(SCM x)
{
	return MAKE_NUMBER(TYPE(x));
}

SCM car_(SCM x)
{
	return (TYPE(x) != TCONTINUATION
	        && (TYPE(CAR(x)) == TPAIR   // FIXME: this is weird
	            || TYPE(CAR(x)) == TREF
	            || TYPE(CAR(x)) == TSPECIAL
	            || TYPE(CAR(x)) == TSYMBOL
	            || TYPE(CAR(x)) == TSTRING)) ? CAR(x) : MAKE_NUMBER(CAR(x));
}

SCM cdr_(SCM x)
{
	return (TYPE(x) != TCHAR
	        && TYPE(x) != TNUMBER
	        && TYPE(x) != TPORT
	        && (TYPE(CDR(x)) == TPAIR
	            || TYPE(CDR(x)) == TREF
	            || TYPE(CDR(x)) == TSPECIAL
	            || TYPE(CDR(x)) == TSYMBOL
	            || TYPE(CDR(x)) == TSTRING)) ? CDR(x) : MAKE_NUMBER(CDR(x));
}

SCM cons(SCM x, SCM y)
{
	return make_cell__(TPAIR, x, y);
}

SCM car(SCM x)
{
	return CAR(x);
}

SCM cdr(SCM x)
{
	return CDR(x);
}

SCM list(SCM x)  ///((arity . n))
{
	return x;
}

SCM null_p(SCM x)
{
	return x == cell_nil ? cell_t : cell_f;
}

SCM eq_p(SCM x, SCM y)
{
	return (x == y
	        || ((TYPE(x) == TKEYWORD && TYPE(y) == TKEYWORD
	             && GetSCM(string_equal_p(x, y)) == cell_t))
	        || (TYPE(x) == TCHAR && TYPE(y) == TCHAR
	            && VALUE(x) == VALUE(y))
	        || (TYPE(x) == TNUMBER && TYPE(y) == TNUMBER
	            && VALUE(x) == VALUE(y)))
	       ? cell_t : cell_f;
}

SCM values(SCM x)  ///((arity . n))
{
	SCM v = cons(0, x);
	TYPE(v) = TVALUES;
	return v;
}

SCM acons(SCM key, SCM value, SCM alist)
{
	return cons(cons(key, value), alist);
}

SCM length__(SCM x)  ///((internal))
{
	SCM n = 0;

	while(x != cell_nil)
	{
		n++;

		if(TYPE(x) != TPAIR)
		{
			return -1;
		}

		x = CDR(x);
	}

	return n;
}

SCM length(SCM x)
{
	return MAKE_NUMBER(length__(x));
}

SCM apply(SCM, SCM);

SCM error(SCM key, SCM x)
{
	SCM throw= GetSCM(module_ref(r0, cell_symbol_throw));

	if(throw != cell_undefined)
	{
		return apply(throw, cons(key, cons(x, cell_nil)));
	}

	display_error_(key);
	eputs(": ");
	write_error_(x);
	eputs("\n");
	assert(0);
	exit(EXIT_FAILURE);
}

//  extra lib
SCM assert_defined(SCM x, SCM e)  ///((internal))
{
	if(e == cell_undefined)
	{
		return error(cell_symbol_unbound_variable, x);
	}

	return e;
}

struct scm* make_string(char const* s, int length);

SCM check_formals(SCM f, SCM formals, SCM args)  ///((internal))
{
	SCM flen = (TYPE(formals) == TNUMBER) ? VALUE(formals) : length__(formals);
	SCM alen = length__(args);

	if(alen != flen && alen != -1 && flen != -1)
	{
		char *s = "apply: wrong number of arguments; expected: ";
		eputs(s);
		eputs(itoa(flen));
		eputs(", got: ");
		eputs(itoa(alen));
		eputs("\n");
		write_error_(f);
		SCM e = GetSCM(MAKE_STRING0(s));
		return error(cell_symbol_wrong_number_of_args, cons(e, f));
	}

	return cell_unspecified;
}

SCM check_apply(SCM f, SCM e)  ///((internal))
{
	char* type = 0;

	if(f == cell_f || f == cell_t)
	{
		type = "bool";
	}

	if(f == cell_nil)
	{
		type = "nil";
	}

	if(f == cell_unspecified)
	{
		type = "*unspecified*";
	}

	if(f == cell_undefined)
	{
		type = "*undefined*";
	}

	if(TYPE(f) == TCHAR)
	{
		type = "char";
	}

	if(TYPE(f) == TNUMBER)
	{
		type = "number";
	}

	if(TYPE(f) == TSTRING)
	{
		type = "string";
	}

	if(TYPE(f) == TSTRUCT && builtin_p(f) == cell_f)
	{
		type = "#<...>";
	}

	if(TYPE(f) == TBROKEN_HEART)
	{
		type = "<3";
	}

	if(type)
	{
		char *s = "cannot apply: ";
		eputs(s);
		eputs(type);
		eputs("[");
		write_error_(e);
		eputs("]\n");
		SCM e = GetSCM(MAKE_STRING0(s));
		return error(cell_symbol_wrong_type_arg, cons(e, f));
	}

	return cell_unspecified;
}

SCM gc_push_frame()  ///((internal))
{
	if(g_stack < 5)
	{
		assert(!"STACK FULL");
	}

	g_stack_array[--g_stack] = cell_f;
	g_stack_array[--g_stack] = r0;
	g_stack_array[--g_stack] = r1;
	g_stack_array[--g_stack] = r2;
	g_stack_array[--g_stack] = r3;
	return g_stack;
}

SCM gc_peek_frame()  ///((internal))
{
	r3 = g_stack_array[g_stack];
	r2 = g_stack_array[g_stack + 1];
	r1 = g_stack_array[g_stack + 2];
	r0 = g_stack_array[g_stack + 3];
	return g_stack_array[g_stack + FRAME_PROCEDURE];
}

SCM gc_pop_frame()  ///((internal))
{
	SCM x = gc_peek_frame();
	g_stack += 5;
	return x;
}

SCM append2(SCM x, SCM y)
{
	if(x == cell_nil)
	{
		return y;
	}

	if(TYPE(x) != TPAIR)
	{
		error(cell_symbol_not_a_pair, cons(x, GetSCM2(cstring_to_symbol("append2"), g_cells)));
	}

	SCM r = cell_nil;

	while(x != cell_nil)
	{
		r = cons(CAR(x), r);
		x = CDR(x);
	}

	return reverse_x_(r, y);
}

SCM append_reverse(SCM x, SCM y)
{
	if(x == cell_nil)
	{
		return y;
	}

	if(TYPE(x) != TPAIR)
	{
		error(cell_symbol_not_a_pair, cons(x, GetSCM2(cstring_to_symbol("append-reverse"), g_cells)));
	}

	while(x != cell_nil)
	{
		y = cons(CAR(x), y);
		x = CDR(x);
	}

	return y;
}

SCM reverse_x_(SCM x, SCM t)
{
	if(x != cell_nil && TYPE(x) != TPAIR)
	{
		error(cell_symbol_not_a_pair, cons(x, GetSCM2(cstring_to_symbol("core:reverse!"), g_cells)));
	}

	SCM r = t;

	while(x != cell_nil)
	{
		t = CDR(x);
		CDR(x) = r;
		r = x;
		x = t;
	}

	return r;
}

SCM pairlis(SCM x, SCM y, SCM a)
{
	if(x == cell_nil)
	{
		return a;
	}

	if(TYPE(x) != TPAIR)
	{
		return cons(cons(x, y), a);
	}

	return cons(cons(car(x), car(y)), pairlis(cdr(x), cdr(y), a));
}

SCM
assq(SCM x, SCM a)
{
	if(TYPE(a) != TPAIR)
	{
		return cell_f;
	}

	int t = TYPE(x);

	if(t == TSYMBOL || t == TSPECIAL)
	{
		while(a != cell_nil && x != CAAR(a))
		{
			a = CDR(a);
		}
	}
	else if(t == TCHAR || t == TNUMBER)
	{
		SCM v = VALUE(x);

		while(a != cell_nil && v != VALUE(CAAR(a)))
		{
			a = CDR(a);
		}
	}
	else if(t == TKEYWORD)
	{
		while(a != cell_nil && GetSCM(string_equal_p(x, CAAR(a))) == cell_f)
		{
			a = CDR(a);
		}
	}
	else
	{
		/* pointer equality, e.g. on strings. */
		while(a != cell_nil && x != CAAR(a))
		{
			a = CDR(a);
		}
	}

	return a != cell_nil ? CAR(a) : cell_f;
}

SCM assoc(SCM x, SCM a)
{
	if(TYPE(x) == TSTRING)
	{
		return assoc_string(x, a);
	}

	while(a != cell_nil && equal2_p(x, CAAR(a)) == cell_f)
	{
		a = CDR(a);
	}

	return a != cell_nil ? CAR(a) : cell_f;
}

SCM set_car_x(SCM x, SCM e)
{
	if(TYPE(x) != TPAIR)
	{
		error(cell_symbol_not_a_pair, cons(x, GetSCM2(cstring_to_symbol("set-car!"), g_cells)));
	}

	CAR(x) = e;
	return cell_unspecified;
}

SCM set_cdr_x(SCM x, SCM e)
{
	if(TYPE(x) != TPAIR)
	{
		error(cell_symbol_not_a_pair, cons(x, GetSCM2(cstring_to_symbol("set-cdr!"), g_cells)));
	}

	CDR(x) = e;
	return cell_unspecified;
}

SCM set_env_x(SCM x, SCM e, SCM a)
{
	SCM p;

	if(TYPE(x) == TVARIABLE)
	{
		p = VARIABLE(x);
	}
	else
	{
		p = assert_defined(x, GetSCM(module_variable(a, x)));
	}

	if(TYPE(p) != TPAIR)
	{
		error(cell_symbol_not_a_pair, cons(p, x));
	}

	return set_cdr_x(p, e);
}

SCM call_lambda(SCM e, SCM x)  ///((internal))
{
	SCM cl = cons(cons(cell_closure, x), x);
	r1 = e;
	r0 = cl;
	return cell_unspecified;
}

SCM make_closure_(SCM args, SCM body, SCM a)  ///((internal))
{
	return make_cell__(TCLOSURE, cell_f, cons(cons(cell_circular, a), cons(args, body)));
}

SCM make_variable_(SCM var)  ///((internal))
{
	return make_cell__(TVARIABLE, var, 0);
}

SCM macro_get_handle(SCM name)
{
	if(TYPE(name) == TSYMBOL)
	{
		return GetSCM(hashq_get_handle(g_macros, name, cell_nil));
	}

	return cell_f;
}

SCM get_macro(SCM name)  ///((internal))
{
	SCM m = macro_get_handle(name);

	if(m != cell_f)
	{
		return MACRO(CDR(m));
	}

	return cell_f;
}

SCM macro_set_x(SCM name, SCM value)  ///((internal))
{
	return GetSCM(hashq_set_x(g_macros, name, value));
}

SCM push_cc(SCM p1, SCM p2, SCM a, SCM c)  ///((internal))
{
	SCM x = r3;
	r3 = c;
	r2 = p2;
	gc_push_frame();
	r1 = p1;
	r0 = a;
	r3 = x;
	return cell_unspecified;
}

SCM add_formals(SCM formals, SCM x)
{
	while(TYPE(x) == TPAIR)
	{
		formals = cons(CAR(x), formals);
		x = CDR(x);
	}

	if(TYPE(x) == TSYMBOL)
	{
		formals = cons(x, formals);
	}

	return formals;
}

int formal_p(SCM x, SCM formals)  /// ((internal))
{
	if(TYPE(formals) == TSYMBOL)
	{
		if(x == formals)
		{
			return x;
		}
		else
		{
			return cell_f;
		}
	}

	while(TYPE(formals) == TPAIR && CAR(formals) != x)
	{
		formals = CDR(formals);
	}

	if(TYPE(formals) == TSYMBOL)
	{
		return formals == x;
	}

	return TYPE(formals) == TPAIR;
}

SCM expand_variable_(SCM x, SCM formals, int top_p)  ///((internal))
{
	while(TYPE(x) == TPAIR)
	{
		if(TYPE(CAR(x)) == TPAIR)
		{
			if(CAAR(x) == cell_symbol_lambda)
			{
				SCM f = CAR(CDAR(x));
				formals = add_formals(formals, f);
			}
			else if(CAAR(x) == cell_symbol_define || CAAR(x) == cell_symbol_define_macro)
			{
				SCM f = CAR(CDAR(x));
				formals = add_formals(formals, f);
			}

			if(CAAR(x) != cell_symbol_quote)
			{
				expand_variable_(CAR(x), formals, 0);
			}
		}
		else
		{
			if(CAR(x) == cell_symbol_lambda)
			{
				SCM f = CADR(x);
				formals = add_formals(formals, f);
				x = CDR(x);
			}
			else if(CAR(x) == cell_symbol_define || CAR(x) == cell_symbol_define_macro)
			{
				SCM f = CADR(x);

				if(top_p && TYPE(f) == TPAIR)
				{
					f = CDR(f);
				}

				formals = add_formals(formals, f);
				x = CDR(x);
			}
			else if(CAR(x) == cell_symbol_quote)
			{
				return cell_unspecified;
			}
			else if(TYPE(CAR(x)) == TSYMBOL
			        && CAR(x) != cell_symbol_boot_module
			        && CAR(x) != cell_symbol_current_module
			        && CAR(x) != cell_symbol_primitive_load
			        && !formal_p(CAR(x), formals))
			{
				SCM v = GetSCM(module_variable(r0, CAR(x)));

				if(v != cell_f)
				{
					CAR(x) = make_variable_(v);
				}
			}
		}

		x = CDR(x);
		top_p = 0;
	}

	return cell_unspecified;
}

SCM expand_variable(SCM x, SCM formals)  ///((internal))
{
	return expand_variable_(x, formals, 1);
}

struct scm* struct_ref_(SCM x, SCM i);
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
		error(cell_symbol_system_error, GetSCM(MAKE_STRING0("eval/apply unknown continuation")));
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
	g_stack_array[g_stack + FRAME_PROCEDURE] = CAR(r1);
	t = TYPE(CAR(r1));

	if(t == TSTRUCT && builtin_p(CAR(r1)) == cell_t)
	{
		check_formals(CAR(r1), builtin_arity(CAR(r1)), CDR(r1));
		r1 = GetSCM(apply_builtin(CAR(r1), CDR(r1)));    /// FIXME: move into eval_apply
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
				g_stack_array[STACK_SIZE - LENGTH(v) + t] = GetSCM(good2bad(vector_ref_(v, t), g_cells));
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
						entry = GetSCM(module_variable(r0, name));

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
					entry = GetSCM(module_variable(r0, name));
					set_cdr_x(entry, r1);
				}
				else
				{
					entry = cons(name, r1);
					aa = cons(entry, cell_nil);
					set_cdr_x(aa, cdr(r0));
					set_cdr_x(r0, aa);
					cl = GetSCM(module_variable(r0, cell_closure));
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

		r1 = assert_defined(r1, GetSCM(module_ref(r0, r1)));
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
		expanders = GetSCM(module_ref(r0, cell_symbol_sc_expander_alist));
		if((CAR(r1) != cell_symbol_begin) && (macro != cell_f) && (expanders != cell_undefined))
		{
			macro = assq(CAR(r1), expanders);
			if(macro != cell_f)
			{
				sc_expand = GetSCM(module_ref(r0, cell_symbol_macro_expand));
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
	v = GetSCM(good2bad(make_vector__(STACK_SIZE - g_stack), g_cells));

	for(t = g_stack; t < STACK_SIZE; t++)
	{
		vector_set_x_(v, t - g_stack, g_stack_array[t]);
	}

	CONTINUATION(x) = v;
	gc_pop_frame();
	push_cc(cons(CAR(r1), cons(x, cell_nil)), x, r0, cell_vm_call_with_current_continuation2);
	goto apply;
call_with_current_continuation2:
	v = GetSCM(good2bad(make_vector__(STACK_SIZE - g_stack), g_cells));

	for(t = g_stack; t < STACK_SIZE; t++)
	{
		vector_set_x_(v, t - g_stack, g_stack_array[t]);
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

SCM apply(SCM f, SCM x)  ///((internal))
{
	push_cc(cons(f, x), cell_unspecified, r0, cell_unspecified);
	r3 = cell_vm_apply;
	return eval_apply();
}

// Jam Collector
SCM g_symbol_max;

int open_boot(char *prefix, char const *boot, char const *location);
void read_boot()  ///((internal))
{
	__stdin = -1;
	char prefix[1024];
	char boot[1024];

	if(getenv("MES_BOOT"))
	{
		strcpy(boot, getenv("MES_BOOT"));
	}
	else
	{
		strcpy(boot, "boot-0.scm");
	}

	if(getenv("MES_PREFIX"))
	{
		strcpy(prefix, getenv("MES_PREFIX"));
		strcpy(prefix + strlen(prefix), "/module");
		strcpy(prefix + strlen(prefix), "/mes/");
		__stdin = open_boot(prefix, boot, "MES_PREFIX");
	}

	if(__stdin < 0)
	{
		char const *p = "module/mes/";
		strcpy(prefix, p);
		__stdin = open_boot(prefix, boot, "module");
	}

	if(__stdin < 0)
	{
		strcpy(prefix, "mes/module/mes/");
		__stdin = open_boot(prefix, boot, ".");
	}

	if(__stdin < 0)
	{
		prefix[0] = 0;
		__stdin = open_boot(prefix, boot, "<boot>");
	}

	if(__stdin < 0)
	{
		eputs("mes: boot failed: no such file: ");
		eputs(boot);
		eputs("\n");
		exit(EXIT_FAILURE);
	}

	r2 = read_input_file_env();
	__stdin = STDIN;
}

int get_env_value(char* c, int alt)
{
	char* s = getenv(c);
	if(NULL == s) return alt;
	return numerate_string(s);
}

SCM mes_environment(int argc, char *argv[]);
int main(int argc, char *argv[])
{
	__ungetc_buf = calloc((RLIMIT_NOFILE + 1), sizeof(int));
	g_continuations = 0;
	g_symbols = 0;
	g_stack = 0;
	r0 = 0;
	r1 = 0;
	r2 = 0;
	r3 = 0;
	m0 = 0;
	g_macros = 0;
	g_ports = 1;
	g_cells = 0;
	g_news = 0;
	__stdin = STDIN;
	__stdout = STDOUT;
	__stderr = STDERR;

	g_debug = get_env_value("MES_DEBUG", 0);

	if(g_debug > 1)
	{
		eputs(";;; MODULEDIR=");
		eputs("module");
		eputs("\n");
	}

	MAX_ARENA_SIZE = get_env_value("MES_MAX_ARENA", 100000000);
	ARENA_SIZE = get_env_value("MES_ARENA", 10000000);
	JAM_SIZE = ARENA_SIZE / 10;
	JAM_SIZE = get_env_value("MES_JAM", 20000);
	GC_SAFETY = ARENA_SIZE / 100;
	GC_SAFETY = get_env_value("MES_SAFETY", 2000);
	STACK_SIZE = get_env_value("MES_STACK", 20000);
	MAX_STRING = get_env_value("MES_MAX_STRING", 524288);

	SCM a = mes_environment(argc, argv);
	a = GetSCM2(mes_builtins(Getstructscm2(a, g_cells)), g_cells);
	a = init_time(a);
	m0 = GetSCM(make_initial_module(a));
	g_macros = GetSCM(make_hash_table_(0));

	if(g_debug > 4)
	{
		module_printer(m0);
	}

	read_boot();
	push_cc(r2, cell_unspecified, r0, cell_unspecified);

	if(g_debug > 2)
	{
		eputs("\ngc stats: [");
		eputs(itoa(g_free));
		eputs("]\n");
	}

	if(g_debug > 3)
	{
		eputs("program: ");
		write_error_(r1);
		eputs("\n");
	}

	r3 = cell_vm_begin_expand;
	r1 = eval_apply();

	if(g_debug)
	{
		write_error_(r1);
		eputs("\n");
	}

	if(g_debug)
	{
		if(g_debug > 4)
		{
			module_printer(m0);
		}

		eputs("\ngc stats: [");
		eputs(itoa(g_free));
		MAX_ARENA_SIZE = 0;
		gc(g_stack);
		eputs(" => ");
		eputs(itoa(g_free));
		eputs("]\n");

		if(g_debug > 4)
		{
			module_printer(m0);
		}

		eputs("\n");
		gc(g_stack);
		eputs(" => ");
		eputs(itoa(g_free));
		eputs("]\n");

		if(g_debug > 4)
		{
			module_printer(m0);
		}

		eputs("\n");
		gc(g_stack);
		eputs(" => ");
		eputs(itoa(g_free));
		eputs("]\n");

		if(g_debug > 4)
		{
			module_printer(m0);
		}

		if(g_debug > 3)
		{
			eputs("ports:");
			write_error_(g_ports);
			eputs("\n");
		}

		eputs("\n");
	}

	return 0;
}
