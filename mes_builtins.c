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

/* Imported Functions */
// src/gc.mes
struct scm* gc_check ();
struct scm* gc ();
// src/hash.mes
struct scm* hashq (SCM x, SCM size);
struct scm* hash (SCM x, SCM size);
struct scm* hashq_get_handle (SCM table, SCM key, SCM dflt);
struct scm* hashq_ref (SCM table, SCM key, SCM dflt);
struct scm* hash_ref (SCM table, SCM key, SCM dflt);
struct scm* hashq_set_x (SCM table, SCM key, SCM value);
struct scm* hash_set_x (SCM table, SCM key, SCM value);
struct scm* hash_table_printer (struct scm* table);
struct scm* make_hash_table_(long size);
struct scm* make_hash_table (SCM x);
// src/lib.mes
struct scm* display_ (SCM x);
struct scm* display_error_ (SCM x);
struct scm* display_port_ (SCM x, SCM p);
struct scm* write_ (SCM x);
struct scm* write_error_ (SCM x);
struct scm* write_port_ (SCM x, SCM p);
struct scm* exit_ (SCM x);
struct scm* frame_printer (SCM frame);
struct scm* make_stack ();
struct scm* stack_length (SCM stack);
struct scm* stack_ref (SCM stack, SCM index);
struct scm* xassq (SCM x, SCM a);
struct scm* memq (SCM x, SCM a);
struct scm* equal2_p (SCM a, SCM b);
struct scm* last_pair (SCM x);
struct scm* pair_p (SCM x);
// src/math.mes
struct scm* greater_p (SCM x);
struct scm* less_p (SCM x);
struct scm* is_p (SCM x);
struct scm* minus (SCM x);
struct scm* plus (SCM x);
struct scm* divide (SCM x);
struct scm* modulo (SCM a, SCM b);
struct scm* multiply (SCM x);
struct scm* logand (SCM x);
struct scm* logior (SCM x);
struct scm* lognot (SCM x);
struct scm* logxor (SCM x);
struct scm* ash (SCM n, SCM count);
// src/mes.mes
SCM acons (SCM key, SCM value, SCM alist);
SCM add_formals (SCM formals, SCM x);
SCM append2 (SCM x, SCM y);
SCM append_reverse (SCM x, SCM y);
SCM arity_ (SCM x);
SCM assoc (SCM x, SCM a);
SCM assq (SCM x, SCM a);
SCM call (SCM fn, SCM x);
SCM car (SCM x);
SCM car_ (SCM x);
SCM cdr (SCM x);
SCM cdr_ (SCM x);
SCM cons (SCM x, SCM y);
struct scm* cons2(struct scm* x, struct scm* y);
SCM cons3(struct scm* x, struct scm* y);
SCM eq_p (SCM x, SCM y);
SCM error (SCM key, SCM x);
SCM eval_apply ();
SCM length (SCM x);
SCM list (SCM x);
SCM macro_get_handle (SCM name);
SCM make_cell_ (SCM type, SCM car, SCM cdr);
SCM make_cell__(long type, SCM car, SCM cdr);
SCM null_p (SCM x);
SCM pairlis (SCM x, SCM y, SCM a);
SCM reverse_x_ (SCM x, SCM t);
SCM set_car_x (SCM x, SCM e);
SCM set_cdr_x (SCM x, SCM e);
SCM set_env_x (SCM x, SCM e, SCM a);
SCM type_ (SCM x);
SCM values (SCM x);
struct scm* builtin_printer(SCM builtin);

// src/module.mes
struct scm* make_module_type ();
struct scm* module_printer (SCM module);
struct scm* module_variable_ (SCM module, SCM name);
struct scm* module_ref (SCM module, SCM name);
struct scm* module_define_x (SCM module, SCM name, SCM value);
// src/posix.mes
struct scm* peek_byte ();
struct scm* read_byte ();
struct scm* unread_byte (SCM i);
struct scm* peek_char ();
struct scm* read_char (SCM port);
struct scm* unread_char (SCM i);
struct scm* write_char (SCM i);
struct scm* write_byte (SCM x);
struct scm* getenv_ (SCM s);
struct scm* setenv_ (SCM s, SCM v);
struct scm* access_p (SCM file_name, SCM mode);
struct scm* current_input_port ();
struct scm* open_input_file (SCM file_name);
struct scm* open_input_string (SCM string);
struct scm* set_current_input_port (SCM port);
struct scm* current_output_port ();
struct scm* current_error_port ();
struct scm* open_output_file (SCM x);
struct scm* set_current_output_port (SCM port);
struct scm* set_current_error_port (SCM port);
struct scm* chmod_ (SCM file_name, SCM mode);
struct scm* isatty_p (SCM port);
struct scm* primitive_fork ();
struct scm* execl_ (SCM file_name, SCM args);
struct scm* waitpid_ (SCM pid, SCM options);
struct scm* current_time ();
struct scm* gettimeofday_ ();
struct scm* get_internal_run_time ();
struct scm* getcwd_ ();
struct scm* dup_ (SCM port);
struct scm* dup2_ (SCM old, SCM new);
struct scm* delete_file (SCM file_name);
// src/reader.mes
struct scm* read_input_file_env_ (SCM e, SCM a);
struct scm* read_input_file_env ();
struct scm* read_env (SCM a);
struct scm* reader_read_sexp (SCM c, SCM a);
struct scm* reader_read_character ();
struct scm* reader_read_binary ();
struct scm* reader_read_octal ();
struct scm* reader_read_hex ();
struct scm* reader_read_string ();
// src/strings.mes
struct scm* string_equal_p (SCM a, SCM b);
struct scm* symbol_to_string (SCM symbol);
struct scm* symbol_to_keyword (SCM symbol);
struct scm* keyword_to_string (SCM keyword);
struct scm* string_to_symbol (SCM string);
struct scm* make_symbol (SCM string);
struct scm* string_to_list (SCM string);
struct scm* list_to_string (SCM list);
struct scm* read_string (SCM port);
struct scm* string_append (SCM x);
struct scm* string_length (SCM string);
struct scm* string_ref (SCM str, SCM k);
// src/struct.mes
struct scm* make_struct_(SCM type, struct scm* fields, SCM printer);
struct scm* struct_length (SCM x);
struct scm* struct_ref (SCM x, SCM i);
struct scm* struct_set_x (SCM x, SCM i, SCM e);
// src/vector.mes
struct scm* make_vector_ (SCM n);
struct scm* vector_length (SCM x);
struct scm* vector_ref (SCM x, SCM i);
struct scm* vector_entry (SCM x);
struct scm* vector_set_x (SCM x, SCM i, SCM e);
struct scm* list_to_vector (SCM x);
struct scm* vector_to_list (SCM v);
SCM init_time(SCM a);

/* Internal functions required*/
struct scm* make_string(char const* s, int length);
struct scm* make_struct (SCM type, SCM fields, SCM printer);
struct scm* make_string_(char const* s);
struct scm* cstring_to_symbol(char const *s);
struct scm* struct_ref_(SCM x, SCM i);
int fdputc (int c, int fd);
int fdputs (char const* s, int fd);
int eputs (char const* s);
void gc_init_cells();

void init_symbol(SCM x, SCM type, char const* name)
{
	g_cells[x].type = type;
	int length = strlen(name);
	SCM string = GetSCM2(bad2good(make_string(name, length), g_cells), g_cells);
	g_cells[x].length = length;
	g_cells[x].string = g_cells[string].string;
	hash_set_x(g_symbols, string, x);
}

SCM mes_symbols()  ///((internal))
{
	gc_init_cells();
	g_free = cell_symbol_test + 1;
	g_symbol_max = g_free;
	g_symbols = GetSCM2(bad2good(make_hash_table_(500), g_cells), g_cells);
	init_symbol(cell_nil, TSPECIAL, "()");
	init_symbol(cell_f, TSPECIAL, "#f");
	init_symbol(cell_t, TSPECIAL, "#t");
	init_symbol(cell_dot, TSPECIAL, ".");
	init_symbol(cell_arrow, TSPECIAL, "=>");
	init_symbol(cell_undefined, TSPECIAL, "*undefined*");
	init_symbol(cell_unspecified, TSPECIAL, "*unspecified*");
	init_symbol(cell_closure, TSPECIAL, "*closure*");
	init_symbol(cell_circular, TSPECIAL, "*circular*");
	init_symbol(cell_begin, TSPECIAL, "*begin*");
	init_symbol(cell_call_with_current_continuation, TSPECIAL, "*call/cc*");
	init_symbol(cell_vm_apply, TSPECIAL, "core:apply");
	init_symbol(cell_vm_apply2, TSPECIAL, "*vm-apply2*");
	init_symbol(cell_vm_begin, TSPECIAL, "*vm-begin*");
	init_symbol(cell_vm_begin_eval, TSPECIAL, "*vm:begin-eval*");
	init_symbol(cell_vm_begin_expand, TSPECIAL, "core:eval");
	init_symbol(cell_vm_begin_expand_eval, TSPECIAL, "*vm:begin-expand-eval*");
	init_symbol(cell_vm_begin_expand_macro, TSPECIAL, "*vm:begin-expand-macro*");
	init_symbol(cell_vm_begin_expand_primitive_load, TSPECIAL, "*vm:core:begin-expand-primitive-load*");
	init_symbol(cell_vm_begin_primitive_load, TSPECIAL, "*vm:core:begin-primitive-load*");
	init_symbol(cell_vm_begin_read_input_file, TSPECIAL, "*vm-begin-read-input-file*");
	init_symbol(cell_vm_call_with_current_continuation2, TSPECIAL, "*vm-call-with-current-continuation2*");
	init_symbol(cell_vm_call_with_values2, TSPECIAL, "*vm-call-with-values2*");
	init_symbol(cell_vm_eval, TSPECIAL, "core:eval-expanded");
	init_symbol(cell_vm_eval2, TSPECIAL, "*vm-eval2*");
	init_symbol(cell_vm_eval_check_func, TSPECIAL, "*vm-eval-check-func*");
	init_symbol(cell_vm_eval_define, TSPECIAL, "*vm-eval-define*");
	init_symbol(cell_vm_eval_macro_expand_eval, TSPECIAL, "*vm:eval-macro-expand-eval*");
	init_symbol(cell_vm_eval_macro_expand_expand, TSPECIAL, "*vm:eval-macro-expand-expand*");
	init_symbol(cell_vm_eval_pmatch_car, TSPECIAL, "*vm-eval-pmatch-car*");
	init_symbol(cell_vm_eval_pmatch_cdr, TSPECIAL, "*vm-eval-pmatch-cdr*");
	init_symbol(cell_vm_eval_set_x, TSPECIAL, "*vm-eval-set!*");
	init_symbol(cell_vm_evlis, TSPECIAL, "*vm-evlis*");
	init_symbol(cell_vm_evlis2, TSPECIAL, "*vm-evlis2*");
	init_symbol(cell_vm_evlis3, TSPECIAL, "*vm-evlis3*");
	init_symbol(cell_vm_if, TSPECIAL, "*vm-if*");
	init_symbol(cell_vm_if_expr, TSPECIAL, "*vm-if-expr*");
	init_symbol(cell_vm_macro_expand, TSPECIAL, "core:macro-expand");
	init_symbol(cell_vm_macro_expand_car, TSPECIAL, "*vm:core:macro-expand-car*");
	init_symbol(cell_vm_macro_expand_cdr, TSPECIAL, "*vm:macro-expand-cdr*");
	init_symbol(cell_vm_macro_expand_define, TSPECIAL, "*vm:core:macro-expand-define*");
	init_symbol(cell_vm_macro_expand_define_macro, TSPECIAL, "*vm:core:macro-expand-define-macro*");
	init_symbol(cell_vm_macro_expand_lambda, TSPECIAL, "*vm:core:macro-expand-lambda*");
	init_symbol(cell_vm_macro_expand_set_x, TSPECIAL, "*vm:core:macro-expand-set!*");
	init_symbol(cell_vm_return, TSPECIAL, "*vm-return*");
	init_symbol(cell_symbol_dot, TSYMBOL, "*dot*");
	init_symbol(cell_symbol_lambda, TSYMBOL, "lambda");
	init_symbol(cell_symbol_begin, TSYMBOL, "begin");
	init_symbol(cell_symbol_if, TSYMBOL, "if");
	init_symbol(cell_symbol_quote, TSYMBOL, "quote");
	init_symbol(cell_symbol_define, TSYMBOL, "define");
	init_symbol(cell_symbol_define_macro, TSYMBOL, "define-macro");
	init_symbol(cell_symbol_quasiquote, TSYMBOL, "quasiquote");
	init_symbol(cell_symbol_unquote, TSYMBOL, "unquote");
	init_symbol(cell_symbol_unquote_splicing, TSYMBOL, "unquote-splicing");
	init_symbol(cell_symbol_syntax, TSYMBOL, "syntax");
	init_symbol(cell_symbol_quasisyntax, TSYMBOL, "quasisyntax");
	init_symbol(cell_symbol_unsyntax, TSYMBOL, "unsyntax");
	init_symbol(cell_symbol_unsyntax_splicing, TSYMBOL, "unsyntax-splicing");
	init_symbol(cell_symbol_set_x, TSYMBOL, "set!");
	init_symbol(cell_symbol_sc_expand, TSYMBOL, "sc-expand");
	init_symbol(cell_symbol_macro_expand, TSYMBOL, "macro-expand");
	init_symbol(cell_symbol_portable_macro_expand, TSYMBOL, "portable-macro-expand");
	init_symbol(cell_symbol_sc_expander_alist, TSYMBOL, "*sc-expander-alist*");
	init_symbol(cell_symbol_call_with_values, TSYMBOL, "call-with-values");
	init_symbol(cell_symbol_call_with_current_continuation, TSYMBOL, "call-with-current-continuation");
	init_symbol(cell_symbol_boot_module, TSYMBOL, "boot-module");
	init_symbol(cell_symbol_current_module, TSYMBOL, "current-module");
	init_symbol(cell_symbol_primitive_load, TSYMBOL, "primitive-load");
	init_symbol(cell_symbol_read_input_file, TSYMBOL, "read-input-file");
	init_symbol(cell_symbol_write, TSYMBOL, "write");
	init_symbol(cell_symbol_display, TSYMBOL, "display");
	init_symbol(cell_symbol_car, TSYMBOL, "car");
	init_symbol(cell_symbol_cdr, TSYMBOL, "cdr");
	init_symbol(cell_symbol_not_a_number, TSYMBOL, "not-a-number");
	init_symbol(cell_symbol_not_a_pair, TSYMBOL, "not-a-pair");
	init_symbol(cell_symbol_system_error, TSYMBOL, "system-error");
	init_symbol(cell_symbol_throw, TSYMBOL, "throw");
	init_symbol(cell_symbol_unbound_variable, TSYMBOL, "unbound-variable");
	init_symbol(cell_symbol_wrong_number_of_args, TSYMBOL, "wrong-number-of-args");
	init_symbol(cell_symbol_wrong_type_arg, TSYMBOL, "wrong-type-arg");
	init_symbol(cell_symbol_buckets, TSYMBOL, "buckets");
	init_symbol(cell_symbol_builtin, TSYMBOL, "<builtin>");
	init_symbol(cell_symbol_frame, TSYMBOL, "<frame>");
	init_symbol(cell_symbol_hashq_table, TSYMBOL, "<hashq-table>");
	init_symbol(cell_symbol_module, TSYMBOL, "<module>");
	init_symbol(cell_symbol_procedure, TSYMBOL, "procedure");
	init_symbol(cell_symbol_record_type, TSYMBOL, "<record-type>");
	init_symbol(cell_symbol_size, TSYMBOL, "size");
	init_symbol(cell_symbol_stack, TSYMBOL, "<stack>");
	init_symbol(cell_symbol_argv, TSYMBOL, "%argv");
	init_symbol(cell_symbol_mes_prefix, TSYMBOL, "%prefix");
	init_symbol(cell_symbol_mes_version, TSYMBOL, "%version");
	init_symbol(cell_symbol_internal_time_units_per_second, TSYMBOL, "internal-time-units-per-second");
	init_symbol(cell_symbol_compiler, TSYMBOL, "%compiler");
	init_symbol(cell_symbol_arch, TSYMBOL, "%arch");
	init_symbol(cell_symbol_pmatch_car, TSYMBOL, "pmatch-car");
	init_symbol(cell_symbol_pmatch_cdr, TSYMBOL, "pmatch-cdr");
	init_symbol(cell_type_bytes, TSYMBOL, "<cell:bytes>");
	init_symbol(cell_type_char, TSYMBOL, "<cell:char>");
	init_symbol(cell_type_closure, TSYMBOL, "<cell:closure>");
	init_symbol(cell_type_continuation, TSYMBOL, "<cell:continuation>");
	init_symbol(cell_type_function, TSYMBOL, "<cell:function>");
	init_symbol(cell_type_keyword, TSYMBOL, "<cell:keyword>");
	init_symbol(cell_type_macro, TSYMBOL, "<cell:macro>");
	init_symbol(cell_type_number, TSYMBOL, "<cell:number>");
	init_symbol(cell_type_pair, TSYMBOL, "<cell:pair>");
	init_symbol(cell_type_port, TSYMBOL, "<cell:port>");
	init_symbol(cell_type_ref, TSYMBOL, "<cell:ref>");
	init_symbol(cell_type_special, TSYMBOL, "<cell:special>");
	init_symbol(cell_type_string, TSYMBOL, "<cell:string>");
	init_symbol(cell_type_struct, TSYMBOL, "<cell:struct>");
	init_symbol(cell_type_symbol, TSYMBOL, "<cell:symbol>");
	init_symbol(cell_type_values, TSYMBOL, "<cell:values>");
	init_symbol(cell_type_variable, TSYMBOL, "<cell:variable>");
	init_symbol(cell_type_vector, TSYMBOL, "<cell:vector>");
	init_symbol(cell_type_broken_heart, TSYMBOL, "<cell:broken-heart>");
	init_symbol(cell_symbol_test, TSYMBOL, "%%test");
	SCM a = cell_nil;
	a = acons(cell_symbol_call_with_values, cell_symbol_call_with_values, a);
	a = acons(cell_symbol_boot_module, cell_symbol_boot_module, a);
	a = acons(cell_symbol_current_module, cell_symbol_current_module, a);
	a = acons(cell_symbol_call_with_current_continuation, cell_call_with_current_continuation, a);
	a = acons(cell_symbol_mes_version, GetSCM2(make_string_("git"), g_cells), a); // FIXME
	a = acons(cell_symbol_mes_prefix, GetSCM2(make_string_("mes"), g_cells), a);  // FIXME
	a = acons(cell_type_bytes, make_cell__ (TNUMBER, 0, (long)TBYTES), a);
	a = acons(cell_type_char, make_cell__ (TNUMBER, 0, (long)TCHAR), a);
	a = acons(cell_type_closure, make_cell__ (TNUMBER, 0, (long)TCLOSURE), a);
	a = acons(cell_type_continuation, make_cell__ (TNUMBER, 0, (long)TCONTINUATION), a);
	a = acons(cell_type_keyword, make_cell__ (TNUMBER, 0, (long)TKEYWORD), a);
	a = acons(cell_type_macro, make_cell__ (TNUMBER, 0, (long)TMACRO), a);
	a = acons(cell_type_number, make_cell__ (TNUMBER, 0, (long)TNUMBER), a);
	a = acons(cell_type_pair, make_cell__ (TNUMBER, 0, (long)TPAIR), a);
	a = acons(cell_type_port, make_cell__ (TNUMBER, 0, (long)TPORT), a);
	a = acons(cell_type_ref, make_cell__ (TNUMBER, 0, (long)TREF), a);
	a = acons(cell_type_special, make_cell__ (TNUMBER, 0, (long)TSPECIAL), a);
	a = acons(cell_type_string, make_cell__ (TNUMBER, 0, (long)TSTRING), a);
	a = acons(cell_type_struct, make_cell__ (TNUMBER, 0, (long)TSTRUCT), a);
	a = acons(cell_type_symbol, make_cell__ (TNUMBER, 0, (long)TSYMBOL), a);
	a = acons(cell_type_values, make_cell__ (TNUMBER, 0, (long)TVALUES), a);
	a = acons(cell_type_variable, make_cell__ (TNUMBER, 0, (long)TVARIABLE), a);
	a = acons(cell_type_vector, make_cell__ (TNUMBER, 0, (long)TVECTOR), a);
	a = acons(cell_type_broken_heart, make_cell__ (TNUMBER, 0, (long)TBROKEN_HEART), a);
	a = acons(cell_closure, a, a);
	return a;
}

SCM make_builtin(struct scm* builtin_type, SCM name, SCM arity, SCM function)
{
	SCM values = cell_nil;
	values = cons(function, values);
	values = cons(arity, values);
	values = cons(name, values);
	values = cons(cell_symbol_builtin, values);
	return GetSCM2(make_struct(GetSCM2(builtin_type, g_cells), values, GetSCM2(cstring_to_symbol("builtin-printer"), g_cells)), g_cells);
}

struct scm* make_builtin_type()  ///(internal))
{
	struct scm* fields = Getstructscm2(cell_nil, g_cells);
	fields = cons2(cstring_to_symbol("address"), fields);
	fields = cons2(cstring_to_symbol("arity"), fields);
	fields = cons2(cstring_to_symbol("name"), fields);
	fields = cons2(fields, Getstructscm2(cell_nil, g_cells));
	fields = cons2(Getstructscm2(cell_symbol_builtin, g_cells), fields);
	return make_struct(cell_symbol_record_type, GetSCM2(fields, g_cells), cell_unspecified);
}

struct scm* init_builtin(struct scm* builtin_type, char const* name, int arity, struct scm*(*function)(), struct scm* a)
{
	SCM s = GetSCM2(cstring_to_symbol(name), g_cells);
	return Getstructscm2(acons(s, make_builtin(builtin_type, GetSCM2(bad2good(symbol_to_string(s), g_cells), g_cells), make_cell__ (TNUMBER, 0, (long)arity), make_cell__ (TNUMBER, 0, (long)function)), GetSCM2(a, g_cells)), g_cells);
}

struct scm* builtin_name(SCM builtin)
{
	return good2bad(struct_ref_(builtin, 3), g_cells);
}

struct scm* builtin_arity(SCM builtin)
{
	return good2bad(struct_ref_(builtin, 4), g_cells);
}

void* builtin_function(SCM builtin)
{
	return (void*)g_cells[GetSCM2(struct_ref_(builtin, 5), g_cells)].rdc;
}

SCM builtin_p(SCM x)
{
	return (g_cells[x].type == TSTRUCT && GetSCM2(struct_ref_(x, 2), g_cells) == cell_symbol_builtin) ? cell_t : cell_f;
}

struct scm* builtin_printer(SCM builtin)
{
	fdputs("#<procedure ", __stdout);
	display_(GetSCM2(bad2good(builtin_name(builtin), g_cells), g_cells));
	fdputc(' ', __stdout);
	int arity = g_cells[GetSCM2(bad2good(builtin_arity(builtin), g_cells), g_cells)].value;

	if(arity == -1)
	{
		fdputc('_', __stdout);
	}
	else
	{
		fdputc('(', __stdout);

		for(int i = 0; i < arity; i++)
		{
			if(i)
			{
				fdputc(' ', __stdout);
			}

			fdputc('_', __stdout);
		}
	}

	fdputc('>', __stdout);
	return good2bad(Getstructscm2(cell_unspecified, g_cells), g_cells);
}

struct scm* apply_builtin(SCM fn, SCM x)  ///((internal))
{
	int arity = g_cells[GetSCM2(bad2good(builtin_arity(fn), g_cells), g_cells)].value;
	struct scm* y = Getstructscm2(x, g_cells);

	if((arity > 0 || arity == -1) && x != cell_nil && g_cells[y->rac].type == TVALUES)
	{
		y = cons2(bad2good(bad2good(bad2good(y->car, g_cells)->cdr, g_cells)->car, g_cells), bad2good(y->cdr, g_cells));
	}

	if((arity > 1 || arity == -1) && x != cell_nil && g_cells[g_cells[x].rdc].type == TPAIR && g_cells[g_cells[g_cells[x].rdc].rac].type == TVALUES)
	{
		y = cons2(bad2good(y->car, g_cells), cons2(bad2good(bad2good(bad2good(bad2good(y->cdr, g_cells)->car, g_cells)->cdr, g_cells)->car, g_cells), bad2good(y->cdr, g_cells)));
	}

	if(arity == 0)
	{
		//function0_t fp = f->function;
		FUNCTION0* fp = builtin_function(fn);
		return bad2good(fp(), g_cells);
	}
	else if(arity == 1)
	{
		//function1_t fp = f->function;
		FUNCTION1* fp = builtin_function(fn);
		return bad2good(fp(y->car), g_cells);
	}
	else if(arity == 2)
	{
		//function2_t fp = f->function;
		FUNCTION2* fp = builtin_function(fn);
		return bad2good(fp(y->car, g_cells[y->rdc].car), g_cells);
	}
	else if(arity == 3)
	{
		//function3_t fp = f->function;
		FUNCTION3* fp = builtin_function(fn);
		return bad2good(fp(y->car, g_cells[y->rdc].car, g_cells[g_cells[y->rdc].rdc].car), g_cells);
	}
	else if(arity == -1)
	{
		//functionn_t fp = f->function;
		FUNCTION1* fp = builtin_function(fn);
		return bad2good(fp(good2bad(y, g_cells)), g_cells);
	}

	return Getstructscm2(cell_unspecified, g_cells);
}


struct scm* mes_builtins(struct scm* a)  ///((internal))
{
	// TODO minimal: cons, car, cdr, list, null_p, eq_p minus, plus,  display_, display_error_, getenv
	struct scm* builtin_type = make_builtin_type();
	/* src/gc.mes */
	a = init_builtin(builtin_type, "gc-check", 0, &gc_check, a);
	a = init_builtin(builtin_type, "gc", 0, &gc, a);
	/* src/hash.mes */
	a = init_builtin(builtin_type, "hashq", 2, &hashq, a);
	a = init_builtin(builtin_type, "hash", 2, &hash, a);
	a = init_builtin(builtin_type, "hashq-get-handle", 3, &hashq_get_handle, a);
	a = init_builtin(builtin_type, "hashq-ref", 3, &hashq_ref, a);
	a = init_builtin(builtin_type, "hash-ref", 3, &hash_ref, a);
	a = init_builtin(builtin_type, "hashq-set!", 3, &hashq_set_x, a);
	a = init_builtin(builtin_type, "hash-set!", 3, &hash_set_x, a);
	a = init_builtin(builtin_type, "hash-table-printer", 1, &hash_table_printer, a);
	a = init_builtin(builtin_type, "make-hash-table", 1, &make_hash_table, a);
	/* src/lib.mes */
	a = init_builtin(builtin_type, "core:display", 1, &display_, a);
	a = init_builtin(builtin_type, "core:display-error", 1, &display_error_, a);
	a = init_builtin(builtin_type, "core:display-port", 2, &display_port_, a);
	a = init_builtin(builtin_type, "core:write", 1, &write_, a);
	a = init_builtin(builtin_type, "core:write-error", 1, &write_error_, a);
	a = init_builtin(builtin_type, "core:write-port", 2, &write_port_, a);
	a = init_builtin(builtin_type, "exit", 1, &exit_, a);
	a = init_builtin(builtin_type, "frame-printer", 1, &frame_printer, a);
	a = init_builtin(builtin_type, "make-stack", -1, &make_stack, a);
	a = init_builtin(builtin_type, "stack-length", 1, &stack_length, a);
	a = init_builtin(builtin_type, "stack-ref", 2, &stack_ref, a);
	a = init_builtin(builtin_type, "xassq", 2, &xassq, a);
	a = init_builtin(builtin_type, "memq", 2, &memq, a);
	a = init_builtin(builtin_type, "equal2?", 2, &equal2_p, a);
	a = init_builtin(builtin_type, "last-pair", 1, &last_pair, a);
	a = init_builtin(builtin_type, "pair?", 1, &pair_p, a);
	/* src/math.mes */
	a = init_builtin(builtin_type, ">", -1, &greater_p, a);
	a = init_builtin(builtin_type, "<", -1, &less_p, a);
	a = init_builtin(builtin_type, "=", -1, &is_p, a);
	a = init_builtin(builtin_type, "-", -1, &minus, a);
	a = init_builtin(builtin_type, "+", -1, &plus, a);
	a = init_builtin(builtin_type, "/", -1, &divide, a);
	a = init_builtin(builtin_type, "modulo", 2, &modulo, a);
	a = init_builtin(builtin_type, "*", -1, &multiply, a);
	a = init_builtin(builtin_type, "logand", -1, &logand, a);
	a = init_builtin(builtin_type, "logior", -1, &logior, a);
	a = init_builtin(builtin_type, "lognot", 1, &lognot, a);
	a = init_builtin(builtin_type, "logxor", -1, &logxor, a);
	a = init_builtin(builtin_type, "ash", 2, &ash, a);
	/* src/mes.mes */
	a = init_builtin(builtin_type, "core:make-cell", 3, &make_cell_, a);
	a = init_builtin(builtin_type, "core:type", 1, &type_, a);
	a = init_builtin(builtin_type, "core:car", 1, &car_, a);
	a = init_builtin(builtin_type, "core:cdr", 1, &cdr_, a);
	a = init_builtin(builtin_type, "cons", 2, &cons, a);
	a = init_builtin(builtin_type, "car", 1, &car, a);
	a = init_builtin(builtin_type, "cdr", 1, &cdr, a);
	a = init_builtin(builtin_type, "list", -1, &list, a);
	a = init_builtin(builtin_type, "null?", 1, &null_p, a);
	a = init_builtin(builtin_type, "eq?", 2, &eq_p, a);
	a = init_builtin(builtin_type, "values", -1, &values, a);
	a = init_builtin(builtin_type, "acons", 3, &acons, a);
	a = init_builtin(builtin_type, "length", 1, &length, a);
	a = init_builtin(builtin_type, "error", 2, &error, a);
	a = init_builtin(builtin_type, "append2", 2, &append2, a);
	a = init_builtin(builtin_type, "append-reverse", 2, &append_reverse, a);
	a = init_builtin(builtin_type, "core:reverse!", 2, &reverse_x_, a);
	a = init_builtin(builtin_type, "pairlis", 3, &pairlis, a);
	a = init_builtin(builtin_type, "assq", 2, &assq, a);
	a = init_builtin(builtin_type, "assoc", 2, &assoc, a);
	a = init_builtin(builtin_type, "set-car!", 2, &set_car_x, a);
	a = init_builtin(builtin_type, "set-cdr!", 2, &set_cdr_x, a);
	a = init_builtin(builtin_type, "set-env!", 3, &set_env_x, a);
	a = init_builtin(builtin_type, "macro-get-handle", 1, &macro_get_handle, a);
	a = init_builtin(builtin_type, "add-formals", 2, &add_formals, a);
	a = init_builtin(builtin_type, "eval-apply", 0, &eval_apply, a);
	a = init_builtin(builtin_type, "make-builtin-type", 0, &make_builtin_type, a);
	a = init_builtin(builtin_type, "make-builtin", 4, &make_builtin, a);
	a = init_builtin(builtin_type, "builtin-name", 1, &builtin_name, a);
	a = init_builtin(builtin_type, "builtin-arity", 1, &builtin_arity, a);
	a = init_builtin(builtin_type, "builtin?", 1, &builtin_p, a);
	a = init_builtin(builtin_type, "builtin-printer", 1, &builtin_printer, a);
	/* src/module.mes */
	a = init_builtin(builtin_type, "make-module-type", 0, &make_module_type, a);
	a = init_builtin(builtin_type, "module-printer", 1, &module_printer, a);
	a = init_builtin(builtin_type, "module-variable", 2, &module_variable_, a);
	a = init_builtin(builtin_type, "module-ref", 2, &module_ref, a);
	a = init_builtin(builtin_type, "module-define!", 3, &module_define_x, a);
	/* src/posix.mes */
	a = init_builtin(builtin_type, "peek-byte", 0, &peek_byte, a);
	a = init_builtin(builtin_type, "read-byte", 0, &read_byte, a);
	a = init_builtin(builtin_type, "unread-byte", 1, &unread_byte, a);
	a = init_builtin(builtin_type, "peek-char", 0, &peek_char, a);
	a = init_builtin(builtin_type, "read-char", -1, &read_char, a);
	a = init_builtin(builtin_type, "unread-char", 1, &unread_char, a);
	a = init_builtin(builtin_type, "write-char", -1, &write_char, a);
	a = init_builtin(builtin_type, "write-byte", -1, &write_byte, a);
	a = init_builtin(builtin_type, "getenv", 1, &getenv_, a);
	a = init_builtin(builtin_type, "setenv", 2, &setenv_, a);
	a = init_builtin(builtin_type, "access?", 2, &access_p, a);
	a = init_builtin(builtin_type, "current-input-port", 0, &current_input_port, a);
	a = init_builtin(builtin_type, "open-input-file", 1, &open_input_file, a);
	a = init_builtin(builtin_type, "open-input-string", 1, &open_input_string, a);
	a = init_builtin(builtin_type, "set-current-input-port", 1, &set_current_input_port, a);
	a = init_builtin(builtin_type, "current-output-port", 0, &current_output_port, a);
	a = init_builtin(builtin_type, "current-error-port", 0, &current_error_port, a);
	a = init_builtin(builtin_type, "open-output-file", -1, &open_output_file, a);
	a = init_builtin(builtin_type, "set-current-output-port", 1, &set_current_output_port, a);
	a = init_builtin(builtin_type, "set-current-error-port", 1, &set_current_error_port, a);
	a = init_builtin(builtin_type, "chmod", 2, &chmod_, a);
	a = init_builtin(builtin_type, "isatty?", 1, &isatty_p, a);
	a = init_builtin(builtin_type, "primitive-fork", 0, &primitive_fork, a);
	a = init_builtin(builtin_type, "execl", 2, &execl_, a);
	a = init_builtin(builtin_type, "core:waitpid", 2, &waitpid_, a);
	a = init_builtin(builtin_type, "current-time", 0, &current_time, a);
	a = init_builtin(builtin_type, "gettimeofday", 0, &gettimeofday_, a);
	a = init_builtin(builtin_type, "get-internal-run-time", 0, &get_internal_run_time, a);
	a = init_builtin(builtin_type, "getcwd", 0, &getcwd_, a);
	a = init_builtin(builtin_type, "dup", 1, &dup_, a);
	a = init_builtin(builtin_type, "dup2", 2, &dup2_, a);
	a = init_builtin(builtin_type, "delete-file", 1, &delete_file, a);
	/* src/reader.mes */
	a = init_builtin(builtin_type, "core:read-input-file-env", 2, &read_input_file_env_, a);
	a = init_builtin(builtin_type, "read-input-file-env", 1, &read_input_file_env, a);
	a = init_builtin(builtin_type, "read-env", 1, &read_env, a);
	a = init_builtin(builtin_type, "reader-read-sexp", 3, &reader_read_sexp, a);
	a = init_builtin(builtin_type, "reader-read-character", 0, &reader_read_character, a);
	a = init_builtin(builtin_type, "reader-read-binary", 0, &reader_read_binary, a);
	a = init_builtin(builtin_type, "reader-read-octal", 0, &reader_read_octal, a);
	a = init_builtin(builtin_type, "reader-read-hex", 0, &reader_read_hex, a);
	a = init_builtin(builtin_type, "reader-read-string", 0, &reader_read_string, a);
	/* src/strings.mes */
	a = init_builtin(builtin_type, "string=?", 2, &string_equal_p, a);
	a = init_builtin(builtin_type, "symbol->string", 1, &symbol_to_string, a);
	a = init_builtin(builtin_type, "symbol->keyword", 1, &symbol_to_keyword, a);
	a = init_builtin(builtin_type, "keyword->string", 1, &keyword_to_string, a);
	a = init_builtin(builtin_type, "string->symbol", 1, &string_to_symbol, a);
	a = init_builtin(builtin_type, "make-symbol", 1, &make_symbol, a);
	a = init_builtin(builtin_type, "string->list", 1, &string_to_list, a);
	a = init_builtin(builtin_type, "list->string", 1, &list_to_string, a);
	a = init_builtin(builtin_type, "read-string", -1, &read_string, a);
	a = init_builtin(builtin_type, "string-append", -1, &string_append, a);
	a = init_builtin(builtin_type, "string-length", 1, &string_length, a);
	a = init_builtin(builtin_type, "string-ref", 2, &string_ref, a);
	/* src/struct.mes */
	a = init_builtin(builtin_type, "make-struct", 3, &make_struct_, a);
	a = init_builtin(builtin_type, "struct-length", 1, &struct_length, a);
	a = init_builtin(builtin_type, "struct-ref", 2, &struct_ref, a);
	a = init_builtin(builtin_type, "struct-set!", 3, &struct_set_x, a);
	/* src/vector.mes */
	a = init_builtin(builtin_type, "core:make-vector", 1, &make_vector_, a);
	a = init_builtin(builtin_type, "vector-length", 1, &vector_length, a);
	a = init_builtin(builtin_type, "vector-ref", 2, &vector_ref, a);
	a = init_builtin(builtin_type, "vector-entry", 1, &vector_entry, a);
	a = init_builtin(builtin_type, "vector-set!", 3, &vector_set_x, a);
	a = init_builtin(builtin_type, "list->vector", 1, &list_to_vector, a);
	a = init_builtin(builtin_type, "vector->list", 1, &vector_to_list, a);
	return a;
}
