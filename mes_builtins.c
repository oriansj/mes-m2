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
struct scm* access_p(struct scm* x);
struct scm* acons(struct scm* x);
struct scm* add_formals(struct scm* x);
struct scm* append2(struct scm* x);
struct scm* ash(struct scm* x);
struct scm* assoc(struct scm* x);
struct scm* assq(struct scm* x);
struct scm* builtin_printer(struct scm* x);
struct scm* car(struct scm* x);
struct scm* cdr(struct scm* x);
struct scm* cons(struct scm* x);
struct scm* current_error_port(struct scm* x);
struct scm* current_input_port(struct scm* x);
struct scm* current_output_port(struct scm* x);
struct scm* delete_file(struct scm* x);
struct scm* display(struct scm* x);
struct scm* display_error(struct scm* x);
struct scm* display_port(struct scm* x);
struct scm* divide(struct scm* x);
struct scm* eq_p(struct scm* x);
struct scm* equal2_p(struct scm* x);
struct scm* error(struct scm* x);
struct scm* eval_apply(struct scm* x);
struct scm* exec(struct scm* x);
struct scm* file_chmod(struct scm* x);
struct scm* file_dup(struct scm* x);
struct scm* file_dup2(struct scm* x);
struct scm* frame_printer(struct scm* x);
struct scm* gc(struct scm* x);
struct scm* gc_check(struct scm* x);
struct scm* get_cwd(struct scm* x);
struct scm* get_env(struct scm* x);
struct scm* greater_p(struct scm* x);
struct scm* hash(struct scm* x);
struct scm* hash_ref(struct scm* x);
struct scm* hash_set_x(struct scm* x);
struct scm* hashq(struct scm* x);
struct scm* hashq_get_handle(struct scm* x);
struct scm* hashq_ref(struct scm* x);
struct scm* hashq_set_x(struct scm* x);
struct scm* is_p(struct scm* x);
struct scm* isatty_p(struct scm* x);
struct scm* keyword_to_string(struct scm* x);
struct scm* last_pair(struct scm* x);
struct scm* length(struct scm* x);
struct scm* less_p(struct scm* x);
struct scm* list(struct scm* x);
struct scm* list_to_string(struct scm* x);
struct scm* list_to_vector(struct scm* x);
struct scm* logand(struct scm* x);
struct scm* logior(struct scm* x);
struct scm* lognot(struct scm* x);
struct scm* logxor(struct scm* x);
struct scm* macro_get_handle(struct scm* x);
struct scm* make_cell(struct scm* x);
struct scm* make_function(struct scm* x);
struct scm* make_hash_table(struct scm* x);
struct scm* make_module_type(struct scm* x);
struct scm* make_number(struct scm* x);
struct scm* make_stack(struct scm* x);
struct scm* make_struct(struct scm* x);
struct scm* make_symbol(struct scm* x);
struct scm* make_vector(struct scm* x);
struct scm* memq(struct scm* x);
struct scm* minus(struct scm* x);
struct scm* module_define_x(struct scm* x);
struct scm* module_printer(struct scm* x);
struct scm* module_ref(struct scm* x);
struct scm* module_variable(struct scm* x);
struct scm* modulo(struct scm* x);
struct scm* multiply(struct scm* x);
struct scm* null_p(struct scm* x);
struct scm* open_input_file(struct scm* x);
struct scm* open_input_string(struct scm* x);
struct scm* open_output_file(struct scm* x);
struct scm* pair_p(struct scm* x);
struct scm* pairlis(struct scm* x);
struct scm* peek_byte(struct scm* x);
struct scm* peek_char(struct scm* x);
struct scm* plus(struct scm* x);
struct scm* primitive_fork(struct scm* x);
struct scm* read_byte(struct scm* x);
struct scm* read_char(struct scm* x);
struct scm* read_env(struct scm* x);
struct scm* read_input_file_env(struct scm* x);
struct scm* read_string(struct scm* x);
struct scm* reader_read_binary(struct scm* x);
struct scm* reader_read_character(struct scm* x);
struct scm* reader_read_hex(struct scm* x);
struct scm* reader_read_octal(struct scm* x);
struct scm* reader_read_sexp(struct scm* x);
struct scm* reader_read_string(struct scm* x);
struct scm* reverse_x(struct scm* x);
struct scm* scm_exit(struct scm* x);
struct scm* scm_write(struct scm* x);
struct scm* set_car_x(struct scm* x);
struct scm* set_cdr_x(struct scm* x);
struct scm* set_current_error_port(struct scm* x);
struct scm* set_current_input_port(struct scm* x);
struct scm* set_current_output_port(struct scm* x);
struct scm* set_env(struct scm* x);
struct scm* set_env_x(struct scm* x);
struct scm* stack_length(struct scm* x);
struct scm* stack_ref(struct scm* x);
struct scm* string_append(struct scm* x);
struct scm* string_equal_p(struct scm* x);
struct scm* string_length(struct scm* x);
struct scm* string_ref(struct scm* x);
struct scm* string_to_list(struct scm* x);
struct scm* string_to_symbol(struct scm* x);
struct scm* struct_length(struct scm* x);
struct scm* struct_ref(struct scm* x);
struct scm* struct_set_x(struct scm* x);
struct scm* symbol_to_keyword(struct scm* x);
struct scm* symbol_to_string(struct scm* x);
struct scm* type(struct scm* x);
struct scm* unread_byte(struct scm* x);
struct scm* unread_char(struct scm* x);
struct scm* values(struct scm* x);
struct scm* vector_length(struct scm* x);
struct scm* vector_ref(struct scm* x);
struct scm* vector_set_x(struct scm* x);
struct scm* vector_to_list(struct scm* x);
struct scm* wait_pid(struct scm* x);
struct scm* write_byte(struct scm* x);
struct scm* write_char(struct scm* x);
struct scm* write_error(struct scm* x);
struct scm* write_port(struct scm* x);
struct scm* xassq(struct scm* x);

/* Internal functions required*/
int eputs(char* s);
int fdputc(int c, int fd);
int fdputs(char* s, int fd);
int string_len(char* a);
struct scm* acons_(struct scm* key, struct scm* value, struct scm* alist);
struct scm* cstring_to_symbol(char* s);
struct scm* display_(struct scm* x);
struct scm* hash_set_x_(struct scm* table, struct scm* key, struct scm* value);
struct scm* make_function_(FUNCTION n);
struct scm* make_hash_table_(SCM size);
struct scm* make_number_(SCM n);
struct scm* make_primitive_(int arity, FUNCTION n);
struct scm* make_string(char* s, int length);
struct scm* make_string_(char* s);
struct scm* make_struct_(struct scm* type, struct scm* fields, struct scm* printer);
struct scm* make_tpair(struct scm* a, struct scm* b);
struct scm* struct_ref_(struct scm* x, SCM i);
struct scm* symbol_to_string_(struct scm* symbol);

void init_symbol(struct scm* x, SCM type, char* name)
{
	struct scm* y = x;
	y->type = type;
	int l = string_len(name);
	struct scm* string = make_string_(name);
	y->length = l;
	y->string = string->string;
	hash_set_x_(g_symbols, string, x);
}

struct scm* mes_symbols()  /*((internal)) */
{
	g_symbol_max = g_free;
	g_symbols = make_hash_table_(500);
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
	struct scm* a = cell_nil;
	a = acons_(cell_symbol_call_with_values, cell_symbol_call_with_values, a);
	a = acons_(cell_symbol_boot_module, cell_symbol_boot_module, a);
	a = acons_(cell_symbol_current_module, cell_symbol_current_module, a);
	a = acons_(cell_symbol_call_with_current_continuation, cell_call_with_current_continuation, a);
	a = acons_(cell_symbol_mes_version, make_string_("git"), a); /* FIXME */
	a = acons_(cell_symbol_mes_prefix, make_string_("mes"), a);  /* FIXME */
	a = acons_(cell_type_bytes, make_number_(TBYTES), a);
	a = acons_(cell_type_char, make_number_(TCHAR), a);
	a = acons_(cell_type_closure, make_number_(TCLOSURE), a);
	a = acons_(cell_type_continuation, make_number_(TCONTINUATION), a);
	a = acons_(cell_type_keyword, make_number_(TKEYWORD), a);
	a = acons_(cell_type_macro, make_number_(TMACRO), a);
	a = acons_(cell_type_number, make_number_(TNUMBER), a);
	a = acons_(cell_type_pair, make_number_(TPAIR), a);
	a = acons_(cell_type_port, make_number_(TPORT), a);
	a = acons_(cell_type_ref, make_number_(TREF), a);
	a = acons_(cell_type_special, make_number_(TSPECIAL), a);
	a = acons_(cell_type_string, make_number_(TSTRING), a);
	a = acons_(cell_type_struct, make_number_(TSTRUCT), a);
	a = acons_(cell_type_symbol, make_number_(TSYMBOL), a);
	a = acons_(cell_type_values, make_number_(TVALUES), a);
	a = acons_(cell_type_variable, make_number_(TVARIABLE), a);
	a = acons_(cell_type_vector, make_number_(TVECTOR), a);
	a = acons_(cell_type_broken_heart, make_number_(TBROKEN_HEART), a);
	a = acons_(cell_closure, a, a);
	return a;
}

struct scm* builtin_symbols;
struct scm* init_builtin(char* name, int arity, FUNCTION function)
{
	struct scm* s = cstring_to_symbol(name);
	builtin_symbols = acons_(s, make_primitive_(arity, function), builtin_symbols);
	return builtin_symbols;
}

struct scm* builtin_name(struct scm* builtin) /* External */
{
	return builtin->car->car;
}

struct scm* builtin_arity_(struct scm* builtin) /* Internal */
{
	return make_number_(builtin->cdr->length);
}

struct scm* builtin_arity(struct scm* x) /* External */
{
	return builtin_arity_(x->car);
}

struct scm* builtin_p_(struct scm* builtin) /* Internal */
{
	/* make sure of correct type first */
	if(TPRIMITIVE != builtin->type) return cell_f;
	return cell_t;
}

struct scm* builtin_p(struct scm* x) /* External */
{
	return builtin_p_(x->car);
}


struct scm* builtin_printer(struct scm* x) /* External */
{
	struct scm* builtin = x->car;
	fdputs("#<procedure ", __stdout);
	display_(builtin_name(builtin));
	fdputc(' ', __stdout);
	int arity = builtin_arity_(builtin)->value;

	if(arity == -1)
	{
		fdputc('_', __stdout);
	}
	else
	{
		fdputc('(', __stdout);

		int i;
		for(i = 0; i < arity; i = i + 1)
		{
			if(i)
			{
				fdputc(' ', __stdout);
			}

			fdputc('_', __stdout);
		}
	}

	fdputc('>', __stdout);
	return cell_unspecified;
}

struct scm* apply_builtin(FUNCTION* fn, struct scm* x)  /* ((internal)) */
{
	if(x != cell_nil && x->car->type == TVALUES)
	{
		x = make_tpair(x->car->cdr->car, x->cdr);
	}

	if(x != cell_nil && x->cdr->type == TPAIR)
	{
		if(x->cdr->car->type == TVALUES)
		{
			x = make_tpair(x->car, make_tpair(x->cdr->car->cdr->car, x->cdr));
		}
	}

	return fn(x);
}


struct scm* mes_builtins()  /* ((internal)) */
{
	builtin_symbols = cell_nil;
	/* TODO minimal: cons, car, cdr, list, null_p, eq_p minus, plus,  display_, display_error_, getenv */
	/* src/gc.mes */
	init_builtin("gc-check", 0, &gc_check);
	init_builtin("gc", 0, &gc);
	/* src/hash.mes */
	init_builtin("hashq", 2, &hashq);
	init_builtin("hash", 2, &hash);
	init_builtin("hashq-get-handle", 3, &hashq_get_handle);
	init_builtin("hashq-ref", 3, &hashq_ref);
	init_builtin("hash-ref", 3, &hash_ref);
	init_builtin("hashq-set!", 3, &hashq_set_x);
	init_builtin("hash-set!", 3, &hash_set_x);
	init_builtin("make-hash-table", 1, &make_hash_table);
	/* src/lib.mes */
	init_builtin("core:display", 1, &display);
	init_builtin("core:display-error", 1, &display_error);
	init_builtin("core:display-port", 2, &display_port);
	init_builtin("core:write", 1, &scm_write);
	init_builtin("core:write-error", 1, &write_error);
	init_builtin("core:write-port", 2, &write_port);
	init_builtin("exit", 1, &scm_exit);
	init_builtin("frame-printer", 1, &frame_printer);
	init_builtin("make-stack", -1, &make_stack);
	init_builtin("stack-length", 1, &stack_length);
	init_builtin("stack-ref", 2, &stack_ref);
	init_builtin("xassq", 2, &xassq);
	init_builtin("memq", 2, &memq);
	init_builtin("equal2?", 2, &equal2_p);
	init_builtin("last-pair", 1, &last_pair);
	init_builtin("pair?", 1, &pair_p);
	/* src/math.mes */
	init_builtin(">", -1, &greater_p);
	init_builtin("<", -1, &less_p);
	init_builtin("=", -1, &is_p);
	init_builtin("-", -1, &minus);
	init_builtin("+", -1, &plus);
	init_builtin("/", -1, &divide);
	init_builtin("modulo", 2, &modulo);
	init_builtin("*", -1, &multiply);
	init_builtin("logand", -1, &logand);
	init_builtin("logior", -1, &logior);
	init_builtin("lognot", 1, &lognot);
	init_builtin("logxor", -1, &logxor);
	init_builtin("ash", 2, &ash);
	/* src/mes.mes */
	init_builtin("core:make-cell", 3, &make_cell);
	init_builtin("core:type", 1, &type);
	init_builtin("core:car", 1, &car);
	init_builtin("core:cdr", 1, &cdr);
	init_builtin("cons", 2, &cons);
	init_builtin("list", -1, &list);
	init_builtin("null?", 1, &null_p);
	init_builtin("eq?", 2, &eq_p);
	init_builtin("values", -1, &values);
	init_builtin("acons", 3, &acons);
	init_builtin("length", 1, &length);
	init_builtin("error", 2, &error);
	init_builtin("append2", 2, &append2);
	init_builtin("core:reverse!", 2, &reverse_x);
	init_builtin("pairlis", 3, &pairlis);
	init_builtin("assq", 2, &assq);
	init_builtin("assoc", 2, &assoc);
	init_builtin("set-car!", 2, &set_car_x);
	init_builtin("set-cdr!", 2, &set_cdr_x);
	init_builtin("set-env!", 3, &set_env_x);
	init_builtin("macro-get-handle", 1, &macro_get_handle);
	init_builtin("add-formals", 2, &add_formals);
	init_builtin("eval-apply", 0, &eval_apply);
	init_builtin("builtin-name", 1, &builtin_name);
	init_builtin("builtin-arity", 1, &builtin_arity);
	init_builtin("builtin?", 1, &builtin_p);
	init_builtin("builtin-printer", 1, &builtin_printer);
	/* src/module.mes */
	init_builtin("make-module-type", 0, &make_module_type);
	init_builtin("module-printer", 1, &module_printer);
	init_builtin("module-variable", 2, &module_variable);
	init_builtin("module-ref", 2, &module_ref);
	init_builtin("module-define!", 3, &module_define_x);
	/* src/posix.mes */
	init_builtin("peek-byte", 0, &peek_byte);
	init_builtin("read-byte", 0, &read_byte);
	init_builtin("unread-byte", 1, &unread_byte);
	init_builtin("peek-char", 0, &peek_char);
	init_builtin("read-char", -1, &read_char);
	init_builtin("unread-char", 1, &unread_char);
	init_builtin("write-char", -1, &write_char);
	init_builtin("write-byte", -1, &write_byte);
	init_builtin("getenv", 1, &get_env);
	init_builtin("setenv", 2, &set_env);
	init_builtin("access?", 2, &access_p);
	init_builtin("current-input-port", 0, &current_input_port);
	init_builtin("open-input-file", 1, &open_input_file);
	init_builtin("open-input-string", 1, &open_input_string);
	init_builtin("set-current-input-port", 1, &set_current_input_port);
	init_builtin("current-output-port", 0, &current_output_port);
	init_builtin("current-error-port", 0, &current_error_port);
	init_builtin("open-output-file", -1, &open_output_file);
	init_builtin("set-current-output-port", 1, &set_current_output_port);
	init_builtin("set-current-error-port", 1, &set_current_error_port);
	init_builtin("chmod", 2, &file_chmod);
	init_builtin("isatty?", 1, &isatty_p);
	init_builtin("primitive-fork", 0, &primitive_fork);
	init_builtin("execl", 2, &exec);
	init_builtin("core:waitpid", 2, &wait_pid);
	init_builtin("getcwd", 0, &get_cwd);
	init_builtin("dup", 1, &file_dup);
	init_builtin("dup2", 2, &file_dup2);
	init_builtin("delete-file", 1, &delete_file);
	/* src/reader.mes */
	init_builtin("core:read-input-file-env", 2, &read_input_file_env);
	init_builtin("read-env", 1, &read_env);
	init_builtin("reader-read-sexp", 3, &reader_read_sexp);
	init_builtin("reader-read-character", 0, &reader_read_character);
	init_builtin("reader-read-binary", 0, &reader_read_binary);
	init_builtin("reader-read-octal", 0, &reader_read_octal);
	init_builtin("reader-read-hex", 0, &reader_read_hex);
	init_builtin("reader-read-string", 0, &reader_read_string);
	/* src/strings.mes */
	init_builtin("string=?", 2, &string_equal_p);
	init_builtin("symbol->string", 1, &symbol_to_string);
	init_builtin("symbol->keyword", 1, &symbol_to_keyword);
	init_builtin("keyword->string", 1, &keyword_to_string);
	init_builtin("string->symbol", 1, &string_to_symbol);
	init_builtin("make-symbol", 1, &make_symbol);
	init_builtin("string->list", 1, &string_to_list);
	init_builtin("list->string", 1, &list_to_string);
	init_builtin("read-string", -1, &read_string);
	init_builtin("string-append", -1, &string_append);
	init_builtin("string-length", 1, &string_length);
	init_builtin("string-ref", 2, &string_ref);
	/* src/struct.mes */
	init_builtin("make-struct", 3, &make_struct);
	init_builtin("struct-length", 1, &struct_length);
	init_builtin("struct-ref", 2, &struct_ref);
	init_builtin("struct-set!", 3, &struct_set_x);
	/* src/vector.mes */
	init_builtin("core:make-vector", 1, &make_vector);
	init_builtin("vector-length", 1, &vector_length);
	init_builtin("vector-ref", 2, &vector_ref);
	init_builtin("vector-set!", -1, &vector_set_x);
	init_builtin("list->vector", 1, &list_to_vector);
	init_builtin("vector->list", 1, &vector_to_list);
	return builtin_symbols;
}
