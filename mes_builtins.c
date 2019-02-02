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
SCM eq_p (SCM x, SCM y);
SCM error (SCM key, SCM x);
SCM eval_apply ();
SCM length (SCM x);
SCM list (SCM x);
SCM macro_get_handle (SCM name);
SCM make_builtin (SCM builtin_type, SCM name, SCM arity, SCM function);
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
struct scm* apply_builtin(SCM fn, SCM x);

// src/module.mes
struct scm* make_module_type ();
struct scm* module_printer (SCM module);
struct scm* module_variable (SCM module, SCM name);
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
struct scm* make_struct (SCM type, SCM fields, SCM printer);
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


struct scm* cstring_to_symbol(char const *s);
struct scm* struct_ref_(SCM x, SCM i);
int fdputc (int c, int fd);
int fdputs (char const* s, int fd);
int eputs (char const* s);

SCM make_builtin_type()  ///(internal))
{
	SCM record_type = cell_symbol_record_type;
	SCM fields = cell_nil;
	fields = cons(GetSCM2(cstring_to_symbol("address"), g_cells), fields);
	fields = cons(GetSCM2(cstring_to_symbol("arity"), g_cells), fields);
	fields = cons(GetSCM2(cstring_to_symbol("name"), g_cells), fields);
	fields = cons(fields, cell_nil);
	fields = cons(cell_symbol_builtin, fields);
	return GetSCM(make_struct(record_type, fields, cell_unspecified));
}

struct scm* init_builtin(SCM builtin_type, char const* name, int arity, struct scm*(*function)(), struct scm* a)
{
	SCM s = GetSCM2(cstring_to_symbol(name), g_cells);
	return Getstructscm2(acons(s, make_builtin(builtin_type, GetSCM(symbol_to_string(s)), MAKE_NUMBER(arity), MAKE_NUMBER(function)), GetSCM2(a, g_cells)), g_cells);
}

SCM make_builtin(SCM builtin_type, SCM name, SCM arity, SCM function)
{
	SCM values = cell_nil;
	values = cons(function, values);
	values = cons(arity, values);
	values = cons(name, values);
	values = cons(cell_symbol_builtin, values);
	return GetSCM(make_struct(builtin_type, values, GetSCM2(cstring_to_symbol("builtin-printer"), g_cells)));
}

SCM builtin_name(SCM builtin)
{
	return GetSCM(struct_ref_(builtin, 3));
}

SCM builtin_arity(SCM builtin)
{
	return GetSCM(struct_ref_(builtin, 4));
}

void* builtin_function(SCM builtin)
{
	return (void*)VALUE(GetSCM(struct_ref_(builtin, 5)));
}

SCM builtin_p(SCM x)
{
	return (TYPE(x) == TSTRUCT && GetSCM(struct_ref_(x, 2)) == cell_symbol_builtin) ? cell_t : cell_f;
}

struct scm* builtin_printer(SCM builtin)
{
	fdputs("#<procedure ", __stdout);
	display_(builtin_name(builtin));
	fdputc(' ', __stdout);
	int arity = VALUE(builtin_arity(builtin));

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
	return Getstructscm(cell_unspecified);
}

struct scm* apply_builtin(SCM fn, SCM x)  ///((internal))
{
	int arity = VALUE(builtin_arity(fn));

	if((arity > 0 || arity == -1) && x != cell_nil && TYPE(CAR(x)) == TVALUES)
	{
		x = cons(CADAR(x), CDR(x));
	}

	if((arity > 1 || arity == -1) && x != cell_nil && TYPE(CDR(x)) == TPAIR && TYPE(CADR(x)) == TVALUES)
	{
		x = cons(CAR(x), cons(CDADAR(x), CDR(x)));
	}

	if(arity == 0)
	{
		//function0_t fp = f->function;
		FUNCTION0* fp = builtin_function(fn);
		return fp();
	}
	else if(arity == 1)
	{
		//function1_t fp = f->function;
		FUNCTION1* fp = builtin_function(fn);
		return fp(Getstructscm(CAR(x)));
	}
	else if(arity == 2)
	{
		//function2_t fp = f->function;
		FUNCTION2* fp = builtin_function(fn);
		return fp(Getstructscm(CAR(x)), Getstructscm(CADR(x)));
	}
	else if(arity == 3)
	{
		//function3_t fp = f->function;
		FUNCTION3* fp = builtin_function(fn);
		return fp(Getstructscm(CAR(x)), Getstructscm(CADR(x)), Getstructscm(CAR(CDDR(x))));
	}
	else if(arity == -1)
	{
		//functionn_t fp = f->function;
		FUNCTION1* fp = builtin_function(fn);
		return fp(Getstructscm(x));
	}

	return Getstructscm(cell_unspecified);
}


struct scm* mes_builtins(struct scm* a)  ///((internal))
{
	// TODO minimal: cons, car, cdr, list, null_p, eq_p minus, plus
	// display_, display_error_, getenv
	SCM builtin_type = make_builtin_type();
	// src/gc.mes
	a = init_builtin(builtin_type, "gc-check", 0, &gc_check, a);
	a = init_builtin(builtin_type, "gc", 0, &gc, a);
	// src/hash.mes
	a = init_builtin(builtin_type, "hashq", 2, &hashq, a);
	a = init_builtin(builtin_type, "hash", 2, &hash, a);
	a = init_builtin(builtin_type, "hashq-get-handle", 3, &hashq_get_handle, a);
	a = init_builtin(builtin_type, "hashq-ref", 3, &hashq_ref, a);
	a = init_builtin(builtin_type, "hash-ref", 3, &hash_ref, a);
	a = init_builtin(builtin_type, "hashq-set!", 3, &hashq_set_x, a);
	a = init_builtin(builtin_type, "hash-set!", 3, &hash_set_x, a);
	a = init_builtin(builtin_type, "hash-table-printer", 1, &hash_table_printer, a);
	a = init_builtin(builtin_type, "make-hash-table", 1, &make_hash_table, a);
	// src/lib.mes
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
	// src/math.mes
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
	// src/mes.mes
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
	// src/module.mes
	a = init_builtin(builtin_type, "make-module-type", 0, &make_module_type, a);
	a = init_builtin(builtin_type, "module-printer", 1, &module_printer, a);
	a = init_builtin(builtin_type, "module-variable", 2, &module_variable, a);
	a = init_builtin(builtin_type, "module-ref", 2, &module_ref, a);
	a = init_builtin(builtin_type, "module-define!", 3, &module_define_x, a);
	// src/posix.mes
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
	// src/reader.mes
	a = init_builtin(builtin_type, "core:read-input-file-env", 2, &read_input_file_env_, a);
	a = init_builtin(builtin_type, "read-input-file-env", 1, &read_input_file_env, a);
	a = init_builtin(builtin_type, "read-env", 1, &read_env, a);
	a = init_builtin(builtin_type, "reader-read-sexp", 3, &reader_read_sexp, a);
	a = init_builtin(builtin_type, "reader-read-character", 0, &reader_read_character, a);
	a = init_builtin(builtin_type, "reader-read-binary", 0, &reader_read_binary, a);
	a = init_builtin(builtin_type, "reader-read-octal", 0, &reader_read_octal, a);
	a = init_builtin(builtin_type, "reader-read-hex", 0, &reader_read_hex, a);
	a = init_builtin(builtin_type, "reader-read-string", 0, &reader_read_string, a);
	// src/strings.mes
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
	// src/struct.mes
	a = init_builtin(builtin_type, "make-struct", 3, &make_struct, a);
	a = init_builtin(builtin_type, "struct-length", 1, &struct_length, a);
	a = init_builtin(builtin_type, "struct-ref", 2, &struct_ref, a);
	a = init_builtin(builtin_type, "struct-set!", 3, &struct_set_x, a);
	// src/vector.mes
	a = init_builtin(builtin_type, "core:make-vector", 1, &make_vector_, a);
	a = init_builtin(builtin_type, "vector-length", 1, &vector_length, a);
	a = init_builtin(builtin_type, "vector-ref", 2, &vector_ref, a);
	a = init_builtin(builtin_type, "vector-entry", 1, &vector_entry, a);
	a = init_builtin(builtin_type, "vector-set!", 3, &vector_set_x, a);
	a = init_builtin(builtin_type, "list->vector", 1, &list_to_vector, a);
	a = init_builtin(builtin_type, "vector->list", 1, &vector_to_list, a);
	return Getstructscm(a);
}
