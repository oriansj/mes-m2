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

/* Imported Functions */
SCM make_builtin_type();
SCM init_builtin(SCM builtin_type, char const* name, int arity, SCM(*function)(SCM), SCM a);
SCM apply_builtin(SCM fn, SCM x);
SCM cstring_to_symbol(char const *s);
SCM make_hash_table_(long size);
SCM make_initial_module(SCM a);
// src/gc.mes
SCM gc_check ();
SCM gc ();
// src/hash.mes
SCM hashq (SCM x, SCM size);
SCM hash (SCM x, SCM size);
SCM hashq_get_handle (SCM table, SCM key, SCM dflt);
SCM hashq_ref (SCM table, SCM key, SCM dflt);
SCM hash_ref (SCM table, SCM key, SCM dflt);
SCM hashq_set_x (SCM table, SCM key, SCM value);
SCM hash_set_x (SCM table, SCM key, SCM value);
void hash_table_printer (SCM table);
SCM make_hash_table (SCM x);
// src/lib.mes
SCM display_ (SCM x);
SCM display_error_ (SCM x);
SCM display_port_ (SCM x, SCM p);
SCM write_ (SCM x);
SCM write_error_ (SCM x);
SCM write_port_ (SCM x, SCM p);
SCM exit_ (SCM x);
void frame_printer (SCM frame);
SCM make_stack ();
SCM stack_length (SCM stack);
SCM stack_ref (SCM stack, SCM index);
SCM xassq (SCM x, SCM a);
SCM memq (SCM x, SCM a);
SCM equal2_p (SCM a, SCM b);
SCM last_pair (SCM x);
SCM pair_p (SCM x);
// src/math.mes
SCM greater_p (SCM x);
SCM less_p (SCM x);
SCM is_p (SCM x);
SCM minus (SCM x);
SCM plus (SCM x);
SCM divide (SCM x);
SCM modulo (SCM a, SCM b);
SCM multiply (SCM x);
SCM logand (SCM x);
SCM logior (SCM x);
SCM lognot (SCM x);
SCM logxor (SCM x);
SCM ash (SCM n, SCM count);
// src/mes.mes
SCM acons (SCM key, SCM value, SCM alist);
SCM add_formals (SCM formals, SCM x);
SCM append2 (SCM x, SCM y);
SCM append_reverse (SCM x, SCM y);
SCM arity_ (SCM x);
SCM assoc (SCM x, SCM a);
SCM assq (SCM x, SCM a);
SCM builtin_arity (SCM builtin);
SCM builtin_name(SCM builtin);
SCM builtin_p (SCM x);
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
SCM make_builtin_type ();
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
void builtin_printer(SCM builtin);

// src/module.mes
SCM make_module_type ();
SCM module_printer (SCM module);
SCM module_variable (SCM module, SCM name);
SCM module_ref (SCM module, SCM name);
SCM module_define_x (SCM module, SCM name, SCM value);
// src/posix.mes
SCM peek_byte ();
SCM read_byte ();
SCM unread_byte (SCM i);
SCM peek_char ();
SCM read_char (SCM port);
SCM unread_char (SCM i);
SCM write_char (SCM i);
SCM write_byte (SCM x);
SCM getenv_ (SCM s);
SCM setenv_ (SCM s, SCM v);
SCM access_p (SCM file_name, SCM mode);
SCM current_input_port ();
SCM open_input_file (SCM file_name);
SCM open_input_string (SCM string);
SCM set_current_input_port (SCM port);
SCM current_output_port ();
SCM current_error_port ();
SCM open_output_file (SCM x);
SCM set_current_output_port (SCM port);
SCM set_current_error_port (SCM port);
SCM force_output (SCM p);
SCM chmod_ (SCM file_name, SCM mode);
SCM isatty_p (SCM port);
SCM primitive_fork ();
SCM execl_ (SCM file_name, SCM args);
SCM waitpid_ (SCM pid, SCM options);
SCM current_time ();
SCM gettimeofday_ ();
SCM get_internal_run_time ();
SCM getcwd_ ();
SCM dup_ (SCM port);
SCM dup2_ (SCM old, SCM new);
SCM delete_file (SCM file_name);
// src/reader.mes
SCM read_input_file_env_ (SCM e, SCM a);
SCM read_input_file_env ();
SCM read_env (SCM a);
SCM reader_read_sexp (SCM c, SCM a);
SCM reader_read_character ();
SCM reader_read_binary ();
SCM reader_read_octal ();
SCM reader_read_hex ();
SCM reader_read_string ();
// src/strings.mes
SCM string_equal_p (SCM a, SCM b);
SCM symbol_to_string (SCM symbol);
SCM symbol_to_keyword (SCM symbol);
SCM keyword_to_string (SCM keyword);
SCM string_to_symbol (SCM string);
SCM make_symbol (SCM string);
SCM string_to_list (SCM string);
SCM list_to_string (SCM list);
SCM read_string (SCM port);
SCM string_append (SCM x);
SCM string_length (SCM string);
SCM string_ref (SCM str, SCM k);
// src/struct.mes
SCM make_struct (SCM type, SCM fields, SCM printer);
SCM struct_length (SCM x);
SCM struct_ref (SCM x, SCM i);
SCM struct_set_x (SCM x, SCM i, SCM e);
// src/vector.mes
SCM make_vector_ (SCM n);
SCM vector_length (SCM x);
SCM vector_ref (SCM x, SCM i);
SCM vector_entry (SCM x);
SCM vector_set_x (SCM x, SCM i, SCM e);
SCM list_to_vector (SCM x);
SCM vector_to_list (SCM v);
SCM init_time(SCM a);

SCM mes_builtins(SCM a)  ///((internal))
{
	// TODO minimal: cons, car, cdr, list, null_p, eq_p minus, plus
	// display_, display_error_, getenv
	SCM builtin_type = make_builtin_type();
	// src/gc.mes
	a = init_builtin(builtin_type, "gc-check", 0, (function1_t)&gc_check, a);
	a = init_builtin(builtin_type, "gc", 0, (function1_t)&gc, a);
	// src/hash.mes
	a = init_builtin(builtin_type, "hashq", 2, (function1_t)&hashq, a);
	a = init_builtin(builtin_type, "hash", 2, (function1_t)&hash, a);
	a = init_builtin(builtin_type, "hashq-get-handle", 3, (function1_t)&hashq_get_handle, a);
	a = init_builtin(builtin_type, "hashq-ref", 3, (function1_t)&hashq_ref, a);
	a = init_builtin(builtin_type, "hash-ref", 3, (function1_t)&hash_ref, a);
	a = init_builtin(builtin_type, "hashq-set!", 3, (function1_t)&hashq_set_x, a);
	a = init_builtin(builtin_type, "hash-set!", 3, (function1_t)&hash_set_x, a);
	a = init_builtin(builtin_type, "hash-table-printer", 1, (function1_t)&hash_table_printer, a);
	a = init_builtin(builtin_type, "make-hash-table", 1, (function1_t)&make_hash_table, a);
	// src/lib.mes
	a = init_builtin(builtin_type, "core:display", 1, (function1_t)&display_, a);
	a = init_builtin(builtin_type, "core:display-error", 1, (function1_t)&display_error_, a);
	a = init_builtin(builtin_type, "core:display-port", 2, (function1_t)&display_port_, a);
	a = init_builtin(builtin_type, "core:write", 1, (function1_t)&write_, a);
	a = init_builtin(builtin_type, "core:write-error", 1, (function1_t)&write_error_, a);
	a = init_builtin(builtin_type, "core:write-port", 2, (function1_t)&write_port_, a);
	a = init_builtin(builtin_type, "exit", 1, (function1_t)&exit_, a);
	a = init_builtin(builtin_type, "frame-printer", 1, (function1_t)&frame_printer, a);
	a = init_builtin(builtin_type, "make-stack", -1, (function1_t)&make_stack, a);
	a = init_builtin(builtin_type, "stack-length", 1, (function1_t)&stack_length, a);
	a = init_builtin(builtin_type, "stack-ref", 2, (function1_t)&stack_ref, a);
	a = init_builtin(builtin_type, "xassq", 2, (function1_t)&xassq, a);
	a = init_builtin(builtin_type, "memq", 2, (function1_t)&memq, a);
	a = init_builtin(builtin_type, "equal2?", 2, (function1_t)&equal2_p, a);
	a = init_builtin(builtin_type, "last-pair", 1, (function1_t)&last_pair, a);
	a = init_builtin(builtin_type, "pair?", 1, (function1_t)&pair_p, a);
	// src/math.mes
	a = init_builtin(builtin_type, ">", -1, (function1_t)&greater_p, a);
	a = init_builtin(builtin_type, "<", -1, (function1_t)&less_p, a);
	a = init_builtin(builtin_type, "=", -1, (function1_t)&is_p, a);
	a = init_builtin(builtin_type, "-", -1, (function1_t)&minus, a);
	a = init_builtin(builtin_type, "+", -1, (function1_t)&plus, a);
	a = init_builtin(builtin_type, "/", -1, (function1_t)&divide, a);
	a = init_builtin(builtin_type, "modulo", 2, (function1_t)&modulo, a);
	a = init_builtin(builtin_type, "*", -1, (function1_t)&multiply, a);
	a = init_builtin(builtin_type, "logand", -1, (function1_t)&logand, a);
	a = init_builtin(builtin_type, "logior", -1, (function1_t)&logior, a);
	a = init_builtin(builtin_type, "lognot", 1, (function1_t)&lognot, a);
	a = init_builtin(builtin_type, "logxor", -1, (function1_t)&logxor, a);
	a = init_builtin(builtin_type, "ash", 2, (function1_t)&ash, a);
	// src/mes.mes
	a = init_builtin(builtin_type, "core:make-cell", 3, (function1_t)&make_cell_, a);
	a = init_builtin(builtin_type, "core:type", 1, (function1_t)&type_, a);
	a = init_builtin(builtin_type, "core:car", 1, (function1_t)&car_, a);
	a = init_builtin(builtin_type, "core:cdr", 1, (function1_t)&cdr_, a);
	a = init_builtin(builtin_type, "cons", 2, (function1_t)&cons, a);
	a = init_builtin(builtin_type, "car", 1, (function1_t)&car, a);
	a = init_builtin(builtin_type, "cdr", 1, (function1_t)&cdr, a);
	a = init_builtin(builtin_type, "list", -1, (function1_t)&list, a);
	a = init_builtin(builtin_type, "null?", 1, (function1_t)&null_p, a);
	a = init_builtin(builtin_type, "eq?", 2, (function1_t)&eq_p, a);
	a = init_builtin(builtin_type, "values", -1, (function1_t)&values, a);
	a = init_builtin(builtin_type, "acons", 3, (function1_t)&acons, a);
	a = init_builtin(builtin_type, "length", 1, (function1_t)&length, a);
	a = init_builtin(builtin_type, "error", 2, (function1_t)&error, a);
	a = init_builtin(builtin_type, "append2", 2, (function1_t)&append2, a);
	a = init_builtin(builtin_type, "append-reverse", 2, (function1_t)&append_reverse, a);
	a = init_builtin(builtin_type, "core:reverse!", 2, (function1_t)&reverse_x_, a);
	a = init_builtin(builtin_type, "pairlis", 3, (function1_t)&pairlis, a);
	a = init_builtin(builtin_type, "assq", 2, (function1_t)&assq, a);
	a = init_builtin(builtin_type, "assoc", 2, (function1_t)&assoc, a);
	a = init_builtin(builtin_type, "set-car!", 2, (function1_t)&set_car_x, a);
	a = init_builtin(builtin_type, "set-cdr!", 2, (function1_t)&set_cdr_x, a);
	a = init_builtin(builtin_type, "set-env!", 3, (function1_t)&set_env_x, a);
	a = init_builtin(builtin_type, "macro-get-handle", 1, (function1_t)&macro_get_handle, a);
	a = init_builtin(builtin_type, "add-formals", 2, (function1_t)&add_formals, a);
	a = init_builtin(builtin_type, "eval-apply", 0, (function1_t)&eval_apply, a);
	a = init_builtin(builtin_type, "make-builtin-type", 0, (function1_t)&make_builtin_type, a);
	a = init_builtin(builtin_type, "make-builtin", 4, (function1_t)&make_builtin, a);
	a = init_builtin(builtin_type, "builtin-name", 1, (function1_t)&builtin_name, a);
	a = init_builtin(builtin_type, "builtin-arity", 1, (function1_t)&builtin_arity, a);
	a = init_builtin(builtin_type, "builtin?", 1, (function1_t)&builtin_p, a);
	a = init_builtin(builtin_type, "builtin-printer", 1, (function1_t)&builtin_printer, a);
	// src/module.mes
	a = init_builtin(builtin_type, "make-module-type", 0, (function1_t)&make_module_type, a);
	a = init_builtin(builtin_type, "module-printer", 1, (function1_t)&module_printer, a);
	a = init_builtin(builtin_type, "module-variable", 2, (function1_t)&module_variable, a);
	a = init_builtin(builtin_type, "module-ref", 2, (function1_t)&module_ref, a);
	a = init_builtin(builtin_type, "module-define!", 3, (function1_t)&module_define_x, a);
	// src/posix.mes
	a = init_builtin(builtin_type, "peek-byte", 0, (function1_t)&peek_byte, a);
	a = init_builtin(builtin_type, "read-byte", 0, (function1_t)&read_byte, a);
	a = init_builtin(builtin_type, "unread-byte", 1, (function1_t)&unread_byte, a);
	a = init_builtin(builtin_type, "peek-char", 0, (function1_t)&peek_char, a);
	a = init_builtin(builtin_type, "read-char", -1, (function1_t)&read_char, a);
	a = init_builtin(builtin_type, "unread-char", 1, (function1_t)&unread_char, a);
	a = init_builtin(builtin_type, "write-char", -1, (function1_t)&write_char, a);
	a = init_builtin(builtin_type, "write-byte", -1, (function1_t)&write_byte, a);
	a = init_builtin(builtin_type, "getenv", 1, (function1_t)&getenv_, a);
	a = init_builtin(builtin_type, "setenv", 2, (function1_t)&setenv_, a);
	a = init_builtin(builtin_type, "access?", 2, (function1_t)&access_p, a);
	a = init_builtin(builtin_type, "current-input-port", 0, (function1_t)&current_input_port, a);
	a = init_builtin(builtin_type, "open-input-file", 1, (function1_t)&open_input_file, a);
	a = init_builtin(builtin_type, "open-input-string", 1, (function1_t)&open_input_string, a);
	a = init_builtin(builtin_type, "set-current-input-port", 1, (function1_t)&set_current_input_port, a);
	a = init_builtin(builtin_type, "current-output-port", 0, (function1_t)&current_output_port, a);
	a = init_builtin(builtin_type, "current-error-port", 0, (function1_t)&current_error_port, a);
	a = init_builtin(builtin_type, "open-output-file", -1, (function1_t)&open_output_file, a);
	a = init_builtin(builtin_type, "set-current-output-port", 1, (function1_t)&set_current_output_port, a);
	a = init_builtin(builtin_type, "set-current-error-port", 1, (function1_t)&set_current_error_port, a);
	a = init_builtin(builtin_type, "force-output", -1, (function1_t)&force_output, a);
	a = init_builtin(builtin_type, "chmod", 2, (function1_t)&chmod_, a);
	a = init_builtin(builtin_type, "isatty?", 1, (function1_t)&isatty_p, a);
	a = init_builtin(builtin_type, "primitive-fork", 0, (function1_t)&primitive_fork, a);
	a = init_builtin(builtin_type, "execl", 2, (function1_t)&execl_, a);
	a = init_builtin(builtin_type, "core:waitpid", 2, (function1_t)&waitpid_, a);
	a = init_builtin(builtin_type, "current-time", 0, (function1_t)&current_time, a);
	a = init_builtin(builtin_type, "gettimeofday", 0, (function1_t)&gettimeofday_, a);
	a = init_builtin(builtin_type, "get-internal-run-time", 0, (function1_t)&get_internal_run_time, a);
	a = init_builtin(builtin_type, "getcwd", 0, (function1_t)&getcwd_, a);
	a = init_builtin(builtin_type, "dup", 1, (function1_t)&dup_, a);
	a = init_builtin(builtin_type, "dup2", 2, (function1_t)&dup2_, a);
	a = init_builtin(builtin_type, "delete-file", 1, (function1_t)&delete_file, a);
	// src/reader.mes
	a = init_builtin(builtin_type, "core:read-input-file-env", 2, (function1_t)&read_input_file_env_, a);
	a = init_builtin(builtin_type, "read-input-file-env", 1, (function1_t)&read_input_file_env, a);
	a = init_builtin(builtin_type, "read-env", 1, (function1_t)&read_env, a);
	a = init_builtin(builtin_type, "reader-read-sexp", 3, (function1_t)&reader_read_sexp, a);
	a = init_builtin(builtin_type, "reader-read-character", 0, (function1_t)&reader_read_character, a);
	a = init_builtin(builtin_type, "reader-read-binary", 0, (function1_t)&reader_read_binary, a);
	a = init_builtin(builtin_type, "reader-read-octal", 0, (function1_t)&reader_read_octal, a);
	a = init_builtin(builtin_type, "reader-read-hex", 0, (function1_t)&reader_read_hex, a);
	a = init_builtin(builtin_type, "reader-read-string", 0, (function1_t)&reader_read_string, a);
	// src/strings.mes
	a = init_builtin(builtin_type, "string=?", 2, (function1_t)&string_equal_p, a);
	a = init_builtin(builtin_type, "symbol->string", 1, (function1_t)&symbol_to_string, a);
	a = init_builtin(builtin_type, "symbol->keyword", 1, (function1_t)&symbol_to_keyword, a);
	a = init_builtin(builtin_type, "keyword->string", 1, (function1_t)&keyword_to_string, a);
	a = init_builtin(builtin_type, "string->symbol", 1, (function1_t)&string_to_symbol, a);
	a = init_builtin(builtin_type, "make-symbol", 1, (function1_t)&make_symbol, a);
	a = init_builtin(builtin_type, "string->list", 1, (function1_t)&string_to_list, a);
	a = init_builtin(builtin_type, "list->string", 1, (function1_t)&list_to_string, a);
	a = init_builtin(builtin_type, "read-string", -1, (function1_t)&read_string, a);
	a = init_builtin(builtin_type, "string-append", -1, (function1_t)&string_append, a);
	a = init_builtin(builtin_type, "string-length", 1, (function1_t)&string_length, a);
	a = init_builtin(builtin_type, "string-ref", 2, (function1_t)&string_ref, a);
	// src/struct.mes
	a = init_builtin(builtin_type, "make-struct", 3, (function1_t)&make_struct, a);
	a = init_builtin(builtin_type, "struct-length", 1, (function1_t)&struct_length, a);
	a = init_builtin(builtin_type, "struct-ref", 2, (function1_t)&struct_ref, a);
	a = init_builtin(builtin_type, "struct-set!", 3, (function1_t)&struct_set_x, a);
	// src/vector.mes
	a = init_builtin(builtin_type, "core:make-vector", 1, (function1_t)&make_vector_, a);
	a = init_builtin(builtin_type, "vector-length", 1, (function1_t)&vector_length, a);
	a = init_builtin(builtin_type, "vector-ref", 2, (function1_t)&vector_ref, a);
	a = init_builtin(builtin_type, "vector-entry", 1, (function1_t)&vector_entry, a);
	a = init_builtin(builtin_type, "vector-set!", 3, (function1_t)&vector_set_x, a);
	a = init_builtin(builtin_type, "list->vector", 1, (function1_t)&list_to_vector, a);
	a = init_builtin(builtin_type, "vector->list", 1, (function1_t)&vector_to_list, a);
	return a;
}
