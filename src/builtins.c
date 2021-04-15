/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include "mes/mes.h"
#include <unistd.h>

struct scm *
make_builtin_type ()            /*:((internal)) */
{
  struct scm *record_type = cell_symbol_record_type;
  struct scm *fields = cell_nil;
  fields = cons (cstring_to_symbol ("address"), fields);
  fields = cons (cstring_to_symbol ("arity"), fields);
  fields = cons (cstring_to_symbol ("name"), fields);
  fields = cons (fields, cell_nil);
  fields = cons (cell_symbol_builtin, fields);
  return make_struct (record_type, fields, cell_unspecified);
}

struct scm *
make_builtin (struct scm *builtin_type, struct scm *name, struct scm *arity, struct scm *function)
{
  struct scm *values = cell_nil;
  values = cons (function, values);
  values = cons (arity, values);
  values = cons (name, values);
  values = cons (cell_symbol_builtin, values);
  return make_struct (builtin_type, values, cstring_to_symbol ("builtin-printer"));
}

struct scm *
builtin_name (struct scm *builtin)
{
  return struct_ref_ (builtin, 3);
}

struct scm *
builtin_arity (struct scm *builtin)
{
  return struct_ref_ (builtin, 4);
}

FUNCTION
builtin_function (struct scm *builtin)
{
  struct scm *x = struct_ref_ (builtin, 5);
  return x->function;
}

struct scm *
builtin_p (struct scm *x)
{
  if (x->type == TSTRUCT)
    if (struct_ref_ (x, 2) == cell_symbol_builtin)
      return cell_t;
  return cell_f;
}

struct scm *
builtin_printer (struct scm *builtin)
{
  fputs ("#<procedure ", __stdout);
  display_ (builtin_name (builtin));
  fputc (' ', __stdout);
  struct scm *x = builtin_arity (builtin);
  int arity = x->value;
  if (arity == -1)
    fputc ('_', __stdout);
  else
    {
      fputc ('(', __stdout);
      int i;
      for (i = 0; i < arity; i = i + 1)
        {
          if (i != 0)
            fputc (' ', __stdout);
          fputc ('_', __stdout);
        }
    }
  fputc ('>', __stdout);
}

struct scm *
init_builtin (struct scm *builtin_type, char const *name, int arity, void* function, struct scm *a)
{
  struct scm *s = cstring_to_symbol (name);
  return acons (s,
                make_builtin (builtin_type, symbol_to_string (s), make_number (arity),
                              make_function (function)), a);
}

struct scm *
mes_builtins (struct scm *a)            /*:((internal)) */
{
  struct scm *builtin_type = make_builtin_type ();

  if (g_mini != 0)
    {
      a = init_builtin (builtin_type, "cons", 2, &cons, a);
      a = init_builtin (builtin_type, "car", 1, &car, a);
      a = init_builtin (builtin_type, "list", -1, &list, a);
      a = init_builtin (builtin_type, "eq?", 2, &eq_p, a);
      a = init_builtin (builtin_type, "-", -1, &minus, a);
      a = init_builtin (builtin_type, "+", -1, &plus, a);
      a = init_builtin (builtin_type, "core:display", 1, &display_, a);
      a = init_builtin (builtin_type, "core:write", 1, &write_, a);
      a = init_builtin (builtin_type, "core:display-error", 1, &display_error_, a);
//      a = init_builtin (builtin_type, "getenv", 1, &getenv_, a);
      a = init_builtin (builtin_type, "gc", 0, &gc, a);
      a = init_builtin (builtin_type, ">", -1, &greater_p, a);
      a = init_builtin (builtin_type, "<", -1, &less_p, a);
      a = init_builtin (builtin_type, "make-vector", -1, &make_vector, a);
      return a;
    }

  /* src/builtins.c */
  a = init_builtin (builtin_type, "make-builtin", 4, &make_builtin, a);
  a = init_builtin (builtin_type, "builtin-name", 1, &builtin_name, a);
  a = init_builtin (builtin_type, "builtin-arity", 1, &builtin_arity, a);
  a = init_builtin (builtin_type, "builtin?", 1, &builtin_p, a);
  a = init_builtin (builtin_type, "builtin-printer", 1, &builtin_printer, a);
  /* src/core.c */
  a = init_builtin (builtin_type, "car", 1, &car, a);
  a = init_builtin (builtin_type, "cdr", 1, &cdr, a);
  a = init_builtin (builtin_type, "list", -1, &list, a);
  a = init_builtin (builtin_type, "null?", 1, &null_p, a);
  a = init_builtin (builtin_type, "eq?", 2, &eq_p, a);
  a = init_builtin (builtin_type, "values", -1, &values, a);
  a = init_builtin (builtin_type, "acons", 3, &acons, a);
  a = init_builtin (builtin_type, "length", 1, &length, a);
  a = init_builtin (builtin_type, "error", 2, &error, a);
  a = init_builtin (builtin_type, "append2", 2, &append2, a);
  a = init_builtin (builtin_type, "append-reverse", 2, &append_reverse, a);
  a = init_builtin (builtin_type, "core:reverse!", 2, &reverse_x_, a);
  a = init_builtin (builtin_type, "assq", 2, &assq, a);
  a = init_builtin (builtin_type, "assoc", 2, &assoc, a);
  /* src/display.c */
  a = init_builtin (builtin_type, "core:display", 1, &display_, a);
  a = init_builtin (builtin_type, "core:display-error", 1, &display_error_, a);
  a = init_builtin (builtin_type, "core:display-port", 2, &display_port_, a);
  a = init_builtin (builtin_type, "core:write", 1, &write_, a);
  a = init_builtin (builtin_type, "core:write-error", 1, &write_error_, a);
  a = init_builtin (builtin_type, "core:write-port", 2, &write_port_, a);
  /* src/eval-apply.c */
  a = init_builtin (builtin_type, "pairlis", 3, &pairlis, a);
  a = init_builtin (builtin_type, "set-car!", 2, &set_car_x, a);
  a = init_builtin (builtin_type, "set-cdr!", 2, &set_cdr_x, a);
  a = init_builtin (builtin_type, "set-env!", 3, &set_env_x, a);
  a = init_builtin (builtin_type, "add-formals", 2, &add_formals, a);
  a = init_builtin (builtin_type, "eval-apply", 0, &eval_apply, a);
  /* src/gc.c */
  a = init_builtin (builtin_type, "gc-stats", 0, &gc_stats, a);
  a = init_builtin (builtin_type, "cons", 2, &cons, a);
  a = init_builtin (builtin_type, "gc-check", 0, &gc_check, a);
  a = init_builtin (builtin_type, "gc", 0, &gc, a);
  /* src/hash.c */
  a = init_builtin (builtin_type, "hashq", 2, &hashq, a);
  a = init_builtin (builtin_type, "hash", 2, &hash, a);
  a = init_builtin (builtin_type, "hashq-get-handle", 3, &hashq_get_handle, a);
  a = init_builtin (builtin_type, "hashq-ref", 3, &hashq_ref, a);
  a = init_builtin (builtin_type, "hash-ref", 3, &hash_ref, a);
  a = init_builtin (builtin_type, "hashq-set!", 3, &hashq_set_x, a);
  a = init_builtin (builtin_type, "hash-set!", 3, &hash_set_x, a);
  a = init_builtin (builtin_type, "hash-table-printer", 1, &hash_table_printer, a);
  a = init_builtin (builtin_type, "make-hash-table", 1, &make_hash_table, a);
  /* src/lib.c */
  a = init_builtin (builtin_type, "core:type", 1, &type_, a);
  a = init_builtin (builtin_type, "core:car", 1, &car_, a);
  a = init_builtin (builtin_type, "core:cdr", 1, &cdr_, a);
  a = init_builtin (builtin_type, "xassq", 2, &xassq, a);
  a = init_builtin (builtin_type, "memq", 2, &memq, a);
  a = init_builtin (builtin_type, "equal2?", 2, &equal2_p, a);
  a = init_builtin (builtin_type, "last-pair", 1, &last_pair, a);
  a = init_builtin (builtin_type, "pair?", 1, &pair_p, a);
  a = init_builtin (builtin_type, "char->integer", 1, &char_to_integer, a);
  a = init_builtin (builtin_type, "integer->char", 1, &integer_to_char, a);
  /* src/math.c */
  a = init_builtin (builtin_type, ">", -1, &greater_p, a);
  a = init_builtin (builtin_type, "<", -1, &less_p, a);
  a = init_builtin (builtin_type, "=", -1, &is_p, a);
  a = init_builtin (builtin_type, "-", -1, &minus, a);
  a = init_builtin (builtin_type, "+", -1, &plus, a);
  a = init_builtin (builtin_type, "/", -1, &divide, a);
  a = init_builtin (builtin_type, "modulo", 2, &modulo, a);
  a = init_builtin (builtin_type, "*", -1, &multiply, a);
  a = init_builtin (builtin_type, "logand", -1, &logand, a);
  a = init_builtin (builtin_type, "logior", -1, &logior, a);
  a = init_builtin (builtin_type, "lognot", 1, &lognot, a);
  a = init_builtin (builtin_type, "logxor", -1, &logxor, a);
  a = init_builtin (builtin_type, "ash", 2, &ash, a);
  /* src/module.c */
  a = init_builtin (builtin_type, "make-module-type", 0, &make_module_type, a);
  a = init_builtin (builtin_type, "module-printer", 1, &module_printer, a);
  a = init_builtin (builtin_type, "module-variable", 2, &module_variable, a);
  a = init_builtin (builtin_type, "module-ref", 2, &module_ref, a);
  a = init_builtin (builtin_type, "module-define!", 3, &module_define_x, a);
  /* src/posix.c */

  a = init_builtin (builtin_type, "exit", 1, &exit_, a);
  a = init_builtin (builtin_type, "peek-byte", 0, &peek_byte, a);
  a = init_builtin (builtin_type, "read-byte", 0, &read_byte, a);
  a = init_builtin (builtin_type, "unread-byte", 1, &unread_byte, a);
  a = init_builtin (builtin_type, "peek-char", 0, &peek_char, a);
  a = init_builtin (builtin_type, "read-char", -1, &read_char, a);
  a = init_builtin (builtin_type, "unread-char", 1, &unread_char, a);
  a = init_builtin (builtin_type, "write-char", -1, &write_char, a);
  a = init_builtin (builtin_type, "write-byte", -1, &write_byte, a);
  a = init_builtin (builtin_type, "getenv", 1, &getenv_, a);
  a = init_builtin (builtin_type, "setenv", 2, &setenv_, a);
  a = init_builtin (builtin_type, "access?", 2, &access_p, a);
  a = init_builtin (builtin_type, "current-input-port", 0, &current_input_port, a);
  a = init_builtin (builtin_type, "open-input-file", 1, &open_input_file, a);
  a = init_builtin (builtin_type, "open-input-string", 1, &open_input_string, a);
  a = init_builtin (builtin_type, "set-current-input-port", 1, &set_current_input_port, a);
  a = init_builtin (builtin_type, "current-output-port", 0, &current_output_port, a);
  a = init_builtin (builtin_type, "current-error-port", 0, &current_error_port, a);
  a = init_builtin (builtin_type, "open-output-file", -1, &open_output_file, a);
  a = init_builtin (builtin_type, "set-current-output-port", 1, &set_current_output_port, a);
  a = init_builtin (builtin_type, "set-current-error-port", 1, &set_current_error_port, a);
  a = init_builtin (builtin_type, "chmod", 2, &chmod_, a);
  a = init_builtin (builtin_type, "isatty?", 1, &isatty_p, a);
  a = init_builtin (builtin_type, "primitive-fork", 0, &primitive_fork, a);
  a = init_builtin (builtin_type, "execl", 2, &execl_, a);
  a = init_builtin (builtin_type, "core:waitpid", 2, &waitpid_, a);
  a = init_builtin (builtin_type, "current-time", 0, &current_time, a);
  a = init_builtin (builtin_type, "gettimeofday", 0, &gettimeofday_, a);
  a = init_builtin (builtin_type, "get-internal-run-time", 0, &get_internal_run_time, a);
  a = init_builtin (builtin_type, "getcwd", 0, &getcwd_, a);
  a = init_builtin (builtin_type, "dup", 1, &dup_, a);
  a = init_builtin (builtin_type, "dup2", 2, &dup2_, a);
  a = init_builtin (builtin_type, "delete-file", 1, &delete_file, a);

  /* src/reader.c */
  a = init_builtin (builtin_type, "core:read-input-file-env", 2, &read_input_file_env_, a);
  a = init_builtin (builtin_type, "read-input-file-env", 1, &read_input_file_env, a);
  a = init_builtin (builtin_type, "read-env", 1, &read_env, a);
  a = init_builtin (builtin_type, "reader-read-sexp", 3, &reader_read_sexp, a);
  a = init_builtin (builtin_type, "reader-read-character", 0, &reader_read_character, a);
  a = init_builtin (builtin_type, "reader-read-binary", 0, &reader_read_binary, a);
  a = init_builtin (builtin_type, "reader-read-octal", 0, &reader_read_octal, a);
  a = init_builtin (builtin_type, "reader-read-hex", 0, &reader_read_hex, a);
  a = init_builtin (builtin_type, "reader-read-string", 0, &reader_read_string, a);
  /* src/stack.c */
  a = init_builtin (builtin_type, "frame-printer", 1, &frame_printer, a);
  a = init_builtin (builtin_type, "make-stack", -1, &make_stack, a);
  a = init_builtin (builtin_type, "stack-length", 1, &stack_length, a);
  a = init_builtin (builtin_type, "stack-ref", 2, &stack_ref, a);
  /* src/string.c */
  a = init_builtin (builtin_type, "string=?", 2, &string_equal_p, a);
  a = init_builtin (builtin_type, "symbol->string", 1, &symbol_to_string, a);
  a = init_builtin (builtin_type, "symbol->keyword", 1, &symbol_to_keyword, a);
  a = init_builtin (builtin_type, "keyword->string", 1, &keyword_to_string, a);
  a = init_builtin (builtin_type, "string->symbol", 1, &string_to_symbol, a);
  a = init_builtin (builtin_type, "make-symbol", 1, &make_symbol, a);
  a = init_builtin (builtin_type, "string->list", 1, &string_to_list, a);
  a = init_builtin (builtin_type, "list->string", 1, &list_to_string, a);
  a = init_builtin (builtin_type, "read-string", -1, &read_string, a);
  a = init_builtin (builtin_type, "string-append", -1, &string_append, a);
  a = init_builtin (builtin_type, "string-length", 1, &string_length, a);
  a = init_builtin (builtin_type, "string-ref", 2, &string_ref, a);
  /* src/struct.c */
  a = init_builtin (builtin_type, "make-struct", 3, &make_struct, a);
  a = init_builtin (builtin_type, "struct-length", 1, &struct_length, a);
  a = init_builtin (builtin_type, "struct-ref", 2, &struct_ref, a);
  a = init_builtin (builtin_type, "struct-set!", 3, &struct_set_x, a);
  /* src/vector.c */
  a = init_builtin (builtin_type, "make-vector", -1, &make_vector, a);
  a = init_builtin (builtin_type, "vector-length", 1, &vector_length, a);
  a = init_builtin (builtin_type, "vector-ref", 2, &vector_ref, a);
  a = init_builtin (builtin_type, "vector-entry", 1, &vector_entry, a);
  a = init_builtin (builtin_type, "vector-set!", 3, &vector_set_x, a);
  a = init_builtin (builtin_type, "list->vector", 1, &list_to_vector, a);
  a = init_builtin (builtin_type, "vector->list", 1, &vector_to_list, a);

  return a;
}
