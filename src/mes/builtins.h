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

#ifndef __MES_BUILTINS_H
#define __MES_BUILTINS_H

/* src/builtins.c */
struct scm *make_builtin (struct scm *builtin_type, struct scm *name, struct scm *arity, struct scm *function);
struct scm *builtin_name (struct scm *builtin);
struct scm *builtin_arity (struct scm *builtin);
struct scm *builtin_p (struct scm *x);
struct scm *builtin_printer (struct scm *builtin);
/* src/core.c */
struct scm *car (struct scm *x);
struct scm *cdr (struct scm *x);
struct scm *list (struct scm *x);
struct scm *null_p (struct scm *x);
struct scm *eq_p (struct scm *x, struct scm *y);
struct scm *values (struct scm *x);
struct scm *acons (struct scm *key, struct scm *value, struct scm *alist);
struct scm *length (struct scm *x);
struct scm *error (struct scm *key, struct scm *x);
struct scm *append2 (struct scm *x, struct scm *y);
struct scm *append_reverse (struct scm *x, struct scm *y);
struct scm *reverse_x_ (struct scm *x, struct scm *t);
struct scm *assq (struct scm *x, struct scm *a);
struct scm *assoc (struct scm *x, struct scm *a);
/* src/display.c */
struct scm *display_ (struct scm *x);
struct scm *display_error_ (struct scm *x);
struct scm *display_port_ (struct scm *x, struct scm *p);
struct scm *write_ (struct scm *x);
struct scm *write_error_ (struct scm *x);
struct scm *write_port_ (struct scm *x, struct scm *p);
/* src/eval-apply.c */
struct scm *pairlis (struct scm *x, struct scm *y, struct scm *a);
struct scm *set_car_x (struct scm *x, struct scm *e);
struct scm *set_cdr_x (struct scm *x, struct scm *e);
struct scm *set_env_x (struct scm *x, struct scm *e, struct scm *a);
struct scm *add_formals (struct scm *formals, struct scm *x);
struct scm *eval_apply ();
/* src/gc.c */
struct scm *gc_stats ();
struct scm *cons (struct scm *x, struct scm *y);
struct scm *gc_check ();
struct scm *gc ();
/* src/hash.c */
struct scm *hashq (struct scm *x, struct scm *size);
struct scm *hash (struct scm *x, struct scm *size);
struct scm *hashq_get_handle (struct scm *table, struct scm *key, struct scm *dflt);
struct scm *hashq_ref (struct scm *table, struct scm *key, struct scm *dflt);
struct scm *hash_ref (struct scm *table, struct scm *key, struct scm *dflt);
struct scm *hashq_set_x (struct scm *table, struct scm *key, struct scm *value);
struct scm *hash_set_x (struct scm *table, struct scm *key, struct scm *value);
struct scm *hash_table_printer (struct scm *table);
struct scm *make_hash_table (struct scm *x);
/* src/lib.c */
struct scm *type_ (struct scm *x);
struct scm *car_ (struct scm *x);
struct scm *cdr_ (struct scm *x);
struct scm *xassq (struct scm *x, struct scm *a);
struct scm *memq (struct scm *x, struct scm *a);
struct scm *equal2_p (struct scm *a, struct scm *b);
struct scm *last_pair (struct scm *x);
struct scm *pair_p (struct scm *x);
struct scm *char_to_integer (struct scm *x);
struct scm *integer_to_char (struct scm *x);
/* src/math.c */
struct scm *greater_p (struct scm *x);
struct scm *less_p (struct scm *x);
struct scm *is_p (struct scm *x);
struct scm *minus (struct scm *x);
struct scm *plus (struct scm *x);
struct scm *divide (struct scm *x);
struct scm *modulo (struct scm *a, struct scm *b);
struct scm *multiply (struct scm *x);
struct scm *logand (struct scm *x);
struct scm *logior (struct scm *x);
struct scm *lognot (struct scm *x);
struct scm *logxor (struct scm *x);
struct scm *ash (struct scm *n, struct scm *count);
/* src/module.c */
struct scm *make_module_type ();
struct scm *module_printer (struct scm *module);
struct scm *module_variable (struct scm *module, struct scm *name);
struct scm *module_ref (struct scm *module, struct scm *name);
struct scm *module_define_x (struct scm *module, struct scm *name, struct scm *value);
/* src/posix.c */
struct scm *exit_ (struct scm *x);
struct scm *peek_byte ();
struct scm *read_byte ();
struct scm *unread_byte (struct scm *i);
struct scm *peek_char ();
struct scm *read_char (struct scm *port);
struct scm *unread_char (struct scm *i);
struct scm *write_char (struct scm *i);
struct scm *write_byte (struct scm *x);
struct scm *getenv_ (struct scm *s);
struct scm *setenv_ (struct scm *s, struct scm *v);
struct scm *access_p (struct scm *file_name, struct scm *mode);
struct scm *current_input_port ();
struct scm *open_input_file (struct scm *file_name);
struct scm *open_input_string (struct scm *string);
struct scm *set_current_input_port (struct scm *port);
struct scm *current_output_port ();
struct scm *current_error_port ();
struct scm *open_output_file (struct scm *x);
struct scm *set_current_output_port (struct scm *port);
struct scm *set_current_error_port (struct scm *port);
struct scm *chmod_ (struct scm *file_name, struct scm *mode);
struct scm *isatty_p (struct scm *port);
struct scm *primitive_fork ();
struct scm *execl_ (struct scm *file_name, struct scm *args);
struct scm *waitpid_ (struct scm *pid, struct scm *options);
struct scm *current_time ();
struct scm *gettimeofday_ ();
struct scm *get_internal_run_time ();
struct scm *getcwd_ ();
struct scm *dup_ (struct scm *port);
struct scm *dup2_ (struct scm *old, struct scm *new);
struct scm *delete_file (struct scm *file_name);
/* src/reader.c */
struct scm *read_input_file_env_ (struct scm *e, struct scm *a);
struct scm *read_input_file_env (struct scm *a);
struct scm *read_env (struct scm *a);
struct scm *reader_read_sexp (struct scm *c, struct scm *s, struct scm *a);
struct scm *reader_read_character ();
struct scm *reader_read_binary ();
struct scm *reader_read_octal ();
struct scm *reader_read_hex ();
struct scm *reader_read_string ();
/* src/stack.c */
struct scm *frame_printer (struct scm *frame);
struct scm *make_stack (struct scm *stack);
struct scm *stack_length (struct scm *stack);
struct scm *stack_ref (struct scm *stack, struct scm *index);
/* src/string.c */
struct scm *string_equal_p (struct scm *a, struct scm *b);
struct scm *symbol_to_string (struct scm *symbol);
struct scm *symbol_to_keyword (struct scm *symbol);
struct scm *keyword_to_string (struct scm *keyword);
struct scm *string_to_symbol (struct scm *string);
struct scm *make_symbol (struct scm *string);
struct scm *string_to_list (struct scm *string);
struct scm *list_to_string (struct scm *list);
struct scm *read_string (struct scm *port);
struct scm *string_append (struct scm *x);
struct scm *string_length (struct scm *string);
struct scm *string_ref (struct scm *str, struct scm *k);
/* src/struct.c */
struct scm *make_struct (struct scm *type, struct scm *fields, struct scm *printer);
struct scm *struct_length (struct scm *x);
struct scm *struct_ref (struct scm *x, struct scm *i);
struct scm *struct_set_x (struct scm *x, struct scm *i, struct scm *e);
/* src/vector.c */
struct scm *make_vector (struct scm *x);
struct scm *vector_length (struct scm *x);
struct scm *vector_ref (struct scm *x, struct scm *i);
struct scm *vector_entry (struct scm *x);
struct scm *vector_set_x (struct scm *x, struct scm *i, struct scm *e);
struct scm *list_to_vector (struct scm *x);
struct scm *vector_to_list (struct scm *v);

#endif /* __MES_BUILTINS_H */
