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
void initialize_constants()
{
	cell_arrow = calloc(1, sizeof(struct scm));
	cell_begin = calloc(1, sizeof(struct scm));
	cell_call_with_current_continuation = calloc(1, sizeof(struct scm));
	cell_circular = calloc(1, sizeof(struct scm));
	cell_closure = calloc(1, sizeof(struct scm));
	cell_dot = calloc(1, sizeof(struct scm));
	cell_f = calloc(1, sizeof(struct scm));
	cell_nil = calloc(1, sizeof(struct scm));
	cell_symbol_arch = calloc(1, sizeof(struct scm));
	cell_symbol_argv = calloc(1, sizeof(struct scm));
	cell_symbol_begin = calloc(1, sizeof(struct scm));
	cell_symbol_boot_module = calloc(1, sizeof(struct scm));
	cell_symbol_buckets = calloc(1, sizeof(struct scm));
	cell_symbol_builtin = calloc(1, sizeof(struct scm));
	cell_symbol_call_with_current_continuation = calloc(1, sizeof(struct scm));
	cell_symbol_call_with_values = calloc(1, sizeof(struct scm));
	cell_symbol_car = calloc(1, sizeof(struct scm));
	cell_symbol_cdr = calloc(1, sizeof(struct scm));
	cell_symbol_compiler = calloc(1, sizeof(struct scm));
	cell_symbol_current_module = calloc(1, sizeof(struct scm));
	cell_symbol_define = calloc(1, sizeof(struct scm));
	cell_symbol_define_macro = calloc(1, sizeof(struct scm));
	cell_symbol_display = calloc(1, sizeof(struct scm));
	cell_symbol_dot = calloc(1, sizeof(struct scm));
	cell_symbol_frame = calloc(1, sizeof(struct scm));
	cell_symbol_hashq_table = calloc(1, sizeof(struct scm));
	cell_symbol_if = calloc(1, sizeof(struct scm));
	cell_symbol_internal_time_units_per_second = calloc(1, sizeof(struct scm));
	cell_symbol_lambda = calloc(1, sizeof(struct scm));
	cell_symbol_macro_expand = calloc(1, sizeof(struct scm));
	cell_symbol_mes_prefix = calloc(1, sizeof(struct scm));
	cell_symbol_mes_version = calloc(1, sizeof(struct scm));
	cell_symbol_module = calloc(1, sizeof(struct scm));
	cell_symbol_not_a_number = calloc(1, sizeof(struct scm));
	cell_symbol_not_a_pair = calloc(1, sizeof(struct scm));
	cell_symbol_pmatch_car = calloc(1, sizeof(struct scm));
	cell_symbol_pmatch_cdr = calloc(1, sizeof(struct scm));
	cell_symbol_portable_macro_expand = calloc(1, sizeof(struct scm));
	cell_symbol_primitive_load = calloc(1, sizeof(struct scm));
	cell_symbol_procedure = calloc(1, sizeof(struct scm));
	cell_symbol_quasiquote = calloc(1, sizeof(struct scm));
	cell_symbol_quasisyntax = calloc(1, sizeof(struct scm));
	cell_symbol_quote = calloc(1, sizeof(struct scm));
	cell_symbol_read_input_file = calloc(1, sizeof(struct scm));
	cell_symbol_record_type = calloc(1, sizeof(struct scm));
	cell_symbol_sc_expand = calloc(1, sizeof(struct scm));
	cell_symbol_sc_expander_alist = calloc(1, sizeof(struct scm));
	cell_symbol_set_x = calloc(1, sizeof(struct scm));
	cell_symbol_size = calloc(1, sizeof(struct scm));
	cell_symbol_stack = calloc(1, sizeof(struct scm));
	cell_symbol_syntax = calloc(1, sizeof(struct scm));
	cell_symbol_system_error = calloc(1, sizeof(struct scm));
	cell_symbol_test = calloc(1, sizeof(struct scm));
	cell_symbol_throw = calloc(1, sizeof(struct scm));
	cell_symbol_unbound_variable = calloc(1, sizeof(struct scm));
	cell_symbol_unquote = calloc(1, sizeof(struct scm));
	cell_symbol_unquote_splicing = calloc(1, sizeof(struct scm));
	cell_symbol_unsyntax = calloc(1, sizeof(struct scm));
	cell_symbol_unsyntax_splicing = calloc(1, sizeof(struct scm));
	cell_symbol_write = calloc(1, sizeof(struct scm));
	cell_symbol_wrong_number_of_args = calloc(1, sizeof(struct scm));
	cell_symbol_wrong_type_arg = calloc(1, sizeof(struct scm));
	cell_t = calloc(1, sizeof(struct scm));
	cell_test = calloc(1, sizeof(struct scm));
	cell_type_broken_heart = calloc(1, sizeof(struct scm));
	cell_type_bytes = calloc(1, sizeof(struct scm));
	cell_type_char = calloc(1, sizeof(struct scm));
	cell_type_closure = calloc(1, sizeof(struct scm));
	cell_type_continuation = calloc(1, sizeof(struct scm));
	cell_type_function = calloc(1, sizeof(struct scm));
	cell_type_keyword = calloc(1, sizeof(struct scm));
	cell_type_macro = calloc(1, sizeof(struct scm));
	cell_type_number = calloc(1, sizeof(struct scm));
	cell_type_pair = calloc(1, sizeof(struct scm));
	cell_type_port = calloc(1, sizeof(struct scm));
	cell_type_ref = calloc(1, sizeof(struct scm));
	cell_type_special = calloc(1, sizeof(struct scm));
	cell_type_string = calloc(1, sizeof(struct scm));
	cell_type_struct = calloc(1, sizeof(struct scm));
	cell_type_symbol = calloc(1, sizeof(struct scm));
	cell_type_values = calloc(1, sizeof(struct scm));
	cell_type_variable = calloc(1, sizeof(struct scm));
	cell_type_vector = calloc(1, sizeof(struct scm));
	cell_undefined = calloc(1, sizeof(struct scm));
	cell_unspecified = calloc(1, sizeof(struct scm));
	cell_vm_apply = calloc(1, sizeof(struct scm));
	cell_vm_apply2 = calloc(1, sizeof(struct scm));
	cell_vm_begin = calloc(1, sizeof(struct scm));
	cell_vm_begin_eval = calloc(1, sizeof(struct scm));
	cell_vm_begin_expand = calloc(1, sizeof(struct scm));
	cell_vm_begin_expand_eval = calloc(1, sizeof(struct scm));
	cell_vm_begin_expand_macro = calloc(1, sizeof(struct scm));
	cell_vm_begin_expand_primitive_load = calloc(1, sizeof(struct scm));
	cell_vm_begin_primitive_load = calloc(1, sizeof(struct scm));
	cell_vm_begin_read_input_file = calloc(1, sizeof(struct scm));
	cell_vm_call_with_current_continuation2 = calloc(1, sizeof(struct scm));
	cell_vm_call_with_values2 = calloc(1, sizeof(struct scm));
	cell_vm_eval = calloc(1, sizeof(struct scm));
	cell_vm_eval2 = calloc(1, sizeof(struct scm));
	cell_vm_eval_check_func = calloc(1, sizeof(struct scm));
	cell_vm_eval_define = calloc(1, sizeof(struct scm));
	cell_vm_eval_macro_expand_eval = calloc(1, sizeof(struct scm));
	cell_vm_eval_macro_expand_expand = calloc(1, sizeof(struct scm));
	cell_vm_eval_pmatch_car = calloc(1, sizeof(struct scm));
	cell_vm_eval_pmatch_cdr = calloc(1, sizeof(struct scm));
	cell_vm_eval_set_x = calloc(1, sizeof(struct scm));
	cell_vm_evlis = calloc(1, sizeof(struct scm));
	cell_vm_evlis2 = calloc(1, sizeof(struct scm));
	cell_vm_evlis3 = calloc(1, sizeof(struct scm));
	cell_vm_if = calloc(1, sizeof(struct scm));
	cell_vm_if_expr = calloc(1, sizeof(struct scm));
	cell_vm_macro_expand = calloc(1, sizeof(struct scm));
	cell_vm_macro_expand_car = calloc(1, sizeof(struct scm));
	cell_vm_macro_expand_cdr = calloc(1, sizeof(struct scm));
	cell_vm_macro_expand_define = calloc(1, sizeof(struct scm));
	cell_vm_macro_expand_define_macro = calloc(1, sizeof(struct scm));
	cell_vm_macro_expand_lambda = calloc(1, sizeof(struct scm));
	cell_vm_macro_expand_set_x = calloc(1, sizeof(struct scm));
	cell_vm_return = calloc(1, sizeof(struct scm));
}
