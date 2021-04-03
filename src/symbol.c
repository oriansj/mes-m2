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

#include "mes/lib.h"
#include "mes/mes.h"

#include <string.h>

// char const *MES_VERSION = "0.22";

#if __M2_PLANET__
#define M2_CELL_SIZE 12
// CONSTANT M2_CELL_SIZE 12
#else
#define M2_CELL_SIZE 1
// CONSTANT M2_CELL_SIZE 12
#endif

struct scm *g_symbol;

struct scm *
init_symbol (struct scm *x, long type, char const *name)
{
  x->type = type;
  if (g_symbols == 0)
    g_free = g_free + M2_CELL_SIZE;
  else
    {
      int length = strlen (name);
      struct scm *string = make_string (name, length);
      x->car_value = length;
      x->cdr = string->string;
      hash_set_x (g_symbols, string, x);
    }
  g_symbol = g_symbol + M2_CELL_SIZE;
  return x;
}

void
init_symbols_ ()                  /*:((internal)) */
{
  g_symbol = cell_nil;
  cell_nil = init_symbol (g_symbol, TSPECIAL, "()");
  cell_f = init_symbol (g_symbol, TSPECIAL, "#f");
  cell_t = init_symbol (g_symbol, TSPECIAL, "#t");
  cell_dot = init_symbol (g_symbol, TSPECIAL, ".");
  cell_arrow = init_symbol (g_symbol, TSPECIAL, "=>");
  cell_undefined = init_symbol (g_symbol, TSPECIAL, "*undefined*");
  cell_unspecified = init_symbol (g_symbol, TSPECIAL, "*unspecified*");
  cell_closure = init_symbol (g_symbol, TSPECIAL, "*closure*");
  cell_circular = init_symbol (g_symbol, TSPECIAL, "*circular*");

  cell_vm_apply = init_symbol (g_symbol, TSPECIAL, "core:apply");
  cell_vm_apply2 = init_symbol (g_symbol, TSPECIAL, "*vm-apply2*");
  cell_vm_begin = init_symbol (g_symbol, TSPECIAL, "*vm-begin*");
  cell_vm_begin_eval = init_symbol (g_symbol, TSPECIAL, "*vm:begin-eval*");
  cell_vm_begin_expand = init_symbol (g_symbol, TSPECIAL, "core:eval");
  cell_vm_begin_expand_eval = init_symbol (g_symbol, TSPECIAL, "*vm:begin-expand-eval*");
  cell_vm_begin_expand_macro = init_symbol (g_symbol, TSPECIAL, "*vm:begin-expand-macro*");
  cell_vm_begin_expand_primitive_load = init_symbol (g_symbol, TSPECIAL, "*vm:core:begin-expand-primitive-load*");
  cell_vm_begin_primitive_load = init_symbol (g_symbol, TSPECIAL, "*vm:core:begin-primitive-load*");
  cell_vm_begin_read_input_file = init_symbol (g_symbol, TSPECIAL, "*vm-begin-read-input-file*");
  cell_vm_call_with_current_continuation2 = init_symbol (g_symbol, TSPECIAL, "*vm-call-with-current-continuation2*");
  cell_vm_call_with_values2 = init_symbol (g_symbol, TSPECIAL, "*vm-call-with-values2*");
  cell_vm_eval = init_symbol (g_symbol, TSPECIAL, "core:eval-expanded");
  cell_vm_eval2 = init_symbol (g_symbol, TSPECIAL, "*vm-eval2*");
  cell_vm_eval_check_func = init_symbol (g_symbol, TSPECIAL, "*vm-eval-check-func*");
  cell_vm_eval_define = init_symbol (g_symbol, TSPECIAL, "*vm-eval-define*");
  cell_vm_eval_macro_expand_eval = init_symbol (g_symbol, TSPECIAL, "*vm:eval-macro-expand-eval*");
  cell_vm_eval_macro_expand_expand = init_symbol (g_symbol, TSPECIAL, "*vm:eval-macro-expand-expand*");
  cell_vm_eval_pmatch_car = init_symbol (g_symbol, TSPECIAL, "*vm-eval-pmatch-car*");
  cell_vm_eval_pmatch_cdr = init_symbol (g_symbol, TSPECIAL, "*vm-eval-pmatch-cdr*");
  cell_vm_eval_set_x = init_symbol (g_symbol, TSPECIAL, "*vm-eval-set!*");
  cell_vm_evlis = init_symbol (g_symbol, TSPECIAL, "*vm-evlis*");
  cell_vm_evlis2 = init_symbol (g_symbol, TSPECIAL, "*vm-evlis2*");
  cell_vm_evlis3 = init_symbol (g_symbol, TSPECIAL, "*vm-evlis3*");
  cell_vm_if = init_symbol (g_symbol, TSPECIAL, "*vm-if*");
  cell_vm_if_expr = init_symbol (g_symbol, TSPECIAL, "*vm-if-expr*");
  cell_vm_macro_expand = init_symbol (g_symbol, TSPECIAL, "core:macro-expand");
  cell_vm_macro_expand_car = init_symbol (g_symbol, TSPECIAL, "*vm:core:macro-expand-car*");
  cell_vm_macro_expand_cdr = init_symbol (g_symbol, TSPECIAL, "*vm:macro-expand-cdr*");
  cell_vm_macro_expand_define = init_symbol (g_symbol, TSPECIAL, "*vm:core:macro-expand-define*");
  cell_vm_macro_expand_define_macro = init_symbol (g_symbol, TSPECIAL, "*vm:core:macro-expand-define-macro*");
  cell_vm_macro_expand_lambda = init_symbol (g_symbol, TSPECIAL, "*vm:core:macro-expand-lambda*");
  cell_vm_macro_expand_set_x = init_symbol (g_symbol, TSPECIAL, "*vm:core:macro-expand-set!*");
  cell_vm_return = init_symbol (g_symbol, TSPECIAL, "*vm-return*");

  cell_symbol_lambda = init_symbol (g_symbol, TSYMBOL, "lambda");
  cell_symbol_begin = init_symbol (g_symbol, TSYMBOL, "begin");
  cell_symbol_if = init_symbol (g_symbol, TSYMBOL, "if");
  cell_symbol_quote = init_symbol (g_symbol, TSYMBOL, "quote");
  cell_symbol_define = init_symbol (g_symbol, TSYMBOL, "define");
  cell_symbol_define_macro = init_symbol (g_symbol, TSYMBOL, "define-macro");
  cell_symbol_quasiquote = init_symbol (g_symbol, TSYMBOL, "quasiquote");
  cell_symbol_unquote = init_symbol (g_symbol, TSYMBOL, "unquote");
  cell_symbol_unquote_splicing = init_symbol (g_symbol, TSYMBOL, "unquote-splicing");
  cell_symbol_syntax = init_symbol (g_symbol, TSYMBOL, "syntax");
  cell_symbol_quasisyntax = init_symbol (g_symbol, TSYMBOL, "quasisyntax");
  cell_symbol_unsyntax = init_symbol (g_symbol, TSYMBOL, "unsyntax");
  cell_symbol_unsyntax_splicing = init_symbol (g_symbol, TSYMBOL, "unsyntax-splicing");
  cell_symbol_set_x = init_symbol (g_symbol, TSYMBOL, "set!");
  cell_symbol_sc_expand = init_symbol (g_symbol, TSYMBOL, "sc-expand");
  cell_symbol_macro_expand = init_symbol (g_symbol, TSYMBOL, "macro-expand");
  cell_symbol_portable_macro_expand = init_symbol (g_symbol, TSYMBOL, "portable-macro-expand");
  cell_symbol_sc_expander_alist = init_symbol (g_symbol, TSYMBOL, "*sc-expander-alist*");
  cell_symbol_call_with_values = init_symbol (g_symbol, TSYMBOL, "call-with-values");
  cell_symbol_call_with_current_continuation = init_symbol (g_symbol, TSYMBOL, "call-with-current-continuation");
  cell_symbol_boot_module = init_symbol (g_symbol, TSYMBOL, "boot-module");
  cell_symbol_current_module = init_symbol (g_symbol, TSYMBOL, "current-module");
  cell_symbol_primitive_load = init_symbol (g_symbol, TSYMBOL, "primitive-load");
  cell_symbol_car = init_symbol (g_symbol, TSYMBOL, "car");
  cell_symbol_cdr = init_symbol (g_symbol, TSYMBOL, "cdr");
  cell_symbol_not_a_number = init_symbol (g_symbol, TSYMBOL, "not-a-number");
  cell_symbol_not_a_pair = init_symbol (g_symbol, TSYMBOL, "not-a-pair");
  cell_symbol_system_error = init_symbol (g_symbol, TSYMBOL, "system-error");
  cell_symbol_throw = init_symbol (g_symbol, TSYMBOL, "throw");
  cell_symbol_unbound_variable = init_symbol (g_symbol, TSYMBOL, "unbound-variable");
  cell_symbol_wrong_number_of_args = init_symbol (g_symbol, TSYMBOL, "wrong-number-of-args");
  cell_symbol_wrong_type_arg = init_symbol (g_symbol, TSYMBOL, "wrong-type-arg");

  cell_symbol_buckets = init_symbol (g_symbol, TSYMBOL, "buckets");
  cell_symbol_builtin = init_symbol (g_symbol, TSYMBOL, "<builtin>");
  cell_symbol_frame = init_symbol (g_symbol, TSYMBOL, "<frame>");
  cell_symbol_hashq_table = init_symbol (g_symbol, TSYMBOL, "<hashq-table>");
  cell_symbol_module = init_symbol (g_symbol, TSYMBOL, "<module>");
  cell_symbol_procedure = init_symbol (g_symbol, TSYMBOL, "procedure");
  cell_symbol_record_type = init_symbol (g_symbol, TSYMBOL, "<record-type>");
  cell_symbol_size = init_symbol (g_symbol, TSYMBOL, "size");
  cell_symbol_stack = init_symbol (g_symbol, TSYMBOL, "<stack>");
  cell_symbol_argv = init_symbol (g_symbol, TSYMBOL, "%argv");
  cell_symbol_mes_datadir = init_symbol (g_symbol, TSYMBOL, "%datadir");
  cell_symbol_mes_version = init_symbol (g_symbol, TSYMBOL, "%version");
  cell_symbol_internal_time_units_per_second = init_symbol (g_symbol, TSYMBOL, "internal-time-units-per-second");
  cell_symbol_compiler = init_symbol (g_symbol, TSYMBOL, "%compiler");
  cell_symbol_arch = init_symbol (g_symbol, TSYMBOL, "%arch");
  cell_symbol_pmatch_car = init_symbol (g_symbol, TSYMBOL, "pmatch-car");
  cell_symbol_pmatch_cdr = init_symbol (g_symbol, TSYMBOL, "pmatch-cdr");

  cell_type_bytes = init_symbol (g_symbol, TSYMBOL, "<cell:bytes>");
  cell_type_char = init_symbol (g_symbol, TSYMBOL, "<cell:char>");
  cell_type_closure = init_symbol (g_symbol, TSYMBOL, "<cell:closure>");
  cell_type_continuation = init_symbol (g_symbol, TSYMBOL, "<cell:continuation>");
  cell_type_function = init_symbol (g_symbol, TSYMBOL, "<cell:function>");
  cell_type_keyword = init_symbol (g_symbol, TSYMBOL, "<cell:keyword>");
  cell_type_macro = init_symbol (g_symbol, TSYMBOL, "<cell:macro>");
  cell_type_number = init_symbol (g_symbol, TSYMBOL, "<cell:number>");
  cell_type_pair = init_symbol (g_symbol, TSYMBOL, "<cell:pair>");
  cell_type_port = init_symbol (g_symbol, TSYMBOL, "<cell:port>");
  cell_type_ref = init_symbol (g_symbol, TSYMBOL, "<cell:ref>");
  cell_type_special = init_symbol (g_symbol, TSYMBOL, "<cell:special>");
  cell_type_string = init_symbol (g_symbol, TSYMBOL, "<cell:string>");
  cell_type_struct = init_symbol (g_symbol, TSYMBOL, "<cell:struct>");
  cell_type_symbol = init_symbol (g_symbol, TSYMBOL, "<cell:symbol>");
  cell_type_values = init_symbol (g_symbol, TSYMBOL, "<cell:values>");
  cell_type_variable = init_symbol (g_symbol, TSYMBOL, "<cell:variable>");
  cell_type_vector = init_symbol (g_symbol, TSYMBOL, "<cell:vector>");
  cell_type_broken_heart = init_symbol (g_symbol, TSYMBOL, "<cell:broken-heart>");

  cell_symbol_program = init_symbol (g_symbol, TSYMBOL, "%program");
  cell_symbol_test = init_symbol (g_symbol, TSYMBOL, "%%test");
}

struct scm *
init_symbols ()                  /*:((internal)) */
{
  g_free = g_cells + M2_CELL_SIZE;

  g_symbols = 0;
  cell_nil = g_free;
  init_symbols_ ();
  g_symbol_max = g_symbol;
  g_symbols = make_hash_table_ (500);
  init_symbols_ ();
  g_ports = cell_nil;

  struct scm *a = cell_nil;
  a = acons (cell_symbol_call_with_values, cell_symbol_call_with_values, a);
  a = acons (cell_symbol_boot_module, cell_symbol_boot_module, a);
  a = acons (cell_symbol_current_module, cell_symbol_current_module, a);

  a = acons (cell_symbol_mes_version, make_string0 (MES_VERSION), a);
  a = acons (cell_symbol_mes_datadir, make_string0 (g_datadir), a);

  a = acons (cell_type_bytes, make_number (TBYTES), a);
  a = acons (cell_type_char, make_number (TCHAR), a);
  a = acons (cell_type_closure, make_number (TCLOSURE), a);
  a = acons (cell_type_continuation, make_number (TCONTINUATION), a);
  a = acons (cell_type_keyword, make_number (TKEYWORD), a);
  a = acons (cell_type_macro, make_number (TMACRO), a);
  a = acons (cell_type_number, make_number (TNUMBER), a);
  a = acons (cell_type_pair, make_number (TPAIR), a);
  a = acons (cell_type_port, make_number (TPORT), a);
  a = acons (cell_type_ref, make_number (TREF), a);
  a = acons (cell_type_special, make_number (TSPECIAL), a);
  a = acons (cell_type_string, make_number (TSTRING), a);
  a = acons (cell_type_struct, make_number (TSTRUCT), a);
  a = acons (cell_type_symbol, make_number (TSYMBOL), a);
  a = acons (cell_type_values, make_number (TVALUES), a);
  a = acons (cell_type_variable, make_number (TVARIABLE), a);
  a = acons (cell_type_vector, make_number (TVECTOR), a);
  a = acons (cell_type_broken_heart, make_number (TBROKEN_HEART), a);

  a = acons (cell_closure, a, a);

  return a;
}
