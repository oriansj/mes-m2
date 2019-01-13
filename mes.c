/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <fcntl.h>
#include <stdio.h>
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <libmes.h>

//#define MES_MINI 1
#if WITH_GLIBC
long ARENA_SIZE = 10000000; // 2.3GiB
#else
long ARENA_SIZE = 300000; // 32b: 3MiB, 64b: 6 MiB
#endif
long MAX_ARENA_SIZE = 100000000;
long STACK_SIZE = 20000;

long JAM_SIZE = 20000;
long GC_SAFETY = 2000;

long MAX_STRING = 524288;

char *g_arena = 0;
typedef long SCM;

int g_debug = 0;
long g_free = 0;

char *g_buf = 0;

SCM g_continuations = 0;
SCM g_symbols = 0;
SCM g_stack = 0;
SCM *g_stack_array = 0;
#define FRAME_SIZE 5
#define FRAME_PROCEDURE 4
// a/env
SCM r0 = 0;
// param 1
SCM r1 = 0;
// save 2
SCM r2 = 0;
// continuation
SCM r3 = 0;
// current-module
SCM m0 = 0;
// macro
SCM g_macros = 0;
SCM g_ports = 1;

// CONSTANT TBYTES         0
#define TBYTES             0
// CONSTANT TCHAR          1
#define TCHAR              1
// CONSTANT TCLOSURE       2
#define TCLOSURE           2
// CONSTANT TCONTINUATION  3
#define TCONTINUATION      3
// CONSTANT TKEYWORD       4
#define TKEYWORD           4
// CONSTANT TMACRO         5
#define TMACRO             5
// CONSTANT TNUMBER        6
#define TNUMBER            6
// CONSTANT TPAIR          7
#define TPAIR              7
// CONSTANT TPORT          8
#define TPORT              8
// CONSTANT TREF           9
#define TREF               9
// CONSTANT TSPECIAL      10
#define TSPECIAL          10
// CONSTANT TSTRING       11
#define TSTRING           11
// CONSTANT TSTRUCT       12
#define TSTRUCT           12
// CONSTANT TSYMBOL       13
#define TSYMBOL           13
// CONSTANT TVALUES       14
#define TVALUES           14
// CONSTANT TVARIABLE     15
#define TVARIABLE         15
// CONSTANT TVECTOR       16
#define TVECTOR           16
// CONSTANT TBROKEN_HEART 17
#define TBROKEN_HEART     17

#if __MESC__
typedef long function0_t;
typedef long function1_t;
typedef long function2_t;
typedef long function3_t;
typedef long functionn_t;
#else // !__MESC__
typedef SCM (*function0_t) (void);
typedef SCM (*function1_t) (SCM);
typedef SCM (*function2_t) (SCM, SCM);
typedef SCM (*function3_t) (SCM, SCM, SCM);
typedef SCM (*functionn_t) (SCM);
#endif // !__MESC__

struct scm
{
  long type;
  SCM car;
  SCM cdr;
};

#if __MESC__
//FIXME
char *foobar = 0;
struct scm *g_cells = foobar;
struct scm *g_news = foobar;
#else
struct scm *g_cells = 0;
struct scm *g_news = 0;
#endif

// CONSTANT cell_nil 1
#define cell_nil 1
// CONSTANT cell_f 2
#define cell_f 2
// CONSTANT cell_t 3
#define cell_t 3
// CONSTANT cell_dot 4
#define cell_dot 4
// CONSTANT cell_arrow 5
#define cell_arrow 5
// CONSTANT cell_undefined 6
#define cell_undefined 6
// CONSTANT cell_unspecified 7
#define cell_unspecified 7
// CONSTANT cell_closure 8
#define cell_closure 8
// CONSTANT cell_circular 9
#define cell_circular 9
// CONSTANT cell_begin 10
#define cell_begin 10
// CONSTANT cell_call_with_current_continuation 11
#define cell_call_with_current_continuation 11

// CONSTANT cell_vm_apply 12
#define cell_vm_apply 12
// CONSTANT cell_vm_apply2 13
#define cell_vm_apply2 13
// CONSTANT cell_vm_begin 14
#define cell_vm_begin 14
// CONSTANT cell_vm_begin_eval 15
#define cell_vm_begin_eval 15
// CONSTANT cell_vm_begin_expand 16
#define cell_vm_begin_expand 16
// CONSTANT cell_vm_begin_expand_eval 17
#define cell_vm_begin_expand_eval 17
// CONSTANT cell_vm_begin_expand_macro 18
#define cell_vm_begin_expand_macro 18
// CONSTANT cell_vm_begin_expand_primitive_load 19
#define cell_vm_begin_expand_primitive_load 19
// CONSTANT cell_vm_begin_primitive_load 20
#define cell_vm_begin_primitive_load 20
// CONSTANT cell_vm_begin_read_input_file 21
#define cell_vm_begin_read_input_file 21
// CONSTANT cell_vm_call_with_current_continuation2 22
#define cell_vm_call_with_current_continuation2 22
// CONSTANT cell_vm_call_with_values2 23
#define cell_vm_call_with_values2 23
// CONSTANT cell_vm_eval 24
#define cell_vm_eval 24
// CONSTANT cell_vm_eval2 25
#define cell_vm_eval2 25
// CONSTANT cell_vm_eval_check_func 26
#define cell_vm_eval_check_func 26
// CONSTANT cell_vm_eval_define 27
#define cell_vm_eval_define 27
// CONSTANT cell_vm_eval_macro_expand_eval 28
#define cell_vm_eval_macro_expand_eval 28
// CONSTANT cell_vm_eval_macro_expand_expand 29
#define cell_vm_eval_macro_expand_expand 29
// CONSTANT cell_vm_eval_pmatch_car 30
#define cell_vm_eval_pmatch_car 30
// CONSTANT cell_vm_eval_pmatch_cdr 31
#define cell_vm_eval_pmatch_cdr 31
// CONSTANT cell_vm_eval_set_x 32
#define cell_vm_eval_set_x 32
// CONSTANT cell_vm_evlis 33
#define cell_vm_evlis 33
// CONSTANT cell_vm_evlis2 34
#define cell_vm_evlis2 34
// CONSTANT cell_vm_evlis3 35
#define cell_vm_evlis3 35
// CONSTANT cell_vm_if 36
#define cell_vm_if 36
// CONSTANT cell_vm_if_expr 37
#define cell_vm_if_expr 37
// CONSTANT cell_vm_macro_expand 38
#define cell_vm_macro_expand 38
// CONSTANT cell_vm_macro_expand_car 39
#define cell_vm_macro_expand_car 39
// CONSTANT cell_vm_macro_expand_cdr 40
#define cell_vm_macro_expand_cdr 40
// CONSTANT cell_vm_macro_expand_define 41
#define cell_vm_macro_expand_define 41
// CONSTANT cell_vm_macro_expand_define_macro 42
#define cell_vm_macro_expand_define_macro 42
// CONSTANT cell_vm_macro_expand_lambda 43
#define cell_vm_macro_expand_lambda 43
// CONSTANT cell_vm_macro_expand_set_x 44
#define cell_vm_macro_expand_set_x 44
// CONSTANT cell_vm_return 45
#define cell_vm_return 45

// CONSTANT cell_symbol_dot 46
#define cell_symbol_dot 46
// CONSTANT cell_symbol_lambda 47
#define cell_symbol_lambda 47
// CONSTANT cell_symbol_begin 48
#define cell_symbol_begin 48
// CONSTANT cell_symbol_if 49
#define cell_symbol_if 49
// CONSTANT cell_symbol_quote 50
#define cell_symbol_quote 50
// CONSTANT cell_symbol_define 51
#define cell_symbol_define 51
// CONSTANT cell_symbol_define_macro 52
#define cell_symbol_define_macro 52

// CONSTANT cell_symbol_quasiquote 53
#define cell_symbol_quasiquote 53
// CONSTANT cell_symbol_unquote 54
#define cell_symbol_unquote 54
// CONSTANT cell_symbol_unquote_splicing 55
#define cell_symbol_unquote_splicing 55
// CONSTANT cell_symbol_syntax 56
#define cell_symbol_syntax 56
// CONSTANT cell_symbol_quasisyntax 57
#define cell_symbol_quasisyntax 57
// CONSTANT cell_symbol_unsyntax 58
#define cell_symbol_unsyntax 58
// CONSTANT cell_symbol_unsyntax_splicing 59
#define cell_symbol_unsyntax_splicing 59

// CONSTANT cell_symbol_set_x 60
#define cell_symbol_set_x 60

// CONSTANT cell_symbol_sc_expand 61
#define cell_symbol_sc_expand 61
// CONSTANT cell_symbol_macro_expand 62
#define cell_symbol_macro_expand 62
// CONSTANT cell_symbol_portable_macro_expand 63
#define cell_symbol_portable_macro_expand 63
// CONSTANT cell_symbol_sc_expander_alist 64
#define cell_symbol_sc_expander_alist 64

// CONSTANT cell_symbol_call_with_values 65
#define cell_symbol_call_with_values 65
// CONSTANT cell_symbol_call_with_current_continuation 66
#define cell_symbol_call_with_current_continuation 66
// CONSTANT cell_symbol_boot_module 67
#define cell_symbol_boot_module 67
// CONSTANT cell_symbol_current_module 68
#define cell_symbol_current_module 68
// CONSTANT cell_symbol_primitive_load 69
#define cell_symbol_primitive_load 69
// CONSTANT cell_symbol_read_input_file 70
#define cell_symbol_read_input_file 70
// CONSTANT cell_symbol_write 71
#define cell_symbol_write 71
// CONSTANT cell_symbol_display 72
#define cell_symbol_display 72

// CONSTANT cell_symbol_car 73
#define cell_symbol_car 73
// CONSTANT cell_symbol_cdr 74
#define cell_symbol_cdr 74
// CONSTANT cell_symbol_not_a_number 75
#define cell_symbol_not_a_number 75
// CONSTANT cell_symbol_not_a_pair 76
#define cell_symbol_not_a_pair 76
// CONSTANT cell_symbol_system_error 77
#define cell_symbol_system_error 77
// CONSTANT cell_symbol_throw 78
#define cell_symbol_throw 78
// CONSTANT cell_symbol_unbound_variable 79
#define cell_symbol_unbound_variable 79
// CONSTANT cell_symbol_wrong_number_of_args 80
#define cell_symbol_wrong_number_of_args 80
// CONSTANT cell_symbol_wrong_type_arg 81
#define cell_symbol_wrong_type_arg 81

// CONSTANT cell_symbol_buckets 82
#define cell_symbol_buckets 82
// CONSTANT cell_symbol_builtin 83
#define cell_symbol_builtin 83
// CONSTANT cell_symbol_frame 84
#define cell_symbol_frame 84
// CONSTANT cell_symbol_hashq_table 85
#define cell_symbol_hashq_table 85
// CONSTANT cell_symbol_module 86
#define cell_symbol_module 86
// CONSTANT cell_symbol_procedure 87
#define cell_symbol_procedure 87
// CONSTANT cell_symbol_record_type 88
#define cell_symbol_record_type 88
// CONSTANT cell_symbol_size 89
#define cell_symbol_size 89
// CONSTANT cell_symbol_stack 90
#define cell_symbol_stack 90

// CONSTANT cell_symbol_argv 91
#define cell_symbol_argv 91
// CONSTANT cell_symbol_mes_prefix 92
#define cell_symbol_mes_prefix 92
// CONSTANT cell_symbol_mes_version 93
#define cell_symbol_mes_version 93

// CONSTANT cell_symbol_internal_time_units_per_second 94
#define cell_symbol_internal_time_units_per_second 94
// CONSTANT cell_symbol_compiler 95
#define cell_symbol_compiler 95
// CONSTANT cell_symbol_arch 96
#define cell_symbol_arch 96
// CONSTANT cell_symbol_pmatch_car 97
#define cell_symbol_pmatch_car 97
// CONSTANT cell_symbol_pmatch_cdr 98
#define cell_symbol_pmatch_cdr 98

// CONSTANT cell_type_bytes 99
#define cell_type_bytes 99
// CONSTANT cell_type_char 100
#define cell_type_char 100
// CONSTANT cell_type_closure 101
#define cell_type_closure 101
// CONSTANT cell_type_continuation 102
#define cell_type_continuation 102
// CONSTANT cell_type_function 103
#define cell_type_function 103
// CONSTANT cell_type_keyword 104
#define cell_type_keyword 104
// CONSTANT cell_type_macro 105
#define cell_type_macro 105
// CONSTANT cell_type_number 106
#define cell_type_number 106
// CONSTANT cell_type_pair 107
#define cell_type_pair 107
// CONSTANT cell_type_port 108
#define cell_type_port 108
// CONSTANT cell_type_ref 109
#define cell_type_ref 109
// CONSTANT cell_type_special 110
#define cell_type_special 110
// CONSTANT cell_type_string 111
#define cell_type_string 111
// CONSTANT cell_type_struct 112
#define cell_type_struct 112
// CONSTANT cell_type_symbol 113
#define cell_type_symbol 113
// CONSTANT cell_type_values 114
#define cell_type_values 114
// CONSTANT cell_type_variable 115
#define cell_type_variable 115
// CONSTANT cell_type_vector 116
#define cell_type_vector 116
// CONSTANT cell_type_broken_heart 117
#define cell_type_broken_heart 117

// CONSTANT cell_test 118
#define cell_test 118

#include "builtins.h"

#define TYPE(x) g_cells[x].type
#define CAR(x) g_cells[x].car
#define CDR(x) g_cells[x].cdr

#define NTYPE(x) g_news[x].type
#define NCAR(x) g_news[x].car
#define NCDR(x) g_news[x].cdr

#define BYTES(x) g_cells[x].car
#define LENGTH(x) g_cells[x].car
#define REF(x) g_cells[x].car
#define START(x) (g_cells[x].car >> 16)
#define LEN(x) (g_cells[x].car & 0xffff)
#define VARIABLE(x) g_cells[x].car

#define CLOSURE(x) g_cells[x].cdr
#define CONTINUATION(x) g_cells[x].cdr

#define CBYTES(x) (char*)&g_cells[x].cdr
#define CSTRING_STRUCT(x) (char*)&g_cells[x.cdr].cdr

#define MACRO(x) g_cells[x].car
#define NAME(x) g_cells[x].cdr
#define PORT(x) g_cells[x].car
#define STRING(x) g_cells[x].cdr
#define STRUCT(x) g_cells[x].cdr
#define VALUE(x) g_cells[x].cdr
#define VECTOR(x) g_cells[x].cdr

#define NLENGTH(x) g_news[x].car
#define NCBYTES(x) (char*)&g_news[x].cdr
#define NVALUE(x) g_news[x].cdr
#define NSTRING(x) g_news[x].cdr
#define NVECTOR(x) g_news[x].cdr

#define CSTRING(x) CBYTES (STRING (x))

#define MAKE_BYTES0(x) make_bytes (x, strlen (x))
#define NAME_SYMBOL(symbol,name) {size_t s = strlen (name); CAR (symbol) = s; CDR (symbol) = make_bytes (name, s);}

#define MAKE_CHAR(n) make_cell__ (TCHAR, 0, n)
#define MAKE_CONTINUATION(n) make_cell__ (TCONTINUATION, n, g_stack)
#define MAKE_NUMBER(n) make_cell__ (TNUMBER, 0, (long)n)
#define MAKE_REF(n) make_cell__ (TREF, n, 0)
#define MAKE_STRING0(x) make_string (x, strlen (x))
#define MAKE_STRING_PORT(x) make_cell__ (TPORT, -length__ (g_ports) - 2, x)
#define MAKE_MACRO(name, x) make_cell__ (TMACRO, x, STRING (name))

#define CAAR(x) CAR (CAR (x))
#define CADR(x) CAR (CDR (x))
#define CDAR(x) CDR (CAR (x))
#define CDDR(x) CDR (CDR (x))
#define CADAR(x) CAR (CDR (CAR (x)))
#define CADDR(x) CAR (CDR (CDR (x)))
#define CDADAR(x) CAR (CDR (CAR (CDR (x))))

SCM apply_builtin (SCM fn, SCM x);
SCM cstring_to_list (char const* s);
SCM cstring_to_symbol (char const *s);
SCM make_bytes (char const* s, size_t length);
SCM make_hash_table_ (long size);
SCM read_input_file_env (SCM);
SCM string_equal_p (SCM a, SCM b);

SCM
alloc (long n)
{
  SCM x = g_free;
  g_free += n;
  if (g_free > ARENA_SIZE)
    assert (!"alloc: out of memory");
  return x;
}

SCM
make_cell__ (long type, SCM car, SCM cdr)
{
  SCM x = alloc (1);
  TYPE (x) = type;
  CAR (x) = car;
  CDR (x) = cdr;
  return x;
}

SCM
make_cell_ (SCM type, SCM car, SCM cdr)
{
  assert (TYPE (type) == TNUMBER);
  long t = VALUE (type);
  if (t == TCHAR || t == TNUMBER)
    return make_cell__ (t, car ? CAR (car) : 0, cdr ? CDR (cdr) : 0);
  return make_cell__ (t, car, cdr);
}

SCM
assoc_string (SCM x, SCM a) ///((internal))
{
  while (a != cell_nil && (TYPE (CAAR (a)) != TSTRING
                           || string_equal_p (x, CAAR (a)) == cell_f))
    a = CDR (a);
  return a != cell_nil ? CAR (a) : cell_f;
}

SCM
type_ (SCM x)
{
  return MAKE_NUMBER (TYPE (x));
}

// SCM
// car_to_cell_ (SCM x)
// {
//   return CAR (x);
// }

// SCM
// cdr_to_cell_ (SCM x)
// {
//   return CDR (x);
// }

// SCM
// car_to_number_ (SCM x)
// {
//   return MAKE_NUMBER (CAR (x));
// }

// SCM
// cdr_to_number_ (SCM x)
// {
//   return MAKE_NUMBER (CDR (x));
// }

SCM
car_ (SCM x)
{
  return (TYPE (x) != TCONTINUATION
          && (TYPE (CAR (x)) == TPAIR // FIXME: this is weird
              || TYPE (CAR (x)) == TREF
              || TYPE (CAR (x)) == TSPECIAL
              || TYPE (CAR (x)) == TSYMBOL
              || TYPE (CAR (x)) == TSTRING)) ? CAR (x) : MAKE_NUMBER (CAR (x));
}

SCM
cdr_ (SCM x)
{
  return (TYPE (x) != TCHAR
          && TYPE (x) != TNUMBER
          && TYPE (x) != TPORT
          && (TYPE (CDR (x)) == TPAIR
              || TYPE (CDR (x)) == TREF
              || TYPE (CDR (x)) == TSPECIAL
              || TYPE (CDR (x)) == TSYMBOL
              || TYPE (CDR (x)) == TSTRING)) ? CDR (x) : MAKE_NUMBER (CDR (x));
}

SCM
cons (SCM x, SCM y)
{
  return make_cell__ (TPAIR, x, y);
}

SCM
car (SCM x)
{
#if !__MESC_MES__
  if (TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cell_symbol_car));
#endif
  return CAR (x);
}

SCM
cdr (SCM x)
{
#if !__MESC_MES__
  if (TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cell_symbol_cdr));
#endif
  return CDR (x);
}

SCM
list (SCM x) ///((arity . n))
{
  return x;
}

SCM
null_p (SCM x)
{
  return x == cell_nil ? cell_t : cell_f;
}

SCM
eq_p (SCM x, SCM y)
{
  return (x == y
          || ((TYPE (x) == TKEYWORD && TYPE (y) == TKEYWORD
               && string_equal_p (x, y) == cell_t))
          || (TYPE (x) == TCHAR && TYPE (y) == TCHAR
              && VALUE (x) == VALUE (y))
          || (TYPE (x) == TNUMBER && TYPE (y) == TNUMBER
              && VALUE (x) == VALUE (y)))
    ? cell_t : cell_f;
}

SCM
values (SCM x) ///((arity . n))
{
  SCM v = cons (0, x);
  TYPE (v) = TVALUES;
  return v;
}

SCM
acons (SCM key, SCM value, SCM alist)
{
  return cons (cons (key, value), alist);
}

long
length__ (SCM x) ///((internal))
{
  long n = 0;
  while (x != cell_nil)
    {
      n++;
      if (TYPE (x) != TPAIR)
        return -1;
      x = CDR (x);
    }
  return n;
}

SCM
length (SCM x)
{
  return MAKE_NUMBER (length__ (x));
}

SCM apply (SCM, SCM, SCM);

SCM
error (SCM key, SCM x)
{
#if !__MESC_MES__
  SCM throw;
  if ((throw = module_ref (r0, cell_symbol_throw)) != cell_undefined)
    return apply (throw, cons (key, cons (x, cell_nil)), r0);
#endif
  display_error_ (key);
  eputs (": ");
  write_error_ (x);
  eputs ("\n");
  assert (0);
  exit (1);
}

//  extra lib
SCM
assert_defined (SCM x, SCM e) ///((internal))
{
  if (e == cell_undefined)
    return error (cell_symbol_unbound_variable, x);
  return e;
}

SCM make_string (char const* s, size_t length);

SCM
check_formals (SCM f, SCM formals, SCM args) ///((internal))
{
  long flen = (TYPE (formals) == TNUMBER) ? VALUE (formals) : length__ (formals);
  long alen = length__ (args);
  if (alen != flen && alen != -1 && flen != -1)
    {
      char *s = "apply: wrong number of arguments; expected: ";
      eputs (s);
      eputs (itoa (flen));
      eputs (", got: ");
      eputs (itoa (alen));
      eputs ("\n");
      write_error_ (f);
      SCM e = MAKE_STRING0 (s);
      return error (cell_symbol_wrong_number_of_args, cons (e, f));
    }
  return cell_unspecified;
}

SCM
check_apply (SCM f, SCM e) ///((internal))
{
  char* type = 0;
  if (f == cell_f || f == cell_t)
    type = "bool";
  if (f == cell_nil)
    type = "nil";
  if (f == cell_unspecified)
    type = "*unspecified*";
  if (f == cell_undefined)
    type = "*undefined*";
  if (TYPE (f) == TCHAR)
    type = "char";
  if (TYPE (f) == TNUMBER)
    type = "number";
  if (TYPE (f) == TSTRING)
    type = "string";
  if (TYPE (f) == TSTRUCT
      && builtin_p (f) == cell_f)
    type = "#<...>";
  if (TYPE (f) == TBROKEN_HEART)
    type = "<3";

  if (type)
    {
      char *s = "cannot apply: ";
      eputs (s);
      eputs (type);
      eputs ("[");
      write_error_ (e);
      eputs ("]\n");
      SCM e = MAKE_STRING0 (s);
      return error (cell_symbol_wrong_type_arg, cons (e, f));
    }
  return cell_unspecified;
}

SCM
gc_push_frame () ///((internal))
{
  if (g_stack < 5)
    assert (!"STACK FULL");
  g_stack_array[--g_stack] = cell_f;
  g_stack_array[--g_stack] = r0;
  g_stack_array[--g_stack] = r1;
  g_stack_array[--g_stack] = r2;
  g_stack_array[--g_stack] = r3;
  return g_stack;
}

SCM
gc_peek_frame () ///((internal))
{
  r3 = g_stack_array[g_stack];
  r2 = g_stack_array[g_stack+1];
  r1 = g_stack_array[g_stack+2];
  r0 = g_stack_array[g_stack+3];
  return g_stack_array[g_stack+FRAME_PROCEDURE];
}

SCM
gc_pop_frame () ///((internal))
{
  SCM x = gc_peek_frame ();
  g_stack += 5;
  return x;
}

SCM
append2 (SCM x, SCM y)
{
  if (x == cell_nil)
    return y;
  if (TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cstring_to_symbol ("append2")));
  SCM r = cell_nil;
  while (x != cell_nil)
    {
      r = cons (CAR (x), r);
      x = CDR (x);
    }
  return reverse_x_ (r, y);
}

SCM
append_reverse (SCM x, SCM y)
{
  if (x == cell_nil)
    return y;
  if (TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cstring_to_symbol ("append-reverse")));
  while (x != cell_nil)
    {
      y = cons (CAR (x), y);
      x = CDR (x);
    }
  return y;
}

SCM
reverse_x_ (SCM x, SCM t)
{
  if (x != cell_nil && TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cstring_to_symbol ("core:reverse!")));
  SCM r = t;
  while (x != cell_nil)
    {
      t = CDR (x);
      CDR (x) = r;
      r = x;
      x = t;
    }
  return r;
}

SCM
pairlis (SCM x, SCM y, SCM a)
{
  if (x == cell_nil)
    return a;
  if (TYPE (x) != TPAIR)
    return cons (cons (x, y), a);
  return cons (cons (car (x), car (y)),
               pairlis (cdr (x), cdr (y), a));
}

SCM
assq (SCM x, SCM a)
{
  if (TYPE (a) != TPAIR)
    return cell_f;
  int t = TYPE (x);
  if (t == TSYMBOL
      || t == TSPECIAL)
    while (a != cell_nil && x != CAAR (a))
      a = CDR (a);
  else if (t == TCHAR
           || t == TNUMBER)
      {
        SCM v = VALUE (x);
        while (a != cell_nil && v != VALUE (CAAR (a)))
          a = CDR (a);
      }
  else if (t == TKEYWORD)
    {
      while (a != cell_nil && string_equal_p (x, CAAR (a)) == cell_f)
        a = CDR (a);
    }
  else
    /* pointer equality, e.g. on strings. */
    while (a != cell_nil && x != CAAR (a))
      a = CDR (a);
  return a != cell_nil ? CAR (a) : cell_f;
}

SCM
assoc (SCM x, SCM a)
{
  if (TYPE (x) == TSTRING)
    return assoc_string (x, a);
  while (a != cell_nil && equal2_p (x, CAAR (a)) == cell_f)
    a = CDR (a);
  return a != cell_nil ? CAR (a) : cell_f;
}

SCM
set_car_x (SCM x, SCM e)
{
  if (TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cstring_to_symbol ("set-car!")));
  CAR (x) = e;
  return cell_unspecified;
}

SCM
set_cdr_x (SCM x, SCM e)
{
  if (TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cstring_to_symbol ("set-cdr!")));
  CDR (x) = e;
  return cell_unspecified;
}

SCM
set_env_x (SCM x, SCM e, SCM a)
{
  SCM p;
  if (TYPE (x) == TVARIABLE)
    p = VARIABLE (x);
  else
    p = assert_defined (x, module_variable (a, x));
  if (TYPE (p) != TPAIR)
    error (cell_symbol_not_a_pair, cons (p, x));
  return set_cdr_x (p, e);
}

SCM
call_lambda (SCM e, SCM x, SCM aa, SCM a) ///((internal))
{
  SCM cl = cons (cons (cell_closure, x), x);
  r1 = e;
  r0 = cl;
  return cell_unspecified;
}

SCM
make_closure_ (SCM args, SCM body, SCM a) ///((internal))
{
  return make_cell__ (TCLOSURE, cell_f, cons (cons (cell_circular, a), cons (args, body)));
}

SCM
make_variable_ (SCM var) ///((internal))
{
  return make_cell__ (TVARIABLE, var, 0);
}

SCM
macro_get_handle (SCM name)
{
  if (TYPE (name) == TSYMBOL)
    return hashq_get_handle (g_macros, name, cell_nil);
  return cell_f;
}

SCM
get_macro (SCM name) ///((internal))
{
  SCM m = macro_get_handle (name);
  if (m != cell_f)
    return MACRO (CDR (m));
  return cell_f;
}

SCM
macro_set_x (SCM name, SCM value) ///((internal))
{
  return hashq_set_x (g_macros, name, value);
}

SCM
push_cc (SCM p1, SCM p2, SCM a, SCM c) ///((internal))
{
  SCM x = r3;
  r3 = c;
  r2 = p2;
  gc_push_frame ();
  r1 = p1;
  r0 = a;
  r3 = x;
  return cell_unspecified;
}

SCM
add_formals (SCM formals, SCM x)
{
  while (TYPE (x) == TPAIR)
    {
      formals = cons (CAR (x), formals);
      x = CDR (x);
    }
  if (TYPE (x) == TSYMBOL)
    formals = cons (x, formals);
  return formals;
}

int
formal_p (SCM x, SCM formals) /// ((internal))
{
  if (TYPE (formals) == TSYMBOL)
    {
      if (x == formals)
        return x;
      else return cell_f;
    }
  while (TYPE (formals) == TPAIR && CAR (formals) != x)
    formals = CDR (formals);
  if (TYPE (formals) == TSYMBOL)
    return formals == x;
  return TYPE (formals) == TPAIR;
}

SCM
expand_variable_ (SCM x, SCM formals, int top_p) ///((internal))
{
  while (TYPE (x) == TPAIR)
    {
      if (TYPE (CAR (x)) == TPAIR)
        {
          if (CAAR (x) == cell_symbol_lambda)
            {
              SCM f = CAR (CDAR (x));
              formals = add_formals (formals, f);
            }
          else if (CAAR (x) == cell_symbol_define
                   || CAAR (x) == cell_symbol_define_macro)
            {
              SCM f = CAR (CDAR (x));
              formals = add_formals (formals, f);
            }
          if (CAAR (x) != cell_symbol_quote)
            expand_variable_ (CAR (x), formals, 0);
        }
      else
        {
          if (CAR (x) == cell_symbol_lambda)
            {
              SCM f = CADR (x);
              formals = add_formals (formals, f);
              x = CDR (x);
            }
          else if (CAR (x) == cell_symbol_define
                   || CAR (x) == cell_symbol_define_macro)
            {
              SCM f = CADR (x);
              if (top_p && TYPE (f) == TPAIR)
                f = CDR (f);
              formals = add_formals (formals, f);
              x = CDR (x);
            }
          else if (CAR (x) == cell_symbol_quote)
            return cell_unspecified;
          else if (TYPE (CAR (x)) == TSYMBOL
                   && CAR (x) != cell_symbol_boot_module
                   && CAR (x) != cell_symbol_current_module
                   && CAR (x) != cell_symbol_primitive_load
                   && !formal_p (CAR (x), formals))
            {
              SCM v = module_variable (r0, CAR (x));
              if (v != cell_f)
                CAR (x) = make_variable_ (v);
            }
        }
      x = CDR (x);
      top_p = 0;
    }
  return cell_unspecified;
}

SCM
expand_variable (SCM x, SCM formals) ///((internal))
{
  return expand_variable_ (x, formals, 1);
}

SCM struct_ref_ (SCM x, long i);
SCM vector_ref_ (SCM x, long i);
SCM make_vector__ (long k);
SCM vector_set_x_ (SCM x, long i, SCM e);

SCM
eval_apply ()
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
  long c;

 eval_apply:
  if (r3 == cell_vm_evlis2) goto evlis2;
  else if (r3 == cell_vm_evlis3) goto evlis3;
  else if (r3 == cell_vm_eval_check_func) goto eval_check_func;
  else if (r3 == cell_vm_eval2) goto eval2;
  else if (r3 == cell_vm_apply2) goto apply2;
  else if (r3 == cell_vm_if_expr) goto if_expr;
  else if (r3 == cell_vm_begin_eval) goto begin_eval;
  else if (r3 == cell_vm_eval_set_x) goto eval_set_x;
  else if (r3 == cell_vm_macro_expand_car) goto macro_expand_car;
  else if (r3 == cell_vm_return) goto vm_return;
  else if (r3 == cell_vm_macro_expand_cdr) goto macro_expand_cdr;
  else if (r3 == cell_vm_eval_define) goto eval_define;
  else if (r3 == cell_vm_macro_expand) goto macro_expand;
  else if (r3 == cell_vm_macro_expand_lambda) goto macro_expand_lambda;
  else if (r3 == cell_vm_eval_pmatch_car) goto eval_pmatch_car;
  else if (r3 == cell_vm_begin_expand_macro) goto begin_expand_macro;
  else if (r3 == cell_vm_macro_expand_define) goto macro_expand_define;
  else if (r3 == cell_vm_begin_expand_eval) goto begin_expand_eval;
  else if (r3 == cell_vm_call_with_current_continuation2) goto call_with_current_continuation2;
  else if (r3 == cell_vm_macro_expand_set_x) goto macro_expand_set_x;
  else if (r3 == cell_vm_eval_pmatch_cdr) goto eval_pmatch_cdr;
  else if (r3 == cell_vm_macro_expand_define_macro) goto macro_expand_define_macro;
  else if (r3 == cell_vm_begin_primitive_load) goto begin_primitive_load;

  else if (r3 == cell_vm_evlis) goto evlis;
  else if (r3 == cell_vm_apply) goto apply;
  else if (r3 == cell_vm_eval) goto eval;
  else if (r3 == cell_vm_eval_macro_expand_eval) goto eval_macro_expand_eval;
  else if (r3 == cell_vm_eval_macro_expand_expand) goto eval_macro_expand_expand;
  else if (r3 == cell_vm_begin) goto begin;
  else if (r3 == cell_vm_begin_expand) goto begin_expand;
  else if (r3 == cell_vm_begin_expand_primitive_load) goto begin_expand_primitive_load;
  else if (r3 == cell_vm_if) goto vm_if;
  else if (r3 == cell_vm_call_with_values2) goto call_with_values2;
  else if (r3 == cell_unspecified) return r1;
  else
    error (cell_symbol_system_error,
           MAKE_STRING0 ("eval/apply unknown continuation"));

 evlis:
  if (r1 == cell_nil)
    goto vm_return;
  if (TYPE (r1) != TPAIR)
    goto eval;
  push_cc (CAR (r1), r1, r0, cell_vm_evlis2);
  goto eval;
 evlis2:
  push_cc (CDR (r2), r1, r0, cell_vm_evlis3);
  goto evlis;
 evlis3:
  r1 = cons (r2, r1);
  goto vm_return;

 apply:
  g_stack_array[g_stack+FRAME_PROCEDURE] = CAR (r1);
  t = TYPE (CAR (r1));
  if (t == TSTRUCT && builtin_p (CAR (r1)) == cell_t)
    {
      check_formals (CAR (r1), builtin_arity (CAR (r1)), CDR (r1));
      r1 = apply_builtin (CAR (r1), CDR (r1)); /// FIXME: move into eval_apply
      goto vm_return;
    }
  else if (t == TCLOSURE)
    {
      cl = CLOSURE (CAR (r1));
      body = CDDR (cl);
      formals = CADR (cl);
      args = CDR (r1);
      aa = CDAR (cl);
      aa = CDR (aa);
      check_formals (CAR (r1), formals, CDR (r1));
      p = pairlis (formals, args, aa);
      call_lambda (body, p, aa, r0);
      goto begin;
    }
  else if (t == TCONTINUATION)
    {
      v = CONTINUATION (CAR (r1));
      if (LENGTH (v))
        {
          for (t=0; t < LENGTH (v); t++)
            g_stack_array[STACK_SIZE-LENGTH (v)+t] = vector_ref_ (v, t);
          g_stack = STACK_SIZE-LENGTH (v);
        }
      x = r1;
      gc_pop_frame ();
      r1 = CADR (x);
      goto eval_apply;
    }
  else if (t == TSPECIAL)
    {
      c = CAR (r1);
      if (c == cell_vm_apply)
        {
          push_cc (cons (CADR (r1), CADDR (r1)), r1, r0, cell_vm_return);
          goto apply;
        }
      else if (c ==  cell_vm_eval)
        {
          push_cc (CADR (r1), r1, CADDR (r1), cell_vm_return);
          goto eval;
        }
      else if (c ==  cell_vm_begin_expand)
        {
          push_cc (cons (CADR (r1), cell_nil), r1, CADDR (r1), cell_vm_return);
          goto begin_expand;
        }
      else if (c ==  cell_call_with_current_continuation)
        {
          r1 = CDR (r1);
          goto call_with_current_continuation;
        }
      else
        check_apply (cell_f, CAR (r1));
    }
  else if (t == TSYMBOL)
    {
      if (CAR (r1) == cell_symbol_call_with_values)
        {
          r1 = CDR (r1);
          goto call_with_values;
        }
      if (CAR (r1) == cell_symbol_current_module)
        {
          r1 = r0;
          goto vm_return;
        }
      if (CAR (r1) == cell_symbol_boot_module)
        {
          r1 = m0;
          goto vm_return;
        }
    }
  else if (t == TPAIR)
    {
      if (CAAR (r1) == cell_symbol_lambda)
        {
          formals = CADR (CAR (r1));
          args = CDR (r1);
          body = CDDR (CAR (r1));
          p = pairlis (formals, CDR (r1), r0);
          check_formals (r1, formals, args);
          call_lambda (body, p, p, r0);
          goto begin;
        }
    }
  // write_error_ (CAR (r1));
  // eputs ("\n");
  push_cc (CAR (r1), r1, r0, cell_vm_apply2);
  goto eval;
 apply2:
  check_apply (r1, CAR (r2));
  r1 = cons (r1, CDR (r2));
  goto apply;

 eval:
  t = TYPE (r1);
  if (t == TPAIR)
    {
      c = CAR (r1);
      if (c ==  cell_symbol_pmatch_car)
        {
          push_cc (CADR (r1), r1, r0, cell_vm_eval_pmatch_car);
          goto eval;
        eval_pmatch_car:
          x = r1;
          gc_pop_frame ();
          r1 = CAR (x);
          goto eval_apply;
        }
      else if (c ==  cell_symbol_pmatch_cdr)
        {
          push_cc (CADR (r1), r1, r0, cell_vm_eval_pmatch_cdr);
          goto eval;
        eval_pmatch_cdr:
          x = r1;
          gc_pop_frame ();
          r1 = CDR (x);
          goto eval_apply;
        }
      else if (c ==  cell_symbol_quote)
        {
          x = r1;
          gc_pop_frame ();
          r1 = CADR (x);
          goto eval_apply;
        }
      else if (c ==  cell_symbol_begin)
        goto begin;
      else if (c ==  cell_symbol_lambda)
        {
          r1 = make_closure_ (CADR (r1), CDDR (r1), r0);
          goto vm_return;
        }
      else if (c ==  cell_symbol_if)
        {
          r1=CDR (r1);
          goto vm_if;
        }
      else if (c ==  cell_symbol_set_x)
        {
          push_cc (CAR (CDDR (r1)), r1, r0, cell_vm_eval_set_x);
          goto eval;
        eval_set_x:
          r1 = set_env_x (CADR (r2), r1, r0);
          goto vm_return;
        }
      else if (c == cell_vm_macro_expand)
        {
          push_cc (CADR (r1), r1, r0, cell_vm_eval_macro_expand_eval);
          goto eval;
        eval_macro_expand_eval:
          push_cc (r1, r2, r0, cell_vm_eval_macro_expand_expand);
          goto macro_expand;
        eval_macro_expand_expand:
          goto vm_return;
        }
      else
        {
          if (TYPE (r1) == TPAIR
              && (CAR (r1) == cell_symbol_define
                  || CAR (r1) == cell_symbol_define_macro))
            {
              global_p = CAAR (r0) != cell_closure;
              macro_p = CAR (r1) == cell_symbol_define_macro;
              if (global_p)
                {
                  name = CADR (r1);
                  if (TYPE (CADR (r1)) == TPAIR)
                    name = CAR (name);
                  if (macro_p)
                    {
                      entry = assq (name, g_macros);
                      if (entry == cell_f)
                        macro_set_x (name, cell_f);
                    }
                  else
                    {
                      entry = module_variable (r0, name);
                      if (entry == cell_f)
                        module_define_x (m0, name, cell_f);
                    }
                }
              r2 = r1;
              if (TYPE (CADR (r1)) != TPAIR)
                {
                  push_cc (CAR (CDDR (r1)), r2, cons (cons (CADR (r1), CADR (r1)), r0), cell_vm_eval_define);
                  goto eval;
                }
              else
                {
                  p = pairlis (CADR (r1), CADR (r1), r0);
                  formals = CDR (CADR (r1));
                  body = CDDR (r1);

                  if (macro_p || global_p)
                    expand_variable (body, formals);
                  r1 = cons (cell_symbol_lambda, cons (formals, body));
                  push_cc (r1, r2, p, cell_vm_eval_define);
                  goto eval;
                }
            eval_define:;
              name = CADR (r2);
              if (TYPE (CADR (r2)) == TPAIR)
                name = CAR (name);
              if (macro_p)
                {
                  entry = macro_get_handle (name);
                  r1 = MAKE_MACRO (name, r1);
                  set_cdr_x (entry, r1);
                }
              else if (global_p)
                {
                  entry = module_variable (r0, name);
                  set_cdr_x (entry, r1);
                }
              else
                {
                  entry = cons (name, r1);
                  aa = cons (entry, cell_nil);
                  set_cdr_x (aa, cdr (r0));
                  set_cdr_x (r0, aa);
                  cl = module_variable (r0, cell_closure);
                  set_cdr_x (cl, aa);
                }
              r1 = cell_unspecified;
              goto vm_return;
            }
          push_cc (CAR (r1), r1, r0, cell_vm_eval_check_func);
          gc_check ();
          goto eval;
        eval_check_func:
          push_cc (CDR (r2), r2, r0, cell_vm_eval2);
          goto evlis;
        eval2:
          r1 = cons (CAR (r2), r1);
          goto apply;
        }
    }
  else if (t == TSYMBOL)
    {
      if (r1 == cell_symbol_boot_module)
        goto vm_return;
      if (r1 == cell_symbol_current_module)
        goto vm_return;
      if (r1 == cell_symbol_begin) // FIXME
        {
          r1 = cell_begin;
          goto vm_return;
        }
      r1 = assert_defined (r1, module_ref (r0, r1));
      goto vm_return;
    }
  else if (t == TVARIABLE)
    {
      r1 = CDR (VARIABLE (r1));
      goto vm_return;
    }
  else if (t == TBROKEN_HEART)
    error (cell_symbol_system_error,  r1);
  else
    goto vm_return;

 macro_expand:
  {
    macro;
    expanders;

    if (TYPE (r1) != TPAIR || CAR (r1) == cell_symbol_quote)
      goto vm_return;

    if (CAR (r1) == cell_symbol_lambda)
      {
        push_cc (CDDR (r1), r1, r0, cell_vm_macro_expand_lambda);
        goto macro_expand;
      macro_expand_lambda:
        CDDR (r2) = r1;
        r1 = r2;
        goto vm_return;
      }

    if (TYPE (r1) == TPAIR
        && (macro = get_macro (CAR (r1))) != cell_f)
      {
        r1 = cons (macro, CDR (r1));
        push_cc (r1, cell_nil, r0, cell_vm_macro_expand);
        goto apply;
      }

    if (CAR (r1) == cell_symbol_define
        || CAR (r1) == cell_symbol_define_macro)
      {
        push_cc (CDDR (r1), r1, r0, cell_vm_macro_expand_define);
        goto macro_expand;
      macro_expand_define:
        CDDR (r2) = r1;
        r1 = r2;
        if (CAR (r1) == cell_symbol_define_macro)
          {
            push_cc (r1, r1, r0, cell_vm_macro_expand_define_macro);
            goto eval;
          macro_expand_define_macro:
            r1 = r2;
          }
        goto vm_return;
      }

    if (CAR (r1) == cell_symbol_set_x)
      {
        push_cc (CDDR (r1), r1, r0, cell_vm_macro_expand_set_x);
        goto macro_expand;
      macro_expand_set_x:
        CDDR (r2) = r1;
        r1 = r2;
        goto vm_return;
      }

    if (TYPE (r1) == TPAIR
        && TYPE (CAR (r1)) == TSYMBOL
        && CAR (r1) != cell_symbol_begin
        && ((macro = macro_get_handle (cell_symbol_portable_macro_expand)) != cell_f)
        && ((expanders = module_ref (r0, cell_symbol_sc_expander_alist)) != cell_undefined)
        && ((macro = assq (CAR (r1), expanders)) != cell_f))
      {
        sc_expand = module_ref (r0, cell_symbol_macro_expand);
        r2 = r1;
        if (sc_expand != cell_undefined && sc_expand != cell_f)
          {
            r1 = cons (sc_expand, cons (r1, cell_nil));
            goto apply;
          }
      }

    push_cc (CAR (r1), r1, r0, cell_vm_macro_expand_car);
    goto macro_expand;

  macro_expand_car:
    CAR (r2) = r1;
    r1 = r2;
    if (CDR (r1) == cell_nil)
      goto vm_return;

    push_cc (CDR (r1), r1, r0, cell_vm_macro_expand_cdr);
    goto macro_expand;

  macro_expand_cdr:
    CDR (r2) = r1;
    r1 = r2;

    goto vm_return;
  }

 begin:
  x = cell_unspecified;
  while (r1 != cell_nil)
    {
      gc_check ();
      if (TYPE (r1) == TPAIR)
        {
          if (CAAR (r1) == cell_symbol_primitive_load)
            {
              program = cons (CAR (r1), cell_nil);
              push_cc (program, r1, r0, cell_vm_begin_primitive_load);
              goto begin_expand;
            begin_primitive_load:
              CAR (r2) = r1;
              r1 = r2;
            }
        }

      if (TYPE (r1) == TPAIR && TYPE (CAR (r1)) == TPAIR)
        {
          if (CAAR (r1) == cell_symbol_begin)
            r1 = append2 (CDAR (r1), CDR (r1));
        }
      if (CDR (r1) == cell_nil)
        {
          r1 = CAR (r1);
          goto eval;
        }
      push_cc (CAR (r1), r1, r0, cell_vm_begin_eval);
      goto eval;
    begin_eval:
      x = r1;
      r1 = CDR (r2);
    }
  r1 = x;
  goto vm_return;


 begin_expand:
  x = cell_unspecified;
  while (r1 != cell_nil)
    {
      gc_check ();

      if (TYPE (r1) == TPAIR)
        {
          if (TYPE (CAR (r1)) == TPAIR && CAAR (r1) == cell_symbol_begin)
            r1 = append2 (CDAR (r1), CDR (r1));
          if (CAAR (r1) == cell_symbol_primitive_load)
            {
              push_cc (CADR (CAR (r1)), r1, r0, cell_vm_begin_expand_primitive_load);
              goto eval; // FIXME: expand too?!
            begin_expand_primitive_load:
              if (TYPE (r1) == TNUMBER && VALUE (r1) == 0)
                ;
              else if (TYPE (r1) == TSTRING)
                input = set_current_input_port (open_input_file (r1));
              else if (TYPE (r1) == TPORT)
                input = set_current_input_port (r1);
              else
                assert (0);

              push_cc (input, r2, r0, cell_vm_return);
              x = read_input_file_env (r0);
              if (g_debug > 4)
                module_printer (m0);
              gc_pop_frame ();
              input = r1;
              r1 = x;
              set_current_input_port (input);
              r1 = cons (cell_symbol_begin, r1);
              CAR (r2) = r1;
              r1 = r2;
              continue;
            }
        }

      push_cc (CAR (r1), r1, r0, cell_vm_begin_expand_macro);
      goto macro_expand;
    begin_expand_macro:
      if (r1 != CAR (r2))
        {
          CAR (r2) = r1;
          r1 = r2;
          continue;
        }
      r1 = r2;
      expand_variable (CAR (r1), cell_nil);
      push_cc (CAR (r1), r1, r0, cell_vm_begin_expand_eval);
      goto eval;
    begin_expand_eval:
      x = r1;
      r1 = CDR (r2);
    }
  r1 = x;
  goto vm_return;

 vm_if:
  push_cc (CAR (r1), r1, r0, cell_vm_if_expr);
  goto eval;
 if_expr:
  x = r1;
  r1 = r2;
  if (x != cell_f)
    {
      r1 = CADR (r1);
      goto eval;
    }
  if (CDDR (r1) != cell_nil)
    {
      r1 = CAR (CDDR (r1));
      goto eval;
    }
  r1 = cell_unspecified;
  goto vm_return;

 call_with_current_continuation:
  gc_push_frame ();
  x = MAKE_CONTINUATION (g_continuations++);
  v = make_vector__ (STACK_SIZE-g_stack);
  for (t=g_stack; t < STACK_SIZE; t++)
    vector_set_x_ (v, t-g_stack, g_stack_array[t]);
  CONTINUATION (x) = v;
  gc_pop_frame ();
  push_cc (cons (CAR (r1), cons (x, cell_nil)), x, r0, cell_vm_call_with_current_continuation2);
  goto apply;
 call_with_current_continuation2:
  v = make_vector__ (STACK_SIZE-g_stack);
  for (t=g_stack; t < STACK_SIZE; t++)
    vector_set_x_ (v, t-g_stack, g_stack_array[t]);
  CONTINUATION (r2) = v;
  goto vm_return;

 call_with_values:
  push_cc (cons (CAR (r1), cell_nil), r1, r0, cell_vm_call_with_values2);
  goto apply;
 call_with_values2:
  if (TYPE (r1) == TVALUES)
    r1 = CDR (r1);
  r1 = cons (CADR (r2), r1);
  goto apply;

 vm_return:
  x = r1;
  gc_pop_frame ();
  r1 = x;
  goto eval_apply;
}

SCM
apply (SCM f, SCM x, SCM a) ///((internal))
{
  push_cc (cons (f, x), cell_unspecified, r0, cell_unspecified);
  r3 = cell_vm_apply;
  return eval_apply ();
}

SCM
mes_g_stack (SCM a) ///((internal))
{
  //g_stack = g_free + ARENA_SIZE;
  g_stack = STACK_SIZE;
  r0 = a;
  r1 = MAKE_CHAR (0);
  r2 = MAKE_CHAR (0);
  r3 = MAKE_CHAR (0);
  return r0;
}

// Jam Collector
SCM g_symbol_max;

SCM
gc_init_cells () ///((internal))
{
  long arena_bytes = (ARENA_SIZE+JAM_SIZE)*sizeof (struct scm);
  void *p = malloc (arena_bytes+STACK_SIZE*sizeof (SCM));
  g_cells = (struct scm *)p;
  g_stack_array = (SCM*)(p + arena_bytes);

  TYPE (0) = TVECTOR;
  LENGTH (0) = 1000;
  VECTOR (0) = 0;
  g_cells++;
  TYPE (0) = TCHAR;
  VALUE (0) = 'c';

  // FIXME: remove MES_MAX_STRING, grow dynamically
  g_buf = (char*)malloc (MAX_STRING);

  return 0;
}

void
init_symbol (long x, long type, char const* name)
{
  TYPE (x) = type;
  int length = strlen (name);
  SCM string = make_string (name, length);
  CAR (x) = length;
  CDR (x) = STRING (string);
  hash_set_x (g_symbols, string, x);
}

SCM
mes_symbols () ///((internal))
{
  gc_init_cells ();

  g_free = cell_symbol_test + 1;
  g_symbol_max = g_free;
  g_symbols = make_hash_table_ (500);

  init_symbol (cell_nil, TSPECIAL, "()");
  init_symbol (cell_f, TSPECIAL, "#f");
  init_symbol (cell_t, TSPECIAL, "#t");
  init_symbol (cell_dot, TSPECIAL, ".");
  init_symbol (cell_arrow, TSPECIAL, "=>");
  init_symbol (cell_undefined, TSPECIAL, "*undefined*");
  init_symbol (cell_unspecified, TSPECIAL, "*unspecified*");
  init_symbol (cell_closure, TSPECIAL, "*closure*");
  init_symbol (cell_circular, TSPECIAL, "*circular*");
  init_symbol (cell_begin, TSPECIAL, "*begin*");
  init_symbol (cell_call_with_current_continuation, TSPECIAL, "*call/cc*");

  init_symbol (cell_vm_apply, TSPECIAL, "core:apply");
  init_symbol (cell_vm_apply2, TSPECIAL, "*vm-apply2*");
  init_symbol (cell_vm_begin, TSPECIAL, "*vm-begin*");
  init_symbol (cell_vm_begin_eval, TSPECIAL, "*vm:begin-eval*");
  init_symbol (cell_vm_begin_expand, TSPECIAL, "core:eval");
  init_symbol (cell_vm_begin_expand_eval, TSPECIAL, "*vm:begin-expand-eval*");
  init_symbol (cell_vm_begin_expand_macro, TSPECIAL, "*vm:begin-expand-macro*");
  init_symbol (cell_vm_begin_expand_primitive_load, TSPECIAL, "*vm:core:begin-expand-primitive-load*");
  init_symbol (cell_vm_begin_primitive_load, TSPECIAL, "*vm:core:begin-primitive-load*");
  init_symbol (cell_vm_begin_read_input_file, TSPECIAL, "*vm-begin-read-input-file*");
  init_symbol (cell_vm_call_with_current_continuation2, TSPECIAL, "*vm-call-with-current-continuation2*");
  init_symbol (cell_vm_call_with_values2, TSPECIAL, "*vm-call-with-values2*");
  init_symbol (cell_vm_eval, TSPECIAL, "core:eval-expanded");
  init_symbol (cell_vm_eval2, TSPECIAL, "*vm-eval2*");
  init_symbol (cell_vm_eval_check_func, TSPECIAL, "*vm-eval-check-func*");
  init_symbol (cell_vm_eval_define, TSPECIAL, "*vm-eval-define*");
  init_symbol (cell_vm_eval_macro_expand_eval, TSPECIAL, "*vm:eval-macro-expand-eval*");
  init_symbol (cell_vm_eval_macro_expand_expand, TSPECIAL, "*vm:eval-macro-expand-expand*");
  init_symbol (cell_vm_eval_pmatch_car, TSPECIAL, "*vm-eval-pmatch-car*");
  init_symbol (cell_vm_eval_pmatch_cdr, TSPECIAL, "*vm-eval-pmatch-cdr*");
  init_symbol (cell_vm_eval_set_x, TSPECIAL, "*vm-eval-set!*");
  init_symbol (cell_vm_evlis, TSPECIAL, "*vm-evlis*");
  init_symbol (cell_vm_evlis2, TSPECIAL, "*vm-evlis2*");
  init_symbol (cell_vm_evlis3, TSPECIAL, "*vm-evlis3*");
  init_symbol (cell_vm_if, TSPECIAL, "*vm-if*");
  init_symbol (cell_vm_if_expr, TSPECIAL, "*vm-if-expr*");
  init_symbol (cell_vm_macro_expand, TSPECIAL, "core:macro-expand");
  init_symbol (cell_vm_macro_expand_car, TSPECIAL, "*vm:core:macro-expand-car*");
  init_symbol (cell_vm_macro_expand_cdr, TSPECIAL, "*vm:macro-expand-cdr*");
  init_symbol (cell_vm_macro_expand_define, TSPECIAL, "*vm:core:macro-expand-define*");
  init_symbol (cell_vm_macro_expand_define_macro, TSPECIAL, "*vm:core:macro-expand-define-macro*");
  init_symbol (cell_vm_macro_expand_lambda, TSPECIAL, "*vm:core:macro-expand-lambda*");
  init_symbol (cell_vm_macro_expand_set_x, TSPECIAL, "*vm:core:macro-expand-set!*");
  init_symbol (cell_vm_return, TSPECIAL, "*vm-return*");

  init_symbol (cell_symbol_dot, TSYMBOL, "*dot*");
  init_symbol (cell_symbol_lambda, TSYMBOL, "lambda");
  init_symbol (cell_symbol_begin, TSYMBOL, "begin");
  init_symbol (cell_symbol_if, TSYMBOL, "if");
  init_symbol (cell_symbol_quote, TSYMBOL, "quote");
  init_symbol (cell_symbol_define, TSYMBOL, "define");
  init_symbol (cell_symbol_define_macro, TSYMBOL, "define-macro");

  init_symbol (cell_symbol_quasiquote, TSYMBOL, "quasiquote");
  init_symbol (cell_symbol_unquote, TSYMBOL, "unquote");
  init_symbol (cell_symbol_unquote_splicing, TSYMBOL, "unquote-splicing");
  init_symbol (cell_symbol_syntax, TSYMBOL, "syntax");
  init_symbol (cell_symbol_quasisyntax, TSYMBOL, "quasisyntax");
  init_symbol (cell_symbol_unsyntax, TSYMBOL, "unsyntax");
  init_symbol (cell_symbol_unsyntax_splicing, TSYMBOL, "unsyntax-splicing");

  init_symbol (cell_symbol_set_x, TSYMBOL, "set!");

  init_symbol (cell_symbol_sc_expand, TSYMBOL, "sc-expand");
  init_symbol (cell_symbol_macro_expand, TSYMBOL, "macro-expand");
  init_symbol (cell_symbol_portable_macro_expand, TSYMBOL, "portable-macro-expand");
  init_symbol (cell_symbol_sc_expander_alist, TSYMBOL, "*sc-expander-alist*");

  init_symbol (cell_symbol_call_with_values, TSYMBOL, "call-with-values");
  init_symbol (cell_symbol_call_with_current_continuation, TSYMBOL, "call-with-current-continuation");
  init_symbol (cell_symbol_boot_module, TSYMBOL, "boot-module");
  init_symbol (cell_symbol_current_module, TSYMBOL, "current-module");
  init_symbol (cell_symbol_primitive_load, TSYMBOL, "primitive-load");
  init_symbol (cell_symbol_read_input_file, TSYMBOL, "read-input-file");
  init_symbol (cell_symbol_write, TSYMBOL, "write");
  init_symbol (cell_symbol_display, TSYMBOL, "display");

  init_symbol (cell_symbol_car, TSYMBOL, "car");
  init_symbol (cell_symbol_cdr, TSYMBOL, "cdr");
  init_symbol (cell_symbol_not_a_number, TSYMBOL, "not-a-number");
  init_symbol (cell_symbol_not_a_pair, TSYMBOL, "not-a-pair");
  init_symbol (cell_symbol_system_error, TSYMBOL, "system-error");
  init_symbol (cell_symbol_throw, TSYMBOL, "throw");
  init_symbol (cell_symbol_unbound_variable, TSYMBOL, "unbound-variable");
  init_symbol (cell_symbol_wrong_number_of_args, TSYMBOL, "wrong-number-of-args");
  init_symbol (cell_symbol_wrong_type_arg, TSYMBOL, "wrong-type-arg");

  init_symbol (cell_symbol_buckets, TSYMBOL, "buckets");
  init_symbol (cell_symbol_builtin, TSYMBOL, "<builtin>");
  init_symbol (cell_symbol_frame, TSYMBOL, "<frame>");
  init_symbol (cell_symbol_hashq_table, TSYMBOL, "<hashq-table>");
  init_symbol (cell_symbol_module, TSYMBOL, "<module>");
  init_symbol (cell_symbol_procedure, TSYMBOL, "procedure");
  init_symbol (cell_symbol_record_type, TSYMBOL, "<record-type>");
  init_symbol (cell_symbol_size, TSYMBOL, "size");
  init_symbol (cell_symbol_stack, TSYMBOL, "<stack>");

  init_symbol (cell_symbol_argv, TSYMBOL, "%argv");
  init_symbol (cell_symbol_mes_prefix, TSYMBOL, "%prefix");
  init_symbol (cell_symbol_mes_version, TSYMBOL, "%version");

  init_symbol (cell_symbol_internal_time_units_per_second, TSYMBOL, "internal-time-units-per-second");
  init_symbol (cell_symbol_compiler, TSYMBOL, "%compiler");
  init_symbol (cell_symbol_arch, TSYMBOL, "%arch");

  init_symbol (cell_symbol_pmatch_car, TSYMBOL, "pmatch-car");
  init_symbol (cell_symbol_pmatch_cdr, TSYMBOL, "pmatch-cdr");

  init_symbol (cell_type_bytes, TSYMBOL, "<cell:bytes>");
  init_symbol (cell_type_char, TSYMBOL, "<cell:char>");
  init_symbol (cell_type_closure, TSYMBOL, "<cell:closure>");
  init_symbol (cell_type_continuation, TSYMBOL, "<cell:continuation>");
  init_symbol (cell_type_function, TSYMBOL, "<cell:function>");
  init_symbol (cell_type_keyword, TSYMBOL, "<cell:keyword>");
  init_symbol (cell_type_macro, TSYMBOL, "<cell:macro>");
  init_symbol (cell_type_number, TSYMBOL, "<cell:number>");
  init_symbol (cell_type_pair, TSYMBOL, "<cell:pair>");
  init_symbol (cell_type_port, TSYMBOL, "<cell:port>");
  init_symbol (cell_type_ref, TSYMBOL, "<cell:ref>");
  init_symbol (cell_type_special, TSYMBOL, "<cell:special>");
  init_symbol (cell_type_string, TSYMBOL, "<cell:string>");
  init_symbol (cell_type_struct, TSYMBOL, "<cell:struct>");
  init_symbol (cell_type_symbol, TSYMBOL, "<cell:symbol>");
  init_symbol (cell_type_values, TSYMBOL, "<cell:values>");
  init_symbol (cell_type_variable, TSYMBOL, "<cell:variable>");
  init_symbol (cell_type_vector, TSYMBOL, "<cell:vector>");
  init_symbol (cell_type_broken_heart, TSYMBOL, "<cell:broken-heart>");

  init_symbol (cell_symbol_test, TSYMBOL, "%%test");

  SCM a = cell_nil;
  a = acons (cell_symbol_call_with_values, cell_symbol_call_with_values, a);
  a = acons (cell_symbol_boot_module, cell_symbol_boot_module, a);
  a = acons (cell_symbol_current_module, cell_symbol_current_module, a);
  a = acons (cell_symbol_call_with_current_continuation, cell_call_with_current_continuation, a);

  a = acons (cell_symbol_mes_version, MAKE_STRING0 (VERSION), a);
  a = acons (cell_symbol_mes_prefix, MAKE_STRING0 (PREFIX), a);

  a = acons (cell_type_bytes, MAKE_NUMBER (TBYTES), a);
  a = acons (cell_type_char, MAKE_NUMBER (TCHAR), a);
  a = acons (cell_type_closure, MAKE_NUMBER (TCLOSURE), a);
  a = acons (cell_type_continuation, MAKE_NUMBER (TCONTINUATION), a);
  a = acons (cell_type_keyword, MAKE_NUMBER (TKEYWORD), a);
  a = acons (cell_type_macro, MAKE_NUMBER (TMACRO), a);
  a = acons (cell_type_number, MAKE_NUMBER (TNUMBER), a);
  a = acons (cell_type_pair, MAKE_NUMBER (TPAIR), a);
  a = acons (cell_type_port, MAKE_NUMBER (TPORT), a);
  a = acons (cell_type_ref, MAKE_NUMBER (TREF), a);
  a = acons (cell_type_special, MAKE_NUMBER (TSPECIAL), a);
  a = acons (cell_type_string, MAKE_NUMBER (TSTRING), a);
  a = acons (cell_type_struct, MAKE_NUMBER (TSTRUCT), a);
  a = acons (cell_type_symbol, MAKE_NUMBER (TSYMBOL), a);
  a = acons (cell_type_values, MAKE_NUMBER (TVALUES), a);
  a = acons (cell_type_variable, MAKE_NUMBER (TVARIABLE), a);
  a = acons (cell_type_vector, MAKE_NUMBER (TVECTOR), a);
  a = acons (cell_type_broken_heart, MAKE_NUMBER (TBROKEN_HEART), a);

  a = acons (cell_closure, a, a);

  return a;
}

SCM
mes_environment (int argc, char *argv[])
{
  SCM a = mes_symbols ();

  char *compiler = "gnuc";
#if __MESC__
  compiler = "mesc";
#elif __TINYC__
  compiler = "tcc";
#endif
  a = acons (cell_symbol_compiler, MAKE_STRING0 (compiler), a);

  char *arch = "x86";
#if __x86_64__
  arch = "x86_64";
#endif
  a = acons (cell_symbol_arch, MAKE_STRING0 (arch), a);

#if !MES_MINI
  SCM lst = cell_nil;
  for (int i=argc-1; i>=0; i--)
    lst = cons (MAKE_STRING0 (argv[i]), lst);
  a = acons (cell_symbol_argv, lst, a);
#endif

  return mes_g_stack (a);
}

SCM
init_builtin (SCM builtin_type, char const* name, int arity, SCM (*function) (SCM), SCM a)
{
  SCM s = cstring_to_symbol (name);
  return acons (s, make_builtin (builtin_type, symbol_to_string (s), MAKE_NUMBER (arity), MAKE_NUMBER (function)), a);
}

SCM
make_builtin_type () ///(internal))
{
  SCM record_type = cell_symbol_record_type;
  SCM fields = cell_nil;
  fields = cons (cstring_to_symbol ("address"), fields);
  fields = cons (cstring_to_symbol ("arity"), fields);
  fields = cons (cstring_to_symbol ("name"), fields);
  fields = cons (fields, cell_nil);
  fields = cons (cell_symbol_builtin, fields);
  return make_struct (record_type, fields, cell_unspecified);
}

SCM
make_builtin (SCM builtin_type, SCM name, SCM arity, SCM function)
{
  SCM values = cell_nil;
  values = cons (function, values);
  values = cons (arity, values);
  values = cons (name, values);
  values = cons (cell_symbol_builtin, values);
  return make_struct (builtin_type, values, cstring_to_symbol ("builtin-printer"));
}

SCM
builtin_name (SCM builtin)
{
  return struct_ref_ (builtin, 3);
}

SCM
builtin_arity (SCM builtin)
{
  return struct_ref_ (builtin, 4);
}

#if __MESC__
long
builtin_function (SCM builtin)
{
  return VALUE (struct_ref_ (builtin, 5));
}
#else
SCM
(*builtin_function (SCM builtin)) (SCM)
{
  return (function1_t)VALUE (struct_ref_ (builtin, 5));
}
#endif

SCM
builtin_p (SCM x)
{
  return (TYPE (x) == TSTRUCT && struct_ref_ (x, 2) == cell_symbol_builtin)
    ? cell_t : cell_f;
}

SCM
builtin_printer (SCM builtin)
{
  fdputs ("#<procedure ", __stdout);
  display_ (builtin_name (builtin));
  fdputc (' ', __stdout);
  int arity = VALUE (builtin_arity (builtin));
  if (arity == -1)
    fdputc ('_', __stdout);
  else
    {
      fdputc ('(', __stdout);
      for (int i = 0; i < arity; i++)
        {
          if (i)
            fdputc (' ', __stdout);
          fdputc ('_', __stdout);
        }
    }
  fdputc ('>', __stdout);
}

SCM
apply_builtin (SCM fn, SCM x) ///((internal))
{
  int arity = VALUE (builtin_arity (fn));
  if ((arity > 0 || arity == -1)
      && x != cell_nil && TYPE (CAR (x)) == TVALUES)
    x = cons (CADAR (x), CDR (x));
  if ((arity > 1 || arity == -1)
      && x != cell_nil && TYPE (CDR (x)) == TPAIR && TYPE (CADR (x)) == TVALUES)
    x = cons (CAR (x), cons (CDADAR (x), CDR (x)));

#if __M2_PLANET__
  FUNCTION fp = builtin_function (fn)
  if (arity == 0)
    return fp ();
  else if (arity == 1)
    return fp (CAR (x));
  else if (arity == 2)
    return fp (CAR (x), CADR (x));
  else if (arity == 3)
    return fp (CAR (x), CADR (x), CAR (CDDR (x)));
  else if (arity == -1)
    return fp (x);
#else // !__M2_PLANET__
  if (arity == 0)
    {
      //function0_t fp = f->function;
      SCM (*fp) (void) = (function0_t)builtin_function (fn);
      return fp ();
    }
  else if (arity == 1)
    {
      //function1_t fp = f->function;
      SCM (*fp) (SCM) = (function1_t)builtin_function (fn);
      return fp (CAR (x));
    }
  else if (arity == 2)
    {
      //function2_t fp = f->function;
      SCM (*fp) (SCM, SCM) = (function2_t)builtin_function (fn);
      return fp (CAR (x), CADR (x));
    }
  else if (arity == 3)
    {
      //function3_t fp = f->function;
      SCM (*fp) (SCM, SCM, SCM) = (function3_t)builtin_function (fn);
      return fp (CAR (x), CADR (x), CAR (CDDR (x)));
    }
  else if (arity == -1)
    {
      //functionn_t fp = f->function;
      SCM (*fp) (SCM) = (function1_t)builtin_function (fn);
      return fp (x);
    }
#endif //! __M2_PLANET__
  return cell_unspecified;
}

SCM
mes_builtins (SCM a) ///((internal))
{
  // TODO minimal: cons, car, cdr, list, null_p, eq_p minus, plus
  // display_, display_error_, getenv

  SCM builtin_type = make_builtin_type ();

  // src/gc.mes
  a = init_builtin (builtin_type, "gc-check", 0, (function1_t)&gc_check, a);
  a = init_builtin (builtin_type, "gc", 0, (function1_t)&gc, a);
  // src/hash.mes
  a = init_builtin (builtin_type, "hashq", 2, (function1_t)&hashq, a);
  a = init_builtin (builtin_type, "hash", 2, (function1_t)&hash, a);
  a = init_builtin (builtin_type, "hashq-get-handle", 3, (function1_t)&hashq_get_handle, a);
  a = init_builtin (builtin_type, "hashq-ref", 3, (function1_t)&hashq_ref, a);
  a = init_builtin (builtin_type, "hash-ref", 3, (function1_t)&hash_ref, a);
  a = init_builtin (builtin_type, "hashq-set!", 3, (function1_t)&hashq_set_x, a);
  a = init_builtin (builtin_type, "hash-set!", 3, (function1_t)&hash_set_x, a);
  a = init_builtin (builtin_type, "hash-table-printer", 1, (function1_t)&hash_table_printer, a);
  a = init_builtin (builtin_type, "make-hash-table", 1, (function1_t)&make_hash_table, a);
  // src/lib.mes
  a = init_builtin (builtin_type, "core:display", 1, (function1_t)&display_, a);
  a = init_builtin (builtin_type, "core:display-error", 1, (function1_t)&display_error_, a);
  a = init_builtin (builtin_type, "core:display-port", 2, (function1_t)&display_port_, a);
  a = init_builtin (builtin_type, "core:write", 1, (function1_t)&write_, a);
  a = init_builtin (builtin_type, "core:write-error", 1, (function1_t)&write_error_, a);
  a = init_builtin (builtin_type, "core:write-port", 2, (function1_t)&write_port_, a);
  a = init_builtin (builtin_type, "exit", 1, (function1_t)&exit_, a);
  a = init_builtin (builtin_type, "frame-printer", 1, (function1_t)&frame_printer, a);
  a = init_builtin (builtin_type, "make-stack", -1, (function1_t)&make_stack, a);
  a = init_builtin (builtin_type, "stack-length", 1, (function1_t)&stack_length, a);
  a = init_builtin (builtin_type, "stack-ref", 2, (function1_t)&stack_ref, a);
  a = init_builtin (builtin_type, "xassq", 2, (function1_t)&xassq, a);
  a = init_builtin (builtin_type, "memq", 2, (function1_t)&memq, a);
  a = init_builtin (builtin_type, "equal2?", 2, (function1_t)&equal2_p, a);
  a = init_builtin (builtin_type, "last-pair", 1, (function1_t)&last_pair, a);
  a = init_builtin (builtin_type, "pair?", 1, (function1_t)&pair_p, a);
  // src/math.mes
  a = init_builtin (builtin_type, ">", -1, (function1_t)&greater_p, a);
  a = init_builtin (builtin_type, "<", -1, (function1_t)&less_p, a);
  a = init_builtin (builtin_type, "=", -1, (function1_t)&is_p, a);
  a = init_builtin (builtin_type, "-", -1, (function1_t)&minus, a);
  a = init_builtin (builtin_type, "+", -1, (function1_t)&plus, a);
  a = init_builtin (builtin_type, "/", -1, (function1_t)&divide, a);
  a = init_builtin (builtin_type, "modulo", 2, (function1_t)&modulo, a);
  a = init_builtin (builtin_type, "*", -1, (function1_t)&multiply, a);
  a = init_builtin (builtin_type, "logand", -1, (function1_t)&logand, a);
  a = init_builtin (builtin_type, "logior", -1, (function1_t)&logior, a);
  a = init_builtin (builtin_type, "lognot", 1, (function1_t)&lognot, a);
  a = init_builtin (builtin_type, "logxor", -1, (function1_t)&logxor, a);
  a = init_builtin (builtin_type, "ash", 2, (function1_t)&ash, a);
  // src/mes.mes
  a = init_builtin (builtin_type, "core:make-cell", 3, (function1_t)&make_cell_, a);
  a = init_builtin (builtin_type, "core:type", 1, (function1_t)&type_, a);
  a = init_builtin (builtin_type, "core:car", 1, (function1_t)&car_, a);
  a = init_builtin (builtin_type, "core:cdr", 1, (function1_t)&cdr_, a);
  a = init_builtin (builtin_type, "cons", 2, (function1_t)&cons, a);
  a = init_builtin (builtin_type, "car", 1, (function1_t)&car, a);
  a = init_builtin (builtin_type, "cdr", 1, (function1_t)&cdr, a);
  a = init_builtin (builtin_type, "list", -1, (function1_t)&list, a);
  a = init_builtin (builtin_type, "null?", 1, (function1_t)&null_p, a);
  a = init_builtin (builtin_type, "eq?", 2, (function1_t)&eq_p, a);
  a = init_builtin (builtin_type, "values", -1, (function1_t)&values, a);
  a = init_builtin (builtin_type, "acons", 3, (function1_t)&acons, a);
  a = init_builtin (builtin_type, "length", 1, (function1_t)&length, a);
  a = init_builtin (builtin_type, "error", 2, (function1_t)&error, a);
  a = init_builtin (builtin_type, "append2", 2, (function1_t)&append2, a);
  a = init_builtin (builtin_type, "append-reverse", 2, (function1_t)&append_reverse, a);
  a = init_builtin (builtin_type, "core:reverse!", 2, (function1_t)&reverse_x_, a);
  a = init_builtin (builtin_type, "pairlis", 3, (function1_t)&pairlis, a);
  a = init_builtin (builtin_type, "assq", 2, (function1_t)&assq, a);
  a = init_builtin (builtin_type, "assoc", 2, (function1_t)&assoc, a);
  a = init_builtin (builtin_type, "set-car!", 2, (function1_t)&set_car_x, a);
  a = init_builtin (builtin_type, "set-cdr!", 2, (function1_t)&set_cdr_x, a);
  a = init_builtin (builtin_type, "set-env!", 3, (function1_t)&set_env_x, a);
  a = init_builtin (builtin_type, "macro-get-handle", 1, (function1_t)&macro_get_handle, a);
  a = init_builtin (builtin_type, "add-formals", 2, (function1_t)&add_formals, a);
  a = init_builtin (builtin_type, "eval-apply", 0, (function1_t)&eval_apply, a);
  a = init_builtin (builtin_type, "make-builtin-type", 0, (function1_t)&make_builtin_type, a);
  a = init_builtin (builtin_type, "make-builtin", 4, (function1_t)&make_builtin, a);
  a = init_builtin (builtin_type, "builtin-name", 1, (function1_t)&builtin_name, a);
  a = init_builtin (builtin_type, "builtin-arity", 1, (function1_t)&builtin_arity, a);
  a = init_builtin (builtin_type, "builtin?", 1, (function1_t)&builtin_p, a);
  a = init_builtin (builtin_type, "builtin-printer", 1, (function1_t)&builtin_printer, a);
  // src/module.mes
  a = init_builtin (builtin_type, "make-module-type", 0, (function1_t)&make_module_type, a);
  a = init_builtin (builtin_type, "module-printer", 1, (function1_t)&module_printer, a);
  a = init_builtin (builtin_type, "module-variable", 2, (function1_t)&module_variable, a);
  a = init_builtin (builtin_type, "module-ref", 2, (function1_t)&module_ref, a);
  a = init_builtin (builtin_type, "module-define!", 3, (function1_t)&module_define_x, a);
  // src/posix.mes
  a = init_builtin (builtin_type, "peek-byte", 0, (function1_t)&peek_byte, a);
  a = init_builtin (builtin_type, "read-byte", 0, (function1_t)&read_byte, a);
  a = init_builtin (builtin_type, "unread-byte", 1, (function1_t)&unread_byte, a);
  a = init_builtin (builtin_type, "peek-char", 0, (function1_t)&peek_char, a);
  a = init_builtin (builtin_type, "read-char", -1, (function1_t)&read_char, a);
  a = init_builtin (builtin_type, "unread-char", 1, (function1_t)&unread_char, a);
  a = init_builtin (builtin_type, "write-char", -1, (function1_t)&write_char, a);
  a = init_builtin (builtin_type, "write-byte", -1, (function1_t)&write_byte, a);
  a = init_builtin (builtin_type, "getenv", 1, (function1_t)&getenv_, a);
  a = init_builtin (builtin_type, "setenv", 2, (function1_t)&setenv_, a);
  a = init_builtin (builtin_type, "access?", 2, (function1_t)&access_p, a);
  a = init_builtin (builtin_type, "current-input-port", 0, (function1_t)&current_input_port, a);
  a = init_builtin (builtin_type, "open-input-file", 1, (function1_t)&open_input_file, a);
  a = init_builtin (builtin_type, "open-input-string", 1, (function1_t)&open_input_string, a);
  a = init_builtin (builtin_type, "set-current-input-port", 1, (function1_t)&set_current_input_port, a);
  a = init_builtin (builtin_type, "current-output-port", 0, (function1_t)&current_output_port, a);
  a = init_builtin (builtin_type, "current-error-port", 0, (function1_t)&current_error_port, a);
  a = init_builtin (builtin_type, "open-output-file", -1, (function1_t)&open_output_file, a);
  a = init_builtin (builtin_type, "set-current-output-port", 1, (function1_t)&set_current_output_port, a);
  a = init_builtin (builtin_type, "set-current-error-port", 1, (function1_t)&set_current_error_port, a);
  a = init_builtin (builtin_type, "force-output", -1, (function1_t)&force_output, a);
  a = init_builtin (builtin_type, "chmod", 2, (function1_t)&chmod_, a);
  a = init_builtin (builtin_type, "isatty?", 1, (function1_t)&isatty_p, a);
  a = init_builtin (builtin_type, "primitive-fork", 0, (function1_t)&primitive_fork, a);
  a = init_builtin (builtin_type, "execl", 2, (function1_t)&execl_, a);
  a = init_builtin (builtin_type, "core:waitpid", 2, (function1_t)&waitpid_, a);
  a = init_builtin (builtin_type, "current-time", 0, (function1_t)&current_time, a);
  a = init_builtin (builtin_type, "gettimeofday", 0, (function1_t)&gettimeofday_, a);
  a = init_builtin (builtin_type, "get-internal-run-time", 0, (function1_t)&get_internal_run_time, a);
  a = init_builtin (builtin_type, "getcwd", 0, (function1_t)&getcwd_, a);
  a = init_builtin (builtin_type, "dup", 1, (function1_t)&dup_, a);
  a = init_builtin (builtin_type, "dup2", 2, (function1_t)&dup2_, a);
  a = init_builtin (builtin_type, "delete-file", 1, (function1_t)&delete_file, a);
  // src/reader.mes
  a = init_builtin (builtin_type, "core:read-input-file-env", 2, (function1_t)&read_input_file_env_, a);
  a = init_builtin (builtin_type, "read-input-file-env", 1, (function1_t)&read_input_file_env, a);
  a = init_builtin (builtin_type, "read-env", 1, (function1_t)&read_env, a);
  a = init_builtin (builtin_type, "reader-read-sexp", 3, (function1_t)&reader_read_sexp, a);
  a = init_builtin (builtin_type, "reader-read-character", 0, (function1_t)&reader_read_character, a);
  a = init_builtin (builtin_type, "reader-read-binary", 0, (function1_t)&reader_read_binary, a);
  a = init_builtin (builtin_type, "reader-read-octal", 0, (function1_t)&reader_read_octal, a);
  a = init_builtin (builtin_type, "reader-read-hex", 0, (function1_t)&reader_read_hex, a);
  a = init_builtin (builtin_type, "reader-read-string", 0, (function1_t)&reader_read_string, a);
  // src/strings.mes
  a = init_builtin (builtin_type, "string=?", 2, (function1_t)&string_equal_p, a);
  a = init_builtin (builtin_type, "symbol->string", 1, (function1_t)&symbol_to_string, a);
  a = init_builtin (builtin_type, "symbol->keyword", 1, (function1_t)&symbol_to_keyword, a);
  a = init_builtin (builtin_type, "keyword->string", 1, (function1_t)&keyword_to_string, a);
  a = init_builtin (builtin_type, "string->symbol", 1, (function1_t)&string_to_symbol, a);
  a = init_builtin (builtin_type, "make-symbol", 1, (function1_t)&make_symbol, a);
  a = init_builtin (builtin_type, "string->list", 1, (function1_t)&string_to_list, a);
  a = init_builtin (builtin_type, "list->string", 1, (function1_t)&list_to_string, a);
  a = init_builtin (builtin_type, "read-string", -1, (function1_t)&read_string, a);
  a = init_builtin (builtin_type, "string-append", -1, (function1_t)&string_append, a);
  a = init_builtin (builtin_type, "string-length", 1, (function1_t)&string_length, a);
  a = init_builtin (builtin_type, "string-ref", 2, (function1_t)&string_ref, a);
  // src/struct.mes
  a = init_builtin (builtin_type, "make-struct", 3, (function1_t)&make_struct, a);
  a = init_builtin (builtin_type, "struct-length", 1, (function1_t)&struct_length, a);
  a = init_builtin (builtin_type, "struct-ref", 2, (function1_t)&struct_ref, a);
  a = init_builtin (builtin_type, "struct-set!", 3, (function1_t)&struct_set_x, a);
  // src/vector.mes
  a = init_builtin (builtin_type, "core:make-vector", 1, (function1_t)&make_vector_, a);
  a = init_builtin (builtin_type, "vector-length", 1, (function1_t)&vector_length, a);
  a = init_builtin (builtin_type, "vector-ref", 2, (function1_t)&vector_ref, a);
  a = init_builtin (builtin_type, "vector-entry", 1, (function1_t)&vector_entry, a);
  a = init_builtin (builtin_type, "vector-set!", 3, (function1_t)&vector_set_x, a);
  a = init_builtin (builtin_type, "list->vector", 1, (function1_t)&list_to_vector, a);
  a = init_builtin (builtin_type, "vector->list", 1, (function1_t)&vector_to_list, a);

  return a;
}

int
open_boot (char *prefix, char const *boot, char const *location)
{
  strcpy (prefix + strlen (prefix), boot);
  if (g_debug > 1)
    {
      eputs ("mes: reading boot-0 [");
      eputs (location);
      eputs ("]: ");
      eputs (prefix);
      eputs ("\n");
    }
  int fd = open (prefix, O_RDONLY);
  if (g_debug && fd > 0)
    {
      eputs ("mes: read boot-0: ");
      eputs (prefix);
      eputs ("\n");
    }
  return fd;
}

SCM
read_boot () ///((internal))
{
  __stdin = -1;
  char prefix[1024];
  char boot[1024];
  if (getenv ("MES_BOOT"))
    strcpy (boot, getenv ("MES_BOOT"));
  else
    strcpy (boot, "boot-0.scm");
  if (getenv ("MES_PREFIX"))
    {
      strcpy (prefix, getenv ("MES_PREFIX"));
      strcpy (prefix + strlen (prefix), "/module");
      strcpy (prefix + strlen (prefix), "/mes/");
      __stdin = open_boot (prefix, boot, "MES_PREFIX");
    }
  if (__stdin < 0)
    {
      char const *p = MODULEDIR "/mes/";
      strcpy (prefix, p);
      __stdin = open_boot (prefix, boot, "MODULEDIR");
    }
  if (__stdin < 0)
    {
      strcpy (prefix, "mes/module/mes/");
      __stdin = open_boot (prefix, boot, ".");
    }
  if (__stdin < 0)
    {
      prefix[0] = 0;
      __stdin = open_boot (prefix, boot, "<boot>");
    }
  if (__stdin < 0)
    {
      eputs ("mes: boot failed: no such file: ");
      eputs (boot);
      eputs ("\n");
      exit (1);
    }

  r2 = read_input_file_env (r0);
  __stdin = STDIN;
  return r2;
}

#include "hash.c"
#include "module.c"
#include "posix.c"
#include "math.c"
#include "lib.c"
#include "vector.c"
#include "strings.c"
#include "struct.c"
#include "gc.c"
#include "reader.c"

int
main (int argc, char *argv[])
{
  char *p;
  if (p = getenv ("MES_DEBUG"))
    g_debug = atoi (p);
  if (g_debug > 1)
    {
      eputs (";;; MODULEDIR=");
      eputs (MODULEDIR);
      eputs ("\n");
    }
  if (p = getenv ("MES_MAX_ARENA"))
    MAX_ARENA_SIZE = atoi (p);
  if (p = getenv ("MES_ARENA"))
    ARENA_SIZE = atoi (p);
  JAM_SIZE = ARENA_SIZE / 10;
  if (p = getenv ("MES_JAM"))
    JAM_SIZE = atoi (p);
  GC_SAFETY = ARENA_SIZE / 100;
  if (p = getenv ("MES_SAFETY"))
    GC_SAFETY = atoi (p);
  if (p = getenv ("MES_STACK"))
    STACK_SIZE = atoi (p);
  if (p = getenv ("MES_MAX_STRING"))
    MAX_STRING = atoi (p);

  SCM a = mes_environment (argc, argv);
  a = mes_builtins (a);
  a = init_time (a);
  m0 = make_initial_module (a);
  g_macros = make_hash_table_ (0);

  if (g_debug > 4)
    module_printer (m0);

  SCM program = read_boot ();
  push_cc (r2, cell_unspecified, r0, cell_unspecified);

  if (g_debug > 2)
    {
      eputs ("\ngc stats: [");
      eputs (itoa (g_free));
      eputs ("]\n");
    }
  if (g_debug > 3)
    {
      eputs ("program: ");
      write_error_ (r1);
      eputs ("\n");
    }
  r3 = cell_vm_begin_expand;
  r1 = eval_apply ();
  if (g_debug)
    {
      write_error_ (r1);
      eputs ("\n");
    }
  if (g_debug)
    {
      if (g_debug > 4)
        module_printer (m0);

      eputs ("\ngc stats: [");
      eputs (itoa (g_free));
      MAX_ARENA_SIZE = 0;

      gc (g_stack);
      eputs (" => ");
      eputs (itoa (g_free));
      eputs ("]\n");
      if (g_debug > 4)
        module_printer (m0);
      eputs ("\n");

      gc (g_stack);
      eputs (" => ");
      eputs (itoa (g_free));
      eputs ("]\n");
      if (g_debug > 4)
        module_printer (m0);
      eputs ("\n");

      gc (g_stack);
      eputs (" => ");
      eputs (itoa (g_free));
      eputs ("]\n");
      if (g_debug > 4)
        module_printer (m0);
      if (g_debug > 3)
        {
          eputs ("ports:"); write_error_ (g_ports); eputs ("\n");
        }
      eputs ("\n");


    }
  return 0;
}
