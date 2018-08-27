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

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <libmes.h>

//#define MES_MINI 1
#if POSIX
long ARENA_SIZE = 100000000; // 64b: 4GiB
#else
long ARENA_SIZE = 200000; // 32b: 2MiB, 64b: 4 MiB
#endif
long MAX_ARENA_SIZE = 100000000;

long JAM_SIZE = 20000;
long GC_SAFETY = 2000;

char *g_arena = 0;
typedef long SCM;

int g_debug = 0;
int g_free = 0;

SCM g_continuations = 0;
SCM g_symbols = 0;
SCM g_stack = 0;
// a/env
SCM r0 = 0;
// param 1
SCM r1 = 0;
// save 2+load/dump
SCM r2 = 0;
// continuation
SCM r3 = 0;
// macro
SCM g_macros = 1;
SCM g_ports = 1;

#if __M2_PLANET__
CONSTANT TCHAR          0
CONSTANT TCLOSURE       1
CONSTANT TCONTINUATION  2
CONSTANT TFUNCTION      3
CONSTANT TKEYWORD       4
CONSTANT TMACRO         5
CONSTANT TNUMBER        6
CONSTANT TPAIR          7
CONSTANT TPORT          8
CONSTANT TREF           9
CONSTANT TSPECIAL      10
CONSTANT TSTRING       11
CONSTANT TSYMBOL       12
CONSTANT TVALUES       13
CONSTANT TVARIABLE     14
CONSTANT TVECTOR       15
CONSTANT TBROKEN_HEART 16
#else // !__M2_PLANET__
enum type_t {TCHAR, TCLOSURE, TCONTINUATION, TFUNCTION, TKEYWORD, TMACRO, TNUMBER, TPAIR, TPORT, TREF, TSPECIAL, TSTRING, TSYMBOL, TVALUES, TVARIABLE, TVECTOR, TBROKEN_HEART};
#endif // !__M2_PLANET__

typedef SCM (*function0_t) (void);
typedef SCM (*function1_t) (SCM);
typedef SCM (*function2_t) (SCM, SCM);
typedef SCM (*function3_t) (SCM, SCM, SCM);
typedef SCM (*functionn_t) (SCM);
#if !POSIX
struct scm {
  enum type_t type;
  SCM car;
  SCM cdr;
};
struct function {
#if __M2_PLANET__
  FUNCTION function;
#else // !__M2_PLANET__
  SCM (*function) (SCM);
#endif // !__M2_PLANET__
  long arity;
  char *name;
};
#else
struct function {
  union {
    function0_t function0;
    function1_t function1;
    function2_t function2;
    function3_t function3;
    functionn_t functionn;
  };
  long arity;
  char const *name;
};
struct scm {
  enum type_t type;
  union {
    char const* name;
    SCM car;
    SCM ref;
    SCM string;
    SCM variable;
    long length;
  };
  union {
    long value;
    long function;
    long port;
    SCM cdr;
    SCM closure;
    SCM continuation;
    SCM macro;
    SCM vector;
  };
};
#endif

#if __MESC__
//FIXME
char *foobar = 0;
struct scm *g_cells = foobar;
struct scm *g_news = foobar;
#else
struct scm *g_cells = 0;
struct scm *g_news = 0;
#endif

struct scm scm_nil = {TSPECIAL, "()",0};
struct scm scm_f = {TSPECIAL, "#f",0};
struct scm scm_t = {TSPECIAL, "#t",0};
struct scm scm_dot = {TSPECIAL, ".",0};
struct scm scm_arrow = {TSPECIAL, "=>",0};
struct scm scm_undefined = {TSPECIAL, "*undefined*",0};
struct scm scm_unspecified = {TSPECIAL, "*unspecified*",0};
struct scm scm_closure = {TSPECIAL, "*closure*",0};
struct scm scm_circular = {TSPECIAL, "*circular*",0};
struct scm scm_begin = {TSPECIAL, "*begin*",0};

struct scm scm_symbol_dot = {TSYMBOL, "*dot*",0};
struct scm scm_symbol_lambda = {TSYMBOL, "lambda",0};
struct scm scm_symbol_begin = {TSYMBOL, "begin",0};
struct scm scm_symbol_if = {TSYMBOL, "if",0};
struct scm scm_symbol_quote = {TSYMBOL, "quote",0};
struct scm scm_symbol_define = {TSYMBOL, "define",0};
struct scm scm_symbol_define_macro = {TSYMBOL, "define-macro",0};

struct scm scm_symbol_quasiquote = {TSYMBOL, "quasiquote", 0};
struct scm scm_symbol_unquote = {TSYMBOL, "unquote", 0};
struct scm scm_symbol_unquote_splicing = {TSYMBOL, "unquote-splicing", 0};
struct scm scm_symbol_syntax = {TSYMBOL, "syntax",0};
struct scm scm_symbol_quasisyntax = {TSYMBOL, "quasisyntax", 0};
struct scm scm_symbol_unsyntax = {TSYMBOL, "unsyntax", 0};
struct scm scm_symbol_unsyntax_splicing = {TSYMBOL, "unsyntax-splicing", 0};

struct scm scm_symbol_set_x = {TSYMBOL, "set!",0};

struct scm scm_symbol_sc_expand = {TSYMBOL, "sc-expand",0};
struct scm scm_symbol_macro_expand = {TSYMBOL, "macro-expand",0};
struct scm scm_symbol_portable_macro_expand = {TSYMBOL, "portable-macro-expand",0};
struct scm scm_symbol_sc_expander_alist = {TSYMBOL, "*sc-expander-alist*",0};

struct scm scm_symbol_call_with_values = {TSYMBOL, "call-with-values",0};
struct scm scm_call_with_current_continuation = {TSPECIAL, "*call/cc*",0};
struct scm scm_symbol_call_with_current_continuation = {TSYMBOL, "call-with-current-continuation",0};
struct scm scm_symbol_current_module = {TSYMBOL, "current-module",0};
struct scm scm_symbol_primitive_load = {TSYMBOL, "primitive-load",0};
struct scm scm_symbol_read_input_file = {TSYMBOL, "read-input-file",0};
struct scm scm_symbol_write = {TSYMBOL, "write",0};
struct scm scm_symbol_display = {TSYMBOL, "display",0};

struct scm scm_symbol_throw = {TSYMBOL, "throw",0};
struct scm scm_symbol_not_a_number = {TSYMBOL, "not-a-number",0};
struct scm scm_symbol_not_a_pair = {TSYMBOL, "not-a-pair",0};
struct scm scm_symbol_system_error = {TSYMBOL, "system-error",0};
struct scm scm_symbol_wrong_number_of_args = {TSYMBOL, "wrong-number-of-args",0};
struct scm scm_symbol_wrong_type_arg = {TSYMBOL, "wrong-type-arg",0};
struct scm scm_symbol_unbound_variable = {TSYMBOL, "unbound-variable",0};

struct scm scm_symbol_argv = {TSYMBOL, "%argv",0};
struct scm scm_symbol_mes_prefix = {TSYMBOL, "%prefix",0};
struct scm scm_symbol_mes_version = {TSYMBOL, "%version",0};

struct scm scm_symbol_car = {TSYMBOL, "car",0};
struct scm scm_symbol_cdr = {TSYMBOL, "cdr",0};
struct scm scm_symbol_pmatch_car = {TSYMBOL, "pmatch-car",0};
struct scm scm_symbol_pmatch_cdr = {TSYMBOL, "pmatch-cdr",0};

struct scm scm_vm_evlis = {TSPECIAL, "*vm-evlis*",0};
struct scm scm_vm_evlis2 = {TSPECIAL, "*vm-evlis2*",0};
struct scm scm_vm_evlis3 = {TSPECIAL, "*vm-evlis3*",0};
struct scm scm_vm_apply = {TSPECIAL, "core:apply",0};
struct scm scm_vm_apply2 = {TSPECIAL, "*vm-apply2*",0};
struct scm scm_vm_eval = {TSPECIAL, "core:eval-expanded",0};

struct scm scm_vm_eval_pmatch_car = {TSPECIAL, "*vm-eval-pmatch-car*",0};
struct scm scm_vm_eval_pmatch_cdr = {TSPECIAL, "*vm-eval-pmatch-cdr*",0};
struct scm scm_vm_eval_define = {TSPECIAL, "*vm-eval-define*",0};

struct scm scm_vm_eval_set_x = {TSPECIAL, "*vm-eval-set!*",0};
struct scm scm_vm_eval_macro_expand_eval = {TSPECIAL, "*vm:eval-macro-expand-eval*",0};
struct scm scm_vm_eval_macro_expand_expand = {TSPECIAL, "*vm:eval-macro-expand-expand*",0};
struct scm scm_vm_eval_check_func = {TSPECIAL, "*vm-eval-check-func*",0};
struct scm scm_vm_eval2 = {TSPECIAL, "*vm-eval2*",0};
struct scm scm_vm_macro_expand = {TSPECIAL, "core:macro-expand",0};
struct scm scm_vm_macro_expand_define = {TSPECIAL, "*vm:core:macro-expand-define*",0};
struct scm scm_vm_macro_expand_define_macro = {TSPECIAL, "*vm:core:macro-expand-define-macro*",0};
struct scm scm_vm_macro_expand_lambda = {TSPECIAL, "*vm:core:macro-expand-lambda*",0};
struct scm scm_vm_macro_expand_set_x = {TSPECIAL, "*vm:core:macro-expand-set!*",0};
struct scm scm_vm_begin_expand_primitive_load = {TSPECIAL, "*vm:core:begin-expand-primitive-load*",0};
struct scm scm_vm_begin_primitive_load = {TSPECIAL, "*vm:core:begin-primitive-load*",0};
struct scm scm_vm_macro_expand_car = {TSPECIAL, "*vm:core:macro-expand-car*",0};
struct scm scm_vm_macro_expand_cdr = {TSPECIAL, "*vm:macro-expand-cdr*",0};
struct scm scm_vm_begin_expand = {TSPECIAL, "core:eval",0};
struct scm scm_vm_begin_expand_eval = {TSPECIAL, "*vm:begin-expand-eval*",0};
struct scm scm_vm_begin_expand_macro = {TSPECIAL, "*vm:begin-expand-macro*",0};
struct scm scm_vm_begin = {TSPECIAL, "*vm-begin*",0};
struct scm scm_vm_begin_read_input_file = {TSPECIAL, "*vm-begin-read-input-file*",0};
struct scm scm_vm_begin_eval = {TSPECIAL, "*vm:begin-eval*",0};
struct scm scm_vm_if = {TSPECIAL, "*vm-if*",0};
struct scm scm_vm_if_expr = {TSPECIAL, "*vm-if-expr*",0};
struct scm scm_vm_call_with_values2 = {TSPECIAL, "*vm-call-with-values2*",0};
struct scm scm_vm_call_with_current_continuation2 = {TSPECIAL, "*vm-call-with-current-continuation2*",0};
struct scm scm_vm_return = {TSPECIAL, "*vm-return*",0};

struct scm scm_type_char = {TSYMBOL, "<cell:char>",0};
struct scm scm_type_closure = {TSYMBOL, "<cell:closure>",0};
struct scm scm_type_continuation = {TSYMBOL, "<cell:continuation>",0};
struct scm scm_type_function = {TSYMBOL, "<cell:function>",0};
struct scm scm_type_keyword = {TSYMBOL, "<cell:keyword>",0};
struct scm scm_type_macro = {TSYMBOL, "<cell:macro>",0};
struct scm scm_type_number = {TSYMBOL, "<cell:number>",0};
struct scm scm_type_pair = {TSYMBOL, "<cell:pair>",0};
struct scm scm_type_port = {TSYMBOL, "<cell:port>",0};
struct scm scm_type_ref = {TSYMBOL, "<cell:ref>",0};
struct scm scm_type_special = {TSYMBOL, "<cell:special>",0};
struct scm scm_type_string = {TSYMBOL, "<cell:string>",0};
struct scm scm_type_symbol = {TSYMBOL, "<cell:symbol>",0};
struct scm scm_type_values = {TSYMBOL, "<cell:values>",0};
struct scm scm_type_variable = {TSYMBOL, "<cell:variable>",0};
struct scm scm_type_vector = {TSYMBOL, "<cell:vector>",0};
struct scm scm_type_broken_heart = {TSYMBOL, "<cell:broken-heart>",0};

struct scm scm_symbol_compiler = {TSYMBOL, "%compiler",0};
struct scm scm_symbol_arch = {TSYMBOL, "%arch",0};

struct scm scm_test = {TSYMBOL, "%%test",0};

#if !_POSIX_SOURCE
#include "mes.mes.symbols.h"
#else
#include "mes.symbols.h"
#endif

struct function g_functions[200];
int g_function = 0;

#if !__GNUC__ || !_POSIX_SOURCE
#include "gc.mes.h"
#include "lib.mes.h"
#include "math.mes.h"
#include "mes.mes.h"
#include "posix.mes.h"
#include "reader.mes.h"
#include "vector.mes.h"
#else
#include "gc.h"
#include "lib.h"
#include "math.h"
#include "mes.h"
#include "posix.h"
#include "reader.h"
#include "vector.h"
#endif

#define TYPE(x) g_cells[x].type
#define CAR(x) g_cells[x].car
#define CDR(x) g_cells[x].cdr

#define NTYPE(x) g_news[x].type
#define NCAR(x) g_news[x].car
#define NCDR(x) g_news[x].cdr

#if !_POSIX_SOURCE
#define LENGTH(x) g_cells[x].car
#define REF(x) g_cells[x].car
#define STRING(x) g_cells[x].car
#define VARIABLE(x) g_cells[x].car

#define CLOSURE(x) g_cells[x].cdr
#define CONTINUATION(x) g_cells[x].cdr

#define FUNCTION(x) g_functions[g_cells[x].cdr]
#define FUNCTION0(x) g_functions[g_cells[x].cdr].function
#define MACRO(x) g_cells[x].cdr
#define PORT(x) g_cells[x].cdr
#define VALUE(x) g_cells[x].cdr
#define VECTOR(x) g_cells[x].cdr

#define NLENGTH(x) g_news[x].car

#define NVALUE(x) g_news[x].cdr
#define NVECTOR(x) g_news[x].cdr

#else
#define CONTINUATION(x) g_cells[x].cdr
#define HITS(x) g_cells[x].hits
#define LENGTH(x) g_cells[x].length
#define NAME(x) g_cells[x].name
#define STRING(x) g_cells[x].string
#define VARIABLE(x) g_cells[x].variable

#define CLOSURE(x) g_cells[x].closure
#define MACRO(x) g_cells[x].macro
#define PORT(x) g_cells[x].port
#define REF(x) g_cells[x].ref
#define VALUE(x) g_cells[x].value
#define VECTOR(x) g_cells[x].vector
#define FUNCTION(x) g_functions[g_cells[x].function]
#define FUNCTION0(x) g_functions[g_cells[x].function].function0

#define NLENGTH(x) g_news[x].length

#define NVALUE(x) g_news[x].value
#define NVECTOR(x) g_news[x].vector
#endif

#define MAKE_CHAR(n) make_cell__ (TCHAR, 0, n)
#define MAKE_CONTINUATION(n) make_cell__ (TCONTINUATION, n, g_stack)
#define MAKE_NUMBER(n) make_cell__ (TNUMBER, 0, n)
#define MAKE_REF(n) make_cell__ (TREF, n, 0)
#define MAKE_STRING(x) make_cell__ (TSTRING, x, 0)
#define MAKE_KEYWORD(x) make_cell__ (TKEYWORD, x, 0)
#define MAKE_STRING_PORT(x) make_cell__ (TPORT, x, -length__ (g_ports) - 2)
#define MAKE_MACRO(name, x) make_cell__ (TMACRO, STRING (name), x)

#define CAAR(x) CAR (CAR (x))
#define CADR(x) CAR (CDR (x))
#define CDAR(x) CDR (CAR (x))
#define CDDR(x) CDR (CDR (x))
#define CADAR(x) CAR (CDR (CAR (x)))
#define CADDR(x) CAR (CDR (CDR (x)))
#define CDADAR(x) CAR (CDR (CAR (CDR (x))))

SCM
alloc (long n)
{
  SCM x = g_free;
  g_free += n;
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
make_symbol_ (SCM s) ///((internal))
{
  SCM x = make_cell__ (TSYMBOL, s, 0);
  g_symbols = cons (x, g_symbols);
  return x;
}

SCM
list_of_char_equal_p (SCM a, SCM b) ///((internal))
{
  while (a != cell_nil && b != cell_nil && VALUE (CAR (a)) == VALUE (CAR (b)))
    {
      assert (TYPE (CAR (a)) == TCHAR);
      assert (TYPE (CAR (b)) == TCHAR);
      a = CDR (a);
      b = CDR (b);
    }
  return (a == cell_nil && b == cell_nil) ? cell_t : cell_f;
}

SCM
lookup_symbol_ (SCM s)
{
  SCM x = g_symbols;
  while (x)
    {
      if (list_of_char_equal_p (STRING (CAR (x)), s) == cell_t)
        break;
      x = CDR (x);
    }
  if (x)
    x = CAR (x);
  if (!x)
    x = make_symbol_ (s);
  return x;
}

SCM
type_ (SCM x)
{
  return MAKE_NUMBER (TYPE (x));
}

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
          && (TYPE (CDR (x)) == TPAIR
              || TYPE (CDR (x)) == TREF
              || TYPE (CDR (x)) == TSPECIAL
              || TYPE (CDR (x)) == TSYMBOL
              || TYPE (CDR (x)) == TSTRING)) ? CDR (x) : MAKE_NUMBER (CDR (x));
}

SCM
arity_ (SCM x)
{
  assert (TYPE (x) == TFUNCTION);
  return MAKE_NUMBER (FUNCTION (x).arity);
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
               && STRING (x) == STRING (y)))
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
  if ((throw = assq_ref_env (cell_symbol_throw, r0)) != cell_undefined)
    return apply (throw, cons (key, cons (x, cell_nil)), r0);
#endif
  display_error_ (key);
  eputs (": ");
  write_error_ (x);
  eputs ("\n");
  exit (1);
}

SCM
string_to_list (char const* s, long i)
{
  SCM p = cell_nil;
  while (i--)
    p = cons (MAKE_CHAR (s[i]), p);
  return p;
}

SCM
cstring_to_list (char const* s)
{
  return string_to_list (s, strlen (s));
}

//  extra lib
SCM
assert_defined (SCM x, SCM e) ///((internal))
{
  if (e == cell_undefined)
    return error (cell_symbol_unbound_variable, x);
  return e;
}

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
      SCM e = MAKE_STRING (cstring_to_list (s));
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
      SCM e = MAKE_STRING (cstring_to_list (s));
      return error (cell_symbol_wrong_type_arg, cons (e, f));
    }
  return cell_unspecified;
}

SCM
gc_push_frame () ///((internal))
{
  SCM frame = cons (r1, cons (r2, cons (r3, cons (r0, cell_nil))));
  g_stack = cons (frame, g_stack);
  return g_stack;
}

SCM
append2 (SCM x, SCM y)
{
  if (x == cell_nil)
    return y;
  if (TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cell_append2));
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
    error (cell_symbol_not_a_pair, cons (x, cell_append_reverse));
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
    error (cell_symbol_not_a_pair, cons (x, cell_reverse_x_));
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
call (SCM fn, SCM x)
{
#if __M2_PLANET__
  struct function *f = FUNCTION (fn);
#else
  struct function *f = &FUNCTION (fn);
#endif
  int arity = f->arity;
  if ((arity > 0 || arity == -1)
      && x != cell_nil && TYPE (CAR (x)) == TVALUES)
    x = cons (CADAR (x), CDR (x));
  if ((arity > 1 || arity == -1)
      && x != cell_nil && TYPE (CDR (x)) == TPAIR && TYPE (CADR (x)) == TVALUES)
    x = cons (CAR (x), cons (CDADAR (x), CDR (x)));

#if __M2_PLANET__
  FUNCTION fp = f->function;
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
#elif !POSIX
  if (arity == 0)
    {
      //function0_t fp = f->function;
      SCM (*fp) (void) = f->function;
      return fp ();
    }
  else if (arity == 1)
    {
      //function1_t fp = f->function;
      SCM (*fp) (SCM) = f->function;
      return fp (CAR (x));
    }
  else if (arity == 2)
    {
      //function2_t fp = f->function;
      SCM (*fp) (SCM, SCM) = f->function;
      return fp (CAR (x), CADR (x));
    }
  else if (arity == 3)
    {
      //function3_t fp = f->function;
      SCM (*fp) (SCM, SCM, SCM) = f->function;
      return fp (CAR (x), CADR (x), CAR (CDDR (x)));
    }
  else if (arity == -1)
    {
      //functionn_t fp = f->function;
      SCM (*fp) (SCM) = f->function;
      return fp (x);
    }
#else
  if (arity == 0)
    return FUNCTION (fn).function0 ();
  else if (arity == 1)
    return FUNCTION (fn).function1 (CAR (x));
  else if (arity == 2)
    return FUNCTION (fn).function2 (CAR (x), CADR (x));
  else if (arity == 3)
    return FUNCTION (fn).function3 (CAR (x), CADR (x), CAR (CDDR (x)));
  else if (arity == -1)
    return FUNCTION (fn).functionn (x);
#endif //! (__M2_PLANET__ || !POSIX)
  return cell_unspecified;
}

SCM
assq (SCM x, SCM a)
{
  if (TYPE (a) != TPAIR)
    return cell_f;
  int t = TYPE (x);
  if (t == TCHAR
      || t == TNUMBER)
      {
        SCM v = VALUE (x);
        while (a != cell_nil && v != VALUE (CAAR (a)))
          a = CDR (a);
      }
    else if (t == TKEYWORD)
      {
        SCM v = STRING (x);
        while (a != cell_nil && v != STRING (CAAR (a)))
          a = CDR (a);
      }
  // else if (t == TSYMBOL)
  // else if (t == TSPECIAL)
  else
    while (a != cell_nil && x != CAAR (a))
      a = CDR (a);
  return a != cell_nil ? CAR (a) : cell_f;
}

SCM
assq_ref_env (SCM x, SCM a)
{
  x = assq (x, a);
  if (x == cell_f)
    return cell_undefined;
  return CDR (x);
}

SCM
set_car_x (SCM x, SCM e)
{
  if (TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cell_set_car_x));
  CAR (x) = e;
  return cell_unspecified;
}

SCM
set_cdr_x (SCM x, SCM e)
{
  if (TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cell_set_cdr_x));
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
    p = assert_defined (x, assq (x, a));
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
  return make_cell__ (TCLOSURE, 0, cons (cons (cell_circular, a), cons (args, body)));
}

SCM
make_variable_ (SCM var) ///((internal))
{
  return make_cell__ (TVARIABLE, var, 0);
}

SCM
lookup_macro_ (SCM x, SCM a) ///((internal))
{
  if (TYPE (x) != TSYMBOL)
    return cell_f;
  SCM m = assq (x, a);
  if (m != cell_f)
    return MACRO (CDR (m));
  return cell_f;
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
gc_peek_frame () ///((internal))
{
  SCM frame = CAR (g_stack);
  r1 = CAR (frame);
  r2 = CADR (frame);
  r3 = CAR (CDDR (frame));
  r0 = CADR (CDDR (frame));
  return frame;
}

SCM
gc_pop_frame () ///((internal))
{
  SCM frame = gc_peek_frame (g_stack);
  g_stack = CDR (g_stack);
  return frame;
}

char const* string_to_cstring (SCM s);

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
                   && CAR (x) != cell_begin
                   && CAR (x) != cell_symbol_begin
                   && CAR (x) != cell_symbol_current_module
                   && CAR (x) != cell_symbol_primitive_load
                   && CAR (x) != cell_symbol_if // HMM
                   && !formal_p (CAR (x), formals))
            {
              SCM v = assq (CAR (x), r0);
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
  SCM x;
  int global_p;
  int macro_p;
  int t;
  int c;

 eval_apply:
  if (r3 == cell_vm_evlis) goto evlis;
  else if (r3 == cell_vm_evlis2) goto evlis2;
  else if (r3 == cell_vm_evlis3) goto evlis3;
  else if (r3 == cell_vm_apply) goto apply;
  else if (r3 == cell_vm_apply2) goto apply2;
  else if (r3 == cell_vm_eval) goto eval;
  else if (r3 == cell_vm_eval_pmatch_car) goto eval_pmatch_car;
  else if (r3 == cell_vm_eval_pmatch_cdr) goto eval_pmatch_cdr;
  else if (r3 == cell_vm_eval_define) goto eval_define;
  else if (r3 == cell_vm_eval_set_x) goto eval_set_x;
  else if (r3 == cell_vm_eval_macro_expand_eval) goto eval_macro_expand_eval;
  else if (r3 == cell_vm_eval_macro_expand_expand) goto eval_macro_expand_expand;
  else if (r3 == cell_vm_eval_check_func) goto eval_check_func;
  else if (r3 == cell_vm_eval2) goto eval2;
  else if (r3 == cell_vm_macro_expand) goto macro_expand;
  else if (r3 == cell_vm_macro_expand_define) goto macro_expand_define;
  else if (r3 == cell_vm_macro_expand_define_macro) goto macro_expand_define_macro;
  else if (r3 == cell_vm_macro_expand_lambda) goto macro_expand_lambda;
  else if (r3 == cell_vm_macro_expand_set_x) goto macro_expand_set_x;
  else if (r3 == cell_vm_macro_expand_car) goto macro_expand_car;
  else if (r3 == cell_vm_macro_expand_cdr) goto macro_expand_cdr;
  else if (r3 == cell_vm_begin) goto begin;
  else if (r3 == cell_vm_begin_eval) goto begin_eval;
  else if (r3 == cell_vm_begin_primitive_load) goto begin_primitive_load;
  else if (r3 == cell_vm_begin_expand) goto begin_expand;
  else if (r3 == cell_vm_begin_expand_eval) goto begin_expand_eval;
  else if (r3 == cell_vm_begin_expand_macro) goto begin_expand_macro;
  else if (r3 == cell_vm_begin_expand_primitive_load) goto begin_expand_primitive_load;
  else if (r3 == cell_vm_if) goto vm_if;
  else if (r3 == cell_vm_if_expr) goto if_expr;
  else if (r3 == cell_vm_call_with_current_continuation2) goto call_with_current_continuation2;
  else if (r3 == cell_vm_call_with_values2) goto call_with_values2;
  else if (r3 == cell_vm_return) goto vm_return;
  else if (r3 == cell_unspecified) return r1;
  else
    error (cell_symbol_system_error,
           MAKE_STRING (cstring_to_list ("eval/apply unknown continuation")));

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
  t = TYPE (CAR (r1));
  if (t == TFUNCTION)
    {
      check_formals (CAR (r1), MAKE_NUMBER (FUNCTION (CAR (r1)).arity), CDR (r1));
      r1 = call (CAR (r1), CDR (r1)); /// FIXME: move into eval_apply
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
      x = r1;
      g_stack = CONTINUATION (CAR (r1));
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
                        {
                          entry = cons (name, cell_f);
                          g_macros = cons (entry, g_macros);
                        }
                    }
                  else
                    {
                      entry = assq (name, r0);
                      if (entry == cell_f)
                        {
                          entry = cons (name, cell_f);
                          aa = cons (entry, cell_nil);
                          set_cdr_x (aa, cdr (r0));
                          set_cdr_x (r0, aa);
                        }
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
                  entry = assq (name, g_macros);
                  r1 = MAKE_MACRO (name, r1);
                  set_cdr_x (entry, r1);
                }
              else if (global_p)
                {
                  entry = assq (name, r0);
                  set_cdr_x (entry, r1);
                }
              else
                {
                  entry = cons (name, r1);
                  aa = cons (entry, cell_nil);
                  set_cdr_x (aa, cdr (r0));
                  set_cdr_x (r0, aa);
                  cl = assq (cell_closure, r0);
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
      if (r1 == cell_symbol_current_module)
        goto vm_return;
      if (r1 == cell_symbol_begin) // FIXME
        {
          r1 = cell_begin;
          goto vm_return;
        }
      r1 = assert_defined (r1, assq_ref_env (r1, r0));
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
        && (macro = lookup_macro_ (CAR (r1), g_macros)) != cell_f)
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
        && ((macro = assq (cell_symbol_portable_macro_expand, g_macros)) != cell_f)
        && ((expanders = assq_ref_env (cell_symbol_sc_expander_alist, r0)) != cell_undefined)
        && ((macro = assq (CAR (r1), expanders)) != cell_f))
      {
        sc_expand = assq_ref_env (cell_symbol_macro_expand, r0);
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
  gc_pop_frame ();
  push_cc (cons (CAR (r1), cons (x, cell_nil)), x, r0, cell_vm_call_with_current_continuation2);
  goto apply;
 call_with_current_continuation2:
  CONTINUATION (r2) = g_stack;
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
  r0 = a;
  r1 = MAKE_CHAR (0);
  r2 = MAKE_CHAR (0);
  r3 = MAKE_CHAR (0);
  g_stack = cons (cell_nil, cell_nil);
  return r0;
}

// Environment setup

#include "posix.c"
#include "math.c"
#include "lib.c"

// Jam Collector
SCM g_symbol_max;

SCM
gc_init_cells () ///((internal))
{
  g_cells = (struct scm *)malloc ((ARENA_SIZE+JAM_SIZE)*sizeof (struct scm));
  TYPE (0) = TVECTOR;
  LENGTH (0) = 1000;
  VECTOR (0) = 0;
  g_cells++;
  TYPE (0) = TCHAR;
  VALUE (0) = 'c';
  return 0;
}

SCM
mes_symbols () ///((internal))
{
  gc_init_cells ();

#if MES_MINI

g_free++;
g_cells[cell_nil] = scm_nil;

g_free++;
g_cells[cell_f] = scm_f;

g_free++;
g_cells[cell_t] = scm_t;

g_free++;
g_cells[cell_dot] = scm_dot;

g_free++;
g_cells[cell_arrow] = scm_arrow;

g_free++;
g_cells[cell_undefined] = scm_undefined;

g_free++;
g_cells[cell_unspecified] = scm_unspecified;

g_free++;
g_cells[cell_closure] = scm_closure;

g_free++;
g_cells[cell_circular] = scm_circular;

g_free++;
g_cells[cell_begin] = scm_begin;

g_free++;
g_cells[cell_symbol_dot] = scm_symbol_dot;

g_free++;
g_cells[cell_symbol_lambda] = scm_symbol_lambda;

g_free++;
g_cells[cell_symbol_begin] = scm_symbol_begin;

g_free++;
g_cells[cell_symbol_if] = scm_symbol_if;

g_free++;
g_cells[cell_symbol_quote] = scm_symbol_quote;

g_free++;
g_cells[cell_symbol_define] = scm_symbol_define;

g_free++;
g_cells[cell_symbol_define_macro] = scm_symbol_define_macro;

g_free++;
g_cells[cell_symbol_quasiquote] = scm_symbol_quasiquote;

g_free++;
g_cells[cell_symbol_unquote] = scm_symbol_unquote;

g_free++;
g_cells[cell_symbol_unquote_splicing] = scm_symbol_unquote_splicing;


////// for GC
g_free++;
g_cells[cell_symbol_syntax] = scm_symbol_syntax;

g_free++;
g_cells[cell_symbol_quasisyntax] = scm_symbol_quasisyntax;

g_free++;
g_cells[cell_symbol_unsyntax] = scm_symbol_unsyntax;

g_free++;
g_cells[cell_symbol_unsyntax_splicing] = scm_symbol_unsyntax_splicing;

g_free++;
g_cells[cell_symbol_set_x] = scm_symbol_set_x;

g_free++;
g_cells[cell_symbol_sc_expand] = scm_symbol_sc_expand;

g_free++;
g_cells[cell_symbol_macro_expand] = scm_symbol_macro_expand;

g_free++;
g_cells[cell_symbol_portable_macro_expand] = scm_symbol_portable_macro_expand;

g_free++;
g_cells[cell_symbol_sc_expander_alist] = scm_symbol_sc_expander_alist;

g_free++;
g_cells[cell_symbol_call_with_values] = scm_symbol_call_with_values;

g_free++;
g_cells[cell_call_with_current_continuation] = scm_call_with_current_continuation;

g_free++;
g_cells[cell_symbol_call_with_current_continuation] = scm_symbol_call_with_current_continuation;

g_free++;
g_cells[cell_symbol_current_module] = scm_symbol_current_module;

g_free++;
g_cells[cell_symbol_primitive_load] = scm_symbol_primitive_load;

g_free++;
g_cells[cell_symbol_read_input_file] = scm_symbol_read_input_file;

g_free++;
g_cells[cell_symbol_write] = scm_symbol_write;

g_free++;
g_cells[cell_symbol_display] = scm_symbol_display;

g_free++;
g_cells[cell_symbol_throw] = scm_symbol_throw;

g_free++;
g_cells[cell_symbol_not_a_number] = scm_symbol_not_a_number;

g_free++;
g_cells[cell_symbol_not_a_pair] = scm_symbol_not_a_pair;

g_free++;
g_cells[cell_symbol_system_error] = scm_symbol_system_error;

g_free++;
g_cells[cell_symbol_wrong_number_of_args] = scm_symbol_wrong_number_of_args;

g_free++;
g_cells[cell_symbol_wrong_type_arg] = scm_symbol_wrong_type_arg;

g_free++;
g_cells[cell_symbol_unbound_variable] = scm_symbol_unbound_variable;

g_free++;
g_cells[cell_symbol_argv] = scm_symbol_argv;

g_free++;
g_cells[cell_symbol_mes_prefix] = scm_symbol_mes_prefix;

g_free++;
g_cells[cell_symbol_mes_version] = scm_symbol_mes_version;

g_free++;
g_cells[cell_symbol_car] = scm_symbol_car;

g_free++;
g_cells[cell_symbol_cdr] = scm_symbol_cdr;

g_free++;
g_cells[cell_symbol_pmatch_car] = scm_symbol_pmatch_car;

g_free++;
g_cells[cell_symbol_pmatch_cdr] = scm_symbol_pmatch_cdr;

g_free++;
g_cells[cell_vm_evlis] = scm_vm_evlis;

g_free++;
g_cells[cell_vm_evlis2] = scm_vm_evlis2;

g_free++;
g_cells[cell_vm_evlis3] = scm_vm_evlis3;

g_free++;
g_cells[cell_vm_apply] = scm_vm_apply;

g_free++;
g_cells[cell_vm_apply2] = scm_vm_apply2;

g_free++;
g_cells[cell_vm_eval] = scm_vm_eval;

g_free++;
g_cells[cell_vm_eval_pmatch_car] = scm_vm_eval_pmatch_car;

g_free++;
g_cells[cell_vm_eval_pmatch_cdr] = scm_vm_eval_pmatch_cdr;

g_free++;
g_cells[cell_vm_eval_define] = scm_vm_eval_define;

g_free++;
g_cells[cell_vm_eval_set_x] = scm_vm_eval_set_x;

g_free++;
g_cells[cell_vm_eval_macro_expand_eval] = scm_vm_eval_macro_expand_eval;

g_free++;
g_cells[cell_vm_eval_macro_expand_expand] = scm_vm_eval_macro_expand_expand;

g_free++;
g_cells[cell_vm_eval_check_func] = scm_vm_eval_check_func;

g_free++;
g_cells[cell_vm_eval2] = scm_vm_eval2;

g_free++;
g_cells[cell_vm_macro_expand] = scm_vm_macro_expand;

g_free++;
g_cells[cell_vm_macro_expand_define] = scm_vm_macro_expand_define;

g_free++;
g_cells[cell_vm_macro_expand_define_macro] = scm_vm_macro_expand_define_macro;

g_free++;
g_cells[cell_vm_macro_expand_lambda] = scm_vm_macro_expand_lambda;

g_free++;
g_cells[cell_vm_macro_expand_set_x] = scm_vm_macro_expand_set_x;

g_free++;
g_cells[cell_vm_begin_expand_primitive_load] = scm_vm_begin_expand_primitive_load;

g_free++;
g_cells[cell_vm_begin_primitive_load] = scm_vm_begin_primitive_load;

g_free++;
g_cells[cell_vm_macro_expand_car] = scm_vm_macro_expand_car;

g_free++;
g_cells[cell_vm_macro_expand_cdr] = scm_vm_macro_expand_cdr;

g_free++;
g_cells[cell_vm_begin_expand] = scm_vm_begin_expand;

g_free++;
g_cells[cell_vm_begin_expand_eval] = scm_vm_begin_expand_eval;

g_free++;
g_cells[cell_vm_begin_expand_macro] = scm_vm_begin_expand_macro;

g_free++;
g_cells[cell_vm_begin] = scm_vm_begin;

g_free++;
g_cells[cell_vm_begin_read_input_file] = scm_vm_begin_read_input_file;

g_free++;
g_cells[cell_vm_begin_eval] = scm_vm_begin_eval;

g_free++;
g_cells[cell_vm_if] = scm_vm_if;

g_free++;
g_cells[cell_vm_if_expr] = scm_vm_if_expr;

g_free++;
g_cells[cell_vm_call_with_values2] = scm_vm_call_with_values2;

g_free++;
g_cells[cell_vm_call_with_current_continuation2] = scm_vm_call_with_current_continuation2;

g_free++;
g_cells[cell_vm_return] = scm_vm_return;

g_free++;
g_cells[cell_symbol_compiler] = scm_symbol_compiler;

g_free++;
g_cells[cell_symbol_arch] = scm_symbol_arch;

g_free++;
g_cells[cell_test] = scm_test;
////////////





#elif !_POSIX_SOURCE
#include "mes.mes.symbols.i"
#else
#include "mes.symbols.i"
#endif

  g_symbol_max = g_free++;
  g_symbols = 0;
  for (int i=1; i<g_symbol_max; i++)
    g_symbols = cons (i, g_symbols);

  SCM a = cell_nil;

#if MES_MINI

g_cells[cell_nil].car = cstring_to_list (scm_nil.car);
g_cells[cell_f].car = cstring_to_list (scm_f.car);
g_cells[cell_t].car = cstring_to_list (scm_t.car);
g_cells[cell_dot].car = cstring_to_list (scm_dot.car);
g_cells[cell_arrow].car = cstring_to_list (scm_arrow.car);
g_cells[cell_undefined].car = cstring_to_list (scm_undefined.car);
g_cells[cell_unspecified].car = cstring_to_list (scm_unspecified.car);
g_cells[cell_closure].car = cstring_to_list (scm_closure.car);
g_cells[cell_circular].car = cstring_to_list (scm_circular.car);
g_cells[cell_begin].car = cstring_to_list (scm_begin.car);
g_cells[cell_symbol_dot].car = cstring_to_list (scm_symbol_dot.car);
g_cells[cell_symbol_lambda].car = cstring_to_list (scm_symbol_lambda.car);
g_cells[cell_symbol_begin].car = cstring_to_list (scm_symbol_begin.car);
g_cells[cell_symbol_if].car = cstring_to_list (scm_symbol_if.car);
g_cells[cell_symbol_quote].car = cstring_to_list (scm_symbol_quote.car);
g_cells[cell_symbol_define].car = cstring_to_list (scm_symbol_define.car);
g_cells[cell_symbol_define_macro].car = cstring_to_list (scm_symbol_define_macro.car);
g_cells[cell_symbol_quasiquote].car = cstring_to_list (scm_symbol_quasiquote.car);
g_cells[cell_symbol_unquote].car = cstring_to_list (scm_symbol_unquote.car);
g_cells[cell_symbol_unquote_splicing].car = cstring_to_list (scm_symbol_unquote_splicing.car);


//// FOR GC
g_cells[cell_symbol_syntax].car = cstring_to_list (scm_symbol_syntax.name);
g_cells[cell_symbol_quasisyntax].car = cstring_to_list (scm_symbol_quasisyntax.name);
g_cells[cell_symbol_unsyntax].car = cstring_to_list (scm_symbol_unsyntax.name);
g_cells[cell_symbol_unsyntax_splicing].car = cstring_to_list (scm_symbol_unsyntax_splicing.name);
g_cells[cell_symbol_set_x].car = cstring_to_list (scm_symbol_set_x.name);
g_cells[cell_symbol_sc_expand].car = cstring_to_list (scm_symbol_sc_expand.name);
g_cells[cell_symbol_macro_expand].car = cstring_to_list (scm_symbol_macro_expand.name);
g_cells[cell_symbol_portable_macro_expand].car = cstring_to_list (scm_symbol_portable_macro_expand.name);
g_cells[cell_symbol_sc_expander_alist].car = cstring_to_list (scm_symbol_sc_expander_alist.name);
g_cells[cell_symbol_call_with_values].car = cstring_to_list (scm_symbol_call_with_values.name);
g_cells[cell_call_with_current_continuation].car = cstring_to_list (scm_call_with_current_continuation.name);
g_cells[cell_symbol_call_with_current_continuation].car = cstring_to_list (scm_symbol_call_with_current_continuation.name);
g_cells[cell_symbol_current_module].car = cstring_to_list (scm_symbol_current_module.name);
g_cells[cell_symbol_primitive_load].car = cstring_to_list (scm_symbol_primitive_load.name);
g_cells[cell_symbol_read_input_file].car = cstring_to_list (scm_symbol_read_input_file.name);
g_cells[cell_symbol_write].car = cstring_to_list (scm_symbol_write.name);
g_cells[cell_symbol_display].car = cstring_to_list (scm_symbol_display.name);
g_cells[cell_symbol_throw].car = cstring_to_list (scm_symbol_throw.name);
g_cells[cell_symbol_not_a_number].car = cstring_to_list (scm_symbol_not_a_number.name);
g_cells[cell_symbol_not_a_pair].car = cstring_to_list (scm_symbol_not_a_pair.name);
g_cells[cell_symbol_system_error].car = cstring_to_list (scm_symbol_system_error.name);
g_cells[cell_symbol_wrong_number_of_args].car = cstring_to_list (scm_symbol_wrong_number_of_args.name);
g_cells[cell_symbol_wrong_type_arg].car = cstring_to_list (scm_symbol_wrong_type_arg.name);
g_cells[cell_symbol_unbound_variable].car = cstring_to_list (scm_symbol_unbound_variable.name);
g_cells[cell_symbol_argv].car = cstring_to_list (scm_symbol_argv.name);
g_cells[cell_symbol_mes_prefix].car = cstring_to_list (scm_symbol_mes_prefix.name);
g_cells[cell_symbol_mes_version].car = cstring_to_list (scm_symbol_mes_version.name);
g_cells[cell_symbol_car].car = cstring_to_list (scm_symbol_car.name);
g_cells[cell_symbol_cdr].car = cstring_to_list (scm_symbol_cdr.name);
g_cells[cell_symbol_pmatch_car].car = cstring_to_list (scm_symbol_pmatch_car.name);
g_cells[cell_symbol_pmatch_cdr].car = cstring_to_list (scm_symbol_pmatch_cdr.name);

g_cells[cell_vm_evlis].car = cstring_to_list ("*vm*");
g_cells[cell_vm_evlis2].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_evlis3].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_apply].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_apply2].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_eval].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_eval_pmatch_car].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_eval_pmatch_cdr].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_eval_define].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_eval_set_x].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_eval_macro_expand_eval].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_eval_macro_expand_expand].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_eval_check_func].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_eval2].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_macro_expand].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_macro_expand_define].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_macro_expand_define_macro].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_macro_expand_lambda].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_macro_expand_set_x].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_begin_expand_primitive_load].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_begin_primitive_load].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_macro_expand_car].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_macro_expand_cdr].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_begin_expand].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_begin_expand_eval].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_begin_expand_macro].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_begin].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_begin_read_input_file].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_begin_eval].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_if].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_if_expr].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_call_with_values2].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_call_with_current_continuation2].car = g_cells[cell_vm_evlis].car;
g_cells[cell_vm_return].car = g_cells[cell_vm_evlis].car;

g_cells[cell_symbol_compiler].car = cstring_to_list (scm_symbol_compiler.name);
g_cells[cell_symbol_arch].car = cstring_to_list (scm_symbol_arch.name);
g_cells[cell_test].car = cstring_to_list (scm_test.name);
////////////////// gc

#elif !_POSIX_SOURCE
#include "mes.mes.symbol-names.i"
#else
#include "mes.symbol-names.i"
#endif

#if !MES_MINI
  a = acons (cell_symbol_call_with_values, cell_symbol_call_with_values, a);
  a = acons (cell_symbol_current_module, cell_symbol_current_module, a);
  a = acons (cell_symbol_call_with_current_continuation, cell_call_with_current_continuation, a);

  a = acons (cell_symbol_mes_version, MAKE_STRING (cstring_to_list (VERSION)), a);
  a = acons (cell_symbol_mes_prefix, MAKE_STRING (cstring_to_list (PREFIX)), a);

  char *compiler = "gcc";
#if __MESC__
  compiler = "mescc";
#elif __TINYC__
  compiler = "tcc";
#endif
  a = acons (cell_symbol_compiler, MAKE_STRING (cstring_to_list (compiler)), a);

  char *arch = "x86";
#if __x86_64__
  arch = "x86_64";
#endif
  a = acons (cell_symbol_arch, MAKE_STRING (cstring_to_list (arch)), a);

#endif // !MES_MINI

  a = acons (cell_type_char, MAKE_NUMBER (TCHAR), a);
  a = acons (cell_type_closure, MAKE_NUMBER (TCLOSURE), a);
  a = acons (cell_type_continuation, MAKE_NUMBER (TCONTINUATION), a);
  a = acons (cell_type_function, MAKE_NUMBER (TFUNCTION), a);
  a = acons (cell_type_keyword, MAKE_NUMBER (TKEYWORD), a);
  a = acons (cell_type_macro, MAKE_NUMBER (TMACRO), a);
  a = acons (cell_type_number, MAKE_NUMBER (TNUMBER), a);
  a = acons (cell_type_pair, MAKE_NUMBER (TPAIR), a);
  a = acons (cell_type_port, MAKE_NUMBER (TPORT), a);
  a = acons (cell_type_ref, MAKE_NUMBER (TREF), a);
  a = acons (cell_type_special, MAKE_NUMBER (TSPECIAL), a);
  a = acons (cell_type_string, MAKE_NUMBER (TSTRING), a);
  a = acons (cell_type_symbol, MAKE_NUMBER (TSYMBOL), a);
  a = acons (cell_type_values, MAKE_NUMBER (TVALUES), a);
  a = acons (cell_type_variable, MAKE_NUMBER (TVARIABLE), a);
  a = acons (cell_type_vector, MAKE_NUMBER (TVECTOR), a);
  a = acons (cell_type_broken_heart, MAKE_NUMBER (TBROKEN_HEART), a);

  a = acons (cell_closure, a, a);

  return a;
}

SCM
mes_environment () ///((internal))
{
  SCM a = mes_symbols ();
  return mes_g_stack (a);
}

SCM
mes_builtins (SCM a) ///((internal))
{
#if MES_MINI

// GCC
//mes
scm_cons.function = g_function;
g_functions[g_function++] = fun_cons;
cell_cons = g_free++;
g_cells[cell_cons] = scm_cons;

scm_car.function = g_function;
g_functions[g_function++] = fun_car;
cell_car = g_free++;
g_cells[cell_car] = scm_car;

scm_cdr.function = g_function;
g_functions[g_function++] = fun_cdr;
cell_cdr = g_free++;
g_cells[cell_cdr] = scm_cdr;

scm_list.function = g_function;
g_functions[g_function++] = fun_list;
cell_list = g_free++;
g_cells[cell_list] = scm_list;

scm_null_p.function = g_function;
g_functions[g_function++] = fun_null_p;
cell_null_p = g_free++;
g_cells[cell_null_p] = scm_null_p;

scm_eq_p.function = g_function;
g_functions[g_function++] = fun_eq_p;
cell_eq_p = g_free++;
g_cells[cell_eq_p] = scm_eq_p;

//math
scm_minus.function = g_function;
g_functions[g_function++] = fun_minus;
cell_minus = g_free++;
g_cells[cell_minus] = scm_minus;

scm_plus.function = g_function;
g_functions[g_function++] = fun_plus;
cell_plus = g_free++;
g_cells[cell_plus] = scm_plus;

//lib
scm_display_.function = g_function;
g_functions[g_function++] = fun_display_;
cell_display_ = g_free++;
g_cells[cell_display_] = scm_display_;

scm_display_error_.function = g_function;
g_functions[g_function++] = fun_display_error_;
cell_display_error_ = g_free++;
g_cells[cell_display_error_] = scm_display_error_;



//mes.environment
scm_cons.string = cstring_to_list (fun_cons.name);
g_cells[cell_cons].string = MAKE_STRING (scm_cons.string);
a = acons (lookup_symbol_ (scm_cons.string), cell_cons, a);

scm_car.string = cstring_to_list (fun_car.name);
g_cells[cell_car].string = MAKE_STRING (scm_car.string);
a = acons (lookup_symbol_ (scm_car.string), cell_car, a);

scm_cdr.string = cstring_to_list (fun_cdr.name);
g_cells[cell_cdr].string = MAKE_STRING (scm_cdr.string);
a = acons (lookup_symbol_ (scm_cdr.string), cell_cdr, a);

scm_list.string = cstring_to_list (fun_list.name);
g_cells[cell_list].string = MAKE_STRING (scm_list.string);
a = acons (lookup_symbol_ (scm_list.string), cell_list, a);

scm_null_p.string = cstring_to_list (fun_null_p.name);
g_cells[cell_null_p].string = MAKE_STRING (scm_null_p.string);
a = acons (lookup_symbol_ (scm_null_p.string), cell_null_p, a);

scm_eq_p.string = cstring_to_list (fun_eq_p.name);
g_cells[cell_eq_p].string = MAKE_STRING (scm_eq_p.string);
a = acons (lookup_symbol_ (scm_eq_p.string), cell_eq_p, a);

//math.environment
 scm_minus.string = cstring_to_list (fun_minus.name);
g_cells[cell_minus].string = MAKE_STRING (scm_minus.string);
a = acons (lookup_symbol_ (scm_minus.string), cell_minus, a);

scm_plus.string = cstring_to_list (fun_plus.name);
g_cells[cell_plus].string = MAKE_STRING (scm_plus.string);
a = acons (lookup_symbol_ (scm_plus.string), cell_plus, a);

//lib.environment
scm_display_.string = cstring_to_list (fun_display_.name);
g_cells[cell_display_].string = MAKE_STRING (scm_display_.string);
a = acons (lookup_symbol_ (scm_display_.string), cell_display_, a);

scm_display_error_.string = cstring_to_list (fun_display_error_.name);
g_cells[cell_display_error_].string = MAKE_STRING (scm_display_error_.string);
a = acons (lookup_symbol_ (scm_display_error_.string), cell_display_error_, a);


// MESC/MES
//mes
// scm_cons.cdr = g_function;
// g_functions[g_function++] = fun_cons;
// cell_cons = g_free++;
// g_cells[cell_cons] = scm_cons;

// scm_car.cdr = g_function;
// g_functions[g_function++] = fun_car;
// cell_car = g_free++;
// g_cells[cell_car] = scm_car;

// scm_cdr.cdr = g_function;
// g_functions[g_function++] = fun_cdr;
// cell_cdr = g_free++;
// g_cells[cell_cdr] = scm_cdr;

// scm_list.cdr = g_function;
// g_functions[g_function++] = fun_list;
// cell_list = g_free++;
// g_cells[cell_list] = scm_list;

// scm_null_p.cdr = g_function;
// g_functions[g_function++] = fun_null_p;
// cell_null_p = g_free++;
// g_cells[cell_null_p] = scm_null_p;

// scm_eq_p.cdr = g_function;
// g_functions[g_function++] = fun_eq_p;
// cell_eq_p = g_free++;
// g_cells[cell_eq_p] = scm_eq_p;

//lib
// scm_display_.cdr = g_function;
// g_functions[g_function++] = fun_display_;
// cell_display_ = g_free++;
// g_cells[cell_display_] = scm_display_;

// scm_display_error_.cdr = g_function;
// g_functions[g_function++] = fun_display_error_;
// cell_display_error_ = g_free++;
// g_cells[cell_display_error_] = scm_display_error_;



#elif !__GNUC__ || !_POSIX_SOURCE
#include "mes.mes.i"

  // Do not sort: Order of these includes define builtins
#include "posix.mes.i"
#include "math.mes.i"
#include "lib.mes.i"
#include "vector.mes.i"
#include "gc.mes.i"
#include "reader.mes.i"

#include "gc.mes.environment.i"
#include "lib.mes.environment.i"
#include "math.mes.environment.i"
#include "mes.mes.environment.i"
#include "posix.mes.environment.i"
#include "reader.mes.environment.i"
#include "vector.mes.environment.i"
#else
#include "mes.i"

  // Do not sort: Order of these includes define builtins
#include "posix.i"
#include "math.i"
#include "lib.i"
#include "vector.i"
#include "gc.i"
#include "reader.i"

#include "gc.environment.i"
#include "lib.environment.i"
#include "math.environment.i"
#include "mes.environment.i"
#include "posix.environment.i"
#include "reader.environment.i"
#include "vector.environment.i"
#endif

  if (g_debug > 3)
    {
      fdputs ("functions: ", STDERR);
      fdputs (itoa (g_function), STDERR);
      fdputs ("\n", STDERR);
      for (int i = 0; i < g_function; i++)
        {
          fdputs ("[", STDERR);
          fdputs (itoa (i), STDERR);
          fdputs ("]: ", STDERR);
          fdputs (g_functions[i].name, STDERR);
          fdputs ("\n", STDERR);
        }
      fdputs ("\n", STDERR);
    }

  return a;
}

SCM read_input_file_env (SCM);

int
load_boot (char *prefix, char const *boot, char const *location)
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
load_env (SCM a) ///((internal))
{
  r0 = a;
  g_stdin = -1;
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
      g_stdin = load_boot (prefix, boot, "MES_PREFIX");
    }
  if (g_stdin < 0)
    {
      char const *p = MODULEDIR "/mes/";
      strcpy (prefix, p);
      g_stdin = load_boot (prefix, boot, "MODULEDIR");
    }
  if (g_stdin < 0)
    {
      strcpy (prefix, "mes/module/mes/");
      g_stdin = load_boot (prefix, boot, ".");
    }
  if (g_stdin < 0)
    {
      prefix[0] = 0;
      g_stdin = load_boot (prefix, boot, "<boot>");
    }
  if (g_stdin < 0)
    {
      eputs ("mes: boot failed: no such file: ");
      eputs (boot);
      eputs ("\n");
      exit (1);
    }

  if (!g_function)
    r0 = mes_builtins (r0);
  r2 = read_input_file_env (r0);
  g_stdin = STDIN;
  return r2;
}

SCM
bload_env (SCM a) ///((internal))
{
#if !_POSIX_SOURCE
  char *mo = "mes/boot-0.32-mo";
  g_stdin = open ("module/mes/boot-0.32-mo", O_RDONLY);
  char *read0 = MODULEDIR "/mes/boot-0.32-mo";
  g_stdin = g_stdin >= 0 ? g_stdin : open (read0, O_RDONLY);
#else
  char *mo ="mes/boot-0.mo";
  g_stdin = open ("module/mes/boot-0.mo", O_RDONLY);
  g_stdin = g_stdin >= 0 ? g_stdin : open (MODULEDIR "/mes/boot-0.mo", O_RDONLY);
#endif

  if (g_stdin < 0)
    {
      eputs ("no such file: ");
      eputs (mo);
      eputs ("\n");
      return 1;
    }
  assert (getchar () == 'M');
  assert (getchar () == 'E');
  assert (getchar () == 'S');

  if (g_debug)
    eputs ("*GOT MES*\n");
  g_stack = getchar () << 8;
  g_stack += getchar ();

  char *p = (char*)g_cells;
  int c = getchar ();
  while (c != EOF)
    {
      *p++ = c;
      c = getchar ();
    }
  g_free = (p-(char*)g_cells) / sizeof (struct scm);
  gc_peek_frame ();
  g_symbols = r1;
  g_stdin = STDIN;
  r0 = mes_builtins (r0);

  char *compiler = "gcc";
#if __MESC__
  compiler = "mescc";
#elif __TINYC__
  compiler = "tcc";
#endif
  a = acons (cell_symbol_compiler, MAKE_STRING (cstring_to_list (compiler)), a);

  char *arch = "x86";
#if __x86_64__
  arch = "x86_64";
#endif
  a = acons (cell_symbol_arch, MAKE_STRING (cstring_to_list (arch)), a);

  if (g_debug > 3)
    {
      eputs ("symbols: ");
      SCM s = g_symbols;
      while (s && s != cell_nil)
        {
          display_error_ (CAR (s));
          eputs (" ");
          s = CDR (s);
        }
      eputs ("\n");
      eputs ("functions: ");
      eputs (itoa (g_function));
      eputs ("\n");
      for (int i = 0; i < g_function; i++)
        {
          eputs ("[");
          eputs (itoa (i));
          eputs ("]: ");
          eputs (g_functions[i].name);
          eputs ("\n");
        }
    }
  return r2;
}

#include "vector.c"
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
  g_stdin = STDIN;
  g_stdout = STDOUT;
  r0 = mes_environment ();

  SCM program = (argc > 1 && !strcmp (argv[1], "--load"))
    ? bload_env (r0) : load_env (r0);
  g_tiny = argc > 2 && !strcmp (argv[2], "--tiny");
  if (argc > 1 && !strcmp (argv[1], "--dump"))
    return dump ();

#if !MES_MINI
  SCM lst = cell_nil;
  for (int i=argc-1; i>=0; i--)
    lst = cons (MAKE_STRING (cstring_to_list (argv[i])), lst);
  r0 = acons (cell_symbol_argv, lst, r0); // FIXME
  r0 = acons (cell_symbol_argv, lst, r0);
#endif
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
  if (g_debug > 3)
    {
      eputs ("symbols: ");
      write_error_ (g_symbols);
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
      eputs ("\ngc stats: [");
      eputs (itoa (g_free));
      MAX_ARENA_SIZE = 0;
      gc (g_stack);
      eputs (" => ");
      eputs (itoa (g_free));
      eputs ("]\n");
    }
  return 0;
}
