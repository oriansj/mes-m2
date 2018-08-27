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

#define MES_MINI 1
//#define HAVE_UNION 1
#if POSIX
#error "POSIX not supported"
#endif

#include <libmes.h>

#include <stdio.h>
#include <assert.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>

typedef long SCM;

#if __M2_PLANET__

int ARENA_SIZE;
int MAX_ARENA_SIZE;
int JAM_SIZE;
int GC_SAFETY;

char *g_arena;

int g_debug;
int g_free;

SCM g_continuations;
SCM g_symbols;
SCM g_macros;
SCM g_ports;
SCM g_stack;
// a/env
SCM r0;
// param 1
SCM r1;
// save 2+load/dump
SCM r2;
// continuation
SCM r3;

#else // !__M2_PLANET__

int ARENA_SIZE = 200000; // 32b: 2MiB, 64b: 4 MiB
int MAX_ARENA_SIZE = 300000000;
int JAM_SIZE = 20000;
int GC_SAFETY = 2000;

char *g_arena = 0;

int g_debug = 0;
int g_free = 0;

SCM g_continuations = 0;
SCM g_symbols = 0;
SCM g_macros = 0;
SCM g_ports = 0;
SCM g_stack = 0;
// a/env
SCM r0 = 0;
// param 1
SCM r1 = 0;
// save 2+load/dump
SCM r2 = 0;
// continuation
SCM r3 = 0;
#endif // !__M2_PLANET__

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
  int arity;
  char *name;
};

#if __M2_PLANET__
struct scm *g_cells;
struct scm *g_news;
int g_function;
struct function *g_functions;
#elif __MESC__
//FIXME
char *foobar = 0;
struct scm *g_cells = foobar;
struct scm *g_news = foobar;
int g_function = 0;
struct function g_functions[200];
#else
struct scm *g_cells = 0;
struct scm *g_news = 0;
int g_function = 0;
struct function g_functions[200];
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
struct scm scm_vm_eval = {TSPECIAL, "core:eval-sexp",0};

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

struct scm scm_test = {TSYMBOL, "test",0};

#include "mes.mes.symbols.h"

#include "gc.mes.h"
#include "lib.mes.h"
#if !MES_MINI
#include "math.mes.h"
#endif
#include "mes.mes.h"

SCM gc_init_news ();

// #if !MES_MINI
// #include "posix.mes.h"
// #ndif
//#include "vector.mes.h"

#if __M2_PLANET__
#define struct_function_size 12
#define struct_size 12
#define TYPE(x) ((x*struct_size)+g_cells)->type

#define CAR(x) ((x*struct_size)+g_cells)->car

#define CDR(x) ((x*struct_size)+g_cells)->cdr
#define FUNCTION(x) ((((x*struct_size)+g_cells)->cdr*struct_function_size)+g_functions)
#define VALUE(x) ((x*struct_size)+g_cells)->cdr

#else // !__M2_PLANET__

#define TYPE(x) g_cells[x].type
#define CAR(x) g_cells[x].car
#define CDR(x) g_cells[x].cdr

#define NTYPE(x) g_news[x].type
#define NCAR(x) g_news[x].car
#define NCDR(x) g_news[x].cdr

#define LENGTH(x) g_cells[x].car
#define REF(x) g_cells[x].car
#define STRING(x) g_cells[x].car
#define VARIABLE(x) g_cells[x].car

#define CLOSURE(x) g_cells[x].cdr
#define CONTINUATION(x) g_cells[x].cdr

#define FUNCTION(x) g_functions[g_cells[x].cdr]
#define MACRO(x) g_cells[x].cdr
#define PORT(x) g_cells[x].cdr
#define VALUE(x) g_cells[x].cdr
#define VECTOR(x) g_cells[x].cdr

#define NLENGTH(x) g_news[x].car

#define NVALUE(x) g_news[x].cdr
#define NVECTOR(x) g_news[x].cdr

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

#endif // !__M2_PLANET__

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
  while (a != cell_nil && b != cell_nil && VALUE (CAR (a)) == VALUE (CAR (b))) {
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
  return (TYPE (CDR (x)) == TPAIR
          || TYPE (CDR (x)) == TREF
          || TYPE (CAR (x)) == TSPECIAL
          || TYPE (CDR (x)) == TSYMBOL
          || TYPE (CDR (x)) == TSTRING) ? CDR (x) : MAKE_NUMBER (CDR (x));
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
  if (TYPE (x) != TPAIR) error (cell_symbol_not_a_pair, cons (x, cell_symbol_car));
  return CAR (x);
}

SCM
cdr (SCM x)
{
  if (TYPE (x) != TPAIR) error (cell_symbol_not_a_pair, cons (x, cell_symbol_cdr));
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

SCM
length (SCM x)
{
  int n = 0;
  while (x != cell_nil)
    {
      n++;
      if (TYPE (x) != TPAIR) return MAKE_NUMBER (-1);
      x = CDR (x);
    }
  return MAKE_NUMBER (n);
}

SCM apply (SCM, SCM, SCM);

SCM
error (SCM key, SCM x)
{
  SCM throw;
  if ((throw = assq_ref_env (cell_symbol_throw, r0)) != cell_undefined)
    return apply (throw, cons (key, cons (x, cell_nil)), r0);
  display_error_ (key);
  eputs (": ");
  display_error_ (x);
  eputs ("\n");
  exit (1);
}

SCM
cstring_to_list (char const* s)
{
  SCM p = cell_nil;
  int i = strlen (s);
  while (i--)
    p = cons (MAKE_CHAR (s[i]), p);
  return p;
}

//  extra lib
SCM
assert_defined (SCM x, SCM e) ///((internal))
{
  if (e == cell_undefined) return error (cell_symbol_unbound_variable, x);
  return e;
}

SCM
check_formals (SCM f, SCM formals, SCM args) ///((internal))
{
  int flen = (TYPE (formals) == TNUMBER) ? VALUE (formals) : VALUE (length (formals));
  int alen = VALUE (length (args));
  if (alen != flen && alen != -1 && flen != -1)
    {
      char *s = "apply: wrong number of arguments; expected: ";
      eputs (s);
      eputs (itoa (flen));
      eputs (", got: ");
      eputs (itoa (alen));
      eputs ("\n");
      display_error_ (f);
      SCM e = MAKE_STRING (cstring_to_list (s));
      return error (cell_symbol_wrong_number_of_args, cons (e, f));
    }
  return cell_unspecified;
}

SCM
check_apply (SCM f, SCM e) ///((internal))
{
  char* type = 0;
  if (f == cell_f || f == cell_t) type = "bool";
  if (f == cell_nil) type = "nil";
  if (f == cell_unspecified) type = "*unspecified*";
  if (f == cell_undefined) type = "*undefined*";
  if (TYPE (f) == TCHAR) type = "char";
  if (TYPE (f) == TNUMBER) type = "number";
  if (TYPE (f) == TSTRING) type = "string";

  if (type)
    {
      char *s = "cannot apply: ";
      eputs (s);
      eputs (type);
      eputs ("[");
      display_error_ (e);
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
  if (TYPE (x) != TPAIR)
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
  //FIXME: move into fast-non eq_p-ing assq core:assq?
  //while (a != cell_nil && x != CAAR (a)) a = CDR (a);
  while (a != cell_nil && eq_p (x, CAAR (a)) == cell_f) a = CDR (a);
  return a != cell_nil ? CAR (a) : cell_f;
}

SCM
assq_ref_env (SCM x, SCM a)
{
  x = assq (x, a);
  if (x == cell_f) return cell_undefined;
  return CDR (x);
}

SCM
set_car_x (SCM x, SCM e)
{
  assert (TYPE (x) == TPAIR);
  CAR (x) = e;
  return cell_unspecified;
}

SCM
set_cdr_x (SCM x, SCM e)
{
  if (TYPE (x) != TPAIR) error (cell_symbol_not_a_pair, cons (x, cell_set_cdr_x));
  CDR (x) = e;
  return cell_unspecified;
}

SCM
set_env_x (SCM x, SCM e, SCM a)
{
  SCM p = assert_defined (x, assq (x, a));
  if (TYPE (p) != TPAIR) error (cell_symbol_not_a_pair, cons (p, x));
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

SCM
eval_apply ()
{
  return cell_unspecified;
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

#if !MES_MINI
#include "posix.c"
#include "math.c"
#endif
#include "lib.c"

// Jam Collector
SCM g_symbol_max;

SCM
gc_init_cells () ///((internal))
{
  int size = ARENA_SIZE * 12;
  size = size * 2;
#if __GNUC__
  g_arena = (char*)malloc (size);
#else
  char *p = 0;
  p = malloc (size);
  g_arena = p;
#endif
  g_cells = g_arena;
  return 0;
  //g_cells = (scm *)malloc (2*ARENA_SIZE*sizeof(scm));

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

#include "mes.mes.symbols.i"

  g_symbol_max = g_free;

  g_symbols = 0;
  for (int i=1; i<g_symbol_max; i++)
    g_symbols = cons (i, g_symbols);

  SCM a = cell_nil;

#include "mes.mes.symbol-names.i"

  a = acons (cell_symbol_mes_version, MAKE_STRING (cstring_to_list (VERSION)), a);
  a = acons (cell_symbol_mes_prefix, MAKE_STRING (cstring_to_list (PREFIX)), a);

  a = acons (cell_symbol_dot, cell_dot, a);
  a = acons (cell_symbol_begin, cell_begin, a);
  a = acons (cell_symbol_call_with_values, cell_symbol_call_with_values, a);
  a = acons (cell_symbol_current_module, cell_symbol_current_module, a);
  a = acons (cell_symbol_call_with_current_continuation, cell_call_with_current_continuation, a);
  a = acons (cell_symbol_sc_expand, cell_f, a);

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
#include "mes.mes.i"

// Do not sort: Order of these includes define builtins
#if !MES_MINI
#include "posix.mes.i"
#include "math.mes.i"
#endif
#include "lib.mes.i"
#if !MES_MINI
#include "vector.mes.i"
#endif
#include "gc.mes.i"
#if !MES_MINI
  //#include "reader.mes.i"
#endif

#include "gc.mes.environment.i"
#include "lib.mes.environment.i"
#if !MES_MINI
#include "math.mes.environment.i"
#endif
#include "mes.mes.environment.i"
#if !MES_MINI
#include "posix.mes.environment.i"
  //#include "reader.mes.environment.i"
#include "vector.mes.environment.i"
#endif

  return a;
}

SCM
bload_env (SCM a) ///((internal))
{
  char *mo = "module/mes/read-0.32-mo";
  g_stdin = open (mo, 0);
  if (g_stdin < 0) {eputs ("no such file: ");eputs (mo);eputs ("\n");return 1;}
  assert (getchar () == 'M');
  assert (getchar () == 'E');
  assert (getchar () == 'S');
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
  g_free = (p-(char*)g_cells) /
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
  set_env_x (cell_symbol_compiler, MAKE_STRING (cstring_to_list (compiler)), r0);

  char *arch = "x86";
#if __x86_64__
  arch = "x86_64";
#endif
  set_env_x (cell_symbol_arch, MAKE_STRING (cstring_to_list (arch)), r0);

  if (g_debug)
    {
      eputs ("symbols: ");
      SCM s = g_symbols;
      while (s && s != cell_nil) {
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
      //display_error_ (r0);
      //puts ("\n");
    }
  return r2;
}

#if !MES_MINI
#include "vector.c"
#endif
#include "gc.c"

int
main (int argc, char *argv[])
{
  char *p;

#if __M2_PLANET__
  ARENA_SIZE = 200000; // 32b: 2MiB, 64b: 4 MiB
  MAX_ARENA_SIZE = 300000000;
  JAM_SIZE = 20000;
  GC_SAFETY = 2000;
  g_cells = 0;
  g_news = 0;
  g_functions = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
#endif

  if (p = getenv ("MES_DEBUG")) g_debug = atoi (p);
  if (g_debug) {eputs (";;; MODULEDIR=");eputs (MODULEDIR);eputs ("\n");}
  if (p = getenv ("MES_MAX_ARENA")) MAX_ARENA_SIZE = atoi (p);
  if (p = getenv ("MES_ARENA")) ARENA_SIZE = atoi (p);
  if (argc > 1 && !strcmp (argv[1], "--help")) return oputs ("Usage: mes [--dump|--load] < FILE\n");
  if (argc > 1 && !strcmp (argv[1], "--version")) {puts ("Mes ");puts (VERSION);puts ("\n");return 0;};
  g_stdout = STDOUT;
  r0 = mes_environment ();

  SCM program = bload_env (r0);
  SCM lst = cell_nil;
  for (int i=argc-1; i>=0; i--) lst = cons (MAKE_STRING (cstring_to_list (argv[i])), lst);
  r0 = acons (cell_symbol_argv, lst, r0);
  push_cc (r2, cell_unspecified, r0, cell_unspecified);
  if (g_debug)
    {
      eputs ("program: ");
      display_error_ (r1);
      eputs ("\n");
    }
  r3 = cell_vm_begin;
  r1 = eval_apply ();
  display_error_ (r1);
  eputs ("\n");
  gc (g_stack);
  if (g_debug)
    {
      eputs ("\nstats: [");
      eputs (itoa (g_free));
      eputs ("]\n");
    }
  return 0;
}
