/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of Mes.
 *
 * Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

#if POSIX
#error "POSIX not supported"
#endif

#if __M2_PLANET__

CONSTANT STDIN 0
CONSTANT EOF 0xffffffff

int g_stdin;
int eputs (char * s);
int oputs (char * s);
int getchar ();
int putchar (int c);
int puts (char * s);
char * itoa (int number);
int open (char *,int,int);
void *malloc (size_t size);
void exit (int);

int assert_fu;
#define assert(x) assert_fu

#else

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <libmes.h>

#endif

char *g_arena;

typedef int SCM;

int g_debug;
int g_free;

SCM g_continuations;
SCM g_symbols;
SCM g_stack;
/* a/env */
SCM r0;
/* param 1 */
SCM r1;
/* save 2+load/dump */
SCM r2;
/* continuation */
SCM r3;

#if !__M2_PLANET__
enum type_t {TCHAR, TCLOSURE, TCONTINUATION, TFUNCTION, TKEYWORD, TMACRO, TNUMBER, TPAIR, TPORT, TREF, TSPECIAL, TSTRING, TSYMBOL, TVALUES, TVARIABLE, TVECTOR, TBROKEN_HEART};
#else
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
#endif

struct scm {
  enum type_t type;
  SCM car;
  SCM cdr;
};

struct function {
#if __M2_PLANET__
  FUNCTION *function;
#else
  int (*function) (void);
#endif
  int arity;
  char *name;
};

struct scm *g_cells;

#define cell_nil 1
#define cell_f 2
#define cell_t 3
#define cell_dot 4
// #define cell_arrow 5
#define cell_undefined 6
#define cell_unspecified 7
#define cell_closure 8
#define cell_circular 9
#define cell_begin 10
#define cell_symbol_dot 11
#define cell_symbol_lambda 12
#define cell_symbol_begin 13
#define cell_symbol_if 14
#define cell_symbol_quote 15
#define cell_symbol_set_x 16

#define cell_vm_apply 45
#define cell_vm_apply2 46

#define cell_vm_eval 47

#define cell_vm_begin 56
//#define cell_vm_begin_read_input_file 57
#define cell_vm_begin2 58

#define cell_vm_return 63

SCM tmp;
SCM tmp_num;
SCM tmp_num2;

#if __M2_PLANET__
int ARENA_SIZE;
#else
int ARENA_SIZE = 20000;
#endif

#if __M2_PLANET__
struct function *g_functions;
#else
struct function g_functions[5];
#endif
int g_function;


#if __M2_PLANET__
SCM make_cell_ (SCM type, SCM car, SCM cdr);
struct function fun_make_cell_ ; #= {&make_cell_,3,"core:make-cell"};
struct scm scm_make_cell_; # =  {TFUNCTION,0,0};
   //, "core:make-cell", 0};
SCM cell_make_cell_;

SCM cons (SCM x, SCM y);
struct function fun_cons; # =  {&cons,2,"cons"};
struct scm scm_cons; # =  {TFUNCTION,0,0};
  // "cons", 0};
SCM cell_cons;

SCM car (SCM x);
struct function fun_car; # =  {&car,1,"car"};
struct scm scm_car; # =  {TFUNCTION,0,0};
  // "car", 0};
SCM cell_car;

SCM cdr (SCM x);
struct function fun_cdr; # =  {&cdr,1,"cdr"};
struct scm scm_cdr; # =  {TFUNCTION,0,0};
SCM cell_cdr;
#else //!__M2_PLANET__

SCM make_cell_ (SCM type, SCM car, SCM cdr);
struct function fun_make_cell_ = {&make_cell_,3,"core:make-cell"};
struct scm scm_make_cell_ = {TFUNCTION,0,0};
   //, "core:make-cell", 0};
SCM cell_make_cell_;

SCM cons (SCM x, SCM y);
struct function fun_cons = {&cons,2,"cons"};
struct scm scm_cons = {TFUNCTION,0,0};
  // "cons", 0};
SCM cell_cons;

SCM car (SCM x);
struct function fun_car = {&car,1,"car"};
struct scm scm_car = {TFUNCTION,0,0};
  // "car", 0};
SCM cell_car;

SCM cdr (SCM x);
struct function fun_cdr = {&cdr,1,"cdr"};
struct scm scm_cdr = {TFUNCTION,0,0};
// "cdr", 0};
SCM cell_cdr;

// SCM eq_p (SCM x, SCM y);
// struct function fun_eq_p = {&eq_p,2,"eq?"};
// scm scm_eq_p = {TFUNCTION,0,0};
// SCM cell_eq_p;

#endif //!__M2_PLANET__

// SCM eq_p (SCM x, SCM y);
// struct function fun_eq_p = {&eq_p,2,"eq?"};
// scm scm_eq_p = {TFUNCTION,0,0};
// SCM cell_eq_p;

#if __M2_PLANET__
#define struct_function_size 12
#define struct_size 12
#define TYPE(x) ((x*struct_size)+g_cells)->type

#define CAR(x) ((x*struct_size)+g_cells)->car

#define CDR(x) ((x*struct_size)+g_cells)->cdr
#define FUNCTION(x) ((((x*struct_size)+g_cells)->cdr*struct_function_size)+g_functions)
#define VALUE(x) ((x*struct_size)+g_cells)->cdr

#else // !__M2_PLANET__
#define TYPE(x) (g_cells[x].type)

#define CAR(x) g_cells[x].car
#define LENGTH(x) g_cells[x].car
#define STRING(x) g_cells[x].car

#define CDR(x) g_cells[x].cdr
#define CONTINUATION(x) g_cells[x].cdr

#define FUNCTION(x) g_functions[g_cells[x].cdr]
#define VALUE(x) g_cells[x].cdr
#define VECTOR(x) g_cells[x].cdr
#endif // !__M2_PLANET__

#define MAKE_CHAR(n) make_cell__ (TCHAR, 0, n)
#define MAKE_NUMBER(n) make_cell__ (TNUMBER, 0, n)

#define CAAR(x) CAR (CAR (x))
#define CDDR(x) CDR (CDR (x))
#define CADAR(x) CAR (CDR (CAR (x)))
#define CDADAR(x) CAR (CDR (CAR (CDR (x))))
#define CADR(x) CAR (CDR (x))

#define MAKE_STRING(x) make_cell__ (TSTRING, x, 0)

SCM
alloc (int n)
{
  SCM x = g_free;
#if __M2_PLANET__
  g_free = g_free + n;
#else
  g_free += n;
#endif
  return x;
}

SCM
make_cell__ (int type, SCM car, SCM cdr)
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
  int t = VALUE (type);
  if (t == TCHAR || t == TNUMBER)
#if __M2_PLANET__
    return make_cell__ (t, CAR (car), CDR (cdr));
#else
    return make_cell__ (t, car ? CAR (car) : 0, cdr ? CDR (cdr) : 0);
#endif
  return make_cell__ (t, car, cdr);
}

SCM
cons (SCM x, SCM y)
{
  return make_cell__ (TPAIR, x, y);
}

SCM
car (SCM x)
{
  return CAR (x);
}

SCM
cdr (SCM x)
{
  return CDR(x);
}

SCM
gc_push_frame ()
{
  SCM frame = cons (r1, cons (r2, cons (r3, cons (r0, cell_nil))));
  g_stack = cons (frame, g_stack);
  return g_stack;
}

SCM
append2 (SCM x, SCM y)
{
  if (x == cell_nil) return y;
  assert (TYPE (x) == TPAIR);
  return cons (car (x), append2 (cdr (x), y));
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
  while (a != cell_nil && x == CAAR (a)) a = CDR (a);
#if __M2_PLANET__
  if (a != cell_nil)
    return CAR (a);
  return cell_f;
#else
  return a != cell_nil ? car (a) : cell_f;
#endif
}

SCM
push_cc (SCM p1, SCM p2, SCM a, SCM c) ///((internal))
{
  oputs ("push cc\n");
  SCM x = r3;
  r3 = c;
  r2 = p2;
  gc_push_frame ();
  r1 = p1;
  r0 = a;
  r3 = x;
  return cell_unspecified;
}

SCM caar (SCM x) {return car (car (x));}
SCM cadr (SCM x) {return car (cdr (x));}
SCM cdar (SCM x) {return cdr (car (x));}
SCM cddr (SCM x) {return cdr (cdr (x));}

SCM call (SCM,SCM);
SCM gc_pop_frame ();

SCM
eval_apply ()
{
  int t;
  SCM x = cell_nil;
  SCM y = cell_nil;

 eval_apply:
  if (r3 == cell_vm_apply) goto apply;
  else if (r3 == cell_unspecified) return r1;

 apply:
  t = TYPE (CAR (r1));
  if (t == TFUNCTION)
    {
      oputs ("apply.function\n");
      r1 = call (car (r1), cdr (r1));
      goto vm_return;
    }

 vm_return:
  x = r1;
  gc_pop_frame ();
  r1 = x;
  goto eval_apply;
}

SCM
call (SCM fn, SCM x)
{
  oputs ("call\n");
#if __M2_PLANET__
  struct function *f = FUNCTION (fn);
  int arity = f->arity;
#else
  struct function *f = &FUNCTION (fn);
  int arity = f->arity;
#endif
  if ((arity > 0 || arity == -1)
      && x != cell_nil && TYPE (CAR (x)) == TVALUES)
    x = cons (CADAR (x), CDR (x));
  if ((arity > 1 || arity == -1)
      && x != cell_nil && TYPE (CDR (x)) == TPAIR && TYPE (CADR (x)) == TVALUES)
    x = cons (CAR (x), cons (CDADAR (x), CDR (x)));
#if __M2_PLANET__
  // TODO
  if (arity == 0)
    {
      FUNCTION fp = f->function;
      //return fp ();
    }
  // else if (a == 1) return (FUNCTION (fn).function) (car (x));
  // else if (a == 2) return (FUNCTION (fn).function) (car (x), cadr (x));
  // else if (a == 3) return (FUNCTION (fn).function) (car (x), cadr (x), car (cddr (x)));
  // else if (a == -1) return (FUNCTION (fn).function) (x);
#else  
  switch (FUNCTION (fn).arity)
    {
#if __MESC__ || !_POSIX_SOURCE
    case 0: return (FUNCTION (fn).function) ();
    case 1: return ((SCM(*)(SCM))(FUNCTION (fn).function)) (CAR (x));
    case 2: return ((SCM(*)(SCM,SCM))(FUNCTION (fn).function)) (CAR (x), CADR (x));
    case 3: return ((SCM(*)(SCM,SCM,SCM))(FUNCTION (fn).function)) (CAR (x), CADR (x), CAR (CDDR (x)));
    case -1: return ((SCM(*)(SCM))(FUNCTION (fn).function)) (x);
    default: return ((SCM(*)(SCM))(FUNCTION (fn).function)) (x);
#else
    case 0: return FUNCTION (fn).function0 ();
    case 1: return FUNCTION (fn).function1 (CAR (x));
    case 2: return FUNCTION (fn).function2 (CAR (x), CADR (x));
    case 3: return FUNCTION (fn).function3 (CAR (x), CADR (x), CAR (CDDR (x)));
    case -1: return FUNCTION (fn).functionn (x);
#endif
    }
#endif //! __M2_PLANET__
  return cell_unspecified;
}

SCM
gc_peek_frame ()
{
  SCM frame = car (g_stack);
  r1 = car (frame);
  r2 = cadr (frame);
  r3 = car (cddr (frame));
  r0 = cadr (cddr (frame));
  return frame;
}

SCM
gc_pop_frame ()
{
  SCM frame = gc_peek_frame (g_stack);
  g_stack = cdr (g_stack);
  return frame;
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
SCM
make_symbol_ (SCM s)
{
  SCM x = make_cell__ (TSYMBOL, s, 0);
  g_symbols = cons (x, g_symbols);
  return x;
}

SCM
make_symbol (SCM s)
{
  SCM x = make_cell__ (TSYMBOL, s, 0);
  g_symbols = cons (x, g_symbols);
  return x;
}

SCM
acons (SCM key, SCM value, SCM alist)
{
  return cons (cons (key, value), alist);
}

// Jam Collector
SCM g_symbol_max;

SCM
gc_init_cells ()
{
  return 0;
}

// INIT NEWS

SCM
mes_symbols () ///((internal))
{
  gc_init_cells ();
  //  gc_init_news ();

#if __GNUC__ && 0
  //#include "mes.symbols.i"
#else
g_free = g_free + 1;
// g_cells[cell_nil] = scm_nil;

g_free = g_free + 1;
// g_cells[cell_f] = scm_f;

g_free = g_free + 1;
// g_cells[cell_t] = scm_t;

g_free = g_free + 1;
// g_cells[cell_dot] = scm_dot;

g_free = g_free + 1;
// g_cells[cell_arrow] = scm_arrow;

g_free = g_free + 1;
// g_cells[cell_undefined] = scm_undefined;

g_free = g_free + 1;
// g_cells[cell_unspecified] = scm_unspecified;

g_free = g_free + 1;
// g_cells[cell_closure] = scm_closure;

g_free = g_free + 1;
// g_cells[cell_circular] = scm_circular;

g_free = g_free + 1;
// g_cells[cell_begin] = scm_begin;

///
g_free = 44;
g_free = g_free + 1;
// g_cells[cell_vm_apply] = scm_vm_apply;

g_free = g_free + 1;
// g_cells[cell_vm_apply2] = scm_vm_apply2;

g_free = g_free + 1;
// g_cells[cell_vm_eval] = scm_vm_eval;

///
g_free = 55;
g_free = g_free + 1;
// g_cells[cell_vm_begin] = scm_vm_begin;

g_free = g_free + 1;
// g_cells[cell_vm_begin_read_input_file] = scm_vm_begin_read_input_file;

g_free = g_free + 1;
// g_cells[cell_vm_begin2] = scm_vm_begin2;

///
g_free = 62;
g_free = g_free + 1;
// g_cells[cell_vm_return] = scm_vm_return;

#endif

  g_symbol_max = g_free;
  g_symbols = 0;
  int i;
  for (i=1; i<g_symbol_max; i = i+1)
    g_symbols = cons (i, g_symbols);

  SCM a = cell_nil;

  a = acons (cell_symbol_dot, cell_dot, a);
  a = acons (cell_symbol_begin, cell_begin, a);
  a = acons (cell_closure, a, a);

  return a;
}

SCM
make_closure (SCM args, SCM body, SCM a)
{
  return make_cell__ (TCLOSURE, 0, cons (cons (cell_circular, a), cons (args, body)));
}

#if __M2_PLANET__
SCM
mes_globals ()
{
  fun_make_cell_->function = make_cell_;
  fun_make_cell_->arity    = 3;
  fun_make_cell_->name     = "core:make-cell";

  scm_make_cell_->type = TFUNCTION;
  scm_make_cell_->car  = 0;
  scm_make_cell_->cdr  = 0;

  fun_cons->function = cons;
  fun_cons->arity    = 2;
  fun_cons->name     = "cons";

  scm_cons->type = TFUNCTION;
  scm_cons->car  = 0;
  scm_cons->cdr  = 0;

  fun_car->function = car;
  fun_car->arity    = 1;
  fun_car->name     = "car";

  scm_car->type = TFUNCTION;
  scm_car->car  = 0;
  scm_car->cdr  = 0;

  fun_cdr->function = cdr;
  fun_cdr->arity    = 1;
  fun_cdr->name     = "cdr";

  scm_cdr->type = TFUNCTION;
  scm_cdr->car  = 0;
  scm_cdr->cdr  = 0;

}
#endif

SCM
mes_environment () ///((internal))
{
  SCM a = 0;
  a = mes_symbols ();
  a = mes_g_stack (a);
  return a;
}

SCM
mes_builtins (SCM a)
{
#if 0
  //__GNUC__
//#include "mes.i"

// #include "lib.i"
// #include "math.i"
// #include "posix.i"
// #include "reader.i"

// #include "lib.environment.i"
// #include "math.environment.i"
// #include "mes.environment.i"
// #include "posix.environment.i"
// #include "reader.environment.i"
#elif __M2_PLANET__

  //mes_globals ();

  scm_make_cell_->cdr = g_function;
// g_functions[g_function] = fun_make_cell_;
// cell_make_cell_ = g_free;
// g_cells[cell_make_cell_] = scm_make_cell_;
// g_function = g_function + 1;
// g_free = g_free + 1;

// scm_cons->cdr = g_function;
// g_functions[g_function] = fun_cons;
// cell_cons = g_free;
// g_cells[cell_cons] = scm_cons;
// g_function = g_function + 1;
// g_free = g_free + 1;
 
// scm_car->cdr = g_function;
// g_functions[g_function] = fun_car;
// cell_car = g_free;
// g_cells[cell_car] = scm_car;
// g_function = g_function + 1;
// g_free = g_free + 1;
 
// scm_cdr->cdr = g_function;
// g_functions[g_function] = fun_cdr;
// cell_cdr = g_free;
// g_cells[cell_cdr] = scm_cdr;
// g_function = g_function + 1;
// g_free = g_free + 1;

#else // ! __M2_PLANET__

scm_make_cell_.cdr = g_function;
g_functions[g_function] = fun_make_cell_;
cell_make_cell_ = g_free;
g_cells[cell_make_cell_] = scm_make_cell_;
g_function = g_function + 1;
g_free = g_free + 1;

scm_cons.cdr = g_function;
g_functions[g_function] = fun_cons;
cell_cons = g_free;
g_cells[cell_cons] = scm_cons;
g_function = g_function + 1;
g_free = g_free + 1;
 
scm_car.cdr = g_function;
g_functions[g_function] = fun_car;
cell_car = g_free;
g_cells[cell_car] = scm_car;
g_function = g_function + 1;
g_free = g_free + 1;
 
scm_cdr.cdr = g_function;
g_functions[g_function] = fun_cdr;
cell_cdr = g_free;
g_cells[cell_cdr] = scm_cdr;
g_function = g_function + 1;
g_free = g_free + 1;
 
#endif
  return a;
}

SCM
bload_env (SCM a) ///((internal))
{
  g_stdin = open ("module/mes/read-0.mo", 0);
  char *p = g_cells;
  assert (getchar () == 'M');
  assert (getchar () == 'E');
  assert (getchar () == 'S');
  g_stack = getchar () << 8;
  g_stack = g_stack + getchar ();
  int c = getchar ();
  while (c != EOF)
    {
      p[0] = c;
      p = p + 1;
      c = getchar ();
    }
  char *q = g_cells;
  g_free = (p-q) / sizeof (struct scm);
  gc_peek_frame ();
  g_symbols = r1;
  g_stdin = STDIN;
  r0 = mes_builtins (r0);

  return r2;
}

SCM
fill ()
{
  TYPE (0) = 0x6c6c6168;
  CAR (0) = 0x6a746f6f;
  CDR (0) = 0x00002165;

  TYPE (1) = TSYMBOL;
  CAR (1) = 0x2d2d2d2d;
  CDR (1) = 0x3e3e3e3e;

  TYPE (9) = 0x2d2d2d2d;
  CAR (9) = 0x2d2d2d2d;
  CDR (9) = 0x3e3e3e3e;

  // (cons 0 1)
  TYPE (10) = TPAIR;
  CAR (10) = 11;
  CDR (10) = 12;

  TYPE (11) = TFUNCTION;
  CAR (11) = 0x58585858;
  // 0 = make_cell_
  // 1 = cons
  // 2 = car
  CDR (11) = 1;

  TYPE (12) = TPAIR;
  CAR (12) = 13;
  //CDR (12) = 1;
  CDR (12) = 14;

  TYPE (13) = TNUMBER;
  CAR (13) = 0x58585858;
  CDR (13) = 0;

  TYPE (14) = TPAIR;
  CAR (14) = 15;
  CDR (14) = 1;

  TYPE (15) = TNUMBER;
  CAR (15) = 0x58585858;
  CDR (15) = 1;

  return 0;
}

SCM
display_ (SCM x)
{
  if (g_debug != 0)
    {
      oputs ("<display>\n");
      oputs (itoa (TYPE (x)));
      oputs ("\n");
    }
  if (TYPE (x) == TCHAR)
    {
      if (g_debug != 0) oputs ("<char>\n");
      oputs ("#\\");
      putchar (VALUE (x));
    }
    else if (TYPE (x) == TFUNCTION)
    {
      if (g_debug != 0) oputs ("<function>\n");
      if (VALUE (x) == 0)
        oputs ("core:make-cell");
      if (VALUE (x) == 1)
        oputs ("cons");
      if (VALUE (x) == 2)
        oputs ("car");
      if (VALUE (x) == 3)
        oputs ("cdr");
    }
  else if (TYPE (x) == TNUMBER)
    {
      if (g_debug != 0) oputs ("<number>\n");
      oputs (itoa (VALUE (x)));
    }
  else if (TYPE (x) == TPAIR)
    {
      if (g_debug != 0) oputs ("<pair>\n");
      //if (cont != cell_f) oputs "(");
      oputs ("(");
      if (x != 0 && x != cell_nil) display_ (CAR (x));
      if (CDR (x) != 0 && CDR (x) != cell_nil)
        {
          if (TYPE (CDR (x)) != TPAIR)
            oputs (" . ");
          display_ (CDR (x));
        }
      //if (cont != cell_f) oputs (")");
      oputs (")");
    }
  else if (TYPE (x) == TSPECIAL)
    {
      if (x == 1) oputs ("()");
      else if (x == 2) oputs ("#f");
      else if (x == 3) oputs ("#t");
      else
        {
          oputs ("<x:");
          oputs (itoa (x));
          oputs (">");
        }
    }
  else if (TYPE (x) == TSYMBOL)
    {
      if (x == 11) oputs (" . ");
      else if (x == 12) oputs ("lambda");
      else if (x == 13) oputs ("begin");
      else if (x == 14) oputs ("if");
      else if (x == 15) oputs ("quote");
      else if (x == 37) oputs ("car");
      else if (x == 38) oputs ("cdr");
      else if (x == 39) oputs ("null?");
      else if (x == 40) oputs ("eq?");
      else if (x == 41) oputs ("cons");
      else
        {
          oputs ("<s:");
          oputs (itoa (x));
          oputs (">");
        }
      }
  else
    {
      if (g_debug != 0) oputs ("<default>\n");
      oputs ("<");
      oputs (itoa (TYPE (x)));
      oputs (":");
      oputs (itoa (x));
      oputs (">");
    }

  return 0;
}

SCM
simple_bload_env (SCM a) ///((internal))
{
  // BLOAD and tiny program dump just been resurrected on wip-gnu
  // let's not test that just now and use `fill'
#if 0 // BLOAD .. later
  oputs ("reading: ");
  char *mo = "module/mes/tiny-0.32-mo";
  oputs (mo);
  oputs ("\n");
  g_stdin = open (mo, 0);
  if (g_stdin < 0) {eputs ("no such file: module/mes/tiny-0.32-mo\n");return 1;}

  char *p = g_cells;
  int c;

  assert (getchar () == 'M');
  assert (getchar () == 'E');
  assert (getchar () == 'S');
  oputs (" *GOT MES*\n");

  g_stack = getchar () << 8;
  g_stack = g_stack + getchar ();

  oputs ("stack: ");
  oputs (itoa (g_stack));
  oputs ("\n");

  c = getchar ();
  while (c != -1)
    {
      p[0] = c;
      p = p + 1;
      c = getchar ();
    }

  oputs ("read done\n");

  char *q = g_cells;
  g_free = (p-q) / sizeof (struct scm);
  
  if (g_free != 15)
    exit (33);
  
  g_symbols = 1;

  g_stdin = STDIN;
  r0 = mes_builtins (r0);
  
  if (g_free != 19)
    exit (34);
  
  oputs ("cells read: ");
  oputs (itoa (g_free));
  oputs ("\n");

  oputs ("symbols: ");
  oputs (itoa (g_symbols));
  oputs ("\n");
  // display_ (g_symbols);
  // oputs ("\n");

  display_ (10);
  oputs ("\n");

#else // !BLOAD
  g_symbols = 1;
  g_free = 15;
  r0 = mes_builtins (r0);
#endif // !BLOAD

  fill ();
  r2 = 10;

  if (TYPE (12) != TPAIR)
    exit (33);

  oputs ("program[");
  oputs (itoa (r2));
  oputs ("]: ");

  display_ (r2);
  oputs ("\n");

  r0 = 1;
  return r2;
}

int
main (int argc, char **argv)
{
  oputs ("Hello cons-mes!\n");
#if !__M2_PLANET__
  if (argc > 1 && !strcmp (argv[1], "--help")) return eputs ("Usage: mes [--dump|--load] < FILE");
#if __GNUC__
  if (argc > 1 && !strcmp (argv[1], "--version")) {eputs ("Mes ");return eputs (VERSION);};
#else
  if (argc > 1 && !strcmp (argv[1], "--version")) {eputs ("Mes ");return eputs ("0.4");};
#endif
#endif //!__M2_PLANET__

  g_stdin = STDIN;

#if __M2_PLANET__
  ARENA_SIZE = 3000;
  g_functions = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
#endif
  g_arena = malloc (ARENA_SIZE * sizeof (struct scm));
  g_cells = g_arena;

#if 1
  r0 = mes_environment ();
  
  SCM program = simple_bload_env (r0);

  oputs ("g_free=");
  oputs (itoa (g_free));
  oputs ("\n");

  push_cc (r2, cell_unspecified, r0, cell_unspecified);

  oputs ("g_free=");
  oputs (itoa (g_free));
  oputs ("\n");

  oputs ("g_stack=");
  oputs (itoa (g_stack));
  oputs ("\n");

  oputs ("r0=");
  oputs (itoa (r0));
  oputs ("\n");

  oputs ("r1=");
  oputs (itoa (r1));
  oputs ("\n");

  oputs ("r2=");
  oputs (itoa (r2));
  oputs ("\n");

  oputs ("r3=");
  oputs (itoa (r3));
  oputs ("\n");

  r3 = cell_vm_apply;
  r1 = eval_apply ();
  display_ (r1);
#endif
  eputs ("\n");
  return 0;
}
