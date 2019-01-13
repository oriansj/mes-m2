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

#include "mes.h"
#include "mes_constants.h"


int mes_open (char const *file_name, int flags, ...);
#define open mes_open

char *itoa (int number);
int fdgetc (int fd);
int fdputc (int c, int fd);
int fdputs (char const* s, int fd);
int fdungetc (int c, int fd);
int eputs (char const* s);

long ARENA_SIZE = 10000000;
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

typedef SCM (*function0_t) (void);
typedef SCM (*function1_t) (SCM);
typedef SCM (*function2_t) (SCM, SCM);
typedef SCM (*function3_t) (SCM, SCM, SCM);
typedef SCM (*functionn_t) (SCM);

struct scm
{
	long type;
	SCM car;
	SCM cdr;
};

struct scm *g_cells = 0;
struct scm *g_news = 0;


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
SCM hash_table_printer (SCM table);
SCM make_hash_table (SCM x);
// src/lib.mes
SCM procedure_name_ (SCM x);
SCM display_ (SCM x);
SCM display_error_ (SCM x);
SCM display_port_ (SCM x, SCM p);
SCM write_ (SCM x);
SCM write_error_ (SCM x);
SCM write_port_ (SCM x, SCM p);
SCM exit_ (SCM x);
SCM frame_printer (SCM frame);
SCM make_stack (SCM stack);
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
SCM make_cell_ (SCM type, SCM car, SCM cdr);
SCM type_ (SCM x);
SCM car_ (SCM x);
SCM cdr_ (SCM x);
SCM arity_ (SCM x);
SCM cons (SCM x, SCM y);
SCM car (SCM x);
SCM cdr (SCM x);
SCM list (SCM x);
SCM null_p (SCM x);
SCM eq_p (SCM x, SCM y);
SCM values (SCM x);
SCM acons (SCM key, SCM value, SCM alist);
SCM length (SCM x);
SCM error (SCM key, SCM x);
SCM append2 (SCM x, SCM y);
SCM append_reverse (SCM x, SCM y);
SCM reverse_x_ (SCM x, SCM t);
SCM pairlis (SCM x, SCM y, SCM a);
SCM call (SCM fn, SCM x);
SCM assq (SCM x, SCM a);
SCM assoc (SCM x, SCM a);
SCM set_car_x (SCM x, SCM e);
SCM set_cdr_x (SCM x, SCM e);
SCM set_env_x (SCM x, SCM e, SCM a);
SCM macro_get_handle (SCM name);
SCM add_formals (SCM formals, SCM x);
SCM eval_apply ();
SCM make_builtin_type ();
SCM make_builtin (SCM builtin_type, SCM name, SCM arity, SCM function);
SCM builtin_arity (SCM builtin);
SCM builtin_p (SCM x);
SCM builtin_printer (SCM builtin);

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
SCM read_input_file_env (SCM a);
SCM read_env (SCM a);
SCM reader_read_sexp (SCM c, SCM s, SCM a);
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
