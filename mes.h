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

#ifndef __MES_LIBMES_H
#define __MES_LIBMES_H

#ifndef __MES_LIBMES_MINI_H
#define __MES_LIBMES_MINI_H

#if !WITH_GLIBC

#ifndef _SIZE_T
#define _SIZE_T
#ifndef __SIZE_T
#define __SIZE_T
#ifndef __MES_SIZE_T
#define __MES_SIZE_T
#undef size_t
typedef unsigned long size_t;
#endif
#endif
#endif

#ifndef _SSIZE_T
#define _SSIZE_T
#ifndef __SSIZE_T
#define __SSIZE_T
#ifndef __MES_SSIZE_T
#define __MES_SSIZE_T
#undef ssize_t
#if __i386__
typedef int ssize_t;
#else
typedef long ssize_t;
#endif
#endif
#endif
#endif

#ifndef __MES_ERRNO_T
#define __MES_ERRNO_T 1
typedef int error_t;
int errno;
#endif // !__MES_ERRNO_T

#endif //!WITH_LIBC

// CONSTANT STDIN 0
#ifndef STDIN
#define STDIN 0
#endif

// CONSTANT STDOUT 1
#ifndef STDOUT
#define STDOUT 1
#endif

// CONSTANT STDERR 2
#ifndef STDERR
#define STDERR 2
#endif

char **environ;
int __stdin;
int __stdout;
int __stderr;

int eputs (char const* s);
int puts (char const* s);
int oputs (char const* s);

#if !WITH_GLIBC
size_t strlen (char const* s);
ssize_t write (int filedes, void const *buffer, size_t size);
#endif // !WITH_GLIBC

#endif //__MES_LIBMES_MINI_H

#if WITH_GLIBC
int mes_open (char const *file_name, int flags, ...);
#define open mes_open
#endif

int __mes_debug ();
double abtod (char const** p, int base);
long abtol (char const** p, int base);
char *dtoab (double number, int base, int signed_p);
char *itoa (int number);
char *ltoa (long number);
char *ltoab (long x, int base);
char *ntoab (long number, int base, int signed_p);
char *ultoa (unsigned long number);
char *utoa (unsigned number);
int atoi (char const *s);
int eputc (int c);
int fdgetc (int fd);
int fdputc (int c, int fd);
int fdputs (char const* s, int fd);
int fdungetc (int c, int fd);
int _fdungetc_p (int fd);
int isdigit (int c);
int isspace (int c);
int isxdigit (int c);
int _open3 (char const *file_name, int flags, int mask);
int _open2 (char const *file_name, int flags);
int oputc (int c);
int oputs (char const* s);
char *search_path (char const *file_name);

#endif //__MES_LIBMES_H


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
