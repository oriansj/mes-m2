/* Essential Libraries */
#include <stdio.h>
#include <stdbool.h>

/* Core data type */
typedef int SCM;

/* Constants needed for reader */
#define MAX_STRING 4096

/* Constants needed for eval */
#define cell_nil 1
#define cell_f 2
#define cell_t 3
#define cell_dot 4
#define cell_arrow 5
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
#define cell_symbol_define 16
#define cell_symbol_define_macro 17
#define cell_symbol_quasiquote 18
#define cell_symbol_unquote 19
#define cell_symbol_unquote_splicing 20
#define cell_symbol_syntax 21
#define cell_symbol_quasisyntax 22
#define cell_symbol_unsyntax 23
#define cell_symbol_unsyntax_splicing 24
#define cell_symbol_set_x 25
#define cell_symbol_sc_expand 26
#define cell_symbol_macro_expand 27
#define cell_symbol_portable_macro_expand 28
#define cell_symbol_sc_expander_alist 29
#define cell_symbol_call_with_values 30
#define cell_call_with_current_continuation 31
#define cell_symbol_call_with_current_continuation 32
#define cell_symbol_current_module 33
#define cell_symbol_primitive_load 34
#define cell_symbol_read_input_file 35
#define cell_symbol_write 36
#define cell_symbol_display 37
#define cell_symbol_throw 38
#define cell_symbol_not_a_number 39
#define cell_symbol_not_a_pair 40
#define cell_symbol_system_error 41
#define cell_symbol_wrong_number_of_args 42
#define cell_symbol_wrong_type_arg 43
#define cell_symbol_unbound_variable 44
#define cell_symbol_argv 45
#define cell_symbol_mes_prefix 46
#define cell_symbol_mes_version 47
#define cell_symbol_car 48
#define cell_symbol_cdr 49
#define cell_symbol_pmatch_car 50
#define cell_symbol_pmatch_cdr 51
#define cell_vm_evlis 52
#define cell_vm_evlis2 53
#define cell_vm_evlis3 54
#define cell_vm_apply 55
#define cell_vm_apply2 56
#define cell_vm_eval 57
#define cell_vm_eval_pmatch_car 58
#define cell_vm_eval_pmatch_cdr 59
#define cell_vm_eval_define 60
#define cell_vm_eval_set_x 61
#define cell_vm_eval_macro_expand_eval 62
#define cell_vm_eval_macro_expand_expand 63
#define cell_vm_eval_check_func 64
#define cell_vm_eval2 65
#define cell_vm_macro_expand 66
#define cell_vm_macro_expand_define 67
#define cell_vm_macro_expand_define_macro 68
#define cell_vm_macro_expand_lambda 69
#define cell_vm_macro_expand_set_x 70
#define cell_vm_begin_expand_primitive_load 71
#define cell_vm_begin_primitive_load 72
#define cell_vm_macro_expand_car 73
#define cell_vm_macro_expand_cdr 74
#define cell_vm_begin_expand 75
#define cell_vm_begin_expand_eval 76
#define cell_vm_begin_expand_macro 77
#define cell_vm_begin 78
#define cell_vm_begin_read_input_file 79
#define cell_vm_begin_eval 80
#define cell_vm_if 81
#define cell_vm_if_expr 82
#define cell_vm_call_with_values2 83
#define cell_vm_call_with_current_continuation2 84
#define cell_vm_return 85
#define cell_type_char 86
#define cell_type_closure 87
#define cell_type_continuation 88
#define cell_type_function 89
#define cell_type_keyword 90
#define cell_type_macro 91
#define cell_type_number 92
#define cell_type_pair 93
#define cell_type_port 94
#define cell_type_ref 95
#define cell_type_special 96
#define cell_type_string 97
#define cell_type_symbol 98
#define cell_type_values 99
#define cell_type_variable 100
#define cell_type_vector 101
#define cell_type_broken_heart 102
#define cell_symbol_gnuc 103
#define cell_symbol_mesc 104
#define cell_test 105

/* Imported functions from mes.c */
SCM cons(SCM x, SCM y);
SCM CAR(SCM x);
SCM read_env(SCM a);
SCM MAKE_NUMBER(int n);
SCM MAKE_CHAR(char c);
SCM MAKE_STRING(SCM x);
SCM MAKE_KEYWORD(SCM x);
SCM cstring_to_list(char* s);
SCM lookup_symbol_(SCM s);
SCM error (SCM key, SCM x);
SCM reverse_x_ (SCM x, SCM t);
SCM gc_push_frame();
SCM gc_peek_frame();

/* Globals used in evalutation */
SCM r0;
SCM r1;
SCM r2;
SCM g_stack;
SCM g_symbols;
SCM* g_cells;
int g_free;
int g_debug;
FILE* g_stdin;

/* Imported functions from vector.c */
SCM list_to_vector (SCM x);

/* Imported functions from gc.c */
SCM gc();

/* Imported functions from lib.c */
SCM display_error_ (SCM x);

/* Function that is needed by Reader but not moved yet */
int fpeek(FILE *stream)
{
	int c;
	c = fgetc(stream);
	ungetc(c, stream);
	return c;
}

int match(char* a, char* b)
{
	int i = -1;
	do
	{
		i = i + 1;
		if(a[i] != b[i]) return false;
	} while((0 != a[i]) && (0 !=b[i]));
	return true;
}
