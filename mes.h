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

#include "gcc_req.h"

//CONSTANT FREE 0
#define FREE 0
//CONSTANT MARKED 1
#define MARKED 1
//CONSTANT INT 2
#define INT 2
//CONSTANT SYM 4
#define SYM 4
//CONSTANT KEYWORD 6
#define KEYWORD 6
//CONSTANT CONS 50
#define CONS 50
//CONSTANT LAMBDA 100
#define LAMBDA 100
//CONSTANT PRIMOP 200
#define PRIMOP 200
//CONSTANT CHAR 300
#define CHAR 300
//CONSTANT STRING 400
#define STRING 400
//CONSTANT VECTOR 500
#define VECTOR 500
//CONSTANT FILE_PORT 600
#define FILE_PORT 600
//CONSTANT RECORD 700
#define RECORD 700
//CONSTANT RECORD_TYPE 800
#define RECORD_TYPE 800
//CONSTANT MACRO 1000
#define MACRO 1000
//CONSTANT EOF_object 1024
#define EOF_object 1024

// CONSTANT FALSE 0
#define FALSE 0
// CONSTANT TRUE 1
#define TRUE 1

struct cell
{
	int type;
	union
	{
		struct cell* car;
		int value;
		char* string;
		FUNCTION* function;
	};
	struct cell* cdr;
	union
	{
		struct cell* env;
		FILE* file;
		int length;
	};
};

/* Common functions */
char* numerate_number(int a);
int match(char* a, char* b);
int numerate_string(char *a);
struct cell* make_cons(struct cell* a, struct cell* b);
void file_print(char* s, FILE* f);
void require(int bool, char* error);

/* Global objects */
struct cell* cell_dot;
struct cell* cell_f;
struct cell* cell_t;
struct cell* cell_unspecified;
struct cell* s_else;
struct cell* nil;
struct cell* quasiquote;
struct cell* quote;
struct cell* s_and;
struct cell* s_begin;
struct cell* s_case;
struct cell* s_cond;
struct cell* s_define;
struct cell* s_define_macro;
struct cell* s_if;
struct cell* s_lambda;
struct cell* s_let;
struct cell* s_macro;
struct cell* s_or;
struct cell* s_setb;
struct cell* s_when;
struct cell* s_while;
struct cell* unquote;
struct cell* unquote_splicing;

/* IO */
char** __argv;
char** __envp;
int __argc;
struct cell* __c_stderr;
struct cell* __c_stdin;
struct cell* __c_stdout;

/* Garbage Collection */
char* memory_block;
unsigned left_to_take;
unsigned arena;
unsigned max_arena;
unsigned GC_SAFETY;
void garbage_collect();

/* Lisp Macine */
struct cell* R0;
struct cell* R1;
struct cell* R2;
struct cell* R3;
struct cell* R4;
struct cell* all_symbols;
struct cell* g_env;
struct cell** g_stack;
int stack_pointer;
unsigned MAX_STRING;
unsigned MAX_TOKEN;
int MAX_STACK;

/* To control debugging info */
unsigned mes_debug_level;

/* To prevent damage while fuzzing */
int FUZZING;
