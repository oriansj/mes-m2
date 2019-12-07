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
//CONSTANT CONS 6
#define CONS 6
//CONSTANT LAMBDA 8
#define LAMBDA 8
//CONSTANT PRIMOP 10
#define PRIMOP 10
//CONSTANT CHAR 12
#define CHAR 12
//CONSTANT STRING 14
#define STRING 14
//CONSTANT VECTOR 16
#define VECTOR 16
//CONSTANT FILE_PORT 18
#define FILE_PORT 18
//CONSTANT RECORD 20
#define RECORD 20
//CONSTANT RECORD_TYPE 22
#define RECORD_TYPE 22
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
		FILE* file;
	};
	struct cell* cdr;
	struct cell* env;
};

/* Common functions */
struct cell* make_cons(struct cell* a, struct cell* b);
int numerate_string(char *a);
char* numerate_number(int a);
int match(char* a, char* b);
void file_print(char* s, FILE* f);
void require(int bool, char* error);

/* Global objects */
struct cell* all_symbols;
struct cell* top_env;
struct cell* nil;
struct cell* cell_unspecified;
struct cell* cell_t;
struct cell* cell_f;
struct cell* cell_dot;
struct cell* quote;
struct cell* quasiquote;
struct cell* unquote;
struct cell* unquote_splicing;
struct cell* s_if;
struct cell* s_lambda;
struct cell* s_define;
struct cell* s_setb;
struct cell* s_cond;
struct cell* s_begin;
struct cell* s_let;
struct cell* s_while;
struct cell* s_macro;
struct cell* s_define_macro;

/* IO */
FILE* __stdin;
FILE* __stdout;
FILE* __stderr;

/* Garbage Collection */
int left_to_take;
char* memory_block;

/* Lisp Macine */
struct cell** g_stack;
struct cell* R0;
struct cell* R1;
struct cell* g_env;
unsigned stack_pointer;
