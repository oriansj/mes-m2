/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include "mes.h"
#include "mes_constants.h"

SCM length__(struct scm* x);
int eputs(char* s);
char* itoa(int number);
struct scm* cstring_to_symbol(char* s);
struct scm* write_error_(struct scm* x);
SCM gc_pop_frame();
struct scm* vector_entry(struct scm* x);
struct scm* make_stack_type();
struct scm* make_struct_(struct scm* type, struct scm* fields, struct scm* printer);
struct scm* make_frame_type();
struct scm* cons(struct scm* x, struct scm* y);
struct scm* make_vector__(SCM k);
void vector_set_x_(struct scm* x, SCM i, struct scm* e);

void require(int bool, char* error);
char* env_lookup(char* token, char** envp);
int numerate_string(char *a);
void block_copy(void* source, void* destination, int num);

SCM GC_SAFETY;
SCM ARENA_SIZE;
SCM MAX_ARENA_SIZE;
SCM JAM_SIZE;
// CONSTANT FRAME_SIZE 5
#define FRAME_SIZE 5

int get_env_value(char* c, int alt)
{
	char* s = env_lookup(c, global_envp);
	if(NULL == s) return alt;
	return numerate_string(s);
}

struct scm *g_news;

void gc_init_cells()  /* ((internal)) */
{
	SCM stack_size = ((ARENA_SIZE + JAM_SIZE) * sizeof(struct scm)) + (STACK_SIZE * sizeof(SCM));
	g_stack_array = calloc(stack_size, 1);
	g_buf = calloc(MAX_STRING, 1);
}

struct scm* make_char(SCM c);
struct scm* mes_g_stack(struct scm* a)  /* ((internal)) */
{
	g_stack = STACK_SIZE;
	R0 = a;
	R1 = make_char(0);
	R2 = make_char(0);
	R3 = make_char(0);
	return R0;
}

void initialize_memory()
{
	g_news = 0;
	MAX_ARENA_SIZE = get_env_value("MES_MAX_ARENA", 100000000);
	ARENA_SIZE = get_env_value("MES_ARENA", 10000000);
	JAM_SIZE = get_env_value("MES_JAM", ARENA_SIZE / 10);
	GC_SAFETY = get_env_value("MES_SAFETY", ARENA_SIZE / 100);
	STACK_SIZE = get_env_value("MES_STACK", 20000);
	g_stack = STACK_SIZE;
	MAX_STRING = get_env_value("MES_MAX_STRING", 524288);
	gc_init_cells();
}

struct scm* make_tpair(struct scm* a, struct scm* b)
{
	struct scm* x = calloc(1, sizeof(struct scm));
	x->type = TPAIR;
	x->car = a;
	x->cdr = b;
	return x;
}

struct scm* make_stack(struct scm* x)  /* External */
{
	require(cell_nil == x, "mes_gc.c: make_stack recieved non-nil list\n");
	struct scm* stack_type = make_stack_type();
	SCM size = (STACK_SIZE - g_stack) / FRAME_SIZE;
	struct scm* frames = make_vector__(size);
	SCM i;

	for(i = 0; i < size; i = i + 1)
	{
		SCM array_index = (STACK_SIZE - (i * FRAME_SIZE));
		struct scm* procedure = g_stack_array[array_index + FRAME_PROCEDURE];

		if(!procedure)
		{
			procedure = cell_f;
		}

		struct scm* frame = make_struct_(make_frame_type()
		                   , make_tpair(cell_symbol_frame, make_tpair(procedure, cell_nil))
		                   , cstring_to_symbol("frame-printer"));
		vector_set_x_(frames, i, frame);
	}

	struct scm* values = cell_nil;
	values = make_tpair(frames, values);
	values = make_tpair(cell_symbol_stack, values);
	return make_struct_(stack_type, values, cell_unspecified);
}


struct scm* make_cell(struct scm* x) /* External */
{
	struct scm* type = x->car;
	struct scm* car = x->cdr->car;
	struct scm* cdr = x->cdr->cdr->car;
	require(type->type == TNUMBER, "type does not match TNUMBER in mes_gc.c: make_cell\n");

	if(type->value == TCHAR || type->value == TNUMBER)
	{
		if(0 != car)
		{
			car = car->car;
		}

		if(0 != cdr)
		{
			cdr = cdr->cdr;
		}
	}

	struct scm* y = calloc(1, sizeof(struct scm));
	y->type = type->value;
	y->car = car;
	y->cdr = cdr;
	return y;
}


struct scm* make_bytes(char* s, SCM length)
{
	struct scm* x = calloc(1, sizeof(struct scm));
	x->type = TBYTES;
	x->length = length;
	x->string = calloc(length + 1, sizeof(char));
	char *p = x->string;

	if(0 != length)
	{
		block_copy(s, p, length + 1);
	}

	return x;
}

struct scm* make_tref(struct scm* y)
{
	struct scm* x = calloc(1, sizeof(struct scm));
	x->type = TREF;
	x->car = y;
	x->cdr = 0;
	return x;
}

struct scm* make_vector__(SCM k)
{
	struct scm* x = calloc(1, sizeof(struct scm));
	struct scm* v = calloc(k, sizeof(struct scm));
	x->type = TVECTOR;
	x->length = k;
	x->cdr = v;

	for(k = k - 1; k >= 0; k = k - 1)
	{
		v->type = TREF;
		v->car = cell_unspecified;
		v->cdr = 0;
		v = v + CELL_SIZE;
	}

	return x;
}

struct scm* make_struct_(struct scm* type, struct scm* fields, struct scm* printer) /* Internal */
{
	SCM size = 2 + length__(fields);
	struct scm* v = calloc(size, sizeof(struct scm));
	struct scm* w = v + 1;
	struct scm* entry = vector_entry(type);
	struct scm* print = vector_entry(printer);

	v->type = entry->type;
	v->car = entry->car;
	v->cdr = entry->cdr;

	w->type = print->type;
	w->car = print->car;
	w->cdr = print->cdr;

	SCM i;
	for(i = 2; i < size; i = i + 1)
	{
		struct scm* e = cell_unspecified;

		if(fields != cell_nil)
		{
			e = fields->car;
			fields = fields->cdr;
		}

		entry = vector_entry(e);
		w = v + i;

		w->type = entry->type;
		w->car = entry->car;
		w->cdr = entry->cdr;
	}

	struct scm* r = calloc(1, sizeof(struct scm));
	r->type = TSTRUCT;
	r->length = size;
	r->cdr = v;
	return r;
}

struct scm* make_struct(struct scm* x) /* External */
{
	return(make_struct_(x->car, x->cdr->car, x->cdr->cdr->car));
}


struct scm* gc_check_() /* Internal*/
{
	return cell_unspecified;
}

struct scm* gc_check(struct scm* x) /* External */
{
	require(cell_nil == x, "mes_gc.c: gc_check recieved non-nil list\n");
	return gc_check_();
}

struct scm* gc_() /* Internal */
{
	return cell_unspecified;
}

struct scm* gc(struct scm* x) /* External */
{
	require(cell_nil == x, "mes_gc.c: gc recieved non-nil list\n");
	return gc_();
}

struct scm* make_tstring1(SCM n)
{
	struct scm* x = calloc(1, sizeof(struct scm));
	x->type = TSTRING;
	x->length = n;
	x->cdr = 0;
	return x;
}

struct scm* make_tstring2(struct scm* a, struct scm* b)
{
	struct scm* x = calloc(1, sizeof(struct scm));
	x->type = TSTRING;
	x->car = a;
	x->cdr = b;
	return x;
}

struct scm* make_keyword(struct scm* a, struct scm* b)
{
	struct scm* x = calloc(1, sizeof(struct scm));
	x->type = TKEYWORD;
	x->car = a;
	x->cdr = b;
	return x;
}

struct scm* make_tsymbol(struct scm* a, struct scm* b)
{
	struct scm* x = calloc(1, sizeof(struct scm));
	x->type = TSYMBOL;
	x->car = a;
	x->cdr = b;
	return x;
}

struct scm* make_port(SCM n, struct scm* s)
{
	struct scm* x = calloc(1, sizeof(struct scm));
	x->type = TPORT;
	x->port = n;
	x->cdr = s;
	return x;
}

struct scm* make_char(SCM c)
{
	struct scm* x = calloc(1, sizeof(struct scm));
	x->type = TCHAR;
	x->car = 0;
	x->value = c;
	return x;
}

struct scm* make_number_(SCM n) /* Internal */
{
	struct scm* x = calloc(1, sizeof(struct scm));
	x->type = TNUMBER;
	x->car = 0;
	x->value = n;
	return x;
}

struct scm* make_number(struct scm* x) /* External */
{
	return make_number_(x->rac);
}

struct scm* make_function_(FUNCTION n) /* Internal */
{
	struct scm* x = calloc(1, sizeof(struct scm));
	x->type = TNUMBER;
	x->car = 0;
	x->func_cdr = n;
	return x;
}

struct scm* make_function(struct scm* x) /* External */
{
	return make_function_(x->func_car);
}

struct scm* make_tmacro(struct scm* a, struct scm* b)
{
	struct scm* x = calloc(1, sizeof(struct scm));
	x->type = TMACRO;
	x->car = a;
	x->cdr = b;
	return x;
}

struct scm* make_tcontinuation(SCM a, SCM b)
{
	struct scm* x = calloc(1, sizeof(struct scm));
	x->type = TCONTINUATION;
	x->length = a;
	x->value = b;
	return x;
}

struct scm* make_closure_(struct scm* args, struct scm* body, struct scm* a)  /* ((internal)) */
{
	struct scm* x = calloc(1, sizeof(struct scm));
	x->type = TCLOSURE;
	x->car = cell_f;
	x->cdr = make_tpair(make_tpair(cell_circular, a), make_tpair(args, body));
	return x;
}

struct scm* make_variable_(struct scm* var)  /* ((internal)) */
{
	struct scm* x = calloc(1, sizeof(struct scm));
	x->type = TVARIABLE;
	x->car = var;
	x->cdr = 0;
	return x;
}
