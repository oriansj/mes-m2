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

#include "mes.h"

/* Deal with the fact GCC converts the 1 to the size of the structs being iterated over */
#define CELL_SIZE 1
//CONSTANT CELL_SIZE sizeof(struct cell)
struct cell* list_to_vector(struct cell* i);

struct cell *free_cells;
struct cell *gc_block_start;
struct cell *top_allocated;

void update_remaining()
{
	int count = 0;
	struct cell* i = free_cells;
	while(NULL != i)
	{
		count = count + 1;
		i = i->cdr;
	}
	left_to_take = count;
}

int string_size(char* a)
{
	int i = 0;
	while(0 != a[i]) i = i + 1;
	return i;
}

struct cell* insert_ordered(struct cell* i, struct cell* list)
{
	if(NULL == list)
	{
		return i;
	}

	if(i < list)
	{
		i->cdr = list;
		return i;
	}

	list->cdr = insert_ordered(i, list->cdr);
	return list;
}

void reclaim_marked()
{
	struct cell* i;
	for(i= top_allocated; i >= gc_block_start ; i = i - CELL_SIZE)
	{
		if(i->type & MARKED)
		{
			i->type = FREE;
			i->car = NULL;
			i->cdr = NULL;
			i->env = NULL;
			free_cells = insert_ordered(i, free_cells);
		}
	}
}

void reclaim_all()
{
	struct cell* i;
	for(i= top_allocated; i >= gc_block_start ; i = i - CELL_SIZE)
	{
		i->type = FREE;
		i->car = NULL;
		i->cdr = free_cells;
		i->env = NULL;
		free_cells = i;
	}
}

void relocate_cell(struct cell* current, struct cell* target, struct cell* list)
{
	for(; NULL != list; list = list->cdr)
	{
		if(list->car == current)
		{
			list->car = target;
		}

		if(list->cdr == current)
		{
			list->cdr = target;
		}

		if(list->env == current)
		{
			list->env = target;
		}

		if((list->type == CONS)|| list->type == LAMBDA)
		{
			relocate_cell(current, target, list->car);
		}
	}
}

struct cell* pop_cons();
void compact(struct cell* list)
{
	for(; NULL != list; list = list->cdr)
	{
		if((FREE != list->type) && (list > free_cells ))
		{
			struct cell* temp = pop_cons();
			temp->type = list->type;
			temp->car = list->car;
			temp->cdr = list->cdr;
			temp->env = list->env;
			relocate_cell(list, temp, all_symbols);
			relocate_cell(list, temp, top_env);
		}

		if((list->type == CONS)|| list->type == LAMBDA)
		{
			compact(list->car);
		}
	}
}


void mark_all_cells()
{
	struct cell* i;
	for(i = gc_block_start; i <= top_allocated; i = i + CELL_SIZE)
	{
		/* if not in the free list */
		if(i->type != FREE)
		{
			/* Mark it */
			i->type = i->type | MARKED;
		}
	}
}

void unmark_cells(struct cell* i)
{
	for(; NULL != i; i = i->cdr)
	{
		require(NULL != i, "unmark_cells impossible cell\n");
		if(0 == (i->type & MARKED)) return;
		i->type = i->type & ~MARKED;

		if(i->type == LAMBDA)
		{
			require(NULL != i->car, "unmark_cells impossible car 1 \n");
			unmark_cells(i->car);
			if(NULL != i->env)
			{
				unmark_cells(i->env);
			}
		}

		if((i->type == CONS) || (i->type == RECORD))
		{
			require(NULL != i->car, "unmark_cells impossible car 2 \n");
			unmark_cells(i->car);
		}
	}
}

void unmark_stack()
{
	unsigned i = 0;
	struct cell* s;
	while(i < stack_pointer)
	{
		s = g_stack[i];
		unmark_cells(s);
		i = i + 1;
	}
}

void garbage_collect()
{
	/* Step one mark all cells */
	mark_all_cells();

	/* Step two unmark cells we want to keep */
	unmark_cells(g_env);
	unmark_cells(all_symbols);
	unmark_cells(R0);
	unmark_cells(R1);
	unmark_cells(R2);
	__stdin->type = __stdin->type & ~MARKED;
	__stdout->type = __stdout->type & ~MARKED;
	__stderr->type = __stderr->type & ~MARKED;
	unmark_stack();

	/* Step three reclaim marked cells */
	reclaim_marked();

	/* Update count of free cells*/
	update_remaining();

	/* Optional step four compact cells */
/*	compact(all_symbols); */
/*	compact(top_env); */
/*	top_allocated = NULL; */
}

void garbage_init()
{
	gc_block_start = calloc(arena + 1, sizeof(struct cell));
	top_allocated = gc_block_start + arena;
	free_cells = NULL;
	reclaim_all();
/*	top_allocated = NULL; */
}

struct cell* pop_cons()
{
	if(NULL == free_cells)
	{
		file_print("OOOPS we ran out of cells", stderr);
		exit(EXIT_FAILURE);
	}
	struct cell* i;
	i = free_cells;
	free_cells = i->cdr;
	i->cdr = NULL;
/*	if(i > top_allocated)
	{
		top_allocated = i;
	} */
	left_to_take = left_to_take - 1;
	return i;
}

struct cell* make_cell(int type, struct cell* a, struct cell* b, struct cell* env)
{
	struct cell* c = pop_cons();
	c->type = type;
	c->car = a;
	c->cdr = b;
	c->env = env;
	return c;
}

/****************************************
 * Internally an INT is just a value    *
 * and a tag saying it is an INT        *
 *      ---------------------------     *
 *     | INT | VALUE | NULL | NULL |    *
 *      ---------------------------     *
 ****************************************/
struct cell* make_int(int a)
{
	struct cell* c = make_cell(INT, NULL, NULL, NULL);
	c->value = a;
	return c;
}

/****************************************
 * Internally a PORT is a pointer to a  *
 * filename, a file pointer and type tag*
 *   ------------------------------     *
 *  | PORT | POINTER | NULL | FILE |    *
 *   ------------------------------     *
 ****************************************/
struct cell* make_file(FILE* a, char* name)
{
	struct cell* c = make_cell(FILE_PORT, NULL, NULL, NULL);
	c->file = a;
	c->string = name;
	return c;
}

/****************************************
 * Internally a CHAR is just a value    *
 * and a type tag                       *
 *      ---------------------------     *
 *     | CHAR | VALUE | NULL | NULL |   *
 *      ---------------------------     *
 ****************************************/
struct cell* make_char(int a)
{
	struct cell* c = make_cell(CHAR, NULL, NULL, NULL);
	c->value = a;
	return c;
}

/****************************************
 * Internally a STRING is a pointer to  *
 * a string, its length and a type tag  *
 *  ----------------------------------  *
 * | STRING | POINTER | NULL | LENGTH | *
 *  ----------------------------------  *
 ****************************************/
struct cell* make_string(char* a, int length)
{
	struct cell* c = make_cell(STRING, NULL, NULL, NULL);
	c->string = a;
	c->length = length;
	return c;
}

/****************************************
 * Internally a SYM is just a pointer   *
 * to a string and a type tag           *
 *    -----------------------------     *
 *   | SYM | POINTER | NULL | NULL |    *
 *    -----------------------------     *
 ****************************************/
struct cell* make_sym(char* name)
{
	struct cell* c = make_cell(SYM, NULL, NULL, NULL);
	c->string = name;
	return c;
}

/****************************************
 * Internally KEYWORD is just a pointer *
 * to a string and a type tag           *
 *  ---------------------------------   *
 * | KEYWORD | POINTER | NULL | NULL |  *
 *  ---------------------------------   *
 ****************************************/
struct cell* make_keyword(char* name)
{
	struct cell* c = make_cell(KEYWORD, NULL, NULL, NULL);
	c->string = name;
	return c;
}

/****************************************
 * Internally a CONS is just 2 pointers *
 * to other CELLS and a type tag        *
 *  ---------------------------------   *
 * | CONS | POINTER | POINTER | NULL |  *
 *  ---------------------------------   *
 ****************************************/
struct cell* make_cons(struct cell* a, struct cell* b)
{
	return make_cell(CONS, a, b, NULL);
}

/********************************************
 * Internally LAMBDA is just 3 pointers     *
 * to other CELLS and a type tag            *
 *  --------------------------------------  *
 * | LAMBDA | POINTER | POINTER | POINTER | *
 *  --------------------------------------  *
 ********************************************/
struct cell* make_proc(struct cell* a, struct cell* b, struct cell* env)
{
	return make_cell(LAMBDA, a, b, env);
}

/********************************************
 * Internally MACRO is just 3 pointers      *
 * to other CELLS and a type tag            *
 *   -------------------------------------  *
 *  | MACRO | POINTER | POINTER | POINTER | *
 *   -------------------------------------  *
 ********************************************/
 struct cell* make_macro(struct cell* a, struct cell* b, struct cell* env)
{
	return make_cell(MACRO, a, b, env);
}

/****************************************
 * Internally PRIMOP is just a pointer  *
 * to a FUNCTION and a type tag         *
 *   --------------------------------   *
 *  | PRIMOP | POINTER | NULL | NULL |  *
 *   --------------------------------   *
 ****************************************/
struct cell* make_prim(FUNCTION fun)
{
	struct cell* c = make_cell(PRIMOP, NULL, NULL, NULL);
	c->function = fun;
	return c;
}

/****************************************
 * Internally VECTOR is just a pointer  *
 * to a CONS list, its length and a     *
 * type tag                             *
 *  ----------------------------------  *
 * | VECTOR | LENGTH | POINTER | NULL | *
 *  ----------------------------------  *
 ****************************************/
 struct cell* make_vector(int count, struct cell* init)
{
	struct cell* r = make_cell(VECTOR, NULL, NULL, NULL);
	struct cell* c = r;
	c->value = count;
	struct cell* i;
	for(count = count - 1; count >= 0; count = count - 1)
	{
		i = make_cell(CONS, NULL, nil, NULL);
		i->car = init;
		c->cdr = i;
		c = i;
	}
	return r;
}

/****************************************
 * Internally RECORD is just a pointer  *
 * to a VECTOR (CDR), a pointer to a    *
 * RECORD-TYPE (CAR) and a type tag     *
 *  ----------------------------------- *
 * | RECORD | POINTER | POINTER | NULL |*
 *  ----------------------------------- *
 ****************************************/
struct cell* make_record(struct cell* type, struct cell* vector)
{
	struct cell* r = make_cell(RECORD, NULL, NULL, NULL);
	r->car = type;
	require(type->cdr->value == vector->value, "mes_cell.c: make_record received vector of wrong length\n");
	r->cdr = vector;
	return r;
}

/**********************************************
 * Internally RECORD_TYPE is just a           *
 * pointer to a string (CAR), a pointer       *
 * to a VECTOR (CDR) and a type tag           *
 *  ----------------------------------------  *
 * | RECORD_TYPE | POINTER | POINTER | NULL | *
 *  ----------------------------------------  *
 **********************************************/
 struct cell* make_record_type(char* name, struct cell* list)
{
	struct cell* r = make_cell(RECORD_TYPE, NULL, NULL, NULL);
	r->string = name;
	r->cdr = list_to_vector(list);
	return r;
}

/****************************************
 * Internally EOF_OBJECT is just a type *
 * tag                                  *
 *  ---------------------------------   *
 * | EOF_OBJECT | NULL | NULL | NULL |  *
 *  ---------------------------------   *
 ****************************************/
 struct cell* make_eof()
{
	return make_cell(EOF_object, NULL, NULL, NULL);
}
