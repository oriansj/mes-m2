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
/* Imported functions */
struct cell* list_to_vector(struct cell* i);


/* Deal with the fact GCC converts the 1 to the size of the structs being iterated over */
//CONSTANT CELL_SIZE sizeof(struct cell)
#define CELL_SIZE 1


/* Globals needed for garbage collection */
struct cell *free_cells;
struct cell *gc_block_start;
struct cell *top_allocated;


/****************************************
 * Efficient tracking of how much free  *
 * Space we have to work with requires  *
 * fixing after garbage collection.     *
 * This is probably the least efficient *
 * way of doing this but it is good     *
 * Enough for now.                      *
 *                                      *
 * Simply walk the list of free cells   *
 * to determine the number of free      *
 * cells we have                        *
 ****************************************/
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


/****************************************
 * If one wishes to allocate from the   *
 * bottom of memory to the top to make  *
 * compaction easier to reason about;   *
 * one needs to allocate memory from    *
 * low to high and a sorted free list   *
 * makes that task much faster.         *
 *                                      *
 * TODO: replace with a loop            *
 ****************************************/
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


/****************************************
 * The Sweep part of the Mark and sweep *
 * garbage collection.                  *
 *                                      *
 *  *One of few performance critical*   *
 *                                      *
 * By sweeping from high to low memory  *
 * addresses we reduce the number of    *
 * recursions needed by insert_ordered  *
 * on average as high blocks should be  *
 * freed more frequently than low       *
 * blocks.                              *
 ****************************************/
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


/****************************************
 * Compaction require relocation of     *
 * cells and the correction of all      *
 * references to those cells.           *
 *                                      *
 *     * CORRECTNESS IS ESSENTIAL *     *
 *  *CRITICAL TO COMPACTION PERFORMACE* *
 *                                      *
 * TODO: ensure correctness before      *
 * enabling function.                   *
 ****************************************/
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


/****************************************
 * A centralized method of allocating   *
 * the free cells to calling functions  *
 *                                      *
 * An ideal place to catch memory       *
 * exhustion errors and engage in       *
 * active garbage collection.           *
 *                                      *
 * The returned cell should have ALL    *
 * fields equal to NULL.                *
 ****************************************/
struct cell* pop_cons()
{
	if(NULL == free_cells)
	{
		/* We have to get free cells if possible */
		garbage_collect();
		require(NULL != free_cells, "OOOPS we ran out of cells\n");
	}
	struct cell* i;
	i = free_cells;
	free_cells = i->cdr;
	i->cdr = NULL;
	left_to_take = left_to_take - 1;
	return i;
}


/****************************************
 * The core of compaction, finding      *
 * cells that in a higher address than  *
 * available free cells, then having    *
 * those relocated.                     *
 ****************************************/
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


/****************************************
 * The first half of the mark phase of  *
 * mark and sweep.                      *
 *                                      *
 *  *One of few performance critical*   *
 *                                      *
 * The bottom 2 bits of the TYPE are    *
 * reserved with the bottom bit         *
 * explicitly for MARKED. This is to    *
 * prevent changing of types when       *
 * marking cells. This is done from low *
 * to high to allow optimaizations like *
 * only having to mark cells that are   *
 * not already free.                    *
 * You don't want to mark FREE cells so *
 * That if you wish to later remove the *
 * counting of free cells at the end    *
 * you can do so without the fear of    *
 * double counting.                     *
 ****************************************/
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


/****************************************
 * The second half of the mark phase of *
 * mark and sweep.                      *
 *                                      *
 *  *One of few performance critical*   *
 *     * CORRECTNESS IS ESSENTIAL *     *
 *                                      *
 * Essentially we need to UNMARK all    *
 * cells which we do not want garbage   *
 * collected.                           *
 *                                      *
 * This function needs to be called on  *
 * ALL ROOTs if any are missed we are   *
 * going to lose data and suffer from   *
 * CORRUPTION which will crash your     *
 * program in hard to debug ways.       *
 ****************************************/
void unmark_cells(struct cell* i)
{
	/* Iteratively walk through cdrs because that path is the most numerous */
	for(; NULL != i; i = i->cdr)
	{
		require(NULL != i, "unmark_cells impossible cell\n");
		/* If we hit an unmarked cell it means we already did that tree so STOP */
		if(0 == (i->type & MARKED)) return;
		i->type = i->type & ~MARKED;

		/* Deal with TYPE that set CAR to be other cells */
		if((i->type == CONS) || (i->type == RECORD) || (i->type == LAMBDA) || (i->type == MACRO))
		{
			require(NULL != i->car, "unmark_cells impossible car\n");
			unmark_cells(i->car);
		}

		/* Deal with the TYPES that set ENV to be other cells */
		if((i->type == LAMBDA) || (i->type == MACRO))
		{
			require((NULL != i->env), "unmark_cells impossible env\n");
			unmark_cells(i->env);
		}
	}
}


/****************************************
 * Nearly all of the essential roots    *
 * are created during evaluation and    *
 * need to be protected from garbage    *
 * collection until the evaluation      *
 * completes. Thus they are pushed unto *
 * the stack and will be unmarked as    *
 * efficiently as possible              *
 *                                      *
 *     * CORRECTNESS IS ESSENTIAL *     *
 * never push non-cells onto the stack  *
 *                                      *
 * NULL vales are fine but promptly     *
 * ignored by unmark_cells.             *
 * The stack pointer always points to   *
 * the first free cell on the stack.    *
 ****************************************/
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


/****************************************
 * The function that orchestrates the   *
 * whole of the mark and sweep garbage  *
 * collection in mes-m2.                *
 *                                      *
 *     * CORRECTNESS IS ESSENTIAL *     *
 *                                      *
 * If any ROOTs are missed here the     *
 * damage will not be fixable.          *
 * This is also the point where         *
 * compaction is enabled/disabled       *
 ****************************************/
void garbage_collect()
{
	/* Step zero: mark all cells */
	mark_all_cells();

	/* Step one: unmark cells we want to keep */
	unmark_cells(g_env);
	unmark_cells(all_symbols);
	unmark_cells(R0);
	unmark_cells(R1);
	unmark_cells(R2);
	__stdin->type = __stdin->type & ~MARKED;
	__stdout->type = __stdout->type & ~MARKED;
	__stderr->type = __stderr->type & ~MARKED;
	unmark_stack();

	/* Step two: reclaim marked cells */
	reclaim_marked();

	/* Step three: Update count of free cells*/
	update_remaining();

	/* Optional step four: compact cells */
/*	compact(all_symbols); */
/*	compact(top_env); */
}


/****************************************
 * We need to create our pool of cells  *
 * to work with in the first place      *
 *                                      *
 * Now that we have a block of memory   *
 * (in a possibly unknown state) we     *
 * need to make it ready for general    *
 * use. We go from high to low to       *
 * reduce the number of cycles needed   *
 * to make our free list sorted from    *
 * low to high. (major part init speed) *
 ****************************************/
void garbage_init()
{
	gc_block_start = calloc(arena + 1, sizeof(struct cell));
	top_allocated = gc_block_start + (arena * CELL_SIZE);
	free_cells = NULL;
	struct cell* i;
	for(i = top_allocated; i >= gc_block_start ; i = i - CELL_SIZE)
	{
		i->type = FREE;
		i->car = NULL;
		i->cdr = free_cells;
		i->env = NULL;
		free_cells = i;
	}
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
	struct cell* c = pop_cons();
	c->type = INT;
	c->value = a;
	return c;
}

/****************************************
 * Internally a PORT is a pointer to a  *
 * filename (CAR), a file pointer (ENV) *
 * and type tag                         *
 *    ------------------------------    *
 *   | PORT | POINTER | NULL | FILE |   *
 *    ------------------------------    *
 ****************************************/
struct cell* make_file(FILE* a, char* name)
{
	struct cell* c = pop_cons();
	c->type = FILE_PORT;
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
	struct cell* c = pop_cons();
	c->type = CHAR;
	c->value = a;
	return c;
}

/****************************************
 * Internally a STRING is a pointer to  *
 * a string (CAR), its length (ENV) and *
 * a type tag                           *
 *  ----------------------------------  *
 * | STRING | POINTER | NULL | LENGTH | *
 *  ----------------------------------  *
 ****************************************/
struct cell* make_string(char* a, int length)
{
	struct cell* c = pop_cons();
	c->type = STRING;
	c->string = a;
	c->length = length;
	return c;
}

/****************************************
 * Internally a SYM is just a pointer   *
 * to a string (CAR) and a type tag     *
 *    -----------------------------     *
 *   | SYM | POINTER | NULL | NULL |    *
 *    -----------------------------     *
 ****************************************/
struct cell* make_sym(char* name)
{
	struct cell* c = pop_cons();
	c->type = SYM;
	c->string = name;
	return c;
}

/****************************************
 * Internally KEYWORD is just a pointer *
 * to a string (CAR) and a type tag     *
 *  ---------------------------------   *
 * | KEYWORD | POINTER | NULL | NULL |  *
 *  ---------------------------------   *
 ****************************************/
struct cell* make_keyword(char* name)
{
	struct cell* c = pop_cons();
	c->type = KEYWORD;
	c->string = name;
	return c;
}

/****************************************
 * Internally a CONS is just 2 pointers *
 * to other CELLS (CAR and CDR)  and a  *
 * type tag                             *
 *  ---------------------------------   *
 * | CONS | POINTER | POINTER | NULL |  *
 *  ---------------------------------   *
 ****************************************/
struct cell* make_cons(struct cell* a, struct cell* b)
{
	struct cell* c = pop_cons();
	c->type = CONS;
	c->car = a;
	c->cdr = b;
	return c;
}

/********************************************
 * Internally LAMBDA is just 3 pointers     *
 * to other CELLS and a type tag            *
 * CAR is a list of local variables         *
 * CDR is the S-expression to execute       *
 * ENV is the environment the lambda was    *
 * defined in                               *
 *  --------------------------------------  *
 * | LAMBDA | POINTER | POINTER | POINTER | *
 *  --------------------------------------  *
 ********************************************/
struct cell* make_proc(struct cell* a, struct cell* b, struct cell* env)
{
	struct cell* c = pop_cons();
	c->type = LAMBDA;
	c->car = a;
	c->cdr = b;
	c->env = env;
	return c;
}

/********************************************
 * Internally MACRO is just 3 pointers      *
 * to other CELLS and a type tag            *
 * CAR is a list of local variables         *
 * CDR is the S-expression to execute       *
 * ENV is the environment the macro was     *
 * defined in                               *
 *   -------------------------------------  *
 *  | MACRO | POINTER | POINTER | POINTER | *
 *   -------------------------------------  *
 ********************************************/
 struct cell* make_macro(struct cell* a, struct cell* b, struct cell* env)
{
	struct cell* c = pop_cons();
	c->type = MACRO;
	c->car = a;
	c->cdr = b;
	c->env = env;
	return c;
}

/****************************************
 * Internally PRIMOP is just a pointer  *
 * to a FUNCTION (CAR) and a type tag   *
 *   --------------------------------   *
 *  | PRIMOP | POINTER | NULL | NULL |  *
 *   --------------------------------   *
 ****************************************/
struct cell* make_prim(FUNCTION fun)
{
	struct cell* c = pop_cons();
	c->type = PRIMOP;
	c->function = fun;
	return c;
}

/****************************************
 * Internally VECTOR is just a pointer  *
 * to a CONS list (CDR), its length     *
 * (CAR) and a type tag                 *
 * each cons points to an entry in the  *
 * vector and the next cons cell until  *
 * the last cons which points to nil    *
 *  ----------------------------------  *
 * | VECTOR | LENGTH | POINTER | NULL | *
 *  ----------------------------------  *
 ****************************************/
 struct cell* make_vector(int count, struct cell* init)
{
	/* Create Vector */
	struct cell* r = pop_cons();
	r->type = VECTOR;
	r->value = count;

	/* Create cons list inside of vector */
	struct cell* c;
	struct cell* i;
	for(c = r; count > 0; count = count - 1)
	{
		i = pop_cons();
		i->type = CONS;
		i->cdr = nil;
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
 * The vector containing the entries in *
 * the record itself                    *
 *  ----------------------------------- *
 * | RECORD | POINTER | POINTER | NULL |*
 *  ----------------------------------- *
 ****************************************/
struct cell* make_record(struct cell* type, struct cell* vector)
{
	struct cell* r = pop_cons();
	r->type = RECORD;
	r->car = type;
	require(type->cdr->value == vector->value, "mes_cell.c: make_record received vector of wrong length\n");
	r->cdr = vector;
	return r;
}

/**********************************************
 * Internally RECORD_TYPE is just a           *
 * pointer to a string (CAR), a pointer       *
 * to a VECTOR (CDR) and a type tag           *
 * The vector containing the names of the     *
 * entries in the record-type                 *
 *  ----------------------------------------  *
 * | RECORD_TYPE | POINTER | POINTER | NULL | *
 *  ----------------------------------------  *
 **********************************************/
 struct cell* make_record_type(char* name, struct cell* list)
{
	struct cell* r = pop_cons();
	r->type = RECORD_TYPE;
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
	struct cell* c = pop_cons();
	c->type = EOF_object;
	return c;
}
