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
#include "mes_constants.h"

struct scm* cstring_to_symbol(char* s);
struct scm* eq_p_(struct scm* x, struct scm* y);
struct scm* make_char(SCM c);
struct scm* make_number_(SCM n);
struct scm* make_struct_(struct scm* type, struct scm* fields, struct scm* printer);
struct scm* make_tpair(struct scm* a, struct scm* b);
struct scm* make_vector__(SCM k);
struct scm* string_equal_p_(struct scm* a, struct scm* b);
struct scm* struct_ref_(struct scm* x, SCM i);
struct scm* vector_equal_p_(struct scm* a, struct scm* b);
struct scm* vector_length_(struct scm* x);
void require(int bool, char* error);
void vector_set_x_(struct scm* x, SCM i, struct scm* e);

struct scm* scm_exit(struct scm* x)  /* External */
{
	struct scm* y = x->car;
	require(TNUMBER == y->type, "exit_ in mes_lib.c didn't recieve a number\n");
	exit(y->value);
}

struct scm* make_frame_type()  /* ((internal)) */
{
	return make_struct_(cell_symbol_record_type, make_tpair(cell_symbol_frame, make_tpair(make_tpair(cell_symbol_procedure, cell_nil), cell_nil)), cell_unspecified);
}


struct scm* make_stack_type()  /* ((internal)) */
{
	return make_struct_(cell_symbol_record_type
	                  , make_tpair(cell_symbol_stack, make_tpair(make_tpair(cstring_to_symbol("frames"), cell_nil), cell_nil))
	                  , cell_unspecified);
}

struct scm* stack_length(struct scm* stack) /* External */
{
	return vector_length_(struct_ref_(stack->car, 3));
}

struct scm* stack_ref(struct scm* x) /* External */
{
	SCM index = x->cdr->rac;
	struct scm* stack = x->car;
	struct scm* y = struct_ref_(stack, 3);
	require(TVECTOR == y->type, "stack_ref in mes_lib.c did not recieve a TVECTOR\n");
	require(index < y->length, "y->length in stack_ref in mes_lib.c was less than or equal to index\n");
	struct scm* e = y->cdr + index;

	if(e->type == TREF)
	{
		return e->car;
	}

	if(e->type == TCHAR)
	{
		return make_char(e->value);
	}

	if(e->type == TNUMBER)
	{
		return make_number_(e->value);
	}

	return e;
}

struct scm* xassq(struct scm* x)  /* External */
{
	struct scm* a = x->cdr->car;
	x = x->car;
	while(a != cell_nil && x != a->car->cdr)
	{
		a = a->cdr;
	}

	if(cell_nil == a) return cell_f;
	return a->car;
}

struct scm* memq(struct scm* x) /* External */
{
	struct scm* a = x->cdr->car;
	x = x->car;
	int t = x->type;

	if(t == TCHAR || t == TNUMBER)
	{
		SCM v = x->value;

		while(a != cell_nil && v != a->car->value)
		{
			a = a->cdr;
		}
	}
	else if(t == TKEYWORD)
	{
		while(a != cell_nil && (a->car->type != TKEYWORD || string_equal_p_(x, a->car) == cell_f))
		{
			a = a->cdr;
		}
	}
	else
	{
		while(a != cell_nil && x != a->car)
		{
			a = a->cdr;
		}
	}

	if(cell_nil == a) return cell_f;
	return a;
}

struct scm* equal2_p_(struct scm* a, struct scm* b) /* Internal */
{
	if(a == b)
	{
		return cell_t;
	}

	if(a->type == TPAIR && b->type == TPAIR)
	{
		if((cell_t == equal2_p_(a->car, b->car)) && (cell_t == equal2_p_(a->cdr, b->cdr))) return cell_t;
		return cell_f;
	}

	if(a->type == TSTRING && b->type == TSTRING)
	{
		return string_equal_p_(a, b);
	}

	if(a->type == TVECTOR && b->type == TVECTOR)
	{
		return vector_equal_p_(a, b);
	}

	return eq_p_(a, b);
}

struct scm* equal2_p(struct scm* x) /* External */
{
	return equal2_p_(x->car, x->cdr->car);
}

struct scm* last_pair(struct scm* x) /* External */
{
	x = x->car;
	while(x != cell_nil && x->cdr != cell_nil)
	{
		x = x->cdr;
	}

	return x;
}

struct scm* pair_p(struct scm* x) /* External */
{
	x = x->car;
	if(TPAIR == x->type) return cell_t;
	return cell_f;
}
