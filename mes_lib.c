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

struct scm* cons_(struct scm* x, struct scm* y);
struct scm* cstring_to_symbol(char const *s);
void vector_set_x_(struct scm* x, long i, struct scm* e);
struct scm* struct_ref_(struct scm* x, long i);
struct scm* vector_length (struct scm* x);
struct scm* make_vector__(long k);
struct scm* make_struct (struct scm* type, struct scm* fields, struct scm* printer);
struct scm* string_equal_p (struct scm* a, struct scm* b);
struct scm* vector_equal_p(struct scm* a, struct scm* b);
struct scm* eq_p (struct scm* x, struct scm* y);

struct scm* make_number(SCM n);
struct scm* make_char(SCM c);

struct scm* exit_(struct scm* x)  ///((name . "exit"))
{
	struct scm* y = x;
	assert(y->type == TNUMBER);
	exit(y->value);
}

struct scm* make_frame_type()  ///((internal))
{
	return make_struct(cell_symbol_record_type, cons_(cell_symbol_frame, cons_(cons_(cell_symbol_procedure, cell_nil), cell_nil)), cell_unspecified);
}


struct scm* make_stack_type()  ///((internal))
{
	return make_struct(cell_symbol_record_type
	                  , cons_(cell_symbol_stack, cons_(cons_(cstring_to_symbol("frames"), cell_nil), cell_nil))
	                  , cell_unspecified);
}

struct scm* stack_length(struct scm* stack)
{
	return vector_length(struct_ref_(stack, 3));
}

struct scm* stack_ref(struct scm* stack, SCM index)
{
	struct scm* y = struct_ref_(stack, 3);
	assert(y->type == TVECTOR);
	assert(index < y->length);
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
		return make_number(e->value);
	}

	return e;
}

struct scm* xassq(struct scm* x, struct scm* a)  ///for speed in core only
{
	struct scm* a2 = a;
	while(a2 != cell_nil && x != a2->car->cdr)
	{
		a2 = a2->cdr;
	}

	return a2 != cell_nil ? a2->car : cell_f;
}

struct scm* memq(struct scm* x, struct scm* a)
{
	struct scm* y = x;
	struct scm* a2 = a;
	int t = y->type;

	if(t == TCHAR || t == TNUMBER)
	{
		SCM v = y->value;

		while(a2 != cell_nil && v != a2->car->value)
		{
			a2 = a2->cdr;
		}
	}
	else if(t == TKEYWORD)
	{
		while(a2 != cell_nil && (a2->car->type != TKEYWORD || string_equal_p(x, a2->car) == cell_f))
		{
			a2 = a2->cdr;
		}
	}
	else
	{
		while(a2 != cell_nil && x != a2->car)
		{
			a2 = a2->cdr;
		}
	}

	return a2 != cell_nil ? a2 : cell_f;
}

struct scm* equal2_p(struct scm* a, struct scm* b)
{
	struct scm* a2 = a;
	struct scm* b2 = b;
	struct scm* tee = cell_t;

	if(a == b)
	{
		return tee;
	}

	if(a2->type == TPAIR && b2->type == TPAIR)
	{
		if((tee == equal2_p(a2->car, b2->car)) && (tee == equal2_p(a2->cdr, b2->cdr))) return tee;
		return cell_f;
	}

	if(a2->type == TSTRING && b2->type == TSTRING)
	{
		return string_equal_p(a, b);
	}

	if(a2->type == TVECTOR && b2->type == TVECTOR)
	{
		return vector_equal_p(a, b);
	}

	return eq_p(a, b);
}

struct scm* last_pair(struct scm* x)
{
	struct scm* y = x;
	while(y != cell_nil && y->cdr != cell_nil)
	{
		y = y->cdr;
	}

	return y;
}

struct scm* pair_p(struct scm* x)
{
	struct scm* y = x;
	return y->type == TPAIR ? cell_t : cell_f;
}
