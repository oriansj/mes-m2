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

struct scm* length__(struct scm* x);
struct scm* cons(struct scm* x, struct scm* y);
struct scm* equal2_p(struct scm* a, struct scm* b);
struct scm* vector_entry(struct scm* x);
struct scm* make_vector__(struct scm* k);

struct scm* make_number(SCM n);
struct scm* make_char(SCM c);
struct scm* make_tref(struct scm* x);
void require(int bool, char* error);

struct scm* make_vector_(struct scm* n)
{
	struct scm* m = n;
	return make_vector__(m->cdr);
}

struct scm* vector_length(struct scm* x)
{
	require(x->type == TVECTOR, "mes_vector.c: vector_length x was not a TVECTOR\n");
	return make_number(x->length);
}

struct scm* vector_ref_(struct scm* table, SCM i)
{
	struct scm* y = table;
	require(y->type == TVECTOR, "mes_vector.c: vector_ref_ table was not a TVECTOR\n");
	require(i < y->length, "mes_vector.c: vector_ref_ i was not less than table->length\n");
	struct scm* e = y->cdr + i;

	if(e->type == TREF)
	{
		return e->car;
	}

	return e;
}

struct scm* vector_equal_p(struct scm* a, struct scm* b)
{
	struct scm* a2 = a;
	struct scm* b2 = b;

	if(a2->length != b2->length)
	{
		return cell_f;
	}

	SCM i;
	for(i = 0; i < a2->length; i = i + 1)
	{
		struct scm* ai = a2->vector + i;
		struct scm* ai2 = ai;
		struct scm* bi = b2->vector + i;
		struct scm* bi2 = bi;

		if(ai2->type == TREF)
		{
			ai = ai2->car;
		}

		if(bi2->type == TREF)
		{
			bi = bi2->car;
		}

		if(equal2_p(ai, bi) == cell_f)
		{
			return cell_f;
		}
	}
	return cell_t;
}

struct scm* vector_ref(struct scm* x, struct scm* i)
{
	require(x->type == TVECTOR, "mes_vector.c: vector_ref x was not of type TVECTOR\n");
	require(i->value < x->length, "mes_vector.c: vector_ref i->value was not less than x->length\n");
	struct scm* e = x->cdr + i->value;

	if(e->type == TREF)
	{
		return e->car;
	}

	return e;
}

struct scm* vector_entry(struct scm* x)
{
	if(TCHAR == x->type)
	{
		return x;
	}

	if(TNUMBER == x->type)
	{
		return x;
	}

	return make_tref(x);
}

void vector_set_x_(struct scm* x, SCM i, struct scm* e)
{
	require(x->type == TVECTOR, "mes_vector.c: vector_set_x_ x was not of type TVECTOR\n");
	require(i < x->length, "mes_vector.c: vector_set_x i was not less than x->length\n");
	struct scm* z = x->cdr + i;
	struct scm* f = vector_entry(e);

	z->type = f->type;
	z->car = f->car;
	z->cdr = f->cdr;
}

struct scm* vector_set_x(struct scm* x)
{
	SCM i = x->cdr->car->value;
	require(x->car->type == TVECTOR, "mes_vector.c: vector_set_x x->car was not of type TVECTOR\n");
	require(i < x->car->length, "mes_vector.c: vector_set_x i was not less than x->car->length\n");
	struct scm* z = x->car->cdr + (i * CELL_SIZE);
	struct scm* f = vector_entry(x->cdr->cdr->car);

	z->type = f->type;
	z->car = f->car;
	z->cdr = f->cdr;
	return cell_unspecified;
}

struct scm* list_to_vector(struct scm* x)
{
	struct scm* v = make_vector__(length__(x));
	struct scm* y = x;
	struct scm* p = v->cdr;
	struct scm* z;

	while(y != cell_nil)
	{
		z = vector_entry(y->car);
		p->type = z->type;
		p->car = z->car;
		p->cdr = z->cdr;
		p = p + 1;
		y = y->cdr;
	}

	return v;
}

struct scm* vector_to_list(struct scm* v)
{
	struct scm* x = cell_nil;
	SCM i;

	for(i = v->length; i; i = i - 1)
	{
		struct scm* f = v->cdr + i -1;

		if(f->type == TREF)
		{
			f = f->car;
		}

		x = cons(f, x);
	}

	return x;
}
