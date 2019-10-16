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

struct scm* make_vector_(struct scm* n)
{
	struct scm* m = n;
	return make_vector__(m->cdr);
}

struct scm* vector_length(struct scm* x)
{
	assert(x->type == TVECTOR);
	return make_number(x->length);
}

struct scm* vector_ref_(struct scm* table, long i)
{
	struct scm* y = table;
	assert(y->type == TVECTOR);
	assert(i < y->length);
	struct scm* e = y->cdr + i;

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

struct scm* vector_equal_p(struct scm* a, struct scm* b)
{
	struct scm* a2 = a;
	struct scm* b2 = b;

	if(a2->length != b2->length)
	{
		return cell_f;
	}

	for(long i = 0; i < a2->length; i++)
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
	struct scm* h = i;
	return vector_ref_(x, h->value);
}

struct scm* vector_entry(struct scm* x)
{
	struct scm* y = x;
	if(y->type != TCHAR && y->type != TNUMBER)
	{
		return make_tref(x);
	}

	return y;
}

void vector_set_x_(struct scm* x, long i, struct scm* e)
{
	struct scm* y = x;
	assert(y->type == TVECTOR);
	assert(i < y->length);
	struct scm* z = y->cdr + i;
	struct scm* f = vector_entry(e);
	/* The below is likely going to be a problem for M2-Planet until we add pointer dereferencing */
	*z = *f;
}

struct scm* vector_set_x(struct scm* x, struct scm* i, struct scm* e)
{
	struct scm* h = i;
	vector_set_x_(x, h->value, e);
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
		/* The below is likely going to be a problem for M2-Planet until we add pointer dereferencing */
		*p = *z;
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
