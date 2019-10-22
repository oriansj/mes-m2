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
struct scm* equal2_p_(struct scm* a, struct scm* b);
struct scm* vector_entry(struct scm* x);
struct scm* make_vector__(struct scm* k);

struct scm* make_number_(SCM n);
struct scm* make_tpair(struct scm* a, struct scm* b);
struct scm* make_char(SCM c);
struct scm* make_tref(struct scm* x);
void require(int bool, char* error);

struct scm* make_vector(struct scm* x) /* External */
{
	struct scm* m = x->car;
	return make_vector__(m->cdr);
}

struct scm* vector_length_(struct scm* x) /* Internal */
{
	require(x->type == TVECTOR, "mes_vector.c: vector_length x was not a TVECTOR\n");
	return make_number_(x->length);
}

struct scm* vector_length(struct scm* x) /* External */
{
	return vector_length_(x->car);
}

struct scm* vector_ref_(struct scm* table, SCM i) /* Internal */
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

struct scm* vector_equal_p(struct scm* a)
{
	struct scm* a2 = a->car;
	struct scm* b2 = a->cdr->car;

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

		if(equal2_p_(ai, bi) == cell_f)
		{
			return cell_f;
		}
	}
	return cell_t;
}

struct scm* vector_ref(struct scm* x) /* External */
{
	return vector_ref_(x->car, x->cdr->car->value);
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

void vector_set_x_(struct scm* x, SCM i, struct scm* e) /* Internal */
{
	require(x->type == TVECTOR, "mes_vector.c: vector_set_x_ x was not of type TVECTOR\n");
	require(i < x->length, "mes_vector.c: vector_set_x i was not less than x->length\n");
	struct scm* z = x->cdr + i;
	struct scm* f = vector_entry(e);

	z->type = f->type;
	z->car = f->car;
	z->cdr = f->cdr;
}

struct scm* vector_set_x(struct scm* x) /* External */
{
	vector_set_x_(x->car, x->cdr->car->value, x->cdr->cdr->car);
	return cell_unspecified;
}

struct scm* list_to_vector_(struct scm* x) /* Internal*/
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

struct scm* list_to_vector(struct scm* x) /* External */
{
	return list_to_vector_(x->car);
}

struct scm* vector_to_list(struct scm* x) /* External */
{
	x = x->car;
	struct scm* r = cell_nil;
	SCM i;

	for(i = x->length; i; i = i - 1)
	{
		struct scm* f = x->cdr + i -1;

		if(f->type == TREF)
		{
			f = f->car;
		}

		r = make_tpair(f, r);
	}

	return r;
}
