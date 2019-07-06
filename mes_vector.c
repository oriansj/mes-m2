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

SCM make_cell__(SCM type, SCM car, SCM cdr);
SCM length__(SCM x);
struct scm* cons(struct scm* x, struct scm* y);
struct scm* equal2_p(SCM a, SCM b);
struct scm* vector_entry(SCM x);
struct scm* make_vector__(SCM k);


struct scm* make_vector_(SCM n)
{
	struct scm* m = Getstructscm2(n);
	return good2bad(make_vector__(m->rdc));
}

struct scm* vector_length(struct scm* x)
{
	x = bad2good(x);
	assert(x->type == TVECTOR);
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, x->length)));
}

struct scm* vector_ref_(SCM table, long i)
{
	struct scm* y = Getstructscm2(table);
	assert(y->type == TVECTOR);
	assert(i < y->length);
	struct scm* e = bad2good(y->cdr) + i;

	if(e->type == TREF)
	{
		return bad2good(e->car);
	}

	if(e->type == TCHAR)
	{
		return Getstructscm2(make_cell__ (TCHAR, 0, e->value));
	}

	if(e->type == TNUMBER)
	{
		return Getstructscm2(make_cell__ (TNUMBER, 0, e->value));
	}

	return e;
}

struct scm* vector_equal_p(SCM a, SCM b)
{
	struct scm* a2 = Getstructscm2(a);
	struct scm* b2 = Getstructscm2(b);

	if(a2->length != b2->length)
	{
		return good2bad(Getstructscm2(cell_f));
	}

	for(long i = 0; i < a2->length; i++)
	{
		SCM ai = a2->vector + i;
		struct scm* ai2 = Getstructscm2(ai);
		SCM bi = b2->vector + i;
		struct scm* bi2 = Getstructscm2(bi);

		if(ai2->type == TREF)
		{
			ai = ai2->rac;
		}

		if(bi2->type == TREF)
		{
			bi = bi2->rac;
		}

		if(equal2_p(ai, bi) == good2bad(Getstructscm2(cell_f)))
		{
			return good2bad(Getstructscm2(cell_f));
		}
	}
	return good2bad(Getstructscm2(cell_t));
}

struct scm* vector_ref(SCM x, SCM i)
{
	struct scm* h = Getstructscm2(i);
	return good2bad(vector_ref_(x, h->rdc));
}

struct scm* vector_entry(SCM x)
{
	struct scm* y = Getstructscm2(x);
	if(y->type != TCHAR && y->type != TNUMBER)
	{
		return Getstructscm2(make_cell__ (TREF, x, 0));
	}

	return y;
}

void vector_set_x_(SCM x, long i, SCM e)
{
	struct scm* y = Getstructscm2(x);
	assert(y->type == TVECTOR);
	assert(i < y->length);
	struct scm* z = bad2good(y->cdr) + i;
	struct scm* f = vector_entry(e);
	/* The below is likely going to be a problem for M2-Planet until we add pointer dereferencing */
	*z = *f;
}

struct scm* vector_set_x(SCM x, SCM i, SCM e)
{
	struct scm* h = Getstructscm2(i);
	vector_set_x_(x, h->rdc, e);
	return good2bad(Getstructscm2(cell_unspecified));
}

struct scm* list_to_vector(SCM x)
{
	struct scm* v = make_vector__(length__(x));
	struct scm* y = Getstructscm2(x);
	struct scm* p = bad2good(v->cdr);
	struct scm* z;

	while(GetSCM2(y) != cell_nil)
	{
		z = vector_entry(y->rac);
		/* The below is likely going to be a problem for M2-Planet until we add pointer dereferencing */
		*p = *z;
		p = p + 1;
		y = bad2good(y->cdr);
	}

	return good2bad(v);
}

struct scm* vector_to_list(struct scm* v)
{
	v = bad2good(v);
	struct scm* x = Getstructscm2(cell_nil);
	SCM i;

	for(i = v->length; i; i = i - 1)
	{
		struct scm* f = bad2good(v->cdr) + i -1;

		if(f->type == TREF)
		{
			f = bad2good(f->car);
		}

		x = cons(f, x);
	}

	return good2bad(x);
}
