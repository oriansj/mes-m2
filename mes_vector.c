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

#define VALUE(x) g_cells[x].rdc
#define TYPE(x) g_cells[x].type
#define LENGTH(x) g_cells[x].rac
#define MAKE_NUMBER(n) make_cell__ (TNUMBER, 0, (long)n)
#define VECTOR(x) g_cells[x].rdc
#define REF(x) g_cells[x].rac
#define MAKE_CHAR(n) make_cell__ (TCHAR, 0, n)
#define MAKE_REF(n) make_cell__ (TREF, n, 0)

SCM alloc(long n);
SCM make_cell__(long type, SCM car, SCM cdr);
long length__(SCM x);
SCM car (SCM x);
SCM cdr (SCM x);
SCM cons (SCM x, SCM y);
struct scm* vector_entry(SCM x);

struct scm* make_vector__(long k)
{
	SCM v = alloc(k);
	struct scm* x = &g_cells[make_cell__(TVECTOR, k, v)];

	for(long i = 0; i < k; i++)
	{
		g_cells[v + i] = *vector_entry(cell_unspecified);
	}

	return x;
}

struct scm* make_vector_(SCM n)
{
	return good2bad(make_vector__(VALUE(n)), g_cells);
}

struct scm* vector_length(struct scm* x)
{
	x = bad2good(x, g_cells);
	assert(x->type == TVECTOR);
	return Getstructscm(MAKE_NUMBER(x->length));
}

struct scm* vector_ref_(SCM table, long i)
{
	struct scm* y = Getstructscm2(table, g_cells);
	assert(y->type == TVECTOR);
	assert(i < y->length);
	struct scm* e = &g_cells[y->vector + i];

	if(e->type == TREF)
	{
		return Getstructscm2(e->ref, g_cells);
	}

	if(e->type == TCHAR)
	{
		return Getstructscm2(MAKE_CHAR(e->value), g_cells);
	}

	if(e->type == TNUMBER)
	{
		return Getstructscm2(MAKE_NUMBER(e->value), g_cells);
	}

	return e;
}

struct scm* vector_ref(SCM x, SCM i)
{
	return good2bad(vector_ref_(x, VALUE(i)), g_cells);
}

struct scm* vector_entry(SCM x)
{
	struct scm* y = Getstructscm2(x, g_cells);
	if(y->type != TCHAR && y->type != TNUMBER)
	{
		return Getstructscm2(MAKE_REF(x), g_cells);
	}

	return y;
}

void vector_set_x_(SCM x, long i, SCM e)
{
	struct scm* y = Getstructscm2(x, g_cells);
	assert(y->type == TVECTOR);
	assert(i < y->length);
	g_cells[y->vector + i] = *vector_entry(e);
}

struct scm* vector_set_x(SCM x, SCM i, SCM e)
{
	vector_set_x_(x, VALUE(i), e);
	return Getstructscm(cell_unspecified);
}

struct scm* list_to_vector(SCM x)
{
	struct scm* v = make_vector__(length__(x));
	SCM p = v->rdc;

	while(x != cell_nil)
	{
		g_cells[p++] = *vector_entry(car(x));
		x = cdr(x);
	}

	return good2bad(v, g_cells);
}

struct scm* vector_to_list(struct scm* v)
{
	v = bad2good(v, g_cells);
	struct scm* x = &g_cells[cell_nil];
	SCM i;

	for(i = v->length; i; i = i - 1)
	{
		struct scm* f = &g_cells[v->vector + i -1];

		if(f->type == TREF)
		{
			f = &g_cells[f->ref];
		}

		x = Getstructscm2(cons(GetSCM2(f, g_cells), GetSCM2(x, g_cells)), g_cells);
	}

	return good2bad(x, g_cells);
}
