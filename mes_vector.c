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

SCM alloc(SCM n);
SCM make_cell__(SCM type, SCM car, SCM cdr);
SCM length__(SCM x);
SCM cons (SCM x, SCM y);
struct scm* vector_entry(SCM x);

struct scm* make_vector__(SCM k)
{
	SCM v = alloc(k);
	struct scm* x = &g_cells[make_cell__(TVECTOR, k, v)];
	SCM i;

	for(i = 0; i < k; i = i + 1)
	{
		g_cells[v + i] = *vector_entry(cell_unspecified);
	}

	return x;
}

struct scm* make_vector_(SCM n)
{
	return good2bad(make_vector__(g_cells[n].rdc), g_cells);
}

struct scm* vector_length(struct scm* x)
{
	x = bad2good(x, g_cells);
	assert(x->type == TVECTOR);
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, x->length), g_cells), g_cells);
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
		return Getstructscm2(make_cell__ (TCHAR, 0, e->value), g_cells);
	}

	if(e->type == TNUMBER)
	{
		return Getstructscm2(make_cell__ (TNUMBER, 0, e->value), g_cells);
	}

	return e;
}

struct scm* vector_ref(SCM x, SCM i)
{
	return good2bad(vector_ref_(x, g_cells[i].rdc), g_cells);
}

struct scm* vector_entry(SCM x)
{
	struct scm* y = Getstructscm2(x, g_cells);
	if(y->type != TCHAR && y->type != TNUMBER)
	{
		return Getstructscm2(make_cell__ (TREF, x, 0), g_cells);
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
	vector_set_x_(x, g_cells[i].rdc, e);
	return good2bad(Getstructscm2(cell_unspecified, g_cells), g_cells);
}

struct scm* list_to_vector(SCM x)
{
	struct scm* v = make_vector__(length__(x));
	struct scm* y = Getstructscm2(x, g_cells);
	SCM p = v->rdc;

	while(GetSCM2(y, g_cells) != cell_nil)
	{
		g_cells[p++] = *vector_entry(y->rac);
		y = bad2good(y->cdr, g_cells);
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
