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
	SCM x = make_cell__(TVECTOR, k, v);

	for(long i = 0; i < k; i++)
	{
		g_cells[v + i] = *vector_entry(cell_unspecified);
	}

	return Getstructscm(x);
}

struct scm* make_vector_(SCM n)
{
	return make_vector__(VALUE(n));
}

struct scm* vector_length(SCM x)
{
	struct scm* y = Getstructscm2(x, g_cells);
	assert(y->type == TVECTOR);
	return Getstructscm(MAKE_NUMBER(y->length));
}

struct scm* vector_ref_(SCM x, long i)
{
	assert(TYPE(x) == TVECTOR);
	assert(i < LENGTH(x));
	SCM e = VECTOR(x) + i;

	if(TYPE(e) == TREF)
	{
		e = REF(e);
	}

	if(TYPE(e) == TCHAR)
	{
		e = MAKE_CHAR(VALUE(e));
	}

	if(TYPE(e) == TNUMBER)
	{
		e = MAKE_NUMBER(VALUE(e));
	}

	return Getstructscm(e);
}

struct scm* vector_ref(SCM x, SCM i)
{
	return vector_ref_(x, VALUE(i));
}

struct scm* vector_entry(SCM x)
{
	if(TYPE(x) != TCHAR && TYPE(x) != TNUMBER)
	{
		x = MAKE_REF(x);
	}

	return Getstructscm2(x, g_cells);
}

struct scm* vector_set_x_(SCM x, long i, SCM e)
{
	assert(TYPE(x) == TVECTOR);
	assert(i < LENGTH(x));
	g_cells[VECTOR(x) + i] = *vector_entry(e);
	return Getstructscm(cell_unspecified);
}

struct scm* vector_set_x(SCM x, SCM i, SCM e)
{
	return vector_set_x_(x, VALUE(i), e);
}

struct scm* list_to_vector(SCM x)
{
	SCM v = GetSCM(make_vector__(length__(x)));
	SCM p = VECTOR(v);

	while(x != cell_nil)
	{
		g_cells[p++] = *vector_entry(car(x));
		x = cdr(x);
	}

	return Getstructscm(v);
}

struct scm* vector_to_list(SCM v)
{
	SCM x = cell_nil;

	for(long i = LENGTH(v); i; i--)
	{
		SCM e = VECTOR(v) + i - 1;

		if(TYPE(e) == TREF)
		{
			e = REF(e);
		}

		x = cons(e, x);
	}

	return Getstructscm(x);
}
