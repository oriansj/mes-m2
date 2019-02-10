/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

// CONSTANT STRUCT_TYPE 0
#define STRUCT_TYPE 0
// CONSTANT STRUCT_PRINTER 1
#define STRUCT_PRINTER 1

long length__(SCM x);
SCM alloc(long n);
SCM make_cell__(long type, SCM car, SCM cdr);
struct scm* vector_entry(SCM x);

struct scm* make_struct(SCM type, struct scm* fields, SCM printer)
{
	fields = bad2good(fields, g_cells);
	long size = 2 + length__(GetSCM2(fields, g_cells));
	SCM v = alloc(size);
	g_cells[v] = *vector_entry(type);
	g_cells[v + 1] = *vector_entry(printer);

	for(long i = 2; i < size; i++)
	{
		SCM e = cell_unspecified;

		if(fields != &g_cells[cell_nil])
		{
			e = fields->rac;
			fields = &g_cells[fields->rdc];
		}

		g_cells[v + i] = *vector_entry(e);
	}

	return good2bad(Getstructscm2(make_cell__(TSTRUCT, size, v), g_cells), g_cells);
}

struct scm* struct_length(struct scm* x)
{
	x = bad2good(x, g_cells);
	assert(x->type == TSTRUCT);
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, x->length), g_cells), g_cells);
}

struct scm* struct_ref_(struct scm* x, long i)
{
	x = bad2good(x, g_cells);
	assert(x->type == TSTRUCT);
	assert(i < x->length);
	struct scm* f = &g_cells[x->struc + i];

	if(f->type == TREF)
	{
		return Getstructscm2(f->ref, g_cells);
	}

	if(f->type == TCHAR)
	{
		return Getstructscm2(make_cell__ (TCHAR, 0, f->rdc), g_cells);
	}

	if(f->type == TNUMBER)
	{
		return Getstructscm2(make_cell__ (TNUMBER, 0, f->rdc), g_cells);
	}

	return f;
}

struct scm* struct_set_x_(struct scm* x, long i, SCM e)
{
	x = bad2good(x, g_cells);
	assert(x->type == TSTRUCT);
	assert(i < x->length);
	g_cells[x->rdc + i] = *vector_entry(e);
	return good2bad(Getstructscm2(cell_unspecified, g_cells), g_cells);
}

struct scm* struct_ref(SCM x, SCM i) /* External */
{
	return good2bad(struct_ref_(good2bad(Getstructscm2(x, g_cells), g_cells), g_cells[i].rdc), g_cells);
}

struct scm* struct_set_x(SCM x, SCM i, SCM e)
{
	return struct_set_x_(good2bad(Getstructscm2(x, g_cells), g_cells), g_cells[i].rdc, e);
}
