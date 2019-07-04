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

SCM cons_(SCM x, SCM y);
struct scm* cstring_to_symbol(char const *s);
void vector_set_x_(SCM x, long i, SCM e);
struct scm* struct_ref_(SCM x, long i);
struct scm* vector_length (struct scm* x);

SCM make_cell__(SCM type, SCM car, SCM cdr);
struct scm* make_vector__(long k);
struct scm* make_struct (SCM type, SCM fields, SCM printer);

struct scm* string_equal_p (SCM a, SCM b);
struct scm* vector_equal_p(SCM a, SCM b);
struct scm* eq_p (SCM x, SCM y);

struct scm* exit_(SCM x)  ///((name . "exit"))
{
	struct scm* y = Getstructscm2(x, g_cells);
	assert(y->type == TNUMBER);
	exit(y->value);
}

struct scm* make_frame_type()  ///((internal))
{
	return good2bad(make_struct(cell_symbol_record_type, cons_(cell_symbol_frame, cons_(cons_(cell_symbol_procedure, cell_nil), cell_nil)), cell_unspecified), g_cells);
}


struct scm* make_stack_type()  ///((internal))
{
	return good2bad(make_struct(cell_symbol_record_type
	                  , cons_(cell_symbol_stack, cons_(cons_(GetSCM2(cstring_to_symbol("frames"), g_cells), cell_nil), cell_nil))
	                  , cell_unspecified), g_cells);
}

struct scm* stack_length(SCM stack)
{
	return vector_length(good2bad(struct_ref_(stack, 3), g_cells));
}

struct scm* stack_ref(SCM stack, SCM index)
{
	struct scm* y = struct_ref_(stack, 3);
	assert(y->type == TVECTOR);
	assert(index < y->length);
	struct scm* e = &g_cells[y->vector + index];

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

struct scm* xassq(SCM x, SCM a)  ///for speed in core only
{
	struct scm* a2 = Getstructscm2(a, g_cells);
	while(GetSCM2(a2, g_cells) != cell_nil && x != bad2good(a2->car, g_cells)->rdc)
	{
		a2 = bad2good(a2->cdr, g_cells);
	}

	return GetSCM2(a2, g_cells) != cell_nil ? a2->car : good2bad(Getstructscm2(cell_f, g_cells), g_cells);
}

struct scm* memq(SCM x, SCM a)
{
	struct scm* y = Getstructscm2(x, g_cells);
	struct scm* a2 = Getstructscm2(a, g_cells);
	int t = y->type;

	if(t == TCHAR || t == TNUMBER)
	{
		SCM v = y->value;

		while(GetSCM2(a2, g_cells) != cell_nil && v != bad2good(a2->car, g_cells)->value)
		{
			a2 = bad2good(a2->cdr, g_cells);
		}
	}
	else if(t == TKEYWORD)
	{
		while(GetSCM2(a2, g_cells) != cell_nil && (bad2good(a2->car, g_cells)->type != TKEYWORD || GetSCM2(string_equal_p(x, a2->rac), g_cells) == cell_f))
		{
			a2 = bad2good(a2->cdr, g_cells);
		}
	}
	else
	{
		while(GetSCM2(a2, g_cells) != cell_nil && x != a2->rac)
		{
			a2 = bad2good(a2->cdr, g_cells);
		}
	}

	return GetSCM2(a2, g_cells) != cell_nil ? good2bad(a2, g_cells) : good2bad(Getstructscm2(cell_f, g_cells), g_cells);
}

struct scm* equal2_p(SCM a, SCM b)
{
	struct scm* a2 = Getstructscm2(a, g_cells);
	struct scm* b2 = Getstructscm2(b, g_cells);
	struct scm* tee = good2bad(Getstructscm2(cell_t, g_cells), g_cells);

	if(a == b)
	{
		return tee;
	}

	if(a2->type == TPAIR && b2->type == TPAIR)
	{
		if((tee == equal2_p(a2->rac, b2->rac)) && (tee == equal2_p(a2->rdc, b2->rdc))) return tee;
		return good2bad(Getstructscm2(cell_f, g_cells), g_cells);
	}

	if(a2->type == TSTRING && b2->type == TSTRING)
	{
		return good2bad(string_equal_p(a, b), g_cells);
	}

	if(a2->type == TVECTOR && b2->type == TVECTOR)
	{
		return vector_equal_p(a, b);
	}

	return eq_p(a, b);
}

struct scm* last_pair(SCM x)
{
	struct scm* y = Getstructscm2(x, g_cells);
	while(GetSCM2(y, g_cells) != cell_nil && y->rdc != cell_nil)
	{
		y = bad2good(y->cdr, g_cells);
	}

	return good2bad(y, g_cells);
}

struct scm* pair_p(SCM x)
{
	struct scm* y = Getstructscm2(x, g_cells);
	return y->type == TPAIR ? good2bad(Getstructscm2(cell_t, g_cells), g_cells) : good2bad(Getstructscm2(cell_f, g_cells), g_cells);
}
