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

SCM length__(struct scm* x);
struct scm* make_number_(SCM n);
struct scm* make_char(SCM c);
struct scm* vector_entry(struct scm* x);
void require(int bool, char* error);

struct scm* struct_length(struct scm* x) /* External*/
{
	require(x->car->type == TSTRUCT, "mes_struct.c: struct_length x was not of type TSTRUCT\n");
	return make_number_(x->car->length);
}

struct scm* struct_ref_(struct scm* x, SCM i)
{
	require(x->type == TSTRUCT, "mes_struct.c: struct_ref_ x was not of type TSTRUCT\n");
	require(i < x->length, "mes_struct.c: struct_ref_ i was not less than x->length\n");
	struct scm* f = x->cdr + i;

	if(f->type == TREF)
	{
		return f->car;
	}

	if(f->type == TCHAR)
	{
		return make_char(f->value);
	}

	if(f->type == TNUMBER)
	{
		return make_number_(f->value);
	}

	return f;
}

struct scm* struct_set_x_(struct scm* x, SCM i, struct scm* e)
{
	require(x->type == TSTRUCT, "mes_struct.c: struct_set_x_ x was not of type TSTRUCT\n");
	require(i < x->length, "mes_struct.c: struct_set_x_ i was not less than x->length\n");
	struct scm* v = vector_entry(e);
	struct scm* y = x->cdr + i;

	y->type = v->type;
	y->car = v->car;
	y->cdr = v->cdr;
	return cell_unspecified;
}

struct scm* struct_ref(struct scm* x) /* External */
{
	struct scm* h = x->cdr->car;
	struct scm* y = x->car;
	return struct_ref_(y, h->value);
}

struct scm* struct_set_x(struct scm* x) /* External */
{
	struct scm* i = x->cdr->car;
	struct scm* e = x->cdr->cdr->car;
	return struct_set_x_(x->car, i->value, e);
}
