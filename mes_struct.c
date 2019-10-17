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

long length__(struct scm* x);
struct scm* make_number(SCM n);
struct scm* make_char(SCM c);
struct scm* vector_entry(struct scm* x);
struct scm* make_struct(struct scm* type, struct scm* fields, struct scm* printer);

struct scm* struct_length(struct scm* x)
{
	assert(x->type == TSTRUCT);
	return make_number(x->length);
}

struct scm* struct_ref_(struct scm* x, long i)
{
	assert(x->type == TSTRUCT);
	assert(i < x->length);
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
		return make_number(f->value);
	}

	return f;
}

struct scm* struct_set_x_(struct scm* x, long i, struct scm* e)
{
	assert(x->type == TSTRUCT);
	assert(i < x->length);
	struct scm* v = vector_entry(e);
	struct scm* y = x->cdr + i;
	/* The below is likely going to be a problem for M2-Planet until we add pointer dereferencing */
	*y = *v;
	return cell_unspecified;
}

struct scm* struct_ref(struct scm* x, struct scm* i) /* External */
{
	struct scm* h = i;
	struct scm* y = x;
	return struct_ref_(y, h->value);
}

struct scm* struct_set_x(struct scm* x, struct scm* i, struct scm* e)
{
	struct scm* h = i;
	struct scm* y = x;
	return struct_set_x_(y, h->value, e);
}
