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

/* Imported functions */
struct cell* make_vector(int count);

struct cell* vector_to_list(struct cell* a)
{
	struct cell* i = a->cdr;
	while(NULL != i->cdr)
	{
		i = i->cdr;
	}
	i->cdr = nil;
	return a->cdr;
}

struct cell* vector_ref(struct cell* a, int i)
{
	struct cell* e = a->cdr;

	while(0 < i)
	{
		e = e->cdr;
		i = i - 1;
	}

	return e->car;
}

struct cell* vector_set(struct cell* v, int i, struct cell* e)
{
	v = v->cdr;
	while(i > 0)
	{
		v = v->cdr;
		i = i - 1;
	}
	v->car = e;
	return NULL;
}

struct cell* list_to_vector(struct cell* i)
{
	struct cell* r = make_vector(0);
	r->cdr = i;
	int count = 1;
	while(nil != i->cdr)
	{
		i = i->cdr;
		count = count + 1;
	}
	i->cdr = NULL;
	r->value = count;
	return r;
}
