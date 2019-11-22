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
	require(VECTOR == a->type, "mes_vector.c: vector_to_list received non-vector\n");

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
	require(VECTOR == a->type, "mes_vector.c: vector_ref received non-vector\n");
	require(i >= 0, "mes_vector.c: vector_ref received negative index\n");
	require(i < a->value, "mes_vector.c: vector_ref received index past end of vector\n");
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
	require(VECTOR == v->type, "mes_vector.c: vector_set received non-vector\n");
	require(i >= 0, "mes_vector.c: vector_set received negative index\n");
	require(i < v->value, "mes_vector.c: vector_set received index past end of vector\n");

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
	require(CONS == i->type, "mes_vector.c: list_to_vector did not recieve a list\n");

	struct cell* r = make_vector(0);
	r->cdr = i;
	int count = 1;
	while(nil != i->cdr)
	{
		require(CONS == i->type, "mes_vector.c: list_to_vector did not recieve a true list\n");
		i = i->cdr;
		count = count + 1;
	}
	i->cdr = NULL;
	r->value = count;
	return r;
}
