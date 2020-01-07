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
struct cell* make_int(int a);
struct cell* make_vector(int count, struct cell* init);
struct cell* equal(struct cell* a, struct cell* b);

struct cell* vector_to_list(struct cell* a)
{
	require(VECTOR == a->type, "mes_vector.c: vector_to_list received non-vector\n");
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
	return cell_unspecified;
}

struct cell* list_to_vector(struct cell* i)
{
	if(nil == i) return make_vector(0, cell_unspecified);

	require(CONS == i->type, "mes_vector.c: list_to_vector did not recieve a list\n");

	struct cell* r = make_vector(0, cell_unspecified);
	r->cdr = i;
	int count = 1;
	while(nil != i->cdr)
	{
		require(CONS == i->type, "mes_vector.c: list_to_vector did not recieve a true list\n");
		i = i->cdr;
		count = count + 1;
	}
	r->value = count;
	return r;
}

struct cell* vector_equal(struct cell* a, struct cell* b)
{
	require(VECTOR == a->type, "mes_vector.c: vector_equal received non-vector\n");
	require(VECTOR == b->type, "mes_vector.c: vector_equal received non-vector\n");
	if(a->value != b->value) return cell_f;
	if(0 == a->value) return cell_t;

	a = a->cdr;
	b = b->cdr;
	while(nil != a)
	{
		if(cell_t != equal(a->car, b->car)) return cell_f;

		a = a->cdr;
		b = b->cdr;
	}

	return cell_t;
}


/* Exposed primitives */
struct cell* builtin_vectoreq(struct cell* args)
{
	require(nil != args, "vector=? requires arguments\n");

	require(VECTOR == args->car->type, "vector=? received non-vector\n");
	struct cell* temp = args->car;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		require(VECTOR == args->car->type, "vector=? received non-vector\n");
		if(cell_t != vector_equal(temp, args->car))
		{
			return cell_f;
		}
	}
	return cell_t;
}

struct cell* builtin_make_vector(struct cell* args)
{
	require(nil != args, "make-vector requires arguments\n");
	require(INT == args->car->type, "make-vector requires a numerical argument\n");
	require(0 <= args->car->value, "make-vector requires a number >= 0\n");
	if(nil == args->cdr) return make_vector(args->car->value, cell_unspecified);

	require(nil == args->cdr->cdr, "make-vector recieved too many arguments\n");
	return make_vector(args->car->value, args->cdr->car);
}

struct cell* builtin_vector_length(struct cell* args)
{
	require(nil != args, "vector-length requires an argument\n");
	return make_int(args->car->value);
}

struct cell* builtin_vector_ref(struct cell* args)
{
	require(nil != args, "vector-ref requires an argument\n");
	require(nil != args->cdr, "vector-ref requires an argument\n");
	require(nil == args->cdr->cdr, "vector-ref received too many arguments\n");
	require(VECTOR == args->car->type, "vector-ref did not receive vector\n");
	require(INT == args->cdr->car->type, "vector-ref did not receive index\n");
	return vector_ref(args->car, args->cdr->car->value);
}

struct cell* builtin_vector_set(struct cell* args)
{
	require(nil != args, "vector-set! requires an argument\n");
	require(nil != args->cdr, "vector-set! requires a second argument\n");
	require(nil != args->cdr->cdr, "vector-set! requires a third argument\n");
	require(nil == args->cdr->cdr->cdr, "vector-set! recieved too many argument\n");
	require(VECTOR == args->car->type, "vector-set! did not receive a vector\n");
	require(INT == args->cdr->car->type, "vector-set! did not receive an index\n");
	return vector_set(args->car, args->cdr->car->value, args->cdr->cdr->car);
}

struct cell* builtin_vector_to_list(struct cell* args)
{
	require(nil != args, "vector->list! requires an argument\n");
	require(VECTOR == args->car->type, "vector->list! ");
	require(nil == args->cdr, "vector-set! too many arguments\n");
	return vector_to_list(args->car);
}

struct cell* builtin_list_to_vector(struct cell* args)
{
	require(nil != args, "list->vector requires an argument\n");
	require(nil == args->cdr, "list->vector only allows a single argument\n");
	return list_to_vector(args->car);
}

struct cell* builtin_vectorp(struct cell* args)
{
	require(nil != args, "vector? requires arguments\n");
	require(nil == args->cdr, "vector? recieved too many arguments\n");
	if(VECTOR == args->car->type) return cell_t;
	return cell_f;
}
