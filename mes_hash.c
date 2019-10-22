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

struct scm* vector_ref_(struct scm* x, SCM i);
struct scm* struct_ref_(struct scm* x, SCM i);

struct scm* make_struct_(struct scm* type, struct scm* fields, struct scm* printer);
struct scm* make_string_(char* s);
struct scm* make_vector__(SCM k);

void vector_set_x_(struct scm* x, SCM i, struct scm* e);
struct scm* error_(struct scm* key, struct scm* x);
struct scm* assq_(struct scm* x, struct scm* a);
struct scm* assoc_(struct scm* x, struct scm* a);
struct scm* acons_(struct scm* key, struct scm* value, struct scm* alist);
struct scm* make_tpair(struct scm* a, struct scm* b);

void require(int bool, char* error);
struct scm* make_number_(SCM n);

SCM hash_cstring(char* s, SCM size)
{
	int h = s[0] * 37;

	if(s[0] && s[1])
	{
		h = h + s[1] * 43;
	}

	require(0 != size, "mes_hash.c: hash_cstring must not be zero");
	h = h % size;
	return h;
}

SCM hashq_(struct scm* x, SCM size) /* Internal */
{
	struct scm* y = x;
	if(y->type == TSPECIAL || y->type == TSYMBOL)
	{
		char* p = y->cdr->string;
		return hash_cstring(p, size);    /* FIXME: hash x directly */
	}

	error_(cell_symbol_system_error, make_tpair(make_string_("hashq_: not a symbol"), x));
	exit(EXIT_FAILURE);
}

SCM hash_(struct scm* x, SCM size) /* Internal */
{
	struct scm* y = x;
	if(y->type == TSTRING)
	{
		char* p = y->cdr->string;
		return hash_cstring(p, size);
	}

	require(FALSE, "mes_hash.c: hash_ impossible condition hit");
	return hashq_(x, size);
}

struct scm* hashq(struct scm* x) /* external */
{
	require(FALSE, "mes_hash.c: hashq impossible condition hit");
	return make_number_(hashq_(x->car, x->cdr->car->value));
}

struct scm* hash(struct scm* x) /* External */
{
	require(FALSE, "mes_hash.c: hash impossible condition hit");
	return make_number_(hash_(x->car, x->cdr->car->value));
}

struct scm* hashq_get_handle_(struct scm* table, struct scm* key, struct scm* dflt) /* Internal */
{
	struct scm* ydflt = dflt;
	if(ydflt->type == TPAIR)
	{
		return ydflt->car;
	}

	struct scm* ybucket = vector_ref_(struct_ref_(table, 4), hashq_(key, struct_ref_(table, 3)->value));
	if(ybucket->type == TPAIR)
	{
		return assq_(key, ybucket);
	}

	return cell_f;
}

struct scm* hashq_get_handle(struct scm* x) /* External */
{
	return hashq_get_handle_(x->car, x->cdr->car, x->cdr->cdr->car);
}

struct scm* hashq_ref_(struct scm* table, struct scm* key, struct scm* dflt) /* Internal */
{
	struct scm* x = hashq_get_handle_(table, key, dflt);

	if(x == cell_f)
	{
		return x;
	}

	return x->cdr;
}

struct scm* hashq_ref(struct scm* x) /* External*/
{
	return hashq_ref_(x->car, x->cdr->car, x->cdr->cdr->car);
}

struct scm* hash_ref_(struct scm* table, struct scm* key) /* Internal */
{
	struct scm* bucket = vector_ref_(struct_ref_(table, 4), hash_(key, struct_ref_(table, 3)->value));
	if(bucket->type == TPAIR)
	{
		struct scm* y = assoc_(key, bucket);
		if(y != cell_f)
		{
			return y->cdr;
		}
	}

	return cell_f;
}

struct scm* hashq_set_x_(struct scm* table, struct scm* key, struct scm* value) /* Internal */
{
	SCM size = struct_ref_(table, 3)->value;
	struct scm* buckets = struct_ref_(table, 4);

	struct scm* ybucket = vector_ref_(buckets, hashq_(key, size));
	if(ybucket->type != TPAIR)
	{
		vector_set_x_(buckets, hashq_(key, size), acons_(key, value, cell_nil));
	}
	else
	{
		vector_set_x_(buckets, hashq_(key, size), acons_(key, value, vector_ref_(buckets, hashq_(key, size))));
	}
	return value;
}

struct scm* hash_set_x_(struct scm* table, struct scm* key, struct scm* value)
{
	SCM size = struct_ref_(table, 3)->value;
	unsigned h = hash_(key, size);
	struct scm* buckets = struct_ref_(table, 4);
	struct scm* bucket = vector_ref_(buckets, h);

	struct scm* ybucket = bucket;
	if(ybucket->type != TPAIR)
	{
		bucket = cell_nil;
	}

	bucket = acons_(key, value, bucket);
	vector_set_x_(buckets, h, bucket);
	return value;
}

struct scm* hash_set_x(struct scm* x)
{
	return hash_set_x_(x->car, x->cdr->car, x->cdr->cdr->car);
}

struct scm* make_hashq_type()  /* ((internal)) */
{
	struct scm* record_type = cell_symbol_record_type; /* FIXME */
	struct scm* fields = cell_nil;
	fields = make_tpair(cell_symbol_buckets, fields);
	fields = make_tpair(cell_symbol_size, fields);
	fields = make_tpair(fields, cell_nil);
	fields = make_tpair(cell_symbol_hashq_table, fields);
	return make_struct_(record_type, fields, cell_unspecified);
}

struct scm* make_hash_table_(SCM size) /* Internal */
{
	if(!size)
	{
		size = 100;
	}

	struct scm* hashq_type = make_hashq_type();
	struct scm* buckets = make_vector__(size);
	struct scm* values = cell_nil;
	values = make_tpair(buckets, values);
	values = make_tpair(make_number_(size), values);
	values = make_tpair(cell_symbol_hashq_table, values);
	/* FIXME: symbol/printer return make_struct_(hashq_type, values, cstring_to_symbol("hash-table-printer"); */
	return make_struct_(hashq_type, values, cell_unspecified);
}

struct scm* make_hash_table(struct scm* x) /* External */
{
	SCM size = 0;

	struct scm* y = x->car;
	if(y->type == TPAIR)
	{
		require(TNUMBER == y->type, "y->type must be TNUMBER\nmes_hash.c: make_hash_table\n");
		size = y->value;
	}

	return make_hash_table_(size);
}


struct scm* hashq_set_x(struct scm* table, struct scm* key, struct scm* value) /* External */
{
	return hashq_set_x_(table, key, value);
}

struct scm* hash_ref(struct scm* x) /* External */
{
	return hash_ref_(x->car, x->cdr->car);
}
