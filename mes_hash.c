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

struct scm* vector_ref_(struct scm* x, long i);
struct scm* struct_ref_(struct scm* x, long i);

struct scm* make_struct (struct scm* type, struct scm* fields, struct scm* printer);
struct scm* make_string_(char const* s);
struct scm* make_vector__(long k);

void vector_set_x_(struct scm* x, long i, struct scm* e);
struct scm* error(struct scm* key, struct scm* x);
struct scm* cons_(struct scm* x, struct scm* y);
struct scm* assq(struct scm* x, struct scm* a);
struct scm* assoc(struct scm* x, struct scm* a);
struct scm* acons_(struct scm* key, struct scm* value, struct scm* alist);

struct scm* make_number(SCM n);

SCM hash_cstring(char const* s, long size)
{
	int h = s[0] * 37;

	if(s[0] && s[1])
	{
		h = h + s[1] * 43;
	}

	assert(size);
	h = h % size;
	return h;
}

SCM hashq_(struct scm* x, long size)
{
	struct scm* y = x;
	if(y->type == TSPECIAL || y->type == TSYMBOL)
	{
		return hash_cstring((char*)&y->cdr->rdc, size);    // FIXME: hash x directly
	}

	error(cell_symbol_system_error, cons_(make_string_("hashq_: not a symbol"), x));
	exit(EXIT_FAILURE);
}

SCM hash_(struct scm* x, long size)
{
	struct scm* y = x;
	if(y->type == TSTRING)
	{
		return hash_cstring((char*)&y->cdr->rdc, size);
	}

	assert(0);
	return hashq_(x, size);
}

struct scm* hashq(struct scm* x, struct scm* size)
{
	struct scm* s = size;
	assert(0);
	return make_number(hashq_(x, s->value));
}

struct scm* hash(struct scm* x, struct scm* size)
{
	struct scm* s = size;
	assert(0);
	return make_number(hash_(x, s->value));
}

struct scm* hashq_get_handle(struct scm* table, struct scm* key, struct scm* dflt)
{
	struct scm* ydflt = dflt;
	if(ydflt->type == TPAIR)
	{
		return ydflt->car;
	}

	struct scm* ybucket = vector_ref_(struct_ref_(table, 4), hashq_(key, struct_ref_(table, 3)->value));
	if(ybucket->type == TPAIR)
	{
		return assq(key, ybucket);
	}

	return cell_f;
}

struct scm* hashq_ref(struct scm* table, struct scm* key, struct scm* dflt)
{
	struct scm* x = hashq_get_handle(table, key, dflt);

	if(x == cell_f)
	{
		return x;
	}

	return x->cdr;
}

struct scm* hash_ref(struct scm* table, struct scm* key, struct scm* dflt) /* External */
{
	struct scm* bucket = vector_ref_(struct_ref_(table, 4), hash_(key, struct_ref_(table, 3)->value));
	if(bucket->type == TPAIR)
	{
		struct scm* y = assoc(key, bucket);
		if(y != cell_f)
		{
			return y->cdr;
		}
	}

	return cell_f;
}

struct scm* hashq_set_x(struct scm* table, struct scm* key, struct scm* value)
{
	long size = struct_ref_(table, 3)->value;
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

struct scm* hash_set_x(struct scm* table, struct scm* key, struct scm* value)
{
	long size = struct_ref_(table, 3)->value;
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

struct scm* make_hashq_type()  ///((internal))
{
	struct scm* record_type = cell_symbol_record_type; // FIXME
	struct scm* fields = cell_nil;
	fields = cons_(cell_symbol_buckets, fields);
	fields = cons_(cell_symbol_size, fields);
	fields = cons_(fields, cell_nil);
	fields = cons_(cell_symbol_hashq_table, fields);
	return make_struct(record_type, fields, cell_unspecified);
}

struct scm* make_hash_table_(long size)
{
	if(!size)
	{
		size = 100;
	}

	struct scm* hashq_type = make_hashq_type();
	struct scm* buckets = make_vector__(size);
	struct scm* values = cell_nil;
	values = cons_(buckets, values);
	values = cons_(make_number(size), values);
	values = cons_(cell_symbol_hashq_table, values);
	//FIXME: symbol/printer return make_struct (hashq_type, values, cstring_to_symbol ("hash-table-printer");
	return make_struct(hashq_type, values, cell_unspecified);
}

struct scm* make_hash_table(struct scm* x)
{
	long size = 0;

	struct scm* y = x;
	if(y->type == TPAIR)
	{
		assert(y->type == TNUMBER);
		size = y->value;
	}

	return make_hash_table_(size);
}


/* Externally exposed */
struct scm* hashq_set_x_(struct scm* table, struct scm* key, struct scm* value)
{
	return hashq_set_x(table, key, value);
}

struct scm* hash_ref_(struct scm* table, struct scm* key, struct scm* dflt)
{
	return hash_ref(table, key, dflt);
}
