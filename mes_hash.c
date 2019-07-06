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

struct scm* vector_ref_(SCM x, long i);
struct scm* struct_ref_(SCM x, long i);

SCM make_cell__(long type, SCM car, SCM cdr);
struct scm* make_struct (SCM type, SCM fields, SCM printer);
struct scm* make_string_(char const* s);
struct scm* make_vector__(long k);

void vector_set_x_(SCM x, long i, SCM e);
SCM error(SCM key, SCM x);
SCM cons_(SCM x, SCM y);
SCM assq(SCM x, SCM a);
SCM assoc(SCM x, SCM a);
SCM acons_(SCM key, SCM value, SCM alist);

int hash_cstring(char const* s, long size)
{
	int hash = s[0] * 37;

	if(s[0] && s[1])
	{
		hash = hash + s[1] * 43;
	}

	assert(size);
	hash = hash % size;
	return hash;
}

int hashq_(SCM x, long size)
{
	struct scm* y = Getstructscm2(x);
	if(y->type == TSPECIAL || y->type == TSYMBOL)
	{
		return hash_cstring((char*)&bad2good(y->cdr)->rdc, size);    // FIXME: hash x directly
	}

	error(cell_symbol_system_error, cons_(GetSCM2(make_string_("hashq_: not a symbol")), x));
	exit(EXIT_FAILURE);
}

int hash_(SCM x, long size)
{
	struct scm* y = Getstructscm2(x);
	if(y->type == TSTRING)
	{
		return hash_cstring((char*)&bad2good(y->cdr)->rdc, size);
	}

	assert(0);
	return hashq_(x, size);
}

struct scm* hashq(SCM x, SCM size)
{
	struct scm* s = Getstructscm2(size);
	assert(0);
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, hashq_(x, s->value))));
}

struct scm* hash(SCM x, SCM size)
{
	struct scm* s = Getstructscm2(size);
	assert(0);
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, hash_(x, s->value))));
}

struct scm* hashq_get_handle(SCM table, SCM key, SCM dflt)
{
	struct scm* ydflt = Getstructscm2(dflt);
	if(ydflt->type == TPAIR)
	{
		return good2bad(bad2good(ydflt->car));
	}

	struct scm* ybucket = vector_ref_(GetSCM2(struct_ref_(table, 4)), hashq_(key, struct_ref_(table, 3)->value));
	if(ybucket->type == TPAIR)
	{
		return good2bad(Getstructscm2(assq(key, GetSCM2(ybucket))));
	}

	return good2bad(Getstructscm2(cell_f));
}

struct scm* hashq_ref(SCM table, SCM key, SCM dflt)
{
	struct scm* x = hashq_get_handle(table, key, dflt);

	if(GetSCM2(x) == cell_f)
	{
		return x;
	}

	return x->cdr;
}

struct scm* hash_ref(SCM table, SCM key, SCM dflt) /* External */
{
	dflt = 0; /* NOP to silence checkers */

	struct scm* bucket = vector_ref_(GetSCM2(struct_ref_(table, 4)), hash_(key, struct_ref_(table, 3)->value));
	if(bucket->type == TPAIR)
	{
		struct scm* y = Getstructscm2(assoc(key, GetSCM2(bucket)));
		if(GetSCM2(y) != cell_f)
		{
			return bad2good(y->cdr);
		}
	}

	return Getstructscm2(cell_f);
}

struct scm* hashq_set_x(SCM table, SCM key, SCM value)
{
	long size = struct_ref_(table, 3)->value;
	SCM buckets = GetSCM2(struct_ref_(table, 4));

	struct scm* ybucket = vector_ref_(buckets, hashq_(key, size));
	if(ybucket->type != TPAIR)
	{
		vector_set_x_(buckets, hashq_(key, size), acons_(key, value, cell_nil));
	}
	else
	{
		vector_set_x_(buckets, hashq_(key, size), acons_(key, value, GetSCM2(vector_ref_(buckets, hashq_(key, size)))));
	}
	return Getstructscm2(value);
}

struct scm* hash_set_x(SCM table, SCM key, SCM value)
{
	long size = struct_ref_(table, 3)->value;
	unsigned hash = hash_(key, size);
	SCM buckets = GetSCM2(struct_ref_(table, 4));
	SCM bucket = GetSCM2(vector_ref_(buckets, hash));

	struct scm* ybucket = Getstructscm2(bucket);
	if(ybucket->type != TPAIR)
	{
		bucket = cell_nil;
	}

	bucket = acons_(key, value, bucket);
	vector_set_x_(buckets, hash, bucket);
	return good2bad(Getstructscm2(value));
}

struct scm* make_hashq_type()  ///((internal))
{
	SCM record_type = cell_symbol_record_type; // FIXME
	SCM fields = cell_nil;
	fields = cons_(cell_symbol_buckets, fields);
	fields = cons_(cell_symbol_size, fields);
	fields = cons_(fields, cell_nil);
	fields = cons_(cell_symbol_hashq_table, fields);
	return good2bad(make_struct(record_type, fields, cell_unspecified));
}

struct scm* make_hash_table_(long size)
{
	if(!size)
	{
		size = 100;
	}

	SCM hashq_type = GetSCM2(bad2good(make_hashq_type()));
	SCM buckets = GetSCM2(make_vector__(size));
	SCM values = cell_nil;
	values = cons_(buckets, values);
	values = cons_(make_cell__ (TNUMBER, 0, size), values);
	values = cons_(cell_symbol_hashq_table, values);
	//FIXME: symbol/printer return make_struct (hashq_type, values, cstring_to_symbol ("hash-table-printer");
	return good2bad(make_struct(hashq_type, values, cell_unspecified));
}

struct scm* make_hash_table(SCM x)
{
	long size = 0;

	struct scm* y = Getstructscm2(x);
	if(y->type == TPAIR)
	{
		assert(y->type == TNUMBER);
		size = y->value;
	}

	return make_hash_table_(size);
}


/* Externally exposed */
struct scm* hashq_set_x_(SCM table, SCM key, SCM value)
{
	return good2bad(hashq_set_x(table, key, value));
}

struct scm* hash_ref_(SCM table, SCM key, SCM dflt)
{
	return good2bad(hash_ref(table, key, dflt));
}
