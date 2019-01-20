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

#define TYPE(x) g_cells[x].type
#define CBYTES(x) (char*)&g_cells[x].rdc
#define STRING(x) g_cells[x].rdc
#define CSTRING(x) CBYTES (STRING (x))
#define MAKE_STRING0(x) make_string (x, strlen (x))
#define MAKE_NUMBER(n) make_cell__ (TNUMBER, 0, (long)n)
#define VALUE(x) g_cells[x].rdc
#define CAR(x) g_cells[x].rac
#define CDR(x) g_cells[x].rdc
#define LENGTH(x) g_cells[x].rac
#define CAAR(x) CAR (CAR (x))

struct scm* make_vector__(long k);
struct scm* vector_ref_(SCM x, long i);
struct scm* vector_set_x_(SCM x, long i, SCM e);
SCM error(SCM key, SCM x);
SCM cons (SCM x, SCM y);
struct scm* make_string(char const* s, int length);
SCM make_cell__(long type, SCM car, SCM cdr);
struct scm* struct_ref_(SCM x, long i);
SCM assq (SCM x, SCM a);
SCM assoc (SCM x, SCM a);
SCM acons (SCM key, SCM value, SCM alist);
int fdputs (char const* s, int fd);
struct scm* display_ (SCM x);
int fdputc (int c, int fd);
SCM write_ (SCM x);
struct scm* make_struct (SCM type, SCM fields, SCM printer);

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
	if(TYPE(x) == TSPECIAL || TYPE(x) == TSYMBOL)
	{
		return hash_cstring(CSTRING(x), size);    // FIXME: hash x directly
	}

	error(cell_symbol_system_error, cons(GetSCM(MAKE_STRING0("hashq_: not a symbol")), x));
	exit(1);
}

int hash_(SCM x, long size)
{
	if(TYPE(x) == TSTRING)
	{
		return hash_cstring(CSTRING(x), size);
	}

	assert(0);
	return hashq_(x, size);
}

struct scm* hashq(SCM x, SCM size)
{
	assert(0);
	return Getstructscm(MAKE_NUMBER(hashq_(x, VALUE(size))));
}

struct scm* hash(SCM x, SCM size)
{
	assert(0);
	return Getstructscm(MAKE_NUMBER(hash_(x, VALUE(size))));
}

struct scm* hashq_get_handle(SCM table, SCM key, SCM dflt)
{
	long size = VALUE(GetSCM(struct_ref_(table, 3)));
	unsigned hash = hashq_(key, size);
	SCM buckets = GetSCM(struct_ref_(table, 4));
	SCM bucket = GetSCM(vector_ref_(buckets, hash));
	SCM x = cell_f;

	if(TYPE(dflt) == TPAIR)
	{
		x = CAR(dflt);
	}

	if(TYPE(bucket) == TPAIR)
	{
		x = assq(key, bucket);
	}

	return Getstructscm(x);
}

struct scm* hashq_ref(SCM table, SCM key, SCM dflt)
{
#if defined (INLINE)
	struct scm* x = hashq_get_handle(table, key, dflt);
#else
	long size = VALUE(GetSCM(struct_ref_(table, 3)));
	unsigned hash = hashq_(key, size);
	SCM buckets = GetSCM(struct_ref_(table, 4));
	SCM bucket = GetSCM(vector_ref_(buckets, hash));
	SCM x = cell_f;

	if(TYPE(dflt) == TPAIR)
	{
		x = CAR(dflt);
	}

	if(TYPE(bucket) == TPAIR)
	{
		x = assq(key, bucket);
	}

#endif

	if(x != cell_f)
	{
		x = CDR(x);
	}

	return Getstructscm(x);
}

struct scm* hash_ref(SCM table, SCM key, SCM dflt)
{
	long size = VALUE(GetSCM(struct_ref_(table, 3)));
	unsigned hash = hash_(key, size);
	SCM buckets = GetSCM(struct_ref_(table, 4));
	SCM bucket = GetSCM(vector_ref_(buckets, hash));
	SCM x = cell_f;

	if(TYPE(dflt) == TPAIR)
	{
		x = CAR(dflt);
	}

	if(TYPE(bucket) == TPAIR)
	{
		x = assoc(key, bucket);

		if(x != cell_f)
		{
			x = CDR(x);
		}
	}

	return Getstructscm(x);
}

#if defined (INLINE)
#error INLINE
struct scm* hash_set_x_(SCM table, unsigned hash, SCM key, SCM value)
{
	SCM buckets = struct_ref_(table, 4);
	SCM bucket = vector_ref_(buckets, hash);

	if(TYPE(bucket) != TPAIR)
	{
		bucket = cell_nil;
	}

	bucket = acons(key, value, bucket);
	vector_set_x_(buckets, hash, bucket);
	return Getstructscm(value);
}
#endif

struct scm* hashq_set_x(SCM table, SCM key, SCM value)
{
	long size = VALUE(GetSCM(struct_ref_(table, 3)));
	unsigned hash = hashq_(key, size);
#if defined (INLINE)
	return hash_set_x_(table, hash, key, value);
#else
	SCM buckets = GetSCM(struct_ref_(table, 4));
	SCM bucket = GetSCM(vector_ref_(buckets, hash));

	if(TYPE(bucket) != TPAIR)
	{
		bucket = cell_nil;
	}

	bucket = acons(key, value, bucket);
	vector_set_x_(buckets, hash, bucket);
	return Getstructscm(value);
#endif
}

struct scm* hash_set_x(SCM table, SCM key, SCM value)
{
	long size = VALUE(GetSCM(struct_ref_(table, 3)));
	unsigned hash = hash_(key, size);
#if defined (INLINE)
	return hash_set_x_(table, hash, key, value);
#else
	SCM buckets = GetSCM(struct_ref_(table, 4));
	SCM bucket = GetSCM(vector_ref_(buckets, hash));

	if(TYPE(bucket) != TPAIR)
	{
		bucket = cell_nil;
	}

	bucket = acons(key, value, bucket);
	vector_set_x_(buckets, hash, bucket);
	return Getstructscm(value);
#endif
}

struct scm* hash_table_printer(SCM table)
{
	fdputs("#<", __stdout);
	display_(GetSCM(struct_ref_(table, 2)));
	fdputc(' ', __stdout);
	fdputs("size: ", __stdout);
	display_(GetSCM(struct_ref_(table, 3)));
	fdputc(' ', __stdout);
	SCM buckets = GetSCM(struct_ref_(table, 4));
	fdputs("buckets: ", __stdout);

	for(int i = 0; i < LENGTH(buckets); i++)
	{
		SCM e = GetSCM(vector_ref_(buckets, i));

		if(e != cell_unspecified)
		{
			fdputc('[', __stdout);

			while(TYPE(e) == TPAIR)
			{
				write_(CAAR(e));
				e = CDR(e);

				if(TYPE(e) == TPAIR)
				{
					fdputc(' ', __stdout);
				}
			}

			fdputs("]\n  ", __stdout);
		}
	}

	fdputc('>', __stdout);
	return Getstructscm(cell_unspecified);
}

struct scm* make_hashq_type()  ///((internal))
{
	SCM record_type = cell_symbol_record_type; // FIXME
	SCM fields = cell_nil;
	fields = cons(cell_symbol_buckets, fields);
	fields = cons(cell_symbol_size, fields);
	fields = cons(fields, cell_nil);
	fields = cons(cell_symbol_hashq_table, fields);
	return make_struct(record_type, fields, cell_unspecified);
}

struct scm* make_hash_table_(long size)
{
	if(!size)
	{
		size = 100;
	}

	SCM hashq_type = GetSCM(make_hashq_type());
	SCM buckets = GetSCM(make_vector__(size));
	SCM values = cell_nil;
	values = cons(buckets, values);
	values = cons(MAKE_NUMBER(size), values);
	values = cons(cell_symbol_hashq_table, values);
	//FIXME: symbol/printer return make_struct (hashq_type, values, cstring_to_symbol ("hash-table-printer");
	return make_struct(hashq_type, values, cell_unspecified);
}

struct scm* make_hash_table(SCM x)
{
	long size = 0;

	if(TYPE(x) == TPAIR)
	{
		assert(TYPE(x) == TNUMBER);
		size = VALUE(x);
	}

	return make_hash_table_(size);
}
