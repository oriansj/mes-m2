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
#define STRING(x) g_cells[x].rdc
#define VALUE(x) g_cells[x].value
#define CAR(x) g_cells[x].rac
#define CDR(x) g_cells[x].rdc
#define LENGTH(x) g_cells[x].length

struct scm* make_vector__(long k);
struct scm* vector_ref_(SCM x, long i);
void vector_set_x_(SCM x, long i, SCM e);
SCM error(SCM key, SCM x);
SCM cons (SCM x, SCM y);
struct scm* make_string_(char const* s);
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
	struct scm* y = Getstructscm2(x, g_cells);
	if(y->type == TSPECIAL || y->type == TSYMBOL)
	{
		return hash_cstring((char*)&bad2good(y->cdr, g_cells)->rdc, size);    // FIXME: hash x directly
	}

	error(cell_symbol_system_error, cons(GetSCM2(make_string_("hashq_: not a symbol"), g_cells), x));
	exit(EXIT_FAILURE);
}

int hash_(SCM x, long size)
{
	struct scm* y = Getstructscm2(x, g_cells);
	if(y->type == TSTRING)
	{
		return hash_cstring((char*)&bad2good(y->cdr, g_cells)->rdc, size);
	}

	assert(0);
	return hashq_(x, size);
}

struct scm* hashq(SCM x, SCM size)
{
	struct scm* s = Getstructscm2(size, g_cells);
	assert(0);
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, hashq_(x, s->value)), g_cells), g_cells);
}

struct scm* hash(SCM x, SCM size)
{
	struct scm* s = Getstructscm2(size, g_cells);
	assert(0);
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, hash_(x, s->value)), g_cells), g_cells);
}

struct scm* hashq_get_handle(SCM table, SCM key, SCM dflt)
{
	struct scm* ydflt = Getstructscm2(dflt, g_cells);
	if(ydflt->type == TPAIR)
	{
		return good2bad(bad2good(ydflt->car, g_cells), g_cells);
	}

	struct scm* ybucket = vector_ref_(GetSCM2(struct_ref_(table, 4), g_cells), hashq_(key, struct_ref_(table, 3)->value));
	if(ybucket->type == TPAIR)
	{
		return good2bad(Getstructscm2(assq(key, GetSCM2(ybucket, g_cells)), g_cells), g_cells);
	}

	return good2bad(Getstructscm2(cell_f, g_cells), g_cells);
}

struct scm* hashq_ref(SCM table, SCM key, SCM dflt)
{
	struct scm* x = hashq_get_handle(table, key, dflt);

	if(GetSCM2(x, g_cells) == cell_f)
	{
		return x;
	}

	return x->cdr;
}

struct scm* hash_ref(SCM table, SCM key, SCM dflt) /* External */
{
	dflt = 0; /* NOP to silence checkers */

	struct scm* bucket = vector_ref_(GetSCM2(struct_ref_(table, 4), g_cells), hash_(key, struct_ref_(table, 3)->value));
	if(bucket->type == TPAIR)
	{
		struct scm* y = Getstructscm2(assoc(key, GetSCM2(bucket, g_cells)), g_cells);
		if(GetSCM2(y, g_cells) != cell_f)
		{
			return y->cdr;
		}
	}

	return good2bad(Getstructscm2(cell_f, g_cells), g_cells);
}

struct scm* hashq_set_x(SCM table, SCM key, SCM value)
{
	long size = struct_ref_(table, 3)->value;
	SCM buckets = GetSCM2(struct_ref_(table, 4), g_cells);

	struct scm* ybucket = vector_ref_(buckets, hashq_(key, size));
	if(ybucket->type != TPAIR)
	{
		vector_set_x_(buckets, hashq_(key, size), acons(key, value, cell_nil));
	}
	else
	{
		vector_set_x_(buckets, hashq_(key, size), acons(key, value, GetSCM2(vector_ref_(buckets, hashq_(key, size)), g_cells)));
	}
	return good2bad(Getstructscm2(value, g_cells), g_cells);
}

struct scm* hash_set_x(SCM table, SCM key, SCM value)
{
	long size = VALUE(GetSCM2(struct_ref_(table, 3), g_cells));
	unsigned hash = hash_(key, size);
	SCM buckets = GetSCM2(struct_ref_(table, 4), g_cells);
	SCM bucket = GetSCM2(vector_ref_(buckets, hash), g_cells);

	struct scm* ybucket = Getstructscm2(bucket, g_cells);
	if(ybucket->type != TPAIR)
	{
		bucket = cell_nil;
	}

	bucket = acons(key, value, bucket);
	vector_set_x_(buckets, hash, bucket);
	return good2bad(Getstructscm2(value, g_cells), g_cells);
}

struct scm* hash_table_printer(struct scm* table)
{
	fdputs("#<", __stdout);
	display_(GetSCM2(struct_ref_(GetSCM2(bad2good(table, g_cells), g_cells), 2), g_cells));
	fdputc(' ', __stdout);
	fdputs("size: ", __stdout);
	display_(GetSCM2(struct_ref_(GetSCM2(bad2good(table, g_cells), g_cells), 3), g_cells));
	fdputc(' ', __stdout);
	SCM buckets = GetSCM2(struct_ref_(GetSCM2(bad2good(table, g_cells), g_cells), 4), g_cells);
	fdputs("buckets: ", __stdout);

	struct scm* ybuckets = Getstructscm2(buckets, g_cells);
	for(int i = 0; i < ybuckets->length; i++)
	{
		struct scm* f = vector_ref_(buckets, i);

		if(f != &table[cell_unspecified])
		{
			fdputc('[', __stdout);

			while(f->type == TPAIR)
			{
				write_(GetSCM2(f->car->car, table));
				f = f->cdr;

				if(f->type == TPAIR)
				{
					fdputc(' ', __stdout);
				}
			}

			fdputs("]\n  ", __stdout);
		}
	}

	fdputc('>', __stdout);
	return good2bad(Getstructscm2(cell_unspecified, g_cells), g_cells);
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

	SCM hashq_type = GetSCM2(bad2good(make_hashq_type(), g_cells), g_cells);
	SCM buckets = GetSCM2(make_vector__(size), g_cells);
	SCM values = cell_nil;
	values = cons(buckets, values);
	values = cons(make_cell__ (TNUMBER, 0, size), values);
	values = cons(cell_symbol_hashq_table, values);
	//FIXME: symbol/printer return make_struct (hashq_type, values, cstring_to_symbol ("hash-table-printer");
	return make_struct(hashq_type, values, cell_unspecified);
}

struct scm* make_hash_table(SCM x)
{
	long size = 0;

	struct scm* y = Getstructscm2(x, g_cells);
	if(y->type == TPAIR)
	{
		assert(y->type == TNUMBER);
		size = y->value;
	}

	return make_hash_table_(size);
}
