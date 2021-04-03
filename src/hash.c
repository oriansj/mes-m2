/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018,2019,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include "mes/lib.h"
#include "mes/mes.h"

#include <string.h>

int
hash_cstring (char const *s, long size)
{
  int hash = s[0] * 37;
  if (s[0] != 0)
    if (s[1] != 0)
      hash = hash + (s[1] * 43);
  assert_msg (size != 0, "size");
  hash = hash % size;
  return hash;
}

int
hashq_ (struct scm *x, long size)
{
  if (x->type == TSPECIAL || x->type == TSYMBOL)
    return hash_cstring (cell_bytes (x->string), size);    /* FIXME: hash x directly. */
  error (cell_symbol_system_error, cons (make_string0 ("hashq_: not a symbol"), x));
}

int
hash_ (struct scm *x, long size)
{
  if (x->type != TSTRING)
    {
      eputs ("hash_ failed, not a string:");
      display_error_ (x);
      assert_msg (0, "0");
    }
  return hash_cstring (cell_bytes (x->string), size);
}

struct scm *
hashq (struct scm *x, struct scm *size)
{
  eputs ("hashq not supporteed\n");
  assert_msg (0, "0");
}

struct scm *
hash (struct scm *x, struct scm *size)
{
  eputs ("hash not supporteed\n");
  assert_msg (0, "0");
}

struct scm *
hashq_get_handle (struct scm *table, struct scm *key, struct scm *dflt)
{
  struct scm *s = struct_ref_ (table, 3);
  long size = s->value;
  unsigned hash = hashq_ (key, size);
  struct scm *buckets = struct_ref_ (table, 4);
  struct scm *bucket = vector_ref_ (buckets, hash);
  struct scm *x = cell_f;
  if (dflt->type == TPAIR)
    x = dflt->car;
  if (bucket->type == TPAIR)
    x = assq (key, bucket);
  return x;
}

struct scm *
hashq_ref (struct scm *table, struct scm *key, struct scm *dflt)
{
  struct scm *x = hashq_get_handle (table, key, dflt);
  if (x != cell_f)
    x = x->cdr;
  return x;
}

struct scm *
hash_ref (struct scm *table, struct scm *key, struct scm *dflt)
{
  struct scm *s = struct_ref_ (table, 3);
  long size = s->value;
  unsigned hash = hash_ (key, size);
  struct scm *buckets = struct_ref_ (table, 4);
  struct scm *bucket = vector_ref_ (buckets, hash);
  struct scm *x = cell_f;
  if (dflt->type == TPAIR)
    x = dflt->car;
  if (bucket->type == TPAIR)
    {
      x = assoc (key, bucket);
      if (x != cell_f)
        x = x->cdr;
    }
  return x;
}

struct scm *
hash_set_x_ (struct scm *table, unsigned hash, struct scm *key, struct scm *value)
{
  struct scm *buckets = struct_ref_ (table, 4);
  struct scm *bucket = vector_ref_ (buckets, hash);
  if (bucket->type != TPAIR)
    bucket = cell_nil;
  bucket = acons (key, value, bucket);
  vector_set_x_ (buckets, hash, bucket);
  return value;
}

struct scm *
hashq_set_x (struct scm *table, struct scm *key, struct scm *value)
{
  struct scm *s = struct_ref_ (table, 3);
  long size = s->value;
  unsigned hash = hashq_ (key, size);
  return hash_set_x_ (table, hash, key, value);
}

struct scm *
hash_set_x (struct scm *table, struct scm *key, struct scm *value)
{
  struct scm *s = struct_ref_ (table, 3);
  long size = s->value;
  unsigned hash = hash_ (key, size);
  return hash_set_x_ (table, hash, key, value);
}

struct scm *
hash_table_printer (struct scm *table)
{
  fdputs ("#<", __stdout);
  display_ (struct_ref_ (table, 2));
  fdputc (' ', __stdout);
  fdputs ("size: ", __stdout);
  display_ (struct_ref_ (table, 3));
  fdputc (' ', __stdout);
  struct scm *buckets = struct_ref_ (table, 4);
  fdputs ("buckets: ", __stdout);
  int i;
  struct scm *e;
  for (i = 0; i < buckets->length; i = i + 1)
    {
      e = vector_ref_ (buckets, i);
      if (e != cell_unspecified)
        {
          fdputc ('[', __stdout);
          while (e->type == TPAIR)
            {
              write_ (e->car->car);
              e = e->cdr;
              if (e->type == TPAIR)
                fdputc (' ', __stdout);
            }
          fdputs ("]\n  ", __stdout);
        }
    }
  fdputc ('>', __stdout);
}

struct scm *
make_hashq_type ()              /*:((internal)) */
{
  struct scm *fields = cell_nil;
  fields = cons (cell_symbol_buckets, fields);
  fields = cons (cell_symbol_size, fields);
  fields = cons (fields, cell_nil);
  fields = cons (cell_symbol_hashq_table, fields);
  return make_struct (cell_symbol_record_type, fields, cell_unspecified);
}

struct scm *
make_hash_table_ (long size)
{
  if (size == 0)
    size = 100;
  struct scm *hashq_type = make_hashq_type ();

  struct scm *buckets = make_vector_ (size, cell_unspecified);
  struct scm *values = cell_nil;
  values = cons (buckets, values);
  values = cons (make_number (size), values);
  values = cons (cell_symbol_hashq_table, values);
  /*FIXME: symbol/printer
    return make_struct (hashq_type, values, cstring_to_symbol ("hash-table-printer");*/
  return make_struct (hashq_type, values, cell_unspecified);
}

struct scm *
make_hash_table (struct scm *x)
{
  long size = 0;
  if (x->type == TPAIR)
    {
      assert_msg (x->type == TNUMBER, "x->type == TNUMBER");
      size = x->value;
    }
  return make_hash_table_ (size);
}
