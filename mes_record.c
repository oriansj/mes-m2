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
struct cell* make_record(struct cell* type, struct cell* vector);
struct cell* make_record_type(char* name, struct cell* list);
struct cell* make_string(char* a);
struct cell* make_vector(int count, struct cell* init);

int record_field_index(struct cell* record, char* name)
{
	require(RECORD_TYPE == record->type, "mes_record.c: record_field_index did not receive a record-type\n");
	struct cell* i = record->cdr->cdr;
	int count = 0;
	while(nil != i)
	{
		if(match(i->car->string, name)) return count;
		count = count + 1;
		i = i->cdr;
	}
	require(FALSE, "mes_record.c: record_field_index did not find field with matching name\n");
	exit(EXIT_FAILURE);
}

struct cell* record_ref(struct cell* type, char* name, struct cell* record)
{
	int i = record_field_index(type, name);
	struct cell* e = record->cdr->cdr;

	while(0 < i)
	{
		e = e->cdr;
		i = i - 1;
	}

	return e->car;
}

struct cell* record_set(struct cell* type, char* name, struct cell* record, struct cell* value)
{
	int i = record_field_index(type, name);
	struct cell* e = record->cdr->cdr;

	while(0 < i)
	{
		e = e->cdr;
		i = i - 1;
	}
	e->car = value;
	return value;
}

struct cell* record_construct(struct cell* type, struct cell* list_args, struct cell* list_vals)
{
	struct cell* e = make_record(type, make_vector(type->cdr->value, cell_f));

	while(nil != list_args)
	{
		require(SYM == list_args->car->type, "mes_record.c: record_construct was not passed symbols\n");
		require(nil != list_vals, "mes_record.c: record_construct received insufficient values\n");
		record_set(type, list_args->car->string, e, list_vals->car);
		list_args = list_args->cdr;
		list_vals = list_vals->cdr;
	}

	return e;
}


/* Exposed primitives */
struct cell* builtin_make_record_type(struct cell* args)
{
	require(nil != args, "make-record-type requires arguments\n");
	require(nil != args->cdr, "make-record-type received insufficient arguments\n");
	require(STRING == args->car->type, "make-record-type did not receive a string\n");
	require(CONS == args->cdr->car->type, "make-record-type did not receive a list\n");
	return make_record_type(args->car->string, args->cdr->car);
}

struct cell* builtin_make_record(struct cell* args)
{
	require(nil != args, "make-record requires arguments\n");
	require(nil != args->cdr, "make-record received insufficient arguments\n");
	require(RECORD_TYPE == args->car->type, "make-record did not receive a string\n");
	require(VECTOR == args->cdr->car->type, "make-record did not receive a vector\n");
	return make_record(args->car, args->cdr->car);
}

struct cell* builtin_record_type_name(struct cell* args)
{
	require(nil != args, "record-type-name requires an argument\n");
	require(nil == args->cdr, "record-type-name received too many arguments\n");
	require(RECORD_TYPE == args->car->type, "record-type-name did not receive a record-type\n");
	return make_string(args->car->string);
}

struct cell* builtin_record_type_fields(struct cell* args)
{
	require(nil != args, "record-type-fields requires an argument\n");
	require(nil == args->cdr, "record-type-fields received too many arguments\n");
	require(RECORD_TYPE == args->car->type, "record-type-fields did not receive a record-type\n");
	return args->car->cdr->cdr;
}

struct cell* builtin_record_typep(struct cell* args)
{
	require(nil != args, "record-type? requires an argument\n");
	require(nil == args->cdr, "record-type? received too many arguments\n");
	if(RECORD_TYPE == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_recordp(struct cell* args)
{
	require(nil != args, "record? requires an argument\n");
	require(nil == args->cdr, "record? received too many arguments\n");
	if(RECORD == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_record_type_descriptor(struct cell* args)
{
	require(nil != args, "record-type-descriptor requires an argument\n");
	require(nil == args->cdr, "record-type-descriptor received too many arguments\n");
	require(RECORD == args->car->type, "record-type-descriptor did not receive a record\n");
	return args->car->car;
}

struct cell* builtin_record_predicate(struct cell* args)
{
	require(nil != args, "core:record-predicate requires an argument\n");
	require(nil != args->cdr, "core:record-predicate received insufficient arguments\n");
	if(RECORD_TYPE == args->car->type)
	{
		if(RECORD == args->cdr->car->type)
		{
			if(args->cdr->car->car == args->car) return cell_t;
		}
	}
	return cell_f;
}

struct cell* builtin_record_accessor(struct cell* args)
{
	require(nil != args, "core:record-accessor requires arguments\n");
	require(nil != args->cdr, "core:record-accessor requires more arguments\n");
	require(nil != args->cdr->cdr, "core:record-accessor requires more arguments\n");
	require(RECORD_TYPE == args->car->type, "core:record-accessor did not receive RECORD-TYPE\n");
	require(SYM == args->cdr->car->type, "core:record-accessor did not receive SYMBOL\n");
	require(RECORD == args->cdr->cdr->car->type, "core:record-accessor did not receive RECORD\n");
	require(args->cdr->cdr->car->car == args->car, "core:record-accessor got a record of a type different than record-type\n");
	return record_ref(args->car, args->cdr->car->string, args->cdr->cdr->car);
}

struct cell* builtin_record_modifier(struct cell* args)
{
	require(nil != args, "core:record-modifier requires arguments\n");
	require(nil != args->cdr, "core:record-modifier requires more arguments\n");
	require(nil != args->cdr->cdr, "core:record-modifier requires more arguments\n");
	require(nil != args->cdr->cdr->cdr, "core:record-modifier requires more arguments\n");
	require(RECORD_TYPE == args->car->type, "core:record-modifier did not receive RECORD-TYPE\n");
	require(SYM == args->cdr->car->type, "core:record-modifier did not receive SYMBOL\n");
	require(RECORD == args->cdr->cdr->car->type, "core:record-modifier did not receive RECORD\n");
	require(args->cdr->cdr->car->car == args->car, "core:record-modifier got a record of a type different than record-type\n");
	return record_set(args->car, args->cdr->car->string, args->cdr->cdr->car, args->cdr->cdr->cdr->car);
}

struct cell* builtin_record_constructor(struct cell* args)
{
	require(nil != args, "core:record-constructor requires arguments\n");
	require(nil != args->cdr, "core:record-constructor requires more arguments\n");
	require(nil != args->cdr->cdr, "core:record-constructor requires more arguments\n");
	require(RECORD_TYPE == args->car->type, "core:record-constructor did not receive RECORD-TYPE\n");
	require(CONS == args->cdr->car->type, "core:record-constructor did not receive argument list\n");
	require(CONS == args->cdr->cdr->car->type, "core:record-constructor did not receive argument list\n");
	return record_construct(args->car, args->cdr->car, args->cdr->cdr->car);
}
