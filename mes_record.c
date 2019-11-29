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
	return NULL;
}
