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
void raw_print(char* s, FILE* f);

void writeobj(FILE *output_file, struct cell* op, int write_p)
{
	if(NULL == op) return;

	if(INT == op->type)
	{
		file_print(numerate_number(op->value), output_file);
	}
	else if(CONS == op->type)
	{
		fputc('(', output_file);
		do
		{
			writeobj(output_file, op->car, write_p);
			if(nil == op->cdr)
			{
				fputc(')', output_file);
				break;
			}
			op = op->cdr;
			if(op->type != CONS)
			{
				file_print(" . ", output_file);
				writeobj(output_file, op, write_p);
				fputc(')', output_file);
				break;
			}
			fputc(' ', output_file);
		} while(TRUE);
	}
	else if(SYM == op->type)
	{
		file_print(op->string, output_file);
	}
	else if(PRIMOP == op->type)
	{
		file_print("#<PRIMOP>", output_file);
	}
	else if(PROC == op->type)
	{
		file_print("#<PROC>", output_file);
	}
	else if(CHAR == op->type)
	{
		fputc(op->value, output_file);
	}
	else if(STRING == op->type)
	{
		if(write_p) fputc('"', output_file);
		if(write_p) raw_print(op->string, output_file);
		else file_print(op->string, output_file);
		if(write_p) fputc('"', output_file);
	}
	else if(VECTOR == op->type)
	{
		file_print("#(", output_file);

		writeobj(output_file, op->cdr->car, write_p);
		int i;
		struct cell* z = op->cdr->cdr;
		for(i = 1; i < op->value; i = i + 1)
		{
			file_print(" ", output_file);
			writeobj(output_file, z->car, write_p);
			z = z->cdr;
		}

		fputc(')',output_file);
	}
	else if(FILE_PORT == op->type)
	{
		file_print("#<input>", output_file);
	}
	else if(EOF_object == op->type)
	{
		file_print("#<eof>", output_file);
	}
	else
	{
		file_print("Type ", stderr);
		file_print(numerate_number(op->type), stderr);
		file_print(" is unknown\nPrint aborting hard\n", stderr);
		exit(EXIT_FAILURE);
	}
}

struct cell* prim_write(struct cell* args, FILE* out)
{
	writeobj(out, args->car, TRUE);
	return NULL;
}

struct cell* prim_display(struct cell* args, FILE* out)
{
	writeobj(out, args->car, FALSE);
	return NULL;
}
