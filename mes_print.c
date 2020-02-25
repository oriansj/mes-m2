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
char char_lookup(int c);
int string_size(char* a);
void raw_print(char* s, FILE* f);
void ugly_print(char* s, FILE* f, int length);

void writeobj(struct cell* output_file, struct cell* op, int write_p)
{
	if(NULL == op) return;

	if(INT == op->type)
	{
		file_print(numerate_number(op->value), output_file->file);
	}
	else if(CONS == op->type)
	{
		fputc('(', output_file->file);
		do
		{
			writeobj(output_file, op->car, write_p);
			if(nil == op->cdr)
			{
				fputc(')', output_file->file);
				break;
			}
			op = op->cdr;
			if(op->type != CONS)
			{
				file_print(" . ", output_file->file);
				writeobj(output_file, op, write_p);
				fputc(')', output_file->file);
				break;
			}
			fputc(' ', output_file->file);
		} while(TRUE);
	}
	else if(SYM == op->type)
	{
		if(cell_unspecified == op) file_print("#<unspecified>", output_file->file);
		else file_print(op->string, output_file->file);
	}
	else if(KEYWORD == op->type)
	{
		file_print(op->string, output_file->file);
	}
	else if(PRIMOP == op->type)
	{
		file_print("#<primitive>", output_file->file);
	}
	else if(LAMBDA == op->type)
	{
		file_print("#<procedure>", output_file->file);
	}
	else if(CHAR == op->type)
	{
		if(write_p)
		{
			fputc('#', output_file->file);
			fputc('\\', output_file->file);
			if(0 == op->value) file_print("nul", output_file->file);
			else if(1 == op->value) file_print("soh", output_file->file);
			else if(2 == op->value) file_print("stx", output_file->file);
			else if(3 == op->value) file_print("etx", output_file->file);
			else if(4 == op->value) file_print("eot", output_file->file);
			else if(5 == op->value) file_print("enq", output_file->file);
			else if(6 == op->value) file_print("ack", output_file->file);
			else if(7 == op->value) file_print("alarm", output_file->file);
			else if(8 == op->value) file_print("backspace", output_file->file);
			else if(9 == op->value) file_print("tab", output_file->file);
			else if(10 == op->value) file_print("newline", output_file->file);
			else if(11 == op->value) file_print("vtab", output_file->file);
			else if(12 == op->value) file_print("page", output_file->file);
			else if(13 == op->value) file_print("return", output_file->file);
			else if(14 == op->value) file_print("so", output_file->file);
			else if(15 == op->value) file_print("si", output_file->file);
			else if(16 == op->value) file_print("dle", output_file->file);
			else if(17 == op->value) file_print("dc1", output_file->file);
			else if(18 == op->value) file_print("dc2", output_file->file);
			else if(19 == op->value) file_print("dc3", output_file->file);
			else if(20 == op->value) file_print("dc4", output_file->file);
			else if(21 == op->value) file_print("nak", output_file->file);
			else if(22 == op->value) file_print("syn", output_file->file);
			else if(23 == op->value) file_print("etb", output_file->file);
			else if(24 == op->value) file_print("can", output_file->file);
			else if(25 == op->value) file_print("em", output_file->file);
			else if(26 == op->value) file_print("sub", output_file->file);
			else if(27 == op->value) file_print("esc", output_file->file);
			else if(28 == op->value) file_print("fs", output_file->file);
			else if(29 == op->value) file_print("gs", output_file->file);
			else if(30 == op->value) file_print("rs", output_file->file);
			else if(31 == op->value) file_print("us", output_file->file);
			else if(32 == op->value) file_print("space", output_file->file);
			else if(127 == op->value) file_print("delete", output_file->file);
			else fputc(char_lookup(op->value), output_file->file);
		}
		else fputc(op->value, output_file->file);
	}
	else if(STRING == op->type)
	{
		if(write_p) fputc('"', output_file->file);
		if(write_p)
		{
			if(op->length != string_size(op->string)) ugly_print(op->string, output_file->file, op->length);
			else raw_print(op->string, output_file->file);
		}
		else file_print(op->string, output_file->file);
		if(write_p) fputc('"', output_file->file);
	}
	else if(VECTOR == op->type)
	{
		file_print("#(", output_file->file);

		if(0 != op->value)
		{
			writeobj(output_file, op->cdr->car, write_p);
			int i;
			struct cell* z = op->cdr->cdr;
			for(i = 1; i < op->value; i = i + 1)
			{
				file_print(" ", output_file->file);
				writeobj(output_file, z->car, write_p);
				z = z->cdr;
			}
		}

		fputc(')',output_file->file);
	}
	else if(FILE_PORT == op->type)
	{
		file_print("#<port: ", output_file->file);
		file_print(op->string, output_file->file);
		file_print(" >", output_file->file);
	}
	else if(RECORD == op->type)
	{
		file_print("#<", output_file->file);
		file_print(op->car->string, output_file->file);
		struct cell* title = op->car->cdr->cdr;
		struct cell* content = op->cdr->cdr;

		while(nil != title)
		{
			file_print(" ", output_file->file);
			file_print(title->car->string, output_file->file);
			file_print(": ", output_file->file);
			writeobj(output_file, content->car, write_p);
			title = title->cdr;
			content = content->cdr;
		}
		file_print(">", output_file->file);
	}
	else if(RECORD_TYPE == op->type)
	{
		file_print("#<record-type ", output_file->file);
		file_print(op->string, output_file->file);
		file_print(">", output_file->file);
	}
	else if(EOF_object == op->type)
	{
		file_print("#<eof>", output_file->file);
	}
	else
	{
		file_print("Type ", stderr);
		file_print(numerate_number(op->type), stderr);
		file_print(" is unknown\nPrint aborting hard\n", stderr);
		exit(EXIT_FAILURE);
	}
}

struct cell* prim_write(struct cell* args, struct cell* out)
{
	writeobj(out, args->car, TRUE);
	return NULL;
}

struct cell* prim_display(struct cell* args, struct cell* out)
{
	writeobj(out, args->car, FALSE);
	return NULL;
}
