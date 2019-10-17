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
#include "mes_constants.h"
#include <unistd.h>

// CONSTANT STRUCT_PRINTER 1
#define STRUCT_PRINTER 1

struct scm* cons(struct scm* x, struct scm* y);
struct scm* apply(struct scm* f, struct scm* x);
struct scm* struct_ref_(struct scm* x, long i);
struct scm* builtin_p(struct scm* x);
struct scm* fdisplay_(struct scm*, int, int);
struct scm* vector_ref_(struct scm* table, long i);

int fdputs(char* s, int fd);
int fdputc(int c, int fd);
char *itoa(int number);
int eputs(char* s);
struct scm* error(struct scm* key, struct scm* x);

/* Imported Functions */
int string_len(char* a);
void raw_print(char* s, int fd);
void fd_print(char* s, int f);
char* char_lookup(int c, int type);

/* Globals */
int g_depth;

struct scm* display_helper(struct scm* x, int cont, char* sep, int fd, int write_p)
{
	struct scm* y = x;
	fdputs(sep, fd);

	if(g_depth == 0)
	{
		return cell_unspecified;
	}

	g_depth = g_depth - 1;
	int t = y->type;

	if(t == TCHAR)
	{
		if(!write_p)
		{
			fdputc(y->value, fd);
		}
		else
		{
			fdputc('#', fd);
			fd_print(char_lookup(y->value, TRUE), fd);
		}
	}
	else if(t == TCLOSURE)
	{
		fdputs("#<closure ", fd);
		struct scm* name = y->cdr->car->cdr->car;
		struct scm* args = y->cdr->cdr->car->car;
		display_helper(name->car, 0, "", fd, 0);
		fdputc(' ', fd);
		display_helper(args, 0, "", fd, 0);
		fdputc('>', fd);
	}
	else if(t == TMACRO)
	{
		fdputs("#<macro ", fd);
		display_helper(y->cdr, cont, "", fd, 0);
		fdputc('>', fd);
	}
	else if(t == TVARIABLE)
	{
		fdputs("#<variable ", fd);
		display_helper(y->car->car, cont, "", fd, 0);
		fdputc('>', fd);
	}
	else if(t == TNUMBER)
	{
		fdputs(itoa(y->value), fd);
	}
	else if(t == TPAIR)
	{
		if(!cont) fdputc('(', fd);

		if(y->car == cell_circular && y->cdr->car != cell_closure)
		{
			fdputs("(*circ* . ", fd);
			int i = 0;
			y = y->cdr;

			while(y != cell_nil && i++ < 10)
			{
				fdisplay_(y->car->car, fd, write_p);
				fdputc(' ', fd);
				y = y->cdr;
			}

			fdputs(" ...)", fd);
		}
		else
		{
			if(y != cell_nil)
			{
				fdisplay_(y->car, fd, write_p);
			}

			if(y->cdr->type == TPAIR)
			{
				display_helper(y->cdr, 1, " ", fd, write_p);
			}
			else if(y->cdr != cell_nil)
			{
				if(y->cdr->type != TPAIR)
				{
					fdputs(" . ", fd);
				}

				fdisplay_(y->cdr, fd, write_p);
			}
		}

		if(!cont) fdputc(')', fd);
	}
	else if(t == TPORT)
	{
		fdputs("#<port ", fd);
		fdputs(itoa(y->port), fd);
		fdputc(' ', fd);
		char *s = y->cdr->cdr->string;
		raw_print(s, fd);
		fdputs("\">", fd);
	}
	else if(t == TKEYWORD)
	{
		fdputs("#:", fd);
		char *s = y->cdr->string;
		raw_print(s, fd);
	}
	else if(t == TSTRING)
	{
		if(write_p) fdputc('"', fd);
		char *s = y->cdr->string;
		fd_print(s, fd);
		if(write_p) fdputc('"', fd);
	}
	else if(t == TSPECIAL)
	{
		char *s = y->cdr->string;
		raw_print(s, fd);
	}
	else if(t == TSYMBOL)
	{
		char *s = y->cdr->string;
		raw_print(s, fd);
	}
	else if(t == TREF)
	{
		fdisplay_(y->car, fd, write_p);
	}
	else if(t == TSTRUCT)
	{
		//struct scm* printer = STRUCT (x) + 1;
		struct scm* printer = struct_ref_(x, STRUCT_PRINTER);

		if(printer->type == TREF)
		{
			printer = printer->car;
		}

		if(printer->type == TCLOSURE || builtin_p(printer) == cell_t)
		{
			apply(printer, cons(x, cell_nil));
		}
		else
		{
			fdputs("#<", fd);
			fdisplay_(y->struc, fd, write_p);

			long size = y->length;

			for(long i = 2; i < size; i++)
			{
				fdputc(' ', fd);
				fdisplay_(y->struc + i, fd, write_p);
			}

			fdputc('>', fd);
		}
	}
	else if(t == TVECTOR)
	{
		fdputs("#(", fd);

		fdisplay_(y->vector, fd, write_p);
		for(long i = 1; i < y->length; i = i + 1)
		{
			fdputs(" ", fd);
			fdisplay_(y->vector + i, fd, write_p);
		}

		fdputc(')', fd);
	}
	else
	{
		fdputs("<", fd);
		fdputs(itoa(t), fd);
		fdputs(":", fd);
		fdputs(itoa((SCM)x), fd);
		fdputs(">", fd);
	}

	return 0;
}

struct scm* display_(struct scm* x)
{
	g_depth = 5;
	return display_helper(x, 0, "", __stdout, 0);
}

struct scm* display_error_(struct scm* x)
{
	g_depth = 5;
	return display_helper(x, 0, "", __stderr, 0);
}

struct scm* display_port_(struct scm* x, struct scm* p)
{
	struct scm* p2 = p;
	assert(p2->type == TNUMBER);
	return fdisplay_(x, p2->value, 0);
}

struct scm* write_(struct scm* x)
{
	g_depth = 5;
	return display_helper(x, 0, "", __stdout, 1);
}

struct scm* write_error_(struct scm* x)
{
	g_depth = 5;
	return display_helper(x, 0, "", __stderr, 1);
}

struct scm* write_port_(struct scm* x, struct scm* p)
{
	struct scm* p2 = p;
	assert(p2->type == TNUMBER);
	return fdisplay_(x, p2->value, 1);
}

struct scm* fdisplay_(struct scm* x, int fd, int write_p)  ///((internal))
{
	g_depth = 5;
	return display_helper(x, 0, "", fd, write_p);
}

struct scm* frame_printer(struct scm* frame)
{
	fdputs("#<", __stdout);
	display_(struct_ref_(frame, 2));
	fdputs(" procedure: ", __stdout);
	display_(struct_ref_(frame, 3));
	fdputc('>', __stdout);
	return cell_unspecified;
}

struct scm* hash_table_printer(struct scm* table)
{
	fdputs("#<", __stdout);
	display_(struct_ref_(table, 2));
	fdputc(' ', __stdout);
	fdputs("size: ", __stdout);
	display_(struct_ref_(table, 3));
	fdputc(' ', __stdout);
	struct scm* buckets = struct_ref_(table, 4);
	fdputs("buckets: ", __stdout);

	struct scm* ybuckets = buckets;
	for(int i = 0; i < ybuckets->length; i++)
	{
		struct scm* f = vector_ref_(buckets, i);

		if(f != cell_unspecified)
		{
			fdputc('[', __stdout);

			while(f->type == TPAIR)
			{
				write_(f->car->car);
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
	return cell_unspecified;
}

struct scm* module_printer(struct scm* module)
{
	/* module = M0; */
	fdputs("#<", __stdout);
	display_(struct_ref_(module, 2));
	fdputc(' ', __stdout);
	fdputs("name: ", __stdout);
	display_(struct_ref_(module, 3));
	fdputc(' ', __stdout);
	fdputs("locals: ", __stdout);
	display_(struct_ref_(module, 4));
	fdputc(' ', __stdout);
	struct scm* table = struct_ref_(module, 5);
	fdputs("globals:\n  ", __stdout);
	display_(table);
	fdputc('>', __stdout);
	return cell_unspecified;
}

void assert_max_string(int i, char* msg, char* string)
{
	if(i > MAX_STRING)
	{
		eputs(msg);
		eputs(":string too long[");
		eputs(itoa(i));
		eputs("]:");
		string[MAX_STRING - 1] = 0;
		eputs(string);
		error(cell_symbol_system_error, cell_f);
	}
}

int eputs(char* s)
{
	write(__stderr, s, string_len(s));
	return 0;
}


struct scm* write_byte(struct scm* x)  ///((arity . n))
{
	struct scm* y = x;
	struct scm* c = y->car;
	struct scm* p = y->cdr;
	struct scm* pp = p->car;
	int fd = __stdout;

	if(p->type == TPAIR && pp->type == TNUMBER)
	{
		fd = pp->value;
	}

	if(1 == fd) fd = __stdout;
	if(2 == fd) fd = __stderr;

	write(fd, &c->string, 1);
	assert(c->type == TNUMBER || c->type == TCHAR);
	return c;
}

struct scm* write_char(struct scm* i)  ///((arity . n))
{
	struct scm* x = i;
	write_byte(x);
	return x;
}
