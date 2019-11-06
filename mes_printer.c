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

// CONSTANT STRUCT_PRINTER 1
#define STRUCT_PRINTER 1

char* char_lookup(int c, int type);
char* itoa(int number);
int fdputc(int c, int fd);
int fdputs(char* s, int fd);
int string_len(char* a);
struct scm* apply(struct scm* f, struct scm* x);
struct scm* builtin_p_(struct scm* x);
struct scm* error_(struct scm* key, struct scm* x);
struct scm* fdisplay_(struct scm*, int, int);
struct scm* make_tpair(struct scm* a, struct scm* b);
struct scm* struct_ref_(struct scm* x, SCM i);
void fd_print(char* s, int f);
void raw_print(char* s, int fd);
void require(int bool, char* error);

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
		display_helper(name->car, 0, "", fd, FALSE);
		fdputc(' ', fd);
		display_helper(args, 0, "", fd, FALSE);
		fdputc('>', fd);
	}
	else if(t == TMACRO)
	{
		fdputs("#<macro ", fd);
		display_helper(y->cdr, cont, "", fd, FALSE);
		fdputc('>', fd);
	}
	else if(t == TVARIABLE)
	{
		fdputs("#<variable ", fd);
		display_helper(y->car->car, cont, "", fd, FALSE);
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

			while(y != cell_nil && i < 10)
			{
				i = i + 1;
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
		char* s = y->cdr->cdr->string;
		raw_print(s, fd);
		fdputs("\">", fd);
	}
	else if(t == TKEYWORD)
	{
		fdputs("#:", fd);
		char* s = y->cdr->string;
		raw_print(s, fd);
	}
	else if(t == TSTRING)
	{
		if(write_p) fdputc('"', fd);
		char* s = y->cdr->string;
		fd_print(s, fd);
		if(write_p) fdputc('"', fd);
	}
	else if(t == TSPECIAL)
	{
		char* s = y->cdr->string;
		raw_print(s, fd);
	}
	else if(t == TSYMBOL)
	{
		char* s = y->cdr->string;
		raw_print(s, fd);
	}
	else if(t == TREF)
	{
		fdisplay_(y->car, fd, write_p);
	}
	else if(t == TSTRUCT)
	{
		/* struct scm* printer = STRUCT(x) + 1; */
		struct scm* printer = struct_ref_(x, STRUCT_PRINTER);

		if(printer->type == TREF)
		{
			printer = printer->car;
		}

		if(printer->type == TCLOSURE || builtin_p_(printer) == cell_t)
		{
			apply(printer, make_tpair(x, cell_nil));
		}
		else
		{
			fdputs("#<", fd);
			fdisplay_(y->struc, fd, write_p);

			SCM size = y->length;
			SCM i;

			for(i = 2; i < size; i = i + 1)
			{
				fdputc(' ', fd);
				fdisplay_(y->struc + (i*CELL_SIZE), fd, write_p);
			}

			fdputc('>', fd);
		}
	}
	else if(t == TVECTOR)
	{
		fdputs("#(", fd);

		fdisplay_(y->vector, fd, write_p);
		SCM i;
		struct scm* z = y->cdr->cdr;
		for(i = 1; i < y->length; i = i + 1)
		{
			fdputs(" ", fd);
			fdisplay_(z, fd, write_p);
			z = z->cdr;
		}

		fdputc(')', fd);
	}
	else
	{
		fdputs("<", fd);
		fdputs(itoa(t), fd);
		fdputs(":", fd);
		fdputs(itoa(x->type), fd);
		fdputs(":", fd);
		fdputs(itoa(x->rac), fd);
		fdputs(":", fd);
		fdputs(itoa(x->value), fd);
		fdputs(">", fd);
	}

	return 0;
}

struct scm* display_(struct scm* x) /* Internal */
{
	g_depth = 5;
	return display_helper(x, 0, "", __stdout, FALSE);
}

struct scm* display(struct scm* x) /* External */
{
	return display_(x->car);
}

struct scm* display_error_(struct scm* x) /* Internal */
{
	g_depth = 5;
	return display_helper(x, 0, "", __stderr, FALSE);
}

struct scm* display_error(struct scm* x) /* External */
{
	return display_error_(x->car);
}

struct scm* display_port_(struct scm* x, struct scm* port) /* Internal */
{
	struct scm* p2 = port;
	require(TNUMBER == p2->type, "mes_printer.c: display_port_ did not recieve TNUMBER\n");
	return fdisplay_(x, p2->value, FALSE);
}

struct scm* display_port(struct scm* x) /* External */
{
	return display_port_(x->car, x->cdr->car);
}

struct scm* scm_write(struct scm* x) /* External */
{
	g_depth = 5;
	return display_helper(x->car, 0, "", __stdout, TRUE);
}

struct scm* write_error_(struct scm* x) /* Internal */
{
	g_depth = 5;
	return display_helper(x, 0, "", __stderr, TRUE);
}

struct scm* write_error(struct scm* x) /* External */
{
	return write_error_(x->car);
}

struct scm* write_port(struct scm* x) /* External */
{
	struct scm* p2 = x->cdr->car;
	require(TNUMBER == p2->type, "mes_printer: write_port_ did not recieve TNUMBER\n");
	return fdisplay_(x->car, p2->value, TRUE);
}

struct scm* fdisplay_(struct scm* x, int fd, int write_p)  /* ((internal)) */
{
	g_depth = 5;
	return display_helper(x, 0, "", fd, write_p);
}

struct scm* frame_printer(struct scm* frame) /* External */
{
	frame = frame->car;
	fdputs("#<", __stdout);
	display_(struct_ref_(frame, 2));
	fdputs(" procedure: ", __stdout);
	display_(struct_ref_(frame, 3));
	fdputc('>', __stdout);
	return cell_unspecified;
}

struct scm* module_printer_(struct scm* module) /* Internal */
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

struct scm* module_printer(struct scm* x) /* External */
{
	return module_printer_(x->car);
}

void assert_max_string(int i, char* msg, char* string)
{
	if(i > MAX_STRING)
	{
		raw_print(msg, __stderr);
		raw_print(":string too long[", __stderr);
		raw_print(itoa(i), __stderr);
		raw_print("]:", __stderr);
		string[MAX_STRING - 1] = 0;
		raw_print(string, __stderr);
		error_(cell_symbol_system_error, cell_f);
	}
}

struct scm* write_byte_(struct scm* x)  /* Internal */
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

	fdputc(fd, c->string[0]);
	require(c->type == TNUMBER || c->type == TCHAR, "mes_printer.c: write_byte was not TNUMBER or TCHAR\n");
	return c;
}

struct scm* write_byte(struct scm* x) /* External */
{
	return write_byte_(x->car);
}

struct scm* write_char(struct scm* x)  /* External */
{
	x = x->car;
	write_byte_(x);
	return x;
}
