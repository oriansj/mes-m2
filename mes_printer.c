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

SCM cons (SCM x, SCM y);
SCM apply(SCM f, SCM x);
struct scm* struct_ref_(SCM x, long i);
struct scm* vector_ref_(SCM x, long i);

SCM builtin_p (SCM x);
struct scm* fdisplay_(SCM, int, int);
int fdputs(char const* s, int fd);
int fdputc(int c, int fd);
char *itoa (int number);
int eputs(char const* s);
SCM error(SCM key, SCM x);

/* Imported Functions */
void raw_print(char* s, int fd);
void fd_print(char* s, int f);
char* char_lookup(int c, int type);

/* Globals */
int g_depth;

struct scm* display_helper(SCM x, int cont, char* sep, int fd, int write_p)
{
	struct scm* y = Getstructscm2(x, g_cells);
	fdputs(sep, fd);

	if(g_depth == 0)
	{
		return good2bad(Getstructscm2(cell_unspecified, g_cells), g_cells);
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
		struct scm* name = bad2good(bad2good(bad2good(bad2good(y->cdr, g_cells)->car, g_cells)->cdr, g_cells)->car, g_cells);
		SCM args = bad2good(bad2good(bad2good(y->cdr, g_cells)->cdr, g_cells)->car, g_cells)->rac;
		display_helper(name->rac, 0, "", fd, 0);
		fdputc(' ', fd);
		display_helper(args, 0, "", fd, 0);
		fdputc('>', fd);
	}
	else if(t == TMACRO)
	{
		fdputs("#<macro ", fd);
		display_helper(y->rdc, cont, "", fd, 0);
		fdputc('>', fd);
	}
	else if(t == TVARIABLE)
	{
		fdputs("#<variable ", fd);
		display_helper(bad2good(y->car, g_cells)->rac, cont, "", fd, 0);
		fdputc('>', fd);
	}
	else if(t == TNUMBER)
	{
		fdputs(itoa(y->value), fd);
	}
	else if(t == TPAIR)
	{
		if(!cont) fdputc('(', fd);

		if(y->rac == cell_circular && bad2good(y->cdr, g_cells)->rac != cell_closure)
		{
			fdputs("(*circ* . ", fd);
			int i = 0;
			y = bad2good(y->cdr, g_cells);

			while(GetSCM2(y, g_cells) != cell_nil && i++ < 10)
			{
				fdisplay_(bad2good(y->car, g_cells)->rac, fd, write_p);
				fdputc(' ', fd);
				y = bad2good(y->cdr, g_cells);
			}

			fdputs(" ...)", fd);
		}
		else
		{
			if(GetSCM2(y, g_cells) != cell_nil)
			{
				fdisplay_(y->rac, fd, write_p);
			}

			if(bad2good(y->cdr, g_cells)->type == TPAIR)
			{
				display_helper(y->rdc, 1, " ", fd, write_p);
			}
			else if(y->rdc != cell_nil)
			{
				if(bad2good(y->cdr, g_cells)->type != TPAIR)
				{
					fdputs(" . ", fd);
				}

				fdisplay_(y->rdc, fd, write_p);
			}
		}

		if(!cont) fdputc(')', fd);
	}
	else if(t == TPORT)
	{
		fdputs("#<port ", fd);
		fdputs(itoa(y->rac), fd);
		fdputc(' ', fd);
		char *s = (char*)bad2good(bad2good(y->cdr, g_cells)->cdr, g_cells)->rdc;
		raw_print(s, fd);
		fdputs("\">", fd);
	}
	else if(t == TKEYWORD)
	{
		fdputs("#:", fd);
		char *s = (char*)&bad2good(y->cdr, g_cells)->rdc;
		raw_print(s, fd);
	}
	else if(t == TSTRING)
	{
		if(write_p) fdputc('"', fd);
		char *s = (char*)&bad2good(y->cdr, g_cells)->rdc;
		fd_print(s, fd);
		if(write_p) fdputc('"', fd);
	}
	else if(t == TSPECIAL)
	{
		char *s = (char*)&bad2good(y->cdr, g_cells)->rdc;
		raw_print(s, fd);
	}
	else if(t == TSYMBOL)
	{
		char *s = (char*)&bad2good(y->cdr, g_cells)->rdc;
		raw_print(s, fd);
	}
	else if(t == TREF)
	{
		fdisplay_(y->ref, fd, write_p);
	}
	else if(t == TSTRUCT)
	{
		//SCM printer = STRUCT (x) + 1;
		struct scm* printer = struct_ref_(x, STRUCT_PRINTER);

		if(printer->type == TREF)
		{
			printer = bad2good(printer->car, g_cells);
		}

		if(printer->type == TCLOSURE || builtin_p(GetSCM2(printer, g_cells)) == cell_t)
		{
			apply(GetSCM2(printer, g_cells), cons_(x, cell_nil));
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
		fdputs("#( ", fd);

		for(long i = 1; i < y->length; i++)
		{
			fdisplay_(y->vector + i, fd, write_p);
		}

		fdputc(')', fd);
	}
	else
	{
		fdputs("<", fd);
		fdputs(itoa(t), fd);
		fdputs(":", fd);
		fdputs(itoa(x), fd);
		fdputs(">", fd);
	}

	return 0;
}

struct scm* display_(SCM x)
{
	g_depth = 5;
	return display_helper(x, 0, "", __stdout, 0);
}

struct scm* display_error_(SCM x)
{
	g_depth = 5;
	return display_helper(x, 0, "", __stderr, 0);
}

struct scm* display_port_(SCM x, SCM p)
{
	struct scm* p2 = Getstructscm2(p, g_cells);
	assert(p2->type == TNUMBER);
	return fdisplay_(x, p2->value, 0);
}

struct scm* write_(SCM x)
{
	g_depth = 5;
	return display_helper(x, 0, "", __stdout, 1);
}

struct scm* write_error_(SCM x)
{
	g_depth = 5;
	return display_helper(x, 0, "", __stderr, 1);
}

struct scm* write_port_(SCM x, SCM p)
{
	struct scm* p2 = Getstructscm2(p, g_cells);
	assert(p2->type == TNUMBER);
	return fdisplay_(x, p2->value, 1);
}

struct scm* fdisplay_(SCM x, int fd, int write_p)  ///((internal))
{
	g_depth = 5;
	return display_helper(x, 0, "", fd, write_p);
}

struct scm* frame_printer(SCM frame)
{
	fdputs("#<", __stdout);
	display_(GetSCM2(struct_ref_(frame, 2), g_cells));
	fdputs(" procedure: ", __stdout);
	display_(GetSCM2(struct_ref_(frame, 3), g_cells));
	fdputc('>', __stdout);
	return good2bad(Getstructscm2(cell_unspecified, g_cells), g_cells);
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

struct scm* module_printer(SCM module)
{
	//module = m0;
	fdputs("#<", __stdout);
	display_(GetSCM2(struct_ref_(module, 2), g_cells));
	fdputc(' ', __stdout);
	fdputs("name: ", __stdout);
	display_(GetSCM2(struct_ref_(module, 3), g_cells));
	fdputc(' ', __stdout);
	fdputs("locals: ", __stdout);
	display_(GetSCM2(struct_ref_(module, 4), g_cells));
	fdputc(' ', __stdout);
	SCM table = GetSCM2(struct_ref_(module, 5), g_cells);
	fdputs("globals:\n  ", __stdout);
	display_(table);
	fdputc('>', __stdout);
	return good2bad(Getstructscm2(cell_unspecified, g_cells), g_cells);
}

void assert_max_string(int i, char const* msg, char* string)
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

int eputs(char const* s)
{
	write(__stderr, s, strlen(s));
	return 0;
}


struct scm* write_byte(SCM x)  ///((arity . n))
{
	struct scm* y = Getstructscm2(x, g_cells);
	struct scm* c = bad2good(y->car, g_cells);
	struct scm* p = bad2good(y->cdr, g_cells);
	struct scm* pp = bad2good(p->car, g_cells);
	int fd = __stdout;

	if(p->type == TPAIR && pp->type == TNUMBER)
	{
		fd = pp->value;
	}

	if(1 == fd) fd = __stdout;
	if(2 == fd) fd = __stderr;

	write(fd, &c->string, 1);
	assert(c->type == TNUMBER || c->type == TCHAR);
	return good2bad(c, g_cells);
}

struct scm* write_char(SCM i)  ///((arity . n))
{
	struct scm* x = Getstructscm2(i, g_cells);
	write_byte(GetSCM2(x, g_cells));
	return good2bad(x, g_cells);
}
