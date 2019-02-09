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

#define FRAME_SIZE 5

#define TYPE(x) g_cells[x].type
#define VARIABLE(x) g_cells[x].variable
#define VALUE(x) g_cells[x].value
#define LENGTH(x) g_cells[x].length
#define REF(x) g_cells[x].ref
#define STRUCT(x) g_cells[x].struc
#define VECTOR(x) g_cells[x].vector
#define CAR(x) g_cells[x].rac
#define CDR(x) g_cells[x].rdc
#define PORT(x) g_cells[x].port
#define STRING(x) g_cells[x].rdc

// CONSTANT STRUCT_TYPE 0
#define STRUCT_TYPE 0
// CONSTANT STRUCT_PRINTER 1
#define STRUCT_PRINTER 1

int g_depth;
struct scm* fdisplay_(SCM, int, int);
int fdputs(char const* s, int fd);
int fdputc(int c, int fd);
char *itoa (int number);
struct scm* struct_ref_(SCM x, long i);
SCM builtin_p (SCM x);
SCM apply(SCM f, SCM x);
SCM car (SCM x);
SCM cdr (SCM x);
SCM cons (SCM x, SCM y);
struct scm* make_struct (SCM type, SCM fields, SCM printer);
struct scm* cstring_to_symbol(char const *s);
struct scm* make_vector__(long k);
void vector_set_x_(SCM x, long i, SCM e);
struct scm* vector_length (struct scm* x);
struct scm* vector_ref (SCM x, SCM i);
struct scm* string_equal_p (SCM a, SCM b);
struct scm* eq_p (SCM x, SCM y);

struct scm* display_helper(SCM x, int cont, char* sep, int fd, int write_p)
{
	fdputs(sep, fd);

	if(g_depth == 0)
	{
		return good2bad(Getstructscm2(cell_unspecified, g_cells), g_cells);
	}

	g_depth = g_depth - 1;
	int t = TYPE(x);

	if(t == TCHAR)
	{
		if(!write_p)
		{
			fdputc(VALUE(x), fd);
		}
		else
		{
			fdputs("#", fd);
			long v = VALUE(x);

			if(v == '\0')
			{
				fdputs("\\nul", fd);
			}
			else if(v == '\a')
			{
				fdputs("\\alarm", fd);
			}
			else if(v == '\b')
			{
				fdputs("\\backspace", fd);
			}
			else if(v == '\t')
			{
				fdputs("\\tab", fd);
			}
			else if(v == '\n')
			{
				fdputs("\\newline", fd);
			}
			else if(v == '\v')
			{
				fdputs("\\vtab", fd);
			}
			else if(v == '\f')
			{
				fdputs("\\page", fd);
			}
			//Nyacc bug
			// else if (v == '\r') fdputs ("return", fd);
			else if(v == 13)
			{
				fdputs("\\return", fd);
			}
			else if(v == ' ')
			{
				fdputs("\\space", fd);
			}
			else
			{
				if(v >= 32 && v <= 127)
				{
					fdputc('\\', fd);
				}

				fdputc(VALUE(x), fd);
			}
		}
	}
	else if(t == TCLOSURE)
	{
		fdputs("#<closure ", fd);
		SCM circ = CAR (CDR (x));
		SCM name = CAR (CDR (circ));
		SCM args = CAR(CDR (CDR (x)));
		display_helper(CAR(name), 0, "", fd, 0);
		fdputc(' ', fd);
		display_helper(args, 0, "", fd, 0);
		fdputs(">", fd);
	}
	else if(t == TMACRO)
	{
		fdputs("#<macro ", fd);
		display_helper(CDR(x), cont, "", fd, 0);
		fdputs(">", fd);
	}
	else if(t == TVARIABLE)
	{
		fdputs("#<variable ", fd);
		display_helper(CAR(VARIABLE(x)), cont, "", fd, 0);
		fdputs(">", fd);
	}
	else if(t == TNUMBER)
	{
		fdputs(itoa(VALUE(x)), fd);
	}
	else if(t == TPAIR)
	{
		if(!cont)
		{
			fdputs("(", fd);
		}

		if(CAR(x) == cell_circular
		        && CAR (CDR (x)) != cell_closure)
		{
			fdputs("(*circ* . ", fd);
			int i = 0;
			x = CDR(x);

			while(x != cell_nil && i++ < 10)
			{
				fdisplay_(CAR (CAR (x)), fd, write_p);
				fdputs(" ", fd);
				x = CDR(x);
			}

			fdputs(" ...)", fd);
		}
		else
		{
			if(x && x != cell_nil)
			{
				fdisplay_(CAR(x), fd, write_p);
			}

			if(CDR(x) && TYPE(CDR(x)) == TPAIR)
			{
				display_helper(CDR(x), 1, " ", fd, write_p);
			}
			else if(CDR(x) && CDR(x) != cell_nil)
			{
				if(TYPE(CDR(x)) != TPAIR)
				{
					fdputs(" . ", fd);
				}

				fdisplay_(CDR(x), fd, write_p);
			}
		}

		if(!cont)
		{
			fdputs(")", fd);
		}
	}
	else if(t == TKEYWORD
	        || t == TPORT
	        || t == TSPECIAL
	        || t == TSTRING
	        || t == TSYMBOL)
	{
		if(t == TPORT)
		{
			fdputs("#<port ", fd);
			fdputs(itoa(PORT(x)), fd);
			fdputs(" ", fd);
			x = STRING(x);
		}

		if(t == TKEYWORD)
		{
			fdputs("#:", fd);
		}

		if((write_p && t == TSTRING) || t == TPORT)
		{
			fdputc('"', fd);
		}

		char const *s = (char*)&g_cells[STRING (x)].rdc;
#if 0
		s += START(x);
		size_t length = LEN(x);
#else
		size_t length = LENGTH(x);
#endif

		for(size_t i = 0; i < length; i++)
		{
			long v = write_p ? s[i] : -1;

			if(v == '\0')
			{
				fdputs("\\0", fd);
			}
			else if(v == '\a')
			{
				fdputs("\\a", fd);
			}
			else if(v == '\b')
			{
				fdputs("\\b", fd);
			}
			else if(v == '\t')
			{
				fdputs("\\t", fd);
			}
			else if(v == '\v')
			{
				fdputs("\\v", fd);
			}
			else if(v == '\n')
			{
				fdputs("\\n", fd);
			}
			else if(v == '\f')
			{
				fdputs("\\f", fd);
			}

#if 1 //__MESC__
			//Nyacc bug
			else if(v == 13)
			{
				fdputs("\\r", fd);
			}
			else if(v == 27)
			{
				fdputs("\\e", fd);
			}

#else
			//else if (v == '\r') fdputs ("\\r", fd);
			//Nyacc crash
			//else if (v == '\e') fdputs ("\\e", fd);
#endif
			else if(v == '\\')
			{
				fdputs("\\\\", fd);
			}
			else if(v == '"') fdputs("\\\"", fd);
			else
			{
				fdputc(s[i], fd);
			}
		}

		if((write_p && t == TSTRING) || t == TPORT)
		{
			fdputc('"', fd);
		}

		if(t == TPORT)
		{
			fdputs(">", fd);
		}
	}
	else if(t == TREF)
	{
		fdisplay_(REF(x), fd, write_p);
	}
	else if(t == TSTRUCT)
	{
		//SCM printer = STRUCT (x) + 1;
		SCM printer = GetSCM2(bad2good(struct_ref_(x, STRUCT_PRINTER), g_cells), g_cells);

		if(TYPE(printer) == TREF)
		{
			printer = REF(printer);
		}

		if(TYPE(printer) == TCLOSURE || builtin_p(printer) == cell_t)
		{
			apply(printer, cons(x, cell_nil));
		}
		else
		{
			fdputs("#<", fd);
			fdisplay_(STRUCT(x), fd, write_p);

			long size = LENGTH(x);

			for(long i = 2; i < size; i++)
			{
				fdputc(' ', fd);
				fdisplay_(STRUCT(x) + i, fd, write_p);
			}

			fdputc('>', fd);
		}
	}
	else if(t == TVECTOR)
	{
		fdputs("#(", fd);

		for(long i = 0; i < LENGTH(x); i++)
		{
			if(i)
			{
				fdputc(' ', fd);
			}

			fdisplay_(VECTOR(x) + i, fd, write_p);
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
	assert(TYPE(p) == TNUMBER);
	return fdisplay_(x, VALUE(p), 0);
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
	assert(TYPE(p) == TNUMBER);
	return fdisplay_(x, VALUE(p), 1);
}

struct scm* fdisplay_(SCM x, int fd, int write_p)  ///((internal))
{
	g_depth = 5;
	return display_helper(x, 0, "", fd, write_p);
}

struct scm* exit_(SCM x)  ///((name . "exit"))
{
	assert(TYPE(x) == TNUMBER);
	exit(VALUE(x));
}

struct scm* frame_printer(SCM frame)
{
	fdputs("#<", __stdout);
	display_(GetSCM2(bad2good(struct_ref_(frame, 2), g_cells), g_cells));
	fdputc(' ', __stdout);
	fdputs("procedure: ", __stdout);
	display_(GetSCM2(bad2good(struct_ref_(frame, 3), g_cells), g_cells));
	fdputc('>', __stdout);
	return good2bad(Getstructscm2(cell_unspecified, g_cells), g_cells);
}

struct scm* make_frame_type()  ///((internal))
{
	SCM record_type = cell_symbol_record_type; // FIXME
	SCM fields = cell_nil;
	fields = cons(cell_symbol_procedure, fields);
	fields = cons(fields, cell_nil);
	fields = cons(cell_symbol_frame, fields);
	return make_struct(record_type, fields, cell_unspecified);
}

struct scm* make_frame(long index)
{
	SCM frame_type = GetSCM2(bad2good(make_frame_type(), g_cells), g_cells);
	long array_index = (STACK_SIZE - (index * FRAME_SIZE));
	SCM procedure = (SCM) g_stack_array[array_index + FRAME_PROCEDURE];

	if(!procedure)
	{
		procedure = cell_f;
	}

	SCM values = cell_nil;
	values = cons(procedure, values);
	values = cons(cell_symbol_frame, values);
	return make_struct(frame_type, values, GetSCM2(cstring_to_symbol("frame-printer"), g_cells));
}

struct scm* make_stack_type()  ///((internal))
{
	SCM record_type = cell_symbol_record_type; // FIXME
	SCM fields = cell_nil;
	fields = cons(GetSCM2(cstring_to_symbol("frames"), g_cells), fields);
	fields = cons(fields, cell_nil);
	fields = cons(cell_symbol_stack, fields);
	return make_struct(record_type, fields, cell_unspecified);
}

struct scm* make_stack()  ///((arity . n))
{
	SCM stack_type = GetSCM2(bad2good(make_stack_type(), g_cells), g_cells);
	long size = (STACK_SIZE - g_stack) / FRAME_SIZE;
	SCM frames = GetSCM2(make_vector__(size), g_cells);

	for(long i = 0; i < size; i++)
	{
		SCM frame = GetSCM2(bad2good(make_frame(i), g_cells), g_cells);
		vector_set_x_(frames, i, frame);
	}

	SCM values = cell_nil;
	values = cons(frames, values);
	values = cons(cell_symbol_stack, values);
	return make_struct(stack_type, values, cell_unspecified);
}

struct scm* stack_length(SCM stack)
{
	SCM frames = GetSCM2(bad2good(struct_ref_(stack, 3), g_cells), g_cells);
	return vector_length(good2bad(Getstructscm2(frames, g_cells), g_cells));
}

struct scm* stack_ref(SCM stack, SCM index)
{
	SCM frames = GetSCM2(bad2good(struct_ref_(stack, 3), g_cells), g_cells);
	return vector_ref(frames, index);
}

struct scm* xassq(SCM x, SCM a)  ///for speed in core only
{
	while(a != cell_nil && x != CDR (CAR (a)))
	{
		a = CDR(a);
	}

	return a != cell_nil ? good2bad(Getstructscm2(CAR(a), g_cells), g_cells) : good2bad(Getstructscm2(cell_f, g_cells), g_cells);
}

struct scm* memq(SCM x, SCM a)
{
	int t = TYPE(x);

	if(t == TCHAR || t == TNUMBER)
	{
		SCM v = VALUE(x);

		while(a != cell_nil && v != VALUE(CAR(a)))
		{
			a = CDR(a);
		}
	}
	else if(t == TKEYWORD)
	{
		while(a != cell_nil && (TYPE(CAR(a)) != TKEYWORD || GetSCM2(bad2good(string_equal_p(x, CAR(a)), g_cells), g_cells) == cell_f))
		{
			a = CDR(a);
		}
	}
	else
	{
		while(a != cell_nil && x != CAR(a))
		{
			a = CDR(a);
		}
	}

	return a != cell_nil ? good2bad(Getstructscm2(a, g_cells), g_cells) : good2bad(Getstructscm2(cell_f, g_cells), g_cells);
}

struct scm* equal2_p(SCM a, SCM b)
{
equal2:

	if(a == b)
	{
		return good2bad(Getstructscm2(cell_t, g_cells), g_cells);
	}

	if(TYPE(a) == TPAIR && TYPE(b) == TPAIR)
	{
		if(equal2_p(CAR(a), CAR(b)) == good2bad(Getstructscm2(cell_t, g_cells), g_cells))
		{
			a = CDR(a);
			b = CDR(b);
			goto equal2;
		}

		return good2bad(Getstructscm2(cell_f, g_cells), g_cells);
	}

	if(TYPE(a) == TSTRING && TYPE(b) == TSTRING)
	{
		return string_equal_p(a, b);
	}

	if(TYPE(a) == TVECTOR && TYPE(b) == TVECTOR)
	{
		if(LENGTH(a) != LENGTH(b))
		{
			return good2bad(Getstructscm2(cell_f, g_cells), g_cells);
		}

		for(long i = 0; i < LENGTH(a); i++)
		{
			SCM ai = VECTOR(a) + i;
			SCM bi = VECTOR(b) + i;

			if(TYPE(ai) == TREF)
			{
				ai = REF(ai);
			}

			if(TYPE(bi) == TREF)
			{
				bi = REF(bi);
			}

			if(equal2_p(ai, bi) == good2bad(Getstructscm2(cell_f, g_cells), g_cells))
			{
				return good2bad(Getstructscm2(cell_f, g_cells), g_cells);
			}
		}

		return good2bad(Getstructscm2(cell_t, g_cells), g_cells);
	}

	return eq_p(a, b);
}

struct scm* last_pair(SCM x)
{
	while(x != cell_nil && CDR(x) != cell_nil)
	{
		x = CDR(x);
	}

	return good2bad(Getstructscm2(x, g_cells), g_cells);
}

struct scm* pair_p(SCM x)
{
	return TYPE(x) == TPAIR ? good2bad(Getstructscm2(cell_t, g_cells), g_cells) : good2bad(Getstructscm2(cell_f, g_cells), g_cells);
}
