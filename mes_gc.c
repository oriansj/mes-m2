/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

SCM GetSCM(struct scm* a, struct scm* table);
struct scm* g2b(struct scm* a);

long length__(SCM x);
int eputs(char const* s);
char *itoa (int number);
SCM error(SCM key, SCM x);
struct scm* cstring_to_symbol(char const *s);
SCM write_error_ (SCM x);
SCM gc_push_frame();
SCM gc_pop_frame();
struct scm* vector_entry(SCM x);
int get_env_value(char* c, int alt);
SCM make_cell__(long type, SCM car, SCM cdr);
struct scm* make_stack_type();
struct scm* make_struct(SCM type, struct scm* fields, SCM printer);
struct scm* make_frame_type();
SCM cons_ (SCM x, SCM y);
struct scm* make_vector__(long k);
void vector_set_x_(SCM x, long i, SCM e);

SCM GC_SAFETY;
SCM ARENA_SIZE;
SCM MAX_ARENA_SIZE;
SCM JAM_SIZE;
SCM STACK_SIZE;
// CONSTANT FRAME_SIZE 5
#define FRAME_SIZE 5


struct scm *g_news;

void initialize_memory()
{
	g_news = 0;
	MAX_ARENA_SIZE = get_env_value("MES_MAX_ARENA", 100000000);
	ARENA_SIZE = get_env_value("MES_ARENA", 10000000);
	JAM_SIZE = get_env_value("MES_JAM", ARENA_SIZE / 10);
	GC_SAFETY = get_env_value("MES_SAFETY", ARENA_SIZE / 100);
	STACK_SIZE = get_env_value("MES_STACK", 20000);
	MAX_STRING = get_env_value("MES_MAX_STRING", 524288);
}

void gc_init_cells()  ///((internal))
{
	SCM arena_bytes = (ARENA_SIZE + JAM_SIZE) * sizeof(struct scm);
	void *p = calloc(arena_bytes + STACK_SIZE * sizeof(SCM), 1);
	g_cells = (struct scm *)p;
	g_stack_array = (struct scm**)((char*)p + arena_bytes);
	// FIXME: remove MES_MAX_STRING, grow dynamically
	g_buf = (char*)calloc(MAX_STRING, 1);
}

SCM mes_g_stack(SCM a)  ///((internal))
{
	g_stack = STACK_SIZE;
	r0 = a;
	r1 = make_cell__ (TCHAR, 0, 0);
	r2 = make_cell__ (TCHAR, 0, 0);
	r3 = make_cell__ (TCHAR, 0, 0);
	return r0;
}

struct scm* make_frame(long index)
{
	long array_index = (STACK_SIZE - (index * FRAME_SIZE));
	SCM procedure = (SCM) g_stack_array[array_index + FRAME_PROCEDURE];

	if(!procedure)
	{
		procedure = cell_f;
	}

	return good2bad(make_struct((SCM) make_frame_type()
	                  , (struct scm*) cons_(cell_symbol_frame, cons_(procedure, cell_nil))
	                  , GetSCM2(cstring_to_symbol("frame-printer"))));
}

struct scm* make_stack()  ///((arity . n))
{
	SCM stack_type = GetSCM2(bad2good(make_stack_type()));
	long size = (STACK_SIZE - g_stack) / FRAME_SIZE;
	SCM frames = GetSCM2(make_vector__(size));

	for(long i = 0; i < size; i++)
	{
		SCM frame = GetSCM2(bad2good(make_frame(i)));
		vector_set_x_(frames, i, frame);
	}

	SCM values = cell_nil;
	values = cons_(frames, values);
	values = cons_(cell_symbol_stack, values);
	return good2bad(make_struct(stack_type, (struct scm*)values, cell_unspecified));
}


size_t bytes_cells(size_t length)
{
	return (1 + sizeof(long) + sizeof(long) + length + sizeof(SCM)) / sizeof(SCM);
}

struct scm* alloc(SCM n)
{
	SCM x = g_free;
	g_free = g_free + n;

	if(g_free > ARENA_SIZE)
	{
		assert(!"alloc: out of memory");
	}

	return Getstructscm2(x);
}

SCM make_cell__(SCM type, SCM car, SCM cdr)
{
	struct scm* x = alloc(1);
	x->type = type;
	x->rac = car;
	x->rdc = cdr;
	return GetSCM2(x);
}

SCM make_cell_(SCM type, SCM car, SCM cdr)
{
	struct scm* t = Getstructscm2(type);
	assert(t->type == TNUMBER);

	if(t->value == TCHAR || t->value == TNUMBER)
	{
		if(0 != car)
		{
			if(0 != cdr)
			{
				return make_cell__(t->value, Getstructscm2(car)->rac, Getstructscm2(cdr)->rdc);
			}
			else
			{
				return make_cell__(t->value, Getstructscm2(car)->rac, 0);
			}
		}
		else
		{
			if(0 != cdr)
			{
				return make_cell__(t->value, 0, Getstructscm2(cdr)->rdc);
			}
			else
			{
				return make_cell__(t->value, 0, 0);
			}
		}
	}

	return make_cell__(t->value, car, cdr);
}

struct scm* make_cell(SCM type, struct scm* car, struct scm* cdr)
{
	struct scm* x = alloc(1);
	x->type = type;
	x->car = good2bad(car);
	x->cdr = good2bad(cdr);
	return x;
}


struct scm* make_bytes(char const* s, size_t length)
{
	size_t size = bytes_cells(length);
	struct scm* x = alloc(size);
	x->type = TBYTES;
	x->length = length;
	char *p = (char*) &x->rdc;

	if(!length)
	{
		*(char*)p = 0;
	}
	else
	{
		memcpy(p, s, length + 1);
	}

	return x;
}

struct scm* make_vector__(SCM k)
{
	SCM v = GetSCM2(alloc(k));
	struct scm* x = bad2good((struct scm*)make_cell__(TVECTOR, k, v));
	SCM i;
	struct scm* w;
	struct scm* u;

	for(i = 0; i < k; i = i + 1)
	{
		w = Getstructscm2(v) + i;
		u = vector_entry(cell_unspecified);
		/* The below is likely going to be a problem for M2-Planet until we add pointer dereferencing */
		*w = *u;
	}

	return x;
}

struct scm* make_struct(SCM type, struct scm* fields, SCM printer)
{
	fields = bad2good(fields);
	long size = 2 + length__(GetSCM2(fields));
	struct scm* v = alloc(size);
	struct scm* w = v + 1;
	struct scm* entry = vector_entry(type);
	struct scm* print = vector_entry(printer);
	/* The below is likely going to be a problem for M2-Planet until we add pointer dereferencing */
	*v = *entry;
	*w = *print;

	for(long i = 2; i < size; i++)
	{
		SCM e = cell_unspecified;

		if(GetSCM2(fields) != cell_nil)
		{
			e = fields->rac;
			fields = bad2good(fields->cdr);
		}

		entry = vector_entry(e);
		w = v + i;
		/* The below is likely going to be a problem for M2-Planet until we add pointer dereferencing */
		*w = *entry;
	}

	return Getstructscm2(make_cell__(TSTRUCT, size, GetSCM2(v)));
}

void gc_flip()  ///((internal))
{
	if(g_debug > 2)
	{
		eputs(";;;   => jam[");
		eputs(itoa(g_free));
		eputs("]\n");
	}

	if(g_free > JAM_SIZE)
	{
		JAM_SIZE = g_free + g_free / 2;
	}

	memcpy(g_cells - 1, g_news - 1, (g_free + 2)*sizeof(struct scm));
	size_t clear = (ARENA_SIZE - g_free ) * sizeof(struct scm) - (STACK_SIZE - g_stack) * sizeof(SCM);
	memset(g_cells + g_free, 0, clear);
}

struct scm* gc_copy(SCM old)  ///((internal))
{
	struct scm* o = Getstructscm2(old);
	if(o->type == TBROKEN_HEART)
	{
		return &g_news[o->rac];
	}

	SCM new = g_free++;
	struct scm* n = &g_news[new];
	*n = *o;

	if(n->type == TSTRUCT || n->type == TVECTOR)
	{
		n->vector = g_free;
		long i;

		struct scm* new;
		struct scm* old;
		for(i = 0; i < o->length; i++)
		{
			new = &g_news[g_free + i];
			old = bad2good(o->cdr) + i;
			/* The below is likely going to be a problem for M2-Planet until we add pointer dereferencing */
			*new = *old;
		}

		g_free = g_free + i;;
	}
	else if(n->type == TBYTES)
	{
		char const *src = (char*)&o->rdc;
		char *dest = (char*)&n->rdc;
		size_t length = n->length;
		memcpy(dest, src, length + 1);
		g_free += bytes_cells(length) - 1;

		if(g_debug > 4)
		{
			eputs("gc copy bytes: ");
			eputs(src);
			eputs("\n");
			eputs("    length: ");
			eputs(itoa(o->length));
			eputs("\n");
			eputs("    nlength: ");
			eputs(itoa(n->length));
			eputs("\n");
			eputs("        ==> ");
			eputs(dest);
			eputs("\n");
		}
	}

	o->type = TBROKEN_HEART;
	o->rac = new;
	return n;
}

struct scm* gc_copy_new(struct scm* old)  ///((internal))
{
	if(old->type == TBROKEN_HEART)
	{
		return bad2good(old->car);
	}

	SCM new = g_free++;
	struct scm* n = &g_news[new];
	*n = *old;

	if(n->type == TSTRUCT || n->type == TVECTOR)
	{
		n->vector = g_free;
		long i;

		struct scm* l = &g_news[g_free];
		struct scm* m;
		struct scm* o;
		for(i = 0; i < old->length; i++)
		{
			m = l + i;
			o = bad2good(old->cdr) + i;
			/* The below is likely going to be a problem for M2-Planet until we add pointer dereferencing */
			*m = *o;
		}

		g_free = g_free + i;;
	}
	else if(n->type == TBYTES)
	{
		char const *src = (char*) &old->cdr;
		char *dest = (char*)&n->cdr;
		size_t length = n->length;
		memcpy(dest, src, length + 1);
		g_free += bytes_cells(length) - 1;

		if(g_debug > 4)
		{
			eputs("gc copy bytes: ");
			eputs(src);
			eputs("\n");
			eputs("    length: ");
			eputs(itoa(old->length));
			eputs("\n");
			eputs("    nlength: ");
			eputs(itoa(n->length));
			eputs("\n");
			eputs("        ==> ");
			eputs(dest);
			eputs("\n");
		}
	}

	old->type = TBROKEN_HEART;
	old->car = g2b(n);
	return n;
}

void gc_loop()  ///((internal))
{
	SCM scan = 1;
	struct scm* s = g_news + 1;
	struct scm* g = &g_news[g_free];

	while(s < g)
	{
		if(s->type == TBROKEN_HEART)
		{
			error(cell_symbol_system_error, GetSCM2(cstring_to_symbol("gc")));
		}

		if(s->type == TMACRO
		        || s->type == TPAIR
		        || s->type == TREF
		        || scan == 1 // null
		        || s->type == TVARIABLE)
		{
			g_news[scan].car = g2b(gc_copy(g_news[scan].rac));
		}

		if((s->type == TCLOSURE
		        || s->type == TCONTINUATION
		        || s->type == TKEYWORD
		        || s->type == TMACRO
		        || s->type == TPAIR
		        || s->type == TPORT
		        || s->type == TSPECIAL
		        || s->type == TSTRING
		        || s->type == TSYMBOL
		        || scan == 1 // null
		        || s->type == TVALUES)
		        && g_news[scan].cdr) // allow for 0 terminated list of symbols
		{
			g_news[scan].cdr = g2b(gc_copy(g_news[scan].rdc));
		}

		if(s->type == TBYTES)
		{
			//scan += bytes_cells(s->length) - 1;
			int b = bytes_cells(s->length) - 1;
			scan += b;
			s += b;
		}

		scan++;
		s++;
		g = &g_news[g_free];
	}

	gc_flip();
}

struct scm* gc();
struct scm* gc_check()
{
	if(g_free + GC_SAFETY > ARENA_SIZE)
	{
		gc();
	}

	return good2bad(Getstructscm2(cell_unspecified));
}

void gc_()  ///((internal))
{
	struct scm* nil = Getstructscm2(g_free);
	nil->type = TVECTOR;
	nil->length = 1000;
	nil->vector = 0;

	nil = nil + 1;
	nil->type = TCHAR;
	nil->value = 'n';

	g_news = g_cells + g_free + 1;

	if(g_debug == 2)
	{
		eputs(".");
	}

	if(g_debug > 2)
	{
		eputs(";;; gc[");
		eputs(itoa(g_free));
		eputs(":");
		eputs(itoa(ARENA_SIZE - g_free));
		eputs("]...");
	}

	g_free = 1;
	if(ARENA_SIZE < MAX_ARENA_SIZE && g_news > 0)
	{
		if(g_debug == 2)
		{
			eputs("+");
		}

		if(g_debug > 2)
		{
			eputs(" up[");
			eputs(itoa((unsigned long)g_cells));
			eputs(",");
			eputs(itoa((unsigned long)g_news));
			eputs(":");
			eputs(itoa(ARENA_SIZE));
			eputs(",");
			eputs(itoa(MAX_ARENA_SIZE));
			eputs("]...");
		}
	}

	for(long i = g_free; i < g_symbol_max; i++)
	{
		//gc_copy(i);
		gc_copy_new(Getstructscm2(i));
	}

	g_symbols = GetSCM(gc_copy_new(Getstructscm2(g_symbols)), g_news);
	g_macros = GetSCM(gc_copy_new(Getstructscm2(g_macros)), g_news);
	g_ports = GetSCM(gc_copy_new(Getstructscm2(g_ports)), g_news);
	m0 = GetSCM(gc_copy_new(Getstructscm2(m0)), g_news);

	for(long i = g_stack; i < STACK_SIZE; i++)
	{
		g_stack_array[i] = g2b(gc_copy((SCM)g_stack_array[i]));
	}

	gc_loop();
}

struct scm* gc()
{
	if(g_debug > 4)
	{
		eputs("symbols: ");
		write_error_(g_symbols);
		eputs("\n");
		eputs("R0: ");
		write_error_(r0);
		eputs("\n");
	}

	gc_push_frame();
	gc_();
	gc_pop_frame();

	if(g_debug > 4)
	{
		eputs("symbols: ");
		write_error_(g_symbols);
		eputs("\n");
		eputs("R0: ");
		write_error_(r0);
		eputs("\n");
	}
	return good2bad(Getstructscm2(cell_unspecified));
}
