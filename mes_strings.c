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

struct scm* make_symbol (SCM string);
int eputs(char const* s);
char *itoa (int number);
SCM error(SCM key, SCM x);
SCM cons (SCM x, SCM y);
SCM make_cell__(long type, SCM car, SCM cdr);
struct scm* make_bytes(char const* s, size_t length);
SCM write_error_ (SCM x);
struct scm* hash_ref (SCM table, SCM key, SCM dflt);
struct scm* hash_set_x (SCM table, SCM key, SCM value);
int readchar();

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

char const* list_to_cstring(struct scm* list, int* size)
{
	int i = 0;
	list = bad2good(list, g_cells);

	while(list != &g_cells[cell_nil])
	{
		assert_max_string(i, "list_to_string", g_buf);

		g_buf[i] = g_cells[list->rac].value;
		i = i + 1;
		list = &g_cells[list->rdc];
	}

	g_buf[i] = 0;
	*size = i;
	return g_buf;
}

struct scm* make_string_(char const* s) // internal only
{
	assert_max_string(strlen(s) , "make_string_", (char*)s);

	SCM x = make_cell__(TSTRING, strlen(s), 0);
	SCM v = GetSCM2(make_bytes(s, strlen(s)), g_cells);
	g_cells[x].rdc = v;
	return Getstructscm2(x, g_cells);
}

struct scm* make_string(char const* s, int length)
{
	assert_max_string(length, "make_string", (char*)s);
	SCM x = make_cell__(TSTRING, length, 0);
	SCM v = GetSCM2(make_bytes(s, length), g_cells);
	g_cells[x].rdc = v;
	return good2bad(Getstructscm2(x, g_cells), g_cells);
}

struct scm* string_equal_p(SCM a, SCM b)  ///((name . "string=?"))
{
	struct scm* a2 = Getstructscm2(a, g_cells);
	struct scm* b2 = Getstructscm2(b, g_cells);
	assert(a2->type == TSTRING || a2->type == TKEYWORD);
	assert(b2->type == TSTRING || b2->type == TKEYWORD);
	struct scm* tee = good2bad(Getstructscm2(cell_t, g_cells), g_cells);
	struct scm* nil = good2bad(Getstructscm2(cell_f, g_cells), g_cells);

	/* If they are the same thing */
	if(a == b) return tee;

	/* If they point to the same string */
	if(a2->cdr == b2->cdr) return tee;

	/*If they are both empty strings */
	if((NULL == a2->car) && (NULL == b2->car)) return tee;

	/* If they are different lengths they can't be the same string */
	if(a2->length != b2->length) return nil;

	/* Need to fix */
	char* s1 = (char*)&g_cells[a2->rdc].string;
	char* s2 = (char*)&g_cells[b2->rdc].string;

	while(s1[0] == s2[0])
	{
		if(0 == s1[0]) return tee;
		s1 = s1 + 1;
		s2 = s2 + 1;
	}

	return nil;
}

struct scm* symbol_to_string(SCM symbol)
{
	struct scm* a = Getstructscm2(symbol, g_cells);
	return good2bad(Getstructscm2(make_cell__(TSTRING, a->length, a->rdc), g_cells), g_cells);
}

struct scm* symbol_to_keyword(SCM symbol)
{
	struct scm* a = Getstructscm2(symbol, g_cells);
	return good2bad(Getstructscm2(make_cell__(TKEYWORD, a->length, a->rdc), g_cells), g_cells);
}

struct scm* keyword_to_string(SCM keyword)
{
	struct scm* a = Getstructscm2(keyword, g_cells);
	return good2bad(Getstructscm2(make_cell__(TSTRING, a->length, a->rdc), g_cells), g_cells);
}

struct scm* string_to_symbol(SCM string)
{
	struct scm* x = bad2good(hash_ref(g_symbols, string, cell_f), g_cells);

	if(x == &g_cells[cell_f])
	{
		x = bad2good(make_symbol(string), g_cells);
	}

	return good2bad(x, g_cells);
}

struct scm* make_symbol(SCM string)
{
	struct scm* x = Getstructscm2(make_cell__(TSYMBOL, g_cells[string].length, g_cells[string].rdc), g_cells);
	hash_set_x(g_symbols, string, GetSCM2(x,g_cells));
	return good2bad(x, g_cells);
}

struct scm* bytes_to_list(char const* s)
{
	SCM i = strlen(s);
	struct scm* p = &g_cells[cell_nil];

	while(0 != i)
	{
		i = i - 1;
		int c = (0xFF & s[i]);
		p = Getstructscm2(cons(make_cell__ (TCHAR, 0, c), GetSCM2(p, g_cells)), g_cells);
	}

	return p;
}

struct scm* cstring_to_symbol(char const *s)
{
	SCM string = GetSCM2(bad2good(make_string(s, strlen (s)), g_cells), g_cells);
	return bad2good(string_to_symbol(string), g_cells);
}

struct scm* string_to_list(SCM string)
{
	return good2bad(bytes_to_list((char*)&g_cells[g_cells[string].rdc].rdc), g_cells);
}

struct scm* list_to_string(SCM list)
{
	int size;
	char const *s = list_to_cstring(good2bad(Getstructscm2(list, g_cells), g_cells), &size);
	return make_string(s, size);
}

struct scm* read_string(SCM port)  ///((arity . n))
{
	int fd = __stdin;
	struct scm* x = Getstructscm2(port, g_cells);

	if(x->type == TPAIR && x->car->type == TNUMBER)
	{
		__stdin = x->car->rdc;
	}

	int c = readchar();
	int i = 0;

	while(EOF != c)
	{
		assert_max_string(i, "read_string", g_buf);

		g_buf[i] = c;
		i = i + 1;
		c = readchar();
	}

	g_buf[i] = 0;
	__stdin = fd;
	return make_string(g_buf, i);
}

struct scm* string_append(SCM x)  ///((arity . n))
{
	char *p = g_buf;
	g_buf[0] = 0;
	int size = 0;
	struct scm* y1 = Getstructscm2(x, g_cells);

	while(y1 != &g_cells[cell_nil])
	{
		struct scm* y2 = bad2good(y1->car, g_cells);
		assert(y2->type == TSTRING);
		memcpy(p, &bad2good(y2->cdr, g_cells)->rdc, y2->rac + 1);
		p += y2->length;
		size += y2->length;

		assert_max_string(size, "string_append", g_buf);

		y1 = bad2good(y1->cdr, g_cells);
	}

	return make_string(g_buf, size);
}

struct scm* string_length(SCM string)
{
	struct scm* x = Getstructscm2(string, g_cells);
	assert(x->type == TSTRING);
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, x->length), g_cells), g_cells);
}

struct scm* string_ref(SCM str, SCM k)
{
	struct scm* x = Getstructscm2(str, g_cells);
	struct scm* y = Getstructscm2(k, g_cells);
	assert(x->type == TSTRING);
	assert(y->type == TNUMBER);
	size_t size = x->length;
	size_t i = y->rdc;

	if(i > size)
	{
		error(cell_symbol_system_error, cons(GetSCM2(bad2good(make_string("value out of range", strlen ("value out of range")), g_cells), g_cells), k));
	}

	char const *p = (char*) &bad2good(x->cdr,g_cells)->string;
	return good2bad(Getstructscm2(make_cell__ (TCHAR, 0, p[i]), g_cells), g_cells);
}
