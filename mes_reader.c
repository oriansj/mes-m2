/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2018 Jeremiah Orians <jeremiah@pdp10.guru>
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
#include <ctype.h>
#include <stdio.h>

/* M2-Planet function imports*/
int in_set(int c, char* s);
int numerate_string(char *a);

/* Standard Mes.c function imports */
void assert_max_string(int i, char const* msg, char* string);
struct scm* read_env (SCM a);
SCM cons (SCM x, SCM y);
int readchar();
struct scm* error(SCM key, SCM x);
struct scm* make_string_(char const* s);
int peekchar();
int unreadchar();
struct scm* make_cell__(long type, SCM car, SCM cdr);
struct scm* cstring_to_symbol(char const *s);
struct scm* symbol_to_keyword (SCM symbol);
struct scm* list_to_vector (SCM x);
int eputs(char const* s);
char* itoa (int number);

struct scm* read_input_file_env_(SCM e, SCM a)
{
	if(e == cell_nil)
	{
		return good2bad(Getstructscm2(cell_nil, g_cells), g_cells);
	}

	return good2bad(Getstructscm2(cons(e, GetSCM(read_input_file_env_(GetSCM(read_env(a)), a))), g_cells), g_cells);
}

struct scm* read_input_file_env()
{
	return read_input_file_env_(GetSCM(read_env(cell_nil)), cell_nil);
}

int reader_read_line_comment(int c)
{
	while(c != EOF)
	{
		if(c == '\n') return c;
		c = readchar();
	}

	error(cell_symbol_system_error, GetSCM2(make_string_("reader_read_line_comment"), g_cells));
	exit(EXIT_FAILURE);
}

int reader_end_of_word_p(int c)
{
	return in_set(c, "\";() \t\n\r");
}

struct scm* reader_read_identifier_or_number(int c)
{
	int i = 0;

	/* Fallthrough: Note that `+', `-', `4a', `+1b' are identifiers */
	while(!reader_end_of_word_p(c))
	{
		g_buf[i++] = c;
		c = readchar();
	}

	unreadchar(c);
	g_buf[i] = 0;

	SCM number = numerate_string(g_buf);

	if((0 != number || '0' == g_buf[0]))
	{
		return make_cell__ (TNUMBER, 0, number);
	}
	return good2bad(cstring_to_symbol(g_buf), g_cells);
}

struct scm* reader_read_hash(int c, SCM a);
struct scm* reader_read_list(int c, SCM a);
struct scm* reader_read_string ();

struct scm* reader_read_sexp_(int c, SCM a)
{
reset_reader:

	if(c == EOF)
	{
		return Getstructscm2(cell_nil, g_cells);
	}

	if(c == ';')
	{
		c = reader_read_line_comment(c);
		goto reset_reader;
	}

	if(in_set(c, " \t\n\f"))
	{
		c = readchar();
		goto reset_reader;
	}

	if(c == '(')
	{
		return reader_read_list(readchar(), a);
	}

	if(c == ')')
	{
		return Getstructscm2(cell_nil, g_cells);
	}

	if(c == '#')
	{
		return bad2good(reader_read_hash(readchar(), a), g_cells);
	}

	if(c == '`')
	{
		return Getstructscm2(cons(cell_symbol_quasiquote, cons(GetSCM2(reader_read_sexp_(readchar(), a), g_cells), cell_nil)), g_cells);
	}

	if(c == ',')
	{
		if(peekchar() == '@')
		{
			readchar();
			return Getstructscm2(cons(cell_symbol_unquote_splicing, cons(GetSCM2(reader_read_sexp_(readchar(), a), g_cells), cell_nil)), g_cells);
		}

		return Getstructscm2(cons(cell_symbol_unquote, cons(GetSCM2(reader_read_sexp_(readchar(), a), g_cells), cell_nil)), g_cells);
	}

	if(c == '\'')
	{
		return Getstructscm2(cons(cell_symbol_quote, cons(GetSCM2(reader_read_sexp_(readchar(), a), g_cells), cell_nil)), g_cells);
	}

	if(c == '"')
	{
		return bad2good(reader_read_string(), g_cells);
	}

	if(c == '.' && (reader_end_of_word_p(peekchar())))
	{
		return Getstructscm2(cell_dot, g_cells);
	}

	return bad2good(reader_read_identifier_or_number(c), g_cells);
}

int reader_read_block_comment(int s, int c)
{
	if(c == s && peekchar() == '#')
	{
		return readchar();
	}

	return reader_read_block_comment(s, readchar());
}

int reader_eat_whitespace(int c)
{
	while(isspace(c))
	{
		c = readchar();
	}

	if(c == ';')
	{
		return reader_eat_whitespace(reader_read_line_comment(c));
	}

	if(c == '#' && (peekchar() == '!' || peekchar() == '|'))
	{
		c = readchar();
		reader_read_block_comment(c, readchar());
		return reader_eat_whitespace(readchar());
	}

	return c;
}

struct scm* reader_read_list(int c, SCM a)
{
	c = reader_eat_whitespace(c);

	if(c == ')')
	{
		return Getstructscm2(cell_nil, g_cells);
	}

	if(c == EOF)
	{
		error(cell_symbol_not_a_pair, GetSCM2(make_string_("EOF in list"), g_cells));
	}

	//return cell_nil;
	struct scm* s = reader_read_sexp_(c, a);

	if(s == &g_cells[cell_dot])
	{
		s = reader_read_list(readchar(), a);
		return bad2good(s->car, g_cells);
	}

	return Getstructscm2(cons(GetSCM2(s, g_cells), GetSCM2(reader_read_list(readchar(), a), g_cells)), g_cells);
}

struct scm* read_env(SCM a)
{
	return good2bad(reader_read_sexp_(readchar(), a), g_cells);
}

int index_number__(char* s, char c) /* Internal only */
{
	int i = 0;
	while(s[i] != c)
	{
		i = i + 1;
	}
	return i;
}

struct scm* set_reader__(char* set, int mult) /* Internal only*/
{
	long n = 0;
	int c = peekchar();
	int negative_p = 0;

	if(c == '-')
	{
		negative_p = 1;
		readchar();
		c = peekchar();
	}

	while(in_set(c, set))
	{
		n = n * mult;
		n = n + index_number__(set, toupper(c));
		readchar();
		c = peekchar();
	}

	if(negative_p)
	{
		n = 0 - n;
	}

	return make_cell__ (TNUMBER, 0, n);
}

struct scm* reader_read_binary()
{
	return set_reader__("01", 2);
}

struct scm* reader_read_octal()
{
	return set_reader__("01234567", 8);
}

struct scm* reader_read_hex()
{
	return set_reader__("0123456789ABCDEFabcdef", 16);
}

struct scm* reader_read_character ();

struct scm* reader_read_hash(int c, SCM a)
{
	if(c == '!')
	{
		reader_read_block_comment(c, readchar());
		return good2bad(reader_read_sexp_(readchar(), a), g_cells);
	}

	if(c == '|')
	{
		reader_read_block_comment(c, readchar());
		return good2bad(reader_read_sexp_(readchar(), a), g_cells);
	}

	if(c == 'f')
	{
		return Getstructscm(cell_f);
	}

	if(c == 't')
	{
		return Getstructscm(cell_t);
	}

	if(c == ',')
	{
		if(peekchar() == '@')
		{
			readchar();
			return Getstructscm(cons(cell_symbol_unsyntax_splicing, cons(GetSCM2(reader_read_sexp_(readchar(), a), g_cells), cell_nil)));
		}

		return Getstructscm(cons(cell_symbol_unsyntax, cons(GetSCM2(reader_read_sexp_(readchar(), a), g_cells), cell_nil)));
	}

	if(c == '\'')
	{
		return Getstructscm(cons(cell_symbol_syntax, cons(GetSCM2(reader_read_sexp_(readchar(), a), g_cells), cell_nil)));
	}

	if(c == '`')
	{
		return Getstructscm(cons(cell_symbol_quasisyntax, cons(GetSCM2(reader_read_sexp_(readchar(), a), g_cells), cell_nil)));
	}

	if(c == ':')
	{
		struct scm* x = bad2good(reader_read_identifier_or_number(readchar()), g_cells);

		if(x->type == TNUMBER)
		{ /* READ error */
			error(cell_symbol_system_error, cons(GetSCM2(make_string_("keyword perifx ':' not followed by a symbol: "), g_cells), GetSCM2(x, g_cells)));
		}

		return symbol_to_keyword(GetSCM2(x, g_cells));
	}

	if(c == 'b')
	{
		return reader_read_binary();
	}

	if(c == 'o')
	{
		return reader_read_octal();
	}

	if(c == 'x')
	{
		return reader_read_hex();
	}

	if(c == '\\')
	{
		return reader_read_character();
	}

	if(c == '(')
	{
		return list_to_vector(GetSCM2(reader_read_list(readchar(), a), g_cells));
	}

	if(c == ';')
	{
		reader_read_sexp_(readchar(), a);
		return good2bad(reader_read_sexp_(readchar(), a), g_cells);
	}

	return good2bad(reader_read_sexp_(readchar(), a), g_cells);
}

struct scm* reader_read_sexp(SCM c, SCM a)
{
	struct scm* x = Getstructscm2(c, g_cells);
	return good2bad(reader_read_sexp_(x->value, a), g_cells);
}

struct scm* reader_read_character()
{
	int c = readchar();
	int p = peekchar();
	int i = 0;

	if(in_set(c, "01234567") && in_set(p, "01234567"))
	{
		c = c - '0';

		while(in_set(p, "01234567"))
		{
			c <<= 3;
			c += readchar() - '0';
			p = peekchar();
		}
	}
	else if(c == 'x' && in_set(p, "01234567abcdefABCDEF"))
	{
		c = bad2good(reader_read_hex(), g_cells)->value;
		eputs("reading hex c=");
		eputs(itoa(c));
		eputs("\n");
	}
	else if(in_set(c, "abcdefghijklmnopqrstuvwxyz*") && in_set(p, "abcdefghijklmnopqrstuvwxyz*"))
	{
		char buf[10];
		buf[i] = c;
		i = i + 1;

		while(in_set(p, "abcdefghijklmnopqrstuvwxyz*"))
		{
			buf[i] = readchar();
			i = i + 1;
			p = peekchar();
		}

		buf[i] = 0;

		if(!strcmp(buf, "*eof*"))
		{
			c = EOF;
		}
		else if(!strcmp(buf, "nul"))
		{
			c = '\0';
		}
		else if(!strcmp(buf, "alarm"))
		{
			c = '\a';
		}
		else if(!strcmp(buf, "backspace"))
		{
			c = '\b';
		}
		else if(!strcmp(buf, "tab"))
		{
			c = '\t';
		}
		else if(!strcmp(buf, "linefeed"))
		{
			c = '\n';
		}
		else if(!strcmp(buf, "newline"))
		{
			c = '\n';
		}
		else if(!strcmp(buf, "vtab"))
		{
			c = '\v';
		}
		else if(!strcmp(buf, "page"))
		{
			c = '\f';
		}
		else if(!strcmp(buf, "return"))
		{
			c = '\r';
		}
		else if(!strcmp(buf, "esc"))
		{
			c = '\e';
		}
		else if(!strcmp(buf, "space"))
		{
			c = ' ';
		}
		else if(!strcmp(buf, "bel"))
		{
			c = '\a';
		}
		else if(!strcmp(buf, "bs"))
		{
			c = '\b';
		}
		else if(!strcmp(buf, "ht"))
		{
			c = '\t';
		}
		else if(!strcmp(buf, "vt"))
		{
			c = '\v';
		}
		else if(!strcmp(buf, "cr"))
		{
			c = '\r';
		}
		else
		{
			eputs("char not supported: ");
			eputs(buf);
			eputs("\n");
			error(cell_symbol_system_error, GetSCM2(make_string_("char not supported"), g_cells));
		}
	}

	return make_cell__ (TCHAR, 0, c);
}

int escape_lookup(int c)
{
	if(c == '\\' || c == '"') return c;
	else if(c == '0') return '\0';
	else if(c == 'a') return '\a';
	else if(c == 'b') return '\b';
	else if(c == 't') return '\t';
	else if(c == 'n') return '\n';
	else if(c == 'v') return '\v';
	else if(c == 'f') return '\f';
	else if(c == 'r') return '\r';
	else if(c == 'e') return '\e';
	else if(c == 'x') return bad2good(reader_read_hex(), g_cells)->value;

	/* Deal with really bad input */
	fprintf(stderr, "Unknown escape recieved: %c Unable to process\n", c);
	exit(EXIT_FAILURE);
}

struct scm* reader_read_string()
{
	int i = 0;
	int c;

	do
	{
		if(i > MAX_STRING)
		{
			assert_max_string(i, "reader_read_string", g_buf);
		}

		c = readchar();

		if(c == '"')
		{
			break;
		}

		if(c == '\\')
		{
			c = escape_lookup(readchar());
		}

		g_buf[i++] = c;
	} while(1);

	g_buf[i] = 0;
	return good2bad(make_string_(g_buf), g_cells);
}
