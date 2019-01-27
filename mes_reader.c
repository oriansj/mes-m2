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

#define CAR(x) g_cells[x].rac
#define TYPE(x) g_cells[x].type
#define VALUE(x) g_cells[x].rdc

/* M2-Planet function imports*/
int in_set(int c, char* s);

/* Standard Mes.c function imports */
void assert_max_string(int i, char const* msg, char* string);
struct scm* read_env (SCM a);
struct scm* reader_read_binary ();
struct scm* reader_read_octal ();
struct scm* reader_read_hex ();
struct scm* reader_read_string ();
struct scm* reader_read_character ();
SCM car (SCM x);
SCM cdr (SCM x);
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
char *itoa (int number);

struct scm* read_input_file_env_(SCM e, SCM a)
{
	if(e == cell_nil)
	{
		return Getstructscm(cell_nil);
	}

	return Getstructscm(cons(e, GetSCM(read_input_file_env_(GetSCM(read_env(a)), a))));
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
	exit(1);
}

int reader_read_block_comment(int s, int c);
struct scm* reader_read_hash(int c, SCM a);
struct scm* reader_read_list(int c, SCM a);

int reader_identifier_p(int c)
{
	return in_set(c, "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!#$%&'*+,-./:<=>?@[\\]^_`{|}");
}

int reader_end_of_word_p(int c)
{
	return in_set(c, "\";() \t\n\r");
}

struct scm* reader_read_identifier_or_number(int c)
{
	int i = 0;
#define NEW 1
#if !NEW
	long n = 0;
	int negative_p = 0;

	if(c == '+' && isdigit(peekchar()))
	{
		c = readchar();
	}
	else if(c == '-' && isdigit(peekchar()))
	{
		negative_p = 1;
		c = readchar();
	}

	while(isdigit(c))
	{
		g_buf[i++] = c;
		n *= 10;
		n += c - '0';
		c = readchar();
	}

	if(reader_end_of_word_p(c))
	{
		unreadchar(c);

		if(negative_p)
		{
			n = 0 - n;
		}

		if(g_debug > 1)
		{
			eputs ("number! "); eputs (itoa (n)); eputs ("\n");
		}
		return make_cell__ (TNUMBER, 0, n);
	}
#else // NEW

	int digit = 1;
#endif
	/* Fallthrough: Note that `4a', `+1b' are identifiers */
	while(!reader_end_of_word_p(c))
	{
		g_buf[i++] = c;
#if NEW
		if(!in_set(c, "0123456789-+"))
		{
			digit = 0;
		}
#endif // NEW
		c = readchar();
	}

	unreadchar(c);
	g_buf[i] = 0;

#if NEW
	// if(g_debug > 1)
	// {
	// 	g_buf[i] = 0;
	// 	eputs ("g_buf: "); eputs (g_buf); eputs (": ");
	// }
	if(0 != digit)
	{
		int n = strtol(g_buf, NULL, 0);
		if(g_debug > 1)
		{
			eputs ("number! "); eputs (itoa (n)); eputs ("\n");
		}
		return make_cell__ (TNUMBER, 0, n);
	}
#endif

	if(g_debug > 1)
	{
		eputs ("symbol! "); eputs (g_buf); eputs ("\n");
	}
	return cstring_to_symbol(g_buf);
}

struct scm* reader_read_sexp_(int c, SCM a)
{
reset_reader:

	if(c == EOF)
	{
		return Getstructscm(cell_nil);
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
		return Getstructscm(cell_nil);
	}

	if(c == '#')
	{
		return reader_read_hash(readchar(), a);
	}

	if(c == '`')
	{
		return Getstructscm(cons(cell_symbol_quasiquote, cons(GetSCM(reader_read_sexp_(readchar(), a)), cell_nil)));
	}

	if(c == ',')
	{
		if(peekchar() == '@')
		{
			readchar();
			return Getstructscm(cons(cell_symbol_unquote_splicing, cons(GetSCM(reader_read_sexp_(readchar(), a)), cell_nil)));
		}

		return Getstructscm(cons(cell_symbol_unquote, cons(GetSCM(reader_read_sexp_(readchar(), a)), cell_nil)));
	}

	if(c == '\'')
	{
		return Getstructscm(cons(cell_symbol_quote, cons(GetSCM(reader_read_sexp_(readchar(), a)), cell_nil)));
	}

	if(c == '"')
	{
		return reader_read_string();
	}

	if(c == '.' && (!reader_identifier_p(peekchar())))
	{
		return Getstructscm(cell_dot);
	}

	return reader_read_identifier_or_number(c);
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
		return Getstructscm(cell_nil);
	}

	if(c == EOF)
	{
		error(cell_symbol_not_a_pair, GetSCM2(make_string_("EOF in list"), g_cells));
	}

	//return cell_nil;
	SCM s = GetSCM(reader_read_sexp_(c, a));

	if(s == cell_dot)
	{
		return Getstructscm(CAR(GetSCM(reader_read_list(readchar(), a))));
	}

	return Getstructscm(cons(s, GetSCM(reader_read_list(readchar(), a))));
}

struct scm* read_env(SCM a)
{
	return reader_read_sexp_(readchar(), a);
}

int reader_read_block_comment(int s, int c)
{
	if(c == s && peekchar() == '#')
	{
		return readchar();
	}

	return reader_read_block_comment(s, readchar());
}

struct scm* reader_read_hash(int c, SCM a)
{
	if(c == '!')
	{
		reader_read_block_comment(c, readchar());
		return reader_read_sexp_(readchar(), a);
	}

	if(c == '|')
	{
		reader_read_block_comment(c, readchar());
		return reader_read_sexp_(readchar(), a);
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
			return Getstructscm(cons(cell_symbol_unsyntax_splicing, cons(GetSCM(reader_read_sexp_(readchar(), a)), cell_nil)));
		}

		return Getstructscm(cons(cell_symbol_unsyntax, cons(GetSCM(reader_read_sexp_(readchar(), a)), cell_nil)));
	}

	if(c == '\'')
	{
		return Getstructscm(cons(cell_symbol_syntax, cons(GetSCM(reader_read_sexp_(readchar(), a)), cell_nil)));
	}

	if(c == '`')
	{
		return Getstructscm(cons(cell_symbol_quasisyntax, cons(GetSCM(reader_read_sexp_(readchar(), a)), cell_nil)));
	}

	if(c == ':')
	{
		SCM x = GetSCM(reader_read_identifier_or_number(readchar()));

		if(TYPE(x) == TNUMBER)
		{ /* READ error */
			error(cell_symbol_system_error, cons(GetSCM2(make_string_("keyword perifx ':' not followed by a symbol: "), g_cells), x));
		}

		return symbol_to_keyword(x);
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
		return list_to_vector(GetSCM(reader_read_list(readchar(), a)));
	}

	if(c == ';')
	{
		reader_read_sexp_(readchar(), a);
		return reader_read_sexp_(readchar(), a);
	}

	return reader_read_sexp_(readchar(), a);
}

struct scm* reader_read_sexp(SCM c, SCM a)
{
	return reader_read_sexp_(VALUE(c), a);
}

struct scm* reader_read_character()
{
	int c = readchar();
	int p = peekchar();
	int i = 0;

	if(c >= '0' && c <= '7' && p >= '0' && p <= '7')
	{
		c = c - '0';

		while(p >= '0' && p <= '7')
		{
			c <<= 3;
			c += readchar() - '0';
			p = peekchar();
		}
	}
	else if(c == 'x' && ((p >= '0' && p <= '9') || (p >= 'a' && p <= 'f') || (p >= 'F' && p <= 'F')))
	{
		c = VALUE(GetSCM(reader_read_hex()));
		eputs("reading hex c=");
		eputs(itoa(c));
		eputs("\n");
	}
	else if(((c >= 'a' && c <= 'z') || c == '*') && ((p >= 'a' && p <= 'z') || p == '*'))
	{
		char buf[10];
		buf[i] = c;
		i = i + 1;

		while((p >= 'a' && p <= 'z') || p == '*')
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

struct scm* reader_read_binary()
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

	while(c == '0' || c == '1')
	{
		n = n << 1;
		n = n + c - '0';
		readchar();
		c = peekchar();
	}

	if(negative_p)
	{
		n = 0 - n;
	}

	return make_cell__ (TNUMBER, 0, n);
}

struct scm* reader_read_octal()
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

	while(c >= '0' && c <= '7')
	{
		n = n << 3;
		n = n + c - '0';
		readchar();
		c = peekchar();
	}

	if(negative_p)
	{
		n = 0 - n;
	}

	return make_cell__ (TNUMBER, 0, n);
}

struct scm* reader_read_hex()
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

	while((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))
	{
		n = n << 4;

		if(c >= 'a')
		{
			n = n + c - 'a' + 10;
		}
		else if(c >= 'A')
		{
			n = n + c - 'A' + 10;
		}
		else
		{
			n = n + c - '0';
		}

		readchar();
		c = peekchar();
	}

	if(negative_p)
	{
		n = 0 - n;
	}

	return make_cell__ (TNUMBER, 0, n);
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
			c = readchar();

			if(c == '\\' || c == '"')
				;
			else if(c == '0')
			{
				c = '\0';
			}
			else if(c == 'a')
			{
				c = '\a';
			}
			else if(c == 'b')
			{
				c = '\b';
			}
			else if(c == 't')
			{
				c = '\t';
			}
			else if(c == 'n')
			{
				c = '\n';
			}
			else if(c == 'v')
			{
				c = '\v';
			}
			else if(c == 'f')
			{
				c = '\f';
			}
			else if(c == 'r')
			{
				c = '\r';
			}
			else if(c == 'e')
			{
				c = '\e';
			}
			else if(c == 'x')
			{
				c = VALUE(GetSCM(reader_read_hex()));
			}
		}

		g_buf[i++] = c;
	} while(1);

	g_buf[i] = 0;
	return good2bad(make_string_(g_buf), g_cells);
}
