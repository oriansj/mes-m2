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


char* itoa(int number);
int eputs(char* s);
int in_set(int c, char* s);
int match(char* a, char* b);
int numerate_string(char* a);
int peekchar();
int readchar();
int toupper(int c);
int unreadchar();
struct scm* cstring_to_symbol(char* s);
struct scm* error_(struct scm* key, struct scm* x);
struct scm* list_to_vector_(struct scm* x);
struct scm* make_char(SCM c);
struct scm* make_number_(SCM n);
struct scm* make_string(char* s, int length);
struct scm* make_string_(char* s);
struct scm* make_tpair(struct scm* a, struct scm* b);
struct scm* reader_read_sexp_(int c, struct scm* a);
struct scm* symbol_to_keyword_(struct scm* symbol);
void assert_max_string(int i, char* msg, char* string);
void require(int bool, char* error);

struct scm* read_env_(struct scm* a) /* Internal */
{
	return reader_read_sexp_(readchar(), a);
}

struct scm* read_env(struct scm* x) /* External */
{
	return read_env_(x->car);
}

struct scm* read_input_file_env_(struct scm* e, struct scm* a) /* Internal */
{
	if(e == cell_nil)
	{
		return cell_nil;
	}

	return make_tpair(e, read_input_file_env_(read_env_(a), a));
}

struct scm* read_input_file_env(struct scm* x) /* External */
{
	return read_input_file_env_(x->car, x->cdr->car);
}

struct scm* read_input_file_env__() /* Internal */
{
	return read_input_file_env_(read_env_(cell_nil), cell_nil);
}

int reader_read_line_comment(int c)
{
	while(c != EOF)
	{
		if(c == '\n') return c;
		c = readchar();
	}

	error_(cell_symbol_system_error, make_string_("reader_read_line_comment"));
	exit(EXIT_FAILURE);
}

int reader_end_of_word_p(int c)
{
	return in_set(c, "\";() \t\n\r") || c == EOF;
}

struct scm* reader_read_identifier_or_number(int c)
{
	int i = 0;

	/* Fallthrough: Note that `+', `-', `4a', `+1b' are identifiers */
	while(!reader_end_of_word_p(c))
	{
		g_buf[i] = c;
		i = i + 1;
		c = readchar();
	}

	unreadchar(c);
	g_buf[i] = 0;

	SCM number = numerate_string(g_buf);

	if((0 != number || '0' == g_buf[0]))
	{
		return make_number_(number);
	}
	return cstring_to_symbol(g_buf);
}

struct scm* reader_read_hash(int c, struct scm* a);
struct scm* reader_read_list(int c, struct scm* a);
struct scm* reader_read_string_();

struct scm* reader_read_sexp_(int c, struct scm* a) /* Internal */
{
reset_reader:

	if(c == EOF)
	{
		return cell_nil;
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
		return cell_nil;
	}

	if(c == '#')
	{
		return reader_read_hash(readchar(), a);
	}

	if(c == '`')
	{
		return make_tpair(cell_symbol_quasiquote, make_tpair(reader_read_sexp_(readchar(), a), cell_nil));
	}

	if(c == ',')
	{
		if(peekchar() == '@')
		{
			readchar();
			return make_tpair(cell_symbol_unquote_splicing, make_tpair(reader_read_sexp_(readchar(), a), cell_nil));
		}

		return make_tpair(cell_symbol_unquote, make_tpair(reader_read_sexp_(readchar(), a), cell_nil));
	}

	if(c == '\'')
	{
		return make_tpair(cell_symbol_quote, make_tpair(reader_read_sexp_(readchar(), a), cell_nil));
	}

	if(c == '"')
	{
		return reader_read_string_();
	}

	if(c == '.' && (reader_end_of_word_p(peekchar())))
	{
		return cell_dot;
	}

	return reader_read_identifier_or_number(c);
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
	while(in_set(c, " \t\n"))
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

struct scm* reader_read_list(int c, struct scm* a)
{
	c = reader_eat_whitespace(c);

	if(c == ')')
	{
		return cell_nil;
	}

	if(c == EOF)
	{
		error_(cell_symbol_not_a_pair, make_string_("EOF in list"));
	}

	//return cell_nil;
	struct scm* s = reader_read_sexp_(c, a);

	if(s == cell_dot)
	{
		s = reader_read_list(readchar(), a);
		return s->car;
	}

	return make_tpair(s, reader_read_list(readchar(), a));
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
	SCM n = 0;
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

	return make_number_(n);
}

struct scm* reader_read_binary(struct scm* x) /* External */
{
	require(cell_nil == x, "mes_reader.c: reader_read_binary recieved arguments\n");
	return set_reader__("01", 2);
}

struct scm* reader_read_octal(struct scm* x) /* External */
{
	require(cell_nil == x, "mes_reader.c: reader_read_octal recieved arguments\n");
	return set_reader__("01234567", 8);
}

struct scm* reader_read_hex(struct scm* x) /* External */
{
	require(cell_nil == x, "mes_reader.c: reader_read_hex recieved arguments\n");
	return set_reader__("0123456789ABCDEFabcdef", 16);
}

struct scm* reader_read_character_();

struct scm* reader_read_hash(int c, struct scm* a)
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
		return cell_f;
	}

	if(c == 't')
	{
		return cell_t;
	}

	if(c == ',')
	{
		if(peekchar() == '@')
		{
			readchar();
			return make_tpair(cell_symbol_unsyntax_splicing, make_tpair(reader_read_sexp_(readchar(), a), cell_nil));
		}

		return make_tpair(cell_symbol_unsyntax, make_tpair(reader_read_sexp_(readchar(), a), cell_nil));
	}

	if(c == '\'')
	{
		return make_tpair(cell_symbol_syntax, make_tpair(reader_read_sexp_(readchar(), a), cell_nil));
	}

	if(c == '`')
	{
		return make_tpair(cell_symbol_quasisyntax, make_tpair(reader_read_sexp_(readchar(), a), cell_nil));
	}

	if(c == ':')
	{
		struct scm* x = reader_read_identifier_or_number(readchar());

		if(x->type == TNUMBER)
		{ /* READ error */
			error_(cell_symbol_system_error, make_tpair(make_string_("keyword perifx ':' not followed by a symbol: "), x));
		}

		return symbol_to_keyword_(x);
	}

	if(c == 'b')
	{
		return set_reader__("01", 2);
	}

	if(c == 'o')
	{
		return set_reader__("01234567", 8);
	}

	if(c == 'x')
	{
		return set_reader__("0123456789ABCDEFabcdef", 16);
	}

	if(c == '\\')
	{
		return reader_read_character_();
	}

	if(c == '(')
	{
		return list_to_vector_(reader_read_list(readchar(), a));
	}

	if(c == ';')
	{
		reader_read_sexp_(readchar(), a);
		return reader_read_sexp_(readchar(), a);
	}

	return reader_read_sexp_(readchar(), a);
}

struct scm* reader_read_sexp(struct scm* x) /* External */
{
	struct scm* c = x->car;
	struct scm* a = x->cdr->car;
	return reader_read_sexp_(c->value, a);
}

struct scm* reader_read_character_() /* Internal */
{
	int c = readchar();
	int p = peekchar();
	int i = 0;
	struct scm* x;

	if(in_set(c, "01234567") && in_set(p, "01234567"))
	{
		c = c - '0';

		while(in_set(p, "01234567"))
		{
			c = c << 3;
			c = c + readchar() - '0';
			p = peekchar();
		}
	}
	else if(c == 'x' && in_set(p, "01234567abcdefABCDEF"))
	{
		x = set_reader__("0123456789ABCDEFabcdef", 16);
		c = x->value;
		eputs("reading hex c=");
		eputs(itoa(c));
		eputs("\n");
	}
	else if(in_set(c, "abcdefghijklmnopqrstuvwxyz*") && in_set(p, "abcdefghijklmnopqrstuvwxyz*"))
	{
		char* buf = reader_buf;
		buf[i] = c;
		i = i + 1;

		while(in_set(p, "abcdefghijklmnopqrstuvwxyz*"))
		{
			buf[i] = readchar();
			i = i + 1;
			p = peekchar();
		}

		buf[i] = 0;

		if(match(buf, "*eof*"))
		{
			c = EOF;
		}
		else if(match(buf, "nul"))
		{
			c = '\0';
		}
		else if(match(buf, "alarm"))
		{
			c = '\a';
		}
		else if(match(buf, "backspace"))
		{
			c = '\b';
		}
		else if(match(buf, "tab"))
		{
			c = '\t';
		}
		else if(match(buf, "linefeed"))
		{
			c = '\n';
		}
		else if(match(buf, "newline"))
		{
			c = '\n';
		}
		else if(match(buf, "vtab"))
		{
			c = '\v';
		}
		else if(match(buf, "page"))
		{
			c = '\f';
		}
		else if(match(buf, "return"))
		{
			c = '\r';
		}
		else if(match(buf, "esc"))
		{
			c = '\e';
		}
		else if(match(buf, "space"))
		{
			c = ' ';
		}
		else if(match(buf, "bel"))
		{
			c = '\a';
		}
		else if(match(buf, "bs"))
		{
			c = '\b';
		}
		else if(match(buf, "ht"))
		{
			c = '\t';
		}
		else if(match(buf, "vt"))
		{
			c = '\v';
		}
		else if(match(buf, "cr"))
		{
			c = '\r';
		}
		else
		{
			eputs("char not supported: ");
			eputs(buf);
			eputs("\n");
			error_(cell_symbol_system_error, make_string_("char not supported"));
		}
	}

	return make_char(c);
}

struct scm* reader_read_character(struct scm* x) /* External */
{
	require(cell_nil == x, "mes_reader.c: reader_read_character recieved arguments\n");
	return reader_read_character_();
}

int escape_lookup(int c)
{

	if(c == '0') return '\0';
	else if(c == 'a') return '\a';
	else if(c == 'b') return '\b';
	else if(c == 't') return '\t';
	else if(c == 'n') return '\n';
	else if(c == 'v') return '\v';
	else if(c == 'f') return '\f';
	else if(c == 'r') return '\r';
	else if(c == 'e') return '\e';
	else if(c == 'x')
	{
		struct scm* x = set_reader__("0123456789ABCDEFabcdef", 16);
		return x->value;
	}
	/* Any other escaped character is itself */
	else return c;
}

struct scm* reader_read_string_() /* Internal */
{
	int i = 0;
	int c;

	do
	{
		if(i > MAX_STRING)
		{
			assert_max_string(i, "reader_read_string_", g_buf);
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

		g_buf[i] = c;
		i = i + 1;
	} while(1);

	g_buf[i] = 0;
	return make_string_(g_buf);
}

struct scm* reader_read_string(struct scm* x) /* External */
{
	require(cell_nil == x, "mes_reader.c: reader_read_string recieved arguments\n");
	return reader_read_string_();
}

struct scm* read_string(struct scm* x)  /* External */
{
	int fd = __stdin;
	x = x->car;

	if(x->type == TPAIR && x->car->type == TNUMBER)
	{
		__stdin = x->car->value;
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
