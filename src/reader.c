/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of Mes.
 *
 * Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <ctype.h>
#include <stdlib.h>
#include "mes.h"

SCM read_input_file_env_ (SCM e, SCM a)
{
	if (e == cell_nil) return e;
	return cons(e, read_input_file_env_ (read_env (a), a));
}

SCM read_input_file_env (SCM a)
{
	r0 = a;
	return read_input_file_env_ (read_env (r0), r0);
}

int reader_read_line_comment ()
{
	int c = fgetc(g_stdin);
	while(EOF != c)
	{
		if (c == '\n') return c;
		c = fgetc(g_stdin);
	}

	/* Never should have gotten here */
	fprintf(stderr, "Never should have reached this error\nreader line 46\n");
	exit(EXIT_FAILURE);
}

SCM reader_read_block_comment (int s, int c);
SCM reader_read_hash (int c, SCM a);
SCM reader_read_list (int c, SCM a);

int reader_identifier_p (int c)
{
	return (c > ' ' && c <= '~' && c != '"' && c != ';' && c != '(' && c != ')' && c != EOF);
}

int reader_end_of_word_p (int c)
{
	return (c == '"' || c == ';' || c == '(' || c == ')' || isspace (c) || c == EOF);
}

SCM process_number(int c)
{
	int n = 0;
	bool negative_p = false;
	if(c == '+')
	{
		c = fgetc(g_stdin);
	}
	if(c == '-')
	{
		negative_p = true;
		c = fgetc(g_stdin);
	}

	do
	{
		n = n * 10;
		n = n + (c - '0');
		c = fgetc(g_stdin);
	}while (isdigit(c));
	ungetc(c, g_stdin);

	if (negative_p) n = 0 - n;

	return MAKE_NUMBER(n);
}

SCM reader_read_identifier_or_number (int c)
{
	char buf[MAX_STRING];
	int i = 0;

	if (((c == '+') || (c == '-')) && isdigit (fpeek(g_stdin)))
	{
		return process_number(c);
	}

	do
	{
		buf[i] = c;
		i = i + 1;
		c = fgetc(g_stdin);
	}while (!reader_end_of_word_p (c));
	ungetc(c,g_stdin);
	buf[i] = 0;
	return lookup_symbol_(cstring_to_list(buf));
}

SCM reader_read_string ();
SCM reader_read_sexp_ (int c, SCM a)
{
reset_reader:
	if(c == EOF) return cell_nil;
	if(c == ';')
	{
		c = reader_read_line_comment ();
		goto reset_reader;
	}
	if((c == ' ') || (c == '\t') || (c == '\n') || (c == '\f'))
	{
		c = fgetc(g_stdin);
		goto reset_reader;
	}
	if(c == '(') return reader_read_list (fgetc(g_stdin), a);
	if(c == ')') return cell_nil;
	if(c == '#') return reader_read_hash (fgetc(g_stdin), a);
	if(c == '`') return cons (cell_symbol_quasiquote, cons(reader_read_sexp_(fgetc(g_stdin), a), cell_nil));
	if(c == ',')
	{
		if (fpeek(g_stdin) == '@')
		{
			fgetc(g_stdin);
			return cons(cell_symbol_unquote_splicing, cons(reader_read_sexp_(fgetc(g_stdin), a), cell_nil));
		}

		return cons (cell_symbol_unquote, cons(reader_read_sexp_(fgetc(g_stdin), a), cell_nil));
	}
	if(c == '\'') return cons (cell_symbol_quote, cons(reader_read_sexp_(fgetc(g_stdin), a), cell_nil));
	if(c == '"') return reader_read_string ();
	if(c == '.' && (!reader_identifier_p(fpeek(g_stdin)))) return cell_dot;

	return reader_read_identifier_or_number (c);
}

int reader_eat_whitespace (int c)
{
	while (isspace (c)) c = fgetc(g_stdin);

	if (c == ';') return reader_eat_whitespace(reader_read_line_comment());
	if (c == '#' && (fpeek(g_stdin) == '!' || fpeek(g_stdin) == '|'))
	{
		c=fgetc(g_stdin);
		reader_read_block_comment (c, fgetc(g_stdin));
		return reader_eat_whitespace (fgetc(g_stdin));
	}
	return c;
}

SCM reader_read_list (int c, SCM a)
{
	c = reader_eat_whitespace (c);
	if (c == ')') return cell_nil;
	if (c == EOF) error (cell_symbol_not_a_pair, MAKE_STRING (cstring_to_list("EOF in list")));

	SCM s = reader_read_sexp_ (c, a);
	if (s == cell_dot) return CAR(reader_read_list(fgetc(g_stdin), a));

	return cons(s, reader_read_list(fgetc(g_stdin), a));
}

SCM read_env (SCM a)
{
	return reader_read_sexp_ (fgetc(g_stdin), a);
}

SCM reader_read_block_comment (int s, int c)
{
	if (c == s && fpeek(g_stdin) == '#') return fgetc(g_stdin);
	return reader_read_block_comment (s, fgetc(g_stdin));
}


SCM reader_read_binary();
SCM reader_read_octal();
SCM reader_read_hex();
SCM reader_read_character();
SCM reader_read_hash (int c, SCM a)
{
	if(c == '!')
	{
		reader_read_block_comment (c, fgetc(g_stdin));
		return reader_read_sexp_ (fgetc(g_stdin), a);
	}
	if(c == '|')
	{
		reader_read_block_comment (c, fgetc(g_stdin));
		return reader_read_sexp_ (fgetc(g_stdin), a);
	}
	if(c == 'f') return cell_f;
	if(c == 't') return cell_t;
	if(c == ',')
	{
		if (fpeek(g_stdin) == '@')
		{
			fgetc(g_stdin);
			return cons (cell_symbol_unsyntax_splicing, cons (reader_read_sexp_ (fgetc(g_stdin), a), cell_nil));
		}

		return cons (cell_symbol_unsyntax, cons (reader_read_sexp_ (fgetc(g_stdin), a), cell_nil));
	}
	if(c == '\'') return cons (cell_symbol_syntax, cons(reader_read_sexp_(fgetc(g_stdin), a), cell_nil));
	if(c == '`') return cons (cell_symbol_quasisyntax, cons(reader_read_sexp_(fgetc(g_stdin), a), cell_nil));
	if(c == ':') return MAKE_KEYWORD(CAR(reader_read_sexp_(fgetc(g_stdin), a)));
	if(c == 'b') return reader_read_binary ();
	if(c == 'o') return reader_read_octal ();
	if(c == 'x') return reader_read_hex ();
	if(c == '\\') return reader_read_character();
	if(c == '(') return list_to_vector (reader_read_list (fgetc(g_stdin), a));
	if(c == ';')
	{
		reader_read_sexp_ (fgetc(g_stdin), a);
		return reader_read_sexp_ (fgetc(g_stdin), a);
	}

	return reader_read_sexp_ (fgetc(g_stdin), a);
}

SCM reader_read_character ()
{
	int c = fgetc(g_stdin);
	int p = fpeek(g_stdin);
	int i = 0;
	if (c >= '0' && c <= '7' && p >= '0' && p <= '7')
	{
		c = c - '0';
		while (p >= '0' && p <= '7')
		{
			c = c << 3;
			c = c + (fgetc(g_stdin) - '0');
			p = fpeek(g_stdin);
		}
	}
	else if (((c >= 'a' && c <= 'z') || c == '*') && ((p >= 'a' && p <= 'z') || p == '*'))
	{
		char buf[10];
		buf[i] = c;
		i = i + 1;
		while ((p >= 'a' && p <= 'z') || p == '*')
		{
			buf[i] = fgetc(g_stdin);
			i = i + 1;
			p  = fpeek(g_stdin);
		}
		buf[i] = 0;
		if (match(buf, "*eof*")) c = EOF;
		else if (match(buf, "nul")) c = '\0';
		else if (match(buf, "alarm")) c = '\a';
		else if (match(buf, "backspace")) c = '\b';
		else if (match(buf, "tab")) c = '\t';
		else if (match(buf, "linefeed")) c = '\n';
		else if (match(buf, "newline")) c = '\n';
		else if (match(buf, "vtab")) c = '\v';
		else if (match(buf, "page")) c = '\f';
		else if (match(buf, "return")) c = '\r';
		else if (match(buf, "esc")) c = '\e';
		else if (match(buf, "space")) c = ' ';
		else if (match(buf, "bel")) c = '\a';
		else if (match(buf, "bs")) c = '\b';
		else if (match(buf, "ht")) c = '\t';
		else if (match(buf, "vt")) c = '\v';
		else if (match(buf, "cr")) c = '\r';
		else
		{
			fprintf(stderr, "char not supported: %s\n", buf);
			exit(EXIT_FAILURE);
		}
	}

	return MAKE_CHAR(c);
}

SCM reader_read_binary ()
{
	int n = 0;
	int c = fpeek(g_stdin);
	int s = 1;
	if (c == '-')
	{
		s = -1;
		fgetc(g_stdin);
		c = fpeek(g_stdin);
	}

	while (c == '0' || c == '1')
	{
		n = n << 1;
		n = n + c - '0';
		fgetc(g_stdin);
		c = fpeek(g_stdin);
	}
	return MAKE_NUMBER (s*n);
}

SCM reader_read_octal ()
{
	int n = 0;
	int c = fpeek(g_stdin);
	int s = 1;
	if (c == '-')
	{
		s = -1;
		fgetc(g_stdin);
		c = fpeek(g_stdin);
	}

	while (c >= '0' && c <= '7')
	{
		n = n << 3;
		n = n + c - '0';
		fgetc(g_stdin);
		c = fpeek(g_stdin);
	}
	return MAKE_NUMBER (s*n);
}

SCM reader_read_hex ()
{
	int n = 0;
	int c = fpeek(g_stdin);
	int s = 1;
	if (c == '-')
	{
		s = -1;
		fgetc(g_stdin);
		c = fpeek(g_stdin);
	}

	while ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))
	{
		n = n << 4;
		if (c >= 'a') n += c - 'a' + 10;
		else if (c >= 'A') n += c - 'A' + 10;
		else n+= c - '0';
		fgetc(g_stdin);
		c = fpeek(g_stdin);
	}
	return MAKE_NUMBER (s*n);
}

SCM reader_read_string ()
{
	SCM lst = cell_nil;
	int c;
	do
	{
		c = fgetc(g_stdin);
		if (c == '"') break;
		if (c == '\\')
		{
			c = fgetc(g_stdin);
			if (c == '\\' || c == '"') cons(MAKE_CHAR(c), lst);
			else if (c == '0') cons(MAKE_CHAR('\0'), lst);
			else if (c == 'a') cons(MAKE_CHAR('\a'), lst);
			else if (c == 'b') cons(MAKE_CHAR('\b'), lst);
			else if (c == 't') cons(MAKE_CHAR('\t'), lst);
			else if (c == 'n') cons(MAKE_CHAR('\n'), lst);
			else if (c == 'v') cons(MAKE_CHAR('\v'), lst);
			else if (c == 'f') cons(MAKE_CHAR('\f'), lst);
			else if (c == 'r') cons(MAKE_CHAR('\r'), lst);
			else if (c == 'e') cons(MAKE_CHAR('\e'), lst);
		}
		else cons(MAKE_CHAR(c), lst);
	} while(true);
	return MAKE_STRING (reverse_x_(lst, cell_nil));
}

int dump ()
{
	r1 = g_symbols;
	gc_push_frame ();
	gc();
	gc_peek_frame ();
	char *p = (char*)g_cells;
	fprintf(stdout, "MES%c%c dumping\n", (g_stack >> 8), (g_stack % 256));

	if (g_debug > 1)
	{
		fprintf(stderr,"program r2=");
		display_error_ (r2);
		fprintf(stderr,"\n");
	}

	int i = 0;
	for (; i < g_free; i = i + 1)
	{
		fputc(p[0], stderr);
		p = p + 1;
	}
	return 0;
}
