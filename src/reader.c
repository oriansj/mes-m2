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

#include <ctype.h>

#define MAX_STRING 4096

SCM
read_input_file_env_ (SCM e, SCM a)
{
  if (e == cell_nil)
    return e;
  return cons (e, read_input_file_env_ (read_env (a), a));
}

SCM
read_input_file_env (SCM a)
{
  r0 = a;
  return read_input_file_env_ (read_env (r0), r0);
}

int
reader_read_line_comment (int c)
{
  while (c != EOF)
    {
      if (c == '\n')
        return c;
      c = readchar ();
    }
  error (cell_symbol_system_error,
         MAKE_STRING (cstring_to_list ("reader_read_line_comment")));
}

SCM reader_read_block_comment (int s, int c);
SCM reader_read_hash (int c, SCM a);
SCM reader_read_list (int c, SCM a);

int
reader_identifier_p (int c)
{
  return (c > ' ' && c <= '~' && c != '"' && c != ';' && c != '(' && c != ')' && c != EOF);
}

int
reader_end_of_word_p (int c)
{
  return (c == '"' || c == ';' || c == '(' || c == ')' || isspace (c) || c == EOF);
}

SCM
reader_read_identifier_or_number (int c)
{
  char buf[MAX_STRING];
  int i = 0;
  int n = 0;
  int negative_p = 0;
  if (c == '+' && isdigit (peekchar ()))
    c = readchar ();
  else if (c == '-' && isdigit (peekchar ()))
    {
      negative_p = 1;
      c = readchar ();
    }
  while (isdigit (c))
    {
      buf[i++] = c;
      n *= 10;
      n += c - '0';
      c = readchar ();
    }
  if (reader_end_of_word_p (c))
    {
      unreadchar (c);
      if (negative_p)
        n = 0 - n;
      return MAKE_NUMBER (n);
    }
  /* Fallthrough: Note that `4a', `+1b' are identifiers */
  while (!reader_end_of_word_p (c))
    {
      buf[i++] = c;
      c = readchar ();
    }
  unreadchar (c);
  buf[i] = 0;
  return lookup_symbol_ (cstring_to_list (buf));
}

SCM
reader_read_sexp_ (int c, SCM a)
{
reset_reader:
  if (c == EOF)
    return cell_nil;
  if (c == ';')
    {
      c = reader_read_line_comment (c);
      goto reset_reader;
    }
  if ((c == ' ') || (c == '\t') || (c == '\n') || (c == '\f'))
    {
      c = readchar ();
      goto reset_reader;
    }
  if (c == '(')
    return reader_read_list (readchar (), a);
  if (c == ')')
    return cell_nil;
  if (c == '#')
    return reader_read_hash (readchar (), a);
  if (c == '`')
    return cons (cell_symbol_quasiquote,
                 cons (reader_read_sexp_ (readchar (), a), cell_nil));
  if(c == ',')
    {
      if (peekchar () == '@')
        {
          readchar ();
          return cons (cell_symbol_unquote_splicing,
                       cons (reader_read_sexp_ (readchar (), a), cell_nil));
        }
      return cons (cell_symbol_unquote,
                   cons (reader_read_sexp_ (readchar (), a), cell_nil));
    }
  if (c == '\'')
    return cons (cell_symbol_quote,
                 cons (reader_read_sexp_ (readchar (), a), cell_nil));
  if (c == '"')
    return reader_read_string ();
  if (c == '.' && (!reader_identifier_p (peekchar ())))
    return cell_dot;
  return reader_read_identifier_or_number (c);
}

int
reader_eat_whitespace (int c)
{
  while (isspace (c))
    c = readchar ();
  if (c == ';')
    return reader_eat_whitespace (reader_read_line_comment (c));
  if (c == '#' && (peekchar () == '!' || peekchar () == '|'))
    {
      c=readchar ();
      reader_read_block_comment (c, readchar ());
      return reader_eat_whitespace (readchar ());
    }
  return c;
}

SCM
reader_read_list (int c, SCM a)
{
  c = reader_eat_whitespace (c);
  if (c == ')')
    return cell_nil;
  if (c == EOF)
    error (cell_symbol_not_a_pair, MAKE_STRING (cstring_to_list ("EOF in list")));
    //return cell_nil;
  SCM s = reader_read_sexp_ (c, a);
  if (s == cell_dot)
    return CAR (reader_read_list (readchar (), a));
  return cons (s, reader_read_list (readchar (), a));
}

SCM
read_env (SCM a)
{
  return reader_read_sexp_ (readchar (), a);
}

SCM
reader_read_block_comment (int s, int c)
{
  if (c == s && peekchar () == '#') return readchar ();
  return reader_read_block_comment (s, readchar ());
}

SCM
reader_read_hash (int c, SCM a)
{
  if (c == '!')
    {
      reader_read_block_comment (c, readchar ());
      return reader_read_sexp_ (readchar (), a);
    }
  if (c == '|')
    {
      reader_read_block_comment (c, readchar ());
      return reader_read_sexp_ (readchar (), a);
    }
  if(c == 'f')
    return cell_f;
  if(c == 't')
    return cell_t;
  if(c == ',')
    {
      if (peekchar () == '@')
        {
          readchar ();
          return cons (cell_symbol_unsyntax_splicing,
                       cons (reader_read_sexp_ (readchar (), a), cell_nil));
        }

      return cons (cell_symbol_unsyntax,
                   cons (reader_read_sexp_ (readchar (), a), cell_nil));
    }
  if (c == '\'')
    return cons (cell_symbol_syntax,
                 cons (reader_read_sexp_ (readchar (), a), cell_nil));
  if (c == '`')
    return cons (cell_symbol_quasisyntax,
                 cons (reader_read_sexp_ (readchar (), a), cell_nil));
  if (c == ':')
    return MAKE_KEYWORD (CAR (reader_read_sexp_ (readchar (), a)));
  if (c == 'b')
    return reader_read_binary ();
  if (c == 'o')
    return reader_read_octal ();
  if (c == 'x')
    return reader_read_hex ();
  if (c == '\\')
    return reader_read_character ();
  if (c == '(')
    return list_to_vector (reader_read_list (readchar (), a));
  if (c == ';')
    {
      reader_read_sexp_ (readchar (), a);
      return reader_read_sexp_ (readchar (), a);
    }
  return reader_read_sexp_ (readchar (), a);
}

SCM
reader_read_sexp (SCM c, SCM s, SCM a)
{
  return reader_read_sexp_ (VALUE (c), a);
}

SCM
reader_read_character ()
{
  int c = readchar ();
  int p = peekchar ();
  int i = 0;
  if (c >= '0' && c <= '7'
      && p >= '0' && p <= '7')
    {
      c = c - '0';
      while (p >= '0' && p <= '7')
        {
          c <<= 3;
          c += readchar () - '0';
          p = peekchar ();
        }
    }
  else if (((c >= 'a' && c <= 'z')
            || c == '*')
           && ((p >= 'a' && p <= 'z')
               || p == '*'))
    {
      char buf[10];
      buf[i] = c;
      i = i + 1;
      while ((p >= 'a' && p <= 'z')
             || p == '*')
        {
          buf[i] = readchar ();
          i = i + 1;
          p = peekchar ();
        }
      buf[i] = 0;
      if (!strcmp (buf, "*eof*")) c = EOF;
      else if (!strcmp (buf, "nul")) c = '\0';
      else if (!strcmp (buf, "alarm")) c = '\a';
      else if (!strcmp (buf, "backspace")) c = '\b';
      else if (!strcmp (buf, "tab")) c = '\t';
      else if (!strcmp (buf, "linefeed")) c = '\n';
      else if (!strcmp (buf, "newline")) c = '\n';
      else if (!strcmp (buf, "vtab")) c = '\v';
      else if (!strcmp (buf, "page")) c = '\f';
#if 1 //__MESC__
      //Nyacc bug
      else if (!strcmp (buf, "return")) c = 13;
      else if (!strcmp (buf, "esc")) c = 27;
#else
      else if (!strcmp (buf, "return")) c = '\r';
      //Nyacc crash else if (!strcmp (buf, "esc")) c = '\e';
#endif
      else if (!strcmp (buf, "space")) c = ' ';

#if 1 // Nyacc uses old abbrevs
      else if (!strcmp (buf, "bel")) c = '\a';
      else if (!strcmp (buf, "bs")) c = '\b';
      else if (!strcmp (buf, "ht")) c = '\t';
      else if (!strcmp (buf, "vt")) c = '\v';

#if 1 //__MESC__
      //Nyacc bug
      else if (!strcmp (buf, "cr")) c = 13;
#else
      else if (!strcmp (buf, "cr")) c = '\r';
#endif
#endif // Nyacc uses old abbrevs

      else
        {
          eputs ("char not supported: ");
          eputs (buf);
          eputs ("\n");
          error (cell_symbol_system_error,
                 MAKE_STRING (cstring_to_list ("char not supported")));
        }
    }
  return MAKE_CHAR (c);
}

SCM
reader_read_binary ()
{
  int n = 0;
  int c = peekchar ();
  int s = 1;
  if (c == '-')
    {
      s = -1;
      readchar ();
      c = peekchar ();
    }
  while (c == '0' || c == '1')
    {
      n = n << 1;
      n = n + c - '0';
      readchar ();
      c = peekchar ();
    }
  return MAKE_NUMBER (s*n);
}

SCM
reader_read_octal ()
{
  int n = 0;
  int c = peekchar ();
  int s = 1;
  if (c == '-')
    {
      s = -1;
      readchar ();
      c = peekchar ();
    }
  while (c >= '0' && c <= '7')
    {
      n = n << 3;
      n = n + c - '0';
      readchar ();
      c = peekchar ();
    }
  return MAKE_NUMBER (s*n);
}

SCM
reader_read_hex ()
{
  int n = 0;
  int c = peekchar ();
  int s = 1;
  if (c == '-')
    {
      s = -1;
      readchar ();
      c = peekchar ();
    }
  while ((c >= '0' && c <= '9')
         || (c >= 'A' && c <= 'F')
         || (c >= 'a' && c <= 'f'))
    {
      n = n << 4;
      if (c >= 'a')
        n = n + c - 'a' + 10;
      else if (c >= 'A')
        n = n + c - 'A' + 10;
      else
        n = n + c - '0';
      readchar ();
      c = peekchar ();
    }
  return MAKE_NUMBER (s*n);
}

SCM
reader_read_string ()
{
  SCM lst = cell_nil;
  int c;
  do
    {
      c = readchar ();
      if (c == '"')
        break;
      if (c == '\\')
        {
          c = readchar ();
          if (c == '\\' || c == '"')
            lst = cons (MAKE_CHAR (c), lst);
          else if (c == '0')
            lst = cons (MAKE_CHAR ('\0'), lst);
          else if (c == 'a')
            lst = cons (MAKE_CHAR ('\a'), lst);
          else if (c == 'b')
            lst = cons (MAKE_CHAR ('\b'), lst);
          else if (c == 't')
            lst = cons (MAKE_CHAR ('\t'), lst);
          else if (c == 'n')
            lst = cons (MAKE_CHAR ('\n'), lst);
          else if (c == 'v')
            lst = cons (MAKE_CHAR ('\v'), lst);
          else if (c == 'f')
            lst = cons (MAKE_CHAR ('\f'), lst);
          else if (c == 'r')
            // Nyacc bug
            // lst = cons (MAKE_CHAR ('\r'), lst);
            lst = cons (MAKE_CHAR (13), lst);
          else if (c == 'e')
            // Nyacc bug
            // lst = cons (MAKE_CHAR ('\e'), lst);
            lst = cons (MAKE_CHAR (27), lst);
        }
      else
        lst = cons (MAKE_CHAR (c), lst);
    }
  while (1);
  return MAKE_STRING (reverse_x_ (lst, cell_nil));
}

int g_tiny = 0;

int
dump ()
{
  r1 = g_symbols;
  gc_push_frame ();
  gc ();
  gc_peek_frame ();
  char *p = (char*)g_cells;
  putchar ('M');
  putchar ('E');
  putchar ('S');
  putchar (g_stack >> 8);
  putchar (g_stack % 256);
  eputs ("dumping\n");
  // See HACKING, simple crafted dump for tiny-mes.c
  if (g_tiny || getenv ("MES_TINY"))
    {
      eputs ("dumping TINY\n");

      TYPE (0) = 0x6c6c6168;
      CAR (0) = 0x6a746f6f;
      CDR (0) = 0x00002165;

      TYPE (1) = TSYMBOL;
      CAR (1) = 0x2d2d2d2d;
      CDR (1) = 0x3e3e3e3e;

      TYPE (9) = 0x2d2d2d2d;
      CAR (9) = 0x2d2d2d2d;
      CDR (9) = 0x3e3e3e3e;

      // (cons 0 1)
      TYPE (10) = TPAIR;
      CAR (10) = 11;
      CDR (10) = 12;

      TYPE (11) = TFUNCTION;
      CAR (11) = 0x58585858;
      // 0 = make_cell_
      // 1 = cons
      // 2 = car
      CDR (11) = 1;

      TYPE (12) = TPAIR;
      CAR (12) = 13;
      CDR (12) = 14;

      TYPE (13) = TNUMBER;
      CAR (13) = 0x58585858;
      CDR (13) = 0;

      TYPE (14) = TPAIR;
      CAR (14) = 15;
      CDR (14) = 1;

      TYPE (15) = TNUMBER;
      CAR (15) = 0x58585858;
      CDR (15) = 1;

      g_free = 16;
    }
  else if (g_debug > 1)
    {
      eputs ("program r2=");
      display_error_ (r2);
      eputs ("\n");
    }

  int i;
  for (i=0; i<g_free * sizeof (struct scm); i = i + 1)
    {
      putchar (p[0]);
      p = p + 1;
    }
  return 0;
}
