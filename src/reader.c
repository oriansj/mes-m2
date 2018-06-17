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
#if 0
  if (assq_ref_env (cell_symbol_read_input_file, r0) != cell_undefined)
    return apply (cell_symbol_read_input_file, cell_nil, r0);
#endif
  return read_input_file_env_ (read_env (r0), r0);
}

int
reader_read_line_comment (int c)
{
  if (c == '\n')
    return c;
  return reader_read_line_comment (readchar ());
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
  char buf[1024];
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
  SCM s = cell_nil;
  switch (c)
    {
    case EOF:
      return cell_nil;
    case ';':
      reader_read_line_comment (c);
    case ' ':
    case '\t':
    case '\n':
    case '\f':
      return reader_read_sexp_ (readchar (), a);
    case '(':
      return reader_read_list (readchar (), a);
    case  ')':
      return cell_nil;
    case '#':
      return reader_read_hash (readchar (), a);
    case '`':
      return cons (cell_symbol_quasiquote,
                   cons (reader_read_sexp_ (readchar (), a), cell_nil));
    case ',':
      if (peekchar () == '@')
        {
          readchar ();
          return cons (cell_symbol_unquote_splicing,
                       cons (reader_read_sexp_ (readchar (), a), cell_nil));
        }
      return cons (cell_symbol_unquote,
                   cons (reader_read_sexp_ (readchar (), a), cell_nil));
    case '\'':
      return cons (cell_symbol_quote,
                   cons (reader_read_sexp_ (readchar (), a), cell_nil));
    case '"':
      return reader_read_string ();
    case '.':
      if (!reader_identifier_p (peekchar ()))
        return cell_dot;
    default:
      return reader_read_identifier_or_number (c);
    }
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
  switch (c)
    {
    case '!':
      reader_read_block_comment (c, readchar ());
      return reader_read_sexp_ (readchar (), a);
    case '|':
      reader_read_block_comment (c, readchar ());
      return reader_read_sexp_ (readchar (), a);
    case 'f':
      return cell_f;
    case 't':
      return cell_t;
    case ',':
      if (peekchar () == '@')
        {
          readchar ();
          return cons (cell_symbol_unsyntax_splicing,
                       cons (reader_read_sexp_ (readchar (), a),
                             cell_nil));
        }
      return cons (cell_symbol_unsyntax,
                   cons (reader_read_sexp_ (readchar (), a), cell_nil));
    case '\'':
      return cons (cell_symbol_syntax,
                   cons (reader_read_sexp_ (readchar (), a), cell_nil));
    case '`':
      return cons (cell_symbol_quasisyntax,
                   cons (reader_read_sexp_ (readchar (), a), cell_nil));
    case ':':
    return MAKE_KEYWORD (CAR (reader_read_sexp_ (readchar (), a)));
    case 'b':
      return reader_read_binary ();
    case 'o':
      return reader_read_octal ();
    case 'x':
      return reader_read_hex ();
    case '\\':
      return reader_read_character ();
    case '(':
      return list_to_vector (reader_read_list (readchar (), a));
    case ';':
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
  if (c >= '0' && c <= '7'
      && peekchar () >= '0' && peekchar () <= '7')
    {
      c = c - '0';
      while (peekchar () >= '0' && peekchar () <= '7')
        {
          c <<= 3;
          c += readchar () - '0';
        }
    }
  else if (((c >= 'a' && c <= 'z')
            || c == '*')
           && ((peekchar () >= 'a' && peekchar () <= 'z')
               || peekchar () == '*'))
    {
      char buf[10];
      char *p = buf;
      *p++ = c;
      while ((peekchar () >= 'a' && peekchar () <= 'z')
             || peekchar () == '*')
        {
          *p++ = readchar ();
        }
      *p = 0;
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
#if !__MESC__
          assert (!"char not supported");
#endif
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
  if (c == '-') {s = -1; readchar (); c = peekchar ();}
  while (c == '0' || c == '1')
    {
      n <<= 1;
      n+= c - '0';
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
  if (c == '-') {s = -1;readchar (); c = peekchar ();}
  while (c >= '0' && c <= '7')
    {
      n <<= 3;
      n+= c - '0';
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
  if (c == '-') {s = -1;readchar (); c = peekchar ();}
  while ((c >= '0' && c <= '9')
         || (c >= 'A' && c <= 'F')
         || (c >= 'a' && c <= 'f'))
    {
      n <<= 4;
      if (c >= 'a') n += c - 'a' + 10;
      else if (c >= 'A') n += c - 'A' + 10;
      else n+= c - '0';
      readchar ();
      c = peekchar ();
    }
  return MAKE_NUMBER (s*n);
}

SCM
reader_read_string ()
{
  char buf[1024];
  SCM lst = cell_nil;
  int i = 0;
  int c = readchar ();
  while (1)
    {
      if (c == '"' || i > 1022)
        {
          buf[i] = 0;
          lst = append2 (lst, string_to_list (buf, i));
          i = 0;
          if (c == '"')
            break;
        }
      if (c == '\\')
        {
          int p = peekchar ();
          if (p == '\\' || p == '"')
            buf[i++] = readchar ();
          else if (p == '0')
            {
              readchar ();
              buf[i++] = '\0';
            }
          else if (p == 'a')
            {
              readchar ();
              buf[i++] = '\a';
            }
          else if (p == 'b')
            {
              readchar ();
              buf[i++] = '\b';
            }
          else if (p == 't')
            {
              readchar ();
              buf[i++] = '\t';
            }
          else if (p == 'n')
            {
              readchar ();
              buf[i++] = '\n';
            }
          else if (p == 'v')
            {
              readchar ();
              buf[i++] = '\v';
            }
          else if (p == 'f')
            {
              readchar ();
              buf[i++] = '\f';
            }
          else if (p == 'r')
            {
              readchar ();
              //Nyacc bug
              //buf[i++] = '\r';
              buf[i++] = 13;
            }
          else if (p == 'e')
            {
              readchar ();
              //buf[i++] = '\e';
              buf[i++] = 27;
            }
        }
#if 0 // !__MESC__
      else if (c == EOF)
        assert (!"EOF in string");
#endif
      else
        buf[i++] = c;
    c = readchar ();
  }
  return MAKE_STRING (lst);
}

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
  if (g_debug > 1)
    {
      eputs ("program r2=");
      display_error_ (r2);
      eputs ("\n");
    }

  for (int i=0; i<g_free * sizeof (struct scm); i++)
    putchar (*p++);
  return 0;
}
