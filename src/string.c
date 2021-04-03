/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include "mes/lib.h"
#include "mes/mes.h"

#include <limits.h>
#include <string.h>

void
assert_max_string (size_t i, char const *msg, char const *string)
{
  if (i > MAX_STRING)
    {
      eputs (msg);
      eputs (":string too long[");
      eputs (itoa (i));
      eputs ("]:");
      char *p = cast_voidp_to_charp (string);
      p[MAX_STRING - 1] = 0;
      eputs (p);
      error (cell_symbol_system_error, cell_f);
    }
}

char const *
list_to_cstring (struct scm *list, size_t *size)
{
  size_t i = 0;
  char *p = g_buf;
  struct scm *x;
  while (list != cell_nil)
    {
      if (i > MAX_STRING)
        assert_max_string (i, "list_to_string", g_buf);
      x = car (list);
      g_buf[i] = x->value;
      i = i + 1;
      list = cdr (list);
    }
  g_buf[i] = 0;
  size[0] = i;

  return g_buf;
}

struct scm *
string_equal_p (struct scm *a, struct scm *b)   /*:((name . "string=?")) */
{
  if (!((a->type == TSTRING && b->type == TSTRING) || (a->type == TKEYWORD || b->type == TKEYWORD)))
    {
      eputs ("type a: ");
      eputs (itoa (a->type));
      eputs ("\n");
      eputs ("type b: ");
      eputs (itoa (b->type));
      eputs ("\n");
      eputs ("a= ");
      write_error_ (a);
      eputs ("\n");
      eputs ("b= ");
      write_error_ (b);
      eputs ("\n");
      assert_msg ((a->type == TSTRING && b->type == TSTRING) || (a->type == TKEYWORD || b->type == TKEYWORD), "(a->type == TSTRING && b->type == TSTRING) || (a->type == TKEYWORD || b->type == TKEYWORD)");
    }
  if (a == b)
    return cell_t;
  if (a->string == b->string)
    return cell_t;
  if (a->length == 0 && b->length == 0)
    return cell_t;
  if (a->length == b->length)
    if (memcmp (cell_bytes (a->string), cell_bytes (b->string), a->length) == 0)
      return cell_t;

  return cell_f;
}

struct scm *
symbol_to_string (struct scm *symbol)
{
  return make_cell (TSTRING, symbol->car, symbol->cdr);
}

struct scm *
symbol_to_keyword (struct scm *symbol)
{
  return make_cell (TKEYWORD, symbol->car, symbol->cdr);
}

struct scm *
keyword_to_string (struct scm *keyword)
{
  return make_cell (TSTRING, keyword->car, keyword->cdr);
}

struct scm *
string_to_symbol (struct scm *string)
{
  struct scm *x = hash_ref (g_symbols, string, cell_f);
  if (x == cell_f)
    x = make_symbol (string);
  return x;
}

struct scm *
make_symbol (struct scm *string)
{
  struct scm *x = make_pointer_cell (TSYMBOL, string->length, string->string);
  hash_set_x (g_symbols, string, x);
  return x;
}

struct scm *
bytes_to_list (char const *s, size_t i)
{
  struct scm *p = cell_nil;
  int c;
  while (i != 0)
    {
      i = i - 1;
      c = (0x100 + s[i]) % 0x100;
      p = cons (make_char (c), p);
    }
  return p;
}

struct scm *
cstring_to_list (char const *s)
{
  return bytes_to_list (s, strlen (s));
}

struct scm *
cstring_to_symbol (char const *s)
{
  struct scm *string = make_string0 (s);
  return string_to_symbol (string);
}

struct scm *
string_to_list (struct scm *string)
{
  return bytes_to_list (cell_bytes (string->string), string->length);
}

struct scm *
list_to_string (struct scm *list)
{
  size_t size;
  char const *s = list_to_cstring (list, &size);
  return make_string (s, size);
}

struct scm *
read_string (struct scm *port)          /*:((arity . n)) */
{
  int fd = __stdin;
  if (port->type == TPAIR)
    {
      struct scm *p = car (port);
      if (p->type == TNUMBER)
        __stdin = p->value;
    }
  int c = readchar ();
  size_t i = 0;
  while (c != -1)
    {
      if (i > MAX_STRING)
        assert_max_string (i, "read_string", g_buf);
      g_buf[i] = c;
      i = i + 1;
      c = readchar ();
    }
  g_buf[i] = 0;
  __stdin = fd;
  return make_string (g_buf, i);
}

struct scm *
string_append (struct scm *x)           /*:((arity . n)) */
{
  char *p = g_buf;
  g_buf[0] = 0;
  size_t size = 0;
  struct scm *string;
  while (x != cell_nil)
    {
      string = x->car;
      assert_msg (string->type == TSTRING, "string->type == TSTRING");
      memcpy (p, cell_bytes (string->string), string->length + 1);
      p = p + string->length;
      size = size + string->length;
      if (size > MAX_STRING)
        assert_max_string (size, "string_append", g_buf);
      x = x->cdr;
    }
  return make_string (g_buf, size);
}

struct scm *
string_length (struct scm *string)
{
  assert_msg (string->type == TSTRING, "string->type == TSTRING");
  return make_number (string->length);
}

struct scm *
string_ref (struct scm *str, struct scm *k)
{
  assert_msg (str->type == TSTRING, "str->type == TSTRING");
  assert_msg (k->type == TNUMBER, "k->type == TNUMBER");
  size_t size = str->length;
  size_t i = k->value;
  if (i > size)
    error (cell_symbol_system_error, cons (make_string0 ("value out of range"), k));
  char const *p = cell_bytes (str->string);
  return make_char (p[i]);
}
