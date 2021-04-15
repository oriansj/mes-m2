/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <stdlib.h>

int g_depth;

char char_lookup(int c)
{
  if(c == '\a') return 'a';
  else if(c == '\b') return 'b';
  else if(c == '\t') return 't';
  else if(c == '\v') return 'v';
  else if(c == '\n') return 'n';
  else if(c == '\f') return 'f';
  else if(c == '\r') return 'r';
  else if(c == '\033') return 'e';
  else if(c == '\\') return '\\';
  else if(c == '"') return '"';
  return c;
}

void
fdwrite_char (char c, FILE* fd)
{
  if(in_set(c, "\a\b\t\b\v\f\n\r\033\"\\"))
  {
    fputc('\\', fd);
    c = char_lookup(c);
  }

  fputc(c, fd);
}

void
fdwrite_string_char (char v, FILE* fd)
{
  if (v == '\0')
    fputs ("\\0", fd);
  else if (v == '\a')
    fputs ("\\a", fd);
  else if (v == '\b')
    fputs ("\\b", fd);
  else if (v == '\t')
    fputs ("\\t", fd);
  else if (v == '\v')
    fputs ("\\v", fd);
  else if (v == '\n')
    fputs ("\\n", fd);
  else if (v == '\f')
    fputs ("\\f", fd);
  /* Nyacc bug
     else if (v == '\r') fdputs ("\\r", fd);
     else if (v == '\e') fdputs ("\\e", fd);
  */
  else if (v == 13)
    fputs ("\\r", fd);
  else if (v == 27)
    fputs ("\\e", fd);
  else if (v == '\\')
    fputs ("\\\\", fd);
  else if (v == '"')
    fputs ("\\\"", fd);
  else
    fputc (v, fd);
}

void
fdwrite_string (char *s, int length, FILE* fd)
{
  int i;
  for (i = 0; i < length; i = i + 1)
    fdwrite_char(s[i], fd);
}

void
fddisplay_string (char *s, int length, FILE* fd)
{
  int i;
  for (i = 0; i < length; i = i + 1)
    fputc(s[i], fd);
}


struct scm *display_helper (struct scm *x, int cont, char *sep, FILE* fd, int write_p);

struct scm *
display_helper (struct scm *x, int cont, char *sep, FILE* fd, int write_p)
{
  char* s;
  fputs (sep, fd);
  if (g_depth == 0)
    return cell_unspecified;
  g_depth = g_depth - 1;

  int t = x->type;
  if (t == TCHAR)
    {
      if (write_p == 0)
        fputc (x->value, fd);
      else
        {
          fputs ("#", fd);
          fputc (x->value, fd);
        }
    }
  else if (t == TCLOSURE)
    {
      fputs ("#<closure ", fd);
      struct scm *circ = x->cdr->car;
      struct scm *name = circ->cdr->car;
      struct scm *args = x->cdr->cdr->car;
      display_helper (name->car, 0, "", fd, 0);
      fputc (' ', fd);
      display_helper (args, 0, "", fd, 0);
      fputs (">", fd);
    }
  else if (t == TMACRO)
    {
      fputs ("#<macro ", fd);
      display_helper (x->cdr, cont, "", fd, 0);
      fputs (">", fd);
    }
  else if (t == TVARIABLE)
    {
      fputs ("#<variable ", fd);
      display_helper (x->variable->car, cont, "", fd, 0);
      fputs (">", fd);
    }
  else if (t == TNUMBER)
    {
      fputs (itoa (x->value), fd);
    }
  else if (t == TPAIR)
    {
      if (cont == 0)
        fputs ("(", fd);
      if (x->car == cell_circular && x->cdr->car != cell_closure)
        {
          fputs ("(*circ* . ", fd);
          int i = 0;
          x = x->cdr;
          while (x != cell_nil && i < 10)
            {
              i = i + 1;
              fdisplay_ (x->car->car, fd, write_p);
              fputs (" ", fd);
              x = x->cdr;
            }
          fputs (" ...)", fd);
        }
      else
        {
          if (x != 0 && x != cell_nil)
            fdisplay_ (x->car, fd, write_p);
          if (x->cdr != 0 && x->cdr->type == TPAIR)
            display_helper (x->cdr, 1, " ", fd, write_p);
          else if (x->cdr != 0 && x->cdr != cell_nil)
            {
              if (x->cdr->type != TPAIR)
                fputs (" . ", fd);
              fdisplay_ (x->cdr, fd, write_p);
            }
        }
      if (cont == 0)
        fputs (")", fd);
    }
  else if (t == TPORT)
    {
      fputs ("#<port ", fd);
      fputs (itoa (x->port), fd);
      fputs (" ", fd);
      x = x->string;
      fputc ('"', fd);
      fdwrite_string (cell_bytes (x->string), x->length, fd);
      fputc ('"', fd);
      fputs (">", fd);
    }
  else if (t == TKEYWORD)
    {
      fputs ("#:", fd);
      fdwrite_string (cell_bytes (x->string), x->length, fd);
    }
  else if (t == TSTRING)
    {
      s = cell_bytes (x->string);
      if (write_p == 1)
        {
          fputc ('"', fd);
          fdwrite_string (s, x->length, fd);
          fputc ('"', fd);
        }
      else
        fddisplay_string (s, x->length, fd);
    }
  else if (t == TSPECIAL || t == TSYMBOL)
  {
    if(x == cell_unspecified)
      fputs("#<unspecified>", fd);
    else
      fdwrite_string (cell_bytes (x->string), x->length, fd);
  }
  else if (t == TREF)
    fdisplay_ (x->ref, fd, write_p);
  else if (t == TSTRUCT)
    {
      struct scm *printer = struct_ref_ (x, STRUCT_PRINTER);
      if (printer->type == TREF)
        printer = printer->ref;
      if (printer->type == TCLOSURE || builtin_p (printer) == cell_t)
        apply (printer, cons (x, cell_nil), R0);
      else
        {
          fputs ("#<", fd);
          fdisplay_ (x->structure, fd, write_p);
          struct scm *t = x->car;
          long size = x->length;
          long i;
          for (i = 2; i < size; i = i + 1)
            {
              fputc (' ', fd);
              fdisplay_ (cell_ref (x->structure, i), fd, write_p);
            }
          fputc ('>', fd);
        }
    }
  else if (t == TVECTOR)
    {
      fputs ("#(", fd);
      struct scm *t = x->car;
      long i;
      for (i = 0; i < x->length; i = i + 1)
        {
          if (i != 0)
            fputc (' ', fd);
          fdisplay_ (cell_ref (x->vector, i), fd, write_p);
        }
      fputc (')', fd);
    }
  else
    {
      fputs ("<", fd);
      fputs (itoa (t), fd);
      fputs (":", fd);
      fputs (ltoa (cast_voidp_to_long (x)), fd);
      fputs (">", fd);
    }
  return cell_unspecified;
}

struct scm *
display_ (struct scm *x)
{
  g_depth = 5;
  return display_helper (x, 0, "", __stdout, 0);
}

struct scm *
display_error_ (struct scm *x)
{
  g_depth = 5;
  return display_helper (x, 0, "", __stderr, 0);
}

struct scm *
display_port_ (struct scm *x, struct scm *p)
{
  assert_msg (p->type == TNUMBER, "p->type == TNUMBER");
  return fdisplay_ (x, p->name_cdr, 0);
}

struct scm *
write_ (struct scm *x)
{
  g_depth = 5;
  return display_helper (x, 0, "", __stdout, 1);
}

struct scm *
write_error_ (struct scm *x)
{
  g_depth = 5;
  return display_helper (x, 0, "", __stderr, 1);
}

struct scm *
write_port_ (struct scm *x, struct scm *p)
{
  assert_msg (p->type == TNUMBER, "p->type == TNUMBER");
  return fdisplay_ (x, p->name_cdr, 1);
}

struct scm *
fdisplay_ (struct scm *x, FILE* fd, int write_p)  /*:((internal)) */
{
  g_depth = 5;
  return display_helper (x, 0, "", fd, write_p);
}
