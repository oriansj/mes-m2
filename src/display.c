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

void
fdwrite_char (char v, int fd)
{
  if (v == '\0')
    fdputs ("\\nul", fd);
  else if (v == '\a')
    fdputs ("\\alarm", fd);
  else if (v == '\b')
    fdputs ("\\backspace", fd);
  else if (v == '\t')
    fdputs ("\\tab", fd);
  else if (v == '\n')
    fdputs ("\\newline", fd);
  else if (v == '\v')
    fdputs ("\\vtab", fd);
  else if (v == '\f')
    fdputs ("\\page", fd);
  /* Nyacc bug
     else if (v == '\r') fdputs ("return", fd);
  */
  else if (v == 13)
    fdputs ("\\return", fd);
  else if (v == ' ')
    fdputs ("\\space", fd);
  else
    {
      if (v >= 32 && v <= 127)
        fdputc ('\\', fd);
      fdputc (v, fd);
    }
}

void
fdwrite_string_char (char v, int fd)
{
  if (v == '\0')
    fdputs ("\\0", fd);
  else if (v == '\a')
    fdputs ("\\a", fd);
  else if (v == '\b')
    fdputs ("\\b", fd);
  else if (v == '\t')
    fdputs ("\\t", fd);
  else if (v == '\v')
    fdputs ("\\v", fd);
  else if (v == '\n')
    fdputs ("\\n", fd);
  else if (v == '\f')
    fdputs ("\\f", fd);
  /* Nyacc bug
     else if (v == '\r') fdputs ("\\r", fd);
     else if (v == '\e') fdputs ("\\e", fd);
  */
  else if (v == 13)
    fdputs ("\\r", fd);
  else if (v == 27)
    fdputs ("\\e", fd);
  else if (v == '\\')
    fdputs ("\\\\", fd);
  else if (v == '"')
    fdputs ("\\\"", fd);
  else
    fdputc (v, fd);
}

void
fdwrite_string (char *s, int length, int fd)
{
  int i;
  for (i = 0; i < length; i = i + 1)
    fdwrite_string_char (s[i], fd);
}

struct scm *display_helper (struct scm *x, int cont, char *sep, int fd, int write_p);

struct scm *
display_helper (struct scm *x, int cont, char *sep, int fd, int write_p)
{
  fdputs (sep, fd);
  if (g_depth == 0)
    return cell_unspecified;
  g_depth = g_depth - 1;

  int t = x->type;
  if (t == TCHAR)
    {
      if (write_p == 0)
        fdputc (x->value, fd);
      else
        {
          fdputs ("#", fd);
          fdwrite_char (x->value, fd);
        }
    }
  else if (t == TCLOSURE)
    {
      fdputs ("#<closure ", fd);
      struct scm *circ = x->cdr->car;
      struct scm *name = circ->cdr->car;
      struct scm *args = x->cdr->cdr->car;
      display_helper (name->car, 0, "", fd, 0);
      fdputc (' ', fd);
      display_helper (args, 0, "", fd, 0);
      fdputs (">", fd);
    }
  else if (t == TMACRO)
    {
      fdputs ("#<macro ", fd);
      display_helper (x->cdr, cont, "", fd, 0);
      fdputs (">", fd);
    }
  else if (t == TVARIABLE)
    {
      fdputs ("#<variable ", fd);
      display_helper (x->variable->car, cont, "", fd, 0);
      fdputs (">", fd);
    }
  else if (t == TNUMBER)
    {
      fdputs (itoa (x->value), fd);
    }
  else if (t == TPAIR)
    {
      if (cont == 0)
        fdputs ("(", fd);
      if (x->car == cell_circular && x->cdr->car != cell_closure)
        {
          fdputs ("(*circ* . ", fd);
          int i = 0;
          x = x->cdr;
          while (x != cell_nil && i < 10)
            {
              i = i + 1;
              fdisplay_ (x->car->car, fd, write_p);
              fdputs (" ", fd);
              x = x->cdr;
            }
          fdputs (" ...)", fd);
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
                fdputs (" . ", fd);
              fdisplay_ (x->cdr, fd, write_p);
            }
        }
      if (cont == 0)
        fdputs (")", fd);
    }
  else if (t == TPORT)
    {
      fdputs ("#<port ", fd);
      fdputs (itoa (x->port), fd);
      fdputs (" ", fd);
      x = x->string;
      fdputc ('"', fd);
      fdwrite_string (cell_bytes (x->string), x->length, fd);
      fdputc ('"', fd);
      fdputs (">", fd);
    }
  else if (t == TKEYWORD)
    {
      fdputs ("#:", fd);
      fdwrite_string (cell_bytes (x->string), x->length, fd);
    }
  else if (t == TSTRING)
    {
      if (write_p == 1)
        {
          fdputc ('"', fd);
          fdwrite_string (cell_bytes (x->string), x->length, fd);
          fdputc ('"', fd);
        }
      else
        fdputs (cell_bytes (x->string), fd);
    }
  else if (t == TSPECIAL || t == TSYMBOL)
    fdwrite_string (cell_bytes (x->string), x->length, fd);
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
          fdputs ("#<", fd);
          fdisplay_ (x->structure, fd, write_p);
          struct scm *t = x->car;
          long size = x->length;
          long i;
          for (i = 2; i < size; i = i + 1)
            {
              fdputc (' ', fd);
              fdisplay_ (cell_ref (x->structure, i), fd, write_p);
            }
          fdputc ('>', fd);
        }
    }
  else if (t == TVECTOR)
    {
      fdputs ("#(", fd);
      struct scm *t = x->car;
      long i;
      for (i = 0; i < x->length; i = i + 1)
        {
          if (i != 0)
            fdputc (' ', fd);
          fdisplay_ (cell_ref (x->vector, i), fd, write_p);
        }
      fdputc (')', fd);
    }
  else
    {
      fdputs ("<", fd);
      fdputs (itoa (t), fd);
      fdputs (":", fd);
      fdputs (ltoa (cast_voidp_to_long (x)), fd);
      fdputs (">", fd);
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
  return fdisplay_ (x, p->value, 0);
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
  return fdisplay_ (x, p->value, 1);
}

struct scm *
fdisplay_ (struct scm *x, int fd, int write_p)  /*:((internal)) */
{
  g_depth = 5;
  return display_helper (x, 0, "", fd, write_p);
}
