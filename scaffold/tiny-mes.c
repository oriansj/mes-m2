/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#if POSIX
#error "POSIX not supported"
#endif

#include <libmes.h>

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>

#if __M2_PLANET__
int open (char const*,int,int);
#else
int open (char const*,int,...);
#endif
void *malloc (size_t size);
void exit (int);
char *arena;

typedef int SCM;

int g_free;
int g_debug;

SCM g_symbols;
SCM g_stack;

/* a/env */
SCM r0;
/* param 1 */
SCM r1;
/* save 2+load/dump */
SCM r2;
/* continuation */
SCM r3;

#if __M2_PLANET__
CONSTANT TCHAR          0
CONSTANT TCLOSURE       1
CONSTANT TCONTINUATION  2
CONSTANT TFUNCTION      3
CONSTANT TKEYWORD       4
CONSTANT TMACRO         5
CONSTANT TNUMBER        6
CONSTANT TPAIR          7
CONSTANT TPORT          8
CONSTANT TREF           9
CONSTANT TSPECIAL      10
CONSTANT TSTRING       11
CONSTANT TSYMBOL       12
CONSTANT TVALUES       13
CONSTANT TVARIABLE     14
CONSTANT TVECTOR       15
CONSTANT TBROKEN_HEART 16
#else // !__M2_PLANET__
enum type_t {TCHAR, TCLOSURE, TCONTINUATION, TFUNCTION, TKEYWORD, TMACRO, TNUMBER, TPAIR, TPORT, TREF, TSPECIAL, TSTRING, TSYMBOL, TVALUES, TVARIABLE, TVECTOR, TBROKEN_HEART};
#endif // !__M2_PLANET__

struct scm {
  enum type_t type;
  SCM car;
  SCM cdr;
};

#if __MESC__
struct scm *g_cells;
#else
struct scm *g_cells;
#endif

#define cell_nil 1
#define cell_f 2
#define cell_t 3

#if __M2_PLANET__
#define struct_size 12
#define TYPE(x) ((x*struct_size)+g_cells)->type
#define CAR(x) ((x*struct_size)+g_cells)->car
#define CDR(x) ((x*struct_size)+g_cells)->cdr
#define VALUE(x) ((x*struct_size)+g_cells)->cdr
#else // !__M2_PLANET__
#define TYPE(x) (g_cells+x)->type
#define CAR(x) (g_cells+x)->car
#define CDR(x) (g_cells+x)->cdr
#define VALUE(x) (g_cells+x)->cdr
#endif // !__M2_PLANET__

SCM
car (SCM x)
{
#if !__M2_PLANET__
  return CAR (x);
#else
  SCM t;
  //////SCM t = CAR (x);
  return t;
#endif
}

SCM
cdr (SCM x)
{
#if !__M2_PLANET__
  return CDR (x);
#endif
}

SCM caar (SCM x) {return car (car (x));}
SCM cadr (SCM x) {return car (cdr (x));}
SCM cdar (SCM x) {return cdr (car (x));}
SCM cddr (SCM x) {return cdr (cdr (x));}

SCM
gc_peek_frame ()
{
  SCM frame = car (g_stack);
  r1 = car (frame);
  r2 = cadr (frame);
  r3 = car (cddr (frame));
  r0 = cadr (cddr (frame));
  return frame;
}

// Environment setup

SCM
mes_environment ()
{
  return 0;
}

SCM
mes_builtins (SCM a)
{
  return a;
}

SCM
fill ()
{
  int i = 0;
  for (i=0; i<20; i)
    {
      TYPE (i) = 0;
      CAR (i) = 0;
      CDR (i) = 0;
      i = i + 1;
    }

#if 0 //__M2_PLANET__

  TYPE (0) = 'h' + ('a' << 8) + ('l' << 16) + ('l' << 24);
  CAR (0) = 'o' + ('o' << 8) + ('t' << 16) + ('j' << 24);
  CDR (0) = 'e' + ('!' << 8) + ('\0' << 16) + ('\0' << 24);

  TYPE (1) = TSYMBOL;
  CAR (1) = '-' + ('-' << 8) + ('-' << 16) + ('-' << 24);
  CDR (1) = '>' + ('>' << 8) + ('>' << 16) + ('>' << 24);

  TYPE (9) = '-' + ('-' << 8) + ('-' << 16) + ('-' << 24);
  CAR (9) = '-' + ('-' << 8) + ('-' << 16) + ('-' << 24);
  CDR (9) = '>' + ('>' << 8) + ('>' << 16) + ('>' << 24);

  TYPE (9) = '-' + ('-' << 8) + ('-' << 16) + ('-' << 24);
  CAR (9) = '-' + ('-' << 8) + ('-' << 16) + ('-' << 24);
  CDR (9) = '>' + ('>' << 8) + ('>' << 16) + ('>' << 24);

  // (A(B))
  TYPE (10) = TPAIR;
  CAR (10) = 11;
  CDR (10) = 12;

  TYPE (11) = TCHAR;
  CAR (11) = 'X' + ('X' << 8) + ('X' << 16) + ('X' << 24);
  CDR (11) = 89;

  TYPE (12) = TPAIR;
  CAR (12) = 13;
  CDR (12) = 1;

  TYPE (13) = TCHAR;
  CAR (13) = 'X' + ('X' << 8) + ('X' << 16) + ('X' << 24);
  CDR (13) = 90;

  TYPE (14) = 'X' + ('X' << 8) + ('X' << 16) + ('X' << 24);
  CAR (14) = 'X' + ('X' << 8) + ('X' << 16) + ('X' << 24);
  CDR (14) = 'X' + ('X' << 8) + ('X' << 16) + ('X' << 24);

  TYPE (14) = 'X' + ('X' << 8) + ('X' << 16) + ('X' << 24);
  CAR (14) = 'X' + ('X' << 8) + ('X' << 16) + ('X' << 24);
  CDR (14) = 'X' + ('X' << 8) + ('X' << 16) + ('X' << 24);

  TYPE (16) = '<' + ('<' << 8) + ('<' << 16) + ('<' << 24);
  CAR (16) = '-' + ('-' << 8) + ('-' << 16) + ('-' << 24);
  CDR (16) = '-' + ('-' << 8) + ('-' << 16) + ('-' << 24);

  TYPE (10) = 0xffffffff; //yeah, -1
  TYPE (10) = 0xfffffffe; //yeah, -2
  TYPE (10) = 0x00000007; //yeah, 7!
  TYPE (10) = 7;          //yeah, 7!
  TYPE (10) = TPAIR;      //yeah, 7!

#else

  TYPE (0) = 0x6c6c6168;
  CAR (0) = 0x6a746f6f;
  CDR (0) = 0x00002165;

  TYPE (1) = TSYMBOL;
  CAR (1) = 0x2d2d2d2d;
  CDR (1) = 0x3e3e3e3e;

  TYPE (9) = 0x2d2d2d2d;
  CAR (9) = 0x2d2d2d2d;
  CDR (9) = 0x3e3e3e3e;

  // (A(B))
  TYPE (10) = TPAIR;
  CAR (10) = 11;
  CDR (10) = 12;

  TYPE (11) = TCHAR;
  CAR (11) = 0x58585858;
  CDR (11) = 89;

  TYPE (12) = TPAIR;
  CAR (12) = 13;
  CDR (12) = 1;

  TYPE (13) = TCHAR;
  CAR (13) = 0x58585858;
  CDR (13) = 90;

  TYPE (14) = 0x58585858;
  CAR (14) = 0x58585858;
  CDR (14) = 0x58585858;

  TYPE (14) = 0x58585858;
  CAR (14) = 0x58585858;
  CDR (14) = 0x58585858;

  TYPE (16) = 0x3c3c3c3c;
  CAR (16) = 0x2d2d2d2d;
  CDR (16) = 0x2d2d2d2d;
#endif

  for (i=0; i<20; i)
    {
      oputs ("i[");
      oputs (itoa (i));
      oputs ("]: ");
      oputs (itoa (TYPE (i)));
      oputs ("\n");
      i = i + 1;
    }

  return 0;
}

SCM
display_ (SCM x)
{
  if (g_debug != 0)
    {
      oputs ("<display>\n");
      oputs (itoa (TYPE (x)));
      oputs ("\n");
    }
  if (TYPE (x) == TCHAR)
    {
      if (g_debug != 0) oputs ("<char>\n");
      oputs ("#\\");
      putchar (VALUE (x));
    }
    else if (TYPE (x) == TFUNCTION)
    {
      if (g_debug != 0) oputs ("<function>\n");
      if (VALUE (x) == 0)
        oputs ("core:make-cell");
      if (VALUE (x) == 1)
        oputs ("cons");
      if (VALUE (x) == 2)
        oputs ("car");
      if (VALUE (x) == 3)
        oputs ("cdr");
    }
  else if (TYPE (x) == TNUMBER)
    {
      if (g_debug != 0) oputs ("<number>\n");
      oputs (itoa (VALUE (x)));
    }
  else if (TYPE (x) == TPAIR)
    {
      if (g_debug != 0) oputs ("<pair>\n");
      //if (cont != cell_f) oputs "(");
      oputs ("(");
      if (x != 0 && x != cell_nil) display_ (CAR (x));
      if (CDR (x) != 0 && CDR (x) != cell_nil)
        {
          if (TYPE (CDR (x)) != TPAIR)
            oputs (" . ");
          display_ (CDR (x));
        }
      //if (cont != cell_f) oputs (")");
      oputs (")");
    }
  else if (TYPE (x) == TSPECIAL)
    {
      if (x == 1) oputs ("()");
      else if (x == 2) oputs ("#f");
      else if (x == 3) oputs ("#t");
      else
        {
          oputs ("<x:");
          oputs (itoa (x));
          oputs (">");
        }
    }
  else if (TYPE (x) == TSYMBOL)
    {
      if (x == 11) oputs (" . ");
      else if (x == 12) oputs ("lambda");
      else if (x == 13) oputs ("begin");
      else if (x == 14) oputs ("if");
      else if (x == 15) oputs ("quote");
      else if (x == 37) oputs ("car");
      else if (x == 38) oputs ("cdr");
      else if (x == 39) oputs ("null?");
      else if (x == 40) oputs ("eq?");
      else if (x == 41) oputs ("cons");
      else
        {
          oputs ("<s:");
          oputs (itoa (x));
          oputs (">");
        }
      }
  else
    {
      if (g_debug != 0) oputs ("<default>\n");
      oputs ("<");
      oputs (itoa (TYPE (x)));
      oputs (":");
      oputs (itoa (x));
      oputs (">");
    }

  return 0;
}

SCM
bload_env (SCM a) ///((internal))
{
#if 1 //!__M2_PLANET__
  oputs ("reading: ");
  char *mo = "module/mes/tiny-0-32.mo";
  oputs (mo);
  oputs ("\n");
  g_stdin = open (mo, 0);
  if (g_stdin < 0) {eputs ("no such file: module/mes/tiny-0-32.mo\n");return 1;}

  // BOOM
  //char *p = arena;
  char *p = g_cells;
  int c;

  c = getchar ();
  putchar (c);
  if (c != 'M') exit (10);
  c = getchar ();
  putchar (c);
  if (c != 'E') exit (11);
  c = getchar ();
  putchar (c);
  if (c != 'S') exit (12);
  oputs (" *GOT MES*\n");

  // skip stack
  getchar ();
  getchar ();

  int i = 0;
  c = getchar ();
  while (c != -1)
    {
      i = i + 1;
      eputs (itoa (i));
      eputs (": ");
      eputs (itoa (c));
      eputs ("\n");
#if __M2_PLANET__
      p[0] = c;
      p = p + 1;
#else
      *p++ = c;
#endif
      c = getchar ();
    }

  oputs ("read done\n");
  display_ (10);

  oputs ("\n");
#endif
  return r2;
}

int
main (int argc, char **argv)
{
  g_debug = argc - 1;
  arena = malloc (300);
  g_cells = arena;
  fill ();
  eputs ("Hello tiny-mes!\n");
  char *p = arena;
  //puts (p);
  oputs ("\n");
  display_ (10);
  oputs ("\n");

  SCM program = bload_env (r0);

  return 0;
}
