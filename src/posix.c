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

#include "mes/mes.h"
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

char*
cast_intp_to_charp (int const *i)
{
  return i;
}

char*
cast_long_to_charp (long i)
{
  return i;
}

long
cast_charp_to_long (char const *i)
{
  return i;
}

long
cast_int_to_long (int i)
{
  return i;
}

long
cast_voidp_to_long (void const *i)
{
  return i;
}

void
assert_msg (int bool, char *msg)
{
  if (bool)
  {
    fputs(msg, stderr);
    exit(EXIT_FAILURE);
  }
}


size_t
__mesabi_uldiv (size_t a, size_t b, size_t *remainder)
{
  remainder[0] = a % b;
  return a / b;
}

char *__itoa_buf;


char *
ntoab (long x, unsigned base, int signed_p)
{
  if (__itoa_buf == 0)
    __itoa_buf = malloc (20);
  char *p = __itoa_buf + 11;

  p[0] = 0;
  p = p - 1;
  assert_msg (base > 0, "base > 0");

  int sign_p = 0;
  size_t i;
  size_t u;
  size_t b = base;
  if (signed_p != 0 && x < 0)
    {
      sign_p = 1;
      /* Avoid LONG_MIN */
      u = (-(x + 1));
      u = u + 1;
    }
  else
    u = x;

  do
    {
      u = __mesabi_uldiv (u, b, &i);
      if (i > 9)
        p[0] = 'a' + i - 10;
      else
        p[0] = '0' + i;
      p = p - 1;
    }
  while (u != 0);

  if (sign_p && p[1] != '0')
    {
      p[0] = '-';
      p = p - 1;
    }

  return p + 1;
}


char *
ltoa (long x)
{
  return ntoab (x, 10, 1);
}

char *
itoa (int x)
{
  return ntoab (x, 10, 1);
}

struct scm *
set_current_input_port (struct scm *port)
{
//  struct scm *prev = __stdin;
  __stdin = port->name_cdr;
// return prev;
  return port;
}

struct scm *
open_input_file (struct scm *file_name)
{
  if(FUZZING) return make_file(stdin);
  char* s = cell_bytes (file_name->string);
  FILE* f = fopen(s, "r");
  if (NULL == f)
    error (cell_symbol_system_error, cons (make_string0 ("No such file or directory"), file_name));
  return make_file (f);
}

struct scm *
init_time (struct scm *a)               /*:((internal)) */
{
  a = acons (cell_symbol_internal_time_units_per_second, make_number (1000), a);
  return a;
}

struct scm *
peek_byte ()
{
  return make_number (peek(__stdin));
}

struct scm *
read_byte ()
{
  return make_number (fgetc(__stdin));
}

struct scm *
unread_byte (struct scm *i)
{
  ungetc(i->value, __stdin);
  return i;
}

struct scm *
peek_char ()
{
  return make_char (peek(__stdin));
}

struct scm *
read_char (struct scm *port)            /*:((arity . n)) */
{
  FILE* fd = __stdin;
  if (port->type == TPAIR)
    if (port->car->type == TNUMBER)
      __stdin = port->car->name_cdr;
  struct scm *c = make_char (fgetc(__stdin));
  __stdin = fd;
  return c;
}

struct scm *
unread_char (struct scm *i)
{
  ungetc(i->value, __stdin);
  return i;
}

struct scm *
write_char (struct scm *i)              /*:((arity . n)) */
{
  write_byte (i);
  return i;
}

struct scm *
write_byte (struct scm *x)              /*:((arity . n)) */
{
  struct scm *c = car (x);
  struct scm *p = cdr (x);
  FILE* fd = __stdout;
  if (p->type == TPAIR)
    {
      struct scm *f = p->car;
      if (f->type == TNUMBER)
        {
          FILE* v = f->name_cdr;
          if (v != stdout)
            fd = v;
          if (v == stderr)
            fd = __stderr;
        }
    }
  char cc = c->value;
  if(!FUZZING) fputc(cc, __stdout);
#if !__MESC__
  assert_msg (c->type == TNUMBER || c->type == TCHAR, "c->type == TNUMBER || c->type == TCHAR");
#endif
  return c;
}

struct scm *
getenv_ (struct scm *s)                 /*:((name . "getenv")) */
{
  char *p;
  p = getenv (cell_bytes (s->string));
  if (p != 0)
    return make_string0 (p);
  return cell_f;
}

struct scm *
setenv_ (struct scm *s, struct scm *v)          /*:((name . "setenv")) */
{
  if(FUZZING) return cell_unspecified;
  char *buf = __setenv_buf;
  strcpy (buf, cell_bytes (s->string));
  setenv (buf, cell_bytes (v->string), 1);
  return cell_unspecified;
}

struct scm *
access_p (struct scm *file_name, struct scm *mode)
{
  int result = access (cell_bytes (file_name->string), mode->value);
  if (result == 0)
    return cell_t;
  return cell_f;
}

struct scm *
current_input_port ()
{
  if (NULL != __stdin)
    return make_file (__stdin);
  struct scm *x = g_ports;
  struct scm *a;
  while (x != 0)
    {
      a = x->car;
      if (a->name_car == __stdin)
        return a;
      x = x->cdr;
    }
  return x->car;
}

struct scm *
current_output_port ()
{
  return make_file (__stdout);
}

struct scm *
current_error_port ()
{
  return make_file (__stderr);
}

struct scm *
open_input_string (struct scm *string)
{
  struct scm *port = make_string_port (string);
  g_ports = cons (port, g_ports);
  return port;
}

struct scm *
open_output_file (struct scm *x)        /*:((arity . n)) */
{
  if(FUZZING) return make_file(stdin);
  struct scm *file_name = car (x);
  x = cdr (x);
  char* mode = "w";
  FILE* f;
//  if (x->type == TPAIR)
//    {
//      struct scm *i = car (x);
//      if (i->type == TNUMBER)
//        mode = i->value;
//    }
  char* s = cell_bytes (file_name->string);
  f = fopen (s, mode);
  if(NULL == f)
    error (cell_symbol_system_error, cons (make_string0 ("unable to create such file"), file_name));

  return make_file (f);
}

struct scm *
set_current_output_port (struct scm *port)
{
  if (NULL != port->name_cdr)
    __stdout = port->name_cdr;
  else
    __stdout = stdout;
  return current_output_port ();
}

struct scm *
set_current_error_port (struct scm *port)
{
  if (NULL != port->name_cdr)
    __stderr = port->name_cdr;
  else
    __stderr = stderr;
  return current_error_port ();
}

struct scm *
chmod_ (struct scm *file_name, struct scm *mode)        /*:((name . "chmod")) */
{
  if(FUZZING) return cell_unspecified;
  char* s = cell_bytes (file_name->string);
  chmod (s, mode->value);
  return cell_unspecified;
}

struct scm *
isatty_p (struct scm *port)
{
  if (isatty (port->value) != 0)
    return cell_t;
  return cell_f;
}

struct scm *
primitive_fork ()
{
  if(FUZZING) return make_number(0);
  return make_number (fork ());
}

struct scm *
execl_ (struct scm *file_name, struct scm *args)        /*:((name . "execl")) */
{
  char **c_argv = __execl_c_argv;
  int i = 0;

  if (length__ (args) > 1000)
    error (cell_symbol_system_error,
           cons (file_name, cons (make_string0 ("too many arguments"), cons (file_name, args))));
  c_argv[i] = cell_bytes (file_name->string);
  i = i + 1;
  struct scm *arg;
  while (args != cell_nil)
    {
      assert_msg (args->car->type == TSTRING, "args->car->type == TSTRING");
      arg = args->car;
      c_argv[i] = cell_bytes (arg->string);
      i = i + 1;
      args = args->cdr;
      if (g_debug > 2)
        {
          eputs ("arg[");
          eputs (itoa (i));
          eputs ("]: ");
          eputs (c_argv[i - 1]);
          eputs ("\n");
        }
    }
  c_argv[i] = 0;
  if(FUZZING) return make_number (0);
  return make_number (execv (c_argv[0], c_argv));
}

struct scm *
waitpid_ (struct scm *pid, struct scm *options)
{
  int status;
  int child = waitpid (pid->value, &status, options->value);
  return cons (make_number (child), make_number (status));
}


struct scm *
current_time ()
{
  return make_number (0);
}

struct scm *
gettimeofday_ ()
{
  return cons (make_number (0), make_number (0));
}

long
seconds_and_nanoseconds_to_long (long s, long ns)
{
  return 0;
}

struct scm *
get_internal_run_time ()
{
  return make_number (0);
}

struct scm *
getcwd_ ()                      /*:((name . "getcwd")) */
{
  return make_string0 (getcwd (0, 4096));
}

struct scm *
dup_ (struct scm *port)                 /*:((name . "dup")) */
{
  if(FUZZING) return cell_unspecified;
  return make_number (dup (port->value));
}

struct scm *
dup2_ (struct scm *old, struct scm *new)        /*:((name . "dup2")) */
{
  if(FUZZING) return cell_unspecified;
  dup2 (old->value, new->value);
  return cell_unspecified;
}

struct scm *
delete_file (struct scm *file_name)
{
  if(FUZZING) return cell_unspecified;
  unlink (cell_bytes (file_name->string));
  return cell_unspecified;
}

struct scm *
exit_ (struct scm *x)                   /*:((name . "exit")) */
{
  assert_msg (x->type == TNUMBER, "x->type == TNUMBER");
  exit (x->value);
}
