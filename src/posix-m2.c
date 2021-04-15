/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019,2020,2021 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>

struct scm *
exit_ (struct scm *x)                   /*:((name . "exit")) */
{
  assert_msg (x->type == TNUMBER, "x->type == TNUMBER");
  exit (x->value);
}

int
peekchar ()
{
  if (__stdin >= 0)
    {
      int c = readchar ();
      unreadchar (c);
      return c;
    }
  struct scm *port = current_input_port ();
  struct scm *string = port->string;
  size_t length = string->length;
  if (length == 0)
    return -1;
  char const *p = cell_bytes (string->string);
  return p[0];
}

int
readchar ()
{
  if (__stdin >= 0)
    return fgetc (__stdin);
  struct scm *port = current_input_port ();
  struct scm *string = port->string;
  size_t length = string->length;
  if (length == 0)
    return -1;
  char const *p = cell_bytes (string->string);
  int c = p[0];
  p = p + 1;
  port->string = make_string (p, length - 1);
  return c;
}

int
unreadchar (int c)
{
  if (__stdin >= 0)
    return ungetc (c, __stdin);
  if (c == EOF) /* can't unread EOF */
    return c;
  struct scm *port = current_input_port ();
  struct scm *string = port->string;
  size_t length = string->length;
  char *p = cell_bytes (string->string);
  p = p - 1;
  string = make_string (p, length + 1);
  p = cell_bytes (string->string);
  p[0] = c;
  port->string = string;
  return c;
}

struct scm *
peek_byte ()
{
  return make_number (peekchar ());
}

struct scm *
read_byte ()
{
  return make_number (readchar ());
}

struct scm *
unread_byte (struct scm *i)
{
  unreadchar (i->value);
  return i;
}

struct scm *
peek_char ()
{
  return make_char (peekchar ());
}

struct scm *
read_char (struct scm *port)            /*:((arity . n)) */
{
  FILE* fd = __stdin;
  if (port->type == TPAIR)
    if (port->car->type == TNUMBER)
      __stdin = port->car->name_cdr;
  struct scm *c = make_char (readchar ());
  __stdin = fd;
  return c;
}

struct scm *
unread_char (struct scm *i)
{
  unreadchar (i->value);
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
  write (fd, &cc, 1);
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
  if (__stdin >= 0)
    return make_number (__stdin->_fileno);
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
open_input_file (struct scm *file_name)
{
  int filedes = mes_open (cell_bytes (file_name->string), O_RDONLY, 0);
  if (filedes == -1)
    error (cell_symbol_system_error, cons (make_string0 ("No such file or directory"), file_name));
  return make_number (filedes);
}

struct scm *
open_input_string (struct scm *string)
{
  struct scm *port = make_string_port (string);
  g_ports = cons (port, g_ports);
  return port;
}

struct scm *
set_current_input_port (struct scm *port)
{
  struct scm *prev = current_input_port ();
  if (port->type == TNUMBER)
    {
      FILE* p = port->name_cdr;
      if (p != 0)
        __stdin = p;
      else
        __stdin = stdin;
    }
  else if (port->type == TPORT)
    __stdin = port->name_car;
  return prev;
}

struct scm *
current_output_port ()
{
  return make_number (__stdout->_fileno);
}

struct scm *
current_error_port ()
{
  return make_number (__stderr->_fileno);
}

struct scm *
open_output_file (struct scm *x)        /*:((arity . n)) */
{
  struct scm *file_name = car (x);
  x = cdr (x);
  int mode = S_IRUSR | S_IWUSR;
  if (x->type == TPAIR)
    {
      struct scm *i = car (x);
      if (i->type == TNUMBER)
        mode = i->value;
    }
  return make_number (mes_open (cell_bytes (file_name->string), O_WRONLY | O_CREAT | O_TRUNC, mode));
}

struct scm *
set_current_output_port (struct scm *port)
{
  if (port->value != 0)
    __stdout = port->name_cdr;
  else
    __stdout = stdout;
  return current_output_port ();
}

struct scm *
set_current_error_port (struct scm *port)
{
  if (port->value != 0)
    __stderr = port->name_cdr;
  else
    __stderr = stderr;
  return current_error_port ();
}

struct scm *
chmod_ (struct scm *file_name, struct scm *mode)        /*:((name . "chmod")) */
{
  chmod (cell_bytes (file_name->string), mode->value);
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
  return make_number (execv (c_argv[0], c_argv));
}

struct scm *
waitpid_ (struct scm *pid, struct scm *options)
{
  int status;
  int child = waitpid (pid->value, &status, options->value);
  return cons (make_number (child), make_number (status));
}

#if __x86_64__
/* Nanoseconds on 64-bit systems with POSIX timers.  */
// CONSTANT TIME_UNITS_PER_SECOND 1000000000
#define TIME_UNITS_PER_SECOND 1000000000U
#else
/* Milliseconds for everyone else.  */
// CONSTANT TIME_UNITS_PER_SECOND 1000
#define TIME_UNITS_PER_SECOND 1000U
#endif

struct scm *
init_time (struct scm *a)               /*:((internal)) */
{
  clock_gettime (CLOCK_PROCESS_CPUTIME_ID, g_start_time);
  a = acons (cell_symbol_internal_time_units_per_second, make_number (TIME_UNITS_PER_SECOND), a);
}

struct scm *
current_time ()
{
  return make_number (time (0));
}

struct scm *
gettimeofday_ ()                /*:((name . "gettimeofday")) */
{
  struct timeval *time = __gettimeofday_time;
  gettimeofday (time, 0);
  return cons (make_number (time->tv_sec), make_number (time->tv_usec));
}

#define UL1000000000 1000000000UL
// CONSTANT UL1000000000 1000000000
long
seconds_and_nanoseconds_to_long (long s, long ns)
{
  size_t uns = ns;
  if (ns < 0)
    {
      uns = - ns;
      return s * TIME_UNITS_PER_SECOND - uns / (UL1000000000 / TIME_UNITS_PER_SECOND);
    }
  return s * TIME_UNITS_PER_SECOND + uns / (UL1000000000 / TIME_UNITS_PER_SECOND);
}

struct scm *
get_internal_run_time ()
{
  struct timespec *ts = __get_internal_run_time_ts;
  clock_gettime (CLOCK_PROCESS_CPUTIME_ID, ts);
  long time = seconds_and_nanoseconds_to_long (ts->tv_sec - g_start_time->tv_sec,
                                               ts->tv_nsec - g_start_time->tv_nsec);
  return make_number (time);
}

struct scm *
getcwd_ ()                      /*:((name . "getcwd")) */
{
  return make_string0 (getcwd (0, PATH_MAX));
}

struct scm *
dup_ (struct scm *port)                 /*:((name . "dup")) */
{
  return make_number (dup (port->value));
}

struct scm *
dup2_ (struct scm *old, struct scm *new)        /*:((name . "dup2")) */
{
  dup2 (old->value, new->value);
  return cell_unspecified;
}

struct scm *
delete_file (struct scm *file_name)
{
  unlink (cell_bytes (file_name->string));
  return cell_unspecified;
}
