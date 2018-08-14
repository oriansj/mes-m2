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

#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>

int readchar ();
int unreadchar ();

int
peekchar ()
{
  if (g_stdin >= 0)
    {
      int c = readchar ();
      unreadchar (c);
      return c;
    }
  SCM port = current_input_port ();
  return VALUE (CAR (STRING (port)));
}

int
readchar ()
{
  if (g_stdin >= 0)
    return fdgetc (g_stdin);
  SCM port = current_input_port ();
  SCM string = STRING (port);
  if (string == cell_nil)
    return -1;
  int c = VALUE (CAR (string));
  STRING (port) = CDR (string);
  return c;
}

int
unreadchar (int c)
{
  if (g_stdin >= 0)
    return fdungetc (c, g_stdin);
  SCM port = current_input_port ();
  STRING (port) = cons (MAKE_CHAR (c), STRING (port));
  return c;
}

SCM
peek_byte ()
{
  return MAKE_NUMBER (peekchar ());
}

SCM
read_byte ()
{
  return MAKE_NUMBER (readchar ());
}

SCM
unread_byte (SCM i)
{
  unreadchar (VALUE (i));
  return i;
}

SCM
peek_char ()
{
  return MAKE_CHAR (peekchar ());
}

SCM
read_char ()
{
  return MAKE_CHAR (readchar ());
}

SCM
unread_char (SCM i)
{
  unreadchar (VALUE (i));
  return i;
}

SCM
write_char (SCM i) ///((arity . n))
{
  write_byte (i);
  return i;
}

SCM
read_string ()
{
  SCM lst = cell_nil;
  SCM c = read_char ();
  while (VALUE (c) != -1)
    {
      lst = append2 (lst, cons (c, cell_nil));
      c = read_char ();
    }
  return MAKE_STRING (lst);
}

SCM
write_byte (SCM x) ///((arity . n))
{
  SCM c = car (x);
  SCM p = cdr (x);
  int fd = g_stdout;
  if (TYPE (p) == TPAIR && TYPE (car (p)) == TNUMBER && VALUE (CAR (p)) != 1)
    fd = VALUE (CAR (p));
  char cc = VALUE (c);
  write (fd, (char*)&cc, 1);
#if !__MESC__
  assert (TYPE (c) == TNUMBER || TYPE (c) == TCHAR);
#endif
  return c;
}

char string_to_cstring_buf[4096];
char const*
string_to_cstring_ (SCM s, char *buf)
{
  char *p = buf;
  s = STRING(s);
  while (s != cell_nil)
    {
      *p++ = VALUE (car (s));
      s = cdr (s);
    }
  *p = 0;
  return buf;
}

char const*
string_to_cstring (SCM s)
{
  return string_to_cstring_ (s, string_to_cstring_buf);
}

SCM
getenv_ (SCM s) ///((name . "getenv"))
{
  char *p;
  p = getenv (string_to_cstring (s));
  return p ? MAKE_STRING (cstring_to_list (p)) : cell_f;
}

SCM
setenv_ (SCM s, SCM v) ///((name . "setenv"))
{
  char buf[1024];
  strcpy (buf, string_to_cstring (s));
  setenv (buf, string_to_cstring (v), 1);
  return cell_unspecified;
}

SCM
access_p (SCM file_name, SCM mode)
{
  return access (string_to_cstring (file_name), VALUE (mode)) == 0 ? cell_t : cell_f;
}

SCM
current_input_port ()
{
  if (g_stdin >= 0)
    return MAKE_NUMBER (g_stdin);
  SCM x = g_ports;
  while (x && PORT (CAR (x)) != g_stdin)
    x = CDR (x);
  return CAR (x);
}

SCM
open_input_file (SCM file_name)
{
  return MAKE_NUMBER (open (string_to_cstring (file_name), O_RDONLY));
}

SCM
open_input_string (SCM string)
{
  SCM port = MAKE_STRING_PORT (STRING (string));
  g_ports = cons (port, g_ports);
  return port;
}

SCM
set_current_input_port (SCM port)
{
  SCM prev = current_input_port ();
  if (TYPE (port) == TNUMBER)
    g_stdin = VALUE (port) ? VALUE (port) : STDIN;
  else if (TYPE (port) == TPORT)
    g_stdin = PORT (port);
  return prev;
}

SCM
current_output_port ()
{
  return MAKE_NUMBER (g_stdout);
}

SCM
open_output_file (SCM x) ///((arity . n))
{
  SCM file_name = car (x);
  x = cdr (x);
  int mode = S_IRUSR|S_IWUSR;
  if (TYPE (x) == TPAIR && TYPE (car (x)) == TNUMBER)
    mode = VALUE (car (x));
  return MAKE_NUMBER (open (string_to_cstring (file_name), O_WRONLY|O_CREAT|O_TRUNC,mode));
}

SCM
set_current_output_port (SCM port)
{
  g_stdout = VALUE (port) ? VALUE (port) : STDOUT;
  return current_output_port ();
}

SCM
force_output (SCM p) ///((arity . n))
{
  return cell_unspecified;
}

SCM
chmod_ (SCM file_name, SCM mode) ///((name . "chmod"))
{
  chmod (string_to_cstring (file_name), VALUE (mode));
  return cell_unspecified;
}

SCM
isatty_p (SCM port)
{
  return isatty (VALUE (port)) ? cell_t : cell_f;
}

SCM
primitive_fork ()
{
  return MAKE_NUMBER (fork ());
}

SCM
execl_ (SCM file_name, SCM args) ///((name . "execl"))
{
  char *c_argv[1000];           // POSIX minimum 4096
  int i = 0;
  int n = 0;

  if (length__ (args) > 1000)
    error (cell_symbol_system_error,
           cons (file_name,
                 cons (MAKE_STRING (cstring_to_list ("too many arguments")),
                       cons (file_name, args))));
  c_argv[i++] = (char*)string_to_cstring_ (file_name, string_to_cstring_buf+n);
  n += length__ (STRING (file_name)) + 1;
  while (args != cell_nil)
    {
      assert (TYPE (CAR (args)) == TSTRING);
      c_argv[i++] = (char*)string_to_cstring_ (CAR (args), string_to_cstring_buf+n);
      n += length__ (STRING (CAR (args))) + 1;
      args = CDR (args);
      if (g_debug > 2)
        {
          eputs ("arg["); eputs (itoa (i)); eputs ("]: "); eputs (c_argv[i-1]); eputs ("\n");
        }
    }
  c_argv[i] = 0;
  return MAKE_NUMBER (execv (c_argv[0], c_argv));
}

SCM
waitpid_ (SCM pid, SCM options)
{
  int status;
  int child = waitpid (VALUE (pid), &status, VALUE (options));
  return cons (MAKE_NUMBER (child), MAKE_NUMBER (status));
}
