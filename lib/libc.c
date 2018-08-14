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

#include <sys/ioctl.h>
#include <stdarg.h>
#include <stdlib.h>
#include <libmes.h>
#include <stdio.h>

#include <libmes.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>

#include <libc-mini.c>
#include <libmes.c>

#if __GNU__
#include <hurd/libc.c>
#elif __linux__
#include <linux/libc.c>
#else
#error both __GNU__ and _linux__ are undefined, choose one
#endif

int g_stdin = 0;

void _env ();

#define MAX(a, b) (((a) > (b)) ? (a) : (b))

int
__mes_debug ()
{
  static int __mes_debug = -1;
  if (__mes_debug == -1)
    {
      char *p = getenv ("MES_DEBUG");
      __mes_debug = p ? MAX (atoi (p), 1) : 0;
    }
  return __mes_debug;
}


#if !___GNU__
#include <string/memcpy.c>
#include <stdlib/malloc.c>
#include <assert/assert.c>
#endif

int
getchar ()
{
  return fdgetc (g_stdin);
}

int
putchar (int c)
{
  write (STDOUT, (char*)&c, 1);
  return 0;
}

int
fputc (int c, FILE* stream)
{
  return fdputc (c, (int)stream);
}

int
fputs (char const* s, FILE* stream)
{
  return fdputs (s, (int)stream);
}

int
putc (int c, FILE* stream)
{
  return fdputc (c, (int)stream);
}

int
getc (FILE *stream)
{
  return fdgetc ((int)stream);
}

int
fgetc (FILE *stream)
{
  return fdgetc ((int)stream);
}

void
free (void *ptr)
{
}

int
ungetc (int c, FILE *stream)
{
  return fdungetc (c, (int)stream);
}

int
strcmp (char const* a, char const* b)
{
  while (*a && *b && *a == *b)
    {
      a++;b++;
    }
  return *a - *b;
}

char *
strcpy (char *dest, char const *src)
{
  char *p = dest;
  while (*src) *p++ = *src++;
  *p = 0;
  return dest;
}

void *
realloc (void *ptr, size_t size)
{
  void *new = malloc (size);
  if (ptr && new)
    {
      memcpy (new, ptr, size);
      free (ptr);
    }
  return new;
}

int
strncmp (char const* a, char const* b, size_t size)
{
  if (!size)
    return 0;
  while (*a && *b && *a == *b && --size)
    {
      a++;
      b++;
    }
  return *a - *b;
}

char *
getenv (char const* s)
{
  char **p = environ;
  int length = strlen (s);
  while (*p)
    {
      if (!strncmp (s, *p, length) && *(*p + length) == '=') return (*p + length + 1);
      p++;
    }
  return 0;
}

int
setenv (char const* s, char const* v, int overwrite_p)
{
  char **p = environ;
  int length = strlen (s);
  while (*p)
    {
      if (!strncmp (s, *p, length) && *(*p + length) == '=')
        break;
      p++;
    }
  char *entry = malloc (length + strlen (v) + 2);
  int end_p = *p == 0;
  *p = entry;
  strcpy (entry, s);
  strcpy (entry + length, "=");
  strcpy (entry + length + 1, v);
  *(entry + length + strlen (v) + 2) = 0;
  if (end_p)
    *++p = 0;
  return 0;
}

int
isatty (int fd)
{
  return ioctl (fd, TCGETS, 0) & 0xf0;
}

int
wait (int *status_ptr)
{
  return waitpid  (-1, status_ptr, 0);
}

int
execv (char const *file_name, char *const argv[])
{
  return execve (file_name, argv, environ);
}
