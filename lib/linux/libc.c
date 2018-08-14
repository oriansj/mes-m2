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

#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <libmes.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/wait.h>

#if __MESC__

#include <linux/x86-mes/mes.c>

#elif __i386__

#include <linux/x86-mes-gcc/mes.c>

#elif __x86_64__

#include <linux/x86_64-mes-gcc/mes.c>

#else

#error arch not supported

#endif

int
fork ()
{
  return _sys_call (SYS_fork);
}

ssize_t
read (int filedes, void *buffer, size_t size)
{
  ssize_t bytes = _sys_call3 (SYS_read, (long)filedes, (long)buffer, (long)size);
  if (__mes_debug () > 3)
    {
      if (bytes == 1)
        {
          eputs ("read fd="); eputs (itoa ((int)filedes)); eputs (" c="); eputc (*(char*)buffer); eputs ("\n");
        }
      else
        {
          eputs ("read fd="); eputs (itoa ((int)filedes));
          eputs (" bytes="); eputs (itoa (bytes)); eputs ("\n");
        }
    }
  return bytes;
}

int
open (char const *file_name, int flags, ...)
{
  va_list ap;
  va_start (ap, flags);
  int mask = va_arg (ap, int);
#if !MES_BOOTSTRAP
  if (!flags)
    {
      _ungetc_pos = -1;
      _ungetc_fd = -1;
    }
#endif
  int r = _sys_call3 (SYS_open, (long)file_name, (long)flags, (long)mask);
  va_end (ap);
  return r;
}

pid_t
waitpid (pid_t pid, int *status_ptr, int options)
{
#if __i386__
  return _sys_call3 (SYS_waitpid, (long)pid, (long)status_ptr, (long)options);
#elif __x86_64__
  return _sys_call4 (SYS_wait4, (long)pid, (long)status_ptr, (long)options, 0);
#else
#error arch not supported
#endif
}

int
execve (char const* file_name, char *const argv[], char *const env[])
{
  return _sys_call3 (SYS_execve, (long)file_name, (long)argv, (long)env);
}

int
chmod (char const *file_name, mode_t mask)
{
  return _sys_call2 (SYS_chmod, (long)file_name, (long)mask);
}

int
access (char const *file_name, int how)
{
  return _sys_call2 (SYS_access, (long)file_name, (long)how);
}

long
brk (void *addr)
{
  return _sys_call1 (SYS_brk, (long)addr);
}

int
ioctl (int filedes, unsigned long command, ...)
{
  va_list ap;
  va_start (ap, command);
  int data = va_arg (ap, int);
  int r = _sys_call3 (SYS_ioctl, (long)filedes, (long)command, (long)data);
  va_end (ap);
  return r;
}

int
fsync (int filedes)
{
  return _sys_call1 (SYS_fsync, (long)filedes);
}
