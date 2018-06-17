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

#include <stdarg.h>
#include <stdio.h>
#include <libmes.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/wait.h>

#define SYS_fork    0x02
#define SYS_read    0x03
#define SYS_open    0x05
#define SYS_waitpid 0x07
#define SYS_execve  0x0b
#define SYS_chmod   0x0f
#define SYS_access  0x21
#define SYS_brk     0x2d
#define SYS_ioctl   0x36
#define SYS_fsync   0x76

int
fork ()
{
  return _sys_call (SYS_fork);
}

ssize_t
read (int filedes, void *buffer, size_t size)
{
  return _sys_call3 (SYS_read, (int)filedes, (int)buffer, (int)size);
}

int
open (char const *file_name, int flags, ...)
{
  va_list ap;
  va_start (ap, flags);
  int mask = va_arg (ap, int);
  int r = _sys_call3 (SYS_open, (int)file_name, (int)flags, (int)mask);
  va_end (ap);
  return r;
}

pid_t
waitpid (pid_t pid, int *status_ptr, int options)
{
  return _sys_call3 (SYS_waitpid, (int)pid, (int)status_ptr, (int)options);
}

int
execve (char const* file_name, char *const argv[], char *const env[])
{
  return _sys_call3 (SYS_execve, (int)file_name, (int)argv, (int)env);
}

int
chmod (char const *file_name, mode_t mask)
{
  return _sys_call2 (SYS_chmod, (int)file_name, (int)mask);
}

int
access (char const *file_name, int how)
{
  return _sys_call2 (SYS_access, (int)file_name, (int)how);
}

int
brk (void *addr)
{
  return _sys_call1 (SYS_brk, (int)addr);
}

int
ioctl (int filedes, unsigned long command, ...)
{
  va_list ap;
  va_start (ap, command);
  int data = va_arg (ap, int);
  int r = _sys_call3 (SYS_ioctl, (int)filedes, (int)command, (int)data);
  va_end (ap);
  return r;
}

int
fsync (int filedes)
{
  return _sys_call1 (SYS_fsync, (int)filedes);
}
