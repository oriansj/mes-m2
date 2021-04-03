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

#include <linux/x86/syscall.h>

// *INDENT-OFF*
static long
__sys_call_internal (long sys_call)
{
  long r;
  asm (
       "mov    %1,%%eax\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "rm" (sys_call)
       : "eax"
       );
  return r;
}


static long
__sys_call2_internal (long sys_call, long one, long two)
{
  long r;
  asm (
       "mov    %1,%%eax\n\t"
       "mov    %2,%%ebx\n\t"
       "mov    %3,%%ecx\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "rm" (sys_call), "rm" (one), "rm" (two)
       : "eax", "ebx", "ecx"
       );
  return r;
}
// *INDENT-ON*

/* Return < 0 on error (errno-like value from kernel), or 0 on success */
int
__raise (int signum)
{
  long pid = __sys_call_internal (SYS_getpid);
  if (pid < 0)
    return pid;
  else
    return __sys_call2_internal (SYS_kill, pid, signum);
}
