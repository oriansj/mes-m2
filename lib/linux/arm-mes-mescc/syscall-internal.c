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

static int
__sys_call_internal (int sys_call)
{
  asm ("!8 ldr____%r7,(%fp,+#$i8)");
  asm ("swi____$0");
}

static int
__sys_call2_internal (int sys_call, int one, int two)
{
  asm ("!8 ldr____%r7,(%fp,+#$i8)");
  asm ("!12 ldr____%r0,(%fp,+#$i8)");
  asm ("!16 ldr____%r1,(%fp,+#$i8)");
  asm ("swi____$0");
}

/* Returns < 0 on error (errno-like value from kernel), or 0 on success */
int
__raise (int signum)
{
  int pid = __sys_call_internal (SYS_getpid);
  if (pid < 0)
    return pid;
  else
    return __sys_call2_internal (SYS_kill, pid, signum);
}
