/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <unistd.h>

int errno;

void
longjmp (jmp_buf env, int val)
{
  val = val == 0 ? 1 : val;
  asm ("mov____0x8(%ebp),%eax !0x0c"); // val
  asm ("mov____0x8(%ebp),%ebp !0x08"); // env*

  asm ("mov____0x8(%ebp),%ebx !0x4");  // env.__pc
  asm ("mov____0x8(%ebp),%esp !0x8");  // env.__sp
  asm ("mov____0x8(%ebp),%ebp !0x0");  // env.__bp
  asm ("jmp____*%ebx");
  // not reached
  exit (42);
}

#if 0
int
setjmp_debug (jmp_buf env, int val)
{
  int i;
#if 1
  i = env->__bp;
  i = env->__pc;
  i = env->__sp;
#else
  i = env[0].__bp;
  i = env[0].__pc;
  i = env[0].__sp;
#endif
  return val == 0 ? 1 : val;
}
#endif

int
setjmp (__jmp_buf *env)
{
  int *p = (int*)&env;
  env[0].__bp = p[-2];
  env[0].__pc = p[-1];
  env[0].__sp = (long)&env;
  return 0;
}
