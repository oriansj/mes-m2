/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <errno.h>
#include <linux/x86_64/syscall.h>

long
__sys_call (long sys_call)
{
#if 0                           // !MES_CCAMD64
  asm ("mov____0x8(%rbp),%rdi !0x10");
#else
  asm ("mov____0x8(%rbp),%rax !0x10");
#endif

  asm ("syscall");
}

long
__sys_call1 (long sys_call, long one)
{
#if 0                           // !MES_CCAMD64
  asm ("mov____0x8(%rbp),%rdi !0x10");
  asm ("mov____0x8(%rbp),%rsi !0x18");
#else
  asm ("mov____0x8(%rbp),%rax !0x10");
  asm ("mov____0x8(%rbp),%rdi !0x18");
#endif

  asm ("syscall");
}

long
__sys_call2 (long sys_call, long one, long two)
{
#if 0                           // !MES_CCAMD64
  asm ("mov____0x8(%rbp),%rdi !0x10");
  asm ("mov____0x8(%rbp),%rsi !0x18");
  asm ("mov____0x8(%rbp),%rdx !0x20");
#else
  asm ("mov____0x8(%rbp),%rax !0x10");
  asm ("mov____0x8(%rbp),%rdi !0x18");
  asm ("mov____0x8(%rbp),%rsi !0x20");
#endif

  asm ("syscall");
}

long
__sys_call3 (long sys_call, long one, long two, long three)
{
#if 0                           // !MES_CCAMD64
  asm ("mov____0x8(%rbp),%rdi !0x10");
  asm ("mov____0x8(%rbp),%rsi !0x18");
  asm ("mov____0x8(%rbp),%rdx !0x20");
  asm ("mov____0x8(%rbp),%rdx !0x28");
#else
  asm ("mov____0x8(%rbp),%rax !0x10");
  asm ("mov____0x8(%rbp),%rdi !0x18");
  asm ("mov____0x8(%rbp),%rsi !0x20");
  asm ("mov____0x8(%rbp),%rdx !0x28");
#endif

  asm ("syscall");
}

long
__sys_call4 (long sys_call, long one, long two, long three, long four)
{
#if 0                           // !MES_CCAMD64
  asm ("mov____0x8(%rbp),%rdi !0x10");
  asm ("mov____0x8(%rbp),%rsi !0x18");
  asm ("mov____0x8(%rbp),%rdx !0x20");
  asm ("mov____0x8(%rbp),%rdx !0x28");
  asm ("mov____0x8(%rbp),%r10 !0x30");
#else
  asm ("mov____0x8(%rbp),%rax !0x10");
  asm ("mov____0x8(%rbp),%rdi !0x18");
  asm ("mov____0x8(%rbp),%rsi !0x20");
  asm ("mov____0x8(%rbp),%rdx !0x28");
  asm ("mov____0x8(%rbp),%r10 !0x30");
#endif

  asm ("syscall");
}

long
_sys_call (long sys_call)
{
  long r = __sys_call (sys_call);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call1 (long sys_call, long one)
{
  long r = __sys_call1 (sys_call, one);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call2 (long sys_call, long one, long two)
{
  long r = __sys_call2 (sys_call, one, two);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call3 (long sys_call, long one, long two, long three)
{
  long r = __sys_call3 (sys_call, one, two, three);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call4 (long sys_call, long one, long two, long three, long four)
{
  long r = __sys_call4 (sys_call, one, two, three, four);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}
