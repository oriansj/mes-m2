/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#ifndef __MES_SETJMP_H
#define __MES_SETJMP_H 1

#if WITH_GLIBC
#undef __MES_SETJMP_H
#include_next <setjmp.h>
#else // ! WITH_GLIBC

typedef struct
{
  int __bp;
  int __pc;
  int __sp;
} __jmp_buf;
typedef __jmp_buf jmp_buf[1];

#if __MESC__
__jmp_buf buf[1];
#else
jmp_buf buf;
#endif

void longjmp (jmp_buf env, int val);
int setjmp (jmp_buf env);

#endif // ! WITH_GLIBC

#endif // __MES_SETJMP_H

