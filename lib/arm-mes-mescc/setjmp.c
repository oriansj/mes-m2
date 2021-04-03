/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
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
#include <stdlib.h>

void
longjmp (jmp_buf env, int val)
{
  val = val == 0 ? 1 : val;
  asm ("!0xc ldr____%r0,(%fp,+#$i8)"); // val
  asm ("!0x8 ldr____%fp,(%fp,+#$i8)"); // env

  asm ("!0x8 ldr____%sp,(%fp,+#$i8)"); // env.__sp
  /* setjmp's ENV argument is missing in setjmp caller's frame.  Re-add it. */
  asm ("pop____%lr"); // junk
  asm ("push___%ebp");
  asm ("!0x4 ldr____%lr,(%fp,+#$i8)"); // env.__lr

  asm ("!0x0 ldr____%fp,(%fp,+#$i8)"); // env.__fp

  asm ("push___%lr");
  /* Avoid function epilogue */
  asm ("ret");
}

int
setjmp (__jmp_buf * env)
{
  /* Function prelude emitter emits: push %lr; push %fp; %fp := %sp */

  long *p = (long*)&env; // location of parameter on stack
  env[0].__fp = p[-2]; // frame of caller of setjmp
  env[0].__lr = p[-1]; // caller of setjmp
  env[0].__sp = p; // stack of caller of setjmp (location of ENV value)

  /* Function epilogue emitter emits: %sp := %fp; pop %fp; pop %lr; %pc := %lr.
     That means that all of the setjmp state is gone after we return from setjmp.
     Once we enter longjmp, we can't use setjmp state and have to reconstruct
     the state of setjmp's call site instead. */
  return 0;
}
