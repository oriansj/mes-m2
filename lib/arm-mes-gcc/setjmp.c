/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2019,2020 Danny Milosavljevic <dannym@scratchpost.org>
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

#ifndef HAVE_FLOAT
#define HAVE_FLOAT 0
#endif

#if !__TINYC__
void
__attribute__ ((noinline))
longjmp (jmp_buf env, int val)
{
  // *INDENT-OFF*
  asm (
       "mov r0, %0\n\t"
       "mov r1, %1\n\t"
       "cmp r1, #0\n\t"
       "moveq r1, #1\n\t" /* returning 0 is not allowed, even when the user wanted to. */
       "ldr r13, [r0], #4\n\t" /* stack pointer (sp) */
       "ldr r14, [r0], #4\n\t" /* link register (lr) */
       "ldmia r0!, {r4, r5, r6, r7, r8, r9, r10, r11}\n\t"
#if HAVE_FLOAT
       // TODO: If using VFP, vldmia r0!, {d8-d15}
#endif
       "mov r0, r1\n\t"
       :
       : "r" (env), "r" (val));
  // *INDENT-ON*
  // not reached
}

#else //__TINYC__

#ifndef SETJMP_ASM
#define SETJMP_ASM 0
#endif

#if SETJMP_ASM

__asm__ (".global longjmp\n");
__asm__ ("longjmp:\n");
__asm__ (".int 0xe52db004\n"); //push  {fp}      ; (str fp, [sp, #-4]!)
__asm__ (".int 0xe28db000\n"); //add   fp, sp, #0
__asm__ (".int 0xe24dd00c\n"); //sub   sp, sp, #12
__asm__ (".int 0xe50b0008\n"); //str   r0, [fp, #-8]
__asm__ (".int 0xe50b100c\n"); //str   r1, [fp, #-12]
__asm__ (".int 0xe51b3008\n"); //ldr   r3, [fp, #-8]
__asm__ (".int 0xe51b200c\n"); //ldr   r2, [fp, #-12]
__asm__ (".int 0xe1a00003\n"); //mov   r0, r3
__asm__ (".int 0xe1a01002\n"); //mov   r1, r2
__asm__ (".int 0xe3510000\n"); //cmp   r1, #0
__asm__ (".int 0x03a01001\n"); //moveq r1, #1
__asm__ (".int 0xe490d004\n"); //ldr   sp, [r0], #4
__asm__ (".int 0xe490e004\n"); //ldr   lr, [r0], #4
__asm__ (".int 0xe8b00ff0\n"); //ldm   r0!, {r4, r5, r6, r7, r8, r9, sl, fp}
#if HAVE_FLOAT
// TODO: If using VFP, vldmia r0!, {d8-d15}
#endif
__asm__ (".int 0xe1a00001\n"); //mov   r0, r1
__asm__ (".int 0xe12fff1e\n"); //bx    lr

#else //!SETJMP_ASM

void
__attribute__ ((noinline))
longjmp (jmp_buf env, int val)
{
  __asm__ (".int 0xe3510000\n"); //cmp   r1, #0
  __asm__ (".int 0x03a01001\n"); //moveq r1, #1
  __asm__ (".int 0xe490d004\n"); //ldr   sp, [r0], #4
  __asm__ (".int 0xe490e004\n"); //ldr   lr, [r0], #4
  __asm__ (".int 0xe8b00ff0\n"); //ldm   r0!, {r4, r5, r6, r7, r8, r9, sl, fp}
#if HAVE_FLOAT
  // TODO: If using VFP, vldmia r0!, {d8-d15}
#endif
  __asm__ (".int 0xe1a00001\n"); //mov   r0, r1
  __asm__ (".int 0xe12fff1e\n"); //bx    lr
}

#endif //!SETJMP_ASM
#endif //__TINYC__

#if !__TINYC__
int
__attribute__ ((noinline))
setjmp (jmp_buf env)
{
  // *INDENT-OFF*
  asm (
       "mov r0, %0\n\t"
       "str r13, [r0], #4\n\t" /* stack pointer (sp) */
       "str r14, [r0], #4\n\t" /* link register (lr) */
       "stmia r0!, {r4, r5, r6, r7, r8, r9, r10, r11}\n\t"
#if HAVE_FLOAT
       // TODO: If using VFP, vstmia r0!, {d8-d15}
#endif
       :
       : "r" (env)
       : "r0");
  // *INDENT-ON*
  return 0;
}
#else //__TINYC__

#if SETJMP_ASM

__asm__ (".global setjmp\n");
__asm__ ("setjmp:\n");
__asm__ (".int 0xe52db004\n"); //push  {fp}      ; (str fp, [sp, #-4]!)
__asm__ (".int 0xe28db000\n"); //add   fp, sp, #0
__asm__ (".int 0xe24dd00c\n"); //sub   sp, sp, #12
__asm__ (".int 0xe50b0008\n"); //str   r0, [fp, #-8]
__asm__ (".int 0xe51b3008\n"); //ldr   r3, [fp, #-8]
__asm__ (".int 0xe1a00003\n"); //mov   r0, r3
__asm__ (".int 0xe480d004\n"); //str   sp, [r0], #4
__asm__ (".int 0xe480e004\n"); //str   lr, [r0], #4
__asm__ (".int 0xe28bd000\n"); //add   sp, fp, #0
__asm__ (".int 0xe49db004\n"); //pop   {fp}      ; (ldr fp, [sp], #4)
__asm__ (".int 0xe8a00ff0\n"); //stmia r0!, {r4, r5, r6, r7, r8, r9, sl, fp}
#if HAVE_FLOAT
// TODO: If using VFP, vstmia r0!, {d8-d15}
#endif
__asm__ (".int 0xe1a0b00d\n"); //mov   fp, sp
__asm__ (".int 0xe3a03000\n"); //mov   r3, #0
__asm__ (".int 0xe1a00003\n"); //mov   r0, r3
__asm__ (".int 0xe12fff1e\n"); //bx    lr

#else //!SETJMP_ASM

int
__attribute__ ((noinline))
setjmp (jmp_buf env)
{
  __asm__ (".int 0xe480d004\n"); //str   sp, [r0], #4
  __asm__ (".int 0xe480e004\n"); //str   lr, [r0], #4
  //__asm__ (".int 0xe1a0300b\n"); //mov  r3, fp
  __asm__ (".int 0xe59bb000\n"); //ldr   fp, [fp]
  __asm__ (".int 0xe8a00ff0\n"); //stmia r0!, {r4, r5, r6, r7, r8, r9, sl, fp}
#if HAVE_FLOAT
  // TODO: If using VFP, vstmia r0!, {d8-d15}
#endif
  __asm__ (".int 0xe1a0b00d\n"); //mov   fp, sp
  //__asm__ (".int 0xe1a0b003\n"); //mov  fp, r3
  __asm__ (".int 0xe3a00000\n"); // mov   r0, #0
}

#endif //!SETJMP_ASM
#endif //__TINYC__
