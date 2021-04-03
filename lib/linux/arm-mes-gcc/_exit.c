/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2019,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include "mes/lib-mini.h"

// *INDENT-OFF*
#if !__TINYC__
#define SYS_exit "0x01"
void
_exit (int code)
{
  asm (
       "mov    r7, $"SYS_exit"\n\t"
       "mov    r0, %0\n\t"
       "swi    $0\n\t"
       : // no outputs "=" (r)
       : "r" (code)
       : "r0", "r7"
       );
  // not reached
  _exit (0);
}
#else //__TINYC__
#define SYS_exit 0x01
void
_exit (int code)
{
  int c = SYS_exit;
  __asm__ (".int 0xe51b7004\n"); //ldr   r7, [fp, #-4] ; "c"
  __asm__ (".int 0xe59b000c\n"); //ldr   r0, [fp, #12] ; code
  __asm__ (".int 0xef000000\n"); //svc   0x00000000
}
#endif //__TINYC__
