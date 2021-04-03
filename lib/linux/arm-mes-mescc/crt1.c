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

#include "mes/lib-mini.h"

int main (int argc, char *argv[], char *envp[]);

void /* must not return */
_start ()
{
/*

sp+1    argv UNROLLED ON STACK
sp   -> argc

environ = &argv[argc + 1]

HOWEVER, the function entry already allocated space for locals on the stack (after saving lr and fp, which moved sp again).  Hence, use fp instead of sp.
*/

  /* stdin = 0 */

  asm ("!0 mov____$i8,%r0");
  asm ("mov____%r0,0x32 &__stdin");

  /* stdout = 1 */

  asm ("!1 mov____$i8,%r0");
  asm ("mov____%r0,0x32 &__stdout");

  /* stderr = 2 */

  asm ("!2 mov____$i8,%r0");
  asm ("mov____%r0,0x32 &__stderr");

  /* Add "environ" to main's arguments */

  asm ("!8 ldr____%r0,(%fp,+#$i8)"); /* "argc" */
  asm ("mov____%fp,%r1");
  asm ("!12 add____%r1,$i8"); /* argv */
  asm ("add____%r2,%r1,%r0,lsl#2"); /* "environ": argv + argc */
  asm ("!4 add____%r2,$i8"); /* "environ": argv + argc + 1 */

  asm ("push___%r2"); /* environ */
  asm ("push___%r1"); /* argv */
  asm ("push___%r0"); /* argc */

  /* environ = r2 */

  asm ("mov____%r2,0x32 &environ");

  main ();

  asm ("SYS_exit mov____$i8,%r7");
  asm ("swi____$0");
  do
    {
      asm ("wfi");
    }
  while (1);
}
