/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

char **environ = 0;
int main (int argc, char *argv[]);

#if __GNUC__

void
_start ()
{
  asm (
       "mov     %%ebp,%%eax\n\t"
       "addl    $4,%%eax\n\t"
       "movzbl  (%%eax),%%eax\n\t"
       "addl    $3,%%eax\n\t"
       "shl     $2,%%eax\n\t"
       "add     %%ebp,%%eax\n\t"
       "movl    %%eax,%0\n\t"
       : "=environ" (environ)
       : //no inputs ""
       );
  asm (
       "mov     %%ebp,%%eax\n\t"
       "addl    $8,%%eax\n\t"
       "push    %%eax\n\t"

       "mov     %%ebp,%%eax\n\t"
       "addl    $4,%%eax\n\t"
       "movzbl  (%%eax),%%eax\n\t"
       "push    %%eax\n\t"

       "call    main\n\t"

       "mov     %%eax,%%ebx\n\t"
       "mov     $1,%%eax\n\t"
       "int     $0x80\n\t"
       "hlt      \n\t"
       :
       );
}

#elif __MESC__

int
_start ()
{
  asm ("mov____%ebp,%eax");
  asm ("add____$i8,%eax !4");

  asm ("movzbl_(%eax),%eax");
  asm ("add____$i8,%eax !3");

  asm ("shl____$i8,%eax !0x02");
  asm ("add____%ebp,%eax");
  asm ("mov____%eax,0x32 &environ");

  asm ("mov____%ebp,%eax");
  asm ("add____$i8,%eax !8");
  asm ("push___%eax");

  asm ("mov____%ebp,%eax");
  asm ("add____$i8,%eax !4");
  asm ("movzbl_(%eax),%eax");
  asm ("push___%eax");

  main ();

  asm ("mov____%eax,%ebx");
  asm ("mov____$i32,%eax %1");
  asm ("int____$0x80");
  asm ("hlt");
}

#endif
