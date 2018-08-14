/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

// gcc/xgcc wants -lg with all these
// what's the story here?

#include <sys/time.h>

#define exit __exit
#define fprintf _fprintf
#define longjmp _longjmp
#define malloc _malloc
#define printf _printf
#define putchar _putchar
#define puts _puts
#define setjmp _setjmp
#define signal _signal
#define strcmp _strcmp
#define sprintf _sprintf
#define sscanf _sscanf

#include <libc+tcc.c>

#if __GNU__
#include <linux/gnu.c>
#elif __linux__
#include <linux/gnu.c>
#else
#error both __GNU__ and _linux__ are undefined, choose one
#endif

#include <m4.c>
#include <binutils.c>
#include <gcc.c>

int
__cleanup ()
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__cleanup stub\n");
  stub = 1;
  return 0;
}

int
__libc_subinit ()
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__libc_subinit stub\n");
  stub = 1;
  return 0;
}

int
__syscall_error ()
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__syscall_error stub\n");
  stub = 1;
  return 0;
}

int
__fpu_control ()
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__fpu_control stub\n");
  stub = 1;
  return 0;
}
