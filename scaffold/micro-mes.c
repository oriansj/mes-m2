/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#if POSIX
#error "POSIX not supported"
#endif

#include <stdio.h>

typedef int SCM;

#if __GNUC__
int g_debug = 0;
#endif

int g_free;

SCM g_symbols;
SCM g_stack;

/* a/env */
SCM r0;
/* param 1 */
SCM r1;
/* save 2+load/dump */
SCM r2;
/* continuation */
SCM r3;

SCM
mes_environment ()
{
  return 0;
}

SCM
bload_env (SCM a) ///((internal))
{
  eputs ("bload_env\n");
  return 0;
}

int
main (int argc, char **argv)
{
  r0 = mes_environment ();
  puts ("Hello micro-mes!\n");
  SCM program = bload_env (r0);
  int i = argc;
  return i;
}
