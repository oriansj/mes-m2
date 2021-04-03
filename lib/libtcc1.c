/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <mes/lib.h>

double
__divdi3 (double a, double b)
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__divdi3 stub\n");
  stub = 1;
#endif
  long ai = a;
  long bi = b;
#if __arm__ && __TINYC__
  return __mesabi_idiv (ai, bi);
#else
  return ai / bi;
#endif
}

double
__moddi3 (double a, double b)
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__moddi3 stub\n");
  stub = 1;
#endif
  long ai = a;
  long bi = b;
#if __arm__ && __TINYC__
  return __mesabi_imod (ai, bi);
#else
  return ai % bi;
#endif
}

#if HAVE_LONG_LONG
unsigned long long
__udivdi3 (unsigned long long a, unsigned long long b)
#else
unsigned long
__udivdi3 (unsigned long a, long ah, unsigned long b)
#endif
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__udivdi3 stub\n");
  stub = 1;
#endif
  unsigned long ai = a;
  unsigned long bi = b;
  if (!b)
    return 0;
#if __arm__ && __TINYC__
  return __mesabi_idiv (ai, bi);
#else
  return ai / bi;
#endif
}

#if HAVE_LONG_LONG
unsigned long long
__umoddi3 (unsigned long long a, unsigned long long b)
#else
unsigned long
__umoddi3 (unsigned long a, long ah, unsigned long b)
#endif
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__umoddi3 stub\n");
  stub = 1;
#endif
  unsigned long ai = a;
  unsigned long bi = b;
#if __arm__ && __TINYC__
  return __mesabi_imod (ai, bi);
#else
  return ai % bi;
#endif
}

#if HAVE_LONG_LONG
unsigned long long
__lshrdi3 (unsigned long long a, long b)
#else
unsigned long
__lshrdi3 (unsigned long a, long ah, long b)
#endif
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__lshrdi3 stub\n");
  stub = 1;
  return a >> b;
#else //  __TINYC__
  for (int i = 0; i < b; i++)
#if __arm__
    a = __mesabi_idiv (a, 2);
#else // !__arm__
    a /= 2;
#endif // !__arm__
  return a;
#endif // __TINYC__
}

#if HAVE_LONG_LONG
long long
__ashldi3 (long long a, long b)
#else
long
__ashldi3 (long a, long ah, long b)
#endif
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__ashldi3 stub\n");
  stub = 1;
  return a << b;
#else //  __TINYC__
  for (int i = 0; i < b; i++)
    a += a;
  return a;
#endif // __TINYC__
}

#if HAVE_LONG_LONG
long long
__ashrdi3 (long long a, long b)
#else
long
__ashrdi3 (long a, long ah, long b)
#endif
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__ashrdi3 stub\n");
  stub = 1;
 return a >> b;
#else //  __TINYC__
  for (int i = 0; i < b; i++)
#if __arm__
    a = __mesabi_idiv (a, 2);
#else // !__arm__
    a /= 2;
#endif // !__arm__
  return a;
#endif // __TINYC__
}

#if HAVE_FLOAT_STUB || HAVE_FLOAT
double
__attribute__((weak))
#if HAVE_LONG_LONG && HAVE_FLOAT
__floatundidf (unsigned long long a)
#else
__floatundidf (unsigned long a)
#endif
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__floatundidf stub\n");
  stub = 1;
#endif
  return 0;
}

#if HAVE_LONG_LONG && HAVE_FLOAT
long double
__attribute__((weak))
__floatundixf (unsigned long long a)
#else
double
__attribute__((weak))
__floatundixf (unsigned long a)
#endif
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__floatundixf stub\n");
  stub = 1;
#endif
  return 0;
}

#if HAVE_LONG_LONG
unsigned long long
#else
unsigned long
#endif
__attribute__((weak))
__fixunsxfdi (double a1)
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__fixunsxfdi stub\n");
  stub = 1;
#endif
  return 0;
}

#if __TINYC__ == 9226
long
#elif __TINYC__
int
#else // !__TINYCC__
long long
#endif // !__TINYCC__
__attribute__((weak))
__fixdfdi (double a1)
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__fixdfdi stub\n");
  stub = 1;
#endif
  return 0;
}

#if HAVE_LONG_LONG
unsigned long long
#else
unsigned long
#endif
__attribute__((weak))
__fixxfdi (double a1)
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__fixxfdi stub\n");
  stub = 1;
#endif
  return 0;
}

#if HAVE_LONG_LONG
long long
#else
long
#endif
__attribute__((weak))
__fixsfdi (double a1)
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__fixsfdi stub\n");
  stub = 1;
#endif
  return 0;
}

double
__attribute__((weak))
__fixunsdfdi (double num, double den)
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__fixunsdfdi stub\n");
  stub = 1;
#endif
  return 0;
}
#endif // HAVE_FLOAT_STUB || HAVE_FLOAT

int
__attribute__((weak))
__fixunsdfsi (int a, int b)
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__fixunsdfsi stub\n");
  stub = 1;
#endif
  return 0;
}

#if __arm__
int
__attribute__((weak))
__floatdisf (int a, int b)
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__floatdisf stub\n");
  stub = 1;
#endif
  return 0;
}

int
__attribute__((weak))
__floatdidf (int a, int b)
{
#if !__TINYC__
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__floatdidf stub\n");
  stub = 1;
#endif
  return 0;
}

int
__divsi3 (int num, int den)
{
  return __mesabi_idiv (num, den);
}

int
__modsi3 (int num, int den)
{
  return __mesabi_imod (num, den);
}

int
__udivsi3 (int num, int den)
{
  return __mesabi_udiv (num, den);
}

int
__umodsi3 (int num, int den)
{
  return __mesabi_umod (num, den);
}
#endif //__arm__
