/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#ifndef __MES_LIMITS_H
#define __MES_LIMITS_H 1

#if WITH_GLIBC
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_LIMITS_H
#include_next <limits.h>

#else // ! WITH_GLIBC

#define CHAR_BIT 8
#define UCHAR_MAX 255
#define UINT_MAX 4294967295U
#define INT_MIN -2147483648
#define INT_MAX 2147483647
#define MB_CUR_MAX 1
#define LONG_MIN -2147483648
#define LONG_MAX 2147483647
#define _POSIX_OPEN_MAX 16

#endif // ! WITH_GLIBC

#endif // __MES_LIMITS_H
