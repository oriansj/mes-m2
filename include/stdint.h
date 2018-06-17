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
#ifndef __MES_STDINT_H
#define __MES_STDINT_H 1

#if WITH_GLIBC
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_STDINT_H
#include_next <stdint.h>

#else // ! WITH_GLIBC

#undef unsigned
#undef uint8_t
#undef int8_t

#undef uint16_t
#undef int16_t

#undef uint32_t
#undef int32_t

#undef uint64_t
#undef int64_t

#undef uintptr_t
#undef intmax_t
#undef intptr_t
#undef uintmax_t
#undef ptrdiff_t

typedef unsigned char uint8_t;
typedef char int8_t;
typedef unsigned short uint16_t;
typedef short int16_t;
typedef unsigned uint32_t;
typedef int int32_t;
typedef unsigned long long uint64_t;
typedef long long int64_t;

#ifndef __MES_SIZE_T
#define __MES_SIZE_T
#undef size_t
typedef unsigned long size_t;
#endif

#ifndef __MES_INTPTR_T
#define __MES_INTPTR_T
#undef intptr_t
typedef long intptr_t;
#endif

// FIXME
typedef int intmax_t;
typedef unsigned uintmax_t;
typedef unsigned* uintptr_t;

#ifndef __MES_PTRDIFF_T
#define __MES_PTRDIFF_T
#undef ptrdiff_t
typedef long ptrdiff_t;
#endif

#endif // ! WITH_GLIBC

#endif // __MES_STDINT_H
