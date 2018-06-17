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
#ifndef __MES_SYS_MMAN_H
#define __MES_SYS_MMAN_H 1

#if WITH_GLIBC
#undef __MES_SYS_MMAN_H
#include_next <sys/mman.h>
#else // ! WITH_GLIBC

#ifndef __MES_SIZE_T
#define __MES_SIZE_T
typedef unsigned long size_t;
#endif

#define PROT_NONE 0
#define PROT_READ 1
#define PROT_WRITE 2
#define PROT_EXEC 4

int mprotect (void *addr, size_t len, int prot);

#endif // ! WITH_GLIBC

#endif // __MES_SYS_MMAN_H

