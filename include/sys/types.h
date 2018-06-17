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
#ifndef __MES_SYS_TYPES_H
#define __MES_SYS_TYPES_H 1

#if WITH_GLIBC
#undef __MES_SYS_TYPES_H
#include_next <sys/types.h>
#else // ! WITH_GLIBC
#include <endian.h>

#ifndef __MES_DEV_T
#define __MES_DEV_T
#undef dev_t
typedef int dev_t;
#endif

#ifndef __MES_GID_T
#define __MES_GID_T
#undef gid_t
typedef int gid_t;
#endif

#ifndef __MES_INO_T
#define __MES_INO_T
#undef ino_t
typedef unsigned ino_t;
#endif

#ifndef __MES_PID_T
#define __MES_PID_T
#undef pid_t
typedef int pid_t;
#endif

#ifndef __MES_SIZE_T
#define __MES_SIZE_T
#undef size_t
typedef unsigned long size_t;
#endif

#ifndef __MES_UID_T
#define __MES_UID_T
#undef uid_t
typedef int uid_t;
#endif

#endif // ! WITH_GLIBC

#endif // __MES_SYS_TYPES_H
