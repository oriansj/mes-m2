/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

/* Cell types */

#define TCHAR              0

#define TBYTES             1
#define TCLOSURE           2
#define TCONTINUATION      3
#define TKEYWORD           4
#define TMACRO             5
#define TNUMBER            6
#define TPAIR              7
#define TPORT              8
#define TREF               9
#define TSPECIAL          10
#define TSTRING           11
#define TSTRUCT           12
#define TSYMBOL           13
#define TVALUES           14
#define TVARIABLE         15
#define TVECTOR           16
#define TBROKEN_HEART     17

/* Struct types */

#define STRUCT_TYPE 0
#define STRUCT_PRINTER 1

#define FRAME_SIZE 5
#define FRAME_PROCEDURE 4

#define STDIN 0
#define STDOUT 1
#define STDERR 2

/* Unknown type 1
*/
#define EOF -1

#define O_RDONLY 0
#define O_WRONLY 1
#define O_CREAT 0x40
#define O_TRUNC 0x200

#define PATH_MAX 1024
#define __FILEDES_MAX 512

#define S_IRUSR 00400
#define S_IWUSR 00200

#define CLOCK_PROCESS_CPUTIME_ID 2
