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

#ifndef __MES_CONSTANTS_H
#define __MES_CONSTANTS_H

/* Cell types */

// CONSTANT TCHAR          0
#define TCHAR              0

// CONSTANT TBYTES         1
#define TBYTES             1
// CONSTANT TCLOSURE       2
#define TCLOSURE           2
// CONSTANT TCONTINUATION  3
#define TCONTINUATION      3
// CONSTANT TKEYWORD       4
#define TKEYWORD           4
// CONSTANT TMACRO         5
#define TMACRO             5
// CONSTANT TNUMBER        6
#define TNUMBER            6
// CONSTANT TPAIR          7
#define TPAIR              7
// CONSTANT TPORT          8
#define TPORT              8
// CONSTANT TREF           9
#define TREF               9
// CONSTANT TSPECIAL      10
#define TSPECIAL          10
// CONSTANT TSTRING       11
#define TSTRING           11
// CONSTANT TSTRUCT       12
#define TSTRUCT           12
// CONSTANT TSYMBOL       13
#define TSYMBOL           13
// CONSTANT TVALUES       14
#define TVALUES           14
// CONSTANT TVARIABLE     15
#define TVARIABLE         15
// CONSTANT TVECTOR       16
#define TVECTOR           16
// CONSTANT TBROKEN_HEART 17
#define TBROKEN_HEART     17

/* Struct types */

// CONSTANT STRUCT_TYPE 0
#define STRUCT_TYPE 0
// CONSTANT STRUCT_PRINTER 1
#define STRUCT_PRINTER 1

// CONSTANT FRAME_SIZE 5
#define FRAME_SIZE 5
// CONSTANT FRAME_PROCEDURE 4
#define FRAME_PROCEDURE 4

// CONSTANT STDIN 0
// CONSTANT STDOUT 1
// CONSTANT STDERR 2

/* Unknown type 1
// CONSTANT EOF -1
*/

// CONSTANT O_RDONLY 0
// CONSTANT O_WRONLY 1
// CONSTANT O_CREAT 0x40
// CONSTANT O_TRUNC 0x200

// CONSTANT PATH_MAX 1024
// CONSTANT __FILEDES_MAX 512

// CONSTANT S_IRUSR 00400
// CONSTANT S_IWUSR 00200

// CONSTANT CLOCK_PROCESS_CPUTIME_ID 2


#endif /* __MES_CONSTANTS_H */
