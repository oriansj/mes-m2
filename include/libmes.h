/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#ifndef __MES_LIBMES_H
#define __MES_LIBMES_H

char const* number_to_ascii (int number, int base, int signed_p);
char const* itoa (int number);
char const* utoa (unsigned number);
char const* itoab (int x, int base);
int _atoi (char const**, int base);
int atoi (char const *s);
int eputc (int c);
int eputs (char const* s);
int fdgetc (int fd);
int fdputc (int c, int fd);
int fdputs (char const* s, int fd);
int fdungetc (int c, int fd);
int isdigit (int c);
int isspace (int c);
int isxdigit (int c);
int oputs (char const* s);

#endif //__MES_LIBMES_H
