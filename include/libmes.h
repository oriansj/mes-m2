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

#ifndef __MES_LIBMES_H
#define __MES_LIBMES_H

#include <libmes-mini.h>

#if WITH_GLIBC
int mes_open (char const *file_name, int flags, ...);
#define open mes_open
#endif

int __mes_debug ();
double abtod (char const** p, int base);
long abtol (char const** p, int base);
char *dtoab (double number, int base, int signed_p);
char *itoa (int number);
char *ltoa (long number);
char *ltoab (long x, int base);
char *ntoab (long number, int base, int signed_p);
char *ultoa (unsigned long number);
char *utoa (unsigned number);
int atoi (char const *s);
int eputc (int c);
int fdgetc (int fd);
int fdputc (int c, int fd);
int fdputs (char const* s, int fd);
int fdungetc (int c, int fd);
int _fdungetc_p (int fd);
int isdigit (int c);
int isspace (int c);
int isxdigit (int c);
int _open3 (char const *file_name, int flags, int mask);
int _open2 (char const *file_name, int flags);
int oputc (int c);
int oputs (char const* s);
char *search_path (char const *file_name);

#endif //__MES_LIBMES_H
