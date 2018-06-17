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
#ifndef __MES_STDLIB_H
#define __MES_STDLIB_H 1

#if WITH_GLIBC
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_STDLIB_H
#include_next <stdlib.h>
#else  // ! WITH_GLIBC

#ifndef __MES_SIZE_T
#define __MES_SIZE_T
#undef size_t
typedef unsigned long size_t;
#endif

#if _ALLOCA_UNSIGNED
void * alloca (unsigned size);
#else
void * alloca (size_t size);
#endif
int atoi (char const *s);
int atexit (void (*function) (void));
void * calloc (size_t nmemb, size_t size);
void _exit (int status);
void exit (int status);
void free (void *ptr);
char* getenv (char const* s);
int setenv (char const* s, char const* v, int overwrite_p);
void unsetenv (char const *name);
void *malloc (size_t);
void qsort (void *base, size_t nmemb, size_t size, int (*compar)(void const *, void const *));
void *realloc (void *p, size_t size);
double strtod (char const *string, char **tailptr);
float strtof (char const *string, char **tailptr);
long double strtold (char const *string, char **tailptr);
long strtol (char const *string, char **tailptr, int base);
long long strtoll (char const *string, char **tailptr, int base);
unsigned long strtoul (char const *string, char **tailptr, int base);
unsigned long long strtoull (char const *string, char **tailptr, int base);

#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0

#ifndef NULL
#define NULL 0
#endif

#if __MESC__
typedef int (*comparison_fn_t) (void const *, void const *);
#else
typedef void (*comparison_fn_t) ();
#endif

void * bsearch (void const *key, void const *array, size_t count, size_t size, comparison_fn_t compare);

#include <endian.h>

#endif // ! WITH_GLIBC

#endif // __MES_STDLIB_H
