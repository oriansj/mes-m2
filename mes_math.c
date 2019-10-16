/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2019 Jeremiah Orians
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

#include "mes.h"
#include "mes_constants.h"
#include <limits.h>

int eputs(char const* s);
struct scm* error(struct scm* key, struct scm* x);
struct scm* cons (struct scm* x, struct scm* y);

struct scm* make_number(SCM n);

void assert_number(char const* name, struct scm* x)
{
	struct scm* y = x;
	if(y->type != TNUMBER)
	{
		eputs(name);
		error(cell_symbol_not_a_number, x);
	}
}

struct scm* greater_p(struct scm* x)  ///((name . ">") (arity . n))
{
	struct scm* y = x;
	if(y == cell_nil)
	{
		return cell_t;
	}

	assert_number("greater_p", y->car);
	long n = y->car->value;
	y = y->cdr;

	while(y != cell_nil)
	{
		assert_number("greater_p", y->car);

		if(y->car->value >= n)
		{
			return cell_f;
		}

		n = y->car->value;
		y = y->cdr;
	}

	return cell_t;
}

struct scm* less_p(struct scm* x)  ///((name . "<") (arity . n))
{
	struct scm* y = x;
	if(y == cell_nil)
	{
		return cell_t;
	}

	assert_number("less_p", y->car);
	long n = y->car->value;
	y = y->cdr;

	while(y != cell_nil)
	{
		assert_number("less_p", y->car);

		if(y->car->value <= n)
		{
			return cell_f;
		}

		n = y->car->value;
		y = y->cdr;
	}

	return cell_t;
}

struct scm* is_p(struct scm* x)  ///((name . "=") (arity . n))
{
	struct scm* y = x;
	if(y == cell_nil)
	{
		return cell_t;
	}

	assert_number("is_p", y->car);
	long n = y->car->value;
	y = y->cdr;

	while(y != cell_nil)
	{
		if(y->car->value != n)
		{
			return cell_f;
		}

		y = y->cdr;
	}

	return cell_t;
}

struct scm* minus(struct scm* x)  ///((name . "-") (arity . n))
{
	struct scm* y = x;
	assert_number("minus", y->car);
	long n = y->car->value;
	y = y->cdr;

	if(y == cell_nil)
	{
		n = -n;
	}

	while(y != cell_nil)
	{
		assert_number("minus", y->car);
		n = n - y->car->value;
		y = y->cdr;
	}

	return make_number(n);
}

struct scm* plus(struct scm* x)  ///((name . "+") (arity . n))
{
	struct scm* y = x;
	long n = 0;

	while(y != cell_nil)
	{
		assert_number("plus", y->car);
		n = n + y->car->value;
		y = y->cdr;
	}

	return make_number(n);
}

struct scm* divide(struct scm* x)  ///((name . "/") (arity . n))
{
	struct scm* y = x;
	long n = 1;

	if(y != cell_nil)
	{
		assert_number("divide", y->car);
		n = y->car->value;
		y = y->cdr;
	}

	while(y != cell_nil)
	{
		assert_number("divide", y->car);

		if(!n)
		{
			break;
		}

		n = n / y->car->value;
		y = y->cdr;
	}

	return make_number(n);
}

struct scm* modulo(struct scm* a, struct scm* b)
{
	struct scm* a2 = a;
	struct scm* b2 = b;
	assert_number("modulo", a);
	assert_number("modulo", b);
	long x = a2->value;

	x = x % b2->value;
	if(b2->value < 0)
	{
		x = x + b2->value;
	}
	return make_number(x);
}

struct scm* multiply(struct scm* x)  ///((name . "*") (arity . n))
{
	struct scm* y = x;
	long n = 1;

	while(y != cell_nil)
	{
		assert_number("multiply", y->car);
		n = n * y->car->value;
		y = y->cdr;
	}

	return make_number(n);
}

struct scm* logand(struct scm* x)  ///((arity . n))
{
	struct scm* y = x;
	long n = -1;

	while(y != cell_nil)
	{
		assert_number("multiply", y->car);
		n = n & y->car->value;
		y = y->cdr;
	}

	return make_number(n);
}

struct scm* logior(struct scm* x)  ///((arity . n))
{
	struct scm* y = x;
	long n = 0;

	while(y != cell_nil)
	{
		assert_number("logior", y->car);
		n = n | y->car->value;
		y = y->cdr;
	}

	return make_number(n);
}

struct scm* lognot(struct scm* x)
{
	struct scm* y = x;
	assert_number("lognot", x);
	long n = ~y->value;
	return make_number(n);
}

struct scm* logxor(struct scm* x)  ///((arity . n))
{
	struct scm* y = x;
	long n = 0;

	while(y != cell_nil)
	{
		assert_number("logxor", y->car);
		n = n ^ y->car->value;
		y = y->cdr;
	}

	return make_number(n);
}

struct scm* ash(struct scm* n, struct scm* count)
{
	struct scm* n2 = n;
	struct scm* count2 = count;
	assert_number("ash", n);
	assert_number("ash", count);
	long cn = n2->value;
	long ccount = count2->value;

	long r;
	if(ccount < 0) r = cn >> -ccount;
	else r= cn << ccount;

	return make_number(r);
}
