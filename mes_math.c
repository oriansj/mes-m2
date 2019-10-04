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
SCM error(SCM key, SCM x);
SCM cons (SCM x, SCM y);
SCM make_cell__(SCM type, SCM car, SCM cdr);

void assert_number(char const* name, SCM x)
{
	struct scm* y = Getstructscm2(x);
	if(y->type != TNUMBER)
	{
		eputs(name);
		error(cell_symbol_not_a_number, x);
	}
}

struct scm* greater_p(SCM x)  ///((name . ">") (arity . n))
{
	struct scm* y = Getstructscm2(x);
	if(GetSCM2(y) == cell_nil)
	{
		return good2bad(Getstructscm2(cell_t));
	}

	assert_number("greater_p", y->rac);
	long n = bad2good(y->car)->value;
	y = bad2good(y->cdr);

	while(GetSCM2(y) != cell_nil)
	{
		assert_number("greater_p", y->rac);

		if(bad2good(y->car)->value >= n)
		{
			return good2bad(Getstructscm2(cell_f));
		}

		n = bad2good(y->car)->value;
		y = bad2good(y->cdr);
	}

	return good2bad(Getstructscm2(cell_t));
}

struct scm* less_p(SCM x)  ///((name . "<") (arity . n))
{
	struct scm* y = Getstructscm2(x);
	if(GetSCM2(y) == cell_nil)
	{
		return good2bad(Getstructscm2(cell_t));
	}

	assert_number("less_p", y->rac);
	long n = bad2good(y->car)->value;
	y = bad2good(y->cdr);

	while(GetSCM2(y) != cell_nil)
	{
		assert_number("less_p", y->rac);

		if(bad2good(y->car)->value <= n)
		{
			return good2bad(Getstructscm2(cell_f));
		}

		n = bad2good(y->car)->value;
		y = bad2good(y->cdr);
	}

	return good2bad(Getstructscm2(cell_t));
}

struct scm* is_p(SCM x)  ///((name . "=") (arity . n))
{
	struct scm* y = Getstructscm2(x);
	if(GetSCM2(y) == cell_nil)
	{
		return good2bad(Getstructscm2(cell_t));
	}

	assert_number("is_p", y->rac);
	long n = bad2good(y->car)->value;
	y = bad2good(y->cdr);

	while(GetSCM2(y) != cell_nil)
	{
		if(bad2good(y->car)->value != n)
		{
			return good2bad(Getstructscm2(cell_f));
		}

		y = bad2good(y->cdr);
	}

	return good2bad(Getstructscm2(cell_t));
}

struct scm* minus(SCM x)  ///((name . "-") (arity . n))
{
	struct scm* y = Getstructscm2(x);
	assert_number("minus", y->rac);
	long n = bad2good(y->car)->value;
	y = bad2good(y->cdr);

	if(GetSCM2(y) == cell_nil)
	{
		n = -n;
	}

	while(GetSCM2(y) != cell_nil)
	{
		assert_number("minus", y->rac);
		n = n - bad2good(y->car)->value;
		y = bad2good(y->cdr);
	}

	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, n)));
}

struct scm* plus(SCM x)  ///((name . "+") (arity . n))
{
	struct scm* y = Getstructscm2(x);
	long n = 0;

	while(GetSCM2(y) != cell_nil)
	{
		assert_number("plus", y->rac);
		n = n + bad2good(y->car)->value;
		y = bad2good(y->cdr);
	}

	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, n)));
}

struct scm* divide(SCM x)  ///((name . "/") (arity . n))
{
	struct scm* y = Getstructscm2(x);
	long n = 1;

	if(GetSCM2(y) != cell_nil)
	{
		assert_number("divide", y->rac);
		//n = bad2good(y->car)->value;
		n = bad2good(y->car)->rdc;
		y = bad2good(y->cdr);
	}

	while(GetSCM2(y) != cell_nil)
	{
		assert_number("divide", y->rac);

		if(!n)
		{
			break;
		}

		n = n / bad2good(y->car)->rdc;
		// n = n / bad2good(y->car)->value;
		y = bad2good(y->cdr);
	}

	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, n)));
}

struct scm* modulo(SCM a, SCM b)
{
	struct scm* a2 = Getstructscm2(a);
	struct scm* b2 = Getstructscm2(b);
	assert_number("modulo", a);
	assert_number("modulo", b);
	long x = a2->rdc;
	// long x = a2->value;

	x = x % b2->value;
	if(b2->value < 0)
	{
		x = x + b2->value;
	}
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, x)));
}

struct scm* multiply(SCM x)  ///((name . "*") (arity . n))
{
	struct scm* y = Getstructscm2(x);
	long n = 1;

	while(GetSCM2(y) != cell_nil)
	{
		assert_number("multiply", y->rac);
		n = n * bad2good(y->car)->rdc;
		//n = n * bad2good(y->car)->value;
		y = bad2good(y->cdr);
	}

	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, n)));
}

struct scm* logand(SCM x)  ///((arity . n))
{
	struct scm* y = Getstructscm2(x);
	long n = -1;

	while(GetSCM2(y) != cell_nil)
	{
		assert_number("multiply", y->rac);
		n = n & bad2good(y->car)->value;
		y = bad2good(y->cdr);
	}

	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, n)));
}

struct scm* logior(SCM x)  ///((arity . n))
{
	struct scm* y = Getstructscm2(x);
	long n = 0;

	while(GetSCM2(y) != cell_nil)
	{
		assert_number("logior", y->rac);
		n = n | bad2good(y->car)->value;
		y = bad2good(y->cdr);
	}

	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, n)));
}

struct scm* lognot(SCM x)
{
	struct scm* y = Getstructscm2(x);
	assert_number("lognot", x);
	long n = ~y->value;
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, n)));
}

struct scm* logxor(SCM x)  ///((arity . n))
{
	struct scm* y = Getstructscm2(x);
	long n = 0;

	while(GetSCM2(y) != cell_nil)
	{
		assert_number("logxor", y->rac);
		n = n ^ bad2good(y->car)->value;
		y = bad2good(y->cdr);
	}

	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, n)));
}

struct scm* ash(SCM n, SCM count)
{
	struct scm* n2 = Getstructscm2(n);
	struct scm* count2 = Getstructscm2(count);
	assert_number("ash", n);
	assert_number("ash", count);
	long cn = n2->value;
	long ccount = count2->value;
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, (ccount < 0) ? cn >> -ccount : cn << ccount)));
}
