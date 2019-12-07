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

struct cell* extend_env(struct cell* sym, struct cell* val, struct cell* env);
struct cell* assoc(struct cell* key, struct cell* alist);

struct cell* define_macro(struct cell* exp, struct cell* env)
{
	if(CONS == exp->cdr->car->type)
	{
		struct cell* fun = exp->cdr->cdr;
		struct cell* arguments = exp->cdr->car->cdr;
		struct cell* name = exp->cdr->car->car;
		exp->cdr = make_cons(name, make_cons(make_cons(s_macro, make_cons(arguments, fun)), nil));
	}

	return(extend_env(exp->cdr->car, exp->cdr->cdr->car, env));
}


struct cell* macro_apply(struct cell* exps, struct cell* env)
{
	// DUMMY macro_apply
	return exps;
}

struct cell* expand_macros()
{
	struct cell* r = R0;
	struct cell* exp;
expand_reset:
	if(CONS != R0->type) return r;
	else if(R0->car == s_define_macro)
	{
		define_macro(R0, g_env);
		return NULL;
	}

	exp = assoc(R0->car, g_env);
	if(CONS == exp->type)
	{
		if(s_macro == exp->cdr->car)
		{
			exit(EXIT_SUCCESS);
		}
	}

	R0 = R0->cdr;
	goto expand_reset;
}
