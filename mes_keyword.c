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
/* Imported functions */
struct cell* make_keyword(char* name);
struct cell* make_sym(char* name);

struct cell* builtin_keywordp(struct cell* args)
{
	require(nil != args, "keyword? requires arguments\n");
	require(nil == args->cdr, "keyword? recieved too many arguments\n");

	if(KEYWORD == args->car->type) return cell_t;
	return cell_f;
}

struct cell* builtin_keyword_to_symbol(struct cell* args)
{
	require(nil != args, "keyword->symbol requires arguments\n");
	require(nil == args->cdr, "keyword->symbol recieved too many arguments\n");
	require(KEYWORD == args->car->type, "keyword->symbol did not recieve a keyword\n");

	return make_sym(args->car->string + 2);
}

struct cell* builtin_string_to_keyword(struct cell* args)
{
	require(nil != args, "string->keyword requires arguments\n");
	require(nil == args->cdr, "string->keyword recieved too many arguments\n");
	require(STRING == args->car->type, "string->keyword did not recieve a string\n");

	return make_keyword(args->car->string);
}
