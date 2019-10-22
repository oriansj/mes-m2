/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

struct scm* struct_ref_(struct scm* x, SCM i);
struct scm* cstring_to_symbol(char* s);
struct scm* make_hashq_type();
struct scm* make_tpair(struct scm* a, struct scm* b);
struct scm* make_struct_(struct scm* type, struct scm* fields, struct scm* printer);
struct scm* make_hash_table_(SCM size);
struct scm* assq_(struct scm* x, struct scm* a);
struct scm* hashq_get_handle_(struct scm* table, struct scm* key, struct scm* dflt);
struct scm* hashq_set_x_(struct scm* table, struct scm* key, struct scm* value);
void require(int bool, char* error);

struct scm* make_module_type_()
{
	struct scm* record_type = cell_symbol_record_type; /* FIXME */
	struct scm* fields = cell_nil;
	fields = make_tpair(cstring_to_symbol("globals"), fields);
	fields = make_tpair(cstring_to_symbol("locals"), fields);
	fields = make_tpair(cstring_to_symbol("name"), fields);
	fields = make_tpair(fields, cell_nil);
	fields = make_tpair(cell_symbol_module, fields);
	return make_struct_(record_type, fields, cell_unspecified);
}

struct scm* module_variable_(struct scm* module, struct scm* name)
{
	/* struct scm* locals = struct_ref_(module, 3); */
	struct scm* locals = module;
	struct scm* x = assq_(name, locals);

	if(x == cell_f)
	{
		struct scm* globals = struct_ref_(M0, 5);
		x = hashq_get_handle_(globals, name, cell_f);
	}

	return x;
}


struct scm* module_ref_(struct scm* module, struct scm* name) /* Internal */
{
	struct scm* y = module_variable_(module, name);

	if(y == cell_f)
	{
		return cell_undefined;
	}

	return y->cdr;
}

struct scm* module_define_x_(struct scm* module, struct scm* name, struct scm* value) /* Internal */
{
	require(cell_nil != module, "mes_module.c: module_define_x false test to quiet checker\n");
	struct scm* globals = struct_ref_(M0, 5);
	return hashq_set_x_(globals, name, value);
}

/* External functions */
struct scm* module_variable(struct scm* x) /* EXTERNAL */
{
	return module_variable_(x->car, x->cdr->car);
}

struct scm* module_define_x(struct scm* x)
{
	return module_define_x_(x->car, x->cdr->car, x->cdr->cdr->car);
}

struct scm* module_ref(struct scm* x) /* EXTERNAL */
{
	return module_ref_(x->car, x->cdr->car);
}

struct scm* make_module_type(struct scm* x) /* EXTERNAL */
{
	require(cell_nil == x, "mes_module.c: make_module_type recieved arguments\n");
	return make_module_type_();
}
