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

struct scm* struct_ref_(SCM x, long i);
struct scm* cstring_to_symbol(char const *s);
struct scm* make_hashq_type();
SCM cons_(SCM x, SCM y);
SCM make_cell__(SCM type, SCM car, SCM cdr);
struct scm* make_struct(SCM type, SCM fields, SCM printer);
SCM acons_(SCM key, SCM value, SCM alist);
struct scm* make_hash_table_(long size);
SCM assq(SCM x, SCM a);
struct scm* hashq_get_handle(SCM table, SCM key, SCM dflt);
struct scm* hashq_set_x(SCM table, SCM key, SCM value);

struct scm* make_module_type()
{
	SCM record_type = cell_symbol_record_type; // FIXME
	SCM fields = cell_nil;
	fields = cons_(GetSCM2(cstring_to_symbol("globals")), fields);
	fields = cons_(GetSCM2(cstring_to_symbol("locals")), fields);
	fields = cons_(GetSCM2(cstring_to_symbol("name")), fields);
	fields = cons_(fields, cell_nil);
	fields = cons_(cell_symbol_module, fields);
	return make_struct(record_type, fields, cell_unspecified);
}

struct scm* module_variable(SCM module, SCM name)
{
	//SCM locals = struct_ref_ (module, 3);
	SCM locals = module;
	SCM x = assq(name, locals);

	if(x == cell_f)
	{
		SCM globals = GetSCM2(struct_ref_(GetSCM2(bad2good(M0)), 5));
		x = GetSCM2(bad2good(hashq_get_handle(globals, name, cell_f)));
	}

	return Getstructscm2(x);
}


struct scm* module_ref(SCM module, SCM name)
{
	struct scm* y = module_variable(module, name);

	if(GetSCM2(y) == cell_f)
	{
		return Getstructscm2(cell_undefined);
	}

	return bad2good(y->cdr);
}

struct scm* module_define_x(SCM module, SCM name, SCM value)
{
	SCM globals = GetSCM2(struct_ref_(GetSCM2(bad2good(M0)), 5));
	return good2bad(hashq_set_x(globals, name, value));
}

/* External functions */
struct scm* module_variable_(SCM module, SCM name) /* EXTERNAL */
{
	return good2bad(module_variable(module, name));
}

struct scm* module_ref_(SCM module, SCM name) /* EXTERNAL */
{
	return good2bad(module_ref(module, name));
}

struct scm* make_module_type_() /* EXTERNAL */
{
	return good2bad(make_module_type());
}
