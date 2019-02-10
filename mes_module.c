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
struct scm* module_define_x (SCM module, SCM name, SCM value);
SCM cons (SCM x, SCM y);
SCM make_cell__(long type, SCM car, SCM cdr);
struct scm* make_struct (SCM type, SCM fields, SCM printer);
SCM acons (SCM key, SCM value, SCM alist);
struct scm* make_hash_table_(long size);
SCM assq (SCM x, SCM a);
struct scm* hashq_get_handle (SCM table, SCM key, SCM dflt);
struct scm* hashq_set_x (SCM table, SCM key, SCM value);

struct scm* make_module_type()  ///(internal))
{
	SCM record_type = cell_symbol_record_type; // FIXME
	SCM fields = cell_nil;
	fields = cons(GetSCM2(cstring_to_symbol("globals"), g_cells), fields);
	fields = cons(GetSCM2(cstring_to_symbol("locals"), g_cells), fields);
	fields = cons(GetSCM2(cstring_to_symbol("name"), g_cells), fields);
	fields = cons(fields, cell_nil);
	fields = cons(cell_symbol_module, fields);
	return good2bad(make_struct(record_type, fields, cell_unspecified), g_cells);
}

struct scm* module_variable(SCM module, SCM name)
{
	//SCM locals = struct_ref_ (module, 3);
	SCM locals = module;
	SCM x = assq(name, locals);

	if(x == cell_f)
	{
		module = m0;
		SCM globals = GetSCM2(struct_ref_(module, 5), g_cells);
		x = GetSCM2(bad2good(hashq_get_handle(globals, name, cell_f), g_cells), g_cells);
	}

	return Getstructscm2(x, g_cells);
}

struct scm* module_variable_(SCM module, SCM name)
{
	return good2bad(module_variable(module, name), g_cells);
}


struct scm* module_ref(SCM module, SCM name)
{
	struct scm* y = module_variable(module, name);

	if(GetSCM2(y, g_cells) == cell_f)
	{
		return good2bad(Getstructscm2(cell_undefined, g_cells), g_cells);
	}

	return y->cdr;
}

struct scm* module_define_x(SCM module, SCM name, SCM value)
{
	module = m0;
	SCM globals = GetSCM2(struct_ref_(module, 5), g_cells);
	return hashq_set_x(globals, name, value);
}
