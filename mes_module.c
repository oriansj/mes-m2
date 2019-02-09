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
int fdputs(char const* s, int fd);
struct scm* display_ (SCM x);
int fdputc(int c, int fd);
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
	return make_struct(record_type, fields, cell_unspecified);
}

struct scm* make_initial_module(SCM a)  ///((internal))
{
	SCM module_type = GetSCM2(bad2good(make_module_type(), g_cells), g_cells);
	a = acons(cell_symbol_module, module_type, a);
	SCM hashq_type = GetSCM2(bad2good(make_hashq_type(), g_cells), g_cells);
	a = acons(cell_symbol_hashq_table, hashq_type, a);
	struct scm* b = Getstructscm2(a, g_cells);
	SCM name = cons(GetSCM2(cstring_to_symbol("boot"), g_cells), cell_nil);
	SCM globals = GetSCM2(bad2good(make_hash_table_(0), g_cells), g_cells);
	SCM locals = cell_nil;
	SCM values = cell_nil;
	values = cons(globals, values);
	values = cons(locals, values);
	values = cons(name, values);
	values = cons(cell_symbol_module, values);
	SCM module = GetSCM2(bad2good(make_struct(module_type, values, GetSCM2(cstring_to_symbol("module-printer"), g_cells)), g_cells), g_cells);
	r0 = cell_nil;
	r0 = cons(bad2good(b->cdr, g_cells)->rac, r0);
	r0 = cons(b->rac, r0);
	m0 = module;

	while(b->type == TPAIR)
	{
		module_define_x(module, bad2good(b->car, g_cells)->rac, bad2good(b->car, g_cells)->rdc);
		b = bad2good(b->cdr, g_cells);
	}

	return good2bad(Getstructscm2(module, g_cells), g_cells);
}

struct scm* module_printer(SCM module)
{
	//module = m0;
	fdputs("#<", __stdout);
	display_(GetSCM2(bad2good(struct_ref_(module, 2), g_cells), g_cells));
	fdputc(' ', __stdout);
	fdputs("name: ", __stdout);
	display_(GetSCM2(bad2good(struct_ref_(module, 3), g_cells), g_cells));
	fdputc(' ', __stdout);
	fdputs("locals: ", __stdout);
	display_(GetSCM2(bad2good(struct_ref_(module, 4), g_cells), g_cells));
	fdputc(' ', __stdout);
	SCM table = GetSCM2(bad2good(struct_ref_(module, 5), g_cells), g_cells);
	fdputs("globals:\n  ", __stdout);
	display_(table);
	fdputc('>', __stdout);
	return good2bad(Getstructscm2(cell_unspecified, g_cells), g_cells);
}

struct scm* module_variable(SCM module, SCM name)
{
	//SCM locals = struct_ref_ (module, 3);
	SCM locals = module;
	SCM x = assq(name, locals);

	if(x == cell_f)
	{
		module = m0;
		SCM globals = GetSCM2(bad2good(struct_ref_(module, 5), g_cells), g_cells);
		x = GetSCM2(bad2good(hashq_get_handle(globals, name, cell_f), g_cells), g_cells);
	}

	return good2bad(Getstructscm2(x, g_cells), g_cells);
}

struct scm* module_ref(SCM module, SCM name)
{
	struct scm* y = bad2good(module_variable(module, name), g_cells);

	if(GetSCM2(y, g_cells) == cell_f)
	{
		return good2bad(Getstructscm2(cell_undefined, g_cells), g_cells);
	}

	return y->cdr;
}

struct scm* module_define_x(SCM module, SCM name, SCM value)
{
	module = m0;
	SCM globals = GetSCM2(bad2good(struct_ref_(module, 5), g_cells), g_cells);
	return hashq_set_x(globals, name, value);
}
