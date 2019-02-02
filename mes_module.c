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

#define CAR(x) g_cells[x].rac
#define CDR(x) g_cells[x].rdc

struct scm* struct_ref_(SCM x, long i);
struct scm* cstring_to_symbol(char const *s);
struct scm* make_hashq_type();
struct scm* module_define_x (SCM module, SCM name, SCM value);
SCM car (SCM x);
SCM cdr (SCM x);
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
	SCM module_type = GetSCM(make_module_type());
	a = acons(cell_symbol_module, module_type, a);
	SCM hashq_type = GetSCM(make_hashq_type());
	a = acons(cell_symbol_hashq_table, hashq_type, a);
	SCM name = cons(GetSCM2(cstring_to_symbol("boot"), g_cells), cell_nil);
	SCM globals = GetSCM(make_hash_table_(0));
	SCM locals = cell_nil;
	SCM values = cell_nil;
	values = cons(globals, values);
	values = cons(locals, values);
	values = cons(name, values);
	values = cons(cell_symbol_module, values);
	SCM module = GetSCM(make_struct(module_type, values, GetSCM2(cstring_to_symbol("module-printer"), g_cells)));
	r0 = cell_nil;
	r0 = cons(CAR(CDR(a)), r0);
	r0 = cons(CAR(a), r0);
	m0 = module;

	while(g_cells[a].type == TPAIR)
	{
		module_define_x(module, CAR(CAR(a)), CDR(CAR(a)));
		a = CDR(a);
	}

	return Getstructscm(module);
}

struct scm* module_printer(SCM module)
{
	//module = m0;
	fdputs("#<", __stdout);
	display_(GetSCM(struct_ref_(module, 2)));
	fdputc(' ', __stdout);
	fdputs("name: ", __stdout);
	display_(GetSCM(struct_ref_(module, 3)));
	fdputc(' ', __stdout);
	fdputs("locals: ", __stdout);
	display_(GetSCM(struct_ref_(module, 4)));
	fdputc(' ', __stdout);
	SCM table = GetSCM(struct_ref_(module, 5));
	fdputs("globals:\n  ", __stdout);
	display_(table);
	fdputc('>', __stdout);
	return Getstructscm(cell_unspecified);
}

struct scm* module_variable(SCM module, SCM name)
{
	//SCM locals = struct_ref_ (module, 3);
	SCM locals = module;
	SCM x = assq(name, locals);

	if(x == cell_f)
	{
		module = m0;
		SCM globals = GetSCM(struct_ref_(module, 5));
		x = GetSCM(hashq_get_handle(globals, name, cell_f));
	}

	return Getstructscm(x);
}

struct scm* module_ref(SCM module, SCM name)
{
	SCM x = GetSCM(module_variable(module, name));

	if(x == cell_f)
	{
		return Getstructscm(cell_undefined);
	}

	return Getstructscm(CDR(x));
}

struct scm* module_define_x(SCM module, SCM name, SCM value)
{
	module = m0;
	SCM globals = GetSCM(struct_ref_(module, 5));
	return hashq_set_x(globals, name, value);
}
