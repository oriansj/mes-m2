/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include "mes_macros.h"

SCM struct_ref_(SCM x, long i);
SCM struct_set_x_(SCM x, long i, SCM e);
SCM cstring_to_symbol(char const *s);
SCM make_hashq_type();
SCM module_define_x (SCM module, SCM name, SCM value);
SCM car (SCM x);
SCM cdr (SCM x);
SCM cons (SCM x, SCM y);
SCM make_cell__(long type, SCM car, SCM cdr);
SCM make_struct (SCM type, SCM fields, SCM printer);
SCM acons (SCM key, SCM value, SCM alist);
SCM make_hash_table_(long size);
int fdputs(char const* s, int fd);
SCM display_ (SCM x);
int fdputc(int c, int fd);
SCM assq (SCM x, SCM a);
SCM hashq_get_handle (SCM table, SCM key, SCM dflt);
SCM hashq_set_x (SCM table, SCM key, SCM value);

SCM make_module_type()  ///(internal))
{
	SCM record_type = cell_symbol_record_type; // FIXME
	SCM fields = cell_nil;
	fields = cons(cstring_to_symbol("globals"), fields);
	fields = cons(cstring_to_symbol("locals"), fields);
	fields = cons(cstring_to_symbol("name"), fields);
	fields = cons(fields, cell_nil);
	fields = cons(cell_symbol_module, fields);
	return make_struct(record_type, fields, cell_unspecified);
}

SCM make_initial_module(SCM a)  ///((internal))
{
	SCM module_type = make_module_type();
	a = acons(cell_symbol_module, module_type, a);
	SCM hashq_type = make_hashq_type();
	a = acons(cell_symbol_hashq_table, hashq_type, a);
	SCM name = cons(cstring_to_symbol("boot"), cell_nil);
	SCM globals = make_hash_table_(0);
	SCM locals = cell_nil;
	SCM values = cell_nil;
	values = cons(globals, values);
	values = cons(locals, values);
	values = cons(name, values);
	values = cons(cell_symbol_module, values);
	SCM module = make_struct(module_type, values, cstring_to_symbol("module-printer"));
	r0 = cell_nil;
	r0 = cons(CADR(a), r0);
	r0 = cons(CAR(a), r0);
	m0 = module;

	while(TYPE(a) == TPAIR)
	{
		module_define_x(module, CAAR(a), CDAR(a));
		a = CDR(a);
	}

	return module;
}

SCM module_printer(SCM module)
{
	//module = m0;
	fdputs("#<", __stdout);
	display_(struct_ref_(module, 2));
	fdputc(' ', __stdout);
	fdputs("name: ", __stdout);
	display_(struct_ref_(module, 3));
	fdputc(' ', __stdout);
	fdputs("locals: ", __stdout);
	display_(struct_ref_(module, 4));
	fdputc(' ', __stdout);
	SCM table = struct_ref_(module, 5);
	fdputs("globals:\n  ", __stdout);
	display_(table);
	fdputc('>', __stdout);
}

SCM module_variable(SCM module, SCM name)
{
	//SCM locals = struct_ref_ (module, 3);
	SCM locals = module;
	SCM x = assq(name, locals);

	if(x == cell_f)
	{
		module = m0;
		SCM globals = struct_ref_(module, 5);
		x = hashq_get_handle(globals, name, cell_f);
	}

	return x;
}

SCM module_ref(SCM module, SCM name)
{
	SCM x = module_variable(module, name);

	if(x == cell_f)
	{
		return cell_undefined;
	}

	return CDR(x);
}

SCM module_define_x(SCM module, SCM name, SCM value)
{
	module = m0;
	SCM globals = struct_ref_(module, 5);
	return hashq_set_x(globals, name, value);
}
