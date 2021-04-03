/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include "mes/lib.h"
#include "mes/mes.h"

struct scm *
make_module_type ()             /*:(internal)) */
{
  struct scm *fields = cell_nil;
  fields = cons (cstring_to_symbol ("globals"), fields);
  fields = cons (cstring_to_symbol ("locals"), fields);
  fields = cons (cstring_to_symbol ("name"), fields);
  fields = cons (fields, cell_nil);
  fields = cons (cell_symbol_module, fields);
  return make_struct (cell_symbol_record_type, fields, cell_unspecified);
}

struct scm *
make_initial_module (struct scm *a)     /*:((internal)) */
{
  struct scm *module_type = make_module_type ();
  a = acons (cell_symbol_module, module_type, a);

  struct scm *hashq_type = make_hashq_type ();
  a = acons (cell_symbol_hashq_table, hashq_type, a);

  struct scm *name = cons (cstring_to_symbol ("boot"), cell_nil);
  struct scm *globals = make_hash_table_ (0);
  struct scm *locals = cell_nil;

  struct scm *values = cell_nil;
  values = cons (globals, values);
  values = cons (locals, values);
  values = cons (name, values);
  values = cons (cell_symbol_module, values);
  struct scm *module = make_struct (module_type, values, cstring_to_symbol ("module-printer"));
  R0 = cell_nil;
  R0 = cons (a->cdr->car, R0);
  R0 = cons (a->car, R0);
  M0 = module;
  while (a->type == TPAIR)
    {
      module_define_x (module, a->car->car, a->car->cdr);
      a = a->cdr;
    }

  return module;
}

struct scm *
module_printer (struct scm *module)
{
  fdputs ("#<", __stdout);
  display_ (struct_ref_ (module, 2));
  fdputc (' ', __stdout);
  fdputs ("name: ", __stdout);
  display_ (struct_ref_ (module, 3));
  fdputc (' ', __stdout);
  fdputs ("locals: ", __stdout);
  display_ (struct_ref_ (module, 4));
  fdputc (' ', __stdout);
  struct scm *table = struct_ref_ (module, 5);
  fdputs ("globals:\n  ", __stdout);
  display_ (table);
  fdputc ('>', __stdout);
}

struct scm *
module_variable (struct scm *module, struct scm *name)
{
  /*struct scm *locals = struct_ref_ (module, 3);*/
  struct scm *locals = module;
  struct scm *x = assq (name, locals);
  if (x == cell_f)
    {
      module = M0;
      struct scm *globals = struct_ref_ (module, 5);
      x = hashq_get_handle (globals, name, cell_f);
    }
  return x;
}

struct scm *
module_ref (struct scm *module, struct scm *name)
{
  struct scm *x = module_variable (module, name);
  if (x == cell_f)
    return cell_undefined;
  return x->cdr;
}

struct scm *
module_define_x (struct scm *module, struct scm *name, struct scm *value)
{
  module = M0;
  struct scm *globals = struct_ref_ (module, 5);
  return hashq_set_x (globals, name, value);
}
