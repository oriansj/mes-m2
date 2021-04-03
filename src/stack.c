/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <stdlib.h>

struct scm *
frame_printer (struct scm *frame)
{
  fdputs ("#<", __stdout);
  display_ (struct_ref_ (frame, 2));
  fdputc (' ', __stdout);
  fdputs ("procedure: ", __stdout);
  display_ (struct_ref_ (frame, 3));
  fdputc ('>', __stdout);
}

struct scm *
make_frame_type ()              /*:((internal)) */
{
  struct scm *fields = cell_nil;
  fields = cons (cell_symbol_procedure, fields);
  fields = cons (fields, cell_nil);
  fields = cons (cell_symbol_frame, fields);
  return make_struct (cell_symbol_record_type, fields, cell_unspecified);
}

struct scm *
make_frame (struct scm *stack, long index)
{
  struct scm *frame_type = make_frame_type ();
  long array_index = (STACK_SIZE - (index * FRAME_SIZE));
  struct scm *procedure = g_stack_array[array_index + FRAME_PROCEDURE];
  if (procedure == 0)
    procedure = cell_f;
  struct scm *values = cell_nil;
  values = cons (procedure, values);
  values = cons (cell_symbol_frame, values);
  return make_struct (frame_type, values, cstring_to_symbol ("frame-printer"));
}

struct scm *
make_stack_type ()              /*:((internal)) */
{
  struct scm *fields = cell_nil;
  fields = cons (cstring_to_symbol ("frames"), fields);
  fields = cons (fields, cell_nil);
  fields = cons (cell_symbol_stack, fields);
  return make_struct (cell_symbol_record_type, fields, cell_unspecified);
}

struct scm *
make_stack (struct scm *stack)          /*:((arity . n)) */
{
  struct scm *stack_type = make_stack_type ();
  long size = (STACK_SIZE - g_stack) / FRAME_SIZE;
  struct scm *frames = make_vector_ (size, cell_unspecified);
  long i;
  struct scm* frame;
  for (i = 0; i < size; i = i + 1)
    {
      frame = make_frame (stack, i);
      vector_set_x_ (frames, i, frame);
    }
  struct scm *values = cell_nil;
  values = cons (frames, values);
  values = cons (cell_symbol_stack, values);
  return make_struct (stack_type, values, cell_unspecified);
}

struct scm *
stack_length (struct scm *stack)
{
  struct scm *frames = struct_ref_ (stack, 3);
  return vector_length (frames);
}

struct scm *
stack_ref (struct scm *stack, struct scm *index)
{
  struct scm *frames = struct_ref_ (stack, 3);
  return vector_ref (frames, index);
}
