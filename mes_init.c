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
struct cell* builtin_and(struct cell* args);
struct cell* builtin_append(struct cell* args);
struct cell* builtin_apply(struct cell* args);
struct cell* builtin_ash(struct cell* args);
struct cell* builtin_car(struct cell* args);
struct cell* builtin_cdr(struct cell* args);
struct cell* builtin_char_to_number(struct cell* args);
struct cell* builtin_chareq(struct cell* args);
struct cell* builtin_charp(struct cell* args);
struct cell* builtin_close(struct cell* args);
struct cell* builtin_command_line(struct cell* args);
struct cell* builtin_cons(struct cell* args);
struct cell* builtin_current_error_port(struct cell* args);
struct cell* builtin_current_input_port(struct cell* args);
struct cell* builtin_current_output_port(struct cell* args);
struct cell* builtin_definedp(struct cell* args);
struct cell* builtin_display(struct cell* args);
struct cell* builtin_display_error(struct cell* args);
struct cell* builtin_div(struct cell* args);
struct cell* builtin_eofp (struct cell* args);
struct cell* builtin_eq(struct cell* args);
struct cell* builtin_equal(struct cell* args);
struct cell* builtin_freecell(struct cell* args);
struct cell* builtin_get_env(struct cell* args);
struct cell* builtin_halt(struct cell* args);
struct cell* builtin_intp(struct cell* args);
struct cell* builtin_keyword_to_symbol(struct cell* args);
struct cell* builtin_keywordp(struct cell* args);
struct cell* builtin_list(struct cell* args);
struct cell* builtin_list_length(struct cell* args);
struct cell* builtin_list_to_string(struct cell* args);
struct cell* builtin_list_to_symbol(struct cell* args);
struct cell* builtin_list_to_vector(struct cell* args);
struct cell* builtin_listeq(struct cell* args);
struct cell* builtin_listp(struct cell* args);
struct cell* builtin_logand(struct cell* args);
struct cell* builtin_lognot(struct cell* args);
struct cell* builtin_logor(struct cell* args);
struct cell* builtin_make_record(struct cell* args);
struct cell* builtin_make_record_type(struct cell* args);
struct cell* builtin_make_vector(struct cell* args);
struct cell* builtin_mod(struct cell* args);
struct cell* builtin_not(struct cell* args);
struct cell* builtin_number_to_char(struct cell* args);
struct cell* builtin_number_to_string(struct cell* args);
struct cell* builtin_numeq(struct cell* args);
struct cell* builtin_numge(struct cell* args);
struct cell* builtin_numgt(struct cell* args);
struct cell* builtin_numle(struct cell* args);
struct cell* builtin_numlt(struct cell* args);
struct cell* builtin_open_read(struct cell* args);
struct cell* builtin_open_write(struct cell* args);
struct cell* builtin_or(struct cell* args);
struct cell* builtin_port_filename(struct cell* args);
struct cell* builtin_primitive_eval(struct cell* args);
struct cell* builtin_primitive_load(struct cell* args);
struct cell* builtin_primitivep(struct cell* args);
struct cell* builtin_procedurep(struct cell* args);
struct cell* builtin_prod(struct cell* args);
struct cell* builtin_read_byte(struct cell* args);
struct cell* builtin_record_accessor(struct cell* args);
struct cell* builtin_record_constructor(struct cell* args);
struct cell* builtin_record_modifier(struct cell* args);
struct cell* builtin_record_predicate(struct cell* args);
struct cell* builtin_record_type_descriptor(struct cell* args);
struct cell* builtin_record_type_fields(struct cell* args);
struct cell* builtin_record_type_name(struct cell* args);
struct cell* builtin_record_typep(struct cell* args);
struct cell* builtin_recordp(struct cell* args);
struct cell* builtin_rem(struct cell* args);
struct cell* builtin_reverse(struct cell* args);
struct cell* builtin_set_current_error_port(struct cell* args);
struct cell* builtin_set_current_input_port(struct cell* args);
struct cell* builtin_set_current_output_port(struct cell* args);
struct cell* builtin_setcar(struct cell* args);
struct cell* builtin_setcdr(struct cell* args);
struct cell* builtin_string_index(struct cell* args);
struct cell* builtin_string_ref(struct cell* args);
struct cell* builtin_string_size(struct cell* args);
struct cell* builtin_string_to_keyword(struct cell* args);
struct cell* builtin_string_to_list(struct cell* args);
struct cell* builtin_string_to_number(struct cell* args);
struct cell* builtin_string_to_symbol(struct cell* args);
struct cell* builtin_stringeq(struct cell* args);
struct cell* builtin_stringp(struct cell* args);
struct cell* builtin_sub(struct cell* args);
struct cell* builtin_sum(struct cell* args);
struct cell* builtin_symbol_to_string(struct cell* args);
struct cell* builtin_ttyname(struct cell* args);
struct cell* builtin_vector_length(struct cell* args);
struct cell* builtin_vector_ref(struct cell* args);
struct cell* builtin_vector_set(struct cell* args);
struct cell* builtin_vector_to_list(struct cell* args);
struct cell* builtin_vectoreq(struct cell* args);
struct cell* builtin_vectorp(struct cell* args);
struct cell* builtin_write(struct cell* args);
struct cell* builtin_write_error(struct cell* args);
struct cell* builtin_xor(struct cell* args);
struct cell* equal(struct cell* a, struct cell* b);
struct cell* extend(struct cell* env, struct cell* symbol, struct cell* value);
struct cell* make_prim(void* fun);
struct cell* make_string(char* a, int length);
struct cell* make_sym(char* name);
struct cell* nullp(struct cell* args);
struct cell* pairp(struct cell* args);
struct cell* portp(struct cell* args);
struct cell* symbolp(struct cell* args);


void spinup(struct cell* sym, struct cell* prim)
{
	all_symbols = make_cons(sym, all_symbols);
	top_env = extend(top_env, sym, prim);
}

/*** Initialization ***/
void init_sl3()
{
	/* Special symbols */
	nil = make_sym("()");
	cell_t = make_sym("#t");
	cell_f = make_sym("#f");
	cell_dot = make_sym(".");
	quote = make_sym("quote");
	quasiquote = make_sym("quasiquote");
	unquote = make_sym("unquote");
	unquote_splicing = make_sym("unquote-splicing");
	cell_unspecified = make_sym("#<unspecified>");
	s_if = make_sym("if");
	s_cond = make_sym("cond");
	s_lambda = make_sym("lambda");
	s_macro = make_sym("macro");
	s_define = make_sym("define");
	s_define_macro = make_sym("define-macro");
	s_setb = make_sym("set!");
	s_begin = make_sym("begin");
	s_let = make_sym("let");
	s_while = make_sym("while");

	/* Globals of interest */
	all_symbols = make_cons(nil, nil);
	top_env = extend(nil, nil, nil);

	/* Add Eval Specials */
	spinup(cell_t, cell_t);
	spinup(cell_f, cell_f);
	spinup(cell_dot, cell_dot);
	spinup(quote, quote);
	spinup(quasiquote, quasiquote);
	spinup(unquote, unquote);
	spinup(unquote_splicing, unquote_splicing);
	spinup(cell_unspecified, cell_unspecified);
	spinup(s_if, s_if);
	spinup(s_cond, s_cond);
	spinup(s_lambda, s_lambda);
	spinup(s_macro, s_macro);
	spinup(s_define, s_define);
	spinup(s_define_macro, s_define_macro);
	spinup(s_setb, s_setb);
	spinup(s_begin, s_begin);
	spinup(s_let, s_let);
	spinup(s_while, s_while);

	/* Add Primitive Specials */
	/* checking type */
	spinup(make_sym("char?"), make_prim(builtin_charp));
	spinup(make_sym("eof-object?"), make_prim(builtin_eofp));
	spinup(make_sym("list?"), make_prim(builtin_listp));
	spinup(make_sym("number?"), make_prim(builtin_intp));
	spinup(make_sym("null?"), make_prim(nullp));
	spinup(make_sym("pair?"), make_prim(pairp));
	spinup(make_sym("port?"), make_prim(portp));
	spinup(make_sym("primitive?"), make_prim(builtin_primitivep));
	spinup(make_sym("procedure?"), make_prim(builtin_procedurep));
	spinup(make_sym("string?"), make_prim(builtin_stringp));
	spinup(make_sym("symbol?"), make_prim(symbolp));
	spinup(make_sym("vector?"), make_prim(builtin_vectorp));
	spinup(make_sym("defined?"), make_prim(builtin_definedp));

	/* Comparisions */
	spinup(make_sym("<"), make_prim(builtin_numlt));
	spinup(make_sym("<="), make_prim(builtin_numle));
	spinup(make_sym("="), make_prim(builtin_numeq));
	spinup(make_sym(">"), make_prim(builtin_numgt));
	spinup(make_sym(">="), make_prim(builtin_numge));
	spinup(make_sym("char=?"), make_prim(builtin_chareq));
	spinup(make_sym("string=?"), make_prim(builtin_stringeq));
	spinup(make_sym("eq?"), make_prim(builtin_eq));
	spinup(make_sym("equal?"), make_prim(builtin_equal));

	/* Math */
	spinup(make_sym("*"), make_prim(builtin_prod));
	spinup(make_sym("+"), make_prim(builtin_sum));
	spinup(make_sym("-"), make_prim(builtin_sub));
	spinup(make_sym("ash"), make_prim(builtin_ash));
	spinup(make_sym("logand"), make_prim(builtin_logand));
	spinup(make_sym("logior"), make_prim(builtin_logor));
	spinup(make_sym("lognot"), make_prim(builtin_lognot));
	spinup(make_sym("logxor"), make_prim(builtin_xor));
	spinup(make_sym("modulo"), make_prim(builtin_mod));
	spinup(make_sym("quotient"), make_prim(builtin_div));
	spinup(make_sym("remainder"), make_prim(builtin_rem));

	/* Files */
	spinup(make_sym("open-input-file"), make_prim(builtin_open_read));
	spinup(make_sym("open-output-file"), make_prim(builtin_open_write));
	spinup(make_sym("close-port"), make_prim(builtin_close));
	spinup(make_sym("set-current-output-port"), make_prim(builtin_set_current_output_port));
	spinup(make_sym("set-current-input-port"), make_prim(builtin_set_current_input_port));
	spinup(make_sym("set-current-error-port"), make_prim(builtin_set_current_error_port));
	spinup(make_sym("current-output-port"), make_prim(builtin_current_output_port));
	spinup(make_sym("current-input-port"), make_prim(builtin_current_input_port));
	spinup(make_sym("current-error-port"), make_prim(builtin_current_error_port));
	spinup(make_sym("display"), make_prim(builtin_display));
	spinup(make_sym("display-error"), make_prim(builtin_display_error));
	spinup(make_sym("write"), make_prim(builtin_write));
	spinup(make_sym("read-char"), make_prim(builtin_read_byte));
	spinup(make_sym("primitive-load"), make_prim(builtin_primitive_load));
	spinup(make_sym("ttyname"), make_prim(builtin_ttyname));
	spinup(make_sym("port-filename"), make_prim(builtin_port_filename));

	/* Deal with Records */
	spinup(make_sym("make-record-type"), make_prim(builtin_make_record_type));
	spinup(make_sym("record-type-name"), make_prim(builtin_record_type_name));
	spinup(make_sym("record-type-fields"), make_prim(builtin_record_type_fields));
	spinup(make_sym("record-type?"), make_prim(builtin_record_typep));
	spinup(make_sym("record?"), make_prim(builtin_recordp));
	spinup(make_sym("record-type-descriptor"), make_prim(builtin_record_type_descriptor));

	/* Dealing with Lists */
	spinup(make_sym("list"), make_prim(builtin_list));
	spinup(make_sym("append"), make_prim(builtin_append));
	spinup(make_sym("list-length"), make_prim(builtin_list_length));
	spinup(make_sym("list->string"), make_prim(builtin_list_to_string));
	spinup(make_sym("list->vector"), make_prim(builtin_list_to_vector));
	spinup(make_sym("list->symbol"), make_prim(builtin_list_to_symbol));

	/* Deal with Vectors */
	spinup(make_sym("make-vector"), make_prim(builtin_make_vector));
	spinup(make_sym("vector-length"), make_prim(builtin_vector_length));
	spinup(make_sym("vector-set!"), make_prim(builtin_vector_set));
	spinup(make_sym("vector-ref"), make_prim(builtin_vector_ref));
	spinup(make_sym("vector->list"), make_prim(builtin_vector_to_list));

	/* Deal with Strings */
	spinup(make_sym("string->list"), make_prim(builtin_string_to_list));
	spinup(make_sym("string-length"), make_prim(builtin_string_size));
	spinup(make_sym("string-index"), make_prim(builtin_string_index));
	spinup(make_sym("string-ref"), make_prim(builtin_string_ref));
	spinup(make_sym("string->number"), make_prim(builtin_string_to_number));
	spinup(make_sym("string->symbol"), make_prim(builtin_string_to_symbol));

	/* Deal with symbols */
	spinup(make_sym("symbol->string"), make_prim(builtin_symbol_to_string));

	/* Deal with keywords */
	spinup(make_sym("keyword?"), make_prim(builtin_keywordp));
	spinup(make_sym("keyword->symbol"), make_prim(builtin_keyword_to_symbol));
	spinup(make_sym("string->keyword"), make_prim(builtin_string_to_keyword));

	/* Deal with numbers */
	spinup(make_sym("number->string"), make_prim(builtin_number_to_string));
	spinup(make_sym("integer->char"), make_prim(builtin_number_to_char));

	/* Deal with Chars */
	spinup(make_sym("char->integer"), make_prim(builtin_char_to_number));

	/* Deal with logicals */
	spinup(make_sym("not"), make_prim(builtin_not));
	spinup(make_sym("and"), make_prim(builtin_and));
	spinup(make_sym("or"), make_prim(builtin_or));

	/* Deal with environment */
	spinup(make_sym("getenv"), make_prim(builtin_get_env));
	spinup(make_sym("command-line"), make_prim(builtin_command_line));

	/* Lisp classics */
	spinup(make_sym("cons"), make_prim(builtin_cons));
	spinup(make_sym("car"), make_prim(builtin_car));
	spinup(make_sym("cdr"), make_prim(builtin_cdr));
	spinup(make_sym("reverse"), make_prim(builtin_reverse));
	spinup(make_sym("set-car!"), make_prim(builtin_setcar));
	spinup(make_sym("set-cdr!"), make_prim(builtin_setcdr));
	spinup(make_sym("apply"), make_prim(builtin_apply));
	spinup(make_sym("primitive-eval"), make_prim(builtin_primitive_eval));
	spinup(make_sym("exit"), make_prim(builtin_halt));

	/* MES unique */
	spinup(make_sym("free_mem"), make_prim(builtin_freecell));
	spinup(make_sym("%version"), make_string("0.19", 4));
	spinup(make_sym("vector=?"), make_prim(builtin_vectoreq));
	spinup(make_sym("list=?"), make_prim(builtin_listeq));
	spinup(make_sym("core:make-record"), make_prim(builtin_make_record));
	spinup(make_sym("core:record-predicate"), make_prim(builtin_record_predicate));
	spinup(make_sym("core:record-accessor"), make_prim(builtin_record_accessor));
	spinup(make_sym("core:record-modifier"), make_prim(builtin_record_modifier));
	spinup(make_sym("core:record-constructor"), make_prim(builtin_record_constructor));
	primitive_env = top_env;
	g_env = top_env;
}
