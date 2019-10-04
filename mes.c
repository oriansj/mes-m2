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
#include "mes_constants.h"

/* Imported Functions */
SCM gc ();
void initialize_memory();
char *itoa (int number);
int match(char* a, char* b);
struct scm* mes_builtins(struct scm* a);
struct scm* cstring_to_symbol(char const *s);

struct scm* hashq_get_handle (SCM table, SCM key, SCM dflt);
struct scm* hashq_set_x (SCM table, SCM key, SCM value);
SCM equal2_p (SCM a, SCM b);
struct scm* string_equal_p (SCM a, SCM b);
SCM reverse_x_ (SCM x, SCM t);
SCM builtin_p (SCM x);
int eputs (char const* s);
struct scm* display_error_ (SCM x);
struct scm* write_error_ (SCM x);
struct scm* module_printer (SCM module);
struct scm* module_variable (SCM module, SCM name);
struct scm* module_ref (SCM module, SCM name);
SCM read_input_file_env ();
SCM init_time(SCM a);
struct scm* module_define_x(SCM module, SCM name, SCM value);

struct scm* make_hashq_type();
struct scm* make_module_type();
struct scm* make_struct (SCM type, SCM fields, SCM printer);
SCM make_cell__(SCM type, SCM car, SCM cdr);
struct scm* make_cell(SCM type, struct scm* car, struct scm* cdr);
struct scm* make_string_(char const* s);
struct scm* make_string(char const* s, int length);
struct scm* make_hash_table_(SCM size);
SCM mes_g_stack(SCM a);

SCM eval_apply();
SCM mes_symbols();

/* M2-Planet Imports */
int numerate_string(char *a);


SCM assoc_string(SCM x, SCM a)  ///((internal))
{
	struct scm* b = Getstructscm2(a);
	struct scm* c;
	struct scm* tee = Getstructscm2(cell_t);
	struct scm* nil = Getstructscm2(cell_nil);

	do
	{
		if(b == nil) return cell_f;
		c = bad2good(b->car);
		if(string_equal_p(x, c->rac) == tee) return b->rac;
		c = bad2good(c->car);
		if(c->type != TSTRING) return cell_f;
		b = bad2good(b->cdr);
	} while(TRUE);
}

SCM type_(SCM x)
{
	struct scm* y = Getstructscm2(x);
	return make_cell__ (TNUMBER, 0, y->type);
}

SCM car_(SCM x)
{
	struct scm* y = Getstructscm2(x);
	if(y->type == TPAIR) return y->rac;
	return make_cell__ (TNUMBER, 0, y->rac);
}

SCM cdr_(SCM x)
{
	struct scm* y = Getstructscm2(x);
	if(y->type == TCHAR) return make_cell__ (TNUMBER, 0, y->rdc);
	if(y->type == TNUMBER) return make_cell__ (TNUMBER, 0, y->rdc);
	if(y->type == TPORT) return make_cell__ (TNUMBER, 0, y->rdc);
	struct scm* z = bad2good(y->cdr);
	if(z->type == TPAIR) return y->rdc;
	if(z->type == TREF) return y->rdc;
	if(z->type == TSPECIAL) return y->rdc;
	if(z->type == TSYMBOL) return y->rdc;
	if(z->type == TSTRING) return y->rdc;
	return make_cell__ (TNUMBER, 0, y->rdc);
}

SCM cons_(SCM x, SCM y)
{
	return make_cell__(TPAIR, x, y);
}

struct scm* cons(struct scm* x, struct scm* y)
{
	return make_cell(TPAIR, x, y);
}

SCM cons3(struct scm* x, struct scm* y)
{
	return GetSCM2(cons(x, y));
}


SCM car(SCM x)
{
	struct scm* y = Getstructscm2(x);
	return y->rac;
}

SCM cdr(SCM x)
{
	struct scm* y = Getstructscm2(x);
	return y->rdc;
}

SCM list(SCM x)  ///((arity . n))
{
	return x;
}

SCM null_p(SCM x)
{
	return x == cell_nil ? cell_t : cell_f;
}

SCM eq_p(SCM x, SCM y)
{
	struct scm* a = Getstructscm2(x);
	struct scm* b = Getstructscm2(y);
	struct scm* tee = Getstructscm2(cell_t);

	if(a == b) return cell_t;
	if(((a->type == TKEYWORD && b->type == TKEYWORD && tee == string_equal_p(x, y)))) return cell_t;
	if((a->type == TCHAR && b->type == TCHAR && a->value == b->value)) return cell_t;
	if((a->type == TNUMBER && b->type == TNUMBER && a->value == b->value)) return cell_t;
	return cell_f;
}

SCM values(SCM x)  ///((arity . n))
{
	SCM v = cons_(0, x);
	struct scm* y = Getstructscm2(v);
	y->type = TVALUES;
	return v;
}

SCM acons_(SCM key, SCM value, SCM alist)
{
	return cons_(cons_(key, value), alist);
}

struct scm* acons2(struct scm* key, struct scm* value, struct scm* alist)
{
	return cons(cons(key, value), alist);
}

SCM acons3(struct scm* key, struct scm* value, struct scm* alist)
{
	return cons3(cons(key, value), alist);
}


SCM length__(SCM x)  ///((internal))
{
	SCM n = 0;
	struct scm* y = Getstructscm2(x);
	struct scm* NIL = Getstructscm2(cell_nil);

	while(y != NIL)
	{
		n++;

		if(y->type != TPAIR)
		{
			return -1;
		}

		y = bad2good(y->cdr);
	}

	return n;
}

SCM length(SCM x)
{
	return make_cell__ (TNUMBER, 0, (length__(x)));
}

SCM apply(SCM, SCM);

SCM error(SCM key, SCM x)
{
	SCM throw= GetSCM2(module_ref(GetSCM2(bad2good(R0)), cell_symbol_throw));

	if(throw != cell_undefined)
	{
		return apply(throw, cons_(key, cons_(x, cell_nil)));
	}

	display_error_(key);
	eputs(": ");
	write_error_(x);
	eputs("\n");
	assert(0);
	exit(EXIT_FAILURE);
}

//  extra lib
SCM assert_defined(SCM x, SCM e)  ///((internal))
{
	if(e == cell_undefined)
	{
		return error(cell_symbol_unbound_variable, x);
	}

	return e;
}

SCM check_formals(SCM f, SCM formals, SCM args)  ///((internal))
{
	struct scm* formal = Getstructscm2(formals);
	SCM flen;
	if(formal->type == TNUMBER)
	{
		flen = formal->value;
	}
	else
	{
		flen = length__(formals);
	}
	SCM alen = length__(args);

	if(alen != flen && alen != -1 && flen != -1)
	{
		char *s = "apply: wrong number of arguments; expected: ";
		eputs(s);
		eputs(itoa(flen));
		eputs(", got: ");
		eputs(itoa(alen));
		eputs("\n");
		write_error_(f);
		SCM e = GetSCM2(bad2good(make_string(s, strlen(s))));
		return error(cell_symbol_wrong_number_of_args, cons_(e, f));
	}

	return cell_unspecified;
}

SCM check_apply(SCM f, SCM e)  ///((internal))
{
	char* type = 0;

	if(f == cell_f || f == cell_t)
	{
		type = "bool";
	}

	if(f == cell_nil)
	{
		type = "nil";
	}

	if(f == cell_unspecified)
	{
		type = "*unspecified*";
	}

	if(f == cell_undefined)
	{
		type = "*undefined*";
	}

	struct scm* g = Getstructscm2(f);

	if(g->type == TCHAR)
	{
		type = "char";
	}

	if(g->type == TNUMBER)
	{
		type = "number";
	}

	if(g->type == TSTRING)
	{
		type = "string";
	}

	if(g->type == TSTRUCT && builtin_p(f) == cell_f)
	{
		type = "#<...>";
	}

	if(g->type == TBROKEN_HEART)
	{
		type = "<3";
	}

	if(type)
	{
		char *s = "cannot apply: ";
		eputs(s);
		eputs(type);
		eputs("[");
		write_error_(e);
		eputs("]\n");
		SCM e = GetSCM2(bad2good(make_string(s, strlen(s))));
		return error(cell_symbol_wrong_type_arg, cons_(e, f));
	}

	return cell_unspecified;
}

SCM gc_push_frame()  ///((internal))
{
	if(g_stack < 5)
	{
		assert(!"STACK FULL");
	}

	g_stack_array[--g_stack] = (struct scm*) cell_f;
	g_stack_array[--g_stack] = R0;
	g_stack_array[--g_stack] = R1;
	g_stack_array[--g_stack] = R2;
	g_stack_array[--g_stack] = R3;
	return g_stack;
}

SCM gc_peek_frame()  ///((internal))
{
	R3 = g_stack_array[g_stack];
	R2 = g_stack_array[g_stack + 1];
	R1 = g_stack_array[g_stack + 2];
	R0 = g_stack_array[g_stack + 3];
	return (SCM) g_stack_array[g_stack + FRAME_PROCEDURE];
}

SCM gc_pop_frame()  ///((internal))
{
	SCM x = gc_peek_frame();
	g_stack_array[g_stack] = 0;
	g_stack_array[g_stack + 1] = 0;
	g_stack_array[g_stack + 2] = 0;
	g_stack_array[g_stack + 3] = 0;
	g_stack_array[g_stack + 4] = 0;
	g_stack += 5;
	return x;
}

SCM append2(SCM x, SCM y)
{
	struct scm* z = Getstructscm2(x);
	if(x == cell_nil)
	{
		return y;
	}

	if(z->type != TPAIR)
	{
		error(cell_symbol_not_a_pair, cons_(x, GetSCM2(cstring_to_symbol("append2"))));
	}

	SCM r = cell_nil;

	while(GetSCM2(z) != cell_nil)
	{
		r = cons_(z->rac, r);
		z = bad2good(z->cdr);
	}

	return reverse_x_(r, y);
}

SCM reverse_x_(SCM x, SCM t)
{
	struct scm* y = Getstructscm2(x);
	if(x != cell_nil && y->type != TPAIR)
	{
		error(cell_symbol_not_a_pair, cons_(x, GetSCM2(cstring_to_symbol("core:reverse!"))));
	}

	struct scm* r = Getstructscm2(t);
	struct scm* s;

	while(GetSCM2(y) != cell_nil)
	{
		s = bad2good(y->cdr);
		y->cdr = good2bad(r);
		r = y;
		y = s;
	}

	return GetSCM2(r);
}

SCM pairlis(SCM x, SCM y, SCM a)
{
	if(x == cell_nil)
	{
		return a;
	}

	struct scm* z = Getstructscm2(x);
	if(z->type != TPAIR)
	{
		return cons_(cons_(x, y), a);
	}

	return cons_(cons_(car(x), car(y)), pairlis(cdr(x), cdr(y), a));
}

SCM assq(SCM x, SCM a)
{
	struct scm* b = Getstructscm2(a);
	struct scm* y = Getstructscm2(x);
	struct scm* NIL = Getstructscm2(cell_nil);

	if(b->type != TPAIR)
	{
		return cell_f;
	}

	int t = y->type;

	if(t == TSYMBOL || t == TSPECIAL)
	{
		while(b != NIL)
		{
			if(x == (bad2good(b->car)->rac)) return b->rac;
			b = bad2good(b->cdr);
		}
		return cell_f;
	}

	if(t == TCHAR || t == TNUMBER)
	{
		SCM v = y->value;
		while(b != NIL)
		{
			if(v == (bad2good(bad2good(b->car)->car)->value)) return b->rac;
			b = bad2good(b->cdr);
		}
		return cell_f;
	}

	if(t == TKEYWORD)
	{
		struct scm* F = Getstructscm2(cell_f);
		while(b != NIL)
		{
			if(F == string_equal_p(x, bad2good(b->car)->rac)) return b->rac;
			b = bad2good(b->cdr);
		}
		return cell_f;
	}

	/* pointer equality, e.g. on strings. */
	while(b != NIL)
	{
		if(x == bad2good(b->car)->rac) return b->rac;
		b = bad2good(b->cdr);
	}
	return cell_f;
}

SCM assoc(SCM x, SCM a)
{
	struct scm* y = Getstructscm2(x);
	struct scm* b = Getstructscm2(a);
	struct scm* NIL = Getstructscm2(cell_nil);

	if(y->type == TSTRING)
	{
		return assoc_string(x, a);
	}

	while(b != NIL)
	{
		if(cell_f != equal2_p(x, bad2good(b->car)->rac)) return b->rac;
		b = bad2good(b->cdr);
	}

	return cell_f;
}

SCM set_car_x(SCM x, SCM e)
{
	struct scm* y = Getstructscm2(x);
	if(y->type != TPAIR)
	{
		error(cell_symbol_not_a_pair, cons_(x, GetSCM2(cstring_to_symbol("set-car!"))));
	}

	y->rac = e;
	return cell_unspecified;
}

SCM set_cdr_x(SCM x, SCM e)
{
	struct scm* y = Getstructscm2(x);
	if(y->type != TPAIR)
	{
		error(cell_symbol_not_a_pair, cons_(x, GetSCM2(cstring_to_symbol("set-cdr!"))));
	}

	y->rdc = e;
	return cell_unspecified;
}

SCM set_env_x(SCM x, SCM e, SCM a)
{
	struct scm* y = Getstructscm2(x);
	struct scm* p;

	if(y->type == TVARIABLE)
	{
		p = Getstructscm2(y->rac);
	}
	else
	{
		p = Getstructscm2(assert_defined(x, GetSCM2(module_variable(a, x))));
	}

	if(p->type != TPAIR)
	{
		error(cell_symbol_not_a_pair, cons_(GetSCM2(p), x));
	}

	return set_cdr_x(GetSCM2(p), e);
}

SCM call_lambda(SCM e, SCM x)  ///((internal))
{
	SCM cl = cons_(cons_(cell_closure, x), x);
	R1 = good2bad(Getstructscm2(e));
	R0 = good2bad(Getstructscm2(cl));
	return cell_unspecified;
}

SCM make_closure_(SCM args, SCM body, SCM a)  ///((internal))
{
	return make_cell__(TCLOSURE, cell_f, cons_(cons_(cell_circular, a), cons_(args, body)));
}

SCM make_variable_(SCM var)  ///((internal))
{
	return make_cell__(TVARIABLE, var, 0);
}

SCM macro_get_handle(SCM name)
{
	struct scm* n = Getstructscm2(name);
	if(n->type == TSYMBOL)
	{
		return GetSCM2(bad2good(hashq_get_handle(g_macros, name, cell_nil)));
	}

	return cell_f;
}

SCM get_macro(SCM name)  ///((internal))
{
	struct scm* m = Getstructscm2(macro_get_handle(name));

	if(GetSCM2(m) != cell_f)
	{
		return bad2good(m->cdr)->macro;
	}

	return cell_f;
}

SCM macro_set_x(SCM name, SCM value)  ///((internal))
{
	return GetSCM2(hashq_set_x(g_macros, name, value));
}

SCM push_cc(SCM p1, SCM p2, SCM a, SCM c)  ///((internal))
{
	struct scm* x = R3;
	R3 = good2bad(Getstructscm2(c));
	R2 = good2bad(Getstructscm2(p2));
	gc_push_frame();
	R1 = good2bad(Getstructscm2(p1));
	R0 = good2bad(Getstructscm2(a));
	R3 = x;
	return cell_unspecified;
}

SCM add_formals(SCM formals, SCM x)
{
	struct scm* y = Getstructscm2(x);
	while(y->type == TPAIR)
	{
		formals = cons_(y->rac, formals);
		y = bad2good(y->cdr);
	}

	if(y->type == TSYMBOL)
	{
		formals = cons_(GetSCM2(y), formals);
	}

	return formals;
}

int formal_p(SCM x, SCM formals)  /// ((internal))
{
	struct scm* f = Getstructscm2(formals);
	if(f->type == TSYMBOL)
	{
		if(x == formals)
		{
			return x;
		}
		else
		{
			return cell_f;
		}
	}

	while(f->type == TPAIR && f->rac != x)
	{
		f = bad2good(f->cdr);
	}

	if(f->type == TSYMBOL)
	{
		return GetSCM2(f) == x;
	}

	return f->type == TPAIR;
}

SCM expand_variable_(SCM x, SCM formals, int top_p)  ///((internal))
{
	struct scm* y = Getstructscm2(x);
	while(y->type == TPAIR)
	{
		if(bad2good(y->car)->type == TPAIR)
		{
			if(bad2good(y->car)->rac == cell_symbol_lambda)
			{
				SCM f = bad2good(bad2good(y->car)->cdr)->rac;
				formals = add_formals(formals, f);
			}
			else if(bad2good(y->car)->rac == cell_symbol_define || bad2good(y->car)->rac == cell_symbol_define_macro)
			{
				SCM f = bad2good(bad2good(y->car)->cdr)->rac;
				formals = add_formals(formals, f);
			}

			if(bad2good(y->car)->rac != cell_symbol_quote)
			{
				expand_variable_(y->rac, formals, 0);
			}
		}
		else
		{
			if(y->rac == cell_symbol_lambda)
			{
				SCM f = bad2good(y->cdr)->rac;
				formals = add_formals(formals, f);
				y = bad2good(y->cdr);
			}
			else if(y->rac == cell_symbol_define || y->rac == cell_symbol_define_macro)
			{
				struct scm* f = bad2good(bad2good(y->cdr)->car);

				if(top_p && f->type == TPAIR)
				{
					f = bad2good(f->cdr);
				}

				formals = add_formals(formals, GetSCM2(f));
				y = bad2good(y->cdr);
			}
			else if(y->rac == cell_symbol_quote)
			{
				return cell_unspecified;
			}
			else if(bad2good(y->car)->type == TSYMBOL
			        && y->rac != cell_symbol_boot_module
			        && y->rac != cell_symbol_current_module
			        && y->rac != cell_symbol_primitive_load
			        && !formal_p(y->rac, formals))
			{
				SCM v = GetSCM2(module_variable(GetSCM2(bad2good(R0)), y->rac));

				if(v != cell_f)
				{
					y->rac = make_variable_(v);
				}
			}
		}

		y = bad2good(y->cdr);
		top_p = 0;
	}

	return cell_unspecified;
}

SCM expand_variable(SCM x, SCM formals)  ///((internal))
{
	return expand_variable_(x, formals, 1);
}

SCM apply(SCM f, SCM x)  ///((internal))
{
	push_cc(cons_(f, x), cell_unspecified, GetSCM2(bad2good(R0)), cell_unspecified);
	R3 = good2bad(Getstructscm2(cell_vm_apply));
	return eval_apply();
}

// Jam Collector
SCM g_symbol_max;

int get_env_value(char* c, int alt)
{
	char* s = getenv(c);
	if(NULL == s) return alt;
	return numerate_string(s);
}

struct scm* make_initial_module(SCM a)  ///((internal))
{
	SCM module_type = GetSCM2(make_module_type());
	a = acons_(cell_symbol_module, module_type, a);
	SCM hashq_type = GetSCM2(bad2good(make_hashq_type()));
	a = acons_(cell_symbol_hashq_table, hashq_type, a);
	struct scm* b = Getstructscm2(a);
	SCM name = cons_(GetSCM2(cstring_to_symbol("boot")), cell_nil);
	SCM globals = GetSCM2(bad2good(make_hash_table_(0)));
	SCM values = cons_(cell_symbol_module, cons_(name, cons_(cell_nil, cons_(globals, cell_nil))));
	SCM module = GetSCM2(make_struct(module_type, values, GetSCM2(cstring_to_symbol("module-printer"))));
	R0 = good2bad(Getstructscm2(cons_(b->rac, cons_(bad2good(b->cdr)->rac, cell_nil))));
	M0 = good2bad(Getstructscm2(module));

	while(b->type == TPAIR)
	{
		module_define_x(module, bad2good(b->car)->rac, bad2good(b->car)->rdc);
		b = bad2good(b->cdr);
	}

	return good2bad(Getstructscm2(module));
}

int open_boot(char *boot);
void do_it(char* file)
{
	if(match(file, "STDIN"))
	{
		__stdin = STDIN;
	}
	else
	{
		__stdin = open_boot(file);
	}

	R2 = good2bad(Getstructscm2(read_input_file_env()));
	push_cc(GetSCM2(bad2good(R2)), cell_unspecified, GetSCM2(bad2good(R0)), cell_unspecified);

	R3 = good2bad(Getstructscm2(cell_vm_begin_expand));
	R1 = good2bad(Getstructscm2(eval_apply()));
}

int main(int argc, char *argv[])
{
	__ungetc_buf = calloc((RLIMIT_NOFILE + 1), sizeof(int));
	g_continuations = 0;
	g_symbols = 0;
	g_stack = 0;
	messy_display = FALSE;
	R0 = good2bad(Getstructscm2(0));
	R1 = good2bad(Getstructscm2(0));
	R2 = good2bad(Getstructscm2(0));
	R3 = good2bad(Getstructscm2(0));
	M0 = good2bad(Getstructscm2(0));
	g_macros = 0;
	g_ports = 1;
	g_cells = 0;
	__stdin = STDIN;
	__stdout = STDOUT;
	__stderr = STDERR;

	initialize_memory();
	mes_symbols();

	M0 = make_initial_module(init_time(GetSCM2(mes_builtins(Getstructscm2(cell_nil)))));
	g_macros = GetSCM2(bad2good(make_hash_table_(0)));

	struct scm* lst = Getstructscm2(cell_nil);
	int i = 1;
	char* file;
	while(i <= argc)
	{
		if(NULL == argv[i])
		{
			i = i + 1;
		}
		else if(match(argv[i], "--boot"))
		{
			file = argv[i + 1];
			do_it(file);
			i = i + 2;
		}
		else if(match(argv[i], "-f") || match(argv[i], "--file"))
		{
			file = argv[i + 1];
			do_it(file);
			i = i + 2;
		}
		else
		{
			lst = cons(make_string_(argv[i]), lst);
			i = i + 1;
		}
	}

	messy_display = TRUE;
	do_it("STDIN");
	return 0;
}
