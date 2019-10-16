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
void initialize_constants();
SCM gc ();
void gc_init_cells();
void initialize_memory();
char *itoa (int number);
struct scm* mes_builtins(struct scm* a);
struct scm* cstring_to_symbol(char const *s);

struct scm* hashq_get_handle (struct scm* table, struct scm* key, struct scm* dflt);
struct scm* hashq_set_x (struct scm* table, struct scm* key, struct scm* value);
struct scm* equal2_p (struct scm* a, struct scm* b);
struct scm* string_equal_p (struct scm* a, struct scm* b);
struct scm* reverse_x_ (struct scm* x, struct scm* t);
struct scm* builtin_p (struct scm* x);
int eputs (char const* s);
struct scm* display_error_ (struct scm* x);
struct scm* write_error_ (struct scm* x);
struct scm* module_printer (struct scm* module);
struct scm* module_variable (struct scm* module, struct scm* name);
struct scm* module_ref (struct scm* module, struct scm* name);
struct scm* read_input_file_env ();
struct scm* init_time(struct scm* a);
struct scm* module_define_x(struct scm* module, struct scm* name, struct scm* value);

struct scm* make_hashq_type();
struct scm* make_module_type();
struct scm* make_struct (struct scm* type, struct scm* fields, struct scm* printer);
struct scm* make_cell(struct scm* type, struct scm* car, struct scm* cdr);
struct scm* make_string_(char const* s);
struct scm* make_string(char const* s, int length);
struct scm* make_hash_table_(struct scm* size);
struct scm* mes_g_stack(struct scm* a);

struct scm* eval_apply();
struct scm* mes_symbols();

/* M2-Planet Imports */
int numerate_string(char *a);

struct scm* make_number(SCM n);
struct scm* make_tpair(struct scm* a, struct scm* b);
struct scm* make_variable_(struct scm* var);


struct scm* assoc_string(struct scm* x, struct scm* a)  ///((internal))
{
	struct scm* b = a;
	struct scm* c;
	struct scm* tee = cell_t;
	struct scm* nil = cell_nil;

	do
	{
		if(b == nil) return cell_f;
		c = b->car;
		if(string_equal_p(x, c->car) == tee) return b->car;
		c = c->car;
		if(c->type != TSTRING) return cell_f;
		b = b->cdr;
	} while(TRUE);
}

struct scm* type_(struct scm* x)
{
	struct scm* y = x;
	return make_number(y->type);
}

struct scm* car_(struct scm* x)
{
	struct scm* y = x;
	if(y->type == TPAIR) return y->car;
	return make_number(y->rac);
}

struct scm* cdr_(struct scm* x)
{
	struct scm* y = x;
	if(y->type == TCHAR) return make_number(y->value);
	if(y->type == TNUMBER) return make_number(y->value);
	if(y->type == TPORT) return make_number(y->value);
	struct scm* z = y->cdr;
	if(z->type == TPAIR) return y->cdr;
	if(z->type == TREF) return y->cdr;
	if(z->type == TSPECIAL) return y->cdr;
	if(z->type == TSYMBOL) return y->cdr;
	if(z->type == TSTRING) return y->cdr;
	return make_number(y->value);
}

struct scm* cons_(struct scm* x, struct scm* y)
{
	return make_tpair(x, y);
}

struct scm* cons(struct scm* x, struct scm* y)
{
	return make_tpair(x, y);
}

struct scm* car(struct scm* x)
{
	struct scm* y = x;
	return y->car;
}

struct scm* cdr(struct scm* x)
{
	struct scm* y = x;
	return y->cdr;
}

struct scm* list(struct scm* x)  ///((arity . n))
{
	return x;
}

struct scm* null_p(struct scm* x)
{
	if(cell_nil == x) return cell_t;
	return cell_f;
}

struct scm* eq_p(struct scm* x, struct scm* y)
{
	struct scm* a = x;
	struct scm* b = y;
	struct scm* tee = cell_t;

	if(a == b) return cell_t;
	if(((a->type == TKEYWORD && b->type == TKEYWORD && tee == string_equal_p(x, y)))) return cell_t;
	if((a->type == TCHAR && b->type == TCHAR && a->value == b->value)) return cell_t;
	if((a->type == TNUMBER && b->type == TNUMBER && a->value == b->value)) return cell_t;
	return cell_f;
}

struct scm* values(struct scm* x)  ///((arity . n))
{
	struct scm* v = cons(0, x);
	struct scm* y = v;
	y->type = TVALUES;
	return v;
}

struct scm* acons(struct scm* key, struct scm* value, struct scm* alist)
{
	return cons(cons(key, value), alist);
}

SCM length__(struct scm* x)  ///((internal))
{
	SCM n = 0;

	while(x != cell_nil)
	{
		n = n + 1;

		if(x->type != TPAIR)
		{
			return -1;
		}

		x = x->cdr;
	}

	return n;
}

struct scm* length(struct scm* x)
{
	return make_number((length__(x)));
}

struct scm* apply(struct scm*, struct scm*);

struct scm* error(struct scm* key, struct scm* x)
{
	struct scm* throw= module_ref(R0, cell_symbol_throw);

	if(throw != cell_undefined)
	{
		return apply(throw, cons(key, cons(x, cell_nil)));
	}

	display_error_(key);
	eputs(": ");
	write_error_(x);
	eputs("\n");
	assert(0);
	exit(EXIT_FAILURE);
}

//  extra lib
struct scm* assert_defined(struct scm* x, struct scm* e)  ///((internal))
{
	if(e == cell_undefined)
	{
		return error(cell_symbol_unbound_variable, x);
	}

	return e;
}

struct scm* check_formals(struct scm* f, struct scm* formals, struct scm* args)  ///((internal))
{
	struct scm* formal = formals;
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
		struct scm* e = make_string(s, strlen(s));
		return error(cell_symbol_wrong_number_of_args, cons(e, f));
	}

	return cell_unspecified;
}

struct scm* check_apply(struct scm* f, struct scm* e)  ///((internal))
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

	struct scm* g = f;

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
		struct scm* e = make_string(s, strlen(s));
		return error(cell_symbol_wrong_type_arg, cons(e, f));
	}

	return cell_unspecified;
}

void gc_push_frame()  ///((internal))
{
	if(g_stack < 5)
	{
		assert(!"STACK FULL");
	}

	g_stack_array[g_stack - 1] = (struct scm*) cell_f;
	g_stack_array[g_stack - 2] = R0;
	g_stack_array[g_stack - 3] = R1;
	g_stack_array[g_stack - 4] = R2;
	g_stack_array[g_stack - 5] = R3;
	g_stack = g_stack - 5;
}

SCM gc_pop_frame()  ///((internal))
{
	/* POP VALUES */
	R3 = g_stack_array[g_stack];
	R2 = g_stack_array[g_stack + 1];
	R1 = g_stack_array[g_stack + 2];
	R0 = g_stack_array[g_stack + 3];

	/* Zero values on stack */
	g_stack_array[g_stack] = 0;
	g_stack_array[g_stack + 1] = 0;
	g_stack_array[g_stack + 2] = 0;
	g_stack_array[g_stack + 3] = 0;
	g_stack_array[g_stack + 4] = 0;

	g_stack = g_stack + 5;
	return g_stack;
}

struct scm* append2(struct scm* x, struct scm* y)
{
	struct scm* z = x;
	if(x == cell_nil)
	{
		return y;
	}

	if(z->type != TPAIR)
	{
		error(cell_symbol_not_a_pair, cons(x, cstring_to_symbol("append2")));
	}

	struct scm* r = cell_nil;

	while(z != cell_nil)
	{
		r = cons(z->car, r);
		z = z->cdr;
	}

	return reverse_x_(r, y);
}

struct scm* reverse_x_(struct scm* x, struct scm* t)
{
	struct scm* y = x;
	if(x != cell_nil && y->type != TPAIR)
	{
		error(cell_symbol_not_a_pair, cons(x, cstring_to_symbol("core:reverse!")));
	}

	struct scm* r = t;
	struct scm* s;

	while(y != cell_nil)
	{
		s = y->cdr;
		y->cdr = r;
		r = y;
		y = s;
	}

	return r;
}

struct scm* pairlis(struct scm* x, struct scm* y, struct scm* a)
{
	if(x == cell_nil)
	{
		return a;
	}

	struct scm* z = x;
	if(z->type != TPAIR)
	{
		return cons(cons(x, y), a);
	}

	return cons(cons(car(x), car(y)), pairlis(cdr(x), cdr(y), a));
}

struct scm* assq(struct scm* x, struct scm* a)
{
	struct scm* b = a;
	struct scm* y = x;
	struct scm* NIL = cell_nil;

	if(b->type != TPAIR)
	{
		return cell_f;
	}

	int t = y->type;

	if(t == TSYMBOL || t == TSPECIAL)
	{
		while(b != NIL)
		{
			if(x == (b->car->car)) return b->car;
			b = b->cdr;
		}
		return cell_f;
	}

	if(t == TCHAR || t == TNUMBER)
	{
		struct scm* v = y->cdr;
		while(b != NIL)
		{
			if(v == (b->car->car->cdr)) return b->car;
			b = b->cdr;
		}
		return cell_f;
	}

	if(t == TKEYWORD)
	{
		struct scm* F = cell_f;
		while(b != NIL)
		{
			if(F == string_equal_p(x, b->car->car)) return b->car;
			b = b->cdr;
		}
		return cell_f;
	}

	/* pointer equality, e.g. on strings. */
	while(b != NIL)
	{
		if(x == b->car->car) return b->car;
		b = b->cdr;
	}
	return cell_f;
}

struct scm* assoc(struct scm* x, struct scm* a)
{
	if(x->type == TSTRING)
	{
		return assoc_string(x, a);
	}

	while(a != cell_nil)
	{
		if(cell_f != equal2_p(x, a->car->car)) return a->car;
		a = a->cdr;
	}

	return cell_f;
}

struct scm* set_car_x(struct scm* x, struct scm* e)
{
	struct scm* y = x;
	if(y->type != TPAIR)
	{
		error(cell_symbol_not_a_pair, cons(x, cstring_to_symbol("set-car!")));
	}

	y->car = e;
	return cell_unspecified;
}

struct scm* set_cdr_x(struct scm* x, struct scm* e)
{
	struct scm* y = x;
	if(y->type != TPAIR)
	{
		error(cell_symbol_not_a_pair, cons(x, cstring_to_symbol("set-cdr!")));
	}

	y->cdr = e;
	return cell_unspecified;
}

struct scm* set_env_x(struct scm* x, struct scm* e, struct scm* a)
{
	struct scm* y = x;
	struct scm* p;

	if(y->type == TVARIABLE)
	{
		p = y->car;
	}
	else
	{
		p = assert_defined(x, module_variable(a, x));
	}

	if(p->type != TPAIR)
	{
		error(cell_symbol_not_a_pair, cons(p, x));
	}

	return set_cdr_x(p, e);
}

struct scm* call_lambda(struct scm* e, struct scm* x)  ///((internal))
{
	struct scm* cl = cons(cons(cell_closure, x), x);
	R1 = e;
	R0 = cl;
	return cell_unspecified;
}

struct scm* macro_get_handle(struct scm* name)
{
	struct scm* n = name;
	if(n->type == TSYMBOL)
	{
		return hashq_get_handle(g_macros, name, cell_nil);
	}

	return cell_f;
}

struct scm* get_macro(struct scm* name)  ///((internal))
{
	struct scm* m = macro_get_handle(name);

	if(m != cell_f)
	{
		return m->cdr->macro;
	}

	return cell_f;
}

struct scm* macro_set_x(struct scm* name, struct scm* value)  ///((internal))
{
	return hashq_set_x(g_macros, name, value);
}

struct scm* push_cc(struct scm* p1, struct scm* p2, struct scm* a, struct scm* c)  ///((internal))
{
	struct scm* x = R3;
	R3 = c;
	R2 = p2;
	gc_push_frame();
	R1 = p1;
	R0 = a;
	R3 = x;
	return cell_unspecified;
}

struct scm* add_formals(struct scm* formals, struct scm* x)
{
	struct scm* y = x;
	while(y->type == TPAIR)
	{
		formals = cons(y->car, formals);
		y = y->cdr;
	}

	if(y->type == TSYMBOL)
	{
		formals = cons(y, formals);
	}

	return formals;
}

struct scm* formal_p(struct scm* x, struct scm* formals)  /// ((internal))
{
	struct scm* f = formals;
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

	while(f->type == TPAIR && f->car != x)
	{
		f = f->cdr;
	}

	if(f->type == TSYMBOL)
	{
		if(f == x) return cell_t;
		return cell_f;
	}

	if(f->type == TPAIR) return cell_t;
	return cell_f;
}

struct scm* expand_variable_(struct scm* x, struct scm* formals, int top_p)  ///((internal))
{
	struct scm* y = x;
	while(y->type == TPAIR)
	{
		if(y->car->type == TPAIR)
		{
			if(y->car->car == cell_symbol_lambda)
			{
				struct scm* f = y->car->cdr->car;
				formals = add_formals(formals, f);
			}
			else if(y->car->car == cell_symbol_define || y->car->car == cell_symbol_define_macro)
			{
				struct scm* f = y->car->cdr->car;
				formals = add_formals(formals, f);
			}

			if(y->car->car != cell_symbol_quote)
			{
				expand_variable_(y->car, formals, 0);
			}
		}
		else
		{
			if(y->car == cell_symbol_lambda)
			{
				struct scm* f = y->cdr->car;
				formals = add_formals(formals, f);
				y = y->cdr;
			}
			else if(y->car == cell_symbol_define || y->car == cell_symbol_define_macro)
			{
				struct scm* f = y->cdr->car;

				if(top_p && f->type == TPAIR)
				{
					f = f->cdr;
				}

				formals = add_formals(formals, f);
				y = y->cdr;
			}
			else if(y->car == cell_symbol_quote)
			{
				return cell_unspecified;
			}
			else if(y->car->type == TSYMBOL
			        && y->car != cell_symbol_boot_module
			        && y->car != cell_symbol_current_module
			        && y->car != cell_symbol_primitive_load
			        && !formal_p(y->car, formals))
			{
				struct scm* v = module_variable(R0, y->car);

				if(v != cell_f)
				{
					y->car = make_variable_(v);
				}
			}
		}

		y = y->cdr;
		top_p = 0;
	}

	return cell_unspecified;
}

struct scm* expand_variable(struct scm* x, struct scm* formals)  ///((internal))
{
	return expand_variable_(x, formals, 1);
}

struct scm* apply(struct scm* f, struct scm* x)  ///((internal))
{
	push_cc(cons(f, x), cell_unspecified, R0, cell_unspecified);
	R3 = cell_vm_apply;
	return eval_apply();
}

// Jam Collector
SCM g_symbol_max;

int open_boot(char *prefix, char const *boot, char const *location);
void read_boot()  ///((internal))
{
	__stdin = -1;
	char prefix[1024];
	char boot[1024];

	if(getenv("MES_BOOT"))
	{
		strcpy(boot, getenv("MES_BOOT"));
	}
	else
	{
		strcpy(boot, "boot-0.scm");
	}

	if(getenv("MES_PREFIX"))
	{
		strcpy(prefix, getenv("MES_PREFIX"));
		strcpy(prefix + strlen(prefix), "/module");
		strcpy(prefix + strlen(prefix), "/mes/");
		__stdin = open_boot(prefix, boot, "MES_PREFIX");
	}

	if(__stdin < 0)
	{
		char const *p = "module/mes/";
		strcpy(prefix, p);
		__stdin = open_boot(prefix, boot, "module");
	}

	if(__stdin < 0)
	{
		strcpy(prefix, "mes/module/mes/");
		__stdin = open_boot(prefix, boot, ".");
	}

	if(__stdin < 0)
	{
		prefix[0] = 0;
		__stdin = open_boot(prefix, boot, "<boot>");
	}

	if(__stdin < 0)
	{
		eputs("mes: boot failed: no such file: ");
		eputs(boot);
		eputs("\n");
		exit(EXIT_FAILURE);
	}

	R2 = read_input_file_env();
	__stdin = STDIN;
}

int get_env_value(char* c, int alt)
{
	char* s = getenv(c);
	if(NULL == s) return alt;
	return numerate_string(s);
}

struct scm* mes_environment(int argc, char *argv[])
{
	struct scm* a = mes_symbols();
	a = acons(cell_symbol_compiler, make_string_("gnuc"), a);
	a = acons(cell_symbol_arch, make_string_("x86_64"), a);

	struct scm* lst = cell_nil;
	for(int i = argc - 1; i >= 0; i--)
	{
		lst = cons(make_string_(argv[i]), lst);
	}

	a = acons(cell_symbol_argv, lst, a);
	return mes_g_stack(a);
}

struct scm* make_initial_module(struct scm* a)  ///((internal))
{
	struct scm* module_type = make_module_type();
	a = acons(cell_symbol_module, module_type, a);
	struct scm* hashq_type = make_hashq_type();
	a = acons(cell_symbol_hashq_table, hashq_type, a);
	struct scm* b = a;
	struct scm* name = cons(cstring_to_symbol("boot"), cell_nil);
	struct scm* globals = make_hash_table_(0);
	struct scm* values = cons(cell_symbol_module, cons(name, cons(cell_nil, cons(globals, cell_nil))));
	struct scm* module = make_struct(module_type, values, cstring_to_symbol("module-printer"));
	R0 = cons(b->car, cons(b->cdr->car, cell_nil));
	M0 = module;

	while(b->type == TPAIR)
	{
		module_define_x(module, b->car->car, b->car->cdr);
		b = b->cdr;
	}

	return module;
}

int main(int argc, char *argv[])
{
	__ungetc_buf = calloc((RLIMIT_NOFILE + 1), sizeof(int));
	g_continuations = 0;
	g_symbols = 0;
	g_stack = 0;
	R0 = 0;
	R1 = 0;
	R2 = 0;
	R3 = 0;
	M0 = 0;
	g_macros = 0;
	g_cells = 0;
	__stdin = STDIN;
	__stdout = STDOUT;
	__stderr = STDERR;

	g_debug = get_env_value("MES_DEBUG", 0);

	if(g_debug > 1) eputs(";;; MODULEDIR=module\n");

	initialize_constants();
	initialize_memory();
	gc_init_cells();
	g_ports = cell_nil;

	struct scm* a = mes_environment(argc, argv);
	a = mes_builtins(a);
	a = init_time(a);
	M0 = make_initial_module(a);
	g_macros = make_hash_table_(0);

	if(g_debug > 4)
	{
		module_printer(M0);
	}

	read_boot();
	push_cc(R2, cell_unspecified, R0, cell_unspecified);

	if(g_debug > 2)
	{
		eputs("\ngc stats: [");
		eputs(itoa(g_free));
		eputs("]\n");
	}

	if(g_debug > 3)
	{
		eputs("program: ");
		write_error_(R1);
		eputs("\n");
	}

	R3 = cell_vm_begin_expand;
	R1 = eval_apply();

	if(g_debug)
	{
		write_error_(R1);
		eputs("\n");
	}

	if(g_debug)
	{
		if(g_debug > 4) module_printer(M0);

		eputs("\ngc stats: [");
		eputs(itoa(g_free));
		gc();
		eputs(" => ");
		eputs(itoa(g_free));
		eputs("]\n");

		if(g_debug > 4) module_printer(M0);

		if(g_debug > 3)
		{
			eputs("ports:");
			write_error_(g_ports);
			eputs("\n");
		}

		eputs("\n");
	}

	return 0;
}
