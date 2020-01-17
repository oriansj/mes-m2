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
struct cell* make_proc(struct cell* a, struct cell* b, struct cell* env);
struct cell* make_macro(struct cell* a, struct cell* b, struct cell* env);
struct cell* reverse_list(struct cell* head);


/* Support functions */
struct cell* findsym(char *name)
{
	struct cell* symlist;
	for(symlist = all_symbols; nil != symlist; symlist = symlist->cdr)
	{
		if(match(name, symlist->car->string))
		{
			return symlist;
		}
	}
	return nil;
}


/****************************************
 * assoc looks up variables from their  *
 * symbol in the current environment    *
 * Which is structured like so:         *
 * CONS -> NEXT ALIST CONS ->...-> NIL  *
 *  |                                   *
 *  V                                   *
 * CONS -> VALUE                        *
 *  |                                   *
 *  V                                   *
 * SYM -> TEXT                          *
 *                                      *
 * Because efforts are used to leverage *
 * The same string for all symbols, we  *
 * can usually cheat and just compare   *
 * the string pointers themselves       *
 * (FAST) and if that fails, we perform *
 * a last ditch (SLOW) lookup, which    *
 * actually compares the contents of    *
 * the strings themselves.              *
 ***************************************/
struct cell* assoc(struct cell* key, struct cell* alist)
{
	if(nil == alist) return nil;
	struct cell* i;
	for(i = alist; nil != i; i = i->cdr)
	{
		if(i->car->car->string == key->string) return i->car;
	}

	if(SYM != key->type) return nil;

	/* Last ditch effort (REALLY SLOW) */
	for(i = alist; nil != i; i = i->cdr)
	{
		if(match(i->car->car->string, key->string)) return i->car;
	}
	return nil;
}


/****************************************
 * The cell stack is just a stack of    *
 * pointers into the pool of CELLs;     *
 * Which grows in the correct direction *
 * (I'm looking at you x86 and AMD64)   *
 * UP and shrinks DOWN towards the      *
 * bottom. To prevent confusion, the    *
 * stack is ZERO'd as values are POP'd  *
 * off. The stack pointer of course is  *
 * always pointing to the first free    *
 * space on the stack.                  *
 *                                      *
 * TODO: add stack bound checks for     *
 * To catch wildly bad states.          *
 ***************************************/
void push_cell(struct cell* a)
{
	g_stack[stack_pointer] = a;
	stack_pointer = stack_pointer + 1;
}

struct cell* pop_cell()
{
	stack_pointer = stack_pointer - 1;
	struct cell* r = g_stack[stack_pointer];
	g_stack[stack_pointer] = NULL;

	return r;
}

/*** Evaluator (Eval/Apply) ***/
void eval();
void evlis()
{
	if(R0 == nil)
	{
		R1 = nil;
		return;
	}

	push_cell(g_env);
	push_cell(R0->cdr);
	R0 = R0->car;
	eval();
	R0 = pop_cell();
	struct cell* i = R1;
	g_env = pop_cell();
	evlis();
	struct cell* j = R1;
	R1 = make_cons(i, j);
}


/****************************************
 * apply is a seperate function because *
 * honestly, I like it better that way  *
 * it easy could be copy and pasted     *
 * into the eval and primitive-apply    *
 * updated accordingly to make that     *
 * work.                                *
 * We probably could make proc -> R0    *
 * and vals -> R1 if we run into some   *
 * craziness like garbage collection    *
 * being called between apply and eval  *
 ****************************************/
void apply(struct cell* proc, struct cell* vals)
{
	FUNCTION* fp;
	struct cell* syms;
	if(proc->type == PRIMOP)
	{
		/* Deal with the simple case of if we have a primitive */
		fp = proc->function;
		R1 = fp(vals);
		return;
	}
	else if(proc->type == LAMBDA)
	{
		/****************************************
		 * The reason for protecting g_env and  *
		 * using proc->env instead is to ensure *
		 * that the last values set to the      *
		 * locals don't survive past the        *
		 * application of the lambda.           *
		 *                                      *
		 * A better way of extending the        *
		 * environment with values for the      *
		 * locals that doesn't result in        *
		 * dynamic scope but would allow for    *
		 * forward references, I haven't        *
		 * figured out yet.                     *
		 ****************************************/
		push_cell(g_env);
		g_env = proc->env;
		syms = proc->car;

		/* extend the locals*/
		while(nil != syms)
		{
			/* Support (define (foo a b . rest) ...) sort of s-expressions */
			if(cell_dot == syms->car)
			{
				g_env = make_cons(make_cons(syms->cdr->car, vals), g_env);
				/* Ignore all symbols after the . rest */
				syms = nil;
			}
			else
			{
				/* Support common case of just mapping of a to 4 in (define (foo a b ..)); (foo 4 5 ..) */
				g_env = make_cons(make_cons(syms->car, vals->car), g_env);
				syms = syms->cdr;
				vals = vals->cdr;
			}
		}

		proc->env = g_env;
		R0 = make_cons(s_begin, proc->cdr);
		eval();
		g_env = pop_cell();
		return;
	}
	file_print("Bad argument to apply: ", stderr);
	require(SYM == proc->type, "{ERROR} unable to print string name\n");
	file_print(proc->string, stderr);
	file_print("\nAborting to avoid problems\n", stderr);
	exit(EXIT_FAILURE);
}

void eval()
{
	struct cell* fun;
	struct cell* arguments;
	struct cell* name;
	struct cell* i;
	struct cell* f;
	struct cell* h;

	if(SYM == R0->type)
	{
		/* Simply lookup the symbol in the environment */
		R1 = assoc(R0, g_env);
		/* bail hard if it is not found */
		if(R1 == nil)
		{
			file_print("Unbound symbol: ", stderr);
			file_print(R0->string, stderr);
			file_print("\nAborting before problems can occur\n", stderr);
			exit(EXIT_FAILURE);
		}

		/* we want the value of the symbol not the symbol itself */
		R1 = R1->cdr;
		return;
	}
	else if(CONS == R0->type)
	{
		if(R0->car == s_if)
		{
			require(nil != R0->cdr, "naked if statement is not a valid s-expression\n");
			/* Evaluate the conditional */
			push_cell(R0);
			R0 = R0->cdr->car;
			eval();
			R0 = pop_cell();

			/* Execute if not false because that is what guile does (believe everything not #f is true) */
			if(R1 != cell_f)
			{
				R0 = R0->cdr->cdr->car;
				eval();
				return;
			}

			/* If there is no ELSE statement do as guile does */
			if(nil == R0->cdr->cdr->cdr)
			{
				R1 = cell_unspecified;
				return;
			}

			/* Just do the ELSE s-expression */
			R0 = R0->cdr->cdr->cdr->car;
			eval();
			return;
		}
		else if(R0->car == s_when)
		{
			require(nil != R0->cdr, "naked when statement is not a valid s-expression\n");
			/* Evaluate the conditional */
			push_cell(R0);
			R0 = R0->cdr->car;
			eval();
			R0 = pop_cell();

			/* Execute if not false because that is what guile does (believe everything not #f is true) */
			if(R1 != cell_f)
			{
				R0 = R0->cdr->cdr->car;
				eval();
				return;
			}

			/* Just do what guile does */
			R1 = cell_unspecified;
			return;
		}
		else if(R0->car == s_cond)
		{
			/* Get past the COND */
			R0 = R0->cdr;

			/* Provide a way to flag no fields in cond */
			R1 = NULL;

			/* Loop until end of list of s-expressions */
			while(nil != R0)
			{
				/* Protect remaining list of s-expressions from garbage collection */
				push_cell(R0);

				/* Evaluate the conditional */
				R0 = R0->car->car;
				eval();
				R0 = pop_cell();

				/* Execute if not false because that is what guile does (believe everything not #f is true) */
				if(cell_f != R1)
				{
					R0 = R0->car->cdr->car;
					eval();
					return;
				}

				/* Iterate to the next in the list of s-expressions */
				R0 = R0->cdr;

				/* The default return in guile if it hits nil */
				R1 = cell_unspecified;
			}

			require(NULL != R1, "a naked cond is not supported\n");
			return;
		}
		else if(R0->car == s_lambda)
		{
			/* (lambda (a b .. N) (s-expression)) */
			R1 = make_proc(R0->cdr->car, R0->cdr->cdr, make_cons(g_env->car, g_env->cdr));
			return;
		}
		else if(R0->car == quote)
		{
			/* (quote (...)) */
			R1 = R0->cdr->car;
			return;
		}
		else if(R0->car == quasiquote)
		{
			/* Protect the s-expression during the entire evaluation */
			push_cell(R0);

			/* (quasiquote (...)) */
			i = R0->cdr->car;
			f = NULL;
			while(nil != i)
			{
				require(NULL != i, "Null in quasiquote expression reached\n");
				require(CONS == i->type, "Not a cons list in quasiquote reached\n");
				h = i->car;
				if(CONS == i->car->type)
				{
					if(unquote == i->car->car)
					{
						R0 = i->car->cdr->car;
						eval();
						h = R1;
					}
					if(unquote_splicing == i->car->car)
					{
						R0 = i->car->cdr->car;
						eval();
						while((NULL != R1) && (nil != R1))
						{
							/* Unsure if correct behavior is to revert to unquote behavior (what guile does) */
							/* Or restrict to just proper lists as the spec (r7rs) requires */
							/* eg. `(foo bar ,@(+ 4 5)) */
							require(CONS == R1->type, "unquote-splicing requires argument of type <proper list>\n");
							f = make_cons(R1->car, f);
							/* Simply convert require to if and the above */
							/* else f = make_cons(R1, f); */
							R1 = R1->cdr;
						}

						/* we really don't want to add that cons after what we just did */
						goto restart_quasiquote;
					}
				}
				f = make_cons(h, f);
restart_quasiquote:
				/* keep walking down the list of s-expressions */
				i = i->cdr;
			}

			/* We created the list backwards because it was simpler, now we have to put it into correct order */
			i = f;
			f = reverse_list(f);
			require(NULL != i, "Impossible quasiquote processed?\n");
			i->cdr = nil;
			R1 = f;

			/* We are finally done with the s-expression, we don't need it back */
			pop_cell();
			return;
		}
		else if(R0->car == s_define)
		{
			/* To support (define (foo a b .. N) (s-expression)) form */
			if(CONS == R0->cdr->car->type)
			{
				fun = R0->cdr->cdr;
				arguments = R0->cdr->car->cdr;
				name = R0->cdr->car->car;
				/* by converting it into (define foo (lambda (a b .. N) (s-expression))) form */
				R0->cdr = make_cons(name, make_cons(make_cons(s_lambda, make_cons(arguments, fun)), nil));
			}

			/* Protect the name from garbage collection */
			push_cell(R0->cdr->car);

			/* Evaluate the s-expression which the name is supposed to equal */
			R0 = R0->cdr->cdr->car;
			eval();
			R0 = pop_cell();

			/* If we define a LAMBDA/MACRO, we need to extend its environment otherwise it can not call itself recursively */
			if((LAMBDA == R1->type) || (MACRO == R1->type))
			{
				R1->env = make_cons(make_cons(R0, R1), R1->env);
			}

			/* We now need to extend the environment with our new name */
			g_env = make_cons(make_cons(R0, R1), g_env);
			R1 = cell_unspecified;
			return;
		}
		else if(R0->car == s_setb)
		{
			/* attempt to lookup the variable we are set! to a new value */
			R2 = assoc(R0->cdr->car, g_env);
			if(nil == R2)
			{
				file_print("Assigning value to unbound variable: ", stderr);
				file_print(R0->cdr->car->string, stderr);
				file_print("\nAborting to prevent problems\n", stderr);
				exit(EXIT_FAILURE);
			}

			/* Get that new value */
			R0 = R0->cdr->cdr->car;
			eval();

			/* update that new variable with that value */
			R2->cdr = R1;
			return;
		}
		else if(R0->car == s_let)
		{
			/* Clean up locals after let completes */
			push_cell(g_env);

			/* Protect the s-expression from garbage collection */
			push_cell(R0->cdr->cdr);

			/* Deal with the (let ((pieces)) ..) */
			for(R0 = R0->cdr->car; R0 != nil; R0 = R0->cdr)
			{
				push_cell(R0);
				R0 = R0->car->cdr->car;
				eval();
				R0 = pop_cell();
				g_env = make_cons(make_cons(R0->car->car, R1), g_env);
			}

			/* Lets execute the pieces of the of (let ((..)) pieces) */
			R0 = pop_cell();
			R0 = make_cons(s_begin, R0);
			eval();

			/* Actual clean up */
			g_env = pop_cell();
			return;
		}
		else if(R0->car == s_begin)
		{
			/* Get past the begin to the list of s-expressions */
			R0 = R0->cdr;

			/* Catch a naked begin */
			R1 = NULL;

			/* Loop through s-expressions and returning the last return value */
			while(R0 != nil)
			{
				/* Protect the rest of the list */
				push_cell(R0->cdr);

				/* Evaluate current leading s-expression */
				R0 = R0->car;
				eval();

				/* Move to next s-expression*/
				R0 = pop_cell();
			}

			require(NULL != R1, "naked begin is not supported\n");
			return;
		}
		else if(R0->car == s_while)
		{
			/* Check if we should even run the while */
			push_cell(R0);
			R0 = R0->cdr->car;
			eval();
			R0 = pop_cell();

			while(cell_f != R1)
			{
				/* Perform single evalutation of the while if it exists */
				if(nil != R0->cdr->cdr)
				{
					push_cell(R0);
					R0 = R0->cdr->cdr->car;
					eval();
					R0 = pop_cell();
				}

				/* Perform another run of the conditional*/
				push_cell(R0);
				R0 = R0->cdr->car;
				eval();
				R0 = pop_cell();
			}

			/* We return the last R1 (until I find out better) */
			return;
		}

		/* Deal with case of (thing ...), so first figure out what thing is */
		push_cell(R0->cdr);
		R0 = R0->car;
		eval();
		R0 = pop_cell();

		/* Now figure out what everything else is so that it can work on it */
		push_cell(R1);
		evlis();

		/* Now apply thing to that list of values */
		R0 = pop_cell();
		apply(R0, R1);
		return;
	}

	/* Fall through case */
	R1 = R0;
}


/* Exposed primitives */
struct cell* builtin_apply(struct cell* args)
{
	require(nil != args, "apply requires arguments\n");
	require(nil != args->cdr, "apply recieved insufficient arguments\n");
	require(CONS == args->cdr->car->type, "apply did not recieve a list\n");
	push_cell(R0);
	push_cell(R1);
	push_cell(g_env);

	/* ensure preservation of s-expression during application */
	R0 = args;
	struct cell* r;
	apply(args->car, args->cdr->car);
	r = R1;
	g_env = pop_cell();
	R1 = pop_cell();
	R0 = pop_cell();
	return r;
}

struct cell* builtin_primitive_eval(struct cell* args)
{
	require(nil != args, "primitive-eval requires an argument\n");
	require(nil == args->cdr, "primitive-eval received too many arguments\n");

	push_cell(R0);
	push_cell(R1);
	push_cell(g_env);
	R0 = args->car;
	/* Need to figure out correct solution as g_env might not be correct in regards to modules */
	eval();
	struct cell* r = R1;
	g_env = pop_cell();
	R1 = pop_cell();
	R0 = pop_cell();
	return r;
}
