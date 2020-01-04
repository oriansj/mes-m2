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

struct cell* token_stack;

char* copy_string(char* target, char* source);
struct cell* findsym(char *name);
struct cell* make_char(int a);
struct cell* make_keyword(char* name);
struct cell* make_string(char* a);
struct cell* make_sym(char* name);
void reset_block(char* a);

/****************************************************************
 *      "Convert a string into a list of tokens."               *
 ****************************************************************/
struct cell* tokenize(struct cell* head, char* fullstring, int size)
{
	int string_index = 0;
	int done = FALSE;
	if((0 >= size) || (0 == fullstring[0]))
	{
		return head;
	}

	reset_block(memory_block);

	int escape;
	int c;
	do
	{
		c = fullstring[string_index];
		if(string_index > size)
		{
			done = TRUE;
		}
		else if('\"' == c)
		{
			escape = FALSE;
			do
			{
				if(!escape && '\\' == c ) escape = TRUE;
				else escape = FALSE;
				memory_block[string_index] = c;
				string_index = string_index + 1;
				c = fullstring[string_index];
			} while(escape || ('\"' != fullstring[string_index]));
			string_index = string_index + 1;
			done = TRUE;
		}
		else
		{
			if((' ' == c) || ('\t' == c) || ('\n' == c) | ('\r' == c))
			{
				string_index = string_index + 1;
				done = TRUE;
			}
			else
			{
				memory_block[string_index] = c;
				string_index = string_index + 1;
			}
		}
	} while(!done);

	if(string_index > 1)
	{
		char* store = calloc(string_index + 1, sizeof(char));
		copy_string(store, memory_block);
		struct cell* temp = make_sym(store);
		temp->cdr = head;
		head = temp;
	}

	head = tokenize(head, (fullstring+string_index), (size - string_index));
	return head;
}


int is_integer(char* a)
{
	int i = numerate_string(a);
	if(0 != i) return TRUE;
	if(match("0", a)) return TRUE;
	if(match("-0", a)) return TRUE;
	return FALSE;
}


char special_lookup(char* s)
{
	if (match(s, "\\nul")) return '\0';
	else if (match(s, "\\alarm")) return '\a';
	else if (match(s, "\\backspace")) return '\b';
	else if (match(s, "\\tab")) return '\t';
	else if (match(s, "\\newline")) return '\n';
	else if (match(s, "\\vtab")) return '\v';
	else if (match(s, "\\page")) return '\f';
	else if (match(s, "\\return")) return '\r';
	else if (match(s, "\\space")) return ' ';
	return s[1];
}

struct cell* readlist();
struct cell* readobj();
struct cell* list_to_vector(struct cell* args);
struct cell* reader_read_hash(struct cell* a)
{
	/* Support #\char*/
	if('\\' == a->string[1])
	{
		return make_char(special_lookup(a->string + 1));
	}

	/* Support #(1 2 3) vectors */
	if('(' == a->string[1])
	{
		return list_to_vector(readlist());
	}

	/* Support #x0123456789ABCDEF hex*/
	if('x' == a->string[1])
	{
		a->string[0] = '0';
		a->type = INT;
		a->value = numerate_string(a->string);
		return a;
	}

	/* Support #o01234567 Octals */
	if('o' == a->string[1])
	{
		a->string = a->string + 1;
		a->string[0] = '0';
		a->type = INT;
		a->value = numerate_string(a->string);
		return a;
	}

	/* Support standard true and false */
	if(match("#t", a->string)) return cell_t;
	if(match("#f", a->string)) return cell_f;

	/* Support #:keywords */
	if(':' == a->string[1])
	{
		return make_keyword(a->string);
	}

	file_print("Unknown hash provided: ", stderr);
	file_print(a->string, stderr);
	exit(EXIT_FAILURE);
}


/********************************************************************
 *     Numbers become numbers                                       *
 *     Strings become strings                                       *
 *     Functions become functions                                   *
 *     quoted things become quoted                                  *
 *     Everything is treated like a symbol                          *
 ********************************************************************/
struct cell* atom(struct cell* a)
{
	/* Check for quote */
	if(match("'", a->string))
	{
		return make_cons(quote, make_cons(readobj(), nil));
	}

	/* Check for quasiquote */
	if(match("`", a->string))
	{
		return make_cons(quasiquote, make_cons(readobj(), nil));
	}

	/* Check for unquote */
	if(match(",", a->string))
	{
		return make_cons(unquote, make_cons(readobj(), nil));
	}

	/* Check for unquote-splicing */
	if(match(",@", a->string))
	{
		return make_cons(unquote_splicing, make_cons(readobj(), nil));
	}

	/* Check for strings */
	if('\"' == a->string[0])
	{
		return make_string(a->string + 1);
	}

	/* Check for specials*/
	if('#' == a->string[0])
	{
		return reader_read_hash(a);
	}

	/* Check for integer */
	if(is_integer(a->string))
	{
		a->type = INT;
		a->value = numerate_string(a->string);
		return a;
	}

	/* Check for functions */
	struct cell* op = findsym(a->string);
	if(nil != op)
	{
		return op->car;
	}

	/* Assume new symbol */
	all_symbols = make_cons(a, all_symbols);
	return a;
}

/****************************************************************
 *     "Read an expression from a sequence of tokens."          *
 ****************************************************************/
struct cell* readobj()
{
	struct cell* head = token_stack;
	token_stack = head->cdr;
	head->cdr = NULL;
	if (match("(", head->string))
	{
		return readlist();
	}

	return atom(head);
}

struct cell* readlist()
{
	struct cell* head = token_stack;
	if (match(")", head->string))
	{
		token_stack = head->cdr;
		return nil;
	}

	struct cell* tmp = readobj();
	return make_cons(tmp,readlist());
}

/****************************************************
 *     Put list of tokens in correct order          *
 ****************************************************/
struct cell* reverse_list(struct cell* head)
{
	struct cell* root = NULL;
	while(NULL != head)
	{
		struct cell* next = head->cdr;
		head->cdr = root;
		root = head;
		head = next;
	}
	return root;
}

/****************************************************
 *       "Read a S-expression from a string."       *
 ****************************************************/
struct cell* parse(char* program, int size)
{
	token_stack = tokenize(NULL, program, size);
	if(NULL == token_stack)
	{
		return nil;
	}
	token_stack = reverse_list(token_stack);
	return readobj();
}
