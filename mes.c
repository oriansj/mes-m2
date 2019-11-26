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
// CONSTANT MAX_STRING 4096
#define MAX_STRING 4096

char* message;

/* Prototypes */
char* env_lookup(char* token, char** envp);
char* string_append(char* a, char* b);
int Readline(FILE* source_file, char* temp, unsigned max_string);
struct cell* parse(char* program, int size);
void eval(struct cell* exp, struct cell* env);
void garbage_collect();
void garbage_init(int number_of_cells);
void init_sl3();
void reset_block(char* a);
void writeobj(FILE *ofp, struct cell* op);

/* Deal with common errors */
void require(int bool, char* error)
{
	if(!bool)
	{
		file_print(error, stderr);
		exit(EXIT_FAILURE);
	}
}

/* Read Eval Print Loop*/
int REPL()
{
	int read;
	/* Read S-Expression block */
	reset_block(message);
	read = Readline(__stdin, message, MAX_STRING);
	if(0 == read) return TRUE;

	/* Process S-expression */
	R0 = parse(message, read);
	/* TODO add macro processing here */
	g_env = top_env;
	eval(R0, g_env);

	/* Print */
	if((stdout == __stdout) && (NULL != R0))
	{
		writeobj(__stdout, R0);
		fputc('\n', __stdout);
	}
	return FALSE;
}

FILE* open_file(char* name, char* mode)
{
	FILE* f = fopen(name, mode);

	if(NULL == f)
	{
		file_print("Unable to open file ", stderr);
		file_print(name, stderr);
		if('r' == mode[0])
		{
			file_print(" for reading\n", stderr);
		}
		else if('w' == mode[0])
		{
			file_print(" for writing\n", stderr);
		}
		else
		{
			file_print(" with unknown mode\n", stderr);
		}
		exit(EXIT_FAILURE);
	}

	return f;
}


int main(int argc, char **argv, char** envp)
{
	stack_pointer = 0;
	__stdin = stdin;
	__stdout = stdout;
	__stderr = stderr;
	int Reached_EOF;

	int arena = numerate_string(env_lookup("MES_ARENA", envp));
	if(0 == arena) arena = 1000000;

	int stack = numerate_string(env_lookup("MES_STACK", envp));
	if(0 == stack) stack = 100000;

	/* Our most important initializations */
	memory_block = calloc(MAX_STRING, sizeof(char));
	message = calloc(MAX_STRING + 2, sizeof(char));
	garbage_init(arena);
	init_sl3();
	g_stack = calloc(stack, sizeof(struct cell*));

	char* testing = env_lookup("MES_CORE", envp);
	if(NULL != testing)
	{
		char* name;

		int i = 1;
		while(i <= argc)
		{
			if(NULL == argv[i])
			{
				i = i + 1;
			}
			else if(match(argv[i], "--boot"))
			{
				Reached_EOF = FALSE;
				name = argv[i + 1];
				__stdin = fopen(name, "r");
				while(!Reached_EOF)
				{
					garbage_collect();
					Reached_EOF = REPL();
				}
				i = i + 2;
			}
			else if(match(argv[i], "-f") || match(argv[i], "--file"))
			{
				Reached_EOF = FALSE;
				name = argv[i + 1];
				__stdin = fopen(name, "r");
				while(!Reached_EOF)
				{
					garbage_collect();
					Reached_EOF = REPL();
				}
				i = i + 2;
			}
			else
			{
				file_print("Received unknown option: ", stderr);
				file_print(argv[i], stderr);
				file_print("\nAborting\n", stderr);
				exit(EXIT_FAILURE);
			}
		}

		Reached_EOF = FALSE;
		__stdin = stdin;
		__stdout = stdout;
		while(!Reached_EOF)
		{
			garbage_collect();
			Reached_EOF = REPL();
		}
		fclose(__stdout);
		return 0;
	}
	else
	{
		char* mes_boot = env_lookup("MES_BOOT", envp);
		if(NULL == mes_boot)
		{
			mes_boot = "boot-0.scm";
		}

		char* mes_path = env_lookup("MES_PREFIX", envp);
		if(NULL == mes_path) mes_path = ".";
		mes_path = string_append(mes_path, "/module/mes/");

		char* boot = string_append(mes_path, mes_boot);
		Reached_EOF = FALSE;
		__stdin = fopen(boot, "r");
		while((NULL != __stdin) && (!Reached_EOF))
		{
			garbage_collect();
			Reached_EOF = REPL();
		}

		file_print("mes: boot failed: no such file: ", stderr);
		file_print(boot, stderr);
		file_print("\nThis is occuring because this branch isn't ready yet\nrun: export MES_CORE=0\nTo disable this cuurently broken code\n", stderr);
		exit(EXIT_FAILURE);
	}
}
