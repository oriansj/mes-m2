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
void eval(struct cell* exp, struct cell* env);
char* env_lookup(char* token, char** envp);
void init_sl3();
int Readline(FILE* source_file, char* temp, unsigned max_string);
struct cell* parse(char* program, int size);
void writeobj(FILE *ofp, struct cell* op);
void garbage_init(int number_of_cells);
void garbage_collect();
void reset_block(char* a);

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
	writeobj(__stdout, R0);
	if((stdout == __stdout) && (NULL != R0)) fputc('\n', __stdout);
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

	char* testing = env_lookup("MES_CORE", envp);
	if(NULL != testing)
	{
		char* name;
		/* Our most important initializations */
		memory_block = calloc(MAX_STRING, sizeof(char));
		message = calloc(MAX_STRING + 2, sizeof(char));
		garbage_init(1000000);
		init_sl3();
		g_stack = calloc(100000, sizeof(struct cell*));

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
		/* Our most important initializations */
		memory_block = calloc(MAX_STRING, sizeof(char));
		message = calloc(MAX_STRING + 2, sizeof(char));
		garbage_init(1000000);
		init_sl3();
		g_stack = calloc(100000, sizeof(struct cell));

		char* mes_boot = env_lookup("MES_BOOT", envp);
		if(NULL == mes_boot)
		{
			mes_boot = "boot-0.scm";
		}

		Reached_EOF = FALSE;
		__stdin = fopen(mes_boot, "R");
		while(!Reached_EOF)
		{
			garbage_collect();
			Reached_EOF = REPL();
		}
	}
}
