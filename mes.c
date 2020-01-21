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

/* globals used in REPL */
char* message;

/* Prototypes */
FILE* open_file(char* name, char* mode);
char* env_lookup(char* token, char** envp);
char* string_append(char* a, char* b);
int Readline(FILE* source_file, char* temp);
struct cell* expand_macros(struct cell* exps);
struct cell* make_file(FILE* a, char* name);
struct cell* parse(char* program, int size);
struct cell* pop_cell();
void eval();
void garbage_init();
void init_sl3();
void push_cell(struct cell* a);
void reset_block(char* a);
void writeobj(struct cell* output_file, struct cell* op, int write_p);

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
	read = Readline(__stdin->file, message);
	if(0 == read) return TRUE;

	/* Process S-expression */
	R0 = parse(message, read);

	/* perform macro processing here */
	R0 = expand_macros(R0);
	/* now to eval what results */
	eval();

	/* Print */
	if(match("/dev/stdin", __stdin->string) && (NULL != R1) && (cell_unspecified != R1))
	{
		file_print("$R0 = ", __stdout->file);
		writeobj(__stdout, R1, TRUE);
		fputc('\n', __stdout->file);
	}

	/* Display user friendly prompt */
	if(match("/dev/stdin", __stdin->string))
	{
		file_print("REPL: ", __stdout->file);
	}
	return FALSE;
}

/* The foundational loader */
struct cell* load_file(char* s)
{
	int Reached_EOF = FALSE;
	FILE* f = fopen(s, "r");

	/* Punt on bad inputs */
	if(NULL == f) return cell_unspecified;

	push_cell(__stdin);
	__stdin = make_file(f, s);
	while(!Reached_EOF)
	{
		garbage_collect();
		Reached_EOF = REPL();
	}
	__stdin = pop_cell();
	return cell_t;
}

int main(int argc, char **argv, char** envp)
{
	__envp = envp;
	__argv = argv;
	__argc = argc;
	stack_pointer = 0;

	arena = numerate_string(env_lookup("MES_ARENA", envp));
	if(0 == arena) arena = 1;

	mes_debug_level = numerate_string(env_lookup("MES_DEBUG", envp));

	max_arena = numerate_string(env_lookup("MES_MAX_ARENA", envp));
	if(0 == max_arena) max_arena = 50000000;

	MAX_STRING = numerate_string(env_lookup("MES_MAX_STRING", envp));
	if(0 == MAX_STRING) MAX_STRING = 4096;

	MAX_TOKEN = numerate_string(env_lookup("MES_MAX_TOKEN", envp));
	if(0 == MAX_TOKEN) MAX_TOKEN = 1024;

	GC_SAFETY = numerate_string(env_lookup("MES_SAFETY", envp));

	int stack = numerate_string(env_lookup("MES_STACK", envp));
	if(0 == stack) stack = 100000;

	/* Our most important initializations */
	memory_block = calloc(MAX_TOKEN + 8, sizeof(char));
	message = calloc(MAX_STRING + 8, sizeof(char));
	garbage_init();
	init_sl3();
	g_stack = calloc(stack, sizeof(struct cell*));

	/* Initialization: stdin, stdout and stderr */
	__stdin = make_file(stdin, "/dev/stdin");
	__stdout = make_file(stdout, "/dev/stdout");
	__stderr = make_file(stderr, "/dev/stderr");

	char* testing = env_lookup("MES_CORE", envp);
	if(NULL != testing)
	{
		int i = 1;
		while(i <= argc)
		{
			if(NULL == argv[i])
			{
				i = i + 1;
			}
			else if(match(argv[i], "--boot"))
			{
				load_file(argv[i + 1]);
				i = i + 2;
			}
			else if(match(argv[i], "-f") || match(argv[i], "--file"))
			{
				load_file(argv[i + 1]);
				i = i + 2;
			}
			else if(match(argv[i], "-h") || match(argv[i], "--help"))
			{
				file_print("Usage: ", stdout);
				file_print(argv[0], stdout);
				file_print(" [--boot boot.scm] [-f|--file file.scm] [-h|--help]\n", stdout);
				i = i + 1;
				exit(EXIT_SUCCESS);
			}
			else
			{
				file_print("Received unknown option: ", stderr);
				file_print(argv[i], stderr);
				file_print("\nUsage: ", stderr);
				file_print(argv[0], stderr);
				file_print(" [--boot boot.scm] [-f|--file file.scm] [-h|--help]\nAborting\n", stderr);
				exit(EXIT_FAILURE);
			}
		}

		file_print("REPL: ", __stdout->file);
		load_file("/dev/stdin");
		file_print("\nexiting, have a nice day!\n", __stdout->file);
		exit(EXIT_SUCCESS);
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
		load_file(boot);

		file_print("mes: boot failed: no such file: ", stderr);
		file_print(boot, stderr);
		file_print("\nIf you prefer not to load a bootfile\nrun: export MES_CORE=0\n", stderr);
		exit(EXIT_FAILURE);
	}
}
