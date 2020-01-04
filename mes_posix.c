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
int string_size(char* a);
struct cell* make_file(FILE* a, char* name);
struct cell* make_string(char* a, int length);
struct cell* prim_display(struct cell* args, struct cell* out);
struct cell* prim_write(struct cell* args, struct cell* out);

char* ntoab(SCM x, int base, int signed_p)
{
	char* p = calloc(12, sizeof(char));
	p = p + 11;
	p[1] = 0;
	int sign_p = 0;
	SCM u = x;

	if(signed_p && x < 0)
	{
		sign_p = 1;
		u = -x;
	}

	do
	{
		SCM i = u % base;
		if(i > 9)
		{
			p[0] = 'a' + i - 10;
		}
		else
		{
			p[0] = '0' + i;
		}
		p = p - 1;
		u = u / base;
	} while(0 != u);

	if(sign_p && p[1] != '0')
	{
		p[0] = '-';
		p = p - 1;
	}

	return p + 1;
}

struct cell* builtin_display(struct cell* args)
{
	require(nil != args, "display requires arguments\n");
	if(nil == args->cdr)
	{
		prim_display(args, __stdout);
		return cell_unspecified;
	}

	require(FILE_PORT == args->cdr->car->type, "You passed something that isn't a file pointer to write in position 2\n");

	prim_display(args, args->cdr->car);
	return cell_unspecified;
}

struct cell* builtin_display_error(struct cell* args)
{
	require(nil != args, "display-error requires arguments\n");
	if(nil == args->cdr)
	{
		prim_display(args, __stderr);
		return cell_unspecified;
	}

	require(FILE_PORT == args->cdr->car->type, "You passed something that isn't a file pointer to write in position 2\n");
	prim_display(args, args->cdr->car);
	return cell_unspecified;
}

struct cell* builtin_write(struct cell* args)
{
	require(nil != args, "write requires arguments\n");
	if(nil == args->cdr)
	{
		prim_write(args, __stdout);
		return cell_unspecified;
	}
	require(FILE_PORT == args->cdr->car->type, "You passed something that isn't a file pointer to write in position 2\n");

	prim_write(args, args->cdr->car);
	return cell_unspecified;
}

struct cell* builtin_write_error(struct cell* args)
{
	require(nil != args, "write-error requires arguments\n");
	if(nil == args->cdr)
	{
		return prim_write(args, __stderr);
	}

	require(FILE_PORT == args->cdr->car->type, "You passed something that isn't a file pointer to write in position 2\n");
	return prim_write(args, args->cdr->car);
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

struct cell* builtin_open(struct cell* args, char* mode)
{
	require(nil != args, "Did not recieve a file name\n");
	require(STRING == args->car->type, "File name must be a string\n");

	return make_file(open_file(args->car->string, mode), args->car->string);
}


struct cell* builtin_open_read(struct cell* args)
{
	return builtin_open(args, "r");
}

struct cell* builtin_open_write(struct cell* args)
{
	return builtin_open(args, "w");
}

struct cell* builtin_set_current_output_port(struct cell* args)
{
	require(nil != args, "set-current-output-port requires arguments\n");
	require(FILE_PORT == args->car->type, "set-current-output-port expects a port\n");
	require(nil == args->cdr, "set-current-output-port expects only a single argument\n");

	__stdout->file = args->car->file;
	__stdout->string = args->car->string;
	return cell_unspecified;
}

struct cell* builtin_set_current_input_port(struct cell* args)
{
	require(nil != args, "set-current-input-port requires arguments\n");
	require(FILE_PORT == args->car->type, "set-current-input-port expects a port\n");
	require(nil == args->cdr, "set-current-input-port expects only a single argument\n");

	__stdin->file = args->car->file;
	__stdin->string = args->car->string;
	return cell_unspecified;
}

struct cell* builtin_set_current_error_port(struct cell* args)
{
	require(nil != args, "set-current-error-port requires arguments\n");
	require(FILE_PORT == args->car->type, "set-current-error-port expects a port\n");
	require(nil == args->cdr, "set-current-error-port expects only a single argument\n");

	__stderr->file = args->car->file;
	__stderr->string = args->car->string;
	return cell_unspecified;
}

struct cell* builtin_current_input_port(struct cell* args)
{
	require(nil == args, "current-input-port does not accept arguments\n");
	return __stdin;
}

struct cell* builtin_current_output_port(struct cell* args)
{
	require(nil == args, "current-output-port does not accept arguments\n");
	return __stdout;
}

struct cell* builtin_current_error_port(struct cell* args)
{
	require(nil == args, "current-error-port does not accept arguments\n");
	return __stderr;
}

struct cell* builtin_ttyname(struct cell* args)
{
	require(nil != args, "ttyname requires an argument\n");
	require(nil == args->cdr, "ttyname only accepts a single argument\n");
	require(FILE_PORT == args->car->type, "ttyname only accepts ports\n");
	return make_string(args->car->string, string_size(args->car->string));
}

struct cell* builtin_command_line(struct cell* args)
{
	require(nil == args, "command-line does not accept arguments\n");
	struct cell* r = nil;
	int i = __argc - 1;
	while(0 <= i)
	{
		r = make_cons(make_string(__argv[i], string_size(__argv[i])), r);
		i = i - 1;
	}

	return r;
}

char* prematch(char* search, char* field)
{
	do
	{
		if(search[0] != field[0]) return NULL;
		search = search + 1;
		field = field + 1;
	} while(0 != search[0]);
	return field+1;
}

char* env_lookup(char* token, char** envp)
{
	if(NULL == envp) return NULL;
	int i = 0;
	char* ret = NULL;
	do
	{
		ret = prematch(token, envp[i]);
		if(NULL != ret) return ret;
		i = i + 1;
	} while(NULL != envp[i]);
	return NULL;
}

struct cell* builtin_get_env(struct cell* args)
{
	require(nil != args, "getenv requires an argument\n");
	require(nil == args->cdr, "getenv requires only a single argument\n");
	require(STRING == args->car->type, "getenv requires a string\n");

	char* pass = env_lookup(args->car->string, __envp);
	if(NULL == pass) return cell_f;
	return make_string(pass, string_size(pass));
}
