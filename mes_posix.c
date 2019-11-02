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

#include <fcntl.h>

#include "mes.h"
#include "mes_constants.h"

#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>


SCM length__(struct scm* x);
int file_getc(int fd);
int match(char* a, char* b);
struct scm* error_(struct scm* key, struct scm* x);
struct scm* make_char(SCM c);
struct scm* make_number_(SCM n);
struct scm* make_port(SCM n, struct scm* s);
struct scm* make_string(char* s, int length);
struct scm* make_string_(char* s);
struct scm* make_tpair(struct scm* a, struct scm* b);
void raw_print(char* s, int fd);
void require(int bool, char* error);

int eputs(char* s)
{
	raw_print(s, __stderr);
	return 0;
}

char* ntoab(SCM x, int base, int signed_p)
{
	char* p = itoa_buf + 11;
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

char* itoa(int x)
{
	return ntoab(x, 10, 1);
}

void __ungetc_init()
{
	if(__ungetc_buf[RLIMIT_NOFILE] == 0)
	{
		memset(__ungetc_buf, -1, (RLIMIT_NOFILE + 1)*sizeof(int));
	}
}

int fdgetc(int fd)
{
	__ungetc_init();
	int i = __ungetc_buf[fd];

	if(i >= 0)
	{
		__ungetc_buf[fd] = -1;
	}
	else
	{
		i = file_getc(fd);
	}

	return i;
}

void fd_print(char* s, int f);
int fdungetc(int c, int fd)
{
	__ungetc_init();

	if(c == -1)
	{
		return c;
	}
	else if(__ungetc_buf[fd] != -1)
	{
		eputs(" ***MES C LIB*** fdungetc ungetc buffer overflow fd=");
		exit(EXIT_FAILURE);
	}

	__ungetc_buf[fd] = c;
	return c;
}

int _fdungetc_p(int fd)
{
	return __ungetc_buf[fd] >= 0;
}

struct scm* current_input_port_();
int readchar();
int unreadchar(int c);
int peekchar()
{
	if(__stdin >= 0)
	{
		int c = readchar();
		unreadchar(c);
		return c;
	}

	struct scm* port = current_input_port_();
	struct scm* string = port->cdr;
	SCM length = string->length;

	if(!length)
	{
		return -1;
	}

	char* p = string->cdr->string;
	return p[0];
}

int readchar()
{
	if(__stdin >= 0)
	{
		return fdgetc(__stdin);
	}

	struct scm* port = current_input_port_();
	struct scm* string = port->cdr;
	SCM length = string->length;

	if(!length)
	{
		return -1;
	}

	char* p = string->cdr->string;
	int c = p[0];
	p = p + 1;
	port->cdr = make_string(p, length - 1);
	return c;
}

int unreadchar(int c)
{
	if(__stdin >= 0)
	{
		return fdungetc(c, __stdin);
	}

	struct scm* port = current_input_port_();
	struct scm* string = port->cdr;
	SCM length = string->length;
	char* p = string->cdr->string;
	p = p - 1;
	string = make_string(p, length + 1);
	p = string->cdr->string;
	p[0] = c;
	port->cdr = string;
	return c;
}

struct scm* peek_byte(struct scm* x) /* External */
{
	require(cell_nil == x, "mes_posix.c: peek_byte recieved arguments\n");
	return make_number_( peekchar());
}

struct scm* read_byte(struct scm* x) /* External */
{
	require(cell_nil == x, "mes_posix.c: read_byte recieved arguments\n");
	return make_number_(readchar());
}

struct scm* unread_byte(struct scm* x) /* External */
{
	x = x->car;
	unreadchar(x->value);
	return x;
}

struct scm* peek_char(struct scm* x) /* External */
{
	require(cell_nil == x, "mes_posix.c: peek_char recieved arguments\n");
	return make_char(peekchar());
}

struct scm* read_char(struct scm* x)  /* External */
{
	int fd = __stdin;
	struct scm* p = x->car;

	if(p->type == TPAIR && p->car->type == TNUMBER)
	{
		__stdin = p->car->value;
	}

	struct scm* c = make_char(readchar());
	__stdin = fd;
	return c;
}

struct scm* unread_char(struct scm* x) /* External */
{
	x = x->car;
	unreadchar(x->value);
	return x;
}

char* prematch(char* search, char* field)
{
	do
	{
		if(search[0] != field[0]) return NULL;
		search = search + 1;
		field = field + 1;
	} while(0 != search[0]);
	return field;
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

struct scm* get_env(struct scm* x)  /* External */
{
	x = x->car;
	char* p = x->cdr->string;
	char* pass = env_lookup(p, global_envp);
	if(NULL == pass) return cell_f;
	return make_string_(pass);
}

int env_update(char* key, char* value, int override);
struct scm* set_env(struct scm* x)  /* External */
{
	struct scm* s = x->car;
	struct scm* v = x->cdr->car;
	char* p1 = s->cdr->string;
	char* p2 = v->cdr->string;
	env_update(p1, p2, FALSE);
	return cell_unspecified;
}

struct scm* access_p(struct scm* x) /* External */
{
	struct scm* file_name = x->car;
	struct scm* mode = x->cdr->car;
	char* p = file_name->cdr->string;
	if( 0 == access(p, mode->value)) return cell_t;

	return cell_f;
}

struct scm* current_input_port_() /* Internal*/
{
	if(__stdin >= 0)
	{
		return make_number_( __stdin);
	}

	struct scm* x = g_ports;

	while(x->car->port != __stdin)
	{
		x = x->cdr;
	}

	return x->car;
}

struct scm* current_input_port(struct scm* x) /* External */
{
	require(cell_nil == x, "mes_posix.c: current_input_port recieved arguments\n");
	return current_input_port_();
}

/* The Mes C Library defines and initializes these in crt1 */
SCM mes_open(char* file_name, int flags, int mode)
{
	__ungetc_init();
	int r = open(file_name, flags, mode);

	if(r > 2)
	{
		__ungetc_buf[r] = -1;
	}

	return r;
}

struct scm* open_input_file_(struct scm* file_name)
{
	struct scm* f = file_name;
	char* p = f->cdr->string;
	return make_number_( mes_open(p, O_RDONLY, 0));
}

struct scm* open_input_file(struct scm* x) /* External */
{
	return open_input_file_(x->car);
}

struct scm* open_input_string(struct scm* x) /* External */
{
	struct scm* string = x->car;
	struct scm* port = make_port( -length__(g_ports) - 2, string);
	g_ports = make_tpair(port, g_ports);
	return port;
}

struct scm* set_current_input_port_(struct scm* port) /* Internal */
{
	port = port->car;
	struct scm* prev = current_input_port_();
	struct scm* x = port;

	if(x->type == TNUMBER)
	{
		if(TRUE == x->value) __stdin = x->value;
		else __stdin = STDIN;
	}
	else if(x->type == TPORT)
	{
		__stdin = x->rac;
	}

	return prev;
}

struct scm* set_current_input_port(struct scm* x) /* External */
{
	return set_current_input_port_(x->car);
}

struct scm* current_output_port(struct scm* x) /* External */
{
	require(cell_nil == x, "mes_posix.c: current_output_port recieved arguments\n");
	return make_number_( __stdout);
}

struct scm* current_error_port(struct scm* x) /* External */
{
	require(cell_nil == x, "mes_posix.c: current_error_port recieved arguments\n");
	return make_number_( __stderr);
}

struct scm* open_output_file(struct scm* x)  /* External */
{
	struct scm* f = x->car;
	x = x->cdr;
	int mode = S_IRUSR | S_IWUSR;

	if(x->type == TPAIR && f->type == TNUMBER)
	{
		mode = f->value;
	}

	char* p = f->cdr->string;
	SCM fl = mes_open(p, O_WRONLY | O_CREAT | O_TRUNC, mode);
	struct scm* handle = make_number_(fl);
	return handle;
}

struct scm* set_current_output_port(struct scm* x) /* External */
{
	__stdout = x->car->value;
	return make_number_( __stdout);
}

struct scm* set_current_error_port(struct scm* x) /* External */
{
	__stderr = x->car->value;
	return make_number_( __stderr);
}

struct scm* file_chmod(struct scm* x)  /* External */
{
	struct scm* file_name = x->car;
	struct scm* mode = x->cdr->car;
	char* p = file_name->cdr->string;
	chmod(p, mode->value);
	return cell_unspecified;
}

struct scm* isatty_p(struct scm* x) /* External */
{
	if(isatty(x->car->value)) return cell_t;
	return cell_f;
}

struct scm* primitive_fork(struct scm* x) /* External*/
{
	require(cell_nil == x, "mes_posix.c: primitive_fork recieved arguments\n");
	return make_number_(fork());
}

void require(int bool, char* error)
{
	if(!bool)
	{
		eputs(error);
		exit(EXIT_FAILURE);
	}
}

struct scm* exec(struct scm* x)  /* External */
{
	struct scm* file_name = x->car;
	struct scm* args = x->cdr->car;
	struct scm* f = file_name;
	struct scm* a = args;
	char** c_argv = execl_argv;           /* POSIX minimum 4096 */
	int i = 0;

	if(length__(args) > 1000)
	{
		error_(cell_symbol_system_error, make_tpair(file_name, make_tpair(make_string_("too many arguments"), make_tpair(file_name, args))));
	}

	char* p = f->cdr->string;
	c_argv[i] = p;
	i = i + 1;

	while(a != cell_nil)
	{
		require(a->car->type == TSTRING, "mes_posix.c: execl_ a->car is not of type TSTRING\n");
		p = a->car->cdr->string;
		c_argv[i] = p;
		i = i + 1;
		a = a->cdr;

		if(g_debug > 2)
		{
			eputs("arg[");
			eputs(ntoab(i, 10, TRUE));
			eputs("]: ");
			eputs(c_argv[i - 1]);
			eputs(";\n");
		}
	}

	c_argv[i] = 0;
	return make_number_(execve(c_argv[0], c_argv, global_envp));
}

struct scm* wait_pid(struct scm* x) /* External */
{
	struct scm* pid = x->car;
	struct scm* options = x->cdr->car;
	struct scm* p = pid;
	struct scm* o = options;
	int status;
	int child = waitpid(p->value, &status, o->value);
	return make_tpair(make_number_( child), make_number_( status));
}

struct scm* get_cwd(struct scm* x)  /* External */
{
	require(cell_nil == x, "mes_posix.c: get_cwd recieved arguments\n");
	char* buf = cwd_buf;
	return make_string_(getcwd(buf, PATH_MAX));
}

struct scm* file_dup(struct scm* x)  /* External */
{
	struct scm* p = x->car;
	return make_number_(dup(p->value));
}

struct scm* file_dup2(struct scm* x)  /* External */
{
	struct scm* old = x->car;
	struct scm* new = x->cdr->car;
	struct scm* o = old;
	struct scm* n = new;
	dup2(o->value, n->value);
	return cell_unspecified;
}

struct scm* delete_file(struct scm* x) /* External */
{
	struct scm* f = x->car;
	char* p = f->cdr->string;
	unlink(p);
	return cell_unspecified;
}

int open_boot(char* boot)
{
	if(g_debug > 1)
	{
		eputs("mes: reading boot-0 [");
		eputs(boot);
		eputs("]\n");
	}

	int fd = mes_open(boot, O_RDONLY, 0);

	if(__stdin < 0)
	{
		eputs("mes: no such file: ");
		eputs(boot);
		eputs("\n");
		exit(EXIT_FAILURE);
	}

	if(g_debug && fd > 0)
	{
		eputs("mes: read boot-0: ");
		eputs(boot);
		eputs("\n");
	}

	return fd;
}
