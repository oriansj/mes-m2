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
#include <limits.h>
#include <stdarg.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#include "mes.h"
#include "mes_constants.h"

int readchar();
int unreadchar();
struct scm* current_input_port();
int fdgetc(int fd);
int eputs(char const* s);
struct scm* make_string(char const* s, int length);
struct scm* make_string_(char const* s);
int fdungetc(int c, int fd);
struct scm* cons(struct scm* x, struct scm* y);
SCM length__(struct scm* x);
struct scm* error(struct scm* key, struct scm* x);
struct scm* acons(struct scm* key, struct scm* value, struct scm* alist);

struct scm* make_number(SCM n);
struct scm* make_port(SCM n, struct scm* s);
struct scm* make_char(SCM c);
int match(char* a, char* b);

char* ntoab(long x, int base, int signed_p)
{
	static char itoa_buf[20];
	char *p = itoa_buf + 11;
	*p-- = 0;
	int sign_p = 0;
	unsigned long u = x;

	if(signed_p && x < 0)
	{
		sign_p = 1;
		u = -x;
	}

	do
	{
		long i = u % base;
		if(i > 9)
		{
			*p = 'a' + i - 10;
		}
		else
		{
			*p = '0' + i;
		}
		p = p - 1;
		u = u / base;
	} while(u);

	if(sign_p && *(p + 1) != '0')
	{
		*p = '-';
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
	char c;
	int i = __ungetc_buf[fd];

	if(i >= 0)
	{
		__ungetc_buf[fd] = -1;
	}
	else
	{
		int r = read(fd, &c, 1);

		if(r < 1)
		{
			return -1;
		}

		i = c;
	}

	if(i < 0)
	{
		i += 256;
	}

	return i;
}

int fdungetc(int c, int fd)
{
	__ungetc_init();

	if(c == -1)
	{
		return c;
	}
	else if(__ungetc_buf[fd] != -1)
	{
		fprintf(stderr, " ***MES C LIB*** fdungetc ungetc buffer overflow fd=%d\n", fd);
		exit(EXIT_FAILURE);
	}

	__ungetc_buf[fd] = c;
	return c;
}

int _fdungetc_p(int fd)
{
	return __ungetc_buf[fd] >= 0;
}

int peekchar()
{
	if(__stdin >= 0)
	{
		int c = readchar();
		unreadchar(c);
		return c;
	}

	struct scm* port = current_input_port();
	struct scm* string = port->cdr;
	size_t length = string->length;

	if(!length)
	{
		return -1;
	}

	char *p = string->cdr->string;
	return p[0];
}

int readchar()
{
	if(__stdin >= 0)
	{
		return fdgetc(__stdin);
	}

	struct scm* port = current_input_port();
	struct scm* string = port->cdr;
	size_t length = string->length;

	if(!length)
	{
		return -1;
	}

	char *p = string->cdr->string;
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

	struct scm* port = current_input_port();
	struct scm* string = port->cdr;
	size_t length = string->length;
	char *p = string->cdr->string;
	p = p - 1;
	string = make_string(p, length + 1);
	p = string->cdr->string;
	p[0] = c;
	port->cdr = string;
	return c;
}

struct scm* peek_byte()
{
	return make_number( peekchar());
}

struct scm* read_byte()
{
	return make_number(readchar());
}

struct scm* unread_byte(struct scm* i)
{
	struct scm* x = i;
	unreadchar(x->value);
	return x;
}

struct scm* peek_char()
{
	return make_char(peekchar());
}

struct scm* read_char(struct scm* port)  ///((arity . n))
{
	int fd = __stdin;
	struct scm* p = port;

	if(p->type == TPAIR && p->car->type == TNUMBER)
	{
		__stdin = p->car->value;
	}

	struct scm* c = make_char(readchar());
	__stdin = fd;
	return c;
}

struct scm* unread_char(struct scm* i)
{
	struct scm* x = i;
	unreadchar(x->value);
	return x;
}

char* env_lookup(char* token, char** envp)
{
	if(NULL == envp) return NULL;
	int i = 0;
	char* ret = NULL;
	do
	{
		if(match(token, envp[i])) ret = envp[i];
		if(NULL != ret) return ret;
		i = i + 1;
	} while(NULL != envp[i]);
	return NULL;
}

struct scm* getenv_(struct scm* s)  ///((name . "getenv"))
{
	struct scm* x = s;
	char* p = x->cdr->string;
	char *pass = env_lookup(p, global_envp);
	if(NULL == pass) return cell_f;
	return make_string_(pass);
}

struct scm* setenv_(struct scm* s, struct scm* v)  ///((name . "setenv"))
{
	struct scm* a = s;
	struct scm* b = v;
	char* p1 = a->cdr->string;
	char* p2 = b->cdr->string;
	setenv(p1, p2, 1);
	return cell_unspecified;
}

struct scm* access_p(struct scm* file_name, struct scm* mode)
{
	struct scm* f = file_name;
	struct scm* m = mode;
	char* p = f->cdr->string;
	return access(p, m->value) == 0 ? cell_t : cell_f;
}

struct scm* current_input_port()
{
	if(__stdin >= 0)
	{
		return make_number( __stdin);
	}

	struct scm* x = g_ports;

	while(x->car->port != __stdin)
	{
		x = x->cdr;
	}

	return x->car;
}

// The Mes C Library defines and initializes these in crt1
SCM mes_open(char const *file_name, int flags, int mode)
{
	__ungetc_init();
	int r = open(file_name, flags, mode);

	if(r > 2)
	{
		__ungetc_buf[r] = -1;
	}

	return r;
}

struct scm* open_input_file(struct scm* file_name)
{
	struct scm* f = file_name;
	char* p = f->cdr->string;
	return make_number( mes_open(p, O_RDONLY, 0));
}

struct scm* open_input_string(struct scm* string)
{
	struct scm* port = make_port( -length__ (g_ports) - 2, string);
	g_ports = cons(port, g_ports);
	return port;
}

struct scm* set_current_input_port(struct scm* port)
{
	struct scm* prev = current_input_port();
	struct scm* x = port;

	if(x->type == TNUMBER)
	{
		__stdin = x->value ? x->value : STDIN;
	}
	else if(x->type == TPORT)
	{
		__stdin = x->rac;
	}

	return prev;
}

struct scm* current_output_port()
{
	return make_number( __stdout);
}

struct scm* current_error_port()
{
	return make_number( __stderr);
}

struct scm* open_output_file(struct scm* x)  ///((arity . n))
{
	struct scm* y = x;
	struct scm* f = y->car;
	y = y->cdr;
	int mode = S_IRUSR | S_IWUSR;

	if(y->type == TPAIR && f->type == TNUMBER)
	{
		mode = f->value;
	}

	char* p = f->cdr->string;
	SCM fl = mes_open(p, O_WRONLY | O_CREAT | O_TRUNC, mode);
	struct scm* handle = make_number(fl);
	return handle;
}

struct scm* set_current_output_port(struct scm* port)
{
	struct scm* p  = port;
	__stdout = p->value ? p->value : STDOUT;
	return current_output_port();
}

struct scm* set_current_error_port(struct scm* port)
{
	struct scm* p = port;
	__stderr = p->value ? p->value : STDERR;
	return current_error_port();
}

struct scm* chmod_(struct scm* file_name, struct scm* mode)  ///((name . "chmod"))
{
	struct scm* f = file_name;
	struct scm* m = mode;
	char* p = f->cdr->string;
	chmod(p, m->value);
	return cell_unspecified;
}

struct scm*  isatty_p(struct scm* port)
{
	struct scm* p = port;
	return isatty(p->value) ? cell_t : cell_f;
}

struct scm* primitive_fork()
{
	return make_number( fork());
}

void require(int bool, char* error)
{
	if(!bool)
	{
		fprintf(stderr, "%s", error);
		exit(EXIT_FAILURE);
	}
}

struct scm* execl_(struct scm* file_name, struct scm* args)  ///((name . "execl"))
{
	struct scm* f = file_name;
	struct scm* a = args;
	char *c_argv[1000];           // POSIX minimum 4096
	int i = 0;

	if(length__(args) > 1000)
	{
		error(cell_symbol_system_error, cons(file_name, cons(make_string_("too many arguments"), cons(file_name, args))));
	}

	char* p = f->cdr->string;
	c_argv[i] = p;
	i = i + 1;

	while(a != cell_nil)
	{
		struct scm* aa = a->car;
		assert(aa->type == TSTRING);
		p = aa->cdr->string;
		c_argv[i] = p;
		i = i + 1;
		a = a->cdr;

		if(g_debug > 2)
		{
			fprintf(stderr, "arg[%d]: %s\n", i, c_argv[i - 1]);
		}
	}

	c_argv[i] = 0;
	return make_number( execv(c_argv[0], c_argv));
}

struct scm* waitpid_(struct scm* pid, struct scm* options)
{
	struct scm* p = pid;
	struct scm* o = options;
	int status;
	int child = waitpid(p->value, &status, o->value);
	return cons(make_number( child), make_number( status));
}

#if __x86_64__
/* Nanoseconds on 64-bit systems with POSIX timers.  */
#define TIME_UNITS_PER_SECOND 1000000000
#else
/* Milliseconds for everyone else.  */
#define TIME_UNITS_PER_SECOND 1000
#endif

struct timespec g_start_time;
struct scm* init_time(struct scm* a)  ///((internal))
{
	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &g_start_time);
	return acons(cell_symbol_internal_time_units_per_second, make_number( TIME_UNITS_PER_SECOND), a);
}

struct scm* current_time()
{
	return make_number( time(0));
}

struct scm* gettimeofday_()  ///((name . "gettimeofday"))
{
	struct timeval time;
	gettimeofday(&time, 0);
	return cons(make_number( time.tv_sec), make_number( time.tv_usec));
}

long seconds_and_nanoseconds_to_long(long s, long ns)
{
	return s * TIME_UNITS_PER_SECOND + ns / (1000000000 / TIME_UNITS_PER_SECOND);
}

struct scm* get_internal_run_time()
{
	struct timespec ts;
	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);
	long time = seconds_and_nanoseconds_to_long(ts.tv_sec - g_start_time.tv_sec, ts.tv_nsec - g_start_time.tv_nsec);
	return make_number( time);
}

struct scm* getcwd_()  ///((name . "getcwd"))
{
	char buf[PATH_MAX];
	return make_string_(getcwd(buf, PATH_MAX));
}

struct scm* dup_(struct scm* port)  ///((name . "dup"))
{
	struct scm* p = port;
	return make_number( dup(p->value));
}

struct scm* dup2_(struct scm* old, struct scm* new)  ///((name . "dup2"))
{
	struct scm* o = old;
	struct scm* n = new;
	dup2(o->value, n->value);
	return cell_unspecified;
}

struct scm* delete_file(struct scm* file_name)
{
	struct scm* f = file_name;
	char* p = f->cdr->string;
	unlink(p);
	return cell_unspecified;
}

int open_boot(char *boot)
{
	if(g_debug > 1)
	{
		eputs("mes: reading boot-0 [");
		eputs(boot);
		eputs("]\n");
	}

	int fd = (int)mes_open(boot, O_RDONLY, 0);

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
