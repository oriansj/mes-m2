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
SCM make_cell__(SCM type, SCM car, SCM cdr);
SCM cons_(SCM x, SCM y);
long length__(SCM x);
SCM error(SCM key, SCM x);
SCM acons_(SCM key, SCM value, SCM alist);

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
		*p-- = i > 9 ? 'a' + i - 10 : '0' + i;
		u = u / base;
	} while(u);

	if(sign_p && *(p + 1) != '0')
	{
		*p-- = '-';
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

	struct scm* port = bad2good(current_input_port());
	struct scm* string = bad2good(port->cdr);
	size_t length = string->length;

	if(!length)
	{
		return -1;
	}

	char const *p = (char*)&bad2good(string->cdr)->rdc;
	return p[0];
}

int readchar()
{
	if(__stdin >= 0)
	{
		return fdgetc(__stdin);
	}

	struct scm* port = bad2good(current_input_port());
	struct scm* string = bad2good(port->cdr);
	size_t length = string->length;

	if(!length)
	{
		return -1;
	}

	char const *p = (char*)&bad2good(string->cdr)->rdc;
	int c = p[0];
	p = p + 1;
	port->rdc = GetSCM2(bad2good(make_string(p, length - 1)));
	return c;
}

int unreadchar(int c)
{
	if(__stdin >= 0)
	{
		return fdungetc(c, __stdin);
	}

	struct scm* port = bad2good(current_input_port());
	struct scm* string = bad2good(port->cdr);
	size_t length = string->length;
	char *p = (char*)&bad2good(string->cdr)->rdc;
	p = p - 1;
	string = bad2good(make_string(p, length + 1));
	p = (char*)&bad2good(string->cdr)->rdc;
	p[0] = c;
	port->cdr = good2bad(string);
	return c;
}

struct scm* peek_byte()
{
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, peekchar())));
}

struct scm* read_byte()
{
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0,readchar())));
}

struct scm* unread_byte(SCM i)
{
	struct scm* x = Getstructscm2(i);
	unreadchar(x->value);
	return good2bad(x);
}

struct scm* peek_char()
{
	return good2bad(Getstructscm2(make_cell__ (TCHAR, 0, peekchar())));
}

struct scm* read_char(SCM port)  ///((arity . n))
{
	int fd = __stdin;
	struct scm* p = Getstructscm2(port);

	if(p->type == TPAIR && bad2good(p->car)->type == TNUMBER)
	{
		__stdin = bad2good(p->car)->value;
	}

	struct scm* c = Getstructscm2(make_cell__ (TCHAR, 0, readchar()));
	__stdin = fd;
	return good2bad(c);
}

struct scm* unread_char(SCM i)
{
	struct scm* x = Getstructscm2(i);
	unreadchar(x->value);
	return good2bad(x);
}

struct scm* getenv_(SCM s)  ///((name . "getenv"))
{
	struct scm* x = Getstructscm2(s);
	char *p = getenv((char*)&bad2good(x->cdr)->rdc);
	return p ? good2bad(make_string_(p)) : good2bad(Getstructscm2(cell_f));
}

struct scm* setenv_(SCM s, SCM v)  ///((name . "setenv"))
{
	struct scm* a = Getstructscm2(s);
	struct scm* b = Getstructscm2(v);
	setenv((char*)&bad2good(a->cdr)->rdc, (char*)&bad2good(b->cdr)->rdc, 1);
	return good2bad(Getstructscm2(cell_unspecified));
}

struct scm* access_p(SCM file_name, SCM mode)
{
	struct scm* f = Getstructscm2(file_name);
	struct scm* m = Getstructscm2(mode);
	return access((char*)&bad2good(f->cdr)->rdc, m->value) == 0 ? good2bad(Getstructscm2(cell_t)) : good2bad(Getstructscm2(cell_f));
}

struct scm* current_input_port()
{
	if(__stdin >= 0)
	{
		return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, __stdin)));
	}

	struct scm* x = Getstructscm2(g_ports);

	while(bad2good(x->car)->port != __stdin)
	{
		x = bad2good(x->cdr);
	}

	return x->car;
}

// The Mes C Library defines and initializes these in crt1
int mes_open(char const *file_name, int flags, int mode)
{
	__ungetc_init();
	int r = open(file_name, flags, mode);

	if(r > 2)
	{
		__ungetc_buf[r] = -1;
	}

	return r;
}

struct scm* open_input_file(SCM file_name)
{
	struct scm* f = Getstructscm2(file_name);
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, mes_open((char*)&bad2good(f->cdr)->rdc, O_RDONLY, 0))));
}

struct scm* open_input_string(SCM string)
{
	struct scm* port = Getstructscm2(make_cell__ (TPORT, -length__ (g_ports) - 2, string));
	g_ports = cons_(GetSCM2(port), g_ports);
	return good2bad(port);
}

struct scm* set_current_input_port(SCM port)
{
	struct scm* prev = current_input_port();
	struct scm* x = Getstructscm2(port);

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
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, __stdout)));
}

struct scm* current_error_port()
{
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, __stderr)));
}

struct scm* open_output_file(SCM x)  ///((arity . n))
{
	struct scm* y = Getstructscm2(x);
	struct scm* f = bad2good(y->car);
	y = bad2good(y->cdr);
	int mode = S_IRUSR | S_IWUSR;

	if(y->type == TPAIR && f->type == TNUMBER)
	{
		mode = f->value;
	}

	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, mes_open((char*)&bad2good(f->cdr)->rdc, O_WRONLY | O_CREAT | O_TRUNC, mode))));
}

struct scm* set_current_output_port(SCM port)
{
	struct scm* p  = Getstructscm2(port);
	__stdout = p->value ? p->value : STDOUT;
	return current_output_port();
}

struct scm* set_current_error_port(SCM port)
{
	struct scm* p = Getstructscm2(port);
	__stderr = p->value ? p->value : STDERR;
	return current_error_port();
}

struct scm* chmod_(SCM file_name, SCM mode)  ///((name . "chmod"))
{
	struct scm* f = Getstructscm2(file_name);
	struct scm* m = Getstructscm2(mode);
	chmod((char*)&bad2good(f->cdr)->rdc, m->value);
	return good2bad(Getstructscm2(cell_unspecified));
}

struct scm*  isatty_p(SCM port)
{
	struct scm* p = Getstructscm2(port);
	return isatty(p->value) ? good2bad(Getstructscm2(cell_t)) : good2bad(Getstructscm2(cell_f));
}

struct scm* primitive_fork()
{
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, fork())));
}

struct scm* execl_(SCM file_name, SCM args)  ///((name . "execl"))
{
	struct scm* f = Getstructscm2(file_name);
	struct scm* a = Getstructscm2(args);
	char *c_argv[1000];           // POSIX minimum 4096
	int i = 0;

	if(length__(args) > 1000)
	{
		error(cell_symbol_system_error, cons_(file_name, cons_(GetSCM2(make_string_("too many arguments")), cons_(file_name, args))));
	}

	c_argv[i] = (char*)&bad2good(f->cdr)->rdc;
	i = i + 1;

	while(GetSCM2(a) != cell_nil)
	{
		struct scm* aa = bad2good(a->car);
		assert(aa->type == TSTRING);
		c_argv[i] = (char*)&bad2good(aa->cdr)->rdc;
		i = i + 1;
		a = bad2good(a->cdr);

		if(g_debug > 2)
		{
			fprintf(stderr, "arg[%d]: %s\n", i, c_argv[i - 1]);
		}
	}

	c_argv[i] = 0;
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, execv(c_argv[0], c_argv))));
}

struct scm* waitpid_(SCM pid, SCM options)
{
	struct scm* p = Getstructscm2(pid);
	struct scm* o = Getstructscm2(options);
	int status;
	int child = waitpid(p->value, &status, o->value);
	return good2bad(Getstructscm2(cons_(make_cell__ (TNUMBER, 0, child), make_cell__ (TNUMBER, 0, status))));
}

#if __x86_64__
/* Nanoseconds on 64-bit systems with POSIX timers.  */
#define TIME_UNITS_PER_SECOND 1000000000
#else
/* Milliseconds for everyone else.  */
#define TIME_UNITS_PER_SECOND 1000
#endif

struct timespec g_start_time;
struct scm* init_time(SCM a)  ///((internal))
{
	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &g_start_time);
	return good2bad(Getstructscm2(acons_(cell_symbol_internal_time_units_per_second, make_cell__ (TNUMBER, 0, TIME_UNITS_PER_SECOND), a)));
}

struct scm* current_time()
{
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, time(0))));
}

struct scm* gettimeofday_()  ///((name . "gettimeofday"))
{
	struct timeval time;
	gettimeofday(&time, 0);
	return good2bad(Getstructscm2(cons_(make_cell__ (TNUMBER, 0, time.tv_sec), make_cell__ (TNUMBER, 0, time.tv_usec))));
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
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, time)));
}

struct scm* getcwd_()  ///((name . "getcwd"))
{
	char buf[PATH_MAX];
	return good2bad(make_string_(getcwd(buf, PATH_MAX)));
}

struct scm* dup_(SCM port)  ///((name . "dup"))
{
	struct scm* p = Getstructscm2(port);
	return good2bad(Getstructscm2(make_cell__ (TNUMBER, 0, dup(p->value))));
}

struct scm* dup2_(SCM old, SCM new)  ///((name . "dup2"))
{
	struct scm* o = Getstructscm2(old);
	struct scm* n = Getstructscm2(new);
	dup2(o->value, n->value);
	return good2bad(Getstructscm2(cell_unspecified));
}

struct scm* delete_file(SCM file_name)
{
	struct scm* f = Getstructscm2(file_name);
	unlink((char*)&bad2good(f->cdr)->rdc);
	return good2bad(Getstructscm2(cell_unspecified));
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
