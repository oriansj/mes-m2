/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *  Copyright © 2019 Jeremiah Orians
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

#define STRING(x) g_cells[x].cdr
#define LENGTH(x) g_cells[x].car
#define CBYTES(x) (char*)&g_cells[x].cdr
#define CSTRING(x) CBYTES (STRING (x))
#define MAKE_NUMBER(n) make_cell__ (TNUMBER, 0, (long)n)
#define MAKE_CHAR(n) make_cell__ (TCHAR, 0, n)
#define TYPE(x) g_cells[x].type
#define CAR(x) g_cells[x].car
#define CDR(x) g_cells[x].cdr
#define VALUE(x) g_cells[x].cdr
#define PORT(x) g_cells[x].car
#define MAKE_STRING0(x) make_string (x, strlen (x))
#define MAKE_STRING_PORT(x) make_cell__ (TPORT, -length__ (g_ports) - 2, x)

void __ungetc_init();
// The Mes C Library defines and initializes these in crt1
int mes_open(char const *file_name, int flags, ...)
{
	va_list ap;
	va_start(ap, flags);
	int mask = va_arg(ap, int);
	__ungetc_init();
	int r = open(file_name, flags, mask);

	if(r > 2)
	{
		__ungetc_buf[r] = -1;
	}

	va_end(ap);
	return r;
}

int readchar();
int unreadchar();
SCM write_byte (SCM x);
SCM current_input_port ();
int fdgetc (int fd);
SCM make_string(char const* s, int length);
int fdungetc (int c, int fd);
SCM make_struct (SCM type, SCM fields, SCM printer);
SCM make_cell__(long type, SCM car, SCM cdr);
SCM car (SCM x);
SCM cdr (SCM x);
SCM cons (SCM x, SCM y);
long length__(SCM x);
SCM error(SCM key, SCM x);
int eputs(char const* s);
char *itoa (int number);
SCM acons (SCM key, SCM value, SCM alist);

int eputs(char const* s)
{
	int i = strlen(s);
	write(__stderr, s, i);
	return 0;
}

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

int fdputc(int c, int fd)
{
	write(fd, (char*)&c, 1);
	return 0;
}

int fdputs(char const* s, int fd)
{
	int i = strlen(s);
	write(fd, s, i);
	return 0;
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
		eputs(" ***MES C LIB*** fdungetc ungetc buffer overflow fd=");
		eputs(itoa(fd));
		eputs("\n");
		exit(1);
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

	SCM port = current_input_port();
	SCM string = STRING(port);
	size_t length = LENGTH(string);

	if(!length)
	{
		return -1;
	}

	char const *p = CSTRING(string);
	return p[0];
}

int readchar()
{
	if(__stdin >= 0)
	{
		return fdgetc(__stdin);
	}

	SCM port = current_input_port();
	SCM string = STRING(port);
	size_t length = LENGTH(string);

	if(!length)
	{
		return -1;
	}

	char const *p = CSTRING(string);
	int c = *p++;
	STRING(port) = make_string(p, length - 1);
	return c;
}

int unreadchar(int c)
{
	if(__stdin >= 0)
	{
		return fdungetc(c, __stdin);
	}

	SCM port = current_input_port();
	SCM string = STRING(port);
	size_t length = LENGTH(string);
	char *p = CSTRING(string);
	p--;
	string = make_string(p, length + 1);
	p = CSTRING(string);
	p[0] = c;
	STRING(port) = string;
	return c;
}

SCM peek_byte()
{
	return MAKE_NUMBER(peekchar());
}

SCM read_byte()
{
	return MAKE_NUMBER(readchar());
}

SCM unread_byte(SCM i)
{
	unreadchar(VALUE(i));
	return i;
}

SCM peek_char()
{
	return MAKE_CHAR(peekchar());
}

SCM read_char(SCM port)  ///((arity . n))
{
	int fd = __stdin;

	if(TYPE(port) == TPAIR && TYPE(car(port)) == TNUMBER)
	{
		__stdin = VALUE(CAR(port));
	}

	SCM c = MAKE_CHAR(readchar());
	__stdin = fd;
	return c;
}

SCM unread_char(SCM i)
{
	unreadchar(VALUE(i));
	return i;
}

SCM write_char(SCM i)  ///((arity . n))
{
	write_byte(i);
	return i;
}

SCM write_byte(SCM x)  ///((arity . n))
{
	SCM c = car(x);
	SCM p = cdr(x);
	int fd = __stdout;

	if(TYPE(p) == TPAIR && TYPE(car(p)) == TNUMBER && VALUE(CAR(p)) != 1)
	{
		fd = VALUE(CAR(p));
	}

	if(TYPE(p) == TPAIR && TYPE(car(p)) == TNUMBER && VALUE(CAR(p)) == 2)
	{
		fd = __stderr;
	}

	char cc = VALUE(c);
	write(fd, (char*)&cc, 1);
#if !__MESC__
	assert(TYPE(c) == TNUMBER || TYPE(c) == TCHAR);
#endif
	return c;
}

SCM getenv_(SCM s)  ///((name . "getenv"))
{
	char *p;
	p = getenv(CSTRING(s));
	return p ? MAKE_STRING0(p) : cell_f;
}

SCM setenv_(SCM s, SCM v)  ///((name . "setenv"))
{
	char buf[1024];
	strcpy(buf, CSTRING(s));
	setenv(buf, CSTRING(v), 1);
	return cell_unspecified;
}

SCM access_p(SCM file_name, SCM mode)
{
	return access(CSTRING(file_name), VALUE(mode)) == 0 ? cell_t : cell_f;
}

SCM current_input_port()
{
	if(__stdin >= 0)
	{
		return MAKE_NUMBER(__stdin);
	}

	SCM x = g_ports;

	while(x && PORT(CAR(x)) != __stdin)
	{
		x = CDR(x);
	}

	return CAR(x);
}

SCM open_input_file(SCM file_name)
{
	return MAKE_NUMBER(mes_open(CSTRING(file_name), O_RDONLY));
}

SCM open_input_string(SCM string)
{
	SCM port = MAKE_STRING_PORT(string);
	g_ports = cons(port, g_ports);
	return port;
}

SCM set_current_input_port(SCM port)
{
	SCM prev = current_input_port();

	if(TYPE(port) == TNUMBER)
	{
		__stdin = VALUE(port) ? VALUE(port) : STDIN;
	}
	else if(TYPE(port) == TPORT)
	{
		__stdin = PORT(port);
	}

	return prev;
}

SCM current_output_port()
{
	return MAKE_NUMBER(__stdout);
}

SCM current_error_port()
{
	return MAKE_NUMBER(__stderr);
}

SCM open_output_file(SCM x)  ///((arity . n))
{
	SCM file_name = car(x);
	x = cdr(x);
	int mode = S_IRUSR | S_IWUSR;

	if(TYPE(x) == TPAIR && TYPE(car(x)) == TNUMBER)
	{
		mode = VALUE(car(x));
	}

	return MAKE_NUMBER(mes_open(CSTRING(file_name), O_WRONLY | O_CREAT | O_TRUNC, mode));
}

SCM set_current_output_port(SCM port)
{
	__stdout = VALUE(port) ? VALUE(port) : STDOUT;
	return current_output_port();
}

SCM set_current_error_port(SCM port)
{
	__stderr = VALUE(port) ? VALUE(port) : STDERR;
	return current_error_port();
}

SCM force_output(SCM port)  ///((arity . n))
{
  	(void)port;
	return cell_unspecified;
}

SCM chmod_(SCM file_name, SCM mode)  ///((name . "chmod"))
{
	chmod(CSTRING(file_name), VALUE(mode));
	return cell_unspecified;
}

SCM isatty_p(SCM port)
{
	return isatty(VALUE(port)) ? cell_t : cell_f;
}

SCM primitive_fork()
{
	return MAKE_NUMBER(fork());
}

SCM execl_(SCM file_name, SCM args)  ///((name . "execl"))
{
	char *c_argv[1000];           // POSIX minimum 4096
	int i = 0;

	if(length__(args) > 1000)
	{
		error(cell_symbol_system_error, cons(file_name, cons(MAKE_STRING0("too many arguments"), cons(file_name, args))));
	}

	c_argv[i++] = CSTRING(file_name);

	while(args != cell_nil)
	{
		assert(TYPE(CAR(args)) == TSTRING);
		c_argv[i++] = CSTRING(CAR(args));
		args = CDR(args);

		if(g_debug > 2)
		{
			eputs("arg[");
			eputs(itoa(i));
			eputs("]: ");
			eputs(c_argv[i - 1]);
			eputs("\n");
		}
	}

	c_argv[i] = 0;
	return MAKE_NUMBER(execv(c_argv[0], c_argv));
}

SCM waitpid_(SCM pid, SCM options)
{
	int status;
	int child = waitpid(VALUE(pid), &status, VALUE(options));
	return cons(MAKE_NUMBER(child), MAKE_NUMBER(status));
}

#if __x86_64__
/* Nanoseconds on 64-bit systems with POSIX timers.  */
#define TIME_UNITS_PER_SECOND 1000000000
#else
/* Milliseconds for everyone else.  */
#define TIME_UNITS_PER_SECOND 1000
#endif

struct timespec g_start_time;
SCM init_time(SCM a)  ///((internal))
{
	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &g_start_time);
	return acons(cell_symbol_internal_time_units_per_second, MAKE_NUMBER(TIME_UNITS_PER_SECOND), a);
}

SCM current_time()
{
	return MAKE_NUMBER(time(0));
}

SCM gettimeofday_()  ///((name . "gettimeofday"))
{
	struct timeval time;
	gettimeofday(&time, 0);
	return cons(MAKE_NUMBER(time.tv_sec), MAKE_NUMBER(time.tv_usec));
}

long seconds_and_nanoseconds_to_long(long s, long ns)
{
	return s * TIME_UNITS_PER_SECOND + ns / (1000000000 / TIME_UNITS_PER_SECOND);
}

SCM get_internal_run_time()
{
	struct timespec ts;
	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);
	long time = seconds_and_nanoseconds_to_long(ts.tv_sec - g_start_time.tv_sec, ts.tv_nsec - g_start_time.tv_nsec);
	return MAKE_NUMBER(time);
}

SCM getcwd_()  ///((name . "getcwd"))
{
	char buf[PATH_MAX];
	return MAKE_STRING0(getcwd(buf, PATH_MAX));
}

SCM dup_(SCM port)  ///((name . "dup"))
{
	return MAKE_NUMBER(dup(VALUE(port)));
}

SCM dup2_(SCM old, SCM new)  ///((name . "dup2"))
{
	dup2(VALUE(old), VALUE(new));
	return cell_unspecified;
}

SCM delete_file(SCM file_name)
{
	unlink(CSTRING(file_name));
	return cell_unspecified;
}

int open_boot(char *prefix, char const *boot, char const *location)
{
	strcpy(prefix + strlen(prefix), boot);

	if(g_debug > 1)
	{
		eputs("mes: reading boot-0 [");
		eputs(location);
		eputs("]: ");
		eputs(prefix);
		eputs("\n");
	}

	int fd = mes_open(prefix, O_RDONLY);

	if(g_debug && fd > 0)
	{
		eputs("mes: read boot-0: ");
		eputs(prefix);
		eputs("\n");
	}

	return fd;
}
