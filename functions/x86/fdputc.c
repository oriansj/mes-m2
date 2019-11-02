/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2018 Jeremiah Orians <jeremiah@pdp10.guru>
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

// CONSTANT stdin 0
// CONSTANT stdout 1
// CONSTANT stderr 2
// CONSTANT EOF 0xFFFFFFFF

// CONSTANT O_RDONLY          0
// CONSTANT O_WRONLY          1
// CONSTANT O_RDWR            2
// CONSTANT O_CREAT        0x40
// CONSTANT O_EXCL         0x80
// CONSTANT O_TRUNC       0x200
// CONSTANT O_APPEND      0x400
// CONSTANT O_DIRECTORY 0x10000

// CONSTANT S_IRWXU 00700
// CONSTANT S_IXUSR 00100
// CONSTANT S_IWUSR 00200
// CONSTANT S_IRUSR 00400

// CONSTANT S_ISUID 0400
// CONSTANT S_ISGID 02000
// CONSTANT S_IXGRP 00010
// CONSTANT S_IXOTH 00001
// CONSTANT S_IRGRP 00040
// CONSTANT S_IROTH 00004
// CONSTANT S_IWGRP 00020
// CONSTANT S_IWOTH 00002
// CONSTANT S_IRWXG 00070
// CONSTANT S_IRWXO 00007

int fdputc(int c, int fd)
{
	asm("LOAD_IMMEDIATE_eax %4"
	    "LOAD_EFFECTIVE_ADDRESS_ebx %4"
	    "LOAD_INTEGER_ebx"
	    "LOAD_EFFECTIVE_ADDRESS_ecx %8"
	    "LOAD_IMMEDIATE_edx %1"
	    "INT_80");
}

int file_getc(int fd)
{
	asm("LOAD_IMMEDIATE_eax %3"
	    "LOAD_EFFECTIVE_ADDRESS_ebx %4"
	    "LOAD_INTEGER_ebx"
	    "PUSH_ebx"
	    "COPY_esp_to_ecx"
	    "LOAD_IMMEDIATE_edx %1"
	    "INT_80"
	    "TEST"
	    "POP_eax"
	    "JUMP_NE8 !FUNCTION_file_getc_Done"
	    "LOAD_IMMEDIATE_eax %-1"
	    ":FUNCTION_file_getc_Done");
}

int open(char* name, int flags, int mode)
{
	asm("LOAD_EFFECTIVE_ADDRESS_ebx %12"
	    "LOAD_INTEGER_ebx"
	    "LOAD_EFFECTIVE_ADDRESS_ecx %8"
	    "LOAD_INTEGER_ecx"
	    "LOAD_EFFECTIVE_ADDRESS_edx %4"
	    "LOAD_INTEGER_edx"
	    "LOAD_IMMEDIATE_eax %5"
	    "INT_80");
}

void chmod(char* name, int mode)
{
	asm("LOAD_EFFECTIVE_ADDRESS_ebx %8"
	    "LOAD_INTEGER_ebx"
	    "LOAD_EFFECTIVE_ADDRESS_ecx %4"
	    "LOAD_INTEGER_ecx"
	    "LOAD_IMMEDIATE_eax %15"
	    "INT_80");
}

int isatty(int fd)
{
	/* TODO */
}

int fork()
{
	asm("LOAD_IMMEDIATE_eax %2"
	    "LOAD_IMMEDIATE_ebx %0"
	    "INT_80");
}

int execve(char* name, char** argv, char** envp)
{
	asm("LOAD_EFFECTIVE_ADDRESS_ebx %12"
	    "LOAD_INTEGER_ebx"
	    "LOAD_EFFECTIVE_ADDRESS_ecx %8"
	    "LOAD_INTEGER_ecx"
	    "LOAD_EFFECTIVE_ADDRESS_edx %4"
	    "LOAD_INTEGER_edx"
	    "LOAD_IMMEDIATE_eax %11"
	    "INT_80");
}

int waitpid(int pid, int* status_ptr, int options)
{
	asm("LOAD_EFFECTIVE_ADDRESS_ebx %12"
	    "LOAD_INTEGER_ebx"
	    "LOAD_EFFECTIVE_ADDRESS_ecx %8"
	    "LOAD_INTEGER_ecx"
	    "LOAD_EFFECTIVE_ADDRESS_edx %4"
	    "LOAD_INTEGER_edx"
	    "LOAD_IMMEDIATE_eax %7"
	    "INT_80");
}

int env_update(char* key, char* value, int override)
{
	/* TODO */
	return 0;
}

int access(char* name, int mode)
{
	/* TODO */
	return 0;
}

// CONSTANT PATH_MAX 4096
char* getcwd(char *buf, int size)
{
	/* TODO */
	return buf;
}

int dup(int oldfd)
{
	/* TODO */
	return oldfd;
}

int dup2(int oldfd, int newfd)
{
	/* TODO */
	return newfd;
}

int unlink(char* pathname)
{
	/* TODO */
	return 0;
}
