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
	asm("LOAD_IMMEDIATE_rax %1"
	    "LOAD_EFFECTIVE_ADDRESS_rdi %8"
	    "LOAD_INTEGER_rdi"
	    "LOAD_EFFECTIVE_ADDRESS_rsi %16"
	    "LOAD_IMMEDIATE_rdx %1"
	    "SYSCALL");
}

int file_getc(int fd)
{
	asm("LOAD_EFFECTIVE_ADDRESS_rdi %8"
	    "LOAD_INTEGER_rdi"
	    "LOAD_IMMEDIATE_rax %0"
	    "PUSH_RAX"
	    "LOAD_EFFECTIVE_ADDRESS_rsi %0"
	    "LOAD_IMMEDIATE_rdx %1"
	    "SYSCALL"
	    "LOAD_IMMEDIATE_rbx %0"
	    "CMP"
	    "POP_RAX"
	    "JUMP_NE %FUNCTION_file_getc_Done"
	    "LOAD_IMMEDIATE_rax %-1"
	    ":FUNCTION_file_getc_Done");
}

int file_open(char* name, int flags, int mode)
{
	asm("LOAD_EFFECTIVE_ADDRESS_rdi %24"
	    "LOAD_INTEGER_rdi"
	    "LOAD_EFFECTIVE_ADDRESS_rsi %16"
	    "LOAD_INTEGER_rsi"
	    "LOAD_EFFECTIVE_ADDRESS_rdx %8"
	    "LOAD_INTEGER_rdx"
	    "LOAD_IMMEDIATE_rax %2"
	    "SYSCALL");
}

void file_chmod(char* name, int mode)
{
	asm("LOAD_EFFECTIVE_ADDRESS_rdi %16"
	    "LOAD_INTEGER_rdi"
	    "LOAD_EFFECTIVE_ADDRESS_rsi %8"
	    "LOAD_INTEGER_rsi"
	    "LOAD_IMMEDIATE_rax %90"
	    "SYSCALL");
}

int tty_detect(int fd)
{
	/* TODO */
}

int fork_process()
{
	asm("LOAD_IMMEDIATE_rax %57"
	    "LOAD_IMMEDIATE_rdi %0"
	    "SYSCALL");
}

int file_execute(char* name, char** argv, char** envp)
{
	asm("LOAD_EFFECTIVE_ADDRESS_rdi %24"
	    "LOAD_INTEGER_rdi"
	    "LOAD_EFFECTIVE_ADDRESS_rsi %16"
	    "LOAD_INTEGER_rsi"
	    "LOAD_EFFECTIVE_ADDRESS_rdx %8"
	    "LOAD_INTEGER_rdx"
	    "LOAD_IMMEDIATE_rax %59"
	    "SYSCALL");
}

int wait_exit(int pid, int* status_ptr, int options)
{
	/* Uses wait4 with struct rusage *ru set to NULL */
	asm("LOAD_EFFECTIVE_ADDRESS_rdi %24"
	    "LOAD_INTEGER_rdi"
	    "LOAD_EFFECTIVE_ADDRESS_rsi %16"
	    "LOAD_INTEGER_rsi"
	    "LOAD_EFFECTIVE_ADDRESS_rdx %8"
	    "LOAD_INTEGER_rdx"
	    "LOAD_IMMEDIATE_r10 %0"
	    "LOAD_IMMEDIATE_rax %61"
	    "SYSCALL");
}
