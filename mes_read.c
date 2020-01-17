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

/****************************************
 * Deal with terriable inputs           *
 ****************************************/
int scrub_byte(FILE* source_file)
{
	int c = fgetc(source_file);
	require(0 != c, "mes-m2 does not support null characters as input\n");
	require(127 > c, "mes-m2 does not support utf-8 at this time\nplease restrict yourself to 7bit ascii\n");
	return c;
}

/****************************************************
 * Clear out everything between #!..!# and #|..|#   *
 ****************************************************/
void reader_read_block_comment(FILE* source_file, int match)
{
	int last = 0;
	int current = fgetc(source_file);
	while((match != last) || ('#' != current))
	{
		require(EOF != current, "Unterminated block comment found\n");
		last = current;
		current = fgetc(source_file);
	}
}

/****************************************************
 * Deal with #:foo and #;( foo ..) S-Expressions    *
 ****************************************************/
void reader_s_expression_dump(FILE* source_file)
{
	int c = scrub_byte(source_file);
	unsigned depth = 0;
	while(TRUE)
	{
		require(EOF != c, "#; s-expression not bounded\n");
		if((-1 == c) || (4 == c))
		{
			return;
		}
		else if((0 == depth) && (('\n'== c) || ('\r' == c) || (' ' == c) || ('\t' == c)))
		{
			return;
		}
		else if('(' == c)
		{
			depth = depth + 1;
		}
		else if(')' == c)
		{
			depth = depth - 1;
			if(0 == depth) return;
		}
		c = scrub_byte(source_file);
	}
}


/****************************************************
 * Do the heavy lifting of reading an s-expression  *
 ****************************************************/
unsigned Readline(FILE* source_file, char* temp)
{
	int c;
	unsigned i = 0;
	unsigned depth = 0;
	int hashed = FALSE;
	int escape = FALSE;

	while(TRUE)
	{
restart_comment:
		c = scrub_byte(source_file);
restart_paren:

		require(i < MAX_STRING, "s-expression exceeds max size\nExpand MES_MAX_STRING value to resolve\n");
		if((EOF == c) || (4 == c))
		{
			require(0 == depth, "Unmatched s-expression\n");
			return i;
		}
		else if('#' == c)
		{
			hashed = TRUE;
			goto restart_comment;
		}
		else if (hashed && (('!' == c) || ('|' == c)))
		{
			reader_read_block_comment(source_file, c);
			hashed = FALSE;
			goto restart_comment;
		}
		else if(('\'' == c) || ('`' == c))
		{
			temp[i] = c;
			temp[i+1] = ' ';
			i = i + 1;
		}
		else if(',' == c)
		{
			c = scrub_byte(source_file);
			temp[i] = ',';
			i = i + 1;
			if('@' == c)
			{
				temp[i] = '@';
				i = i + 1;
				c = ' ';
			}
			temp[i] = ' ';
			i = i + 1;
			goto restart_paren;
		}
		else if(hashed && (';' == c))
		{
			reader_s_expression_dump(source_file);
			hashed = FALSE;
			c = ' ';
			goto restart_paren;
		}
		else if(hashed && ('\\' == c))
		{
			temp[i] = '#';
			temp[i+1] = '\\';
			c = scrub_byte(source_file);
			i = i + 2;
			hashed = FALSE;
			if('"' == c)
			{
				temp[i] = '"';
				temp[i+1] = ' ';
				i = i + 2;
				goto restart_comment;
			}
			goto restart_paren;
		}
		else if(';' == c)
		{
			/* drop everything until we hit newline */
			while('\n' != c)
			{
				c = fgetc(source_file);
				require(EOF != c, "recieved EOF in line comment, please add newline at end of file\n");
			}
			goto restart_comment;
		}
		else if('"' == c)
		{ /* Deal with strings */
			do
			{
				if(!escape && '\\' == c ) escape = TRUE;
				else escape = FALSE;
				temp[i] = c;
				i = i + 1;
				c = scrub_byte(source_file);
				require(EOF != c, "s-expression string does not have matching \"\n");
				require(i < MAX_STRING, "s-expression string exceeded limit of s-expression\nExpand MES_MAX_STRING value to resolve\n");
			} while('"' != c || escape);
			temp[i] = c;
		}
		else if((0 == depth) && (('\n' == c) || ('\r' == c) || (' ' == c) || ('\t' == c)))
		{
			goto Line_complete;
		}
		else if(('(' == c) || (')' == c))
		{
			if('(' == c)
			{
				depth = depth + 1;
			}

			if(')' == c)
			{
				depth = depth - 1;
			}

			if(hashed)
			{
				temp[i] = '#';
				temp[i+1] = c;
				temp[i+2] = ' ';
				i = i + 2;
				hashed = FALSE;
			}
			else
			{
				temp[i] = ' ';
				temp[i+1] = c;
				temp[i+2] = ' ';
				i = i + 2;
			}

			c = ' ';
			goto restart_paren;
		}
		else if(hashed)
		{
			temp[i] = '#';
			temp[i+1] = c;
			i = i + 1;
			hashed = FALSE;
		}
		else
		{
			temp[i] = c;
		}

		i = i + 1;
	}

Line_complete:
	if(0 == i)
	{
		return Readline(source_file, temp);
	}

	return i;
}
