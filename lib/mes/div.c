/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
 * Copyright © 2020 Nathalie Kopaczewski <natkopa@gmail.com>
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

#include <mes/lib.h>
#include <stdint.h>
#include <limits.h>
#include <signal.h>

struct ldiv_t
{
  long quot;
  long rem;
};

int __raise (int);

#if __TINYC__
#define __raise(x) -1
#endif

void
__mesabi_div0 (void)
{
  if (__raise (SIGFPE) < 0) /* could not raise SIGFPE */
    {
      /* Fail in any way possible */
      unsigned char *x = (unsigned char *) 0;
      *x = 2;
    }
}

#define ULONG_HIGHBITMASK LONG_MIN
#define ULONG_BITCOUNT (sizeof (unsigned long)*8)

/** Compute the logarithm of base 2 of D.  The result is rounded down.
    That is equal to the highest-index set bit in D.

    The idea is to shift D to the right in order to find the index i of the first most-significant digit > 0.

    The computation is done by bisection, for speed.

    Recurse:
      Two halves are determined of the remaining slice.
      The first half checked is the higher-significant half.
        If that higher-significant half is not zero, recurse on that one.
        Otherwise, recurse on the lower-significant half.

    Precondition: D > 0 */
static unsigned int
__mesabi_log2i (unsigned long D)
{
  unsigned int n = ULONG_BITCOUNT;
  unsigned int i = 0U;
  unsigned long D1;
  while (n >= 2U)
    {                           /* while still two halves possible */
      n >>= 1U;
      /* D1: higher-significant half of D */
      D1 = D >> n;
      if (D1 > 0UL)
        {
          /* We know that the resulting index has to be in the higher-significant half.
             In that case, lower-significant half of D is superfluous for determination of i,
             therefore scroll and continue with higher-significant half. */
          D = D1;
          i += n;
        }
    }
  return i;
}

#if 0
static void
test_log2i (void)
{
  assert (log2i (1) == 0);
  assert (log2i (1) == 0);
  assert (log2i (2) == 1);
  assert (log2i (3) == 1);
  assert (log2i (4) == 2);
  assert (log2i (5) == 2);
  assert (log2i (6) == 2);
  assert (log2i (7) == 2);
  assert (log2i (8) == 3);
  assert (log2i (9) == 3);
  assert (log2i (10) == 3);
  assert (log2i (11) == 3);
  assert (log2i (12) == 3);
  assert (log2i (13) == 3);
  assert (log2i (71) == 6);
  assert (log2i (72) == 6);
  assert (log2i (73) == 6);
  assert (log2i (74) == 6);
  assert (log2i (75) == 6);
  assert (log2i (99) == 6);
  assert (log2i (2147483648) == 31);
  assert (log2i (3221225471) == 31);
  assert (log2i (4294967294) == 31);
  assert (log2i (4294967295) == 31);
}
#endif

/** Perform unsigned integer division of N by D; store the remainder
    into *REMAINDER; return the quotient.

    This is currently implemented as long division.

    R is the remainder.  R >= 0.  R starts at N.

    QUOTIENT is built up bit by bit starting at the most significant bit [*].

    Values D', starting at D << ULONG_BITCOUNT [*], going down to 1,
    divided by 2 each time, are iterated over, doing: If R >= D',
    subtract D' from R, and append new LSB 1 to the QUOTIENT.
    Otherwise, subtract 0 from R (implicit), and append new LSB 0 to the
    QUOTIENT (0 is the implicit default).

    [*] As a special consideration for C throwing away bits when
        left-shifting, D' starts at the highest value that will not lose
        bits in this way instead.  (ULONG_BITCOUNT - log2i(D) - 1) is
        the number of leading zeroes in D in binary radix.

    Precondition: D > 0 */
static unsigned long
__mesabi_uldiv1 (unsigned long N, unsigned long D, unsigned long *remainder)
{
  // Note: __mesabi_log2i(D) < ULONG_BITCOUNT
  unsigned int j = ULONG_BITCOUNT - __mesabi_log2i (D); /* Note: Or j = __mesabi_log2i(N) + 1 - __mesabi_log2i(D) */
  // Note: assert(j - 1 == __builtin_clzl(D));  on GCC
  unsigned long quotient = 0UL;
  unsigned long R = N;
  for (D <<= (j - 1); j > 0U; --j, D >>= 1U)
    {
      quotient <<= 1U;
      if (R >= D)
        {
          R -= D;
          quotient |= 1UL;
        }
    }
  *remainder = R;
  return quotient;
}

#if 0
static void
assert_uldiv (unsigned long N, unsigned long D, unsigned long expected_quotient,
              unsigned long expected_remainder)
{
  unsigned long remainder;
  unsigned long quotient;
  quotient = uldiv (N, D, &remainder);
  printf ("%lu/%lu = %lu;%lu\n", N, D, quotient, remainder);
  assert (quotient == expected_quotient);
  assert (remainder == expected_remainder);
}

static void
test_uldiv (void)
{
  //assert_uldiv(0, 0, 0, 0);
  assert_uldiv (0, 1, 0, 0);
  assert_uldiv (1, 1, 1, 0);
  assert_uldiv (72, 5, 14, 2);
  assert_uldiv (0xffffffff, 1, 0xffffffff, 0);
  assert_uldiv (0xffffffff, 2, 0x7fffffff, 1);
}
#endif

/* Compare gcc: __udivmoddi4 */
unsigned long
__mesabi_uldiv (unsigned long a, unsigned long b, unsigned long *remainder)
{
  unsigned long tmp;
  if (!remainder)
    remainder = &tmp;
  *remainder = 0;
  switch (b)
    {
    case 64UL:
      *remainder = a & 63UL;
      return a >> 6UL;
    case 32UL:
      *remainder = a & 31UL;
      return a >> 5UL;
    case 16UL:
      *remainder = a & 15UL;
      return a >> 4UL;
    case 8UL:
      *remainder = a & 7UL;
      return a >> 3UL;
    case 4UL:
      *remainder = a & 3UL;
      return a >> 2UL;
    case 2UL:
      *remainder = a & 1UL;
      return a >> 1UL;
    case 1UL:
      *remainder = 0;
      return a;
    case 0UL:
      __mesabi_div0 ();
      return 0UL;
    default:
      return __mesabi_uldiv1 (a, b, remainder);
    }
}

/* Note: Rounds towards zero.
   Maintainer: Be careful to satisfy quot * b + rem == a.
               That means that rem can be negative. */
void
__mesabi_ldiv (long a, long b, struct ldiv_t *result)
{
  int negate_result = (a < 0) ^ (b < 0);
  if (b == LONG_MIN)
    __mesabi_div0 ();
  if (a != LONG_MIN)
    {
      int negative_a = (a < 0);
      if (negative_a)
        a = -a;
      if (b < 0)
        b = -b;
      result->quot = __mesabi_uldiv (a, b, &result->rem);
      if (negate_result)
        result->quot = -result->quot;
      if (negative_a)
        result->rem = -result->rem;
    }
  else
    {
      result->rem = 0;
      if (b < 0)
        b = -b;
      if (b == 1)
        {
          result->quot = a;
          /* Since result->quot is already negative, don't negate it again. */
          negate_result = !negate_result;
        }
      else if (b == 0)
        __mesabi_div0 ();
      else
        {
          long x;
          for (x = 0; a <= -b; a += b)
            ++x;
          result->rem = a;      /* negative */
          result->quot = x;
        }
      if (negate_result)
        result->quot = -result->quot;
    }
}

long
__mesabi_imod (long a, long b)
{
  struct ldiv_t result;
  __mesabi_ldiv (a, b, &result);
  return result.rem;
}

long
__mesabi_idiv (long a, long b)
{
  struct ldiv_t result;
  __mesabi_ldiv (a, b, &result);
  return result.quot;
}

unsigned long
__mesabi_umod (unsigned long a, unsigned long b)
{
  unsigned long result;
  __mesabi_uldiv (a, b, &result);
  return result;
}

unsigned long
__mesabi_udiv (unsigned long a, unsigned long b)
{
  unsigned long result;
  return __mesabi_uldiv (a, b, &result);
}
