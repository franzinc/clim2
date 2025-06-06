/*
 */
/*
 * Copyright Kyoto University Research Institute for Mathematical Sciences
 *                 1987, 1988, 1989, 1990, 1991, 1992
 * Copyright OMRON Corporation. 1987, 1988, 1989, 1990, 1991, 1992
 * Copyright ASTEC, Inc. 1987, 1988, 1989, 1990, 1991, 1992
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that all of the following conditions are satisfied:
 *
 * 1) The above copyright notices appear in all copies
 * 2) Both those copyright notices and this permission notice appear
 *    in supporting documentation
 * 3) The name of "Wnn" isn't changed unless substantial modifications
 *    are made, or
 * 3') Following words followed by the above copyright notices appear
 *    in all supporting documentation of software based on "Wnn":
 *
 *   "This software is based on the original version of Wnn developed by
 *    Kyoto University Research Institute for Mathematical Sciences (KURIMS),
 *    OMRON Corporation and ASTEC Inc."
 *
 * 4) The names KURIMS, OMRON and ASTEC not be used in advertising or
 *    publicity pertaining to distribution of the software without
 *    specific, written prior permission
 *
 * KURIMS, OMRON and ASTEC make no representations about the suitability
 * of this software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 * Wnn consortium is one of distributors of the official Wnn source code
 * release.  Wnn consortium also makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * KURIMS, OMRON, ASTEC AND WNN CONSORTIUM DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL KURIMS, OMRON, ASTEC OR
 * WNN CONSORTIUM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 */
/*	Version 4.0
 */
/*
	long strings library. Copyed from GMW programs!!
*/
#include <stdio.h>
#include "commonhd.h"
#include "wnn_string.h"

#undef NULL
#define NULL	0

#define is_half_width(c)	(! is_full_width(c))
#define is_full_width(c)	(((c) & 0xff00))
#define is_gaiji(c)		(is_full_width(c) && ! ((c) & 0x8080))
#define is_jis_kanji(c)		(((c) & 0x8080) == 0x8080)

w_char *wnn_Strcat(s1,s2)
register w_char *s1;
register w_char *s2;
{
	register w_char *d;

	for (d = s1;*d++ != 0;);
	for (--d;(*d++ = *s2++) != 0;);
	return s1;
}

w_char *wnn_Strncat(s1,s2,n)
register w_char *s1;
register w_char *s2;
register int n;
{
	register w_char *d;

	for (d = s1;*d++ != 0;);
	for (--d;n > 0 && (*d++ = *s2++) != 0;n--);
	if (n <= 0) {
		*d = 0;
		return d;
	} else {
		return --d;
	}
}

int wnn_Strcmp(s1,s2)
register w_char *s1;
register w_char *s2;
{
  for (;*s1 != 0 && *s1 == *s2;s1++, s2++);
  if(*s1 > *s2) return 1;
  if(*s1 == *s2) return 0;
  return(-1);
/*  return (int)(*s1 - *s2);  Since w_char, it is always positive */
}

/* s1 is substr of s2?  then return 1*/
int wnn_Substr(s1, s2)
register w_char *s1;
register w_char *s2;
{
  for (;*s1 != 0 && *s1 == *s2;s1++, s2++);
  return (int)(!*s1);
}



int wnn_Strncmp(s1,s2,n)
register w_char *s1;
register w_char *s2;
register int n;
{
  if(n == 0)return(0);
  for (;n > 0 && *s1++ == *s2++;n--);
  return (int)(*--s1 - *--s2);
}

w_char *wnn_Strcpy(s1,s2)
register w_char *s1;
register w_char *s2;
{
	register w_char *d;

	for (d = s1;(*d++ = *s2++) != 0;);
	return s1;
}

w_char *wnn_Strncpy(s1,s2,n)
register w_char *s1;
register w_char *s2;
register int n;
{
/*
	register w_char *d;

	for (d = s1;n > 0;n--) {
		*d++ = *s2++;
	}
	return s1;
*/
    if (s2 > s1) {
	for ( ; n-- > 0; ) {
	    *s1++ = *s2++;
	}
    } else if (s2 < s1) {
	s1 += n - 1;
	s2 += n - 1;
	for ( ; n-- > 0; ) {
	    *s1-- = *s2--;
	}
    }
    return s1;
}

int wnn_Strlen(s)
register w_char *s;
{
	register int n;

	for (n = 0;*s++ != 0;n++);
	return n;
}

void
wnn_Sreverse(d, s)
w_char *d, *s;
{
  w_char *s0;

  s0 = s;
  for(;*s++;);
  s--;
  for(;--s >= s0;){
    *d++ = *s;
  }
  *d = 0;
}

#ifdef nodef
w_char *wnn_Index(s,c)
register w_char *s;
register w_char c;
{
	while (*s != c) {
		if (*s++ == 0) return 0;
	}
	return s;
}

w_char *wnn_Rindex(s,c)
register w_char *s;
register w_char c;
{
	register w_char *p = 0;

	while (*s != 0) {
		if (*s++ == c) p = s-1;
	}
	return p;
}

w_char *wnn_Strpbrk(s1,s2)
register w_char *s1;
register w_char *s2;
{
	register w_char *p;

	while (*s1 != 0) {
		for (p = s2;*p != 0;p++) {
			if (*s1++ == *p) return s1-1;
		}
	}
	return 0;
}

int wnn_Strspn(s1,s2)
register w_char *s1;
register w_char *s2;
{
	register w_char *p;
	register int n;

	while (*s1 != 0) {
		for (p = s2;*p != 0;p++) {
			if (*s1++ == *p) {
				n = 1;
				while (*s1 != 0) {
					for (p = s2;*p != 0;p++) {
						if (*s1++ == *p) {
							n++;
							break;
						}
					}
					if (*p == 0) {
						return n;
					}
				}
				return n;
			}
		}
	}
	return 0;
}

int wnn_Strcspn(s1,s2)
register w_char *s1;
register w_char *s2;
{
	register w_char *p;
	register int n;

	while (*s1 != 0) {
		for (p = s2;*p != 0;p++) {
			if (*s1++ != *p) {
				n = 1;
				while (*s1 != 0) {
					for (p = s2;*p != 0;p++) {
						if (*s1++ != *p) {
							n++;
							break;
						}
					}
					if (*p == 0) {
						return n;
					}
				}
				return n;
			}
		}
	}
	return 0;
}

w_char *wnn_Strtok(s1,s2)
register w_char *s1;
register w_char *s2;
{
	static w_char *p = 0,*s;
	static w_char c;
	register int i,j,n;

	if (s1 == 0) {
		if (p == 0) return 0;
		*p = c;
	} else {
		p = s1;
	}
	n = wnn_Strlen(p) - (j = wnn_Strlen(s2));
	for (i = 0;i <= n;i++) {
		if (wnn_Strncmp(p++,s2,j) == 0) {
			s = p+j-1;
			break;
		}
	}
	if (i > n) return 0;
	for (;i <= n;i++) {
		if (wnn_Strncmp(p++,s2,j) == 0) {
			p--;
			c = *p;
			*p = 0;
			break;
		}
	}
	return s;
}

int wnn_Strwidth(buf)
w_char *buf;
{
  register int width;

	for (width = 0;*buf != 0;buf++) {
		if (is_full_width(*buf)) {
			width += 2;
		} else {
			width++;
		}
	}
	return width;
}

int wnn_Strnwidth(buf,n)
w_char *buf;
int n;
{
	register int width,i;

	for (width = 0,i = 0;i < n;buf++,i++) {
		if (is_full_width(*buf)) {
			width += 2;
		} else {
			width++;
		}
	}
	return width;
}

void
wnn_delete_ss2(s, n)
register unsigned int *s;
register int n;
{
    register unsigned int x;

    for(; n != 0 && (x = *s); n--, s++){
	if ((x & 0xff00) == 0x8e00) *s &= ~0xff00;
	if (x == 0xffffffff) break;
    }
}

void
wnn_delete_w_ss2(s, n)
register w_char *s;
register int n;
{
    register w_char x;

    for(; n != 0 && (x = *s); n--, s++){
	if ((x & 0xff00) == 0x8e00) *s &= ~0xff00;
    }
}

int
wnn_byte_count(in)
register int in;
{
    return(((in <= 0xa0 && in != 0x00 && in != 0x8e) || in == 0xff) ? 1 : 2);
}
#endif /* nodef */
