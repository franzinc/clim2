/*
 */
/*
 * Copyright Kyoto University Research Institute for Mathematical Sciences
 *                 1987, 1988, 1989, 1990, 1991
 * Copyright OMRON Corporation. 1987, 1988, 1989, 1990, 1991
 * Copyright ASTEC, Inc. 1987, 1988, 1989, 1990, 1991
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
  eval(crypt(PWD, "HA")) routine.
*/
#ifndef JS
#include <stdio.h>
#include "commonhd.h"
#include "jslib.h"
#include "wnn_os.h"
#endif

extern char *crypt();

#ifdef JS
static
#endif
void
new_pwd(src, encd)
char *src, *encd;
{
    int i, x,c;
    char xx[2];
    char *cr;

    if(encd == NULL)encd = src;
    if(strcmp(src, "") == 0){ bzero(encd, WNN_PASSWD_LEN);return;}
    x = time(NULL);
    xx[0] = x & 0x3f;
    xx[1] = (x & 0x3f00) >> 8;
    for (i = 0; i < 2; i++) {
	c = xx[i] + '.';
	if (c > '9')
	    c += 7;
	if (c > 'Z')
	    c += 6;
	xx[i] = c;
    }
    cr = crypt(src, xx);
    bzero(encd, WNN_PASSWD_LEN);
    strncpy(encd, cr, WNN_PASSWD_LEN);
}

#ifdef JS
static
#endif
int
check_pwd(src, encd)
char *src, *encd;
{
    if(strcmp(encd, "") == 0) return(1);  /* No passwd */
    return(!strncmp(encd, crypt(src, encd),WNN_PASSWD_LEN));
}
