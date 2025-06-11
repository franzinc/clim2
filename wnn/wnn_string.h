/*
 */
/*
 * Copyright Kyoto University Research Institute for Mathematical Sciences
 *                 1991, 1992
 * Copyright OMRON Corporation. 1991, 1992
 * Copyright ASTEC, Inc. 1991, 1992
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

#ifndef _WNN_STRING_
#define _WNN_STRING_

extern int wnn_sStrcpy();
extern int wnn_Sstrcpy();
extern void wnn_Sreverse();
extern char * wnn_Stos();
extern char *wnn_sStrncpy();
extern w_char *wnn_Strcat();
extern w_char *wnn_Strncat();
extern int wnn_Strcmp();
extern int wnn_Substr();
extern int wnn_Strncmp();
extern w_char *wnn_Strncpy();
extern int wnn_Strlen();
extern w_char *wnn_Strcpy();
extern void wnn_delete_w_ss2();
extern int wnn_byte_count();
#ifndef JS
extern int check_pwd();
#endif
#ifdef CHINESE
extern int wnn_Sstrcat();
#endif
#endif
