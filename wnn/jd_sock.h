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
/*	jd_sock.h
	jslib header file
*/

#ifdef UX386
#undef	AF_UNIX
#include <X11/Xos.h>
#else
#include <sys/types.h>
#endif /* UX386 */
#include <sys/socket.h>
#ifdef	AF_UNIX
#include <sys/un.h>
#endif	/* AF_UNIX */

#ifdef UX386
#include <net/in.h>
#else
#include <netinet/in.h>
#endif /* UX386 */

#if	defined(uniosu) || defined(UX386)
#include <net/netdb.h>	/* SX */
#else
#include <netdb.h>	/* SUN or BSD SYSV*/
#endif /*uniosu */

#ifdef TAIWANESE
#ifndef CHINESE
#define CHINESE
#endif
#endif

#define	ERROR	-1

#ifdef	JAPANESE	/* Japanese */
# define WNN_PORT_IN	(0x5701)
static char *sockname = "/tmp/jd_sockV4";	/* for jserver */
# define LANG_NAME	"ja_JP"
# define SERVERNAME	"wnn4"
# define MESSAGE_FILE	"jserver.msg"
#else	/* JAPANESE */

#ifdef	CHINESE
#ifdef	TAIWANESE		/* Traditional Chinese */
# define WNN_PORT_IN	(0x5731)
static char *sockname = "/tmp/td_sockV4";	/* for tserver */
# define LANG_NAME	"zh_TW"
# define SERVERNAME	"wnn4_Tw"
# define MESSAGE_FILE	"tserver.msg"
#else	/* TAIWANESE */		/* Simplified Chinese */
# define WNN_PORT_IN	(0x5711)
static char *sockname = "/tmp/cd_sockV4";	/* for cserver */
# define LANG_NAME	"zh_CN"
# define SERVERNAME	"wnn4_Cn"
# define MESSAGE_FILE	"cserver.msg"
#endif	/* TAIWANESE */
#else	/* CHINESE */

#ifdef	KOREAN			/* Korean *//* not yet */
# define WNN_PORT_IN	(0x5721)
static char *sockname = "/tmp/kd_sockV4";	/* for kserver */
# define LANG_NAME	"ko_KR"
# define SERVERNAME	"wnn4_Kr"
# define MESSAGE_FILE	"kserver.msg"
#else	/* KOREAN */

# define WNN_PORT_IN	(0x5701)
static char *sockname = "/tmp/jd_sockV4";	/* for jserver */
# define LANG_NAME	"ja_JP"
# define SERVERNAME	"wnn4"
# define MESSAGE_FILE	"jserver.msg"
#endif	/* KOREAN */
#endif	/* CHINESE */
#endif	/* JAPANESE */

#define	S_BUF_SIZ	1024	/* NEVER change this */
#define	R_BUF_SIZ	1024	/* NEVER change this */

