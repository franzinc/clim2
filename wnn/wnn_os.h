/*
 */
/*
 * Copyright Kyoto University Research Institute for Mathematical Sciences
 *                 1992
 * Copyright OMRON Corporation. 1992
 * Copyright ASTEC, Inc. 1992
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

#ifndef _WNN_OS_
#define _WNN_OS_

/* OS dependent */

#include <signal.h>

#ifdef SYSVR2
#include <fcntl.h>
#include <string.h>
#define index	strchr
#ifndef re_signal
# define re_signal(x, y) signal((x), (y))
#endif
#else
# define re_signal(x, y)
#endif

#if defined(SYSVR2) || defined(UX386)
#include <sys/param.h>
#define getdtablesize() (NOFILE)        /* sys/param.h must be included */
#ifndef SIGCHLD
# define SIGCHLD SIGCLD
#endif
#endif

#include <sys/types.h>
#include <sys/file.h>

#ifdef BSD42
#include <strings.h>
#endif

#ifdef TERMINFO
#include <curses.h>
#include <term.h>
#endif


/*
  if your system has wait3() system call define HAVE_WAIT3.
  wait3() doesn't have to be fully supported.
  uum uses only NULL for the 3rd parameter rusage.
 */

#if !defined(UX386) && !defined(SVR4) && !defined(hpux) && !defined(AIXV3)
#define HAVE_WAIT3
#endif

#if defined(luna) && !defined(SIGNALRETURNSINT)
#define SIGNALRETURNSINT
#endif

#ifndef SIGNALRETURNSINT
typedef void intfntype;
#define SIGNAL_RETURN	return
#else
typedef int intfntype;
#define SIGNAL_RETURN	return(0)
#endif
typedef intfntype (*intfnptr)();

#endif	/* _WNN_OS_ */
