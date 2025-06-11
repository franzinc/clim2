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
#ifndef	LIBDIR
#define	LIBDIR		"/usr/local/lib/wnn"
#endif	/* LIBDIR */

#ifdef TAIWANESE
#ifndef CHINESE
#define CHINESE
#endif
#endif

#define WNN_USERNAME_ENV	"WNNUSER"
#define WNN_JSERVER_ENV		"JSERVER"
#define WNN_CSERVER_ENV		"CSERVER"
#define WNN_KSERVER_ENV		"KSERVER"
#define WNN_TSERVER_ENV		"TSERVER"
#define WNN_J_LANG		"ja_JP"
#define WNN_C_LANG		"zh_CN"
#define WNN_K_LANG		"ko_KR"
#define WNN_T_LANG		"zh_TW"
#ifdef JAPANESE
#define WNN_DEFAULT_LANG	WNN_J_LANG
#define WNN_DEF_SERVER_ENV	WNN_JSERVER_ENV
#else /* JAPANESE */
#ifdef CHINESE
#ifdef TAIWANESE
#define WNN_DEFAULT_LANG	WNN_T_LANG
#define WNN_DEF_SERVER_ENV	WNN_TSERVER_ENV
#else /* TAIWANESE */
#define WNN_DEFAULT_LANG	WNN_C_LANG
#define WNN_DEF_SERVER_ENV	WNN_CSERVER_ENV
#endif /* TAIWANESE */
#else /* CHINESE */
#ifdef KOREAN
#define WNN_DEFAULT_LANG	WNN_K_LANG
#define WNN_DEF_SERVER_ENV	WNN_KSERVER_ENV
#else /* KOREAN */
#define WNN_DEFAULT_LANG	WNN_J_LANG
#define WNN_DEF_SERVER_ENV	WNN_JSERVER_ENV
#endif /* KOREAN */
#endif /* CHINESE */
#endif /* JAPANESE */
#define WNN_UUM_ENV  		"UUMRC"
#define WNN_KEYBOARD_ENV	"KEYBOARD"
#define WNN_COUNTDOWN_ENV	"UUM_COUNTDOWN"

#define PATHNAMELEN	256

/* for jserver */
#ifndef	SERVER_INIT_FILE
# ifdef JAPANESE
#  define SERVER_INIT_FILE	"/ja_JP/jserverrc"
# else /* JAPANESE */
# ifdef	CHINESE
# ifdef	TAIWANESE
#  define SERVER_INIT_FILE	"/zh_TW/tserverrc"
# else /* TAIWANESE */
#  define SERVER_INIT_FILE	"/zh_CN/cserverrc"
# endif /* TAIWANESE */
# else /* CHINESE */
# ifdef KOREAN
#  define SERVER_INIT_FILE	"/ko_KR/kserverrc"
# else /* KOREAN */
#  define SERVER_INIT_FILE	"/ja_JP/jserverrc"	/* Default */
# endif /* KOREAN */
# endif /* CHINESE */
# endif /* JAPANESE */
#endif
#define JSERVER_DIR		"/tmp"

/* for uum */
#define RCFILE			"/uumrc"	/* LIBDIR/@LANG/RCFILE */
#define USR_UUMRC		"/.uumrc"
#define RKFILE			"/rk/mode"	/* LIBDIR/@LANG/RKFILE */
#define CPFILE			"/uumkey"	/* LIBDIR/@LANG/CPFILE */
#define MESSAGEFILE		"/message_file"

#define CONVERT_FILENAME	"/cvt_key_tbl"

#define ENVRCFILE		"/wnnenvrc"
#ifndef	HINSIDATA_FILE
# define HINSIDATA_FILE		"/ja_JP/hinsi.data"
#endif	/* HINSIDATA_FILE */

#define USR_DIC_DIR_VAR "@USR"

/*
  if you wish to do flow control active for your tty,
  define FLOW_CONTROL to 1.
  note that this 'tty' means the tty from which wnn is invoked.
 */

#define FLOW_CONTROL 0

#define C_LOCAL '!'		
/* For Local File Name.
   Local File Name is send as "Hostname!Filename" when C_LOCAL is '!'.
   It is also used in jl_library to specify local file-name, that is,
   file-names which start with this character are considered to be local.
   */


/*
  define default kanji code system for your 'tty' side and 'pty' side.
  'tty' side (TTY_KCODE) means 'your terminal's code'.
  'pty' side (PTY_KCODE) means 'application's code'.
 */

#ifdef luna
# ifdef uniosu
#define TTY_KCODE J_EUJIS
#define PTY_KCODE J_EUJIS
# else	/* defined(MACH) || defined(uniosb) */
#define TTY_KCODE J_EUJIS
#define PTY_KCODE J_EUJIS
# endif
#else /* !luna */
# ifdef DGUX
#define TTY_KCODE J_EUJIS
#define PTY_KCODE J_EUJIS
# else /* !DGUX */
#define TTY_KCODE J_JIS
#define PTY_KCODE J_JIS
# endif
#endif

#define TTY_CCODE C_EUGB
#define PTY_CCODE C_EUGB
#define TTY_TCODE C_BIG5
#define PTY_TCODE C_BIG5

#define TTY_HCODE K_EUKSC
#define PTY_HCODE K_EUKSC

/*
  OPTIONS defines what options are available.
  define it by modifying ALL_OPTIONS string.
  if you wish to make some option abailable, leave that character unchanged.
  else turn that character some non-option character, ex. '*'.
  TAKE CARE NOT TO MOVE CHARACTER POSITION, ORDER, ETC!!

  see sdefine.h for precise definition of ALL_OPTIONS. defines below
  may be incorrect.

#define GETOPTSTR   "hHujsUJSPxXk:c:r:l:D:n:vL:"
#define ALL_OPTIONS "hHujsUJSPxXkcrlDnvL"

#ifndef OPTIONS
#define OPTIONS ALL_OPTIONS
#endif
 */

#define	WNN_TIMEOUT	5	/* connect の際に５秒待つんだよ */
#define	WNN_DISP_MODE_LEN	5	/* モード表示に必要なcolumn数 */
