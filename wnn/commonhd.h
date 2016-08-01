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
/****************
 * Common header 
 ****************/
#include <stdio.h>
#ifdef linux
#include <unistd.h>
#endif
#define	JSERVER_VERSION	0x4200	/* minor version */
#define	_SERVER_VERSION	"4.20"

#include "wnnerror.h"

#ifndef JS
typedef  unsigned int  UINT;
typedef  unsigned char UCHAR;
#ifndef	w_char
# define w_char unsigned short
#endif	/* w_char */
#endif /*JS */

#ifdef TAIWANESE
#ifndef	CHINESE
#define CHINESE
#endif
#endif

#ifdef	CHINESE
#define	CONVERT_from_TOP
#define	CONVERT_by_STROKE	/* É®·Á(Bi Xing) */
#define	CONVERT_with_SiSheng	/* »ÍÀ¼(Si Sheng) */
#define NO_FZK			/* ÉÕÂ°¸ì¤Ï¡¢¤Ê¤¤ */
#define NO_KANA			/* ¤Ò¤é¤¬¤Ê(ÆÉ¤ß¤ÈÆ±¤¸¸õÊä)¤Ï¡¢¤Ê¤¤ */
#endif

#ifdef KOREAN
#define CONVERT_from_TOP
#define NO_FZK
#endif

#ifdef luna
#ifdef uniosu
# ifndef	SYSVR2
#  define	SYSVR2
# endif
# ifndef	TERMINFO
#  define	TERMINFO
# endif
#else	/* if defined(MACH) || defined(uniosb) */
# ifndef	BSD42
#  define	BSD42
# endif
# ifndef	BSD43
#  define	BSD43
# endif
#  if defined(luna68k)
#   ifndef	BSD44
#    define	BSD44
#   endif
#  endif /* defined(luna68k) */
# ifndef	TERMCAP
#  define	TERMCAP
# endif
#endif
#else	/* defined(luna) */
#if defined(sun) && !defined(SVR4)
# ifndef	BSD42
#  define	BSD42
# endif
# ifndef	TERMCAP
#  define	TERMCAP
# endif
#else	/* sun else */
#if defined(DGUX) || defined(linux)
# ifndef	SYSVR2
#  define	SYSVR2
# endif
# ifndef	TERMCAP
#  define	TERMCAP
# endif
#else
#if defined(SVR4) || defined(hpux) 
# ifndef	SYSVR2
#  define	SYSVR2
# endif
# ifndef	TERMINFO
#  define	TERMINFO
# endif
# ifdef sun
#  define SOLARIS
# endif
#else
# ifndef	BSD43
#  define	BSD43
# endif
# ifndef	BSD42
#  define	BSD42
# endif
# ifndef	TERMCAP
#  define	TERMCAP
# endif
#endif /* defined(SVR4) || defined(hpux) */
#endif /* DGUX */
#endif /* sun */
#endif /* luna */

#if defined(SYSVR2) || defined(sun)
# define SRAND48
#endif
#if defined(SVR4) || defined(hpux)
#ifndef F_OK
#define F_OK	0
#endif
#ifndef R_OK
#define R_OK	4
#endif
#endif

#define MAXBUNSETSU	80
#define LIMITBUNSETSU   400
#define MAXJIKOUHO	400

#define J_IUJIS 0
#define J_EUJIS 1
#define J_JIS   2
#define J_SJIS  3

#define C_IUGB	0
#define C_EUGB	1

#define C_ICNS11643  0
#define C_ECNS11643  1
#define C_BIG5  2

#define K_IUKSC 0
#define K_EUKSC 1
#define K_KSC   2

#ifndef	True
#define	True	1
#endif
#ifndef	False
#define	False	0
#endif

#define KANJI(x)	((x) & 0x80)


#define Ctrl(X)		((X) & 0x1f)

#define NEWLINE		Ctrl('J')
#define CR		Ctrl('M')
#define ESC		'\033'

#ifdef luna
#ifdef uniosu
#define RUBOUT		0x08	/* BS */
#else
#define RUBOUT		'\177'
#endif
#else
#define RUBOUT		'\177'
#endif
#define SPACE		' '


#define JSPACE		0xa1a1
#ifdef KOREAN
#define BAR		0xA1aa	/* ¡¼	*/
#else
#define BAR		0xA1BC	/* ¡¼	*/
#endif
#define KUTEN_NUM	0xA1A3	/* ¡£	*/
#define TOUTEN_NUM	0xA1A2	/* ¡¢	*/
#define S_NUM		0xA3B0	/* £°	*/
#define E_NUM		0xA3B9	/* £¹	*/
#ifdef KOREAN
#define S_HIRA		0xAAA1	/* ¤¡	*/
#define E_HIRA		0xAAF3	/* ¤ó	*/
#define S_KATA		0xABA1	/* ¥¡	*/
#define E_KATA		0xABF6	/* ¥ö	*/
#else
#define S_HIRA		0xA4A1	/* ¤¡	*/
#define E_HIRA		0xA4F3	/* ¤ó	*/
#define S_KATA		0xA5A1	/* ¥¡	*/
#define E_KATA		0xA5F6	/* ¥ö	*/
#endif
#define S_HANKATA	0x00A1	/* Ž¡	*/
#define E_HANKATA	0x00DF	/* Žß    */

#ifdef KOREAN
#define S_JUMO		0xa4a1	/* ¤¡ */
#define E_JUMO		0xa4fe	/* ¤þ */
#define S_HANGUL	0xb0a1	/* °¡ */
#define E_HANGUL	0xc8fe	/* Èþ */
#define S_HANJA		0xcaa1	/* Ê¡ */
#define E_HANJA		0xfdfe	/* ýþ */

#define ishanja(x)      ((unsigned)((x) - S_HANJA) <= (E_HANJA - S_HANJA))
#define ishangul(x)     ((unsigned)((x) - S_HANGUL) <= (E_HANGUL - S_HANGUL))
#endif

#define HIRAP(X) ((X) >= S_HIRA && (X) <= E_HIRA)
#define KATAP(X) (((X) >= S_KATA && (X) <= E_KATA) || ((X) == BAR))
#define ASCIIP(X) ((X) < 0x7f)
#define KANJIP(X) (!(HIRAP(X) || KATAP(X) || ASCIIP(X)))

#define YOMICHAR(X) ((HIRAP(X)) || \
		     ('0'<=(X)&&'9'>=(X)) || \
		     ('A'<=(X)&&'Z'>=(X)) || \
		     ('a'<=(X)&&'z'>=(X)) || \
		     (BAR == X) \
		    )
#define HIRA_OF(X) ((KATAP(X) && !(BAR == (X)))? ((X) & ~0x0100) : (X))

#ifdef	CONVERT_by_STROKE
# define Q_MARK		'?'
#endif	/* CONVERT_by_STROKE */

#define LENGTHYOMI      256	/* jisho ni touroku suru yomi no nagasa */
#define LENGTHKANJI     256	/* jisho ni touroku suru kanji no nagasa */
#define LENGTHBUNSETSU  264    /* Ê¸Àá¤ÎºÇÂçÄ¹ */
#define LENGTHCONV      512	/* ÊÑ´¹²ÄÇ½ºÇÂçÊ¸»ú¿ô */

#define JISHOKOSUU     20	

#define DIC_RDONLY 1   /* ¼­½ñ¤¬¥ê¡¼¥É¡¦¥ª¥ó¥ê¡¼¤Ç¤¢¤ë¡£*/


/* Ê£¿ô¤Î¥Õ¥¡¥¤¥ë¤Ë¤Þ¤¿¤¬¤Ã¤ÆÍÑ¤¤¤é¤ì¤Æ¤¤¤ë¥Ð¥Ã¥Õ¥¡¥µ¥¤¥º¤ÎÄêµÁ */
#define EXPAND_PATH_LENGTH 256 /* expand_expr()¤¬ÍÑ¤¤¤ë¥Ð¥Ã¥Õ¥¡¤Î¥µ¥¤¥º */


#define WNN_FILE_STRING "£×£î£î¤Î¥Õ¥¡¥¤¥ë"

#define WNN_FILE_STRING_LEN 16


#define F_STAMP_NUM 64
#define FILE_ALREADY_READ -2
#define FILE_NOT_READ -3

/*	file ID	*/
/*
  Local Variables:
  kanji-flag: t
  End:
*/

