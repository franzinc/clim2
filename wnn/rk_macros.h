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
/***********************************************************************
			rk_macros.h
						87.11.17  改 正

    本変換で使っているマクロ関数群。rk_header.h にincludeされている。
    これをincludeすると、自動的にrk_spclval.hもincludeされる。ユーザの
    プログラムでこれをincludeすれば、to_upperなどのマクロが使える。 
***********************************************************************/
/*	Version 3.0
 */
#ifndef RKMCRO

#define RKMCRO
#include "rk_spclval.h" /* ctype.hを使わないマクロは、この中で定義してある */

#ifndef MVUX
#  include <ctype.h>
#endif

	/* マクロ関数群（引数を複数回評価するものも多いので注意）*/

 /* 7ビットコードchar用マクロ */
#define isoctal(c) (isdigit(c) && (c) < '8') /* 8進の数字か */
#define isnulsp(c) (isspace(c) || (c) == '\0') /* EOL又は空白文字であるか */

  /* 大文字←→小文字変換。定義域は
     SYSVR2定義時  toupdown   7ビットchar（isasciiの成り立つ範囲）
		   _toupdown  英文字（isalphaの成り立つ範囲）
    その他の場合   toupdown		    〃				*/
#ifdef SYSVR2
#  define _toupdown(c) (isupper(c) ? _tolower(c) : _toupper(c))
#  define toupdown(c) (isupper(c) ? _tolower(c) : toupper(c))
#else
#  define toupdown(c) (isupper(c) ? tolower(c) : toupper(c))
#endif

 /* ctypeマクロの定義域を拡張したもの。letter型引き数にも適用可。
    is_eolspだけは独自のもの。*/
#define is_lower(l) (isascii(l) && islower(l))
#define is_upper(l) (isascii(l) && isupper(l))
#define is_alpha(l) (isascii(l) && isalpha(l))
#define is_alnum(l) (isascii(l) && isalnum(l))
#define is_digit(l) (isascii(l) && isdigit(l))
#define is_octal(l) (isascii(l) && isoctal(l))
#define is_xdigit(l) (isascii(l) && isxdigit(l))
#define is_space(l) (isascii(l) && isspace(l))
#define is_cntrl(l) (isascii(l) && iscntrl(l))
#define is_nulsp(l) (isascii(l) && isnulsp(l))
#define is_eolsp(l) (is_space(l) || (l) == EOLTTR)
#ifdef SYSVR2
#  define to_upper(l) (is_lower(l) ? _toupper(l) : (l))
#  define to_lower(l) (is_upper(l) ? _tolower(l) : (l))
#  define to_updown(l) (is_alpha(l) ? _toupdown(l) : (l))
#else
#  define to_upper(l) (is_lower(l) ? toupper(l) : (l))
#  define to_lower(l) (is_upper(l) ? tolower(l) : (l))
#  define to_updown(l) (is_alpha(l) ? toupdown(l) : (l))
#endif

#endif /* RKMCRO */
