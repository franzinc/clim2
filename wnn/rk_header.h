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
			rk_header.h
						88. 5.20  訂 補

	rk_main.c rk_read.c rk_modread.c rk_bltinfn.cの共通ヘッダ。
	中で取っている配列の大きさなどを定義。
***********************************************************************/
/*	Version 3.0
 */
/*	make時に必要なdefine

	BSD42		BSDにてstrings.hを使用（string.hを使う場合は不要）
	SYSVR2		System Vにて定義域の制限されたtoupper・tolowerを使用
			（なくても動く）
	MVUX		ECLIPSE MVでの運転時にdefine  IKISが自動defineされる

	RKMODPATH="文字列"
			その文字列をモード定義表のサーチパスの環境変数の
			名前にする
	WNNDEFAULT	「@LIBDIR」で標準設定表のあるディレクトリを表せる
			ようにする	
	IKIS		半角仮名の１バイト目を0xA8（デフォルトは0x8E）にする

	この他 デバッグ時は必要に応じて KDSP、CHMDSPをdefine
*/

#ifdef MVUX
#  define IKIS
#endif

#include <stdio.h>
#ifdef BSD42
#  include <strings.h>
#  define strchr  index
#  define strrchr rindex
#else
#  include <string.h>
#endif
#include "rk_macros.h"

#define ESCCHR	'\033'
#define BASEMX	(26 + 10)

#define REALFN	200 /* 表のフルネームの最大長 */

#ifdef KOREAN
#define LINALL  30000 /* 対応表全部の行数合計 */
#define SIZALL  300000 /* 対応表の内部表現の最大サイズ*/
#else
#define LINALL	2000 /* 対応表全部の行数合計 */
#define SIZALL	20000 /* 対応表の内部表現の最大サイズ・
			表一つの変数の変域長の合計としても使ってる */
#endif

#define LINSIZ	1000 /* 対応表の一行の最大サイズ */
#define TRMSIZ	500 /* 対応表の一項目の最大サイズ・
			モード名の最長としても使ってる */
#define KBFSIZ	100 /* 本処理バッファのサイズ */
#define DSPLIN	256 /* デバッグ用 */
#define OUTSIZ	200 /* 出力バッファのサイズ */
#define RSLMAX	20 /* 関数の値として返る文字列の最長 */

#define VARMAX	50 /* 表一個の変数個数 */
#define VARTOT	2000 /* 全表の変数個数計 */
#define VARLEN	500 /* 変数名の長さの計 */

#define FILNST	20

	/* rk_modread.cで使うdefine */

#define HYOMAX	40 /* 変換対応表の最大個数 */
#define HYOMEI	500 /* 表名の文字数計 */
#define PTHMAX	30 /* サーチパスの最大個数 */
#define PTHMEI	800 /* サーチパス名の文字数計 */
#define MODMAX	50 /* モードの種類数 */
#define MODMEI	300 /* モードの全文字数 */
#define DMDMAX	40 /* モード表示の種類数 */
#define DMDCHR	250 /* モード表示の全文字数 */
#define MDHMAX	2500 /* モード表の最大サイズ */
  /* モード表の最初のlistscanの時は、エラーを考慮して、リスト1個のbufferに
	表のサイズ分取っておく。*/
#define MDT1LN	200 /* モード設定リスト1個の最大長 */
#define NAIBMX	400 /* モード定義表の内部表現の最大サイズ */
			/* Change KURI 200 --> 400 */

#define ARGMAX	2 /* 条件判断関数の引数の最大個数 */

 /* ディレクトリ名の区切りのdefine（UNIX用）。UNIX以外の環境で使うには
    これと、fixednamep()も変更の必要がある（readmode()のgetenv関係も勿論）。*/
#define KUGIRI '/'

 /* エラー処理用 */
#ifndef _WNN_SETJMP
#define _WNN_SETJMP
#include <setjmp.h>
#endif
