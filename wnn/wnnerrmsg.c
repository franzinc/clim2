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
#include <stdio.h>
#include "jslib.h"
#include "commonhd.h"
#include "wnn_os.h"
#include "msg.h"

#ifdef	not_use
char *wnn_errormsg[] ={
"no_error 0",
"ファイルが存在しません。",		/*WNN_NO_EXIST	1 */
"no_error 2",
"メモリallocで失敗しました。",		/*WNN_MALLOC_ERR	3 */
"no_error 4",
"辞書ではありません。",		/*WNN_NOT_A_DICT	5  */
"頻度ファイルではありません。",	/*WNN_NOT_A_HINDO_FILE	6  */
"付属語ファイルではありません。",	/*WNN_NOT_A_FUZOKUGO_FILE 7  */
"no_error 8",
"辞書テーブルが一杯です。",	/*WNN_JISHOTABLE_FULL	9*/
"頻度ファイルが、指定された辞書の頻度ファイルではありません。",
				 /*WNN_HINDO_NO_MATCH	10 */
"no_error 11",
"no_error 12",
"no_error 13",
"no_error 14",
"no_error 15",
"ファイルがオープンできません。",	/*WNN_OPENF_ERR		16*/
"正しい頻度ファイルではありません。",		/* WNN_NOT_HINDO_FILE	17 */
"正しい付属語ファイルではありません。",	/* WNN_NOT_FZK_FILE 18 */
"付属語の個数、ベクタ長さなどが多過ぎます。",	/* WNN_FZK_TOO_DEF 19 */
"その番号の辞書は、使われていません。",	/*WNN_DICT_NOT_USED	20 */
"no_error 21",
"no_error 22",
"no_error 23",
"付属語ファイルの内容が正しくありません", /* WNN_BAD_FZK_FILE  24     */
"疑似品詞番号が異常ですhinsi.dataが正しくありません。", /*WNN_GIJI_HINSI_ERR 25 */
"未定義の品詞が前端品詞として定義されています。", /*WNN_NO_DFE_HINSI 26*/
"付属語ファイルが読み込まれていません。", /*WNN_FZK_FILE_NO_LOAD	27 */
"no_error 28",
"no_error 29",
"辞書のエイントリが多過ぎます。",	/*WNN_DIC_ENTRY_FULL	30*/
"変換しようとする文字列が長過ぎます。",	/*WNN_LONG_MOJIRETSU	31 */
"付属語解析領域が不足しています。",	/*WNN_WKAREA_FULL	32 */
"no_error 33",
"次候補領域が不足しています。",		/* WNN_JKTAREA_FULL 34 */
"候補が 1 つも作れませんでした ",	/* WNN_NO_KOUHO 35 */
"no_error 36",
"no_error 37",
"no_error 38",
"no_error 39",
"読みが長過ぎます。",			/*WNN_YOMI_LONG	40 */
"漢字が長過ぎます。",			/*WNN_KANJI_LONG	41 */
"指定された辞書は、登録可能ではありません。", /*WNN_NOT_A_UD	42 */
"読みの長さが0です。",			/*WNN_NO_YOMI	43 */
"指定された辞書は、逆引き可能ではありません。", /* WNN_NOT_A_REV	44 */
"リードオンリーの辞書に登録/削除しようとしました。",	/*WNN_RDONLY	45 */
"環境に存在しない辞書に登録しようとしました。", /* WNN_DICT_NOT_IN_ENV 46 */
"no_error 47",
"no_error 48",
"リードオンリーの頻度を変更しようとしました。", /* WNN_RDONLY_HINDO 49 */
"指定された単語が存在しません。",	/*WNN_WORD_NO_EXIST	50 */
"no_error 51",
"no_error 52",
"no_error 53",
"no_error 54",
"no_error 55",
"no_error 56",
"no_error 57",
"no_error 58",
"no_error 59",
"メモリallocで失敗しました。",		/*WNN_MALLOC_INITIALIZE	60 */
"no_error 61",
"no_error 62",
"no_error 63",
"no_error 64",
"no_error 65",
"no_error 66",
"no_error 67",
"何かのエラーが起こりました。",		/* WNN_SOME_ERROR 68 */
"バグが発生している模様です。",		/*WNN_SONOTA 69 */
"サーバが死んでいます。",		/*WNN_JSERVER_DEAD 70 */
"allocに失敗しました。",	/*WNN_ALLOC_FAIL	71 */
"サーバと接続できませんでした。",	/*WNN_SOCK_OPEN_FAIL	72 */
"通信プロトコルのバージョンが合っていません。", /* WNN_BAD_VERSION	73 */
"クライアントの生成した環境ではありません。", /* WNN_BAD_ENV 74 */
"no_error 75",
"no_error 76",
"no_error 77",
"no_error 78",
"no_error 79",
"ディレクトリを作ることができません。",	/* WNN_MKDIR_FAIL	80	*/
"no_error 81",
"no_error 82",
"no_error 83",
"no_error 84",
"no_error 85",
"no_error 86",
"no_error 87",
"no_error 88",
"no_error 89",
"ファイルを読み込むことができません。",		/* WNN_FILE_READ_ERROR	90*/
"ファイルを書き出すことができません。",		/* WNN_FILE_WRITE_ERROR	91*/
"クライアントの読み込んだファイルではありません。",	/* WNN_FID_ERROR 92*/
"これ以上ファイルを読み込むことができません。",	/* WNN_NO_MORE_FILE	93*/
"パスワードが間違っています。",		/* WNN_INCORRECT_PASSWD    94   */
"ファイルが読み込まれています。 ", /*WNN_FILE_IN_USE    	95 */
"ファイルが削除できません。 ", /*WNN_UNLINK    		96 */
"ファイルが作成出来ません。", /*WNN_FILE_CREATE_ERROR	97 */
"Ｗｎｎのファイルでありません。", /*WNN_NOT_A_FILE		98	*/
"ファイルのI-nodeとFILE_UNIQを一致させる事ができません。", /*WNN_INODE_CHECK_ERROR   99 */

"品詞ファイルが大き過ぎます。",		/* WNN_TOO_BIG_HINSI_FILE 100 */
"品詞ファイルが大き過ぎます。",		/* WNN_TOO_LONG_HINSI_FILE_LINE 101  */
"品詞ファイルが存在しません。",		/* WNN_NO_HINSI_DATA_FILE 102 */
"品詞ファイルの内容が間違っています。",	/* WNN_BAD_HINSI_FILE 103 */
"no_error 104",
"品詞ファイルが読み込まれていません。",	/* WNN_HINSI_NOT_LOADED 105*/
"品詞名が間違っています。",		/* WNN_BAD_HINSI_NAME 106 */
"品詞番号が間違っています。",		/* WNN_BAD_HINSI_NO 107 */
"no_error 108",
"その操作はサポートされていません。", /*NOT_SUPPORTED_OPERATION 109 Not Used*/
"パスワードの入っているファイルがオープンできません。", /*WNN_CANT_OPEN_PASSWD_FILE 110  */
/* 初期化時のエラー  */
"uumrcファイルが存在しません。", /*WNN_RC_FILE_NO_EXIST    111 Not Used*/
"uumrcファイルの形式が誤っています。", /* WNN_RC_FILE_BAD_FORMAT  112 Not Used*/
"これ以上環境を作ることはできません。", /* WNN_NO_MORE_ENVS  113 */
"このクライアントが読み込んだファイルでありません。", /* WNN_FILE_NOT_READ_FROM_CLIENT 114 */
"辞書に頻度ファイルがついていません。", /*WNN_NO_HINDO_FILE 115 */
"パスワードのファイルが作成出来ません。" /*WNN_CANT_CREATE_PASSWD_FILE 116*/
};

int wnn_errormsg_len = sizeof(wnn_errormsg) / sizeof(char *);

extern	int	wnn_errorno;
static char msg[] = ":BAD ERRORNO!!! ";

char *wnn_perror()
{
static char msgarea[100];

  if((wnn_errorno < 0) || (wnn_errorno > sizeof(wnn_errormsg) / sizeof(char *))){
    sprintf(msgarea , "%d"  , wnn_errorno);
    strcat(msgarea , msg);
    return(msgarea);
  }
  return(wnn_errormsg[wnn_errorno]);
}
#endif	/* not_use */

extern	int	wnn_errorno;
extern struct msg_cat *wnn_msg_cat;
static char msg[] = ":BAD ERRORNO!!! ";

char *
wnn_perror_lang(lang)
char *lang;
{
    static char msgarea[100];

    sprintf(msgarea , "%d"  , wnn_errorno);
    strcat(msgarea , msg); 
    return(msg_get(wnn_msg_cat, wnn_errorno, msgarea, lang));
}

char *
wnn_perror()
{
    return(wnn_perror_lang(NULL));
}

/*
  Local Variables:
  eval: (setq kanji-flag t)
  eval: (setq kanji-fileio-code 0)
  eval: (mode-line-kanji-code-update)
  End:
*/
