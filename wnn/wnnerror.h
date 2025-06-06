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
/* file 関係のエラー */
#define WNN_FILE_READ_ERROR	90 /* ファイルを読み込むことができません。 */
#define WNN_FILE_WRITE_ERROR	91 /* ファイルを書き出すことができません。 */
#define WNN_FID_ERROR		92 /* クライアントの読み込んだファイルではありません。 */
#define WNN_NO_MORE_FILE	93 /* これ以上ファイルを読み込むことができません。*/
#define WNN_INCORRECT_PASSWD    94 /* パスワードが間違っています。 */
#define WNN_FILE_IN_USE    	95 /* ファイルが読み込まれています。 */
#define WNN_UNLINK    		96 /* ファイルが削除できません。 */
#define WNN_FILE_CREATE_ERROR	97 /* ファイルが作成出来ません。*/
#define WNN_NOT_A_FILE		98	/*Ｗｎｎのファイルでありません。*/
#define WNN_INODE_CHECK_ERROR   99 /* ファイルのI-nodeとFILE_UNIQを一致させる事ができません。 */

/*	V3	*/
/* 辞書追加関係のエラー */
#define WNN_NO_EXIST		1	/*ファイルが存在しません。*/
#define WNN_MALLOC_ERR		3	/*メモリallocで失敗しました。*/
#define WNN_NOT_A_DICT		5 	/*辞書ではありません。*/
#define WNN_NOT_A_HINDO_FILE	6	/*頻度ファイルではありません。*/
#define WNN_NOT_A_FUZOKUGO_FILE	7	/*付属語ファイルではありません。*/
#define WNN_JISHOTABLE_FULL	9	/*辞書テーブルが一杯です。*/
#define WNN_HINDO_NO_MATCH	10	/*頻度ファイルが、指定された辞書の
					  頻度ファイルではありません。*/
#define WNN_OPENF_ERR		16	/*ファイルがオープンできません。*/
#define WNN_NOT_HINDO_FILE	17	/*正しい頻度ファイルでありません。*/
#define WNN_NOT_FZK_FILE	18	/*正しい付属語ファイルでありません。*/
#define WNN_FZK_TOO_DEF		19	/*付属語の個数、ベクタ長さなどが
					  多過ぎます*/
/* 辞書削除関係のエラー */
#define WNN_DICT_NOT_USED	20	/*その番号の辞書は、使われていません。*/

/* 変換時のエラー */
#define WNN_BAD_FZK_FILE  24      /*付属語ファイルの内容が正しくありません*/
#define WNN_GIJI_HINSI_ERR	25	/*疑似品詞番号が異常です
					  hinsi.dataが正しくありません*/
#define WNN_NO_DFE_HINSI	26	/*未定義の品詞が前端品詞として
					  定義されています Not Used*/
#define WNN_FZK_FILE_NO_LOAD	27	/*付属語ファイルが読み込まれていません*/

/* jishobiki.c */
#define WNN_DIC_ENTRY_FULL	30	/*辞書のエイントリが多過ぎます。*/
#define WNN_LONG_MOJIRETSU	31	/*変換しようとする文字列が長過ぎます。*/
#define WNN_WKAREA_FULL		32	/*付属語解析領域が不足しています。*/
#define WNN_JKTAREA_FULL 	34	/*次候補領域が不足しています。*/
#define WNN_NO_KOUHO		35	/*候補が 1 つも作れませんでした*/

/* 単語登録時のエラー */
#define WNN_YOMI_LONG		40	/*読みが長過ぎます。*/
#define WNN_KANJI_LONG		41	/*漢字が長過ぎます。*/
#define WNN_NOT_A_UD		42 /*指定された辞書は、登録可能ではありません。*/
#define WNN_NO_YOMI		43	/*読みの長さが0です。*/
#define WNN_NOT_A_REV		44 /*指定された辞書は、逆引き可能ではありません。*/
#define WNN_RDONLY		45	/*リードオンリーの辞書に登録しようとしました。*/
#define WNN_DICT_NOT_IN_ENV	46	/*環境に存在しない辞書に登録しようとしました。*/

/* 頻度更新のエラー */
#define WNN_RDONLY_HINDO	49 /* リードオンリーの頻度を変更しようとしました。 */

/* 単語削除時、品詞削除時のエラー */
/*
WNN_RDONLY
*/
#define WNN_WORD_NO_EXIST	50	/*指定された単語が存在しません。*/

/* 初期化の時のエラー */
#define WNN_MALLOC_INITIALIZE	60	/*メモリallocで失敗しました。*/

#define WNN_SOME_ERROR		68	/* 何かのエラーが起こりました。*/
#define WNN_SONOTA		69	/*バグが発生している模様です。Not Used*/
#define WNN_JSERVER_DEAD	70	/*サーバが死んでいます。(jlib)*/
#define WNN_ALLOC_FAIL		71	/*allocに失敗(jlib)*/
#define WNN_SOCK_OPEN_FAIL	72	/*jd_beginでsocketのopenに失敗(jlib)*/
#define WNN_BAD_VERSION		73	/*通信プロトコルのバージョンが合っていません。*/
#define WNN_BAD_ENV		74      /*クライアントの生成した環境ではありません。*/

#define WNN_MKDIR_FAIL		80	/* ディレクトリを作り損なった */


/*品詞ファイル */
#define WNN_TOO_BIG_HINSI_FILE 100 	/*品詞ファイルが大き過ぎます。*/
#define WNN_TOO_LONG_HINSI_FILE_LINE 101		/*品詞ファイルが大き過ぎます。*/
#define WNN_NO_HINSI_DATA_FILE 102	/*品詞ファイルが存在しません。 */
#define WNN_BAD_HINSI_FILE 103	       /*品詞ファイルの内容が間違っています。*/

#define WNN_HINSI_NOT_LOADED 105 	/* 品詞ファイルが読み込まれていません。*/
#define WNN_BAD_HINSI_NAME 106		/* 品詞名が間違っています */
#define WNN_BAD_HINSI_NO 107		/* 品詞番号が間違っています */

#define NOT_SUPPORTED_OPERATION 109 /*その操作はサポートされていませんNot Used*/

/* 
 *
 *  jl （高水準ライブラリ）のエラー
 * 
 */

#define WNN_CANT_OPEN_PASSWD_FILE 110  /* パスワードの入っているファイルが
					 オープンできません(jl) */
/* 初期化時のエラー  */
#define WNN_RC_FILE_NO_EXIST 111 /* uumrcファイルが存在しません(jl) Not Used*/
#define WNN_RC_FILE_BAD_FORMAT 112 /* uumrcファイルの形式が誤っています(jl)Not Used */
#define WNN_NO_MORE_ENVS  113 /* これ以上環境を作ることは出来ません。*/
#define WNN_FILE_NOT_READ_FROM_CLIENT 114 /* このクライアントが読み込んだファイルでありません。*/
#define WNN_NO_HINDO_FILE 115 /* 辞書に頻度ファイルがついていません。*/

#define WNN_CANT_CREATE_PASSWD_FILE 116 /*パスワードのファイルが作成出来ません。*/
/*
  Local Variables:
  kanji-flag: t
  End:
*/
