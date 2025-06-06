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
			rk_extvars.h
						88. 6.16  訂 補

	二つ以上のファイルにまたがって使われる変数のextern宣言。
	本体はrk_vars.cで定義。
***********************************************************************/
/*	Version 3.0
 */
extern	char	rk_errstat;

extern	int	flags;
extern	jmp_buf env0;

extern	FILE	*modefile;
extern	char	nulstr[];

extern	char	*hcurread, *mcurread, *curfnm, *curdir;
extern	letter	*ltrbufbgn;
extern	FILE	*nestfile[], **base;

extern	struct	modestat
{
	modetyp	moderng;	/* モードの状態の最大値＋１ */
	modetyp	curmode;	/* モードの状態 */
} modesw[];

extern	char	hyoshu[];
extern	char	**modmeiptr, *modmeibgn[], *modmeimem, modmeimem_[];
extern	char	**dspnamptr, *dspnambgn[], *dspcod, dspcod_[], *dspmod[][2];

extern	char	**hyomeiorg, **hyomeiptr, *hyomeimem;
extern	char	**pathmeiorg, **pathmeiptr, *pathareaorg, *pathmeimem;

extern	int	usemaehyo[], usehyo[], useatohyo[], naibu_[], *naibu;

extern	letter	*lptr;
extern	letter	rk_input, disout[], rk_output[], keybuf[], urabuf[];
extern	int	lastoutlen, lastkbflen;
extern	letter	oneletter[], nil[];
extern	int	hyonum;

extern	struct funstr	/* 組み込み関数の性質を保持するstruct */
{
	char	*fnname;	/* 関数名 */
	char	appear;		/* 入力コード部・出力コード部・バッファ残り部
				   に書けるかどうかのフラグ３ビット */
	char	argnum;		/* 引数の個数 */
	char	fntype;		/* 結果のタイプ */
} rk_funstr;

extern	struct	dat	/* 入力・出力・バッファ残り部へのポインタ */
{
	letter	*code[3];
} rk_dat;

extern	struct	hyo
/* 一つの対応表の、変換データ及び各変数の変域の、格納場所へのポインタ。*/
{
	struct	dat	*data;
	letter		**hensudef;
} rk_hyo;

extern	struct	funstr	func[];
extern	struct	hyo	hyo_n[];
