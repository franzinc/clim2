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
			rk_main.c
						88. 6.10  改 正

	ローマ字かな変換・その他、入力コードを変換するプログラム。
	ASCIIコード使用を、前提としている。
	このファイルは、変換のメインルーチン。
***********************************************************************/
/*	Version 3.0
 */
#include "rk_header.h"
#include "rk_extvars.h"
#include "rk_fundecl.h"

struct	matchpair /*	現在マッチされている変数の番号と、マッチした文字の
			組を保持。*/
{
	int	hennum;
	letter	ltrmch;
};

letter	memory[SIZALL]; /* 変換対応表や、変数の変域などを入れる領域 */
struct	dat	data[LINALL]; /* 対応表の一行ごとのデータへのポインタ */
letter	*hensudefhyo[VARTOT]; /* 変数ごとの変域データへのポインタ */
struct	matchpair	henmatch[VARTOT]; /* 変数のマッチ状況をためておく */

#ifdef KDSP
#  ifdef MVUX
letter	displine[DSPLIN]; /* デバッグ用	 表示の一行分を記憶 */
#  endif
#endif

letter	*curdis;
int	codein_len;
letter	*codeout, *remainkbf;
letter	ungetc_buf = EOLTTR; /* romkan_ungetcが一文字退避しておくバッファ */
letter	unnext_buf = EOLTTR; /* romkan_unnextが		   〃		  */

letter	(*keyin_method)(); /* キーイン関数のアドレス */
int	(*bytcnt_method)(); /* バイトカウント関数のアドレス */
int	(*kbytcnt_method)(); /* キー入力解釈用バイトカウント関数のアドレス */
char	prv_modfnm[REALFN] = "\0"; /* モード表名（又はそのパス名）を保存 */
	    /* 初期設定がいい加減! */

#define DISOUT ((flags & RK_NONISE) ? rk_output : disout)
 /* フラグの値はromkan_init3()の中で設定され、そのRK_NONISEのビットの値の
    標準は0。従ってDISOUTの標準値はdisout。これをrk_outputに変えると、偽コード
    （除：LTREOF）を一切出さない（但し、キー入力があれば必ず何かを返すように
    してある時に限り、EOLTTRをも返す）。*/

char	eofflg; /* romkan_next()からLTREOFが来たときに、romkan_henkan()内で1
		   にする。これの値が非0なら変換対応表の(error)を無視して、
		   本処理バッファ内の未確定コードも、強制的に変換させる。更に
		   LTREOFが来た時に特別にある出力を出すように指定されていれば
		   その処理も行う（但し、その処理は急ごしらえで不完全）。その
		   処理中はeofflgは2。*/

letter	evalbuf[2][2][OUTSIZ];
char	ebf_sw = 0;
letter	saishu_out[OUTSIZ];
letter	delchr, delchr2 = EOLTTR, nisedl; /* DELのキャラクタは二個まで持てる*/

modetyp	chgmod(), incmod(), decmod();


/* letterを返す関数の定義及び宣言（一部にはcharのものも混じっている） */

static letter	mchedsrc();
letter	romkan_next(), romkan_unnext();
letter	romkan_getc(), romkan_ungetc(), *romkan_henkan();
letter	to_zenalpha(), to_zenhira(), to_zenkata();
 /* to_zenhira;kata は濁点を持つ文字を一まとめにしない。*/

void BUGreport();
static void maeato_henkan(), add_at_eof(), rk_delete(),
  set_rubout(), mchevl(), codeout_chg(), ltrevlcpy();
int match();
static int p_eq(), hen_ikisrc(), henkan_ok(), rk_rst(),
  head_bytecount();
extern void readdata(), romkan_reset(), hank_setup(), to_hankata(),
  to_digit(), dakuadd(), handakuadd(), allchgmod();
extern int ltov();

static
letter	*ltrcpy(lp1, lp2)
letter	*lp1, *lp2;
{
	letter	*org;

	org = lp1;
	while((*lp1++ = *lp2++) != EOLTTR);
	return(org);
}

#ifdef notuse
letter	*ltrncpy(lp1, lp2, n) /* 末尾に'EOLTTR'をセット */
letter	*lp1, *lp2;
int	n;
{
	letter	*org;

	org = lp1;
	for(; n; --n) if(EOLTTR == (*lp1++ = *lp2++)) return(org);
	*lp1 = EOLTTR;
	return(org);
}
#endif

 /** letterの列の末尾に一文字つなげる */
static
letter	*ltr1cat(lp, l)
letter	*lp, l;
{
	letter	*org;

	org = lp;
	totail(lp);
	*lp++ = l;
	*lp = EOLTTR;
	return(org);
}

 /** 文字列の末尾に一文字つなげる。ltr1catのchar版 */
char	*chrcat(s, c)
char	*s, c;
{
	char	*org;

	org = s;
	strtail(s);
	*s++ = c;
	*s = '\0';
	return(org);
}

static
letter	*ltrcat(lp1, lp2)
letter	*lp1, *lp2;
{
	letter	*org;

	org = lp1;
	totail(lp1);
	ltrcpy(lp1, lp2);
	return(org);
}

 /** letterの列lp2の各要素の最上位ビットを立ててから、lp1の後ろにつなげる。*/
static
letter	*bitup_ltrcat(lp1, lp2)
letter	*lp1, *lp2;
{
	letter	*org;

	org = lp1;
	totail(lp1);

	while((*lp1 = *lp2++) != EOLTTR) *lp1++ |= HG1BIT;
	 /** lp2 の要素全ての最上位ビットを立てる。*lp2がNISEBP(rk_spclval.h
	     でdefine)などのときは最初から最上位が立ってるからよい */

	return(org);
}

 /** ltrcatしたあと、結果の文字列の末尾にポインタをもっていく。*/
letter	*ltrgrow(lp1, lp2)
letter	*lp1, *lp2;
{
	totail(lp1);
	while((*lp1 = *lp2++) != EOLTTR) lp1++;
	return(lp1);
}

static int
ltrlen(lp)
letter	*lp;
{
	letter	*org;

	for(org = lp; *lp != EOLTTR; lp++);
	return(lp - org);
}

 /**	letterの列の最後の文字へのポインタを返す。但し空文字列に対しては
	そのまま返す。*/
letter	*ltrend(lp)
letter	*lp;
{
	return((*lp != EOLTTR) ? (lp + ltrlen(lp) - 1) : lp);
}

 /**	文字列の最後の文字へのポインタを返す。ltrend()のchar版。ltrend()と
	同様、空文字列のときは特例がある。*/
char	*strend(s)
char	*s;
{
	return(*s ? (s + strlen(s) - 1) : s);
}

int
ltrcmp(lp1, lp2)
letter	*lp1, *lp2;
{
	for(; *lp1 == *lp2; lp1++, lp2++) if(*lp1 == EOLTTR) return(0);
	return(*lp1 > *lp2 ? 1 : -1);
}

#ifdef notuse
int
ltrncmp(lp1, lp2, n)
letter	*lp1, *lp2;
int	n;
{
	for(; n && *lp1 == *lp2; n--, lp1++, lp2++)
		if(*lp1 == EOLTTR) return(0);
	return(n == 0 ? 0 : (*lp1 > *lp2 ? 1 : -1));
}

letter	*ltr_index(lp, l)
letter	l, *lp;
{
	for(; *lp != EOLTTR; lp++) if(*lp == l) return(lp);
	return(NULL);
}
#endif

static
letter	*ltr_rindex(lp, l)
letter	l, *lp;
{
	letter	*lp2;

	for(lp += ltrlen(lp2 = lp); lp2 != lp; ) if(*--lp == l) return(lp);
	return(NULL);
}

 /** 文字列の末尾一文字をカットする。*/
letter	*ltr1cut(lp)
letter	*lp;
{
	int	l;

	if(0 == (l = ltrlen(lp))) BUGreport(0); /* Illegal POP */
	*(lp + --l) = EOLTTR;
	return(lp);
}

 /** letterの一文字を長さ１の文字列に変換する */
static
letter	*ltr_to_ltrseq(lp, l)
letter	*lp, l;
{
	*lp++ = l;
	*lp-- = EOLTTR;
	return(lp);
}

 /** charの列からletterを一つ取り出す。但し'\0'はEOLTTRに変える */
letter	letterpick(lbfpptr)
uns_chr **lbfpptr;
{
	letter	l = 0;
	register int	i;

	for(i = (*bytcnt_method)(*lbfpptr); i; i--)
		l = (l << 8) + *(*lbfpptr)++;
	return(l == 0 ? EOLTTR : l);
}

 /**	入力されたコードを文字単位にまとめる。但しEOFはLTREOF（rk_spclval.h
	にて定義）に変える。*/
letter	romkan_next()
{
	letter	in;
	int	i, n;
	uns_chr c;

	  /* unnextされている場合は、それを取り出す。*/
	if(unnext_buf != EOLTTR)
		return(in = unnext_buf, unnext_buf = EOLTTR, in);

	if( (letter)EOF == (in = (*keyin_method)() )) return(LTREOF);
	c = (in &= 0xff);
	n = (*kbytcnt_method)(&c);
	for(i = 1; i < n; i++) in = (in << 8) + ((*keyin_method)() & 0xff);
	return(in);
}

 /*	letterの文字 l とcharの文字 c の比較。エンドマーク同士（letter列の
	エンドマークはEOLTTR、char列のは'\0'）も一致とみなす。*/
#define ltrchreq(l, c) ((l) == ((c) != 0 ? (c) : EOLTTR))

 /** letterの文字列とcharの文字列の比較 但し大小比較は符号なしとしてやる */
int
ltrstrcmp(l, s)
register letter *l;
register char	*s;
{
	for(; ltrchreq(*l, *s); l++, s++) if(*s == 0) return(0);
	return((*l < (uns_chr)*s || *l == EOLTTR)? -1 : 1);
}

 /** 最後にマッチした文字 つまりurabufの最後。urabufが空だとEOLTTRを返す */
static
letter	lastmch()
{
	return(*(ltrend(urabuf)));
}

/* end of 'letter' functions */


#ifdef	OMAKE
 /** お	 ま  け
	ＴＶアニメのご案内（一部特撮を含む）。引数が非負整数の時はどれか一つ、
	-1なら全てを出力し、それ以外なら何も出力しない。いずれの場合も、データ
	の個数を返す。*/
takeoka(i)
int	i;
{
#define OPIMAX (numberof(opinion))

    static char *opinion[] = {
	"「魔法のアイドル パステルユーミ」は良い番組だ。by竹岡",
	"「機動戦士ガンダムΖΖ」の地球編は良い番組だ。by服部",
	"「魔法のスター マジカルエミ」は良い番組だ。by竹岡",
	"（参考意見）「マジカルエミ〔蝉時雨〕」は良いアニメだ。by森島",
	"「魔法の天使 クリィミーマミ」は良い番組だ。by竹岡",
	"「サザエさん」は良い番組だ。by鈴木",
	"「ムーミン」は良い番組だ。by鈴木",
	"「マンガ大作戦」のロードランナーシリーズは良い番組だ。by新出",
	"「スプーンおばさん」は良い番組だ。by鴨",
	"「はじめ人間ギャートルズ」は良い番組だ。by富永",
	"「宇宙の騎士テッカマン」は良い番組だ。by駒嵐",
	"「妖怪人間ベム」は良い番組だ。by鴨",
	"「アルプスの少女ハイジ」は良い番組だ。by富永",
	"「ときめきトゥナイト」は良い番組だ。by駒嵐",
	"「あしたのジョー」は良い番組だったね。by立木（ついき）",
	"「きまぐれオレンジ・ロード」は良い番組だ。by三好・服部",
	"「ゲッターロボ」（「ゲッターロボＧ」を含む）は良い番組だ。by新出",
	"「テッちゃん」（「じゃりン子チエ」のこと）は良い番組だ。by松田",
	"「チキチキマシン猛レース」は良い番組だ。by服部",
	"「小公女セーラ」は良い番組だ。by森島",
	"「秘密戦隊ゴレンジャー」は良い番組だ。by新出",
	"（白黒時代の）「鉄人28号」は良い番組だ。by桜川"
    };

	if(i >= 0) printf("%s\n", opinion[i % OPIMAX]);
	 else if(i == -1) for(i= 0; i< OPIMAX; i++) printf("%s\n",opinion[i]);
	return(OPIMAX);
}
#endif	/* OMAKE */


 /** 変換バッファのクリア */
void	romkan_clear()
{
#ifdef KDSP
#  ifdef MVUX
	*displine = EOLTTR;
#  endif
#endif
	ungetc_buf = *keybuf = *urabuf = *disout = *rk_output = EOLTTR;
	curdis = DISOUT;
	lastoutlen = lastkbflen = 0;
}

 /**	表を読み込んで変換の初期設定をする。（part 1）*/
int
romkan_init(modhyo, delchr_, chmoutf, keyinfn, bytcntfn)
char	*modhyo, chmoutf;
letter	delchr_;
letter	(*keyinfn)();
int	(*bytcntfn)(); /* 引数の詳細はromkan_init3を参照 */
{
	return(romkan_init2(modhyo, delchr_, chmoutf, keyinfn, bytcntfn,
			    0, 0, 0));
}

#define ifflg(a, b) ((a) ? (b) : 0)

 /**	表を読み込んで変換の初期設定をする。（part 2: キー入力に対し必ず何か
	返すようにするか、キーバッファをクリアするかどうかなども指定可）*/
int
romkan_init2
(modhyo, delchr_, chmoutf, keyinfn, bytcntfn, keyackf, restartf, nonisecodf)
char	*modhyo, chmoutf, keyackf, restartf, nonisecodf;
letter	delchr_;
letter	(*keyinfn)();
int	(*bytcntfn)(); /* 引数の詳細はromkan_init3を参照 */
{
	return(romkan_init3(modhyo, delchr_, toNISE(delchr_), EOLTTR,
			    keyinfn, bytcntfn, (int (*)())NULL,
			    restartf,
			    ifflg(chmoutf, RK_CHMOUT) |
			    ifflg(keyackf, RK_KEYACK) |
			    ifflg(nonisecodf, RK_NONISE) |
			    RK_DSPNIL));
}

 /**	表を読み込んで変換の初期設定をする。（part 3）*/
int
romkan_init3
(modhyo, delchr_, nisedl_, delchr2_, keyinfn, bytcntfn, kbytcntfn, restartf,
 flags_)
char	*modhyo;	/* モード定義表の名又はそのパス名 */
letter	delchr_;	/* DELとして使うコード */
letter	nisedl_;	/* 偽DELとして使うコード */
letter	delchr2_;	/* DELとして使うコードが二つある場合そのもう一方 */
letter	(*keyinfn)();	/* キーイン関数 */
int	(*bytcntfn)();	/* バイトカウント関数 */
int	(*kbytcntfn)(); /* キーインの解釈用バイトカウント関数。NULL指定の時は
			   bytcntfnと同じものが使われる */
char	restartf;
 /*	rk_rst()内から呼ばれた時に立つフラグ。これが立つと、バッファをクリア
	せずに済ます。この時はエラー検出時のメッセージも少し異なる */
int	flags_;
 /* 以下のフラグが利用可。これらはrk_spclval.hにて定義。
	RK_CHMOUT:モードチェンジを知らせるコードを返すか？
	RK_KEYACK:キーインに対し必ず何かを返すか
	RK_DSPNIL:romkan_disp(off)modeのデフォルトを空文字列にするか
	RK_NONISE:偽コードを出さないようにするか
	RK_REDRAW:Wnn用特殊フラグ（redraw用のフラグを出すかどうか）
	RK_SIMPLD:deleteの動作を単純にするか
	RK_VERBOS:verboseモードで起こすか
 */
{
	int	errcod; /* 今の所1のみ */

	 /* 偽コードを出すかの設定はromkan_clearで使うので、フラグの設定を
	    先にやっておかないといけない。*/
	if((flags = flags_) & RK_VERBOS){
		fprintf(stderr, "romkan_init invoked.\r\n");
		fflush(stderr);
	}

#ifdef	OMAKE
	if(modhyo == NULL) takeoka(-1); /* おまけ；通常は実行されない */
#endif	/* OMAKE */

	keyin_method = keyinfn;
	bytcnt_method = (bytcntfn == NULL ? head_bytecount : bytcntfn);
	kbytcnt_method = (kbytcntfn == NULL ? bytcnt_method : kbytcntfn);
	strcpy(prv_modfnm, modhyo);

	delchr = delchr_;
	nisedl = nisedl_;
	delchr2 = delchr2_;
 /**	実はDELのキャラクタを二個まで持てる。二個目：delchr2は、未設定の
	とき（値がEOLTTR）は無視される。それ以外は、delchr2が入力されると
	delchrが入力されたのと同じ振る舞いをする。*/

	if((errcod = setjmp(env0)) != 0){
		fprintf(stderr,"romkan_init failed.");
		if(!restartf){
			romkan_reset();
		 /*	fprintf(stderr,
			      "No conversion is currently performed."); */
			fprintf(stderr, "\r\n");
		} else {
			fprintf(stderr,
				"Conversion method was unchanged.\r\n");
		}
	} else {
		readdata(memory, data, hensudefhyo, modhyo);
		if(!restartf){
			romkan_clear();
			hank_setup();
		}
	}

	if(!errcod && (flags & RK_VERBOS)){
		fprintf(stderr, "romkan_init finished.\r\n");
		fflush(stderr);
	}
	return(errcod); /* 正常終了なら0 */
}

 /**	バイトカウント関数のデフォルト。sの指している所に入っているのが
	何バイトコードかを返す。
	romkan_nextに注意。そちらでは、バイトカウント関数の値は
	引数の一文字目にのみ依存すると仮定している。*/
static int
head_bytecount(s)
uns_chr *s;
{
#ifdef IKIS
	return((*s <= 0xa0 || *s == 0xff)? 1 : 2);
#else
	return(((*s <= 0xa0 && *s != HNKAK1) || *s == 0xff)? 1 : 2);
#endif
}

 /** 変換された文字を順次返す */
letter	romkan_getc()
{
	letter	l;

	  /* ungetcされている場合は、それを取り出す。*/
	if(ungetc_buf != EOLTTR)
		return(l = ungetc_buf, ungetc_buf = EOLTTR, l);

	while(*curdis == EOLTTR){
		 /* romkan_next()の値がLTREOFの時も、そのまま送れば良い。*/
		curdis = romkan_henkan(romkan_next());

		 /* keyackflgが非0なら、キーインがあれば必ず何か返す。その
		    ため、返すべきものがない時はEOLTTRを返すことにする。*/
		if(flags & RK_KEYACK) break;
	}

	if(EOLTTR != (l = *curdis)) curdis++;
	return(l);
	 /* 偽物の文字なら、HG1BITが立っている。*/
	 /* 特別なコード(例えば偽のBEEPとしてNISEBP)を返すときがある。それらは
	    rk_spclval.hに定義されている。*/
}

 /**	romkan_getcの下位関数として、入力を一文字受け取って
	変換結果の文字列を出力する。*/
letter	*romkan_henkan(mae_in)
letter	mae_in;
{
	letter	mae_out[2], *p;

	/* if(*curdis != EOLTTR){p = curdis; curdis = nil; return(p);} */
	/* ↑これではmae_inが無視される */
	curdis = nil;

	  /* 前・後処理への入力は常にただ一文字 */
	mae_out[0] = mae_out[1] = EOLTTR;

	eofflg = rk_errstat = 0;
	*rk_output = *disout = EOLTTR;

	maeato_henkan(mae_in, mae_out, usemaehyo);
	rk_input = *mae_out;

	if(rk_input == LTREOF){
		 /* LTREOFが来た場合、(error)を無視し、本処理バッファの末尾迄
		    強制変換する。そのためにeofflgを1にする。その後、結果の
		    末尾に、LTREOFが来た時の特別コード（指定されていれば）と、
		    LTREOFをつなぐ。*/
		eofflg = 1;
		match();

		eofflg = 2;
		add_at_eof(); /* codeoutに、LTREOFが来た時出すコードが入る。*/
		ltr1cat(codeout, LTREOF);

		ltrcat(disout, codeout);
		ltrcat(rk_output, codeout);
	} else if(rk_input == EOLTTR){
		 /* EOLTTRが来た場合も、上と同様の処理を行うが、LTREOFは
		    つながない。なお、これはromkan_getc()を呼んでいる時は
		    起こらない（romkan_next()がEOLTTRを返さないから）。*/
		eofflg = 1;
		match();
	} else if(rk_input == delchr || rk_input == delchr2){
	    /* delchr2が未設定ならその値はEOLTTRなのでrk_inputと等しくない。*/
		rk_delete(rk_input);
	} else {
		ltr1cat(keybuf, rk_input);
		ltr1cat(disout, toNISE(rk_input));
		match();
	}

	if(!(flags & RK_CHMOUT)){
	   /* chmoutflgが0の時は、CHMSIGを出さない。*/
		for(p = DISOUT; ; p++){
			while(*p == CHMSIG) ltrcpy(p, p + 1);
			if(*p == EOLTTR) break;
		}
	}

	if((flags & RK_REDRAW) && NULL != (p = ltr_rindex(disout, nisedl))){
		for(p++; *p != EOLTTR || (ltr1cat(disout, REDRAW), 0); p++)
			if(!isSPCL(*p)) break;
	}
	 /* wnnのredrawのフラグが立っていて、disoutがnisedl以後特殊コードのみ
	    で終わっていたら、REDRAWを出して、Wnnに変換行のredrawをさせる。*/

	return(DISOUT);
}


 /* デバッグ用関数 */
#ifdef KDSP
#  ifdef MVUX

void
pridbg2(a, b, c)
char	*a, *c;
letter	*b;
{
	printf("%s", a); dump_fordbg(b); printf("%s", c);
}

int
ltr_displen(l)
letter	l;
{
	while(l >= 256) l >>= 8;

	if(l == 0) return(0);
	if(l < ' ') return(0);
	if(168 <= l && l <= 170) return(1);
	return(l > 160 ? 2 : 1);
}

void
dump_fordbg(lp)
letter	*lp;
{
	while(*lp != EOLTTR) printf("%x/", *lp++);
}

void
print_fordbg(lp)
letter	*lp;
{
	while(*lp != EOLTTR) putletter(*lp++ & ~HG1BIT);
}

void
print_ltrseq(lp)
letter	*lp;
{
	while(*lp != EOLTTR) print_ltr(*lp++ & ~HG1BIT);
}

void
print_ltr(l)
letter	l;
{
	letter	*disptail;
	int	i;

	if(!isSPCL(l)) l &= ~HG1BIT;

	if(l == CHMSIG)
#ifdef CHMDSP
		printf("...mode=%s\n",romkan_dispmode())
#endif
		;
	else
#define BEEPCH	'\007'
#define NEWLIN	'\n'
	if(l == BEEPCH || l == NISEBP) putchar(BEEPCH);
	else
	if(l == NEWLIN){
		*displine = *keybuf = *urabuf = EOLTTR;
		lastoutlen = lastkbflen = 0;
		putchar(l);
	}
	else
	if(l == delchr || l == nisedl){
		if(*displine == EOLTTR) putchar(BEEPCH);
		 else {
			disptail = ltrend(displine);
			for(i = ltr_displen(*disptail); i; i--)
				printf("\031 \031");
			*disptail = EOLTTR;
		}
	}
	else {
		ltr1cat(displine, l);
		putletter(l);
	}
}

#  endif /* of #ifdef MVUX */
#endif
/* デバッグ用関数終わり */


 /** DELが入力されたときの処理をする */
static void
rk_delete(input_del)
letter	input_del;
{
	if(ltrlen(keybuf) > ((flags & RK_SIMPLD) ? 0 : lastkbflen)){
		ltr1cut(keybuf);
		set_rubout(disout, 1, nisedl);
	} else {
		if(*urabuf != EOLTTR && !(flags & RK_SIMPLD)){
			ltr1cut(ltrcpy(keybuf, urabuf));
			*urabuf = EOLTTR;

			set_rubout(rk_output, lastoutlen, input_del);

			set_rubout(disout, lastkbflen, nisedl);
			bitup_ltrcat(ltrcat(disout, rk_output), keybuf);

			lastkbflen = lastoutlen = 0;
		} else {
			set_rubout(disout, 1, input_del);
			set_rubout(rk_output, 1, input_del);
		}
	}
}

 /**	letterの列 lp1 と lp2 のマッチを試みる。返値は、lp1がlp2の頭部と
	一致の時 -1、lp1またはその頭部とlp2が一致のときは一致長（lp2が
	空文字列の時を含む。この場合返値は0）、それ以外は -2。
	lp2側に式が含まれていたら評価をする。lp1側には式を含んではだめ */
static int
prefixp(lp1, lp2)
letter	*lp1, *lp2;
{
	/* 1行マッチさせるごとに、まずmatch情報をクリアしてから。
	   つまり、henmatch[0] . ltrmch = EOLTTR; としておく。*/

	register int	mch_len = 0, d_len;

	for(;;){
		if(*lp2 == EOLTTR) return(mch_len);
		if(*lp1 == EOLTTR) return(-1);
		if((d_len = p_eq(&lp2, &lp1)) < 0) return(d_len);
		mch_len += d_len;
	}
}

 /** num番目の変数が既にある文字とマッチしていると仮定して、その文字を返す */
static
letter	mchedsrc(num)
int	num;
{
	struct	matchpair	*pairptr;

	for(pairptr = henmatch; pairptr -> ltrmch != EOLTTR; pairptr++){
		if(num == pairptr -> hennum)
			return(pairptr -> ltrmch);
	}
	return(BUGreport(8), 0);
}

 /**	num番目の変数が文字 l とマッチするか調べる。その変数がunboundだった
	場合は l にbindする。マッチしたら(bindの時を含む)1、しないと0を返す */
static int
mchsrc(num, l)
int	num;
letter	l;
{
	struct	matchpair	*pairptr;

	if(hen_ikisrc(num, l) == 0) return(0);
	for(pairptr = henmatch; pairptr -> ltrmch != EOLTTR; pairptr++){
		if(num == pairptr -> hennum)
			return(pairptr -> ltrmch == l);
	}

	pairptr -> ltrmch = l;
	pairptr -> hennum = num;
	(++pairptr) -> ltrmch = EOLTTR;
	return(1);
}

 /**	l1pから一単位を取って評価し、文字l2と一致するかどうかを返す。評価した
	結果が一文字にならなかったら、当然一致しない。*/
static int
l_eq(l1p, l2)
letter	*l1p, l2;
{
	letter	evlrsl[RSLMAX];

	switch(SHUBET(*l1p)){
		case 0: return(*l1p == l2);
		case 1: return(mchsrc((int)LWRMSK(*l1p), l2));
		case 2:
			mchevl(&l1p, evlrsl);
			return(evlrsl[0] == l2 && evlrsl[1] == EOLTTR);
		default:
			return(BUGreport(1), 0);
	}
}

 /**	prefixp内で使用	 但し引数の順序は逆、すなわち式が含まれうるのはl1pp側
	のみ。l1ppから一単位ぶん取って評価したものとl2ppのマッチを試みる。それ
	がl2ppまたはその頭部とマッチすれば一致長を返し（l1ppの評価結果が空文字
	列の時を含む。この場合返値は0）、逆にl1ppの評価結果の頭部とl2ppがマッ
	チした時は -1を返す。マッチが失敗したら返値は -2。*/
static int
p_eq(l1pp, l2pp)
register letter **l1pp, **l2pp;
{
	int	num;
	letter	evlrsl[RSLMAX], *rslptr;
	register int	retval = -2;

	 /*  l2pp側には式を含まない筈 */
	if(!is_HON(**l2pp)){
	    /*	if(is_HON(**l1pp)) retval = p_eq(l2pp, l1pp); else  */
		BUGreport(9);
	} else {
		switch(SHUBET(**l1pp)){
			case 0: /* 文字同士 */
				retval = (*(*l1pp)++ == *(*l2pp)++ ? 1 : -2);
				break;
			case 1: /* 変数と文字 */
				num = LWRMSK(*(*l1pp)++);
				retval = (mchsrc(num, *(*l2pp)++) ? 1 : -2);
				break;
			case 2: /* 式と文字 */
				mchevl(l1pp, rslptr = evlrsl);
				for(retval = 0; *rslptr != EOLTTR; retval++){
					if(**l2pp == EOLTTR){
						retval = -1; break;
					} else if(*rslptr++ != *(*l2pp)++){
						retval = -2; break;
					}
				}
				break;
			default:
				BUGreport(2);
		}
	}

	return(retval);
}

 /** l1pから一単位評価してl2pに入れる */
static void
mchevl(l1pp, l2p)
letter	**l1pp, *l2p;
{
	letter	*l1p, tmpevl[RSLMAX];

	l1p = *l1pp;

	switch(SHUBET(*l1p)){
		case 0: *l2p++ = *l1p++; break;
		case 1: *l2p++ = mchedsrc((int)LWRMSK(*l1p++)); break;
		case 2:	 /* toupper, tolower, error, … */
			switch(LWRMSK(*l1p++)){
				case 2: mchevl(&l1p, tmpevl);
					*l2p++ = to_upper(*tmpevl);
					break;
				case 3: mchevl(&l1p, tmpevl);
					*l2p++ = to_lower(*tmpevl);
					break;
				case 4:
					*l2p++ = CHMSIG;
					*l2p++ = LWRMSK(*l1p++);
					*l2p++ = 0; break; /* EOLではない */
				case 5:
					*l2p++ = CHMSIG;
					*l2p++ = LWRMSK(*l1p++);
					*l2p++ = 1; break;
				case 6:
					*l2p++ = CHMSIG;
					*l2p++ = XY2INT(2, LWRMSK(*l1p++));
					*l2p++ = 1; break;
				case 7:
					mchevl(&l1p, tmpevl);
					*l2p++ = to_updown(*tmpevl);
					break;
				case 8:
					mchevl(&l1p, tmpevl);
					*l2p++ = to_zenalpha(*tmpevl);
					break;
				case 9:
					mchevl(&l1p, tmpevl);
					*l2p++ = to_hira(*tmpevl);
					break;
				case 10:
					mchevl(&l1p, tmpevl);
					*l2p++ = to_kata(*tmpevl);
					break;
				case 11:
					mchevl(&l1p, tmpevl);
					to_hankata(*tmpevl, &l2p);
					break; /* 特殊 */
				case 12:
					mchevl(&l1p, tmpevl);
					*l2p++ = to_zenhira(*tmpevl);
					break;
				case 13:
					mchevl(&l1p, tmpevl);
					*l2p++ = to_zenkata(*tmpevl);
					break;
				case 14:
					mchevl(&l1p, tmpevl);
					*l2p = *tmpevl;
					mchevl(&l1p, tmpevl);
					*l2p += *tmpevl;
					LWRCUT(*l2p++);
					break;
				case 15:
					mchevl(&l1p, tmpevl);
					*l2p = *tmpevl;
					mchevl(&l1p, tmpevl);
					*l2p -= *tmpevl;
					LWRCUT(*l2p++);
					break;
				case 16:
					mchevl(&l1p, tmpevl);
					*l2p = *tmpevl;
					mchevl(&l1p, tmpevl);
					*l2p *= *tmpevl;
					LWRCUT(*l2p++);
					break;
				case 17:
					mchevl(&l1p, tmpevl);
					*l2p = *tmpevl;
					mchevl(&l1p, tmpevl);
					if(! *tmpevl) *l2p = LTRHUG;
					 else *l2p /= *tmpevl;
					LWRCUT(*l2p++);
					break;
				case 18:
					mchevl(&l1p, tmpevl);
					*l2p = *tmpevl;
					mchevl(&l1p, tmpevl);
					if(! *tmpevl) *l2p = LTRHUG;
					 else *l2p %= *tmpevl;
					LWRCUT(*l2p++);
					break;
				 /* 19〜21・30は、条件を満たすと空文字列、
				    そうでないとUNUSDCを文字列として返す。*/
				case 19:
					mchevl(&l1p, tmpevl);
					if(lastmch() != *tmpevl)
						*l2p++ = UNUSDC;
					break;
				case 20:
					if(! modesw[LWRMSK(*l1p++)] . curmode)
						*l2p++ = UNUSDC;
					break;
				case 21:
					if(modesw[LWRMSK(*l1p++)] . curmode)
						*l2p++ = UNUSDC;
					break;
				case 22:
					*l2p++ = REASIG;
					break;
				case 23:
					*l2p++ = delchr;
					break;
				case 24:
					*l2p++ = CHMSIG;
					*l2p++ = XY2INT(1, 0);
					 /* これで「all」を表す */
					*l2p++ = 0; break;
				case 25:
					*l2p++ = CHMSIG;
					*l2p++ = XY2INT(1, 0);
					*l2p++ = 1; break;
				case 26:
					mchevl(&l1p, tmpevl);
					*l2p = *tmpevl;
					mchevl(&l1p, tmpevl);
					*l2p &= *tmpevl;
					LWRCUT(*l2p++);
					break;
				case 27:
					mchevl(&l1p, tmpevl);
					*l2p = *tmpevl;
					mchevl(&l1p, tmpevl);
					*l2p |= *tmpevl;
					LWRCUT(*l2p++);
					break;
				case 28:
					mchevl(&l1p, tmpevl);
					*l2p = ~(*tmpevl);
					LWRCUT(*l2p++);
					break;
				case 29:
					*l2p++ = URBFCL;
					break;
				case 30:
					if(eofflg != 2 || *keybuf != EOLTTR)
						*l2p++ = UNUSDC;
					break;
				case 31:
					{
						letter	code, basenum;

						mchevl(&l1p, tmpevl);
						code = *tmpevl;
						mchevl(&l1p, tmpevl);
						if((basenum = *tmpevl) <= 1 ||
						   BASEMX < basenum)
							basenum = 10;
						to_digit(code, basenum, &l2p);
					}
					break;
				case 32:
					mchevl(&l1p, tmpevl);
					dakuadd(*tmpevl, &l2p);
					break; /* 特殊 */
				case 33:
					mchevl(&l1p, tmpevl);
					handakuadd(*tmpevl, &l2p);
					break; /* 特殊 */
				case 34:
					mchevl(&l1p, tmpevl);
					*l2p++ = ltov(*tmpevl);
					break;
				case 35:
					*l2p++ = ERRCOD;
					break;
			     /*	case 36: omitted */
				case 37:
					*l2p++ = CHMSIG;
					*l2p++ = LWRMSK(*l1p++);
					*l2p++ = *l1p++; break;
				case 38:
					*l2p++ = CHMSIG;
					*l2p++ = XY2INT(2, LWRMSK(*l1p++));
					*l2p++ = *l1p++; break;
				case 39:
					*l2p++ = CHMSIG;
					*l2p++ = XY2INT(3, LWRMSK(*l1p++));
					*l2p++ = *l1p++; break;
				case 40:
					{
						letter	modnum;

						modnum = LWRMSK(*l1p++);
						if(modesw[modnum] . curmode !=
						   *l1p++)
							*l2p++ = UNUSDC;
						break;
					}
				case 41:
					{
						letter	modnum;

						modnum = LWRMSK(*l1p++);
						if(modesw[modnum] . curmode ==
						   *l1p++)
							*l2p++ = UNUSDC;
						break;
					}
				case 42:
					{
						letter	modnum;

						modnum = LWRMSK(*l1p++);
						if(modesw[modnum] . curmode >=
						   *l1p++)
							*l2p++ = UNUSDC;
						break;
					}
				case 43:
					{
						letter	modnum;

						modnum = LWRMSK(*l1p++);
						if(modesw[modnum] . curmode <=
						   *l1p++)
							*l2p++ = UNUSDC;
						break;
					}
				case 44:
					mchevl(&l1p, tmpevl);
					*l2p++ = SENDCH;
					*l2p++ = *tmpevl;
					break;
				default:/* case 0及び上記以外 */;
					BUGreport(7);
			}
	}

	*l2p = EOLTTR;
	*l1pp = l1p;
}

 /** num番目の変数の変域が文字 l を含むかどうかを返す */
static int
hen_ikisrc(num, l)
int	num;
letter	l;
{
	letter	*defptr;

	defptr = hyo_n[hyonum] . hensudef[num];
	if(*defptr == VARRNG){
		for(defptr++; *defptr != EOLTTR; )
			if(*defptr++ <= l && l <= *defptr++) return(1);
		return(0);
	}

	for(; *defptr != EOLTTR; defptr++){
		if(l == *defptr) return(1);
	}
	return(0);
}

 /**	変換のメインルーチン。本処理を行うが、ついでに後処理もやっている。
	ちなみに前処理は、romkan_getcの下位関数romkan_henkanの中で、
	この関数を呼ぶ前にやっている。
	この関数は、romkan_nextから一文字来る度に呼ばれる。呼び出された直後は
	outputは空文字列、disoutには入力コード一文字が入っている。
	この関数で得られる文字の列が、romkan_henkanに渡り、
	romkan_getcは、それを文字ごとに分解して返す。
	(error)でエラーが引き起こされた場合は0を返し、正常終了時は1を返す */
int
match()
{
	int	henkanflg = 0, okcode = 0, chm_exist;
	letter	*p;
	letter	urabufcreate[KBFSIZ], orgkeybuf[KBFSIZ], kbftail[KBFSIZ];
	letter	*urabufjunbi, *outcutptr, *dis_end;

	if(*keybuf == EOLTTR){
		*urabuf = EOLTTR;
		return(1);
	}

	ltrcpy(urabufjunbi = orgkeybuf, keybuf);
	outcutptr = rk_output;

	while((okcode = henkan_ok()) > 0){
		henkanflg = 1;

		codeout_chg();
		ltrcat(rk_output, codeout);

		ltrcpy(kbftail, keybuf + codein_len);
		ltrcat(ltrcpy(keybuf, remainkbf), kbftail);

		if(okcode == 2){
			ltrcpy(urabufjunbi = urabufcreate, keybuf);
			totail(outcutptr);
		}
	}

	if(okcode == 0){
		ltr1cut(ltrcpy(keybuf, orgkeybuf));
		ltr_to_ltrseq(disout, NISEBP);
		*rk_output = EOLTTR;
		return(0);
	}
	if(henkanflg){
		ltrcpy(urabuf, urabufjunbi);

		set_rubout(disout, ltrlen(orgkeybuf) - 1, nisedl);

		dis_end = disout;
		totail(dis_end);
		ltrcpy(dis_end, rk_output);

		 /* モードチェンジを直ちに知らせるため CHMSIGを出力
		    （flags中のRK_CHMOUTが立ってないと、あとで一文字ずつに
		    分解する時点で、CHMSIGをカット）。
		    但し、rk_outputからは、CHMSIGを抜く。
		    また、CHMSIGは末尾に１回しか出力しない
		    （２回以上あっても、１回にまとめて、末尾に置く）。*/
		for(chm_exist = 0, p = rk_output; ; p++){
			while(*p == CHMSIG){
				chm_exist = 1;
				if(ltrcpy(p, p + 1) < outcutptr) outcutptr--;
			}
			if(*p == EOLTTR) break;
		}
		if(chm_exist){
		 /* CHMSIGを１つにまとめたものをdis_endにつなげ直す。
		    このif文をカットすれば、CHMSIGのとりまとめはやらない */
			ltr1cat(ltrcpy(dis_end, rk_output), CHMSIG);
		}

		bitup_ltrcat(disout, keybuf);
		lastoutlen = ltrlen(outcutptr);
		lastkbflen = ltrlen(keybuf);
	}
	return(1);
}

 /**	LTREOFが入ったときに、何か出すように指定されているか調べて、
	codeoutをその結果の文字列（指定がなかったら当然空）にポイントする。
	超急ごしらえで、特殊コード等は一切無視する。*/
static void
add_at_eof()
{
	register struct dat	*datptr;
	register int	i;
	int	hyoseq;
	letter	evlrsl[RSLMAX], *p;

	for(hyoseq = 0; (hyonum = usehyo[hyoseq]) != -1; hyoseq++){
		for(i = 0, datptr = hyo_n[hyonum] . data;
		    NULL != (p = datptr[i] . code[0]); i++){
			henmatch[0] . ltrmch = EOLTTR;
			while(*p != EOLTTR){
				switch(SHUBET(*p)){
					case 0: /* 文字 */
					case 1: /* 変数 */
					 /* これらがある場合は、NULLとは
					    マッチし得ない。*/
						goto Pass;
					case 2: /* 式 */
						mchevl(&p, evlrsl);
						if(*evlrsl != EOLTTR)
							goto Pass;
					 /* 入力コード部に、評価すると
					    空文字列になるものが、他にはない
					    ことが前提。*/
				}
			}

			ltrevlcpy(codeout = p = evalbuf[0][0],
				  datptr[i] . code[1]);
			while(*p != EOLTTR){
				if(isSPCL(*p)) ltrcpy(p, p + 1);
				 else p++;
			}
			codeout_chg();
			return;

Pass:			;
		}
	}
	codeout = nil;
}

 /**	一回マッチを試みる。返値は、マッチして確定した場合1（モードチェンジが
	混じっている場合は2）、マッチしたが未確定の時-1、マッチしなかったら0。
	実行中は、変数 l に、それまでに一致した長さの最高記録を入れており、
	より長く一致するものが見つかるごとに、これを更新する。lの値は、マッチ
	していても0になることもある。p_eq() 及び prefixp() の注釈文を参照。*/
static int
henkan_ok()
{
	register struct dat	*datptr;
	register int	i, k;
	int	l, j, hyoseq;
	char	urabuf_clrf; /* モードチェンジなどで、urabufをクリアする必要が
				生じた場合はこれが立ち、その結果、henkan_ok()
				が1を返すべきところで2を返す。それを見て、
				match()がurabufなどの調整をする。*/
	register letter *p;

	if(*keybuf == EOLTTR) return(-1);

	for(l = -1, hyoseq = 0; (hyonum = usehyo[hyoseq]) != -1; hyoseq++){
		for(i = 0, datptr = hyo_n[hyonum] . data;
		    NULL != (p = datptr[i] . code[0]); i++){
			henmatch[0] . ltrmch = EOLTTR;
			switch(k = prefixp(keybuf, p)){
				case -2: break;
				case -1:
					if(eofflg != 0) break;
				 /* eofflgが立っていたら、未確定の可能性は
				    捨てる。*/

					return(-1);
				default:
					if(k > l){
						ebf_sw = !ebf_sw;
						for(j = 1; j <= 2; j++){
							ltrevlcpy(
							 evalbuf[ebf_sw][j-1],
							 datptr[i] . code[j]);
						}

						l = k;
					}
			}
		}
	}

	if(l >= 0){
		codein_len = l;
		codeout = evalbuf[ebf_sw][0];
		remainkbf = evalbuf[ebf_sw][1];

		for(urabuf_clrf = 0, p = codeout; *p != EOLTTR;){
			switch(*p){
				case CHMSIG:
					p++;
			 /* codeoutの1バイト目に関数のスイッチとモード番号、
			    2バイト目にモードの新状態が入ってる */
					switch(SHUBET(*p)){
						case 0:
							chgmod((int)*p,
							   (modetyp)*(p + 1));
							break;
						case 1:
							allchgmod((modetyp)
								    *(p + 1));
							break;
						case 2:
							incmod((int)LWRMSK(*p)
							  ,(modetyp)*(p + 1));
							break;
						case 3:
							decmod((int)LWRMSK(*p)
							  ,(modetyp)*(p + 1));
							break;
					}
					ltrcpy(p, p + 2);
					 /* CHMSIGだけ残して1,2バイト目cut */

					urabuf_clrf = 1;
					break;
				case URBFCL:
				 /* urabufのクリアを明示的に指定する */
					urabuf_clrf = 1;
					ltrcpy(p, p + 1);
					break;
				default:
					p++;
			}
		}

		if(*codeout == ERRCOD){
			if(eofflg == 0){
				rk_errstat = 1;
				return(0);
			}

			 /* (error)であって、しかもeofflgが立ってたら、keybuf
			    の末尾まで、そのまま出す。*/
			codein_len = ltrlen(keybuf);
			codeout = ltrcpy(evalbuf[ebf_sw][0], keybuf);
			remainkbf = nil;
			rk_errstat = 2;

			return(1);
		}
	   /* (error)は単独でしか書けないので、エラー検出はこれで十分。*/

		if(*codeout == REASIG){
			*codeout = (rk_rst() != 0 ? EOLTTR : CHMSIG);
			 /* 再readでエラったらモードチェンジの通知はしない */

			urabuf_clrf = 1;
		}
	   /* 表の再read。但し、これが起こったことを外に知らせるのはCHMSIGで
	      このコードそのものは外へ出ない。(restart)は、(error)同様、
	      単独でしか書けないので、検出はこれで十分。*/

		return(urabuf_clrf ? 2 : 1);
	}

	 /* 表に現れていないコードはそのまま返す */

	codein_len = 1;
	*(codeout = oneletter) = *keybuf;
	remainkbf = nil;
	return(1);
}

 /* rk_rst内で使うマクロ */
#define taihi(X, Y, N) {for(i = 0; i < N; i++) X[i] = Y[i];}
#define recov(X, Y, N) taihi(Y, X, N)

 /**	表の動的再読み込みをする。現在の内部表現を全て退避し、前と同じ
	ディレクトリ（に、現在のところ限定）から表を読み込む。もし、
	読み込み中にエラーを検出すれば、もとの内部データを復活し非0を返す。*/
static int
rk_rst()
{
	register int	i;
	int	j;

	letter	memoryt[SIZALL];
	struct	dat	datat[LINALL];
	struct	hyo	hyo_nt[HYOMAX];
	letter	*hensudefhyot[VARTOT];
	struct	modestat	modeswt[MODMAX];
	char	hyoshut[HYOMAX];
	char	*modmeibgnt[MODMAX], modmeimem_t[MODMEI];
	char	*dspnambgnt[DMDMAX], dspcod_t[DMDCHR];
	int	usemaehyot[HYOMAX], usehyot[HYOMAX], useatohyot[HYOMAX];
	int	naibu_t[NAIBMX];
	char	*dspmodt[2][2];

	taihi(memoryt,		memory,		SIZALL);
	taihi(datat,		data,		LINALL);
	taihi(hyo_nt,		hyo_n,		HYOMAX);
	taihi(hensudefhyot,	hensudefhyo,	VARTOT);
	taihi(modeswt,		modesw,		MODMAX);
	taihi(hyoshut,		hyoshu,		HYOMAX);
	taihi(modmeibgnt,	modmeibgn,	MODMAX);
	taihi(modmeimem_t,	modmeimem_,	MODMEI);
	taihi(dspnambgnt,	dspnambgn,	DMDMAX);
	taihi(dspcod_t,		dspcod_,	DMDCHR);
	taihi(usemaehyot,	usemaehyo,	HYOMAX);
	taihi(usehyot,		usehyo,		HYOMAX);
	taihi(useatohyot,	useatohyo,	HYOMAX);
	taihi(naibu_t,		naibu_,		NAIBMX);
	for(i = 0; i < 2; i++)
		for(j = 0; j < 2; j++)	dspmodt[i][j] = dspmod[i][j];

	if(0 == romkan_init3(prv_modfnm, delchr, nisedl, delchr2,
			     keyin_method, bytcnt_method, kbytcnt_method, 1,
			     flags & ~RK_VERBOS))
		return(0); /* 正常終了 */

	recov(memoryt,		memory,		SIZALL);
	recov(datat,		data,		LINALL);
	recov(hyo_nt,		hyo_n,		HYOMAX);
	recov(hensudefhyot,	hensudefhyo,	VARTOT);
	recov(modeswt,		modesw,		MODMAX);
	recov(hyoshut,		hyoshu,		HYOMAX);
	recov(modmeibgnt,	modmeibgn,	MODMAX);
	recov(modmeimem_t,	modmeimem_,	MODMEI);
	recov(dspnambgnt,	dspnambgn,	DMDMAX);
	recov(dspcod_t,		dspcod_,	DMDCHR);
	recov(usemaehyot,	usemaehyo,	HYOMAX);
	recov(usehyot,		usehyo,		HYOMAX);
	recov(useatohyot,	useatohyo,	HYOMAX);
	recov(naibu_t,		naibu_,		NAIBMX);
	for(i = 0; i < 2; i++)
		for(j = 0; j < 2; j++)	dspmod[i][j] = dspmodt[i][j];
	return(1);
}

 /** lp2から評価して得た文字列をlp1にコピー */
static void
ltrevlcpy(lp1, lp2)
letter	*lp1, *lp2;
{
	while(*lp2 != EOLTTR){
		mchevl(&lp2, lp1);
		totail(lp1);
	}
	*lp1 = EOLTTR;
}

static void
set_rubout(lp, n, del)
 /** lpに 「del」n個の列をセットする。ここに del は 'delchr'か'nisedl' */
letter	*lp, del;
int	n;
{
	for(; n; n--) *lp++ = del;
	*lp = EOLTTR;
}

 /** これが実行されたらバグ。但し実行はそのまま続く */
void
BUGreport(n)
int	n;
{
	fprintf(stderr, "\r\nromkan-Bug%d!!\r\n", n);
}

 /** 前処理(mae_in→mae_out)又は後処理(ato_in→ato_out)を行う。*/
static void
maeato_henkan(in, outp, m_a_hyo)
letter	in;		/* 入力の一文字 */
letter	*outp;		/* 出力はここに入る */
int	*m_a_hyo;	/* どの前・後処理表が選択されているかの情報 */
{
	struct	dat	*datptr;
	int	i, hyoseq;
	letter	*curdat;

	if(isSPCL(in)){
	 /* LTREOFやCHMSIGが来うるので、特殊コードはそのまま返すように
	    細工しておかないといけない。*/
		ltr_to_ltrseq(outp, in);
		return;
	}

	for(hyoseq = 0; (hyonum = m_a_hyo[hyoseq]) != -1; hyoseq++){
		for(i = 0, datptr = hyo_n[hyonum] . data;
		    NULL != (curdat = datptr[i] . code[0]); i++){
			henmatch[0] . ltrmch = EOLTTR;
			if(!l_eq(curdat, in)) continue;

			ltrevlcpy(outp, datptr[i] . code[1]);
			return;
		}
	}
	ltr_to_ltrseq(outp, in);
}

 /** 後処理 */
static void
codeout_chg()
{
	letter	*saishu_outp;

	*(saishu_outp = saishu_out) = EOLTTR;

	for(; *codeout != EOLTTR; codeout++){
		maeato_henkan(*codeout, saishu_outp, useatohyo);
		totail(saishu_outp);
	}

	codeout = saishu_out;
}

 /** 一文字プッシュ・バック */
letter	romkan_ungetc(l)
letter	l;
{
	return(ungetc_buf = l);
}

 /** romkan_nextに対し一文字プッシュ・バック */
letter	romkan_unnext(l)
letter	l;
{
	return(unnext_buf = l);
}

 /**	deleteとして使うキャラクタの設定（偽deleteも）。これを実行後は
	romkan_clearを実行しておかないと混乱のもとになります。*/
/*	（廃止）
void
romkan_setdel(delchr_, nisedl_)
letter	delchr_, nisedl_;
{
	delchr = delchr_;
	nisedl = nisedl_;
}
*/
