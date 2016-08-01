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
			rk_read.c
						88. 6.11  改 正

	rk_main.c の部品。変換対応表の読み込みを担当。
***********************************************************************/
/*	Version 3.0
 */
#include "rk_header.h"
#include "rk_extvars.h"
#include <sys/types.h>
#include <sys/stat.h>

letter	onescan(), *rangekettei();
char	*ename();

extern	letter	*ltrgrow(), *ltrend(), letterpick(), *ltr1cut();
extern	char	*chrcat(), *strend();

static int termsscan(), evalandcpy(), eval1cpy(), partscan(),
  evlis(), atermscan(), serfun(), hensrc_tourk(), blankpass(),
  modsrcL(), chkL_get_int();
static void ERRLIN(), ERHOPN(), readhyo(),
  ltr1tostr(), ERRHYO(), vchk(), rangeset(), de_bcksla(), 
  listscan(), singleqscan(), doubleqscan();
extern void BUGreport(), choosehyo(), readmode();
extern int ltrcmp(), ltrstrcmp(), readfnm(), fixednamep();

#define IHENSU (1 << 24) /* 内部表現の上位１バイトで、変数を表す */
#define IKANSU (2 << 24) /*	      〃	      関数を表す */
#define IMODNM (3 << 24) /*	      〃	      モード名を表す */

#define ENDOF_NestFileArea ((FILE *)~0)
	 /** includeファイルのディスクリプタ格納エリアのエンドマーク*/


 /**	一行読み込み。一字も読み込まないうちにEOFにつきあたったら0を返す。
	但しincludeを考慮している。対応表には空白文字以外のコントロール文字は
	生では混じらないものとし、混じっていたらチェックする。*/
int
readln(buf)
uns_chr *buf;
{
	register uns_chr *p;
	register int	c;

	if(NULL == *base){
		*buf = '\0';
		return(0);
	}

	p = buf;
	while(1){
		while(EOF == (c = getc(*base))){
			fclose(*base);
			if(NULL == *--base){
				c = EOF;
				break;
			}
		}
		if(c == EOF || c == '\n') break;

		if(is_cntrl(c) && !isspace(c)){
			sprintf(hcurread, "\\%03o", c);
			ERRLIN(21);
		}
		*p++ = c;
	}
	*p = '\0';
	return(p != buf || c != EOF);
}

letter	*memptr, *ltrbufptr, *hensumei, *hen_iki, *term, **henptr;
letter	*dummy;
struct	dat	*datptr;
struct	hensuset
{
	unsigned	regdflg : 1;	/* 既登録の変数を表す */
	unsigned	curlinflg : 1;	/* 現在行に既出の変数を表す */
	unsigned	constflg : 1; /* 定数として定義されたことを表す */
	letter		*name;
	letter		*range;
} *hentourkptr, *henorg;

 /** 与えられたファイルがディレクトリなら非0を返す */
int
isdir(fnm)
char	*fnm;
{
	struct	stat	statbuf;
/*
	return(stat(fnm, &statbuf) == 0 && (statbuf.st_mode & S_IFDIR));
*/
	return(stat(fnm, &statbuf) == 0 && ((statbuf.st_mode & S_IFMT) == S_IFDIR));
}

 /**	nameで与えられた名の変換対応表をオープンする。固定名と見なされない
	名前（fixednamep()参照）に対しては、サーチパスを見る。その場合、
	オープンに成功したら、パス名を *dirnamptrに入れて返る。その他の場合は
	*dirnamptrに空文字列が入る。*errcodには次のエラーコードが入る。
	与えられたファイルがディレクトリの時1（サーチパスを探した時は3）、
	ファイルをオープンできない時2（同4）。*/
FILE	*trytoopen(name, dirnamptr, errptr)
char	*name, **dirnamptr;
int	*errptr;
{
	char	**src, taio_fnm[REALFN];
	FILE	*opened;

	*dirnamptr = nulstr;
	*errptr = 0;

	if(fixednamep(name)){
		if(isdir(name)){
			*errptr = 1;
			return(NULL);
		}
		if(NULL == (opened = fopen(name, "r"))){
			*errptr = 2;
			return(NULL);
		}
		if(flags & RK_VERBOS){
			fprintf(stderr, "romkan: using Taio-hyo %s ...\r\n",
				name);
		}
		return(opened);
	} else {
		for(src = pathmeiorg; *src != NULL; src++){
			strcpy(taio_fnm, (*dirnamptr = *src));
			strcat(taio_fnm, name);

			if(isdir(taio_fnm)){
				*errptr = 3;
				return(NULL);
			}
			if(NULL == (opened = fopen(taio_fnm, "r"))) continue;

			 /* Now Taio-hyo found */
			if(flags & RK_VERBOS)
				fprintf(stderr,
					"romkan: using Taio-hyo %s ...\r\n",
					taio_fnm);
			return(opened);
		}
		if(flags & RK_VERBOS){ /* Taio-hyo not found */
			char	*p, *q;

			fprintf(stderr, "no %s in", name);
			for(src = pathmeiorg; *src != NULL; src++){
				fputc(' ', stderr);
				q = *src;
				if(*q == KUGIRI && *(q + 1) == '\0') q++;
				 else q = strend(q);
				for(p = *src; p < q; p++)
					fputc(*p, stderr);
			}
			fprintf(stderr, ".\n");
		}
		*dirnamptr = nulstr;
		*errptr = 4;
		return(NULL);
	}
}

 /** 表全部の読み込み */
void
readdata(memory, data, hensudefhyo, modf)
letter	*memory;	/* 対応表の内部表現を入れる配列の先頭番地を渡される */
struct	dat	*data;	/* 対応表の行ごとのデータを入れる配列の番地 */
letter	**hensudefhyo;	/* 変数定義のデータを入れる配列の番地 */
char	*modf;		/* モード定義表の名又はそのパス名が入っている */
{
	int	i, j;
	char	*hyomeibgn[HYOMAX];
	 /* 表の名へのポインタを入れる配列 */
	char	hyomeimem_[HYOMEI];
	 /* 表の名の実際の文字列を入れる配列 */
	char	*pathmeibgn[PTHMAX];
	 /* サーチパス名へのポインタを入れる配列 */
	char	pathmeimem_[PTHMEI];
	 /* サーチパス名の実際の文字列を入れる配列 */
	char	modfnm[REALFN];
	 /* モード表のファイル名をここへコピる のちにはそのパス名が入る */

	memptr = memory;
	datptr = data;
	henptr = hensudefhyo;
	*(pathmeiptr = pathmeiorg = pathmeibgn) = NULL;
	*(pathmeimem = pathmeimem_) = '\0';
	*(hyomeiptr = hyomeiorg = hyomeibgn) = NULL;
	*(hyomeimem = hyomeimem_) = '\0';
	*(modmeiptr = modmeibgn) = NULL;
	*(modmeimem = modmeimem_) = '\0';
	*(dspnamptr = dspnambgn) = NULL;
	*(dspcod = dspcod_) = '\0';
	*(naibu = naibu_) = '\0';

	strcpy(modfnm, modf);
	     /* modfnmが与えたファイル名。パス名だけ与えられたら、デフォルトの
		モード表名"mode"をつなぐ。KUGIRI＝ディレクトリの区切り文字 */

	if(*modfnm == '\0' || *(strend(modfnm)) == KUGIRI){
		strcat(modfnm, "mode");
	} else if(isdir(modfnm)){
		chrcat(modfnm, KUGIRI);
		strcat(modfnm, "mode");
	}

	curdir = nulstr;
	readmode(curfnm = modfnm);

	for(i = 0; hyomeiorg[i] != NULL; i++){
		int	err;

		for(j = 0; j < FILNST; j++) nestfile[j] = NULL;
		nestfile[FILNST - 1] = ENDOF_NestFileArea;
		base = nestfile + 1;

		*base = trytoopen(curfnm = hyomeiorg[i], &curdir, &err);
		if(err){
			switch(err){
				case 1:
				case 3: ERHOPN(1);
				case 2:
				case 4: ERHOPN(0);
			}
		}
		readhyo(i);
	}

	hyo_n[i] . data = NULL;
	choosehyo();
}

 /** ファイル名からパス名を除いた部分の先頭を返す。*/
char	*ename(s)
char	*s;
{
	char	*p;

	p = strrchr(s, KUGIRI);
	return(p == NULL ? s : p + 1);
}

 /**	ファイル名のチェック。先頭（パス名は除く）が'1'〜'3'でないといけない。
	正当なものなら1〜3（前・本・後処理表の区別を表す）を返し、そうでないと
	0を返す。*/
int
filnamchk(s)
char	*s;
{
	char	c;

	c = *(ename(s)) - '0';
	return((1 <= c && c <= 3) ? c : 0);
}

 /**	linbufに入っているunsigned charの列をletterの列にしてltrbufに入れる。
	エンドマークはEOLTTRになる。flgが非0の時は、先頭の空白文字は飛ばす。*/
void
ustrtoltr(linbuf, ltrbuf, flg)
uns_chr *linbuf;
int	flg;
register letter *ltrbuf;
{
	register letter l;

	if(flg){
		while(l = letterpick(&linbuf), is_eolsp(l)){
			if(l == EOLTTR){
				*ltrbuf = EOLTTR;
				return;
			}
		}
		*ltrbuf++ = l;
	}
	while((*ltrbuf++ = letterpick(&linbuf)) != EOLTTR);
}

 /**	letterの列を文字列にコンバート。二つのポインタが同一番地であっても
	動作を保証すること。*/
void
ltrtostr(ltrbuf, linbuf)
char	*linbuf;
letter	*ltrbuf;
{
	letter	l;

	while((l = *ltrbuf++) != EOLTTR) ltr1tostr(l, &linbuf);
	*linbuf = '\0';
}

 /** letterを文字列にコンバート */
static void
ltr1tostr(l, sptr)
char	**sptr;
letter	l;
{
	int	i;

	for(i = 0; i < 3 && 0 == (l & (0xff000000)); i++) l <<= 8;
	for(; i < 4; i++){
		*(*sptr)++ = (char)(l >> 24);
		l <<= 8;
	}
}

 /** 変数の「現在行既出フラグ」を全てクリア */
void
hen_useflgclr(hensu)
struct	hensuset	*hensu;
{
	for(; hensu -> name != NULL; hensu++)
		hensu -> curlinflg = 0; /* 現在行に未出 */
}

 /** 対応表一つ読み込み */
static void
readhyo(n)
int	n;
{
	uns_chr linbuf[LINSIZ];
	letter	ltrbuf[LINSIZ], *lp;
	letter	termbuf[SIZALL]; /* エラーを考慮して、表のサイズ分取っておく*/
	letter	dummybuf[TRMSIZ], hensumei_[VARLEN], hen_iki_[SIZALL];
	struct	hensuset	hensu[VARMAX];
	int	m, hyosw, rsltyp[3];

	hcurread = (char *)linbuf; /* エラー処理用 */
	ltrbufbgn = ltrbuf;

	*(hensumei = hensumei_) = EOLTTR;
	*(hen_iki = hen_iki_) = EOLTTR;
	(henorg = hentourkptr = hensu) -> name = NULL;
	dummy = dummybuf;

	hyo_n[n] . hensudef = henptr;
	hyo_n[n] . data = datptr;

	hyosw = hyoshu[n];
	while(readln(linbuf)){
		hen_useflgclr(henorg);
		ustrtoltr(linbuf, ltrbuf, 1);
		ltrbufptr = ltrbuf;

		for(m = 0; termsscan(&ltrbufptr, term = termbuf, 1); m++){
			 /* mは、何番目の項目を見ているかを表す */
			if(*term == ';') break; /* 注釈行 */
			if(m == 3) ERRLIN(15);
			if(m != 0 && rsltyp[0] == 4) ERRLIN(12);

			datptr -> code[m] = memptr;
			if((rsltyp[m] = evalandcpy(&term, m)) == 4){
				if(m) ERRLIN(14);
			 /* 宣言は最初の項目にしか来れない。
			    funstr[]のappearフラグでもチェックしているが
			    将来のために一応ここにもチェックを入れておく。*/
			} else {
			 /* 宣言の時には内部表現へのポインタは進めない */
				totail(memptr);
				memptr++;
			}
		}

		if(m != 0 && rsltyp[0] != 4){
			for( /* m=? */ ; m < 3; m++){
				datptr -> code[m] = nil;
				rsltyp[m] = -1; /* doesn't exist */
			}
			datptr++;

 /* rsltyp: 0=文字項 1=文字列連 2=データ連 3=機能項 4=宣言項 -1=存在せず */
			switch(hyosw){
		   /* 前・後処理は、表の内容に制限がある。それを検査 */
				case 1:
					if(!(rsltyp[0] == 0 &&
					     rsltyp[1] == 0 && 
					     rsltyp[2] == -1
					    ))
						ERRLIN(17);
					break;
				case 2:
					if(rsltyp[1] == 3 && rsltyp[2] != -1)
						ERRLIN(19);
					break;
				case 3:
					if(!(rsltyp[0] == 0 &&
					     (rsltyp[1] == 0 ||
					      rsltyp[1] == 1) &&
					     rsltyp[2] == -1
					    ))
						ERRLIN(18);
					break;
				default:
					BUGreport(10);
			}

		}
	}

	(datptr++) -> code[0] = NULL;

		/* 変数登録ここでまとめてする */
	for(lp = hen_iki_; lp < hen_iki; ) *memptr++ = *lp++;
	for(hentourkptr = henorg; hentourkptr -> name != NULL;
	    hentourkptr++){
		if(hentourkptr -> regdflg == 0) ERRHYO(0);
		*henptr++ = memptr - (lp - hentourkptr -> range);
	}
	*henptr++ = NULL;

	/* ここで fclose(*base); は不要。readln内ですんでいる */
}

 /**	変換対応表の項目一つを、解釈して、内部形式のデータエリアにコピーする。
	返す値は、解釈した項目が文字項なら0、それ以外で文字列連なら1、それ以外
	でデータ連なら2、機能項なら3、宣言項なら4。それ以外はエラー。*/
static int
evalandcpy(socp, m)
register letter **socp; /* 項目へのポインタ（へのポインタ）*/
int	m;   /* 対応表の何番目の項目を見ているかを表す。入力コード部を
		見ているときは0、出力コード部なら1、バッファ残り部なら2 */
{
#define TYPMAX	5

	char	exist[TYPMAX], total;
	 /* existは、それぞれタイプ0〜4の項の出現のフラグ（eval1cpy()参照）。
	    totalは全体としての出現フラグ。どちらも、1=未出 2=一回出 その他の
	    時は下2ビット0 */
	int	type;

#define TIME_0(flag) ((flag) == 1)
#define TIME_1(flag) ((flag) == 2)
#define TIME_n(flag) (((flag) & 3) == 0)

	total = 1;
	for(type = 0; type < TYPMAX; type++) exist[type] = 1;

	while(!is_eolsp(**socp)){
		if(!(0 <= (type = eval1cpy(socp, m, 0)) && type < TYPMAX))
			BUGreport(3);
		exist[type] <<= 1;
		total <<= 1;
	}

	if(TIME_0(total))
		BUGreport(13); /* 項目が空ならevalandcpyは実行しない筈 */
	if(!TIME_0(exist[3]) || !TIME_0(exist[4])){
		if(TIME_n(total)) ERRLIN(9);
		return(type); /* 3又は4。typeが値を保持している筈 */
	}
	if(TIME_1(total) && TIME_1(exist[0])) return(0);
	return(!TIME_0(exist[2]) ? 2 : 1);
}

 /**	対応表の項目の中の項一つを解釈し、内部形式のデータエリアにコピーし、
	それのタイプ（文字項=0 文字列項=1 特殊関数項=2 機能項=3 宣言項=4）を
	返す。flgが非0なら、再帰的に呼ばれたことを意味し、その場合、
	現在行に未出の変数を検出したらエラー。また、mが非0のとき（入力コード部
	以外の所を見ている時）も、現在行に未出の変数を検出したらエラー。*/
static int
eval1cpy(socp, m, flg)
letter	**socp; /* flg以外の引数の意味はevalandcpyと同じ */
int	m, flg;
{
	letter	t1buf[TRMSIZ], *t1bufp;

	t1bufp = t1buf;

	*memptr = EOLTTR;
	switch(partscan(socp, t1bufp)){
		case 1: /* 単文字 */
			memptr = ltrgrow(memptr, t1bufp);
			*memptr = EOLTTR;
			return(0);
		case 2: /* 引用文字 */
			t1bufp++;
			*memptr++ = onescan(&t1bufp, dummy);
			*memptr = EOLTTR;
			return(0);
		case 3: /* 引用文字列 */
			t1bufp++;
			while(*t1bufp != '"'){
				*memptr++ = onescan(&t1bufp, dummy);
			}
			*memptr = EOLTTR;
			return(1);
		case 0: /* リスト */
			return(evlis(m, &t1bufp, flg));
			 /* evlis内で *memptr = EOLTTR; をしている。*/
		default:
			BUGreport(4);
			return(-1);
			 /*NOTREACHED*/
	}
	/*NOTREACHED*/
}

#define bitchk(x, n) ((x) & (1 << (n)))

#define get_ltr(lp) (*(lp)++)
#define unget_ltr(l, lp) (*--(lp) = (l))

 /** globalなポインタから指されているletter列から一文字取ってくる。*/
letter	get1ltr()
{
	return(get_ltr(lptr));
}

letter	unget1ltr(l)
letter	l;
{
	return(unget_ltr(l, lptr));
}

int int_get1ltr() {return((int)(get1ltr()));}
int int_unget1ltr(c) letter c; {return((int)(unget1ltr((letter) c)));}
 /** 汚いことこの上なし！なぜ関数の型のcastができないの？
    「(int ()) get1ltr」と書きたい！ */

 /**	includeファイル名のletter列をstringに取り出す作業を、letter列の終わり
	まで続ける。flg & 01が非0なら、'/'でも終了。*/
letter	getfrom_dblq(socp, destp, flg)
letter	**socp;
char	**destp;
int	flg;
{
	letter	l;

	while(**socp != EOLTTR && !(flg & 01 && **socp == KUGIRI)){
		if (**socp == '\\') *(*destp)++ = '\\';
		l = onescan(socp, dummy);
		ltr1tostr(l, destp);
	}
	*(*destp)++ = '\0';
	return(**socp);
}

int
getfrom_lptr(sptr, flg)
char	**sptr;
int	flg;
{
	return((int)getfrom_dblq(&lptr, sptr, flg));
}

 /**	リストを解釈して内部表現にする。返値は、そのリストのタイプを表す数。
	文字変数項又は文字関数項:0 文字列関数項:1 特殊関数項:2
	機能項:3 宣言項:4 */
static int
evlis(m, socp, flg)
letter	**socp; /* 引数の意味はeval1cpyを参照 */
int	m, flg;
{
	int	fnnum, hennum, i;
	letter	t1buf[TRMSIZ];

	(*socp)++; /* '('をスキップ */
	atermscan(socp, t1buf, 3);

	fnnum = serfun(t1buf);
	if(fnnum != -1 && !bitchk(func[fnnum] . appear, m)) ERRLIN(14);
	 /* mの値によって、現れてはいけない所への出現かどうか見ている。*/

	switch(fnnum){ /* defaultの所以外は func[fnnum].argnumを使ってない */
		case -1: /* 変数 */
			vchk(t1buf);
			atermscan(socp, dummy, 2); /* あればERR */
			hennum = hensrc_tourk(t1buf, ((m==0 && !flg)? 0 : 1));
			*memptr++ = (henorg[hennum] . constflg ? 
				     *(henorg[hennum].range) : hennum|IHENSU);
			break;

		case 0: /* fn No.0 defvar */
			atermscan(socp, t1buf, 3);
			if(*t1buf == '('){
				letter	*soc2, t1buf2[TRMSIZ], t1buf3[TRMSIZ];
				letter	*common_hen;

				atermscan(socp, t1buf3, 3);

				soc2 = t1buf + 1; /* skip '(' */

				atermscan(&soc2, t1buf2, 3);
				vchk(t1buf2);
				if(-1 != serfun(t1buf2)) ERRLIN(11);
				hennum = hensrc_tourk(t1buf2, 2);
				common_hen = rangekettei(hennum, t1buf3);

				while(atermscan(&soc2, t1buf2, 0)){
					vchk(t1buf2);
					if(-1 != serfun(t1buf2)) ERRLIN(11);
					hennum = hensrc_tourk(t1buf2, 2);
					rangeset(hennum, common_hen);
				}
			} else {
				vchk(t1buf);
				if(-1 != serfun(t1buf)) ERRLIN(11);
				hennum = hensrc_tourk(t1buf, 2);
			  /* defvar・defconstの変数名の重複を避けるため */
				atermscan(socp, t1buf, 3);
				rangekettei(hennum, t1buf);
			}
			atermscan(socp, dummy, 2);
			break;
		case 36: /* fn No.36 defconst */
			atermscan(socp, t1buf, 3);
			vchk(t1buf);
			if(-1 != serfun(t1buf)) ERRLIN(11);

			hennum = hensrc_tourk(t1buf, 6);
			  /* defvar・defconstの変数名重複を避けるため */

			rangeset(hennum, hen_iki);

			blankpass(socp, 1);
			if(*(*socp)++ != '\'') ERRLIN(8);
			*hen_iki++ = onescan(socp, dummy);
			(*socp)++; /*「'」が閉じていることの検査は済んでいる*/
			*hen_iki++ = EOLTTR; /* needed? */
			*hen_iki = EOLTTR;

			atermscan(socp, dummy, 2);
			break;

		case 1: /* fn No.1 include */
			{
				char	fnmtmparea[REALFN], *s, *dirnamptr;
				int	dummyc = 0, err;

				blankpass(socp, 1);
				if(3 != partscan(socp, t1buf)) ERRLIN(22);
				atermscan(socp, dummy, 2);
				 /* 余分にあればERR */

				ltr1cut(lptr = t1buf + 1);
				*(s = fnmtmparea) = '\0';
				err = readfnm(int_get1ltr, int_unget1ltr,
					      getfrom_lptr, &s, &dummyc);

				if(err){
					hcurread = s;
					switch(err){
						case 1:
						case 3: ERRLIN(25);
						case 2: ERRLIN(26);
						case 4: ERRLIN(27);
					}
				}
				de_bcksla(fnmtmparea, fnmtmparea);

				if(*++base == ENDOF_NestFileArea){
					base--;
					ERRLIN(23);
				}
				*base= trytoopen(fnmtmparea, &dirnamptr,&err);
				if(err){
					switch(err){
						case 1:
						case 3:
						case 2:
						case 4: base--;
							ERRLIN(24);
					}
				}
			}
			break;

		 /* モード名一つを引数に取るもの */
		case 4: /* fn No.4〜6 off,on,switch */
		case 5:
		case 6:
		case 20: /* fn No.20,21 if,unless */
		case 21:
			*memptr++ = fnnum | IKANSU;
			atermscan(socp, t1buf, 3);
			*memptr++ = modsrcL(t1buf) | IMODNM;
			atermscan(socp, t1buf, 2);
			break;

		 /* モード名と文字 一つずつを引数に取るもの */
		case 37: /* fn No.37〜43 setmodeなど */
		case 38:
		case 39:
		case 40:
		case 41:
		case 42:
		case 43:
			{
				int	err, n;
				modetyp	stat;

				*memptr++ = fnnum | IKANSU;
				atermscan(socp, t1buf, 3);
				*memptr++ = (n = modsrcL(t1buf))| IMODNM;
				atermscan(socp, t1buf, 3);
				err = chkL_get_int(t1buf, &stat,
							 modesw[n] . moderng);
				if(err != 0) ERRLIN(29);
				*memptr++ = stat;
				atermscan(socp, t1buf, 2);
				break;
			}

		 /* 普通（引数を取らないものを含む） */
		default: /* toupper,tolower… */
			*memptr++ = fnnum | IKANSU;
			*memptr = EOLTTR;
			for(i = 0; i < func[fnnum] . argnum; i++){
				blankpass(socp, 1);
				if(eval1cpy(socp, m, 1) != 0) ERRLIN(13);
			}
			atermscan(socp, dummy, 2); /* 余分にあればERR */
			break;
	}
	*memptr = EOLTTR;
	return(fnnum == -1 ? 0 : func[fnnum] . fntype);
}

 /** 文字列中の「\」を抜く */
static void
de_bcksla(s, r)
char	*s, *r;
{
	for(; *s; *r++ = *s++) if(*s == '\\') s++;
	*r = '\0';
}

 /**	defvarの第二引数（shikiに入る）を解釈して、その変数の変域を決定する。
	変数列の最後にエンドマークもちゃんと入る。返値は、変域を格納した
	所へのポインタ。*/
letter	*rangekettei(num, shiki)
letter	*shiki;
int	num; /* 変域を決定しつつある変数の内部番号 */
{
	letter	hyoukabuf[TRMSIZ];

	rangeset(num, hen_iki);
	*hen_iki = EOLTTR;

	if(*shiki++ != '(') ERRLIN(8);
	atermscan(&shiki, hyoukabuf, 1);

	if(!ltrstrcmp(hyoukabuf, "all")){
		*hen_iki++ = VARRNG;
		*hen_iki++ = 0;
		*hen_iki++ = LTRHUG; /* 変域は全文字 */
		*hen_iki++ = EOLTTR;
		*hen_iki = EOLTTR;
		atermscan(&shiki, dummy, 2); /* 余分にあればERR */
	} else 
	if(!ltrstrcmp(hyoukabuf, "between")){
		int	i;

		*hen_iki++ = VARRNG;
		while(blankpass(&shiki, 1), *shiki != ')'){
			for(i = 1; i <= 2; i++){
				switch(*shiki){
					case '\'':
						shiki++;
						*hen_iki++ =
						       onescan(&shiki, dummy);
						shiki++;
						break;
					case ')':
					case '"':
					case '(': ERRLIN(8);
					default:
						*hen_iki++ = *shiki++;
				}
				if(i < 2){
					if(!is_eolsp(*shiki)) ERRLIN(8);
					blankpass(&shiki, 1);
				}
			}
		}
		*hen_iki++ = EOLTTR;
		*hen_iki = EOLTTR;
	} else
	if(!ltrstrcmp(hyoukabuf, "list")){
		while(blankpass(&shiki, 1), *shiki != ')'){
			switch(*shiki){
				case '"':
				case '(': ERRLIN(8);
				case '\'':
					shiki++;
					*hen_iki++ = onescan(&shiki, dummy);
					shiki++; /* 本当に「'」が閉じているか
					 どうかの検査はもう済んでいる。*/
					break;
				default:
					*hen_iki++ = *shiki++;
			}
			if(!is_eolsp(*shiki)) ERRLIN(8);
		}
		*hen_iki++ = EOLTTR;
		*hen_iki = EOLTTR;
	} else {
		ERRLIN(8); /* 将来はこの他の構文も許す予定であった */
	}

	return(henorg[num] . range);
}

 /**	num番目の変数の変域を指すべきポインタの指し先を決定し、その変数を
	既登録状態にする。*/
static void
rangeset(num, range)
letter	*range; /* 変域の入る場所のポインタ */
int	num;
{
	henorg[num] . range = range;
	henorg[num] . regdflg = 1;
}

 /**	nameで指定された名の変数を探し、なければ登録。変数名の最後に
	エンドマークもちゃんと入る。
	flg & 01が非0の時、その名の変数が現在行に未出ならエラー（constとして
	既登録の時を除く）。また flg & 02が非0の時、その名の変数が既定義なら
	エラー（defvarの重複チェック用）。flg & 04が非0ならconstとして登録。*/
static int
hensrc_tourk(name, flg)
letter	*name;
int	flg;
{
	int	i;

	for(i = 0; henorg[i] . name != NULL; i++){
		if(ltrcmp(henorg[i] . name, name)) continue;
		 /* found */
		if(flg & 04) ERRLIN(28);
		if(flg & 02 && henorg[i] . regdflg != 0) ERRLIN(10);
		if(flg & 01 && henorg[i] . curlinflg == 0 &&
				 henorg[i] . constflg == 0)
			ERRLIN(5);
		henorg[i] . curlinflg = 1;
		return(i);
	}
	if(henorg + i != hentourkptr) BUGreport(5);

	 /* ここへ来たということは、初出の変数ということ。当然、flg & 01が
	    非0ならエラー。*/
	if(flg & 01) ERRLIN(5);
	hentourkptr -> name = hensumei;
	hentourkptr -> curlinflg = 1;
	hentourkptr -> regdflg = 0; /* 初出だからrangeのdefは未だの筈 */
	hentourkptr -> constflg = ((flg & 04) != 0);
	(++hentourkptr) -> name = NULL;
	hensumei = ltrgrow(hensumei, name);
	*++hensumei = EOLTTR;

	return(i);
}

 /** 組み込み関数・機能名に対してはその番号を、そうでないものなら-1を返す */
static int
serfun(lp)
register letter *lp; /* 関数・機能名もしくは変数名 */
{
	register int	i;

	for(i = 0; func[i] . fnname != NULL; i++){
		if(! ltrstrcmp(lp, func[i] . fnname)) return(i);
	}
	return(-1);
}

 /** 変数の名前のチェック おかしいとエラー */
static void
vchk(lp)
letter	*lp;
{
	if(is_digit(*lp)) ERRLIN(3);
	for(; *lp != EOLTTR; lp++){
	  /*	if(is_lower(*lp)) *lp = to_upper(*lp);	*/
		if(!is_alnum(*lp) && *lp != '_') ERRLIN(3);
	}
}

 /**	一項目を取り出す。取り出しに成功すると1を返す。flgが非0の時は、')'が
	見つかるとエラー、';'はそれだけで一項目扱い。*/
static int
termsscan(socp, dest, flg)
register letter **socp, *dest;
	 /* socpの指しているポインタが指している所から取り出してdestに入れる。
	    その後、socpが指しているポインタを進める。このファイルの **scan()
	    という関数は全てそうなってる。*/
int	flg;
{
	letter	*bgn;

	bgn = dest;

	if(blankpass(socp, 0) == 0){
		if(flg && **socp == ';'){
			*dest++ = *(*socp)++;
		} else
		while(!is_eolsp(**socp)){
			if(**socp == ')'){
				if(flg) ERRLIN(0);
				break;
			} else {
				partscan(socp, dest);
				totail(dest);
			}
		}
	}

	*dest = EOLTTR;
	return(bgn != dest);
}

 /**	リスト一つか、単純項の一まとまりを取り出す。成功したら1を返す。
	flgが1のとき、')'が見つかるとエラー。
	flgが2のとき、取り出しに成功したらエラー。
	flgが3のとき、取り出しに失敗したらエラー。*/
static int
atermscan(socp, dest, flg)
register letter **socp, *dest;
int	flg;
{
	letter	*bgn;
	int	found;

	bgn = dest;

	if(blankpass(socp, 0) == 0){
		if(**socp == '('){
			listscan(socp, dest);
			totail(dest);
		} else {
			while(!is_eolsp(**socp) && **socp != '('){
				if(**socp == ')'){
					if(flg == 1) ERRLIN(0);
					break;
				} else {
					partscan(socp, dest);
					totail(dest);
				}
			}
		}
	}

	*dest = EOLTTR;

	found = (bgn != dest);
	if((!found && flg == 3) || (found && flg == 2)) ERRLIN(7);
	return(found);
}

 /**	項一つを取り出す。取り出したものがリストなら返値は0、単文字なら1、
	引用文字なら2、引用文字列なら3。*/
static int
partscan(socp, dest)
register letter **socp, *dest;
{
	switch(**socp){
		case '(':
			listscan(socp, dest);
			return(0);
		case '\'':
			singleqscan(socp, dest);
			return(2);
		case '"':
			doubleqscan(socp, dest);
			return(3);
		default:
			*dest++ = *(*socp)++;
			*dest = EOLTTR;
			return(1);
	}
}

 /** シングルクォート表現一つを取り出す。*/
static void
singleqscan(socp, dest)
letter	**socp, *dest;
{
	*dest++ = *(*socp)++;
	onescan(socp, dest);
	totail(dest);
	if((*dest++ = *(*socp)++) != '\'') ERRLIN(1);

	*dest = EOLTTR;
}

 /** ダブルクォート表現一つを取り出す。*/
static void
doubleqscan(socp, dest)
letter	**socp, *dest;
{
	*dest++ = *(*socp)++;
	while(**socp != '"'){
		if(**socp == EOLTTR) ERRLIN(1);
		onescan(socp, dest);
		totail(dest);
	}
	*dest++ = *(*socp)++;

	*dest = EOLTTR;
}

 /**	8・10・16進コード用キャラクタを実際のコードに直す。入力のチェックは
	せず、英文字と数字以外の入力に対しては単に0を返す。*/
int
ltov(l)
letter	l;
{
	if(is_upper(l)) return(l - 'A' + 10);
	if(is_lower(l)) return(l - 'a' + 10);
	if(is_digit(l)) return(l - '0'); else return(0);
}

 /** ltovの逆 */
letter	vtol(l)
letter	l;
{
	if(BASEMX <= l) return('*');
	return(l + (l < 10 ? '0' : 'A' - 10));
}

 /**	シングル・ダブルクォートの中での一文字取り出し。
	「^」によるコントロールコード表現、「\」による8・10・16進表現にも
	対応。「\」表現は、「\［o又はd又はx］数字の並び［;］」である。*/
letter	onescan(socp, dest)
letter	**socp, *dest;
{
	letter	l, realcode;
	int	digflg;

	switch(realcode = *dest++ = *(*socp)++){
		case '^':
			if(!(' ' <= (l = *(*socp)++) && l < '\177'))ERRLIN(2);
			realcode = ((*dest++ = l) == '?' ? '\177' : l & 0x1f);
			break;
		case '\\':
			digflg = 0;
			switch(**socp){
				case 'n':
					*dest++ = *(*socp)++; realcode = '\n';
					break;
				case 't':
					*dest++ = *(*socp)++; realcode = '\t';
					break;
				case 'b':
					*dest++ = *(*socp)++; realcode = '\b';
					break;
				case 'r':
					*dest++ = *(*socp)++; realcode = '\r';
					break;
				case 'f':
					*dest++ = *(*socp)++; realcode = '\f';
					break;
				case 'e':
				case 'E':
					*dest++ = *(*socp)++; realcode=ESCCHR;
					break;
				case 'o':
					*dest++ = *(*socp)++;
					for(realcode = 0; is_octal(**socp);){
						digflg = 1;
						realcode <<= 3;
						realcode += ltov
						       (*dest++ = *(*socp)++);
					}
					if(!digflg) ERRLIN(2);
					if(**socp== ';') *dest++ = *(*socp)++;
					break;
				case 'x':
					*dest++ = *(*socp)++;
					for(realcode = 0; is_xdigit(**socp);){
						digflg = 1;
						realcode <<= 4;
						realcode += ltov
						       (*dest++ = *(*socp)++);
					}
					if(!digflg) ERRLIN(2);
					if(**socp== ';') *dest++ = *(*socp)++;
					break;
				case 'd':
					*dest++ = *(*socp)++;
					for(realcode = 0; is_digit(**socp);){
						digflg = 1;
						realcode *= 10;
						realcode += ltov
						       (*dest++ = *(*socp)++);
					}
					if(!digflg) ERRLIN(2);
					if(**socp== ';') *dest++ = *(*socp)++;
					break;
				default:
					if(is_octal(**socp)){
						for(realcode = 0;
						    is_octal(**socp);){
							realcode <<= 3;
							realcode += ltov(
							*dest++ = *(*socp)++);
						}
						if(**socp == ';')
							*dest++ = *(*socp)++;
					} else {
						realcode= *dest++= *(*socp)++;
					}
			}
			break;
		default: ;
	}

	*dest = EOLTTR;
	return(realcode);
}
	
 /**	letterの列の先頭にある空白をスキップする。
	行末に達したら、flgが0のときは1を返し、そうでないとエラー。*/
static int
blankpass(pptr, flg)
register letter **pptr;
	 /* letterの列のポインタへのポインタ。これが指しているものを進める */
int	flg;
{
	while(is_eolsp(**pptr)){
		if(EOLTTR == **pptr){
			if(flg) ERRLIN(4);
			return(1);
		}
		(*pptr)++;
	}
	return(0);
}

 /** リスト一個取り出し */
static void
listscan(socp, dest)
register letter **socp, *dest;
{
	int	eofreach;

	*dest++ = *(*socp)++; /* = '(' */
	*dest++ = ' ';
	
	while(eofreach = blankpass(socp, 0), **socp != ')'){
		if(eofreach){
			if(! readln((uns_chr *)hcurread)) ERRLIN(20);
			ustrtoltr((uns_chr *)hcurread, (*socp= ltrbufbgn), 1);
			 /* listの中で行が切れている場合、一行追加読み込みを
			    する。uns_chr用のバッファも、letter用のものも、
			    以前の物を先頭から再利用しているので注意。また、
			    エラーが起きた場合、 エラーの位置にかかわらず、
			    現在行として表示されるのは、最後に読まれた行のみ*/
		} else {
			termsscan(socp, dest, 0);
			totail(dest);
			*dest++ = ' ';
		}
	}
	*dest++ = *(*socp)++; /* = ')' */
	*dest = EOLTTR;
}

 /** lpで指定されたモード名を探す。見つからないとエラー */
static int
modsrcL(lp)
letter	*lp;
{
	int	n;

	for(n = 0; modmeibgn[n] != NULL; n++)
		if(!ltrstrcmp(lp, modmeibgn[n])) return(n);

	ERRLIN(16);
	 /*NOTREACHED*/
	return(-1);
}

 /** chk_get_int（rk_modread.c）のletter版 */
static int
chkL_get_int(lp, ip, range)
letter	*lp;
modetyp	*ip;
modetyp range;
{
	int	sgn = 1;
	modetyp	out;

	if(*lp == '-'){
		lp++;
		sgn = -1;
	}
	for(out = 0; *lp != EOLTTR; lp++){
		if(!is_digit(*lp)) return(-1);
#if defined(UX386) || defined(sun386)
		out = out * 10;
#else
		out *= 10;
#endif
		out += ltov(*lp);
	}
	if(range != 0) out %= range;
	if(sgn == -1 && out != 0) out = range - out;
	*ip = out;
	return(0);
}

 /**	この先は、表読み込みでエラった時に警告するルーチン。nはエラーコード。
	またこれらは全て、romkan_initのエラーコードとして、longjmp経由で
	1を返す。*/

void
ERMOPN(n) /* モード定義表がオープンできない */
unsigned int	n;
{
	static	char	*ermes[] = {
 /*  0 */	"Can't open Mode-hyo",
		"Unprintable error"
	};

	if(n >= numberof(ermes)) n = 1;

	fprintf(stderr, "\r\nMode-hyo %s ---\r\n", curfnm);
	fprintf(stderr, "%d: %s.\r\n", n, ermes[n]);
	longjmp(env0, 1);
}

static void
ERHOPN(n) /* 変換対応表がオープンできない */
unsigned int	n;
{
	static	char	*ermes[] = {
 /*  0 */	"Can't open Taio-hyo",
		"Is a directory",
		"Unprintable error"
	};

	if(n >= numberof(ermes)) n = 2;

	fprintf(stderr, "\r\nTaio-hyo %s%s ---\r\n", curdir, curfnm);
	fprintf(stderr, "%d: %s.\r\n", n, ermes[n]);
	longjmp(env0, 1);
}

void
ERRMOD(n) /* モード定義表のエラー */
unsigned int	n;
{
	static	char	*ermes[] = {
 /*  0 */	"Table incomplete",
		"')' mismatch",
		"Unprintable error",
		"Illegal filename",
		"Illegal modename",
 /*  5 */	"Undefined mode",
		"Illegal content(s) of list",
		"Illegal ^,\\o,\\x or \\d expression",
		"Illegal defmode",
		"Unrecognized keyword",
 /* 10 */	"Incomplete string",
		"Search path specified after filename",
		"Argument must be a string",
		"Can't get home directory",
		"Illegal @-keyword",
 /* 15 */	"User doesn't exist",
		"Illegal character",
		"Defmode or set-path placed wrong"
	};

	if(n >= numberof(ermes)) n = 2;

	fprintf(stderr, "\r\nMode-hyo %s%s ---\r\n%s\r\n",
		curdir, curfnm, mcurread);
	fprintf(stderr, "%d: %s.\r\n", n, ermes[n]);
	fclose(modefile);
	longjmp(env0, 1);
}

static void
ERRLIN(n) /* 変換対応表のエラー */
unsigned int	n;
{
	static	char	*ermes[] = {
 /*  0 */	"')' mismatch",
		"Incomplete single-quote or double-quote expression",
		"Illegal ^,\\o,\\x or \\d expression",
		"Illegal variable name",
		"Incomplete line",
 /*  5 */	"Evaluation of unbound variable",
		"Unprintable error",
		"Too many or too few contents of list",
		"Illegal defvar/defconst",
		"Faculity or declaration joined other item(s)",
 /* 10 */	"Duplicate defvar/defconst",
		"Variable/constant name conflicts with Function name",
		"A line contains both declaration and other output item(s)",
		"Argument isn't a letter",
		"Function, faculity or declaration in illegal place",
 /* 15 */	"More than 3 items",
		"Undefined mode",
		"Against the restriction of pre-transform table",
		"Against the restriction of after-transform table",
		"Item comes after faculity",
 /* 20 */	"Incomplete list",
		"Illegal character",
		"Illegal include",
		"Too many levels of 'include' nest",
		"Can't open include file",
 /* 25 */	"Can't get home directory",
		"Illegal @-keyword",
		"User doesn't exist",
		"Constant must be defined before used",
		"Illegal mode status"
	};

	if(n >= numberof(ermes)) n = 6;

	fprintf(stderr, "\r\nTaio-hyo %s%s ---\r\n%s\r\n",
		curdir, curfnm, hcurread);
	fprintf(stderr, "%d: %s.\r\n", n, ermes[n]);

	while(NULL != *base) fclose(*base--);

	longjmp(env0, 1);
}

static void
ERRHYO(n) /* ERRLINと同様、対応表のエラーだが、一行だけの誤りでなく
 全体を見ないとわからない誤り。今の所、「未定義変数の出現」のみ。*/
unsigned int	n;
{
	static	char	*ermes[] = {
 /*  0 */	"Undefined variable was found",
		"Unprintable error"
	};

	if(n >= numberof(ermes)) n = 1;

	fprintf(stderr, "\r\nTaio-hyo %s%s ---\r\n", curdir, curfnm);
	fprintf(stderr, "%d: %s.\r\n", n, ermes[n]);

	while(NULL != *base) fclose(*base--);

	longjmp(env0, 1);
}
