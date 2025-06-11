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
/**********************************************************************
			rk_modread.c
						88. 6.16  改 正

	モード定義表の読み込みを担当するプログラム。
***********************************************************************/
/*	Version 3.0
 */
#include "rk_header.h"
#include "rk_extvars.h"
#ifdef WNNDEFAULT
#  include "config.h"
 /* マクロLIBDIRの定義（のためだけ）。コンパイル時は、ヘッダファイルの
    サーチパスに、Wnnのインクルードファイルのありかを設定しておくこと。*/
#endif
#include <pwd.h>

#define Terminator 0 /* intの列（naibu[]）の終止コード */

char	*getenv();
#if defined(uniosu)
extern	struct	passwd	*getpwnam();
#endif
extern	char	*chrcat(), *strend(), *ename();
extern	void	romkan_clear();
char	*modhyopath;

static void cond_evl(), mystrcpy(),
  rd_bcksla(), rd_ctrl(), hyouse(), look_choose();
void choosehyo();
static int mystrcmp(), read1tm(), mod_evl(), fnmsrc_tourk(), dspnamsrc_tourk(),
  scan1tm(), modsrc_tourk(), chk_get_int(), pathsrc_tourk(),
  modnamchk(), ctov(), look_cond(), evlcond(), chkchar_getc(); 
static char codeeval();
extern void ERRMOD(), ERMOPN(), BUGreport();
extern int filnamchk();

struct	kwdpair {
	 /* キーワードとその内部表現の対応を与える構造体。内部表現を
	    持たないものに対しては0が与えられている。*/
	char	*name;
	int	code;
} modfn[] = {
	"defmode",	0,
	"if",		XY2INT(2, 0),
	"when",		XY2INT(2, 1),
	"path",		0,
	"search",	0,
	"on_dispmode",	XY2INT(5, 0),
	"off_dispmode", XY2INT(5, 1),
	"on_unchg",	XY2INT(6, 0),
	"off_unchg",	XY2INT(6, 1),
	NULL
}; /* 下を見よ キーワード定義表はまだあと二つあるのだ */

struct	kwdpair modcond[] = {
	"not",	XY2INT(3, 0),
	"and",	XY2INT(3, 1),
	"or",	XY2INT(3, 2),
	"true", XY2INT(3, 3),
	"false",XY2INT(3, 4),
	"=",	XY2INT(3, 5),
	"!=",	XY2INT(3, 6),
	"<",	XY2INT(3, 7),
	">",	XY2INT(3, 8),
	NULL
};
int	condarg[] = {1, 2, 2, 0, 0, 2, 2, 2, 2}; /* 条件判断関数の引数個数 */

struct	kwdpair swstat[] = {
	"on",	0,
	"off",	0,
	NULL
};
 /* 1×2^24代はモード名、4×2^24代はモード表示文字列、7×2^24代はモード状態定
    数の内部表現に使っている */

 /** キーワード（if, andなど）が正当なものかチェックし、その番号を返す */
static int
kwdsrc(hyo, wd)
struct	kwdpair *hyo;	/* どのキーワード表を使うか */
char	*wd;		/* チェックされるキーワード */
{
	int	i;

	for(i = 0; hyo[i] . name != NULL; i++)
		if(!mystrcmp(hyo[i] . name, wd)) return(i);
	ERRMOD(9);
	 /*NOTREACHED*/
}

 /** モード表の読み込み */
void
readmode(modfname)
char	*modfname; /* モード表の名 */
{
	char	buf[MDHMAX], *bufp;
#ifdef RKMODPATH
	char	*genv, *pathenv, *pathp;
#endif

	mcurread = buf; /* エラー処理用 */

#ifdef RKMODPATH
	if(!fixednamep(modfname) &&
	   NULL != (pathenv = genv = getenv(RKMODPATH)) && *genv != '\0'){
	 /* PATHに少なくとも一つのサーチパスがある場合 */
		for(;;){
			 /* サーチパスの各々を入れる領域には、pathmeimemを借用
			    している。後で、サーチパスの先頭に、モード表のある
			    ディレクトリを設定するので、その便宜のためもある*/
			for(pathp = pathmeimem; *genv != ':' && *genv; genv++)
				*pathp++ = *genv;
			*pathp = '\0';

			if(*(strend(pathmeimem)) != KUGIRI) *pathp++ = KUGIRI;
			 /* pathの区切りはDG（MV）であっても'/' */

			strcpy(pathp, modfname);
			if(NULL != (modefile = fopen(pathmeimem, "r"))){
				 /* Now Mode-hyo found */
				if(flags & RK_VERBOS)
					fprintf(stderr,
					"romkan: using Mode-hyo %s ...\r\n",
						pathmeimem);
				curdir = pathmeimem; /* この時点ではファイル名
					込みだが、あとでパス名だけになる */
				curfnm = ename(modfname);
				break;
			}

			if(*genv != ':'){ /* Mode-hyo not found */
				if(flags & RK_VERBOS){
					fprintf(stderr, "no %s in ",modfname);
					for(genv = pathenv; *genv; genv++){
						fputc((*genv == ':' ?
						      ' ' : *genv), stderr);
					}
					fprintf(stderr, ".\n");
				}
				ERMOPN(0);
			} else genv++; /* coutinues searching Mode-hyo */
		}
	} else
#endif
	{
		if(NULL == (modefile = fopen(modfname, "r"))) ERMOPN(0);
		if(flags & RK_VERBOS)
			fprintf(stderr, "romkan: using Mode-hyo %s ...\r\n",
				modfname);
		strcpy(pathmeimem, modfname);
	}

	 /* サーチパスの先頭に、モード表のあるディレクトリを設定している。*/
	*(ename(pathmeimem)) = '\0';
	modhyopath = *pathmeiptr++ = pathmeimem;
	*pathmeiptr = NULL;
	strtail(pathmeimem);
	*(pathareaorg = ++pathmeimem) = '\0';
	 /* pathareaorgは、pathmeimem_[]のうちモード表のあるディレクトリ名を
	    格納した残りの部分の先頭を指す。*/

	while(bufp = buf, read1tm(&bufp, 0)) mod_evl(buf);
	fclose(modefile);
}

 /**	固定された（サーチパスを見る必要のない）ファイル名に対しては非0を
	返す。現在のところ、 / ./ ../ のどれかで始まるものとしているが（作者の
	独断）、適当に変えてよい。strchr(s,'/')!=NULL とする方が良いかも */
int
fixednamep(s)
char	*s;
{
	return(!strncmp("/",s,1)|| !strncmp("./",s,2)|| !strncmp("../",s,3));
}

 /**	モード表の一かたまり（リスト、ファイル名、モード表示文字列）を
	解釈する。返す値は、defmode,search及びpathの時0、それ以外なら1。*/
static int
mod_evl(s)
char	*s; /* モード表の内部表現の列へのポインタ */
{
	char	md1[MDT1LN], *bgn, *end;
	int	num, retval = 1;

	if(*s != '('){
		if(*s != '"'){
			num = fnmsrc_tourk(s);
			*naibu++ = XY2INT(4, num);
		} else {
			s++;
			if(*(end = strend(s)) != '"') ERRMOD(10);
			*end = '\0';
			num = dspnamsrc_tourk(s);
			*naibu++ = XY2INT(5, 0);
			*naibu++ = num;
		}
	} else {
		s++;
		scan1tm(&s, md1, 1);
		switch(num = kwdsrc(modfn, md1)){
			case 0: /* defmode */
				retval = 0;
				scan1tm(&s, md1, 1); /* modename */
				num = modsrc_tourk(md1, 0);
				if(scan1tm(&s, md1, 0) == 0){
				 /* 初期on-offについて何も書いてない時の
				    defaultはoff */
					modesw[num] . moderng = 2;
					modesw[num] . curmode = 0;
					break;
				}

				if(*md1 == '('){
					char	tmp[MDT1LN], *s;
					unsigned int	i, j;

					s = md1 + 1;

					scan1tm(&s, tmp, 1);
					if(chk_get_int(tmp, &i, 0) != 0)
						ERRMOD(8);
					modesw[num] . moderng = i;
					scan1tm(&s, tmp, 1);
					if(chk_get_int(tmp, &j,
						  modesw[num] . moderng) != 0)
						ERRMOD(8);
					modesw[num] . curmode = j;
					if(
#ifdef ModeNotInt
					   modesw[num] . moderng != i ||
					   modesw[num] . curmode != j ||
#endif
					   i == 1 || (i != 0 && j >= i)){
					   	ERRMOD(8);
					}
					scan1tm(&s, tmp, 2);
				} else {
					switch(kwdsrc(swstat, md1)){
						case 0: modesw[num] . curmode
								= 1; break;
						case 1: modesw[num] . curmode
								= 0; break;
					}
					modesw[num] . moderng = 2;
				}
				scan1tm(&s, md1, 2); /* あればerr */
				break;
			case 1: /* if */
			case 2: /* when */
				*naibu++ = modfn[num] . code;
				scan1tm(&s, md1, 1); /* condition */
				cond_evl(md1);
				while(scan1tm(&s, md1, 0)){
					if(mod_evl(md1) == 0) ERRMOD(17);
				}
				*naibu++ = Terminator;
				break;
			case 3: /* path */
				*(pathmeimem = pathareaorg) = '\0';
				*(pathmeiptr = pathmeiorg) = NULL;
			case 4: /* search */
				retval = 0;
				if(hyomeiptr != hyomeiorg) ERRMOD(11);
				 /* サーチパスの指定はファイル名の出現より
				    先行しなければならないとしておく。*/

				while(scan1tm(&s, md1, 0)){/* find pathname */
					pathsrc_tourk(md1);
				}
				break;
			case 5: /* on_dispmode */
			case 6: /* off_dispmode */
				*naibu++ = modfn[num] . code;
				scan1tm(&s, md1, 1); /* dispmode string */

				if(*(bgn = md1) != '"') ERRMOD(12);
				bgn++;
				if(*(end = strend(bgn)) != '"') ERRMOD(10);
				*end = '\0';
				*naibu++ = dspnamsrc_tourk(bgn);
				scan1tm(&s, md1, 2); /* あればerr */
				break;
			case 7: /* on_unchg */
			case 8: /* off_unchg */
				*naibu++ = modfn[num] . code;
				scan1tm(&s, md1, 2); /* あればerr */
				break;
		}

	}
	*naibu = Terminator;
	return(retval);
}

 /** 条件式（モード名 又はnot,andなどの式）一つを解釈 */
static void
cond_evl(cod)
char	*cod; /* 条件式の内部表現の列へのポインタ */
{
	char	md1[MDT1LN];
	unsigned int	num;
	int	i; 

	if(is_digit(*cod) || *cod == '-'){
		*naibu++ = XY2INT(7, 0);
		if(0 != chk_get_int(cod, &num, 0)) ERRMOD(4);
		*naibu++ = num;
	} else if(*cod != '('){
		num = modsrc_tourk(cod, 1);
		*naibu++ = XY2INT(1, num);
	} else {
		cod++;
		scan1tm(&cod, md1, 1); /* not;and;or */
		num = kwdsrc(modcond, md1);
		*naibu++ = XY2INT(3, num);
		for(i = condarg[num]; i; i--){
			scan1tm(&cod, md1, 0);
			cond_evl(md1);
		}
		scan1tm(&cod, md1, 2);
	}
	*naibu = Terminator;
}

 /**	sで指定されたファイル名が既登録か探し、なければ登録。但し、既登録か
	どうかのチェックは厳密ではないが（例えば、同じファイルでも、
	パス名付きと無しとでは、同じと見ない）、ファイル名が既登録かどうか
	チェックするのは、メモリ節約のために同じ表を読み込むのを防ぐため
	だけなので、それ以外には別に困る点はない。*/
static int
fnmsrc_tourk(s)
char	*s;
{
	int	n;

	for(n = 0; hyomeiorg[n] != NULL; n++)
		if(!mystrcmp(hyomeiorg[n], s)) return(n);

	if(hyomeiorg + n != hyomeiptr) BUGreport(101);

	*hyomeiptr++ = hyomeimem;
	*hyomeiptr = NULL;
	mystrcpy(hyomeimem, s);
	if(!(hyoshu[n] = filnamchk(hyomeimem))) ERRMOD(3);
	strtail(hyomeimem);
	*++hyomeimem = '\0';
	return(n);
}

 /**	sで指定されたサーチパス名が既登録か探し、なければ登録。但し、fnmsrc_
	tourk()同様、既登録かどうかのチェックは厳密ではないが問題ない。*/
static int
pathsrc_tourk(s)
char	*s;
{
	int	n;
	char	fnm_addsla[MDT1LN];

	mystrcpy(fnm_addsla, s);
	if( !(*fnm_addsla == '\0' || *(strend(fnm_addsla)) == KUGIRI))
		chrcat(fnm_addsla, KUGIRI);
	 /* パス名が'/'で終わってなければ、それを付加する。*/

	for(n = 0; pathmeiorg[n] != NULL; n++)
		if(!strcmp(pathmeiorg[n], fnm_addsla)) return(n);

	if(pathmeiorg + n != pathmeiptr) BUGreport(104);

	*pathmeiptr++ = pathmeimem;
	*pathmeiptr = NULL;
	strcpy(pathmeimem, fnm_addsla);

	strtail(pathmeimem);

	*++pathmeimem = '\0';
	return(n);
}

 /** sで指定されたモード表示文字列が既登録か探し、なければ登録 */
static int
dspnamsrc_tourk(s)
char	*s;
{
	int	n;

	for(n = 0; dspnambgn[n] != NULL; n++)
		if(!mystrcmp(dspnambgn[n], s)) return(n);

	if(dspnambgn + n != dspnamptr) BUGreport(103);

	*dspnamptr++ = dspcod;
	*dspnamptr = NULL;
	mystrcpy(dspcod, s);
	strtail(dspcod);
	*++dspcod = '\0';
	return(n);
}

 /**	登録されているモード名の中から、sで指定されたモード名を探す。*np に
	モード番号が入る。見つからないと現モード名の総数が入る。その場合
	返値は0。*/
static int
modnam_src(s, np)
char	*s;
int	*np;
{
	for(*np = 0; modmeibgn[*np] != NULL; (*np)++ )
		if(!mystrcmp(modmeibgn[*np], s)) return(1);
	return(0);
}

 /**	sで指定されたモード名を探し、なければ登録。但し、flgが非0なら、
	見つからなければエラー */
static int
modsrc_tourk(s, flg)
char	*s;
int	flg;
{
	int	n;

	if(modnam_src(s, &n)) return(n);

	if(flg) ERRMOD(5);

	if(modmeibgn + n != modmeiptr) BUGreport(102);

	*modmeiptr++ = modmeimem;
	*modmeiptr = NULL;
	mystrcpy(modmeimem, s);
	if(!modnamchk(modmeimem)) ERRMOD(4);
	strtail(modmeimem);
	*++modmeimem = '\0';
	return(n);
}

 /** ファイルから一文字読む（空白文字は飛ばす）。読んだ文字がEOFなら0を返す */
static char
fspcpass()
{
	register int	c;

	while(EOF != (c = chkchar_getc(modefile)) && is_nulsp(c));
	return(c == EOF ? '\0' : c);
}

 /**	モード表には空白文字以外のコントロール文字は生では混じらないものと
	する。混じっていた場合はチェックしつつ、getcを行う。*/
static int
chkchar_getc(f)
FILE	*f;
{
	register int	c;

	c = getc(f);
	if(is_cntrl(c) && !isspace(c)){
		sprintf(mcurread, "\\%03o", c);
		ERRMOD(16);
	}
	return(c);
}	

static int
modehyo_getc()
{
	return(chkchar_getc(modefile));
}

static int
modehyo_ungetc(c)
register int	c;
{
	return(ungetc(c, modefile));
}

 /**	socの名のユーザのログイン・ディレクトリ名をdestに入れ、*destにその
	末尾を指させる。但しsocが空列なら自分のログイン・ディレクトリ名、
	NULLなら自分のホーム・ディレクトリ名。いずれの場合も、不成功時は
	何もしない。返値は、不成功時-1（getenv("HOME")失敗時だけは-2）。*/
static int
get_hmdir(dest, soc)
char	**dest, *soc;
{
	struct	passwd	*usr;
	char	*p;

	if(soc == NULL){
		if(NULL == (p = getenv("HOME"))) return(-2);
	} else {
		if(NULL == (usr = (*soc? getpwnam(soc) : getpwuid(getuid()))))
			return(-1);
		p = usr -> pw_dir;
	}
	strcpy(*dest, p);
	strtail(*dest);
	return(0);
}

 /**	モード表・対応表中の、ファイル名の部分の読み込み。先頭が @ 又は ~ の
	時は、特殊処理を行う。引数は、一字読み込み・一字戻し・文字列取り出しの
	関数と、結果を入れるエリアの番地へのポインタ、次に読まれる文字を入れる
	ポインタ。返値は、正常終了時0、@HOMEでホーム・ディレクトリが取れない時
	1、@のあとに変なものが来たら2、~で自分のホーム・ディレクトリが取れない
	時3、~のあとに存在しないユーザ名が来たら4。*/
int
readfnm(readchar_func, unreadc_func, readstr_func, areap, lastcptr)
register int	(*readchar_func)(), (*unreadc_func)(), (*readstr_func)();
char	**areap;
int	*lastcptr;
{
	char	*head;
	register int	c;

	c = (*readchar_func)();
	if(c == '@'){ /* @HOME, @MODEDIR, @LIBDIR */
		*(*areap)++ = c;
		head = *areap;
		(*readstr_func)(areap, 1);

		if(mystrcmp("HOME", head) == 0){
			*areap = --head;
			if(get_hmdir(areap, (char *)NULL) != 0){
				*areap = head;
				return(1);
			}
		} else
		if(mystrcmp("MODEDIR", head) == 0){
			strcpy(*areap = --head, modhyopath);
			if(KUGIRI== *(*areap= strend(*areap))) **areap = '\0';
		} else
#ifdef WNNDEFAULT
		if(mystrcmp("LIBDIR", head) == 0){
			strcpy(*areap = --head, LIBDIR);
			strtail(*areap);
		} else
#endif
		{
			*areap = --head;
			return(2);
		}

	} else
	if(c == '~'){ /* ~user */
		int	err;
			
		*(*areap)++ = c;
		head = *areap;
		(*readstr_func)(areap, 1);

		mystrcpy(head, head);
		*areap = head - 1;
		if((err = get_hmdir(areap, (*head ? head : NULL)))!= 0){
			*areap = --head;
			return(err == -2 ? 3 : 4);
		}

	} else {
		(*unreadc_func)(c);
	}

	*lastcptr = (*readstr_func)(areap, 0);
	return(0);
}

 /**	モード表から一文字分取り出す作業を、空白・括弧のどれか
	又はEOFが来るまで続ける。flg & 01が非0なら、'/'が来ても
	終わる。返値は、次に読まれる文字。*/
static int
rd_string(readfile, sptr, flg)
register FILE	*readfile;
char	**sptr;
int	flg;
{
	int	c;

	while(EOF != (c = chkchar_getc(readfile)) &&
	      !(is_nulsp(c) || c == '(' || c == ')') &&
	      !(flg & 01 && c == KUGIRI)){
		switch(c){
			case '\\': rd_bcksla(readfile, sptr); break;
			case '^': rd_ctrl(readfile, sptr); break;
			default: *(*sptr)++ = c;
		}
	}
	**sptr = '\0';
	return(ungetc(c, readfile));
}

static int
rd_str_from_modefile(sptr, flg)
char	**sptr;
int	flg;
{
	return(rd_string(modefile, sptr, flg));
}


 /**	モード表からバックスラッシュ形式の一文字分を取り出し、'\（8進）;'
	の形に直す。但し、先頭の'\\'は既に読まれたあと。*/
static void
rd_bcksla(readfile, sptr)
register FILE	*readfile;
char	**sptr;
{
	int	c, code = 0, digflg = 0;

	switch(c = chkchar_getc(readfile)){
		case 'n':
			code = '\n'; digflg = 1; break;
		case 't':
			code = '\t'; digflg = 1; break;
		case 'b':
			code = '\b'; digflg = 1; break;
		case 'r':
			code = '\r'; digflg = 1; break;
		case 'f':
			code = '\f'; digflg = 1; break;
		case 'e':
		case 'E':
			code = ESCCHR; digflg = 1; break;
		case 'o':
			while(c = chkchar_getc(readfile), is_octal(c)){
				code <<= 3;
				code += ctov(c);
				digflg = 1;
			}
			if(c != ';') ungetc(c, readfile);
			break;
		case 'd':
			while(c = chkchar_getc(readfile), is_digit(c)){
				code *= 10;
				code += ctov(c);
				digflg = 1;
			}
			if(c != ';') ungetc(c, readfile);
			break;
		case 'x':
			while(c = chkchar_getc(readfile), is_xdigit(c)){
				code <<= 4;
				code += ctov(c);
				digflg = 1;
			}
			if(c != ';') ungetc(c, readfile);
			break;
		default:
			if(is_octal(c)){
				digflg = 1;
				code = ctov(c);
				while(c= chkchar_getc(readfile), is_octal(c)){
					code <<= 3;
					code += ctov(c);
				}
				if(c != ';') ungetc(c, readfile);
			} else {
				code = c;
				digflg = 1;
			}
	}

	if(digflg == 0) ERRMOD(7);
	sprintf(*sptr, "\\%o;", code);
	strtail(*sptr);
}

 /**	モード表からコントロールコード形式の一文字分を取り出し、
	'\（8進）;' の形に直す。但し、先頭の'^'は既に読まれたあと。*/
static void
rd_ctrl(readfile, sptr)
register FILE	*readfile;
char	**sptr;
{
	int	c;

	if(!(' ' <= (c = chkchar_getc(readfile)) && c < '\177')) ERRMOD(7);
	if(c == '?') c = '\177'; else c &= 0x1f;

	sprintf(*sptr, "\\%o;", c);
	strtail(*sptr);
}

 /**	モード表の一かたまり（リスト、ファイル名、モード表示文字列）を
	切り出す。その際、特殊な表記（'^','\'による）は、'\（8進）;' の
	形に直す。flgが非0なら、EOFでエラーを起こし、')'で0を返す。*/
static int
read1tm(sptr, flg)
char	**sptr; /* モード表の内部表現の列へのポインタへのポインタ。
		   rd_bcksla()、rd_ctrl()、codeeval()でも同様 */
int	flg;
{
	int	c, err, retval = 1;
	char	*s;

	s = *sptr;

	while((c = fspcpass()) == ';'){
	  /* 注釈文を検出したら、行末までとばして再試行。*/
		while((c = chkchar_getc(modefile)) != '\n' && c != EOF);
	}

	switch(c){
		case '\0': /* EOFを表す */
			if(flg) ERRMOD(0);
			 else retval = 0;
			break;
		case ')':
			if(flg) retval = 0;
			 else ERRMOD(1);
			break;
		case '(':
			*s++ = c;
			*s++ = ' ';
			while(read1tm(&s, 1)) *s++ = ' ';
			*s++ = ')';
			break;
		case '"':
			*s++ = c;
			while((c = chkchar_getc(modefile)) != '"'){
				switch(c){
					case EOF : ERRMOD(0);
					case '\\': rd_bcksla(modefile, &s);
						   break;
					case '^' : rd_ctrl(modefile, &s);
						   break;
					default	 : *s++ = c;
				}
			}
			*s++ = '"';
			break;
		default:
			ungetc(c, modefile);
		 /* 先頭が @ 又は ~ の時は、特殊処理。*/
			err = readfnm(modehyo_getc, modehyo_ungetc,
				      rd_str_from_modefile, &s, &c);
			if(err){
				mcurread = s;
				switch(err){
					case 1:
					case 3: ERRMOD(13);
					case 2: ERRMOD(14);
					case 4: ERRMOD(15);
				}
			}

			if(c == EOF && flg) ERRMOD(0);
			if(c == ')' && !flg) ERRMOD(1);
	}

	*s = '\0';
	*sptr = s;
	return(retval);
}

 /**	8・10・16進コード用のキャラクタを実際のコードに直す。入力のチェックは
	しない。*/
static int
ctov(c)
char	c;
{
	if(is_upper(c)) return(c - 'A' + 10);
	if(is_lower(c)) return(c - 'a' + 10);
	return(c - '0');
}

 /**	リストの中身のscanに専用。')'で0を返す。EOLは来ないはず。
	flg == 1 のとき、取り出しに失敗したらエラー。
	flg == 2 のとき、取り出しに成功したらエラー。
	特殊なコード表記は既に全て '\（8進）;' の形に直っている筈。*/
static int
scan1tm(socp, dest, flg)
char	**socp, *dest;
	 /* socpの指しているポインタが指している所から取り出してdestに入れる。
	    その後、socpが指しているポインタを進める。*/
int	flg;
{
	char	c;
	int	retval = 1;

	while(c = *(*socp)++, is_nulsp(c)) if(c == '\0') ERRMOD(6);
	switch(c){
		case ')':
			retval = 0;
			break;
		case '(':
			*dest++ = c;
			*dest++ = ' ';
			while(scan1tm(socp, dest, 0)){
				strtail(dest);
				*dest++ = ' ';
			}
			*dest++ = ')';
			break;
		case '"':
			*dest++ = c;
			while((c = *dest++ = *(*socp)++) != '"'){
				if(c == '\\'){ /* '\（8進）;'の解釈 */
					while(c = *dest++ = *(*socp)++,
					      is_octal(c));
				}
			}
			break;
		default:
			*dest++ = c;
			while(!is_nulsp(**socp)) *dest++ = *(*socp)++;
	}

	*dest = '\0';
	if((flg == 1 && retval == 0) || (flg == 2 && retval == 1)) ERRMOD(6);
	return(retval);
}

 /** モード名として正当かチェック。英数字からなっていればいい */
static int
modnamchk(s)
char	*s;
{
	if(is_digit(*s)) return(0);
	for(; *s; s++) if(!is_alnum(*s) && *s != '_') return(0);
	return(1);
}

#define modu1(a, b) ((b) ? ((a) % (b)) : (a))
#define curmod(num) (modesw[num] . curmode)
#define modrng(num) (modesw[num] . moderng)

 /**	num番目のモードをチェンジし、変換表を選択し直す。引数 mode の値が0なら
	モードをoff、1ならonすることになる。なお、旧modeの値を返す。*/
modetyp	chgmod(num, mode)
int	num;
modetyp mode;
{
	modetyp oldmod;

	oldmod = curmod(num);
	curmod(num) = modu1(mode, modrng(num));
	choosehyo();
	return(oldmod);
}

 /** 全モードをまとめて切り替える */
void
allchgmod(mode)
modetyp	mode;
{
	int	i;

	for(i = 0; modmeibgn[i] != NULL; i++){
		curmod(i) = modu1(mode, modrng(i));
	}
	choosehyo();
}

 /**	num番目のモードを指定した数だけインクリメントし、旧modeの値を返す。*/
modetyp	incmod(num, dmode)
int	num;
modetyp	dmode;
{
	modetyp	oldmod, newmod;

	newmod = oldmod = curmod(num);
	newmod += dmode;
	if(oldmod > newmod) newmod -= modrng(num);
	return(chgmod(num, newmod));
}

 /**	num番目のモードを指定した数だけデクリメントし、旧modeの値を返す。都合
	により、incmodとは別に用意しなくてはならない。*/
modetyp	decmod(num, dmode)
int	num;
modetyp	dmode;
{
	modetyp	oldmod, newmod;

	newmod = oldmod = curmod(num);
	newmod -= dmode;
	if(oldmod < newmod) newmod += modrng(num);
	return(chgmod(num, newmod));
}

 /**	nameの名のモードがなければ非0を返し、あればそのモード番号・及びその
	状態の最大値＋１と現在の状態を取ってくる */
int
romkan_getmode(name, nump, modep, moderngp)
char	*name;
int	*nump;
modetyp	*modep, *moderngp;
{
	if(!modnam_src(name, nump)) return(-1);
	*modep = curmod(*nump);
	*moderngp = modrng(*nump);
	return(0);
}

 /**	nameの名のモードがなければ非0を返し、あればその状態をセットして
	変換表を再選択の後、旧状態を取り込んで0を返す。*/
int
romkan_setmode(name, modep)
char	*name;
modetyp	*modep;
{
	modetyp	oldmode, moderng;
	int	modenum;

	if(romkan_getmode(name, &modenum, &oldmode, &moderng)!= 0) return(-1);
	chgmod(modenum, *modep);
	*modep = oldmode;
	return(0);
}

 /** 変換表のクリア */
void
romkan_reset()
{
	naibu_[0] = Terminator;
	choosehyo();
	romkan_clear();
}

 /** 変換対応表の選択を行う */
void
choosehyo()
{
	int	*naibup, i;

	naibup = naibu_;
	usemaehyo[0] = usehyo[0] = useatohyo[0] = -1;
	for(i = 0; i < 2; i++){
		dspmod[1][i] = dspmod[0][i];
		dspmod[0][i] = NULL;
	}

	look_choose(&naibup, 1);
}

 /**	モード表の内部形式を順次見ていき、使用表の選択及びモード表示文字列の
	選択を行っていく。但しflgが0ならスキップするだけ */
static void
look_choose(naibupp, flg)
int	**naibupp;   /* モード表の内部表現の列へのポインタへのポインタ。
			look_cond()、evlcond()でも同様 */
int	flg;
{
	int	*naibup, naibu1, naibu2, branch, lcrsl;

	naibup = *naibupp;

	while((naibu1 = *naibup++) != Terminator){
		switch(SHUBET(naibu1)){
			case 4: /* 表名 */
				if(flg) hyouse(LWRMSK(naibu1));
				break;
			case 2: /* 条件式 */
				branch = LWRMSK(naibu1); /* if;when */
				lcrsl = look_cond(&naibup, flg);
				if(branch == 0 && lcrsl) flg = 0;
				break;
			case 5: /* romkanがon・off時それぞれの
				   モード表示文字列 */
				naibu2 = *naibup++;
				if(flg) dspmod[0][LWRMSK(naibu1)] =
							    dspnambgn[naibu2];
				break;
			case 6: /* romkanがそれぞれon・off時のモード表示
				   文字列を前のままに */
				if(flg) dspmod[0][LWRMSK(naibu1)] = 
						    dspmod[1][LWRMSK(naibu1)];
				break;
			default:
				BUGreport(6);
		}
	}

	*naibupp = naibup;
}

 /**	*naibupp が、内部表現の列で条件式を表すところを指している筈なので、
	それを評価し、真ならその続きを解釈しにいく。偽なら読み飛ばす。
	返値は、最初に評価した条件式の真偽値。*/
static int
look_cond(naibupp, flg)
int	**naibupp, flg;
{
	int	*naibup, condrsl;

	naibup = *naibupp;

	condrsl = evlcond(&naibup); /* 必ず評価しないといけないため */
	flg = flg && condrsl;
	look_choose(&naibup, flg);

	*naibupp = naibup;
	return(flg);
}

 /** 条件式の真偽値の評価  返値は0か1とは限らんぞ */
static int
evlcond(naibupp)
int	**naibupp;
{
	int	*naibup, naibu1, retval = -1, tmpval[ARGMAX], i, imax;

	naibup = *naibupp;

	naibu1 = *naibup++;
	switch(SHUBET(naibu1)){
		case 7: /* 数値 */
			retval = *naibup++; break;
		case 1: /* モード名 */
			retval = modesw[LWRMSK(naibu1)] . curmode; break;
		case 3: /* andなど */
			imax =	condarg[LWRMSK(naibu1)];
			for(i = 0; i < imax; i++)
				tmpval[i] = evlcond(&naibup);
			switch(LWRMSK(naibu1)){
			 /* 上から順にtrue,false,not,and,or */
				case 0: retval = !tmpval[0]; break;
				case 1: retval = tmpval[0]&& tmpval[1]; break;
				case 2: retval = tmpval[0]|| tmpval[1]; break;
				case 3: retval = 1; break;
				case 4: retval = 0; break;
				case 5: retval = (tmpval[0] == tmpval[1]);
					break;
				case 6: retval = (tmpval[0] != tmpval[1]);
					break;
				case 7: retval = ((unsigned int)tmpval[0] <
						  (unsigned int)tmpval[1]);
					break;
				case 8: retval = ((unsigned int)tmpval[0] >
						  (unsigned int)tmpval[1]);
					break;
			}
			break;
	}

	*naibupp = naibup;
	return(retval);
}

 /** num番目の表を、使用するものとして登録する。前・本・後処理の区別もする */
static void
hyouse(num)
int	num;
{
	int	*ptr;

	switch(hyoshu[num]){
		case 1: ptr = usemaehyo; break;
		case 2: ptr = usehyo; break;
		case 3: ptr = useatohyo; break;
		default: BUGreport(11); return;
	}
	for(; *ptr != -1; ptr++) if(*ptr == num) return;
	*ptr = num;
	*++ptr = -1;
}

 /** strcmpと同等  但し、'\（8進）;'も解釈する。*/
static int
mystrcmp(s1, s2)
char	*s1, *s2;
{
	char	c1, c2;

	while((c1 = codeeval(&s1)) == (c2 = codeeval(&s2)))
		if(c1 == '\0') return(0);
	return(c1 > c2 ? 1 : -1);
}

 /** strcpyと同等 但し'\（8進）;'も解釈する。s1 <= s2なら正常動作するはず */
static void
mystrcpy(s1, s2)
char	*s1, *s2;
{
	while(*s1++ = codeeval(&s2));
}

 /**	一文字の解釈を行う。普通の文字はそのまま、'\（8進）;'は実際のコードに
	直す。その後、文字列へのポインタを一文字分進めておく（少なくとも
	１バイト分進むことが保証されるはず）。*/
static char
codeeval(sptr)
register char	**sptr;
{
	register char	c;
	char	code = 0;

	if((c = *(*sptr)++) != '\\') return(c);
	while(c = *(*sptr)++, is_octal(c)){
		code <<= 3;
		code += ctov(c);
	}
	if(c != ';') BUGreport(12);
	return(code);
}

 /** romkanがon時のモード表示文字列を返す関数。無指定であってRK_DSPNILフラグが
     立っている時は空文字列を返す。*/

char	*romkan_dispmode()
{
	return(dspmod[0][0] == NULL && (flags & RK_DSPNIL) ?
	       nulstr : dspmod[0][0]);
}

 /** romkanがoff時のモード表示文字列を返す関数。無指定であってRK_DSPNILフラグ
     が立っている時は空文字列を返す。*/
char	*romkan_offmode()
{
	return(dspmod[0][1] == NULL && (flags & RK_DSPNIL) ?
	       nulstr : dspmod[0][1]);
}

 /** 文字列が10進整数ならその解釈をし、そうでなければ非0を返す */
static int
chk_get_int(p, ip, range)
char	*p;
unsigned int	*ip;
modetyp range;
{
	int	sgn = 1;
	modetyp	out;

	if(*p == '-'){
		p++;
		sgn = -1;
	}
	for(out = 0; *p; p++){
		if(!is_digit(*p)) return(-1);
#if defined(UX386) || defined(sun386)
		out = out * 10;
#else
		out *= 10;
#endif
		out += ctov(*p);
	}
	if(range != 0) out %= range;
	if(sgn == -1 && out != 0) out = range - out;
	*ip = out;
	return(0);
}
