/*
 */
/** 		Copyright OMRON Corporation. 1989, 1990, 1991, 1992, 1992
 *
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided 
 * that all of the following conditions are satisfied:
 *
 * <1>	The above copyright notices appear in all copies
 * <2>	Both the copyright notices and this permission notice appear in 
 *	supporting documentation
 * <3>	The name of "cWnn" isn't changed unless substantial modifications 
 * 	are made
 * <4>	Following words followed by the above copyright notices appear
 *    	in all supporting documentation of software based on "cWnn":
 *
 * 	This software is based on the original version of cWnn developed 
 *	by OMRON Corporation and Wnn developed by Kyoto University Research 
 *	Institute for Mathematical Sciences (KURIMS), OMRON Corporation and 
 * 	ASTEC Inc."
 *
 * <5>	The names of OMRON may not be used in advertising or publicity 
 *	pertaining to distribution of the software without specific, written 
 *	prior permission
 *
 *  OMRON Corporation makes no representations about the suitability of this 
 *  software for any purpose.  It is provided "as is" without express or 
 *  implied warranty.
 *
 *  Wnn consortium is one of distributors of the official Wnn source code
 *  release.  Wnn consortium also makes no representations about the
 *  suitability of this software for any purpose.  It is provided "as is"
 *  without express or implied warranty.
 *
 *  OMRON AND WNN CONSORTIUM DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
 *  SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
 *  IN NO EVENT SHALL OMRON OR WNN CONSORTIUM BE LIABLE FOR ANY SPECIAL,
 *  INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 *  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 *  OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 *  PERFORMANCE OF THIS SOFTWARE.
 *
 *  This sofeware is developed by OMRON Corporation, based on the original 
 *  version of Wnn which was developed by Kyoto University Research Institute 
 *  for Mathematical Sciences (KURIMS), OMRON Corporation and ASTEC Inc. 
 *
 *  	Copyright Kyoto University Research Institute for Mathematical 
 *		Sciences 1987,1988,1989,1990,1991, 1992
 * 	Copyright OMRON Corporation 1987,1988,1989,1990,1991, 1992
 *	Copyright ASTEC Inc. 1987, 1988,1989,1990,1991, 1992
 *
 *  Contact:	Tel:   	+81-75-951-5111
 *		Fax:   	+81-75-955-2442
 *		E-mail: zhong@ari.ncl.omron.co.jp
**/
/**  cWnn  Version 1.1	 **/

#include  <ctype.h>
#include "commonhd.h"
#ifdef CHINESE
#include "cplib.h"

/* pyshengmu_tbl[]: ShengMu table of Chinese PinYin */
char 	*py_shengmu_tbl[PY_NUM_SHENGMU] = {
	
	"",  "B", "C",  "Ch", "D",  
	"F", "G", "H",  "J",  "K", 
	"L", "M", "N",  "P",  "Q", 
	"R", "S", "Sh", "T",  "W",
	"X", "Y", "Z",  "Zh" 
	};
	
/* py_yunmu_tbl[]: YunMu table of Chinese ZhuYin */
char 	*py_yunmu_tbl[PY_NUM_YUNMU*5] = {

	"é¿",    "é¿",    "é¿",    "é¿",    "é¿",
	"aé¿",    "é°é¿",    "é¢é¿",    "é£é¿",    "é§é¿",
	"aié¿",   "é°ié¿",   "é¢ié¿",   "é£ié¿",   "é§ié¿",
	"ané¿",   "é°né¿",   "é¢né¿",   "é£né¿",   "é§né¿",
	"angé¿",  "é°ngé¿",  "é¢ngé¿",  "é£ngé¿",  "é§ngé¿",
	"aoé¿",   "é°oé¿",   "é¢oé¿",   "é£oé¿",   "é§oé¿",
	"eé¿",    "é•é¿",    "é¶é¿",    "éßé¿",    "é®é¿",
	"eié¿",   "é•ié¿",   "é¶ié¿",   "éßié¿",   "é®ié¿",
	"ené¿",   "é•né¿",   "é¶né¿",   "éßné¿",   "é®né¿",
	"engé¿",  "é•ngé¿",  "é¶ngé¿",  "éßngé¿",  "é®ngé¿",
	"eré¿",   "é•ré¿",   "é¶ré¿",   "éßré¿",   "é®ré¿",
	"ié¿",    "é©é¿",    "é™é¿",    "é´é¿",    "é¨é¿",
	"iaé¿",   "ié°é¿",   "ié¢é¿",   "ié£é¿",   "ié§é¿",
	"iané¿",  "ié°né¿",  "ié¢né¿",  "ié£né¿",  "ié§né¿",
	"iangé¿", "ié°ngé¿", "ié¢ngé¿", "ié£ngé¿", "ié§ngé¿",
	"iaoé¿",  "ié°oé¿",  "ié¢oé¿",  "ié£oé¿",  "ié§oé¿",
	"ieé¿",   "ié•é¿",   "ié¶é¿",   "iéßé¿",   "ié®é¿",
	"iné¿",   "é©né¿",   "é™né¿",   "é´né¿",   "é¨né¿",
	"ingé¿",  "é©ngé¿",  "é™ngé¿",  "é´ngé¿",  "é¨ngé¿",
	"iongé¿", "ié≠ngé¿", "iéÆngé¿", "iéØngé¿", "ié∞ngé¿",
	"iué¿",   "ié±é¿",   "ié≤é¿",   "ié≥é¿",   "ié¥é¿",
	"mé¿",    "mé¿",    "mé¿",    "mé¿",    "mé¿",
	"né¿",    "né¿",    "éΩé¿",    "éæé¿",    "éøé¿",
	"ngé¿",   "ngé¿",   "ngé¿",   "ngé¿",   "ngé¿",
	"oé¿",    "é≠é¿",    "éÆé¿",    "éØé¿",    "é∞é¿",
	"ongé¿",  "é≠ngé¿",  "éÆngé¿",  "éØngé¿",  "é∞ngé¿",
	"oué¿",   "é≠ué¿",   "éÆué¿",   "éØué¿",   "é∞ué¿",
	"ué¿",    "é±é¿",    "é≤é¿",    "é≥é¿",    "é¥é¿",
	"uaé¿",   "ué°é¿",   "ué¢é¿",   "ué£é¿",   "ué§é¿",
	"uaié¿",  "ué°ié¿",  "ué¢ié¿",  "ué£ié¿",  "ué§ié¿",
	"uané¿",  "ué°né¿",  "ué¢né¿",  "ué£né¿",  "ué§né¿",
	"uangé¿", "ué°ngé¿", "ué¢ngé¿", "ué£ngé¿", "ué§ngé¿",
	"ueé¿",   "ué•é¿",   "ué¶é¿",   "uéßé¿",   "ué®é¿",
	"uié¿",   "ué©é¿",   "ué™é¿",   "ué´é¿",   "ué¨é¿",
	"uné¿",   "é±né¿",   "é≤né¿",   "é≥né¿",   "é¥né¿",
	"uoé¿",   "ué≠é¿",   "uéÆé¿",   "uéØé¿",   "ué∞é¿",
	"éπé¿",    "éµé¿",    "é∂é¿",    "é∑é¿",    "é∏é¿",
	"éπeé¿",   "éπé•é¿",   "éπé¶é¿",   "éπéßé¿",   "éπé®é¿",
	"0é¿",   "1é¿",   "2é¿",   "3é¿",   "4é¿",  /* for undefinited YunMu  */
	};


/* pinyin_tbl:  size is NUM_SHENGMU*NUM_YUNMU, including empty ShengMu */
/*     and empty YunMu , and undefinited YunMu'-' */

int  pinyin_tbl[PY_NUM_SHENGMU*PY_NUM_YUNMU] = {
					    	
  0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
  0,1,1,1,1,1,0,1,1,1,0,1,0,1,0,1,1,1,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1,
  0,1,1,1,1,1,1,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,0,0,1,1,1,0,0,1,
  0,1,1,1,1,1,1,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,1,1,0,1,1,1,0,0,1,
  0,1,1,1,1,1,1,1,1,1,0,1,1,1,0,1,1,0,1,0,1,0,0,0,0,1,1,1,0,0,1,0,0,1,1,1,0,0,1,
  0,1,0,1,1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,1,
  0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,1,1,1,0,0,1,
  0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,1,1,1,1,1,1,0,1,1,1,0,0,1,
  0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,0,0,1,0,1,0,1,0,0,0,1,
  0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,1,1,1,0,0,1,
  0,1,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,0,1,0,0,0,1,1,1,1,0,0,1,0,0,0,1,1,1,1,1,
  0,1,1,1,1,1,1,1,1,1,0,1,0,1,0,1,1,1,1,0,1,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,1,
  0,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,0,1,0,0,0,0,1,1,1,0,0,1,0,0,0,0,1,1,1,1,
  0,1,1,1,1,1,0,1,1,1,0,1,0,1,0,1,1,1,1,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,1,
  0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,0,0,1,0,1,0,1,0,0,0,1,
  0,0,0,1,1,1,1,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,1,1,1,0,0,1,
  0,1,1,1,1,1,1,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,0,0,1,1,1,0,0,1,
  0,1,1,1,1,1,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,1,1,1,0,0,1,
  0,1,1,1,1,1,1,1,0,1,0,1,0,1,0,1,1,0,1,0,0,0,0,0,0,1,1,1,0,0,1,0,0,1,1,1,0,0,1,
  0,1,1,1,1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1,
  0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,0,0,1,0,1,0,1,0,0,0,1,
  0,1,0,1,1,1,1,0,0,0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,1,0,0,1,0,1,0,1,0,0,0,1,
  0,1,1,1,1,1,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,0,0,1,1,1,0,0,1,
  0,1,1,1,1,1,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,1,1,1,0,0,1
	};
#endif /* CHINESE */
