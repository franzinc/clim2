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
			rk_bltinfn.c
						87.12. 6  Äû Êä

	ÊÑ´¹ÍÑ¤ÎÁÈ¤ß¹þ¤ß´Ø¿ô¤Î¤¦¤ÁÊ£»¨¤Ê¤â¤Î¤òÄêµÁ¤·¤Æ¤¢¤ë¡£
	Á´³Ñ¢«¢ªÈ¾³Ñ¤ÎÊÑ´¹¤¬¼çÂÎ¡£
***********************************************************************/
/*	Version 3.0
 */
#include "commonhd.h"
#include "config.h"
#include "rk_header.h"

 /* È¾³ÑÊ¸»ú¤Î¥³¡¼¥É¤Îdefine */
#define HKCHOU	(HNKAK1 * 0x100 + 0xB0) /* Ž° */
#define HKDKTN	(HNKAK1 * 0x100 + 0xDE) /* ŽÞ */
#define HKHNDK	(HNKAK1 * 0x100 + 0xDF) /* Žß */
#define HKMARU	(HNKAK1 * 0x100 + 0xA1) /* Ž¡ */
#define HKHRKG	(HNKAK1 * 0x100 + 0xA2) /* Ž¢ */
#define HKTJKG	(HNKAK1 * 0x100 + 0xA3) /* Ž£ */
#define HKTTEN	(HNKAK1 * 0x100 + 0xA4) /* Ž¤ */
#define HKNKPT	(HNKAK1 * 0x100 + 0xA5) /* Ž¥ */

 /* Á´³ÑÊ¸»ú¤Î¥³¡¼¥É¤Îdefine */
#define CHOUON	(0xA1BC) /* ¡¼ */
#define DAKUTN	(0xA1AB) /* ¡« */
#define HNDAKU	(0xA1AC) /* ¡¬ */
#define MNMARU	(0xA1A3) /* ¡£ */ /* Ì¾Á°¤Ï MaNMARU¡Ê¤Þ¤ó¤Þ¤ë¡Ë¤ÎÎ¬ */
#define HRKKAG	(0xA1D6) /* ¡Ö */
#define TJIKAG	(0xA1D7) /* ¡× */
#define TOUTEN	(0xA1A2) /* ¡¢ */
#define NKPOTU	(0xA1A6) /* ¡¦ */

static char _lang[6];

void
romkan_set_lang(lang)
char *lang;
{
	strncpy(_lang, lang, 5);
	_lang[5] = 0;
}

 /** ASCIIÊ¸»ú¢ªÁ´³Ñ */
letter	to_zenalpha(l)
letter	l;
{
	letter	retval;

	static	uns_chr *data = (uns_chr *)
	   "¡¡¡ª¡É¡ô¡ð¡ó¡õ¡Ç¡Ê¡Ë¡ö¡Ü¡¤¡Ý¡¥¡¿£°£±£²£³£´£µ£¶£·£¸£¹¡§¡¨¡ã¡á¡ä¡©\
¡÷£Á£Â£Ã£Ä£Å£Æ£Ç£È£É£Ê£Ë£Ì£Í£Î£Ï£Ð£Ñ£Ò£Ó£Ô£Õ£Ö£×£Ø£Ù£Ú¡Î¡ï¡Ï¡°¡²¡®£á£â£ã£ä£å\
£æ£ç£è£é£ê£ë£ì£í£î£ï£ð£ñ£ò£ó£ô£õ£ö£÷£ø£ù£ú¡Ð¡Ã¡Ñ¡±";

#ifdef	CHINESE
	static	uns_chr *data_cn = (uns_chr *)
	   "¡¡£¡¡±££¡ç£¥£¦£§£¨£©¡ù£«£¬¡ª£®£¯£°£±£²£³£´£µ£¶£·£¸£¹£º£»¡´£½¡µ£¿\
£À£Á£Â£Ã£Ä£Å£Æ£Ç£È£É£Ê£Ë£Ì£Í£Î£Ï£Ð£Ñ£Ò£Ó£Ô£Õ£Ö£×£Ø£Ù£Ú£Û£¤£Ý£Þ£ß£à£á£â£ã£ä£å\
£æ£ç£è£é£ê£ë£ì£í£î£ï£ð£ñ£ò£ó£ô£õ£ö£÷£ø£ù£ú£û£ü£ý£þ";
#endif	/* CHINESE */
#ifdef	KOREAN
	static  uns_chr *data_ko = (uns_chr *)
	   "¡¡£¡£¢£££¤£¥£¦£§£¨£©£ª£«£¬£­£®£¯£°£±£²£³£´£µ£¶£·£¸£¹£º£»£¼£½£¾£¿\
£À£Á£Â£Ã£Ä£Å£Æ£Ç£È£É£Ê£Ë£Ì£Í£Î£Ï£Ð£Ñ£Ò£Ó£Ô£Õ£Ö£×£Ø£Ù£Ú£Û£Ü£Ý£Þ£ß£à£á£â£ã£ä£å\
£æ£ç£è£é£ê£ë£ì£í£î£ï£ð£ñ£ò£ó£ô£õ£ö£÷£ø£ù£ú£û£ü£ý£þ";
#endif  /* KOREAN */

	if(' ' <= l && l <= '~') {
		l = (l - ' ') << 1;
#ifdef	CHINESE
	    if(!strcmp(_lang, WNN_C_LANG) || !strcmp(_lang, WNN_T_LANG)){
		retval = data_cn[l] << 8;
		retval += data_cn[++l];
	    } else
#endif
#ifdef  KOREAN
	    if(!strcmp(_lang, WNN_K_LANG)){
		retval = data_ko[l] << 8;
		retval += data_ko[++l];
	    } else
#endif  /* KOREAN */
	    {
		retval = data[l] << 8;
		retval += data[++l];
	    }
		return(retval);
	} else return(l);
}

static	char	*hankdata[] = {
	"Ž§","Ž±","Ž¨","Ž²","Ž©","Ž³","Žª","Ž´","Ž«","Žµ",
	"Ž¶","Ž¶ŽÞ","Ž·","Ž·ŽÞ","Ž¸","Ž¸ŽÞ","Ž¹","Ž¹ŽÞ","Žº","ŽºŽÞ",
	"Ž»","Ž»ŽÞ","Ž¼","Ž¼ŽÞ","Ž½","Ž½ŽÞ","Ž¾","Ž¾ŽÞ","Ž¿","Ž¿ŽÞ",
	"ŽÀ","ŽÀŽÞ","ŽÁ","ŽÁŽÞ","Ž¯","ŽÂ","ŽÂŽÞ","ŽÃ","ŽÃŽÞ","ŽÄ","ŽÄŽÞ",
	"ŽÅ","ŽÆ","ŽÇ","ŽÈ","ŽÉ",
	"ŽÊ","ŽÊŽÞ","ŽÊŽß","ŽË","ŽËŽÞ","ŽËŽß","ŽÌ","ŽÌŽÞ","ŽÌŽß",
	"ŽÍ","ŽÍŽÞ","ŽÍŽß","ŽÎ","ŽÎŽÞ","ŽÎŽß",
	"ŽÏ","ŽÐ","ŽÑ","ŽÒ","ŽÓ",
	"Ž¬","ŽÔ","Ž­","ŽÕ","Ž®","ŽÖ",
	"Ž×","ŽØ","ŽÙ","ŽÚ","ŽÛ",
	"¥î","ŽÜ","¥ð","¥ñ","Ž¦","ŽÝ",
	"Ž³ŽÞ","¥õ","¥ö"
};  /* Á´³Ñ¤¬º®¤¸¤Ã¤Æ¤ë¤Î¤ÇÃí°Õ */

 /**	¾å¤Îhankdata¤¬¡¢¼ÂºÝ¤Ë»È¤¦È¾³Ñ¥³¡¼¥É¤òÉ½¤·¤Æ¤¤¤Ê¤¤¤È¤­¡¢¼ÂºÝ¤Î¤â¤Î¤Ë
	½¤Àµ¤¹¤ë¡£½é´üÀßÄê»þ¤Ë°ì²ó¤À¤±¸Æ¤Ö */
void
hank_setup()
{
	int	i;
	char	*s, orig_hnkak1;

	orig_hnkak1 = *hankdata[0];
 /*	*hankdata[] ¤Ç¤ÎÈ¾³ÑÊ¸»ú¤Î£±¥Ð¥¤¥È¤á¡£È¾³ÑÊ¸»ú¤Î£±¥Ð¥¤¥È¤á¤À¤±¤¬°Û¤Ê¤ë
	¤è¤¦¤ÊÂ¾µ¡¼ï¤Ë°Ü¿¢¤¹¤ë¤È¤­¤Ï¡¢HNKAK1¤Îdefine¤òÊÑ¤¨¤ì¤ÐOK¡£Ã¢¤·romkan¤Î
	¥½¡¼¥¹Ãæ¤ÎÈ¾³ÑÊ¸»ú¡Ê¤³¤Î¥Õ¥¡¥¤¥ë¤Ë¤Î¤ßÂ¸ºß¡Ë¤â¥³¥ó¥Ð¡¼¥È¤·¤Æ¡¢¤½¤Îµ¡¼ï
	¤Ë¹ç¤ï¤»¤ë¤Û¤¦¤¬Ë¾¤Þ¤·¤¤¡£¤·¤«¤·¡¢¥¨¥Ç¥£¥¿¤Ç¤³¤Î¥Õ¥¡¥¤¥ë¤ò½¤Àµ¤·¤¿¤ê
	¤¹¤ë¾ì¹ç¤Ë¡¢È¾³ÑÊ¸»ú¤Î°·¤¤¤¬¤¦¤Þ¤¯¤¤¤«¤Ê¤¤¾ì¹ç¤¬¤¢¤ë¤Î¤Ç¡¢ÆÃ¤Ë
	¥³¥ó¥Ð¡¼¥È¤ò¤·¤Ê¤¯¤È¤âÆ°ºî¤¹¤ë¤è¤¦¤Ë½èÃÖ¤Ï¤·¤Æ¤¢¤ë¡£¤½¤ì¤¬¡¢¤³¤Î
	hank_setup()¤Ç¤¢¤ë¡£hankdata¤Ï¡¢½é´üÀßÄê»þ¤Ë hank_setup()¤Ë¤è¤Ã¤Æ
	¼ÂºÝ¤ÎÈ¾³Ñ¥³¡¼¥É¤ËÄ¾¤µ¤ì¤ë¡£*/

	if(orig_hnkak1 == (char)HNKAK1) return;
	for(i = 0; i < numberof(hankdata); i++){
		for(s = hankdata[i]; *s; s += 2)
			if(*s == orig_hnkak1) *s = HNKAK1;
	}
}

 /** ¤«¤Ê¢ªÈ¾³Ñ¥«¥¿¥«¥Ê¡£·ë²Ì¤¬ÆóÊ¸»ú¤Ë¤Ê¤ë¤³¤È¤â¤¢¤ë¡£*/
void
to_hankata(in, outp)
letter	in, **outp;
{
	uns_chr *p, c;
	letter	*out;

	out = *outp;
	switch(in){
		case CHOUON: *out++ = HKCHOU; break;
		case DAKUTN: *out++ = HKDKTN; break;
		case HNDAKU: *out++ = HKHNDK; break;
		case MNMARU: *out++ = HKMARU; break;
		case HRKKAG: *out++ = HKHRKG; break;
		case TJIKAG: *out++ = HKTJKG; break;
		case TOUTEN: *out++ = HKTTEN; break;
		case NKPOTU: *out++ = HKNKPT; break;
		default:
			if(is_kata(in)){
				for(p = (uns_chr *)hankdata[in - KATBGN];
				    c = *p; p++)
					*out++ = (c << 8) + *++p;
			} else 
			if(is_hira(in)){
				for(p = (uns_chr *)hankdata[in - HIRBGN];
				    c = *p; p++)
					*out++ = (c << 8) + *++p;
			} else {
				*out++ = in;
			}
	}
	*out = EOLTTR;
	*outp = out;
}

 /**	È¾³Ñ¥«¥¿¥«¥Ê¢ª¤Ò¤é¤¬¤Ê¡£Ã¢¤·¡¢ÂùÅÀ¤ò»ý¤ÄÊ¸»ú¤ò°ì¤Ä¤Ë¤Þ¤È¤á¤Æ¤Ï
	¤¯¤ì¤Ê¤¤¤Î¤ÇÃí°Õ¡£*/
letter	to_zenhira(l)
letter	l;
{
	letter	retval;

	static	uns_chr *data = (uns_chr *)
	   "¡£¡Ö¡×¡¢¡¦¤ò¤¡¤£¤¥¤§¤©¤ã¤å¤ç¤Ã¡¼¤¢¤¤¤¦¤¨¤ª¤«¤­¤¯¤±¤³¤µ¤·¤¹¤»¤½¤¿\
¤Á¤Ä¤Æ¤È¤Ê¤Ë¤Ì¤Í¤Î¤Ï¤Ò¤Õ¤Ø¤Û¤Þ¤ß¤à¤á¤â¤ä¤æ¤è¤é¤ê¤ë¤ì¤í¤ï¤ó¡«¡¬";

#ifdef	CHINESE
	static	uns_chr *data_cn = (uns_chr *)
	   "¡£¡¸¡¹¡¢¡¤¤ò¤¡¤£¤¥¤§¤©¤ã¤å¤ç¤Ã¡¼¤¢¤¤¤¦¤¨¤ªÂð¤­¤¯¤±¤³¶È¤·¤¹¤»¤½¤¿\
¤Á¤Ä¤Æ¤È¤Ê¤Ë¤Ì¤ÍµÄ¤Ï¤Ò¤Õ¤Ø¤Û¤Þ¤ß¤à¤áÒ²¤ä¤æ¤è¤é¤ê¤ë¤ì¤í¤ï¤ó¡å¡ã";
#endif	/* CHINESE */

	if(is_hankata(l)){
		l = (l - HKKBGN) << 1;
#ifdef	CHINESE
	    if(!strcmp(_lang, WNN_C_LANG) || !strcmp(_lang, WNN_T_LANG)){
		retval = data_cn[l] << 8;
		retval += data_cn[++l];
	    } else
#endif
	    {
		retval = data[l] << 8;
		retval += data[++l];
	    }
		return(retval);
	} else return(l);
}

 /**	È¾³Ñ¥«¥¿¥«¥Ê¢ªÁ´³Ñ¡£Ã¢¤·¡¢ÂùÅÀ¤ò»ý¤ÄÊ¸»ú¤ò°ì¤Ä¤Ë¤Þ¤È¤á¤Æ¤Ï
	¤¯¤ì¤Ê¤¤¤Î¤ÇÃí°Õ¡£*/
letter	to_zenkata(l)
letter	l;
{
	return(is_hankata(l) ? (l = to_zenhira(l), to_kata(l)) : l);
}

 /* ¥Ó¥Ã¥È¥Ù¥¯¥¿¤Î¹½À® */
#define bitvec(b0, b1, b2, b3, b4, b5, b6, b7) (			 \
	(char)b0 | ((char)b1 << 1) | ((char)b2 << 2) | ((char)b3 << 3) | ((char)b4 << 4) | ((char)b5 << 5) | \
	((char)b6 << 6) | ((char)b7 << 7)						 \
)

 /** char¤ÎÇÛÎó h ¤ò¥Ó¥Ã¥È¥Ù¥¯¥¿¤È¸«¤Æ¤½¤ÎÂèi¥Ó¥Ã¥È¤ò¥Á¥§¥Ã¥¯¤¹¤ë */
#define bitlook(h, i) (h[(i) >> 3] & (1 << ((i) & 7)))

#define KATRPT	0xA1B3 /* ¡³ */
#define HIRRPT	0xA1B5 /* ¡µ */
#define KATA_U	0xA5A6 /* ¥¦ */
#define KAT_VU	0xA5F4 /* ¥ô */
#define HIR_KA	0xA4AB /* ¤« */
#define HIR_HO	0xA4DB /* ¤Û */
#define KAT_KA	0xA5AB /* ¥« */
#define KAT_HO	0xA5DB /* ¥Û */
#define HIR_HA	0xA4CF /* ¤Ï */
#define KAT_HA	0xA5CF /* ¥Ï */

 /**	¸å¤í¤ËÈ¾ÂùÅÀ¤ò¤¯¤Ã¤Ä¤±¤ë¡£·ë²Ì¤Ï°ìËô¤ÏÆóÊ¸»ú¡£*/
void
handakuadd(in, outp)
letter	in, **outp;
{
	if((HIR_HA <= in && in <= HIR_HO) ? 0 == (in - HIR_HA) % 3 :
	   (KAT_HA <= in && in <= KAT_HO && 0 == (in - KAT_HA) % 3)){
		*(*outp)++ = in + 2;
	} else {
		*(*outp)++ = in;
		*(*outp)++ = HNDAKU;
	}
	**outp = EOLTTR;
}

 /**	¸å¤í¤ËÂùÅÀ¤ò¤¯¤Ã¤Ä¤±¤ë¡£·ë²Ì¤Ï°ìËô¤ÏÆóÊ¸»ú¡£*/
void
dakuadd(in, outp)
letter	in, **outp;
{
	static	char	flgbit[] = {
		bitvec(1, 0, 1, 0, 1, 0, 1, 0), /* ¤«¤¬¤­¤®¤¯¤°¤±¤² */
		bitvec(1, 0, 1, 0, 1, 0, 1, 0), /* ¤³¤´¤µ¤¶¤·¤¸¤¹¤º */
		bitvec(1, 0, 1, 0, 1, 0, 1, 0), /* ¤»¤¼¤½¤¾¤¿¤À¤Á¤Â */
		bitvec(0, 1, 0, 1, 0, 1, 0, 0), /* ¤Ã¤Ä¤Å¤Æ¤Ç¤È¤É¤Ê */
		bitvec(0, 0, 0, 0, 1, 0, 0, 1), /* ¤Ë¤Ì¤Í¤Î¤Ï¤Ð¤Ñ¤Ò */
		bitvec(0, 0, 1, 0, 0, 1, 0, 0), /* ¤Ó¤Ô¤Õ¤Ö¤×¤Ø¤Ù¤Ú */
		bitvec(1, 0, 0, 0, 0, 0, 0, 0)	/* ¤Û */
	};
	letter	c;

	if((HIR_KA <= in && in <= HIR_HO) ? (c = in - HIR_KA, 1) :
	   (KAT_KA <= in && in <= KAT_HO && (c = in - KAT_KA, 1))){
		if(bitlook(flgbit, c)){
			*(*outp)++ = in + 1;
		} else {
			*(*outp)++ = in;
			*(*outp)++ = DAKUTN;
		}
	} else switch(in){
		case KATRPT:
		case HIRRPT:
			*(*outp)++ = in + 1; break;
		case KATA_U:
			*(*outp)++ = KAT_VU; break;
		default:
			*(*outp)++ = in;
			*(*outp)++ = DAKUTN;
	}
	**outp = EOLTTR;
}

 /** in¤ÇÍ¿¤¨¤é¤ì¤¿¥³¡¼¥É¤òbase¿Ê¤Î¿ô»ú¤Ë¤·¤Æoutp¤ËÆþ¤ì¤ë¡£*/
void
to_digit(in, base, outp)
letter	in, base, **outp;
{
	letter	c, vtol();

	if(c = in, c /= base) to_digit(c, base, outp);
	*(*outp)++ = vtol(in % base);
	**outp = EOLTTR;
}

