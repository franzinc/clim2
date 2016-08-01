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


#ifndef min
#define min(a,b) ((int)(a) > (int)(b)? (b):(a))
#endif 

#define YIN_LEN         10
#define PY_LEN          10      /*   'ChuangŽÀ', 'ZhuangŽÀ'  */
#define PY_LEN_W        7       /* for w_char  */

#define PY_MARK       'P'   /* for PinYincode: to know if need to change to */
#define ZY_MARK       'Z'   /* for PinYincode: to know if need to change to */
#define CWNN_PINYIN	0	/* For Pinyin */
#define CWNN_ZHUYIN	1	/* For Zhuyin */


#define  PY_EOF 	0x8ec0  /* PY_EOF is end charactor of one PinYin
                       		         must be a 2 bytes code.  HUANG */
#define  ZY_EOF_0	0x8ec0  /* ZhuYin end character(no sisheng): 'ŽÀ' */
#define  ZY_EOF_1	0x8ec1	/* ZhuYin end character(sisheng 1 ): 'ŽÁ' */
#define  ZY_EOF_2	0x8ec2	/* ZhuYin end character(sisheng 2 ): 'ŽÂ' */
#define  ZY_EOF_3	0x8ec3	/* ZhuYin end character(sisheng 3 ): 'ŽÃ' */
#define  ZY_EOF_4	0x8ec4	/* ZhuYin end character(sisheng 4 ): 'ŽÄ' */

#define  isZY_EOF(X)  ( ((int)(X) >= ZY_EOF_0 && (int)(X) <= ZY_EOF_4 )? 1 : 0 )

#define PY_NUM_SHENGMU	24		/* ShengMu table size of PinYin */
#define PY_NUM_YUNMU	39		/* YunMu table size of PinYin */
#define ZY_NUM_SHENGMU	24		/* ShengMu table size of ZhuYin */
#define ZY_NUM_YUNMU	41		/* YunMu table size of ZhuYin */

#define EMPTY_SHENG_RAW 0		/* position of ShengMu EMPTY in ShengMu
					   table  */
#define EMPTY_YUN_RAW 0			/* position of YunMu EMPTY in YunMu
					   table  */
#define X_SHENG_RAW	20		/* position of ShengMu X in ShengMu 
					   table  */

/* YINcode creating is based on PinYin */
/* isyincod_d():  Check it is in the domain of Pinyin. To check if it is a 
   Pinyin, you need to use cp_isyincod() wihich checks the PinYin table  */
#define  _cwnn_isyincod_d(c)  ( ((c & 0x80) &&		\
				(!(c & 0x8000)) &&		\
				(c & 0x7f) >= 0x20 &&		\
				(((int)c >> (int)8) & 0x7f) >= 0x20 )	\
			      ?1:0 )	/* if is in YINcode's limite */

#define _cwnn_sisheng(YINcod)	( ((YINcod & 0x100) == 0x100)?  \
			   ((YINcod & 0x03 ) + 1): 0 )

#define _cwnn_yincod_0(YINcod)    ((YINcod) & 0xfefc)

/* Shengraw based on PinYin table */
#define Shengraw(YINcod) (((int)((YINcod - 0x20a0) & 0x7c)>>(int)2) + 0x01)

/* Yunraw based on PinYin table  */
#define Yunraw(YINcod) 	((int)(((YINcod) - 0x20a0) & 0x7e00) >> (int)9)


/* to see if the char is a start char of a pinyin without sisheng */
#define py0_first_ch(X) (((int)(X)>'A' && (int)(X)<='Z' &&  \
			  (X)!='E' && (X)!='O'&& \
			  (X)!='I' && (X)!='U' && (X)!='V') || \
			  (X)=='a'||(X)=='e'||(X)=='o' || (X)=='n'? 1: 0)

/* to see if the char is a start char of a zhuyin */

    /* for sisheng */
#define S_S_YOMI(X)  	( ((int)(X&0xff)>=0xa1)&&((int)(X&0xff)<=0xbf))||((int)(X>>8)==0x8e)
#define py_first_ch(X)	( py0_first_ch(X) || S_S_YOMI(X) )
#define zy_first_ch(X)   ( ((int)(X&0xff) >= 0xc0 && (int)(X&0xff) <= 0xe9 && \
			    ((int)(X>>(int)8)) == 0x8e) )

#define _cwnn_has_sisheng(YINcod)	( (( (YINcod)& 0x0100) != 0 ) ?1:0)

extern  unsigned char  last_mark;

extern  char  	*py_shengmu_tbl[];  	/* PinYin ShengMu table */
extern  char  	*py_yunmu_tbl[];	/* PinYin YunMu table   */
extern  char  	*zy_shengmu_tbl[];	/* ZhuYin ShengMu table */
extern  char  	*zy_yunmu_tbl[];	/* ZhuYin YunMu table   */

extern  int  	pinyin_tbl[]; 		/* PinYin table		*/
extern  int  	zhuyin_tbl[]; 		/* ZhuYin table 	*/
