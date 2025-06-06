/*
 */
/** 		Copyright OMRON Corporation. 1989, 1990, 1991, 1992
 *
 * Permission to use, copy, modify, distribute and sell this software and its 
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

#include  <stdio.h>
#include  <ctype.h>
#include  "commonhd.h"
#ifdef CHINESE
#ifdef SYSVR2
#include  <string.h>
#endif
#ifdef BSD42
#include  <strings.h>
#endif
#include  "cplib.h"
#include  "rk_spclval.h"
#include  "jh.h"
#include  "wnn_string.h"

extern char  *py_table[];
extern char  *zy_table[];
unsigned char last_mark;     /* Using to reme previous auto_state() */

/* copied from old sstrings.c */
static int
cwnn_sStrcpy(c , w)
register char *c;
register w_char *w;
{
    char *c0 = c;
    for(;*w != 0 ; w++){
	if ((*w & 0x8080) == 0x8000) {
	    *c++ = 0x8f;
	    *c++ = (char)((*w & 0xff00) >> 8);
	    *c++ = (char)((*w & 0x007f) | 0x0080);
	} else {
	  if (*w & 0xff00)
	    *c++ = (char)((*w & 0xff00) >> 8);
	  else if (*w & 0x80)
	    *c++ = 0x8e;
	  *c++ = (char)(*w & 0x00ff);
	}
    }
    *c = '\0';
    return (c - c0);
}


static int
cwnn_Sstrcpy(w , c )
w_char *w;
unsigned char *c;
{
    w_char *w0 = w;

    for(;*c;){
	if (*c & 0x80) {
	    if (*c == 0x8e) {
		c++;
		*w++ = (unsigned short)*c++;
	    } else if (*c == 0x8f) {
		c++;
		*w = (unsigned short)(*c++ << 8);
		*w++ |= (unsigned short)(*c++ & 0x7f);
	    } else {
		*w = (unsigned short)(*c++ << 8);
		*w++ |= (unsigned short)*c++;
	    }
	} else {
	    *w++ = (unsigned short)*c++;
	}
    }
    *w = 0;
    return(w - w0);
}

static int
cwnn_Sstrcat(w , c )
w_char *w;
unsigned char *c;
{
    w_char *w0 = w;

    for ( ; *w; w++);

    for(;*c;){
	if (*c & 0x80) {
	    if (*c == 0x8e) {
		c++;
		*w++ = (unsigned short)*c++;
	    } else if (*c == 0x8f) {
		c++;
		*w = (unsigned short)(*c++ << 8);
		*w++ |= (unsigned short)(*c++ & 0x7f);
	    } else {
		*w = (unsigned short)(*c++ << 8);
		*w++ |= (unsigned short)*c++;
	    }
	} else {
	    *w++ = (unsigned short)*c++;
	}
    }
    *w = 0;
    return(w - w0);
}

/********** py_yunmu(), zy_yunmu(): if yuyin with YunMu, return YunMu's 
	position in YunMu table. after that, you must calculate its 
	real yun_raw static	   if without YunMu, return -1  
***********/
static int  py_yunmu(yuyin)
register char	*yuyin;
{
register int	i;
  for ( i = (PY_NUM_YUNMU*5)- 1; i >= 0 ; i-- ){
    if ( strncmp(yuyin, py_yunmu_tbl[i], strlen(py_yunmu_tbl[i])) == 0 ) 
	return(i);
    }
  return(-1);
}

static int  zy_yunmu(yuyin)
register char	*yuyin;
{
register int	i;
  for ( i = (ZY_NUM_YUNMU*5)- 1; i >= 0 ; i-- ){
    if ( strncmp(yuyin, zy_yunmu_tbl[i], strlen(zy_yunmu_tbl[i])) == 0 ) 
	return(i);
    }
  return(-1);
}

/* is_pinyin():  if is PinYin with Shengmu,	return 1   */
/*		if is PinYin without Shengmu ,   return 0   */
/*		else			     	return -1  */

static int is_pinyin(sheng_raw, yun_raw)
register int 	sheng_raw;
register int	yun_raw;
{

    	 if ( (sheng_raw >= 0) && (sheng_raw < PY_NUM_SHENGMU) && 
	     (yun_raw >= 0) && (yun_raw < PY_NUM_YUNMU)  &&
	     ( pinyin_tbl[sheng_raw * PY_NUM_YUNMU + yun_raw] == 1) )  {
		if ( sheng_raw == EMPTY_SHENG_RAW ) 
			return(0);
		else 	return(1);
		}
    else return(-1);

}

/* is_zhuyin():  if is ZhuYin with Shengmu: 	return 1   */
/*               if is ZhuYin without Shengmu: 	return 0   */
/*				    	 else:	return -1  */

static int is_zhuyin(sheng_raw, yun_raw)
register int 	sheng_raw;
register int	yun_raw;
{

    if ( (sheng_raw >= 0) && (sheng_raw < ZY_NUM_SHENGMU) && 
	 (yun_raw >= 0) && (yun_raw < ZY_NUM_YUNMU)  &&
	 ((zhuyin_tbl[sheng_raw * ZY_NUM_YUNMU + yun_raw] & 0x8000) == 0x8000) )
	if ( sheng_raw == EMPTY_SHENG_RAW )
		return ( 0);
	else 	return(1);
    return(-1);
}
/* py_shengmu(), zy_shengmu(): 
	if yuyin with ShengMu, return Shengmu's position
 	in ShengMu table.  if without ShengMu, return -1  
*/
static int py_shengmu(yuyin)
register char	*yuyin;
{
register int	i;
  for ( i = PY_NUM_SHENGMU - 1; i > 0;  i-- )  {
    if ( strncmp(yuyin, py_shengmu_tbl[i], strlen(py_shengmu_tbl[i])) == 0 ) 
	return (i);
  }
  return ( -1);
}

static int zy_shengmu(yuyin)
register char	*yuyin;
{
   register int	i;
   for ( i = ZY_NUM_SHENGMU - 1; i > 0;  i-- )  {
    if ( strncmp(yuyin, zy_shengmu_tbl[i], strlen(zy_shengmu_tbl[i])) == 0 ) 
	return (i);
   }
  return (-1);
}

#ifdef nodef
static void getstr_pzy(lstr, yincod, which)
register letter *lstr;
register w_char  yincod;
int which;
{
register letter  *ltmp;

	ltmp = lstr;
	for (; *lstr; lstr++)  

	if ((which == CWNN_PINYIN) && ((*lstr & 0x0000ffff) == PY_EOF ))
		break;
	else 
	if ((which == CWNN_ZHUYIN) && (isZY_EOF(*lstr & 0x0000ffff)) )
		break;
	lstr++;
        *ltmp++ = yincod;
        for (; *lstr; lstr++)
                *ltmp++ = *lstr;
        *ltmp = 0;
}
#endif
  	
/* create_yincod(): input:  
	raw in ShengMu table of PinYin, raw in YunMu table of PinYin,
  	sisheng	 output: yincod: if is PinYin, otherwise:0. 
*/

static w_char
create_yincod(sheng_raw, yun_raw, ss)
register int	sheng_raw;
register int	yun_raw;
register int	ss;
{
    int ret = 0;
    if (is_pinyin(sheng_raw, yun_raw ) == 1)  		/*Has Shengmu*/
 	ret = 0x0080 + (((yun_raw<<1)+0x20)<<8) +
			((sheng_raw-1)<<2) + 0x20;
    else if (is_pinyin(sheng_raw, yun_raw ) == 0) 	/*Not Shengmu */
	ret = 0x0080 + (((yun_raw<<1)+0x20)<<8) +
			((X_SHENG_RAW-1)<<2) + 0x20;	/*Rent this */
    else return(ret);
    if ((ss>0)&&(ss<=4))
    	ret += 0x0100 + ss - 1 ;
    return(ret);
}

/* pzy_yincod() 
	in:     The param is expected to be a PinYin or a ZhuYin.
	return:  a Yin_code is returned, if it is a PinYin or a ZhuYin.
	otherwise, 0 is returned, 
*/
static int
pzy_get_sheng_yun(yuyin, ss, sheng_raw, yun_raw, which)
register char  *yuyin;          /* one PinYin or ZhuYin with end of character */
register int   *ss;        	/* get SiSheng from PinYin or ZhuYin */
register int   *sheng_raw;      /* position in ShengMu table */
int   *yun_raw;        /* position in YunMu table */
int which;
{
/*
  register int     j;
*/
  register char    *pzytmp;

  *ss = -1;
  *sheng_raw = -1;
  *yun_raw = -1;

  pzytmp = yuyin;

    if ( which == CWNN_PINYIN ){	/* for Pinyin case */

  	if ((*sheng_raw = py_shengmu(pzytmp)) == -1 ) {  /* no ShengMu  */
	    if ( (*yun_raw = py_yunmu(pzytmp)) == -1 )  
		return(0);
	    else  {
		pzytmp += strlen(py_yunmu_tbl[*yun_raw]);
		*sheng_raw = 0;
		*ss = *yun_raw % 5;
		*yun_raw = *yun_raw / 5;
		return(pzytmp - yuyin);
	    }
  	} else { 		/* has ShengMu */ 
/*
	    for ( j = 0; (int)j < (int)strlen(py_shengmu_tbl[*sheng_raw]); j++) 
		pzytmp++;
*/
	    pzytmp += strlen(py_shengmu_tbl[*sheng_raw]);
	    if (strlen(pzytmp) == 0 )  return(0);
	    if ( (*yun_raw = py_yunmu(pzytmp)) != -1 ) {
		pzytmp += strlen(py_yunmu_tbl[*yun_raw]);
	    	*ss = *yun_raw % 5;
	    	*yun_raw = *yun_raw / 5;
	    	return(pzytmp - yuyin);
	    } else {
	   	   pzytmp = yuyin;
	   	   if ( (*yun_raw = py_yunmu(pzytmp)) == -1 ) 
			return(0);
	    	   else {
			pzytmp += strlen(py_yunmu_tbl[*yun_raw]);
			*sheng_raw = 0;
	    		*ss = *yun_raw % 5;
	    		*yun_raw = *yun_raw / 5;
	    		return(pzytmp - yuyin);
		   }
	    }
  	}	/* has ShengMu when Pinyin*/
    }	
    else {				/* for Zhuyin case */

  	if ((*sheng_raw = zy_shengmu(pzytmp)) == -1 ) {   /* no ShengMu  */
	    if ( (*yun_raw = zy_yunmu(pzytmp)) == -1 )  
		return(0);
	    else  {
		pzytmp += strlen(zy_yunmu_tbl[*yun_raw]);
		*sheng_raw = 0;
		*ss = *yun_raw % 5;
		*yun_raw = *yun_raw / 5;
		return(pzytmp - yuyin);
	     }
  	} else { 		/* has ShengMu */
/*
 	    for ( j = 0; (int)j < (int)strlen(zy_shengmu_tbl[*sheng_raw]); j++) 
		pzytmp++;
*/
	    pzytmp += strlen(zy_shengmu_tbl[*sheng_raw]);
	    if (strlen(pzytmp) == 0 )  return(0);
	    if ( (*yun_raw = zy_yunmu(pzytmp)) != -1 ) {
		pzytmp += strlen(zy_yunmu_tbl[*yun_raw]);
	   	*ss = *yun_raw % 5;
	     	*yun_raw = *yun_raw / 5;
	     	return(pzytmp - yuyin);
	    } else {
	   	pzytmp = yuyin;
		if ( (*yun_raw = zy_yunmu(pzytmp)) == -1 ) 
			return(0);
		else {
			pzytmp += strlen(zy_yunmu_tbl[*yun_raw]);
			*sheng_raw = 0;
	    		*ss = *yun_raw % 5;
	    		*yun_raw = *yun_raw / 5;
	    		return(pzytmp - yuyin);
		}
	    }
	}	/* has ShengMu when Zhuyin */
    }	/* which */
}

static w_char pzy_yincod(one_yuyin, len)
register char *one_yuyin;
register int *len;
{
  int     ss[1];
  int     sheng_raw[1];
  int     yun_raw[1];
  register  int	    zytmp;
  register int ret;

  *len = 0;
    /* for Pinyin */ 
    if(ret = pzy_get_sheng_yun(one_yuyin, ss, sheng_raw, yun_raw, CWNN_PINYIN)) 
	if ( is_pinyin(sheng_raw[0], yun_raw[0]) != -1  ) {
	    *len = ret;
	    return(create_yincod(sheng_raw[0], yun_raw[0], ss[0]));
	}
    /* for Zhuyin */	
    if(ret = pzy_get_sheng_yun(one_yuyin, ss, sheng_raw, yun_raw, CWNN_ZHUYIN)){
	zytmp = zhuyin_tbl[sheng_raw[0] * ZY_NUM_YUNMU + yun_raw[0]];
	if ( is_zhuyin(sheng_raw[0], yun_raw[0]) != -1 )  {
	    if ((zytmp & 0x0080 ) == 0x0080) {
		sheng_raw[0] = (zytmp >>8) & 0x7f;
		yun_raw[0] = zytmp & 0x7f;
	    }
	    *len = ret;
	    return(create_yincod(sheng_raw[0], yun_raw[0], ss[0]));
	}
    }
return(0);	/* Otherwise, Not a Pinyin nor Zhuyin  */
}

/* ltoScpy():  copy strings from letter type to w_char type */

static int ltoScpy(w,l)
register w_char *w;
register letter *l;
{
  register w_char *w0 = w;

    for(;*l;l++){
	if (/* ((*l & 0x0000ffff) == PY_EOF) || isZY_EOF(*l & 0x0000ffff)
	    ||*/ (*l == EOLTTR)) /* add by Kuwari */
	       break;
        *w++ = (*l & 0x0000ffff);
    }
    *w = (w_char)0;
    return(w - w0);
}

/* find_pinyin():  	find a YuYin in a string.  if there is a YuYin.
	 it must be at the tail of string.  return point of start YuYin
	 else return -1 eg. ;abcdHuang. 'Huang.' is a PinYin & return 5
	 012345
*/
static int
find_pinyin(str)
register char	*str;
{
register char 	*py_zy_tmp;
register int	i;
register int	pnt ;
int len;
    pnt = -1;
    if ((((*(str+strlen(str)-2)<<8)&0xff00)|
			(*(str+strlen(str)-1)&0x00ff))!= PY_EOF)
	return( -1 );
    for ( i = strlen(str) - 1; i >= 0; i-- ) {
	if ( (int)(strlen(str) - i) > PY_LEN )  
		return( pnt );
	py_zy_tmp = str + i;
	if ( pzy_yincod(py_zy_tmp, &len) != 0 )
		pnt = i;
    }
    return (pnt);
}

static int
find_zhuyin(str)
register char	*str;
{
register char 	*py_zy_tmp;
register int	i;
register int	pnt ;
int len;
   pnt = -1;
   if (!isZY_EOF(((*(str+strlen(str)-2)<<8)&0xff00)|
				(*(str+strlen(str)-1)&0x00ff)))
	return( -1 );
   for ( i = strlen(str) - 1; i >= 0; i-- ) {
	if ( (int)(strlen(str) - i) > PY_LEN )  
		return( pnt );
	py_zy_tmp = str + i;
	if ( pzy_yincod(py_zy_tmp, &len) != 0 )
		pnt = i;
   }
   return (pnt);
}

/* get_one_zhuyin(): get one ZhuYin from ZhuYin strings */
/* get_one_pinyin(): get one PinYin from PinYin strings */
static int
get_one_pinyin(pinzhuyin_str,one_pinzhuyin)
register unsigned char *pinzhuyin_str;
register char *one_pinzhuyin;
{
register w_char  chrtmp;
for (; (chrtmp = (((*pinzhuyin_str<<8)&0xff00)|(*(pinzhuyin_str+1)&0x00ff))) 
	!= PY_EOF && *pinzhuyin_str != 0;pinzhuyin_str++)  
    *one_pinzhuyin++ = *pinzhuyin_str;
   if (chrtmp == PY_EOF)   
	{
	*one_pinzhuyin++ = *pinzhuyin_str;
	pinzhuyin_str++;
	*one_pinzhuyin++ = *pinzhuyin_str;
	*one_pinzhuyin = 0;
	return(1);
	}
    else  {
	*one_pinzhuyin = 0;
	return(0);
	}
}

static int
get_one_zhuyin(pinzhuyin_str,one_pinzhuyin)
register unsigned char *pinzhuyin_str;
register char *one_pinzhuyin;
{
register w_char  chrtmp;
for (; !isZY_EOF(chrtmp = (((*pinzhuyin_str<<8)&0xff00)|(*(pinzhuyin_str+1)
	&0x00ff))) && *pinzhuyin_str != 0; pinzhuyin_str++)
    *one_pinzhuyin++ = *pinzhuyin_str;
    if ( isZY_EOF(chrtmp) ) 
	{
	*one_pinzhuyin++ = *pinzhuyin_str;
	pinzhuyin_str++;
	*one_pinzhuyin++ = *pinzhuyin_str;
	*one_pinzhuyin = 0;
	return(1);
	}
    else  {
	*one_pinzhuyin = 0;
	return(0);
	}
}

/* cwnn_is_yincod(c)	To check is "c"is a yincod. 
	if so, return(1) otherwise return 0*/ 
int cwnn_is_yincod(c)
register w_char	 c;
{
register int sheng_raw;
register int yun_raw;

  	if (! _cwnn_isyincod_d(c) ) return (0);

    	sheng_raw = Shengraw(c);
    	yun_raw = Yunraw(c);
    	if ( is_pinyin(sheng_raw, yun_raw) ) return(1);
    	if  ( sheng_raw == X_SHENG_RAW && is_pinyin(EMPTY_SHENG_RAW, yun_raw) == 0)
      	    return( 1 );
	else return(0);
}

/* For a given 'yincod', creat the corresponding Pinyin or Zhuyin
   to pzy_buf as a w_char string. If the given 'yincod' is not a yincod,
   'yincod', followed by a NULL is created to pzy_fub.
   Return: the lenth of pzy_buf is returned.
   Lenth means the lenth in console but not num of character.
*/ 
int  cwnn_yincod_pzy(pzy_buf, c, which )
register w_char  *pzy_buf;	/* out:    a Pinyin or Zhuyin */
register w_char  c;		/* input:  a yincod 	*/
int which;			/* option Pinyin or Zhuyin */
{
register int sheng_raw;
register int yun_raw;
register int ss;	/* for Sisheng	*/
register int zytmp;

  if ( ! cwnn_is_yincod(c) ) { 
	*pzy_buf = c;
	*(pzy_buf + 1) = 0;
	return(1);

/*	if ( ((c&0x00ff)>0xa0) && ((c&0x00ff)< 0xff) &&
	     ((c>>8) > 0xa0) && ((c>>8) < 0xff) )
		return(2);	
	else	return(1);
*/
  }
	    
  sheng_raw = Shengraw(c);
  yun_raw = Yunraw(c);
  ss = _cwnn_sisheng(c);

    if  (which == CWNN_PINYIN){		/* For Pinyin case */
	if (sheng_raw == X_SHENG_RAW && is_pinyin(sheng_raw, yun_raw) == -1 ) 
	     if ( is_pinyin(EMPTY_SHENG_RAW, yun_raw) == 0 )
		sheng_raw = EMPTY_SHENG_RAW;
	cwnn_Sstrcpy(pzy_buf, py_shengmu_tbl[sheng_raw]);
	if ( _cwnn_has_sisheng(c) ) 
	    cwnn_Sstrcat(pzy_buf, py_yunmu_tbl[yun_raw*5+ss]);
	else 
	    cwnn_Sstrcat(pzy_buf, py_yunmu_tbl[yun_raw*5]);
    }
    else {		/* For Zhuyin case */

	zytmp = zhuyin_tbl[sheng_raw * ZY_NUM_YUNMU + yun_raw];
	if ( is_zhuyin(sheng_raw, yun_raw) == -1 ) {
	     if ((zytmp & 0x0080)== 0x0080 ){
	       sheng_raw = (zytmp >>8) & 0x7f;	
	       yun_raw = zytmp & 0x7f;
	     }
	     else {
	     if ((sheng_raw == X_SHENG_RAW) &&(is_zhuyin(EMPTY_SHENG_RAW,yun_raw)==0))
		sheng_raw = EMPTY_SHENG_RAW;
	     }
 	}
	cwnn_Sstrcpy(pzy_buf, zy_shengmu_tbl[sheng_raw]);
	if ( yun_raw == EMPTY_YUN_RAW ) {
	    w_char tmp_w;
	    if ( _cwnn_has_sisheng(c) ) {
	      switch  (ss) {
		case 1:
		    tmp_w = ZY_EOF_1;
		    break;
		case 2:
		    tmp_w = ZY_EOF_2;
		    break;
		case 3:
		    tmp_w = ZY_EOF_3;
		    break;
		case 4:
		    tmp_w = ZY_EOF_4;
		    break;
	      }
	     } else {
		tmp_w = ZY_EOF_0;
	     }
	     wnn_Strncat(pzy_buf, &tmp_w, 1);
	}
	else {
	    if ( _cwnn_has_sisheng(c))
		  cwnn_Sstrcat(pzy_buf, zy_yunmu_tbl[yun_raw*5+ss]);
	    else 
		cwnn_Sstrcat(pzy_buf, zy_yunmu_tbl[yun_raw*5]);
	}
  }
return(wnn_Strlen(pzy_buf));
}

/* Copy s2 which having yincod to s1 in which yincod are replaced by
the corresponding Pinyin or Zhuyin. Lenth of s2 is returned
*/
int cwnn_yincod_pzy_str(s1, s2, n, which)
register w_char *s1;	/* result string having Pinyin or Zhuyin */
register w_char *s2;	/* input string having Yincod */
int  n;
int  which;
{
    w_char s2tmp[LINE_SIZE];
    register int i, j;
    w_char pzy_buf[10];
    int	len, sum_len;

	len = 0;
	sum_len = 0;
    	for (i = 0; i < n;  i++)  s2tmp[i] = s2[i];
    	for (i = 0; i < n;  i++) {

/*	len = cwnn_yincod_pzy(pzy_buf, s2tmp[i], which);
	for (j = 0; j < len; j++) 
		*s1++ = pzy_buf[j];
	sum_len += len;
*/
/* Strlen(pzy_buf) is the num of w_char , but "len" means the width */

	len = cwnn_yincod_pzy(pzy_buf, s2tmp[i], which);
	for (j = 0; j < wnn_Strlen(pzy_buf); j++) 
	*s1++ = pzy_buf[j];
	sum_len += wnn_Strlen(pzy_buf); 
	}
	*s1 =0;
	return(sum_len);
}

/* cwnn_pzy_yincod(s1, s2, which): 
After the matching in automaton, the string may be a Pinyin or a Zhuyin
If so, it will be replace by the coreesponding Yincod */

int cwnn_pzy_yincod(s1, s2, which)
letter *s1, *s2;
int which;
{
  /*
  register w_char codetmp2[PY_LEN];
  register char *codetmp1 = {"          "};
  */
  w_char codetmp2_buf[PY_LEN*10+1];
  char codetmp1_buf[PY_LEN*20+2];
  register w_char *codetmp2 = codetmp2_buf;
  register char *codetmp1 = codetmp1_buf;
  register letter *lettertmp = s2, *s1tmp = s1;
  register w_char yincod;
  int len;
  int conv = 0;
  w_char save_w[2];
  char save, tmp[6];

  save_w[0] = save_w[1] = 0;
	ltoScpy(codetmp2, lettertmp);
	if (cwnn_sStrcpy(codetmp1, codetmp2) <= 0) return(0);

/*	if ((yincod = pzy_yincod(codetmp1)) != 0) 
		getstr_pzy(s1, yincod, which);

Jun 13 Zhong */
	for (;*lettertmp && *lettertmp != EOLTTR;) {
	    if ( ( yincod = pzy_yincod(codetmp1, &len)) != 0) {
		conv++;
		*s1tmp++ = (letter)yincod;
		save = codetmp1[len];
		codetmp1[len] = '\0';
		lettertmp += cwnn_Sstrcpy(codetmp2, codetmp1);
		codetmp1[len] = save;
		codetmp1 += len;
	    } else {
		save_w[0] = (w_char)(*lettertmp & 0xffff);
		*s1tmp++ = *lettertmp++;
		codetmp1 += cwnn_sStrcpy(tmp, save_w);
	    }
	}
	if (*lettertmp == EOLTTR) *s1tmp++ = *lettertmp++;
	if (conv) {
	    return(s1tmp - s1);
	} else {
	    return(0);
	}
}

/* cwnn_py_yincod_str(), cwnn_zy_yincod_str():HUANG: for atod
	we get yomi as PinYin or ZhuYin strings from ascii-dictionary and
	translate it to YINcode
*/
void
cwnn_py_yincod_str(yuyin_str, css, un_sisheng_yincod_str, yincod_str)
register char    *yuyin_str;        /* yomi: PinYin or ZhuYin strings */
register char    *css;        /* get sisheng strings from PinYin strings */
register w_char  *un_sisheng_yincod_str; /* no-sisheng Yincod strings */
register w_char  *yincod_str;            /* Yincod strings with sisheng */
{
/*
  register  char    one_yuyin[LINE_SIZE];
  register  w_char not_yuyin[LINE_SIZE];
*/
  char    one_yuyin_buf[LINE_SIZE];
  w_char    not_yuyin_buf[LINE_SIZE];
  register  char    *one_yuyin = one_yuyin_buf;
  register  w_char    *not_yuyin = not_yuyin_buf;
  register  int     yin_eof;
  register  w_char  yincod;
  register  int     i, pst;
  int len;

    for ( ; *yuyin_str; ) {
	yin_eof = get_one_pinyin(yuyin_str, one_yuyin);
	yuyin_str += strlen(one_yuyin);
	cwnn_Sstrcpy(not_yuyin,  one_yuyin);
	pst = find_pinyin(one_yuyin);
	if ( yin_eof == 1  && pst != -1 ) {
		for ( i = 0; i < pst; i++ ) {
			*yincod_str++ = not_yuyin[i];
			*un_sisheng_yincod_str++ = not_yuyin[i];
			*css++ = '5';
			}
		yincod = pzy_yincod(one_yuyin, &len);
		*yincod_str ++ = yincod;
		*un_sisheng_yincod_str++ = _cwnn_yincod_0(yincod);
		*css++ = (char)(_cwnn_sisheng(yincod) + 0x30);
	}
	else {
		for ( i = 0;  not_yuyin[i]; i++ ) {
			*yincod_str++ = not_yuyin[i];
			*un_sisheng_yincod_str++ = not_yuyin[i];
			*css++ = '5';
		}
	}
    }
	*yincod_str = 0;
	*un_sisheng_yincod_str = 0;
	*css = 0;
}

void
cwnn_zy_yincod_str(yuyin_str, css, un_sisheng_yincod_str, yincod_str)
register char    *yuyin_str;        /* yomi: PinYin or ZhuYin strings */
register char    *css;        /* get sisheng strings from PinYin strings */
register w_char  *un_sisheng_yincod_str; /* no-sisheng Yincod strings */
register w_char  *yincod_str;            /* Yincod strings with sisheng */
{
/*
  register  char    one_yuyin[LINE_SIZE];
  register  w_char not_yuyin[LINE_SIZE];
*/
  char    one_yuyin_buf[LINE_SIZE];
  w_char    not_yuyin_buf[LINE_SIZE];
  register  char    *one_yuyin = one_yuyin_buf;
  register  w_char    *not_yuyin = not_yuyin_buf;
  register  int     yin_eof;
  register  w_char  yincod;
  register  int     i, pst;
  int len;

    for ( ; *yuyin_str; ) {
	yin_eof = get_one_zhuyin(yuyin_str, one_yuyin);
	yuyin_str += strlen(one_yuyin);
	cwnn_Sstrcpy(not_yuyin,  one_yuyin);
	pst = find_zhuyin(one_yuyin);
	if ( yin_eof == 1  && pst != -1 ) {
		for ( i = 0; i < pst; i++ ) {
			*yincod_str++ = not_yuyin[i];
			*un_sisheng_yincod_str++ = not_yuyin[i];
			*css++ = '5';
			}
		yincod = pzy_yincod(one_yuyin, &len);
		*yincod_str ++ = yincod;
		*un_sisheng_yincod_str++ = _cwnn_yincod_0(yincod);
		*css++ = (char)(_cwnn_sisheng(yincod) + 0x30);
	}
	else {
		for ( i = 0;  not_yuyin[i]; i++ ) {
			*yincod_str++ = not_yuyin[i];
			*un_sisheng_yincod_str++ = not_yuyin[i];
			*css++ = '5';
		}
	}
    }
	*yincod_str = 0;
	*un_sisheng_yincod_str = 0;
	*css = 0;
}

/* cwnn_py_str_analysis(), cp_zy_str_analysis():   HUANG: for atod
	we get yomi as PinYin or ZhuYin strings from ascii-dictionary 
	and translate it to YINcode
*/
void
cwnn_py_str_analysis(yuyin_str, css, un_sisheng_yincod_str, yincod_str)
register char    *yuyin_str;        /* yomi: PinYin or ZhuYin strings */
register char    *css;    /* get sisheng strings from PinYin strings */
register w_char  *un_sisheng_yincod_str; /* no-sisheng Yincod strings */
register w_char  *yincod_str;       /* Yincod strings with sisheng */
{
/*
  register  char    one_yuyin[LINE_SIZE];
  register  w_char not_yuyin[LINE_SIZE];
*/
  char    one_yuyin_buf[LINE_SIZE];
  w_char    not_yuyin_buf[LINE_SIZE];
  register  char    *one_yuyin = one_yuyin_buf;
  register  w_char    *not_yuyin = not_yuyin_buf;
  register  int     yin_eof;
  register  w_char  yincod;
  register  int     i, pst;
  int len;
    for ( ; *yuyin_str; ) {
	yin_eof = get_one_pinyin(yuyin_str, one_yuyin);
	yuyin_str += strlen(one_yuyin);
	cwnn_Sstrcpy(not_yuyin,  one_yuyin);
	pst = find_pinyin(one_yuyin);
	if ( yin_eof == 1  && pst != -1 ) {
		for ( i = 0; i < pst; i++ ) {
			*yincod_str++ = not_yuyin[i];
			*un_sisheng_yincod_str++ = not_yuyin[i];
			*css++ = '5';
			}
		yincod = pzy_yincod(one_yuyin, &len);
		*yincod_str ++ = yincod;
		*un_sisheng_yincod_str++ = _cwnn_yincod_0(yincod);
		*css++ = (char)(_cwnn_sisheng(yincod) + 0x30);
	}
	else {  for ( i = 0;  not_yuyin[i]; i++ ) {
			*yincod_str++ = not_yuyin[i];
			*un_sisheng_yincod_str++ = not_yuyin[i];
			*css++ = '5';
		}
	}
    }
	*yincod_str = 0;
	*un_sisheng_yincod_str = 0;
	*css = 0;
}

void
cwnn_zy_str_analysis(yuyin_str, css, un_sisheng_yincod_str, yincod_str)
register char    *yuyin_str;        /* yomi: PinYin or ZhuYin strings */
register char    *css;     /* get sisheng strings from PinYin strings */
register w_char  *un_sisheng_yincod_str; /* no-sisheng Yincod strings */
register w_char  *yincod_str;      /* Yincod strings with sisheng */
{
/*
  register  char    one_yuyin[LINE_SIZE];
  register  w_char not_yuyin[LINE_SIZE];
*/
  char    one_yuyin_buf[LINE_SIZE];
  w_char    not_yuyin_buf[LINE_SIZE];
  register  char    *one_yuyin = one_yuyin_buf;
  register  w_char    *not_yuyin = not_yuyin_buf;
  register  int     yin_eof;
  register  w_char  yincod;
  register  int     i, pst;
  int len;
    for ( ; *yuyin_str; ) {
	yin_eof = get_one_zhuyin(yuyin_str, one_yuyin);
	yuyin_str += strlen(one_yuyin);
	cwnn_Sstrcpy(not_yuyin,  one_yuyin);
	pst = find_zhuyin(one_yuyin);
	if ( yin_eof == 1  && pst != -1 ) {
		for ( i = 0; i < pst; i++ ) {
			*yincod_str++ = not_yuyin[i];
			*un_sisheng_yincod_str++ = not_yuyin[i];
			*css++ = '5';
			}
		yincod = pzy_yincod(one_yuyin, &len);
		*yincod_str ++ = yincod;
		*un_sisheng_yincod_str++ = _cwnn_yincod_0(yincod);
		*css++ = (char)(_cwnn_sisheng(yincod) + 0x30);
	}
	else {  for ( i = 0;  not_yuyin[i]; i++ ) {
			*yincod_str++ = not_yuyin[i];
			*un_sisheng_yincod_str++ = not_yuyin[i];
			*css++ = '5';
		}
	}
    }
	*yincod_str = 0;
	*un_sisheng_yincod_str = 0;
	*css = 0;
}
#endif /* CHINESE */
