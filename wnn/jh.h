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
/* 
  header file for jisho structure program.
*/

/* The following defin and structure definition
 * are concerned with ASCII (UJIS) files.
 */

#define COMMENT "\\comment"
#define TOTAL "\\total"
#define ASC_GIJI "\\giji"
#define HINSI "\\hinsi"
#ifdef CHINESE
#define CHINSI "\\cixing"
#define PINYIN "\\Pinyin"
#define ZHUYIN "\\Zhuyin"
#define BIXING "\\BiXing"
#endif
#define DIC_NO "\\dic_no"

#define REV_NORMAL 2
#define REVERSE 1
#define NORMAL  0

#ifndef	JS
struct je {
  w_char *yomi;
  w_char *kan;			/* Historically kanji is used so use kan. */
  w_char *comm;
  UCHAR *kanji;
  unsigned int hinsi;
#ifdef	CONVERT_with_SiSheng
  unsigned int ss;
#endif
  unsigned int hindo;
  int serial;			/* Only used for rev_dic */
};

extern struct je **jeary;
#endif	/* JS */

extern w_char file_comment[];
extern w_char hinsi_list[];

/*
 * Used in atod and others parameters.
 */

#define HEAP_PER_LINE 10	/* avelage of kanji + comment bytes */
#define YOMI_PER_LINE 3		/* avelage of yomi length(in w_char)*/
#define LINE_SIZE 1024
#define BADLMAX 3		
#define YOMI_KINDS (1 << 16)   /* Yomi characters Maximal */
#define MAX_ENTRIES 70000	/* default of max-entries for atod */
#define DEF_ENTRIES 10000

#define HEAPINC 1000
