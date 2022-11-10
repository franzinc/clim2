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
			rk_header.h
						88. 5.20  �� ��

	rk_main.c rk_read.c rk_modread.c rk_bltinfn.c�ζ��̥إå���
	��Ǽ�äƤ���������礭���ʤɤ������
***********************************************************************/
/*	Version 3.0
 */
/*	make����ɬ�פ�define

	BSD42		BSD�ˤ�strings.h����ѡ�string.h��Ȥ��������ס�
	SYSVR2		System V�ˤ����������¤��줿toupper��tolower�����
			�ʤʤ��Ƥ�ư����
	MVUX		ECLIPSE MV�Ǥα�ž����define  IKIS����ưdefine�����

	RKMODPATH="ʸ����"
			����ʸ�����⡼�����ɽ�Υ������ѥ��δĶ��ѿ���
			̾���ˤ���
	WNNDEFAULT	��@LIBDIR�פ�ɸ������ɽ�Τ���ǥ��쥯�ȥ��ɽ����
			�褦�ˤ���	
	IKIS		Ⱦ�Ѳ�̾�Σ��Х����ܤ�0xA8�ʥǥե���Ȥ�0x8E�ˤˤ���

	����¾ �ǥХå�����ɬ�פ˱����� KDSP��CHMDSP��define
*/

#ifdef MVUX
#  define IKIS
#endif
#if defined(MACM1)
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>
#endif

#include <stdio.h>
#ifdef BSD42
#  include <strings.h>
#  define strchr  index
#  define strrchr rindex
#else
#  include <string.h>
#endif
#include "rk_macros.h"

#define ESCCHR	'\033'
#define BASEMX	(26 + 10)

#define REALFN	200 /* ɽ�Υե�͡���κ���Ĺ */

#ifdef KOREAN
#define LINALL  30000 /* �б�ɽ�����ιԿ���� */
#define SIZALL  300000 /* �б�ɽ������ɽ���κ��祵����*/
#else
#define LINALL	2000 /* �б�ɽ�����ιԿ���� */
#define SIZALL	20000 /* �б�ɽ������ɽ���κ��祵������
			ɽ��Ĥ��ѿ����Ѱ�Ĺ�ι�פȤ��Ƥ�ȤäƤ� */
#endif

#define LINSIZ	1000 /* �б�ɽ�ΰ�Ԥκ��祵���� */
#define TRMSIZ	500 /* �б�ɽ�ΰ���ܤκ��祵������
			�⡼��̾�κ�Ĺ�Ȥ��Ƥ�ȤäƤ� */
#define KBFSIZ	100 /* �ܽ����Хåե��Υ����� */
#define DSPLIN	256 /* �ǥХå��� */
#define OUTSIZ	200 /* ���ϥХåե��Υ����� */
#define RSLMAX	20 /* �ؿ����ͤȤ����֤�ʸ����κ�Ĺ */

#define VARMAX	50 /* ɽ��Ĥ��ѿ��Ŀ� */
#define VARTOT	2000 /* ��ɽ���ѿ��Ŀ��� */
#define VARLEN	500 /* �ѿ�̾��Ĺ���η� */

#define FILNST	20

	/* rk_modread.c�ǻȤ�define */

#define HYOMAX	40 /* �Ѵ��б�ɽ�κ���Ŀ� */
#define HYOMEI	500 /* ɽ̾��ʸ������ */
#define PTHMAX	30 /* �������ѥ��κ���Ŀ� */
#define PTHMEI	800 /* �������ѥ�̾��ʸ������ */
#define MODMAX	50 /* �⡼�ɤμ���� */
#define MODMEI	300 /* �⡼�ɤ���ʸ���� */
#define DMDMAX	40 /* �⡼��ɽ���μ���� */
#define DMDCHR	250 /* �⡼��ɽ������ʸ���� */
#define MDHMAX	2500 /* �⡼��ɽ�κ��祵���� */
  /* �⡼��ɽ�κǽ��listscan�λ��ϡ����顼���θ���ơ��ꥹ��1�Ĥ�buffer��
	ɽ�Υ�����ʬ��äƤ�����*/
#define MDT1LN	200 /* �⡼������ꥹ��1�Ĥκ���Ĺ */
#define NAIBMX	400 /* �⡼�����ɽ������ɽ���κ��祵���� */
			/* Change KURI 200 --> 400 */

#define ARGMAX	2 /* ���Ƚ�Ǵؿ��ΰ����κ���Ŀ� */

 /* �ǥ��쥯�ȥ�̾�ζ��ڤ��define��UNIX�ѡˡ�UNIX�ʳ��δĶ��ǻȤ��ˤ�
    ����ȡ�fixednamep()���ѹ���ɬ�פ������readmode()��getenv�ط��������ˡ�*/
#define KUGIRI '/'

 /* ���顼������ */
#ifndef _WNN_SETJMP
#define _WNN_SETJMP
#include <setjmp.h>
#endif
