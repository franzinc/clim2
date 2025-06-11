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
	demcom.h
	entry functions	definitions
*/

/*
 *	Demon Commands
 */
#define	JS_VERSION	0x00
#define	JS_OPEN		0x01
/*	#define	JS_OPEN_IN	0x02	*/
#define	JS_CLOSE	0x03
#define	JS_CONNECT	0x05
#define	JS_DISCONNECT	0x06
#define	JS_ENV_EXIST	0x07
#define	JS_ENV_STICKY	0x08
#define	JS_ENV_UN_STICKY	0x09


#define	JS_KANREN	0x11
#define	JS_KANTAN_SHO	0x12
#define	JS_KANZEN_SHO	0x13
#define	JS_KANTAN_DAI	0x14
#define	JS_KANZEN_DAI	0x15
#define	JS_HINDO_SET	0x18


#define	JS_DIC_ADD	0x21
#define	JS_DIC_DELETE	0x22
#define	JS_DIC_USE	0x23
#define	JS_DIC_LIST	0x24
#define	JS_DIC_INFO	0x25

#define	JS_FUZOKUGO_SET	0x29
#define	JS_FUZOKUGO_GET	0x30


#define	JS_WORD_ADD	0x31
#define	JS_WORD_DELETE	0x32
#define	JS_WORD_SEARCH	0x33
#define	JS_WORD_SEARCH_BY_ENV	0x34
#define	JS_WORD_INFO	0x35
#define JS_WORD_COMMENT_SET 0x36

#define	JS_PARAM_SET	0x41
#define	JS_PARAM_GET	0x42

#define	JS_MKDIR	0x51
#define	JS_ACCESS	0x52
#define	JS_WHO		0x53
#define	JS_ENV_LIST	0x55
#define	JS_FILE_LIST_ALL	0x56
#define	JS_DIC_LIST_ALL	0x57

#define	JS_FILE_READ	0x61
#define	JS_FILE_WRITE	0x62
#define	JS_FILE_SEND	0x63
#define	JS_FILE_RECEIVE	0x64

#define	JS_HINDO_FILE_CREATE	0x65
#define	JS_DIC_FILE_CREATE	0x66
#define JS_FILE_REMOVE	0x67

#define	JS_FILE_LIST	0x68
#define	JS_FILE_INFO	0x69
#define	JS_FILE_LOADED	0x6A
#define	JS_FILE_LOADED_LOCAL	0x6B
#define	JS_FILE_DISCARD	0x6C
#define JS_FILE_COMMENT_SET 0x6D
#define JS_FILE_PASSWORD_SET 0x6E /* 89/9/8 */

#define	JS_FILE_STAT	0x6F
#define JS_KILL		0x70

#define	JS_HINDO_FILE_CREATE_CLIENT	0x71
#define	JS_HINSI_LIST			0x72
#define JS_HINSI_NAME	0x73
#define JS_HINSI_NUMBER	0x74
#define JS_HINSI_DICTS  0x75
#define JS_HINSI_TABLE_SET 0x76
