/*
 */
/*
 * Copyright 1989, 1992 OMRON Corporation
 * Copyright 1990, 1992 OMRON Corporation and Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of OMRON and MIT not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  OMRON and MIT make no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 *
 * Wnn consortium is one of distributors of the official Wnn source code
 * release.  Wnn consortium also makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * OMRON, MIT AND WNN CONSORTIUM DISCLAIM ALL WARRANTIES WITH REGARD TO
 * THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS, IN NO EVENT SHALL OMRON, MIT OR WNN CONSORTIUM BE LIABLE FOR ANY
 * SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 * CONTRACT, NEGLIGENCE OR OTHER TORTUOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *      Author: Hiroshi Kuribayashi    OMRON Corporation
 *                              kuri@omron.co.jp
 */

struct msg_bd {
    int msg_id;
    char *msg;
};

struct msg_cat {
    char lang[32];
    char name[64];
    char nlspath[64];
    int msg_cnt;
    struct msg_cat *nextp;
    struct msg_bd *msg_bd;
    /*int encoding;*/
};

#define	DEF_MSG "Message not found.\n"
/*
#define	DEF_LANG "C"
*/
#define	DEF_LANG "ja_JP"

extern struct msg_cat *msg_open();
extern char *msg_get();
extern void msg_close();
