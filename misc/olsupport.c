/*				-[]-
 *
 * copyright (c) 1992 Franz Inc, Berkeley, CA  All rights reserved.
 *
 * The software, data and information contained herein are proprietary
 * to, and comprise valuable trade secrets of, Franz, Inc.  They are
 * given in confidence by Franz, Inc. pursuant to a written license
 * agreement, and may be stored and used only in accordance with the terms
 * of such license.
 *
 * Restricted Rights Legend
 * ------------------------
 * Use, duplication, and disclosure of the software, data and information
 * contained herein by any agency, department or entity of the U.S.
 * Government are subject to restrictions of Restricted Rights for
 * Commercial Software developed at private expense as specified in FAR
 * 52.227-19 or DOD FAR Supplement 252 52.227-7013 (c) (1) (ii), as
 * applicable.
 *
 * $fiHeader: olsupport.c,v 1.2 93/03/31 10:39:10 cer Exp $
 */

/************************************************************************/
/* Support code for Openlook interface                                      */
/************************************************************************/

int ol_appl_add_item (fn, widget, parent, reference, item)
int (*fn)();
char *widget, *item;
char *parent, *reference;
{
    return (*fn)(widget, parent, reference, item);
}

int ol_list_item_pointer (x)
int x;
{
    return (OlListItemPointer(x));
}

void ol_appl_touch_item (fn, widget, token)
int (*fn)();
char *widget;
int token;
{
    (*fn)(widget, token);
}



void ol_appl_delete_item (fn, widget, token)
int (*fn)();
char *widget;
int token;
{
    (*fn)(widget, token);
}

#include "climgccursor.c"
