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
 * Commercial Software developed at private expense as specified in 
 * DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
 *
 * $Header: /repo/cvs.copy/clim2/misc/olsupport.c,v 1.7 1997/02/05 01:49:13 tomj Exp $
 */

/************************************************************************/
/* Support code for Openlook interface                                  */
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
