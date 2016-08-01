/*				-[Tue Apr 17 14:44:44 2007 by layer]-
 *
** See the file LICENSE for the full license governing this code.
 *
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
