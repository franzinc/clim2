/*
** See the file LICENSE for the full license governing this code.
 *
 * $Id: xlibsupport.c,v 2.6 2007/04/17 21:45:54 layer Exp $
 */

/************************************************************************/
/* Support code for Xlib interface                                      */
/************************************************************************/

#include <X11/Xlib.h>

lisp_XDrawString(dpy, d, gc, x, y, string, start, end)
    register Display *dpy;
    Drawable d;
    GC gc;
    int x, y;
    register char *string;
    register int start, end;
{
    XDrawString(dpy, d, gc, x, y, &string[start], end - start);
}

lisp_XDrawString16(dpy, d, gc, x, y, string, start, end)
    register Display *dpy;
    Drawable d;
    GC gc;
    int x, y;
    register XChar2b *string;
    register int start, end;
{
    XDrawString16(dpy, d, gc, x, y, &string[start], end - start);
}
