#if defined(__alpha)
# pragma pointer_size (save)
# pragma pointer_size (long)
#endif

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <signal.h>
#include <X11/cursorfont.h>


#if defined(__alpha)
# pragma pointer_size (restore)
#endif

static Widget gc_widget = (Widget)NULL;
static Cursor widget_cursor = 0;

/* extern int (*gc_before)(),(*gc_after)(); */
/* static int (*old_gc_before)(),(*old_gc_after)(); */

static Cursor find_font_cursor (cursor)
int cursor;
{
    Window window;

    if (gc_widget && (window = XtWindow(gc_widget))) {
	Display *display = XtDisplayOfObject(gc_widget);
	if (cursor) {
	    Cursor font_cursor;
	    static XContext context = 0;
	    Window root = RootWindowOfScreen(XtScreen(gc_widget));

	    if (!context) context = XUniqueContext();
	    if (XFindContext(display, root, context, (XPointer *)&font_cursor)) {
		font_cursor = XCreateFontCursor(display, cursor);
		XSaveContext(display, root, context, (XPointer)font_cursor);
	    }
	    return (font_cursor);
	}
    }
    return (0);
}


static set_the_cursor (cursor)
Cursor cursor;
{
    Window window;

    if (gc_widget && (window = XtWindow(gc_widget))) {
	Display *display = XtDisplayOfObject(gc_widget);
	XDefineCursor(display, window, cursor);
	XFlush(display);
    }
}


clim_starting_gc()
{
  /* if (old_gc_before) (*old_gc_before)(); */
  set_the_cursor(find_font_cursor(XC_watch));
  return 0;
}

clim_stopping_gc()
{
  set_the_cursor(widget_cursor);
  return 0;
  /* if (old_gc_after) (*old_gc_after)(); */
}
		      

static remove_gc_cursor (widget, client_data, call_data)
Widget widget;
Widget client_data;
int *call_data;
{
    if (gc_widget == client_data) {
	gc_widget = 0;
    }
}

init_clim_gc_cursor_stuff(vec)
     int (*vec[])();
{
  if (vec) {
    vec[0] = clim_starting_gc;
    vec[1] = clim_stopping_gc;
  }
  /* Currently nothing to do to stop gc cursor. */
}


set_clim_gc_cursor_widget(widget, cursor)
Widget widget;
Cursor cursor;
{
    widget_cursor = cursor;
    if (gc_widget != widget) {
	if (widget) {
	    /* What does calling add callback repeatedly do? */
	    XtRemoveCallback(widget, XtNdestroyCallback, (XtCallbackProc) remove_gc_cursor, widget);
	    XtAddCallback(widget, XtNdestroyCallback, (XtCallbackProc) remove_gc_cursor, widget);
	}
	gc_widget = widget;
    }
}


