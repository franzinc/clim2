#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <signal.h>
#include <stdio.h>


static Widget gc_widget = (Widget)NULL;
static Cursor widget_cursor = 0;

extern int (*gc_before)(),(*gc_after)();
static int (*old_gc_before)(),(*old_gc_after)();

static find_font_cursor (cursor)
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

	    if (XFindContext(display, root, context, &font_cursor)) {
		font_cursor = XCreateFontCursor(display, cursor);
		fprintf(stderr, "Making cursor\n");
		fflush(stderr);
		XSaveContext(display, root, context, font_cursor);
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

	if (cursor)
	    XDefineCursor(display, window, cursor);
	else
	    XUndefineCursor(display, window);
	XFlush(display);
    }
}


clim_starting_gc()
{
    if (old_gc_before) (*old_gc_before)();
    set_the_cursor(find_font_cursor(149));
}

clim_stopping_gc()
{
    set_the_cursor(widget_cursor);
    if (old_gc_after) (*old_gc_after)();
}
		      

static remove_gc_cursor (widget, client_data, call_data)
Widget widget;
Widget client_data;
int *call_data;
{
    if (gc_widget == client_data) {
	fprintf(stderr, "Removing gc cursor\n");
	fflush(stderr);
	gc_widget = 0;
    }
}

init_clim_gc_cursor_stuff(state)
int state;
{
    static int current_state = 0;
    if (state != current_state) {
	if (state) {
	    old_gc_before= old_gc_before;
	    gc_before = clim_starting_gc;
	    old_gc_after = gc_after;
	    gc_after = clim_stopping_gc;
	    signal(SIGPIPE, SIG_IGN);

	}
	else {
	    gc_before = old_gc_before;
	    gc_after = old_gc_after;
	}
    }

    current_state = state;
}

set_clim_gc_cursor_widget(widget, cursor)
Widget widget;
Cursor cursor;
{
    widget_cursor = cursor;
    if (gc_widget != widget) {
	if (widget) {
	    /* What does calling add callback repeatably do? */
	    XtRemoveCallback(widget, XtNdestroyCallback, remove_gc_cursor, widget);
	    XtAddCallback(widget, XtNdestroyCallback, remove_gc_cursor, widget);
	}
	gc_widget = widget;
    }
}


