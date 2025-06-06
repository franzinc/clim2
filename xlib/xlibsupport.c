/*
** See the file LICENSE for the full license governing this code.
 *
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

//
// MAW, 06/2024: There is a bug in the Franz FFI / Apple compiler interop; basically,
// foreign functions with more than 9 args won't work, as args 9+ aren't correctly passed.
// So, this casused the infamous "scrolling" bug on the Apple Silicon versions of ptools;
// This function was hence "broken up" into xcopyarea_setargs, and xcopyarea. An ugly kludge, 
// but does the work. Franz did not commit to fix this bug (they are claiming Apple compiler 
// is doing it wrong), but maybe it gets fixed in the future. We can then remove the patched
// version and replace the original definition, which is currently commented out:
//

/*

lisp_xcopyarea(display, src, dest, gc, src_x, src_y, width, height, dest_x, dest_y )
      Display *display;
      Drawable src, dest;
      GC gc;
      int src_x, src_y;
      unsigned int width, height;
      unsigned dest_x, dest_y; 
{

  XCopyArea(display, src, dest, gc, src_x, src_y, width, height, dest_x, dest_y); 
  
}

*/ 

int src_x0, src_y0;
unsigned int width0, height0;
int dest_x0, dest_y0;

lisp_xcopyarea_setargs(src_x, src_y, width, height, dest_x, dest_y)
      int src_x, src_y;
      unsigned int width, height;
      int dest_x, dest_y;
{
  src_x0 = src_x;
  src_y0 = src_y;
  width0 = width;
  height0 = height;
  dest_x0 = dest_x;
  dest_y0 = dest_y;

} 

  
lisp_xcopyarea_patched(display, src, dest, gc)
      Display *display;
      Drawable src, dest;
      GC gc;
{

  XCopyArea(display, src, dest, gc, src_x0, src_y0, width0, height0, dest_x0, dest_y0); 
  
} 

/*

//
// MAW, 06/2024 - this is test code that was used to diagnose the FFI
// Franz / MacOS compiler bug Basically, FFI with more than 9 args are
// currently not supported. The corresponding definitions are in
// xlib-funs.lisp; commented out, but useful to keep for checking on
// the status of the Apple / Franz compiler bugfix in the future if
// the above lisp_xcopyarea_patched patch can be removed.
//

lisp_mw_test_args(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
     int a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13; 
{

  printf("\nmw_test_args: a1 %d a2 %d a3 %d a4 %d a5 %d a6 %d a7 %d a8 %d a9 %d a10 %d a11 %d a12 %d a13 %d\n\n",
	 a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13); 

} 


lisp_mw_test_args2(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
     unsigned long a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13; 
{

  printf("\nmw_test_args: a1 %d a2 %d a3 %d a4 %d a5 %d a6 %d a7 %d a8 %d a9 %d a10 %d a11 %d a12 %d a13 %d\n\n",
	 a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13); 

} 


int lisp_mw_test(int x) {

  return 42+x;
  
}

int lisp_mw_test2(Display *dpy) {

  return 42;
  
}

Display* lisp_mw_test3(Display *dpy) {

  return dpy; 
  
}

lisp_mw_test_copy(display, src, dest, gc, src_x, src_y, width, height,  dest_x, dest_y)
      Display *display;
      Drawable src, dest;
      GC gc;
      int src_x, src_y;
      unsigned int width, height;
      int dest_x, dest_y;
{

  printf("src_x %d src_y %d width %d height %d dest_x %d dest_y %d\n",
	 src_x, src_y,
	 width, height, 
	 dest_x, dest_y); 
} 

*/ 
