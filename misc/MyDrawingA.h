/*				-[Tue Apr 17 14:44:45 2007 by layer]-
 *
** See the file LICENSE for the full license governing this code.
 *
 */

#ifndef _XmMyDrawingArea_h
#define _XmMyDrawingArea_h

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Class record constants */

externalref WidgetClass xmMyDrawingAreaWidgetClass;

typedef struct _XmMyDrawingAreaClassRec * XmMyDrawingAreaWidgetClass;
typedef struct _XmMyDrawingAreaRec      * XmMyDrawingAreaWidget;


#ifndef XmIsMyDrawingArea
#define XmIsMyDrawingArea(w)  (XtIsSubclass (w, xmMyDrawingAreaWidgetClass))
#endif



/********    Public Function Declarations    ********/
#ifdef _NO_PROTO

extern Widget XmCreateMyDrawingArea() ;

#else

extern Widget XmCreateMyDrawingArea( 
                        Widget p,
                        String name,
                        ArgList args,
                        Cardinal n) ;

#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmMyDrawingArea_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
