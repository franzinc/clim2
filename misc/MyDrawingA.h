/*				-[Thu Dec  8 13:24:38 2005 by layer]-
 *
 * copyright (c) 1992-2005 Franz Inc, Berkeley, CA  All rights reserved.
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
 * $Header: /repo/cvs.copy/clim2/misc/MyDrawingA.h,v 2.5 2005/12/08 21:25:45 layer Exp $
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
