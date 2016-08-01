/*				-[Tue Apr 17 14:44:45 2007 by layer]-
 *
** See the file LICENSE for the full license governing this code.
 *
 */

#ifndef _XmMyDrawingAreaP_h
#define _XmMyDrawingAreaP_h

#include <Xm/DrawingAP.h>
#include "MyDrawingA.h"

#ifdef __cplusplus
extern "C" {
#endif

/*  New fields for the MyDrawingArea widget class record  */

typedef struct
{
   int mumble;   /* No new procedures */
} XmMyDrawingAreaClassPart;


/* Full class record declaration */

typedef struct _XmMyDrawingAreaClassRec
{
	CoreClassPart		 core_class;
	CompositeClassPart	 composite_class;
	ConstraintClassPart	 constraint_class;
	XmManagerClassPart	 manager_class;
	XmDrawingAreaClassPart	 drawing_area_class;
	XmMyDrawingAreaClassPart my_drawing_area_class;
} XmMyDrawingAreaClassRec;


externalref XmMyDrawingAreaClassRec xmMyDrawingAreaClassRec;


/* New fields for the MyDrawingArea widget record */

typedef struct
{
    int dummy;			/* Don't really need any at this point. */
} XmMyDrawingAreaPart;


/* Full instance record declaration */

typedef struct _XmMyDrawingAreaRec
{
	CorePart		core;
	CompositePart		composite;
	ConstraintPart		constraint;
	XmManagerPart		manager;
	XmDrawingAreaPart	drawing_area;
	XmMyDrawingAreaPart	my_drawing_area;
} XmMyDrawingAreaRec;


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmMyDrawingAreaP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
