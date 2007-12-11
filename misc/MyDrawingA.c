/*				-[Thu Dec  8 13:24:37 2005 by layer]-
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
 * $Header: /repo/cvs.copy/clim2/misc/MyDrawingA.c,v 2.5.42.1 2007/12/11 14:26:53 afuchs Exp $
 */

#if defined(__alpha)
# pragma pointer_size (save)
# pragma pointer_size (long)
#endif

#include <Xm/XmP.h>
#include "MyDrawingAP.h"

/******************************************************************************/
/* This sucks */

static int (*querygeometryfunction) () = 0;

static QueryGeometry (da, intended, desired)
            XmDrawingAreaWidget da ;
            XtWidgetGeometry * intended ;
            XtWidgetGeometry * desired ;
{
    /* call out to lisp to get the result */
    if (querygeometryfunction)
	return ((*querygeometryfunction)(da, intended, desired));
    else
	return (XtGeometryYes);
}

InitializeMyDrawingAreaQueryGeometry(fn)
    int (*fn)();
{
    querygeometryfunction = fn;
}


/******************************************************************************/

externaldef( xmmydrawingareaclassrec) XmMyDrawingAreaClassRec
                     xmMyDrawingAreaClassRec =
{
   {			/* core_class fields      */
      (WidgetClass) &xmDrawingAreaClassRec,	/* superclass         */
      "XmMyDrawingArea",			/* class_name         */
      sizeof(XmMyDrawingAreaRec),		/* widget_size        */
      NULL,	        		        /* class_initialize   */
      NULL,			                /* class_part_init    */
      FALSE,					/* class_inited       */
      NULL,       			        /* initialize         */
      NULL,					/* initialize_hook    */
      XtInheritRealize,				/* realize            */
      NULL,				        /* actions	      */
      0,			                /* num_actions	      */
      NULL,				        /* resources          */
      0,			                /* num_resources      */
      NULLQUARK,				/* xrm_class          */
      TRUE,					/* compress_motion    */
      XtExposeCompressMultiple,			/* compress_exposure  */
      TRUE,					/* compress_enterlv   */
      FALSE,					/* visible_interest   */
      NULL,			                /* destroy            */
      XtInheritResize,           		/* resize             */
      XtInheritExpose,	        		/* expose             */
      NULL,                		        /* set_values         */
      NULL,					/* set_values_hook    */
      XtInheritSetValuesAlmost,	        	/* set_values_almost  */
      NULL,					/* get_values_hook    */
      NULL,					/* accept_focus       */
      XtVersion,				/* version            */
      NULL,					/* callback_private   */
      XtInheritTranslations,			/* tm_table           */
      QueryGeometry,                            /* query_geometry     */
      NULL,             	                /* display_accelerator*/
      NULL,                                     /* extension          */
   },
   {		/* composite_class fields */
      XtInheritGeometryManager,    	        /* geometry_manager   */
      XtInheritChangeManaged,	                /* change_managed     */
      XtInheritInsertChild,			/* insert_child       */
      XtInheritDeleteChild,     		/* delete_child       */
      NULL,                                     /* extension          */
   },

   {		/* constraint_class fields */
      NULL,					/* resource list        */   
      0,					/* num resources        */   
      0,					/* constraint size      */   
      NULL,					/* init proc            */   
      NULL,					/* destroy proc         */   
      NULL,					/* set values proc      */   
      NULL,                                     /* extension            */
   },

   {		/* manager_class fields */
      XtInheritTranslations,			/* translations           */
      NULL,				        /* syn_resources      	  */
      0,			                /* num_get_resources 	  */
      NULL,					/* syn_cont_resources     */
      0,					/* num_get_cont_resources */
      XmInheritParentProcess,                   /* parent_process         */
      NULL,					/* extension           */    
   },

   {		/* drawingArea class - none */     
      0						/* mumble */
   },

   {		/* mydrawingArea class - none */     
      0						/* mumble */
   }	
};

externaldef( xmmydrawingareawidgetclass) WidgetClass xmMyDrawingAreaWidgetClass
                         = (WidgetClass) &xmMyDrawingAreaClassRec ;

/****************************************************************
 * This convenience function creates and returns a MyDrawingArea widget.
 ****************/
Widget 
#ifdef _NO_PROTO
XmCreateMyDrawingArea( p, name, args, n )
        Widget p ;
        String name ;
        ArgList args ;
        Cardinal n ;
#else
XmCreateMyDrawingArea(
        Widget p,
        String name,
        ArgList args,
        Cardinal n )
#endif /* _NO_PROTO */
{
/****************/

    Widget r = XtCreateWidget( name, xmMyDrawingAreaWidgetClass, p, args, n);
    XmImRegister (r, 0);
/*    XmImGetXIC (r, XmINHERIT_POLICY, NULL, 0); */
    return r;
}

#if defined(__alpha)
# pragma pointer_size (restore)
#endif
