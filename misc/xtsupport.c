/* $fiHeader: xtsupport.c,v 1.9 1993/12/04 00:32:10 duane Exp $ */

#include <X11/Intrinsic.h>

#include <signal.h>
#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include "IntrinsicI.h"



/* Some systems running NTP daemons are known to return strange usec
 * values from gettimeofday.  At present (3/90) this has only been
 * reported on SunOS...
 */

#ifndef NEEDS_NTPD_FIXUP
# if defined(sun) || defined(MOTOROLA)
#  define NEEDS_NTPD_FIXUP 1
# else
#  define NEEDS_NTPD_FIXUP 0
# endif
#endif

#if NEEDS_NTPD_FIXUP
#define FIXUP_TIMEVAL(t) { \
	while ((t).tv_usec >= 1000000) { \
	    (t).tv_usec -= 1000000; \
	    (t).tv_sec++; \
	} \
	while ((t).tv_usec < 0) { \
	    if ((t).tv_sec > 0) { \
		(t).tv_usec += 1000000; \
		(t).tv_sec--; \
	    } else { \
		(t).tv_usec = 0; \
		break; \
	    } \
	}}
#else
#define FIXUP_TIMEVAL(t)
#endif /*NEEDS_NTPD_FIXUP*/



/*
 * Private routines
 */
#define ADD_TIME(dest, src1, src2) { \
	if(((dest).tv_usec = (src1).tv_usec + (src2).tv_usec) >= 1000000) {\
	      (dest).tv_usec -= 1000000;\
	      (dest).tv_sec = (src1).tv_sec + (src2).tv_sec + 1 ; \
	} else { (dest).tv_sec = (src1).tv_sec + (src2).tv_sec ; \
	   if(((dest).tv_sec >= 1) && (((dest).tv_usec <0))) { \
	    (dest).tv_sec --;(dest).tv_usec += 1000000; } } }


#define TIMEDELTA(dest, src1, src2) { \
	if(((dest).tv_usec = (src1).tv_usec - (src2).tv_usec) < 0) {\
	      (dest).tv_usec += 1000000;\
	      (dest).tv_sec = (src1).tv_sec - (src2).tv_sec - 1;\
	} else 	(dest).tv_sec = (src1).tv_sec - (src2).tv_sec;  }

#define IS_AFTER(t1, t2) (((t2).tv_sec > (t1).tv_sec) \
	|| (((t2).tv_sec == (t1).tv_sec)&& ((t2).tv_usec > (t1).tv_usec)))

#define IS_AT_OR_AFTER(t1, t2) (((t2).tv_sec > (t1).tv_sec) \
	|| (((t2).tv_sec == (t1).tv_sec)&& ((t2).tv_usec >= (t1).tv_usec)))


/*
 * This is what it's all for
 */


unsigned long XtAppIntervalNextTimer(app)
    XtAppContext app;
{
    struct timeval  cur_time;
    struct timeval  wait_time;

    (void) gettimeofday (&cur_time, NULL);
    FIXUP_TIMEVAL(cur_time);

    if(app->timerQueue != NULL && IS_AFTER(cur_time, app->timerQueue->te_timer_value))
    {
	TIMEDELTA (wait_time, app->timerQueue->te_timer_value, cur_time);
	return (wait_time.tv_sec*1000+wait_time.tv_usec/1000);
    }
    else 
	return -1;
}


xt_widget_num_popups (w)
Widget w;
{
    return (w->core.num_popups);
}
