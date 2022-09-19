/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#ifdef _KEEP_RCS_ID
static const char source_file[] = __FILE__;
static const char rcs_id[] = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/util/resource.c,v $ $Revision: 1.1.1.1 $";
#endif

#if (1)
# include <sys/time.h>
# include <sys/resource.h>
  typedef struct rusage time_buf_t;
#else
# include <sys/param.h>
# include <sys/types.h>
# include <sys/times.h>
# include <time.h>
  typedef struct tms time_buf_t;
#endif

#include <unistd.h>
#include <string.h>
/* Hack: These were defined in sys/param.h and will be redefined in defs.h
 * if we don't prevent it.  TODO: Really defs.h should check to see if they are
 * defined before defining them.
 */
#ifdef MAX
#undef MAX
#endif
#ifdef MIN
#undef MIN
#endif

#include "defs.h"
#include "resource.h"


/* ====================================================================
 *
 * Basic internal data structures:
 *
 * ====================================================================
 */

/* The following structure may contain absolute or delta information: */
struct resources {
    TIME_INFO	utime;	/* User cpu time */
    TIME_INFO	stime;	/* System cpu time */
    TIME_INFO	etime;	/* Elapsed time */
    /* 32 bits are enought to hold the value of memory usage. However, 
     * in the process of keeping track of memory usage, <memory> is 
     * used sometimes to hold the value of pointer. e.g the value 
     * return from sbrk(), therefore, memory should be 64bits long 
     * on 64bit environment. 
     */
    INTPTR memory;	/* Memory used (bytes) */
    INTPTR freemem;	/* Free memory allocated */
};

/* The following structure contains the latest absolute and the
 * accumulated delta information.  It is the basic object exported
 * to the world, although they don't know it.
 */
struct rstate {
    RESOURCES cur;	/* The latest absolute information */
    RESOURCES del;	/* The accumulated delta information */
    struct rstate *dad;	/* The parent structure in hierarchy */
    char *name;		/* The resource record name */
};

static void Clear_Resource ( RESOURCES *r );
static void Get_Resources ( RESOURCES *r );
static void Get_Delta_Time ( TIME_INFO *current,
                        TIME_INFO *base,
                        TIME_INFO *delta );
static void Get_Delta_Resource ( RESOURCES *current,
                            RESOURCES *base,
                            RESOURCES *delta );
static void Accum_Delta_Time ( TIME_INFO *delta, TIME_INFO *summary );
static void Accum_Delta_Resource ( RESOURCES *delta,
                              RESOURCES *summary );

/* We need a few buffers for internal use: */
static time_buf_t tbuf;
static RESOURCES curtime, deltime;
static RSTATE runtime;
#if (1)
static struct timeval start_time;
#else
static INT start_time;	/* Initial date/time */
#endif
static INT initialized = 0;

/* ====================================================================
 *
 * Clear_Resource
 *
 * Clear all components of a resource structure.
 *
 * ====================================================================
 */

static void
Clear_Resource (
    RESOURCES *r
)
{
    r->utime.secs  = 0;
    r->utime.usecs = 0;
    r->stime.secs  = 0;
    r->stime.usecs = 0;
    r->etime.secs  = 0;
    r->etime.usecs = 0;
    r->memory	   = 0;
    r->freemem	   = 0;
}

/* ====================================================================
 *
 * Get_Resources
 *
 * Fill the RESOURCES structure passed with the current time/state.
 *
 * ====================================================================
 */

static void
Get_Resources (
    RESOURCES *r
)
{
#if (1)
    struct timeval now;
#else
    INT secs, frac;
#endif

    /* Initialize if the user didn't: */
    if ( ! initialized ) Resource_Init ();

    /* Get the elapsed time: */
#if (1)
#ifndef linux
    gettimeofday(&now);
#else
    gettimeofday(&now, NULL);
#endif
    r->etime.secs  = now.tv_sec  - start_time.tv_sec;
    r->etime.usecs = now.tv_usec - start_time.tv_usec;
#else
    r->etime.secs  = time (0) - start_time;
    r->etime.usecs = 0;
#endif

    /* Get the CPU time information from the system: */
#if (1)
    getrusage (RUSAGE_SELF, &tbuf);
#else
    (void) times (&tbuf);
#endif

    /* Transfer it to caller's structure: */
#if (1)
    r->utime.secs  = tbuf.ru_utime.tv_sec;
    r->utime.usecs = tbuf.ru_utime.tv_usec;
    r->stime.secs  = tbuf.ru_stime.tv_sec;
    r->stime.usecs = tbuf.ru_stime.tv_usec;
#else
    secs = tbuf.tms_utime / HZ;
    frac = tbuf.tms_utime - (secs*HZ);
    r->utime.secs  = secs;
    r->utime.usecs = frac * (1000000/HZ);
    secs = tbuf.tms_stime / HZ;
    frac = tbuf.tms_stime - (secs*HZ);
    r->stime.secs  = secs;
    r->stime.usecs = frac * (1000000/HZ);
#endif

    /* Get the memory information */
    r->memory = (INTPTR)sbrk(0);
    r->freemem = 0;
}

/* ====================================================================
 *
 * Get_Delta_Time
 *
 * Set delta to the time difference between current and base.
 *
 * ====================================================================
 */

static void
Get_Delta_Time (
    TIME_INFO *current,	/* This is current time */
    TIME_INFO *base,	/* Calculate delta from this base */
    TIME_INFO *delta    /* Put delta here */
)
{
    delta->secs  = current->secs  - base->secs;
    delta->usecs = current->usecs - base->usecs;
    if ( delta->usecs < 0 ) {
	delta->usecs += 1000000;
	delta->secs --;
    }
}


/* ====================================================================
 *
 * Get_Delta_Resource
 *
 * Calculate the delta resource usage from a given base.
 *
 * ====================================================================
 */

static void
Get_Delta_Resource (
    RESOURCES *current,	/* Put current state here */
    RESOURCES *base,	/* Calculate delta from this base */
    RESOURCES *delta	/* Put delta here */
)
{
    Get_Resources ( current );

    Get_Delta_Time ( &current->utime, &base->utime, &delta->utime );
    Get_Delta_Time ( &current->stime, &base->stime, &delta->stime );
    Get_Delta_Time ( &current->etime, &base->etime, &delta->etime );
    delta->memory  = current->memory  - base->memory;
    delta->freemem = current->freemem - base->freemem;
}

/* ====================================================================
 *
 * Accum_Delta_Time
 *
 * Accumulate a delta time into a summary record.
 *
 * ====================================================================
 */

static void
Accum_Delta_Time (
    TIME_INFO *delta,	/* Add this delta ... */
    TIME_INFO *summary	/* ... to this summary record */
)
{
    summary->secs  += delta->secs;
    summary->usecs += delta->usecs;
    if ( summary->usecs >= 1000000 ) {
	summary->usecs -= 1000000;
	summary->secs ++;
    }
}


/* ====================================================================
 *
 * Accum_Delta_Resource
 *
 * Accumulate a delta resource record into a summary record.
 *
 * ====================================================================
 */

static void
Accum_Delta_Resource (
    RESOURCES *delta,	/* Add this delta ... */
    RESOURCES *summary	/* ... to this summary record */
)
{
    Accum_Delta_Time ( &delta->utime, &summary->utime );
    Accum_Delta_Time ( &delta->stime, &summary->stime );
    Accum_Delta_Time ( &delta->etime, &summary->etime );
    summary->memory  = MAX(summary->memory,delta->memory);
    summary->freemem += delta->freemem;
}

/* ====================================================================
 *
 * Resource_Init
 *
 * Initialize the resource module.  Currently means just initializing
 * the start_time and runtime variables.
 *
 * ====================================================================
 */

void
Resource_Init ( void )
{
    /* Initialize elapsed time base: */
#if (1)
#ifndef linux
    gettimeofday(&start_time);
#else
    gettimeofday(&start_time, NULL);
#endif
#else
    start_time = time (0);
#endif
    initialized = 1;

    /* Initialize the process initialization structure: */
    runtime.name = "Process";
    Get_Resources ( &runtime.cur );
}


/* ====================================================================
 *
 * Resource_Alloc
 *
 * Allocate and initialize a resource structure.
 *
 * ====================================================================
 */

RSTATE *
Resource_Alloc (
    char *rname,
    RSTATE *parent
)
{
    RSTATE *r;

    r = (RSTATE *) calloc ( 1, sizeof(RSTATE) );
    r->name = rname;
    r->dad  = parent;

    return r;
}

/* ====================================================================
 *
 * Resource_Accum
 *
 * Accumulate resource information.  Valid requests are:
 *   RR_Clear:	Clear the delta information.
 *   RR_Start:	Start a new delta period (i.e. set current state).
 *   RR_Stop:	Add a delta period to the summary.
 *   RR_End:	Accumulate delta information to parent and clear.
 * The default for an unrecognized request is RR_Start.  Note that if
 * there is no parent, RR_End is equivalent to RR_Clear.  All of the
 * requests set current state.
 *
 * See Resource_Report and Resource_Time for reporting possibilities.
 *
 * ====================================================================
 */

void
Resource_Accum ( RSTATE *r, RES_REQUEST req )
{
    /* If passed a NULL pointer, just return: */
    if ( ! r ) return;

    /* Get the current state: */
    Get_Delta_Resource ( &curtime, &r->cur, &deltime );

    /* Do the right thing: */
    switch ( req ) {
	case RR_Clear:	Clear_Resource ( &r->del );
			break;
	case RR_Start:	break;
	case RR_Stop:	Accum_Delta_Resource ( &deltime, &r->del );
			break;
	case RR_End:	if ( r->dad ) {
			    Accum_Delta_Resource ( &r->del, &r->dad->del );
			    Clear_Resource ( &r->del );
			}
			break;
    }

    /* Reset the current state: */
    r->cur = curtime;
}

/* ====================================================================
 *
 * Get_Time
 *
 * Return a pointer to the requesting timing structure.  Returns NULL
 * if the request is not recognized.
 *
 * ====================================================================
 */

TIME_INFO *
Get_Time (
    RSTATE *r,
    RES_REQUEST req
)
{
    switch ( req ) {
	case RR_Current_User:		return &(r->cur.utime);
	case RR_Current_System:		return &(r->cur.stime);
	case RR_Current_Elapsed:	return &(r->cur.etime);
	case RR_Delta_User:		return &(r->del.utime);
	case RR_Delta_System:		return &(r->del.stime);
	case RR_Delta_Elapsed:		return &(r->del.etime);
    }
    return 0;
}


/* ====================================================================
 *
 * Get_Timer_Name
 *
 * Return the name string for a resource record.
 *
 * ====================================================================
 */

char *
Get_Timer_Name (
    RSTATE *r
)
{
    return r->name;
}

/* ====================================================================
 *
 * Get_Memory
 *
 * Return the current or delta memory usage.
 *
 * ====================================================================
 */

INT
Get_Memory (
    RSTATE *r,
    RES_REQUEST req
)
{
    switch ( req ) {
	case RR_Current_Memory:		return r->cur.memory;
	case RR_Delta_Memory:		return r->del.memory;
    }
    return 0;
}

/* ====================================================================
 *
 * Resource_Report
 *
 * Report resource usage.  If the RSTATE structure pointer passed
 * is non-null, report its current or delta content, as requested.
 * If it is null, report the current process state.
 *
 * ====================================================================
 */

void
Resource_Report (
    FILE *file,
    RES_REQUEST func,
    RSTATE *r,
    char *title
)
{
    RESOURCES *res;

    /* Deal with NULL r: */
    if ( ! r ) {
	res = &curtime;
	Get_Resources (res);

    /* Otherwise select reportable structure using func: */
    } else {
	switch ( func ) {
	    case RR_Report_Delta:	res = &(r->del);
					break;
	    case RR_Report_Current:
	    default:			res = &(r->cur);
					break;
	}
    }

    /* Report: */
    if ( title && *title ) fprintf ( file, "%s\n", title );
    fprintf ( file,
#if (1)
      "\tuser:\t%4d.%06d\n\tsystem:\t%4d.%06d\n\telapsed: %4d.%06d\n",
	      res->utime.secs, res->utime.usecs,
	      res->stime.secs, res->stime.usecs,
	      res->etime.secs, res->etime.usecs );
#else
      "\tuser:\t%4d.%03d\n\tsystem:\t%4d.%03d\n\telapsed: %4d.%02d\n",
	      res->utime.secs, res->utime.usecs/1000,
	      res->stime.secs, res->stime.usecs/1000,
	      res->etime.secs, res->etime.usecs/10000 );
#endif
    fprintf ( file, "\tmemory:\t%8x\n\tfree:\t%8x\n",
	      (INT)res->memory, (INT)res->freemem);
}

