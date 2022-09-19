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


#ifndef resource_INCLUDED
#define resource_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif



#ifdef _KEEP_RCS_ID
static char *resource_rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/util/resource.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

/* Request codes: */
typedef INT RES_REQUEST;

#define RR_Report_Current	1	/* Report current usage */
#define RR_Report_Delta		2	/* Report delta usage */
#define RR_Clear		3	/* Clear a delta summary */
#define RR_Start		4	/* Start a delta */
#define RR_Stop			5	/* Stop a delta */
#define RR_End			6	/* Transfer a delta to parent */
#define RR_Current_User		7	/* Return current user time */
#define RR_Current_System	8	/* Return current system time */
#define RR_Current_Elapsed	9	/* Return current elapsed time */
#define RR_Current_Memory	10	/* Return current memory usage */
#define RR_Delta_User		11	/* Return delta user time */
#define RR_Delta_System		12	/* Return delta system time */
#define RR_Delta_Elapsed	13	/* Return delta elapsed time */
#define RR_Delta_Memory		14	/* Return delta memory usage */

/* Typedefs */
typedef struct resources RESOURCES;
typedef struct rstate RSTATE;
typedef RSTATE *PSTATE;


/* Time structure: */
typedef struct time_info {
    INT secs;	/* Full seconds */
    INT usecs;	/* Fraction in microseconds */
} TIME_INFO;

/* External routines: */

/* Initialize base timer, etc.: */
extern void Resource_Init ( void );

/* Allocate a resource structure: */
extern RSTATE *Resource_Alloc ( char *rname, RSTATE *parent );

/* Accumulate resource usage: */
extern void Resource_Accum ( RSTATE *r, RES_REQUEST req );


/* Return a timing record from r for caller use: */
extern TIME_INFO *Get_Time ( RSTATE *r, RES_REQUEST req );

/* Return memory usage from r for caller use: */
extern INT Get_Memory ( RSTATE *r, RES_REQUEST req );

/* Return the name of a timer for caller use: */
extern char *Get_Timer_Name ( RSTATE *r );

/* Report resource usage from r (if non-NULL) or current usage,
 * according to the request function req and preceded by title:
 */
extern void Resource_Report (
    FILE *file,
    RES_REQUEST func,
    RSTATE *r,
    char *title
);

#ifdef __cplusplus
}
#endif
#endif /* resource_INCLUDED */
