/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* USMID @(#) libu/ffio/event_parse.h	92.1	10/07/99 22:14:06 */

/*  EVENT LAYER ASSIGN PARSING DEFINITION */

#include "evntio.h"
#define NUM_EVENT_ALIAS    0
#define NUM_EVENT_NUMERICS 0

/* The undocumented "trace" parameter is accepted on PVP systems. */
/* It has been informally supported there, so that internal people could */
/* use application's tools for examining it. I have no idea whether */
/* it still works there. This tool isn't available on the T3E. It is */
/* available on Irix, but the format of the trace file has changed - so */
/* if we ever support "trace" there, changes will be required (7/17/97) */
struct LAYER_OPTS _event_opts[] = {
 CLASS_EVENT, _STR1M, _INFO1_STR1(TR_EVNT_NOSTAT),   0, 0, _STR1M, 0, 0, "nostat" ,
 CLASS_EVENT, _STR1M, _INFO1_STR1(TR_EVNT_SUMMARY),  0, 0, _STR1M, 0, 0, "summary" ,
 CLASS_EVENT, _STR1M, _INFO1_STR1(TR_EVNT_BRIEF),    0, 0, _STR1M, 0, 0, "brief",
#if !defined(__mips) && !defined(_CRAYT3E) && !defined(_LITTLE_ENDIAN)
 CLASS_EVENT, _STR2M, _INFO1_STR2(TR_EVNT_TRACE),    0, 0, _STR2M, 0, 0, "trace"
#endif
} ;


#define NUM_EVENT_OPTS     (sizeof(_event_opts)/sizeof(struct LAYER_OPTS))

struct LAYER_DATA _event_data =
    {
         CLASS_EVENT ,
         FLD_TYPE,
         "event",
         "" ,
         0,
         0,
         NUM_EVENT_OPTS,
         1 ,
         NUM_EVENT_NUMERICS,
         NUM_EVENT_ALIAS ,
         _event_opts ,
	 NULL,
	 NULL,
         NULL
    } ;

