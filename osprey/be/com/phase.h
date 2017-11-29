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


/* ====================================================================
 * ====================================================================
 *
 * Module: phase.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:38-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.phase.h $
 *
 * Revision history:
 *  17-Feb-95 - Original Version
 *
 * Description:
 *   Phase-specific functions and data structures that the common driver
 *   need to know.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef phase_INCLUDED
#define phase_INCLUDED

typedef enum {
    PHASE_COMMON,		    /* non-phase specific */
    PHASE_LNO,			    /* loop-nest optimizer */
    PHASE_WOPT,			    /* global optimizer  */
    PHASE_CG,			    /* code generator */
    PHASE_W2C,	                    /* whirl2c */
    PHASE_W2F,	                    /* whirl2f */
    PHASE_IPL,			    /* ipl */
    PHASE_COUNT
} BE_PHASES;

typedef struct {
    const char *group_name;	    /* option group name */
    INT group_name_length;	    /* string length of group_name */
    BE_PHASES phase;		    /* the phase where this group belongs */
} PHASE_SPECIFIC_OPTION_GROUP;

extern PHASE_SPECIFIC_OPTION_GROUP phase_ogroup_table[];

#endif /* phase_INCLUDED */
