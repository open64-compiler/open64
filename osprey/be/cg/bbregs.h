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
 * Module: bbregs.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:20-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.bbregs.h $
 *
 * Revision history:
 *  21-may-93 - Original Version
 *
 * Description:	header file for extended BB-related data structures
 * in the new implementation of GRA 
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef bbregs_INCLUDED
#define bbregs_INCLUDED

#ifdef _KEEP_RCS_ID
static char *bbregs_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.bbregs.h $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "gtn_set.td"
#include "register.h"


typedef struct bbregs {
  GTN_SET	*defreach_in;	    /* Some def reaches top */
  GTN_SET	*defreach_out;      /* Some def reaches bottom */
  GTN_SET	*live_in;           /* Some use exposed to top */
  GTN_SET	*live_out;          /* Some use exposed to bottom */
  GTN_SET       *live_use;          /* TNs with upward exposed uses in bb */
  GTN_SET       *live_def;          /* TNs defined above all uses in same bb */
} BBREGS;

/* define the access macros from BBREGS */
#define BBREGS_defreach_in(bbr)		((bbr)->defreach_in)
#define BBREGS_defreach_out(bbr)	((bbr)->defreach_out)
#define BBREGS_live_in(bbr)		((bbr)->live_in)
#define BBREGS_live_out(bbr)		((bbr)->live_out)
#define BBREGS_live_use(bbr)		((bbr)->live_use)
#define BBREGS_live_def(bbr)		((bbr)->live_def)


/* access macros from BB */
#define BB_defreach_in(bb)	BBREGS_defreach_in(BB_bbregs(bb))
#define BB_defreach_out(bb)	BBREGS_defreach_out(BB_bbregs(bb))
#define BB_live_in(bb)		BBREGS_live_in(BB_bbregs(bb))
#define BB_live_out(bb)		BBREGS_live_out(BB_bbregs(bb))
#define BB_live_use(bb)		BBREGS_live_use(BB_bbregs(bb))
#define BB_live_def(bb)		BBREGS_live_def(BB_bbregs(bb))


#endif /* bbregs_INCLUDED */
