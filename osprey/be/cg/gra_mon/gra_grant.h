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

//  Grants of local registers
/////////////////////////////////////
//  
//  Description:
//
//      GRA allocates registers for locals as well as globals.  But it doesn't
//      actually assign locals to registers, this is the job of local register
//      allocation.  Instead it <grants> registers in each block which the
//      local register allation may use.  Local register allocation may
//      additionally use any other register in the block, but at the cost of
//      spilling and restoring the register at the top and bottom of the block.
//
//      This module contains a really very simple mechanism to maintain the
//      information about grants and communicate it to the local register
//      allocator.
//
//  Exported functions:
//
//      void GRA_GRANT_Initialize(void)
//          Call to allocate the grant mapping.
//
//      void GRA_GRANT_Finalize(void)
//          Call to delete the grant mapping.
//
//      void GRA_GRANT_Local_Register( GRA_BB* gbb, ISA_REGISTER_CLASS rc,
//                                                  REGISTER           reg )
//          Call to grant the given <reg> and <rc> in <bb>.
//
//      REGISTER_SET GRA_GRANT_Get_Local_Registers( BB*                bb,
//                                                  ISA_REGISTER_CLASS rc )
//          Return the registers granted for locals in the given <bb> and <rc>.
//
//      void GRA_GRANT_Transfer( BB* from_bb, BB* to_bb )
//
//          Transfer any local register grants from <from_bb> to <to_bb>.
//
//      void GRA_GRANT_Unused_Caller_Saved( void )
//
//          Grant any unused caller saved registers in each block to the
//	    local register allocator, regardless of its request.  This will
//	    be of some help to GCM.
//
/////////////////////////////////////


//  $Revision: 1.2 $
//  $Date: 02/11/07 23:41:29-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_grant.h $


#ifndef GRA_GRANT_INCLUDED
#define GRA_GRANT_INCLUDED

#ifndef GRA_GRANT_RCS_ID
#define GRA_GRANT_RCS_ID
#ifdef _KEEP_RCS_ID
static char *gra_grant_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_grant.h $ $Revision: 1.2 $";
#endif
#endif

#include "bb.h"
#include "register.h"
#include "gra_bb.h"

extern void
GRA_GRANT_Initialize(void);
extern void 
GRA_GRANT_Finalize(void);
extern void 
GRA_GRANT_Local_Register( GRA_BB* gbb, ISA_REGISTER_CLASS rc, REGISTER reg );
extern REGISTER_SET 
GRA_GRANT_Get_Local_Registers( BB* bb, ISA_REGISTER_CLASS rc );
extern void
GRA_GRANT_Transfer( BB* from_bb, BB* to_bb );

extern void
GRA_GRANT_Unused_Caller_Saved(void);

#ifdef TARG_SL2 //minor_reg_alloc
extern void 
GRA_GRANT_REGISTER_SET_Set_For_BB(BB * bb, ISA_REGISTER_CLASS rc, REGISTER_SET reg_set);
#endif 
#endif
