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


#include <stdio.h>
#include "ti_si.h"
#include "targ_isa_properties.h"
#include "targ_isa_hazards.h"
#include "targ_isa_subset.h"
#include "ti_errors.h"
#include "ti_latency.h"


/* ====================================================================
 *
 *  TI_LATENCY_Commit_Cycle
 *
 *  See interface description
 *
 *  If we are asking this question, we are asking it about a branch and we
 *  really just want to know if it has a delay slot (and how many).
 *  This allows for delay slot filling.
 *
 * ====================================================================
 */
INT TI_LATENCY_Commit_Cycle(
  TOP opcode
)
{
  if ( opcode == TOP_intrncall ) return 1;
  if ( TOP_is_xfer(opcode) ) return 2;
  if ( TOP_is_select(opcode) && TOP_is_simulated(opcode) ) return 1;
  return 0;
}

/* ====================================================================
 *
 *  TI_LATENCY_Operand_Hazard
 *
 *  See interface description
 *
 * ====================================================================
 */
INT TI_LATENCY_Operand_Hazard(
  TOP  opcode,
  INT *opnd,
  INT *error
)
{
  ISA_HAZARD_INFO *info;
  INT              ops = 0;
  *error = TI_RC_OKAY;

  for ( info = ISA_HAZARD_First(opcode);
	info != NULL;
	info = ISA_HAZARD_Next(info) 
  ) {
    if ( ISA_HAZARD_Type(info) == ISA_HAZARD_operand ) {
      INT this_ops;
      INT pre_ops  = ISA_HAZARD_Pre_Ops(info);
      INT post_ops = ISA_HAZARD_Post_Ops(info);

      if ( pre_ops ) {
	if (post_ops != 0) {
		sprintf(TI_errmsg, "found both a pre- and post-hazard for %s",
			 TOP_Name(opcode));
  		*error = TI_RC_ERROR;
		return 0;
	}
	this_ops = -pre_ops;
      } else if ( post_ops ) {
	this_ops = post_ops;
      } else {
	this_ops = 0;
      }

      if ( this_ops ) {
	if (ops != 0) {
		sprintf(TI_errmsg, "multiple operand hazards for %s",
			 TOP_Name(opcode));
  		*error = TI_RC_ERROR;
		return 0;
	}
	ops = this_ops;
	*opnd = ISA_HAZARD_Data(info);
      }
    }
  }

  return ops;
}


/* ====================================================================
 *
 *  TI_LATENCY_Result_Hazard
 *
 *  See interface description
 *
 * ====================================================================
 */
INT TI_LATENCY_Result_Hazard(
  TOP opcode,
  INT *result,
  INT *error
)
{
  ISA_HAZARD_INFO *info;
  INT              ops = 0;
  *error = TI_RC_OKAY;

  for ( info = ISA_HAZARD_First(opcode);
	info != NULL;
	info = ISA_HAZARD_Next(info) 
  ) {
    if ( ISA_HAZARD_Type(info) == ISA_HAZARD_result ) {
      INT pre_ops  = ISA_HAZARD_Pre_Ops(info);
      INT post_ops = ISA_HAZARD_Post_Ops(info);

      if (pre_ops != 0) {
	sprintf(TI_errmsg, "found a result pre-hazard for %s",
		 TOP_Name(opcode));
  	*error = TI_RC_ERROR;
	return 0;
      }

      if ( post_ops ) {
      	if (ops != 0) {
		sprintf(TI_errmsg, "multiple result hazards for %s",
			 TOP_Name(opcode));
  		*error = TI_RC_ERROR;
		return 0;
      	}
	ops = post_ops;
	*result = ISA_HAZARD_Data(info);
      }
    }
  }

  return ops;
}


/* ====================================================================
 *
 *  TI_LATENCY_Errata_Hazard
 *
 *  See interface description
 *
 * ====================================================================
 */
INT TI_LATENCY_Errata_Hazard(
  TOP  opcode,
  INT *number,
  INT *error
)
{
  ISA_HAZARD_INFO *info;
  INT              ops = 0;
  *error = TI_RC_OKAY;

  for ( info = ISA_HAZARD_First(opcode);
	info != NULL;
	info = ISA_HAZARD_Next(info) 
  ) {
    if ( ISA_HAZARD_Type(info) == ISA_HAZARD_errata ) {
      INT pre_ops  = ISA_HAZARD_Pre_Ops(info);
      INT post_ops = ISA_HAZARD_Post_Ops(info);

      if (pre_ops != 0) {
	sprintf(TI_errmsg, "found an errata pre-hazard for %s",
		 TOP_Name(opcode));
  	*error = TI_RC_ERROR;
	return 0;
      }

      if ( post_ops ) {
      	if (ops != 0) {
		sprintf(TI_errmsg, "multiple errata hazards for %s",
			 TOP_Name(opcode));
  		*error = TI_RC_ERROR;
		return 0;
      	}
	ops = post_ops;
	*number = ISA_HAZARD_Data(info);
      }
    }
  }

  return ops;
}
