/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */
                                                                                
                                                                                
/* ====================================================================
 * ====================================================================
 *
 * Module: cg_misc.cxx
 *
 * Description:
 *      Misc routines in support of x86 code generation.
 *
 * ====================================================================
 * ====================================================================
 */

#include <alloca.h>
#include "defs.h"
#include "config.h"
#include "mempool.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "pu_info.h"
#include "cg.h"
#include "cg_flags.h"
#include "ttype.h"
#include "targ_sim.h"
#include "bb_set.h"
#include "freq.h"
#include "cgtarget.h"
#include "whirl2ops.h"
#include "dominate.h"
#include "findloops.h"
#include "cg_vector.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "data_layout.h"
#include "op.h"
#include "cflow.h"
#include "reg_live.h"

// Reduce the amount of precision in a x87 result by storing it to and
// reloading it from memory.  Do this for float and double types.
void
Add_Float_Stores ()
{
  OP *op, *next;
  BB *bb;
  int i;
  ST *mem4, *mem8;

  mem4 = NULL;
  mem8 = NULL;

  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    for (op = BB_first_op(bb); op != NULL; op = next) {
      next = OP_next(op);
      // Skip non-x87 OPs and x87 loads and stores.
      if (!TOP_is_x87(OP_code(op)) ||
	  OP_load(op) ||
	  OP_store(op)) {
	continue;
      }

      for (i = 0; i < OP_results(op); i++) {
        TN *tn = OP_result(op, i);
	if (!TN_is_float(tn))
	  continue;
	Is_True(TN_register_class(tn) == ISA_REGISTER_CLASS_x87,
		("Add_Float_Stores: x87 result uses non-x87 reg class"));

	// Add store if result is float or double.

	ST **mem_p;
	if (TN_size(tn) == 4)
	  mem_p = &mem4;
	else if (TN_size(tn) == 8)
	  mem_p = &mem8;
	else
	  continue;

	// Allocate memory ST.
	if (*mem_p == NULL) {
	  TY_IDX ty = MTYPE_To_TY(TN_size(tn) == 4 ?
				    Spill_Float32_Mtype : Spill_Float_Mtype);
	  *mem_p = Gen_Temp_Symbol(ty, "x87_store");
	}

	// Generate store and load.
	OPS ops = OPS_EMPTY;
	OP *new_op;
	CGTARG_Store_To_Memory(tn, *mem_p, &ops);
	Set_OP_volatile(OPS_first(&ops));	// prevent EBO elimination
	CGTARG_Load_From_Memory(tn, *mem_p, &ops);
	FOR_ALL_OPS_OPs(&ops, new_op) OP_srcpos(new_op) = OP_srcpos(op);
	BB_Insert_Ops_After(bb, op, &ops);
      }
    }
  }
}
