/*
 *  Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of version 2 of the GNU General Public License as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it would be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 *  Further, this software is distributed without any warranty that it is
 *  free of the rightful claim of any third person regarding infringement
 *  or the like.  Any license provided herein, whether implied or
 *  otherwise, applies only to this software file.  Patent licenses, if
 *  any, provided herein do not apply to combinations of this program with
 *  other software, or any other product whatsoever.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with this program; if not, write the Free Software Foundation, Inc., 59
 *  Temple Place - Suite 330, Boston MA 02111-1307, USA.
 *
 */
/*
 * Most copies are okay, but there are cases where we generate
 * something like:
 * mov r1, r2
 * @p bra L1
 * add r1, r2, 1
 * L1:
 * use r1
 * The open64 register allocator would preference r1 and r2,
 * but OCG doesn't do that (I think cause use of r2 in later block),
 * so try to clean this up here.
 *
 * Note that we don't actually remove the copy here,
 * we just replace the use with the new value (e.g. add r1,r1,1), 
 * which makes it easier for OCG to later remove the copy 
 * (no longer a use after the copy).
 *
 * The basic algorithm is:
 * if opnd is source of an earlier copy,
 * and reaching def of copy dest is copy,
 * and reaching def of opnd is same as reaching def of copy's src,
 * then replace opnd with dest of copy.
*/

#include "defs.h"
#include "tracing.h"
#include "errors.h"
#include "wn.h"
#include "mempool.h"
#include "bb.h"
#include "op.h"
#include "tn.h"
#include "cg.h"
#include "cgtarget.h"
#include "reg_live.h"

static BOOL tracing = FALSE;
#define Trace(msg)	if (tracing) fprintf(TFile, msg "\n");

static MEM_POOL tn_copy_pool;
static TN_MAP tn_copy_src_op; // copy op with tn as source

void
Optimize_Copy_Usage (void)
{
  BB *bb;
  OP *op;
  OP *copy_op;
  OP *def_op;
  TN *tn;
  TN *copy_tn;

  tracing = Get_Trace(TP_EBO, 0x800);
  MEM_POOL_Initialize (&tn_copy_pool, "tn_copy_pool", FALSE);
  MEM_POOL_Push(&tn_copy_pool);
  tn_copy_src_op = TN_MAP_Create();

  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    FOR_ALL_BB_OPs (bb, op) {
      if (OP_copy(op)) {
        // what if already has copy map?
        // okay, only checking against most recent copy;
        // if not unique def, then later checks will not match.
        TN_MAP_Set (tn_copy_src_op, OP_opnd(op,0), op);
	continue; // don't replace this copy opnd
      }
      for (INT i = 0; i < OP_opnds(op); ++i) {
        tn = OP_opnd(op,i);
        if (!TN_is_register(tn)) 
          continue;
	copy_op = (OP*) TN_MAP_Get (tn_copy_src_op, tn);
	if (copy_op && tn == OP_opnd(copy_op,0)) {
          // has earlier copy
          copy_tn = OP_result(copy_op,0);
	  def_op = Find_Reaching_Def (copy_tn, op);
	  if (def_op == copy_op) {
            // copy dest reaches op
	    def_op = Find_Reaching_Def (tn, op);
	    if (def_op && def_op == Find_Reaching_Def (tn, copy_op)) {
              // tn not redefined between copy and op
              DevWarn("found src copy of tn%d in BB%d, replace with TN%d", TN_number(tn), BB_id(bb), TN_number(copy_tn));
              Set_OP_opnd(op,i, copy_tn);
            }
          }
        }
      }
    }
  }

  tn_copy_src_op = TN_MAP_Create();
  TN_MAP_Delete (tn_copy_src_op);
  MEM_POOL_Pop(&tn_copy_pool);
  MEM_POOL_Delete (&tn_copy_pool);
}

