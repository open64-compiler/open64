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


// IA-64 specific declarations

#include "tn.h"
#include "op.h"
#include "bb.h"
#include "tn_map.h"
#include "op_map.h"
#include "cxx_memory.h"

extern TN_MAP PQS_tn_map;

extern PQS_NODE_IDX PQS_TN_get_last_definition(const TN *t);
extern void         PQS_TN_set_last_definition(const TN *t, PQS_NODE_IDX p);
extern BOOL         PQS_TN_used_as_qual_pred(const TN *t);
extern void         PQS_TN_set_used_as_qual_pred(const TN *t);
extern void         PQS_TN_set_no_query(const TN *t);
extern BOOL         PQS_TN_no_query(const TN *t);
extern TN *         PQS_TN_get_tn_to_use(const TN *t);
extern void         PQS_TN_set_tn_to_use(const TN *t, const TN *to_use);

extern OP_MAP PQS_op_map;
extern void PQS_OP_set_pqs_idx(OP *op, PQS_NODE_IDX p);
extern PQS_NODE_IDX PQS_OP_get_pqs_idx(OP *op);

extern PQS_ITYPE 
PQS_classify_instruction (OP *inst, TN * &qual, TN * &p1, TN * &p2, PQS_NODE_FLAGS &flags);
