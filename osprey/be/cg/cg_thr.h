/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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
* Module: cg_thr.h
* $Revision: 1.6 $
* $Date: 05/12/05 08:59:04-08:00 $
* $Author: bos@eng-24.pathscale.com $
* $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_thr.h $
*
* Description:
*
* Interface to the CG Tree-Height Reduction Phase.
*
* ====================================================================
* ====================================================================
*/

#ifndef cg_thr_INCLUDED
#define cg_thr_INCLUDED

#include "bb_set.h"
#include "cg_vector.h"
#include "ti_res_res.h"
#include "tn_map.h"
#include "hb_sched.h"
#include "cg_dep_graph.h"

// Exported interfaces
#define THR_NONE                                       0x0000
#define THR_DATA_SPECULATION_NO_RB                     0x0001
#define THR_DATA_SPECULATION_RB                        0x0002
#define THR_DATA_SPECULATION_NO_RB_CLEANUP             0x0004
#define THR_ALL                                        0x1111

typedef UINT16 THR_TYPE;

extern BOOL Trace_THR;
extern void Remove_Unnecessary_Check_Instrs(BB *bb);

// Misc flags

// Accessors:

// ====================================================================
//
// CG_THR:
//
// This class provides the infra-structure to perform various tree-height
// reduction tasks.
//
// ====================================================================
class CG_THR {
private:
  // private functions:
  MEM_POOL     _thr_pool;
  MEM_POOL     _thr_map_pool;
  BB           *_bb;
  THR_TYPE     _thr_type;
  BOOL         _thr_before_regalloc;
  BB_MAP       _thr_map;
  std::list<ARC*>   _candidate_list;
  BOOL         _chk_instr_inserted;

public:

  // Constructor/Destructors:

  CG_THR ();
  ~CG_THR();

  //  Set members

  // Member access functions:

  // Exported functions:
  void Init (BB*, THR_TYPE, BOOL);
  void Perform_THR();
  BOOL OP_Has_Restrictions(OP*, OP*, BOOL);
  void Check_THR_Profitability();
  void Perform_THR_Code_Generation();
};


// Other Exported utility routines

// Trace Utility routines

#endif /* cg_thr_INCLUDED */

