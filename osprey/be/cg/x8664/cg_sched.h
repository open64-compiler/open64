/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

/* A local instruction scheduler dedicated to opteron.
   May 29, 2003
*/


#ifndef cg_sched_INCLUDED
#define cg_sched_INCLUDED "cg_sched.h"

#include "mempool.h"
#include "tn.h"
#include "cg_loop.h"
#include <stdint.h>


class DSP_SCH {
private:
  MEM_POOL* mem_pool;
  BB* bb;

  VECTOR _ready_vector;
  bool trace;
  bool can_sched;

  OP* defop_by_reg[ISA_REGISTER_CLASS_MAX+1][REGISTER_MAX+1];
  int Addr_Generation( OP* );

  int _U;
  int _true_cp;
  int _cp;
  void Summary_BB();

  void Tighten_Release_Time( OP* );
  OP* last_mem_op;
  OP* Winner( OP*, OP*, int );
  OP* Select_Variable( int );

  void Init();
  void Compute_Insn_Size( OP* );
  void Build_OPR();
  void Build_Ready_Vector();
  void Schedule_BB();
  void Reorder_BB();

  // Sets of available registers in each register class.
  REGISTER_SET avail_reg_set[ISA_REGISTER_CLASS_MAX+1];

public:
  DSP_SCH( BB*, MEM_POOL*, bool );
  ~DSP_SCH() {};
  VECTOR _sched_vector;
}; 


#endif
