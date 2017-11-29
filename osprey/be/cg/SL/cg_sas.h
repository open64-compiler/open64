/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

/* An interface from Pro64 scheduler to sas, a stand-alone scheduler. */

#ifndef cg_sas_INCLUDED
#define cg_sas_INCLUDED "cg_sas.h"

#include "mempool.h"
#include "tn.h"
#include "cg_loop.h"
#include <map>
#include <stdint.h>


enum SWP_FAILURE_CODE {
  SKIP_FDIV_SQRT,         // swp failed: hw bug in fdiv and sqrt
  REGISTER_ALLOC_FAILED,  // swp failed: failure to allocate register
  MODULO_SCHED_FAILED,    // swp failed: failure to schedule
  ONE_STAGE_COUNT_ONLY    // swp failed: the kernel has only one stage count
};

enum GLUE_DIRECTION {  APPEND, PREPEND };

class KEY_SCH {
private:
  MEM_POOL* mem_pool;

  VECTOR schedule;
  int unrolls;      // how many times this loop has been unrolled.
  const bool perform_swp;
  bool trace;

  typedef int (*ARRAY_ELEMENT_CMP_FUNC)(const void*, const void*);

  TN_SET* tn_invariants;
  TN_SET* tn_live_ins;
  TN_SET* tn_live_outs;
  void rename_invariants();

  /* An embedded function for Pro64 to dump the source basic block. */
  bool is_eof;
  void GetLine( FILE*, char* ) ;
  void Emit_TN( char* buf, int size, TN* tn ) const ;
  void Emit_Src_DDG() const ;

  void Construct_addr_vector();
  void Adjust_ofst( const OP*, OP* ) const;
  void Adjust_incr( OP* );

  void Handle_Ldst_Addiu();
  void Collect_Sched_Info();
  void Schedule_DDG();
  void Reorder_Kernel( ARRAY_ELEMENT_CMP_FUNC );
  void Schedule_Kernel();

  // for swp
  int mii;
  int res_mii;
  int rec_mii;
  int sc;
  int nOps;
  int sched_len;
  int min_sched_len;
  int Kmin;  // the number copies of the kernel is needed.

  BB* glue_prolog;
  BB* glue_epilog;

  BB* prolog;
  BB* epilog;
  BB* kernel;

  /* tn_renames[tn,i] gives the update tn name for tn at
     i-th unrolling. */
  TN_MAP tn_renames;
  void tn_renaming( int );
  TN* rename_tn( TN*, const int, const int ) const;
  TN* New_live_in_tn( TN* );
  TN* New_live_out_tn( TN* );

  // Sets of available registers in each register class.
  REGISTER_SET avail_reg_set[ISA_REGISTER_CLASS_MAX+1];

  std::map<CLASS_REG_PAIR, TN*> reg2tn_map;
  std::map<TN*, CLASS_REG_PAIR> reg_allocation;

  void Delete_Backpatches();

  void Add_Glue( TN *result, TN *opnd, BB *bb, GLUE_DIRECTION );
  void Assign_Register( TN* );
  void register_allocation_init();
  void Gen_Kernel_Info();
  void Gen_Kernel_Fail_Info( SWP_FAILURE_CODE );

  void Compute_Kmin();
  void Loop_Unrolling( CG_LOOP&, int );

  void Peeling_For_Known_Trip( CG_LOOP&, TN* );
  void Loop_Peeling( const int, TN* ) const;
  void Peeling_For_Unknown_Trip( CG_LOOP&, TN* );
  void Loop_Preconditioning( CG_LOOP& );

  void Gen_PKE( CG_LOOP& );

public:
  bool success;

  KEY_SCH( BB*, MEM_POOL* );
  KEY_SCH( CG_LOOP&, BB*, BB*, bool );

  ~KEY_SCH() {};
}; 


extern void Emit_KEY_SWP_Note( BB*, FILE* );


#endif
