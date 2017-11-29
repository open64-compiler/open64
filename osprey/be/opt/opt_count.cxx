//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_count.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_count.cxx,v $
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
// Collecting statistics
// 
// Scan CFG and count the number of loads and stores
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#include "defs.h"
#include "cxx_memory.h"
#include "opt_cfg.h"
#include "opt_sym.h"
#include "opt_htable.h"
#include "opt_config.h"
#include "tracing.h"
#include "config_targ.h"
#include "opt_util.h"
#include "opt_sys.h"
#include "opt_bb.h"
#include "opt_main.h"

// ====================================================================
//
// OPTCOUNT class - This unexported class holds counters for statistics
//
// ====================================================================

class OPTCOUNT {
private:
  MEM_POOL _mpool;
  CFG     *_cfg;	       	// handle on the control-flow graph
  OPT_STAB*_opt_stab;
  BOOL     _tracing;		// is general tracing enabled?

  UINT16  *_loads;              // direct loads for each BB
  UINT16  *_stores;             // direct stores for each BB
  UINT16  *_iloads;             // iloads for each BB
  UINT16  *_istores;            // istores for each BB
  UINT32  *_freq;               // execution count for BB

  UINT32  _weighted_total_loads;
  UINT32  _weighted_total_stores;
  UINT32  _weighted_total_iloads;
  UINT32  _weighted_total_istores;

  UINT32  _total_loads;
  UINT32  _total_stores;
  UINT32  _total_iloads;
  UINT32  _total_istores;
  
  CFG     *Cfg(void)       const      { return _cfg; }
  OPT_STAB *Opt_stab(void) const      { return _opt_stab; }

  UINT16   Loads(IDTYPE bb_id)  const { return _loads[bb_id]; }
  UINT16   Stores(IDTYPE bb_id) const { return _stores[bb_id]; }
  UINT16   Iloads(IDTYPE bb_id) const { return _iloads[bb_id]; }
  UINT16   Istores(IDTYPE bb_id)const { return _istores[bb_id]; }
  UINT32   Freq(IDTYPE bb_id)   const { return _freq[bb_id]; }

  void     Inc_loads(IDTYPE bb_id)    { _loads[bb_id]++; }
  void     Inc_stores(IDTYPE bb_id)   { _stores[bb_id]++; }
  void     Inc_iloads(IDTYPE bb_id)   { _iloads[bb_id]++; }
  void     Inc_istores(IDTYPE bb_id)  { _istores[bb_id]++; }
  void     Set_freq(IDTYPE bb_id, UINT32 c)     
                                      { _freq[bb_id] = c; }

  UINT32   Total_loads(void)          { return _total_loads; }
  UINT32   Total_stores(void)         { return _total_stores; }
  UINT32   Total_iloads(void)         { return _total_iloads; }
  UINT32   Total_istores(void)        { return _total_istores; }

  void     Acc_total_loads(UINT32 x)  { _total_loads += x; }
  void     Acc_total_stores(UINT32 x) { _total_stores += x; }
  void     Acc_total_iloads(UINT32 x) { _total_iloads += x; }
  void     Acc_total_istores(UINT32 x){ _total_istores += x; }

  UINT32   Weighted_total_loads(void) { return _weighted_total_loads; }
  UINT32   Weighted_total_stores(void){ return _weighted_total_stores; }
  UINT32   Weighted_total_iloads(void){ return _weighted_total_iloads; }
  UINT32   Weighted_total_istores(void){ return _weighted_total_istores; }

  void     Acc_weighted_total_loads(UINT32 x)  { _weighted_total_loads += x; }
  void     Acc_weighted_total_stores(UINT32 x) { _weighted_total_stores += x; }
  void     Acc_weighted_total_iloads(UINT32 x) { _weighted_total_iloads += x; }
  void     Acc_weighted_total_istores(UINT32 x){ _weighted_total_istores += x;}
  
public:
  OPTCOUNT(CFG *cfg, OPT_STAB *opt_stab): _cfg(cfg), _opt_stab(opt_stab)
    { 
      OPT_POOL_Initialize(&_mpool, "opt_count_mempool", FALSE, -1);
      OPT_POOL_Push(&_mpool, -1);
      _tracing = Get_Trace( TP_GLOBOPT, STATISTICS_FLAG);
      _loads = (UINT16 *) CXX_NEW_ARRAY(UINT16, Cfg()->Total_bb_count(), &_mpool);
      _stores = (UINT16 *) CXX_NEW_ARRAY(UINT16, Cfg()->Total_bb_count(), &_mpool);
      _iloads = (UINT16 *) CXX_NEW_ARRAY(UINT16, Cfg()->Total_bb_count(), &_mpool);
      _istores = (UINT16 *) CXX_NEW_ARRAY(UINT16, Cfg()->Total_bb_count(), &_mpool);
      _freq = (UINT32 *) CXX_NEW_ARRAY(UINT32, Cfg()->Total_bb_count(), &_mpool);
      BZERO(_loads, Cfg()->Total_bb_count()*sizeof(_loads[0]));
      BZERO(_stores, Cfg()->Total_bb_count()*sizeof(_stores[0]));
      BZERO(_iloads, Cfg()->Total_bb_count()*sizeof(_iloads[0]));
      BZERO(_istores, Cfg()->Total_bb_count()*sizeof(_istores[0]));
      BZERO(_freq, Cfg()->Total_bb_count()*sizeof(_freq[0]));

      _weighted_total_loads = _weighted_total_stores = 
	_weighted_total_iloads = _weighted_total_istores = 0;

      _total_loads = _total_stores = _total_iloads = _total_istores = 0;
  
    }
  
  ~OPTCOUNT(void) 
    {
      OPT_POOL_Pop(&_mpool, -1);
      OPT_POOL_Delete(&_mpool, -1);
    }
  void Bottom_up_cr(IDTYPE bb, CODEREP *cr, BOOL is_store = FALSE);
  void Bottom_up_stmt(STMTREP *stmt, IDTYPE bb);

  // Collect statistics
  void Collect_statistics( void );  

  BOOL Tracing(void) const	              { return _tracing; }
}; // end of class OPTCOUNT

void
OPTCOUNT::Bottom_up_cr(IDTYPE bb, CODEREP *cr, BOOL is_store)
{
  switch (cr->Kind()) {
  case CK_CONST:  
  case CK_RCONST: 
  case CK_LDA:    
    break;

  case CK_VAR:	    // variable terminal

    if (ST_class( Opt_stab()->St(cr->Aux_id()) ) != CLASS_PREG)
      Inc_loads(bb);
    break;

  case CK_IVAR:	// non-terminal

    // Skip the DUMMY OPR_PARM
    if (cr->Opr() == OPR_PARM) {
      if (cr->Offset() & WN_PARM_DUMMY) 
	break;
    }
    else {
      if (is_store) 
	Inc_istores(bb);
      else
	Inc_iloads(bb);
    }

    Bottom_up_cr(bb, is_store ? cr->Istr_base() : cr->Ilod_base());

    if ( cr->Opr() == OPR_MLOAD ) {
      Bottom_up_cr(bb, cr->Mload_size() ? cr->Mload_size() : cr->Mstore_size());
    }
    else if ( cr->Opr() == OPR_ILOADX ) {
      Bottom_up_cr(bb, cr->Index());
    }
    break;

  case CK_OP:		// non-terminal
    {
      for (INT32 i=0; i<cr->Kid_count(); i++)	{ 
	Bottom_up_cr(bb, cr->Opnd(i));
      }
      break;
    }
  default:		// illegal kind
    break;
  }
}

void
OPTCOUNT::Bottom_up_stmt(STMTREP *stmt, IDTYPE bb)
{
  const OPERATOR stmt_opr = stmt->Opr();

  CODEREP *rhs = stmt->Rhs();

  if (OPCODE_is_call(stmt->Op())) {
    for (INT32 i = 0; i < rhs->Kid_count(); i++) {
      Bottom_up_cr(bb, rhs->Opnd(i));
    }
  } else if (rhs != NULL) {
    Bottom_up_cr(bb, rhs);
  }

  if (OPERATOR_is_scalar_istore (stmt_opr)) {
    Bottom_up_cr(bb, stmt->Lhs(), TRUE);
  } else if (OPERATOR_is_scalar_store (stmt_opr)) {

    switch(stmt->Lhs()->Kind()) {
    case CK_VAR:
      if (ST_class(Opt_stab()->St(stmt->Lhs()->Aux_id())) != CLASS_PREG)
	Inc_stores(bb);
      break;
    default:
      Inc_stores(bb);
      Bottom_up_cr(bb,stmt->Lhs());
    }  
  }

}

void
OPTCOUNT::Collect_statistics( void )
{
  BB_NODE *bb;
  CFG_ITER cfg_iter(Cfg());

  FOR_ALL_ELEM (bb, cfg_iter, Init()) {
    Is_Trace(Tracing(),(TFile,
	      "====== OPTCOUNT::Collect_static_statistics, BB%d ======\n",
	      bb->Id()));

    Set_freq(bb->Id(), bb->Freq());

    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      Is_Trace_cmd(Tracing(),stmt->Print(TFile));
      Bottom_up_stmt(stmt, bb->Id());
    }

    Is_Trace(Tracing(),(TFile,
			"Loads %d, Stores %d, Iloads %d, Istores %d, Freq %d \n", 
			Loads(bb->Id()), Stores(bb->Id()), Iloads(bb->Id()), 
			Istores(bb->Id()), Freq(bb->Id())));

  }

  for (UINT i = 0; i < Cfg()->Total_bb_count(); ++i) {
    Acc_total_loads(Loads(i));
    Acc_total_stores(Stores(i));
    Acc_total_iloads(Iloads(i));
    Acc_total_istores(Istores(i));

    Acc_weighted_total_loads(Loads(i)*Freq(i));
    Acc_weighted_total_stores(Stores(i)*Freq(i));
    Acc_weighted_total_iloads(Iloads(i)*Freq(i));
    Acc_weighted_total_istores(Istores(i)*Freq(i));
  }

  fprintf(TFile,"Total static: loads %12d, stores %12d, iloads %12d, istores %12d\n", Total_loads(), Total_stores(), Total_iloads(), Total_istores());
  fprintf(TFile,"Total dynamic : loads %12d, stores %12d, iloads %12d, istores %12d\n", Weighted_total_loads(), Weighted_total_stores(), Weighted_total_iloads(), Weighted_total_istores());
}

void
COMP_UNIT::Collect_statistics(void)
{
  OPTCOUNT optcount( Cfg(), Opt_stab() );

  optcount.Collect_statistics();

}







