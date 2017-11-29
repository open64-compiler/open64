/*
 * Copyright (C) 2008 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2005-2007 NVIDIA Corporation.  All rights reserved.
 */

//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_util.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_util.cxx,v $
//
// Revision history:
//  12-SEP-94 shin - Original Version
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
// Description: Defines utilities for Optimizer
//
// ====================================================================
// ====================================================================


#define opt_util_CXX	"opt_util.cxx"
#ifdef _KEEP_RCS_ID
static char *rcs_id = 	opt_util_CXX"$Revision: 1.8 $";
#endif /* _KEEP_RCS_ID */

#include <stdarg.h>
#include <stdio.h>
#include <strings.h>
#include <time.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>
#if defined(TARG_SL)
#include "intrn_info.h"
#endif

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//  DO NOT INCLUDE "defs.h" before Warn_todo().
//   
//     defs.h is incompatible with stdarg.h!
//
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


const int MAX_WARN_LEN = 1024;
const int MAX_WARN_MESSAGES = 1024;
static char *warn_msg[MAX_WARN_MESSAGES];
static int n_msgs = 0;

// The use of stdarg marcos must be before the include of defs.h.
//
void Warn_todo(const char *msg, ... )
{
#ifdef WARN_TODO
  va_list vp;
  char msg_buf[MAX_WARN_LEN];
  int len;

  va_start ( vp, msg );
  len = vsprintf(msg_buf, msg, vp);     // if msg is too long, it might overrun the buf
  //  FmtAssert doesn't work here!
  if (len >= MAX_WARN_LEN)
     fprintf(stderr, "Warning message buffer too small.");
  va_end ( vp );

  for (int i = 0; i < n_msgs; i++) {
    if (strcmp(msg_buf, warn_msg[i]) == 0)  // the message has been appeared before
      return;
  }
  if (n_msgs < MAX_WARN_MESSAGES)
    warn_msg[n_msgs++] = strdup(msg_buf);      // save the message for comparision
  fprintf(stderr, "TODO %s \n", msg_buf);
#endif
}

// ====================================================================
// Provide a variable-argument interface to the TLOG performance
// tracing routine.
// ====================================================================

static void Opt_tlog2( char *, long long, char * );
static int  Opt_tlog_trace( void );

// TLOG interface for reporting optimizations
extern "C" void 
Opt_tlog( char *keyword, long long srcpos, const char *fmt, ...)
{
  va_list vp;
  char msg_buf[MAX_WARN_LEN];
  int len;

  // can accept either -tt1:1 or -tt1:
  if ( ! Opt_tlog_trace() ) {
    return;
  }

  va_start ( vp, fmt );
  vsprintf(msg_buf, fmt, vp);     // if msg is too long, it might overrun the buf
  len = strlen(msg_buf);
  //  FmtAssert doesn't work here!
  if (len >= MAX_WARN_LEN) {
    fprintf(stderr, "Opt_tlog message buffer too small.");
  }

  va_end ( vp );

  Opt_tlog2( keyword, srcpos, msg_buf );
}


#include "defs.h"
#include "config_wopt.h"
#include "opt_defs.h"
#include "opt_sys.h"
#include "errors.h"
#include "tracing.h"
#include "srcpos.h"
#include "tlog.h"
#include "optimizer.h"
#include "opt_bb.h"
#include "erglob.h"
#include "opt_htable.h"
#include "opt_cfg.h"


// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//   All other functions should be written after this line !
//
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

// ====================================================================
// these two functions are used to get around the stupid type-checking 
// and varargs cruft.
// ====================================================================

static BOOL
Opt_tlog_trace( void )
{
  return Get_Trace ( TP_PTRACE1, TP_PTRACE1_OPT );
}

static const char *tlog_phase = NULL;

static void Opt_tlog2( char *keyword, INT64 srcpos, char *msg )
{
  // use the keyword as both the transformation name and keyword
  Generate_Tlog( tlog_phase, keyword, (SRCPOS)srcpos, keyword, msg, "","" );
}

extern "C" void
Set_tlog_phase(const INT32 phase) 
{
  switch (phase) {
  case PREOPT_PHASE:
  case PREOPT_LNO_PHASE:
  case PREOPT_LNO1_PHASE:
  case PREOPT_DUONLY_PHASE:
  case PREOPT_IPA0_PHASE:
  case PREOPT_IPA1_PHASE:
    tlog_phase = "PRE_OPT";
    break;
  case MAINOPT_PHASE:
    tlog_phase = "WOPT";
    break;
  }
}



// ====================================================================
const INT32    PHASE_STRLEN = 72;
const INT32    MAX_SUBPHASES = 200;
static char    phase_name[PHASE_STRLEN];
static const   char   *phases[MAX_SUBPHASES];
static INT32   times[MAX_SUBPHASES];
static INT32   reps[MAX_SUBPHASES];
static INT32   cum_times[MAX_SUBPHASES];
static float   peak_times[MAX_SUBPHASES];
static INT32   total_time;
static INT32   curr_phase = 0;
static INT32   max_phase  = 0;
static INT32   prev_time  = 0;
static UINT64  mem[MAX_SUBPHASES];
static void   *prev_mem   = NULL;

//  Set_opt_phase is a no-op if Is_True_On is not defined.
//    It collected the time spent in each optimizer phase.
//    It changes the global phase name for better error reporting.
//
INT Set_opt_phase(INT32 *phase_id, const char *subphase)
{
  INT32  curr_time;
  void  *curr_mem;

  if (Get_Trace(TKIND_INFO, TINFO_TIME)) {
    curr_time = CLOCK_IN_MS();
#ifdef __MINGW32__
    DevWarn("sbrk not supported on Win NT");
#else
    curr_mem  = sbrk(0);
#endif /* __MINGW32__ */
    times[curr_phase] += (curr_time - prev_time);
    mem[curr_phase] += (char *) curr_mem - (char *) prev_mem;
    if (phase_id == NULL || *phase_id == 0) {
      curr_phase = ++max_phase;
      if (phase_id != NULL) {
	*phase_id = curr_phase;
      }
      times[curr_phase] = 0;
      reps[curr_phase] = 1;
    }
    else {
      curr_phase = *phase_id;
      reps[curr_phase] += 1;
    }
    if (curr_phase >= MAX_SUBPHASES) return 0;   // quietly ignore the extra phases.
    phases[curr_phase] = subphase;
    prev_time = curr_time;
    prev_mem  = curr_mem;
  }
  strncpy(phase_name, "Global Optimization -- ", PHASE_STRLEN);
  strncat(phase_name, subphase, PHASE_STRLEN);
  Set_Error_Phase(phase_name);
  return 1;
}

//  This routine is called at the end of the optimizer to print out the
//  timing information to the TFile.
//
INT Report_statistics()
{
  INT32 i;
  INT32 local_total = 0;
  if (Get_Trace(TKIND_INFO, TINFO_TIME)) {
    fprintf(TFile, "%sCompilation Time\n%s", DBar, DBar);
    for ( i = 1; i < curr_phase && i < MAX_SUBPHASES; i++) {
      local_total += times[i];
      total_time += times[i];
    }
    for ( i = 1; i < curr_phase && i < MAX_SUBPHASES; i++) {
      float f, p;
      cum_times[i] += times[i];
      f = ((float)cum_times[i]/(float)total_time)*100;
      p = ((float)times[i]/(float)local_total)*100;
      if ((local_total > 60) && (p > peak_times[i]))
	peak_times[i] = p;
      fprintf(TFile,
	      "%8d %6.2f%% (cum %8d %5.2f, peak %5.2f%%) ms %5lldk mem in %s",
	      times[i], p, cum_times[i], f, peak_times[i], mem[i] >> 10,
	      phases[i]);
      if (reps[i] > 1) {
	fprintf(TFile, " (%d reps)", reps[i]);
      }
      fprintf(TFile, "\n");
    }
    fprintf(TFile,
	    "%8d %6.2f%% (cum %8d %5.2f, peak %5.2f%%) ms  in %s\n",
	    local_total, 1.0, total_time, 0.0, 0.0, "PU Total");
    curr_phase = 0;
    max_phase = 0;
  }
  return 1;
}



static inline INT32 Sign(INT64 v)
{
  if (v > 0) 
    return 1;
  else if (v < 0)
    return -1;
  return 0;
}
  
void
NUMBER::Eval1(OPERATOR opr, NUMBER *n)
{
  if (n->Desc() == NUMBER_KNOWN) {
    Set_desc(n->Desc());
    if (opr == OPR_NEG) {
      INT64 v = n->Value();
      INT32 sign = Sign(v);
      if (sign != 0 && -sign != Sign(-v)) 
	Set_desc(NUMBER_OVERFLOW);
      else
	Set_value(- v);
    } else
      Set_desc(NUMBER_UNKNOWN);
  } else
    Set_desc(n->Desc());
}

void
NUMBER::Eval2(OPERATOR opr, NUMBER *n1, NUMBER *n2)
{
  INT64 value;
  INT32 sign;
  Set_desc( MIN(n1->Desc(), n2->Desc()));
  if (Desc() == NUMBER_KNOWN) {
    INT64 v1 = n1->Value();
    INT64 v2 = n2->Value();
    if (opr == OPR_MPY) {
      value = v1 * v2;
      if (Sign(v1) * Sign(v2) != Sign(value) ||
	  value / v2 != v1)
	Set_desc(NUMBER_OVERFLOW);
#ifdef TARG_X8664 // bug 8229: such situations are dangerous
      else if (value > UINT32_MAX && v1 <= UINT32_MAX && v2 <= UINT32_MAX)
	Set_desc(NUMBER_OVERFLOW);
#endif
      else
	Set_value(value);
    } else if (opr == OPR_ADD) {
      value = v1 + v2;
      sign = Sign(v1) + Sign(v2);
      if (sign != 0 && Sign(sign) != Sign(value))
	Set_desc(NUMBER_OVERFLOW);
      else
	Set_value(value);
    } else if (opr == OPR_SUB) {
      value = v1 - v2;
      sign = Sign(v1) - Sign(v2);
      if (sign != 0 && Sign(sign) != Sign(value))
	Set_desc(NUMBER_OVERFLOW);
      else
	Set_value(value);
    }
    else
      Set_desc(NUMBER_UNKNOWN);
  }
}


//  The constant is representable using n-bits.
BOOL
NUMBER::Representable_in_nbits(INT32 nbits)
{
  INT64 v = Value();
  INT64 min = (INT64) ~((((UINT64) 1) << (nbits - 1)) - 1);
  INT64 max = -(min + 1);
  return (v <=  max && v >= min);
}

// Identify the variant part from a tree of ADD/SUB/NEG/MUL operators.
// Return NO_VARIANT if there is no variant.
// Return NOT_ONE_VARIANT if there is a variant that is not vr.
// Return NOT_ONE_VARIANT if there is more than one variant.
// Return NOT_ONE_VARIANT if there is some operator that is not ADD, SUB, NEG, or MUL.
// Return ONE_VARIANT if there is exactly one variant that is vr.
//
// Determine whether the relation of the sign of the IV and the sign of the expression
//
NUM_VARIANTS
Find_one_variant(BB_NODE *bb, CODEREP *vr, CODEREP *cr, NUMBER *factor,
                 CODEMAP *htable)
{
  OPCODE opc;
  OPERATOR opr;
  switch (cr->Kind()) {
  case CK_LDA:
    factor->Set_desc(NUMBER_UNKNOWN);
    return NO_VARIANT;

  case CK_CONST:
    factor->Set_const(cr->Const_val());
    return NO_VARIANT;
    
  case CK_VAR:
    if (bb->Innermost()->Invariant_cr(cr)) {
      factor->Set_desc(NUMBER_UNKNOWN);
      return NO_VARIANT;
    } else {
      BOOL is_same_var = (htable)?
        (cr->Aux_id() == vr->Aux_id()) : (cr->Bitpos() == vr->Bitpos());
      if (is_same_var) {
        factor->Set_desc(NUMBER_KNOWN);
        factor->Set_value(1);
        return ONE_VARIANT;
      } else
        return NOT_ONE_VARIANT;
    }
  case CK_IVAR:
    if (WOPT_Enable_LFTR_Ivar) {
      if (bb->Innermost()->Invariant_cr(cr)) {
	factor->Set_desc(NUMBER_UNKNOWN);
	return NO_VARIANT;
      }
    }
    return NOT_ONE_VARIANT;
    
  case CK_OP:
  {
    NUM_VARIANTS r0, r1;
    NUMBER f0, f1;
    opc = cr->Op();
    opr = OPCODE_operator(opc);

    switch (opr) {
    case OPR_PAREN:
      return Find_one_variant(bb, vr, cr->Opnd(0), factor, htable);
      
    case OPR_NEG:
      r0 = Find_one_variant(bb, vr, cr->Opnd(0), factor, htable);
      factor->Eval1(OPR_NEG, factor);
      return r0;

    case OPR_CVT:
      /* CVTL-RELATED start (performance)  */
#ifdef TARG_MIPS
      if (opc == OPC_U8I4CVT) {
	r0 = Find_one_variant(bb, vr, cr->Opnd(0), factor, htable);
	return r0;
      } else
#elif defined(TARG_X8664) || defined(TARG_NVISA)
      if (opc == OPC_U8I4CVT || opc == OPC_I8I4CVT || opc == OPC_U8U4CVT) {
	r0 = Find_one_variant(bb, vr, cr->Opnd(0), factor, htable);
	return r0;
      } else
#endif
      /* CVTL-RELATED finish */
	return NOT_ONE_VARIANT;
      
    case OPR_ADD:
    case OPR_SUB:
    case OPR_MPY:
      // Process the right subtree first because the right subtree is usually shorter.
      f1.Init();
      r1 = Find_one_variant(bb, vr, cr->Opnd(1), &f1, htable);
      // if the right subtree contains some other variant, then stop here.
      if (r1 == NOT_ONE_VARIANT)
	return NOT_ONE_VARIANT;
      
      f0.Init();
      r0 = Find_one_variant(bb, vr, cr->Opnd(0), &f0, htable);
      // if the left subtree contains some other variant, then stop here.
      if (r0 == NOT_ONE_VARIANT)
	return NOT_ONE_VARIANT;
      
      // if both subtree are invariant, then return invariant.
      if (r0 == NO_VARIANT && r1 == NO_VARIANT) {
	factor->Eval2(opr, &f0, &f1);
	return NO_VARIANT;
      }
      
      if (r0 == ONE_VARIANT && r1 == ONE_VARIANT) {
	if (opr == OPR_ADD || opr == OPR_SUB) {
	  //  handle cand like  i * 3 + i * n
	  factor->Eval2(opr, &f0, &f1);
	  return ONE_VARIANT;
	} else
	  // do not handle i * i
	  return NOT_ONE_VARIANT;
      }
      
      // if we reach this point, then one of the subtree is ONE_VARIANT, and
      // the other is NO_VARIANT.   We should return ONE_VARIANT.
      if (opr == OPR_MPY) {
	factor->Eval2(opr, &f0, &f1);
      } else {
	if (r0 == ONE_VARIANT) {
	  factor->Copy(&f0);
	} else {
	  if (opr == OPR_SUB)
	    factor->Eval1(OPR_NEG, &f1);
	  else if (opr == OPR_ADD)
	    factor->Copy(&f1);
	  else
	    factor->Set_desc(NUMBER_UNKNOWN);
	}
      }
      return ONE_VARIANT;
      
    default:
      return NOT_ONE_VARIANT;  // no ops other than neg/add/sub/mul are supported.
    }
  }

  default:
    return NOT_ONE_VARIANT; // this CK_kind not supported
  }
}

// ====================================================================
// set_volatile_map: walks the whole CFG and sets a mapping from coderep_id
// to a boolean, where the mapping (vol) is TRUE whenever the coderep
// contains a reference to volatile memory or a volatile OPC.
// ====================================================================

static BOOL set_volatile_mapCR(CODEREP *cr, 
			       STMTREP *stmt,
			       BVECTOR &visited,
			       BVECTOR &vol)
{
   BOOL has_vol_ref = vol[cr->Coderep_id()];
   
   if (!visited[cr->Coderep_id()])
   {
      visited[cr->Coderep_id()] = bool(TRUE);
      
      switch (cr->Kind())
      {
      case CK_CONST:
      case CK_RCONST:
      case CK_LDA:
	 break;

      case CK_VAR:
	 has_vol_ref = cr->Is_var_volatile();
	 break;

      case CK_IVAR:
	 {
	    CODEREP *const vsym = cr->Get_ivar_vsym();
	    if (vsym != NULL && set_volatile_mapCR(vsym, stmt, visited, vol))
	       has_vol_ref = TRUE;
	    
	    // Ilod_base can be different from Istr_base if the Iload side is
	    // dead, i.e. when Usecnt() == 0
	    // 
	    if (cr == stmt->Lhs() && OPCODE_is_store(stmt->Op()))
	    {
	       if (cr->Opr() == OPR_MLOAD && 
		   set_volatile_mapCR(cr->Mstore_size(), stmt, visited, vol))
		  has_vol_ref = TRUE;
	       if (set_volatile_mapCR(cr->Istr_base(), stmt, visited, vol))
		  has_vol_ref = TRUE;
	    }
	    else
	    {
	       if (cr->Opr() == OPR_MLOAD && 
		   set_volatile_mapCR(cr->Mload_size(), stmt, visited, vol))
		  has_vol_ref = TRUE;
	       if (set_volatile_mapCR(cr->Ilod_base(), stmt, visited, vol))
		  has_vol_ref = TRUE;
	    }
	    if (!has_vol_ref && cr->Is_ivar_volatile())
	       has_vol_ref = TRUE;
	 }
         break;

      case CK_OP:
	 {
	    for (INT32 i=0; i<cr->Kid_count(); i++)
	       if (set_volatile_mapCR(cr->Opnd(i), stmt, visited, vol))
		  has_vol_ref = TRUE;

	    if (!has_vol_ref && OPERATOR_is_volatile(cr->Opr()))
	       has_vol_ref = TRUE;
	 }
         break;

      case CK_DELETED:	// should never happen
      default:		// illegal kind
	 FmtAssert(FALSE, 
		   ("set_volatile_mapCR, unexpected kind 0x%x", cr->Kind()));
	 break;
      }
      vol[cr->Coderep_id()] = has_vol_ref;
   }
   return has_vol_ref;
} // set_volatile_mapCR


class SET_VOLMAP_FOR_CR
{
   BVECTOR *_visited;
   BVECTOR *_vol;
public:

   SET_VOLMAP_FOR_CR(BVECTOR &visited, BVECTOR &vol):
      _visited(&visited), _vol(&vol) {}

   void operator() (CODEREP *cr, STMTREP *stmt, INT32)
   {
      set_volatile_mapCR(cr, stmt, *_visited, *_vol);
   }
}; // class SET_VOLMAP_FOR_CR


void 
Set_volatile_map(CFG *cfg, BVECTOR &vol)
{
   // For each coderep (i), determines whether it contains a reference
   // to a volatile memory location or a volatile OPC, and if this is 
   // the case sets vol[i]=TRUE.  
   //
   // PRECONDITION: For all i
   //                 where: 0 <= i <= cfg->Htable()->Coderep_id_cnt()+1
   //                 vol[i] == FALSE
   //
   Is_True(vol.size() == cfg->Htable()->Coderep_id_cnt()+1,
	   ("Inadequate size of vector (vol) for Set_volatile_map()"));

   OPT_POOL_Push(cfg->Loc_pool(), -1);
   {
      BVECTOR visited(cfg->Htable()->Coderep_id_cnt()+1,
		      bool(FALSE), 
		      BVECTOR_ALLOCATOR(cfg->Loc_pool()));
      
      CFG_ITER bb_iter(cfg);
      BB_NODE *bb;
      FOR_ALL_ELEM (bb, bb_iter, Init())
      {
	 SET_VOLMAP_FOR_CR set_volmapCR(visited, vol);
	 STMTREP_ITER      stmt_iter(bb->Stmtlist());
	 STMTREP          *stmt;
	 FOR_ALL_NODE(stmt, stmt_iter, Init())
	    traverseSR(stmt, set_volmapCR);
      }
   }
   OPT_POOL_Pop(cfg->Loc_pool(), -1);
} // set_volatile_map

#if defined(TARG_SL)
BOOL CR_Intrinsic_Op_Slave( CODEREP *cr) {
  if (cr->Kind() == CK_OP && cr->Opr() == OPR_INTRINSIC_OP) {
    INTRINSIC ins = cr->Intrinsic();
    if (INTRN_is_slave(ins))
      return TRUE;
  }
  return FALSE;
}
#endif

