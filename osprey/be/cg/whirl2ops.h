/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#ifndef	whirl2ops_INCLUDED
#define	whirl2ops_INCLUDED


/* The <local_flag1> flag is overloaded temporarily in Convert_WHIRL_To_OPs
 * as <has_global> to keep track of the fact that we have seen a 
 * dedicated TN which we will try to match up with an already 
 * allocated TN in a previously compiled REGION.  The bit will be
 * cleared by the time we exit from Convert_WHIRL_To_OPs.
 */
#define BB_has_globals		BB_local_flag1
#define Set_BB_has_globals	Set_BB_local_flag1
#define Reset_BB_has_globals	Reset_BB_local_flag1

/* TODO: Eventually put def_BB in a separate array outside STs */
#define STL_def_BB(st)        (struct bb *)STL_ltemp(st)
#define Set_STL_def_BB(st,v)  Set_STL_ltemp(st,v)

/* TRUE iff Convert_WHIRL_To_OPs is invoked on a REGION WN */
extern BOOL Compiling_Proper_REGION;

/* Walk through the WHIRL nodes of a 'pu' and convert them into OPs. */
extern void Convert_WHIRL_To_OPs (WN *pu);

/* Create map created by Convert_WHIRL_To_OPs */
extern void Whirl2ops_Initialize(struct ALIAS_MANAGER *alias_mgr);
/* Delete maps created by Convert_WHIRL_To_OPs */
extern void Whirl2ops_Finalize(void);

/* array to map PREGs into TNs. */
extern TN **PREG_To_TN_Array;
extern TYPE_ID *PREG_To_TN_Mtype;

/* If TN is a mtype_b preg, return its complement TN. */
extern TN *Get_Complement_TN(TN *tn);

/* Return the TN corresponding to the PREG preg_num. If a TN has not yet
 * been allocated for the PREG, this routine does that.
 */
extern TN * PREG_To_TN (ST *preg_st, PREG_NUM preg_num);
extern TN * PREG_To_TN (TY_IDX preg_ty, PREG_NUM preg_num);

extern void PREG_To_TN_Clear (void);

/* Return the PREG corresponding to the 'tn' if one exists. Otherwise 
 * the routine returns 0. 
 * NOTE: This routine is inefficiently implemented for non-dedicated TNs, use 
 *       with caution.
 */
extern PREG_NUM TN_To_PREG (TN *tn);

/* Return the physical PREG assigned to the TN. This routine can be called
 * only if the TN has an assigned register.
 */
extern PREG_NUM TN_To_Assigned_PREG (TN *tn);

extern WN * Preg_Is_Rematerializable(PREG_NUM preg, BOOL *gra_homeable);

/* Return the WN corresponding to a given memory OP. 
 * If OP is not a memory OP returns NULL. 
 */
#include "op_map.h"
extern OP_MAP OP_to_WN_map;
inline WN *Get_WN_From_Memory_OP( const OP *op ) 
{
  if ( OP_to_WN_map == NULL || !OP_memory(op) && !OP_call(op) ) return NULL;
  return (WN*) OP_MAP_Get(OP_to_WN_map, op);
}

extern OP_MAP OP_Asm_Map;
#if defined(TARG_IA64)
extern OP_MAP OP_Ld_GOT_2_Sym_Map;
#endif

/* Information about the predicate under which conditional memory OPs
 * are executed.  Note that this is not currently maintained to adjust
 * for renaming, etc. - we just need to tell whether two mem OPs are
 * executed under the same condition to determine memory dependence.
 */
extern void Set_Memory_OP_Predicate_Info(OP *memop, TN *pred_tn,
					 UINT8 omega, BOOL inverted);
extern void Get_Memory_OP_Predicate_Info(OP *memop, TN **pred_tn,
					 UINT8 *omega, BOOL *inverted);

/* Copy the Get_WN_From_Memory_OP entry for <src> to <dest> so that
 * Get_WN_From_Memory_OP(dest) == Get_WN_From_Memory_OP(src).
 * Also copies predicate information (see above).
 */
extern void Copy_WN_For_Memory_OP(OP *dest, OP *src);

/* Return the OP corresponding to a given prefetch WN. 
 * If WN is not a prefetch WN returns NULL. 
 */
extern OP *Get_OP_From_WN(WN *wn);

/* Test the kind of the prefetch  { {L1,L2}, {load,store} } against
 * the four CG_enable_pf flags.
 */
extern BOOL Prefetch_Kind_Enabled(WN *wn);

/* Remove TN correspondence needed for DIVREM, MINMAX
 */
extern void TN_CORRESPOND_Free(void);

extern BB * Add_Label(LABEL_IDX);

#if defined(KEY) || defined(TARG_PPC32)
extern OPS New_OPs;
extern OP *Last_Processed_OP;
extern SRCPOS current_srcpos;
extern INT total_bb_insts;
extern BB *Cur_BB;
extern void Process_New_OPs(void);
extern BB_MAP outer_label_map;
#endif
#if defined(TARG_X8664) || defined(TARG_SL)
extern BOOL W2OPS_Pragma_Preamble_End_Seen (void);
#endif
#ifdef TARG_LOONGSON
/* wrapper for calling the static functions */
extern void Begin_New_Basic_Block(void);
extern void Start_New_Label(LABEL_IDX lb_idx, LABEL *lb, const char *str);
#endif

/* For new call inserted during whirl to ops, 
 * we need to finish current BB and start new bb
 * The original ops will be append to original BB,
 * then it will be re-initialized to contain new OPs for new BB.
 * for example: __tls_get_addr 
 */
void Expand_New_Call_To_OPs(WN*, OPERATOR, OPS*);
BB*  Start_New_Basic_Block();
#endif /* whirl2ops_INCLUDED */
