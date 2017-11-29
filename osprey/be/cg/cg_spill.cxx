/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


/* =======================================================================
 * =======================================================================
 *
 *  Module: cg_spill.c
 *  $Revision: 1.20 $
 *  $Date: 05/12/05 08:59:04-08:00 $
 *  $Author: bos@eng-24.pathscale.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_spill.cxx $
 *
 *  Description:
 *  ============
 *
 *  Routines for handling spills.
 *
 * =======================================================================
 * =======================================================================
 */

#include "defs.h"
#include "tracing.h"
#include "errors.h"
#include "erglob.h"
#include "ercg.h"
#include "glob.h"
#include "config.h"
#include "xstats.h"
#include "bitset.h"
#include "wn.h"
#include "ir_reader.h"
#include "cgir.h"
#include "const.h"	/* needed to manipulate target/host consts */
#include "targ_const.h"	/* needed to manipulate target/host consts */
#include "targ_isa_lits.h"
#include "cg_flags.h"
#include "register.h"
#include "stblock.h"
#include "st_list.h"
#include "cgexp.h"
#include "cg_spill.h"
#include "cgprep.h"
#include "cg_loop.h"
#include "data_layout.h"
#include "cg.h"
#include "gra.h"
#include "opt_alias_interface.h"
#include "cgtarget.h"
#include "targ_proc_properties.h"
#include "tag.h"

#ifdef TARG_IA64
#define TRACE_REMAT 0x2
#endif
#ifdef KEY
static SPILL_SYM_INFO_MAP spill_sym_info_map;
static void CGSPILL_Record_Spill (ST *spill_loc, OP *spill_op);
void CGSPILL_Inc_Restore_Count (ST *spill_loc);
#endif

static BOOL Trace_Remat; /* Trace rematerialization */
static BOOL Trace_GRA_spill_placement;
static const char *Remat_Phase;
static char *Remat_PU;

static MEM_POOL spill_id_pool;	/* Pool for allocation of spill ID bit vectors
				 */
static BOOL spill_id_pool_inited; /* True after we've inited the spill ID
				 * pool.
				 */
static BS **spill_ids;		/* Array of spill ID bit vectors.
				 * Indexed by symbol table level.
				 */
static INT max_spill_level;	/* Max symbol table level of any spill
				 * encountered.
				 */

/* Spill location symbol name roots for the various clients:
 */
#define SYM_ROOT_GRA	"gra_spill"	/* Global Register Allocator */
#define SYM_ROOT_LGRA	"lgra_spill"	/* Global Register Spilled by LRA */
#define SYM_ROOT_LCL	"lcl_spill"	/* TN Localization phase */
#define SYM_ROOT_LRA	"lra_spill"	/* Local Register Allocator */
#define SYM_ROOT_SWP	"swp_spill"	/* Software Pipeliner */


/*  Local type LOCAL_SPILLS
 *
 *      Used to keep track of local spill locations, both free for use 
 *      and those that are already allocated.  Access only through the 
 *      functions provided.
 *
 *      It has the following fields:
 *
 *	    TY_IDX   mem_type
 *
 *		The type used to generate the spill locations.
 *
 *          ST_LIST *free
 *
 *              List of STs describing free spill locations (not yet used
 *              in this local block/loop).
 *
 *          ST_LIST *used
 *
 *              List of STs that are being used in the current block/loop
 *		as spill locations.
 *
 */
typedef struct local_spills {
  TY_IDX	 mem_type;
  ST_LIST	*free;
  ST_LIST	*used;
} LOCAL_SPILLS;

#define LOCAL_SPILLS_mem_type(x)  ((x)->mem_type)
#define LOCAL_SPILLS_free(x)      ((x)->free)
#define LOCAL_SPILLS_used(x)      ((x)->used)


/* One for each kind of spill location: */
static LOCAL_SPILLS lra_float_spills, lra_int_spills;
static LOCAL_SPILLS swp_float_spills, swp_int_spills;

#ifdef TARG_X8664
static LOCAL_SPILLS lra_sse2_spills;
static LOCAL_SPILLS lra_x87_spills;
static LOCAL_SPILLS lra_mmx_spills;
#endif /* TARG_X8664 */

#ifdef KEY
static LOCAL_SPILLS lra_float32_spills;
static LOCAL_SPILLS lra_int32_spills;
#endif

extern VARIANT Memop_Variant(WN *);

/*
 * only rematerialize LDID's homeable by gra if spilling in service
 * to gra.
 */
#define Can_Rematerialize_TN(tn, cl) \
(TN_is_rematerializable(tn) || (TN_is_gra_homeable(tn) && cl == CGSPILL_GRA))

#ifdef TARG_IA64
void
ld_2_ld_fill (OPS *ops, BOOL force=FALSE) {

  BOOL ld_is_last_op = TRUE ;

  OP * op = OPS_last(ops);

  if (!OP_load (op)) {
    if (OP_code(op) == TOP_cmp_i_ne || OP_code(op) == TOP_mov_t_br) {
      op = OP_prev(op); ld_is_last_op = FALSE ;
    } else 
      op = NULL ;
  }

  if (!op) return ;

  switch (OP_code(op)) {
  case TOP_ld1:
  case TOP_ld2:
  case TOP_ld4:
  case TOP_ld8:
      if (!TN_is_take_nat(OP_result(op,0)) && !force) break; 
      OPS_Remove_Op (ops,op);
      op = Mk_OP(TOP_ld8_fill, OP_result(op,0), 
                 OP_opnd(op,OP_find_opnd_use(op,OU_predicate)),
                 Gen_Enum_TN(ECV_ldhint),
                 OP_opnd(op,OP_find_opnd_use(op,OU_base)));

      if (ld_is_last_op || (OPS_length(ops) == 1)) 
        OPS_Append_Op (ops,op);
      else
        OPS_Insert_Op_Before (ops, OPS_last(ops),op);
    
  }
}

void 
st_2_st_spill (OPS * ops, BOOL force=FALSE) {
 
  OP * op = OPS_last(ops) ;
  TN *tn = OP_opnd(op,OP_find_opnd_use(op,OU_storeval));
  switch (OP_code(op)) {
  case TOP_st1:
  case TOP_st2:
  case TOP_st4:
  case TOP_st8:
      if (!TN_is_take_nat(tn) && !force) break; 
      OPS_Remove_Op (ops,op); 
      op = Mk_OP(TOP_st8_spill, /* op code */
                 OP_opnd(op,OP_find_opnd_use(op,OU_predicate)), /* guarded predicate */
                 Gen_Enum_TN(ECV_sthint), /* hint */
                 OP_opnd(op,OP_find_opnd_use(op,OU_base)),
                 OP_opnd(op,OP_find_opnd_use(op,OU_storeval)));
      OPS_Append_Op (ops,op);
    
    break ;
  case TOP_stfs:
  case TOP_stfd:
  case TOP_stfe:
    break ;
  }
}
#endif

/* =======================================================================
 *
 *  LOCAL_SPILLS_Reset
 *
 *  Reset the free list so that all the allocated spill locations are
 *  available.
 *
 * =======================================================================
 */
static void
LOCAL_SPILLS_Reset( LOCAL_SPILLS *slc)
{
  ST_LIST *stlist;

  if (LOCAL_SPILLS_free(slc) == NULL) {
    LOCAL_SPILLS_free(slc) = LOCAL_SPILLS_used(slc);
  }
  else {
    for (stlist = LOCAL_SPILLS_free(slc); 
	 ST_LIST_rest(stlist) != NULL;
	 stlist = ST_LIST_rest(stlist));
    ST_LIST_rest(stlist) = LOCAL_SPILLS_used(slc);
  }
  LOCAL_SPILLS_used(slc) = NULL;
}


/* =======================================================================
 *
 *  Gen_Spill_Symbol
 *
 *  Return a spill symbol of type <ty> and allocate it to memory. Use 
 *  the <root> as the prefix of the temporary name.
 *
 * =======================================================================
 */
static ST *
Gen_Spill_Symbol (TY_IDX ty, const char *root)
{
  ST *st;

  st = Gen_Temp_Symbol (ty, root);
  ++Spill_Var_Cnt;
  Allocate_Temp_To_Memory (st);

  /* Remember the ID of this spill location.
   */
  if (CG_opt_level > 0) {
    INT level = ST_level(st);

    if (!spill_id_pool_inited) {
      MEM_POOL_Initialize(&spill_id_pool, "cg_spill_symbol_id_bit_set", FALSE);
      spill_id_pool_inited = TRUE;
    }

    if (spill_ids == NULL) MEM_POOL_Push(&spill_id_pool);

    if (level > max_spill_level) {
      spill_ids = TYPE_MEM_POOL_REALLOC_N(BS *, 
					  &spill_id_pool,
					  spill_ids,
					  max_spill_level+1,
					  level+1);
      bzero(spill_ids + max_spill_level + 1, 
	    (level - max_spill_level) * sizeof(*spill_ids));
      max_spill_level = level;
    }

    if (spill_ids[level] == NULL) {
      spill_ids[level] = BS_Create_Empty(ST_index(st), &spill_id_pool);
    }

    spill_ids[level] = BS_Union1D(spill_ids[level], ST_index(st), &spill_id_pool);
  }

  return st;
}


/* =======================================================================
 *
 *  LOCAL_SPILLS_Get_Spill_Location
 *
 *  Return a ST representing a spill location.  If one is availble on the
 *  given 'slc', it is allocated.  Otherwise a new one is generated. The 
 *  allocated spill location is added to the 'slc's used list.
 *
 * =======================================================================
 */
static ST *
LOCAL_SPILLS_Get_Spill_Location (LOCAL_SPILLS *slc, const char *root)
{
  ST *result;

  if ( LOCAL_SPILLS_free(slc) != NULL ) {
    ST_LIST *stl = LOCAL_SPILLS_free(slc);
    /* Remove the first entry in the free list and add it to the used list */
    LOCAL_SPILLS_free(slc) = ST_LIST_rest(stl);
    ST_LIST_rest(stl) = LOCAL_SPILLS_used(slc);
    LOCAL_SPILLS_used(slc) = stl;
    result = ST_LIST_first(stl);
  }
  else {
    result = Gen_Spill_Symbol (LOCAL_SPILLS_mem_type(slc), root);
    LOCAL_SPILLS_used(slc) = ST_LIST_Push( result, LOCAL_SPILLS_used(slc),
                                                 &MEM_pu_pool);
  }
  return result;
}


/* =======================================================================
 *
 *  Check_Phase_And_PU
 *
 *  See if we've changed phases or PUs since the last time we were called.
 *  If we have, print a message announcing the new phase and PU.
 *
 * =======================================================================
 */
static void
Check_Phase_And_PU(void)
{
  const char *cur_phase = Get_Error_Phase();
  if (Cur_PU_Name != Remat_PU || cur_phase != Remat_Phase) {
    fprintf(TFile, "\n<Rematerialize> PU: %s, Phase: %s\n", Cur_PU_Name,
	    cur_phase);
    Remat_Phase = cur_phase;
    Remat_PU = Cur_PU_Name;
  }
}


/* =======================================================================
 *
 *  CGSPILL_Reset_Local_Spills
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
CGSPILL_Reset_Local_Spills (void)
{
  LOCAL_SPILLS_Reset(&lra_float_spills);
  LOCAL_SPILLS_Reset(&lra_int_spills);
  LOCAL_SPILLS_Reset(&swp_float_spills);
  LOCAL_SPILLS_Reset(&swp_int_spills);
#ifdef TARG_X8664
  LOCAL_SPILLS_Reset(&lra_sse2_spills);
  LOCAL_SPILLS_Reset(&lra_x87_spills);
  LOCAL_SPILLS_Reset(&lra_mmx_spills);
#endif /* TARG_X8664 */

#ifdef KEY
  LOCAL_SPILLS_Reset(&lra_float32_spills);
  LOCAL_SPILLS_Reset(&lra_int32_spills);
#endif
}


/* =======================================================================
 *
 *  CGSPILL_Initialize_For_PU
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
CGSPILL_Initialize_For_PU(void)
{
  LOCAL_SPILLS *slc;
  slc = &lra_int_spills;
  LOCAL_SPILLS_mem_type(slc) = Spill_Int_Type;
  LOCAL_SPILLS_free(slc) = NULL;
  LOCAL_SPILLS_used(slc) = NULL;
  slc = &lra_float_spills;
  LOCAL_SPILLS_mem_type(slc) = Spill_Float_Type;
  LOCAL_SPILLS_free(slc) = NULL;
  LOCAL_SPILLS_used(slc) = NULL;
  slc = &swp_int_spills;
  LOCAL_SPILLS_mem_type(slc) = Spill_Int_Type;
  LOCAL_SPILLS_free(slc) = NULL;
  LOCAL_SPILLS_used(slc) = NULL;
  slc = &swp_float_spills;
  LOCAL_SPILLS_mem_type(slc) = Spill_Float_Type;
  LOCAL_SPILLS_free(slc) = NULL;
  LOCAL_SPILLS_used(slc) = NULL;
#ifdef TARG_X8664
  slc = &lra_sse2_spills;
  LOCAL_SPILLS_mem_type(slc) = Quad_Type;
  LOCAL_SPILLS_free(slc) = NULL;
  LOCAL_SPILLS_used(slc) = NULL;

  slc = &lra_x87_spills;
  LOCAL_SPILLS_mem_type(slc) = MTYPE_To_TY( MTYPE_F10 );
  LOCAL_SPILLS_free(slc) = NULL;
  LOCAL_SPILLS_used(slc) = NULL;

  slc = &lra_mmx_spills;
  LOCAL_SPILLS_mem_type(slc) = MTYPE_To_TY( MTYPE_M8I1 );
  LOCAL_SPILLS_free(slc) = NULL;
  LOCAL_SPILLS_used(slc) = NULL;
#endif /* TARG_X8664 */

#ifdef KEY
  slc = &lra_int32_spills;
  LOCAL_SPILLS_mem_type(slc) = Spill_Int32_Type;
  LOCAL_SPILLS_free(slc) = NULL;
  LOCAL_SPILLS_used(slc) = NULL;

  slc = &lra_float32_spills;
  LOCAL_SPILLS_mem_type(slc) = Spill_Float32_Type;
  LOCAL_SPILLS_free(slc) = NULL;
  LOCAL_SPILLS_used(slc) = NULL;

  spill_sym_info_map.clear();
#endif // KEY

  Trace_Remat = Get_Trace(TP_CG, 4);
  Trace_GRA_spill_placement = Get_Trace(TP_GRA, 0x2000);
  Remat_Phase = NULL;
  Remat_PU = NULL;

  spill_ids = NULL;
  max_spill_level = -1;
}


/* =======================================================================
 *
 *  CGSPILL_Finalize_For_PU
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
CGSPILL_Finalize_For_PU(void)
{
  if (spill_ids) MEM_POOL_Pop(&spill_id_pool);
}


/* ====================================================================
 *
 *  CGSPILL_TN_Can_Be_Spilled
 *
 *  See interface description.
 *
 * ==================================================================== 
 */
BOOL
CGSPILL_TN_Can_Be_Spilled ( const TN* tn )
{
  Is_True( TN_register_class(tn) != ISA_REGISTER_CLASS_UNDEFINED,
             ("Trying to spill TN%d with undefined register class",
              TN_number(tn)) );

  // even if ! REGISTER_CLASS_can_store,
  // may still be possible to store because of special-case code
  // in cgtarg_load_from_memory
  return TRUE;
}

  
/* ====================================================================
 *
 *  CGSPILL_Get_TN_Spill_Location
 *
 *  See interface description.
 *
 * ==================================================================== 
 */
ST *
CGSPILL_Get_TN_Spill_Location (TN *tn, CGSPILL_CLIENT client)
{
  ST *mem_location;
  LOCAL_SPILLS *slc;

  if ( ! CGSPILL_TN_Can_Be_Spilled(tn) ) {
    /* say what register we're trying to spill */
    ErrMsg(EC_Ill_Reg_Spill1, REGISTER_name(TN_register_class(tn), 0));
    /* suggest a reduced optimization level */
    ErrMsg(EC_Ill_Reg_Spill2b, CG_opt_level-1);
    /* above error message aborts, so we don't get here */
  }

  /* only gra can home such tn's.  this should either occur in SWP
   * (with a dup'd tn), or in LRA (after gra, so we don't care if
   * the attributes are reset ... CGSPILL_LGRA is used when spilling
   * globals from within LRA).
   */
  if (TN_is_gra_homeable(tn) && client != CGSPILL_GRA) {
    if (client != CGSPILL_SWP && client != CGSPILL_LRA &&
	client != CGSPILL_LGRA) {
      DevWarn("CGSPILL_Get_TN_Spill_Location: turning off gra homing for client other than SWP or LRA for TN%d.\n", TN_number(tn));
    }
    Reset_TN_is_gra_homeable(tn);
    Set_TN_home(tn, NULL);
  }

  if (   Can_Rematerialize_TN(tn, client) && client != CGSPILL_SWP) {
    if (Trace_Remat) {
      #pragma mips_frequency_hint NEVER
      Check_Phase_And_PU();
      fprintf(TFile, "<Rematerialize> Suppressing allocation of spill "
		     "loc for rematerializable TN%d.\n", TN_number(tn));
    }
    return (ST *)TN_home(tn);
  }

  switch (client) {
  case CGSPILL_GRA:
  case CGSPILL_LCL:
  case CGSPILL_LGRA:
    mem_location = TN_spill(tn);
    if (mem_location == NULL) {
      const char *root;
      TY_IDX mem_type = TN_is_float(tn) || TN_is_fcc_register(tn) ? 
		        Spill_Float_Type : Spill_Int_Type;
#ifdef TARG_X8664
      /* bug#1741
	 For -m32, the size of long double is 96-bit long.
       */
      if( TN_size(tn) == 16 || TN_size(tn) == 12 ){
	mem_type = TN_register_class(tn) == ISA_REGISTER_CLASS_x87
	  ? MTYPE_To_TY( MTYPE_F10 ) : Quad_Type;
      }
      // MMX
      if (TN_register_class(tn) == ISA_REGISTER_CLASS_mmx) {
	mem_type = MTYPE_To_TY(MTYPE_M8I1);
      }
#endif /* TARG_X8664 */

#ifdef KEY
      if( CG_min_spill_loc_size && TN_size(tn) <= 4 ){
	mem_type = TN_is_float(tn) ? Spill_Float32_Type : Spill_Int32_Type;
      }
#if defined(TARG_SL)
      if (TN_size(tn) <= MTYPE_byte_size(TY_mtype(mem_type)))
        DevWarn("TN size is larger than the size of spill type.") ;
#endif
#endif

      if (client == CGSPILL_GRA) {
	root = SYM_ROOT_GRA;
      } else if (client == CGSPILL_LGRA) {
	root = SYM_ROOT_LGRA;
      } else {
	root = SYM_ROOT_LCL;
      }
      mem_location = Gen_Spill_Symbol (mem_type, root);
      Set_TN_spill(tn, mem_location);
    }
    break;
  case CGSPILL_LRA:
    mem_location = TN_spill (tn);
    if (mem_location == NULL) {
      slc = TN_is_float(tn) || TN_is_fcc_register(tn) ?
	      &lra_float_spills : &lra_int_spills;
#ifdef TARG_X8664
      if (TN_register_class(tn) == ISA_REGISTER_CLASS_mmx) {	// MMX
	slc = &lra_mmx_spills;
      } else if( TN_size(tn) == 16 || TN_size(tn) == 12 ){
	slc = TN_register_class(tn) == ISA_REGISTER_CLASS_x87
	  ? &lra_x87_spills : &lra_sse2_spills;
      }
#endif /* TARG_X8664 */

#ifdef KEY
      if( CG_min_spill_loc_size && TN_size(tn) <= 4 ){
	slc = TN_is_float(tn) ? &lra_float32_spills : &lra_int32_spills;
      }
#endif

      mem_location = LOCAL_SPILLS_Get_Spill_Location (slc, SYM_ROOT_LRA);
      Set_TN_spill(tn, mem_location);
    }
    break;
  case CGSPILL_SWP:
    FmtAssert(!TN_is_fcc_register(tn), ("SWP attempted to spill an fcc register"));
    slc = TN_is_float(tn) ? &swp_float_spills : &swp_int_spills;
    mem_location = LOCAL_SPILLS_Get_Spill_Location (slc, SYM_ROOT_SWP);
    break;
  }

  return mem_location;
}


/* ====================================================================
 *
 *  CGSPILL_Is_Spill_Location
 *
 *  See interface description.
 *
 * ==================================================================== 
 */
BOOL
CGSPILL_Is_Spill_Location (ST *mem_loc)
{
  return    mem_loc 
	 && ST_level(mem_loc) <= max_spill_level
	 && spill_ids
	 && spill_ids[ST_level(mem_loc)]
	 && BS_MemberP(spill_ids[ST_level(mem_loc)], ST_index(mem_loc));
}


/* ====================================================================
 *
 *  CGSPILL_OP_Spill_Location
 *
 *  See interface description.
 *
 * ==================================================================== 
 */
ST *
CGSPILL_OP_Spill_Location (OP *op)
{
  ST *mem_loc = NULL;

  if (spill_ids) {
    TN *spill_tn = NULL;

    if (OP_load(op) && (OP_results(op) == 1)
#ifdef TARG_IA64
	&& OP_spill_restore(op)
#endif
	) {
      spill_tn = OP_result(op,0);
    } else if (OP_store(op)
#ifdef TARG_IA64
	       && OP_spill_restore(op)
#endif
	       ) {
      spill_tn = OP_opnd(op,TOP_Find_Operand_Use(OP_code(op), OU_storeval));
    }
#ifdef KEY
    if( spill_tn != NULL &&
	TN_has_spill( spill_tn ) ){
      INT n = TOP_Find_Operand_Use( OP_code(op), OU_offset );
      if( n >= 0 ){
	TN* ctn = OP_opnd( op, n );
	if ( TN_is_constant(ctn) && TN_is_symbol(ctn) &&
	     TN_var(ctn) == TN_spill(spill_tn))
	  mem_loc = TN_spill( ctn );
      }
    }
#else
    if (spill_tn) mem_loc = TN_spill(spill_tn);
#endif

    if (mem_loc &&
	(ST_level(mem_loc) > max_spill_level ||
	 spill_ids[ST_level(mem_loc)] == NULL ||
	 !BS_MemberP(spill_ids[ST_level(mem_loc)], ST_index(mem_loc)))
    ) {
      mem_loc = NULL;
    }
  }

  return mem_loc;
}

/* ======================================================================
 *
 * CGSPILL_Cost_Estimate
 *
 * See interface description
 *
 * ======================================================================*/
void 
CGSPILL_Cost_Estimate (TN *tn, ST *mem_loc, 
		       float *store_cost, float *restore_cost,
		       CGSPILL_CLIENT client)
{
  /* TODO: more accurate instruction counts for the rematerializable TNs,
   * TODO: target/type dependent cost estimates
   * TODO: count memory references for the restores in rematerializable TNs
   */
  if (Can_Rematerialize_TN(tn, client))
  {
    WN *home = TN_home(tn);
    OPS	OPs  = OPS_EMPTY;
    TN  *result = tn;

    *store_cost = 0.0F;

    switch (WN_operator(home))
    {
    case OPR_LDID:
      {
	OPCODE opcode = WN_opcode(home);
	Exp_Load (OPCODE_rtype(opcode), OPCODE_desc(opcode), tn, WN_st(home),
		  WN_offset(home), &OPs, home ? Memop_Variant (home) : V_NONE);
#ifdef TARG_IA64
        ld_2_ld_fill (&OPs) ;
#endif
	*restore_cost = OPS_length(&OPs);
	*store_cost = *restore_cost;
      }
      break;
    case OPR_LDA:
      Exp_Lda(	WN_rtype(home),
		result,
		WN_st(home),
		WN_lda_offset(home),
		OPERATOR_UNKNOWN,
		&OPs
#ifdef TARG_SL
                , WN_is_internal_mem_ofst(home)
#endif
		);
      *restore_cost = OPS_length(&OPs);
      break; 
    case OPR_INTCONST:
      *restore_cost = 1.0F;
      if (!CGTARG_Can_Load_Immediate_In_Single_Instruction (WN_const_val(home)))
	*restore_cost += 1.0F;
      break;
    case OPR_CONST:
      Exp_OP1 (WN_opcode(home), result, Gen_Symbol_TN(WN_st(home),0,0), &OPs);
      *restore_cost = OPS_length(&OPs) + .25F;
      break;
    }
  }
  else
  {
    *store_cost = CGSPILL_DEFAULT_STORE_COST;
    *restore_cost = CGSPILL_DEFAULT_RESTORE_COST;
    if (   mem_loc
	&& !CGTARG_Can_Load_Immediate_In_Single_Instruction(ST_ofst(mem_loc)))
    {
      *store_cost += 2.0F;
      *restore_cost += 2.0F;
    }
  }

  if (Trace_Remat)
  {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "<Rematerialize> CGSPILL_Cost_Estimate for rematerializable TN%d (store_cost=%f) (restore_cost=%f)\n", TN_number(tn), *store_cost, *restore_cost);
  }
}


/* ======================================================================
 *
 * CGSPILL_Load_From_Memory
 *
 * See interface description
 *
 * ======================================================================*/
void
CGSPILL_Load_From_Memory (TN *tn, ST *mem_loc, OPS *ops, CGSPILL_CLIENT client,
			  BB *bb)
{
  INT32 max_sdata_save = Max_Sdata_Elt_Size;

  /* hack to prevent use of gp in loading constant in prolog and epilog
   * before/after gp setup/restore.  NULL bb if no possibility of spilling
   * in prolog or epilog (used in swp calls).
   */
  if (bb && (BB_entry(bb) || BB_exit(bb))) {
    Max_Sdata_Elt_Size = 0;
  }
  if (   Can_Rematerialize_TN(tn, client)
      && ((WN *)mem_loc == TN_home(tn))
  ) {
    WN *home = TN_home(tn);
    OPCODE opcode;
    OPERATOR opr;
    TN *const_tn;

    FmtAssert (home != NULL, 
		    ("No home for rematerializable TN%d", TN_number(tn)));
    FmtAssert((WN *)mem_loc == TN_home(tn),
	      ("Rematerializable TN%d has a spill loc", TN_number(tn)));
    opcode = WN_opcode (home);
    opr = OPCODE_operator(opcode);
    /* make sure st is allocated. */
    if (OPCODE_has_sym(opcode) && WN_st(home) != NULL) {
      Allocate_Object (WN_st(home));
    }
    switch (opr) {
    case OPR_LDID:
      /* homing load */
      Exp_Load (OPCODE_rtype(opcode), OPCODE_desc(opcode), tn,
		WN_st(home), WN_offset(home), ops, home ? Memop_Variant (home) : V_NONE);
      if (Trace_Remat && !TN_is_gra_homeable(tn)) {
#pragma mips_frequency_hint NEVER
	fprintf(TFile, "<Rematerialize> LDID for rematerializeable TN%d\n",
		TN_number(tn));
      }
      break;
    case OPR_LDA:
      Exp_Lda (OPCODE_rtype(opcode), tn, WN_st(home), WN_lda_offset(home),
	       OPERATOR_UNKNOWN, ops
#ifdef TARG_SL
	       , WN_is_internal_mem_ofst(home)
#endif
	       );
      break;
    case OPR_CONST:
#ifdef TARG_X8664
    if (OPCODE_rtype(opcode) == MTYPE_V16F4 ||
	OPCODE_rtype(opcode) == MTYPE_V16F8 ||
	OPCODE_rtype(opcode) == MTYPE_V16I1 ||
	OPCODE_rtype(opcode) == MTYPE_V16I2 ||
	OPCODE_rtype(opcode) == MTYPE_V16I4 ||
	OPCODE_rtype(opcode) == MTYPE_V16I8) {
      TCON then = ST_tcon_val(WN_st(home));
      TCON now  = Create_Simd_Const (OPCODE_rtype(opcode), then);
      ST *sym = New_Const_Sym (Enter_tcon (now), 
			       Be_Type_Tbl(OPCODE_rtype(opcode)));
      Allocate_Object(sym);
      Exp_OP1 (opcode, tn, Gen_Symbol_TN (sym, 0, 0), ops);
    } else
#endif
      Exp_OP1 (opcode, tn, Gen_Symbol_TN(WN_st(home),0,0), ops);
      break;
    case OPR_INTCONST:
      switch (opcode) {
      case OPC_I8INTCONST:
      case OPC_U8INTCONST:
#if defined(EMULATE_LONGLONG) && !defined(TARG_SL) && !defined(TARG_PPC32)
        {
          extern TN *Gen_Literal_TN_Pair(UINT64);
          const_tn = Gen_Literal_TN_Pair((UINT64) WN_const_val(home));
        }
#else
	const_tn = Gen_Literal_TN (WN_const_val(home), 8);
#endif
	break;
      case OPC_I4INTCONST:
	const_tn = Gen_Literal_TN ((INT32) WN_const_val(home), 4);
	break;
      case OPC_U4INTCONST:
#ifdef TARG_X8664
	/* Opteron will zero-out the higher 32-bit. (bug#3387) */
	const_tn = Gen_Literal_TN ((UINT32) WN_const_val(home), 4);
#else
	/* even for U4 we sign-extend the value
	 * so it matches what we want register to look like */
	const_tn = Gen_Literal_TN ((INT32) WN_const_val(home), 4);
#endif // TARG_X8664
	break;
      default:
	ErrMsg (EC_Unimplemented,
		"CGSPILL_Restore: cannot handle WHIRL node");
      }
      Exp_OP1 (opcode, tn, const_tn, ops);
      break;
    default:
      ErrMsg (EC_Unimplemented, "CGSPILL_Restore: cannot handle WHIRL node");
    }

    if (Trace_Remat) {
      #pragma mips_frequency_hint NEVER
      Check_Phase_And_PU();
      fprintf(TFile, "<Rematerialize> rematerializing TN%d:\n", TN_number(tn));
      Print_OPS(ops);
    }
  } else {

    /* Must actually load it from memory
     */
    CGTARG_Load_From_Memory(tn, mem_loc, ops);
#ifdef TARG_IA64
    ld_2_ld_fill (ops);
#endif
#ifdef KEY
    CGSPILL_Inc_Restore_Count(mem_loc);
#endif
  }
  Max_Sdata_Elt_Size = max_sdata_save;
#ifdef TARG_IA64
  if(ops->last)
	 Set_OP_spill_restore(ops->last);
#endif
}


/* ======================================================================
 *
 * CGSPILL_Store_To_Memory
 *
 * See interface description
 *
 * ======================================================================*/
void 
CGSPILL_Store_To_Memory (TN *src_tn, ST *mem_loc, OPS *ops,
			 CGSPILL_CLIENT client, BB *bb)
{
  INT32 max_sdata_save = Max_Sdata_Elt_Size;

  /* hack to prevent use of gp in loading constant in prolog and epilog
   * before/after gp setup/restore.  NULL bb if no possibility of spilling
   * in prolog or epilog (used in swp calls).
   */
  if (bb && (BB_entry(bb) || BB_exit(bb))) {
    Max_Sdata_Elt_Size = 0;
  }

  /* Don't need to store back a rematerializable constant
   */
  if (   Can_Rematerialize_TN(src_tn, client)
      && ((WN *)mem_loc == TN_home(src_tn))
  ) {
    WN *home = TN_home(src_tn);
    if (WN_operator(home) == OPR_LDID) {
      Exp_Store (OPCODE_desc(WN_opcode(home)), src_tn, WN_st(home),
		 WN_offset(home), ops, home ? Memop_Variant (home) : V_NONE);
#ifdef TARG_IA64
      st_2_st_spill (ops) ;
#endif
    } else if (Trace_Remat) {
#pragma mips_frequency_hint NEVER
      Check_Phase_And_PU();
      fprintf(TFile, "<Rematerialize> Suppressing spill of rematerializable "
		     "TN%d.\n", TN_number(src_tn));
    }
#ifdef TARG_IA64
    if(ops->last)
	   Set_OP_spill_restore(ops->last);
#endif
#ifdef KEY
    // Looks like a bug in the Open64 compiler. 
    // In the case of entry/exit BBs, the Max_Sdata_Elt_Size might get
    // reset and never set back if the next return is executed.
    Max_Sdata_Elt_Size = max_sdata_save;  
#endif
    return;
  }

  CGTARG_Store_To_Memory(src_tn, mem_loc, ops);
#ifdef TARG_IA64
  st_2_st_spill (ops);
#endif
  Max_Sdata_Elt_Size = max_sdata_save;
#ifdef TARG_IA64
  if(ops->last)
	 Set_OP_spill_restore(ops->last);
#endif

#ifdef KEY
  CGSPILL_Record_Spill(mem_loc, OPS_last(ops));
#endif
}


/* ======================================================================
 * 
 * Find_Last_Copy
 *
 * Find the last copy in the block following the stack adjustment (i.e.
 * save tn copies).
 *
 * ====================================================================== */
static OP* Find_Last_Copy(BB *bb)
{
  OP *last_copy_op;

  if (BB_handler(bb)) {
    if (!GRA_Spill_Around_Save_TN_Copies()) {
      return NULL;
    }
    last_copy_op = BB_first_op(bb);
  } else {
    last_copy_op = BB_entry_sp_adj_op(bb);
  }
  if (last_copy_op && GRA_Spill_Around_Save_TN_Copies()) {
    OP *tmp_op;
    for (tmp_op = OP_next(last_copy_op); tmp_op;
	 tmp_op = OP_next(tmp_op)) {
      if (OP_copy(tmp_op) && TN_is_save_reg(OP_result(tmp_op,0))) {
	last_copy_op = tmp_op;
      }
    }
  }
  return last_copy_op;
}


/* ======================================================================
 * 
 * Find_First_Copy
 *
 * Find the first copy in the block preceeding the stack adjustment (i.e.
 * save tn copies).
 *
 * ====================================================================== */
static OP* Find_First_Copy(BB *bb)
{
  OP *first_copy_op = BB_exit_sp_adj_op(bb);
  if (first_copy_op && GRA_Spill_Around_Save_TN_Copies()) {
    OP *tmp_op;
    for (tmp_op = OP_prev(first_copy_op); tmp_op;
	 tmp_op = OP_prev(tmp_op)) {
      if (OP_copy(tmp_op) && TN_is_save_reg(OP_opnd(tmp_op, OP_COPY_OPND))) {
	first_copy_op = tmp_op;
      }
    }
  }
  return first_copy_op;
}

/* ======================================================================
 *
 * CGSPILL_Prepend_Ops
 *
 * See interface description
 *
 * ======================================================================*/
void
CGSPILL_Prepend_Ops (BB *bb, OPS *ops)
{
  if (OPS_first(ops) == NULL) return;


  Reset_BB_scheduled (bb);

  if (BB_entry(bb)) {
    OP *last_copy = Find_Last_Copy(bb);
    if (last_copy) {
      BB_Insert_Ops_After (bb, last_copy, ops);
    } else {
      BB_Prepend_Ops(bb, ops);
    }
    return;
  }

  if (Is_Caller_Save_GP && PU_Has_Calls && !Constant_GP) {
    OP *op;
    ISA_REGISTER_CLASS cl = TN_register_class(GP_TN);
    REGISTER reg = TN_register(GP_TN);

    FOR_ALL_OPS_OPs(ops, op) {
      if (OP_Refs_Reg(op, cl, reg)) {
        // The new code uses GP.  Determine where it is reset.

        for (OP *bb_op = BB_first_op(bb); bb_op != NULL; bb_op = OP_next(bb_op)) {
          if (OP_Defs_Reg(bb_op, cl, reg)) {
            BB_Insert_Ops_After (bb, bb_op, ops);
            return;
          }
        }

        break;
      }
    }

  }

  BB_Prepend_Ops (bb, ops);
}


/* ======================================================================
 *
 * CGSPILL_Insert_Ops_Before
 *
 * See interface description
 *
 * ======================================================================*/
void
CGSPILL_Insert_Ops_Before (BB *bb, OP *point, OPS *ops)
{
  OP *op;
#if Is_True_On
  /* Assert that if BB_entry, point is not before SP adjustment. */
  if (BB_entry(bb) && !BB_handler(bb)) {
    FmtAssert (OP_Follows(point, BB_entry_sp_adj_op(bb)),
	       ("cannot insert spill ops before SP adjust"));
  }
#endif
  if (BB_exit(bb)) {
    /* check if the <point> is after the SP adjustment OP. If it is,
     * change the point to be the SP adjustment OP.
     */
    OP *first_copy = Find_First_Copy(bb);
    if (OP_Follows(point, first_copy)) point = first_copy;
  }
  else {
    OP *prev_op = OP_prev (point);
    if (prev_op != NULL && OP_xfer(prev_op)) {
      point = prev_op;
    }
  }
  /* inserting ops before, but want ops associated with srcpos of point */
  FOR_ALL_OPS_OPs(ops, op) OP_srcpos(op) = OP_srcpos(point);
  BB_Insert_Ops_Before (bb, point, ops);
  Reset_BB_scheduled (bb);
}

/* ======================================================================
 *
 * Is_Aliased_With_Home
 *
 * Determine if an op is an aliased memory reference with the home 
 * location of tn.
 *
 * ======================================================================*/
static BOOL Is_Aliased_With_Home(TN *tn, OP* op)
{
  if (OP_store(op) || OP_load(op)) {
    WN *wn = Get_WN_From_Memory_OP(op);
    if (wn != NULL) {
      ALIAS_RESULT alias = Aliased(Alias_Manager, TN_home(tn), wn);
      return alias == SAME_LOCATION;
    }
  }
  return FALSE;
}

/* ======================================================================
 *
 * CGSPILL_Insert_Ops_Before_First_Use
 *
 * See interface description
 *
 * ======================================================================*/
void
CGSPILL_Insert_Ops_Before_First_Use (TN *tn, BB *bb, OPS *ops)
{
  OP *op;

  for (op = BB_first_op(bb); op != NULL; op = OP_next(op)) {
    INT i;
    for (i = 0; i < OP_opnds(op); i++) {
      if (OP_opnd(op,i) == tn || 
	  (TN_is_gra_homeable(tn) && Is_Aliased_With_Home(tn, op))) {
	CGSPILL_Insert_Ops_Before(bb, op, ops);
	if (Trace_GRA_spill_placement) {
	  #pragma mips_frequency_hint NEVER
	  fprintf(TFile, "<gra> moving spill load of TN%d before first ",
		  TN_number(tn));
	  fprintf(TFile, "use in BB%d\n", BB_id(bb));
	}
	return;
      }
    }
    for (i = 0; i < OP_results(op); i++) {
      if (OP_result(op,i) == tn) {
	if (OP_cond_def(op)) {
	  CGSPILL_Insert_Ops_Before(bb, op, ops);
	  if (Trace_GRA_spill_placement) {
	    #pragma mips_frequency_hint NEVER
	    fprintf(TFile, "<gra> moving spill load of TN%d before first cond_def",
		    TN_number(tn));
	    fprintf(TFile, "use in BB%d\n", BB_id(bb));
	  }
	  return;
	}
	else {
	  // the spill reload is redundant.  don't do it.
	  if (Trace_GRA_spill_placement) {
	    #pragma mips_frequency_hint NEVER
	    fprintf(TFile, "<gra> not adding redundant spill load of TN%d to ",
		    TN_number(tn));
	    fprintf(TFile,"bottom of BB%d\n ", BB_id(bb));
	  }
	  return;
	}
      }
    }
  }

  //
  // use not found.  put it at the bottom of the block.
  //
  CGSPILL_Append_Ops(bb, ops);
  if (Trace_GRA_spill_placement) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "<gra> moving spill load of TN%d to bottom of BB%d\n ",
	    TN_number(tn), BB_id(bb));
  }

}

/* ======================================================================
 *
 * Rename_Opnds
 *
 * Rename each occurance of <old> with <new> in the operands of <op>
 *
 * ======================================================================*/
static void
Rename_Opnds( OP* op, TN* old_tn, TN* new_tn )
{
  INT i;

  for (i = OP_opnds(op) - 1; i >= 0; --i) {
    if (OP_opnd(op,i) == old_tn) 
      Set_OP_opnd(op,i,new_tn);
  }
}
  

/* ======================================================================
 *
 * OP_Refs_TN_Reg
 *
 * Does <op> have an operand TN allocated to the same register as <tn>?
 * If so, return the operand TN by reference in <dep_tn>.
 *
 * ======================================================================*/
static BOOL
OP_Refs_TN_Reg( OP* op, TN* tn, TN** dep_tn )
{
  if ( TN_is_constant(tn) || TN_is_const_reg(tn) )
    return FALSE;
  else {
    INT i;
    REGISTER reg = TN_register(tn);
    ISA_REGISTER_CLASS reg_class = TN_register_class(tn);
  
    if ( reg == REGISTER_UNDEFINED )
      return FALSE;

    for ( i = OP_opnds(op) - 1; i >= 0; --i ) {
      TN* opnd = OP_opnd(op,i);

      if ( ! (TN_is_constant(opnd) || TN_is_const_reg(opnd)) ) {
        REGISTER opnd_reg = TN_register(opnd);
        ISA_REGISTER_CLASS opnd_reg_class = TN_register_class(opnd);

        if ( opnd_reg_class == reg_class
             && opnd_reg == reg
        ) {
          *dep_tn = opnd;
          return TRUE;
        }
      }
    }

    return FALSE;
  }
}


/* ======================================================================
 * Fix_Xfer_Dependences
 *
 * We are about to append <ops> to <bb>, a block ending in a branch.  We can't
 * actually append <ops>, we have to put them in front of the branch.  But
 * what if they define a TN that the branch uses?  This function check for
 * this situation.  For each operand of the branch that is defined in <ops>, a
 * copy is made into a temp just above the branch and the reference in the
 * branch is renamed to use the temp.  After this transformation, it is safe
 * to insert OPs just above the branch.
 *
 * We also check for the case TN is different but the assigned register
 * is the same.
 * ======================================================================*/
static void
Fix_Xfer_Dependences (BB* bb, OPS* ops)
{
  OP* op;
  OP* xfer_op = BB_last_op(bb);

  Is_True(OP_xfer(xfer_op),("Expecting a xfer op as last in block"));

  /* if an OP in <ops> writes a TN used in the branch instruction, we'll
   * have to use a temp to hold the branch operand:
   */
  FOR_ALL_OPS_OPs_FWD(ops,op) {
    INT i;
    for (i = 0; i < OP_results(op); ++i) {
      TN *dep_tn = OP_result(op,i);

      if (    OP_Refs_TN (xfer_op, dep_tn)
           || OP_Refs_TN_Reg (xfer_op, OP_result(op,i), &dep_tn)
      ) {
        OPS copy_ops = OPS_EMPTY;
        TN* tmp_tn = Dup_TN_Even_If_Dedicated (dep_tn);

        Exp_COPY (tmp_tn, dep_tn, &copy_ops);
        BB_Insert_Ops_Before (bb, xfer_op, &copy_ops);
        Rename_Opnds (xfer_op, dep_tn, tmp_tn);
      }
    }
  }
}


/* ======================================================================
 *
 * CGSPILL_Append_Ops
 *
 * See interface description
 *
 * ======================================================================*/
void
CGSPILL_Append_Ops (BB *bb, OPS *ops)
{
  OP *before_point = NULL;
  BOOL after_tagged_op = FALSE;
  LABEL_IDX tag_idx = 0;

  OP *orig_last_op;
  OP *new_last_op;

  if (BB_exit(bb)) {
    before_point = Find_First_Copy(bb);
    /* check if sp_adj is in delay slot */
    if (OP_prev(before_point) != NULL && OP_xfer(OP_prev(before_point))) {
	before_point = OP_prev(before_point);
    }
  }  else {
    OP *last_op;

    if (PROC_has_branch_delay_slot())
      BB_Move_Delay_Slot_Op (bb);
    last_op = BB_last_op (bb);
    if (last_op != NULL && OP_xfer(last_op)) {
      Fix_Xfer_Dependences (bb, ops);
      before_point = BB_last_op(bb);
    }

#ifdef TARG_SL //adjust before_point if the bb has c2_joint instruction
  if(last_op != NULL) {
    OP* tmp;
    FOR_ALL_BB_OPs_REV(bb, tmp)
    {
       if(OP_code(tmp) == TOP_c2_joint  || OP_code(tmp) == TOP_loop )
       {
          before_point = tmp;
	   break;
	 }
    }
  }
#endif 

#ifdef TARG_X8664
    else if (last_op != NULL && OP_code(last_op) == TOP_savexmms)
      before_point = BB_last_op(bb);
#endif
  }

  if (before_point != NULL) {
    BB_Insert_Ops_Before (bb, before_point, ops);
  }
   else {
    BB_Append_Ops (bb, ops);
  }
  Reset_BB_scheduled (bb);
}


/* ======================================================================
 *
 * CGSPILL_Insert_Ops_After
 *
 * See interface description
 *
 * ======================================================================*/
void
CGSPILL_Insert_Ops_After (BB *bb, OP *point, OPS *ops)
{
  OP *op;
#if Is_True_On
  /* Assert that if BB_exit(bb), then point is not after SP adjust. */
  if (BB_exit(bb)) {
    FmtAssert (OP_Follows(BB_exit_sp_adj_op(bb), point),
	       ("cannot insert spill ops after SP adjust"));
  }
  /* Assert that <point> is not a branch or a delay slot instruction */
  if (OP_xfer(point) || 
      (OP_prev(point) != NULL && OP_xfer(OP_prev(point))))
  {
    FmtAssert (FALSE, ("Cannot insert spill ops after branch."));
  }
#endif
  if (BB_entry(bb)) {
    /* check if the <point> is before the SP adjustment OP. If it is,
     * change the point to be the SP adjustment OP.
     */
    OP *last_copy = Find_Last_Copy(bb);
    if (last_copy && OP_Follows(last_copy, point)) point = last_copy;
  }
  /* inserting ops before, but want ops associated with srcpos of point */
  FOR_ALL_OPS_OPs(ops, op) OP_srcpos(op) = OP_srcpos(point);
  BB_Insert_Ops_After (bb, point, ops);
  Reset_BB_scheduled (bb);
}

/* ======================================================================
 *
 * CGSPILL_Insert_Ops_After_Last_Def
 *
 * See interface description
 *
 * ======================================================================*/
void
CGSPILL_Insert_Ops_After_Last_Def (TN *tn, BB *bb, OPS *ops)
{
  OP *op;
  INT i;

  for (op = BB_last_op(bb); op != NULL; op = OP_prev(op)) {
    for (i = 0; i < OP_results(op); ++i) {
      if (OP_result(op,i) == tn) goto found_def;
    }
    if (TN_is_gra_homeable(tn) && Is_Aliased_With_Home(tn, op)) break;
  }
found_def:
  
  //
  // def not found.  put spill at top of block.
  //
  if (op == NULL) {
    CGSPILL_Prepend_Ops(bb, ops);
    if (Trace_GRA_spill_placement) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "<gra> moving spill store of TN%d to top of BB%d\n ",
	      TN_number(tn), BB_id(bb));
    }
  } else if (op == BB_last_op(bb)) {
    // line info of spilled op
    OP *tmp;
    FOR_ALL_OPS_OPs(ops, tmp) {
      if (OP_srcpos(tmp) == 0) 
        OP_srcpos(tmp) = OP_srcpos(op);
    }
    //
    // we do some fancy footwork if we're spilling the delay slot
    // instruction.  let CGSPILL_Append_Ops handle it.
    //
    CGSPILL_Append_Ops(bb, ops);
  } else {
    CGSPILL_Insert_Ops_After(bb, op, ops);
    if (Trace_GRA_spill_placement) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "<gra> moving spill store of TN%d before last ",
	      TN_number(tn));
      fprintf(TFile, "def in BB%d\n", BB_id(bb));
    }
    
  }
}

/* ======================================================================
 *
 * CGSPILL_Force_Rematerialization_For_BB
 *
 * See interface description
 *
 * ======================================================================*/
void
CGSPILL_Force_Rematerialization_For_BB(BB *bb)
{
  OP *op;

  if (!CGSPILL_Rematerialize_Constants) return;

  FOR_ALL_BB_OPs(bb, op) {
    INT k;

    for (k = 0; k < OP_opnds(op); k++) {
      TN *tn = OP_opnd(op, k);
      if (TN_is_rematerializable(tn)) {
	OP *new_op;
	OPS ops = OPS_EMPTY;
	TN *new_tn = Dup_TN(tn);
	CGSPILL_Load_From_Memory(new_tn, (ST *)TN_home(new_tn), &ops,
				 CGSPILL_LCL, bb);

        if (Is_CG_LOOP_Op(op)) {
	  FOR_ALL_OPS_OPs(&ops, new_op) {
	    CGPREP_Init_Op(new_op);
	    CG_LOOP_Init_Op(new_op);
          }
	}
	CGSPILL_Insert_Ops_Before(bb, op, &ops);
	// if (CG_DEP_Has_Graph(bb)) {
	//  FOR_ALL_OPS_OPs(&ops, new_op) {
	//    CG_DEP_Op_Added(new_op);
	//  }
	// }

	Reset_TN_is_rematerializable(new_tn);
	Set_TN_home(new_tn, NULL);

	Set_OP_opnd(op, k, new_tn);
	// if (CG_DEP_Has_Graph(bb)) {
	//  CG_DEP_Op_Opnd_Changed(op, k, tn);
	// }
      }
    }
  }
}


/* ======================================================================
 *
 * CGSPILL_Force_Rematerialization
 *
 * See interface description
 *
 * ======================================================================*/
void
CGSPILL_Force_Rematerialization(void)
{
  BB *bb;

  if (!CGSPILL_Rematerialize_Constants) return;

  if (Trace_Remat) {
    #pragma mips_frequency_hint NEVER
    fprintf ( TFile, "\n%s%s\tIR before CGSPILL_Force_Rematerialization\n%s%s\n",
              DBar, DBar, DBar, DBar );
    for (bb = REGION_First_BB; bb; bb = BB_next(bb)) Print_BB(bb);
    fprintf ( TFile, "%s%s\n", DBar, DBar );
  }

  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    RID *rid = BB_rid(bb);
    if (!rid || !RID_has_reg_alloc(rid)) {
      CGSPILL_Force_Rematerialization_For_BB(bb);
    }
  }

  if (Trace_Remat) {
    #pragma mips_frequency_hint NEVER
    fprintf ( TFile, "\n%s%s\tIR after CGSPILL_Force_Rematerialization\n%s%s\n",
              DBar, DBar, DBar, DBar );
    for (bb = REGION_First_BB; bb; bb = BB_next(bb)) Print_BB(bb);
    fprintf ( TFile, "%s%s\n", DBar, DBar );
  }
}


/* ======================================================================
 *
 * CGSPILL_Attach_Lda_Remat
 *
 * See interface description
 *
 * ======================================================================*/
void CGSPILL_Attach_Lda_Remat(TN *tn, TYPE_ID typ, INT64 offset, ST *st)
{
  if (CGSPILL_Rematerialize_Constants) {
    OPCODE opc = OPCODE_make_op(OPR_LDA, typ, MTYPE_V);
    WN *wn = WN_CreateLda(opc, offset, (TY_IDX) NULL, st);
    if (wn) {
      Set_TN_is_rematerializable(tn);
      Set_TN_home(tn, wn);
    }
  }
}


/* ======================================================================
 *
 * CGSPILL_Attach_Intconst_Remat
 *
 * See interface description
 *
 * ======================================================================*/
void CGSPILL_Attach_Intconst_Remat(TN *tn, INT64 val)
{
  if (CGSPILL_Rematerialize_Constants) {
    WN *wn = WN_CreateIntconst(OPC_I8INTCONST, val);
    if (wn) {
      Set_TN_is_rematerializable(tn);
      Set_TN_home(tn, wn);
    }
  }
}


/* ======================================================================
 *
 * CGSPILL_Attach_Floatconst_Remat
 *
 * See interface description
 *
 * ======================================================================*/
void CGSPILL_Attach_Floatconst_Remat(TN *tn, TYPE_ID typ, double val)
{
  TCON tcon = Host_To_Targ_Float(typ, val);
  ST *st = New_Const_Sym(Enter_tcon(tcon), Be_Type_Tbl(TCON_ty(tcon)));
  CGSPILL_Attach_Const_Remat(tn, typ, st);
}


/* ======================================================================
 *
 * CGSPILL_Attach_Const_Remat
 *
 * See interface description
 *
 * ======================================================================*/
void CGSPILL_Attach_Const_Remat(TN *tn, TYPE_ID typ, ST *st)
{
  if (CGSPILL_Rematerialize_Constants) {
    OPCODE opc = OPCODE_make_op(OPR_CONST, typ, MTYPE_V);
    WN *wn = WN_CreateConst(opc, st);
    if (wn) {
      Set_TN_is_rematerializable(tn);
      Set_TN_home(tn, wn);
    }
  }
}


#ifdef KEY
// Record that SPILL_OP is associated with SPILL_LOC.
void
CGSPILL_Record_Spill (ST *spill_loc, OP *spill_op)
{
  SPILL_SYM_INFO &info = spill_sym_info_map[ST_IDX((INTPTR)spill_loc)];

  // Don't keep track of more than one spill.
  info.Set_Spill_Op(info.Spill_Count() == 0 ? spill_op : NULL);
  info.Inc_Spill_Count();
}


void
CGSPILL_Inc_Restore_Count (ST *spill_loc)
{
  SPILL_SYM_INFO &info = spill_sym_info_map[ST_IDX((INTPTR)spill_loc)];
  info.Inc_Restore_Count();
}


SPILL_SYM_INFO &
CGSPILL_Get_Spill_Sym_Info (ST *spill_loc)
{
  return spill_sym_info_map[ST_IDX((INTPTR)spill_loc)];
}
#endif
