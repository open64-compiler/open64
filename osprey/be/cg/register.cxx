/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 * Module: register.c
 * $Revision: 1.17 $
 * $Date: 06/03/20 17:31:47-08:00 $
 * $Author: gautam@jacinth.keyresearch $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.register.cxx $
 *
 * Revision history:
 *  17-May-93 - Original Version
 *
 * Description:
 *
 *      register implementation.
 *
 * ====================================================================
 * ====================================================================
 */

#define INCLUDING_IN_REGISTER

#include <vector>
#include <utility>
#include "defs.h"
#include "errors.h"
#include "tracing.h"
#include "mempool.h"
#include "config.h"
#include "glob.h"
#include "util.h"
#include "calls.h"
#include "data_layout.h"
#include "tn.h"
#include "targ_sim.h"
#include "op.h"

#include "targ_isa_registers.h"
#include "targ_abi_properties.h"
#include "targ_isa_operands.h"

#include "register.h"
#include "cgtarget.h"

/* ====================================================================
 * Define a few things that differ among code generators
 * ====================================================================
 */

/* ====================================================================
 * Is the frame-pointer register required for the current function
 * ====================================================================
 */

#define FRAME_POINTER_REQUIRED_FOR_PU	\
  	(Current_PU_Stack_Model != SMODEL_SMALL || PUSH_FRAME_POINTER_ON_STACK)

/* ====================================================================
 * Shared data structures
 * ====================================================================
 */

/* Exported data:
 */
REGISTER_SUBCLASS_INFO REGISTER_SUBCLASS_info[ISA_REGISTER_SUBCLASS_MAX + 1];
ISA_REGISTER_CLASS  REGISTER_CLASS_vec[ISA_REGISTER_CLASS_MAX + 1];
REGISTER_CLASS_INFO REGISTER_CLASS_info[ISA_REGISTER_CLASS_MAX + 1];
CLASS_REG_PAIR      CLASS_REG_PAIR_zero;
CLASS_REG_PAIR      CLASS_REG_PAIR_ep;
CLASS_REG_PAIR      CLASS_REG_PAIR_gp;
CLASS_REG_PAIR      CLASS_REG_PAIR_sp;
CLASS_REG_PAIR      CLASS_REG_PAIR_fp;
CLASS_REG_PAIR      CLASS_REG_PAIR_ra;
CLASS_REG_PAIR      CLASS_REG_PAIR_v0;
CLASS_REG_PAIR      CLASS_REG_PAIR_static_link;
CLASS_REG_PAIR      CLASS_REG_PAIR_pfs;
CLASS_REG_PAIR      CLASS_REG_PAIR_lc;
CLASS_REG_PAIR      CLASS_REG_PAIR_ec;
CLASS_REG_PAIR      CLASS_REG_PAIR_true;
CLASS_REG_PAIR      CLASS_REG_PAIR_fzero;
CLASS_REG_PAIR      CLASS_REG_PAIR_fone;
#ifdef TARG_X8664
CLASS_REG_PAIR      CLASS_REG_PAIR_f0;
#endif
#ifdef TARG_IA64
CLASS_REG_PAIR      CLASS_REG_PAIR_tp;
#endif
#if defined(TARG_SL)
CLASS_REG_PAIR      CLASS_REG_PAIR_k0;
CLASS_REG_PAIR      CLASS_REG_PAIR_k1;
CLASS_REG_PAIR      CLASS_REG_PAIR_ja;
CLASS_REG_PAIR      CLASS_REG_PAIR_lc0;
CLASS_REG_PAIR      CLASS_REG_PAIR_lc1;
CLASS_REG_PAIR      CLASS_REG_PAIR_lc2;
CLASS_REG_PAIR      CLASS_REG_PAIR_lc3;
CLASS_REG_PAIR      CLASS_REG_PAIR_hi;
CLASS_REG_PAIR      CLASS_REG_PAIR_acc0;
CLASS_REG_PAIR      CLASS_REG_PAIR_acc1;
CLASS_REG_PAIR      CLASS_REG_PAIR_acc2;
CLASS_REG_PAIR      CLASS_REG_PAIR_acc3;
CLASS_REG_PAIR      CLASS_REG_PAIR_add0;
CLASS_REG_PAIR      CLASS_REG_PAIR_add1;
CLASS_REG_PAIR      CLASS_REG_PAIR_add2;
CLASS_REG_PAIR      CLASS_REG_PAIR_add3;
CLASS_REG_PAIR      CLASS_REG_PAIR_add4;
CLASS_REG_PAIR      CLASS_REG_PAIR_add5;
CLASS_REG_PAIR      CLASS_REG_PAIR_add6;
CLASS_REG_PAIR      CLASS_REG_PAIR_add7;
CLASS_REG_PAIR      CLASS_REG_PAIR_addsize0;
CLASS_REG_PAIR      CLASS_REG_PAIR_addsize1;
CLASS_REG_PAIR      CLASS_REG_PAIR_addsize2;
CLASS_REG_PAIR      CLASS_REG_PAIR_addsize3;
CLASS_REG_PAIR      CLASS_REG_PAIR_addsize4;
CLASS_REG_PAIR      CLASS_REG_PAIR_addsize5;
CLASS_REG_PAIR      CLASS_REG_PAIR_addsize6;
CLASS_REG_PAIR      CLASS_REG_PAIR_addsize7;

CLASS_REG_PAIR      CLASS_REG_PAIR_c2acc;
CLASS_REG_PAIR      CLASS_REG_PAIR_c2cond;
CLASS_REG_PAIR      CLASS_REG_PAIR_c2mvsel;
CLASS_REG_PAIR      CLASS_REG_PAIR_c2vlcs;
CLASS_REG_PAIR      CLASS_REG_PAIR_c2movpat;
#endif
#ifdef TARG_LOONGSON
CLASS_REG_PAIR      CLASS_REG_PAIR_at; 
CLASS_REG_PAIR      CLASS_REG_PAIR_hi; 
CLASS_REG_PAIR      CLASS_REG_PAIR_lo; 
CLASS_REG_PAIR      CLASS_REG_PAIR_fsr;
#endif

const CLASS_REG_PAIR CLASS_REG_PAIR_undef =
  {CREATE_CLASS_N_REG(ISA_REGISTER_CLASS_UNDEFINED,REGISTER_UNDEFINED)};

#if ISA_REGISTER_MAX >= 64
const REGISTER_SET REGISTER_SET_EMPTY_SET = { 0 };
#endif /* ISA_REGISTER_MAX >= 64 */

/* Track the "allocatable" state of each register.
 */
enum {
  AS_default = 0,	/* the default is what targ_info says */
  AS_allocatable = 1,
  AS_not_allocatable = 2
};

static mUINT8 reg_alloc_status[ISA_REGISTER_CLASS_MAX + 1][REGISTER_MAX + 1];

// list of registers that should not be allocated, both globally and locally.
static vector< pair< ISA_REGISTER_CLASS, REGISTER> > dont_allocate_these_registers;
static vector< pair< ISA_REGISTER_CLASS, REGISTER> > dont_allocate_these_registers_in_pu;


/* ====================================================================
 *
 *  Set_CLASS_REG_PAIR
 *
 *  Set the rclass and register.x
 *
 * ====================================================================
 */
void Set_CLASS_REG_PAIR(CLASS_REG_PAIR& rp, ISA_REGISTER_CLASS rclass, REGISTER reg)
{
  rp.class_reg.rclass = rclass;
  rp.class_reg.reg = reg;
}


/* ====================================================================
 *
 *  REGISTER_SET_Range
 *
 *  Return the a set of the registers: low .. high
 *
 * ====================================================================
 */
REGISTER_SET
REGISTER_SET_Range(UINT low, UINT high)
{
#if ISA_REGISTER_MAX < 64
  Is_True(low >= REGISTER_MIN && low <= high && high <= REGISTER_MAX,
	  ("REGISTER_SET_Range: bad range specification"));

  UINT leading_zeros = (sizeof(REGISTER_SET_WORD) * 8) - high;
  UINT trailing_zeros = low - REGISTER_MIN;
  return   ((REGISTER_SET_WORD)-1 << (leading_zeros + trailing_zeros)) 
	>> leading_zeros;
#else /* ISA_REGISTER_MAX < 64 */
  INT i;
  REGISTER_SET set;
  for (i = 0; i <= MAX_REGISTER_SET_IDX; ++i) {
    UINT this_low = (i * 64) + REGISTER_MIN;
    UINT this_high = this_low + 63;
    if (low > this_high || high < this_low) {
      set.v[i] = 0;
    } else {
      UINT leading_zeros = high > this_high ? 0 : this_high - high;
      UINT trailing_zeros = low < this_low ? 0 : low - this_low;
      set.v[i] =   ((REGISTER_SET_WORD)-1 << (leading_zeros + trailing_zeros)) 
		>> leading_zeros;
    }
  }
  return set;
#endif /* ISA_REGISTER_MAX < 64 */
}

/* ====================================================================
 * ====================================================================
 *
 * Initialization and termination
 *
 * ====================================================================
 * ====================================================================
 */


/* ====================================================================
 *
 *  Initialize_Register_Class
 *
 *  Initialize the register class 'rclass'. A register class may be
 *  intialized multiple times.
 *
 * ====================================================================
 */
static void
Initialize_Register_Class(
  ISA_REGISTER_CLASS rclass
)
{
  INT32              i;
  const ISA_REGISTER_CLASS_INFO *icinfo = ISA_REGISTER_CLASS_Info(rclass);
  const char        *rcname         = ISA_REGISTER_CLASS_INFO_Name(icinfo);
  INT		     bit_size       = ISA_REGISTER_CLASS_INFO_Bit_Size(icinfo);
  INT                first_isa_reg  = ISA_REGISTER_CLASS_INFO_First_Reg(icinfo);
  INT                last_isa_reg   = ISA_REGISTER_CLASS_INFO_Last_Reg(icinfo);
  INT                register_count = last_isa_reg - first_isa_reg + 1;
  REGISTER_SET       allocatable    = REGISTER_SET_EMPTY_SET;
  REGISTER_SET       caller         = REGISTER_SET_EMPTY_SET;
  REGISTER_SET       callee         = REGISTER_SET_EMPTY_SET;
  REGISTER_SET       func_argument  = REGISTER_SET_EMPTY_SET;
  REGISTER_SET       func_value     = REGISTER_SET_EMPTY_SET;
  REGISTER_SET       shrink_wrap    = REGISTER_SET_EMPTY_SET;
  REGISTER_SET	     stacked        = REGISTER_SET_EMPTY_SET;
  REGISTER_SET	     rotating       = REGISTER_SET_EMPTY_SET;
//#ifdef TARG_X8664
//  REGISTER_SET	     eight_bit_regs = REGISTER_SET_EMPTY_SET;
//#endif

  /* Verify we have a valid rclass and that the type used to implement 
   * a register set is large enough.
   */
  FmtAssert(rclass >= ISA_REGISTER_CLASS_MIN && rclass <= ISA_REGISTER_CLASS_MAX,
	    ("invalide register class %d", (INT)rclass));
  FmtAssert((sizeof(REGISTER_SET) * 8) >= register_count,
	    ("REGISTER_SET type cannot represent all registers in "
	     "the class %s", rcname));

  REGISTER_CLASS_name(rclass) = rcname;

  /* Now make sets of various register properties:
   */
  for ( i = 0; i < register_count; ++i ) {
    INT      isa_reg        = i + first_isa_reg;
    REGISTER reg            = i + REGISTER_MIN;
    BOOL     is_allocatable = ABI_PROPERTY_Is_allocatable(rclass, isa_reg);
    INT      alloc_status   = reg_alloc_status[rclass][reg];

    /* CG likes to pretend that a class with only one register can't
     * be allocated, so perpetuate that illusion.
     */
    if ( register_count <= 1 ) is_allocatable = FALSE;

    switch ( alloc_status ) {
      case AS_allocatable:
	is_allocatable = TRUE;
	break;
      case AS_not_allocatable:
	is_allocatable = FALSE;
	break;
      case AS_default:
	break;
      default:
	Is_True(FALSE, ("unhandled allocations status: %d", alloc_status));
    }

#ifdef TARG_IA64    
    // "(rclass==ISA_REGISTER_CLASS_branch && reg==1)" means the branch register <b0>.
    //This is to set the register <b0> to be non-allocatable during Register allocation.
    //The reason is : enable <b0> to be allocatable will leads to change content of <b0>,
    // while the PU need <b0> be the right value as its return address.
    // By doing this, OPs likes 
    //
    //		mov b0,r9;
    //		br.few b0
    //		........(the value of b0 has not been saved/restored,so the return value is not correct.)
    //          br.ret.sptk.many b0 ;;
    //
    //	 will not appear. And in those OPs <b0> will be replaced by <b6>.
    
    if (rclass==ISA_REGISTER_CLASS_branch && reg==1)
    	is_allocatable = FALSE;
#endif

    if ( is_allocatable ) {
      allocatable = REGISTER_SET_Union1(allocatable,reg);
#ifdef ABI_PROPERTY_global_ptr
      if ( ABI_PROPERTY_Is_global_ptr(rclass, isa_reg) ) {
        if ( GP_Is_Preserved ) {
          /* neither caller nor callee saved (always preserved). */
        } else if ( Is_Caller_Save_GP ) {
          /* caller-saved. */
          caller = REGISTER_SET_Union1(caller, reg);
        } else {
          /* callee-saved. */
          callee = REGISTER_SET_Union1(callee, reg);
        }
      }
      else 
#endif
	{
#ifdef ABI_PROPERTY_callee	// has calling convention
        if ( ABI_PROPERTY_Is_callee(rclass, isa_reg) ) {
          callee = REGISTER_SET_Union1(callee, reg);
          shrink_wrap = REGISTER_SET_Union1(shrink_wrap, reg);
        }
        if ( ABI_PROPERTY_Is_caller(rclass, isa_reg) )
          caller = REGISTER_SET_Union1(caller, reg);
        if ( ABI_PROPERTY_Is_func_arg(rclass, isa_reg) )
          func_argument = REGISTER_SET_Union1(func_argument, reg);
        if ( ABI_PROPERTY_Is_func_val(rclass, isa_reg) )
          func_value = REGISTER_SET_Union1(func_value, reg);
#endif
#if defined(TARG_MIPS) || defined(TARG_IA64) || defined(TARG_LOONGSON)
        if ( ABI_PROPERTY_Is_ret_addr(rclass, isa_reg) )
          shrink_wrap = REGISTER_SET_Union1(shrink_wrap, reg);
#endif
// #if !defined(TARG_MIPS) && !defined(TARG_X8664) && !defined(TARG_SL)
#ifdef ABI_PROPERTY_stacked
        if ( ABI_PROPERTY_Is_stacked(rclass, isa_reg) )
          stacked = REGISTER_SET_Union1(stacked, reg);
#endif
      }
    }

#ifdef TARG_X8664
    // Any better way to get rid of this itch?
    if (bit_size == 64 &&
	Is_Target_32bit() &&
	rclass != ISA_REGISTER_CLASS_float) {	// Bug 9109
      bit_size = 32;
    }
#endif
    REGISTER_bit_size(rclass, reg) = bit_size;
    REGISTER_machine_id(rclass, reg) = isa_reg;
    REGISTER_allocatable(rclass, reg) = is_allocatable;
#if !defined(TARG_NVISA) //
    REGISTER_name(rclass, reg) = ISA_REGISTER_CLASS_INFO_Reg_Name(icinfo, isa_reg);
#endif

#ifdef ABI_PROPERTY_frame_ptr
    if ( ABI_PROPERTY_Is_frame_ptr(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_fp, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_fp, rclass);
    }
#endif
#ifdef ABI_PROPERTY_static_link
    else if ( ABI_PROPERTY_Is_static_link(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_static_link, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_static_link, rclass);
    }
#endif
#if defined(TARG_MIPS) || defined(TARG_IA64) || defined(TARG_PPC32) || defined(TARG_LOONGSON)
    else if ( ABI_PROPERTY_Is_global_ptr(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_gp, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_gp, rclass);
    }
    else if ( ABI_PROPERTY_Is_ret_addr(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_ra, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_ra, rclass);
    }
#endif
#ifdef TARG_IA64
    else if ( ABI_PROPERTY_Is_thread_ptr(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_tp, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_tp, rclass);
    }
#endif
#ifdef ABI_PROPERTY_stack_ptr
    else if ( ABI_PROPERTY_Is_stack_ptr(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_sp, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_sp, rclass);
    }
#endif
#if defined(TARG_MIPS) || defined(TARG_IA64) || defined(TARG_LOONGSON)
    else if ( ABI_PROPERTY_Is_entry_ptr(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_ep, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_ep, rclass);
    }
    else if ( ABI_PROPERTY_Is_zero(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_zero, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_zero, rclass);
    }
#ifdef TARG_SL
    else if ( ABI_PROPERTY_Is_tmp1(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_k0, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_k0, rclass);
    }
    else if ( ABI_PROPERTY_Is_tmp2(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_k1, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_k1, rclass);
    } 
    //ja reg
    else if ( ABI_PROPERTY_Is_jump_addr(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_ja, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_ja, rclass);
    }
    // lc register
    else if (ABI_PROPERTY_Is_loopcount0(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_lc0, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_lc0, rclass);  
    }
    else if (ABI_PROPERTY_Is_loopcount1(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_lc1, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_lc1, rclass);  
    }
    else if (ABI_PROPERTY_Is_loopcount2(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_lc2, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_lc2, rclass);  
    }
    else if (ABI_PROPERTY_Is_loopcount3(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_lc3, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_lc3, rclass);  
    }
    else if (ABI_PROPERTY_Is_hi_reg(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_hi, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_hi, rclass);  
    }
    else if (ABI_PROPERTY_Is_acc0(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_acc0, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_acc0, rclass);
    }
    else if (ABI_PROPERTY_Is_acc1(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_acc1, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_acc1, rclass);
    }
    else if (ABI_PROPERTY_Is_acc2(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_acc2, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_acc2, rclass);
    }	
    else if (ABI_PROPERTY_Is_acc3(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_acc3, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_acc3, rclass);
    }
    else if (ABI_PROPERTY_Is_add0(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_add0, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_add0, rclass);
    }
    else if (ABI_PROPERTY_Is_add1(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_add1, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_add1, rclass);
    }
    else if (ABI_PROPERTY_Is_add2(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_add2, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_add2, rclass);
    }
    else if (ABI_PROPERTY_Is_add3(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_add3, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_add3, rclass);
    }
    else if (ABI_PROPERTY_Is_add4(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_add4, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_add4, rclass);
    }
    else if (ABI_PROPERTY_Is_add5(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_add5, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_add5, rclass);
    }
    else if (ABI_PROPERTY_Is_add6(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_add6, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_add6, rclass);
    }
    else if (ABI_PROPERTY_Is_add7(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_add7, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_add7, rclass);
    }
    else if (ABI_PROPERTY_Is_addrsize0(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_addsize0, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_addsize0, rclass);
    }
    else if (ABI_PROPERTY_Is_addrsize1(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_addsize1, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_addsize1, rclass);
    }
    else if (ABI_PROPERTY_Is_addrsize2(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_addsize2, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_addsize2, rclass);
    }
    else if (ABI_PROPERTY_Is_addrsize3(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_addsize3, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_addsize3, rclass);
    }
    else if (ABI_PROPERTY_Is_addrsize4(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_addsize4, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_addsize4, rclass);
    }
    else if (ABI_PROPERTY_Is_addrsize5(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_addsize5, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_addsize5, rclass);
    }
    else if (ABI_PROPERTY_Is_addrsize6(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_addsize6, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_addsize6, rclass);
    }
    else if (ABI_PROPERTY_Is_addrsize7(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_addsize7, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_addsize7, rclass);
    }
    else if (ABI_PROPERTY_Is_c2acc(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_c2acc, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_c2acc, rclass);  
    }
    else if (ABI_PROPERTY_Is_c2cond(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_c2cond, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_c2cond, rclass);  
    }
    else if (ABI_PROPERTY_Is_c2mvsel(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_c2mvsel, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_c2mvsel, rclass);  
    }
    else if (ABI_PROPERTY_Is_c2vlcs(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_c2vlcs, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_c2vlcs, rclass);  
    }
    else if (ABI_PROPERTY_Is_c2movpat(rclass, isa_reg)) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_c2movpat, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_c2movpat, rclass);  
    }
#endif // TARG_SL

#endif // TARG_MIPS
#if defined(TARG_IA64) || defined(TARG_LOONGSON)
    else if ( ABI_PROPERTY_Is_prev_funcstate(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_pfs, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_pfs, rclass);
    }
    else if ( ABI_PROPERTY_Is_loop_count(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_lc, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_lc, rclass);
    }
    else if ( ABI_PROPERTY_Is_epilog_count(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_ec, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_ec, rclass);
    }
    else if ( ABI_PROPERTY_Is_true_predicate(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_true, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_true, rclass);
    }
    else if ( ABI_PROPERTY_Is_fzero(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_fzero, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_fzero, rclass);
    }
    else if ( ABI_PROPERTY_Is_fone(rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_fone, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_fone, rclass);
    }
#endif
#ifdef TARG_LOONGSON
    else if ( ABI_PROPERTY_Is_assembler_temporary(rclass, isa_reg)) {
       Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_at, reg);
       Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_at, rclass);
    }
    else if ( ABI_PROPERTY_Is_hi(rclass, isa_reg)) {
       Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_hi, reg);
       Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_hi, rclass);
    }
    else if ( ABI_PROPERTY_Is_lo(rclass, isa_reg)) {
       Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_lo, reg);
       Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_lo, rclass);
    }else if (ABI_PROPERTY_Is_fsr(rclass, isa_reg)) {
       Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_fsr, reg);
       Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_fsr, rclass);
    }
#endif
  }

  REGISTER_CLASS_universe(rclass)          =
	REGISTER_SET_Range(REGISTER_MIN, REGISTER_MIN + register_count - 1);
  REGISTER_CLASS_allocatable(rclass)       = allocatable;
  REGISTER_CLASS_callee_saves(rclass)      = callee;
  REGISTER_CLASS_caller_saves(rclass)      = caller;
  REGISTER_CLASS_function_argument(rclass) = func_argument;
  REGISTER_CLASS_function_value(rclass)    = func_value;
  REGISTER_CLASS_shrink_wrap(rclass)       = shrink_wrap;
  REGISTER_CLASS_register_count(rclass)    = register_count;
  REGISTER_CLASS_stacked(rclass)           = stacked;
  REGISTER_CLASS_rotating(rclass)          = rotating;
  REGISTER_CLASS_can_store(rclass)
	= ISA_REGISTER_CLASS_INFO_Can_Store(icinfo);
  REGISTER_CLASS_multiple_save(rclass)
	= ISA_REGISTER_CLASS_INFO_Multiple_Save(icinfo);
//#ifdef TARG_X8664
//  REGISTER_CLASS_eight_bit_regs(rclass)    = eight_bit_regs;
//#endif

  /* There are multiple integer return regs -- v0 is the lowest
   * of the set.
   */
  if ( rclass == ISA_REGISTER_CLASS_integer ) {
    Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_v0, REGISTER_SET_Choose(func_value));
    Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_v0, rclass);
  }
#ifdef TARG_X8664
  if ( rclass == ISA_REGISTER_CLASS_float ) {
    Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_f0, REGISTER_SET_Choose(func_value));
    Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_f0, rclass);
  }
#endif

}


/* ====================================================================
 *
 *  Initialize_Register_Subclasses
 *
 *  Initialize the register subclass information cache.
 *
 * ====================================================================
 */
static void
Initialize_Register_Subclasses(void)
{
  ISA_REGISTER_SUBCLASS sc;

  for (sc = (ISA_REGISTER_SUBCLASS)0; // standard iterator skips _UNDEFINED (0)
       sc <= ISA_REGISTER_SUBCLASS_MAX; 
       sc = (ISA_REGISTER_SUBCLASS)(sc + 1))
  {
    INT i;
    ISA_REGISTER_CLASS rc;
    REGISTER_SET members = REGISTER_SET_EMPTY_SET;
    const ISA_REGISTER_SUBCLASS_INFO *scinfo = ISA_REGISTER_SUBCLASS_Info(sc);
    INT count = ISA_REGISTER_SUBCLASS_INFO_Count(scinfo);

    for (i = 0; i < count; ++i) {
      INT isa_reg = ISA_REGISTER_SUBCLASS_INFO_Member(scinfo, i);
      const char *reg_name = ISA_REGISTER_SUBCLASS_INFO_Reg_Name(scinfo, i);
      REGISTER reg = (REGISTER)(isa_reg + REGISTER_MIN);
      members = REGISTER_SET_Union1(members, reg);
      REGISTER_SUBCLASS_reg_name(sc, reg) = reg_name;
    }
    rc = ISA_REGISTER_SUBCLASS_INFO_Class(scinfo);
    members = REGISTER_SET_Intersection(members, REGISTER_CLASS_universe(rc));

    REGISTER_SUBCLASS_members(sc) = members;
    REGISTER_SUBCLASS_name(sc) = ISA_REGISTER_SUBCLASS_INFO_Name(scinfo);
    REGISTER_SUBCLASS_register_class(sc) = rc;
  }
}


/* ====================================================================
 *
 *  REGISTER_Begin
 *
 *  See interface description
 *
 * ====================================================================
 */
void
REGISTER_Begin(void)
{
  ISA_REGISTER_CLASS rclass;

  /*  Create the register classes for all the target registers.
   */
  FOR_ALL_ISA_REGISTER_CLASS( rclass ) {
	Initialize_Register_Class(rclass);
#ifdef HAS_STACKED_REGISTERS
    	REGISTER_Init_Stacked(rclass);
#endif
  }
  Initialize_Register_Subclasses();
  Init_Mtype_RegClass_Map();
}

struct Dont_Allocate_Dreg
{
  inline void operator() (UINT32, ST_ATTR *st_attr) const {
        if (ST_ATTR_kind (*st_attr) != ST_ATTR_DEDICATED_REGISTER)
	    return;
	PREG_NUM preg = ST_ATTR_reg_id(*st_attr);
	ISA_REGISTER_CLASS rclass;
	REGISTER reg;
	CGTARG_Preg_Register_And_Class(preg, &rclass, &reg);
	REGISTER_Set_Allocatable (rclass, reg, FALSE /* is_allocatable */);
  }
};
 
/* ====================================================================
 *
 *  REGISTER_Pu_Begin
 *
 *  See interface description
 *
 * ====================================================================
 */
extern void
REGISTER_Pu_Begin(void)
{
  ISA_REGISTER_CLASS rclass;

#ifdef TARG_X8664
  const TN* ebx_tn = Ebx_TN();
#endif

  /* Scan all the registers to find if the initial allocation status
   * will be different from the current state. The initial status
   * is all registers are set to their "default".
   */
  FOR_ALL_ISA_REGISTER_CLASS( rclass ) {
    REGISTER reg;
    BOOL re_init = FALSE;

    for ( reg = REGISTER_MIN;
	  reg <= REGISTER_CLASS_last_register(rclass);
	  reg++
    ) {
      if ( reg_alloc_status[rclass][reg] != AS_default) {
	reg_alloc_status[rclass][reg] = AS_default;
	re_init = TRUE;
      }
    }

#ifdef TARG_X8664
    /* Set register %ebx as un-allocatable under -m32 -fpic. %ebx will
       be used to hold the address pointing to the global offset table.
    */
    if( Is_Target_32bit() &&
	Gen_PIC_Shared    &&
	rclass == TN_register_class(ebx_tn) ){
      reg_alloc_status[rclass][TN_register(ebx_tn)] = AS_not_allocatable;
      re_init = TRUE;
    }
#endif

    if ( re_init ) Initialize_Register_Class(rclass);

    // always reset rotating register set
    REGISTER_CLASS_rotating(rclass) = REGISTER_SET_EMPTY_SET;

#ifdef HAS_STACKED_REGISTERS
    REGISTER_Init_Stacked(rclass);
#endif
  }

  // now check for any registers that user doesn't want allocated
  vector< pair< ISA_REGISTER_CLASS, REGISTER > >::iterator r;
  for (r = dont_allocate_these_registers.begin(); 
	r != dont_allocate_these_registers.end(); 
	++r)
  {
	REGISTER_Set_Allocatable ((*r).first, (*r).second, FALSE /* is_allocatable */);
  }
  // also check for user register variables in PU (local dreg list).
  if ( ST_ATTR_Table_Size (CURRENT_SYMTAB)) {
    For_all (St_Attr_Table, CURRENT_SYMTAB, 
	Dont_Allocate_Dreg());
  }

  if ( Get_Trace(TP_CG, 0x10) ) REGISTER_CLASS_Trace_All();
}

// possibly reset fp to non-allocatable if need a frame pointer
extern void
REGISTER_Reset_FP (void)
{
  ISA_REGISTER_CLASS rclass;
#ifndef TARG_X8664
  if (FRAME_POINTER_REQUIRED_FOR_PU && FP_TN != NULL) 
#else
  if (Current_PU_Stack_Model != SMODEL_SMALL || Opt_Level == 0)
#endif
  {
	rclass = TN_register_class(FP_TN);
	reg_alloc_status[rclass][TN_register(FP_TN)] = AS_not_allocatable;
	Initialize_Register_Class(rclass);
  }
}


/* ====================================================================
 * ====================================================================
 *
 * REGISTER_SET functions
 *
 * ====================================================================
 * ====================================================================
 */


/* ====================================================================
 *
 *  REGISTER_SET_Difference_Range
 *
 *  See interface description
 *
 * ====================================================================
 */
REGISTER_SET
REGISTER_SET_Difference_Range(
  REGISTER_SET   set,
  REGISTER       low,
  REGISTER       high
)
{
  return REGISTER_SET_Difference(set, REGISTER_SET_Range(low, high));
}


/* ====================================================================
 *
 *  REGISTER_SET_CHOOSE_ENGINE
 *
 *  The guts of the REGISTER_SET_Choose... functions. Return the index
 *  (1-based) of the first set bit in 'set'.
 *
 * ====================================================================
 */
inline REGISTER REGISTER_SET_Choose_Engine(
  REGISTER_SET set
)
{
  INT i = 0;
  do {
    REGISTER_SET_WORD w = REGISTER_SET_ELEM(set, i);
    INT regbase = REGISTER_MIN + (i * sizeof(REGISTER_SET_WORD) * 8);
    do {
      REGISTER_SET_WORD lowb = w & 0xff;
      if ( lowb ) return regbase + UINT8_least_sig_one[lowb];
    } while (regbase += 8, w >>= 8);
  } while (++i <= MAX_REGISTER_SET_IDX);
  return REGISTER_UNDEFINED;
}


/* ====================================================================
 *
 *  REGISTER_SET_Choose
 *
 *  See interface description
 *
 * ====================================================================
 */
extern REGISTER
REGISTER_SET_Choose(
  REGISTER_SET set
)
{
  return REGISTER_SET_Choose_Engine(set);
}


/* ====================================================================
 *
 *  REGISTER_SET_Choose_Range
 *
 *  See interface description
 *
 * ====================================================================
 */

extern REGISTER
REGISTER_SET_Choose_Range(
  REGISTER_SET set,
  REGISTER     low,
  REGISTER     high
)
{
  if (low > REGISTER_MAX) {
    return REGISTER_UNDEFINED;
  } else {
    REGISTER_SET temp;
    temp = REGISTER_SET_Intersection(set, REGISTER_SET_Range(low, high));
    return REGISTER_SET_Choose_Engine(temp);
  }
}


/* ====================================================================
 *
 *  REGISTER_SET_Choose_Next
 *
 *  See interface description
 *
 * ====================================================================
 */
extern REGISTER
REGISTER_SET_Choose_Next(
  REGISTER_SET set,
  REGISTER     reg
)
{
  if ( reg >= REGISTER_MAX ) {
    return REGISTER_UNDEFINED;
  } else {
    REGISTER_SET temp;
    temp = REGISTER_SET_Difference(set, REGISTER_SET_Range(REGISTER_MIN, reg));
    return REGISTER_SET_Choose_Engine(temp);
  }
}


/* ====================================================================
 *
 *  REGISTER_SET_Choose_Intersection
 *
 *  See interface description
 *
 * ====================================================================
 */
extern REGISTER
REGISTER_SET_Choose_Intersection(
  REGISTER_SET set1,
  REGISTER_SET set2
)
{
  REGISTER_SET set = REGISTER_SET_Intersection(set1, set2);
  return REGISTER_SET_Choose(set);
}


/* ====================================================================
 *
 *  REGISTER_SET_Size
 *
 *  See interface description
 *
 * ====================================================================
 */
extern INT32
REGISTER_SET_Size(
  REGISTER_SET set
)
{
  INT32 size = 0;
  INT i = 0;
  do {
    REGISTER_SET_WORD w = REGISTER_SET_ELEM(set, i);
    do {
      size += UINT8_pop_count[w & 0xff];
    } while (w >>= 8);
  } while (++i <= MAX_REGISTER_SET_IDX);
  return size;
}


/* ====================================================================
 *
 *  REGISTER_SET_Print
 *
 *  Prints out a register set
 *
 * ====================================================================
 */
extern void
REGISTER_SET_Print(
  REGISTER_SET regset,
  FILE *f
)
{
  REGISTER    i;
  const char *sep = "";

  fprintf(f, "[");
  for ( i = REGISTER_SET_Choose(regset);
        i != REGISTER_UNDEFINED;
        i = REGISTER_SET_Choose_Next(regset,i)
  ) {
    fprintf(f, "%s%d", sep, i);
    sep = ",";
  }

  fprintf(f, "]");
}

/* ====================================================================
 * ====================================================================
 *
 * REGISTER_CLASS functions
 *
 * ====================================================================
 * ====================================================================
 */

/* ====================================================================
 *
 *  REGISTER_CLASS_OP_Update_Mapping
 *
 *  See interface description
 *
 * ====================================================================
 */
extern void
REGISTER_CLASS_OP_Update_Mapping(
    OP *op
)
{
  INT32 i;
  const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info(OP_code(op));

  for (i = OP_results(op) - 1; i >= 0; --i) {
    TN *tn = OP_result(op,i);

    if (    TN_is_register(tn)
         && TN_register_class(tn) == ISA_REGISTER_CLASS_UNDEFINED
    ) {
      const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Result(oinfo, i);
      ISA_REGISTER_CLASS rclass = ISA_OPERAND_VALTYP_Register_Class(otype);
      Set_TN_register_class(tn, rclass);
    }
  }

  for ( i = OP_opnds(op) - 1; i >= 0; --i ) {
    TN *tn = OP_opnd(op,i);

    if (    TN_is_register(tn)
         && TN_register_class(tn) == ISA_REGISTER_CLASS_UNDEFINED
    ) {
      const ISA_OPERAND_VALTYP *otype = ISA_OPERAND_INFO_Operand(oinfo, i);
      ISA_REGISTER_CLASS rclass = ISA_OPERAND_VALTYP_Register_Class(otype);
      Set_TN_register_class(tn, rclass);
    }
  }
}

/* ====================================================================
 * ====================================================================
 *
 * REGISTER functions
 *
 * ====================================================================
 * ====================================================================
 */


/* ====================================================================
 *
 *  REGISTER_Print
 *
 *  Prints out a register to a specified file.
 *
 * ====================================================================
 */
extern void
REGISTER_Print(
  ISA_REGISTER_CLASS rclass,
  REGISTER reg,
  FILE *f
)
{
  fprintf(f, REGISTER_name(rclass, reg));
}

/* ====================================================================
 *
 *  CLASS_REG_PAIR_Print
 *
 *  Prints out a register to a specified file.
 *
 * ====================================================================
 */
extern void
CLASS_REG_PAIR_Print(
  CLASS_REG_PAIR crp,
  FILE *f
)
{
  REGISTER_Print(CLASS_REG_PAIR_rclass(crp), CLASS_REG_PAIR_reg(crp),f);
}


/* ====================================================================
 *
 *  REGISTER_Set_Allocatable
 *
 *  See interface description
 *
 * ====================================================================
 */
void
REGISTER_Set_Allocatable(
  ISA_REGISTER_CLASS rclass,
  REGISTER           reg,
  BOOL               is_allocatable
)
{
  //added by li xin
  if (rclass > ISA_REGISTER_CLASS_MAX)
	  return;
  INT prev_status = reg_alloc_status[rclass][reg];
  INT new_status  = is_allocatable ? AS_allocatable : AS_not_allocatable;

  if ( prev_status != new_status ) {
    reg_alloc_status[rclass][reg] = new_status;

    Initialize_Register_Class(rclass);
  }
}

/* ====================================================================
 * ====================================================================
 *
 * Tracing
 *
 * ====================================================================
 * ====================================================================
 */


#define TRUE_FALSE(b) ((b) ? "true" : "false")


/* =======================================================================
 *
 *  REGISTER_SET_Print_Name
 *
 *  
 *
 * =======================================================================
 */
static void
REGISTER_SET_Print_Name(
  ISA_REGISTER_CLASS rclass,
  REGISTER_SET regset,
  FILE *f
)
{
  REGISTER i;
  const char    *sep = "";

  fprintf(f, "[");
  for ( i = REGISTER_SET_Choose(regset);
        i != REGISTER_UNDEFINED;
        i = REGISTER_SET_Choose_Next(regset,i)
  ) {
    fprintf(f, "%s%s",sep,REGISTER_name(rclass,i));
    sep = ",";
  }

  fprintf(f, "]");
}


/* =======================================================================
 *
 *  REGISTER_Trace
 *
 *  See interface description
 *
 * =======================================================================
 */
extern void REGISTER_Trace(
  ISA_REGISTER_CLASS rclass,
  REGISTER reg
)
{
  if (    reg < REGISTER_MIN 
       || reg > REGISTER_CLASS_last_register(rclass) ) return;

  fprintf(TFile, "  reg %2d:"
		 " name=%-5s"
		 " bit-size=%-3d"
		 " mach-id=%-2d"
		 " allocatable=%-5s\n",
		 reg,
		 REGISTER_name(rclass, reg),
		 REGISTER_bit_size(rclass, reg),
		 REGISTER_machine_id(rclass, reg),
		 TRUE_FALSE(REGISTER_allocatable(rclass, reg)));
}


/* =======================================================================
 *
 *  REGISTER_CLASS_Trace
 *
 *  See interface description
 *
 * =======================================================================
 */
extern void REGISTER_CLASS_Trace(
  ISA_REGISTER_CLASS rclass
)
{
  REGISTER reg;
  REGISTER_SET set;

  fprintf(TFile, "register class %d (%s) register-count=%d can-store=%s\n",
		 rclass, REGISTER_CLASS_name(rclass),
		 REGISTER_CLASS_register_count(rclass),
		 TRUE_FALSE(REGISTER_CLASS_can_store(rclass)));

  for ( reg = REGISTER_MIN; reg <= REGISTER_MAX; reg++ ) {
    REGISTER_Trace(rclass, reg);
  }

  fprintf(TFile, "\n  universe: ");
  REGISTER_SET_Print(REGISTER_CLASS_universe(rclass), TFile);
  fprintf(TFile, "\n");

  set = REGISTER_CLASS_allocatable(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  allocatable: ");
    REGISTER_SET_Print(set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_callee_saves(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  callee_saves: ");
    REGISTER_SET_Print(set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_caller_saves(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  caller_saves: ");
    REGISTER_SET_Print(set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_function_argument(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  function_argument: ");
    REGISTER_SET_Print(set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_function_value(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  function_value: ");
    REGISTER_SET_Print(set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_shrink_wrap(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  shrink_wrap: ");
    REGISTER_SET_Print(set, TFile);
    fprintf(TFile, "\n");
  }

  fprintf(TFile, "\n  universe: ");
  REGISTER_SET_Print_Name(rclass, REGISTER_CLASS_universe(rclass), TFile);
  fprintf(TFile, "\n");

  set = REGISTER_CLASS_allocatable(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  allocatable: ");
    REGISTER_SET_Print_Name(rclass, set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_callee_saves(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  callee_saves: ");
    REGISTER_SET_Print_Name(rclass, set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_caller_saves(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  caller_saves: ");
    REGISTER_SET_Print_Name(rclass, set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_function_argument(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  function_argument: ");
    REGISTER_SET_Print_Name(rclass, set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_function_value(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  function_value: ");
    REGISTER_SET_Print_Name(rclass, set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_shrink_wrap(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  shrink_wrap: ");
    REGISTER_SET_Print_Name(rclass, set, TFile);
    fprintf(TFile, "\n");
  }
#ifdef TARG_LOONGSON
  set = REGISTER_CLASS_assembler_temporary(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  assembler temporary: ");
    REGISTER_SET_Print_Name(rclass, set, TFile);
    fprintf(TFile, "\n");
  }
#endif
}


/* =======================================================================
 *
 *  REGISTER_CLASS_Trace_All
 *
 *  See interface description
 *
 * =======================================================================
 */
extern void
REGISTER_CLASS_Trace_All(void)
{
  ISA_REGISTER_CLASS rclass;

  fprintf(TFile, "\n%s"
                 " REGISTERs and ISA_REGISTER_CLASSes for PU \"%s\"\n"
                 "%s",
                 DBar, Cur_PU_Name, DBar);

  FOR_ALL_ISA_REGISTER_CLASS( rclass ) {
    fprintf(TFile, "\n");
    REGISTER_CLASS_Trace(rclass);
  }
}

// user wants given register to not be allocatable in file.
void
Set_Register_Never_Allocatable (char *regname) 
{
	ISA_REGISTER_CLASS rclass;
	REGISTER reg;
	switch (regname[0]) {
	case 'r':
		rclass = ISA_REGISTER_CLASS_integer;
		break;
#if !defined(TARG_SL)
	case 'f':
		rclass = ISA_REGISTER_CLASS_float;
		break;
#endif
	default:
		FmtAssert(FALSE, ("unexpected reg letter %c", regname[0]));
	}
	reg = REGISTER_MIN + atoi(regname+1);
	FmtAssert(reg <= REGISTER_CLASS_last_register(rclass),
		("%s is not a valid register", regname));
	dont_allocate_these_registers.push_back( pair< ISA_REGISTER_CLASS, REGISTER>( rclass, reg ));
}

// user wants given register to not be allocatable in file.
void
Set_Register_Never_Allocatable (PREG_NUM preg) 
{
	ISA_REGISTER_CLASS rclass;
	REGISTER reg;
	CGTARG_Preg_Register_And_Class(preg, &rclass, &reg);
	dont_allocate_these_registers.push_back( pair< ISA_REGISTER_CLASS, REGISTER>( rclass, reg ));
}

