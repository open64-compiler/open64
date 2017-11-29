/*
 * Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
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


//  
//  Generate ISA properties information
///////////////////////////////////////
// The instructions are listed by category. The different categories of
// instructions are:
//
//   1. Operator attributes descriptors
//   2. Exception classification descriptors
//   3. Other operator descriptors (mostly for global optimization). 
//
// Within each ISA_PROPERTY instructions are listed in alphabetical order and
// as shown in the ISA manual
/////////////////////////////////////
//  $Revision: 1.6 $
//  $Date: 2006/05/30 06:52:26 $
//  $Author: weitang $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/isa/MIPS/isa_properties.cxx,v $


#include <stddef.h>
#include "topcode.h"
#include "isa_properties_gen.h"

main()
{
  ISA_PROPERTY
    move,   		/* Move operator */
    load,   		/* Memory load operator */
    store, 		/* Memory store operator */
    prefetch,		/* Prefetch operator */
    xfer, 		/* Control transfer operator */
    call, 		/* Subprogram call operator */
    cond, 		/* Call/xfer is conditional */
    likely, 		/* Cond call/xfer is likely */
    unalign_ld, 	/* Unaligned load operator */
    unalign_store,	/* Unaligned store operator */
    unknown_addr,	/* Memory operator potentially access any memory location */
    unknown_memdata,	/* Memory operator potentially alter data loaded/stored */
    cond_move, 		/* conditional move */
    uniq_res, 		/* Result must not be opnd */
    same_res,		/* Result must be same as opnd */
    noop, 		/* No-op operator */
    select, 		/* Operator is a machine level select */
    dummy, 		/* No-op doesn't get emitted */
    iadd, 		/* Integer add operator */
    isub, 		/* Integer subtract operator */
    imul, 		/* Integer multiply operator */
    idiv,		/* Integer divide operator */
    iop64,		/* 64-bit only integer operator */
    flop, 		/* Any proper floating point op */
    fadd, 		/* FP add operator */
    fsub,		/* FP subtract operator */
    fmul, 		/* FP multiply operator */
    fmisc,              /* FP miscellaneous class type */
    madd,		/* The kind that do two at once */
    mmalu,              /* Multimedia ALU operator */
    mmmul,              /* Multimedia multiply operator */
    mmshf,              /* Multimedia shift operator */
    itrap,		/* Integer trap potential */
    safe,		/* Never traps -- always safe */
    ftrap,		/* Floating point trap potential */
    fdiv,		/* Floating point divides */
    sqrt,		/* Square root operator */
    memtrap,		/* Memory trap potential */
    unsafe,		/* Unsafe always */
    defs_fp,		/* Operator defines FP reg */
    defs_fcc,		/* Operator defines FP CC reg */
    defs_fcr,		/* Operator defines FCR reg */
    refs_fcr,		/* Operator uses FCR reg */
    defs_fpu_int,	/* Operator defs int val in FP reg */
    ior,		/* Integer logical OR operator */
    jump,		/* Jump operator */
    ijump,		/* Indirect jump operator */
    exde_bit, /* extract and depost bits */
    ixor,		/* Integer logical exclusive OR operator */
    iand,		/* Integer logical AND operator */
    icmp,		/* Integer compare operator */
    uicmp,       /* Unsigned compare opertor */
    fcmp,		/* Float compare operator */
    f_group,            /* Instruction must be first in an instruction group */
    l_group,            /* Instruction must be last in an instruction group */
    privileged,         /* Instruction is a privileged instruction */
    simulated,		/* Instruction is simulated, i.e. a macro */
    predicated,		/* Instruction is predicated */
    access_reg_bank,	/* Instruction accesses rotating register banks */
    side_effects,	/* Instruction has side effects */
    branch_predict,	/* Branch prediction (but not actual xfer) */
    mem_fill_type,      /* Memory instructions which are fill/spill type */
    var_opnds,		/* Variable number of operands AND/OR results */
    base_update,	/* Instruction updates 'base' operand */
    loop_start;         /* Instruction marks the start of a zero-cost loop */

  ISA_Properties_Begin ("PPC32");

/* ====================================================================
 *              Operator attributes descriptors
 * ====================================================================
 */

/* ===== Move operator ====== */
  move = ISA_Property_Create ("move");
  Instruction_Group (move,
//					TOP_mc_z_eq,
//					TOP_mc_z_ne,
//					TOP_mc_z_gt,
//					TOP_mc_z_ge,
//					TOP_mc_z_lt,
//					TOP_mc_z_le,
//					TOP_mc_zn_eq,
//					TOP_mc_zn_ne,
//					TOP_mc_zn_gt,
//					TOP_mc_zn_ge,
//					TOP_mc_zn_lt,
//					TOP_mc_zn_le,
//					TOP_mc_t_eq,
//					TOP_mc_t_ne,
//					TOP_mc_t_gt,
//					TOP_mc_t_ge,
//					TOP_mc_t_lt,
//					TOP_mc_t_le,
//					TOP_mc_tn_eq,
//					TOP_mc_tn_ne,
//					TOP_mc_tn_gt,
//					TOP_mc_tn_ge,
//					TOP_mc_tn_lt,
//					TOP_mc_tn_le,
//		      TOP_movf,
//		      TOP_movn,
//		      TOP_movt,
//		      TOP_movz,
//		      TOP_mov_s,
		      TOP_fmr,
//		      TOP_movf_s,
//		      TOP_movf_d,
//		      TOP_movn_s,
//		      TOP_movn_d,
//		      TOP_movt_s,
//		      TOP_movt_d,
//		      TOP_movz_s,
//		      TOP_movz_d,
		      TOP_mflr,
		      TOP_mtlr,
		      TOP_mfctr,
		      TOP_mtctr,
		      TOP_mfcr,
		      TOP_mcrf,
		        
					TOP_mtfsb0,
					TOP_mtfsb1,
          TOP_UNDEFINED);

/* ===== Memory load operator ====== */
  load = ISA_Property_Create ("load");
  Instruction_Group (load,
		      TOP_lbz,
		      TOP_lbzu,
		      TOP_lhz,
		      TOP_lhzu,
		      TOP_lha,
		      TOP_lhau,
		      TOP_lwz,
		      TOP_lwzu,		      
		      
          TOP_lbzx,
        	TOP_lbzux,
        	TOP_lhzx,
        	TOP_lhzux,
        	TOP_lhax,
        	TOP_lhaux,
        	TOP_lwzx,
        	TOP_lwzux,
        	
		      TOP_lfs,
//	        TOP_lfsu,
//	        TOP_lfsux,
	        TOP_lfsx,
	        TOP_lfd,
//	        TOP_lfdu,
//	        TOP_lfdux,
	        TOP_lfdx,
//		      TOP_ll,
//		      TOP_lwu,
//		      TOP_ld,
//		      TOP_lld,
//		      TOP_lwc1,
//		      TOP_ldc1,
//		      TOP_lwxc1,
//		      TOP_ldxc1,
//		      TOP_lwl,
//		      TOP_lwr,
//		      TOP_ldl,
//		      TOP_ldr,
              TOP_UNDEFINED);




/* ===== Memory store operator ====== */
  store = ISA_Property_Create ("store");
  Instruction_Group (store,
		      TOP_stb,
		      TOP_stbu,
		      TOP_sth,
          TOP_sthu,
		      TOP_stw,
		      TOP_stwu,		     
          	
        	TOP_stbx,
        	TOP_stbux,
        	TOP_sthx,
        	TOP_sthux,
        	TOP_stwx,
        	TOP_stwux,
		      
		      TOP_stfs,
//	        TOP_stfsu,
//	        TOP_stfsux,
	        TOP_stfsx,
	        TOP_stfd,
//	        TOP_stfdu,
//	        TOP_stfdux,
	        TOP_stfdx,
//		      TOP_sc,
//		      TOP_sd,
//		      TOP_scd,
//		      TOP_swc1,
//		      TOP_sdc1,
//		      TOP_swxc1,
//		      TOP_sdxc1,
//		      TOP_swl,
//		      TOP_swr,
//		      TOP_sdl,
//		      TOP_sdr,
                     TOP_UNDEFINED);

/* ===== Prefetch operator ====== */
  prefetch = ISA_Property_Create ("prefetch");
  Instruction_Group (prefetch,
//	  	      TOP_pref,
//	  	      TOP_prefx,
                    TOP_UNDEFINED);

/* ===== Memory fill/spill type instructions ====== */
  mem_fill_type = ISA_Property_Create ("mem_fill_type");
  Instruction_Group (mem_fill_type,
     TOP_UNDEFINED);

/* ===== Control transfer operator ====== */
  xfer = ISA_Property_Create ("xfer");
  Instruction_Group (xfer,
		      TOP_ba,
		      TOP_bla,
		      TOP_blrl,
 		      TOP_blr,
 		      TOP_bctr,
 		      TOP_bctrl,
 		      TOP_b,
		      TOP_bl,
            // reletive branch
            	TOP_beq,  
            	TOP_bge,
            	TOP_bgt,
            	TOP_ble,
            	TOP_blt,
            	
            	TOP_bnl,
            	TOP_bne,
            	TOP_bng,
            	
            	TOP_bso,
            	TOP_bns,
            	TOP_bun,
            	TOP_bnu,
            	
            	// absoult branch
            	TOP_beqa,  
            	TOP_bgea,
            	TOP_bgta,
            	TOP_blea,
            	TOP_blta,
            	
            	TOP_bnla,
            	TOP_bnea,
            	TOP_bnga,
            	
            	TOP_bsoa,
            	TOP_bnsa,
            	TOP_buna,
            	TOP_bnua,
            	
            	// branch to lr
            	TOP_beqlr,  
            	TOP_bgelr,
            	TOP_bgtlr,
            	TOP_blelr,
            	TOP_bltlr,
            	
            	TOP_bnllr,
            	TOP_bnelr,
            	TOP_bnglr,
            	
            	TOP_bsolr,
            	TOP_bnslr,
            	TOP_bunlr,
            	TOP_bnulr,
            	
            	// branch to ctr
            	TOP_beqctr,  
            	TOP_bgectr,
            	TOP_bgtctr,
            	TOP_blectr,
            	TOP_bltctr,
            	
            	TOP_bnlctr,
            	TOP_bnectr,
            	TOP_bngctr,
            	
            	TOP_bsoctr,
            	TOP_bnsctr,
            	TOP_bunctr,
            	TOP_bnuctr,
            	
            	// with lr update
            	// reletive branch
            	TOP_beql,  
            	TOP_bgel,
            	TOP_bgtl,
            	TOP_blel,
            	TOP_bltl,
            	
            	TOP_bnll,
            	TOP_bnel,
            	TOP_bngl,
            	
            	TOP_bsol,
            	TOP_bnsl,
            	TOP_bunl,
            	TOP_bnul,
            	
            	// absoult branch
            	TOP_beqla,  
            	TOP_bgela,
            	TOP_bgtla,
            	TOP_blela,
            	TOP_bltla,
            	
            	TOP_bnlla,
            	TOP_bnela,
            	TOP_bngla,
            	
            	TOP_bsola,
            	TOP_bnsla,
            	TOP_bunla,
            	TOP_bnula,
            	
            	// branch to lr
            	TOP_beqlrl,  
            	TOP_bgelrl,
            	TOP_bgtlrl,
            	TOP_blelrl,
            	TOP_bltlrl,
            	
            	TOP_bnllrl,
            	TOP_bnelrl,
            	TOP_bnglrl,
            	
            	TOP_bsolrl,
            	TOP_bnslrl,
            	TOP_bunlrl,
            	TOP_bnulrl,
            	
            	// branch to ctr
            	TOP_beqctrl,  
            	TOP_bgectrl,
            	TOP_bgtctrl,
            	TOP_blectrl,
            	TOP_bltctrl,
            	
            	TOP_bnlctrl,
            	TOP_bnectrl,
            	TOP_bngctrl,
            	
            	TOP_bsoctrl,
            	TOP_bnsctrl,
            	TOP_bunctrl,
            	TOP_bnuctrl,
//		      TOP_bc1f,
//		      TOP_bc1t,

                    TOP_UNDEFINED);

/* ===== Subprogram call operator ====== */
  call = ISA_Property_Create ("call");
  Instruction_Group (call,
		      TOP_blrl,
		      TOP_bl,
		      TOP_bctrl,
          TOP_UNDEFINED);

/* ===== Call/xfer is conditional ====== */
  cond = ISA_Property_Create ("cond");
  Instruction_Group (cond,
        // reletive branch
        	TOP_beq,  
        	TOP_bge,
        	TOP_bgt,
        	TOP_ble,
        	TOP_blt,
        	
        	TOP_bnl,
        	TOP_bne,
        	TOP_bng,
        	
        	TOP_bso,
        	TOP_bns,
        	TOP_bun,
        	TOP_bnu,
        	
        	// absoult branch
        	TOP_beqa,  
        	TOP_bgea,
        	TOP_bgta,
        	TOP_blea,
        	TOP_blta,
        	
        	TOP_bnla,
        	TOP_bnea,
        	TOP_bnga,
        	
        	TOP_bsoa,
        	TOP_bnsa,
        	TOP_buna,
        	TOP_bnua,
        	
        	// branch to lr
        	TOP_beqlr,  
        	TOP_bgelr,
        	TOP_bgtlr,
        	TOP_blelr,
        	TOP_bltlr,
        	
        	TOP_bnllr,
        	TOP_bnelr,
        	TOP_bnglr,
        	
        	TOP_bsolr,
        	TOP_bnslr,
        	TOP_bunlr,
        	TOP_bnulr,
        	
        	// branch to ctr
        	TOP_beqctr,  
        	TOP_bgectr,
        	TOP_bgtctr,
        	TOP_blectr,
        	TOP_bltctr,
        	
        	TOP_bnlctr,
        	TOP_bnectr,
        	TOP_bngctr,
        	
        	TOP_bsoctr,
        	TOP_bnsctr,
        	TOP_bunctr,
        	TOP_bnuctr,
        	
        	// with lr update
        	// reletive branch
        	TOP_beql,  
        	TOP_bgel,
        	TOP_bgtl,
        	TOP_blel,
        	TOP_bltl,
        	
        	TOP_bnll,
        	TOP_bnel,
        	TOP_bngl,
        	
        	TOP_bsol,
        	TOP_bnsl,
        	TOP_bunl,
        	TOP_bnul,
        	
        	// absoult branch
        	TOP_beqla,  
        	TOP_bgela,
        	TOP_bgtla,
        	TOP_blela,
        	TOP_bltla,
        	
        	TOP_bnlla,
        	TOP_bnela,
        	TOP_bngla,
        	
        	TOP_bsola,
        	TOP_bnsla,
        	TOP_bunla,
        	TOP_bnula,
        	
        	// branch to lr
        	TOP_beqlrl,  
        	TOP_bgelrl,
        	TOP_bgtlrl,
        	TOP_blelrl,
        	TOP_bltlrl,
        	
        	TOP_bnllrl,
        	TOP_bnelrl,
        	TOP_bnglrl,
        	
        	TOP_bsolrl,
        	TOP_bnslrl,
        	TOP_bunlrl,
        	TOP_bnulrl,
        	
        	// branch to ctr
        	TOP_beqctrl,  
        	TOP_bgectrl,
        	TOP_bgtctrl,
        	TOP_blectrl,
        	TOP_bltctrl,
        	
        	TOP_bnlctrl,
        	TOP_bnectrl,
        	TOP_bngctrl,
        	
        	TOP_bsoctrl,
        	TOP_bnsctrl,
        	TOP_bunctrl,
        	TOP_bnuctrl,
//		      TOP_bc1f,
//		      TOP_bc1t,
                     TOP_UNDEFINED);

/* ===== Cond call/xfer is likely ====== */
  likely = ISA_Property_Create ("likely");
  Instruction_Group (likely,
                     TOP_UNDEFINED);

/* ===== Result def is conditional ====== */
  cond_move = ISA_Property_Create ("cond_move");
  Instruction_Group (cond_move,
//            TOP_mfcr,
//					TOP_mc_z_eq,
//					TOP_mc_z_ne,
//					TOP_mc_z_gt,
//					TOP_mc_z_ge,
//					TOP_mc_z_lt,
//					TOP_mc_z_le,
//					TOP_mc_zn_eq,
//					TOP_mc_zn_ne,
//					TOP_mc_zn_gt,
//					TOP_mc_zn_ge,
//					TOP_mc_zn_lt,
//					TOP_mc_zn_le,
//					TOP_mc_t_eq,
//					TOP_mc_t_ne,
//					TOP_mc_t_gt,
//					TOP_mc_t_ge,
//					TOP_mc_t_lt,
//					TOP_mc_t_le,
//					TOP_mc_tn_eq,
//					TOP_mc_tn_ne,
//					TOP_mc_tn_gt,
//					TOP_mc_tn_ge,
//					TOP_mc_tn_lt,
//					TOP_mc_tn_le,  
//		      TOP_movf,
//		      TOP_movn,
//		      TOP_movt,
//		      TOP_movz,
//		      TOP_movf_s,
//		      TOP_movf_d,
//		      TOP_movn_s,
//		      TOP_movn_d,
//		      TOP_movt_s,
//		      TOP_movt_d,
//		      TOP_movz_s,
//		      TOP_movz_d,
                     TOP_UNDEFINED);

/* ===== Result must not be opnd ====== */
  uniq_res = ISA_Property_Create ("uniq_res");
  Instruction_Group (uniq_res,
                     TOP_UNDEFINED);

/* ===== Result must be same as opnd ====== */
  same_res = ISA_Property_Create ("same_res");
  Instruction_Group (same_res,
		     /* Because asm macros can share the same operand between
		        inputs and outputs, we conservatively mark this 
			property so that the compiler doesn't try to separate
			any common operands. */
                     TOP_asm,
                     TOP_rlwimi,
                     TOP_UNDEFINED);

/* ===== Operator is a machine level select ====== */
  select = ISA_Property_Create ("select");
  Instruction_Group (select,
                     TOP_fsel,
                     TOP_UNDEFINED);

/* ===== Unaligned load operator ====== */
  unalign_ld = ISA_Property_Create ("unalign_ld");
  Instruction_Group (unalign_ld,
//		      TOP_lwl,
//		      TOP_lwr,
//		      TOP_ldl,
//		      TOP_ldr,
                     TOP_UNDEFINED);

/* ===== Unaligned store operator ====== */
  unalign_store = ISA_Property_Create ("unalign_store");
  Instruction_Group (unalign_store,
//		      TOP_swl,
//		      TOP_swr,
//		      TOP_sdl,
//		      TOP_sdr,
		     TOP_UNDEFINED);

/* ===== Unknown addr operator ====== */
  unknown_addr = ISA_Property_Create ("unknown_memdata");
  Instruction_Group (unknown_memdata,
	     TOP_UNDEFINED);

/* ===== Unknown addr operator ====== */
  unknown_addr = ISA_Property_Create ("unknown_addr");
  Instruction_Group (unknown_addr,
		     TOP_UNDEFINED);

/* ===== Integer add operator ====== */
  iadd = ISA_Property_Create ("iadd");
  Instruction_Group (iadd,
		      TOP_add,
		      TOP_addi,
          TOP_addis,
		      TOP_add_,
		      TOP_addic,
		      TOP_addc,
					TOP_addco,
					TOP_adde,
					TOP_addeo,
					TOP_addme,
					TOP_addmeo,
					TOP_addze,
					TOP_addzeo,

//		      TOP_addiu,
//		      TOP_addu,
//		      TOP_dadd,
//		      TOP_daddi,
//		      TOP_daddiu,
//		      TOP_daddu,
                     TOP_UNDEFINED);

/* ===== Integer subtract operator ====== */
  isub = ISA_Property_Create ("isub");
  Instruction_Group (isub,
           TOP_subf,
           TOP_subfc,
           TOP_subfe,
           TOP_subfic,
           TOP_subfco,
					TOP_subfeo,
					TOP_subfme,
					TOP_subfmeo,
					TOP_subfze,
					TOP_subfzeo,
//		     TOP_subu,
//		     TOP_dsub,
//		     TOP_dsubu,
                     TOP_UNDEFINED);

/* ===== Integer multiply operator ====== */
  imul = ISA_Property_Create ("imul");
  Instruction_Group (imul,
	  	     TOP_mullw,
	  	     TOP_mulli,
             TOP_mullwo,
             TOP_mulhwu,
//	  	     TOP_mullwu,
//	  	     TOP_dmult,
//	  	     TOP_dmultu,
                     TOP_UNDEFINED);

/* ===== Integer divide operator ====== */
  idiv = ISA_Property_Create ("idiv");
  Instruction_Group (idiv,
	  	     TOP_divw,
	  	     TOP_divwo,
             TOP_divwu,
             TOP_divwuo, 
             TOP_UNDEFINED);

/* ===== 64-bit-only integer operator ====== */
  iop64 = ISA_Property_Create ("iop64");
  Instruction_Group (iop64,
//		     TOP_dadd,
//		     TOP_daddi,
//		     TOP_daddiu,
//		     TOP_daddu,
//		     TOP_dsub,
//		     TOP_dsubu,
//	  	     TOP_dmult,
//	  	     TOP_dmultu,
//	  	     TOP_ddiv,
//	  	     TOP_ddivu,
//		     TOP_dsll,
//		     TOP_dsll32,
//		     TOP_dsllv,
//		     TOP_dsra,
//		     TOP_dsra32,
//		     TOP_dsrav,
//		     TOP_dsrl,
//		     TOP_dsrl32,
//		     TOP_dsrlv,
//		     TOP_dmfc1,
//		     TOP_dmtc1,
                     TOP_UNDEFINED);

/* ===== Any proper floating point op ====== */
  flop = ISA_Property_Create ("flop");
  Instruction_Group (flop,
//		      TOP_abs_s,
		      TOP_fabs,
		      TOP_fnabs,
//		      TOP_add_s,
          TOP_fadds,
		      TOP_fadd,
//		      TOP_c_f_s,
//		      TOP_c_f_d,
//		      TOP_c_t_s,
//		      TOP_c_t_d,
//		      TOP_c_un_s,
//		      TOP_c_un_d,
//		      TOP_c_or_s,
//		      TOP_c_or_d,
//		      TOP_c_eq_s,
//		      TOP_c_eq_d,
//		      TOP_c_neq_s,
//		      TOP_c_neq_d,
//		      TOP_c_ueq_s,
//		      TOP_c_ueq_d,
//		      TOP_c_olg_s,
//		      TOP_c_olg_d,
//		      TOP_c_olt_s,
//		      TOP_c_olt_d,
//		      TOP_c_uge_s,
//		      TOP_c_uge_d,
//		      TOP_c_ult_s,
//		      TOP_c_ult_d,
//		      TOP_c_oge_s,
//		      TOP_c_oge_d,
//		      TOP_c_ole_s,
//		      TOP_c_ole_d,
//		      TOP_c_ugt_s,
//		      TOP_c_ugt_d,
//		      TOP_c_ule_s,
//		      TOP_c_ule_d,
//		      TOP_c_ogt_s,
//		      TOP_c_ogt_d,
//		      TOP_c_sf_s,
//		      TOP_c_sf_d,
//		      TOP_c_st_s,
//		      TOP_c_st_d,
//		      TOP_c_ngle_s,
//		      TOP_c_ngle_d,
//		      TOP_c_gle_s,
//		      TOP_c_gle_d,
//		      TOP_c_seq_s,
//		      TOP_c_seq_d,
//		      TOP_c_sne_s,
//		      TOP_c_sne_d,
//		      TOP_c_ngl_s,
//		      TOP_c_ngl_d,
//		      TOP_c_gl_s,
//		      TOP_c_gl_d,
//		      TOP_c_lt_s,
//		      TOP_c_lt_d,
//		      TOP_c_nlt_s,
//		      TOP_c_nlt_d,
//		      TOP_c_nge_s,
//		      TOP_c_nge_d,
//		      TOP_c_ge_s,
//		      TOP_c_ge_d,
//		      TOP_c_le_s,
//		      TOP_c_le_d,
//		      TOP_c_nle_s,
//		      TOP_c_nle_d,
//		      TOP_c_ngt_s,
//		      TOP_c_ngt_d,
//		      TOP_c_gt_s,
//		      TOP_c_gt_d,
//		      TOP_divwxwxwx_s,
          TOP_fdivs,
		      TOP_fdiv,
//		      TOP_mul_s,
          TOP_fmuls,
		      TOP_fmul,
//		      TOP_neg_s,
		      TOP_fneg,
//		      TOP_sub_s,
          TOP_fsubs,
		      TOP_fsub,
		      TOP_fsqrts,
		      TOP_fsqrt,
		      TOP_fmadds,
		      TOP_fmadd,
		      TOP_fmsubs,
		      TOP_fmsub,
		      TOP_fnmadds,
		      TOP_fnmadd,
		      TOP_fmsubs,
		      TOP_fmsub,
		      TOP_fres,
//		      TOP_recip_d,
		      TOP_frsqrte,
		      TOP_fcmpu,
		      TOP_fcmpo,
		      TOP_fsel,
                     TOP_UNDEFINED);

/* ===== FP add operator ====== */
  fadd = ISA_Property_Create ("fadd");
  Instruction_Group (fadd,
		      TOP_fadds,
		      TOP_fadd,
                     TOP_UNDEFINED);

/* ===== FP subtract operator ====== */
  fsub = ISA_Property_Create ("fsub");
  Instruction_Group (fsub,
		      TOP_fsubs,
		      TOP_fsub,
                     TOP_UNDEFINED);

/* ===== FP multiply operator ====== */
  fmul = ISA_Property_Create ("fmul");
  Instruction_Group (fmul,
		      TOP_fmuls,
		      TOP_fmul,
                     TOP_UNDEFINED);

/* ===== FP miscellaneous operator ====== */
  fmisc = ISA_Property_Create ("fmisc");
  Instruction_Group (fmisc,
//		      TOP_abs_s,
		      TOP_fabs,
		      TOP_fres,
//		      TOP_recip_d,
		      TOP_fsqrts,
		      TOP_fsqrt,
//		      TOP_neg_s,
		      TOP_fneg,
		      TOP_frsqrte,
		      TOP_fnabs,
	     TOP_UNDEFINED);

/* ===== The kind that do two at once ====== */
  // Used in isa/expand.cxx only and not for any purpose we could use.
  madd = ISA_Property_Create ("madd");
  Instruction_Group (madd,
		      TOP_fmadds,
		      TOP_fmadd,
		      TOP_fmsubs,
		      TOP_fmsub,
		      TOP_fnmadds,
		      TOP_fnmadd,
		      TOP_fnmsubs,
		      TOP_fnmsub,
                    TOP_UNDEFINED);

/* ===== Instructions belonging to Multimedia ALU type ====== */
  mmalu = ISA_Property_Create ("mmalu");
  Instruction_Group (mmalu,
		     TOP_UNDEFINED);

/* ===== Instructions belonging to Multimedia shift (MMSHF) type ====== */
  mmshf = ISA_Property_Create ("mmshf");
  Instruction_Group (mmshf,
		     TOP_UNDEFINED);

/* ===== Instructions belonging to Multimedia multiply (MMMUL) type ====== */
  mmmul = ISA_Property_Create ("mmmul");
  Instruction_Group (mmmul,
		     TOP_UNDEFINED);

  noop = ISA_Property_Create ("noop");
  Instruction_Group (noop,
                     TOP_nop,
                     TOP_UNDEFINED);

  dummy = ISA_Property_Create ("dummy");
  Instruction_Group (dummy,
//		     TOP_begin_pregtn,
//		     TOP_end_pregtn,
//		     TOP_fwd_bar,
//		     TOP_bwd_bar,
//		     TOP_label,
//                     TOP_noop,
	     TOP_UNDEFINED);

/* ====================================================================
 *              Exception classification descriptors
 * ====================================================================
 */

  /* ===== Integer trap potential ====== */
  itrap = ISA_Property_Create ("itrap");
  Instruction_Group (itrap,
		     TOP_tweq,
		     TOP_tweqi,
		     TOP_twge,
		     TOP_twgei,
//		     TOP_tgeiu,
//		     TOP_tgeu,
		     TOP_twlt,
		     TOP_twlti,
//		     TOP_tltiu,
//		     TOP_tltu,
		     TOP_twne,
		     TOP_twnei,
                     TOP_UNDEFINED);

  /* ===== Never traps -- always safe ====== */
  safe = ISA_Property_Create ("safe");
  Instruction_Group (safe,
			// TODO
                     TOP_UNDEFINED);

  /* ===== Unsafe always ====== */
  unsafe = ISA_Property_Create ("unsafe");
  Instruction_Group (unsafe,
//                     TOP_fwd_bar, TOP_bwd_bar,
                     TOP_UNDEFINED);

  /* ===== Floating point trap potential ====== */
  ftrap = ISA_Property_Create ("ftrap");
  Instruction_Group (ftrap,
                     TOP_UNDEFINED);

  /* ===== Floating point divides ====== */
  fdiv = ISA_Property_Create ("fdiv");
  Instruction_Group (fdiv,
//		     TOP_recip_d,
//		     TOP_divwxwxwx_s,
         TOP_fdivs,
		     TOP_fdiv,
                     TOP_UNDEFINED);

  /* ===== Square roots ====== */
  sqrt = ISA_Property_Create ("sqrt");
  Instruction_Group (sqrt,
		      TOP_fsqrts,
		      TOP_fsqrt,
                     TOP_UNDEFINED);

  /* ===== Memory trap potential ====== */
  memtrap = ISA_Property_Create ("memtrap");
  Instruction_Group (memtrap,
		      TOP_lbz,
		      TOP_lbzu,
		      TOP_lhz,
		      TOP_lhzu,
		      TOP_lha,
		      TOP_lhau,
		      TOP_lwz,
		      TOP_lwzu,
		      TOP_lfs,
//	        TOP_lfsu,
//	        TOP_lfsux,
	        TOP_lfsx,
	        TOP_lfd,
//	        TOP_lfdu,
//	        TOP_lfdux,
	        TOP_lfdx,
//		      TOP_ll,
//		      TOP_lwu,
//		      TOP_ld,
//		      TOP_lld,
//		      TOP_lwc1,
//		      TOP_ldc1,
//		      TOP_lwxc1,
//		      TOP_ldxc1,
//		      TOP_lwl,
//		      TOP_lwr,
//		      TOP_ldl,
//		      TOP_ldr,
		      TOP_subf,
		      TOP_sth,
		      TOP_stw,
		      TOP_stfs,
//	        TOP_stfsu,
//	        TOP_stfsux,
	        TOP_stfsx,
	        TOP_stfd,
//	        TOP_stfdu,
//	        TOP_stfdux,
	        TOP_stfdx,
                     TOP_UNDEFINED);

  /* ===== Instruction must be first in an instruction group ====== */
  f_group = ISA_Property_Create ("f_group");
  Instruction_Group (f_group,
		     TOP_UNDEFINED);

  /* ===== Instruction must be last in an instruction group ====== */
  l_group = ISA_Property_Create ("l_group");
  Instruction_Group (l_group,
		     TOP_UNDEFINED);

  /* ===== Instruction is a privileged instruction ====== */
  privileged = ISA_Property_Create ("privileged");
  Instruction_Group (privileged,
		     TOP_UNDEFINED);

/* ====================================================================
 * Other operator descriptors (mostly for global optimization).
 * TODO: These descriptors should actually be determined from mips_operands.
 * ====================================================================
 */

/* ===== Operator defines FP CC reg ====== */
  defs_fcc = ISA_Property_Create ("defs_fcc");
  Instruction_Group (defs_fcc,
            TOP_fcmpu,
	     TOP_fcmpo,
            TOP_UNDEFINED);

/* ===== Operator defines FCR reg ====== */
  defs_fcr = ISA_Property_Create ("defs_fcr");
  Instruction_Group (defs_fcr,
            TOP_fcmpu,
	     TOP_fcmpo,
	     TOP_mtfsf,
	     TOP_mtfsb0,
	     TOP_mtfsb1,
            TOP_UNDEFINED);

/* ===== Operator uses FCR reg ====== */
  refs_fcr = ISA_Property_Create ("refs_fcr");
  Instruction_Group (refs_fcr,
	  	      TOP_mcrfs,
                    TOP_mffs,
                    TOP_UNDEFINED);

/* ===== Operator defs int val in FP reg ====== */
  defs_fpu_int = ISA_Property_Create ("defs_fpu_int");
  Instruction_Group (defs_fpu_int,
	  		// TODO
                     TOP_UNDEFINED);

/* ===== Operator defines FP reg ====== */
  defs_fp = ISA_Property_Create ("defs_fp");
  Instruction_Group (defs_fp,
	  		// TODO
                     TOP_UNDEFINED);

/* ===== Logical OR operator ====== */
  ior = ISA_Property_Create ("ior");
  Instruction_Group (ior,
                     TOP_or,
                     TOP_ori,
                     TOP_UNDEFINED);

/* ===== Jump operator ====== */
  jump = ISA_Property_Create ("jump");
  Instruction_Group (jump,
                     TOP_ba,
                     TOP_UNDEFINED);

/* ===== Indirect jump operator ====== */
  ijump = ISA_Property_Create ("ijump");
  Instruction_Group (ijump,
                     TOP_blr,
                     TOP_bctr,
                     TOP_blrl,
                     TOP_bctrl,
                     TOP_UNDEFINED);

/* ===== Extract and Depost bits operator ====== */
  exde_bit = ISA_Property_Create ("exde_bit");
  Instruction_Group (exde_bit,
//										TOP_depb,
//										TOP_extrbs,
//										TOP_extrbu,
                     TOP_UNDEFINED);

/* ===== Logical exclusive OR operator ====== */
  ixor = ISA_Property_Create ("ixor");
  Instruction_Group (ixor,
                     TOP_xor,
                     TOP_xori,
                     TOP_UNDEFINED);

/* ===== Logical AND operator ====== */
  iand = ISA_Property_Create ("iand");
  Instruction_Group (iand,
                     TOP_and,
                     TOP_andi_,
                     TOP_andis_,
                     TOP_UNDEFINED);

/* ===== Integer compare operator ====== */
  icmp = ISA_Property_Create ("icmp");
  Instruction_Group (icmp,
	  	     TOP_cmpw,
	  	     TOP_cmpwi,
	  	     TOP_cmplw,
	  	     TOP_cmplwi,
                     TOP_UNDEFINED);
/* ===== Unsigned integer compare operator ====== */
  uicmp = ISA_Property_Create ("uicmp");
  Instruction_Group (uicmp,
	  	     TOP_cmplw,
	  	     TOP_cmplwi,
                     TOP_UNDEFINED);
  
/* ===== Float compare operator ======*/
  fcmp = ISA_Property_Create ("fcmp");
  Instruction_Group (fcmp,
	  	     TOP_fcmpu,
	  	     TOP_fcmpo,
                     TOP_UNDEFINED);

/* ===== Simulated instructions ====== */
  simulated = ISA_Property_Create ("simulated");
  Instruction_Group (simulated,
                     TOP_asm,
                     TOP_spadjust,
                     TOP_intrncall,
                     TOP_simaddi,
                     TOP_UNDEFINED);

/* ===== Predicated instructions ====== */
  predicated = ISA_Property_Create ("predicated");
  Instruction_Group (predicated,
                     TOP_UNDEFINED);

/* ===== Instructions access rotating register banks ====== */
  access_reg_bank = ISA_Property_Create ("access_reg_bank");
  Instruction_Group (access_reg_bank,
		     TOP_UNDEFINED);

/* ===== Instructions with side effects ====== */
  side_effects = ISA_Property_Create ("side_effects");
  Instruction_Group (side_effects,
		     /* Because asm macros can trash memory, we conservatively 
			mark this property so that the compiler doesn't move
			instructions around it. */
                     TOP_asm, 
//		     TOP_sync, 
		     TOP_UNDEFINED);

/* ===== Instructions with branch predictions ====== */
  side_effects = ISA_Property_Create ("branch_predict");
  Instruction_Group (branch_predict,
		     TOP_UNDEFINED);

/* ===== Instructions with variable number of operands/results ====== */
  var_opnds = ISA_Property_Create ("var_opnds");
  Instruction_Group (var_opnds,
  		     TOP_asm,
		     TOP_UNDEFINED);

/* ===== Instructions that update 'base' operand ====== */
  base_update = ISA_Property_Create ("base_update");
  Instruction_Group (base_update,
		     TOP_UNDEFINED);

/* ===== Instructions that mark the head of a zero-cost loop ====== */
  loop_start = ISA_Property_Create ("loop_start");
  Instruction_Group (loop_start,
		     TOP_UNDEFINED);

  ISA_Properties_End();
  return 0;
}
