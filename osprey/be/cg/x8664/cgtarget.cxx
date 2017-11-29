/*
 * Copyright (C) 2011 Hewlett Packard, Company.  All Rights Reserved.
 */
/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007, 2008 PathScale, LLC.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: cgtarget.cxx
 * $Revision: 1.140 $
 * $Date: 05/09/01 11:48:02-07:00 $
 * $Author: tkong@hyalite.keyresearch $
 * $Source: be/cg/x8664/SCCS/s.cgtarget.cxx $
 *
 * Description:
 *
 * Support routines for target-specific code generator functionality.
 *
 * ====================================================================
 * ====================================================================
 */

#include <ctype.h>

#include "defs.h"
#include "util.h"
#include "config.h"
#include "config_targ_opt.h"
#include "erglob.h"
#include "tracing.h"
#include "data_layout.h"
#include "const.h"
#include "wn.h"
#include "opt_alias_interface.h"
#include "opt_alias_mgr.h"
#include "cgir.h"
#include "cg.h"
#include "void_list.h"
#include "cg_dep_graph.h"
#include "cg_spill.h"
#include "cg_vector.h"
#include "whirl2ops.h"
#include "ti_errors.h"
#include "ti_latency.h"
#include "w2op.h"
#include "cgexp.h"
#include "cg_loop_recur.h"
#include "targ_proc_properties.h"
#include "ti_bundle.h"
#include "hb_sched.h"
#include "hb_hazards.h"
#include "bb.h"
#include "op.h"
#include "op_list.h"
#include "cg_grouping.h"
#include "calls.h"
#include "cgtarget.h"
#include "calls.h"
#include "cg_loop.h"
#include "config_lno.h"  // for LNO_Prefetch_Ahead
#include "erbe.h"
#include "stblock.h" //for Base_Symbol_And_Offset_For_Addressing
#include "be_symtab.h" //Preg_Lda

#define UNCONDITIONAL_MOVNTI -1

UINT32 CGTARG_branch_taken_penalty;
BOOL CGTARG_branch_taken_penalty_overridden = FALSE;

TOP CGTARG_Invert_Table[TOP_count+1];
TOP CGTARG_Immed_To_Reg_Table[TOP_count+1];

OPCODE CGTARG_Assoc_Base_Opr_Table[TOP_count];
mTOP CGTARG_Assoc_Base_Top_Table[TOP_count];
mTOP CGTARG_Assoc_Base_Fnc_Table[TOP_count];

mTOP CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_MAX+1][ISA_REGISTER_CLASS_MAX+1][2];

/* Trace flags: */
BOOL Trace_TD = FALSE;	/* Target-dependent prep trace */
BOOL Trace_Eager = FALSE; /* gcm used to set this... */
extern BOOL Trace_Call_Exp;	/* Trace call expansion, from cgexp */


UINT32 CGTARG_Mem_Ref_Bytes(const OP *memop)
/* -----------------------------------------------------------------------
 * Requires: OP_load(memop) || OP_store(memop)
 * See interface description.
 * -----------------------------------------------------------------------
 */
{
  const TOP topcode = OP_code(memop);

  if (TOP_is_vector_op(topcode)) {
    switch (topcode) {
      case TOP_stlps:
      case TOP_stlpsx:
      case TOP_stlpsxx:
      case TOP_stlps_n32:
      case TOP_stlpd:
      case TOP_stlpdx:
      case TOP_stlpdxx:
      case TOP_stlpd_n32:
      case TOP_ldlps:
      case TOP_ldlpsx:
      case TOP_ldlpsxx:
      case TOP_ldlps_n32:
      case TOP_ldlpd:
      case TOP_ldlpdx:
      case TOP_ldlpdxx:
      case TOP_ldlpd_n32:
      case TOP_sthps:
      case TOP_sthpsx:
      case TOP_sthpsxx:
      case TOP_sthpd:
      case TOP_sthpdx:
      case TOP_sthpdxx:
      case TOP_sthpd_n32:
      case TOP_ldhps:
      case TOP_ldhpsx:
      case TOP_ldhpsxx:
      case TOP_ldhpd:
      case TOP_ldhpdx:
      case TOP_ldhpdxx:
      case TOP_ldhpd_n32:
      // only source operand of cvtps2pd can be memory, so the ref is 8 bytes.
      case TOP_cvtps2pd:
      case TOP_cvtps2pd_x:
      case TOP_cvtps2pd_xx:
      case TOP_cvtps2pd_xxx:
        return 8;
      default:
        return 16;
    }
  }

  // For SSE OPs, get the number of memory bytes from the OP code instead of
  // from the size of the SSE register used.  This is because isa_operands.cxx
  // says 32-bit SSE OPs use fp64 registers even though only 4 bytes are
  // referenced.
  if (OP_flop(memop) &&
      !TOP_is_x87(topcode)) {	// not x87
    switch (topcode) {
      case TOP_ldss:		// 32 bit
      case TOP_ldss_n32:
      case TOP_ldssx:
      case TOP_ldssxx:
      case TOP_stssxx:
      case TOP_stss:
      case TOP_stss_n32:
      case TOP_stssx:
      case TOP_stntss:
      case TOP_stntssx:
      case TOP_stntssxx:
      case TOP_divxxxss:
      case TOP_addxxxss:
      case TOP_subxxxss:
      case TOP_mulxxxss:
      case TOP_comixss:
      case TOP_comixxss:
      case TOP_comixxxss:
      case TOP_divxss:
      case TOP_divxxss:
      case TOP_addxss:
      case TOP_subxss:
      case TOP_mulxss:
      case TOP_addxxss:
      case TOP_subxxss:
      case TOP_mulxxss:
      case TOP_cvtsi2ss_x:
      case TOP_cvtsi2ss_xx:
      case TOP_cvtsi2ss_xxx:
      case TOP_vldss:		// 32 bit
      case TOP_vldss_n32:
      case TOP_vldssx:
      case TOP_vldssxx:
      case TOP_vstssxx:
      case TOP_vstss:
      case TOP_vstss_n32:
      case TOP_vstssx:
      case TOP_vstntss:
      case TOP_vstntssx:
      case TOP_vstntssxx:
      case TOP_vdivxxxss:
      case TOP_vfaddxxxss:
      case TOP_vsubxxxss:
      case TOP_vmulxxxss:
      case TOP_vcomixss:
      case TOP_vcomixxss:
      case TOP_vcomixxxss:
      case TOP_vdivxss:
      case TOP_vdivxxss:
      case TOP_vfaddxss:
      case TOP_vsubxss:
      case TOP_vmulxss:
      case TOP_vfaddxxss:
      case TOP_vsubxxss:
      case TOP_vmulxxss:
      case TOP_vcvtsi2ssx:
      case TOP_vcvtsi2ssxx:
      case TOP_vcvtsi2ssxxx:
      // FMA4
      case TOP_vfmaddxss:
      case TOP_vfmaddxxss:
      case TOP_vfmaddxxxss:
      case TOP_vfmaddxrss:
      case TOP_vfmaddxxrss:
      case TOP_vfmaddxxxrss:
      case TOP_vfnmaddxss:
      case TOP_vfnmaddxxss:
      case TOP_vfnmaddxxxss:
      case TOP_vfnmaddxrss:
      case TOP_vfnmaddxxrss:
      case TOP_vfnmaddxxxrss:
      case TOP_vfmsubxss:
      case TOP_vfmsubxxss:
      case TOP_vfmsubxxxss:
      case TOP_vfmsubxrss:
      case TOP_vfmsubxxrss:
      case TOP_vfmsubxxxrss:
      case TOP_vfnmsubxss:
      case TOP_vfnmsubxxss:
      case TOP_vfnmsubxxxss:
      case TOP_vfnmsubxrss:
      case TOP_vfnmsubxxrss:
      case TOP_vfnmsubxxxrss:
      // FMA3: form1
      case TOP_xfmadd132xss:
      case TOP_xfmadd132xxss:
      case TOP_xfmadd132xxxss:
      case TOP_xfnmadd132xss:
      case TOP_xfnmadd132xxss:
      case TOP_xfnmadd132xxxss:
      case TOP_xfmsub132xss:
      case TOP_xfmsub132xxss:
      case TOP_xfmsub132xxxss:
      case TOP_xfnmsub132xss:
      case TOP_xfnmsub132xxss:
      case TOP_xfnmsub132xxxss:
      // FMA3: form2
      case TOP_xfmadd213xss:
      case TOP_xfmadd213xxss:
      case TOP_xfmadd213xxxss:
      case TOP_xfnmadd213xss:
      case TOP_xfnmadd213xxss:
      case TOP_xfnmadd213xxxss:
      case TOP_xfmsub213xss:
      case TOP_xfmsub213xxss:
      case TOP_xfmsub213xxxss:
      case TOP_xfnmsub213xss:
      case TOP_xfnmsub213xxss:
      case TOP_xfnmsub213xxxss:
      // FMA3: form3
      case TOP_xfmadd231xss:
      case TOP_xfmadd231xxss:
      case TOP_xfmadd231xxxss:
      case TOP_xfnmadd231xss:
      case TOP_xfnmadd231xxss:
      case TOP_xfnmadd231xxxss:
      case TOP_xfmsub231xss:
      case TOP_xfmsub231xxss:
      case TOP_xfmsub231xxxss:
      case TOP_xfnmsub231xss:
      case TOP_xfnmsub231xxss:
      case TOP_xfnmsub231xxxss:
      case TOP_vfsqrtxss:
      case TOP_vfsqrtxxss:
      case TOP_vfsqrtxxxss:
      case TOP_vfrsqrtxss:
      case TOP_vfrsqrtxxss:
      case TOP_vfrsqrtxxxss:
	return 4;

      case TOP_ldsd:		// 64 bit
      case TOP_ldsd_n32:
      case TOP_ldsdx:
      case TOP_ldsdxx:
      case TOP_stsdxx:
      case TOP_stsd:
      case TOP_stsd_n32:
      case TOP_stsdx:
      case TOP_stntsd:
      case TOP_stntsdx:
      case TOP_stntsdxx:
      case TOP_storelpd:
      case TOP_divxxxsd:
      case TOP_addxxxsd:
      case TOP_subxxxsd:
      case TOP_mulxxxsd:
      case TOP_comixsd:
      case TOP_comixxsd:
      case TOP_comixxxsd:
      case TOP_divxsd:
      case TOP_divxxsd:
      case TOP_addxsd:
      case TOP_subxsd:
      case TOP_mulxsd:
      case TOP_addxxsd:
      case TOP_subxxsd:
      case TOP_mulxxsd:
      case TOP_cvtsd2ss_x:
      case TOP_cvtsd2ss_xx:
      case TOP_cvtsd2ss_xxx:
      case TOP_cvtsi2sd_x:
      case TOP_cvtsi2sd_xx:
      case TOP_cvtsi2sd_xxx:
      case TOP_cvtsi2sdq_x:
      case TOP_cvtsi2sdq_xx:
      case TOP_cvtsi2sdq_xxx:
      case TOP_cvtsi2ssq_x:
      case TOP_cvtsi2ssq_xx:
      case TOP_cvtsi2ssq_xxx:
      case TOP_vldsd:		// 64 bit
      case TOP_vldsd_n32:
      case TOP_vldsdx:
      case TOP_vldsdxx:
      case TOP_vstsdxx:
      case TOP_vsthpd:
      case TOP_vsthpdx:
      case TOP_vsthpdxx:
      case TOP_vsthps:
      case TOP_vsthpsx:
      case TOP_vsthpsxx:
      case TOP_vstlpd:
      case TOP_vstlpdx:
      case TOP_vstlpdxx:
      case TOP_vstlps:
      case TOP_vstlpsx:
      case TOP_vstlpsxx:
      case TOP_vstsd:
      case TOP_vstsd_n32:
      case TOP_vstsdx:
      case TOP_vstorelpd:
      case TOP_vdivxxxsd:
      case TOP_vfaddxxxsd:
      case TOP_vsubxxxsd:
      case TOP_vmulxxxsd:
      case TOP_vcomixsd:
      case TOP_vcomixxsd:
      case TOP_vcomixxxsd:
      case TOP_vdivxsd:
      case TOP_vdivxxsd:
      case TOP_vfaddxsd:
      case TOP_vsubxsd:
      case TOP_vmulxsd:
      case TOP_vfaddxxsd:
      case TOP_vsubxxsd:
      case TOP_vmulxxsd:
      case TOP_vcvtsd2ssx:
      case TOP_vcvtsd2ssxx:
      case TOP_vcvtsd2ssxxx:
      case TOP_vcvtsi2sdx:
      case TOP_vcvtsi2sdxx:
      case TOP_vcvtsi2sdxxx:
      case TOP_vcvtsi2sdqx:
      case TOP_vcvtsi2sdqxx:
      case TOP_vcvtsi2sdqxxx:
      case TOP_vcvtsi2ssqx:
      case TOP_vcvtsi2ssqxx:
      case TOP_vcvtsi2ssqxxx:
      // FMA4
      case TOP_vfmaddxsd:
      case TOP_vfmaddxxsd:
      case TOP_vfmaddxxxsd:
      case TOP_vfmaddxrsd:
      case TOP_vfmaddxxrsd:
      case TOP_vfmaddxxxrsd:
      case TOP_vfnmaddxsd:
      case TOP_vfnmaddxxsd:
      case TOP_vfnmaddxxxsd:
      case TOP_vfnmaddxrsd:
      case TOP_vfnmaddxxrsd:
      case TOP_vfnmaddxxxrsd:
      case TOP_vfmsubxsd:
      case TOP_vfmsubxxsd:
      case TOP_vfmsubxxxsd:
      case TOP_vfmsubxrsd:
      case TOP_vfmsubxxrsd:
      case TOP_vfmsubxxxrsd:
      case TOP_vfnmsubxsd:
      case TOP_vfnmsubxxsd:
      case TOP_vfnmsubxxxsd:
      case TOP_vfnmsubxrsd:
      case TOP_vfnmsubxxrsd:
      case TOP_vfnmsubxxxrsd:
      // FMA3: form1
      case TOP_xfmadd132xsd:
      case TOP_xfmadd132xxsd:
      case TOP_xfmadd132xxxsd:
      case TOP_xfnmadd132xsd:
      case TOP_xfnmadd132xxsd:
      case TOP_xfnmadd132xxxsd:
      case TOP_xfmsub132xsd:
      case TOP_xfmsub132xxsd:
      case TOP_xfmsub132xxxsd:
      case TOP_xfnmsub132xsd:
      case TOP_xfnmsub132xxsd:
      case TOP_xfnmsub132xxxsd:
      // FMA3: form2
      case TOP_xfmadd213xsd:
      case TOP_xfmadd213xxsd:
      case TOP_xfmadd213xxxsd:
      case TOP_xfnmadd213xsd:
      case TOP_xfnmadd213xxsd:
      case TOP_xfnmadd213xxxsd:
      case TOP_xfmsub213xsd:
      case TOP_xfmsub213xxsd:
      case TOP_xfmsub213xxxsd:
      case TOP_xfnmsub213xsd:
      case TOP_xfnmsub213xxsd:
      case TOP_xfnmsub213xxxsd:
      // FMA3: form3
      case TOP_xfmadd231xsd:
      case TOP_xfmadd231xxsd:
      case TOP_xfmadd231xxxsd:
      case TOP_xfnmadd231xsd:
      case TOP_xfnmadd231xxsd:
      case TOP_xfnmadd231xxxsd:
      case TOP_xfmsub231xsd:
      case TOP_xfmsub231xxsd:
      case TOP_xfmsub231xxxsd:
      case TOP_xfnmsub231xsd:
      case TOP_xfnmsub231xxsd:
      case TOP_xfnmsub231xxxsd:
      case TOP_vfsqrtxsd:
      case TOP_vfsqrtxxsd:
      case TOP_vfsqrtxxxsd:
      case TOP_vmovddupx:
      case TOP_vmovddupxx:
      case TOP_vmovddupxxx:
	return 8;

      case TOP_lddqa:		// 128 bit
      case TOP_lddqa_n32:
      case TOP_lddqu:
      case TOP_ldapd:
      case TOP_ldapd_n32:
      case TOP_ldaps:
      case TOP_ldaps_n32:
      case TOP_ldups:
      case TOP_ldupsx:
      case TOP_ldupsxx:
      case TOP_ldups_n32:
      case TOP_ldupd:
      case TOP_ldupdx:
      case TOP_ldupdxx:
      case TOP_ldupd_n32:
      case TOP_lddqax:
      case TOP_lddqux:
      case TOP_ldapdx:
      case TOP_ldapsx:
      case TOP_lddqaxx:
      case TOP_lddquxx:
      case TOP_ldapdxx:
      case TOP_ldapsxx:
      case TOP_fmovsldupx:
      case TOP_fmovshdupx:
      case TOP_fmovddupx:
      case TOP_fmovsldupxx:
      case TOP_fmovshdupxx:
      case TOP_fmovddupxx:
      case TOP_fmovsldupxxx:
      case TOP_fmovshdupxxx:
      case TOP_fmovddupxxx:
      case TOP_stdqa:
      case TOP_stdqa_n32:
      case TOP_stntpd:
      case TOP_stntps:
      case TOP_stdqu:
      case TOP_stdqax:
      case TOP_stntpdx:
      case TOP_stntpsx:
      case TOP_stdqux:
      case TOP_stdqaxx:
      case TOP_stntpdxx:
      case TOP_stntpsxx:
      case TOP_stdquxx:
      case TOP_staps:
      case TOP_staps_n32:
      case TOP_stapd:
      case TOP_stapd_n32:
      case TOP_stapsx:
      case TOP_stapdx:
      case TOP_stapsxx:
      case TOP_stapdxx:
      case TOP_storenti128:
      case TOP_vlddqa:		// 128 bit
      case TOP_vlddqa_n32:
      case TOP_vlddqu:
      case TOP_vldapd:
      case TOP_vldapd_n32:
      case TOP_vldaps:
      case TOP_vldaps_n32:
      case TOP_vldups:
      case TOP_vldupsx:
      case TOP_vldupsxx:
      case TOP_vldups_n32:
      case TOP_vldupd:
      case TOP_vldupdx:
      case TOP_vldupdxx:
      case TOP_vldupd_n32:
      case TOP_vlddqax:
      case TOP_vlddqux:
      case TOP_vldapdx:
      case TOP_vldapsx:
      case TOP_vlddqaxx:
      case TOP_vlddquxx:
      case TOP_vldapdxx:
      case TOP_vldapsxx:
      case TOP_vmovsldupx:
      case TOP_vmovsldupxx:
      case TOP_vmovsldupxxx:
      case TOP_vmovshdupx:
      case TOP_vmovshdupxx:
      case TOP_vmovshdupxxx:
      case TOP_vstdqa:
      case TOP_vstdqa_n32:
      case TOP_vstntpd:
      case TOP_vstntps:
      case TOP_vstdqu:
      case TOP_vstdqax:
      case TOP_vstntpdx:
      case TOP_vstntpsx:
      case TOP_vstdqux:
      case TOP_vstdqaxx:
      case TOP_vstntpdxx:
      case TOP_vstntpsxx:
      case TOP_vstdquxx:
      case TOP_vstaps:
      case TOP_vstaps_n32:
      case TOP_vstapd:
      case TOP_vstapd_n32:
      case TOP_vstapsx:
      case TOP_vstapdx:
      case TOP_vstapsxx:
      case TOP_vstapdxx:
      case TOP_vstorenti128:
	return 16;

      case TOP_store64_fm:	// Misc non-SSE, non-x87.
      case TOP_store64_fm_n32:
	break;

      default:
	FmtAssert(FALSE, ("CGTARG_Mem_Ref_Bytes: unknown SSE OP code"));
    }
  }

  if( OP_store( memop ) ){
    const int opnd = OP_find_opnd_use( memop, OU_storeval );
    return OP_opnd_size( (OP*)memop, opnd ) / 8;
  }

  switch (topcode){ 
  case TOP_addxr8:
  case TOP_addxxr8:
  case TOP_addxxxr8:
  case TOP_addxr8_n32:
  case TOP_addixr8:
  case TOP_addixxr8:
  case TOP_addixxxr8:
  case TOP_addixr8_n32:
  case TOP_subxr8:
  case TOP_subxxr8:
  case TOP_subxxxr8:
  case TOP_subxr8_n32:
  case TOP_subixr8:
  case TOP_subixxr8:
  case TOP_subixxxr8:
  case TOP_subixr8_n32:
  case TOP_andxr8:
  case TOP_andxxr8:
  case TOP_andxxxr8:
  case TOP_andxr8_n32:
  case TOP_andixr8:
  case TOP_andixxr8:
  case TOP_andixxxr8:
  case TOP_andixr8_n32:
  case TOP_orxr8:
  case TOP_orxxr8:
  case TOP_orxxxr8:
  case TOP_orxr8_n32:
  case TOP_orixr8:
  case TOP_orixxr8:
  case TOP_orixxxr8:
  case TOP_orixr8_n32:
  case TOP_xorxr8:
  case TOP_xorxxr8:
  case TOP_xorxxxr8:
  case TOP_xorxr8_n32:
  case TOP_xorixr8:
  case TOP_xorixxr8:
  case TOP_xorixxxr8:
  case TOP_xorixr8_n32:
  case TOP_negxr8:
  case TOP_negxxr8:
  case TOP_negxxxr8:
  case TOP_negxr8_n32:
  case TOP_notxr8:
  case TOP_notxxr8:
  case TOP_notxxxr8:
  case TOP_notxr8_n32:
  case TOP_incxr8:
  case TOP_incxxr8:
  case TOP_incxxxr8:
  case TOP_incxr8_n32:
  case TOP_decxr8:
  case TOP_decxxr8:
  case TOP_decxxxr8:
  case TOP_decxr8_n32:
  case TOP_xor8:
  case TOP_xorx8:
  case TOP_xorxx8:
  case TOP_xorxxx8:
  case TOP_and8:
  case TOP_andx8:
  case TOP_andxx8:
  case TOP_andxxx8:
  case TOP_test8:
  case TOP_testx8:
  case TOP_testxx8:
  case TOP_testxxx8:
  case TOP_cmp8:
  case TOP_cmpx8:
  case TOP_cmpxx8:
  case TOP_cmpxxx8:
  case TOP_cmpxi8:
  case TOP_cmpxxi8:
  case TOP_cmpxxxi8:
  case TOP_cmpxr8:
  case TOP_cmpxxr8:
  case TOP_cmpxxxr8:
  case TOP_or8:
  case TOP_orx8:
  case TOP_orxx8:
  case TOP_orxxx8:
  case TOP_ld8_32_n32:
  case TOP_ldu8_32_n32:
  case TOP_ld8_32:
  case TOP_ldx8_32:
  case TOP_ldxx8_32:
  case TOP_ldu8_32:
  case TOP_ldxu8_32:
  case TOP_ldxxu8_32:
  case TOP_ld8_64:
  case TOP_ldx8_64:
  case TOP_ldxx8_64:
  case TOP_ldu8_64:
  case TOP_ldxu8_64:
  case TOP_ldxxu8_64:
  case TOP_ld8_abs:
  case TOP_lock_xadd8:
    return 1;
      
  case TOP_addxr16:
  case TOP_addxxr16:
  case TOP_addxxxr16:
  case TOP_addxr16_n32:
  case TOP_addixr16:
  case TOP_addixxr16:
  case TOP_addixxxr16:
  case TOP_addixr16_n32:
  case TOP_subxr16:
  case TOP_subxxr16:
  case TOP_subxxxr16:
  case TOP_subxr16_n32:
  case TOP_subixr16:
  case TOP_subixxr16:
  case TOP_subixxxr16:
  case TOP_subixr16_n32:
  case TOP_andxr16:
  case TOP_andxxr16:
  case TOP_andxxxr16:
  case TOP_andxr16_n32:
  case TOP_andixr16:
  case TOP_andixxr16:
  case TOP_andixxxr16:
  case TOP_andixr16_n32:
  case TOP_orxr16:
  case TOP_orxxr16:
  case TOP_orxxxr16:
  case TOP_orxr16_n32:
  case TOP_orixr16:
  case TOP_orixxr16:
  case TOP_orixxxr16:
  case TOP_orixr16_n32:
  case TOP_xorxr16:
  case TOP_xorxxr16:
  case TOP_xorxxxr16:
  case TOP_xorxr16_n32:
  case TOP_xorixr16:
  case TOP_xorixxr16:
  case TOP_xorixxxr16:
  case TOP_xorixr16_n32:
  case TOP_negxr16:
  case TOP_negxxr16:
  case TOP_negxxxr16:
  case TOP_negxr16_n32:
  case TOP_notxr16:
  case TOP_notxxr16:
  case TOP_notxxxr16:
  case TOP_notxr16_n32:
  case TOP_incxr16:
  case TOP_incxxr16:
  case TOP_incxxxr16:
  case TOP_incxr16_n32:
  case TOP_decxr16:
  case TOP_decxxr16:
  case TOP_decxxxr16:
  case TOP_decxr16_n32:
  case TOP_xor16:
  case TOP_xorx16:
  case TOP_xorxx16:
  case TOP_xorxxx16:
  case TOP_and16:
  case TOP_andx16:
  case TOP_andxx16:
  case TOP_andxxx16:
  case TOP_test16:
  case TOP_testx16:
  case TOP_testxx16:
  case TOP_testxxx16:
  case TOP_cmp16:
  case TOP_cmpx16:
  case TOP_cmpxx16:
  case TOP_cmpxxx16:
  case TOP_cmpxi16:
  case TOP_cmpxxi16:
  case TOP_cmpxxxi16:
  case TOP_cmpxr16:
  case TOP_cmpxxr16:
  case TOP_cmpxxxr16:
  case TOP_or16:
  case TOP_orx16:
  case TOP_orxx16:
  case TOP_orxxx16:
  case TOP_ld16_32_n32:
  case TOP_ldu16_32_n32:
  case TOP_ld16_32:
  case TOP_ldx16_32:
  case TOP_ldxx16_32:
  case TOP_ldu16_32:
  case TOP_ldxu16_32:
  case TOP_ldxxu16_32:
  case TOP_ld16_64:
  case TOP_ldx16_64:
  case TOP_ldxx16_64:
  case TOP_ldu16_64:
  case TOP_ldxu16_64:
  case TOP_ldxxu16_64:
  case TOP_fldcw:
  case TOP_filds:
  case TOP_ld16_abs:
  case TOP_lock_xadd16:
    return 2;

  case TOP_addxr32:
  case TOP_addxxr32:
  case TOP_addxxxr32:
  case TOP_addxr32_n32:
  case TOP_addixr32:
  case TOP_addixxr32:
  case TOP_addixxxr32:
  case TOP_addixr32_n32:
  case TOP_subxr32:
  case TOP_subxxr32:
  case TOP_subxxxr32:
  case TOP_subxr32_n32:
  case TOP_subixr32:
  case TOP_subixxr32:
  case TOP_subixxxr32:
  case TOP_subixr32_n32:
  case TOP_andxr32:
  case TOP_andxxr32:
  case TOP_andxxxr32:
  case TOP_andxr32_n32:
  case TOP_andixr32:
  case TOP_andixxr32:
  case TOP_andixxxr32:
  case TOP_andixr32_n32:
  case TOP_orxr32:
  case TOP_orxxr32:
  case TOP_orxxxr32:
  case TOP_orxr32_n32:
  case TOP_orixr32:
  case TOP_orixxr32:
  case TOP_orixxxr32:
  case TOP_orixr32_n32:
  case TOP_xorxr32:
  case TOP_xorxxr32:
  case TOP_xorxxxr32:
  case TOP_xorxr32_n32:
  case TOP_xorixr32:
  case TOP_xorixxr32:
  case TOP_xorixxxr32:
  case TOP_xorixr32_n32:
  case TOP_negxr32:
  case TOP_negxxr32:
  case TOP_negxxxr32:
  case TOP_negxr32_n32:
  case TOP_notxr32:
  case TOP_notxxr32:
  case TOP_notxxxr32:
  case TOP_notxr32_n32:
  case TOP_incxr32:
  case TOP_incxxr32:
  case TOP_incxxxr32:
  case TOP_incxr32_n32:
  case TOP_decxr32:
  case TOP_decxxr32:
  case TOP_decxxxr32:
  case TOP_decxr32_n32:
  case TOP_xorx32:
  case TOP_xorxx32:
  case TOP_xorxxx32:
  case TOP_orx32:
  case TOP_orxx32:
  case TOP_orxxx32:
  case TOP_andx32:
  case TOP_andxx32:
  case TOP_andxxx32:
  case TOP_cmpx32:
  case TOP_cmpxx32:
  case TOP_cmpxxx32:
  case TOP_cmpxi32:
  case TOP_cmpxxi32:
  case TOP_cmpxxxi32:
  case TOP_cmpxr32:
  case TOP_cmpxxr32:
  case TOP_cmpxxxr32:
  case TOP_testx32:
  case TOP_testxx32:
  case TOP_testxxx32:
  case TOP_ld32_n32:
  case TOP_ld32:
  case TOP_ldx32:
  case TOP_ldxx32:
  case TOP_ld32_64:
  case TOP_ldx32_64:
  case TOP_ldxx32_64:
  case TOP_ld32_64_off:
  case TOP_ldss:
  case TOP_ldss_n32:
  case TOP_ldssx:
  case TOP_ldssxx:
  case TOP_addx32:
  case TOP_subx32:
  case TOP_addxx32:
  case TOP_subxx32:
  case TOP_addxxx32:
  case TOP_subxxx32:
  case TOP_addxss:
  case TOP_subxss:
  case TOP_addxxss:
  case TOP_subxxss:
  case TOP_addxxxss:
  case TOP_subxxxss:
  case TOP_mulxss:
  case TOP_mulxxss:
  case TOP_mulxxxss:
  case TOP_divxss:
  case TOP_divxxss:
  case TOP_divxxxss:
  case TOP_comixss:
  case TOP_comixxss:
  case TOP_comixxxss:
  case TOP_fildl:
  case TOP_flds:
  case TOP_flds_n32:
  case TOP_ld32_abs:
  case TOP_cvtsi2sd_x:
  case TOP_cvtsi2sd_xx:
  case TOP_cvtsi2sd_xxx:
  case TOP_cvtsi2ss_x:
  case TOP_cvtsi2ss_xx:
  case TOP_cvtsi2ss_xxx:
  case TOP_lock_add32:
  case TOP_lock_adc32:
  case TOP_lock_and32:
  case TOP_lock_or32:
  case TOP_lock_xor32:
  case TOP_lock_sub32:
  case TOP_lock_xadd32:
  // AVX
  case TOP_vldss:
  case TOP_vldss_n32:
  case TOP_vldssx:
  case TOP_vldssxx:
  case TOP_vfaddxss:
  case TOP_vsubxss:
  case TOP_vfaddxxss:
  case TOP_vsubxxss:
  case TOP_vfaddxxxss:
  case TOP_vsubxxxss:
  case TOP_vmulxss:
  case TOP_vmulxxss:
  case TOP_vmulxxxss:
  case TOP_vdivxss:
  case TOP_vdivxxss:
  case TOP_vdivxxxss:
  case TOP_vcomixss:
  case TOP_vcomixxss:
  case TOP_vcomixxxss:
  case TOP_vcvtsd2ssx:
  case TOP_vcvtsd2ssxx:
  case TOP_vcvtsd2ssxxx:
  case TOP_vcvtsi2ssx:
  case TOP_vcvtsi2ssxx:
  case TOP_vcvtsi2ssxxx:
    return 4;

  case TOP_addxr64:
  case TOP_addxxr64:
  case TOP_addxxxr64:
  case TOP_addxr64_off:
  case TOP_addixr64:
  case TOP_addixxr64:
  case TOP_addixxxr64:
  case TOP_addixr64_off:
  case TOP_subxr64:
  case TOP_subxxr64:
  case TOP_subxxxr64:
  case TOP_subxr64_off:
  case TOP_subixr64:
  case TOP_subixxr64:
  case TOP_subixxxr64:
  case TOP_subixr64_off:
  case TOP_andxr64:
  case TOP_andxxr64:
  case TOP_andxxxr64:
  case TOP_andxr64_off:
  case TOP_andixr64:
  case TOP_andixxr64:
  case TOP_andixxxr64:
  case TOP_andixr64_off:
  case TOP_orxr64:
  case TOP_orxxr64:
  case TOP_orxxxr64:
  case TOP_orxr64_off:
  case TOP_orixr64:
  case TOP_orixxr64:
  case TOP_orixxxr64:
  case TOP_orixr64_off:
  case TOP_xorxr64:
  case TOP_xorxxr64:
  case TOP_xorxxxr64:
  case TOP_xorxr64_off:
  case TOP_xorixr64:
  case TOP_xorixxr64:
  case TOP_xorixxxr64:
  case TOP_xorixr64_off:
  case TOP_negxr64:
  case TOP_negxxr64:
  case TOP_negxxxr64:
  case TOP_negxr64_off:
  case TOP_notxr64:
  case TOP_notxxr64:
  case TOP_notxxxr64:
  case TOP_notxr64_off:
  case TOP_incxr64:
  case TOP_incxxr64:
  case TOP_incxxxr64:
  case TOP_incxr64_off:
  case TOP_decxr64:
  case TOP_decxxr64:
  case TOP_decxxxr64:
  case TOP_decxr64_off:
  case TOP_xorx64:
  case TOP_xorxx64:
  case TOP_xorxxx64:
  case TOP_orx64:
  case TOP_orxx64:
  case TOP_orxxx64:
  case TOP_andx64:
  case TOP_andxx64:
  case TOP_andxxx64:
  case TOP_cmpx64:
  case TOP_cmpxx64:
  case TOP_cmpxxx64:
  case TOP_cmpxi64:
  case TOP_cmpxxi64:
  case TOP_cmpxxxi64:
  case TOP_cmpxr64:
  case TOP_cmpxxr64:
  case TOP_cmpxxxr64:
  case TOP_testx64:
  case TOP_testxx64:
  case TOP_testxxx64:
  case TOP_ld64:
  case TOP_ldx64:
  case TOP_ldxx64:
  case TOP_ldsd_n32:
  case TOP_ldsd:
  case TOP_ldsdx:
  case TOP_ldsdxx:
  case TOP_ld64_2m:
  case TOP_ld64_2m_n32:
  case TOP_addx64:
  case TOP_subx64:
  case TOP_addxx64:
  case TOP_subxx64:
  case TOP_addxxx64:
  case TOP_subxxx64:
  case TOP_addxsd:
  case TOP_subxsd:
  case TOP_addxxsd:
  case TOP_subxxsd:
  case TOP_addxxxsd:
  case TOP_subxxxsd:
  case TOP_mulxsd:
  case TOP_mulxxsd:
  case TOP_mulxxxsd:
  case TOP_divxsd:
  case TOP_divxxsd:
  case TOP_divxxxsd:
  case TOP_comixsd:
  case TOP_comixxsd:
  case TOP_comixxxsd:
  case TOP_ijmpx:
  case TOP_ijmpxx:
  case TOP_ijmpxxx:
  case TOP_icallx:
  case TOP_icallxx:
  case TOP_icallxxx:
  case TOP_fildll:
  case TOP_fldl:
  case TOP_fldl_n32:
  case TOP_ld64_abs:
  case TOP_ld64_off:
  case TOP_cvtsd2ss_x:
  case TOP_cvtsd2ss_xx:
  case TOP_cvtsd2ss_xxx:
  case TOP_cvtsi2sdq_x:
  case TOP_cvtsi2sdq_xx:
  case TOP_cvtsi2sdq_xxx:
  case TOP_cvtsi2ssq_x:
  case TOP_cvtsi2ssq_xx:
  case TOP_cvtsi2ssq_xxx:
  case TOP_lock_add64:
  case TOP_lock_and64:
  case TOP_lock_or64:
  case TOP_lock_xor64:
  case TOP_lock_sub64:
  case TOP_lock_xadd64:
  case TOP_fmovsldupx:
  case TOP_fmovshdupx:
  case TOP_fmovddupx:
  case TOP_fmovsldupxx:
  case TOP_fmovshdupxx:
  case TOP_fmovddupxx:
  case TOP_fmovsldupxxx:
  case TOP_fmovshdupxxx:
  case TOP_fmovddupxxx:
  // AVX
  case TOP_vldsd_n32:
  case TOP_vldsd:
  case TOP_vldsdx:
  case TOP_vldsdxx:
  case TOP_vfaddxsd:
  case TOP_vfaddxxsd:
  case TOP_vfaddxxxsd:
  case TOP_vsubxsd:
  case TOP_vsubxxsd:
  case TOP_vsubxxxsd:
  case TOP_vmulxsd:
  case TOP_vmulxxsd:
  case TOP_vmulxxxsd:
  case TOP_vdivxsd:
  case TOP_vdivxxsd:
  case TOP_vdivxxxsd:
  case TOP_vcomixsd:
  case TOP_vcomixxsd:
  case TOP_vcomixxxsd:
  case TOP_vmovsldupx:
  case TOP_vmovsldupxx:
  case TOP_vmovsldupxxx:
  case TOP_vmovshdupx:
  case TOP_vmovshdupxx:
  case TOP_vmovshdupxxx:
  case TOP_vmovddupx:
  case TOP_vmovddupxx:
  case TOP_vmovddupxxx:
  case TOP_vcvtsi2sdx:
  case TOP_vcvtsi2sdxx:
  case TOP_vcvtsi2sdxxx:
  case TOP_vcvtsi2sdqx:
  case TOP_vcvtsi2sdqxx:
  case TOP_vcvtsi2sdqxxx:
  case TOP_vcvtsi2ssqx:
  case TOP_vcvtsi2ssqxx:
  case TOP_vcvtsi2ssqxxx:
    return 8;

  case TOP_fldt:
  case TOP_fldt_n32:
    return 16;
  }

  FmtAssert( false, ("Unknown mem ref bytes: %s", TOP_Name(topcode)) );
  return 0;
}

/* ====================================================================
 *
 * CGTARG_Is_OP_Speculative
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Is_OP_Speculative(OP *op)
{
  if (!OP_load(op)) return FALSE;

  // speculative and advanced loads are safe to speculate.
  if (CGTARG_Is_OP_Advanced_Load(op) || CGTARG_Is_OP_Speculative_Load(op))
    return TRUE;

  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Perform_THR_Code_Generation
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Perform_THR_Code_Generation (OP *load_op, OP *chk_load,
					 THR_TYPE type)
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}

/* ====================================================================
 *
 * CGTARG_ARC_Sched_Latency
 *
 * See interface description
 *
 * ====================================================================
 */
INT CGTARG_ARC_Sched_Latency(
  ARC *arc
)
{
  if ( ARC_kind(arc) == CG_DEP_PREBR && 
		  PROC_has_same_cycle_branch_shadow() )
    return 0;
  else
    return ARC_latency(arc);
}


/* ====================================================================
 *
 * CGTARG_Bundle_Slot_Available
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL 
CGTARG_Bundle_Slot_Available(TI_BUNDLE              *bundle,
			     OP                     *op,
			     INT                     slot,
			     ISA_EXEC_UNIT_PROPERTY *prop, 
			     BOOL                    stop_bit_reqd,
			     const CG_GROUPING      *grouping)
{
  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Bundle_Stop_Bit_Available
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL 
CGTARG_Bundle_Stop_Bit_Available(TI_BUNDLE *bundle, INT slot)
{
  // Return TRUE the stop-bit is already set.
  if (TI_BUNDLE_stop_bit(bundle, slot)) return TRUE;

  return TI_BUNDLE_Stop_Bit_Available(bundle, slot);
}

/* ====================================================================
 *
 * CGTARG_Handle_Bundle_Hazard
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Handle_Bundle_Hazard (OP                          *op, 
			     TI_BUNDLE                   *bundle, 
			     VECTOR                      *bundle_vector,
			     BOOL                        can_fill, 
			     INT                         slot_pos, 
			     INT                         max_pos,
			     BOOL                        stop_bit_reqd,
			     ISA_EXEC_UNIT_PROPERTY      prop) 
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}

/* ====================================================================
 *
 * CGTARG_Handle_Errata_Hazard
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Handle_Errata_Hazard (OP *op, INT erratnum, INT ops_to_check)
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}

/* ====================================================================
 *
 * Reduce_Fraction
 *
 * Half hearted attempt to reduce a fraction. If we don't succeed
 * the only problem will be that we might round incorrectly on a
 * instruction rate.
 *
 * The algorithm is to first try the denominator as a factor and
 * then a few small primes.
 *
 * ====================================================================
 */
static void
Reduce_Fraction(INT frac[2])
{
  INT i;
  static const INT primes[] = {2, 3, 5, 7, 11, 13};
  INT n = frac[0];
  INT d = frac[1];
  INT p = d;

  if (d < -1 || d > 1) {
    for (i = sizeof(primes) / sizeof(primes[0]); ; p = primes[--i]) {
      while (n % p == 0 && d % p == 0) {
	n = n / p;
	d = d / p;
      }
      if (i == 0) break;
    }
  }

  frac[0] = n;
  frac[1] = d;
}


/* ====================================================================
 *
 * Harmonic_Mean
 *
 * Compute the harmonic weighted mean of two rates as follows:
 *
 *	  1        a                    b
 *	---- = ( ----- * a_rate ) + ( ----- * b_rate )
 *	mean     a + b                a + b
 *
 * Where:
 *
 *	"a" is the number of operations of class "a"
 *	"b" is the number of operations of class "b"
 *
 * ====================================================================
 */
static void
Harmonic_Mean(
  INT mean[2],
  INT a,
  const INT a_rate[2],
  INT b,
  const INT b_rate[2]
) {
  if (a == 0) {
    mean[0] = b_rate[0];
    mean[1] = b_rate[1];
  } else if (b == 0) {
    mean[0] = a_rate[0];
    mean[1] = a_rate[1];
  } else {
    mean[1] =   (a * a_rate[1] * b_rate[0]) 
	      + (b * b_rate[1] * a_rate[0]);
    mean[0] = (a + b) * a_rate[0] * b_rate[0];
    Reduce_Fraction(mean);
  }
}


/* ====================================================================
 *
 * CGTARG_Peak_Rate
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Peak_Rate( PEAK_RATE_CLASS prc, PRC_INFO *info, INT ratio[2] )
{
  ratio[0] = 1;
  ratio[1] = 1;
  
  switch (prc) {
  case PRC_INST:
    ratio[0] = 4;
    break;
  case PRC_MADD:
  case PRC_MEMREF:
    ratio[0] = 2;
    break;
  case PRC_FLOP:
  case PRC_FADD:
  case PRC_FMUL:
    ratio[0] = 2;
    break;
  case PRC_IOP:
    ratio[0] = 2;
    break;
  default:
    ratio[0] = 2;
    break;
  }
}

/* =======================================================================
 *
 *  Plural
 *
 *  Return "s" if i != 1, "" otherwise.  Used to get the number of nouns
 *  right when printing.
 *
 * =======================================================================
 */
#define Plural(i) ((i) != 1 ? "s" : "")


/* =======================================================================
 *
 *  Percent_Of_Peak
 *
 *  Compute the percentage of peak instructions executed. Both the
 *  actual number of instructions executed and the peak attainable
 *  are expressed as a fraction of insts/cycle.
 *
 * =======================================================================
 */
static INT
Percent_Of_Peak(INT numer, INT denom, INT peak[2])
{
  if (numer == 0) return 0;
  return (numer * peak[1] * 100) / ((denom * peak[0]) + peak[1] - 1);
}


/* =======================================================================
 *
 *  CGTARG_Print_PRC_INFO
 *
 *  Print statistics for the PRC_INFO to a 'file'.
 *
 * =======================================================================
 */
void
CGTARG_Print_PRC_INFO(
  FILE       *file,
  PRC_INFO   *info,
  INT32      ii,
  const char *prefix,
  const char *suffix
)
{
  const char *s;
  INT madds_per_cycle[2];
  INT memrefs_per_cycle[2];
  INT flops_per_cycle[2];
  INT fadds_per_cycle[2];
  INT fmuls_per_cycle[2];
  INT iops_per_cycle[2];
  INT insts_per_cycle[2];
  INT insts = info->refs[PRC_INST];
  INT memrefs = info->refs[PRC_MEMREF];
  INT flops = info->refs[PRC_FLOP];
  INT madds = info->refs[PRC_MADD];
  INT fadds = info->refs[PRC_FADD];
  INT fmuls = info->refs[PRC_FMUL];
  INT iops = info->refs[PRC_IOP];

  CGTARG_Peak_Rate(PRC_INST, info, insts_per_cycle);
  CGTARG_Peak_Rate(PRC_MEMREF, info, memrefs_per_cycle);
  CGTARG_Peak_Rate(PRC_FLOP, info, flops_per_cycle);
  CGTARG_Peak_Rate(PRC_MADD, info, madds_per_cycle);
  CGTARG_Peak_Rate(PRC_FADD, info, fadds_per_cycle);
  CGTARG_Peak_Rate(PRC_FMUL, info, fmuls_per_cycle);
  CGTARG_Peak_Rate(PRC_IOP, info, iops_per_cycle);

  FmtAssert( madds == 0, ("madds != 0 ") );

  if (flops != 0) {
    BOOL unbalanced_fpu = FALSE;

    if ( madds_per_cycle[0] != 0 ) {
      fprintf(file,"%s%5d flop%1s        (%3d%% of peak)%s",
		 prefix,
		 flops,
		 Plural(flops),
		 Percent_Of_Peak(flops, ii, flops_per_cycle),
		 suffix);
    }
    else {
      fprintf(file,"%s%5d flop%1s        (%3d%% of peak)%s",
		 prefix,
		 flops,
		 Plural(flops),
		 Percent_Of_Peak(flops, ii, flops_per_cycle),
		 suffix);
    }

    if ( unbalanced_fpu ) {
      INT fmuls2_per_cycle[2]; /* combined fmul/madd peak rate */
      INT fadds2_per_cycle[2]; /* combined fadd/madd peak rate */
      INT fadds2 = fadds + madds;
      INT fmuls2 = fmuls + madds;

      Harmonic_Mean(fmuls2_per_cycle,
		    fmuls, fmuls_per_cycle,
		    madds, madds_per_cycle);
      Harmonic_Mean(fadds2_per_cycle,
		    fadds, fadds_per_cycle,
		    madds, madds_per_cycle);

      fprintf(file,"%s%5d fmul%1s        (%3d%% of peak)%s%s",
		 prefix,
		 fmuls2,
		 Plural(fmuls2),
		 Percent_Of_Peak(fmuls2, ii, fmuls2_per_cycle),
		 madds_per_cycle[0] ? " (madds count as 1)" : "",
		 suffix);
      fprintf(file,"%s%5d fadd%1s        (%3d%% of peak)%s%s",
		 prefix,
		 fadds2,
		 Plural(fadds2),
		 Percent_Of_Peak(fadds2, ii, fadds2_per_cycle),
		 madds_per_cycle[0] ? " (madds count as 1)" : "",
		 suffix);
    }
  }

  s = "";
  if (FALSE) {
    iops += memrefs;
    s = " (mem refs included)";
  }

  fprintf(file,"%s%5d mem ref%1s     (%3d%% of peak)%s"
               "%s%5d integer op%1s  (%3d%% of peak)%s%s"
               "%s%5d instruction%1s (%3d%% of peak)%s",
               prefix,
               memrefs,
               Plural(memrefs),
               Percent_Of_Peak(memrefs, ii, memrefs_per_cycle),
	       suffix,
               prefix,
               iops,
               Plural(iops),
               Percent_Of_Peak(iops, ii, iops_per_cycle),
	       s,
	       suffix,
               prefix,
               insts,
               Plural(insts),
               Percent_Of_Peak(insts, ii, insts_per_cycle),
	       suffix);
}



/* =======================================================================
 *
 *  CGTARG_Compute_PRC_INFO
 *
 *  Compute some basic information about the given 'bb'. 
 *
 * =======================================================================
 */
void
CGTARG_Compute_PRC_INFO(
  BB *bb,
  PRC_INFO *info
)
{
  OP *op;

  bzero (info, sizeof (PRC_INFO));

  for ( op = BB_first_op(bb); op != NULL; op = OP_next(op) ) {
    INT num_insts = OP_Real_Ops (op);

    if (num_insts == 0) continue;

    info->refs[PRC_INST] += num_insts;

    if( OP_memory(op) || OP_load_exe(op) ){
      ++info->refs[PRC_MEMREF];
    }

    if( OP_memory(op) && !OP_load_exe(op) ){
      ;
    }
    else if ( OP_flop(op) ) {
      BOOL is_single = (OP_result_size(op,0) == 32);

      ++info->refs[PRC_FLOP];
      info->refs[PRC_FLOP_S] += is_single;
      if (OP_madd(op)) {
        ++info->refs[PRC_MADD];
	info->refs[PRC_MADD_S] += is_single;
      }
      else if (OP_fadd(op) || OP_fsub(op)) {
	++info->refs[PRC_FADD];
	info->refs[PRC_FADD_S] += is_single;
      }
      else if (OP_fmul(op)) {
	++info->refs[PRC_FMUL];
	info->refs[PRC_FMUL_S] += is_single;
      }
    }
    else {
      INT k;

      /* Conditional moves and m[tf]c1 are not tagged as flops.
       * We certainly don't want to call them integer ops, so assume
       * anything that uses FP regs isn't an integer instruction.
       */
      if (OP_has_result(op) && TN_is_float(OP_result(op,0))) goto not_iop;

      for (k = 0; k < OP_opnds(op); k++) {
	if (TN_is_float(OP_opnd(op,k))) goto not_iop;
      }

      info->refs[PRC_IOP] += num_insts;

    not_iop:
      ;
    }
  }
}


/* ====================================================================
 *
 * CG_TARG_Branch_Info
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Branch_Info ( const OP  *op,
                    INT *tfirst,  /* Which operand is the first target? */
                    INT *tcount ) /* How many target operands are there? */
{
  INT i;
  TN *tn;

  /* Initialize results: */
  *tfirst = -1;
  *tcount = 0;

  /* Find the first target: */
  for ( i = 0; ; i++ ) {
    if ( i >= OP_opnds(op) ) return;
    tn = OP_opnd(op,i);
    if ( tn != NULL && TN_is_label(tn) ) break;
  }
  *tfirst = i;

  /* Count the targets: */
  *tcount = 1;
  for ( i++; i < OP_opnds(op); i++ ) {
    tn = OP_opnd(op,i);
    if ( tn == NULL || ! TN_is_label(tn) ) return;
    (*tcount)++;
  }
  return;
}


/* ====================================================================
 *
 * CGTARG_Can_Be_Speculative
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Can_Be_Speculative( OP *op )
{
  WN *wn;

  /* not allowed to speculate anything. */
  if (Eager_Level == EAGER_NONE) return FALSE;

  /* don't speculate volatile memory references. */
  if (OP_volatile(op)) return FALSE;

  // TOP_Can_Be_Speculative is a test for OPs that _cannot_ be speculated.
  // Don't assume OP can be speculated just because TOP_Can_Be_Speculative
  // returns TRUE.  Bug 13958.
  if (!TOP_Can_Be_Speculative(OP_code(op))) return FALSE;

  if (!OP_load(op)) return FALSE;

  /* Try to identify simple scalar loads than can be safely speculated:
   *  a) read only loads (literals, GOT-loads, etc.)
   *  b) load of a fixed variable (directly referenced)
   *  c) load of a fixed variable (base address is constant or
   *     known to be in bounds)
   *  d) speculative, advanced and advanced-speculative loads are safe.
   */

  /*  a) read only loads (literals, GOT-loads, etc.)
   */
  if (OP_no_alias(op)) goto scalar_load;

  /*  b) load of a fixed variable (directly referenced); this
   *     includes spill-restores.
   *  b') exclude cases of direct loads of weak symbols (#622949).
   */
  if (TN_is_symbol(OP_opnd(op, 1)) &&
      !ST_is_weak_symbol(TN_var(OP_opnd(op, 1)))) goto scalar_load;

  /*  c) load of a fixed variable (base address is constant or
   *     known to be in bounds), comment out the rematerizable bit check 
   *     since it doesn;t guarantee safeness all the time.
   */
  if (/*   TN_is_rematerializable(OP_opnd(op, 0)) || */
      (   (wn = Get_WN_From_Memory_OP(op))
	  && Alias_Manager->Safe_to_speculate(wn))) goto scalar_load;

  /* d) speculative, advanced, speculative-advanced loads are safe to 
   *    speculate. 
   */
  if (CGTARG_Is_OP_Speculative(op)) goto scalar_load;

  /* If we got to here, we couldn't convince ourself that we have
   * a scalar load -- no speculation this time...
   */
  return FALSE;

  /* We now know we have a scalar load of some form. Determine if they
   * are allowed to be speculated.
   */
scalar_load:
  return TRUE; 
}

/* ====================================================================
 *
 * CGTARG_Is_OP_Speculative_Load
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Is_OP_Speculative_Load( OP *memop )
{
  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Is_OP_Advanced_Load
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Is_OP_Advanced_Load( OP *memop )
{
  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Is_OP_Check_Load
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Is_OP_Check_Load( OP *memop )
{
  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_OP_Defs_TN
 * CGTARG_OP_Refs_TN
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_OP_Defs_TN( OP *op, TN *tn )
{
  return FALSE;
}

BOOL
CGTARG_OP_Refs_TN( OP *op, TN *tn )
{
  return FALSE;
}


/* ====================================================================
 *
 * CGTARG_Interference implementation starts here
 *
 * ====================================================================
 */

static MEM_POOL interference_pool;
static VOID_LIST** writing;     /* writing[i] is a list of live ranges being
                                   written into registers in cycle i */
static BOOL is_loop;            /* Are we working on a loop? */
static INT32 assumed_longest_latency = 40;
                                /* We need to allocate <writing> to be somewhat
                                   longer than the number of cycles in the
                                   schedule in order to accommodate writes
                                   initiated near the end of the schedule.
                                   We'll check and grow this number as
                                   necessary. */
static INT32 cycle_count;       /* Number of cycles in the schedule under
                                   consideration. */
static void (*make_interference)(void*,void*);
                                /* Client's interference call back. */

/* ====================================================================
 *
 * Increase_Assumed_Longest_Latency
 *
 * We need to increase our assumptions about the longest latency operation
 * in our target.  Also reallocate <writing>.
 *
 * ====================================================================
 */
static void
Increase_Assumed_Longest_Latency(INT32 new_longest_latency )
{
  DevWarn("Assumed longest latency should be at least %d",
          new_longest_latency);
  writing = TYPE_MEM_POOL_REALLOC_N(VOID_LIST*,&interference_pool,writing,
                                    cycle_count + assumed_longest_latency,
                                    cycle_count + new_longest_latency);
  assumed_longest_latency = new_longest_latency;
}

/* ====================================================================
 *
 * CGTARG_Interference_Required
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Interference_Required(void)
{
  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Interference_Initialize
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Interference_Initialize( INT32 cycle_count_local, BOOL is_loop_local,
                                void (*make_interference_local)(void*,void*) )
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}

/* ====================================================================
 *
 * CGTARG_Result_Live_Range
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Result_Live_Range( void* lrange, OP* op, INT32 offset )
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}

/* ====================================================================
 *
 * CGTARG_Operand_Live_Range
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Operand_Live_Range( void* lrange, INT   opnd, OP*   op, INT32 offset )
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}

/* ====================================================================
 *
 * CGTARG_Interference_Finalize
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Interference_Finalize(void)
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}


/* ====================================================================
 *
 * CGTARG_Preg_Register_And_Class
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Preg_Register_And_Class(
  WN_OFFSET preg,
  ISA_REGISTER_CLASS *p_rclass,
  REGISTER *p_reg
)
{
  ISA_REGISTER_CLASS rclass;
  INT regnum;
  /* Get the target register number and class associated with the
   * preg, if there is one that is.
   */
  if (!Preg_Is_Dedicated(preg))
    return FALSE;

  if (!Preg_Offset_Is_Int(preg)   &&
      !Preg_Offset_Is_Float(preg) &&
      !Preg_Offset_Is_X87(preg)   &&
      !Preg_Offset_Is_MMX(preg))
    return FALSE;

  /* Get the target register number and class associated with the
   *    * preg, if there is one that is.
   */
  if (Preg_Offset_Is_Int(preg)) {
    regnum = preg - Int_Preg_Min_Offset;
    rclass = ISA_REGISTER_CLASS_integer;
  }
  else if (Preg_Offset_Is_Float(preg)) {
    regnum = preg - Float_Preg_Min_Offset;
    rclass = ISA_REGISTER_CLASS_float;
  }
  else if (Preg_Offset_Is_X87(preg)) {
    regnum = preg - X87_Preg_Min_Offset;
    rclass = ISA_REGISTER_CLASS_x87;
  }
  else if (Preg_Offset_Is_MMX(preg)) {
    regnum = preg - MMX_Preg_Min_Offset;
    rclass = ISA_REGISTER_CLASS_mmx;
  }
  else if (preg == 0) {
    regnum = 0;
    rclass = ISA_REGISTER_CLASS_integer;
  } 
  else {
    return FALSE;
  }

  /* Find the CG register for the target register and class. */
  for ( REGISTER reg = REGISTER_MIN;
	reg <= REGISTER_CLASS_last_register(rclass);
	reg++ )
  {
    if ( REGISTER_machine_id(rclass,reg) == regnum )
    {
      *p_reg = reg;
      *p_rclass = rclass;
      return TRUE;
    }
  }

  FmtAssert(FALSE, ("failed to map preg %d", preg));
  /*NOTREACHED*/
}


/* ====================================================================
 *
 * CGTARG_Compute_Branch_Parameters
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Compute_Branch_Parameters(INT32 *mispredict, INT32 *fixed, INT32 *brtaken, double *factor)
{
  *mispredict = 0;
  *fixed = 0;
  *brtaken = 0;
  *factor = 0.0;

  if (Is_Target_x86_64() )
  {
    *mispredict= 7; *fixed= 1; *brtaken= 1; *factor = 1.0;
  }
  else
  {
    FmtAssert(FALSE, ("invalid target"));
  }

 /*
  * override for command line options
  *	-CG:mispredicted_branch=N
  *	-CG:mispredicted_factor=N
  */
  if (CG_branch_mispredict_penalty >= 0)
    *mispredict= CG_branch_mispredict_penalty ;

  if (CG_branch_mispredict_factor >= 0)
    *factor= CG_branch_mispredict_factor * (.01);
}


/* ====================================================================
 *
 * CGTARG_Can_Change_To_Brlikely
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL CGTARG_Can_Change_To_Brlikely(OP *xfer_op, TOP *new_opcode)
{
  return FALSE;
}


/* ====================================================================
 *
 * CGTARG_Is_Long_Latency
 *
 * See interface description
 *
 * ====================================================================
 */
INT32 CGTARG_Latency( TOP op )
{
  return ( TI_LATENCY_Result_Available_Cycle(op,0) -
	   TI_LATENCY_Operand_Access_Cycle(op,0) );
}

BOOL CGTARG_Is_Long_Latency(TOP op)
{
  return ( CGTARG_Latency(op) > 2 );
}

/* ====================================================================
 *
 * CGTARG_Analyze_Branch
 *
 * See interface description
 *
 * ====================================================================
 */
VARIANT CGTARG_Analyze_Branch(
  OP *br,
  TN **tn1,
  TN **tn2)
{
  OP* cmp_op = NULL;

  /* Find out the cmp op first. */
  for( OP* op = OP_prev(br); op != NULL; op = OP_prev(op) ){
    if( TOP_is_change_rflags( OP_code(op) ) ){
      cmp_op = op;
      break;
    }
  }

#ifdef TARG_X8664
  // Expand_Ordered_Select_Compare can generate a compare followed by 2
  // conditional branches.  The second branch, jnp, will not have a cmp_op in
  // its BB.  Search for the cmp_op in the first BB.
  if (cmp_op == NULL) {
    Is_True(OP_code(br) == TOP_jnp,
	    ("CGTARG_Analyze_Branch: unexpected branch OP code"));
    BBLIST *edge;
    BB *fall_thru_pred = BB_Fall_Thru_Predecessor(OP_bb(br));
    int preds = 0;
    // Search all the preds.  The cmp op may or may not be in the fall thru
    // pred because cflow may have rearranged the BBs.
    FOR_ALL_BB_PREDS(OP_bb(br), edge) {
      BB *pred = BBLIST_item(edge);
      preds++;
      // Find the cmp op.
      for (OP* op = BB_last_op(pred); op != NULL; op = OP_prev(op)) {
	if (TOP_is_change_rflags(OP_code(op))) {
	  Is_True(cmp_op == NULL,
		  ("CGTARG_Analyze_Branch: found multiple cmp ops"));
	  cmp_op = op;
	  break;
	}
      }
#if !defined(Is_True_On)
      if (cmp_op != NULL)
	break;
#endif
    }
    Is_True(preds == 2,
	    ("CGTARG_Analyze_Branch: unexpected number of pred BBs"));
  }
#endif

  FmtAssert( cmp_op != NULL, ("compare op is missing") );

  if( OP_icmp( cmp_op ) ){
    /* Branch operands are usually number 0 and 1. Where something
       different is used, we override below. */
    *tn1 = OP_opnd( cmp_op, 0 );
    *tn2 = OP_opnd( cmp_op, 1 );

    if( OP_code(cmp_op) == TOP_test32  ||
	OP_code(cmp_op) == TOP_test64  ||
	OP_code(cmp_op) == TOP_testi32 ||
	OP_code(cmp_op) == TOP_testi64 ||
    OP_code(cmp_op) == TOP_test8   ||
    OP_code(cmp_op) == TOP_test16   ||
    OP_code(cmp_op) == TOP_testi8   ||
    OP_code(cmp_op) == TOP_testi16
    ){
      if( *tn1 == *tn2 )
	*tn2 = Gen_Literal_TN( 0, 4 );
      else
	return V_BR_NONE;
    }
  } else {
    /* The test operation has been removed by ebo. */
    *tn1 = OP_result( cmp_op, 0 );
    *tn2 = Gen_Literal_TN( 0, 4 );
  }

  /* bug#1407
     Don't miss the 64-bit comparisions.
   */
  const bool is_64bit = OP_result_size(cmp_op,0) == 64;

  switch( OP_code(br) ){
  case TOP_jge:
    return OP_flop(cmp_op) ? V_BR_DGE : ( is_64bit ? V_BR_I8GE : V_BR_I4GE );

  case TOP_jae:
    return OP_flop(cmp_op) ? V_BR_DGE : ( is_64bit ? V_BR_U8GE : V_BR_U4GE );

  case TOP_jg:
    return OP_flop(cmp_op) ? V_BR_DGT : ( is_64bit ? V_BR_I8GT : V_BR_I4GT );

  case TOP_ja:
    return OP_flop(cmp_op) ? V_BR_DGT : ( is_64bit ? V_BR_U8GT : V_BR_U4GT );

  case TOP_jle:
    return OP_flop(cmp_op) ? V_BR_DLE : ( is_64bit ? V_BR_I8LE : V_BR_I4LE );

  case TOP_jbe:
    return OP_flop(cmp_op) ? V_BR_DLE : ( is_64bit ? V_BR_U8LE : V_BR_U4LE );

  case TOP_jl:
    return OP_flop(cmp_op) ? V_BR_DLT : ( is_64bit ? V_BR_I8LT : V_BR_I4LT );

  case TOP_jb:
    return OP_flop(cmp_op) ? V_BR_DLT : ( is_64bit ? V_BR_U8LT : V_BR_U4LT );

  case TOP_je:
    return OP_flop(cmp_op) ? V_BR_DEQ : ( is_64bit ? V_BR_U8EQ : V_BR_U4EQ );

  case TOP_jne:
    return OP_flop(cmp_op) ? V_BR_DNE : ( is_64bit ? V_BR_U8NE : V_BR_U4NE );

  case TOP_jp:
  case TOP_jnp:
    // TOP_jp is used to force IEEE comparisons for TOP_jne and TOP_je.
    // So we cannot decide here.
    return V_BR_NONE;

  case TOP_js:
    Is_True(!OP_flop(cmp_op), ("CGTARG_Analyze_Branch: unexpected conditional branch %s for floating point\n", TOP_Name(OP_code(br))));
    return ( is_64bit ? V_BR_I8LT0 : V_BR_I4LT0 );
    
  case TOP_jns:
    Is_True(!OP_flop(cmp_op), ("CGTARG_Analyze_Branch: unexpected conditional branch %s for floating point\n", TOP_Name(OP_code(br))));
    return ( is_64bit ? V_BR_I8GE0 : V_BR_I4GE0 );
    
  default:
    FmtAssert( false, ("unexpected conditional branch %s\n", TOP_Name(OP_code(br))) );
    break;
  }

  return V_BR_NONE;
}


/* ====================================================================
 *
 * CGTARG_Analyze_Compare
 *
 * See interface description
 *
 * ====================================================================
 */
VARIANT CGTARG_Analyze_Compare(
  OP *br,
  TN **tn1,
  TN **tn2,
  OP **compare_op)
{
  TN* cond_tn1 = NULL;
  TN* cond_tn2 = NULL;

  /* Classify the condition based on the branch instruction.  */

  VARIANT variant = CGTARG_Analyze_Branch(br, &cond_tn1, &cond_tn2);

  /* Once we have varients for 'bf' and 'bt' (and possibly some
     others), we can attempt to find the comparison. For now, none of
     the branches have associated comparisons. */

  for( OP* op = OP_prev(br); op != NULL; op = OP_prev(op) ){
    if( TOP_is_change_rflags( OP_code(op) ) ){
      if( OP_icmp( op ) )
	*compare_op = op;
      break;
    }
  }

  *tn1 = cond_tn1;
  *tn2 = cond_tn2;

  return variant;
}


/* ====================================================================
 *
 * CGTARG_Equivalent_Nonindex_Memory_Op
 *
 * See interface description
 *
 * ====================================================================
 */
TOP
CGTARG_Equiv_Nonindex_Memory_Op ( OP *op )
{
  return TOP_UNDEFINED;
}

/* ====================================================================
 *
 * CGTARG_Which_OP_Select
 *
 * See interface description
 *
 * ====================================================================
 */
TOP
CGTARG_Which_OP_Select ( UINT16 bit_size, BOOL is_float, BOOL is_fcc )
{
  FmtAssert( FALSE, ( "CGTARG_Which_OP_Select: Unsupported Target") );
  return TOP_UNDEFINED;
}

/* ====================================================================
 *
 * Is_OP_fp_op1
 *
 * FP_OP1 = {sfmac, sfmisc, xma, xmpy, fmac, cvt.fx, fmisc}
 *
 * ====================================================================
 */
static BOOL
Is_OP_fp_op1(OP *op)
{
  return FALSE;
}

/* ====================================================================
 *
 * Insert_Stop_Bits
 *
 * ====================================================================
 */
void
Insert_Stop_Bits(BB *bb)
{
}

/* ====================================================================
 *
 * CGTARG_Special_Min_II
 *
 * See interface description
 *
 * ====================================================================
 */
INT32 CGTARG_Special_Min_II(BB* loop_body, BOOL trace)
{
  return 0;
}

/* ====================================================================
 *
 * Hardware_Workarounds
 *
 * Placeholder for all Hardware workarounds. 
 *
 * ====================================================================
 */
void
Hardware_Workarounds(void)
{
}

/* ====================================================================
 *
 * CGTARG_Initialize
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Initialize(void)
{
#define Set_Inv_Table(a,b)        \
   do {                           \
     CGTARG_Invert_Table[a] = b;  \
     CGTARG_Invert_Table[b] = a;  \
   } while( 0 )

#define Set_Immed_To_Reg_Table(a,b)    \
   do {                                \
     CGTARG_Immed_To_Reg_Table[a] = b; \
     CGTARG_Immed_To_Reg_Table[b] = a; \
   } while( 0 )

  INT32 i;

  /* Init all table entries to TOP_UNDEFINED.
   */
  for(i = 0; i <= TOP_count; ++i) {
    CGTARG_Invert_Table[i] = TOP_UNDEFINED;
    CGTARG_Immed_To_Reg_Table[i] = TOP_UNDEFINED;
  }

  for (i = 0; i <= ISA_REGISTER_CLASS_MAX; ++i) {
    INT j;
    for (j = 0; j <= ISA_REGISTER_CLASS_MAX; ++j) {
      CGTARG_Inter_RegClass_Copy_Table[i][j][FALSE] = TOP_UNDEFINED;
      CGTARG_Inter_RegClass_Copy_Table[i][j][TRUE] = TOP_UNDEFINED;
    }
  }

  /* Init table for CGTARG_Invert:
   */
  Set_Inv_Table( TOP_addss, TOP_subss );
  Set_Inv_Table( TOP_addsd, TOP_subsd );
  Set_Inv_Table( TOP_add32, TOP_sub32 );
  Set_Inv_Table( TOP_add64, TOP_sub64 );

  Set_Inv_Table( TOP_cmovb,  TOP_cmovae );
  Set_Inv_Table( TOP_cmove,  TOP_cmovne );
  Set_Inv_Table( TOP_cmovbe, TOP_cmova );
  Set_Inv_Table( TOP_cmovl,  TOP_cmovge );
  Set_Inv_Table( TOP_cmovle, TOP_cmovg );

  Set_Inv_Table( TOP_jb,  TOP_jae );
  Set_Inv_Table( TOP_je,  TOP_jne );
  Set_Inv_Table( TOP_jbe, TOP_ja );
  Set_Inv_Table( TOP_jge, TOP_jl );
  Set_Inv_Table( TOP_jle, TOP_jg );

  Set_Inv_Table( TOP_setb,  TOP_setae );
  Set_Inv_Table( TOP_sete,  TOP_setne );
  Set_Inv_Table( TOP_setbe, TOP_seta );
  Set_Inv_Table( TOP_setge, TOP_setl );
  Set_Inv_Table( TOP_setle, TOP_setg );

  /* Init table for CGTARG_Immed_To_Reg:
   */
  Set_Immed_To_Reg_Table( TOP_addi32, TOP_add32 );
  Set_Immed_To_Reg_Table( TOP_addi64, TOP_add64 );
  Set_Immed_To_Reg_Table( TOP_andi32, TOP_and32 );
  Set_Immed_To_Reg_Table( TOP_andi64, TOP_and64 );
  Set_Immed_To_Reg_Table( TOP_ori32,  TOP_or32  );
  Set_Immed_To_Reg_Table( TOP_ori64,  TOP_or64  );
  Set_Immed_To_Reg_Table( TOP_xori32, TOP_xor32 );
  Set_Immed_To_Reg_Table( TOP_xori64, TOP_xor64 );
  Set_Immed_To_Reg_Table( TOP_cmpi8,  TOP_cmp8  );
  Set_Immed_To_Reg_Table( TOP_cmpi16, TOP_cmp16 );
  Set_Immed_To_Reg_Table( TOP_cmpi32, TOP_cmp32 );
  Set_Immed_To_Reg_Table( TOP_cmpi64, TOP_cmp64 );
  Set_Immed_To_Reg_Table( TOP_imuli32,TOP_imul32 );
  Set_Immed_To_Reg_Table( TOP_imuli64,TOP_imul64 );


  /* Init table for CGTARG_Inter_RegClass_Copy_Table:
   */
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_float]
				  [ISA_REGISTER_CLASS_integer]
				  [FALSE] = TOP_UNDEFINED;
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_float]
				  [ISA_REGISTER_CLASS_integer]
				  [TRUE]  = TOP_UNDEFINED;

  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_integer]
				  [ISA_REGISTER_CLASS_float]
				  [FALSE] = TOP_UNDEFINED;
  CGTARG_Inter_RegClass_Copy_Table[ISA_REGISTER_CLASS_integer]
				  [ISA_REGISTER_CLASS_float]
				  [TRUE]  = TOP_UNDEFINED;

#undef Set_Inv_Table
#undef Set_Immed_To_Reg_Table

  return;
}


/* ====================================================================
 *
 * CGTARG_Load_From_Memory
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Load_From_Memory(TN *tn, ST *mem_loc, OPS *ops)
{
  TYPE_ID mtype = TY_mtype(ST_type(mem_loc));
  Exp_Load(mtype, mtype, tn, mem_loc, 0, ops, 0);
}


/* ====================================================================
 *
 * CGTARG_Store_To_Memory
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Store_To_Memory(TN *tn, ST *mem_loc, OPS *ops)
{
  TYPE_ID mtype = TY_mtype(ST_type(mem_loc));
  Exp_Store(mtype, tn, mem_loc, 0, ops, 0);
}


/* ====================================================================
 *
 * CGTARG_Init_Assoc_Base
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Init_Assoc_Base(void)
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}


/* ====================================================================
 *
 * CGTARG_Copy_Operand
 *
 * See interface description
 *
 * ====================================================================
 */
INT CGTARG_Copy_Operand(OP *op)
{
  const TOP opr = OP_code(op);

  switch( opr ){
  case TOP_addi32:
  case TOP_addi64:
  case TOP_subi32:
  case TOP_subi64:
  case TOP_sari32:
  case TOP_sari64:
  case TOP_shli32:
  case TOP_shli64:
  case TOP_shri32:
  case TOP_shri64:
  case TOP_lea32:
  case TOP_lea64:
    if (TN_has_value(OP_opnd(op,1)) && TN_value(OP_opnd(op,1)) == 0)
      return 0;
    break;

  case TOP_andi32:
  case TOP_andi64:
    {
      TN *src1 = OP_opnd( op, 1 );
      if (TN_is_constant(src1)) {
	INT64 val;
	if (TN_has_value(src1))
	  val = TN_value(src1);
	else FmtAssert(FALSE,("unexpected constant in CGTARG_Copy_Operand"));
	if (val == -1)
	  return 0;
      }
      break;
    }

  case TOP_movabsq:
  case TOP_ldc32:
  case TOP_ldc64:
    {
      TN* opnd = OP_opnd( op, 0 );
      if( TN_has_value( opnd ) ){
	return 0;
      }
    }
    break;

  case TOP_mov64:
  case TOP_movsd:
  case TOP_movss:
  case TOP_vmovsd:
  case TOP_vmovss:
  case TOP_movdq:
  case TOP_movapd:
  case TOP_movaps:
  case TOP_vmovdqa:
  case TOP_vmovapd:
  case TOP_vmovaps:
  case TOP_fmov:
  case TOP_mov64_m:
  case TOP_movm_2i32:
  case TOP_movm_2i64:
  case TOP_movi32_2m:
  case TOP_movi64_2m:
    return 0;

  case TOP_mov32:
    if( TN_size(OP_result(op,0)) == TN_size(OP_opnd(op,0)) )
      return 0;
    break;

  case TOP_ori32:
  case TOP_ori64:
  case TOP_xori32:
  case TOP_xori64:
    {
      TN *src1 = OP_opnd( op, 1 );
      if (TN_is_constant(src1)) {
	INT64 val;
	if (TN_has_value(src1))
	  val = TN_value(src1);
	else FmtAssert(FALSE,("unexpected constant in CGTARG_Copy_Operand"));
	if (val == 0)
	  return 0;
      }
      break;
    }
  }

  if (OP_copy(op)) {
    if (opr == TOP_add32 || opr == TOP_add64 ||
        opr == TOP_or32  || opr == TOP_or64  ||
        opr == TOP_mov32 || opr== TOP_mov64  ||
	opr == TOP_movsd || opr == TOP_movss ||
	opr == TOP_fmov )
      return 0;
  }

  if( OP_cond_move( op ) &&
      TNs_Are_Equivalent( OP_result(op,0), OP_opnd(op,0) ) ){
    return 0;
  }

  return -1;
}


/* ====================================================================
 *
 * CGTARG_Can_Fit_Immediate_In_Add_Instruction
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL CGTARG_Can_Fit_Immediate_In_Add_Instruction (INT64 immed)
{
  return ISA_LC_Value_In_Class (immed, LC_simm32);
}


/* ====================================================================
 *
 * CGTARG_Can_Load_Immediate_In_Single_Instruction
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL CGTARG_Can_Load_Immediate_In_Single_Instruction (INT64 immed)
{
  return ISA_LC_Value_In_Class (immed, LC_simm32);
}



/* ====================================================================
 *
 * CGTARG_Predicate_OP
 *
 * See interface description
 *
 * ====================================================================
 */
/*ARGSUSED*/
void
CGTARG_Predicate_OP(BB* bb, OP* op, TN* pred_tn)
{
  if (OP_has_predicate(op)) {
    FmtAssert( FALSE, ( "CGTARG_Which_OP_Select: Unsupported Target") );
  }
}

/* ====================================================================
 *
 * CGTARG_Branches_On_True
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Branches_On_True(OP* br_op, OP* cmp_op)
{
  return FALSE;
}



/* ====================================================================
 *
 * CGTARG_Parallel_Compare
 *
 * See interface description
 *
 * ====================================================================
 */
TOP
CGTARG_Parallel_Compare(OP* cmp_op, COMPARE_TYPE ctype)
{
  return TOP_UNDEFINED;
}


static BOOL OP_Reads_Dedicated_TN( OP* op, TN* ded_tn )
{
  for( int i = 0; i < OP_opnds(op); i++ ){
    TN* tmp_tn = CGTARG_Gen_Dedicated_Subclass_TN( op, i, FALSE );
    if( tmp_tn != NULL &&
	TNs_Are_Equivalent( tmp_tn, ded_tn ) )
      return TRUE;
  }

  return FALSE;
}


static BOOL OP_Writes_Dedicated_TN( OP* op, TN* ded_tn )
{
  for( int i = 0; i < OP_results(op); i++ ){
    TN* tmp_tn = CGTARG_Gen_Dedicated_Subclass_TN( op, i, TRUE );
    if( tmp_tn != NULL &&
	TNs_Are_Equivalent( tmp_tn, ded_tn ) )
      return TRUE;
  }

  return FALSE;  
}


static BOOL OP_is_lea( OP* op )
{
  const TOP top = OP_code(op);

  return ( top == TOP_lea32 || top == TOP_lea64 ||
	   top == TOP_leax32 || top == TOP_leax64 ||
	   top == TOP_leaxx32 || top == TOP_leaxx64 );
}

/* ====================================================================
 *
 * CGTARG_Dependence_Required
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL CGTARG_Dependence_Required( OP* pred_op, OP* succ_op )
{
  FmtAssert( OP_bb(pred_op) == OP_bb(succ_op), ("NYI") );

  /* Do not change the relative order for operations that store callee-saved
     registers for exception handling code. Refer to bug#1928 for detail.
     (bug#2205)
  */

  if( PU_Has_Exc_Handler ){
    /* Don't bother to scan thru <Saved_Callee_Saved_Regs> one-by-one to save
       us some compilation time.
    */
    if( BB_entry( OP_bb(pred_op) ) &&
	OP_store( pred_op )        &&
	OP_store( succ_op )        &&
	TN_is_save_reg( OP_opnd(pred_op,0) ) )
      return TRUE;
  }

  /* If exists a dependence between <pred_op> and <succ_op>, then we can return
     to avoid generating superfluous MISC arcs.
  */
  {
    for( ARC_LIST* arcs = OP_succs(pred_op);
	 arcs != NULL;
	 arcs = ARC_LIST_rest(arcs) ){
      ARC *arc = ARC_LIST_first(arcs);
      if( ARC_succ(arc) == succ_op )
	return FALSE;
    }
  }

  /* A sp adjustment operation should always be scheduled before a store operation,
     which could access the stack potentially.
  */
  {
    if( OP_results(pred_op) > 0              && 
	TN_is_sp_reg( OP_result(pred_op,0) ) &&
	OP_store( succ_op ) )
      return TRUE;
  }

  /* Do not schedule a load/store/mfence operation across an mfence operation.
   */
  {
    if( TOP_is_unknown_memdata( OP_code(pred_op) ) ){
      if( OP_memory( succ_op )   ||
	  OP_load_exe( succ_op ) ||
	  TOP_is_unknown_memdata( OP_code(succ_op) ) )
	return TRUE;
    }

    if( TOP_is_unknown_memdata( OP_code(succ_op) ) ){
      if( OP_memory( pred_op )   ||
	  OP_load_exe( pred_op ) ||
	  TOP_is_unknown_memdata( OP_code(pred_op) ) )
	return TRUE;
    }
  }

  /* Bug #336
     Don't schedule an op before/after the entry/exit stack
     adjustment code; otherwise will cause spilling error if
     this op is spilled later (before the stack is formed).
   */
  {
    BB* bb = OP_bb( pred_op );
    if( BB_entry(bb) && !BB_handler(bb) ){
      if( pred_op == BB_entry_sp_adj_op(bb) )
	return TRUE;
    }

    if( BB_exit(bb) ){
      if( succ_op == BB_exit_sp_adj_op(bb) )
	return TRUE;
    }
  }

  /* Return TRUE if <pred_op> and <succ_op> are connected with x87
     control-word.  Also return TRUE if one op changes the x87 control-word and
     the other op is an x87 arithmetic instruction.
   */
  {
    if (TOP_is_change_x87_cw(OP_code(pred_op)) &&
	(TOP_is_read_x87_cw(OP_code(succ_op)) ||
	 TOP_is_x87(OP_code(succ_op))))
      return TRUE;

    if ((TOP_is_read_x87_cw(OP_code(pred_op)) ||
	 TOP_is_x87(OP_code(pred_op))) &&
	TOP_is_change_x87_cw(OP_code(succ_op)))
      return TRUE;
  }

  /* Dependence exists between emms and x87/MMX OP.  Bug 11800. */
  {
    BOOL pred_is_x87_mmx = (OP_x87(pred_op) || OP_mmx(pred_op)) ? TRUE : FALSE;
    BOOL succ_is_x87_mmx = (OP_x87(succ_op) || OP_mmx(succ_op)) ? TRUE : FALSE;
    if ((OP_code(pred_op) == TOP_emms && succ_is_x87_mmx) ||
	(OP_code(succ_op) == TOP_emms && pred_is_x87_mmx))
      return TRUE;
  }

  /* Return TRUE if <pred_op> can change rflags, and <succ_op> is a
     comparison op. */

  /* So far TOP_lea and TOP_leax are considered as the ones can change
     rflags, since these two ops might be converted to add later. */
  {
    const BOOL pred_is_lea = OP_is_lea( pred_op );
    const BOOL succ_is_lea = OP_is_lea( succ_op );    

    if( TOP_is_change_rflags( OP_code(pred_op) ) || pred_is_lea ){
      if( OP_icmp( succ_op ) || OP_reads_rflags( succ_op ) )
	return TRUE;

      /* A <neg> op will set the %rflags implicitly...
	 To Do: this routine should be re-written!!! */
      if( OP_code(succ_op) == TOP_neg32 || OP_code(succ_op) == TOP_neg64 ){
	for( ARC_LIST* arcs = OP_succs(succ_op); arcs != NULL; arcs = ARC_LIST_rest(arcs) ){
	  ARC* arc = ARC_LIST_first(arcs);
	  OP* op = ARC_succ(arc);
	  const TOP top = OP_code( op );

	  if( ( TOP_is_cond_set(top) || TOP_is_cond_move(top) ) &&
	      TNs_Are_Equivalent( OP_result(op,0), OP_result(succ_op,0) ) )
	    return TRUE;
	}
      }
    }

    /* Return TRUE if <succ_op> can change rflags, and <pred_op> will take
       rflags as one of the opnds.
    */
    if( TOP_is_change_rflags( OP_code(succ_op) ) || succ_is_lea ){
      if( OP_reads_rflags( pred_op ) )
	return TRUE;
    }
  }

  /* Return TRUE for case where <succ_op>, like div, will over-write
     some registers implicitly, and these registers are used by <pred_op> also.
     However, there other way around will be the wrong code sequence introduced
     by some other optimizations, like ebo.
  */
  {
    for( int i = 0; i < OP_opnds(succ_op); i++ ){
      TN* tmp_tn = CGTARG_Gen_Dedicated_Subclass_TN( succ_op, i, FALSE );
      if( tmp_tn != NULL ){
	// for RAW
	if( OP_Writes_Dedicated_TN( pred_op, tmp_tn ) )
	  return TRUE;

	// For an operation like shift whose operand requires a specific
	// dedicated register R, Preallocate_Single_Register_Subclasses() will
	// insert a move to copy the original operand into R and replace the
	// operand with R.  Thus, an implicit WAR exists between PRED_OP and
	// SUCC_OP.  Bug 352.
	if (OP_Reads_Dedicated_TN(pred_op, tmp_tn)) {
	  // SUCC_OP's operand is not yet a dedicated register.  A copy will be
	  // inserted.
	  if (!TN_is_dedicated(OP_opnd(succ_op, i)))
	    return TRUE;

	  // SUCC_OP's operand is not the correct dedicated register.  A copy
	  // will be inserted.  Bug 11781.
	  else if (TN_register(OP_opnd(succ_op, i)) != TN_register(tmp_tn))
	    return TRUE;
	}
      }
    }

    for( int i = 0; i < OP_results(succ_op); i++ ){
      TN* tmp_tn = CGTARG_Gen_Dedicated_Subclass_TN( succ_op, i, TRUE );
      if( tmp_tn != NULL ){
	// for WAW or WAR
	if( OP_Writes_Dedicated_TN( pred_op, tmp_tn ) ||
	    OP_Reads_Dedicated_TN( pred_op, tmp_tn ) ){
	  return TRUE;
	}
      }
    }
  }

  /* Misc. requirements.
   */
  {
    if (OP_code(succ_op) == TOP_savexmms)
      return TRUE;

    if (OP_code(succ_op) == TOP_leave)
      return TRUE;

    /* Under -fpic -m32, do not re-schedule operations that compute GOT.
     */
    if( OP_computes_got( pred_op ) ||
	OP_computes_got( succ_op ) )
      return TRUE;
  }

  /* Since a test operation will be removed, one more checking is required.
   */
  {
    if( TOP_is_change_rflags( OP_code(pred_op) ) &&
	TOP_is_change_rflags( OP_code(succ_op) ) ){
      for( OP* next = OP_next(succ_op); next != NULL; next = OP_next(next) ){
	if( OP_reads_rflags(next) )
	  return TRUE;
	if( TOP_is_change_rflags( OP_code(next) ) )
	  break;
      }
    }   
  }

  return FALSE;
}


/* ====================================================================
 *
 * CGTARG_Adjust_Latency
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Adjust_Latency(OP *pred_op, OP *succ_op, CG_DEP_KIND kind, UINT8 opnd, INT *latency)
{
  // Add latency between pointer load and pointer use.
  if (CG_ptr_load_use_latency != 0 &&
      kind == CG_DEP_REGIN &&
      (OP_load(pred_op) ||
       // Load-execute computes pointer, treat it like a pointer load.
       OP_load_exe(pred_op)) &&
      // Detect pointer use.
      (OP_load(succ_op) ||
       OP_store(succ_op) ||
       OP_load_exe(succ_op) ||
       OP_load_exe_store(succ_op))) {
    int base_idx = OP_find_opnd_use(succ_op, OU_base);
    int index_idx = OP_find_opnd_use(succ_op, OU_index);
    if (opnd == base_idx ||
	opnd == index_idx) {
      *latency = MAX(*latency,
		     TI_LATENCY_Result_Available_Cycle(OP_code(pred_op), 0)
		       + CG_ptr_load_use_latency);
    }
  }
}

/* ====================================================================
 *
 * CGTARG_Generate_Remainder_Branch
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Generate_Remainder_Branch(TN *trip_count, TN *label_tn,
				 OPS *prolog_ops, OPS *body_ops)
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}


/* ====================================================================
 *
 * CGTARG_OP_is_counted_loop
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL CGTARG_OP_is_counted_loop(OP *op) 
{
  return FALSE;
}


/* ======================================================================
 *
 * Loop_Countsdown_Xformed
 *
 * Return TRUE if either of the following conditions is TRUE and FALSE
 * otherwise.
 * (1) If the loop is not amenable to counts down loop tranformation
 *     (the branching condition is not a jne (upperbound)
 * (2) If the loop has already been thru this tranformation (as a result
 *     of which one may find a dec type instruction preceding the branch.
 *
 * ======================================================================
 */
static BOOL
Loop_Countsdown_Xformed ( BB* bb )
{
  OP* op;

  if ( OP_code( BB_branch_op( bb ) ) != TOP_jne )
    return TRUE;

  // Look back at the last op that sets the rflags for this conditional 
  // branch. 
  for ( op = BB_branch_op( bb ); op != NULL; op = op->prev )
    if ( TOP_is_change_rflags( OP_code( op ) ) )
      break;
  if ( op == NULL /* the paranoid case */ || 
       ( OP_code( op ) == TOP_dec32 && 
	 OP_code( op ) == TOP_dec64 ) /* already processed */ )
    return TRUE;

  return FALSE;
}

extern BOOL TN_live_out_of( TN*, BB* );

static BOOL CGTARG_live_out_of ( TN* tn, BB* tail, LOOP_DESCR *loop )
{
  BB* bb;
  OP* op;
  INT opnd, result;

  if ( TN_live_out_of( tn, tail ) ) {
    if ( TN_live_out_of( tn, BB_next( tail ) ) )
      return TRUE;

    FOR_ALL_BB_SET_members( LOOP_DESCR_bbset(loop), bb ) {
      if (bb == tail)
	break;
      FOR_ALL_BB_OPs ( bb, op ) {
	for ( opnd = 0; opnd < OP_opnds( op ); opnd ++ )
	  if ( OP_opnd( op, opnd ) == tn )
	    return TRUE;
	for ( result = 0; result < OP_results( op ); result ++ )
	  if ( OP_result( op, result ) == tn )
	    return TRUE;
      }
    }
  }
      
  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Generate_Countdown_Loop
 *
 * Tranform a loop that looks like:
 *    do i = 0, N, i ++
 *      body
 *    enddo
 * to:
 *    do i = N, 0, i --
 *	body
 *    enddo
 * The advantage of this tranformation is two-fold:
 *    (1) we can make use of the dec instruction in x86 and get rid of 
 *        extra moves/loop control variable update.
 *    (2) free a register used as a temporary to hold the loop
 *        control variable (instead we count down the upper bound).
 *
 * - inspired by bug 1254.
 *
 * ====================================================================
 */
void
CGTARG_Generate_Countdown_Loop ( TN *trip_count_tn,
				 BB *tail, 
				 OPS *prolog_ops,
				 OPS *body_ops, 
				 BOOL single_bb, 
				 LOOP_DESCR *loop )
{ 
  OP *cmp, *incr, *op, *branch;
  BOOL cmp_found = FALSE, incr_found = FALSE;
  INT opnd, result;

  if (!CG_LOOP_cloop)
    return;

  if (Loop_Countsdown_Xformed(tail))
    return;
  
  branch = BB_branch_op(tail);
  incr = cmp = NULL;

  // Find the compare operation that sets the rflags preceding the 
  // branch instruction. Also, find the loop increment instruction.
  for ( op = branch->prev; op != NULL; op = op->prev ) {
    if (!cmp_found) {
      // Quit if the comparion before the branch is actually a "test".  
      if (OP_code(op) == TOP_test32 || OP_code(op) == TOP_test64 || 
          OP_code(op) == TOP_test8 || OP_code(op) == TOP_test16 )
	return;

      if (OP_code(op) == TOP_cmp32 || OP_code(op) == TOP_cmp64 ||
          OP_code(op) == TOP_cmp8 || OP_code(op) == TOP_cmp16 ) {
	cmp_found = TRUE;
	cmp = op;
      }
    }

    if ( !incr_found && cmp_found &&
	 ( OP_code( op ) == TOP_addi32 ||
	   OP_code( op ) == TOP_addi64 ) && 
	 OP_opnd( op, 0 ) == OP_result( op, 0 ) /* defopnd */ && 
	 ( OP_result( op, 0 ) == OP_opnd( cmp, 0 ) ||
	   OP_result( op, 0 ) == OP_opnd( cmp, 1 ) ) &&
	 !CGTARG_live_out_of( OP_result( op, 0 ), tail, loop ) &&
	 TN_is_constant( OP_opnd( op, 1 ) ) &&
	 TN_value( OP_opnd ( op, 1 ) ) == 1 ) {
      incr_found = TRUE;
      incr = op;
    }
  }

  if (!incr_found || !cmp_found)
    return;

  // Safety checks.  Note that OPs that define/use incr_result can appear
  // anywhere before and after the incr OP.
  TN *incr_result = OP_result(incr, 0);
  for (op = branch->prev; op != NULL; op = op->prev) {
    if (op == cmp || op == incr)
      continue;

    if (OP_result(op, 0 ) == OP_opnd(cmp, 0) ||
	OP_result(op, 0 ) == OP_opnd(cmp, 1))
      return;

    // Make sure the operands to the cmp op is not one of those registers
    // written out to memory every iteration of the loop.
    if (TOP_is_store(OP_code(op)) &&
	(OP_opnd(op, 0) == OP_opnd(cmp, 0) ||
	 OP_opnd(op, 0) == OP_opnd(cmp, 1)))
      return;
      
    for (opnd = 0; opnd < OP_opnds(op); opnd++)
      if (OP_opnd(op, opnd) == incr_result)
	return;

    for (result = 0; result < OP_results(op); result++)
      if (OP_result(op, result ) == incr_result)
	return;
  }

  // At this point, we know all we need to know to transform the
  // loop into a countdown loop.   

  // Borrowed from IA64
  INT32 trip_size = TN_size(trip_count_tn);
  TN *lc_tn_tmp;
  TN* lc_tn = Gen_Register_TN (ISA_REGISTER_CLASS_integer,
			       trip_size);

  if (TN_is_constant(trip_count_tn)) {
    lc_tn_tmp = Gen_Literal_TN(TN_value(trip_count_tn),
			       trip_size);
  } else {
    lc_tn_tmp = trip_count_tn;
  } 

  // workaround a Exp_COPY bug?
  if (TN_is_constant(lc_tn_tmp)) {
    TN *tmp_tn = Gen_Register_TN (ISA_REGISTER_CLASS_integer,
				  trip_size);
    Exp_COPY(tmp_tn, lc_tn_tmp, prolog_ops);
    lc_tn_tmp = tmp_tn;
  }

  Exp_COPY(lc_tn, lc_tn_tmp, prolog_ops);

  // New ops to be added to the tail BB.
  Build_OP( OP_code( cmp ) == TOP_cmp32 ? TOP_dec32 : TOP_dec64, 
	    lc_tn, lc_tn, body_ops );
  if ( single_bb ) {
    CG_LOOP_Init_Op( OPS_last( body_ops ) );
    Set_OP_omega( OPS_last( body_ops ), 0, 1 );
  }
  Build_OP ( OP_code( branch ), OP_opnd( branch, 0 ), 
	     OP_opnd( branch, 1 ), body_ops );
  if ( single_bb ) {
    CG_LOOP_Init_Op( OPS_last( body_ops ) );
    Set_OP_omega( OPS_last( body_ops ), 0, OP_omega( branch, 0));
    Set_OP_omega( OPS_last( body_ops ), 1, OP_omega( branch, 1));
  }

  // Old ops to be removed from the tail BB.
  BB_Remove_Op( tail, incr );
  BB_Remove_Op( tail, cmp );
  BB_Remove_Op( tail, branch );

  return;  
}

STACK<OP*> Working_Set(Malloc_Mem_Pool);

static inline TN* OP_opnd_use( OP* op, ISA_OPERAND_USE use )
{
  const int indx = OP_find_opnd_use( op, use );
  return ( indx >= 0 ) ? OP_opnd( op, indx ) : NULL;
}

static TOP Movnti_Top(TOP old_top)
{
   switch(old_top){
    case TOP_stapd:     return TOP_stntpd;   break;
    case TOP_stapdx:    return TOP_stntpdx;  break;
    case TOP_stapdxx:   return TOP_stntpdxx; break;
    case TOP_staps:     return TOP_stntps;   break;
    case TOP_stapsx:    return TOP_stntpsx;  break;
    case TOP_stapsxx:   return TOP_stntpsxx; break;
    case TOP_stdqa:     return TOP_stntpd;   break;
    case TOP_stdqax:    return TOP_stntpdx;  break;
    case TOP_stdqaxx:   return TOP_stntpdxx; break;
    case TOP_vstapd:    return TOP_vstntpd;   break;
    case TOP_vstapdx:   return TOP_vstntpdx;  break;
    case TOP_vstapdxx:  return TOP_vstntpdxx; break;
    case TOP_vstaps:    return TOP_vstntps;   break;
    case TOP_vstapsx:   return TOP_vstntpsx;  break;
    case TOP_vstapsxx:  return TOP_vstntpsxx; break;
    case TOP_vstdqa:    return TOP_vstntpd;   break;
    case TOP_vstdqax:   return TOP_vstntpdx;  break;
    case TOP_vstdqaxx:  return TOP_vstntpdxx; break;

    case TOP_store32:   return TOP_storenti32;   break;
    case TOP_storex32:  return TOP_storentix32;  break;
    case TOP_storexx32: return TOP_storentixx32; break;

    case TOP_store64:   return TOP_storenti64;   break;
    case TOP_storex64:  return TOP_storentix64;  break;
    case TOP_storexx64: return TOP_storentixx64; break;

    case TOP_stss:      return TOP_stntss; break;
    case TOP_stssx:     return TOP_stntssx; break;
    case TOP_stssxx:    return TOP_stntssxx; break;
    case TOP_stsd:      return TOP_stntsd; break;
    case TOP_stsdx:     return TOP_stntsdx; break;
    case TOP_stsdxx:    return TOP_stntsdxx; break;

    /* following cases asserted by CG_movnti == UNCONDITIONAL_MOVNTI */
    case TOP_stups:     return TOP_stntps; break;
    case TOP_stupsx:    return TOP_stntpsx; break;
    case TOP_stupsxx:   return TOP_stntpsxx; break;
    case TOP_vstups:    return TOP_vstntps; break;
    case TOP_vstupsx:   return TOP_vstntpsx; break;
    case TOP_vstupsxx:  return TOP_vstntpsxx; break;
    case TOP_stupd:     return TOP_stntpd; break;
    case TOP_stupdx:    return TOP_stntpdx; break;
    case TOP_stupdxx:   return TOP_stntpdxx; break;
    case TOP_vstupd:    return TOP_vstntpd; break;
    case TOP_vstupdx:   return TOP_vstntpdx; break;
    case TOP_vstupdxx:  return TOP_vstntpdxx; break;
    }
   FmtAssert(FALSE,("Non-Temporal Store: not supported!"));
   return TOP_UNDEFINED;
}

// Bug 3774 - Compute total working set size by counting
// all disjoint source and destination bytes in the loop.
BOOL Op_In_Working_Set ( OP* op )
{
  /* address = ofst + base + index * scale */
  struct ADDRESS_COMPONENT {
    TN* index;
    TN* base;
    TN* offset;
    TN* scale;
  } a, b;

  bzero( &a, sizeof(a) );
  a.scale  = OP_opnd_use( op, OU_scale );
  a.base   = OP_opnd_use( op, OU_base );
  a.index  = OP_opnd_use( op, OU_index );
  a.offset = OP_opnd_use( op, OU_offset );
  if (a.scale == NULL)
    a.scale = Gen_Literal_TN( 1, 4 );

  for (INT i = 0; i < Working_Set.Elements(); i++) {
    OP* last = Working_Set.Top_nth(i);

    bzero( &b, sizeof(b) );
    b.scale  = OP_opnd_use( last, OU_scale );
    b.base   = OP_opnd_use( last, OU_base );
    b.index  = OP_opnd_use( last, OU_index );
    b.offset = OP_opnd_use( last, OU_offset );    
    if (b.scale == NULL)
      b.scale = Gen_Literal_TN(1, 4);

    if (((a.base && b.base && TNs_Are_Equivalent(b.base, a.base)) ||
         (!a.base && !b.base)) &&
	((a.index && b.index && TNs_Are_Equivalent(b.index, a.index)) ||
	 (!a.index && !b.index)) && 	
	TN_value(b.offset) == TN_value(a.offset) &&
	TN_value(b.scale) == TN_value(a.scale))
      return TRUE;
  }
  
  Working_Set.Push(op);
  return FALSE;
}

/* Convert temporal stores to non-temporal stores if the amount of data that
   <loop> will access is larger than the cache can provide.
 */
void CGTARG_LOOP_Optimize( LOOP_DESCR* loop )
{
  if(CG_movnti==0) return;

  BB* body = LOOP_DESCR_loophead(loop);
  OP* op = NULL;

  if (CG_movnti != UNCONDITIONAL_MOVNTI)
  {
    UINT32 trip_count = 0;
    TN* trip_count_tn = CG_LOOP_Trip_Count(loop);

    if( trip_count_tn != NULL &&
        TN_is_constant(trip_count_tn) ){
      trip_count = TN_value( trip_count_tn );

    } else {
      const ANNOTATION* annot = ANNOT_Get(BB_annotations(body), ANNOT_LOOPINFO);
      const LOOPINFO* info = NULL;
      if (annot != NULL)
        info = ANNOT_loopinfo(annot);
      if (info != NULL)
        trip_count = WN_loop_trip_est(LOOPINFO_wn(info));
    }

    INT64 size = 0;

    Working_Set.Clear();

    /* First, estimate the totol size (in bytes) that this loop will
       bring to the cache.
    */
    FOR_ALL_BB_OPs_FWD( body, op ){
      if(((OP_store( op ) && !TOP_is_nt_store(OP_code(op))) || //stores
  	  OP_load(op) ) && //loads
        !Op_In_Working_Set(op)){ //that were not in the working set
        size += CGTARG_Mem_Ref_Bytes(op);
      }
    }

    size *= trip_count;

    const INT64 cache_size = CG_movnti * 1024;

    if( size < cache_size )
      return;
  }
  // if CG_movnti == UNCONDITIONAL_MOVNTI:  generate non-temporal stores
  // unconditionally

  FOR_ALL_BB_OPs_FWD( body, op ){
    if( OP_prefetch( op ) ){
      /* Get rid of any prefetchw operation, because it "loads the prefetched
	 line and sets the cache-line state to Modified, in anticipation
	 of subsequent data writes to the line." In our case here, we
	 do not anticipate the data will be used in the near future.
       */
      const ISA_ENUM_CLASS_VALUE pfhint = TN_enum( OP_opnd(op,0) );
      if( pfhint == ECV_pfhint_L1_store )
	OP_Change_To_Noop( op );
      
      // Bug 5280 - prefetch ahead by 10 lines automatically if 
      // stores are to be converted to non-temporal stores.
      // Assumes cache line size is 64 bytes.
      else if ( pfhint == ECV_pfhint_L1_L2_load && LNO_Prefetch_Ahead == 2 ) {
	INT opnd_num = OP_find_opnd_use(op, OU_offset);
	if (opnd_num >= 0 &&
	    TN_has_value(OP_opnd(op, opnd_num))) {
	  TN *tn = OP_opnd(op, opnd_num);
	  Set_OP_opnd(op, opnd_num,
		      Gen_Literal_TN(TN_value(tn) + 64*8, TN_size(tn)));
	}
      }

      if ( pfhint == ECV_pfhint_L1_L2_load ) {
	switch(OP_code(op)) {
	case TOP_prefetcht0:   OP_Change_Opcode(op, TOP_prefetchnta); break;
	case TOP_prefetcht0x:  OP_Change_Opcode(op, TOP_prefetchntax); break;
	case TOP_prefetcht0xx: OP_Change_Opcode(op, TOP_prefetchntaxx); break;
        case TOP_prefetchnta:
        case TOP_prefetchntax:
        case TOP_prefetchntaxx: break; //do nothing             
	default: FmtAssert(FALSE, ("NYI"));
	}
      }	

      continue;
    }

    /* Change a store to a non-temporal store.

       Perform the change only if the store doesn't involve a cache line that
       is reused.  Detect reuse by looking for OPs that load the same memory
       location.  It doesn't matter if the load is before or after the store,
       since the load will involve the same cache line.  Bug 11853. */
 /* temporarily disable this changeset for two reasons:
  (1) we may need to consider load forwarding (even though there are reuses, no
      cache line is required)
  (2) this change provents performance tuning using -CG:movnti in some case 
      (bug 12036)
 */
    TOP new_top = TOP_UNDEFINED;
    switch( OP_code(op) ){
    //SSE support
    case TOP_staps:
    case TOP_stapsx:
    case TOP_stapsxx:
    case TOP_vstaps:
    case TOP_vstapsx:
    case TOP_vstapsxx: 
       {
         new_top = Movnti_Top(OP_code(op));
         break;
       }
    case TOP_stups:
    case TOP_stupsx:
    case TOP_stupsxx:
    case TOP_vstups:
    case TOP_vstupsx:
    case TOP_vstupsxx:
       {
         if (CG_movnti == UNCONDITIONAL_MOVNTI)
           new_top = Movnti_Top(OP_code(op));
         break;
       }
    //SSE2 support
    case TOP_stapd:
    case TOP_stapdx:
    case TOP_stapdxx:
    case TOP_stdqa:
    case TOP_stdqax:
    case TOP_stdqaxx:
    case TOP_vstapd:
    case TOP_vstapdx:
    case TOP_vstapdxx:
    case TOP_vstdqa:
    case TOP_vstdqax:
    case TOP_vstdqaxx:
    case TOP_store32:
    case TOP_storex32:
    case TOP_storexx32:
    case TOP_store64:
    case TOP_storex64:
    case TOP_storexx64: {
// Bug 14393 : TOP_is_vector_op restriction added
         if(Is_Target_SSE2() && TOP_is_vector_op(OP_code(op)))
	  new_top = Movnti_Top(OP_code(op));
         break;
       }
    //SSE4a/SSE41 support
    case TOP_stss:
    case TOP_stssx:
    case TOP_stssxx:
    case TOP_stsd:
    case TOP_stsdx:
    case TOP_stsdxx:
       { 
         if(Is_Target_SSE4a() || Is_Target_SSE41())
            new_top = Movnti_Top(OP_code(op));
         break;
       }
    case TOP_stupd:
    case TOP_stupdx:
    case TOP_stupdxx:
    case TOP_vstupd:
    case TOP_vstupdx:
    case TOP_vstupdxx:
       {
         if (CG_movnti == UNCONDITIONAL_MOVNTI &&
             (Is_Target_SSE4a() || Is_Target_SSE41()))
           new_top = Movnti_Top(OP_code(op));
         break;
       }
    }//end switch
    
   if( new_top != TOP_UNDEFINED )
      OP_Change_Opcode( op, new_top );
  }
}


/* ====================================================================
 *
 * CGTARG_Generate_Branch_Cloop
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Generate_Branch_Cloop(OP *br_op,
			     TN *unrolled_trip_count,
			     TN *trip_count_tn,
			     INT32 ntimes,
			     TN *label_tn,
			     OPS *prolog_ops,
			     OPS *body_ops)
{ 
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}

static TN* asm_constraint_tn[10];
static ISA_REGISTER_SUBCLASS asm_constraint_sc[10];
static char asm_constraint_name[10][8];
static INT asm_constraint_index;

// must be called once per asm
void
CGTARG_Init_Asm_Constraints (void)
{
  // can use any type; it will be ignored
  Setup_Output_Parameter_Locations (MTYPE_To_TY(MTYPE_I8));
  for (INT i = 0; i < 10; ++i) {
    asm_constraint_tn[i] = NULL;
    asm_constraint_sc[i] = ISA_REGISTER_SUBCLASS_UNDEFINED;
    asm_constraint_name[i][0] = '\0';
  }
  asm_constraint_index = 0;
}

#define CONST_OK_FOR_LETTER(VALUE, C)                           \
  ((C) == 'I' ? (VALUE) >= 0 && (VALUE) <= 31                   \
   : (C) == 'J' ? (VALUE) >= 0 && (VALUE) <= 63                 \
   : (C) == 'K' ? (VALUE) >= -128 && (VALUE) <= 127             \
   : (C) == 'L' ? (VALUE) == 0xff || (VALUE) == 0xffff          \
   : (C) == 'M' ? (VALUE) >= 0 && (VALUE) <= 3                  \
   : (C) == 'N' ? (VALUE) >= 0 && (VALUE) <= 255                \
   : (C) == 'i' ? 1 \
   : (C) == 'n' ? 1                                             \
   : 0)

// -----------------------------------------------------------------------
// Given a constraint for an ASM parameter, and the load of the matching
// argument passed to ASM (possibly NULL), choose an appropriate TN for it
// -----------------------------------------------------------------------
extern TN* 
CGTARG_TN_For_Asm_Operand (const char* constraint, 
                           const WN* load,
                           TN* pref_tn,
                           ISA_REGISTER_SUBCLASS* subclass, 
                           const WN* asm_wn,
			   TYPE_ID type)
{
  // skip constraint modifiers:
  // = input and output parameters are separated in the WHIRL for ASM
  // & early_clobber flag is set in Handle_ASM
  // % commutativity of operands is ignored for now
  static const char* modifiers = "=&%";
  while (strchr(modifiers, *constraint))
  {
    constraint++;
  }

  const char* initial_constraint = constraint;
  
  // TODO: we should really look at multiple specified constraints
  // and the load in order to select the best TN, but for now we
  // assume that when we get here we can safely pick a TN

  // if 'm' or 'g' is one of the choices, always prefer that one
  // TODO: we decide this in the front end, but it's not optimal
  if (*constraint != 'm' && *constraint != 'g')
  {
    const char* m = constraint;
    while (*++m)
    {
      if (*m == 'm' || *m == 'g')
      {
        constraint = m;
        break;
      }
    }
  }
  
  // prefer register/memory over immediates; this isn't optimal, 
  // but we may not always be able to generate an immediate
  static const char* immediates = "inIJKLMNO";
  // Bug 950
  // The '#' in a constraint is inconsequential or it is just a typo
  static const char* hash = "#";
  if (!strstr(constraint, hash)) {
    while (strchr(immediates, *constraint) && *(constraint+1)) 
      {
	constraint++;
      }
  }

  // Bug 14409: In contrast to what the above comment says about
  // preferring register to immediates, immediates should always be
  // preferred if it is possible to generate one. Otherwise, if it
  // is a constant, it is not easy to decode any type cast it may have,
  // giving the possibility of a wrong register width.
  // See if there is an immediate constraint.
  const char * found_immediate = initial_constraint; // original constraint
  while (!strchr(immediates, *found_immediate) && *(found_immediate+1)) 
  {
    found_immediate++;
  }

  // Is there an immediate constraint, and is the load a constant?
  if (strchr(immediates, *found_immediate) &&
      WN_operator(load) == OPR_INTCONST)
    constraint = found_immediate; // generate an immediate

  TN* ret_tn;
  BOOL first = FALSE, second = FALSE, third = FALSE, fourth = FALSE;
  
  // TODO: check that the operand satisifies immediate range constraint
  if (strchr(immediates, *constraint))
  {
    if (load && WN_operator(load)==OPR_LDID && WN_class(load)==CLASS_PREG)
    {
      // immediate could have been put in preg by wopt
      if (Preg_Is_Rematerializable(WN_load_offset(load), NULL)) {
        load = Preg_Is_Rematerializable(WN_load_offset(load), NULL);
      }
      // shared load(lda) has been lifted to preg
      else if (Preg_Lda(WN_load_offset(load))) {
        load = Preg_Lda(WN_load_offset(load));
      }
      else {
        load = NULL;
      }
    }

    // bug916 open64.net, &var.field is allowed as "i"
    // bug962 open64.net, function symbol is allowed as "i"
    if (!(load && (WN_operator(load) == OPR_INTCONST ||
                       (WN_operator(load) == OPR_LDA &&
                        (ST_sym_class(WN_st(load)) == CLASS_VAR || 
                         ST_sym_class(WN_st(load)) == CLASS_CONST ||
                         ST_sym_class(WN_st(load)) == CLASS_FUNC
                         ))))) {
      ErrMsgSrcpos(EC_Invalid_Asm_Constrain, WN_Get_Linenum(asm_wn),
                    ": Cannot find immediate operand for ASM");
    }
    if (WN_operator(load) == OPR_INTCONST)
    {
      ret_tn = Gen_Literal_TN(WN_const_val(load), 
                              MTYPE_bit_size(WN_rtype(load))/8);
      // Bugs 3177, 3043 - safety check from gnu/config/i386/i386.h.
      if (!CONST_OK_FOR_LETTER(WN_const_val(load), *constraint)) {
        ErrMsgSrcpos(EC_Invalid_Asm_Constrain, WN_Get_Linenum(asm_wn),
                ": The value of immediate operand supplied is not within expected range.");
      }
      if (Is_Target_32bit() && (WN_const_val(load) > INT32_MAX || WN_const_val(load) < INT32_MIN)) {
        char c[200];
        sprintf(c,"%lld", WN_const_val(load));
        ErrMsgSrcpos(EC_Ill_Int_Oflow, WN_Get_Linenum(asm_wn),
                     INT32_MIN,c,INT32_MAX);
      }
    }
    else if (ST_sym_class(WN_st(load)) == CLASS_VAR) {
      ST *base;
      INT64 ofst;
      Base_Symbol_And_Offset_For_Addressing (WN_st(load), WN_lda_offset(load), &base, &ofst);
      ret_tn = Gen_Symbol_TN(base,ofst,0);
    }
    else
    {
      // Bug 14390: string constant with an immediate constraint
      ST * base;
      INT64 ofst;
      // Allocate the string to the rodata section
      Allocate_Object (WN_st(load));
      Base_Symbol_And_Offset (WN_st(load), &base, &ofst);
      ret_tn = Gen_Symbol_TN(base, ofst, 0);
    }
  }
  // digit constraint means that we should reuse a previous operand
  else if (isdigit(*constraint))
  {
    // TODO: make sure that frontend checks that string is number
    INT prev_index = strtol(constraint, NULL, 10);
    if (prev_index < 0 || prev_index >= asm_constraint_index ||
        ! asm_constraint_tn[prev_index] ) {
       FmtAssert( FALSE, ("invalid matching constraint reference") );
    }
    ret_tn = asm_constraint_tn[prev_index];
  }
  else if (strchr("m", *constraint) || strchr("g", *constraint))
  {
    TYPE_ID rtype = (load != NULL ? WN_rtype(load) : MTYPE_I4);
    FmtAssert(MTYPE_is_integral(rtype),
              ("ASM operand does not satisfy its constraint"));
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(rtype));
  }
  else if ((*constraint == 'r') || (*constraint == 'a') ||
	   (*constraint == 'b') || (*constraint == 'v') ||
	   (*constraint == 'h') || (*constraint == 'l') ||
	   (*constraint == 'd') || (*constraint == 'c') ||
	   (*constraint == 'S') || (*constraint == 'D') || 
	   (*constraint == 'A') || (*constraint == 'q') || 
	   (*constraint == 'Q') || 
	   (*constraint == 'Z') ||  // bug 14413: handle 'Z' similar to 'e'.
	   (*constraint == 'e' && *(constraint+1) == 'r'))
  {
    TYPE_ID rtype;
    
    if (load != NULL) {
      // Bugs 482, 505, 626, 1045
      rtype = (WN_desc(load) == MTYPE_V) ? WN_rtype(load) : WN_desc(load);
      if (WN_operator(load) == OPR_CVTL) { // Bug 3223
	switch (WN_cvtl_bits(load)) { 
	// Don't care signed/unsigned but the width should be set right.
	case 32: rtype = MTYPE_U4; break;
	case 16: rtype = MTYPE_U2; break;
	case 8:  rtype = MTYPE_U1; break;
	default: FmtAssert(FALSE, ("NYI")); 
	}
      }
      else if (WN_operator(load) == OPR_CVT /* bug 14419 */ ||
               // CVT may have been folded into the load, but the rtype
               // cannot always be taken, for example, in U4U2LDID.
               (WN_operator(load) == OPR_LDID &&
                WN_desc(load) == MTYPE_U4)) // bug 14427
        rtype = WN_rtype(load);
    } else {
      /* We can't figure out what type the operand is, probably because the
	 optimizer deleted the references to it, so return some default type */      
      rtype = type /* from PRAGMA preg type */;
    }
    if (pref_tn)
      ret_tn = pref_tn;
    else if ((*constraint == 'a') || (*constraint == 'b') ||
	     (*constraint == 'c') || (*constraint == 'd') ||
	     (*constraint == 'S') || (*constraint == 'D') || 
	     (*constraint == 'A')) {
      REGISTER reg;
      switch (*constraint) {
      case 'a': reg = RAX; break;
      case 'b': reg = RBX; break;
      case 'c': reg = RCX; break;
      case 'd': reg = RDX; break;
      case 'S': reg = RSI; break;
      case 'D': reg = RDI; break;
      // TODO: handle 'A' constraint as a register pair for x86
      case 'A': reg = RAX; break;       
      }
      ret_tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, reg, 
      				  // Bug 1880 - now that we have dedicated 
				  // TNs of sizes < 4 bytes, use the appropriate
				  // size.
				  MTYPE_byte_size(rtype));
    } else     
      ret_tn = Build_TN_Of_Mtype(rtype);

    if (*constraint == 'q' || *constraint == 'Q') {
      // For m64, although all 16 integer registers can be addressed as 8-bit
      // registers, use only AX/BX/CX/DX for the 8-bit 'q' constraint.  An ASM,
      // such as "%h1", may ask for the second least significant byte of the
      // register to be the 8-bit register, for example "ah".  Only AX/BX/CX/DX
      // allows this type of addressing.  Bug 9598.
      *subclass = ISA_REGISTER_SUBCLASS_m32_8bit_regs;
    }
  }
  else if (*constraint == 't' || *constraint == 'u')
  {
    // It is really the user's responsibility to do any type conversions between
    // long double and double/float. We will not handle any type conversions
    // as part of handling the inline asm. This is what Gcc does.
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(MTYPE_F10));
  }
  else if (*constraint == 'f')
  {
    FmtAssert(FALSE, ("Asm constraint <f> requires x87 implementation"));
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(MTYPE_F4));
  }
  else if (*constraint == 'x')
  {
    TYPE_ID mtype = MTYPE_F4;    
    if (load)
      mtype = WN_rtype(load);
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(mtype));
  }
  else if (*constraint == 'X') // bug 11884
  {
    TYPE_ID mtype = MTYPE_I4;    
    if (load)
      mtype = WN_rtype(load);
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(mtype));
  }
  else if (*constraint == 'Y')
  {
    TYPE_ID mtype = MTYPE_F8;    
    if (load)
      mtype = WN_rtype(load);
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(mtype));
  }
  else if (*constraint == 'y')
  {
    if(Is_Target_32bit())
      ret_tn = (pref_tn ? pref_tn : Build_RCLASS_TN(ISA_REGISTER_CLASS_mmx));
    else
      ret_tn = (pref_tn ? pref_tn : Build_RCLASS_TN(ISA_REGISTER_CLASS_float));
  }
  else if (*constraint == 'p') 
  {
    FmtAssert(load, (" there must be load expression for constraint p\n"));
    if (load && WN_operator(load)==OPR_LDID && WN_class(load)==CLASS_PREG)
    {
      // immediate could have been put in preg by wopt
      if (Preg_Is_Rematerializable(WN_load_offset(load), NULL)) {
        load = Preg_Is_Rematerializable(WN_load_offset(load), NULL);
      }
      // shared load(lda) has been lifted to preg
      else if (Preg_Lda(WN_load_offset(load))) {
        load = Preg_Lda(WN_load_offset(load));
      }
      else {
        load = NULL;
      }
    }
    if (WN_operator(load) == OPR_INTCONST)
    {
      if (Is_Target_32bit() && (WN_const_val(load) > INT32_MAX || WN_const_val(load) < INT32_MIN)) {
        char c[200];
        sprintf(c,"%lld", WN_const_val(load));
        ErrMsgSrcpos(EC_Ill_Int_Oflow, WN_Get_Linenum(asm_wn),
                     INT32_MIN,c,INT32_MAX);
      }
      ret_tn = Gen_Literal_TN(WN_const_val(load), 
                              MTYPE_bit_size(WN_rtype(load))/8);
    }
    else if ( WN_operator(load) == OPR_LDA && ST_sym_class(WN_st(load)) == CLASS_CONST) 
    {
      ST * base;
      INT64 ofst;
      // Allocate the string to the rodata section
      Allocate_Object (WN_st(load));
      Base_Symbol_And_Offset (WN_st(load), &base, &ofst);
      ret_tn = Gen_Symbol_TN(base, ofst, 0);
    }
    else if ( WN_operator(load) == OPR_LDA && ST_sym_class(WN_st(load)) == CLASS_VAR)
    {
      ST * base;
      INT64 ofst;
      Base_Symbol_And_Offset_For_Addressing (WN_st(load), WN_lda_offset(load), &base, &ofst);
      if (ofst == 0) { //no offset, using a symbol TN
        ret_tn = Gen_Symbol_TN(base, ofst, 0);
      } else { //has a offset, using a new TN to express the address
        ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(WN_rtype(load)));
      }
    } 
    else //other cases
    { 
      ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(WN_rtype(load)));
    }
  }
  else
  {
    FmtAssert(FALSE, ("ASM constraint <%s> not supported", constraint));
  }

  asm_constraint_tn[asm_constraint_index] = ret_tn;
  asm_constraint_index++;
  
  return ret_tn;
}


static const char *
Get_TN_Assembly_Name (TN *tn)
{
  return "moo";
}

void
CGTARG_TN_And_Name_For_Asm_Constraint (char *constraint, TYPE_ID mtype,
	TYPE_ID desc, TN **tn, const char **name)
{
	INT i;
	if (*constraint == '=') {
		// ignore
		CGTARG_TN_And_Name_For_Asm_Constraint (constraint+1, 
			mtype, desc, tn, name);
		return;
	}
	if (mtype == MTYPE_V) {
		// unknown parameter, so pick mtype from constraint
		if (*constraint == 'f') mtype = MTYPE_F8;
		else mtype = MTYPE_I8;
	}
	switch (*constraint) {
	case 'r':
		FmtAssert(MTYPE_is_integral(mtype), 
			("ASM constraint is integer but parameter is not"));
		break;
	case 'f':
		FmtAssert(MTYPE_is_float(mtype), 
			("ASM constraint is float but parameter is not"));
		break;
	case 'm':
	case 'g':
		break;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9': {
                i = strtol(constraint, NULL, 10);
                if (i < 0 || i >= asm_constraint_index || ! asm_constraint_tn[i] ) {
                    FmtAssert( FALSE, ("invalid matching constraint reference") );
                }

                *tn = asm_constraint_tn[i];
                asm_constraint_tn[asm_constraint_index] = *tn;

                *name = asm_constraint_name[i];
                strcpy(asm_constraint_name[asm_constraint_index],*name);

		++asm_constraint_index;
		return;
        }
	case 'i':
		// let caller figure out the name
		*tn = NULL;
		*name = NULL;
		return;
	default:
		FmtAssert(FALSE, ("ASM constraint <%s> not supported", constraint));
	}
	PLOC ploc = Get_Output_Parameter_Location (MTYPE_To_TY(mtype));
	*tn = PREG_To_TN (MTYPE_To_PREG(mtype), PLOC_reg(ploc));
	asm_constraint_tn[asm_constraint_index] = *tn;
	*name = Get_TN_Assembly_Name(*tn);
	if (*constraint == 'm' || *constraint == 'g') {
	    	sprintf(asm_constraint_name[asm_constraint_index], "[%s]", 
			*name);
	} else {
	    	sprintf(asm_constraint_name[asm_constraint_index], "%s", 
			*name);
	}

	*name = asm_constraint_name[asm_constraint_index];
	++asm_constraint_index;
}


/* ====================================================================
 * target specific modifiers for printing different versions
 * of register names when they appear as AM operands 
 * ====================================================================
 */
char CGTARG_Asm_Opnd_Modifiers[] = { 'r' };
INT  CGTARG_Num_Asm_Opnd_Modifiers = 1;

static const char* x86_reg_names[4][16] = {
  /* BYTE_REG: low 8-bit */
  { "%al", "%bl", "%bpl", "%spl", "%dil", "%sil", "%dl", "%cl",
    "%r8b",  "%r9b",  "%r10b", "%r11b", "%r12b", "%r13b", "%r14b", "%r15b" },
  /* WORD_REG: 16-bit */
  { "%ax", "%bx", "%bp", "%sp", "%di", "%si", "%dx", "%cx",
    "%r8w",  "%r9w",  "%r10w", "%r11w", "%r12w", "%r13w", "%r14w", "%r15w" },
  /* DWORD_REG: 32-bit */
  { "%eax", "%ebx", "%ebp", "%esp", "%edi", "%esi", "%edx", "%ecx",
    "%r8d",  "%r9d",  "%r10d", "%r11d", "%r12d", "%r13d", "%r14d", "%r15d" },
  /* SSE2_REG: 128-bit */
  { "%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7",
    "%xmm8", "%xmm9", "%xmm10", "%xmm11", "%xmm12", "%xmm13", "%xmm14", "%xmm15" },
};

const char* 
CGTARG_Modified_Asm_Opnd_Name(char modifier, TN* tn, char *tn_name)
{
  if (TN_register_class(tn) == ISA_REGISTER_CLASS_x87)
    return tn_name;

  if (modifier == 'r') {
    REGISTER reg;
    const ISA_REGISTER_CLASS rc = TN_register_class(tn);
    for( reg = REGISTER_MIN; reg <= REGISTER_CLASS_last_register( rc ); reg++ ){
      const char* n = REGISTER_name( rc, reg );
      if( strcmp( n, tn_name ) == 0 ){
	break;
      }
    }

    if ( rc == ISA_REGISTER_CLASS_integer ) {
      switch(TN_size(tn)) {
      case 1:
        return x86_reg_names[0][reg - REGISTER_MIN];
      case 2:
        return x86_reg_names[1][reg - REGISTER_MIN];
      case 4:
        return x86_reg_names[2][reg - REGISTER_MIN];
      default:
        FmtAssert(TN_size(tn) == 8, ("Bad TN size for integer"));
        return tn_name;
      }
    }
    else if ( rc == ISA_REGISTER_CLASS_float ) {
      if ( TN_size(tn) == 32 ) 
        return tn_name;
      else
        return x86_reg_names[3][reg - REGISTER_MIN];
    }
    else {
      FmtAssert(FALSE, ("Unknown register class"));
    }
  }
  else {
    FmtAssert(FALSE, ("Unknown ASM operand modifier '%c'", modifier));
  }
  /*NOTREACHED*/
}

/* For the "m" constraint, there is no need to introduce op like
           TN108 :- lea32 TN4(%rsp) (sym:size+0)
   otherwise, all the registers will be used up pretty soon.
   The fix here is to remove the op that computes the address,
   and put the offset tn into opnd[num_opnds]. According this
   offset tn, later phase in Modify_Asm_String will generate
   the right offset and base info.   (bug#3111)
*/
TN* CGTARG_Process_Asm_m_constraint( WN* load, void** offset, OPS* ops )
{
  Is_True( load != NULL, ("Asm_m_constraint: load is NULL") );
  TN* asm_opnd = NULL;

  if( WN_operator(load) == OPR_LDA ){
    OP* lda_op = OPS_last( ops );
    // open64.net bug951. On finding the symbol TN, don't miss the cases of:
    //  TN :- ld32 GTN2(%rbx) (sym:base_sym +0)
    // and 
    //  TN_tmp :- ld32 GTN2(%rbx) (sym:base_sym +0)
    //  TN :- lea32 TN_tmp ofst_TN
    // result_sym of the first case is
    // (sym:base_sym + 0), reloc: TN_RELOC_IA32_GOT
    // result_sym of the latter case is
    // (sym:base_sym + base_ofst), reloc: TN_RELOC_IA32_GOTOFF

    if (Is_Target_32bit() && Gen_PIC_Shared) {
      OP * prev_lda = OP_prev(lda_op);
      if (OP_code(lda_op) == TOP_lea32 &&
          prev_lda && 
          OP_code(prev_lda) == TOP_ld32 &&
          TN_is_constant(OP_opnd(lda_op, 1)) &&
          TN_register(OP_opnd(lda_op, 0)) == TN_register(OP_result(prev_lda, 0)) &&
          TN_is_symbol(OP_opnd(prev_lda, 1)) &&
          TN_relocs(OP_opnd(prev_lda, 1)) == TN_RELOC_IA32_GOT) {
        asm_opnd = Gen_Symbol_TN( TN_var(OP_opnd(prev_lda, 1)), 
                                  TN_value(OP_opnd(lda_op, 1)), 
                                  TN_RELOC_IA32_GOTOFF );
        OPS_Remove_Op (ops, prev_lda);
      } 
      else if (OP_code(lda_op) == TOP_ldc32) 
        asm_opnd = OP_opnd( lda_op, 0);
      else
        asm_opnd = OP_opnd( lda_op, 1 );
    } else {
      asm_opnd = OP_iadd(lda_op) ? OP_opnd( lda_op, 1 ) : OP_opnd( lda_op, 0 );
    }
    OPS_Remove_Op( ops, lda_op );

  } else if( WN_operator(load) == OPR_ADD ){
    OP* add_op = OPS_last( ops );
    TN* ofst_tn = OP_opnd( add_op, 1 );

    if( !TN_is_constant(ofst_tn) )
      return NULL;

    *offset = (void*)Gen_Literal_TN( TN_value(ofst_tn), 4 );

    asm_opnd = OP_opnd( add_op, 0 );
    OPS_Remove_Op( ops, add_op );

    /* Do some pattern matching to save one register by removing
       duplicated load.
    */

    OP* ld_op = OPS_last(ops);
    if( ld_op != NULL  &&
	OP_load(ld_op) &&
	OP_result(ld_op,0) == asm_opnd ){

      for( OP* prev_ld = OP_prev(ld_op);
	   prev_ld != NULL;
	   prev_ld = OP_prev(prev_ld) ){

	if( OP_store(prev_ld) )
	  break;

	if( OP_load(prev_ld) &&
	    OP_opnds(prev_ld) == OP_opnds(ld_op) ){
	  bool match = true;
	  for( int i = 0; i < OP_opnds(ld_op); i++ ){
	    if( OP_opnd(prev_ld,i) != OP_opnd(ld_op,i) ){
	      match = false;
	      break;
	    }
	  }

	  if( match ){
	    OPS_Remove_Op( ops, ld_op );
	    asm_opnd = OP_result( prev_ld, 0 );
	    break;
	  }
	}
      }
    }

  } else if( WN_operator(load) == OPR_LDID ){
    ;

  } else {
    DevWarn( "Asm_m_constraint: Unsupported opcode (%s)",
	     OPCODE_name(WN_opcode(load)) );
    return NULL;
  }

  return asm_opnd;
}

/* ====================================================================
 *
 * CGTARG_Postprocess_Asm_String: currently no-op for IA-64
 *
 * ====================================================================
 */
void 
CGTARG_Postprocess_Asm_String (char*)
{
}

/* ====================================================================
 *
 * CGTARG_Unconditional_Compare
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL CGTARG_Unconditional_Compare(OP *op, TOP* uncond_ver)
{
  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Invert_Branch
 *
 * See interface description
 *
 * ====================================================================
 */
TOP CGTARG_Invert_Branch(BB* bb)
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
  return TOP_UNDEFINED;
}


/* ====================================================================
 *
 * CGTARG_Init_OP_cond_def_kind
 *
 *  See interface description
 *
 * ====================================================================
 */
void CGTARG_Init_OP_cond_def_kind(OP *op)
{
  if( OP_has_predicate(op) ) {
    FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
  } else {
      {
	Set_OP_cond_def_kind(op, OP_ALWAYS_UNC_DEF);
      }
  }
}


/* ====================================================================
 *
 * CGTARG_Get_unc_Variant
 *
 *  Given a compare opcode, return the unconditional variant form. 
 *  Return the opcode if there is no such form.
 *
 * ====================================================================
 */
TOP CGTARG_Get_unc_Variant(TOP top)
{
  /* This doesn't seem to be used. */
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
  return TOP_UNDEFINED;
}

////////////////////////////////////////////////////////////////
// If a BB ends in an unconditional branch, turn it into a conditional branch 
// With TRUE predicate, so we can predicate with something else later.
// If we can't find an unconditional branch, just give up and do nothing
//
void
Make_Branch_Conditional(BB *bb)
{
  return;
}

/* ====================================================================
 *
 * CGTARG_Check_OP_For_HB_Suitability
 *
 * Returns TRUE if OP is a suitable candidate for HBF. Otherwise, return
 * FALSE.
 *
 * ====================================================================
 */
BOOL 
CGTARG_Check_OP_For_HB_Suitability(OP *op)
{
  switch(Eager_Level) {
  case EAGER_NONE:
    return FALSE;
  case EAGER_SAFE:
    if (OP_fadd(op) || 
	OP_fdiv(op) || 
	OP_fsub(op) || 
	OP_fmul(op) || 
	OP_load(op) ||
	OP_store(op) ||
	OP_prefetch(op) || 
	// idiv, imul use hilo registers
	OP_idiv(op) ||
	OP_imul(op) )
      return FALSE;
    else
      return TRUE;      
  case EAGER_ARITH:
    if (OP_load(op) ||
	OP_store(op) ||
	OP_prefetch(op) ||
	// Divide by zero
	OP_fdiv(op) || 
	OP_idiv(op) ||
	OP_imul(op) )
      return FALSE;
    else
      return TRUE;
  case EAGER_DIVIDE:
    if (OP_load(op) ||
	OP_store(op) ||
	OP_prefetch(op) ||
	OP_idiv(op) ||
	OP_imul(op) )
      return FALSE;
    else
      return TRUE;
  case EAGER_MEMORY:
  case EAGER_OTHER:
    if (OP_idiv(op) ||
	OP_imul(op) )
      return FALSE;
    else
      return TRUE;
  default:
    FmtAssert(FALSE, ("Handle this case"));
    return FALSE;
  }
}


TN* CGTARG_Gen_Dedicated_Subclass_TN( OP* op, int idx, BOOL is_result )
{
  // If idx is out of fixed number of operands or results, there is no 
  // register set info in the targ_info for the op. In this case, return 
  // tn if it is dedicated.
  int opnd_result_cnt = is_result ? OP_fixed_results(op) : OP_fixed_opnds(op);
  if (idx >= opnd_result_cnt) {
    TN* tn = is_result ? OP_result( op, idx ) : OP_opnd( op, idx );
    return TN_is_dedicated(tn) ? tn : NULL;
  }

  const ISA_REGISTER_SUBCLASS subclass = is_result ?
    OP_result_reg_subclass( op, idx ) : OP_opnd_reg_subclass( op, idx );
  const REGISTER_SET subclass_regs = REGISTER_SUBCLASS_members(subclass);

  if( REGISTER_SET_Size(subclass_regs) != 1 ){
    TN* tn = is_result ? OP_result( op, idx ) : OP_opnd( op, idx );
    return TN_is_dedicated(tn) ? tn : NULL;
  }

  const REGISTER reg = REGISTER_SET_Choose(subclass_regs);
  const ISA_REGISTER_CLASS rc = REGISTER_SUBCLASS_register_class(subclass);

  return Build_Dedicated_TN( rc, reg, 0 );
}


// Return TRUE if OP accesses thread-local memory.
BOOL
CGTARG_Is_Thread_Local_Memory_OP (OP* op)
{
  TOP code = OP_code(op);

  int base_opnd = TOP_Find_Operand_Use(code, OU_base);
  if (base_opnd != -1 &&
      TN_is_thread_seg_ptr(OP_opnd(op, base_opnd))) {
    return TRUE;
  }

  int offset_opnd = TOP_Find_Operand_Use(code, OU_offset);
  // if already has the base, do not append the TLS segment register
  if (offset_opnd != -1 && base_opnd == -1 && 
      (TN_relocs(OP_opnd(op, offset_opnd)) == TN_RELOC_X8664_TPOFF32 ||
       TN_relocs(OP_opnd(op, offset_opnd)) == TN_RELOC_X8664_TPOFF32_seg_reg)) {
    return TRUE;
  }

  return FALSE;
}
