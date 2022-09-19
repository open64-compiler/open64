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
// Generate pseudo instruction encoding and decoding information.
/////////////////////////////////////
/////////////////////////////////////
#include "topcode.h"
#include "isa_pseudo_gen.h"
 
main()
{
  ISA_Pseudo_Begin("ia64");

  /* ========== machine to pseudo instruction translations ========== */

  Machine_To_Pseudo(TOP_alloc, TOP_alloc_3);
  Map_Arg("RESULT(0)", "RESULT(0)");	// r1
  Map_Arg("OPND(0)", "0");		// i = 0
  Map_Arg("OPND(1)", "OPND(1)");	// l = sol
  Map_Arg("OPND(2)", "OPND(0) - OPND(1)"); // o = sof - sol
  Map_Arg("OPND(3)", "OPND(2)");	// r = sor

  Machine_To_Pseudo(TOP_br, TOP_br_cond);
  Require("OPND(0) == 0");		// qp == p0
  Require("OPND(1) == 0");		// bwh == sptk
  Map_Arg("OPND(0)", "OPND(2)");	// ph
  Map_Arg("OPND(1)", "OPND(3)");	// dh
  Map_Arg("OPND(2)", "OPND(4)");	// target_25

  Machine_To_Pseudo(TOP_br_r, TOP_br_r_cond);
  Require("OPND(0) == 0");		// qp == p0
  Require("OPND(1) == 0");		// bwh == sptk
  Map_Arg("OPND(0)", "OPND(2)");	// ph
  Map_Arg("OPND(1)", "OPND(3)");	// dh
  Map_Arg("OPND(2)", "OPND(4)");	// b2

  Machine_To_Pseudo(TOP_brl, TOP_brl_cond);
  Require("OPND(0) == 0");		// qp == p0
  Require("OPND(1) == 0");		// bwh == sptk
  Map_Arg("OPND(0)", "OPND(2)");	// ph
  Map_Arg("OPND(1)", "OPND(3)");	// dh
  Map_Arg("OPND(2)", "OPND(4)");	// target_64

  Machine_To_Pseudo(TOP_fabs, TOP_fmerge_s);
  Require("OPND(1) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(2)");	// f3

  Machine_To_Pseudo(TOP_fadd, TOP_fma);
  Require("OPND(3) == 1");		// f4
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3
  Map_Arg("OPND(3)", "OPND(4)");	// f2

  Machine_To_Pseudo(TOP_fadd_d, TOP_fma_d);
  Require("OPND(3) == 1");		// f4
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3
  Map_Arg("OPND(3)", "OPND(4)");	// f2

  Machine_To_Pseudo(TOP_fadd_s, TOP_fma_s);
  Require("OPND(3) == 1");		// f4
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3
  Map_Arg("OPND(3)", "OPND(4)");	// f2

  // TOP_fclass_nm
  // TOP_fclass_nm_unc
  //	These cannot be uniquely identified.

  // TOP_fcvt_xuf
  // TOP_fcvt_xuf_s
  // TOP_fcvt_xuf_d
  //	These have the same translation as fnorm.

  Machine_To_Pseudo(TOP_fmpy, TOP_fma);
  Require("OPND(4) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3
  Map_Arg("OPND(3)", "OPND(3)");	// f4

  Machine_To_Pseudo(TOP_fmpy_s, TOP_fma_s);
  Require("OPND(4) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3
  Map_Arg("OPND(3)", "OPND(3)");	// f4

  Machine_To_Pseudo(TOP_fmpy_d, TOP_fma_d);
  Require("OPND(4) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3
  Map_Arg("OPND(3)", "OPND(3)");	// f4

  Machine_To_Pseudo(TOP_fneg, TOP_fmerge_ns);
  Require("OPND(1) == OPND(2)");	// f3
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// f3

  Machine_To_Pseudo(TOP_fnegabs, TOP_fmerge_ns);
  Require("OPND(1) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(2)");	// f3

  Machine_To_Pseudo(TOP_fnmpy, TOP_fnma);
  Require("OPND(4) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3
  Map_Arg("OPND(3)", "OPND(3)");	// f4

  Machine_To_Pseudo(TOP_fnmpy_s, TOP_fnma_s);
  Require("OPND(4) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3
  Map_Arg("OPND(3)", "OPND(3)");	// f4

  Machine_To_Pseudo(TOP_fnmpy_d, TOP_fnma_d);
  Require("OPND(4) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3
  Map_Arg("OPND(3)", "OPND(3)");	// f4

  Machine_To_Pseudo(TOP_fnorm, TOP_fma);
  Require("OPND(3) == 1");		// f4
  Require("OPND(4) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3

  Machine_To_Pseudo(TOP_fnorm_s, TOP_fma_s);
  Require("OPND(3) == 1");		// f4
  Require("OPND(4) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3

  Machine_To_Pseudo(TOP_fnorm_d, TOP_fma_d);
  Require("OPND(3) == 1");		// f4
  Require("OPND(4) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3

  Machine_To_Pseudo(TOP_fpabs, TOP_fpmerge_s);
  Require("OPND(1) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(2)");	// f3

  Machine_To_Pseudo(TOP_fpmpy, TOP_fpma);
  Require("OPND(4) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3
  Map_Arg("OPND(3)", "OPND(3)");	// f4

  Machine_To_Pseudo(TOP_fpneg, TOP_fpmerge_ns);
  Require("OPND(1) == OPND(2)");	// f3
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// f3

  Machine_To_Pseudo(TOP_fpnegabs, TOP_fpmerge_ns);
  Require("OPND(1) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(2)");	// f3

  Machine_To_Pseudo(TOP_fpnmpy, TOP_fpnma);
  Require("OPND(4) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3
  Map_Arg("OPND(3)", "OPND(3)");	// f4

  Machine_To_Pseudo(TOP_fsub, TOP_fms);
  Require("OPND(3) == 1");		// f4
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3
  Map_Arg("OPND(3)", "OPND(4)");	// f2

  Machine_To_Pseudo(TOP_fsub_s, TOP_fms_s);
  Require("OPND(3) == 1");		// f4
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3
  Map_Arg("OPND(3)", "OPND(4)");	// f2

  Machine_To_Pseudo(TOP_fsub_d, TOP_fms_d);
  Require("OPND(3) == 1");		// f4
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// .sf
  Map_Arg("OPND(2)", "OPND(2)");	// f3
  Map_Arg("OPND(3)", "OPND(4)");	// f2

  Machine_To_Pseudo(TOP_mov_t_br, TOP_mov_t_br_i);
  Require("OPND(1) == 1");		// mwh == none
  Require("OPND(2) == 0");		// ph_pvec_ih == 0
  Require("OPND(4) == 0");		// tag_13 == 0
  Map_Arg("RESULT(0)", "RESULT(0)");	// b1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(3)");	// r2

  Machine_To_Pseudo(TOP_mov_f, TOP_fmerge_s);
  Require("OPND(1) == OPND(2)");	// f2 == f3
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// f3

  Machine_To_Pseudo(TOP_mov, TOP_adds);
  Require("OPND(1) == 0");		// imm_14 == 0
  Map_Arg("RESULT(0)", "RESULT(0)");	// r1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(2)");	// r3

  Machine_To_Pseudo(TOP_mov_i, TOP_addl);
  Require("OPND(2) == 0");		// r3 == 0
  Map_Arg("RESULT(0)", "RESULT(0)");	// r1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// imm_22

  Machine_To_Pseudo(TOP_shl_i, TOP_dep_z);
  Require("OPND(2) == 64 - OPND(3)");	// pos_6 == 64 - len_6
  Map_Arg("RESULT(0)", "RESULT(0)");	// r1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// r2
  Map_Arg("OPND(2)", "OPND(2)");	// count_6 = pos_6

  Machine_To_Pseudo(TOP_shr_i, TOP_extr);
  Require("OPND(2) == 64 - OPND(3)");	// pos_6 == 64 - len_6
  Map_Arg("RESULT(0)", "RESULT(0)");	// r1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// r2
  Map_Arg("OPND(2)", "OPND(2)");	// count_6 = pos_6

  Machine_To_Pseudo(TOP_shr_i_u, TOP_extr_u);
  Require("OPND(2) == 64 - OPND(3)");	// pos_6 == 64 - len_6
  Map_Arg("RESULT(0)", "RESULT(0)");	// r1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// r2
  Map_Arg("OPND(2)", "OPND(2)");	// count_6 = pos_6

  Machine_To_Pseudo(TOP_xmpy_l, TOP_xma_l);
  Require("OPND(3) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// f3
  Map_Arg("OPND(2)", "OPND(2)");	// f4

  Machine_To_Pseudo(TOP_xmpy_h, TOP_xma_h);
  Require("OPND(3) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// f3
  Map_Arg("OPND(2)", "OPND(2)");	// f4

  Machine_To_Pseudo(TOP_xmpy_hu, TOP_xma_hu);
  Require("OPND(3) == 0");		// f2
  Map_Arg("RESULT(0)", "RESULT(0)");	// f1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// f3
  Map_Arg("OPND(2)", "OPND(2)");	// f4

  // TOP_tbit_nz
  // TOP_tbit_nz_unc
  // TOP_tnat_nz
  // TOP_tnat_nz_unc
  // TOP_xma_lu
  // TOP_xmpy_lu
  //	These cannot be uniquely identified.

  /* ========== pseudo to machine instruction translations ========== */

  Pseudo_To_Machine(TOP_alloc_3, TOP_alloc);
  Map_Arg("RESULT(0)", "RESULT(0)");	// r1
  Map_Arg("OPND(0)", "OPND(0) + OPND(1) + OPND(2)"); // sof = i + l + o
  Map_Arg("OPND(1)", "OPND(0) + OPND(1)"); // sol = i + l
  Map_Arg("OPND(2)", "OPND(3)");	// sor = r

  Pseudo_To_Machine(TOP_dep_z, TOP_shl_i);
  Map_Arg("RESULT(0)", "RESULT(0)");	// r1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// r2
  Map_Arg("OPND(2)", "OPND(2)");	// pos_6 = count_6
  Map_Arg("OPND(3)", "64 - OPND(2)");	// len_6 = 64 - count_6

  Pseudo_To_Machine(TOP_extr, TOP_shr_i);
  Map_Arg("RESULT(0)", "RESULT(0)");	// r1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// r2
  Map_Arg("OPND(2)", "OPND(2)");	// pos_6 = count_6
  Map_Arg("OPND(3)", "64 - OPND(2)");	// len_6 = 64 - count_6

  Pseudo_To_Machine(TOP_extr_u, TOP_shr_i_u);
  Map_Arg("RESULT(0)", "RESULT(0)");	// r1
  Map_Arg("OPND(0)", "OPND(0)");	// qp
  Map_Arg("OPND(1)", "OPND(1)");	// r2
  Map_Arg("OPND(2)", "OPND(2)");	// pos_6 = count_6
  Map_Arg("OPND(3)", "64 - OPND(2)");	// len_6 = 64 - count_6

  ISA_Pseudo_End();
  return 0;
}
