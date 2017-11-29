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
// Group TOPS with similar packing format together. 
/////////////////////////////////////////////////////////
// The instructions are listed by category. The different categories of
// instructions are:
//
//   1. ALU
//   2. Integer
//   3. Memory
//   4. Branch
//   5. Float
//
// Within each Pack_Type instructions are listed in the order as shown
// in the IA-64 instructions formats manual
/////////////////////////////////////

#include <stddef.h>
#include "topcode.h"
#include "isa_pack_gen.h"
 
main()
{
  ISA_PACK_TYPE	a1,	// Integer ALU -- Register-Register
		a2,	// Shift Left and Add
		a3,	// Integer ALU -- Immediate_8-Register 
		a4,	// Add Immediate_14
		a5,	// Add Immediate_22 
		a6,	// Integer Compare -- Register-Register
		a7,	// Integer Compare to Zero -- Register
		a8a,	// Integer Compare -- Immediate-Register
		a8b,	// Integer Compare -- Immediate-Register
		a9,	// Multimedia ALU
		a10,	// Multimedia Shift and Add
		a11,	// Move General Register (pseudo)
		a12,	// Move Immediate (pseudo)
		a13,	// Integer Compare -- Register-Register (pseudo)
		a14,	// Integer Compare -- Register-Register (pseudo)
		a15,	// Integer Compare -- Register-Register (pseudo)
		a16,	// Integer Compare -- Register-Register (pseudo)
		a17,	// Integer Compare -- Immediate-Register (pseudo)
		a18,	// Integer Compare -- Immediate-Register (pseudo)
		a19,	// Integer Compare -- Immediate-Register (pseudo)
		a20,	// Integer Compare -- Immediate-Register (pseudo)
		a21,	// Integer Compare -- Immediate-Register (pseudo)
		a22,	// Integer Compare to Zero -- Register (pseudo)
		a23;	// Integer Compare to Zero -- Register (pseudo)

  ISA_PACK_TYPE	i1,	// Multimedia Multiply and Shift
		i2,	// Multimedia Multiply/Mix/Pack/Unpack
		i3,	// Multimedia Mux1
		i4,	// Multimedia Mux2
		i5,	// Shift Right -- Variable
		i6,	// Multimedia Shift Right -- Fixed
		i7,	// Shift Left -- Variable
		i8,	// Multimedia Shift Left -- Fixed
		i9,	// Population Count
		i10,	// Shift Right Pair
		i11,	// Extract
		i12,	// Zero and Deposit
		i13,	// Zero and Deposit Immediate_8
		i14,	// Deposit Immediate_1
		i15,	// Deposit
		i16,	// Test Bit
		i17,	// Test NaT
//		i18,	// Move Long Immediate_64 (NOTE: now X2)
		i19,	// Break/Nop
		i20,	// Integer Speculation Check
		i21,	// Move to BR
		i22,	// Move from BR
		i23,	// Move to Predicates -- Register
		i24,	// Move to Predicates -- Immediate_44
		i25,	// Move from Predicates/IP
		i26,	// Move to AR -- Register
		i27,	// Move to AR -- Immediate_8
		i28,	// Move from AR
		i29,	// Sign/Zero Extend/Compute Zero Index
		i30,	// Move to BR (pseudo)
		i31,	// Test Bit (pseudo)
		i32,	// Test NaT (pseudo)
		i33,	// Shift Left Immediate (pseudo)
		i34;	// Shift Right Immediate (pseudo)

  ISA_PACK_TYPE	m1a,	// Integer Load
		m1b,	// Integer Load
		m2a,	// Integer Load -- Increment by Register
		m2b,	// Integer Load -- Increment by Register
		m3a,	// Integer Load -- Increment by Immediate
		m3b,	// Integer Load -- Increment by Immediate
		m4a,	// Integer Store
		m4b,	// Integer Store
		m5a,	// Integer Store -- Increment by Immediate
		m5b,	// Integer Store -- Increment by Immediate
		m6a,	// Floating-point Load
		m6b,	// Floating-point Load
		m7a,	// Floating-point Load -- Increment by Register
		m7b,	// Floating-point Load -- Increment by Register
		m8a,	// Floating-point Load -- Increment by Immediate
		m8b,	// Floating-point Load -- Increment by Immediate
		m9,	// Floating-point Store
		m10,	// Floating-point Store -- Increment by Immediate
		m11,	// Floating-point Load Pair
		m12,	// Floating-point Load Pair -- Increment by Immediate
		m13,	// Line Prefetch
		m14,	// Line Prefetch -- Increment by Register
		m15,	// Line Prefetch -- Increment by Immediate
		m16a,	// Exchange/Compare and Exchange
		m16b,	// Exchange/Compare and Exchange
		m17,	// Fetch and Add -- Immediate
		m18,	// Set FR
		m19,	// Get FR
		m20,	// Integer Speculation Check
		m21,	// Floating-point Speculation Check
		m22,	// Integer Advanced Load Check
		m23,	// Floating-point Advanced Load Check
		m24,	// Sync/Fence/Serialize/ALAT Control
		m25,	// RSE Control
		m26,	// Integer ALAT Entry Invalidate
		m27,	// Floating-point ALAT Entry Invalidate
		m28,	// Flush Cache/Purge Translation Cache Entry
		m29,	// Move to AR -- Register
		m30,	// Move to AR -- Immediate_8
		m31,	// Move from AR
		m32,	// Move to CR
		m33,	// Move from CR
		m34,	// Allocate Register Stack Frame
		m35,	// Move to PSR
		m36,	// Move from PSR
		m37,	// Break/Nop
		m38,	// Probe -- Register
		m39,	// Probe -- Immediate_2
		m40,	// Probe Fault
		m41,	// Translation Cache Insert
		m42,	// Move to Indirect Register/Translation Register Insert
		m43,	// Move from Indirect Register
		m44,	// Set/Reset User/System Mask
		m45,	// Translation Purge
		m46,	// Translation Access
		m47;	// Allocate Register Stack Frame (pseudo)

  ISA_PACK_TYPE	b1,	// IP-Relative Branch
		b2,	// IP-Relative Counted Branch
		b3,	// IP-Relative Call
		b4,	// Indirect Branch
		b4a,	// Indirect Branch
		b5,	// Indirect Call
		b6,	// IP-Relative Predict
		b7,	// Indirect Predict
		b8,	// Miscellaneous
		b9,	// Break/Nop
		b10,	// IP-Relative Unconditional Branch (pseudo)
		b11;	// Indirect Unconditional Branch (pseudo)

  ISA_PACK_TYPE	f1,	// Floating-point Multiply Add
		f2,	// Fixed-point Multiply Add
		f3,	// Parallel Floating-point Select
		f4,	// Floating-point Compare
		f5,	// Floating-point Class
		f6,	// Floating-point Reciprocal Approximation
		f7,	// Floating-point Reciprocal Square Root Approximation
		f8,	// Minimum/Maximum and Parallel Compare
		f9,	// Merge and Logical
		f10,	// Convert Floating-point to Fixed-point
		f11,	// Convert Fixed-point to Floating-point
		f12,	// Floating-point Set Controls
		f13,	// Floating-point Clear Flags
		f14,	// Floating-point Check Flags
		f15,	// Break/Nop
		f16,	// Floating-point Absolute Value (pseudo)
		f17,	// Floating-point Negate (pseudo)
		f18,	// Fixed-point Multiply Add (pseudo)
		f19,	// Fixed-point Multiply (pseudo)
		f20,	// Convert Unsigned Integer to Floating-point (pseudo)
		f21,	// Floating-point Add/Subtract (pseudo)
		f22,	// Floating-point Multiply (pseudo)
		f23,	// Floating-point Class (pseudo)
		f24,	// Floating-point Compare (pseudo)
		f25,	// Floating-point Compare (pseudo)
		f26,	// Floating-point Compare (pseudo)
		f27;	// Floating-point Parallel Compare (pseudo) 

  ISA_PACK_TYPE	x1,	// Floating-point Multiply Add
		x2,	// Move Long Immediate_64
		x3,	// Long Branch
		x4,	// Long Call
		x5;	// Long Branch (pseudo)

  OPND_ADJ_TYPE	comp6,
		no_adj,
		decr,
		incr,
		sext8_incr,
		sext8,
		comp5,
		pack_i1,
		unpack_i1,
		pack_m17,
		unpack_m17;

  ISA_Pack_Begin("ia64", 41);

  /* Create the various adjustments that need to be performed between
   * assembly language form and packed form. Note that simple shift 
   * adjustments have been accomplished directly in the operand packing
   * specification.
   */
  no_adj = Create_Operand_Adjustment("no adjustment", "O_VAL");
  comp6 = Create_Operand_Adjustment("6-bit complement", "O_VAL ^ 0x3f");
  decr = Create_Operand_Adjustment("decrement", "O_VAL - 1");
  incr = Create_Operand_Adjustment("increment", "O_VAL + 1");
  sext8_incr = Create_Operand_Adjustment("sign-extend(8)/increment", 
					 "((O_VAL << 56) >> 56) + 1");
  sext8 = Create_Operand_Adjustment("sign-extend(8)", "(O_VAL << 56) >> 56");
  comp5 = Create_Operand_Adjustment("5-bit complement", "O_VAL ^ 0x1f");
  pack_i1 = Create_Operand_Adjustment("Pack (I1)",
		"(O_VAL == 0) ? 0 : (O_VAL == 7) ? 1 : (O_VAL == 15) ? 2 : 3");
  unpack_i1 = Create_Operand_Adjustment("Unpack (I1)",
		"(O_VAL == 0) ? 0 : (O_VAL == 1) ? 7 : (O_VAL == 2) ? 15 : 16");
  pack_m17 = Create_Operand_Adjustment("Pack (M17)",
	"((O_VAL < 0) * 4) + ((O_VAL & 1) ? 3 : (O_VAL & 4) ? 2 : (O_VAL & 8) ? 1 : 0)");
  unpack_m17 = Create_Operand_Adjustment("unpack (M17)",
	"(((O_VAL >= 4) ? -1 : 1) * (((O_VAL & 3) == 3) ? 1 : 1 << (4 - (O_VAL & 3))))");

/* =====  A1: Integer ALU -- Register-Register ===== */
  a1 = ISA_Pack_Type_Create("a1");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// r2
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(a1,
	TOP_add,		0x10000000000ULL,
	TOP_add_1,		0x10008000000ULL,
	TOP_sub,		0x10028000000ULL,
	TOP_sub_1,		0x10020000000ULL,
	TOP_addp4,		0x10040000000ULL,
	TOP_and,		0x10060000000ULL,
	TOP_andcm,		0x10068000000ULL,
	TOP_or,			0x10070000000ULL,
	TOP_xor,		0x10078000000ULL,
	TOP_UNDEFINED);

/* ===== A2: Shift Left and Add ===== */
  a2 = ISA_Pack_Type_Create("a2");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// r2
  Adjust_Operand(2, decr, incr); // ct_2d = count_2 - 1
  Operand(2, 0, 27, 2);		// ct_2d
  Operand(3, 0, 20, 7);		// r3
  Instruction_Pack_Group(a2,
	TOP_shladd,		0x10080000000ULL,
	TOP_shladdp4,		0x100c0000000ULL,
	TOP_UNDEFINED);

/* ===== A3: Integer ALU -- Immediate_8-Register ===== */
  a3 = ISA_Pack_Type_Create("a3");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// imm_7b
  Operand(1, 7, 36, 1);		// s
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(a3,
	TOP_sub_i,		0x10128000000ULL,
	TOP_and_i,		0x10160000000ULL,
	TOP_andcm_i,		0x10168000000ULL,
	TOP_or_i,		0x10170000000ULL,
	TOP_xor_i,		0x10178000000ULL,
	TOP_UNDEFINED);

/* ===== A4: Add Immediate_14 ===== */
  a4 = ISA_Pack_Type_Create("a4");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// imm_7b
  Operand(1, 7, 27, 6);		// imm_6d
  Operand(1, 13, 36, 1);	// s
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(a4,
	TOP_adds,		0x10800000000ULL,
	TOP_addp4_i,		0x10c00000000ULL,
	TOP_UNDEFINED);

/* ===== A5: Add Immediate_22 ===== */
  a5 = ISA_Pack_Type_Create("a5");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// imm_7b
  Operand(1, 7, 27, 9);		// imm_9d
  Operand(1, 16, 22, 5);	// imm_5c
  Operand(1, 21, 36, 1);	// s
  Operand(2, 0, 20, 2);		// r3
  Instruction_Pack_Group(a5,
	TOP_addl,		0x12000000000ULL,
	TOP_UNDEFINED);

/* ===== A6: Integer Compare -- Register-Register ===== */
  a6 = ISA_Pack_Type_Create("a6");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 6);		// p1
  Result(1, 27, 6);		// p2
  Operand(1, 0, 13, 7);		// r2
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(a6,
	TOP_cmp_lt,		0x18000000000ULL,
	TOP_cmp_ltu,		0x1a000000000ULL,
	TOP_cmp_eq,		0x1c000000000ULL,
	TOP_cmp_lt_unc,		0x18000001000ULL,
	TOP_cmp_ltu_unc,	0x1a000001000ULL,
	TOP_cmp_eq_unc,		0x1c000001000ULL,
	TOP_cmp_eq_and,		0x18200000000ULL,
	TOP_cmp_eq_or,		0x1a200000000ULL,
	TOP_cmp_eq_or_andcm,	0x1c200000000ULL,
	TOP_cmp_ne_and,		0x18200001000ULL,
	TOP_cmp_ne_or,		0x1a200001000ULL,
	TOP_cmp_ne_or_andcm,	0x1c200001000ULL,
	TOP_cmp4_lt,		0x18400000000ULL,
	TOP_cmp4_ltu,		0x1a400000000ULL,
	TOP_cmp4_eq,		0x1c400000000ULL,
	TOP_cmp4_lt_unc,	0x18400001000ULL,
	TOP_cmp4_ltu_unc,	0x1a400001000ULL,
	TOP_cmp4_eq_unc,	0x1c400001000ULL,
	TOP_cmp4_eq_and,	0x18600000000ULL,
	TOP_cmp4_eq_or,		0x1a600000000ULL,
	TOP_cmp4_eq_or_andcm,	0x1c600000000ULL,
	TOP_cmp4_ne_and,	0x18600001000ULL,
	TOP_cmp4_ne_or,		0x1a600001000ULL,
	TOP_cmp4_ne_or_andcm,	0x1c600001000ULL,
	TOP_UNDEFINED);

/* ===== A7: Integer Compare to Zero -- Register ===== */
  a7 = ISA_Pack_Type_Create("a7");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 6);		// p1
  Result(1, 27, 6);		// p2
  Operand(1, 0, 20, 7);		// r3
  Instruction_Pack_Group(a7,
	TOP_cmp_z1_gt_and,	0x19000000000ULL,
	TOP_cmp_z1_gt_or,	0x1b000000000ULL,
	TOP_cmp_z1_gt_or_andcm,	0x1d000000000ULL,
	TOP_cmp_z1_le_and,	0x19000001000ULL,
	TOP_cmp_z1_le_or,	0x1b000001000ULL,
	TOP_cmp_z1_le_or_andcm,	0x1d000001000ULL,
	TOP_cmp_z1_ge_and,	0x19200000000ULL,
	TOP_cmp_z1_ge_or,	0x1b200000000ULL,
	TOP_cmp_z1_ge_or_andcm,	0x1d200000000ULL,
	TOP_cmp_z1_lt_and,	0x19200001000ULL,
	TOP_cmp_z1_lt_or,	0x1b200001000ULL,
	TOP_cmp_z1_lt_or_andcm,	0x1d200001000ULL,
	TOP_cmp4_z1_gt_and,	0x19400000000ULL,
	TOP_cmp4_z1_gt_or,	0x1b400000000ULL,
	TOP_cmp4_z1_gt_or_andcm,0x1d400000000ULL,
	TOP_cmp4_z1_le_and,	0x19400001000ULL,
	TOP_cmp4_z1_le_or,	0x1b400001000ULL,
	TOP_cmp4_z1_le_or_andcm,0x1d400001000ULL,
	TOP_cmp4_z1_ge_and,	0x19600000000ULL,
	TOP_cmp4_z1_ge_or,	0x1b600000000ULL,
	TOP_cmp4_z1_ge_or_andcm,0x1d600000000ULL,
	TOP_cmp4_z1_lt_and,	0x19600001000ULL,
	TOP_cmp4_z1_lt_or,	0x1b600001000ULL,
	TOP_cmp4_z1_lt_or_andcm,0x1d600001000ULL,
	TOP_UNDEFINED);

/* ===== A8a: Integer Compare -- Immediate-Register ===== */
  a8a = ISA_Pack_Type_Create("a8a");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 6);		// p1
  Result(1, 27, 6);		// p2
  Operand(1, 0, 13, 7);		// imm_7b
  Operand(1, 7, 36, 1);		// s
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(a8a,
	TOP_cmp_i_lt,		0x18800000000ULL,
	TOP_cmp_i_eq,		0x1c800000000ULL,
	TOP_cmp_i_lt_unc,	0x18800001000ULL,
	TOP_cmp_i_eq_unc,	0x1c800001000ULL,
	TOP_cmp_i_eq_and,	0x18a00000000ULL,
	TOP_cmp_i_eq_or,	0x1aa00000000ULL,
	TOP_cmp_i_eq_or_andcm,	0x1ca00000000ULL,
	TOP_cmp_i_ne_and,	0x18a00001000ULL,
	TOP_cmp_i_ne_or,	0x1aa00001000ULL,
	TOP_cmp_i_ne_or_andcm,	0x1ca00001000ULL,
	TOP_cmp4_i_lt,		0x18c00000000ULL,
	TOP_cmp4_i_eq,		0x1cc00000000ULL,
	TOP_cmp4_i_lt_unc,	0x18c00001000ULL,
	TOP_cmp4_i_eq_unc,	0x1cc00001000ULL,
	TOP_cmp4_i_eq_and,	0x18e00000000ULL,
	TOP_cmp4_i_eq_or,	0x1ae00000000ULL,
	TOP_cmp4_i_eq_or_andcm,	0x1ce00000000ULL,
	TOP_cmp4_i_ne_and,	0x18e00001000ULL,
	TOP_cmp4_i_ne_or,	0x1ae00001000ULL,
	TOP_cmp4_i_ne_or_andcm,	0x1ce00001000ULL,
	TOP_UNDEFINED);

/* ===== A8b: Integer Compare -- Immediate-Register ===== */
  a8b = ISA_Pack_Type_Create("a8b");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 6);		// p1
  Result(1, 27, 6);		// p2
  Adjust_Operand(1, no_adj, sext8); // imm8 = sext(imm) (unpack only)
  Operand(1, 0, 13, 7);		// imm_7b
  Operand(1, 7, 36, 1);		// s
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(a8b,
	TOP_cmp_i_ltu,		0x1a800000000ULL,
	TOP_cmp_i_ltu_unc,	0x1a800001000ULL,
	TOP_cmp4_i_ltu,		0x1ac00000000ULL,
	TOP_cmp4_i_ltu_unc,	0x1ac00001000ULL,
	TOP_UNDEFINED);

/* ===== A9: Multimedia ALU ===== */
  a9 = ISA_Pack_Type_Create("a9");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// r2
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(a9,
	TOP_padd1,		0x10400000000ULL,
	TOP_padd2,		0x10600000000ULL,
	TOP_padd4,		0x11400000000ULL,
	TOP_padd1_sss,		0x10408000000ULL,
	TOP_padd2_sss,		0x10608000000ULL,
	TOP_padd1_uuu,		0x10410000000ULL,
	TOP_padd2_uuu,		0x10610000000ULL,
	TOP_padd1_uus,		0x10418000000ULL,
	TOP_padd2_uus,		0x10618000000ULL,
	TOP_psub1,		0x10420000000ULL,
	TOP_psub2,		0x10620000000ULL,
	TOP_psub4,		0x11420000000ULL,
	TOP_psub1_sss,		0x10428000000ULL,
	TOP_psub2_sss,		0x10628000000ULL,
	TOP_psub1_uuu,		0x10430000000ULL,
	TOP_psub2_uuu,		0x10630000000ULL,
	TOP_psub1_uus,		0x10438000000ULL,
	TOP_psub2_uus,		0x10638000000ULL,
	TOP_pavg1,		0x10450000000ULL,
	TOP_pavg2,		0x10650000000ULL,
	TOP_pavg1_raz,		0x10458000000ULL,
	TOP_pavg2_raz,		0x10658000000ULL,
	TOP_pavgsub1,		0x10470000000ULL,
	TOP_pavgsub2,		0x10670000000ULL,
	TOP_pcmp1_eq,		0x10520000000ULL,
	TOP_pcmp2_eq,		0x10720000000ULL,
	TOP_pcmp4_eq,		0x11520000000ULL,
	TOP_pcmp1_gt,		0x10528000000ULL,
	TOP_pcmp2_gt,		0x10728000000ULL,
	TOP_pcmp4_gt,		0x11528000000ULL,
	TOP_UNDEFINED);

/* ===== A10: Multimedia Shift and Add ===== */
  a10 = ISA_Pack_Type_Create("a10");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// r2
  Adjust_Operand(2, decr, incr); // ct_2d = count_2 - 1
  Operand(2, 0, 27, 2);		// ct_2d
  Operand(3, 0, 20, 7);		// r3
  Instruction_Pack_Group(a10,
	TOP_pshladd2,		0x10680000000ULL,
	TOP_pshradd2,		0x106c0000000ULL,
	TOP_UNDEFINED);

/* ===== A11: Move General Register (pseudo) ===== */
  a11 = ISA_Pack_Type_Create("a11");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 20, 7);		// r3
  Instruction_Pack_Group(a11,
	TOP_mov,		0x10800000000ULL,
	TOP_UNDEFINED);

/* ===== A12: Move Immediate (pseudo) ===== */
  a12 = ISA_Pack_Type_Create("a12");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// imm_7b
  Operand(1, 7, 27, 9);		// imm_9d
  Operand(1, 16, 22, 5);	// imm_5c
  Operand(1, 21, 36, 1);	// s
  Instruction_Pack_Group(a12,
	TOP_mov_i,		0x12000000000ULL,
	TOP_UNDEFINED);

/* ===== A13: Integer Compare -- Register-Register (pseudo) ===== */
  a13 = ISA_Pack_Type_Create("a13");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 6);		// p1
  Result(1, 27, 6);		// p2
  Operand(1, 0, 13, 7);		// r2
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(a13,
	TOP_cmp_eq_andcm,	0x18200001000ULL,
	TOP_cmp_eq_orcm,	0x1a200001000ULL,
	TOP_cmp_ne_andcm,	0x18200000000ULL,
	TOP_cmp_ne_orcm,	0x1a200000000ULL,
	TOP_cmp4_eq_andcm,	0x18600001000ULL,
	TOP_cmp4_eq_orcm,	0x1a600001000ULL,
	TOP_cmp4_ne_andcm,	0x18600000000ULL,
	TOP_cmp4_ne_orcm,	0x1a600000000ULL,
	TOP_UNDEFINED);

/* ===== A14: Integer Compare -- Register-Register (pseudo) ===== */
// swapped preds
  a14 = ISA_Pack_Type_Create("a14");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 27, 6);		// p2
  Result(1, 6, 6);		// p1
  Operand(1, 0, 13, 7);		// r2
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(a14,
	TOP_cmp_eq_and_orcm,	0x1c200001000ULL,
	TOP_cmp_ge,		0x18000000000ULL,
	TOP_cmp_ge_unc,		0x18000001000ULL,
	TOP_cmp_geu,		0x1a000000000ULL,
	TOP_cmp_geu_unc,	0x1a000001000ULL,
	TOP_cmp_ne_and_orcm,	0x1c200000000ULL,
	TOP_cmp_ne,		0x1c000000000ULL,
	TOP_cmp_ne_unc,		0x1c000001000ULL,
	TOP_cmp4_eq_and_orcm,	0x1c600001000ULL,
	TOP_cmp4_ge,		0x18400000000ULL,
	TOP_cmp4_ge_unc,	0x18400001000ULL,
	TOP_cmp4_geu,		0x1a400000000ULL,
	TOP_cmp4_geu_unc,	0x1a400001000ULL,
	TOP_cmp4_ne_and_orcm,	0x1c600000000ULL,
	TOP_cmp4_ne,		0x1c400000000ULL,
	TOP_cmp4_ne_unc,	0x1c400001000ULL,
	TOP_UNDEFINED);

/* ===== A15: Integer Compare -- Register-Register (pseudo) ===== */
// swapped args
  a15 = ISA_Pack_Type_Create("a15");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 6);		// p1
  Result(1, 27, 6);		// p2
  Operand(1, 0, 20, 7);		// r3
  Operand(2, 0, 13, 7);		// r2
  Instruction_Pack_Group(a15,
	TOP_cmp_gt,		0x18000000000ULL,
	TOP_cmp_gt_unc,		0x18000001000ULL,
	TOP_cmp_gtu,		0x1a000000000ULL,
	TOP_cmp_gtu_unc,	0x1a000001000ULL,
	TOP_cmp4_gt,		0x18400000000ULL,
	TOP_cmp4_gt_unc,	0x18400001000ULL,
	TOP_cmp4_gtu,		0x1a400000000ULL,
	TOP_cmp4_gtu_unc,	0x1a400001000ULL,
	TOP_UNDEFINED);

/* ===== A16: Integer Compare -- Register-Register (pseudo) ===== */
// swapped preds and args
  a16 = ISA_Pack_Type_Create("a16");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 27, 6);		// p2
  Result(1, 6, 6);		// p1
  Operand(1, 0, 20, 7);		// r3
  Operand(2, 0, 13, 7);		// r2
  Instruction_Pack_Group(a16,
	TOP_cmp_le,		0x18000000000ULL,
	TOP_cmp_le_unc,		0x18000001000ULL,
	TOP_cmp_leu,		0x1a000000000ULL,
	TOP_cmp_leu_unc,	0x1a000001000ULL,
	TOP_cmp4_le,		0x18400000000ULL,
	TOP_cmp4_le_unc,	0x18400001000ULL,
	TOP_cmp4_leu,		0x1a400000000ULL,
	TOP_cmp4_leu_unc,	0x1a400001000ULL,
	TOP_UNDEFINED);

/* ===== A17: Integer Compare -- Immediate-Register (pseudo) ===== */
  a17 = ISA_Pack_Type_Create("a17");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 6);		// p1
  Result(1, 27, 6);		// p2
  Operand(1, 0, 13, 7);		// imm_7b
  Operand(1, 7, 36, 1);		// s
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(a17,
	TOP_cmp_i_eq_andcm,	0x18a00001000ULL,
	TOP_cmp_i_eq_orcm,	0x1aa00001000ULL,
	TOP_cmp_i_ne_andcm,	0x18a00000000ULL,
	TOP_cmp_i_ne_orcm,	0x1aa00000000ULL,
	TOP_cmp4_i_eq_andcm,	0x18e00001000ULL,
	TOP_cmp4_i_eq_orcm,	0x1ae00001000ULL,
	TOP_cmp4_i_ne_andcm,	0x18e00000000ULL,
	TOP_cmp4_i_ne_orcm,	0x1ae00000000ULL,
	TOP_UNDEFINED);

/* ===== A18: Integer Compare -- Immediate-Register (pseudo) ===== */
// swapped preds
  a18 = ISA_Pack_Type_Create("a18");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 27, 6);		// p2
  Result(1, 6, 6);		// p1
  Operand(1, 0, 13, 7);		// imm_7b
  Operand(1, 7, 36, 1);		// s
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(a18,
	TOP_cmp_i_eq_and_orcm,	0x1ca00001000ULL,
	TOP_cmp_i_ge,		0x18800000000ULL,
	TOP_cmp_i_ge_unc,	0x18800001000ULL,
	TOP_cmp_i_ne_and_orcm,	0x1ca00000000ULL,
	TOP_cmp_i_ne,		0x1c800000000ULL,
	TOP_cmp_i_ne_unc,	0x1c800001000ULL,
	TOP_cmp4_i_eq_and_orcm,	0x1ce00001000ULL,
	TOP_cmp4_i_ge,		0x18c00000000ULL,
	TOP_cmp4_i_ge_unc,	0x18c00001000ULL,
	TOP_cmp4_i_ne_and_orcm,	0x1ce00000000ULL,
	TOP_cmp4_i_ne,		0x1cc00000000ULL,
	TOP_cmp4_i_ne_unc,	0x1cc00001000ULL,
	TOP_UNDEFINED);

/* ===== A19: Integer Compare -- Immediate-Register (pseudo) ===== */
// swapped preds and sext(immed)
  a19 = ISA_Pack_Type_Create("a19");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 27, 6);		// p2
  Result(1, 6, 6);		// p1
  Adjust_Operand(1, no_adj, sext8); // imm8 = sext(imm) (unpack only)
  Operand(1, 0, 13, 7);		// imm_7b
  Operand(1, 7, 36, 1);		// s
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(a19,
	TOP_cmp_i_geu,		0x1a800000000ULL,
	TOP_cmp_i_geu_unc,	0x1a800001000ULL,
	TOP_cmp4_i_geu,		0x1ac00000000ULL,
	TOP_cmp4_i_geu_unc,	0x1ac00001000ULL,
	TOP_UNDEFINED);

/* ===== A20: Integer Compare -- Immediate-Register (pseudo) ===== */
// immed-1
  a20 = ISA_Pack_Type_Create("a20");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 6);		// p1
  Result(1, 27, 6);		// p2
  Adjust_Operand(1, decr, sext8_incr); // imm_8 = imm_8 - 1
  Operand(1, 0, 13, 7);		// imm_7b
  Operand(1, 7, 36, 1);		// s
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(a20,
	TOP_cmp_i_le,		0x18800000000ULL,
	TOP_cmp_i_le_unc,	0x18800001000ULL,
	TOP_cmp_i_leu,		0x1a800000000ULL,
	TOP_cmp_i_leu_unc,	0x1a800001000ULL,
	TOP_cmp4_i_le,		0x18c00000000ULL,
	TOP_cmp4_i_le_unc,	0x18c00001000ULL,
	TOP_cmp4_i_leu,		0x1ac00000000ULL,
	TOP_cmp4_i_leu_unc,	0x1ac00001000ULL,
	TOP_UNDEFINED);

/* ===== A21: Integer Compare -- Immediate-Register (pseudo) ===== */
// swapped pred and immed-1
  a21 = ISA_Pack_Type_Create("a21");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 27, 6);		// p2
  Result(1, 6, 6);		// p1
  Adjust_Operand(1, decr, sext8_incr); // imm_8 = imm_8 - 1
  Operand(1, 0, 13, 7);		// imm_7b
  Operand(1, 7, 36, 1);		// s
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(a21,
	TOP_cmp_i_gt,		0x18800000000ULL,
	TOP_cmp_i_gt_unc,	0x18800001000ULL,
	TOP_cmp_i_gtu,		0x1a800000000ULL,
	TOP_cmp_i_gtu_unc,	0x1a800001000ULL,
	TOP_cmp4_i_gt,		0x18c00000000ULL,
	TOP_cmp4_i_gt_unc,	0x18c00001000ULL,
	TOP_cmp4_i_gtu,		0x1ac00000000ULL,
	TOP_cmp4_i_gtu_unc,	0x1ac00001000ULL,
	TOP_UNDEFINED);

/* ===== A22: Integer Compare to Zero -- Register (pseudo) ===== */
// swapped preds
  a22 = ISA_Pack_Type_Create("a22");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 27, 6);		// p2
  Result(1, 6, 6);		// p1
  Operand(1, 0, 20, 7);		// r3
  Instruction_Pack_Group(a22,
	TOP_cmp_z1_gt_and_orcm,	0x1d000001000ULL,
	TOP_cmp_z1_le_and_orcm,	0x1d000000000ULL,
	TOP_cmp_z1_ge_and_orcm,	0x1d200001000ULL,
	TOP_cmp_z1_lt_and_orcm,	0x1d200000000ULL,
	TOP_cmp4_z1_gt_and_orcm,0x1d400001000ULL,
	TOP_cmp4_z1_le_and_orcm,0x1d400000000ULL,
	TOP_cmp4_z1_ge_and_orcm,0x1d600001000ULL,
	TOP_cmp4_z1_lt_and_orcm,0x1d600000000ULL,
	TOP_cmp_z2_gt_and_orcm,	0x1d200000000ULL,
	TOP_cmp_z2_le_and_orcm,	0x1d200001000ULL,
	TOP_cmp_z2_ge_and_orcm,	0x1d000000000ULL,
	TOP_cmp_z2_lt_and_orcm,	0x1d000001000ULL,
	TOP_cmp4_z2_gt_and_orcm,0x1d600000000ULL,
	TOP_cmp4_z2_le_and_orcm,0x1d600001000ULL,
	TOP_cmp4_z2_ge_and_orcm,0x1d400000000ULL,
	TOP_cmp4_z2_lt_and_orcm,0x1d400001000ULL,
	TOP_UNDEFINED);

/* ===== A23: Integer Compare to Zero -- Register (pseudo) ===== */
  a23 = ISA_Pack_Type_Create("a23");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 6);		// p1
  Result(1, 27, 6);		// p2
  Operand(1, 0, 20, 7);		// r3
  Instruction_Pack_Group(a23,
	TOP_cmp_z1_gt_andcm,	0x19000001000ULL,
	TOP_cmp_z1_gt_orcm,	0x1b000001000ULL,
	TOP_cmp_z1_le_andcm,	0x19000000000ULL,
	TOP_cmp_z1_le_orcm,	0x1b000000000ULL,
	TOP_cmp_z1_ge_andcm,	0x19200001000ULL,
	TOP_cmp_z1_ge_orcm,	0x1b200001000ULL,
	TOP_cmp_z1_lt_andcm,	0x19200000000ULL,
	TOP_cmp_z1_lt_orcm,	0x1b200000000ULL,
	TOP_cmp4_z1_gt_andcm,	0x19400001000ULL,
	TOP_cmp4_z1_gt_orcm,	0x1b400001000ULL,
	TOP_cmp4_z1_le_andcm,	0x19400000000ULL,
	TOP_cmp4_z1_le_orcm,	0x1b400000000ULL,
	TOP_cmp4_z1_ge_andcm,	0x19600001000ULL,
	TOP_cmp4_z1_ge_orcm,	0x1b600001000ULL,
	TOP_cmp4_z1_lt_andcm,	0x19600000000ULL,
	TOP_cmp4_z1_lt_orcm,	0x1b600000000ULL,
	TOP_cmp_z2_gt_andcm,	0x19200000000ULL,
	TOP_cmp_z2_gt_orcm,	0x1b200000000ULL,
	TOP_cmp_z2_le_andcm,	0x19200001000ULL,
	TOP_cmp_z2_le_orcm,	0x1b200001000ULL,
	TOP_cmp_z2_ge_andcm,	0x19000000000ULL,
	TOP_cmp_z2_ge_orcm,	0x1b000000000ULL,
	TOP_cmp_z2_lt_andcm,	0x19000001000ULL,
	TOP_cmp_z2_lt_orcm,	0x1b000001000ULL,
	TOP_cmp4_z2_gt_andcm,	0x19600000000ULL,
	TOP_cmp4_z2_gt_orcm,	0x1b600000000ULL,
	TOP_cmp4_z2_le_andcm,	0x19600001000ULL,
	TOP_cmp4_z2_le_orcm,	0x1b600001000ULL,
	TOP_cmp4_z2_ge_andcm,	0x19400000000ULL,
	TOP_cmp4_z2_ge_orcm,	0x1b400000000ULL,
	TOP_cmp4_z2_lt_andcm,	0x19400001000ULL,
	TOP_cmp4_z2_lt_orcm,	0x1b400001000ULL,
	TOP_cmp_z2_gt_and,	0x19200001000ULL,
	TOP_cmp_z2_gt_or,	0x1b200001000ULL,
	TOP_cmp_z2_gt_or_andcm,	0x1d200001000ULL,
	TOP_cmp_z2_le_and,	0x19200000000ULL,
	TOP_cmp_z2_le_or,	0x1b200000000ULL,
	TOP_cmp_z2_le_or_andcm,	0x1d200000000ULL,
	TOP_cmp_z2_ge_and,	0x19000001000ULL,
	TOP_cmp_z2_ge_or,	0x1b000001000ULL,
	TOP_cmp_z2_ge_or_andcm,	0x1d000001000ULL,
	TOP_cmp_z2_lt_and,	0x19000000000ULL,
	TOP_cmp_z2_lt_or,	0x1b000000000ULL,
	TOP_cmp_z2_lt_or_andcm,	0x1d000000000ULL,
	TOP_cmp4_z2_gt_and,	0x19600001000ULL,
	TOP_cmp4_z2_gt_or,	0x1b600001000ULL,
	TOP_cmp4_z2_gt_or_andcm,0x1d600001000ULL,
	TOP_cmp4_z2_le_and,	0x19600000000ULL,
	TOP_cmp4_z2_le_or,	0x1b600000000ULL,
	TOP_cmp4_z2_le_or_andcm,0x1d600000000ULL,
	TOP_cmp4_z2_ge_and,	0x19400001000ULL,
	TOP_cmp4_z2_ge_or,	0x1b400001000ULL,
	TOP_cmp4_z2_ge_or_andcm,0x1d400001000ULL,
	TOP_cmp4_z2_lt_and,	0x19400000000ULL,
	TOP_cmp4_z2_lt_or,	0x1b400000000ULL,
	TOP_cmp4_z2_lt_or_andcm,0x1d400000000ULL,
	TOP_UNDEFINED);

/* ===== I1: Multimedia Multiply and Shift ===== */
  i1 = ISA_Pack_Type_Create("i1");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// r2
  Operand(2, 0, 20, 7);		// r3
  Adjust_Operand(3, pack_i1, unpack_i1); // ct_2d = (count_2 == 0) ? 0 : (count_2 == 7) ? 1 : (count_2 == 15) ? 2 : 3
  Operand(3, 0, 30, 2);		// ct_2d
  Instruction_Pack_Group(i1,
	TOP_pmpyshr2,		0x0e230000000ULL,
	TOP_pmpyshr2_u,		0x0e210000000ULL,
	TOP_UNDEFINED);

/* ===== I2: Multimedia Multiply/Mix/Pack/Unpack ===== */
  i2 = ISA_Pack_Type_Create("i2");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// r2
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(i2,
	TOP_pmpy2_r,		0x0ead0000000ULL,
	TOP_pmpy2_l,		0x0eaf0000000ULL,
	TOP_mix1_r,		0x0e880000000ULL,
	TOP_mix2_r,		0x0ea80000000ULL,
	TOP_mix4_r,		0x0f880000000ULL,
	TOP_mix1_l,		0x0e8a0000000ULL,
	TOP_mix2_l,		0x0eaa0000000ULL,
	TOP_mix4_l,		0x0f8a0000000ULL,
	TOP_pack2_uss,		0x0ea00000000ULL,
	TOP_pack2_sss,		0x0ea20000000ULL,
	TOP_pack4_sss,		0x0f820000000ULL,
	TOP_unpack1_h,		0x0e840000000ULL,
	TOP_unpack2_h,		0x0ea40000000ULL,
	TOP_unpack4_h,		0x0f840000000ULL,
	TOP_unpack1_l,		0x0e860000000ULL,
	TOP_unpack2_l,		0x0ea60000000ULL,
	TOP_unpack4_l,		0x0f860000000ULL,
	TOP_pmin1_u,		0x0e810000000ULL,
	TOP_pmax1_u,		0x0e850000000ULL,
	TOP_pmin2,		0x0ea30000000ULL,
	TOP_pmax2,		0x0ea70000000ULL,
	TOP_psad1,		0x0e8b0000000ULL,
	TOP_UNDEFINED);

/* ===== I3: Multimedia Mux1 ===== */
  i3 = ISA_Pack_Type_Create("i3");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// r2
  Operand(2, 0, 20, 4);		// mbt_4c
  Instruction_Pack_Group(i3,
	TOP_mux1,		0x0eca0000000ULL,
	TOP_UNDEFINED);

/* ===== I4: Multimedia Mux2 ===== */
  i4 = ISA_Pack_Type_Create("i4");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// r2
  Operand(2, 0, 20, 8);		// mht_8c
  Instruction_Pack_Group(i4,
	TOP_mux2,		0x0eea0000000ULL,
	TOP_UNDEFINED);

/* ===== I5: Shift Right -- Variable ===== */
  i5 = ISA_Pack_Type_Create("i5");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 20, 7);		// r3
  Operand(2, 0, 13, 7);		// r2
  Instruction_Pack_Group(i5,
	TOP_pshr2,		0x0e220000000ULL,
	TOP_pshr4,		0x0f020000000ULL,
	TOP_shr,		0x0f220000000ULL,
	TOP_pshr2_u,		0x0e200000000ULL,
	TOP_pshr4_u,		0x0f000000000ULL,
	TOP_shr_u,		0x0f200000000ULL,
	TOP_UNDEFINED);

/* ===== I6: Multimedia Shift Right -- Fixed ===== */
  i6 = ISA_Pack_Type_Create("i6");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 20, 7);		// r3
  Operand(2, 0, 14, 5);		// count_5b
  Instruction_Pack_Group(i6,
	TOP_pshr2_i,		0x0e630000000ULL,
	TOP_pshr4_i,		0x0f430000000ULL,
	TOP_pshr2_i_u,		0x0e610000000ULL,
	TOP_pshr4_i_u,		0x0f410000000ULL,
	TOP_UNDEFINED);

/* ===== I7: Shift Left -- Variable ===== */
  i7 = ISA_Pack_Type_Create("i7");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// r2
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(i7,
	TOP_pshl2,		0x0e240000000ULL,
	TOP_pshl4,		0x0f040000000ULL,
	TOP_shl,		0x0f240000000ULL,
	TOP_UNDEFINED);

/* ===== I8: Multimedia Shift Left -- Fixed ===== */
  i8 = ISA_Pack_Type_Create("i8");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// r2
  Adjust_Operand(2, comp5, comp5); // ccount_5b = 31 - count_5
  Operand(2, 0, 20, 5);		// ccount_5b
  Instruction_Pack_Group(i8,
	TOP_pshl2_i,		0x0ee50000000ULL,
	TOP_pshl4_i,		0x0fc50000000ULL,
	TOP_UNDEFINED);

/* ===== I9: Population Count ===== */
  i9 = ISA_Pack_Type_Create("i9");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 20, 7);		// r3
  Instruction_Pack_Group(i9,
	TOP_popcnt,		0x0e690000000ULL,
	TOP_UNDEFINED);

/* ===== I10: Shift Right Pair ===== */
  i10 = ISA_Pack_Type_Create("i10");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// r2
  Operand(2, 0, 20, 7);		// r3
  Operand(3, 0, 27, 6);		// count_6d
  Instruction_Pack_Group(i10,
	TOP_shrp,		0x0ac00000000ULL,
	TOP_UNDEFINED);

/* ===== I11: Extract ===== */
  i11 = ISA_Pack_Type_Create("i11");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 20, 7);		// r3
  Operand(2, 0, 14, 6);		// pos_6b
  Adjust_Operand(3, decr, incr); // len_6d = len_6 - 1
  Operand(3, 0, 27, 6);		// len_6d
  Instruction_Pack_Group(i11,
	TOP_extr_u,		0x0a400000000ULL,
	TOP_extr,		0x0a400002000ULL,
	TOP_UNDEFINED);

/* ===== I12: Zero and Deposit ===== */
  i12 = ISA_Pack_Type_Create("i12");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// r2
  Adjust_Operand(2, comp6, comp6); // cpos_6c = 63 - pos_6
  Operand(2, 0, 20, 6);		// cpos_6c
  Adjust_Operand(3, decr, incr); // len_6d = len_6 - 1
  Operand(3, 0, 27, 6);		// len_6d
  Instruction_Pack_Group(i12,
	TOP_dep_z,		0x0a600000000ULL,
	TOP_UNDEFINED);

/* ===== I13: Zero and Deposit Immediate_8 ===== */
  i13 = ISA_Pack_Type_Create("i13");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// imm_7b
  Operand(1, 7, 36, 1);		// s
  Adjust_Operand(2, comp6, comp6); // cpos_6c = 63 - pos_6
  Operand(2, 0, 20, 6);		// cpos_6c
  Adjust_Operand(3, decr, incr); // len_6d = len_6 - 1
  Operand(3, 0, 27, 6);		// len_6d
  Instruction_Pack_Group(i13,
	TOP_dep_i_z,		0x0a604000000ULL,
	TOP_UNDEFINED);

/* ===== I14: Deposit Immediate_1 ===== */
  i14 = ISA_Pack_Type_Create("i14");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 36, 1);		// s
  Operand(2, 0, 20, 7);		// r3
  Adjust_Operand(3, comp6, comp6); // cpos_6b = 63 - pos_6
  Operand(3, 0, 14, 6);		// cpos_6b
  Adjust_Operand(4, decr, incr); // len_6d = len_6 - 1;
  Operand(4, 0, 27, 6);		// len_6d
  Instruction_Pack_Group(i14,
	TOP_dep_i,		0x0ae00000000ULL,
	TOP_UNDEFINED);

/* ===== I15: Deposit ===== */
  i15 = ISA_Pack_Type_Create("i15");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// r2
  Operand(2, 0, 20, 7);		// r3
  Adjust_Operand(3, comp6, comp6); // cpos_6b = 63 - pos_6
  Operand(3, 0, 31, 6);		// cpos_6d
  Adjust_Operand(4, decr, incr); // len_4d = len_4 - 1;
  Operand(4, 0, 27, 4);		// len_4d
  Instruction_Pack_Group(i15,
	TOP_dep,		0x08000000000ULL,
	TOP_UNDEFINED);

/* ===== I16: Test Bit ===== */
  i16 = ISA_Pack_Type_Create("i16");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 6);		// p1
  Result(1, 27, 6);		// p2
  Operand(1, 0, 20, 7);		// r3
  Operand(2, 0, 14, 6);		// pos_6b
  Instruction_Pack_Group(i16,
	TOP_tbit_z,		0x0a000000000ULL,
	TOP_tbit_z_unc,		0x0a000001000ULL,
	TOP_tbit_z_and,		0x0b000000000ULL,
	TOP_tbit_nz_and,	0x0b000001000ULL,
	TOP_tbit_z_or,		0x0a200000000ULL,
	TOP_tbit_nz_or,		0x0a200001000ULL,
	TOP_tbit_z_or_andcm,	0x0b200000000ULL,
	TOP_tbit_nz_or_andcm,	0x0b200001000ULL,
	TOP_UNDEFINED);

/* ===== I17: Test NaT ===== */
  i17 = ISA_Pack_Type_Create("i17");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 6);		// p1
  Result(1, 27, 6);		// p2
  Operand(1, 0, 20, 7);		// r3
  Instruction_Pack_Group(i17,
	TOP_tnat_z,		0x0a000002000ULL,
	TOP_tnat_z_unc,		0x0a000003000ULL,
	TOP_tnat_z_and,		0x0b000002000ULL,
	TOP_tnat_nz_and,	0x0b000003000ULL,
	TOP_tnat_z_or,		0x0a200002000ULL,
	TOP_tnat_nz_or,		0x0a200003000ULL,
	TOP_tnat_z_or_andcm,	0x0b200002000ULL,
	TOP_tnat_nz_or_andcm,	0x0b200003000ULL,
	TOP_UNDEFINED);

/* ===== I19: Break/Nop ===== */
  i19 = ISA_Pack_Type_Create("i19");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 6, 20);		// imm_20a
  Operand(1, 20, 36, 1);	// i
  Instruction_Pack_Group(i19,
	TOP_break_i,		0x00000000000ULL,
	TOP_nop_i,		0x00008000000ULL,
	TOP_UNDEFINED);

/* ===== I20: Integer Speculation Check ===== */
  i20 = ISA_Pack_Type_Create("i20");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 13, 7);		// r2
// Adjust_Operand(2, shr4, shl4); // imm_21 = target_25 >> 4
  Operand(2, 4, 6, 7);		// imm_7a
  Operand(2, 11, 20, 13);	// imm_13c
  Operand(2, 24, 36, 1);	// s
  Instruction_Pack_Group(i20,
	TOP_chk_s_i,		0x00200000000ULL,
	TOP_UNDEFINED);

/* ===== I21: Move to BR ===== */
  i21 = ISA_Pack_Type_Create("i21");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 20, 2);		// mwh
  Operand(2, 0, 23, 1);		// ih
  Result(0, 6, 3);		// b1
  Operand(3, 0, 13, 7);		// r2
// Adjust_Operand(4, shr4, shl4); // timm_9c = tag_13 >> 4
  Operand(4, 4, 24, 9);		// timm_9c
  Instruction_Pack_Group(i21,
	TOP_mov_t_br_i,		0x00e00000000ULL,
	TOP_mov_t_br_ret,	0x00e00400000ULL,
	TOP_UNDEFINED);

/* ===== I22: Move from BR ===== */
  i22 = ISA_Pack_Type_Create("i22");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 3);		// b2
  Instruction_Pack_Group(i22,
	TOP_mov_f_br,		0x00188000000ULL,
	TOP_UNDEFINED);

/* ===== I23: Move to Predicates -- Register ===== */
  i23 = ISA_Pack_Type_Create("i23");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 13, 7);		// r2
// Adjust_Operand(2, shr1, shl1); // mask_16 = mask_17 >> 1
  Operand(2, 1, 6, 7);		// mask_7a
  Operand(2, 8, 24, 8);		// mask_8c
  Operand(2, 16, 36, 1);	// s
  Instruction_Pack_Group(i23,
	TOP_mov_t_pr,		0x00600000000ULL,
	TOP_UNDEFINED);

/* ===== I24: Move to Predicates -- Immediate_44 ===== */
  i24 = ISA_Pack_Type_Create("i24");
  Operand(0, 0, 0, 6);		// qp
// Adjust_Operand(1, shr16, shl16); // imm_28 = imm_44 >> 16
  Operand(1, 16, 6, 27);	// imm_27a
  Operand(1, 43, 36, 1);	// s
  Instruction_Pack_Group(i24,
	TOP_mov_t_pr_i,		0x00400000000ULL,
	TOP_UNDEFINED);

/* ===== I25: Move from Predicates/IP ===== */
  i25 = ISA_Pack_Type_Create("i25");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Instruction_Pack_Group(i25,
	TOP_mov_f_ip,		0x00180000000ULL,
	TOP_mov_f_pr,		0x00198000000ULL,
	TOP_UNDEFINED);

/* ===== I26: Move to AR -- Register ===== */
  i26 = ISA_Pack_Type_Create("i26");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 20, 7);		// ar3
  Operand(1, 0, 13, 7);		// r2
  Instruction_Pack_Group(i26,
	TOP_mov_t_ar_r_i,	0x00150000000ULL,
	TOP_UNDEFINED);

/* ===== I27: Move to AR -- Immediate_8 ===== */
  i27 = ISA_Pack_Type_Create("i27");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 20, 7);		// ar3
  Operand(1, 0, 13, 7);		// imm_7b
  Operand(1, 7, 36, 1);		// s
  Instruction_Pack_Group(i27,
	TOP_mov_t_ar_i_i,	0x00050000000ULL,
	TOP_UNDEFINED);

/* ===== I28: Move from AR ===== */
  i28 = ISA_Pack_Type_Create("i28");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 20, 7);		// ar3
  Instruction_Pack_Group(i28,
	TOP_mov_f_ar_i,		0x00190000000ULL,
	TOP_UNDEFINED);

/* ===== I29: Sign/Zero Extend/Compute Zero Index ===== */
  i29 = ISA_Pack_Type_Create("i29");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 20, 7);		// r3
  Instruction_Pack_Group(i29,
	TOP_zxt1,		0x00080000000ULL,
	TOP_zxt2,		0x00088000000ULL,
	TOP_zxt4,		0x00090000000ULL,
	TOP_sxt1,		0x000a0000000ULL,
	TOP_sxt2,		0x000a8000000ULL,
	TOP_sxt4,		0x000b0000000ULL,
	TOP_czx1_l,		0x000c0000000ULL,
	TOP_czx2_l,		0x000c8000000ULL,
	TOP_czx1_r,		0x000e0000000ULL,
	TOP_czx2_r,		0x000e8000000ULL,
	TOP_UNDEFINED);

/* ===== I30: Move to BR (pseudo) ===== */
  i30 = ISA_Pack_Type_Create("i30");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 3);		// b1
  Operand(1, 0, 13, 7);		// r2
  Instruction_Pack_Group(i30,
	TOP_mov_t_br,		0x00e00100000ULL,
	TOP_UNDEFINED);

/* ===== I31: Test Bit (pseudo) ===== */
  i31 = ISA_Pack_Type_Create("i31");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 27, 6);		// p2
  Result(1, 6, 6);		// p1
  Operand(1, 0, 20, 7);		// r3
  Operand(2, 0, 14, 6);		// pos_6b
  Instruction_Pack_Group(i31,
	TOP_tbit_nz,		0x0a000000000ULL,
	TOP_tbit_nz_unc,	0x0a000001000ULL,
	TOP_UNDEFINED);

/* ===== I32: Test NaT (pseudo) ===== */
  i32 = ISA_Pack_Type_Create("i32");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 27, 6);		// p2
  Result(1, 6, 6);		// p1
  Operand(1, 0, 20, 7);		// r3
  Instruction_Pack_Group(i32,
	TOP_tnat_nz,		0x0a000002000ULL,
	TOP_tnat_nz_unc,	0x0a000003000ULL,
	TOP_UNDEFINED);

/* ===== I33: Shift Left Immediate (pseudo) ===== */
// NOTE: this packing description is never used (because of the
// 64-count transformation required), it must be lowered to a 
// machine inst instead.
  i33 = ISA_Pack_Type_Create("i33");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// r2
  Operand(2, 0, 20, 6);		// cpos_6c
  Operand(2, 0, 27, 6);		// len_6d
  Instruction_Pack_Group(i33,
	TOP_shl_i,		0x0a600000000ULL,
	TOP_UNDEFINED);

/* ===== I34: Shift Right Immediate (pseudo) ===== */
// NOTE: this packing description is never used (because of the
// 64-count transformation required), it must be lowered to a 
// machine inst instead.
  i34 = ISA_Pack_Type_Create("i34");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 20, 7);		// r3
  Operand(2, 0, 14, 6);		// pos_6b
  Operand(2, 0, 27, 6);		// len_6d
  Instruction_Pack_Group(i34,
	TOP_shr_i_u,		0x0a400000000ULL,
	TOP_shr_i,		0x0a400002000ULL,
	TOP_UNDEFINED);

/* ===== M1a: Integer Load ===== */
  m1a = ISA_Pack_Type_Create("m1a");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 32, 4);		// ldtype
  Operand(2, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// r1
  Operand(3, 0, 20, 7);		// r3
  Instruction_Pack_Group(m1a,
	TOP_ld1,		0x08000000000ULL,
	TOP_ld2,		0x08040000000ULL,
	TOP_ld4,		0x08080000000ULL,
	TOP_ld8,		0x080c0000000ULL,
	TOP_UNDEFINED);

/* ===== M1b: Integer Load ===== */
  m1b = ISA_Pack_Type_Create("m1b");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// r1
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(m1b,
	TOP_ld8_fill,		0x086c0000000ULL,
	TOP_UNDEFINED);

/* ===== M2a: Integer Load -- Increment by Register ===== */
  m2a = ISA_Pack_Type_Create("m2a");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 32, 4);		// ldtype
  Operand(2, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// r1
  Operand(3, 0, 20, 7);		// r3
  Operand(4, 0, 13, 7);		// r2
  Instruction_Pack_Group(m2a,
	TOP_ld1_r,		0x09000000000ULL,
	TOP_ld2_r,		0x09040000000ULL,
	TOP_ld4_r,		0x09080000000ULL,
	TOP_ld8_r,		0x090c0000000ULL,
	TOP_UNDEFINED);

/* ===== M2b: Integer Load -- Increment by Register ===== */
  m2b = ISA_Pack_Type_Create("m2b");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// r1
  Operand(2, 0, 20, 7);		// r3
  Operand(3, 0, 13, 7);		// r2
  Instruction_Pack_Group(m2b,
	TOP_ld8_r_fill,		0x096c0000000ULL,
	TOP_UNDEFINED);

/* ===== M3a: Integer Load -- Increment by Immediate ===== */
  m3a = ISA_Pack_Type_Create("m3a");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 32, 4);		// ldtype
  Operand(2, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// r1
  Operand(3, 0, 20, 7);		// r3
  Operand(4, 0, 13, 7);		// imm_7b
  Operand(4, 7, 27, 1);		// i
  Operand(4, 8, 36, 1);		// s
  Instruction_Pack_Group(m3a,
	TOP_ld1_i,		0x0a000000000ULL,
	TOP_ld2_i,		0x0a040000000ULL,
	TOP_ld4_i,		0x0a080000000ULL,
	TOP_ld8_i,		0x0a0c0000000ULL,
	TOP_UNDEFINED);

/* ===== M3b: Integer Load -- Increment by Immediate ===== */
  m3b = ISA_Pack_Type_Create("m3b");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// r1
  Operand(2, 0, 20, 7);		// r3
  Operand(3, 0, 13, 7);		// imm_7b
  Operand(3, 7, 27, 1);		// i
  Operand(3, 8, 36, 1);		// s
  Instruction_Pack_Group(m3b,
	TOP_ld8_i_fill,		0x0a6c0000000ULL,
	TOP_UNDEFINED);

/* ===== M4a: Integer Store ===== */
  m4a = ISA_Pack_Type_Create("m4a");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 32, 1);		// sttype
  Operand(2, 0, 28, 2);		// sthint
  Operand(3, 0, 20, 7);		// r3
  Operand(4, 0, 13, 7);		// r2
  Instruction_Pack_Group(m4a,
	TOP_st1,		0x08c00000000ULL,
	TOP_st2,		0x08c40000000ULL,
	TOP_st4,		0x08c80000000ULL,
	TOP_st8,		0x08cc0000000ULL,
	TOP_UNDEFINED);

/* ===== M4b: Integer Store ===== */
  m4b = ISA_Pack_Type_Create("m4b");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 28, 2);		// sthint
  Operand(2, 0, 20, 7);		// r3
  Operand(3, 0, 13, 7);		// r2
  Instruction_Pack_Group(m4b,
	TOP_st8_spill,		0x08ec0000000ULL,
	TOP_UNDEFINED);

/* ===== M5a: Integer Store -- Increment by Immediate ===== */
  m5a = ISA_Pack_Type_Create("m5a");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 32, 1);		// sttype
  Operand(2, 0, 28, 2);		// sthint
  Operand(3, 0, 20, 7);		// r3
  Operand(4, 0, 13, 7);		// r2
  Operand(5, 0, 6, 7);		// imm_7a
  Operand(5, 7, 27, 1);		// i
  Operand(5, 8, 36, 1);		// s
  Instruction_Pack_Group(m5a,
	TOP_st1_i,		0x0ac00000000ULL,
	TOP_st2_i,		0x0ac40000000ULL,
	TOP_st4_i,		0x0ac80000000ULL,
	TOP_st8_i,		0x0acc0000000ULL,
	TOP_UNDEFINED);

/* ===== M5b: Integer Store -- Increment by Immediate ===== */
  m5b = ISA_Pack_Type_Create("m5b");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 28, 2);		// sthint
  Operand(2, 0, 20, 7);		// r3
  Operand(3, 0, 13, 7);		// r2
  Operand(4, 0, 6, 7);		// imm_7a
  Operand(4, 7, 27, 1);		// i
  Operand(4, 8, 36, 1);		// s
  Instruction_Pack_Group(m5b,
	TOP_st8_i_spill,	0x0aec0000000ULL,
	TOP_UNDEFINED);

/* ===== M6a: Floating-point Load ===== */
  m6a = ISA_Pack_Type_Create("m6a");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 32, 4);		// fldtype
  Operand(2, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// f1
  Operand(3, 0, 20, 7);		// r3
  Instruction_Pack_Group(m6a,
	TOP_ldfs,		0x0c080000000ULL,
	TOP_ldfd,		0x0c0c0000000ULL,
	TOP_ldf8,		0x0c040000000ULL,
	TOP_ldfe,		0x0c000000000ULL,
	TOP_UNDEFINED);

/* ===== M6b: Floating-point Load ===== */
  m6b = ISA_Pack_Type_Create("m6b");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// f1
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(m6b,
	TOP_ldf_fill,		0x0c6c0000000ULL,
	TOP_UNDEFINED);

/* ===== M7a: Floating-point Load -- Increment by Register ===== */
  m7a = ISA_Pack_Type_Create("m7a");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 32, 4);		// fldtype
  Operand(2, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// f1
  Operand(3, 0, 20, 7);		// r3
  Operand(4, 0, 13, 7);		// r2
  Instruction_Pack_Group(m7a,
	TOP_ldfs_r,		0x0d080000000ULL,
	TOP_ldfd_r,		0x0d0c0000000ULL,
	TOP_ldf8_r,		0x0d040000000ULL,
	TOP_ldfe_r,		0x0d000000000ULL,
	TOP_UNDEFINED);

/* ===== M7b: Floating-point Load -- Increment by Register ===== */
  m7b = ISA_Pack_Type_Create("m7b");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// f1
  Operand(2, 0, 20, 7);		// r3
  Operand(3, 0, 13, 7);		// r2
  Instruction_Pack_Group(m7b,
	TOP_ldf_r_fill,		0x0d6c0000000ULL,
	TOP_UNDEFINED);

/* ===== M8a: Floating-point Load -- Increment by Immediate ===== */
  m8a = ISA_Pack_Type_Create("m8a");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 32, 4);		// fldtype
  Operand(2, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// f1
  Operand(3, 0, 20, 7);		// r3
  Operand(4, 0, 13, 7);		// imm_7a
  Operand(4, 7, 27, 1);		// i
  Operand(4, 8, 36, 1);		// s
  Instruction_Pack_Group(m8a,
	TOP_ldfs_i,		0x0e080000000ULL,
	TOP_ldfd_i,		0x0e0c0000000ULL,
	TOP_ldf8_i,		0x0e040000000ULL,
	TOP_ldfe_i,		0x0e000000000ULL,
	TOP_UNDEFINED);

/* ===== M8b: Floating-point Load -- Increment by Immediate ===== */
  m8b = ISA_Pack_Type_Create("m8b");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// f1
  Operand(2, 0, 20, 7);		// r3
  Operand(3, 0, 13, 7);		// imm_7a
  Operand(3, 7, 27, 1);		// i
  Operand(3, 8, 36, 1);		// s
  Instruction_Pack_Group(m8b,
	TOP_ldf_i_fill,		0x0e6c0000000ULL,
	TOP_UNDEFINED);

/* ===== M9: Floating-point Store ===== */
  m9 = ISA_Pack_Type_Create("m9");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 28, 2);		// sthint
  Operand(2, 0, 20, 7);		// r3
  Operand(3, 0, 13, 7);		// f2
  Instruction_Pack_Group(m9,
	TOP_stfs,		0x0cc80000000ULL,
	TOP_stfd,		0x0ccc0000000ULL,
	TOP_stf8,		0x0cc40000000ULL,
	TOP_stfe,		0x0cc00000000ULL,
	TOP_stf_spill,		0x0cec0000000ULL,
	TOP_UNDEFINED);

/* ===== M10: Floating-point Store -- Increment by Immediate ===== */
  m10 = ISA_Pack_Type_Create("m10");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 28, 2);		// sthint
  Operand(2, 0, 20, 7);		// r3
  Operand(3, 0, 13, 7);		// f2
  Operand(4, 0, 6, 7);		// imm_7a
  Operand(4, 7, 27, 1);		// i
  Operand(4, 8, 36, 1);		// s
  Instruction_Pack_Group(m10,
	TOP_stfs_i,		0x0ec80000000ULL,
	TOP_stfd_i,		0x0ecc0000000ULL,
	TOP_stf8_i,		0x0ec40000000ULL,
	TOP_stfe_i,		0x0ec00000000ULL,
	TOP_stf_i_spill,	0x0eec0000000ULL,
	TOP_UNDEFINED);

/* ===== M11: Floating-point Load Pair ===== */
  m11 = ISA_Pack_Type_Create("m11");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 32, 4);		// fldtype
  Operand(2, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// f1
  Result(1, 13, 7);		// f2
  Operand(3, 0, 20, 7);		// r3
  Instruction_Pack_Group(m11,
	TOP_ldfps,		0x0c088000000ULL,
	TOP_ldfpd,		0x0c0c8000000ULL,
	TOP_ldfp8,		0x0c048000000ULL,
	TOP_UNDEFINED);

/* ===== M12: Floating-point Load Pair -- Increment by Immediate ===== */
  m12 = ISA_Pack_Type_Create("m12");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 32, 4);		// fldtype
  Operand(2, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// f1
  Result(1, 13, 7);		// f2
  Operand(3, 0, 20, 7);		// r3
  Instruction_Pack_Group(m12,
	TOP_ldfps_i,		0x0d088000000ULL,
	TOP_ldfpd_i,		0x0d0c8000000ULL,
	TOP_ldfp8_i,		0x0d048000000ULL,
	TOP_UNDEFINED);

/* ===== M13: Line Prefetch ===== */
  m13 = ISA_Pack_Type_Create("m13");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 28, 2);		// lfhint
  Operand(2, 0, 20, 7);		// r3
  Instruction_Pack_Group(m13,
	TOP_lfetch,		0x0cb00000000ULL,
	TOP_lfetch_excl,	0x0cb40000000ULL,
	TOP_lfetch_fault,	0x0cb80000000ULL,
	TOP_lfetch_fault_excl,	0x0cbc0000000ULL,
	TOP_UNDEFINED);

/* ===== M14: Line Prefetch -- Increment by Register ===== */
  m14 = ISA_Pack_Type_Create("m14");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 28, 2);		// lfhint
  Operand(2, 0, 20, 7);		// r3
  Operand(3, 0, 13, 7);		// r2
  Instruction_Pack_Group(m14,
	TOP_lfetch_r,		0x0db00000000ULL,
	TOP_lfetch_r_excl,	0x0db40000000ULL,
	TOP_lfetch_r_fault,	0x0db80000000ULL,
	TOP_lfetch_r_fault_excl,0x0dbc0000000ULL,
	TOP_UNDEFINED);

/* ===== M15: Line Prefetch -- Increment by Immediate ===== */
  m15 = ISA_Pack_Type_Create("m15");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 28, 2);		// lfhint
  Operand(2, 0, 20, 7);		// r3
  Operand(3, 0, 13, 7);		// imm_7b
  Operand(3, 7, 27, 1);		// i
  Operand(3, 8, 36, 1);		// s
  Instruction_Pack_Group(m15,
	TOP_lfetch_i,		0x0eb00000000ULL,
	TOP_lfetch_i_excl,	0x0eb40000000ULL,
	TOP_lfetch_i_fault,	0x0eb80000000ULL,
	TOP_lfetch_i_fault_excl,0x0ebc0000000ULL,
	TOP_UNDEFINED);

/* ===== M16a: Exchange/Compare and Exchange ===== */
  m16a = ISA_Pack_Type_Create("m16a");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 32, 1);		// sem
  Operand(2, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// r1
  Operand(3, 0, 20, 7);		// r3
  Operand(4, 0, 13, 7);		// r2
  Instruction_Pack_Group(m16a,
	TOP_cmpxchg1,		0x08008000000ULL,
	TOP_cmpxchg2,		0x08048000000ULL,
	TOP_cmpxchg4,		0x08088000000ULL,
	TOP_cmpxchg8,		0x080c8000000ULL,
	TOP_UNDEFINED);

/* ===== M16b: Exchange/Compare and Exchange ===== */
  m16b = ISA_Pack_Type_Create("m16b");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// r1
  Operand(2, 0, 20, 7);		// r3
  Operand(3, 0, 13, 7);		// r2
  Instruction_Pack_Group(m16b,
	TOP_xchg1,		0x08208000000ULL,
	TOP_xchg2,		0x08248000000ULL,
	TOP_xchg4,		0x08288000000ULL,
	TOP_xchg8,		0x082c8000000ULL,
	TOP_UNDEFINED);

/* ===== M17: Fetch and Add -- Immediate ===== */
  m17 = ISA_Pack_Type_Create("m17");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 32, 1);		// sem
  Operand(2, 0, 28, 2);		// ldhint
  Result(0, 6, 7);		// r1
  Operand(3, 0, 20, 7);		// r3
  Adjust_Operand(4, pack_m17, unpack_m17);
  Operand(4, 0, 13, 3);		// i_2b/s
  Instruction_Pack_Group(m17,
	TOP_fetchadd4,		0x08488000000ULL,
	TOP_fetchadd8,		0x084c8000000ULL,
	TOP_UNDEFINED);

/* ===== M18: Set FR ===== */
  m18 = ISA_Pack_Type_Create("m18");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// f1
  Operand(1, 0, 13, 7);		// r2
  Instruction_Pack_Group(m18,
	TOP_setf_sig,		0x0c708000000ULL,
	TOP_setf_exp,		0x0c748000000ULL,
	TOP_setf_s,		0x0c788000000ULL,
	TOP_setf_d,		0x0c7c8000000ULL,
	TOP_UNDEFINED);

/* ===== M19: Get FR ===== */
  m19 = ISA_Pack_Type_Create("m19");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// f2
  Instruction_Pack_Group(m19,
	TOP_getf_sig,		0x08708000000ULL,
	TOP_getf_exp,		0x08748000000ULL,
	TOP_getf_s,		0x08788000000ULL,
	TOP_getf_d,		0x087c8000000ULL,
	TOP_UNDEFINED);

/* ===== M20: Integer Speculation Check ===== */
  m20 = ISA_Pack_Type_Create("m20");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 13, 7);		// r2
// Adjust_Operand(2, shr4, shl4); // imm_21 = target_25 >> 4
  Operand(2, 4, 6, 7);		// imm_7a
  Operand(2, 11, 20, 13);	// imm_13c
  Operand(2, 24, 36, 1);	// s
  Instruction_Pack_Group(m20,
	TOP_chk_s_m,		0x02200000000ULL,
	TOP_UNDEFINED);

/* ===== M21: Floating-point Speculation Check ===== */
  m21 = ISA_Pack_Type_Create("m21");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 13, 7);		// f2
// Adjust_Operand(2, shr4, shl4); // imm_21 = target_25 >> 4
  Operand(2, 4, 6, 7);		// imm_7a
  Operand(2, 11, 20, 13);	// imm_13c
  Operand(2, 24, 36, 1);	// s
  Instruction_Pack_Group(m21,
	TOP_chk_f_s,		0x02600000000ULL,
	TOP_UNDEFINED);

/* ===== M22: Integer Advanced Load Check ===== */
  m22 = ISA_Pack_Type_Create("m22");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 33, 1);		// aclr
  Operand(2, 0, 6, 7);		// r1
// Adjust_Operand(3, shr4, shl4); // imm_21 = target_25 >> 4
  Operand(3, 4, 13, 20);	// imm_20b
  Operand(3, 24, 36, 1);	// s
  Instruction_Pack_Group(m22,
	TOP_chk_a,		0x00800000000ULL,
	TOP_UNDEFINED);

/* ===== M23: Floating-point Advanced Load Check ===== */
  m23 = ISA_Pack_Type_Create("m23");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 33, 1);		// aclr
  Operand(2, 0, 6, 7);		// f1
// Adjust_Operand(3, shr4, shl4); // imm_21 = target_25 >> 4
  Operand(3, 4, 13, 20);	// imm_20b
  Operand(3, 24, 36, 1);	// s
  Instruction_Pack_Group(m23,
	TOP_chk_f_a,		0x00c00000000ULL,
	TOP_UNDEFINED);

/* ===== M24: Sync/Fence/Serialize/ALAT Control ===== */
  m24 = ISA_Pack_Type_Create("m24");
  Operand(0, 0, 0, 6);		// qp
  Instruction_Pack_Group(m24,
	TOP_invala,		0x00080000000ULL,
	TOP_fwb,		0x00100000000ULL,
	TOP_mf,			0x00110000000ULL,
	TOP_mf_a,		0x00118000000ULL,
	TOP_srlz_d,		0x00180000000ULL,
	TOP_srlz_i,		0x00188000000ULL,
	TOP_sync_i,		0x00198000000ULL,
	TOP_UNDEFINED);

/* ===== M25: RSE Control ===== */
  m25 = ISA_Pack_Type_Create("m25");
  Instruction_Pack_Group(m25,
	TOP_flushrs,		0x00060000000ULL,
	TOP_loadrs,		0x00050000000ULL,
	TOP_UNDEFINED);

/* ===== M26: Integer ALAT Entry Invalidate ===== */
  m26 = ISA_Pack_Type_Create("m26");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 6, 7);		// r1
  Instruction_Pack_Group(m26,
	TOP_invala_e,		0x00090000000ULL,
	TOP_UNDEFINED);

/* ===== M27: Floating-point ALAT Entry Invalidate ===== */
  m27 = ISA_Pack_Type_Create("m27");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 6, 7);		// f1
  Instruction_Pack_Group(m27,
	TOP_invala_f_e,		0x00098000000ULL,
	TOP_UNDEFINED);

/* ===== M28: Flush Cache/Purge Translation Cache Entry ===== */
  m28 = ISA_Pack_Type_Create("m28");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 20, 7);		// r3
  Instruction_Pack_Group(m28,
	TOP_fc,			0x02180000000ULL,
	TOP_ptc_e,		0x021a0000000ULL,
	TOP_UNDEFINED);

/* ===== M29: Move to AR -- Register ===== */
  m29 = ISA_Pack_Type_Create("m29");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 20, 7);		// ar3
  Operand(1, 0, 13, 7);		// r2
  Instruction_Pack_Group(m29,
	TOP_mov_t_ar_r_m,	0x02150000000ULL,
	TOP_UNDEFINED);

/* ===== M30: Move to AR -- Immediate_8 ===== */
  m30 = ISA_Pack_Type_Create("m30");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 20, 7);		// ar3
  Operand(1, 0, 13, 7);		// imm_7b
  Operand(1, 7, 36, 1);		// s
  Instruction_Pack_Group(m30,
	TOP_mov_t_ar_i_m,	0x00140000000ULL,
	TOP_UNDEFINED);

/* ===== M31: Move from AR ===== */
  m31 = ISA_Pack_Type_Create("m31");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 20, 7);		// ar3
  Instruction_Pack_Group(m31,
	TOP_mov_f_ar_m,		0x02110000000ULL,
	TOP_UNDEFINED);

/* ===== M32: Move to CR ===== */
  m32 = ISA_Pack_Type_Create("m32");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 20, 7);		// cr3
  Operand(1, 0, 13, 7);		// r2
  Instruction_Pack_Group(m32,
	TOP_mov_t_cr,		0x02160000000ULL,
	TOP_UNDEFINED);

/* ===== M33: Move from CR ===== */
  m33 = ISA_Pack_Type_Create("m33");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 20, 7);		// cr3
  Instruction_Pack_Group(m33,
	TOP_mov_f_cr,		0x02120000000ULL,
	TOP_UNDEFINED);

/* ===== M34: Allocate Register Stack Frame ===== */
  m34 = ISA_Pack_Type_Create("m34");
  Result(0, 6, 7);		// r1
  Operand(0, 0, 13, 7);		// sof
  Operand(1, 0, 20, 7);		// sol
  Operand(2, 3, 27, 4);		// sor >> 3
  Instruction_Pack_Group(m34,
	TOP_alloc_3,		0x02c00000000ULL,
	TOP_UNDEFINED);

/* ===== M35: Move to PSR ===== */
  m35 = ISA_Pack_Type_Create("m35");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 13, 7);		// r2
  Instruction_Pack_Group(m35,
	TOP_mov_t_psr,		0x02168000000ULL,
	TOP_mov_t_psrum,	0x02148000000ULL,
	TOP_UNDEFINED);

/* ===== M36: Move from PSR ===== */
  m36 = ISA_Pack_Type_Create("m36");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Instruction_Pack_Group(m36,
	TOP_mov_f_psr,		0x02128000000ULL,
	TOP_mov_f_psrum,	0x02108000000ULL,
	TOP_UNDEFINED);

/* ===== M37: Break/Nop ===== */
  m37 = ISA_Pack_Type_Create("m37");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 6, 20);		// imm_20a
  Operand(1, 20, 36, 1);	// i
  Instruction_Pack_Group(m37,
	TOP_break_m,		0x00000000000ULL,
	TOP_nop_m,		0x00008000000ULL,
	TOP_UNDEFINED);

/* ===== M38: Probe -- Register ===== */
  m38 = ISA_Pack_Type_Create("m38");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 20, 7);		// r3
  Operand(2, 0, 13, 7);		// r2
  Instruction_Pack_Group(m38,
	TOP_probe_r,		0x021c0000000ULL,
	TOP_probe_w,		0x021c8000000ULL,
	TOP_UNDEFINED);

/* ===== M39: Probe -- Immediate_2 ===== */
  m39 = ISA_Pack_Type_Create("m39");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 20, 7);		// r3
  Operand(2, 0, 13, 2);		// i_2b
  Instruction_Pack_Group(m39,
	TOP_probe_i_r,		0x020c0000000ULL,
	TOP_probe_i_w,		0x020c8000000ULL,
	TOP_UNDEFINED);

/* ===== M40: Probe Fault ===== */
  m40 = ISA_Pack_Type_Create("m40");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 20, 7);		// r3
  Operand(2, 0, 13, 2);		// i_2b
  Instruction_Pack_Group(m40,
	TOP_probe_r_fault,	0x02190000000ULL,
	TOP_probe_w_fault,	0x02198000000ULL,
	TOP_probe_rw_fault,	0x02188000000ULL,
	TOP_UNDEFINED);

/* ===== M41: Translation Cache Insert ===== */
  m41 = ISA_Pack_Type_Create("m41");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 13, 7);		// r2
  Instruction_Pack_Group(m41,
	TOP_itc_d,		0x02170000000ULL,
	TOP_itc_i,		0x02178000000ULL,
	TOP_UNDEFINED);

/* ===== M42: Move to Indirect Register/Translation Register Insert ===== */
  m42 = ISA_Pack_Type_Create("m42");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 20, 7);		// r3
  Operand(2, 0, 13, 7);		// r2
  Instruction_Pack_Group(m42,
	TOP_mov_t_rr,		0x02000000000ULL,
	TOP_mov_t_dbr,		0x02008000000ULL,
	TOP_mov_t_ibr,		0x02010000000ULL,
	TOP_mov_t_pkr,		0x02018000000ULL,
	TOP_mov_t_pmc,		0x02020000000ULL,
	TOP_mov_t_pmd,		0x02028000000ULL,
	TOP_mov_t_msr,		0x02030000000ULL,
	TOP_itr_d,		0x02070000000ULL,
	TOP_itr_i,		0x02078000000ULL,
	TOP_UNDEFINED);

/* ===== M43: Move from Indirect Register ===== */
  m43 = ISA_Pack_Type_Create("m43");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 20, 7);		// r3
  Instruction_Pack_Group(m43,
	TOP_mov_f_rr,		0x02080000000ULL,
	TOP_mov_f_dbr,		0x02088000000ULL,
	TOP_mov_f_ibr,		0x02090000000ULL,
	TOP_mov_f_pkr,		0x02098000000ULL,
	TOP_mov_f_pmc,		0x020a0000000ULL,
	TOP_mov_f_pmd,		0x020a8000000ULL,
	TOP_mov_f_msr,		0x020b0000000ULL,
	TOP_mov_f_cpuid,	0x020b8000000ULL,
	TOP_UNDEFINED);

/* ===== M44: Set/Reset User/System Mask ===== */
  m44 = ISA_Pack_Type_Create("m44");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 6, 21);		// imm_21a
  Operand(1, 21, 31, 2);	// i_2d
  Operand(1, 23, 36, 1);	// i
  Instruction_Pack_Group(m44,
	TOP_sum,		0x00020000000ULL,
	TOP_rum,		0x00028000000ULL,
	TOP_ssm,		0x00030000000ULL,
	TOP_rsm,		0x00038000000ULL,
	TOP_UNDEFINED);

/* ===== M45: Translation Purge ===== */
  m45 = ISA_Pack_Type_Create("m45");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 20, 7);		// r3
  Operand(2, 0, 13, 7);		// r2
  Instruction_Pack_Group(m45,
	TOP_ptc_l,		0x02048000000ULL,
	TOP_ptc_g,		0x02050000000ULL,
	TOP_ptc_ga,		0x02058000000ULL,
	TOP_ptr_d,		0x02060000000ULL,
	TOP_ptr_i,		0x02068000000ULL,
	TOP_UNDEFINED);

/* ===== M46: Translation Access ===== */
  m46 = ISA_Pack_Type_Create("m46");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 20, 7);		// r3
  Instruction_Pack_Group(m46,
	TOP_thash,		0x020d0000000ULL,
	TOP_ttag,		0x020d8000000ULL,
	TOP_tpa,		0x020f0000000ULL,
	TOP_tak,		0x020f8000000ULL,
	TOP_UNDEFINED);

/* ===== M47: Allocate Register Stack Frame (pseudo) ===== */
// NOTE: this packing description is never used (because of the
// operand transformations required), it must be lowered to a 
// machine inst instead.
  m47 = ISA_Pack_Type_Create("m47");
  Result(0, 6, 7);		// r1
  Operand(0, 0, 13, 7);		// i
  Operand(1, 0, 20, 7);		// l
  Operand(2, 0, 0, 0);		// o
  Operand(3, 0, 27, 4);		// r
  Instruction_Pack_Group(m47,
	TOP_alloc,		0x02c00000000ULL,
	TOP_UNDEFINED);

/* ===== B1: IP-Relative Branch ===== */
  b1 = ISA_Pack_Type_Create("b1");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 33, 2);		// bwh
  Operand(2, 0, 12, 1);		// ph
  Operand(3, 0, 35, 1);		// dh
// Adjust_Operand(4, shr4, shl4); // imm_21 = target_25 >> 4
  Operand(4, 4, 13, 20);	// imm_20b
  Operand(4, 24, 36, 1);	// s
  Instruction_Pack_Group(b1,
	TOP_br_cond,		0x08000000000ULL,
	TOP_br_wexit,		0x08000000080ULL,
	TOP_br_wtop,		0x080000000c0ULL,
	TOP_UNDEFINED);

/* ===== B2: IP-Relative Counted Branch ===== */
  b2 = ISA_Pack_Type_Create("b2");
  Operand(0, 0, 33, 2);		// bwh
  Operand(1, 0, 12, 1);		// ph
  Operand(2, 0, 35, 1);		// dh
// Adjust_Operand(3, shr4, shl4); // imm_21 = target_25 >> 4
  Operand(3, 4, 13, 20);	// imm_20b
  Operand(3, 24, 36, 1);	// s
  Instruction_Pack_Group(b2,
	TOP_br_cloop,		0x08000000140ULL,
	TOP_br_cexit,		0x08000000180ULL,
	TOP_br_ctop,		0x080000001c0ULL,
	TOP_UNDEFINED);

/* ===== B3: IP-Relative Call ===== */
  b3 = ISA_Pack_Type_Create("b3");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 33, 2);		// bwh
  Operand(2, 0, 12, 1);		// ph
  Operand(3, 0, 35, 1);		// dh
  Result(0, 6, 3);		// b1
// Adjust_Operand(4, shr4, shl4); // imm_21 = target_25 >> 4
  Operand(4, 4, 13, 20);	// imm_20b
  Operand(4, 24, 36, 1);	// s
  Instruction_Pack_Group(b3,
	TOP_br_call,		0x0a000000000ULL,
	TOP_UNDEFINED);

/* ===== B4: Indirect Branch ===== */
  b4 = ISA_Pack_Type_Create("b4");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 33, 2);		// bwh
  Operand(2, 0, 12, 1);		// ph
  Operand(3, 0, 35, 1);		// dh
  Operand(4, 0, 13, 3);		// b2
  Instruction_Pack_Group(b4,
	TOP_br_r_cond,		0x00100000000ULL,
	TOP_br_ret,		0x00108000100ULL,
	TOP_UNDEFINED);

/* ===== B4a: Indirect Branch ===== */
  b4a = ISA_Pack_Type_Create("b4a");
  Operand(0, 0, 33, 2);		// bwh
  Operand(1, 0, 12, 1);		// ph
  Operand(2, 0, 35, 1);		// dh
  Operand(3, 0, 13, 3);		// b2
  Instruction_Pack_Group(b4a,
	TOP_br_ia,		0x00100000040ULL,
	TOP_UNDEFINED);

/* ===== B5: Indirect Call ===== */
  b5 = ISA_Pack_Type_Create("b5");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 33, 2);		// bwh
  Operand(2, 0, 12, 1);		// ph
  Operand(3, 0, 35, 1);		// dh
  Result(0, 6, 3);		// b1
  Operand(4, 0, 13, 3);		// b2
  Instruction_Pack_Group(b5,
	TOP_br_r_call,		0x02000000000ULL,
	TOP_UNDEFINED);

/* ===== B6: IP-Relative Predict ===== */
  b6 = ISA_Pack_Type_Create("b6");
  Operand(0, 0, 3, 2);		// ipwh
  Operand(1, 0, 35, 1);		// ih
// Adjust_Operand(2, shr4, shl4); // imm_21 = target_25 >> 4
  Operand(2, 4, 13, 20);	// imm_20b
  Operand(2, 24, 36, 1);	// s
// Adjust_Operand(3, shr4, shl4); // timm_9 = tag_13 >> 4
  Operand(3, 4, 6, 7);		// timm_7a
  Operand(3, 11, 33, 2);	// t_2e
  Instruction_Pack_Group(b6,
	TOP_brp,		0x0e000000000ULL,
	TOP_UNDEFINED);

/* ===== B7: Indirect Predict ===== */
  b7 = ISA_Pack_Type_Create("b7");
  Operand(0, 0, 3, 2);		// indwh
  Operand(1, 0, 35, 1);		// ih
  Operand(2, 0, 13, 3);		// b2
// Adjust_Operand(3, shr4, shl4); // timm_9 = tag_13 >> 4
  Operand(3, 4, 6, 7);		// timm_7a
  Operand(3, 11, 33, 2);	// t_2e
  Instruction_Pack_Group(b7,
	TOP_brp_r,		0x04080000000ULL,
	TOP_brp_ret,		0x04088000000ULL,
	TOP_UNDEFINED);

/* ===== B8: Miscellaneous ===== */
  b8 = ISA_Pack_Type_Create("b8");
  Instruction_Pack_Group(b8,
	TOP_cover,		0x00010000000ULL,
	TOP_clrrrb,		0x00020000000ULL,
	TOP_clrrrb_pr,		0x00028000000ULL,
	TOP_rfi,		0x00040000000ULL,
	TOP_bsw_0,		0x00060000000ULL,
	TOP_bsw_1,		0x00068000000ULL,
	TOP_epc,		0x00080000000ULL,
	TOP_UNDEFINED);

/* ===== B9: Break/Nop ===== */
  b9 = ISA_Pack_Type_Create("b9");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 6, 20);		// imm_20a
  Operand(1, 20, 36, 1);	// i
  Instruction_Pack_Group(b9,
	TOP_break_b,		0x00000000000ULL,
	TOP_nop_b,		0x04000000000ULL,
	TOP_UNDEFINED);

/* ===== B10: IP-Relative Unconditional Branch (pseudo) ===== */
  b10 = ISA_Pack_Type_Create("b10");
  Operand(0, 0, 12, 1);		// ph
  Operand(1, 0, 35, 1);		// dh
// Adjust_Operand(2, shr4, shl4); // imm_21 = target_25 >> 4
  Operand(2, 4, 13, 20);	// imm_20b
  Operand(2, 24, 36, 1);	// s
  Instruction_Pack_Group(b10,
	TOP_br,			0x08000000000ULL,
	TOP_UNDEFINED);

/* ===== B11: Indirect Unconditional Branch (pseudo) ===== */
  b11 = ISA_Pack_Type_Create("b11");
  Operand(0, 0, 12, 1);		// ph
  Operand(1, 0, 35, 1);		// dh
  Operand(2, 0, 13, 3);		// b2
  Instruction_Pack_Group(b11,
	TOP_br_r,		0x00100000000ULL,
	TOP_UNDEFINED);

/* ===== F1: Floating-point Multiply Add ===== */
  f1 = ISA_Pack_Type_Create("f1");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 34, 2);		// sf
  Result(0, 6, 7);		// f1
  Operand(2, 0, 20, 7);		// f3
  Operand(3, 0, 27, 7);		// f4
  Operand(4, 0, 13, 7);		// f2
  Instruction_Pack_Group(f1,
	TOP_fma,		0x10000000000ULL,
	TOP_fma_s,		0x11000000000ULL,
	TOP_fma_d,		0x12000000000ULL,
	TOP_fpma,		0x13000000000ULL,
	TOP_fms,		0x14000000000ULL,
	TOP_fms_s,		0x15000000000ULL,
	TOP_fms_d,		0x16000000000ULL,
	TOP_fpms,		0x17000000000ULL,
	TOP_fnma,		0x18000000000ULL,
	TOP_fnma_s,		0x19000000000ULL,
	TOP_fnma_d,		0x1a000000000ULL,
	TOP_fpnma,		0x1b000000000ULL,
	TOP_UNDEFINED);

/* ===== F2: Fixed-point Multiply Add ===== */
  f2 = ISA_Pack_Type_Create("f2");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// f1
  Operand(1, 0, 20, 7);		// f3
  Operand(2, 0, 27, 7);		// f4
  Operand(3, 0, 13, 7);		// f2
  Instruction_Pack_Group(f2,
	TOP_xma_l,		0x1d000000000ULL,
	TOP_xma_h,		0x1dc00000000ULL,
	TOP_xma_hu,		0x1d800000000ULL,
	TOP_UNDEFINED);

/* ===== F3: Parallel Floating-point Select ===== */
  f3 = ISA_Pack_Type_Create("f3");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// f1
  Operand(1, 0, 20, 7);		// f3
  Operand(2, 0, 27, 7);		// f4
  Operand(3, 0, 13, 7);		// f2
  Instruction_Pack_Group(f3,
	TOP_fselect,		0x1c000000000ULL,
	TOP_UNDEFINED);

/* ===== F4:  Floating-point Compare ===== */
  f4 = ISA_Pack_Type_Create("f4");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 34, 2);		// sf
  Result(0, 6, 6);		// p1
  Result(1, 27, 6);		// p2
  Operand(2, 0, 13, 7);		// f2
  Operand(3, 0, 20, 7);		// f3
  Instruction_Pack_Group(f4,
	TOP_fcmp_eq,		0x08000000000ULL,
	TOP_fcmp_lt,		0x09000000000ULL,
	TOP_fcmp_le,		0x08200000000ULL,
	TOP_fcmp_unord,		0x09200000000ULL,
	TOP_fcmp_eq_unc,	0x08000001000ULL,
	TOP_fcmp_lt_unc,	0x09000001000ULL,
	TOP_fcmp_le_unc,	0x08200001000ULL,
	TOP_fcmp_unord_unc,	0x09200001000ULL,
	TOP_UNDEFINED);

/* ===== F5: Floating-point Class ===== */
  f5 = ISA_Pack_Type_Create("f5");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 6);		// p1
  Result(1, 27, 6);		// p2
  Operand(1, 0, 13, 7);		// f2
  Operand(2, 0, 33, 2);		// fc_2
  Operand(2, 2, 20, 7);		// fclass_7c
  Instruction_Pack_Group(f5,
	TOP_fclass_m,		0x0a000000000ULL,
	TOP_fclass_m_unc,	0x0a000001000ULL,
	TOP_UNDEFINED);

/* ===== F6: Floating-point Reciprocal Approximation ===== */
  f6= ISA_Pack_Type_Create("f6");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 34, 2);		// sf
  Result(0, 6, 7);		// f1
  Result(1, 27, 6);		// p2
  Operand(2, 0, 13, 7);		// f2
  Operand(3, 0, 20, 7);		// f3
  Instruction_Pack_Group(f6,
	TOP_frcpa,		0x00200000000ULL,
	TOP_fprcpa,		0x02200000000ULL,
	TOP_UNDEFINED);

/* ===== F7: Floating-point Reciprocal Square Root Approximation ===== */
  f7 = ISA_Pack_Type_Create("f7");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 34, 2);		// sf
  Result(0, 6, 7);		// f1
  Result(1, 27, 6);		// p2
  Operand(2, 0, 20, 7);		// f3
  Instruction_Pack_Group(f7,
	TOP_frsqrta,		0x01200000000ULL,
	TOP_fprsqrta,		0x03200000000ULL,
	TOP_UNDEFINED);

/* ===== F8: Minimum/Maximum and Parallel Compare ===== */
  f8 = ISA_Pack_Type_Create("f8");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 34, 2);		// sf
  Result(0, 6, 7);		// f1
  Operand(2, 0, 13, 7);		// f2
  Operand(3, 0, 20, 7);		// f3
  Instruction_Pack_Group(f8,
	TOP_fmin,		0x000a0000000ULL,
	TOP_fmax,		0x000a8000000ULL,
	TOP_famin,		0x000b0000000ULL,
	TOP_famax,		0x000b8000000ULL,
	TOP_fpmin,		0x020a0000000ULL,
	TOP_fpmax,		0x020a8000000ULL,
	TOP_fpamin,		0x020b0000000ULL,
	TOP_fpamax,		0x020b8000000ULL,
	TOP_fpcmp_eq,		0x02180000000ULL,
	TOP_fpcmp_lt,		0x02188000000ULL,
	TOP_fpcmp_le,		0x02190000000ULL,
	TOP_fpcmp_unord,	0x02198000000ULL,
	TOP_fpcmp_neq,		0x021a0000000ULL,
	TOP_fpcmp_nlt,		0x021a8000000ULL,
	TOP_fpcmp_nle,		0x021b0000000ULL,
	TOP_fpcmp_ord,		0x021b8000000ULL,
	TOP_UNDEFINED);

/* ===== F9: Merge and Logical ===== */
  f9 = ISA_Pack_Type_Create("f9");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// f1
  Operand(1, 0, 13, 7);		// f2
  Operand(2, 0, 20, 7);		// f3
  Instruction_Pack_Group(f9,
	TOP_fmerge_s,		0x00080000000ULL,
	TOP_fmerge_ns,		0x00088000000ULL,
	TOP_fmerge_se,		0x00090000000ULL,
	TOP_fmix_lr,		0x001c8000000ULL,
	TOP_fmix_r,		0x001d0000000ULL,
	TOP_fmix_l,		0x001d8000000ULL,
	TOP_fsxt_r,		0x001e0000000ULL,
	TOP_fsxt_l,		0x001e8000000ULL,
	TOP_fpack,		0x00140000000ULL,
	TOP_fswap,		0x001a0000000ULL,
	TOP_fswap_nl,		0x001a8000000ULL,
	TOP_fswap_nr,		0x001b0000000ULL,
	TOP_fand,		0x00160000000ULL,
	TOP_fandcm,		0x00168000000ULL,
	TOP_for,		0x00170000000ULL,
	TOP_fxor,		0x00178000000ULL,
	TOP_fpmerge_s,		0x02080000000ULL,
	TOP_fpmerge_ns,		0x02088000000ULL,
	TOP_fpmerge_se,		0x02090000000ULL,
	TOP_UNDEFINED);

/* ===== F10: Convert Floating-point to Fixed-point ===== */
  f10 = ISA_Pack_Type_Create("f10");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 34, 2);		// sf
  Result(0, 6, 7);		// f1
  Operand(2, 0, 13, 7);		// f2
  Instruction_Pack_Group(f10,
	TOP_fcvt_fx,		0x000c0000000ULL,
	TOP_fcvt_fxu,		0x000c8000000ULL,
	TOP_fcvt_fx_trunc,	0x000d0000000ULL,
	TOP_fcvt_fxu_trunc,	0x000d8000000ULL,
	TOP_fpcvt_fx,		0x020c0000000ULL,
	TOP_fpcvt_fxu,		0x020c8000000ULL,
	TOP_fpcvt_fx_trunc,	0x020d0000000ULL,
	TOP_fpcvt_fxu_trunc,	0x020d8000000ULL,
	TOP_UNDEFINED);

/* ===== F11: Convert Fixed-point to Floating-point ===== */
  f11 = ISA_Pack_Type_Create("f11");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// f1
  Operand(1, 0, 13, 7);		// f2
  Instruction_Pack_Group(f11,
	TOP_fcvt_xf,		0x000e0000000ULL,
	TOP_UNDEFINED);

/* ===== F12: Floating-point Set Controls ===== */
  f12 = ISA_Pack_Type_Create("f12");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 34, 2);		// sf
  Operand(2, 0, 13, 7);		// amask_7b
  Operand(3, 0, 20, 7);		// omask_7c
  Instruction_Pack_Group(f12,
	TOP_fsetc,		0x00020000000ULL,
	TOP_UNDEFINED);

/* ===== F13: Floating-point Clear Flags ===== */
  f13 = ISA_Pack_Type_Create("f13");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 34, 2);		// sf
  Instruction_Pack_Group(f13,
	TOP_fclrf,		0x00028000000ULL,
	TOP_UNDEFINED);

/* ===== F14: Floating-point Check Flags ===== */
  f14 = ISA_Pack_Type_Create("f14");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 34, 2);		// sf
// Adjust_Operand(2, shr4, shl4); // imm_21 = target_25 >> 4
  Operand(2, 4, 6, 20);		// imm_20a
  Operand(2, 24, 36, 1);	// s
  Instruction_Pack_Group(f14,
	TOP_fchkf,		0x00040000000ULL,
	TOP_UNDEFINED);

/* ===== F15: Break/Nop ===== */
  f15 = ISA_Pack_Type_Create("f15");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 6, 20);		// imm_20a
  Operand(1, 20, 36, 1);	// i
  Instruction_Pack_Group(f15,
	TOP_break_f,		0x00000000000ULL,
	TOP_nop_f,		0x00008000000ULL,
	TOP_UNDEFINED);

/* ===== F16: Floating-point Absolute Value (pseudo) ===== */
  f16 = ISA_Pack_Type_Create("f16");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// f1
  Operand(1, 0, 20, 7);		// f3
  Instruction_Pack_Group(f16,
	TOP_fabs,		0x00080000000ULL,
	TOP_fnegabs,		0x00088000000ULL,
	TOP_fpabs,		0x02080000000ULL,
	TOP_fpnegabs,		0x02088000000ULL,
	TOP_UNDEFINED);

/* ===== F17: Floating-point Negate (pseudo) ===== */
  f17 = ISA_Pack_Type_Create("f17");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// f1
  Operand(1, 0, 13, 7);		// f3
  Operand(1, 0, 20, 7);		// f3
  Instruction_Pack_Group(f17,
	TOP_fneg,		0x00088000000ULL,
	TOP_fpneg,		0x02088000000ULL,
	TOP_mov_f,		0x00080000000ULL,
	TOP_UNDEFINED);

/* ===== F18: Fixed-point Multiply Add (pseudo) ===== */
  f18 = ISA_Pack_Type_Create("f18");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// f1
  Operand(1, 0, 20, 7);		// f3
  Operand(2, 0, 27, 7);		// f4
  Operand(3, 0, 13, 7);		// f2
  Instruction_Pack_Group(f18,
	TOP_xma_lu,		0x1d000000000ULL,
	TOP_UNDEFINED);

/* ===== F19: Fixed-point Multiply (pseudo) ===== */
  f19 = ISA_Pack_Type_Create("f19");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// f1
  Operand(1, 0, 20, 7);		// f3
  Operand(2, 0, 27, 7);		// f4
  Instruction_Pack_Group(f19,
	TOP_xmpy_l,		0x1d000000000ULL,
	TOP_xmpy_h,		0x1dc00000000ULL,
	TOP_xmpy_hu,		0x1d800000000ULL,
	TOP_xmpy_lu,		0x1d000000000ULL,
	TOP_UNDEFINED);

/* ===== F20: Convert Unsigned Integer to Floating-point (pseudo) ===== */
  f20 = ISA_Pack_Type_Create("f20");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 34, 2);		// sf
  Result(0, 6, 7);		// f1
  Operand(2, 0, 20, 7);		// f3
  Instruction_Pack_Group(f20,
	TOP_fcvt_xuf,		0x10008000000ULL,
	TOP_fcvt_xuf_s,		0x11008000000ULL,
	TOP_fcvt_xuf_d,		0x12008000000ULL,
	TOP_fnorm,		0x10008000000ULL,
	TOP_fnorm_s,		0x11008000000ULL,
	TOP_fnorm_d,		0x12008000000ULL,
	TOP_UNDEFINED);

/* ===== F21: Floating-point Add/Subtract (pseudo) ===== */
  f21 = ISA_Pack_Type_Create("f21");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 34, 2);		// sf
  Result(0, 6, 7);		// f1
  Operand(2, 0, 20, 7);		// f3
  Operand(3, 0, 13, 7);		// f2
  Instruction_Pack_Group(f21,
	TOP_fadd,		0x10008000000ULL,
	TOP_fadd_s,		0x11008000000ULL,
	TOP_fadd_d,		0x12008000000ULL,
	TOP_fsub,		0x14008000000ULL,
	TOP_fsub_s,		0x15008000000ULL,
	TOP_fsub_d,		0x16008000000ULL,
	TOP_UNDEFINED);

/* ===== F22: Floating-point Multiply (pseudo) ===== */
  f22 = ISA_Pack_Type_Create("f22");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 34, 2);		// sf
  Result(0, 6, 7);		// f1
  Operand(2, 0, 20, 7);		// f3
  Operand(3, 0, 27, 7);		// f4
  Instruction_Pack_Group(f22,
	TOP_fmpy,		0x10000000000ULL,
	TOP_fmpy_s,		0x11000000000ULL,
	TOP_fmpy_d,		0x12000000000ULL,
	TOP_fnmpy,		0x18000000000ULL,
	TOP_fnmpy_s,		0x19000000000ULL,
	TOP_fnmpy_d,		0x1a000000000ULL,
	TOP_fpmpy,		0x13000000000ULL,
	TOP_fpnmpy,		0x1b000000000ULL,
	TOP_UNDEFINED);

/* ===== F23: Floating-point Class (pseudo) ===== */
  f23 = ISA_Pack_Type_Create("f23");
  Operand(0, 0, 0, 6);		// qp
  Result(0, 27, 6);		// p2
  Result(1, 6, 6);		// p1
  Operand(1, 0, 13, 7);		// f2
  Operand(2, 0, 33, 2);		// fc_2
  Operand(2, 2, 20, 7);		// fclass_7c
  Instruction_Pack_Group(f23,
	TOP_fclass_nm,		0x0a000000000ULL,
	TOP_fclass_nm_unc,	0x0a000001000ULL,
	TOP_UNDEFINED);

/* ===== F24: Floating-point Compare (pseudo) ===== */
// swapped args
  f24 = ISA_Pack_Type_Create("f24");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 34, 2);		// sf
  Result(0, 6, 6);		// p1
  Result(1, 27, 6);		// p2
  Operand(2, 0, 20, 7);		// f3
  Operand(3, 0, 13, 7);		// f2
  Instruction_Pack_Group(f24,
	TOP_fcmp_gt,		0x09000000000ULL,
	TOP_fcmp_ge,		0x08200000000ULL,
	TOP_fcmp_gt_unc,	0x09000001000ULL,
	TOP_fcmp_ge_unc,	0x08200001000ULL,
	TOP_UNDEFINED);

/* ===== F25:  Floating-point Compare (pseudo) ===== */
// swapped preds
  f25 = ISA_Pack_Type_Create("f25");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 34, 2);		// sf
  Result(0, 27, 6);		// p2
  Result(1, 6, 6);		// p1
  Operand(2, 0, 13, 7);		// f2
  Operand(3, 0, 20, 7);		// f3
  Instruction_Pack_Group(f25,
	TOP_fcmp_neq,		0x08000000000ULL,
	TOP_fcmp_nlt,		0x09000000000ULL,
	TOP_fcmp_nle,		0x08200000000ULL,
	TOP_fcmp_ord,		0x09200000000ULL,
	TOP_fcmp_neq_unc,	0x08000001000ULL,
	TOP_fcmp_nlt_unc,	0x09000001000ULL,
	TOP_fcmp_nle_unc,	0x08200001000ULL,
	TOP_fcmp_ord_unc,	0x09200001000ULL,
	TOP_UNDEFINED);

/* ===== F26: Floating-point Compare (pseudo) ===== */
// swapped args and preds
  f26 = ISA_Pack_Type_Create("f26");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 34, 2);		// sf
  Result(0, 27, 6);		// p2
  Result(1, 6, 6);		// p1
  Operand(2, 0, 20, 7);		// f3
  Operand(3, 0, 13, 7);		// f2
  Instruction_Pack_Group(f26,
	TOP_fcmp_ngt,		0x09000000000ULL,
	TOP_fcmp_nge,		0x08200000000ULL,
	TOP_fcmp_ngt_unc,	0x09000001000ULL,
	TOP_fcmp_nge_unc,	0x08200001000ULL,
	TOP_UNDEFINED);

/* ===== F27: Floating-point Parallel Compare (pseudo) ===== */
// swapped args
  f27 = ISA_Pack_Type_Create("f27");
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 34, 2);		// sf
  Result(0, 6, 7);		// f1
  Operand(2, 0, 20, 7);		// f3
  Operand(3, 0, 13, 7);		// f2
  Instruction_Pack_Group(f27,
	TOP_fpcmp_gt,		0x02188000000ULL,
	TOP_fpcmp_ge,		0x02190000000ULL,
	TOP_fpcmp_ngt,		0x021a8000000ULL,
	TOP_fpcmp_nge,		0x021b0000000ULL,
	TOP_UNDEFINED);

/* ===== X1: Break/Nop ===== */
  x1 = ISA_Pack_Type_Create("x1");
  Operand(1, 21, 0, 41);	// imm_41
  Next_Word();
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 6, 20);		// imm_20a
  Operand(1, 20, 36, 1);	// i
  Instruction_Pack_Group(x1,
	TOP_break_x,		0x00000000000ULL, 0x00000000000ULL,
	TOP_nop_x,		0x00000000000ULL, 0x00008000000ULL,
	TOP_UNDEFINED);

/* ===== X2: Move Long Immediate_64 ===== */
  x2 = ISA_Pack_Type_Create("x2");
  Operand(1, 22, 0, 41);	// imm_41
  Next_Word();
  Operand(0, 0, 0, 6);		// qp
  Result(0, 6, 7);		// r1
  Operand(1, 0, 13, 7);		// imm_7b
  Operand(1, 7, 27, 9);		// imm_9d
  Operand(1, 16, 22, 5);	// imm_5c
  Operand(1, 21, 21, 1);	// ic
  Operand(1, 63, 36, 1);	// i
  Instruction_Pack_Group(x2,
	TOP_movl,		0x00000000000ULL, 0x0c000000000ULL,
	TOP_UNDEFINED);

/* ===== X3: Long Branch ===== */
  x3 = ISA_Pack_Type_Create("x3");
// Adjust_Operand(4, shr4, shl4); // imm_60 = target_64 >> 4
  Operand(4, 24, 2, 39);	// imm_39
  Next_Word();
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 33, 2);		// bwh
  Operand(2, 0, 12, 1);		// ph
  Operand(3, 0, 35, 1);		// dh
  Operand(4, 4, 13, 20);	// imm_20b
  Operand(4, 63, 36, 1);	// i
  Instruction_Pack_Group(x3,
	TOP_brl_cond,		0x00000000000ULL, 0x18000000000ULL,
	TOP_UNDEFINED);

/* ===== X4: Long Call ===== */
  x4 = ISA_Pack_Type_Create("x4");
// Adjust_Operand(4, shr4, shl4); // imm_60 = target_64 >> 4
  Operand(4, 24, 2, 39);	// imm_39
  Next_Word();
  Operand(0, 0, 0, 6);		// qp
  Operand(1, 0, 33, 2);		// bwh
  Operand(2, 0, 12, 1);		// ph
  Operand(3, 0, 35, 1);		// dh
  Result(0, 6, 3);		// b1
  Operand(4, 4, 13, 20);	// imm_20b
  Operand(4, 63, 36, 1);	// i
  Instruction_Pack_Group(x4,
	TOP_brl_call,		0x00000000000ULL, 0x1a000000000ULL,
	TOP_UNDEFINED);

/* ===== X5: Long Branch (pseudo) ===== */
  x5 = ISA_Pack_Type_Create("x5");
// Adjust_Operand(2, shr4, shl4); // imm_60 = target_64 >> 4
  Operand(2, 24, 2, 39);	// imm_39
  Next_Word();
  Operand(0, 0, 12, 1);		// ph
  Operand(1, 0, 35, 1);		// dh
  Operand(2, 4, 13, 20);	// imm_20b
  Operand(2, 63, 36, 1);	// i
  Instruction_Pack_Group(x5,
	TOP_brl,		0x00000000000ULL, 0x18000000000ULL,
	TOP_UNDEFINED);

  ISA_Pack_End();
  return 0;
}
