/*
 *  Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of version 2 of the GNU General Public License as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it would be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 *  Further, this software is distributed without any warranty that it is
 *  free of the rightful claim of any third person regarding infringement 
 *  or the like.  Any license provided herein, whether implied or 
 *  otherwise, applies only to this software file.  Patent licenses, if 
 *  any, provided herein do not apply to combinations of this program with 
 *  other software, or any other product whatsoever.  
 *
 *  You should have received a copy of the GNU General Public License along
 *  with this program; if not, write the Free Software Foundation, Inc., 59
 *  Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */
// 
// We'd like to use 16bit operations and 16bit registers whenever possible.
// Because of C promotion rules, when a user codes with "short", it gets
// promoted to 32bit int.  WHIRL only has 32-bit operations, so we see code
// that does a 16bit load into a 32bit reg, does a 32bit add, then stores
// 16bits back to memory.  But some NVIDIA chips have 16bit registers,
// and 16bit multiply may be cheaper than 32bit multiply, so want to 
// "shorten" ops and registers back to 16bits whenever possible.
//
// Possible options:
//
// WOPT's BDCE (opt_bdce) tracks live bits to get rid of cvtls.
// By adding I2 versions of whirl operators and looking for cases
// where only 16bits are live, I was able to optimize a simple 
// short = short + short case.  However, it got stumped when
// there were pregs introduced.  Pregs are 32bits, so there would be
// copies of 16 bits to a 32bit preg, then uses of the 32bit preg,
// which would then look like it had 32bits live.  To fix this would 
// either have to start introducing 16bit pregs or somehow propagate
// the live bits through preg copies.  I have concerns about introducing
// I2 pregs and I2 ops in the middle of wopt, as that may trigger
// other problems since it might break some assumptions in wopt.
// Maybe could do version of bdce opt in whirl on cg input?
//
// There is also a wn_retype_expr file that pathscale added to 
// change 64bit ops to 32bit ops.  Fred says it is not turned on
// by default cause it didn't help most codes.  It again doesn't seem
// to handle ld/st copies; ldid are not changed at all, just the
// arithmetic ops.  It is done early in wopt, so would again potentially
// cause destabilization in wopt.
// 
// The other option is to do this optimization in CG, which avoids
// WOPT destabilization, and also limits it to the target where it matters.
// Swarup wrote remove_typeconv which attempts to do this on the 
// instructions; it works for some cases but wasn't finished.
// It also creates copies of the code in both 16 and 32bits, relying
// on OCG to clean it up, but which makes the ptx hard to read.
//
// This file is an attempt to redo remove_typeconv (doing it in parallel
// for comparison purposes while it is in progress).  In this version
// we will do it before register allocation, and will replace code with
// 16bit versions rather than create copies, then will have pass that 
// looks for mismatches and inserts copies where needed (on theory
// that usually all uses will have been replaced).
// The basic algorithm is as follows (similar to remove_typconv but 
// different naming):
// if ld.16 tn, <addr>; then tn is def_16bit (def is 16 bits)
// if st.16 <addr>, tn; then tn is use_16bit (use is 16 bits)
// if op such that if result is 16bit then source is 16bit (e.g. add),
//   and result tn is use_16bit, then opnds are use_16bit
// if op such that if operands are 16bit then result is 16bit (e.g. and),
//   and opnd tns are def_16bit, then result is def_16bit 
// iterate till no changes.
// then iterate to unmark any tns that are used in a op that will not
//  be converted to a 16bit equivalent.
// if tn is both def and use 16bit, change to 16bit tn.
// if result and opnd tns are 16bit, then make op be 16bit.
//
// Need to also catch mul24 cases; 
// not necessarily a 16bit mul, but may still fit in mul24.  
// E.g. from cgemm
//      cvt.u32.u16     $r5, %tid.x;
//      and.u32         $r6, $r5, 15;
//	mul.lo.u32      $r37, $r6, 17;
// 16bit * 8bit = 24bit
// so if const < 255 will fit in 24bit
// TODO:  use mul16.wide instead

#include <map>
#include "defs.h"
#include "tracing.h"
#include "errors.h"
#include "wn.h"
#include "bb.h"
#include "op.h"
#include "tn.h"
#include "cg.h"
#include "cgtarget.h"

static std::map<TOP, TOP> InstructionMap;

// tn map will be to a bit mask, 0 means no value
// IS_16BIT means is definitely def/use as 16bit;
// IS_NOT16BIT means has def/use that is definitely not 16bit;
// IS_MAYBE16BIT means has def/use that is currently 32bit but 
//   may be converted to 16bit once we propagate; 
// ideally everything is definite after we finish propagating, 
// but if not be conservative.
static TN_MAP tn_16bit_map;
#define DEF_IS_16BIT 	0x1
#define USE_IS_16BIT 	0x2
#define DEF_IS_NOT16BIT 0x4
#define USE_IS_NOT16BIT	0x8
#define DEF_IS_MAYBE16BIT 0x10
#define USE_IS_MAYBE16BIT 0x20
#define TN_16bit_flags(tn) ((INTPTR)TN_MAP_Get(tn_16bit_map,tn))
#define TN_def_is_16bit(tn)      (TN_16bit_flags(tn) & DEF_IS_16BIT)
#define TN_use_is_16bit(tn)      (TN_16bit_flags(tn) & USE_IS_16BIT)
#define TN_def_is_not16bit(tn)   (TN_16bit_flags(tn) & DEF_IS_NOT16BIT)
#define TN_use_is_not16bit(tn)   (TN_16bit_flags(tn) & USE_IS_NOT16BIT)
#define TN_def_is_maybe16bit(tn) (TN_16bit_flags(tn) & DEF_IS_MAYBE16BIT)
#define TN_use_is_maybe16bit(tn) (TN_16bit_flags(tn) & USE_IS_MAYBE16BIT)
#define Set_TN_def_is_16bit(tn)		TN_MAP_Set(tn_16bit_map, tn, \
	(void*) ((INTPTR)TN_MAP_Get(tn_16bit_map,tn) | DEF_IS_16BIT))
#define Set_TN_use_is_16bit(tn)		TN_MAP_Set(tn_16bit_map, tn, \
	(void*) ((INTPTR)TN_MAP_Get(tn_16bit_map,tn) | USE_IS_16BIT))
#define Set_TN_def_is_not16bit(tn)	TN_MAP_Set(tn_16bit_map, tn, \
	(void*) ((INTPTR)TN_MAP_Get(tn_16bit_map,tn) | DEF_IS_NOT16BIT))
#define Set_TN_use_is_not16bit(tn)	TN_MAP_Set(tn_16bit_map, tn, \
	(void*) ((INTPTR)TN_MAP_Get(tn_16bit_map,tn) | USE_IS_NOT16BIT))
#define Set_TN_def_is_maybe16bit(tn)	TN_MAP_Set(tn_16bit_map, tn, \
	(void*) ((INTPTR)TN_MAP_Get(tn_16bit_map,tn) | DEF_IS_MAYBE16BIT))
#define Set_TN_use_is_maybe16bit(tn)	TN_MAP_Set(tn_16bit_map, tn, \
	(void*) ((INTPTR)TN_MAP_Get(tn_16bit_map,tn) | USE_IS_MAYBE16BIT))
#define Reset_TN_def_is_16bit(tn)	TN_MAP_Set(tn_16bit_map, tn, \
	(void*) ((INTPTR)TN_MAP_Get(tn_16bit_map,tn) & ~DEF_IS_16BIT))
#define Reset_TN_use_is_16bit(tn)	TN_MAP_Set(tn_16bit_map, tn, \
	(void*) ((INTPTR)TN_MAP_Get(tn_16bit_map,tn) & ~USE_IS_16BIT))
#define Reset_TN_def_is_maybe16bit(tn)	TN_MAP_Set(tn_16bit_map, tn, \
	(void*) ((INTPTR)TN_MAP_Get(tn_16bit_map,tn) & ~DEF_IS_MAYBE16BIT))
#define Reset_TN_use_is_maybe16bit(tn)	TN_MAP_Set(tn_16bit_map, tn, \
	(void*) ((INTPTR)TN_MAP_Get(tn_16bit_map,tn) & ~USE_IS_MAYBE16BIT))

static BOOL tracing = FALSE;

static
void Create_Instruction_Map(void)
{
  // 8bit cvts will become moves of 16bit registers
  InstructionMap[TOP_cvt_s8_s32] = TOP_mov_s16;
  InstructionMap[TOP_cvt_s8_u32] = TOP_mov_s16;
  InstructionMap[TOP_cvt_u8_s32] = TOP_mov_u16;
  InstructionMap[TOP_cvt_u8_u32] = TOP_mov_u16;
  InstructionMap[TOP_cvt_s32_s8] = TOP_mov_s16;
  InstructionMap[TOP_cvt_u32_u8] = TOP_mov_u16;
  InstructionMap[TOP_cvt_s8_s32_b32] = TOP_mov_s16;
  InstructionMap[TOP_cvt_s8_u32_b32] = TOP_mov_s16;
  InstructionMap[TOP_cvt_u8_s32_b32] = TOP_mov_u16;
  InstructionMap[TOP_cvt_u8_u32_b32] = TOP_mov_u16;
  InstructionMap[TOP_cvt_s16_s32] = TOP_mov_s16;
  InstructionMap[TOP_cvt_s16_u32] = TOP_mov_s16;
  InstructionMap[TOP_cvt_u16_s32] = TOP_mov_u16;
  InstructionMap[TOP_cvt_u16_u32] = TOP_mov_u16;
  InstructionMap[TOP_cvt_s32_s16] = TOP_mov_s16;
  InstructionMap[TOP_cvt_u32_u16] = TOP_mov_u16;
  InstructionMap[TOP_cvt_s16_s32_b32] = TOP_mov_s16;
  InstructionMap[TOP_cvt_s16_u32_b32] = TOP_mov_s16;
  InstructionMap[TOP_cvt_u16_s32_b32] = TOP_mov_u16;
  InstructionMap[TOP_cvt_u16_u32_b32] = TOP_mov_u16;

  InstructionMap[TOP_mov_s32] = TOP_mov_s16;
  InstructionMap[TOP_mov_s32_lit] = TOP_mov_s16_lit;
  InstructionMap[TOP_mov_u32] = TOP_mov_u16;
  InstructionMap[TOP_mov_u32_lit] = TOP_mov_u16_lit;

  InstructionMap[TOP_min_s32] = TOP_min_s16;
  InstructionMap[TOP_max_s32] = TOP_max_s16;
  InstructionMap[TOP_min_u32] = TOP_min_u16;
  InstructionMap[TOP_max_u32] = TOP_max_u16;
  InstructionMap[TOP_and_b32] = TOP_and_b16;
  InstructionMap[TOP_and_b32_lit] = TOP_and_b16_lit;
  InstructionMap[TOP_or_b32] = TOP_or_b16;
  InstructionMap[TOP_or_b32_lit] = TOP_or_b16_lit;
  InstructionMap[TOP_xor_b32] = TOP_xor_b16;
  InstructionMap[TOP_xor_b32_lit] = TOP_xor_b16_lit;
  InstructionMap[TOP_shl_b32] = TOP_shl_b16;
  InstructionMap[TOP_shr_u32] = TOP_shr_u16;
  InstructionMap[TOP_shl_b32_lit] = TOP_shl_b16_lit;
  InstructionMap[TOP_shr_u32_lit] = TOP_shr_u16_lit;
  InstructionMap[TOP_div_u32] = TOP_div_u16;
  InstructionMap[TOP_rem_u32] = TOP_rem_u16;
  InstructionMap[TOP_div_u32_lit] = TOP_div_u16_lit;
  InstructionMap[TOP_rem_u32_lit] = TOP_rem_u16_lit;
  InstructionMap[TOP_not_b32] = TOP_not_b16;
  InstructionMap[TOP_abs_s32] = TOP_abs_s16;
  InstructionMap[TOP_abs_u32] = TOP_abs_u16;
  InstructionMap[TOP_neg_s32] = TOP_neg_s16;
  InstructionMap[TOP_neg_u32] = TOP_neg_u16;

  InstructionMap[TOP_add_s32] = TOP_add_s16;
  InstructionMap[TOP_add_s32_lit] = TOP_add_s16_lit;
  InstructionMap[TOP_add_u32] = TOP_add_u16;
  InstructionMap[TOP_add_u32_lit] = TOP_add_u16_lit;
  InstructionMap[TOP_sub_s32] = TOP_sub_s16;
  InstructionMap[TOP_sub_s32_lit] = TOP_sub_s16_lit;
  InstructionMap[TOP_sub_u32] = TOP_sub_u16;
  InstructionMap[TOP_sub_u32_lit] = TOP_sub_u16_lit;
  InstructionMap[TOP_mul_lo_s32] = TOP_mul_lo_s16;
  InstructionMap[TOP_mul_lo_s32_lit] = TOP_mul_lo_s16_lit;
  InstructionMap[TOP_mul_lo_u32] = TOP_mul_lo_u16;
  InstructionMap[TOP_mul_lo_u32_lit] = TOP_mul_lo_u16_lit;

  InstructionMap[TOP_ld_qualifier_space_u8_b32] = TOP_ld_qualifier_space_u8;
  InstructionMap[TOP_ld_qualifier_space_s8_b32] = TOP_ld_qualifier_space_s8;
  InstructionMap[TOP_ld_qualifier_space_u8_b32_o] = TOP_ld_qualifier_space_u8_o;
  InstructionMap[TOP_ld_qualifier_space_s8_b32_o] = TOP_ld_qualifier_space_s8_o;
  InstructionMap[TOP_ld_qualifier_space_u8_b32_r] = TOP_ld_qualifier_space_u8_r;
  InstructionMap[TOP_ld_qualifier_space_s8_b32_r] = TOP_ld_qualifier_space_s8_r;
  InstructionMap[TOP_st_qualifier_space_u8_b32] = TOP_st_qualifier_space_u8;
  InstructionMap[TOP_st_qualifier_space_s8_b32] = TOP_st_qualifier_space_s8;
  InstructionMap[TOP_st_qualifier_space_u8_b32_o] = TOP_st_qualifier_space_u8_o;
  InstructionMap[TOP_st_qualifier_space_s8_b32_o] = TOP_st_qualifier_space_s8_o;
  InstructionMap[TOP_st_qualifier_space_u8_b32_r] = TOP_st_qualifier_space_u8_r;
  InstructionMap[TOP_st_qualifier_space_s8_b32_r] = TOP_st_qualifier_space_s8_r;
  InstructionMap[TOP_ld_qualifier_space_u16_b32] = TOP_ld_qualifier_space_u16;
  InstructionMap[TOP_ld_qualifier_space_s16_b32] = TOP_ld_qualifier_space_s16;
  InstructionMap[TOP_ld_qualifier_space_u16_b32_o] = TOP_ld_qualifier_space_u16_o;
  InstructionMap[TOP_ld_qualifier_space_s16_b32_o] = TOP_ld_qualifier_space_s16_o;
  InstructionMap[TOP_ld_qualifier_space_u16_b32_r] = TOP_ld_qualifier_space_u16_r;
  InstructionMap[TOP_ld_qualifier_space_s16_b32_r] = TOP_ld_qualifier_space_s16_r;
  InstructionMap[TOP_st_qualifier_space_u16_b32] = TOP_st_qualifier_space_u16;
  InstructionMap[TOP_st_qualifier_space_s16_b32] = TOP_st_qualifier_space_s16;
  InstructionMap[TOP_st_qualifier_space_u16_b32_o] = TOP_st_qualifier_space_u16_o;
  InstructionMap[TOP_st_qualifier_space_s16_b32_o] = TOP_st_qualifier_space_s16_o;
  InstructionMap[TOP_st_qualifier_space_u16_b32_r] = TOP_st_qualifier_space_u16_r;
  InstructionMap[TOP_st_qualifier_space_s16_b32_r] = TOP_st_qualifier_space_s16_r;

  InstructionMap[TOP_ld_qualifier_space_u8_b32_a64] = TOP_ld_qualifier_space_u8_a64;
  InstructionMap[TOP_ld_qualifier_space_s8_b32_a64] = TOP_ld_qualifier_space_s8_a64;
  InstructionMap[TOP_ld_qualifier_space_u8_b32_a64_o] = TOP_ld_qualifier_space_u8_a64_o;
  InstructionMap[TOP_ld_qualifier_space_s8_b32_a64_o] = TOP_ld_qualifier_space_s8_a64_o;
  InstructionMap[TOP_ld_qualifier_space_u8_b32_a64_r] = TOP_ld_qualifier_space_u8_a64_r;
  InstructionMap[TOP_ld_qualifier_space_s8_b32_a64_r] = TOP_ld_qualifier_space_s8_a64_r;
  InstructionMap[TOP_st_qualifier_space_u8_b32_a64] = TOP_st_qualifier_space_u8_a64;
  InstructionMap[TOP_st_qualifier_space_s8_b32_a64] = TOP_st_qualifier_space_s8_a64;
  InstructionMap[TOP_st_qualifier_space_u8_b32_a64_o] = TOP_st_qualifier_space_u8_a64_o;
  InstructionMap[TOP_st_qualifier_space_s8_b32_a64_o] = TOP_st_qualifier_space_s8_a64_o;
  InstructionMap[TOP_st_qualifier_space_u8_b32_a64_r] = TOP_st_qualifier_space_u8_a64_r;
  InstructionMap[TOP_st_qualifier_space_s8_b32_a64_r] = TOP_st_qualifier_space_s8_a64_r;
  InstructionMap[TOP_ld_qualifier_space_u16_b32_a64] = TOP_ld_qualifier_space_u16_a64;
  InstructionMap[TOP_ld_qualifier_space_s16_b32_a64] = TOP_ld_qualifier_space_s16_a64;
  InstructionMap[TOP_ld_qualifier_space_u16_b32_a64_o] = TOP_ld_qualifier_space_u16_a64_o;
  InstructionMap[TOP_ld_qualifier_space_s16_b32_a64_o] = TOP_ld_qualifier_space_s16_a64_o;
  InstructionMap[TOP_ld_qualifier_space_u16_b32_a64_r] = TOP_ld_qualifier_space_u16_a64_r;
  InstructionMap[TOP_ld_qualifier_space_s16_b32_a64_r] = TOP_ld_qualifier_space_s16_a64_r;
  InstructionMap[TOP_st_qualifier_space_u16_b32_a64] = TOP_st_qualifier_space_u16_a64;
  InstructionMap[TOP_st_qualifier_space_s16_b32_a64] = TOP_st_qualifier_space_s16_a64;
  InstructionMap[TOP_st_qualifier_space_u16_b32_a64_o] = TOP_st_qualifier_space_u16_a64_o;
  InstructionMap[TOP_st_qualifier_space_s16_b32_a64_o] = TOP_st_qualifier_space_s16_a64_o;
  InstructionMap[TOP_st_qualifier_space_u16_b32_a64_r] = TOP_st_qualifier_space_u16_a64_r;
  InstructionMap[TOP_st_qualifier_space_s16_b32_a64_r] = TOP_st_qualifier_space_s16_a64_r;

  InstructionMap[TOP_setp_eq_s32] = TOP_setp_eq_s16;
  InstructionMap[TOP_setp_ne_s32] = TOP_setp_ne_s16;
  InstructionMap[TOP_setp_lt_s32] = TOP_setp_lt_s16;
  InstructionMap[TOP_setp_le_s32] = TOP_setp_le_s16;
  InstructionMap[TOP_setp_gt_s32] = TOP_setp_gt_s16;
  InstructionMap[TOP_setp_ge_s32] = TOP_setp_ge_s16;
  InstructionMap[TOP_setp_eq_u32] = TOP_setp_eq_u16;
  InstructionMap[TOP_setp_ne_u32] = TOP_setp_ne_u16;
  InstructionMap[TOP_setp_lt_u32] = TOP_setp_lt_u16;
  InstructionMap[TOP_setp_le_u32] = TOP_setp_le_u16;
  InstructionMap[TOP_setp_gt_u32] = TOP_setp_gt_u16;
  InstructionMap[TOP_setp_ge_u32] = TOP_setp_ge_u16;
}

static void
dump_tn_16bit_info (TN *tn)
{
  fprintf(TFile, "TN%d(%x)", TN_number(tn), (INT) TN_16bit_flags(tn));
  if (TN_def_is_16bit(tn) && TN_use_is_16bit(tn))
    fprintf(TFile, " is def and use 16bit\n");
  else if (TN_def_is_16bit(tn))
    fprintf(TFile, " is def 16bit\n");
  else if (TN_use_is_16bit(tn))
    fprintf(TFile, " is use 16bit\n");
  else
    fprintf(TFile, "\n");
}

// loads, cvts, and literal moves could be def of 16 bits
static BOOL
OP_def_is_16bit (OP *op)
{
  return (TOP_is_load_16in32(OP_code(op))
       || TOP_is_load_8in32(OP_code(op))
       || TOP_is_cvt_8to32(OP_code(op))
       || TOP_is_cvt_32to8in32(OP_code(op))
       || TOP_is_cvt_16to32(OP_code(op))
       || TOP_is_cvt_32to16in32(OP_code(op))
       || (OP_code(op) == TOP_mov_s32_lit 
           && ISA_LC_Value_In_Class(TN_value(OP_opnd(op,0)), LC_i16))
       || (OP_code(op) == TOP_mov_u32_lit 
           && ISA_LC_Value_In_Class(TN_value(OP_opnd(op,0)), LC_u16)) );
}

// stores and cvts could be use of 16 bits
static BOOL
OP_use_is_16bit (OP *op)
{
  return (TOP_is_store_16from32(OP_code(op))
       || TOP_is_store_8from32(OP_code(op))
       || TOP_is_cvt_32to8(OP_code(op))
       || TOP_is_cvt_32to8in32(OP_code(op))
       || TOP_is_cvt_32to16(OP_code(op))
       || TOP_is_cvt_32to16in32(OP_code(op)) );
}

static void
Look_For_16bit_Ops (void)
{
  BB *bb;
  OP *op;
  TN *tn;
  INT i;
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    FOR_ALL_BB_OPs (bb, op) {
      // Parameter registers cannot change, so mark as not16bit.
      // Because we rely on allocating any changed tns to 16bit regs,
      // mark as not16bit any tns that are not allocatable.
      if (OP_results(op)) {
        tn = OP_result(op,0);
        if (TN_is_register(tn) && ! TN_Is_Allocatable(tn))
          Set_TN_def_is_not16bit(tn);
      }
      for (i = 0; i < OP_opnds(op); ++i) {
        tn = OP_opnd(op,i);
        if (TN_is_register(tn) && ! TN_Is_Allocatable(tn))
          Set_TN_use_is_not16bit(tn);
      }

      if (OP_def_is_16bit(op)) {
        tn = OP_result(op,0);
        if (!TN_is_register(tn))
	  continue;
        if (TN_def_is_not16bit(tn))	// was non-16bit-load
          continue;
        if (TN_use_is_not16bit(tn))	// avoid later conflict
          continue;
        else if (TN_def_is_maybe16bit(tn)) // potential conflict
          continue;
        Set_TN_def_is_16bit(tn);
      }
      else if (OP_load(op)) {	
        // non-16bit load
        tn = OP_result(op,0);
        Set_TN_def_is_not16bit(tn);
        Reset_TN_def_is_16bit(tn);
        // if def not 16bit, remove tn from consideration even if use is 16bit
        Reset_TN_use_is_16bit(tn);
      }
      else if (OP_results(op) && TN_is_register(OP_result(op,0))) {
        // unknown size, but mark as seen 
        tn = OP_result(op,0);
        Set_TN_def_is_maybe16bit(tn);
        if (TN_def_is_16bit(tn))
          Reset_TN_def_is_16bit(tn);
      }

      if (OP_use_is_16bit(op)) {
        if (OP_store(op)) 
          tn = OP_opnd(op, OP_find_opnd_use(op,OU_storeval) );
        else // cvt
          tn = OP_opnd(op,0);
        if (!TN_is_register(tn))
	  continue;
        if (TN_use_is_not16bit(tn))	// was non-16bit store
	  continue;
        if (TN_def_is_not16bit(tn))	// avoid later conflict
	  continue;
        else if (TN_use_is_maybe16bit(tn)) // potential conflict
	  continue;
        Set_TN_use_is_16bit(tn);
      }
      else if (OP_store(op)) {
        // non-16bit store
        tn = OP_opnd(op, OP_find_opnd_use(op,OU_storeval) );
        if (!TN_is_register(tn))
	  continue;
        Set_TN_use_is_not16bit(tn);
        Reset_TN_use_is_16bit(tn);
        // if use not 16bit, remove tn from consideration even if def is 16bit
        Reset_TN_def_is_16bit(tn); 
      }
      else {
        // unknown use but mark as seen
        for (i = 0; i < OP_opnds(op); ++i) {
          tn = OP_opnd(op,i);
          if (!TN_is_register(tn))
            continue;
          Set_TN_use_is_maybe16bit(tn);
          if (TN_use_is_16bit(tn))
            Reset_TN_use_is_16bit(tn);
        }
      }
    }
  }
}

static void
Propagate_16bit_Info (void)
{
  BB *bb;
  OP *op;
  TN *tn;
  INT i;
  INT changed;
  INT count = 0;
  BOOL okay;

  do {
    changed = 0;
  
    // clear maybe field so know whether seen yet in new pass.
    for (i = First_Regular_TN; i <= Last_TN; ++i) {
      tn = TNvec(i);
      Reset_TN_def_is_maybe16bit(tn);
      Reset_TN_use_is_maybe16bit(tn);
    }
  
    for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
      // iterate backward, propagating use info
      FOR_ALL_BB_OPs_REV (bb, op) {
        if (OP_use_is_16bit(op)) {
          okay = TRUE;
        }
        // mul_lo is a special case:
        // if result is 16bit then can mark inputs as 16bit,
	// but if inputs are 16bit can change op to wide op.
        else if (TOP_is_mul_lo_32(OP_code(op)))
        {
          okay = TRUE;
          if ( TN_use_is_16bit(OP_result(op,0)))
            ; // result is 16bit
          else {
            // check for 16bit inputs
            for (i = 0; i < OP_opnds(op); ++i) {
              TN *otn = OP_opnd(op,i);
              if (TN_is_register(otn) && TN_def_is_16bit(otn))
                ;
              else if (TN_has_value(otn) 
                && ISA_LC_Value_In_Class(TN_value(otn), 
                  (OP_opnd_is_signed(op,i) ? LC_i16 : LC_u16) ) )
                ;
              else
                okay = FALSE;
            }
          }
        }
        // op is such that if result is 16bits, opnds can be 16bits
        else if (TOP_is_convertible_to16_if_out16(OP_code(op)))
        {
          okay = TRUE;
          if (OP_results(op) == 0)
            okay = FALSE;
          tn = OP_result(op,0);
          if (!TN_is_register(tn))
            okay = FALSE;
          if (!TN_use_is_16bit(tn))
            okay = FALSE;
        }
        // subtlety:  if op where if all opnds are def16 then op can be 16bit,
        // then mark operands as use16 too (else tns and op won't be modified).
        else if (TOP_is_compare_32(OP_code(op)))
        {
          okay = TRUE;
          for (i = 0; i < OP_opnds(op); ++i) {
            TN *otn = OP_opnd(op,i);
            if (TN_is_register(otn) && TN_def_is_16bit(otn))
              ;
            else if (TN_has_value(otn) 
              && ISA_LC_Value_In_Class(TN_value(otn), 
                (OP_opnd_is_signed(op,i) ? LC_i16 : LC_u16) ) )
              ;
            else
              okay = FALSE;
          }
        }
        // subtlety: if have op where if opnds are 16bits result can be 16bit,
        // and if result is already use16 and opnds are def16,
        // then op will be legal so mark operands as use16 
        // (else will never get marked and then tn won't be modified).
        else if (TOP_is_convertible_to16_if_in16(OP_code(op)))
        {
          okay = TRUE;
          if (OP_results(op)) {
            tn = OP_result(op,0);
            if (TN_is_register(tn) && !TN_use_is_16bit(tn))
  	    okay = FALSE;
          }
          for (i = 0; i < OP_opnds(op); ++i) {
            TN *otn = OP_opnd(op,i);
            if (TN_is_register(otn) && TN_def_is_16bit(otn))
              ;
            else if (TN_has_value(otn) 
              && ISA_LC_Value_In_Class(TN_value(otn), 
                (OP_opnd_is_signed(op,i) ? LC_i16 : LC_u16) ) )
              ;
            else
              okay = FALSE;
          }
        }
        else {
          okay = FALSE;
        }
        for (i = 0; i < OP_opnds(op); ++i) {
          tn = OP_opnd(op,i);
          if (!TN_is_register(tn))
            continue;
          if (okay) {
            if (TN_use_is_16bit(tn))
              continue;
            if (TN_use_is_not16bit(tn))
              continue;
            if (TN_def_is_not16bit(tn))
              continue;
            if (TN_use_is_maybe16bit(tn))
              continue;
            Set_TN_use_is_16bit(tn);
            ++changed;
          }
          else {
            Set_TN_use_is_maybe16bit(tn);
            if (TN_use_is_16bit(tn)) {
              Reset_TN_use_is_16bit(tn);
              --changed;
            }
          }
        }
      }
  
      // iterate forward, propagating def info
      FOR_ALL_BB_OPs_FWD (bb, op) {
        if (OP_results(op) == 0)
          continue;
        tn = OP_result(op,0);
        if (!TN_is_register(tn))
          continue;
        if (TN_def_is_not16bit(tn))
          continue;
        if (TN_use_is_not16bit(tn))
          continue;
  
        if (OP_def_is_16bit(op)) {
          okay = TRUE;
        }
        // look for ops such that if opnds are 16bits, result can be 16bit
        else if (TOP_is_convertible_to16_if_in16(OP_code(op)))
        {
          okay = TRUE;
          for (i = 0; i < OP_opnds(op); ++i) {
            TN *otn = OP_opnd(op,i);
            if (TN_is_register(otn) && TN_def_is_16bit(otn))
              ;
            else if (TN_has_value(otn) 
              && ISA_LC_Value_In_Class(TN_value(otn), 
                (OP_opnd_is_signed(op,i) ? LC_i16 : LC_u16) ) )
              ;
            else
              okay = FALSE;
          }
        }
        // subtlety: if have op where if result is 16bits opnds can be 16bit
        // (but not reverse, which is handled above, so this is a widening op),
        // and if result is already use16 and opnds are def16,
        // then op will be legal so mark result as def16
        // (else will never get marked and then tn won't be modified).
        else if (TOP_is_convertible_to16_if_out16(OP_code(op))
              && TN_use_is_16bit(tn) )
        {
          okay = TRUE;
          for (i = 0; i < OP_opnds(op); ++i) {
            TN *otn = OP_opnd(op,i);
            if (TN_is_register(otn) && TN_def_is_16bit(otn))
              ;
            else if (TN_has_value(otn) 
              && ISA_LC_Value_In_Class(TN_value(otn), 
                (OP_opnd_is_signed(op,i) ? LC_i16 : LC_u16) ) )
              ;
            else
              okay = FALSE;
          }
        }
        else {  // no info
          okay = FALSE;
        }
        if (okay && !TN_def_is_16bit(tn) && !TN_def_is_maybe16bit(tn)) {
          Set_TN_def_is_16bit(tn);
          ++changed;
        }
        else if (!okay) {
          Set_TN_def_is_maybe16bit(tn);
          if (TN_def_is_16bit(tn)) {
            Reset_TN_def_is_16bit(tn);
            --changed;
          }
        }
      }
    }
  
    ++count;
    FmtAssert(count < 50, ("shouldn't iterate this much"));
  } while (changed);
  if (count > 3) DevWarn("propagate iterations = %d", count);
  if (tracing) {
    for (INT i = First_Regular_TN; i <= Last_TN; ++i) {
      dump_tn_16bit_info(TNvec(i));
    }
  }
}

// is some cases may have 16bit def&use tn that cannot be used in op
// because rest of op is not def&use, so want to unmark it and then 
// propagate that info backwards.  Else would have to insert cvts
// before using, which makes code worse.
static BOOL
Unpropagate_Unusable_16bit_Info (void)
{
  BB *bb;
  OP *op;
  TN *tn;
  INT i;
  INT count = 0;
  INT changed;

  do {
    changed = 0;
    for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
      FOR_ALL_BB_OPs (bb, op) {
        BOOL has_16bit_tn = FALSE;
        BOOL has_32bit_tn = FALSE;
        for (i = 0; i < OP_results(op); ++i) {
          tn = OP_result(op,i);
          if (!TN_is_register(tn))
            continue;
          if (TN_register_class(tn) != ISA_REGISTER_CLASS_integer)
            continue;
          if (TN_use_is_16bit(tn) && TN_def_is_16bit(tn))
            has_16bit_tn = TRUE;
          else if (TOP_is_cvt_32to16in32(OP_code(op))
            || TOP_is_cvt_32to8in32(OP_code(op)))
            // result of cvt could not be fully 16bit,
            // but don't want to penalize all earlier computations,
            // so leave in place, and let later modify_ops insert
            // a convert at this point (should end up with single convert).
            ;
          else if (TOP_is_mul_lo_32(OP_code(op)))
            // result of mul could not be fully 16bit,
            // would replace with mul.wide, so leave in place
            ;
          else
            has_32bit_tn = TRUE;
        }
        for (i = 0; i < OP_opnds(op); ++i) {
          tn = OP_opnd(op,i);
          if (!TN_is_register(tn))
            continue;
          if (TN_register_class(tn) != ISA_REGISTER_CLASS_integer)
            continue;
          if (OP_opnd_use(op,i) == OU_base)
            continue; // ignore memory address
          if (TN_use_is_16bit(tn) && TN_def_is_16bit(tn))
            has_16bit_tn = TRUE;
          else
            has_32bit_tn = TRUE;
        }
        if (has_16bit_tn && has_32bit_tn) {
          if (tracing) {
            fprintf(TFile, "has 16bit tn mixed with 32bit tns:\n");
            Print_OP_No_SrcLine(op);
          }
          for (i = 0; i < OP_results(op); ++i) {
            tn = OP_result(op,i);
            if (!TN_is_register(tn))
              continue;
            if (TN_register_class(tn) != ISA_REGISTER_CLASS_integer)
              continue;
            if (TN_use_is_16bit(tn) && TN_def_is_16bit(tn)) {
              Reset_TN_use_is_16bit(tn); // unmark it
              ++changed;
            }
          }
          for (i = 0; i < OP_opnds(op); ++i) {
            tn = OP_opnd(op,i);
            if (!TN_is_register(tn))
              continue;
            if (TN_register_class(tn) != ISA_REGISTER_CLASS_integer)
              continue;
            if (TN_use_is_16bit(tn) && TN_def_is_16bit(tn)) {
              Reset_TN_use_is_16bit(tn); // unmark it
              ++changed;
            }
          }
        }
      }
    }
    ++count;
    FmtAssert(count < 50, ("shouldn't iterate this much"));
  } while (changed);
  if (count > 3) DevWarn("unpropagate iterations = %d", count);
}

// if tn is def and use 16bit, then change its register class to be 16bit
static void
Modify_16bit_TNs (void)
{
  INT i;
  TN *tn;
  for (i = First_Regular_TN; i <= Last_TN; ++i) {
    tn = TNvec(i);
    if (tracing) dump_tn_16bit_info(tn);
    if (TN_def_is_16bit(tn) && TN_use_is_16bit(tn) 
      && TN_register_class(tn) == ISA_REGISTER_CLASS_integer) 
    {
      Set_TN_register_class(tn, ISA_REGISTER_CLASS_integer16);
    }
  }
}

// if opcode, results, and operands can all be 16bit, 
// then change instruction to be 16bit.
static void
Modify_16bit_Ops (void)
{
  BB *bb;
  OP *op;
  TN *tn;
  INT i;
  BOOL newop = FALSE;
  INT count = 0;

  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    FOR_ALL_BB_OPs (bb, op) {
      TOP new_opc = InstructionMap[OP_code(op)];
      BOOL def_okay = TRUE;
      BOOL use_okay = TRUE;
      BOOL tn_def_is_16bit = FALSE;
      BOOL tn_use_is_16bit = FALSE;
      if (newop) {
        // ignore ops that we add while modifying
        newop = FALSE;
        continue;
      }
      for (i = 0; i < OP_results(op); ++i) {
        tn = OP_result(op,i);
        if (!TN_is_register(tn))
          continue;
        if (TN_register_class(tn) == ISA_REGISTER_CLASS_predicate)
          continue;
        if (TN_use_is_16bit(tn) && TN_def_is_16bit(tn))
          tn_def_is_16bit = TRUE;
        else if (TN_register_class(tn) == ISA_REGISTER_CLASS_integer16)
          continue;
        else
          def_okay = FALSE;
      }
      for (i = 0; i < OP_opnds(op); ++i) {
        if (OP_opnd_use(op,i) == OU_base)
          continue; // ignore memory address
        tn = OP_opnd(op,i);
	if (TN_is_register(tn)) {
          if (OP_opnd_use(op, i) == OU_base)
            continue; // ignore memory address
          if (TN_use_is_16bit(tn) && TN_def_is_16bit(tn))
            tn_use_is_16bit = TRUE;
          else if (TN_register_class(tn) == ISA_REGISTER_CLASS_integer16)
            continue;
          else
            use_okay = FALSE;
        }
        else if (TN_has_value(tn) 
          && ISA_LC_Value_In_Class(TN_value(tn), 
            (OP_opnd_is_signed(op,i) ? LC_i16 : LC_u16) ) )
          ; // ignore
	else if (TN_is_enum(tn))
          ; // ignore
        else {
          use_okay = FALSE;
        }
      }

      if (CG_skip_local_16bit &&
        ((count < CG_local_skip_before) ||
         (count > CG_local_skip_after)  ||
         (count == CG_local_skip_equal))) 
      {
        // if skip a change, then need to add converts
        // (because will have still created 16bit tns).
        // So do checks twice.
        if (def_okay && use_okay && new_opc)
	  ++count;
        else if (use_okay && TOP_is_mul_lo_32(OP_code(op)))
	  ++count;
        use_okay = FALSE;
        def_okay = FALSE;
      }

      if (def_okay && use_okay && new_opc) {
        if (tracing) {
          fprintf(TFile, "change op to 16bit %s\n", TOP_Name(new_opc));
          Print_OP_No_SrcLine(op);
        }
        OP_Change_Opcode(op, new_opc);
        ++count;
      }
      else if (use_okay && TOP_is_mul_lo_32(OP_code(op)))
      {
        // change to mul.wide
        switch (OP_code(op)) {
        case TOP_mul_lo_s32: new_opc = TOP_mul_wide_s16; break;
        case TOP_mul_lo_u32: new_opc = TOP_mul_wide_u16; break;
        case TOP_mul_lo_s32_lit: new_opc = TOP_mul_wide_s16_lit; break;
        case TOP_mul_lo_u32_lit: new_opc = TOP_mul_wide_u16_lit; break;
        }
        if (tracing) {
          fprintf(TFile, "change op to mul.wide %s\n", TOP_Name(new_opc));
          Print_OP_No_SrcLine(op);
        }
        OP_Change_Opcode(op, new_opc);
        ++count;
      }
      else if (tn_def_is_16bit || tn_use_is_16bit) {
        DevWarn("16bit tn in unchanged op");
        if (tracing) {
          fprintf(TFile, "modified tn in unchanged op, will add converts:\n");
          Print_OP_No_SrcLine(op);
        }
        if (tn_def_is_16bit) {
          // insert cvt after def
          for (i = 0; i < OP_results(op); ++i) {
            tn = OP_result(op,i);
            if (!TN_is_register(tn))
              continue;
            if (TN_use_is_16bit(tn) && TN_def_is_16bit(tn)) {
              OPS ops = OPS_EMPTY;
              TN *tmp = Build_RCLASS_TN (ISA_REGISTER_CLASS_integer);
              Set_OP_result(op,i,tmp);
              Build_OP (TOP_cvt_u16_u32, tn, tmp, &ops); 
	      OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);
              BB_Insert_Ops_After(bb, op, &ops);
              newop = TRUE; // so won't process this op recursively
            }
          }
        }
        if (tn_use_is_16bit) {
          // insert cvt before use
          for (i = 0; i < OP_opnds(op); ++i) {
            tn = OP_opnd(op,i);
            if (!TN_is_register(tn))
              continue;
            if (TN_use_is_16bit(tn) && TN_def_is_16bit(tn)) {
              OPS ops = OPS_EMPTY;
              TN *tmp = Build_RCLASS_TN (ISA_REGISTER_CLASS_integer);
              Set_OP_opnd(op,i,tmp);
              Build_OP (TOP_cvt_u32_u16, tmp, tn, &ops); 
	      OP_srcpos(OPS_last(&ops)) = OP_srcpos(op);
              BB_Insert_Ops_Before(bb, op, &ops);
            }
          }
        }
      }
    }
  }
  if (count > 0) DevWarn("changed %d ops to 16bit", count);
}

// Should maybe merge this into modify_16bit_ops to avoid extra pass;
// but easier to debug for now when separate.
// Usually a 16x8 multiply will become a mul.wide earlier,
// but sometimes that doesn't happen cause tn needs to be left
// as 32bit, so do mul24 with 32bit tn.
static void
Modify_24bit_Muls (void)
{
  BB *bb;
  OP *op;
  TN *tn;
  INT64 val;
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    FOR_ALL_BB_OPs (bb, op) {
      switch (OP_code(op)) {
      case TOP_mul_lo_s32_lit:
        tn = OP_opnd(op,0);
        if (!TN_def_is_16bit(tn))
          continue; 
        val = TN_value(OP_opnd(op,1));
        if (ISA_LC_Value_In_Class(val, LC_i8)) {
          if (tracing) {
            fprintf(TFile, "can use 24bit mul\n");
            Print_OP_No_SrcLine(op);
          }
	  OP_Change_Opcode(op, TOP_mul24_lo_s32_lit);
        }
        break;
      case TOP_mul_lo_u32_lit:
        tn = OP_opnd(op,0);
        if (!TN_def_is_16bit(tn))
          continue; 
        val = TN_value(OP_opnd(op,1));
        if (ISA_LC_Value_In_Class(val, LC_u8)) {
          if (tracing) {
            fprintf(TFile, "can use 24bit mul\n");
            Print_OP_No_SrcLine(op);
          }
	  OP_Change_Opcode(op, TOP_mul24_lo_u32_lit);
        }
        break;
      }
    }
  }
}

void
Use_16bit_Ops (void)
{
  tracing = Get_Trace(TP_EBO, 0x1000);
  tn_16bit_map = TN_MAP_Create();
  Create_Instruction_Map();

  Look_For_16bit_Ops ();
  Propagate_16bit_Info ();
  Unpropagate_Unusable_16bit_Info ();
  Modify_16bit_TNs ();
  Modify_16bit_Ops ();
  Modify_24bit_Muls ();

  InstructionMap.clear();
  TN_MAP_Delete(tn_16bit_map);
}

