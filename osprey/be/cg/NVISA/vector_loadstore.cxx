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
// Merge contiguous global memory accesses into vector accesses.
// We only merge accesses within a BB.
// To handle multiple interleaved accesses,
// we keep a dynamic list of vectors we are working on.

#include <list>
#include <map>
#include "defs.h"
#include "tracing.h"
#include "mempool.h"
#include "bb.h"
#include "op.h"
#include "tn.h"
#include "vector_loadstore.h"
#include "whirl2ops.h"

static BOOL tracing = FALSE;
static INT num_vectors = 0;
static std::map<TOP, TOP> V2InstructionMap;
static std::map<TOP, TOP> V4InstructionMap;

static void Create_VInstruction_Maps(void)
{
  V2InstructionMap[TOP_ld_qualifier_space_s8_r] = TOP_ld_qualifier_global_v2_s8_r;
  V2InstructionMap[TOP_ld_qualifier_space_u8_r] = TOP_ld_qualifier_global_v2_u8_r;
  V2InstructionMap[TOP_ld_qualifier_space_s8_b32_r] = TOP_ld_qualifier_global_v2_s8_b32_r;
  V2InstructionMap[TOP_ld_qualifier_space_u8_b32_r] = TOP_ld_qualifier_global_v2_u8_b32_r;
  V2InstructionMap[TOP_ld_qualifier_space_s16_r] = TOP_ld_qualifier_global_v2_s16_r;
  V2InstructionMap[TOP_ld_qualifier_space_u16_r] = TOP_ld_qualifier_global_v2_u16_r;
  V2InstructionMap[TOP_ld_qualifier_space_s16_b32_r] = TOP_ld_qualifier_global_v2_s16_b32_r;
  V2InstructionMap[TOP_ld_qualifier_space_u16_b32_r] = TOP_ld_qualifier_global_v2_u16_b32_r;
  V2InstructionMap[TOP_ld_qualifier_space_s32_r] = TOP_ld_qualifier_global_v2_s32_r;
  V2InstructionMap[TOP_ld_qualifier_space_u32_r] = TOP_ld_qualifier_global_v2_u32_r;
  V2InstructionMap[TOP_ld_qualifier_space_s64_r] = TOP_ld_qualifier_global_v2_s64_r;
  V2InstructionMap[TOP_ld_qualifier_space_u64_r] = TOP_ld_qualifier_global_v2_u64_r;
  V2InstructionMap[TOP_ld_qualifier_space_f32_r] = TOP_ld_qualifier_global_v2_f32_r;
  V2InstructionMap[TOP_ld_qualifier_space_f64_r] = TOP_ld_qualifier_global_v2_f64_r;
  V2InstructionMap[TOP_st_qualifier_space_s8_r] = TOP_st_qualifier_global_v2_s8_r;
  V2InstructionMap[TOP_st_qualifier_space_u8_r] = TOP_st_qualifier_global_v2_u8_r;
  V2InstructionMap[TOP_st_qualifier_space_s8_b32_r] = TOP_st_qualifier_global_v2_s8_b32_r;
  V2InstructionMap[TOP_st_qualifier_space_u8_b32_r] = TOP_st_qualifier_global_v2_u8_b32_r;
  V2InstructionMap[TOP_st_qualifier_space_s16_r] = TOP_st_qualifier_global_v2_s16_r;
  V2InstructionMap[TOP_st_qualifier_space_u16_r] = TOP_st_qualifier_global_v2_u16_r;
  V2InstructionMap[TOP_st_qualifier_space_s16_b32_r] = TOP_st_qualifier_global_v2_s16_b32_r;
  V2InstructionMap[TOP_st_qualifier_space_u16_b32_r] = TOP_st_qualifier_global_v2_u16_b32_r;
  V2InstructionMap[TOP_st_qualifier_space_s32_r] = TOP_st_qualifier_global_v2_s32_r;
  V2InstructionMap[TOP_st_qualifier_space_u32_r] = TOP_st_qualifier_global_v2_u32_r;
  V2InstructionMap[TOP_st_qualifier_space_s64_r] = TOP_st_qualifier_global_v2_s64_r;
  V2InstructionMap[TOP_st_qualifier_space_u64_r] = TOP_st_qualifier_global_v2_u64_r;
  V2InstructionMap[TOP_st_qualifier_space_f32_r] = TOP_st_qualifier_global_v2_f32_r;
  V2InstructionMap[TOP_st_qualifier_space_f64_r] = TOP_st_qualifier_global_v2_f64_r;
  V2InstructionMap[TOP_ld_qualifier_space_s8_a64_r] = TOP_ld_qualifier_global_v2_s8_a64_r;
  V2InstructionMap[TOP_ld_qualifier_space_u8_a64_r] = TOP_ld_qualifier_global_v2_u8_a64_r;
  V2InstructionMap[TOP_ld_qualifier_space_s8_b32_a64_r] = TOP_ld_qualifier_global_v2_s8_b32_a64_r;
  V2InstructionMap[TOP_ld_qualifier_space_u8_b32_a64_r] = TOP_ld_qualifier_global_v2_u8_b32_a64_r;
  V2InstructionMap[TOP_ld_qualifier_space_s16_a64_r] = TOP_ld_qualifier_global_v2_s16_a64_r;
  V2InstructionMap[TOP_ld_qualifier_space_u16_a64_r] = TOP_ld_qualifier_global_v2_u16_a64_r;
  V2InstructionMap[TOP_ld_qualifier_space_s16_b32_a64_r] = TOP_ld_qualifier_global_v2_s16_b32_a64_r;
  V2InstructionMap[TOP_ld_qualifier_space_u16_b32_a64_r] = TOP_ld_qualifier_global_v2_u16_b32_a64_r;
  V2InstructionMap[TOP_ld_qualifier_space_s32_a64_r] = TOP_ld_qualifier_global_v2_s32_a64_r;
  V2InstructionMap[TOP_ld_qualifier_space_u32_a64_r] = TOP_ld_qualifier_global_v2_u32_a64_r;
  V2InstructionMap[TOP_ld_qualifier_space_s64_a64_r] = TOP_ld_qualifier_global_v2_s64_a64_r;
  V2InstructionMap[TOP_ld_qualifier_space_u64_a64_r] = TOP_ld_qualifier_global_v2_u64_a64_r;
  V2InstructionMap[TOP_ld_qualifier_space_f32_a64_r] = TOP_ld_qualifier_global_v2_f32_a64_r;
  V2InstructionMap[TOP_ld_qualifier_space_f64_a64_r] = TOP_ld_qualifier_global_v2_f64_a64_r;
  V2InstructionMap[TOP_st_qualifier_space_s8_a64_r] = TOP_st_qualifier_global_v2_s8_a64_r;
  V2InstructionMap[TOP_st_qualifier_space_u8_a64_r] = TOP_st_qualifier_global_v2_u8_a64_r;
  V2InstructionMap[TOP_st_qualifier_space_s8_b32_a64_r] = TOP_st_qualifier_global_v2_s8_b32_a64_r;
  V2InstructionMap[TOP_st_qualifier_space_u8_b32_a64_r] = TOP_st_qualifier_global_v2_u8_b32_a64_r;
  V2InstructionMap[TOP_st_qualifier_space_s16_a64_r] = TOP_st_qualifier_global_v2_s16_a64_r;
  V2InstructionMap[TOP_st_qualifier_space_u16_a64_r] = TOP_st_qualifier_global_v2_u16_a64_r;
  V2InstructionMap[TOP_st_qualifier_space_s16_b32_a64_r] = TOP_st_qualifier_global_v2_s16_b32_a64_r;
  V2InstructionMap[TOP_st_qualifier_space_u16_b32_a64_r] = TOP_st_qualifier_global_v2_u16_b32_a64_r;
  V2InstructionMap[TOP_st_qualifier_space_s32_a64_r] = TOP_st_qualifier_global_v2_s32_a64_r;
  V2InstructionMap[TOP_st_qualifier_space_u32_a64_r] = TOP_st_qualifier_global_v2_u32_a64_r;
  V2InstructionMap[TOP_st_qualifier_space_s64_a64_r] = TOP_st_qualifier_global_v2_s64_a64_r;
  V2InstructionMap[TOP_st_qualifier_space_u64_a64_r] = TOP_st_qualifier_global_v2_u64_a64_r;
  V2InstructionMap[TOP_st_qualifier_space_f32_a64_r] = TOP_st_qualifier_global_v2_f32_a64_r;
  V2InstructionMap[TOP_st_qualifier_space_f64_a64_r] = TOP_st_qualifier_global_v2_f64_a64_r;
  V2InstructionMap[TOP_ld_qualifier_space_s8_o] = TOP_ld_qualifier_global_v2_s8_o;
  V2InstructionMap[TOP_ld_qualifier_space_u8_o] = TOP_ld_qualifier_global_v2_u8_o;
  V2InstructionMap[TOP_ld_qualifier_space_s8_b32_o] = TOP_ld_qualifier_global_v2_s8_b32_o;
  V2InstructionMap[TOP_ld_qualifier_space_u8_b32_o] = TOP_ld_qualifier_global_v2_u8_b32_o;
  V2InstructionMap[TOP_ld_qualifier_space_s16_o] = TOP_ld_qualifier_global_v2_s16_o;
  V2InstructionMap[TOP_ld_qualifier_space_u16_o] = TOP_ld_qualifier_global_v2_u16_o;
  V2InstructionMap[TOP_ld_qualifier_space_s16_b32_o] = TOP_ld_qualifier_global_v2_s16_b32_o;
  V2InstructionMap[TOP_ld_qualifier_space_u16_b32_o] = TOP_ld_qualifier_global_v2_u16_b32_o;
  V2InstructionMap[TOP_ld_qualifier_space_s32_o] = TOP_ld_qualifier_global_v2_s32_o;
  V2InstructionMap[TOP_ld_qualifier_space_u32_o] = TOP_ld_qualifier_global_v2_u32_o;
  V2InstructionMap[TOP_ld_qualifier_space_s64_o] = TOP_ld_qualifier_global_v2_s64_o;
  V2InstructionMap[TOP_ld_qualifier_space_u64_o] = TOP_ld_qualifier_global_v2_u64_o;
  V2InstructionMap[TOP_ld_qualifier_space_f32_o] = TOP_ld_qualifier_global_v2_f32_o;
  V2InstructionMap[TOP_ld_qualifier_space_f64_o] = TOP_ld_qualifier_global_v2_f64_o;
  V2InstructionMap[TOP_st_qualifier_space_s8_o] = TOP_st_qualifier_global_v2_s8_o;
  V2InstructionMap[TOP_st_qualifier_space_u8_o] = TOP_st_qualifier_global_v2_u8_o;
  V2InstructionMap[TOP_st_qualifier_space_s8_b32_o] = TOP_st_qualifier_global_v2_s8_b32_o;
  V2InstructionMap[TOP_st_qualifier_space_u8_b32_o] = TOP_st_qualifier_global_v2_u8_b32_o;
  V2InstructionMap[TOP_st_qualifier_space_s16_o] = TOP_st_qualifier_global_v2_s16_o;
  V2InstructionMap[TOP_st_qualifier_space_u16_o] = TOP_st_qualifier_global_v2_u16_o;
  V2InstructionMap[TOP_st_qualifier_space_s16_b32_o] = TOP_st_qualifier_global_v2_s16_b32_o;
  V2InstructionMap[TOP_st_qualifier_space_u16_b32_o] = TOP_st_qualifier_global_v2_u16_b32_o;
  V2InstructionMap[TOP_st_qualifier_space_s32_o] = TOP_st_qualifier_global_v2_s32_o;
  V2InstructionMap[TOP_st_qualifier_space_u32_o] = TOP_st_qualifier_global_v2_u32_o;
  V2InstructionMap[TOP_st_qualifier_space_s64_o] = TOP_st_qualifier_global_v2_s64_o;
  V2InstructionMap[TOP_st_qualifier_space_u64_o] = TOP_st_qualifier_global_v2_u64_o;
  V2InstructionMap[TOP_st_qualifier_space_f32_o] = TOP_st_qualifier_global_v2_f32_o;
  V2InstructionMap[TOP_st_qualifier_space_f64_o] = TOP_st_qualifier_global_v2_f64_o;
  V2InstructionMap[TOP_ld_qualifier_space_s8_a64_o] = TOP_ld_qualifier_global_v2_s8_a64_o;
  V2InstructionMap[TOP_ld_qualifier_space_u8_a64_o] = TOP_ld_qualifier_global_v2_u8_a64_o;
  V2InstructionMap[TOP_ld_qualifier_space_s8_b32_a64_o] = TOP_ld_qualifier_global_v2_s8_b32_a64_o;
  V2InstructionMap[TOP_ld_qualifier_space_u8_b32_a64_o] = TOP_ld_qualifier_global_v2_u8_b32_a64_o;
  V2InstructionMap[TOP_ld_qualifier_space_s16_a64_o] = TOP_ld_qualifier_global_v2_s16_a64_o;
  V2InstructionMap[TOP_ld_qualifier_space_u16_a64_o] = TOP_ld_qualifier_global_v2_u16_a64_o;
  V2InstructionMap[TOP_ld_qualifier_space_s16_b32_a64_o] = TOP_ld_qualifier_global_v2_s16_b32_a64_o;
  V2InstructionMap[TOP_ld_qualifier_space_u16_b32_a64_o] = TOP_ld_qualifier_global_v2_u16_b32_a64_o;
  V2InstructionMap[TOP_ld_qualifier_space_s32_a64_o] = TOP_ld_qualifier_global_v2_s32_a64_o;
  V2InstructionMap[TOP_ld_qualifier_space_u32_a64_o] = TOP_ld_qualifier_global_v2_u32_a64_o;
  V2InstructionMap[TOP_ld_qualifier_space_s64_a64_o] = TOP_ld_qualifier_global_v2_s64_a64_o;
  V2InstructionMap[TOP_ld_qualifier_space_u64_a64_o] = TOP_ld_qualifier_global_v2_u64_a64_o;
  V2InstructionMap[TOP_ld_qualifier_space_f32_a64_o] = TOP_ld_qualifier_global_v2_f32_a64_o;
  V2InstructionMap[TOP_ld_qualifier_space_f64_a64_o] = TOP_ld_qualifier_global_v2_f64_a64_o;
  V2InstructionMap[TOP_st_qualifier_space_s8_a64_o] = TOP_st_qualifier_global_v2_s8_a64_o;
  V2InstructionMap[TOP_st_qualifier_space_u8_a64_o] = TOP_st_qualifier_global_v2_u8_a64_o;
  V2InstructionMap[TOP_st_qualifier_space_s8_b32_a64_o] = TOP_st_qualifier_global_v2_s8_b32_a64_o;
  V2InstructionMap[TOP_st_qualifier_space_u8_b32_a64_o] = TOP_st_qualifier_global_v2_u8_b32_a64_o;
  V2InstructionMap[TOP_st_qualifier_space_s16_a64_o] = TOP_st_qualifier_global_v2_s16_a64_o;
  V2InstructionMap[TOP_st_qualifier_space_u16_a64_o] = TOP_st_qualifier_global_v2_u16_a64_o;
  V2InstructionMap[TOP_st_qualifier_space_s16_b32_a64_o] = TOP_st_qualifier_global_v2_s16_b32_a64_o;
  V2InstructionMap[TOP_st_qualifier_space_u16_b32_a64_o] = TOP_st_qualifier_global_v2_u16_b32_a64_o;
  V2InstructionMap[TOP_st_qualifier_space_s32_a64_o] = TOP_st_qualifier_global_v2_s32_a64_o;
  V2InstructionMap[TOP_st_qualifier_space_u32_a64_o] = TOP_st_qualifier_global_v2_u32_a64_o;
  V2InstructionMap[TOP_st_qualifier_space_s64_a64_o] = TOP_st_qualifier_global_v2_s64_a64_o;
  V2InstructionMap[TOP_st_qualifier_space_u64_a64_o] = TOP_st_qualifier_global_v2_u64_a64_o;
  V2InstructionMap[TOP_st_qualifier_space_f32_a64_o] = TOP_st_qualifier_global_v2_f32_a64_o;
  V2InstructionMap[TOP_st_qualifier_space_f64_a64_o] = TOP_st_qualifier_global_v2_f64_a64_o;

  V4InstructionMap[TOP_ld_qualifier_space_s8_r] = TOP_ld_qualifier_global_v4_s8_r;
  V4InstructionMap[TOP_ld_qualifier_space_u8_r] = TOP_ld_qualifier_global_v4_u8_r;
  V4InstructionMap[TOP_ld_qualifier_space_s8_b32_r] = TOP_ld_qualifier_global_v4_s8_b32_r;
  V4InstructionMap[TOP_ld_qualifier_space_u8_b32_r] = TOP_ld_qualifier_global_v4_u8_b32_r;
  V4InstructionMap[TOP_ld_qualifier_space_s16_r] = TOP_ld_qualifier_global_v4_s16_r;
  V4InstructionMap[TOP_ld_qualifier_space_u16_r] = TOP_ld_qualifier_global_v4_u16_r;
  V4InstructionMap[TOP_ld_qualifier_space_s16_b32_r] = TOP_ld_qualifier_global_v4_s16_b32_r;
  V4InstructionMap[TOP_ld_qualifier_space_u16_b32_r] = TOP_ld_qualifier_global_v4_u16_b32_r;
  V4InstructionMap[TOP_ld_qualifier_space_s32_r] = TOP_ld_qualifier_global_v4_s32_r;
  V4InstructionMap[TOP_ld_qualifier_space_u32_r] = TOP_ld_qualifier_global_v4_u32_r;
  V4InstructionMap[TOP_ld_qualifier_space_f32_r] = TOP_ld_qualifier_global_v4_f32_r;
  V4InstructionMap[TOP_st_qualifier_space_s8_r] = TOP_st_qualifier_global_v4_s8_r;
  V4InstructionMap[TOP_st_qualifier_space_u8_r] = TOP_st_qualifier_global_v4_u8_r;
  V4InstructionMap[TOP_st_qualifier_space_s8_b32_r] = TOP_st_qualifier_global_v4_s8_b32_r;
  V4InstructionMap[TOP_st_qualifier_space_u8_b32_r] = TOP_st_qualifier_global_v4_u8_b32_r;
  V4InstructionMap[TOP_st_qualifier_space_s16_r] = TOP_st_qualifier_global_v4_s16_r;
  V4InstructionMap[TOP_st_qualifier_space_u16_r] = TOP_st_qualifier_global_v4_u16_r;
  V4InstructionMap[TOP_st_qualifier_space_s16_b32_r] = TOP_st_qualifier_global_v4_s16_b32_r;
  V4InstructionMap[TOP_st_qualifier_space_u16_b32_r] = TOP_st_qualifier_global_v4_u16_b32_r;
  V4InstructionMap[TOP_st_qualifier_space_s32_r] = TOP_st_qualifier_global_v4_s32_r;
  V4InstructionMap[TOP_st_qualifier_space_u32_r] = TOP_st_qualifier_global_v4_u32_r;
  V4InstructionMap[TOP_st_qualifier_space_f32_r] = TOP_st_qualifier_global_v4_f32_r;
  V4InstructionMap[TOP_ld_qualifier_space_s8_a64_r] = TOP_ld_qualifier_global_v4_s8_a64_r;
  V4InstructionMap[TOP_ld_qualifier_space_u8_a64_r] = TOP_ld_qualifier_global_v4_u8_a64_r;
  V4InstructionMap[TOP_ld_qualifier_space_s8_b32_a64_r] = TOP_ld_qualifier_global_v4_s8_b32_a64_r;
  V4InstructionMap[TOP_ld_qualifier_space_u8_b32_a64_r] = TOP_ld_qualifier_global_v4_u8_b32_a64_r;
  V4InstructionMap[TOP_ld_qualifier_space_s16_a64_r] = TOP_ld_qualifier_global_v4_s16_a64_r;
  V4InstructionMap[TOP_ld_qualifier_space_u16_a64_r] = TOP_ld_qualifier_global_v4_u16_a64_r;
  V4InstructionMap[TOP_ld_qualifier_space_s16_b32_a64_r] = TOP_ld_qualifier_global_v4_s16_b32_a64_r;
  V4InstructionMap[TOP_ld_qualifier_space_u16_b32_a64_r] = TOP_ld_qualifier_global_v4_u16_b32_a64_r;
  V4InstructionMap[TOP_ld_qualifier_space_s32_a64_r] = TOP_ld_qualifier_global_v4_s32_a64_r;
  V4InstructionMap[TOP_ld_qualifier_space_u32_a64_r] = TOP_ld_qualifier_global_v4_u32_a64_r;
  V4InstructionMap[TOP_ld_qualifier_space_f32_a64_r] = TOP_ld_qualifier_global_v4_f32_a64_r;
  V4InstructionMap[TOP_st_qualifier_space_s8_a64_r] = TOP_st_qualifier_global_v4_s8_a64_r;
  V4InstructionMap[TOP_st_qualifier_space_u8_a64_r] = TOP_st_qualifier_global_v4_u8_a64_r;
  V4InstructionMap[TOP_st_qualifier_space_s8_b32_a64_r] = TOP_st_qualifier_global_v4_s8_b32_a64_r;
  V4InstructionMap[TOP_st_qualifier_space_u8_b32_a64_r] = TOP_st_qualifier_global_v4_u8_b32_a64_r;
  V4InstructionMap[TOP_st_qualifier_space_s16_a64_r] = TOP_st_qualifier_global_v4_s16_a64_r;
  V4InstructionMap[TOP_st_qualifier_space_u16_a64_r] = TOP_st_qualifier_global_v4_u16_a64_r;
  V4InstructionMap[TOP_st_qualifier_space_s16_b32_a64_r] = TOP_st_qualifier_global_v4_s16_b32_a64_r;
  V4InstructionMap[TOP_st_qualifier_space_u16_b32_a64_r] = TOP_st_qualifier_global_v4_u16_b32_a64_r;
  V4InstructionMap[TOP_st_qualifier_space_s32_a64_r] = TOP_st_qualifier_global_v4_s32_a64_r;
  V4InstructionMap[TOP_st_qualifier_space_u32_a64_r] = TOP_st_qualifier_global_v4_u32_a64_r;
  V4InstructionMap[TOP_st_qualifier_space_f32_a64_r] = TOP_st_qualifier_global_v4_f32_a64_r;
  V4InstructionMap[TOP_ld_qualifier_space_s8_o] = TOP_ld_qualifier_global_v4_s8_o;
  V4InstructionMap[TOP_ld_qualifier_space_u8_o] = TOP_ld_qualifier_global_v4_u8_o;
  V4InstructionMap[TOP_ld_qualifier_space_s8_b32_o] = TOP_ld_qualifier_global_v4_s8_b32_o;
  V4InstructionMap[TOP_ld_qualifier_space_u8_b32_o] = TOP_ld_qualifier_global_v4_u8_b32_o;
  V4InstructionMap[TOP_ld_qualifier_space_s16_o] = TOP_ld_qualifier_global_v4_s16_o;
  V4InstructionMap[TOP_ld_qualifier_space_u16_o] = TOP_ld_qualifier_global_v4_u16_o;
  V4InstructionMap[TOP_ld_qualifier_space_s16_b32_o] = TOP_ld_qualifier_global_v4_s16_b32_o;
  V4InstructionMap[TOP_ld_qualifier_space_u16_b32_o] = TOP_ld_qualifier_global_v4_u16_b32_o;
  V4InstructionMap[TOP_ld_qualifier_space_s32_o] = TOP_ld_qualifier_global_v4_s32_o;
  V4InstructionMap[TOP_ld_qualifier_space_u32_o] = TOP_ld_qualifier_global_v4_u32_o;
  V4InstructionMap[TOP_ld_qualifier_space_f32_o] = TOP_ld_qualifier_global_v4_f32_o;
  V4InstructionMap[TOP_st_qualifier_space_s8_o] = TOP_st_qualifier_global_v4_s8_o;
  V4InstructionMap[TOP_st_qualifier_space_u8_o] = TOP_st_qualifier_global_v4_u8_o;
  V4InstructionMap[TOP_st_qualifier_space_s8_b32_o] = TOP_st_qualifier_global_v4_s8_b32_o;
  V4InstructionMap[TOP_st_qualifier_space_u8_b32_o] = TOP_st_qualifier_global_v4_u8_b32_o;
  V4InstructionMap[TOP_st_qualifier_space_s16_o] = TOP_st_qualifier_global_v4_s16_o;
  V4InstructionMap[TOP_st_qualifier_space_u16_o] = TOP_st_qualifier_global_v4_u16_o;
  V4InstructionMap[TOP_st_qualifier_space_s16_b32_o] = TOP_st_qualifier_global_v4_s16_b32_o;
  V4InstructionMap[TOP_st_qualifier_space_u16_b32_o] = TOP_st_qualifier_global_v4_u16_b32_o;
  V4InstructionMap[TOP_st_qualifier_space_s32_o] = TOP_st_qualifier_global_v4_s32_o;
  V4InstructionMap[TOP_st_qualifier_space_u32_o] = TOP_st_qualifier_global_v4_u32_o;
  V4InstructionMap[TOP_st_qualifier_space_f32_o] = TOP_st_qualifier_global_v4_f32_o;
  V4InstructionMap[TOP_ld_qualifier_space_s8_a64_o] = TOP_ld_qualifier_global_v4_s8_a64_o;
  V4InstructionMap[TOP_ld_qualifier_space_u8_a64_o] = TOP_ld_qualifier_global_v4_u8_a64_o;
  V4InstructionMap[TOP_ld_qualifier_space_s8_b32_a64_o] = TOP_ld_qualifier_global_v4_s8_b32_a64_o;
  V4InstructionMap[TOP_ld_qualifier_space_u8_b32_a64_o] = TOP_ld_qualifier_global_v4_u8_b32_a64_o;
  V4InstructionMap[TOP_ld_qualifier_space_s16_a64_o] = TOP_ld_qualifier_global_v4_s16_a64_o;
  V4InstructionMap[TOP_ld_qualifier_space_u16_a64_o] = TOP_ld_qualifier_global_v4_u16_a64_o;
  V4InstructionMap[TOP_ld_qualifier_space_s16_b32_a64_o] = TOP_ld_qualifier_global_v4_s16_b32_a64_o;
  V4InstructionMap[TOP_ld_qualifier_space_u16_b32_a64_o] = TOP_ld_qualifier_global_v4_u16_b32_a64_o;
  V4InstructionMap[TOP_ld_qualifier_space_s32_a64_o] = TOP_ld_qualifier_global_v4_s32_a64_o;
  V4InstructionMap[TOP_ld_qualifier_space_u32_a64_o] = TOP_ld_qualifier_global_v4_u32_a64_o;
  V4InstructionMap[TOP_ld_qualifier_space_f32_a64_o] = TOP_ld_qualifier_global_v4_f32_a64_o;
  V4InstructionMap[TOP_st_qualifier_space_s8_a64_o] = TOP_st_qualifier_global_v4_s8_a64_o;
  V4InstructionMap[TOP_st_qualifier_space_u8_a64_o] = TOP_st_qualifier_global_v4_u8_a64_o;
  V4InstructionMap[TOP_st_qualifier_space_s8_b32_a64_o] = TOP_st_qualifier_global_v4_s8_b32_a64_o;
  V4InstructionMap[TOP_st_qualifier_space_u8_b32_a64_o] = TOP_st_qualifier_global_v4_u8_b32_a64_o;
  V4InstructionMap[TOP_st_qualifier_space_s16_a64_o] = TOP_st_qualifier_global_v4_s16_a64_o;
  V4InstructionMap[TOP_st_qualifier_space_u16_a64_o] = TOP_st_qualifier_global_v4_u16_a64_o;
  V4InstructionMap[TOP_st_qualifier_space_s16_b32_a64_o] = TOP_st_qualifier_global_v4_s16_b32_a64_o;
  V4InstructionMap[TOP_st_qualifier_space_u16_b32_a64_o] = TOP_st_qualifier_global_v4_u16_b32_a64_o;
  V4InstructionMap[TOP_st_qualifier_space_s32_a64_o] = TOP_st_qualifier_global_v4_s32_a64_o;
  V4InstructionMap[TOP_st_qualifier_space_u32_a64_o] = TOP_st_qualifier_global_v4_u32_a64_o;
  V4InstructionMap[TOP_st_qualifier_space_f32_a64_o] = TOP_st_qualifier_global_v4_f32_a64_o;
}

typedef struct {
	OP* op[4];
	INT index; // index of last vector, -1 if none
} vector_info_t;

static void
Print_Vector (vector_info_t *v)
{
	INT i;
	fprintf(TFile, "vector has %d ops:\n", v->index);
	for (i = 0; i <= v->index; ++i)
		Print_OP_No_SrcLine(v->op[i]);
}

inline void Clear_Vector (vector_info_t *v)
{
	v->index = -1;
}

inline BOOL Vector_Is_Empty (vector_info_t *v)
{
	return (v->index == -1);
}

inline void Start_Vector (vector_info_t *v, OP *op)
{
	v->op[0] = op;
	v->index = 0;
}

inline BOOL
Vector_Is_Load (vector_info_t *v)
{
	return OP_load(v->op[0]);
}

inline BOOL
Vector_Is_Store (vector_info_t *v)
{
	return OP_store(v->op[0]);
}

inline TN*
OP_Qualifier_TN (OP *op)
{
	return OP_opnd(op,0);
}
inline TN*
OP_Space_TN (OP *op)
{
	return OP_opnd(op,1);
}
inline TN*
OP_Base_TN (OP *op)
{
	return OP_opnd(op,OP_find_opnd_use(op,OU_base));
}
inline TN*
OP_Offset_TN (OP *op)
{
	return OP_opnd(op,OP_find_opnd_use(op,OU_offset));
}
inline TN*
OP_Storeval_TN (OP *op)
{
	return OP_opnd(op,OP_find_opnd_use(op,OU_storeval));
}
inline TN*
Vector_Base_TN (vector_info_t *v)
{
	return OP_Base_TN(v->op[0]);
}

inline TN*
Vector_Offset_TN (vector_info_t *v, INT index)
{
	return OP_Offset_TN(v->op[index]);
}
inline INT
Vector_Offset_Value (vector_info_t *v, INT index)
{
	return TN_value(Vector_Offset_TN(v,index));
}
inline TN*
Vector_Storeval_TN (vector_info_t *v, INT index)
{
	return OP_Storeval_TN(v->op[index]);
}

inline void Add_To_Vector (vector_info_t *v, OP *op)
{
	Is_True(v->index < 3, ("no room in vector"));
	v->index++;
	if (TN_value(OP_Offset_TN(op)) > TN_value(Vector_Offset_TN(v,v->index-1)) ) {
	    // put at end of vector
	    v->op[v->index] = op;
	} else {
	    // want vectors in ascending order, so find where to put op
	    for (INT j = 0; j < v->index; ++j) {
	      if (TN_value(OP_Offset_TN(op)) < TN_value(Vector_Offset_TN(v,j))) {
		// put at beginning of vector, so shift everything over
		for (INT i = v->index; i > j; --i)
			v->op[i] = v->op[i-1];
		v->op[j] = op;
		break; // stop after inserting else will duplicate
	      }
	    }
	}
}

static TOP Choose_Vector_Opcode (vector_info_t *v)
{
	TOP opc;
	switch (v->index) {
	case 1:
		opc = V2InstructionMap[OP_code(v->op[0])];
		return (opc != 0 ? opc : TOP_UNDEFINED);
	case 3:
		opc = V4InstructionMap[OP_code(v->op[0])];
		return (opc != 0 ? opc : TOP_UNDEFINED);
	default:
		return TOP_UNDEFINED;
	}
}

// return size in bytes
static INT
Sizeof_Load_Store (OP *op)
{
    if (TOP_is_memory_8bit(OP_code(op)))
	return 1;
    else if (TOP_is_memory_16bit(OP_code(op)))
	return 2;
    else if (TOP_is_memory_32bit(OP_code(op)))
	return 4;
    else if (TOP_is_memory_64bit(OP_code(op)))
	return 8;
    else 
	return 0;
}

// return 0 if no gaps, else return index of gap;
// search for gaps between start and end.
inline INT Find_Gap_In_Vector (vector_info_t *v, INT start, INT end)
{
	INT step_size = Sizeof_Load_Store(v->op[0]);
	INT i = start;
	while (i < end) {
		if (Vector_Offset_Value(v,i+1) 
		 != Vector_Offset_Value(v,i) + step_size)
			return i+1;
		++i;
	}
	return 0; // no gaps
}

static ST *
Get_Vector_Symbol (vector_info_t *v)
{
	TN *base_tn = Vector_Base_TN(v);
	if (TN_is_symbol(base_tn)) {
		// if directly accessing a symbol, use that info
		return TN_var(base_tn);
	}
	return NULL;
}

static TY_IDX 
Get_Vector_Type (vector_info_t *v)
{
	WN *wn = Get_WN_From_Memory_OP(v->op[0]);
	TY_IDX ty = WN_ty(wn);
	ST *st = Get_Vector_Symbol(v);
	if (st) {
		// if directly accessing a symbol, use that info
		ty = ST_type(st);
	}
	if (TY_kind(ty) == KIND_POINTER) {
		ty = TY_pointed(ty);
	}
	return ty;
}

inline BOOL
Vector_Is_Padded (vector_info_t *v)
{
        TY_IDX ty = Get_Vector_Type(v);

        // if the vector elements are STRUCT or
        // ARRAY of STRUCT, check if it has padding
        if (TY_kind(ty) == KIND_ARRAY) {
                ty = TY_etype(ty);
        }
       
        if (TY_kind(ty) == KIND_STRUCT) {
                FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty));
                int total_size = 0;

                do
                {
                        FLD_HANDLE fld(fld_iter);
                        total_size += TY_size(FLD_type(fld));
                } while (!FLD_last_field(fld_iter++));

                if (TY_size(ty) > total_size) return TRUE;
        }

        return FALSE;
}

static BOOL Vector_Is_Possible (vector_info_t *v)
{
	if (v->index < 1)
		return FALSE;	// no vector
	// Vector must be properly aligned.
	// Delay align check till now because might have gathered
	// pieces in unaligned order.
	ST *st = Get_Vector_Symbol(v);
	TY_IDX ty = Get_Vector_Type(v);
	INT step_size = Sizeof_Load_Store(v->op[0]);
	INT min_alignment;
        if (v->index == 1) {
		// must be double aligned
		min_alignment = 2 * step_size;
	} else {
		// want quad aligned
		min_alignment = 4 * step_size;
	}
	if (TY_align(ty) < min_alignment 
	  && (min_alignment % TY_align(ty)) == 0
	  && st != NULL) 
	{
		// first try to increase alignment so we can use v2 or v4;
		// can only do this if accessing a symbol
		DevWarn("increase alignment to %d", min_alignment);
		FmtAssert(TY_kind(ST_type(st)) != KIND_POINTER,
 			("reached pointer type in vectors"));
		Set_TY_align(ty, min_alignment);
		Set_ST_type(st, ty);
		if (Has_Base_Block(st)) {
			Set_STB_align(ST_base(st), 
				MAX (STB_align(ST_base(st)), min_alignment));
		}
        }

	switch (v->index) {
	case 1:
		// must be double aligned
		if ((Vector_Offset_Value(v,0) % min_alignment) != 0)
			return FALSE;
		if ((TY_align(ty) % min_alignment) != 0)
			return FALSE;
		// check that no gaps left in vector
		if (Find_Gap_In_Vector (v, 0, 1) != 0)
			return FALSE;
		break;
	case 2:
                // No real advantage to emitting v3,
                // since hw doesn't do 12-byte accesses.
                // Instead, try to do v4 if aligned load or
                // aligned store to padded struct,
                // or v2 of first or last two accesses.
                if ((TY_align(ty) % min_alignment) == 0 &&
                    (Vector_Is_Load(v) || 
                     (Vector_Is_Store(v) && Vector_Is_Padded(v)))) {
                    // try to change to v4
                    INT i = Find_Gap_In_Vector(v,0,2);
                    if (i == 0) {
                        // no gap, add to beginning or end 
                        if ((Vector_Offset_Value(v,0) % min_alignment) == 0)
                            i = 3; // add to end
                        else if (((Vector_Offset_Value(v,0)-step_size) % min_alignment) == 0)
                            i = 0; // add to beginning
                        else
                            i = -1; // cannot safely add
                    }
                    if (i != -1) {
                        DevWarn("v3 is aligned as v4, add sink at %d", i);
                        // create dummy op with unused dest, new offset
                        OP *new_op = Dup_OP(v->op[0]);
                        Copy_WN_For_Memory_OP (new_op, v->op[0]);
                        if (Vector_Is_Load(v)) {
                            Set_OP_result(new_op,0, Unused_TN(
                                        TN_register_class(OP_result(v->op[0],0))));
                        } 

                        if (i > 0) {
                            Set_OP_opnd(new_op, 
                                    OP_find_opnd_use(new_op,OU_offset),
                                    Gen_Literal_TN(
                                        Vector_Offset_Value(v,i-1) + step_size,4));
                        } else {
                            Set_OP_opnd(new_op, 
                                    OP_find_opnd_use(new_op,OU_offset),
                                    Gen_Literal_TN(
                                        Vector_Offset_Value(v,0) - step_size,4));
                        }

                        BB_Insert_Op_After (OP_bb(v->op[0]), v->op[0], new_op);
                        Add_To_Vector (v, new_op);
                        if (!Vector_Is_Possible(v)) { 
                          // if still not a vector, need to remove sink op
                          // that we temporarily added.
                          BB_Remove_Op (OP_bb(v->op[0]), new_op);
                          return FALSE;
                        } else {
                          return TRUE;
                        }
                    }
                }
		min_alignment = 2 * step_size;
		if ((TY_align(ty) % min_alignment) != 0)
			return FALSE;
		if (((Vector_Offset_Value(v,0) % min_alignment) == 0) 
		  // if 0,2,3 then want to use last 2
		  && (Vector_Offset_Value(v,1) 
		   == Vector_Offset_Value(v,0) + step_size))
		{
			v->index = 1;	// will ignore 3rd op
		}
		else if ((Vector_Offset_Value(v,1) % min_alignment) == 0) {
			// will ignore 1st op, so shift ops over
			v->op[0] = v->op[1];
			v->op[1] = v->op[2];
			v->index = 1;	
		}
		else
			return FALSE;
		// check that no gaps left in vector
		if (Find_Gap_In_Vector (v, 0, 1) != 0)
			return FALSE;
		break;
	case 3:
		// must be quad aligned
		if ((Vector_Offset_Value(v,0) % min_alignment) != 0)
			return FALSE;
		if ((TY_align(ty) % min_alignment) != 0)
			return FALSE;
		// check that no gaps left in vector
		if (Find_Gap_In_Vector (v, 0, 3) != 0)
			return FALSE;
		break;
	default: // e.g. -1
		return FALSE;
	}
	TOP opc = Choose_Vector_Opcode (v);
	if (opc == TOP_UNDEFINED) {
		DevWarn("TBD: vector for %s", TOP_Name(OP_code(v->op[0])));
		return FALSE;
	}
	return TRUE;
}

static OP*
Earliest_Vector_Op (vector_info_t *v)
{
	OP *first_op = v->op[0];
	for (INT i = 1; i <= v->index; ++i) {
		if (OP_Precedes(v->op[i], first_op))
			first_op = v->op[i];
	}
	return first_op;
}

static OP*
Latest_Vector_Op (vector_info_t *v)
{
	OP *last_op = v->op[v->index];
	for (INT i = 0; i < v->index; ++i) {
		if (OP_Precedes(last_op, v->op[i]))
			last_op = v->op[i];
	}
	return last_op;
}

static void Emit_Vector (vector_info_t *v)
{
	OP *vop;
	TOP opc = Choose_Vector_Opcode (v);
	BB *bb = OP_bb(v->op[0]);
  	if (tracing) fprintf(TFile, "emit vector: %s\n", TOP_Name(opc));
	++num_vectors;

	if (Vector_Is_Load(v)) {
	  switch (v->index) {
	  case 1:
		vop = Mk_OP (opc, 
			OP_result(v->op[0],0), OP_result(v->op[1],0),
			OP_Qualifier_TN(v->op[0]),
			Vector_Base_TN(v), Vector_Offset_TN(v,0));
		break;
	  case 2:
		vop = Mk_OP (opc, 
			OP_result(v->op[0],0), OP_result(v->op[1],0),
			OP_result(v->op[2],0),
			OP_Qualifier_TN(v->op[0]),
			Vector_Base_TN(v), Vector_Offset_TN(v,0));
		break;
	  case 3:
		vop = Mk_OP (opc, 
			OP_result(v->op[0],0), OP_result(v->op[1],0),
			OP_result(v->op[2],0), OP_result(v->op[3],0),
			OP_Qualifier_TN(v->op[0]),
			Vector_Base_TN(v), Vector_Offset_TN(v,0));
		break;
	  default:
		FmtAssert(FALSE, ("unexpected"));
	  }
	  // emit new vector op before first load op
	  BB_Insert_Op_Before (bb, Earliest_Vector_Op(v), vop);
	}

	else {	// store
	  switch (v->index) {
	  case 1:
		vop = Mk_OP (opc, OP_Qualifier_TN(v->op[0]),
			Vector_Base_TN(v), Vector_Offset_TN(v,0),
			Vector_Storeval_TN(v,0), Vector_Storeval_TN(v,1) );
		break;
	  case 2:
		vop = Mk_OP (opc, OP_Qualifier_TN(v->op[0]),
			Vector_Base_TN(v), Vector_Offset_TN(v,0),
			Vector_Storeval_TN(v,0), Vector_Storeval_TN(v,1),
			Vector_Storeval_TN(v,2) );
		break;
	  case 3:
		vop = Mk_OP (opc, OP_Qualifier_TN(v->op[0]),
			Vector_Base_TN(v), Vector_Offset_TN(v,0),
			Vector_Storeval_TN(v,0), Vector_Storeval_TN(v,1),
			Vector_Storeval_TN(v,2), Vector_Storeval_TN(v,3) );
		break;
	  default:
		FmtAssert(FALSE, ("unexpected"));
	  }
	  // emit new vector op after last store op
	  BB_Insert_Op_After (bb, Latest_Vector_Op(v), vop);
	}

	// remove each old op
	for (INT i=0; i <= v->index; ++i) {
		BB_Remove_Op (bb, v->op[i]);
	}
}

static BOOL
Vector_Can_Add_Op (vector_info_t *v, OP *op)
{
	if (Vector_Is_Empty(v)) return FALSE;
	if (v->index == 3) return FALSE; // vector is full
	// for several of these checks, can just check against
	// first op in vector, since all vector ops should be same
	if (OP_load(op) != OP_load(v->op[0]))
		return FALSE;	// not both load or both store
	if (OP_Base_TN(op) != Vector_Base_TN(v))
		return FALSE;	// base is different 
	if (! TN_has_value(OP_Offset_TN(op)))
		return FALSE;	// no immediate offset?
	if (! TN_has_value(Vector_Offset_TN(v,0)))
		return FALSE;	// no immediate offset?
	if (OP_Qualifier_TN(op) != OP_Qualifier_TN(v->op[0]))
		return FALSE;	// volatile-ness must be same

        if (TOP_is_fp_loadstore(OP_code(op)) 
		!= TOP_is_fp_loadstore(OP_code(v->op[0])) )
	{
		return FALSE;	// types don't match
	}
	// check that values are in same register type
	// (e.g. if have 16bit values in 16 and 32 bit registers).
	if (Vector_Is_Load(v)) {
	    if (TN_register_class(OP_result(op,0)) 
	     != TN_register_class(OP_result(v->op[0],0)) )
		return FALSE;
	}
	else { // store
	    if (TN_register_class(OP_Storeval_TN(op)) 
	     != TN_register_class(Vector_Storeval_TN(v,0)) )
		return FALSE;
	}

        if (Vector_Is_Load(v)) {
	  // vector load inserted at first load, 
	  // so check if result of load is used between loads
	  // E.g. load d=r1,0
	  //	op =d2
	  //	load d2=r1,4
	  // So search intervening instructions for use.
	  // Also search for intervening store to address we are loading from.
	  OP *opi; // intervening op
  	  for (INT i = 0; i < OP_results(op); i++) {
 	    if (!TN_is_register(OP_result(op,i)))
		continue;
	    for (opi = Earliest_Vector_Op(v); opi != op; opi = OP_next(opi)) {
		if (OP_Refs_TN(opi, OP_result(op,i))) {
  	    	  if (tracing) fprintf(TFile, "load used in-between\n");
	          return FALSE;	// used in-between
		}
		else if (OP_store(opi)
		  && OP_Base_TN(opi) == OP_Base_TN(op)
		  && TN_value(OP_Offset_TN(opi)) == TN_value(OP_Offset_TN(op)))
		{
  	    	  if (tracing) {
			fprintf(TFile, "store in-between\n");
	    		Print_OP_No_SrcLine(opi);
	    		Print_OP_No_SrcLine(op);
			Print_Vector(v);
		  }
	          return FALSE;	// store in-between
		}
		else if (TOP_is_v2_store(OP_code(opi))
		  && OP_Base_TN(opi) == OP_Base_TN(op)
		  && TN_value(OP_Offset_TN(op)) >= TN_value(OP_Offset_TN(opi))
		  && TN_value(OP_Offset_TN(op)) <= 
		    TN_value(OP_Offset_TN(opi)) + Sizeof_Load_Store(opi))
		{
  	    	  if (tracing) {
			fprintf(TFile, "v2 store in-between\n");
	    		Print_OP_No_SrcLine(opi);
	    		Print_OP_No_SrcLine(op);
			Print_Vector(v);
		  }
	          return FALSE;	// store in-between
		}
		else if (TOP_is_v4_store(OP_code(opi))
		  && OP_Base_TN(opi) == OP_Base_TN(op)
		  && TN_value(OP_Offset_TN(op)) >= TN_value(OP_Offset_TN(opi))
		  && TN_value(OP_Offset_TN(op)) <= 
		    TN_value(OP_Offset_TN(opi)) + 3*Sizeof_Load_Store(opi))
		{
  	    	  if (tracing) {
			fprintf(TFile, "v4 store in-between\n");
	    		Print_OP_No_SrcLine(opi);
	    		Print_OP_No_SrcLine(op);
			Print_Vector(v);
		  }
	          return FALSE;	// store in-between
		}
	    }
	  }
        }

	INT step_size = Sizeof_Load_Store(v->op[0]);
	if (Sizeof_Load_Store(op) != step_size)
		return FALSE;	// sizes don't match

	// check for case of double-aligned but 4 in a row;
	// in that case want to create 2 vectors of 2 elements each.
	ST *st = Get_Vector_Symbol(v);
	TY_IDX ty = Get_Vector_Type(v);
        INT max_vector_elements = 4;
        if (st == NULL // no info so can't change alignment
	  && (TY_align(ty) % (4 * step_size)) != 0  // not quad aligned
	  && (TY_align(ty) % (2 * step_size)) == 0) // double aligned
	{
		if (tracing) fprintf(TFile, "stop at 2 elements\n");
		max_vector_elements = 2;
	}

	// now check that offset fits in vector sequence
	INT op_offset = TN_value(OP_Offset_TN(op));
	INT op0_offset = TN_value(Vector_Offset_TN(v,0));
	INT field_id = WN_field_id(Get_WN_From_Memory_OP(op));
	INT field0_id = WN_field_id(Get_WN_From_Memory_OP(v->op[0]));
        TY_IDX struct_ty = WN_ty(Get_WN_From_Memory_OP(op));
	if (TY_kind(struct_ty) == KIND_POINTER)
    	  struct_ty = TY_pointed(struct_ty);
	INT num_elements = TY_size(struct_ty) / step_size;

        // check that offset not already in vector
        for (INT i=0; i <= v->index; ++i) {
          if (TN_value(Vector_Offset_TN(v,i)) == op_offset) {
            return FALSE;
          }
        }
	// if have field_id info, 
	// then offset should match field_id position
	// (might go in middle of sequence if saw 0,8,4 as offsets)
	if (field_id != 0 && field0_id != 0 && field_id != field0_id) {
	  if ((abs(field_id-field0_id) < num_elements)
	    && ((field_id-field0_id) == ((op_offset-op0_offset) / step_size)))
		return TRUE;
	  else
		return FALSE;
	}
	// no field_id info, so just see if offset fits at beginning or end
	// With arrays of structs could end up with offsets that are too far
	// apart, so check total distance too.
	INT distance1, distance2;
	distance1 = op0_offset - op_offset;
	distance2 = op_offset - TN_value(Vector_Offset_TN(v,v->index));
	if (distance1 == step_size 
          && abs(distance2) < step_size * max_vector_elements)
	{
		return TRUE;	// fits at beginning of vector
	}
	if (distance2 == step_size 
	  && abs(distance1) < step_size * max_vector_elements)
 	{
		return TRUE;	// fits at end of vector
	}
	
  	if (tracing) fprintf(TFile, "vector doesn't fit\n");
	return FALSE;	// doesn't fit?
}

static inline BOOL Is_Potential_Load_Or_Store (OP *op)
{
  if (OP_load(op) || OP_store(op)) {
    if (TN_enum(OP_Space_TN(op)) == ECV_space_global) {
      if (Get_WN_From_Memory_OP(op)) {	// if no wn then no alignment info
	// only handle _r and _o forms that have offsets
	if (OP_find_opnd_use(op,OU_offset) != -1)
          return TRUE;
      }
    }
  }
  return FALSE;
}

// Want to merge loads that have intervening instructions
// when regs not redefined by instructions.
static BOOL
OP_Interferes_With_Vector (OP *op, vector_info_t *v)
{
  INT i, j, k;
  if (Vector_Is_Empty(v))
	return FALSE;

  if (OP_store(op) && Vector_Is_Store(v)) {
    if (TN_enum(OP_Space_TN(op)) == ECV_space_global) {
      if (OP_Base_TN(op) == Vector_Base_TN(v)) {
        // check that offset not already in vector
        for (INT i=0; i <= v->index; ++i) {
          if (TN_value(Vector_Offset_TN(v,i)) == TN_value(OP_Offset_TN(op))) {
  	    if (tracing) {
		fprintf(TFile, "store redefines vector value\n");
	    	Print_OP_No_SrcLine(op);
	    	Print_Vector(v);
	    }
            return TRUE;
          }
        }
      }
    }
  }

  // check if base is redefined
  for (i = 0; i < OP_results(op); i++) {
      if (OP_result(op,i) == Vector_Base_TN(v)) {
  	if (tracing) fprintf(TFile, "redefines base\n");
	return TRUE;	// redefines base
      }
  }

  if (Vector_Is_Store(v)) {
	// vector store inserted at last store, 
	// so check if opnds of stores are redefined
	// E.g. store d=r1,0
	//	mov r1=
	//	store d2=r1,4
  	for (i = 0; i < OP_results(op); i++) {
 	    if (!TN_is_register(OP_result(op,i)))
		continue;
	    for (j = 0; j <= v->index; ++j) {
  		for (k = 0; k < OP_opnds(v->op[j]); k++) {
			if (OP_result(op,i) == OP_opnd(v->op[j],k)) {
				if (tracing) fprintf(TFile, "store redefined (%d,%d,%d)\n", i,j,k);
				return TRUE;	// store op redefined
			}
		}
	    }
	}
  }
  // else load:
  // vector load inserted at first load, 
  // so check if result of load is used
  // E.g. load d=r1,0
  //	op =d2
  //	load d2=r1,4
  // However, this requires knowing the future load....
  // so wait until reach that load and then check in Can_Add_Op.

  return FALSE;	// is safe to ignore
}

// go through ops and combine contiguous loads/stores into vector instructions
extern void Create_Vector_Load_Stores (void)
{
  BB *bb;
  OP *op;
  std::list<vector_info_t*> vlist;
  std::list<vector_info_t*>::iterator vlist_it;
  vector_info_t *v;
  BOOL added = FALSE;
  MEM_POOL vec_pool;
  MEM_POOL_Initialize (&vec_pool, "vector_loadstore", FALSE);
  tracing = Get_Trace(TP_EBO, 0x200);
  num_vectors = 0;
  Create_VInstruction_Maps();

  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    // could keep list of vector pointers and reuse them,
    // but simpler to just push and pop at each bb.
    MEM_POOL_Push(&vec_pool);

    FOR_ALL_BB_OPs (bb, op) {

      if (Is_Potential_Load_Or_Store (op)) {
	// add to existing vector or start new vector
        added = FALSE;
        for (vlist_it = vlist.begin(); vlist_it != vlist.end(); ++vlist_it) {
          v = *vlist_it;
	  if (Vector_Can_Add_Op (v, op)) {
	    Add_To_Vector (v, op);
            added = TRUE;
	    break;
	  }
	  // check if op interferes with vector (stores to offset in vector)
          else if (OP_Interferes_With_Vector (op, v)) {
	    // cannot continue with vector, so emit it if can
	    if (Vector_Is_Possible (v)) {
		Emit_Vector (v);
	    }
	    Clear_Vector(v);	// future iterations will ignore vector
          }
        }
        if ( ! added) {
	  v = TYPE_MEM_POOL_ALLOC(vector_info_t,&vec_pool);
	  // put at end of list so earlier vectors will finish first.
	  vlist.push_back(v); 
    	  if (tracing) { 
		fprintf(TFile, "start vector in bb %d\n", BB_id(bb));
		Print_OP_No_SrcLine(op);
	  }
	  Start_Vector (v,op);
        }
      }
      else {
        // check for interfering op
        for (vlist_it = vlist.begin(); vlist_it != vlist.end(); ++vlist_it) {
          v = *vlist_it;
          if (OP_Interferes_With_Vector (op, v)) {
	    // cannot continue with vector, so emit it if can
	    if (Vector_Is_Possible (v)) {
		Emit_Vector (v);
	    }
	    Clear_Vector(v);	// future iterations will ignore vector
          }
	  // else can ignore op
        }
      }
    }
      
    // end of BB, so emit any remaining vectors
    for (vlist_it = vlist.begin(); vlist_it != vlist.end(); ++vlist_it) {
      v = *vlist_it;
      if (Vector_Is_Possible (v)) {
	Emit_Vector (v);
      }
    }
    vlist.clear();
    MEM_POOL_Pop(&vec_pool);
  }
  if (tracing) fprintf(TFile, "%d vectors created\n", num_vectors);

  V2InstructionMap.clear();
  V4InstructionMap.clear();
  MEM_POOL_Delete (&vec_pool);
}

