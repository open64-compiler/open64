/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

#ifndef EXP_INTRN_INFO_H_
#define EXP_INTRN_INFO_H_

#include "register.h"
#include "topcode.h"
#ifdef TARG_SL

/* internal registers used in sl2 instructions, refer to scheduling rules for detail */ 
#define sum_acc_tn          Build_Dedicated_TN(ISA_REGISTER_CLASS_c2accum, REGISTER_c2acc, 0)
#define c2_cond_tn           Build_Dedicated_TN(ISA_REGISTER_CLASS_c2cond, REGISTER_c2cond, 0)
#define mvsel_internals_tn  Build_Dedicated_TN(ISA_REGISTER_CLASS_c2mvsel, REGISTER_c2mvsel, 0)
#define mvpat_internals_tn  Build_Dedicated_TN(ISA_REGISTER_CLASS_c2movpat, REGISTER_c2movpat, 0)
#define vlcs_internals_tn   Build_Dedicated_TN(ISA_REGISTER_CLASS_c2vlcs, REGISTER_c2vlcs, 0)

/* control register used in SL2, refer to sl2_instruction_set.doc for detail */ 
#define vadd_shft_tn  Build_Dedicated_TN(ISA_REGISTER_CLASS_cop_creg, REGISTER_MIN+0, 0)
#define dcac_shft_tn  Build_Dedicated_TN(ISA_REGISTER_CLASS_cop_creg, REGISTER_MIN+1, 0) 
#define add_ctrl_tn   Build_Dedicated_TN(ISA_REGISTER_CLASS_cop_creg, REGISTER_MIN+2, 0) 
#define mvsel_tn      Build_Dedicated_TN(ISA_REGISTER_CLASS_cop_creg, REGISTER_MIN+3, 0) 
#define sum_ctrl_tn   Build_Dedicated_TN(ISA_REGISTER_CLASS_cop_creg, REGISTER_MIN+4, 0) 
#define ls_sw_tn      Build_Dedicated_TN(ISA_REGISTER_CLASS_cop_creg, REGISTER_MIN+5, 0) 
#define ls_ctrl_tn    Build_Dedicated_TN(ISA_REGISTER_CLASS_cop_creg, REGISTER_MIN+6, 0) 
#define mult_hi_tn    Build_Dedicated_TN(ISA_REGISTER_CLASS_cop_creg, REGISTER_MIN+7, 0) 
#define ofst_zero_tn  Gen_Literal_TN(0, 4)

typedef enum {
  attr_undef = 0,
  attr_lit     = 0x01,
  attr_cop_creg = 0x2, 
  attr_cop_breg = 0x3,
  attr_cop_vreg = 0x4,
  attr_cop_sreg = 0x5,
  attr_integer_reg = 0x6,
  attr_expr = 0x7,
  attr_sym = 0x8,
/* internal registers */ 
  attr_c2sum_acc = 0x9,
  attr_c2cond = 0xa,
  attr_c2mvsel_internals= 0xb,
  attr_c2movpat_internals = 0xc,
  attr_c2vlcs_internals = 0xd,
/* control registers */ 
  attr_c2vadd_shft = 0xe, 
  attr_c2dcac_shft = 0xf, 
  attr_c2add_ctrl = 0x10, 
  attr_c2mvsel = 0x11, 
  attr_c2sum_ctrl = 0x12, 
  attr_c2ls_sw = 0x13, 
  attr_c2ls_ctrl = 0x14, 
  attr_c2mult_hi = 0x15, 
/* if the register need pair */   
  attr_pair_cop_vreg = 0x16,
/* offset zero for filling c2.ld/st field immed5 */ 
  attr_ofst_zero = 0x17 
} OPND_ATTR_TYPE ;

/* select one Build_OP to generate intrinsic instruction  and 
 * number represent which tn operands in function Build_OP */
#define OPNDS_MAX 10    // the number of operands
#define RESULTS_MAX 4    // the number of operands
#define OPND_INFO_MAX 100 // the number of operand info type

typedef UINT32 INTRN_INFO_GRP_IDX;  // index used to point to sl intrsic info entry
typedef UINT32 INTRN_INFO_OPNDS_IDX;

typedef struct sl_opnd_info {
  OPND_ATTR_TYPE type; // attribute type for current operand
  UINT32 pos;  // what position current opnd should locate in function Build_OP
} SL_OPND_INFO;

#define OPNDS_INFO_MAX 200
typedef enum {
  opnd_info_undef,
  /* immed */
  immed_pos_0,  immed_pos_1,  immed_pos_2,  immed_pos_3, 
  immed_pos_4,  immed_pos_5,  immed_pos_6,  immed_pos_7,
  immed_pos_8,
  
  /* cop_creg */
  creg_pos_0,  creg_pos_1,  creg_pos_2,  creg_pos_3,
  creg_pos_4,  creg_pos_5,  creg_pos_6, 

  /* cop_vreg */
  vreg_pos_0,  vreg_pos_1,  vreg_pos_2,  vreg_pos_3,
  vreg_pos_4, 

  /* pair_cop_vreg */ 
  pair_vreg_pos_0,  pair_vreg_pos_1, pair_vreg_pos_2, 

  /* gpr */
  gpr_pos_0,  gpr_pos_1,  gpr_pos_2,  gpr_pos_3,
  gpr_pos_4,
  
  /* expr */
  expr_pos_0,  expr_pos_1,  expr_pos_2,  expr_pos_3, 
  expr_pos_4, 
  
  /* symbol */
  sym_pos_0,  sym_pos_1, sym_pos_2, sym_pos_3, 
  sym_pos_4, 

  /* sl2 internal registers: sum_acc, c2_cond, mvsel(internal registers), vlcs, movpat */ 
  sum_acc_pos_0,

  c2cond_pos_0,

  c2mvsel_internals_pos_0,

  c2vlcs_internals_pos_0,

  c2movpat_internals_pos_0,
  
  /* sl2 control registers: vadd_shft, dcac_shft, add_ctrl,
   * mvsel(control register),  sum_ctrl, ls_sw, ls_ctrl, mult_hi
   */ 
  vadd_shft_pos_0, 
  
  dcac_shft_pos_0, 
  
  add_ctrl_pos_0,
  
  mvsel_pos_0, 
  
  sum_ctrl_pos_0,
  
  ls_sw_pos_0,
  
  ls_ctrl_pos_0, 
  
  mult_hi_pos_0,

  ofst_zero_pos_0
} RES_OPND_INFO;


SL_OPND_INFO  sl_opnd_info_array[70] = {
  /* NONE */ 
  {attr_undef, 0},
  /* immed */
  {attr_lit, 0},
  {attr_lit, 1},
  {attr_lit, 2},
  {attr_lit, 3},
  {attr_lit, 4},
  {attr_lit, 5},
  {attr_lit, 6},
  {attr_lit, 7},
  {attr_lit, 8},

  /* cop_creg */
  {attr_cop_creg, 0}, 
  {attr_cop_creg, 1}, 
  {attr_cop_creg, 2}, 
  {attr_cop_creg, 3},  
  {attr_cop_creg, 4},  
  {attr_cop_creg, 5},  
  {attr_cop_creg, 6},  
  
  /* cop_vreg */
  {attr_cop_vreg, 0},  
  {attr_cop_vreg, 1},
  {attr_cop_vreg, 2},
  {attr_cop_vreg, 3},
  {attr_cop_vreg, 4},

  /* pair_cop_vreg */ 
  {attr_pair_cop_vreg, 0},  
  {attr_pair_cop_vreg, 1},
  {attr_pair_cop_vreg, 2},

  /* gpr */ 
  {attr_integer_reg, 0},
  {attr_integer_reg, 1},
  {attr_integer_reg, 2},
  {attr_integer_reg, 3},
  {attr_integer_reg, 4},
  
  /* expr */
  {attr_expr,        0},
  {attr_expr,        1},
  {attr_expr,        2},
  {attr_expr,        3},
  {attr_expr,        4},    
  
  /* symbol */
  {attr_sym,         0},
  {attr_sym,         1},
  {attr_sym,         2},
  {attr_sym,         3},
  {attr_sym,         4},
  /* c2 acc */
  {attr_c2sum_acc, 0},

  /* c2 acc ctrl */
  {attr_c2cond, 0},

  /* c2 mvsel internal register */
  {attr_c2mvsel_internals, 0},

  /* c2 vlcs internal register */
  {attr_c2vlcs_internals, 0},
 
  /* c2 movpat internal register */
  {attr_c2movpat_internals, 0},

  {attr_c2vadd_shft, 0}, 

  {attr_c2dcac_shft, 0}, 
  
  {attr_c2add_ctrl, 0}, 
  
  {attr_c2mvsel, 0}, 
  
  {attr_c2sum_ctrl, 0}, 

  {attr_c2ls_sw, 0}, 
  
  {attr_c2ls_ctrl, 0}, 
  
  {attr_c2mult_hi, 0},

  {attr_ofst_zero, 0}
};

inline SL_OPND_INFO* 
SL_Opnd_Info(UINT32 idx) {
  return &sl_opnd_info_array[idx];
}

inline OPND_ATTR_TYPE 
SL_OPND_INFO_Type(SL_OPND_INFO* info) {
  return info->type;
}

inline UINT32
SL_OPND_INFO_Pos(SL_OPND_INFO* info) {
  return info->pos;
}

/* opnds_num_opnds_infos */
/* u: undef
 * g: gpr
 * v: vreg
 * i: immed
 * c: creg
 * e: expr  
 * m: symbol
 * p: if need pair reg 
 * */
typedef enum opnds_info_idx {
  undef,  
  i1, 	
  i2, 
  i3, 
  i4, 
  i5, 
  i6, 
  i7, 
  g0, 
  v0, 
  v1, 
  v2, 
  v3, 
  c0, 
  e0, 
  e1, 
  e2, 
  e3, 
  m1, 
  m2, 
  m3, 
  m4, 
  pv0, 
  pv1, 
  pv2, 
/* sl2 internal registers */   
  sum_acc, 
  c2cond, 
  mvsel, 
  vlcs, 
  mvpat, 
/* sl2 control registers */   
  vadd_shft, 
  dcac_shft, 
  add_ctrl, 
  mvsel_ctrl, 
  sum_ctrl, 
  ls_sw, 
  ls_ctrl, 
  mult_hi, 
  ofst_zero
} OPNDS_INFO_IDX;

RES_OPND_INFO  intrn_opnds_info_array[OPNDS_INFO_MAX+1] = {
  /* Group 0  */ 
  opnd_info_undef,
  immed_pos_1, 
  immed_pos_2, 
  immed_pos_3, 
  immed_pos_4, //i4
  immed_pos_5, 
  immed_pos_6, //i6
  immed_pos_7, //i7
  gpr_pos_0,  // g0
  vreg_pos_0,  //v0
  vreg_pos_1,  //v1
  vreg_pos_2, //v2
  vreg_pos_3, //v3
  creg_pos_0, // c0
  expr_pos_0,  //e0
  expr_pos_1,   //e1  
  expr_pos_2,   //e2  
  expr_pos_3,   //e3  
  sym_pos_1,  //m1,
  sym_pos_2,  //m2,
  sym_pos_3, // m3, 
  sym_pos_4, //m4, 
  pair_vreg_pos_0, 
  pair_vreg_pos_1, 
  pair_vreg_pos_2, 
  sum_acc_pos_0, 
  c2cond_pos_0,
  c2mvsel_internals_pos_0,
  c2vlcs_internals_pos_0,
  c2movpat_internals_pos_0,
  vadd_shft_pos_0, 
  dcac_shft_pos_0, 
  add_ctrl_pos_0,
  mvsel_pos_0, 
  sum_ctrl_pos_0,
  ls_sw_pos_0,
  ls_ctrl_pos_0, 
  mult_hi_pos_0,
  ofst_zero_pos_0 
};


typedef enum 
{
  need_pair = 0x1
}EXT_FLAGS;
/* this struct is used to descript intrinsic information for each intrisic function 
 * it is root of struct tree and use index to access corresponding entry.
 * 		opcode       : the first parameter in function Build_OP
 * 		build_op_type: which Build_OP used to build one op for the intrinsic 
 *      group_idx    : idx to sl_intrn_opnds_info array */
typedef struct sl_intrn_info{
  TOP opcode;
  OPNDS_INFO_IDX res_opnd_info[10]; 
  EXT_FLAGS flags[5];  // flags for expansion results and operands 
} SL_INTRN_EXP_INFO;

#define SL_INTRN_MAX 600

SL_INTRN_EXP_INFO sl_intrn_info_array[SL_INTRN_MAX+1] = {
  /* NONE */
  {TOP_UNDEFINED,             {undef}},
  {TOP_c2_mvgr_r2g_h_u,       {g0, v0, e1}},
  {TOP_c2_mvgr_r2g_h,         {g0, v0, e1}},
  {TOP_c2_mvgr_r2g_w,         {g0, v0, pv0, e1}},
  {TOP_c2_mvgr_r2g_h_u_i,     {g0, v0, i1}},
  {TOP_c2_mvgr_r2g_h_i,       {g0, v0, i1}},
  {TOP_c2_mvgr_r2g_w_i,       {g0, v0, pv0, i1}},
  {TOP_c2_mvgr_g2r_ba_lh,     {v0, e1}},
  {TOP_c2_mvgr_g2r_ba_hh,     {v0, e1}},
  {TOP_c2_mvgr_g2r_ba_w,      {v0, pv0, e1}},
  {TOP_c2_mvgr_g2r_lh_i,      {v0, e1, i2}},
  {TOP_c2_mvgr_g2r_hh_i,      {v0, e1, i2}},
  {TOP_c2_mvgr_g2r_w_i,       {v0, pv0, e1, i2}},
  {TOP_c2_mvgr_g2r_lh,        {v0, e1, e2}},
  {TOP_c2_mvgr_g2r_hh,        {v0, e1, e2}},
  {TOP_c2_mvgr_g2r_w,         {v0, pv0, e1, e2}},
  {TOP_c2_mvgr_g2r_bh,        {v0, e1}},
  {TOP_c2_mvgr_g2r_bh_u,      {v0, e1}},
  {TOP_c2_mvgr_g2r_bv,        {v0, e1}},
  {TOP_c2_mvgr_g2r_bv_u,      {v0, e1}},
  {TOP_c2_mvgr_g2r_b4_i,      {v0, e1,i2}},
  {TOP_c2_mvgr_g2r_b4,        {v0, e1, e2}}, 
  {TOP_c2_mvgc_c2g,           {g0, c0}},
  {TOP_c2_mvgc_g2c,           {c0, e1}},
  {TOP_c2_ld_v_b_u,           {v0, e1, ls_ctrl}},
  {TOP_c2_ld_v_b,             {v0, e1, ls_ctrl}},
  {TOP_c2_ld_v_h,             {v0, e1, ls_ctrl}},
  {TOP_c2_ld_v_w,             {v0, pv0, e1, ls_ctrl}},
  {TOP_c2_ld_v_sw,            {v0, mvpat, e1, ls_sw}},
  {TOP_c2_ld_v_m_b_u,         {v0, e1, ls_ctrl}},
  {TOP_c2_ld_v_m_b,           {v0, e1, ls_ctrl}},
  {TOP_c2_ld_v_m_h,           {v0, e1, ls_ctrl}},
  {TOP_c2_ld_v_m_w,           {v0, pv0, e1, ls_ctrl}},
  {TOP_c2_ld_s_h_u_p,         {g0, e0}},
  {TOP_c2_ld_s_h_u,           {g0, e0}},
  {TOP_c2_ld_s_h_p,           {g0, e0}},
  {TOP_c2_ld_s_h,             {g0, e0}},
  {TOP_c2_ld_s_w_p,           {g0, e0}},
  {TOP_c2_ld_s_w,             {g0, e0}},
  {TOP_c2_ld_v2g_b_u,         {g0, e0, ofst_zero}},
  {TOP_c2_ld_v2g_b,           {g0, e0, ofst_zero}},
  {TOP_c2_ld_v2g_h_u,         {g0, e0, ofst_zero}},
  {TOP_c2_ld_v2g_h,           {g0, e0, ofst_zero}},
  {TOP_c2_ld_v2g_w,           {g0, e0, ofst_zero}},
  {TOP_c2_st_v_b,             {v0, e1, ls_ctrl}},
  {TOP_c2_st_v_h,             {v0, e1, ls_ctrl}},
  {TOP_c2_st_v_w,             {v0, pv0, e1, ls_ctrl}},
  {TOP_c2_st_v_m_b,           {v0, e1,  ls_ctrl}},
  {TOP_c2_st_v_m_h,           {v0, e1, ls_ctrl}},
  {TOP_c2_st_v_m_w,           {v0, pv0, e1, ls_ctrl}},
  {TOP_c2_st_s_h,             {e0, e1}},
  {TOP_c2_st_s_h_p,           {e0, e1}},
  {TOP_c2_st_s_w,             {e0, e1}},
  {TOP_c2_st_s_w_p,           {e0, e1}},
  {TOP_c2_st_g2v_b,           {e0, e1, ofst_zero}},
  {TOP_c2_st_g2v_h,           {e0, e1, ofst_zero}},
  {TOP_c2_st_g2v_w,           {e0, e1, ofst_zero}},
  {TOP_c2_ldi_s_h_u,          {g0, m2}},
  {TOP_c2_ldi_s_h,            {g0, m2}},
  {TOP_c2_ldi_s_w,            {g0, m2}},
  {TOP_c2_ldi_c,              {c0, m1}},
  {TOP_c2_ldi_v_b_u,          {v0, m4, ls_ctrl}},
  {TOP_c2_ldi_v_b,            {v0, m4, ls_ctrl}},
  {TOP_c2_ldi_v_h,            {v0, m4, ls_ctrl}},
  {TOP_c2_ldi_v_w,            {v0, pv0, m4, ls_ctrl}},
  {TOP_c2_ldi_v_m_b_u,        {v0, m4, ls_ctrl}},
  {TOP_c2_ldi_v_m_b,          {v0, m4, ls_ctrl}},
  {TOP_c2_ldi_v_m_h,          {v0, m4, ls_ctrl}},
  {TOP_c2_ldi_v_m_w,          {v0, pv0, m4, ls_ctrl}},
  {TOP_c2_ldi_v2g_b_u,        {g0, m2}}, 
  {TOP_c2_ldi_v2g_b,          {g0, m2}},
  {TOP_c2_ldi_v2g_h_u,        {g0, m2}},
  {TOP_c2_ldi_v2g_h,          {g0, m2}},
  {TOP_c2_ldi_v2g_w,          {g0, m2}},
  {TOP_c2_sti_v_b,            {v0, m3, ls_ctrl}},
  {TOP_c2_sti_v_h,            {v0, m3, ls_ctrl}},
  {TOP_c2_sti_v_w,            {v0, pv0, m3, ls_ctrl}},
  {TOP_c2_sti_v_m_b,          {v0, m3, ls_ctrl}},
  {TOP_c2_sti_v_m_h,          {v0, m3, ls_ctrl}},
  {TOP_c2_sti_v_m_w,          {v0, pv0, m3, ls_ctrl}},
  {TOP_c2_sti_c,              {c0, m1}},
  {TOP_c2_sti_s_h,            {e0, m2}},
  {TOP_c2_sti_s_w,            {e0, m2}},
  {TOP_c2_sti_g2v_b,          {e0, m2}},
  {TOP_c2_sti_g2v_h,          {e0, m2}},
  {TOP_c2_sti_g2v_w,          {e0, m2}},
  {TOP_c2_vadds_h,            {v0, v1, v2, i5, i6}},
  {TOP_c2_vadds_w,            {v0, pv0, v1, pv1, v2, pv2, i5, i6}},
  {TOP_c2_vadds_p,            {v0, pv0, v1, pv1, v2, pv2, i5, i6}},
  {TOP_c2_vadds_h_mode6,      {v0, v1, v2, i5, i6, dcac_shft}},
  {TOP_c2_vadds_h_mode2,      {v0, v1, v2, i5, i6, vadd_shft}},
  {TOP_c2_vadds_w_mode6,      {v0, pv0, v1, pv1, v2, pv2, i5, i6, dcac_shft}},
  {TOP_c2_vadds_w_mode2,      {v0, pv0, v1, pv1, v2, pv2, i5, i6, vadd_shft}},
  {TOP_c2_vadds_p_mode6,      {v0, pv0, v1, pv1, v2, pv2, i5, i6, dcac_shft}},
  {TOP_c2_vadds_p_mode2,      {v0, pv0, v1, pv1, v2, pv2, i5, i6, vadd_shft}},
  {TOP_c2_vsubs_h,            {v0, v1, v2, i6, i7}},
  {TOP_c2_vsubs_h_sm,         {v0, v1, v2, i6, i7, vadd_shft}},
  {TOP_c2_vsubs_h_abs,        {v0, v1, v2, i6, i7}},
  {TOP_c2_vsubs_h_abs_sm,     {v0, v1, v2, i6, i7, vadd_shft}},
  {TOP_c2_vabs_h,             {v0, v2}},
  {TOP_c2_vabs_h_sm,          {v0, v2, vadd_shft}},
  {TOP_c2_vsubs_w,            {v0, pv0, v1, pv1, v2, pv2, i6, i7}},
  {TOP_c2_vsubs_w_sm,         {v0, pv0, v1, pv1, v2, pv2, i6, i7, vadd_shft}},
  {TOP_c2_vsubs_w_abs,        {v0, pv0, v1, pv1, v2, pv2, i6, i7}},
  {TOP_c2_vsubs_w_abs_sm,     {v0, pv0, v1, pv1, v2, pv2, i6, i7, vadd_shft}},
  {TOP_c2_vabs_w,             {v0, pv0, v2, pv2}},
  {TOP_c2_vabs_w_sm,          {v0, pv0, v2, pv2, vadd_shft}},
  {TOP_c2_vsubs_p,            {v0, pv0, v1, pv1, v2, pv2, i6, i7}},
  {TOP_c2_vsubs_p_sm,         {v0, pv0, v1, pv1, v2, pv2, i6, i7, vadd_shft}},
  {TOP_c2_vsubs_p_abs,        {v0, pv0, v1, pv1, v2, pv2, i6, i7}},
  {TOP_c2_vsubs_p_abs_sm,     {v0, pv0, v1, pv1, v2, pv2, i6, i7, vadd_shft}},
  {TOP_c2_vabs_p,             {v0, pv0, v2, pv2}},
  {TOP_c2_vabs_p_sm,          {v0, pv0, v2, pv2, vadd_shft}},
  {TOP_c2_vmul_h,             {v0, v1, v2}},
  {TOP_c2_vmul_w,             {v0, pv0, v1, v2}},
  {TOP_c2_vneg_h,             {v0, v1, v2}},
  {TOP_c2_vneg_w,             {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vneg_p,             {v0, pv0, v1, pv1,  v2, pv2}},
  {TOP_c2_vshr_p,             {v0, pv0, v1, pv1, i5}},
  {TOP_c2_vshr_h,             {v0, v1, i5}},
  {TOP_c2_vshr_w,             {v0, pv0, v1, pv1, i5}},
  {TOP_c2_vshl_p,             {v0, pv0, v1, pv1, i5}},
  {TOP_c2_vshl_h,             {v0, v1, i5}},
  {TOP_c2_vshl_w,             {v0, pv0, v1, pv1, i5}},
  {TOP_c2_vclp,               {v0, v1}},
  {TOP_c2_vclp_p,             {v0, pv0, v1, pv1}},
  {TOP_c2_vclp_a,             {v0, v1, v2}},
  {TOP_c2_vclp_s,             {v0, v1, v2}},
  {TOP_c2_vclp_2,             {v0, v1, v2, v0}},
  {TOP_c2_vclp_n,             {v0, v1, v2}},
  {TOP_c2_vclg_h_lt_and,      {v0, v1, v2}},
  {TOP_c2_vclg_h_lt_or,       {v0, v1, v2}},
  {TOP_c2_vclg_h_le_and,      {v0, v1, v2}},
  {TOP_c2_vclg_h_le_or,       {v0, v1, v2}},
  {TOP_c2_vclg_h_eq_and,      {v0, v1, v2}},
  {TOP_c2_vclg_h_eq_or,       {v0, v1, v2}},
  {TOP_c2_vclg_h_ge_and,      {v0, v1, v2}},
  {TOP_c2_vclg_h_ge_or,       {v0, v1, v2}},
  {TOP_c2_vclg_h_gt_and,      {v0, v1, v2}},
  {TOP_c2_vclg_h_gt_or,       {v0, v1, v2}},
  {TOP_c2_vclg_h_and,         {v0, v1, v2}},
  {TOP_c2_vclg_h_or,          {v0, v1, v2}},
  {TOP_c2_vclg_h_le,          {v0, v1, v2}},
  {TOP_c2_vclg_h_lt,          {v0, v1, v2}},
  {TOP_c2_vclg_h_ge,          {v0, v1, v2}},
  {TOP_c2_vclg_h_gt,          {v0, v1, v2}},
  {TOP_c2_vclg_w_lt_and,      {v0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_w_lt_or,       {v0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_w_le_and,      {v0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_w_le_or,       {v0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_w_eq_and,      {v0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_w_eq_or,       {v0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_w_ge_and,      {v0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_w_ge_or,       {v0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_w_gt_and,      {v0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_w_gt_or,       {v0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_w_and,         {v0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_w_or,          {v0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_w_le,          {v0, v1, pv1, v2}},
  {TOP_c2_vclg_w_lt,          {v0, v1, pv1, v2}},
  {TOP_c2_vclg_w_ge,          {v0, v1, pv1, v2}},
  {TOP_c2_vclg_w_gt,          {v0, v1, pv1, v2}},
  {TOP_c2_vclg_p_lt_and,      {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_p_lt_or,       {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_p_le_and,      {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_p_le_or,       {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_p_eq_and,      {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_p_eq_or,       {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_p_ge_and,      {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_p_ge_or,       {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_p_gt_and,      {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_p_gt_or,       {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_p_and,         {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_p_or,          {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_p_le,          {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_p_eq,          {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_p_ge,          {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vclg_p_gt,          {v0, pv0, v1, pv1, v2, pv2}},
  {TOP_c2_vcmov_h_f,          {v0, v1, v2}},
  {TOP_c2_vcmov_h_t,          {v0, v1, v2}},
  {TOP_c2_vcmov_w_f,          {v0, pv0, v1, v2, pv2}},
  {TOP_c2_vcmov_w_t,          {v0, pv0, v1, v2, pv2}},
  {TOP_c2_lczero_z,           {v0, v1}},
  {TOP_c2_lczero_nz_fw,       {v0, v1}},
  {TOP_c2_lczero_nz_bw,       {v0, v1}},
  {TOP_c2_vrnd_h,             {v0, v1, i3}},
  {TOP_c2_vrnd_w,             {v0, v1, pv1, i3}},
  {TOP_c2_vspas,              {v0, pv0, v1, pv1, i2, i3}},
  {TOP_c2_vspel_mul_h,        {v0, v1, i5}},
  {TOP_c2_vspel_mul_w,        {v0, pv0, v1, pv1, i5}},
  {TOP_c2_vspel_adds,         {v0, v1, i4, i5}},
  {TOP_c2_vspel_mac_h,        {v0, v1, i2, i4}},
  {TOP_c2_vspel_mac_w,        {v0, pv0, v1, pv1, i2, i4}},
  {TOP_c2_mmul_h,             {v0, v1, v2, i4, i5}},
  {TOP_c2_mmul_w,             {v0, pv0, v1, pv1, v2, pv2, i4, i5}},
  {TOP_c2_vmov,               {v0, v1, i4}},
  {TOP_c2_vmov_swin,          {v0, v1, i2, i4, mvpat}},
  {TOP_c2_vcopy,              {v0, v1}},
  {TOP_c2_vcmpr_h_eq,         {v0, v1, v2}},
  {TOP_c2_vcmpr_h_lt,         {v0, v1, v2}},
  {TOP_c2_vcmpr_h_le,         {v0, v1, v2}},
  {TOP_c2_vcmpr_h_gt,         {v0, v1, v2}},
  {TOP_c2_vcmpr_h_ge,         {v0, v1, v2}},
  {TOP_c2_vcmpr_w_eq,         {v0, v1, pv1, v2, pv2}},
  {TOP_c2_vcmpr_w_lt,         {v0, v1, pv1, v2, pv2}},
  {TOP_c2_vcmpr_w_le,         {v0, v1, pv1, v2, pv2}},
  {TOP_c2_vcmpr_w_gt,         {v0, v1, pv1, v2, pv2}},
  {TOP_c2_vcmpr_w_ge,         {v0, v1, pv1, v2, pv2}},
  {TOP_c2_sad,                {g0, v0, v1, v2}}, 
  {TOP_c2_satd,               {g0, v0, undef, v1, v2, i3}}, 
  {TOP_c2_intra,              {v0, v1, i2}},
  {TOP_c2_intra_0_1_9_14_16,  {v0, v1, i2}},
  {TOP_c2_intra_2_3_8_10,     {v0, v1, i2}},
  {TOP_c2_intra_4,            {v0, v1, i2}},
  {TOP_c2_intra_5_11,         {v0, v1, i2}},
  {TOP_c2_intra_6,            {v0, v1, i2}},             
  {TOP_c2_intra_7,            {v0, v1, i2}},        
  {TOP_c2_intra_12_13,        {v0, v1, i2}}, 
  {TOP_c2_intra_15_17,        {v0, v1, i2}},              
  {TOP_c2_mvsel_mode0,        {g0, mvsel, e0, e1, i2, mvsel_ctrl}},
  {TOP_c2_mvsel_mode1,        {g0, mvsel, e0, e1, i2, mvsel}},
  {TOP_c2_mvsel_mode2,        {g0, mvsel, e0, e1, i2, mvsel, mvsel_ctrl}},
  {TOP_c2_mvsel_mode345,      {g0, e0, e1, i2, mvsel}},
  {TOP_c2_bcst_q,             {g0, e0, e1}},
  {TOP_c2_bcst_i,             {g0, e0, e1}},
  {TOP_c2_vlcs_dc,            {v0, pv0, vlcs, v1, vlcs}},
  {TOP_c2_vlcs_ac,            {v0, pv0, vlcs, v1, vlcs}},
  {TOP_c2_vlcs_wb,            {g0, vlcs}},
  {TOP_c2_add_shl_g_i,        {g0, e0, i1, i3, i4, i5}},
  {TOP_c2_add_shr_g_i,        {g0, e0, i1, i3, i4, i5}},
  {TOP_c2_add_shl_g,          {g0, e0, e1, i3, i4, i5}},
  {TOP_c2_add_shr_g,          {g0, e0, e1, i3, i4, i5}},
  {TOP_c2_add_shl_r_h_i,      {v0, v1, i2, i5, i6, i7, add_ctrl}},
  {TOP_c2_add_shr_r_h_i,      {v0, v1, i2, i5, i6, i7, add_ctrl}},
  {TOP_c2_add_shl_r_w_i,      {v0, pv0, v1, pv1, i2, i5, i6, i7, add_ctrl}},
  {TOP_c2_add_shr_r_w_i,      {v0, pv0, v1, pv1, i2, i5, i6, i7, add_ctrl}},
  {TOP_c2_add_shl_r_h,        {v0, v1, v2, i5, i6, i7, add_ctrl}},
  {TOP_c2_add_shr_r_h,        {v0, v1, v2, i5, i6, i7, add_ctrl}},
  {TOP_c2_add_shl_r_w,        {v0, pv0, v1, pv1, v2, pv2, i5, i6, i7, add_ctrl}},
  {TOP_c2_add_shr_r_w,        {v0, pv0, v1, pv1, v2, pv2, i5, i6, i7, add_ctrl}},
  {TOP_c2_sub_g_abs_i,        {g0, e0, i1}},
  {TOP_c2_subs_g_i,           {g0, e0, i1, i4}},
  {TOP_c2_sub_g_abs,          {g0, e0, e1}},
  {TOP_c2_subs_g,             {g0, e0, e1, i4}},
  {TOP_c2_subs_r_h_i,         {v0, v1, i2, i6, add_ctrl}},
  {TOP_c2_subs_r_w_i,         {v0, pv0, v1, pv1, i2, i6, add_ctrl}},
  {TOP_c2_sub_r_abs_h_i,      {v0, v1, i2, add_ctrl}},
  {TOP_c2_sub_r_abs_w_i,      {v0, pv0, v1, pv1, i2, add_ctrl}},
  {TOP_c2_subs_r_h,           {v0, v1, v2, i6, add_ctrl}},
  {TOP_c2_subs_r_w,           {v0, pv0, v1, pv1, v2, pv2, i6, add_ctrl}},
  {TOP_c2_sub_r_abs_h,        {v0, v1, v2, add_ctrl}},
  {TOP_c2_sub_r_abs_w,        {v0, pv0, v1, pv1, v2, pv2, add_ctrl}},
  {TOP_c2_muls,               {g0, mult_hi, e0, e1, i2, e3, i4}},
  {TOP_c2_mads,               {g0, e0, e1, e2, i3, i4}},
  {TOP_c2_smads,              {g0, e0, e1, e2}},
  {TOP_c2_min,                {g0, e0, e1}},
  {TOP_c2_max,                {g0, e0, e1}},
  {TOP_c2_cmov,               {g0, e0, e1, e2}},
  {TOP_c2_mov_g,              {g0, sum_acc, e0}},
  {TOP_c2_mov_r,              {v0, sum_acc, v1, sum_ctrl}},
  {TOP_c2_mov_c_i,            {sum_acc, v1, i2}},
  {TOP_c2_mov_c,              {sum_acc, v1, e2, sum_ctrl}},
  {TOP_c2_mov_s_i,            {v0, sum_acc, v1, i2, i3}},
  {TOP_c2_mov_s,              {v0, sum_acc, v1, e2, e3}},             
  {TOP_c2_clp,                {g0, e0, e1, e2}},
  {TOP_c2_clp_i,              {g0, e0, i1, e2}},
  {TOP_c2_chkrng,             {g0, e0, i1, i2}},
  {TOP_c2_scond_r_h_wb_eq,    {g0, c2cond, v0, v1, add_ctrl}},
  {TOP_c2_scond_r_h_wb_lt,    {g0, c2cond, v0, v1, add_ctrl}},
  {TOP_c2_scond_r_h_wb_le,    {g0, c2cond, v0, v1, add_ctrl}},
  {TOP_c2_scond_r_h_wb_gt,    {g0, c2cond, v0, v1, add_ctrl}},
  {TOP_c2_scond_r_h_wb_ge,    {g0, c2cond, v0, v1, add_ctrl}},
  {TOP_c2_scond_r_wb_eq_i,    {g0, c2cond, v0, i1, add_ctrl}},
  {TOP_c2_scond_r_wb_lt_i,    {g0, c2cond, v0, i1, add_ctrl}},
  {TOP_c2_scond_r_wb_le_i,    {g0, c2cond, v0, i1, add_ctrl}},
  {TOP_c2_scond_r_wb_gt_i,    {g0, c2cond, v0, i1, add_ctrl}},
  {TOP_c2_scond_r_wb_ge_i,    {g0, c2cond, v0, i1, add_ctrl}},
  {TOP_c2_scond_r_w_wb_eq,    {g0, c2cond, v0, pv0, v1, pv1, add_ctrl}},
  {TOP_c2_scond_r_w_wb_lt,    {g0, c2cond, v0, pv0, v1, pv1, add_ctrl}},
  {TOP_c2_scond_r_w_wb_le,    {g0, c2cond, v0, pv0, v1, pv1, add_ctrl}},
  {TOP_c2_scond_r_w_wb_gt,    {g0, c2cond, v0, pv0, v1, pv1, add_ctrl}},
  {TOP_c2_scond_r_w_wb_ge,    {g0, c2cond, v0, pv0, v1, pv1, add_ctrl}},
  {TOP_c2_scond_r_w_wb_eq_i,  {g0, c2cond, v0, pv0, i1, add_ctrl}},
  {TOP_c2_scond_r_w_wb_lt_i,  {g0, c2cond, v0, pv0, i1, add_ctrl}},
  {TOP_c2_scond_r_w_wb_le_i,  {g0, c2cond, v0, pv0, i1, add_ctrl}},
  {TOP_c2_scond_r_w_wb_gt_i,  {g0, c2cond, v0, pv0, i1, add_ctrl}},
  {TOP_c2_scond_r_w_wb_ge_i,  {g0, c2cond, v0, pv0, i1, add_ctrl}},
  {TOP_c2_scond_r_h_eq,       {c2cond, v0, v1, add_ctrl}},
  {TOP_c2_scond_r_h_lt,       {c2cond, v0, v1, add_ctrl}},
  {TOP_c2_scond_r_h_le,       {c2cond, v0, v1, add_ctrl}},
  {TOP_c2_scond_r_h_gt,       {c2cond, v0, v1, add_ctrl}},
  {TOP_c2_scond_r_h_ge,       {c2cond, v0, v1, add_ctrl}},
  {TOP_c2_scond_r_h_eq_i,     {c2cond, v0, i1, add_ctrl}},
  {TOP_c2_scond_r_h_lt_i,     {c2cond, v0, i1, add_ctrl}},
  {TOP_c2_scond_r_h_le_i,     {c2cond, v0, i1, add_ctrl}},
  {TOP_c2_scond_r_h_gt_i,     {c2cond, v0, i1, add_ctrl}},
  {TOP_c2_scond_r_h_ge_i,     {c2cond, v0, i1, add_ctrl}},
  {TOP_c2_scond_r_w_eq,       {c2cond, v0, pv0, v1, pv1, add_ctrl}},
  {TOP_c2_scond_r_w_lt,       {c2cond, v0, pv0, v1, pv1, add_ctrl}},
  {TOP_c2_scond_r_w_le,       {c2cond, v0, pv0, v1, pv1, add_ctrl}},
  {TOP_c2_scond_r_w_gt,       {c2cond, v0, pv0, v1, pv1, add_ctrl}},
  {TOP_c2_scond_r_w_ge,       {c2cond, v0, pv0, v1, pv1, add_ctrl}},
  {TOP_c2_scond_r_w_eq_i,     {c2cond, v0, pv0, i1, add_ctrl}},
  {TOP_c2_scond_r_w_lt_i,     {c2cond, v0, pv0, i1, add_ctrl}},
  {TOP_c2_scond_r_w_le_i,     {c2cond, v0, pv0, i1, add_ctrl}},
  {TOP_c2_scond_r_w_gt_i,     {c2cond, v0, pv0, i1, add_ctrl}},
  {TOP_c2_scond_r_w_ge_i,     {c2cond, v0, pv0, i1, add_ctrl}},
  {TOP_c2_scond_eq,           {g0, c2cond, e0, e1}},
  {TOP_c2_scond_lt,           {g0, c2cond, e0, e1}},
  {TOP_c2_scond_le,           {g0, c2cond, e0, e1}},
  {TOP_c2_scond_gt,           {g0, c2cond, e0, e1}},
  {TOP_c2_scond_ge,           {g0, c2cond, e0, e1}},
  {TOP_c2_scond_eq_i,         {g0, c2cond, e0, i1}},
  {TOP_c2_scond_lt_i,         {g0, c2cond, e0, i1}},
  {TOP_c2_scond_le_i,         {g0, c2cond, e0, i1}},
  {TOP_c2_scond_gt_i,         {g0, c2cond, e0, i1}},
  {TOP_c2_scond_ge_i,         {g0, c2cond, e0, i1}},
  {TOP_c2_bop_ls,             {g0, e0, e1}},
  {TOP_c2_bop_rs,             {g0, e0, e1}},
  {TOP_c2_bop_and,            {g0, e0, e1}},
  {TOP_c2_bop_or,             {g0, e0, e1}},
  {TOP_c2_bop_xor,            {g0, e0, e1}},
  {TOP_c2_bop_ls_i,           {g0, e0, i1}},
  {TOP_c2_bop_rs_i,           {g0, e0, i1}},
  {TOP_c2_bop_and_i,          {g0, e0, i1}},
  {TOP_c2_bop_or_i,           {g0, e0, i1}},
  {TOP_c2_bop_xor_i,          {g0, e0, i1}},
  {TOP_c2_bop_andxor,         {g0, e0, e1}},
  {TOP_c2_bdep_l,             {e0, e1, e2, undef}},
  {TOP_c2_bdep_m,             {e0, e1, e2, undef}},
  {TOP_c2_bxtr_u_l,           {g0, e0, e1, e2}},
  {TOP_c2_bxtr_s_l,           {g0, e0, e1, e2}},
  {TOP_c2_bxtr_u_m,           {g0, e0, e1, e2}},
  {TOP_c2_bxtr_s_m,           {g0, e0, e1, e2}},
  {TOP_c2_bxtrr48,            {g0, v0, e1, e2}},
  {TOP_c2_bxtrr48_i,          {g0, v0, i1, e2}},
  {TOP_c2_sum4_c,             {sum_acc, v1, i2}},
  {TOP_c2_sum4_g,             {g0, sum_acc, v0, i1}},
  {TOP_c2_sum4_sw,            {g0, sum_acc, v0, i1}},
  {TOP_c2_sum4_r,             {v0, sum_acc, v1, v2, v3, sum_acc, sum_ctrl}},
  {TOP_c2_sum4_saddr,         {g0, sum_acc, e0, e1, e2}},
  {TOP_c2_med,                {g0, e0, e1, e2}},
  {TOP_c2_gsums,              {g0, e0, e1, i2, i3}},
  {TOP_c2_wrap,               {g0, e0, e1}},
  {TOP_c2_clzob_zd,           {g0, e0, e1}},
  {TOP_c2_clzob_od,           {g0, e0, e1}},
  {TOP_c2_clzob_zd_i,         {g0, e0, i1}},
  {TOP_c2_clzob_od_i,         {g0, e0, i1}},
  {TOP_c2_thctrl_lock,        {undef}}, 
  {TOP_c2_thctrl_unlock,      {undef}}, 
  {TOP_c2_thctrl_deact,       {undef}}, 
  {TOP_c2_thctrl_act,         {i1}}, 
  {TOP_c2_thctrl_mode4,       {i1}}, 
  {TOP_c2_thctrl_mode5,       {i1}}, 
  {TOP_c2_thctrl_mode6,       {i1}}, 
  {TOP_c2_thctrl_mode7,       {i1}}, 
  {TOP_c2_joint,              {undef}},  
  {TOP_peripheral_rw_begin,   {undef}}, 
  {TOP_peripheral_rw_end,     {undef}}
};

inline OPNDS_INFO_IDX* 
SL_INTRN_INFO_Idx_Array(SL_INTRN_EXP_INFO* info) {
  Is_True(info,  ("Intrinisc Expansion information is NULL")); 
  return &info->res_opnd_info[0]; 
}

inline RES_OPND_INFO  
SL_INTRN_INFO_Res_Opnd_Info(OPNDS_INFO_IDX idx) 	{
  return intrn_opnds_info_array[idx]; 
}

inline SL_INTRN_EXP_INFO* 
SL_Intrn_Info( TOP id ) {
  return &sl_intrn_info_array[ id - TOP_c2_mvgr_r2g_h_u+1/* INTRN_SL_INTRN_BGN*/ ];
}

inline TOP
SL_INTRN_INFO_Opcode ( SL_INTRN_EXP_INFO* info ) {
  return info->opcode;
}
#endif  /* TARG_SL */

#endif /*EXP_INTRN_INFO_H_*/
