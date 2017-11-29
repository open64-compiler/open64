/*

  Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.

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

*/

#ifndef EXP_INTRN_INFO_H_
#define EXP_INTRN_INFO_H_

#include "register.h"
#include "topcode.h"
#ifdef TARG_SL

typedef enum {
   attr_undef = 0,
   opnd_is_lit     = 0x01,
   opnd_is_cop_creg = 0x2, 
   opnd_is_cop_breg = 0x4,
   opnd_is_cop_vreg = 0x8,
   opnd_is_cop_sreg = 0x10,
   opnd_is_integer_reg = 0x20,
   opnd_is_expr = 0x40,
   opnd_is_sym = 0x80,
} OPND_ATTR_TYPE ;

/* select one Build_OP to generate intrinsic instruction  and 
 * number represent which tn operands in function Build_OP */
typedef enum {
   BUILD_OP_0, 
   BUILD_OP_1,
   BUILD_OP_2,
   BUILD_OP_3,
   BUILD_OP_4,
   BUILD_OP_5,
   BUILD_OP_6,
   BUILD_OP_7,
   BUILD_OP_8,
   BUILD_OP_9,
} build_op_type; 

#define OPNDS_MAX 10    // the number of operands
#define OPND_INFO_MAX 100 // the number of operand info type

typedef build_op_type BUILD_OP_TYPE;
typedef UINT32 INTRN_INFO_GRP_IDX;  // index used to point to sl intrsic info entry
typedef UINT32 INTRN_INFO_OPNDS_IDX;

typedef struct sl_opnd_info {
   OPND_ATTR_TYPE type; // attribute type for current operand
   UINT32 pos;  // what position current opnd should locate in function Build_OP
} SL_OPND_INFO;

#define OPNDS_INFO_MAX 90
typedef enum opnd_info_idx {
	opnd_info_undef,
	/* immed */
	immed_pos_0,   immed_pos_1,       immed_pos_2,        immed_pos_3, 
	immed_pos_4,   immed_pos_5,       immed_pos_6,        immed_pos_7,
	immed_pos_8,
	/* cop_creg */
	creg_pos_0, 	creg_pos_1, 	creg_pos_2, 	creg_pos_3,
	/* cop_vreg */
	vreg_pos_0, 	vreg_pos_1, 	vreg_pos_2, 	vreg_pos_3,
    /* cop_sreg */
    sreg_pos_0,     sreg_pos_1,     sreg_pos_2,     sreg_pos_3,
    sreg_pos_4,      
    /* gpr */
    gpr_pos_0,      gpr_pos_1,      gpr_pos_2,      gpr_pos_3,
    gpr_pos_4,
    /* expr */
    expr_pos_0,     expr_pos_1,     expr_pos_2,     expr_pos_4,
    /* symbol */
    sym_pos_0,      sym_pos_1
}OPNDS_ATTR_IDX;

SL_OPND_INFO  sl_opnd_info_array[OPNDS_INFO_MAX+1] = {
	/* NONE */ 
	{attr_undef, 0},
	/* immed */
	{opnd_is_lit, 0},
    {opnd_is_lit, 1},
    {opnd_is_lit, 2},
    {opnd_is_lit, 3},
    {opnd_is_lit, 4},
    {opnd_is_lit, 5},
    {opnd_is_lit, 6},
    {opnd_is_lit, 7},
    {opnd_is_lit, 8},
	/* cop_creg */
	{opnd_is_cop_creg, 0}, 
	{opnd_is_cop_creg, 1}, 
	{opnd_is_cop_creg, 2}, 
	{opnd_is_cop_creg, 3},  
    /* cop_vreg */
    {opnd_is_cop_vreg, 0},  
	{opnd_is_cop_vreg, 1},
	{opnd_is_cop_vreg, 2},
	{opnd_is_cop_vreg, 3},
	 /* cop_sreg */
    {opnd_is_cop_sreg, 0},  
	{opnd_is_cop_sreg, 1},
	{opnd_is_cop_sreg, 2},
	{opnd_is_cop_sreg, 3},
    {opnd_is_cop_sreg, 4},
	/* gpr */ 
	{opnd_is_integer_reg, 0},
    {opnd_is_integer_reg, 1},
  	{opnd_is_integer_reg, 2},
	{opnd_is_integer_reg, 3},
    {opnd_is_integer_reg, 4},
    /* expr */
    {opnd_is_expr,        0},
    {opnd_is_expr,        1},
    {opnd_is_expr,        2},
    {opnd_is_expr,        3},
    /* symbol */
    {opnd_is_sym,         0},
    {opnd_is_sym,         1}
    
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

inline bool OPND_is_cop_creg(SL_OPND_INFO* info) {
     return (info->type & opnd_is_cop_creg);
}

inline bool OPND_is_cop_vreg(SL_OPND_INFO* info) {
     return (info->type & opnd_is_cop_vreg);
}


typedef enum n_opnds{
   zero_opnds, one_opnds, two_opnds, three_opnds, four_opnds, five_opnds,
   six_opnds, seven_opnds, eight_opnds, nine_opnds
} N_OPNDS;

/* this struct is used to descript opnds information for each intrinsic function
 * 		opnds_num : the number of operands 
 *      opnds_idx : point to  */
typedef struct sl_intrn_opnds_info{
   BUILD_OP_TYPE build_type; 
   N_OPNDS opnds_num;  //including result and operands;
   OPNDS_ATTR_IDX opnds_idx[OPNDS_MAX];
   bool need_merge_opnds; // merge several const operands to generate one const
} SL_INTRN_OPNDS_INFO;

/* opnds_num_opnds_infos */
/* u: undef
 * g: gpr
 * s: sreg
 * v: vreg
 * i: immed
 * c: creg
 * e: expr  
 * m: symbol
 * */
typedef enum opnds_info_idx {
  opnds_zero,  
  u_v0_s1,
  u_v1_s0,    
  u_v_v,
  u_s_s_u_u_u_i_i,
  i_i_i,
  i_i_u_i_i,
  i_s_s,
  i_s_s_u_u_u_i,
  i1_c0,
  i1_v0,
  i_v_v,
  i_v_v_u_u_u_i,
  i_i_i_i,
  i_i_i_u_i,
  i_i_i_u_u_i,
  i_i_i_u_u_u_i,
  i_i_i_i_i_i_i_i,
  i_i_i_i_i_i_i_i_i,  //12
  g_c,
  g_s,
  g_v,
  g_v_i,
  g_s_i,
  g_v_i_2,  // gpr and vreg has different position not like above case
  g_c_i,   // 19
  s_i,
  s_s,
  s_s_s,
  s_i_s,
  s1_v0,
  s_v_i,
  s_s_u_u_i,  //new
  s_s_s_s,
  s_s_s_s_2, //different position
  s_s_u_i_i,
  s_s_s_u_i,
  s_s_s_u_u_i,
  s_s_s_u_s,
  s_s_s_u_u_u_i, //28
  v0_i1,
  v1_i0,
  v1_s0,
  v_v,
  v1_u_s0,
  v_s_i,
  v_s_s,
  v_i_v, 
  v_v_v,
  v_v_i,
  v_s_u_i,
  v_v_u_i,
  v_v_u_u_i,  //new 
  v1_v0_u_i3_u_i2,
  v1_s0_u_i3_u_i2,
  v_v_v_u_u_i,
  v_v_i_i,
  v_v_v_i,
  v_v_v_u_i,
  v_v_v_i3_i4, 
  v_v_v_i4_i3,
  v_v_v_s,
  v2_v3_v0_s1,
  v2_v0_v3_s1,
  v_v_v_v,
  v_v_v_u_u_u_i,
  v_v_v_v_2,
  c,
  c1_i0,
  e0_s,
  e1_s,
  e0_v,
  e1_v,
  e_v_i,
  e_c_i,
  m1_c0,
  m1_v0,
  c1_m0,
  v0_m1
} OPNDS_INFO_IDX;


SL_INTRN_OPNDS_INFO  intrn_opnds_info_array[OPNDS_INFO_MAX+1] = {
	/* NONE group 0  */ 
	{BUILD_OP_0, zero_opnds, {}, 0 },

    /* Group 1 : undef  */

    {BUILD_OP_2, three_opnds,  {opnd_info_undef, vreg_pos_0, sreg_pos_1}, 0},     

    {BUILD_OP_2, three_opnds,  {opnd_info_undef, vreg_pos_1, sreg_pos_0}, 0},     
    /* u_v_v */
    {BUILD_OP_3, three_opnds,  {opnd_info_undef, vreg_pos_1, vreg_pos_0}, 0}, 
 
    /* u_s_s_u_u_u_i_i */
    {BUILD_OP_4, eight_opnds,  {opnd_info_undef, sreg_pos_1, sreg_pos_0,opnd_info_undef, \
    	opnd_info_undef, opnd_info_undef, immed_pos_2, immed_pos_3}, 0}, 

    /* Group 2  : immed */
    {BUILD_OP_3,  three_opnds,  {immed_pos_1, immed_pos_2, immed_pos_0 }, 0},

    {BUILD_OP_4,  five_opnds,  {immed_pos_1, immed_pos_0, opnd_info_undef, immed_pos_3, immed_pos_2 }, 0},

    {BUILD_OP_3,  three_opnds,  {immed_pos_2, sreg_pos_1,  sreg_pos_0 }, 0},            

    {BUILD_OP_4,  seven_opnds,  {immed_pos_2, sreg_pos_1,  sreg_pos_0, opnd_info_undef,
    	          opnd_info_undef, opnd_info_undef, immed_pos_3 }, 0},            

    {BUILD_OP_2,  two_opnds,    {immed_pos_1, creg_pos_0 }, 0},                

    {BUILD_OP_2,  two_opnds,    {immed_pos_1, vreg_pos_0 }, 0},                

    {BUILD_OP_3,  three_opnds,  {immed_pos_2, vreg_pos_1,  vreg_pos_0 }, 0},                

    {BUILD_OP_4,  seven_opnds,  {immed_pos_2, vreg_pos_1,  vreg_pos_0, opnd_info_undef,
    	          opnd_info_undef, opnd_info_undef, immed_pos_3 }, 0},           
  
    {BUILD_OP_4, four_opnds,  {immed_pos_1, immed_pos_2, immed_pos_3, immed_pos_0}, 0},             

    {BUILD_OP_4, five_opnds,  {immed_pos_1, immed_pos_2, immed_pos_0, \
    	opnd_info_undef, immed_pos_3}, 0},             

 	{BUILD_OP_4, six_opnds,  {immed_pos_1, immed_pos_2, immed_pos_0, \
    	opnd_info_undef, opnd_info_undef, immed_pos_3}, 0},             

    {BUILD_OP_4, seven_opnds,  {immed_pos_1, immed_pos_2, immed_pos_0, \
    	opnd_info_undef, opnd_info_undef, opnd_info_undef, immed_pos_3}, 0},        

    {BUILD_OP_8, eight_opnds,  {immed_pos_0, immed_pos_1, immed_pos_2, immed_pos_3, \
                   immed_pos_4, immed_pos_5, immed_pos_6, immed_pos_7 }, 0},        

    {BUILD_OP_9, nine_opnds,  {immed_pos_0, immed_pos_1, immed_pos_2, immed_pos_3, \
                   immed_pos_4, immed_pos_5, immed_pos_6, immed_pos_7, \
                   immed_pos_8 }, 0},        

	/* Group 3 : gpr  */
	{BUILD_OP_2, two_opnds,   {gpr_pos_0,  creg_pos_1}, 0},

	{BUILD_OP_2, two_opnds,   {gpr_pos_0,  sreg_pos_1}, 0},

	{BUILD_OP_2, two_opnds,   {gpr_pos_0,  vreg_pos_1}, 0},

	{BUILD_OP_3,  three_opnds, {gpr_pos_0,  vreg_pos_1, immed_pos_2}, 0},

    {BUILD_OP_3,  three_opnds, {gpr_pos_0,  sreg_pos_1, immed_pos_2}, 0},

    {BUILD_OP_3,  three_opnds, {gpr_pos_1,  vreg_pos_0, immed_pos_2}, 0},

    {BUILD_OP_3,  three_opnds, {gpr_pos_1,  creg_pos_0, immed_pos_2}, 0},

    /* Group 4: sreg */
    {BUILD_OP_2,  two_opnds,   {sreg_pos_1, immed_pos_2}, 0},

    {BUILD_OP_2,  two_opnds,   {sreg_pos_1, sreg_pos_0}, 0},     

    {BUILD_OP_3,  three_opnds, {sreg_pos_1, sreg_pos_2,  sreg_pos_0}, 0}, 

    {BUILD_OP_3,  three_opnds, {sreg_pos_1, immed_pos_2, sreg_pos_0}, 0},     
    
    {BUILD_OP_2,  two_opnds, {sreg_pos_1, vreg_pos_0}, 0},     

    {BUILD_OP_3,  three_opnds, {sreg_pos_1, vreg_pos_0, immed_pos_2}, 0},     

    {BUILD_OP_3,  five_opnds, {sreg_pos_1, sreg_pos_0, opnd_info_undef, opnd_info_undef, immed_pos_2}, 0},     

    {BUILD_OP_4,  four_opnds,  {sreg_pos_1, sreg_pos_2, sreg_pos_3, sreg_pos_0}, 0},     

    {BUILD_OP_4,  four_opnds,  {sreg_pos_1, sreg_pos_3, sreg_pos_2, sreg_pos_0}, 0},     

    {BUILD_OP_4,  five_opnds,  {sreg_pos_1, sreg_pos_0,  opnd_info_undef, \
   	              immed_pos_3, immed_pos_2}, 0}, 

    {BUILD_OP_4,  five_opnds,  {sreg_pos_1, sreg_pos_2, sreg_pos_0,  \
           	       opnd_info_undef, immed_pos_3}, 0}, 

    {BUILD_OP_4,  six_opnds,  {sreg_pos_1, sreg_pos_2, sreg_pos_0,  \
           	       opnd_info_undef, opnd_info_undef, immed_pos_3}, 0}, 

    {BUILD_OP_4,  five_opnds,  {sreg_pos_1, sreg_pos_2, sreg_pos_0,  \
           	       opnd_info_undef, sreg_pos_3}, 0}, 

    {BUILD_OP_4,  seven_opnds,  {sreg_pos_1, sreg_pos_2, sreg_pos_0, \
    	opnd_info_undef, opnd_info_undef, opnd_info_undef, immed_pos_3}, 0}, 

    /* Group 5 : vreg */
	{BUILD_OP_2,  two_opnds, {vreg_pos_0, immed_pos_1}, 0},
	
	{BUILD_OP_2,  two_opnds, {vreg_pos_1, immed_pos_0}, 0},	

	{BUILD_OP_2,  two_opnds, {vreg_pos_1, sreg_pos_0}, 0},		

    {BUILD_OP_2,  two_opnds,   {vreg_pos_1, vreg_pos_0}, 0},

    /* v1_u_s0 */
    {BUILD_OP_2,  three_opnds,  {vreg_pos_1, opnd_info_undef, sreg_pos_0}, 0}, 

    {BUILD_OP_3,  three_opnds, {vreg_pos_1, sreg_pos_0, immed_pos_2}, 0},
    
    {BUILD_OP_3,  three_opnds, {vreg_pos_1, sreg_pos_0, sreg_pos_2}, 0},    

    {BUILD_OP_3,  three_opnds, {vreg_pos_1, immed_pos_2, vreg_pos_0}, 0},
    
    {BUILD_OP_3,  three_opnds, {vreg_pos_1, vreg_pos_2, vreg_pos_0}, 0},

    {BUILD_OP_3,  three_opnds, {vreg_pos_1, vreg_pos_0, immed_pos_2}, 0},

    {BUILD_OP_3,  four_opnds, {vreg_pos_1, sreg_pos_0, opnd_info_undef, immed_pos_2}, 0},    
    
    {BUILD_OP_3,  four_opnds,  {vreg_pos_1, vreg_pos_0, opnd_info_undef, immed_pos_2}, 0},        

    {BUILD_OP_3,  five_opnds, {vreg_pos_1, vreg_pos_0, opnd_info_undef, opnd_info_undef, immed_pos_2}, 0},    
    /* v1_v0_u_i3_u_i2 */
    {BUILD_OP_4,  six_opnds, {vreg_pos_1, vreg_pos_0, opnd_info_undef, \
    	          immed_pos_3, opnd_info_undef, immed_pos_2}, 0},    
    /* v1_s0_u_i3_u_i2 */
    {BUILD_OP_4,  six_opnds, {vreg_pos_1, sreg_pos_0, opnd_info_undef, \
    	          immed_pos_3, opnd_info_undef, immed_pos_2}, 0},    

    {BUILD_OP_4,  six_opnds,  {vreg_pos_1, vreg_pos_2, vreg_pos_0, opnd_info_undef, \
    	          opnd_info_undef, immed_pos_3}, 0},        

    {BUILD_OP_4,  four_opnds,  {vreg_pos_1, vreg_pos_0, immed_pos_3, immed_pos_2}, 0},    

    {BUILD_OP_4,  four_opnds,  {vreg_pos_1, vreg_pos_2, vreg_pos_0,  immed_pos_3}, 0},
    
    {BUILD_OP_4,  five_opnds,  {vreg_pos_1, vreg_pos_2, vreg_pos_0,  opnd_info_undef, \
    	          immed_pos_3}, 0},    

    /* v_v_v_i3_i4 */
    {BUILD_OP_5,  five_opnds,  {vreg_pos_1, vreg_pos_2, vreg_pos_0,  
    	               immed_pos_3, immed_pos_4}, 0},
    /* v_v_v_i4_i3 */
    {BUILD_OP_5,  five_opnds,  {vreg_pos_1, vreg_pos_2, vreg_pos_0,  
    	               immed_pos_4, immed_pos_3}, 0},

    {BUILD_OP_4,  four_opnds,  {vreg_pos_1, vreg_pos_2, vreg_pos_0,  sreg_pos_3}, 0},    
    
    {BUILD_OP_4,  four_opnds,  {vreg_pos_2, vreg_pos_3, vreg_pos_0,  sreg_pos_1}, 0},        
    
    /* v2_v0_v3_s1 */
    {BUILD_OP_4,  four_opnds,  {vreg_pos_2, vreg_pos_0, vreg_pos_3,  sreg_pos_1}, 0},        

    {BUILD_OP_4,  four_opnds,  {vreg_pos_1, vreg_pos_2, vreg_pos_3,  vreg_pos_0}, 0},     

    {BUILD_OP_4,  seven_opnds,  {vreg_pos_1, vreg_pos_2, vreg_pos_0, \
    	opnd_info_undef, opnd_info_undef, opnd_info_undef, immed_pos_3}, 0},     

    {BUILD_OP_4,  four_opnds,  {vreg_pos_1, vreg_pos_3, vreg_pos_2,  vreg_pos_0}, 0},     
          
	/* Group 6 : creg */
	{BUILD_OP_1,  one_opnds,   {creg_pos_1}, 0},

	{BUILD_OP_2,  two_opnds,   {creg_pos_1, immed_pos_0}, 0},	

    /* Group 7 : expr */
    {BUILD_OP_2,  two_opnds, {expr_pos_0,  sreg_pos_1}, 0},    

    {BUILD_OP_2,  two_opnds, {expr_pos_1,  sreg_pos_0}, 0},
   
    {BUILD_OP_2,  two_opnds, {expr_pos_0,  vreg_pos_1}, 0},        
   
    {BUILD_OP_2,  two_opnds, {expr_pos_1,  vreg_pos_0}, 0},

    {BUILD_OP_3,  three_opnds, {expr_pos_1,  vreg_pos_0, immed_pos_2}, 0},
    
    {BUILD_OP_3,  three_opnds, {expr_pos_1,  creg_pos_0, immed_pos_2}, 0},
    /* Group 8 : symbol */
    {BUILD_OP_2,  two_opnds,    {sym_pos_1, creg_pos_0 }, 0},                

    {BUILD_OP_2,  two_opnds,    {sym_pos_1, vreg_pos_0 }, 0}, 
    
  	{BUILD_OP_2,  two_opnds,    {creg_pos_1, sym_pos_0}, 0},	
  	
    {BUILD_OP_2,  two_opnds,    {vreg_pos_0, sym_pos_1}, 0},
    
};

inline BUILD_OP_TYPE
SL_INTRN_OPNDS_INFO_Build_Type(SL_INTRN_OPNDS_INFO* info) {
    return info->build_type;
}

inline UINT32 
SL_INTRN_OPNDS_INFO_Num (SL_INTRN_OPNDS_INFO* info) {
    return info->opnds_num;
}

inline OPNDS_ATTR_IDX*
SL_INTRN_OPNDS_INFO_Array (SL_INTRN_OPNDS_INFO* info) {
    return &(info->opnds_idx[0]);
}

inline bool
SL_INTRN_OPNDS_INFO_Merge (SL_INTRN_OPNDS_INFO* info) {
    return info->need_merge_opnds;
}


/* this struct is used to descript intrinsic information for each intrisic function 
 * it is root of struct tree and use index to access corresponding entry.
 * 		opcode       : the first parameter in function Build_OP
 * 		build_op_type: which Build_OP used to build one op for the intrinsic 
 *      group_idx    : idx to sl_intrn_opnds_info array */
typedef struct sl_intrn_info{
   TOP opcode;
   //BUILD_OP_TYPE optype;
   OPNDS_INFO_IDX idx;
} SL_INTRN_INFO;

#define SL_INTRN_MAX 300

SL_INTRN_INFO sl_intrn_info_array[SL_INTRN_MAX+1] = {
   /* NONE */
   {TOP_UNDEFINED,  opnds_zero},

             {  TOP_mfc2_v,  g_v_i }, 
             {  TOP_mfc2_s,  g_s_i }, 

             {  TOP_mtc2_ba, e_v_i }, 
             {  TOP_mtc2_sg, e_v_i }, 
             {  TOP_mtc2_bh, e_v_i }, 
             {  TOP_mtc2_bh_u, e_v_i }, 
             {  TOP_mtc2_bv, e_v_i }, 
             {  TOP_mtc2_bv_u, e_v_i }, 
             {  TOP_mtc2_bb, e_v_i }, 
             {  TOP_mtc2_sc, e_v_i }, 
             {  TOP_cfc2,  g_c }, 
             {  TOP_ctc2,  e_c_i }, 
             {  TOP_lwc2_s_b,    e1_s   }, 
             {  TOP_lwc2_s_b_u,    e1_s   }, 
             {  TOP_lwc2_s_rh_u,    e1_s   }, 
             {  TOP_lwc2_s_h_u,    e1_s   }, 
             {  TOP_lwc2_s_rh,    e1_s   }, 
             {  TOP_lwc2_s_h,    e1_s   }, 
             {  TOP_lwc2_s_rw,    e1_s   }, 
             {  TOP_lwc2_s_w,    e1_s   }, 
             {  TOP_lwc2_v_b_u,    e1_v   }, 
             {  TOP_lwc2_v_b,    e1_v   }, 
             {  TOP_lwc2_v_h_u,    e1_v   }, 
             {  TOP_lwc2_v_h,    e1_v   }, 
             {  TOP_lwc2_v_w,    e1_v   }, 
             {  TOP_lwc2_swin,   u_v0_s1  }, 
             {  TOP_mlwc2_v_b_u,    e1_v   }, 
             {  TOP_mlwc2_v_b,    e1_v   }, 
             {  TOP_mlwc2_v_h_u,    e1_v   }, 
             {  TOP_mlwc2_v_h,    e1_v   }, 
             {  TOP_mlwc2_v_w,    e1_v   }, 
             {  TOP_mlwc2_swin,   u_v0_s1  },
             {  TOP_swc2_s_b,    e1_s   }, 
             {  TOP_swc2_s_h,    e1_s   }, 
             {  TOP_swc2_s_rh,    e1_s   }, 
             {  TOP_swc2_s_w,    e1_s   }, 
             {  TOP_swc2_s_rw,    e1_s   }, 
             {  TOP_swc2_v_b,    e1_v   }, 
             {  TOP_swc2_v_h,    e1_v   }, 
             {  TOP_swc2_v_w,    e1_v   }, 
             {  TOP_swc2_sr,     u_v0_s1 },     
             {  TOP_mswc2_v_b,    e1_v   }, 
             {  TOP_mswc2_v_h,    e1_v   }, 
             {  TOP_mswc2_v_w,    e1_v   },
             {  TOP_mswc2_sr,    u_v0_s1 },       

             {  TOP_lwc2_tmp_s_b,    e1_s   }, 
             {  TOP_lwc2_tmp_s_b_u,    e1_s   }, 
             {  TOP_lwc2_tmp_s_rh_u,    e1_s   }, 
             {  TOP_lwc2_tmp_s_h_u,    e1_s   }, 
             {  TOP_lwc2_tmp_s_rh,    e1_s   }, 
             {  TOP_lwc2_tmp_s_h,    e1_s   }, 
             {  TOP_lwc2_tmp_s_rw,    e1_s   }, 
             {  TOP_lwc2_tmp_s_w,    e1_s   }, 
             {  TOP_lwc2_tmp_v_b_u,    e1_v   }, 
             {  TOP_lwc2_tmp_v_b,    e1_v   }, 
             {  TOP_lwc2_tmp_v_h_u,    e1_v   }, 
             {  TOP_lwc2_tmp_v_h,    e1_v   }, 
             {  TOP_lwc2_tmp_v_w,    e1_v   }, 
             {  TOP_lwc2_tmp_swin,   u_v0_s1  }, 
             {  TOP_mlwc2_tmp_v_b_u,    e1_v   }, 
             {  TOP_mlwc2_tmp_v_b,    e1_v   }, 
             {  TOP_mlwc2_tmp_v_h_u,    e1_v   }, 
             {  TOP_mlwc2_tmp_v_h,    e1_v   }, 
             {  TOP_mlwc2_tmp_v_w,    e1_v   }, 
             {  TOP_mlwc2_tmp_swin,   u_v0_s1  },
             {  TOP_swc2_tmp_s_b,    e1_s   }, 
             {  TOP_swc2_tmp_s_h,    e1_s   }, 
             {  TOP_swc2_tmp_s_rh,    e1_s   }, 
             {  TOP_swc2_tmp_s_w,    e1_s   }, 
             {  TOP_swc2_tmp_s_rw,    e1_s   }, 
             {  TOP_swc2_tmp_v_b,    e1_v   }, 
             {  TOP_swc2_tmp_v_h,    e1_v   }, 
             {  TOP_swc2_tmp_v_w,    e1_v   }, 
             {  TOP_swc2_tmp_sr,     u_v0_s1 },     
             {  TOP_mswc2_tmp_v_b,    e1_v   }, 
             {  TOP_mswc2_tmp_v_h,    e1_v   }, 
             {  TOP_mswc2_tmp_v_w,    e1_v   },
             {  TOP_mswc2_tmp_sr,    u_v0_s1 },       


             {  TOP_cp2_adds,        v_v_v_i3_i4 }, 
             {  TOP_cp2_adds_p,      v_v_v_i3_i4 }, 
             {  TOP_cp2_subs,    v_v_v_u_i   }, 
             {  TOP_cp2_subs_abs,    v_v_v_u_i   }, 
             {  TOP_cp2_abs,         u_v_v    }, 
             {  TOP_cp2_subs_p,    v_v_v_u_i   }, 
             {  TOP_cp2_subs_abs_p,    v_v_v_u_i   }, 
             {  TOP_cp2_abs_p,         u_v_v    }, 
             {  TOP_cp2_mul,    v_v_v   }, 
             {  TOP_cp2_neg,    v_v_v   }, 
             {  TOP_cp2_neg_p,    v_v_v   }, 
             {  TOP_cp2_macs,    v_v_v_i4_i3 }, 
             {  TOP_cp2_mdcs,    v_v_v_i4_i3 }, 
             {  TOP_cp2_clps,    v_v_i   }, 
             {  TOP_cp2_shr,    v_v_i   }, 
             {  TOP_cp2_shl,    v_v_i   }, 
             {  TOP_cp2_shr_p,    v_v_i   }, 
             {  TOP_cp2_shl_p,    v_v_i   }, 
             {  TOP_cp2_cmp_lt_and,    v_v_v   }, 
             {  TOP_cp2_cmp_lt_or,    v_v_v   }, 
             {  TOP_cp2_cmp_le_and,    v_v_v   }, 
             {  TOP_cp2_cmp_le_or,    v_v_v   }, 
             {  TOP_cp2_cmp_eq_and,    v_v_v   }, 
             {  TOP_cp2_cmp_eq_or,    v_v_v   }, 
             {  TOP_cp2_cmp_ge_and,    v_v_v   }, 
             {  TOP_cp2_cmp_ge_or,    v_v_v   }, 
             {  TOP_cp2_cmp_gt_and,    v_v_v   }, 
             {  TOP_cp2_cmp_gt_or,    v_v_v   }, 
             {  TOP_cp2_cmp_and,    v_v_v   }, 
             {  TOP_cp2_cmp_or,    v_v_v   }, 
             {  TOP_cp2_cmp_le,    v_v_v   }, 
             {  TOP_cp2_cmp_lt,    v_v_v   }, 
             {  TOP_cp2_cmp_ge,    v_v_v   }, 
             {  TOP_cp2_cmp_gt,    v_v_v   }, 
             {  TOP_cp2_cmp_lt_and_p,    v_v_v   }, 
             {  TOP_cp2_cmp_lt_or_p,    v_v_v   }, 
             {  TOP_cp2_cmp_le_and_p,    v_v_v   }, 
             {  TOP_cp2_cmp_le_or_p,    v_v_v   }, 
             {  TOP_cp2_cmp_eq_and_p,    v_v_v   }, 
             {  TOP_cp2_cmp_eq_or_p,    v_v_v   }, 
             {  TOP_cp2_cmp_ge_and_p,    v_v_v   }, 
             {  TOP_cp2_cmp_ge_or_p,    v_v_v   }, 
             {  TOP_cp2_cmp_gt_and_p,    v_v_v   }, 
             {  TOP_cp2_cmp_gt_or_p,    v_v_v   }, 
             {  TOP_cp2_cmp_and_p,    v_v_v   }, 
             {  TOP_cp2_cmp_or_p,    v_v_v   }, 
             {  TOP_cp2_cmp_le_p,    v_v_v   }, 
             {  TOP_cp2_cmp_eq_p,    v_v_v   }, 
             {  TOP_cp2_cmp_ge_p,    v_v_v   }, 
             {  TOP_cp2_cmp_gt_p,    v_v_v   }, 
             {  TOP_cp2_cmov_f,    v_v_v   }, 
             {  TOP_cp2_cmov_t,    v_v_v   }, 
             {  TOP_cp2_round,      v_v_i }, 
             {  TOP_cp2_spadds,     v_v_i_i  }, 
             {  TOP_cp2_zero,       v_v  }, 
             {  TOP_cp2_csimd_u,   v_v_u_i  }, 
             {  TOP_cp2_csimd,      v_v_u_i  }, 
             {  TOP_cp2_sad,        v2_v3_v0_s1  }, 
             {  TOP_cp2_satd,       v2_v0_v3_s1  }, 
             {  TOP_cp2_intra_h,     v_v_i  }, 
             {  TOP_cp2_intra_a,     v_v_i  }, 
             {  TOP_cp2_imv,        v_v_v_i  }, 
             {  TOP_cp2_bit,    v_v  }, 
             {  TOP_cp2_scan_dc,    v_v  }, 
             {  TOP_cp2_scan_ac,    v_v  }, 
             {  TOP_cp2_scan_wb,    v1_s0  }, 
             {  TOP_cp2_sadds_o,    i_i_i_u_u_u_i   }, 
             {  TOP_cp2_sadds_s,    s_s_s_u_u_u_i   }, 
             {  TOP_cp2_sadds_v,    v_v_v_u_u_u_i   }, 
             {  TOP_cp2_sadds_o_i,   i_i_i_u_u_u_i   }, 
             {  TOP_cp2_sadds_s_i,   i_s_s_u_u_u_i   }, 
             {  TOP_cp2_sadds_v_i,   i_v_v_u_u_u_i   }, 
             {  TOP_cp2_ssub_abs_o,  i_i_i   }, 
             {  TOP_cp2_ssub_abs_s,   s_s_s }, 
             {  TOP_cp2_ssub_abs_v,   v_v_v }, 
             {  TOP_cp2_ssub_abs_o_i,  i_i_i  }, 
             {  TOP_cp2_ssub_abs_s_i,   s_i_s  }, 
             {  TOP_cp2_ssub_abs_v_i,   v_i_v  }, 
             {  TOP_cp2_ssub_o,        i_i_i  }, 
             {  TOP_cp2_ssub_s,        s_s_s }, 
             {  TOP_cp2_ssub_v,        v_v_v }, 
             {  TOP_cp2_ssub_o_i,       i_i_i  }, 
             {  TOP_cp2_ssub_s_i,       s_i_s  }, 
             {  TOP_cp2_ssub_v_i,      v_v_i  }, 
             {  TOP_cp2_smuls_o,    i_i_i_u_u_i  }, 
             {  TOP_cp2_smuls_s,    s_s_s_u_u_i  }, 
             {  TOP_cp2_smuls_v,    v_v_v_u_u_i  }, 
             {  TOP_cp2_smads,    v_v_v_v  }, 
             {  TOP_cp2_ssmads,   s_s_s_s  }, 
             {  TOP_cp2_scmov1_abs_o,    i_i_i  }, 
             {  TOP_cp2_scmov1_min_o,    i_i_i  }, 
             {  TOP_cp2_scmov1_max_o,    i_i_i  }, 
             {  TOP_cp2_scmov1_abs_s,    s_s_s  }, 
             {  TOP_cp2_scmov1_min_s,    s_s_s  }, 
             {  TOP_cp2_scmov1_max_s,    s_s_s  }, 
             {  TOP_cp2_scmov2,    s_s_s_s  }, 
             {  TOP_cp2_scmov3,    i_i_i_i   }, 
             {  TOP_cp2_smov_o,    v_v_u_u_i }, 
             {  TOP_cp2_smov_s,    s_s_u_u_i }, 
             {  TOP_cp2_smov_v,    v_v_u_u_i }, 
             {  TOP_cp2_smov_dbk_inc_v,    opnds_zero },  // don't need 
             {  TOP_cp2_smov_dbk_inc_h,    opnds_zero },  // don't need 
             {  TOP_cp2_smov_dbk_rst_v,    opnds_zero }, 
             {  TOP_cp2_smov_dbk_rst_h,    opnds_zero }, 
             {  TOP_cp2_sclamp_o,        i_i_i_u_i  }, 
             {  TOP_cp2_sclamp_add_o,    i_i_i_u_i  }, 
             {  TOP_cp2_sclamp_sub_o,    i_i_i_u_i  }, 
             {  TOP_cp2_sclamp_rng_o,    i_i_i_u_i  }, 
             {  TOP_cp2_sclamp_s,        s_s_s_u_i  },       
             {  TOP_cp2_sclamp_add_s,    s_s_s_u_i  }, 
             {  TOP_cp2_sclamp_sub_s,    s_s_s_u_i  }, 
             {  TOP_cp2_sclamp_rng_s,    s_s_s_u_i  }, 
             {  TOP_cp2_srnge_add_o,     i_i_u_i_i   }, 
             {  TOP_cp2_srnge_s,         s_s_u_i_i  }, 
             {  TOP_cp2_sum4,           v_v_v_v_2  }, 
             {  TOP_cp2_dbhsum,    i_i_i_i_i_i_i_i_i  }, 
             {  TOP_cp2_dbvsum,    i_i_i_i_i_i_i_i_i  }, 
             {  TOP_cp2_dbhsum1,    i_i_i_i_i_i_i_i }, 
             {  TOP_cp2_dbvsum1,    i_i_i_i_i_i_i_i }, 
             {  TOP_cp2_dbhsum2,    i_i_i_i_i_i_i_i_i }, 
             {  TOP_cp2_dbvsum2,    i_i_i_i_i_i_i_i_i }, 
             {  TOP_cp2_scond_eq_o,    i_i_i }, 
             {  TOP_cp2_scond_lt_o,    i_i_i }, 
             {  TOP_cp2_scond_le_o,    i_i_i }, 
             {  TOP_cp2_scond_gt_o,    i_i_i }, 
             {  TOP_cp2_scond_ge_o,    i_i_i }, 
             {  TOP_cp2_scond_eq_s,    s_s_s }, 
             {  TOP_cp2_scond_lt_s,    s_s_s }, 
             {  TOP_cp2_scond_le_s,    s_s_s }, 
             {  TOP_cp2_scond_gt_s,    s_s_s }, 
             {  TOP_cp2_scond_ge_s,    s_s_s }, 
             {  TOP_cp2_scond_eq_o_i,    i_i_i }, 
             {  TOP_cp2_scond_lt_o_i,    i_i_i }, 
             {  TOP_cp2_scond_le_o_i,    i_i_i }, 
             {  TOP_cp2_scond_gt_o_i,    i_i_i }, 
             {  TOP_cp2_scond_ge_o_i,    i_i_i }, 
             {  TOP_cp2_scond_eq_s_i,    i_s_s }, 
             {  TOP_cp2_scond_lt_s_i,    i_s_s }, 
             {  TOP_cp2_scond_le_s_i,    i_s_s }, 
             {  TOP_cp2_scond_gt_s_i,    i_s_s }, 
             {  TOP_cp2_scond_ge_s_i,    i_s_s }, 
             {  TOP_cp2_slbs_ls_o,    i_i_i }, 
             {  TOP_cp2_slbs_rs_o,    i_i_i }, 
             {  TOP_cp2_slbs_and_o,    i_i_i }, 
             {  TOP_cp2_slbs_or_o,    i_i_i }, 
             {  TOP_cp2_slbs_xor_o,    i_i_i }, 
             {  TOP_cp2_slbs_ls_s,    s_s_s }, 
             {  TOP_cp2_slbs_rs_s,    s_s_s }, 
             {  TOP_cp2_slbs_and_s,    s_s_s }, 
             {  TOP_cp2_slbs_or_s,    s_s_s }, 
             {  TOP_cp2_slbs_xor_s,    s_s_s }, 
             {  TOP_cp2_slbs_ls_o_i,    i_i_i }, 
             {  TOP_cp2_slbs_rs_o_i,    i_i_i }, 
             {  TOP_cp2_slbs_and_o_i,    i_i_i }, 
             {  TOP_cp2_slbs_or_o_i,    i_i_i }, 
             {  TOP_cp2_slbs_xor_o_i,    i_i_i },             
             {  TOP_cp2_slbs_ls_s_i,    s_i_s }, 
             {  TOP_cp2_slbs_rs_s_i,    s_i_s }, 
             {  TOP_cp2_slbs_and_s_i,    s_i_s }, 
             {  TOP_cp2_slbs_or_s_i,    s_i_s }, 
             {  TOP_cp2_slbs_xor_s_i,    s_i_s },      
             {  TOP_cp2_sbdep,    s_s_s_s_2 }, 
             {  TOP_cp2_sbxtru,    s_s_s_s_2 }, 
             {  TOP_cp2_sbxtrs,    s_s_s_s_2 }, 
             {  TOP_cp2_rsum4,     v_v_u_i }, 
//             {  TOP_cp2_rsum4_sp,  v1_v0_u_i3_u_i2 }, 
             {  TOP_cp2_rsum4_sp_v,  v1_v0_u_i3_u_i2 }, 
             {  TOP_cp2_rsum4_sp_s,  v1_s0_u_i3_u_i2 },              
             {  TOP_cp2_rsum4_s,  v_s_u_i },   
             {  TOP_cp2_xfer_s2v_w, s_v_i },
		     {  TOP_cp2_xfer_s2v_h, s_v_i },
		     {  TOP_cp2_xfer_s2v_l, s_v_i },
		     {  TOP_cp2_xfer_v2s_w, v_s_i },
		     {  TOP_cp2_xfer_v2s_h, v_s_i },
		     {  TOP_cp2_xfer_v2s_l, v_s_i },
		     {  TOP_cp2_xfer_v2s_d, v_s_s },
		     {  TOP_cp2_vblc_b_u,   m1_v0 },
		     {  TOP_cp2_vblc_b,     m1_v0 },
		     {  TOP_cp2_vblc_h_u,   m1_v0 },
		     {  TOP_cp2_vblc_h,     m1_v0 },
		     {  TOP_cp2_vblc_w,     m1_v0 },
     	     {  TOP_mcp2_vblc_b_u,  m1_v0 },
		     {  TOP_mcp2_vblc_b,    m1_v0 },
		     {  TOP_mcp2_vblc_h_u,  m1_v0 },
		     {  TOP_mcp2_vblc_h,    m1_v0 },
		     {  TOP_mcp2_vblc_w,    m1_v0 },
		     {  TOP_cp2_vbls_u,     s1_v0 },
		     {  TOP_cp2_vbls,       s1_v0 },
		     {  TOP_mcp2_vbls_u,    s1_v0 },
		     {  TOP_mcp2_vbls,      s1_v0 },
    	     {  TOP_cp2_vbsc_b,     v0_m1 },
		     {  TOP_cp2_vbsc_h,     v0_m1 },
		     {  TOP_cp2_vbsc_w,     v0_m1 },
		     {  TOP_mcp2_vbsc_b,    v0_m1 },
		     {  TOP_mcp2_vbsc_h,    v0_m1 },
		     {  TOP_mcp2_vbsc_w,    v0_m1 },
		     {  TOP_cp2_vbss_b,     v1_s0 },
		     {  TOP_mcp2_vbss_b,    v1_s0 },      
    	     {  TOP_cp2_sblc,       m1_c0 },
		     {  TOP_cp2_sbls,       s_s   },
		     {  TOP_cp2_sbls_r,     s_s   },
		     {  TOP_cp2_sbs,        c1_i0 },
             {  TOP_cp2_csimd_subpel_1, v_v_u_i }, 
             {  TOP_cp2_csimd_subpel_2, v_v_u_i }, 
             {  TOP_cp2_csimd_mmul,  v_v_v_i },  
             {  TOP_cp2_csimd_mov_l_i, v_v_u_u_i }, 
             {  TOP_cp2_csimd_mov_32l_i, v_v_u_u_i }, 
             {  TOP_cp2_csimd_mov_32h_i, v_v_u_u_i },
             {  TOP_cp2_csimd_mov_h_i,   v_v_u_u_i }, 
             {  TOP_cp2_csimd_mov_l,  v_v }, 
             {  TOP_cp2_csimd_mov_32l, v_v }, 
             {  TOP_cp2_csimd_mov_32h, v_v }, 
             {  TOP_cp2_csimd_mov_h,  v_v },
//After jiqiang has moved all ssmads to new version this intrinsic will
// be removed. 		     
             {  TOP_cp2_ssmads_tmp, v_v_v_v} 	     
};



inline SL_INTRN_INFO* 
SL_Intrn_Info( TOP id ) {
     return &sl_intrn_info_array[ id - TOP_mfc2_v+1/* INTRN_SL_INTRN_BGN*/ ];
}

inline TOP
SL_INTRN_INFO_Opcode ( SL_INTRN_INFO* info ) {
	return info->opcode ;
}

inline INTRN_INFO_OPNDS_IDX
SL_INTRN_INFO_Idx ( SL_INTRN_INFO* info ) {
	return info->idx;
}

inline SL_INTRN_OPNDS_INFO *
SL_Opnds_Info( SL_INTRN_INFO* info ) {
    INTRN_INFO_OPNDS_IDX opnds_grp_idx = SL_INTRN_INFO_Idx(info);
    INT opnds_info_idx = SL_INTRN_INFO_Idx(info);
    return &intrn_opnds_info_array[opnds_info_idx];
}
#endif  /* TARG_SL */

#endif /*EXP_INTRN_INFO_H_*/
