/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


// ====================================================================
// ====================================================================
//
// Module: dra_ec.cxx
//
// Revision history:
//  25-Aug-96: Original Version
//
// Description:
//  Routines used for error checking of reshaped array portions
//  passed as function arguments.
// 
// ====================================================================
// ====================================================================

#include "defs.h"               // standard definitions
#include "wn.h"                 // WN
#include "wn_util.h"            // WN_COPY, WN_INSERT
#include "targ_const.h"         // Host_To_Targ_String
#include "symtab_idx.h"         // ST_IDX etc 
#include "symtab.h"             // ST, TY
#include "symtab_access.h"      
#include "opcode.h"             // OPCODE_make_op
#include "strtab.h"             // Save_Str
#include "config_targ.h"        // Pointer_type
#include "const.h"              // Gen_String_Sym
#include "dra_demangle.h"       // DRA_Demangle_Func
#include "dra_export.h"       	// Find_Return_Registers
#include "dra_ec.h"             // CART_ offsets
#include "dra_internal.h"

// ===================================================================
//
//                          Exported variables
//
// ===================================================================

TY_IDX DRA_EC_struct_ptr_ty = (TY_IDX) NULL;

#define OPC_UNKNOWN OPCODE_UNKNOWN

static OPCODE Ldid_Opcode [MTYPE_LAST + 1] = {
  OPC_UNKNOWN,    /* MTYPE_UNKNOWN */
  OPC_UNKNOWN,    /* MTYPE_UNKNOWN */
  OPC_I4I1LDID,   /* MTYPE_I1 */
  OPC_I4I2LDID,   /* MTYPE_I2 */
  OPC_I4I4LDID,   /* MTYPE_I4 */
  OPC_I8I8LDID,   /* MTYPE_I8 */
  OPC_U4U1LDID,   /* MTYPE_U1 */
  OPC_U4U2LDID,   /* MTYPE_U2 */
  OPC_U4U4LDID,   /* MTYPE_U4 */
  OPC_U8U8LDID,   /* MTYPE_U8 */
  OPC_F4F4LDID,   /* MTYPE_F4 */
  OPC_F8F8LDID,   /* MTYPE_F8 */
#if defined(TARG_IA64) || defined(TARG_X8664)
  OPC_F10F10LDID, /* MTYPE_F10 */
#else
  OPC_UNKNOWN,    /* MTYPE_F10 */
#endif
  OPC_UNKNOWN,    /* MTYPE_F16 */
  OPC_UNKNOWN,    /* MTYPE_STR */
  OPC_FQFQLDID,   /* MTYPE_FQ */
  OPC_UNKNOWN,    /* MTYPE_M */
  OPC_C4C4LDID,   /* MTYPE_C4 */
  OPC_C8C8LDID,   /* MTYPE_C8 */
  OPC_CQCQLDID,   /* MTYPE_CQ */
  OPC_UNKNOWN,     /* MTYPE_V */
  OPC_UNKNOWN,    /* MTYPE_BS */
  OPC_UNKNOWN,    /* MTYPE_A4 */
  OPC_UNKNOWN,    /* MTYPE_A8 */
#if defined(TARG_IA64) || defined(TARG_X8664)
  OPC_C10C10LDID, /* MTYPE_C10 */
#endif
};



// ===================================================================
//
//                      Local function prototypes
//
// ===================================================================

static WN*  Gen_Malloc_Cart (WN* block,
                             WN* insert_wn, 
                             INT32 size, 
                             ST* cart_st);

static void Gen_Free_Cart (WN* block,
                           WN* prev_wn, 
                           WN* stid_wn, 
                           ST* cart_st);

static WN*  Gen_Call_Array (WN* block,
                            WN* prev_wn, 
                            WN* array_wn,
                            ST* func_st, 
                            ST* retval_st);

static WN*  Get_Array_Dim_Size (TY& array_ty, 
                                INT dim);

static ST* Create_Local_ST (char *name, TY_IDX ty);

static ST* Declare_Func_N_Arg (const char* ty_name, 
                               const char* st_name, 
                               TY_IDX ret_ty, 
                               INT nargs, 
                               TY_IDX ty_array[]);



// ===================================================================
//
//                       File static variables
//
// ===================================================================

static ST* ECHT_Check   = NULL;
static ST* ECHT_Compare = NULL;


// ===================================================================
//
//                    Exported function defintions
//
// ===================================================================


// ===================================================================
//
// Function Name: DRA_EC_Declare_Types
//
// Description: Declare types of data structures used in 
//              error-checking of reshaped array portions
//              passed as arguments.
//
// ===================================================================

void 
DRA_EC_Declare_Types () 
{
  // First declare the desired type of the runtime structure.

  // declare an array of INT64 
  TY_IDX I8_ty = Be_Type_Tbl(MTYPE_I8);
  TY_IDX index_array_ty_idx;
  TY& index_array_ty = New_TY (index_array_ty_idx);

  ARB_HANDLE new_ari = New_ARB ();
  ARB_Init (new_ari, 1, 1, 1);
  Set_ARB_first_dimen(new_ari);
  Set_ARB_last_dimen(new_ari);
  Set_ARB_const_lbnd(new_ari);
  Set_ARB_lbnd_val(new_ari,0);
  Set_ARB_const_ubnd(new_ari);
  Set_ARB_ubnd_val(new_ari,9);
  Set_ARB_const_stride(new_ari);
  Set_ARB_stride_val(new_ari,TY_size(I8_ty));

  TY_Init (index_array_ty, 10*TY_size(I8_ty), KIND_ARRAY, MTYPE_UNKNOWN, Save_Str("INDEX_ARRAY_TY"));
  Set_TY_arb(index_array_ty, new_ari);
  Set_TY_align (index_array_ty_idx, 8);
  index_array_ty.Set_etype(I8_ty);

  FLD_HANDLE first_field = New_FLD ();

  FLD_Init (first_field, Save_Str("array_base"), I8_ty, CART_array_base);

  FLD_HANDLE field = New_FLD ();
  FLD_Init (field, Save_Str("ndims"), I8_ty, CART_ndims);

  field = New_FLD ();
  FLD_Init (field, Save_Str("element_size"), I8_ty, CART_element_size);

  field = New_FLD ();
  FLD_Init (field, Save_Str("index"), index_array_ty_idx, CART_index);
  Set_FLD_last_field(field);

  // declare a struct with the above field list 
  TY_IDX ec_struct_ty_idx;
  TY& ec_struct_ty       = New_TY(ec_struct_ty_idx);

  TY_Init (ec_struct_ty, 3*TY_size(I8_ty) + TY_size(index_array_ty), KIND_STRUCT, MTYPE_M, Save_Str("DRA_EC_STRUCT_TY"));
  Set_TY_fld(ec_struct_ty, first_field);
  Set_TY_align (ec_struct_ty_idx, 8);

  DRA_EC_struct_ptr_ty = Make_Pointer_Type ( ec_struct_ty_idx, TRUE );
  Set_TY_ptr_as_array(Ty_Table[DRA_EC_struct_ptr_ty]);


  TY_IDX voidpty = Make_Pointer_Type (Be_Type_Tbl(MTYPE_V), TRUE);
  ECHT_Check = Declare_Func_N_Arg (".__dsm_echt_check", 
                                   "__dsm_echt_check",
                                   voidpty,
                                   1,
                                   &voidpty);

  TY_IDX string_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1), TRUE);
  TY_IDX arg_ty[4] = {voidpty, voidpty, string_ty, string_ty};
  ECHT_Compare = Declare_Func_N_Arg (".__dsm_echt_compare", 
                                     "__dsm_echt_compare",
                                     Be_Type_Tbl(MTYPE_V),
                                     4,
                                     arg_ty);
}



// ===================================================================
//
// Function Name: DRA_EC_Array_Portion_Parms
//
// Description: Recognize all array parameters, and check if they are 
//              reshaped portions. If so, generate runtime check.
//
// ===================================================================

void 
DRA_EC_Array_Portion_Parms (WN* func_wn, WN* entry_wn) 
{
  Is_True (WN_opcode(func_wn) == OPC_FUNC_ENTRY &&
           (WN_opcode(entry_wn) == OPC_FUNC_ENTRY || 
            WN_opcode(entry_wn) == OPC_ALTENTRY),
           ("EC_Parms: expected a FUNC_ENTRY or ALTENTRY node\n"));
  
  // insert into block after prev_wn, or before insert_wn
  //
  WN *block     = WN_func_body(func_wn);
  WN *prev_wn   = Get_Preamble_End(entry_wn);
  WN *insert_wn = WN_next(prev_wn);

  // For FUNC_ENTRY num_formals is kid_count-3
  // For ALTEENTRY num_formals is kid_count
  //
  INT num_formals = (WN_opcode(entry_wn) == OPC_FUNC_ENTRY ?
                     WN_num_formals(entry_wn) : WN_kid_count(entry_wn));

  for (INT i = 0; i < num_formals; i++) {

    ST& formal_st = St_Table[WN_st_idx(WN_formal(entry_wn, i))];
    TY_IDX array_ty = (TY_IDX) NULL;
    TY& formal_ty = Ty_Table[ST_type(formal_st)];
    Is_True (ST_sclass(formal_st) == SCLASS_FORMAL ||
             ST_sclass(formal_st) == SCLASS_FORMAL_REF,
             ("EC_Parms: IDNAME(%s) not a FORMAL\n", ST_name(formal_st)));

    BOOL array_parm = FALSE;
    switch (ST_sclass(formal_st)) {
    case SCLASS_FORMAL: 
      if (TY_kind(formal_ty) == KIND_POINTER &&
          TY_kind(TY_pointed(formal_ty)) == KIND_ARRAY) {
        array_ty = TY_pointed(formal_ty);
        array_parm = TRUE;
      }
      break;
    case SCLASS_FORMAL_REF:
      if (TY_kind(formal_ty) == KIND_ARRAY) {
        array_ty = ST_type(formal_st);
        array_parm = TRUE;
      }
      break;
    }
    if (!array_parm) continue;

    //
    // Here we could also check that the array is not reshaped!
    //

    // ok -- it is an array parameter
    //
    char name[64];
    (void) strcpy(name, "$cart_formal_");
    (void) strncat(name, ST_name(WN_st(entry_wn)), 20);
    (void) strncat(name, ST_name(formal_st), 20);
    ST *cart_st = Create_Local_ST(name, DRA_EC_struct_ptr_ty);

    // Generate a check to see if in the hash-table
    //
    WN *array_ldid = WN_Ldid(Pointer_type, (WN_OFFSET)0, &formal_st, ST_type(formal_st));

    WN* stid_wn = 
      Gen_Call_Array (block, prev_wn, array_ldid, ECHT_Check, cart_st);
    prev_wn = stid_wn;

    WN* cart_ldid_wn = WN_Ldid (Pointer_type, 0, cart_st, ST_type(cart_st));
    WN* if_wn = WN_CreateIf(cart_ldid_wn, WN_CreateBlock(), WN_CreateBlock());
    WN_INSERT_BlockAfter (block, prev_wn, if_wn);
    WN_Set_Linenum (if_wn, WN_Get_Linenum(prev_wn));
    prev_wn = if_wn;

    // cart->array_base = array-base
    array_ldid = WN_COPY_Tree(array_ldid);
    cart_ldid_wn = WN_COPY_Tree(cart_ldid_wn);
    WN *istore_wn = 
      WN_CreateIstore (OPCODE_make_op (OPR_ISTORE, MTYPE_V, Pointer_type),
                       CART_array_base, 
                       Make_Pointer_Type(Be_Type_Tbl(Pointer_type), TRUE),
                       array_ldid,
                       cart_ldid_wn);
    WN_Set_Linenum (istore_wn, WN_Get_Linenum(insert_wn));
    WN_INSERT_BlockBefore (WN_then(if_wn), NULL, istore_wn);
    insert_wn = WN_first(WN_then(if_wn));
    prev_wn = insert_wn;

    // now generate allocate cart before it
    INT ndims = TY_AR_ndims(Ty_Table[array_ty]);
    WN* cart_stid_wn = 
      Gen_Malloc_Cart (WN_then(if_wn), insert_wn, 8*(ndims+3), cart_st);

    
    OPCODE istore_op = OPCODE_make_op(OPR_ISTORE, MTYPE_V, MTYPE_I8);
    TY_IDX I8ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8), TRUE);

    // cart->ndims = ndims
    cart_ldid_wn = WN_COPY_Tree(cart_ldid_wn);
    istore_wn = WN_CreateIstore (istore_op, 
                                 CART_ndims, 
                                 I8ptr_ty,
                                 WN_Intconst(MTYPE_I8, ndims),
                                 cart_ldid_wn);
    WN_Set_Linenum(istore_wn, WN_Get_Linenum(insert_wn));
    WN_INSERT_BlockAfter (WN_then(if_wn), prev_wn, istore_wn);
    prev_wn = istore_wn;

    // cart->element_size = element_size
    cart_ldid_wn = WN_COPY_Tree(cart_ldid_wn);
    istore_wn = WN_CreateIstore(istore_op, 
                                CART_element_size, 
                                I8ptr_ty,
                                WN_Intconst(MTYPE_I8,
                                            TY_size(TY_AR_etype(array_ty))),
                                cart_ldid_wn);
    WN_Set_Linenum(istore_wn, WN_Get_Linenum(insert_wn));
    WN_INSERT_BlockAfter (WN_then(if_wn), prev_wn, istore_wn);
    prev_wn = istore_wn;
    
    for (INT j=0; j<ndims; j++) {
      // cart->dim-size = dim-size
      WN* size_wn = Get_Array_Dim_Size (Ty_Table[array_ty], j);
      cart_ldid_wn = WN_COPY_Tree(cart_ldid_wn);
      istore_wn = WN_CreateIstore (istore_op, 
                                   CART_index+8*j, 
                                   I8ptr_ty,
                                   size_wn, 
                                   cart_ldid_wn);
      WN_Set_Linenum(istore_wn, WN_Get_Linenum(insert_wn));
      WN_INSERT_BlockAfter (WN_then(if_wn), prev_wn, istore_wn);
      prev_wn = istore_wn;
    }

    // call compare cart
    {
      array_ldid = WN_COPY_Tree(array_ldid);
      cart_ldid_wn = WN_COPY_Tree(cart_ldid_wn);

      WN* call_wn = WN_Create(OPCODE_make_op(OPR_CALL, MTYPE_V, MTYPE_V), 4);

      // array
      WN* parm_wn = WN_CreateParm (Pointer_type, 
                                   array_ldid,
                                   Be_Type_Tbl(Pointer_type),
                                   WN_PARM_BY_VALUE);
      WN_kid(call_wn, 0) = parm_wn;

      // cart
      parm_wn = WN_CreateParm (Pointer_type, 
                               cart_ldid_wn,
                               Be_Type_Tbl(Pointer_type),
                               WN_PARM_BY_VALUE);
      WN_kid(call_wn, 1) = parm_wn;

      // function name
      OPCODE lda_op = OPCODE_make_op (OPR_LDA, Pointer_type, MTYPE_V);
      TY_IDX string_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1), TRUE);

      char *func_name = DRA_Demangle_Func(ST_name(WN_st(entry_wn)));
      if (func_name == NULL) {
        func_name = ST_name(WN_st(entry_wn));
      }
      TCON tcon = 
        Host_To_Targ_String (MTYPE_STRING, func_name, strlen(func_name)+1);
      ST *rt_func_name = 
        Gen_String_Sym (&tcon, Be_Type_Tbl(MTYPE_STRING), FALSE);
      WN *func_name_wn = WN_CreateLda (lda_op, 0, string_ty, rt_func_name);
      parm_wn = WN_CreateParm (Pointer_type, 
                               func_name_wn,
                               Be_Type_Tbl(Pointer_type),
                               WN_PARM_BY_REFERENCE);
      WN_kid(call_wn, 2) = parm_wn;

      // array name
      char* array_name = ST_name(formal_st);
      tcon = 
        Host_To_Targ_String (MTYPE_STRING, array_name, strlen(array_name)+1);
      ST *rt_array_name = 
        Gen_String_Sym (&tcon, Be_Type_Tbl(MTYPE_STRING), FALSE);
      WN *array_name_wn = WN_CreateLda (lda_op, 0, string_ty, rt_array_name);
      parm_wn = WN_CreateParm (Pointer_type, 
                               array_name_wn,
                               Be_Type_Tbl(Pointer_type),
                               WN_PARM_BY_REFERENCE);
      WN_kid(call_wn, 3) = parm_wn;

      WN_st_idx(call_wn) = ST_st_idx(ECHT_Compare);
      WN_INSERT_BlockAfter (WN_then(if_wn), prev_wn, call_wn);
      prev_wn = call_wn;
    }

    Gen_Free_Cart (WN_then(if_wn), prev_wn, cart_stid_wn, cart_st);
    prev_wn = if_wn;
  }
} 



// ===================================================================
// 
// Function Name: Get_Array_Dim_Size
// 
// Description: Given an array type and a dimension number,
//              return a WHIRL tree for the size of that dimension.
//
// ===================================================================

static WN* 
Get_Array_Dim_Size (TY& array_ty, INT dim) 
{
  INT ndims = TY_AR_ndims(array_ty);

  if ((!TY_AR_const_lbnd(array_ty, dim)) &&
      (TY_AR_lbnd_val(array_ty, dim) == 0))
    return WN_Intconst (MTYPE_I8, -1);
  if ((!TY_AR_const_ubnd(array_ty, dim)) &&
      (TY_AR_ubnd_val(array_ty, dim) == 0))
    return WN_Intconst (MTYPE_I8, -1);

  WN* lb;
  if (TY_AR_const_lbnd(array_ty, dim)) {
    lb = WN_Intconst(MTYPE_I8, TY_AR_lbnd_val(array_ty, dim));
  }
  else {
    ST_IDX  lbnd_st_idx = TY_AR_lbnd_var(array_ty,dim);
    ST&	  lbnd_st     = St_Table [lbnd_st_idx];
    TY_IDX  lbnd_ty_idx = ST_type (lbnd_st);

    TYPE_ID lty_mtype = TY_mtype  (Ty_Table [lbnd_ty_idx]);
    lb = WN_CreateLdid (Ldid_Opcode [lty_mtype],
                        (WN_OFFSET)0, &lbnd_st, lbnd_ty_idx);
  }

  WN* ub;
  if (TY_AR_const_ubnd(array_ty, dim)) {
    ub = WN_Intconst(MTYPE_I8, TY_AR_ubnd_val(array_ty, dim));
  }
  else {
    ST_IDX  ubnd_st_idx = TY_AR_ubnd_var(array_ty,dim);
    ST&     ubnd_st     = St_Table [ubnd_st_idx];
    TY_IDX  ubnd_ty_idx = ST_type (ubnd_st);

    TYPE_ID lty_mtype = TY_mtype  (Ty_Table [ubnd_ty_idx]);
    ub = WN_CreateLdid (Ldid_Opcode [lty_mtype],
                        (WN_OFFSET)0, &ubnd_st, ubnd_ty_idx);
  }

  WN* size_wn = WN_Add (MTYPE_I8, 
                        WN_Sub (MTYPE_I8, ub, lb),
                        WN_Intconst (MTYPE_I8, 1));
  return size_wn;
}




// ===================================================================
// 
// Function Name: Gen_Malloc_Cart
//
// Description: Generate  cart_st = malloc(size) 
//              Insert code into block before insert_wn
//              Return the STID of cart_st
//
// ===================================================================

static WN* 
Gen_Malloc_Cart (WN* block, WN* insert_wn, INT32 size, ST* cart_st) 
{
  OPCODE icallop = OPCODE_make_op(OPR_INTRINSIC_CALL, Pointer_type, MTYPE_V);
  WN* call_wn = WN_Create(icallop, 1);
  WN_intrinsic(call_wn) =
    Pointer_Size == 8 ? INTRN_U8I8MALLOC : INTRN_U4I4MALLOC;
  WN_Set_Linenum (call_wn, WN_Get_Linenum(insert_wn));
  WN* size_wn = WN_Intconst (Pointer_Size == 8 ? MTYPE_I8 : MTYPE_I4, size);
  WN* parm_wn = WN_CreateParm(MTYPE_U8, size_wn,
                              Be_Type_Tbl(MTYPE_U8), WN_PARM_BY_VALUE);
  WN_kid0(call_wn) = parm_wn;

  WN_INSERT_BlockBefore (block, insert_wn, call_wn);
  PREG_NUM rreg1, rreg2;
  ST* rst = Find_Return_Registers(Pointer_type, &rreg1, &rreg2);
  FmtAssert(rreg1 != 0 && rreg2 == 0, ("Bad pointer type ret regs"));
      
  WN *ldid_wn = WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                              Pointer_type,
                                              Pointer_type),
                               rreg1, 
                               rst, 
                               DRA_EC_struct_ptr_ty);

  OPCODE op = OPCODE_make_op(OPR_STID, MTYPE_V, Pointer_type);
  WN *stid_wn = WN_CreateStid(op, 0, cart_st, ST_type(cart_st), ldid_wn);

  WN_Set_Linenum (stid_wn, WN_Get_Linenum(insert_wn));
  WN_INSERT_BlockBefore (block, insert_wn, stid_wn);

  return stid_wn;
} 




// ===================================================================
// 
// Function Name: Gen_Free_Cart
//
// Description: Generate free(cart_st) and insert code into block 
//              after prev_wn
//
// ===================================================================

static void 
Gen_Free_Cart (WN* block, WN* prev_wn, WN* stid_wn, ST *cart_st) 
{
  OPCODE icallop = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
  WN* call_wn = WN_Create(icallop, 1);
  WN_intrinsic(call_wn) = Pointer_Size == 8 ? INTRN_U8FREE : INTRN_U4FREE;

  WN* ldid_wn = WN_Ldid (Pointer_type, 0, cart_st, ST_type(cart_st));
  WN* parm_wn = WN_CreateParm(Pointer_type, 
                              ldid_wn,
                              Be_Type_Tbl(Pointer_type), 
                              WN_PARM_BY_REFERENCE);
  WN_kid0(call_wn) = parm_wn;

  WN_Set_Linenum (call_wn, WN_Get_Linenum(prev_wn));
  WN_INSERT_BlockAfter (block, prev_wn, call_wn);
} 




// ===================================================================
// 
// Function Name: Gen_Call_Array
//
// Description: Given an ldid in array_wn, generate a call to the 
//              given function with a single argument, array_wn.
//              Store the return value of the function into retval_st, 
//              and return the stid. 
//              Insert generated code into block after prev_wn.
//
// ===================================================================

static WN* 
Gen_Call_Array (WN* block, 
                WN* prev_wn, 
                WN* array_wn, 
                ST* func_st, 
                ST* retval_st) 
{
  WN* call_wn = WN_Create(OPCODE_make_op(OPR_CALL, Pointer_type, MTYPE_V), 1);
  WN* parm_wn = WN_CreateParm (Pointer_type, 
                               array_wn,
                               Be_Type_Tbl(Pointer_type),
                               WN_PARM_BY_VALUE);
  WN_kid(call_wn, 0) = parm_wn;
  WN_st_idx(call_wn) = ST_st_idx(func_st);
  
  WN_INSERT_BlockAfter (block, prev_wn, call_wn);
  prev_wn = call_wn;

  // generate retval assignment into retval_st
  //
  PREG_NUM rreg1, rreg2;
  ST* rst = Find_Return_Registers(Pointer_type, &rreg1, &rreg2);
  FmtAssert(rreg1 != 0 && rreg2 == 0, ("Bad pointer type ret regs"));
      
  WN *ldid_wn = WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                              Pointer_type,
                                              Pointer_type),
                               rreg1, 
                               rst, 
                               DRA_EC_struct_ptr_ty);

  OPCODE op = OPCODE_make_op(OPR_STID, MTYPE_V, Pointer_type);
  WN *stid_wn = WN_CreateStid(op, 0, retval_st, ST_type(retval_st), ldid_wn);

  WN_Set_Linenum (stid_wn, WN_Get_Linenum(prev_wn));
  WN_INSERT_BlockAfter (block, prev_wn, stid_wn);

  return stid_wn;
} 



// ===================================================================
//
// Create local ST* entry with a given name and type
//
// ===================================================================

static ST* 
Create_Local_ST(char *name, TY_IDX ty)
{
  ST *st = New_ST(CURRENT_SYMTAB);
  Set_ST_is_temp_var(st);
  ST_Init(st, Save_Str(name), CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, ty);
/* TODO::
  Set_ST_pt_to_unique_mem(st);
*/
  return st;
}


// ===================================================================
//
// Declare prototype for a function with N arguments
//
// ===================================================================

static ST* 
Declare_Func_N_Arg (const char* ty_name, 
                    const char* st_name, 
                    TY_IDX ret_ty, 
                    INT nargs, 
                    TY_IDX ty_array[])
{
  // Create function prototype
  //
  TY_IDX voidpty = Make_Pointer_Type (Be_Type_Tbl(MTYPE_V), TRUE); 

  TY_IDX func_ty;
  TY& ty = New_TY(func_ty);
  PU_IDX pu_idx;
  PU& pu = New_PU (pu_idx);

  TYLIST_IDX idx = Tylist_Table.Insert(ret_ty);
  for (INT i = 0; i < nargs; i++) {
    Tylist_Table.Insert(ty_array[i]);
  }
  Tylist_Table.Insert(0);
  TY_Init (ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, Save_Str(ty_name));
  Set_TY_tylist (ty, idx);

  Set_TY_align (func_ty, TY_align(voidpty));
  PU_Init (pu, func_ty, GLOBAL_SYMTAB + 1);

  // Make an ST: add function to global symbol table
  //
  ST *func_st = New_ST ( GLOBAL_SYMTAB );
  ST_Init(func_st, Save_Str(st_name), CLASS_FUNC, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, (TY_IDX) pu_idx);

  return func_st;
}
