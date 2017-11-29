/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


//                     LEGO LOCAL
//                     ----------
//
// Description:
//
//	Look for MP regions with local or last_local clause for reshaped
//	arrays. Replace references to these arrays in the MP region with
//	references to local temp arrays. Create loops for copy-out if it
//	is for a last_local.  For now, local temp arrays are all
//	statically allocated arrays so we handle only fixed size array
//	for now. Local temp arrays are re-used across multiple MP regions.
//
/* ====================================================================
 * ====================================================================
 *
 * Module: lego_local.cxx
 * $Revision: 1.6 $
 * $Date: 04/12/21 14:57:13-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.lego_local.cxx $
 *
 * Revision history:
 *  08-21-96 - Original Version
 *
 * Description: Lego local and last_local
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

const static char *source_file = __FILE__;
const static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.lego_local.cxx $";

#include <sys/types.h>
#include <alloca.h>
#include <ctype.h>
#include <limits.h>

#include "pu_info.h"
#include "lnopt_main.h"
#include "config_targ.h"
#include "erbe.h"
#include "stab.h"
#include "strtab.h"
#include "stblock.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "lego_util.h"
#include "lego_pragma.h"
#include "scalar_expand.h"
#include "lego_affinity.h"
#include "config.h"
#include "region_util.h"
#include "debug.h"

typedef struct {
  WN*		pragma;
  BOOL		localized;
} LOCAL_PRAGMA;

typedef DYN_ARRAY<LOCAL_PRAGMA> LOCAL_PRAGMA_STACK;
typedef DYN_ARRAY<WN*> DYNARRAY_OF_ARRAY_REFERENCES;

#undef VB_PRINT
#define VB_PRINT(x) if (Verbose_Lego) { x; }

static void Lego_Fix_Local_Rec(WN *wn, BOOL in_a_parallel_region);
static ST *Create_Local_Lda_Array(DISTR_INFO *di, WN* mp_region);
static void Get_Reshaped_Array_Refs(WN* wn,DYNARRAY_OF_ARRAY_REFERENCES *refs);
static ST* Create_Local_Array_ST (char* array_name, TY_IDX ty);
static WN* Copy_Array(DISTR_INFO *di, ST *local_st, WN *mp_region,
                       WN *tmp_array_def, BOOL copy_out);
extern void Fix_Up_Loop_Info(WN *IO_node, WN **loops, INT num_loops);
  // from lego_io.cxx


void Lego_Fix_Local(WN *func_nd)
{
  VB_PRINT(printf("Lego Localization Phase Begin..\n"));
  Lego_Fix_Local_Rec(func_nd,/* in_a_parallel_region= */ FALSE);
  VB_PRINT(printf("Lego Localization Phase End.\n"));
}

static void Lego_Fix_Local_Rec(WN *wn, BOOL in_a_parallel_region)
{
  
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK) {
  
    WN *kid = WN_first (wn);
    while (kid) {
      Lego_Fix_Local_Rec(kid,in_a_parallel_region);
      kid = WN_next(kid);
    }
  } else if (Is_Mp_Region(wn)) {

    MEM_POOL_Push(&LNO_local_pool);

    LOCAL_PRAGMA_STACK local_pragma_stack;
    local_pragma_stack.Set_Mem_Pool(&LNO_local_pool);
    WN* mp_region=wn;

    WN* block=WN_region_pragmas(wn);
    WN *kid = WN_first (block);
    BOOL is_parallel_region=FALSE;
    while (kid) {
      WN_PRAGMA_ID pragma = (WN_PRAGMA_ID)WN_pragma(kid);
      if (pragma == WN_PRAGMA_LOCAL ||
	  pragma == WN_PRAGMA_LASTLOCAL ||
	  pragma == WN_PRAGMA_FIRSTPRIVATE) {

        ST* st=WN_st(kid);
        DISTR_INFO* di=da_hash->Find(st);

        // find local and last_local pragmas for
        // reshaped local and formal arrays
        if (di && di->IsReshaped()) {
  
          INT i=local_pragma_stack.Newidx();
          local_pragma_stack[i].pragma=kid;
          local_pragma_stack[i].localized=FALSE;
          VB_PRINT(printf("Get reshaped array %s\n", ST_name(st)));
        }
      } else if (pragma == WN_PRAGMA_PARALLEL_BEGIN)
	is_parallel_region=TRUE;

      kid = WN_next(kid);
    }

    if (local_pragma_stack.Elements()==0) {
      MEM_POOL_Pop(&LNO_local_pool);
      // now walk the body of the MP region for nested MP-regions.
      Lego_Fix_Local_Rec (WN_region_body(wn),
			is_parallel_region || in_a_parallel_region);
      return;
    }
  
    DYNARRAY_OF_ARRAY_REFERENCES *refs =
      CXX_NEW(DYNARRAY_OF_ARRAY_REFERENCES(&LNO_local_pool),&LNO_local_pool);

    Get_Reshaped_Array_Refs(wn,refs);

    INT i;
    for (i=0; i<refs->Elements(); i++) {

      WN* ldid_lda=(*refs)[i];
      if (ldid_lda == NULL) {
        // we've already processed this one (see for-k loop below) so
        // just keep going
        continue;
      }
      ST* st_tmp=WN_st(ldid_lda);
      ST* st_new=NULL;
      DISTR_INFO* di=da_hash->Find(st_tmp);

      // for now, we only handle fixed size array and always create
      // a local lda array to replace the reshaped array
      for (INT j=0; j<local_pragma_stack.Elements(); j++) {
        if (st_tmp==WN_st(local_pragma_stack[j].pragma))
          if (local_pragma_stack[j].localized)
            break;
          else {
            if (st_new==NULL)
	      st_new=Create_Local_Lda_Array(di,mp_region);
            WN* local_alias_host=NULL;
            WN* wn_without_alias=NULL;
	    // points to the iload if copy-in and istore if copy-out

            if ((WN_PRAGMA_ID)WN_pragma(local_pragma_stack[j].pragma) ==
                WN_PRAGMA_LASTLOCAL) {
              WN* copy_out_store=Copy_Array(di, st_new, mp_region, NULL, TRUE);

	      if (in_a_parallel_region) {
		FmtAssert(WN_next(mp_region) != NULL, 
		  ("Lego_Fix_Local_Rec: Expecting non-NULL pointer"));
		Create_Single_Region(LWN_Get_Parent(mp_region), WN_next(mp_region), 
		  WN_next(WN_next(mp_region))); 
	      }

              local_alias_host=WN_kid0(copy_out_store);
              Create_lda_array_alias(
		Alias_Mgr,WN_array_base(WN_kid0(local_alias_host)),
				  local_alias_host);

	      wn_without_alias=copy_out_store;

            } else if ((WN_PRAGMA_ID)WN_pragma(local_pragma_stack[j].pragma) ==
                WN_PRAGMA_FIRSTPRIVATE) {
              WN* copy_in_store=Copy_Array(di, st_new, mp_region, NULL, FALSE);

	      if (in_a_parallel_region) 
	        Create_Single_Region(LWN_Get_Parent(mp_region), WN_prev(mp_region), mp_region);

              WN* copy_in_load=WN_kid0(copy_in_store);

              local_alias_host=copy_in_store;
              Create_lda_array_alias(
		Alias_Mgr,WN_array_base(WN_kid1(local_alias_host)),
				  local_alias_host);

	      wn_without_alias=copy_in_load;
            }

            local_pragma_stack[j].localized=TRUE;

            for (INT k=i; k<refs->Elements(); k++) {
              WN* orig_ldid_lda=(*refs)[k];
              if (WN_st(orig_ldid_lda)==st_tmp) {

                WN* iload_istore=
                  LWN_Get_Parent(LWN_Get_Parent(orig_ldid_lda));

                if (iload_istore &&
                    WN_operator(iload_istore)!=OPR_ILOAD &&
                    WN_operator(iload_istore)!=OPR_ISTORE)
                  iload_istore=NULL;

                if (wn_without_alias!=NULL && iload_istore) {
                  Copy_alias_info(Alias_Mgr,iload_istore,wn_without_alias);
                  // we might get iload under io, which has
		  // conservative alias info
                  wn_without_alias=NULL;
                }

                WN* parent=LWN_Get_Parent(orig_ldid_lda);
		if (WN_operator(parent) == OPR_PARM) {
		  OPCODE opcode=WN_opcode(LWN_Get_Parent(parent));
		  Is_True(OPCODE_is_call(opcode) ||
			  OPCODE_operator(opcode)==OPR_PURE_CALL_OP || // KEY
			  OPCODE_operator(opcode)==OPR_INTRINSIC_OP,
			  ("PARAM under neither call or intrinisc op?"));
		  ErrMsgSrcpos(EC_DRA_unsupported_type,
                    WN_Get_Linenum(mp_region),
                    "LOCAL/LASTLOCAL/FIRSTPRIVATE",
                    ST_name(st_tmp),
                    "Reshaped arrays arguments cannot appear in LOCAL/LASTLOCAL/FIRSTPRIVATE list\n");
		}
		WN* new_ldid_lda=orig_ldid_lda;
                INT kid;
                for (kid=0; kid<WN_kid_count(parent); kid++)
                  if (WN_kid(parent,kid)==orig_ldid_lda)
                    break;
                if (WN_operator(orig_ldid_lda) == OPR_LDA)
                  WN_st_idx(orig_ldid_lda) = ST_st_idx(st_new);
                else {
                  WN* lda =
                    WN_CreateLda (OPCODE_make_op(OPR_LDA,Pointer_type,MTYPE_V),
                                  0,
                                  Make_Pointer_Type(ST_type(st_new)),
                                  st_new);
                  new_ldid_lda=lda;
                  LWN_Delete_Tree(WN_kid(parent,kid));
                  (*refs)[k] = NULL;
                  WN_kid(parent,kid) = lda;
                  LWN_Set_Parent(lda,parent);
                }

                if (iload_istore)
                  if (local_alias_host)
                    Copy_alias_info(Alias_Mgr,local_alias_host,iload_istore);
                  else {
                    local_alias_host=iload_istore;
		    Create_lda_array_alias(
			Alias_Mgr,new_ldid_lda,local_alias_host);
                  }
              }
            }

            WN_st_idx(local_pragma_stack[j].pragma)=ST_st_idx(st_new);

          }
      }
    }

    for (i=0; i<local_pragma_stack.Elements(); i++) {
      if (local_pragma_stack[i].localized==FALSE) {
        WN* wn=local_pragma_stack[i].pragma;
	LWN_Delete_Tree_From_Block(wn);
      }
    }

    refs->Free_array();
    local_pragma_stack.Free_array();
    MEM_POOL_Pop(&LNO_local_pool);
    // now walk the body of the MP region for nested MP-regions.
    Lego_Fix_Local_Rec (WN_region_body(wn),
			is_parallel_region || in_a_parallel_region);

  } else {
  
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
  
      Lego_Fix_Local_Rec(WN_kid(wn,kidno),in_a_parallel_region);
    }
  }
}

// Create a local lda array to replace a reshaped array inside an MP region
static ST *Create_Local_Lda_Array(DISTR_INFO *di, WN* mp_region)
{
  
  SYMBOL symb;
  static INT number;
  TY_IDX element_type;

  ST* array_st=di->Array_ST();
  TY_IDX array_ty = ST_type(array_st);
  INT i;
  
  if (ST_sclass(array_st)==SCLASS_FORMAL) {

    Is_True (TY_kind(array_ty) == KIND_POINTER,
           ("Create_Local_Lda_Array: expected an POINTER type for formal"));
    array_ty = TY_pointed(array_ty);
  } else if (ST_Var_Kind(array_st)==var_local &&
	     TY_kind(array_ty) == KIND_POINTER)
    array_ty = TY_pointed(array_ty);

  Is_True (TY_kind(array_ty) == KIND_ARRAY,
           ("Create_Local_Lda_Array: expected an ARRAY type"));

  // check that it is a fixed-size array
  for (i=0; i<TY_AR_ndims(array_ty); i++) {
    if (!TY_AR_const_lbnd(array_ty, i) ||
        !TY_AR_const_ubnd(array_ty, i)) {
      // not fixed size. Barf
      ErrMsgSrcpos(EC_DRA_unsupported_type,
                   WN_Get_Linenum(mp_region),
                   "LOCAL/LASTLOCAL/FIRSTPRIVATE",
                   ST_name(array_st),
                   "Assumed-size reshaped arrays cannot appear in LOCAL/LASTLOCAL/FIRSTPRIVATE list\n");
    }
  }

  ST* st_new = New_ST(CURRENT_SYMTAB);
  ST_Init (st_new,
           Save_Str("$tmp_local"),
           CLASS_VAR,
           SCLASS_AUTO,
           EXPORT_LOCAL,
           array_ty);
  Set_ST_is_temp_var(st_new);
  Set_ST_pt_to_unique_mem(st_new);
  Set_ST_pt_to_compiler_generated_mem(st_new);
  return st_new;

}

// Create a local alloc'd array to replace a reshaped array inside an MP region
static ST *Create_Tmp_Array(DISTR_INFO *di, WN *mp_region, WN **tmp_array_def)
{
  
  SYMBOL symb;
  static INT number;
  TY_IDX element_type;

  ST* array_st=di->Array_ST();
  
  if (TY_kind(ST_type(array_st)) == KIND_POINTER) {
  
    element_type = TY_AR_etype(TY_pointed(ST_type(array_st)));
  } else {
  
    Is_True(TY_kind(ST_type(array_st)) == KIND_ARRAY,
            ("Non-array,non-pointer in Create_Tmp_Array"));
    element_type = TY_AR_etype(ST_type(array_st));
  }
  TYPE_ID machine_element_type = TY_mtype(element_type);

  // need to compute size

  WN *bsz = LWN_Make_Icon(Pointer_type,TY_size(element_type));
  INT num_dim = di->Num_Dim();
  for (INT i=0; i<num_dim; i++)
    bsz = LWN_CreateExp2(OPCODE_make_op(OPR_MPY,Pointer_type,MTYPE_V),
                         bsz,LWN_Copy_Tree(di->Get_Array_Dim_Size(i)));

  SE_Symbols_For_SE(&symb,"tmp_local",number,machine_element_type);
  *tmp_array_def =
    Get_Expansion_Space(symb,bsz,"tmp_local",number++,machine_element_type,
                        mp_region,mp_region,mp_region);
  return symb.St();
}

// Add any LDID and LDA referring to a LOCAL, LASTLOCAL, or FIRSTPRIVATE
// reshaped array element to the set of "refs".
static void
Get_Reshaped_Array_Refs(WN* wn, DYNARRAY_OF_ARRAY_REFERENCES *refs)
{
  
  OPCODE opcode = WN_opcode(wn);
  OPERATOR oper = OPCODE_operator(opcode);
  if ((oper == OPR_LDA) || (oper == OPR_LDID)) {
    if (ST_class(WN_st(wn)) == CLASS_VAR && ST_is_reshaped(WN_st(wn))) {
  
      refs->AddElement(wn);
    }
  } else {
    if (opcode == OPC_BLOCK) {
      WN *kid = WN_first (wn);
      while (kid) {
        Get_Reshaped_Array_Refs(kid,refs);
        kid = WN_next(kid);
      }
    } else {
      for (INT i=0; i<WN_kid_count(wn); i++) {
        Get_Reshaped_Array_Refs(WN_kid(wn,i),refs);
      }
    }
  }
}

static ST* Create_Local_Array_ST ( char* array_name, TY_IDX ty)
{
  char name[64];
  ST *st;

  sprintf (name, "_%s",
           ((strlen(array_name)<50) ? array_name : "LongName"));
  st = New_ST(CURRENT_SYMTAB);

  ST_Init (st,
           Save_Str(name),
           CLASS_VAR,
           SCLASS_AUTO,
           EXPORT_LOCAL,
           ty);
  Set_ST_is_temp_var(st);
  Set_ST_pt_to_unique_mem(st);
  Set_ST_pt_to_compiler_generated_mem(st);
  return st;
}

// Copy-out an array after an MP-region. Return the ISTORE
// if tmp_array_def is NULL, we assume the temp array is an lda array
// otherwise it's an dynamically alloc'd array
static WN*
Copy_Array(DISTR_INFO* di, ST *local_st, WN *mp_region, WN *tmp_array_def,
	   BOOL copy_out)
{
  ST *array_st = di->Array_ST();
  MEM_POOL_Push(&LNO_local_pool);
  DISTR_ARRAY* dact = Lookup_DACT (array_st);
  INT num_dim = di->Num_Dim();
  WN *alias_host = NULL;

  // Create a num_dim dimensional loop nest
  WN *insert_before;
  WN *insert_parent = LWN_Get_Parent(mp_region);
  WN *do_loop;
  if (copy_out)
    insert_before = WN_next(mp_region);
  else
    insert_before = mp_region;

  TYPE_ID index_type;
// >> WHIRL 0.30: Added MTYPE_A8
// TODO WHIRL 0.30: get rid of MTYPE_I8 and MTYPE_U8
  if ((Pointer_type == MTYPE_A8) || (Pointer_type == MTYPE_U8) ||
      (Pointer_type == MTYPE_I8)) {
// << WHIRL 0.30: Added MTYPE_A8
    index_type = MTYPE_I8;
  } else {
    index_type = MTYPE_I4;
  }

#ifdef _NEW_SYMTAB
  TY_IDX element_type;
  ARB_HANDLE arb;
  if (TY_kind(ST_type(array_st)) == KIND_POINTER) {
    arb = TY_arb(TY_pointed(ST_type(array_st)));
    element_type = TY_AR_etype(TY_pointed(ST_type(array_st)));
  } else {
    arb = TY_arb(ST_type(array_st));
    element_type = TY_AR_etype(ST_type(array_st));
  }
  TYPE_ID machine_element_type = TY_mtype(element_type);
#else
  ARI *ari;
  TY_IDX element_type;
  if (TY_kind(ST_type(array_st)) == KIND_POINTER) {
    ari = TY_arinfo(TY_pointed(ST_type(array_st)));
    element_type = TY_AR_etype(TY_pointed(ST_type(array_st)));
  } else { // TY_kind(ST_type(array_st)) == KIND_ARRAY
    ari = TY_arinfo(ST_type(array_st));
    element_type = TY_AR_etype(ST_type(array_st));
  }
  TYPE_ID machine_element_type = TY_btype(element_type);
#endif

  char name[20];
  WN **loop_starts = CXX_NEW_ARRAY(WN *,num_dim,&LNO_local_pool);
  WN **loop_steps = CXX_NEW_ARRAY(WN *,num_dim,&LNO_local_pool);
  WN **loops = CXX_NEW_ARRAY(WN *,num_dim,&LNO_local_pool);
  INT i;
  for (i=0; i<num_dim; i++) {
    sprintf(name,"copy_%d",i);
    WN_OFFSET index_var_num;
    ST *index_var_st;
#ifdef _NEW_SYMTAB
    index_var_num = Create_Preg(index_type,name);
#else
    index_var_num = Create_Preg(index_type,name,NULL);
#endif
    index_var_st = MTYPE_To_PREG(index_type);
    WN *index = WN_CreateIdname(index_var_num,index_var_st);
#ifndef _NEW_SYMTAB
    ARB arb=ARI_bnd(ari,i);
#endif

    WN *start;
/* These lines don't do anything, why are they here DEM?
    if (ARB_const_lbnd(arb))
      start=LWN_Make_Icon(index_type,ARB_lbnd_val(arb));
    else
      start=LWN_Copy_Tree((WN*)ARB_lbnd_tree(arb));
*/

    start=LWN_Make_Icon(index_type,0);  
    start = LWN_CreateStid(OPCODE_make_op(OPR_STID,MTYPE_V,index_type),
                           index_var_num, index_var_st,
			   Be_Type_Tbl(index_type), start);

    Create_alias(Alias_Mgr,start);
    LWN_Copy_Linenumber(mp_region,start);
    loop_starts[i] = start;

    WN *end_use = 
      LWN_CreateLdid(OPCODE_make_op(OPR_LDID,index_type,index_type),start);
    WN *num_iters;
    if (ARB_const_ubnd(arb))
      num_iters=LWN_Make_Icon(index_type,ARB_ubnd_val(arb));
    else
      //num_iters=LWN_Copy_Tree((WN*)ARB_ubnd_tree(arb));
      num_iters=di->Get_Array_Dim_Size(i);
    num_iters=di->Get_Array_Dim_Size(i);

    WN *end = LWN_CreateExp2
      (OPCODE_make_op(OPR_LT,Boolean_type,index_type),end_use, num_iters);

    WN *step_use = 
      LWN_CreateLdid(OPCODE_make_op(OPR_LDID,index_type,index_type),start);
    WN* stride;
    if (Fixed_Size_Array_Is_Stride_One(array_st))
      stride=LWN_Make_Icon(index_type,1);
    else {
      DevWarn("Non stride-one array");
      stride=LWN_Make_Icon(index_type,1);
    }


    WN *add = LWN_CreateExp2(OPCODE_make_op(OPR_ADD,index_type,MTYPE_V),
                             step_use, stride);
    WN *step = LWN_CreateStid(WN_opcode(start),start,add);
    LWN_Copy_Linenumber(mp_region,step);
    loop_steps[i] = start;
    do_loop = LWN_CreateDO(index,start,end,step,WN_CreateBlock());
    loops[i] = do_loop;
    LWN_Copy_Linenumber(mp_region,do_loop);
    LWN_Copy_Linenumber(mp_region,WN_do_body(do_loop));
    LWN_Insert_Block_Before(insert_parent,insert_before,do_loop);
    insert_before = NULL;
    insert_parent = WN_do_body(do_loop);

    Du_Mgr->Add_Def_Use(start,end_use);
    Du_Mgr->Add_Def_Use(step,end_use);
    Du_Mgr->Add_Def_Use(start,step_use);
    Du_Mgr->Add_Def_Use(step,step_use);
    DEF_LIST *deflist = Du_Mgr->Ud_Get_Def(end_use);
    deflist->Set_loop_stmt(do_loop);
    deflist = Du_Mgr->Ud_Get_Def(step_use);
    deflist->Set_loop_stmt(do_loop);
    
  }


  // now do the copy
  //dact->Dinfo()->Add_Array_Use_WN(array_base);

  OPCODE op_array = OPCODE_make_op(OPR_ARRAY,Pointer_type,MTYPE_V);
  WN *local_array=WN_Create(op_array,1+2*num_dim);
  WN_element_size(local_array) = TY_size(element_type);

  if (tmp_array_def) { // for the ldid temp array
    WN_array_base(local_array) = LWN_CreateLdid(
      OPCODE_make_op(OPR_LDID,Pointer_type,Pointer_type),tmp_array_def);
    Du_Mgr->Add_Def_Use(tmp_array_def,WN_array_base(local_array));
  } else { // for the lda temp array
    WN_array_base(local_array) = 
      WN_CreateLda (OPCODE_make_op(OPR_LDA,Pointer_type,MTYPE_V),
                  0,
                  Make_Pointer_Type(ST_type(local_st)),
                  local_st);
  }


  WN *old_array=WN_Create(op_array,1+2*num_dim);
  WN_element_size(old_array) = TY_size(element_type);
  WN_array_base(old_array)=di->Load_Distr_Array();

  for (i=0; i<num_dim; i++) {
  
    WN_array_index(local_array,i) =
      LWN_CreateLdid(OPCODE_make_op(OPR_LDID,index_type,index_type),
                     loop_starts[i]);
    WN_array_dim(local_array,i) = di->Get_Array_Dim_Size(i);

    Du_Mgr->Add_Def_Use(loop_starts[i],WN_array_index(local_array,i));
    Du_Mgr->Add_Def_Use(loop_steps[i],WN_array_index(local_array,i));
    DEF_LIST *deflist = Du_Mgr->Ud_Get_Def(WN_array_index(local_array,i));
    deflist->Set_loop_stmt(loops[i]);
  }

  for (i=0; i<num_dim; i++) {
  
    WN_array_index(old_array,i) =
      LWN_CreateLdid(OPCODE_make_op(OPR_LDID,index_type,index_type),
                     loop_starts[i]);
    WN_array_dim(old_array,i) = di->Get_Array_Dim_Size(i);

    Du_Mgr->Add_Def_Use(loop_starts[i],WN_array_index(old_array,i));
    Du_Mgr->Add_Def_Use(loop_steps[i],WN_array_index(old_array,i));
    DEF_LIST *deflist = Du_Mgr->Ud_Get_Def(WN_array_index(old_array,i));
    deflist->Set_loop_stmt(loops[i]);
  }
  for (i=0; i<WN_kid_count(old_array); i++) {
  
    LWN_Set_Parent(WN_kid(old_array,i),old_array);
  }
  for (i=0; i<WN_kid_count(local_array); i++) {
  
    LWN_Set_Parent(WN_kid(local_array,i),local_array);
  }

  WN *value, *store;
  if (copy_out) {
    value = LWN_CreateIload(
	OPCODE_make_op(OPR_ILOAD,machine_element_type,machine_element_type),
	0,element_type,Make_Pointer_Type(element_type),local_array);

    store = LWN_CreateIstore(
	OPCODE_make_op(OPR_ISTORE,MTYPE_V,machine_element_type),
	0,Make_Pointer_Type(element_type),value,
	old_array);

    if (tmp_array_def) // for the ldid temp array
      Create_unique_pointer_alias(
	Alias_Mgr,WN_st(WN_array_base(local_array)),NULL,value);
    else
      Create_lda_array_alias(Alias_Mgr,WN_array_base(local_array),value);

    WN* old_base=WN_array_base(old_array);
    if (ST_sclass(WN_st(old_base)) == SCLASS_FORMAL) {
      Create_formal_alias(Alias_Mgr,WN_st(old_base),old_base,store);
    } else if (WN_operator(old_base)==OPR_LDA) {
      Create_lda_array_alias(Alias_Mgr,old_base,store);
    } else {
      Create_unique_pointer_alias(Alias_Mgr,WN_st(old_base),old_base,store);
    }
  } else {
    value = LWN_CreateIload(
	OPCODE_make_op(OPR_ILOAD,machine_element_type,machine_element_type),
	0,element_type,Make_Pointer_Type(element_type),old_array);

    store = LWN_CreateIstore(
	OPCODE_make_op(OPR_ISTORE,MTYPE_V,machine_element_type),
	0,Make_Pointer_Type(element_type),value,
	local_array);


    if (tmp_array_def) // for the ldid temp array
      Create_unique_pointer_alias(
	Alias_Mgr,WN_st(WN_array_base(local_array)),NULL,store);
    else
      Create_lda_array_alias(Alias_Mgr,WN_array_base(local_array),store);

    WN* old_base=WN_array_base(old_array);
    if (ST_sclass(WN_st(old_base)) == SCLASS_FORMAL) {
      Create_formal_alias(Alias_Mgr,WN_st(old_base),old_base,value);
    } else if (WN_operator(old_base)==OPR_LDA) {
      Create_lda_array_alias(Alias_Mgr,old_base,value);
    } else {
      Create_unique_pointer_alias(Alias_Mgr,WN_st(old_base),old_base,value);
    }
  }

  LWN_Copy_Linenumber(mp_region,store);

  Fix_Up_Loop_Info(mp_region,loops,num_dim);

  LWN_Insert_Block_Before(WN_do_body(do_loop),NULL,store);
  CXX_DELETE_ARRAY(loop_starts,&LNO_local_pool);
  CXX_DELETE_ARRAY(loop_steps,&LNO_local_pool);
  CXX_DELETE_ARRAY(loops,&LNO_local_pool);
  MEM_POOL_Pop(&LNO_local_pool);
  return (store);
}

