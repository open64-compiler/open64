/*

  Copyright (C) 2009-2011 Advanced Micro Devices, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

#include <stdint.h>
#include <math.h>

#include "defs.h"
#include "wn.h"
#include "wn_util.h"
#include "symtab.h"
#include "cxx_memory.h"
#include "lnopt_main.h"
#include "config_opt.h"
#include "config_lno.h"
#include "tracing.h"
#include "array_copy.h"
#include "lnopt_main.h"
#include "ir_reader.h"
#include "targ_sim.h"
#include "opt_du.h"
#include "lwn_util.h"
#include "lnoutils.h"

BOOL Trace_SAC = FALSE;

// utility routine to extract loop stride from the WN_step node of a doloop
// returns NULL if the step function is not simply enough
static WN* Get_Loop_Stride(WN* loop)
{
  WN *add = WN_kid0(WN_step(loop));
  if(WN_operator(add) != OPR_ADD)
    return NULL;

 if(WN_operator(WN_kid0(add))==OPR_LDID &&
     SYMBOL(WN_kid0(add)) == SYMBOL(WN_index(loop)) &&
     Is_Loop_Invariant_Exp(WN_kid1(add),loop) &&
     WN_operator(WN_kid1(add)) != OPR_INTCONST)
     return WN_kid1(add); //i = i + inv

 if(WN_operator(WN_kid1(add))==OPR_LDID &&
     SYMBOL(WN_kid1(add)) == SYMBOL(WN_index(loop)) &&
     Is_Loop_Invariant_Exp(WN_kid0(add),loop) &&
     WN_operator(WN_kid0(add)) != OPR_INTCONST)
    return WN_kid0(add); //i = inv + i

  return NULL;
}

// clean up before exiting
void Delete_SAC_Info(SAC_INFO*& sac_info)
{
  if (!sac_info) return;

  CXX_DELETE_ARRAY(sac_info->fld_info, &LNO_local_pool);
  CXX_DELETE(sac_info, &LNO_local_pool);
  sac_info = NULL;
}

// Top level routine for optimization, called from Lnoptimizer
void Perform_Structure_Array_Copy_Opt(WN* func_node)
{
  // if disabled, don't do anything
  if (!OPT_Struct_Array_Copy) return;

  Trace_SAC = Get_Trace(TP_LNOPT2, TT_STRUCT_ARRAY_COPY /*0x08000000*/);

  int depth = 0;
  SAC_INFO* sac_info = NULL;
  Find_Struct_Array_Copy_Candidate(sac_info, func_node, FALSE, depth);
  if (!sac_info) return; // no candidates found

  sac_info->func_entry = func_node;
  // Check legality
  if (!Check_Candidate_Legality(func_node, sac_info))
  {
    Delete_SAC_Info(sac_info);
    return;
  }

  // It seems legal so far, continue
  Create_New_Struct_Type(sac_info);

  // Insert the copy code in the outer loop
  Insert_Array_Copy_Code(sac_info);
  if (!sac_info) return; // something bad happened

  // Replace all references in the inner loop with references to the new copy
  Traverse_WN_Tree_For_Struct_Copy_Opt(sac_info);

  // If there are writes to our struct outside the inner loop, sync
  // the copy with the original version
  Insert_Sync_Copy_Code(sac_info);

  // clean up
  Delete_SAC_Info(sac_info);

  return;
}

// this routine is recursive
void Find_Struct_Array_Copy_Candidate(SAC_INFO*& sac_info,
                                      WN* wn, 
                                      bool collect_field_info,
                                      int& depth)
{
  if (!wn) return;
  if (sac_info) return;  // only one instance of this optimization allowed

  OPCODE opcode = WN_opcode(wn);
  
  if (opcode == OPC_DO_LOOP)
  {
    DO_LOOP_INFO* loop_info = Get_Do_Loop_Info(wn);
    Is_True(loop_info, ("No loop info!"));

    depth++;

    TY_IDX cand_ty = ST_type(WN_st(WN_index(wn)));
    // we want a nested loop with a struct ptr index type.
    if (depth > 2 && 
        (TY_kind(cand_ty) == KIND_POINTER) &&
        (TY_kind(TY_pointed(cand_ty)) == KIND_STRUCT))
    {
      if (!sac_info)
      {
        sac_info = CXX_NEW(SAC_INFO, &LNO_local_pool);
        memset(sac_info, 0, sizeof(SAC_INFO));
      }
      if (Trace_SAC)
        fprintf(stderr, "FOUND CANDIDATE LOOP\n");
      sac_info->wn_loop = wn;
      sac_info->orig_ty = TY_pointed(ST_type(WN_st(WN_index(wn))));

      FLD_ITER fld_itr = Make_fld_iter(TY_fld(sac_info->orig_ty));
      do
      {
        sac_info->orig_num_fields++;
      } while (!FLD_last_field(fld_itr++));

      sac_info->fld_info = CXX_NEW_ARRAY(SAC_FLD_INFO, 
                                         sac_info->orig_num_fields+1, 
                                         &LNO_local_pool);
      memset(sac_info->fld_info, 0, ((sac_info->orig_num_fields+1) *
                                     sizeof(SAC_FLD_INFO)));
      // Collect set of fields accessed. 
      Collect_Loop_Field_Refs(WN_do_body(wn), sac_info);
      if (!sac_info) return;

      int total_access_size = 0;
      for (int i = 0; i < sac_info->orig_num_fields; i++)
      {
        int field_id = i+1;
        if (sac_info->fld_info[field_id].is_read == TRUE)
        {
          sac_info->new_num_fields++;
          UINT cur_field_id = 0;
          FLD_HANDLE orig_fld = FLD_get_to_field(sac_info->orig_ty, field_id,
                                                 cur_field_id);
          total_access_size += TY_size(FLD_type(orig_fld));
          if (Trace_SAC)
            fprintf(stderr, "Read struct field %d\n", field_id);
        }
      }
      if (Trace_SAC)
        fprintf(stderr, "Total size of accessed fields = %d\n",
                total_access_size);

      // if no fields are referenced
      // if num referenced fields is more than half of the total num fields
      // if size of ref'ed fields is more than half of the total struct size
      // don't perform this optimization
      if ((sac_info->new_num_fields == 0) ||
          (sac_info->new_num_fields > sac_info->orig_num_fields / 2) ||
          (total_access_size > TY_size(sac_info->orig_ty)))
      {
        Delete_SAC_Info(sac_info);
        if (Trace_SAC)
        {
          fprintf(stderr, "Disable SAC: Criteria not met\n");
        }
        return;
      }
      else
      {
        // we only allow one instance of this optimization
        return;
      }
    }
  } else if (opcode == OPC_WHILE_DO || opcode == OPC_DO_WHILE)
    depth++;
  
  if (opcode == OPC_BLOCK)
  {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
    {
      Find_Struct_Array_Copy_Candidate(sac_info, w, 
                                       collect_field_info, depth);
    }
  }
  else
  {
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++)
    {
      WN* kid = WN_kid(wn, kidno);
      Find_Struct_Array_Copy_Candidate(sac_info, kid, 
                                       collect_field_info, depth);
    }
  }
  if (opcode == OPC_DO_LOOP || opcode ==OPC_DO_WHILE || opcode == OPC_WHILE_DO)
    depth--;
}

bool Check_Candidate_Legality(WN* wn_tree, SAC_INFO* sac_info)
{
  bool is_legal = TRUE;
  Traverse_WN_Tree_For_SAC_Legality(wn_tree, sac_info, is_legal);
  return is_legal;
}

void Traverse_WN_Tree_For_SAC_Legality(WN* wn,
                                       SAC_INFO* sac_info, 
                                       bool& is_legal)
{
  if (!wn) return;

  if (WN_operator(wn) == OPR_CALL)
  {
    if (WN_st(wn) && ST_sclass(WN_st(wn)) == SCLASS_TEXT)
    {
      // We can avoid an IPA legality check phase if we have complete
      // inlining (ie. no function calls within the live range of the
      // copy array) so that we don't have to worry about any
      // un-sync'ed reads/writes to the copy array.  If there *is* a
      // function call here, maybe it is a recursive function that has
      // one or more inlined iterations.  If so, then we can assert
      // the safety of this optimization because we've already seen
      // the function body and know it doesn't do any reads or writes
      // that we haven't already accounted for
      if (!Routine_Is_Inlined_And_Safe(sac_info->func_entry, WN_st(wn),
                                       sac_info))
      {
        is_legal = FALSE;
        return;
      }
    }
  }  

  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK)
  {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
    {
      Traverse_WN_Tree_For_SAC_Legality(w, sac_info, is_legal);
    }
  }
  else
  {
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++)
    {
      WN* kid = WN_kid(wn, kidno);
      Traverse_WN_Tree_For_SAC_Legality(kid, sac_info, is_legal);
    }
  }
}

bool Routine_Is_Inlined_And_Safe(WN* tree, ST* callee_st, SAC_INFO* sac_info)
{
  bool is_inlined = FALSE;
  Check_For_Inlined_Routine_Safety(tree, callee_st, sac_info,
                                   is_inlined);
  return is_inlined;
}

void Check_For_Inlined_Routine_Safety(WN* wn, ST* callee_st, 
                                      SAC_INFO* sac_info,
                                      bool& is_inlined)
{
  // if the routine is inlined, we've already scanned its body for
  // safety.  So all we need to do here is make sure that it has been
  // inlined.
  if (!wn) return;
  if (is_inlined) return;
  
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_PRAGMA)
  {
    WN_PRAGMA_ID pragma = (WN_PRAGMA_ID) WN_pragma(wn);
    if (pragma == WN_PRAGMA_INLINE_BODY_START)
    {
      is_inlined = true;
      return;
    }
  }
  if (opcode == OPC_BLOCK)
  {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
    {
      Check_For_Inlined_Routine_Safety(w, callee_st, sac_info, 
                                       is_inlined);
    }
  }
  else
  {
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++)
    {
      WN* kid = WN_kid(wn, kidno);
      Check_For_Inlined_Routine_Safety(kid, callee_st, sac_info, 
                                       is_inlined);
    }
  }
  return;
}

// this routine is recursive
void Collect_Loop_Field_Refs(WN* wn, SAC_INFO*& sac_info)
{
  if (!sac_info) return;
  Check_WN_For_Field_Refs(wn, sac_info);

  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK)
  {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
    {
      Collect_Loop_Field_Refs(w, sac_info);
    }
  }
  else
  {
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++)
    {
      WN* kid = WN_kid(wn, kidno);
      Collect_Loop_Field_Refs(kid, sac_info);
    }
  }
}
    
void Check_WN_For_Field_Refs(WN* wn, SAC_INFO*& sac_info)
{
  if (!sac_info) return;

  switch (WN_operator(wn))
  {
    case OPR_ILOAD:
    {
      if ((WN_operator(WN_kid0(wn)) != OPR_ARRAY) ||
          (WN_field_id(wn) == 0))
        break;

      {
        WN* array_wn = WN_kid0(wn);
        
        if (TY_kind(WN_ty(WN_kid0(array_wn))) != KIND_POINTER ||
            TY_pointed(WN_ty(WN_kid0(array_wn))) != WN_ty(wn))
          break;
        
        int field_id = WN_field_id(wn);
        if (Trace_SAC)
        {
          fprintf(stderr, "Field ref:\n");
          dump_tree(wn);
        }
        sac_info->fld_info[field_id].is_read = TRUE;
      }
      break;
    }
    case OPR_ISTORE:
    {    
      // if there are writes to the struct in the original loop,
      // delete the sac_info and return
      if ((TY_kind(WN_ty(wn)) == KIND_POINTER) &&
          (TY_IDX_index(TY_pointed(WN_ty(wn)))  == 
           TY_IDX_index(sac_info->orig_ty)))
      {
        if (Trace_SAC)
          fprintf(stderr, "Disable SAC: Found write to struct type in loop\n");
        Delete_SAC_Info(sac_info);
      }
      break;
    }
    default:
      break;
  }
  return;
}

// create the new struct type with only the fields that are accessed
void Create_New_Struct_Type(SAC_INFO* sac_info)
{
  if (sac_info->new_num_fields == 0)
    return;

  bool create_new_ty = TRUE;
  int offset = 0;
  TY_IDX new_ty = TY_IDX_ZERO;
  FLD_HANDLE last_fld = FLD_HANDLE();
  UINT new_field_id = 0;

  for (int i = 0; i < sac_info->orig_num_fields; i++)
  {
    if (sac_info->fld_info[i+1].is_read != TRUE)
      continue;
    
    new_field_id++;

    UINT cur_field_id = 0;
    FLD_HANDLE orig = FLD_get_to_field(sac_info->orig_ty, i+1, cur_field_id);
    
    // Create the first field
    FLD_HANDLE fld = New_FLD();
    memcpy(fld.Entry(), orig.Entry(), sizeof(FLD));
    
    if (create_new_ty)
    {
      // Create the new struct TY
      TY_Init(New_TY(new_ty), sizeof(int) /*dummy size*/,
              KIND_STRUCT, MTYPE_M, 
              Save_Str2i(TY_name(sac_info->orig_ty), "..", 12345));
      create_new_ty = FALSE;
      offset = 0;
      Set_TY_fld(new_ty, fld);
      Set_TY_align(new_ty, TY_align(sac_info->orig_ty));
    }
    else
    {
      int align = TY_align(FLD_type(fld));
      offset = (INT)ceil(((double)offset)/align) * align;
    }
    last_fld = fld;
    Set_FLD_ofst(fld, offset);
    sac_info->fld_info[i+1].new_fld_id = new_field_id;
    sac_info->fld_info[i+1].new_offset = offset;
    offset += TY_size(FLD_type(fld));
  }
  Set_FLD_last_field(last_fld);
  Set_TY_size(new_ty, offset);
  if (Trace_SAC)
    fprintf(stderr, "new ty size = %d\n", offset);
  sac_info->new_ty = new_ty;
}

void Insert_Array_Copy_Code(SAC_INFO*& sac_info)
{
  // Insert the new code somewhere after the start and stop nodes have
  // been defined.
  if (!Find_Insertion_Point(sac_info))
  {
    if (Trace_SAC)
      fprintf(stderr, "Could not find valid insertion point\n");
    Delete_SAC_Info(sac_info);
    return;
  }
  else
    Is_True(((sac_info->copy_insertion_block != NULL) &&
             (sac_info->copy_insertion_wn != NULL)),
            ("SAC insertion point is not valid!"));

  WN* cur_block = sac_info->copy_insertion_block;
  WN* cur_wn = sac_info->copy_insertion_wn;

  WN* orig_loop = sac_info->wn_loop;

  ST *old_sym=NULL, *new_sym=NULL;
  Copy_Bounds_Defs(WN_step(orig_loop), sac_info, cur_block, cur_wn,
                   old_sym, new_sym);
  if (!sac_info)
    return;

  WN* new_block = WN_CreateBlock();
  Setup_Common_Info(sac_info, new_block);
  if (!sac_info)
    return;

  // Create and Allocate Structure Copy Array
  Allocate_Struct_Copy_Array(sac_info, new_block, NULL);

  Create_Copy_Loop_Code(sac_info, new_block);

  WN_INSERT_BlockBefore(cur_block, cur_wn, new_block);

  // Free the copy array at the end of the insertion block
  Free_Struct_Copy_Array(sac_info, cur_block);

  // Parentize after insertion of copy loop
  LWN_Parentize(cur_block);
}

BOOL Find_Insertion_Point(SAC_INFO* sac_info)
{
  WN* start_def_block = NULL;
  WN* start_def_node = NULL;

  WN* start = WN_start(sac_info->wn_loop);
  Find_Def_Block(start, sac_info, start_def_block, start_def_node);
  if (!start_def_block || !start_def_node)
    return FALSE;

  WN* end_def_block = NULL;
  WN* end_def_node = NULL;

  WN* end = WN_end(sac_info->wn_loop);
  Find_Def_Block(end, sac_info, end_def_block, end_def_node);
  if (!end_def_block || !end_def_node)
    return FALSE;
  
  if (start_def_block != end_def_block)
    return FALSE;

  BOOL start_def_found = FALSE;
  BOOL end_def_found = FALSE;
  for (WN* w = WN_first(start_def_block); w; w = WN_next(w))
  {
    if (w == start_def_node)
      start_def_found = TRUE;
    if (w == end_def_node)
      end_def_found = TRUE;
    
    // place copy block after start and end defs have been seen, but
    // before any control flow
    if (start_def_found && end_def_found &&
        (WN_opcode(w) == OPC_WHILE_DO ||
         WN_opcode(w) == OPC_IF ||
         WN_opcode(w) == OPC_DO_WHILE ||
         WN_opcode(w) == OPC_DO_LOOP))
    {
      sac_info->copy_insertion_block = start_def_block;
      sac_info->copy_insertion_wn = w;
      return TRUE;
    }
  }

  return FALSE;
}

void Find_Def_Block(WN* expr, SAC_INFO* sac_info, 
                    WN*& def_block, WN*& def_node)
{
  if (!expr) return;

  OPCODE opcode = WN_opcode(expr);
  if ((WN_operator(expr) == OPR_LDID) &&
      (SYMBOL(expr) != SYMBOL(WN_index(sac_info->wn_loop))) &&
      (TY_kind(WN_ty(expr)) == KIND_POINTER) &&
      (TY_pointed(WN_ty(expr)) == sac_info->orig_ty))
  {
    DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(expr);
    if (!def_list->Incomplete())
    {
      DEF_LIST_ITER iter(def_list);
      for (const DU_NODE* node = iter.First(); 
           !iter.Is_Empty(); 
           node=iter.Next())
      {
        // we expect to only find one
        WN* def = (WN*) node->Wn();
        def_node = def;

        WN* parent_wn = LWN_Get_Parent(def);
        while (parent_wn && WN_opcode(parent_wn) != OPC_BLOCK)
        {
          parent_wn = LWN_Get_Parent(parent_wn);
        }
        if (parent_wn && WN_opcode(parent_wn) == OPC_BLOCK)
          def_block = parent_wn;
        return;
      }
    }
  }
  for (INT kidno = 0; kidno < WN_kid_count(expr); kidno++)
  {
    WN* kid = WN_kid(expr, kidno);
    Find_Def_Block(kid, sac_info, def_block, def_node);
  }
  
}

void Copy_Bounds_Defs(WN* expr,
                      SAC_INFO*& sac_info, WN* insertion_block, 
                      WN* insertion_wn, ST* old_sym, ST* new_sym)
{
  // for any vars used in the array bounds, make sure their initial
  // definitions are available before we insert the copy loop
  if (!expr) return;
  if (!sac_info) return;

  OPCODE opcode = WN_opcode(expr);

  if (WN_operator(expr) == OPR_LDID)
  {
    DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(expr);
    DEF_LIST_ITER iter(def_list);
    if (def_list->Incomplete())
    {
      WN* def = Find_Definition(WN_st(expr), 
                                insertion_block,
                                sac_info->wn_loop);
      if (!def)
      {
        Delete_SAC_Info(sac_info);
        return;
      }
      // Create a new symbol
      char new_sym_name[128];
      sprintf(new_sym_name, "%s_copy", ST_name(WN_st(expr)));
      ST* st = MTYPE_To_PREG(WN_ty(expr)>>8);
      // ST* st = MTYPE_To_PREG(MTYPE_U4);
      WN_OFFSET preg = Create_Preg(WN_ty(expr), new_sym_name);
      OPCODE op_stid = OPCODE_make_op(OPR_STID, MTYPE_V, WN_ty(expr));
      WN* def_copy = LWN_CreateStid(op_stid, preg, st,
                                     ST_type(st),
                                     WN_COPY_Tree(WN_kid0(def)));
      
      Do_DU_Update(def_copy);
      WN_INSERT_BlockBefore(insertion_block, insertion_wn, def_copy);
      sac_info->old_stride_sym = WN_st(expr);
      sac_info->new_stride_sym = st;
      sac_info->new_stride_preg = preg;
      return;
    }
  }
  else if (opcode == OPC_BLOCK)
  {
    for (WN* w = WN_first(expr); w; w = WN_next(w))
    {
      Copy_Bounds_Defs(w, sac_info, insertion_block, insertion_wn,
                        old_sym, new_sym);
    }
  }
  else
  {
    for (INT kidno = 0; kidno < WN_kid_count(expr); kidno++)
    {
      WN* kid = WN_kid(expr, kidno);
      Copy_Bounds_Defs(kid, sac_info, insertion_block, insertion_wn,
                       old_sym, new_sym);
    }
  }
}

WN* Find_Definition(ST* sym, WN* block, WN* stop)
{
  WN* def_node = NULL;
  LWN_ITER* itr = LWN_WALK_TreeIter(block);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (wn == stop)
      return def_node;
    else if (OPCODE_is_store(WN_opcode(wn)) &&
             (WN_operator(wn) == OPR_STID) && 
             (WN_st(wn) == sym))
    {
      def_node = wn;
      return def_node;
    }
  }
  return def_node;
}

// Create a new variable for the new struct array, and allocate it to
// the appropriate size as calculated
// This function generates whirl for:
// copy_struct_sac = calloc(chunk_count * stride, sizeof(new type))
void Allocate_Struct_Copy_Array(SAC_INFO* sac_info, WN* insertion_block,
                                WN* insertion_wn)
{
  char new_struct_name[64];
  sprintf(new_struct_name, "%s_sac", TY_name(sac_info->new_ty));

  ST* st = New_ST(CURRENT_SYMTAB);
  ST_Init(st, Save_Str(new_struct_name), CLASS_VAR, 
          SCLASS_AUTO, EXPORT_LOCAL, 
          Make_Pointer_Type(sac_info->new_ty));
  Set_ST_pt_to_unique_mem(st);
  Set_ST_is_temp_var(st);
  
  // calloc the new var to the appropriate size
  TY_IDX void_ptr_ty = Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)); // void *
  
  TY_IDX ty = Make_Function_Type(Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)));
  ST* calloc_st = Gen_Intrinsic_Function(ty, "calloc");
  WN* calloc_call = WN_Call(Pointer_Mtype, MTYPE_V, 2, calloc_st);
  
  // parm0 = num_chunks * struct stride
  TYPE_ID index_type = MTYPE_U4;
  OPCODE op_mpy = OPCODE_make_op(OPR_MPY, index_type, MTYPE_V);
  OPCODE op_ldid = OPCODE_make_op(OPR_LDID, index_type, index_type);
  WN* parm0 = WN_CreateExp2(op_mpy, 
                            LWN_CreateLdid(op_ldid, 
                                           sac_info->saved_num_chunks_wn),
                            LWN_CreateLdid(op_ldid,
                                           sac_info->saved_struct_stride));
  WN *arg0 = WN_CreateParm(MTYPE_U4, parm0, 
                           Be_Type_Tbl(MTYPE_U4), WN_PARM_BY_VALUE);
  WN* parm1 = WN_Intconst(MTYPE_U4, TY_size(sac_info->new_ty));
  WN* arg1 = WN_CreateParm(MTYPE_U4, parm1, 
                           Be_Type_Tbl(MTYPE_U4), WN_PARM_BY_VALUE);
  WN_kid0(calloc_call) = arg0;
  WN_kid1(calloc_call) = arg1;

  // save the return addr of the calloc call into the new variable
  RETURN_INFO return_info = Get_Return_Info(MTYPE_To_TY(Pointer_Mtype),
                                            Complex_Not_Simulated);
  PREG_NUM reg_ret = RETURN_INFO_preg(return_info, 0);
  WN* ret_ldid = WN_Ldid(Pointer_Mtype, reg_ret, Return_Val_Preg, 
                         Be_Type_Tbl(Pointer_Mtype));
  WN* wn_stid = WN_Stid(Pointer_Mtype,
                        0, st, ST_type(st), ret_ldid);
  sac_info->array_copy_wn = wn_stid;

  WN_INSERT_BlockBefore(insertion_block, insertion_wn, calloc_call);
  WN_INSERT_BlockBefore(insertion_block, insertion_wn, wn_stid);

  Do_DU_Update(calloc_call);
  Do_DU_Update(wn_stid);
}

// create a call to free() to delete the copy array when we're done
void Free_Struct_Copy_Array(SAC_INFO* sac_info, WN* insertion_block)
{
  TYPE_ID index_type = MTYPE_U4;
  OPCODE op_ldid = OPCODE_make_op(OPR_LDID, index_type, index_type);

  TY_IDX ty = Make_Function_Type(MTYPE_To_TY(MTYPE_V));
  ST* st = Gen_Intrinsic_Function(ty, "free");
  WN* call = WN_Call(MTYPE_V, MTYPE_V, 1, st);
  WN* parm0 = WN_CreateParm(Pointer_Mtype, 
                            LWN_CreateLdid(op_ldid, sac_info->array_copy_wn),
                            Be_Type_Tbl(Pointer_Mtype),
                            WN_PARM_BY_VALUE);
  
  WN_kid0(call) = parm0;
  WN_INSERT_BlockBefore(insertion_block, NULL, call);
  Do_DU_Update(call);
}

// Calculate and save a bunch of commonly used wns:
// Saves:
//   saved_start_val
//   saved_end_val
//   saved

void Setup_Common_Info(SAC_INFO*& sac_info, WN* copy_block)
{
  TYPE_ID array_type = MTYPE_I8;
  TYPE_ID index_type = MTYPE_U4;
  
  OPCODE op_stid = OPCODE_make_op(OPR_STID, MTYPE_V, index_type);
  OPCODE op_ldid = OPCODE_make_op(OPR_LDID, index_type, index_type);
  OPCODE op_add = OPCODE_make_op(OPR_ADD, index_type, MTYPE_V);
  OPCODE op_sub = OPCODE_make_op(OPR_SUB, index_type, MTYPE_V);
  OPCODE op_mpy = OPCODE_make_op(OPR_MPY, index_type, MTYPE_V);
  OPCODE op_div = OPCODE_make_op(OPR_DIV, index_type, MTYPE_V);
  OPCODE ldaop = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
  ST* copy_array_type_preg_st = MTYPE_To_PREG(array_type);
  ST* index_type_preg_st = MTYPE_To_PREG(index_type);

  WN* tmp_ldid;

  // Save a bunch of frequently used values/pointers
  WN* orig_start = WN_start(sac_info->wn_loop);

  ST* orig_start_st = WN_st(orig_start);
  Is_True((TY_kind(ST_type(orig_start_st)) == KIND_POINTER), 
          ("lcv type kind != KIND_POINTER"));

  // if the start value contains a (positive) offset, strip it.
  WN* start_val = WN_kid0(orig_start);
  if (WN_operator(start_val) == OPR_ADD)
  {
    if (WN_operator(WN_kid0(start_val)) == OPR_LDID)
      start_val = WN_kid0(start_val);
    else if (WN_operator(WN_kid1(start_val)) == OPR_LDID)
      start_val = WN_kid1(start_val);
  }
  TYPE_ID outer_lcv_type = WN_desc(orig_start);
  ST* outer_loop_st = MTYPE_To_PREG(outer_lcv_type);
  //ST* outer_loop_st = MTYPE_To_PREG(index_type);
  WN_OFFSET saved_start_preg = Create_Preg(outer_lcv_type, "saved_start_val");
  WN* saved_start_stid = LWN_CreateStid(op_stid, saved_start_preg,
                                        outer_loop_st, 
                                        Be_Type_Tbl(outer_lcv_type),
                                        WN_COPY_Tree(start_val));
  WN_INSERT_BlockBefore(copy_block, NULL, saved_start_stid);

  WN_OFFSET saved_end_preg = Create_Preg(outer_lcv_type, "saved_end_val");
  WN* saved_end_stid;
  bool end_is_kid0 = TRUE;
  if (ST_st_idx(WN_st(WN_kid0(WN_end(sac_info->wn_loop)))) != 
      ST_st_idx(WN_st(orig_start)))
  {
    saved_end_stid = 
      LWN_CreateStid(op_stid, saved_end_preg,
                     outer_loop_st,
                     Be_Type_Tbl(outer_lcv_type),
                     WN_COPY_Tree(WN_kid0(WN_end(sac_info->wn_loop))));
  }
  else
  {
    end_is_kid0 = FALSE;
    saved_end_stid =
      LWN_CreateStid(op_stid, saved_end_preg,
                     outer_loop_st,
                     Be_Type_Tbl(outer_lcv_type),
                     WN_COPY_Tree(WN_kid1(WN_end(sac_info->wn_loop))));
  }
  WN_INSERT_BlockBefore(copy_block, NULL, saved_end_stid);
                                      
    
  WN_OFFSET struct_stride_preg = 
    Create_Preg(index_type, "saved_struct_stride");
  WN_OFFSET int_stride_preg = Create_Preg(index_type, "saved_int_stride");
  WN* loop_stride = Get_Loop_Stride(sac_info->wn_loop);
  if (!loop_stride)
  {
    Delete_SAC_Info(sac_info);
    return;
  }

  WN* stride_wn = WN_COPY_Tree(loop_stride);
  if (sac_info->new_stride_sym != NULL)
  {
    Replace_Symbol(stride_wn,
                   SYMBOL(sac_info->old_stride_sym, 0, 
                          ST_type(sac_info->old_stride_sym)),
                   SYMBOL(sac_info->new_stride_sym, 
                          sac_info->new_stride_preg,
                          ST_type(sac_info->new_stride_sym)),
                   NULL, NULL);
    
  }
  WN* int_stride_stid = LWN_CreateStid(op_stid, int_stride_preg,
                                       index_type_preg_st,
                                       Be_Type_Tbl(index_type),
                                       stride_wn);
  WN_INSERT_BlockBefore(copy_block, NULL, int_stride_stid);
  // stride of orig loop is for struct* type.  divide by size of
  // struct since we just want a count for struct increment
  WN* div_op = 
    LWN_CreateExp2(op_div, WN_COPY_Tree(stride_wn),
                   LWN_Make_Icon(index_type, TY_size(sac_info->orig_ty)));
  WN* struct_stride_stid = LWN_CreateStid(op_stid, struct_stride_preg,
                                          index_type_preg_st,
                                          Be_Type_Tbl(index_type),
                                          div_op);
  WN_INSERT_BlockBefore(copy_block, NULL, struct_stride_stid);

  // save start and end computations in the sac_info since they'll be
  // used frequently
  sac_info->end_comp_op = WN_opcode(WN_end(sac_info->wn_loop));
  sac_info->end_is_kid0 = end_is_kid0;
  sac_info->saved_end_wn = saved_end_stid;
  sac_info->saved_start_wn = saved_start_stid;
  sac_info->saved_struct_stride = struct_stride_stid;
  sac_info->saved_int_stride = int_stride_stid;

  // now create the loop to count the number of chunks in the copy array
  // Create a simple copy of the original loop and count how many
  // chunks we'll be copying  
  ST* tmp_st = New_ST(CURRENT_SYMTAB);
  ST_Init(tmp_st, Save_Str("sac_tmp"), CLASS_VAR,
          SCLASS_AUTO, EXPORT_LOCAL,
          Make_Pointer_Type(sac_info->orig_ty));
  Set_ST_is_temp_var(tmp_st);
  WN* tmp_start = WN_Stid(Pointer_Mtype, 0, tmp_st,
                          ST_type(tmp_st),
                          LWN_CreateLdid(op_ldid, sac_info->saved_start_wn));

  WN* tmp_end;
  if (end_is_kid0)
  {
    tmp_end = LWN_CreateExp2(sac_info->end_comp_op,
                             LWN_CreateLdid(op_ldid, 
                                            sac_info->saved_end_wn),
                             LWN_CreateLdid(op_ldid, tmp_start));
  }
  else
  {
    tmp_end = LWN_CreateExp2(sac_info->end_comp_op,
                             LWN_CreateLdid(op_ldid, tmp_start),
                             LWN_CreateLdid(op_ldid, 
                                            sac_info->saved_end_wn));
  }

  WN* tmp_step = LWN_CreateStid(op_stid, tmp_start,
                                LWN_CreateExp2(op_add, 
                                               LWN_CreateLdid(op_ldid,
                                                              tmp_start),
                                               LWN_CreateLdid(op_ldid,
                                                              sac_info->
                                                              saved_int_stride)));  
  WN* tmp_idx = WN_CreateIdname(0, tmp_st);
  WN* chunk_count_loop = LWN_CreateDO(tmp_idx,
                                      tmp_start,
                                      tmp_end,
                                      tmp_step,
                                      WN_CreateBlock());

  WN_OFFSET tmp_preg = Create_Preg(index_type, "sac_chunk_counter");
  WN* init_cc = LWN_CreateStid(op_stid, tmp_preg,
                               index_type_preg_st,
                               Be_Type_Tbl(index_type),
                               LWN_Make_Icon(index_type, 0));
  WN_INSERT_BlockBefore(copy_block, NULL, init_cc);

  tmp_ldid = LWN_CreateLdid(op_ldid, init_cc);
  WN* incr_cc = LWN_CreateStid(op_stid, init_cc,
                               LWN_CreateExp2(op_add, tmp_ldid,
                                              LWN_Make_Icon(index_type, 1)));
  WN_INSERT_BlockBefore(WN_do_body(chunk_count_loop), NULL, incr_cc);
  WN_INSERT_BlockBefore(copy_block, NULL, chunk_count_loop);
  sac_info->saved_num_chunks_wn = init_cc;

  DO_LOOP_INFO*  dli = (DO_LOOP_INFO *)
    CXX_NEW(DO_LOOP_INFO(&LNO_default_pool,NULL,NULL,NULL,
                         FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE),
            &LNO_default_pool);
  dli->Depth = 0;
  WN_MAP_Set(LNO_Info_Map, chunk_count_loop, (void*)dli);
  DOLOOP_STACK *loop_stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                               &LNO_local_pool);

  Build_Doloop_Stack(LWN_Get_Parent(chunk_count_loop), loop_stack);

  LNO_Build_Access(chunk_count_loop, loop_stack, &LNO_default_pool);
  LNO_Build_Do_Access(chunk_count_loop, loop_stack);
}

// Create the actual loop that copies the appropriate indices and
// fields out of the original struct array
WN* Create_Copy_Loop_Code(SAC_INFO* sac_info, WN* copy_block)
{
  TYPE_ID array_type = MTYPE_I8;
  TYPE_ID index_type = MTYPE_U4;
  
  OPCODE op_stid = OPCODE_make_op(OPR_STID, MTYPE_V, index_type);
  OPCODE op_ldid = OPCODE_make_op(OPR_LDID, index_type, index_type);
  OPCODE op_add = OPCODE_make_op(OPR_ADD, index_type, MTYPE_V);
  OPCODE op_sub = OPCODE_make_op(OPR_SUB, index_type, MTYPE_V);
  OPCODE op_mpy = OPCODE_make_op(OPR_MPY, index_type, MTYPE_V);
  OPCODE op_div = OPCODE_make_op(OPR_DIV, index_type, MTYPE_V);
  OPCODE ldaop = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
  ST* copy_array_type_preg_st = MTYPE_To_PREG(array_type);
  ST* index_type_preg_st = MTYPE_To_PREG(index_type);

  WN* orig_loop = sac_info->wn_loop;
  WN* tmp_ldid;


  ST* outer_trip_count_st = MTYPE_To_PREG(index_type);
  WN_OFFSET tc_preg = Create_Preg(index_type, "sac_outer_tc");
  WN* init_trip_count = LWN_CreateStid(op_stid, tc_preg,
                                       index_type_preg_st,
                                       Be_Type_Tbl(index_type),
                                       LWN_Make_Icon(index_type, 0));
  WN_INSERT_BlockAfter(copy_block, NULL, init_trip_count);

  // Now create outer WHILE_DO
  ST* tmp_arc_st = New_ST(CURRENT_SYMTAB);
  ST_Init(tmp_arc_st, Save_Str("sac_tmp_arc_ptr"), CLASS_VAR,
          SCLASS_AUTO, EXPORT_LOCAL,
          Make_Pointer_Type(sac_info->orig_ty));
  Set_ST_is_temp_var(tmp_arc_st);
  WN* init_stid = WN_Stid(Pointer_Mtype, 0, tmp_arc_st, 
                          ST_type(tmp_arc_st), 
                          LWN_CreateLdid(op_ldid, sac_info->saved_start_wn));
  WN_INSERT_BlockBefore(copy_block, NULL, init_stid);  


  WN* ldid = LWN_CreateLdid(op_ldid, init_stid);
  WN* test_wn;
  if (sac_info->end_is_kid0)
  {
    test_wn = 
      WN_CreateExp2(sac_info->end_comp_op,
                    LWN_CreateLdid(op_ldid, sac_info->saved_end_wn),
                    ldid);

  }
  else
  {
    test_wn = 
      WN_CreateExp2(sac_info->end_comp_op,
                    ldid,
                    LWN_CreateLdid(op_ldid, sac_info->saved_end_wn));
  }
                                 
  WN* while_do_block = WN_CreateBlock();
  WN* while_do = LWN_CreateWhileDo(test_wn, while_do_block);

  // Create the inner DO_LOOP
  WN_OFFSET preg_num2 = Create_Preg(index_type, "sac_inner_loop_idx");
  WN* loop_start2 = LWN_CreateStid(op_stid, preg_num2,
                                  index_type_preg_st,
                                  Be_Type_Tbl(index_type),
                                  LWN_Make_Icon(index_type, 0));
  
  WN* ldid3 = LWN_CreateLdid(op_ldid, loop_start2);
  WN* ldid4 = LWN_CreateLdid(op_ldid, sac_info->saved_struct_stride);
  WN* loop_end2 = 
    LWN_CreateExp2(OPCODE_make_op(OPR_LT, Boolean_type, index_type),
                   ldid3, ldid4);

  WN* ldid5 = LWN_CreateLdid(op_ldid, loop_start2);
  WN* loop_step2 = LWN_CreateStid(op_stid, loop_start2,
                                  LWN_CreateExp2(op_add, ldid5,
                                                LWN_Make_Icon(index_type, 1)));

  WN* loop_index2 = WN_CreateIdname(preg_num2, index_type_preg_st);
  WN* inner_loop_block = WN_CreateBlock();
  WN* inner_loop = LWN_CreateDO(loop_index2,
                                loop_start2,
                                loop_end2,
                                loop_step2,
                                inner_loop_block);

  WN_INSERT_BlockBefore(while_do_block, NULL, inner_loop);

  // create istore for each referenced field
  WN* tmp_ldid2;
  int new_struct_size = TY_size(sac_info->new_ty);
  for (int i = 0; i < sac_info->orig_num_fields; i++)
  {
    if (sac_info->fld_info[i+1].is_read != TRUE)
      continue;

    UINT cur_field_id = 0;
    FLD_HANDLE orig_fld = FLD_get_to_field(sac_info->orig_ty, 
                                           i+1, cur_field_id);

    TY_IDX field_ty = FLD_type(orig_fld);
    TYPE_ID field_mtype;
    if (TY_kind(field_ty) == KIND_POINTER)
      field_mtype = Pointer_Mtype;
    else
      field_mtype = field_ty >> 8;

    OPCODE iloadop = OPCODE_make_op(OPR_ILOAD, field_mtype, field_mtype);
    OPCODE istoreop = OPCODE_make_op(OPR_ISTORE, MTYPE_V, field_mtype);
    
    // otherwise, copy this field 
    tmp_ldid = LWN_CreateLdid(op_ldid, init_stid);
    WN* iload_wn = LWN_CreateIload(iloadop, FLD_ofst(orig_fld), 
                                   sac_info->orig_ty,
                                   Make_Pointer_Type(sac_info->orig_ty), 
                                   tmp_ldid, i+1);

    // new index = (inner_loop_idx * num_chunks + outer_loop_idx)
    tmp_ldid = LWN_CreateLdid(op_ldid, WN_start(inner_loop));
    tmp_ldid2 = LWN_CreateLdid(op_ldid, 
                               sac_info->saved_num_chunks_wn);

    WN* row_offset = LWN_CreateExp2(op_mpy, tmp_ldid, tmp_ldid2);
    WN* col_offset = LWN_CreateLdid(op_ldid, init_trip_count);
    WN* offset_idx = LWN_CreateExp2(op_add, row_offset, col_offset);
    WN* offset = LWN_CreateExp2(op_mpy, offset_idx, 
                                LWN_Make_Icon(index_type, new_struct_size));
    
    WN* base = LWN_CreateLdid(op_ldid, sac_info->array_copy_wn);
    WN* base_plus_offset = LWN_CreateExp2(op_add, base, offset);

    UINT new_field_id = sac_info->fld_info[i+1].new_fld_id;
    int new_field_offset = sac_info->fld_info[i+1].new_offset;
    WN* istore_wn = LWN_CreateIstore(istoreop, new_field_offset, 
                                     Make_Pointer_Type(sac_info->new_ty), 
                                     iload_wn,
                                     base_plus_offset, new_field_id);
    WN_INSERT_BlockBefore(inner_loop_block, NULL, istore_wn);
  }

  // increment outer lcv inside inner loop
  tmp_ldid = LWN_CreateLdid(op_ldid, init_stid);
  WN* incr_outer_lcv = 
    LWN_CreateStid(op_stid, init_stid,
                   LWN_CreateExp2(op_add, tmp_ldid,
                                  LWN_Make_Icon(index_type,
                                                TY_size(sac_info->orig_ty))));
  WN_INSERT_BlockBefore(inner_loop_block, NULL, incr_outer_lcv);
  
  // increment outer loop trip count
  WN* ldid6 = LWN_CreateLdid(op_ldid, init_trip_count);
  WN* incr_tc = LWN_CreateStid(op_stid, init_trip_count,
                               LWN_CreateExp2(op_add, ldid6,
                                              LWN_Make_Icon(index_type, 1)));
  WN_INSERT_BlockBefore(while_do_block, NULL, incr_tc);
  
  WN_INSERT_BlockAfter(copy_block, init_stid, while_do);

  if (Trace_SAC)
  {
    fprintf(stderr, "_______________________\n");
    fdump_tree(stderr, copy_block);
    fprintf(stderr, "_______________________\n");
  }

  LWN_Parentize(copy_block);


  DO_LOOP_INFO*  dli2 = (DO_LOOP_INFO *)
    CXX_NEW(DO_LOOP_INFO(&LNO_default_pool,NULL,NULL,NULL,
                         FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE) ,
            &LNO_default_pool);
  dli2->Depth = 0;
  WN_MAP_Set(LNO_Info_Map, inner_loop, (void*)dli2);
  DOLOOP_STACK *loop_stack2=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                               &LNO_local_pool);

  Build_Doloop_Stack(LWN_Get_Parent(inner_loop), loop_stack2);

  LNO_Build_Access(inner_loop, loop_stack2, &LNO_default_pool);
  LNO_Build_Do_Access(inner_loop, loop_stack2);

  Do_DU_Update(copy_block);

  return copy_block;
}

void Do_DU_Update(WN* wn)
{
  // TODO
  // Set all DU info for new these updates to incomplete for now This
  // should obviously be fixed eventually, but there does not seem to
  // be any performance impact by preventing further optimizations of
  // this code in future phases for now.
  OPCODE opcode = WN_opcode(wn);
  if (OPCODE_is_store(opcode))
  {
    Du_Mgr->Create_Use_List(wn);
    USE_LIST* use_list = Du_Mgr->Du_Get_Use(wn);
    use_list->Set_Incomplete();
  }
  else if (OPCODE_is_load(opcode))
  {
    Du_Mgr->Create_Def_List(wn);
    DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn);
    def_list->Set_Incomplete();
  }
  else if (opcode == OPC_BLOCK)
  {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
    {
      Do_DU_Update(w);
    }
  }
  else
  {
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++)
    {
      WN* kid = WN_kid(wn, kidno);
      Do_DU_Update(kid);
    }
  }
}

void Traverse_WN_Tree_For_Struct_Copy_Opt(SAC_INFO* sac_info)
{
  // initial index = (loop_start_val - saved_start_val
  TYPE_ID index_type = MTYPE_U4;
  OPCODE op_ldid = OPCODE_make_op(OPR_LDID, index_type, index_type);
  OPCODE op_stid = OPCODE_make_op(OPR_STID, MTYPE_V, index_type);
  OPCODE op_sub = OPCODE_make_op(OPR_SUB, index_type, MTYPE_V);
  OPCODE op_add = OPCODE_make_op(OPR_ADD, index_type, MTYPE_V);
  OPCODE op_mpy = OPCODE_make_op(OPR_MPY, index_type, MTYPE_V);
  OPCODE op_div = OPCODE_make_op(OPR_DIV, index_type, MTYPE_V);
  // Duplicate the start node up out of the loop so we can use it to
  // compute the current index into our copy
  ST* curr_copy_idx_st = New_ST(CURRENT_SYMTAB);
  ST_Init(curr_copy_idx_st, Save_Str("curr_copy_idx_st"), CLASS_VAR,
          SCLASS_AUTO, EXPORT_LOCAL,
          MTYPE_U4 << 8);
  Set_ST_is_temp_var(curr_copy_idx_st);
  WN* init_start = WN_COPY_Tree(WN_start(sac_info->wn_loop));

  // precompute the index into the new array
  WN* diff = LWN_CreateExp2(op_div,
                            WN_CreateExp2(op_sub,
                                          LWN_CreateLdid(op_ldid,
                                                         init_start),
                                          LWN_CreateLdid(op_ldid,
                                                         sac_info->saved_start_wn)),
                            LWN_Make_Icon(MTYPE_U4, TY_size(sac_info->orig_ty)));
                            
  WN* current_copy_index = 
    LWN_CreateStid(op_stid, 0, 
                   curr_copy_idx_st,
                   Be_Type_Tbl(index_type),
                   WN_CreateExp2(op_mpy,
                                 diff,
                                 LWN_CreateLdid(op_ldid,
                                                sac_info->saved_num_chunks_wn)));
  WN_INSERT_BlockBefore(LWN_Get_Parent(sac_info->wn_loop),
                        sac_info->wn_loop, init_start);
  WN_INSERT_BlockBefore(LWN_Get_Parent(sac_info->wn_loop),
                        sac_info->wn_loop, current_copy_index);
  Do_DU_Update(init_start);
  Do_DU_Update(current_copy_index);

  // Walk the tree and do replacement
  Walk_And_Replace_Refs(WN_do_body(sac_info->wn_loop), sac_info,
                        current_copy_index, NULL, -1);
  // insert an increment of the index at the end of the loop
  WN* incr_idx = LWN_CreateStid(op_stid,
                                current_copy_index,
                                WN_CreateExp2(op_add, 
                                              LWN_CreateLdid(op_ldid,
                                                             current_copy_index),
                                              LWN_Make_Icon(index_type, 1)));
  WN_INSERT_BlockBefore(WN_do_body(sac_info->wn_loop), NULL, incr_idx);
  Do_DU_Update(incr_idx);

}

// replace any references to the original struct array with references
// to the copy.  ie:
// y = x->field   ==>  y = x_copy[index].field
void Walk_And_Replace_Refs(WN* wn, SAC_INFO* sac_info, WN* idx_expr,
                           WN* parent_wn, int kidno)
{
  //  if (!wn) return;

  OPCODE opcode = WN_opcode(wn);

  if (WN_operator(wn) == OPR_ILOAD)
  {
    // check if this is an ILOAD of one of our referenced fields
    if ((WN_operator(WN_kid0(wn)) == OPR_ARRAY) &&
        WN_field_id(wn) != 0)
    {
      WN* array_wn = WN_kid0(wn);
      if (TY_kind(WN_ty(WN_kid0(array_wn))) == KIND_POINTER &&
          TY_pointed(WN_ty(WN_kid0(array_wn))) == sac_info->orig_ty)
      {      
        int field_id = WN_field_id(wn);
        UINT cur_field_id = 0;
        FLD_HANDLE orig_fld = FLD_get_to_field(sac_info->orig_ty, 
                                               field_id, cur_field_id);
        TY_IDX field_ty = FLD_type(orig_fld);
        TYPE_ID field_mtype;
        if (TY_kind(field_ty) == KIND_POINTER)
          field_mtype = Pointer_Mtype;
        else
          field_mtype = field_ty >> 8;

        TYPE_ID index_type = MTYPE_U4;
        OPCODE op_iload = OPCODE_make_op(OPR_ILOAD, field_mtype, field_mtype);
        OPCODE op_add = OPCODE_make_op(OPR_ADD, index_type, MTYPE_V);
        OPCODE op_mpy = OPCODE_make_op(OPR_MPY, index_type, MTYPE_V);
        OPCODE op_ldid = OPCODE_make_op(OPR_LDID, index_type, index_type);

        if (Trace_SAC)
          fprintf(stderr, "Replacing ref to field id %d\n", field_id);
        int new_field_offset = sac_info->fld_info[field_id].new_offset;
        int new_field_id = sac_info->fld_info[field_id].new_fld_id;
        WN* ldid = LWN_CreateLdid(op_ldid, sac_info->array_copy_wn);
        WN* mpy_expr = 
          LWN_CreateExp2(op_mpy,
                         LWN_CreateLdid(op_ldid,
                                        idx_expr),
                         LWN_Make_Icon(index_type,
                                       TY_size(sac_info->new_ty)));
        WN* ldid_plus_offset = LWN_CreateExp2(op_add,
                                              ldid,
                                              mpy_expr);
        WN* new_iload = LWN_CreateIload(op_iload,
                                        new_field_offset,
                                        sac_info->new_ty,
                                        Make_Pointer_Type(sac_info->new_ty),
                                        ldid_plus_offset, 
                                        new_field_id);
        Is_True(parent_wn && (kidno != -1), ("no parent or kidno!")); 
        WN_kid(parent_wn, kidno) = new_iload;
        Do_DU_Update(parent_wn);
        if (Trace_SAC)
        {
          fprintf(stderr, "Old wn\n");
          dump_tree(wn);
          fprintf(stderr, "New ILOAD\n");
          dump_tree(new_iload);
        }
      }
    }
  }
  
  if (opcode == OPC_BLOCK)
  {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
    {
      Walk_And_Replace_Refs(w, sac_info, idx_expr, NULL, -1);
    }
  }
  else
  {
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++)
    {
      WN* kid = WN_kid(wn, kidno);
      Walk_And_Replace_Refs(kid, sac_info, idx_expr, wn, kidno);
    }
  }  
}

// synchronize the copy with any updates to the original struct in the
// outer loop(s)
void Insert_Sync_Copy_Code(SAC_INFO* sac_info)
{
  LWN_Parentize(sac_info->copy_insertion_block);
  // walk over WNs looking for ISTOREs to our struct type
  Find_Writes_To_Struct_Type(sac_info->copy_insertion_block, 
                             sac_info, FALSE);
  LWN_Parentize(sac_info->copy_insertion_block);
}

void Find_Writes_To_Struct_Type(WN* wn, SAC_INFO* sac_info, 
                                bool found_insertion_pt)
{
  if (!sac_info) return;

  OPCODE opcode = WN_opcode(wn);
  if (wn == sac_info->copy_insertion_wn)
    found_insertion_pt = true;
  if ((WN_operator(wn) == OPR_ISTORE) && found_insertion_pt)
  {
    // check if this is a store to a field in our struct
    if ((TY_kind(WN_ty(wn)) == KIND_POINTER) &&
        (TY_IDX_index(TY_pointed(WN_ty(wn))) == 
         TY_IDX_index(sac_info->orig_ty))) 
    {
      WN* copy_tree = Generate_Copy_Code_For_Write(wn, sac_info);
    }
  }
  else if (opcode == OPC_BLOCK)
  {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
    {
      Find_Writes_To_Struct_Type(w, sac_info, found_insertion_pt);
    }
  }
  else
  {
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++)
    {
      WN* kid = WN_kid(wn, kidno);
      Find_Writes_To_Struct_Type(kid, sac_info, found_insertion_pt);
    }
  }
}

WN* Generate_Copy_Code_For_Write(WN* wn, SAC_INFO* sac_info)
{
  TYPE_ID comp_type = WN_desc(WN_start(sac_info->wn_loop));
  TYPE_ID index_type = MTYPE_U4;
  OPCODE op_and = OPCODE_make_op(OPR_LAND, Boolean_type, MTYPE_V);
  OPCODE op_lt = OPCODE_make_op(OPR_LE, Boolean_type, comp_type);
  OPCODE op_ge = OPCODE_make_op(OPR_GE, Boolean_type, comp_type);
  OPCODE op_ldid = OPCODE_make_op(OPR_LDID, index_type, index_type);
  OPCODE op_div = OPCODE_make_op(OPR_DIV, index_type, MTYPE_V);
  OPCODE op_mod = OPCODE_make_op(OPR_MOD, index_type, MTYPE_V);
  OPCODE op_sub = OPCODE_make_op(OPR_SUB, index_type, MTYPE_V);
  OPCODE op_add = OPCODE_make_op(OPR_ADD, index_type, MTYPE_V);
  OPCODE op_mpy = OPCODE_make_op(OPR_MPY, index_type, MTYPE_V);

  WN* ldid1 = WN_COPY_Tree(WN_kid1(wn)); // base addr
  WN* ldid2 = LWN_CreateLdid(op_ldid, sac_info->saved_start_wn);
  WN* ldid3 = LWN_CreateLdid(op_ldid, sac_info->saved_end_wn);

  WN* lb_compare = LWN_CreateExp2(op_ge, ldid1, ldid2);
  WN* ub_compare;
  if (sac_info->end_is_kid0)
  {
    ub_compare = LWN_CreateExp2(sac_info->end_comp_op, 
                                ldid3, WN_COPY_Tree(WN_kid1(wn)));
  }
  else
  {
    ub_compare = LWN_CreateExp2(sac_info->end_comp_op, 
                                  WN_COPY_Tree(WN_kid1(wn)), ldid3);
  }
  WN* if_test = LWN_CreateExp2(op_and, lb_compare, ub_compare);

  // col = (addr - start) / stride;
  // row = (addr - start) % stride;
  // index = row * num_chunks + col;
  // syncs a write to the original struct
  //    x->field = y;
  // by copying the write to the copy struct
  //   x_copy[index].field = y;
  UINT orig_field_id = WN_field_id(wn);
  UINT new_field_id = sac_info->fld_info[orig_field_id].new_fld_id;
  int new_field_offset = sac_info->fld_info[orig_field_id].new_offset;

  UINT cur_field_id = 0;
  FLD_HANDLE orig_fld = FLD_get_to_field(sac_info->orig_ty, 
                                         orig_field_id, cur_field_id);
  TY_IDX field_ty = FLD_type(orig_fld);
  TYPE_ID field_mtype;
  if (TY_kind(field_ty) == KIND_POINTER)
    field_mtype = Pointer_Mtype;
  else
    field_mtype = field_ty >> 8;
  OPCODE op_istore = OPCODE_make_op(OPR_ISTORE, MTYPE_V, field_mtype);

  WN* diff = 
    LWN_CreateExp2(op_div,
                   LWN_CreateExp2(op_sub,
                                  LWN_CreateLdid(op_ldid, 
                                                 WN_COPY_Tree(WN_kid1(wn))),
                                  LWN_CreateLdid(op_ldid,
                                                 sac_info->saved_start_wn)),
                   LWN_Make_Icon(MTYPE_U4, TY_size(sac_info->orig_ty)));  

  WN* col = 
    LWN_CreateExp2(op_div,
                   WN_COPY_Tree(diff),
                   LWN_CreateLdid(op_ldid, sac_info->saved_struct_stride));

  WN* row = 
    LWN_CreateExp2(op_mod,
                   WN_COPY_Tree(diff),
                   LWN_CreateLdid(op_ldid, sac_info->saved_struct_stride));
                   
  WN* index = 
    LWN_CreateExp2(op_add,
                   LWN_CreateExp2(op_mpy,
                                  row,
                                  LWN_CreateLdid(op_ldid,
                                                 sac_info->saved_num_chunks_wn)),
                   col);
  WN* offset = 
    LWN_CreateExp2(op_mpy, index, 
                   LWN_Make_Icon(index_type, TY_size(sac_info->new_ty)));
  WN* base_plus_offset = 
    LWN_CreateExp2(op_add, 
                   LWN_CreateLdid(op_ldid, sac_info->array_copy_wn),
                   offset);
  
  WN* istore_wn = LWN_CreateIstore(op_istore, new_field_offset,
                                   Make_Pointer_Type(sac_info->new_ty),
                                   WN_COPY_Tree(WN_kid0(wn)),
                                   base_plus_offset, new_field_id);
  WN* if_then = WN_CreateBlock();
  WN_INSERT_BlockBefore(if_then, NULL, istore_wn);
  WN* copy_tree = WN_CreateIf(if_test, if_then, WN_CreateBlock());
  
  WN* parent_block = LWN_Get_Parent(wn);
  while (WN_opcode(parent_block) != OPC_BLOCK) {
    parent_block = LWN_Get_Parent(parent_block);
  } 
  // insert copy BEFORE the original wn, otherwise we will
  // attempt to process it below
  WN_INSERT_BlockBefore(parent_block, wn, copy_tree);
  if (Trace_SAC)
  {
    fprintf(stderr, "Found write to struct type\n");
    dump_tree(wn);
    fprintf(stderr, "copy:\n");
    dump_tree(copy_tree);
  }

  IF_INFO* ii =
    CXX_NEW(IF_INFO(&LNO_default_pool, FALSE, FALSE), &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, copy_tree, (void*) ii);
  DOLOOP_STACK stk(&LNO_local_pool);
  Build_Doloop_Stack(copy_tree, &stk);
  LNO_Build_If_Access(copy_tree, &stk);

  Do_DU_Update(copy_tree);
  return copy_tree;
}
