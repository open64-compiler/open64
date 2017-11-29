/*
 * Copyright (C) 2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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


//                     Automatic Data Distribution
//                     ---------------------------
//
// Automatically distribute arrays.  Several types
//
// 1. For local and global arrays, if all the parallel loops are
//    in the same dimension[s], do a block distribution
//    across that dimension.  Only do it if the array reference is
//    within epsilon of a[i-lb].  Only do it for arrays larger
//    than 150K bytes.
//
// 2. For local , "well behaved", arrays, transpose the arrays so that we 
//    can make the parallel dimensions stride n
//    The transpose graph works as follows.  We build a graph with vertices
//    for transposable arrays (with > 1 dimension) and for snls.  
//    With each snl is associated
//    a vector stating whether loop 'i' in the snl can go parallel.  We
//    zero entry 'i' if dependences prevent parallelization or if there
//    exists a non-transposable array with greater than one dimension that
//    references loop 'i' in the stride-1 dimension.
//
//    With each edge from an snl vertex to an array, is associated a 
//    constraint vector.  If contraint[i] = j, that means that if
//    chose loop 'i' to be parallel, we need to make dimension 'j'
//    the non-stride-1 dimension.
//    With each edge from an array to an snl vertex, is associated a 
//    constraint vector.  If contraint[i] = j, that means that if
//    chose dimension 'i' to be the least stride-1, we need to make loop 'j'
//    parallel.  Whenever we see an array reference that uses loop variable
//    'i' in dimension 'j', we set constraint[j]=i in the edge going from
//    the array to the snl, and we set constraint[i]=j in the reverse edge.
//
//    Given the constraint graph, we then solve as follows.  We visit the
//    snls in order, for each snl, we go through the possible loops to be
//    mp.  For each loop, we propogate the contraint through the graph.
//    If there is an inconsistency, it's impossible to select a set of 
//    loops so that all transposable arrays can be parallel in their least
//    stride-1 dimension.  We transform the arrays according to the first
//    consistent setting we find.
//   
//
/* ====================================================================
 * ====================================================================
 *
 * Module: autod.cxx
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:57:12-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.autod.cxx $
 *
 * Revision history:
 *  21-2-97 - Original Version
 *
 * Description: Automatic data distribution
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

static const char *source_file = __FILE__;
static const char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.autod.cxx $ $Revision: 1.5 $";

#include <sys/types.h>
#include <ctype.h>
#include <limits.h>
#include <alloca.h>

#include "pu_info.h"
#include "autod.h"
#include "lego_pragma.h"
#include "fiz_fuse.h"
#include "snl_utils.h"

static MEM_POOL Distr_Local_Pool;
static BOOL distributed_something=FALSE;
static BOOL distributed_something_pu;
TY_IDX Create_New_Array_Type(TY_IDX old_array_ty);
extern "C" {
  void Lego_File_Init (void);
}

void Automatic_Data_Distribute(WN *wn)
{
  distributed_something_pu=FALSE;
  MEM_POOL_Initialize(&Distr_Local_Pool,"Distr_Local_Pool",FALSE);
  DISTRIBUTION(wn,&Distr_Local_Pool);
  MEM_POOL_Delete(&Distr_Local_Pool);
}

void Transpose_For_MP(WN *wn)
{
  MEM_POOL_Push(&LNO_local_pool);
  TRANSPOSE_DIRECTED_GRAPH16 graph(100,100);
  ARRAY_TRANSPOSE_TREE *arrays = 
    CXX_NEW(ARRAY_TRANSPOSE_TREE(&LNO_local_pool),&LNO_local_pool);
  graph.Build(wn,arrays);
  graph.Solve(arrays);
  if (graph.Did_Transpose()) {
    graph.Transpose(wn,arrays);
    LNO_Build_Access(wn, &LNO_default_pool);
  }
  MEM_POOL_Pop(&LNO_local_pool);
}

// Regular distributions of arrays
DISTRIBUTION::DISTRIBUTION(WN *wn, MEM_POOL *pool)
{
  _pool = pool;
  MEM_POOL_Push(_pool);
  _locals = CXX_NEW(ARRAY_DESCR_TREE(_pool),_pool);
  _locals_stack = CXX_NEW(ARRAY_DESCR_STACK(_pool),_pool);
  _globals = CXX_NEW(ARRAY_DESCR_TREE(_pool),_pool);
  _globals_stack = CXX_NEW(ARRAY_DESCR_STACK(_pool),_pool);
  _do_stack = CXX_NEW(DOLOOP_STACK(_pool),_pool);
  _preamble = WN_first(WN_func_body(wn));
  while (WN_opcode(_preamble) != OPC_PRAGMA ||
	 WN_pragma(_preamble) != WN_PRAGMA_PREAMBLE_END) {
    _preamble = WN_next(_preamble);
  }

  Gather_Arrays(wn,FALSE);
  Distribute_Arrays();
  MEM_POOL_Pop(_pool);
}

// Distribute all the local and global arrays
void DISTRIBUTION::Distribute_Arrays()
{

  INT i;

  for (i=0; i<_locals_stack->Elements(); i++) {
    ARRAY_DESCRIPTOR ad = ARRAY_DESCRIPTOR(_locals_stack->Bottom_nth(i),0,0);
    BINARY_TREE_NODE<ARRAY_DESCRIPTOR> *find = _locals->Find(ad);
    if (find) { 
      ARRAY_DESCRIPTOR *descr = find->Get_Data();
      descr->Distribute_Array(WN_next(_preamble));
    }
  }

  for (i=0; i<_globals_stack->Elements(); i++) {
    ARRAY_DESCRIPTOR ad = ARRAY_DESCRIPTOR(_globals_stack->Bottom_nth(i),0,0);
    BINARY_TREE_NODE<ARRAY_DESCRIPTOR> *find = _globals->Find(ad);
    if (find) { 
      ARRAY_DESCRIPTOR *descr = find->Get_Data();
      descr->Distribute_Array(WN_next(_preamble));
    }
  }
}

// Distribute a local or global array
void ARRAY_DESCRIPTOR::Distribute_Array(WN *insert_point)
{
  if (Is_Bad()) {
    return;
  }

  TY_IDX array_ty = Lego_Get_Array_Type(_st);
  mINT16 ndims = TY_AR_ndims (array_ty);

  // Check that the bounds are all fine
  INT i;
  for (i=0; i<ndims; i++) {
    if (!TY_AR_const_lbnd(array_ty,i) ||
        !TY_AR_const_ubnd(array_ty,i)) {
      return;
    }
  }

  if (!LNO_Run_Lego) {
    if (!distributed_something) {
      Lego_File_Init();
    }
    if (!distributed_something_pu) {
      Lego_PU_Init();
    }
  }
  LNO_Run_Lego = TRUE;
  distributed_something=TRUE;
  distributed_something_pu=TRUE;
#ifdef _NEW_SYMTAB
  Set_PU_mp_needs_lno(Get_Current_PU());
  Set_FILE_INFO_needs_lno(File_info);
#else
  Set_SYMTAB_mp_needs_lno(Current_Symtab);
  Set_SYMTAB_mp_needs_lno(Global_Symtab);
#endif


  // Generate the pragmas
  if (ndims != _parallel_dims->Size()) return;
  WN *first_pragma;
  for (i=0; i<ndims; i++) {
    BOOL is_block = _parallel_dims->Test(i);
    WN *pragma = WN_CreatePragma(WN_PRAGMA_DISTRIBUTE,_st,0,0);
    WN_set_pragma_compiler_generated(pragma);
    if (i == 0) first_pragma = pragma;
    WN_pragma_index(pragma) = i;
    if (is_block) {
      WN_pragma_distr_type(pragma) = DISTRIBUTE_BLOCK;
    } else {
      WN_pragma_distr_type(pragma) = DISTRIBUTE_STAR;
    }
    LWN_Insert_Block_Before(LWN_Get_Parent(insert_point),insert_point,pragma);

    INT64 size = (TY_AR_ubnd_val(array_ty, ndims-1-i)-
                  TY_AR_lbnd_val(array_ty, ndims-1-i)+1);
    WN *xpwn = WN_Create(OPC_XPRAGMA, 1);
    WN_pragma(xpwn) = WN_PRAGMA_DISTRIBUTE;
    WN_set_pragma_compiler_generated(xpwn);
    WN_st_idx(xpwn) = ST_st_idx(_st);
    if (size > INT32_MAX) {
      WN_kid0(xpwn) = LWN_Make_Icon(MTYPE_I8,size);
    } else {
      WN_kid0(xpwn) = LWN_Make_Icon(MTYPE_I4,size);
    }
    LWN_Parentize(xpwn);
    LWN_Insert_Block_Before(LWN_Get_Parent(insert_point),insert_point,xpwn);

  }
  Read_Pragma_Distribute(first_pragma);
}




// Walk the code looking at all local and global arrays
// Enter all the candidates for distribution into the tree
// Also enter all the arrays we know that we don't want
// to distribute into the tree
void DISTRIBUTION::Gather_Arrays(WN *wn, BOOL seen_mp)
{
  OPCODE opc = WN_opcode(wn);
  OPERATOR oper = OPCODE_operator(opc);
  if (opc == OPC_BLOCK) {
    WN *kid = WN_first(wn);
    while (kid) {
      Gather_Arrays(kid,seen_mp);
      kid = WN_next(kid);
    }
    return;
  }
  if (opc == OPC_DO_LOOP) {
    if (Do_Loop_Is_Mp(wn)) {
      seen_mp = TRUE;
    }
    _do_stack->Push(wn);
  } else if ((oper == OPR_ILOAD) || (oper == OPR_ISTORE)) {
    if (seen_mp) Process_Memory(wn);
  }
  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    Gather_Arrays(WN_kid(wn,kidno),seen_mp);
  }
  if (opc == OPC_DO_LOOP) {
    _do_stack->Pop();
  }
}

// Process an array load or store
void DISTRIBUTION::Process_Memory(WN *wn)
{
  // First, is it a clean, local or global array
  WN *array ;
  if (OPCODE_is_store(WN_opcode(wn))) {
    array = WN_kid1(wn);
  } else {
    array = WN_kid0(wn);
  }
  if (WN_operator(array) != OPR_ARRAY) return;
  if (WN_offset(wn)) return;
  WN *array_base = WN_array_base(array);
  if (WN_operator(array_base) != OPR_LDA) {
    return;
  }
  ST *st = WN_st(array_base);
  ARRAY_DESCR_TREE *tree;
  ARRAY_DESCR_STACK *stack;

#ifdef _NEW_SYMTAB
  if ((ST_class(st) == CLASS_VAR) && !ST_is_not_used(st) &&
      ST_addr_not_saved(st) && 
      (TY_size(ST_type(st)) > 150000) &&
#else
  if ((ST_symclass(st) == CLASS_VAR) && !ST_is_not_used(st) &&
      !ST_addr_taken_saved(st) && 
      (ST_size(st) > 150000) && 
#endif
      (!ST_is_reshaped(st)) &&
      (TY_kind(ST_type(st)) == KIND_ARRAY) &&
      (!da_hash || !da_hash->Find(st))) {
    if (ST_sclass(st) == SCLASS_AUTO &&
	ST_base_idx(st) == ST_st_idx(st)) {
      if (!ST_has_nested_ref(st)) {
        // a good local array
        tree = _locals;
        stack = _locals_stack;
      } else {
	return;
      }
    } else if (ST_sclass(st) == SCLASS_COMMON || 
#ifdef _NEW_SYMTAB
	((ST_base_idx(st) != ST_st_idx(st)) && 
#else
	((ST_sclass(st) == SCLASS_BASED) && 
#endif
	 (ST_sclass(ST_base(st)) == SCLASS_COMMON))) {
      // a good global array
      tree = _globals;
      stack = _globals_stack;
    } else {
      return;
    }
  } else { 
    return;
  }

  // It's a clean array, now check if we want to distribute it
  MEM_POOL_Push(&LNO_local_pool);
  INT num_dim = WN_num_dim(array);
  BIT_VECTOR *reference = CXX_NEW(BIT_VECTOR(num_dim,
			&LNO_local_pool),&LNO_local_pool);
  BOOL is_bad = FALSE;
  ACCESS_ARRAY *access_array =
	    (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array);
  if (access_array->Num_Vec() != num_dim) is_bad = TRUE;
  if (access_array->Too_Messy) is_bad = TRUE;

  for (INT i=0; i<num_dim && !is_bad; i++) {
    ACCESS_VECTOR *av = access_array->Dim(i);
    if (av->Too_Messy) {
      is_bad = TRUE;
    } else {
      INT num_loop_var = 0;
      INT mp_var = -1;
      for (INT j=0; j<av->Nest_Depth() &&!is_bad; j++) {
	if (av->Loop_Coeff(j) == 1) {
	  if (Do_Loop_Is_Mp(_do_stack->Bottom_nth(j))) {
	    num_loop_var++;
	    mp_var = j;
          }
	} else if (av->Loop_Coeff(j)) {
	  if (Do_Loop_Is_Mp(_do_stack->Bottom_nth(j))) {
	    is_bad = TRUE;
          }
        }
      }
      if (mp_var != -1) { // [...+i..] where 'i' is mp
	if (num_loop_var != 1 || av->Non_Const_Loops() > mp_var) {
	  is_bad = TRUE;
        } else {
	  // now we know that it's [i+const_expr]
	  // want to verify that when i=lb, i+const_expr
	  // is close to zero, otherwise we would have to distribute
	  // with an offset, which we don't do
	  ACCESS_ARRAY *lb = Get_Do_Loop_Info(_do_stack->Bottom_nth(mp_var))->LB;
	  if (lb->Too_Messy || lb->Num_Vec() != 1) {
	    is_bad = TRUE;
          } else {
	    ACCESS_VECTOR *low = CXX_NEW(ACCESS_VECTOR(lb->Dim(0),&LNO_local_pool),&LNO_local_pool);
	    if (abs(low->Const_Offset - av->Const_Offset) < epsilon) {
	      low->Negate_Me();
	      low->Const_Offset = av->Const_Offset;
	      if (*low == *av) {
	        reference->Set(i);  // a good candidate for distribution
              } else {
		is_bad = TRUE;
	      }
	    } else {
	      is_bad = TRUE;
	    }
          }
        }
      }
    }
  }
  if (is_bad || reference->Pop_Count()) {
    ARRAY_DESCRIPTOR ad = ARRAY_DESCRIPTOR(st,reference,is_bad);
    BINARY_TREE_NODE<ARRAY_DESCRIPTOR> *find = tree->Find(ad);
    if (find) { // we've seen this array
      find->Get_Data()->Union(&ad);
    } else { // we haven't seen this array before
      BIT_VECTOR *bv = CXX_NEW(BIT_VECTOR(reference->Size(),_pool),_pool);
      *bv = *reference;
      tree->Enter(ARRAY_DESCRIPTOR(st,bv,is_bad));
      stack->Push(st);
    }
  } 
  MEM_POOL_Pop(&LNO_local_pool);

}

// Routines to transpose arrays so that we
// can get parallelisim on non stride1 dimensions

// Build a graph representing all the arrays and SNLs
// Put in all the constraints where a constraint is something
// like (if loop 'i' is parallel, then dim 'j' of an array should be
// parallel, and vice versa)
void TRANSPOSE_DIRECTED_GRAPH16::Build(WN *func_nd,ARRAY_TRANSPOSE_TREE *arrays)
{
  Gather_Arrays(func_nd,arrays);
  FIZ_FUSE_INFO* ffi=
    CXX_NEW(FIZ_FUSE_INFO(&LNO_local_pool), &LNO_local_pool);
  ffi->Build(func_nd);
  for (INT i = 0; i < ffi->Num_Snl() && !_is_bad; i++) {
    INT nloops = ffi->Get_Depth(i);
    if (nloops >=1 && nloops <= TRANSPOSE_MAX_SIZE) {
      if (ffi->Get_Type(i) == Inner || ffi->Get_Type(i) == Not_Inner) {
	WN *outer = ffi->Get_Wn(i);
	if (!Outermore_Parallelizable(LWN_Get_Parent(outer))) {
          WN *inner = SNL_Get_Inner_Snl_Loop(outer,nloops);
	  if (Contains_Parallelizable(inner,nloops)) {
            Build_Snl(inner,nloops,arrays);
          }
        }
      }
    }
  }
}

// Walk the code looking for all the arrays
void TRANSPOSE_DIRECTED_GRAPH16::Gather_Arrays(WN *wn,
				ARRAY_TRANSPOSE_TREE *arrays)
{
  OPCODE opc = WN_opcode(wn);
  if (opc == OPC_BLOCK) {
    WN *kid=WN_first(wn);
    while (kid) {
      Gather_Arrays(kid,arrays);
      kid = WN_next(kid);
    }
  } else if (OPCODE_operator(opc) == OPR_LDA) {
    if (Local_Array(WN_st(wn))) {
      BOOL transposable = FALSE;
      WN *parent = LWN_Get_Parent(wn);
      TY_IDX array_ty = Lego_Get_Array_Type(WN_st(wn));
      if ((WN_operator(parent) == OPR_ARRAY) &&
	  (wn == WN_array_base(parent)) &&
	  (WN_element_size(parent) >= 1) && 
	  (!WN_offset(wn)) &&
	  TY_AR_ndims(array_ty) == WN_num_dim(parent)) {
        
        // We're only checking LDA, so it must be a fixed-size
        // automatic (VLA automatics are LDIDs). In that case verify
        // that the bounds in the array reference match those in the
        // type description.

        BOOL good_bounds = TRUE;

        for (INT i=0; i<TY_AR_ndims(array_ty); i++) {
          
          // verify that bounds are const in both type table and in
          // the array node, and that they match.

          if (TY_AR_const_ubnd(array_ty,i) &&
              TY_AR_const_lbnd(array_ty,i) &&
              WN_operator(WN_array_dim(parent,i)) == OPR_INTCONST &&
              (WN_const_val(WN_array_dim(parent,i)) ==
               (TY_AR_ubnd_val(array_ty,i)-TY_AR_lbnd_val(array_ty,i)+1))) {
            continue;
          }

          good_bounds = FALSE;
          break;
        }

        if (good_bounds) {

          WN *grandparent = LWN_Get_Parent(parent);
          OPCODE gopc = WN_opcode(grandparent);
          if (OPCODE_is_load(gopc) ||
              ((OPCODE_operator(gopc) == OPR_ISTORE) &&
               (parent == WN_kid1(grandparent)))) {
            if (WN_offset(grandparent) == 0) { 
              transposable = TRUE;
            }
          } else if (IO_element_read(grandparent)) {
            transposable = TRUE;
          }
        }
      }
      ARRAY_TRANSPOSE_DESCRIPTOR tmp = ARRAY_TRANSPOSE_DESCRIPTOR(WN_st(wn));
      BINARY_TREE_NODE<ARRAY_TRANSPOSE_DESCRIPTOR> *find = arrays->Find(tmp);
      if (find) { 
        ARRAY_TRANSPOSE_DESCRIPTOR *atd = find->Get_Data();
	if (!transposable) atd->Reset_Transposable();
      } else {
	arrays->Enter(ARRAY_TRANSPOSE_DESCRIPTOR(WN_st(wn),transposable));
      }
    }
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Gather_Arrays(WN_kid(wn,kidno),arrays);
    }
  }
}

// Is this a simple read of a variable
BOOL TRANSPOSE_DIRECTED_GRAPH16::IO_element_read(WN *item)
{
  if (WN_operator(item) == OPR_IO_ITEM) {
    if (WN_io_item(item) == IOL_VAR) {
      WN *parent = LWN_Get_Parent(item);
      if (WN_opcode(parent) == OPC_IO) {
	if (WN_io_statement(parent) == IOS_READ) {
	  return TRUE;
        } else if (WN_io_statement(parent) == IOS_CR_FRF) {
	  return TRUE;
        } else if (WN_io_statement(parent) == IOS_CR_FRU) {
	  return TRUE;
        }
      }
    }
  }
  return FALSE;
}
    

// Add the vertices and edges and constraints for an snl
void TRANSPOSE_DIRECTED_GRAPH16::Build_Snl(WN *inner, INT nloops,
		ARRAY_TRANSPOSE_TREE *arrays)
{
  VINDEX16 snl_v = Add_Vertex(nloops,inner);
  if (!snl_v) {
    _is_bad = TRUE;
    return;
  }
  WN *wn = inner;
  WN *outer;
  for (INT i=0; i<nloops; i++) {
    outer=wn;
    if (Get_Do_Loop_Info(wn)->Parallelizable) {
      Set_Can_Be_Parallel(snl_v,nloops-1-i);
    } else {
      Reset_Can_Be_Parallel(snl_v,nloops-1-i);
    }
    wn = LWN_Get_Parent(LWN_Get_Parent(wn));
  }
  Build_Snl_Arrays(outer,arrays,Do_Loop_Depth(outer),Do_Loop_Depth(inner),snl_v);
  if (_is_bad) return;
    
}

// Walk the snl, processing all the arrays
void TRANSPOSE_DIRECTED_GRAPH16::Build_Snl_Arrays(WN *wn, 
		ARRAY_TRANSPOSE_TREE *arrays, INT outer_depth,INT inner_depth,VINDEX16 snl_v)
{
  OPCODE opc = WN_opcode(wn);
  if (opc == OPC_BLOCK) {
    WN *kid=WN_first(wn);
    while (kid) {
      Build_Snl_Arrays(kid,arrays,outer_depth,inner_depth,snl_v);
      if (_is_bad) return;
      kid = WN_next(kid);
    }
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Build_Snl_Arrays(WN_kid(wn,kidno),arrays,outer_depth,inner_depth,snl_v);
      if (_is_bad) return;
    }
    if (OPCODE_operator(opc) == OPR_ARRAY) {
      if (WN_num_dim(wn) > 1) {
	Build_Snl_Array(wn,arrays,outer_depth,inner_depth,snl_v);
      }
    }
  }
}

// Process a single array
void TRANSPOSE_DIRECTED_GRAPH16::Build_Snl_Array(WN *array,
		ARRAY_TRANSPOSE_TREE *arrays, INT outer_depth,INT inner_depth,VINDEX16 snl_v)
{
  WN *base = WN_array_base(array);
  ACCESS_ARRAY *access_array =
	    (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array);
  if (!WN_has_sym(base) ||
      access_array->Too_Messy ||
      access_array->Num_Vec() != WN_num_dim(array)) {
    return;
  }

  ARRAY_TRANSPOSE_DESCRIPTOR atd = ARRAY_TRANSPOSE_DESCRIPTOR(WN_st(base), 0);
  BINARY_TREE_NODE<ARRAY_TRANSPOSE_DESCRIPTOR> *find = arrays->Find(atd);
  if (find && find->Get_Data()->Transposable()) {
    VINDEX16 array_v = find->Get_Data()->Get_Vertex();
    if (!array_v) {
      array_v = Add_Vertex(WN_num_dim(array),WN_st(base));
      if (!array_v) {
	_is_bad = TRUE;
	return;
      }
      find->Get_Data()->Set_Vertex(array_v);
    }
    EINDEX16 e1 = Get_Edge(snl_v,array_v);
    EINDEX16 e2 = Get_Edge(array_v,snl_v);
    for (INT i=0; i<access_array->Num_Vec(); i++) {
      ACCESS_VECTOR *av = access_array->Dim(i);
      if (!av->Too_Messy) {
	for (INT j=outer_depth; j<MIN(inner_depth+1,av->Nest_Depth()); j++) {
	  if (av->Loop_Coeff(j)) {
	    if (!e1) e1 = Add_Edge(snl_v,array_v,inner_depth-outer_depth+1);
	    if (!e2) e2 = Add_Edge(array_v,snl_v,access_array->Num_Vec());
	    if (!e1 || !e2) {
	      _is_bad = TRUE;
	      return;
            }
	    // if 'j' is parallel, dim 'i' should be innermost (least stride 1)
	    Set_Constraint(e1,j-outer_depth,i);
	    // if 'i' is innermost, loop 'j' should be parallel
	    Set_Constraint(e2,i,j-outer_depth);
          }
        }
      }
    }
  } else {  // can't transpose this array, make sure it's stride 1
	    // dimensions isn't parallel
    ACCESS_ARRAY *access_array =
	    (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array);
    if (!access_array->Too_Messy) {
      ACCESS_VECTOR *av = access_array->Dim(access_array->Num_Vec()-1);
      if (!av->Too_Messy) {
	for (INT i=outer_depth; i<MIN(inner_depth+1,av->Nest_Depth()); i++) {
	  if (av->Loop_Coeff(i)) {
            Reset_Can_Be_Parallel(snl_v,i-outer_depth);
	  }
	}
      }
    }
  }
}

// Propogate all the contraints, if we find a consistent set, great.
void TRANSPOSE_DIRECTED_GRAPH16::Solve(ARRAY_TRANSPOSE_TREE *arrays)
{
  // while there are loop vertices in the graph
  VINDEX16 v=Get_Loop_Vertex();
  while (v) {
    BOOL consistent = FALSE;
    for (INT i=0; i<_v[v].size && !consistent; i++) {
      if (Can_Be_Parallel(v,i)) {
        Clear_Values();
        _v[v].value = i;
	if (Propogate_V(v)) {
	  consistent=TRUE;
	  Record(arrays); 
	}
      }
    }
    if (!consistent) {
      _is_bad = TRUE;
      return;
    }
    v=Get_Loop_Vertex();
  }
}

// Mark the selected arrays with their values, remove
// them from the graph
void TRANSPOSE_DIRECTED_GRAPH16::Record(ARRAY_TRANSPOSE_TREE *arrays)
{
  for (INT i=1; i<_v.Lastidx()+1; i++) {
    if (!_v[i].Is_Free()) {
      if (_v[i].value != -1) {
        if (_v[i].value > 0 && !_v[i].is_loop) {
          ARRAY_TRANSPOSE_DESCRIPTOR atd =
            ARRAY_TRANSPOSE_DESCRIPTOR(_v[i].tvertex_union.st, 0);
          BINARY_TREE_NODE<ARRAY_TRANSPOSE_DESCRIPTOR> *find = arrays->Find(atd);
	  find->Get_Data()->Set_Dimension(_v[i].value);
	  Transpose_Array(_v[i].tvertex_union.st,_v[i].value);
	  if (LNO_Verbose) {
	    fprintf(stderr,"Transposing array %s \n",
	    	ST_name(_v[i].tvertex_union.st));
          }
	  _did_transpose = TRUE;
        }
	Delete_Vertex(i);
      }
    }
  }
}

// Walk the code and transpose
void TRANSPOSE_DIRECTED_GRAPH16::Transpose(WN *wn, ARRAY_TRANSPOSE_TREE *arrays)
{
  OPCODE opc = WN_opcode(wn);
  if (opc == OPC_BLOCK) {
    WN *kid = WN_first(wn);
    while (kid) {
      Transpose(kid,arrays);
      kid = WN_next(kid);
    }
  } else if (OPCODE_operator(opc) == OPR_LDA) {
    WN *parent = LWN_Get_Parent(wn);
    if ((WN_operator(parent) == OPR_ARRAY) &&
	(wn == WN_array_base(parent))) {
      ARRAY_TRANSPOSE_DESCRIPTOR atd = ARRAY_TRANSPOSE_DESCRIPTOR(WN_st(wn),
								  0);
      BINARY_TREE_NODE<ARRAY_TRANSPOSE_DESCRIPTOR> *find = arrays->Find(atd);
      if (find && (find->Get_Data()->Get_Dimension() != -1)) {
	Transpose_Array(parent,find->Get_Data()->Get_Dimension());
      }
    }
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Transpose(WN_kid(wn,kidno),arrays);
    }
  }
}

// Make dimension 'dim' of the array the 0'th dimension
void TRANSPOSE_DIRECTED_GRAPH16::Transpose_Array(WN *array,INT dim)
{
  WN *zero_dim = WN_array_dim(array,dim);
  WN *zero_index = WN_array_index(array,dim);
  for (INT i=dim; i>0; i--) { // shift the other kids to make room
    WN_array_dim(array,i) = WN_array_dim(array,i-1);
    WN_array_index(array,i) = WN_array_index(array,i-1);
  }
  WN_array_dim(array,0) = zero_dim;
  WN_array_index(array,0) = zero_index;
}

// Transpose the symbol table entry for the array
void TRANSPOSE_DIRECTED_GRAPH16::Transpose_Array(ST *st,INT dim)
{
  INT ndims = TY_AR_ndims(ST_type(st));
  INT element_size = TY_size(TY_AR_etype(ST_type(st)));

  TY_IDX old_ty = ST_type(st);
  TY_IDX new_ty = Create_New_Array_Type(old_ty);
  Set_ST_type(st,new_ty);
  Set_TY_AR_lbnd_val(new_ty,0, TY_AR_lbnd_val(old_ty,dim));
  Set_TY_AR_ubnd_val(new_ty,0, TY_AR_ubnd_val(old_ty,dim));
  INT i;
  for (i=1; i<=dim; i++) {
    Set_TY_AR_lbnd_val(new_ty,i, TY_AR_lbnd_val(old_ty,i-1));
    Set_TY_AR_ubnd_val(new_ty,i, TY_AR_ubnd_val(old_ty,i-1));
  }

  for (i=0; i<ndims; i++) {
    INT stride = element_size;
    for (INT j=i+1; j<ndims; j++) {
      stride = (stride * (TY_AR_ubnd_val(new_ty, j) -
                          TY_AR_lbnd_val(new_ty, j) + 1));
    }
    Set_TY_AR_stride_val(new_ty,i,stride);
  }
}



// Propogate the contraints from a vertex
// Return 1 if things were consistent, 0 otherwise
BOOL TRANSPOSE_DIRECTED_GRAPH16::Propogate_V(VINDEX16 v)
{
  EINDEX16 e = _v[v].Get_Out_Edge();
  INT v_value = _v[v].value;
  while (e) {
    INT constraint = Get_Constraint(e,v_value);
    if (constraint != -1) {  // we have a new constraint
      VINDEX16 v2 = _e[e].Get_Sink();
      if (_v[v2].is_loop && !Can_Be_Parallel(v2,constraint)) return 0;
      if (_v[v2].value != -1) { // v2 is already constrained
	if (_v[v2].value != constraint) return 0;
      } else {
	_v[v2].value = constraint;
	if (!Propogate_V(v2)) {
	  return 0;
        }
      }
    }
    e = _e[e].Get_Next_Out_Edge();
  }
  return 1;
}

VINDEX16 TRANSPOSE_DIRECTED_GRAPH16::Get_Loop_Vertex()
{
  for (INT i=1; i<_v.Lastidx()+1; i++) {
    if (!_v[i].Is_Free() && _v[i].is_loop) {
      return i;
    }
  }
  return 0;
}

void TRANSPOSE_DIRECTED_GRAPH16::Clear_Values()
{
  for (INT i=1; i<_v.Lastidx()+1; i++) {
    if (!_v[i].Is_Free()) {
      _v[i].value = -1;
    }
  }
}

BOOL TRANSPOSE_DIRECTED_GRAPH16::Local_Array(ST *st)
{
#ifdef _NEW_SYMTAB
  if ((ST_class(st) == CLASS_VAR) && 
      (TY_size(ST_type(st)) > 0) &&
      (TY_kind(ST_type(st)) == KIND_ARRAY) &&
      (ST_sclass(st) == SCLASS_AUTO) &&
      (!ST_is_initialized(st)) &&
      (!ST_has_nested_ref(st)) &&
      (!ST_is_equivalenced(st)) && 
      (!da_hash || !da_hash->Find(st))) {
#else
  if ((ST_symclass(st) == CLASS_VAR) && 
      (ST_size(st) > 0) &&
      (TY_kind(ST_type(st)) == KIND_ARRAY) &&
      (ST_sclass(st) == SCLASS_AUTO) &&
      (!ST_is_initialized(st)) &&
      (!ST_has_nested_ref(st)) &&
      (!ST_is_non_contiguous(st)) &&
      (!ST_is_equivalenced(st)) && 
      (!da_hash || !da_hash->Find(st))) {
#endif
    return TRUE;
  } else {
    return FALSE;
  }
}

// Is there a loop further out than wn that can go mp
BOOL TRANSPOSE_DIRECTED_GRAPH16::Outermore_Parallelizable(WN *wn)
{
  if (!wn) return FALSE;
  OPCODE opc = WN_opcode(wn);
  if (opc == OPC_DO_LOOP && Get_Do_Loop_Info(wn)->Parallelizable) {
    return TRUE;
  }
  return Outermore_Parallelizable(LWN_Get_Parent(wn));
}

// Does this SNL contain a parallelizable loop
BOOL TRANSPOSE_DIRECTED_GRAPH16::Contains_Parallelizable(WN *wn, INT nloops)
{
  for (INT i=0; i<nloops; i++) {
    if (Get_Do_Loop_Info(wn)->Parallelizable) return TRUE;
    wn = LWN_Get_Parent(LWN_Get_Parent(wn));
  }
  return FALSE;
}

void TRANSPOSE_DIRECTED_GRAPH16::Print(FILE *fp)
{
  for (INT i=1; i<_v.Lastidx()+1; i++) {
    if (!_v[i].Is_Free()) {
      if (_v[i].is_loop) {
        fprintf(fp,"Vertex %d for loop ",i); 
        Dump_WN(_v[i].tvertex_union.inner_loop,fp,TRUE,0,0);
        fprintf(fp,"\n");
        fprintf(fp,"Can be parallel is ");
        for (INT j=0; j<_v[i].size; j++) {
	  fprintf(fp," %d ",_v[i].can_be_parallel[j]);
        }
        fprintf(fp,"\n");
      } else {
        fprintf(fp,"Vertex %d for array %s",i,
		ST_name(_v[i].tvertex_union.st));
        fprintf(fp,"\n");
      }
      EINDEX16 e = _v[i].Get_Out_Edge();
      while (e) {
        fprintf(fp,"Edge %d to vertex %d ",e,_e[e].Get_Sink());
        fprintf(fp,"Constraint is ");
        for (INT j=0; j<_v[i].size; j++) {
	  fprintf(fp," %d ",_e[e].constraint[j]);
        }
        fprintf(fp,"\n");
	e = _e[e].Get_Next_Out_Edge();
      }
    }
  }
}
  
