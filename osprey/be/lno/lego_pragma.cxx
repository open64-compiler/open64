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


#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

// ***********************************************************************
// 
// Algorithm for processing fill-align and data-distribution (reshape)
// pragmas with and without IPA.
// 
// Local (automatic) and formal variables:
// =======================================
//  - each of fill-align and data-distribution directives are
//    processed on a per-PU basis. No change with/without -IPA.
//    NOTE: We disable inlining if a PU contains reshape directives,
//    since we need to locate the pragmas etc in the preamble. Do we
//    need to do the same for fill-align? Not usually, only if the
//    fill-align pragma is not a simple update of the ST but requires
//    us to find the alloca etc.
//
// C Globals:
// ==========
//  fill-align pragmas:
//      w/o IPA:
//          - processed on a per-PU basis
//          - avoid duplicate processing through hash-table in f.a. processing
//          - object allocated after fill-align processing the file
//            that contains a true definition.
//      w/IPA:
//          - processed only when processing the dummy PU in symtab file
//          - duplicates within the symtab.G file are caught by the
//            usual hash-table in f.a. processing
//          - while compiling a 1.I, 2.I, etc file, if we are running
//            w/IPA and compiling a 1.I etc file, then we deliberately
//            ignore globals.
//          - object allocated after fill-align processing the
//            symtab.G file (the only file with a true definition).
//
// reshape pragmas:
//      w/o IPA:
//          - processed on a per-PU basis
//          - since changes are made in place, mangle the type only
//            the first time, and store the orig-ty in a hash-table on the side
//          - array is allocated in the file (.o) that contains a true
//            definition, not just a declaration
//
//      w/IPA:
//          - processed on a per-PU basis, including dummy PUs in
//            symtab.G file
//          - symtab.G mangles in place
//          - while processing other files, they find the original
//            type in the TYPE_OF_RESHAPED_ARRAY pragma in func-pragma-block.
//          - array is allocated while processing the symtab.G file,
//            since only that contains a true defintion; others find an
//            EXTERN declaration.
//
// Fortran COMMONS:
// ================
//  fill-align pragmas:
//      w/o IPA:
//          - processed on a per-PU basis; nothing shared between PUs,
//            so no duplicates.
//          - COMMONs don't appear to need Allocate_Object...
//      w/IPA:
//          - since fill-align requires that portion of the COMMON to
//            be consistent, IPA guarantees that this ST from multiple
//            PUs/files will be merged into exactly a single ST.
//          - We encounter a fill-align in each PU that references it,
//            but process it only the first time, and hash-table it to
//            avoid duplicates.
//          
//  reshape:
//      w/o IPA:
//          - processed on a per-PU basis, absolutely nothing shared
//            between PUs, so no duplicates.
//      w/IPA:
//          - as for fill-align above, ST must be merged by IPA
//          - encountered and processed on a per-PU basis, since each
//            PU needs to know the details of reshaping, but use a
//            hash-table to avoid duplicate mangling that is performed
//            in Reshape_ST_Entry.
//
// ***********************************************************************


#include <alloca.h>
#include <sys/types.h>
#include <ctype.h>
#include <limits.h>

#include "pu_info.h"
#include "lnopt_main.h"
#include "config_targ.h"
#include "cxx_memory.h"
#include "erbe.h"
#include "erglob.h"
#include "strtab.h"
#include "wn_pragmas.h"
#include "lego_pragma.h"
#include "lwn_util.h"
#include "opt_du.h"
#include "opt_alias_interface.h"
#include "lego_util.h"
#include "lego_opts.h"
#include "lnoutils.h"
#include "data_layout.h"
#include "lego_affinity.h"
#include "region_util.h"
#include <alloca.h>
#include "tile.h"
#include "be_symtab.h"

#pragma weak New_Construct_Id

/***********************************************************************
 *
 * External declarations.
 *
 ***********************************************************************/

DA_HASH_TABLE           *da_hash;
DA_STACK                *da_stack;
DA_GLOBAL_HASH_TABLE    *da_global;

extern void Read_Distr_Pragmas (WN* func_nd);
TY_IDX DART_ptr_TY = (TY_IDX) NULL;
extern ST *Find_Return_Registers(TYPE_ID type,PREG_NUM *rreg1,PREG_NUM *rreg2);
extern WN* Read_Pragma_Redistribute (WN* pwn, BOOL gen_phase);
extern WN* Numprocs (ST* array_st, INT dim, WN** block_wn);
extern WN* Find_SCF_Inside(WN* parent_wn, OPCODE opc); // in ff_utils.cxx
extern ST*  Create_Local_Array_ST(char* array_name, TY_IDX ty, INT num);
extern void Move_Alloca (ST* st);

/***********************************************************************
 *
 * Local Definitions.
 *
 ***********************************************************************/
static WN* Read_Pragma_Distribute_Reshape   (WN* pwn);
static WN* Read_Pragma_Affinity             (WN* pwn);
static WN* Read_Pragma_Page_Place           (WN* pwn);

static void Read_Pragma_Data_Affinity       (WN* do_wn, WN* rwn);
static void Read_Pragma_Thread_Affinity     (WN* pwn, WN* do_wn, WN* rwn);
static void Do_Loop_Explicit_Affinity       (WN* loop);
static void Do_Loop_Implicit_Affinity       (WN* loop);
static WN*  Thread_Affinity_Lower           (WN* do_wn,
                                             SYMBOL* thread_sym,
                                             WN* stid_wn);
static void Increment_Loop_Depths           (WN* wn);

static DISTR_ARRAY* New_DACT (WN** pwn, ST* array_st, INT ndims);
static BOOL Check_Expr (WN* expr_wn, SYMBOL* index_sym,
                        INT32* coeff, INT32* constant);
#ifdef _NEW_SYMTAB
static ST* Lookup_Variable(char *nm, INT stab);
#else
static ST* Lookup_Variable(char *nm, SYMTAB *stab);
#endif
static ST* Create_Global_Variable (char* nm, TYPE_ID type, ST* array_st,
                                   INT32 i);
static ST* Create_Local_Variable (const char* nm, TYPE_ID type, ST* array_st,
                                   INT32 i);
static ST* New_DART (ST* array_st);
static ST* Create_Common_Block (ST* array_st, INT numdim);
static mBOOL Array_Bounds_Equiv(TY_IDX array_ty1, TY_IDX array_ty2, INT32 dim1, 
				INT32 dim2);
static mBOOL Lower_Bounds_Equiv(TY_IDX array_ty1, TY_IDX array_ty2, INT32 dim1,
				INT32 dim2);
static mBOOL Upper_Bounds_Equiv(TY_IDX array_ty1, TY_IDX array_ty2, INT32 dim1,
				INT32 dim2);
static mBOOL Strides_Equiv(TY_IDX array_ty1, TY_IDX array_ty2, INT32 dim1,
			   INT32 dim2);

/* list of dynamic declarations seen so far. BOOL argument is bogus,
 * we're just interested in whether the entry exists or not
 */
typedef HASH_TABLE<ST*, BOOL> ST_HASH_TABLE;
static ST_HASH_TABLE *dynamic_list = NULL;

/***********************************************************************
 *
 * Return TRUE if given ST is for a local VLA (true automatic)
 * FALSE otherwise.
 *
 ***********************************************************************/
extern BOOL Is_VLA (ST* st) {
  if (st &&
      ST_sclass(st) == SCLASS_AUTO &&
      TY_kind(ST_type(st)) == KIND_POINTER &&
      TY_kind(TY_pointed(ST_type(st))) == KIND_ARRAY &&
      TY_size(TY_pointed(ST_type(st))) == 0) 
    return TRUE;
  return FALSE;
}


/***********************************************************************
 *
 * For block/star distributions.
 *
 ***********************************************************************/
void DISTR_DIM::Init_Block_Star (DISTRIBUTE_TYPE dt) {
  Is_True ((dt == DISTRIBUTE_BLOCK) || (dt == DISTRIBUTE_STAR),
           ("Distribute type must be BLOCK or STAR"));
  _distr_type = dt;
  return;
}

/***********************************************************************
 *
 * For cyclic-const distributions.
 *
 ***********************************************************************/
void DISTR_DIM::Init_Cyclic_Const (DISTRIBUTE_TYPE dt,
                                   INT64 const_val,
                                   WN* pwn) {
  Is_True (dt == DISTRIBUTE_CYCLIC_CONST,
           ("Distribute type must be CYCLIC_CONST"));
  _distr_type = dt;
  _chunksize.const_val = const_val;
  if (const_val < 1) {
    ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(pwn),
                 WN_pragmas[WN_PRAGMA_DISTRIBUTE].name,
                 "chunksize is 0 or -ve, assuming 1");
    _chunksize.const_val = 1;
  }
  return;
}

/***********************************************************************
 *
 * For cyclic-expr distributions.
 *
 ***********************************************************************/
void DISTR_DIM::Init_Cyclic_Expr (DISTRIBUTE_TYPE dt, WN* val_wn) {
  Is_True (dt == DISTRIBUTE_CYCLIC_EXPR,
           ("Distribute type must be CYCLIC_EXPR"));
  _distr_type = dt;
  _chunk_val_wn = val_wn;
  return;
}

/***********************************************************************
 *
 * Compare two distributions, return TRUE iff they are equal,
 * including the chunksize, if any.
 *
 ***********************************************************************/
BOOL DISTR_DIM::operator ==(const DISTR_DIM& test_dim) const
{
  if (Distr_Type() != test_dim.Distr_Type()) return FALSE;

  else if ((Distr_Type() == DISTRIBUTE_CYCLIC_CONST) &&
	   (Chunk_Const_Val() != test_dim.Chunk_Const_Val())) return FALSE;

  else if ((Distr_Type() == DISTRIBUTE_CYCLIC_EXPR) &&
	   (Tree_Equiv(Chunk_WN(), test_dim.Chunk_WN()))) return FALSE;

  return TRUE;
}

/***********************************************************************
 *
 * Return a WHIRL expression for the chunksize --- 
 * either an LDID for the symbol into which we have stored the value
 * of chunksize, or an OPR_CONST. 
 * This must be called after Convert_Expr_To_Symbol has been called.
 *
 ***********************************************************************/
WN* DISTR_DIM::Chunksize () {
  WN* ldid_wn = NULL;
  switch (_distr_type) {
  case DISTRIBUTE_CYCLIC_EXPR:
    Is_True (_chunksize.sym,
             ("DISTR_DIM::Chunksize called before evaluating chunksize expr"));
    // this automatically loads chunksize as I4 or I8
    ldid_wn = WN_CreateLdid (OPCODE_make_op(OPR_LDID, _chunksize.sym->Type,
                                            _chunksize.sym->Type),
                             _chunksize.sym->WN_Offset(),
                             _chunksize.sym->St(),
                             Be_Type_Tbl(_chunksize.sym->Type));
    Copy_alias_info (Alias_Mgr, _chunk_stid_wn, ldid_wn);
    Du_Mgr->Add_Def_Use (_chunk_stid_wn, ldid_wn);
    break;
  case DISTRIBUTE_CYCLIC_CONST:
    if (_chunksize.const_val <= INT32_MAX)
      ldid_wn = LWN_Make_Icon (MTYPE_I4, _chunksize.const_val);
    else ldid_wn = LWN_Make_Icon (MTYPE_I8, _chunksize.const_val);
    break;
  default:
    FmtAssert (FALSE, ("Chunksize called on illegal distribution"));
    break;
  }
  return ldid_wn;
}

void DISTR_DIM::Print (FILE* fp) const {
  fprintf (fp, "%s", 
           (_distr_type == DISTRIBUTE_STAR) ? "STAR" :
           (_distr_type == DISTRIBUTE_BLOCK) ? "BLOCK" :
           (_distr_type == DISTRIBUTE_CYCLIC_EXPR) ? "CYCLIC_VAR" :
           (_distr_type == DISTRIBUTE_CYCLIC_CONST) ? "CYCLIC_CONST" :
           "unknown");
  if (_distr_type == DISTRIBUTE_CYCLIC_EXPR) {
    fprintf (fp, " (");
    _chunksize.sym->Print(fp);
    fprintf (fp, ")");
  }
  if (_distr_type == DISTRIBUTE_CYCLIC_CONST) {
    fprintf (fp, " (%lld)", _chunksize.const_val);
  }
  fprintf (fp, "\n");
}


INT DISTR_ARRAY::Num_Distr_Dim () {
  INT count = 0;
  Is_True (_dinfo, ("Num_Distr_Dim: _dinfo is NULL\n"));
  for (INT i=0; i<_dinfo->Num_Dim(); i++)
    if (_dims[i].Distr_Type() != DISTRIBUTE_STAR) count++;
  return count;
}

/***********************************************************************
 *
 * Is this distribution equivalent to the test_dact in the specified dim?
 * The array bounds and the DISTR_DIMs must match.
 *
 * In addition, it is necessary that the two dacts have
 * (a) the same total number of distributed dimensions, and
 * (b) this dimension occuring in the same position amongst the list
 * of distributed dimensions.
 * This second set of conditions ensures that the two dimensions
 * get the same number of processors at runtime.
 *
 ***********************************************************************/
mBOOL 
DISTR_ARRAY::DACT_Equiv(DISTR_ARRAY *test_dact, INT32 my_dim, INT32 test_dim)
{
  FmtAssert(test_dact, ("DACT_Equiv: test_dact is NULL\n"));

  DISTR_INFO *test_dinfo = test_dact->Dinfo();
  Is_True(_dinfo && test_dinfo, ("DACT_Equiv: dinfo is NULL\n"));

  /* First check the number of distributed dimensions */
  if (Num_Distr_Dim() != test_dact->Num_Distr_Dim()) return FALSE;
  /* and the relative position of this dimension. */
  INT32 curr_pos=0, test_pos=0;
  INT i;
  for (i=0; i<=my_dim; i++) {
    DISTRIBUTE_TYPE dt = Get_Dim(i)->Distr_Type ();
    if (dt != DISTRIBUTE_STAR) curr_pos++;
  }
  for (i=0; i<=test_dim; i++) {
    DISTRIBUTE_TYPE dt = test_dact->Get_Dim(i)->Distr_Type ();
    if (dt != DISTRIBUTE_STAR) test_pos++;
  }
  if (curr_pos != test_pos) return FALSE;

  // now look at the onto clause
  if (Has_Onto() && !test_dact->Has_Onto()) return FALSE;
  if (!Has_Onto() && test_dact->Has_Onto()) return FALSE;
  if (Has_Onto()) {
    curr_pos = 0; test_pos = 0;
    for (i = 0; i<Num_Distr_Dim(); i++) {
      while (Get_Dim(curr_pos)->Distr_Type() == DISTRIBUTE_STAR)
        curr_pos++;
      while (test_dact->Get_Dim(test_pos)->Distr_Type() == DISTRIBUTE_STAR)
        test_pos++;
      if (Onto(curr_pos) != test_dact->Onto(test_pos)) return FALSE;
      curr_pos++;
      test_pos++;
    }
  }

  DISTR_DIM *curr_distr_dim = this->Get_Dim(my_dim);
  DISTR_DIM *test_distr_dim = test_dact->Get_Dim(test_dim);

  /* We now have sample array references for each array,
   * so now look at the two sample references and compare 
   * just the upper bound expressions.
   */
  WN* my_ubound   = Array_Size_WN(my_dim);
  WN* test_ubound = test_dact->Array_Size_WN(test_dim);
  mBOOL retval = (Tree_Equiv(my_ubound, test_ubound) &&
                  (*curr_distr_dim == *test_distr_dim));
  LWN_Delete_Tree (my_ubound);
  LWN_Delete_Tree (test_ubound);
  return retval;

/*
 * This is the previous scheme when we had to determine equality based
 * on the array TY entries -- by basically doing a Tree_Equiv on each
 * of lower-bound, upper-bound, and stride.
 * Now we have sample references, and the above code just needs to 
 * compare upper bounds.
 *
 *  TY_IDX curr_ty = _dinfo->Orig_TY();
 *  TY_IDX test_ty = test_dinfo->Orig_TY();
 *
 *  return ((Array_Bounds_Equiv(curr_ty, test_ty, my_dim, test_dim) &&
 *	  (*curr_distr_dim == *test_distr_dim)));
 */
}

/***********************************************************************
 *
 * Is this distribution equivalent to the test_dact in all dimensions?
 * The array bounds and the DISTR_DIMs must match.
 *
 ***********************************************************************/
mBOOL DISTR_ARRAY::DACT_Equiv(DISTR_ARRAY *test_dact)
{
  DISTR_INFO *test_dinfo = test_dact->Dinfo();
  Is_True(_dinfo && test_dinfo, ("DACT_Equiv: dinfo is NULL\n"));

  if (test_dinfo->Num_Dim() != _dinfo->Num_Dim()) return FALSE;

  for (INT i = 0; i < _dinfo->Num_Dim(); i++) {
    if (!DACT_Equiv(test_dact, i, i)) return FALSE;
  }

  return TRUE;
}


/***********************************************************************
 *
 * For each distribute_cyclic_expr distribution, generate code to evaluate 
 * the expression into a symbol, and replace the whirl tree in DISTR_DIM
 * with the SYMBOL.
 *
 ***********************************************************************/
void DISTR_ARRAY::Convert_Expr_To_Symbol () {
  INT ndims = _dinfo->Num_Dim();
  INT i;
  WN* first_wn = _first_pragma_wn;
  for (i=0; i<ndims; i++) {
    DISTR_DIM* dd = &(_dims[i]);
    if (dd->Distr_Type() != DISTRIBUTE_CYCLIC_EXPR) continue;

    ST* local_st;
    SYMBOL* sym;

    if (_dinfo->Small_Index()) {
      local_st = Create_Local_Variable("chunksize",
                                       MTYPE_I4,
                                       _dinfo->Array_ST(),
                                       i);
      sym = CXX_NEW (SYMBOL (local_st, 0, MTYPE_I4), LEGO_pool);
    }
    else {
      local_st = Create_Local_Variable("chunksize",
                                       MTYPE_I4,
                                       _dinfo->Array_ST(),
                                       i);
      sym = CXX_NEW (SYMBOL (local_st, 0, MTYPE_I8), LEGO_pool);
    }

    WN* chunk_wn = dd->Chunk_WN();
    if ((WN_operator(chunk_wn) == OPR_INTCONST) &&
        (WN_const_val(chunk_wn) == 0)) {

      /* do a runtime lookup of the chunksize */
      DISTR_INFO *dinfo = Dinfo();
      OPCODE call_op = OPCODE_make_op (OPR_CALL, MTYPE_V, MTYPE_V);

      WN *call_wn = WN_Create(call_op, 1);
      WN *array_load_wn = dinfo->Load_Distr_Array ();
      if (LNO_Use_Parm) {
        WN* parm_wn = WN_CreateParm (Pointer_type, array_load_wn,
                                     Be_Type_Tbl(Pointer_type),
                                     WN_PARM_BY_VALUE);
        LWN_Set_Parent (array_load_wn, parm_wn);
        array_load_wn = parm_wn;
      }
      WN_kid(call_wn, 0) = array_load_wn;
      WN_st_idx(call_wn) = ST_st_idx(distr_st_entries[HT_Top]);
      Set_Runtime_Call_Side_Effects (call_wn);
      LWN_Set_Parent(array_load_wn, call_wn);
      LWN_Insert_Block_Before (NULL, first_wn, call_wn);
      WN_linenum(call_wn) = LWN_Get_Linenum(first_wn);

      // Use the return value (dart*) as base for ILOAD of chunksize
      PREG_NUM rreg1, rreg2;
      ST* rst = Find_Return_Registers (Pointer_type, &rreg1, &rreg2);
      FmtAssert(rreg1 != 0 && rreg2 == 0, ("Bad pointer type ret regs"));
  
      //OPCODE     stop = OPCODE_make_op(OPR_STID, MTYPE_V, Pointer_type);
      //OPCODE     ldop = OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type);

      WN *ldid_wn = WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                                  Pointer_type,
                                                  Pointer_type),
                                   rreg1, rst, Be_Type_Tbl(Pointer_type));
      Create_alias (Alias_Mgr, ldid_wn);
      Du_Mgr->Add_Def_Use (call_wn, ldid_wn);
      OPCODE iload_op = OPCODE_make_op(OPR_ILOAD, MTYPE_I8, MTYPE_I8);
      WN* dart_k_wn = LWN_CreateIload
        (iload_op,
         dart_offset_distr_k+i*TY_size(distr_ty_entries[RT_dim_struct]),
         Be_Type_Tbl(MTYPE_I8),
         Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8)),
         ldid_wn);
      LWN_Parentize (dart_k_wn);
      WN* stid_wn = LWN_CreateStid(OPCODE_make_op(OPR_STID, MTYPE_V,
                                                  sym->Type),
                                   sym->WN_Offset(), sym->St(),
                                   Be_Type_Tbl(sym->Type),
                                   dart_k_wn);
      Create_local_alias (Alias_Mgr, stid_wn);
      LWN_Insert_Block_After (NULL, call_wn, stid_wn);
      dd->Evaluate_Chunk_Into_Symbol (sym, stid_wn);
      continue;
    }
    /* come here if regular chunksize */

    WN* expr_wn = LWN_Copy_Tree (dd->Chunk_WN());
    LWN_Copy_Def_Use (dd->Chunk_WN(), expr_wn, Du_Mgr);
    LWN_Parentize (expr_wn);
    WN* stid_wn = LWN_CreateStid (OPCODE_make_op(OPR_STID, MTYPE_V, sym->Type),
                                  sym->WN_Offset(), sym->St(),
                                  Be_Type_Tbl(sym->Type),
                                  expr_wn);
    Create_local_alias (Alias_Mgr, stid_wn);
    LWN_Insert_Block_Before (NULL, first_wn, stid_wn);
    dd->Evaluate_Chunk_Into_Symbol (sym, stid_wn);
  }
} /* DISTR_ARRAY::Convert_Expr_To_Symbol */

void DISTR_ARRAY::Print (FILE* fp) const {
  INT i;
  fprintf (fp, "Distribution: \n");
  for (i=0; i<_dinfo->Num_Dim(); i++) {
    _dims[i].Print (fp);
  }
}


/***********************************************************************
 *
 * Lookup up the variable in the symbol table
 *
 ***********************************************************************/
#ifdef _NEW_SYMTAB
static ST* Lookup_Variable(char *nm, INT stab)
{
  ST *st;
  INT i;

  FOREACH_SYMBOL(stab,st,i) {
    if ((ST_class(st) == CLASS_VAR) && (strcmp(ST_name(st), nm) == 0))
      return st;
  }

  return NULL;
}
#else
static ST* Lookup_Variable(char *nm, SYMTAB *stab)
{
  ST *st;

  FOR_ALL_SYMBOLS(stab, st) {
    if ((ST_class(st) == CLASS_VAR) && (strcmp(ST_name(st), nm) == 0))
      return st;
  }

  return NULL;
}
#endif

/***********************************************************************
 *
 * Create a global ST for Jennifer's variables.
 *
 ***********************************************************************/
static ST* Create_Global_Variable (const char* nm, TYPE_ID type, ST* array_st,
                                   INT32 i) {
  char *name;
  if (ST_base(array_st) != array_st && ST_isCommon(array_st)) {
    name = (char*) alloca (strlen(ST_name(ST_base(array_st))) +
                           strlen(ST_name(array_st)) + strlen(nm) + 10);
    sprintf (name, "_%s_%s_%s_%d", ST_name(ST_base(array_st)), 
             ST_name(array_st), nm, i);
  } else {
    name = (char*) alloca (strlen(ST_name(array_st)) + strlen(nm) + 10);
    sprintf (name, "_%s_%s_%d", ST_name(array_st), nm, i);
  }

  // See if the variable already exists
#ifdef _NEW_SYMTAB
  ST *st = Lookup_Variable(name, GLOBAL_SYMTAB);
#else
  ST *st = Lookup_Variable(name, Global_Symtab);
#endif
  if (st) return st;

  // Create a new variable
  st = New_ST(GLOBAL_SYMTAB);
  ST_Init (st,
           Save_Str(name),
           CLASS_VAR,
           ((ST_base_idx(array_st) != ST_st_idx(array_st)) ?
            // it's a partial split common
            SCLASS_COMMON :
            ST_sclass(array_st)),
           EXPORT_PREEMPTIBLE,
           Be_Type_Tbl(type));

  Allocate_Object (st);
  return st;
} /* Create_Global_Variable */

/***********************************************************************
 *
 * Create a local ST for Jennifer's variables.
 *
 ***********************************************************************/
static ST* Create_Local_Variable (const char* nm, TYPE_ID type, ST* array_st,
                                  INT32 i) {
  char *name;
  if (ST_base(array_st) != array_st) {
    name = (char*) alloca (strlen(ST_name(ST_base(array_st))) +
                           strlen(ST_name(array_st)) + strlen(nm) + 10);
    sprintf (name, "_%s_%s_%s_%d", ST_name(ST_base(array_st)), 
             ST_name(array_st), nm, i);
  } else {
    name = (char*) alloca (strlen(ST_name(array_st)) + strlen(nm) + 10);
    sprintf (name, "_%s_%s_%d", ST_name(array_st), nm, i);
  }

  // See if the variable already exists
  /* never do this for locals.
   * ST *st = Lookup_Variable(name, CURRENT_SYMTAB);
   * if (st) return st;
   */

  // Create a new variable
  ST* st;
  st = New_ST(CURRENT_SYMTAB);
  ST_Init (st,
           Save_Str(name),
           CLASS_VAR,
           SCLASS_AUTO,
           EXPORT_LOCAL,
           Be_Type_Tbl(type));
  return st;
} /* Create_Local_Variable */

/***********************************************************************
 *
 * Create a ST * for a DART, in the appropriate symtab (same as array_st).
 * If one already exists, then return that one.
 *
 ***********************************************************************/
static ST* New_DART (ST* array_st) {
  char *name;
  BOOL isglobal = ST_isGlobal(array_st);
  if (ST_isCommon(array_st)) {
    name = (char*) alloca (strlen(ST_name(ST_base(array_st))) +
                           strlen(ST_name(array_st)) + 10);
    sprintf (name, "_%s_%s_dart", ST_name(ST_base(array_st)), 
             ST_name(array_st));
  } else {
    char* tmps = ST_name(array_st);
    name = (char*) alloca (strlen(tmps) + 10);
    sprintf (name, "_%s_dart", tmps);
  }

  // See if the variable already exists
  /* never lookup for locals. */
  ST* dart_st;
  if (isglobal) {
#ifdef _NEW_SYMTAB
    dart_st = Lookup_Variable(name, 
                              isglobal ? GLOBAL_SYMTAB : CURRENT_SYMTAB);
#else
    dart_st = Lookup_Variable(name, 
                              isglobal ? Global_Symtab : Current_Symtab);
#endif
    if (dart_st) return dart_st;
  }

  // Create a new variable
  dart_st = New_ST(isglobal ? GLOBAL_SYMTAB : CURRENT_SYMTAB);
  if (!isglobal) {
    ST_Init (dart_st,
             Save_Str(name),
             CLASS_VAR,
             SCLASS_AUTO,
             EXPORT_LOCAL,
             DART_ptr_TY);
  } else {
    ST_Init (dart_st,
             Save_Str(name),
             CLASS_VAR,
             ((ST_base_idx(array_st) != ST_st_idx(array_st)) ?
              // it's a partial split common
              SCLASS_COMMON : 
              ST_sclass(array_st)),
             EXPORT_PREEMPTIBLE,
             DART_ptr_TY);
    Allocate_Object(dart_st);
  }

  /* TODO: check the sclass -- may change if global */
  Set_ST_pt_to_unique_mem(dart_st);
  Set_ST_pt_to_compiler_generated_mem(dart_st);
  return dart_st;
} /* New_DART */

/***********************************************************************
 *
 * Create a common block for book-keeping variables of an array
 * in a common block. Return the ST entry for the common block.
 *
 ***********************************************************************/
#ifdef _NEW_SYMTAB
static ST* Create_Common_Block (ST* array_st, INT numdim) {
  Is_True (ST_isCommon(array_st),
           ("Create_Common_Block called with non-COMMON ST (%s)",
            ST_name(array_st)));
  char *name;
  name = CXX_NEW_ARRAY (char, (strlen(ST_name(ST_base(array_st)))+
                               strlen(ST_name(array_st))+11),
                        LEGO_pool);
  
  ST *base_st = ST_base(array_st);
  Is_True (ST_class(base_st) == CLASS_VAR,
           ("Base of common (%s) not CLASS_VAR", ST_name(base_st)));
  Is_True (ST_sclass(base_st) == SCLASS_COMMON,
           ("Base of common (%s) not SCLASS_COMMON", ST_name(base_st)));
  Is_True (ST_level(base_st) == GLOBAL_SYMTAB && 
           ST_level(base_st) == GLOBAL_SYMTAB,
           ("COMMON Array %s or base %s not in global symtab",
            ST_name(array_st), ST_name(base_st)));

  /* Declare type of the array for each of dimsize and numprocs */
  TY_IDX array_ty_idx;
  TY& array_ty          = New_TY(array_ty_idx);
  TY_Init (array_ty,
           numdim*TY_size(Be_Type_Tbl(MTYPE_I8)),
           KIND_ARRAY,
           MTYPE_UNKNOWN,
           Save_Str ("array_I8"));
  
  ARB_HANDLE arb = New_ARB ();
  ARB_Init(arb,0,numdim-1,1);
  Set_ARB_first_dimen (arb);
  Set_ARB_last_dimen (arb);
  Set_TY_etype(array_ty,Be_Type_Tbl(MTYPE_I8));

  Set_TY_align(array_ty_idx,8);
  Set_TY_arb(array_ty,arb);

  /* Create a struct for the common */
  FLD_HANDLE fld = New_FLD();
  FLD_HANDLE first_fld = fld;
  FLD_Init(fld,Save_Str("dart"),DART_ptr_TY,0);



  TY_IDX element_type  = TY_AR_etype(Lego_Get_Array_Type(array_st));
  TY_IDX array_type    =
    Make_Pointer_Type(Make_Pointer_Type(element_type));
  Set_TY_ptr_as_array(array_type);
  Set_TY_ptr_as_array(TY_pointed(array_type));

  fld = New_FLD ();
  FLD_Init(fld,Save_Str("ST_name(array_st)"),array_type,
           TY_size(Be_Type_Tbl(MTYPE_I8)));

  fld = New_FLD ();
  FLD_Init(fld,Save_Str("dimsize_array"),array_ty_idx,
           2*TY_size(Be_Type_Tbl(MTYPE_I8)));

  fld = New_FLD ();
  FLD_Init(fld,Save_Str("numprocs_array"),array_ty_idx,
           TY_size(array_ty) + 2*TY_size(Be_Type_Tbl(MTYPE_I8)));
  Set_FLD_last_field(fld);
  
  /* Create a struct type with the above fields */
  char ty_name[64]; sprintf (ty_name, "BKblock_ty_%d", numdim);
  TY_IDX ty_idx;
  TY& struct_ty = New_TY(ty_idx);
  TY_Init (struct_ty,
           2*TY_size(array_ty)+2*TY_size(Be_Type_Tbl(MTYPE_I8)),
           KIND_STRUCT,
           MTYPE_M,
           Save_Str(ty_name));
  Set_TY_fld(struct_ty,first_fld);
  Set_TY_align(ty_idx,8);
                           

  /* Now create the ST entry */
  sprintf (name, "_%s_%s_BKblock", ST_name(ST_base(array_st)),
           ST_name(array_st));
  ST *st        = New_ST(GLOBAL_SYMTAB);
  ST_Init (st,
           Save_Str(name),
           CLASS_VAR,
           SCLASS_COMMON,
           EXPORT_PREEMPTIBLE,
           ty_idx);
  Set_ST_base(st,st);
  // the variables in this common block are initialized at program startup
  // and never changed for the duration of the program. So mark them as
  // such -- wopt does better optimizations with this bit.
  Set_ST_is_const_var(st);
  Set_BE_ST_unknown_const(st);
  
  CXX_DELETE_ARRAY (name, LEGO_pool);
  return st;
} /* Create_Common_Block */

#else  // _NEW_SYMTAB

static ST* Create_Common_Block (ST* array_st, INT numdim) {
  Is_True (ST_isCommon(array_st),
           ("Create_Common_Block called with non-COMMON ST (%s)",
            ST_name(array_st)));
  char *name;
  name = CXX_NEW_ARRAY (char, (strlen(ST_name(ST_base(array_st)))+
                               strlen(ST_name(array_st))+11),
                        LEGO_pool);
  
  ST *base_st = ST_base(array_st);
  Is_True (ST_class(base_st) == CLASS_VAR,
           ("Base of common (%s) not CLASS_VAR", ST_name(base_st)));
  Is_True (ST_sclass(base_st) == SCLASS_COMMON,
           ("Base of common (%s) not SCLASS_COMMON", ST_name(base_st)));
  Is_True (!ST_is_global(base_st) && !ST_is_global(array_st),
           ("Array %s or base %s not in local ST",
            ST_name(array_st), ST_name(base_st)));

  /* Declare type of the array for each of dimsize and numprocs */
  TY_IDX array_ty          = New_TY(FALSE);
  TY_kind(array_ty)     = KIND_ARRAY;
  TY_btype(array_ty)    = MTYPE_M;

  ARI *ari = New_ARI (1, FALSE);
  ARI_etype(ari)        = Be_Type_Tbl(MTYPE_I8);
  ARI_const_zofst(ari)  = TRUE;
  ARI_zofst_val(ari)    = 0;
  ARB_const_lbnd(ARI_bnd(ari,0))    = TRUE;
  ARB_lbnd_val(ARI_bnd(ari,0))      = 0;
  ARB_const_ubnd(ARI_bnd(ari,0))    = TRUE;
  ARB_ubnd_val(ARI_bnd(ari,0))      = numdim-1;
  ARB_const_stride(ARI_bnd(ari,0))  = TRUE;
  ARB_stride_val(ARI_bnd(ari,0))    = 1;
  TY_size(array_ty)     = numdim*TY_size(Be_Type_Tbl(MTYPE_I8));
  TY_align(array_ty)    = 8;
  TY_name(array_ty)     = Save_Str ("array_I8");
  TY_arinfo(array_ty)   = ari;
  Enter_TY (array_ty);

  /* Create a struct for the common */
  FLD *field, *next;
  field = New_FLD (4, FALSE);
  FLD_name(field)   = Save_Str("dart");
  FLD_type(field)   = DART_ptr_TY;
  FLD_ofst(field)   = 0;
  FLD_flags(field)  = 0;

  next = FLD_next(field);
  TY_IDX element_type  = TY_AR_etype(Lego_Get_Array_Type(array_st));
  TY_IDX array_type    =
    Make_Pointer_Type(Make_Pointer_Type(element_type));
  Set_TY_ptr_as_array(array_type);
  Set_TY_ptr_as_array(TY_pointed(array_type));
  FLD_type(next)    = array_type;
  FLD_name(next)    = Save_Str(ST_name(array_st));
  // Ordinarily the following is correct, but we want to always reserve
  // 8 bytes regardless of the size of the pointer -- 4 (-n32) or 8 (-64)
  // FLD_ofst(next)    = TY_size(DART_ptr_TY);
  FLD_ofst(next)    = TY_size(Be_Type_Tbl(MTYPE_I8));
  FLD_flags(next)   = 0;

  next = FLD_next(next);
  FLD_name(next)    = Save_Str("dimsize_array");
  FLD_type(next)    = array_ty;
  // again, reserve 8 bytes regardless of pointer size
  FLD_ofst(next)    = 2*TY_size(Be_Type_Tbl(MTYPE_I8));
  FLD_flags(next)   = 0;
  
  next = FLD_next(next);
  FLD_name(next)    = Save_Str("numprocs_array");
  FLD_type(next)    = array_ty;
  FLD_ofst(next)    = TY_size(array_ty) + 2*TY_size(Be_Type_Tbl(MTYPE_I8));
  FLD_flags(next)   = 0;
  
  /* Create a struct type with the above fields */
  TY_IDX struct_ty = New_TY(FALSE);
  TY_kind(struct_ty)    = KIND_STRUCT;
  TY_btype(struct_ty)   = MTYPE_M;
  char ty_name[64]; sprintf (ty_name, "BKblock_ty_%d", numdim);
  TY_name(struct_ty)    = Save_Str(ty_name);
  TY_flist(struct_ty)   = field;
  TY_size(struct_ty)    = 2*TY_size(array_ty)+2*TY_size(Be_Type_Tbl(MTYPE_I8));
  TY_align(struct_ty)   = 8;
  Enter_TY(struct_ty);
                           

  /* Now create the ST entry */
  sprintf (name, "_%s_%s_BKblock", ST_name(ST_base(array_st)),
           ST_name(array_st));
  ST *st        = New_ST(CURRENT_SYMTAB);
  ST_Init (st,
           Save_Str(name),
           CLASS_VAR,
           SCLASS_COMMON,
           EXPORT_PREEMPTIBLE,
           struct_ty);
  ST_base(st)   = st;
  // the variables in this common block are initialized at program startup
  // and never changed for the duration of the program. So mark them as
  // such -- wopt does better optimizations with this bit.
  Set_ST_is_const_var(st);
  
  CXX_DELETE_ARRAY (name, LEGO_pool);
  return st;
} /* Create_Common_Block */
#endif


/***********************************************************************
 *
 * Are the bounds of the two array STs equivalent in the given dimension?
 *
 ***********************************************************************/
mBOOL Upper_Bounds_Equiv(TY_IDX array_ty1, TY_IDX array_ty2, 
                         INT32 dim1, INT32 dim2)
{

  if (TY_AR_const_ubnd(array_ty1, dim1) && 
      TY_AR_const_ubnd(array_ty2, dim2)) {

    return (TY_AR_ubnd_val(array_ty1, dim1) ==
            TY_AR_ubnd_val(array_ty2, dim2));

  } else if (!TY_AR_const_ubnd(array_ty1, dim1) && 
             !TY_AR_const_ubnd(array_ty2, dim2)) {

    return (TY_AR_ubnd_var(array_ty1, dim1) ==
            TY_AR_ubnd_var(array_ty2, dim2));
  } 

  return FALSE;
}


mBOOL Lower_Bounds_Equiv(TY_IDX array_ty1, TY_IDX array_ty2, INT32 dim1, 
							INT32 dim2)
{

  if (TY_AR_const_lbnd(array_ty1, dim1) && 
      TY_AR_const_lbnd(array_ty2, dim2)) {

    return (TY_AR_lbnd_val(array_ty1, dim1) ==
            TY_AR_lbnd_val(array_ty2, dim2));

  } else if (!TY_AR_const_lbnd(array_ty1, dim1) && 
             !TY_AR_const_lbnd(array_ty2, dim2)) {

    return (TY_AR_lbnd_var(array_ty1, dim1) ==
            TY_AR_lbnd_var(array_ty2, dim2));
  } 

  return FALSE;
}

mBOOL Strides_Equiv(TY_IDX array_ty1, TY_IDX array_ty2, INT32 dim1, INT32 dim2)
{

  if (TY_AR_const_stride(array_ty1, dim1) && 
      TY_AR_const_stride(array_ty2, dim2)) {

    return (TY_AR_stride_val(array_ty1,dim1) ==
            TY_AR_stride_val(array_ty2,dim2));

  } else if (!TY_AR_const_stride(array_ty1, dim1) && 
             !TY_AR_const_stride(array_ty2, dim2)) {

    return (TY_AR_stride_var(array_ty1, dim1) ==
            TY_AR_stride_var(array_ty2, dim2));
  } 

  return FALSE;
}


mBOOL Array_Bounds_Equiv(TY_IDX array_ty1, TY_IDX array_ty2, INT32 dim1, 
								INT32 dim2)
{
  if (array_ty1 == array_ty2) return TRUE;

  return (Lower_Bounds_Equiv(array_ty1, array_ty2, dim1, dim2) &&
	  Upper_Bounds_Equiv(array_ty1, array_ty2, dim1, dim2) &&
	  Strides_Equiv(array_ty1, array_ty2, dim1, dim2));
  
} /* Array_Bounds_Equiv */


DISTR_INFO::DISTR_INFO (mBOOL isreshaped, INT numdim, SYMBOL* array) {
  INT i;
  _isreshaped = isreshaped;
  // Invalidate alias info based on this ST
  if (_isreshaped) {
    Note_Invalid_Based_Symbol(array->St());
  }
  _isdynamic = FALSE;
  _num_dim = numdim;
  _array = array;
  _dart_st = NULL;
  _dart_ptr_alias_wn = NULL;
  _dart_wn.Set_Mem_Pool(LEGO_pool);

  _ec_dart_def_wn = NULL;
  _ec_dart_ptr_wn = NULL;

  _array_alias_wn = NULL;
  _array_ptr_alias_wn = NULL;
  _array_def_wn = NULL;

  _dimsize = CXX_NEW_ARRAY (SYMBOL*, numdim, LEGO_pool);
  _numprocs = CXX_NEW_ARRAY (SYMBOL*, numdim, LEGO_pool);
  _array_common = NULL;
  _array_common_alias_wn = NULL;

  ST* array_st = array->St();
  _orig_ty = Lego_Get_Array_Type(array_st);

  _small_index = FALSE;

  // determine if we can use small indices
  INT small_index_size = TY_size(Be_Type_Tbl(MTYPE_U4));
  if (TY_size(Be_Type_Tbl(Pointer_type)) <= small_index_size) {
    // -n32 compilation
    _small_index = TRUE;
  }
  else {
    // -64 compilation -- look at the dimension sizes
    INT ndims = TY_AR_ndims(_orig_ty);
    INT i;
    for (i=0; i<ndims; i++) {

      // constant bounds
      if (TY_AR_const_lbnd(_orig_ty, i) && TY_AR_const_ubnd(_orig_ty, i)) {
        INT64 size = TY_AR_ubnd_val(_orig_ty,i) - TY_AR_lbnd_val(_orig_ty,i);
        if (size > INT32_MAX) break;
        else continue;
      }

      // non-constant bounds -- look at the type of the WHIRL tree
      if (!TY_AR_const_lbnd(_orig_ty, i)) {
#ifdef _NEW_SYMTAB
        ST* dim_var = ST_ptr(TY_AR_lbnd_var(_orig_ty, i));
        TYPE_ID dim_type = ST_type(dim_var);
#else
	WN *dim_wn = TY_AR_lbnd_tree(_orig_ty,i);
        TY *dim_type = Be_Type_Tbl(WN_rtype(dim_wn));
#endif
        if (TY_size(dim_type) <= small_index_size)
          continue;
        else break;
      }
      if (!TY_AR_const_ubnd(_orig_ty, i)) {
#ifdef _NEW_SYMTAB
        ST* dim_var = ST_ptr(TY_AR_ubnd_var(_orig_ty, i));
        TYPE_ID dim_type = ST_type(dim_var);
#else
        WN* dim_wn = TY_AR_ubnd_tree(_orig_ty, i);
        TY* dim_type = Be_Type_Tbl(WN_rtype(dim_wn));
#endif
        if (TY_size(dim_type) <= small_index_size)
          continue;
        else break;
      }
    }

    if (i == ndims) _small_index = TRUE;
  }

  _hoist_proc_array = NULL;
  _hoist_proc_index = -1;
  _hoist_proc_alias = NULL;
  _buddy_dinfo = NULL;

  // if array is a common, then create a common block for these variables
  ST* base_st = NULL;
  if (ST_isCommon(array_st)) {
    base_st = Create_Common_Block (array_st, numdim);
    // also create dart_st
    char name[64];
    char* tmps = ST_name(array_st);
    sprintf (name, "_%s_dart", ((strlen(tmps) < 50) ? tmps : "LongName"));
    
#ifdef _NEW_SYMTAB
    _dart_st = New_ST(GLOBAL_SYMTAB);
#else
    _dart_st = New_ST(CURRENT_SYMTAB);
#endif
    ST_Init (_dart_st,
             Save_Str(name),
             CLASS_VAR,
             SCLASS_COMMON,
             EXPORT_LOCAL,
             DART_ptr_TY);
    Set_ST_base_idx(_dart_st,ST_st_idx(base_st));
    Set_ST_ofst(_dart_st,0);
    Set_ST_pt_to_unique_mem(_dart_st);
    Set_ST_pt_to_compiler_generated_mem(_dart_st);

    if (_isreshaped) {
      TY_IDX element_type  = TY_AR_etype(Lego_Get_Array_Type(array_st));
      TY_IDX array_type    =
       Make_Pointer_Type(Make_Pointer_Type(element_type));
      Set_TY_ptr_as_array(array_type);
      Set_TY_ptr_as_array(TY_pointed(array_type));
      sprintf (name, "_%s_array", ((strlen(tmps) < 50) ? tmps : "LongName"));

#ifdef _NEW_SYMTAB    
      ST* array_common_st = New_ST(GLOBAL_SYMTAB);
#else
      ST* array_common_st = New_ST(CURRENT_SYMTAB);
#endif
      ST_Init (array_common_st,
               Save_Str(name),
               CLASS_VAR,
               SCLASS_COMMON,
               EXPORT_LOCAL,
               array_type);
      Set_ST_base_idx(array_common_st,ST_st_idx(base_st));
      Set_ST_ofst(array_common_st,TY_size(Be_Type_Tbl(MTYPE_I8)));
      Set_ST_pt_to_unique_mem(array_common_st);
      Set_ST_pt_to_compiler_generated_mem(array_common_st);
      Set_ST_is_const_var(array_common_st);
      Set_BE_ST_unknown_const(array_common_st);

      _array_common = CXX_NEW (SYMBOL(array_common_st, 0, 
                                      Pointer_type), LEGO_pool);
      Set_ST_is_const_var(_dart_st);
      Set_BE_ST_unknown_const(_dart_st);
    }
  }
  else _dart_st = New_DART(array_st);

  // Allocate dimsize and numprocs STs
  for (i=0; i<numdim; i++) {
    switch (ST_Var_Kind(array_st)) {
    case var_global:
    {
      ST* numprocs_st = Create_Global_Variable ("numprocs", MTYPE_I8,
                                                array_st, i);
      SYMBOL *numprocs_sym = CXX_NEW(SYMBOL(numprocs_st,0,MTYPE_I8),
                                     LEGO_pool);
      _numprocs[i] = numprocs_sym;
      ST* dimsize_st = Create_Global_Variable ("dimsize", MTYPE_I8,
                                               array_st, i);
      SYMBOL *dimsize_sym = CXX_NEW (SYMBOL(dimsize_st,0, MTYPE_I8),
                                     LEGO_pool);
      _dimsize[i] = dimsize_sym;
      break;
    }
    case var_local:
    case var_formal:
    {
      /* Need a real ST again so that these variables are SHARED
       * within MP regions.
       */
      ST* numprocs_st = Create_Local_Variable ("numprocs", MTYPE_I8,
                                               array_st, i);
      SYMBOL *numprocs_sym = CXX_NEW(SYMBOL(numprocs_st,0,MTYPE_I8),
                                     LEGO_pool);
      _numprocs[i] = numprocs_sym;
      ST* dimsize_st = Create_Local_Variable ("dimsize", MTYPE_I8,
                                              array_st, i);
      SYMBOL *dimsize_sym = CXX_NEW (SYMBOL(dimsize_st,0, MTYPE_I8),
                                     LEGO_pool);
      _dimsize[i] = dimsize_sym;
      break;
    }
    case var_common:
    {
      char name[64];
      sprintf (name, "_%s_dart_dimsize_%d", ((strlen(ST_name(array_st)) < 40)
                                             ? ST_name(array_st) :
                                             "LongName"), i);
#ifdef _NEW_SYMTAB
      ST* st = New_ST(GLOBAL_SYMTAB);
#else
      ST* st = New_ST(CURRENT_SYMTAB);
#endif
      ST_Init (st,
               Save_Str(name),
               CLASS_VAR,
               SCLASS_COMMON,
               EXPORT_LOCAL,
               Be_Type_Tbl(MTYPE_I8));
      Set_ST_base(st,base_st);
      Set_ST_ofst(st,(2*TY_size(Be_Type_Tbl(MTYPE_I8)) +
                       (i*TY_size(Be_Type_Tbl(MTYPE_I8)))));
      Set_ST_is_const_var(st);
      Set_BE_ST_unknown_const(st);
      _dimsize[i] = CXX_NEW(SYMBOL(st, 0, MTYPE_I8), LEGO_pool);

      sprintf (name, "_%s_dart_numprocs_%d",((strlen(ST_name(array_st)) < 40)
                                             ? ST_name(array_st) :
                                             "LongName"), i);
#ifdef _NEW_SYMTAB
      st = New_ST(GLOBAL_SYMTAB);
#else
      st = New_ST(CURRENT_SYMTAB);
#endif
      ST_Init (st,
               Save_Str(name),
               CLASS_VAR,
               SCLASS_COMMON,
               EXPORT_LOCAL,
               Be_Type_Tbl(MTYPE_I8));
      Set_ST_base(st,base_st);
      Set_ST_ofst(st,(2*TY_size(Be_Type_Tbl(MTYPE_I8)) +
                       ((numdim+i)*TY_size(Be_Type_Tbl(MTYPE_I8)))));
      Set_ST_is_const_var(st);
      Set_BE_ST_unknown_const(st);
      _numprocs[i] = CXX_NEW(SYMBOL(st, 0, MTYPE_I8), LEGO_pool);

      break;
    }
    }
  }

  _dimsize_wn = CXX_NEW_ARRAY (DYN_ARRAY<WN*>, numdim, LEGO_pool);
  _numprocs_wn = CXX_NEW_ARRAY (DYN_ARRAY<WN*>, numdim, LEGO_pool);
  for (i=0; i<numdim; i++) {
    _dimsize_wn[i].Set_Mem_Pool(LEGO_pool);
    _numprocs_wn[i].Set_Mem_Pool(LEGO_pool);
  }

  _dact_da.Set_Mem_Pool (LEGO_pool);
  _redistr_da.Set_Mem_Pool (LEGO_pool);
  _gen_redistr_da.Set_Mem_Pool (LEGO_pool);
}

DISTR_INFO::~DISTR_INFO () {
  INT i;

  ST* array_st = _array->St();
  for (i=0; i<_num_dim; i++) {
    Is_True (_dimsize[i], ("_dimsize is NULL"));
    CXX_DELETE (_dimsize[i], LEGO_pool);
    Is_True (_numprocs[i], ("_numprocs is NULL"));
    CXX_DELETE (_numprocs[i], LEGO_pool);
  }
  CXX_DELETE_ARRAY (_dimsize, LEGO_pool);
  CXX_DELETE_ARRAY (_numprocs, LEGO_pool);

  /* Set the dimension of hoist_proc_array */
  if (_hoist_proc_array) {
    FmtAssert (_hoist_proc_index >= 0,
               ("Hoist-proc-array has zero elements\n"));
    TY_IDX ty_idx = ST_type(_hoist_proc_array);
    Set_TY_size(ty_idx,(_hoist_proc_index+1) * TY_size(TY_AR_etype(ty_idx)));
    Set_TY_AR_ubnd_val(ty_idx,TY_AR_ndims(ty_idx)-1,_hoist_proc_index);
  }

  for (i=0; i<_num_dim; i++) {
    /* Delete the WHIRL tree, if ldid, since that must be a copy */
    WN* wn;
    if (_dimsize_wn[i].Elements() == 1) {
      wn = _dimsize_wn[i][0];
      if (WN_operator(wn) == OPR_LDID)
        LWN_Delete_Tree (wn);
    }
    if (_numprocs_wn[i].Elements() == 1) {
      wn = _numprocs_wn[i][0];
      if (WN_operator(wn) == OPR_LDID)
        LWN_Delete_Tree (wn);
    }
  }
  if (_array_alias_wn) LWN_Delete_Tree(_array_alias_wn);
  if (_array_ptr_alias_wn) LWN_Delete_Tree(_array_ptr_alias_wn);
  CXX_DELETE_ARRAY (_dimsize_wn, LEGO_pool);
  CXX_DELETE_ARRAY (_numprocs_wn, LEGO_pool);

  CXX_DELETE (_array, LEGO_pool);
  for (i=0; i<_dact_da.Elements(); i++) 
    CXX_DELETE (_dact_da[i], LEGO_pool);
  for (i=0; i<_redistr_da.Elements(); i++) 
    CXX_DELETE (_redistr_da[i], LEGO_pool);
  for (i=0; i<_gen_redistr_da.Elements(); i++) 
    CXX_DELETE (_gen_redistr_da[i], LEGO_pool);
}

void DISTR_INFO::Hoist_Proc_Init (TYPE_ID type) {
  if (_hoist_proc_array == NULL) {
    Is_True (_hoist_proc_index == -1,
             ("Hoist-array is NULL, but index is not 0"));
    char *name = (char*) alloca (strlen(ST_name(Array_ST()))+10);
    sprintf (name, "$%s_hoist", ST_name(Array_ST()));
    _hoist_proc_array = Create_Local_Array_ST (name, Be_Type_Tbl(type), 1);
  }
}

/***********************************************************************
 *
 * Called each time an ldid_wn is generated.
 * Updates alias info and def-use info as appropriate.
 *
 ***********************************************************************/
void DISTR_INFO::Numprocs_Ldid (INT dim, WN* ldid_wn) {
  if (_numprocs_wn[dim].Elements() == 0) {
    // first ldid. Make a copy, since all the rest of LNO happens
    // between now and lego-gen, and this ldid can easily be deleted..
    if (ST_isLocal(WN_st(ldid_wn)))
      Create_local_alias (Alias_Mgr, ldid_wn);
    else Create_global_alias (Alias_Mgr, WN_st(ldid_wn), ldid_wn, NULL);
    WN* copy_wn = LWN_Copy_Tree(ldid_wn);
    _numprocs_wn[dim][_numprocs_wn[dim].Newidx()] = copy_wn;
    Copy_alias_info (Alias_Mgr, ldid_wn, copy_wn);
    Du_Mgr->Add_Def_Use (Current_Func_Node, ldid_wn);
    return;
  }

  WN* wn = _numprocs_wn[dim][_numprocs_wn[dim].Lastidx()];
  if (WN_operator(wn) == OPR_LDID) {
    // last one was an LDID, must be the only one
    FmtAssert (_numprocs_wn[dim].Elements() == 1,
               ("Numprocs_Ldid: why are we storing more than 1 ldid?"));
    Copy_alias_info (Alias_Mgr, wn, ldid_wn);
    Du_Mgr->Add_Def_Use (Current_Func_Node, ldid_wn);
    return;
  }

  Copy_alias_info (Alias_Mgr, wn, ldid_wn);
  // Must be all STIDs
  for (INT i=0; i<_numprocs_wn[dim].Elements(); i++) {
    wn = _numprocs_wn[dim][i];
    FmtAssert (WN_operator(wn) == OPR_STID,
               ("Numprocs_Ldid: stored wn neither ldid nor stid"));
    Du_Mgr->Add_Def_Use (wn, ldid_wn);
  }
  if (!ST_isLocal(_numprocs[dim]->St())) {
    // global variable, also add incoming def
    Du_Mgr->Add_Def_Use (Current_Func_Node, ldid_wn);
  }
}

/***********************************************************************
 *
 * Called each time an stid_wn is generated.
 * Update alias info and def-use info appropriately.
 *
 ***********************************************************************/
void DISTR_INFO::Numprocs_Stid (INT dim, WN* stid_wn) {
  if (_numprocs_wn[dim].Elements() == 0) {
    // nothing has happened yet
    _numprocs_wn[dim][_numprocs_wn[dim].Newidx()] = stid_wn;
    if (ST_isLocal(WN_st(stid_wn)))
      Create_local_alias (Alias_Mgr, stid_wn);
    else Create_global_alias (Alias_Mgr, WN_st(stid_wn), stid_wn, NULL);
    return;
  }
  
  WN* wn = _numprocs_wn[dim][_numprocs_wn[dim].Lastidx()];
  if (WN_operator(wn) == OPR_LDID) {
    // this is the first stid
    FmtAssert (_numprocs_wn[dim].Elements() == 1,
               ("Numprocs_Stid: why are we storing more than 1 ldid?"));
    Copy_alias_info (Alias_Mgr, wn, stid_wn);
    /* The ldid was a copy-tree, so we can delete it now */
    LWN_Delete_Tree (wn);
    _numprocs_wn[dim][_numprocs_wn[dim].Lastidx()] = stid_wn;
    USE_LIST* use_list = Du_Mgr->Du_Get_Use(Current_Func_Node);
    USE_LIST_ITER use_iter(use_list);
    DU_NODE* node;
    // walk all the uses from the Current_Func_Node, and replace
    // their defs to be stid_wn
    for (node = use_iter.First(); !use_iter.Is_Empty();
                                  node = use_iter.Next()) {
      WN* uwn = node->Wn();
      if (WN_operator(uwn) == OPR_LDID) {
        SYMBOL sym(uwn);
        if (sym == *(_numprocs[dim])) {
          // use of this ST
          if (!ST_isLocal(_numprocs[dim]->St())) 
            // local variable, delete global def
            Du_Mgr->Delete_Def_Use (Current_Func_Node, uwn);
          Du_Mgr->Add_Def_Use (stid_wn, uwn);
          Du_Mgr->Ud_Get_Def(uwn)->Set_loop_stmt(NULL);
        }
      }
    }
    return;
  }
  
  FmtAssert (WN_operator(wn) == OPR_STID,
             ("Numprocs_Stid: Expected stored STID"));
  _numprocs_wn[dim][_numprocs_wn[dim].Newidx()] = stid_wn;
  Copy_alias_info (Alias_Mgr, wn, stid_wn);
  USE_LIST* use_list = Du_Mgr->Du_Get_Use(wn);
  USE_LIST_ITER use_iter(use_list);
  DU_NODE* node;
  // walk all the uses from wn, and add a def from stid_wn
  for (node = use_iter.First(); !use_iter.Is_Empty();
                                node = use_iter.Next()) {
    WN* uwn = node->Wn();
    if (WN_operator(uwn) == OPR_LDID) {
      SYMBOL sym(uwn);
      if (sym == *(_numprocs[dim])) {
        // use of this ST
        Du_Mgr->Add_Def_Use (stid_wn, uwn);
        Du_Mgr->Ud_Get_Def(uwn)->Set_loop_stmt(NULL);
      }
    }
  }
} /* DISTR_INFO::Numprocs_Stid */

/***********************************************************************
 *
 * Return an ldid for the numprocs variable of the given dimension, "i".
 *
 ***********************************************************************/
WN* DISTR_INFO::Numprocs (INT dim) {
  if (_buddy_dinfo) return _buddy_dinfo->Numprocs(dim);

  SYMBOL* numprocs = _numprocs[dim];
  WN* ldid_wn = NULL;

  Is_True(numprocs, ("Numprocs is NULL ST(%s)\n", ST_name(Array_ST())));

  // this can always be an I4 load since the number of processors
  // is always within 32-bits
  OPCODE ldid_op = OPCODE_make_op(OPR_LDID, MTYPE_I4, MTYPE_I8);
  ldid_wn = WN_CreateLdid (ldid_op, numprocs->WN_Offset(),
                           numprocs->St(), Be_Type_Tbl(MTYPE_I4));
  Numprocs_Ldid(dim, ldid_wn);
  return ldid_wn;
}

/***********************************************************************
 *
 * Return an ldid for the numprocs variable of the given dimension, "i".
 * This routine is distinct from the above routine in that it does
 * a runtime lookup of the numprocs.
 *
 * Any statements generated are returned in the block_wn.
 *
 ***********************************************************************/
extern WN* Numprocs (ST* array_st, INT dim, BOOL inside_loop, WN** block_wn) {

  OPCODE call_op = OPCODE_make_op (OPR_CALL, MTYPE_V, MTYPE_V);
  WN *call_wn = WN_Create(call_op, 1);
  WN* array_load_wn = Load_Distr_Array (array_st);
  if (LNO_Use_Parm) {
    WN* parm_wn = WN_CreateParm (Pointer_type, array_load_wn,
				 Be_Type_Tbl(Pointer_type),
				 WN_PARM_BY_VALUE);
    LWN_Set_Parent (array_load_wn, parm_wn);
    array_load_wn = parm_wn;
  }
  WN_kid(call_wn, 0) = array_load_wn;
  WN_st_idx(call_wn) = ST_st_idx(ST_is_reshaped(array_st) ? 
                                 distr_st_entries[HT_Top] :
                                 distr_st_entries[HT_Check]);
  Set_Runtime_Call_Side_Effects (call_wn);
  LWN_Set_Parent(array_load_wn, call_wn);
  if (inside_loop)
    Array_Dependence_Graph->Add_Vertex(call_wn); 

  // Generate code to store the return values into dart
  PREG_NUM rreg1, rreg2;
  ST* rst = Find_Return_Registers (Pointer_type, &rreg1, &rreg2);
  FmtAssert(rreg1 != 0 && rreg2 == 0, ("Bad I8 type ret regs"));
  
  //OPCODE        stop = OPCODE_make_op(OPR_STID, MTYPE_V, Pointer_type);
  //OPCODE        ldop = OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type);

  WN *ldid_wn = WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                              Pointer_type,
                                              Pointer_type),
                               rreg1, rst, Be_Type_Tbl(Pointer_type));
  Create_alias (Alias_Mgr, ldid_wn); // just a register
  Du_Mgr->Add_Def_Use (call_wn, ldid_wn);
  SYMBOL* preg_sym = CXX_NEW (SYMBOL(Create_Preg_Symbol("$local_dart",
                                                        Pointer_type)),
                              LEGO_pool);
  WN *stid_wn = AWN_StidIntoSym (preg_sym, ldid_wn);
  *block_wn = WN_CreateBlock ();
  LWN_Insert_Block_Before (*block_wn, NULL, call_wn);
  LWN_Insert_Block_After (*block_wn, call_wn, stid_wn);
  ldid_wn = AWN_LdidSym (preg_sym);
  Create_alias (Alias_Mgr, stid_wn); // just a register
  Du_Mgr->Add_Def_Use (stid_wn, ldid_wn);

  if (!ST_is_reshaped(array_st)) {
    /* it is possible that there was no dart for this, so handle that.
     * Generate a conditional
     */

    WN *cond_wn = AWN_LdidSym (preg_sym);
    Du_Mgr->Add_Def_Use (stid_wn, cond_wn);
    WN *if_wn = WN_CreateIf(cond_wn, WN_CreateBlock(), WN_CreateBlock());
    LWN_Parentize(if_wn);

    SYMBOL* numprocs_sym = CXX_NEW (SYMBOL(Create_Preg_Symbol("$numprocs",
                                                              MTYPE_I4)),
                                    LEGO_pool);

    // then-part: (dart not-NULL)
    //
    WN *iload_wn= LWN_CreateIload(OPCODE_make_op(OPR_ILOAD,MTYPE_I4,MTYPE_I8),
                                  dart_offset_distr_p +
                                  dim*TY_size(distr_ty_entries[RT_dim_struct]),
                                  Be_Type_Tbl(MTYPE_I4),
                                  Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8)),
                                  ldid_wn);
    Create_alias (Alias_Mgr, iload_wn);
    if (inside_loop) Array_Dependence_Graph->Add_Vertex(iload_wn);
    WN *then_stid = AWN_StidIntoSym (numprocs_sym, iload_wn);
    LWN_Insert_Block_After (WN_then(if_wn), NULL, then_stid);

    // else-part: (dart is NULL) -- just return 1
    // We have to return 1 here since subsequent calls to
    // __dsm_dynamic_affinity_bounds(...) in libmp will assign all the
    // iterations to each invoking thread.
    // It would be nice if we could use something like
    // mp_sug_numthreads instead, with later calls dividing the
    // iterations, but that leads to errors in the case where we the
    // data affinity is for a distributed array which has only some of
    // its dimensions distributed, but not all. In that case assigning
    // mp_sug_numthreads to a non-distributed dimension results in
    // garbage.
    //
    OPCODE opc = OPCODE_make_op(OPR_INTCONST, MTYPE_I4, MTYPE_V);
    WN *else_stid = AWN_StidIntoSym (numprocs_sym, WN_CreateIntconst(opc, 1));
    LWN_Insert_Block_After (WN_else(if_wn), NULL, else_stid);

    // now insert the if
    //
    LWN_Insert_Block_Before (*block_wn, NULL, if_wn);

    ldid_wn = AWN_LdidSym(numprocs_sym);
    Create_alias (Alias_Mgr, then_stid);
    Copy_alias_info (Alias_Mgr, then_stid, else_stid);
    Du_Mgr->Add_Def_Use (then_stid, ldid_wn);
    Du_Mgr->Add_Def_Use (else_stid, ldid_wn);
    return ldid_wn;
  }
  

  // again, this can be an I4 iload, since number of processors is small
  ldid_wn = LWN_CreateIload (OPCODE_make_op(OPR_ILOAD, MTYPE_I4, MTYPE_I8),
                             dart_offset_distr_p +
                             dim*TY_size(distr_ty_entries[RT_dim_struct]),
                             Be_Type_Tbl(MTYPE_I4),
                             Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8)),
                             ldid_wn);
  Create_alias (Alias_Mgr, ldid_wn);
  if (inside_loop)
    Array_Dependence_Graph->Add_Vertex(ldid_wn); 
  return ldid_wn;
} /* Numprocs () */

/***********************************************************************
 *
 * Called each time an ldid_wn is generated.
 * Updates alias info and def-use info as appropriate.
 *
 ***********************************************************************/
void DISTR_INFO::Dimsize_Ldid (INT dim, WN* ldid_wn) {
  if (_dimsize_wn[dim].Elements() == 0) {
    // first ldid. Make a copy, since all the rest of LNO happens
    // between now and lego-gen, and this ldid can easily be deleted..
    if (ST_isLocal(WN_st(ldid_wn)))
      Create_local_alias (Alias_Mgr, ldid_wn);
    else Create_global_alias (Alias_Mgr, WN_st(ldid_wn), ldid_wn, NULL);
    WN* copy_wn = LWN_Copy_Tree (ldid_wn);
    _dimsize_wn[dim][_dimsize_wn[dim].Newidx()] = copy_wn;
    Copy_alias_info (Alias_Mgr, ldid_wn, copy_wn);
    Du_Mgr->Add_Def_Use (Current_Func_Node, ldid_wn);
    return;
  }

  WN* wn = _dimsize_wn[dim][_dimsize_wn[dim].Lastidx()];
  if (WN_operator(wn) == OPR_LDID) {
    // last one was an LDID, must be the only one
    FmtAssert (_dimsize_wn[dim].Elements() == 1,
               ("Dimsize_Ldid: why are we storing more than 1 ldid?"));
    Copy_alias_info (Alias_Mgr, wn, ldid_wn);
    Du_Mgr->Add_Def_Use (Current_Func_Node, ldid_wn);
    return;
  }

  Copy_alias_info (Alias_Mgr, wn, ldid_wn);
  // Must be all STIDs
  for (INT i=0; i<_dimsize_wn[dim].Elements(); i++) {
    wn = _dimsize_wn[dim][i];
    FmtAssert (WN_operator(wn) == OPR_STID,
               ("Dimsize_Ldid: stored wn neither ldid nor stid"));
    Du_Mgr->Add_Def_Use (wn, ldid_wn);
  }
  if (ST_isLocal(_dimsize[dim]->St()))
    // global variable, also add incoming def
    Du_Mgr->Add_Def_Use (Current_Func_Node, ldid_wn);
} /* DISTR_INFO::Dimsize_Ldid */

/***********************************************************************
 *
 * Called each time an stid_wn is generated.
 * Update alias info and def-use info appropriately.
 *
 ***********************************************************************/
void DISTR_INFO::Dimsize_Stid (INT dim, WN* stid_wn) {
  if (_dimsize_wn[dim].Elements() == 0) {
    // nothing has happened yet
    _dimsize_wn[dim][_dimsize_wn[dim].Newidx()] = stid_wn;
    if (ST_isLocal(WN_st(stid_wn)))
      Create_local_alias (Alias_Mgr, stid_wn);
    else Create_global_alias (Alias_Mgr, WN_st(stid_wn), stid_wn, NULL);
    return;
  }
  
  WN* wn = _dimsize_wn[dim][_dimsize_wn[dim].Lastidx()];
  if (WN_operator(wn) == OPR_LDID) {
    // this is the first stid
    FmtAssert (_dimsize_wn[dim].Elements() == 1,
               ("Dimsize_Stid: why are we storing more than 1 ldid?"));
    Copy_alias_info (Alias_Mgr, wn, stid_wn);
    /* the ldid was a copy-tree, so we can delete it now */
    LWN_Delete_Tree (wn);
    _dimsize_wn[dim][_dimsize_wn[dim].Lastidx()] = stid_wn;
    USE_LIST* use_list = Du_Mgr->Du_Get_Use(Current_Func_Node);
    USE_LIST_ITER use_iter(use_list);
    DU_NODE* node;
    // walk all the uses from the Current_Func_Node, and replace
    // their defs to be stid_wn
    for (node = use_iter.First(); !use_iter.Is_Empty();
                                  node = use_iter.Next()) {
      WN* uwn = node->Wn();
      if (WN_operator(uwn) == OPR_LDID) {
        SYMBOL sym (uwn);
        if (sym == *(_dimsize[dim])) {
          // use of this ST
          if (ST_isLocal(_dimsize[dim]->St()))
            // local variable, delete global def
            Du_Mgr->Delete_Def_Use (Current_Func_Node, uwn);
          Du_Mgr->Add_Def_Use (stid_wn, uwn);
          Du_Mgr->Ud_Get_Def(uwn)->Set_loop_stmt(NULL);
        }
      }
    }
    return;
  }
  
  FmtAssert (WN_operator(wn) == OPR_STID,
             ("Dimsize_Stid: Expected stored STID"));
  _dimsize_wn[dim][_dimsize_wn[dim].Newidx()] = stid_wn;
  Copy_alias_info (Alias_Mgr, wn, stid_wn);
  USE_LIST* use_list = Du_Mgr->Du_Get_Use(wn);
  USE_LIST_ITER use_iter(use_list);
  DU_NODE* node;
  // walk all the uses from wn, and add a def from stid_wn
  for (node = use_iter.First(); !use_iter.Is_Empty();
                                node = use_iter.Next()) {
    WN* uwn = node->Wn();
    if (WN_operator(uwn) == OPR_LDID) {
      SYMBOL sym (uwn);
      if (sym == *(_dimsize[dim])) {
        // use of this ST
        Du_Mgr->Add_Def_Use (stid_wn, uwn);
        Du_Mgr->Ud_Get_Def(uwn)->Set_loop_stmt(NULL);
      }
    }
  }
} /* DISTR_INFO::Dimsize_Stid */

/***********************************************************************
 *
 * Return an ldid for the dimsize variable of the given dimension, "i".
 *
 ***********************************************************************/
WN* DISTR_INFO::Dimsize (INT dim) {
  if (_buddy_dinfo) return _buddy_dinfo->Dimsize(dim);

  SYMBOL* dimsize = _dimsize[dim];
  WN* ldid_wn = NULL;

  Is_True (dimsize, ("Asking for dimsize of a STAR distributed dimension"));
  if (Small_Index()) { 
    OPCODE ldid_op = OPCODE_make_op(OPR_LDID, MTYPE_I4, MTYPE_I8);
    ldid_wn = WN_CreateLdid (ldid_op, dimsize->WN_Offset(),
                             dimsize->St(), Be_Type_Tbl(MTYPE_I4));
  }
  else {
    OPCODE ldid_op = OPCODE_make_op(OPR_LDID, MTYPE_I8, MTYPE_I8);
    ldid_wn = WN_CreateLdid (ldid_op, dimsize->WN_Offset(),
                             dimsize->St(), Be_Type_Tbl(MTYPE_I8));
  }
  Dimsize_Ldid(dim, ldid_wn);
  return ldid_wn;
}

/***********************************************************************
 *
 * Return a usable WHIRL tree for the chunksize in the given dimension.
 *
 ***********************************************************************/
WN* DISTR_INFO::Chunksize (INT dim) {
  DISTR_ARRAY* dact = Lookup_DACT(Array_ST());
  WN* chunksize_wn = dact->Chunksize(dim);
  return chunksize_wn; 
}

/***********************************************************************
 *
 * Return a pointer to the WHIRL expression for the bound of the array
 * in the given dimension.
 *
 * Caution: This routine returns a cloned tree: if the caller only needs 
 * to look at it, then the caller should explicitly free it when done.
 *
 ***********************************************************************/
WN* DISTR_INFO::Get_Array_Dim_Size (INT dim) {
  DISTR_ARRAY* dact = NULL;
  if (Num_Dact() > 0) {
    if (Num_Dact() != 1)
      DevWarn ("Array %s has more than one dact", ST_name(_array->St()));

    dact = _dact_da[0];
  }
  else if (Num_Redistr() > 0) {
    dact = Get_Redistr(0);
  }
  else FmtAssert (FALSE, ("Array %s has no dact", ST_name(_array->St())));
  return dact->Array_Size_WN (dim);
}


static BOOL DU_Closure_Collect_Defs (DYN_ARRAY<WN*> &stmt_stack, WN* load_wn);

/***********************************************************************
 *
 * This routine MUST be called with an ST for a local VLA.
 * It tries to find the alloca for that array directly in the body-block
 * of the function, and moves it (and preceeding dependent instructions)
 * to before the preamble.
 * 
 * If it cannot find the alloca, then it DevWarns and returns.
 *
 ***********************************************************************/
extern void Move_Alloca (ST* st) {

  if (!Is_VLA (st)) {
    DevWarn ("Move_Alloca: suspicious looking ST - %s\n", ST_name(st));
  }
  
  // do the processing anyway
  WN *wn;
  wn = WN_first(WN_func_body(Current_Func_Node));
  WN* preamble_wn = NULL;
  while (wn) {

    if (WN_operator(wn) == OPR_PRAGMA &&
        WN_pragma(wn) == WN_PRAGMA_PREAMBLE_END)
      preamble_wn = wn;


    if (WN_operator(wn) == OPR_STID && WN_st(wn) == st) {
      // found a store
      WN* prev_wn = WN_prev(wn);

      FmtAssert (prev_wn &&
                 WN_operator(prev_wn) == OPR_INTRINSIC_CALL &&
                 (Pointer_Size == 8 ?
                  WN_intrinsic(prev_wn) == INTRN_U8I8ALLOCA :
                  WN_intrinsic(prev_wn) == INTRN_U4I4ALLOCA),
                 ("Unable to distribute VLA (%s). Check bounds expression",
                  ST_name(st)));


      if (preamble_wn == NULL) {
        // haven't seen a preamble so far, so the alloca is already OK
        return;
      }

      MEM_POOL_Push (LEGO_pool);
      {
        DYN_ARRAY<WN*> stmt_stack;
        stmt_stack.Set_Mem_Pool (LEGO_pool);
        stmt_stack.AddElement (wn);
        stmt_stack.AddElement (prev_wn);

        INT idx = 1;

        BOOL ok_to_move = TRUE;
        while (idx < stmt_stack.Elements()) {
          WN* stmt_wn = stmt_stack[idx];
          WN* load_wn = WN_kid0(stmt_wn);
    
          if (DU_Closure_Collect_Defs (stmt_stack, load_wn) == (BOOL) 0) {
            ok_to_move = FALSE;
            break;
          }

          idx++;
        }

        FmtAssert (ok_to_move,
                   ("Cannot support distribute/redistribute on this VLA %s.\n Please simplify the bounds expression\n",
                    ST_name(st)));

        // ok_to_move, so move all the stats in stmt_stack to before preamble


      // start with the final STID, and move backwards until the PREAMBLE
        WN* stmt_wn = stmt_stack[0];
        while (!(WN_operator(stmt_wn) == OPR_PRAGMA &&
                 WN_pragma(stmt_wn) == WN_PRAGMA_PREAMBLE_END)) {

          // move those statements that are in the move-list (i.e. stmt_stack)
          BOOL in_stmt_stack = FALSE;
          INT i;
          for (i=0; i<stmt_stack.Elements(); i++) {
            if (stmt_stack[i] == stmt_wn) break;
          }

          if (i < stmt_stack.Elements()) {
            // stmt_wn is present in stmt_stack
            WN* tmp = stmt_wn;
            stmt_wn = WN_prev(stmt_wn);
            tmp = LWN_Extract_From_Block (tmp);
            LWN_Insert_Block_Before (WN_func_body(Current_Func_Node),
                                     preamble_wn,
                                     tmp);
            preamble_wn = tmp;
          }
          else stmt_wn = WN_prev(stmt_wn);
        }
      }

      MEM_POOL_Pop (LEGO_pool);
      return;
    }
    wn = WN_next(wn);
  }

  // if we reach here, we didn't find an alloca. Error!
  FmtAssert (FALSE, ("Unable to find alloca of %s\n", ST_name(st)));
}
      
      
           

/***********************************************************************
 *
 * Given an expr-node in load_wn, recursively find all LDIDs and add
 * their defs to stmt_stack if the defs are good --- i.e. single defs
 * before the statement doing the load.
 * 
 * Return FALSE if bad defs, or if the expr contains ILOADS or LDAs
 * Return TRUE otherwise.
 *
 ***********************************************************************/
static BOOL DU_Closure_Collect_Defs (DYN_ARRAY<WN*> &stmt_stack, WN* load_wn) {
  if (load_wn == NULL) return TRUE;

  // don't allow ILOADS or LDAs
  if (WN_operator(load_wn) == OPR_ILOAD ||
      WN_operator(load_wn) == OPR_LDA)
    return FALSE;

  if (WN_operator(load_wn) == OPR_LDID) {

    // first find the position of load_wn within the func_body_block
    WN* pos_wn = load_wn;
    while (pos_wn) {
      if (LWN_Get_Parent(pos_wn) == WN_func_body(Current_Func_Node))
        break;
      pos_wn = LWN_Get_Parent(pos_wn);
    }
    FmtAssert (pos_wn, ("DU_Closure_Collect_Defs -- error processing %s\n",
                        ST_name(WN_st(load_wn))));

    DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(load_wn);
    if (!def_list || def_list->Incomplete()) return FALSE;

    // we want a unique def to reach this load.
    DEF_LIST_ITER iter(def_list);
    DU_NODE* node = iter.First();
    DU_NODE* tmp = iter.Next();
    if (!iter.Is_Empty()) return FALSE;

    WN* def_wn = node->Wn();

    // is the def ok? Either a direct STID, or func-entry

    if (WN_opcode(def_wn) == OPC_FUNC_ENTRY) return TRUE;

    if (WN_operator(def_wn) != OPR_STID ||
        WN_st(def_wn) != WN_st(load_wn)) {
      return FALSE;
    }

    // is it directly in the func_body_block?
    if (LWN_Get_Parent(def_wn) != WN_func_body(Current_Func_Node))
      return FALSE;

    // is it before the load?
    WN* prev_wn = WN_prev(pos_wn);
    while (prev_wn) {
      if (prev_wn == def_wn) break;
      prev_wn = WN_prev(prev_wn);
    }
    if (prev_wn == NULL) return FALSE;

    // if we reached here, we have a unique def that is before
    // the load. So Add it.
    INT i;
    for (i=0; i<stmt_stack.Elements(); i++) {
      if (def_wn == stmt_stack[i]) {
        // is a duplicate
        break;
      }
    }
    if (i == stmt_stack.Elements()) {
      // not a duplicate
      stmt_stack.AddElement (def_wn);
    }
    return TRUE;
  }
  else {
    // recurse
    for (INT i=0; i<WN_kid_count(load_wn); i++) {
      if (DU_Closure_Collect_Defs (stmt_stack, WN_kid(load_wn,i)) == FALSE)
        return FALSE;
    }
    return TRUE;
  }
}


/***********************************************************************
 *
 * Given a adjustable-sized local array,
 * walk the preamble of the function body looking for an STID 
 * (of the alloca) into the array variable.
 * Return the STID.
 *
 ***********************************************************************/
static WN* Find_Alloca (ST* st) {
  WN* wn = WN_func_body(Current_Func_Node);
  Is_True (WN_operator(wn) == OPR_BLOCK,
           ("Expected function body to be a OPR_BLOCK\n"));
  wn = WN_first(wn);
  while (wn) {
    FmtAssert ((WN_operator(wn) != OPR_PRAGMA) ||
               (WN_pragma(wn) != WN_PRAGMA_PREAMBLE_END),
               ("Reached end of preamble w/o finding alloca of %s\n",
                ST_name(st)));
    if ((WN_operator(wn) == OPR_STID) && (WN_st(wn) == st)) {
      return wn;
    }
    wn = WN_next(wn);
  }
  FmtAssert (FALSE,
             ("Reached end of function w/o finding either preamble or alloca of %s\n",
              ST_name(st)));
  return NULL;
}

/***********************************************************************
 *
 * Given a distributed adjustable-sized local array,
 * walk the preamble of the function body looking for an STID 
 * (of the alloca) into the array variable.
 * This is useful for subsequent DU-chaing etc.
 *
 ***********************************************************************/
void DISTR_INFO::Find_Alloca () {
  if (Get_Array_Def_WN()) return;
  ST* array_st = Array_ST();
  WN* wn = WN_func_body(Current_Func_Node);
  Is_True (WN_operator(wn) == OPR_BLOCK,
           ("Expected function body to be a OPR_BLOCK\n"));
  wn = WN_first(wn);
  while (wn) {
    FmtAssert ((WN_operator(wn) != OPR_PRAGMA) ||
               (WN_pragma(wn) != WN_PRAGMA_PREAMBLE_END),
               ("Reached end of preamble w/o finding alloca of %s\n",
                ST_name(array_st)));
    if ((WN_operator(wn) == OPR_STID) &&
        (WN_st(wn) == array_st)) {
      Create_unique_pointer_alias (Alias_Mgr, array_st, wn, NULL);
      WN* copy_wn = LWN_Copy_Tree(wn);
      Copy_alias_info (Alias_Mgr, wn, copy_wn);
      Set_Array_Alias_WN (copy_wn);
      Set_Array_Def_WN (wn);
      return;
    }
    wn = WN_next(wn);
  }
  FmtAssert (FALSE,
             ("Reached end of function w/o finding either preamble or alloca of %s\n",
              ST_name(array_st)));
}

/***********************************************************************
 *
 * Load the address of a distributed array, given an ST,
 * and update alias/DU info as required.
 * Return an ldid or an lda of the array.
 *
 * Required when we are doing dynamic affinity w/o a distribute directive.
 *
 ***********************************************************************/
extern WN* Load_Distr_Array (ST* array_st) {
  DISTR_INFO* dinfo = da_hash->Find(array_st);
  if (dinfo) return dinfo->Load_Distr_Array ();

  WN *array_load_wn = NULL;

  if (TY_kind(ST_type(array_st)) == KIND_POINTER) {
    // formal, or else assumed size local
    Is_True (ST_isFormal(array_st) || ST_isLocal(array_st),
             ("ST (%s) is kind_pointer: Must be formal or local\n",
              ST_name(array_st)));
    OPCODE ldid_op = OPCODE_make_op (OPR_LDID, Pointer_type, Pointer_type);
    array_load_wn = WN_CreateLdid (ldid_op, 0, array_st, ST_type(array_st));
    switch (ST_Var_Kind(array_st)) {
    case var_local:
    {
      WN* stid_wn = Find_Alloca(array_st);
      Create_local_alias (Alias_Mgr, stid_wn);
      Copy_alias_info (Alias_Mgr, stid_wn, array_load_wn);
      Du_Mgr->Add_Def_Use (stid_wn, array_load_wn);
      break;
    }
    case var_formal:
    {
      Create_formal_alias (Alias_Mgr, array_st, array_load_wn, NULL);
      Du_Mgr->Add_Def_Use (Current_Func_Node, array_load_wn);
      break;
    }
    default:
      FmtAssert (FALSE, ("ST (%s) must be local or formal\n",
                         ST_name(array_st)));
      break;
    }
  } else {
    OPCODE lda_op = OPCODE_make_op (OPR_LDA, Pointer_type, MTYPE_V);
    array_load_wn = WN_CreateLda
      (lda_op, 0, Make_Pointer_Type(ST_type(array_st)),
       array_st);
  }
  return array_load_wn;
} /* Load_Distr_Array () */

/***********************************************************************
 *
 * Load the address of a distributed array, 
 * and update alias/DU info as required.
 * Return an ldid or an lda of the array.
 *
 ***********************************************************************/
WN* DISTR_INFO::Load_Distr_Array () {
  ST* array_st = Array_ST();
  WN *array_load_wn = NULL;

  // If the array is reshaped, or if it was a formal (either reshaped or not)
  // or if it was an assumed-size local,
  // then the array_st is a pointer. Otherwise the array_st is the array itself

  if (TY_kind(ST_type(array_st)) == KIND_POINTER) {
    // formal, or else reshaped
    Is_True (ST_isFormal(array_st) || ST_isLocal(array_st) || IsReshaped(),
             ("ST (%s) is kind_pointer: Must be formal, local, or reshaped\n",
              ST_name(array_st)));
    OPCODE ldid_op = OPCODE_make_op (OPR_LDID, Pointer_type, Pointer_type);
    array_load_wn = WN_CreateLdid (ldid_op, 0, array_st, ST_type(array_st));
    switch (ST_Var_Kind(array_st)) {
    case var_local:
      if (Get_Array_Alias_WN())
        Copy_alias_info (Alias_Mgr,Get_Array_Alias_WN(),
                         array_load_wn);
      else {
        Create_local_alias (Alias_Mgr, array_load_wn);
        WN* copy_wn = LWN_Copy_Tree (array_load_wn);
        Copy_alias_info (Alias_Mgr, array_load_wn, copy_wn);
        Set_Array_Alias_WN(copy_wn);
      }
      break;
    case var_global:
    case var_common:
      if (Get_Array_Alias_WN())
        Copy_alias_info (Alias_Mgr,Get_Array_Alias_WN(), array_load_wn);
      else {
        Create_global_alias (Alias_Mgr, array_st, array_load_wn, NULL);
        WN* copy_wn = LWN_Copy_Tree (array_load_wn);
        Copy_alias_info (Alias_Mgr, array_load_wn, copy_wn);
        Set_Array_Alias_WN(copy_wn);
      }
      break;
    case var_formal:
      if (Get_Array_Alias_WN())
        Copy_alias_info (Alias_Mgr,Get_Array_Alias_WN(), array_load_wn);
      else {
        Create_formal_alias (Alias_Mgr, array_st, array_load_wn, NULL);
        WN* copy_wn = LWN_Copy_Tree (array_load_wn);
        Copy_alias_info (Alias_Mgr, array_load_wn, copy_wn);
        Set_Array_Alias_WN(copy_wn);
      }
      break;
    }
    Add_Array_Use_WN(array_load_wn);
  } else {
    OPCODE lda_op = OPCODE_make_op (OPR_LDA, Pointer_type, MTYPE_V);
    array_load_wn = WN_CreateLda
      (lda_op, 0, Make_Pointer_Type(ST_type(array_st)),
       array_st);
  }
  return array_load_wn;
} /* DISTR_INFO::Load_Distr_Array () */


/***********************************************************************
 *
 * Load the address of a reshaped distributed array, 
 * and update alias/DU info as required.
 * Return an ldid or an lda of the array.
 *  
 * Called after array lowering, only for reshaped arrays in a COMMON block.
 *
 ***********************************************************************/
WN* DISTR_INFO::Load_New_Distr_Array () {
  ST* array_st = Array_ST();
  Is_True (IsReshaped(),
           ("Load_New_Distr_Array called for non-reshaped array %s\n",
            ST_name(array_st)));
  Is_True (ST_Var_Kind(array_st) == var_common,
           ("Load_New_Distr_Array called for non-common array %s\n",
            ST_name(array_st)));
  /* is a common */
  OPCODE ldid_op = OPCODE_make_op (OPR_LDID, Pointer_type, Pointer_type);
  WN* array_load_wn = WN_CreateLdid (ldid_op, _array_common->WN_Offset(),
                                     _array_common->St(),
                                     ST_type(_array_common->St()));
  if (_array_common_alias_wn)
    Copy_alias_info (Alias_Mgr, _array_common_alias_wn, array_load_wn);
  else {
    Create_global_alias (Alias_Mgr, _array_common->St(), 
                         array_load_wn, NULL);
    _array_common_alias_wn = array_load_wn;
  }
  Du_Mgr->Add_Def_Use (Current_Func_Node, array_load_wn);
  return array_load_wn;  
} /* DISTR_INFO::Load_New_Distr_Array () */

/***********************************************************************
 *
 * Return an LDID of the DART.
 *
 ***********************************************************************/
WN* DISTR_INFO::DART_Ldid (ST* st) {

  // If ST* is passed, than it is a local dart used in error checking
  // Use always comes after def, and the def node is _ec_dart_def_wn

  if (st != NULL) {
    WN *dart_ldid_wn = WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                                     Pointer_type,
                                                     Pointer_type),
                                      0, st, distr_ty_entries[RT_ptr]);
    Is_True (_ec_dart_def_wn, ("EC-dart-ldid: expected a def\n"));
    Copy_alias_info (Alias_Mgr, _ec_dart_def_wn, dart_ldid_wn);
    Du_Mgr->Add_Def_Use (_ec_dart_def_wn, dart_ldid_wn);
    return dart_ldid_wn;
  }


  /* Possible scenarios:
   *    - first call, before any defs have been generated
   *    - not first call, but before any defs
   *    - after one (or more) defs
   */

  WN *dart_ldid_wn = WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                                   Pointer_type,
                                                   Pointer_type),
                                    0, _dart_st, distr_ty_entries[RT_ptr]);

  if (_dart_wn.Elements() == 0) {
    /* First use */
    if (ST_isLocal(_dart_st))
      Create_local_alias (Alias_Mgr, dart_ldid_wn);
    else Create_global_alias (Alias_Mgr, WN_st(dart_ldid_wn),
                              dart_ldid_wn, NULL);
    WN* copy_wn = LWN_Copy_Tree (dart_ldid_wn);
    Copy_alias_info (Alias_Mgr, dart_ldid_wn, copy_wn);
    Du_Mgr->Add_Def_Use (Current_Func_Node, dart_ldid_wn);
    _dart_wn[_dart_wn.Newidx()] = copy_wn;
    return dart_ldid_wn;
  }

  WN* wn = _dart_wn[_dart_wn.Lastidx()];
  if (WN_operator(wn) == OPR_LDID) {
    // only LDIDs so far
    FmtAssert (_dart_wn.Elements() == 1,
               ("DART_Ldid: why are we storing more than 1 ldid?"));
    Copy_alias_info (Alias_Mgr, wn, dart_ldid_wn);
    Du_Mgr->Add_Def_Use (Current_Func_Node, dart_ldid_wn);
    return dart_ldid_wn;
  }

  Copy_alias_info (Alias_Mgr, wn, dart_ldid_wn);
  // Must be all STIDs
  for (INT i=0; i<_dart_wn.Elements(); i++) {
    wn = _dart_wn[i];
    FmtAssert (WN_operator(wn) == OPR_STID,
               ("DART_Ldid: stored wn neither ldid nor stid"));
    Du_Mgr->Add_Def_Use (wn, dart_ldid_wn);
  }
  if (ST_level(_dart_st) == GLOBAL_SYMTAB)
    // global variable, also add incoming def
    Du_Mgr->Add_Def_Use (Current_Func_Node, dart_ldid_wn);
  return dart_ldid_wn;
}

/***********************************************************************
 *
 * Called with an STID of the DART, update alias info, DU-chains.
 *
 ***********************************************************************/
void DISTR_INFO::DART_Stid (WN* stid_wn, ST* st) {

  // If ST* is passed, than it is a local dart used in error checking
  // st is always local and def node is saved in _ec_dart_def_wn

  if (st != NULL) {
    _ec_dart_def_wn = stid_wn;
    Create_local_alias (Alias_Mgr, stid_wn);
    return;
  }


  /* Possible scenarios:
   *    - no action so far
   *    - one or more uses have been generated, but no defs
   *    - some defs have been generated 
   */

  if (_dart_wn.Elements() == 0) {
    // nothing has happened yet
    _dart_wn[_dart_wn.Newidx()] = stid_wn;
    if (ST_isLocal(_dart_st)) 
      Create_local_alias (Alias_Mgr, stid_wn);
    else Create_global_alias (Alias_Mgr, WN_st(stid_wn), stid_wn, NULL);
    return;
  }

  WN* wn = _dart_wn[_dart_wn.Lastidx()];
  if (WN_operator(wn) == OPR_LDID) {
    // this is the first stid
    FmtAssert (_dart_wn.Elements() == 1,
               ("Dart_Stid: why are we storing more than 1 ldid?"));
    Copy_alias_info (Alias_Mgr, wn, stid_wn);
    _dart_wn[_dart_wn.Lastidx()] = stid_wn;
    USE_LIST* use_list = Du_Mgr->Du_Get_Use(Current_Func_Node);
    USE_LIST_ITER use_iter(use_list);
    DU_NODE* node;
    SYMBOL dart_sym (wn);
    // walk all the uses from the Current_Func_Node, and replace
    // their defs to be stid_wn
    for (node = use_iter.First(); !use_iter.Is_Empty();
                                  node = use_iter.Next()) {
      WN* uwn = node->Wn();
      if (WN_operator(uwn) == OPR_LDID) {
        SYMBOL sym (uwn);
        if (sym == dart_sym) {
          // use of this ST
          if (ST_level(_dart_st) != GLOBAL_SYMTAB)
            // local variable, delete global def
            Du_Mgr->Delete_Def_Use (Current_Func_Node, uwn);
          Du_Mgr->Add_Def_Use (stid_wn, uwn);
          Du_Mgr->Ud_Get_Def(uwn)->Set_loop_stmt(NULL);
        }
      }
    }

    /* the ldid was a copy-tree, so we can delete it now */
    LWN_Delete_Tree (wn);

    return;
  }
  
  FmtAssert (WN_operator(wn) == OPR_STID,
             ("Dart_Stid: Expected stored STID"));
  _dart_wn[_dart_wn.Newidx()] = stid_wn;
  Copy_alias_info (Alias_Mgr, wn, stid_wn);
  USE_LIST* use_list = Du_Mgr->Du_Get_Use(wn);
  USE_LIST_ITER use_iter(use_list);
  DU_NODE* node;
  SYMBOL dart_sym (_dart_st, 0, Pointer_type);
  // walk all the uses from wn, and add a def from stid_wn
  for (node = use_iter.First(); !use_iter.Is_Empty();
                                node = use_iter.Next()) {
    WN* uwn = node->Wn();
    if (WN_operator(uwn) == OPR_LDID) {
      SYMBOL sym (uwn);
      if (sym == dart_sym) {
        // use of this ST
        Du_Mgr->Add_Def_Use (stid_wn, uwn);
        Du_Mgr->Ud_Get_Def(uwn)->Set_loop_stmt(NULL);
      }
    }
  }
} /* DISTR_INFO::DART_Stid () */

void DISTR_INFO::DART_Ptr_Ref (WN* wn, ST* st) {
  // If ST* is passed, than it is a local dart used in error checking
  if (st != NULL) {
    if (_ec_dart_ptr_wn)
      Copy_alias_info (Alias_Mgr, _ec_dart_ptr_wn, wn);
    else {
      Create_unique_pointer_alias (Alias_Mgr, st, NULL, wn);
      _ec_dart_ptr_wn = wn;
    }
  }
  // Otherwise, use the real _dart_st
  else {
    if (_dart_ptr_alias_wn)
      Copy_alias_info (Alias_Mgr, _dart_ptr_alias_wn, wn);
    else {
      Create_unique_pointer_alias (Alias_Mgr, _dart_st, NULL, wn);
      _dart_ptr_alias_wn = wn;
    }
  }
}

/***********************************************************************
 *
 * Given the function tree, read all the data-distribution pragmas
 * and build up internal representations (the dinfo structure).
 *
 ***********************************************************************/
void Read_Distr_Pragmas (WN* func_nd) {
  WN *pwn = func_nd;
  extern MEM_POOL LNO_local_pool;
  MEM_POOL_Push (&LNO_local_pool);
  dynamic_list = CXX_NEW (ST_HASH_TABLE(20, &LNO_local_pool),
                          &LNO_local_pool);

  // Walk the tree, processing pragma nodes
  while (pwn) {
    if ((WN_opcode(pwn) == OPC_PRAGMA) || (WN_opcode(pwn) == OPC_XPRAGMA)) {
      switch (WN_pragma(pwn)) {
        // Check that the WN_st is non-NULL before processing the pragma
        // since the front-end discards the ST entry if the array is not
        // referenced in the PU
      case WN_PRAGMA_DISTRIBUTE:
        DB_PRINT(printf ("Found a distribute pragma\n"));
        if (WN_st(pwn)) pwn = Read_Pragma_Distribute (pwn);
        else pwn = LWN_Get_Next_Stmt_Node (pwn);
        break;
      case WN_PRAGMA_REDISTRIBUTE:
        DB_PRINT(printf ("Found a redistribute pragma\n"));
        if (WN_st(pwn)) pwn = Read_Pragma_Redistribute (pwn);
        else pwn = LWN_Get_Next_Stmt_Node (pwn);
        break;
      case WN_PRAGMA_DISTRIBUTE_RESHAPE:
        DB_PRINT(printf ("Found a distribute-reshape pragma\n"));
        if (WN_st(pwn)) pwn = Read_Pragma_Distribute_Reshape (pwn);
        else pwn = LWN_Get_Next_Stmt_Node (pwn);
        break;
      case WN_PRAGMA_DYNAMIC:
      {
        DB_PRINT(printf ("Found a dynamic pragma\n"));
        DISTR_INFO* dinfo = da_hash->Find(WN_st(pwn));
        if (dinfo) dinfo->Set_Dynamic();
        else dynamic_list->Enter (WN_st(pwn), TRUE);
        WN* tmp_wn = pwn;
        pwn = LWN_Get_Next_Stmt_Node (pwn);
        LWN_Delete_Tree_From_Block (tmp_wn);
        break;
      }
      case WN_PRAGMA_AFFINITY:
      case WN_PRAGMA_DATA_AFFINITY:
      case WN_PRAGMA_THREAD_AFFINITY:
        DB_PRINT(printf ("Found an affinity pragma\n"));
        pwn = Read_Pragma_Affinity (pwn);
        break;
      case WN_PRAGMA_PAGE_PLACE:
        DB_PRINT(printf ("Found a page-place pragma\n"));
        pwn = Read_Pragma_Page_Place (pwn);
        break;
      default:
        pwn = LWN_Get_Next_Stmt_Node (pwn);
        break;
      }
    } else pwn = LWN_Get_Next_Stmt_Node (pwn);
  }

  /* Now process the dynamic list */
  HASH_TABLE_ITER<ST*, BOOL> iter (dynamic_list);
  ST* st;
  BOOL mybool;
  while (iter.Step(&st, &mybool)) {
    DISTR_INFO* dinfo = da_hash->Find (st);
    if (dinfo) dinfo->Set_Dynamic();
    // otherwise it doesn't matter, since any affinity scheduling will 
    // be a dynamic lookup anyway.
  }
  CXX_DELETE (dynamic_list, &LNO_local_pool);
  dynamic_list = NULL;
  MEM_POOL_Pop (&LNO_local_pool);
} /* Read_Distr_Pragmas */

/***********************************************************************
 *
 * Given an affinity pragma, consume it by annotating do loop 
 * with the LEGO_INFO structure. Delete the pragma node, and 
 * return a pointer to the subsequent statement node.
 *
 ***********************************************************************/
static WN* Read_Pragma_Affinity (WN* pwn) {
#ifdef Is_True_On
  // Do some error checking -- look for 
  //    - next affinity xpragma, 
  //    - doacross, and 
  //    - do-loop nodes
  {
    WN* tmp_wn = WN_next(pwn);
    Is_True (tmp_wn && (WN_opcode(tmp_wn) == OPC_XPRAGMA ||
                        WN_opcode(tmp_wn) == OPC_PRAGMA) &&
             ((WN_pragma(tmp_wn) == WN_PRAGMA_AFFINITY) ||
              (WN_pragma(tmp_wn) == WN_PRAGMA_DATA_AFFINITY) ||
              (WN_pragma(tmp_wn) == WN_PRAGMA_THREAD_AFFINITY)),
             ("Affinity statements should have 2 xpragmas, but found only 1"));
    tmp_wn = WN_first(LWN_Get_Parent(pwn));
    while (tmp_wn) {
      if ((WN_opcode(tmp_wn) == OPC_PRAGMA) &&
          ((WN_pragma(tmp_wn) == WN_PRAGMA_DOACROSS) ||
           (WN_pragma(tmp_wn) == WN_PRAGMA_PDO_BEGIN) ||
           (WN_pragma(tmp_wn) == WN_PRAGMA_PARALLEL_DO)))
        break;
      tmp_wn = WN_next(tmp_wn);
    }
    Is_True (tmp_wn, ("Missing doacross/pdo before affinity XPRAGMA"));

    // start searching for a DO-LOOP from the body of the region,
    // which is the next kid in the region node (parent of pwn).
    WN* do_wn = WN_first(WN_kid(LWN_Get_Parent(LWN_Get_Parent(pwn)), 2));
    while (do_wn) {
      if (WN_operator(do_wn) == OPR_DO_LOOP) break;
      do_wn = WN_next(do_wn);
    }
    Is_True (do_wn,("Missing DO-LOOP after doacross and affinity pragma"));
  }
#endif

  // what kind of affinity?
  WN* tmp_wn = WN_next(pwn);
  while (tmp_wn) {
    if (((WN_opcode(tmp_wn) == OPC_PRAGMA) ||
         (WN_opcode(tmp_wn) == OPC_XPRAGMA)) &&
        ((WN_pragma(tmp_wn) == WN_PRAGMA_DATA_AFFINITY) ||
         (WN_pragma(tmp_wn) == WN_PRAGMA_THREAD_AFFINITY)))
      break;
    tmp_wn = WN_next(tmp_wn);
  }
  FmtAssert (tmp_wn, ("Missing type of affinity pragma\n"));

  /* Find the do loop */
  WN* do_wn = WN_first(WN_region_body(LWN_Get_Parent(LWN_Get_Parent(pwn))));
  while (do_wn) {
    if (WN_operator(do_wn) == OPR_DO_LOOP) break;
    do_wn = WN_next(do_wn);
  }
  Is_True (do_wn, ("Missing parallel do loop\n"));

  /* The affinity pragmas are replicated (by the front-end)
   * in the body of the do-loop -- this ensures that the expression
   * survives correctly through preopt. So we have to use the expression from
   * the affinity pragma in the loop-body.
   * So now find the same affinity pragmas in the body of the do-loop.
   */
  WN* rwn = do_wn;
  INT32 nest_count = Get_Do_Loop_Info(do_wn)->Mp_Info->Nest_Total();
  for (INT i=1; i<nest_count; i++) {
    rwn = WN_first(WN_do_body(rwn));
    while (rwn && (WN_operator(rwn) != OPR_REGION)) {
      rwn = WN_next(rwn);
    }
    FmtAssert (rwn, ("nested-doacross: cannot find nested region %d\n", i));
    rwn = WN_first(WN_region_body(rwn));
    while (rwn && (WN_operator(rwn) != OPR_DO_LOOP)) {
      rwn = WN_next(rwn);
    }
    FmtAssert(rwn, ("nested-doacross: missing doloop in MP region %d\n",i));
  }

  /* rwn is now the last loop of interest. Search for the pragmas in body */
  rwn = WN_do_body(rwn);
  rwn = WN_first(rwn);
  while (rwn) {
    if ((WN_opcode(rwn) == OPC_XPRAGMA) &&
        (WN_pragma(rwn) == WN_PRAGMA_AFFINITY))
      break;
    rwn = WN_next(rwn);
  }
  FmtAssert (rwn, ("Missing affinity pragma in do-loop body\n"));

  if (!Get_Trace(TP_LNOPT2, TT_LEGO_DISABLE_EXPLICIT_AFFINITY)) {
    switch (WN_pragma(tmp_wn)) {
    case WN_PRAGMA_DATA_AFFINITY:
      Read_Pragma_Data_Affinity (do_wn, rwn);
      break;
    case WN_PRAGMA_THREAD_AFFINITY:
      Read_Pragma_Thread_Affinity (pwn, do_wn, rwn);
      break;
    }
  }

  // Delete all the affinity nodes.
  // In the replication of the do-body
  // (rwn should still point to the replication in the do-body)
  while (rwn) {
    if ((WN_opcode(rwn) == OPC_XPRAGMA) &&
        (WN_pragma(rwn) == WN_PRAGMA_AFFINITY)) {
      // delete this affinity node, and keep going
      WN* tmp_wn = rwn;
      rwn = WN_next(rwn);
      LWN_Delete_Tree_From_Block (tmp_wn);
      continue;
    }
    if ((WN_opcode(rwn) == OPC_XPRAGMA) &&
        (WN_pragma(rwn) == WN_PRAGMA_DATA_AFFINITY)) {
      // delete this affinity node and keep going, 
      // since there may be multiple data-affinity nodes
      WN* tmp_wn = rwn;
      rwn = WN_next(rwn);
      LWN_Delete_Tree_From_Block (tmp_wn);
      continue;
    }
    if ((WN_opcode(rwn) == OPC_XPRAGMA) &&
        (WN_pragma(rwn) == WN_PRAGMA_THREAD_AFFINITY)) {
      LWN_Delete_Tree_From_Block (rwn);
      break;
    }
    // reach here only if done with affinity nodes.
    break;
  }

  // And in the pragma list of the MP region 
  // (pwn should still point to the original affinity xpragma node)
  WN* pragma_block_wn = LWN_Get_Parent(pwn);
  do {
    WN* prev_wn = pwn;
    pwn = WN_next(pwn);
    LWN_Delete_Tree_From_Block(prev_wn);
  } while (pwn &&
           (WN_opcode(pwn) == OPC_XPRAGMA) &&
           ((WN_pragma(pwn) == WN_PRAGMA_AFFINITY) ||
            (WN_pragma(pwn) == WN_PRAGMA_DATA_AFFINITY) ||
            (WN_pragma(pwn) == WN_PRAGMA_THREAD_AFFINITY)));

  if (pwn == NULL)
    // that was the last pragma node. Go up to region node
    pwn = WN_kid(LWN_Get_Parent(pragma_block_wn), 2);
  return pwn;
} /* Read_Pragma_Affinity */

/***********************************************************************
 *
 * If expr contains an LDID of an ST other than sym,
 * but that ST has a unique definition in terms of sym,
 * then replace the LDID by the unique definition.
 *
 ***********************************************************************/
static void Propagate_Loop_Index (SYMBOL* sym, WN* expr_wn) {
  if (!expr_wn) return;

  if (WN_operator(expr_wn) == OPR_LDID) {
    SYMBOL this_sym (expr_wn);
    if (*sym == this_sym) return;

    DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(expr_wn);
    if (!def_list || def_list->Incomplete()) return;

    DEF_LIST_ITER iter(def_list);
    const DU_NODE* node = NULL;

    // make sure there is only one DEF
    INT count = 0;
    for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
      if (count > 0) return;
      count++;
    }

    node = iter.First();
    WN* def_wn = node->Wn();
    SYMBOL def_sym (def_wn);
    if ((def_sym != *sym) || 
        (WN_operator(def_wn) != OPR_STID)) return;

    WN* rhs_wn = WN_kid0(def_wn);
    WN* new_rhs = LWN_Copy_Tree (rhs_wn, TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use (rhs_wn, new_rhs, Du_Mgr);
    Replace_WN (expr_wn, new_rhs);   
    LWN_Delete_Tree (expr_wn);
    Propagate_Loop_Index (sym, new_rhs);
    return;
  }

  Is_True (WN_operator(expr_wn) != OPR_BLOCK,
           ("Expr cannot contain a BLOCK"));
  for (INT i=0; i<WN_kid_count(expr_wn); i++) {
    WN* kid = WN_kid (expr_wn, i);
    Propagate_Loop_Index (sym, kid);
  }
}

/***********************************************************************
 *
 * Called with a pointer to the first affinity pragma node in both
 *  - the replicated pragma instance in the body of the do-loop (rwn).
 *  - as well as the do-loop pointer (do_wn)
 * Do all the data-affinity processing and return.
 *
 ***********************************************************************/
static void Read_Pragma_Data_Affinity (WN* do_wn, WN* rwn) {
  // for now assume only in a doacross. Later migrate to other PCF things

  // Make sure that the do-loop has unit-step, since we cannot handle
  // it otherwise.
  if (!Loop_Bounds_Simple(do_wn)) {
    ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(rwn),
                 WN_pragmas[WN_pragma(rwn)].name,
                 "step-size of loop must be 1 (ignoring).");
    return;
  }
  
  INT nest_count = 0;
  WN* index_wn = rwn;
  WN* array_wn = rwn;
  while ((WN_opcode(array_wn) == OPC_XPRAGMA) &&
         (WN_pragma(array_wn) == WN_PRAGMA_AFFINITY)) {
    nest_count++;
    array_wn = WN_next(array_wn);
  }
  MP_INFO* mpi = Get_Do_Loop_Info(do_wn)->Mp_Info;  
  if ((mpi == NULL) ||
      (mpi->Nest_Index() != 0) ||
      (mpi->Nest_Total() != nest_count)) {
    ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(rwn),
                 WN_pragmas[WN_pragma(rwn)].name,
                 "mismatch in data-affinity clause and nesting of doacross (ignoring).");
    return;
  }
  if ((WN_opcode(array_wn) != OPC_XPRAGMA) ||
      (WN_pragma(array_wn) != WN_PRAGMA_DATA_AFFINITY)) {
    ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(rwn),
                 WN_pragmas[WN_pragma(rwn)].name,
                 "missing reference in data-affinity clause (ignoring).");
    return;
  }
  array_wn = WN_kid0(array_wn);
  if (WN_operator(array_wn) != OPR_ILOAD) {
    ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(rwn),
                 WN_pragmas[WN_pragma(rwn)].name,
                 "affinity must be for an array reference (ignoring).");
    return;
  }
  array_wn = WN_kid0(array_wn);
  if (WN_operator(array_wn) != OPR_ARRAY) {
    ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(rwn),
                 WN_pragmas[WN_pragma(rwn)].name,
                 "affinity must be for an array reference (ignoring).");
    return;
  }
#if defined(__linux) || defined(BUILD_OS_DARWIN)
  // Nothing we can do about this for now.
  return;
#endif
  // list of do-loops to which we have attached a lego-info
  // if we run into an error, then we must undo the lego-info for
  // eachof them
  DYN_ARRAY<WN*> do_list;
  do_list.Set_Mem_Pool (LEGO_pool);

  INT i;
  for (i=0; i<nest_count; i++) {
    // now compute the desired distribution
    INT32 dimnum = -1, coeff, constant;
    INT32 tmp_coeff, tmp_constant;
    SYMBOL index_sym (WN_index(do_wn));
    // SYMBOL index_sym (WN_kid0(index_wn));
    for (INT32 j=0; j<WN_num_dim(array_wn); j++) {
      Propagate_Loop_Index (&index_sym, WN_array_index(array_wn, j));
      if (Check_Expr (WN_array_index(array_wn,j),
                      &index_sym,&tmp_coeff,&tmp_constant)) {
        if (dimnum != -1) {
          ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(rwn),
                       WN_pragmas[WN_pragma(rwn)].name,
                       "cannot have coupled subscripts (ignoring).");
          goto cleanup;
        }
        dimnum = j;
        constant = tmp_constant;
        coeff = tmp_coeff;
        // we could break here, but instead continue just for error checking.
      }
    }
    if (dimnum == -1) {
      ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(rwn),
                   WN_pragmas[WN_pragma(rwn)].name,
                   "bad array subscripts (ignoring).");
      goto cleanup;
    }
    /* now get the array base symbol */
    WN* base = WN_array_base(array_wn);
    if ((WN_operator(base) != OPR_LDA) &&
        (WN_operator(base) != OPR_LDID)) {
      ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(rwn),
                   WN_pragmas[WN_pragma(rwn)].name,
                   "must have an array reference (ignoring).");
      goto cleanup;
    }
    SYMBOL *symb = CXX_NEW (SYMBOL, LEGO_pool);
    symb->Init (base);

    /* Do some error checking */
    {
      ST* array_st = WN_st(base);
      DISTR_INFO* dinfo = da_hash->Find(array_st);
      if (dinfo) {
        // The following may not work if there are redistributes
        DISTRIBUTE_TYPE dt=dinfo->Get_Dact(0)->Get_Dim(dimnum)->Distr_Type ();
        if (dt == DISTRIBUTE_STAR) {
          ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(rwn),
                       WN_pragmas[WN_pragma(rwn)].name,
                       "has a non-distributed array (ignoring).");
          goto cleanup;
        }
        INT num_distr_dim = 0;
        DISTR_ARRAY* dact = dinfo->Get_Dact(0);
        for (INT j=0; j<dinfo->Num_Dim(); j++) {
          DISTRIBUTE_TYPE dt = dact->Get_Dim(j)->Distr_Type ();
          if (dt != DISTRIBUTE_STAR) num_distr_dim++;
        }
        if (num_distr_dim == 0) {
          ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(rwn),
                       WN_pragmas[WN_pragma(rwn)].name,
                       "has a non-distributed array (ignoring).");
          goto cleanup;
        }
        if (num_distr_dim != nest_count) {
          ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(rwn),
                       WN_pragmas[WN_pragma(rwn)].name,
                       "array has multiple distributed dimensions, may get poor load-balancing");
        }
      }
    }


    // allow -ve stride only on block distributions
    if (coeff < 0) {
      DISTR_ARRAY* dact = Lookup_DACT (WN_st(base));
      if (dact && dact->Get_Dim(dimnum)->Distr_Type() != DISTRIBUTE_BLOCK) {
        ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(rwn),
                 WN_pragmas[WN_pragma(rwn)].name,
                 "-ve stride allowed only on BLOCK distribution (ignoring).");
        goto cleanup;
      }
    }

    LEGO_INFO* li = CXX_NEW (LEGO_INFO (symb, dimnum, coeff, constant, 0, 0),
                             LEGO_pool);
    ST* array_st = WN_st(base);
    DISTR_INFO* dinfo = da_hash->Find(array_st);
    if (!dinfo || dinfo->IsDynamic ())
      li->Set_Dynamic_Affinity ();
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(do_wn);
    dli->Lego_Info = li;

    // add this do-loop to the list
    do_list.AddElement (do_wn);
    
    // now move index_wn and do_wn
    if (i<(nest_count-1)) {
      index_wn = WN_next(index_wn);
      do_wn = WN_do_body(do_wn);
      do_wn = WN_first(do_wn);
      while (do_wn && WN_operator(do_wn) != OPR_REGION) {
        do_wn = WN_next(do_wn);
      }
      FmtAssert (do_wn, ("Missing region in nested doacross"));
      do_wn = WN_first(WN_region_body(do_wn));
      while (do_wn && (WN_operator(do_wn) != OPR_DO_LOOP)) {
        do_wn = WN_next(do_wn);
      }
      FmtAssert (do_wn, ("Missing do-loop in nested doacross"));
    }
  }
  return;

  cleanup:
  for (i=0; i<do_list.Elements(); i++) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info (do_list[i]);
    FmtAssert (dli->Lego_Info,
               ("lego-info cleanup: missing lego-info\n"));
    CXX_DELETE (dli->Lego_Info, LEGO_pool);
    dli->Lego_Info = NULL;
  }
} /* Read_Pragma_Affinity */

/***********************************************************************
 *
 * Given the index expression for an array dimension and an ST
 * for the loop index variable, verify that the index expression is of
 * the form a*i+b.
 * Then see if given ST is used in the index expression. If so, 
 * set coeff and constant to the appropriate values (a and b respectively),
 * and return TRUE
 * otherwise return FALSE.
 *
 ***********************************************************************/
static BOOL Check_Expr (WN* expr_wn, SYMBOL* index_sym, 
                        INT32* coeff, INT32* constant) {
  switch (WN_operator(expr_wn)) {
  case OPR_LDID:
  {
    /* simple case - arr(i) */
    SYMBOL expr_sym (expr_wn);
    if (expr_sym == *index_sym) {
      *coeff = 1;
      *constant = 0;
      return TRUE;
    }
    return FALSE;
  }

  case OPR_MPY:
  {
    /* arr(a*i) */
    WN* e1 = WN_kid0(expr_wn);
    WN* e2 = WN_kid1(expr_wn);
    if (WN_operator(e1) != OPR_INTCONST) {
      WN* tmp_wn = e1;
      e1 = e2;
      e2 = tmp_wn;
    }
    if ((WN_operator(e1) != OPR_INTCONST) ||
        (WN_operator(e2) != OPR_LDID)) {
      ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(expr_wn),
                   "AFFINITY", "bad array subscripts (ignoring).");
      return FALSE;
    }
    /* Now e1 is constant, e2 is load of variable */
    SYMBOL expr_sym (e2);
    if (expr_sym == *index_sym) {
      *coeff = WN_const_val(e1);
      *constant = 0;
      return TRUE;
    }
    return FALSE;
  }
    
  case OPR_ADD:
  case OPR_SUB:
  {
    BOOL did_swap = FALSE;
    BOOL is_sub = WN_operator(expr_wn) == OPR_SUB;

    /* arr(a*i+b), or arr(a*i-b) */
    WN* e1 = WN_kid0(expr_wn);
    WN* e2 = WN_kid1(expr_wn);
    if (WN_operator(e1) != OPR_INTCONST) {
      WN* tmp_wn = e1;
      e1 = e2;
      e2 = tmp_wn;
      did_swap = TRUE;
    }
    if (WN_operator(e1) != OPR_INTCONST) {
      ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(expr_wn),
                   "AFFINITY", "bad subscripts (ignoring).");
      return FALSE;
    }
    /* e1 is now the constant, e2 is now an ldid(i), or a*i */
    *constant = WN_const_val(e1);
    if (is_sub && did_swap)
      *constant = (0-*constant);

    switch (WN_operator(e2)) {
    case OPR_LDID:
    {
      /* simple arr(i+k) */
      SYMBOL expr_sym (e2);
      if (expr_sym == *index_sym) {
        *coeff = 1;
        if (is_sub && !did_swap) *coeff = -1;
        return TRUE;
      }
      return FALSE;
    }
    case OPR_MPY:
    {
      WN* e3 = WN_kid0(e2);
      e2 = WN_kid1(e2);
      if (WN_operator(e2) != OPR_INTCONST) {
        WN* tmp_wn = e2;
        e2 = e3;
        e3 = tmp_wn;
      }
      if (WN_operator(e2) != OPR_INTCONST) {
        ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(expr_wn),
                     "AFFINITY", "bad subscripts (ignoring).");
        return FALSE;
      }
      if (WN_operator(e3) != OPR_LDID) {
        ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(expr_wn),
                     "AFFINITY", "bad subscripts (ignoring).");
        return FALSE;
      }
      SYMBOL expr_sym (e3);
      if (expr_sym == *index_sym) {
        *coeff = WN_const_val(e2);
        if (is_sub && !did_swap) *coeff = (-1) * (*coeff);
        return TRUE;
      }
      return FALSE;
    }
    default:
      ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(expr_wn),
                   "AFFINITY", "bad subscripts (ignoring).");
      return FALSE;
    }
  }

  default:
    // The expression doesn't contain the given ST
    return FALSE;
  }
} /* Check_Expr */

/***********************************************************************
 *
 * Called with a pointer to the first affinity pragma node in both
 *  - the pragma-list of the region (pwn)
 *  - the replicated pragma instance in the body of the do-loop (rwn).
 *  - as well as the do-loop pointer (do_wn)
 * Do all the thread-affinity processing and return.
 *
 ***********************************************************************/
static void Read_Pragma_Thread_Affinity (WN* pwn, WN* do_wn, WN* rwn) {

#if defined(__linux) || defined(BUILD_OS_DARWIN)
  // Nothing we can do about this for now.
  return; 
#endif 

  // Walk the pragmas, until we find the thread-affinity pragma
  while (rwn) {
    if ((WN_opcode(rwn) == OPC_XPRAGMA) &&
        (WN_pragma(rwn) == WN_PRAGMA_THREAD_AFFINITY)) break;
    rwn = WN_next(rwn);
  }
  FmtAssert (rwn, ("Missing thread-affinity pragma in do-loop body\n"));

  // figure out if doacross or pdo
  BOOL is_doacross;
  {
    WN* tmp_wn = WN_first(LWN_Get_Parent(pwn));
    while (tmp_wn) {
      if (WN_operator(tmp_wn) == OPR_PRAGMA) {
        if (WN_pragma(tmp_wn) == WN_PRAGMA_DOACROSS 
	    || WN_pragma(tmp_wn) == WN_PRAGMA_PARALLEL_DO) {
          is_doacross = TRUE;
          break;
        }
        else if (WN_pragma(tmp_wn) == WN_PRAGMA_PDO_BEGIN) {
          is_doacross = FALSE;
          break;
        }
      }
      tmp_wn = WN_next(tmp_wn);
    }
    Is_True(tmp_wn, ("Missing doacross/pdo for thread-affinity"));
  }

  /*
   * 
   * For doacross: (thr-aff-max is a local variable, not preg)
   *    thr-aff-max = NST
   *    c$doacross
   *    do preg = 0..thr-aff-max - 1
   *        do i = ...
   *          if ((expr % thr-aff-max) == preg)
   *
   * For pdo: (thr-aff-max is a preg)
   *    thr-aff-max = NCT
   *    c$par pdo
   *    do preg = 0..thr-aff-max - 1
   *        do i = ...
   *          if ((expr % thr-aff-max) == preg)
   *
   */

  SYMBOL thread_sym;
  WN* stid_wn;
  if (is_doacross) {
    // Create an ST, since it needs to be shared
    ST* st = New_ST(CURRENT_SYMTAB);
    ST_Init (st,
             Save_Str("$thr_aff_max"),
             CLASS_VAR,
             SCLASS_AUTO,
             EXPORT_LOCAL,
             Be_Type_Tbl(MTYPE_I8));
    thread_sym = SYMBOL (st, 0, MTYPE_I8);
    stid_wn = AWN_StidIntoSym (&thread_sym, Get_Runtime_Numthreads_Ldid());
  }
  else {
    // a pdo, must be a local symbol, to NCT
    Freeze_Cur_Numthreads_Func(do_wn);
    thread_sym = SYMBOL (MTYPE_To_PREG(MTYPE_I8),
                         Create_Preg(MTYPE_I8, "$thr_aff_max"),
                         MTYPE_I8);
    stid_wn = AWN_StidIntoSym (&thread_sym, 
      Get_Runtime_Cur_Numthreads_Func(do_wn));
  }

  // insert stid_wn before the parallel region
  {
    WN* tmp_wn = pwn;
    while (tmp_wn) {
      if (WN_opcode(tmp_wn) == OPC_REGION &&
          (RID_TYPE_mp(REGION_get_rid(tmp_wn)))) 
        break;
      tmp_wn = LWN_Get_Parent(tmp_wn);
    }
    Is_True(tmp_wn, ("Missing MP-region for thread-affinity"));
    LWN_Insert_Block_Before (NULL, tmp_wn, stid_wn);
  }
  Create_local_alias (Alias_Mgr, stid_wn);


  WN* newdo_wn = Thread_Affinity_Lower (do_wn, &thread_sym, stid_wn);

  //
  // Generate (<expr> % numthreads) == mythreadnum
  //
  WN* thread_wn = LWN_Copy_Tree(WN_kid0(rwn));
  LWN_Copy_Def_Use (WN_kid0(rwn), thread_wn, Du_Mgr);
  WN* max_threads_wn = AWN_LdidSym (&thread_sym);
  Copy_alias_info (Alias_Mgr, stid_wn, max_threads_wn);
  Du_Mgr->Add_Def_Use (stid_wn, max_threads_wn);
  thread_wn=AWN_Rem_Safe(MTYPE_I4, thread_wn, max_threads_wn);

  // use loop index variable of outer loop for mythreadnum
  SYMBOL index_sym(WN_start(newdo_wn));
  WN* myid_wn = AWN_LdidSym(&index_sym);
  Du_Mgr->Add_Def_Use (WN_start(newdo_wn), myid_wn);
  Du_Mgr->Add_Def_Use (WN_step(newdo_wn), myid_wn);
  DEF_LIST* deflist = Du_Mgr->Ud_Get_Def(myid_wn);
  deflist->Set_loop_stmt (newdo_wn);
  WN* cond_wn = LWN_CreateExp2 (OPCODE_make_op(OPR_EQ,
                                               Boolean_type,
                                               Promote_Type(MTYPE_I4)),
                                thread_wn, myid_wn);
  WN* block_wn = WN_CreateBlock();
  WN* if_wn = LWN_CreateIf (cond_wn, WN_do_body(do_wn), block_wn);
  block_wn = WN_CreateBlock ();
  LWN_Insert_Block_Before (block_wn, NULL, if_wn);
  WN_do_body(do_wn) = block_wn;
  LWN_Set_Parent(block_wn, do_wn);

  // Update access vectors for expr in the ifnode
  BOOL has_do_loops = FALSE;
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(do_wn);
  if (!dli->Is_Inner) has_do_loops = TRUE;
  IF_INFO *ii=CXX_NEW (IF_INFO(&LNO_default_pool,has_do_loops,
                               Find_SCF_Inside(if_wn,OPC_REGION)!=NULL),
                       &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map,if_wn,(void *)ii);
  DOLOOP_STACK* stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                                &LNO_local_pool);
  Build_Doloop_Stack(if_wn, stack);
  LNO_Build_If_Access(if_wn, stack);
  CXX_DELETE(stack, &LNO_local_pool);

  return;

} /* Read_Pragma_Thread_Affinity */

/***********************************************************************
 *
 * Pseudo-lower a do-loop for thread affinity. Replace the do with
 *  c$doacross or c$par pdo
 *  do preg = 0..thread_sym-1
 *    do ...original loop...
 *
 * (use stid_wn as store of thread-sym for DU-chains)
 * Return a pointer to the new do loop.
 *
 ***********************************************************************/
static WN* Thread_Affinity_Lower (WN* do_wn, SYMBOL* thread_sym, WN* stid_wn) {
  Is_True (do_wn && WN_opcode(do_wn) == OPC_DO_LOOP,
           ("Lower_Thread_Affinity -- do-loop node missing\n"));

  

  SYMBOL index_sym = Create_Preg_Symbol ("$thr_aff_index", MTYPE_I4);
  WN* index_wn = WN_CreateIdname(index_sym.WN_Offset(), index_sym.St());
  WN* start_def = AWN_StidIntoSym (&index_sym, LWN_Make_Icon(MTYPE_I4, 0));
  LWN_Copy_Linenumber(do_wn, start_def);
  WN* compare_use = AWN_LdidSym (&index_sym);
  WN* compare_th = AWN_LdidSym (thread_sym);
  Copy_alias_info (Alias_Mgr, stid_wn, compare_th);
  Du_Mgr->Add_Def_Use (stid_wn, compare_th);
  compare_th = LWN_CreateExp2(OPCODE_make_op(OPR_SUB, MTYPE_I4, MTYPE_V),
                              compare_th, LWN_Make_Icon(MTYPE_I4, 1));
  WN* compare_wn=LWN_CreateExp2(OPCODE_make_op(OPR_LE, Boolean_type, MTYPE_I4),
                                compare_use, compare_th);
  WN* step_use = AWN_LdidSym(&index_sym);
  WN* step_def = AWN_StidIntoSym
    (&index_sym, LWN_CreateExp2(OPCODE_make_op(OPR_ADD, MTYPE_I4, MTYPE_V),
                                step_use,
                                LWN_Make_Icon (MTYPE_I4, 1)));
  WN* newdo_wn = LWN_CreateDO(index_wn,
                              start_def,
                              compare_wn,
                              step_def,
                              WN_CreateBlock());

  LWN_Copy_Linenumber(do_wn, newdo_wn);
  LWN_Copy_Linenumber(do_wn, WN_do_body(newdo_wn));
  LWN_Insert_Block_Before (NULL, do_wn, newdo_wn);  // insert new do
  LWN_Extract_From_Block (do_wn);
  LWN_Insert_Block_Before (WN_do_body(newdo_wn), NULL, do_wn);// move to new do

  // Fixup DU-chains
  Create_alias (Alias_Mgr, start_def);
  Du_Mgr->Add_Def_Use (start_def, compare_use);
  Du_Mgr->Add_Def_Use (start_def, step_use);
  Du_Mgr->Add_Def_Use (step_def, compare_use);
  Du_Mgr->Add_Def_Use (step_def, step_use);
  DEF_LIST *deflist = Du_Mgr->Ud_Get_Def(compare_use);
  deflist->Set_loop_stmt(newdo_wn);
  deflist = Du_Mgr->Ud_Get_Def(step_use);
  deflist->Set_loop_stmt(newdo_wn);

  // Fixup do-loop info
  {
    DO_LOOP_INFO* odli = (DO_LOOP_INFO*) Get_Do_Loop_Info(do_wn);
    DO_LOOP_INFO* dli  = ((DO_LOOP_INFO*) CXX_NEW
                          (DO_LOOP_INFO(odli, &LNO_default_pool),
                           &LNO_default_pool));
    CXX_DELETE (odli->Mp_Info, odli->Pool());
    odli->Mp_Info = NULL;
    dli->Is_Inner = FALSE;
    Increment_Loop_Depths (do_wn);  // recursively
    dli->Mp_Info->Disable_Plowering();
    Set_Do_Loop_Info (newdo_wn, dli);
  }
  return newdo_wn;
} /* Thread_Affinity_Lower () */


/***********************************************************************
 *
 * Recursively increment all do-loop depths by 1.
 *
 ***********************************************************************/
static void Increment_Loop_Depths (WN* wn) {
  if (wn == NULL) return;

  if (WN_opcode(wn) == OPC_DO_LOOP) {
    DO_LOOP_INFO* dli = (DO_LOOP_INFO*) Get_Do_Loop_Info(wn);
    dli->Depth = dli->Depth+1;
    Increment_Loop_Depths(WN_do_body(wn));
    return;
  }

  if (WN_opcode(wn) == OPC_BLOCK) {
    WN* kid = WN_first(wn);
    while (kid) {
      Increment_Loop_Depths(kid);
      kid = WN_next(kid);
    }
    return;
  }

  for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
    Increment_Loop_Depths(WN_kid(wn, kidno));
  }
  return;
}

/***********************************************************************
 *
 * Consume a page-place pragma, by generating a call to the runtime
 * routine to actually migrate the pages.
 * (it's a headache to wait till lego_gen -- just generate code now)
 * Return a pointer to the statement following the last pragma node.
 *
 ***********************************************************************/
static WN* Read_Pragma_Page_Place (WN* pwn) {
  WN *addr_xpwn, *size_xpwn, *proc_xpwn;

  if ((WN_opcode(pwn) != OPC_XPRAGMA) ||
      (WN_pragma(pwn) != WN_PRAGMA_PAGE_PLACE)) {
    ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(pwn),
                 WN_pragmas[WN_pragma(pwn)].name,
                 "Missing address expression (ignoring).");
    return pwn;
  }
  addr_xpwn = pwn;
  pwn = WN_next(pwn);
  if ((WN_opcode(pwn) != OPC_XPRAGMA) ||
      (WN_pragma(pwn) != WN_PRAGMA_PAGE_PLACE)) {
    ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(pwn),
                 WN_pragmas[WN_pragma(pwn)].name,
                 "Missing size expression (ignoring).");
    LWN_Delete_Tree_From_Block (addr_xpwn);
    return pwn;
  }
  size_xpwn = pwn;
  pwn = WN_next(pwn);
  if ((WN_opcode(pwn) != OPC_XPRAGMA) ||
      (WN_pragma(pwn) != WN_PRAGMA_PAGE_PLACE)) {
    ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(pwn),
                 WN_pragmas[WN_pragma(pwn)].name,
                 "Missing thread-num expression (ignoring).");
    LWN_Delete_Tree_From_Block (addr_xpwn);
    LWN_Delete_Tree_From_Block (size_xpwn);
    return pwn;
  }
  proc_xpwn = pwn;
  pwn = LWN_Get_Next_Stmt_Node(pwn);

  OPCODE callop = OPCODE_make_op (OPR_CALL, MTYPE_V, MTYPE_V);
  WN* call_wn = WN_Create (callop, 3);
  WN_st_idx(call_wn) = ST_st_idx(distr_st_entries[Migrate_Pages]);
  Set_Runtime_Call_Side_Effects (call_wn);
  WN_linenum(call_wn) = LWN_Get_Linenum(pwn);
  WN* tmp_wn = LWN_Copy_Tree(WN_kid0(addr_xpwn));
  LWN_Copy_Def_Use (WN_kid0(addr_xpwn), tmp_wn, Du_Mgr);
  WN* parm_wn = WN_CreateParm (Pointer_type, tmp_wn,
                               Be_Type_Tbl(Pointer_type),
                               WN_PARM_BY_VALUE);
  LWN_Set_Parent (tmp_wn, parm_wn);
  WN_kid(call_wn, 0) = parm_wn;
  LWN_Set_Parent (parm_wn, call_wn);
  tmp_wn = LWN_Copy_Tree(WN_kid0(size_xpwn));
  parm_wn = WN_CreateParm (MTYPE_I8, tmp_wn,
                           Be_Type_Tbl(MTYPE_I8), WN_PARM_BY_VALUE);
  LWN_Set_Parent (tmp_wn, parm_wn);
  WN_kid(call_wn, 1) = parm_wn;
  LWN_Set_Parent (parm_wn, call_wn);
  tmp_wn = LWN_Copy_Tree(WN_kid0(proc_xpwn));
  parm_wn = WN_CreateParm (MTYPE_I8, tmp_wn,
                           Be_Type_Tbl(MTYPE_I8),
                           WN_PARM_BY_VALUE);
  LWN_Set_Parent (tmp_wn, parm_wn);
  WN_kid(call_wn, 2) = parm_wn;
  LWN_Set_Parent (parm_wn, call_wn);
  LWN_Insert_Block_After (NULL, proc_xpwn, call_wn);
  /* Now get rid of the pragma nodes */
  LWN_Delete_Tree_From_Block (addr_xpwn);
  LWN_Delete_Tree_From_Block (size_xpwn);
  LWN_Delete_Tree_From_Block (proc_xpwn);
  return pwn;
} /* Read_Pragma_Page_Place */

/***********************************************************************
 *
 * Check that the array TY is kosher for data distribution.
 *
 ***********************************************************************/
static BOOL Array_TY_OK (TY_IDX array_ty) {
  INT i;
  Is_True (TY_kind(array_ty) == KIND_ARRAY,
           ("Check_Array_TY called on a non-array"));
  INT ndims = TY_AR_ndims(array_ty);
  if (ndims == 0) return FALSE;

  INT elem_size = TY_size(TY_AR_etype(array_ty));
  for (i=0; i<ndims; i++) {
    /* Each thing must be a constant or a WHIRL <expr> tree */
    if ((!TY_AR_const_lbnd(array_ty, i)) &&
        (TY_AR_lbnd_val(array_ty, i) == 0))
      return FALSE;
    if ((!TY_AR_const_ubnd(array_ty, i)) &&
        (TY_AR_ubnd_val(array_ty, i) == 0))
      return FALSE;

    /* if the bounds are variable, the stride might be variable.
     * so let non-const strides go.
     */
    if ((!TY_AR_const_stride(array_ty, i)) &&
        (TY_AR_stride_val(array_ty, i) == 0))
      return FALSE;

    /* Not even non-unit stride. Only first dim has stride == elem_size.
     * For the other dimensions the stride gets multiplied,
     * so that stride is always byte increment in a single dimension.
     */
    if ((i == ndims-1) && (TY_AR_stride_val(array_ty, i) != elem_size))
      return FALSE;
  }
  return TRUE;
}

/***********************************************************************
 *
 * Read a distribute pragma. 
 * Return pointer to the statement node following the last pragma node 
 * in this set.
 * This involves the following steps:
 * 
 * Globals: 
 * Locals:
 * Formals:    
 *     Allocate and initialize DACT
 *     Insert DACT into hash table
 *
 ***********************************************************************/
WN* Read_Pragma_Distribute (WN* pwn) {
  WN* first_pwn = pwn;  /* store this, so that we can delete pragma nodes
                         * in case of error.
                         */
  ST *array_st = WN_st(pwn);

  TY_IDX array_ty = Lego_Get_Array_Type(array_st);

  if (TY_kind(array_ty) != KIND_ARRAY) {
    printf ("Pragma Distribute on a non-array\n");
    WN* retval = LWN_Get_Next_Stmt_Node(pwn);
    LWN_Delete_Tree_From_Block (pwn);
    return retval;
  }

  if (!Array_TY_OK (array_ty)) {
    ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(pwn),
                 WN_pragmas[WN_pragma(pwn)].name,
                 "Bad array type, ignoring.");
    WN* retval = LWN_Get_Next_Stmt_Node(pwn);
    LWN_Delete_Tree_From_Block (pwn);
    return retval;
  }
    
  mINT16 ndims = TY_AR_ndims (array_ty);

  INT i;


#ifdef Is_True_On
  FmtAssert (ST_class(array_st) == CLASS_VAR,
             ("Distributed array is not a variable"));
  FmtAssert (TY_AR_ndims(array_ty) > 0, ("Array with zero dimensions?\n"));
#endif


  /* Compile-Time data structures:
   * allocate, initialize, store in hash-table.
   */

  // If it is a VLA, move the alloca
  if (Is_VLA (array_st)) {
    if (PU_src_lang(CURRENT_SYMTAB) == PU_C_LANG ||
        PU_src_lang(CURRENT_SYMTAB) == PU_CXX_LANG) {
      Move_Alloca (array_st);
    }
  }

  DISTR_ARRAY* dact = New_DACT (&pwn, array_st, ndims);
  if (dact == NULL) {
    /* all distributions were DISTRIBUTE_STAR */
    WN* retval;
    while (first_pwn != pwn) {
      retval = LWN_Get_Next_Stmt_Node (first_pwn);
      LWN_Delete_Tree_From_Block (first_pwn);
      first_pwn = retval;
    }
    retval = LWN_Get_Next_Stmt_Node (first_pwn);
    LWN_Delete_Tree_From_Block (first_pwn);
    return retval;
  }

  DISTR_INFO* dinfo = da_hash->Find(array_st);
  if (!dinfo) {
    SYMBOL* st_sym = CXX_NEW (SYMBOL(array_st, (WN_OFFSET) 0, 0),
                              LEGO_pool);
    dinfo = CXX_NEW (DISTR_INFO (FALSE, ndims, st_sym), LEGO_pool);
    if (dynamic_list && dynamic_list->Find (array_st)) dinfo->Set_Dynamic();
    da_hash->Enter (array_st, dinfo);
    da_stack->Push (dinfo);
  }
  dinfo->Add_Dact (dact);
  dact->Convert_Expr_To_Symbol ();
  DB_PRINT(printf ("Distribute DACT\n");
           dact->Print (stdout));

  return LWN_Get_Next_Stmt_Node(pwn);
} /* Read_Pragma_Distribute () */



/***********************************************************************
 *
 * Consume a distribute-reshape pragma. Return pointer to the statement 
 * node following the last pragma node in this set.
 * This involves the following steps:
 * 
 * Globals: 
 * Locals:
 * Formals:    
 *     Allocate and initialize DACT
 *     Insert DACT into hash table
 *
 ***********************************************************************/
static WN* Read_Pragma_Distribute_Reshape (WN* pwn) {
  WN* first_pwn = pwn;
  ST* array_st = WN_st(pwn);

  if (ST_sclass(array_st) == SCLASS_DGLOBAL) {
    /* Defined (initialized) C global data, allocated in this module */
    FmtAssert (FALSE, ("Cannot reshape initialized data (%s)",
                       ST_name(array_st)));
    WN* retval = LWN_Get_Next_Stmt_Node(pwn);
    LWN_Delete_Tree_From_Block (pwn);
    return retval;
  }

  if (ST_is_initialized(array_st)) {
    /* cannot handle initialized ST for now */
    FmtAssert (FALSE, ("Cannot reshape initialized data (%s)",
                       ST_name(array_st)));
    WN* retval = LWN_Get_Next_Stmt_Node(pwn);
    LWN_Delete_Tree_From_Block (pwn);
    return retval;
  }

  TY_IDX array_ty = Lego_Get_Array_Type (array_st);

  if (TY_kind(array_ty) != KIND_ARRAY) {
    FmtAssert (FALSE, ("Pragma Distribute-Reshape on a non-array (%s)\n",
                       ST_name(array_st)));
    WN* retval = LWN_Get_Next_Stmt_Node(pwn);
    LWN_Delete_Tree_From_Block (pwn);
    return retval;
  }

  if (!Array_TY_OK (array_ty)) {
    ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(pwn),
                 WN_pragmas[WN_pragma(pwn)].name,
                 "Bad array type, ignoring.");
    FmtAssert (FALSE, ("Cannot reshape array %s\n",
                       ST_name(array_st)));
    WN* retval = LWN_Get_Next_Stmt_Node(pwn);
    LWN_Delete_Tree_From_Block (pwn);
    return retval;
  }

  mINT16 ndims = TY_AR_ndims (array_ty);
  FmtAssert (ST_class(array_st) == CLASS_VAR,
             ("Distribute-reshaped array is not a variable"));
  FmtAssert (TY_AR_ndims(array_ty) > 0, ("Array with zero dimensions?\n"));

  DISTR_ARRAY* dact = New_DACT (&pwn, array_st, ndims);
  if (dact == NULL) {
    FmtAssert (FALSE, ("Could not process distribute_reshape %s\n",
                       ST_name(array_st)));
    WN* retval;
    while (first_pwn != pwn) {
      retval = LWN_Get_Next_Stmt_Node (first_pwn);
      LWN_Delete_Tree_From_Block (first_pwn);
      first_pwn = retval;
    }
    retval = LWN_Get_Next_Stmt_Node (first_pwn);
    LWN_Delete_Tree_From_Block (first_pwn);
    return retval;
  }

  DISTR_INFO* dinfo = da_hash->Find(array_st);
  if (!dinfo) {
    SYMBOL* st_sym = CXX_NEW (SYMBOL(array_st, (WN_OFFSET) 0, 0),
                              LEGO_pool);
    dinfo = CXX_NEW (DISTR_INFO (TRUE, ndims, st_sym), LEGO_pool);
    if (dynamic_list->Find (array_st)) dinfo->Set_Dynamic();
    da_hash->Enter (array_st, dinfo);
    da_stack->Push (dinfo);
  }
  dinfo->Add_Dact (dact);
  dact->Convert_Expr_To_Symbol();
  DB_PRINT(printf ("Distribute-Reshape DACT\n");
           dact->Print (stdout));
  if (ST_Var_Kind(array_st) == var_common)
    PU_has_reshaped_commons = TRUE;
  return LWN_Get_Next_Stmt_Node(pwn);
} /* Read_Pragma_Distribute_Reshape () */

/***********************************************************************
 *
 * Consume a redistribute pragma. Return pointer to the statement node
 * following the last pragma node in this set.
 *
 * This involves the following steps:
 * 
 * Globals, Formals and Locals: 
 *     Allocate and initialize DACT 
 *     Insert DACT into redistribute hash table
 *
 * This routine could be called either during Lego_Read_Pragmas
 * or during Lego_Lower_Pragmas (the latter is done because since the
 * redistribute pragmas may be moved around during LNO, we re-read the 
 * redistribute pragmas.
 *
 ***********************************************************************/
extern WN* Read_Pragma_Redistribute (WN* pwn, BOOL gen_phase) {
  /* Compile time data structures:
   * Generate a def at the redistribute point, and a use at each use for
   * affinity scheduling. DU-chains will help identify reaching
   * redistributes, that may enable compile-time affinity scheduling,
   * otherwise dynamic.
   */
  WN* first_pwn = pwn;
  ST* array_st = WN_st(pwn);

  TY_IDX array_ty = Lego_Get_Array_Type(array_st);

  if (TY_kind(array_ty) != KIND_ARRAY) {
    printf ("Pragma ReDistribute on a non-array\n");
    WN* retval = LWN_Get_Next_Stmt_Node(pwn);
    LWN_Delete_Tree_From_Block (pwn);
    return retval;
  }

  if (!Array_TY_OK (array_ty)) {
    ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(pwn),
                 WN_pragmas[WN_pragma(pwn)].name,
                 "Bad array type, ignoring.");
    WN* retval = LWN_Get_Next_Stmt_Node(pwn);
    LWN_Delete_Tree_From_Block (pwn);
    return retval;
  }

  mINT16 ndims = TY_AR_ndims (array_ty);

  INT i;

#ifdef Is_True_On
  FmtAssert (ST_class(array_st) == CLASS_VAR,
             ("ReDistributed array is not a variable"));
  FmtAssert (TY_AR_ndims(array_ty) > 0, ("Array with zero dimensions?\n"));
#endif

  // If it is a VLA, move the alloca
  if (Is_VLA (array_st)) {
    if (PU_src_lang(CURRENT_SYMTAB) == PU_C_LANG ||
        PU_src_lang(CURRENT_SYMTAB) == PU_CXX_LANG) {
      Move_Alloca (array_st);
    }
  }

  DISTR_ARRAY* dact = New_DACT (&pwn, array_st, ndims);
  if (dact == NULL) {
    /* all distributions were DISTRIBUTE_STAR */
    WN* retval;
    while (first_pwn != pwn) {
      retval = LWN_Get_Next_Stmt_Node (first_pwn);
      LWN_Delete_Tree_From_Block (first_pwn);
      first_pwn = retval;
    }
    retval = LWN_Get_Next_Stmt_Node (first_pwn);
    LWN_Delete_Tree_From_Block (first_pwn);
    return retval;
  }

  DISTR_INFO* dinfo = da_hash->Find(array_st);
  if (!dinfo) {
    SYMBOL* st_sym = CXX_NEW (SYMBOL(array_st, (WN_OFFSET) 0, 0),
                              LEGO_pool);
    dinfo = CXX_NEW (DISTR_INFO (FALSE, ndims, st_sym), LEGO_pool);
    if (dynamic_list->Find (array_st)) dinfo->Set_Dynamic();
    da_hash->Enter (array_st, dinfo);
    da_stack->Push (dinfo);
  }
  if (gen_phase) dinfo->Add_Gen_Redistr (dact);
  else dinfo->Add_Redistr (dact);
  dact->Convert_Expr_To_Symbol();
  DB_PRINT(printf ("Redistribute DACT\n");
           dact->Print (stdout));

  return LWN_Get_Next_Stmt_Node(pwn);
} /* Read_Pragma_Redistribute () */



/***********************************************************************
 *
 * Given a pointer to the first whirl pragma node,
 * consume all the pragma nodes belonging to this 
 * generic data distribution pragma and return a 
 * dact. As a side effect, the pointer to the pragma node
 * in the caller is bumped up to point to the last pragma
 * whirl node of this distribute.
 * Note that in addition to the pragma nodes, there is an
 * XPRAGMA containing a sample array reference that is also
 * part of this set of pragmas.
 *
 * Layout of data-distr pragma node:
 *
 * WN_pragma_index(wn)      8 bits      dimension number
 * WN_pragma_distr_type(wn) 8 bits      distribution type (star, block, etc)
 * WN_pragma_pad1(wn)      16 bits      unused
 *
 * If cyclic_const, then contains const value in node.
 * If cyclic_expr, then must be followed by an XPRAGMA node, for which kid0
 * gives a whirl expression which is an expression for cyclic-expr.
 *
 ***********************************************************************/
static DISTR_ARRAY* New_DACT (WN** pwn_addr, ST* array_st, INT ndims) {
  INT i;
  DISTR_DIM* dd;
  DISTR_ARRAY* dact = NULL;
  WN *pwn = *pwn_addr;
  WN *first_pwn = pwn;
  WN *last_pwn = pwn;
  INT distr_dim = 0;
  WN_PRAGMA_ID pragma_id = WN_PRAGMA_UNDEFINED;
  WN** bounds_wns;   // array to store the bounds expressions

  dd = CXX_NEW_ARRAY (DISTR_DIM, ndims, LEGO_pool);
  bounds_wns = CXX_NEW_ARRAY (WN*, ndims, LEGO_pool);
  for (i=0; i<ndims; i++) {
    // Process ith dimension. pwn points to node for ith dimension
    FmtAssert (WN_opcode(pwn) == OPC_PRAGMA,
               ("Distribute_Pragma: node isn't a pragma node\n"));
    pragma_id = (WN_PRAGMA_ID) WN_pragma(pwn);
    FmtAssert ((pragma_id == WN_PRAGMA_DISTRIBUTE) ||
               (pragma_id == WN_PRAGMA_DISTRIBUTE_RESHAPE) ||
               (pragma_id == WN_PRAGMA_REDISTRIBUTE),
               ("Distribute_Pragma: node isn't a distribute pragma\n"));
#ifdef Is_True_On
    if (WN_pragma_index(pwn) != i) {
      DevWarn ("Subscript mismatch: expect %d, got %d, in distr pragma",
               i, WN_pragma_index(pwn));
    }
#endif
    switch (WN_pragma_distr_type(pwn)) {
    case DISTRIBUTE_STAR:
      dd[i].Init_Block_Star ((DISTRIBUTE_TYPE) WN_pragma_distr_type(pwn));
      break;
    case DISTRIBUTE_BLOCK:
      distr_dim++;
      dd[i].Init_Block_Star ((DISTRIBUTE_TYPE) WN_pragma_distr_type(pwn));
      break;
    case DISTRIBUTE_CYCLIC_CONST:
      distr_dim++;
      dd[i].Init_Cyclic_Const ((DISTRIBUTE_TYPE) WN_pragma_distr_type(pwn),
                               (INT64) WN_pragma_arg2(pwn), pwn);
      break;
    case DISTRIBUTE_CYCLIC_EXPR:
      distr_dim++;
      last_pwn = pwn;
      pwn = LWN_Get_Next_Stmt_Node(pwn);
      FmtAssert (WN_operator(pwn) == OPR_XPRAGMA,
                 ("Expected an XPRAGMA node"));
      FmtAssert (WN_pragma(pwn) == pragma_id,
                 ("Unexpected type of XPRAGMA node"));
      if ((pragma_id == WN_PRAGMA_DISTRIBUTE_RESHAPE) &&
          (WN_operator(WN_kid0(pwn)) == OPR_INTCONST) &&
          (WN_const_val(WN_kid0(pwn)) == 0)) {
        FmtAssert (ST_Var_Kind(array_st) == var_formal,
                   ("cyclic_expr(0) can only be on formal parameters"));
        FmtAssert (WN_pragma_compiler_generated(pwn),
                   ("cyclic_expr(0) should be a compiler-generated pragma"));
      }
      dd[i].Init_Cyclic_Expr ((DISTRIBUTE_TYPE)
                              WN_pragma_distr_type(last_pwn),
                              WN_kid0 (pwn));
      break;
    default:
      FmtAssert (FALSE, ("Strange distribute type\n"));
      break;
    }
    last_pwn = pwn;
    pwn = LWN_Get_Next_Stmt_Node(pwn);

    /* Now get the bounds expression for this node */
    FmtAssert (WN_opcode(pwn) == OPC_XPRAGMA,
               ("Distribute_Pragma: expected a bounds xpragma\n"));
    pragma_id = (WN_PRAGMA_ID) WN_pragma(pwn);
    FmtAssert (pragma_id == WN_pragma(pwn),
               ("Distribute_Pragma: bound has different pragma type\n"));

    // Check that we don't have an adjustable size array (local or formal)
    // if the function has alternate entry points. (nenad, 96/08/28)
    //
    if (PU_has_altentry(Get_Current_PU()) &&
        WN_operator(WN_kid0(pwn)) != OPR_INTCONST) {
      ErrMsgSrcpos(EC_DRA_unsupported_type, 
                   WN_Get_Linenum(pwn),
                   WN_pragmas[WN_pragma(pwn)].name,
                   ST_name(array_st),
                   "Distribution of adjustable size arrays in functions with alternate entry points is currently not supported");
    }
    
    bounds_wns[i] = pwn;
    last_pwn = pwn;
    pwn = LWN_Get_Next_Stmt_Node(pwn);
  } /* end for */

  // now see if there is an ONTO clause
  INT64* onto = NULL;
  {
    WN* onto_wn = pwn;
    // preopt is getting rid of the stores. so make them optional for now
    INT count = 0;
    while (onto_wn) {
      if (WN_operator(onto_wn) != OPR_STID) break;
      count++;
      if (count > 2) break;
      onto_wn = LWN_Get_Next_Stmt_Node (onto_wn);
    }
    if (onto_wn &&
        WN_opcode(onto_wn) == OPC_XPRAGMA &&
        WN_pragma(onto_wn) == WN_PRAGMA_ONTO) {
      // there is an onto pragma
      onto = CXX_NEW_ARRAY (INT64, ndims, LEGO_pool);
      for (INT i=0; i<ndims; i++) {
        onto[i] = -1;
        if (dd[i].Distr_Type() != DISTRIBUTE_STAR) {
          Is_True (onto_wn &&
                   WN_opcode(onto_wn) == OPC_XPRAGMA &&
                   WN_pragma(onto_wn) == WN_PRAGMA_ONTO,
                   ("Searching for ONTO: not enough of them\n"));
          Is_True (WN_kid0(onto_wn) &&
                   WN_operator(WN_kid0(onto_wn))==OPR_INTCONST,
                   ("Searching for ONTO: expected an INT-CONST kid\n"));
          onto[i] = WN_const_val(WN_kid0(onto_wn));
          last_pwn = onto_wn;
          onto_wn = WN_next(onto_wn);
        }
      }
      pwn = onto_wn;
    }
  }

  if (distr_dim) {
    SYMBOL* st_sym = CXX_NEW (SYMBOL(array_st, (WN_OFFSET) 0, 0),
                              LEGO_pool);
    dact = CXX_NEW (DISTR_ARRAY(dd, first_pwn, last_pwn, bounds_wns, onto),
                    LEGO_pool);

  }
  else {
    CXX_DELETE_ARRAY (dd, LEGO_pool);
    CXX_DELETE_ARRAY (bounds_wns, LEGO_pool);
  }
  *pwn_addr = last_pwn;
  return dact;
} /* New_DACT */
