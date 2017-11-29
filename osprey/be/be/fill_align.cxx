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


#include "defs.h"
#include "stab.h"
#include "wn.h"
#include "ir_reader.h"
#include "strtab.h"
#include "stblock.h"
#include "data_layout.h"
#include "erbe.h"
#include "cxx_hash.h"

/*
 *
 * Only one exported routine:
 *  Process_Fill_Align_Pragmas (WN* func_wn)
 *
 */

extern "C" {
  void Process_Fill_Align_Pragmas (WN* func_wn);
}

/* From <targ>/fill_align_targ.cxx
 */
void Fill_Align_Initialize_Parameters (INT *L1_sz, INT *L2_sz, INT *pg_sz);

static void Process_Fill_Align_Pragmas_Recursive (WN* wn, WN* func_wn);
static void Fill_Align_Symbol (WN* wn, WN* func_wn);

typedef HASH_TABLE<ST*, BOOL> ST_HASH_TABLE;
static ST_HASH_TABLE *symbol_ht;
static MEM_POOL symbol_ht_pool;
static INT L1_cache_line_sz, L2_cache_line_sz, page_sz;

/*
 *
 * Given the PU-tree in func_wn and the current node,
 * recursively walk the tree looking for fill/align
 * pragmas, and process them.
 *
 */
void Process_Fill_Align_Pragmas (WN* wn) {


  /* Notice that this memory-pool is never deleted, and basically "leaks"
   * memory. But since it must survive all the way through all the PUs
   * in the back-end, it doesn't matter, since it will be freed when be
   * is basically over.   
   */
  static INT initialized = 0;
  if (!initialized) {
    initialized = 1;
    Fill_Align_Initialize_Parameters (&L1_cache_line_sz,
				      &L2_cache_line_sz, 
				      &page_sz);
    MEM_POOL_Initialize (&symbol_ht_pool, "Fill Align Pool", FALSE);
    MEM_POOL_Push_Freeze (&symbol_ht_pool);
    symbol_ht = CXX_NEW (ST_HASH_TABLE (20, &symbol_ht_pool), &symbol_ht_pool);
  }

  Process_Fill_Align_Pragmas_Recursive (wn, wn);
}

static void Process_Fill_Align_Pragmas_Recursive (WN* wn, WN* func_wn) {
  OPCODE opc;
  OPERATOR oper;
  INT i;

  if (!wn) return;

  opc = WN_opcode(wn);
  oper = OPCODE_operator(opc);
  

  if (oper == OPR_PRAGMA) {
    if (WN_pragma(wn) == WN_PRAGMA_FILL ||
        WN_pragma(wn) == WN_PRAGMA_ALIGN) {
      Fill_Align_Symbol (wn, func_wn);
    }
    return;
  }

  if (WN_operator(wn) == OPR_BLOCK) {
    WN* kid = WN_first(wn);
    while (kid) {
      Process_Fill_Align_Pragmas_Recursive (kid, func_wn);
      kid = WN_next(kid);
    }
    return;
  }

  for (i=0; i<WN_kid_count(wn); i++) {
    Process_Fill_Align_Pragmas_Recursive (WN_kid(wn, i), func_wn);
  }
  return;
}


/*
 *
 * Local routines.
 *
 */

static BOOL Local_Variable (ST* st) {

  if (ST_sclass(st) == SCLASS_AUTO ||
      ST_sclass(st) == SCLASS_PSTATIC ||
      (ST_base(st) &&
       ST_sclass(ST_base(st)) == SCLASS_AUTO))
    return TRUE;
  else return FALSE;

}

static BOOL Global_Variable (ST* st) {

  if (Is_Global_Symbol(st) && ST_sclass(st) != SCLASS_COMMON)
    return TRUE;
  else return FALSE;
}

static BOOL Common_Variable (ST* st) {

  if (ST_sclass(st) == SCLASS_COMMON) {
    Is_True (Is_Global_Symbol(st) && ST_base_idx(st) != ST_st_idx(st),
             ("Symbol %s is SCLASS_COMMON but is weird\n", ST_name(st)));
    return TRUE;
  }
  else return FALSE;

}

/*
 *
 * Must be called for local or global variables.
 * Return TRUE if all sizes are completely specified and constant,
 * FALSE otherwise (i.e. if some size is not supplied, or is variable).
 *
 */
static BOOL Known_Size (ST* st) {
  
  INT ndims;
  INT i;

  TY& ty = Ty_Table[ST_type(st)];

  /* only arrays can be under-specified */
  if (TY_kind(ty) != KIND_ARRAY) return TRUE;

  /* extern int a[] should come out as non-const upper-bound,
   * with a NULL tree.
   *
   * VLAs will come out as non-const upper-bounds.
   */
  ndims = TY_AR_ndims(ty);
  for (i=0; i<ndims; i++) {
    if (!TY_AR_const_lbnd(ty, i) ||
        !TY_AR_const_ubnd(ty, i))
      return FALSE;
  }
  return TRUE;
  
}

/* recursively search, looking inside regions, for an alloca */
static WN *
Find_Alloca (WN *block, ST *sym)
{
    Is_True (block && WN_opcode(block) == OPC_BLOCK,
             ("find_alloca: expected a OPC_BLOCK node"));
    WN *alloca_wn = WN_first(block);
    while (alloca_wn) {
      if ((WN_operator(alloca_wn) == OPR_STID) &&
          (&St_Table[WN_st_idx(alloca_wn)] == sym)) {	// found it
		return alloca_wn;
      }
      // Change this to a DevWarn, since the front-end cannot always
      // move the alloca before the preamble (in the presence of
      // initialization, for example).
      if (WN_operator(alloca_wn) == OPR_PRAGMA &&
          WN_pragma(alloca_wn) == WN_PRAGMA_PREAMBLE_END) {
        DevWarn ("Reached end of preamble w/o finding alloca of %s\n",
                 ST_name(sym));
      }
      else if (WN_opcode(alloca_wn) == OPC_REGION) {
	WN *alloca_in_region;
	alloca_in_region = Find_Alloca (WN_region_body(alloca_wn), sym);
	if (alloca_in_region != NULL) { 	// found it
		return alloca_in_region;
	}
      }
      alloca_wn = WN_next(alloca_wn);
    }
    return alloca_wn;
}

/*
 *
 * Given a fill/align pragma in wn, and the WN_func_body of a PU-tree in
 * body_wn, process the fill/align pragma by padding/aligning the symbol
 * appropriately.
 *
 * A fill_symbol (s, val) pragma requires that the symbol s be allocated
 * with enough padding around it such that there is no other data item
 * within "val" alignment at either end. For instance, if val is 32, then
 * the symbol "s" should not share a level-one cache line (32-bytes) with
 * any other data.
 *
 * A align_symbol (s, val) pragma requires that the symbol s be aligned
 * to start with the supplied alignment in "val".
 *
 * This routine processes all kinds of symbols, including
 *  - local variables
 *  - dynamically-size local arrays (VLAs)
 *  - global variables (C/C++)
 *  - common block variables (Fortran).
 * 
 */

static void Fill_Align_Symbol (WN* wn, WN* func_wn) {
  ST* st = &St_Table[WN_st_idx(wn)];

  INT orig_size;        /* size of original variable */
  INT new_size;         /* new size after fill */
  INT pad;              /* amount of pad required to fill */
  INT orig_align;       /* original alignment of variable */
  INT log_orig_align=0; /* log of original alignment */
  INT64 new_ofst;       /* offset into padded section after variable
                         * has been filled.
                         */
  INT fa_value;         /* power of 2 storage size that must contain
                         * this variable.
                         */

  BOOL is_fill;         /* True if it is a fill, false otherwise
                         * i.e. align
                         */

  WN* body_wn = WN_func_body(func_wn);

  BOOL ignore_global_symbols = FALSE;   // when IPA is enabled, we
                                        // want to process global
                                        // symbols only in the
                                        // symtab.G file, and NOT in
                                        // each .I file.

  if (FILE_INFO_ipa(File_info) && Read_Global_Data) {
    // IPA is enabled, and this is not symtab.G
    ignore_global_symbols = TRUE;
  }

  is_fill = (WN_pragma(wn) == WN_PRAGMA_FILL);

  fa_value = WN_pragma_arg2(wn);
  switch (fa_value) {
  case -1:
    /* L1 cache line */
    fa_value = L1_cache_line_sz;
    break;
  case -2:
    /* L2 cache line */
    fa_value = L2_cache_line_sz;
    break;
  case -3:
    /* page */
    fa_value = page_sz;
    break;
  default:
    break;
  }

  /* Error-checking cases */

  /* must be a variable */
  if (!(ST_class(st) == CLASS_VAR && (Local_Variable(st) ||
                                      Global_Variable(st) ||
                                      Common_Variable(st)))) {
    ErrMsgSrcpos (EC_Bad_Pragma_Abort, WN_Get_Linenum(wn),
                  WN_pragmas[WN_pragma(wn)].name,
                  ST_name(st),
                  "must be supplied a variable");
  }


  /* global variables must be completely specified */
  if  (Global_Variable(st) && !Known_Size(st)) {
    ErrMsgSrcpos (EC_Bad_Pragma_Abort, WN_Get_Linenum(wn),
                  WN_pragmas[WN_pragma(wn)].name,
                  ST_name(st),
                  "global variable must be completely specified");
  }


  /* cannot align local fixed-size variables to greater than 16 bytes */
  if (Local_Variable(st) && Known_Size(st) && !is_fill && fa_value > 16) {
    ErrMsgSrcpos (EC_Bad_Pragma_Abort, WN_Get_Linenum(wn),
                  WN_pragmas[WN_pragma(wn)].name,
                  ST_name(st),
                  "cannot align automatic variables to greater than 16 bytes");
  }

  if (Global_Variable(st) && ignore_global_symbols) {
    return;
  }
  

  /* duplicates can arise due to inlining */
  if (Is_Global_Symbol(st)) {
    if (symbol_ht->Find (st)) {
      /* printf ("Duplicate: %s\n", ST_name(st)); */
      return;
    }
    else symbol_ht->Enter (st, TRUE);
  }

  /* Only common variables can have an offset */
  FmtAssert (Common_Variable(st) || ST_ofst(st) == 0,
             ("Fill/Align_Symbol (%s): ST has an unexpected offset %llu\n",
              ST_name(st), ST_ofst(st)));


  // IPA can sometimes mark symbols to be GP-rel, and we might expand
  // them beyond the gp-size, leading to problems later. So clear the
  // gp-rel bit.
  Clear_ST_gprel(st);

  /* compute the original alignment */

  orig_align = TY_align(ST_type(st));
  while (orig_align) {
    if (orig_align & 0x1) break;
    log_orig_align++;
    orig_align = orig_align >> 1;
  }
  orig_align = TY_align(ST_type(st));


  /* compute the original and new sizes */

  orig_size = TY_size(ST_type(st));
  if (is_fill) {

    if (Local_Variable(st) && Known_Size(st) && is_fill) {
      /* pad on each side, since we cannot align on the stack.
       * the amount of front-pad is the larger of the desired fa_value
       * and the original alignment; the trailing pad is just fa_value.
       */
      if (orig_align > fa_value) {
        pad = orig_align+fa_value;
        new_ofst = orig_align;
      }
      else {
        pad = 2*fa_value;
        new_ofst = fa_value;
      }
      new_size = orig_size + pad;
    }
    else {
      INT tmp;
      tmp = orig_size & (fa_value-1);
      if (tmp) new_size = orig_size+fa_value-tmp;
      else new_size = orig_size;
      pad = fa_value-tmp;    /* amount of extra space */
      new_ofst = pad/2;
    }
  }
  else {
    new_size = orig_size;
    pad = 0;
    new_ofst = 0;
  }

  /* Now try to place the symbol someplace in the middle of the
   * new block. Must be subject to original alignment, though.
   */
  new_ofst = ((new_ofst >> log_orig_align) << log_orig_align);

             

  if (Global_Variable(st) || (Local_Variable(st) && Known_Size(st))) {

    /* global, or non-vla local. similar processing. */

    /*
     * printf ("%s (%s): glob/fixed-local. oldsize %d, new_size %d, pad %lld ofst %d\n",
     *            WN_pragmas[WN_pragma(wn)].name, ST_name(st),
     *            orig_size, new_size, pad, new_ofst);
     */
    
    if (orig_size != new_size) {

      /* create a base st, type class_block */
      ST* base_st;
      INT new_align;

      /* alignment of base block must be the larger of the original
       * alignment and the desired fa_value alignment.
       */
      new_align = (orig_align > fa_value ? orig_align : fa_value);

      base_st = New_ST_Block (Save_Str2("__fill_", ST_name(st)), 
		Is_Global_Symbol(st), 
		ST_sclass(st), new_align, new_size);
      Enter_ST(base_st);

      Set_ST_base(st, base_st);
      Set_ST_ofst(st, ST_ofst(st) + new_ofst);
    }
    else {

      /* size is unchanged; just align */

      if (TY_align(ST_type(st)) < fa_value) {
        TY_IDX new_ty = Copy_TY (ST_type(st));
        Set_TY_align(new_ty, fa_value);
        Set_ST_type(st, new_ty);
                
        FmtAssert (ST_ofst(st) == 0,
                   ("Fill/Align_Symbol (%s): ST has an unexpected offset %llu\n",
                    ST_name(st), ST_ofst(st)));
      }
    }
    if (Global_Variable(st)) {
      Allocate_Object (st);
    }
  }

  else if (Local_Variable(st) && !Known_Size(st)) {

    /* local VLA */
    
    WN* alloca_wn, *alloca_arg_wn, *alloca_store_wn;
    WN* arg_wn;
    ST* base_st = ST_base(st);      /* vlas are based, with a base-st */
    TYPE_ID rtype;
    WN *ldid_wn, *stid_wn;
    INT log_fa_value;
    INT tmp;

    /*
     * printf ("%s (%s): VLA\n", WN_pragmas[WN_pragma(wn)].name, ST_name(st));
     */

    /* find the alloca */
    alloca_wn = Find_Alloca (body_wn, base_st);
    FmtAssert (alloca_wn, ("Reached end of PU w/o finding alloca of %s\n",
                           ST_name(st)));

    alloca_store_wn = alloca_wn;
    if (Alloca_Dealloca_On) {
      FmtAssert (WN_operator(WN_kid0(alloca_wn)) == OPR_ALLOCA,
                 ("fill/align symbol (%s): Expected alloca", ST_name(st)));
    } else {
      alloca_wn = WN_prev(alloca_wn);
      FmtAssert (WN_operator(alloca_wn) == OPR_INTRINSIC_CALL,
                 ("fill/align symbol (%s): Expected alloca", ST_name(st)));
    }
    
    /* now allocate 1x to 2x of fa_value extra storage, to aid alignment
     * and padding (if fill).
     */
    alloca_arg_wn = WN_kid0(WN_kid0(alloca_wn));
    rtype   = WN_rtype(alloca_arg_wn);

    /* add 1x/2x fa_value to original size */
    arg_wn = WN_CreateExp2 (OPCODE_make_op(OPR_ADD, rtype, MTYPE_V),
                            alloca_arg_wn, 
                            WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST,
                                                             rtype,
                                                             MTYPE_V),
                                              is_fill?(fa_value*2):fa_value));
    WN_kid0(WN_kid0(alloca_wn)) = arg_wn;

    /* now pad the start address up to fill-value alignment.
     * basically do:
     *  p = ((p+fa_value-1) >> log_fa_value) << log_fa_value
     */
    tmp = fa_value;
    log_fa_value = 0;
    while (tmp) {
      if (tmp & 0x1) break;
      log_fa_value++;
      tmp = tmp >> 1;
    }

    /* now we're manipulating the return value of the alloca */
    rtype = WN_desc(alloca_store_wn);

    ldid_wn = WN_RLdid(Mtype_comparison(rtype), rtype, 0, base_st, ST_type(base_st));
    ldid_wn = WN_CreateExp2 (OPCODE_make_op(OPR_ADD, rtype, MTYPE_V),
                             ldid_wn,
                             WN_CreateIntconst (OPCODE_make_op(OPR_INTCONST,
                                                               rtype,
                                                               MTYPE_V),
                                                fa_value-1));
    ldid_wn = WN_CreateExp2 (OPCODE_make_op(OPR_ASHR, rtype, MTYPE_V),
                             ldid_wn,
                             WN_CreateIntconst (OPCODE_make_op(OPR_INTCONST,
                                                               rtype,
                                                               MTYPE_V),
                                                log_fa_value));
    ldid_wn = WN_CreateExp2 (OPCODE_make_op(OPR_SHL, rtype, MTYPE_V),
                             ldid_wn,
                             WN_CreateIntconst (OPCODE_make_op(OPR_INTCONST,
                                                               rtype,
                                                               MTYPE_V),
                                                log_fa_value));
    stid_wn = WN_CreateStid (OPCODE_make_op(OPR_STID, MTYPE_V, rtype),
                             0, base_st, ST_type(base_st), ldid_wn);
    WN_next(stid_wn) = WN_next(alloca_store_wn);
    WN_prev(WN_next(alloca_store_wn)) = stid_wn;
    WN_next(alloca_store_wn) = stid_wn;
    WN_prev(stid_wn) = alloca_store_wn;
  }

  else if (Common_Variable(st)) {

    /* common variable */

    /* compute new size. 
     * increase size of underlying common block
     * increase alignment of common block.
     * change offsets of this symbol, and all subsequent
     * offsets within that common.
     */


    /*
     * printf ("%s (%s): Common\n",WN_pragmas[WN_pragma(wn)].name,ST_name(st));
     */

    /* The common block needs expansion if the size has changed,
     * or even if the ofst is not a multiple of the fa_value.
     * The latter is necessary because we are going to up the
     * alignment of the common block, and need to ensure that the
     * ofst is a multiple of fa_value to ensure no false-sharing.
     */

    if (orig_size != new_size || (ST_ofst(st) % fa_value)) {

      /* must expand the common block */
      ST* cst;
      INT delta = new_size - orig_size;

      /* it is possible that the current offset is not fa_value aligned.
       * in which case we may need a pre-pad to get there.
       * compute that here.
       */
      delta += ST_ofst(st) % fa_value;
      new_size = orig_size + delta;
      new_ofst += ST_ofst(st) % fa_value;

      /* the change in size MUST be a multiple of 8,
       * to preserve the alignment of subsequent STs.
       */
      if (delta % 8) {
        delta = (((delta+7) >> 3) << 3);
        new_size = orig_size + delta;
      }

      INT i;
      FOREACH_SYMBOL(CURRENT_SYMTAB, cst, i) {
        if (ST_base(cst) == ST_base(st) && st != cst &&
            ST_ofst(cst) > ST_ofst(st)) {

          /* this element is in the same common, but after our symbol */
          Set_ST_ofst(cst, ST_ofst(cst) + delta);
        }
      }

      TY_IDX common_ty;
      
      /* now increase the size of the common block */
      common_ty = ST_type(ST_base(st));

      Is_True (TY_kind(common_ty) == KIND_STRUCT,
               ("fill/align_symbol (%s): expected type struct for common",
                ST_name(st)));

      /* now update the description of the underlying struct type */
      TY &ty = Ty_Table[common_ty];
      Set_TY_size(ty, TY_size(common_ty) + delta);
      FLD_ITER fld_iter = Make_fld_iter (TY_fld (ty));
      do {
	FLD_HANDLE field (fld_iter);
	if (FLD_ofst(field) == ST_ofst(st)) {
	  Set_FLD_ofst(field, FLD_ofst(field) + new_ofst);
	}
	else if (FLD_ofst(field) > ST_ofst(st)) {
	  Set_FLD_ofst(field, FLD_ofst(field) + delta);
        }
      } while (! FLD_last_field (fld_iter++));

      /* finally change the offset of the original st */
      Set_ST_ofst(st, ST_ofst(st) + new_ofst);

      /* Now increase the alignment of the common block, if needed */
      if (TY_align(common_ty) < fa_value) {
        TY_IDX new_ty = Copy_TY (common_ty);
        Set_TY_align(new_ty, fa_value);
        Set_ST_type(ST_base(st), new_ty);
      }
    }

    else {

      /* just need to adjust the alignment on the common block */

      TY_IDX common_ty = ST_type(ST_base(st));

      if (TY_align(common_ty) < fa_value) {
        TY_IDX new_ty = Copy_TY (common_ty);
        Set_TY_align(new_ty, fa_value);
        Set_ST_type(ST_base(st), new_ty);
      }
    }
  }
  else {
    /* error */
    ErrMsgSrcpos (EC_Bad_Pragma_Abort, WN_Get_Linenum(wn),
                  WN_pragmas[WN_pragma(wn)].name,
                  ST_name(st),
                  "variable must be local, global, or common");
  }
}
