/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Copyright (C) 2007, University of Delaware, Hewlett-Packard Company, 
//  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// ====================================================================

#include "defs.h"
#include "tracing.h"
#include "cxx_memory.h"
#include "opt_base.h"
#include "bb_node_set.h"
#include "opt_lmv_helper.h"
#include "opt_ivr.h"

// get_field_type() 
//   return the field type of aggregate type <agg_ty>
static TY_IDX
get_field_type (TY_IDX agg_ty, INT field_id) {
  UINT cur_field_id = 0;
  FLD_HANDLE fld = FLD_get_to_field (agg_ty, field_id, cur_field_id);
  Is_True (!fld.Is_Null(), ("Invalid field id %d for type %p",
                             field_id, agg_ty));
  return FLD_type (fld);
}

// return the object type of CK_VAR/CV_IVAR coderep
static TY_IDX
coderep_obj_ty (CODEREP* cr) {

  if (cr->Kind () == CK_VAR || cr->Kind() == CK_IVAR) {

    TY_IDX ty; 
    UINT32 fld_id;

    if (cr->Kind() == CK_VAR) {
      ty = cr->Lod_ty();
      fld_id = cr->Field_id ();
    } else {
      ty = cr->Ilod_ty();
      fld_id = cr->I_field_id ();
    }

    if (fld_id != 0) {
      if (TY_kind(ty) == KIND_STRUCT) {
        return get_field_type (ty, fld_id);
      } else {
      }
    }
    return ty;
  }

  return (TY_IDX)0;
}

/////////////////////////////////////////////////////////////////////////
//
//  Implementation of ADDR_LINEAR_EXPR
//
/////////////////////////////////////////////////////////////////////////
//
inline BOOL
ADDR_LINEAR_EXPR::operator == (const ADDR_LINEAR_EXPR& that) const {
  if (_flags != that._flags || _flags == 0) { 
    return FALSE;
  }
  if (Is_const ()) return _const == that._const;
  return _coeff == that._coeff && _const == that._const && 
         _cr == that._cr;
}

//
// Add two linear expr, if the results cannot be expressed
// as "coeff*var+const" return FALSE, otherwise, return TRUE.
//
BOOL
ADDR_LINEAR_EXPR::Add (const ADDR_LINEAR_EXPR &that) {
 
  Is_True ((Is_const() || Is_nonconst()) && 
           (that.Is_const() || that.Is_nonconst()),  
           ("linear addresses are not valid"));
  
  if (Is_const()) {
    INT v = Const_val();
    *this = that;
    _const += v;
    return TRUE;
  }

  if (that.Is_const ()) {
    _const += that.Const_val ();  
    return TRUE;
  }

  // not "compatible"
  if (that._cr != _cr) return FALSE;
  
  _coeff += that._coeff;
  _const += that._const;

  return TRUE;
}

void
ADDR_LINEAR_EXPR::Multiply (INT v) {
  Is_True (Is_const() || Is_nonconst(), ("not linear expr"));
  _const *= v;
}

/////////////////////////////////////////////////////////////////////////
//
//   Implementation of MA_POINTER
//
/////////////////////////////////////////////////////////////////////////
//
// return the TY_IDX of pointed objected 
TY_IDX
MA_POINTER::Pointed_ty (void) const {
  if (_ty == (TY_IDX)0) return (TY_IDX)0;
  if (TY_kind(_ty) != KIND_POINTER) {
    return (TY_IDX)0; // it is possible when pointer is PREG.
  }

  TY_IDX pt_ty = TY_pointed(_ty);

  if (_kind == MA_PTR_LDA && TY_kind(pt_ty) == KIND_STRUCT) {
    if (_is_lda._afield_id != (TY_IDX)0)
      pt_ty = get_field_type (pt_ty, _is_lda._afield_id);
  }
   
  return pt_ty;
}

/////////////////////////////////////////////////////////////////////////
//
//   Implementation of MA_OFFSET
//
/////////////////////////////////////////////////////////////////////////
//
BOOL 
MA_OFFSET::operator == (const MA_OFFSET& that) const {
  if (Kind () != that.Kind()) return FALSE;

  switch (Kind()) {
  case MA_OFST_FIXED:
  case MA_OFST_LINEAR:
    return _ofst.low == that._ofst.low;

  case MA_OFST_RANGE:
    return _ofst.low == that._ofst.low &&
           _ofst.high == that._ofst.high;

  case MA_OFST_TOO_MESSY:
    return Expr() == that.Expr() && Expr() != NULL;

  case MA_OFST_INVALID: 
  case MA_OFST_UNKNOWN:
    break;

  default:
    FmtAssert (FALSE, ("invalid ofst kind"));
  }
  return FALSE;
}

void
MA_OFFSET::Add (INT adjust) {
  switch (Kind()) {
  case MA_OFST_FIXED:
  case MA_OFST_LINEAR:
    _ofst.low.Add (adjust);
    break;

  case MA_OFST_RANGE:
    _ofst.low.Add (adjust);
    _ofst.high.Add (adjust);
    break;

  case MA_OFST_TOO_MESSY:
    if (adjust != 0) {
      Set_unknown_ofst ();
    }
    break;

  case MA_OFST_UNKNOWN:
    break;

  case MA_OFST_INVALID:
  default:  
    FmtAssert (FALSE, ("Offset is invalid")); 
    return;
  }
}

void
MA_OFFSET::Add (MA_OFFSET* adjust) {
  BOOL add_succ = TRUE;

  switch (_kind) {
  case MA_OFST_FIXED:
  case MA_OFST_LINEAR:
    if (adjust->Kind() == MA_OFST_FIXED ||
        adjust->Kind() == MA_OFST_LINEAR) {
      if (add_succ = _ofst.low.Add (*adjust->Linear_ofst())) {
        if (_ofst.low.Is_nonconst())
          _kind = MA_OFST_LINEAR;
      }
    } else if (adjust->Kind() == MA_OFST_RANGE) {

      const ADDR_LINEAR_EXPR_RANGE* r = adjust->Linear_ofst_range ();
      ADDR_LINEAR_EXPR low_expr = _ofst.low;
      ADDR_LINEAR_EXPR high_expr = _ofst.low;
      add_succ = low_expr.Add(r->low);
      _ofst.low = low_expr;
      if (add_succ) {
        add_succ = high_expr.Add(r->high);
        _ofst.high = high_expr;
      }
      _kind = MA_OFST_RANGE;
    } else {
      Set_unknown_ofst ();
    }

    break;

  case MA_OFST_RANGE:
    if (adjust->Kind() == MA_OFST_FIXED ||
        adjust->Kind() == MA_OFST_LINEAR) { 
      add_succ = _ofst.low.Add (*adjust->Linear_ofst());
      if (add_succ) 
        _ofst.high.Add (*adjust->Linear_ofst());
    } else if (adjust->Kind() == MA_OFST_RANGE) {
      const ADDR_LINEAR_EXPR_RANGE* r = adjust->Linear_ofst_range ();
      add_succ = _ofst.low.Add (r->low); 
      if (add_succ)
        add_succ = _ofst.high.Add (r->high);
    }
    break;

  case MA_OFST_TOO_MESSY:
    if (adjust->Kind() != MA_OFST_FIXED || 
        adjust->Get_fixed_ofst () != 0) {
      Set_unknown_ofst ();
    }
    break;

  case MA_OFST_UNKNOWN:
    break;

  case MA_OFST_INVALID:
  default:  
    FmtAssert (FALSE, ("Offset is invalid")); 
    return;
  }

  if (!add_succ) Set_unknown_ofst ();
}

// work out a offset range that (tightly) cover both <this> and <that>
//
void
MA_OFFSET::Union (const MA_OFFSET* that, LMV_LOOP_INFO* loopinfo) {

  Is_True (Kind() != MA_OFST_INVALID && that->Kind() != MA_OFST_INVALID,
           ("Invalid offset kind"));

  if (Kind() == MA_OFST_UNKNOWN || *this == *that) {
    return;
  }

  if ((INT)Kind() > (INT)that->Kind()) {
    // By swaping <this> and <that>, we only need to handle 
    // (n**2)/2 rather than n**2 combinations.
    //
    MA_OFFSET tmp;
    tmp = *that; 
    tmp.Union (this, loopinfo);
    *this = tmp;
    return;
  }

  INT low, high; // addr range
  if (Kind() == MA_OFST_FIXED) {
    switch (that->Kind()) {
    case MA_OFST_FIXED:
      {
        low = _ofst.low.Const_val();
        high = that->Get_fixed_ofst ();
        if (low < high) { INT t = low; low = high; high = t; }
        Set_linear_ofst_range (ADDR_LINEAR_EXPR(low), ADDR_LINEAR_EXPR(high));
      }
      return;

    case MA_OFST_LINEAR:
      {
        VAR_VAL_RANGE vr;
        that->Linear_ofst()->Get_range (&vr,loopinfo);
        ADDR_LINEAR_EXPR low_expr;
        ADDR_LINEAR_EXPR high_expr;
        if (vr.Low_is_const()) {
          low = Get_fixed_ofst();
          low = MIN(low, vr.Low_val());
          low_expr.Set_const_expr(low);
        }
        else {
          // Non-const lower bound not supported yet...
          break;
        }

        if (vr.High_is_const()) {
          high = Get_fixed_ofst();
          high = MAX(high, vr.High_val());
          high_expr.Set_const_expr(high);
        }
        else {
          Is_True(vr.Low_is_const(),("Unsupported non-const lower bound\n"));
          // Skip if fixed offset is > lower bound as we cannot
          // determine if it is < non-const upper bound

          // While this may make sense, the artificial initial value
          // of offset == 0 is problematic, need a better initial state.

          //if (Get_fixed_ofst() > vr.Low_val())
          //  break;
          const ADDR_LINEAR_EXPR *ofst_expr = that->Linear_ofst();
          INT const_val = ofst_expr->Const_part();
          if ( vr.High_is_cr_subone() )
            const_val -= 1;
          high_expr.Set_linear_expr(ofst_expr->Coefficient(),
              vr.High_cr(),const_val);
        }
        Set_linear_ofst_range(low_expr,high_expr);
        return;
      }
      break;

    case MA_OFST_RANGE:
      {
        if (that->_ofst.low.Is_invalid() || 
            that->_ofst.high.Is_invalid()) {
          break;
        }

        low = high = Get_fixed_ofst ();
	if (!that->_ofst.low.Is_const()) {
          VAR_VAL_RANGE vr ;
          loopinfo->Analyze_var_value_range (
                           that->_ofst.low.Var(), 
                           that->_ofst.low.Var_ver(), 
                           &vr);
          if (!vr.Low_is_const() || !vr.High_is_const())
            break;

          low = MIN(low, vr.Low_val ());
          high = MAX(high, vr.High_val ());
        }

	if (!that->_ofst.high.Is_const()) {
          VAR_VAL_RANGE vr ;
          loopinfo->Analyze_var_value_range (
                           that->_ofst.high.Var(), 
                           that->_ofst.high.Var_ver(), 
                           &vr);
          if (!vr.Low_is_const() || !vr.High_is_const())
            break;

          low = MIN(low, vr.Low_val ());
          high = MAX(high, vr.High_val ());
        }

        Set_linear_ofst_range (ADDR_LINEAR_EXPR(low), ADDR_LINEAR_EXPR(high));
      }
      return;
   
    case MA_OFST_TOO_MESSY:
      if (Get_fixed_ofst () == 0) {
        *this = *that;
        return;
      } 
      break;

    case MA_OFST_UNKNOWN:
      break;

    default:
      FmtAssert (FALSE, ("Invalid offset kind"));
    }

    Set_unknown_ofst (); 
    return;
  }

  if (Kind () == MA_OFST_LINEAR) {
    switch (that->Kind()) {
    case MA_OFST_LINEAR:
      {
        VAR_VAL_RANGE that_vr, this_vr;
        loopinfo->Analyze_var_value_range (
                    that->_ofst.low.Var(), 
                    that->_ofst.low.Var_ver(), 
                    &that_vr);

        // No non-const lower bound for now...
        if (!that_vr.Low_is_const())
          break;

        loopinfo->Analyze_var_value_range 
           (_ofst.low.Var(), _ofst.low.Var_ver(), &this_vr);

        // Non non-const lower bound for now...
        if (!this_vr.Low_is_const())
          break;
   
        // The lower bound of the combined range is simply the
        // minimum of the two ranges.
        low = MIN(that_vr.Low_val(), this_vr.Low_val());

        // When computing the upper bound, either both must be
        // constant or both non-const
        ADDR_LINEAR_EXPR high_expr;
        if (that_vr.High_is_const()) {
          if (!this_vr.High_is_const())
            break;
          high = MAX(that_vr.High_val(), that_vr.High_val());
          high_expr.Set_const_expr(high);
        }
        else {
          if (!this_vr.High_is_cr())
            break;
          if (this_vr.High_cr()->Aux_id() != that_vr.High_cr()->Aux_id() ||
              this_vr.High_cr()->Version() != that_vr.High_cr()->Version())
            break;
          high_expr.Set_linear_expr(1,this_vr.High_cr(),
              MAX(this_vr.High_is_cr_subone()?-1:0,
                  that_vr.High_is_cr_subone()?-1:0));
        }

        Set_linear_ofst_range (ADDR_LINEAR_EXPR(low), high_expr);
        return;
      }
      break;
   
    case MA_OFST_RANGE:
      {
        VAR_VAL_RANGE this_vr;
        loopinfo->Analyze_var_value_range (
                            this->_ofst.low.Var(),
                            this->_ofst.low.Var_ver(),
                            &this_vr);

        // No non-const lower bound for now...
        if (!this_vr.Low_is_const())
          break;

        ADDR_LINEAR_EXPR low_expr;
        ADDR_LINEAR_EXPR high_expr;
        const ADDR_LINEAR_EXPR *this_expr = this->Linear_ofst();
        const ADDR_LINEAR_EXPR_RANGE* r = that->Linear_ofst_range ();

        Is_True(r->low.Is_const(),("Expected const lower bound"));
        // Compute the lower bound of the linear offset from the
        // and merge it into the range offset. We keep the minimum.
        INT linear_low = this_expr->Coefficient() * this_vr.Low_val() +
            this_expr->Const_part();
        low_expr.Set_const_expr(MIN(linear_low,r->low.Const_val()));

        // Now we do the same for the upper bound of the linear offset
        if (this_vr.High_is_const()) {
          if (!r->high.Is_const())
            break;
          INT linear_high = this_expr->Coefficient() * this_vr.High_val() +
              this_expr->Const_part();
          high_expr.Set_const_expr(MAX(linear_high,r->high.Const_val()));
        }
        else {
          if (!this_vr.High_is_cr())
            break;
          if (r->high.Is_const() || 
	      this_vr.High_cr() != r->high.cr())
            break;

          INT const_val = this_expr->Const_part();
          if (this_vr.High_is_cr_subone())
            const_val -= 1;
          high_expr.Set_linear_expr(this_expr->Coefficient(),this_vr.High_cr(),
              MAX(const_val,r->high.Const_part()));
        }

        Set_linear_ofst_range(low_expr,high_expr);
        return;
      }
      break;

    case MA_OFST_TOO_MESSY:
    case MA_OFST_UNKNOWN:
      break;

    default:
      FmtAssert (FALSE, ("unknown offset kind"));
    }
    Set_unknown_ofst ();
    return;
  }
  
  if (Kind() == MA_OFST_RANGE) {
    switch (that->Kind()) {
    case MA_OFST_RANGE: 
      {
        MA_OFFSET t1(&that->Linear_ofst_range()->low);
        Union (&t1, loopinfo); 
        if (Kind() == MA_OFST_UNKNOWN) break;

        MA_OFFSET t2(&that->Linear_ofst_range()->high);
        Union (&t2, loopinfo); 
        return;
      }
      break;

    case MA_OFST_TOO_MESSY: 
    case MA_OFST_UNKNOWN: 
      Set_unknown_ofst (); 
      break; 

    default:
      FmtAssert (FALSE, ("Unkown offset kind"));
    }
    Set_unknown_ofst ();
    return;
  }
  
  if (Kind() == MA_OFST_TOO_MESSY) {
    Set_unknown_ofst ();
  } else {
    FmtAssert (FALSE, ("omit other offset kind"));
  }
}

void
MEM_ACCESS::Adjust_ofst (MA_POINTER* ptr, MA_OFFSET* ofst) {
  ptr = ptr;  
  _ofst.Add (ofst); 
}

MEM_ACCESS_ANALYZER::MEM_ACCESS_ANALYZER 
  (OPT_STAB* opt_stab, LMV_LOOP_INFO* loopinfo,
   MEM_POOL* mp, BOOL trace): 
  _mp(mp), _opt_stab(opt_stab), _cfg(opt_stab->Cfg()), _loopinfo(loopinfo), 
  _ma_map (256, NULL, _mp, FALSE), _ptr_mgr (_mp), _all_ma(_mp) {

    Is_True(_loopinfo->Loop()->Child() == NULL,
        ("MEM_ACCESS_ANALYZER can handle only inner most loops"));

    _ma_map.Init ();
    _read_cnt = _write_cnt = 0;
    _last_ma_id = 1;
    _trace = trace;
    _trace_detail = trace;
}

inline MEM_ACCESS*
MEM_ACCESS_ANALYZER::Alloc_mem_access (void) {
  MEM_ACCESS* ma = CXX_NEW (MEM_ACCESS(_last_ma_id++), _mp);
  _all_ma.push_back (ma);
  return ma;
}

// return TRUE if <cr> is definitely of pointer type 
BOOL
MEM_ACCESS_ANALYZER::Expr_of_ptr_ty (CODEREP* cr) {
  
  if (cr->Kind () == CK_VAR) {
    ST* st = _opt_stab->Aux_stab_entry (cr->Aux_id())->St ();
    if (ST_class(st) == CLASS_VAR || (ST_class(st) == CLASS_PREG)) {
      return TY_kind(cr->Lod_ty()) == KIND_POINTER;
    } else {
      return FALSE;
    }
  } else if (cr->Kind () == CK_IVAR) {
    TY_IDX ld_ty = cr->Ilod_ty(); 
    if (cr->I_field_id() != (TY_IDX)0) {
      ld_ty = get_field_type (ld_ty, cr->I_field_id());
    }
    return TY_kind(ld_ty) == KIND_POINTER;
  } else if (cr->Kind() == CK_OP) {
    if (cr->Opr() == OPR_ARRAY) {
      return TRUE;
    } else if (cr->Opr() == OPR_ADD && 
        Pointer_Size == MTYPE_byte_size(cr->Dtyp())) {
      BOOL b1 = Expr_of_ptr_ty (cr->Opnd(0));
      BOOL b2 = Expr_of_ptr_ty (cr->Opnd(1));

      // one and only one is of pointer type
      return b1 && !b2 || !b1 && b2;
    }
  } else if (cr->Kind() == CK_LDA) {
    return TRUE;
  }

  return FALSE;
}

//////////////////////////////////////////////////////////////////////////////
//
// Analyze_preg_pointer 
// 
// Analyze the memory access address <addr> which is a load operator from a PREG
//
//////////////////////////////////////////////////////////////////////////////
//
PTR_OFST_PAIR*
MEM_ACCESS_ANALYZER::Analyze_preg_pointer (CODEREP* addr, BOOL is_read) {

  Is_True (addr->Kind() == CK_VAR, ("CODEREP is not direct load/store"));
  MA_POINTER* ptr = _ptr_mgr.Alloc_ptr ();
  MA_OFFSET* ofst = _ptr_mgr.Alloc_ofst ();
  ofst->Set_fixed_ofst (0);

  ptr->Init_ptr_as_preg (addr->Offset(), addr->Version(), addr->Lod_ty(), addr);
  if (Is_loop_invariant (addr)) {
    ptr->Set_loop_invar();  
  }
  ptr->Set_indirect_level (1);
  ptr->Set_coderep (addr);

  PTR_OFST_PAIR* pair = _ptr_mgr.Alloc_ptr_ofst_pair (ptr, ofst);
  Map_pointer (addr, pair);

  return pair;
}

PTR_OFST_PAIR*
MEM_ACCESS_ANALYZER::Analyze_named_symbol_pointer 
    (CODEREP* addr, BOOL is_read) {

  Is_True (addr->Kind() == CK_VAR, ("CODEREP is not direct load/store"));

  MA_POINTER* ptr = _ptr_mgr.Alloc_ptr ();
  MA_OFFSET* ofst = _ptr_mgr.Alloc_ofst ();
  ofst->Set_fixed_ofst (0);

  ptr->Init_ptr_as_named_sym (addr->Aux_id(), addr->Version(), 
                _opt_stab->Aux_stab_entry (addr->Aux_id())->St (),
                addr->Lod_ty(), addr);
  if (is_read) ptr->Set_ld_cnt(1); else ptr->Set_st_cnt(1); 
  ptr->Set_indirect_level (1);
  ptr->Set_coderep (addr);

  PTR_OFST_PAIR* pair = _ptr_mgr.Alloc_ptr_ofst_pair (ptr, ofst);
  Map_pointer (addr, pair);

  return pair;
}

PTR_OFST_PAIR*
MEM_ACCESS_ANALYZER::Analyze_lda_pointer 
    (CODEREP* addr, BOOL is_read) {

  Is_True (addr->Kind() == CK_LDA, ("CODEREP is not LDA"));

  MA_POINTER* ptr = _ptr_mgr.Alloc_ptr ();
  MA_OFFSET* ofst = _ptr_mgr.Alloc_ofst ();
  ofst->Set_fixed_ofst (0);

  ptr->Init_lda_ptr (addr->Lda_aux_id(), addr->Lda_base_st(), 
                     addr->Lda_ty(), addr->Afield_id(), addr);
  if (is_read) ptr->Set_ld_cnt(1); else ptr->Set_st_cnt(1); 
  ptr->Set_indirect_level (1); // categorized as indiect load/store

  ofst->Set_fixed_ofst (addr->Offset());
  PTR_OFST_PAIR* pair = _ptr_mgr.Alloc_ptr_ofst_pair (ptr, ofst);

  Map_pointer (addr, pair);

  return pair;
}


//////////////////////////////////////////////////////////////////////////////
//
// Analyze_iload_pointer
//
//  Analyze the load/store address <addr> which is a indirect load 
//
//////////////////////////////////////////////////////////////////////////////
PTR_OFST_PAIR*
MEM_ACCESS_ANALYZER::Analyze_iload_pointer 
    (CODEREP* addr, STMTREP* stmt, BOOL is_read) {
  
  Is_True (addr->Kind() == CK_IVAR, ("CODEREP is not indirect load/store"));

  PTR_OFST_PAIR* ptr_ofst = Get_pointer_ofst_pair (addr);
  if (ptr_ofst) {
    return ptr_ofst; 
  }

  MEM_ACCESS* ld_ptr = Analyze_mem_access (addr, stmt, TRUE);
  MA_POINTER* ptr = _ptr_mgr.Alloc_ptr ();
  MA_OFFSET* ofst = _ptr_mgr.Alloc_ofst ();
  ofst->Set_fixed_ofst (addr->Offset());
  ptr->Init_indirect_ptr (ld_ptr, ld_ptr->Obj_ty(), 
                          ld_ptr->Indirect_level()+1, addr);
  ptr->Set_coderep (addr);
  if (is_read) ptr->Set_ld_cnt(1); else ptr->Set_st_cnt(1); 

  PTR_OFST_PAIR* pair = _ptr_mgr.Alloc_ptr_ofst_pair (ptr, ofst);
  Map_pointer (addr, pair);

  return pair;
}

PTR_OFST_PAIR*
MEM_ACCESS_ANALYZER::Analyze_array_access 
 (CODEREP* addr, STMTREP* stmt, BOOL is_read) {

  Is_True (addr->Opr() == OPR_ARRAY, ("operator is not ARRAY."));
  PTR_OFST_PAIR* ptr_ofst = Get_pointer_ofst_pair (addr);
  if (ptr_ofst) {
    return ptr_ofst; 
  }

  CODEREP* kid0 = addr->Opnd(0);
  ptr_ofst = Analyze_pointer (addr->Opnd(0), stmt, is_read);

  // go ahead to analyze the offset -- borrow from 
  // OPT_STAB::Analyze_Range()
  MA_OFFSET* ofst = _ptr_mgr.Alloc_ofst ();

  mINT64 elm_size = addr->Elm_siz ();
  if (elm_size < 0) {
    // Negative element size signifies non-contiguous array. There are
    // no address bounds implicit in a non-contiguous array access, so
    // we can't do anything. (bug 708002) -- borrowed from 
    // OPT_STAB::Analyze_Range() 
    return Gen_messy_access_pair (addr, stmt, is_read);
  }

  // determine the dimension
  INT32 n_dim = addr->Kid_count();
  Is_True (n_dim & 1, ("CODEREP of ARRAY should have odd number of kids"));
  n_dim = n_dim >> 1;

  // the lower and higer range of the access
  mINT64 lower, higher;
  lower = higher = 0;

  BOOL range_is_unknown = FALSE;

  // scan from higher dimension to lower dimension
  //
  for (INT32 i = 1; i <= n_dim; i++) {
    CODEREP* dim = addr->Opnd (i);
    CODEREP* index = addr->Opnd (i + n_dim);
    
    INT low_idx, high_idx;

    if (dim->Kind() == CK_CONST && dim->Const_val() > 1) {
      if (index->Kind() == CK_CONST) {
        low_idx = high_idx = index->Const_val();
      } else {
        low_idx = 0; high_idx = dim->Const_val()-1;
      }

      lower = lower * dim->Const_val() + low_idx;
      higher = higher * dim->Const_val() + high_idx;
    } else {
      // unbound array 
      range_is_unknown = TRUE;
      break;
    }
  }

  lower *= elm_size;
  higher *= elm_size;
  
  // figure out of the offset from the base of the array
  //
  if (!range_is_unknown) {
    if (lower != higher) {
      ofst->Set_linear_ofst_range (ADDR_LINEAR_EXPR(lower),
                                   ADDR_LINEAR_EXPR(higher));
    } else {
      ofst->Set_fixed_ofst (lower);
    }
  } else {
    ofst->Set_unknown_ofst ();
  }
  ofst->Add (ptr_ofst->second);

  // finally, form the "pointer, offset" pair.
  PTR_OFST_PAIR* pair = _ptr_mgr.Alloc_ptr_ofst_pair (ptr_ofst->first, ofst);
  Map_pointer (addr, pair);

  return pair;
}

PTR_OFST_PAIR* 
MEM_ACCESS_ANALYZER::Gen_messy_access_pair 
 (CODEREP* addr, STMTREP* stmt, BOOL is_read) {

  MA_POINTER* ptr = _ptr_mgr.Alloc_ptr ();
  ptr->Init_too_messy_ptr ((TY_IDX)0, addr);
  if (is_read) ptr->Set_ld_cnt (1); else ptr->Set_st_cnt (1);
  MA_OFFSET* ofst = _ptr_mgr.Alloc_ofst ();
  ofst->Set_fixed_ofst (0);
  PTR_OFST_PAIR* pair = _ptr_mgr.Alloc_ptr_ofst_pair (ptr,ofst);

  Map_pointer (addr, pair);
  return pair;
}

//////////////////////////////////////////////////////////////////////////////
//
// Analyze_pointer
//
//  Analyze the pointer of memory access with address being <addr>, return 
// the pair of pointer and offset
// 
//////////////////////////////////////////////////////////////////////////////
PTR_OFST_PAIR*
MEM_ACCESS_ANALYZER::Analyze_pointer 
  (CODEREP* addr, STMTREP* stmt, BOOL is_read) {
  
  PTR_OFST_PAIR* ptr_ofst = Get_pointer_ofst_pair (addr);
  if (ptr_ofst != NULL) {
    // if the pointer has already been analyzed, update the 
    // read/write counter and return
    //
    MA_POINTER* ptr = ptr_ofst->first; 
    MA_OFFSET* ofst = ptr_ofst->second;
    if (is_read)
      ptr->Set_ld_cnt (ptr->Ld_cnt()+1);
    else
      ptr->Set_st_cnt (ptr->St_cnt()+1);

    return ptr_ofst;
  }

  if (addr->Kind() == CK_VAR) {
    // the address is a load from a pointer
    ST* st = _opt_stab->Aux_stab_entry (addr->Aux_id())->St ();
    if (ST_class(st) == CLASS_PREG) {
      return Analyze_preg_pointer (addr, is_read);
    } else {
      return Analyze_named_symbol_pointer (addr, is_read);
    }
  } else if (addr->Kind() == CK_IVAR) {
    // the address is an indirect load of pointer
    return Analyze_iload_pointer (addr, stmt, is_read);
  } else if (addr->Kind() == CK_OP) {
    // The address is a expression, currently we can handle 
    // addition only
    if (addr->Opr () == OPR_ADD) {
      CODEREP* opnd0 = addr->Opnd(0);
      CODEREP* opnd1 = addr->Opnd(1);
       
      BOOL opnd0_is_ptr = Expr_of_ptr_ty (opnd0);
      BOOL opnd1_is_ptr = Expr_of_ptr_ty (opnd1);
      
      if (opnd0_is_ptr && !opnd1_is_ptr || 
          !opnd0_is_ptr && opnd1_is_ptr) {
        CODEREP* addr_kid = opnd0_is_ptr ? opnd0 : opnd1; 
        CODEREP* ofst_kid = opnd0_is_ptr ? opnd1 : opnd0;
        
        PTR_OFST_PAIR* pair = Analyze_pointer (addr_kid, stmt, is_read);
        MA_POINTER* ptr = pair->first;
        if (is_read) ptr->Set_ld_cnt (1); else ptr->Set_st_cnt (1);
  
        MA_OFFSET* ofst = _ptr_mgr.Alloc_ofst();
        Analyze_ofst (ofst_kid, ofst);
        ofst->Add (pair->second);

        PTR_OFST_PAIR* merge_pair = 
          _ptr_mgr.Alloc_ptr_ofst_pair(ptr, ofst); 
        Map_pointer (addr, merge_pair);

        return merge_pair;
      } else if (_trace_detail) {
        fprintf (TFile, "cannot distinct pointer kid from this expr:\n");     
        addr->Print(0,stderr);
      }
    } else if (addr->Opr () == OPR_ARRAY) {
      return Analyze_array_access (addr, stmt, is_read);
    }
  } else if (addr->Kind() == CK_LDA) {
    return Analyze_lda_pointer (addr, is_read);
  } 

  return Gen_messy_access_pair (addr, stmt, is_read);
}

void
MEM_ACCESS_ANALYZER::Analyze_ofst_helper (CODEREP* ofst, MA_OFFSET* res) {
  res->Set_unknown_ofst ();

  if (ofst->Kind() == CK_OP) {
    switch (ofst->Opr()) {
    case OPR_CVT:
      switch (ofst->Op()) {
      case OPC_I8I4CVT:
      case OPC_U8I8CVT:
      case OPC_I8U8CVT:
      case OPC_U8U4CVT:
        Analyze_ofst (ofst->Opnd(0), res);
        return;
      }
      break;

    case OPR_MPY:
      {
      if (!MTYPE_is_integral(ofst->Dtyp())) {
        break;
      }
      CODEREP* ckid, *nckid; 
      ckid = nckid = NULL;
      if (ofst->Opnd(0)->Kind() == CK_CONST) {
        ckid = ofst->Opnd(0), nckid = ofst->Opnd(1);
      } else if (ofst->Opnd(1)->Kind() == CK_CONST) {
        ckid = ofst->Opnd(1), nckid = ofst->Opnd(0);
      }
      if (ckid && nckid) {
        MA_OFFSET t;
        INT scale = ckid->Const_val();
        Analyze_ofst (nckid, &t);

        if (t.Kind() == MA_OFST_LINEAR) {
          const ADDR_LINEAR_EXPR* r = t.Linear_ofst ();
          t.Set_linear_ofst (r->Coefficient() * scale, 
                             r->cr(), r->Const_part () * scale);
        } else if (t.Kind() == MA_OFST_RANGE) {
          ADDR_LINEAR_EXPR_RANGE r = *t.Linear_ofst_range(); 
          r.low.Multiply (scale);
          r.high.Multiply (scale);
          t.Set_linear_ofst_range (r.low, r.high);
        } else if (t.Kind() == MA_OFST_FIXED) {
          t.Set_fixed_ofst (t.Get_fixed_ofst() * scale); 
        } else {
          t.Set_unknown_ofst();
        }
        *res = t;
        return;
      }
      }
      break;

    case OPR_ADD:
      {
      if (!MTYPE_is_integral(ofst->Dtyp())) {
        break;
      }
      MA_OFFSET t1, t2;
      Analyze_ofst (ofst->Opnd(0), &t1);
      Analyze_ofst (ofst->Opnd(1), &t2);
      t1.Add (&t2);
      *res = t1;
      }
      return;
    }
    /*end of ofst->Kind() == CK_OP */
  } else if (ofst->Kind() == CK_CONST) {
    res->Set_fixed_ofst (ofst->Const_val());
    return;
  } else if (ofst->Kind() == CK_VAR) {
    if (_loopinfo->Iv_map().Lookup(ofst->Aux_id()) != NULL)
    {
      res->Set_linear_ofst (1, ofst, 0);
      return;
    }
    else if (ofst->Defstmt() != NULL &&
         !ofst->Is_flag_set ((CR_FLAG)(CF_DEF_BY_PHI|CF_DEF_BY_CHI))) {
       STMTREP* stmt = ofst->Defstmt();
       if (stmt->Opr() == OPR_STID &&
           stmt->Rhs()->Kind() == CK_VAR)
       {
         AUX_ID rhs_var = stmt->Rhs()->Aux_id();
         if (_loopinfo->Iv_map().Lookup(rhs_var) != NULL)
         {
           res->Set_linear_ofst(1,stmt->Rhs(),0);
           return;
         }
       }
    }
  }

  res->Set_unknown_ofst ();
  return;
}

void
MEM_ACCESS_ANALYZER::Analyze_ofst (CODEREP* ofst, MA_OFFSET* res) {
  Analyze_ofst_helper (ofst, res);
  if (_trace_detail) {
    ofst->Print(0,TFile);
    res->Print (TFile);
  }
}

//////////////////////////////////////////////////////////////////////////////
//
// Analyze_mem_access()
//
// Analyze memory load/store rooted at <cr> of statement <stmt>. <is_read> 
// is set FALSE when cr is IVAR/VAR and it is lhs of <stmt>.
//
//////////////////////////////////////////////////////////////////////////////
MEM_ACCESS*
MEM_ACCESS_ANALYZER::Analyze_mem_access 
  (CODEREP* cr, STMTREP* stmt, BOOL is_read) {

  if (cr->Kind () == CK_OP) {
    for (INT i = cr->Kid_count () - 1; i >= 0; i--) {
      Analyze_mem_access (cr->Opnd(i), stmt, TRUE);
    }
    return NULL;
  } else if (cr->Kind() == CK_VAR) {
    // ignore the load/store of PREG
    ST* st = _opt_stab->Aux_stab_entry (cr->Aux_id())->St ();
    if (ST_class(st) == CLASS_PREG) return NULL;
  } else if (cr->Kind () != CK_IVAR) {
    // not memory load/store at all
    return NULL; 
  }

  if (cr->Non_leaf() && 
      !OPERATOR_is_load (cr->Opr()) && !OPERATOR_is_store (cr->Opr())) {

    // cr->Kind() being CK_IVAR/CK_VAR does not necessarily mean the 
    // CODEREP is load or store. A case in point is PARAM which is 
    // of CK_IVAR kind. Besides, we intentionally ignore all the prefechings
    //
    return NULL;
  }

  // update the counter 
  if (is_read) _read_cnt++ ; else _write_cnt++; 

  MEM_ACCESS* ma = NULL;
  if (ma = Get_mem_access (cr)) {
    // the memory access has already been analyzed 
    return ma;
  }

  MA_POINTER* ptr = NULL;
  MA_OFFSET* ofst = NULL;
  ma = Alloc_mem_access ();
  if (cr->Kind() == CK_VAR) {
    // for the case of direct load/store
    ST* st = _opt_stab->Aux_stab_entry (cr->Aux_id())->St ();
    ma->Set_indirect_level(0);
    ma->Set_st (st);
    ma->Ofst().Set_fixed_ofst (cr->Offset());
  } else {
    // for the case of indirect load/store
    CODEREP* addr = is_read ? cr->Ilod_base () : cr->Istr_base();

    PTR_OFST_PAIR* pair = Analyze_pointer (addr, stmt, is_read);
    ptr = pair->first; 
    ofst = pair->second;
    ma->Set_indirect_level(ptr->Indirect_level ());
    Is_True (ma->Indirect_level() != 0, ("indirect level is not set properly"));
    ma->Set_ptr (ptr);
    ptr->Add_mem_access (ma);

    ma->Ofst().Set_fixed_ofst (cr->Offset());
    ma->Adjust_ofst (pair->first, pair->second);
  }

  // Set type properly 
  TY_IDX obj_ty, hl_ty;
  UINT field_id = 0;
  if (cr->Kind() == CK_VAR) {
    obj_ty = hl_ty = cr->Lod_ty();
    if (field_id = cr->Field_id()) {
      obj_ty = coderep_obj_ty (cr); 
    }
  } else {
    obj_ty = hl_ty = cr->Ilod_ty();
    if (field_id = cr->I_field_id()) { 
      obj_ty = coderep_obj_ty (cr); 
    }
  }
  ma->Set_obj_ty (obj_ty);
  ma->Set_hl_ty (hl_ty);
  ma->Set_field_id (field_id);

  if (is_read) ma->Set_is_read (cr); else ma->Set_is_write(stmt); 

  // Set the access size 
  ma->Set_byte_size (TY_size(ma->Obj_ty()));

  // Finally, bind the CODEREP and MA_ACCESS. By doing that, we obviate the 
  // need of re-analyzing the memory access when it is revisited relater on.
  // HINT: the HSSA is DAG not tree.
  //
  Map_mem_access (cr, ma);

  return ma;
}

//////////////////////////////////////////////////////////////////////////////
//
// Analyze_mem_access() 
//   Analyze all memory loads/stores in current loop
// 
//////////////////////////////////////////////////////////////////////////////
void
MEM_ACCESS_ANALYZER::Analyze_mem_access (void) {
  
  BB_NODE_SET_ITER iter;
  BB_NODE* blk; 
  FOR_ALL_ELEM (blk, iter, Init(_loopinfo->Loop()->True_body_set())) {
    STMTREP_ITER stmt_iter (blk->Stmtlist());
    STMTREP* stmt;
    FOR_ALL_NODE (stmt, stmt_iter, Init()) {
      if (stmt->Lhs()) Analyze_mem_access (stmt->Lhs(), stmt, FALSE);
      if (stmt->Rhs()) Analyze_mem_access (stmt->Rhs(), stmt, TRUE);
    }
  }
 
  if (_trace) {
    Print (TFile);
  }
}

BOOL
MEM_ACCESS_ANALYZER::Assemble_aliased_mem_groups(const ALIAS_RULE *alias_rule,
                                                 MEM_GROUP_VECT &groups)
{
  if (_ptr_mgr.Ptr_sum() < 2) {
    // there is no more than one group of mem-ops. No chance for
    // multiversioning.
    return FALSE;
  }

  // Generate a MEM_GROUP for the accesses of MA_POINTER that alias
  // with an MA_POINTER that contains write accesses.
  ID_MAP<MEM_GROUP *,MA_POINTER*> group_created(32,NULL,_mp,FALSE);
  group_created.Init();

  BOOL add_write = FALSE;
  MA_PTR_VECT& vect = _ptr_mgr.All_ptrs();
  for (MA_PTR_VECT_ITER write_iter = vect.begin();
      write_iter != vect.end(); write_iter++) {
    MA_POINTER *ptr1 = *write_iter;
    if (ptr1->St_cnt() && !group_created.Lookup(ptr1)) {
      for (MA_PTR_VECT_ITER all_iter = vect.begin();
          all_iter != vect.end(); all_iter++) {
        MA_POINTER *ptr2 = *all_iter;
        if (ptr2 == ptr1) continue;
        if (group_created.Lookup(ptr2)) continue;

        MEM_ACCESS_VECT& v1 = ptr1->All_mem_access();
        MEM_ACCESS_VECT& v2 = ptr2->All_mem_access();
        /* Paranoia. If for some reason we have an empty access vector
         * punt out.  Clearly there exists a write reference in the
         * loop for 'ptr1', as to why there may not be any accesses???
         */
        if (v1.size() == 0 || v2.size() == 0)
        {
          if (_trace)
            fprintf(TFile, "----> AAMG: empty access vector\n");
          return FALSE;
        }

        if (!alias_rule->Aliased_Memop (v1[0]->Points_to(_opt_stab),
            v2[0]->Points_to(_opt_stab), (TY_IDX)0, (TY_IDX)0))
          continue;

        /* If we have found an alias and one of the involved access
         * vectors is too messy for us to have computed the access
         * range, then we give up.
         */
        if (ptr1->Kind() == MA_PTR_TOO_MESSY ||
            ptr2->Kind() == MA_PTR_TOO_MESSY)
        {
          if (_trace)
            fprintf(TFile, "----> AAMG: too messy\n");
          return FALSE;
        }

        MA_OFFSET ofst;
        ofst.Set_fixed_ofst(0);
        INT sz = 0;

        for (MEM_ACCESS_VECT_ITER iter = v2.begin ();
            iter != v2.end (); iter++) {
          ofst.Union (&(*iter)->Ofst(), _loopinfo);
          sz = MAX(sz, (*iter)->Byte_size());
        }

        MEM_RANGE *r = CXX_NEW(MEM_RANGE(), _mp);
        r->Set_base_ptr (ptr2);
        r->Set_access_range (&ofst, _loopinfo, sz);
        if (!r->Access_range().low.Is_const ())
        {
          if (_trace)
            fprintf(TFile, "----> AAMG: access range non const\n");

          return FALSE;
        }

        MEM_GROUP *mg = CXX_NEW(MEM_GROUP(v2,r,ptr2->St_cnt()>0),_mp);
        groups.push_back(mg);
        add_write = TRUE;

        group_created.Insert(ptr2,mg);
      }
      if (add_write) {
        Is_True(!group_created.Lookup(ptr1),
            ("Attempt to add access vector to groups more than once"));
        MA_OFFSET ofst;
        ofst.Set_fixed_ofst(0);
        INT sz = 0;

        MEM_ACCESS_VECT &v = ptr1->All_mem_access();
        for (MEM_ACCESS_VECT_ITER iter = v.begin ();
            iter != v.end (); iter++) {
          ofst.Union (&(*iter)->Ofst(), _loopinfo);
          sz = MAX(sz, (*iter)->Byte_size());
        }

        MEM_RANGE *r = CXX_NEW(MEM_RANGE(), _mp);
        r->Set_base_ptr (ptr1);
        r->Set_access_range (&ofst, _loopinfo, sz);
        if (!r->Access_range().low.Is_const ())
        {
          if (_trace)
            fprintf(TFile, "----> AAMG: access range non const\n");
          return FALSE;
        }

        MEM_GROUP *mg = CXX_NEW(MEM_GROUP(v,r,TRUE),_mp);
        groups.push_back(mg);
        add_write = FALSE;

        group_created.Insert(ptr1,mg);
      }
    }
  }

  return groups.size() > 1;
}

///////////////////////////////////////////////////////////////
//
//   Implementation of misceallneous classes
//
///////////////////////////////////////////////////////////////
//
void
MEM_RANGE::Set_access_range (MA_OFFSET* ofst, LMV_LOOP_INFO* 
                             loopinfo, INT access_sz) {

  switch (ofst->Kind()) {
  case MA_OFST_FIXED:
  case MA_OFST_LINEAR:
    _access_range.low = *ofst->Linear_ofst ();
    _access_range.high = _access_range.low;
    break;

  case MA_OFST_RANGE: 
    {
    const ADDR_LINEAR_EXPR_RANGE* r = ofst->Linear_ofst_range ();
    _access_range.low = r->low; 
    _access_range.high = r->high;
    }
    break; 

  case MA_OFST_TOO_MESSY:
  case MA_OFST_UNKNOWN:
    _access_range.low.Init(); 
    _access_range.high.Init(); 
    break;

  default:
    FmtAssert (FALSE, ("Unknown ofset kind %d", ofst->Kind()));
  }
 
  if (!_access_range.low.Is_invalid() && !_access_range.high.Is_invalid()) {
    return;
  }
 
  TY_IDX ty = (TY_IDX)0; 
  if (Base_is_symbol ()) ty = ST_type(Base_sym ());
  else ty = Base_ptr()->Pointed_ty(); 
  
  if (ty != (TY_IDX)0 && TY_kind(ty) == KIND_ARRAY) {
    INT sz = TY_size(ty);
    if (sz != 0) {
      _access_range.low.Set_const_expr (0);
      _access_range.high.Set_const_expr (sz);
    }
  }
}


LMV_LOOP_INFO::LMV_LOOP_INFO (BB_LOOP* loop, MEM_POOL* mp,
                              IVR &ivr, BOOL trace)
 :_loop(loop),
  _mp(mp),
  _trace(trace),
  //32: a medium-sized loop normally has less than this much pointers 
  _val_range_map(32, NULL, _mp, FALSE),
  _iv_map(32,NULL,_mp,FALSE)
{
  _val_range_map.Init();
  _iv_map.Init();

  /* Here we perform induction variable analysis to identify
   * the ivs within the loop.  We will add additional AUX_IDs
   * to the map as we find symbols that are copys from these
   * initial ivs.
   */
  if ( _trace )
    fprintf(TFile,"=== Start Induction Variables===\n");
  ivr.Ident_all_iv_cands(loop,loop->Header());
  vector<IV_CAND*>::iterator iv_cand_iter;
  for (iv_cand_iter = ivr.Get_iv_candidates().begin();
      iv_cand_iter != ivr.Get_iv_candidates().end();
      iv_cand_iter++) {
    IV_CAND *cur_iv = *iv_cand_iter;
    if ( _trace )
      cur_iv->Print(TFile);
    _iv_map.Insert(cur_iv->Var()->Aux_id(),cur_iv);
  }
  if ( _trace )
    fprintf(TFile,"=== End Induction Variables===\n");
}

BOOL
LMV_LOOP_INFO::Equivalent_iv(CODEREP *coderep, AUX_ID aux_id)
{
  IV_CAND *iv = _iv_map.Lookup(aux_id);
  Is_True(coderep->Kind() == CK_VAR,("Expecting CK_VAR here"));
  Is_True(iv != NULL, ("Must be valid iv"));

  if (coderep->Aux_id() == aux_id)
      return TRUE;
  else if (coderep->Defstmt() != NULL &&
      !coderep->Is_flag_set ((CR_FLAG)(CF_DEF_BY_PHI|CF_DEF_BY_CHI))) {
    STMTREP* stmt = coderep->Defstmt();
    if (stmt->Opr() == OPR_STID &&
        stmt->Rhs()->Kind() == CK_VAR)
    {
      AUX_ID rhs_var = stmt->Rhs()->Aux_id();
      if (rhs_var == aux_id) {
        _iv_map.Insert(coderep->Aux_id(),iv);
        return TRUE;
      }
    }
  }
  return FALSE;
}

// Determine the upper boundary of induction variable in a while-do 
// construction which will be converted into Do-while construction
// in emit phase.
// 
// This function returnr TRUE iff upper boundary is determined.
// Borrow much code from code-emit phase
BOOL
LMV_LOOP_INFO::Get_iv_upperbound (IV_CAND *iv_cand, VAR_VAL_RANGE *vr)
{
  if (!_loop->Well_formed() || _loop->Exit_early()) {
    return FALSE;
  }

  if (_loop->Flags() != LOOP_WHILE && 
      _loop->Flags() != LOOP_PRE_WHILE &&
      _loop->Flags() != LOOP_PRE_DO) {
    return FALSE;
  }

  BB_NODE *header = _loop->Header();
  BB_NODE *preheader = _loop->Preheader();
  BB_NODE *loopback = _loop->Loopback();
  INT loopback_opnd_num = _loop->Loopback_pred_num();

  if (loopback_opnd_num != 1) {
    return FALSE; // too complex
  }
  
  STMTREP *cond_br = header->Branch_stmtrep();

  // make sure we had a conditional branch
  if (cond_br->Op() != OPC_TRUEBR && cond_br->Op() != OPC_FALSEBR ) 
    return FALSE;

  // make sure we have a comparison with two operands
  CODEREP *cmp = cond_br->Rhs();
  if (cmp->Kind() != CK_OP || !OPCODE_is_compare(cmp->Op())) 
    return FALSE;

  // It is hard to know the trip counter or IV upper boundary if 
  // compararision is OPR_NE or OPR_EQ.
  OPERATOR cond_opr = cmp->Opr();
  if (!(cond_opr == OPR_LE || cond_opr == OPR_GE ||
        cond_opr == OPR_LT || cond_opr == OPR_GT)) {
    return FALSE;
  }
 
  CODEREP* iv, *upbound_cr; 
  iv = upbound_cr = NULL;

  // Which operand of the compare is the induction variable?
  if (cmp->Opnd(0)->Kind() == CK_VAR &&
      Equivalent_iv(cmp->Opnd(0),iv_cand->Var()->Aux_id()))
  {
    iv = cmp->Opnd(0);
    upbound_cr = cmp->Opnd(1);
  }
  else if (cmp->Opnd(1)->Kind() == CK_VAR &&
        Equivalent_iv(cmp->Opnd(1),iv_cand->Var()->Aux_id()))
  {
    iv = cmp->Opnd(1);
    upbound_cr = cmp->Opnd(0);
  }

  if (!iv)
    return FALSE;

  BOOL reverse = FALSE;
  if (iv == cmp->Opnd(1)) reverse = TRUE; 
  if (cond_br->Op() == OPC_TRUEBR) {
    reverse = !reverse;
  }

  if (reverse) {
    switch (cond_opr) {
    case OPR_LE: cond_opr = OPR_GE; break;
    case OPR_LT: cond_opr = OPR_GT; break;
    case OPR_GE: cond_opr = OPR_LE; break;
    case OPR_GT: cond_opr = OPR_GE; break;
    }
  }
  
  if (!(upbound_cr->Kind() == CK_CONST ||
        upbound_cr->Kind() == CK_VAR ))
    return FALSE;

  if (cond_opr == OPR_LE || cond_opr == OPR_GE)
  {
    if (upbound_cr->Kind() == CK_CONST)
      vr->Set_high(upbound_cr->Const_val());
    else
      vr->Set_high(upbound_cr);
    return TRUE;
  }
  else if (cond_opr == OPR_LT || cond_opr == OPR_GT)
  {
      if ( upbound_cr->Kind() == CK_CONST)
        vr->Set_high(upbound_cr->Const_val()-1);
      else
      {
        vr->Set_high(upbound_cr);
        vr->Set_high_is_cr_subone();
      }
      return TRUE;
  }

  return FALSE;
}

// Analyze the value range of <var> of given <ver>. If <ver> is 
// not specified (i.e ver=0), this function tries to figure out 
// the range of possible values of <var> used within current 
// loop regardless its verion. 
// 
// The analysis is done in conservative manner, meaning the returned
// value-range always covers the all possible values.
//
// NOTE: Currently, the analysis is done only to the loop's primary 
//   induction variable without considering its version.
//
void
LMV_LOOP_INFO::Analyze_var_value_range 
  (AUX_ID var, VER_ID ver, VAR_VAL_RANGE* val) {

  // suppress gcc complains, currently we ignore verion
  ver = ver; 
  VAR_VAL_RANGE* vr = _val_range_map.Lookup (var); 
  if (vr != NULL) {
    *val = *vr; 
    return;
  }

  val->Init();

  // If the variable is not a known induction variable, we give up
  IV_CAND *iv = _iv_map.Lookup(var);
  if ( iv == NULL )
      return;

  vr = CXX_NEW (VAR_VAL_RANGE, _mp);

  // Now, determine the lower and upper bounds on the induction variable
  CODEREP *init_value = iv->Init_value();
  Is_True(init_value != NULL, ("Expected iv to have initial value\n"));
  if ( init_value->Kind() == CK_CONST )
    vr->Set_low(init_value->Const_val());
  // It can be following scenario. So looking ahead a bit may be useful.
  //    LDC
  //  STID 256 cr-x  // out-side loop
  //  ...
  //  loop-head-block:
  //   iv = phi(cr-x, cr-y)
  //
  else if (init_value->Defstmt() != NULL &&
      !init_value->Is_flag_set ((CR_FLAG)(CF_DEF_BY_PHI|CF_DEF_BY_CHI))) {
    STMTREP* stmt = init_value->Defstmt();
    if (stmt->Opr() == OPR_STID &&
        stmt->Rhs()->Kind() == CK_CONST) {
      vr->Set_low (stmt->Rhs()->Const_val ());
    }
  }
  else {
     // Not yet supported non-const lower bound.
  }

  // Now, determine the uppper bound
  CODEREP* trip = _loop->Trip_count_expr();
  if (trip) {
    if (trip->Kind() == CK_CONST && vr->Low_is_const ()) {
      vr->Set_high (trip->Const_val() + vr->Low_val() - 1);
    }
  }
  else
    Get_iv_upperbound(iv,vr);

  _val_range_map.Insert (var, vr);
  *val = *vr;
}

void
ADDR_LINEAR_EXPR::Get_range 
  (VAR_VAL_RANGE* vr, LMV_LOOP_INFO* loopinfo) const {

  if (Is_const()) {
    vr->Set_low (Const_val ());
    vr->Set_high (Const_val ());
    return;
  } 

  vr->Init();
  if (Is_nonconst()) {
    VAR_VAL_RANGE t;
    loopinfo->Analyze_var_value_range (Var(), Var_ver(), &t);

    if (!t.Low_is_invalid() && !t.High_is_invalid()) {
      if (t.Low_is_const()) {
        INT low = Coefficient() * t.Low_val() + Const_part ();
        low = MIN(low, Const_part ());
        vr->Set_low (low);
      }
      else
        ; // Non-const lower bound not supported yet....

      if (t.High_is_const()) {
        INT high = Coefficient() * t.High_val() + Const_part ();
        high = MAX(high, Const_part ());
        vr->Set_high (high);
      }
      else {
        vr->Set_high(t.High_cr());
        if (t.High_is_cr_subone())
          vr->Set_high_is_cr_subone();
      }
    }
  }
}

//////////////////////////////////////////////////////////////////////////////
//
//   All print, trace and other cold routines are clustered here.
//
//////////////////////////////////////////////////////////////////////////////
void
MA_POINTER::Print (FILE* f, BOOL verbose) {
  fprintf (f, "[%3d] ", _id); 
 
  switch (_kind) {
  case MA_PTR_INVALID: fprintf (f, "invalid\n"); return; 
  case MA_PTR_PREG: 
    fprintf (f, "R%d,v%d,",  _is_preg._preg_num, _ver); 
    break;
  case MA_PTR_SYM:
    fprintf (f, "%s,v%d,", ST_name(_is_sym._name), _ver); 
    break;
  case MA_PTR_INDIRECT:
    fprintf (f, "indirect MA-%d,", _is_indirect._ma->Id()); 
    break;
  case MA_PTR_TOO_MESSY:
    fprintf (f, "too-messy cr%d,", _cr->Coderep_id());
    break;
  case MA_PTR_LDA:
    fprintf (f, "&%s, cr%d,", ST_name(_is_lda._base), _cr->Coderep_id());
    break;
  default:
    FmtAssert (FALSE, ("Unknown pointer type"));
  }

  if (_ty != (TY_IDX)0) {
    if (TY_kind(_ty) == KIND_POINTER)
      fprintf (f, "Ty:->'%s',", TY_name(Pointed_ty()));
    else 
      fprintf (f, "Ty:'%s',", TY_name(_ty));
  }

  fprintf (f, "ilevel:%d,ld:%d,st:%d,", _ind_level, _ld_cnt, _st_cnt);
  if (Is_loop_invar()) { fprintf (f, "invar,"); }
  fprintf (f, "\n");

  if (_kind == MA_PTR_INDIRECT || _kind == MA_PTR_TOO_MESSY) {
    _cr->Print (4, f);
  }
  
  if (verbose) {
    fprintf (f, "All memops via this pointer:\n");
    for (MEM_ACCESS_VECT_ITER iter = _mem_access.begin (); 
         iter != _mem_access.end (); iter ++) {
      (*iter)->Print (f); 
    }
  }
}

void
MA_OFFSET::Print (FILE* f) {

  switch (_kind) {
  case MA_OFST_INVALID: fprintf (f, "invalid"); break;
  case MA_OFST_FIXED:   fprintf (f, "fixed,%d", Get_fixed_ofst()); break;   
  case MA_OFST_LINEAR:  
     fprintf (f, "linear %d*aux_%dv%d+%d", _ofst.low.Coefficient (), 
              _ofst.low.Var(), _ofst.low.Var_ver(), _ofst.low.Const_part ());
     break; 
  case MA_OFST_RANGE:   
     fprintf (f, "range [");  
     if (_ofst.low.Is_nonconst ()) {
       fprintf (f, "linear %d*aux_%dv%d+%d", _ofst.low.Coefficient (), 
                _ofst.low.Var(), _ofst.low.Var_ver(), _ofst.low.Const_part ());
     } else {
       fprintf (f, "fixed,%d", _ofst.low.Const_val());   
     }

     fprintf (f, ",");
     if (_ofst.high.Is_nonconst ()) {
       fprintf (f, "%d*aux_%dv%d+%d", _ofst.high.Coefficient (), 
                _ofst.high.Var(), _ofst.high.Var_ver(), _ofst.high.Const_part ());
     } else {
       fprintf (f, "fixed,%d", _ofst.high.Const_val()); 
     }
     fprintf (f, "]");
     break;

  case MA_OFST_UNKNOWN: fprintf (f, "unknown"); break;
  case MA_OFST_TOO_MESSY:
     fprintf (f, "ofst is:\n");
     _expr->Print (4, TFile);
     break;

  default:
    FmtAssert (FALSE, ("unknown ofst kind"));
    break;
  }
}

void
MEM_ACCESS::Print (FILE* f) {
  fprintf (f, "[%3d] cr%d,", _id, _coderep ? _coderep->Coderep_id() : 0); 
  if (_ind_level == 0) {
    fprintf (f, "var:%s,", ST_name(_st));
  } else {
    fprintf (f, "ivar:ptr%d,", _ptr ? _ptr->Id() : 0);
  }

  fprintf (f, "ofst:");
  _ofst.Print (f);
  
  fprintf (f, ",sz:%d,fld:%d,", (INT)_size, _field_id);
  if (_obj_ty != (TY_IDX)0) {
    fprintf (f, "Ty:%s,", TY_name(_obj_ty));
  }
  if (_hl_ty != (TY_IDX)0) {
    fprintf (f, "Hlty:%s,", TY_name(_hl_ty));
  }
  fprintf (f, "%s\n", Is_read() ? "ld" : "st");
}

void
MEM_ACCESS_ANALYZER::Print (FILE* f) {

  fprintf (f, "\nMEM_ACCESS_ANALYZER:\n%s\n", DBar);
  fprintf (f, "LOOP:hdr-bb%d, ld:%d, st:%d, MEM_ACCESS are:\n", 
          _loopinfo->Loop()->Header()->Id(), _read_cnt, _write_cnt);
     
  // go through all MEM_ACCESS
  //
  for (MEM_ACCESS_VECT_ITER iter = _all_ma.begin(); 
       iter != _all_ma.end(); iter++) {
    (*iter)->Print (f);
  }

  // go through all MA_POINTER
  fprintf (f, "MA_POINTERS are:\n");
  MA_PTR_VECT& all_ptrs = _ptr_mgr.All_ptrs();
  for (MA_PTR_VECT_ITER iter = all_ptrs.begin(); 
       iter != all_ptrs.end(); iter++) {
    (*iter)->Print (f);
  }
}

void
ADDR_LINEAR_EXPR::Print (FILE* f) {
  if (Is_const()) { 
    fprintf (f, "%d ", Const_val());
  } else if (Is_nonconst ()) { 
    fprintf (f, "%d*aux%dv%d+%d ", Coefficient (),
             (INT)Var(), (INT)Var_ver(), Const_part());
  } else {
    fprintf (f, "invalid ");
  }
}

void
ADDR_LINEAR_EXPR_RANGE::Print (FILE* f) {
  fprintf (f, "[");
  low.Print (f);
  fprintf (f, ",");
  high.Print (f);
  fprintf (f, "]");
}

void
MEM_RANGE::Print (FILE* f) {
  if (Base_is_symbol ()) {
    fprintf (f, "base:");
    Base_sym()->Print (f, FALSE);
  } else if (Base_is_ptr ()) {
    fprintf (f, "pointer is:");
    Base_ptr()->Print (f);
  } else {
    fprintf (f, "Invalid\n");
    return;
  }

  _access_range.Print (f);
  fprintf (f, "\n"); 
}

void VAR_VAL_RANGE::Print(FILE* f) {
  if (Low_is_const())
    fprintf(f,"low: %d,",Low_val());
  else if (Low_is_cr()) {
    fprintf(f,"low: aux%dv%d,",Low_cr()->Aux_id(),Low_cr()->Version());
  }
  else
    fprintf(f,"low: invalid,");
  if (High_is_const())
     fprintf(f,"high: %d\n",High_val());
   else if (High_is_cr()) {
     fprintf(f,"high: aux%dv%d",High_cr()->Aux_id(),High_cr()->Version());
     if (High_is_cr_subone())
       fprintf(f,"-1");
     fprintf(f,"\n");
   }
   else
     fprintf(f,"high: invalid\n");
}

void LMV_CANDIDATE::Print_mem_groups(FILE *f)
{
  for (MEM_GROUP_VECT_CITER val_iter = Mem_groups().begin();
      val_iter != Mem_groups().end(); val_iter++) {
      (*val_iter)->Print(f);
  }
}

void MEM_GROUP::Print(FILE *f)
{
  fprintf(f,"Write: %d\n",Write());
  fprintf(f,"Accesses:\n");
  // go through all MEM_ACCESS
  for (MEM_ACCESS_VECT_ITER iter = Mem_accesses().begin();
      iter != Mem_accesses().end(); iter++) {
    (*iter)->Print(f);
  }
  fprintf(f,"Range:\n");
  Mem_range()->Print(f);
}
