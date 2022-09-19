/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

// These functions are defined to be compatible with the old symbol table
// format.  They are defined to ease the transition to the new symbol table.
// Once the transition is over, this file will be removed.


#ifndef symtab_compatible_INCLUDED
#define symtab_compatible_INCLUDED

#define SYMTAB_COMPATIBLE_DEVWARN_LIMIT 2

// access functions for ST

inline STR_IDX
ST_name_idx (const ST* s)		{ return s->u1.name_idx; }
inline void
Set_ST_name_idx (ST* s, STR_IDX idx)	{ s->u1.name_idx = idx; }
inline char *
ST_name (const ST* s)			{ return &Str_Table[ST_name_idx (s)]; }
#define Set_ST_name(s,idx)		Set_ST_name_idx(s,idx)

inline TCON_IDX
ST_tcon (const ST* s)			{ return s->u1.tcon; }
inline void
Set_ST_tcon (ST* s, TCON_IDX tcon)	{ s->u1.tcon = tcon; }

inline ST_CLASS
ST_sym_class (const ST* s)		{ return s->sym_class; }

inline ST_CLASS				// for compatibility
ST_class (const ST* s)			{ return ST_sym_class (s); }

inline ST_CLASS				// for compatibility
ST_class (const ST& s)			{ return ST_sym_class (s); }

inline void
Set_ST_class (ST* s, ST_CLASS c)	{ s->sym_class = c; }

inline ST_SCLASS
ST_sclass (const ST* s)			{ return s->storage_class; }

inline void
Set_ST_sclass (ST* s, ST_SCLASS sc)	{ s->storage_class = sc; }

inline ST_EXPORT
ST_export (const ST* s)			{ return s->export_class; }
inline void
Set_ST_export (ST* s, ST_EXPORT export_class) 
{ 
#ifdef Is_True_On
    ST_Verify_Sclass_Export (ST_sclass (s), export_class, s);
#endif
    s->export_class = export_class; 
}

inline TY_IDX
ST_type (const ST* s) {
#ifdef Is_True_On
    switch (s->sym_class) {
    default:
	Fail_FmtAssertion ("Invalid argument for ST_type ()");
#ifdef KEY
    case CLASS_BLOCK: // this is for UpFormal_Arg_StkSeg used in va_start
#endif
    case CLASS_VAR:
    case CLASS_CONST:
    case CLASS_PREG:
    case CLASS_NAME:
	return s->u2.type;
    case CLASS_FUNC:
	Lmt_DevWarn (SYMTAB_COMPATIBLE_DEVWARN_LIMIT,
                     ("Should use ST_pu_type instead"));
	return PU_prototype (Pu_Table[s->u2.pu]);
    }
#else
    if (s->sym_class == CLASS_FUNC) {
	Lmt_DevWarn (SYMTAB_COMPATIBLE_DEVWARN_LIMIT,
                     ("Should use ST_pu_type instead"));
	return PU_prototype (Pu_Table[s->u2.pu]);
    } else {
       return s->u2.type;
    }
#endif // Is_True_On
}
inline void
Set_ST_type (ST* s, TY_IDX t) {
#ifdef Is_True_On
    switch (s->sym_class) {
    default:
	Fail_FmtAssertion ("Invalid argument for ST_type ()");
    case CLASS_VAR:
    case CLASS_CONST:
    case CLASS_PREG:
    case CLASS_NAME:
	s->u2.type = t;
    }
#else
    s->u2.type = t;
#endif // Is_True_On
}

inline TY_IDX
ST_pu_type (const ST_IDX s) {
    Is_True (St_Table[s].sym_class == CLASS_FUNC, ("Invalid argument for ST_pu"));
    return PU_prototype (Pu_Table[ST_pu (St_Table[s])]);
}

inline PU_IDX
ST_pu (const ST* s) {
    Is_True (s->sym_class == CLASS_FUNC ||
	     (s->sym_class == CLASS_NAME &&
	      ST_asm_function_st(*s)),
	     ("Invalid argument for ST_pu"));
    return s->u2.pu;
}
inline void
Set_ST_pu (ST* s, PU_IDX pu)
{
    Is_True (s->sym_class == CLASS_FUNC ||
	     (s->sym_class == CLASS_NAME &&
	      ST_asm_function_st(*s)),
	     ("Invalid argument for ST_pu"));
    s->u2.pu = pu;
}

inline BLK_IDX
ST_blk (const ST* s) {
    Is_True (s->sym_class == CLASS_BLOCK, ("Invalid argument for ST_blk"));
    return s->u2.blk;
}
inline void
Set_ST_blk (ST* s, BLK_IDX b)
{
    Is_True (s->sym_class == CLASS_BLOCK, ("Invalid argument for ST_blk"));
    s->u2.blk = b;
}

inline ST_IDX
ST_st_idx (const ST* s)
  { return (s != NULL? s->st_idx : (ST_IDX) 0); }
inline void
Set_ST_st_idx (ST* s, ST_IDX idx)	{ s->st_idx = idx; }

inline BOOL
ST_is_weak_symbol (const ST* s)		{ return s->flags & ST_IS_WEAK_SYMBOL;}
inline void
Set_ST_is_weak_symbol (ST* s)		{ s->flags |= ST_IS_WEAK_SYMBOL; }
inline void
Clear_ST_is_weak_symbol (ST* s)		{ s->flags &= ~ST_IS_WEAK_SYMBOL; }

inline BOOL
ST_is_split_common (const ST* s)	{ return s->flags & ST_IS_SPLIT_COMMON;}
inline void
Set_ST_is_split_common (ST* s)		{ s->flags |= ST_IS_SPLIT_COMMON; }
inline void
Clear_ST_is_split_common (ST* s)	{ s->flags &= ~ST_IS_SPLIT_COMMON; }

inline ST_IDX
ST_base_idx (const ST* s)		
{ 
    if (ST_is_split_common (s))
        return s->st_idx;
    else if (ST_is_weak_symbol (s) && ST_sclass(s) == SCLASS_EXTERN)
        return s->st_idx;
    else
        return s->base_idx;
}
inline void
Set_ST_base_idx (ST* s, ST_IDX base)	
{ 
        if (ST_is_split_common (s))
          Lmt_DevWarn (SYMTAB_COMPATIBLE_DEVWARN_LIMIT,
                       ("Shouldn't set base when split"));
        if (ST_is_weak_symbol(s) && ST_sclass(s) == SCLASS_EXTERN)
          Lmt_DevWarn (SYMTAB_COMPATIBLE_DEVWARN_LIMIT,
                       ("Shouldn't set base when weak"));
	s->base_idx = base; 
}
inline ST*
ST_base (const ST* s)			{ return &St_Table[ST_base_idx (s)]; }
inline void
Set_ST_base (ST* s, ST* base)		{ Set_ST_base_idx(s, ST_st_idx(base)); }

inline UINT64
ST_ofst (const ST* s)			{ return s->offset; }
inline void
Set_ST_ofst (ST* s, UINT64 offset)	{ s->offset = offset; }

// ST flags

inline BOOL
ST_is_not_used (const ST* s)		{ return s->flags & ST_IS_NOT_USED;}
inline void
Set_ST_is_not_used (ST* s)		{ s->flags |= ST_IS_NOT_USED; }
inline void
Clear_ST_is_not_used (ST* s)		{ s->flags &= ~ST_IS_NOT_USED; }

inline BOOL
ST_is_initialized (const ST* s)		{ return s->flags & ST_IS_INITIALIZED;}
inline void
Set_ST_is_initialized (ST* s)		{ s->flags |= ST_IS_INITIALIZED; }
inline void
Clear_ST_is_initialized (ST* s)		{ s->flags &= ~ST_IS_INITIALIZED; }

inline BOOL
ST_is_return_var (const ST* s)		{ return s->flags & ST_IS_RETURN_VAR;}
inline void
Set_ST_is_return_var (ST* s)		{ s->flags |= ST_IS_RETURN_VAR; }
inline void
Clear_ST_is_return_var (ST* s)		{ s->flags &= ~ST_IS_RETURN_VAR; }

inline BOOL
ST_is_value_parm (const ST* s)		{ return s->flags & ST_IS_VALUE_PARM;}
inline void
Set_ST_is_value_parm (ST* s)		{ s->flags |= ST_IS_VALUE_PARM; }
inline void
Clear_ST_is_value_parm (ST* s)		{ s->flags &= ~ST_IS_VALUE_PARM; }

inline BOOL
ST_promote_parm (const ST* s)		{ return s->flags & ST_PROMOTE_PARM;}
inline void
Set_ST_promote_parm (ST* s)		{ s->flags |= ST_PROMOTE_PARM; }
inline void
Clear_ST_promote_parm (ST* s)		{ s->flags &= ~ST_PROMOTE_PARM; }

inline BOOL
ST_keep_name_w2f (const ST* s)		{ return s->flags & ST_KEEP_NAME_W2F;}
inline void
Set_ST_keep_name_w2f (ST* s)		{ s->flags |= ST_KEEP_NAME_W2F; }
inline void
Clear_ST_keep_name_w2f (ST* s)		{ s->flags &= ~ST_KEEP_NAME_W2F; }

inline BOOL
ST_is_datapool (const ST* s)		{ return s->flags & ST_IS_DATAPOOL;}
inline void
Set_ST_is_datapool (ST* s)		{ s->flags |= ST_IS_DATAPOOL; }
inline void
Clear_ST_is_datapool (ST* s)		{ s->flags &= ~ST_IS_DATAPOOL; }

inline BOOL
ST_is_reshaped (const ST* s)		{ return s->flags & ST_IS_RESHAPED;}
inline void
Set_ST_is_reshaped (ST* s)		{ s->flags |= ST_IS_RESHAPED; }
inline void
Clear_ST_is_reshaped (ST* s)		{ s->flags &= ~ST_IS_RESHAPED; }

inline BOOL
ST_emit_symbol (const ST* s)		{ return s->flags & ST_EMIT_SYMBOL;}
inline void
Set_ST_emit_symbol (ST* s)		{ s->flags |= ST_EMIT_SYMBOL; }
inline void
Clear_ST_emit_symbol (ST* s)		{ s->flags &= ~ST_EMIT_SYMBOL; }

inline BOOL
ST_has_nested_ref (const ST* s)		{ return s->flags & ST_HAS_NESTED_REF;}
inline void
Set_ST_has_nested_ref (ST* s)		{ s->flags |= ST_HAS_NESTED_REF; }
inline void
Clear_ST_has_nested_ref (ST* s)		{ s->flags &= ~ST_HAS_NESTED_REF; }

inline BOOL
ST_init_value_zero (const ST* s)	{ return s->flags & ST_INIT_VALUE_ZERO;}
inline void
Set_ST_init_value_zero (ST* s)		{ s->flags |= ST_INIT_VALUE_ZERO; }
inline void
Clear_ST_init_value_zero (ST* s)	{ s->flags &= ~ST_INIT_VALUE_ZERO; }

inline BOOL
ST_gprel (const ST* s)			{ return s->flags & ST_GPREL;}
inline void
Set_ST_gprel (ST* s)			{ s->flags |= ST_GPREL; }
inline void
Clear_ST_gprel (ST* s)			{ s->flags &= ~ST_GPREL; }

inline BOOL
ST_not_gprel (const ST* s)		{ return s->flags & ST_NOT_GPREL;}
inline void
Set_ST_not_gprel (ST* s)		{ s->flags |= ST_NOT_GPREL; }
inline void
Clear_ST_not_gprel (ST* s)		{ s->flags &= ~ST_NOT_GPREL; }

inline BOOL
ST_is_namelist (const ST* s)		{ return s->flags & ST_IS_NAMELIST;}
inline void
Set_ST_is_namelist (ST* s)		{ s->flags |= ST_IS_NAMELIST; }
inline void
Clear_ST_is_namelist (ST* s)		{ s->flags &= ~ST_IS_NAMELIST; }

inline BOOL
ST_is_f90_target (const ST* s)		{ return s->flags & ST_IS_F90_TARGET;}
inline void
Set_ST_is_f90_target (ST* s)		{ s->flags |= ST_IS_F90_TARGET; }
inline void
Clear_ST_is_f90_target (ST* s)		{ s->flags &= ~ST_IS_F90_TARGET; }

inline BOOL
ST_declared_static (const ST* s)	{ return s->flags & ST_DECLARED_STATIC;}
inline void
Set_ST_declared_static (ST* s)		{ s->flags |= ST_DECLARED_STATIC; }
inline void
Clear_ST_declared_static (ST* s)	{ s->flags &= ~ST_DECLARED_STATIC; }

inline BOOL
ST_is_equivalenced (const ST* s)	{ return s->flags & ST_IS_EQUIVALENCED;}
inline void
Set_ST_is_equivalenced (ST* s)		{ s->flags |= ST_IS_EQUIVALENCED; }
inline void
Clear_ST_is_equivalenced (ST* s)	{ s->flags &= ~ST_IS_EQUIVALENCED; }

inline BOOL
ST_is_fill_align (const ST* s)		{ return s->flags & ST_IS_FILL_ALIGN;}
inline void
Set_ST_is_fill_align (ST* s)		{ s->flags |= ST_IS_FILL_ALIGN; }
inline void
Clear_ST_is_fill_align (ST* s)		{ s->flags &= ~ST_IS_FILL_ALIGN; }

inline BOOL
ST_is_optional_argument (const ST* s)	{ return s->flags & ST_IS_OPTIONAL_ARGUMENT;}
inline void
Set_ST_is_optional_argument (ST* s)	{ s->flags |= ST_IS_OPTIONAL_ARGUMENT; }
inline void
Clear_ST_is_optional_argument (ST* s)	{ s->flags &= ~ST_IS_OPTIONAL_ARGUMENT; }

inline BOOL
ST_is_temp_var (const ST* s)		{ return s->flags & ST_IS_TEMP_VAR;}
inline void
Set_ST_is_temp_var (ST* s)		{ s->flags |= ST_IS_TEMP_VAR; }
inline void
Clear_ST_is_temp_var (ST* s)		{ s->flags &= ~ST_IS_TEMP_VAR; }

inline BOOL
ST_is_const_var (const ST* s)		{ return s->flags & ST_IS_CONST_VAR;}
inline void
Set_ST_is_const_var (ST* s)		{ s->flags |= ST_IS_CONST_VAR; }
inline void
Clear_ST_is_const_var (ST* s)		{ s->flags &= ~ST_IS_CONST_VAR; }

inline BOOL
ST_addr_saved (const ST* s)		{ return s->flags & ST_ADDR_SAVED;}
inline void
Set_ST_addr_saved (ST* s)		{ s->flags |= ST_ADDR_SAVED; }
inline void
Clear_ST_addr_saved (ST* s)		{ s->flags &= ~ST_ADDR_SAVED; }

inline BOOL
ST_addr_passed (const ST* s)		{ return s->flags & ST_ADDR_PASSED;}
inline void
Set_ST_addr_passed (ST* s)		{ s->flags |= ST_ADDR_PASSED; }
inline void
Clear_ST_addr_passed (ST* s)		{ s->flags &= ~ST_ADDR_PASSED; }

// Please stop using the following functions (*ST_addr_not_*). They
// are going away *soon*!
inline BOOL
ST_addr_not_saved(const ST &s)		{ return !(s.flags & ST_ADDR_SAVED); }
inline void
Set_ST_addr_not_saved(ST &s)		{ s.flags &= ~ST_ADDR_SAVED; }
inline void
Clear_ST_addr_not_saved(ST &s)		{ s.flags |= ST_ADDR_SAVED; }

inline BOOL
ST_addr_not_passed(const ST &s)		{ return !(s.flags & ST_ADDR_PASSED); }
inline void
Set_ST_addr_not_passed(ST &s)		{ s.flags &= ~ST_ADDR_PASSED; }
inline void
Clear_ST_addr_not_passed(ST &s)		{ s.flags |= ST_ADDR_PASSED; }

inline BOOL
ST_addr_not_saved (const ST* s)		{ return !(s->flags & ST_ADDR_SAVED); }
inline void
Set_ST_addr_not_saved (ST* s)		{ s->flags &= ~ST_ADDR_SAVED; }
inline void
Clear_ST_addr_not_saved (ST* s)		{ s->flags |= ST_ADDR_SAVED; }

inline BOOL
ST_addr_not_passed (const ST* s)	{ return !(s->flags & ST_ADDR_PASSED);}
inline void
Set_ST_addr_not_passed (ST* s)		{ s->flags &= ~ST_ADDR_PASSED; }
inline void
Clear_ST_addr_not_passed (ST* s)	{ s->flags |= ST_ADDR_PASSED; }

inline BOOL
ST_is_thread_private (const ST* s)	{ return s->flags & ST_IS_THREAD_PRIVATE;}
inline void
Set_ST_is_thread_private (ST* s)	{ s->flags |= ST_IS_THREAD_PRIVATE; }
inline void
Clear_ST_is_thread_private (ST* s)	{ s->flags &= ~ST_IS_THREAD_PRIVATE; }

inline BOOL
ST_pt_to_unique_mem (const ST* s)	{ return s->flags & ST_PT_TO_UNIQUE_MEM;}
inline void
Set_ST_pt_to_unique_mem (ST* s)		{ s->flags |= ST_PT_TO_UNIQUE_MEM; }
inline void
Clear_ST_pt_to_unique_mem (ST* s)	{ s->flags &= ~ST_PT_TO_UNIQUE_MEM; }

inline BOOL
ST_pt_to_compiler_generated_mem (const ST* s)	
	{ return s->flags & ST_PT_TO_COMPILER_GENERATED_MEM;}
inline void
Set_ST_pt_to_compiler_generated_mem (ST* s)		
	{ s->flags |= ST_PT_TO_COMPILER_GENERATED_MEM; }
inline void
Clear_ST_pt_to_compiler_generated_mem (ST* s)	
	{ s->flags &= ~ST_PT_TO_COMPILER_GENERATED_MEM; }

inline BOOL
ST_assigned_to_dedicated_preg (const ST* s)	
	{ return s->flags & ST_ASSIGNED_TO_DEDICATED_PREG;}
inline void
Set_ST_assigned_to_dedicated_preg (ST* s)		
	{ s->flags |= ST_ASSIGNED_TO_DEDICATED_PREG; }
inline void
Clear_ST_assigned_to_dedicated_preg (ST* s)	
	{ s->flags &= ~ST_ASSIGNED_TO_DEDICATED_PREG; }

inline BOOL
ST_addr_taken (const ST* s)
{
	return ST_addr_saved(s) || ST_addr_passed(s);
}

inline ST*
ST_full (const ST* s) {
    FmtAssert (ST_is_split_common (s), ("Expecting a split common"));
    return &St_Table[s->base_idx];
}
inline ST*
ST_strong (const ST* s) {
    FmtAssert (ST_is_weak_symbol (s), ("Expecting a weak symbol"));
    return &St_Table[s->base_idx];
}

inline TCON&
STC_val (const ST* s)			{ return Tcon_Table[ST_tcon (s)]; }

inline void
Print_ST (FILE *f, const ST* st, BOOL)	{ st->Print (f); }

// TY

inline UINT
TY_id (TY_IDX ty)			{ return TY_IDX_index (ty); }

inline FLD_HANDLE
TY_flist (const TY& ty)			{ return TY_fld (ty); }

//
// TY flags
//

// TY pu_flags

inline void Print_TY (FILE* f, TY_IDX tyi) {
  Ty_Table[tyi].Print (f);
}

//
// TYLIST
//

inline TY_IDX
TYLIST_item (const TYLIST_IDX tli)	{ return TYLIST_type(tli); }
inline TYLIST_IDX
TYLIST_next (const TYLIST_IDX tli)	{ return tli+1; }

//
// FLD
//

inline FLD_HANDLE
FLD_next (FLD_HANDLE f)
{
    if (FLD_last_field (f))
	return FLD_HANDLE();
    else
	return FLD_HANDLE (f.Idx() + 1); 
}


//
// TY_AR
//

inline BOOL
TY_AR_const_lbnd (const TY& ty, INT32 i)
{
    return ARB_const_lbnd (TY_arb(ty)[i]);
}

inline void
Set_TY_AR_const_lbnd (const TY& ty, INT32 i)
{
    Set_ARB_const_lbnd (TY_arb(ty)[i]);
}

inline void
Clear_TY_AR_const_lbnd (const TY& ty, INT32 i)
{
    Clear_ARB_const_lbnd (TY_arb(ty)[i]);
}

inline BOOL
TY_AR_const_ubnd (const TY& ty, INT32 i)
{
    return ARB_const_ubnd(TY_arb(ty)[i]);
}

inline void
Set_TY_AR_const_ubnd (const TY& ty, INT32 i)
{
    Set_ARB_const_ubnd (TY_arb(ty)[i]);
}

inline void
Clear_TY_AR_const_ubnd (const TY& ty, INT32 i)
{
    Clear_ARB_const_ubnd (TY_arb(ty)[i]);
}

inline BOOL
TY_AR_const_stride (const TY& ty, INT32 i)
{
    return ARB_const_stride(TY_arb(ty)[i]);
}

inline void
Set_TY_AR_const_stride (const TY& ty, INT32 i)
{
    Set_ARB_const_stride (TY_arb(ty)[i]);
}

inline void
Clear_TY_AR_const_stride (const TY& ty, INT32 i)
{
    Clear_ARB_const_stride (TY_arb(ty)[i]);
}

inline BOOL
TY_AR_last_dimen (const TY& ty, INT32 i)
{
    return ARB_last_dimen(TY_arb(ty)[i]);
}

inline INT64
TY_AR_lbnd_val (const TY& ty, INT32 i)
{
    return ARB_lbnd_val(TY_arb(ty)[i]);
}

inline void
Set_TY_AR_lbnd_val (const TY& ty, INT32 i, INT64 v)
{
    Set_ARB_lbnd_val (TY_arb(ty)[i], v);
}

inline ST_IDX
TY_AR_lbnd_var (const TY& ty, INT32 i)
{
    return ARB_lbnd_var(TY_arb(ty)[i]);
}

inline void
Set_TY_AR_lbnd_var (const TY& ty, INT32 i, ST_IDX s)
{
    Set_ARB_lbnd_var (TY_arb(ty)[i], s);
}

inline INT64
TY_AR_ubnd_val (const TY& ty, INT32 i)
{
    return ARB_ubnd_val(TY_arb(ty)[i]);
}

inline void
Set_TY_AR_ubnd_val (const TY& ty, INT32 i, INT64 v)
{
    Set_ARB_ubnd_val (TY_arb(ty)[i], v);
}

inline ST_IDX
TY_AR_ubnd_var (const TY& ty, INT32 i)
{
    return ARB_ubnd_var(TY_arb(ty)[i]);
}

inline void
Set_TY_AR_ubnd_var (const TY& ty, INT32 i, ST_IDX s)
{
    Set_ARB_ubnd_var (TY_arb(ty)[i], s);
}

inline INT64
TY_AR_stride_val (const TY& ty, INT32 i)
{
    return ARB_stride_val(TY_arb(ty)[i]);
}

inline void
Set_TY_AR_stride_val (const TY& ty, INT32 i, INT64 v)
{
    Set_ARB_stride_val (TY_arb(ty)[i], v);
}

inline ST_IDX
TY_AR_stride_var (const TY& ty, INT32 i)
{
    return ARB_stride_var(TY_arb(ty)[i]);
}

inline void
Set_TY_AR_stride_var (const TY& ty, INT32 i, ST_IDX s)
{
    Set_ARB_stride_var (TY_arb(ty)[i], s);
}

inline TY_IDX
TY_AR_etype (const TY& ty)		{ return TY_etype (ty); }

inline INT32
TY_AR_ndims (const TY& ty)
{
   return ARB_dimension (TY_arb (ty));
}


// TY_AR with TY_IDX

inline BOOL
TY_AR_const_lbnd (const TY_IDX ty_idx, INT32 i)
{
    return TY_AR_const_lbnd (Ty_Table[ty_idx], i);
}

inline void
Set_TY_AR_const_lbnd (const TY_IDX ty_idx, INT32 i)
{
  Set_TY_AR_const_lbnd (Ty_Table[ty_idx], i);
}

inline void
Clear_TY_AR_const_lbnd (const TY_IDX ty_idx, INT32 i)
{
    Clear_TY_AR_const_lbnd (Ty_Table[ty_idx], i);
}

inline BOOL
TY_AR_const_ubnd (const TY_IDX ty_idx, INT32 i)
{
    return TY_AR_const_ubnd (Ty_Table[ty_idx], i);
}

inline void
Set_TY_AR_const_ubnd (const TY_IDX ty_idx, INT32 i)
{
    Set_TY_AR_const_ubnd (Ty_Table[ty_idx], i);
}

inline void
Clear_TY_AR_const_ubnd (const TY_IDX ty_idx, INT32 i)
{
    Clear_TY_AR_const_ubnd (Ty_Table[ty_idx], i);
}

inline BOOL
TY_AR_const_stride (const TY_IDX ty_idx, INT32 i)
{
    return TY_AR_const_stride(Ty_Table[ty_idx], i);
}

inline void
Set_TY_AR_const_stride (const TY_IDX ty_idx, INT32 i)
{
    Set_TY_AR_const_stride (Ty_Table[ty_idx], i);
}

inline void
Clear_TY_AR_const_stride (const TY_IDX ty_idx, INT32 i)
{
    Clear_TY_AR_const_stride (Ty_Table[ty_idx], i);
}

inline BOOL
TY_AR_last_dimen (const TY_IDX ty_idx, INT32 i)
{
    return TY_AR_last_dimen(Ty_Table[ty_idx], i);
}

inline INT64
TY_AR_lbnd_val (const TY_IDX ty_idx, INT32 i)
{
    return TY_AR_lbnd_val(Ty_Table[ty_idx], i);
}

inline void
Set_TY_AR_lbnd_val (const TY_IDX ty_idx, INT32 i, INT64 v)
{
    Set_TY_AR_lbnd_val (Ty_Table[ty_idx], i, v);
}

inline ST_IDX
TY_AR_lbnd_var (const TY_IDX ty_idx, INT32 i)
{
    return TY_AR_lbnd_var(Ty_Table[ty_idx], i);
}

inline void
Set_TY_AR_lbnd_var (const TY_IDX ty_idx, INT32 i, ST_IDX s)
{
    Set_TY_AR_lbnd_var (Ty_Table[ty_idx], i, s);
}

inline INT64
TY_AR_ubnd_val (const TY_IDX ty_idx, INT32 i)
{
    return TY_AR_ubnd_val(Ty_Table[ty_idx], i);
}

inline void
Set_TY_AR_ubnd_val (const TY_IDX ty_idx, INT32 i, INT64 v)
{
  Set_TY_AR_ubnd_val (Ty_Table[ty_idx], i, v);
}

inline ST_IDX
TY_AR_ubnd_var (const TY_IDX ty_idx, INT32 i)
{
    return TY_AR_ubnd_var(Ty_Table[ty_idx], i);
}

inline void
Set_TY_AR_ubnd_var (const TY_IDX ty_idx, INT32 i, ST_IDX s)
{
  Set_TY_AR_ubnd_var (Ty_Table[ty_idx], i, s);
}

inline INT64
TY_AR_stride_val (const TY_IDX ty_idx, INT32 i)
{
  return TY_AR_stride_val(Ty_Table[ty_idx], i);
}

inline void
Set_TY_AR_stride_val (const TY_IDX ty_idx, INT32 i, INT64 v)
{
  Set_TY_AR_stride_val (Ty_Table[ty_idx], i, v);
}

inline ST_IDX
TY_AR_stride_var (const TY_IDX ty_idx, INT32 i)
{
    return TY_AR_stride_var(Ty_Table[ty_idx], i);
}

inline void
Set_TY_AR_stride_var (const TY_IDX ty_idx, INT32 i, ST_IDX s)
{
  Set_TY_AR_stride_var (Ty_Table[ty_idx], i, s);
}

inline TY_IDX
TY_AR_etype (const TY_IDX ty_idx)   { return TY_AR_etype (Ty_Table[ty_idx]); }

inline INT32
TY_AR_ndims (const TY_IDX ty_idx)
{
    return ARB_dimension (TY_arb (Ty_Table[ty_idx]));
}
    
// PU

inline SYMTAB_IDX
PU_lexical_level (const PU_IDX pui)	{ return Pu_Table[pui].lexical_level; }

inline BOOL
PU_has_exc_scopes (const PU_IDX pui)	{ return Pu_Table[pui].flags & PU_HAS_EXC_SCOPES; }

inline BOOL
PU_in_elf_section (const PU_IDX pui)	{ return Pu_Table[pui].flags & PU_IN_ELF_SECTION; }

inline BOOL
PU_is_mainpu (const PU_IDX pui)		{ return Pu_Table[pui].flags & PU_IS_MAINPU; }

inline UINT64 
PU_src_lang (const PU_IDX pui)		{ return Pu_Table[pui].src_lang; }

// decrement the symtab/scope level
#define SYMTAB_parent(s)	(s-1)

// LABEL

inline STR_IDX
LABEL_name_idx (const LABEL_IDX idx)    { return Label_Table[idx].name_idx; }

#define Set_LABEL_kind(l,k)	Set_LABEL_KIND(l,k)

inline BOOL
LABEL_begin_eh_range (const LABEL_IDX lbi)
{
	return (LABEL_kind(Label_Table[lbi]) == LKIND_BEGIN_EH_RANGE);
}
inline void
Set_LABEL_begin_eh_range (LABEL_IDX lbi)
{
	Set_LABEL_kind(Label_Table[lbi], LKIND_BEGIN_EH_RANGE);
}
inline BOOL
LABEL_end_eh_range (const LABEL_IDX lbi)
{
	return (LABEL_kind(Label_Table[lbi]) == LKIND_END_EH_RANGE);
}
inline void
Set_LABEL_end_eh_range (LABEL_IDX lbi)
{
	Set_LABEL_kind(Label_Table[lbi], LKIND_END_EH_RANGE);
}
inline BOOL
LABEL_target_of_goto_outer_block (const LABEL_IDX lbi)
{
	return (LABEL_target_of_goto_outer_block(Label_Table[lbi]));
}
inline void
Set_LABEL_target_of_goto_outer_block (const LABEL_IDX lbi)
{
	Set_LABEL_target_of_goto_outer_block(Label_Table[lbi]);
}
inline void
Clear_LABEL_target_of_goto_outer_block (const LABEL_IDX lbi)
{
	Clear_LABEL_target_of_goto_outer_block(Label_Table[lbi]);
}
inline BOOL
LABEL_addr_saved (const LABEL_IDX lbi)
{
	return (LABEL_addr_saved(Label_Table[lbi]));
}
inline void
Set_LABEL_addr_saved (const LABEL_IDX lbi)
{
	Set_LABEL_addr_saved(Label_Table[lbi]);
}
inline void
Clear_LABEL_addr_saved (const LABEL_IDX lbi)
{
	Clear_LABEL_addr_saved(Label_Table[lbi]);
}
inline BOOL
LABEL_addr_passed (const LABEL_IDX lbi)
{
	return (LABEL_addr_passed(Label_Table[lbi]));
}
inline void
Set_LABEL_addr_passed (const LABEL_IDX lbi)
{
	Set_LABEL_addr_passed(Label_Table[lbi]);
}
inline void
Clear_LABEL_addr_passed (const LABEL_IDX lbi)
{
	Clear_LABEL_addr_passed(Label_Table[lbi]);
}

// PREG
/*
inline void
Set_Preg_Name(PREG_NUM preg_num, const char *const name)
{
  Is_True(preg_num > Last_Dedicated_Preg_Offset,
	  ("Set_Preg_Name: Cannot set name of dedicated PREG"));
  Set_PREG_name(Preg_Table[preg_num - Last_Dedicated_Preg_Offset], name);
}
*/
#endif // symtab_compatible_INCLUDED
