/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

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

// access functions for various symbol tables

// this file contains functions for direct read/write to individual field
// of each symbol table data structure.  Access functions that do not
// directly return the value of a field in a struct should be placed in
// symtab.h or symtab_util.h.

// TODO:  This file should really be generated automatically from
// symtab_defs.h 

#ifndef symtab_access_INCLUDED
#define symtab_access_INCLUDED

//----------------------------------------------------------------------
// unique handles for each structure
//----------------------------------------------------------------------

class FLD_HANDLE
{
private:
    FLD* entry;
    FLD_IDX idx;

public:
    FLD_HANDLE () : entry (NULL), idx (0) {}

    explicit FLD_HANDLE (FLD_IDX fld_idx) :
	entry (fld_idx == 0 ? NULL : &Fld_Table[fld_idx]), idx (fld_idx) {}

    FLD_HANDLE (FLD* fld, FLD_IDX fld_idx) : entry (fld), idx (fld_idx) {
	Is_True ((fld_idx == 0 && fld == NULL) || (&Fld_Table[fld_idx] == fld),
		 ("Inconsistent FLD* and FLD_IDX"));
    }

    FLD_HANDLE (const FLD_HANDLE& fld) : entry (fld.entry), idx (fld.idx) {}

    FLD_HANDLE (const FLD_ITER& iter) : entry (&(*iter)), idx (iter.Index()) {}

    FLD* Entry () const { return entry; }
    FLD_IDX Idx () const { return idx; }

    BOOL operator== (const FLD_HANDLE& fld) const {
	return idx == fld.idx;
    }

    BOOL operator!= (const FLD_HANDLE& fld) const {
	return idx != fld.idx;
    }


    BOOL Is_Null () const { return idx == 0; }
};

class ARB_HANDLE
{
private:
    ARB* entry;
    ARB_IDX idx;

public:
    ARB_HANDLE () : entry (NULL), idx (0) {}

    explicit ARB_HANDLE (ARB_IDX arb_idx) :
	entry (arb_idx == 0 ? NULL : &Arb_Table[arb_idx]), idx (arb_idx) {}

    ARB_HANDLE (ARB* arb, ARB_IDX arb_idx) : entry (arb), idx (arb_idx) {
	Is_True ((arb_idx == 0 && arb == NULL) || (&Arb_Table[arb_idx] == arb),
		 ("Inconsistent ARB* and ARB_IDX"));
    }

    ARB_HANDLE (const ARB_HANDLE& arb) : entry (arb.entry), idx (arb.idx) {}

    ARB_HANDLE (const ARB_ITER& iter) : entry (&(*iter)), idx (iter.Index()) {}

    ARB* Entry () const { return entry; }
    ARB_IDX Idx () const { return idx; }

    BOOL operator== (const ARB_HANDLE& arb) const {
	return idx == arb.idx;
    }

    ARB_HANDLE operator[] (INT32 i) {
       return ARB_HANDLE(idx + i);
    }

    BOOL Is_Null () const { return idx == 0; }
};
//----------------------------------------------------------------------
// access functions for ST
//----------------------------------------------------------------------

inline STR_IDX
ST_name_idx (const ST& s)		{ return s.u1.name_idx; }
inline void
Set_ST_name_idx (ST& s, STR_IDX idx)	{ s.u1.name_idx = idx; }
inline char *
ST_name (const ST& s)			{ return &Str_Table[ST_name_idx (s)]; }

inline TCON_IDX
ST_tcon (const ST& s)			{ return s.u1.tcon; }
inline void
Set_ST_tcon (ST& s, TCON_IDX tcon)	{ s.u1.tcon = tcon; }

inline ST_CLASS
ST_sym_class (const ST& s)		{ return s.sym_class; }
inline void
Set_ST_sym_class (ST& s, ST_CLASS c)	{ s.sym_class = c; }

inline ST_SCLASS
ST_storage_class (const ST& s)		{ return s.storage_class; }
inline void
Set_ST_storage_class (ST& s, ST_SCLASS sc)	{ s.storage_class = sc; }

inline ST_SCLASS
ST_sclass (const ST& s)			{ return s.storage_class; }
inline void
Set_ST_sclass (ST& s, ST_SCLASS sc)	{ s.storage_class = sc; }

inline ST_EXPORT
ST_export (const ST& s)			{ return s.export_class; }
inline void
Set_ST_export (ST& s, ST_EXPORT eclass) { s.export_class = eclass; }

inline TY_IDX
ST_type (const ST& s) {
#ifdef Is_True_On
    switch (s.sym_class) {
    default:
	Fail_FmtAssertion ("Invalid argument for ST_type ()");
    case CLASS_VAR:
    case CLASS_CONST:
    case CLASS_PREG:
    case CLASS_NAME:
	return s.u2.type;
    }
#else
    return s.u2.type;
#endif // Is_True_On
}
inline void
Set_ST_type (ST& s, TY_IDX t) {
#ifdef Is_True_On
    switch (s.sym_class) {
    default:
	Fail_FmtAssertion ("Invalid argument for ST_type ()");
    case CLASS_VAR:
    case CLASS_CONST:
    case CLASS_PREG:
    case CLASS_NAME:
	s.u2.type = t;
    }
#else
    s.u2.type = t;
#endif // Is_True_On
}

inline PU_IDX
ST_pu (const ST& s) {
    Is_True (s.sym_class == CLASS_FUNC ||
	     (s.sym_class == CLASS_NAME &&
	      (s.flags & ST_ASM_FUNCTION_ST)),
	     ("Invalid argument for ST_pu"));
    return s.u2.pu;
}
inline void
Set_ST_pu (ST& s, PU_IDX pu)
{
    Is_True (s.sym_class == CLASS_FUNC ||
	     (s.sym_class == CLASS_NAME &&
	      s.flags & ST_ASM_FUNCTION_ST),
	     ("Invalid argument for ST_pu"));
    s.u2.pu = pu;
}

inline BLK_IDX
ST_blk (const ST& s) {
    Is_True (s.sym_class == CLASS_BLOCK, ("Invalid argument for ST_blk"));
    return s.u2.blk;
}
inline void
Set_ST_blk (ST& s, BLK_IDX b)
{
    Is_True (s.sym_class == CLASS_BLOCK, ("Invalid argument for ST_blk"));
    s.u2.blk = b;
}

inline ST_IDX
ST_st_idx (const ST& s)			{ return s.st_idx; }
inline void
Set_ST_st_idx (ST& s, ST_IDX idx)	{ s.st_idx = idx; }

inline TY_IDX
ST_vtable_ty_idx (const ST& s)			{ return s.vtable_ty_idx; }
inline void
Set_ST_vtable_ty_idx (ST& s, TY_IDX idx)	{ s.vtable_ty_idx = idx; }

inline mUINT32
ST_Line (const ST& s)    { return s.line; }
inline void
Set_ST_Line (ST& s, mUINT32 lineno)    { s.line = lineno; }

inline ST*
ST_ptr (ST_IDX idx)                     { return &(St_Table[idx]); }

inline UINT64
ST_ofst (const ST& s)			{ return s.offset; }
inline void
Set_ST_ofst (ST& s, UINT64 offset)	{ s.offset = offset; }

//----------------------------------------------------------------------
// ST flags
//----------------------------------------------------------------------

inline BOOL
ST_is_weak_symbol (const ST& s)		{ return s.flags & ST_IS_WEAK_SYMBOL;}
inline void
Set_ST_is_weak_symbol (ST& s)		{ s.flags |= ST_IS_WEAK_SYMBOL; }
inline void
Clear_ST_is_weak_symbol (ST& s)		{ s.flags &= ~ST_IS_WEAK_SYMBOL; }

inline BOOL
ST_is_split_common (const ST& s)	{ return s.flags & ST_IS_SPLIT_COMMON;}
inline void
Set_ST_is_split_common (ST& s)		{ s.flags |= ST_IS_SPLIT_COMMON; }
inline void
Clear_ST_is_split_common (ST& s)	{ s.flags &= ~ST_IS_SPLIT_COMMON; }

inline ST_IDX
ST_base_idx (const ST& s)		
{ 
    if (ST_is_split_common (s))
        return s.st_idx;
    else if (ST_is_weak_symbol (s) && ST_sclass(s) == SCLASS_EXTERN)
        return s.st_idx;
    else
        return s.base_idx;
}
inline void
Set_ST_base_idx (ST& s, ST_IDX base)	
{ 
    if (ST_is_split_common (s))
	DevWarn ("Shouldn't set base when split");
    else if (ST_is_weak_symbol (s) && ST_sclass(s) == SCLASS_EXTERN)
	DevWarn ("Shouldn't set base when weak");
    s.base_idx = base;
}

inline ST_IDX
ST_strong_idx (const ST& s)
{
    Is_True (ST_is_weak_symbol (s), ("Expecting a weak symbol"));
    return (ST_sclass (s) == SCLASS_EXTERN) ? s.base_idx : s.st_idx;
}
inline void
Set_ST_strong_idx (ST& s, ST_IDX base)
{
    Is_True (ST_is_weak_symbol (s) && ST_sclass(s) == SCLASS_EXTERN,
	     ("Expecting an external weak symbol"));
    s.base_idx = base;
}

inline ST_IDX
ST_full_idx (const ST& s)
{
    Is_True (ST_is_split_common (s), ("Expecting a splict common"));
    return s.base_idx;
}
inline void
Set_ST_full_idx (ST& s, ST_IDX base)
{
    Is_True (ST_is_split_common (s), ("Expecting a splict common"));
    s.base_idx = base;
} 

inline BOOL
ST_is_not_used (const ST& s)		{ return s.flags & ST_IS_NOT_USED;}
inline void
Set_ST_is_not_used (ST& s)		{ s.flags |= ST_IS_NOT_USED; }
inline void
Clear_ST_is_not_used (ST& s)		{ s.flags &= ~ST_IS_NOT_USED; }

inline BOOL
ST_is_initialized (const ST& s)		{ return s.flags & ST_IS_INITIALIZED;}
inline void
Set_ST_is_initialized (ST& s)		{ s.flags |= ST_IS_INITIALIZED; }
inline void
Clear_ST_is_initialized (ST& s)		{ s.flags &= ~ST_IS_INITIALIZED; }

inline BOOL
ST_is_return_var (const ST& s)		{ return s.flags & ST_IS_RETURN_VAR;}
inline void
Set_ST_is_return_var (ST& s)		{ s.flags |= ST_IS_RETURN_VAR; }
inline void
Clear_ST_is_return_var (ST& s)		{ s.flags &= ~ST_IS_RETURN_VAR; }

inline BOOL
ST_is_value_parm (const ST& s)		{ return s.flags & ST_IS_VALUE_PARM;}
inline void
Set_ST_is_value_parm (ST& s)		{ s.flags |= ST_IS_VALUE_PARM; }
inline void
Clear_ST_is_value_parm (ST& s)		{ s.flags &= ~ST_IS_VALUE_PARM; }

inline BOOL
ST_promote_parm (const ST& s)		{ return s.flags & ST_PROMOTE_PARM;}
inline void
Set_ST_promote_parm (ST& s)		{ s.flags |= ST_PROMOTE_PARM; }
inline void
Clear_ST_promote_parm (ST& s)		{ s.flags &= ~ST_PROMOTE_PARM; }

inline BOOL
ST_keep_name_w2f (const ST& s)		{ return s.flags & ST_KEEP_NAME_W2F;}
inline void
Set_ST_keep_name_w2f (ST& s)		{ s.flags |= ST_KEEP_NAME_W2F; }
inline void
Clear_ST_keep_name_w2f (ST& s)		{ s.flags &= ~ST_KEEP_NAME_W2F; }

inline BOOL
ST_is_datapool (const ST& s)		{ return s.flags & ST_IS_DATAPOOL;}
inline void
Set_ST_is_datapool (ST& s)		{ s.flags |= ST_IS_DATAPOOL; }
inline void
Clear_ST_is_datapool (ST& s)		{ s.flags &= ~ST_IS_DATAPOOL; }

inline BOOL
ST_is_reshaped (const ST& s)		{ return s.flags & ST_IS_RESHAPED;}
inline void
Set_ST_is_reshaped (ST& s)		{ s.flags |= ST_IS_RESHAPED; }
inline void
Clear_ST_is_reshaped (ST& s)		{ s.flags &= ~ST_IS_RESHAPED; }

inline BOOL
ST_emit_symbol (const ST& s)		{ return s.flags & ST_EMIT_SYMBOL;}
inline void
Set_ST_emit_symbol (ST& s)		{ s.flags |= ST_EMIT_SYMBOL; }
inline void
Clear_ST_emit_symbol (ST& s)		{ s.flags &= ~ST_EMIT_SYMBOL; }

inline BOOL
ST_has_nested_ref (const ST& s)		{ return s.flags & ST_HAS_NESTED_REF;}
inline void
Set_ST_has_nested_ref (ST& s)		{ s.flags |= ST_HAS_NESTED_REF; }
inline void
Clear_ST_has_nested_ref (ST& s)		{ s.flags &= ~ST_HAS_NESTED_REF; }

inline BOOL
ST_init_value_zero (const ST& s)	{ return s.flags & ST_INIT_VALUE_ZERO;}
inline void
Set_ST_init_value_zero (ST& s)		{ s.flags |= ST_INIT_VALUE_ZERO; }
inline void
Clear_ST_init_value_zero (ST& s)	{ s.flags &= ~ST_INIT_VALUE_ZERO; }

inline BOOL
ST_gprel (const ST& s)			{ return s.flags & ST_GPREL;}
inline void
Set_ST_gprel (ST& s)			{ s.flags |= ST_GPREL; }
inline void
Clear_ST_gprel (ST& s)			{ s.flags &= ~ST_GPREL; }

inline BOOL
ST_not_gprel (const ST& s)		{ return s.flags & ST_NOT_GPREL;}
inline void
Set_ST_not_gprel (ST& s)		{ s.flags |= ST_NOT_GPREL; }
inline void
Clear_ST_not_gprel (ST& s)		{ s.flags &= ~ST_NOT_GPREL; }

inline BOOL
ST_is_namelist (const ST& s)		{ return s.flags & ST_IS_NAMELIST;}
inline void
Set_ST_is_namelist (ST& s)		{ s.flags |= ST_IS_NAMELIST; }
inline void
Clear_ST_is_namelist (ST& s)		{ s.flags &= ~ST_IS_NAMELIST; }

inline BOOL
ST_is_f90_target (const ST& s)		{ return s.flags & ST_IS_F90_TARGET;}
inline void
Set_ST_is_f90_target (ST& s)		{ s.flags |= ST_IS_F90_TARGET; }
inline void
Clear_ST_is_f90_target (ST& s)		{ s.flags &= ~ST_IS_F90_TARGET; }

inline BOOL
ST_declared_static (const ST& s)	{ return s.flags & ST_DECLARED_STATIC;}
inline void
Set_ST_declared_static (ST& s)		{ s.flags |= ST_DECLARED_STATIC; }
inline void
Clear_ST_declared_static (ST& s)	{ s.flags &= ~ST_DECLARED_STATIC; }

inline BOOL
ST_is_equivalenced (const ST& s)	{ return s.flags & ST_IS_EQUIVALENCED;}
inline void
Set_ST_is_equivalenced (ST& s)		{ s.flags |= ST_IS_EQUIVALENCED; }
inline void
Clear_ST_is_equivalenced (ST& s)	{ s.flags &= ~ST_IS_EQUIVALENCED; }

inline BOOL
ST_is_fill_align (const ST& s)		{ return s.flags & ST_IS_FILL_ALIGN;}
inline void
Set_ST_is_fill_align (ST& s)		{ s.flags |= ST_IS_FILL_ALIGN; }
inline void
Clear_ST_is_fill_align (ST& s)		{ s.flags &= ~ST_IS_FILL_ALIGN; }

inline BOOL
ST_is_optional_argument (const ST& s)	{ return s.flags & ST_IS_OPTIONAL_ARGUMENT;}
inline void
Set_ST_is_optional_argument (ST& s)	{ s.flags |= ST_IS_OPTIONAL_ARGUMENT; }
inline void
Clear_ST_is_optional_argument (ST& s)	{ s.flags &= ~ST_IS_OPTIONAL_ARGUMENT; }

inline BOOL
ST_is_temp_var (const ST& s)		{ return s.flags & ST_IS_TEMP_VAR;}
inline void
Set_ST_is_temp_var (ST& s)		{ s.flags |= ST_IS_TEMP_VAR; }
inline void
Clear_ST_is_temp_var (ST& s)		{ s.flags &= ~ST_IS_TEMP_VAR; }

inline BOOL
ST_is_const_var (const ST& s)		{ return s.flags & ST_IS_CONST_VAR;}
inline void
Set_ST_is_const_var (ST& s)		{ s.flags |= ST_IS_CONST_VAR; }
inline void
Clear_ST_is_const_var (ST& s)		{ s.flags &= ~ST_IS_CONST_VAR; }

inline BOOL
ST_addr_saved (const ST& s)		{ return s.flags & ST_ADDR_SAVED;}
inline void
Set_ST_addr_saved (ST& s)		{ s.flags |= ST_ADDR_SAVED; }
inline void
Clear_ST_addr_saved (ST& s)		{ s.flags &= ~ST_ADDR_SAVED; }

inline BOOL
ST_addr_passed (const ST& s)		{ return s.flags & ST_ADDR_PASSED;}
inline void
Set_ST_addr_passed (ST& s)		{ s.flags |= ST_ADDR_PASSED; }
inline void
Clear_ST_addr_passed (ST& s)		{ s.flags &= ~ST_ADDR_PASSED; }

inline BOOL
ST_is_thread_private (const ST& s)	{ return s.flags & ST_IS_THREAD_PRIVATE;}
inline void
Set_ST_is_thread_private (ST& s)	{ s.flags |= ST_IS_THREAD_PRIVATE; }
inline void
Clear_ST_is_thread_private (ST& s)	{ s.flags &= ~ST_IS_THREAD_PRIVATE; }

inline BOOL
ST_pt_to_unique_mem (const ST& s)	{ return s.flags & ST_PT_TO_UNIQUE_MEM;}
inline void
Set_ST_pt_to_unique_mem (ST& s)		{ s.flags |= ST_PT_TO_UNIQUE_MEM; }
inline void
Clear_ST_pt_to_unique_mem (ST& s)	{ s.flags &= ~ST_PT_TO_UNIQUE_MEM; }

inline BOOL
ST_pt_to_compiler_generated_mem (const ST& s)	
	{ return s.flags &  ST_PT_TO_COMPILER_GENERATED_MEM;}
inline void
Set_ST_pt_to_compiler_generated_mem (ST& s)		
	{ s.flags |=  ST_PT_TO_COMPILER_GENERATED_MEM; }
inline void
Clear_ST_pt_to_compiler_generated_mem (ST& s)	
	{ s.flags &= ~ST_PT_TO_COMPILER_GENERATED_MEM; }

inline BOOL
ST_is_shared_auto (const ST& s)	{ return s.flags & ST_IS_SHARED_AUTO;}
inline void
Set_ST_is_shared_auto (ST& s)	{ s.flags |= ST_IS_SHARED_AUTO; }
inline void
Clear_ST_is_shared_auto (ST& s)	{ s.flags &= ~ST_IS_SHARED_AUTO; }

inline BOOL
ST_assigned_to_dedicated_preg (const ST& s)
	{ return s.flags & ST_ASSIGNED_TO_DEDICATED_PREG; }
inline void
Set_ST_assigned_to_dedicated_preg (ST& s)
	{ s.flags |= ST_ASSIGNED_TO_DEDICATED_PREG; }
inline void
Clear_ST_assigned_to_dedicated_preg (ST& s)
	{ s.flags &= ~ST_ASSIGNED_TO_DEDICATED_PREG; }

inline BOOL
ST_asm_function_st(const ST& s)	{ return s.flags & ST_ASM_FUNCTION_ST; }
inline void
Set_ST_asm_function_st(ST &s)	{ s.flags |= ST_ASM_FUNCTION_ST; }
inline void
Clear_ST_asm_function_st(ST &s)	{ s.flags &= ~ST_ASM_FUNCTION_ST; }

// TODO:  should probably replace all ref params with ST*
inline BOOL
ST_has_named_section (const ST* s)	{ return s->flags & ST_HAS_NAMED_SECTION; }
inline void
Set_ST_has_named_section (ST* s)	{ s->flags |= ST_HAS_NAMED_SECTION; }
inline void
Clear_ST_has_named_section (ST* s)	{ s->flags &= ~ST_HAS_NAMED_SECTION; }

#ifdef KEY
inline BOOL
ST_one_per_pu (const ST* s)	{ return s->flags_ext & ST_ONE_PER_PU; }
inline void
Set_ST_one_per_pu (ST* s)	{ s->flags_ext |= ST_ONE_PER_PU; }
inline void
Clear_ST_one_per_pu (ST* s)	{ s->flags_ext &= ~ST_ONE_PER_PU; }

inline BOOL
ST_copy_constructor_st(const ST* s)	{ return s->flags_ext & ST_COPY_CONSTRUCTOR_ST; }
inline void
Set_ST_copy_constructor_st(ST* s)	{ s->flags_ext |= ST_COPY_CONSTRUCTOR_ST; }
inline void
Clear_ST_copy_constructor_st(ST* s)	{ s->flags_ext &= ~ST_COPY_CONSTRUCTOR_ST; }

inline BOOL
ST_initv_in_other_st (const ST* s)     { return s->flags_ext & ST_INITV_IN_OTHER_ST; }
inline void
Set_ST_initv_in_other_st (ST* s)       { s->flags_ext |= ST_INITV_IN_OTHER_ST; }
inline void
Clear_ST_initv_in_other_st (ST* s)     { s->flags_ext &= ~ST_INITV_IN_OTHER_ST; }

#ifdef TARG_SL
inline BOOL
ST_in_v1buf (const ST* s)     { return s->flags_ext & ST_IN_V1BUF; }
inline void
Set_ST_in_v1buf (ST *s)       { s->flags_ext |= ST_IN_V1BUF; }
inline void
Clear_ST_in_v1buf (ST *s)     { s->flags_ext &= ~ST_IN_V1BUF; }

inline BOOL
ST_in_v2buf (const ST* s)     { return s->flags_ext & ST_IN_V2BUF; }
inline void
Set_ST_in_v2buf (ST *s)       { s->flags_ext |= ST_IN_V2BUF; }
inline void
Clear_ST_in_v2buf (ST *s)     { s->flags_ext &= ~ST_IN_V2BUF; }

inline BOOL
ST_in_v4buf (const ST* s)     { return s->flags_ext & ST_IN_V4BUF; }
inline void
Set_ST_in_v4buf (ST *s)       { s->flags_ext |= ST_IN_V4BUF; }
inline void
Clear_ST_in_v4buf (ST *s)     { s->flags_ext &= ~ST_IN_V4BUF; }

inline BOOL
ST_in_vbuf (const ST *s) { 
    return (ST_in_v1buf(s) || ST_in_v2buf(s) || ST_in_v4buf(s));
}

inline BOOL
ST_in_sdram (const ST* s)     { return s->flags_ext & ST_IN_SDRAM; }
inline void
Set_ST_in_sdram (ST *s)       { s->flags_ext |= ST_IN_SDRAM; }
inline void
Clear_ST_in_sdram (ST *s)     { s->flags_ext &= ~ST_IN_SDRAM; }

inline BOOL
ST_in_sbuf (const ST* s)     { return s->flags_ext & ST_IN_SBUF; }
inline void
Set_ST_in_sbuf (ST *s)       { s->flags_ext |= ST_IN_SBUF; }
inline void
Clear_ST_in_sbuf (ST *s)     { s->flags_ext &= ~ST_IN_SBUF; }

inline BOOL	
ST_is_vbuf_ofst (const ST* s)     { return s->flags_ext & ST_IS_VBUF_OFFSET; }
inline void
Set_ST_is_vbuf_ofst (ST *s)       { s->flags_ext |= ST_IS_VBUF_OFFSET; }
inline void
Clear_ST_is_vbuf_ofst (ST *s)     { s->flags_ext &= ~ST_IS_VBUF_OFFSET; }

inline BOOL
ST_is_sbuf_ofst (const ST* s)     { return s->flags_ext & ST_IS_SBUF_OFFSET; }
inline void
Set_ST_is_sbuf_ofst (ST *s)       { s->flags_ext |= ST_IS_SBUF_OFFSET; }
inline void
Clear_ST_is_sbuf_ofst (ST *s)     { s->flags_ext &= ~ST_IS_SBUF_OFFSET; }
#endif // TARG_SL

inline BOOL
ST_is_inintialized_in_f90 (const ST* s)     { return s->flags_ext & ST_IS_INITIALIZED_IN_F90; }
inline void
Set_ST_is_inintialized_in_f90 (ST *s)       { s->flags_ext |= ST_IS_INITIALIZED_IN_F90; }
inline void
Clear_ST_is_inintialized_in_f90 (ST *s)     { s->flags_ext &= ~ST_IS_INITIALIZED_IN_F90; }

inline BOOL
ST_is_method_func (const ST* s)	{ return s->flags_ext & ST_IS_METHOD_FUNC; }
inline void
Set_ST_is_method_func (ST* s)   { s->flags_ext |= ST_IS_METHOD_FUNC; }
inline void
Reset_ST_is_method_func (ST* s) { s->flags_ext &= ~ST_IS_METHOD_FUNC; }

inline BOOL
ST_is_this_ptr (const ST* s)  { return s->flags_ext & ST_IS_THIS_PTR; }
inline void
Set_ST_is_this_ptr (ST* s)    { s->flags_ext |= ST_IS_THIS_PTR; }
inline void
Reset_ST_is_this_ptr (ST* s)  { s->flags_ext &= ~ST_IS_THIS_PTR; }

#ifdef TARG_NVISA
inline BOOL
ST_in_global_mem (const ST* s)     { return (s->memory_space == MEMORY_GLOBAL); }
inline void
Set_ST_in_global_mem (ST *s)       { s->memory_space = MEMORY_GLOBAL; }

inline BOOL
ST_in_local_mem (const ST* s)     { return (s->memory_space == MEMORY_LOCAL); }
inline void
Set_ST_in_local_mem (ST *s)       { s->memory_space = MEMORY_LOCAL; }

inline BOOL
ST_in_shared_mem (const ST* s)     { return (s->memory_space == MEMORY_SHARED); }
inline void
Set_ST_in_shared_mem (ST *s)       { s->memory_space = MEMORY_SHARED; }

inline BOOL
ST_in_constant_mem (const ST* s)     { return (s->memory_space == MEMORY_CONSTANT); }
inline void
Set_ST_in_constant_mem (ST *s)       { s->memory_space = MEMORY_CONSTANT; }

inline BOOL
ST_in_texture_mem (const ST* s)     { return (s->memory_space == MEMORY_TEXTURE); }
inline void
Set_ST_in_texture_mem (ST *s)       { s->memory_space = MEMORY_TEXTURE; }

inline BOOL
ST_in_param_mem (const ST* s)     { return (s->memory_space == MEMORY_PARAM); }
inline void
Set_ST_in_param_mem (ST *s)       { s->memory_space = MEMORY_PARAM; }
#else
/* tls-model */
inline ST_TLS_MODEL
ST_tls_model (const ST* s)                   { return s->tls_model;  }
inline void
Set_ST_tls_model (ST* s, ST_TLS_MODEL model) { s->tls_model = model; }
#endif /* TARG_NVISA */

inline BOOL
ST_is_pure_vfunc (const ST* s)  { return s->flags_ext & ST_IS_PURE_VFUNC; }
inline void
Set_ST_is_pure_vfunc (ST* s)    { s->flags_ext |= ST_IS_PURE_VFUNC; }
inline void
Reset_ST_is_pure_vfunc (ST* s)  { s->flags_ext &= ~ST_IS_PURE_VFUNC; }

inline BOOL
ST_is_thread_local (const ST* s) { return s->flags_ext & ST_IS_THREAD_LOCAL;}
inline void
Set_ST_is_thread_local (ST* s)	 { s->flags_ext |= ST_IS_THREAD_LOCAL; }
inline void
Reset_ST_is_thread_local (ST* s) { s->flags_ext &= ~ST_IS_THREAD_LOCAL; }
inline BOOL
ST_is_thread_local (const ST& s) { return s.flags_ext & ST_IS_THREAD_LOCAL; }
inline void
Set_ST_is_thread_local (ST& s)   { s.flags_ext |= ST_IS_THREAD_LOCAL; }
inline void
Reset_ST_is_thread_local (ST& s) { s.flags_ext &= ~ST_IS_THREAD_LOCAL; }
inline void
Clear_ST_is_thread_local (ST* s) { s->flags_ext &= ~ST_IS_THREAD_LOCAL; }

inline BOOL
ST_is_array_remapping_candidate(const ST *s) { return s->flags_ext & ST_IS_ARRAY_REMAPPING_CANDIDATE; }
inline void
Set_ST_is_array_remapping_candidate(ST *s) { s->flags_ext |= ST_IS_ARRAY_REMAPPING_CANDIDATE; }
inline void
Clear_ST_is_array_remapping_candidate(ST *s) { s->flags_ext &= ~ST_IS_ARRAY_REMAPPING_CANDIDATE; }

inline BOOL
ST_is_array_remapping_candidate_malloc(const ST *s) { return s->flags_ext & ST_IS_ARRAY_REMAPPING_CANDIDATE_MALLOC; }
inline void
Set_ST_is_array_remapping_candidate_malloc(ST *s) { s->flags_ext |= ST_IS_ARRAY_REMAPPING_CANDIDATE_MALLOC; }
inline void
Clear_ST_is_array_remapping_candidate_malloc(ST *s) { s->flags_ext &= ~ST_IS_ARRAY_REMAPPING_CANDIDATE_MALLOC; }
inline BOOL
ST_is_global_as_local(const ST * s) { return s->flags_ext & ST_IS_GLOBAL_AS_LOCAL; }
inline void
Set_ST_is_global_as_local(ST * s) { s->flags_ext |= ST_IS_GLOBAL_AS_LOCAL; }
inline void 
Clear_ST_is_global_as_local(ST *s) { s->flags_ext &= ~ST_IS_GLOBAL_AS_LOCAL; }

inline BOOL
ST_is_vtable (const ST* s)  { return s->flags_ext & ST_IS_VTABLE; }
inline void
Set_ST_is_vtable (ST* s)    { s->flags_ext |= ST_IS_VTABLE; }
inline void
Reset_ST_is_vtable (ST* s)  { s->flags_ext &= ~ST_IS_VTABLE; }

#endif /* KEY */

//----------------------------------------------------------------------
// access functions for PU
//----------------------------------------------------------------------

inline TARGET_INFO_IDX
PU_target_idx (const PU& pu)		{ return pu.target_idx; }
inline void
Set_PU_target_idx (PU& pu, TARGET_INFO_IDX idx) { pu.target_idx = idx; }

inline TY_IDX
PU_prototype (const PU& pu)		{ return pu.prototype; }
inline void
Set_PU_prototype (PU& pu, TY_IDX ty)	{ pu.prototype = ty; }

inline UINT8
PU_gp_group (const PU& pu)		{ return pu.gp_group; }
inline void
Set_PU_gp_group (PU& pu, mUINT8 grp)	{ pu.gp_group = grp; }

inline SYMTAB_IDX
PU_lexical_level (const PU& pu)		{ return pu.lexical_level; }
inline void
Set_PU_lexical_level (PU& pu, SYMTAB_IDX l) { pu.lexical_level = l; }

inline INITO_IDX
PU_misc_info (const PU& pu)		{ return pu.misc; }
inline void
Set_PU_misc_info (PU& pu, INITO_IDX i)	{ pu.misc = i; }

inline TY_IDX 
PU_base_class (const PU& pu) { return pu.base_class; } 
inline void 
Set_PU_base_class (PU& pu, TY_IDX ty_idx) { pu.base_class = ty_idx; } 

#ifdef TARG_NVISA
inline SYMTAB_IDX
PU_thread_limit(const PU& pu)		{ return pu.thread_limit; }
inline void
Set_PU_thread_limit (PU& pu, int l) { pu.thread_limit = l; }

inline SYMTAB_IDX
PU_block_limit(const PU& pu)		{ return pu.block_limit; }
inline void
Set_PU_block_limit (PU& pu, int l) { pu.block_limit = l; }
#endif /* TARG_NVISA */

//----------------------------------------------------------------------
// PU flags
//----------------------------------------------------------------------

inline BOOL
PU_is_pure (const PU& pu)		{ return pu.flags & PU_IS_PURE; }
inline void
Set_PU_is_pure (PU& pu)			{ pu.flags |= PU_IS_PURE; }
inline void
Clear_PU_is_pure (PU& pu)		{ pu.flags &= ~PU_IS_PURE; }

inline BOOL
PU_no_side_effects (const PU& pu)	{ return pu.flags & PU_NO_SIDE_EFFECTS; }
inline void
Set_PU_no_side_effects (PU& pu)		{ pu.flags |= PU_NO_SIDE_EFFECTS; }
inline void
Clear_PU_no_side_effects (PU& pu)	{ pu.flags &= ~PU_NO_SIDE_EFFECTS; }

inline BOOL
PU_is_inline_function (const PU& pu)	{ return pu.flags & PU_IS_INLINE_FUNCTION; }
inline void
Set_PU_is_inline_function (PU& pu)	{ pu.flags |= PU_IS_INLINE_FUNCTION; }
inline void
Clear_PU_is_inline_function (PU& pu)	{ pu.flags &= ~PU_IS_INLINE_FUNCTION; }

inline BOOL
PU_no_inline (const PU& pu)		{ return pu.flags & PU_NO_INLINE; }
inline void
Set_PU_no_inline (PU& pu)		{ pu.flags |= PU_NO_INLINE; }
inline void
Clear_PU_no_inline (PU& pu)		{ pu.flags &= ~PU_NO_INLINE; }

inline BOOL
PU_must_inline (const PU& pu)		{ return pu.flags & PU_MUST_INLINE; }
inline void
Set_PU_must_inline (PU& pu)		{ pu.flags |= PU_MUST_INLINE; }
inline void
Clear_PU_must_inline (PU& pu)		{ pu.flags &= ~PU_MUST_INLINE; }

inline BOOL
PU_no_delete (const PU& pu)		{ return pu.flags & PU_NO_DELETE; }
inline void
Set_PU_no_delete (PU& pu)		{ pu.flags |= PU_NO_DELETE; }
inline void
Clear_PU_no_delete (PU& pu)		{ pu.flags &= ~PU_NO_DELETE; }

inline BOOL
PU_has_exc_scopes (const PU& pu)	{ return pu.flags & PU_HAS_EXC_SCOPES; }
inline void
Set_PU_has_exc_scopes (PU& pu)		{ pu.flags |= PU_HAS_EXC_SCOPES; }
inline void
Clear_PU_has_exc_scopes (PU& pu)	{ pu.flags &= ~PU_HAS_EXC_SCOPES; }

inline BOOL
PU_is_nested_func (const PU& pu)	{ return pu.flags & PU_IS_NESTED_FUNC; }
inline void
Set_PU_is_nested_func (PU& pu)		{ pu.flags |= PU_IS_NESTED_FUNC; }
inline void
Clear_PU_is_nested_func (PU& pu)	{ pu.flags &= ~PU_IS_NESTED_FUNC; }

inline BOOL
PU_has_non_mangled_call (const PU& pu)	{ return pu.flags & PU_HAS_NON_MANGLED_CALL; }
inline void
Set_PU_has_non_mangled_call (PU& pu)	{ pu.flags |= PU_HAS_NON_MANGLED_CALL; }
inline void
Clear_PU_has_non_mangled_call (PU& pu)	{ pu.flags &= ~PU_HAS_NON_MANGLED_CALL; }

inline BOOL
PU_args_aliased (const PU& pu)		{ return pu.flags & PU_ARGS_ALIASED; }
inline void
Set_PU_args_aliased (PU& pu)		{ pu.flags |= PU_ARGS_ALIASED; }
inline void
Clear_PU_args_aliased (PU& pu)		{ pu.flags &= ~PU_ARGS_ALIASED; }

inline BOOL
PU_needs_fill_align_lowering (const PU& pu) {
    return pu.flags & PU_NEEDS_FILL_ALIGN_LOWERING;
}
inline void
Set_PU_needs_fill_align_lowering (PU& pu) {
    pu.flags |= PU_NEEDS_FILL_ALIGN_LOWERING;
}
inline void
Clear_PU_needs_fill_align_lowering (PU& pu) {
    pu.flags &= ~PU_NEEDS_FILL_ALIGN_LOWERING;
}

inline BOOL
PU_needs_t9 (const PU& pu)		{ return pu.flags & PU_NEEDS_T9; }
inline void
Set_PU_needs_t9 (PU& pu)		{ pu.flags |= PU_NEEDS_T9; }
inline void
Clear_PU_needs_t9 (PU& pu)		{ pu.flags &= ~PU_NEEDS_T9; }

inline BOOL
PU_has_very_high_whirl (const PU& pu)	{ return pu.flags & PU_HAS_VERY_HIGH_WHIRL; }
inline void
Set_PU_has_very_high_whirl (PU& pu)	{ pu.flags |= PU_HAS_VERY_HIGH_WHIRL; }
inline void
Clear_PU_has_very_high_whirl (PU& pu)	{ pu.flags &= ~PU_HAS_VERY_HIGH_WHIRL; }

inline BOOL
PU_has_altentry (const PU& pu)		{ return pu.flags & PU_HAS_ALTENTRY; }
inline void
Set_PU_has_altentry (PU& pu)		{ pu.flags |= PU_HAS_ALTENTRY; }
inline void
Clear_PU_has_altentry (PU& pu)		{ pu.flags &= ~PU_HAS_ALTENTRY; }

inline BOOL
PU_recursive (const PU& pu)		{ return pu.flags & PU_RECURSIVE; }
inline void
Set_PU_recursive (PU& pu)		{ pu.flags |= PU_RECURSIVE; }
inline void
Clear_PU_recursive (PU& pu)		{ pu.flags &= ~PU_RECURSIVE; }

inline BOOL
PU_is_mainpu (const PU& pu)		{ return pu.flags & PU_IS_MAINPU; }
inline void
Set_PU_is_mainpu (PU& pu)		{ pu.flags |= PU_IS_MAINPU; }
inline void
Clear_PU_is_mainpu (PU& pu)		{ pu.flags &= ~PU_IS_MAINPU; }

inline BOOL
PU_uplevel (const PU& pu)		{ return pu.flags & PU_UPLEVEL; }
inline void
Set_PU_uplevel (PU& pu)			{ pu.flags |= PU_UPLEVEL; }
inline void
Clear_PU_uplevel (PU& pu)		{ pu.flags &= ~PU_UPLEVEL; }

inline BOOL
PU_mp_needs_lno (const PU& pu)		{ return pu.flags & PU_MP_NEEDS_LNO; }
inline void
Set_PU_mp_needs_lno (PU& pu)		{ pu.flags |= PU_MP_NEEDS_LNO; }
inline void
Clear_PU_mp_needs_lno (PU& pu)		{ pu.flags &= ~PU_MP_NEEDS_LNO; }

inline BOOL
PU_has_alloca (const PU& pu)		{ return pu.flags & PU_HAS_ALLOCA; }
inline void
Set_PU_has_alloca (PU& pu)		{ pu.flags |= PU_HAS_ALLOCA; }
inline void
Clear_PU_has_alloca (PU& pu)		{ pu.flags &= ~PU_HAS_ALLOCA; }

inline BOOL
PU_in_elf_section (const PU& pu)	{ return pu.flags & PU_IN_ELF_SECTION; }
inline void
Set_PU_in_elf_section (PU& pu)		{ pu.flags |= PU_IN_ELF_SECTION; }
inline void
Clear_PU_in_elf_section (PU& pu)	{ pu.flags &= ~PU_IN_ELF_SECTION; }

inline BOOL
PU_has_mp (const PU& pu)		{ return pu.flags & PU_HAS_MP; }
inline void
Set_PU_has_mp (PU& pu)			{ pu.flags |= PU_HAS_MP; }
inline void
Clear_PU_has_mp (PU& pu)		{ pu.flags &= ~PU_HAS_MP; }

inline BOOL
PU_mp (const PU& pu)			{ return pu.flags & PU_MP; }
inline void
Set_PU_mp (PU& pu)			{ pu.flags |= PU_MP; }
inline void
Clear_PU_mp (PU& pu)			{ pu.flags &= ~PU_MP; }

inline BOOL
PU_has_namelist (const PU& pu)		{ return pu.flags & PU_HAS_NAMELIST; }
inline void
Set_PU_has_namelist (PU& pu)		{ pu.flags |= PU_HAS_NAMELIST; }
inline void
Clear_PU_has_namelist (PU& pu)		{ pu.flags &= ~PU_HAS_NAMELIST; }

inline BOOL
PU_has_return_address (const PU& pu)	{ return pu.flags & PU_HAS_RETURN_ADDRESS; }
inline void
Set_PU_has_return_address (PU& pu)	{ pu.flags |= PU_HAS_RETURN_ADDRESS; }
inline void
Clear_PU_has_return_address (PU& pu)	{ pu.flags &= ~PU_HAS_RETURN_ADDRESS; }

inline BOOL
PU_has_region (const PU& pu)		{ return pu.flags & PU_HAS_REGION; }
inline void
Set_PU_has_region (PU& pu)		{ pu.flags |= PU_HAS_REGION; }
inline void
Clear_PU_has_region (PU& pu)		{ pu.flags &= ~PU_HAS_REGION; }

inline BOOL
PU_has_inlines (const PU& pu)		{ return pu.flags & PU_HAS_INLINES; }
inline void
Set_PU_has_inlines (PU& pu)		{ pu.flags |= PU_HAS_INLINES; }
inline void
Clear_PU_has_inlines (PU& pu)		{ pu.flags &= ~PU_HAS_INLINES; }

inline BOOL
PU_calls_setjmp (const PU& pu)		{ return pu.flags & PU_CALLS_SETJMP; }
inline void
Set_PU_calls_setjmp (PU& pu)		{ pu.flags |= PU_CALLS_SETJMP; }
inline void
Clear_PU_calls_setjmp (PU& pu)		{ pu.flags &= ~PU_CALLS_SETJMP; }

inline BOOL
PU_calls_longjmp (const PU& pu)		{ return pu.flags & PU_CALLS_LONGJMP; }
inline void
Set_PU_calls_longjmp (PU& pu)		{ pu.flags |= PU_CALLS_LONGJMP; }
inline void
Clear_PU_calls_longjmp (PU& pu)		{ pu.flags &= ~PU_CALLS_LONGJMP; }

inline BOOL
PU_ipa_addr_analysis (const PU& pu)     { return pu.flags & PU_IPA_ADDR_ANALYSIS; }
inline void
Set_PU_ipa_addr_analysis (PU& pu)	{ pu.flags |= PU_IPA_ADDR_ANALYSIS; }
inline void
Clear_PU_ipa_addr_analysis (PU& pu)	{ pu.flags &= ~PU_IPA_ADDR_ANALYSIS; }

inline BOOL
PU_smart_addr_analysis (const PU& pu)   { return pu.flags & PU_SMART_ADDR_ANALYSIS; }
inline void
Set_PU_smart_addr_analysis (PU& pu)	{ pu.flags |= PU_SMART_ADDR_ANALYSIS; }
inline void
Clear_PU_smart_addr_analysis (PU& pu)	{ pu.flags &= ~PU_SMART_ADDR_ANALYSIS; }

inline BOOL
PU_has_global_pragmas (const PU& pu)	{ return pu.flags & PU_HAS_GLOBAL_PRAGMAS; }
inline void
Set_PU_has_global_pragmas (PU& pu)	{ pu.flags |= PU_HAS_GLOBAL_PRAGMAS; }
inline void
Clear_PU_has_global_pragmas (PU& pu)	{ pu.flags &= ~PU_HAS_GLOBAL_PRAGMAS; }

inline BOOL
PU_has_user_alloca (const PU& pu)	{ return (pu.flags & PU_HAS_USER_ALLOCA) != 0; }
inline void
Set_PU_has_user_alloca (PU& pu)		{ pu.flags |= PU_HAS_USER_ALLOCA; }
inline void
Clear_PU_has_user_alloca (PU& pu)	{ pu.flags &= ~PU_HAS_USER_ALLOCA; }

inline BOOL
PU_has_unknown_control_flow (const PU& pu)	{ return (pu.flags & PU_HAS_UNKNOWN_CONTROL_FLOW) != 0; }
inline void
Set_PU_has_unknown_control_flow (PU& pu)	{ pu.flags |= PU_HAS_UNKNOWN_CONTROL_FLOW; }
inline void
Clear_PU_has_unknown_control_flow (PU& pu)	{ pu.flags &= ~PU_HAS_UNKNOWN_CONTROL_FLOW; }

inline BOOL
PU_has_syscall_linkage (const PU& pu)	{ return pu.flags & PU_HAS_SYSCALL_LINKAGE; }
inline void
Set_PU_has_syscall_linkage (PU& pu)	{ pu.flags |= PU_HAS_SYSCALL_LINKAGE; }
inline void
Clear_PU_has_syscall_linkage (PU& pu)	{ pu.flags &= ~PU_HAS_SYSCALL_LINKAGE; }

inline BOOL
PU_is_thunk (const PU& pu)		{ return (pu.flags & PU_IS_THUNK) != 0; }
inline void
Set_PU_is_thunk (PU& pu)		{ pu.flags |= PU_IS_THUNK; }
inline void
Clear_PU_is_thunk (PU& pu)		{ pu.flags &= ~PU_IS_THUNK; }

#ifdef KEY
inline BOOL
PU_needs_manual_unwinding (const PU& pu) { return (pu.flags & PU_NEEDS_MANUAL_UNWINDING) != 0;}
inline void
Set_PU_needs_manual_unwinding (PU& pu) { pu.flags |= PU_NEEDS_MANUAL_UNWINDING;}
inline void
Clear_PU_needs_manual_unwinding (PU& pu) { pu.flags &= ~PU_NEEDS_MANUAL_UNWINDING;}

inline BOOL
PU_is_extern_inline (const PU& pu)	{ return (pu.flags & PU_IS_EXTERN_INLINE) != 0; }
inline void
Set_PU_is_extern_inline (PU& pu) 	{ pu.flags |= PU_IS_EXTERN_INLINE; }
inline void
Clear_PU_is_extern_inline (PU& pu)	{ pu.flags &= ~PU_IS_EXTERN_INLINE; }

inline BOOL
PU_mp_lower_generated (const PU& pu)	{ return (pu.flags & PU_MP_LOWER_GENERATED) != 0; }
inline void
Set_PU_mp_lower_generated (PU& pu) 	{ pu.flags |= PU_MP_LOWER_GENERATED; }
inline void
Clear_PU_mp_lower_generated (PU& pu)	{ pu.flags &= ~PU_MP_LOWER_GENERATED; }

inline BOOL
PU_is_operator (const PU& pu)    { return (pu.flags & PU_IS_OPERATOR) != 0; }
inline void
Set_PU_is_operator (PU& pu)      { pu.flags |= PU_IS_OPERATOR; }
inline void
Clear_PU_is_operator (PU& pu)    { pu.flags &= ~PU_IS_OPERATOR; }

inline BOOL
PU_has_attr_malloc (const PU& pu)      { return (pu.flags & PU_HAS_ATTR_MALLOC) != 0; } 
inline void
Set_PU_has_attr_malloc (PU& pu)        { pu.flags |= PU_HAS_ATTR_MALLOC; }
inline void
Clear_PU_has_attr_malloc (PU& pu)      { pu.flags &= ~PU_HAS_ATTR_MALLOC; }

inline BOOL
PU_has_attr_pure (const PU& pu)      { return (pu.flags & PU_HAS_ATTR_PURE) != 0; } 
inline void
Set_PU_has_attr_pure (PU& pu)        { pu.flags |= PU_HAS_ATTR_PURE; }
inline void
Clear_PU_has_attr_pure (PU& pu)      { pu.flags &= ~PU_HAS_ATTR_PURE; }

inline BOOL
PU_has_attr_noreturn (PU& pu)        { return (pu.flags & PU_HAS_ATTR_NORETURN) != 0; } 
inline void
Set_PU_has_attr_noreturn (PU& pu)    { pu.flags |= PU_HAS_ATTR_NORETURN; }
inline void
Clear_PU_has_attr_noreturn (PU& pu)  { pu.flags &= ~PU_HAS_ATTR_NORETURN; }

inline BOOL
PU_is_marked_inline (const PU& pu)	{ return (pu.flags & PU_IS_MARKED_INLINE) != 0; }
inline void
Set_PU_is_marked_inline (PU& pu) 	{ pu.flags |= PU_IS_MARKED_INLINE; }
inline void
Clear_PU_is_marked_inline (PU& pu)	{ pu.flags &= ~PU_IS_MARKED_INLINE; }

inline BOOL
PU_no_instrument (const PU& pu)         { return (pu.flags & PU_NO_INSTRUMENT) != 0; }
inline void
Set_PU_no_instrument (PU& pu)           { pu.flags |= PU_NO_INSTRUMENT; }
inline void
Clear_PU_no_instrument (PU& pu)         { pu.flags &= ~PU_NO_INSTRUMENT; }

inline BOOL
PU_need_trampoline (const PU& pu)	{ return (pu.flags & PU_NEED_TRAMPOLINE) != 0; }
inline void
Set_PU_need_trampoline (PU& pu)		{ pu.flags |= PU_NEED_TRAMPOLINE; }

inline BOOL
PU_has_nonlocal_goto_label (const PU& pu) { return (pu.flags & PU_HAS_NONLOCAL_GOTO_LABEL) != 0; }
inline void
Set_PU_has_nonlocal_goto_label (PU& pu)	{ pu.flags |= PU_HAS_NONLOCAL_GOTO_LABEL; }

inline BOOL
PU_has_goto_outer_block (const PU& pu) { return (pu.flags & PU_HAS_GOTO_OUTER_BLOCK) != 0; }
inline void
Set_PU_has_goto_outer_block (PU& pu)	{ pu.flags |= PU_HAS_GOTO_OUTER_BLOCK; }
#endif

#ifdef TARG_X8664
inline BOOL
PU_ff2c_abi (const PU& pu)		{ return (pu.flags & PU_FF2C_ABI) != 0;}
inline void
Set_PU_ff2c_abi (PU& pu)		{ pu.flags |= PU_FF2C_ABI; }
inline void
Clear_PU_ff2c_abi (PU& pu)		{ pu.flags &= ~PU_FF2C_ABI; }
#endif

inline BOOL
PU_is_cdecl (const PU_IDX pui)		{ return (Pu_Table[pui].flags & PU_IS_CDECL) != 0; }
inline void
Set_PU_is_cdecl (PU_IDX pui)		{ Pu_Table[pui].flags |= PU_IS_CDECL; }
inline void
Clear_PU_is_cdecl (PU_IDX pui)		{ Pu_Table[pui].flags &= ~PU_IS_CDECL; }

inline BOOL
PU_nothrow (const PU& pu)		{ return (pu.flags & PU_NOTHROW) != 0;}
inline void
Set_PU_nothrow (PU& pu)			{ pu.flags |= PU_NOTHROW; }
inline void
Clear_PU_nothrow (PU& pu)		{ pu.flags &= ~PU_NOTHROW; }

inline BOOL
PU_has_apply_args (const PU& pu)        { return (pu.flags & PU_HAS_APPLY_ARGS) != 0;}
inline void
Set_PU_has_apply_args (PU& pu)          { pu.flags |= PU_HAS_APPLY_ARGS; }
inline void
Clear_PU_has_apply_args (PU& pu)        { pu.flags &= ~PU_HAS_APPLY_ARGS; }

inline UINT64
PU_src_lang (const PU& pu)		{ return pu.src_lang; }

inline BOOL
PU_simple_eh(const PU& pu)		{ return (pu.flags & PU_SIMPLE_EH_RANGE) != 0;}
inline void
Set_PU_simple_eh(PU& pu)			{ pu.flags |= PU_SIMPLE_EH_RANGE; }
inline void
Clear_PU_simple_eh(PU& pu)		{ pu.flags &= ~PU_SIMPLE_EH_RANGE; }

inline BOOL
PU_mixed_lang (const PU& pu)		{ return (pu.src_lang & PU_MIXED_LANG) != 0; }
inline void
Set_PU_mixed_lang (PU& pu)		{ pu.src_lang |= PU_MIXED_LANG; }
inline void
Clear_PU_mixed_lang (PU& pu)		{ pu.src_lang &= ~PU_MIXED_LANG; }

inline BOOL
PU_c_lang (const PU& pu)		{ return (pu.src_lang & PU_C_LANG) != 0; }
inline void
Set_PU_c_lang (PU& pu)			{ pu.src_lang |= PU_C_LANG; }
inline void
Clear_PU_c_lang (PU& pu)		{ pu.src_lang &= ~PU_C_LANG; }

inline BOOL
PU_cxx_lang (const PU& pu)		{ return (pu.src_lang & PU_CXX_LANG) != 0; }
inline void
Set_PU_cxx_lang (PU& pu)		{ pu.src_lang |= PU_CXX_LANG; }
inline void
Clear_PU_cxx_lang (PU& pu)		{ pu.src_lang &= ~PU_CXX_LANG; }

inline BOOL
PU_f77_lang (const PU& pu)		{ return (pu.src_lang & PU_F77_LANG) != 0; }
inline void
Set_PU_f77_lang (PU& pu)		{ pu.src_lang |= PU_F77_LANG; }
inline void
Clear_PU_f77_lang (PU& pu)		{ pu.src_lang &= ~PU_F77_LANG; }

inline BOOL
PU_f90_lang (const PU& pu)		{ return (pu.src_lang & PU_F90_LANG) != 0; }
inline void
Set_PU_f90_lang (PU& pu)		{ pu.src_lang |= PU_F90_LANG; }
inline void
Clear_PU_f90_lang (PU& pu)		{ pu.src_lang &= ~PU_F90_LANG; }

inline BOOL
PU_java_lang (const PU& pu)		{ return (pu.src_lang & PU_JAVA_LANG) != 0; }
inline void
Set_PU_java_lang (PU& pu)		{ pu.src_lang |= PU_JAVA_LANG; }
inline void
Clear_PU_java_lang (PU& pu)		{ pu.src_lang &= ~PU_JAVA_LANG; }

inline BOOL
PU_is_constructor (const PU& pu)        { return (pu.flags & PU_IS_CONSTRUCTOR) != 0; }
inline void
Set_PU_is_constructor (PU& pu)          { pu.flags |= PU_IS_CONSTRUCTOR; }
inline void
Clear_PU_is_constructor (PU &pu)        { pu.flags &= ~PU_IS_CONSTRUCTOR; }

// PU_ftn_lang (f77 or f90) is defined in symtab.h
// PU_has_nested (f77 or f90) is defined in symtab.h
//----------------------------------------------------------------------
// access functions for TY
//----------------------------------------------------------------------

inline UINT64
TY_size (const TY& ty)			    { return ty.size; }
inline void
Set_TY_size (TY& ty, UINT64 size)	{ ty.size = size; }
inline void
Set_TY_size (TY_IDX tyi, UINT64 size)       {
  Set_TY_size(Ty_Table[tyi], size);
}

inline TY_KIND
TY_kind (const TY& ty)			        { return ty.kind; }
inline void
Set_TY_kind (TY& ty, TY_KIND kind)	    { ty.kind = kind; }
inline void
Set_TY_kind (TY_IDX tyi, TY_KIND kind)	{ Set_TY_kind(Ty_Table[tyi], kind); }

inline TYPE_ID
TY_mtype (const TY& ty)			        { return ty.mtype; }
inline void
Set_TY_mtype (TY& ty, TYPE_ID mtype)	{ ty.mtype = mtype; }
inline void
Set_TY_mtype (TY_IDX tyi, TYPE_ID mtype){ Set_TY_mtype(Ty_Table[tyi],mtype); }

inline UINT16
TY_flags (const TY& ty)			        { return ty.flags; }
inline void
Set_TY_flags (TY& ty, mUINT16 flags)	{ ty.flags = flags; }
inline UINT16
TY_flags (const TY_IDX tyi)             { return TY_flags(Ty_Table[tyi]); }
inline void
Set_TY_flags (TY_IDX tyi, mUINT16 flags){ Set_TY_flags(Ty_Table[tyi],flags); }

inline FLD_HANDLE
TY_fld (const TY& ty)			{ return FLD_HANDLE (ty.Fld ()); }
inline void
Set_TY_fld (TY& ty, FLD_HANDLE fld)	{ ty.Set_fld (fld.Idx()); }
inline FLD_HANDLE
TY_fld (const TY_IDX tyi)               { return TY_fld(Ty_Table[tyi]); }
inline void
Set_TY_fld (TY_IDX tyi, FLD_HANDLE fld)	{ Set_TY_fld(Ty_Table[tyi], fld); }

inline TYLIST_IDX
TY_tylist (const TY& ty)		{ return ty.Tylist (); }
inline void
Set_TY_tylist (TY& ty, TYLIST_IDX idx) { ty.Set_tylist (idx); }
inline TYLIST_IDX
TY_tylist (const TY_IDX tyi)            { return TY_tylist(Ty_Table[tyi]); }
inline void
Set_TY_tylist (TY_IDX tyi, TYLIST_IDX idx) {
  Set_TY_tylist(Ty_Table[tyi],idx);
}

inline ARB_HANDLE
TY_arb (const TY& ty)			{ return ARB_HANDLE(ty.Arb ()); }
inline void
Set_TY_arb (TY& ty, ARB_HANDLE idx)	{ ty.Set_arb (idx.Idx()); }
inline ARB_HANDLE
TY_arb (const TY_IDX tyi)			{ return TY_arb(Ty_Table[tyi]); }
inline void
Set_TY_arb (TY_IDX tyi, ARB_HANDLE idx)	{ Set_TY_arb(Ty_Table[tyi], idx); }


inline STR_IDX
TY_name_idx (const TY& ty)		{ return ty.name_idx; }
inline void
Set_TY_name_idx (TY& ty, UINT64 name)	{ ty.name_idx = name; }
inline STR_IDX
TY_name_idx (const TY_IDX tyi)      { return TY_name_idx(Ty_Table[tyi]); }
inline void
Set_TY_name_idx (TY_IDX tyi, UINT64 name)   {
  Set_TY_name_idx(Ty_Table[tyi], name);
}
inline char *
TY_name (const TY& ty)			{ return &Str_Table[ty.name_idx]; }

inline TY_IDX
TY_etype (const TY& ty)			{ return ty.Etype (); }
inline void
Set_TY_etype (TY& ty, TY_IDX idx)	{ ty.Set_etype (idx); }
inline TY_IDX
TY_etype (const TY_IDX tyi)		{ return TY_etype(Ty_Table[tyi]); }
inline void
Set_TY_etype (TY_IDX tyi, TY_IDX idx)   { Set_TY_etype(Ty_Table[tyi],idx); }

inline TY_IDX
TY_pointed (const TY& ty)		{ return ty.Pointed (); }
inline void
Set_TY_pointed (TY& ty, TY_IDX idx)	{ ty.Set_pointed (idx); }
inline TY_IDX
TY_pointed (const TY_IDX tyi)		{ return Ty_Table[tyi].Pointed (); }
inline void
Set_TY_pointed (TY_IDX tyi, TY_IDX idx) { Set_TY_pointed(Ty_Table[tyi],idx); }

#ifdef KEY
inline ST_IDX
TY_copy_constructor (const TY& ty)	{ return ty.Copy_constructor (); }
inline void
Set_TY_copy_constructor (TY& ty, ST_IDX idx)	{ ty.Set_copy_constructor (idx); }
inline ST_IDX
TY_copy_constructor (const TY_IDX tyi)	{ return Ty_Table[tyi].Copy_constructor (); }
inline void
Set_TY_copy_constructor (TY_IDX tyi, ST_IDX idx) { Set_TY_copy_constructor(Ty_Table[tyi],idx); }
#endif
inline ST_IDX
TY_vtable (const TY& ty)	{ return ty.Vtable (); }
inline void
Set_TY_vtable (TY& ty, ST_IDX idx)	{ ty.Set_vtable (idx); }
inline ST_IDX
TY_vtable (const TY_IDX tyi)	{ return Ty_Table[tyi].Vtable (); }
inline void
Set_TY_vtable (TY_IDX tyi, ST_IDX idx) { Set_TY_vtable(Ty_Table[tyi],idx); }


//----------------------------------------------------------------------
// TY flags
//----------------------------------------------------------------------

inline BOOL
TY_is_character (const TY& ty)		{ return ty.flags & TY_IS_CHARACTER; }
inline void
Set_TY_is_character (TY& ty)		{ ty.flags |= TY_IS_CHARACTER; }
inline void
Clear_TY_is_character (TY& ty)		{ ty.flags &= ~TY_IS_CHARACTER; }
inline BOOL
TY_is_character (const TY_IDX tyi)	{ return TY_is_character(Ty_Table[tyi]); }
inline void
Set_TY_is_character (TY_IDX tyi)    { Set_TY_is_character(Ty_Table[tyi]); }
inline void
Clear_TY_is_character (TY_IDX tyi)  { Clear_TY_is_character(Ty_Table[tyi]); }

inline BOOL
TY_is_logical (const TY& ty)		{ return ty.flags & TY_IS_LOGICAL; }
inline void
Set_TY_is_logical (TY& ty)		{ ty.flags |= TY_IS_LOGICAL; }
inline void
Clear_TY_is_logical (TY& ty)		{ ty.flags &= ~TY_IS_LOGICAL; }
inline BOOL
TY_is_logical (const TY_IDX tyi)    { return TY_is_logical(Ty_Table[tyi]); }
inline void
Set_TY_is_logical (TY_IDX tyi)      { Set_TY_is_logical(Ty_Table[tyi]); }
inline void
Clear_TY_is_logical (TY_IDX tyi)    { Clear_TY_is_logical(Ty_Table[tyi]); }

inline BOOL
TY_is_union (const TY& ty)		{ return ty.flags & TY_IS_UNION; }
inline void
Set_TY_is_union (TY& ty)		{ ty.flags |= TY_IS_UNION; }
inline void
Clear_TY_is_union (TY& ty)		{ ty.flags &= ~TY_IS_UNION; }
inline BOOL
TY_is_union (const TY_IDX tyi)      { return TY_is_union(Ty_Table[tyi]); }
inline void
Set_TY_is_union (TY_IDX tyi)        { Set_TY_is_union(Ty_Table[tyi]); }
inline void
Clear_TY_is_union (TY_IDX tyi)      { Clear_TY_is_union(Ty_Table[tyi]); }

inline BOOL
TY_is_packed (const TY& ty)		{ return ty.flags & TY_IS_PACKED; }
inline void
Set_TY_is_packed (TY& ty)		{ ty.flags |= TY_IS_PACKED; }
inline void
Clear_TY_is_packed (TY& ty)		{ ty.flags &= ~TY_IS_PACKED; }
inline BOOL
TY_is_packed (const TY_IDX tyi)     { return TY_is_packed(Ty_Table[tyi]); }
inline void
Set_TY_is_packed (TY_IDX tyi)       { Set_TY_is_packed(Ty_Table[tyi]); }
inline void
Clear_TY_is_packed (TY_IDX tyi)     { Clear_TY_is_packed(Ty_Table[tyi]); }

inline BOOL
TY_ptr_as_array (const TY& ty)		{ return ty.flags & TY_PTR_AS_ARRAY; }
inline void
Set_TY_ptr_as_array (TY& ty)		{ ty.flags |= TY_PTR_AS_ARRAY; }
inline void
Clear_TY_ptr_as_array (TY& ty)		{ ty.flags &= ~TY_PTR_AS_ARRAY; }
inline BOOL
TY_ptr_as_array (const TY_IDX tyi)  { return TY_ptr_as_array(Ty_Table[tyi]); }
inline void
Set_TY_ptr_as_array (TY_IDX tyi)    { Set_TY_ptr_as_array(Ty_Table[tyi]); }
inline void
Clear_TY_ptr_as_array (TY_IDX tyi)  { Clear_TY_ptr_as_array(Ty_Table[tyi]); }

#ifdef TARG_NVISA
inline BOOL
TY_can_be_vector (const TY_IDX tyi)	{ return (TY_flags(Ty_Table[tyi]) & TY_CAN_BE_VECTOR); }
inline void
Set_TY_can_be_vector (TY_IDX tyi)	{ Set_TY_flags(Ty_Table[tyi], TY_flags(Ty_Table[tyi]) | TY_CAN_BE_VECTOR); }
#endif

inline BOOL
TY_anonymous (const TY& ty)		{ return ty.flags & TY_ANONYMOUS; }
inline void
Set_TY_anonymous (TY& ty)		{ ty.flags |= TY_ANONYMOUS; }
inline void
Clear_TY_anonymous (TY& ty)		{ ty.flags &= ~TY_ANONYMOUS; }
inline BOOL
TY_anonymous (const TY_IDX tyi)     { return TY_anonymous(Ty_Table[tyi]); }
inline void
Set_TY_anonymous (TY_IDX tyi)       { Set_TY_anonymous(Ty_Table[tyi]); }
inline void
Clear_TY_anonymous (TY_IDX tyi)     { Clear_TY_anonymous(Ty_Table[tyi]); }

inline BOOL
TY_split (const TY& ty)			{ return ty.flags & TY_SPLIT; }
inline void
Set_TY_split (TY& ty)			{ ty.flags |= TY_SPLIT; }
inline void
Clear_TY_split (TY& ty)			{ ty.flags &= ~TY_SPLIT; }
inline BOOL
TY_split (const TY_IDX tyi)         { return TY_split(Ty_Table[tyi]); }
inline void
Set_TY_split (TY_IDX tyi)           { Set_TY_split(Ty_Table[tyi]); }
inline void
Clear_TY_split (TY_IDX tyi)         { Clear_TY_split(Ty_Table[tyi]); }

inline BOOL
TY_is_f90_pointer (const TY& ty)	{ return ty.flags & TY_IS_F90_POINTER; }
inline void
Set_TY_is_f90_pointer (TY& ty)		{ ty.flags |= TY_IS_F90_POINTER; }
inline void
Clear_TY_is_f90_pointer (TY& ty)	{ ty.flags &= ~TY_IS_F90_POINTER; }
inline BOOL
TY_is_f90_pointer (const TY_IDX tyi){ return TY_is_f90_pointer(Ty_Table[tyi]);}
inline void
Set_TY_is_f90_pointer (TY_IDX tyi)  { Set_TY_is_f90_pointer(Ty_Table[tyi]); }
inline void
Clear_TY_is_f90_pointer (TY_IDX tyi){ Clear_TY_is_f90_pointer(Ty_Table[tyi]); }

inline BOOL
TY_not_in_union (const TY& ty)		{ return ty.flags & TY_NOT_IN_UNION; }
inline void
Set_TY_not_in_union (TY& ty)		{ ty.flags |= TY_NOT_IN_UNION; }
inline void
Clear_TY_not_in_union (TY& ty)		{ ty.flags &= ~TY_NOT_IN_UNION; }
inline BOOL
TY_not_in_union (const TY_IDX tyi)  { return TY_not_in_union(Ty_Table[tyi]); }
inline void
Set_TY_not_in_union (TY_IDX tyi)    { Set_TY_not_in_union(Ty_Table[tyi]); }
inline void
Clear_TY_not_in_union (TY_IDX tyi)  { Clear_TY_not_in_union(Ty_Table[tyi]); }

inline BOOL
TY_no_ansi_alias (const TY& ty)		{ return ty.flags & TY_NO_ANSI_ALIAS; }
inline void
Set_TY_no_ansi_alias (TY& ty)		{ ty.flags |= TY_NO_ANSI_ALIAS; }
inline void
Clear_TY_no_ansi_alias (TY& ty)		{ ty.flags &= ~TY_NO_ANSI_ALIAS; }
inline BOOL
TY_no_ansi_alias (const TY_IDX tyi) { return TY_no_ansi_alias(Ty_Table[tyi]); }
inline void
Set_TY_no_ansi_alias (TY_IDX tyi)   { Set_TY_no_ansi_alias(Ty_Table[tyi]); }
inline void
Clear_TY_no_ansi_alias (TY_IDX tyi) { Clear_TY_no_ansi_alias(Ty_Table[tyi]); }

inline BOOL
TY_is_non_pod (const TY& ty)		{ return ty.flags & TY_IS_NON_POD; }
inline void
Set_TY_is_non_pod (TY& ty)		{ ty.flags |= TY_IS_NON_POD; }
inline void
Clear_TY_is_non_pod (TY& ty)		{ ty.flags &= ~TY_IS_NON_POD; }
inline BOOL
TY_is_non_pod (const TY_IDX tyi)	{ return TY_is_non_pod(Ty_Table[tyi]); }
inline void
Set_TY_is_non_pod (TY_IDX tyi)    { Set_TY_is_non_pod(Ty_Table[tyi]); }
inline void
Clear_TY_is_non_pod (TY_IDX tyi)  { Clear_TY_is_non_pod(Ty_Table[tyi]); }

#ifdef KEY
inline BOOL
TY_return_in_mem (const TY& ty)		{ return ty.flags & TY_RETURN_IN_MEM; }
inline void
Set_TY_return_in_mem (TY& ty)		{ ty.flags |= TY_RETURN_IN_MEM; }
inline void
Clear_TY_return_in_mem (TY& ty)	{ ty.flags &= ~TY_RETURN_IN_MEM; }
inline BOOL
TY_return_in_mem (const TY_IDX tyi)    { return TY_return_in_mem(Ty_Table[tyi]); }
inline void
Set_TY_return_in_mem (TY_IDX tyi)      { Set_TY_return_in_mem(Ty_Table[tyi]); }
inline void
Clear_TY_return_in_mem (TY_IDX tyi)    { Clear_TY_return_in_mem(Ty_Table[tyi]); }
inline BOOL
TY_content_seen (const TY& ty)		{ return ty.flags & TY_CONTENT_SEEN; }
inline void
Set_TY_content_seen (TY& ty)		{ ty.flags |= TY_CONTENT_SEEN; }
inline void
Clear_TY_content_seen (TY& ty)	{ ty.flags &= ~TY_CONTENT_SEEN; }
inline BOOL
TY_content_seen (const TY_IDX tyi)    { return TY_content_seen(Ty_Table[tyi]); }
inline void
Set_TY_content_seen (TY_IDX tyi)      { Set_TY_content_seen(Ty_Table[tyi]); }
inline void
Clear_TY_content_seen (TY_IDX tyi)    { Clear_TY_content_seen(Ty_Table[tyi]); }

inline BOOL
TY_is_incomplete (const TY& ty)		{ return ty.flags & TY_IS_INCOMPLETE; }
inline void
Set_TY_is_incomplete (TY& ty)		{ ty.flags |= TY_IS_INCOMPLETE; }
inline void
Clear_TY_is_incomplete (TY& ty)		{ ty.flags &= ~TY_IS_INCOMPLETE; }
inline BOOL
TY_is_incomplete (const TY_IDX tyi)	{ return TY_is_incomplete(Ty_Table[tyi]); }
inline void
Set_TY_is_incomplete (TY_IDX tyi)    { Set_TY_is_incomplete(Ty_Table[tyi]); }
inline void
Clear_TY_is_incomplete (TY_IDX tyi)  { Clear_TY_is_incomplete(Ty_Table[tyi]); }

inline BOOL
TY_no_split (const TY& ty)		{ return ty.flags & TY_NO_SPLIT; }
inline void
Set_TY_no_split (TY& ty)		{ ty.flags |= TY_NO_SPLIT; }
inline void
Clear_TY_no_split (TY& ty)		{ ty.flags &= ~TY_NO_SPLIT; }
inline BOOL
TY_no_split (const TY_IDX tyi)	{ return TY_no_split(Ty_Table[tyi]); }
inline void
Set_TY_no_split (TY_IDX tyi)    { Set_TY_no_split(Ty_Table[tyi]); }
inline void
Clear_TY_no_split (TY_IDX tyi)  { Clear_TY_no_split(Ty_Table[tyi]); }

inline BOOL
TY_complete_struct_relayout_candidate(const TY& ty)
  { return ty.flags & TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE; }
inline void
Set_TY_complete_struct_relayout_candidate(TY& ty)
  { ty.flags |= TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE; }
inline void
Clear_TY_complete_struct_relayout_candidate(TY& ty)
  { ty.flags &= ~TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE; }
inline BOOL
TY_complete_struct_relayout_candidate(const TY_IDX tyi)
  { return TY_complete_struct_relayout_candidate(Ty_Table[tyi]); }
inline void
Set_TY_complete_struct_relayout_candidate(TY_IDX tyi)
  { Set_TY_complete_struct_relayout_candidate(Ty_Table[tyi]); }
inline void
Clear_TY_complete_struct_relayout_candidate(TY_IDX tyi)
  { Clear_TY_complete_struct_relayout_candidate(Ty_Table[tyi]); }
#endif

// TY pu_flags

inline BOOL
TY_return_to_param (const TY& ty)	{ return ty.Pu_flags () & TY_RETURN_TO_PARAM; }
inline void
Set_TY_return_to_param (TY& ty)		{ ty.Set_pu_flag (TY_RETURN_TO_PARAM); }
inline void
Clear_TY_return_to_param (TY& ty)	{ ty.Clear_pu_flag (TY_RETURN_TO_PARAM); }
inline BOOL
TY_return_to_param (const TY_IDX tyi)   {
  return TY_return_to_param(Ty_Table[tyi]);
}
inline void
Set_TY_return_to_param (TY_IDX tyi)  { Set_TY_return_to_param(Ty_Table[tyi]); }
inline void
Clear_TY_return_to_param (TY_IDX tyi){
  Clear_TY_return_to_param(Ty_Table[tyi]);
}

inline BOOL
TY_is_varargs (const TY& ty)		{ return ty.Pu_flags () & TY_IS_VARARGS; }
inline void
Set_TY_is_varargs (TY& ty)		{ ty.Set_pu_flag (TY_IS_VARARGS); }
inline void
Clear_TY_is_varargs (TY& ty)		{ ty.Clear_pu_flag (TY_IS_VARARGS); }
inline BOOL
TY_is_varargs (const TY_IDX tyi)    { return TY_is_varargs(Ty_Table[tyi]); }
inline void
Set_TY_is_varargs (TY_IDX tyi)      { Set_TY_is_varargs(Ty_Table[tyi]); }
inline void
Clear_TY_is_varargs (TY_IDX tyi)    { Clear_TY_is_varargs(Ty_Table[tyi]); }

inline BOOL
TY_has_prototype (const TY_IDX tyi) {
	return Ty_Table[tyi].Pu_flags() & TY_HAS_PROTOTYPE;
}
inline void
Set_TY_has_prototype (TY_IDX tyi) {
	Ty_Table[tyi].Set_pu_flag (TY_HAS_PROTOTYPE);
}
inline void
Clear_TY_has_prototype (TY_IDX tyi) {
	Ty_Table[tyi].Clear_pu_flag (TY_HAS_PROTOTYPE);
}

#ifdef TARG_X8664
inline BOOL
TY_has_sseregister_parm (const TY& ty) {
	return ty.Pu_flags () & TY_HAS_SSEREG_PARM;
}
inline void
Set_TY_has_sseregister_parm (TY& ty) {
	ty.Set_pu_flag (TY_HAS_SSEREG_PARM);
}
inline BOOL
TY_has_sseregister_parm (const TY_IDX tyi) {
	return TY_has_sseregister_parm(Ty_Table[tyi]);
}
inline void
Set_TY_has_sseregister_parm (TY_IDX tyi) {
	Set_TY_has_sseregister_parm(Ty_Table[tyi]);
}

inline INT
TY_register_parm (const TY& ty)
{
	if ((ty.Pu_flags() & TY_HAS_1_REG_PARM) == 0 &&
	    (ty.Pu_flags() & TY_HAS_2_REG_PARM) == 0)
		return 0;
	if ((ty.Pu_flags() & TY_HAS_1_REG_PARM) &&
	    (ty.Pu_flags() & TY_HAS_2_REG_PARM))
		return 3;
	if (ty.Pu_flags() & TY_HAS_1_REG_PARM)
		return 1;
	if (ty.Pu_flags() & TY_HAS_2_REG_PARM)
		return 2;
}

inline void
Set_TY_register_parm (TY& ty, INT num)
{
	if (num == 0) return;
	if (num == 1) ty.Set_pu_flag (TY_HAS_1_REG_PARM);
	else if (num == 2) ty.Set_pu_flag (TY_HAS_2_REG_PARM);
	else if (num == 3) ty.Set_pu_flag (TY_HAS_3_REG_PARM);
}

inline INT
TY_register_parm (const TY_IDX tyi) {
	return TY_register_parm (Ty_Table[tyi]);
}

inline void
Set_TY_register_parm (TY_IDX tyi, INT num) {
	Set_TY_register_parm (Ty_Table[tyi], num);
}

inline BOOL
TY_has_stdcall (const TY& ty) {
	return ty.Pu_flags () & TY_HAS_STDCALL;
}
inline void
Set_TY_has_stdcall (TY& ty) {
	ty.Set_pu_flag (TY_HAS_STDCALL);
}
inline BOOL
TY_has_stdcall (const TY_IDX tyi) {
	return TY_has_stdcall(Ty_Table[tyi]);
}
inline void
Set_TY_has_stdcall (TY_IDX tyi) {
	Set_TY_has_stdcall(Ty_Table[tyi]);
}

inline BOOL
TY_has_fastcall (const TY& ty) {
	return ty.Pu_flags () & TY_HAS_FASTCALL;
}
inline void
Set_TY_has_fastcall (TY& ty) {
	ty.Set_pu_flag (TY_HAS_FASTCALL);
	Set_TY_register_parm (ty, 2);  // fastcall uses ECX and EDX
}
inline BOOL
TY_has_fastcall (const TY_IDX tyi) {
	return TY_has_fastcall(Ty_Table[tyi]);
}
inline void
Set_TY_has_fastcall (TY_IDX tyi) {
	Set_TY_has_fastcall(Ty_Table[tyi]);
}

#endif

//----------------------------------------------------------------------
// access functions for FLD
//----------------------------------------------------------------------

inline STR_IDX
FLD_name_idx (FLD_HANDLE fld)		{ return fld.Entry()->name_idx; }
inline void
Set_FLD_name_idx (FLD_HANDLE fld, STR_IDX idx){ fld.Entry()->name_idx = idx; }
inline char *
FLD_name (FLD_HANDLE fld)		{ return &Str_Table[fld.Entry()->name_idx]; }

inline TY_IDX
FLD_type (FLD_HANDLE fld)		{ return fld.Entry()->type; }
inline void
Set_FLD_type (FLD_HANDLE fld, TY_IDX ty){ fld.Entry()->type = ty; }

inline UINT64
FLD_ofst (FLD_HANDLE fld)		{ return fld.Entry()->ofst; }
inline void
Set_FLD_ofst (FLD_HANDLE fld, UINT64 ofst) { fld.Entry()->ofst = ofst; }

inline UINT8
FLD_bsize (FLD_HANDLE fld)		{ return fld.Entry()->bsize; }
inline void
Set_FLD_bsize (FLD_HANDLE fld, UINT8 bsize) { fld.Entry()->bsize = bsize; }

inline UINT8
FLD_bofst (FLD_HANDLE fld)		{ return fld.Entry()->bofst; }
inline void
Set_FLD_bofst (FLD_HANDLE fld, UINT8 bofst) { fld.Entry()->bofst = bofst; }

inline UINT16
FLD_flags (FLD_HANDLE fld)		{ return fld.Entry()->flags; }
inline void
Set_FLD_flags (FLD_HANDLE fld, UINT16 flags) { fld.Entry()->flags = flags; }

inline ST_IDX
FLD_st (FLD_HANDLE fld)			{ return fld.Entry()->st; }
inline void
Set_FLD_st (FLD_HANDLE fld, ST_IDX st)	{ fld.Entry()->st = st; }

//----------------------------------------------------------------------
// FLD flags
//----------------------------------------------------------------------

inline BOOL
FLD_last_field (FLD_HANDLE fld)		{ return fld.Entry()->flags & FLD_LAST_FIELD; }
inline void
Set_FLD_last_field (FLD_HANDLE fld)	{ fld.Entry()->flags |= FLD_LAST_FIELD; }
inline void
Clear_FLD_last_field (FLD_HANDLE fld)	{ fld.Entry()->flags &= ~FLD_LAST_FIELD; }

inline BOOL
FLD_equivalence (FLD_HANDLE fld)	{ return fld.Entry()->flags & FLD_EQUIVALENCE; }
inline void
Set_FLD_equivalence (FLD_HANDLE fld)	{ fld.Entry()->flags |= FLD_EQUIVALENCE; }
inline void
Clear_FLD_equivalence (FLD_HANDLE fld)	{ fld.Entry()->flags &= ~FLD_EQUIVALENCE; }

inline BOOL
FLD_begin_union (FLD_HANDLE fld)	{ return fld.Entry()->flags & FLD_BEGIN_UNION; }
inline void
Set_FLD_begin_union (FLD_HANDLE fld)	{ fld.Entry()->flags |= FLD_BEGIN_UNION; }
inline void
Clear_FLD_begin_union (FLD_HANDLE fld)	{ fld.Entry()->flags &= ~FLD_BEGIN_UNION; }

inline BOOL
FLD_end_union (FLD_HANDLE fld)		{ return fld.Entry()->flags & FLD_END_UNION; }
inline void
Set_FLD_end_union (FLD_HANDLE fld)	{ fld.Entry()->flags |= FLD_END_UNION; }
inline void
Clear_FLD_end_union (FLD_HANDLE fld)	{ fld.Entry()->flags &= ~FLD_END_UNION; }

inline BOOL
FLD_begin_map (FLD_HANDLE fld)		{ return fld.Entry()->flags & FLD_BEGIN_MAP; }
inline void
Set_FLD_begin_map (FLD_HANDLE fld)	{ fld.Entry()->flags |= FLD_BEGIN_MAP; }
inline void
Clear_FLD_begin_map (FLD_HANDLE fld)	{ fld.Entry()->flags &= ~FLD_BEGIN_MAP; }

inline BOOL
FLD_end_map (FLD_HANDLE fld)		{ return fld.Entry()->flags & FLD_END_MAP; }
inline void
Set_FLD_end_map (FLD_HANDLE fld)	{ fld.Entry()->flags |= FLD_END_MAP; }
inline void
Clear_FLD_end_map (FLD_HANDLE fld)	{ fld.Entry()->flags &= ~FLD_END_MAP; }

inline BOOL
FLD_is_bit_field (FLD_HANDLE fld)	{ return fld.Entry()->flags & FLD_IS_BIT_FIELD; }
inline void
Set_FLD_is_bit_field (FLD_HANDLE fld)	{ fld.Entry()->flags |= FLD_IS_BIT_FIELD; }
inline void
Clear_FLD_is_bit_field (FLD_HANDLE fld)	{ fld.Entry()->flags &= ~FLD_IS_BIT_FIELD; }

inline BOOL
FLD_is_anonymous (FLD_HANDLE fld)       { return fld.Entry()->flags & FLD_IS_ANONYMOUS; }
inline void
Set_FLD_is_anonymous (FLD_HANDLE fld)   { fld.Entry()->flags |= FLD_IS_ANONYMOUS; }
inline void
Clear_FLD_is_anonymous (FLD_HANDLE fld) { fld.Entry()->flags &= ~FLD_IS_ANONYMOUS; }

inline BOOL
FLD_is_base_class (FLD_HANDLE fld)       { return fld.Entry()->flags & FLD_IS_BASE_CLASS; }
inline void
Set_FLD_is_base_class (FLD_HANDLE fld)   { fld.Entry()->flags |= FLD_IS_BASE_CLASS; }
inline void
Clear_FLD_is_base_class (FLD_HANDLE fld) { fld.Entry()->flags &= ~FLD_IS_BASE_CLASS; }

inline BOOL
FLD_is_virtual (FLD_HANDLE fld)       { return fld.Entry()->flags & FLD_IS_VIRTUAL; }
inline void
Set_FLD_is_virtual (FLD_HANDLE fld)   { fld.Entry()->flags |= FLD_IS_VIRTUAL; }
inline void
Clear_FLD_is_virtual (FLD_HANDLE fld) { fld.Entry()->flags &= ~FLD_IS_VIRTUAL; }

//----------------------------------------------------------------------
// access functions for TYLIST
//----------------------------------------------------------------------

inline TY_IDX
TYLIST_type (TYLIST tylist)		{ return tylist; }
inline void
Set_TYLIST_type (TYLIST& tylist, TY_IDX ty) { tylist = ty; }

// TYLIST_type doesn't do anything?
// define TYLIST_ty to access indexed value
inline TY_IDX
TYLIST_ty (TYLIST_IDX tli)		{ return Tylist_Table[tli]; }
inline void
Set_TYLIST_ty (TYLIST_IDX tli, TY_IDX ty) { Tylist_Table[tli] = ty; }

//----------------------------------------------------------------------
// access functions for ARB
//----------------------------------------------------------------------

inline UINT16
ARB_flags (const ARB_HANDLE arb)		{ return arb.Entry()->flags; }
inline void
Set_ARB_flags (ARB_HANDLE arb, UINT16 flags){ arb.Entry()->flags = flags; }

inline UINT16
ARB_dimension (const ARB_HANDLE arb)		{ return arb.Entry()->dimension; }
inline void
Set_ARB_dimension (ARB_HANDLE arb, UINT16 dim){ arb.Entry()->dimension = dim; }

inline INT64
ARB_lbnd_val (const ARB_HANDLE arb)		{ return arb.Entry()->Lbnd_val (); }
inline void
Set_ARB_lbnd_val (ARB_HANDLE arb, INT64 val)	{ arb.Entry()->Set_lbnd_val (val); }

inline ST_IDX
ARB_lbnd_var (const ARB_HANDLE arb)		{ return arb.Entry()->Lbnd_var (); }
inline void
Set_ARB_lbnd_var (ARB_HANDLE arb, ST_IDX var)	{ arb.Entry()->Set_lbnd_var (var); }

inline INT64
ARB_ubnd_val (const ARB_HANDLE arb)		{ return arb.Entry()->Ubnd_val (); }
inline void
Set_ARB_ubnd_val (ARB_HANDLE arb, INT64 val)	{ arb.Entry()->Set_ubnd_val (val); }

inline ST_IDX
ARB_ubnd_var (const ARB_HANDLE arb)		{ return arb.Entry()->Ubnd_var (); }
inline void
Set_ARB_ubnd_var (ARB_HANDLE arb, ST_IDX var)	{ arb.Entry()->Set_ubnd_var (var); }

inline INT64
ARB_stride_val (const ARB_HANDLE arb)		{ return arb.Entry()->Stride_val (); }
inline void
Set_ARB_stride_val (ARB_HANDLE arb, INT64 val){ arb.Entry()->Set_stride_val (val); }

inline ST_IDX
ARB_stride_var (const ARB_HANDLE arb)		{ return arb.Entry()->Stride_var (); }
inline void
Set_ARB_stride_var (ARB_HANDLE arb, ST_IDX var) { arb.Entry()->Set_stride_var (var); }

//----------------------------------------------------------------------
// ARB flags
//----------------------------------------------------------------------

inline BOOL
ARB_const_lbnd (const ARB_HANDLE arb)		{ return arb.Entry()->flags & ARB_CONST_LBND; }
inline void
Set_ARB_const_lbnd (ARB_HANDLE arb)		{ arb.Entry()->flags |= ARB_CONST_LBND; }
inline void
Clear_ARB_const_lbnd (ARB_HANDLE arb)		{ arb.Entry()->flags &= ~ARB_CONST_LBND; }

inline BOOL
ARB_const_ubnd (const ARB_HANDLE arb)		{ return arb.Entry()->flags & ARB_CONST_UBND; }
inline void
Set_ARB_const_ubnd (ARB_HANDLE arb)		{ arb.Entry()->flags |= ARB_CONST_UBND; }
inline void
Clear_ARB_const_ubnd (ARB_HANDLE arb)		{ arb.Entry()->flags &= ~ARB_CONST_UBND; }

inline BOOL
ARB_const_stride (const ARB_HANDLE arb)	{ return arb.Entry()->flags & ARB_CONST_STRIDE; }
inline void
Set_ARB_const_stride (ARB_HANDLE arb)		{ arb.Entry()->flags |= ARB_CONST_STRIDE; }
inline void
Clear_ARB_const_stride (ARB_HANDLE arb)	{ arb.Entry()->flags &= ~ARB_CONST_STRIDE; }

inline BOOL
ARB_first_dimen (const ARB_HANDLE arb)	{ return arb.Entry()->flags & ARB_FIRST_DIMEN; }
inline void
Set_ARB_first_dimen (ARB_HANDLE arb)		{ arb.Entry()->flags |= ARB_FIRST_DIMEN; }
inline void
Clear_ARB_first_dimen (ARB_HANDLE arb)	{ arb.Entry()->flags &= ~ARB_FIRST_DIMEN; }

inline BOOL
ARB_last_dimen (const ARB_HANDLE arb)		{ return arb.Entry()->flags & ARB_LAST_DIMEN; }
inline void
Set_ARB_last_dimen (ARB_HANDLE arb)		{ arb.Entry()->flags |= ARB_LAST_DIMEN; }
inline void
Clear_ARB_last_dimen (ARB_HANDLE arb)		{ arb.Entry()->flags &= ~ARB_LAST_DIMEN; }


//----------------------------------------------------------------------
// access functions for LABEL
//----------------------------------------------------------------------

inline STR_IDX
LABEL_name_idx (const LABEL& lbl)	{ return lbl.name_idx; }
inline void
Set_LABEL_name_idx (LABEL& lbl, STR_IDX s) { lbl.name_idx = s; }
inline char *
LABEL_name (const LABEL& lbl)		{ return &Str_Table[lbl.name_idx]; }

inline LABEL_KIND
LABEL_kind (const LABEL& lbl)		{ return lbl.kind; }
inline void
Set_LABEL_KIND (LABEL& lbl, LABEL_KIND k) { lbl.kind = k; }

inline BOOL
LABEL_target_of_goto_outer_block (const LABEL& l)
{ return l.flags & LABEL_TARGET_OF_GOTO_OUTER_BLOCK;}
inline void
Set_LABEL_target_of_goto_outer_block (LABEL& l)
{ l.flags |= LABEL_TARGET_OF_GOTO_OUTER_BLOCK; }
inline void
Clear_LABEL_target_of_goto_outer_block (LABEL& l)
{ l.flags &= ~LABEL_TARGET_OF_GOTO_OUTER_BLOCK; }

inline BOOL
LABEL_addr_saved (const LABEL& l)	{ return l.flags & LABEL_ADDR_SAVED;}
inline void
Set_LABEL_addr_saved (LABEL& l)		{ l.flags |= LABEL_ADDR_SAVED; }
inline void
Clear_LABEL_addr_saved (LABEL& l)	{ l.flags &= ~LABEL_ADDR_SAVED; }

inline BOOL
LABEL_addr_passed (const LABEL& l)	{ return l.flags & LABEL_ADDR_PASSED;}
inline void
Set_LABEL_addr_passed (LABEL& l)	{ l.flags |= LABEL_ADDR_PASSED; }
inline void
Clear_LABEL_addr_passed (LABEL& l)	{ l.flags &= ~LABEL_ADDR_PASSED; }


//----------------------------------------------------------------------
// access functions for PREG
//----------------------------------------------------------------------


inline STR_IDX
PREG_name_idx (const PREG& preg)	{ return preg.name_idx; }
inline void
Set_PREG_name_idx (PREG& preg, STR_IDX s) { preg.name_idx = s; }
inline void
Set_PREG_name (PREG &preg, const char *const name)
  { Set_PREG_name_idx(preg, Save_Str(name)); }
inline char *
PREG_name (const PREG& preg)		{ return &Str_Table[preg.name_idx]; }


//----------------------------------------------------------------------
// access functions for ST_ATTR
//----------------------------------------------------------------------


inline ST_IDX
ST_ATTR_st_idx (const ST_ATTR& st_attr)		{ return st_attr.st_idx; }
inline void
Set_ST_ATTR_st_idx (ST_ATTR& st_attr, ST_IDX st) { st_attr.st_idx = st; }
inline ST_ATTR_KIND
ST_ATTR_kind (const ST_ATTR& st_attr)		{ return st_attr.kind; }
inline PREG_NUM
ST_ATTR_reg_id (const ST_ATTR& st_attr)
{
    Is_True (st_attr.kind == ST_ATTR_DEDICATED_REGISTER,
	     ("attribute is not for a dedicated register"));
    return st_attr.Get_reg_id();
}
inline void
Set_ST_ATTR_reg_id (ST_ATTR& st_attr, PREG_NUM id)
{
    st_attr.kind = ST_ATTR_DEDICATED_REGISTER;
    st_attr.Set_reg_id (id);
}
inline STR_IDX
ST_ATTR_section_name (const ST_ATTR& st_attr)
{
    Is_True (st_attr.kind == ST_ATTR_SECTION_NAME,
	     ("attribute is not for a section name"));
    return st_attr.Get_section_name ();
}
inline void
Set_ST_ATTR_section_name (ST_ATTR& st_attr, STR_IDX name)
{
    st_attr.kind = ST_ATTR_SECTION_NAME;
    st_attr.Set_section_name (name);
}

//----------------------------------------------------------------------
// access functions for FILE_INFO
//----------------------------------------------------------------------

inline UINT8
FILE_INFO_gp_group (const FILE_INFO& f)	{ return f.gp_group; }
inline void
Set_FILE_INFO_gp_group (FILE_INFO& f, mUINT8 grp) { f.gp_group = grp; }

inline BOOL
FILE_INFO_ipa (const FILE_INFO& f)	{ return f.flags & FI_IPA; }
inline void
Set_FILE_INFO_ipa (FILE_INFO& f)	{ f.flags |= FI_IPA; }
inline void
Clear_FILE_INFO_ipa (FILE_INFO& f)	{ f.flags &= ~FI_IPA; }

inline BOOL
FILE_INFO_needs_lno (const FILE_INFO& f){ return f.flags & FI_NEEDS_LNO; }
inline void
Set_FILE_INFO_needs_lno (FILE_INFO& f)	{ f.flags |= FI_NEEDS_LNO; }
inline void
Clear_FILE_INFO_needs_lno (FILE_INFO& f){ f.flags &= ~FI_NEEDS_LNO; }

inline BOOL
FILE_INFO_has_inlines (const FILE_INFO& f) { return f.flags & FI_HAS_INLINES; }
inline void
Set_FILE_INFO_has_inlines (FILE_INFO& f) { f.flags |= FI_HAS_INLINES; }
inline void
Clear_FILE_INFO_has_inlines (FILE_INFO& f) { f.flags &= ~FI_HAS_INLINES; }

inline BOOL
FILE_INFO_has_mp (const FILE_INFO& f){ return f.flags & FI_HAS_MP; }
inline void
Set_FILE_INFO_has_mp (FILE_INFO& f)	{ f.flags |= FI_HAS_MP; }
inline void
Clear_FILE_INFO_has_mp (FILE_INFO& f){ f.flags &= ~FI_HAS_MP; }

inline BOOL
FILE_INFO_has_global_asm (const FILE_INFO& f)  { return f.flags & FI_HAS_GLOBAL_ASM; }
inline void
Set_FILE_INFO_has_global_asm (FILE_INFO& f)    { f.flags |= FI_HAS_GLOBAL_ASM; }
inline void
Clear_FILE_INFO_has_global_asm (FILE_INFO& f)  { f.flags &= ~FI_HAS_GLOBAL_ASM; }


//----------------------------------------------------------------------
// access functions for the TABLES
//----------------------------------------------------------------------


inline PREG&
PREG_TABLE::operator[] (PREG_IDX idx) {
    return Scope_tab[CURRENT_SYMTAB].preg_tab->Entry (idx);
}

inline PREG&
PREG_TABLE::operator() (SYMTAB_IDX level, PREG_IDX idx) {
    return Scope_tab[level].preg_tab->Entry (idx);
}

inline ST_ATTR&
ST_ATTR_TABLE::operator[] (ST_ATTR_IDX idx) {
    return Scope_tab[CURRENT_SYMTAB].st_attr_tab->Entry (idx);
}

inline ST_ATTR&
ST_ATTR_TABLE::operator() (SYMTAB_IDX level, ST_ATTR_IDX idx) {
    return Scope_tab[level].st_attr_tab->Entry (idx);
}

inline TY&
TYPE_TABLE::operator[] (TY_IDX idx) {
    return Ty_tab[TY_IDX_index(idx)];
}

inline TY_TAB*
TYPE_TABLE::operator& () {
    return &Ty_tab;
}

#endif // symtab_access_INCLUDED
