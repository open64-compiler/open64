/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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

#ifndef symtab_INCLUDED
#define symtab_INCLUDED

// This file should contain only inlined functions for accessing various
// fields in the symbol table classes.

#include <iosfwd>

#ifndef segmented_array_INCLUDED
#include "segmented_array.h"
#endif

#ifndef cmplr_segmented_array_INCLUDED
#include "cmplr_segmented_array.h"
#endif

#ifndef symtab_idx_INCLUDED
#include "symtab_idx.h"
#endif

#ifndef mtypes_INCLUDED
#include "mtypes.h"
#endif

#ifndef targ_const_INCLUDED
#include "targ_const.h"
#endif

#ifndef strtab_INCLUDED
#include "strtab.h"
#endif

#ifndef irbdata_defs_INCLUDED
#include "irbdata_defs.h"
#endif

#include "symtab_defs.h"		// definitions of each structure
#include "symtab_verify.h"		// verifier
#include "symtab_access.h"		// standard access functions
#include "symtab_utils.h"		// out-of-line utilities
#include "symtab_compatible.h"		// compatibility with old symtab

// misc. inline functions that are too simple to go into symtab_utils.h

//----------------------------------------------------------------------
// ST
//----------------------------------------------------------------------

inline ST*
New_ST (SYMTAB_IDX level)
{
    UINT idx;
    ST& s = Scope_tab[level].st_tab->New_entry (idx);
    // Clear the random padding bits in ST.
    // Otherwise, these random paddings may make bcmp/memcmp mis-compare.
    memset(&s, 0, sizeof(ST));  // bug 14141
    Set_ST_st_idx (s, make_ST_IDX (idx, level));
    return &s;
}

inline ST *
New_ST () { return New_ST (CURRENT_SYMTAB); }


inline void
ST_Init (ST* st, STR_IDX n, ST_CLASS sc, ST_SCLASS stc, ST_EXPORT exp,
	 TY_IDX t)
{
    st->u1.name_idx = n;
    st->sym_class = sc;
    st->storage_class = stc;
    st->export_class = exp;
    st->u2.type = t;
    st->base_idx = st->st_idx;
    st->offset = 0;
    st->flags = 0;
    st->flags_ext = 0;
#ifdef KEY
    // bug 14141
    st->pad = 0;
#endif
    st->vtable_ty_idx = 0;
}

inline void
Set_ST_classes (ST* s, ST_CLASS c, ST_SCLASS sc)
{
#ifdef Is_True_On
    ST_Verify_Class_Sclass (c, sc);
#endif
    s->sym_class = c;
    s->storage_class = sc;
}


inline UINT32
ST_Table_Size (SYMTAB_IDX level) {
    return Scope_tab[level].st_tab->Size ();
}

// overload these access functions for convenience (but slower)
inline char *
ST_name (ST_IDX idx)			{ return ST_name (St_Table[idx]); }

inline ST_CLASS				// for compatibility
ST_class (const ST_IDX s)		{ return ST_class (St_Table[s]); }

inline ST_SCLASS			// for compatibility
ST_sclass (const ST_IDX s)		{ return ST_sclass(St_Table[s]); }

inline TY_IDX
ST_type (const ST_IDX s)		{ return ST_type(St_Table[s]); }

inline TYPE_ID
ST_mtype (const ST* s) {
    return TY_mtype (Ty_Table[ST_type (s)]);
}
inline TYPE_ID
ST_btype (const ST* s) 			{ return ST_mtype(s); }

inline void
Set_ST_base (ST& s, ST& base)		{ s.base_idx = ST_st_idx(base); }

inline ST&
ST_base (const ST& s)			{ return St_Table[ST_base_idx (s)]; }

inline ST&
ST_strong (const ST& s) {
    FmtAssert (ST_is_weak_symbol (s), ("Expecting a weak symbol"));
    return St_Table[s.base_idx];
}

inline ST&
ST_full (const ST& s) {
    FmtAssert (ST_is_split_common (s), ("Expecting a split common"));
    return St_Table[s.base_idx];
}

inline TCON&
ST_tcon_val (const ST* s)		{ return Tcon_Table[ST_tcon (s)]; }

// optional symbols are also preemptible
inline BOOL
ST_is_preemptible (const ST* s) {
    return (ST_export (s) == EXPORT_PREEMPTIBLE ||
	    ST_export (s) == EXPORT_OPTIONAL);
}

inline BOOL
ST_is_export_local (const ST *s)
{
	// can be local or local_internal
	return (ST_export(s) == EXPORT_LOCAL ||
		ST_export(s) == EXPORT_LOCAL_INTERNAL);
}

// bug fix for OSP_155
inline BOOL
ST_is_export_hidden (const ST *s)
{
	return (ST_export(s) == EXPORT_HIDDEN);
}

inline TY_IDX
ST_pu_type (const ST* s)
{
  Is_True (s->sym_class == CLASS_FUNC ||
	   (ST_sym_class (s) == CLASS_NAME &&
	    (ST_asm_function_st (*s))),
	   ("Invalid argument for ST_pu_type"));
  return PU_prototype (Pu_Table[ST_pu (s)]);
}

inline SYMTAB_IDX
ST_level (const ST* s)			{ return ST_IDX_level(ST_st_idx(s)); }
inline UINT32
ST_index (const ST* s)			{ return ST_IDX_index(ST_st_idx(s)); }

inline BOOL
Has_Base_Block (const ST* s) {
    return ST_base_idx (s) != ST_st_idx (s);
}

inline BOOL
Has_Strong_Symbol (const ST* s) {
	return ST_is_weak_symbol(s) && (ST_sclass(s) == SCLASS_EXTERN) 
		&& (ST_strong(s) != s);
}

//----------------------------------------------------------------------
// INITO
//----------------------------------------------------------------------
inline UINT
INITO_Table_Size (SYMTAB_IDX level) {
    return Scope_tab[level].inito_tab->Size();
}

//----------------------------------------------------------------------
// INITV
//----------------------------------------------------------------------
inline UINT
INITV_Table_Size () { return Initv_Table.Size();}


//----------------------------------------------------------------------
// PU
//----------------------------------------------------------------------

inline PU&
New_PU (PU_IDX& pu)			{ return Pu_Table.New_entry (pu); }

inline void
PU_Init (PU& pu, TY_IDX prototype, SYMTAB_IDX level)
{
    pu.target_idx = TARGET_INFO_IDX_ZERO;
    pu.prototype = prototype;
    Is_True (level > GLOBAL_SYMTAB, ("lexical level of a PU must be > 1"));
    pu.base_class = TY_IDX_ZERO;
    pu.lexical_level = level;
#ifdef TARG_NVISA
    pu.thread_limit = 0;
    pu.block_limit  = 0;
#endif
    pu.gp_group = 0;
    pu.src_lang = PU_UNKNOWN_LANG;
    pu.misc = 0;
    pu.unused = 0;
    pu.flags = 0;
}

inline UINT
PU_Table_Size ()			{ return Pu_Table.Size(); }

inline BOOL
PU_has_nested (const PU& pu)		{ return (PU_uplevel(pu) ||
						  PU_has_mp(pu)); }

inline BOOL
PU_ftn_lang (const PU& pu)		{ return (PU_f90_lang(pu) ||
                                                  PU_f77_lang(pu)); }

inline SYMTAB_IDX
PU_lexical_level (const ST* st)
{
    Is_True (ST_sym_class (st) == CLASS_FUNC ||
	     (ST_sym_class (st) == CLASS_NAME &&
	      (ST_asm_function_st (*st))),
	     ("Symbol is not a function"));
    return PU_lexical_level (Pu_Table[ST_pu (st)]);
}

inline ST *
Get_Current_PU_ST ()
{
    return Scope_tab[CURRENT_SYMTAB].st;
}

inline PU &
Get_Current_PU ()
{
#ifdef FRONT_END
    return Pu_Table[ST_pu (Scope_tab[CURRENT_SYMTAB].st)];
#else
    return *Current_pu;
#endif
}

inline TY_IDX
Get_Current_PU_TY ()
{
    return PU_prototype( Get_Current_PU() );
}

// get PU at specified level
inline PU &
Get_Scope_PU (UINT level)
{
    return Pu_Table[ST_pu (Scope_tab[level].st)];
}

//----------------------------------------------------------------------
// TY
//----------------------------------------------------------------------

inline TY&
New_TY (TY_IDX& ty_idx)
{
    UINT idx;
    TY& ty = Ty_tab.New_entry (idx);
    ty_idx = make_TY_IDX (idx);
    return ty;
}

inline void
TY_Init (TY& ty, UINT64 s, TY_KIND k, TYPE_ID t, STR_IDX n)
{
#ifdef Is_True_On
    if (k == KIND_FUNCTION) 
      TY_Verify_Kind_Function(k,s,t);

    TY_Verify_Kind_Mtype (k, t);
#endif

    ty.size = s;
    ty.kind = k;
    ty.mtype = t;
    ty.flags = 0;
    ty.u1.fld = 0;
    ty.name_idx = n;
    ty.u2.etype = 0;
    ty.vtable = 0;
} // TY_Init

inline UINT32
TY_Table_Size ()			{ return Ty_tab.Size (); }

inline UINT64
TY_size (TY_IDX ty_idx)			{ return TY_size (Ty_Table[ty_idx]); }

inline TY_KIND
TY_kind (TY_IDX ty_idx)			{ return TY_kind (Ty_Table[ty_idx]); }

inline TYPE_ID
TY_mtype (TY_IDX ty_idx)		{ return Ty_Table[ty_idx].mtype; }

inline char *
TY_name (TY_IDX ty_idx)			{ return TY_name (Ty_Table[ty_idx]); }
 
inline TY_IDX
TY_ret_type (const TY& ty) {
    Is_True (TY_kind (ty) == KIND_FUNCTION, ("TY_kind is not KIND_FUNCTION"));
    return Tylist_Table[TY_tylist (ty)];
}
inline TY_IDX
TY_ret_type (const TY_IDX ty_idx)	{ return TY_ret_type(Ty_Table[ty_idx]);}

inline TYLIST_IDX
TY_parms (const TY& ty) {
    Is_True (TY_kind (ty) == KIND_FUNCTION, ("TY_kind is not KIND_FUNCTION"));
    return TY_tylist (ty) + 1;
}
inline TYLIST_IDX
TY_parms (const TY_IDX ty_idx)		{ return TY_parms(Ty_Table[ty_idx]); }

inline BOOL
Is_Simple_Type (const TY& ty)
{
    switch (TY_kind (ty)) {
    case KIND_SCALAR:
    case KIND_POINTER:
    case KIND_VOID:
	return TRUE;
    default:
	return FALSE;
    }
}
inline BOOL
Is_Simple_Type (TY_IDX ty)	{ return Is_Simple_Type (Ty_Table[ty]); }


inline BOOL
Is_Structure_Type (const TY& ty)
{
    return TY_kind (ty) == KIND_STRUCT;
}
inline BOOL
Is_Structure_Type (TY_IDX ty)	{ return Is_Structure_Type (Ty_Table[ty]); }


inline BOOL
Is_Composite_Type (const TY& ty)
{
    switch (TY_kind (ty)) {
    case KIND_STRUCT:
    case KIND_ARRAY:
	return TRUE;
    default:
	return FALSE;
    }
}
inline BOOL
Is_Composite_Type (TY_IDX ty)	{ return Is_Composite_Type (Ty_Table[ty]); }

//----------------------------------------------------------------------
// FLD
//----------------------------------------------------------------------
inline FLD_HANDLE
New_FLD ()
{
    FLD_IDX fld_idx;
    FLD& fld = Fld_Table.New_entry (fld_idx);
    return FLD_HANDLE (&fld, fld_idx);
}

inline void
FLD_Init (FLD_HANDLE f, STR_IDX _name, TY_IDX _type, UINT64 _ofst)
{
    FLD* fld = f.Entry ();
    fld->name_idx = _name;
    fld->type = _type;
    fld->ofst = _ofst;
    fld->flags = 0;
    fld->bsize = fld->bofst = 0;
    fld->st = 0;
} // FLD::Init

inline UINT
FLD_Table_Size ()			{ return Fld_Table.Size(); }

//----------------------------------------------------------------------
// ARB
//----------------------------------------------------------------------

inline ARB_HANDLE
New_ARB () 
{
   ARB_IDX arb_idx;
   ARB& arb = Arb_Table.New_entry(arb_idx);
   return ARB_HANDLE(&arb,arb_idx);
}

inline void
ARB_Init (ARB_HANDLE arb_h, INT64 lbnd, INT64 ubnd, INT64 stride)
{
   ARB * arb = arb_h.Entry();
   arb->flags = ARB_CONST_LBND | ARB_CONST_UBND | ARB_CONST_STRIDE;
   arb->dimension = 1;
   arb->unused = 0;
   arb->u1.lbnd_val = lbnd;
   arb->u2.ubnd_val = ubnd;
   arb->u3.stride_val = stride;
}
      
inline UINT
ARB_Table_Size ()			{ return Arb_Table.Size(); }

//----------------------------------------------------------------------
// TYLIST
//----------------------------------------------------------------------
inline TYLIST&
New_TYLIST (TYLIST_IDX& tylist) { return Tylist_Table.New_entry (tylist); }

inline UINT32
TYLIST_Table_Size ()			{ return Tylist_Table.Size (); }

//----------------------------------------------------------------------
// access functions for LABEL
//----------------------------------------------------------------------
inline LABEL&
New_LABEL (SYMTAB_IDX scope, LABEL_IDX& label_idx)
{
    LABEL& label = Scope_tab[scope].label_tab->New_entry (label_idx);
    Set_LABEL_name_idx (label, 0);
    Set_LABEL_KIND (label, LKIND_DEFAULT);
    label.flags = 0;
    label_idx = make_LABEL_IDX(label_idx, scope);
    return label;
}

inline void
LABEL_Init (LABEL& label, STR_IDX name_idx, LABEL_KIND kind)
{
    label.name_idx = name_idx;
    label.kind = kind;
    label.flags = 0;
}

inline UINT32
LABEL_Table_Size (SYMTAB_IDX level)
{
    return Scope_tab[level].label_tab->Size ();
}


inline char *
LABEL_name (LABEL_IDX idx)		{ return LABEL_name(Label_Table[idx]);}

//----------------------------------------------------------------------
// PREG
//----------------------------------------------------------------------
inline PREG&
New_PREG_explicit (SCOPE *scope_tab, SYMTAB_IDX scope, PREG_IDX& p)
{
    return scope_tab[scope].preg_tab->New_entry (p);
}

inline PREG&
New_PREG (SYMTAB_IDX scope, PREG_IDX& p)
{
    return New_PREG_explicit(Scope_tab, scope, p);
}

inline UINT32
PREG_Table_Size_explicit (SCOPE *scope_tab, SYMTAB_IDX level) {
    return scope_tab[level].preg_tab->Size();
}

inline UINT32
PREG_Table_Size (SYMTAB_IDX level) {
    return PREG_Table_Size_explicit(Scope_tab, level);
}

inline void
Reset_PREG_Table_Size_explicit (SCOPE *scope_tab, SYMTAB_IDX level, UINT32 size)
{
    INT32 diff = PREG_Table_Size_explicit (scope_tab, level) - size;
    Is_True (diff >= 0, ("Deleting too many entries"));
    scope_tab[level].preg_tab->Delete_last (diff);
}

inline void
Reset_PREG_Table_Size (SYMTAB_IDX level, UINT32 size)
{
    Reset_PREG_Table_Size_explicit(Scope_tab, level, size);
}

//----------------------------------------------------------------------
// ST_ATTR
//----------------------------------------------------------------------
inline ST_ATTR&
New_ST_ATTR_explicit (SCOPE *scope_tab, SYMTAB_IDX scope, ST_ATTR_IDX& p)
{
    return scope_tab[scope].st_attr_tab->New_entry (p);
}

inline ST_ATTR&
New_ST_ATTR (SYMTAB_IDX scope, ST_ATTR_IDX& p)
{
    return New_ST_ATTR_explicit(Scope_tab, scope, p);
}

inline void
ST_ATTR_Init (ST_ATTR& st_attr, ST_IDX st_idx, ST_ATTR_KIND akind, UINT64 val)
{
    st_attr.st_idx = st_idx;
    st_attr.kind = akind;
    switch (akind) {
    case ST_ATTR_DEDICATED_REGISTER:
      st_attr.Set_reg_id(val);
      break;
    case ST_ATTR_SECTION_NAME:
      st_attr.Set_section_name(val);
      break;
    default:
      Is_True (FALSE, ("Unknown kind of ST_ATTR_Init "));
    }
}

inline UINT32
ST_ATTR_Table_Size_explicit (SCOPE *scope_tab, SYMTAB_IDX level) {
    return scope_tab[level].st_attr_tab->Size();
}

inline UINT32
ST_ATTR_Table_Size (SYMTAB_IDX level) {
    return ST_ATTR_Table_Size_explicit(Scope_tab, level);
}

inline void
Reset_ST_ATTR_Table_Size_explicit (SCOPE *scope_tab, SYMTAB_IDX level, UINT32 size)
{
    INT32 diff = ST_ATTR_Table_Size_explicit (scope_tab, level) - size;
    Is_True (diff >= 0, ("Deleting too many entries"));
    scope_tab[level].st_attr_tab->Delete_last (diff);
}

inline void
Reset_ST_ATTR_Table_Size (SYMTAB_IDX level, UINT32 size)
{
    Reset_ST_ATTR_Table_Size_explicit(Scope_tab, level, size);
}

//----------------------------------------------------------------------
// TCON
//----------------------------------------------------------------------
const UINT32 MAX_PREDEFINED_TCON_IDX = 2;

TCON_IDX
Enter_tcon (const TCON& tcon);

void Init_Constab (void);

inline UINT32
TCON_Table_Size ()			{ return Tcon_Table.Size (); }

//----------------------------------------------------------------------
// SCOPE
//----------------------------------------------------------------------

// symbol in global symbol table?
inline BOOL
Is_Global_Symbol (const ST* s)	{ return ST_level(s) <= GLOBAL_SYMTAB; }
// local means is in current symtab, does not include uplevel non-globals
inline BOOL
Is_Local_Symbol (const ST* s)	{ return ST_level(s) == CURRENT_SYMTAB; }


// access functions for BLK
// currently, we always get this info via the ST

inline BLK&
New_BLK (BLK_IDX& blk_idx)
{
    BLK& blk = Blk_Table.New_entry(blk_idx);
    blk.Init ();
    return blk;
}


inline UINT64
STB_size (const ST *s) {
    return Blk_Table[ST_blk(s)].Size();
}
inline void
Set_STB_size (ST *s, UINT64 size) {
    Blk_Table[ST_blk(s)].Set_size(size);
}
inline UINT
STB_align (const ST *s) {
  UINT16 align = Blk_Table[ST_blk(s)].Align();
  /* Unpack align into a real value. Must be within 5 bits.
   * If all 16 bits are 1 then interpret the alignment as zero.
   * This is done because currently data-layout often uses an
   * alignment of 0, but there is no way to represent 0-alignment
   * with packed alignment. So we simply treat 0xffff as 0.
   */
  Is_True ((align == 0xffff) ||
           ((align & ~TY_ALIGN) == 0),
           ("STB_align unacceptably large (%d)\n", align));
  if (align == 0xffff) return 0;
  return (1 << align);
}
inline void
Set_STB_align (ST *s, UINT align) {
  UINT16 packed_align;
  extern UINT32 TY_log_base2 (UINT32 align);

  if (align == 0) {
    /* store 0xffff to represent an alignment of 0 */
    packed_align = 0xffff;
  }
  else if (align & 0x3f) {
    packed_align = (align & 0x7) ? (align >> 1) : 3 + (align >> 4);
  }  else {
    packed_align = TY_log_base2(align);
  }
  
  Blk_Table[ST_blk(s)].Set_align(packed_align);
}
inline UINT16
STB_section_idx (const ST *s) {
    return Blk_Table[ST_blk(s)].Section_idx();
}
inline void
Set_STB_section_idx (ST *s, UINT16 sec) {
    Blk_Table[ST_blk(s)].Set_section_idx(sec);
}
inline UINT16
STB_scninfo_idx (const ST *s) {
    return Blk_Table[ST_blk(s)].Scninfo_idx();
}
inline void
Set_STB_scninfo_idx (ST *s, UINT16 scn) {
    Blk_Table[ST_blk(s)].Set_scninfo_idx(scn);
}
inline UINT16
STB_flags (const ST *s) {
	return Blk_Table[ST_blk(s)].Flags();
}
inline BOOL
STB_is_set (const ST* s, UINT16 flags)	{ 
	return Blk_Table[ST_blk(s)].Is_set (flags); 
}
inline void
Set_STB_flags (ST* s, UINT16 flags)	{ 
	Blk_Table[ST_blk(s)].Set_flags (flags); 
}
inline void
Reset_STB_flags (ST* s, UINT16 flags)	{ 
	Blk_Table[ST_blk(s)].Clear_flags (flags); 
}
#define STB_decrement(s)	(STB_is_set (s, BLK_DECREMENT))
#define Set_STB_decrement(s)	(Set_STB_flags (s, BLK_DECREMENT))
#define Reset_STB_decrement(s)	(Reset_STB_flags (s, BLK_DECREMENT))
#define STB_section(s)		(STB_is_set (s, BLK_SECTION))
#define Set_STB_section(s)	(Set_STB_flags (s, BLK_SECTION))
#define Reset_STB_section(s)	(Reset_STB_flags (s, BLK_SECTION))
#define STB_root_base(s)	(STB_is_set (s, BLK_ROOT_BASE))
#define Set_STB_root_base(s)	(Set_STB_flags (s, BLK_ROOT_BASE))
#define Reset_STB_root_base(s)	(Reset_STB_flags (s, BLK_ROOT_BASE))
#define STB_is_basereg(s)	(STB_is_set (s, BLK_IS_BASEREG))
#define Set_STB_is_basereg(s)	(Set_STB_flags (s, BLK_IS_BASEREG))
#define Reset_STB_is_basereg(s)	(Reset_STB_flags (s, BLK_IS_BASEREG))
#define STB_exec(s)		(STB_is_set (s, BLK_EXEC))
#define Set_STB_exec(s)		(Set_STB_flags (s, BLK_EXEC))
#define Reset_STB_exec(s)	(Reset_STB_flags (s, BLK_EXEC))
#define STB_nobits(s)		(STB_is_set (s, BLK_NOBITS))
#define Set_STB_nobits(s)	(Set_STB_flags (s, BLK_NOBITS))
#define Reset_STB_nobits(s)	(Reset_STB_flags (s, BLK_NOBITS))
#define STB_merge(s)		(STB_is_set (s, BLK_MERGE))
#define Set_STB_merge(s)	(Set_STB_flags (s, BLK_MERGE))
#define Reset_STB_merge(s)	(Reset_STB_flags (s, BLK_MERGE))
#define STB_compiler_layout(s)		(STB_is_set (s, BLK_COMPILER_LAYOUT))
#define Set_STB_compiler_layout(s)	(Set_STB_flags (s, BLK_COMPILER_LAYOUT))
#define Reset_STB_compiler_layout(s)	(Reset_STB_flags (s, BLK_COMPILER_LAYOUT))

#endif /* symtab_INCLUDED */


