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

#ifndef symtab_utils_INCLUDED
#define symtab_utils_INCLUDED

// This file should contains only function prototypes and inlined function
// definitions for utility routines operating on the symbol table classes.

//----------------------------------------------------------------------
// ST-related utilities
//----------------------------------------------------------------------
extern INT64 
ST_size (const ST *);			// size of st (depends on class)

extern ST *
Copy_ST (ST *st);			// make a copy of st in the same scope

extern ST *
Copy_ST (ST *st, SYMTAB_IDX scope);	// copy an st to different ST table

extern ST *
Copy_ST_No_Base (ST *st, SYMTAB_IDX scope);	// copy an st to different ST table, used by IPA inlining and cloning

// Return TRUE iff the ST is known to be a constant literal or a const
// variable.
extern BOOL
ST_is_constant (const ST *);

// Return FALSE if the current PU is recursive or the ST is not in
// the local symtab. May return TRUE otherwise; the more we return
// TRUE, the better we optimize.
extern BOOL
ST_is_private_local(const ST *);

// Create symbols for temp var or symbol.
#ifdef KEY
extern ST *
Gen_Temp_Named_Symbol(TY_IDX, const char *, ST_CLASS, ST_SCLASS);
#endif // KEY
extern ST *
Gen_Temp_Symbol(TY_IDX ty, const char *rootname);

extern ST *
Gen_Read_Only_Symbol(TY_IDX, const char *rootname);

inline BOOL
ST_visible_outside_dso(const ST &s)
{
  // rely on fe or ipa to set static funcs to internal
  return (ST_export(s) != EXPORT_INTERNAL &&
	  ST_export(s) != EXPORT_LOCAL_INTERNAL);
}
inline BOOL
ST_visible_outside_dso(const ST *s)	{ return ST_visible_outside_dso(*s); }

ST *
Gen_Intrinsic_Function(TY_IDX, const char *function_name); // implementation
						     // still
						     // incomplete.

TY_IDX
Make_Function_Type(TY_IDX return_ty_idx);  // implementation still
					   // incomplete.

TY_IDX
Make_Array_Type(TYPE_ID element_type, INT32 ndim, INT64 len);

void
Clear_local_symtab_addr_flags (const SCOPE& scope);

//----------------------------------------------------------------------
// TY-related utilities
//----------------------------------------------------------------------
extern TY_IDX MTYPE_TO_TY_array[MTYPE_LAST+1];
#define MTYPE_To_TY(t)	MTYPE_TO_TY_array[t]
#define Be_Type_Tbl(t)	MTYPE_TO_TY_array[t]
		    
// Well known predefined types
extern TY_IDX Void_Type, FE_int_Type, FE_double_Type;
extern TY_IDX Spill_Int_Type, Spill_Float_Type;
extern TY_IDX Quad_Type;

#ifdef KEY
extern TY_IDX Spill_Int32_Type;
extern TY_IDX Spill_Float32_Type;
#endif // KEY

TY_IDX
Copy_TY (TY_IDX ty);			// make a copy of a ty 

// Given a alignment value that is assumed to be a power of 2, returns the
// log (base 2) of it.  That is, (1 << TY_log_base2 (align)) == align.
extern UINT
TY_log_base2 (UINT align);

// return a type that points to ty.  return 0 if none can be found
TY_IDX
TY_pointer (TY_IDX ty, BOOL f90_pointer = FALSE);

// f90_pointer argument is ignored now, use Make_F90_Pointer_Type
TY_IDX
Make_Pointer_Type (TY_IDX ty_idx, BOOL f90_pointer = FALSE);

TY_IDX
Make_F90_Pointer_Type (TY_IDX ty_idx);


inline TY_IDX
Make_Align_Type (TY_IDX ty_idx, INT32 align)
{
    Set_TY_align (ty_idx, align);
    return ty_idx;
}

TY_IDX
Make_Array_Type (TYPE_ID element, INT32 ndim, INT64 len);

/* Given a variable, find the type to which it is promoted for
 * passing as a procedure argument.  For most, this is simply
 * ST_type(st).  However, if ST_promote_parm(st) is set, we must check
 * the promotion rules for C: 
 */
TY_IDX
Promoted_Parm_Type(const ST *);

/* Are two types equivalent? This function does equivalence check on
 * name equivalence + structural equivalence (looks also at names) by
 * default, while flags may be set to turn off/on certain checks.  The
 * default is to have all flags be zero (FALSE).  Note that alignment
 * and qualifier checking can only be turned on for the top level of a
 * constructed type.  Nested type references are always checked for
 * qualifier and alignment equality.
*/
#define TY_EQUIV_NO_FLAGS     0x00000000
#define TY_EQUIV_IGNORE_NAMES 0x00000001 /* ignore names */
#define TY_EQUIV_ALIGN        0x00000002 /* check for equal alignment */
#define TY_EQUIV_QUALIFIER    0x00000004 /* check for equal qualifiers */
BOOL
TY_are_equivalent (TY_IDX ty_id1, 
		   TY_IDX ty_id2, 
		   UINT32  flags = TY_EQUIV_NO_FLAGS);

/* TY_is_unique: Is a given type "new" or has it already been created? 
 * calls TY_are_equivalent with all flags "off" (TY_EQUIV_NO_FLAGS)
 * to check for duplicates from existing TY table;  Returns the input
 * TY_IDX if the given type is unique, and another equivalent TY_IDX
 * otherwise.
 *
 * Note that this function serves two purposes.  It checks for duplicates
 * against other types for which this function has already been called;
 * and it enters a unique type into a hash-table for use in subsequent
 * calls to this function.  I.e. it serves both to test for uniqueness,
 * and to create a dataset of unique types.
*/
TY_IDX
TY_is_unique (TY_IDX);

/* ty either is union or has union in one of its fields (called recursively) */
BOOL TY_has_union (TY_IDX ty);

/* ty is an aggregate type that has some member of volatile type */
BOOL TY_has_volatile (TY_IDX ty);

#ifdef TARG_NVISA
/* base ty (field ty) of vector */
inline TY_IDX
TY_vector_elem_ty (TY_IDX ty)
{
	return FLD_type(TY_fld(ty));
}
/* number of elements in the vector */
UINT
TY_vector_count (TY_IDX ty);
#endif

// return mtype associated with type and offset
// (e.g. might be mtype of structure field)
TYPE_ID Mtype_For_Type_Offset (TY_IDX ty, INT64 offset);

//----------------------------------------------------------------------
// PREG-related utilities
//----------------------------------------------------------------------
/*
 * predefined PREG symbols, one for each mtype
 * (actually only have pregs for the register-size mtypes and simulated mtypes;
 * in particular, the I1/I2/U1/U2 mtypes point to the 4-byte PREG.
 */
extern ST* MTYPE_TO_PREG_array[MTYPE_LAST+1];
#define MTYPE_To_PREG(t)	MTYPE_TO_PREG_array[t]
#define Int32_Preg	MTYPE_To_PREG (MTYPE_I4)
#define Int64_Preg	MTYPE_To_PREG (MTYPE_I8)
#define Float32_Preg	MTYPE_To_PREG (MTYPE_F4)
#define Float64_Preg	MTYPE_To_PREG (MTYPE_F8)
/* preferred preg symbols for physical registers 
 * (point to one of above pregs, depending on ABI). */
extern ST	*Int_Preg, *Float_Preg, *Return_Val_Preg;	/* for pseudo-registers */
// bug fix for OSP_87
extern ST      *Branch_Preg;
#ifdef TARG_X8664
extern ST* X87_Preg;
#endif

const char *
Preg_Name (PREG_NUM i);

/* create a non-dedicated preg */
extern PREG_NUM
Create_Preg_explicit (TYPE_ID mtype, const char *name,
		      SCOPE *scope_tab, SYMTAB_IDX level);

extern PREG_NUM
Create_Preg (TYPE_ID mtype, const char *name);

extern INT32
Preg_Increment (TYPE_ID mtype);

//----------------------------------------------------------------------
// ARB utilities
//----------------------------------------------------------------------
BOOL
ARB_are_equivalent(ARB_HANDLE arb_id1, 
		   ARB_HANDLE arb_id2, 
		   UINT32  flags = TY_EQUIV_NO_FLAGS);

// Swap the internal contents of two ARB's
void ARB_swap(ARB_HANDLE arb1, ARB_HANDLE arb2);

// Copy the internal contents of an ARB's
inline void ARB_copy(ARB_HANDLE arb1, ARB_HANDLE arb2) {
   *(arb1.Entry()) = *(arb2.Entry());
}


//----------------------------------------------------------------------
// FLD utilities
//----------------------------------------------------------------------
BOOL
FLD_are_equivalent(FLD_HANDLE fld_id1, 
		   FLD_HANDLE fld_id2, 
		   UINT32  flags = TY_EQUIV_NO_FLAGS);

extern FLD_HANDLE 
FLD_get_to_field (TY_IDX struct_ty_idx, UINT field_id, UINT &cur_field_id);

//----------------------------------------------------------------------
// TYLIST utilities
//----------------------------------------------------------------------
BOOL
TYLIST_are_equivalent(TYLIST_IDX tylist_id1, 
		      TYLIST_IDX tylist_id2, 
		      UINT32     flags = TY_EQUIV_NO_FLAGS);

//----------------------------------------------------------------------
// BLK utilities
//----------------------------------------------------------------------
BLK_IDX Copy_BLK (BLK_IDX b);	// copy a block

// Get the base symbol and the offset from a ST
extern void  Base_Symbol_And_Offset (
  ST     *st,               // Symbol to analyze
  ST    **base_symbol,      // Result: root base of st
  INT64  *offset_from_base  // Result: offset from primary base
);

//----------------------------------------------------------------------
// Printing routines
//----------------------------------------------------------------------

const char *
Class_Name (INT cl);

const char *
Sclass_Name (INT s);

const char *
Export_Name (INT );

const char *
Kind_Name (INT k);

void
Print_local_symtab (FILE *f, const SCOPE& scope);

void
Print_global_symtab (FILE *f);

inline void
Print_symtab (FILE *f, SYMTAB_IDX level)
{
    if (level > GLOBAL_SYMTAB)
	Print_local_symtab (f, Scope_tab[level]);
    else
	Print_global_symtab (f);
}


//----------------------------------------------------------------------
// Initialization
//----------------------------------------------------------------------
void
New_Scope (SYMTAB_IDX level, MEM_POOL *pool, BOOL reserve_index_zero);

void
Delete_Scope (SYMTAB_IDX level);

void
Initialize_Symbol_Tables (BOOL reserve_index_zero);

void
Initialize_Special_Global_Symbols ();

	    
//----------------------------------------------------------------------
// Iterators
//----------------------------------------------------------------------
inline PU_ITER
Make_pu_iter (PU_IDX pu_idx)
{
    return PU_ITER (&Pu_Table, pu_idx);
}

inline ST_ITER
Make_st_iter (const ST *st)
{
    ST_IDX idx = ST_st_idx (*st);
    return ST_ITER (Scope_tab[ST_IDX_level (idx)].st_tab, ST_IDX_index (idx));
}

inline TY_ITER
Make_ty_iter (TY_IDX ty_idx)
{
    return TY_ITER (&Ty_Table, TY_IDX_index (ty_idx));
}

inline FLD_ITER
Make_fld_iter (FLD_HANDLE fld)
{
    return FLD_ITER (&Fld_Table, fld.Idx());
}

inline TYLIST_ITER
Make_tylist_iter (TYLIST_IDX tylist_idx)
{
    return TYLIST_ITER (&Tylist_Table, tylist_idx);
}

inline ARB_ITER
Make_arb_iter (ARB_HANDLE arb)
{
    return ARB_ITER (&Arb_Table, arb.Idx());
}

inline LABEL_ITER
Make_label_iter (LABEL_IDX label_idx)
{
    return LABEL_ITER (Scope_tab[LABEL_IDX_level (label_idx)].label_tab, 
		       LABEL_IDX_index (label_idx));
}

inline PREG_ITER
Make_preg_iter (PREG_IDX preg_idx)
{
    return PREG_ITER (Scope_tab[CURRENT_SYMTAB].preg_tab, preg_idx);
}

inline ST_ATTR_ITER
Make_st_attr_iter (ST_ATTR_IDX st_attr_idx)
{
    return ST_ATTR_ITER (Scope_tab[CURRENT_SYMTAB].st_attr_tab, st_attr_idx);
}

inline TCON_ITER
Make_tcon_iter (TCON_IDX tcon_idx)
{
    return TCON_ITER (&Tcon_Table, tcon_idx);
}

inline INITO_ITER
Make_inito_iter (INITO_IDX inito_idx)
{
    return INITO_ITER (Scope_tab[INITO_IDX_level (inito_idx)].inito_tab,
		       INITO_IDX_index (inito_idx));
}

inline INITV_ITER
Make_initv_iter (INITV_IDX initv_idx)
{
    return INITV_ITER (&Initv_Table, initv_idx);
}


// loop over all symbols, always skip the first entry because it is reserved.

template <class T, UINT block_size, class OP>
inline void
For_all (SEGMENTED_ARRAY<T, block_size>& table, const OP& op)
{
    For_all_entries (table, op, 1);
}

template <class T, UINT block_size, class OP>
inline void
For_all (RELATED_SEGMENTED_ARRAY<T, block_size>& table, const OP& op)
{
    For_all_entries (table, op, 1);
}

template <class OP>
inline void
For_all (const SYMBOL_TABLE&, SYMTAB_IDX level, const OP& op)
{
    For_all_entries (*Scope_tab[level].st_tab, op, 1);
}

template <class OP>
inline void
For_all (const INITO_TABLE&, SYMTAB_IDX level, const OP& op)
{
    For_all_entries (*Scope_tab[level].inito_tab, op, 1);
}

template <class OP>
inline void
For_all (const LABEL_TABLE&, const OP& op)
{
    For_all_entries (*Scope_tab[CURRENT_SYMTAB].label_tab, op, 1);
}

template <class OP>
inline void
For_all (const PREG_TABLE&, const OP& op)
{
    For_all_entries (*Scope_tab[CURRENT_SYMTAB].preg_tab, op, 1);
}

template <class OP>
inline void
For_all (const ST_ATTR_TABLE&, SYMTAB_IDX level, const OP& op)
{
    For_all_entries (*Scope_tab[level].st_attr_tab, op, 1);
}

template <class OP>
inline void
For_all (const TYPE_TABLE&, const OP& op)
{
    For_all_entries (Ty_tab, op, 1);
}


//----------------------------------------------------------------------
// iterator with early exit
//----------------------------------------------------------------------

template <class T, UINT block_size, class PREDICATE>
inline UINT32
For_all_until (const SEGMENTED_ARRAY<T, block_size>& table,
	       const PREDICATE& pred)
{
    UINT32 idx = Find_entry_if (table, pred, 1);
    return (idx == NOT_FOUND) ? 0 : idx;
}

template <class T, UINT block_size, class PREDICATE>
inline UINT32
For_all_until (const RELATED_SEGMENTED_ARRAY<T, block_size>& table,
	       const PREDICATE& pred)
{
    UINT32 idx = Find_entry_if (table, pred, 1);
    return (idx == NOT_FOUND) ? 0 : idx;
}

template <class PREDICATE>
inline ST_IDX
For_all_until (const SYMBOL_TABLE&, SYMTAB_IDX level, const PREDICATE& pred)
{
    UINT32 idx = Find_entry_if (*Scope_tab[level].st_tab, pred, 1);
    return (idx == NOT_FOUND) ? 0 : make_ST_IDX (idx, level);
}

template <class PREDICATE>
inline INITO_IDX
For_all_until (const INITO_TABLE&, SYMTAB_IDX level, const PREDICATE& pred)
{
    UINT32 idx = Find_entry_if (*Scope_tab[level].inito_tab, pred, 1);
    return (idx == NOT_FOUND) ? 0 : make_INITO_IDX (idx, level);
}

template <class PREDICATE>
inline UINT32
For_all_until (const LABEL_TABLE&, const PREDICATE& pred)
{
    UINT32 idx = Find_entry_if (*Scope_tab[CURRENT_SYMTAB].label_tab, pred, 1);
    return (idx == NOT_FOUND) ? 0 : idx;
}

template <class PREDICATE>
inline UINT32
For_all_until (const PREG_TABLE&, const PREDICATE& pred)
{
    UINT32 idx = Find_entry_if (*Scope_tab[CURRENT_SYMTAB].preg_tab, pred, 1);
    return (idx == NOT_FOUND) ? 0 : idx;
}

template <class PREDICATE>
inline ST_ATTR_IDX
For_all_until (const ST_ATTR_TABLE&, SYMTAB_IDX level, const PREDICATE& pred)
{
    UINT32 idx = Find_entry_if (*Scope_tab[level].st_attr_tab, pred, 1);
    return (idx == NOT_FOUND) ? 0 : idx;
}


template <class PREDICATE>
inline TY_IDX
For_all_until (const TYPE_TABLE&, const PREDICATE& pred)
{
    UINT32 idx = Find_entry_if (Ty_tab, pred, 1);
    return (idx == NOT_FOUND) ? 0 : make_TY_IDX (idx);
}




// loop over all symbols (should change to use iterators)

#define FOREACH_SYMBOL(level,s,i) \
	for (i = 1; i < ST_Table_Size(level) && (s = &St_Table(level,i)); ++i)

#define FOREACH_LABEL(level,s,i) \
	for (i = 1; i < LABEL_Table_Size(level) && (s = &Label_Table(i)); ++i)

// loop over all inito's
#define FOREACH_INITO(level,o,i) \
	for (i = 1; i < INITO_Table_Size(level) && (o = &Inito_Table(level,i)); ++i)

#define FOREACH_INITV(init,v) \
	for (v = init; v != 0; v = Initv_Table[v].next)

//
// The following utility function Find_Section_Name_For_ST() is moved
// from data_layout.cxx for reuse in ipc_symtab_merge.cxx
//

// return section name for corresponding ST via st_attr table
struct find_st_attr_secname {
        ST_IDX st;
        find_st_attr_secname (const ST *s) : st (ST_st_idx (*s)) {}
 
        BOOL operator () (UINT, const ST_ATTR *st_attr) const {
            return (ST_ATTR_kind (*st_attr) == ST_ATTR_SECTION_NAME &&
                    ST_ATTR_st_idx (*st_attr) == st);
        }
};
 
inline STR_IDX
Find_Section_Name_For_ST (const ST *st)
{
    ST_IDX idx = ST_st_idx (*st);
    ST_ATTR_IDX d;

    d = For_all_until (St_Attr_Table, ST_IDX_level (idx),
                          find_st_attr_secname(st));
    FmtAssert(d != 0, ("didn't find section name for ST %s", ST_name(*st)));
    return ST_ATTR_section_name(St_Attr_Table(ST_IDX_level (idx), d));
}

#endif /* symtab_utils_INCLUDED */

 
