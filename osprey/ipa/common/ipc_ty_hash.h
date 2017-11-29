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
#ifndef ipc_ty_hash_INCLUDED
#define ipc_ty_hash_INCLUDED

// we steal a TY_IDX bit for temporary marking of merge state
const UINT TY_INDEX_IS_VISITED	= 0x1;	// for loop detection
const UINT TY_INDEX_IS_TEMP	= 0x2;	// TY_IDX is being validated
const UINT TY_INDEX_IS_INSERTED = 0x4;	// inserted into the Ty_Table but
					// not in the hash table

static inline BOOL
Valid_TY_IDX (TY_IDX ty_idx)
{
    return ((ty_idx & 0xff) == 0 && ty_idx != 0);
}
	
static inline void
Set_TY_Merging (TY_IDX& ty_idx)
{
    ty_idx = TY_INDEX_IS_VISITED;
}

static inline BOOL
TY_Merging (TY_IDX ty_idx)
{
    return ty_idx == TY_INDEX_IS_VISITED;
}

static inline void
Set_TY_Temp_Idx (TY_IDX& ty_idx, TY_IDX temp)
{
    ty_idx = (temp &  ~0xff) | TY_INDEX_IS_TEMP | (ty_idx & 0xff);
}

static inline void
Clear_TY_Temp_Idx (TY_IDX& ty_idx)
{
    ty_idx &= (~TY_INDEX_IS_TEMP & 0xff);
}

static inline BOOL
Is_TY_Temp_Idx (TY_IDX ty_idx)
{
    return (ty_idx & TY_INDEX_IS_TEMP);
}

static inline TY_IDX
TY_Temp_Idx (TY_IDX_MAP& ty_map, TY_IDX old_idx)
{
    TY_IDX ty_idx = ty_map.map_[TY_IDX_index (old_idx)];
    return (ty_idx & TY_INDEX_IS_TEMP) ? (ty_idx & ~0xff) : 0;
}


static inline void
Set_TY_Inserted (TY_IDX& ty_idx, TY_IDX new_idx)
{
    ty_idx = Replace_TY_IDX_index (TY_INDEX_IS_INSERTED, new_idx);
}

static inline BOOL
TY_Inserted (TY_IDX ty_idx)
{
    return ty_idx & TY_INDEX_IS_INSERTED;
}


static inline void
Clean_TY_IDX (TY_IDX& ty_idx)
{
    ty_idx &= ~0xff;
}

static inline TY_IDX
TY_IDX_Attributes (TY_IDX ty_idx)
{
    return (ty_idx & 0xff);
}


extern BOOL
Partial_Compare_Fld (FLD_HANDLE merged_fld, const FLD* new_fld);

extern BOOL
Partial_Compare_Arb (ARB_HANDLE merged_arb, const ARB* new_arb);

extern void
Initialize_Type_Merging_Hash_Tables (MEM_POOL* pool);

extern void
Setup_Type_Merging_Hash_Tables (const IPC_GLOBAL_TABS& original_tabs,
				IPC_GLOBAL_IDX_MAP& idx_map);

extern TY_IDX
Insert_Unique_Ty (const TY& ty);

extern void
Insert_Allocated_Ty (TY& ty, TY_IDX ty_idx);

#define IN_SET(__set, __element) (__set.find(__element) != __set.end())
#define NOT_IN_SET(__set, __element) (__set.find(__element) == __set.end())
    
inline BOOL
TY_is_incomplete_struct (const TY &ty) {
    return TY_kind(ty) == KIND_STRUCT &&
           TY_size(ty) == 0 && ty.Fld() == 0;
}

extern void
Insert_Recursive_Type (TY_IDX ty_idx);

inline BOOL
TY_is_incomplete_struct (TY_IDX tyi) {
    return TY_is_incomplete_struct(Ty_Table[tyi]);
}

inline TY_IDX
TY_IDX_without_attribute(TY_IDX tyi) {
    return tyi & (~TY_ALIGN);
}

void
Insert_Recursive_Type (TY_IDX ty_idx);

// below 2 functions should be removed when old type merge is removed.

void
Initialize_New_Recursive_Type (TY_IDX ty_idx);

void
Finalize_New_Recursive_Type ();

typedef vector<TY_IDX> TY_IDX_VEC;

extern void
Find_Matching_Ty (const TY& ty, TY_IDX_VEC& matched_list);

#endif /* ipc_ty_hash_INCLUDED */
