/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement
 * or the like.  Any license provided herein, whether implied or
 * otherwise, applies only to this software file.  Patent licenses, if
 * any, provided herein do not apply to combinations of this program with
 * other software, or any other product whatsoever.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */

#ifndef be_ipa_util_INCLUDED
#define be_ipa_util_INCLUDED

#include "segmented_array.h"

struct BE_SUMMARY_HEADER
{
  mUINT64 offset;           // offset from beginning of section
  mUINT64 size;             // size in bytes
  mUINT32 entsize;          // size of each entry
};

struct pu_mod_ref_info
{
  PU_IDX pu_idx;            // pu id
  mUINT32 size;             // size in bytes of mod info (must equal ref)
  mUINT8 * mod;             // bit-vector for mod
  mUINT8 * ref;             // bit-vector for ref
};

typedef SEGMENTED_ARRAY<pu_mod_ref_info> MOD_REF_INFO_TAB;

extern MOD_REF_INFO_TAB Mod_Ref_Info_Table;

inline pu_mod_ref_info&
New_Mod_Ref_Info (UINT32 &index)
{
  return Mod_Ref_Info_Table.New_entry (index);
}

inline UINT
Mod_Ref_Info_Table_Size (void)  { return Mod_Ref_Info_Table.Size(); }

#endif // be_ipa_util_INCLUDED
