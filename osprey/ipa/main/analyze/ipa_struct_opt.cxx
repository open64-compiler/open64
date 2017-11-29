/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007, 2008 PathScale, LLC. All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify it
// under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it is
// free of the rightful claim of any third person regarding infringement
// or the like.  Any license provided herein, whether implied or
// otherwise, applies only to this software file.  Patent licenses, if
// any, provided herein do not apply to combinations of this program with
// other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write the Free Software Foundation, Inc., 59
// Temple Place - Suite 330, Boston MA 02111-1307, USA.
////////////////////////////////////////////////////////////////////////////

#include <stdint.h>
#include <stdlib.h>

#include "defs.h"
#include "wn.h"
#include "wn_util.h"
#include "symtab.h"
#include "config_ipa.h"
#include "ipa_struct_opt.h"
#include "tracing.h"
#include "ipa_option.h" // Trace_IPA

Field_pos *Struct_field_layout = NULL;
INT Struct_split_count = 0;

TYPE_ID complete_struct_relayout_type_id = 0;
TYPE_ID struct_with_field_pointing_to_complete_struct_relayout_type_id
  [MAX_NUM_STRUCTS_WITH_FIELD_POINTING_TO_COMPLETE_STRUCT_RELAYOUT];
int struct_with_field_pointing_to_complete_struct_relayout_field_num
  [MAX_NUM_STRUCTS_WITH_FIELD_POINTING_TO_COMPLETE_STRUCT_RELAYOUT];
int num_structs_with_field_pointing_to_complete_struct_relayout = 0;

// Invalidate struct types that are either parameters or returned from
// functions, or if a pointer to such types is a parameter or returned
// from a function.
void Traverse_PU_parameters (void)
{
  for (INT puid = 1; puid < PU_Table_Size(); puid++)
  {
    TY_IDX type = TY_IDX_ZERO;
    TY_IDX prototype = PU_prototype (Pu_Table[puid]);
    TYLIST_IDX tylist = TY_tylist(prototype);

    while ((type = Tylist_Table[tylist]) != 0)
    {
      if (TY_kind(type) == KIND_STRUCT)
        Set_TY_no_split(type);
      else if (TY_kind(type) == KIND_POINTER &&
               TY_kind(TY_pointed(type)) == KIND_STRUCT)
        Set_TY_no_split(TY_pointed(type));

      tylist++;
    }
  }
}

// Invalidate all child STRUCT types.
static void invalidate_it (TY& ty)
{
  // Don't traverse empty structs
  if (TY_size(ty) && ty.Fld())
  {
    FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty));
    do
    {
      FLD_HANDLE fld(fld_iter);

      TY_IDX idx = FLD_type(fld);
      if (TY_kind(idx) == KIND_STRUCT)
        invalidate_it (Ty_Table[idx]);
    } while (!FLD_last_field(fld_iter++));
  }

  Set_TY_no_split(ty);
}

INT struct_field_count (TY_IDX ty_idx)
{
  Is_True (Ty_Table[ty_idx].Fld() != 0, ("struct TY should have >0 fields"));
  FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty_idx));
  INT field_count = 0;
  do
  {
    field_count++;
  } while (!FLD_last_field(fld_iter++));

  return field_count;
}

extern mUINT32 Struct_split_candidate_index;
extern mUINT32 Struct_update_index;

// Traverse through type-table and invalidate any as necessary.
void Traverse_TYs (void)
{
  TY_TAB::iterator iter;

  // First pass
  for (iter = Ty_tab.begin(); iter != Ty_tab.end(); iter++)
  {
    //TY_IDX ty = iter.Index();
    TY& ty = *iter.Ptr();
    if (TY_kind(ty) == KIND_STRUCT && !TY_no_split(ty))
    {
      // Empty structs
      if (TY_size(ty) == 0 || ty.Fld() == 0)
        Set_TY_no_split(ty);
      else if (TY_is_union(ty))
        Set_TY_no_split(ty);
      else
      {
        FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty));
        INT field_count = 0;
        do
        {
          FLD_HANDLE fld(fld_iter);
          TYPE_ID mtype = TY_mtype(FLD_type(fld));
          field_count++;
          if (FLD_is_bit_field(fld) || FLD_begin_union(fld))
          {
            Set_TY_no_split(ty);
            break;
          }
          if (IPA_Enable_Struct_Opt == 1 &&
              ((mtype != MTYPE_U8 && mtype != MTYPE_C4) ||
               TY_kind(FLD_type(fld)) == KIND_POINTER))
          {
            Set_TY_no_split(ty);
            break;
          }
          if (FLD_last_field(fld))
            break;
        } while (!FLD_last_field(fld_iter++));

        // Struct must have 2 fields.
        if (IPA_Enable_Struct_Opt == 1 && field_count != 2)
          Set_TY_no_split(ty);

        if (IPA_Enable_Struct_Opt == 2 && field_count != 8)
          Set_TY_no_split(ty);
      }
    }

    if (TY_kind(ty) == KIND_STRUCT && TY_no_split(ty))
      invalidate_it (ty);
  }

  // Second pass
  for (iter = Ty_tab.begin(); iter != Ty_tab.end(); iter++)
  {
    TY& ty = *iter.Ptr();
    if (TY_kind(ty) == KIND_STRUCT && !TY_no_split(ty))
    {
      Struct_split_candidate_index = iter.Index();
      // Take only the first such struct TY.
      break;
    }
  }

  if (!Struct_split_candidate_index)
    return;

  // Third pass
  for (iter = Ty_tab.begin(); iter != Ty_tab.end(); iter++)
  {
    TY& ty = *iter.Ptr();
    if (TY_kind(ty) == KIND_STRUCT)
    {
      // Empty struct, union
      if (TY_size(ty) == 0 || ty.Fld() == 0 || TY_is_union(ty))
        continue;
      else
      {
        FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty));
        INT field_count = 0;
        do
        {
          FLD_HANDLE fld(fld_iter);
          field_count++;
          TY_IDX fldtype = FLD_type(fld);

          if (TY_kind(fldtype) == KIND_POINTER &&
              TY_IDX_index(TY_pointed(fldtype)) == Struct_split_candidate_index)
          {
            if (Struct_update_index)
            {
              // There can be at most 1 struct with at most 1 pointer to the
              // type being split.
              Struct_update_index = 0;
              Struct_split_candidate_index = 0;
              return;
            }
            Struct_update_index = iter.Index();
          }

          if (FLD_last_field(fld))
            break;
        } while (!FLD_last_field(fld_iter++));
      }
    }
  }

  if (IPA_Enable_Struct_Opt == 1 && Struct_update_index == 0)
    Struct_split_candidate_index = 0;

  if (Struct_split_candidate_index)
  {
    INT field_count = struct_field_count (Struct_split_candidate_index<<8);

    Struct_field_layout =
            (Field_pos *) malloc (field_count * sizeof(Field_pos));

    Struct_split_count = field_count;

    // Relayout the struct into individual fields.
    for (INT i=0; i<field_count; i++)
    {
      Struct_field_layout[i].u.struct_id = i;
      Struct_field_layout[i].fld_id = 1;
      Struct_field_layout[i].st_idx = ST_IDX_ZERO;
    }
  }
}

// Prepare for complete_struct_relayout optimization.  This involves retrieving
// the structure marked for this optimization by ipl and set up some supporting
// data structures.
static void
Traverse_TYs_for_complete_struct_relayout(void)
{
  TY_TAB::iterator iter;
  TYPE_ID local_complete_struct_relayout_type_id = 0;
  int i;

  if (IPA_Enable_Struct_Opt == 0)
    return; // nothing to do

  for (iter = Ty_tab.begin(); iter != Ty_tab.end(); iter++)
  {
    TY& ty = *iter.Ptr();
    if (TY_kind(ty) == KIND_STRUCT &&
        TY_complete_struct_relayout_candidate(ty)) // marked by ipl
    {
      if (local_complete_struct_relayout_type_id == 0)
        local_complete_struct_relayout_type_id = iter.Index();
      else
      {
        complete_struct_relayout_type_id = 0;
        return; // ipl should only have marked one
      }
    }
  }

  if (local_complete_struct_relayout_type_id == 0)
  {
    complete_struct_relayout_type_id = 0;
    return; // nothing marked
  }
  complete_struct_relayout_type_id = local_complete_struct_relayout_type_id;

  // some more restrictions
  if (struct_field_count(complete_struct_relayout_type_id << 8) >=
    MAX_NUM_FIELDS_IN_COMPLETE_STRUCT_RELAYOUT)
  {
    complete_struct_relayout_type_id = 0;
    return;
  }

  // now go through all the structures again and record all the fields whose
  // type is a pointer to the complete_struct_relayout
  for (i = 0; i <
       MAX_NUM_STRUCTS_WITH_FIELD_POINTING_TO_COMPLETE_STRUCT_RELAYOUT; i++)
  {
    // initialization
    struct_with_field_pointing_to_complete_struct_relayout_type_id[i] = 0;
    struct_with_field_pointing_to_complete_struct_relayout_field_num[i] = 0;
  }
  for (iter = Ty_tab.begin(); iter != Ty_tab.end(); iter++)
  {
    TY& ty = *iter.Ptr();
    if (TY_kind(ty) == KIND_STRUCT)
    {
      FLD_ITER field_iter = Make_fld_iter(TY_fld(ty));
      i = 1;
      do
      {
        FLD_HANDLE field_handle(field_iter);
        if (TY_kind(FLD_type(field_handle)) == KIND_POINTER &&
            (TY_IDX_index(TY_pointed(FLD_type(field_handle))) ==
               complete_struct_relayout_type_id))
        {
          struct_with_field_pointing_to_complete_struct_relayout_type_id
            [num_structs_with_field_pointing_to_complete_struct_relayout] =
            iter.Index();
          struct_with_field_pointing_to_complete_struct_relayout_field_num
            [num_structs_with_field_pointing_to_complete_struct_relayout] = i;
          num_structs_with_field_pointing_to_complete_struct_relayout++;
          if (num_structs_with_field_pointing_to_complete_struct_relayout >=
              MAX_NUM_STRUCTS_WITH_FIELD_POINTING_TO_COMPLETE_STRUCT_RELAYOUT)
          {
            // too many
            complete_struct_relayout_type_id = 0;
            break;
          }
        }
        i++;
      } while (!FLD_last_field(field_iter++));
    }
  }

  if (complete_struct_relayout_type_id != 0)
  {
    if (Get_Trace(TP_IPA, 1))
      fprintf(TFile, "ipa -> complete_struct_relayout %d %s\n",
        complete_struct_relayout_type_id,
        TY_name(complete_struct_relayout_type_id << 8));
  }
  return; // all ready for ipo
}

void IPA_struct_opt_legality (void)
{
  Traverse_PU_parameters ();

  // This function should be called after all type legality checks are done.
  Traverse_TYs ();

  if (Struct_split_candidate_index != 0)
  {
    // the design decision is such that complete_struct_relayout optimization
    // will not compete with structure splitting or structure peeling
    // optimizations; so if the above identifies a candidate for either of the
    // latter optimizations, the new complete_struct_relayout optimization will
    // not be invoked
    return;
  }

  // no opportunities for structure splitting/peeling were found; try
  // complete_struct_relayout optimization
  complete_struct_relayout_type_id = 0;
  // save some compile time, only do this if -mso (multi-core scaling
  // optimization is on) (otherwise nothing would have been marked anyway)
  // comment out the following since this optimization benefits both mso as well
  // as non-mso compilations
  // if (IPA_Enable_Scale)
  Traverse_TYs_for_complete_struct_relayout();
}
