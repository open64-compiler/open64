/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008 PathScale, LLC. All Rights Reserved.
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

#ifndef cxx_ipa_struct_opt_INCLUDED
#define cxx_ipa_struct_opt_INCLUDED

#include "defs.h"
#include "wn.h"
#include "wn_util.h"
#include "symtab.h"
#include "config_ipa.h"

// Gives the fate of each field in a struct to be split. Fields going
// to a new sub-struct will have the same STRUCT_ID. The FLD_ID will
// tell the order of these fields in that sub-struct. For individual
// fields, they will have different STRUCT_ID, and a FLD_ID of 1.
typedef struct Field_pos_
{
  union
  {
    mUINT32    struct_id;
    TY_IDX     new_ty;
  } u;

  FLD_IDX    fld_id;
  ST_IDX     st_idx;
} Field_pos;

// Data structure to tell IPO how to relayout a struct. It has N
// elements, where N is the number of fields in the original struct.
extern Field_pos *Struct_field_layout;
// Number of pieces (individual fields or smaller structs) a struct
// is split into.
extern INT Struct_split_count;

#define MAX_NUM_FIELDS_IN_COMPLETE_STRUCT_RELAYOUT 16
extern TYPE_ID complete_struct_relayout_type_id;

#define MAX_NUM_STRUCTS_WITH_FIELD_POINTING_TO_COMPLETE_STRUCT_RELAYOUT 32
extern TYPE_ID struct_with_field_pointing_to_complete_struct_relayout_type_id
  [MAX_NUM_STRUCTS_WITH_FIELD_POINTING_TO_COMPLETE_STRUCT_RELAYOUT];
extern int struct_with_field_pointing_to_complete_struct_relayout_field_num
  [MAX_NUM_STRUCTS_WITH_FIELD_POINTING_TO_COMPLETE_STRUCT_RELAYOUT];
extern int num_structs_with_field_pointing_to_complete_struct_relayout;

#endif // cxx_ipa_struct_opt_INCLUDED
