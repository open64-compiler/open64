/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


// This may look like C code, but it is really -*- C++ -*-

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include "opt_alias_interface.h"
#include "opt_alias_mgr.h"
#include "ara_loop.h" 
#include "ara_utils.h"

extern void print_indent(FILE *fp, INT indent)
{
  for (INT i = 0; i < indent; ++i) {
    fprintf(fp," ");
  }
}

//-----------------------------------------------------------------------
// NAME: Merge_Scalar_List
// FUNCTION: Merge the scalars on 'st_from' to 'st_to'.
//-----------------------------------------------------------------------

extern void Merge_Scalar_List(SCALAR_STACK* st_from,
                              SCALAR_STACK* st_to)
{
  for (INT i = 0; i < st_from->Elements(); i++) {
    SCALAR_NODE* sn_ref = st_from->Bottom_nth(i);
    SYMBOL* sym_ref = &sn_ref->_scalar;
    for (INT j = 0; j < sn_ref->Elements(); ++j) {
      WN* wn_ref = sn_ref->Bottom_nth(j)->Wn;
      if (OPCODE_is_call(WN_opcode(wn_ref))
          || WN_operator(wn_ref) == OPR_LDA) {
        st_to->Add_Scalar(wn_ref, sym_ref, 0);
      } else {
        st_to->Add_Scalar(wn_ref, 0);
      }
    }
  }
}

//-----------------------------------------------------------------------
// NAME: Merge_Scalar_List_Covered
// FUNCTION: Merge each of the scalars on 'st_from' to one of two lists
//   'st_to_covered' or 'st_to_not_covered'.  The first list is for
//   "covered" symbols, the second for "non-covered" symbols.
//-----------------------------------------------------------------------

extern void Merge_Scalar_List_Covered(SCALAR_STACK* st_from,
                                      ARA_LOOP_INFO* ali_to,
                                      SCALAR_STACK* st_to_covered,
                                      SCALAR_STACK* st_to_not_covered)
{
  for (INT i = 0; i < st_from->Elements(); i++) {
    SCALAR_NODE* sn_ref = st_from->Bottom_nth(i);
    SYMBOL* sym_ref = &sn_ref->_scalar;
    for (INT j = 0; j < sn_ref->Elements(); ++j) {
      WN* wn_ref = sn_ref->Bottom_nth(j)->Wn;
      if (ali_to->Is_Covered(wn_ref)) {
        if (OPCODE_is_call(WN_opcode(wn_ref))
            || WN_operator(wn_ref) == OPR_LDA) {
           st_to_covered->Add_Scalar(wn_ref, sym_ref, 0);
        } else {
           st_to_covered->Add_Scalar(wn_ref, 0);
        }
      } else {
        if (OPCODE_is_call(WN_opcode(wn_ref))
            || WN_operator(wn_ref) == OPR_LDA) {
           st_to_not_covered->Add_Scalar(wn_ref, sym_ref, 0);
        } else {
           st_to_not_covered->Add_Scalar(wn_ref, 0);
        }
      }
    }
  }
}

extern POINTS_TO* Points_To(ARA_REF* ara_ref,
                            MEM_POOL* mem_pool)
{
  ST* st_array = ara_ref->Array().St();
  INT size_array = TY_size(ST_type(st_array));
  return CXX_NEW(POINTS_TO(st_array, 0, size_array), mem_pool);
}

extern POINTS_TO* Points_To(SCALAR_NODE* sn,
                            MEM_POOL* mem_pool)
{
  ST* st_scalar = sn->_scalar.St();
  INT64 st_offset = sn->_scalar.ST_Offset();
  INT st_size = MTYPE_RegisterSize(sn->_scalar.Type);
  return CXX_NEW(POINTS_TO(st_scalar, st_offset, st_size), mem_pool);
}

extern POINTS_TO* Points_To(WN* wn,
			    MEM_POOL* mem_pool)
{ 
  SYMBOL symbol(wn);
  ST* st_scalar = symbol.St();
  INT64 st_offset = symbol.ST_Offset();
  INT st_size = MTYPE_RegisterSize(symbol.Type); 
  return CXX_NEW(POINTS_TO(st_scalar, st_offset, st_size), mem_pool);
}

