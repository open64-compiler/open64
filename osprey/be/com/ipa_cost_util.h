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


#ifndef ipa_cost_util_INCLUDED
#define ipa_cost_util_INCLUDED "ipa_cost_util.h"

// Constant declaration.

const INT DEFAULT_TRIP_COUNT = 100;
const INT64 DEFAULT_CALL_COST = 100; 
const INT MAX_VALUE_COUNT = 100;
const INT MAX_EXPR_COUNT = 100;
const INT MAX_CALL_EXPR_COUNT = 15;

extern INT IPL_EX_New_Constant(DYN_ARRAY<SUMMARY_VALUE>* sv,
                               INT64 constant_value);

extern INT IPL_EX_New_Value_Expr(DYN_ARRAY<SUMMARY_EXPR>* sx,
                                 INT sv_index);

extern void IPL_EX_Eliminate_Value(DYN_ARRAY<SUMMARY_VALUE>* sv,
                                   DYN_ARRAY<SUMMARY_EXPR>* sx,
                                   INT value_index);

extern void IPL_EX_Add_Value_Offsets(DYN_ARRAY<SUMMARY_VALUE>* sv,
                                     INT formal_offset,
                                     INT global_offset);

extern void IPL_EX_Add_Expr_Offsets(DYN_ARRAY<SUMMARY_EXPR>* sx,
                                    INT value_offset,
                                    INT expr_offset);

extern void Print_Exprs(FILE* fp,
			DYN_ARRAY<SUMMARY_VALUE>* sv,
                        DYN_ARRAY<SUMMARY_EXPR>* sx);

extern INT Check_Exprs(DYN_ARRAY<SUMMARY_VALUE>* sv,       
                       DYN_ARRAY<SUMMARY_EXPR>* sx,
                       FILE* fp);

extern void IPL_EX_Collapse_Trip_Counts(DYN_ARRAY<SUMMARY_VALUE>* sv,
                                        DYN_ARRAY<SUMMARY_EXPR>* sx);

extern BOOL IPL_EXS_Too_Complicated(DYN_ARRAY<SUMMARY_VALUE>* sv,
                                    DYN_ARRAY<SUMMARY_EXPR>* sx,
				    INT multiplier);

extern INT IPL_EXS_Chop_Down_Estimate(DYN_ARRAY<SUMMARY_VALUE>* sv,
                                      DYN_ARRAY<SUMMARY_EXPR>* sx);

extern void IPL_EX_Simplify(DYN_ARRAY<SUMMARY_VALUE>* sv,
                            DYN_ARRAY<SUMMARY_EXPR>* sx);

#endif /* ipa_cost_util_INCLUDED */

