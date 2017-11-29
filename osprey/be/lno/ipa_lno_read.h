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


#ifndef ipa_lno_read_INCLUDED
#define ipa_lno_read_INCLUDED "ipa_lno_read.h"

#ifndef _IPA_LNO_FILE
#include "ipa_lno_file.h"
#endif

extern TYPE_ID Formal_Machine_Type(WN* wn_call, 
				   INT formal_number,
				   IPA_LNO_READ_FILE* IPA_LNO_File);

extern BOOL Scalar_Expr(WN* wn_expr);

extern BOOL Linear_Expr(WN* wn_expr,
                        DYN_ARRAY<WN*>* wn_list,
                        DYN_ARRAY<INT>* int_list,
                        INT64* const_value);

extern void IPA_LNO_Map_Calls(WN* func_nd,
                              IPA_LNO_READ_FILE* IPA_LNO_File);

extern INT IPA_LNO_Procedure_Index(WN* wn_call,
                                   IPA_LNO_READ_FILE* IPA_LNO_File);

extern void Update_Loop_Stmt(WN* wn_use);

extern void Add_Scalars_In_Expr(WN* wn_expr,
                                SCALAR_STACK* st_scalar);

extern WN* Single_Definition_Temp(WN* wn_argument);

#endif /* ipa_lno_read_INCLUDED */
