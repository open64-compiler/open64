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


#ifndef _LEGO_GEN_INCLUDED_
#define _LEGO_GEN_INCLUDED_

/***********************************************************************
 *
 * Generate code for lego pragmas.
 *
 * Exported functions:
 *
 *  extern void Generate_Runtime_Stuff ();
 *      Called before ANY LNO processing, from Lno_Init.
 *      Generate ST and TY entries in the global symbol table
 *      for functions and types in the MP runtime.
 *
 *  extern void Lower_Distr_Pragmas (WN* func_nd);
 *      Generate code for data distribution pragmas.
 *      Called on a per-PU basis at the end of LNO processing.
 *      Generates code into the PU for each data distribution pragma
 *      contained in the global variables da_hash and da_hash_redistr.
 *
 *  extern void Rewrite_Reshaped_Commons (WN* wn)
 *      Walk the entire tree, replacing all references to a reshaped
 *      common array to be to the new ST in the new common block that we
 *      have generated.
 *
 ***********************************************************************/

#include "lego_pragma.h"
#include "lego_util.h"

extern "C" void Generate_Runtime_Stuff ();
extern void Lower_Distr_Pragmas (WN* func_nd);
extern WN*  Get_Array_Dimension_Size (TY* array_ty, INT i); 
extern void Init_Special_Lego_Mp_Call();
extern BOOL Special_Lego_Or_Mp_Call(ST* st_call); 
extern ST *Create_Local_Array_ST(char* array_name, TY_IDX ty, INT num);
extern void Rewrite_Reshaped_Commons (WN* wn);

#endif // _LEGO_GEN_INCLUDED_
