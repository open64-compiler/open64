/* 
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 *
 * File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
 * front-ends to GNU 3.2.2 release.
 */

/* generate symtab references for compiler-generated runtime calls */

#include "defs.h"
#include "errors.h"
#include "symtab.h"
#include "strtab.h"

ST_IDX Throw_Runtime_st_idx()
{
  static ST_IDX result = ST_IDX_ZERO;

  if (result != ST_IDX_ZERO)
    return result;

  TY_IDX func_ty_idx = Make_Function_Type(MTYPE_To_TY(MTYPE_V));
  PU_IDX pu_idx;
  PU&    pu = New_PU(pu_idx);
  PU_Init(pu, func_ty_idx, GLOBAL_SYMTAB + 1);
  Set_PU_cxx_lang(pu);
  ST * st = New_ST(GLOBAL_SYMTAB);
  ST_Init(st, Save_Str("__throw"), CLASS_FUNC, SCLASS_EXTERN,
		       EXPORT_PREEMPTIBLE, TY_IDX(pu_idx));
  return ST_st_idx(st);
}
  

  
  


