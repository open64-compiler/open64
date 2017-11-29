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


// -*-C++-*-

/** $Revision: 1.6 $
*** $Date: 04/12/21 14:57:14-08:00 $
*** $Author: bos@eng-25.internal.keyresearch.com $
*** $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.lu_mat_textra.cxx $
**/

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#define lu_mat_textra_CXX      "lu_mat_textra.cxx"
const static char *rcs_id =   lu_mat_textra_CXX "$Revision: 1.6 $";


#include "lnopt_main.h"
#include "mat.h"
#include "lu_mat.h"

template<>
void LU_DMAT::Print_Element(FILE* f, double e)
{
  fprintf(f, "%g", e);
}

template<>
void LU_FMAT::Print_Element(FILE* f, FRAC e)
{
  e.Print(f);
}

template<>
BOOL LU_FMAT::Exact_Arithmetic()
{
  return TRUE;
}

template<>
BOOL LU_DMAT::Exact_Arithmetic()
{
  return FALSE;
}

