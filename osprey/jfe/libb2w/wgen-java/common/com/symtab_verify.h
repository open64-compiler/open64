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


// -*-Mode: c++;-*- (Tell emacs to use c++ mode) 

#ifndef symtab_verify_INCLUDED
#define symtab_verify_INCLUDED
#include "config.h" // For LANG_Symtab_Verify_On

//
// Extern interface for Symbol Table Verifiers; 

extern void ST_Verify_Class_Sclass (ST_CLASS, ST_SCLASS );

extern void ST_Verify_Sclass_Export (ST_SCLASS, ST_EXPORT, const ST* st = NULL);

extern void TY_Verify_Kind_Mtype (TY_KIND , mTYPE_ID );

/*ARGSUSED*/ 
inline void TY_Verify_Kind_Function(TY_KIND k, UINT64 s, TYPE_ID t) {
  Is_True( k == KIND_FUNCTION && 
           s == 0 && t == 0,
           ("ty.size/mtype should be 0 for KIND_FUNCTION"));
}
  

extern void  Verify_LOCAL_SYMTAB (const SCOPE& , SYMTAB_IDX );

extern void  Verify_GLOBAL_SYMTAB ();

// ======================================================================
//  Verify_SYMTAB(): inline function
// ======================================================================

inline void
Verify_SYMTAB (SYMTAB_IDX level)
{
  if (!LANG_Symtab_Verify_On) {
    DevWarn ("The NEW SYMTAB Verification is turned OFF; Proceed on your own");
    return;
  }

  // We really do want to verify symbol tables

  if (level > GLOBAL_SYMTAB)
    Verify_LOCAL_SYMTAB (Scope_tab[level], level);
  else
    Verify_GLOBAL_SYMTAB ();
}

#endif // symtab_verify_INCLUDED
