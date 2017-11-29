/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
#ifndef cxx_ipa_cprop_annot_INCLUDED
#define cxx_ipa_cprop_annot_INCLUDED

extern BOOL
Union_Formal_Cprop_Annot (IPA_NODE *callee, IPA_EDGE* e);

extern BOOL
Intra_PU_Formal_Cprop (IPA_NODE *node);

extern BOOL
Intra_PU_Global_Cprop (IPA_NODE* node);

extern void
Map_Global_Constants (IPA_NODE *node);


enum IPA_CLONING_ACTION 
{
  NEEDS_CLONING,
  ANNOTATION_CHANGED,
  NO_CHANGE
};

// Union of cprop annotations for a quasi clone and its origin
extern void 
Union_Quasi_Clone_Cprop_Annot (IPA_NODE* origin, IPA_NODE* clone);

// Union of cprop annotations enhanced by cloning
extern IPA_CLONING_ACTION 
Union_Formal_Cprop_Annot_With_Cloning (IPA_NODE*, IPA_EDGE*);

// Check if two edges are equivalent wrt to their cprop annotations
extern BOOL 
Edges_Have_Equiv_Cprop_Annots (const IPA_EDGE*, const IPA_EDGE*);

#endif // cxx_ipa_cprop_annot_INCLUDED
