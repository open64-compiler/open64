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


// isa_subset_gen.h
/////////////////////////////////////
//
//  Interface for describing which instructions are valid in which ISA subsets.
//
//  void ISA_Subset_Begin( const char* archname )
//      Initialize to generate Instruction Set Architecture subset information
//      for the architecture with the given <archname>.  The information will
//      be written to the files targ_isa_subset.[ch].  
//
//  TYPE ISA_SUBSET
//      An abstract type that represents a subset of the Instruction Set
//      Architecture.  No client visible fields.
//
//  ISA_SUBSET ISA_Subset_Create( ISA_SUBSET isa_subset, const char* name )
//      Used to create a new ISA_SUBTYPE.  <isa_subset> may be either another
//      ISA_SUBSET in which case the newly created set will be a subset of the
//      given <isa_subset>, or it may be NULL, in which case the created set
//      is a root subset.  <name> should be the name for the created
//      ISA_SUBSET and is provided for debugging and documentation purposes.
//
//  void Instruction_Group( ISA_SUBSET subset, ... )
//      Lists the instructions the given <subset>.  Subsequent arguments are
//      TOPs, terminating in TOP_UNDEFINED.  These instructions are also
//      added to all supersets of <subset>.
//
//  void ISA_Subset_End(void)
//      Complete processing.
//
//
/////////////////////////////////////


//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_subset_gen.h,v $


#ifndef ISA_SUBSET_GEN_INCLUDED
#define ISA_SUBSET_GEN_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif
#ifndef ISA_SUBSET_GEN_RCS_ID
#define ISA_SUBSET_GEN_RCS_ID
#ifdef _KEEP_RCS_ID
static char *isa_subset_gen_rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_subset_gen.h,v $ $Revision: 1.1.1.1 $";
#endif
#endif

void ISA_Subset_Begin( const char* archname );
typedef struct isa_subset *ISA_SUBSET;
ISA_SUBSET ISA_Subset_Create( ISA_SUBSET subset, const char* name );
void Instruction_Group( ISA_SUBSET isa_subset, ... );
void ISA_Subset_End(void);

#ifdef __cplusplus
}
#endif
#endif
