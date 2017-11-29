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


// isa_hazards_gen.h
/////////////////////////////////////
//
//  Interface for describing instructions that have ISA hazards, and what
//  the hazard is.
//
//  void ISA_Hazards_Begin( const char* archname )
//      Initialize to generate Instruction Set Architecture hazard information
//      for the architecture with the given <archname>.  The information will
//      be written to the files <archname>_hazards.[ch].  
//
//  TYPE ISA_HAZARD
//      An abstract type that represents a type of ISA hazard.
//      No client visible fields.
//
//  ISA_HAZARD Hazard_Create( const char *name )
//      Used to create a new ISA_HAZARD. <name> is used to construct
//      an enumeration constant for this hazard in the generated output
//	of the form: ISA_HAZARD_<name>
//
//  void Hazard_Group( TOP topcode, ... )
//      Lists a group of instructions (by TOP), with a common hazard 
//	specification, terminating in TOP_UNDEFINED. Subsequent
//	statements specify the details of the hazard. The specification
//	is terminated by another Hazard_Group, or ISA_Hazards_End.
//
//  void Hazard_Type( ISA_HAZARD isa_hazard )
//	Indicates the type of the hazard.
//
//  void Hazard_Data( int data )
//	One word of hazard-specific data, e.g. an operand number.
//
//  void Hazard_Post_Ops( int ops )
//	The number of OPs which must follow an OP in the hazard group
//
//  void Hazard_Pre_Ops( int ops )
//	The number of OPs which must precede an OP in the hazard group
//
//  void Hazard_ISA( ISA_SUBSET isa_subset )
//	Not all ISAs have the same hazards. Hazard_ISA specifies that
//	'isa_subset' has the current hazard. Hazard_ISA may be called
//	multiple times per hazard group.
//
//  void ISA_Hazards_End(void)
//      Complete processing.
//
//
/////////////////////////////////////


//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_hazards_gen.h,v $


#ifndef ISA_HAZARDS_GEN_INCLUDED
#define ISA_HAZARDS_GEN_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif
#ifndef ISA_HAZARDS_GEN_RCS_ID
#define ISA_HAZARDS_GEN_RCS_ID
#ifdef _KEEP_RCS_ID
static const char isa_hazards_gen_rcs_id[] = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_hazards_gen.h,v $ $Revision: 1.1.1.1 $";
#endif
#endif

void ISA_Hazards_Begin( const char* archname );
typedef struct isa_hazard *ISA_HAZARD;
ISA_HAZARD Hazard_Create( const char *name );
void Hazard_Group( TOP topcode, ... );
void Hazard_Type( ISA_HAZARD isa_hazard );
void Hazard_Data( int data );
void Hazard_Post_Ops( int ops );
void Hazard_Pre_Ops( int ops );
void Hazard_ISA( ISA_SUBSET isa_subset );
void ISA_Hazards_End(void);

#ifdef __cplusplus
}
#endif
#endif
