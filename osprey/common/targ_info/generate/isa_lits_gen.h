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


// isa_lits_gen.h
/////////////////////////////////////
//
// Interface to create literal classes.
//
//  void ISA_Lits_Begin(void)
//      Initialize to generate literal class information for an architecture.
//      The information will be written to the files targ_isa_lits.[ch].  
//
//  TYPE LIT_RANGE
//      An abstract type that represents a literal range.
//      No client visible fields.
//
//  const LIT_RANGE LIT_RANGE_END
//	A special LIT_RANGE value that is used to mark the end of a
//	list of LIT_RANGE parameters.
//
//  LIT_RANGE ISA_Create_Lit_Range(const char *name, 
//				   long long min, long long max)
//	Create a literal range named <name> (used for debugging/informational
//	purposes only) with minimum value <min> and maximum value <max>.
//
//  LIT_RANGE SignedBitRange(unsigned int bit_size)
//  LIT_RANGE UnsignedBitRange(unsigned int bit_size)
//	Create a signed/unsigned literal range for a bit-field of size
//	<bit_size>.
//
//  TYPE (enum) LIT_CLASS_TYPE
//	An enumeration of the types of literals: eg, UNSIGNED or SIGNED.
//
//  void ISA_Create_Lit_Class (const char *name, LIT_CLASS_TYPE type, ...)
//	Create a LIT CLASS with the <name> and type <type>. The variable
//	argument list contains a sequence of LIT_RANGE parameters to
//	identify one or more disjoint ranges that are legal for the
//	literal class. The list is ended with a parameter of LIT_RANGE_END.
//
//  void ISA_Lits_End(void)
//      Complete processing of literals.
//
/////////////////////////////////////

//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_lits_gen.h,v $

#ifndef isa_lits_gen_INCLUDED
#define isa_lits_gen_INCLUDED

#ifdef _KEEP_RCS_ID
static const char isa_lits_gen_rcs_id[] = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_lits_gen.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
  UNSIGNED,
  SIGNED
} LIT_CLASS_TYPE;

typedef struct lit_range *LIT_RANGE;

#define LIT_RANGE_END ((LIT_RANGE)0)

extern void ISA_Lits_Begin (void);

extern LIT_RANGE ISA_Create_Lit_Range(const char *name, 
				      long long min, 
				      long long max);

extern LIT_RANGE SignedBitRange(unsigned int bit_size);
extern LIT_RANGE UnsignedBitRange(unsigned int bit_size);

extern void ISA_Create_Lit_Class (const char *name, LIT_CLASS_TYPE type, ...);

extern void ISA_Lits_End (void);

#ifdef __cplusplus
}
#endif
#endif /* isa_lits_gen_INCLUDED */
