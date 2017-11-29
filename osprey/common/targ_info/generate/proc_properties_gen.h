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


// proc_properties_gen.h
/////////////////////////////////////
//
//  Interface for specifying properties (attributes) for various
//  instructions in the PROC.
//
//  void PROC_Properties_Begin( const char* archname )
//      Initialize to generate properties information for the architecture 
//      with the given <archname>.  The information will be written to the 
//      files targ_proc_properties.[ch].  
//
//  TYPE PROC_PROPERTY
//      An abstract type that represents a property of a processor.
//      No client visible fields.
//
//  PROC_PROPERTY PROC_Property_Create( const char* name )
//      Used to create a new PROC_PROPERTY.  <name> is the property name.
//      It will be used to define a PROCESSOR_<name> access function.
//
//  void Processor_Group( PROC_PROPERTY property, ... )
//      Lists the processors with the given <property>. Subsequent arguments 
//      are PROCs, terminating in PROCESSOR_UNDEFINED.  
//
//  void PROC_Properties_End(void)
//      Complete processing of properties.
//
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/proc_properties_gen.h,v $

typedef struct proc_property *PROC_PROPERTY;

extern void PROC_Properties_Begin( const char* archname );
extern PROC_PROPERTY PROC_Property_Create( const char* name );
extern void Processor_Group( PROC_PROPERTY property, ... );
extern void PROC_Properties_End(void);
