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


// proc_gen.h
/////////////////////////////////////
//
// Interface to create a new PROC (actually just an enum of all the processors).
//
//  void PROC_Create (const char *proc_name, ...);
//     Create an PROC with the <name>, The subsequent arguments are strings
//     that are the names of the instructions. The list is terminated by a 
//     NULL string.
//
//  The output of PROC_Create is to generate targ_proc.[ch], which define:
//
//     PROCESSOR stands for Target Processor; prefix is PROCESSOR.
//
//     enum PROCESSOR
//         Containins all the target processors.  Their names have the form 
//         PROCESSOR_<name>.
//
//     const PROCESSOR PROCESSOR_UNDEFINED
//         Useful value guaranteed not to be a valid PROCESSOR.
//
//     const int PROCESSOR_count
//         Gives the number of processors.
//
//     char* PROCESSOR_Name(PROCESSOR proc)
//         Returns the name of the given PROCESSOR.
//    
/////////////////////////////////////


//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/proc_gen.h,v $

extern void PROC_Create (const char *proc_name, ...);
