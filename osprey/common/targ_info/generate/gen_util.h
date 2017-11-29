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


// gen_util.h
////////////////////////////////////
//
// Utility functions to help in generating .c and .h files.
// These functions are only to be used by the generators.
//
//  void Emit_Header (FILE *hfile, 
//		      const char *name, 
//		      const char * const *interface_desc)
//     Write out the standard h file header to <hfile>. The basename
//     of the header being created is specified by <name>. An optional
//     interface description is specified by <interface_desc> (pass NULL
//     if it is not desired).
//
//  void Emit_Footer (FILE *file)
//     Write out the standard h file footer to <hfile>.
//
//  void Emit_Definitions (FILE *hfile, const char *prefix)
//     Write out client specified definitions (specified with Define_xxx)
//     to <hfile>. The definition names will all be prefxed by <prefix>.
//
////////////////////////////////////

//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/gen_util.h,v $

#ifndef gen_util_INCLUDED
#define gen_util_INCLUDED

#include "gen_util_gen.h"

#ifdef __MINGW32__
#define LL_FORMAT "I64"
#else
#define LL_FORMAT "ll"
#endif /* __MINGW32__ */

#ifdef __cplusplus
extern "C" {
#endif

extern void Emit_Header (FILE *hfile, 
			 const char *name, 
			 const char * const *interface_desc);
extern void Emit_Footer (FILE *hfile);
extern void Emit_Definitions (FILE *hfile, const char *prefix);

#ifdef __cplusplus
}
#endif
#endif /* gen_util_INCLUDED */
