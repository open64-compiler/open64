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


#ifndef whirl2f_common_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: whirl2f_common.c
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:43 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/whirl2f_common.h,v $
 *
 * Revision history:
 *  17-Apr-95 - Original Version
 *
 * Description:
 *
 * This file is to be included in all major translating modules
 * belonging to whirl2f.  It includes all files that these modules
 * will depend on and defines some utilities that are generally
 * useful.
 *
 *    Output files
 *    ------------
 *
 *      1) The Whirl2f_File is the file to which we would write the
 *         program units (subroutines/functions) in the high-level 
 *         output language.

 *      2) The Whirl2f_Srcpos_Map_File is the file into which we 
 *         will write the mapping between source positions in the
 *         original source and source positions in the whirl2f
 *         generated source.
 *
 *    Identifier Naming
 *    -----------------
 *       We provide some utilities for creating identifier names.  For
 *       names that are generated or altered by whirl2f, these utilities
 *       can be used to prepend a reserved whirl2f prefix (e.g. "w2f$").
 *       While this moves names outside of the user's name-space, it
 *       does not prevent prefixed names from being overloaded.
 *
 *       Note that this is all fine and dandy for local names, while
 *       some extra work is required by the caller of these routines
 *       for external names.  An external name ending in '_' should
 *       be converted into a name without the '_', while an external
 *       name not ending in a '_' should be suffixed by a '$'.  This
 *       will be left to the users of the routines provided here, since
 *       we do not know whether or not a name is external in this
 *       module.
 *
 *       WHIRL2F_number_as_name: Converts the given number into
 *          a valid Fortran identifier (prefixed by WHIRL2F_prefix).
 *
 *       WHIRL2F_ptr_as_name: Converts the given pointer value
 *           into a valid Fortran identifier.  Note that the number
 *           may be a 32 or 64 bits value, depending on the pointer
 *           representation and will be prefixed by WHIRL2F_prefix.
 *
 *       WHIRL2F_make_valid_name: If the given name is already a
 *          valid Fortran name, then it is simply returned.  If the name
 *          is NULL, then return NULL.  Otherwise, construct a valid
 *          Fortran identifier by removing invalid characters, thus
 *          returning a non-NULL name.
 *
 *    Comments and Parenthesis
 *    ------------------------
 *
 *       WHIRL2F_Append_Comment: Adds prefix_lines+1+postfix_lines 
 *          number of comment-lines to the given token buffer, where
 *          the given comment-string (if non-NULL) will be insterted
 *          on a separate line between the prefix and postifix at
 *          the current offset.  The last comment line is NOT terminated
 *          with a newline character!
 *
 *       WHIRL2F_Parenthesize:  Surround the current contents og the
 *          given buffer by parenthesis.
 *
 * ====================================================================
 * ====================================================================
 */


                     /* Common include files */
                     /*----------------------*/

#include "common_include.h"
#include "w2f_driver.h"

   /* Error checking during type accesses */
   /*-------------------------------------*/

#define W2F_TY_pointed(ty, msg) \
   (ASSERT_DBG_FATAL(TY_Is_Pointer(ty), (DIAG_W2F_EXPECTED_PTR, (msg))), \
    TY_pointed(ty))


   /* Ensure that we do not override Fortran specific interfaces */
   /*------------------------------------------------------------*/

#define Prepend_Idented_Newline DO_NOT_USE_THIS_FUNCTION
#define Append_Idented_Newline DO_NOT_USE_THIS_FUNCTION


                     /* Identifier naming */
                     /*-------------------*/

#define WHIRL2F_number_as_name(number) Number_as_String(number, "%lld")
   
#define WHIRL2F_ptr_as_name(ptr) Ptr_as_String(ptr)

extern const char * WHIRL2F_make_valid_name(const char *name, BOOL allow_dot);


                     /* Fortran comments */
                     /*------------------*/

extern void WHIRL2F_Append_Comment(TOKEN_BUFFER tokens,
				   const char   *cmt,
				   UINT          prefix_lines,
				   UINT          suffix_lines);

extern void WHIRL2F_Parenthesize(TOKEN_BUFFER tokens);

/* This variable is TRUE for Fortran 90 program units */

extern BOOL WN2F_F90_pu;

#endif /* whirl2f_common_INCLUDED */
