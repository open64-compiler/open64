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


#ifndef st2f_INCLUDED
#define st2f_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: st2f.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:43 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/st2f.h,v $
 *
 * Revision history:
 *    13-Apr-95 - Original Version
 *
 * Description:
 *
 *    ST2F_initialize:
 *       Should be called before any other routines in this module.
 *
 *    ST2F_finalize:
 *       Should be called after completed use of this module (i.e.
 *       just before whirl2f terminates).
 *
 *    ST2F_use_translate:
 *       Translate a variable reference.
 *
 *    ST2F_decl_translate:
 *       Translate a variable declaration
 *
 *    ST2F_deref_translate:
 *       Translate a pointer variable to its dereferenced version.
 *
 *    ST2F_func_header:
 *       Adds tokens for the function header (parameter declarations,
 *       function name, and return type) in a function definition. Note 
 *       that the resultant token buffer will not have appended a 
 *       newline after the function header.
 *
 *    ST2F_Use_Preg:
 *       Given an ST_type(), a preg offset, and a context, this function 
 *       appends the name of the preg to the given TOKEN_BUFFER.  The 
 *       preg will be declared as a side-effect of this call if it has 
 *       not yet been declared in the current context.  It is left to 
 *       the caller to cast the resultant value to ST_type(st) when:
 *       PUinfo_Preg_Type(ST_type(st)) != ST_type(st).
 *
 *    ST2F_Declare_Tempvar:
 *       Declares a tempvar with the given index in the local scope.
 *
 *    ST2F_FIRST_PARAM_IDX:
 *       An index to the first parameter to be explicitly declared in
 *       Fortran, given the type of the function and the type of the 
 *       first (implicit or explicit) argument.
 *
 * ====================================================================
 * ====================================================================
 */

extern void ST2F_initialize();
extern void ST2F_finalize();

extern void ST2F_use_translate(TOKEN_BUFFER tokens, ST *st);

extern void ST2F_decl_translate(TOKEN_BUFFER tokens, const ST *st);

extern void ST2F_deref_translate(TOKEN_BUFFER tokens, ST *st);
  
extern void ST2F_func_header(TOKEN_BUFFER tokens,
			     ST          *st,           /* Function ST entry */
			     ST         **params,       /* Array of parameters */
			     INT32        num_parms,    /* # of parms in params */
			     BOOL         is_altentry); /* Alternate entry */


extern void ST2F_Use_Preg(TOKEN_BUFFER tokens,
			  TY_IDX       preg_ty,
			  PREG_IDX     preg_idx);

extern void ST2F_Declare_Tempvar(TY_IDX ty, UINT idx);

//-------------------------------------

#define ST2F_FIRST_PARAM_IDX(funtype) \
   (Func_Return_To_Param(funtype)? \
    (Func_Return_Character(funtype)? 2 : 1) : 0)

#endif /* st2f_INCLUDED */

