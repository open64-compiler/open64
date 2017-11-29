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


#ifndef st2c_INCLUDED
#define st2c_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: st2c.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:59-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.st2c.h $
 *
 * Revision history:
 *  07-Oct-94 - Original Version
 *
 * Description:
 *
 *    ST2C_initialize:
 *    ST2C_finalize:
 *       This will initialize and terminate usage of this module().
 *       This must be done after the call to TY2C_initialize.
 *
 *    ST2C_decl_translate:
 *       Declares a variable, function, or constant for the given ST 
 *       in C and appends the declaration to the tokens.  Any other 
 *       kinds of STs will result in an assertion failure.
 *
 *    ST2C_weakext_translate:
 *       Declares a variable or function, attributed as being weak,
 *       followed by a #pragma weak directive.  Note that the tokens
 *       added by this call should not be followed by a semicolon ';'.
 *
 *    ST2C_use_translate:
 *       Translates an ST into a variable, constant, sym_const, 
 *       or function reference and appends the tokens to the given
 *       TOKEN_BUFFER.  Note that pregs and labels must be handled
 *       independently in wn2c.c. The "name" may consist of more than
 *       an identifier for sym_consts.
 *
 *    ST2C_Use_Preg:
 *       Given an ST_type(), a preg offset, and a context, this function 
 *       appends the name of the preg to the given TOKEN_BUFFER.  The 
 *       preg will be declared as a side-effect of this call if it has 
 *       not yet been declared in the current context.  It is left to 
 *       the caller to cast the resultant value to ST_type(st) when:
 *       PUinfo_Preg_Type(ST_type(st)) != ST_type(st).
 *
 *    ST2C_func_header:
 *       Adds tokens for the function header (parameter declarations,
 *       function name, and return type) in a function definition. Note 
 *       that the resultant token buffer will not have appended a 
 *       newline after the function header.
 *
 *    ST2C_Declare_Tempvar:
 *       Declares a tempvar with the given index in the local scope.
 *
 *    ST2C_New_Common_Block:
 *       Denotes the given ST as a Fortran common block and marks it
 *       as having been declared to avoid a declaration in the current
 *       local PU scope.  This should be called for all common blocks
 *       in a local scope prior to translating the enclosing PU 
 *       (i.e. function/procedure) body ... it is essential to getting
 *       the variable references right.
 *
 *    ST2C_Define_Common_Blocks:
 *       Declares all common blocks in the compilation unit.  Usually
 *       done shortly before ST2C_finalize().
 *
 * ====================================================================
 * ====================================================================
 */

extern void ST2C_initialize(CONTEXT context);
extern void ST2C_finalize(void);

extern void ST2C_decl_translate(TOKEN_BUFFER tokens,
                                const ST    *st,
                                CONTEXT      context);
extern void ST2C_weakext_translate(TOKEN_BUFFER tokens, 
				   const ST    *st, 
				   CONTEXT      context);
extern void ST2C_use_translate(TOKEN_BUFFER tokens,
                               const ST    *st,
                               CONTEXT      context);

extern void ST2C_Use_Preg(TOKEN_BUFFER tokens, 
			  TY_IDX       preg_ty, 
			  PREG_IDX     preg_idx, 
			  CONTEXT      context);

extern void ST2C_func_header(TOKEN_BUFFER  tokens, 
			     const ST     *st,     /* ST for function */
			     ST          **params, /*list of formal parms */
			     CONTEXT       context);

extern void ST2C_Declare_Tempvar(TY_IDX ty, UINT idx);

extern void ST2C_New_Common_Block(const ST *st);
extern void ST2C_Define_Common_Blocks(TOKEN_BUFFER tokens, CONTEXT context);


#endif /* st2c_INCLUDED */

