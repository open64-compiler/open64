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


#ifndef wn2c_pragma_INCLUDED
#define wn2c_pragma_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: wn2c_pragma.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:42:00-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.wn2c_pragma.h $
 *
 * Revision history:
 *    12-Aug-95 - Original Version
 *
 * Description:
 *
 *    WN2C_pragma:
 *       Translates a pragma present in a body of statements.
 *
 *    WN2C_pragma_list_begin:
 *       Translates the list of pragmas associated with PU or a
 *       region.
 *
 *    WN2C_pragma_list_end:
 *       Completes the translation initiated with a call to 
 *       WN2C_pragma_list_begin(), after the body of the PU or region
 *       has been translated.
 *
 * ====================================================================
 * ====================================================================
 */

extern BOOL
   WN2C_Skip_Pragma_Stmt(const WN *wn);

extern STATUS 
   WN2C_pragma(TOKEN_BUFFER tokens, const WN *wn, CONTEXT context);


STATUS 
   WN2C_pragma_list_begin(TOKEN_BUFFER tokens,
                          const WN    *first_pragma,
                          CONTEXT      context);

STATUS 
   WN2C_pragma_list_end(TOKEN_BUFFER tokens, 
                        const WN    *first_pragma,
                        CONTEXT      context);

BOOL
   Ignore_Synchronized_Construct(const WN     *construct_pragma,  
				 CONTEXT context);

#endif /* wn2c_pragma_INCLUDED */


