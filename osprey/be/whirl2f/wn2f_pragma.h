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


#ifndef wn2f_pragma_INCLUDED
#define wn2f_pragma_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: wn2f_pragma.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:43 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/wn2f_pragma.h,v $
 *
 * Revision history:
 *    12-Aug-95 - Original Version
 *
 * Description:
 *
 *    WN2F_pragma:
 *       Translates a pragma present in a body of statements.
 *
 *    WN2F_pragma_list_begin:
 *       Translates the list of pragmas associated with PU or a
 *       region.
 *
 *    WN2F_pragma_list_end:
 *       Completes the translation initiated with a call to 
 *       WN2F_pragma_list_begin(), after the body of the PU or region
 *       has been translated.
 *
 * ====================================================================
 * ====================================================================
 */

extern BOOL
   WN2F_Skip_Pragma_Stmt(WN *wn);

extern WN2F_STATUS 
   WN2F_pragma(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);


WN2F_STATUS 
   WN2F_pragma_list_begin(TOKEN_BUFFER tokens,
                          WN          *first_pragma,
                          WN2F_CONTEXT context);

WN2F_STATUS 
   WN2F_pragma_list_end(TOKEN_BUFFER tokens, 
                        WN          *first_pragma,
                        WN2F_CONTEXT context);

BOOL
Ignore_Synchronized_Construct(WN          *construct_pragma,  
			      WN2F_CONTEXT context);

#endif /* wn2f_pragma_INCLUDED */


