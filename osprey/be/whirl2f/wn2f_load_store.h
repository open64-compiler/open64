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


#ifndef wn2f_load_store_INCLUDED
#define wn2f_load_store_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: wn2f_load_store.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:43 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/wn2f_load_store.h,v $
 *
 * Revision history:
 *    13-Apr-95 - Original Version
 *
 * Description:
 *
 * ====================================================================
 * ====================================================================
 */

extern void WN2F_Load_Store_initialize(void);
extern void WN2F_Load_Store_finalize(void);
      
extern WN2F_STATUS 
   WN2F_istore(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);
extern WN2F_STATUS 
   WN2F_istorex(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);
extern WN2F_STATUS 
   WN2F_mstore(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);
extern WN2F_STATUS 
   WN2F_stid(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_iload(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);
extern WN2F_STATUS 
   WN2F_iloadx(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);
extern WN2F_STATUS 
   WN2F_mload(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);
extern WN2F_STATUS 
   WN2F_ldid(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);
extern WN2F_STATUS 
   WN2F_lda(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);
extern WN2F_STATUS
   WN2F_array(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);
extern void
   WN2F_array_bounds(TOKEN_BUFFER tokens, 
		     WN *wn, 
		     TY_IDX array_ty,
		     WN2F_CONTEXT context) ;
extern void
   WN2F_String_Argument(TOKEN_BUFFER  tokens,
			WN           *base,
			WN           *length,
			WN2F_CONTEXT  context);


extern BOOL WN2F_Is_Address_Preg(WN * wn ,TY_IDX ptr_ty) ;

#endif /* wn2f_load_store_INCLUDED */

