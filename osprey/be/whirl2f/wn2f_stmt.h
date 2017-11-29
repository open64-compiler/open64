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


#ifndef wn2f_stmt_INCLUDED
#define wn2f_stmt_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: wn2f_stmt.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:42 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/wn2f_stmt.h,v $
 *
 * Revision history:
 *    13-Apr-95 - Original Version
 *
 * Description:
 *
 *    WN2F_Stmt_Initialize:  
 *       First thing to do in the handler for a func_entry, which
 *       will setup the state for statement translation.
 *
 *    WN2F_Stmt_Finalize:
 *       Last thing we need to do.
 *
 * ====================================================================
 * ====================================================================
 */

extern void WN2F_Stmt_initialize(void);
extern void WN2F_Stmt_finalize(void);

/* Some statements can be skipped, based on previous analysis of
 * call and return sites (e.g. storing to/from return registers).
 */
extern BOOL WN2F_Skip_Stmt(WN *stmt);

extern WN2F_STATUS 
   WN2F_block(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_region(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_compgoto(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_do_loop(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_implied_do(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);


extern WN2F_STATUS 
   WN2F_do_while(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_while_do(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_if(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_goto(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_agoto(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_condbr(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_return(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_return_val(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_label(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_istore(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_intrinsic_call(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_call(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_prefetch(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern WN2F_STATUS 
   WN2F_eval(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

extern void
   WN2F_Append_Block_Data(TOKEN_BUFFER t);

#endif /* wn2f_stmt_INCLUDED */


