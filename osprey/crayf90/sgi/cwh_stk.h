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


/* ====================================================================
 * ====================================================================
 *
 * Module: cwh_stk.h
 * $Revision$
 * $Date$
 * $Author$
 * $Source$
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Exports global functions in cwh_stk.c and the 
 *              definition of stack items.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef CWH_STK_INCLUDED
#define CWH_STK_INCLUDED

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

enum item_class {
  UNDEF,
  ST_item,             /* item is an ST*  */
  ST_item_whole_array, /* item is an ST* and represents a whole array */
  WN_item,             /* item is a  WN*  */
  WN_item_whole_array, /* item is a  WN* and represents a whole array */
  FLD_item,            /* item is a  FLD* */
  STR_item,            /* marker for string length and address */
  ADDR_item,           /* item is WN, address */
  DEREF_item,          /* item is an address, which must be deref'ed */
  PCONST_item,         /* item is the address (in the compiler's world) of a byte string */
  LB_item              /* item is a label IDX */
} ;


extern void cwh_stk_push( void * item, enum item_class);
extern void cwh_stk_push_STR(void * len,void * addr, TY_IDX  ty, enum item_class addr_class) ;
extern void cwh_stk_push_typed( void * item, enum item_class Class, TY_IDX  ty) ;

extern WN  * cwh_stk_pop_WHIRL(void) ;
extern WN  * cwh_stk_pop_WN(void) ;
extern ST  * cwh_stk_pop_ST(void) ;
extern FLD_IDX  cwh_stk_pop_FLD(void) ;
extern LABEL_IDX  cwh_stk_pop_LB(void) ;
extern void  cwh_stk_pop_STR(void) ;
extern WN  * cwh_stk_pop_ADDR(void) ;
extern WN  * cwh_stk_pop_DEREF(void) ;
extern ST  * cwh_stk_pop_PCONST(void) ;
extern WN   *cwh_stk_pop_iostmt(IOSTATEMENT, INT32);
extern void  cwh_stk_pop_whatever(void) ;

extern TY_IDX   cwh_stk_get_TY(void);
extern TY_IDX   cwh_stk_get_FLD_TY(void);
extern enum item_class cwh_stk_get_class(void) ;
extern void cwh_stk_dump(void) ;
extern INT32 cwh_stk_count_STRs(INT32 n) ;
extern INT32 cwh_stk_get_num_inquire_items(void);
extern void cwh_stk_push_top_item_again(void);
extern void cwh_stk_verify_empty(void) ;
extern char * cwh_stk_fld_name(void);  /* return a string for the field on top of the stack */
extern BOOL cwh_stk_is_byte_STR(INT32 n); /* is STR n, a byte? */


#endif /* CWH_STK_INCLUDED */

