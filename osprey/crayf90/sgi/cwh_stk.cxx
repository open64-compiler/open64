/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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
 * Module: cwh_stk
 * $Revision: 1.6 $
 * $Date: 04/12/21 14:57:33-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: 
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: maintains a stack to evaluate expressions. Items on
 *              the stack are tagged as WN,ST,FLD (an offset) or 
 *              STRs(strings). The STR_items are NULL and just mark
 *              the length and address below. ADDR_items and DEREF_items
 *              are WN's that have been tagged because they were marked
 *              as an argument address or a deref of a dope vector respectively.
 *              The TY field is usually NULL, but in a few instances where
 *              the address of a FLD has been formed,  the TY of the FLD
 *              has to be preserved until later eg: to go into a PARM node.
 *              Logicals and small integer constants have TYs too.
 *
 * ====================================================================
 * ====================================================================
 */

static const char *source_file = __FILE__;

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/crayf90/sgi/SCCS/s.cwh_stk.cxx $ $Revision: 1.6 $";
#endif /* _KEEP_RCS_ID */


/* sgi includes */

#include "defs.h"
#include "symtab.h"
#include "wn.h"
#include "wn_util.h"
#include "ir_reader.h"

/* conversion includes */

#include "cwh_defines.h"
#include "cwh_stk.h"

typedef struct {
  void * item  ;
  TY_IDX it_ty ;
  enum   item_class Class;
} stk_entry_t ;

#define STK_SIZE_CHANGE 512
#define STK_EMPTY -1

static INT32 current_size = 0;
static stk_entry_t *stk=NULL;
static INT32 top = STK_EMPTY ;

/*===============================================
 *
 * cwh_stk_push
 *
 * Push an item onto the expression stack.
 *
 *===============================================
 */ 
extern void
cwh_stk_push( void * item, enum item_class Class)
{
  cwh_stk_push_typed(item,Class,0) ;

  return ;
}

/*===============================================
 *
 * cwh_stk_push_typed
 *
 * Push an item (WN/ADDR) on the expression stack.
 * This is used directly when the TY of a FLD is 
 * needed - the address of the FLD has been computed,
 * but the TY has to be preserved until later. 
 * 
 * It's used above, in the general case.
 *
 *===============================================
 */ 
extern void
cwh_stk_push_typed( void * item, enum item_class Class, TY_IDX ty)
{
  if (ty != 0) {
    DevAssert(((Class == ADDR_item) || 
	       (Class == WN_item)   || 
	       (Class == WN_item_whole_array) ||
	       (Class == ST_item)   || 
	       (Class == ST_item_whole_array)   || 
	       (Class == DEREF_item)),
	      (" Cannot type this item"));
  }
	     
  top ++ ;

  if (top >= current_size) {  /* More space for the stack ? */
     
     current_size += STK_SIZE_CHANGE;
     stk = (stk_entry_t *) realloc(stk,sizeof(stk_entry_t)*current_size);
  }

  stk[top].item  = item ;
  stk[top].Class = Class ;
  stk[top].it_ty = ty ; 

  return ;
}

/*===============================================
 *
 * cwh_stk_push_STR
 *
 * Push a STR_item onto the expression stack.
 * This is length, address, ty, and marker. 
 * The item_class is that of the address (ST/WN).
 *
 *===============================================
 */ 
extern void
cwh_stk_push_STR(void * len,void * addr, TY_IDX ty, enum item_class addr_class)
{
#ifdef KEY
  DevAssert((ty != 0),("STR missing TY"));
#else
  DevAssert((ty != NULL),("STR missing TY"));
#endif
  cwh_stk_push_typed(addr,addr_class,ty) ;
  cwh_stk_push(len,WN_item);      
  cwh_stk_push(NULL,STR_item);
}

/*===============================================
 *
 * cwh_stk_pop_WN
 *
 * Pop a WN from the expression stack.
 *
 *===============================================
 */ 
extern WN *
cwh_stk_pop_WN(void)
{

  DevAssert((top >= 0), ("Stack underflow"));
  DevAssert((stk[top].Class == WN_item || stk[top].Class == WN_item_whole_array), (" TOS is not WN"));

  stk[top].Class = UNDEF ;
  return ((WN *) stk[top--].item) ;
}

/*===============================================
 *
 * cwh_stk_pop_WHIRL
 *
 * Pop a WHIRL node from the expression stack.
 * This is used when it doesn't matter if it's
 * a WN/ADDR/DEREF and distinguishing the cases
 * would be tiresome.
 *
 *===============================================
 */ 
extern WN *
cwh_stk_pop_WHIRL(void)
{

  DevAssert((top >= 0), ("Stack underflow"));
  DevAssert((stk[top].Class == WN_item  ||
	     stk[top].Class == WN_item_whole_array ||
	     stk[top].Class == DEREF_item ||
	     stk[top].Class == ADDR_item), (" TOS is not WN"));

  stk[top].Class = UNDEF ;
  return ((WN *) stk[top--].item) ;
}

/*===============================================
 *
 * cwh_stk_pop_ST
 *
 * Pop an ST from the expression stack.
 *
 *===============================================
 */ 
extern ST *
cwh_stk_pop_ST(void)
{
  DevAssert((top >= 0), ("Stack underflow"));
  DevAssert((stk[top].Class == ST_item || stk[top].Class == ST_item_whole_array), (" TOS is not ST"));

  stk[top].Class = UNDEF ;  
  return ((ST *) stk[top--].item) ;

}

/*===============================================
 *
 * cwh_stk_pop_FLD
 *
 * Pop a FLD from the expression stack.
 *
 *===============================================
 */ 
extern FLD_IDX 
cwh_stk_pop_FLD(void)
{
  DevAssert((top >= 0), ("Stack underflow"));
  DevAssert((stk[top].Class == FLD_item), (" TOS is not FLD"));

  stk[top].Class = UNDEF ;  
  return ((FLD_IDX ) (INTPTR)stk[top--].item) ;
}

/*===============================================
 *
 * cwh_stk_pop_ADDR
 *
 * Pop a ADDR from the expression stack. This is
 * used as a marker for an address, where a WN 
 * isn't sufficient context. eg: don't want to
 * load the thing.
 *
 *===============================================
 */ 
extern WN *
cwh_stk_pop_ADDR(void)
{
  DevAssert((top >= 0), ("Stack underflow"));
  DevAssert((stk[top].Class == ADDR_item), (" TOS is not ADDR"));

  stk[top].Class = UNDEF ;  
  return ((WN *) stk[top--].item) ;
}

/*===============================================
 *
 * cwh_stk_pop_DEREF
 *
 * Pop a DEREF from the expression stack. This is
 * used as a marker for a dope vector dereference.
 * It's of interest in fei_nseq_subscr, but appears
 * in a few places where addresses do.
 *
 *===============================================
 */ 
extern WN *
cwh_stk_pop_DEREF(void)
{
  DevAssert((top >= 0), ("Stack underflow"));
  DevAssert((stk[top].Class == DEREF_item), (" TOS is not DEREF"));

  stk[top].Class = UNDEF ;  
  return ((WN *) stk[top--].item) ;
}

/*===============================================
 *
 * cwh_stk_pop_STR
 *
 * Pop an STR from the expression stack.
 *
 *===============================================
 */ 
extern void
cwh_stk_pop_STR(void)
{
  DevAssert((top-2 >= 0), ("Stack underflow"));
  DevAssert((stk[top].Class == STR_item), (" TOS is not STR"));
#ifdef KEY
  DevAssert((stk[top-2].Class != 0), (" STR missing TY"));
#else
  DevAssert((stk[top-2].Class != NULL), (" STR missing TY"));
#endif

  stk[top--].Class = UNDEF ;  
}

/*===============================================
 *
 * cwh_stk_pop_PCONST
 *
 * Pop a PCONST from the expression stack. This is
 * used only for bit string constants, during
 * static initialization. Not to be used elsewhere.
 *
 *===============================================
 */ 
extern ST *
cwh_stk_pop_PCONST(void)
{
  DevAssert((top >= 0), ("Stack underflow"));
  DevAssert((stk[top].Class == PCONST_item), (" TOS is not PCONST"));

  stk[top].Class = UNDEF ;  
  return ((ST *) stk[top--].item) ;
}

/*===============================================
 *
 * cwh_stk_pop_LBL
 *
 * Pop a LABEL_IDX from the expression stack. 
 *
 *===============================================
 */ 
extern LABEL_IDX
cwh_stk_pop_LB(void)
{
  DevAssert((top >= 0), ("Stack underflow"));
  DevAssert((stk[top].Class == LB_item), (" TOS is not LB"));

  stk[top].Class = UNDEF ;  
  return ((LABEL_IDX) (INTPTR)stk[top--].item) ;
}

/*===============================================
 *
 * cwh_stk_pop_whatever
 *
 * Pop whatever is TOS.
 *
 *===============================================
 */ 
extern void
cwh_stk_pop_whatever(void)
{
  DevAssert((top >= 0), ("Stack underflow"));

  stk[top--].Class = UNDEF ;

}

/*===============================================
 *
 * cwh_stk_get_class
 *
 * Find out what sort of item is TOS.
 *
 *===============================================
 */ 
extern enum item_class
cwh_stk_get_class(void)
{
  DevAssert((top >= 0), ("Stack underflow"));
  
  return (stk[top].Class) ;

}
/*===============================================
 *
 * cwh_stk_get_TY
 *
 * Find out TY at TOS.
 *
 *===============================================
 */ 
extern TY_IDX
cwh_stk_get_TY(void)
{
  DevAssert((top >= 0), ("Stack underflow"));
  
  return (stk[top].it_ty) ;

}

/*===============================================
 *
 * cwh_stk_get_FLD_TY
 *
 * Find out TY at for FLD item. It works for 
 * nested FLDs, because the top FLD is the innermost
 * (as written in the source eg: a%b%c%d - D is TOS).
 *
 *===============================================
 */ 
extern TY_IDX
cwh_stk_get_FLD_TY(void)
{
  FLD_IDX fld ;
  DevAssert((top >= 0), ("Stack underflow"));
  DevAssert((stk[top].Class == FLD_item), (" TOS is not FLD"));

  fld = (FLD_IDX ) (INTPTR)stk[top].item;
  return (FLD_type(FLD_HANDLE (fld)));
}

/*===============================================
 *
 * cwh_stk_count_STRs
 *
 * Count the STR_items in the n items at TOS
 *
 *===============================================
 */ 
extern INT32
cwh_stk_count_STRs(INT32 n)
{
  INT32 i = top;
  INT32 c = 0 ;


  while (n-- > 0) {
    DevAssert((i >= 0 ), (" stk miscount"));    

    if (stk[i].Class == STR_item) {
      c++ ;
      i -= 3 ;
    } else
      i -- ;
    
  }
  return (c) ;

}

/*===============================================
 *
 * cwh_stk_is_byte_STR
 *
 * is the STR item at depth n, a single byte?
 * 0 == TOS. Assumes all items above are STRs.
 * Says not a byte if the expression isn't a 
 * simple load - ie: no transformationals etc.
 * Utility routine for cwh_stmt_character_store.
 *
 *===============================================
 */ 
extern BOOL
cwh_stk_is_byte_STR(INT32 n)
{
  INT32 i = top;
  BOOL  res = FALSE;

  while (n-- > 0) {

    DevAssert((i >= 0 ), (" stk miscount"));    
    DevAssert((stk[i].Class == STR_item),(" all STRs?"));

    i -= 3 ;
  }

  DevAssert((stk[i].Class == STR_item),(" all STRs?"));
  i -- ;
  DevAssert((stk[i].Class == WN_item),("size?"));

  if (WN_operator((WN *)stk[i].item) == OPR_INTCONST) {
    if (WN_const_val((WN *)stk[i].item) == 1) {

      /* does this look like a simple load? */

      i --;
      if ((stk[i].Class == ST_item) ||
	  (stk[i].Class == ST_item_whole_array) ||
	  (stk[i].Class == FLD_item))
	res = TRUE;

      else if ((stk[i].Class == WN_item) ||
	       (stk[i].Class == WN_item_whole_array) ||
	       (stk[i].Class == DEREF_item)) {

	WN * wn = (WN *)stk[i].item ;
	OPERATOR op = WN_operator(wn);
      
	if (op == OPR_ARRAYEXP) {
	  wn = WN_kid0(wn);
	  op = WN_operator(wn);
        }

	if (op == OPR_INTRINSIC_OP && WN_intrinsic(wn) == INTRN_CHAR) {
	  wn = WN_kid0(WN_kid0(wn)); /* skip PARM */
	  op = WN_operator(wn);
	}
      
	res = (op == OPR_ARRAY) || (op == OPR_ARRSECTION) ||
	      (op == OPR_LDID)  || (op == OPR_ILOAD);
      }
    }
  }
  return (res) ;
}

/*===============================================
 *
 * cwh_stk_verify_empty
 *
 * check to see nothing is left on the stack.
 * and to clear it if so..
 *
 *===============================================
 */ 
extern void
cwh_stk_verify_empty(void)
{
  DevAssert((top == STK_EMPTY), ("Stack not empty"));

  top = STK_EMPTY ;

}

/*===============================================
 *
 * cwh_stk_dump
 *
 * Dump the stack.
 *
 *===============================================
 */ 

extern void
cwh_stk_dump(void)
{
  static const char * str_name[] = {
    " ???? ",
    "WN_item",
    "ADDR_item",
    "DEREF_item",
    "FLD_item",
    "ST_item",
    "PCONST_item",
    "STR_item - length & address below",
    "ST_item_whole_array",
    "WN_item_whole_array",
    "LB_item"
  };
  
  static const char * null_str = "   <null>\n";
  int i,j ;
  
  printf ("\n Stack --- \n\n");

  for (i = top ; i >= 0 ; i-- ) {
    switch(stk[i].Class) {
    case WN_item:    j = 1;  break;
    case ADDR_item:  j = 2;  break;
    case DEREF_item: j = 3;  break;
    case FLD_item:   j = 4;  break;
    case ST_item:    j = 5;  break;
    case PCONST_item:j = 6;  break;
    case STR_item:   j = 7;  break;
    case ST_item_whole_array:  j=8;  break;
    case WN_item_whole_array:  j=9;  break;
    case LB_item            : j=10;  break;
    default:         j = 0;  break;

    }

    if (stk[i].it_ty == 0)
      printf("%s\n",str_name[j]);
    else 
      printf("%s    TY = 0x%x \n",str_name[j],stk[i].it_ty) ;

    switch(stk[i].Class) {
    case WN_item: 
    case WN_item_whole_array:
    case ADDR_item: 
    case DEREF_item: 
      if (stk[i].item == NULL)
	printf("%s",null_str);
      else
	DUMP_WN((WN *)stk[i].item) ;
      break ;
	
    case PCONST_item:
    case ST_item: 
    case ST_item_whole_array:
      if (stk[i].item == NULL)
	printf("%s",null_str);
      else
	DUMP_ST((ST *)stk[i].item) ;
      break ;
	
    case FLD_item: 
      if (stk[i].item == NULL)
	printf("%s",null_str);
      else {
	FLD_HANDLE f ((FLD_IDX )(INTPTR)stk[i].item);
	printf ("%s: offset: %lld \n",FLD_name(f), FLD_ofst(f));
      }
      break;

    case LB_item:
      if (stk[i].item == NULL)
	printf("%s",null_str);
      else
	DUMP_LB((LABEL_IDX)(INTPTR)stk[i].item);
      break;

    case STR_item: 
      break ;
	
    }
  }
}

/*===============================================
 *
 * cwh_stk_pop_iostmt
 *
 * Pop the IO operands from the stack to make a 
 * whirl node representing an IO statement. If
 * an err=, end= or eor= clause was specified, there
 * might be an extra ST at the bottom of the stack to
 * hold the return status. We pass this along as an
 * IO_ITEM called "RETURN_TEMP" .
 *
 *===============================================
 */ 
extern WN *
cwh_stk_pop_iostmt ( IOSTATEMENT opc , INT32 eeeflag)
{
  INT   i, j;
  WN  * wn;
  INT top_adjustment = 0;
  INT num_items = 0;
  INT bottom;

  for(i=top; i>=0; i--) {
     if (stk[i].Class == WN_item || stk[i].Class == WN_item_whole_array) {
       wn = (WN *)stk[i].item;
       if ((wn == NULL) || (WN_operator_is(wn, OPR_IO_ITEM))) {
	  top_adjustment++;
       } else {
	  break;
       }
     } else {
       break;
     }
  }

  bottom = top - top_adjustment + 1;

  for(i = bottom; i <= top; i++ )
    if (stk[i].item != NULL)
      num_items++;


  wn = WN_CreateIo ( opc, num_items);

  for ( i = bottom, j=0; i <= top;) {
    if (stk[i].item != NULL) {
      WN_kid(wn,j++) = (WN *)stk[i++].item ;
    } else {
      i++;
    }
  }

  top = top - top_adjustment;

  return wn;
}

/*===============================================
 *
 * cwh_stk_get_num_inquire_items
 *
 * Returns the number of iolist items for this
 * Inquire(iolength) statement.
 *
 *===============================================
 */

extern INT32
cwh_stk_get_num_inquire_items(void)
{
  INT32 i;
  WN *wn;
  INT32 num_items = 0;

  for(i=top; i>=0; i--) {
     if (stk[i].Class == WN_item || stk[i].Class == WN_item_whole_array) {
	wn = (WN *)stk[i].item;
	if (wn == NULL) {
	   cwh_stk_pop_WN();
	   break;
        } else if (WN_operator_is(wn, OPR_IO_ITEM)) {
	   num_items++;
        } else {
	   break;
        }
     } else {
       break;
     }
  }
  return num_items;
}

/*===============================================
 *
 * cwh_stk_push_top_item_again
 *
 * Push the top item again on the stack
 *
 *===============================================
 */
extern void
cwh_stk_push_top_item_again(void)
{
   INT fld_count,i;

   switch (stk[top].Class) {
    case ST_item:
    case ST_item_whole_array:
    case LB_item:
       cwh_stk_push_typed(stk[top].item,stk[top].Class,stk[top].it_ty) ;
       break;

    case ADDR_item:   
    case DEREF_item:  
    case WN_item:     
    case WN_item_whole_array:
    case PCONST_item: 
       /* Need to duplicate these first */
       cwh_stk_push_typed(WN_COPY_Tree((WN *)stk[top].item),stk[top].Class,stk[top].it_ty) ;
       break;

    case STR_item:    
       /* Need to get the three items, then duplicate them */
       /* This copies the addr */
       if (stk[top-2].Class == ST_item_whole_array ||
	   stk[top-2].Class == ST_item) {
	  cwh_stk_push_typed(stk[top-2].item,stk[top-2].Class,stk[top-2].it_ty) ;
       } else {
	  cwh_stk_push_typed(WN_COPY_Tree((WN *)stk[top-2].item),stk[top-2].Class,stk[top-2].it_ty) ;
       }
       /* This copies the len */
       cwh_stk_push_typed(WN_COPY_Tree((WN *)stk[top-2].item),stk[top-2].Class,stk[top-2].it_ty) ;
       
       /* This copies the STR */
       cwh_stk_push_typed(stk[top-2].item,stk[top-2].Class,stk[top-2].it_ty) ;
    case FLD_item:    
       fld_count = 0;
       while ((stk[top-fld_count].Class == FLD_item) && fld_count <= top) {
	  ++fld_count;
       }
       /* fld_count contains the number of FLD items to duplicate */
       /* First, duplicate the base address underlying the field */
       if (stk[top-fld_count].Class == ST_item_whole_array ||
	   stk[top-fld_count].Class == ST_item) {
	  cwh_stk_push_typed(stk[top-fld_count].item,stk[top-fld_count].Class,
			     stk[top-fld_count].it_ty) ;
       } else {
	  cwh_stk_push_typed(WN_COPY_Tree((WN *)stk[top-fld_count].item),stk[top-fld_count].Class,
			     stk[top-fld_count].it_ty) ;
       }
       /* Now duplicate all the FLDs */
       for (i = 0; i < fld_count; i++) {
	  cwh_stk_push_typed(stk[top-fld_count].item,stk[top-fld_count].Class,
			     stk[top-fld_count].it_ty) ;
       }
       break;

    default:
       DevAssert((0), (" unduplicatable stack item"));    
       break;
   }

   return ;
}



/*===============================================
 *
 * char * cwh_stk_fld_name
 *
 * get field names from top of the stack
 *
 *===============================================
 */
extern char *
cwh_stk_fld_name(void)
{
   INT i;
   char *r;
   char *s, *fname;

   r = (char *) malloc(1);
   *r = 0;

   for (i=top; i >= 0; i--) {
      if (stk[i].Class != FLD_item) break;
      /* Add the field name at the beginning of the r string */
      
      if (stk[i].item != NULL) {
	 fname = FLD_name(FLD_HANDLE ((FLD_IDX)(INTPTR)stk[i].item));
	 s = (char *) malloc(strlen(r) + strlen(fname) + 2) ;
	 sprintf(s,"%%%s%s",fname,r);
	 free(r);
	 r = s;
      }
   }
   
   return (r);
}
  
