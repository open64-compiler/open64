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


#ifndef linklist_INCLUDED
#define linklist_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif
/* ====================================================================
 * ====================================================================
 *
 * Module: linklist.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:17:57 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/linklist.h,v $
 *
 * Revision history:
 *  01-Feb-89 - Original Version
 *  26-May-91 - Integrated from Josie
 *
 * Description:
 *
 * This file contains definitions of linked list data structures and
 * macros that manipulate them.  Both simple linked lists and arrays
 * of simple linked lists are defined.  Macros that manipulate arrays
 * of linked lists are always prefaced with 'ARY_'.  These lists are
 * singly-linked,  i.e. they have a 'next' pointer, but not a 'prev'
 * pointer.
 *
 * The definitions are set up for linked lists of 32-bit integers.
 * Additional sets of macros may be defined to extend the definitions
 * to apply to linked lists of any objects that can be represented
 * in 32 bits or less.  The files "bb_lst.h" and "tn_lst.h" contain
 * examples of these extentions.  E.g. "bb_lst.h" defines names that
 * can be used to work with linked lists of BB numbers.  It builds
 * these names from the generic linked-list names defined in this file.
 *
 * This file defines types, macros and functions for linked list
 * objects.  The types defined are:
 *    tlst_val ... a data value in a linked list (it is a 32-bit object)
 *    LST_ITM  ... item in a linked list (data val and ptr to next item)
 *    LNK_LST  ... a header of a single linked list
 *    LNK_LST_ARY ...  a header of an array of linked lists
 *
 *
 * A variety of macros and functions are defined in this file to
 * manipulate linked lists.  Some of these are low-level utilities
 * used only internally by the linked list package.  Those that are
 * meant to be visible externally, are described in the table, below.
 * In the table descriptions, the following variable names are used
 * as arguments to the functions and macros.
 *
 * Macro and function parameters:
 *    l   ... ptr of a LNK_LST object (header of a single list)
 *    a   ... ptr of a LNK_LST_ARY object (header of an array of lists)
 *    i   ... index into the array of lists
 *    n   ... an integer (e.g. the number of elements in the array
 *    s   ... storage for recursively traversing lists (ptr to LST_ITM)
 *    msg ... a printf-style character string message
 *
 *
 * An 'M' in the left column of the following table indicates the
 * operation is implemented as a macro, rather than a function.  Some
 * examples of how these macros/functions are used appear to manipulate
 * the linked lists are given after the table.
 *
 * Initialization, allocation, and freeing:
 * ----------------------------------------
 * M  void Init_List(l)          Initialize pointers and count for
 *                               list 'l'
 *    void Free_List(l)          Free all items in the list 'l'
 * M  void ARY_Del_List(a,i)     Delete (Free) the list 'a[i]'
 *    void ARY_Init_List(a,n)    Allocate and init an array of 'n' lists
 *    void ARY_Free_List(a)      Free an array of lists, 'a'
 *
 *
 * List queries:
 * -------------
 * M  INT32 LST_Len(l)             The number of items in the list 'l'
 *    BOOL Item_In_List(l,v)       Check if item 'v' is in list 'l'
 * M  BOOL LST_Empty(l)            Check to see if the list is empty
 * M  INT32 ARY_LST_Len(a,i)       For the list a[i], the number of
 *                                 items in it
 *    BOOL ARY_Item_In_List(a,i,v) Check to see if item 'v' is in
 *                                 list 'a[i]'
 * M  INT32 ARY_LST_n_elems(a)     The number of lists in the array of
 *                                 lists 'a'
 *    BOOL Validate_List(l)	   Verify non-cyclic, correct count.
 *
 *
 * Stepping through lists:
 * -----------------------
 * M  tlst_val First_Item(l)       Get the value of the first item in
 *                                 the list 'l'
 * M  tlst_val Next_Item(l)        Get the value of the next item in the
 *                                 list 'l'
 * M  tlst_val ARY_First_Item(a,i) Get val of first item in list 'a[i]'
 * M  tlst_val ARY_Next_Item(a,i)  Get val of next item in list 'a[i]'
 *
 *
 * Stepping through lists, using explicit storage to remember position:
 * --------------------------------------------------------------------
 * M  tlst_val First_Item_Strg(l,s)       Get the value of the first
 *                                        item in the list 'l'
 * M  tlst_val Next_Item_Strg(l,s)        Get the value of the next
 *                                        item in the list 'l'
 * M  tlst_val ARY_First_Item_Strg(a,i,s) For the list a[i], get val of
 *                                        first item
 * M  tlst_val ARY_Next_Item_Strg(a,i,s)  For the list a[i], get val of
 *                                        next item
 *
 *
 * Testing the values of list items:
 * ---------------------------------
 * M  BOOL Valid_Item(v)        Check to see if the 'v' is valid
 * M  BOOL Invalid_Item(v)      Check to see if the 'v' is invalid
 *
 *
 * Adding/Deleting items:
 * ----------------------
 *    BOOL Add_Item(l,v)             Append value 'v' to list 'l' (if it
 *                                   is not already there)
 *    BOOL Add_Item_Validate(l,v)    Add_Item with Validate_List
 *    void Add_Item_Dup(l,v)         Append value 'v' to 'l' (even if
 *                                   it is already there)
 *    void Add_First_Item(l,v)       Insert value 'v' to beginning of
 *                                   list 'l'
 *    BOOL Del_Item(l,v)             Delete value 'v' from list 'l'
 *    BOOL Del_Cur_Item(l)	     Delete "current" item from list 'l'
 * M  BOOL ARY_Add_Item(a,i,v)       Append value 'v' to list 'a[i]' (if
 *                                   not already there)
 * M  BOOL ARY_Add_Item_Validate(a,i,v)
 * M  void ARY_Add_Item_Dup(a,i,v)   Append value 'v' to list 'a[i]'
 *                                   (even if it is already there)
 * M  void ARY_Del_Item(a,i,v)       Delete value 'v' from list 'a[i]'
 * M  void ARY_Del_Cur_Item(a,i)     Delete current item from list 'a[i]'
 * M  void ARY_Add_First_Item(a,i,v) Insert value 'v' to beginning of
 *                                   list 'a[i]'
 *
 *
 * Some miscellaneous routines:
 * ----------------------------
 *    void List_Print(l,msg,...)      Print message 'msg' followed by
 *                                    list 'l'
 *    void ARY_List_Print(a,msg,...)  Print message 'msg' followed by
 *                                    array of lists
 *    void Free_All_List_Items()      Free blocks of mememory allocated
 *                                    for linked lists
 *
 * Examples:
 *    -- to create (declare) a simple linked list object and a pointer
 *       to a list object, use:
 *         LNK_LST lst, *lst_ptr;
 *
 *    -- to initialize a list to be empty:
 *         List_Init(&lst);
 *               or
 *         List_Init(lst_ptr);
 *
 *    -- to create a header for an array of linked lists (and a
 *       pointer to an array), use:
 *         LNK_LST_ARY Succ, *Succ_Ptr;
 *
 *    -- to allocate and initialize the array of list headers for, say,
 *       'BB_Count' elemnts (i.e. an array of 'BB_Count' lists), use:
 *         ARY_Init_List(&Succ,BB_Count);
 *
 *    -- to add BB number 'bbno' to the end of a simple list, use:
 *         Add_Item(lst_ptr,bbno);
 *       (Note: if 'bbno' is already in the list, it will not be added.
 *        Use 'Add_Item_Dup()', if duplicate list entries are desired.)
 *
 *    -- to insert BB number 'bbno' at the beginning of a simple
 *       list, use:
 *         Add_First_Item(lst_ptr,bbno);
 *       (Note: this adds 'bbno' regardless of whether it is already in
 *        the list.)
 *
 *    -- to add BB number 'bbno' to the end of the i'th list in
 *       the array of lists 'Succ', use:
 *         ARY_Add_Item(&Succ,i,bbno);
 *
 *    -- to obtain the number of lists in the 'Succ' array of
 *       lists, use:
 *         num_lists = ARY_LST_n_elems(&Succ);
 *
 *    -- to obtain the number of items in a simple list, use:
 *         num_items = LST_Len(&lst);
 *
 *    -- to obtain the number of items in the i'th list in an array
 *       of lists named 'Succ', use:
 *         num_items = ARY_LST_Len(&Succ,i);
 *
 *    -- to check if BB number 'bbno' is in the list the 'lst', use:
 *       number of which is given by cur_bbnum:
 *         if (Item_In_List(&lst,bbno)) {
 *           ................
 *           ................
 *         }
 *
 *    -- to check if BB number 'bbno' is in the i'th element of the
 *       array of lists named 'Succ', use:
 *         if (ARY_Item_In_List(&Succ,i,bbno)) {
 *           ................
 *           ................
 *         }
 *
 *    -- to delete the item containing BB number 'bbno' from the
 *       i'th element of the array of lists named 'Succ', use:
 *         ARY_Del_Item(&Succ,i,bbno);
 *
 *    -- to loop over all the BB numbers 'bbno' in a simple list 'lst':
 *         for (bbno=First_Item(&lst); Valid_Item(bbno);
 *                                            bbno=Next_Item(&lst)) {
 *           ................
 *           ................
 *         }
 *
 *    -- to loop over all the BB numbers 'bbno' in the i'th list of
 *       the array of lists named 'Succ', use:
 *         for (bbno=ARY_First_Item(&Succ,i); Valid_Item(bbno);
 *                                       bbno=ARY_Next_Item(&Succ,i)) {
 *           ................
 *           ................
 *         }
 *
 *
 * NOTE: In the above examples, Basic Block numbers were used as the
 * list data values.  In a real program, we would not use the generic
 * list objects defined here.  Instead, we would use the specific
 * instances of the list objects define in "bb_lst.h".
 * As a rule, we should never see the simple references given in the
 * above examples; we should always see a specific instance.  For
 * example, rather than:
 *        bbno = ARY_First_Item(&Succ,i);
 * we would have:
 *        bbno = BB_ARY_First_Item(&Succ,i);
 * (see "bb_lst.h")
 *
 * ====================================================================
 * ====================================================================
 */


/* Define a flag that enables some linked-list sanity checking: */
#ifdef Is_True_On
# define LNK_LST_CHECK
#endif /* Is_True_On */

/* Include erglob.h to make sure Is_True is available: */
#ifdef LNK_LST_CHECK
# include "erglob.h"
#endif

/*
 *  Defining STORE_LIST_LEN causes the length of the list to be stored
 *  directly in the linked list data structure.  This allows list length
 *  determination to be trivial.  If STORE_LIST_LEN is not defined, the
 *  calculation of a list length is done by a function.  We can
 *  experiment with the tradeoff between space (to store the list length
 *  directly) and time (to compute the list length), by changing the
 *  define STORE_LIST_LEN.
 *
 *  Note: all dependencies on STORE_LIST_LEN are in this file.
 *        Alternating between the two modes of list length determination
 *        is done by defining or un-defining STORE_LIST_LEN right here.
 */
#define STORE_LIST_LEN

/* data values in lists are 32-bit quantities: */
typedef mINT32 tlst_val;

typedef struct lst_itm {    /* linked list items */
  tlst_val val;               /* the data value of the item */
  struct lst_itm *next;       /* ptr to the next item of data */
} LST_ITM;

typedef struct lnk_lst {      /* linked list header */
#ifdef STORE_LIST_LEN
  mINT32 len;         /* the number of data items in the list */
#endif      /* STORE_LIST_LEN */
  LST_ITM *nxt;        /* next list item */
  LST_ITM *first;      /* ptr to the first data item in the list */
} LNK_LST;

typedef struct lnk_lst_ary {        /* array of linked lists */
  mINT32 n_elems;  /* number of elements in the array of linked lists */
  LNK_LST *lists;  /* ptr to array linked list headers */
} LNK_LST_ARY;

extern void ARY_Init_List(LNK_LST_ARY *ary, INT32 n_elems);
extern void Free_List(LNK_LST *lst);
extern void ARY_Free_List(LNK_LST_ARY *ary);
extern BOOL Validate_List ( LNK_LST *lst );
extern BOOL Add_Item ( LNK_LST *lst, tlst_val val );
extern BOOL Add_Item_Validate ( LNK_LST *lst, tlst_val val);
extern void Add_Item_Dup(LNK_LST *lst, tlst_val val);
extern void Add_First_Item(LNK_LST *lst, tlst_val val);
extern BOOL Add_Ordered_Item(LNK_LST *lst, tlst_val val);
extern void Add_Ordered_Item_Dupl(LNK_LST *lst, tlst_val val);
extern BOOL Del_Item(LNK_LST *lst, tlst_val val);
extern BOOL Del_Cur_Item(LNK_LST *lst);
extern BOOL Item_In_List(LNK_LST *lst, tlst_val val);
extern BOOL ARY_Item_In_List(LNK_LST_ARY *ary, INT32 i, tlst_val val);
extern void Free_All_List_Items(void);
extern void List_Print (
#ifndef NO_VARARGS_PROTOTYPES
  LNK_LST *lst, char *msg, ...
#endif
  );
extern void ARY_List_Print (
#ifndef NO_VARARGS_PROTOTYPES
  LNK_LST_ARY *ary, char *msg, ...
#endif
  );

/*
 *  Here are the macros that manipulate the above linked list data
 *  structures.  We declare a temporary '_tmp_lst' that will be 'static'
 *  in each file that includes this file -- i.e., we will get multiple
 *  copies of it.  That is OK (and possibly desireable).  We use this
 *  only to hold very temproary information (we could completely
 *  eliminate it), to avoid re-evaluating some index operations.  By
 *  making it 'static' (rather than a simple global) it may be possible
 *  for some C compilers to better optimize references to it.
 *
 *  Note that we define the null list item 'NULL_ITEM' to be the
 *  value '-1'.  This limits the generality with which these data
 *  structures/macros can be used and applied.  Specifically, if '-1'
 *  is a valid data value, then these macros cannot be used.  This lack
 *  of generality is a result of the technique with which we manipulate
 *  the lists.  We never deal with the pointers directly, we just use
 *  macros that return data values, and one of the data values is
 *  reserved to be the end marker.
 *
 */
extern LST_ITM *_lst_itm;

#ifdef STORE_LIST_LEN
#  define LST_Len(lst)      ((lst)->len)
#  define incr_LST_len(l)   ( ++((l)->len) )
#  define decr_LST_len(l)   ( --((l)->len) )
#  define clr_LST_len(l)    ((l)->len = 0)
#else /* STORE_LIST_LEN not defined */
#  ifdef PROTOTYPE
     extern INT32 LST_Len(LNK_LST *lst);
#  else                /* PROTOTYPE checking not supported */
     extern INT32 LST_Len();
#  endif /* PROTOTYPE checking not supported */
#  define incr_LST_len(l)   /* no-op */
#  define decr_LST_len(l)   /* no-op */
#  define clr_LST_len(l)    /* no-op */
#endif       /* STORE_LIST_LEN not defined */

/* null list item is -1 -- this limits generality */
#define NULL_ITEM ((tlst_val) -1)
#define Valid_Item(val)     (((tlst_val) (val)) != NULL_ITEM)
#define Invalid_Item(val)   (((tlst_val) (val)) == NULL_ITEM)
#define LST_val(itm)        ((itm)->val)
#define LST_next(itm)       ((itm)->next)
#define LST_first(lst)      ((lst)->first)
#define LST_nxt(lst)        ((lst)->nxt)
#define LST_Empty(lst)      ( LST_first(lst) == NULL )
#define ARY_LST_n_elems(a)  ((a)->n_elems)
#define LST_lists(a)        ((a)->lists)

#ifdef LNK_LST_CHECK
   extern char *_ary_lst_bounds_msg;
#  define _ary_lst_bnds_chk(a,i) \
	Is_True ( ((i) >= 0) && ((i)<ARY_LST_n_elems(a)), \
		(_ary_lst_bounds_msg, (i), (ARY_LST_n_elems(a)-1) ) )
#else	/* LNK_LST_CHECK not defined */
#  define _ary_lst_bnds_chk(a,i) ((void) 1)
#endif	/* LNK_LST_CHECK not defined */

#define ARY_LST_Elem(a,i)   ( _ary_lst_bnds_chk((a),(i)), \
				(((a)->lists)+(i)) )
#define First_Item(l)       ( (_lst_itm = LST_first(l)) ? \
                                 ( (LST_nxt(l) = LST_next(_lst_itm)), \
				   LST_val(_lst_itm) ) : NULL_ITEM )
#define Next_Item(l)        ( (_lst_itm = LST_nxt(l)) ? \
                                 ( (LST_nxt(l) = LST_next(_lst_itm)), \
                                      LST_val(_lst_itm) ) : NULL_ITEM )
/* "Fast" forms of above, for simple, readonly scans of a list.
 * There must be no modifications to the list (additions or deletions)
 * during the scan.  Notice that Next_Item_Fast does not use 'l' and
 * Valid_Item_Fast does not use 'v'; they are there for interface
 * consistency.  The Fast versions of First_Item and Next_Item do
 * not return the item; they only control list traversal.  The user
 * of the Fast macros must do a Get_Item_Fast to get the value. */
#define First_Item_Fast(l,t) ( (t) = LST_first(l) )
#define Next_Item_Fast(l,t)  ( (t) = LST_next(t)  )
#define Valid_Item_Fast(v,t) ( t )
#define Get_Item_Fast(l,t)   ( LST_val(t) )
#define ARY_First_Item(a,i) ( First_Item(ARY_LST_Elem(a,i)) )
#define ARY_Next_Item(a,i)  ( Next_Item(ARY_LST_Elem(a,i)) )
#define ARY_Del_List(a,i)   Free_List(ARY_LST_Elem(a,i))
#define First_Item_Strg(l,s)       ( (s = _lst_itm = LST_first(l)) ? \
                                        ((s = LST_next(s)), \
					LST_val(_lst_itm)) : NULL_ITEM )
/* 
 *  NOTE: 'l' is not used in the following macro -- it is here for
 *  to provide an inteface that appears consistent to the outside world.
 */
#define Next_Item_Strg(l,s)        ( (_lst_itm = s) ? \
                                        ((s = LST_next(s)), \
					LST_val(_lst_itm)) : NULL_ITEM )
#define ARY_First_Item_Strg(a,i,s) ( First_Item_Strg(ARY_LST_Elem(a,i),s) )
#define ARY_Next_Item_Strg(a,i,s)  ( Next_Item_Strg(ARY_LST_Elem(a,i),s) )
#define ARY_LST_Len(a,i)    LST_Len(ARY_LST_Elem(a,i))
#define ARY_Add_Item(a,i,v) Add_Item(ARY_LST_Elem(a,i),v)
#define ARY_Add_Item_Validate(a,i,v) Add_Item_Validate(ARY_LST_Elem(a,i),v)
#define ARY_Add_Item_Dup(a,i,v)   Add_Item_Dup(ARY_LST_Elem(a,i),v)
#define ARY_Add_First_Item(a,i,v) Add_First_Item(ARY_LST_Elem(a,i),v)
#define ARY_Add_Ordered_Item(a,i,v) \
			Add_Ordered_Item(ARY_LST_Elem(a,i),v)
#define ARY_Add_Ordered_Item_Dupl(a,i,v) \
			Add_Ordered_Item_Dupl(ARY_LST_Elem(a,i),v)
#define ARY_Del_Item(a,i,v)       Del_Item(ARY_LST_Elem(a,i),v);
#define ARY_Del_Cur_Item(a,i)     Del_Cur_Item(ARY_LST_Elem(a,i));
#ifdef STORE_LIST_LEN
#  define Init_List(l)      ( (LST_nxt(l) = LST_first(l) = NULL), \
                                        clr_LST_len(l) )
#else      /* STORE_LIST_LEN not defined */
#  define Init_List(l)      ( LST_nxt(l) = LST_first(l) = NULL )
#endif      /* STORE_LIST_LEN not defined */

#ifdef __cplusplus
}
#endif
#endif /* linklist_INCLUDED */
