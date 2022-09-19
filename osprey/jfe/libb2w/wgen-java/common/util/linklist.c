/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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



#include "defs.h"

#include "erglob.h"
#include "tracing.h"
#define NO_VARARGS_PROTOTYPES
#include "linklist.h"
#undef NO_VARARGS_PROTOTYPES

char *_ary_lst_bounds_msg = "ARY LST index = %d, out of bounds: 0..%d";

/*
 *  Define these macros in case we ever decide to put an additional
 *  filter on memory for linked lists data.  We never call 'malloc()'
 *  or 'free()' directly in this file -- we always go through these
 *  macros.
 */
#define lnk_lst_malloc(x) ((MEM_PTR)malloc(x))
#define lnk_lst_free(x)   free((MEM_PTR) (x))

/*
 *  This data structure links together blocks of list items that have
 *  been allocated.  We have to save pointers to each allocated block
 *  so we can free all the list items at the end of a program unit.
 */
typedef struct block_list_items {
  MEM_PTR block;                  /* ptr to block of memory alloced */
  struct block_list_items *next;  /* ptr to next block header */
} BLK_LST_ITMS;

#define BLK_block(blk)  ((blk)->block)
#define BLK_next(blk)   ((blk)->next)

LST_ITM * _lst_itm;  /* Definition added for ANSI C 4/19/92 (jfw) */

static BLK_LST_ITMS *block_item_hdr = NULL;

/*
 *  Allocate N_LIST_BLOCK linked list data items per block.
 *  We make this number just under some nice power of two, because we
 *  assume that blocks with a size that is a power of 2 (or just under
 *  a power of 2) are allocated fairly efficiently.  We don't make the
 *  block exactly a power of 2, because we must allocate an extra few
 *  bytes of overhead to keep a linked list of the blocks of memory
 *  allocated -- thus allowing us to free things up eventually (see the
 *  BLK_LST_ITMS data structure, above).
 */
#define N_LIST_BLOCK (2048 - 5)

/*
 *  Memory Blocks Size:  the total number of bytes of memory allocated
 *  by every call to 'list_malloc()', see below.
 *  This should be a power of 2 (or slightly less than a power of 2).
 */
#define M_BLOCK_SIZE \
		  ((sizeof(LST_ITM)*N_LIST_BLOCK)+sizeof(BLK_LST_ITMS))

static LST_ITM *list_items = NULL;
static LST_ITM *_list_tmp;

static LST_ITM *list_malloc(void);

/* ====================================================================
 *
 *  LST_ITM *item_alloc()   (macro)
 *
 *  This macro removes a list item from the free list and returns a
 *  pointer to the removed item.  If the free list is empty, it
 *  allocates a block of items, links that block together onto the
 *  free list, and returns one of the newly allocated items.
 *
 *  NOTE: THIS IS THE ONLY WAY THAT A LIST ITEM SHOULD BE ALLOCATED.
 *
 * ====================================================================
 */
#define item_alloc()   ( list_items ? \
                        ( (_list_tmp = list_items), \
                        (list_items = LST_next(list_items)), \
                        (LST_next(_list_tmp) = NULL), _list_tmp ) \
                                                   : list_malloc() )

/* ====================================================================
 *
 *  item_free()   (macro)
 *
 *  This macro places a list item onto the free list.
 *
 *  NOTE: THIS IS THE ONLY WAY THAT A LIST ITEM SHOULD BE FREED.
 *
 * ====================================================================
 */
#ifdef LNK_LST_CHECK
# define item_free(itm) ( (LST_next(itm)=list_items), \
				(list_items=(itm)), (LST_val(itm)=0) )
#else	/* LNK_LST_CHECK not defined */
# define item_free(itm) ((LST_next(itm)=list_items), (list_items=(itm)))
#endif	/* LNK_LST_CHECK not defined */

#ifndef MONGOOSE_BE
/* ====================================================================
 *
 *  static LST_ITM *list_malloc()
 *
 *  Allocate a block of memory for N_LIST_BLOCK linked list items and
 *  for a BLK_LST_ITMS element.  Link this block up with any previously
 *  allocated blocks.  Also, link all but one of of the N_LIST_BLOCK
 *  elements together and put them on the free list.
 *
 *  Return a pointer to the one item that was not put on the free list
 *  (and NULL out the 'LST_next()' field of the returned item).
 *
 *  NOTE:  THIS ROUTINE ASSUMES THE FREE LIST IS EMPTY.  THE ONLY CALLS
 *         TO THIS ROUTINE SHOULD BE FROM THE MACRO 'item_alloc()' WHEN
 *         THERE ARE NO FREE ELEMENTS REMAINING.
 * ====================================================================
 */

static LST_ITM *
list_malloc ( void )
{
  MEM_PTR mem_block;
  BLK_LST_ITMS *blk;
  register LST_ITM *itm;
  register INT32 i;

  /*
   *  The free list had better be empty.
   */
  Is_True (list_items == NULL, ("list_malloc: free list is not empty"));

  if ( (mem_block = lnk_lst_malloc(M_BLOCK_SIZE)) == NULL )
    ErrMsg ( EC_No_Mem, "list_malloc" );

  /*
   *  Link this block up with any previously allocated blocks of
   *  list items.
   */
  blk = (BLK_LST_ITMS *) mem_block;
  BLK_block(blk) = mem_block;  /* it points to itself! */
  BLK_next(blk) = block_item_hdr;
  block_item_hdr = blk;

  /*
   *  Link (N_LIST_BLOCK-1) elements together and place them on the free
   *  list, making sure the last one on the free list points to NULL.
   *  Take the one remaining item, NULL out its pointer, and return it.
   */
  list_items = (LST_ITM *) ++blk;
  for (itm=list_items, i=0; i<(N_LIST_BLOCK-2); ++i, ++itm) {
    LST_val(itm) = -1;
    LST_next(itm) = itm+1;
  }
  LST_next(itm) = NULL;     /* "ground" the end of the free list */

  ++itm;     /* 'itm' now points to the one remaining item */
  LST_next(itm) = NULL;

  return itm;
}
#endif /* MONGOOSE_BE */

#ifdef Is_True_On
/* ====================================================================
 *
 *  check_linked_list_free
 *
 *  This routine is called only for compile-time checking.  At the point
 *  of call, we are about to free all the memory ever allocated for
 *  linked list elements.  Here, we walk through the entire free list,
 *  counting number of elements we have, and make sure that we have
 *  exactly the number that were allocated in total.  If they are the
 *  same, we just return TRUE.  If they differ, we print a diagnostic
 *  and return FALSE.
 *
 *  NOTE: this routine will not be called in a production compiler.
 *  If it finds we have not freed all the items it means we have a
 *  memory leak somewhere.  This is bad, but it is not a valid reason
 *  to abort a user's compilation.
 *
 * ====================================================================
 */

static void
check_linked_list_free ( void )
{
  INT32 n_total = 0;
  INT32 n_in_free = 0;
  INT32 n_blocks = 0;
  BLK_LST_ITMS *blk;
  LST_ITM *item = list_items;

  for (blk=block_item_hdr; blk!=NULL; blk=BLK_next(blk), ++n_blocks)
    n_total += N_LIST_BLOCK;

  for (item=list_items; item!=NULL; item=LST_next(item), ++n_in_free)
    /* NULL statement list -- just counting items */ ;

  if ( n_total != n_in_free ) {
    DevWarn("WARNING: %d linked list items allocated in %d"
		" blocks, but %d in free list",
		n_total, n_blocks, n_in_free);
    ErrMsg ( EC_Mem_Leak, "Free_All_List_Items" );	/* Warning */
  }
}
#endif    /* Is_True_On */

/* ====================================================================
 *
 *  void Free_All_List_Items()
 *
 *  Step through the blocks of memory that were allocated for all the
 *  list items and free them up.
 *
 *  This function should be called once for each program unit, after all
 *  linked list manipulation is complete.
 *
 * ====================================================================
 */

void
Free_All_List_Items ( void )
{
  BLK_LST_ITMS *blk, *nblk;

/* We've had more than a year to decide we cared enough about this to
 * track it down and we haven't.  There's a bug filed.  I'm sick of
 * the stupid/cute message.  Enable the check if you plan to do
 * something about it and not until!    -- Rutt
 */ 
 
#if 0
# ifdef Is_True_On
  check_linked_list_free();
# endif
# endif

  for (blk=block_item_hdr; blk!=NULL; blk=nblk) {
    nblk = BLK_next(blk);
    (void) lnk_lst_free(blk);
  }

  list_items = NULL;
  block_item_hdr = NULL;
}

#ifndef MONGOOSE_BE
/* ====================================================================
 *
 *  void ARY_Init_List(LNK_LST_ARY *ary, INT32 n_elems)
 *
 *  Allocate an array of 'n_elems' linked list headers for the array of
 *  lists 'ary'.  Initialize each list.
 *
 * ====================================================================
 */

void
ARY_Init_List ( LNK_LST_ARY *ary, INT32 n_elems )
{
  register INT32 i;
  register LNK_LST *lst;

  Is_True (n_elems >= 1,
     ("ARY_Init_List: attempt to allocate array of size %d", n_elems) );

  if ((lst=(LNK_LST *)lnk_lst_malloc(sizeof(LNK_LST)*n_elems)) == NULL)
    ErrMsg ( EC_No_Mem, "ARY_Init_List" );

  LST_lists(ary) = lst;
  ARY_LST_n_elems(ary) = n_elems;

  for (i=0; i<n_elems; ++i, ++lst)
    Init_List( lst );
}
/* ====================================================================
 *
 *  void Free_List(LNK_LST *lst)
 *
 *  Free all the items in the linked list 'lst' (i.e., put the items on
 *  the free list).  Set the length of the list to 0.
 *
 * ====================================================================
 */

void
Free_List ( LNK_LST *lst )
{
  register LST_ITM *p1, *p2;

  for (p1=LST_first(lst); p1!=NULL; p1=p2) {
    p2 = LST_next(p1);
    item_free(p1);
  }

  Init_List(lst);
}

/* ====================================================================
 *
 *  void ARY_Free_List(LNK_LST_ARY *ary)
 *
 *  Free each list in the array of linked lists 'ary'.  Also, free up
 *  the linked list headers.
 *
 * ====================================================================
 */

void
ARY_Free_List ( LNK_LST_ARY *ary )
{
  INT32 n_elems = ARY_LST_n_elems(ary);
  INT32 i;

  for (i=0; i<n_elems; ++i)
    Free_List ( ARY_LST_Elem(ary,i) );

  (void) lnk_lst_free(LST_lists(ary));     /* free the headers */
}

/* ====================================================================
 *
 * Validate_List
 *
 * Validate a list, i.e. verify that it contains no cycles and that its
 * stored length matches its actual length.
 *
 * ====================================================================
 */

BOOL
Validate_List ( LNK_LST *lst )
{
  INT32 count = LST_Len(lst);
  LST_ITM *p;

  for ( p=LST_first(lst); p!=NULL; p=LST_next(p) ) {
    if ( --count < 0 ) break;
  }
  if ( count != 0 ) {
    DevWarn("##### Validate_List: Invalid count #####" );
    count = 10*LST_Len(lst);
    for ( p=LST_first(lst); p!=NULL; p=LST_next(p) ) {
	if ( --count < 0 ) break;
	DevWarn("  %p: %d", p, LST_val(p) );
    }
    FmtAssert ( FALSE,
		( "Validate_List: invalid count %d", LST_Len(lst) ) );
    return FALSE;
  }
  return TRUE;
}
#endif /* MONGOOSE_BE */

#ifndef STORE_LIST_LEN
/* ====================================================================
 *
 *  INT32 LST_Len(LNK_LST *lst)
 *
 *  Count the number of elements in the list and return that value.
 *
 *  Note that this is implemented as a routine only if the length is
 *  not stored directly in the list data structures.  If the length
 *  is stored there, this operation is implemented as a macro.  See
 *  the discussion in "linklist.h" regarding STORE_LST_LEN.
 *
 * ====================================================================
 */

INT32
LST_Len ( LNK_LST *lst )
{
  register INT32 count = 0;
  register LST_ITM *p;

  for (p=LST_first(lst); p!=NULL; p=LST_next(p))
    ++count;

  return count;
}
#endif      /* STORE_LIST_LEN not defined */

#ifndef MONGOOSE_BE
/* ====================================================================
 *
 *  BOOL Add_Item(LNK_LST *lst, tlst_val val)
 *
 *  Add the item 'val' to the linked list 'lst'.  If the item is already
 *  in the list, do not add it again.
 *  Update the number of items in the list.
 *
 *  Return TRUE if the item was added, FALSE if not.
 *
 * ====================================================================
 */

BOOL
Add_Item ( LNK_LST *lst, tlst_val val)
{
  register LST_ITM *p, *last;

  if (LST_Empty(lst)) {
    LST_first(lst) = p = item_alloc();
    LST_val(p) = val;
    incr_LST_len(lst);
    return TRUE;
  }

  last = NULL;	/* unnecessary except to eliminate warning msg */
  for (p=LST_first(lst); p!=NULL; p=LST_next(p)) {
    if (LST_val(p) == val)
      return FALSE;    /* already in the list => return FALSE */
    last = p;
  }

  /*
   *  If we get to here, we went through the list without finding a
   *  matching item.  Append a new item to the end of the list.
   */
  LST_next(last) = p = item_alloc();
  LST_val(p) = val;
  incr_LST_len(lst);

  /*
   *  If the pointer to the next item in the list is NULL, i.e. when
   *  stepping through the list the end was reached, then make the next
   *  item be this new item.
   */
  if (LST_nxt(lst) == NULL)
    LST_nxt(lst) = p;

  return TRUE;
}

/* ====================================================================
 *
 * Add_Item_Validate
 *
 * Identical to Add_Item, plus verification of the list afterwards.
 *
 * ====================================================================
 */

BOOL
Add_Item_Validate ( LNK_LST *lst, tlst_val val)
{
  BOOL added = Add_Item ( lst, val );

  (void) Validate_List ( lst );
  return added;
}

/* ====================================================================
 *
 *  void Add_Item_Dup(LNK_LST *lst, tlst_val val)
 *
 *  Add the item 'val' to the linked list 'lst', even if the item is
 *  already in the list.
 *  Update the number of items in the list.
 *
 * ====================================================================
 */

void
Add_Item_Dup ( LNK_LST *lst, tlst_val val )
{
  LST_ITM *p;
  LST_ITM *last;


  if (LST_Empty(lst)) {
    LST_first(lst) = p = item_alloc();
    LST_val(p) = val;
    incr_LST_len(lst);
    return;
  }

  /*
   *  Find the end of the list.
   */
  last = NULL;	/* unnecessary except to eliminate warning msg */
  for (p=LST_first(lst); p!=NULL; p=LST_next(p))
    last = p;

  /*
   *  Append a new item to the end of the list.
   */
  LST_next(last) = p = item_alloc();
  LST_val(p) = val;
  incr_LST_len(lst);

  /*
   *  If the pointer to the next item in the list is NULL, i.e. when
   *  stepping through the list the end was reached, then make the next
   *  item be this new item.
   */
  if (LST_nxt(lst) == NULL)
    LST_nxt(lst) = p;

#ifdef BB_VALIDATE_LIST
  Validate_List ( lst );
#endif

  return;
}

/* ====================================================================
 *
 *  void Add_First_Item(LNK_LST *lst, tlst_val val)
 *
 *  Add the item 'val' to the beginning of linked list 'lst'.
 *  Update the number of items in the list.
 *
 *  NOTE:  The item is added even if it is already in the list.
 *
 * ====================================================================
 */

void
Add_First_Item ( LNK_LST *lst, tlst_val val )
{
  register LST_ITM *p;

  p = item_alloc();
  LST_val(p) = val;
  LST_next(p) = LST_first(lst);
  LST_first(lst) = p;
  incr_LST_len(lst);
}

/* ====================================================================
 *
 *  BOOL Add_Ordered_Item(LNK_LST *lst, tlst_val val)
 *
 *  Add the item 'val' to the ordered linked list 'lst'.  If the item is
 *  already in the list, do not add it again.
 *  Update the number of items in the list.
 *
 *  Note: The list *must* be ordered and may not contain any duplicates
 *  for this routine to work properly.
 *
 *  Return TRUE if the item was added, FALSE if not.
 *
 * ====================================================================
 */

BOOL
Add_Ordered_Item ( LNK_LST *lst, tlst_val val )
{
  register LST_ITM *p, *last;
  register tlst_val this_val;

  if (LST_Empty(lst)) {
    LST_first(lst) = p = item_alloc();
    LST_val(p) = val;
    incr_LST_len(lst);
    return TRUE;
  }

  p = LST_first(lst);
  if ( (this_val = LST_val(p)) == val ) {
    return FALSE;    /* already in the list => return FALSE */

  } else if ( val < this_val ) {  /* insert at beginning of the list */
    register LST_ITM *new = item_alloc();
    LST_next(new) = p;
    LST_first(lst) = new;
    LST_val(new) = val;
    incr_LST_len(lst);
    return TRUE;
  }

  last = p;
  for ( p=LST_next(p); p!=NULL; p=LST_next(p)) {
#ifdef LNK_LST_CHECK
    Is_True ( this_val < LST_val(p),
      ("ordered list not sorted: elems %d and %d",this_val,LST_val(p)));
#endif	/* LNK_LST_CHECK */
    if ( (this_val = LST_val(p)) == val ) {
      return FALSE;    /* already in the list => return FALSE */

    } else if ( val < this_val ) {  /* insert here */
      register LST_ITM *new = item_alloc();
      LST_next(new) = p;
      LST_next(last) = new;
      LST_val(new) = val;
      incr_LST_len(lst);
      return TRUE;
    }
    last = p;
  }

  /*
   *  If we get to here, we went through the list without finding a
   *  matching item, and all items in the list have values less than
   *  the new value.  Append a new item to the end of the list.
   */
  LST_next(last) = p = item_alloc();
  LST_val(p) = val;
  incr_LST_len(lst);

  /*
   *  If the pointer to the next item in the list is NULL, i.e. when
   *  stepping through the list the end was reached, then make the next
   *  item be this new item.
   */
  if (LST_nxt(lst) == NULL)
    LST_nxt(lst) = p;

  return TRUE;
}

/* ====================================================================
 *
 *  void Add_Ordered_Item_Dupl(LNK_LST *lst, tlst_val val)
 *
 *  Add the item 'val' to the ordered linked list 'lst'.  If the item is
 *  already in the list, add a duplicate.
 *  Update the number of items in the list.
 *
 *  Note: The list *must* already be ordered for this routine to
 *  work properly.
 *
 * ====================================================================
 */

void
Add_Ordered_Item_Dupl ( LNK_LST *lst, tlst_val val )
{
  register LST_ITM *p, *last;
  register tlst_val this_val;

  if (LST_Empty(lst)) {
    LST_first(lst) = p = item_alloc();
    LST_val(p) = val;
    incr_LST_len(lst);
    return;
  }

  p = LST_first(lst);
  this_val = LST_val(p);

  if ( val <= this_val ) {  /* insert at beginning of the list */
    register LST_ITM *new = item_alloc();
    LST_next(new) = p;
    LST_first(lst) = new;
    LST_val(new) = val;
    incr_LST_len(lst);
    return;
  }

  last = p;
  for ( p=LST_next(p); p!=NULL; p=LST_next(p)) {
#ifdef LNK_LST_CHECK
    Is_True ( this_val <= LST_val(p),
      ("ordered list not sorted: elems %d and %d",this_val,LST_val(p)));
#endif	/* LNK_LST_CHECK */

    this_val = LST_val(p);
    if ( val <= this_val ) {  /* insert here */
      register LST_ITM *new = item_alloc();
      LST_next(new) = p;
      LST_next(last) = new;
      LST_val(new) = val;
      incr_LST_len(lst);
      return;
    }
    last = p;
  }

  /*
   *  If we get to here, we went through the list without finding a
   *  matching item, and all items in the list have values less than
   *  the new value.  Append a new item to the end of the list.
   */
  LST_next(last) = p = item_alloc();
  LST_val(p) = val;
  incr_LST_len(lst);

  /*
   *  If the pointer to the next item in the list is NULL, i.e. when
   *  stepping through the list the end was reached, then make the next
   *  item be this new item.
   */
  if (LST_nxt(lst) == NULL)
    LST_nxt(lst) = p;
}

/* ====================================================================
 *
 *  BOOL Del_Item(LNK_LST *lst, tlst_val val)
 *
 *  Delete the item 'val' from the linked list 'lst' (and free the
 *  deleted item).
 *  Update the number of items in the list.
 *
 *  Return TRUE if the item was deleted, FALSE if not (ie it wasn't
 *  there).
 *
 *  NOTE:
 *	If the item being deleted is the 'next' item in the list
 *	then make the 'next' item in the list the one AFTER the
 *	item being deleted.
 *
 * ====================================================================
 */

BOOL
Del_Item ( LNK_LST *lst, tlst_val val )
{
  register LST_ITM *p, *last;

  if (LST_Empty(lst))
    return FALSE; /* not in the list => nothing to delete */

  last = NULL;
  for (p=LST_first(lst); p!=NULL; p=LST_next(p)) {

    if (LST_val(p) == val) {

      if (LST_nxt(lst) == p) /* item being deleted is 'next' item */
	LST_nxt(lst) = LST_next(p);

      if (last == NULL)     /* it was the first item in the list */
        LST_first(lst) = LST_next(p);
      else      /* it was not the first item in the list */
        LST_next(last) = LST_next(p);

      item_free(p);       /* put deleted item into the free list */
      decr_LST_len(lst);
      return TRUE; /* it was deleted from the list => return TRUE */

    }

    last = p;
  }

  /*
   *  If we get to here, we went through the list searching for the item
   *  but could not find it.  Just return FALSE.
   */
  return FALSE;
}

/* ====================================================================
 *
 *  BOOL Del_Cur_Item(LNK_LST *lst)
 *
 *  Delete the "current" item from the linked list 'lst' (and free the
 *  deleted item).  Note that the current item is, by definition, the
 *  item whose LST_next(item) field the same as the LST_nxt(lst) field
 *  of the list-header.  Therefore, this routine can only be called
 *  when the LST_nxt(lst) field is valid -- i.e., only when stepping
 *  through the list.
 *
 *  Update the number of items in the list.
 *
 *  Return TRUE if there was an item that was deleted, FALSE if not
 *  (i.e., FALSE when the list was empty on input).
 *
 * ====================================================================
 */

BOOL
Del_Cur_Item ( LNK_LST *lst )
{
  LST_ITM *p, *last;
  LST_ITM *next_item = LST_nxt(lst);

  if ( LST_Empty(lst) )
    return FALSE; /* not in the list => nothing to delete */

  last = NULL;
  for ( p=LST_first(lst); p!=NULL; p=LST_next(p) ) {

    if ( LST_next(p) == next_item ) {

      if ( last == NULL )	/* it was the first item in the list */
        LST_first(lst) = LST_next(p);
      else      /* it was not the first item in the list */
        LST_next(last) = LST_next(p);

      item_free ( p );	/* put deleted item into the free list */
      decr_LST_len(lst);
      return TRUE;	/* it was deleted from the list => return TRUE */

    }

    last = p;
  }

  /* If we get to here, we went through the list, but we did not find
   *  an item with an LST_next field matching the next item of the list.
   */
  return FALSE;
}

/* ====================================================================
 *
 *  BOOL Item_In_List(LNK_LST *lst, tlst_val val)
 *
 *  Check to see if the item 'val' is in the linked list 'lst'.
 *
 *  Return TRUE if the item is in the list, FALSE if not.
 *
 * ====================================================================
 */

BOOL
Item_In_List ( LNK_LST *lst, tlst_val val )
{
  register LST_ITM *p;

  if (LST_Empty(lst))
    return FALSE;       /* empty list => not in the list */

  for (p=LST_first(lst); p!=NULL; p=LST_next(p))
    if (LST_val(p) == val)
      return TRUE; /* found it */

  /*
   *  If we get to here, we went through the list searching for the item
   *  but could not find it.  Just return FALSE.
   */
  return FALSE;
}

/* ====================================================================
 *
 *  BOOL ARY_Item_In_List(LNK_LST_ARY *ary, INT32 i, tlst_val val)
 *
 *  Check to see if the item 'val' is in the linked list 'ary[i]' ('ary'
 *  is an array of linked lists).
 *
 *  Return TRUE if the item is in the list, FALSE if not.
 *
 * ====================================================================
 */

BOOL
ARY_Item_In_List ( LNK_LST_ARY *ary, INT32 i, tlst_val val )
{
  register LNK_LST *lst = ARY_LST_Elem(ary,i);
  register LST_ITM *p;

  if (LST_Empty(lst))
    return FALSE;       /* empty list => not in the list */

  for (p=LST_first(lst); p!=NULL; p=LST_next(p))
    if (LST_val(p) == val)
      return TRUE; /* found it */

  /*
   *  If we get to here, we went through the list searching for the item
   *  but could not find it.  Just return FALSE.
   */
  return FALSE;
}
/* ====================================================================
 *
 * List_Print
 *
 * Print the message 'msg' along with any arguments.  Then print each
 * item in the linked list 'lst' (print the items in decimal form).
 *
 * NOTE: All output is written to the "trace file" whose file
 *       pointer is in the externally defined variable, 'TFile'.
 *
 * ====================================================================
 */

void
List_Print ( lst, msg, arg1, arg2, arg3 )
  LNK_LST *lst;
  char *msg;
  INT32 arg1, arg2, arg3;
{
  tlst_val data;

  fprintf ( TFile, msg, arg1, arg2, arg3 );
  fprintf ( TFile,"\n" );

  /*
   *  Loop over all the items, printing the data value.
   */
  data = First_Item(lst);
  for (data=First_Item(lst); Valid_Item(data); data=Next_Item(lst))
    fprintf ( TFile," %d", data );

  fprintf(TFile,"\n");
}

/* ====================================================================
 *
 * ARY_List_Print
 *
 * Print the message 'msg' along with any arguments.  Then print each
 * linked list in the array of linked lists 'ary' (print the items in
 * decimal form).
 *
 * NOTE: All output is written to the "trace file" whose file
 *       pointer is in the externally defined variable, 'TFile'.
 *
 * ====================================================================
 */

void
ARY_List_Print ( ary, msg, arg1, arg2, arg3 )
  LNK_LST_ARY *ary;
  char *msg;
  INT32 arg1, arg2, arg3;
{
  INT32 n_elems = ARY_LST_n_elems(ary);
  INT32 i;
  tlst_val data;
  LNK_LST *lst;

  fprintf(TFile,msg,arg1,arg2,arg3);
  fprintf(TFile,"\n");

  /*
   *  Loop over all the lists in the array of linked lists.
   */
  for (i=0; i<n_elems; ++i) {
    /*
     *  Loop over all the items, printing the data value.
     */
    lst = ARY_LST_Elem(ary,i);
    data=First_Item(lst);
    if (Invalid_Item(data))
      continue;
    fprintf(TFile,"%3d: ", i);
    for (; Valid_Item(data); data=Next_Item(lst))
      fprintf ( TFile," %d", data );

    fprintf(TFile,"\n");
  }
}
#endif /* MONGOOSE_BE */
