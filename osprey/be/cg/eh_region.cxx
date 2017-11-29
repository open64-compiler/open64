/*
 * Copyright (C) 2009-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/*
 * This module encapsulates the generation of exception range tables.
 */

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <algorithm>
#include <vector>
#if ! defined(BUILD_OS_DARWIN)
#include "libelf/libelf.h"
#endif /* ! defined(BUILD_OS_DARWIN) */
#include "dwarf_stuff.h"
#include "defs.h"
#include "errors.h"
#include "tracing.h"
#include "mempool.h"
#include "symtab.h"
#include "wn.h"
#include "irbdata.h"
#include "strtab.h"
#include "stblock.h"
#include "config.h"
#include "config_opt.h"
#include "xstats.h"
#include "eh_region.h"
#include "data_layout.h"
#include "region_util.h"
#include "region_main.h"
#include "bb.h"
#include "whirl2ops.h"
#include "label_util.h"

#ifdef TARG_IA64
#include "dwarf_stuff.h"
#endif
extern "C" {
#include "pro_encode_nm.h"
}
#include <map>
#include <set>


/*
 * eh_region.cxx is responsible for building the EH range tables
 * used to implement exception handling.  It does this by building
 * an initialized object for each PU.  The emitter will use these
 * initialized objects to write the tables into the object file.
 * Each range  table consists of a header and an array of ranges,
 * each range specifying the following:
 *   a region supplement pointer
 *   a kind (try-block, cleanup, mask, or exception specification)
 *   a low and high adress (offsets from start of function)
 *   a pointer to the parent range.
 *
 * A table may use either short (16-bit) or long (32-bit) offsets
 * to represent the low and high addresses.  16 bits almost always
 * suffices.  The header specifies which kind of offset is used.
 *
 * Before the initialized object is created, the range table is
 * represented internally by an object of type EH_RANGE_LIST.
 * An EH_RANGE_LIST is implemented using a vector of objects
 * of type EH_RANGE.  An EH_RANGE contains the components which
 * will form the range table, together with a number of fields
 * required during the process of building and modifying the
 * range list.
 *
 * An EH range list is built in a number of stages:
 *
 * An initial list is built before optimization and code generation,
 * by the routine EH_Generate_Range_List.
 * This uses the RID tree for the PU to create a tree of EH ranges,
 * represented as a postorder list.  The tree structure is indicated
 * only by the parent fields.  The tree is the natural subtree of
 * the RID tree formed by the EH regions.  A pointer to each
 * range is put into the corresponding RID, for use during code
 * generation.

 *
 * During code generation, when an EH region is encountered,
 * EH_Set_Start_Label is called at the beginning and EH_Set_EndLabel
 * at the end to set the start_label and end_label fields in the
 * range.  When a call is processed, EH_Set_Has_Call is called
 * for each EH region currently on the region stack, to set the
 * has_call field in the range.  Ranges for which this field is
 * not set will be eliminated later (by EH_Prune_Range_list).
 *
 * Prior to CG optimization, EH_Prune_Range_List is called to
 * eliminate unneeded EH ranges.  This gets rid of ranges which
 * do not have calls or throws.  Such ranges are irrelevant to
 * exception handling.  Eliminating them before CG optimization
 * allows transformations which might otherwise be unsafe.
 *
 * EH_Write_Range_Table does some transformations before
 * generating the INITO for a PU.  First of all, mask ranges
 * need to be eliminated.  This is done by resetting the parent
 * of each mask region to the parent of the nearest ancestor
 * cleanup range.  Then both mask regions and guard regions
 * must be replaced by cleanup regions with a trivial cleanup.
 * (Mask regions cannot be left in the range table because of backwards
 * compatibility requirements).
 *
 * At this point, we need to take account of the fact that CG
 * optimization may have reordered the basic blocks.  The range
 * table needs to be reordered accordingly.
 * 
 * Finally, the INITO is created.
 */

 /* Iterators and function objects related to the RIDs. */

 /* Generation of the range list requires doing a post-order
  * tree walk of the RIDs.  After the tree walk, we need to
  * set the parent fields in the ranges.  These two phases
  * require defining iterators RID_POST_ITER and RID_PARENT_ITER.
  */

 /* We also define a function object IS_EH_RID to identify the
  * EH regions in the RID.  It is important to note that
  * null-cleanup regions are not counted as EH regions.  They
  * are retained during the frontend but eliminated at this
  * point.
  */

class RID_POST_ITER {
private:
  RID * start;
  RID * own;
public:
  typedef std::forward_iterator_tag iterator_category;
  typedef RID * 	       value_type;
  typedef ptrdiff_t            difference_type;
  typedef value_type *         pointer;
  typedef value_type &         reference;
  RID_POST_ITER(RID* p = NULL);
  RID * operator*() const {return own;}
  RID_POST_ITER& operator++();
  RID_POST_ITER  operator++(int);
  friend bool operator==(const RID_POST_ITER&, const RID_POST_ITER&);
#ifdef KEY
  friend bool operator!=(const RID_POST_ITER&, const RID_POST_ITER&);
#endif
};

RID_POST_ITER::RID_POST_ITER(RID * p): start(p), own(p) {
  if (own != NULL) {
    while (RID_first_kid(own) != NULL)
      own = RID_first_kid(own);
  }
}

RID_POST_ITER& RID_POST_ITER::operator++()
{
  if (own == start) {
    own = NULL;
  }
  else
  if (RID_next(own) != NULL) {
    own = RID_next(own);
    while (RID_first_kid(own) != NULL)
      own = RID_first_kid(own);
  }
  else
    own = RID_parent(own);

  return *this;
}

RID_POST_ITER RID_POST_ITER::operator++(int)
{
  RID_POST_ITER tmp(*this);
  ++*this;
  return tmp;
}

inline bool operator==(const RID_POST_ITER& x, const RID_POST_ITER & y)
{
  return x.own == y.own;
}

#ifdef KEY
inline bool operator!=(const RID_POST_ITER& x, const RID_POST_ITER & y)
{
  return x.own != y.own;
}
#endif

class RID_PARENT_ITER {
private:
  RID * p;
public:
  typedef std::forward_iterator_tag iterator_category;
  typedef RID * 	       value_type;
  typedef ptrdiff_t	       difference_type;
  typedef value_type *         pointer;
  typedef value_type &         reference;
  RID_PARENT_ITER(RID* x = NULL): p(x) {}
  RID * operator*() const {return p;}
  RID_PARENT_ITER& operator++() {p = RID_parent(p); return *this;}
  RID_PARENT_ITER  operator++(int) {
    RID_PARENT_ITER tmp = *this;
    ++*this;
    return tmp;
  }
  friend bool operator==(const RID_PARENT_ITER&, const RID_PARENT_ITER &);
#ifdef KEY
  friend bool operator!=(const RID_PARENT_ITER&, const RID_PARENT_ITER &);
#endif
};

bool operator==(const RID_PARENT_ITER& x, const RID_PARENT_ITER& y) {
  return x.p == y.p;
}

#ifdef KEY
bool operator!=(const RID_PARENT_ITER& x, const RID_PARENT_ITER& y) {
  return x.p != y.p;
}
#endif

struct IS_EH_RID
{
  bool operator()(const RID* rid) {
    return RID_TYPE_eh(rid);}
};

/* The EH_RANGE and EH_RANGE_LIST classes. */

/* The first three eh_range_kind enumerators correspond to the
 * kinds that appear in the range table.  eh_mask and eh_guard
 * will be replaced by eh_cleanup before the table is created.
 */


enum eh_range_kind {
  ehk_try_block,
  ehk_exc_spec,
  ehk_cleanup,
  ehk_mask,
  ehk_guard,
  ehk_last};

static eh_range_kind Range_Kind(RID * rid)
{
  if (RID_TYPE_try(rid))
    return ehk_try_block;
  if (RID_TYPE_exc_spec(rid))
    return ehk_exc_spec;
  if (RID_TYPE_cleanup(rid))
    return ehk_cleanup;
  if (RID_TYPE_null_cleanup(rid))
    return ehk_cleanup;
  if (RID_TYPE_mask(rid))
    return ehk_mask;
  if (RID_TYPE_guard(rid))
    return ehk_guard;
  return ehk_last;
}

struct EH_RANGE {
// components of range table
  INITO_IDX    	ereg_supp;
  LABEL_IDX     start_label;
  LABEL_IDX     end_label;
  EH_RANGE      *parent;
  eh_range_kind kind;

// bookkeeping items
  RID *		rid;		// for setting parents
  BB		*end_bb;	// for sorting
  EH_RANGE*	id;		// for sorting
  INT32		key;		// for sorting
  INT32		adjustment;	// to adjust parents after compression
  bool		has_call;	// ranges without calls get deleted

// constructor
  EH_RANGE(RID * x):
    ereg_supp(WN_ereg_supp(RID_rwn(x))),
    start_label(LABEL_IDX_ZERO),
    end_label(LABEL_IDX_ZERO),
    parent(NULL),
    kind(Range_Kind(x)),
    rid(x),
    end_bb(NULL),
    id(this),
    key(0),
    adjustment(0),
    has_call(false) {}
};

/* an EH_RANGE_LIST is essentially just a vector of EH_RANGE.  We
 * make it a separate type because it has a tree structure
 * and we want to define a parent iterator on it.
 */

class EH_RANGE_LIST {
private:
  std::vector<EH_RANGE> v;
public:
  EH_RANGE_LIST(): v() {}
  void add_range(EH_RANGE range) {
    v.push_back(range);
  }
  void clear() {v.clear();}
  size_t size() {return v.size();}
  EH_RANGE& operator[](size_t i) {return v[i];}
  typedef std::vector<EH_RANGE>::iterator 	     iterator;
  typedef std::vector<EH_RANGE>::reverse_iterator reverse_iterator;
  iterator begin() {return v.begin();}
  iterator end()   {return v.end();}
  reverse_iterator rbegin() {return v.rbegin();}
  reverse_iterator rend()   {return v.rend();}
  iterator erase(iterator first, iterator last) {
    return v.erase(first, last);}
};

class EH_RANGE_LIST_PARENT_ITER {
private:
  EH_RANGE_LIST::iterator iter;
public:
  typedef std::forward_iterator_tag iterator_category;
  typedef EH_RANGE             value_type;
  typedef ptrdiff_t            difference_type;
  typedef value_type *         pointer;
  typedef value_type &         reference;
  EH_RANGE_LIST_PARENT_ITER(): iter() {}
  EH_RANGE_LIST_PARENT_ITER(EH_RANGE_LIST::iterator x): iter(x) {}
  EH_RANGE& operator*() {return *iter;}
  EH_RANGE_LIST_PARENT_ITER& operator++() {
#ifdef KEY // workaround g++ 3.2 problem
    iter = EH_RANGE_LIST::iterator(iter->parent); return *this;}
#else
    iter = iter->parent; return *this;}
#endif
  EH_RANGE_LIST_PARENT_ITER operator++(int) {
    EH_RANGE_LIST_PARENT_ITER tmp = *this;
    ++*this;
    return tmp;
  }
  friend bool operator==(const EH_RANGE_LIST_PARENT_ITER&,
			 const EH_RANGE_LIST_PARENT_ITER&);
#ifdef KEY 
  friend bool operator!=(const EH_RANGE_LIST_PARENT_ITER&,
			 const EH_RANGE_LIST_PARENT_ITER&);
#endif
};

inline bool operator==(const EH_RANGE_LIST_PARENT_ITER & x,
		       const EH_RANGE_LIST_PARENT_ITER &y) {
  return x.iter == y.iter;
}

#ifdef KEY 
inline bool operator!=(const EH_RANGE_LIST_PARENT_ITER & x,
		       const EH_RANGE_LIST_PARENT_ITER &y) {
  return x.iter != y.iter;
}
#endif

/* There is always just one EH_RANGE_LIST which belongs to
 * eh_region.cxx and which is cleared at the beginning of
 * EH_Generate_Range_List.
 */

static EH_RANGE_LIST range_list;

/* We define a function object ADD_EH_RANGE to be passed to
 * for_each as we iterate over the RID using RID_POST_ITER.
 * This will add a range for every EH region.
 */
#ifdef KEY
// workaround for g++ 3.2 bug, the function object below does not
// get called properly
void ADD_EH_RANGE (RID * rid)
{
    if (RID_TYPE_eh(rid)) {
    	if (WN_block_empty(WN_region_pragmas(RID_rwn(rid))))
      	    range_list.add_range(rid);
	else
	    Set_ST_is_not_used (INITO_st(WN_ereg_supp(RID_rwn(rid))));
    }
}
#else
struct ADD_EH_RANGE {
  void operator()(RID * rid) {
    if (RID_TYPE_eh(rid)) {
      range_list.add_range(rid);
    }
  }
};
#endif // KEY

/* The function object SET_PARENT finds the nearest ancestor 
 * EH region and sets the parent accordingly.
 */

struct SET_PARENT {
  void operator()(EH_RANGE& r) {
    RID_PARENT_ITER first(r.rid);
    RID_PARENT_ITER last(NULL);
#if (__GNUC__ < 4 ||(__GNUC__ == 4 && __GNUC_MINOR__ == 0))
    first = find_if(++first, last, IS_EH_RID(), std::__iterator_category(first));
#else
    first = std::__find_if(++first, last, IS_EH_RID(), std::__iterator_category(first));
#endif
    if (first == last)
      r.parent = NULL;
    else
      r.parent = RID_eh_range_ptr(*first);
  }
};

/* EH_Generate_Range_List does a post-order RID walk to create
 * the range list, then iterates over the range list to set the
 * parent fields.
 */
 
void
EH_Generate_Range_List(WN * pu)
{
  range_list.clear();

  RID * rid = (RID *) WN_MAP_Get(RID_map, pu);
  RID_POST_ITER rid_first(rid);
  RID_POST_ITER rid_last(NULL);

  while (rid_first != rid_last)
  {
    ADD_EH_RANGE(*rid_first);
    ++rid_first;
  }

  EH_RANGE_LIST::iterator list_first(range_list.begin());
  EH_RANGE_LIST::iterator list_last (range_list.end());

  for (EH_RANGE_LIST::iterator p = list_first; p!=list_last; p++)
#ifdef KEY	// workaround g++ 3.2 problem
    RID_eh_range_ptr(p->rid) = &(*p);
#else
    RID_eh_range_ptr(p->rid) = p;
#endif

  std::for_each(list_first, list_last, SET_PARENT());
}


/* Normally EH_Set_Start_Label just creates a label and an
 * associated basic block and sets the start_label field of
 * the designated range accordingly.  This simple picture is
 * complicated by the requirements of guard ranges.  A guard
 * range is required before every mask range to fill up the
 * unused space in the enclosing cleanup region:  otherwise
 * binary search can be foiled and incorrectly attribute an
 * address to a mask region.  So we don't create a new label
 * for a guard range:  instead we use the end label of the
 * elder sibling, if any, and otherwise the start label of the
 * enclosing range.  The function object IS_SIB_RANGE is
 * used to search for the elder sibling.
 */

struct IS_SIB_RANGE {
  const EH_RANGE* me;
  IS_SIB_RANGE(const EH_RANGE * x): me(x) {}
  bool operator()(const EH_RANGE& r) const {
    return (r.rid != me->rid)       &&
           (r.parent == me->parent) &&
           (r.end_label != (LABEL_IDX)NULL);
  }
};


static LABEL_IDX Duplicate_LABEL (LABEL_IDX oldi)
{
	LABEL_IDX lbi;
	LABEL& lab = New_LABEL (CURRENT_SYMTAB, lbi);
	LABEL old = Label_Table[oldi];
	Set_LABEL_name_idx(lab, Save_Str2(LABEL_name(old), ".dup"));
	Set_LABEL_kind(lab, LABEL_kind(old));
	return lbi;
}


// Check whether the EH range has it's corresponding landing pad
bool
EH_Has_Landing_Pad (EH_RANGE *range)
{
  if (range->ereg_supp == 0)   return false;
  ST* st = INITO_st(range->ereg_supp);
  if (ST_is_not_used(st))      return false;

  INITV_IDX blk = INITO_val(range->ereg_supp);
  if (INITV_kind(blk) != INITVKIND_BLOCK)
    return false;

  INITV_IDX first = INITV_blk(blk);
  if (INITV_kind(first) == INITVKIND_LABEL)
    return true;
  else
   return false;
}

void
EH_Set_Start_Label(EH_RANGE* p)
{
  LABEL_IDX label;
#ifdef TARG_IA64
  // too much Eh label will affect the cfg and region formation,
  // further affects the register allocation and instruction schedule
  // for optimization of EH implementation
  if (!EH_Has_Landing_Pad (p) && PU_is_mainpu (Get_Current_PU ()))
    return;
#endif
  if (p->kind == ehk_guard) {
#ifdef KEY
    EH_RANGE_LIST::reverse_iterator rfirst = range_list.rbegin();
    while (&(*rfirst) != p)
      rfirst++;
#else
    EH_RANGE_LIST::reverse_iterator rfirst(p);
#endif
    EH_RANGE_LIST::reverse_iterator rlast  = range_list.rend();
    EH_RANGE_LIST::reverse_iterator riter =
      std::find_if(rfirst, rlast, IS_SIB_RANGE(p));
    if (riter == rlast) {
      if (p->parent != NULL) {
	label = Duplicate_LABEL(p->parent->start_label);
     	Set_Label_BB(label, NULL);
      }
    }

    else {
      label = Duplicate_LABEL(riter->end_label);
      Set_Label_BB(label, NULL);
      Set_LABEL_kind(Label_Table[label], LKIND_DEFAULT);
      Set_LABEL_begin_eh_range(label);
    }
    Add_Label(label);
  }

  else {
    label   = Gen_Temp_Label();
    BB * bb = Add_Label(label);
    Set_LABEL_begin_eh_range(label);
  }

  p->start_label = label;
}

/* No complications with EH_Set_End_Label. */

void
EH_Set_End_Label(EH_RANGE* p)
{
  LABEL_IDX label;
#ifdef TARG_IA64
  // too much Eh label will affect the cfg and region formation,
  // further affects the register allocation and instruction schedule
  // for optimization of EH implementation
  if (!EH_Has_Landing_Pad (p) && PU_is_mainpu (Get_Current_PU ()))
    return;
#endif
  label = Gen_Temp_Label();
  BB * bb    = Add_Label(label);
  p->end_label = label;
  p->end_bb    = bb;
  Set_LABEL_end_eh_range(label);
}

/* Guard regions also complicate EH_Set_Has_Call.  We don't want
 * to eliminate guard regions when they are required by a mask
 * region.  Therefore when we find a call in a mask region, we
 * set has_call in the associated guard region as well.
 */


void EH_Set_Has_Call(EH_RANGE* p)
{
  p->has_call = TRUE;
  if (p->kind == ehk_mask) {
    // set has_call for associated guard region also
#ifdef KEY // workaround constructor call bug in g++ 3.2
    EH_RANGE_LIST::reverse_iterator rfirst = range_list.rbegin();
    while (&(*rfirst) != p)
      rfirst++;
#else
    EH_RANGE_LIST::reverse_iterator rfirst(p);
#endif
    EH_RANGE_LIST::reverse_iterator rlast  = range_list.rend();
    rfirst = std::find_if(rfirst, rlast, IS_SIB_RANGE(p));
    Is_True(rfirst != rlast && rfirst->kind == ehk_guard,
		      ("mask region must have guard"));
    rfirst->has_call = TRUE;
  }    
}

/*
 * EH_Prune_Range_list has four phases:
 *  (0) ** KEY ** CG cflow (cflow_unreachable) may have deleted 
 *      BBs which will result in removal of EHRANGEs. If such
 *      a range is a parent of another range, fix the parent
 *      pointer in the kid. (bug 5600)
 *
 *  (1) The adjustment field of each range is set to the number
 *      of ranges prior to this one which contain no call and
 *      will therefore be eliminated.
 *
 *  (2) The adjustment field of each range is replace by the
 *	adjustment of its parent.
 *
 *  (3) The ranges with no call are eliminated.
 *
 *  (4) The parents are adjusted.
 *
 */


struct HAS_NO_CALL_OR_HAS_NULL_OR_UNREACHABLE_LABEL {
  bool operator()(const EH_RANGE& r) {
    if (Inhibit_EH_opt) return false;
    if (r.has_call && r.start_label != (LABEL_IDX)NULL) {
      BB *start_bb = Get_Label_BB(r.start_label);
      if (start_bb && !BB_unreachable(start_bb)) return false;
    }
    return true;
    }
};

#ifdef KEY
struct FIX_PARENT
{
  bool operator() (EH_RANGE& r)
  {
    while (r.parent &&
           HAS_NO_CALL_OR_HAS_NULL_OR_UNREACHABLE_LABEL() (*(r.parent)))
      r.parent = r.parent->parent;
  }
};
#endif // KEY

struct SET_ADJUSTMENT {
  INT32 amount;
  SET_ADJUSTMENT(): amount(0) {}
  void operator()(EH_RANGE& r) {
    if (HAS_NO_CALL_OR_HAS_NULL_OR_UNREACHABLE_LABEL()(r)) {
      ++amount;
      if (r.start_label != (LABEL_IDX) NULL)
        Set_LABEL_kind(Label_Table[r.start_label], LKIND_DEFAULT);
      if (r.end_label != (LABEL_IDX) NULL) 
        Set_LABEL_kind(Label_Table[r.end_label], LKIND_DEFAULT);
      Set_ST_is_not_used(INITO_st(r.ereg_supp));
    }
    r.adjustment = amount;
  }
};

struct CLEAR_USED {
  CLEAR_USED() {}
  void operator()(EH_RANGE& r) {
    if (!HAS_NO_CALL_OR_HAS_NULL_OR_UNREACHABLE_LABEL()(r))
      Clear_ST_is_not_used(INITO_st(r.ereg_supp));
  }
};

struct SET_ADJUSTMENT_TO_PARENT_ADJUSTMENT {
  void operator()(EH_RANGE & r) {
    if (r.parent == NULL) {
      r.adjustment = 0;
    }
    else {
      r.adjustment = r.parent->adjustment;
    }
  }
};

struct ADJUST_PARENT {
  void operator()(EH_RANGE&r) {r.parent -= r.adjustment;}
};


struct SET_NUM_EH_RANGE {
  void operator()(EH_RANGE & r) {
    RID_num_eh_ranges(r.rid) = 1;  // every eh region contains 1 eh_range before flatten
  }
};

void
EH_Prune_Range_List(void)
{
  EH_RANGE_LIST::iterator first(range_list.begin());
  EH_RANGE_LIST::iterator last(range_list.end());
  if (first == last) return;
  if (!PU_has_exc_scopes(Get_Current_PU()) && !Inhibit_EH_opt) {
    range_list.erase(first, last);
    return;
  }
  for_each  (first, last, FIX_PARENT());
  std::for_each  (first, last, SET_ADJUSTMENT());
  std::for_each  (first, last, CLEAR_USED());
  std::for_each  (first, last, SET_ADJUSTMENT_TO_PARENT_ADJUSTMENT());

  range_list.erase(
    remove_if (first, last, 
               HAS_NO_CALL_OR_HAS_NULL_OR_UNREACHABLE_LABEL()), 
    last);
  std::for_each  (range_list.begin(), range_list.end(), ADJUST_PARENT());

#if defined(KEY) && defined(Is_True_On)
  for (INT i=0; i<range_list.size(); i++)
    Is_True (&range_list[i] != range_list[i].parent,
       ("EH_Prune_Range_List end: kid == parent"));
#endif // Is_True_On
} 

struct COMPARE_RANGES {
  bool operator()(const EH_RANGE& r1, const EH_RANGE& r2) {
    if (r1.key < r2.key) return true;
    if (r1.key > r2.key) return false;
    return r1.id < r2.id;
  }
};

void
reorder_range_list()
{
  BB * bb;
  INT32 bb_count;
  size_t i;

#ifdef KEY	// Speed up the original code.  Bug 10289.
  {
    INT32 *bb_counts = (INT32 *) alloca(sizeof(INT32) * (PU_BB_Count + 1));
#if Is_True_On
    memset(bb_counts, 0, sizeof(INT32) * (PU_BB_Count + 1));
#endif
    for (bb = REGION_First_BB, bb_count = 0;
	 bb != NULL;
	 bb = BB_next(bb), ++bb_count) {
      bb_counts[BB_id(bb)] = bb_count;
    }
    for (i = 0; i < range_list.size(); ++i) {
      bb_count = bb_counts[BB_id(range_list[i].end_bb)];
      Is_True(bb_count != 0,("reorder_range_list: bad bb_count"));
      range_list[i].key = bb_count;
      range_list[i].id = &range_list[i];
    }
  }
#else
  for (bb = REGION_First_BB, bb_count = 0;
       bb != NULL;
       bb = BB_next(bb), ++bb_count) {
    for (i = 0; i < range_list.size(); ++i) {
      if (range_list[i].end_bb == bb)
	range_list[i].key = bb_count;
      range_list[i].id = &range_list[i];
    }
  }
#endif

  EH_RANGE_LIST::iterator first(range_list.begin());
  EH_RANGE_LIST::iterator last (range_list.end());

  stable_sort(first, last, COMPARE_RANGES());

  // reset parent pointers using inverse vector

  std::vector<int> inv(range_list.size());
  for (i = 0; i < range_list.size(); ++i)
    inv[range_list[i].id - &range_list[0]] = i;

  for (i = 0; i < range_list.size(); ++i)
    if (range_list[i].parent != NULL)
      range_list[i].parent = &range_list[0] + 
			     inv[range_list[i].parent - &range_list[0]];
}

struct IS_CLEANUP_RANGE {
  bool operator()(const EH_RANGE& r) const {return r.kind == ehk_cleanup;}
};

struct FIX_MASK_PARENT {
  void operator()(EH_RANGE& r) {
    if (r.kind == ehk_mask) {
#ifdef KEY
      EH_RANGE_LIST_PARENT_ITER first(EH_RANGE_LIST::iterator(r.parent));
      EH_RANGE_LIST_PARENT_ITER last(EH_RANGE_LIST::iterator(NULL));
#else
      EH_RANGE_LIST_PARENT_ITER first(r.parent);
      EH_RANGE_LIST_PARENT_ITER last (NULL);
#endif
#if (__GNUC__ < 4 || __GNUC_MINOR__ == 0)
      first = find_if(first, last, IS_CLEANUP_RANGE(), std::__iterator_category(first));
#else
      first = std::__find_if(first, last, IS_CLEANUP_RANGE(), std::__iterator_category(first));
#endif
      Is_True(first != last, ("mask region must have cleanup ancestor"));
      r.parent = (*first).parent;
    }
  }
};

struct CHANGE_MASK_OR_GUARD_TO_CLEANUP {
  void operator()(EH_RANGE& r) {
    if (r.kind == ehk_mask || r.kind == ehk_guard)
      r.kind = ehk_cleanup;
    }
};

static void
fix_mask_ranges(void)
{
  /*
   * For mask regions the parent pointers need to be readjusted.
   * This needs to be done from the outside in, so we traverse the
   * range table in reverse.  For every mask region, we follow the 
   * parent pointers till we encounter a cleanup region, then set
   * the parent to the parent of that cleanup region. Then we make
   * a second pass and replace eh_mask by eh_cleanup.
   */

  EH_RANGE_LIST::reverse_iterator rfirst(range_list.rbegin());
  EH_RANGE_LIST::reverse_iterator rlast (range_list.rend());

  std::for_each(rfirst, rlast, FIX_MASK_PARENT());
  std::for_each(range_list.begin(), range_list.end(),
	   CHANGE_MASK_OR_GUARD_TO_CLEANUP());
}

static ST * eh_pu_range_st;

extern ST* EH_Get_PU_Range_ST(void)
{
  return eh_pu_range_st;
}

inline BOOL Use_Long_EH_Range_Offsets(void)
{
  return Force_Long_EH_Range_Offsets ||
	 PU_WN_BB_Cnt + PU_WN_Stmt_Cnt > 2000;
}

static ST*
ST_For_Range_Table(WN * wn)
{
  ST * pu = WN_st(wn);
  ST * st;

  // Size:
  // header_size + number_of_ranges * range_size
  // range_size: size of
  //	one pointer 	      (ereg_supp)
  //    one offset into table (parent)
  //    two bytes             (kind)
  //    two offsets	      (low and high, 2 or 4 bytes each)

  UINT32 header_size = 8;
  UINT32 number_of_ranges = range_list.size();
  UINT32 parent_size = 2;
  UINT32 kind_size   = 2;
  UINT32 offset_size = Use_Long_EH_Range_Offsets() ? 4 : 2;
  UINT32 range_size = Pointer_Size + parent_size + kind_size + 
		      2 * offset_size;
  UINT32 size = header_size + number_of_ranges * range_size;

#ifdef KEY
// This is a temporary solution. The above 'size' is not properly
// calculated. TODO: Fix the size above.
  size = 0;	// don't output the size
#endif // KEY

  TY_IDX tyi;
  TY& ty = New_TY(tyi);
  TY_Init(ty, size, KIND_STRUCT, MTYPE_M,
	  Save_Str2(".range_table.",ST_name(pu)));
#ifdef KEY
  if (Is_Target_64bit())
    Set_TY_align(tyi, 8);
  else
    Set_TY_align(tyi, 4);
#else
  Set_TY_align(tyi, 4);
#endif
  st = New_ST(CURRENT_SYMTAB);
  ST_Init(st, TY_name_idx(ty),
	  CLASS_VAR, SCLASS_EH_REGION, EXPORT_LOCAL, tyi);
  Set_ST_is_initialized(st);
  Allocate_Object(st);
  return st;
}

#define SHORT_OFFSETS  0
#define LONG_OFFSETS   1
#define HEADER_VERSION 1

inline INT16 parent_offset(INT32 i)
{
  if (range_list[i].parent == NULL)
    return 0;
  else
    return (INT16) (range_list[i].parent - &range_list[i]);
}


/* This part are support routine for creating INITO of landing pad.
   In fact, they are architecture independent, need to merge with 
   the same support routine implemented on X8664. 
   TODO: merge these code with code implemented by pathscale on X8664.
 */

#include <map>
#include <set>
using namespace std;

typedef std::map< ST_IDX, int > 	TF_MAP;		// <type_ST_IDX, filter>
typedef std::map< int, ST_IDX > 	FT_MAP; 	// <filter, type_ST_IDX>
typedef std::set< ST_IDX >      	EH_PTS; 	// eh pic type set
// global tfmap and ftmap for each PU
TF_MAP tfmap;
FT_MAP ftmap;

    
static void
EH_Build_PIC_Type(ST_IDX idx)
{
  static EH_PTS pts;
  if (pts.find(idx) != pts.end())	
    return;
  pts.insert(idx);

  ST* st = &St_Table[idx];
  ST* pst = New_ST(GLOBAL_SYMTAB);
  STR_IDX pname = Save_Str2 ("DW.ref.", ST_name (st));
  ST_Init(pst, pname, CLASS_VAR, SCLASS_DGLOBAL, EXPORT_HIDDEN, MTYPE_TO_TY_array[MTYPE_U8]);
  Set_ST_is_weak_symbol (pst);
  Set_ST_is_initialized (pst);
  
  ST_ATTR_IDX st_attr_idx;
  ST_ATTR&    st_attr = New_ST_ATTR(GLOBAL_SYMTAB, st_attr_idx);
  ST_ATTR_Init(st_attr, ST_st_idx(pst), ST_ATTR_SECTION_NAME, 
               Save_Str2 (".gnu.linkonce.d.", ST_name(pst)));
                                                                                
  INITV_IDX iv = New_INITV();
  INITV_Init_Symoff(iv, st, 0, 1);
  New_INITO (ST_st_idx(pst), iv);
  Assign_ST_To_Named_Section (pst, ST_ATTR_section_name (st_attr));
}

static int
Get_EH_Filter_By_Type(ST_IDX idx, TF_MAP& tfmap)
{
  TF_MAP::iterator it;
  it = tfmap.find(idx);
  return (it != tfmap.end()) ? it->second : 0;
}

static ST_IDX
Get_EH_ST_By_Filter(int filter, FT_MAP& ftmap)
{
  FT_MAP::iterator it;
  it = ftmap.find(filter);
  return (it != ftmap.end() ? it->second : 0);
}

// return max_filter
static int
Convert_TF_Map_To_FT_Map(TF_MAP& src, FT_MAP& dst)
{
  TF_MAP::iterator it;
  int filter = 0;
  
  dst.clear();
  for (it = src.begin(); it != src.end(); it++)
  {
    dst.insert(std::make_pair(it->second, it->first));
    if (it->second > filter) filter = it->second;
  }
  return filter;
}

static INITV_IDX
Get_TF_Map_and_EH_Spec_List(PU& pu, TF_MAP& tfmap)
{
  INITO_IDX ino_idx = pu.misc;
  tfmap.clear();

  if (ino_idx == INITO_IDX_ZERO) {
    return INITV_IDX_ZERO;
  }

  INITV_IDX exc_ptr_iv = INITO_val(ino_idx);
  INITV_IDX filter_iv  = INITV_next(exc_ptr_iv);
  INITV_IDX tinfo      = INITV_next(filter_iv);
  INITV_IDX eh_spec    = INITV_next(tinfo);

  INITO_IDX id  = TCON_uval(INITV_tc_val(tinfo));

  // tfmap init
  if (id != INITO_IDX_ZERO) {
    ST *st = INITO_st(id);
    INITV_IDX blk = INITO_val(id);

    while (blk != INITV_IDX_ZERO) {
      INITV_IDX type_st_iv = INITV_blk(blk);
      int filter = TCON_ival(INITV_tc_val(INITV_next(type_st_iv)));
      ST_IDX    type_st_idx = 0;
      if (INITVKIND_ZERO != INITV_kind(type_st_iv))
        type_st_idx = TCON_uval(INITV_tc_val(type_st_iv));

      if (/*(Gen_PIC_Call_Shared || Gen_PIC_Shared) &&*/ type_st_idx != 0)
        EH_Build_PIC_Type(type_st_idx);
      tfmap.insert(std::make_pair(type_st_idx, filter));
      blk = INITV_next(blk);
    }
  }

  // eh_spec blk list
  id = TCON_uval(INITV_tc_val(eh_spec));
  return (id != 0) ? INITV_blk(INITO_val(id)) : INITV_IDX_ZERO;
}


// bug 3416: The exception ranges in exception table must be sorted in
// increasing order of call-site address. The problem shows up when we
// generate nested regions due to code like "if (foo()) bar();".
// So flatten the regions here to remove overlap between parent and child
// regions. As a side-effect, add new ranges resulting from the splits.
// Assumption: a parent region is always listed after its children (even if
// not immediately after). The caller of this function must, therefore, sort
// the list before calling.
// Transformation:
// R1_begin:
//    R2_begin:
//    R2_end:
// R1_end:
//
// R2_end - R2_begin  ===>    R2_begin - R1_begin
// R1_end - R1_begin          R2_end - R2_begin
//                            R1_end - R2_end (bug 3736)
//
// TODO: Check if the new regions created have any call, if not, don't
// create, or delete the region.

// FIX: remove two improper assumptions in previous implementation:
//   1. there is no gaps between children of a same parent. It has been violated
//      by some test case;
//   2. there is no level 2 or higher vertexes.
//      it is not violated yet, but we remove it for safety.
// We now assume that the EH_RANGES forms an arbitrary forrest now.
// We still assume that parents are always listed after their children, and regions
// are listed in order of starting BB. Also, we discard some empty new ranges.
static void flatten_regions (void)
{
  vector<EH_RANGE> new_ranges;
  int i, j;
  const int no_child = -1;

  // set the num_eh_ranges for all eh region to 1
  for_each  (range_list.begin(), range_list.end(), SET_NUM_EH_RANGE());

  for ( j = range_list.size() - 1; j > 0; j--)
  {
    int first_child, last_child;
    bool more_parents;

    first_child = last_child = no_child;
    more_parents = false;
    for (i=0; i < j; i++)
    {
      if(range_list[i].parent)
        more_parents = true;
      if (range_list[i].parent == &range_list[j])
      {
        if (first_child == no_child)
          last_child = first_child = i;
        if ( first_child != no_child && i != last_child )
        {
          EH_RANGE new_range (range_list[j].rid);
          new_range.start_label = range_list[last_child].end_label;
          new_range.end_label = range_list[i].start_label;
          new_range.end_bb = Get_Label_BB(range_list[i].start_label);
          new_range.has_call = range_list[j].has_call; // not accurate
          if (Get_Label_BB(new_range.start_label) != Get_Label_BB(new_range.end_label))
             new_ranges.push_back (new_range);

        }
        range_list[i].parent = NULL;
        last_child = i;
      }
    }

    if (first_child != no_child)
    {
      EH_RANGE new_range (range_list[j].rid);
      new_range.start_label = range_list[last_child].end_label;
      new_range.end_label = range_list[j].end_label;
      new_range.end_bb = range_list[j].end_bb;
      new_range.has_call = range_list[j].has_call; // not accurate
      if (Get_Label_BB(new_range.start_label) != Get_Label_BB(new_range.end_label))
        new_ranges.push_back (new_range);

      // Update the parent now to end before the 1st child
      range_list[j].end_label = range_list[first_child].start_label;
      range_list[j].end_bb = Get_Label_BB (range_list[first_child].start_label);
    }
    if (!more_parents)
     break;
  }

  for (vector<EH_RANGE>::iterator iter = new_ranges.begin();
       iter != new_ranges.end(); ++iter) {
    range_list.add_range (*iter);
    RID_num_eh_ranges(iter->rid) ++;
  }
}

#include <map>
using namespace std;

struct cmpst
{
  bool operator() (const ST_IDX i1, const ST_IDX i2) const
  {
  	return (i1 < i2);
  }
};

// Seems I need some features of 'map' and some features of 'vector'. I want
// a map from an st entry to a filter. This map would be sorted based on st.
// I need an ordering based on filter (which is not the key in the map).
map<ST_IDX, int, cmpst> type_filter_map;

struct type_filter_entry {
	ST_IDX st;
	int filter;
};

struct sort_on_filter : public binary_function<type_filter_entry, 
					type_filter_entry, bool>
{
  bool operator() (const type_filter_entry t1, 
			const type_filter_entry t2) const
  {
	return t1.filter > t2.filter;
  }
};

static INITO*
Create_Type_Filter_Map (void)
{
  INITV_IDX i = INITV_next (INITV_next (INITO_val (PU_misc_info (Get_Current_PU()))));
  INITO* ino;
  INITO_IDX idx = TCON_uval (INITV_tc_val(i));
  if (idx)	// idx for typeinfo_table
  {
    ino = &Inito_Table[idx];
    ST* st = INITO_st(ino);

    FmtAssert (!strcmp(ST_name (*st), "__TYPEINFO_TABLE__") && 
    	      (ST_sclass(st) == SCLASS_EH_REGION_SUPP), 
	      ("Unexpected ST in PU"));
    INITV_IDX blk = INITO_val(*ino);

    do 
    {
	INITV_IDX st_entry = INITV_blk(blk);
	// st_idx, filter
	ST_IDX st_idx = 0;
	if (INITV_kind (st_entry) != INITVKIND_ZERO)
	    st_idx = TCON_uval(INITV_tc_val (st_entry));
	int filter = TCON_ival (INITV_tc_val (INITV_next (st_entry)));
	type_filter_map [st_idx] = filter;
    } while (INITV_next(blk) && (blk=INITV_next(blk)));
  }

  i = INITV_next (i);
  ino = 0;
  idx = TCON_uval (INITV_tc_val(i));
  if (idx)	// idx for eh-spec
  {
    ino = &Inito_Table[idx];
    ST* st = INITO_st(ino);

    FmtAssert (!strcmp(ST_name (*st), "__EH_SPEC_TABLE__") && 
    	      (ST_sclass(st) == SCLASS_EH_REGION_SUPP), 
	      ("Unexpected ST in PU"));
  }
  return ino;
}

// This function returns the size of an LEB128 encoding of value. We do
// not use the encoding however. We emit the unencoded value with the LEB128
// directive.
static int
sizeof_signed_leb128 (int value)
{
  char buff[ENCODE_SPACE_NEEDED];
  int size;
  int res = _dwarf_pro_encode_signed_leb128_nm (value, &size, buff, sizeof(buff));
  FmtAssert (res == DW_DLV_OK, ("Encoding for exception table failed"));
  return size;
}


static const char*
Get_INITV_kind (INITVKIND kind)
{
#define CASE_KIND(n)	case n:return #n
  switch (kind) {
	  CASE_KIND(INITVKIND_UNK);
	  CASE_KIND(INITVKIND_SYMOFF);
	  CASE_KIND(INITVKIND_ZERO);
	  CASE_KIND(INITVKIND_ONE);
          CASE_KIND(INITVKIND_VAL);
          CASE_KIND(INITVKIND_BLOCK);
	  CASE_KIND(INITVKIND_PAD);
	  CASE_KIND(INITVKIND_SYMDIFF);
	  CASE_KIND(INITVKIND_SYMDIFF16);
	  CASE_KIND(INITVKIND_LABEL);
#ifdef TARG_IA64
	  CASE_KIND(INITVKIND_SYMIPLT);
#endif
  }
  return "unknown";
#undef CASE_KIND
}

static const char*
Get_LABEL_Kind (LABEL_KIND kind)
{
#define CASE_KIND(n)	case n: return #n
  switch (kind) {
  	CASE_KIND(LKIND_DEFAULT);
	CASE_KIND(LKIND_ASSIGNED);
	CASE_KIND(LKIND_BEGIN_EH_RANGE);
	CASE_KIND(LKIND_END_EH_RANGE);
	CASE_KIND(LKIND_BEGIN_HANDLER);
	CASE_KIND(LKIND_END_HANDLER);
  }
  return "unknown";
#undef CASE_KIND
}

static void
EH_Dump_INITV (INITV_IDX inv, FILE* fp, int step)
{
  if (inv == 0)return;
  // for identation, increased by step
  for (int i=0; i<step; i++)
    fprintf(fp, "\t");
  
  fprintf(fp, "%s (%d)", Get_INITV_kind(INITV_kind(inv)), inv);
  
  if (INITVKIND_BLOCK == INITV_kind(inv)) {
    fprintf(fp, "\n");
    EH_Dump_INITV(INITV_blk(inv), fp, step + 1);
  }
  else if (INITVKIND_LABEL == INITV_kind(inv)) {
    LABEL& lab = Label_Table[INITV_lab(inv)];
    fprintf(fp, " %s: (%s) (%d)\n", Get_LABEL_Kind (lab.kind), LABEL_name (lab), INITV_lab(inv));
  }
  else if (INITVKIND_VAL == INITV_kind(inv)) {
    int sym = TCON_ival(INITV_tc_val(inv));
    fprintf(fp, " INITVKIND_VAL: value = %d (0x%08x)\n", sym, sym);
  }
  else if (INITVKIND_ZERO == INITV_kind(inv)) {
    fprintf(fp, " INITVKIND_VAL: value = 0\n");
  }
  else if (INITVKIND_ONE == INITV_kind(inv)) {
    fprintf(fp, " INITVKIND_VAL: value = 1\n");
  }
  else {
    fprintf(fp, "\n");
  }
  if (INITV_next(inv) != 0)
    EH_Dump_INITV(INITV_next(inv), fp, step);
}

void
EH_Dump_INITOs (WN *pu, FILE *fp)
{
  INT i;
  INITO *ino;
  static int num = 0;
  FOREACH_INITO (CURRENT_SYMTAB, ino, i) {
    ST *st = INITO_st(ino);
    if (st->storage_class == SCLASS_EH_REGION ||
     	st->storage_class == SCLASS_EH_REGION_SUPP)
      if (!ST_is_not_used(st)) {
        INITV_IDX inv = ino->val;
        fprintf(fp, "EH Symbol [%d]: \n", num++);
        fprintf(fp, "\tName:   %s\n", ST_name(st));
	EH_Dump_INITV (inv, fp, 1);
      }
  }
}

static void
Print_EH_Range(EH_RANGE& range, FILE* fp, const int i)
{
  fprintf (fp, "-----------------------------------------------------------------------\n");
  fprintf(fp, "EH_RANGE [%d]: (%p)\n", i, &range);
  fprintf(fp, "\t[0]. kind        = %d\n", (int)range.kind);
  fprintf(fp, "\t[1]. ereg_supp   = %d\n", (int)range.ereg_supp);
  fprintf(fp, "\t[2]. has_call    = %d\n", (int)range.has_call);
  if (range.start_label && range.end_label) {
    fprintf(fp, "\t[3]. start_label = %d (%d)(%s)\n", (int)range.start_label,
	Label_Table[range.start_label].kind, LABEL_name(Label_Table[range.start_label]));
    fprintf(fp, "\t[4]. end_label   = %d (%d)(%s)\n", (int)range.end_label,
          Label_Table[range.end_label].kind, LABEL_name(Label_Table[range.end_label]));
  }
  fprintf(fp, "\t[5]. parent      = %p\n", range.parent);
  
  if (range.ereg_supp == 0)	return;
  INITV_IDX blk = INITO_val(range.ereg_supp);
  fprintf(fp, "\t[6]. ereg_supp.INITV.kind = %d, INITV_IDX: %d",INITV_kind(blk), blk);
  if (INITV_kind(blk) == INITVKIND_LABEL) {
    LABEL& lab = Label_Table[INITV_lab(blk)];
    fprintf(fp, ", lab:%d (%d)(%s)", INITV_lab(blk), lab.kind, LABEL_name(lab));
  }
  fprintf(fp, "\n");
  
  if (INITV_kind(blk) != INITVKIND_BLOCK)  return;

  fprintf(fp, "\t[7]. The corresponding action info:\n");
  INITV_IDX first = INITV_blk(blk);
  for (INITV_IDX tmp = first; tmp; tmp = INITV_next(tmp)) {
    fprintf(fp, "\t\t.INITV.kind = %d", (int)INITV_kind(tmp));
    if (INITV_kind(tmp) == INITVKIND_LABEL) {
      LABEL& lab = Label_Table[INITV_lab(tmp)];
      fprintf(fp, ", lab: %d (%s)(%s)", INITV_lab(tmp), LABEL_name(lab), Get_LABEL_Kind (lab.kind));
    }
    if (INITV_kind(tmp) == INITVKIND_VAL) {
      int sym = TCON_ival(INITV_tc_val(tmp));
      fprintf(fp, ", value = 0x%08x", sym);
    }
    if (INITV_kind(tmp) == INITVKIND_ZERO) {
      fprintf (fp, ", value = 0");
    }
    fprintf(fp, "\n");
  }
}

void 
EH_Print_Range_List (void)
{
  INT32 i;
  for (i = 0; i < range_list.size(); i++) {
    EH_RANGE& range = range_list[i];
    
    // dump the info of each EH range in current PU
    if (Get_Trace (TP_EH, 0x0002)) {
      Print_EH_Range(range, TFile, i);
    }
  }		                   
  fprintf (TFile, "-----------------------------------------------------------------------\n");
}

void
Print_PU_EH_Entry(PU& pu, ST* pu_st, FILE* fp)
{
  INITO_IDX ino_idx = pu.misc;
  /*
   *    .eh_info (INITO) = <etable (ST), exc_ptr_iv (INITV)>
   *			--> exc_ptr_iv(__Exc_Ptr__)			(ST_IDX)
   *			--> filter_iv (___Exc_Filter__)			(ST_IDX)
   *			--> tinfo (type filter entry, 0 if none)	(INITO_IDX)
   *			--> eh_spec (eh spec, 0 if none)		(INITO_IDX)
   */
  fprintf (fp, "\n=======================================================================\n");
  fprintf (fp, "\t EH ENTRY INFO for PU: %s \t\n", ST_name(pu_st));
  fprintf (fp, "=======================================================================\n");
  if (ino_idx == INITO_IDX_ZERO) {
    fprintf(fp, "\tno eh entry\n");
    return;
  }

  INITV_IDX exc_ptr_iv = INITO_val(ino_idx);
  INITV_IDX filter_iv  = INITV_next(exc_ptr_iv);
  INITV_IDX tinfo      = INITV_next(filter_iv);
  INITV_IDX eh_spec    = INITV_next(tinfo);
  
  fprintf (TFile, "\t[0].");
  Print_INITV_idx (exc_ptr_iv);
  fprintf (TFile, "\t[1].");
  Print_INITV_idx (filter_iv);
  
  /*	
   * id (INITO) = <typeinfo (ST), start (INITV)> (this is stored in tinfo)
   * -->[blk <typest --> filter>]+
   */
  INITO_IDX id 	= TCON_uval(INITV_tc_val(tinfo));
  ST*	    st;
  INITV_IDX blk;
 
  if (id != 0) {
    st  = INITO_st(id);
    blk = INITO_val(id);
  
    fprintf(fp, "\t[2]. tinfo list:");
    while (blk != INITV_IDX_ZERO) {
      INITV_IDX type_st_iv = INITV_blk(blk);
      int filter = TCON_ival(INITV_tc_val(INITV_next(type_st_iv)));
      ST_IDX    type_st_idx = 0;
      if (INITVKIND_ZERO != INITV_kind(type_st_iv))
        type_st_idx = TCON_uval(INITV_tc_val(type_st_iv));

      fprintf(fp, "ST_IDX = 0x%08x [%s (%d)], filter = %d\n", type_st_idx,
              type_st_idx ? ST_name(&St_Table[type_st_idx]) : "*ALL*", 
 	      type_st_idx ? (int)(!ST_is_not_used(&St_Table[type_st_idx])) : 0,
	      filter);
      blk = INITV_next(blk);
    }
  }
  else
    fprintf(fp, "\t[2]. tinfo list: <none>\n");
   
  /*	
   * id (INITO) = <eh_spec (ST), start (INITV)>  (this is stored in eh_spec)
   * -->block	[ st]+
   */  
  id = TCON_uval(INITV_tc_val(eh_spec));
  if (id != 0) {
    st  = INITO_st(id);
    blk = INITO_val(id);

    fprintf(fp, "\t[3]. eh_spec list:");
    FmtAssert (INITV_kind(blk) == INITVKIND_BLOCK, ("root initv for eh_spec must be block"));
    INITV_IDX type_st_iv = INITV_blk(blk);
    while (type_st_iv != INITV_IDX_ZERO) {
      ST_IDX    type_st_idx = 0;
      if (INITVKIND_ZERO == INITV_kind(type_st_iv))
	      break;
      
      type_st_idx = TCON_uval(INITV_tc_val(type_st_iv));
      fprintf(fp, "0x%08x [%s]\n", type_st_idx, 
              type_st_idx ? ST_name(&St_Table[type_st_idx]):"*END*");
      type_st_iv = INITV_next(type_st_iv);
    }
  }
  else
    fprintf(fp, "\t[3]. eh_spec list: <none>\n");  
  
  fprintf(fp, "\n");
}


static void
INITV_Init_Integer_2(INITV_IDX inv, TYPE_ID mtype, INT64 val, UINT16 repeat)
{
    if (val == 0)
	INITV_Set_ZERO (Initv_Table[inv], mtype, repeat);
    else {
    	TCON tc  = Host_To_Targ (mtype, val);
    	INITV_Set_VAL (Initv_Table[inv], Enter_tcon(tc), repeat);
    }
}

static void
Check_Initv(INITV_IDX idx, FILE* fp)
{
  if (idx == 0) return;
  fprintf(fp, "idx = %d, type = %d, val = %d\n", (int)idx, (int)INITV_kind(idx),
      (INITV_kind(idx) == INITVKIND_VAL ? TCON_ival(INITV_tc_val(idx)) : -1));
  FmtAssert(INITVKIND_UNK != INITV_kind(idx), ("INITV.kind = UNKNOWN\n"));
  if (INITVKIND_BLOCK == INITV_kind(idx))
    Check_Initv(INITV_blk(idx), fp);
  Check_Initv(INITV_next(idx), fp);
}

static INITO* eh_pu_range_inito = NULL;

INITO*
EH_Get_PU_Range_INITO(bool bSetNull)
{
  INITO* ret = eh_pu_range_inito;
  if (bSetNull == true)
    eh_pu_range_inito = NULL;
  return ret;
}




#ifdef TARG_IA64 
bool pu_need_LSDA;
/* This trick seems that only reasonable on IA64, you can refer to 
   eh_personality routine in libstdc++ for more details.
   Is there difference in libstdc++ between IA64 and X8664?
 */

// check whether need not to create INITO for LSDA
// Another way: check if exception type info stored in INITO pu.misc is NULL/ZERO
bool
PU_Need_Not_Create_LSDA ()
{
  bool flag = true;
  for (INT32 i = 0; i < range_list.size(); i++) {
    EH_RANGE& range = range_list[i];
    if (range.ereg_supp == 0)	continue;
    ST* st = INITO_st(range.ereg_supp);
    if (ST_is_not_used(st)) 	continue;

    INITV_IDX blk = INITO_val(range.ereg_supp);
    if (INITV_kind(blk) != INITVKIND_BLOCK) 
      continue;

    INITV_IDX first = INITV_blk(blk);
    if (INITV_kind(first) == INITVKIND_LABEL) {
      flag = false;
      return flag;
    }
  }
  if (flag) 
    for (INT32 i = 0; i < range_list.size(); i++) {
      EH_RANGE& range = range_list[i];
      if (range.ereg_supp != 0) {
        ST* st = INITO_st(range.ereg_supp);
        Set_ST_is_not_used(st);
      }
    }
  return flag;
}

/* There are subtle difference between IA64 and X8664 when creating INITO for landing pad.
   TODO: need to merge them?
 */
static void
Create_INITO_For_Range_Table(ST * st, ST * pu)
{
  INITV_IDX eh_spec_iv = Get_TF_Map_and_EH_Spec_List(Get_Current_PU(), tfmap);
  INITO_IDX inito = New_INITO(st);
  INITV_IDX inv_blk = New_INITV();
  INITV_IDX inv, prev_inv, cinv, inv_action, backup;
  int act_offset = 1;	// biased by 1

  eh_pu_range_inito = &Inito_Table[inito];

  /*
   * inito-> inv_blk -> list of INITVs (start with inv)

   * [inv]			-> mark where action table starts
   * call-site-table-initvS
   * [inv-action]		-> mark where type table starts
   * action-table-initvS
   * [cinv]			-> mark where eh-spec table starts
   * single-type-table	
   * eh-spec-table		
  */
  Set_INITO_val(inito, inv_blk);
  inv = New_INITV();
  INITV_Init_Block(inv_blk, inv);

#define WINUX_ALLOC_INV(inv) 	\
  prev_inv = inv;		\
  inv = New_INITV();		\
  Set_INITV_next(prev_inv, inv);

  // prepare action table, start with cinv
  inv_action = cinv = New_INITV();
  INITV_Init_Integer_2(inv, MTYPE_U4, inv_action, 1);	// mark where action table start:)

  // call-site table and action table  
  for (INT32 i = 0; i < range_list.size(); i++) {
    EH_RANGE& range = range_list[i];
    if (range.ereg_supp == 0)	continue;
    ST* st = INITO_st(range.ereg_supp);
    if (ST_is_not_used(st)) 	continue;

    RID_num_eh_ranges(range.rid) --;
    if ( RID_num_eh_ranges(range.rid) == 0 ) // all flatten eh_ranges has been processed
      Set_ST_is_not_used(st);

    INITV_IDX blk = INITO_val(range.ereg_supp);
    if (INITV_kind(blk) != INITVKIND_BLOCK) {
	    Set_ST_is_not_used(st);
	    continue;
    }
    FmtAssert(INITV_kind(blk) == INITVKIND_BLOCK, 
		    ("eh_range.ereg_supp.inito.initv.kind != block, %d\n", INITV_kind(blk)));
    
    INITV_IDX first = INITV_blk(blk);

#ifdef OSP_OPT
    if ((INITV_kind(first) != INITVKIND_LABEL) &&
	        PU_is_mainpu (Get_Current_PU ())) {
      continue;
    }
#endif
    /*
     * struct CallSiteRecord
     * {
     *    char*	cs_start; //	offset to Start IP of current proc
     *    char*	cs_len;	  //	length to the next call-site?
     *    char*	cs_lp;	  //	ladding pad offset to lpStart
     *    char*	cs_action;//    the first action table offset (biased by 1, 0 indicates there are no
     *                          actions)
     * };
     */ 
    // call-site record
 
    // cs_start
    WINUX_ALLOC_INV(inv)
    INITV_Init_Symdiff(inv, range.start_label, pu, !Use_Long_EH_Range_Offsets());

    // cs_len (we have to init two labels instead of Symdiff
    WINUX_ALLOC_INV(inv)
    INITV_Init_Label(inv, range.start_label, 1);
    WINUX_ALLOC_INV(inv)
    INITV_Init_Label(inv, range.end_label, 1);

    // cs_lp
    WINUX_ALLOC_INV(inv)
    bool bHasLandingPad = true;
    if (INITV_kind(first) == INITVKIND_LABEL) {
      INITV_Init_Symdiff(inv, INITV_lab(first), pu, !Use_Long_EH_Range_Offsets());
    }
    else {	// no landing pad
      INITV_Init_Integer_2(inv, MTYPE_U4, 0, 1);
      bHasLandingPad = false;
    }

    // cs_action pointer
    WINUX_ALLOC_INV(inv)
    INITV_Init_Integer_2(inv, MTYPE_U4, act_offset, 1);

    /*
     * struct ActionRecord
     * {
     *   uint16	ar_filter;	
     *   ==0, cleanup, 
     *   > 0, exception handler, ar_filter is an index to the type table, ttType.
     *   < 0, exception specification, ar_filter is a offset to the type list table, ttType
     *   example, throw (B,C)then ar_filter = -1, ttType[-1] = (2,3,0), ttType[2] = B, ttType[3] = 
     *   uint16	ar_disp;	
     *   next action record = &ar_disp + ar_disp (if ar_disp != 0) 
     * };
     */ 
    // action record
    int ar_count = 0;
    bool bNeedCleanup = false;
    for (INITV_IDX next = INITV_next(first); next; next = INITV_next(next)) {
      // begin write action record (cinv, next)
      int filter = 0;
      if (INITVKIND_VAL == INITV_kind(next))
	filter = TCON_ival(INITV_tc_val(next));

      if (filter > 0) { // handler
        filter = Get_EH_Filter_By_Type(filter, tfmap);
	FmtAssert(bHasLandingPad, ("Landing pad must exist for handler."));
      }
      else if (filter < 0) {// eh-spec 
      } 
      else {  // filter = 0, eh-spec or catch-all or cleanup?
        if (INITV_next(next)) {
          INITV_IDX next_tmp = INITV_next(next);
          if (INITVKIND_VAL == INITV_kind(next_tmp))
            if (TCON_ival(INITV_tc_val(next_tmp)) < 0) { // eh-spec
              continue;	// omit current mark (0)
            }
        }

	if (bHasLandingPad == false) continue;

        // catch all or clean-up
	// FmtAssert(bHasLandingPad, ("Landing pad must exist for catch-all or cleanup."));
	filter = Get_EH_Filter_By_Type(filter, tfmap);
        if (filter == 0) {
		bNeedCleanup = true;
		continue; // cleanup
	}
      }

      ar_count++;
      WINUX_ALLOC_INV(cinv)
      INITV_Init_Integer_2(cinv, MTYPE_I4, filter, 1); // ar_filter
      
      WINUX_ALLOC_INV(cinv)
      if (INITV_next(next) == 0) {
	INITV_Init_Integer_2(cinv, MTYPE_I4, 0, 1); // ar_next
      }
      else {
	INITV_Init_Integer_2(cinv, MTYPE_I4, 1, 1);
      } 
      act_offset += sizeof_signed_leb128(filter) + 1;
      // end write action record
    } // end action record .for

    if (bNeedCleanup && ar_count) {
      FmtAssert(bHasLandingPad, ("Landing pad must exist for cleanup"));
      INITV_Init_Integer_2(cinv, MTYPE_I4, 1, 1);	// reset ar_next, not finished yet
      
      WINUX_ALLOC_INV(cinv)
      INITV_Init_Integer_2(cinv, MTYPE_I4, 0, 1);	// ar_filter
      WINUX_ALLOC_INV(cinv)
      INITV_Init_Integer_2(cinv, MTYPE_I4, 0, 1);    	// ar_next, end of action record list

      act_offset += 2;
    }
    if (ar_count == 0)
      INITV_Init_Integer_2(inv, MTYPE_U4, 0, 1);
    else
      INITV_Init_Integer_2(cinv, MTYPE_U4, 0, 1);
      
  } // end range_list.for

  Set_INITV_next(inv, inv_action);

  WINUX_ALLOC_INV(cinv)
  backup = INITV_next(inv_action);
  INITV_Init_Integer_2(inv_action, MTYPE_U4, cinv, 1); // mark where type table start:)
  Set_INITV_next(inv_action, backup);
  inv_action = cinv;

  // single-type table
  int maxft = Convert_TF_Map_To_FT_Map(tfmap, ftmap);
  for (int i=maxft; i >= 1; i--) {
    ST_IDX ix = Get_EH_ST_By_Filter(i, ftmap);
    WINUX_ALLOC_INV(cinv)
    INITV_Init_Integer_2(cinv, MTYPE_U4, ix, 1); 	// ST_IDX
  }
/*  
  TF_MAP::iterator it;
  for (it = tfmap.begin(); it != tfmap.end(); it++) {
    WINUX_ALLOC_INV(cinv)
    INITV_Init_Integer_2(cinv, MTYPE_U4, it->first, 1); // ST_IDX
  }
*/
  WINUX_ALLOC_INV(cinv)
  backup = INITV_next(inv_action);
  INITV_Init_Integer_2(inv_action, MTYPE_U4, cinv, 1); // mark where eh-spec-table starts;
  Set_INITV_next(inv_action, backup);
  INITV_Init_Integer_2(cinv, MTYPE_U4, cinv, 1);

  // eh-spec table
  int eh_filter = 0;
  for (INITV_IDX next = eh_spec_iv; next; next = INITV_next(next)) {
    WINUX_ALLOC_INV(cinv)
    eh_filter = 0;
    if (INITVKIND_ZERO != INITV_kind(next)) {
      eh_filter = Get_EH_Filter_By_Type(TCON_ival(INITV_tc_val(next)), tfmap);
    }
    INITV_Init_Integer_2(cinv, MTYPE_I4, eh_filter, 1);
  }
  if (eh_filter != 0) {
    WINUX_ALLOC_INV(cinv)
    INITV_Init_Integer_2(cinv, MTYPE_I1, 0, 1);
  }
#undef WINUX_ALLOC_INV
}

#else // TARG_IA64

/* implementation on X8664. 
 */
#ifdef KEY
static void
Create_INITO_For_Range_Table(ST * st, ST * pu)
{
  INITV_IDX blk=0, start, action_table_start, prev_action_start;
// This gives the offset into the action table, which is output as the
// 4th field in a call-site record.
  int running_ofst=1;
  int bytes_for_filter;
  INITO_IDX tmp = PU_misc_info (Get_Current_PU());

  INITO* eh_spec = (tmp) ? Create_Type_Filter_Map () : NULL ;

  vector<INITV_IDX> action_chains;

  // process the exception range list
  for (INT32 i=0; i<range_list.size(); ++i)
  {
    // region start
    INITV_IDX begin = New_INITV();
    INITV_Init_Symdiff (begin, range_list[i].start_label,
			   pu, !Use_Long_EH_Range_Offsets());

// We ideally need a INITV_Init_Symdiff which could store the difference
// between 2 labels. Without that, we emit the 2 labels and then check
// for the pattern in cgemit
    INITV_IDX high_pc = New_INITV();
    INITV_Init_Label (high_pc, range_list[i].end_label, 1);
    Set_INITV_next (begin, high_pc);

    INITV_IDX low_pc = New_INITV();
    INITV_Init_Label (low_pc, range_list[i].start_label, 1);
    Set_INITV_next (high_pc, low_pc);

    // landing pad
    INITO_IDX ereg = range_list[i].ereg_supp;
    INITV_IDX first_initv = INITV_blk (INITO_val (ereg));
    Set_ST_is_not_used (*(INITO_st (ereg)));

    LABEL_IDX pad_label=0;
    if (INITV_kind(first_initv) != INITVKIND_ZERO)
    	pad_label = INITV_lab (first_initv);

    INITV_IDX pad = New_INITV();
    if (pad_label)
    	INITV_Init_Symdiff (pad, pad_label, pu, !Use_Long_EH_Range_Offsets());
    else
	INITV_Set_ZERO (Initv_Table[pad], MTYPE_U4, 1);
    Set_INITV_next (low_pc, pad);

    // first action index
    // build chain of actions
    FmtAssert (INITV_next (first_initv) != 0, ("No handler information available"));
    INITV_IDX action_ofst = 0;
    INITV_IDX first_action = New_INITV();
    for (INITV_IDX next_initv=INITV_next (first_initv);
    		next_initv; next_initv=INITV_next (next_initv))
    {
    	INITV_IDX action = New_INITV();
	// The special value INITVKIND_ONE represents a catch-all handler.
	// A zero in the list means no handler, although there may still
	// be a landing pad (cleanup).
	int sym = 0;
	bool catch_all = false;
	if (INITV_kind(next_initv) == INITVKIND_ONE) {
	    FmtAssert (pad_label, ("Catch-all with no landing pad"));
	    catch_all = true;
	}
	else if (INITV_kind(next_initv) != INITVKIND_ZERO)
	    sym = TCON_uval(INITV_tc_val (next_initv));

	// action field
	// Check if we have any action for this eh-region, if not, emit 0
	// for action start marker.
	bool zero_action = false;
	if (sym < 0) // eh spec offset
	{
    	    INITV_Set_VAL (Initv_Table[action],
		Enter_tcon (Host_To_Targ (MTYPE_I4,sym)), 1);
	    bytes_for_filter = sizeof_signed_leb128 (sym);
	}
	else if (sym || catch_all)
	{
    	    INITV_Set_VAL (Initv_Table[action],
		Enter_tcon (Host_To_Targ (MTYPE_I4,type_filter_map[sym])), 1);
	    bytes_for_filter = sizeof_signed_leb128 (type_filter_map[sym]);
	}
	else 
	{
	    INITV_Set_ZERO (Initv_Table[action], MTYPE_I4, 1);
	    zero_action = true;
	    bytes_for_filter = 1;
	}

    	if (!action_ofst)
	{
	    // store the head of each action chain
	    action_chains.push_back (action);

	    // action start marker for call-site record
	    if (zero_action && !INITV_next (next_initv))
	    {
	      // There is no action-record for this eh-region, so mark the
	      // action-record ofst as zero.
	      INITV_Set_ZERO (Initv_Table[first_action], MTYPE_I4, 1);
	    }
	    else
	      INITV_Set_VAL (Initv_Table[first_action],
		Enter_tcon (Host_To_Targ (MTYPE_I4, running_ofst)), 1);
	    // store offset into first action **Note: not the filter, but offset to it
	    Set_INITV_next (pad, first_action);
	}

	if (action_ofst)    Set_INITV_next (action_ofst, action);

// offset to next action. currently it is either 1 or 0, 0 indicates this
// is the last action.
    	action_ofst = New_INITV();
	if (INITV_next (next_initv))
	    INITV_Set_ONE (Initv_Table[action_ofst], MTYPE_I4, 1);
	else
	    INITV_Set_ZERO (Initv_Table[action_ofst], MTYPE_I4, 1);
	Set_INITV_next (action, action_ofst);
	running_ofst += (1 + bytes_for_filter);
    }

    if (i == 0)
    {
    	blk = start = New_INITV();
	INITV_Init_Block (blk, begin);
    }
    else
    {// the entire call site table is taken as a single array of mixed types
    // instead of being an array of structures. Thus, the array elements are
    // r0 start, r0 end, l0, a0, r1 start, r1 end, l1, a1, ...
	Set_INITV_next (prev_action_start, begin);
	/*
    	INITV_IDX next_blk = New_INITV();
	INITV_Init_Block (next_blk, begin);
	Set_INITV_next (blk, next_blk);
	blk = next_blk;
	*/
    }
    if (i == (range_list.size()-1))
    {
  	INITO_IDX inito = New_INITO (ST_st_idx(st), start);
    }
    prev_action_start = first_action;
  }

  INITV_IDX action_blk=0, prev_action_blk=0;
  if (action_chains.size())
  {
  	action_blk = New_INITV();
	INITV_Init_Block (action_blk, action_chains[0], 1, INITVFLAGS_ACTION_REC);
	Set_INITV_next (blk, action_blk);
	prev_action_blk = action_blk;
  }

  for (INT32 i=1; i<action_chains.size(); ++i)
  {
  	action_blk = New_INITV();
	INITV_Init_Block (action_blk, action_chains[i], 1, INITVFLAGS_ACTION_REC);
	Set_INITV_next (prev_action_blk, action_blk);
	prev_action_blk = action_blk;
  }

  vector<type_filter_entry> ti;
  for (map<ST_IDX, int, cmpst>::iterator i=type_filter_map.begin();
  		i != type_filter_map.end(); ++i)
  {
	type_filter_entry f;
	f.st = (*i).first;
	f.filter = (*i).second;
	ti.push_back (f);
  }
  if (!ti.empty()) sort (ti.begin(), ti.end(), sort_on_filter());

  // Search for any other EH information the front-end has sent
  INITV_IDX prev=0;
  for (vector<type_filter_entry>::iterator i = ti.begin();
		i != ti.end(); ++i)
  {	// Process EH information from FE (currently only typeinfo)
	INITV_IDX type = New_INITV();
	if ((*i).st)
	    INITV_Init_Symoff (type, &St_Table [(*i).st] , 0);
	else
	    INITV_Set_ZERO (Initv_Table[type], 
	    		    (Is_Target_64bit() ? MTYPE_U8 : MTYPE_U4), 1);
	if (prev)	Set_INITV_next (prev, type);
	else		start = type;	// mark the first one
	prev = type;
  }
  INITV_IDX type_blk=0;
  if (prev)
  {
	type_blk = New_INITV();
	INITV_Init_Block (type_blk, start, 1, INITVFLAGS_TYPEINFO);
	if (action_blk)
	    Set_INITV_next (action_blk, type_blk);
	else if (blk)
	    Set_INITV_next (blk, type_blk);
  }
  if (eh_spec)
  {
  	prev=0;
	INITV_IDX first_eh_spec = INITV_blk (INITO_val (*eh_spec));
	FmtAssert (first_eh_spec, ("Empty EH specification"));
	for (; first_eh_spec; first_eh_spec = INITV_next (first_eh_spec))
	{
	    ST_IDX sym = 0;
	    if (INITV_kind (first_eh_spec) != INITVKIND_ZERO)
	    	sym = TCON_uval(INITV_tc_val (first_eh_spec));
	    INITV_IDX spec = New_INITV();
	    if (sym)
    	      INITV_Set_VAL (Initv_Table[spec],
		 Enter_tcon (Host_To_Targ (MTYPE_I4,type_filter_map[sym])), 1);
	    else INITV_Set_ZERO (Initv_Table[spec], MTYPE_I4, 1);
	    if (prev)	Set_INITV_next (prev, spec);
	    else	start = spec;
	    prev = spec;
	}
	INITV_IDX spec_blk = New_INITV();
	INITV_Init_Block (spec_blk, start, 1, INITVFLAGS_EH_SPEC);
	if (type_blk)
	    Set_INITV_next (type_blk, spec_blk);
	else if (action_blk)
	    Set_INITV_next (action_blk, spec_blk);
	else if (blk)
	    Set_INITV_next (blk, spec_blk);
  }
  // Done with this PU
  type_filter_map.clear();
}

#else
/* The first implementation, not conform to the C++ ABI.
   NO call site table, type table, action table, .etc. 
 */
static void
Create_INITO_For_Range_Table(ST * st, ST * pu)
{
  INITO_IDX inito = New_INITO(st);
  INITV_IDX inv_blk = New_INITV ();
  INITV_IDX inv;
  INITV_IDX prev_inv;

  // create block of blocks
  prev_inv = Append_INITV (inv_blk, inito, INITV_IDX_ZERO);
  inv_blk = New_INITV ();
  INITV_Init_Block(prev_inv, inv_blk);
  // header: pad(31), short/long(1), version(16), count(16)
  inv = New_INITV ();
  INITV_Init_Integer (inv, MTYPE_I4, 
               	         (Use_Long_EH_Range_Offsets() ? LONG_OFFSETS 
						      : SHORT_OFFSETS) );
  INITV_Init_Block (inv_blk, inv);
  prev_inv = inv;
  inv = New_INITV ();
  INITV_Init_Integer (inv, MTYPE_I2, HEADER_VERSION);
  prev_inv = Append_INITV(inv, INITO_IDX_ZERO, prev_inv);
  inv = New_INITV ();
  INITV_Init_Integer (inv, MTYPE_I2, range_list.size());
  prev_inv = Append_INITV(inv, INITO_IDX_ZERO, prev_inv);

  for (INT32 i = 0; i < range_list.size(); i++) {
    /* block for each range */
    inv_blk = Append_INITV (New_INITV (), INITO_IDX_ZERO, inv_blk);
    // supp(32), parent(16), pad(14), kind(2), low(16/32), high(16/32)
    inv = New_INITV();
    if (range_list[i].ereg_supp == 0)
        INITV_Init_Integer (inv, MTYPE_I4, 0);
    else
        INITV_Init_Symoff (inv, INITO_st(range_list[i].ereg_supp), 0);
    INITV_Init_Block (inv_blk, inv);
    prev_inv = inv;
    inv = New_INITV();
    INITV_Init_Integer (inv, MTYPE_I2, parent_offset(i));
    prev_inv = Append_INITV(inv, INITO_IDX_ZERO, prev_inv);
    inv = New_INITV();
    INITV_Init_Integer (inv, MTYPE_I2, range_list[i].kind);
    prev_inv = Append_INITV(inv, INITO_IDX_ZERO, prev_inv);
    inv = New_INITV();
    INITV_Init_Symdiff (inv, range_list[i].start_label,
			   pu, !Use_Long_EH_Range_Offsets());
    prev_inv = Append_INITV(inv, INITO_IDX_ZERO, prev_inv);
    inv = New_INITV();
    INITV_Init_Symdiff (inv, range_list[i].end_label,
			   pu, !Use_Long_EH_Range_Offsets());
    prev_inv = Append_INITV(inv, INITO_IDX_ZERO, prev_inv);
  }
}
#endif // KEY
#endif // TARG_IA64 


void
EH_Dump_LSDA (FILE *fp)
{
  if (!EH_Get_PU_Range_INITO(false) || !EH_Get_PU_Range_ST())
    return;
  fprintf (TFile, "=======================================================================\n");
  fprintf (fp, "\t\tLSDA sturcture of PU:%s\n", ST_name (Get_Current_PU_ST()));
  fprintf (TFile, "=======================================================================\n");
  
  ST* eh_range_table = EH_Get_PU_Range_ST();
  INITO *ino = EH_Get_PU_Range_INITO(false);

  char* sym_name = ST_name(eh_range_table);
  FmtAssert(INITO_st(ino) == eh_range_table, ("Write_LSDA_INITO.st and inito are not paired.\n"));
  FmtAssert(sym_name != NULL &&
            strncmp(sym_name, ".range_table.", strlen(".range_table.")) == 0,	
	    ("Write_LSDA_INITO.ST name = %s\n", sym_name ? sym_name : "<null>"));  
  
  INITV_IDX inv_blk = INITO_val(*ino);
  FmtAssert(INITVKIND_BLOCK == INITV_kind(inv_blk), ("RangeTable.Initv1.kind != BLOCK\n"));
  INITV_IDX first = INITV_blk(inv_blk);
  INITV_IDX act_inv = (INITV_IDX)TCON_uval(INITV_tc_val(first));
  INITV_IDX type_inv = (INITV_IDX)TCON_uval(INITV_tc_val(act_inv));
  INITV_IDX eh_spec_inv = (INITV_IDX)TCON_uval(INITV_tc_val(type_inv));

  INITV_IDX inv;
  inv = INITV_next(first);
 
  // call site table
  fprintf (fp, "\n--------------------------- CALL SITE TABLE ---------------------------\n");
  for(int i = 0;inv && inv != act_inv; inv = INITV_next(inv), i++) {
    INITV_IDX prev_inv;
    fprintf (fp, "Call Site Record [%d]:\n", i);

    // cs_start (SymDiff)
    FmtAssert (INITV_kind(inv) == INITVKIND_SYMDIFF || INITV_kind(inv) == INITVKIND_SYMDIFF16, 
		    ("@start != INITVKIND_SYMDIFF."));
    fprintf (fp, "\t[0]. cs_start:\t\t");
    Print_INITV_idx (inv);

    inv = INITV_next(inv);
    // cs_len: range->start_label
    fprintf (fp, "\t[1]. start_label:\t");
    Print_INITV_idx (inv);
    inv = INITV_next(inv);
    // cs_len: range->end_label
    fprintf (fp, "\t[2]. end_label:\t\t");
    Print_INITV_idx (inv);      

    inv = INITV_next(inv);
    // cs_lp: landing pad pointer
    if (INITVKIND_ZERO != INITV_kind(inv)) {
      FmtAssert(INITVKIND_SYMDIFF == INITV_kind(inv) ||
	        INITVKIND_SYMDIFF16 == INITV_kind(inv), ("CS_lp.kind != INITVKIND_SYMDIFF."));
      fprintf (fp, "\t[3]. landing pad:\t");
      Print_INITV_idx (inv);
    }
    else {
      fprintf (fp, "\t[3]. NO landing pad:\t");
      Print_INITV_idx (inv);
    }

    inv = INITV_next(inv);
    // cs_action:offset in the action table
    fprintf (fp, "\t[4]. cs_action:\t\t");
    Print_INITV_idx (inv);
  }
  
  // action table
  inv = INITV_next(act_inv);
  fprintf (fp, "\n---------------------------- ACTION TABLE -----------------------------\n");
  for(int i = 0; inv && inv != type_inv; inv = INITV_next(inv), i++) {
    fprintf (fp, "Action Record [%d]:\n", i);
    // ar_filter
    fprintf (fp, "\t[0]. ar_filter:\t\t");
    Print_INITV_idx (inv);
   
    // ar_next
    fprintf (fp, "\t[1]. ar_next:\t\t");
    Print_INITV_idx (inv);
  }
  
  // type table
  inv = INITV_next(type_inv);
  fprintf (fp, "\n------------------------------ TYPE TABLE ------------------------------\n");
  for(int i = 0; inv && inv != eh_spec_inv; inv = INITV_next(inv)) {
    int eh_filter = 0;
    ST_IDX type_st_idx = 0;
    fprintf (fp, "Exceptions Object [%d]:\n", i);
    fprintf (fp, "\t[0]. ");
    Print_INITV_idx (inv);
    if (INITVKIND_ZERO != INITV_kind(inv))
      type_st_idx = TCON_uval(INITV_tc_val(inv));
    if (type_st_idx == 0)
      fprintf(fp, "\t[1]. type_st_idx == 0\n");
    else {
      fprintf (fp, "\t[1]. exceptions object: %s\n", ST_name (type_st_idx));
      eh_filter = Get_EH_Filter_By_Type (type_st_idx, tfmap);
      fprintf (fp, "\t[2]. eh_filter: %d\n", eh_filter);
    }
  }

  // type_spec table
  fprintf (fp, "\n------------------------ TYPE SPECIFICATION TABLE -----------------------\n");
  fprintf (fp, "Exception Objects Type in eh_spec:");
  inv = INITV_next(eh_spec_inv);
  for(int i = 0; inv; inv = INITV_next(inv), i++) {
    ST_IDX type_st_idx;
    fprintf (fp, "\t[%d]. ", i);
    Print_INITV_idx (inv);
    if (INITVKIND_ZERO != INITV_kind(inv)) {
      type_st_idx = Get_EH_ST_By_Filter (TCON_ival(INITV_tc_val(inv)), ftmap);
      fprintf (fp, "%s\n", ST_name (type_st_idx));
    }
    else
      fprintf (fp, "\n");
  }
  fprintf (fp, "\n");
}

// Temporary workaround
struct SET_NOT_USED {
  SET_NOT_USED() {}
  void operator()(EH_RANGE& r) {
      Set_ST_is_not_used(INITO_st(r.ereg_supp));
  }
};

void 
EH_Write_Range_Table(WN * wn)
{
  if (range_list.size() == 0) {
    eh_pu_range_st = NULL;
    return;
  }

#ifdef KEY
  // C++ exceptions not yet supported within MP regions.
  if (!LANG_Enable_CXX_Openmp && PU_mp_lower_generated (Get_Current_PU ()))
  {
    EH_RANGE_LIST::iterator first(range_list.begin());
    EH_RANGE_LIST::iterator last(range_list.end());
    for_each  (first, last, SET_NOT_USED());
    eh_pu_range_st = NULL;
    return;
  }
#endif // KEY

  fix_mask_ranges();
#ifdef KEY
  flatten_regions();
#endif
  reorder_range_list();

  ST * st = ST_For_Range_Table(wn);
  eh_pu_range_st = st;
  Create_INITO_For_Range_Table(st, WN_st(wn));  
}

void print_label(LABEL_IDX label)
{
  Label_Table[label].Print(stderr);
}
