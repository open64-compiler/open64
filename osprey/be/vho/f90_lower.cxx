/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


#include <stdint.h>
#include "defs.h"
#include "errors.h"
#include "tracing.h"
#include "timing.h"
#include "stab.h"
#include "util.h"
#include "strtab.h"
#include "wn.h"
#include "wn_util.h"
#include "stblock.h"
#include "ir_reader.h"
#include "config.h"
#include "config_opt.h"
#include "targ_sim.h"
#include "targ_const.h"
#include "const.h"
#include "wn_map.h"
#include "wn_simp.h"
#include "cxx_template.h" 
#include "cxx_memory.h"
#include "pu_info.h"
#include "wb_f90_lower.h"

#include "intrn_info.h"

#include "f90_utils.h"

#define F90_LOWER_INTERNAL
#include "f90_lower.h"

#ifdef __GNUC__
#if ! defined(BUILD_OS_DARWIN)
#pragma weak New_Construct_Id
#endif /* defined(BUILD_OS_DARWIN) */
#endif

/* Useful macros */
#define is_constant(x) (WN_operator(x)==OPR_INTCONST)
#define ST_is_f90_pointer(x) TY_is_f90_pointer(ST_type(x))

/* To keep C++ from complaining */
#define WN_GET_INTRINSIC(x) (INTRINSIC) WN_intrinsic(x)

/* Tracing info */
#define TRACE_DEPENDENCE 2
#define TRACE_INSERTIONS 4
#define TRACE_DOLOOPS 8
#define TRACE_DEPENDENCE_ANALYSIS 0x20
#define TRACE_SYMTAB 0x1
#define TRACE_TRANSFORMATIONALS 0x40
#define TRACE_COPIES 0x80
#define TRACE_DEPINFO 0x100

#ifdef KEY
extern void Set_addr_saved_expr(WN *, BOOL);
#endif

static BOOL trace_dependence;
static BOOL trace_depinfo;

typedef struct f90lower_aux_data_s {
  WN *prelist;  /* BLOCK of statements to add before current statement */
  WN *postlist; /* BLOCK  of statements to add after current statement */
  WN *alloc_prelist; /* BLOCK of allocations to add before current statement */
  WN *dealloc_postlist;  /* BLOCK of deallocations to add after current statement */
  WN **iter_count;     /* pointer to array of WHIRL nodes giving iteration counts for
			   DO loops */
  mINT16 *perm_index;  /* Array of permutation indices */
  DIR_FLAG *directions; /* Array of loop directions */
  mINT16  ndim;   /* Dimensionality of the construct */
  COPY_FLAG_T copy_flag:4;
  BOOL    known_independent:2; /* If we know that we don't need to do dependence analysis */
} F90_LOWER_AUX_DATA;


/* Memory management */
static MEM_POOL f90_lower_pool_s;  /* Place to put auxilliary data structures */
static MEM_POOL *f90_lower_pool=NULL;

typedef struct f90_dep_info_s {
   INT ndim;
   DEP_SUMMARY summary;
   DIR_FLAG directions[MAX_NDIM];
} DEP_INFO;

#define DEP_NDIM(x) ((x)->ndim)
#define DEP_DIRECTION(x,n) ((x)->directions[(n)])
#define DEP_SUMMARY(x) ((x)->summary)
#define SET_DEP_NDIM(x,y) (x)->ndim=(y)
#define SET_DEP_SUMMARY(x,y) (x)->summary=(y)
#define SET_DEP_DIRECTION(x,n,d) (x)->directions[(n)]=(d)

/* Accessors for aux data structure */
#define PRELIST(x) ((x)->prelist)
#define POSTLIST(x) ((x)->postlist)
#define ALLOC_PRELIST(x) ((x)->alloc_prelist)
#define DEALLOC_POSTLIST(x) ((x)->dealloc_postlist)
#define ITER_COUNT(x,n) (*(((x)->iter_count) + (n)))
#define ITER_COUNT_PTR(x) ((x)->iter_count)
#define PERM_INDEX(x,n) (*(((x)->perm_index) + (n)))
#define DIRECTION(x,n) (*(((x)->directions) + (n)))
#define NDIM(x) ((x)->ndim)
#define COPY_FLAG(x) ((x)->copy_flag)
#define KNOWN_INDEPENDENT(x) ((x)->known_independent)

#define SET_PRELIST(x,y) (x)->prelist = (y)
#define SET_POSTLIST(x,y) (x)->postlist = (y)
#define SET_ALLOC_PRELIST(x,y) (x)->alloc_prelist = (y)
#define SET_DEALLOC_POSTLIST(x,y) (x)->dealloc_postlist = (y)
#define SET_ITER_COUNT(x,n,y) *(((x)->iter_count) + (n)) = (y)
#define SET_PERM_INDEX(x,n,y) *(((x)->perm_index) + (n)) = (y)
#define SET_DIRECTION(x,n,y) *(((x)->directions) + (n)) = (y)
#define SET_NDIM(x,y) (x)->ndim = (y) 
#define SET_COPY_FLAG(x,y) (x)->copy_flag = (COPY_FLAG_T) (y)
#define SET_KNOWN_INDEPENDENT(x,y) (x)->known_independent = (y)

/* To be used mainly when initializing the structures */
#define SET_ITER_COUNT_P(x,y) (x)->iter_count = (y)
#define SET_DIRECTION_P(x,y) (x)->directions = (y)
#define SET_PERM_INDEX_P(x,y) (x)->perm_index = (y)


/* WHIRL Map for the lowering data */
static WN_MAP f90_lower_map;
#define SET_F90_MAP(x,t) WN_MAP_Set(f90_lower_map,(x),(void *) (t))
#define GET_F90_MAP(x) ((F90_LOWER_AUX_DATA *) WN_MAP_Get(f90_lower_map,(x)))

/* WHIRL opcodes for doing address arithmetic/size computation */
static OPCODE OPCmpy,OPCadd,OPCsub,OPCarrsection,OPCtriplet,OPCint,OPCmod;
static INTRINSIC INTRNalloca,INTRNfree,INTRNmalloc,INTRNgetstack,INTRNsetstack;
static TYPE_ID doloop_ty;
static INT num_temps = 0;
static BOOL pointer8 = 0;
#define SELECT_OP(x,y) (pointer8 ? x : y)
static PREG_NUM pointer_return_reg;
static TY_IDX char_ty; /* A TY for character*1 */
static SRCPOS current_srcpos;

static BOOL array_statement_seen; /* Indicates that there is something to do */
static BOOL temp_allocations_inserted; /* Have we inserted any temps? */

/* Forward definitions */
static WN * F90_Lower_Walk(WN *expr, PREG_NUM *indices, INT ndim,
			   WN * block, WN *insert_point);

/* These three allow insertions before and after the loopnest being lowered at any given time */
static WN * F90_Current_Block; /* The block where the currently lowered statement is */
static WN * F90_Current_Loopnest; /* The statement containing the current loopnest. */
static WN * F90_Current_Stmt;     /* The current statement being lowered */

static WN_VECTOR F90_MP_Region;    /* The current MP region, if any */
static BOOL_VECTOR F90_MP_Region_Isworkshare;

// Keep track of the current PU
static PU *current_pu;

/* Useful macros */
#define ARREXP_SIZES(x) &WN_kid1(x)

/* This grabs the assignment statement from an array syntax statement.
 *  If the statement is a WHERE, it looks underneath for the assignment.
 *  Otherwise, it just returns stmt.
 */
static WN * get_assignment_from_stmt(WN *stmt)
{
   WN *assignment;
   if (WN_opcode(stmt) == OPC_WHERE) {
      assignment = WN_first(WN_kid1(stmt));
      if (!assignment) assignment = WN_first(WN_kid2(stmt));
   } else {
      assignment = stmt;
   }
   return(assignment);
}
      

/* 
 * Return TRUE if the subtree has an array expression 
 * or transformational anywhere in the subtree
 */

static BOOL arrayexp_in_subtree(WN *tree)
{
   WN_ITER *tree_iter;
   WN *node;
   OPERATOR opr;

   tree_iter = WN_WALK_TreeIter(tree);
   while (tree_iter) {
      node = WN_ITER_wn(tree_iter);
      opr = WN_operator(node);
      if (opr == OPR_ARRAYEXP ||
	  opr == OPR_ARRSECTION ||
	  (opr == OPR_INTRINSIC_OP && F90_Is_Transformational(WN_GET_INTRINSIC(node)))) {
	 WN_WALK_Abort(tree_iter);
	 return (TRUE);
      }
      tree_iter = WN_WALK_TreeNext(tree_iter);
   }
   return (FALSE);
}

/* 
 * Utility routine to find the first ARRSECTION node under a tree
 */
static WN *find_arrsection(WN * tree) 
{
   WN *r;
   INT num_kids,i;

   switch (WN_operator(tree)) {
    case OPR_ARRSECTION:
      return (tree);
      
    case OPR_ARRAY:
    case OPR_ARRAYEXP:
      return (find_arrsection(WN_kid0(tree)));

    default:
      num_kids = WN_kid_count(tree);
      r = NULL;
      for (i=0; i < num_kids; i++) {
	 r = find_arrsection(WN_kid(tree,i));
	 if (r) break;
      }
      return (r);
   }
}

/*
 * This routine walks an ARRSECTION node to find all the vector axes.
 * It returns the number of axes found, and sets vecaxes to contain the indices of the axes.
 *  For example, the reference A(1,3:5,7,4:6,9) would return 2, and vecaxes[0]=1,
 *  vecaxes[1]=3.
 */
   

static INT find_vector_axes(INT * vecaxes, WN *arrsect)
{
   INT numaxes,i,num_kids;
   OPERATOR opr;
   
   if (WN_operator(arrsect) != OPR_ARRSECTION) return (0);
   num_kids = (WN_kid_count(arrsect)-1)/2;
   numaxes = 0;
   for (i=0; i < num_kids; i++) {
      opr = WN_operator(WN_kid(arrsect,i+1+num_kids));
      if (opr == OPR_ARRAYEXP || opr == OPR_TRIPLET) {
	 vecaxes[numaxes] = i;
	 ++numaxes;
      }
   }
   return (numaxes);
}


/******************************************************************/
/******************************************************************
*
* General purpose statement walker routine
*
* F90_Walk_Statements(WN *tree, walker_func prewalk, walk_scf)
* 
* Walk all the statements in tree, calling prewalk on each one, and passing the BLOCK node
* containing it.
* 
* if walk_scf is TRUE, also call the prewalker on SCF nodes.
*
* prewalk is a pointer to a function of 2 arguments:
*   prewalk(WN* node, WN* containing_block). If prewalk returns FALSE, the walk terminates.
*
******************************************************************/


static WN *insert_handle;


static BOOL do_prewalk(WN * tree, WN * block, BOOL prewalk(WN * node, WN *block1)) 
{
   BOOL keep_going;
   WN *new_tree;
   WN *old_prev;
   
   /* Put the comment in the tree. This gives us a place to add the source
    * line info.
    */
   current_srcpos = WN_Get_Linenum(tree);
   if (current_srcpos) {
      old_prev = WN_prev(tree);
      WN_prev(insert_handle) = old_prev;
      WN_next(insert_handle) = tree;
      if (old_prev) WN_next(old_prev) = insert_handle;
      WN_prev(tree) = insert_handle;
      if (block) {
	 if (WN_first(block) == tree) WN_first(block) = insert_handle;
      }
      
      keep_going = prewalk(tree,block);
      /* Fix up line number information, remove insert handle */
      new_tree = WN_next(insert_handle);
      WN_Set_Linenum(new_tree,current_srcpos);
      old_prev = WN_prev(insert_handle);
      WN_prev(new_tree) = old_prev;
      if (old_prev) WN_next(old_prev) = new_tree;
      if (block) {
	 if (WN_first(block) == insert_handle) WN_first(block) = new_tree;
      }
   } else {
      keep_going = prewalk(tree,block);
   }      
   return (keep_going);
}

#ifdef KEY
#include "cxx_base.h"
class FF_STMT_NODE: public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS( FF_STMT_NODE);
private:
  WN *_stmt;
public:
  FF_STMT_NODE() { _stmt = NULL; };
  FF_STMT_NODE(WN *stmt) { _stmt = stmt; };
  ~FF_STMT_NODE() {};
  void Set_Stmt(WN *stmt) { _stmt = stmt; }
  WN *Get_Stmt() const { return _stmt; }
};
                                                                                                                                                             
class FF_STMT_LIST: public SLIST {
  DECLARE_SLIST_CLASS( FF_STMT_LIST, FF_STMT_NODE )
public:
  ~FF_STMT_LIST(void){};
  void Append(WN *stmt, MEM_POOL *mpool) {
    Append(CXX_NEW(FF_STMT_NODE(stmt), mpool));
  }
  void Prepend(WN *stmt, MEM_POOL *mpool) {
    Prepend(CXX_NEW(FF_STMT_NODE(stmt), mpool));
  }
};
                                                                                                                                                             
class FF_STMT_ITER: public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS( FF_STMT_ITER, FF_STMT_NODE, FF_STMT_LIST )
public:
  ~FF_STMT_ITER() {};
};

struct F90_LOOP_INFO{
INT start, end, step;
};

static BOOL Find_Preceeding_Pragma(WN* wn, WN_PRAGMA_ID pragma_id)
{
  WN* prev_pragma=WN_prev(wn);
  while (prev_pragma &&
        (WN_opcode(prev_pragma)==OPC_PRAGMA ||
         WN_opcode(prev_pragma)==OPC_XPRAGMA)) {
    if (WN_pragma(prev_pragma)==pragma_id)
      return TRUE;
    prev_pragma=WN_prev(prev_pragma);
  }
  return FALSE;
}
static void 
F90_Separate(WN* in_loop, WN* block, WN* in_stmt, UINT8 level, WN** new_loop, BOOL create_empty_loop = FALSE)
{

  WN* loop_body1;
  WN* loop_body2;
  WN* wn1;
  WN* wn2;

  FmtAssert(WN_opcode(in_loop)==OPC_DO_LOOP, 
    ("non-loop input node in Separate()\n") );

  if (!in_stmt && !create_empty_loop) {
    Is_True(0, ("Null stmt passed into LNO:Separate()\n"));
    return;
  }
  

  if (in_stmt && !create_empty_loop && (WN_next(in_stmt) == NULL)) {
    // loop with single statement
    *new_loop=NULL;
    return;
  }

  // Create DO loop node for loop2, TODO should use wn_pool
  *new_loop =WN_CreateDO (
	WN_COPY_Tree(WN_index(in_loop)),	// index
	WN_COPY_Tree(WN_start(in_loop)),	// start
	WN_COPY_Tree(WN_end(in_loop)),	// end
	WN_COPY_Tree(WN_step(in_loop)),	// step
  	WN_CreateBlock(),			// body
	NULL);

  WN_Set_Linenum(WN_do_body(*new_loop),WN_Get_Linenum(WN_do_body(in_loop)));
  // Assumption: start, end, and step expressions are all loop-invariant

  loop_body1=WN_do_body(in_loop);
  loop_body2=WN_do_body(*new_loop);

  // cut the loop body of loop1 at in_stmt
  if (in_stmt){
    if (WN_next(in_stmt)!=NULL){
      WN_first(loop_body2)=WN_next(in_stmt);
      WN_last(loop_body2)=WN_last(loop_body1);
      WN_last(loop_body1)=in_stmt;
      WN_prev(WN_first(loop_body2))=NULL;
      WN_next(WN_last(loop_body2))=NULL;
      WN_next(WN_last(loop_body1))=NULL;
    }
  } else {
    WN_first(loop_body2)=WN_first(loop_body1);
    WN_last(loop_body2)=WN_last(loop_body1);
    WN_first(loop_body1)=NULL;
    WN_last(loop_body1)=NULL;
  }

  wn1 = in_loop;
  wn2 = *new_loop;

  WN_Set_Linenum(wn2, WN_Get_Linenum(wn1));
  WN_INSERT_BlockAfter(block,	                                        // block
	               wn1,						// wn
	               wn2						// in
	               );						// pool

}
static void F90_Separate_And_Update(WN* in_loop, WN* block, DYN_ARRAY<FF_STMT_LIST>& loop, UINT fission_level)
{
  UINT total_loops=loop.Lastidx()+1;
  WN*** wn_starts=CXX_NEW_ARRAY(WN**, fission_level, f90_lower_pool);
  WN*** wn_ends=CXX_NEW_ARRAY(WN**, fission_level, f90_lower_pool);
  WN*** wn_steps=CXX_NEW_ARRAY(WN**, fission_level, f90_lower_pool);
  INT i;

  for (i=0; i<fission_level; i++) {
    wn_starts[i]=CXX_NEW_ARRAY(WN*, total_loops, f90_lower_pool);
    wn_ends[i]=CXX_NEW_ARRAY(WN*, total_loops, f90_lower_pool);
    wn_steps[i]=CXX_NEW_ARRAY(WN*, total_loops, f90_lower_pool);
  }

  WN*** new_loop=CXX_NEW_ARRAY(WN**, fission_level, f90_lower_pool);
  WN* wn = in_loop;
  WN* outer_most_loop;

  for (i=fission_level-1; i>=0; i--) {
    new_loop[i]=CXX_NEW_ARRAY(WN*,total_loops,f90_lower_pool);
    new_loop[i][0]=wn;
    wn_starts[i][0]=WN_kid0(WN_start(wn));
    wn_ends[i][0]=WN_end(wn);
    wn_steps[i][0]=WN_kid0(WN_step(wn));
    if (i==0)
      outer_most_loop=wn;
  }

  for (i=total_loops-1; i>0; i--) {
    WN* loop_body = WN_do_body(in_loop);
    WN* first_stmt = loop[i].Head()->Get_Stmt();
    FF_STMT_NODE* stmt_node_p;
    F90_Separate(in_loop, block, WN_prev(first_stmt), fission_level, &wn);
  }
}

static BOOL Arraysection_in_Subtree(WN *tree)
{
   WN_ITER *tree_iter;
   WN *node;
   OPERATOR opr;

   tree_iter = WN_WALK_TreeIter(tree);
   while (tree_iter) {
      node = WN_ITER_wn(tree_iter);
      opr = WN_operator(node);
      if (opr == OPR_ARRSECTION) {
	 WN_WALK_Abort(tree_iter);
	 return (TRUE);
      }
      tree_iter = WN_WALK_TreeNext(tree_iter);
   }
   return (FALSE);
}

static BOOL F90_Processed_ST(ST *st, STACK<ST*> *processed)
{
      for (INT i=0; i<processed->Elements(); ++i)
        if (processed->Bottom_nth(i)==st)
          return TRUE;
      return FALSE;

}
static inline INT64
Num_Elements(ARB_HANDLE arb)
{
  return abs(ARB_ubnd_val(arb) - ARB_lbnd_val(arb) + 1);
}
static INT64
Get_New_Size_Padding(TY_IDX new_array_ty_idx, TY_IDX old_array_ty_idx,
                     INT32 start_dim)
{
 TY&  new_array_ty = Ty_Table[new_array_ty_idx];
 TY& old_array_ty = Ty_Table[old_array_ty_idx];
                                                                                                                                                             
  INT64 old_size = 1;
  INT64 new_size = 1;
  INT old_num_dims = TY_AR_ndims(old_array_ty);
  INT new_num_dims = TY_AR_ndims(new_array_ty);
  ARB_HANDLE old_arb_base = TY_arb(old_array_ty);
  ARB_HANDLE new_arb_base = TY_arb(new_array_ty);
                                                                                                                                                             
 Is_True((start_dim < old_num_dims), ("start dim = %d , num_dims = %d  in Get_New_Size_Padding \n", start_dim, old_num_dims));
                                                                                                                                                             
  for (INT i=start_dim; i<old_num_dims; ++i)
  {
      ARB_HANDLE old_arb = old_arb_base[i];
      ARB_HANDLE new_arb = new_arb_base[i];
      old_size = old_size*Num_Elements(old_arb);
      new_size = new_size*Num_Elements(new_arb);
  }
  for (INT i=old_num_dims;i<new_num_dims; ++i){
      ARB_HANDLE new_arb = new_arb_base[i];
      new_size = new_size*Num_Elements(new_arb);
  }
   
  new_size = new_size - old_size;
  return new_size;
}

static void F90_Expand_Array(ST *st, F90_LOOP_INFO *loop_info, WN *parent_loop, WN **new_subscript, STACK<ST*> *processed)
{
  WN *index = WN_index(parent_loop);
  if (!F90_Processed_ST(st, processed)){
    TY_IDX old_array_ty_idx = ST_type(st);
    INT num_dims = TY_AR_ndims(old_array_ty_idx);
    TY_IDX etype_idx = TY_etype(old_array_ty_idx);
    const TY& etype = Ty_Table[etype_idx];
    TY_IDX new_array_ty_idx = Make_Array_Type(TY_mtype(etype), num_dims+1, 1);
    Set_TY_etype(new_array_ty_idx, etype_idx);
    Set_TY_name_idx(Ty_Table[new_array_ty_idx], TY_name_idx(Ty_Table[old_array_ty_idx]));
    Set_TY_size(Ty_Table[new_array_ty_idx],TY_size(Ty_Table[old_array_ty_idx]));
    num_dims = TY_AR_ndims(new_array_ty_idx);
    ARB_HANDLE arb_base = TY_arb(new_array_ty_idx);
    ARB_HANDLE old_arb_base = TY_arb(old_array_ty_idx);
    ARB_HANDLE  arb, old_arb;
    UINT i;
#ifndef KEY
    for (i = 0; i < num_dims-1 ; ++i)
    {
      arb = arb_base[i];
      old_arb = old_arb_base[i];
      ARB_Init(arb, ARB_lbnd_val(old_arb),
               ARB_ubnd_val(old_arb),
               ARB_stride_val(old_arb)*(loop_info->end-loop_info->start+1)/loop_info->step);
      Set_ARB_dimension(arb, num_dims-i);
      if (i==0)
        Set_ARB_first_dimen(arb_base[0]);
    }
    arb = arb_base[i];
    ARB_Init(arb, 1, 1+abs((loop_info->end-loop_info->start)/loop_info->step), abs((loop_info->end-loop_info->start)/loop_info->step));
    Set_ARB_dimension(arb, num_dims-i);
    Set_ARB_last_dimen(arb_base[i]);
#else
   // When expanding an array (increasing the number of dimensions), attach 
    // the newly added dimension before all other arb dimensions not after. 
    // Based on bug 1973, this seems to be the right thing to do. Without
    // this, LNO pad will see a mismatch between WN and symtable ARB entry and
    // will adjust the ARRAY WNs incorrectly.
    arb = arb_base[0];
    ARB_Init(arb, 1, 1+abs((loop_info->end-loop_info->start)/loop_info->step), abs((loop_info->end-loop_info->start)/loop_info->step));    
    Set_ARB_first_dimen(arb_base[0]);
    Set_ARB_dimension(arb, num_dims);
    for (i = 1; i < num_dims ; ++i)
    {
      arb = arb_base[i];
      old_arb = old_arb_base[i-1];
      ARB_Init(arb, ARB_lbnd_val(old_arb),
               ARB_ubnd_val(old_arb),
               ARB_stride_val(old_arb)*(loop_info->end-loop_info->start+1)/loop_info->step);
      Set_ARB_dimension(arb, num_dims-i);
      if (i==num_dims-1)
        Set_ARB_last_dimen(arb_base[i]);
    }
#endif

    TY& new_array_ty = Ty_Table[new_array_ty_idx];
    TY& old_array_ty = Ty_Table[old_array_ty_idx];
    etype_idx = TY_etype(old_array_ty);
    INT64 add_size  = Get_New_Size_Padding(new_array_ty_idx,
                                         old_array_ty_idx,0);
    add_size = add_size*TY_size(etype_idx);
    Set_TY_size(new_array_ty, TY_size(old_array_ty) + add_size);
    Set_ST_type(*st, new_array_ty_idx);
    processed->Push(st);
  }

  TYPE_ID index_type = WN_desc(WN_start(parent_loop));
  ST *loop_index =  WN_st(index);
  *new_subscript = WN_Div(index_type, 
                   WN_Sub(index_type, 
                          WN_Ldid(index_type, WN_idname_offset(index), loop_index, MTYPE_To_TY(index_type)),
                          WN_Intconst(index_type, loop_info->start)), 
                   WN_Intconst(index_type,loop_info->step));
  *new_subscript = WN_Simplify_Tree(*new_subscript);

}

static WN *Find_Arrsection_and_Pos(WN *tree, WN **parent, INT* pos,  STACK<WN*> *processed_wn)
{
   WN *r;
   INT num_kids,i;
   switch (WN_operator(tree)) {
    case OPR_ARRSECTION:
      for ( i=0; i<processed_wn->Elements(); ++i)
        if (processed_wn->Bottom_nth(i)==tree)
          return NULL;
       processed_wn->Push(tree);
      return (tree);
    case OPR_ARRAY:
    case OPR_ARRAYEXP:
      *pos = 0;
      *parent = tree;
      return (Find_Arrsection_and_Pos(WN_kid0(tree), parent, pos, processed_wn));
    default:
      num_kids = WN_kid_count(tree);
      r = NULL;
      for (i=0; i < num_kids; i++) {
         *pos = i;
         *parent = tree;
         r = Find_Arrsection_and_Pos(WN_kid(tree,i), parent, pos, processed_wn);
         if (r) break;
      }
      return (r);
   }
}
static void F90_Modify_Array_Section(ST *st, WN *old_wn, WN *parent, INT pos, WN *new_wn, STACK<WN*> *processed_wn)
{
  TY_IDX array_ty_idx = ST_type(st);
  INT ndim = TY_AR_ndims(array_ty_idx);

   /* Create the ARRSECTION node referring to the temp */
   WN *arrsection = WN_Create(OPCarrsection,2*ndim+1);
   TY_IDX temp_ty = TY_etype(array_ty_idx);
   TY_IDX ptr_ty = Make_Pointer_Type(temp_ty);
   WN_kid0(arrsection) = WN_Lda(Pointer_type, (WN_OFFSET) 0, st);
   INT element_size = TY_size(temp_ty);
   WN_element_size(arrsection) = element_size;

   ARB_HANDLE arb_base = TY_arb(array_ty_idx);
#ifndef KEY
   WN_kid(arrsection, 1) = WN_Intconst(MTYPE_I8, ARB_ubnd_val(arb_base[ndim-1]));
#else
   // When expanding an array (increasing the number of dimensions), attach 
   // the newly added dimension before all other arb dimensions not after. 
   // Based on bug 1973, this seems to be the right thing to do. Without
   // this, LNO pad will see a mismatch between WN and symtable ARB entry and
   // will adjust the ARRAY WNs incorrectly.
   WN_kid(arrsection, 1) = WN_Intconst(MTYPE_I8, ARB_ubnd_val(arb_base[0]));
#endif
   WN_kid(arrsection, ndim+1) = WN_COPY_Tree(new_wn);
   
   for (INT i=1; i < ndim; i++) {
      /* Kids 1 to ndim are the array sizes */
      WN_kid(arrsection,i+1) = WN_COPY_Tree(WN_kid(old_wn,i));
      /* Kids ndim+1 to 2*ndim are the index expressions */
      WN_kid(arrsection,i+1+ndim) = WN_COPY_Tree(WN_kid(old_wn,i+ndim-1));
   }
   WN_kid(parent, pos) = arrsection;
   processed_wn->Push(arrsection);
}

static void F90_Array_Expansion(F90_LOOP_INFO *loop_info, WN *parent_loop, WN *stmt, STACK<ST*> *processed)
{
   OPCODE op;
   OPERATOR opr;
   BOOL is_arrayexp;
   WN *arrayexp = NULL;
   INT i,num_kids;
   WN *arraysection = NULL;
   WN *new_subscript = NULL;

   op = WN_opcode(stmt);
   opr = OPCODE_operator(op);
   
   if (opr == OPR_WHERE) 
      arrayexp = WN_first(WN_kid0(stmt)); 
   else if (opr == OPR_MSTORE || opr == OPR_ISTORE) {
     if (WN_operator(WN_kid1(stmt)) == OPR_ARRAYEXP) 
       arrayexp = WN_kid1(stmt);
   } 
   WN *parent; 
   INT pos;
   if (arrayexp) {
     STACK<WN*> *processed_wn = CXX_NEW(STACK<WN*>(f90_lower_pool), f90_lower_pool);
     arraysection = Find_Arrsection_and_Pos(arrayexp, &parent, &pos, processed_wn);
     while (arraysection){
       WN *addr = WN_kid0(arraysection);
       if (WN_operator(addr) == OPR_LDA && ST_is_temp_var(WN_st(addr))){
         F90_Expand_Array(WN_st(addr), loop_info, parent_loop, &new_subscript, processed);
         F90_Modify_Array_Section(WN_st(addr), arraysection, parent, pos, new_subscript, processed_wn); 
       }
       arraysection = Find_Arrsection_and_Pos(arrayexp, &parent, &pos, processed_wn);
     }
   }
}

static BOOL F90_Get_Loop_Info(WN *parent_loop, F90_LOOP_INFO *loop_info){
  WN *start = WN_kid0(WN_start(parent_loop));
  WN *end = WN_kid1(WN_end(parent_loop));
  WN *step = WN_kid1(WN_kid0(WN_step(parent_loop)));
  if (WN_operator(start)!=OPR_INTCONST || WN_operator(end)!=OPR_INTCONST || WN_operator(step)!=OPR_INTCONST)
    return FALSE;
  loop_info->start = WN_const_val(start);
  loop_info->end = WN_const_val(end);
  loop_info->step = WN_const_val(step);
  return TRUE;
}

static void F90_Fission_Loop(WN* wn, WN *block)
{
  INT level = 1;
  WN* parent_loop = wn;
  DYN_ARRAY<FF_STMT_LIST> loops(f90_lower_pool);
  F90_LOOP_INFO loop_info;

  STACK<ST*> *processed = CXX_NEW(STACK<ST*>(f90_lower_pool), f90_lower_pool);
  WN* stmt=WN_first(WN_do_body(parent_loop));
  if (!F90_Get_Loop_Info(parent_loop, &loop_info))
    return;
  INT total_loops = 0;
  while (stmt){
/* So far we can not handle the case of FORALL Loop nest with where statement. The extension is to implemented later */
    if (WN_operator(stmt) == OPR_DO_LOOP)
      return;

    loops.Newidx();
    if (Arraysection_in_Subtree(stmt))
      F90_Array_Expansion(&loop_info, parent_loop, stmt, processed);
    loops[total_loops++].Append(stmt, f90_lower_pool);
    stmt=WN_next(stmt);
  }
  F90_Separate_And_Update(parent_loop, block, loops, level);
}
static void F90_Walk_Stmts_return_where (WN *tree, BOOL *where_flag)
{
  OPCODE op;
  WN *node;

  if (*where_flag == TRUE)
    return;

  op = WN_opcode(tree);
  if (op == OPC_BLOCK) {
    node = WN_first(tree);
    while (node) {
      F90_Walk_Stmts_return_where(node, where_flag);
      node = WN_next(node);
    }
  } else if (OPCODE_is_scf(op) && op != OPC_WHERE) { 
    for (INT i=0; i < WN_kid_count(tree); i++) {
      F90_Walk_Stmts_return_where(WN_kid(tree,i), where_flag);
    }
  } else if (OPCODE_is_stmt(op) || op == OPC_WHERE) {
    if (op == OPC_WHERE)
      *where_flag = TRUE;
    return; 
  }
  return;
}

#endif   

static BOOL F90_Walk_Statements_Helper(WN * tree, WN * block,
				       BOOL prewalk(WN * node, WN *block),
				       BOOL walk_scf)
{
   OPCODE op;
   WN *callblock;
   WN *node,*nextnode;
   BOOL keep_going = TRUE;
   BOOL is_mp_region = FALSE;
   INT i,numkids;

   op = WN_opcode(tree);

   if (op == OPC_REGION && WN_region_kind(tree) == REGION_KIND_MP) {
     is_mp_region = TRUE;
     F90_MP_Region.insert(F90_MP_Region.begin(),tree);
     WN *pragma_wn = WN_first(WN_region_pragmas(tree));
     bool bWorkshare;
     if( (pragma_wn != NULL) &&
       (WN_opcode(pragma_wn) == OPC_PRAGMA) ){
       WN_PRAGMA_ID pragma = (WN_PRAGMA_ID)WN_pragma(pragma_wn);
       bWorkshare = (pragma == WN_PRAGMA_PWORKSHARE_BEGIN) ||
                    (pragma == WN_PRAGMA_PARALLEL_WORKSHARE);
     }
     else{
       bWorkshare = false;
     }
     F90_MP_Region_Isworkshare.insert(F90_MP_Region_Isworkshare.begin(), bWorkshare);
   }

   if (op == OPC_BLOCK) {
      callblock = tree;
      node = WN_first(tree);
      while (node && keep_going) {
	 nextnode = WN_next(node); /* Because the walk may remove the statement */
	 keep_going = F90_Walk_Statements_Helper(node, callblock, prewalk, walk_scf);
	 node = nextnode;
      }
   } else if (OPCODE_is_scf(op) && op != OPC_WHERE) { /* special WHERE handling */
      if (walk_scf) {
	 keep_going = do_prewalk(tree,block,prewalk);
	 if (!keep_going) goto done;
      }
#ifdef KEY
      if (WN_operator(tree) == OPR_DO_LOOP) {
        BOOL forall_flag =  Find_Preceeding_Pragma(tree,WN_PRAGMA_FORALL);
        BOOL where_flag = FALSE;
        F90_Walk_Stmts_return_where(tree, &where_flag);
        if (forall_flag == TRUE && where_flag == TRUE)
          F90_Fission_Loop(tree, block);
      }
#endif
      numkids = WN_kid_count(tree);
      for (i=0; i < numkids; i++) {
	 keep_going = F90_Walk_Statements_Helper(WN_kid(tree,i), block, prewalk, walk_scf);
	 if (!keep_going) break;
      }
   } else if (OPCODE_is_stmt(op) || op == OPC_WHERE) {
      keep_going = do_prewalk(tree,block,prewalk);
   }
done:
   if (is_mp_region) {
     F90_MP_Region.erase(F90_MP_Region.begin());
     F90_MP_Region_Isworkshare.erase(F90_MP_Region_Isworkshare.begin());
   }
   return (keep_going);
}

static void F90_Walk_Statements(WN * tree, BOOL prewalk(WN * node, WN *block))
{
   /* Walk the tree, starting with a NULL block at the top */
   insert_handle = WN_Create(OPC_COMMENT,0);
   (void) F90_Walk_Statements_Helper(tree, NULL, prewalk,FALSE);
   WN_Delete(insert_handle);
}

static void F90_Walk_All_Statements(WN * tree, BOOL prewalk(WN * node, WN *block))
{
   /* Walk the tree, starting with a NULL block at the top */
   insert_handle = WN_Create(OPC_COMMENT,0);
   (void) F90_Walk_Statements_Helper(tree, NULL, prewalk,TRUE);
   WN_Delete(insert_handle);
}

/***************************************************************
***************************************************************
*
*  Auxilliary routines and variables
*
**************************************************************
**************************************************************/

/*
   Auxilliary stuff needed to maintain correspondence between PREG used for allocation
   in alloca and PREG used for freeing stack space.
*/

static INT num_alloca;
static INT max_num_alloca;

typedef struct {
   WN_OFFSET offset;
   ST *alloc_st;
   PREG_NUM saved_sp;
} alloc_correspond_s;


static alloc_correspond_s *alloc_correspond;

/*
 * called before lowering a program unit, 
 * initializes memory pools
 */
static void F90_Lower_Init(void) {
   TYPE_ID	mtype1;
   TYPE_ID	mtype2;
   PREG_NUM      rreg2;

   /* Initialize memory pool */
   if (! f90_lower_pool) {
      f90_lower_pool = &f90_lower_pool_s;
      MEM_POOL_Initialize(f90_lower_pool,"f90_lower_pool",TRUE); /* Init to 0 always */
   }
   MEM_POOL_Push(f90_lower_pool);
   
   /* Create the WHIRL map */
   f90_lower_map = WN_MAP_Create(f90_lower_pool);
   
   /* Get the return PREG for allocations of pointer types */
   if (WHIRL_Return_Info_On) {

      RETURN_INFO return_info = Get_Return_Info (Be_Type_Tbl(Pointer_type),
						 Use_Simulated);

      if (RETURN_INFO_count(return_info) <= 2) {

	 mtype1 = RETURN_INFO_mtype (return_info, 0);
	 mtype2 = RETURN_INFO_mtype (return_info, 1);
 	 pointer_return_reg = RETURN_INFO_preg (return_info, 0);
	 rreg2 = RETURN_INFO_preg (return_info, 1);
      }

      else
	 Fail_FmtAssertion ("F90_Lower_Init: more than 2 return registers");
   }

   else { 
     Get_Return_Mtypes(Be_Type_Tbl(Pointer_type), Use_Simulated, &mtype1, &mtype2);
     Get_Return_Pregs(mtype1, mtype2, &pointer_return_reg, &rreg2);
   }

   num_alloca = 0;
   max_num_alloca = 0;
   
   /* Set up the various opcodes, etc. */
   pointer8 = Pointer_Size == 8;
   if (pointer8) {
      OPCmpy = OPC_I8MPY;
      OPCadd = OPC_I8ADD;
      OPCsub = OPC_I8SUB;
      OPCmod = OPC_I8MOD;
      OPCint = OPC_I8INTCONST;
      OPCtriplet = OPC_I8TRIPLET;
      OPCarrsection = OPC_U8ARRSECTION;
      INTRNalloca = INTRN_U8I8ALLOCA;
      INTRNfree = INTRN_U8FREE;
      INTRNmalloc = INTRN_U8I8MALLOC;
      INTRNgetstack = INTRN_U8READSTACKPOINTER;
      INTRNsetstack = INTRN_U8I8SETSTACKPOINTER;
      doloop_ty = MTYPE_I8;
   } else {
      OPCmpy = OPC_I4MPY;
      OPCadd = OPC_I4ADD;
      OPCsub = OPC_I4SUB;
      OPCmod = OPC_I4MOD;
      OPCint = OPC_I4INTCONST;
      OPCtriplet = OPC_I4TRIPLET;
      OPCarrsection = OPC_U4ARRSECTION;
      INTRNalloca = INTRN_U4I4ALLOCA;
      INTRNfree = INTRN_U4FREE;
      INTRNmalloc = INTRN_U4I4MALLOC;
      INTRNgetstack = INTRN_U4READSTACKPOINTER;
      INTRNsetstack = INTRN_U4I4SETSTACKPOINTER;
      doloop_ty = MTYPE_I4;
   }
   char_ty = TY_IDX_ZERO;
   temp_allocations_inserted = FALSE;
   array_statement_seen = FALSE;
}

/* called after lowering a program unit
 * cleans up memory pools 
 */
static void F90_Lower_Term(void) {
   
   WN_MAP_Delete(f90_lower_map);
   /* Free the memory pool */
   MEM_POOL_Pop(f90_lower_pool);
}


/*
 *
 * Routines for maintaining alloca correspondences
 *
 * void add_alloca_correspondence(ST * alloca_data, WN_OFFSET offset, PREG_NUM alloca_save_sp)
 *    adds an entry to the alloca correspondence tables.
 *
 * PREG_NUM get_corresponding_sp(ST * alloca_data, WN_OFFSET offset)
 *    returns the restore SP entry in the alloca tables for the allocation temp
 */

static void add_alloca_correspondence(ST *alloca_data, WN_OFFSET offset, PREG_NUM alloca_save_sp)
{
#define ALLOCA_CHUNK_SIZE 64   
   num_alloca += 1;
   if (num_alloca > max_num_alloca) {
      /* Reallocate the tables */
      if (max_num_alloca == 0) {
	 alloc_correspond = TYPE_MEM_POOL_ALLOC_N(alloc_correspond_s,f90_lower_pool,ALLOCA_CHUNK_SIZE);
      } else {
	 alloc_correspond = TYPE_MEM_POOL_REALLOC_N(alloc_correspond_s,
						    f90_lower_pool,alloc_correspond,
						    max_num_alloca,
						    max_num_alloca + ALLOCA_CHUNK_SIZE);
      }
      max_num_alloca += ALLOCA_CHUNK_SIZE;
   }
   alloc_correspond[num_alloca-1].offset = offset;
   alloc_correspond[num_alloca-1].alloc_st = alloca_data;
   alloc_correspond[num_alloca-1].saved_sp = alloca_save_sp;
}

static PREG_NUM get_existing_sp(ST * alloca_data, WN_OFFSET offset)
{
   INT i;
   for (i=0; i < num_alloca; i++) {
      if (alloc_correspond[i].offset == offset &&
          alloc_correspond[i].alloc_st == alloca_data) {
         return (alloc_correspond[i].saved_sp);
      }
   }
   return (0);
}

static PREG_NUM get_corresponding_sp(ST * alloca_data, WN_OFFSET offset)
{
   INT i;
   for (i=0; i < num_alloca; i++) {
      if (alloc_correspond[i].offset == offset &&
	  alloc_correspond[i].alloc_st == alloca_data) {
	 return (alloc_correspond[i].saved_sp);
      }
   }
   DevAssert(0,("Couldn't find sp corresponding to ST/offset 0x%x %d",alloca_data,offset));
   return (0);
}

/*================================================================
 *  F90_LOWER_AUX_DATA *F90_Lower_New_Aux_Data(INT ndim)
 *
 *   Allocate and initialize a new auxiliary data structure, 
 *   with a number of dimensions equal to ndim. All fields are zeroed by the 
 *   memory allocation routines.
 *================================================================
 */

static F90_LOWER_AUX_DATA * F90_Lower_New_Aux_Data(INT ndim) 
{
   F90_LOWER_AUX_DATA *r;
   WN **iter_count;
   mINT16 *perm_index;
   DIR_FLAG *directions;
   INT i;

   /* allocate the main structure */
   r = TYPE_MEM_POOL_ALLOC( F90_LOWER_AUX_DATA,f90_lower_pool);
   SET_NDIM(r,ndim);
   SET_KNOWN_INDEPENDENT(r,FALSE);
   SET_COPY_FLAG(r,COPY_NONE);

   /* allocate the arrays */
   if (ndim > 0) {
      iter_count = TYPE_MEM_POOL_ALLOC_N(WN *,f90_lower_pool,ndim);
      perm_index = TYPE_MEM_POOL_ALLOC_N(mINT16,f90_lower_pool,ndim);
      directions = TYPE_MEM_POOL_ALLOC_N(DIR_FLAG,f90_lower_pool,ndim);

      SET_ITER_COUNT_P(r,iter_count);
      SET_PERM_INDEX_P(r,perm_index);
      SET_DIRECTION_P(r,directions);
   }

   /* Allocate empty blocks for the pre, post lists, etc. */

   SET_PRELIST(r,WN_CreateBlock());
   SET_POSTLIST(r,WN_CreateBlock());
   SET_ALLOC_PRELIST(r,WN_CreateBlock());
   SET_DEALLOC_POSTLIST(r,WN_CreateBlock());

   /* Initialize fields */
   for (i=0; i < ndim; i++) {
      SET_PERM_INDEX(r,i,i);
      SET_DIRECTION(r,i,DIR_DONTCARE);
   }
   return (r);
}

/* Copy the contents of a adata structure (except for the PRE, POST lists and COPY_FLAG) */

static F90_LOWER_AUX_DATA * F90_Lower_Copy_Aux_Data(F90_LOWER_AUX_DATA * adata)
{
   F90_LOWER_AUX_DATA * r;
   INT ndim;
   INT i;
   ndim = NDIM(adata);
   
   r = F90_Lower_New_Aux_Data(ndim);
   for (i=0; i < ndim; i++) {
      SET_PERM_INDEX(r,i,PERM_INDEX(adata,i));
      SET_ITER_COUNT(r,i,WN_COPY_Tree(ITER_COUNT(adata,i)));
      SET_DIRECTION(r,i,DIRECTION(adata,i));
   }

   return (r);
}

/*================================================================
 * 
 * char * create_tempname (char *name) 
 * 
 * creates a temporary name by appending the temp number to the name string. 
 * the created name is statically allocated, so it needs to be copied before use. 
 *
 *================================================================
 */

static char * create_tempname(const char * name)
{
   static char buf[64];
   num_temps += 1;
   sprintf(buf,"%s_%d",name,num_temps);
   return(buf);
}

//================================================================
// Utility routine to create an ST* for the many temp symbols generated
//================================================================
static ST * new_temp_st(const char * name)
{
   ST * st;
   st = New_ST();
   ST_Init(st,Save_Str(create_tempname(name)),
	   CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, (TY_IDX) 0);

   Add_Pragma_To_MP_Regions (&F90_MP_Region,WN_PRAGMA_LOCAL,
			     st,0,WN_MAP_UNDEFINED,FALSE);
#ifdef KEY
   // Bug 4479 - set is_temp flag for this ST. This is later used 
   // inside the SIMD module to identify (unaligned) loads and 
   // stores if need be.
   Set_ST_is_temp_var(st);
#endif
   return (st);
}



/*================================================================
 * static WN * get_first_dimension_ubound(TY_IDX ty)
 *
 * Utility routine to create a WN containing a load of the value of the first 
 * dimension of an array TY.
 *================================================================*/

static WN * get_first_dimension_ubound(TY_IDX ty)
{
   ST_IDX bound_st_idx;
   TY_IDX bound_st_ty;
   WN *r;

   bound_st_idx = TY_AR_ubnd_var(ty,0);
   bound_st_ty = ST_type(bound_st_idx);
   
   r = WN_Ldid(TY_mtype(bound_st_ty),0,bound_st_idx,bound_st_ty);
   return (r);
}

/*
 * F90_Lower_Create_Temp - Create the space for an array-valued or scalar temporary
 * 
 * 
 * ST * F90_Lower_Create_Temp(WN **alloc_block, WN **free_block, WN **size, INT ndim, TY_IDX ty)
 *   alloc_block (output) - a block of statements doing the allocate
 *   free_block (output) - a block of statements freeing the allocated memory
 *   size (input) - array of WHIRL nodes indicating the size for each dimension
 *   ndim (input) - number of dimensions
 *   ty (input) - type of the symbol
 *   element_size (input) - size of an element of an array to allocate. 
 *       If NULL, size comes from the TY.
 *
 *   If ndim is 0, this will create a SCALAR temp on the stack of type ty.
 *   In this case, alloc_block and free_block will be untouched.
 * 
 *   returns - the ST of the newly created temp
 * 
 *   if alloc_block is not null on entry, the allocations are appended to the block
 *   if free_block is not null on entry, the deallocations are prepended to the block
 */
static ST * F90_Lower_Create_Temp(WN **alloc_block, WN **free_block, WN **size,
                                  INT ndim, TY_IDX ty,
                                  WN *element_size)
{
   INT i;
   WN *total_size,*save_temp;
   ST *st;
   TY_IDX pty;
   OPCODE callop;
   WN *call;
   INTRINSIC aintrin,fintrin;
   BOOL is_var_len_char = FALSE;
   WN *cur_size;
   
   /* Create a stack temp symbol */
   st = new_temp_st("@f90");

   if (TY_is_character(ty) &&
       TY_kind(ty) == KIND_ARRAY &&
       TY_size(ty) == 0 &&
       ! ARB_const_ubnd(TY_arb(ty))) {

      is_var_len_char = TRUE;
   }

   if (ndim > 0 || is_var_len_char) {
      pty = Make_Pointer_Type(ty);
      Set_ST_type(st,pty);
      Set_ST_pt_to_unique_mem(st);
   } else {
      Set_ST_type(st,ty);
   }

   if (ndim == 0 && ! is_var_len_char) {
      return (st);
   }

   DevAssert((alloc_block),("missing alloc_block in F90_Lower_Create_Temp"));

   /* Create the various blocks */
   if (!*alloc_block) *alloc_block = WN_CreateBlock();
   if (!*free_block) *free_block = WN_CreateBlock();
   
   /* Create the total size */
   if (element_size) {
      total_size = WN_COPY_Tree(element_size);
   } else {
      if (TY_size(ty) != 0) {
         total_size = WN_CreateIntconst(OPCint,TY_size(ty));
      } else {
	 total_size = get_first_dimension_ubound(ty);
      }
   }
   for (i=0; i < ndim; i++) {
      cur_size = WN_COPY_Tree(size[i]);
#ifdef KEY
      if (MTYPE_size_min(WN_rtype(cur_size)) != MTYPE_size_min(WN_rtype(total_size)))
        cur_size = WN_Cvt(WN_rtype(cur_size), WN_rtype(total_size), cur_size);
#endif
      total_size = WN_CreateExp2(OPCmpy,total_size,cur_size);
   }
   
   if (Heap_Allocation_Threshold == -1) {
      aintrin = INTRN_F90_STACKTEMPALLOC;
      fintrin = INTRN_F90_STACKTEMPFREE;
   } else if (Heap_Allocation_Threshold == 0) {
      aintrin = INTRN_F90_HEAPTEMPALLOC;
      fintrin = INTRN_F90_HEAPTEMPFREE;
   } else {
      aintrin = INTRN_F90_DYNAMICTEMPALLOC;
      fintrin = INTRN_F90_DYNAMICTEMPFREE;
   }
   
   /* The allocate */
   callop = OPCODE_make_op(OPR_INTRINSIC_OP, Pointer_type, MTYPE_V);
   call = WN_CreateParm(Pointer_type, total_size, Be_Type_Tbl(Pointer_type),
			WN_PARM_BY_VALUE);
   call = WN_Create_Intrinsic(callop,aintrin,1,&call);
   save_temp = WN_Stid(Pointer_type,(WN_OFFSET) 0,st,pty,call);
   WN_INSERT_BlockLast(*alloc_block,save_temp);

   /* And the free */
   callop = OPCODE_make_op(OPR_INTRINSIC_CALL, Pointer_type, MTYPE_V);
   call = WN_Ldid(Pointer_type,(WN_OFFSET) 0, st, pty);
   call = WN_CreateParm(Pointer_type,call,pty,WN_PARM_BY_VALUE);
   call = WN_Create_Intrinsic(callop,fintrin,1,&call);
   WN_INSERT_BlockFirst(*free_block,call);

   /* return the symbol for the new temp */
   return (st);
}


/* 
 * F90_Lower_Alloc_Dealloc(WN *stmt, WN *block)
 *
 * Lower the temporary intrinsics F90_[STACK,HEAP]TEMP[ALLOC,FREE]
 *
 * stmt - the statement to check
 * block - block containing the statement
 */

static BOOL F90_Lower_Alloc_Dealloc(WN *stmt, WN *block)
{
   static OPCODE callop=OPCODE_UNKNOWN;
   OPERATOR opr;
   WN *k0,*k0k0;
   WN *call;
   WN * save_sp;
   PREG_NUM free_preg,sp_tmp;
   INTRINSIC intr;

   WN * heap_block;
   WN * stack_block;
   WN * tmp_stmt;
   WN * if_stmt;
   WN * alloc_size;
   WN * preg_load;
   TYPE_ID alloc_type;
   
   if (callop==OPCODE_UNKNOWN) callop = OPCODE_make_op(OPR_INTRINSIC_CALL, Pointer_type, MTYPE_V);

   opr = WN_operator(stmt);

   if (opr == OPR_INTRINSIC_CALL) {
     intr = WN_GET_INTRINSIC(stmt);
     //================ HEAP free
     if (intr == INTRN_F90_HEAPTEMPFREE) {
       WN_intrinsic(stmt) = INTRNfree;
       WN_Set_Call_Does_Mem_Free(stmt);

     //================ STACK free
     } else if (intr == INTRN_F90_STACKTEMPFREE) {
	k0 = WN_kid0(stmt);
	k0k0 = WN_kid0(k0);
	/* This had better be an LDID */
	DevAssert((WN_operator(k0k0)==OPR_LDID),("Lower_Alloc_Dealloc saw something bad"));
	free_preg = get_corresponding_sp(WN_st(k0k0),WN_offset(k0k0));
	preg_load = WN_LdidPreg(Pointer_type,free_preg);

	if (Alloca_Dealloca_On) {
	   call = WN_CreateDealloca(2);
	   WN_INSERT_BlockBefore(block,stmt,call);
	   WN_kid1(call) = k0k0;  // Save the LDID of the actual pointer
	   WN_kid0(call) = preg_load;  // Add the actual item being deallocated

	   WN_EXTRACT_FromBlock(block,stmt);
	   WN_Delete(k0);
	   WN_Delete(stmt);
	} else {
	   /* Find the corresponding Saved SP and just replace in the tree */
	   WN_intrinsic(stmt) = INTRNsetstack;
	   WN_Set_Call_Does_Mem_Free(stmt);
	   WN_DELETE_Tree(k0k0);
	   WN_kid0(WN_kid0(stmt)) = preg_load;
	}

     //================ DYNAMIC free
     } else if (intr == INTRN_F90_DYNAMICTEMPFREE) {
       /* Find the corresponding Saved SP and just replace in the tree */
       /* This had better be an LDID */
       k0 = WN_kid0(WN_kid0(stmt));
       DevAssert((WN_operator(k0)==OPR_LDID),("Lower_Alloc_Dealloc saw something bad"));
       free_preg = get_corresponding_sp(WN_st(k0),WN_offset(k0));
       preg_load = WN_LdidPreg(Pointer_type,free_preg);
       
       heap_block = WN_CreateBlock();
       stack_block = WN_CreateBlock();
       
       // test for free_preg == 0
       if_stmt = WN_EQ(Pointer_Mtype,WN_LdidPreg(Pointer_type,free_preg),WN_Zerocon(Pointer_Mtype));
       if_stmt = WN_CreateIf(if_stmt,heap_block,stack_block);
       WN_INSERT_BlockBefore(block,stmt,if_stmt);
       WN_EXTRACT_FromBlock(block,stmt);
       
       // Then, generate the heap free
       tmp_stmt = WN_COPY_Tree(stmt);
       WN_intrinsic(tmp_stmt) = INTRNfree;
       WN_Set_Call_Does_Mem_Free(tmp_stmt);
       WN_INSERT_BlockFirst(heap_block,tmp_stmt);
       
       // Generate the stack temp free
       if (Alloca_Dealloca_On) {
	  call = WN_CreateDealloca(2);
	  WN_INSERT_BlockFirst(stack_block,call);
	  WN_kid0(call) = preg_load;
	  WN_kid1(call) = WN_kid0(WN_kid0(stmt));
	  WN_Delete(WN_kid0(stmt));
	  WN_Delete(stmt);
       } else {
	  WN_INSERT_BlockFirst(stack_block,stmt);
	  WN_intrinsic(stmt) = INTRNsetstack;
	  WN_Set_Call_Does_Mem_Free(stmt);
	  WN_DELETE_Tree(k0);
	  WN_kid0(WN_kid0(stmt)) = preg_load;
       }
     }

   } else if (opr == OPR_STID) {
     k0 = WN_kid0(stmt);
     if (WN_operator(k0) == OPR_INTRINSIC_OP) {
       //================ HEAP alloc
       if (WN_GET_INTRINSIC(k0) == INTRN_F90_HEAPTEMPALLOC) {
	 /* Insert an INTRINSIC call before this statement */
	 k0k0 = WN_kid0(k0);
	 call = WN_Create_Intrinsic(callop, INTRNmalloc, 1, &k0k0);
	 WN_Set_Call_Default_Flags(call);
	 WN_Set_Call_Does_Mem_Alloc(call);
	 WN_INSERT_BlockBefore(block,stmt,call);
	 WN_Delete(k0);
	 WN_kid0(stmt) = WN_LdidPreg(Pointer_type,pointer_return_reg);

       //================ STACK alloc
       } else if (WN_GET_INTRINSIC(k0) == INTRN_F90_STACKTEMPALLOC) {
	 Set_PU_has_alloca(*current_pu);
         sp_tmp = get_existing_sp(WN_st(stmt),WN_offset(stmt));
         if (sp_tmp == 0) {
	    sp_tmp = Create_Preg(Pointer_type,create_tempname("@f90sp"));
	    /* enter into correspondence tables */
	    add_alloca_correspondence(WN_st(stmt),WN_offset(stmt), sp_tmp);
         }

	 if (Alloca_Dealloca_On) {
	    /* First, need to save the stack pointer */
	    call = WN_CreateAlloca(WN_Zerocon(Pointer_Mtype));
	    call = WN_StidPreg(Pointer_Mtype,sp_tmp,call);
	    WN_INSERT_BlockBefore(block,stmt,call);
	    
	    k0k0 = WN_kid0(k0);
	    WN_Delete(k0);
	    k0 = k0k0;
	    k0k0 = WN_kid0(k0);
	    WN_Delete(k0);
	    call = WN_CreateAlloca(k0k0);
	    WN_kid0(stmt) = call;
	 } else {
	    /* First, need to save the stack pointer */
	    call = WN_Create_Intrinsic(callop,INTRNgetstack,0,NULL);
	    WN_Set_Call_Non_Parm_Ref(call);
	    WN_Set_Call_Non_Data_Ref(call);
	    WN_INSERT_BlockBefore(block,stmt,call);
	    
	    save_sp = WN_LdidPreg(Pointer_type,pointer_return_reg);
	    save_sp = WN_StidIntoPreg(Pointer_type,sp_tmp,MTYPE_To_PREG(Pointer_type), save_sp);
	    WN_INSERT_BlockBefore(block,stmt,save_sp);
	    
	    k0k0 = WN_kid0(k0);
	    call = WN_Create_Intrinsic(callop,INTRNalloca,1,&k0k0);
	    WN_Set_Call_Default_Flags(call);
	    WN_Set_Call_Does_Mem_Alloc(call);
	    WN_INSERT_BlockBefore(block,stmt,call);
	    WN_Delete(k0);
	    WN_kid0(stmt) = WN_LdidPreg(Pointer_type,pointer_return_reg);
	 }

       //================ DYNAMIC alloc
       } else if (WN_GET_INTRINSIC(k0) == INTRN_F90_DYNAMICTEMPALLOC) {
	 Set_PU_has_alloca(*current_pu);
	 heap_block = WN_CreateBlock();
	 stack_block = WN_CreateBlock();
	 
	 alloc_size = WN_COPY_Tree(WN_kid0(WN_kid0(k0)));
	 alloc_type = WN_rtype(alloc_size);
	 if_stmt = WN_GT(alloc_type,alloc_size,WN_Intconst(alloc_type,Heap_Allocation_Threshold));
	 if_stmt = WN_CreateIf(if_stmt,heap_block,stack_block);
	 WN_INSERT_BlockBefore(block,stmt,if_stmt);
	 WN_EXTRACT_FromBlock(block,stmt);

	 // Stack allocation
	 tmp_stmt = WN_COPY_Tree(stmt);
         sp_tmp = get_existing_sp(WN_st(stmt),WN_offset(stmt));
         if (sp_tmp == 0) {
	    sp_tmp = Create_Preg(Pointer_type,create_tempname("@f90sp"));
	    /* enter into correspondence tables */
	    add_alloca_correspondence(WN_st(stmt),WN_offset(stmt), sp_tmp);
         }

	 if (Alloca_Dealloca_On) {
	    save_sp = WN_CreateAlloca(WN_Zerocon(Pointer_Mtype));
	    save_sp = WN_StidPreg(Pointer_Mtype,sp_tmp,save_sp);
	    WN_INSERT_BlockLast(stack_block,save_sp);
	    
	    k0 = WN_kid0(tmp_stmt);
	    k0k0 = WN_kid0(k0);
	    WN_Delete(k0);
	    k0 = k0k0;
	    k0k0 = WN_kid0(k0);
	    WN_Delete(k0);
	    call = WN_CreateAlloca(k0k0);
	    WN_kid0(tmp_stmt) = call;
	    WN_INSERT_BlockLast(stack_block,tmp_stmt);
	 } else {
	    call = WN_Create_Intrinsic(callop,INTRNgetstack,0,NULL);
	    WN_Set_Call_Non_Parm_Ref(call);
	    WN_Set_Call_Non_Data_Ref(call);
	    WN_INSERT_BlockLast(stack_block,call);
	    
	    save_sp = WN_LdidPreg(Pointer_type,pointer_return_reg);
	    save_sp = WN_StidIntoPreg(Pointer_type,sp_tmp,MTYPE_To_PREG(Pointer_type), save_sp);
	    WN_INSERT_BlockLast(stack_block,save_sp);
	    
	    k0 = WN_kid0(tmp_stmt);
	    k0k0 = WN_kid0(k0);
	    call = WN_Create_Intrinsic(callop,INTRNalloca,1,&k0k0);
	    WN_Set_Call_Default_Flags(call);
	    WN_Set_Call_Does_Mem_Alloc(call);
	    WN_INSERT_BlockLast(stack_block,call);
	    WN_Delete(k0);
	    WN_kid0(tmp_stmt) = WN_LdidPreg(Pointer_type,pointer_return_reg);
	    WN_INSERT_BlockLast(stack_block,tmp_stmt);
	 }
	 
	 // Heap allocation
	 save_sp = WN_StidIntoPreg(Pointer_type,sp_tmp,MTYPE_To_PREG(Pointer_type),
				   WN_Zerocon(Pointer_Mtype));
	 WN_INSERT_BlockFirst(heap_block,save_sp);
	 k0 = WN_kid0(stmt);
	 k0k0 = WN_kid0(k0);
	 call = WN_Create_Intrinsic(callop, INTRNmalloc, 1, &k0k0);
	 WN_Set_Call_Default_Flags(call);
	 WN_Set_Call_Does_Mem_Alloc(call);
	 WN_INSERT_BlockLast(heap_block,call);
	 WN_Delete(k0);
	 WN_kid0(stmt) = WN_LdidPreg(Pointer_type,pointer_return_reg);
	 WN_INSERT_BlockLast(heap_block,stmt);
	 
       }
     }
   }
   
   return (TRUE);
}


/*
 * F90_Lower_Copy_To_ATemp - Copy an expression to an array temp 
 * 
 * WN * F90_Lower_Copy_To_ATemp(WN **alloc_block, WN **free_block, WN **copy_store,
 *                              WN *expr, WN **size, INT ndim)
 *   alloc_block (input/output) - a block of statements doing the allocate
 *   free_block (input/output) - a block of statements freeing the allocated memory
 *   copy_store (output) - the copy store statement
 *   expr (input) - the expression to move
 *   size (input) - array of WHIRL nodes indicating the size for each dimension
 *   ndim (input) - number of dimensions
 *
 *   returns - A WN representing a load of the copy.
 * 
 */

static WN * F90_Lower_Copy_To_ATemp(WN **alloc_block, WN **free_block, WN **copy_store,
			     WN *expr, WN **size, INT ndim)
{
   ST *temp_st;
   TY_IDX temp_ty,ptr_ty;
   WN_ESIZE element_size;
   WN *arrsection;
   WN *arrexp;
   WN *store;
   WN *load;
   WN *arg;
   WN *kid0;
   WN *stride;
   WN *mload_size;
   INT i;
   OPCODE expr_op;
   BOOL is_mexpr;
   BOOL is_char;
   BOOL multiply_indices=FALSE;
   WN *sizemult;
   TYPE_ID expr_type;

#ifdef KEY // bug 8083
   Set_addr_saved_expr(expr, FALSE);
#endif

   /* First, determine the type of the expression */
   is_mexpr = FALSE;
   is_char = FALSE;
   mload_size = NULL;

   expr_op = WN_opcode(expr);
   expr_type = OPCODE_rtype(expr_op);
   if (expr_type == MTYPE_B) expr_type = MTYPE_I4; // until MTYPE_B is fully implemented

   if (OPCODE_operator(expr_op) == OPR_ARRAYEXP) {
      /* Remove the arrayexp before proceeding */
      kid0 = WN_kid0(expr);
      WN_kid0(expr) = WN_Zerocon(MTYPE_I4); /* Just a dummy */
      WN_DELETE_Tree(expr); /* get rid of the ARRAYEXP */
      return F90_Lower_Copy_To_ATemp(alloc_block,free_block,copy_store,kid0,size,ndim);
   }
   
   if (expr_op == OPC_MLOAD) {
      is_mexpr = TRUE;
      mload_size = WN_kid1(expr);
      ptr_ty = WN_ty(expr);
      temp_ty = TY_pointed(ptr_ty);
      if (TY_kind(temp_ty) == KIND_ARRAY) {
	 temp_ty = TY_AR_etype(temp_ty);
      }
      is_char = TY_is_character(temp_ty);
      if (is_char) {
	 /* Need to use the larger character type */
	 temp_ty = TY_pointed(ptr_ty);
      }
      if (WN_operator(mload_size)==OPR_INTCONST) {
	 element_size = WN_const_val(mload_size);
      } else {
	 element_size = -1;
	 multiply_indices = TRUE;
      }
   } else if (expr_op == OPC_MINTRINSIC_OP || 
	      (OPCODE_operator(expr_op) == OPR_INTRINSIC_OP &&
	       F90_Is_Transformational(WN_GET_INTRINSIC(expr)) &&
	       WN_opcode(WN_kid0(expr)) == OPC_MPARM)) {
      is_mexpr = TRUE;
      /* Pick up the type from the first child */
      arg = WN_kid0(expr);
      temp_ty = WN_ty(arg);
      /* This will be the type of the parameter; we need to strip off an ARRAY ty 
       * to get to the scalar part.
       */
      if (TY_kind(temp_ty) == KIND_ARRAY) {
         temp_ty = TY_AR_etype(temp_ty);
      }
      is_char = TY_is_character(temp_ty);
      /* Need to make pointer type */
      ptr_ty = Make_Pointer_Type(temp_ty);
      element_size = TY_size(temp_ty);
      if (element_size == 0) {
	 element_size = -1;
	 multiply_indices = TRUE;
	 mload_size = get_first_dimension_ubound(temp_ty);
      } else {
	 mload_size = WN_CreateIntconst(OPCint,element_size);
      }
   } else {
      temp_ty = Be_Type_Tbl(expr_type);
      element_size = TY_size(temp_ty);
      ptr_ty = Make_Pointer_Type(temp_ty);
   }

   temp_st = F90_Lower_Create_Temp(alloc_block,free_block,size,ndim,temp_ty,mload_size);
   
   /* Create the ARRSECTION node referring to the temp */
   arrsection = WN_Create(OPCarrsection,2*ndim+1);
   /* Reset ptr_ty to get rid of the f90_pointer attribute */
   if (TY_is_f90_pointer(ptr_ty)) {
      temp_ty = TY_pointed(ptr_ty);
      ptr_ty = Make_Pointer_Type(temp_ty);
   }
   WN_kid0(arrsection) = WN_Ldid(Pointer_type, (WN_OFFSET) 0, temp_st, ptr_ty);
   WN_element_size(arrsection) = element_size;
   if (multiply_indices) {
      sizemult = WN_COPY_Tree(mload_size);
   }
   for (i=0; i < ndim; i++) {
      /* Kids 1 to ndim are the array sizes */
      if (multiply_indices) {
         WN_kid(arrsection,i+1) = WN_COPY_Tree(sizemult);
         sizemult = WN_CreateExp2(OPCmpy,sizemult,WN_COPY_Tree(size[i]));
      } else {
         WN_kid(arrsection,i+1) = WN_COPY_Tree(size[i]);
      }
      /* Kids ndim+1 to 2*ndim are the index expressions */
      stride = WN_CreateIntconst(OPCint,(INT64) 1);
      WN_kid(arrsection,i+1+ndim) = WN_CreateExp3(OPCtriplet,
						  WN_CreateIntconst(OPCint,(INT64) 0),
						  stride,
   						  WN_COPY_Tree(size[i]));
   }
   if (multiply_indices) {
      WN_DELETE_Tree(sizemult);
   }
   /* Create the ISTORE node into the temp */
   arrexp = F90_Wrap_ARREXP(arrsection);
   if (is_mexpr) {
      store = WN_CreateMstore((WN_OFFSET) 0, ptr_ty, expr, arrexp, WN_COPY_Tree(mload_size));
      load = WN_CreateMload((WN_OFFSET) 0, ptr_ty, 
			    WN_COPY_Tree(arrsection),WN_COPY_Tree(mload_size));
   } else {
      store = WN_Istore(expr_type,(WN_OFFSET) 0, ptr_ty, arrexp, expr);
      load = WN_RIload(expr_type,expr_type,(WN_OFFSET) 0, ptr_ty, WN_COPY_Tree(arrsection));
   }
   
   *copy_store = store;
   temp_allocations_inserted = TRUE;
   return (load);
}

/*
 * F90_Lower_Copy_To_STemp - Copy an expression to a scalar temp 
 * 
 * WN * F90_Lower_Copy_To_STemp(WN **copy_store,WN *expr)
 *   copy_store (output) - the copy store statement
 *   expr (input) - the expression to move
 *
 *   returns - A WN representing a load of the copy.
 * 
 */

static WN * F90_Lower_Copy_To_STemp(WN **copy_store,WN *expr)
{
   ST *temp_st;
   TY_IDX temp_ty,ptr_ty;
   WN *lda;
   WN *load;
   OPCODE expr_op;
   TYPE_ID type;
   PREG_NUM p;

   /* First, determine the type of the expression */
   expr_op = WN_opcode(expr);
   if (expr_op == OPC_MLOAD) {
      ptr_ty = WN_ty(expr);
      /* Strip off the "pointer to" part */
      temp_ty = TY_pointed(ptr_ty);
      /* Strip off the f90_pointer attribute */
      ptr_ty = Make_Pointer_Type(temp_ty);
      temp_st = F90_Lower_Create_Temp(NULL,NULL,NULL,0,temp_ty,NULL);
      lda = WN_Lda(Pointer_type,(WN_OFFSET) 0, temp_st);
      *copy_store = WN_CreateMstore((WN_OFFSET) 0, ptr_ty, expr, lda, WN_COPY_Tree(WN_kid1(expr)));
      load = WN_CreateMload((WN_OFFSET) 0, ptr_ty, WN_COPY_Tree(lda),WN_COPY_Tree(WN_kid1(expr)));
   } else {
      type = OPCODE_rtype(expr_op);
      p = Create_Preg(type,create_tempname("@f90s"));
      *copy_store = WN_StidPreg(type,p,expr);
      load = WN_LdidPreg(type,p);
   }
   
   return (load);
}


/*
 * Walk the PU inserting the contents of all the pre- and postlists.
 * Set the pre and postlist to NULL just to make sure.
 * This is a routine to be called from F90_Walk_Statements.
 */

BOOL F90_Insert_All_Prelists(WN *stmt, WN *i_block)
{
   WN *block;
   F90_LOWER_AUX_DATA *adata;

   adata = GET_F90_MAP(stmt);
   if (adata) {
      block = PRELIST(adata);
      if (block) {
	 WN_INSERT_BlockBefore(i_block,stmt,block);
	 SET_PRELIST(adata,NULL);
      } 
      block = POSTLIST(adata);
      if (block) {
	 WN_INSERT_BlockAfter(i_block,stmt,block);
	 SET_POSTLIST(adata,NULL);
      } 
   }
   return(TRUE);
}   


/*
 * Walk the PU inserting all the temp allocations and deallocations.
 * Set the alloc_prelist and alloc_postlists to NULL just to make sure.
 * This is a routine to be called from F90_Walk_Statements.
 */
static BOOL F90_Insert_Temp_Allocations(WN *stmt, WN *i_block)
{
   WN *block;
   F90_LOWER_AUX_DATA *adata;
   
   adata = GET_F90_MAP(stmt);
   if (adata) {
      block = ALLOC_PRELIST(adata);
      if (block) {
	 WN_INSERT_BlockBefore(i_block,stmt,block);
	 SET_ALLOC_PRELIST(adata,NULL);
      } 
      block = DEALLOC_POSTLIST(adata);
      if (block) {
	 WN_INSERT_BlockAfter(i_block,stmt,block);
	 SET_DEALLOC_POSTLIST(adata,NULL);
      } 
   }
   return (TRUE);
}


/*****************************************************************
 * Special intrinsic handling
 *****************************************************************/

/* Convert by value representation required by the lowerer into a by reference
 * version required by the rest of the back-end.
 */
static void convert_to_reference(WN *wn,WN *block, WN *insert_point)
{
   ST *st;
   TY_IDX ptr_ty;
   TY_IDX ty;
   WN *lda;
   WN *store;
   WN *val;

 
   if ((WN_flag(wn) & WN_PARM_BY_REFERENCE) != 0) return;

   /* Turn the PARM node into a by reference PARM node */
   ty = WN_ty(wn);
   WN_set_flag(wn, WN_PARM_BY_REFERENCE);
   WN_set_opcode(wn,OPCODE_make_op(OPR_PARM,Pointer_Mtype,MTYPE_V));

   st = new_temp_st("@f90_reftemp");
   Set_ST_type(*st,ty);
   Set_ST_addr_passed(*st);
   
   ptr_ty = Make_Pointer_Type(ty);
   WN_set_ty(wn,ptr_ty);
   lda = WN_CreateLda(OPCODE_make_op(OPR_LDA,Pointer_Mtype,MTYPE_V),0,ptr_ty,st);
   val = WN_kid0(wn);
   WN_kid0(wn) = lda;
   store = WN_Stid(TY_mtype(ty),(WN_OFFSET) 0,st,ty,val);

   WN_INSERT_BlockBefore(block,insert_point,store);
}

/* Lower the RANDOM_NUMBER intrinsic */
static WN * lower_random_number(WN *rcall, WN *block, WN *insert_point)
{
   WN *wn;
   TYPE_ID rt;
   INTRINSIC intr;
   PREG_NUM rreg1,rreg2,rpreg;
   
   intr = WN_GET_INTRINSIC(rcall);
   if (intr == INTRN_F8I4RAN) {
      rt = MTYPE_F8;
   } else {
      rt = MTYPE_F4;
   }

   if (WHIRL_Return_Info_On) {

      RETURN_INFO return_info = Get_Return_Info (Be_Type_Tbl(rt),
                                                 Use_Simulated);

      if (RETURN_INFO_count(return_info) <= 2) {

	 rreg1 = RETURN_INFO_preg (return_info, 0);
	 rreg2 = RETURN_INFO_preg (return_info, 1);
      }

      else
         Fail_FmtAssertion ("lower_random_number: more than 2 return registers");
   }

   else
     Get_Return_Pregs(rt,MTYPE_V, &rreg1, &rreg2);
   /* Change into an INTRINSIC_CALL, then return the load of the PREG */
   WN_DELETE_Tree(rcall);
   rcall = WN_Create_Intrinsic(OPCODE_make_op(OPR_INTRINSIC_CALL,rt,MTYPE_V),intr,0,NULL);
   WN_INSERT_BlockBefore(block, insert_point, rcall);
   rpreg = Create_Preg(rt,create_tempname("@f90ran"));
   wn = WN_StidPreg(rt,rpreg,WN_LdidPreg(rt,rreg1));
   WN_INSERT_BlockBefore(block, insert_point, wn);
   wn = WN_LdidPreg(rt,rpreg);
   return (wn);
}


/* Lower the CHAR intrinsic */
static WN * lower_char (WN *ival,WN *block, WN *insert_point)
{
   ST *st;
   TY_IDX ptr_ty;
   WN *lda;
   WN *istore;
   TY *char_ty_p;

   /* Build up the ty for the character temp if needed */
   if (!char_ty) {
      char_ty_p = &New_TY (char_ty) ; 
      
      TY_Init(*char_ty_p,1,KIND_SCALAR,MTYPE_I1,Save_Str(".character."));
      Set_TY_align(char_ty,1) ;
      Set_TY_is_character(*char_ty_p);
   }
   st = new_temp_st("@f90_chartemp");
   Set_ST_type(*st,char_ty);

   ptr_ty = Make_Pointer_Type(char_ty);
   lda = WN_CreateLda(OPCODE_make_op(OPR_LDA,Pointer_Mtype,MTYPE_V),0,ptr_ty,st);

   istore = WN_CreateIstore(OPC_I1ISTORE,0,ptr_ty,WN_kid0(ival),WN_COPY_Tree(lda));
   WN_Delete(ival);

   WN_INSERT_BlockBefore(block,insert_point,istore);
   return (lda);
}

/* Lower the MERGE intrinsic for character and derived types */

static WN * lower_merge (WN *kids[],WN *block, WN *insert_point)
{
   WN *t_case;
   WN *f_case;
   WN *condition;
   TY_IDX ptr_ty;
   TY_IDX temp_ty;
   ST *temp_st;
   WN *lda;
   WN *size;
   WN *true_block,*false_block;
   WN *store;
   WN *t_store;
   WN *f_store;
   WN *result;
   WN *alloc_block = NULL;
   WN *free_block = NULL;
   
   condition = WN_kid0(kids[0]);
   t_case = WN_kid0(kids[1]);
   f_case = WN_kid0(kids[2]);
   WN_Delete(kids[0]);
   WN_Delete(kids[1]);
   WN_Delete(kids[2]);

   if (WN_opcode(t_case) == OPC_MMLDID ||
       WN_opcode(t_case) == OPC_MMILOAD) {

     temp_ty = WN_ty(t_case);

     temp_st = F90_Lower_Create_Temp(&alloc_block,&free_block,NULL,0,
                                     temp_ty,NULL);
     
     t_store = WN_CreateStid(OPC_MSTID, 0, temp_st, temp_ty, t_case);
     f_store = WN_CreateStid(OPC_MSTID, 0, temp_st, temp_ty, f_case);
     
     result = WN_CreateLdid(OPC_MMLDID, 0, temp_st, temp_ty);
   }
   else {
     /* t_case and f_case had better be MLOADs at this point */
     FmtAssert((WN_opcode(t_case) == OPC_MLOAD),("Expected an MLOAD node"));
     FmtAssert((WN_opcode(f_case) == OPC_MLOAD),("Expected an MLOAD node"));

     ptr_ty = WN_ty(t_case);
     /* Strip off the "pointer to" part */
     temp_ty = TY_pointed(ptr_ty);
     temp_st = F90_Lower_Create_Temp(&alloc_block,&free_block,NULL,0,
                                     temp_ty,NULL);
     lda = WN_Lda(Pointer_type,(WN_OFFSET) 0, temp_st);
     size = WN_kid1(t_case);

     t_store = WN_CreateMstore((WN_OFFSET) 0, ptr_ty, t_case, WN_COPY_Tree(lda), WN_COPY_Tree(size));

     f_store = WN_CreateMstore((WN_OFFSET) 0, ptr_ty, f_case, WN_COPY_Tree(lda), WN_COPY_Tree(size));

     if (TY_is_character(temp_ty)) {
        result = lda;
     } else {
        result = WN_CreateMload((WN_OFFSET) 0, ptr_ty, lda, WN_COPY_Tree(size));
     }
   }

   true_block = WN_CreateBlock();
   false_block = WN_CreateBlock();

   WN_INSERT_BlockFirst(true_block,t_store);
   WN_INSERT_BlockFirst(false_block,f_store);

   if (alloc_block) {
      WN_INSERT_BlockBefore(block,insert_point,alloc_block);
   }

   /* Put the if in the block */
   store = WN_CreateIf(condition,true_block,false_block);
   WN_INSERT_BlockBefore(block,insert_point,store);

   if (free_block) {
      WN_INSERT_BlockAfter(block,insert_point,free_block);
   }
   
   return (result);
}

static void F90_Lower_Intrinsic_Fixup_walk(WN *expr, WN *stmt, WN *block) 
{
   WN *kid;
   WN *newkid;
   INT i,j,numkids;
   INTRINSIC intr;
   
      numkids = WN_kid_count(expr);
      for (i=0; i < numkids; i++) {
	 kid = WN_kid(expr,i);
	 F90_Lower_Intrinsic_Fixup_walk(kid, stmt, block);
	 if (WN_operator(kid) == OPR_INTRINSIC_OP) {
	    intr = WN_GET_INTRINSIC(kid);
	    switch (intr) {
	       /* Despite these being marked by_reference, we've already set 
		* the parameters correctly.
		*/
	     case INTRN_CEQEXPR:
	     case INTRN_CNEEXPR:
	     case INTRN_CGEEXPR:
	     case INTRN_CGTEXPR:
	     case INTRN_CLEEXPR:
	     case INTRN_CLTEXPR:
	       break;

	     case INTRN_F8I4RAN:
	     case INTRN_F4I4RAN:
	       newkid = lower_random_number(kid,block,stmt);
	       WN_kid(expr,i) = newkid;
	       break;

	     case INTRN_CHAR:
	       newkid = lower_char(WN_kid0(kid),block,stmt);
	       WN_kid(expr,i) = newkid;
	       WN_Delete(kid);
	       break;
	       
	     case INTRN_MERGE:
	       newkid = lower_merge(&WN_kid0(kid),block,stmt);
	       WN_kid(expr,i) = newkid;
	       WN_Delete(kid);
	       break;

	     default:
	       if (!INTRN_by_value(intr)) {
		  for (j=0; j < WN_kid_count(kid); j++) {
		     convert_to_reference(WN_kid(kid,j),block,stmt);
		  }
	       }
	       break;
	    }
	 }
      }
}

static BOOL F90_Lower_Intrinsic_Fixup(WN *stmt, WN *block)
{
   F90_Lower_Intrinsic_Fixup_walk(stmt,stmt,block);
   return (TRUE);
}



/*===============================================================================
 * Slightly different interface to Copy_To_ATemp; this takes a tree, a statement
 * and a block, and copies the tree into the block just before statement. The allocation goes
 * on the prelist of the new store, the deallocation on the postlist of the current statement
 * 
 *  returns - a load of the new temp.
 *
 * expr - expression to move
 * stmt - original statement, place to insert things
 * block - block to insert things into
 *
 *================================================================
 */
static WN * F90_Lower_Copy_Expr_to_Temp(WN *expr, WN *stmt, WN *block) 
{
   INT ndim,i;
   WN *sizes[MAX_NDIM];
   WN *temp;
   WN *copy_store;
   
   F90_LOWER_AUX_DATA *adata,*adatatemp;
   
   adata = GET_F90_MAP(stmt);
   if (!adata) {
      adata = F90_Lower_New_Aux_Data(0);
      SET_F90_MAP(stmt,adata);
   }

   if (F90_Size_Walk(expr,&ndim,sizes)) {
      /* Array temporary */
      adatatemp = F90_Lower_New_Aux_Data(ndim);
      /* Fill in the iteration counts */
      for (i=0; i <ndim ; i++) {
	 SET_ITER_COUNT(adatatemp,i,sizes[i]);
      }
      temp = F90_Lower_Copy_To_ATemp(&ALLOC_PRELIST(adatatemp),&DEALLOC_POSTLIST(adata),
					&copy_store,expr,sizes,ndim);
      SET_F90_MAP(copy_store,adatatemp);
      WN_INSERT_BlockFirst(PRELIST(adatatemp),PRELIST(adata));
      WN_INSERT_BlockFirst(ALLOC_PRELIST(adatatemp),ALLOC_PRELIST(adata));
      SET_PRELIST(adata,WN_CreateBlock());
      SET_ALLOC_PRELIST(adata,WN_CreateBlock());
   } else {
      /* Scalar */
      temp = F90_Lower_Copy_To_STemp(&copy_store,expr);
   }
   WN_INSERT_BlockBefore(block,stmt,copy_store);
   return (temp);
}   

/**********************************************************************
* Dependence analysis
***********************************************************************
*
*  F90_Lower_Init_Dep_Info(DEP_INFO *d, INT ndim)
*
*   Initialize the dep DEP_INFO data structure, 
*   with a number of dimensions equal to ndim. It is initialized to 
*   INDEPENDENT, and all summaries are set to DONT_CARE.
*/

static void F90_Lower_Init_Dep_Info(DEP_INFO *d, INT ndim) 
{
   INT i;

   /* allocate the main structure */
   SET_DEP_NDIM(d,ndim);
   SET_DEP_SUMMARY(d,DEP_INDEPENDENT);

   /* Initialize fields */
   for (i=0; i < MAX_NDIM; i++) {
      SET_DEP_DIRECTION(d,i,DIR_DONTCARE);
   }
   return;
}

static void print_dep_info(DEP_INFO *d)
{
   static const char * summ[4] = {"UNK","IND","===","REM"};
   static const char * dirr[5] = {"/","+","-","0","?"};
   INT i;
   
   fprintf(TFile,"%s ",summ[d->summary]);
   if (d->summary == DEP_REMOVABLE) {
      fprintf(TFile,":");
      for (i=0; i < d->ndim; i++) {
	 fprintf(TFile," %s",dirr[DEP_DIRECTION(d,i)]);
      }
   }
   fprintf(TFile,"\n");
}

/*
 *  static BOOL F90_Lower_Merge_Dep_Info(DEP_INFO *in1, DEP_INFO *in2)
 *
 * in1(input/output) - the accumulating dependence info, modified by the routine
 * in2(input) - the additional dependence information to merge
 * 
 *   Merge the dependence info in in1 and in2. Returns TRUE
 *   if the result of the merge is anything but UNKNOWN, since once we get an UNKNOWN
 *   we can give up. See the lowering document for the details (summarized here)

 Merging analysis results
 Since the statement as a whole needs to be dealt with, it is necessary to 
 merge pair-wise dependencies into an overall summary. Let A be the 
 overall summary and B be the dependence information being merged. A 
 is initialized to INDEPENDENT. Produce a new A by applying the fol
 lowing rules, in order:
 -- If B is INDEPENDENT, A is unchanged
 -- If B is UNKNOWN, A is unknown and the analysis terminates.
 -- If B is IDENTICAL and A is INDEPENDENT, then A is IDENTICAL.
 -- If B is IDENTICAL, A is unchanged.
 -- If B is REMOVABLE and A is either IDENTICAL or INDEPENDENT, then A becomes B.
 -- If one is applying this rule, then A is REMOVABLE and B is REMOVABLE.
    The result is determined by going over the array of 
    per-axis information, merging them using the rules
    If A and B are the same, no change
    If either one is unknown, return unknown
    If one is 0, return the other
    If one is positive and the other negative, return unknown.
    If any of the axes returns unknown, the overall type is UNKNOWN
    otherwise it remains REMOVABLE.
***********************************************************************/

static BOOL F90_Lower_Merge_Dep_Info(DEP_INFO *in1, DEP_INFO *in2) 
{

   INT ndim1,ndim2;
   INT i;
   DEP_SUMMARY sum1,sum2;
   DIR_FLAG dir1,dir2,dir_merge;
   BOOL all_zero;

#define SHOW_DEP_RES  if (trace_depinfo) {fprintf(TFile,"M :");  print_dep_info(in1);}

   if (trace_depinfo) {
      fprintf(TFile,"===============\n1 :");
      print_dep_info(in1);
      fprintf(TFile,"2 :");
      print_dep_info(in2);
   }

   ndim1 = DEP_NDIM(in1);
   ndim2 = DEP_NDIM(in2);
   sum1 = DEP_SUMMARY(in1);
   sum2 = DEP_SUMMARY(in2);

   /* If the number of dimensions is different, or either is UNKNOWN, give up */
   if ((ndim1 != ndim2) ||
       (sum1 == DEP_UNKNOWN) ||
       (sum2 == DEP_UNKNOWN)) {
      SET_DEP_SUMMARY(in1,DEP_UNKNOWN);
      SHOW_DEP_RES;
      return (FALSE);
   }
   if (sum2 == DEP_INDEPENDENT) {
      SHOW_DEP_RES;
      return (TRUE);
   }
   if (sum2 == DEP_IDENTICAL) {
      if (sum1 == DEP_INDEPENDENT) {
	 SET_DEP_SUMMARY(in1,DEP_IDENTICAL);
      } 
      SHOW_DEP_RES;
      return (TRUE);
   }
   
   /* sum2 is REMOVABLE */
   if (sum1 == DEP_IDENTICAL || sum1 == DEP_INDEPENDENT) {
      *in1 = *in2;
      SHOW_DEP_RES;
      return (TRUE);
   }
   
   all_zero = TRUE;
   for (i=0; i < ndim1; i++) {
      dir1 = DEP_DIRECTION(in1,i);
      dir2 = DEP_DIRECTION(in2,i);
      if (dir1 == dir2) {
	 dir_merge = dir1;
      } else if (dir1 == DIR_UNKNOWN || dir2 == DIR_UNKNOWN) {
	 dir_merge = DIR_UNKNOWN;
      } else if (dir1 == DIR_ZERO) {
	 dir_merge = dir2;
      } else if (dir2 == DIR_ZERO) {
	 dir_merge = dir1;
      } else if ((dir1 == DIR_POSITIVE && dir2 == DIR_NEGATIVE) ||
		 (dir2 == DIR_POSITIVE && dir1 == DIR_NEGATIVE)) {
	 dir_merge = DIR_UNKNOWN;
      }
      if (dir_merge == DIR_UNKNOWN) {
	 SET_DEP_SUMMARY(in1,DEP_UNKNOWN);
	 SHOW_DEP_RES;
	 return (FALSE);
      }	 
      if (dir_merge != DIR_ZERO) all_zero = FALSE;
      SET_DEP_DIRECTION(in1,i,dir_merge);
   }
   if (all_zero) {
      SET_DEP_SUMMARY(in1,DEP_IDENTICAL);
   }
   SHOW_DEP_RES;
   return (TRUE);
}



/************************************************************************************
*
* Helper routine to pick apart something we think is an address, and get at the base symbol
* and offset. It sets a flag with some properties of the symbol.
*
*/

#define F90_FORMAL 1       /* symbol is a formal */
#define F90_UNALIASED 2    /* Symbol is known to be unaliased */
#define F90_BASED 4        /* Symbol is BASED */
#define F90_UNKNOWN 8      /* Can't figure it out */
#define F90_TARGET 16      /* Symbol is a target */
#define F90_POINTER 32     /* Symbol is an F90 pointer */
#define F90_THROUGH_ARRAY 64   /* Symbol occurs under an ARRAY or ARRAYEXP node */
#define F90_THROUGH_EXPR 128   /* Symbol occurs under an ADD, SUB or MPY node */
#define F90_EXPR 256           /* LDID only, is an expression, not an address */
#define F90_ARRSECTION     512   /* is it multidimensional? */
#define F90_INDIRECTION   1024   /* is there any indirection underneath? */


static void f90_flag_dump(FILE *f, INT flag)
{
   fprintf(f,"Flag = 0x%x: ",flag);
   if (flag & F90_FORMAL) fprintf(f,"formal,");
   if (flag & F90_UNALIASED) fprintf(f,"unaliased,");
   if (flag & F90_BASED) fprintf(f,"based,");
   if (flag & F90_UNKNOWN) fprintf(f,"unknown,");
   if (flag & F90_TARGET) fprintf(f,"target,");
   if (flag & F90_POINTER) fprintf(f,"pointer,");
   if (flag & F90_THROUGH_ARRAY) fprintf(f,"array,");
   if (flag & F90_THROUGH_EXPR) fprintf(f,"texpr,");
   if (flag & F90_EXPR) fprintf(f,"expr,");
   if (flag & F90_ARRSECTION) fprintf(f,"asect,");
   if (flag & F90_INDIRECTION) fprintf(f,"indirect");
   fprintf(f,"\n");
}

INT f90_fdump(INT f)
{
   f90_flag_dump(stdout,f);
   return (0);
}

static const char * f90_depsum_name(DEP_SUMMARY d)
{
   switch (d) {
    case DEP_UNKNOWN: return ("UNKNOWN"); 
    case DEP_INDEPENDENT: return ("INDEPENDENT");
    case DEP_IDENTICAL: return ("IDENTICAL"); 
    case DEP_REMOVABLE: return ("REMOVABLE"); 
   }
   return ("error");
}

#define MAX_ADDR_PIECES 128

/* Information for the LHS symbol (since this is going to be used multiple times) */
static WN *lhs_address;
static WN *lhs_arrsection; /* Set in analyze_assignment */
static ST *lhs_sym;
static WN *lhs_pieces[MAX_ADDR_PIECES];
static INT lhs_num_pieces;
static INT64 lhs_const_offset,lhs_base_offset,lhs_size;
static INT lhs_flag;
static TY_IDX lhs_ty;

/* Information for the RHS symbol (since this is going to be used multiple times) */
static ST *rhs_sym;
static WN *rhs_pieces[MAX_ADDR_PIECES];
static INT rhs_num_pieces;
static INT64 rhs_const_offset,rhs_base_offset,rhs_size;
static INT rhs_flag;
static TY_IDX rhs_ty;


static BOOL is_f90_pointer(WN *addr)
{
   OPERATOR opr;
   ST *st;
   opr = WN_operator(addr);
   if (opr == OPR_LDID || opr == OPR_LDA) {
      st = WN_st(addr);
      if (ST_sym_class(st) == CLASS_VAR) {
	 return (ST_is_f90_pointer(st));
      } else {
	 return (FALSE);
      }
   } else if (opr == OPR_ILOAD) {
      return (TY_is_f90_pointer(TY_pointed(WN_load_addr_ty(addr))) ||
	      TY_is_f90_pointer(WN_load_addr_ty(addr)));
   } else {
      return (TRUE);
   }
}

static BOOL is_f90_target(WN *addr)
{
   OPERATOR opr;
   ST *st;
   opr = WN_operator(addr);
   if (opr == OPR_LDID || opr == OPR_LDA) {
      st = WN_st(addr);
      if (ST_sym_class(st) == CLASS_VAR) {
	 return (ST_is_f90_target(st));
      } else {
	 return (FALSE);
      }
   } else {
      return (TY_is_f90_pointer(TY_pointed(WN_load_addr_ty(addr))));
   }
}

static BOOL is_f90_formal(WN *addr)
{
   OPERATOR opr;
   WN *kid;
   ST *st;
   opr = WN_operator(addr);
   if (opr == OPR_LDID || opr == OPR_LDA) {
      st = WN_st(addr);
      return ((ST_sclass(st) == SCLASS_FORMAL) ||
	      (ST_sclass(st) == SCLASS_FORMAL_REF));
   } else if (opr == OPR_ILOAD) {
      kid = WN_kid0(addr);
      opr = WN_operator(kid);
      if (opr == OPR_LDID || opr == OPR_LDA) {
	 st = WN_st(kid);
	 return ((ST_sclass(st) == SCLASS_FORMAL) || 
		 (ST_sclass(st) == SCLASS_FORMAL_REF));
      } else {
	 return (FALSE);
      }
   }
   return (FALSE);
}

#define ADD_PIECE(x) if (*num_pieces < MAX_ADDR_PIECES) { \
	    pieces[*num_pieces] = (x); \
	    *num_pieces = *num_pieces + 1; \
	 } else { \
	    *sym = NULL; \
	    *flag = F90_UNKNOWN; \
	    done = TRUE; \
	    continue; \
	 }


static void Analyze_one_address(WN *a, ST **sym, WN **pieces, INT *num_pieces, 
				INT64 *const_offset, INT64 *base_offset, INT64 *size,
				INT *flag, TY_IDX *ty)
{
   OPERATOR opr,opr1;
   ST *s;
   TY_IDX t;
   WN *addr;
   BOOL done;
   WN *kid;
   
   *base_offset = 0;
   *size = 0;
   *sym = NULL;
   *num_pieces = 0;
   *flag = 0;
   *ty = (TY_IDX)0;
   addr = a;
   done = FALSE;
   
   while (!done) {
      opr = WN_operator(addr);
      if (opr == OPR_LDID) {
	 ADD_PIECE(addr);
	 if (trace_dependence) fprintf(TFile,"  Analyze_one_address (LDID): ");
	 /* An indirect reference of something */
	 s = WN_st(addr);
	 t = WN_ty(addr);
	 *const_offset += WN_offset(addr);
	 *ty = t;
	 *sym = s;
	 if (ST_sym_class(s) == CLASS_CONST) {
	    *flag = F90_UNALIASED;
	    /* Early exit */
	    return;
	 } 
	 if (ST_sclass(s) == SCLASS_FORMAL || ST_sclass(s) == SCLASS_FORMAL_REF ) {
	    *flag |= F90_FORMAL | F90_UNALIASED;
	 } else if (ST_sclass(s) == SCLASS_REG) {
	    DevWarn(("Should not see in PREG in F90 alias analysis"));
	 } else {
	    /* An indirect reference to some local quantity, either an
	       allocatable or a pointer */
	    *flag |= F90_INDIRECTION;
	    Base_Symbol_And_Offset(s,sym,base_offset);
	    if (ST_pt_to_unique_mem(s)) {
	       *flag |= F90_UNALIASED;
	    } 
	    if (ST_is_f90_target(s)) {
	       *flag |= F90_TARGET;
	    }
	    if (ST_is_f90_pointer(s)) {
	       *flag |= F90_POINTER;
	    }
	 }
	 if (TY_kind(t) != KIND_POINTER) {
	    /* Give up on LDID */
	    *flag |= F90_EXPR;
	 }
	 done = TRUE;
      } else if (opr == OPR_LDA) {
	 ADD_PIECE(addr);
	 if (trace_dependence) fprintf(TFile,"  Analyze_one_address (LDA): ");
	 /* Direct load of an address */
	 s = WN_st(addr);
	 t = ST_type(s);
	 *sym = s;
	 *ty = t;
	 *const_offset += WN_offset(addr);
	 if (ST_sym_class(s) == CLASS_CONST) {
	    *flag = F90_UNALIASED;
	    /* Early exit */
	    return;
	 } 
	 Base_Symbol_And_Offset(s,sym,base_offset);
	 *size = TY_size(t);
	 if (ST_is_f90_target(s)) {
	    *flag |= F90_TARGET;
	 } 
	 if (ST_is_f90_pointer(s)) {
	    *flag |= F90_POINTER;
	 }
	 if (!*flag & (F90_TARGET | F90_POINTER)) {
	    /* simple local */
	    *flag |= F90_UNALIASED;
	 }
	 done = TRUE;
      } else if (opr == OPR_ARRAY || opr == OPR_ARRSECTION || opr == OPR_ILOAD) {
	 if (opr == OPR_ARRSECTION) {
	    *flag |= F90_ARRSECTION;
	 } else if (opr == OPR_ILOAD) {
	    *flag |= F90_INDIRECTION;
	    /* Pick up the type on the ILOAD, see if it's an F90_POINTER */
	    if (is_f90_pointer(addr)) {
	       *flag |= F90_POINTER;
	    }
	 }
	 /* Address is in first child */
	 ADD_PIECE(addr);
	 addr = WN_kid0(addr);
	 *flag |= F90_THROUGH_ARRAY;
	 if (trace_dependence) fprintf(TFile,"  Analyze_one_address (ARRAY/ARRSECT %d)\n",*num_pieces);
	 continue;
      } else if (opr == OPR_ARRAYEXP) {
	 /* Ignore arrayexps */
	 addr = WN_kid0(addr);
	 continue;
      } else if (opr == OPR_ADD) {
	 kid = WN_kid1(addr);
	 if (WN_operator(kid) == OPR_INTCONST) {
	    ADD_PIECE(kid);
	    addr = WN_kid0(addr);
	    if (trace_dependence) fprintf(TFile,"  Analyze_one_address (Add %lld)\n",WN_const_val(kid));
	    continue;
	 }
	 kid = WN_kid0(addr);
	 if (WN_operator(kid) == OPR_INTCONST) {
	    ADD_PIECE(kid);
	    addr = WN_kid1(addr);
	    if (trace_dependence) fprintf(TFile,"  Analyze_one_address (Add %lld)\n",WN_const_val(kid));
	    continue;
	 }
	 /* An operand isn't constant; add it to the piece list and hope one of the 
	    children is an array, arrsection LDA or LDID. If not, we are completely
	    without hope of figuring out what is going on */
	 ADD_PIECE(addr);
	 opr1 = WN_operator(WN_kid0(addr));
	 if (opr1 == OPR_LDID || opr1 == OPR_LDA ||
	     opr1 == OPR_ARRAY || opr1 == OPR_ARRSECTION) {
	    addr = WN_kid0(addr);
	    *flag |= F90_THROUGH_EXPR;
	    if (trace_dependence) fprintf(TFile,"  Analyze_one_address (Add expr %d\n)",*num_pieces );
	    continue;
	 }
	 opr1 = WN_operator(WN_kid1(addr));
	 if (opr1 == OPR_LDID || opr1 == OPR_LDA ||
	     opr1 == OPR_ARRAY || opr1 == OPR_ARRSECTION) {
	    addr = WN_kid1(addr);
	    *flag |= F90_THROUGH_EXPR;
	    if (trace_dependence) fprintf(TFile,"  Analyze_one_address (Add expr %d\n)",*num_pieces );
	    continue;
	 }
	 /* Alas, we are lost! */
	 *flag = F90_UNKNOWN;
	 *sym = NULL;
	 done = TRUE;
	 continue;
      } else {
	 /* Again, we don't know what to do anymore */
	 *flag = F90_UNKNOWN;
	 *sym = NULL;
	 done = TRUE;
	 continue;
      }
   }
   if (trace_dependence) {
      if (*sym) {
	 fprintf(TFile,"sym: %s, base_offset %lld, const_offset %lld, size %lld\n",
		 ST_name(*sym),*base_offset,*const_offset,*size);
	 f90_flag_dump(TFile,*flag);
      } else {
	 fprintf(TFile,"sym not found\n");
      }
   }
   
   return;
}

/* Do a subtract. Pick the right size opcode */
static WN * get_difference(WN *t1, WN *t2)
{
   INT ty1,ty2;
   OPCODE subop;
   ty1 = MTYPE_size_reg(WN_rtype(t1));
   ty2 = MTYPE_size_reg(WN_rtype(t2));
   if (ty1 == 32 && ty2 == 32) {
      subop = OPC_I4SUB;
   } else {
      subop = OPC_I8SUB;
   }
   return (WN_CreateExp2(subop,t1,t2));
}


/***************************************************************
*
* Try to figure out if two indices overlap. It returns either
*  DIR_POSITIVE - if each value in i2 < corresponding value in i1 (need to run loop forward)
*  DIR_POSITIVE - if each value in i2 > corresponding value in i1 (need to run loop backward)
*  DIR_ZERO - if the two values are always equal
*  DIR_DONTCARE - if the two values are never equal
*  DIR_UNKNOWN - if the eqality can't be determined
*
* This routine makes heavy use of the simplifier to determine the state of affairs.
*
*/


#define SHOW_REASON(x) if (trace_dependence){fprintf(TFile,"returns %d %s\n",r,x);}

static DIR_FLAG Analyze_index(WN *i1, WN *i2) {
   DIR_FLAG r;
   OPERATOR opr1,opr2;
   WN *lb1, *ex1, *st1, *st2;
   WN *temp,*temp1;
   INT64 diff;
   INT64 s1,s2,e,l;
   BOOL e_constant;
   BOOL l_constant;
   BOOL s_constant;
   BOOL one_scalar;  /* Do we have a scalar and a triplet */

   if (trace_dependence) {
      fprintf(TFile,"Analyze Index:\n");
      fdump_tree(TFile,i1);
      fprintf(TFile,"------\n");
      fdump_tree(TFile,i2);
   }
      
   r = DIR_UNKNOWN; /* Start conservatively */
   opr1 = WN_operator(i1);
   opr2 = WN_operator(i2);

   /* Check for either one being an ARRAYEXP, which means we need to give up */
   if (opr1 == OPR_ARRAYEXP || opr2 == OPR_ARRAYEXP) {
      SHOW_REASON("not arrayexp");
      return(r);
   }
   
   /* Check for equality or two simple scalar constants */
   temp = get_difference(WN_COPY_Tree(i1),WN_COPY_Tree(i2));
   if (is_constant(temp)) {
      diff = WN_const_val(temp);
      if (diff == 0) {
	 r = DIR_ZERO;
	 SHOW_REASON("equality");
      } else {
	 r = DIR_DONTCARE;
	 SHOW_REASON("scalar !=");
      }
   }
   WN_DELETE_Tree(temp);
   if (r != DIR_UNKNOWN) return (r);

   /* if neither is a triplet, return here as well */
   if (opr1 != OPR_TRIPLET && opr2 != OPR_TRIPLET) {
      /* if we do not know more about the two scalars, to assume DIR_ZERO 
       * is safe, and it is better than DIR_UNKNOWN */
      r = DIR_ZERO; 
      SHOW_REASON("no triplets");
      return(r);
   }
   
   if (opr1 != OPR_TRIPLET) {
      /* Normalize triplet */
      lb1 = get_difference(WN_COPY_Tree(i1),WN_COPY_Tree(WN_kid0(i2)));
      ex1 = WN_COPY_Tree(WN_kid2(i2));
      st1 = WN_COPY_Tree(WN_kid1(i2));
      one_scalar = TRUE;
   } else if (opr2 != OPR_TRIPLET) {
      /* Normalize triplet */
      lb1 = get_difference(WN_COPY_Tree(i2),WN_COPY_Tree(WN_kid0(i1)));
      ex1 = WN_COPY_Tree(WN_kid2(i1));
      st1 = WN_COPY_Tree(WN_kid1(i1));
      one_scalar = TRUE;
   } else {
      /* Quick check, if ex1 != ex2, we can give up right away */
      ex1 = WN_COPY_Tree(WN_kid2(i1));
      lb1 = get_difference(WN_COPY_Tree(WN_kid0(i1)),WN_COPY_Tree(WN_kid0(i2)));
      st1 = WN_COPY_Tree(WN_kid1(i1));
      st2 = WN_COPY_Tree(WN_kid1(i2));
      one_scalar = FALSE;
   }

   /* First case: there is a scalar */
   if (one_scalar) {
      /* if lb1 mod st1 isn't constant, give up */
      temp = WN_CreateExp2(OPC_I8MOD,WN_COPY_Tree(lb1),WN_COPY_Tree(st1));
      if (!is_constant(temp)) {
	 SHOW_REASON("l mod s not const");
	 r = DIR_UNKNOWN;
	 WN_DELETE_Tree(temp);
      } else {
	 diff = WN_const_val(temp);
	 WN_DELETE_Tree(temp);
	 /* lb1 mod st1 != 0 and is constant; we are done */
	 if (diff != 0) {
	    r = DIR_DONTCARE;
	    SHOW_REASON("l mod s != 0");
	 } else {
	    /* Compute (lb1/st1) < 0 || (lb1/st1) >= ex1. If this is true, we have
	     * independence 
	     */
	    lb1 = WN_CreateExp2(OPC_I8DIV,lb1,WN_COPY_Tree(st1));
	    temp = WN_LT(MTYPE_I8,WN_COPY_Tree(lb1),WN_Zerocon(MTYPE_I8));
	    temp1 = WN_GE(MTYPE_I8,WN_COPY_Tree(lb1),WN_COPY_Tree(ex1));
	    temp = WN_LIOR(temp,temp1);
	    if (is_constant(temp) && WN_const_val(temp) != 0) {
	       r = DIR_DONTCARE;
	       SHOW_REASON("not contained");
	    } else {
	       r = DIR_UNKNOWN;
	       SHOW_REASON("contained");
	    }
	    WN_DELETE_Tree(temp);
	 }
      }
      WN_DELETE_Tree(lb1);
      WN_DELETE_Tree(st1);
      WN_DELETE_Tree(ex1);
      SHOW_REASON("returning");
      return (r);
   }

   /* Two triplets */

   e_constant = is_constant(ex1);
   if (e_constant) e = WN_const_val(ex1);
   l_constant = is_constant(lb1);
   if (l_constant) l = WN_const_val(lb1);
   s_constant = is_constant(st1) && is_constant(st2);
   if (s_constant) {
      s1 = WN_const_val(st1);
      s2 = WN_const_val(st2);
   }
   WN_DELETE_Tree(lb1);
   WN_DELETE_Tree(ex1);

   if (trace_dependence) {
      if (e_constant) fprintf(TFile,"E = %lld\n",e);
      if (l_constant) fprintf(TFile,"L = %lld\n",l);
      if (s_constant) fprintf(TFile,"S1,S2 = %lld %lld\n",s1,s2);
   }

   /* First, a couple of special cases */
   /* Case 1: st1 = st2 */

   temp = get_difference(st1,st2); 
   if (is_constant(temp) && WN_const_val(temp) == 0 && l_constant) {
      /* Independent if l mod s is not 0 */
      if (s_constant) {
	 if (l % s1 != 0) {
	    r = DIR_DONTCARE;
	 } else if (l < 0 && s1 > 0 || l < 0 && s1 > 0) {
	    r = DIR_POSITIVE;
	 } else if (l > 0 && s1 > 0 || l < 0 && s1 < 0) {
	    r = DIR_NEGATIVE;
	 } else if (l == 0) {
	    r = DIR_ZERO;
	 }
      }
   }
   WN_DELETE_Tree(temp);
   if (r != DIR_UNKNOWN) {
      SHOW_REASON("triplet case 1");
      return (r);
   }

   /* Case 2: e = 1 */
   if (e_constant && e == 1 && l_constant) {
      if (l == 0) {
	 r = DIR_ZERO;
      } else {
	 r = DIR_DONTCARE;
      }
   }
   if (r != DIR_UNKNOWN) {
      SHOW_REASON("triplet case 2");
      return (r);
   }

   /* Back to the general case, we only can do something if l and s are constant */
   if (l_constant && s_constant) {
      r = F90_Lower_Analyze_Triplet(l,s1,s2,e,e_constant,f90_lower_pool);
      SHOW_REASON("general constant case");
   } else {
      SHOW_REASON("done");
   }
   return (r);
}

/*================================================================
 *
 * Analyze_all_indices checks all the indices in a pair of ARRAY
 * and ARRSECTION nodes, returning the DEP_SUMMARY for the overall
 *
 * Analyze_all_indices(WN *lhs, WN *rhs, DEP_INFO *dep_info)
 *
 *================================================================*/
static DEP_SUMMARY Analyze_all_indices(WN *lhs, WN *rhs, DEP_INFO *kid_dep)
{
   BOOL allzero,allzerounknown;
   INT lhskids,rhskids;
   INT ndim,i;
   DIR_FLAG dir;
   
   lhskids = WN_kid_count(lhs);
   rhskids = WN_kid_count(rhs);
   if ((lhskids != rhskids) || (WN_element_size(lhs) != WN_element_size(rhs))) {
      SET_DEP_SUMMARY(kid_dep,DEP_UNKNOWN);
      return (DEP_UNKNOWN);
   }
   ndim = (lhskids - 1) / 2;
   SET_DEP_NDIM(kid_dep,ndim);

   /* Compute dep info for each axis */
   allzero = TRUE;
   allzerounknown = TRUE;
   for (i=0; i < ndim; i++) {
      dir = Analyze_index(WN_kid(lhs,i+ndim+1),WN_kid(rhs,i+ndim+1));
      SET_DEP_DIRECTION(kid_dep,i,dir);
      if (dir == DIR_DONTCARE) {
	 SET_DEP_SUMMARY(kid_dep,DEP_INDEPENDENT);
	 allzero = FALSE;
	 allzerounknown = FALSE;
	 break;
      } else if (dir == DIR_UNKNOWN) {
	 allzero = FALSE;
      } else if (dir == DIR_POSITIVE || dir == DIR_NEGATIVE) {
	 SET_DEP_SUMMARY(kid_dep,DEP_REMOVABLE);
	 allzero = FALSE;
	 allzerounknown = FALSE;
      }
   }
   if (allzero) {
      /* all zero means identical */
      SET_DEP_SUMMARY(kid_dep,DEP_IDENTICAL);
   } else if (allzerounknown && !allzero) {
      /* all zero or unknown means unknown if at least one unknown was found */
      SET_DEP_SUMMARY(kid_dep,DEP_UNKNOWN);
   }
   return (DEP_SUMMARY(kid_dep));
}

/*================================================================
 *
 * Check_overlap: Given bases and sizes, decide if there is overlap between 
 * two variables.
 *
 * static DEP_SUMMARY check_overlap(INT64 base1, INT64 base2, INT64 size1, INT64 size2)
 *
 * returns either DEP_IDENTICAL if the two are the same,
 *                DEP_INDEPENDENT if the two do not overlap
 *                DEP_UNKNOWN otherwise
 *
 *================================================================
 */
static DEP_SUMMARY check_overlap(INT64 base1, INT64 base2, INT64 size1, INT64 size2)
{
   INT64 t;
   if (base2 == base1 && size2 == size1) return (DEP_IDENTICAL);

   /* Swap so that base1 is always less than base2 */
   if (base1 > base2) {
      t = base2;
      base2 = base1;
      base1 = t;
      t = size2;
      size2 = size1;
      size1 = t;
   }
   if (base2 < base1 + size1) {
      return (DEP_UNKNOWN);
   } else {
      return (DEP_INDEPENDENT);
   }
}  


/* 
 * In a derived-type reference, walk from the bottom up and see if we get 
 * the same member or a definitely different one.
 * When we see an indirect reference we stop.
 */
static DEP_SUMMARY analyze_structure_bases(INT lhs_start, INT rhs_start, INT *lhs_end1, INT *rhs_end1)
{
   INT64 lc,rc;
   INT i,ri,li,lhs_end,rhs_end;
   OPERATOR opr;
   WN *lhsa,*rhsa;
   DEP_SUMMARY t;
   DEP_INFO dummy_dep;

   lc = lhs_const_offset;
   rc = rhs_const_offset;
   /* Do the LHS structure constant values */
   for (i = lhs_start; i >= 0; i--) {
      opr = WN_operator(lhs_pieces[i]);
      if (opr == OPR_ILOAD ||
	  opr == OPR_LDID) {
	 break;
      } else if (opr == OPR_INTCONST) {
	 lc += WN_const_val(lhs_pieces[i]);
      }
   }
   lhs_end = i;
   if (lhs_end1) *lhs_end1 = lhs_end;

   for (i = rhs_start; i >= 0; i--) {
      opr = WN_operator(rhs_pieces[i]);
      if (opr == OPR_ILOAD ||
	  opr == OPR_LDID) {
	 break;
      } else if (opr == OPR_INTCONST) {
	 rc += WN_const_val(rhs_pieces[i]);
      }
   }
   rhs_end = i;
   if (rhs_end1) *rhs_end1 = rhs_end;


   /* Now that we have the const vals, we need to determine the members and see if there
    * is any overlap. This can happen if the LHS type and RHS type are different
    * (due to equivalences, for example).
    */
   if (lhs_ty == rhs_ty) {
      if (lc != rc) return (DEP_INDEPENDENT);
   } else {
      return (DEP_UNKNOWN);
   }

   /* Check all the other pieces for known indentical or not known identical. */
   ri = rhs_start;
   li = lhs_start;
   while (ri > rhs_end && li > lhs_end) {
      /* Find first ARRSECTION or ARRAY node on each */
      for (; li > lhs_end ; li--) {
	 opr = WN_operator(lhs_pieces[li]);
	 if (opr == OPR_ARRSECTION || opr == OPR_ARRAY) break;
      }
      for (; ri > rhs_end ; ri--) {
	 opr = WN_operator(rhs_pieces[ri]);
	 if (opr == OPR_ARRSECTION || opr == OPR_ARRAY) break;
      }
      /* li and ri point to ARRAY and/or ARRSECTION nodes */
      if (li > lhs_end && ri > rhs_end) {
	 lhsa = lhs_pieces[li];
	 rhsa = rhs_pieces[ri];
	 /* If both are ARRSECTIONS, skip */
	 if (WN_operator(lhsa) == OPR_ARRSECTION &&
	     WN_operator(rhsa) == OPR_ARRSECTION) {
	    li--;
	    ri--;
	    continue;
	 }
	 /* array section vs. array or array vs. array */
	 /* compare all the indices. Each one will get back either 
	    independent or unknown */
	 t = Analyze_all_indices(lhsa,rhsa,&dummy_dep);
	 if (t == DEP_UNKNOWN || t == DEP_INDEPENDENT) {
	    return (t);
	 } else if (t == DEP_REMOVABLE) {
	    /* This shouldn't happen */
	    return (DEP_UNKNOWN);
	 }
	 /* Otherwise, we have an identical case */
	 li--;
	 ri--;
      } else if ((li > lhs_end && ri <= rhs_end) ||
		 (ri > rhs_end && li <= lhs_end)) {
	 /* One runs out first, return unknown */
	 return (DEP_UNKNOWN);
      } else {
	 li--;
	 ri--;
      }
   }
   
   /* At this point, we can only be identical */
   return (DEP_IDENTICAL);
}
      

/***************************************************************
*
* DEP_FLAG Analyze_bases(WN * addr, BOOL is_expr);
*
* addr - address expression for the load base 
* is_expr - true if addr is the actual load rather than the address.
*      In this case, addr should be an LDID.
* is_char - are these bases for a character variable
*
* Try to figure out if two bases might overlap. It returns either
* DEP_UNKNOWN - Can't figure it out
* DEP_INDEPENDENT - the bases are different and non-overlapping
* DEP_IDENTICAL - the bases are the same thing
*
*/

static DEP_SUMMARY Analyze_bases(WN * addr, BOOL is_expr, BOOL is_char)
{
   DEP_SUMMARY r;
   OPERATOR opr;
   INT i;
   BOOL same;
   INT  li,ri;
   WN *lhsa,*rhsa;
   BOOL l_pointer,l_target,l_formal;
   BOOL r_pointer,r_target,r_formal;
      
   r = DEP_UNKNOWN;
   if (trace_dependence) fprintf(TFile,"Analyze_bases: ");

   /* First, check for identical addresses */
   if (!is_expr) {
      if (WN_Simp_Compare_Trees(lhs_address,addr) == 0) {
	 if (trace_dependence) fprintf(TFile,"same addresses\n");
	 return (DEP_IDENTICAL);
      }
   }

   if (trace_dependence) fprintf(TFile,"\n");
   
   if (lhs_sym == NULL || rhs_sym == NULL || 
       (F90_UNKNOWN & (lhs_flag | rhs_flag))) {
      if (trace_dependence) fprintf(TFile,"unanalyzable symbols\n");
      return (DEP_UNKNOWN);
   }
   
   /* Check for case in which no indirect references exist */
   
   if (!((lhs_flag | rhs_flag) & F90_INDIRECTION)) {
      /* We know that we will eventually either have the same base or 
	 else we are independent */
      if (lhs_sym != rhs_sym) {
	 if (trace_dependence) fprintf(TFile,"no indirection, different symbols\n");
	 return (DEP_INDEPENDENT);
      }
      /* Same symbol, check for overlap in equivalences */
      r = check_overlap(lhs_base_offset,rhs_base_offset,lhs_size,rhs_size);
      if (r == DEP_INDEPENDENT || r == DEP_UNKNOWN) {
	 if (trace_dependence) fprintf(TFile,"same base, overlap check %s\n",f90_depsum_name(r));
	 return (r);
      }
      /* Truly same base. Now we need to see if the same member of the 
	 structure is referenced */
      if (!is_char) {
	 r = analyze_structure_bases(lhs_num_pieces-1,rhs_num_pieces-1,NULL,NULL);
	 if (trace_dependence) fprintf(TFile,"no indirection, same symbol %s\n",f90_depsum_name(r));
      } else {
	 /* For charcater variables, alas, we are just going to give up and say the bases 
	    are identical. This will be conservative */
	 r = DEP_IDENTICAL; 
	 if (trace_dependence) fprintf(TFile,"no indirection, same symbol (char) %s\n",
				       f90_depsum_name(r));
      }
      return (r);
   }

   /* Identical except for the ARRAYSECTION node */
   if (lhs_num_pieces == rhs_num_pieces) {
      same = TRUE;
      for (i = 0; i < lhs_num_pieces ; i++) {
	 if (WN_operator(lhs_pieces[i]) != OPR_ARRSECTION &&
	     WN_operator(rhs_pieces[i]) != OPR_ARRSECTION) {
	    if (WN_Simp_Compare_Trees(lhs_pieces[i],rhs_pieces[i]) != 0) {
	       same = FALSE;
	       break;
	    }
	 }
      }
      if (same) {
	 if (trace_dependence) fprintf(TFile,"all but ARRSECTIONs IDENTICAL\n");
	 return (DEP_IDENTICAL);
      }
   }

   /* The hard case: We have an indirection of some sort, and we need to analyze it */
   /* We only need to look at the top address */
   li = 0;
   ri = 0;
   while (li < lhs_num_pieces) {
      opr = WN_operator(lhs_pieces[li]);
      if (opr == OPR_ILOAD ||
	  opr == OPR_LDID  || 
	  opr == OPR_LDA) {
	 break;
      }
      li++;
   }
   while (ri < rhs_num_pieces) {
      opr = WN_operator(rhs_pieces[ri]);
      if (opr == OPR_ILOAD ||
	  opr == OPR_LDID  || 
	  opr == OPR_LDA) {
	 break;
      }
      ri++;
   }
   if (ri < rhs_num_pieces && li < lhs_num_pieces) {
      lhsa = lhs_pieces[li];
      rhsa = rhs_pieces[ri];
      if (WN_Simp_Compare_Trees(lhsa,rhsa)==0) {
	 /* Base addresses are the same, check things on the way up */
	 r = analyze_structure_bases(li-1,ri-1,NULL,NULL);
	 if (trace_dependence) fprintf(TFile,"same top base addresses %s\n",f90_depsum_name(r));
	 return (r);
      }
      
      /* The addresses are different. Here are the possibilities */
      /*
	 LHS/RHS  ILOAD       LDA      LDID
	 ILOAD      U          2         2
	 LDA        2          X         1
	 LDID       2          1         1
	 
	 U - unknown, since both must be F90 pointers and they are different,
	     unless one or both is a formal.
	 X - can't happen
	 1 - If the symbols are different, it's INDEPENDENT if 
	 either is lacking the TARGET attribute, unless both have the POINTER
	 attribute. If either has the FORMAL attribute, they aren't aliased.
	 
	 2 - If the ILOAD has the pointer attribute, and the LDA/LDID doesn't
	 have either the TARGET or POINTER attributes, INDEPENDENT. If either
	 has the FORMAL attribute, INDEPENDENT. If either has the TARGET or POINTER
	 attribute, UNKNOWN. If the ILOAD does not have the pointer attribute, it 
	 must either be a dummy or a cray pointer or an allocatable. In which case, 
	 we are INDEPENDENT.
	 
	 */

      /* Check for possible pointer/target combinations */
      l_pointer = is_f90_pointer(lhsa);
      l_target  = is_f90_target(lhsa);
      l_formal  = is_f90_formal(lhsa);
      r_pointer = is_f90_pointer(rhsa);
      r_target  = is_f90_target(rhsa);
      r_formal  = is_f90_formal(rhsa);
      if (l_formal) {
	 l_pointer = l_pointer || ST_is_f90_pointer(lhs_sym);
	 l_target  = l_target  || ST_is_f90_target(lhs_sym);
      }
      if (r_formal) {
	 r_pointer = r_pointer || ST_is_f90_pointer(rhs_sym);
	 r_target  = r_target  || ST_is_f90_target(rhs_sym);
      }
      if (is_f90_formal(lhsa) || is_f90_formal(rhsa)) {
	 r = DEP_INDEPENDENT;
      } else if ((l_pointer && r_target) ||
	  (r_pointer && l_target) || 
	  (r_pointer && l_pointer)) {
	 if (Alias_F90_Pointer_Unaliased) {
	    r = DEP_INDEPENDENT;
	 } else {
	    r = DEP_UNKNOWN;
	 }
      } else {
	 /* Need to check for the case where we have only one indirection */
	 if ((rhs_flag ^ lhs_flag) & F90_INDIRECTION) {
	    /* Only one, but not both */
	    r = DEP_INDEPENDENT;
	 } else if ((rhs_flag | lhs_flag) & F90_UNALIASED) { 
	    r = DEP_INDEPENDENT;
	 } else {
	    /* Last ditch attempt. If there are no F90_POINTERs 
	     * F90_TARGETs in one or the other of the subtrees, we are independent
	     */
	    l_pointer = (lhs_flag & F90_POINTER) != 0;
	    r_pointer = (rhs_flag & F90_POINTER) != 0;
	    l_target = l_pointer || ((lhs_flag & F90_TARGET) != 0);
	    r_target = r_pointer || ((rhs_flag & F90_TARGET) != 0);
	    if ((l_pointer && r_target) ||
		(r_pointer && l_target)) {
	       r = DEP_UNKNOWN;
	    } else {
	       r = DEP_INDEPENDENT;
	    }
	 }
      }
   } else {
      /* We fell off the edge, give up */
      r = DEP_UNKNOWN;
   }
   if (trace_dependence) fprintf(TFile,"bases with indirection %s\n",f90_depsum_name(r));
   return (r);
}


/******************************************************************
 *
 * Walk tree, seeing if there is a reference to the lhs data on the expr.
 * This is a recursive walker.
 * 
 * The LHS information must already have been stored in the static lhs_xxx variables.
 *
 * Dependence_Walk(WN *expr,F90_LOWER_AUX_DATA *adata, DEP_INFO *dep, 
 *                 BOOL transformational_seen, WN *parent, INT kidno, INT lhsdim, BOOL is_char_load)
 *
 * expr - expression tree to analyze
 * adata - aux data structure so that scalar copies can be generated
 * dep - dependence information about the expresion.
 * transformational_seen - true if a transformational intrinsic was seen on the way down,
 *                       or if this is is seen under a dependent ILOAD.
 * parent - immediate parent to this node. Void if no replacement to be done here.
 * kidno - child of this parent representing expr. These last two are so that we can replace 
 *       expr with a temp if necessary. 
 * lhsdim - number of dimensions on the LHS
 * is_char_load - TRUE if the expression is to be treated as if it were under an ILOAD. Used for
 *                character expressions.
 *
 */
static void Dependence_Walk(WN *expr,F90_LOWER_AUX_DATA *adata, DEP_INFO *dep, 
			    BOOL transformational_seen,
			    WN *parent, INT kidno, INT lhsdim, BOOL is_char_load)
{
   OPCODE op;
   OPERATOR opr;
   DEP_INFO kid_dep;
   WN *kid;
   WN *copy_store;
   INT i,num_kids;
   BOOL keep_going;
   DEP_SUMMARY base_dep;
   
   op = WN_opcode(expr);
   opr = OPCODE_operator(op);
   
   if (opr == OPR_LDID) {
      return; 
   } else if (opr == OPR_ILOAD || opr == OPR_MLOAD || is_char_load) {
      if (opr == OPR_ILOAD || opr == OPR_MLOAD) {
	 rhs_const_offset = WN_offset(expr);
	 kid = WN_kid0(expr);
	 if (opr == OPR_MLOAD) {
	    /* Check for dummy MLOADS */
	    if (WN_operator(kid) == OPR_ARRAYEXP) {
	       kid = WN_kid0(kid);
	    }
	    if (WN_operator(kid) == OPR_INTRINSIC_OP) {
	       /* Not a real MLOAD */
	       goto check_kids;
	    }
	 }
      } else {
	 /* Must be a character expression */
	 rhs_const_offset = 0;
	 kid = expr;
	 if (WN_operator(kid) == OPR_ARRAYEXP) {
	    kid = WN_kid0(kid);
	 }
      }
      Analyze_one_address(kid,&rhs_sym,rhs_pieces,&rhs_num_pieces,&rhs_const_offset,
		       &rhs_base_offset,&rhs_size,&rhs_flag,&rhs_ty);
      base_dep = Analyze_bases(expr,FALSE,is_char_load);
      /* Scalar copy, if we can */
      if (base_dep == DEP_UNKNOWN) {
	 if (rhs_sym != NULL && !(rhs_flag & F90_ARRSECTION) && parent) {
	    kid = F90_Lower_Copy_To_STemp(&copy_store,expr);
	    WN_kid(parent,kidno) = kid;
	    WN_INSERT_BlockLast(PRELIST(adata),copy_store);
	    SET_DEP_SUMMARY(dep,DEP_INDEPENDENT);
	 } else {
	    SET_DEP_SUMMARY(dep,DEP_UNKNOWN);
	 }
	 return;
      } else if (base_dep == DEP_INDEPENDENT) {
	 goto check_kids; /* GOTO just to be safe */
      } else { /* DEP_IDENTICAL */
	 /* If it's scalar and we can, copy it */
	 if (!(rhs_flag & F90_ARRSECTION) && parent) {
	    kid = F90_Lower_Copy_To_STemp(&copy_store,expr);
	    WN_kid(parent,kidno) = kid;
	    WN_INSERT_BlockLast(PRELIST(adata),copy_store);
	    SET_DEP_SUMMARY(dep,DEP_INDEPENDENT);
	    return;
	 }
	 /* If we are under a transformational, we need to give up */
	 if (transformational_seen) {
	    SET_DEP_SUMMARY(dep,DEP_UNKNOWN);
	    return;
	 }
	 /* Now, analyze indices */
	 /* Find the RHS arrsection */
	 if (lhs_arrsection) {
	    for (i=0; i < rhs_num_pieces && WN_operator(rhs_pieces[i]) != OPR_ARRSECTION; i++);
	    DevAssert((WN_operator(rhs_pieces[i])==OPR_ARRSECTION),("Expected to find an arrsection"));
	    (void) Analyze_all_indices(lhs_arrsection,rhs_pieces[i],&kid_dep);
	    keep_going = F90_Lower_Merge_Dep_Info(dep,&kid_dep);
	    if (!keep_going) return;
	 }
      }
   } else if (opr == OPR_INTRINSIC_OP && F90_Is_Transformational(WN_GET_INTRINSIC(expr))) {
      /* The children are seen under a transformational intrinsic */
      transformational_seen = TRUE;
   }      

check_kids:   
   /* Walk the children, merging their analyses into this one */
   num_kids = WN_kid_count(expr);
   if (opr == OPR_TRIPLET) num_kids = 2; /* Don't walk the third child of a TRIPLET */
   for (i=0; i < num_kids; i++) {
      kid = WN_kid(expr,i);
      F90_Lower_Init_Dep_Info(&kid_dep,lhsdim);
      Dependence_Walk(kid, adata, &kid_dep, transformational_seen,expr,i,lhsdim,FALSE);
      keep_going = F90_Lower_Merge_Dep_Info(dep,&kid_dep);
      if (!keep_going) break;
   }
   
   return;
}



/*************************************************************************
 * Dependence analysis for MSTORE/MLOADS. If they overlap, we have add a temp
 ************************************************************************/

static void f90_analyze_mstore(WN *stmt, WN *block)
{
   F90_LOWER_AUX_DATA *adata;
   WN *lhs,*rhs;
   DEP_INFO dep;

   stmt = stmt;
   lhs = WN_kid1(stmt);
   rhs = WN_kid0(stmt);

   adata = F90_Lower_New_Aux_Data(0);
   SET_F90_MAP(stmt,adata);

   lhs_const_offset = WN_offset(stmt);
      
   F90_Lower_Init_Dep_Info(&dep,0);
      
   lhs_address = lhs;
   lhs_arrsection = NULL;
   Analyze_one_address(lhs,&lhs_sym,lhs_pieces,&lhs_num_pieces,&lhs_const_offset,
		       &lhs_base_offset,&lhs_size,&lhs_flag,&lhs_ty);
      
      
   /* Do the LHS/RHS walk, which will move scalar dependencies */
   Dependence_Walk(rhs,adata,&dep,FALSE,stmt,0,0,FALSE);

   /* Insert the statements, if necessary */
   (void) F90_Insert_All_Prelists(stmt,block);
   (void) F90_Insert_Temp_Allocations(stmt,block);
   return;
}

/*************************************************************************
 * Dependence analysis for simple assignments and WHERE statements
 *  This removes the ARRAYEXP from the LHS of the assignment statement.
 *  It puts scalar references to the LHS variable in temps, and puts the temp
 *  assignment on the statment's PRELIST
 * 
 * The input must be either an ISTORE or MSTORE with address child 
 * an ARRAYEXP of an ARRSECTION, or else a WHERE statement containing 
 * Exactly one assignment or elemental subroutine call in either the 
 * true or false block.
 * BLOCK is the block containing STMT, so that other statements can be inserted.
 * 
 * It can also be any of the CHARACTER calls CONCAT, CASSIGNMENT, ADJUSTL, ADJUSTR
 ************************************************************************/

static void f90_analyze_assignment(WN *stmt, WN *block, BOOL is_call)
{
   F90_LOWER_AUX_DATA *adata;
   WN *lhs_arrayexp;
   INT lhs_arrayexp_kid = 0;
   INT ndim;
   INT lhsdim;
   INT i,num_kids;
   INT vec_axes,vec_axis_list[MAX_NDIM],axis;
   WN *assignment;
   WN *lhs,*rhs;
   WN *count,*copy_store;
   DEP_INFO lhs_dep,rhs_dep;
   INTRINSIC intr;
   int	rhs_kidno;
   WN	*rhs_parent;
   
   if (is_call) {
      if (WN_opcode(stmt) != OPC_WHERE) {
	 if (WN_operator(stmt)==OPR_INTRINSIC_CALL) {
	    intr = WN_GET_INTRINSIC(stmt);
	 } else {
	    intr = INTRINSIC_NONE;
	 }
	 if (WN_operator(stmt) == OPR_CALL || WN_operator(stmt) == OPR_ICALL) {
	    num_kids = WN_kid_count(stmt);
	    for (i=0; i < num_kids; i++) {
	       lhs_arrayexp = WN_kid0(WN_kid(stmt,i));
               lhs_arrayexp_kid = i;
	       if (WN_operator(lhs_arrayexp) == OPR_ARRAYEXP) {
		  break;
	       }
	    }
	 } else {
	    lhs_arrayexp = WN_kid0(WN_kid0(stmt));
	 }
	 assignment = stmt;
	 ndim = WN_kid_count(lhs_arrayexp) - 1;
      } else if (WN_opcode(stmt) == OPC_WHERE) {
	 /* Each WHERE contains a single assignment, we just need to find it */
         /* or a single elemental call for defined assignment */
	 lhs_arrayexp = WN_kid0(stmt);
	 ndim = WN_kid_count(lhs_arrayexp)-1;
	 assignment = get_assignment_from_stmt(stmt);
	 lhs_arrayexp = WN_kid0(WN_kid0(assignment));
	 if (WN_operator(assignment)==OPR_INTRINSIC_CALL) {
	    intr = WN_GET_INTRINSIC(assignment);
	 } else {
	    intr = INTRINSIC_NONE;
	 }
      }
      /* The only thing that does not go into a temp immendiately is 
       * CASSIGNMENT. Otherwise, we just set the sizes.
       */
      lhs = WN_kid0(lhs_arrayexp);
      if (intr == INTRN_CASSIGNSTMT) {
	 rhs = WN_kid0(WN_kid1(assignment));
         rhs_kidno = 0;
         rhs_parent = WN_kid1(assignment);
      } else if (WN_opcode(stmt) == OPC_WHERE &&
                 (WN_operator(assignment) == OPR_CALL || 
                  WN_operator(assignment) == OPR_ICALL)) {
	 rhs = WN_kid0(WN_kid1(assignment));
         rhs_kidno = 0;
         rhs_parent = WN_kid1(assignment);
      } else {
	 rhs = NULL; /* Don't do any dependence analysis, just set sizes */
         rhs_kidno = 0;
         rhs_parent = NULL;
      }
   } else {
      /* Set up a repository for the aux data */
      if (WN_opcode(stmt) != OPC_WHERE) {
	 lhs_arrayexp = WN_kid1(stmt);
	 ndim = WN_kid_count(lhs_arrayexp) - 1;
	 assignment = stmt;
      } else if (WN_opcode(stmt) == OPC_WHERE) {
	 /* Each WHERE contains a single assignment, we just need to find it */
         /* or a single elemental call for defined assignment */
	 lhs_arrayexp = WN_kid0(stmt);
	 ndim = WN_kid_count(lhs_arrayexp)-1;
	 assignment = get_assignment_from_stmt(stmt);
	 lhs_arrayexp = WN_kid1(assignment);
      } 
      rhs = WN_kid0(assignment);
      rhs_kidno = 0;
      rhs_parent = assignment;
      lhs = WN_kid0(lhs_arrayexp);
   }
   adata = F90_Lower_New_Aux_Data(ndim);
   SET_F90_MAP(stmt,adata);
   /* 
    * adata now on each statement, and we can walk the assignment to do the
    * dependence analysis.
    */
   
   if (rhs) {
      if (is_call) {
	 lhs_const_offset = 0;
      } else {
	 lhs_const_offset = WN_offset(assignment);
      }
      lhs_arrsection = find_arrsection(lhs);
      lhsdim = (WN_kid_count(lhs_arrsection)-1)/2;
      
      F90_Lower_Init_Dep_Info(&lhs_dep,lhsdim);
      F90_Lower_Init_Dep_Info(&rhs_dep,lhsdim);
      
      lhs_address = lhs;
      Analyze_one_address(lhs,&lhs_sym,lhs_pieces,&lhs_num_pieces,&lhs_const_offset,
			  &lhs_base_offset,&lhs_size,&lhs_flag,&lhs_ty);
      
      /* Do the LHS/LHS walk, to see if we need to do a COPY_LEFT */
      num_kids = WN_kid_count(lhs_arrsection);
      for (i=lhsdim+1; i < num_kids ; i++) {
	 Dependence_Walk(WN_kid(lhs_arrsection,i),adata,&lhs_dep,FALSE,lhs_arrsection,i,lhsdim,FALSE);
      }
      
      if (DEP_SUMMARY(&lhs_dep) != DEP_INDEPENDENT) {
	 /* Must mark this as COPY_LEFT */
	 SET_COPY_FLAG(adata,COPY_LEFT | COPY_FLAG(adata));
      }

      /* Need to check for appearence in the ARRAYEXP portion of the tree as well */
      for (i=1; i <= ndim ; i++) {
	 Dependence_Walk(WN_kid(lhs_arrayexp,i),adata,&lhs_dep,FALSE,lhs_arrayexp,i,lhsdim,FALSE);
      }
      
      /* Do the LHS/RHS walk, to see if we need to do a COPY_RIGHT */
      Dependence_Walk(rhs,adata,&rhs_dep,FALSE,rhs_parent,rhs_kidno,lhsdim,is_call);
      switch (DEP_SUMMARY(&rhs_dep)) {
       case DEP_INDEPENDENT:
       case DEP_IDENTICAL:
	 /* Don't need to do anything */
	 break;
	 
       case DEP_UNKNOWN:
	 /* Mark COPY_RIGHT */
	 SET_COPY_FLAG(adata,COPY_RIGHT | COPY_FLAG(adata));
	 break;
	 
       case DEP_REMOVABLE:
	 /* Tricky, need to set direction flags appropriately. The hard part is
	  * identifying which are the non-scalar indices.
	  */
	 vec_axes = find_vector_axes(vec_axis_list,lhs_arrsection);
	 for (i=0; i <vec_axes; i++) {
	    axis = vec_axis_list[i];
	    if (DEP_DIRECTION(&rhs_dep,axis) == DIR_POSITIVE ||
		DEP_DIRECTION(&rhs_dep,axis) == DIR_NEGATIVE) {
	       SET_DIRECTION(adata,i,DEP_DIRECTION(&rhs_dep,axis));
	    }
	 }
	 FmtAssert((vec_axes == ndim),("found wrong number of vector axes"));
	 break;
      }
   }
   
   /* 
    * Copy the information from the ARRAYEXP node. We have to do this after 
    * the dependence walk, because it's just possible that the sizes in the
    * ARRAYEXP node might be modified by the assignment.
    */
   for (i=0; i < ndim; i++) {
      if (ITER_COUNT(adata,i)) {
	 /* Already exists, so the arrayexp data is redundant */
	 WN_DELETE_Tree(WN_kid(lhs_arrayexp,i+1));
      } else {
	 count = WN_kid(lhs_arrayexp,i+1);
	 if (arrayexp_in_subtree(count)) {
	    count = F90_Lower_Copy_To_STemp(&copy_store,count);
	    WN_INSERT_BlockBefore(block,stmt,copy_store);
	 }
	 SET_ITER_COUNT(adata,i,count);
      }
   }

   if (is_call) {
      WN_kid0(WN_kid(assignment,lhs_arrayexp_kid)) = WN_kid0(lhs_arrayexp);
   } else {
      WN_kid1(assignment) = WN_kid0(lhs_arrayexp);
   }
   WN_Delete(lhs_arrayexp);
   
   return;
}


/*************************************************************************
* This is the main driver routine for the dependence analysis necessary to 
* lower array expressions. It also does analysis for scalar MSTOREs.
*
************************************************************************/

static BOOL F90_Do_Dependence_Analysis(WN *stmt, WN *block)
{
   OPCODE op;
   OPERATOR opr;
   BOOL is_arrayexp;
   WN *arrayexp;
   INT i,num_kids;

   /* First, check for any array expressions anywhere in the subtree, set the global variable
      array_statement_seen, and set the local variable is_arrayexp */

   is_arrayexp = arrayexp_in_subtree(stmt);
   array_statement_seen = array_statement_seen || is_arrayexp;

   op = WN_opcode(stmt);
   opr = OPCODE_operator(op);
   
   /* Check for scalar MSTORE */
   if (opr == OPR_MSTORE && 
       WN_operator(WN_kid1(stmt)) != OPR_ARRAYEXP) {
      f90_analyze_mstore(stmt,block);
      return(TRUE); /* Don't do any further analysis */
   }
   
   /*
    * Determine where the arrayexp statement is. The expected trees are
    * 1) ISTORE(ARRAYEXP, result)
    * 2) MSTORE(ARRAYEXP, result)
    * 3) A WHERE statement
    * 4) An INTRINSIC_CALL of CASSIGNSTMT, CONCATEXPR, ADJUSTL or ADJUSTR
    * 5) An INTRINSIC_CALL of one of the IEEE intrinsics
    * 6) An elemental subroutine or function
    */
   
   if (opr == OPR_WHERE) {
      arrayexp = get_assignment_from_stmt(stmt);
      if (WN_operator(arrayexp) == OPR_INTRINSIC_CALL) {
	 f90_analyze_assignment(stmt, block, TRUE);
      } else if (WN_operator(arrayexp) == OPR_CALL || 
                 WN_operator(arrayexp) == OPR_ICALL) {
         if (WN_operator(arrayexp) == OPR_ICALL) {
            num_kids = WN_kid_count(stmt) - 1;
         } else {
            num_kids = WN_kid_count(stmt);
         }
         for (i=0 ; i < num_kids; i++) {
            arrayexp = WN_kid0(WN_kid(stmt,i));
            if (WN_operator(arrayexp) == OPR_ARRAYEXP) {
               f90_analyze_assignment(stmt,block,TRUE);
               break;
            }
         }
      } else {
	 f90_analyze_assignment(stmt, block, FALSE);
      }
   } else if (opr == OPR_MSTORE || opr == OPR_ISTORE) {
      /* Expect an arrayexp here for case 1 or 2 */
      arrayexp = WN_kid1(stmt);
      if (WN_operator(arrayexp) == OPR_ARRAYEXP) {
	 /* Do the analysis for simple assignments */
	 f90_analyze_assignment(stmt, block, FALSE);
      }
   } else if (opr == OPR_INTRINSIC_CALL) {
      if (WN_kid_count(stmt) > 0) {
	 arrayexp = WN_kid0(WN_kid0(stmt));
	 if (WN_operator(arrayexp) == OPR_ARRAYEXP) {
	    f90_analyze_assignment(stmt,block,TRUE);
	 }
      }
   } else if (opr == OPR_CALL || opr == OPR_ICALL) {
      if (opr == OPR_ICALL) {
	 /* Don't check the last argument (the address of the thing to call) */
	 num_kids = WN_kid_count(stmt) - 1;
      } else {
	 num_kids = WN_kid_count(stmt);
      }
      for (i=0 ; i < num_kids; i++) {
	 arrayexp = WN_kid0(WN_kid(stmt,i));
	 if (WN_operator(arrayexp) == OPR_ARRAYEXP) {
	    f90_analyze_assignment(stmt,block,TRUE);
	    break;
	 }
      }
   }
   
   return (TRUE);
}


static void F90_Analyze_Dependencies (WN *pu) 
{
   F90_Walk_Statements(pu,F90_Do_Dependence_Analysis);
}

/*================================================================
 * 
 * Remove any reduction expressions from underneath TRIPLET and 
 * ARRAYEXP nodes, to make everyone else's life a little easier.
 *
 * expr - expression to analyze
 * stmt, block - insertion point
 * move - if 1, move any transformational intrinsics into scalar temps
 *        if 2, move any intrinsics into scalar temps
 *
 * Returns - a scalarized tree
 *================================================================
 */

static WN * F90_Triplet_Scalarization_Walk(WN *expr, WN *stmt, WN *block, INT move)
{
   OPERATOR opr;
   INT i,numkids;
   WN *copy_store;
   WN *kid;
   
   opr = WN_operator(expr);
   numkids = WN_kid_count(expr);
   
   if (opr == OPR_ARRAYEXP) {
      WN_kid0(expr) = F90_Triplet_Scalarization_Walk(WN_kid0(expr),stmt,block,FALSE);
      for (i=1; i<numkids; i++) {
	 WN_kid(expr,i) = F90_Triplet_Scalarization_Walk(WN_kid(expr,i),stmt,block,TRUE);
      }
   } else if (opr == OPR_TRIPLET) {
      WN_kid0(expr) = F90_Triplet_Scalarization_Walk(WN_kid0(expr),stmt,block,1);
      WN_kid1(expr) = F90_Triplet_Scalarization_Walk(WN_kid1(expr),stmt,block,1);
      /* Don't need to scalarize EXTENT child */
      WN_kid2(expr) = F90_Triplet_Scalarization_Walk(WN_kid2(expr),stmt,block,0);
   } else if (opr == OPR_DO_LOOP) {
      /* Need to move the lower bound to a temp under some circumstances */
      kid = WN_kid1(expr);
      WN_kid0(kid) = F90_Triplet_Scalarization_Walk(WN_kid0(kid),stmt,block,2);
      for (i = 2; i < numkids; i++) {
	 WN_kid(expr,i) = F90_Triplet_Scalarization_Walk(WN_kid(expr,i),stmt,block,move);
      }
   } else if ((move > 0) && opr == OPR_INTRINSIC_OP &&
	      (F90_Is_Transformational(WN_GET_INTRINSIC(expr)) || move == 2)) {
      /* Need to move this to a scalar temp */
      expr = F90_Lower_Copy_To_STemp(&copy_store,expr);
      WN_INSERT_BlockBefore(block,stmt,copy_store);
   } else {
      /* General case, walk children */
      for (i=0; i<numkids; i++) {
	 WN_kid(expr,i) = F90_Triplet_Scalarization_Walk(WN_kid(expr,i),stmt,block,move);
      }
   }
   return (expr);
}


static BOOL F90_Scalarize_Triplets_And_Sizes(WN *stmt, WN *block) 
{
   (void) F90_Triplet_Scalarization_Walk(stmt,stmt,block,0);
   return (TRUE);
}


/*
 * For each statement, see if we need to do the copy right or copy left 
 */

static BOOL F90_Do_Copies(WN *stmt, WN *block)
{
   F90_LOWER_AUX_DATA *adata,*copy_adata;
   INT ndim,i;
   COPY_FLAG_T copy;
   WN *assignment;
   WN *new_rhs,*new_lhsptr;
   WN *copy_store;
   WN *rhs;
   TY_IDX ty;
   OPERATOR assign_opr;
   BOOL  in_where,in_elsewhere;
   WN *sizes[MAX_NDIM];
   WN *new_where;

   adata = GET_F90_MAP(stmt);
   if (!adata) return (TRUE);
   copy = COPY_FLAG(adata);
   if (copy == COPY_NONE) return (TRUE);

   if (WN_opcode(stmt) == OPC_WHERE) {
      in_where = TRUE;
      in_elsewhere = FALSE;
      assignment = WN_first(WN_kid1(stmt));
      if (!assignment) {
	 assignment = WN_first(WN_kid2(stmt));
	 in_elsewhere = TRUE;
      }
   } else {
      in_where = FALSE;
      assignment = stmt;
   }

   assignment = get_assignment_from_stmt(stmt);
   assign_opr = WN_operator(assignment);

   /* Always do the copy_right first */
   if (copy == COPY_RIGHT || copy == COPY_BOTH) {
      if (assign_opr == OPR_ISTORE || assign_opr == OPR_MSTORE) {
	 if (in_where) {
	    /* Need to put the copy under another WHERE */
	    copy_adata = F90_Lower_Copy_Aux_Data(adata);
	    ndim = NDIM(copy_adata);
	    /* Fill in the iteration counts */
	    for (i=0; i <ndim ; i++) {
	       sizes[i] = ITER_COUNT(copy_adata,i);
	    }
	    new_rhs = F90_Lower_Copy_To_ATemp(&ALLOC_PRELIST(copy_adata),&DEALLOC_POSTLIST(adata),
					      &copy_store,WN_kid0(assignment),sizes,ndim);
	    WN_INSERT_BlockFirst(PRELIST(copy_adata),PRELIST(adata));
	    WN_INSERT_BlockFirst(ALLOC_PRELIST(copy_adata),ALLOC_PRELIST(adata));
	    SET_PRELIST(adata,WN_CreateBlock());
	    SET_ALLOC_PRELIST(adata,WN_CreateBlock());
	    
	    new_where = WN_Create(OPC_WHERE,3);
	    WN_kid0(new_where) = WN_COPY_Tree(WN_kid0(stmt));
	    WN_kid1(new_where) = WN_CreateBlock();
	    WN_kid2(new_where) = WN_CreateBlock();

	    if (in_elsewhere) {
	       WN_INSERT_BlockFirst(WN_kid2(new_where),copy_store);
	    } else {
	       WN_INSERT_BlockFirst(WN_kid1(new_where),copy_store);
	    }
	    SET_F90_MAP(new_where,copy_adata);
	    WN_INSERT_BlockBefore(block,stmt,new_where);
	 } else {
	    new_rhs = F90_Lower_Copy_Expr_to_Temp(WN_kid0(assignment),stmt,block);
	 }
	 WN_kid0(assignment) = new_rhs;
      } else {
	 /* It's a CASSIGNSTMT or CALL statement, create an MLOAD of the RHS (kid1), and move
	  * it to a temp.
	  */
	 TY_IDX ety;
	 WN *wn_size;
	 copy_adata = F90_Lower_Copy_Aux_Data(adata);
	 rhs = WN_kid0(WN_kid1(assignment));
	 if (assign_opr == OPR_CALL)
	 {
	    ety = TY_AR_etype(ST_type(WN_st(WN_kid0(WN_kid0(rhs)))));
	    wn_size = WN_CreateIntconst(OPCint, TY_size(ety));
	    ty = Make_Pointer_Type(ety);
	 } else {
	    ty = WN_ty(WN_kid0(assignment));
	    wn_size = WN_COPY_Tree(WN_kid0(WN_kid(assignment,3)));
	 }
	 rhs = WN_CreateMload(0,ty,rhs, wn_size); 
	 new_rhs = F90_Lower_Copy_To_ATemp(&ALLOC_PRELIST(copy_adata), &DEALLOC_POSTLIST(adata),
					   &copy_store,rhs,
					   ITER_COUNT_PTR(adata), NDIM(adata));
	 /* Get rid of the MLOAD and its length argument */
	 WN_kid0(WN_kid1(assignment)) = WN_kid0(new_rhs);
	 WN_DELETE_Tree(WN_kid1(new_rhs));
	 WN_Delete(new_rhs);
	 SET_F90_MAP(copy_store,copy_adata);
	 WN_INSERT_BlockBefore(block,stmt,copy_store);
         // bug 8687: the prelist of adata should be inserted before copy_store now
#ifdef KEY // because rhs may use them         
         WN_INSERT_BlockFirst(PRELIST(copy_adata),PRELIST(adata));
         SET_PRELIST(adata,WN_CreateBlock());
#endif
	 WN_INSERT_BlockFirst(ALLOC_PRELIST(copy_adata),ALLOC_PRELIST(adata));
	 SET_ALLOC_PRELIST(adata,WN_CreateBlock());
      }
   }
   
   if (copy == COPY_LEFT || copy == COPY_BOTH) {
      /* Copy the address expression underneath the ISTORE to a temp */
      if (assign_opr == OPR_ISTORE || assign_opr == OPR_MSTORE) {
	 new_lhsptr = F90_Lower_Copy_Expr_to_Temp(WN_kid1(assignment),stmt,block);
	 WN_kid1(assignment) = new_lhsptr;
      } else {
	 /* It's a CASSIGNMENT statement. Copy the address of the first argument into 
	  * a temp.
	  */
	 new_lhsptr = F90_Lower_Copy_Expr_to_Temp(WN_kid0(WN_kid0(assignment)),stmt,block);
	 WN_kid0(WN_kid0(assignment)) = new_lhsptr;
	 
      }
   }
   return (TRUE);
}



//================================================================
// Helper routine for Move_Transformational_Walk
// Returns TRUE if it makes sense to move a kid for MATMUL or SPREAD
//
static BOOL move_kid_for_mm_or_spread(WN * expr)
{
  OPERATOR opr;
  opr = WN_operator(expr);

  if (opr == OPR_PARM || opr == OPR_ARRAYEXP) {
    return move_kid_for_mm_or_spread(WN_kid0(expr));
  } else if (opr == OPR_ILOAD ||
      opr == OPR_MLOAD ||
      opr == OPR_LDID ||
      opr == OPR_TRIPLET ||
      opr == OPR_ARRAY ||
      opr == OPR_ARRSECTION) {
    return (FALSE);
  } else if (opr == OPR_INTRINSIC_OP) {
    INTRINSIC i = WN_GET_INTRINSIC(expr);
    // these two won't cause any increase in number of operations
    if (i == INTRN_TRANSPOSE || i == INTRN_SPREAD) {
      return (FALSE);
    } else {
      return (TRUE);
    }
  } else {
    return (TRUE);
  }
}

/*================================================================
 * Walker to move transformationals
 *
 * t - node to check
 * stmt - insertion point statement
 * block - insertion point block
 * parent - parent of the current statement, used to optimize A = FUNC(...)
 * in_where - TRUE if under a WHERE statement
 *================================================================
 */

static WN * F90_Move_Transformational_Walk(WN *t, WN *stmt, WN *block, WN *parent, BOOL in_where)
{
   INT numkids;
   WN *kid;
   WN *temp;
   INT i;
   INT rank,rank1,rank2;
   OPERATOR opr;
   BOOL walking_where;
   F90_LOWER_AUX_DATA *adata;


   /* Walk the children first, then do the current node */
   numkids = WN_kid_count(t);
   opr = WN_operator(t);

   if (opr == OPR_BLOCK) {
      kid = WN_first(t);
      while (kid) {
	(void) F90_Move_Transformational_Walk(kid, stmt, block,t,in_where);
	kid = WN_next(kid);
      }
   } else {
     for (i=0; i < numkids; i++) {
       kid = WN_kid(t,i);
       walking_where = (opr == OPR_WHERE) || in_where;
       temp = F90_Move_Transformational_Walk(kid,stmt,block,t,walking_where);
       WN_kid(t,i) = temp;
     }
   }
   
   if (opr != OPR_INTRINSIC_OP) {
      return (t);
   }
   
   /* Analyze the current intrinsic */
   switch (WN_GET_INTRINSIC(t)) {
    case INTRN_MATMUL:
      rank = F90_Rank_Walk(t);
      rank1 = F90_Rank_Walk(WN_kid0(t));
      rank2 = F90_Rank_Walk(WN_kid1(t));
      if ((rank==2 || rank1==1) && move_kid_for_mm_or_spread(WN_kid0(t))) {
	kid = WN_kid0(WN_kid0(t)); /* Get through the PARM node */
	temp = F90_Lower_Copy_Expr_to_Temp(kid,stmt,block);
	WN_kid0(WN_kid0(t)) = temp;
      }
      if ((rank==2 || rank2==1) && move_kid_for_mm_or_spread(WN_kid1(t))) {
	kid = WN_kid0(WN_kid1(t)); /* Get through the PARM node */
	temp = F90_Lower_Copy_Expr_to_Temp(kid,stmt,block);
	WN_kid0(WN_kid1(t)) = temp;
      }
      return (t);

    case INTRN_SPREAD:
      if (move_kid_for_mm_or_spread(WN_kid0(t))) {
	kid = WN_kid0(WN_kid0(t)); /* Get through the PARM node */
	temp = F90_Lower_Copy_Expr_to_Temp(kid,stmt,block);
	WN_kid0(WN_kid0(t)) = temp;
      }
      return (t);
      
    case INTRN_PACK:
    case INTRN_UNPACK:
    case INTRN_EOSHIFT:
      if (!OPCODE_is_stmt(WN_opcode(parent)) || in_where) {
	temp = F90_Lower_Copy_Expr_to_Temp(t,stmt,block);
	return(temp);
      } else {
	/* Don't move if of form LHS = FUNCTION(...) */
	return (t);
      }

    default:
      return (t);
   }
}


/*================================================================
 *
 * F90_Move_Transformationals moves the transformationals 
 * SPREAD (sometimes), PACK, UNPACK, EOSHIFT
 *
 *================================================================
*/
static BOOL F90_Move_Transformationals(WN *stmt, WN *block)
{
   /* Walk, looking for transformational intrinsics that need to be moved */
   (void) F90_Move_Transformational_Walk(stmt,stmt,block,NULL,FALSE);
   return (TRUE);
}

/***********************************************
* 
* Helper routine, creates a DO loop node
*/
static WN * create_doloop_node(WN *index_id, WN *start, WN *end,
			       WN *step, WN *body)
{
  WN *temp;
  WN_Set_Linenum(start,current_srcpos);
  WN_Set_Linenum(step,current_srcpos);
  temp = WN_CreateDO(index_id, start, end, step, body, NULL);
  WN_Set_Linenum(temp,current_srcpos);

  return (temp);
}


/***********************************************
* 
* Helper routine, creates a DO loop running from 0 to count-1
* with loop control variable index, in direction dir, with body.
*
* index(output) - the preg_num for the created loop index
* index_name(input) - a name for the index variable
* count - number of iterations to execute the loop
* dir - direction. If DIR_POSITIVE or DONTCARE, go from 0 to count-1
*                  otherwise, go from count-1 to 0.
* body - the loop body
****************************************************************/

static WN * create_doloop(PREG_NUM *index, char *index_name, WN *count, DIR_FLAG dir, WN *body)
{
   WN *index_id, *count_expr, *start, *end, *step, *doloop;
   OPCODE intconst_op;
   TYPE_ID index_type;
   WN *temp;

   index_type = doloop_ty;
   intconst_op = OPCODE_make_op(OPR_INTCONST,index_type,MTYPE_V);
#ifdef KEY
   if (MTYPE_size_min(index_type) != MTYPE_size_min(WN_rtype(count)))
     count = WN_Cvt(WN_rtype(count), index_type, count);
#endif
   
   /* Create an index */
   *index = Create_Preg(index_type,Index_To_Str(Save_Str(index_name)));

   index_id = WN_CreateIdname(*index,MTYPE_To_PREG(index_type));
   count_expr = WN_CreateExp2(OPCODE_make_op(OPR_SUB,index_type,MTYPE_V),
			      count,
			      WN_CreateIntconst(intconst_op,1));
   
   if (dir == DIR_NEGATIVE) {
      /* Loop runs downward */
      start = WN_StidPreg(index_type,*index,count_expr);
      end = WN_CreateExp2(OPCODE_make_op(OPR_GE,MTYPE_I4,index_type),
			  WN_LdidPreg(index_type,*index),
			  WN_CreateIntconst(intconst_op,0));
      
      step = WN_CreateExp2(OPCODE_make_op(OPR_SUB,index_type,MTYPE_V),
			   WN_LdidPreg(index_type,*index),
			   WN_CreateIntconst(intconst_op,1));
      step = WN_StidPreg(index_type,*index,step);
   } else {
      /* Loop runs upward */
      start = WN_StidPreg(index_type,*index, WN_CreateIntconst(intconst_op,0));
      end = WN_CreateExp2(OPCODE_make_op(OPR_LE,MTYPE_I4,index_type),
			  WN_LdidPreg(index_type,*index),
			  count_expr);
      
      step = WN_CreateExp2(OPCODE_make_op(OPR_ADD,index_type,MTYPE_V),
			   WN_LdidPreg(index_type,*index),
			   WN_CreateIntconst(intconst_op,1));
      step = WN_StidPreg(index_type,*index,step);
   }
   
   /* Make sure body is a BLOCK node */
   if (WN_opcode(body) != OPC_BLOCK) {
      temp = WN_CreateBlock();
      WN_INSERT_BlockFirst(temp,body);
   } else {
      temp = body;
   }

   doloop = create_doloop_node(index_id, start, end, step, temp);
   return (doloop);
}

/*
 *  Helper routine to create a loop nest
 * indices - (output) list of indices 
 * doloop - (output) the crated doloop nest
 * sizes - (input) iteration counts
 * ndim - (input) number of loops
 *
 * returns pointer to the block node in the loopnest
 */
 
static WN * create_doloop_nest(PREG_NUM indices[], WN **doloop, WN *sizes[], INT ndim)
{
   INT i;
   WN *loopnest, *stlist;
   char tempname[32];
   PREG_NUM index;

   loopnest = WN_CreateBlock();
   stlist = loopnest;
   num_temps += 1;
   for (i=ndim-1; i >= 0; i--) {
      sprintf(tempname,"@f90li_%d_%d",i,num_temps);
      
      /* Create the DO loop node */
      loopnest = create_doloop(&index,tempname,sizes[i],DIR_POSITIVE,loopnest);
      indices[i] = index;
   }
   *doloop = loopnest;
   return (stlist);
}

/*================================================================
 * 
 * Routines to lower reductions.
 *
 * static WN * lower_reduction(TYPE_ID ty, OPERATOR reduction_opr,
 *			    WN *kids[], PREG_NUM *indices, INT ndim,
 * 		            WN * block, WN *insert_point)
 *
 * ty - type of the operation
 * reduction_opr - the operator used for the reductions
 * kids[] - 0 - ARRAY argument
 *          1 - DIM argument
 *          2 - MASK argument
 * indices - current DO loop indices
 * ndim - current DO loop nest depth
 * block - block of interior of current loopnest
 * insert_point - current place to insert things in the loopnest
 *
 * Returns - an LDID of the reduction temp.
 * Modifies - F90_Current_Loopnest for global reductions. 
 *
 *================================================================
 */

static WN * lower_reduction(TYPE_ID rty, OPERATOR reduction_opr,
			    WN *kids[], PREG_NUM *indices, INT ndim,
			    WN * block, WN *insert_point)
{
   INT dim;
   OPCODE reduction_op;
   PREG_NUM accum;
   ST * accum_st;
   WN *sizes[MAX_NDIM];
   PREG_NUM new_indices[MAX_NDIM],index;
   INT rank,i,j;
   WN *idty_store;
   WN *accum_store,*accum_block;
   WN *loopnest,*stlist,*mask_expr,*accum_expr;
   char tempname[32];
   WN *result;
   BOOL Mask_Present;
   F90_LOWER_AUX_DATA *adata;
   WN *dealloc_post,*post;
   TYPE_ID ty;
   WN* region;

#ifdef KEY // bug14194
   for (i=0; i<MAX_NDIM; i++)
     sizes[i] = NULL;
#endif

   if (kids[1]) {
      dim = F90_Get_Dim(kids[1]);
      WN_DELETE_Tree(kids[1]);
   } else {
      dim = 0;
   }
   
   if (!kids[2] || (WN_operator(kids[2]) == OPR_INTCONST && 
		    WN_const_val(kids[2]) == 1)) {
      Mask_Present = FALSE;
   } else {
      Mask_Present = TRUE;
   }
   
   ty = Mtype_comparison(rty);

   /* All cases, create a temp and insert an identity store */
   accum_st = New_ST(CURRENT_SYMTAB);
   ST_Init(accum_st, Save_Str(create_tempname("@f90acc")),
                   CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, MTYPE_To_TY(ty));
   Add_Pragma_To_MP_Regions (&F90_MP_Region,WN_PRAGMA_LOCAL,
			     accum_st,0,WN_MAP_UNDEFINED,FALSE);
#ifdef DEBUG
   printf("%s:%d -- accum_st = 0x%x, ST_type(accum_st) = %d\n",
		   __FILE__, __LINE__, (unsigned)accum_st, (int)ST_type(accum_st) );
#endif
//   Set_ST_is_temp_var(accum_st);
   reduction_op = OPCODE_make_op(reduction_opr,ty,MTYPE_V);
   idty_store = WN_Stid(ty,0,accum_st,MTYPE_To_TY(ty),
		   Make_Reduction_Identity(reduction_opr,rty));

   /* Determine the sizes of reduction argument */
   (void) F90_Size_Walk(kids[0],&rank,sizes);
   loopnest = WN_CreateBlock();
   stlist = loopnest;

   /* Case 1, DIM not present */
   if (dim == 0 || (dim == 1 && rank == 1)) {
      WN_INSERT_BlockBefore(F90_Current_Block,F90_Current_Loopnest,idty_store);
      adata = GET_F90_MAP(F90_Current_Loopnest);
      if (adata) {
	 dealloc_post = DEALLOC_POSTLIST(adata);
	 post = POSTLIST(adata);
	 SET_DEALLOC_POSTLIST(adata,WN_CreateBlock());
	 SET_POSTLIST(adata,WN_CreateBlock());
      }
      
      /* Create a full-size loopnest */
      num_temps += 1;
      for (i=rank-1; i >= 0 ; i--) {
	 sprintf(tempname,"@f90li_%d_%d",i,num_temps);
	 loopnest = create_doloop(&index,tempname,sizes[i],DIR_POSITIVE,loopnest);
	 new_indices[i] = index;
      }
      BOOL_VECTOR::iterator bi = F90_MP_Region_Isworkshare.begin();
      if( (bi != F90_MP_Region_Isworkshare.end()) && *bi ){
	/* if locate in a WORKSHARE region */
	region = WN_CreateRegion(REGION_KIND_MP,
			WN_CreateBlock(),
			WN_CreateBlock(),
			WN_CreateBlock(),
			-1,
			0);
	WN_INSERT_BlockBefore(F90_Current_Block, F90_Current_Loopnest, region);
	WN_INSERT_BlockFirst(WN_region_body(region), loopnest);
	F90_Current_Loopnest = region;

	WN* pragmas = WN_region_pragmas(region);

	WN* pragma = WN_CreatePragma(WN_PRAGMA_PDO_BEGIN, (ST *)NULL, 0, 1);
	WN_set_pragma_omp(pragma);
	WN_set_pragma_compiler_generated(pragma);
	WN_INSERT_BlockFirst(pragmas, pragma);

	pragma = WN_CreatePragma(WN_PRAGMA_REDUCTION, accum_st, 0, reduction_opr);
	WN_set_pragma_omp(pragma);
	WN_set_pragma_compiler_generated(pragma);
	WN_INSERT_BlockLast(pragmas, pragma);

	ST *index_st = MTYPE_To_PREG(doloop_ty);

	for(i=0; i<rank; i++){
	  pragma = WN_CreatePragma(WN_PRAGMA_LOCAL, index_st, new_indices[i], 0);
	  WN_set_pragma_omp(pragma);
	  WN_set_pragma_compiler_generated(pragma);
	  WN_INSERT_BlockLast(pragmas, pragma);
	}

	pragma = WN_CreatePragma(WN_PRAGMA_END_MARKER, (ST *)NULL, 0, 0);
	WN_set_pragma_omp(pragma);
	WN_set_pragma_compiler_generated(pragma);
	WN_INSERT_BlockLast(pragmas, pragma);
      }
      else{
        WN_INSERT_BlockBefore(F90_Current_Block,F90_Current_Loopnest,loopnest);
        /* Update the Current_Loopnest, to point at the identity store
	   so that all lower-level insertions happen there */
        F90_Current_Loopnest = loopnest;
      }
      
      /* put the DEALLOC_POSTLIST and POSTLIST on the current loopnest */
      if (adata) {
	 adata = F90_Lower_New_Aux_Data(0);
	 SET_POSTLIST(adata,post);
	 SET_DEALLOC_POSTLIST(adata,dealloc_post);
	 SET_F90_MAP(loopnest,adata);
      }

      /* Lower the mask and array argument */
      accum_expr = F90_Lower_Walk(kids[0],new_indices,rank,stlist,NULL);
      accum_store = WN_Stid(ty,0,accum_st,MTYPE_To_TY(ty),WN_CreateExp2(reduction_op,
			                 WN_Ldid(ty, 0, accum_st, MTYPE_To_TY(ty)),
					 accum_expr));
      accum_block = WN_CreateBlock();
      WN_INSERT_BlockFirst(accum_block,accum_store);
      if (Mask_Present) {
	 mask_expr = F90_Lower_Walk(kids[2],new_indices,rank,stlist,NULL);
	 /* Insert if (mask), etc. */
	 accum_block = WN_CreateIf(mask_expr,accum_block,WN_CreateBlock());
      }
      /* Stick into the loopnest */
      WN_INSERT_BlockLast(stlist,accum_block);
   
      /*****************************************************************/
      /*****************************************************************/
   } else {
      /* DIM is present, similar to above, but insert the single loop in the current
	 loop nest */
      WN_INSERT_BlockBefore(block,insert_point,idty_store);
      
      /* Create a single loopnest */
      num_temps += 1;
#ifdef KEY // bug14194
      dim = rank + 1 - dim; /* account for ordering */
#else
      dim = ndim + 2 - dim; /* account for ordering */
#endif /* KEY */
      sprintf(tempname,"@f90red_%d",num_temps);
      loopnest = create_doloop(&index,tempname,sizes[dim-1],DIR_POSITIVE,loopnest);
      for (i=0,j=0; i < ndim+1; i++) {
	 if (i != dim-1) {
	    WN_DELETE_Tree(sizes[i]);
	    new_indices[i] = indices[j];
	    j += 1;
	 } else {
	    new_indices[i] = index;
	 }
      }

      WN_INSERT_BlockBefore(block,insert_point,loopnest);
      /* Lower the mask and array argument */
      accum_expr = F90_Lower_Walk(kids[0],new_indices,ndim+1,stlist,NULL);
      accum_store = WN_Stid(ty,0,accum_st,MTYPE_To_TY(ty),WN_CreateExp2(reduction_op,
			                   WN_Ldid(ty, 0, accum_st, MTYPE_To_TY(ty)),
					   accum_expr));
      accum_block = WN_CreateBlock();
      WN_INSERT_BlockFirst(accum_block,accum_store);
      if (Mask_Present) {
	 mask_expr = F90_Lower_Walk(kids[2],new_indices,rank,stlist,NULL);
	 /* Insert if (mask), etc. */
	 accum_block = WN_CreateIf(mask_expr,accum_block,WN_CreateBlock());
      }
      /* Stick into the loopnest */
      WN_INSERT_BlockLast(stlist,accum_block);
   }
   
   /* Return the load of the result variable */
   result = WN_Ldid(ty, 0, accum_st, MTYPE_To_TY(ty));
   if (rty == MTYPE_I1) {
      result = WN_CreateCvtl(OPC_I4CVTL,8,result);
   } else if (rty == MTYPE_I2) {
      result = WN_CreateCvtl(OPC_I4CVTL,16,result);
   }

   return (result);
}

/*================================================================
 * 
 * static WN * lower_maxminloc(OPERATOR reduction_opr,
 *			    WN *kids[], PREG_NUM *indices, INT ndim,
 * 		            WN * block, WN *insert_point)
 *
 * reduction_opr - the operator used for the reductions, either MAX or MIN
 * kids[] - 0 - ARRAY argument
 *          1 - DIM argument
 *          2 - MASK argument
 * indices - current DO loop indices
 * ndim - current DO loop nest depth
 * block - block of interior of current loopnest
 * insert_point - current place to insert things in the loopnest
 *
 * Returns - an ILOAD of the array section of the TEMP
 *
 *================================================================
 */

static WN * lower_maxminloc(OPERATOR reduction_opr,
			    WN *kids[], PREG_NUM *indices, INT ndim,
			    WN * block, WN *insert_point)
{
   OPCODE reduction_op;
   PREG_NUM accum;
   WN *sizes[MAX_NDIM];
   PREG_NUM new_indices[MAX_NDIM],index,red_index;
   INT rank,i,j,dim;
   WN *idty_store;
   WN *accum_store,*accum_block;
   WN *loopnest,*stlist,*mask_expr,*accum_expr,*comp_expr,*red_store;
   char tempname[32];
   WN *result;
   BOOL Mask_Present;
   PREG_NUM cur_val;
   ST * retval;
   TY_IDX  retty, ret_elem_ptr;
   TYPE_ID expr_ty;

#ifdef KEY // bug14194
   for (i=0; i<MAX_NDIM; i++)
     sizes[i] = NULL;
#endif

   if (kids[1]) {
      dim = F90_Get_Dim(kids[1]);
      WN_DELETE_Tree(kids[1]);
   } else {
      dim = 0;
   }

   if (!kids[2] || (WN_operator(kids[2]) == OPR_INTCONST && 
		    WN_const_val(kids[2]) == 1)) {
      Mask_Present = FALSE;
   } else {
      Mask_Present = TRUE;
   }

   /* All cases, create a temp and insert an identity store */
   expr_ty = WN_rtype(kids[0]);
   accum = Create_Preg(expr_ty,create_tempname("@f90acc"));
   cur_val = Create_Preg(expr_ty,create_tempname("@f90accval"));
   if (reduction_opr == OPR_MAX) {
#ifdef KEY /* Bug 3395 */
      reduction_op = OPCODE_make_op(OPR_GE,MTYPE_I4,expr_ty);
#else
      reduction_op = OPCODE_make_op(OPR_GT,MTYPE_I4,expr_ty);
#endif /* KEY */
   } else {
#ifdef KEY /* Bug 3395 */
      reduction_op = OPCODE_make_op(OPR_LE,MTYPE_I4,expr_ty);
#else
      reduction_op = OPCODE_make_op(OPR_LT,MTYPE_I4,expr_ty);
#endif /* KEY */
   }
   idty_store = WN_StidPreg(expr_ty,accum,Make_Reduction_Identity(reduction_opr,expr_ty));

   if (dim == 0) {
      /* Global reduction */
      
      /* Determine the sizes of reduction argument */
      (void) F90_Size_Walk(kids[0],&rank,sizes);
      loopnest = WN_CreateBlock();
      stlist = loopnest;
      
      WN_INSERT_BlockBefore(F90_Current_Block,F90_Current_Loopnest,idty_store);
      
      /* Create a full-size loopnest */
      num_temps += 1;
      for (i=rank-1; i >=0 ; i--) {
	 sprintf(tempname,"@f90li_%d_%d",i,num_temps);
#ifdef KEY /* Bug 3395 */
	 loopnest = create_doloop(&index,tempname,sizes[i],DIR_NEGATIVE,loopnest);
#else
	 loopnest = create_doloop(&index,tempname,sizes[i],DIR_POSITIVE,loopnest);
#endif /* KEY */
	 new_indices[i] = index;
      }
      WN_INSERT_BlockBefore(F90_Current_Block,F90_Current_Loopnest,loopnest);
      /* Update the Current_Loopnest, to point at the identity store
	 so that all lower-level insertions happen there */
      F90_Current_Loopnest = idty_store;
      
      /* Create a temp to hold the result. We will allocate this temp on the stack, 
       * but not dynamically 
       */
      retval = new_temp_st("@f90mmloc");
      retty = Make_Array_Type(doloop_ty,1,rank);
      ret_elem_ptr = Make_Pointer_Type(Be_Type_Tbl(doloop_ty));
      Set_ST_type(*retval,retty);
      
      /* Lower the mask and array argument */
      accum_expr = F90_Lower_Walk(kids[0],new_indices,rank,stlist,NULL);
      accum_store = WN_StidPreg(expr_ty,cur_val,accum_expr);
// Bug #411
#ifdef KEY
      WN_INSERT_BlockLast(stlist,accum_store);
#else
      WN_INSERT_BlockFirst(stlist,accum_store);
#endif
      
      comp_expr = WN_CreateExp2(reduction_op,WN_LdidPreg(expr_ty,cur_val),
				WN_LdidPreg(expr_ty,accum));
      if (Mask_Present) {
	 mask_expr = F90_Lower_Walk(kids[2],new_indices,rank,stlist,NULL);
	 mask_expr = WN_LAND(mask_expr,comp_expr);
      } else {
	 mask_expr = comp_expr;
      }
      accum_block = WN_CreateBlock();
      accum_store = WN_StidPreg(expr_ty,accum,WN_LdidPreg(expr_ty,cur_val));
      WN_INSERT_BlockLast(accum_block,accum_store);

      for (i = 0; i < rank; i++) {
	 /* Create an ARRAY node for the return value */
      
	 result = WN_Create(OPCODE_make_op(OPR_ARRAY,Pointer_Mtype,MTYPE_V),3);
	 WN_element_size(result) = Pointer_Size;
	 WN_kid1(result) = WN_Intconst(MTYPE_I4,rank);
	 WN_kid2(result) = WN_Intconst(doloop_ty,i);
	 WN_kid0(result) = WN_Lda(Pointer_type,0,retval);
	 accum_store = WN_Istore(doloop_ty, (WN_OFFSET) 0, ret_elem_ptr, WN_COPY_Tree(result), 
				 WN_LdidPreg(doloop_ty,new_indices[rank-i-1]));
	 
	 WN_INSERT_BlockLast(accum_block,accum_store);
	 /* Initialize the return value to 0 */
	 accum_store = WN_Istore(doloop_ty, (WN_OFFSET) 0, ret_elem_ptr,result, 
				 WN_Intconst(doloop_ty,-1));
	 WN_INSERT_BlockBefore(F90_Current_Block,idty_store,accum_store);
      }
      accum_store = WN_CreateIf(mask_expr,accum_block,WN_CreateBlock());
      WN_INSERT_BlockLast(stlist,accum_store);
            result = WN_Create(OPCODE_make_op(OPR_ARRAY,Pointer_Mtype,MTYPE_V),3);
      WN_element_size(result) = Pointer_Size;
      WN_kid1(result) = WN_Intconst(MTYPE_I4,rank);
      WN_kid2(result) = WN_LdidPreg(doloop_ty,indices[0]);
      WN_kid0(result) = WN_Lda(Pointer_type,0,retval);
      result = WN_Iload(doloop_ty,0,ret_elem_ptr,result);
      result = WN_CreateExp2(OPCODE_make_op(OPR_ADD,doloop_ty,MTYPE_V),result,
			     WN_Intconst(doloop_ty,1));

      /* The result is the lowering of the created temp with the right index */
   } else {
      /* New F95/HPF maxloc along a dimension */

      /* All cases, create a temp and insert an identity store */
      red_index = Create_Preg(doloop_ty,create_tempname("@f90redindex"));
// Bug 2155
# ifdef KEY
      red_store = WN_StidPreg(doloop_ty,red_index,WN_Intconst(doloop_ty,0));
# else
      red_store = WN_StidPreg(doloop_ty,red_index,WN_Intconst(doloop_ty,-1));
# endif
      
      /* Determine the sizes of reduction argument */
      (void) F90_Size_Walk(kids[0],&rank,sizes);
      loopnest = WN_CreateBlock();
      stlist = loopnest;
      
      WN_INSERT_BlockBefore(block,insert_point,idty_store);
      WN_INSERT_BlockBefore(block,insert_point,red_store);
      
      /* Create a single loopnest */
      num_temps += 1;
#ifdef KEY // bug14194
      dim = rank + 1 - dim; /* account for ordering */
#else
      dim = ndim + 2 - dim; /* account for ordering */
#endif /* KEY */
      sprintf(tempname,"@f90red_%d",num_temps);
#ifdef KEY /* Bug 3395 */
      loopnest = create_doloop(&index,tempname,sizes[dim-1],DIR_NEGATIVE,loopnest);
#else
      loopnest = create_doloop(&index,tempname,sizes[dim-1],DIR_POSITIVE,loopnest);
#endif /* KEY */
      for (i=0,j=0; i < ndim+1; i++) {
	 if (i != dim-1) {
	    WN_DELETE_Tree(sizes[i]);
	    new_indices[i] = indices[j];
	    j += 1;
	 } else {
	    new_indices[i] = index;
	 }
      }

      WN_INSERT_BlockBefore(block,insert_point,loopnest);

      /* Lower the mask and array argument */
      accum_expr = F90_Lower_Walk(kids[0],new_indices,rank,stlist,NULL);
      accum_store = WN_StidPreg(expr_ty,cur_val,accum_expr);
      WN_INSERT_BlockFirst(stlist,accum_store);

      comp_expr = WN_CreateExp2(reduction_op,WN_LdidPreg(expr_ty,cur_val),
				WN_LdidPreg(expr_ty,accum));
      if (Mask_Present) {
	 mask_expr = F90_Lower_Walk(kids[2],new_indices,rank,stlist,NULL);
	 mask_expr = WN_LAND(mask_expr,comp_expr);
      } else {
	 mask_expr = comp_expr;
      }


      /* Save the current value */
      accum_block = WN_CreateBlock();
      accum_store = WN_StidPreg(expr_ty,accum,WN_LdidPreg(expr_ty,cur_val));
      WN_INSERT_BlockLast(accum_block,accum_store);

      /* Save the index value */
      accum_store = WN_CreateExp2(OPCODE_make_op(OPR_ADD,doloop_ty,MTYPE_V),
				  WN_LdidPreg(doloop_ty,index),
				  WN_Intconst(doloop_ty,1));
      accum_store = WN_StidPreg(doloop_ty,red_index,accum_store);
      WN_INSERT_BlockLast(accum_block,accum_store);

      /* stick the IF in the tree */
      accum_store = WN_CreateIf(mask_expr,accum_block,WN_CreateBlock());
      WN_INSERT_BlockLast(stlist,accum_store);

      /* Result is red_index */
      result = WN_LdidPreg(doloop_ty,red_index);
   }
   
   return (result);
}
   
/*================================================================
 * Special routines for PACK, UNPACK, EOSHIFT, MAXLOC and MINLOC
 *================================================================
 */

static WN * lower_pack(WN *kids[],PREG_NUM *indices,INT ndim,WN *block, WN *insert_point)
{
   /* Note:
      This routine makes several assumptions about what is coming in:
      It expects to be called while lowering something of the form
      I/MSTORE
         PACK
	 LHS_ADDRESS

      It also assumes that LHS_ADDRESS has not been lowered. It will return a lowering
      of the VECTOR argument, and also inserts the necessary loops after the current statement
      to do the rest of the pack.
      */
   
   WN *store;
   WN *lhs;
   WN *vector, *mask, *array;
   WN *sizes[MAX_NDIM];
   PREG_NUM new_indices[MAX_NDIM];
   INT ndim_array;
   PREG_NUM pack_index;
   WN *loopnest;
   WN *stlist;
   WN *temp;
   WN *increment;
   
   /* Reach up, and grab the current statement. Strip off the RHS, and copy it */
   store = F90_Current_Stmt;
   temp = WN_kid0(store);
   WN_kid0(store) = WN_Zerocon(MTYPE_I4); /* so we don't copy the RHS */
   lhs = WN_COPY_Tree(store);
   WN_kid0(store) = temp;

   vector = F90_Lower_Walk(kids[2],indices,ndim,block,insert_point);
   
   /* Now, create a loop nest */
   (void) F90_Size_Walk(kids[0],&ndim_array,sizes);
   stlist = create_doloop_nest(new_indices,&loopnest,sizes,ndim_array);

   /* Create a PREG to hold the current count */
   pack_index = Create_Preg(doloop_ty,create_tempname("@f90pack"));
   
   /* Insert initialization expression, and insert DO loop */
   temp = WN_StidPreg(doloop_ty,pack_index,WN_Zerocon(doloop_ty));
   WN_INSERT_BlockAfter(F90_Current_Block,F90_Current_Loopnest,loopnest);
   WN_INSERT_BlockAfter(F90_Current_Block,F90_Current_Loopnest,temp);

   /* Now, fill in the guts of the loop nest */
   mask = F90_Lower_Walk(kids[1],new_indices,ndim_array,stlist,NULL);
   array= F90_Lower_Walk(kids[0],new_indices,ndim_array,stlist,NULL);
   (void) F90_Lower_Walk(lhs, &pack_index,  1, stlist, NULL);
   WN_Delete(WN_kid0(lhs));
   WN_kid0(lhs) = array;
   /* Since LHS was a store, it is not altered by the lower walk */
   /* Insert (if (mask) then lhs = array; pack_index = pack_index + 1;) */
   temp = WN_CreateBlock();
   WN_INSERT_BlockFirst(temp,lhs);
   increment = WN_StidPreg(doloop_ty,pack_index,
			   WN_CreateExp2(OPCadd,WN_LdidPreg(doloop_ty,pack_index),
					 WN_Intconst(doloop_ty,1)));
   WN_INSERT_BlockLast(temp,increment);
   temp = WN_CreateIf(mask,temp,WN_CreateBlock());
   WN_INSERT_BlockFirst(stlist,temp);
   return (vector);
}

static WN * lower_unpack(WN *kids[],PREG_NUM *indices,INT ndim,WN *block, WN *insert_point)
{
   WN *vector, *mask, *field;
   PREG_NUM pack_index,p;
   WN *if_stmt,*then_block,*else_block;
   WN *temp;
   WN *increment;
   TY_IDX ptr_ty,temp_ty;
   WN *lda;
   ST *temp_st;
   TYPE_ID type;
   WN *true_store;
   WN *false_store;
   WN *load;

   /* Create a scalar counter and intialize it */
   pack_index = Create_Preg(doloop_ty,create_tempname("@f90pack"));
   
   /* Insert initialization expression, and insert DO loop */
   temp = WN_StidPreg(doloop_ty,pack_index,WN_Zerocon(doloop_ty));
   WN_INSERT_BlockBefore(F90_Current_Block,F90_Current_Loopnest,temp);

   /* Lower all the arguments */
   vector = F90_Lower_Walk(kids[0],&pack_index,1,block,insert_point);
   mask = F90_Lower_Walk(kids[1],indices,ndim,block,insert_point);
   field = F90_Lower_Walk(kids[2],indices,ndim,block,insert_point);
   
   /* Create a scalar symbol to hold the result of the unpack for each iteration */
   if (WN_opcode(vector) == OPC_MLOAD) {
      ptr_ty = WN_ty(vector);
      /* Strip off the "pointer to" part */
      temp_ty = TY_pointed(ptr_ty);
      temp_st = F90_Lower_Create_Temp(NULL,NULL,NULL,0,temp_ty,NULL);
      lda = WN_Lda(Pointer_type,(WN_OFFSET) 0, temp_st);
      true_store = WN_CreateMstore((WN_OFFSET) 0, ptr_ty, vector, WN_COPY_Tree(lda),
				   WN_COPY_Tree(WN_kid1(vector)));
      false_store = WN_CreateMstore((WN_OFFSET) 0, ptr_ty, field, WN_COPY_Tree(lda), 
				    WN_COPY_Tree(WN_kid1(vector)));
      load = WN_CreateMload((WN_OFFSET) 0, ptr_ty, lda,WN_COPY_Tree(WN_kid1(vector)));
   } else {
      type = WN_rtype(vector);
      p = Create_Preg(type,create_tempname("@f90s"));
      true_store = WN_StidPreg(type,p,vector);
      false_store = WN_StidPreg(type,p,field);
      load = WN_LdidPreg(type,p);
   }

   then_block = WN_CreateBlock();
   else_block = WN_CreateBlock();
   WN_INSERT_BlockFirst(then_block,true_store);
   increment = WN_StidPreg(doloop_ty,pack_index,
			   WN_CreateExp2(OPCadd,WN_LdidPreg(doloop_ty,pack_index),
					 WN_Intconst(doloop_ty,1)));
   WN_INSERT_BlockLast(then_block,increment);
   WN_INSERT_BlockFirst(else_block,false_store);
   if_stmt = WN_CreateIf(mask,then_block,else_block);
   WN_INSERT_BlockBefore(block,insert_point,if_stmt);
   return (load);
}



/*================================================================ 
  EOSHIFT is non-obvious enough that I'll describe the algorithm.
  EOSHIFT(ARRAY,SHIFT,[BOUNDARY],[DIM])

  DIM - must be an integer constant in order to get here. The first step
  is to convert from the user's 1 based DIM to our 0 based-reversed dim. 

  Let EXTENT be the size of dimension DIM.

  We then check for constant SHIFT. If SHIFT=0, just lower ARRAY. If |SHIFT| >=
  EXTENT, just lower BOUNDARY and return.

  Otherwise, we will pull out the EOSHIFT. Copy (using the utility routine)
  the ARRAY argument to an array TEMP. This will put the store just before
  the F90_Current_Loopnest, and insert the allocations and deallocations in
  the right place. The lowered load of the TEMP is the result of the
  EOSHIFT.
  
  Remove the store from the program, and build up a loopnest for all the
  loops but the shift dimension.  put this new loopnest back into the
  program. Make sure that the adata structure is copied to the loop nest,
  so that we get the allocation. 

  Lower SHIFT and BOUNDARY. Inside the loopnest, build the following magic:
  
  if (SHIFT < 0) {
   l2 = 0
   u1 = extent - 1
   l1 = -shift
   u2 = l1 - 1
  } else {
   l1 = 0
   u2 = extent - 1
   u1 = extent - 1 - shift (or u2 - shift)
   l2 = u1 + 1
  } 
  DO I = L1,U1
    RESULT(...,I,...) = ARRAY(...,I+SHIFT,...)
  ENDDO
  DO I = L2,U2
    RESULT(...,I,...) = BOUNDARY(...)
  ENDDO


*/
  

static WN *lower_eoshift(WN *kids[],PREG_NUM indices[],INT ndim,WN *block, WN *insert_point)
{
   INT dim;
   WN *shift;
   WN *limit_shift;
   WN *boundary;
   PREG_NUM new_indices[MAX_NDIM],index,sb_indices[MAX_NDIM];
   WN *sizes[MAX_NDIM];
   WN *extent;
   WN *result;
   INT ndim_array;
   PREG_NUM shift_index,new_index;
   BOOL shift_pos,shift_neg;
   char tempname[32];
   WN *pos_block;
   WN *neg_block;
   WN *temp;
   WN *setup;
   WN *start,*end,*step,*index_id,*body;
   WN *lhs;
   WN *shift_comp;
   PREG_NUM l1,l2,u1,u2;
   WN *loopnest,*stlist;
   WN *temp_store;
   WN * original_loopnest;

   INT i,j;

   dim = ndim - F90_Get_Dim(kids[3]);
   WN_DELETE_Tree(kids[3]);

   /* Get shift */
   for (i=0, j=0; i < ndim; i++) {
      if (i != dim) {
	 new_indices[j++] = indices[i];
      }
   }
   shift = kids[1];
   
   /* Get the sizes along the shift dimension */
   (void) F90_Size_Walk(kids[0],&ndim_array,sizes);
   extent = sizes[dim];

   /* Check for 0 shift */
   if (WN_operator(shift)==OPR_INTCONST) {
      if (WN_const_val(shift) == 0) {
	 WN_DELETE_Tree(shift);
	 WN_DELETE_Tree(kids[2]);
	 for (i=0 ; i <ndim; i++) {
	    WN_DELETE_Tree(sizes[i]);
	 }
	 result = F90_Lower_Walk(kids[0],indices,ndim,block,insert_point);
	 return (result);
      } else if (WN_operator(extent) == OPR_INTCONST) {
	 if (WN_const_val(shift) >= WN_const_val(extent) ||
	     -WN_const_val(shift) >= WN_const_val(extent)) {
	    WN_DELETE_Tree(kids[0]);
	    WN_DELETE_Tree(shift);
	    for (i=0 ; i <ndim; i++) {
	       WN_DELETE_Tree(sizes[i]);
	    }
	    boundary = F90_Lower_Walk(kids[2],new_indices,ndim-1,block,insert_point);
	    return (boundary);
	 }
      }
   }

   /*
    * General case; we create a brand-new set of loops, and delete the current loopnest
    */

#ifdef KEY
// Bug# 267
   if (MTYPE_bit_size(Pointer_type) == 64 && WN_rtype(shift) != MTYPE_I8)
     kids[1] = WN_Type_Conversion(kids[1], MTYPE_I8);
#endif

   temp_store = F90_Current_Stmt;
   WN_EXTRACT_FromBlock(block,temp_store);
   original_loopnest = F90_Current_Loopnest;
   
   num_temps += 1;
   loopnest = WN_CreateBlock();
   stlist = loopnest;
   if (ndim > 1) {
      for (i=ndim-1,j=ndim-2; i >=0 ; i--) {
	 if (i != dim) {
	    sprintf(tempname,"@f90li_%d_%d",i,num_temps);
	    loopnest = create_doloop(&index,tempname,sizes[i],DIR_POSITIVE,loopnest);
	    sb_indices[j--] = index;
	    new_indices[i] = index;
	 }
      }
      WN_INSERT_BlockBefore(F90_Current_Block,F90_Current_Loopnest,loopnest);
      F90_Current_Loopnest = loopnest;
   }

   /* Now, we can lower boundary and shift */
   boundary = F90_Lower_Walk(kids[2],sb_indices,ndim-1,block,insert_point);
   shift = F90_Lower_Walk(kids[1],sb_indices,ndim-1,block,insert_point);

   /* Create the extents for the various loops */
   l1 = Create_Preg(doloop_ty,create_tempname("@f90_eos_l1"));
   u1 = Create_Preg(doloop_ty,create_tempname("@f90_eos_u1"));
   l2 = Create_Preg(doloop_ty,create_tempname("@f90_eos_l2"));
   u2 = Create_Preg(doloop_ty,create_tempname("@f90_eos_u2"));

   shift_comp = WN_CreateExp2(OPCODE_make_op(OPR_GT,MTYPE_I4,doloop_ty),
			      WN_COPY_Tree(shift),
			      WN_Intconst(doloop_ty,0));
   if (WN_operator(shift_comp) == OPR_INTCONST) {
      if (WN_const_val(shift_comp) == 0) {
	 shift_neg = TRUE;
	 shift_pos = FALSE;
      } else {
	 shift_neg = FALSE;
	 shift_pos = TRUE;
      }
      WN_DELETE_Tree(shift_comp);
   } else {
      shift_neg = TRUE;
      shift_pos = TRUE;
   }
      
   /* Create the assignments for the cases */
   if (shift_pos) {
      /* 
       * l1 = 0
       * u2 = extent - 1
       * u1 = extent - 1 - shift (or u2 - shift)
       * l2 = u1 + 1
       */
      pos_block = WN_CreateBlock();
      temp = WN_StidPreg(doloop_ty,l1,WN_Intconst(doloop_ty,0));
      WN_INSERT_BlockLast(pos_block,temp);

      limit_shift = WN_CreateExp2(OPCODE_make_op(OPR_MIN,doloop_ty,MTYPE_V),
				  WN_COPY_Tree(sizes[dim]),WN_COPY_Tree(shift));
   
      temp = WN_StidPreg(doloop_ty,u2,
			 WN_CreateExp2(OPCsub,WN_COPY_Tree(sizes[dim]),
				       WN_Intconst(doloop_ty,1)));
      WN_INSERT_BlockLast(pos_block,temp);
      
      temp = WN_StidPreg(doloop_ty,u1,WN_CreateExp2(OPCsub,
						    WN_LdidPreg(doloop_ty,u2),
						    limit_shift));
      WN_INSERT_BlockLast(pos_block,temp);

      temp = WN_StidPreg(doloop_ty,l2,WN_CreateExp2(OPCadd,
						    WN_LdidPreg(doloop_ty,u1),
						    WN_Intconst(doloop_ty,1)));
      WN_INSERT_BlockLast(pos_block,temp);
      setup = pos_block;
   }   

   if (shift_neg) {
      /* 
       * l2 = 0
       * u1 = extent - 1
       * l1 = -shift
       * u2 = l1 - 1
       */
      neg_block = WN_CreateBlock();
      temp = WN_StidPreg(doloop_ty,l2,WN_Intconst(doloop_ty,0));
      WN_INSERT_BlockLast(neg_block,temp);
      
      limit_shift = WN_CreateExp1(OPCODE_make_op(OPR_NEG,doloop_ty,MTYPE_V),
				  WN_COPY_Tree(shift));
      limit_shift = WN_CreateExp2(OPCODE_make_op(OPR_MIN,doloop_ty,MTYPE_V),
				  WN_COPY_Tree(sizes[dim]),limit_shift);

      temp = WN_StidPreg(doloop_ty,u1,
			 WN_CreateExp2(OPCsub,WN_COPY_Tree(sizes[dim]),
				       WN_Intconst(doloop_ty,1)));
      WN_INSERT_BlockLast(neg_block,temp);

      temp = WN_StidPreg(doloop_ty,l1,limit_shift);

      WN_INSERT_BlockLast(neg_block,temp);
      
      temp = WN_StidPreg(doloop_ty,u2,WN_CreateExp2(OPCsub,
						    WN_LdidPreg(doloop_ty,l1),
						    WN_Intconst(doloop_ty,1)));
      WN_INSERT_BlockLast(neg_block,temp);
      setup = neg_block;
   }   
   
   if (shift_pos && shift_neg) {
      setup = WN_CreateIf(shift_comp, pos_block, neg_block);
   }

   /* Insert the setup code */
   WN_INSERT_BlockFirst(stlist,setup);

   new_index = Create_Preg(doloop_ty,create_tempname("@f90_eos_idx"));
   shift_index = Create_Preg(doloop_ty,create_tempname("@f90_eos_shft"));
   
   
   start = WN_StidPreg(doloop_ty,new_index,WN_LdidPreg(doloop_ty,l1));
   end = WN_CreateExp2(OPCODE_make_op(OPR_LE,MTYPE_I4,doloop_ty),
		       WN_LdidPreg(doloop_ty,new_index),
		       WN_LdidPreg(doloop_ty,u1));
   step = WN_StidPreg(doloop_ty,new_index,
		      WN_CreateExp2(OPCadd,WN_LdidPreg(doloop_ty,new_index),
				    WN_Intconst(doloop_ty,1)));
   index_id = WN_CreateIdname(new_index,MTYPE_To_PREG(doloop_ty));
   

   body = WN_CreateBlock();
   temp = create_doloop_node(index_id, start, end, step, body);

   WN_INSERT_BlockLast(stlist,temp);

   temp = WN_StidPreg(doloop_ty,shift_index,WN_CreateExp2(OPCadd,
							  WN_LdidPreg(doloop_ty,new_index),
							  shift));
   WN_INSERT_BlockFirst(body,temp);

   /* Now, build the contents of the first DO loop */
   /* Remember that temp_store has the form TEMP = ARRAY, where
    * ARRAY is the first argument to EOSHIFT
    */
   WN_kid0(temp_store) = WN_Intconst(doloop_ty,0); /* So that the copy is cheap */
   lhs = WN_COPY_Tree(temp_store);
   WN_DELETE_Tree(WN_kid0(temp_store));
   WN_DELETE_Tree(WN_kid0(lhs));
   
   WN_INSERT_BlockLast(body,temp_store);
   new_indices[dim] = shift_index;
   temp = F90_Lower_Walk(kids[0],new_indices,ndim,body,temp_store);
   WN_kid0(temp_store) = temp;
   new_indices[dim] = new_index;
   temp = F90_Lower_Walk(WN_kid1(temp_store),new_indices,ndim,body,temp_store);
   /* Form of the stored address is the same in both loops */
   WN_kid1(temp_store) = temp;
   WN_kid1(lhs) = WN_COPY_Tree(temp);
   WN_kid0(lhs) = boundary;

   /* Second loop */
   start = WN_StidPreg(doloop_ty,new_index,WN_LdidPreg(doloop_ty,l2));
   end = WN_CreateExp2(OPCODE_make_op(OPR_LE,MTYPE_I4,doloop_ty),
		       WN_LdidPreg(doloop_ty,new_index),
		       WN_LdidPreg(doloop_ty,u2));
   step = WN_StidPreg(doloop_ty,new_index,
		      WN_CreateExp2(OPCadd,WN_LdidPreg(doloop_ty,new_index),
				    WN_Intconst(doloop_ty,1)));

   body = WN_CreateBlock();
   temp = create_doloop_node(WN_COPY_Tree(index_id), start, end, step, body);
   WN_INSERT_BlockLast(stlist,temp);
   WN_INSERT_BlockLast(body,lhs);


   /* If ndim=1, we need to put stlist into the tree */
   if (ndim == 1) {
      WN_INSERT_BlockBefore(F90_Current_Block,F90_Current_Loopnest,stlist);
   }

   /* remove the original loopnest */
   WN_EXTRACT_FromBlock(F90_Current_Block,original_loopnest);
   WN_DELETE_Tree(original_loopnest);
   
   return (NULL);
}


static WN *lower_cshift(WN *kids[],PREG_NUM indices[],INT ndim,WN *block, WN *insert_point)
{
   INT dim;
   WN *shift;
   PREG_NUM new_indices[MAX_NDIM];
   WN *sizes[MAX_NDIM];
   WN *extent;
   WN *store;
   WN *result;
   INT ndim_array;
   PREG_NUM shifted_index,shift_preg;

   INT i,j;

   dim = ndim - F90_Get_Dim(kids[2]);
   WN_DELETE_Tree(kids[2]);

   /* Get shift */
   for (i=0, j=0; i < ndim; i++) {
      if (i != dim) {
	 new_indices[j++] = indices[i];
      }
   }
   shift = F90_Lower_Walk(kids[1],new_indices,ndim-1,block,insert_point);
   
   /* Get the sizes along the shift dimension */
   (void) F90_Size_Walk(kids[0],&ndim_array,sizes);
   for (i=0; i < ndim; i++) {
      if (i != dim) {
	 new_indices[i] = indices[i];
	 WN_DELETE_Tree(sizes[i]);
      } else {
	 extent = sizes[i];
      }
   }
#ifdef KEY
// Bug# 274
   if (MTYPE_bit_size(Pointer_type) == 64 && WN_rtype(shift) != MTYPE_I8)
     shift = WN_Type_Conversion(shift, MTYPE_I8);
#endif
   shift = WN_CreateExp2(OPCmod,shift,WN_COPY_Tree(extent));

   /* Check for 0 shift */
   if (WN_operator(shift)==OPR_INTCONST && WN_const_val(shift) == 0) {
      WN_DELETE_Tree(shift);
      WN_DELETE_Tree(extent);
      new_indices[dim] = indices[dim];
   } else {
      shifted_index = Create_Preg(doloop_ty,create_tempname("@f90cshift"));
      shift_preg = Create_Preg(doloop_ty,create_tempname("@f90cshiftp"));
      new_indices[dim] = shifted_index;
      
      /* Shift_preg contains old_index + modulo(shift,extent) */
      store = WN_StidPreg(doloop_ty,shift_preg,
			  WN_CreateExp2(OPCadd,WN_LdidPreg(doloop_ty,indices[dim]),shift));
      WN_INSERT_BlockBefore(block,insert_point,store);
      
      store = WN_CreateExp3(OPCODE_make_op(OPR_SELECT,doloop_ty,MTYPE_V),
			    WN_CreateExp2(OPCODE_make_op(OPR_GE,MTYPE_I4,doloop_ty),
					  WN_LdidPreg(doloop_ty,shift_preg),
					  WN_COPY_Tree(extent)),
			    extent,
			    WN_Intconst(doloop_ty,0));
      store = WN_CreateExp2(OPCsub,WN_LdidPreg(doloop_ty,shift_preg),store);
      store = WN_StidPreg(doloop_ty,shifted_index,store);
      WN_INSERT_BlockBefore(block,insert_point,store);
   }
      
   result = F90_Lower_Walk(kids[0],new_indices,ndim,block,insert_point);
   return (result);
}

 

//================================================================
//
// Lower matrix-matrix multiply
//

static WN * lower_mm_matmul(TYPE_ID rty,WN *kids[],PREG_NUM indices[],WN *block,WN *insert_point)
{
  WN *inner_sizes[2];
  TYPE_ID ty;
  INT ndim;
  PREG_NUM A1_indices[2];
  PREG_NUM A2_indices[2];
  PREG_NUM accum;
  PREG_NUM inner_index;
  WN * idty_store;
  WN *a1, *a2;
  WN * stlist, *loopnest;
  WN * result;
  OPCODE add_op, mpy_op;

  
  // get the inner size
  (void) F90_Size_Walk(kids[0],&ndim,inner_sizes);
  WN_DELETE_Tree(inner_sizes[1]);

  /* Create a temp and insert an identity store */
  ty = Mtype_comparison(rty);
  
  if (rty == MTYPE_B) {
    add_op = OPC_I4LIOR;
    mpy_op = OPC_I4LAND;
    ty = MTYPE_I4;
    rty = MTYPE_I4;
  } else {
    add_op = OPCODE_make_op(OPR_ADD,ty,MTYPE_V);
    mpy_op = OPCODE_make_op(OPR_MPY,ty,MTYPE_V);
  }

  accum = Create_Preg(rty,create_tempname("@f90mm"));
  idty_store = WN_StidPreg(rty,accum,WN_Zerocon(ty));
  WN_INSERT_BlockBefore(block,insert_point,idty_store);
  
  loopnest = WN_CreateBlock();
  stlist = loopnest;
  loopnest = create_doloop(&inner_index,create_tempname("@f90_mmidx"),inner_sizes[0],DIR_POSITIVE,
			   loopnest);
  WN_INSERT_BlockBefore(block,insert_point,loopnest);
  A1_indices[0] = inner_index;
  A1_indices[1] = indices[1];
  A2_indices[0] = indices[0];
  A2_indices[1] = inner_index;
  
  /* Lower the array arguments */
  a1 = F90_Lower_Walk(kids[0],A1_indices,2,stlist,NULL);
  a2 = F90_Lower_Walk(kids[1],A2_indices,2,stlist,NULL);

  /* Generate the multiply-add */
  a1 = WN_CreateExp2(mpy_op,a1,a2);
  a1 = WN_StidPreg(rty,accum,WN_CreateExp2(add_op,WN_LdidPreg(rty,accum),a1));
  
  WN_INSERT_BlockLast(stlist,a1);

  /* Return the load of the result variable */
  result = WN_LdidPreg(rty,accum);
  
  return (result);

}

//================================================================
//
// Lower matrix-vector multiply
//

static WN * lower_mv_matmul(TYPE_ID rty,WN *kids[],PREG_NUM indices[],WN *block,WN *insert_point)
{
  WN *inner_sizes1[2];
  WN *inner_sizes2[2];
  TYPE_ID ty;
  INT ndim1,ndim2;
  PREG_NUM A1_indices[2];
  PREG_NUM A2_indices[2];
  PREG_NUM accum;
  PREG_NUM inner_index;
  WN * idty_store;
  WN *a1, *a2;
  WN * stlist, *loopnest;
  WN * result;
  OPCODE add_op, mpy_op;
  
  
  /* Create a temp and insert an identity store */
  ty = Mtype_comparison(rty);
  
  if (rty == MTYPE_B) {
    add_op = OPC_I4LIOR;
    mpy_op = OPC_I4LAND;
    ty = MTYPE_I4;
    rty = MTYPE_I4;
  } else {
    add_op = OPCODE_make_op(OPR_ADD,ty,MTYPE_V);
    mpy_op = OPCODE_make_op(OPR_MPY,ty,MTYPE_V);
  }

  accum = Create_Preg(rty,create_tempname("@f90mv"));
  idty_store = WN_StidPreg(rty,accum,WN_Zerocon(ty));
  WN_INSERT_BlockBefore(block,insert_point,idty_store);
  
  // get the inner sizes
  (void) F90_Size_Walk(kids[0],&ndim1,inner_sizes1);
  (void) F90_Size_Walk(kids[1],&ndim2,inner_sizes2);

  loopnest = WN_CreateBlock();
  stlist = loopnest;

  // Figure out which is the matrix, and which the vector
  if (ndim1 == 2) {
    // matrix-vector
    WN_DELETE_Tree(inner_sizes1[0]);
    WN_DELETE_Tree(inner_sizes1[1]);
    loopnest = create_doloop(&inner_index,create_tempname("@f90_mvidx"),inner_sizes2[0],DIR_POSITIVE,
			     loopnest);
    A1_indices[0] = inner_index;
    A1_indices[1] = indices[0];
    a1 = F90_Lower_Walk(kids[0],A1_indices,2,stlist,NULL);
    A2_indices[0] = inner_index;
    a2 = F90_Lower_Walk(kids[1],A2_indices,1,stlist,NULL);
  } else {
    // vector-matrix
    WN_DELETE_Tree(inner_sizes2[0]);
    WN_DELETE_Tree(inner_sizes2[1]);
    loopnest = create_doloop(&inner_index,create_tempname("@f90_mvidx"),inner_sizes1[0],DIR_POSITIVE,
			     loopnest);
    A1_indices[0] = inner_index;
    a1 = F90_Lower_Walk(kids[0],A1_indices,1,stlist,NULL);
    A2_indices[0] = indices[0];
    A2_indices[1] = inner_index;
    a2 = F90_Lower_Walk(kids[1],A2_indices,2,stlist,NULL);
  }    
  WN_INSERT_BlockBefore(block,insert_point,loopnest);

  /* Generate the multiply-add */
  a1 = WN_CreateExp2(mpy_op,a1,a2);
  a1 = WN_StidPreg(rty,accum,WN_CreateExp2(add_op,WN_LdidPreg(rty,accum),a1));
  
  WN_INSERT_BlockLast(stlist,a1);
  
  /* Return the load of the result variable */
  result = WN_LdidPreg(rty,accum);
  
  return (result);

}

/***********************************************
* 
* Routines to lower transformational intrinsics. Some are dealt with in lower_transformationals
* others require their own routine.
*
* Arguments are the same as F90_Lower_Walk
****************************************************************/

static WN * lower_transformationals(WN *expr, PREG_NUM *indices, INT ndim, WN * block, WN *insert_point)
{
   WN *result;
   WN *kids[6];
   WN *kid;
   INT dim;
   TYPE_ID ty;
   INTRINSIC intrin;
   INT numargs,i,j;
   PREG_NUM new_indices[MAX_NDIM];

   numargs = WN_kid_count(expr);
   ty = WN_rtype(expr);
   intrin = WN_GET_INTRINSIC(expr);

   /* First, strip all the arguments out, remove their PARM nodes */
   for (i=0 ; i < numargs; i++) {
      kid = WN_kid(expr,i);
      if (WN_kid_count(kid) == 0 || WN_opcode(kid) == OPC_VPARM) {
	 kids[i] = NULL;
	 WN_Delete(kid);
      } else if (WN_operator(kid) == OPR_PARM) {
	 kids[i] = WN_kid0(kid);
	 WN_Delete(kid);
      } else {
	 kids[i] = kid;
      }
   }
   WN_Delete(expr);

   switch (intrin) {
    case INTRN_RESHAPE:
    case INTRN_DOT_PRODUCT:
    case INTRN_COUNT:
      DevAssert((0),("Intrinsic should not get inlined"));
      break;

    case INTRN_SPREAD:
      dim = ndim - WN_const_val(kids[1]); /* account for ordering */
      for (i=0,j=0; i < ndim; i++) {
	 if (dim != i) {
	    new_indices[j++] = indices[i];
	 }
      }
      WN_DELETE_Tree(kids[1]);
      WN_DELETE_Tree(kids[2]);
      result = F90_Lower_Walk(kids[0],new_indices,ndim-1, block,insert_point);
      break;


    case INTRN_TRANSPOSE:
      new_indices[0] = indices[1];
      new_indices[1] = indices[0];
      result = F90_Lower_Walk(kids[0],new_indices,2, block,insert_point);
      break;

      
    case INTRN_MATMUL:
      if (ndim == 1) {
	result = lower_mv_matmul(ty,kids,indices,block,insert_point);
      } else if (ndim==2) {
	result = lower_mm_matmul(ty,kids,indices,block,insert_point);
      } else {
	Is_True(FALSE,("matmul with ndim != 1 or 2"));
      }
      break;

    case INTRN_ALL:
      kids[2] = NULL;
      result = lower_reduction(MTYPE_I4,OPR_LAND,kids,indices,ndim,block,insert_point);
      break;
    case INTRN_ANY:
      kids[2] = NULL;
      result = lower_reduction(MTYPE_I4,OPR_LIOR,kids,indices,ndim,block,insert_point);
      break;
    case INTRN_PRODUCT:
      result = lower_reduction(ty,OPR_MPY,kids,indices,ndim,block,insert_point);
      break;
    case INTRN_SUM:
      result = lower_reduction(ty,OPR_ADD,kids,indices,ndim,block,insert_point);
      break;
    case INTRN_MAXVAL:
      result = lower_reduction(ty,OPR_MAX,kids,indices,ndim,block,insert_point);
      break;
    case INTRN_MINVAL:
      result = lower_reduction(ty,OPR_MIN,kids,indices,ndim,block,insert_point);
      break;

    case INTRN_PACK:
      result = lower_pack(kids,indices,ndim,block,insert_point);
      break;

    case INTRN_UNPACK:
      result = lower_unpack(kids,indices,ndim,block,insert_point);
      break;

    case INTRN_MAXLOC:
      result = lower_maxminloc(OPR_MAX,kids,indices,ndim,block,insert_point);
      break;

    case INTRN_MINLOC:
      result = lower_maxminloc(OPR_MIN,kids,indices,ndim,block,insert_point);
      break;

    case INTRN_CSHIFT:
      result = lower_cshift(kids,indices,ndim,block,insert_point);
      break;

    case INTRN_EOSHIFT:
      result = lower_eoshift(kids,indices,ndim,block,insert_point);
      break;

    default:
      DevAssert((0),("Unknown intrinsic in lower_transformationals"));
   }

   return (result);
}

/***************************************************************************
 *
 * strip_mloads removes MLOADS from all the children of character intrinsics,
 * because unfortunately the transformationals may have left them there.
 *
 */
static WN * strip_mloads(WN *w)
{
   WN *kid;
   while (WN_opcode(w) == OPC_MLOAD) {
      kid = WN_kid0(w);
      WN_DELETE_Tree(WN_kid1(w));
      WN_Delete(w);
      w = kid;
   }
   return (w);
}

/***********************************************************
*
* Turn VH constructs into H constructs. This is the heart of the lowering routines
* 
* WN * F90_Lower_Walk(WN *expr, PREG_NUM *indices, INT ndim, WN * block, WN *insert_point)
* 
* expr - the current expression to lower
* indices - array of loop indices to use in the lowering
* ndim - dimensionality of the loop
* block - block in which to insert additional statements (The block child of the innermost 
*         loop nest)
* insert_point - the current statement in the DO loop nest
*      (the parent statement for the expression) 
* 
* Returns - a properly lowered expression. In case the expression is a statement 
* level expression, it is guaranteed that the statement node will not have a new address
* (i.e., it will be updated in place).
*
*************************************************************/


static WN * F90_Lower_Walk(WN *expr, PREG_NUM *indices, INT ndim, WN * block, WN *insert_point)
{
   OPCODE op,kidop;
   OPERATOR opr,kidopr;
   WN *kid,*kid1;
   WN *result;
   INT numkids,array_ndim,i,j;
   BOOL do_kids = FALSE;
   TYPE_ID ty;
   WN * index_ldid;
   INTRINSIC intr;

   op = WN_opcode(expr);
   opr = OPCODE_operator(op);
   numkids = WN_kid_count(expr);
   result = expr;
   
   switch (opr) {
    case OPR_BLOCK:
      kid = WN_first(expr);
      while (kid) {
	 F90_Lower_Walk(kid,indices,ndim,block,insert_point);
	 kid = WN_next(kid);
      }
      break;

    case OPR_ARRAYEXP:
      /* If we see this, we just lower the first child and remove the rest */
      for (i=1; i<numkids; i++) {
	 WN_DELETE_Tree(WN_kid(expr,i));
      }
      kid = WN_kid0(expr);
      WN_Delete(expr);
      result = F90_Lower_Walk(kid,indices,ndim,block,insert_point);
      break; /* Don't set do_kids */

    case OPR_ARRSECTION:
      /* Change the operand to OPC_ARRAY */
      WN_set_opcode(result,OPCODE_make_op(OPR_ARRAY,OPCODE_rtype(op),MTYPE_V));

      /* Lower only those children which are array operations */
      array_ndim = (numkids - 1) / 2;
      j = 0;
      for (i=array_ndim+1; i < numkids; i++) {
	 kid = WN_kid(expr,i);
	 kidop = WN_opcode(kid);
	 kidopr = OPCODE_operator(kidop);
	 if (kidopr == OPR_ARRAYEXP || kidopr == OPR_TRIPLET) {
	    /* Lower this child, and replace it */
	    kid = F90_Lower_Walk(kid,&indices[j],1,block,insert_point);
	    j += 1; /* Move onto next index */
	    WN_kid(expr,i) = kid;
	 } else {
	   // Still need to lower the kid (just in case there is a reduction in it))
	    kid = F90_Lower_Walk(kid,NULL,0,block,insert_point);
	    WN_kid(expr,i) = kid;
	 }
      }
      break; /* Don't lower children */

    case OPR_TRIPLET:
      /* return kid0 + kid1*index */
      FmtAssert((ndim==1),("F90_Lower_Walk: trying to lower a triplet with ndim != 1"));
      WN_DELETE_Tree(WN_kid2(expr)); /* Get rid of the extent child */
      kid = F90_Lower_Walk(WN_kid0(expr),NULL,0,block,insert_point);
      kid1 = F90_Lower_Walk(WN_kid1(expr),NULL,0,block,insert_point);
      ty = doloop_ty;
#ifdef KEY // bug 3130
      if (MTYPE_byte_size(ty) != MTYPE_byte_size(WN_rtype(kid)))
        kid = WN_Cvt(WN_rtype(kid), ty, kid);
#endif
#ifdef KEY // bug 3518
      if (MTYPE_byte_size(ty) != MTYPE_byte_size(WN_rtype(kid1)))
        kid1 = WN_Cvt(WN_rtype(kid1), ty, kid1);
#endif
      index_ldid = WN_LdidPreg(ty,indices[0]);
      kid1 = WN_CreateExp2(OPCODE_make_op(OPR_MPY,ty,MTYPE_V),
					   index_ldid,
					   kid1);
#ifdef KEY // bug 3130
      if (MTYPE_byte_size(ty) != MTYPE_byte_size(WN_rtype(kid1)))
        kid1 = WN_Cvt(WN_rtype(kid1), ty, kid1);
#endif
      result = WN_CreateExp2(OPCODE_make_op(OPR_ADD,ty,MTYPE_V), kid, kid1);
      break;

    case OPR_WHERE:
      /* Replace the WHERE with an IF, lower children */
      WN_set_opcode(result,OPC_IF);
      do_kids = TRUE;
      break;

    case OPR_MLOAD:
      /* Need to avoid MLOAD(MLOAD) */
      kid = F90_Lower_Walk(WN_kid0(expr),indices,ndim,block,insert_point);
      WN_kid0(expr) = strip_mloads(kid);
      kid = F90_Lower_Walk(WN_kid1(expr),indices,ndim,block,insert_point);
      WN_kid1(expr) = kid;
      do_kids = FALSE;
      break;

    case OPR_INTRINSIC_CALL:
    case OPR_INTRINSIC_OP:
      intr = WN_GET_INTRINSIC(expr);
      if (F90_Is_Transformational(intr)) {
	 result = lower_transformationals(expr,indices, ndim, block, insert_point);
	 do_kids = FALSE;
      } else if (F90_Is_Char_Intrinsic(intr)) {
	 for (i=0; i < numkids; i++) {
	    kid = WN_kid0(WN_kid(expr,i));
	    kid = F90_Lower_Walk(kid,indices,ndim,block,insert_point);
	    WN_kid0(WN_kid(expr,i)) = strip_mloads(kid);
	 }
	 do_kids = FALSE;
      } else {
	 do_kids = TRUE;
      }
      break;

    default:
      /* Just lower all the children */
      do_kids = TRUE;
      break;
   }

   if (do_kids) {
      for (i=0; i < numkids; i++) {
	 kid = WN_kid(expr,i);
	 kid = F90_Lower_Walk(kid,indices,ndim,block,insert_point);
	 if (kid) {
	    WN_kid(expr,i) = kid;
	 }
      }
   }

   return (result);
}


/***********************************************************************
* 
* This is the driver routine for doing the final transformation to DO loops
* 
**********************************************************************/



static BOOL F90_Generate_Loops(WN *stmt, WN *block)
{
   F90_LOWER_AUX_DATA *adata;
   INT ndim;
   PREG_NUM indices[MAX_NDIM];
   PREG_NUM index;
   INT i;
   INT perm;
   DIR_FLAG dir;
   WN *count;
   WN *loopnest,*stlist;
   char tempname[32]; /* This isn't going to overflow */

#ifndef KEY // bug 8567
   /* Don't walk I/O statements */
   if (WN_operator(stmt) == OPR_IO) {
      return(TRUE);
   }
#endif

   adata = GET_F90_MAP(stmt);
   if (adata) {
      ndim = NDIM(adata);
   } else {
      ndim = 0;
   }

   if (ndim > 0) {
      /* Create the DO loop nest */
      loopnest = WN_CreateBlock();
      stlist = loopnest;
      num_temps += 1;
      for (i=ndim-1; i >= 0; i--) {
	 perm = PERM_INDEX(adata,i);
	 count = ITER_COUNT(adata,perm);
	 dir = DIRECTION(adata,perm);
	 sprintf(tempname,"@f90li_%d_%d",i,num_temps);
	 
	 /* Create the DO loop node */
	 loopnest = create_doloop(&index,tempname,count,dir,loopnest);
	 indices[i] = index;
      }
      
      /* Put the DO loop in the block before stmt, then remove stmt from the
	 block */
      WN_INSERT_BlockBefore(block,stmt,loopnest);
      (void) WN_EXTRACT_FromBlock(block,stmt);
      WN_INSERT_BlockFirst(stlist,stmt);
      
      /* if in the workshare region, autoparallelize the DO loop */
      BOOL_VECTOR::iterator bi = F90_MP_Region_Isworkshare.begin();
      if( (bi != F90_MP_Region_Isworkshare.end()) && *bi ){
        WN* region = WN_CreateRegion(REGION_KIND_MP,
                        WN_CreateBlock(),
                        WN_CreateBlock(),
                        WN_CreateBlock(),
                        -1,
                        0);
        WN* region_body = WN_region_body(region);

        WN_INSERT_BlockBefore(block,loopnest,region);
        (void) WN_EXTRACT_FromBlock(block, loopnest);
        WN_INSERT_BlockFirst(region_body,loopnest);
	      
        WN* pragmas = WN_region_pragmas(region);

        WN* pragma = WN_CreatePragma(WN_PRAGMA_PDO_BEGIN, (ST *)NULL, 0, 1);
        WN_set_pragma_omp(pragma);
        WN_INSERT_BlockFirst(pragmas, pragma);

        ST *index_st = MTYPE_To_PREG(doloop_ty);

        for(i=0; i<ndim; i++){
          pragma = WN_CreatePragma(WN_PRAGMA_LOCAL, index_st, indices[i], 0);
          WN_set_pragma_omp(pragma);
          WN_INSERT_BlockLast(pragmas, pragma);
        }

        pragma = WN_CreatePragma(WN_PRAGMA_END_MARKER, (ST *)NULL, 0, 0);
        WN_set_pragma_omp(pragma);
        WN_INSERT_BlockLast(pragmas, pragma);
        F90_Current_Loopnest = region;
      }
      else{
        F90_Current_Loopnest = loopnest;
      }
       
      /* Lower the statement */
      F90_Current_Stmt = stmt;
      F90_Current_Loopnest = loopnest;
   } else {
      /* Case of a statement which may only contain a call to a reduction */
      F90_Current_Stmt = stmt;
      F90_Current_Loopnest = stmt;
      stlist = block;
   }
   F90_Current_Block = block;
   stmt = F90_Lower_Walk(stmt,indices,ndim,stlist,stmt);
   
   return (TRUE);
}



/* This is the main external interface to the F90 lowerer 
 *
 *  WN * F90_Lower_PU (WN *pu);
 *
 * Lowers the program unit in *pu. Returns a lowered program unit
 * (most likely, it will be the same pointer passed in, with a lot of changed subtrees,
 * but not necessarily).
 *
 */

#define TRACE_AFTER(x,y) if (Get_Trace(TP_LOWER90,x)) {\
      fprintf(TFile,"\n\n========== Dump after %s ==========\n",y); fdump_tree(TFile,pu);}
    
#ifdef Is_True_On
#define DUPCHECK 1
#else
#undef DUPCHECK
#endif

#ifdef DUPCHECK
#define SET_P_MAP(x,t) WN_MAP_Set(f90_parent_map,(x),(void *) (t))
#define GET_P_MAP(x) ((WN *) WN_MAP_Get(f90_parent_map,(x)))

static void check_for_duplicates(WN *pu, const char *str)
{
   /* Set up a parent map */
   static WN_MAP f90_parent_map;
   WN_ITER *ti;
   WN *w, *k, *p;
   INT i;
   BOOL found_dup = FALSE;

   f90_parent_map = WN_MAP_Create(f90_lower_pool);
   WB_F90_Lower_Set_Parent_Map(f90_parent_map);
   
   /* Walk everything */
   ti = WN_WALK_TreeIter(pu);
   while (ti) {
      w = WN_ITER_wn(ti);
      /* look at all the kids */
      for (i=0; i < WN_kid_count(w) ; i++) {
	 k = WN_kid(w,i);
	 p = GET_P_MAP(k);
	 if ((p != NULL) && (p != w)) {
	    fprintf(TFile,"\n%s: Multiparented node p=%8p, w=%8p, k=%d\n",str,p,w,i);
	    fprintf(TFile,"parent:\n"); fdump_tree(TFile,p);
	    fprintf(TFile,"current:\n"); fdump_tree(TFile,w);
	    
	    // csc. 2002/11/14
	    WN * temp_csc = GET_P_MAP(w);
	    if( temp_csc != NULL ){
		    fprintf(TFile, "current's parent:\n");
		    fdump_tree(TFile, temp_csc );
	    }
	    
	    fprintf(TFile,"multichild:\n"); fdump_tree(TFile,k);
	    found_dup = TRUE;
	 } else {
	    SET_P_MAP(k,w);
	 }
      }
      ti = WN_WALK_TreeNext(ti);
   }
   WB_F90_Lower_Set_Parent_Map(WN_MAP_UNDEFINED);
   WN_MAP_Delete(f90_parent_map);
   if (found_dup) {
      DevWarn(("Duplicate WHIRL nodes found %s\n"),str);
   }
}
#endif
/*=============================================================
 *  *
 *   * void Strip_OMP_Workshare(WN *pu)
 *    *
 *     * Simply strip OpenMP WORKSHARE/END WORKSHARE directives for the program unit.
 *      * (Added by Jiang Hongshan, Aug., 8, 2003)
 *       *
 *        *================================================================
 *         */
void Strip_OMP_Workshare(WN * pu)
{
  OPCODE opcode = WN_opcode(pu);
    if(opcode != OPC_REGION){
      if(opcode == OPC_BLOCK){
        WN *kid = WN_first(pu);
        while(kid){
            Strip_OMP_Workshare(kid);
            kid = WN_next(kid);
        }
      }
      else if(OPCODE_is_scf(opcode)){
        INT numkids = WN_kid_count(pu);
        for(INT i=0; i<numkids; i++){
          Strip_OMP_Workshare(WN_kid(pu,i));
        }
      }
      return;
    }
    WN *pragmas = WN_region_pragmas(pu);
    WN *body = WN_region_body(pu);
    if(pragmas){
      WN *pragma = WN_first(pragmas);
      while(pragma){
        if(WN_opcode(pragma)==OPC_PRAGMA){
          if((WN_PRAGMA_ID)WN_pragma(pragma)==WN_PRAGMA_PWORKSHARE_BEGIN){
            WN_Delete(WN_EXTRACT_FromBlock(pragmas, pragma));
            WN *end_wn = WN_last(pragmas);
            if(end_wn && WN_opcode(end_wn) == OPC_PRAGMA &&
              ((WN_PRAGMA_ID)WN_pragma(end_wn)==WN_PRAGMA_NOWAIT) ||
              ((WN_PRAGMA_ID)WN_pragma(end_wn)==WN_PRAGMA_END_MARKER) ){
              WN_Delete(WN_EXTRACT_FromBlock(pragmas, end_wn));
            }
          break;
        }
        if((WN_PRAGMA_ID)WN_pragma(pragma)==WN_PRAGMA_PARALLEL_WORKSHARE){
          WN_pragma(pragma) = WN_PRAGMA_PARALLEL_BEGIN;
          break;
        }
      }
      pragma = WN_next(pragma);
    }
  }
  Strip_OMP_Workshare(body);
}

/*=============================================================
 *
 * Main entry point 
 * 
 * WN * F90_Lower (WN *pu)*
 *
 * Remove most of the F90 constructs, turning them into loops.
 * See doc.noship/Fortran/lowering.frame for detailed description
 *
 *================================================================
 */

WN * F90_Lower (PU_Info* pu_info, WN *pu) {

   current_pu = &Get_Current_PU();

   /* See if we are actually processing an F90 pu */
   if (!PU_f90_lang(*current_pu)){
	if(PU_has_mp(*current_pu)) Strip_OMP_Workshare(pu);
	return (pu);
   }
   
   F90_Lower_Init();
   
   trace_dependence = Get_Trace(TP_LOWER90,TRACE_DEPENDENCE_ANALYSIS);
   trace_depinfo    = Get_Trace(TP_LOWER90,TRACE_DEPINFO);

   if (Get_Trace ( TKIND_IR, TP_LOWER90)) {
      fprintf(TFile,"\n\n========== Dump before F90 Lowering ==========\n");
      fdump_tree(TFile,pu);
   }

#ifdef DUPCHECK
   check_for_duplicates(pu,"before");
#endif

   /* Move array expressions in places we don't want to see them into temps */
   F90_Walk_All_Statements(pu,F90_Scalarize_Triplets_And_Sizes);

   /* Do dependence analysis, and see if there is anything to do
    * This routine also removes all scalar dependencies (MLOAD/MSTORES)
    * This is the place where the iteration counts, loop directions and 
    * permutation indices are set up 
    */
   F90_Analyze_Dependencies(pu);

   if (array_statement_seen) {
      TRACE_AFTER(TRACE_DEPENDENCE,"Dependence Analysis");
      
      /* Do temp copies needed for dependence removal */
      F90_Walk_Statements(pu,F90_Do_Copies);
      TRACE_AFTER(TRACE_COPIES,"Copy motion");

      /* Pull out transformational intrinsics which can't be inlined */
      F90_Walk_Statements(pu, F90_Move_Transformationals);
      TRACE_AFTER(TRACE_TRANSFORMATIONALS,"Transformational motion");

      F90_Walk_All_Statements(pu, F90_Insert_All_Prelists);
      TRACE_AFTER(TRACE_INSERTIONS,"Extra statement insertions");

      /* Temp merging, and insert all the allocates and deallocates */
      if (temp_allocations_inserted) {
	 F90_Walk_All_Statements(pu, F90_Insert_Temp_Allocations);
	 temp_allocations_inserted = FALSE;
      }

      /* Generate the loops and do the inlining of intrinsics which need it */
      F90_Walk_Statements(pu,F90_Generate_Loops);
      if (temp_allocations_inserted) {
	 F90_Walk_All_Statements(pu, F90_Insert_Temp_Allocations);
	 temp_allocations_inserted = FALSE;
      }
      TRACE_AFTER(TRACE_DOLOOPS,"Do loop creation");
   }
   
   /* Lower special scalar intrinsics CHAR and MERGE, and generate the reference for by
      reference intrinsics, and fix up the random intrinsics */
   F90_Walk_All_Statements(pu,F90_Lower_Intrinsic_Fixup);


   /* Turn the allocs and deallocs into something the rest of the compiler understands */
   F90_Walk_Statements(pu,F90_Lower_Alloc_Dealloc);

#ifdef DUPCHECK
   check_for_duplicates(pu,"after");
#endif
   
   if (Get_Trace ( TKIND_IR, TP_LOWER90)) {
      fprintf(TFile,"\n\n========== Dump after F90 Lowering ==========\n");
      fdump_tree(TFile,pu);
   }
   if (Get_Trace(TKIND_SYMTAB,TP_LOWER90)) {
      fprintf(TFile,"\n\n========== Symbol tables after F90 Lowering ==========\n");
      Print_symtab (TFile, GLOBAL_SYMTAB);
      Print_symtab (TFile, CURRENT_SYMTAB);
   }
   
   F90_Lower_Term();
   
   return (pu);
}
