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

/*
 ** these keep wtable.h happy
 */
#define EXTERN  extern
#include "wtable.h"

#include "f90_utils.h"

#define F90_LOWER_INTERNAL
#include "f90_lower.h"

/* Useful macros */
#define is_constant(x) (WN_operator(x)==OPR_INTCONST)

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
static INT    INTRNalloca,INTRNfree,INTRNmalloc,INTRNgetstack,INTRNsetstack;
static TYPE_ID doloop_ty;
static INT num_temps = 0;
static BOOL pointer8 = 0;
#define SELECT_OP(x,y) (pointer8 ? x : y)
static PREG_NUM pointer_return_reg;
static TY *char_ty; /* A TY for character*1 */
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
   

static BOOL F90_Walk_Statements_Helper(WN * tree, WN * block,
				       BOOL prewalk(WN * node, WN *block),
				       BOOL walk_scf)
{
   OPCODE op;
   WN *callblock;
   WN *node,*nextnode;
   BOOL keep_going = TRUE;
   INT i,numkids;

   op = WN_opcode(tree);
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
	 if (!keep_going) return (keep_going);
      }
      numkids = WN_kid_count(tree);
      for (i=0; i < numkids; i++) {
	 keep_going = F90_Walk_Statements_Helper(WN_kid(tree,i), block, prewalk, walk_scf);
	 if (!keep_going) break;
      }
   } else if (OPCODE_is_stmt(op) || op == OPC_WHERE) {
      keep_going = do_prewalk(tree,block,prewalk);
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

/* Memory management */
static MEM_POOL f90_lower_pool_s;  /* Place to put auxilliary data structures */
static MEM_POOL *f90_lower_pool=NULL;

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
   Get_Return_Mtypes(Be_Type_Tbl(Pointer_type), Use_Simulated, &mtype1, &mtype2);
   Get_Return_Pregs(mtype1, mtype2, &pointer_return_reg, &rreg2);

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
   char_ty = NULL;
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

static char * create_tempname(char * name)
{
   static char buf[64];
   num_temps += 1;
   sprintf(buf,"%s_%d",name,num_temps);
   return(buf);
}


/*
 * F90_Lower_Create_Temp - Create the space for an array-valued or scalar temporary
 * 
 * 
 * ST * F90_Lower_Create_Temp(WN **alloc_block, WN **free_block, WN **size, INT ndim, TY *ty)
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
                                  INT ndim, TY *ty,
                                  WN *element_size)
{
   INT i;
   WN *total_size,*save_temp;
   ST *st;
   TY *pty;
   OPCODE callop;
   WN *call;
   BOOL allocate_on_stack = TRUE;
   INTRINSIC aintrin,fintrin;
   
   /* Create a stack temp symbol */
   st = New_ST(FALSE);
   ST_name(st) = Save_Str(create_tempname("@f90"));
   ST_class(st) = CLASS_VAR;
   Set_ST_sclass(st, SCLASS_AUTO);

   if (ndim > 0) {
      pty = Make_Pointer_Type(ty,FALSE);
      ST_type(st) = pty;
      Set_ST_pt_to_unique_mem(st);
      Set_ST_is_not_aliased(st);
   } else {
      ST_type(st) = ty;
   }
   Enter_ST(st);
   if (ndim == 0) {
      return (st);
   }

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
         total_size = WN_COPY_Tree(TY_AR_ubnd_tree(ty,0));
      }
   }
   for (i=0; i < ndim; i++) {
      total_size = WN_CreateExp2(OPCmpy,total_size,WN_COPY_Tree(size[i]));
   }
   
   if (allocate_on_stack) {
      aintrin = INTRN_F90_STACKTEMPALLOC;
      fintrin = INTRN_F90_STACKTEMPFREE;
   } else {
      aintrin = INTRN_F90_HEAPTEMPALLOC;
      fintrin = INTRN_F90_HEAPTEMPFREE;
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
   
   if (callop==OPCODE_UNKNOWN) callop = OPCODE_make_op(OPR_INTRINSIC_CALL, Pointer_type, MTYPE_V);

   opr = WN_operator(stmt);

   if (opr == OPR_INTRINSIC_CALL) {
      intr = WN_GET_INTRINSIC(stmt);
      if (intr == INTRN_F90_HEAPTEMPFREE) {
	 WN_intrinsic(stmt) = INTRNfree;
      } else if (intr == INTRN_F90_STACKTEMPFREE) {
	 /* Find the corresponding Saved SP and just replace in the tree */
	 WN_intrinsic(stmt) = INTRNsetstack;
	 k0 = WN_kid0(WN_kid0(stmt));
	 /* This had better be an LDID */
	 DevAssert((WN_operator(k0)==OPR_LDID),("Lower_Alloc_Dealloc saw something bad"));
	 free_preg = get_corresponding_sp(WN_st(k0),WN_offset(k0));
	 WN_DELETE_Tree(k0);
	 k0 = WN_LdidPreg(Pointer_type,free_preg);
	 WN_kid0(WN_kid0(stmt)) = k0;
      }
   } else if (opr == OPR_STID) {
      k0 = WN_kid0(stmt);
      if (WN_operator(k0) == OPR_INTRINSIC_OP) {
	 if (WN_GET_INTRINSIC(k0) == INTRN_F90_HEAPTEMPALLOC) {
	    /* Insert an INTRINSIC call before this statement */
	    k0k0 = WN_kid0(k0);
	    call = WN_Create_Intrinsic(callop, INTRNmalloc, 1, &k0k0);
	    WN_INSERT_BlockBefore(block,stmt,call);
	    WN_Delete(k0);
	    WN_kid0(stmt) = WN_LdidPreg(Pointer_type,pointer_return_reg);
	 } else if (WN_GET_INTRINSIC(k0) == INTRN_F90_STACKTEMPALLOC) {
	    Set_SYMTAB_has_alloca(Current_Symtab);
	    /* First, need to save the stack pointer */
	    sp_tmp = Create_Preg(Pointer_type,create_tempname("@f90sp"),NULL);
	    call = WN_Create_Intrinsic(callop,INTRNgetstack,0,NULL);
	    WN_INSERT_BlockBefore(block,stmt,call);
	    
	    save_sp = WN_LdidPreg(Pointer_type,pointer_return_reg);
	    save_sp = WN_StidIntoPreg(Pointer_type,sp_tmp,MTYPE_To_PREG(Pointer_type), save_sp);
	    WN_INSERT_BlockBefore(block,stmt,save_sp);
	    
	    k0k0 = WN_kid0(k0);
	    call = WN_Create_Intrinsic(callop,INTRNalloca,1,&k0k0);
	    WN_INSERT_BlockBefore(block,stmt,call);
	    WN_Delete(k0);
	    WN_kid0(stmt) = WN_LdidPreg(Pointer_type,pointer_return_reg);
	    
	    /* enter into correspondence tables */
	    add_alloca_correspondence(WN_st(stmt),WN_offset(stmt), sp_tmp);
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
   TY *temp_ty,*ptr_ty;
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

   /* First, determine the type of the expression */
   is_mexpr = FALSE;
   is_char = FALSE;
   mload_size = NULL;

   expr_op = WN_opcode(expr);
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
      temp_ty = TY_AR_etype(temp_ty);
      is_char = TY_is_character(temp_ty);
      /* Need to make pointer type */
      ptr_ty = Make_Pointer_Type(temp_ty,FALSE);
      element_size = TY_size(temp_ty);
      if (element_size == 0) {
	 element_size = -1;
	 multiply_indices = TRUE;
	 mload_size = WN_COPY_Tree(TY_AR_ubnd_tree(temp_ty,0));
      } else {
	 mload_size = WN_CreateIntconst(OPCint,element_size);
      }
   } else {
      temp_ty = Be_Type_Tbl(OPCODE_rtype(expr_op));
      element_size = TY_size(temp_ty);
      ptr_ty = Make_Pointer_Type(temp_ty,FALSE);
   }

   temp_st = F90_Lower_Create_Temp(alloc_block,free_block,size,ndim,temp_ty,mload_size);
   
   /* Create the ARRSECTION node referring to the temp */
   arrsection = WN_Create(OPCarrsection,2*ndim+1);
   /* Reset ptr_ty to get rid of the f90_pointer attribute */
   if (TY_is_f90_pointer(ptr_ty)) {
      temp_ty = TY_pointed(ptr_ty);
      ptr_ty = Make_Pointer_Type(temp_ty,TY_is_global(temp_ty));
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
      TYPE_ID type = OPCODE_rtype(expr_op);
      
      store = WN_Istore(type,(WN_OFFSET) 0, ptr_ty, arrexp, expr);
      load = WN_RIload(type,type,(WN_OFFSET) 0, ptr_ty, WN_COPY_Tree(arrsection));
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
   TY *temp_ty,*ptr_ty;
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
      ptr_ty = Make_Pointer_Type(temp_ty,TY_is_global(temp_ty));
      temp_st = F90_Lower_Create_Temp(NULL,NULL,NULL,0,temp_ty,NULL);
      Set_ST_addr_used_locally(temp_st);
      lda = WN_Lda(Pointer_type,(WN_OFFSET) 0, temp_st);
      *copy_store = WN_CreateMstore((WN_OFFSET) 0, ptr_ty, expr, lda, WN_COPY_Tree(WN_kid1(expr)));
      load = WN_CreateMload((WN_OFFSET) 0, ptr_ty, WN_COPY_Tree(lda),WN_COPY_Tree(WN_kid1(expr)));
   } else {
      type = OPCODE_rtype(expr_op);
      p = Create_Preg(type,create_tempname("@f90s"),NULL);
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
   TY *ptr_ty;
   TY *ty;
   WN *lda;
   WN *store;
   WN *val;

 
   if ((WN_flag(wn) & WN_PARM_BY_REFERENCE) != 0) return;

   /* Turn the PARM node into a by reference PARM node */
   ty = WN_ty(wn);
   WN_set_flag(wn, WN_PARM_BY_REFERENCE);
   WN_set_opcode(wn,OPCODE_make_op(OPR_PARM,Pointer_Mtype,MTYPE_V));

   st = New_ST(FALSE);
   ST_name(st) = Save_Str(create_tempname("@f90_reftemp"));
   ST_class(st) = CLASS_VAR;
   Set_ST_sclass(st, SCLASS_AUTO);
   ST_type(st) = ty;
   Set_ST_addr_taken_passed(st);
   Set_ST_addr_used_locally(st);
   Enter_ST(st);
   
   ptr_ty = Make_Pointer_Type(ty, TY_is_global(ty));
   WN_set_ty(wn,ptr_ty);
   lda = WN_CreateLda(OPCODE_make_op(OPR_LDA,Pointer_Mtype,MTYPE_V),0,ptr_ty,st);
   val = WN_kid0(wn);
   WN_kid0(wn) = lda;
   store = WN_Stid(TY_btype(ty),(WN_OFFSET) 0,st,ty,val);

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

   Get_Return_Pregs(rt,MTYPE_V, &rreg1, &rreg2);
   /* Change into an INTRINSIC_CALL, then return the load of the PREG */
   WN_DELETE_Tree(rcall);
   rcall = WN_Create_Intrinsic(OPCODE_make_op(OPR_INTRINSIC_CALL,rt,MTYPE_V),intr,0,NULL);
   WN_INSERT_BlockBefore(block, insert_point, rcall);
   rpreg = Create_Preg(rt,create_tempname("@f90ran"),NULL);
   wn = WN_StidPreg(rt,rpreg,WN_LdidPreg(rt,rreg1));
   WN_INSERT_BlockBefore(block, insert_point, wn);
   wn = WN_LdidPreg(rt,rpreg);
   return (wn);
}


/* Lower the CHAR intrinsic */
static WN * lower_char (WN *ival,WN *block, WN *insert_point)
{
   ST *st;
   TY *ptr_ty;
   WN *lda;
   WN *istore;

   /* Build up the ty for the character temp if needed */
   if (!char_ty) {
      char_ty  = New_TY (FALSE) ; 
      
      TY_size(char_ty)  = 1 ;
      TY_align(char_ty) = 1 ;
      TY_btype(char_ty) = MTYPE_I1;
      TY_kind(char_ty)  = KIND_SCALAR;
      TY_name(char_ty)  = Save_Str(".character.");
      
      Set_TY_is_character(char_ty);
      Enter_TY (char_ty);
   }
   st = New_ST(FALSE);
   ST_name(st) = Save_Str("@f90_chartemp");
   ST_class(st) = CLASS_VAR;
   Set_ST_sclass(st, SCLASS_AUTO);
   ST_type(st) = char_ty;
   Set_ST_addr_used_locally(st);
   Enter_ST(st);
   
   ptr_ty = Make_Pointer_Type(char_ty, TY_is_global(char_ty));
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
   TY *ptr_ty;
   TY *temp_ty;
   ST *temp_st;
   WN *lda;
   WN *size;
   WN *true_block,*false_block;
   WN *store;
   WN *result;
   
   condition = WN_kid0(kids[0]);
   t_case = WN_kid0(kids[1]);
   f_case = WN_kid0(kids[2]);
   WN_Delete(kids[0]);
   WN_Delete(kids[1]);
   WN_Delete(kids[2]);

   /* t_case and f_case had better be MLOADs at this point */
   FmtAssert((WN_opcode(t_case) == OPC_MLOAD),("Expected an MLOAD node"));
   FmtAssert((WN_opcode(f_case) == OPC_MLOAD),("Expected an MLOAD node"));

   ptr_ty = WN_ty(t_case);
   /* Strip off the "pointer to" part */
   temp_ty = TY_pointed(ptr_ty);
   temp_st = F90_Lower_Create_Temp(NULL,NULL,NULL,0,temp_ty,NULL);
   Set_ST_addr_used_locally(temp_st);
   lda = WN_Lda(Pointer_type,(WN_OFFSET) 0, temp_st);
   size = WN_kid1(t_case);

   true_block = WN_CreateBlock();
   false_block = WN_CreateBlock();

   store = WN_CreateMstore((WN_OFFSET) 0, ptr_ty, t_case, WN_COPY_Tree(lda), WN_COPY_Tree(size));
   WN_INSERT_BlockFirst(true_block,store);

   store = WN_CreateMstore((WN_OFFSET) 0, ptr_ty, f_case, WN_COPY_Tree(lda), WN_COPY_Tree(size));
   WN_INSERT_BlockFirst(false_block,store);

   /* Put the if in the block */
   store = WN_CreateIf(condition,true_block,false_block);
   WN_INSERT_BlockBefore(block,insert_point,store);
   
   if (TY_is_character(temp_ty)) {
      result = lda;
   } else {
      result = WN_CreateMload((WN_OFFSET) 0, ptr_ty, lda, WN_COPY_Tree(size));
   }
   return (result);
}

static void F90_Lower_Intrinsic_Fixup_walk(WN *expr, WN *stmt, WN *block) 
{
   OPERATOR opr;
   WN *kid;
   WN *newkid;
   INT i,j,numkids;
   INTRINSIC intr;
   
   opr = WN_operator(expr);
   
   if (opr == OPR_BLOCK) {
      kid = WN_first(expr);
      while (kid) {
	 F90_Lower_Intrinsic_Fixup_walk(kid, kid, expr);
	 kid = WN_next(kid);
      }
   } else {
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
	       if (!INTR_by_value(intr)) {
		  for (j=0; j < WN_kid_count(kid); j++) {
		     convert_to_reference(WN_kid(kid,j),block,stmt);
		  }
	       }
	       break;
	    }
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
   static char * summ[4] = {"UNK","IND","===","REM"};
   static char * dirr[5] = {"/","+","-","0","?"};
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

static char * f90_depsum_name(DEP_SUMMARY d)
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
static TY *lhs_ty;

/* Information for the RHS symbol (since this is going to be used multiple times) */
static ST *rhs_sym;
static WN *rhs_pieces[MAX_ADDR_PIECES];
static INT rhs_num_pieces;
static INT64 rhs_const_offset,rhs_base_offset,rhs_size;
static INT rhs_flag;
static TY *rhs_ty;


static BOOL is_f90_pointer(WN *addr)
{
   OPERATOR opr;
   ST *st;
   opr = WN_operator(addr);
   if (opr == OPR_LDID || opr == OPR_LDA) {
      st = WN_st(addr);
      if (ST_class(st) == CLASS_VAR) {
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
      if (ST_class(st) == CLASS_VAR) {
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
				INT *flag, TY **ty)
{
   OPERATOR opr,opr1;
   ST *s;
   TY *t;
   WN *addr;
   BOOL done;
   WN *kid;
   
   *base_offset = 0;
   *size = 0;
   *sym = NULL;
   *num_pieces = 0;
   *flag = 0;
   *ty = NULL;
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
	 if (ST_class(s) == CLASS_CONST) {
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
	    if (ST_pt_to_unique_mem(s) || ST_is_not_aliased(s)) {
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
	 if (ST_class(s) == CLASS_CONST) {
	    *flag = F90_UNALIASED;
	    /* Early exit */
	    return;
	 } 
	 if (ST_sclass(s) == SCLASS_BASED) {
	    Base_Symbol_And_Offset(s,sym,base_offset);
	    *flag |= F90_BASED;
	    *size = TY_size(t);
	 } 
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
	    *flag |= is_f90_pointer(addr);
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
   ty1 = MTYPE_size_reg(OPCODE_rtype(WN_opcode(t1)));
   ty2 = MTYPE_size_reg(OPCODE_rtype(WN_opcode(t2)));
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
	    temp = WN_CreateExp2(OPC_I8LT,WN_COPY_Tree(lb1),WN_Zerocon(MTYPE_I8));
	    temp1 = WN_CreateExp2(OPC_I8GE,WN_COPY_Tree(lb1),WN_COPY_Tree(ex1));
	    temp = WN_CreateExp2(OPC_LIOR,temp,temp1);
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
*
* Try to figure out if two bases might overlap. It returns either
* DEP_UNKNOWN - Can't figure it out
* DEP_INDEPENDENT - the bases are different and non-overlapping
* DEP_IDENTICAL - the bases are the same thing
*
*/

static DEP_SUMMARY Analyze_bases(WN * addr, BOOL is_expr)
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
      
      r = analyze_structure_bases(lhs_num_pieces-1,rhs_num_pieces-1,NULL,NULL);
      if (trace_dependence) fprintf(TFile,"no indirection, same symbol %s\n",f90_depsum_name(r));
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
      if ((l_pointer && r_target) ||
	  (r_pointer && l_target) || 
	  (r_pointer && l_pointer)) {
	 if (Alias_F90_Pointer_Unaliased) {
	    r = DEP_INDEPENDENT;
	 } else {
	    r = DEP_UNKNOWN;
	 }
      } else if (is_f90_formal(lhsa) || is_f90_formal(rhsa)) {
	 r = DEP_INDEPENDENT;
      } else {
	 /* Need to check for the case where we have only one indirection */
	 if ((rhs_flag ^ lhs_flag) & F90_INDIRECTION) {
	    /* Only one, but not both */
	    r = DEP_INDEPENDENT;
	 } else if ((rhs_flag | lhs_flag) & F90_UNALIASED) { 
	    r = DEP_INDEPENDENT;
	 } else {
	    r = DEP_UNKNOWN;
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
 *                 BOOL transformational_seen, WN *parent, INT kidno, INT lhsdim)
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
 *
 */
static void Dependence_Walk(WN *expr,F90_LOWER_AUX_DATA *adata, DEP_INFO *dep, 
			    BOOL transformational_seen,
			    WN *parent, INT kidno, INT lhsdim)
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
   } else if (opr == OPR_ILOAD || opr == OPR_MLOAD) {
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
      rhs_const_offset = WN_offset(expr);
      Analyze_one_address(kid,&rhs_sym,rhs_pieces,&rhs_num_pieces,&rhs_const_offset,
		       &rhs_base_offset,&rhs_size,&rhs_flag,&rhs_ty);
      base_dep = Analyze_bases(expr,FALSE);
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
      Dependence_Walk(kid, adata, &kid_dep, transformational_seen,expr,i,lhsdim);
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
   Dependence_Walk(rhs,adata,&dep,FALSE,stmt,0,0);

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
 * Exactly one assignment in either the true or false block.
 * BLOCK is the block containing STMT, so that other statements can be inserted.
 * 
 * It can also be any of the CHARACTER calls CONCAT, CASSIGNMENT, ADJUSTL, ADJUSTR
 ************************************************************************/

static void f90_analyze_assignment(WN *stmt, WN *block, BOOL is_call)
{
   F90_LOWER_AUX_DATA *adata;
   WN *lhs_arrayexp;
   INT ndim;
   INT lhsdim;
   INT i,num_kids;
   INT vec_axes,vec_axis_list[MAX_NDIM],axis;
   WN *assignment;
   WN *lhs,*rhs;
   WN *count,*copy_store;
   DEP_INFO lhs_dep,rhs_dep;
   INTRINSIC intr;
   
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
      } else {
	 rhs = NULL; /* Don't do any dependence analysis, just set sizes */
      }
   } else {
      /* Set up a repository for the aux data */
      if (WN_opcode(stmt) != OPC_WHERE) {
	 lhs_arrayexp = WN_kid1(stmt);
	 ndim = WN_kid_count(lhs_arrayexp) - 1;
	 assignment = stmt;
      } else if (WN_opcode(stmt) == OPC_WHERE) {
	 /* Each WHERE contains a single assignment, we just need to find it */
	 lhs_arrayexp = WN_kid0(stmt);
	 ndim = WN_kid_count(lhs_arrayexp)-1;
	 assignment = get_assignment_from_stmt(stmt);
	 lhs_arrayexp = WN_kid1(assignment);
      } 
      rhs = WN_kid0(assignment);
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
	 Dependence_Walk(WN_kid(lhs_arrsection,i),adata,&lhs_dep,FALSE,lhs_arrsection,i,lhsdim);
      }
      
      if (DEP_SUMMARY(&lhs_dep) != DEP_INDEPENDENT) {
	 /* Must mark this as COPY_LEFT */
	 SET_COPY_FLAG(adata,COPY_LEFT | COPY_FLAG(adata));
      }

      /* Need to check for appearence in the ARRAYEXP portion of the tree as well */
      for (i=1; i <= ndim ; i++) {
	 Dependence_Walk(WN_kid(lhs_arrayexp,i),adata,&lhs_dep,FALSE,lhs_arrayexp,i,lhsdim);
      }
      
      /* Do the LHS/RHS walk, to see if we need to do a COPY_RIGHT */
      Dependence_Walk(rhs,adata,&rhs_dep,FALSE,assignment,0,lhsdim);
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
      WN_kid0(WN_kid0(assignment)) = WN_kid0(lhs_arrayexp);
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
   TY *ty;
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
	 /* It's a CASSIGNSTMT statement, create an MLOAD of the RHS (kid1), and move
	  * it to a temp.
	  */
	 copy_adata = F90_Lower_Copy_Aux_Data(adata);
	 rhs = WN_kid0(WN_kid1(assignment));
	 ty = WN_ty(WN_kid0(assignment));
	 rhs = WN_CreateMload(0,ty,rhs,WN_COPY_Tree(WN_kid0(WN_kid(assignment,3)))); 
	 new_rhs = F90_Lower_Copy_To_ATemp(&ALLOC_PRELIST(copy_adata), &DEALLOC_POSTLIST(adata),
					   &copy_store,rhs,
					   ITER_COUNT_PTR(adata), NDIM(adata));
	 /* Get rid of the MLOAD and its length argument */
	 WN_kid0(WN_kid1(assignment)) = WN_kid0(new_rhs);
	 WN_DELETE_Tree(WN_kid1(new_rhs));
	 WN_Delete(new_rhs);
	 SET_F90_MAP(copy_store,copy_adata);
	 WN_INSERT_BlockBefore(block,stmt,copy_store);
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
   OPERATOR opr;
   BOOL walking_where;

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
    case INTRN_SPREAD:
      /* Special case for spread, only move child if required  */
      kid = WN_kid0(WN_kid0(t)); /* Get through the PARM node */
      if (WN_operator(kid) != OPR_ILOAD && WN_operator(kid) != OPR_MLOAD &&
	  WN_operator(kid) != OPR_LDID &&
	  (WN_operator(kid) != OPR_INTRINSIC_OP && WN_GET_INTRINSIC(kid) != INTRN_SPREAD )) {
	 /* Move the expression to a temporary */
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
 * SPREAD (sometimes), PACK, UNPACK, EOSHIFT, MAXLOC, MINLOC
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
   
   /* Create an index */
   *index = Create_Preg(index_type,Save_Str(index_name),NULL);

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

   WN_Set_Linenum(start,current_srcpos);
   WN_Set_Linenum(step,current_srcpos);
   doloop = WN_CreateDO(index_id, start, end, step, temp, NULL);
   WN_Set_Linenum(doloop,current_srcpos);
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
   accum = Create_Preg(ty,create_tempname("@f90acc"),NULL);
   
   reduction_op = OPCODE_make_op(reduction_opr,ty,MTYPE_V);
   idty_store = WN_StidPreg(ty,accum,Make_Reduction_Identity(reduction_opr,rty));

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
      WN_INSERT_BlockBefore(F90_Current_Block,F90_Current_Loopnest,loopnest);
      /* Update the Current_Loopnest, to point at the identity store
	 so that all lower-level insertions happen there */
      F90_Current_Loopnest = loopnest;
      
      /* put the DEALLOC_POSTLIST and POSTLIST on the current loopnest */
      if (adata) {
	 adata = F90_Lower_New_Aux_Data(0);
	 SET_POSTLIST(adata,post);
	 SET_DEALLOC_POSTLIST(adata,dealloc_post);
	 SET_F90_MAP(loopnest,adata);
      }

      /* Lower the mask and array argument */
      accum_expr = F90_Lower_Walk(kids[0],new_indices,rank,stlist,NULL);
      accum_store = WN_StidPreg(ty,accum,WN_CreateExp2(reduction_op,
						       WN_LdidPreg(ty,accum),
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
      dim = ndim + 2 - dim; /* account for ordering */
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
      accum_store = WN_StidPreg(ty,accum,WN_CreateExp2(reduction_op,
						       WN_LdidPreg(ty,accum),
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
   result = WN_LdidPreg(ty,accum);
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
   TY * retty, *retty_ptr;
   TYPE_ID expr_ty;

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
   expr_ty = OPCODE_rtype(WN_opcode(kids[0]));
   accum = Create_Preg(expr_ty,create_tempname("@f90acc"),NULL);
   cur_val = Create_Preg(expr_ty,create_tempname("@f90accval"),NULL);
   if (reduction_opr == OPR_MAX) {
      reduction_op = OPCODE_make_op(OPR_GT,MTYPE_I4,expr_ty);
   } else {
      reduction_op = OPCODE_make_op(OPR_LT,MTYPE_I4,expr_ty);
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
	 loopnest = create_doloop(&index,tempname,sizes[i],DIR_POSITIVE,loopnest);
	 new_indices[i] = index;
      }
      WN_INSERT_BlockBefore(F90_Current_Block,F90_Current_Loopnest,loopnest);
      /* Update the Current_Loopnest, to point at the identity store
	 so that all lower-level insertions happen there */
      F90_Current_Loopnest = idty_store;
      
      /* Create a temp to hold the result. We will allocate this temp on the stack, 
       * but not dynamically 
       */
      retval = New_ST(FALSE);
      ST_name(retval) = Save_Str(create_tempname("@f90mmloc"));
      ST_class(retval) = CLASS_VAR;
      Set_ST_sclass(retval, SCLASS_AUTO);
      retty = Make_Array_Type(doloop_ty,1,rank,FALSE);
      retty_ptr = Make_Pointer_Type(retty,TY_is_global(retty));
      ST_type(retval) = retty;
      Set_ST_addr_used_locally(retval);
      Enter_ST(retval);
      
      /* Lower the mask and array argument */
      accum_expr = F90_Lower_Walk(kids[0],new_indices,rank,stlist,NULL);
      accum_store = WN_StidPreg(expr_ty,cur_val,accum_expr);
      WN_INSERT_BlockFirst(stlist,accum_store);
      
      comp_expr = WN_CreateExp2(reduction_op,WN_LdidPreg(expr_ty,cur_val),
				WN_LdidPreg(expr_ty,accum));
      if (Mask_Present) {
	 mask_expr = F90_Lower_Walk(kids[2],new_indices,rank,stlist,NULL);
	 mask_expr = WN_CreateExp2(OPC_LAND,mask_expr,comp_expr);
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
	 accum_store = WN_Istore(doloop_ty, (WN_OFFSET) 0, retty_ptr, WN_COPY_Tree(result), 
				 WN_LdidPreg(doloop_ty,new_indices[rank-i-1]));
	 
	 WN_INSERT_BlockLast(accum_block,accum_store);
	 /* Initialize the return value to 0 */
	 accum_store = WN_Istore(doloop_ty, (WN_OFFSET) 0, retty_ptr,result, 
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
      result = WN_Iload(doloop_ty,0,retty,result);
      result = WN_CreateExp2(OPCODE_make_op(OPR_ADD,doloop_ty,MTYPE_V),result,
			     WN_Intconst(doloop_ty,1));

      /* The result is the lowering of the created temp with the right index */
   } else {
      /* New F95/HPF maxloc along a dimension */

      /* All cases, create a temp and insert an identity store */
      red_index = Create_Preg(doloop_ty,create_tempname("@f90redindex"),NULL);
      red_store = WN_StidPreg(doloop_ty,red_index,WN_Intconst(doloop_ty,-1));
      
      /* Determine the sizes of reduction argument */
      (void) F90_Size_Walk(kids[0],&rank,sizes);
      loopnest = WN_CreateBlock();
      stlist = loopnest;
      
      WN_INSERT_BlockBefore(block,insert_point,idty_store);
      WN_INSERT_BlockBefore(block,insert_point,red_store);
      
      /* Create a single loopnest */
      num_temps += 1;
      dim = ndim + 2 - dim; /* account for ordering */
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
      accum_expr = F90_Lower_Walk(kids[0],new_indices,rank,stlist,NULL);
      accum_store = WN_StidPreg(expr_ty,cur_val,accum_expr);
      WN_INSERT_BlockFirst(stlist,accum_store);

      comp_expr = WN_CreateExp2(reduction_op,WN_LdidPreg(expr_ty,cur_val),
				WN_LdidPreg(expr_ty,accum));
      if (Mask_Present) {
	 mask_expr = F90_Lower_Walk(kids[2],new_indices,rank,stlist,NULL);
	 mask_expr = WN_CreateExp2(OPC_LAND,mask_expr,comp_expr);
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
   pack_index = Create_Preg(doloop_ty,create_tempname("@f90pack"),NULL);
   
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
   TY *ptr_ty,*temp_ty;
   WN *lda;
   ST *temp_st;
   TYPE_ID type;
   WN *true_store;
   WN *false_store;
   WN *load;

   /* Create a scalar counter and intialize it */
   pack_index = Create_Preg(doloop_ty,create_tempname("@f90pack"),NULL);
   
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
      Set_ST_addr_used_locally(temp_st);
      lda = WN_Lda(Pointer_type,(WN_OFFSET) 0, temp_st);
      true_store = WN_CreateMstore((WN_OFFSET) 0, ptr_ty, vector, WN_COPY_Tree(lda),
				   WN_COPY_Tree(WN_kid1(vector)));
      false_store = WN_CreateMstore((WN_OFFSET) 0, ptr_ty, field, WN_COPY_Tree(lda), 
				    WN_COPY_Tree(WN_kid1(vector)));
      load = WN_CreateMload((WN_OFFSET) 0, ptr_ty, lda,WN_COPY_Tree(WN_kid1(vector)));
   } else {
      type = OPCODE_rtype(WN_opcode(vector));
      p = Create_Preg(type,create_tempname("@f90s"),NULL);
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
   l1 = Create_Preg(doloop_ty,create_tempname("@f90_eos_l1"),NULL);
   u1 = Create_Preg(doloop_ty,create_tempname("@f90_eos_u1"),NULL);
   l2 = Create_Preg(doloop_ty,create_tempname("@f90_eos_l2"),NULL);
   u2 = Create_Preg(doloop_ty,create_tempname("@f90_eos_u2"),NULL);

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

   new_index = Create_Preg(doloop_ty,create_tempname("@f90_eos_idx"),NULL);
   shift_index = Create_Preg(doloop_ty,create_tempname("@f90_eos_shft"),NULL);
   
   
   start = WN_StidPreg(doloop_ty,new_index,WN_LdidPreg(doloop_ty,l1));
   end = WN_CreateExp2(OPCODE_make_op(OPR_LE,MTYPE_I4,doloop_ty),
		       WN_LdidPreg(doloop_ty,new_index),
		       WN_LdidPreg(doloop_ty,u1));
   step = WN_StidPreg(doloop_ty,new_index,
		      WN_CreateExp2(OPCadd,WN_LdidPreg(doloop_ty,new_index),
				    WN_Intconst(doloop_ty,1)));
   index_id = WN_CreateIdname(new_index,MTYPE_To_PREG(doloop_ty));
   

   body = WN_CreateBlock();
   temp = WN_CreateDO(index_id, start, end, step, body, NULL);
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
   temp = WN_CreateDO(WN_COPY_Tree(index_id), start, end, step, body, NULL);
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
   shift = WN_CreateExp2(OPCmod,shift,WN_COPY_Tree(extent));

   /* Check for 0 shift */
   if (WN_operator(shift)==OPR_INTCONST && WN_const_val(shift) == 0) {
      WN_DELETE_Tree(shift);
      WN_DELETE_Tree(extent);
      new_indices[dim] = indices[dim];
   } else {
      shifted_index = Create_Preg(doloop_ty,create_tempname("@f90cshift"),NULL);
      shift_preg = Create_Preg(doloop_ty,create_tempname("@f90cshiftp"),NULL);
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
   ty = OPCODE_rtype(WN_opcode(expr));
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
    case INTRN_MATMUL:
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
      index_ldid = WN_LdidPreg(ty,indices[0]);
      result = WN_CreateExp2(OPCODE_make_op(OPR_ADD,ty,MTYPE_V),
			     kid,
			     WN_CreateExp2(OPCODE_make_op(OPR_MPY,ty,MTYPE_V),
					   index_ldid,
					   kid1));
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


   /* Don't walk I/O statements */
   if (WN_operator(stmt) == OPR_IO) {
      return(TRUE);
   }

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

static void check_for_duplicates(WN *pu,char *str)
{
   /* Set up a parent map */
   static WN_MAP f90_parent_map;
   WN_ITER *ti;
   WN *w, *k, *p;
   INT i;
   BOOL found_dup = FALSE;

   f90_parent_map = WN_MAP_Create(f90_lower_pool);

   /* Walk everything */
   ti = WN_WALK_TreeIter(pu);
   while (ti) {
      w = WN_ITER_wn(ti);
      /* look at all the kids */
      for (i=0; i < WN_kid_count(w) ; i++) {
	 k = WN_kid(w,i);
	 p = GET_P_MAP(k);
	 if ((p != NULL) && (p != w)) {
	    fprintf(TFile,"\n%s: Multiparented node p=%08x, w=%08x, k=%d\n",str,p,w,i);
	    fprintf(TFile,"parent:\n"); fdump_tree(TFile,p);
	    fprintf(TFile,"current:\n"); fdump_tree(TFile,w);
	    fprintf(TFile,"multichild:\n"); fdump_tree(TFile,k);
	    found_dup = TRUE;
	 } else {
	    SET_P_MAP(k,w);
	 }
      }
      ti = WN_WALK_TreeNext(ti);
   }
   WN_MAP_Delete(f90_parent_map);
   if (found_dup) {
      DevWarn(("Duplicate WHIRL nodes found %s\n"),str);
   }
}
#endif

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

WN * F90_Lower (WN *pu) {
   /* See if we are actually processing an F90 pu */
   if (!(SYMTAB_src_lang(Current_Symtab) & SYMTAB_F90_LANG)) return (pu);

   F90_Lower_Init();

   trace_dependence = Get_Trace(TP_LOWER90,TRACE_DEPENDENCE_ANALYSIS);
   trace_depinfo    = Get_Trace(TP_LOWER90,TRACE_DEPINFO);

   if (Get_Trace ( TKIND_IR, TP_LOWER90 )) {
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
   
   if (Get_Trace ( TKIND_IR, TP_LOWER90 )) {
      fprintf(TFile,"\n\n========== Dump after F90 Lowering ==========\n");
      fdump_tree(TFile,pu);
   }
   if (Get_Trace(TKIND_SYMTAB,TP_LOWER90)) {
      fprintf(TFile,"\n\n========== Symbol tables after F90 Lowering ==========\n");
      Trace_SYMTAB (TFile, Global_Symtab, TRUE);
      Trace_SYMTAB (TFile, Current_Symtab, TRUE);
   }
   
   F90_Lower_Term();
   
   return (pu);
}

