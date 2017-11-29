/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007. Pathscale, LLC. All Rights Reserved.
 */

/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
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


#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <bstring.h> /* has bcopy */
#include <sys/types.h>

#include "defs.h"
#include "stab.h"
#include "opcode.h"
#include "lwn_util.h"
#include "ir_reader.h" 
#include "wn_simp.h"
#include "fb_whirl.h"
#include "symtab_idx.h"

#ifdef LNO
#include "lnopt_main.h"
#include "dep_graph.h"
#include "opt_du.h"
#include "optimizer.h"
#include "name.h"
#include "region_util.h"
#include "lego_opts.h"
#include "call_info.h"
#ifdef __cplusplus
extern "C" {
#endif
// in region_util.cxx
void REGION_clone(WN *old_wn, WN *new_wn, WN *parent_region);
#ifdef __cplusplus
}
#endif
#endif

#include "strtab.h"     // for Save_Str
#include "config_asm.h" // for User_Label_Number_Format
#include "irbdata.h"    // for INITO/INITV stuff

#include "cxx_template.h"
#include "cxx_hash.h"

typedef STACK<WN *> STACK_OF_WN;

#ifdef KEY
extern INT Num_Prefetches;
#endif

/* Return next tree node in in-order traversal. 
 * Go all the way to the leaves.
 * Return NULL if no more nodes.
 */
extern WN* LWN_Get_Next_Tree_Node (const WN* wn) {
  WN* ret_wn = NULL;

  if (wn != NULL) {
    /* try to find the first kid */
    if (WN_opcode(wn) == OPC_BLOCK) {
      ret_wn = WN_first (wn);
    }
    else {
      if (WN_kid_count (wn) > 0) { 
        INT kid_no;
        /* find the first non-NULL kid */
        for (kid_no=0; kid_no<WN_kid_count(wn); kid_no++) {
          ret_wn = WN_kid (wn, kid_no);
          if (ret_wn != NULL) {
            break;
          }
          /* else, kid was NULL, so try the next kid. */
        }
      }
    }
    if (ret_wn == NULL) {
      /* couldn't find a kid, so go up to find a sibling */
      /* i.e. find the NEXT kid of the parent */
      WN* parent_wn = LWN_Get_Parent (wn);
      INT kid_no = 0;
      
      while (1) { 
        if (parent_wn == NULL) {
          /* reached the top of the tree */
          break;
        }
        
        Is_True (!(OPCODE_is_leaf (WN_opcode(parent_wn))),
                 ("parent 0x%p is a leaf!\n", parent_wn));
        if (WN_opcode(parent_wn) == OPC_BLOCK) {
          ret_wn = WN_next (wn); 
        }
        else {
          INT kidno;
          /* first find this kid */
          for (kidno=0; kidno<WN_kid_count(parent_wn); kidno++) {
            if (wn == WN_kid (parent_wn, kidno)) {
              break;
            }
          }
          Is_True ((kidno < WN_kid_count (parent_wn)),
                   ("kid 0x%p not a child of its parent 0x%p\n", wn, parent_wn));
          
          /* now find the next kid */
          kidno++;
          for (; kidno<WN_kid_count (parent_wn); kidno++) {
            ret_wn = WN_kid (parent_wn, kidno);
            if (ret_wn != NULL) {
              break;
            }
            /* else, kid was NULL, so keep going */
          }
        }
        
        if (ret_wn == NULL) {
          /* no more non-NULL kids; keep going up */
          wn = parent_wn;
          parent_wn = LWN_Get_Parent (wn);
          continue;
        }
        else break;
      }
    }
  }
  return ret_wn;
}


/* Return next stmt node in in-order traversal. 
 * Return NULL if no more stmt nodes.
 */
extern WN* LWN_Get_Next_Stmt_Node (const WN* wn) {
  WN* ret_wn = NULL;
  
  if (wn != NULL) {
    while (1) {
      
      /* try to find the first non-expr kid */
      if (WN_opcode(wn) == OPC_BLOCK) {
        ret_wn = WN_first (wn);
        while ((ret_wn) && (OPCODE_is_expression(WN_opcode(ret_wn)))) {
          ret_wn = WN_next (ret_wn);
        }
      }
      else {
        if (WN_kid_count (wn) > 0) { 
          INT kidno;
          /* find the first non-NULL and non-expr kid */
          for (kidno=0; kidno<WN_kid_count (wn); kidno++) {
            ret_wn = WN_kid (wn, kidno);
            if ((ret_wn != NULL) && (!OPCODE_is_expression(WN_opcode(ret_wn)))) {
              break;
            }
            /* else, kid was NULL or expr, so try the next kid. */
          }
        }
      }
      /* ret_wn could be non-NULL, but an expr */
      if ((ret_wn) && (OPCODE_is_expression(WN_opcode(ret_wn)))) {
        ret_wn = NULL;
      }
      
      if (ret_wn == NULL) {
        /* couldn't find a kid, so go up to find a sibling */
        /* i.e. find the NEXT kid of the parent */
        WN* parent_wn = LWN_Get_Parent (wn);
        INT kidno = 0;
        
        while (1) { 
          if (parent_wn == NULL) {
            /* reached the top of the tree */
            break;
          }
          
          if (WN_opcode(parent_wn) == OPC_BLOCK) {
            ret_wn = WN_next (wn); 
            while ((ret_wn) && (OPCODE_is_expression(WN_opcode(ret_wn)))) {
              ret_wn = WN_next (ret_wn);
            }
          }
          else {
            /* first find this kid */
            for (kidno=0; kidno<WN_kid_count(parent_wn); kidno++) {
              if (wn == WN_kid (parent_wn, kidno)) {
                break;
              }
            }
            Is_True ((kidno < WN_kid_count (parent_wn)),
                     ("kid 0x%p not a child of its parent 0x%p\n", wn, parent_wn));
            
            /* now find the next kid */
            kidno++;
            for (; kidno<WN_kid_count (parent_wn); kidno++) {
              ret_wn = WN_kid (parent_wn, kidno);
              if ((ret_wn != NULL) && (!OPCODE_is_expression(WN_opcode(ret_wn)))) {
                break;
              }
              /* else, kid was NULL or expr, so keep going */
            }
          }
          /* ret_wn could be non-NULL, but an expr */
          if ((ret_wn) && (OPCODE_is_expression(WN_opcode(ret_wn)))) {
            ret_wn = NULL;
          }
          
          if (ret_wn == NULL) {
            /* no more non-NULL kids; keep going up */
            wn = parent_wn;
            parent_wn = LWN_Get_Parent (wn);
            continue;
          }
          else break;
        }
      }
      if (ret_wn) {
        if (OPCODE_is_stmt(WN_opcode(ret_wn))) {
          break;
        }
        if (OPCODE_is_scf(WN_opcode(ret_wn))) {
          wn = ret_wn;
          ret_wn = NULL;
          continue;
        }
        Is_True (!(OPCODE_is_expression(WN_opcode(ret_wn))),
                 ("GetNextStmtNode: 0x%p trying to return an expression\n", ret_wn));
        if (OPCODE_is_expression(WN_opcode(ret_wn))) {
          printf ("GetNextStmtNode: 0x%p trying to return an expression\n", ret_wn);
          exit (1);
        }
      }
      else break;
    }
  }
  Is_True (((!ret_wn) || OPCODE_is_stmt(WN_opcode(ret_wn))),
           ("GetNextStmtNode: returning a non-stmt 0x%p\n", ret_wn));
  return ret_wn;
} /* GetNextStmtNode */


/* Return next SCF node in in-order traversal. 
 * Return NULL if no more SCF nodes.
 */
extern WN* LWN_Get_Next_SCF_Node (const WN* wn) {
  WN* ret_wn = NULL;
  
  if (wn != NULL) {
    /* try to find the first SCF kid */
    if (WN_opcode(wn) == OPC_BLOCK) {
      ret_wn = WN_first (wn);
      while ((ret_wn) && (!OPCODE_is_scf(WN_opcode(ret_wn)))) {
        ret_wn = WN_next (ret_wn);
      }
    }
    else {
      if (WN_kid_count (wn) > 0) { 
        INT kidno;
        /* find the first non-NULL and non-expr kid */
        for (kidno=0; kidno<WN_kid_count (wn); kidno++) {
          ret_wn = WN_kid (wn, kidno);
          if ((ret_wn != NULL) && (OPCODE_is_scf(WN_opcode(ret_wn)))) {
            break;
          }
          /* else, kid was NULL or not-scf, so try the next kid. */
        }
      }
    }
    /* ret_wn could be non-NULL, but not scf */
    if ((ret_wn) && (!OPCODE_is_scf(WN_opcode(ret_wn)))) {
      ret_wn = NULL;
    }
     
    if (ret_wn == NULL) {
      /* couldn't find a kid, so go up to find a sibling */
      /* i.e. find the NEXT kid of the parent */
      WN* parent_wn = LWN_Get_Parent (wn);
      INT kidno = 0;
      
      while (1) { 
        if (parent_wn == NULL) {
          /* reached the top of the tree */
          break;
        }
        
        if (WN_opcode(parent_wn) == OPC_BLOCK) {
          ret_wn = WN_next (wn); 
          while ((ret_wn) && (!OPCODE_is_scf(WN_opcode(ret_wn)))) {
            ret_wn = WN_next (ret_wn);
          }
        }
        else {
          /* first find this kid */
          for (kidno=0; kidno<WN_kid_count(parent_wn); kidno++) {
            if (wn == WN_kid (parent_wn, kidno)) {
              break;
            }
          }
          Is_True ((kidno < WN_kid_count (parent_wn)),
                   ("kid 0x%p not a child of its parent 0x%p\n", wn, parent_wn));
          
          /* now find the next kid */
          kidno++;
          for (; kidno<WN_kid_count (parent_wn); kidno++) {
            ret_wn = WN_kid (parent_wn, kidno);
            if ((ret_wn != NULL) && (OPCODE_is_scf(WN_opcode(ret_wn)))) {
              break;
            }
            /* else, kid was NULL or expr, so keep going */
          }
        }
        /* ret_wn could be non-NULL, but an expr */
        if ((ret_wn) && (!OPCODE_is_scf(WN_opcode(ret_wn)))) {
          ret_wn = NULL;
        }
        
        if (ret_wn == NULL) {
          /* no more non-NULL kids; keep going up */
          wn = parent_wn;
          parent_wn = LWN_Get_Parent (wn);
          continue;
        }
        else break;
      }
    }
  }
  Is_True (((!ret_wn) || (OPCODE_is_scf(WN_opcode(ret_wn)))), 
           ("GetNextSCFNode: trying to return a non-SCF 0x%p\n", ret_wn));
  return ret_wn;
} /* GetNextSCFNode */

void LWN_Insert_Block_Before (WN *block, WN *wn, WN *in)
{
  WN *first;
  WN *last;
  WN *w;

  Is_True(wn || block, ("LWN_Insert_Block_Before: nowhere to insert"));

  if (wn) {
    WN *parent_wn = LWN_Get_Parent(wn);
    Is_True (parent_wn, ("LWN_Insert_Block_Before: parent is NULL\n"));
  
    if (block == NULL)
      block = parent_wn;
    else {
      Is_True(block == parent_wn,
	      ("LWN_Insert_Block_Before: supplied block not parent"));
    }
  }

  Is_True(!block || (WN_opcode(block) == OPC_BLOCK),
	  ("LWN_Insert_Block_Before: block is not a BLOCK node"));
  Is_True(!wn || OPCODE_is_stmt(WN_opcode(wn)) || OPCODE_is_scf(WN_opcode(wn)),
	  ("LWN_Insert_Block_Before: insert node 0x%p not a stmt/scf\n", wn));
  Is_True((in != NULL),
	  ("LWN_Insert_Block_Before: insert a NULL node?"));
  Is_True(OPCODE_is_stmt(WN_opcode(in)) || OPCODE_is_scf(WN_opcode(in)), 
	  ("LWN_Insert_Block_Before: insert node 0x%p not a stmt/scf\n", in));

  /* check that 'wn' is in the list of children? */
#ifdef Is_True_On
  if (wn) {
    WN *kids = WN_first(block);
    while ((kids) && (kids != wn)) {
      kids = WN_next (kids);
    }
    Is_True (kids, ("LWN_Insert_Block_Before: could not find node 'wn'"));
  }
#endif

  if (WN_opcode(in) != OPC_BLOCK) {
    first = in;
    last = in;
    LWN_Set_Parent(in, block);
  } else if (WN_first(in)==NULL && WN_last(in)==NULL) {
    WN_Delete(in);
    return;
  } else {
    first = WN_first(in);
    last = WN_last(in);
    for (w = first; w; w = WN_next(w))
      LWN_Set_Parent(w, block);
    WN_Delete(in);
  }

  WN_prev(first) = wn ? WN_prev(wn) : WN_last(block);
  WN_next(last) = wn ? wn : NULL;
  if (WN_prev(first))
    WN_next(WN_prev(first)) = first;
  else
    WN_first(block) = first;
  if (WN_next(last))
    WN_prev(WN_next(last)) = last;
  else
    WN_last(block) = last;
}

void LWN_Insert_Block_After (WN *block, WN *wn, WN *in)
{
  WN *first;
  WN *last;
  WN *w;

  Is_True(wn || block, ("LWN_Insert_Block_After: nowhere to insert"));

  if (wn) {
    WN *parent_wn = LWN_Get_Parent(wn);
    Is_True (parent_wn, ("LWN_Insert_Block_After: parent is NULL\n"));
  
    if (block == NULL)
      block = parent_wn;
    else {
      Is_True(block == parent_wn,
	      ("LWN_Insert_Block_After: supplied block not parent"));
    }
  }

  Is_True(!block || (WN_operator(block) == OPR_BLOCK),
	  ("LWN_Insert_Block_After: block is not a BLOCK node"));
  Is_True(!wn || OPCODE_is_stmt(WN_opcode(wn)) || OPCODE_is_scf(WN_opcode(wn)),
	  ("LWN_Insert_Block_After: insert node 0x%p not a stmt/scf\n", wn));
  Is_True((in != NULL),
	  ("LWN_Insert_Block_After: insert a NULL node?"));
  Is_True(OPCODE_is_stmt(WN_opcode(in)) || OPCODE_is_scf(WN_opcode(in)), 
	  ("LWN_Insert_Block_After: insert node 0x%p not a stmt/scf\n", in));

  /* check that 'wn' is in the list of children? */
#ifdef Is_True_On
  if (wn) {
    WN *kids = WN_first(block);
    while ((kids) && (kids != wn)) {
      kids = WN_next (kids);
    }
    Is_True (kids, ("LWN_Insert_Block_After: could not find node 'wn'"));
  }
#endif

  if (WN_opcode(in) != OPC_BLOCK) {
    first = in;
    last = in;
    LWN_Set_Parent(in, block);
  } else if (WN_first(in)==NULL && WN_last(in)==NULL) {
    WN_Delete(in);
    return;
  } else {
    first = WN_first(in);
    last = WN_last(in);
    for (w = first; w; w = WN_next(w))
      LWN_Set_Parent(w, block);
    WN_Delete(in);
  }

  WN_prev(first) = wn ? wn : NULL; 
  WN_next(last) = wn ? WN_next(wn) : WN_first(block);
  if (WN_prev(first))
    WN_next(WN_prev(first)) = first;
  else
    WN_first(block) = first;
  if (WN_next(last))
    WN_prev(WN_next(last)) = last;
  else
    WN_last(block) = last;
}

WN* LWN_Extract_From_Block(WN* item)
{
  WN_EXTRACT_FromBlock(LWN_Get_Parent(item), item);
  LWN_Set_Parent(item, NULL);
  return item;
}

WN* LWN_Extract_From_Block(WN* parent, WN* item)
{
  FmtAssert(LWN_Get_Parent(item) == parent,
	    ("Bad parent for LWN_Extract_From_Block"));
  WN_EXTRACT_FromBlock(parent, item);
  LWN_Set_Parent(item, NULL);
  return item;
}

/* delete node "wn" from the block (which may be NULL).
 * adjust the next, previous, and block first and last pointers
 */
extern void LWN_Delete_From_Block(WN *block, WN* wn) {
  WN *node;
  WN* parent_wn;

  Is_True (wn, ("LWN_DeleteFromBlock: deleting a NULL node"));
  Is_True (((!block) || (WN_opcode(block) == OPC_BLOCK)),
           ("LWN_DeleteFromBlock: Expecting a BLOCK node"));
  parent_wn = LWN_Get_Parent(wn);
  Is_True (((!block) || (block == parent_wn)),
           ("LWN_DeleteFromBlock: block is not the parent"));
  
  /* make sure the operators are legal */
  Is_True (OPCODE_is_stmt(WN_opcode(wn)) || OPCODE_is_scf(WN_opcode(wn)),
           ("LWN_DeleteFromBlock: Expecting a SCF or a stmt node"));

  node = WN_first(parent_wn);
  while (node) {
    if (node == wn) break;
    node = WN_next (node);
  }

  FmtAssert (node != NULL,
             ("LWN_DeleteFromBlock: could not find node to delete"));

  if (WN_first(parent_wn) == wn && WN_last(parent_wn) == wn) {
    WN_first(parent_wn)=WN_last(parent_wn)=NULL;
  } else if (WN_first(parent_wn) == wn) {
    WN_first(parent_wn) = WN_next(wn);
    WN_prev(WN_first(parent_wn)) = NULL;
  } else if (WN_last(parent_wn) == wn) {
    WN_last(parent_wn) = WN_prev(wn);
    WN_next(WN_last(parent_wn)) = NULL;
  } else {
    WN_next(WN_prev(wn)) = WN_next(wn);
    WN_prev(WN_next(wn)) = WN_prev(wn);
  }
  
  WN_Delete(wn);
}

#ifdef LNO
extern BOOL Inside_Loop_With_Goto(WN *wn)
{
  while (wn) {
    if (WN_operator(wn) == OPR_DO_LOOP &&
	Do_Loop_Has_Gotos(wn)) {
      return TRUE;
    }
    wn = LWN_Get_Parent(wn);
  }
  return FALSE;
}
#endif

/***********************************************************************
 *
 * "wn" must be the child of a block node. 
 * Remove "wn" from the block node, and delete the entire tree "wn".
 *
 ***********************************************************************/
extern void LWN_Delete_Tree_From_Block(WN* wn) {
  LWN_Delete_Tree (LWN_Extract_From_Block(wn));
}

static WN* LWN_Copy_Tree_R(WN *,BOOL,WN_MAP,BOOL,WN_MAP,STACK_OF_WN *,
			   BOOL); 

/* copy a tree */
/* if copy_access is TRUE, create copies of all the access vectors */
extern WN* LWN_Copy_Tree(WN *wn, BOOL copy_access, WN_MAP access_map,
                         BOOL copy_version, WN_MAP version_map,
			 BOOL copy_all_nodes) {
  BOOL simp_state_save = WN_Simplifier_Enable(FALSE);
  STACK_OF_WN *region_stack;
#ifdef LNO
  region_stack=CXX_NEW(STACK_OF_WN(Malloc_Mem_Pool),
                                    Malloc_Mem_Pool);
#else
  region_stack=NULL;
#endif
  WN *result = 
    LWN_Copy_Tree_R(wn,copy_access,access_map,copy_version,version_map,
                    region_stack, copy_all_nodes);
#ifdef LNO
  CXX_DELETE(region_stack,Malloc_Mem_Pool);
#endif
  WN_Simplifier_Enable(simp_state_save);
  return result;
}
static  WN* LWN_Copy_Tree_R(WN *wn, BOOL copy_access, WN_MAP access_map,
			    BOOL copy_version, WN_MAP version_map,
			    STACK_OF_WN *region_stack,
			    BOOL copy_all_nodes) {
  WN* ret_wn = NULL;
  WN* kid;
  WN* ret_kid;
  WN* prev_wn;
  /* be careful to copy parent annotations */
  if (wn) {
    ret_wn = WN_CopyNode (wn);
    LWN_Copy_Linenumber(wn, ret_wn);

#ifdef LNO
    OPCODE opcode = WN_opcode(wn);
    OPERATOR oper = OPCODE_operator(opcode);
    if (OPCODE_is_store(opcode) || OPCODE_is_load(opcode) || oper == OPR_PARM
	|| oper == OPR_LDA) {
      if (oper == OPR_LDID || oper == OPR_STID || oper == OPR_LDA) {
        Copy_alias_info(Alias_Mgr,wn,ret_wn);
      } else {
        Duplicate_alias_info(Alias_Mgr,wn,ret_wn);
      }
    } else if (oper==OPR_REGION) {
      WN* parent_region=NULL;
      if (region_stack->Elements()>0)
        parent_region=region_stack->Top();
      REGION_clone(wn, ret_wn, parent_region);	// copy region info
      region_stack->Push(ret_wn);
    }

    if (copy_access) {  // copy the access vectors
      if (oper == OPR_ARRAY) {
        ACCESS_ARRAY *array = (ACCESS_ARRAY *) WN_MAP_Get(access_map,
                                                          (WN *) wn);
        // If the input node doesn't have a map value,
        // die in the debug version of the compiler,
        // but just continue in the shipped compiler.
        if (LNO_enabled) {
          Is_True (array, ("LWN_Copy_Tree missing array-access-map"));
        }
        if (array) {
          MEM_POOL *pool = array->Pool();
          WN_MAP_Set(access_map,ret_wn,
                     (void *) CXX_NEW(ACCESS_ARRAY(array,pool),pool));
        }
      } else if (opcode == OPC_DO_LOOP) {
        DO_LOOP_INFO *dli = (DO_LOOP_INFO *) WN_MAP_Get(access_map,(WN *) wn);
        if (LNO_enabled) {
          Is_True (dli, ("LWN_Copy_Tree missing do-loop access-map"));
        }
        if (dli) {
          MEM_POOL *pool = dli->Pool();
          WN_MAP_Set(access_map,ret_wn, 
                     (void *) CXX_NEW(DO_LOOP_INFO(dli,pool),pool));
        }
      } else if (opcode == OPC_IF) {
        IF_INFO *ii = (IF_INFO *) WN_MAP_Get(access_map,(WN *) wn);
        if (LNO_enabled) {
          Is_True (ii, ("LWN_Copy_Tree missing if-access-map"));
        }
        if (ii) {
          MEM_POOL *pool = ii->Pool();
          WN_MAP_Set(access_map,ret_wn, 
                     (void *) CXX_NEW(IF_INFO(ii, pool),pool));
        }
      }
    }
    {
      extern WN_MAP Safe_Spec_Map;
      if (Safe_Spec_Map != WN_MAP_UNDEFINED &&
          (oper == OPR_DIV || oper == OPR_REM || oper == OPR_MOD) &&
          WN_MAP_Get(Safe_Spec_Map, (WN*) wn))
        {

          WN_MAP_Set (Safe_Spec_Map, ret_wn,
                      (void*) WN_MAP_Get(Safe_Spec_Map, (WN*) wn));
        }
    }
    {
      extern WN_MAP RR_Map;
      extern MEM_POOL *LEGO_pool;
      if (RR_Map != WN_MAP_UNDEFINED && oper == OPR_ARRAY && Get_RR_Map(wn)) {
        RR_INFO* new_rri = CXX_NEW (RR_INFO(Get_RR_Map(wn)), LEGO_pool);
        Set_RR_Map (ret_wn, new_rri);
      }
    }
#else
    Is_True(copy_access == FALSE,
	    ("Compile with -DLNO to copy access vectors in LWN_Copy_Tree_R"));
#endif

    if (WN_opcode(wn) == OPC_BLOCK) {
      WN_first(ret_wn) = WN_last(ret_wn) = NULL;
      kid = WN_first (wn);
      if (kid) {
        ret_kid = LWN_Copy_Tree_R (kid,
				   copy_access, access_map,
				   copy_version, version_map,
				   region_stack,
				   copy_all_nodes);
        WN_prev (ret_kid) = WN_next (ret_kid) = NULL;
        LWN_Set_Parent(ret_kid, ret_wn);
        WN_first(ret_wn) = WN_last(ret_wn) = ret_kid;
        kid = WN_next (kid);
        prev_wn = ret_kid;
      }
      while (kid) {
        ret_kid = LWN_Copy_Tree_R (kid,
                                 copy_access, access_map,
                                 copy_version, version_map,
                                 region_stack, copy_all_nodes);
        WN_next (prev_wn) = ret_kid;
        WN_prev (ret_kid) = prev_wn;
        WN_next (ret_kid) = NULL;
        WN_last (ret_wn) = ret_kid;
        LWN_Set_Parent (ret_kid, ret_wn);
        prev_wn = ret_kid;
        kid = WN_next(kid);
      }
    }
    else {
      INT kidno;
      for (kidno=0; kidno<WN_kid_count(wn); kidno++) {
        if (WN_kid (wn, kidno)) { 
          ret_kid = LWN_Copy_Tree_R (WN_kid (wn, kidno), 
                                     copy_access, access_map,
                                     copy_version, version_map,
				     region_stack,
				     copy_all_nodes);
          LWN_Set_Parent (ret_kid, ret_wn);
          WN_kid (ret_wn, kidno) = ret_kid;
        }
        else {
          WN_kid (ret_wn, kidno) = NULL;
        }
      }
    }

#ifdef LNO
    if (copy_version) {
      // copy it for everything that has a dep-graph vertex
      // (not just loads/stores)
      // useful for updating dependence graph later.
      if (copy_all_nodes) {
	WN_MAP_Set (version_map, ret_wn, WN_MAP_Get(version_map, wn));
	WN_MAP_Set (version_map, wn, ret_wn);
      }
      else if (Array_Dependence_Graph->Get_Vertex(wn)) {
        WN_MAP_Set (version_map, ret_wn, WN_MAP_Get(version_map, wn));
        WN_MAP_Set (version_map, wn, ret_wn);
      }

      // now copy it for the array references.
      OPCODE opcode = WN_opcode(wn);
      if ((OPCODE_operator(opcode) == OPR_ILOAD) &&
          (WN_operator(WN_kid0(wn)) == OPR_ARRAY)) {
        WN_MAP_Set(version_map, WN_kid0(ret_wn),
                   WN_MAP_Get(version_map, WN_kid0(wn)));
        WN_MAP_Set(version_map, WN_kid0(wn), WN_kid0(ret_wn));
      }
      else if ((OPCODE_operator(opcode) == OPR_ISTORE) &&
               (WN_operator(WN_kid1(wn)) == OPR_ARRAY)) {
        WN_MAP_Set(version_map, WN_kid1(ret_wn),
                   WN_MAP_Get(version_map, WN_kid1(wn)));
        WN_MAP_Set(version_map, WN_kid1(wn), WN_kid1(ret_wn));
      }
    }

    if (WN_operator(wn)==OPR_REGION)
      region_stack->Pop();
#endif
    
    // Copy the execution counts
    if (Cur_PU_Feedback) {
      WN_MAP32_Set(WN_MAP_FEEDBACK, ret_wn, WN_MAP32_Get(WN_MAP_FEEDBACK, wn));
    }

  }
  return ret_wn;
}


#ifdef LNO
#ifdef KEY
extern WN * Loop_being_replaced;
#endif

void LWN_Copy_Def_Use_Node(WN* from_node, WN* to_node, DU_MANAGER* du)
{ 
  DEF_LIST* deflist = du->Ud_Get_Def(from_node);
  if (deflist) {
    DEF_LIST_ITER iter(deflist);
    for (DU_NODE* n = iter.First(); !iter.Is_Empty(); n = iter.Next()) {
      WN* def = n->Wn();
      du->Add_Def_Use(def, to_node);
    }
    DEF_LIST* deflist2 = du->Ud_Get_Def(to_node);
    if (deflist2 == NULL) {
      du->Create_Def_List(to_node);
      deflist2 = du->Ud_Get_Def(to_node);
    } 
#ifdef KEY
    WN * loop = deflist->Loop_stmt();
    if (Loop_being_replaced == loop)
      deflist2->Set_loop_stmt (NULL);
    else
      deflist2->Set_loop_stmt (loop);
#else
    deflist2->Set_loop_stmt(deflist->Loop_stmt());
#endif // KEY
    if (deflist->Incomplete()) 
      deflist2->Set_Incomplete();
  }
} 

// If 'from_exp' is set in 'map', set up pointers to each other for 
// 'from_exp' and "to_exp'.
void LWN_Copy_Map(WN * from_exp, WN * to_exp, WN_MAP map)
{
  if (map) {
    if (WN_opcode(from_exp) == OPC_BLOCK) {
      WN * wn_from = WN_first(from_exp);
      WN * wn_to = WN_first(to_exp);

      while (wn_from) {
	LWN_Copy_Map(wn_from, wn_to, map);
	wn_from = WN_next(wn_from);
	wn_to = WN_next(wn_to);
      }
    }
    else {
      WN_CopyMap(to_exp, map, from_exp);
      
      for (INT k = 0; k < WN_kid_count(from_exp); k++)
	LWN_Copy_Map(WN_kid(from_exp, k), WN_kid(to_exp, k), map);
    }
  } 
}

void LWN_Copy_Def_Use(WN* from_exp, WN* to_exp, DU_MANAGER* du)
{
  OPCODE fop = WN_opcode(from_exp);
  OPCODE top = WN_opcode(to_exp);

  FmtAssert(fop == top && OPCODE_is_expression(fop),
	    ("Opcodes unequal LWN_Copy_Def_Use(%d,%d) or not expr", fop, top));

  for (INT k = 0; k < WN_kid_count(from_exp); k++)
    LWN_Copy_Def_Use(WN_kid(from_exp,k), WN_kid(to_exp,k), du);

  LWN_Copy_Def_Use_Node(from_exp, to_exp, du);

}
#endif

/* Given a tree, initialize its parent pointers.
 * Override what was there, if anything.
 * Do not update parent pointer of the root node 'wn'.
 */
extern void LWN_Parentize (WN* wn) {
  if (!OPCODE_is_leaf (WN_opcode (wn))) { 
    if (WN_opcode(wn) == OPC_BLOCK) {
      WN* kid = WN_first (wn);
      while (kid) {
        LWN_Set_Parent (kid, wn);
        LWN_Parentize (kid);
        kid = WN_next (kid);
      }
    }
    else {
      INT kidno;
      WN* kid;
      for (kidno=0; kidno<WN_kid_count(wn); kidno++) {
        kid = WN_kid (wn, kidno);
        if (kid) { 
          LWN_Set_Parent (kid, wn);
          LWN_Parentize (kid);
        }
      }
    }
  }
}

/* Given a tree, check that its parent pointers are consistent.
 * Return TRUE if ok, FALSE otherwise.
 */
BOOL LWN_Check_Parentize (const WN* wn) {
  BOOL isok = TRUE;
  if (!OPCODE_is_leaf (WN_opcode (wn))) { 
    if (WN_opcode(wn) == OPC_BLOCK) {
      WN* kid = WN_first (wn);
      while (kid) {
        Is_True ((LWN_Get_Parent (kid) == wn),
                 ("CheckParentize Error: kid 0x%p, parent 0x%p\n", kid, wn));
        isok &= (LWN_Get_Parent (kid) == wn);
        if (!isok) break;
        isok &= LWN_Check_Parentize (kid);
        if (!isok) break;
        kid = WN_next (kid);
      }
    } else if (WN_opcode(wn) == OPC_REGION) {
      isok &= (LWN_Get_Parent(WN_region_body(wn)) == wn);
      isok &= LWN_Check_Parentize (WN_region_body(wn));
    } else {
      INT kidno;
      WN* kid;
      for (kidno=0; kidno<WN_kid_count(wn); kidno++) {
        kid = WN_kid (wn, kidno);
        if (kid) { 
          Is_True ((LWN_Get_Parent (kid) == wn),
                   ("LWN_Check_Parentize(): kid 0x%p (%s), parent 0x%p (%s)\n",
		    kid, OPCODE_name(WN_opcode(kid)),
		    wn, OPCODE_name(WN_opcode(wn))));
          isok &= (LWN_Get_Parent (kid) == wn);
          if (!isok) break;
          isok &= LWN_Check_Parentize (kid);
          if (!isok) break;
        }
      }
    }
  }
  return isok;
}

#ifdef LNO
BOOL inside_parallelizable_loop( WN *wn )
{
  while ( wn ) {
    if ( WN_opcode(wn) == OPC_DO_LOOP ) {
      DO_LOOP_INFO *dl = Get_Do_Loop_Info(wn, TRUE);
      if ( dl && dl->Parallelizable ) {
	return TRUE;
      }
    }
    wn = LWN_Get_Parent(wn);
  }
  return FALSE;
}
#endif

// Given a WHIRL node, set the parent pointers for its immediate children
// only.
void LWN_Parentize_One_Level (const WN* wn) {
   INT i;
   for (i=0; i < WN_kid_count(wn) ; i++) {
      LWN_Set_Parent(WN_kid(wn,i),wn);
   }
}

BOOL Is_Descendent(WN *low, WN *high)
{
  while (low) {
    if (low == high) {
      return TRUE;
    }
    low = LWN_Get_Parent(low);
  }
  return FALSE;
}



/* Delete a tree and all its descendants 
 */
void LWN_Delete_Tree(WN* wn)
{
  if (wn == NULL)
    return;

#ifdef LNO
  // keep track of deleted loop
  if (WN_opcode(wn) == OPC_DO_LOOP) {
    Deleted_Loop_Map->Enter(wn, TRUE);
  }    
#endif

  if (WN_opcode(wn) == OPC_BLOCK) {
    WN* kid = WN_first (wn);
    if (kid != NULL) {
      WN* prev = kid;
      kid = WN_next(kid);
      while (kid != NULL) {
	LWN_Delete_Tree(prev);
	prev = kid;
	kid = WN_next(kid);
      }
      LWN_Delete_Tree(prev);
    }
  } else {
    for (INT i = 0; i < WN_kid_count(wn); i++) {
      LWN_Delete_Tree(WN_kid(wn,i));
    }
  }

#ifdef LNO
  if (WN_opcode(wn) == OPC_REGION) {
    RID_Delete(Current_Map_Tab, wn);
  }
#endif

  WN *parent = LWN_Get_Parent(wn);

#ifdef Is_True_On
  // This is a good place to catch illegal-parent bugs
  if (parent) {
    OPCODE opc = WN_opcode(parent);
    Is_True (opc != 0,
             ("Ahha -- opcode is zero"));
  }
#endif

  if (parent && WN_operator(parent) == OPR_BLOCK) {
    LWN_Delete_From_Block(parent,wn);
  } else {
    WN_Delete(wn);
  }
}

#ifdef LNO

// Look recursively inside wn_tree for OPR_CALL nodes and
// remove any references therein to scalar_node
void Remove_scalar_ref(WN* wn_tree, WN* scalar_node) {
  if (WN_operator(wn_tree) == OPERATOR_UNKNOWN) 
    // wn_tree has already been deleted.
    return;

  if (WN_operator(wn_tree) == OPR_CALL && Has_Call_Info(wn_tree)) {
    ARA_LOOP_INFO* ali = Get_Call_Info(wn_tree)->Call_Ara_Info();
    SCALAR_STACK scalar_uses = ali->SCALAR_USE();
    scalar_uses.Remove_Scalar(scalar_node);
  }

  if (WN_operator(wn_tree) == OPR_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Remove_scalar_ref(wn, scalar_node);
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Remove_scalar_ref((WN_kid(wn_tree, i)), scalar_node);
  } 
  return;
}

//Eliminate scalar references to a node
void LWN_Delete_SR(WN *scalar_node) {
  if (!scalar_node)
    return;

  if(!((WN_operator(scalar_node) == OPR_LDID) ||
       (WN_operator(scalar_node) == OPR_STID)))
    // Not a scalar.
    return;

  // Inspect the eclosing function for calls that
  // reference the scalar_node.
  Remove_scalar_ref(Current_Func_Node, scalar_node);
  return;
}

// Eliminate the du chains connected to a node
void LWN_Delete_DU(WN *wn)
{
  if (Du_Mgr->Ud_Get_Def(wn)) {
    Du_Mgr->Remove_Use_From_System(wn);
  }
  if (Du_Mgr->Du_Get_Use(wn)) {
    Du_Mgr->Remove_Def_From_System(wn);
  }
}

// Eliminate the lno dep graph edges/vertices connected to a node
void LWN_Delete_LNO_dep_graph(WN *wn)
{
  OPCODE        op = WN_opcode(wn);

  if (OPCODE_is_load(op) || OPCODE_is_store(op) || OPCODE_is_call(op)) {
    VINDEX16 v = Array_Dependence_Graph->Get_Vertex(wn);
    if (v) {
      EINDEX16 e;
      EINDEX16 enext = 0;
      for (e = Array_Dependence_Graph->Get_In_Edge(v); e; e = enext) {
	enext = Array_Dependence_Graph->Get_Next_In_Edge(e);
	Array_Dependence_Graph->Delete_Array_Edge(e);
      }
      for (e = Array_Dependence_Graph->Get_Out_Edge(v); e; e = enext) {
	enext = Array_Dependence_Graph->Get_Next_Out_Edge(e);
	Array_Dependence_Graph->Delete_Array_Edge(e);
      }
      Array_Dependence_Graph->Delete_Vertex(v);
    }
  }
}

// Eliminate the cg dep graph edges/vertices connected to a node
void LWN_Delete_CG_dep_graph(WN *wn)
{
  if (!Current_Dep_Graph) return;
  OPCODE        op = WN_opcode(wn);

  if (OPCODE_is_load(op) || OPCODE_is_store(op)) {
    VINDEX16 v = Current_Dep_Graph->Get_Vertex(wn);
    if (v) {
      EINDEX16 e;
      EINDEX16 enext = 0;
      for (e = Current_Dep_Graph->Get_In_Edge(v); e; e = enext) {
	enext = Current_Dep_Graph->Get_Next_In_Edge(e);
	Current_Dep_Graph->Remove_Edge(e);
      }
      for (e = Current_Dep_Graph->Get_Out_Edge(v); e; e = enext) {
	enext = Current_Dep_Graph->Get_Next_Out_Edge(e);
	Current_Dep_Graph->Remove_Edge(e);
      }
      Current_Dep_Graph->Delete_Vertex(v);
    }
  }
}

/* Get rid of all the DEF_USE info relating to the wn subtree */
void LWN_Update_Def_Use_Delete_Tree(WN *wn, DU_MANAGER* du)
{
  if (du == NULL)
    du = Du_Mgr;

  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK) {
    WN* kid = WN_first (wn);
    while (kid) {
      LWN_Update_Def_Use_Delete_Tree(kid, du);
      kid = WN_next(kid);
    }
  } 
  if (OPCODE_is_load(opcode)) { 
    du->Remove_Use_From_System(wn);
  } else if (OPCODE_is_store(opcode)) { 
    du->Remove_Def_From_System(wn);
  }

  for (INT i = 0; i < WN_kid_count(wn); i++) {
    LWN_Update_Def_Use_Delete_Tree(WN_kid(wn,i), du);
  }
}

void LWN_Update_Dg_Delete_Tree(WN *wn, ARRAY_DIRECTED_GRAPH16* dg)
{
  FmtAssert(wn, ("LWN_Update_Dg_Delete_Tree null wn!"));

  if (WN_opcode(wn) == OPC_BLOCK) {
    WN* kid = WN_first (wn);
    if (kid != NULL) {
      WN* prev = kid;
      kid = WN_next(kid);
      while (kid != NULL) {
	LWN_Update_Dg_Delete_Tree(prev, dg);
	prev = kid;
	kid = WN_next(kid);
      }
      LWN_Update_Dg_Delete_Tree(prev, dg);
    }
  }
  else {
    for (INT i = 0; i < WN_kid_count(wn); i++) {
      LWN_Update_Dg_Delete_Tree(WN_kid(wn,i), dg);
    }
  }

  OPCODE        op = WN_opcode(wn);

  if (OPCODE_is_load(op) || OPCODE_is_store(op)) {
    VINDEX16 v = dg->Get_Vertex(wn);
    if (v) {
      EINDEX16 e;
      EINDEX16 enext = 0;
      for (e = dg->Get_In_Edge(v); e; e = enext) {
	enext = dg->Get_Next_In_Edge(e);
	dg->Delete_Array_Edge(e);
      }
      for (e = dg->Get_Out_Edge(v); e; e = enext) {
	enext = dg->Get_Next_Out_Edge(e);
	dg->Delete_Array_Edge(e);
      }
      dg->Delete_Vertex(v);
    }
  }
  else {
    Is_True(dg->Get_Vertex(wn) == 0,
	    ("vertex for non load/store op=%d", op));
  }
}


#endif

extern WN *LWN_Get_Statement(WN *wn)
{
  while (!OPCODE_is_stmt(WN_opcode(wn)) && !OPCODE_is_scf(WN_opcode(wn))) {
    wn = LWN_Get_Parent(wn);
  }
  return(wn);
}

WN *LWN_CreateDO(WN *index, WN *start, WN *end, WN *step,
		       WN *body)
{
  WN *loop_info = LWN_CreateLoopInfo(LWN_Copy_Tree(index),NULL,0,0,0);
  WN* wn = WN_CreateDO(index, start, end, step, body, loop_info);
  if (index) LWN_Set_Parent(index, wn);
  if (start) LWN_Set_Parent(start, wn);
  if (end) LWN_Set_Parent(end, wn);
  if (step) LWN_Set_Parent(step, wn);
  if (body) LWN_Set_Parent(body, wn);
  LWN_Set_Parent(loop_info,wn);
  return wn;
}

WN *LWN_CreateLoopInfo(WN *induction, WN *trip, UINT16 trip_est,
				UINT16 depth, INT32 flags)
{
  WN *wn = WN_CreateLoopInfo(induction, trip, trip_est,depth, flags);
  if (induction) LWN_Set_Parent(induction,wn);
  if (trip) LWN_Set_Parent(trip,wn);
  return wn;
}


WN *LWN_CreateDoWhile(WN *test, WN *body)
{
  WN* wn = WN_CreateDoWhile(test, body);
  if (test) LWN_Set_Parent(test, wn);
  if (body) LWN_Set_Parent(body, wn);
  return wn;
}

WN *LWN_CreateWhileDo(WN *test, WN *body)
{
  WN* wn = WN_CreateWhileDo(test, body);
  if (test) LWN_Set_Parent(test, wn);
  if (body) LWN_Set_Parent(body, wn);
  return wn;
}

WN *LWN_CreateIf(WN *test, WN *if_then, WN *if_else)
{
  WN* wn = WN_CreateIf(test, if_then, if_else);
  if (test) LWN_Set_Parent(test, wn);
  if (if_then) LWN_Set_Parent(if_then, wn);
  if (if_else) LWN_Set_Parent(if_else, wn);
  return wn;
}

WN *LWN_CreateCondbr(INT32 label_number,WN *exp)
{
  WN* wn = WN_CreateTruebr(label_number,exp);
  if (exp) LWN_Set_Parent(exp, wn);
  return wn;
}

WN *LWN_CreateReturn()
{
   WN* wn = WN_CreateReturn();
   return wn;
}

WN *LWN_CreateCompgoto(INT32 num_entries, WN *value,
			WN *block, WN *deflt)
{
  WN* wn = WN_CreateCompgoto(num_entries, value, block, deflt, 0);
  if (value) LWN_Set_Parent(value, wn);
  if (block) LWN_Set_Parent(block, wn);
  if (deflt) LWN_Set_Parent(deflt, wn);
  return wn;
}

#ifndef KEY
WN *LWN_CreateIstore(OPCODE opc, WN_OFFSET offset,
		     TY_IDX ty, WN *value, WN *addr)
{
  WN* wn = WN_CreateIstore(opc, offset, ty, value, addr);
#else
WN *LWN_CreateIstore(OPCODE opc, WN_OFFSET offset,
		     TY_IDX ty, WN *value, WN *addr, UINT field_id)
{
  WN* wn = WN_CreateIstore(opc, offset, ty, value, addr, field_id);
#endif /* KEY */
  if (value) LWN_Set_Parent(value, wn);
  if (addr) LWN_Set_Parent(addr, wn);
  return wn;
}

WN *LWN_CreateMstore(WN_OFFSET offset, TY_IDX ty, WN *value,
		     WN *addr, WN *num_bytes)
{
  WN* wn = WN_CreateMstore(offset, ty,value, addr, num_bytes);
  if (value) LWN_Set_Parent(value, wn);
  if (addr) LWN_Set_Parent(addr, wn);
  if (num_bytes) LWN_Set_Parent(addr, num_bytes);
  return wn;
}

WN *LWN_CreateStid(OPCODE opc, WN_OFFSET offset, 
		   ST* st, TY_IDX ty,WN *value)
{
  WN* wn = WN_CreateStid(opc, offset, st, ty,value);
  if (value) LWN_Set_Parent(value, wn);
  return wn;
}

WN *LWN_CreateStid(OPCODE opc, WN *orig_op, WN *value)
{
  Is_True((WN_operator(orig_op) == OPR_LDID) ||
          (WN_operator(orig_op) == OPR_STID),
	  ("Illegal orig_op in LWN_Create_Stid"));
//bug 13013: we need field_id if it is not 0 (default) 
#ifdef KEY
  WN* wn = WN_CreateStid(opc, WN_offset(orig_op),
		WN_st(orig_op), WN_ty(orig_op),value, WN_field_id(orig_op));
#else
  WN* wn = WN_CreateStid(opc, WN_offset(orig_op),
                WN_st(orig_op), WN_ty(orig_op),value);
#endif

#ifdef LNO
  Copy_alias_info(Alias_Mgr,orig_op,wn);
#endif
  if (value) LWN_Set_Parent(value, wn);
  return wn;
}

WN *LWN_CreateLdid(OPCODE opc, WN *orig_op)
{
  Is_True(orig_op, ("LWN_CreateLdid() called will null orig_op"));
  Is_True((WN_operator(orig_op) == OPR_LDID) ||
          (WN_operator(orig_op) == OPR_STID),
	  ("Illegal orig_op in LWN_Create_Ldid"));
  WN* wn = WN_CreateLdid(opc, WN_offset(orig_op),
#ifndef KEY //bug 13586
		WN_st(orig_op), WN_ty(orig_op));
#else
		WN_st(orig_op), WN_ty(orig_op),WN_field_id(orig_op));
#endif
#ifdef LNO
  Copy_alias_info(Alias_Mgr,orig_op,wn);
#endif
  return wn;
}

WN *LWN_CreateDivceil(TYPE_ID type, WN *kid0, WN *kid1)
{
  OPCODE op = OPCODE_make_op(OPR_INTRINSIC_OP, type, MTYPE_V);
  INT i;
  WN *kids[2];
  kids[0] = kid0;
  kids[1] = kid1;
  INT nkids = 2;

  for (i = 0; i < nkids; i++) {
    if (WN_operator(kids[i]) != OPR_PARM) {
      TYPE_ID type = WN_rtype(kids[i]);
      kids[i] = LWN_CreateParm(type, kids[i], Be_Type_Tbl(type), 
			       WN_PARM_BY_VALUE);
    }
  }

  INTRINSIC intrinsic;
  switch (type) {
    case MTYPE_I4:
      intrinsic = INTRN_I4DIVCEIL;
      break;
    case MTYPE_I8:
      intrinsic = INTRN_I8DIVCEIL;
      break;
    case MTYPE_U4:
      intrinsic = INTRN_U4DIVCEIL;
      break;
    case MTYPE_U8:
      intrinsic = INTRN_U8DIVCEIL;
      break;
    default:
      FmtAssert(FALSE, 
		("Cannot create DIVCEIL intrinsic of type %d\n", type));
  }

  WN *wn = WN_Create_Intrinsic(op, intrinsic, 2, kids);
  LWN_Parentize_One_Level(wn);

  return (wn);
}


WN *LWN_CreateDivfloor(TYPE_ID type, WN *kid0, WN *kid1)
{
  OPCODE op = OPCODE_make_op(OPR_INTRINSIC_OP, type, MTYPE_V);
  INT i;
  WN *kids[2];
  kids[0] = kid0;
  kids[1] = kid1;
  INT nkids = 2;

  for (i = 0; i < nkids; i++) {
    if (WN_operator(kids[i]) != OPR_PARM) {
      TYPE_ID type = WN_rtype(kids[i]);
      kids[i] = LWN_CreateParm(type, kids[i], Be_Type_Tbl(type), 
			       WN_PARM_BY_VALUE);
    }
  }

  INTRINSIC intrinsic;
  switch (type) {
    case MTYPE_I4:
      intrinsic = INTRN_I4DIVFLOOR;
      break;
    case MTYPE_I8:
      intrinsic = INTRN_I8DIVFLOOR;
      break;
    case MTYPE_U4:
      intrinsic = INTRN_U4DIVFLOOR;
      break;
    case MTYPE_U8:
      intrinsic = INTRN_U8DIVFLOOR;
      break;
    default:
      FmtAssert(FALSE, 
		("Cannot create DIVFLOOR intrinsic of type %d\n", type));
  }

  WN *wn = WN_Create_Intrinsic(op, intrinsic, 2, kids);
  LWN_Parentize_One_Level(wn);

  return (wn);
}

WN *LWN_CreateEval(WN *exp)
{
  WN* wn = WN_CreateEval(exp);
  if (exp) LWN_Set_Parent(exp, wn);
  return wn;
}

WN *LWN_CreateExp1(OPCODE opc, WN *kid0)
{
  WN* wn = WN_CreateExp1(opc, kid0);
  LWN_Parentize_One_Level(wn);
  return wn;
}

WN *LWN_CreateExp2(OPCODE opc, WN *kid0, WN *kid1)
{
  WN* wn = WN_CreateExp2(opc, kid0, kid1);
  LWN_Parentize_One_Level(wn);
  return wn;
}

#ifndef KEY
WN *LWN_CreateIload(OPCODE opc, WN_OFFSET offset, 
		   TY_IDX ty1, TY_IDX ty2,WN *addr)
{
  WN* wn = WN_CreateIload(opc, offset, ty1, ty2, addr);
#else
WN *LWN_CreateIload(OPCODE opc, WN_OFFSET offset, 
		    TY_IDX ty1, TY_IDX ty2,WN *addr, UINT field_id)
{
  WN* wn = WN_CreateIload(opc, offset, ty1, ty2, addr, field_id);
#endif /* KEY */
  LWN_Parentize_One_Level(wn);
  return wn;
}

WN *LWN_CreateMload(WN_OFFSET offset, TY_IDX ty,WN *addr,
		    WN *num_bytes)
{
  WN* wn = WN_CreateMload(offset, ty,addr, num_bytes);
  LWN_Parentize_One_Level(wn);
  return wn;
}

WN *LWN_CreateCvtl(OPCODE opc, INT16 cvtl_bits, WN *kid0)
{
  WN* wn = WN_CreateCvtl(opc, cvtl_bits, kid0);
  LWN_Parentize_One_Level(wn);
  return wn;
}

WN *LWN_CreateParm(TYPE_ID rtype, WN *parm_node, TY_IDX ty, UINT32 flag)
{
  WN *wn = WN_CreateParm(rtype, parm_node, ty, flag);
  LWN_Parentize_One_Level(wn);
  return wn;
}


#ifdef LNO
WN *LWN_CreatePrefetch(WN_OFFSET offset, UINT32 flag, WN* addr) {
#ifdef KEY
  Num_Prefetches ++;
#endif
  WN* wn = WN_CreatePrefetch (offset, flag, addr);
  LWN_Parentize_One_Level (wn);
  return wn;
}
#endif

WN* LWN_Int_Type_Conversion(WN *wn, TYPE_ID to_type)
{
  WN* wn_new = WN_Int_Type_Conversion(wn, to_type);
  if (wn_new != wn &&
      (WN_operator(wn_new) == OPR_CVT || WN_operator(wn_new) == OPR_CVTL)) {
#ifndef KEY
    FmtAssert(WN_kid0(wn_new) == wn,
              ("strange parent %s: %p != %p",
               OPCODE_name(WN_opcode(wn_new)), WN_kid0(wn_new), wn));
    LWN_Set_Parent(wn, wn_new);
#else
    // WN simplifier could over-write wn and create a new wn_new.
    // Reset parent for wn only if wn is wn_new's kid.
    if (WN_kid0(wn_new) == wn)
      LWN_Set_Parent(wn, wn_new);
#endif
  }
  return wn_new;
}

// Promote the less than 4 byte types to 4 bytes
extern TYPE_ID Promote_Type(TYPE_ID mtype)
{
  switch (mtype) {
    case MTYPE_I1 : case MTYPE_I2: return(MTYPE_I4);
    case MTYPE_U1 : case MTYPE_U2: return(MTYPE_U4);
    default: return mtype;
  }
}

/*
 *      Return true if the sub-trees wn1 and wn2 and "equiv"
 *      (the equiv notion is similar to a recursive WN_Equiv).
 */
extern BOOL Tree_Equiv (WN *wn1, WN* wn2) {

  if (!wn1 && !wn2) return TRUE;    // both are NULL
  if (!wn1 || !wn2) return FALSE;   // one (but not both) is NULL
  if (!WN_Equiv (wn1, wn2)) return FALSE;   // not the same

  // now examine the kids
  if (WN_opcode(wn1) == OPC_BLOCK) {
    WN *kid1 = WN_first(wn1);
    WN *kid2 = WN_first(wn2);
    while (1) {
      if (!Tree_Equiv (kid1, kid2)) return FALSE;
      if (kid1 == NULL) break;
      kid1 = WN_next (kid1);
      kid2 = WN_next (kid2);
    };
    return TRUE;
  }
  else {
    // since the two nodes are equiv, they have the same # of children
    for (INT i=0; i<WN_kid_count(wn1); i++)
      if (!Tree_Equiv (WN_kid(wn1,i), WN_kid(wn2,i))) return FALSE;
    return TRUE;
  }
}

#ifdef LNO
extern WN *LWN_Loop_Trip_Count(const WN *loop)
{
  WN *lb;			/* lower bound of loop */
  WN *ub;			/* upper bound of loop */
  OPCODE ub_compare;		/*   and comparison op for ub */
  WN *incr;			/* loop increment */
  BOOL is_incr;			/*   and is it increment or decrement */
  WN *trip_cnt;			/* calculated trip count tree */
  BOOL saved_fold_enable;	/* saved state of simplifier */
  TYPE_ID trip_mtype;		/* mtype of tripcount expression */

  /* if this isn't a do_loop, we don't have a clue what the 
   * tripcount may be. (unless we later get some kind of help 
   * from someone)
   */
  if ( WN_opcode(loop) != OPC_DO_LOOP )
    return NULL;

  lb = WN_LOOP_LowerBound( loop );
  if ( lb == NULL )
    return NULL;

  ub = WN_LOOP_UpperBound( loop, &ub_compare );
  if ( ub == NULL )
    return NULL;

  incr = WN_LOOP_Increment( loop, &is_incr );
  if ( incr == NULL )
    return NULL;

  /* if we're using floating point, we don't try to figure out the
   * trip count.
   */
  trip_mtype = OPCODE_desc(ub_compare);
  if ( ! MTYPE_is_integral(WN_rtype(lb)) ||
       ! MTYPE_is_integral(WN_rtype(ub)) ||
       ! MTYPE_is_integral(WN_rtype(incr)) ||
       ! MTYPE_is_integral(trip_mtype) )
    return NULL;

  /* force simplification to be enabled */
  saved_fold_enable = WN_Simplifier_Enable(TRUE);

  /* calculate trip_cnt = ((ub - lb) + incr) / incr
   * (but leave out "+ incr" if comparison is GT or LT)
   */
   
  WN *new_lb = LWN_Copy_Tree(lb);
  WN *new_ub = LWN_Copy_Tree(ub);
  if (Du_Mgr) {
    LWN_Copy_Def_Use(lb,new_lb,Du_Mgr);
    LWN_Copy_Def_Use(ub,new_ub,Du_Mgr);
  }
  trip_cnt = LWN_CreateExp2( OPCODE_make_op(OPR_SUB,trip_mtype,MTYPE_V),
			    new_ub,new_lb);
  if (OPCODE_operator(ub_compare) != OPR_GT &&
      OPCODE_operator(ub_compare) != OPR_LT)
    trip_cnt = LWN_CreateExp2( OPCODE_make_op(OPR_ADD,trip_mtype,MTYPE_V),
			      trip_cnt, LWN_Copy_Tree(incr) );
  trip_cnt = LWN_CreateExp2( OPCODE_make_op(OPR_DIV,trip_mtype,MTYPE_V),
			    trip_cnt, LWN_Copy_Tree(incr) );

  /* reset simplification */
  (void) WN_Simplifier_Enable(saved_fold_enable);

  return trip_cnt;
}
#endif


#ifdef _USE_OLD_FEEDBACK
extern void LWN_Set_Frequency_Tree(const WN* wn, const INT32 count)
{
  if (Cur_PU_Feedback) {
    LWN_ITER* wniter = LWN_WALK_StmtIter( (WN *) wn );
    while (wniter) {
      WN *cur = wniter->wn;
      wniter = LWN_WALK_TreeNext(wniter);
	if (WN_opcode(cur) != OPC_BLOCK) 
	  WN_MAP32_Set(WN_MAP_FEEDBACK, cur, count);	
    }
  }
}

extern void LWN_Copy_Frequency_Tree(const WN* wn, const WN* wn_from)
{
  if (Cur_PU_Feedback) {
    INT32 count = WN_MAP32_Get(WN_MAP_FEEDBACK, wn_from);
    LWN_ITER* wniter = LWN_WALK_StmtIter( (WN *) wn );

    while (wniter) {
      WN *cur = wniter->wn;
      wniter = LWN_WALK_TreeNext(wniter);
      if (WN_opcode(cur) != OPC_BLOCK) 
	WN_MAP32_Set(WN_MAP_FEEDBACK, cur, count);
    }
  }
}

extern void LWN_Adjust_Frequency_Tree(const WN* wn, const INT32 count)
{
  if (Cur_PU_Feedback) {
    LWN_ITER *wniter = LWN_WALK_StmtIter( (WN *) wn );
    
    while (wniter) {
      WN *cur = wniter->wn;
      wniter = LWN_WALK_TreeNext(wniter);
      if (WN_opcode(cur) != OPC_BLOCK) {
	INT32 count_new = MAX(0,WN_MAP32_Get(WN_MAP_FEEDBACK, cur) + count);
	WN_MAP32_Set(WN_MAP_FEEDBACK, cur, count_new);
      }
    }
  }
}

extern void LWN_Scale_Frequency_Tree(const WN* wn, const float ratio)
{
  if (Cur_PU_Feedback) {
    LWN_ITER *wniter = LWN_WALK_StmtIter( (WN *) wn );
    
    while (wniter) {
      WN *cur = wniter->wn;
      wniter = LWN_WALK_TreeNext(wniter);
      if (WN_opcode(cur) != OPC_BLOCK) {
	INT32 count_new = WN_MAP32_Get(WN_MAP_FEEDBACK, cur)*ratio;
	WN_MAP32_Set(WN_MAP_FEEDBACK, cur, count_new);
      }
    }
  }
}
#else
void LWN_Set_Frequency_Tree(const WN*, const INT32) {}
void LWN_Copy_Frequency_Tree(const WN*, const WN *) {}
void LWN_Adjust_Frequency_Tree(const WN*, const INT32) {}
void LWN_Scale_Frequency_Tree(const WN*, const float) {}
#endif // _USE_OLD_FEEDBACK


/***********************************************************************
 *
 * For dbx: print out the map value.
 *
 ***********************************************************************/
void DBX_WN_Map (WN_MAP map, WN* wn) {
  if (!wn) {
    printf ("wn is NULL\n");
    return;
  }
  printf ("Map = %p\n", WN_MAP_Get(map, wn));
}

//-----------------------------------------------------------------------
// NAME: LWN_Simplify_Tree 
// FUNCTION: Simplify the tree rooted at 'wn', and return the simplified 
//   tree.  
//-----------------------------------------------------------------------

WN* LWN_Simplify_Tree(WN* wn)
{
  INT i;
  WN* wn_parent = LWN_Get_Parent(wn); 

  for (i = 0; i < WN_kid_count(wn_parent); i++) 
    if (WN_kid(wn_parent, i) == wn) 
      break;

  WN* wn_new = WN_Simplify_Tree(wn);
  WN_kid(wn_parent, i) = wn_new; 
  LWN_Set_Parent(wn_new, wn_parent); 
  LWN_Parentize(wn_new); 
  return wn_new; 
} 
