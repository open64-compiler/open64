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
 *  Module: cg_loop_scc.h
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:21-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_loop_scc.h $
 *
 *  Revision history:
 *   16-Oct-91 - Original Version
 *
 *  Synopsis:
 *
 *      Representation and creation of strongly connected components.
 *
 *  Prefixes: CG_LOOP_SCC  - for Loop Strongly Connected Component
 *
 *  Interface Description:
 *
 *      Exported types:
 *
 *          typedef struct cg_loop *CG_LOOP_SCC
 *
 *              Represents a strongly connected component in the data
 *              dependence graph.  Its elements are OPs.  The
 *              definition of "strongly connected component" is that
 *              to elements a,b are in the same strongly connected
 *              component iff there is a path from a to b and from b
 *              to a.  The strongly connected components are thus a
 *              representation of recurrences in the code.  (Note that
 *              elements that do not belong to any graph cycle have
 *              components to themselves.)
 *
 *              CG_LOOP_SCCs have the following fields:
 *
 *                  INT         id
 *
 *                      A unique identifing integer.  Also index of
 *                      self in CG_LOOP_SCC_Vec.
 *                      
 *                  OP        **members
 *
 *                      Vector of OPs that are members of the SCC.
 *                      Access i-th member with _member(scc,i).
 *
 *                  INT         member_count
 *
 *                      Number of members.
 *
 *                  CG_LOOP_SCC    **succs
 *                  INT         succ_count
 *                  CG_LOOP_SCC    **preds
 *                  INT         pred_count
 *
 *                      Vectors and their lengths for predecessor,
 *                      successor SCCs.  A SCC s is a predecessor
 *                      (successor) of self iff:
 *
 *                          1. s != self, and
 *
 *                          2. There is a member m of self st some
 *                             member of s is a predecessor
 *                             (successor) of m.
 *
 *                  INT32       mark
 *
 *                      Integer mark for simple set membership.  See
 *                      CG_LOOP_SCC_mark_count, CG_LOOP_SCC_1_Set...
 *
 *                  CG_LOOP_SCC_PROP *ancestors
 *                  CG_LOOP_SCC_PROP *descendants
 *
 *                      Transitive closures of the REGIN dependency
 *                      relation between SCCs.
 *
 *                  DOUBLE crit_res_ratio
 *
 *                      Greatest ratio of this SCCs use of any
 *                      critical resource relative to the total
 *                      available.
 *
 *		    INT32 rec_mii
 *
 *			The minimum loop scheduling iteration interval
 *			implied by the longest recurrence cycle in this
 *			SCC.  Valid only after the cg_prep_resource.c routine
 *			Calculate_Max_Costs_And_Min_Recurrence_II has
 *			been executed.
 *
 *		    BB *bb
 *
 *			The BB in which this SCC lives.
 *
 *              CG_LOOP_SCCs have the following field reference macros:
 *
 *                  OP *CG_LOOP_SCC_member(
 *                      SCC *scc,
 *                      INT  i
 *                  )
 *
 *                      Reference to the 'i'-th member of 'scc'.
 *
 *                  SCC *CG_LOOP_SCC_pred(
 *                      SCC *scc,
 *                      INT  i
 *                  )
 *
 *                  SCC *CG_LOOP_SCC_succ(
 *                      SCC *scc,
 *                      INT  i
 *                  )
 *
 *                      Reference to 'scc's 'i'-th predecessor
 *                      (successor) SCC.
 *
 *      Exported variables:
 *
 *          CG_LOOP_SCC *CG_LOOP_SCC_Vec
 *
 *              Vector of the strongly connected components valid
 *              after call to CG_LOOP_Make_Strongly_Connected_Components.
 *
 *          INT CG_LOOP_SCC_Count
 *
 *              Length of CG_LOOP_SCC_Vec valid after call to
 *              CG_LOOP_Make_Strongly_Connected_Components.
 *
 *          INT32 CG_LOOP_max_scc_member_count
 *
 *              Number of members in the largest SCC in this loop.
 *
 *      Exported functions:
 *
 *          void CG_LOOP_Make_Strongly_Connected_Components(
 *              BB  *bb,
 *		MEM_POOL *vec_pool,
 *		BOOL ignore_non_def_mem_deps
 *          )
 *
 *              Finds and initializes the strongly connected
 *              components in 'bb'.  'vec_pool' is used for allocation
 *		of CG_LOOP_SCC_Vec.  See CG_LOOP_SCC_{Vec,Count}.
 *		If <ignore_non_def_mem_deps>, then non-definite memory
 *		dependences will be ignored.
 *
 *          void CG_LOOP_Print_Strongly_Connected_Components(void)
 *
 *              Prints out the strongly connected components to TFile.
 *
 *          void CG_LOOP_Print_Strongly_Connected_Components_With_Costs(
 *              void
 *          )
 *
 *              Also prints the cost table.  (This can be really long!)
 *              
 *          void CG_LOOP_SCC_Print(
 *              SCC   *scc
 *          )
 *
 *              Prints out a single strongly connected component to
 *              the TFile.  (Mostly for interactive use when
 *              debugging.)
 *              
 *          void CG_LOOP_SCC_Print_With_Costs(
 *              SCC   *scc
 *          )
 *
 *              Also print the cost table.  (This can be really long!)
 *
 *  Exported macros:
 *
 *      void CG_LOOP_SCC_MAKE_1_SET_EMPTY()
 *
 *      BOOL CG_LOOP_SCC_IS_1_SET_MEMBER(
 *          CG_LOOP_SCC  *scc
 *      )
 *
 *      void CG_LOOP_SCC_ADD_TO_1_SET(
 *          CG_LOOP_SCC  *scc
 *      )
 *
 *          These three macros allow very fast operations to be
 *          performed on a single set at a time.  The bug is that
 *          you'll need to hard wire it if you want another set at the
 *          same time.  Use this for well understood local algorithms.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef CG_LOOP_SCC_INCLUDED
#define CG_LOOP_SCC_INCLUDED

#include "cg_loop_scc_prop.h"

typedef struct cg_loop_scc CG_LOOP_SCC;

struct cg_loop_scc {
  double             crit_res_ratio;
  INT                id;
  OP               **members;
  INT                member_count;
  CG_LOOP_SCC      **succs;
  INT                succ_count;
  CG_LOOP_SCC      **preds;
  INT                pred_count;
  CG_LOOP_SCC_PROP  *ancestors;
  CG_LOOP_SCC_PROP  *descendants;
  INT32              mark;
  INT32              rec_mii;
  BOOL               visited;
};

#define     CG_LOOP_SCC_crit_res_ratio(x)    ((x)->crit_res_ratio+0)
#define Set_CG_LOOP_SCC_crit_res_ratio(x,v)  ((x)->crit_res_ratio=(v))
#define     CG_LOOP_SCC_id(x)                ((x)->id+0)
#define Set_CG_LOOP_SCC_id(x,v)              ((x)->id=(v))
#define     CG_LOOP_SCC_members(x)           ((x)->members+0)
#define Set_CG_LOOP_SCC_members(x,v)         ((x)->members=(v))
#define     CG_LOOP_SCC_member_count(x)      ((x)->member_count+0)
#define Set_CG_LOOP_SCC_member_count(x,v)    ((x)->member_count=(v))
#define     CG_LOOP_SCC_succs(x)             ((x)->succs+0)
#define Set_CG_LOOP_SCC_succs(x,v)           ((x)->succs=(v))
#define     CG_LOOP_SCC_succ_count(x)        ((x)->succ_count+0)
#define Set_CG_LOOP_SCC_succ_count(x,v)      ((x)->succ_count=(v))
#define     CG_LOOP_SCC_preds(x)             ((x)->preds+0)
#define Set_CG_LOOP_SCC_preds(x,v)           ((x)->preds=(v))
#define     CG_LOOP_SCC_pred_count(x)        ((x)->pred_count+0)
#define Set_CG_LOOP_SCC_pred_count(x,v)      ((x)->pred_count=(v))
#define     CG_LOOP_SCC_visited(x)           ((x)->visited+0)
#define Set_CG_LOOP_SCC_visited(x,v)         ((x)->visited=(v))
#define     CG_LOOP_SCC_ancestors(x)         ((x)->ancestors+0)
#define Set_CG_LOOP_SCC_ancestors(x,v)       ((x)->ancestors=(v))
#define     CG_LOOP_SCC_descendants(x)       ((x)->descendants+0)
#define Set_CG_LOOP_SCC_descendants(x,v)     ((x)->descendants=(v))
#define     CG_LOOP_SCC_mark(x)              ((x)->mark+0)
#define Set_CG_LOOP_SCC_mark(x,v)            ((x)->mark=(v))
#define     CG_LOOP_SCC_rec_mii(x)           ((x)->rec_mii+0)
#define Set_CG_LOOP_SCC_rec_mii(x,v)         ((x)->rec_mii=(v))
#define     CG_LOOP_SCC_bb(x)                (CG_LOOP_SCC_Current_BB+0)
#define Set_CG_LOOP_SCC_bb(x,v)              (CG_LOOP_SCC_Current_BB=(v))
    
#define     CG_LOOP_SCC_member(x,i)          (CG_LOOP_SCC_members(x)[i]+0)
#define Set_CG_LOOP_SCC_member(x,i,v)        (CG_LOOP_SCC_members(x)[i]=(v))
#define     CG_LOOP_SCC_succ(x,i)            (CG_LOOP_SCC_succs(x)[i]+0)
#define Set_CG_LOOP_SCC_succ(x,i,v)          (CG_LOOP_SCC_succs(x)[i]=(v))
#define     CG_LOOP_SCC_pred(x,i)            (CG_LOOP_SCC_preds(x)[i]+0)
#define Set_CG_LOOP_SCC_pred(x,i,v)          (CG_LOOP_SCC_preds(x)[i]=(v))


extern BB          *CG_LOOP_SCC_Current_BB;
extern CG_LOOP_SCC *CG_LOOP_SCC_Vec;
extern INT          CG_LOOP_SCC_Count;
extern INT32        CG_LOOP_max_scc_member_count;

extern void CG_LOOP_Make_Strongly_Connected_Components(
    BB  *bb,
    MEM_POOL *vec_pool,
    BOOL ignore_non_def_mem_deps
);

extern void CG_LOOP_Print_Strongly_Connected_Components(void);
#pragma mips_frequency_hint NEVER CG_LOOP_Print_Strongly_Connected_Components

extern void CG_LOOP_Print_Strongly_Connected_Components_With_Costs(
    void
);
#pragma mips_frequency_hint NEVER CG_LOOP_Print_Strongly_Connected_Components_With_Costs

extern void CG_LOOP_SCC_Print(
    CG_LOOP_SCC   *scc
);
#pragma mips_frequency_hint NEVER CG_LOOP_SCC_Print

extern void CG_LOOP_SCC_Print_With_Costs(
    CG_LOOP_SCC   *scc
);
#pragma mips_frequency_hint NEVER CG_LOOP_SCC_Print_With_Costs

extern INT32 CG_LOOP_SCC_mark_count; /* To be used only by 1 set macros. */

#define CG_LOOP_SCC_MAKE_1_SET_EMPTY()                                      \
  (++CG_LOOP_SCC_mark_count)

#define CG_LOOP_SCC_IS_1_SET_MEMBER(scc)                                    \
  (CG_LOOP_SCC_mark(scc) == CG_LOOP_SCC_mark_count)

#define CG_LOOP_SCC_ADD_TO_1_SET(scc)                                       \
  Set_CG_LOOP_SCC_mark(scc,CG_LOOP_SCC_mark_count)

#endif /* CG_LOOP_SCC_INCLUDED */
