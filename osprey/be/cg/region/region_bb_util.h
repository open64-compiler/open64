/*
  Copyright (C) 2000-2003, Institute of Computing Technology, Chinese Academy of Sciences
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:
  
  Redistributions of source code must retain the above copyright notice, this list
  of conditions and the following disclaimer. 

  Redistributions in binary form must reproduce the above copyright notice, this list
  of conditions and the following disclaimer in the documentation and/or other materials
  provided with the distribution. 

  Neither the name of the owner nor the names of its contributors may be used to endorse or
  promote products derived from this software without specific prior written permission. 

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
 
//-*-c++-*-
/* =============================================================================
 
 * Module: region_bb_util.h
 * $Revision: 1.1 $
 * $Date: 2005/12/30 01:47:13 $
 * $Author: weitang $ 
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/region/region_bb_util.h,v $
 * =============================================================================
 */
#ifndef  region_bb_util_INCLUDED
#define  region_bb_util_INCLUDED

#include "region.h"
#include "gtn_universe.h"
#include "gtn_set.h"

/* ====================================================================
 * ====================================================================
 *
 * Module: region_bb_util.h
 * $Revision: 1.1 $
 * $Date: 2005/12/30 01:47:13 $
 * $Author: weitang $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/region/region_bb_util.h,v $
 *
 * Description:
 *      Definitions for interface of about global cfg and regional_cfg.
 * 
 *
 * Utilities:
 *
 *   inline REGIONAL_CFG_NODE *Regional_Cfg_Node(BB *bb);
 *      Get the corresponding Regional cfg node of bb
 *
 *   REGIONAL_CFG_NODE *Regional_Cfg_Node(REGIONAL_CFG_NODE *node, REGION *rgn)
 *      Find the corresponding regional cfg node of node in rgn
 *
 *   inline REGION *Home_Region(BB *bb);
 *      Get the direct Region where bb is 
 *
 *   inline BOOL Region_Contains_BB(REGION *rgn, BB *bb)
 *      Test if bb is contained by rgn
 *
 * The following functions all has an argument regional_cfg, its default 
 * value is NULL. when regional_cfg==NULL, get it from the another argument
 * BB.
 *
 *
 *   BB *RGN_Gen_And_Insert_BB(BB *pred_bb, BB *succ_bb,
 *                             REGIONAL_CFG *regional_cfg=NULL,
 *                             int force_not_fall_through=FALSE,
 *                             float prob=0.0);
 *	    Create a new empty BB, 
 *	    if force_not_fall_through is TRUE, 
 *	        append it after the last BB of this PU, 
 *      else if succ_bb is fall through of pred_bb, 
 *                add it between pred_bb and succ_bb
 *           else
 *                append it after the last BB of this PU
 *      add this new BB and edge(pred_bb,new_bb) and edge(new_bb,succ_bb)
 *	    into global cfg and regional_cfg
 *      if force_not_fall_through is FALSE
 *          del the edge(pred_bb, succ_bb) from global cfg and regional_cfg
 *      if prob=0 get prob=prob(pred_bb,succ_bb)
 *
 *
 *   BB *RGN_Gen_And_Insert_BB_After(BB *point, 
 *                   REGIONAL_CFG *regional_cfg=NULL)
 *   BB *RGN_Gen_And_Insert_BB_Before(BB *point, 
 *                   REGIONAL_CFG *regional_cfg=NULL)
 *      Create a new empty BB, append it before or after <point> in 
 *      global cfg and regional_cfg. 
 *      The RID is determined by using <point> as a model for Gen_BB_Like.
 *
 * 
 *   void RGN_Remove_BB_And_Edges(BB *bb, 
 *                   REGIONAL_CFG *regional_cfg=NULL)
 *      Remove a BB from the BB list, and from regional_cfg
 *      Remove all edges in global cfg and regional_cfg which from or to BB
 *      Update entry set and exit set of regions
 *
 *
 *   void RGN_Add_Regional_Cfg_Edge(BB *pred, 
 *                                  BB *succ, 
 *                                  REGIONAL_CFG *regional_cfg=NULL);
 *      link pred and succ in regional cfg
 *
 *   void RGN_Link_Pred_Succ_With_Prob(BB *pred, BB *succ, float prob,
 *                   REGIONAL_CFG *regional_cfg=NULL);
 *      link pred and succ in global cfg and regional cfg
 *
 *
 *   void RGN_Del_Regional_Cfg_Edge(BB *pred, 
 *                                  BB *succ, 
 *                                  REGIONAL_CFG *regional_cfg=NULL);
 *      Unlink pred and succ in regional cfg
 *
 *   void RGN_Unlink_Pred_Succ(BB *pred, BB *succ, 
 *                   REGIONAL_CFG *regional_cfg=NULL);
 *      Unlink Pred and succ in global cfg and regional cfg
 *
 *  
 *   void Collect_Exit_BBs(REGION *rgn, BB_VECTOR *exits);
 *      Collect all exits bbs of rgn and its child  
 *
 *   void Collect_Entry_BBs(REGION *rgn, BB_VECTOR *entries);
 *      Collect all entries bbs of rgn and its child
 *
 *
 * ====================================================================
 *
 * TODO: Complete this interface description.
 *
 * ====================================================================
 * ====================================================================
 */
extern REGIONAL_CFG_NODE *Regional_Cfg_Node(BB *bb);
extern REGIONAL_CFG_NODE *Regional_Cfg_Node(REGIONAL_CFG_NODE *node, REGION *rgn);
extern REGION *Home_Region(BB *bb);
extern BOOL Region_Contains_BB(REGION *rgn, BB *bb);

extern void RGN_Gen_And_Insert_Node(BB *new_bb, BB *pred_bb, BB *succ_bb,
                                    REGIONAL_CFG *regional_cfg = NULL);

extern BB *RGN_Gen_And_Insert_BB(BB *pred_bb, BB *succ_bb, 
				 REGIONAL_CFG *regional_cfg = NULL,
				 BOOL fall_through = FALSE,
                 float prob = 0.0);
extern BB *RGN_Gen_And_Insert_BB_After(BB *point, 
                  REGIONAL_CFG *regional_cfg = NULL);
extern BB *RGN_Gen_And_Insert_BB_Before(BB *point, 
                  REGIONAL_CFG *regional_cfg = NULL);

 
extern void RGN_Remove_BB_And_Edges(BB *bb, 
                  REGIONAL_CFG *regional_cfg = NULL);

extern void Add_Regional_Cfg_Edge(REGIONAL_CFG_NODE *pred, 
                                  REGIONAL_CFG_NODE *succ, 
                                  REGION *rgn);
extern void RGN_Add_Regional_Cfg_Edge(BB *pred, 
                                      BB *succ, 
                                      REGIONAL_CFG *regional_cfg=NULL);
extern void RGN_Link_Pred_Succ_With_Prob(BB *pred, BB *succ, float prob,
                  REGIONAL_CFG *regional_cfg = NULL);



extern void Collect_Exit_BBs(REGION *rgn, BB_VECTOR *exits);
extern void Collect_Entry_BBs(REGION *rgn, BB_VECTOR *entries);
extern void Del_Regional_Cfg_Edge(REGIONAL_CFG_NODE *pred, 
                                  REGIONAL_CFG_NODE *succ, 
                                  REGION *rgn);
extern void RGN_Del_Regional_Cfg_Edge(BB *pred, 
                                      BB *succ, 
                                      REGIONAL_CFG *regional_cfg=NULL);
extern void RGN_Unlink_Pred_Succ(BB *pred, BB *succ, 
                  REGIONAL_CFG *regional_cfg = NULL);



extern GTN_SET *Region_Def_Reach_In(REGION *rgn, MEM_POOL *pool);
extern BB *RGN_Divide_BB(BB *bb, OP *point);

#endif
