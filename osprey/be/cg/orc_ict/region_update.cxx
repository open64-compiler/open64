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
//=============================================================================
//
// Module: region_update.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $ 
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_ict/region_update.cxx,v $
//
//=============================================================================

#include "region.h"
#include "region_update.h"
#include "interval_processor.h"
#include "bb.h"
#include "cg_loop.h"
#include "vt_region.h"
#include "ipfec_defs.h"
#include "tracing.h"

//=============================================================================
// 
// REGION_LOOP_UPDATE class
//
//=============================================================================

//=============================================================================
//  Function: Record_Loop_Region_Boundary
//
//  input:  LOOP_REGION *loop
//
//  side_effection:
//          modify the variable : _bbs_not_in_loop
//  Description:
//          scan all BB in REGION_TREE, if this BB is not contained by this 
//          loop region, add it in _bbs_not_in_loop
//=============================================================================
void 
REGION_LOOP_UPDATE::Record_Loop_Region_Boundary(LOOP_REGION *loop){
            
    _bbs_not_in_loop.clear();
    _loop_bbs.clear();    
        
    if (Get_Trace(TP_A_REGION, TT_RGN_UPDATE_DEBUG)){
        fprintf(TFile,"before loop optimization\n");
        draw_global_cfg("before loop optimization");
        draw_regional_cfg(loop->Parent());
        if(loop->Parent()->Parent()){
            fprintf(TFile,"draw the root region \n ");
            draw_regional_cfg(loop->Tree()->Root());
        }
    }

    for(INNERMOST_REGION_FIRST_ITER iter(_region_tree); iter!=0; ++iter){
        REGION *region = *iter;
        if(region == loop || region->Is_Contained_By(loop))
            continue;
        for(SEQ_REGIONAL_CFG_ITER node_iter(region->Regional_Cfg()); node_iter != 0; ++node_iter){
            REGIONAL_CFG_NODE *node = *node_iter;
            if(! node->Is_Region())
                _bbs_not_in_loop.push_back(node->BB_Node());
        }
    }       
}

//=============================================================================
//  Function: Del_Loop_Region
//
//  input:  LOOP_REGION *loop
//
//  output:
//          loop's parent region
//  Description:
//          del loop from region_tree
//

//=============================================================================
REGION *
REGION_LOOP_UPDATE::Del_Loop_Region(LOOP_REGION *loop){
    Is_True(loop->First_Kid()==NULL, ("loop_region should be leaf region"));
    REGION *par = loop->Parent();
    _region_tree->Del_Region(loop);
    return par;
}

//=============================================================================
//
//  Function: Find_Loop_BBs
//
//  Side_effection:
//          empty _bbs_not_in_loop
//          fill _loop_bbs
//
//  Description:
//          Find all bbs which are not in _bbs_not_in_loop
//
//=============================================================================
void 
REGION_LOOP_UPDATE::Find_Loop_BBs(){
    for (BB *bb = _first_bb; bb != NULL; bb = BB_next(bb)) {  // for each BB in PU
        BOOL find = FALSE;
        for(BB_ITER iter=_bbs_not_in_loop.begin(); iter!=_bbs_not_in_loop.end(); ++iter){
            if((*iter) == bb){
                find = TRUE;
                break;
            }
        }
        if(!find)
            _loop_bbs.push_back(bb);
    }
    _bbs_not_in_loop.clear();
}


//=============================================================================
//  Function: Set_No_Further_Optimization
//
//  input : REGION *par
//
//  Description:
//      for each loop region in par's child , if its bb is scheduled , 
//      set this loop region NO_FURTHER_OPTIMIZATION 
//
//=============================================================================
void 
REGION_LOOP_UPDATE::Set_No_Further_Optimization(REGION *par){
    for(REGION_KID_ITER rgn_iter(par); rgn_iter!=0; ++rgn_iter){
        REGION *rgn = *rgn_iter;
        if(rgn->Region_Type()==LOOP){ 
            for(SEQ_REGIONAL_CFG_ITER node_iter(rgn->Regional_Cfg()); node_iter!=0; ++node_iter){
                if((*node_iter)->Is_Region()) continue;
                BB *bb = (*node_iter)->BB_Node();
                if(BB_scheduled(bb))
                    rgn->Attribute(NO_FURTHER_OPTIMIZATION);
            }
        }
                
        if(rgn->First_Kid())
            Set_No_Further_Optimization(rgn);

    }
}


//=============================================================================
//  Function: Rebuid_Loop_Region
//
//  input : LOOP_REGION *loop
//
//  Description:
//      using _bbs_not_in_loop to get all bbs in loop and all bbs new generated 
//      in loop optimization to rebuild this loop region
//
//=============================================================================
void 
REGION_LOOP_UPDATE::Rebuild_Loop_Region(REGION *par, BOOL succ){
    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY))
        fprintf(TFile, "*** beginning of REGION_LOOP_UPDATE::Rebuild_Loop_Region(parent_region_id :%d)\n",
                par->Id());
    
    Find_Loop_BBs();
    
    // recompute the subtree which BB set is _loop_bbs
    
    if (Get_Trace(TP_A_REGION, TT_RGN_UPDATE_DEBUG)){
        fprintf(TFile,"added bbs are: ");
        for(BB_VECTOR_ITER iter=_loop_bbs.begin(); iter!=_loop_bbs.end(); iter++){
            fprintf(TFile,"%d ", BB_id(*iter)); 
        }
        fprintf(TFile,"\n");
    }

    NODE_VECTOR *new_nodes = NULL;
    new_nodes = CXX_NEW(NODE_VECTOR(), &_m);
	par->Regional_Cfg()->Add_BBS_And_Edges(_loop_bbs, new_nodes);
    
    if (Get_Trace(TP_A_REGION, TT_RGN_UPDATE_DEBUG)){
        fprintf(TFile,"after add bbs and edges \n");
        draw_global_cfg("after add bbs and edges");
        draw_regional_cfg(par);
    }
        
    par->Regional_Cfg()->Print(TFile);
    

    SEME_REGION *seme=_region_tree->Add_SEME_Region(par, *new_nodes);    

    INTERVAL_PROCESSOR inter(seme);
    inter.Process();
   
    // maintain some info about par
    if(par->Region_Type()==LOOP){
        LOOP_REGION *par_loop=(LOOP_REGION *)par;
        for(TOPOLOGICAL_REGIONAL_CFG_ITER iter(par->Regional_Cfg()); iter!=0; ++iter){
            if((*iter)->First_Pred()==NULL)
                par_loop->Loop_Head((*iter));
            if((*iter)->First_Succ()==NULL)
                par_loop->Loop_Tail((*iter));
        }
    }

    // set NO_FURTHER_OPTIMIZATION
    if(succ){
        seme->Attribute(NO_FURTHER_OPTIMIZATION);
        Set_No_Further_Optimization(par);
    }
    
    if (Get_Trace(TP_A_REGION, TT_RGN_UPDATE_DEBUG)){
        fprintf(TFile,"after loop optimization\n");
        draw_regional_cfg(par);
    }

    if (Get_Trace(TP_A_REGION, TT_RGN_SUMMERY))
        fprintf(TFile, "*** end of REGION_LOOP_UPDATE::Rebuild_Loop_Region(parent_region_id :%d)\n",
                par->Id());
}


//=============================================================================
// 
// end of REGION_LOOP_UPDATE class
//
//=============================================================================

//=============================================================================
//  Function: Record_Loop_Region_Boundary
//
//  input : LOOP_DESCR *loop, void *tmp
//
//  Description:
//      used in cg_loop.cxx before loop_optimization to record loop region's 
//      boundary      
//
//=============================================================================
void *Record_And_Del_Loop_Region(LOOP_DESCR *loop, void *tmp){
    REGION_LOOP_UPDATE *rgn_loop_update = (REGION_LOOP_UPDATE *)tmp;
    BB *bb = BB_SET_Choose(LOOP_DESCR_bbset(loop));
    REGION *rgn = Home_Region(bb);    
    
    if (!rgn->Region_Type()==LOOP) {
        DevWarn("Loop Region Been Decomposed.Will Not Do Anything To It In SWP");

        return FALSE;
    }
    
    LOOP_REGION *loop_rgn = (LOOP_REGION *)rgn;
    
    // if loop_rgn is not leaf region, not do loop optimization
    // because when del_region, need maintain entries and exits, which need
    // global cfg's info, del_region can not do after loop optimization
    // in order to keep the shape of region_tree, that is to say, it is same 
    // between before loop opt and after loop opt, only do loop opt when loop 
    // region is leaf region
    if(loop_rgn->First_Kid())
        return NULL;
    
    // if loop_rgn has the entry of its parent region 
    // return NULL (failed)
    // because under this condition, while do rebuild loop region,
    // INTERVAL_PROCESSOR::Find_Cycles will can not work because of every node
    // has more than one pred.  
    if(loop_rgn->Regional_Cfg_Node()->Is_Entry())
        return NULL;
            
    
    rgn_loop_update->Record_Loop_Region_Boundary(loop_rgn);
    void *ret = (void *)(rgn_loop_update->Del_Loop_Region(loop_rgn));
        
    if (Get_Trace(TP_A_REGION, TT_RGN_UPDATE_DEBUG)){
        fprintf(TFile,"before loop opt and after del loop region\n");
        if(ret)
            draw_regional_cfg((REGION *)ret);
    }

    return ret;
}


//=============================================================================
//  Function: Rebuid_Loop_Region
//
//  input : void *tmp_loop_update, void *tmp_loop_rgn
//
//  Description:
//      used in cg_loop.cxx after loop_optimization to rebuild loop region      
//
//=============================================================================
void Rebuild_Loop_Region(void *tmp_loop_update, void *tmp_par_rgn, BOOL succ){
    REGION_LOOP_UPDATE *rgn_loop_update = (REGION_LOOP_UPDATE *)tmp_loop_update;
    REGION *par_rgn = (LOOP_REGION *)tmp_par_rgn;
    rgn_loop_update->Rebuild_Loop_Region(par_rgn, succ);

}


//=============================================================================
// 
//  Function: No_Across_Opt_In_Path  
//
//  Input:
//          REGION *ance // ancrstry region
//          REGION *desc // descendant region
//
//  Output: BOOL
//
//  Decription:
//          Test if there is a region which Attribute is 
//          NO_OPTIMIZATION_ACROSS_BOUNDARY from desc region to ance region, 
//          it is not contains ance region and desc region
//  
//=============================================================================
static BOOL Have_No_Across_Opt_In_Path(REGION *ance, REGION *desc){
    // assert "ance is ancestry of desc"
    Is_True(desc->Is_Contained_By(ance), 
            ("REGION_VERIFY::Have_No_Across_Opt_In_Path"));
    
    REGION *parent = desc->Parent();
    while(parent != ance){
        if(parent->Is_No_Opt_Across())
            return TRUE;
    }
    return FALSE;
}


//=============================================================================
// 
//  Function: Can_Be_Moved 
//
//  Input: 
//          REGION *src       // source region
//          REGION *target    // target region
//
//  Output: BOOL
//
//  Description:
//          test if one op/TN.. can be moved from source region to target region
//
//=============================================================================
static BOOL Can_Be_Moved(REGION *src, REGION *target)
{       
    if (src->Is_No_Opt_Across()|| 
            target->Is_No_Opt_Across())
        return FALSE;

    REGION *parent = src->Find_Common_Parent(target);
    if(parent != src){
        if(Have_No_Across_Opt_In_Path(parent, src))
            return FALSE;
    }
    
    if(parent !=target){
        if(Have_No_Across_Opt_In_Path(parent,target))
            return FALSE;
    }
        
    return true;
}


//=============================================================================
// 
//  Function: Can_Be_Moved
//
//  Input: 
//          BB *src       // source BB
//          BB *target    // target BB
//
//  Output: BOOL
//
//  Description:
//        test if one op/TN.. can be moved from source BB to target BB
//
//=============================================================================
BOOL Can_Be_Moved(BB * src_bb, BB * target_bb){
    return Can_Be_Moved(Home_Region(src_bb), Home_Region(target_bb));
}


