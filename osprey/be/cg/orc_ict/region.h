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

#ifndef  region_INCLUDED
#define  region_INCLUDED

#include "bb.h"
#include "hb_tail_duplication.h"
#include <stack>
#include <vector>
#include <queue>
#include <list>
#include "defs.h"
#include "cxx_memory.h"
#include "timing.h"

//============================================================================
//  region.h
//
//  This file contains the interface for regions.
//  In our design, all regions are formed in a hierarchical manner.
//
//  - The root region is the root of a region tree. 
//  - MEME regions are regions with multiple entries and exits.
//  - SEME regions are regions with only one entry and multiple exits.
//  - Loop regions contain natural loops, which have single entries.
//  - Improper regions are regions with irreducible part or loops with 
//    multiple entries.
//
//============================================================================
extern BB_MAP bb_node_map;
class SCHEDULER;
class REGION;
class REGIONAL_CFG_NODE;
class REGIONAL_CFG_EDGE;
class TOPOLOGICAL_REGIONAL_CFG_ITER;
class INNERMOST_REGION_FIRST_ITER;
class INTERVAL_PROCESSOR; 
class SCC_FINDER;

//=============================================================================
//  Used to record global cycles.
//=============================================================================
struct GLOBAL_CYCLE {
    BB *src;
    BB *dest;
};

//=============================================================================
//  Used to control the region size.
//=============================================================================
struct RGN_SIZE {
    INT32 max_bb_num;
    INT32 max_op_num;
};
//=============================================================================
//  Used to pass region formation parameters.
//=============================================================================
struct RGN_FORM_PAR {
    float    exit_prob;
    float    dup_ratio;
	INT32    max_dup_num;
    RGN_SIZE size;
};

//=============================================================================
//  Data structure defined to record edge's profiling info temporarily.
//=============================================================================
template <class POINT> struct EDGE_PROF;
template <class POINT>
struct EDGE_PROF {
    POINT src;
    POINT dest;
    float freq;
    float prob;
};

//=============================================================================
//  Typedef to facilitate the profiling info iteration.
//=============================================================================
typedef EDGE_PROF<REGIONAL_CFG_NODE *>        NODE_PROF;
typedef mempool_allocator<NODE_PROF>          PROF_ALLOC;
typedef std::vector<NODE_PROF,PROF_ALLOC>          NODE_PROF_VECTOR;
typedef NODE_PROF_VECTOR::iterator            NODE_PROF_VECTOR_ITER;

//=============================================================================
//  Typedef to facilitate the global cycles iteration.
//=============================================================================
typedef mempool_allocator<GLOBAL_CYCLE>          GLOBAL_CYCLE_ALLOC;
typedef std::vector<GLOBAL_CYCLE,GLOBAL_CYCLE_ALLOC>  GLOBAL_CYCLE_VECTOR;
typedef GLOBAL_CYCLE_VECTOR::iterator            GLOBAL_CYCLE_VECTOR_ITER;

extern GLOBAL_CYCLE_VECTOR global_cycles;

typedef mempool_allocator<INT>                 INT_ALLOC;
typedef std::vector<INT,INT_ALLOC>                  INT_VECTOR;
typedef mempool_allocator<REGIONAL_CFG_NODE*>  NODE_ALLOC;
typedef std::vector<REGIONAL_CFG_NODE*,NODE_ALLOC>  NODE_VECTOR;

typedef mempool_allocator<REGION*>             REGION_ALLOC;
typedef std::vector<REGION*,REGION_ALLOC>           REGION_VECTOR;
typedef REGION_VECTOR::iterator                REGION_VECTOR_ITER;
typedef REGION_VECTOR::const_iterator          RGN_VECTOR_CONST_ITER;

typedef mempool_allocator<BB*>                 BB_ALLOC;
typedef std::vector<BB*,BB_ALLOC>                   BB_VECTOR;
typedef BB_VECTOR::iterator                    BB_VECTOR_ITER;
typedef BB_VECTOR::const_iterator              BB_VECTOR_CONST_ITER;
typedef mempool_allocator<REGIONAL_CFG_EDGE*>  EDGE_ALLOC;  
typedef std::vector<REGIONAL_CFG_EDGE*,EDGE_ALLOC>  EDGE_VECTOR;
typedef EDGE_VECTOR::iterator                  EDGE_VECTOR_ITER;
typedef NODE_VECTOR::iterator                  NODE_VECTOR_ITER;
typedef std::stack<REGIONAL_CFG_NODE*,NODE_VECTOR>  NODE_STACK;
typedef std::stack<REGIONAL_CFG_EDGE*,EDGE_VECTOR>  EDGE_STACK;

//=============================================================================
//  Region Types
//  - ROOT region: The root node of region tree,which is the outermost region.
//  - MEME region: region with multiple entries and multiple exits.
//  - SEME region: region with unique entry node.Entry node may be region or BB.
//  - IMPROPER region: region with irreducible control flow,which is difficult
//    to be explored optimization opportunities.
//  - LOOP region: loops.
//
//=============================================================================
enum REGION_TYPE {
        UNKNOWN  = 0x00,
        ROOT     = 0x01,
        MEME     = 0x02,
        SEME     = 0x04,
        IMPROPER = 0x08,
        LOOP     = 0x10
        //RECOVERY_REGION
     };

     
//=============================================================================
//  REGION_ATTRIBUTE
//
//  regions may have four restrictive attributes:
//  - NO FURTHER OPTIMIZATION attribute means that no further optimization 
//    can be done to this region.
//  - RIGID means it can not be decomposed.
//  - PERSISTENT BOUNDARY: the region's boundary can not be changed,i.e,its
//    exit and entry nodes can not be changed. 
//  - NO OPTIMIZATION ACROSS BOUNDARY: no optimization across region boundary,
//    for example,code motion from one region to another.
//
//=============================================================================
enum REGION_ATTRIBUTE {
        NONE                           = 0x00,
        NO_FURTHER_OPTIMIZATION        = 0x01,
        RIGID                          = 0x02,
        PERSISTENT_BOUNDARY            = 0x04,
        NO_OPTIMIZAION_ACROSS_BOUNDARY = 0x08
     };

extern  BB *Copy_BB_For_Tail_Duplication(HB *hb,BB *old_bb);

//=============================================================================
//  Functions used to create a map from every BB to its corresponding regional 
//  cfg node.
//=============================================================================
void    Create_BB_Node_Map(void);
void    Delete_BB_Node_Map(void);

//=============================================================================
//  Functions used to find a node in a BB(NODE) vector;if found,the iterator is
//  returned,otherwise return NULL.
//=============================================================================
NODE_VECTOR_ITER    Find_In_Vector(REGIONAL_CFG_NODE *node,
                                   NODE_VECTOR& nodes); 
inline BB_VECTOR_ITER      Find_In_BB_Vector(BB *bb,BB_VECTOR& bbs);

inline void    Print_Node_Vector(NODE_VECTOR nodes);
inline void    Print_Ops_In_BB(BB *bb);
inline void    Print_BB_Vector(BB_VECTOR bbs);
//=============================================================================
//  TEMP_RGN
//
//  Data structure used to facilitate the region formation in parameter 
//  transfering.
//  - NODES:Nodes contained by the selected scope.
//  - Main entry and main exit of the selected scope.
//  - SEED:The seed node selected during forming a SEME or MEME region,this 
//    node may be erased from the nodes in SEME region formation process.
//  - BB NUM and OP NUM:Number of BBs and OPS of the selected nodes.
//  - The Number of duplicated BBs and OPs during tail duplication phase in  
//    SEME region formation.
//  - Frequency info of the seed node and Exit Prob means the completion prob
//    of the main exit node.
//
//=============================================================================
struct TEMP_RGN {
    NODE_VECTOR nodes;  
	REGIONAL_CFG_NODE  *main_entry;
	REGIONAL_CFG_NODE  *main_exit;
    REGIONAL_CFG_NODE  *seed;
	INT32 bb_num;
	INT32 op_num;
    INT32 dup_bb_num;
	INT32 dup_op_num;
    float seed_freq;
	float exit_prob;
};

inline void Initialize_Temp_Rgn(TEMP_RGN& temp_rgn,MEM_POOL _m); 
//=============================================================================
//
//  Class Name: REGIONAL_CFG_EDGE
//
//  Base Class: <none>
//
//  Derived Class: <none>
//
//  Class Description:Edges in regional control flow graph.
//
//==============================================================================   
class REGIONAL_CFG_EDGE {

friend class REGIONAL_CFG_NODE;
friend class REGIONAL_CFG;
friend class SCHEDULER;

private:
    REGIONAL_CFG_EDGE *_next_succ;
    REGIONAL_CFG_EDGE *_next_pred;
    REGIONAL_CFG_NODE *_src;
    REGIONAL_CFG_NODE *_dest;
    
    float _freq; 
    float _prob;

    void Next_Succ(REGIONAL_CFG_EDGE *e) { _next_succ = e; }
    void Next_Pred(REGIONAL_CFG_EDGE *e) { _next_pred = e; }

    float Freq(void)       { return _freq; }  
    void  Freq(float freq) { _freq = freq; } 
    
    void  Prob(float prob) { _prob = prob; }

public:

    float Prob(void) const { return _prob; }
    
    REGIONAL_CFG_EDGE(REGIONAL_CFG_NODE *v, REGIONAL_CFG_NODE *w) {    
        _src   =  v;
        _dest  =  w;
        _prob  =  0;
        _freq  =  0;
        _next_succ = 0;
        _next_pred = 0;
    }
    
    ~REGIONAL_CFG_EDGE() {}
    
    REGIONAL_CFG_EDGE *Next_Succ(void)       { return _next_succ; };  
    REGIONAL_CFG_EDGE *Next_Pred(void)       { return _next_pred; };  
    REGIONAL_CFG_NODE *Src(void)             { return _src;  };
    REGIONAL_CFG_NODE *Dest(void)            { return _dest; };
    
    void  Print(FILE *f = stderr);
};

//=============================================================================
//
//  Class Name: REGIONAL_CFG_NODE
//
//  Base Class: <none>
//
//  Derived Class: <none>
//
//  Class Description: Nodes in regional control flow graph,every node represent
//  either a basic block or a region nesting in the home region.
//
//=============================================================================   
class REGIONAL_CFG_NODE {
   
friend class REGIONAL_CFG;
friend class SCHEDULER;
friend BB *RGN_Divide_BB(BB *bb, OP *point, BOOL force);
private:    
    union source_node {
        REGION *r;
        BB     *bb;
    } _node;
    
    INT32              _id;
    INT32              _n_succ;
    INT32              _n_pred;
    
    BOOL               _is_region;
    BOOL               _is_exit;
    BOOL               _is_entry;
    
    REGIONAL_CFG_EDGE  *_first_succ;
    REGIONAL_CFG_EDGE  *_first_pred;
    REGION             *_home_r;
    float               _freq;
	
    void Connect_Succ_Edge(REGIONAL_CFG_EDGE *e) {
        ++_n_succ;
        e->Next_Succ(_first_succ);
        _first_succ = e; 
    }
    
    void Connect_Pred_Edge(REGIONAL_CFG_EDGE *e) { 
        ++_n_pred;
        e->Next_Pred(_first_pred);
        _first_pred = e; 
    }

    void Disconnect_Succ_Edge(REGIONAL_CFG_EDGE *e) {
        REGIONAL_CFG_EDGE *t;
       
        if (_first_succ == e) {
            _first_succ = _first_succ->Next_Succ();
            --_n_succ;
            return;
        }
        
        for (t = _first_succ; (t != 0)&&(t->Next_Succ() != e); 
        t = t->Next_Succ());
        
        if (t != 0) {
            t->Next_Succ(e->Next_Succ());
            --_n_succ;
        }    
    }

    void Disconnect_Pred_Edge(REGIONAL_CFG_EDGE *e) {
        REGIONAL_CFG_EDGE *t;
        
        if (_first_pred == e) {
            _first_pred = _first_pred->Next_Pred();
            --_n_pred;
            return;
        }
        
        for (t = _first_pred; (t != 0)&&(t->Next_Pred() != e); 
        t = t->Next_Pred());
        
        if (t != 0) {
            t->Next_Pred(e->Next_Pred());
            --_n_pred;   
        } 
    }

    void  Freq(float freq) { _freq = freq; } 
    
    void                Is_Entry(BOOL is) { _is_entry = is;      }
    void                Is_Exit(BOOL is)  { _is_exit  = is;      }  
    void                Set_Is_Entry(BOOL is) { _is_entry = is;      }
    void                Set_Is_Exit(BOOL is)  { _is_exit  = is;      }
public:

	REGIONAL_CFG_NODE(BB *bb) {
        _node.bb = bb;
        _n_succ     = _n_pred     = 0;
        _first_succ = _first_pred = NULL;
        _is_exit    = _is_entry   = FALSE;
        _is_region  = FALSE;
        _home_r     = NULL;
    }
    
    REGIONAL_CFG_NODE(REGION *r) {
        _node.r = r;
        _n_succ     = _n_pred     = 0;
        _first_succ = _first_pred = NULL;
        _is_exit    = _is_entry   = FALSE;
        _is_region  = TRUE;
        _home_r     = NULL;
    }
    
    ~REGIONAL_CFG_NODE() {}
      
    REGION *Region_Node(void) {
        Is_True((_is_region == TRUE), ("REGIONAL_CFG_NODE::Node is not a region")); 
         
        return _node.r;
    }
        
    BB *BB_Node(void) {
        
        Is_True((_is_region == FALSE), ("REGIONAL_CFG_NODE::Node %d is not a bb",_id)); 
       
        return _node.bb;
    }
    
    REGIONAL_CFG_EDGE  *Find_Succ_Edge(REGIONAL_CFG_NODE *node) {
        REGIONAL_CFG_EDGE *e;
        
        for (e =_first_succ; (e->Dest() != node)&&(e != 0);
        e = e->Next_Succ());
        
        return e;
    }        
    
    REGIONAL_CFG_EDGE  *Find_Pred_Edge(REGIONAL_CFG_NODE *node) {
        REGIONAL_CFG_EDGE *e;
        
        for (e =_first_pred; (e->Src() != node)&&(e != 0);
        e = e->Next_Pred());
        
        return e;
    }
        //-------------------------------------------------------
	//  This function decides whether node has a unique succ,
	//  if it does,return the succ,otherwise return NULL.
	//-------------------------------------------------------
	REGIONAL_CFG_NODE   *Unique_Succ(void) {
	    if (_n_succ == 1) {
		    return _first_succ->Dest();
        }
	
		return NULL;
    }
        //-------------------------------------------------------
	//  This function decides whether node has a unique succ, 
	//  if it does,return the succ,otherwise return NULL. 
	//-------------------------------------------------------
	REGIONAL_CFG_NODE   *Unique_Pred(void) {
	    if (_n_pred == 1) {
		    return _first_pred->Src();
        }
	
		return NULL;
    }
    //---------------------------------------------------
    // The number of its successor or predecessor nodes.
    //---------------------------------------------------
    INT32                Succ_Num(void)    const { return _n_succ; } 
    INT32                Pred_Num(void)    const { return _n_pred; }  
    //---------------------------------------------------------------
    // Node id is unique in a region identifing a regional cfg node.
    //---------------------------------------------------------------
    INT32                Id(void)          const { return _id;     }  
      
    REGIONAL_CFG_EDGE   *First_Succ(void)  const { return _first_succ;  }
    REGIONAL_CFG_EDGE   *First_Pred(void)  const { return _first_pred;  }    
    BOOL                 Is_Region(void)   const { return _is_region;   } 
    //---------------------------------------
    // The region in which this node nests.
    //---------------------------------------
	REGION              *Home_Region(void) const { return _home_r;      } 
    //-------------------------------------------------------------
    // Whether the node is a entry or exit node of its home region.
    //-------------------------------------------------------------
    BOOL                 Is_Entry(void)    const { return _is_entry;    }
    BOOL                 Is_Exit(void)     const { return _is_exit;     }
    void                 Id(INT32 id)      { _id = id; }     
    float                 Freq(void)       { return _freq; }  

    void  Print(FILE *f = stderr);
	BOOL  Is_Loop_Head(void); 
	BOOL  Is_Loop_Tail(void); 

};

//============================================================================
//
//   Class Name: REGIONAL_CFG_MEM
//
//   Base Class: <none>
//
//   Derived Class: REGIONAL_CFG
//
//   Class Description: Calss used for memory allocation of regional cfg.
//    
//============================================================================   

class REGIONAL_CFG_MEM {

protected:
    MEM_POOL _m;

    REGIONAL_CFG_MEM() {
         MEM_POOL_Initialize( &_m, "REGIONAL_CFG_MEM", true );
         MEM_POOL_Push( &_m );
    }
    
    ~REGIONAL_CFG_MEM() {
        MEM_POOL_Pop( &_m );
        MEM_POOL_Delete(&_m );
    }
};
        
//============================================================================
//
//  Class Name: REGIONAL_CFG
//
//  Base Class: REGIONAL_CFG_MEM
//
//  Derived Class: <none>
//
//  Class Description: A region's regional cfg.Most region formation related 
//  functions are contained by this class.
//
//============================================================================   
class REGIONAL_CFG : public REGIONAL_CFG_MEM{

friend class TOPOLOGICAL_REGIONAL_CFG_ITER;
friend class PREORDER_REGIONAL_CFG_ITER;

friend class REGION;
friend class REGION_TREE;
friend class INTERVAL_PROCESSOR;
friend class SCC_FINDER;
friend class REVERSE_TOPO_REGIONAL_CFG_ITER;
friend class SEQ_REGIONAL_CFG_ITER;
friend class REGION_LOOP_UPDATE;

friend class SCHEDULER;
friend class CG_VALUE_INSTRUMENT_WALKER;
//------------------------------------------------------------
// some functions for maintainance of regional cfg as friends
//------------------------------------------------------------
friend void RGN_Gen_And_Insert_Node(BB *new_bb,BB *pred_bb, BB *succ_bb, 
                                    REGIONAL_CFG *regional_cfg);
friend BB   *RGN_Gen_And_Insert_BB_After(BB *point,REGIONAL_CFG *regional_cfg);
friend BB   *RGN_Gen_And_Insert_BB_Before(BB *point, REGIONAL_CFG *regional_cfg);
friend void RGN_Remove_BB_And_Edges(BB *bb, REGIONAL_CFG *regional_cfg);
friend void RGN_Unlink_BB_Edges(BB *bb, REGIONAL_CFG *regional_cfg);
friend void Add_Regional_Cfg_Edge(REGIONAL_CFG_NODE *pred, REGIONAL_CFG_NODE *succ, REGION *rgn);
friend void Del_Regional_Cfg_Edge(REGIONAL_CFG_NODE *pred, REGIONAL_CFG_NODE *succ, REGION *rgn);
friend void RGN_Link_Pred_Succ_With_Prob(BB *pred, BB *succ, float prob,
                  REGIONAL_CFG *regional_cfg);
friend void RGN_Unlink_Pred_Succ(BB *pred, BB *succ, REGIONAL_CFG *regional_cfg);
friend void RGN_Add_Regional_Cfg_Edge(BB *pred,BB *succ,REGIONAL_CFG *cfg);
friend void RGN_Del_Regional_Cfg_Edge(BB *pred,BB *succ,REGIONAL_CFG *cfg);
friend void Collect_Entry_BBs(REGION *rgn, BB_VECTOR *entries);
friend void Collect_Exit_BBs(REGION *rgn, BB_VECTOR *exits);
friend BB *RGN_Divide_BB(BB *bb, OP *point, BOOL force);
//------------------------------------
// region verify functions as friends
//------------------------------------
friend void Verify_Cfg(REGIONAL_CFG *cfg);
friend void Verify_SEME_Region(REGION *r);
friend void Verify_Node(REGIONAL_CFG_NODE *node,REGION *rgn);
friend INT  Edge_Counter(REGIONAL_CFG_NODE *src,REGIONAL_CFG_NODE *dest,REGIONAL_CFG *cfg);

private:    
    NODE_VECTOR   _exits;
    NODE_VECTOR   _entries;
    //------------------------------------------------------------
    // node set contains all nodes contained by the regional cfg. 
    //------------------------------------------------------------
    NODE_VECTOR   _node_set;
    //-------------------------------------------------------------------------
    // _seq_num is the sequential number of node contained by this regional_cfg,
    // which starts from 0,used for facilitating the node id generation.
    //-------------------------------------------------------------------------
    INT32         _seq_num;   
     
    REGION       *_r; 
    BOOL          _exits_entries_valid;
    //--------------------------------------------------------------------------
    // Once the regional control flow graph is changed,this field will
    // be set invalid.
    //----------------------------------------------------------------------
    BOOL          _freq_valid;
  
    //=========================================================================
    //  
    //    Insert_Node
    //
    //    Insert a regional cfg node to the node set which contains all 
    //    regional cfg nodes in the regional cfg.  
    //
    //=========================================================================
    void Insert(REGIONAL_CFG_NODE *node) {
        _node_set.push_back(node);
        _seq_num++;
        node->Id(_seq_num);
        node->_home_r = _r;
    }    
        
    //========================================================================
    //  Erase
    //
    //  This function only erase a regional cfg node from the regional 
    //  cfg,and the erased node will be put back to its parent region.
    //
    //=========================================================================
    void Erase(REGIONAL_CFG_NODE *node) {
        NODE_VECTOR_ITER iter;
        
        for (iter = _node_set.begin(); iter != _node_set.end(); iter++) {

            if (*iter == node) {
                _node_set.erase(iter);

                break;
            }
        }
        
        node->_home_r = NULL;
    }

    //========================================================================
    //
    //  Add_Node
    //  
    //  Add a new BB or REGION in local cfg.In these two functions,new 
    //  regional cfg node will be created. 
    //
    //========================================================================
    REGIONAL_CFG_NODE *Add_Node(REGION *r); 
      
    REGIONAL_CFG_NODE *Add_Node(BB *bb);
    
    void Del_Node(REGIONAL_CFG_NODE *v); 

    //========================================================================
    //
    //  Add_Edge
    //  
    //  This function create a edge of node v and w.And this edge added to
    //  v's succ edge set and w's pred edge set.  
    //
    //========================================================================
    REGIONAL_CFG_EDGE *Add_Edge(REGIONAL_CFG_NODE *v, REGIONAL_CFG_NODE *w) {
        
        for (REGIONAL_CFG_EDGE *succ = v->_first_succ;succ != NULL;succ = succ->_next_succ) {
            if ((succ->Dest()) == w) {
                return succ;
            } 
         }
         
         REGIONAL_CFG_EDGE *e = CXX_NEW(REGIONAL_CFG_EDGE(v, w),&_m);
         v->Connect_Succ_Edge(e);
         w->Connect_Pred_Edge(e);
         //When cfg updated,the freq information set to FALSE.
         _freq_valid = FALSE;

         return e;
    }
    
    //========================================================================
    //
    //  Del_Edge
    //  
    //  Permanently delete the edge.
    //
    //========================================================================
    void Del_Edge(REGIONAL_CFG_EDGE *e) {
        e->Src()->Disconnect_Succ_Edge(e);
        e->Dest()->Disconnect_Pred_Edge(e);
        CXX_DELETE(e,&_m);
	//---------------------------------------------------- 
        //When cfg updated,the freq information set to FALSE.
        //----------------------------------------------------
	_freq_valid = FALSE;
    }    
    
    //========================================================================
    // See region.cxx for interface description.
    //========================================================================
    void               Find_MEME_Nodes(TEMP_RGN& meme_rgn,RGN_SIZE size,
                       NODE_VECTOR bad_nodes);

    //========================================================================
    // See region.cxx for interface description.
    //========================================================================
    void               Find_SEME_Nodes(TEMP_RGN& seme_rgn,RGN_FORM_PAR par,
                       NODE_VECTOR bad_nodes);

    //========================================================================
    // See region.cxx for interface description.
    //========================================================================
    float              Compute_Completion_Prob(NODE_VECTOR meme_nodes,
                       REGIONAL_CFG_NODE *exit,
                       REGIONAL_CFG_NODE *main_entry);

    //========================================================================
    // See region.cxx for interface description.
    //========================================================================
    void               Compute_Scope_Based_On_Main_Exit(NODE_VECTOR& temp_seme,
                       NODE_VECTOR meme_nodes,REGIONAL_CFG_NODE *exit);

    //========================================================================
    // See region.cxx for interface description.
    //========================================================================
    float              Compute_Duplicate_Ratio(NODE_VECTOR nodes,
	               NODE_VECTOR& duplicated,	
                       INT32 max_bb_num,INT32 max_dup_num);

    //========================================================================
    // See region.cxx for interface description.
    //======================================================================== 
    void               Tail_Duplicate(NODE_VECTOR& nodes,
	               REGIONAL_CFG_NODE *main_entry,
                       INT32& dup_bb_num,INT32& dup_op_num,BOOL re_shrink);

    //========================================================================
    // See region.cxx for interface description.
    //========================================================================  
    void               Compute_Edges_Freq(void);
    
    //========================================================================
    // See region.cxx for interface description.
    //========================================================================  
    void               Compute_Side_Entries(NODE_VECTOR& side_entries,
                       NODE_VECTOR meme_nodes,REGIONAL_CFG_NODE *main_entry);

    //========================================================================
    //  See region.cxx for interface description.
    //========================================================================  
    REGIONAL_CFG_NODE *Find_Seed(NODE_VECTOR bad_nodes);
    
    //========================================================================
    //  See region.cxx for interface description.
    //========================================================================
    float              Compute_Nodes_Weight(NODE_VECTOR nodes);
    
    //========================================================================
    //  See region.cxx for interface description.
    //========================================================================
    INT32              Do_Selective_Add(NODE_VECTOR& nodes,
                       NODE_VECTOR duplicate,NODE_VECTOR& leaf_nodes,
                       REGIONAL_CFG_NODE *main_exit,
                       REGIONAL_CFG_NODE *main_entry,
                       INT32 op_num,RGN_FORM_PAR par);
    //========================================================================
    //  See region.cxx for interface description.
    //=========================================================================
    BOOL               Do_Selective_Cut(NODE_VECTOR& nodes,
                       REGIONAL_CFG_NODE *main_entry,
                       REGIONAL_CFG_NODE *main_exit,
                       RGN_FORM_PAR par);
    
    //========================================================================
    //  See region.cxx for interface description.
    //=========================================================================
    void               Compute_Exits(NODE_VECTOR nodes,NODE_VECTOR& exits);

    //========================================================================
    //  See region.cxx for interface description.
    //=========================================================================
    void               Compute_Nodes_To_Be_Duplicated(NODE_VECTOR& dup,
                       NODE_VECTOR nodes,REGIONAL_CFG_NODE *main_entry);
    
    //========================================================================
    // See region.cxx for interface description.
    //=========================================================================
    INT32              Compute_Num_Of_Ops(NODE_VECTOR nodes,BOOL is_dup);

    //========================================================================
    // See region.cxx for interface description.
    //=========================================================================
    void               Compute_BBs_In_Region_Node(BB_VECTOR& bbs,REGIONAL_CFG_NODE *node);

    //========================================================================
    // See region.cxx for interface description. 
    //=========================================================================
    void               Find_BBs_From_Nodes(BB_VECTOR& bbs,NODE_VECTOR nodes);
    
    //========================================================================
    // See region.cxx for interface description. 
    //=========================================================================
    void               Compute_BBs_Need_Duplicate(BB_VECTOR& need_duplicate,
                       BS *unduplicated,NODE_VECTOR dup);
    
    //========================================================================
    //  
    //  Duplicate
    //
    //  Duplicate the bb.
    //
    //=========================================================================
    void               Duplicate(BB_VECTOR bbs, BB* side_entrance,
                       BB *entry,BB_MAP duplicate,BB_MAP rev_duplicate,
                       BB** last,BB_VECTOR& duplicate_bbs,BB_VECTOR& br_bbs);

    //========================================================================
    // See region.cxx for interface description.
    //=========================================================================
    void               Fixup_Arcs(BB_VECTOR& bbs,BB *old_bb,BB *new_bb,
                       BB *entry,BB_MAP duplicate,BB **fall_thru,
                       BB_VECTOR& duplicate_bbs,BB_VECTOR& br_bbs);

    //=========================================================================
    // See region.cxx for interface description.
    //=========================================================================

    void               Update_BB_Prof_Info(BB_VECTOR bbs,BB_VECTOR dup,
                       BB_MAP duplicate,BB_MAP rev_duplicate,
                       BB_VECTOR br_bbs,BB *main_entry);
    
    //========================================================================
    //  See region.cxx for interface description.
    //=========================================================================
    void               Compute_Node_Prof_Info(NODE_VECTOR nodes,NODE_VECTOR dup,
                       REGIONAL_CFG_NODE *main_entry,
                       NODE_PROF_VECTOR& node_profs);
    
    void               Add_BBS_And_Edges(BB_VECTOR bbs,
                       NODE_VECTOR *new_nodes = NULL);

    REGIONAL_CFG_EDGE *Select_Freq_Succ(REGIONAL_CFG_NODE *node,
                       NODE_VECTOR nodes);
    
    REGIONAL_CFG_EDGE *Select_Freq_Pred(REGIONAL_CFG_NODE *node,                                       NODE_VECTOR nodes,NODE_VECTOR bad_nodes);

    REGIONAL_CFG_NODE *Select_Freq_Connected_Node(NODE_VECTOR nodes,                                   REGIONAL_CFG_NODE *last_node);
    
    BOOL               Succ_Suit(REGIONAL_CFG_EDGE *edge,float seed_prob,                              float T,float Ts);
    
    BOOL               Pred_Suit(REGIONAL_CFG_EDGE *edge,float seed_prob,
                       float T,float Ts);
    
    BOOL               Is_Unwanted_Node(REGIONAL_CFG_NODE *node);
    BOOL               BB_Not_Suit_Duplicate(BB *bb);  
    void               Add_To_Exits(REGIONAL_CFG_NODE *node);
    void               Add_To_Entries(REGIONAL_CFG_NODE *node);
    void               Remove_From_Exits(REGIONAL_CFG_NODE *node);
    void               Remove_From_Entries(REGIONAL_CFG_NODE *node); 

    INT                Num_Of_Entries(void) { return _entries.size(); }   
    INT                Num_Of_Exits(void)   { return _exits.size();   }
    
    BOOL               ARN_Is_Log_If(BB *bb);
public:    
    REGIONAL_CFG():
        _node_set(NODE_ALLOC(&_m)),
        _exits(NODE_ALLOC(&_m)),
        _entries(NODE_ALLOC(&_m)) { 
        _seq_num    = 0;
        _freq_valid = FALSE; } 
        
    ~REGIONAL_CFG() {}
    
    float  Edge_Freq(REGIONAL_CFG_EDGE *edge) {
        if (_freq_valid) {
            
            return edge->Freq();
        } else {
            Compute_Edges_Freq();
            _freq_valid = TRUE;

            return edge->Freq();
        }
    }
    
    float  Edge_Prob(REGIONAL_CFG_EDGE *edge) {
        if (_freq_valid) {
            return edge->Prob();
        } else {
            Compute_Edges_Freq();
            _freq_valid = TRUE;

            return edge->Prob();
        }
    }
    
    float Node_Freq(REGIONAL_CFG_NODE *node) {
        if (_freq_valid) {
            return node->Freq();
        } else {
            Compute_Edges_Freq();
            _freq_valid = TRUE;

            return node->Freq();
        }
    }

    REGIONAL_CFG_EDGE  *Find_Edge(REGIONAL_CFG_NODE *v,REGIONAL_CFG_NODE *w);           
    
    NODE_VECTOR        Node_Set(void) { return _node_set; }
    INT32              Seq_Num(void)  { return _seq_num;  }
       
    void               Print(FILE *f = stderr);
};

//============================================================================
//
//   Class Name: CFG_SUCC_NODE_ITER
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//   
//   Note:  
//
//============================================================================   

class CFG_SUCC_NODE_ITER {

private:
    REGIONAL_CFG_EDGE *_cur;

public:
    CFG_SUCC_NODE_ITER(REGIONAL_CFG_NODE *v)   {
       _cur = v->First_Succ();
    }
    
    ~CFG_SUCC_NODE_ITER() {} 
    
    CFG_SUCC_NODE_ITER &operator ++(void) {
        _cur = _cur->Next_Succ();
        return *this; 
    }
  
    REGIONAL_CFG_NODE *operator *(void) {
        return _cur->Dest(); 
    }
  
    friend BOOL operator ==(const CFG_SUCC_NODE_ITER& x, 
        const CFG_SUCC_NODE_ITER& y) {
        return x._cur == y._cur; 
    }
    
    BOOL operator !=(INT){
        return _cur!=0;
    }
};

//============================================================================
//
//   Class Name: CFG_PRED_NODE_ITER
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//   
//   Note:  
//
//============================================================================   

class CFG_PRED_NODE_ITER {

private:
    REGIONAL_CFG_EDGE *_cur;

public:
    CFG_PRED_NODE_ITER(REGIONAL_CFG_NODE *v)   {
        _cur = v->First_Pred();
    }
    
    ~CFG_PRED_NODE_ITER() {}

    CFG_PRED_NODE_ITER &operator ++(void) {
        _cur = _cur->Next_Pred();
        return *this; 
    }
  
    REGIONAL_CFG_NODE *operator *(void) {
        return _cur->Src(); 
    }
  
    friend BOOL operator ==(const CFG_PRED_NODE_ITER& x,
        const CFG_PRED_NODE_ITER& y) {
        return x._cur == y._cur; 
    }

    BOOL operator !=(INT) {
        return _cur!=0;
    }
};

//============================================================================
//
//   Class Name: CFG_PRED_EDGE_ITER
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//   
//   Note:  
//
//============================================================================   

class CFG_PRED_EDGE_ITER {

private:
    REGIONAL_CFG_EDGE *_cur;

public:
    CFG_PRED_EDGE_ITER(REGIONAL_CFG_NODE *v)   {
        _cur = v->First_Pred();
    }
    
    ~CFG_PRED_EDGE_ITER() {}

    CFG_PRED_EDGE_ITER &operator ++(void) {
        _cur = _cur->Next_Pred();
        return *this; 
    }
  
    REGIONAL_CFG_EDGE *operator *(void) {
        return _cur; 
    }
  
    friend BOOL operator ==(const CFG_PRED_EDGE_ITER& x, 
        const CFG_PRED_EDGE_ITER& y) {
        return x._cur == y._cur; 
    }

    BOOL operator !=(INT) {
        return _cur != 0;
    }
};

//============================================================================
//
//   Class Name: CFG_SUCC_EDGE_ITER
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//   
//   Note:  
//
//============================================================================   

class CFG_SUCC_EDGE_ITER {

private:
    REGIONAL_CFG_EDGE *_cur;

public:
    CFG_SUCC_EDGE_ITER(REGIONAL_CFG_NODE *v)   {
        _cur = v->First_Succ();
    }
    
    ~CFG_SUCC_EDGE_ITER() {}

    CFG_SUCC_EDGE_ITER &operator ++(void) {
        _cur = _cur->Next_Succ();
        return *this; 
    }
  
    REGIONAL_CFG_EDGE *operator *(void) {
        return _cur; 
    }
  
    friend BOOL operator ==(const CFG_SUCC_EDGE_ITER& x,
        const CFG_SUCC_EDGE_ITER& y) {
        return x._cur == y._cur; 
    }

    BOOL operator !=(INT) {
        return _cur != 0;
    }
};

//============================================================================
//
//   Class Name: TOPOLOGICAL_REGIONAL_CFG_ITER
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//    
//     Iterators for regional cfg.It will traverse regional cfg nodes in a 
//     topological order.A topological order means first traverse nodes with-
//     out preds not visited.
//
//   Note: The input cfg can not be cyclic cfg. 
//
//============================================================================   
class TOPOLOGICAL_REGIONAL_CFG_ITER_MEM {

protected:
    MEM_POOL _m;

    TOPOLOGICAL_REGIONAL_CFG_ITER_MEM() {
         MEM_POOL_Initialize( &_m, "TOPOLOGICAL_REGIONAL_CFG_ITER_MEM", true );
         MEM_POOL_Push( &_m );
    }
    
    ~TOPOLOGICAL_REGIONAL_CFG_ITER_MEM() {
        MEM_POOL_Pop( &_m );
        MEM_POOL_Delete(&_m );
    }
};

class  TOPOLOGICAL_REGIONAL_CFG_ITER : public TOPOLOGICAL_REGIONAL_CFG_ITER_MEM {

typedef std::list<REGIONAL_CFG_NODE *,NODE_ALLOC>              NODE_LIST;
typedef std::queue<REGIONAL_CFG_NODE *,NODE_LIST>              NODE_QUEUE;

private:    
    BS*                 _visited_s;
    NODE_QUEUE          _candi_s;   
    REGIONAL_CFG_NODE  *_cur;
 
     
    BOOL Visited(REGIONAL_CFG_NODE *node) {
        return BS_MemberP(_visited_s,node->Id());
    } 
    
    void Set_Visited(REGIONAL_CFG_NODE *node) {
        Is_True (node != NULL, ("Hey!, <node> is NULL pointer"));
        if (node) {
            BS_Union1D(_visited_s,node->Id(),&_m);
        } 
    }
        
    void Set_Cur(REGIONAL_CFG *cfg);
     
public:    
    TOPOLOGICAL_REGIONAL_CFG_ITER(REGIONAL_CFG *cfg):
      _candi_s(NODE_LIST(NODE_ALLOC(&_m))){
          _visited_s = BS_Create_Empty(cfg->_seq_num+2, &_m);
          Set_Cur(cfg);
    }
   
    ~TOPOLOGICAL_REGIONAL_CFG_ITER() {} 


    TOPOLOGICAL_REGIONAL_CFG_ITER &operator ++(void);
    BOOL                          operator!=(INT)     { return _cur != NULL;}
    REGIONAL_CFG_NODE             *operator *(void)   { return _cur; }
 
};

//============================================================================
//
//   Class Name: REVERSE_TOPO_REGIONAL_CFG_ITER
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//    
//     Iterators for regional cfg.It will traverse regional cfg nodes in a 
//     reverse topological order.A reverse topological order means first 
//     traverse nodes without succs not visited.
//
//   Note: The input cfg can not be cyclic cfg. But it can be multi entries cfg 
//
//============================================================================   
class  REVERSE_TOPO_REGIONAL_CFG_ITER {

typedef std::list<REGIONAL_CFG_NODE *,NODE_ALLOC>              NODE_LIST;
typedef std::queue<REGIONAL_CFG_NODE *,NODE_LIST>              NODE_QUEUE;

private:    
    NODE_VECTOR         _visited_s;
    NODE_QUEUE          _candi_s; 
    REGIONAL_CFG_NODE  *_cur;
    
    BOOL Visited(REGIONAL_CFG_NODE *node) {
        NODE_VECTOR_ITER iter;     
        
        for (iter = _visited_s.begin(); iter != _visited_s.end(); iter++) {
             if (*iter == node) return TRUE;
         }
        
        return FALSE;
    } 
    
    void Set_Visited(REGIONAL_CFG_NODE *node) {
        _visited_s.push_back(node);
    }
        
    void Set_Cur(REGIONAL_CFG *cfg);
     
public:    
    REVERSE_TOPO_REGIONAL_CFG_ITER(REGIONAL_CFG *cfg):
      _visited_s(NODE_ALLOC(&(cfg->_m))),
      _candi_s(NODE_LIST(NODE_ALLOC(&(cfg->_m)))) {
          Set_Cur(cfg);
    }
   
    ~REVERSE_TOPO_REGIONAL_CFG_ITER() {} 


    REVERSE_TOPO_REGIONAL_CFG_ITER  &operator ++(void);
    BOOL                            operator!=(INT)     { return _cur != NULL;}
    REGIONAL_CFG_NODE               *operator *(void)   { return _cur; }
 
};

//============================================================================
//
//   Class Name: PREORDER_REGIONAL_CFG_ITER
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//    
//     Iterators for regional cfg.It will traverse regional cfg nodes in a 
//     preorder.
//
//   Note: The regional cfg can have cycles in it.
//
//============================================================================   
class  PREORDER_REGIONAL_CFG_ITER {

typedef std::list<REGIONAL_CFG_NODE *,NODE_ALLOC>              NODE_LIST;
typedef std::queue<REGIONAL_CFG_NODE *,NODE_LIST>              NODE_QUEUE;

private:    
    NODE_VECTOR         _visited_s;
    NODE_QUEUE          _candi_s;   
    REGIONAL_CFG_NODE   *_cur;
    
    BOOL Visited(REGIONAL_CFG_NODE *node) {
        NODE_VECTOR_ITER iter;     
        
        for (iter = _visited_s.begin(); iter != _visited_s.end(); iter++) {
             if (*iter == node) return TRUE;
         }
        
        return FALSE;
    } 
    
    void Set_Visited(REGIONAL_CFG_NODE *node) {
        _visited_s.push_back(node);
    }
    void Set_Cur(REGIONAL_CFG *cfg);
     
public:    
    PREORDER_REGIONAL_CFG_ITER(REGIONAL_CFG *cfg):
      _visited_s(NODE_ALLOC(&(cfg->_m))),
      _candi_s(NODE_LIST(NODE_ALLOC(&(cfg->_m)))){
          Set_Cur(cfg);
    }
   
    ~PREORDER_REGIONAL_CFG_ITER() {} 


    PREORDER_REGIONAL_CFG_ITER    &operator ++(void);
    BOOL                          operator!=(INT)     { return _cur != NULL;}
    REGIONAL_CFG_NODE             *operator *(void)   { return _cur; }
 
};

//============================================================================
//
//   Class Name: SEQ_REGIONAL_CFG_ITER
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//    
//     Iterators for regional cfg.It will traverse regional cfg nodes in a 
//     sequential order.
//
//============================================================================   
class  SEQ_REGIONAL_CFG_ITER {
private:
    NODE_VECTOR         *_nodes;
    NODE_VECTOR_ITER     _cur;
     
public:    
    SEQ_REGIONAL_CFG_ITER(REGIONAL_CFG *cfg){
        _nodes = &(cfg->_node_set);
        _cur = _nodes->begin();
    }
   
    ~SEQ_REGIONAL_CFG_ITER() {} 


    SEQ_REGIONAL_CFG_ITER         &operator ++(void)  { _cur++;}
    BOOL                          operator!=(INT)     { return _cur != _nodes->end();}
    REGIONAL_CFG_NODE             *operator *(void)   { return *_cur; }
 
};

//============================================================================
//
//   Class Name: REGION
//
//   Base Class: <none>
//
//   Derived Class: MEME_REGION
//
//   Class Description: 
// 
//     Each REGION object corresponds to a region, which is a node in 
//     the region tree. The REGION class contains summary information
//     of each region. It also contains attributes associated with the
//     region and points to the associated region CFG.
//
//   Note:
//
//============================================================================
class REGION_TREE;
class REGION_LOOP_UPDATE;

class REGION {
    
friend class REGION_TREE;
friend class REGION_LOOP_UPDATE;

private:
    BB                *Edge_Splitting(BB *from_bb, BB *to_bb);

protected:
    REGIONAL_CFG      _cfg;  // Region's local cfg. 
    //-------------------------------------------------------------       
    // Region properties used to connect regions in a region tree.
    //-------------------------------------------------------------
    REGION            *_first_kid;
    REGION            *_parent;
    REGION            *_next_sibling;
    REGION            *_prev_sibling;
    REGION_TREE       *_tree;
    
    INT32             _level; // The level in the region tree. 
    INT32             _n_kids;
    INT32             _id;
    REGION_TYPE       _type;  // Default value of region type is UNKNOWN.
    INT16             _attribute;
    
    REGIONAL_CFG_NODE *_node;
    
    void Attribute(REGION_ATTRIBUTE attr,BOOL set=1) {
        if (set) {
            _attribute |= attr;
        } else {
            _attribute &= ~attr;
        }
    }

public:
    REGION()
    {
      
        _first_kid    =    _parent        = NULL;
        _level        =    _n_kids        = 0;
        _prev_sibling =    _next_sibling  = NULL;
        _node         = NULL;
        _tree         = NULL;
        _id        = 0;
        _type      = UNKNOWN;
        _cfg._r    = this;
    } 
    
    REGION(REGIONAL_CFG *cfg);
    ~REGION() {}

    REGION_TYPE Region_Type()                 { return _type; }  
    void        Region_Type(REGION_TYPE type) { _type = type; }
    //----------------------------------------
    // Functions get REGION properties value.
    //----------------------------------------
    REGIONAL_CFG  *Regional_Cfg(void)     { return &_cfg; }
    
    REGION        *First_Kid(void)        { return _first_kid;    }
    REGION        *Next_Sibling(void)     { return _next_sibling; }
    REGION        *Prev_Sibling(void)     { return _prev_sibling; }
    REGION        *Parent(void)           { return _parent;    }
    REGION_TREE   *Tree(void)             { return _tree;      }
    INT32         N_Kids(void)            { return _n_kids;    }    
    INT32         Level(void)             { return _level;     }  
    INT32         Id(void)                { return _id;        }
    INT16         Attribute(void)         { return _attribute; } 
    void          Id(INT32 id)            { _id = id;          }
    
    NODE_VECTOR   Exits(void)             { return _cfg._exits;     }
    NODE_VECTOR   Entries(void)           { return _cfg._entries;   }  
    
    BOOL          Is_Rigid(void)          { return _attribute&RIGID; }
    BOOL          Is_No_Further_Opt(void) { return _attribute&NO_FURTHER_OPTIMIZATION; }
    BOOL          Is_Persist_Bound(void)  { return _attribute&PERSISTENT_BOUNDARY;     }
    BOOL          Is_No_Opt_Across(void)  { return _attribute&NO_OPTIMIZAION_ACROSS_BOUNDARY; }    
        
    REGIONAL_CFG_NODE *Regional_Cfg_Node(void) { return _node; }
    void               Regional_Cfg_Node(REGIONAL_CFG_NODE *node) {
        _node = node;
    }
        
    //====================================================================
    //
    //  Add_Kid
    //  
    //  Add a region to current region's kids list.And reset the added 
    //  region's parent region.
    //
    //====================================================================
    void Add_Kid(REGION *r) { 
       ++_n_kids;
       r->_next_sibling = _first_kid;
       
       if (_first_kid != NULL) {
           _first_kid->_prev_sibling = r;
           _first_kid = r;
       } else {
           _first_kid = r;
           r->_prev_sibling = NULL;
       }         
       
       r->_parent = this;
    }
    
    //====================================================================
    //
    //  Del_Kid
    //  
    //  Remove a region from current region's kids list.And reset the
    //  removed region's parent region.
    //
    //====================================================================
    void Del_Kid(REGION *r) {
        REGION *t;     
       
        if (_first_kid == r) {
            _first_kid = _first_kid->_next_sibling;
            if (_first_kid != NULL) {
                _first_kid->_prev_sibling = NULL;
            }   
            r->_parent = NULL;
            --_n_kids;
            return;
        }
           
        for (t = _first_kid; ((t->_next_sibling != r)&&(t != NULL)); 
        t = t->_next_sibling);
        
        if (t != NULL) {
            t->_next_sibling = r->_next_sibling;
            r->_prev_sibling = t->_prev_sibling;
            r->_parent = NULL;
            --_n_kids;
        }
    }   
    
    //=====================================================================
    // 
    //  Find_Common_Parent
    //
    //  Find the least common ancestor region of regions r1 and r2's 
    //  in the region tree. 
    //
    //=====================================================================
    REGION      *Find_Common_Parent(REGION *r);
    
    //=====================================================================
    // 
    //  Is_Container_By
    //
    //  Check whether "this" region is contained by region r. 
    //
    //=====================================================================
    BOOL        Is_Contained_By(REGION *r);
    
    //=====================================================================
    //   Is_Kid_Region_Of
    //  
    //   Check whether this region is an immediate kid of region r.
    //
    //=====================================================================
    BOOL        Is_Kid_Region_Of(REGION *r);

    //=====================================================================
    //  Is_Parent_Region_Of
    //
    //  Check whether this region is the immediate parent of region r.   
    //
    //=====================================================================
    BOOL        Is_Parent_Region_Of(REGION *r);

    //=========================================================================
    //  Edge_Splitting
    //
    //  split all of critical edge in this region 
    //  critical edge: the source node has more than one succ, and the target
    //                 node has more than one pred
    //=========================================================================
    void        Edge_Splitting(void);

    void        Print(FILE *f = stderr);
};


//============================================================================
//
//   Class Name: MEME_REGION
//
//   Base Class: REGION
//
//   Derived Class: IMPROPER_REGION
//
//   Class Description: 
//   
//   Note:
//
//============================================================================

class MEME_REGION:public REGION {
friend class REGION_TREE;

public:
    MEME_REGION() { _type = MEME; }
    MEME_REGION(REGIONAL_CFG *cfg);
    ~MEME_REGION() {}
};

    
//============================================================================
//
//   Class Name: SEME_REGION
//
//   Base Class: MEME_REGION
//
//   Derived Class: LOOP_REGION
//
//   Class Description: 
//   
//   Note:
//
//============================================================================
  
class SEME_REGION:public MEME_REGION {
private:
    REGIONAL_CFG_NODE *_main_exit;

public: 
    SEME_REGION() { _type = SEME; };
    SEME_REGION(REGIONAL_CFG *cfg);
    ~SEME_REGION() {}; 
  
};

 
//============================================================================
//
//   Class Name: LOOP_REGION
//
//   Base Class: SEME_REGION
//
//   Derived Class: <none>
//
//   Class Description: 
//
//     LOOP_REGION will contain only a loop with a single entry. A loop 
//     with multiple entries is irreducible and will correspond to 
//     IMPROPER_REGION.
//
//   Note:
//
//============================================================================

class LOOP_REGION:public SEME_REGION {

private:
    INT32              _nest_level; // nest level of nested loop.
    
    REGIONAL_CFG_NODE  *_loop_head;
    REGIONAL_CFG_NODE  *_loop_tail;
public:
    //The nest level not implemented.
    INT32 Nest_Level(void) {
        return _nest_level;
    }
    
    LOOP_REGION(void) {
        _loop_head  = NULL;
        _loop_tail  = NULL;
        _nest_level = 0;
        _type       = LOOP;
        
    }
    
    LOOP_REGION(REGIONAL_CFG *cfg);
    
    ~LOOP_REGION() {}
     
    REGIONAL_CFG_NODE *Loop_Head(void)          { return _loop_head; }
    REGIONAL_CFG_NODE *Loop_Tail(void)          { return _loop_tail; }

    void    Loop_Head(REGIONAL_CFG_NODE *head)  { _loop_head = head; }
    void    Loop_Tail(REGIONAL_CFG_NODE *tail)  { _loop_tail = tail; }
}; 


//============================================================================
//
//   Class Name: IMPROPER_REGION
//
//   Base Class: MEME_REGION
//
//   Derived Class: <none>
//
//   Class Description: 
//
//   Note:  
//
//============================================================================

class IMPROPER_REGION:public MEME_REGION {
public:
    IMPROPER_REGION()  { _type = IMPROPER; }
    ~IMPROPER_REGION() {}
};


//============================================================================
//
//   Class Name: REGION_TREE_MEM
//
//   Base Class: <none>
//
//   Derived Class: REGION_TREE
//
//   Class Description: 
//
//   Note:  
//
//============================================================================

class REGION_TREE_MEM {

protected:
    MEM_POOL  _m;

    REGION_TREE_MEM() {
        MEM_POOL_Initialize(&_m, "REGION_TREE_MEM", true);
        MEM_POOL_Push(&_m);
      }

    ~REGION_TREE_MEM() {
        MEM_POOL_Pop(&_m);
        MEM_POOL_Delete(&_m);
    }
};


//============================================================================
//
//   Class Name: REGION_TREE
//
//   Base Class: REGION_TREE_MEM
//
//   Derived Class: <none>
//
//   Class Description: 
//
//     Region tree: All regions are organized in a region tree 
//     hierarchically. 
//
//   Note:  
//
//============================================================================   

class REGION_TREE :public REGION_TREE_MEM {

friend class INNERMOST_REGION_FIRST_ITER;
friend class INTERVAL_PROCESSOR;
friend class REGION_LOOP_UPDATE;
friend class REGIONAL_CFG;

friend void Verify_Region_Tree(REGION_TREE *tree,BB *first_bb);
friend void RGN_Unlink_BB_Edges(BB *bb, REGIONAL_CFG *regional_cfg);
friend void RGN_Remove_BB_And_Edges(BB *bb, REGIONAL_CFG *regional_cfg);
typedef mempool_allocator<REGION*>       REGION_ALLOC;

private:
    REGION                 *_root;
    REGION_VECTOR          _region_set;
    // _seq_num is the seq_num of REGION contained by this REGION_TREE
    // which starts from 0
    INT32                  _seq_num;  
    
    REGION_VECTOR_ITER Find(REGION *r) {
        REGION_VECTOR_ITER iter; 
        for (iter = _region_set.begin(); iter != _region_set.end(); iter++) {
            if (*iter == r) return iter;
        }
    }       
       
    //=========================================================================
    //  
    //    Insert_Node
    //
    //    Insert a region to the node set which contains all region nodes in
    //    the region set.  
    //
    //=========================================================================
    void Insert(REGION *r) {
        _seq_num++;
        r->Id(_seq_num);
        _region_set.push_back(r);
        r->_tree = this;
    }
    
    //========================================================================
    //  Erase
    //
    //  This function will only erase a region node from the region set. 
    //  
    //=========================================================================
    void Erase(REGION *r)  {
        REGION_VECTOR_ITER iter;
        
        for (iter = _region_set.begin(); iter != _region_set.end(); iter++) {
            if (*iter == r) {
                _region_set.erase(iter);

                break;
            }    
        }
        
        r->_tree = NULL;
    }     
    
    //========================================================================
    //  Add_Region
    //
    //  In this function,region tree will new a region and insert it to the 
    //  region set.The region will be allocated a unique sequential id.And 
    //  the region's type is set to UNKNOWN.
    //  
    //=========================================================================
    REGION *Add_Region(void);
    
        
    //========================================================================
    //  Del_Region
    //
    //  In this function,region will be permannently deleted from mempool,
    //  its pointer will also be removed from region set.
    //  
    //=========================================================================
    void    Del_Region(REGION *r);

    
    //========================================================================
    //  Build_Regional_Cfg
    //
    //  This function is used by REGION_TREE(BB *firstbb)
    //  It map global cfg to regional cfg of root region 
    //  
    //=========================================================================
    INT32    Build_Regional_Cfg(BB *);
    
    //========================================================================
    //  See region.cxx for interface description.
    //=========================================================================
    void    Shrink (REGION *parent,REGION *kid,NODE_VECTOR nodes); 
  
    void    Process_Intervals(REGION *r);  
   
public:
   
    REGION_TREE(BB *firstbb);
    ~REGION_TREE(); 
   
    //========================================================================
    //  Add_Loop_Region
    //
    //  In this function,region tree will new a loop region and insert it to 
    //  the region set.The region will be allocated a unique sequential id.And 
    //  region's type will be set to UNKNOWN.
    //  
    //=========================================================================
    LOOP_REGION     *Add_Loop_Region(void);
    
    LOOP_REGION     *Add_Loop_Region(REGION *parent,REGIONAL_CFG_EDGE *backedge,                     NODE_VECTOR node_set);

    IMPROPER_REGION *Add_Improper_Region(REGION *parent,NODE_VECTOR node_set);
    
    MEME_REGION     *Add_MEME_Region(REGION *parent,NODE_VECTOR node_set);
    
    SEME_REGION     *Add_SEME_Region(REGION *parent,NODE_VECTOR node_set);
    
    //========================================================================
    //  Decompose_Region_To_SEME  
    //
    //  See region.cxx for detail description.
    //  
    //=========================================================================
    void Decompose_Region_To_SEME(REGION* r,RGN_FORM_PAR par);

    //========================================================================
    //
    //  Decomposition
    //
    //=========================================================================
    void Decomposition();
    
    void Decompose_Region_To_MEME(REGION* r,RGN_SIZE size);
	void Set_Loop_Head_Tail(void);
    void Statistic(void);

    REGION      *Root(void) { return _root; }
    REGION_VECTOR Region_Set(void) { return _region_set;}
    INT32        Seq_Num(void) { return _seq_num; } 
    void Print(FILE *f = stderr);
};


//============================================================================
//
//   Class Name: REGION_KID_ITER
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//
//   Note:  
//
//============================================================================   

class REGION_KID_ITER {

private:  
    REGION *_cur;
    
public:
    REGION_KID_ITER(REGION *r)   {
       _cur = r->First_Kid();
    }
    
    ~REGION_KID_ITER() {}

    REGION_KID_ITER &operator ++(void) {
        _cur = _cur->Next_Sibling();
        return *this; 
    }
  
    REGION *operator *(void) {
        return _cur; 
    }
  
    friend BOOL operator ==(const REGION_KID_ITER& x,const REGION_KID_ITER& y) {
        return x._cur == y._cur; 
    }

    BOOL operator !=(INT) {
        return _cur!=0;
    }
};

//============================================================================
//
//   Class Name: INNERMOST_REGION_FIRST_ITER
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//
//   Note:  
//
//============================================================================   

class INNERMOST_REGION_FIRST_ITER {

private:
    REGION          *_root;
    REGION          *_cur;
    
    void Set_Cur(REGION *root);    

public:
    INNERMOST_REGION_FIRST_ITER(REGION_TREE *tree){
        _root = tree->_root;
        Set_Cur(_root);
    }

    INNERMOST_REGION_FIRST_ITER(REGION *root){
        _root = root;
        Set_Cur(_root);
    }
        
    ~INNERMOST_REGION_FIRST_ITER() {}
    

    INNERMOST_REGION_FIRST_ITER  &operator ++(void);    
    REGION                       *operator *(void)    { return _cur;} 
    BOOL                         operator !=(INT)     {return _cur != NULL;}

};

#endif
