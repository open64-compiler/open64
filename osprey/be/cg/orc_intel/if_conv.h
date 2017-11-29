/*
  Copyright (C) 2000-2003, Intel Corporation
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

#ifndef if_conv_INCLUDED
#define if_conv_INCLUDED

#include <vector>
#include <set>
#include "bb.h"
#include "defs.h"
#include "mempool.h"
#include "error.h"
#include "bb_set.h"
#include "region.h"
#include "cgtarget.h"

// During Forceed If Conversion biased areas are still converted if they are small
// compared to the whole loop.
#define IF_CONV_BIASE_THRESHOLD 20 //in percent

//*****************************************************************************
//  if_conv.h
//
//  This file contains the data structure for if-conversion. It will be used 
//  only in the if-conversion phase. 
//  
//  The most important class in the file is IF_CONV_AREA, which is used to 
//  record information for if-conversion candidates. Each IF_CONV_AREA consists 
//  of several basic blocks, which are to be merged into one basic block by 
//  if-conversion. 
//*****************************************************************************

class IF_CONV_AREA;
class BB_PREDICATE_INFO;

typedef mempool_allocator<IF_CONV_AREA *>      IF_CONV_ALLOC;
typedef std::vector<IF_CONV_AREA *, IF_CONV_ALLOC>  AREA_CONTAINER;

typedef mempool_allocator<TN*>    TN_CONTAINER_ALLOC;
typedef std::vector<TN*, TN_CONTAINER_ALLOC>  TN_CONTAINER;

typedef mempool_allocator<BB *>   BB_CONTAINER_ALLOC;
typedef std::vector<BB *, BB_CONTAINER_ALLOC>  BB_CONTAINER;

class TN_INFO_MEM 
{
public:
    MEM_POOL _m;

    TN_INFO_MEM(void) {
         MEM_POOL_Initialize(&_m, "TN_INFO_MEM", true);
         MEM_POOL_Push(&_m);
    }
    
    ~TN_INFO_MEM(void) {
        MEM_POOL_Pop( &_m );
        MEM_POOL_Delete(&_m );
    };
};

//*****************************************************************************
//  AREA_TYPE: 
//  
//  To define the state of an IF_CONV_AREA.
//
//  - UPWARD_UNSUITABLE : indicate that it is not suitable for being merged 
//            into its predecessor by if-conversion.
//  
//  - DOWNWARD_UNSUITABLE : indicate that its successors are not suitable for 
//            being merge into it by if-conversion.
//
//  - UNSUITABLE : indicate it is not suitable for being merged with others.
//            
//  - SUITABLE_FOR_IF_CONV : indicate that it is suitable for if-conversion
//
//*****************************************************************************

enum  AREA_TYPE 
{
    SUITABLE_FOR_IF_CONV = 0x00,
    UPWARD_UNSUITABLE = 0x01,
    DOWNWARD_UNSUITABLE = 0x02,
    UNSUITABLE = 0x03
};

//*****************************************************************************
//  IF_CONV_TYPE: 
//  
//  To define the state of an IF_CONV_AREA.
//
//  - NO_IF_CONV : indicate the area needn't if-conversion
//  
//  - PARTIAL_IF_CONV: indicate the area should be partially if-converted.
//
//  - FULLY_IF_CONV  : indicate the area should be fully if-converted.
//
//*****************************************************************************
enum  IF_CONV_TYPE 
{
    NO_IF_CONV = 0x1,
    PARTIAL_IF_CONV = 0x2,
    FULLY_IF_CONV = 0x4
};
//******************************************************************************
//   CFLOW_TYPE:
//   
//     To define the control-flow-pattern of an IF_CONV_AREA and its successors. 
//   - NO_TYPE:  it means an IF_CONV_AREA and its successors are not a pattern 
//               suitable for if-conversion. 
//   
//   - SERIAL_TYPE: it means an IF_CONV_AREA has only one successor and its 
//                  successor has only one predecessor. 
// 
//   - IF_THEN_TYPE: it means the following pattern:
//                    
//                    node  
//                   /   |
//                  /    |
//           successor1  |
//                  \    |
//                   \   |
//                   node2
// 
//   - IF_THEN_ELSE_TYPE: it means the following pattern: 
//             
//                    node  
//                   /   \
//                  /     \
//           successor1   successor2
//                  \      /
//                   \    /
//                    node2
// 
//*****************************************************************************
enum  CFLOW_TYPE 
{
    NO_TYPE = 0x1,
    SERIAL_TYPE = 0x2,
    IF_THEN_TYPE = 0x4,
    IF_THEN_ELSE_TYPE = 0x8
};

//*****************************************************************************
//   BB_MERGE_TYPE :
//   To define the type of basic blocks used in the course of merging 
//   basic blocks.
//   - CASE_ALL_IN_AREA : all sucessors are in the IF_CONV_AREA
//
//   - CASE_CALL_OUT : a call block, and it has at least one outside successor
//
//   - CASE_CALL_IN : call with all successors in the area 
//   
//   - CASE_UNCOND_BR : unconditional branch whose target is out of the area
//  
//   - CASE_FALL_OUT : not a branch, fall through outside
//  
//   - CASE_IF_FALL_IN : conditional branch, whose target is out of the area 
//                       and whose fallthrough successor is in the area
// 
//   - CASE_IF_FALL_OUT : conditional branch, whose target is in the area 
//                        and whose fallthrough successor is out of  the area
// 
//   - CASE_IF_OUT: its both successors are out of the area
// 
//   - CASE_CHECK_IN: a bb including a check and whose fall-through successor
//                    (non recovery block) is in its home region
//
//   - CASE_CHECK_OUT: a bb including a check and whose fall-through successor
//                    (non recovery block) is not in its home region.
//*****************************************************************************


enum BB_MERGE_TYPE 
{ 
    CASE_ALL_IN_AREA = 0x1,
    CASE_CALL_OUT = 0x2,
    CASE_CALL_IN = 0x3,
    CASE_UNCOND_BR  = 0x4,
    CASE_FALL_OUT = 0x5, 
    CASE_IF_FALL_IN = 0x6,
    CASE_IF_FALL_OUT = 0x7,
    CASE_IF_OUT = 0x8,
    CASE_CHECK_IN = 0x9,
    CASE_CHECK_OUT = 0x10
};

//*****************************************************************************
//   Class Name: CNTL_DEP
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//        it is used to record the control dependence information for basic 
//        blocks in a bb set. The class can be used out of if-conversion  phase.
//
//   Note:
//*****************************************************************************
class IF_CONVERTOR;

class CNTL_DEP 
{
private:
    BB     *_entry;

    // the class will compute and record the control dependence information 
    // for a bb set
    BB_SET *_bb_set;

    //record control dependences for all basic blocks in the set
    BB_MAP _control_dependences; 

    // true_edges are those edges which are traversed when the branch 
    // instruction in the BB is executed.    
    BB_MAP _true_edges;

    // the function is used to help to get a bottom-up order 
    // of the control-dependent tree
    void   _Post_Order_Helper(BB_CONTAINER& result, BB *bb, IF_CONVERTOR*);
public:
    // constructor and destructor
    // now we only compute the control-dependence-information for a BB_SET
    // other types of constructors can be added later if they are required
    CNTL_DEP(BB *, BB_CONTAINER&, MEM_POOL *);
    ~CNTL_DEP()
    {
        BB_MAP_Delete(_control_dependences);
        BB_MAP_Delete(_true_edges);
    }

    // Cntl_Dep_Parent returns the parent in the control dependence tree
    BB_SET *Cntl_Dep_Parents(BB *bb)
    {
        BB_SET* set = (BB_SET*)BB_MAP_Get(_control_dependences, bb);
        Is_True( set != NULL, ("NULL cntl-dep-parents set!\n"));
        return set;
    }

    BB_SET *True_Cntl_Dep_Parents(BB *bb){
        BB_SET* set = (BB_SET*)BB_MAP_Get(_true_edges, bb);
        Is_True( set != NULL, ("NULL true-edges-set set!\n"));
        return set;
    }

    // judge if bb2 control-dependent on bb1
    BOOL   Is_Cntl_Dep(BB *bb1, BB *bb2); 

    // find out the child of bb in control dependent tree and put them in 
	// bb_set  
    void   Cntl_Dep_Children(BB_SET* &result, BB *, MEM_POOL *); 

    // get a bottom-up order of control dependent tree
    void   Get_Post_Order(BB_CONTAINER& result, IF_CONVERTOR *);

    // dump the control dependenct information
    void   Print(FILE* file = stderr);
};

//*****************************************************************************
//   Class Name: IF_CONVERTOR
//
//   Base Class: <IF_CONV_MEM>
//
//   Derived Class: <none>
//
//   Class Description: 
//        it is a class used to perform if-conversion.
//   Note:
//*****************************************************************************

class IF_CONV_MEM 
{
protected:
    MEM_POOL _m;

    IF_CONV_MEM(void) {
         MEM_POOL_Initialize(&_m, "IF_CONV_MEM", true);
         MEM_POOL_Push(&_m);
    }
    
    ~IF_CONV_MEM(void) {
        MEM_POOL_Pop( &_m );
        MEM_POOL_Delete(&_m );
    };
};

class IF_CONVERTOR : public IF_CONV_MEM 
{
    friend class IF_CONV_AREA;
    friend class CNTL_DEP;
protected:
    // some tool functions
    AREA_TYPE Suitable_For_If_Conv(BB *);
    BOOL     Is_Partial_Redundant_Def(BB* bb, OP* op,TN* tn);
    void     Compute_Min_Cycle(INT32&,BB_CONTAINER&);
    float    Prob_Of_Area(IF_CONV_AREA *, IF_CONV_AREA *);
    INT32    Compute_Critical_Path_Len(BB *, BOOL);
    AREA_CONTAINER::iterator 
             Find_Area(AREA_CONTAINER&, IF_CONV_AREA *);
    void     Add_Edge(IF_CONV_AREA *, IF_CONV_AREA *);
    BOOL     Is_BB_Container_Member(BB_CONTAINER&, BB*);

private:
    // variables
    INT32 _loop_length;
    
    // do initialization
    void     If_Conversion_Init(REGION *region, 
                               AREA_CONTAINER& area_list);

    // select proper if-conversion-candicates
    void     Select_Candidates (AREA_CONTAINER&, BOOL forced=FALSE);
    CFLOW_TYPE 
             Detect_Type (IF_CONV_AREA *, 
                          AREA_CONTAINER&,
                          BOOL forced = FALSE);
    BOOL     Worth_If_Convert (AREA_CONTAINER&, 
                               CFLOW_TYPE,
                               BOOL forced=FALSE);
    void     Reduce_By_Type(AREA_CONTAINER&, 
                            CFLOW_TYPE, 
                            AREA_CONTAINER&);
    
    // generate parallel compare instructions
    BOOL     Check_If_Gen_Useless_Predicate(BB_PREDICATE_INFO *);
    void     Find_Start_Node(IF_CONV_AREA *, BB *);
    void     Record_Para_Comp_Info(IF_CONV_AREA *, BB *);
    void     Detect_Para_Comp(IF_CONV_AREA *);
    void     Gen_Para_Comp(IF_CONV_AREA *area);
    void     Gen_Predicate_Assign(TN *, TN *, OP *, COMPARE_TYPE, TOP, TN *, 
                 OPS *);
    TN       *Get_1_Pred_And_Erase(TN_CONTAINER&);
    BOOL     Get_2_Pred_And_Erase(TN*&, TN*&, COMPARE_TYPE&, BB*, 
                 BB_PREDICATE_INFO *); 
    BOOL     Has_Para_Comp_Top(OP*); 
    TOP      Get_Para_Comp_Top(OP*, COMPARE_TYPE);
    TN*      Get_Start_Node_Pred(TN *, BB *, IF_CONV_AREA *);

    // insert assignment for predicates
    BOOL     Insert_Predicate(IF_CONV_AREA *area);

    // merge basic blocks
    BOOL     Convert_Candidates(AREA_CONTAINER& areas);
    BB_MERGE_TYPE 
             Classify_BB(BB *,IF_CONV_AREA *);
    void     Merge_Blocks(BB *, BB *);
    void     Merge_Area(IF_CONV_AREA *area);
    void     Set_Fall_Thru(BB* , BB* );
    BOOL     Can_Merge_Into_One_BB(IF_CONV_AREA*);
    BOOL     Can_Merge_Into_One_Area(AREA_CONTAINER&);

    void     Print_All_Areas(AREA_CONTAINER&, FILE* file = stderr);
    void     Print_BB_Merge_Type(BB_MERGE_TYPE, FILE* file = stderr); 
    INT32    Calculate_Loop_Critical_Length (IF_CONV_AREA* area);
    INT32    Calculate_Loop_Cycled_Critical_Length (IF_CONV_AREA* area);

public:
    IF_CONVERTOR(void);
    IF_CONVERTOR(REGION_TREE *);
    ~IF_CONVERTOR(void) {}

    BB       *Force_If_Convert(LOOP_DESCR *, BOOL);
};

//*****************************************************************************
//   Class Name: EXIT_TARGET_TYPE
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//        it is used to record the information about exits of an IF_CONV_AREA
//        in an IF_CONV_AREA. There may be several exits. But, the targets of
//        those exits may be an identical one. such as:
//                      1
//                     /  \
//                    /    |
//                   2     |
//                  / \    |
//                 /   \   |
//                3       4
//        among them, BB1 and BB2 are in the IF_CONV_AREA. There may be two
//        predicates related to the exit. One guards the edge 1--> 4
//        and the other guards the edge 2->4. Between them, we call the lexically
//        last one as main_predicate, and the other as auxiliary predicates.
//   Note:
//*****************************************************************************
class EXIT_TARGET_INFO {
private:
    BB                 *_target;  
    TN_CONTAINER       _main_predicates; 
    TN_CONTAINER       _aux_predicates;

public: 
    EXIT_TARGET_INFO(BB* tgt, MEM_POOL* mem_pool):
    _aux_predicates(TN_CONTAINER_ALLOC(mem_pool))
    {
        _target = tgt;
    }
    ~EXIT_TARGET_INFO(void);

    BB      *Target(void)           { return _target;           }
    TN_CONTAINER&  Main_Predicate(void)   { return _main_predicates;   }
    TN_CONTAINER&  Aux_Predicates(void)   { return _aux_predicates; }
    void    Add_Main_Predicate(TN* tn) { _main_predicates.push_back(tn); }
    void    Del_Main_Predicate(TN* tn);
    void    Add_Aux_Predicate(TN* tn) { _aux_predicates.push_back(tn); }

    BOOL    Is_Main_Predicate(TN*);
    void    Assign_Aux_Predicates(BB* bb, OP *br); 
    void    Update_Predicate(TN *old_tn, TN* new_tn);

};

typedef mempool_allocator<EXIT_TARGET_INFO*>    EXIT_CONTAINER_ALLOC;
typedef std::vector<EXIT_TARGET_INFO*, EXIT_CONTAINER_ALLOC>  EXIT_CONTAINER;

//*****************************************************************************
//   Class Name: IF_CONV_AREA
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//        it is used to record information for if-conversion candidates. The 
//        detailed description about each field of the class is in the 
//        declaration of it.
//
//   Note:
//*****************************************************************************

class IF_CONV_AREA
{
    friend class IF_CONVERTOR;
private:
    
    // the entry point of the IF_CONV_AREA
    BB                  *_head;
    // all basic block in the IF_CONV_AREA
    BB_CONTAINER        _included_blocks;    

    // indicate how many cycles the critical path of the area will take
    INT32               _cycled_critical_length;
    // indicate how long the critical path of the area is
    INT32               _critical_length;

    AREA_CONTAINER      _predecessors;
    AREA_CONTAINER      _successors;

    // indicate if the area is suitable for if-conversion and for which kind 
    // of if-conversion
    AREA_TYPE           _if_suitable;
    IF_CONV_TYPE        _need_if_convert;

    // information about control dependences
    CNTL_DEP            *_control_deps; 

    // information about predicate assignment
    // it is a map from BB to BB_PREDICATE_INFO
    BB_MAP              _pred_assign_info;

    EXIT_CONTAINER      _exit_targets;
    
    // to find out a given IF_CONV_AREA in an AREA_CONTAINER
    BOOL                _Find_Area(AREA_CONTAINER&, IF_CONV_AREA*);
public:

    IF_CONV_AREA(BB *, IF_CONVERTOR *);
    ~IF_CONV_AREA(void)
    {
        if ( _pred_assign_info )
            BB_MAP_Delete( _pred_assign_info);
    }

protected:
    // the member functions to access the private members
    BB                *Entry_BB(void) { return _head;                        }
    mBB_NUM           Area_Label(void){ return BB_id(_head);                 }
    BB_CONTAINER&     Blocks(void)    { return _included_blocks;             }
    void              Remove_BB(BB*);

    EXIT_CONTAINER&   Exit_Targets(void) { return _exit_targets;             }

    AREA_TYPE         Area_Type(void)    { return _if_suitable;              }
    void              Area_Type(AREA_TYPE t) 
    {    _if_suitable = (AREA_TYPE)(((INT)_if_suitable)|t);            
    }

    IF_CONV_TYPE      Conv_Type(void) { return _need_if_convert;             }
    void              Conv_Type (IF_CONV_TYPE t)    { _need_if_convert = t;  }

    INT32    Cycled_Critical_Len(void)       { return _cycled_critical_length;}
    void     Cycled_Critical_Len(INT32 len)  { _cycled_critical_length = len; }

    INT32    Critical_Len(void)              { return _critical_length;       }
    void     Critical_Len(INT32 len)         { _critical_length = len;        }

    AREA_CONTAINER&          Pred_Set(void){ return  _predecessors;           }
    AREA_CONTAINER&          Succ_Set(void){ return  _successors;             }
    INT                      Pred_Num(void){ return  _predecessors.size();    }
    INT                      Succ_Num(void){ return  _successors.size();      } 

    CNTL_DEP           *Cntl_Dep_Info(void){ return _control_deps;            }
    BB_PREDICATE_INFO  *Pred_Assign_Info(BB *bb)
    {
        return (BB_PREDICATE_INFO*)BB_MAP_Get(_pred_assign_info, bb);
    }
    
    BOOL Is_Succ(IF_CONV_AREA *area,IF_CONVERTOR *convertor)
    {
        if (convertor -> Find_Area(_successors, area) != _successors.end()) 
            return true;
        return false;
    }
    BOOL Is_Pred(IF_CONV_AREA *area, IF_CONVERTOR *convertor)
    {
        if (convertor -> Find_Area(_predecessors, area) != _predecessors.end()) 
            return true;
        return false;
    }
    void Add_Succ(IF_CONV_AREA *area, IF_CONVERTOR *convertor)
    {    
        if ( !Is_Succ(area,convertor))
            _successors.push_back(area);
    }
    void Add_Pred(IF_CONV_AREA *area, IF_CONVERTOR *convertor)
    {
        if ( !Is_Pred(area,convertor))
            _predecessors.push_back(area);
    }
    void Del_Succ(IF_CONV_AREA *area, IF_CONVERTOR *convertor)
    {
        if ( Is_Succ(area,convertor))
            _successors.erase( convertor ->Find_Area(_successors, area));
    }
    void Del_Pred(IF_CONV_AREA *area, IF_CONVERTOR *convertor)
    {
        if ( area -> Is_Succ(this,convertor))
            _predecessors.erase(convertor -> Find_Area(_predecessors, area));
    }

    // if an IF_CONV_AREA is to be if-converted, we must initialize 
    // some BB_MAPs to record some infomation
    void Init_Conversion_Info(MEM_POOL *);

    EXIT_TARGET_INFO* Exit_Target(BB*);
    void              Add_Exit_Target(BB* target, TN* predicate, MEM_POOL*);

    // combine included blocks of two areas
    void Combine_Blocks_With(IF_CONV_AREA *, MEM_POOL *);

    void Print(FILE *file = stderr);
    void Print_IR(FILE *file = stderr);
};
    
//*****************************************************************************
//   Class Name: BB_PREDICATE_INFO
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//    The class is used to record the information about predicate assignment for 
//    each bb. In the class , we record the following information for each bb:
//            1) the predicate assigned to the bb
//            2) the predicates, for which we should insert predicate assignment 
//                in the bb 
//            3) the starting-nodes of each bb
//            4) if the bb is transitional. Here, transitional node is the basic
//             block in which there is no obstacle to prevent moving the related 
//             predicate assignments up to the control dependent parent of it.
//   Note:
//*****************************************************************************

class BB_PREDICATE_INFO 
{
private:
    // the starting-node information of the bb
    BB_MAP       _start_nodes;
    // the predicate assigned to the bb
    TN           *_predicate;

    // because of parallel compare generation, the predicate assigned to 
    // one BB will changed. 
    TN           *_orig_pred;

    // indicate if the bb is a transitional node
    BOOL         _is_transitional;
    BOOL         _eqv_class_head;
    BB*          _eqv_class_head_bb;

    // the following fields are used to record the predicates, 
    // for which we should generate predicate-assignment for them in the bb
    TN           *_true_tn;
    TN           *_false_tn;

    TN_CONTAINER _or_tns;
    TN_CONTAINER _orcm_tns;
    TN_CONTAINER _and_tns;
    TN_CONTAINER _andcm_tns;

public:
    // constructor and destructor
    BB_PREDICATE_INFO(MEM_POOL *mem_pool):
    _or_tns(TN_CONTAINER_ALLOC(mem_pool)),
    _orcm_tns(TN_CONTAINER_ALLOC(mem_pool)),
    _and_tns(TN_CONTAINER_ALLOC(mem_pool)),
    _andcm_tns(TN_CONTAINER_ALLOC(mem_pool))
    {
          _start_nodes = 0;
          _predicate = 0;
          _orig_pred = 0;
          _is_transitional = false;
          _eqv_class_head = false;
          _true_tn = 0;
          _false_tn = 0;
    }
    ~BB_PREDICATE_INFO(void)
    {
        if (_start_nodes)
            BB_MAP_Delete(_start_nodes);
    }

    // some functions to access private members
    BOOL Transitional(void)     { return _is_transitional;   }
    void Set_Transitional(void) { _is_transitional = true;   }

    BOOL Eqv_Class_Head(void)   { return _eqv_class_head;    }
    void Set_Eqv_Class_Head(void){ _eqv_class_head = true;    }
    
    BB*  Eqv_Class_Head_BB(void)  { return _eqv_class_head_bb; }
    void Eqv_Class_Head_BB(BB* bb){ _eqv_class_head_bb = bb; }


    BOOL Has_Start_Node(void)   { return _start_nodes != 0;  }
    BB   *Start_Node(BB *pred)  
    {    
        return (BB*)BB_MAP_Get(_start_nodes, pred);
    }
    void Start_Node(BB *start_node, BB *pred)
    {
         if (! _start_nodes) 
             _start_nodes = BB_MAP_Create();
         BB_MAP_Set( _start_nodes, pred, start_node);
    }

    TN   *Predicate(void)       { return _predicate;  }
    void Predicate(TN *p)       { _predicate = p;     }

    TN   *Orig_Pred(void)       { return _orig_pred;  }
    void Orig_Pred(TN *p)       { _orig_pred = p;     }

    TN   *True_TN(void)         { return _true_tn;    }
    void True_TN(TN *tn)        { _true_tn = tn;      }
    TN   *False_TN(void)        { return _false_tn;   }
    void False_TN(TN *tn)       { _false_tn = tn;     }

    TN_CONTAINER& Or_TNs(void)  { return _or_tns;     }
    TN_CONTAINER& Orcm_TNs(void){ return _orcm_tns;   }
    TN_CONTAINER& And_TNs(void) { return _and_tns;    }
    TN_CONTAINER& Andcm_TNs(void){ return _andcm_tns;  }
    
    void Add_Or_TNs(TN *tn)           
    {    
        _or_tns.push_back(tn);    
    }
    void Add_Orcm_TNs(TN *tn)         
    {    
        _orcm_tns.push_back(tn);  
    }
    void Add_And_TNs(TN *tn)          
    {    
        _and_tns.push_back(tn);   
    }
    void Add_Andcm_TNs(TN *tn)        
    {    
        _andcm_tns.push_back(tn); 
    }
};

extern hTN_MAPf    frequency_of_predicates;
extern hTN_MAP     init_op_info;
extern TN_INFO_MEM info_mem;

BOOL Is_Para_Comp_May_Def(OP* op);
BOOL Is_In_Infinite_Loop(REGION*);
BOOL Is_Abnormal_Loop(REGION*);
static OP* TN_Defined_At_Op (TN *tn, OP *op, std::vector<OP *> *ops);

#endif
