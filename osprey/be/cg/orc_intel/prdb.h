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

#ifndef prdb_INCLUDED
#define prdb_INCLUDED

#include <vector>
#include <utility>
#include "bb.h"
#include "defs.h"
#include "tn.h"
#include "mempool.h"
#include "cgtarget.h"
#include "op.h"
#include "bitset.h"
#include "region_map.h"
#include "if_conv.h"
#include "region.h"

#include "bb_map.h"

//***************************************************************************
//  prdb.h
//
//  This file contains the interface for PRDB.
//  It includes several classes, which aims to implement PRDB utilities.
//  PRDB is constructed to be a partition graph, which makes PRDB query and 
//  implementation easier.
//***************************************************************************

extern MEM_POOL* PRDB_pool;
extern COMPARE_TYPE Compare_Type(TOP opcode);


//*****************************************************************************
//  CODE_MOTION_TYPE: 
//  
//  To define the state of an code motion to update PRDB_GEN.
//  - MOVE_TO   : instruction is moved.
//
//  - COPY_TO   : instruction is copied.
//  
//  - DELETE    : instruction is deleted.
//
//*****************************************************************************

typedef enum {
        MOVE_TO,
        COPY_TO,
        DELETE
} CODE_MOTION_TYPE;

//class declaration for subsequent use
class PARTITION;
class PARTITION_GRAPH_NODE;
class PARTITION_GRAPH;
//*****************************************************************************
//   Class Name: PRDB_GEN
//
//   Base Class: <PRDB_MEM>
//
//   Derived Class: <none>
//
//   Class Description: 
//     This is class PRDB_GEN,which implements internal and external interfaces.
//     Firstly, PRDB_GEN constructs partition graph and initializes it
//     Secondly, it implements external utilities to answer PRDB_GEN query
//     Thirdly, it uses two functions to check the partition graph.
//   Note:
//*****************************************************************************
class PRDB_MEM {
protected:
    MEM_POOL _m;

    PRDB_MEM() {
         _m.magic_num = 0;
         MEM_POOL_Initialize( &_m, "PRDB_MEM", TRUE );
         MEM_POOL_Push( &_m );
    }
    
    ~PRDB_MEM() {
        MEM_POOL_Pop( &_m );
        MEM_POOL_Delete(&_m );
    };
};

class PRDB_GEN : public PRDB_MEM
{ 
    friend class PARTITION;
    friend class PARTITION_GRAPH;
    friend class PARTITION_GRAPH_NODE;
private:

    REGION_MAP _region_graph;
public:

    PRDB_GEN() {}
    PRDB_GEN(REGION_TREE*);
    PRDB_GEN(REGION*);
    ~PRDB_GEN(){ REGION_MAP_Delete(_region_graph); }

    PARTITION_GRAPH* Partition_Graph(REGION*);
    //dump the assumed information
    void Print(FILE* file = stderr, REGION_TREE* region_tree = NULL);
};

//*****************************************************************************
//   Class Name: PARTITION_GRAPH_NODE
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//     This class records partition graph node properties, which connect 
//     partition graph nodes to form a partition graph.
//   Note:
//*****************************************************************************

typedef mempool_allocator<PARTITION_GRAPH_NODE *> PG_ALLOC;
typedef std::vector<PARTITION_GRAPH_NODE *, PG_ALLOC>  PG_CONTAINER;

typedef mempool_allocator<PARTITION*> PT_ALLOC;
typedef std::vector<PARTITION*, PT_ALLOC>  PT_CONTAINER;

typedef mempool_allocator<OP*> OP_ALLOC;
typedef std::vector<OP*, OP_ALLOC> OP_CONTAINER;

typedef mempool_allocator<BOOL> BIT_ALLOC;
typedef std::vector<BOOL, BIT_ALLOC> BV_VECTOR;
typedef mempool_allocator<BV_VECTOR> BV_ALLOC;
typedef std::vector<BV_VECTOR, BV_ALLOC>  BV_CONTAINER;

typedef std::pair<TN*, const OP*> TN_OP_PAIR;
typedef mempool_allocator<TN_OP_PAIR*> TP_ALLOCATOR;
typedef std::vector<TN_OP_PAIR*, TP_ALLOCATOR>  TP_CONTAINER;

class PARTITION_GRAPH_NODE
{
    friend class PARTITION_GRAPH;
private:
    //define an index marking the node order to ensure all nodes having 
    //different indexes while initialization and update.
    INT _index;

    //In which level this node lies according to root(level 1)
    INT _level;

    //These partitions help to traverse the whole partition graph.
    //_parent_partitions record partitions whose parent is current node
    PT_CONTAINER _parent_partitions;
    //_child_partitions to record partitions whose child is current node
    PT_CONTAINER _child_partitions;

    //all tns or bbs mapped to this partition graph node
    TP_CONTAINER _related_tns; // record TN_OP_PAIRs
    BB_SET*      _related_bbs;
    TN*           _control_pred; //record bb's control predicate TN;

public:
    PARTITION_GRAPH_NODE();
    ~PARTITION_GRAPH_NODE(){}

    //add bb or tn to _related_bbs or _related_tns of this node
    void Add_Related_BB(BB* bb)
    {
        BB_SET_Union1D(_related_bbs, bb, PRDB_pool);
    }
    BOOL Is_Related_TN(TN_OP_PAIR* tp)
    {
        TP_CONTAINER::iterator iter;
        for (  iter = _related_tns.begin(); 
        iter!= _related_tns.end(); 
        iter++) 
        {
            if ( tp == * iter) return TRUE;
        }
        return FALSE;
    }
    void Add_Related_TN(TN_OP_PAIR* tp)
    {
        if (!Is_Related_TN(tp))
            _related_tns.push_back(tp);
    }

    TN* Control_Pred() { return _control_pred; }
    void Control_Pred(TN* tn) { _control_pred = tn; }

    TP_CONTAINER& Get_TNs() { return _related_tns; }
    BB_SET* Get_BBs() { return _related_bbs;       }

    void  Index(INT idx)  { _index = idx;        }
    INT Index()           { return _index;       }

    void  Level(INT level) { _level = level;     }
    INT Level()            { return _level;      }
    BOOL  Is_Reachable()     { return (_level != -1); }

    void Add_Child_Partition(PARTITION* child)
    {
        _child_partitions.push_back (child);
    }
    PT_CONTAINER& Child_Partitions() 
    { return _child_partitions;}
    
    void Add_Parent_Partition(PARTITION* parent)
    {
        _parent_partitions.push_back (parent);
    }
    PT_CONTAINER& Parent_Partitions()     
    { return _parent_partitions;}

    void Del_Related_TN(TN* tn, OP* op);

    void Clear_Child_Partitions() { _child_partitions.clear(); }
    void Clear_Parent_Partitions() { _parent_partitions.clear(); }
    void Print(FILE* file = stderr);
};

//*****************************************************************************
//   Class Name: PARTITION
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//     This class describes partition attributes
//     There is also a member function that stands for an operator "=" to judge
//     whether one partition table item is equal to the other.
//   Note:
//*****************************************************************************
class PARTITION
{
private:

    PARTITION_GRAPH_NODE* _parent;
    PG_CONTAINER          _child;

public:

    PARTITION() : _child(PG_ALLOC(PRDB_pool))
    {
    };

    ~PARTITION() {}

    PARTITION_GRAPH_NODE* Parent() { return _parent; }
    PG_CONTAINER& Child() { return _child; }

    void Parent(PARTITION_GRAPH_NODE* node) { _parent = node; }

    //add a child to current partition, children's indices are incremental.
    void Add_Child(PARTITION_GRAPH_NODE* node) {
        PG_CONTAINER::iterator iter;
        for(iter=_child.begin();iter!=_child.end();iter++)
            if((*iter)->Index()>node->Index()) break;
        iter = _child.insert(iter, node); 
    }

    //delete a child of partition and delete all if it has only one child.
    void Del_Child(PARTITION_GRAPH_NODE* node) {
        PG_CONTAINER::iterator iter;
        for ( iter = _child.begin(); iter != _child.end(); iter++)
        {
            if (*iter == node )
            {
                iter = _child.erase(iter);
                break;
            }
        }
        if (_child.size() == 1 && (*(_child.begin()))->Index() == 0)
            _child.clear();
    }

    BOOL operator==(PARTITION* );

    void Print(FILE* file = stderr);
};


//*****************************************************************************
//   Class Name: PARTITION_GRAPH
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//     This class constructs a partition graph and implements its utilities to 
//     access the partition graph
//   Note:
//*****************************************************************************
class PARTITION_GRAPH
{
    friend class PRDB_GEN;

protected:
    //The INT global_nd_index is a global member to record the currently biggest node
    //index
    INT partition_graph_node_number;

private:
    //define some special partition graph nodes to record different node type.
    PARTITION_GRAPH_NODE* _root;
    PARTITION_GRAPH_NODE* _dummy_node;

    OP_MAP _tn_node_map;
    BB_MAP _bb_node_map;

    BV_CONTAINER _disjoint_relation;
    BV_CONTAINER _subset_relation; //the former is subset of the latter
    std::vector<bool, std::allocator<bool> >  _visited; //indicate whether the node is visited
    PG_CONTAINER _nodes;
    
    //mark node level(the level of root is 1)
    void Mark_Level(PARTITION_GRAPH_NODE* node);

    // result will record des's sibling with respect to anc.
    void Find_Sibling(PG_CONTAINER* result, PARTITION_GRAPH_NODE* anc, 
        PARTITION_GRAPH_NODE* des);

    void Collect_Info(REGION* region);
    void Look_For_Partition(REGION* region);
    void Look_Partition_For_Or_Type(TN* tn, PARTITION_GRAPH_NODE* parent, 
            PARTITION_GRAPH_NODE* child1, PARTITION_GRAPH_NODE* child2, 
            OP* op);
    void Look_Partition_For_And_Type(TN* tn, PARTITION_GRAPH_NODE* parent, 
            PARTITION_GRAPH_NODE* child1, PARTITION_GRAPH_NODE* child2, 
            OP* op);

    //add edges to connect urreachable nodes from root to form a complete 
    //partition graph
    void Complete_Partition_Graph();
    
    BOOL Find_Reachable_Descendant(PG_CONTAINER*,PARTITION_GRAPH_NODE*);

    //least common ancestor
    PARTITION_GRAPH_NODE* Get_Lca(PARTITION_GRAPH_NODE*,PARTITION_GRAPH_NODE*);
    PARTITION_GRAPH_NODE* Get_Gcd(PARTITION_GRAPH_NODE*,PARTITION_GRAPH_NODE*);

    //Get all reachable nodes for NODE
    PG_CONTAINER* Get_Subset_Nodes(PARTITION_GRAPH_NODE* node);
    
    //the result will record the subset after if subtracts des. And if it
    //executes successfully, a true value returns; otherwise a FALSE
    //value will return.
    BOOL Subtract(PG_CONTAINER* result, PARTITION_GRAPH_NODE* des);

    BOOL Subtract(PG_CONTAINER* result, PG_CONTAINER* set);

    void Add_Partition(PARTITION_GRAPH_NODE*, PG_CONTAINER*);
    
    //compute nodes relations such as disjoint, subset, superset
    void Pre_Computing();

    PARTITION_GRAPH_NODE* Find_Node_In_OP(TN_OP_PAIR* tn_op);
    void Set_Disjoint(PARTITION_GRAPH_NODE*,PARTITION_GRAPH_NODE*);
    void Set_Subset(PARTITION_GRAPH_NODE*,PARTITION_GRAPH_NODE*);
    void Rec_Set_Disjoint(PARTITION_GRAPH_NODE*,PARTITION_GRAPH_NODE*);

    void Copy_To(OP* op, BB* tgt_BB);
    void Move_To(OP* op, BB* tgt_BB);
    void Delete(OP* op);

    BOOL Is_Complementary(PARTITION_GRAPH_NODE*,PARTITION_GRAPH_NODE*,
        PARTITION_GRAPH_NODE*);

    void Sum(TN_OP_PAIR* tp , TP_CONTAINER* );
    void Diff(TN_OP_PAIR tp, TP_CONTAINER*);
    //simplify a predicate set.
    void Reduce(TP_CONTAINER* tp_set, BOOL is_dum);
    //simplify a partition_graph_node set.
    void Reduce(PG_CONTAINER* pg_set, BOOL is_dum);

public:
    //return true if cycle is detected;
    BOOL Cycle_Detector();

    //return TRUE with illegal partition
    BOOL Illegal_Partition();

    //Constructor and destructor
    PARTITION_GRAPH(){}
    PARTITION_GRAPH(REGION*);
    ~PARTITION_GRAPH() {
        BB_MAP_Delete(_bb_node_map);
        OP_MAP_Delete(_tn_node_map);
    }

    //return the root node and dummy node of graph
    PARTITION_GRAPH_NODE* Root() { return _root; }
    PARTITION_GRAPH_NODE* Dummy() { return _dummy_node; }

    //get the partition graph node set
    PG_CONTAINER& Nodes() { return _nodes;}

    OP_MAP Tn_Node_Map() { return _tn_node_map; }
    
    inline void Add_Node(PARTITION_GRAPH_NODE* pgn) 
    {
        _nodes.push_back (pgn);
    }

    //reset all relations to FALSE if the corresponding node is deleted.
    void Del_Relation(INT index)
    {
        if(!index) return;
        PG_CONTAINER::iterator iter;
        for(iter = _nodes.begin(); iter!=_nodes.end(); iter++)
        {
            _disjoint_relation[index][(*iter)->Index()] = FALSE;
            _disjoint_relation[(*iter)->Index()][index] = FALSE;
            _subset_relation[index][(*iter)->Index()] = FALSE;
            _subset_relation[(*iter)->Index()][index] = FALSE;
        }
    }

    //recompute disjoint and subset relation when new nodes are added.
    void Add_Relation(PARTITION_GRAPH_NODE* child,
                      PARTITION_GRAPH_NODE* parent);

    BOOL Is_Disjoint(TN_OP_PAIR, TN_OP_PAIR);
    BOOL Is_Disjoint(BB* , BB*);
    BOOL Is_Subset(TN_OP_PAIR, TN_OP_PAIR);
    BOOL Is_Subset(BB* , BB*);
    BOOL Is_Superset(TN_OP_PAIR, TN_OP_PAIR);
    BOOL Is_Superset(BB* , BB*);
    BOOL Get_Complementary(TP_CONTAINER* result,TN_OP_PAIR,TN_OP_PAIR);
    BOOL Get_Complementary(TP_CONTAINER* result,TN_OP_PAIR , BB* base_bb);
    TN* Get_Complementary(TN_OP_PAIR , BB* base_bb);
    BOOL Is_Complementary(TN_OP_PAIR , TN_OP_PAIR , TN_OP_PAIR);
    BOOL Is_Complementary(TN_OP_PAIR , TN_OP_PAIR, BB*);
    float Get_Probability(TN* tn);
    float Get_Probability(TN* tn1, TN* tn2);

    void Glb_Sum(TN_OP_PAIR* tp, TP_CONTAINER* tp_set);
    void Lub_Sum(TN_OP_PAIR* tp, TP_CONTAINER* tp_set);
    void Lub_Diff(TN_OP_PAIR tp, TP_CONTAINER*);
    void Glb_Diff(TN_OP_PAIR tp, TP_CONTAINER*);

    //update PARTITION GRAPH after instructions are moved, copied or deleted.
    void Update(OP* op, BB* tgt_BB, CODE_MOTION_TYPE);
    void Print(FILE* file = stderr);
};

inline PRDB_GEN* Generate_PRDB(REGION_TREE*);
inline PRDB_GEN* Generate_PRDB(REGION*);
void Delete_PRDB(void);
PRDB_GEN* PRDB_Init(REGION_TREE* rgn_tree);
PRDB_GEN* PRDB_Init(REGION* region);
PRDB_GEN* Get_PRDB(void);
BOOL PRDB_Valid(void);

BOOL  Is_In_Abnormal_Loop(REGION* r);
inline BOOL Is_Critical_Edge(REGIONAL_CFG_EDGE*);
//return TRUE if region doesn't have a bb node
BOOL Is_No_BB_Region(REGION* region);

//return region's entry bb node even though its entry is a nested region.
inline BB* Find_Region_Entry_BB(REGION*);
    

#endif

// Local Variables:
// mode:C++
// End:
