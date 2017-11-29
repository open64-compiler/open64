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

//*********************************************************************
//
// Module: vt_dag.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_ict/vt_dag.cxx,v $
//
// Description:
//
// Functions for visualizing dependence graph (DAG) using daVinci.
//
//*********************************************************************

#include "bb.h"
#include "defs.h"
#include "cg_flags.h"
#include "cg.h"
#include "cg_dep_graph.h"
#include "cg_loop.h"
#include "DaVinci.h"
#include "glob.h"
#include "mempool.h"
#include "op.h"
#include "region.h"
#include "scheduler.h"
#include "symtab.h"
#include "symtab_idx.h"
#include "tag.h"
#include "tn.h"
#include "topcode.h"
#include "vt_dag.h"
#include "vt_region.h"
#include "whirl2ops.h"
#include "wn_core.h"
#include <ipfec_options.h>

/* ====================================================================
 *
 *  Dependence_Color (CG_DEP_KIND kind)
 *
 *  A fucntion to enquire the color of diverse dependencies in a
 *  denpendence graph.
 *
 * ====================================================================
 */
static char * Dependence_Color (CG_DEP_KIND kind)
{
    switch (kind)
    {
        case CG_DEP_REGIN  : // edges related to register are black
        case CG_DEP_REGOUT :
        case CG_DEP_REGANTI:
        return("black");
    
        case CG_DEP_MEMIN  : // edges related to memory are blue
        case CG_DEP_MEMOUT :
        case CG_DEP_MEMANTI:
        case CG_DEP_MEMVOL :
        case CG_DEP_MEMREAD:
        return("blue");
            
        case CG_DEP_PREBR  : // edges related to branch are red
        case CG_DEP_POSTBR :
        return("red");

        default            :
        return("orange");
    }
}


typedef mempool_allocator<void *>   VOID_ALLOC;
typedef std::vector<void *, VOID_ALLOC>  VOID_CONTAINER;

/* ====================================================================
 *
 *  Class Name:     VOID_SET
 *
 *  Base Class:     <none>
 *
 *  Derived Class:  <none>
 *
 *  Class Description:    
 *
 *      For storing a number of void*.
 *
 *  Note: <none> 
 *
 * ====================================================================
 */
class VOID_SET
{
private:
    VOID_CONTAINER    _voids;
public:
    VOID_SET()    { _voids.clear(); }
    ~VOID_SET()   {}

    void Insert (void *p)       { _voids.push_back (p); }
    void Clear  ()              { _voids.clear();       }
    BOOL Is_In  (void *p);
};

BOOL
VOID_SET::Is_In(void *p)
{
    for (INT32 i=0; i< _voids.size() ; i++) {
        if ( p == _voids[i]) return TRUE;
    }
    return FALSE;
}


/* ====================================================================
 *
 *  Structure Name:     DAG_OP_INFO
 *
 *  Structure Description: 
 *    
 *      Store the information useful to dump the regional dependence graph.
 *
 *      node_id     is the NODE_ID of this OP, which is void* in fact. And
 *                  it is unique for the structure.
 *      dep_dumped  is a flag that the dependencies from this OP is dumped
 *                  or not in the graph NOW.
 *      dep_idx     is the index of this OP's dependencies in _dest_ops.
 *      dep_num     is the number of this OP's dependencies.
 *
 * ====================================================================
 */
typedef struct dag_op_info {
    NODE_ID     node_id;            // OP's NODE_ID is void*
    BOOL        dep_dumped;
    INT32       dep_idx, dep_num;
}DAG_OP_INFO;

typedef mempool_allocator<DAG_OP_INFO>          DAG_OP_INFO_ALLOC;
typedef std::vector<DAG_OP_INFO,DAG_OP_INFO_ALLOC>   DAG_OP_INFO_VECTOR;


/* ====================================================================
 *
 *  Structure Name:     DAG_DEP_INFO
 *
 *  Structure Description: 
 *    
 *      Store the information useful to dump the dependencies when a OP
 *      in a regional graph is selected.
 *
 *      dest_op     is the Destination OP of the Dependency.
 *      kind        is the kind of Dependency.
 *
 * ====================================================================
 */
typedef struct dag_dep_info {
    OP          *dest_op;           // Destination OP of the Dependency
    CG_DEP_KIND kind;               // kind of Dependency
    BOOL        dotted;             // arc is dotted or not
}DAG_DEP_INFO;

typedef mempool_allocator<DAG_DEP_INFO>             DAG_DEP_INFO_ALLOC;
typedef std::vector<DAG_DEP_INFO,DAG_DEP_INFO_ALLOC>     DAG_DEP_INFO_VECTOR;


/* ====================================================================
 *
 *  Class Name:     DAG_OPS_INFO
 *
 *  Base Class:     <none>
 *
 *  Derived Class:  <none>
 *
 *  Class Description:    
 *
 *      In a regional dependence graph, when a OP node is selected, its
 *      dependencies should be dumped. The dependency inquiry is handled
 *      by the class of DAG_OPS_INFO.
 *
 *      _ops_info   is a vector to store the dependency information of
 *                  all the OP that have been selected. Every OP is an
 *                  item. This access inqury is easier than location to
 *                  the compiler's data structures in future use.
 *      _deps_info  is a vector to store the dependencies of all the OPs
 *                  that have been selected. It should be accessed with
 *                  index from _ops_info.
 *
 *      Insert_op_info  is a member function to add a OP's dependence 
 *                  information to _ops_info and _deps_info, so that
 *                  Dependencies() can access the information more easily
 *                  in future use.
 *      Dependeies  is a member function to inquiry the dependencies of
 *                  an OP. It MUST be used after Insert_op_info().
 *
 *  Note: <none> 
 *
 * ====================================================================
 */
class DAG_OPS_INFO {
private:
    DAG_OP_INFO_VECTOR      _ops_info;
    DAG_DEP_INFO_VECTOR     _deps_info;
public:
    DAG_OPS_INFO()   {
        _ops_info.clear();
        _deps_info.clear();
    }
    ~DAG_OPS_INFO()  {}

    void                    Insert_op_info( OP *op, BOOL dep_in_bb,
                                            BOOL dep_dumped, 
                                            VOID_SET *bb_ops);
    DAG_DEP_INFO_VECTOR     Dependencies (const NODE_ID op, BOOL& dumped);
};

/* ====================================================================
 *
 *  DAG_OPS_INFO::Insert_op_info (  OP *op, BOOL dep_in_bb, BOOL dep_dumped, 
 *                                  VOID_SET *bb_ops )
 *
 *  A member function to add a OP's dependence information to _ops_info
 *  and _deps_info.
 *
 *  op              is the op to queried.
 *  dep_in_bb       is a flag that whether dependencies is restriced in 
 *                  a BB. Default value is FALSE.
 *  dep_dumped      is a flag that is set by the extern functions
 *                  that call this Insert_op_info(...) function. Actually,
 *                  it describe whether the dependencies are displayed
 *                  at first when the graph is visualized.
 *  bb_ops          is a pointer to a VOID_SET that contains all the OPs
 *                  in this BB. This parameter is valid only if dep_in_bb
 *                  is TRUE. Default value is NULL.
 *
 * ====================================================================
 */
void
DAG_OPS_INFO::Insert_op_info (  OP *op, BOOL dep_in_bb, BOOL dep_dumped,
                                VOID_SET *bb_ops )
{
    DAG_OP_INFO             tmp_op_info;
    DAG_DEP_INFO            tmp_dep_info;
    INT32                   idx, succ_counter, deps_info_size;

    // Since the compiler will add the PREBR or POSTBR dependency from
    // each OP to the last OP, these Branch relative dependencies is
    // not necessary to dump. But if stick to dumping, use option of 
    // "-Wb,-VT:dag_br".
    BOOL    show_branch_edge;
    if (VT_Enable_DAG_BR) show_branch_edge = TRUE;
        else show_branch_edge = FALSE;

    for (INT32 i=0; i< _ops_info.size() ; i++) {
        if ( ((NODE_ID)op) == _ops_info[i].node_id ) return;
    }

    // if op is NOT in the _ops_info, _CG_DEP_OP_INFO should be used
    // to find the dependencies and the correspond info should be added
    // into _ops_info and _dest_ops.
    _CG_DEP_OP_INFO*    this_op_dep_info    = _CG_DEP_op_info (op);
    ARC_LIST*           this_op_succ_list   = this_op_dep_info -> succs;

    succ_counter = 0;       // count the succ OPs, for Updating _ops_info
    deps_info_size = _deps_info.size();     // for Updating _deps_info

    for ( ARC* arc = ARC_LIST_first (this_op_succ_list) ;
        arc != NULL ; arc = ARC_LIST_first( arc -> next[1]) )
    {
        tmp_dep_info.dest_op    = arc -> succ;
        tmp_dep_info.kind       = ARC_kind(arc);
        tmp_dep_info.dotted     = ARC_is_dotted(arc);

        if (! show_branch_edge)
            if (CG_DEP_PREBR    == tmp_dep_info.kind || 
                CG_DEP_POSTBR   == tmp_dep_info.kind)
                continue;
        
        // When the dependencies are to be restricted in BB, if a 
        // dependency's destination OP is not in this BB, just skip it.
        if (dep_in_bb) {
            if ( ! (bb_ops -> Is_In(tmp_dep_info.dest_op)) )    continue;
        } else if ( bb_ops -> Is_In(tmp_dep_info.dest_op) )     continue;

        _deps_info.push_back (tmp_dep_info);    // Update _deps_info
        succ_counter ++;                        // for updating _ops_info
    }

    tmp_op_info.node_id     = (NODE_ID) op;
    tmp_op_info.dep_dumped  = dep_dumped;
    tmp_op_info.dep_idx     = deps_info_size;
    tmp_op_info.dep_num     = succ_counter;

    // Update _ops_info, for easier access in future use 
    _ops_info.push_back (tmp_op_info);
}

/* ====================================================================
 *
 *  DAG_OPS_INFO::Dependencies (const NODE_ID op, BOOL dumped)
 *
 *  A member function to enquire the dependencies of op.
 *
 *  Return Value    is a vector of dependencies information.
 *  op              is the op to queried.
 *  dumped          is an flag that whether dependencies from this OP is
 *                  dumped or not in the graph NOW.
 *
 * ====================================================================
 */
DAG_DEP_INFO_VECTOR
DAG_OPS_INFO::Dependencies (const NODE_ID op, BOOL& dumped)
{
    DAG_DEP_INFO_VECTOR     dep_vector;     // space for return value
    DAG_OP_INFO             tmp_op_info;
    DAG_DEP_INFO            tmp_dep_info;
    INT32                   idx, succ_counter, deps_info_size;
    
    dep_vector.clear();

    // if op is in the _ops_info, just find the index and number of
    // dependencies and the destination OPs can be find easily.
    for (INT32 i=0; i< _ops_info.size() ; i++) {
        if ( op == _ops_info[i].node_id ) {
            tmp_op_info = _ops_info[i];
            dumped      = tmp_op_info.dep_dumped;
            idx         = tmp_op_info.dep_idx;
            for (INT32 j = 0; j < tmp_op_info.dep_num; j ++ ) {
                dep_vector.push_back( _deps_info[idx + j] );
            }
            _ops_info[i].dep_dumped = ! _ops_info[i].dep_dumped;
            return dep_vector;
        }
    }

    // if op is NOT in the _ops_info, something ERROR.
    Is_True(TRUE, ("Dependencies() must be afer Insert_op_info()."));
    
}

/* ====================================================================
 *
 *  Class Name:     BB_DAG_Callback
 *
 *  Base Class:     <DaVinci_Callback>
 *
 *  Derived Class:  <none>
 *
 *  Class Description: 
 *    
 *      Response to daVinci events is managented by Callback functions.
 *      The dumping function draw_bb_dependence_graph() creates a 
 *      subclass of DaVinci_Callback and redefines the virtual member for 
 *      events. The Callback subclass object is passed to Event_Loop().
 *
 *   Note: <none> 
 *
 * ====================================================================
 */
class BB_DAG_Callback : public DaVinci_Callback {
private:
    DaVinci         *_dv;
    VOID_SET        *_bb_ops;
    DAG_OPS_INFO    _dag_ops_info;
public:
    BB_DAG_Callback(DaVinci* dv, VOID_SET *bb_ops)
    {
        _dv = dv;
        _bb_ops = bb_ops;
    }
    ~BB_DAG_Callback (){}

    void Insert_op_info( OP *op ) {
        _dag_ops_info.Insert_op_info ( op, TRUE, TRUE, _bb_ops );
    }
    virtual void Node_Select( const INT n_ids, const NODE_ID id_array[] );
};

void
BB_DAG_Callback::Node_Select(const INT n_ids, const NODE_ID id_array[])
{
    NODE_ID                 node;
    DAG_DEP_INFO_VECTOR     succ_nodes;
    DAG_DEP_INFO            tmp_dep_info;
    BOOL                    dumped;
    EDGE_TYPE               et,et_plain,et_dotted;

    et_dotted.Pattern (EP_DOTTED);

    _dv -> Update_Begin();
    for (INT32 i = 0; i < n_ids; ++i) {
        node = id_array[i];

        succ_nodes = _dag_ops_info.Dependencies ( node, dumped );
        if (dumped) {
            for (INT32 i=0 ; i<succ_nodes.size() ; i++ ) {
                _dv -> Delete_Edge ( EDGE_ID ( node , (NODE_ID)(INTPTR)i ));
            }
            dumped = FALSE;
        }   // of if
        else {
            for (INT32 i=0 ; i<succ_nodes.size() ; i++ ) {
                tmp_dep_info = succ_nodes[i];

                if (tmp_dep_info.dotted)    et = et_dotted;
                    else                    et = et_plain;

                // to identify whether the edge is related to scheduled OP
                if ( OP_bundled((OP*)node) &&
                     OP_bundled(tmp_dep_info.dest_op) )
                    et.Color("gray");
                else {     
                    // to identify the edge's dependence kind
                    et.Color(Dependence_Color(tmp_dep_info.kind));
                }
                _dv -> New_Edge ( EDGE_ID ( node , (NODE_ID)(INTPTR) i ), et,
                                  node, (NODE_ID) tmp_dep_info.dest_op );
            }   // of for(INT32 ...)
            dumped = TRUE;
        }   // of else
    }   // of for (INT32 ...)
    _dv -> Update_End();

    char buffer[1024];
    char *s = Print_OP ((OP*)node, buffer);
    Is_True(strlen(s) < 1024, ("Print_OP buffer overflowed"));
    _dv -> Show_Message (s);
}

/* ====================================================================
 *
 *  Class Name:     Regioanl_DAG_Callback
 *
 *  Base Class:     <DaVinci_Callback>
 *
 *  Derived Class:  <none>
 *
 *  Class Description: 
 *    
 *      Response to daVinci events is managented by Callback functions.
 *      The dumping function draw_regional_dependence_graph() creates a 
 *      subclass of DaVinci_Callback and redefines the virtual member for 
 *      events. The Callback subclass object is passed to Event_Loop().
 *
 *  Note:
 *
 *      To distinguish the dependencies in or across BB, the second part
 *      of EDGE_ID of dependencies across BB are all odd number. On
 *      the contrary, the second part of EDGE_ID of dependencies in BB
 *      are all even number.
 *
 * ====================================================================
 */
class Regioanl_DAG_Callback : public DaVinci_Callback {
private:
    DaVinci         *_dv;
    VOID_SET        *_regions, *_no_dep_bbs;
    DAG_OPS_INFO    _dag_ops_info;
public:
    Regioanl_DAG_Callback(DaVinci* dv,VOID_SET *regions,VOID_SET *no_dep_bbs)
    {
        _dv = dv;
        _regions = regions;
        _no_dep_bbs = no_dep_bbs;
    }
    ~Regioanl_DAG_Callback (){}

    void Insert_op_info( OP *op, VOID_SET *bb_ops ) {
        _dag_ops_info.Insert_op_info ( op, FALSE, FALSE, bb_ops );
    }
    virtual void Node_Select( const INT n_ids, const NODE_ID id_array[] );
};

void
Regioanl_DAG_Callback::Node_Select(const INT n_ids, const NODE_ID id_array[])
{
    NODE_ID                 node;
    DAG_DEP_INFO_VECTOR     succ_nodes;
    DAG_DEP_INFO            tmp_dep_info;
    BOOL                    dumped;
    EDGE_TYPE               et,et_plain,et_dotted;

    et_dotted.Pattern (EP_DOTTED);

    for (INT32 i = 0; i < n_ids; ++i) {
        node = id_array[i];
        if ( _regions -> Is_In(node) )
            draw_regional_cfg( (REGION*) node);
        else if ( _no_dep_bbs -> Is_In(node) ) {}
        else        
        {   // if the selected node is NOT a Non-DEP BB, draw the dependencies

            //if the selected node is a scheduled OP, skip it.
            if ( ! OP_bundled((OP*)node) ) {
                _dv -> Update_Begin();
                succ_nodes = _dag_ops_info.Dependencies ( node, dumped );

                // To distinguish the dependencies in or across BB,
                // the second part of EDGE_ID of dependencies across BB are 
                // all odd number.
                if (dumped) {
                    for (INT32 i=0 ; i<succ_nodes.size() ; i++ ) {
                        _dv -> Delete_Edge (EDGE_ID (node ,NODE_ID((INTPTR)(2*i+1))));
                    }
                    dumped = FALSE;
                }   // of if
                else {
                    for (INT32 i=0 ; i<succ_nodes.size() ; i++ ) {
                        tmp_dep_info = succ_nodes[i];

                        if (tmp_dep_info.dotted)    et = et_dotted;
                            else                    et = et_plain;

                        // to identify edge is related to scheduled OP or not
                        if ( OP_bundled((OP*)node) &&
                             OP_bundled(tmp_dep_info.dest_op) )
                            et.Color("gray");
                        else {     
                            // to identify the edge's dependence kind
                            et.Color(Dependence_Color(tmp_dep_info.kind));
                        }
                        _dv -> New_Edge (EDGE_ID (node,NODE_ID((void*)INTPTR(2*i+1))), et,
                                         node,(NODE_ID)tmp_dep_info.dest_op);
                    }   // of for(INT32 ...)
                    dumped = TRUE;
                }   // of else
                _dv -> Update_End();
            }

            char buffer[1024];
            char *s = Print_OP ((OP*)node, buffer);
            Is_True(strlen(s) < 1024, ("Print_OP buffer overflowed"));
            _dv -> Show_Message (s);
        }   // of else
    }   // of for (INT32 ...)

}

/* ====================================================================
 *
 *  draw_bb_dependence_graph(BB *bb)
 *
 *  Visualization Flag:     VT_Enable_BB_DAG 
 *  Visualization Option:   -Wb,-VT:bb_dag
 *
 *  Visualize a dependence graph (DAG) of a BB. In the DAG, nodes are
 *  OPs and an edge indicates a dependence between the connected nodes.
 *
 *  If a OP node is selected, its dependency(ies) is hided. Re-select it
 *  to re-display the denpendency(ies) from the node. And, more detailed
 *  information of the OP is displayed on the status bar of the daVinci
 *  window.
 *
 *  NOTE:   Since the compiler will add the PREBR or POSTBR dependency 
 *          from each OP to the last OP, these Branch relative
 *          dependencies is not necessary to dump. But if necessary to 
 *          dumping, use option of "-Wb,-VT:dag_br".
 *
 * ====================================================================
 */
void draw_bb_dependence_graph(BB *bb,const char *mes)
{
    if (! DaVinci::enabled(TRUE)) return;

    MEM_POOL dv_pool; 
    dv_pool.magic_num = 0;
    MEM_POOL_Constructor pool( &dv_pool, "DaVinci", FALSE);

    DaVinci dv (&dv_pool, NULL);

    char window_title[128];         // set the window's label
    if ( NULL == mes)
        sprintf(window_title,"DAG for BB %d - %s",
                BB_id(bb), Cur_PU_Name);
    else
        sprintf(window_title,"DAG for BB %d - %s - %s ",
                BB_id(bb), Cur_PU_Name, mes);
    Is_True(strlen(window_title) < 128, ("Window Title buf overflowed"));
    dv.Title(window_title);

    // Now we start drawing
    char        nlabel[64];             // temp space for node label
    INT32       edge_id_number;         // used as edge ID
    VOID_SET    bb_ops;
    NODE_TYPE   nt, nt_plain, nt_cross_bb;
                                        // to store the current OP type
    EDGE_TYPE   et,et_dotted,et_plain;  // to store the current edge type
    BOOL        show_branch_edge;       // to dump branch dependence or not
    BOOL        dep_cross_bb;

    nt_cross_bb.Boarder (NB_DOUBLE);
    et_dotted.Pattern   (EP_DOTTED);

    // Since the compiler will add the PREBR or POSTBR dependency from
    // each OP to the last OP, these Branch relative dependencies is
    // not necessary to dump. But if stick to dumping, use option of 
    // "-Wb,-VT:dag_br".
    if (VT_Enable_DAG_BR) show_branch_edge = TRUE;
        else show_branch_edge = FALSE;

    OPS op_list = bb->ops;

    // Firstly, tranverse all the OP in this BB, and store them into bb_ops
    for (OP* op = op_list.first ; op != NULL ; op = op -> next) {
        bb_ops.Insert(op);
    }

    BB_DAG_Callback     bb_dag_callback (&dv, &bb_ops);
    for (OP* op = op_list.first ; op != NULL ; op = op -> next) {
        bb_dag_callback.Insert_op_info(op);
    }

    dv.Graph_Begin();
    for (OP* op = op_list.first ; op != NULL ; op = op -> next)
    {
        _CG_DEP_OP_INFO* this_op_dep_info = _CG_DEP_op_info(op);
        ARC_LIST* this_op_succ_list = this_op_dep_info -> succs;
  
        // to identify whether the op's dependency cross BB or not
        dep_cross_bb = FALSE;
        for ( ARC* arc = ARC_LIST_first (this_op_succ_list) ;
            arc != NULL ; arc = ARC_LIST_first( arc -> next[1]) )
            if ( ! bb_ops.Is_In(arc->succ) )    dep_cross_bb = TRUE;
        if (dep_cross_bb)   nt = nt_cross_bb;
            else nt = nt_plain;

        // to identify whether the op is scheduled or not
        if (OP_bundled(op)) nt.Color("orange"); // scheduled OP is orange
            else nt.Color("palegreen");         // unscheduled OP is green

        sprintf(nlabel,"[ %d ] %s",OP_map_idx(op),TOP_Name((TOP)(op->opr)));
        Is_True(strlen(nlabel) < 64, ("Node Label buf overflowed"));

        dv.Node_Begin (NODE_ID (op), nlabel, nt);

        edge_id_number = 0;

        // add all the node's successors (edges)
        for ( ARC* arc = ARC_LIST_first (this_op_succ_list) ;
            arc != NULL ; arc = ARC_LIST_first( arc -> next[1]) )
        {
            // If the arc destination OP is in not in this BB,
            // this arc is ignored.
            if (! bb_ops.Is_In(arc->succ)) continue;

            CG_DEP_KIND kind = ARC_kind(arc);
            if (ARC_is_dotted(arc)) et = et_dotted;
                else                et = et_plain;
            // to identify whether the edge is related to scheduled OP
            if ( (OP_bundled(arc->pred)) && OP_bundled(arc->succ) )
                et.Color("gray");
            else {
                // to identify the edge's dependence kind
                et.Color(Dependence_Color(kind));
            }

            if (    ((CG_DEP_PREBR != kind) && (CG_DEP_POSTBR != kind)) ||
                    show_branch_edge   ) {
                dv.Out_Edge ( EDGE_ID ( NODE_ID(op),
                                        NODE_ID(INTPTR(edge_id_number ++))),
                              et, NODE_ID (arc->succ));
            }; // of if
        } // of for( ARC* ... )

        dv.Node_End();
    }   // of for( OP* ... )

    dv.Graph_End();
    dv.Event_Loop(&bb_dag_callback);
}

/* ====================================================================
 *
 *  draw_regional_dependence_graph(REGION *r)
 *
 *  Visualization Flag:     VT_Enable_Regional_DAG
 *  Visualization Option:   -Wb,-VT:rgnl_dag
 *
 *  Visualize a dependence graph (DAG) of a region.
 *
 *  In fact, this graph is more like a regional control flow graph. If a
 *  regional cfg node is a region, just draw a node for a region. If a
 *  regional cfg node is a BB, all OPs in the BB are dumped. Edges between
 *  OP in the graph just means the sequence in the OP list. Edges between
 *  BB(REGION)s is derived from regional CFG.
 *
 *  If a OP node is selected, its dependency(ies) is dumped. Re-select it
 *  to remove the denpendency(ies) from the graph. And, more detailed
 *  information of the OP is displayed on the status bar of the daVinci
 *  window.
 *
 *  NOTE:   Since the compiler will add the PREBR or POSTBR dependency 
 *          from each OP to the last OP, these Branch relative
 *          dependencies is not necessary to dump. But if necessary to 
 *          dumping, use option of "-Wb,-VT:dag_br".
 *
 *          To distinguish the dependencies in or across BB, the second 
 *          part of EDGE_ID of dependencies across BB are all odd number. 
 *          On the contrary, the second part of EDGE_ID of dependencies 
 *          in BB are all even number.
 *
 * ====================================================================
 */
void draw_regional_dependence_graph(REGION *r,const char *mes)
{
    if (! DaVinci::enabled(TRUE)) return;

    MEM_POOL dv_pool; 
    dv_pool.magic_num = 0;
    MEM_POOL_Constructor pool( &dv_pool, "DaVinci", FALSE);

    DaVinci dv (&dv_pool, NULL);

    char window_title[128];             // set the window's label
    if ( NULL == mes)
        sprintf(window_title,"Reduced DAG for Region %d - %s",
                r -> Id(), Cur_PU_Name);
    else
        sprintf(window_title,"Reduced DAG for Region %d - %s - %s ",
                r -> Id(), Cur_PU_Name, mes);
    Is_True(strlen(window_title) < 128, ("Window Title buf overflowed"));
    dv.Title(window_title);

    //Now we start drawing
    REGIONAL_CFG_NODE   *being_drawn_node;
    REGION              *being_drawn_region;
    BB                  *being_drawn_bb;
    void                *last_node;         // region or the last OP of BB
    VOID_SET            regions, no_dep_bbs, bb_ops;
    char                nlabel[64];         // temp space for node label
    INT32               edge_id_number;     // used as edge ID
    BOOL                show_branch_edge;   // dump branch dependency or not
    NODE_TYPE           nt,nt_region,nt_op_sched,nt_op_unsched,
                        nt_empty_bb,nt_entry_bb,nt_exit_bb;
    EDGE_TYPE           et,et_btwn_op,et_btwn_bb,et_dotted,et_plain;

    // If a regional cfg node is a region, just draw a node for a region.
    nt_region.Color     ("orange");
    nt_region.Boarder   (NB_DOUBLE);
    nt_region.Shape     (NS_CIRCLE);
    // If a regional cfg node is a Non-Empty BB, all OPs in the BB are dumped.
    nt_op_sched.Color   ("pink");
    nt_op_unsched.Color ("palegreen");
    // If a regional cfg node is an Entry BB, or an Exit BB or an Empty BB,
    // just draw a node.
    nt_entry_bb.Color   ("gray");
    nt_entry_bb.Boarder (NB_DOUBLE);
    nt_entry_bb.Shape   (NS_RHOMBUS);
    nt_exit_bb.Color    ("gray");
    nt_exit_bb.Boarder  (NB_DOUBLE);
    nt_exit_bb.Shape    (NS_ELLIPSE);
    nt_empty_bb.Color   ("gray");
    nt_empty_bb.Boarder (NB_DOUBLE);
    nt_empty_bb.Shape   (NS_BOX);
    // Edges between OP in the graph just means the sequence in the OP list.
    // The color of this kind of edges is white, which is INVISIBLE in fact.
    et_btwn_op.Direction(ED_NONE);
    et_btwn_op.Color    ("white");
    // Edges between BB(REGION)s is derived from regional CFG.
    et_btwn_bb.Pattern  (EP_THICK);
    et_btwn_bb.Color    ("pink");
    // Edge which is dotted by ARC_is_dotted(arc) is dotted.
    et_dotted.Pattern   (EP_DOTTED);


    if (VT_Enable_DAG_BR) show_branch_edge = TRUE;
        else show_branch_edge = FALSE;

    Regioanl_DAG_Callback regional_dag_callback(&dv, &regions, &no_dep_bbs);

    dv.Graph_Begin();

    REGIONAL_CFG *cfg = r -> Regional_Cfg();

    for (SEQ_REGIONAL_CFG_ITER iter(cfg) ; iter != 0 ; ++ iter)
    {
        being_drawn_node = *iter;
        if ( being_drawn_node -> Is_Region() ) {
            being_drawn_region = being_drawn_node -> Region_Node();
            sprintf(nlabel, "  %d  ", being_drawn_region -> Id());
            Is_True(strlen(nlabel) < 64, ("Node Label buf overflowed"));
            dv.Node_Begin ( NODE_ID(being_drawn_region), nlabel, nt_region);

            // Store all the sub-regions, for use in Node_Select
            regions.Insert( being_drawn_region );
            last_node = being_drawn_region;
        }   // of if
        else {  
            being_drawn_bb = being_drawn_node -> BB_Node();
            if ( BB_entry(being_drawn_bb) || BB_exit(being_drawn_bb) ||
                 0 == BB_length(being_drawn_bb) )
            {
                // If the CFG node is an Entry BB, or an Exit BB or 
                // an Empty BB,just draw a node.
                sprintf(nlabel, "  %d  ", being_drawn_bb -> id);
                Is_True(strlen(nlabel) < 64, ("Node Label buf overflowed"));

                if (BB_entry(being_drawn_bb))  nt = nt_entry_bb;
                else if (BB_exit(being_drawn_bb))   nt = nt_exit_bb;
                else if (0 == BB_length(being_drawn_bb)) nt = nt_empty_bb;
                dv.Node_Begin(NODE_ID(being_drawn_bb), nlabel, nt);

                no_dep_bbs.Insert( being_drawn_bb );
                last_node = being_drawn_bb;
            }   // of if
            else {
                // If the CFG node is NOT a Non-DEP BB, all the OPs in the BB
                // are dumped.
                OPS op_list = being_drawn_bb -> ops;
                OP  *op;

                // Firstly, tranverse all the OP in this BB
                for (OP* op = op_list.first ; op != NULL ; op = op -> next) {
                    bb_ops.Insert(op);
                }

                for (OP* op = op_list.first ; op != NULL ; op = op -> next) {
                    regional_dag_callback.Insert_op_info(op, &bb_ops);
                }

                for ( op = op_list.first; op!= NULL; op = op -> next) {
                    // Set the OP node's appearance
                    if (OP_bundled(op))  nt=nt_op_sched;    // scheduled
                        else    nt = nt_op_unsched;         // unscheduled
                    sprintf(nlabel,"[ %d ] %s", OP_map_idx(op),
                                                TOP_Name((TOP)(op->opr)));
                    Is_True(strlen(nlabel)<64,("Node Label buf overflowed"));

                    dv.Node_Begin (NODE_ID (op), nlabel, nt);

                    _CG_DEP_OP_INFO* this_op_dep_info = _CG_DEP_op_info(op);
                    ARC_LIST* this_op_succ_list = this_op_dep_info -> succs;

                    edge_id_number = 0;

                    // add all the node's successors (edges)
                    for ( ARC* arc = ARC_LIST_first (this_op_succ_list) ;
                        arc != NULL ; arc = ARC_LIST_first( arc -> next[1]) )
                    {
                        // If the arc destination OP is in not in this BB,
                        // this arc is ignored.
                        if (! bb_ops.Is_In(arc->succ)) continue;

                        CG_DEP_KIND kind = ARC_kind(arc);
                        if (ARC_is_dotted(arc)) et = et_dotted;
                            else                et = et_plain;
                        // to identify edge is related to scheduled OP or not
                        if ((OP_bundled(arc->pred)) && OP_bundled(arc->succ))
                            et.Color("gray");
                        else {     
                            // to identify the edge's dependence kind
                            et.Color(Dependence_Color(kind));
                        }

                        // To distinguish the dependencies in or across BB,
                        // the second part of EDGE_ID of dependencies in BB
                        // are all even number.
                        if (((CG_DEP_PREBR != kind)&&(CG_DEP_POSTBR != kind))
                            || show_branch_edge   ) {
                            dv.Out_Edge ( EDGE_ID ( NODE_ID(op),
                                          NODE_ID(INTPTR(2* edge_id_number++))),
                                          et, NODE_ID (arc->succ));
                        }
                    } // of for( ARC* ... )

                    if (op->next!= NULL) {
                        dv.Out_Edge(EDGE_ID( NODE_ID(op),NODE_ID(op->next) ),
                                    et_btwn_op,
                                    NODE_ID (op -> next));
                        dv.Node_End   ();
                    }   // of if
                    last_node = op;
                }   // of for(op= ... )
                bb_ops.Clear();
            }   // of else
        }   // of else

        for (CFG_SUCC_NODE_ITER kid_iter(being_drawn_node);
             kid_iter!=0; ++ kid_iter)
        {
            REGIONAL_CFG_NODE   *kid_cfg_node = *kid_iter;
            void                *kid_node;
            if ( kid_cfg_node -> Is_Region() )
                kid_node  = kid_cfg_node -> Region_Node();
            else {
                being_drawn_bb = kid_cfg_node -> BB_Node();
                if ( BB_entry(being_drawn_bb) || BB_exit(being_drawn_bb) ||
                     0 == BB_length(being_drawn_bb) )
                    kid_node = being_drawn_bb;
                else kid_node = being_drawn_bb -> ops.first;
            }   // of else

            dv.Out_Edge(EDGE_ID(last_node, kid_node), et_btwn_bb, kid_node);
        }   // of for(CFG_SUCC_NODE_ITER ...)

        dv.Node_End();
    }   // of for(SEQ_REGIONAL_CFG_ITER ...)

    dv.Graph_End();

    dv.Event_Loop( (DaVinci_Callback*) &regional_dag_callback );
}


/* ====================================================================
 *  Print_OP (OP *op, char *buf)
 *
 *  Print a OP's detailed information as a string.
 *
 * ====================================================================
 */
char *
Print_OP (OP *op, char *buf)
{
    char *result = buf;

    WN *wn;
    BOOL cg_loop_op = Is_CG_LOOP_Op(op);
    buf += sprintf(buf,"[ %d ] ", OP_map_idx(op) );
    if (OP_has_tag(op)) {
        LABEL_IDX tag = Get_OP_Tag(op);
        buf += sprintf (buf, "<tag %s>: ", LABEL_name(tag) );
    }
    for (INT32 i = 0; i < OP_results(op); i++) {
        char buffer[1024];
        char *s = sPrint_TN (OP_result(op,i), FALSE, buffer);
        Is_True(strlen(s) < 1024, ("Print_TN buffer overflowed"));
        buf += sprintf(buf, " %s", s);
    }
    buf += sprintf(buf," %s", TOP_Name((TOP)(op->opr)) );
    if ( OP_variant(op) != 0 )
        buf += sprintf (buf, " (%x)", OP_variant(op) );
    for (INT32 i=0; i<OP_opnds(op); i++) {
        TN *tn = OP_opnd(op,i);
        char buffer[1024];
        char *s = sPrint_TN (tn, FALSE, buffer);
        Is_True(strlen(s) < 1024, ("Print_TN buffer overflowed"));
        buf += sprintf(buf, " %s", s);

        if ( cg_loop_op ) {
            INT omega = TN_is_symbol(tn) ? OP_restore_omega(op) :
                                           OP_omega(op,i);
            if (omega)
                buf += sprintf(buf, "[%d]", omega);
        }
        if (OP_Defs_TN(op, tn)) buf += sprintf(buf, "<defopnd>");
        buf += sprintf(buf, " ");
    }
    if (OP_glue(op))        buf += sprintf (buf, " glue");
    if (OP_no_alias(op))    buf += sprintf (buf, " noalias");
    if (OP_copy(op))        buf += sprintf (buf, " copy");
    if (OP_volatile(op))    buf += sprintf (buf, " volatile");
    if (OP_side_effects(op))buf += sprintf (buf, " side_effects");
    if (OP_hoisted(op))     buf += sprintf (buf, " hoisted");
    if (OP_cond_def(op))    buf += sprintf (buf, " cond_def");
    if (OP_end_group(op))   buf += sprintf (buf, " end_group");
    if (OP_tail_call(op))   buf += sprintf (buf, " tail_call");
    if (OP_no_move_before_gra(op)) buf += sprintf (buf, " no_move");
    if (OP_Scheduled(op))   buf += sprintf (buf, " scheduled");

    if (wn = Get_WN_From_Memory_OP(op)) {
        char buffer[500];
        buffer[0] = '\0';
        if (Alias_Manager) Print_alias_info (buffer, Alias_Manager, wn);
        buf += sprintf(buf, " WN=%p %s", wn, buffer);
    }
    if (OP_unrolling(op)) {
        UINT16 unr = OP_unrolling(op);
        buf += sprintf(buf, " %d%s unrolling", unr,
            unr == 1 ? "st" : unr == 2 ? "nd" : unr == 3 ? "rd" : "th");
    }
    return result;
}
