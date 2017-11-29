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
// Module: vt_region.cxx
// $Date: 2005/12/30 01:47:13 $
// $Author: weitang $
// $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/region/vt_region.cxx,v $
//
// Description:
//
// Functions for visualizing Graphs about Region using daVinci.
//
//*********************************************************************

#include "bb.h"
#include "DaVinci.h"
#include "glob.h"
#include "mempool.h"
#include "cg.h"        /* for Alias_Manager */
#include "whirl2ops.h" /* for Get_WN_From_Memory_OP */
#include "op.h"
#include "tag.h"
#include "region.h"
#include "vt_region.h"
#include "cg_loop.h"
#include "ipfec_options.h"
#include <deque>

static char* sPrint_OP (OP *op, char *buf);

/* ====================================================================
 *
 *  Structure Name:     CFG_LABEL_INFO
 *
 *  Structure Description: 
 *    
 *      Store the information useful to dump the edge's label.
 *
 * ====================================================================
 */
typedef struct cfg_label_info {
    float     prob;
    float     freq;
    NODE_ID   dest_node_id;
}CFG_LABEL_INFO;


/* ====================================================================
 *
 *  draw_bb_op (BB *bb)
 *
 *  Visualization Flag:     VT_Enable_BB_OP
 *  Visualization Option:   -Wb,-VT:bb_op
 *
 *  Dumping all the OPs in a BB.
 *
 * ====================================================================
 */
void draw_bb_op (BB *bb,const char *mes)
{
    if (! DaVinci::enabled(TRUE)) return;

    MEM_POOL dv_pool; 
    dv_pool.magic_num = 0;
    MEM_POOL_Constructor pool( &dv_pool, "DaVinci", FALSE);

    DaVinci dv (&dv_pool, NULL);

    char window_title[128];         // set the window's label
    if ( NULL == mes)
        sprintf(window_title,"OP in BB %d , BB Freq is %#.5f - %s",
                BB_id(bb), bb -> freq, Cur_PU_Name);
    else
        sprintf(window_title,"OP in BB %d , BB Freq is %#.5f - %s - %s ",
                BB_id(bb), bb -> freq, Cur_PU_Name, mes);
    Is_True(strlen(window_title) < 128, ("Window Title buf overflowed"));
    dv.Title(window_title);

    // Now we start drawing
    NODE_TYPE   nt;                 // to store the current OP type
    EDGE_TYPE   et;                 // to store the current edge type

    et.Direction(ED_NONE);
    et.Color    ("gray");

    dv.Graph_Begin();

    OPS op_list = bb->ops;

    for (OP* op = op_list.first ; op != NULL ; op = op -> next)
    {
        // set the OP's label
        char buffer[1024];          
        char *nlabel = sPrint_OP (op, buffer);   // temp space for node label
        Is_True(strlen(nlabel) < 1024, ("Node Label buf overflowed"));

        dv.Node_Begin (NODE_ID (op), nlabel, nt);
        if ( NULL != (op -> next) )
            dv.Out_Edge ( EDGE_ID ( NODE_ID(op), NODE_ID(op -> next)),
                          et, 
                          NODE_ID (op -> next));
        dv.Node_End();
    }   // of for( OP* ... )

    dv.Graph_End();
    dv.Event_Loop(NULL);
}

/* ====================================================================
 *
 *  Class Name:     Regional_CFG_Callback
 *
 *  Base Class:     <DaVinci_Callback>
 *
 *  Derived Class:  <none>
 *
 *  Class Description: 
 *    
 *      Response to daVinci events is managented by Callback functions.
 *      The dumping function draw_regional_cfg() creates a subclass of
 *      DaVinci_Callback and redefines the virtual member for events.
 *      The Callback subclass object is passed to Event_Loop().
 *
 *   Note: <none> 
 *
 * ====================================================================
 */
class Regional_CFG_Callback : public DaVinci_Callback {
private:
    DaVinci         *_dv;
    INT32           _pseudo_node_num;
    REGIONAL_CFG    *_cfg;
public:
    Regional_CFG_Callback (DaVinci *dv,REGIONAL_CFG *cfg,INT32 label_num = 0)
    {
        _dv = dv;
        _pseudo_node_num = label_num;
        _cfg = cfg;
    }
    ~Regional_CFG_Callback (){}

    virtual void Node_Select( const INT n_ids, const NODE_ID id_array[] );
    virtual void Edge_Select( const EDGE_ID& edge_id);
};


/* ====================================================================
 *
 *  Regional_CFG_Callback::Node_Select
 *
 *  In a regional cfg, some nodes are REGIONs and some are BBs. When a
 *  REGION is selected, its regional cfg is to be dumped. When a BB is
 *  selected, its ops are to be dumped.
 *
 * ====================================================================
 */
void
Regional_CFG_Callback::Node_Select(const INT n_ids, const NODE_ID id_array[])
{
    REGIONAL_CFG_NODE* node;
    for (INT32 i = 0; i < n_ids; ++i) {
        node = (REGIONAL_CFG_NODE*) id_array[i];
        if ( ((INT32)node) > _pseudo_node_num)
            if ( node -> Is_Region() )
                draw_regional_cfg ( node -> Region_Node() );
            else draw_bb_op ( node -> BB_Node() );
    }
}

/* ====================================================================
 *
 *  Regional_CFG_Callback::Edge_Select
 *
 *  In a regional cfg, when a edge is selected, its frequence is 
 *  displayed on the status bar of the daVinci window.
 *
 * ====================================================================
 */
void
Regional_CFG_Callback::Edge_Select(const EDGE_ID& edge_id)
{
    if (! VT_Enable_CFG_Label)
    {
        REGIONAL_CFG_NODE   *src_node = (REGIONAL_CFG_NODE*) edge_id.src;
        REGIONAL_CFG_NODE   *dst_node = (REGIONAL_CFG_NODE*) edge_id.dst;
        REGIONAL_CFG_EDGE   *edge     = src_node -> Find_Succ_Edge(dst_node);
        float               freq      = _cfg -> Edge_Freq (edge);

        char    message[128];
        sprintf ( message, "Selected Edge Frequence: %#.5f", freq);
        Is_True(strlen(message)<128, ("daVinci message buffer overflowed"));
        _dv -> Show_Message (message);
   }
}

/* ====================================================================
 *
 *  draw_regional_cfg (Region *r)
 *
 *  Visualization Flag:     VT_Enable_Regional_CFG
 *  Visualization Option:   -Wb,-VT:rgnl_cfg
 *
 *  Visualize a Regional Control Flow Graph.
 *  
 *  NOTE:   To dump the edge's label, use option of -Wb,-VT:cfg_label.
 *          The edge's label is its probability and frequency.
 *
 * ====================================================================
 */
void draw_regional_cfg (REGION *r,const char *mes)
{
    if (! DaVinci::enabled (TRUE)) return;

    MEM_POOL dv_pool;
    dv_pool.magic_num = 0;		// force it to be unintialized
    MEM_POOL_Constructor pool (&dv_pool, "DaVinci", FALSE);

    DaVinci dv (&dv_pool, NULL);

    char window_title[128];                     // set the window's label
    if ( NULL == mes)
        sprintf(window_title,"Regional CFG for Region %d - %s",
                r -> Id(), Cur_PU_Name);
    else
        sprintf(window_title,"Regional CFG for Region %d - %s - %s ",
                r -> Id(), Cur_PU_Name, mes);
    Is_True(strlen(window_title) < 128, ("Window Title buf overflowed"));
    dv.Title(window_title);

    //Now we start drawing
    char                    nlabel[64];         // temp space for node label
    INT32                   pseudo_node_id=0;   // be used as label's id
    CFG_LABEL_INFO          label_info_item;
    vector<CFG_LABEL_INFO>  label_info_vector;
    BOOL                    show_edge_label;    // to dump edge label or not
    NODE_TYPE               nt,nt_bb,nt_region,nt_pseudo; // node type
    EDGE_TYPE               et_source,et_destination;
    REGIONAL_CFG_NODE       *being_drawn_node;

    nt_bb.Shape         (NS_CIRCLE);
    nt_region.Color     ("pink");
    nt_region.Boarder   (NB_DOUBLE);
    nt_pseudo.Shape     (NS_TEXT);             // pseudo_node is label
    et_source.Direction (ED_NONE);

    if (VT_Enable_CFG_Label) show_edge_label = TRUE;
        else show_edge_label = FALSE;

    dv.Graph_Begin();

    REGIONAL_CFG *cfg = r -> Regional_Cfg();

    for (SEQ_REGIONAL_CFG_ITER iter(cfg) ; iter != 0 ; ++ iter)
    {
        being_drawn_node = *iter;

        if ( being_drawn_node -> Is_Region() ) {
            nt = nt_region;
            sprintf(nlabel, " %d ", being_drawn_node->Region_Node()->Id() );
        }
        else {
            nt = nt_bb;
            if (BB_entry(being_drawn_node -> BB_Node())) nt.Shape(NS_RHOMBUS);
            if (BB_exit(being_drawn_node -> BB_Node()))  nt.Shape(NS_ELLIPSE);
            sprintf(nlabel, "%d", being_drawn_node -> BB_Node() -> id );
        }

        Is_True(strlen(nlabel) < 64, ("Node Label buf overflowed"));
        dv.Node_Begin (NODE_ID (being_drawn_node), nlabel, nt);

        for (CFG_SUCC_NODE_ITER kid_iter(being_drawn_node);
             kid_iter!=0; ++ kid_iter)
        {
            REGIONAL_CFG_NODE *kid_node = *kid_iter; 
            if (show_edge_label)
            { 
                REGIONAL_CFG_EDGE
                    *edge = being_drawn_node -> Find_Succ_Edge(kid_node);
                // store every label's information
                label_info_item.prob = cfg -> Edge_Prob (edge);
                label_info_item.freq = cfg -> Edge_Freq (edge);
                label_info_item.dest_node_id = NODE_ID (kid_node);
                label_info_vector.push_back (label_info_item);

                dv.Out_Edge (EDGE_ID (NODE_ID (being_drawn_node),
                                      NODE_ID (pseudo_node_id)),
                             et_source,
                             NODE_ID (pseudo_node_id));

                pseudo_node_id ++;        // count the number of labels
            }
            else {
                // to dump each edge's label: probability / frequency
                dv.Out_Edge(EDGE_ID(NODE_ID(being_drawn_node),
                                    NODE_ID(kid_node)),
                            et_destination,
                            NODE_ID(kid_node));
            }

        }
        dv.Node_End();
    }

    // Draw every edge label
    if (show_edge_label)
        for (INT32 i=0; i<pseudo_node_id ; i++)
        {
            // to dump each edge's label: probability / frequency
            sprintf(nlabel,"%#.2f / %#.2f", label_info_vector [i].prob,
                                            label_info_vector [i].freq);
            Is_True(strlen(nlabel) < 64, ("Node Label buf overflowed"));
            dv.Node_Begin (NODE_ID (i), nlabel, nt_pseudo);
            dv.Out_Edge(EDGE_ID(NODE_ID(i),label_info_vector[i].dest_node_id),
                        et_destination,
                        label_info_vector [i].dest_node_id );
            dv.Node_End ();
        }
    dv.Graph_End ();

    Regional_CFG_Callback regional_cfg_callback(&dv, cfg, pseudo_node_id);
    dv.Event_Loop( (DaVinci_Callback*) &regional_cfg_callback );
}



/* ====================================================================
 *
 *  Class Name:     Region_Tree_Callback
 *
 *  Base Class:     <DaVinci_Callback>
 *
 *  Derived Class:  <none>
 *
 *  Class Description: 
 *    
 *      Response to daVinci events is managented by Callback functions.
 *      The dumping function draw_region_tree() creates a subclass of
 *      DaVinci_Callback and redefines the virtual member for events.
 *      The Callback subclass object is passed to Event_Loop().
 *
 *   Note: <none> 
 *
 * ====================================================================
 */
class Region_Tree_Callback : public DaVinci_Callback {
public:
    Region_Tree_Callback (){}
    ~Region_Tree_Callback (){}

    virtual void Node_Select( const INT n_ids, const NODE_ID id_array[] );
};


/* ====================================================================
 *
 *  Region_Tree_Callback::Node_Select
 *
 *  In a region tree, every node is a region. When a node is selected,
 *  the regional cfg of that region is to be dumped.
 *
 * ====================================================================
 */
void
Region_Tree_Callback::Node_Select(const INT n_ids, const NODE_ID id_array[])
{
    NODE_ID node_id;
    for (INT32 i = 0; i < n_ids; ++i) {
        node_id = id_array[i];
        draw_regional_cfg ( (REGION*) node_id );
    }
}


/* ====================================================================
 *
 *  draw_region_tree (Region *r)
 *
 *  Visualization Flag:     VT_Enable_Region_Tree
 *  Visualization Option:   -Wb,-VT:rgn_tree
 *
 *  Visualize a region tree. The region tree is a hierarchical tree that
 *  reflects the composition of a PU. All the children of a parent node
 *  are all the composed part of that parent. And every node can be
 *  divided into its children for details if necessary.
 *
 * ====================================================================
 */
void draw_region_tree (REGION *r,const char *mes)
{
    if (! DaVinci::enabled(TRUE)) return;

    MEM_POOL dv_pool; 
    dv_pool.magic_num = 0;
    MEM_POOL_Constructor pool( &dv_pool, "DaVinci", FALSE);
  
    DaVinci dv (&dv_pool, NULL);

    char window_title[128];         // set the window's label
    if ( NULL == mes)
        sprintf(window_title,"Region Tree for Region %d - %s",
                r -> Id(), Cur_PU_Name);
    else
        sprintf(window_title,"Region Tree for Region %d - %s - %s ",
                r -> Id(), Cur_PU_Name, mes);
    Is_True(strlen(window_title) < 128, ("Window Title buf overflowed"));
    dv.Title(window_title);

    //Now we start drawing
    char            nlabel[16];     // temp space for node label
    NODE_TYPE       nt, nt_plain;   // to store the current OP type
    EDGE_TYPE       et;             // to store the current edge type
    std::deque<REGION*>  r_deque;        // for traversing region tree
    REGION          *being_drawn_region, *kid_r;

    r_deque.push_back (r);
    dv.Graph_Begin();

    while ( ! r_deque.empty() )
    {
        being_drawn_region = r_deque[0];
        r_deque.pop_front();

        nt = nt_plain;
        switch( being_drawn_region -> Region_Type() )
        {
            case UNKNOWN        :                                   break;
            case ROOT           : nt.Color ("lightgoldenrod");      break;
            case MEME           : nt.Color ("pink");                break;
            case SEME           : nt.Color ("palegreen");           break;
            case IMPROPER       : nt.Color ("gray");                break;
            case LOOP           : nt.Shape (NS_CIRCLE);             break;
        }
        switch( being_drawn_region -> Attribute() )
        {
            case NONE                   :                           break;
            case NO_FURTHER_OPTIMIZATION:   nt.Shape (NS_RHOMBUS);
                                            nt.Boarder(NB_DOUBLE);  break;
            case RIGID                  :   nt.Shape (NS_RHOMBUS);  break;
            case PERSISTENT_BOUNDARY    :   nt.Boarder(NB_DOUBLE);  break;
            case NO_OPTIMIZAION_ACROSS_BOUNDARY:
                                            nt.Shape (NS_ELLIPSE);  break;
        }

        // set the node's label
        sprintf(nlabel,"%d",being_drawn_region -> Id() );
        Is_True(strlen(nlabel) < 16, ("Node Label buf overflowed"));
        dv.Node_Begin (NODE_ID (being_drawn_region), nlabel, nt);

        if (being_drawn_region -> N_Kids() > 0 )
            for (REGION_KID_ITER r_kid_iter(being_drawn_region);
                 r_kid_iter != 0; ++ r_kid_iter )
            {
                kid_r = * r_kid_iter;
                r_deque.push_back (kid_r);
                dv.Out_Edge ( EDGE_ID ( NODE_ID(being_drawn_region),
                                        NODE_ID(kid_r) ),
                              et, NODE_ID (kid_r));
            } // of for

        dv.Node_End();
    }     // of while

    dv.Graph_End();

    Region_Tree_Callback region_tree_callback;
    dv.Event_Loop( (DaVinci_Callback*) &region_tree_callback );
}


/* ====================================================================
 *
 *  Class Name:     Global_CFG_Callback
 *
 *  Base Class:     <DaVinci_Callback>
 *
 *  Derived Class:  <none>
 *
 *  Class Description: 
 *    
 *      Response to daVinci events is managented by Callback functions.
 *      The dumping function draw_global_cfg() creates a subclass of
 *      DaVinci_Callback and redefines the virtual member for events.
 *      The Callback subclass object is passed to Event_Loop().
 *
 *   Note: <none> 
 *
 * ====================================================================
 */
class Global_CFG_Callback : public DaVinci_Callback {
private:
    INT32   _pseudo_node_num;
public:
    Global_CFG_Callback (INT32 label_num = 0)
        {_pseudo_node_num = label_num;}
    ~Global_CFG_Callback (){}

    virtual void Node_Select( const INT n_ids, const NODE_ID id_array[] );
};

/* ====================================================================
 *
 *  Global_CFG_Callback::Node_Select
 *
 *  In a global cfg, nodes are BBs. When a BB is selected, its ops are
 *  to be dumped.
 *
 * ====================================================================
 */
void
Global_CFG_Callback::Node_Select(const INT n_ids, const NODE_ID id_array[])
{
    BB* node;
    for (INT32 i = 0; i < n_ids; ++i) {
        node =  (BB*) id_array[i];
        if ( ((INT32)node) > _pseudo_node_num)
            draw_bb_op ( (BB*) node );
    }
}

/* ====================================================================
 *
 *  draw_global_cfg ()
 *
 *  Visualization Flag:     VT_Enable_Global_CFG
 *  Visualization Option:   -Wb,-VT:glbl_cfg
 *
 *  Visualize a global control flow graph. The edge's label is its
 *  probability and frequency.
 *  
 *  NOTE:   To dump the edge's label, use option of -Wb,-VT:cfg_label.
 *          The edge's label is its probability and frequency.
 *
 * ====================================================================
 */
void
draw_global_cfg(const char *mes)
{
    if (! DaVinci::enabled (TRUE)) return;

    MEM_POOL dv_pool;
    dv_pool.magic_num = 0;		// force it to be unintialized
    MEM_POOL_Constructor pool (&dv_pool, "DaVinci", FALSE);

    DaVinci dv (&dv_pool, NULL);

    char window_title[128];                     // set the window's label
    if ( NULL == mes)
        sprintf(window_title,"Global CFG - %s", Cur_PU_Name);
    else
        sprintf(window_title,"Global CFG - %s - %s ", Cur_PU_Name, mes);
    Is_True(strlen(window_title) < 128, ("Window Title buf overflowed"));
    dv.Title(window_title);

    // Now we start drawing
    char                    nlabel[64];         // temp space for node label
    INT32                   pseudo_node_id=0;   // be used as label's id
    CFG_LABEL_INFO          label_info_item;
    vector<CFG_LABEL_INFO>  label_info_vector;
    BOOL                    show_edge_label;    // to dump edge label or not
    NODE_TYPE               nt, nt_plain, nt_entry, nt_exit, nt_multi, 
                            nt_call, nt_pseudo;
    EDGE_TYPE               et_source,et_destination;

    // set the appearance of different kinds of nodes
    nt_entry.Shape      (NS_RHOMBUS);
    nt_exit.Shape       (NS_ELLIPSE);
    nt_multi.Shape      (NS_CIRCLE);
    nt_pseudo.Shape     (NS_TEXT);             // pseudo_node is label

    et_source.Direction(ED_NONE);

    if (VT_Enable_CFG_Label) show_edge_label = TRUE;
        else show_edge_label = FALSE;

    dv.Graph_Begin ();

    // add all nodes
    for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
        sprintf(nlabel,"%d",BB_id(bb));
        Is_True(strlen(nlabel) < 64, ("Node Label buf overflowed"));

        nt = nt_plain;
        if (BBlist_Len(BB_preds(bb)) > 1 || BBlist_Len(BB_succs(bb)) > 1)
            nt = nt_multi;
        if (BB_entry(bb)) nt = nt_entry;
        if (BB_exit(bb))  nt = nt_exit;

        if (BB_call(bb))  nt.Boarder (NB_DOUBLE);

        // BB with different BRANCH has different color
        OP *br_op = BB_xfer_op(bb);

        // Begin a node
        dv.Node_Begin (NODE_ID (bb), nlabel, nt);
        BBLIST *sedge;
	    BB     *succ_bb;
        for (sedge = bb->succs; sedge != NULL; sedge = sedge-> next)
        {
            succ_bb = sedge -> item;
 
            if (show_edge_label)
            { 
                // store every label's information
                label_info_item.prob = sedge -> prob;
                //label_info_item.freq = sedge -> freq;
                label_info_item.dest_node_id = NODE_ID (succ_bb);
                label_info_vector.push_back (label_info_item);

                dv.Out_Edge (EDGE_ID (NODE_ID (bb), NODE_ID (pseudo_node_id)),
                             et_source,
                             NODE_ID (pseudo_node_id));

                pseudo_node_id ++;        // count the number of labels
            }
            else {
                // to dump each edge's label: probability / frequency
                dv.Out_Edge (EDGE_ID (NODE_ID (bb), NODE_ID (succ_bb)),
                             et_destination,
                             NODE_ID (succ_bb));        
                 }
        }
        dv.Node_End ();
    }
  
    // Draw every edge label
    if (show_edge_label)
        for (INT32 i=0; i<pseudo_node_id ; i++)
        {
            // to dump each edge's label: probability / frequency
            sprintf(nlabel,"%#.2f / %#.2f", label_info_vector [i].prob,
                                            label_info_vector [i].freq);
            Is_True(strlen(nlabel) < 64, ("Node Label buf overflowed"));
            dv.Node_Begin (NODE_ID (i), nlabel, nt_pseudo);
            dv.Out_Edge(EDGE_ID(NODE_ID(i),label_info_vector[i].dest_node_id),
                        et_destination,
                        label_info_vector [i].dest_node_id );
            dv.Node_End ();
        }

    dv.Graph_End ();

    Global_CFG_Callback global_cfg_callback(pseudo_node_id);
    dv.Event_Loop( (DaVinci_Callback*) &global_cfg_callback );
}


/* ====================================================================
 *  sPrint_OP (OP *op, char *buf)
 *
 *  Print a OP's detailed information as a string.
 *
 * ====================================================================
 */
extern char * sPrint_TN ( TN *tn, BOOL verbose, char *buf );
static char *
sPrint_OP (OP *op, char *buf)
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
