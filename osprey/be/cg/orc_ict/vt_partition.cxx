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
// Module: vt_partition.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_ict/vt_partition.cxx,v $
//
// Description:
//
// Functions for visualizing Graphs about Partition using daVinci.
//
//*********************************************************************

#include "prdb.h"
#include "DaVinci.h"
#include "glob.h"
#include "mempool.h"

#define     COLORPOOL_NUM   8

/* ====================================================================
 *
 *  Class Name:     COLORPOOL
 *
 *  Base Class:     <none>
 *
 *  Derived Class:  <none>
 *
 *  Class Description:    
 *
 *      Get a color from the color pool.
 *
 *  Note: <none> 
 *
 * ====================================================================
 */
class COLORPOOL{
private:
    char        *_color_array[COLORPOOL_NUM];
    INT32       _index;

public:
    COLORPOOL()    {
        _index = 0;
        _color_array[0]= "black";
        _color_array[1]= "palegreen";
        _color_array[2]= "yellow";
        _color_array[3]= "red";
        _color_array[4]= "blue";
        _color_array[5]= "orange";
        _color_array[6]= "pink";
        _color_array[7]= "gray";
    }
    ~COLORPOOL()   {}

    const char* GetColor(){
        return _color_array[ (_index ++) % COLORPOOL_NUM ];
    }
};

/* ====================================================================
 *  Class Name:     PARTITION_GRAPH_NODE_SET
 *
 *  Base Class:     <none>
 *
 *  Derived Class:  <none>
 *
 *  Class Description:    
 *
 *      For tranversing the partition graph.
 *
 *  Note: <none> 
 *
 * ====================================================================
 */

class PARTITION_GRAPH_NODE_SET
{
private:
    PG_CONTAINER    _pnodes;
    INT32           _index;
public:
    PARTITION_GRAPH_NODE_SET() {
        _pnodes.clear();
        _index=0;

    }
    ~PARTITION_GRAPH_NODE_SET() {}

    BOOL Is_In(PARTITION_GRAPH_NODE *pnode);
    BOOL Empty();
    void Push_Back(PARTITION_GRAPH_NODE *pnode);
    PARTITION_GRAPH_NODE* Pop_Front();
};

BOOL
PARTITION_GRAPH_NODE_SET::Is_In(PARTITION_GRAPH_NODE *pnode)
{
    for (INT32 i=0; i< _pnodes.size() ; i++)
        if ( pnode == _pnodes[i]) return TRUE;
    return FALSE;
}

BOOL
PARTITION_GRAPH_NODE_SET::Empty()
{
    return ( _index >= _pnodes.size() );
}

void
PARTITION_GRAPH_NODE_SET::Push_Back(PARTITION_GRAPH_NODE *pnode)
{
    _pnodes.push_back(pnode);
}

PARTITION_GRAPH_NODE*
PARTITION_GRAPH_NODE_SET::Pop_Front()
{
    PARTITION_GRAPH_NODE*   pnode = _pnodes[_index];
    _index ++;
    return pnode;
}

/* ====================================================================
 *  draw_partition_graph (PARTITION_GRAPH *pg)
 *
 *  Visualization Flag:     VT_Enable_Global_CFG
 *  Visualization Option:   -Wb,-VT:glbl_cfg
 *
 *  Visualize a partition grapah.
 * ====================================================================
 */
void draw_partition_graph (PARTITION_GRAPH *pg, const char *mes = NULL)
{
    if (! DaVinci::enabled(TRUE)) return;

    MEM_POOL dv_pool; 
    dv_pool.magic_num = 0;
    MEM_POOL_Constructor pool( &dv_pool, "DaVinci", FALSE);
  
    DaVinci dv (&dv_pool, NULL);

    char window_title[128];         // set the window's label
    if ( NULL == mes)
        sprintf(window_title,"Partition Graph - %s", Cur_PU_Name);
    else
        sprintf(window_title,"Partition Graph - %s - %s ",Cur_PU_Name,mes);
    Is_True(strlen(window_title) < 128, ("Window Title buf overflowed"));
    dv.Title(window_title);

    //Now we start drawing
    char            nlabel[64];     // temp space for node label
    NODE_TYPE       nt, nt_plain, nt_root, nt_dummy;
                                    // to store the current OP type
    EDGE_TYPE       et;             // to store the current edge type
    COLORPOOL       edge_colorpool;
    INT32           edge_id_number  =0; // used as edge ID

    BOOL                            dummy_in_graph = FALSE;
    PARTITION_GRAPH_NODE            *being_drawn_pnode, *child_pnode;
    PARTITION_GRAPH_NODE_SET        drawn_pnodes;   // for traversing

    nt_plain.Color("white");        // color of ordinary node is white
    nt_root.Color ("pink");         // color of root node is pink
    nt_dummy.Color("gray");         // color of dummy node is gray

    drawn_pnodes.Push_Back( pg -> Root() );
    dv.Graph_Begin();
    
    // breadth traversing using a queue
    while ( ! drawn_pnodes.Empty() )
    {
        being_drawn_pnode = drawn_pnodes.Pop_Front();

        if (pg -> Dummy() == being_drawn_pnode) {
            nt = nt_dummy;
            dummy_in_graph = TRUE;
        }
        else if (pg -> Root() == being_drawn_pnode) nt = nt_root;
        else nt = nt_plain;
        sprintf( nlabel, "%d", being_drawn_pnode -> Index() );
        Is_True(strlen(nlabel) < 64, ("Node Label buf overflowed"));
        dv.Node_Begin (NODE_ID (being_drawn_pnode), nlabel, nt);

        // get the child partitions of this node
        PT_CONTAINER &parent_partition=being_drawn_pnode->Parent_Partitions();
        for (INT32 i=0; i<parent_partition.size() ; i++ )
        {
            et.Color ( edge_colorpool.GetColor() );
            PG_CONTAINER &child_pnodes = parent_partition[i] -> Child();
            for (INT32 j=0; j<child_pnodes.size() ; j++ )
            {
                child_pnode = child_pnodes[j];

                // if the node has not been drawn, push it in the queue
                if (! drawn_pnodes.Is_In(child_pnode))
                    drawn_pnodes.Push_Back(child_pnode);

                dv.Out_Edge ( EDGE_ID ( NODE_ID((void*)(INTPTR)edge_id_number++),
                                        NODE_ID((void*)(INTPTR)edge_id_number++) ),
                              et, NODE_ID (child_pnode));
            }
        }
        dv.Node_End();
    }

    // if dummy node is not in the partition graph, it should be drawn
    // particularly.
    if (! dummy_in_graph)
    {
        being_drawn_pnode = pg -> Dummy();
        sprintf( nlabel, "%d", being_drawn_pnode -> Index() );
        dv.Node_Begin (NODE_ID (being_drawn_pnode), nlabel, nt_dummy);
        dv.Node_End();
    }

    dv.Graph_End();
    dv.Event_Loop(NULL);
}


