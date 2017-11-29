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
// Module: vt_dag.h
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_ict/vt_dag.h,v $
//
// Description:
//
// Functions for visualizing dependence graph (DAG) using daVinci.
//
//*********************************************************************

#ifndef vt_dag_INCLUDED
#define	vt_dag_INCLUDED

#include "bb.h"
#include "region.h"

/* ====================================================================
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
 *          from each OP to the last OP, these Branch relatives
 *          dependencies inot necessary to dump. But if necessary to 
 *          dumping, use option of "-Wb,-VT:dag_br".
 * ====================================================================
 */
extern void draw_bb_dependence_graph(BB *bb,const char *mes = NULL);

/* ====================================================================
 *  draw_regional_dependence_graph(REGION *r)
 *
 *  Visualization Flag:     VT_Enable_Regional_DAG
 *  Visualization Option:   -Wb,-VT:rgnl_dag
 *
 *  Visualize a dependence graph (DAG) of a region.
 *
 *  In fact, this graph is more like a regional control flow graph.If a
 *  regional cfg node is a region, just draw a node for a region. If a
 *  regional cfg node is a Non-Empty BB, all OPs in the BB are dumped.
 *  If a regional cfg node is an Empty BB, just draw a node. Edges between
 *  OP in the graph just means the sequence in the OP list. Edges between
 *  BB(REGION)s is derived from regional CFG.
 *
 *  If a OP node is selected, its dependency(ies) is dumped. Re-select it
 *  to remove the denpendency(ies) from the graph.
 * ====================================================================
 */
extern void draw_regional_dependence_graph(REGION *r,const char *mes = NULL);

/* ====================================================================
 *  Print_OP (OP *op, char *buf)
 *
 *  Print a OP's detailed information as a string.
 *
 * ====================================================================
 */
extern char * Print_OP (OP *op, char * buf);

#endif
