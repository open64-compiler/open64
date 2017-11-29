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
// Module: vt_region.h
// $Date: 2005/12/30 01:47:13 $
// $Author: weitang $
// $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/region/vt_region.h,v $
//
// Description:
//
// Functions for visualizing Graphs about Region using daVinci.
//
//*********************************************************************

#ifndef vt_region_INCLUDED
#define	vt_region_INCLUDED

#include "region.h"
#include "bb.h"

/* ====================================================================
 *  draw_bb_op (BB *bb)
 *
 *  Visualization Flag:     VT_Enable_BB_OP
 *  Visualization Option:   -Wb,-VT:bb_op
 *
 *  Dumping all the OPs in a BB.
 * ====================================================================
 */
extern void draw_bb_op (BB *bb,const char *mes = NULL);

/* ====================================================================
 *  draw_regional_cfg (Region *r)
 *
 *  Visualization Flag:     VT_Enable_Regional_CFG
 *  Visualization Option:   -Wb,-VT:rgnl_cfg
 *
 *  Visualize a Regional Control Flow Graph. The edges in this regional
 *  cfg are not labeled.
 *  
 *  NOTE: To dump the edge's label, use option of -Wb,-VT:cfg_label.
 * ====================================================================
 */
extern void draw_regional_cfg (REGION *r,const char *mes = NULL);

/* ====================================================================
 *  draw_region_tree (Region *r)
 *
 *  Visualization Flag:     VT_Enable_Region_Tree
 *  Visualization Option:   -Wb,-VT:rgn_tree
 *
 *  Visualize a region tree. The region tree is a hierarchical tree that
 *  reflects the composition of a PU. All the children of a parent node
 *  are all the composed part of that parent. And every node can be
 *  divided into its children for details if necessary.
 * ====================================================================
 */
extern void draw_region_tree (REGION *r,const char *mes = NULL);

/* ====================================================================
 *  draw_global_cfg ()
 *
 *  Visualization Flag:     VT_Enable_Global_CFG
 *  Visualization Option:   -Wb,-VT:glbl_cfg
 *
 *  Visualize a global control flow graph. The edge's label is its
 *  probability and frequency.
 *  
 *  NOTE: To dump the edge's label, use option of -Wb,-VT:cfg_label.
 * ====================================================================
 */
extern void draw_global_cfg (const char *mes = NULL);

#endif
