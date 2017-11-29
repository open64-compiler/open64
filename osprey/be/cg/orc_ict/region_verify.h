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
//=============================================================================
//
// Module: region_verify.h
// Author: lren
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_ict/region_verify.h,v $
//
//=============================================================================
//=============================================================================
//
// Description:
//      verify the attribution of region, including 
//      REGION_ATTRIBUTE and its inherent attibute such as SEME_REGION only has
//      one entry...  
//
//=============================================================================

#ifndef  region_verify_INCLUDED
#define  region_verify_INCLUDED

#include "region.h"
#include "bb.h"
#include <stack>
#include <vector>
#include <list>
#include "defs.h"
#include "cxx_memory.h"
#include "region_bb_util.h"

extern void Verify_Node(REGIONAL_CFG_NODE *node);
extern void Verify_Cfg(REGIONAL_CFG *cfg);
extern void Verify_SEME_Region(REGION *region);
extern void Verify_Region_Tree(REGION_TREE *tree, BB *first_bb);

#endif 
