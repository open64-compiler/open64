/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

#define USE_STANDARD_TYPES
#include <vector>
#include <list>
#include "defs.h"
#include "errors.h"
#include "tracing.h"
#include "matrix.h"
#include "mempool_allocator.h"
#include "cg.h"
#include "cg_swp.h"
#include "cg_swp_options.h"
#include "cg_swp_target.h"
#include "cgprep.h"
#include "glob.h"    // for Cur_PU_Name
#include "op.h"
#include "cg_loop.h"
#include "cgtarget.h"
#include "ti_si.h"
#include "cg_grouping.h"  // Defines INT32_VECTOR and CG_GROUPING


void 
SWP_Bundle(SWP_OP_vector& op_state, bool trace)
{
} // SWP_Bundle


void 
SWP_Dont_Bundle(SWP_OP_vector& op_state)
{
} // SWP_Dont_Bundle


void
SWP_Undo_Bundle(SWP_OP_vector& op_state, BB *body)
{
}

