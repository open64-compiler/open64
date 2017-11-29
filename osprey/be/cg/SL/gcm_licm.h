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

//-*-c++-*-

 /* =========================================================================
  * =========================================================================
  * 
  * Module: loop_invar_hoist.h 
  * $Revision: 1.1.1.1 $
  * $Date: 2005/10/21 19:00:00 $ 
  * $Author: marcel $
  * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_ict/loop_invar_hoist.h,v $
  *
  * Revision comments:
  * 
  *    14-April-2003 - Initial version
  * 
  * ref the header of loop_invar_hoist.cxx for more descriptions. 
  *
  * =========================================================================
  * =========================================================================
  */
extern void Perform_Loop_Invariant_Code_Motion( LOOP_DESCR*, MEM_POOL*, BOOL );

#define OP_is_loop(o) (OP_code(o)==TOP_loop)
#define OP_is_mvtc(o) ( (OP_code(o)==TOP_mvtc_i) || (OP_code(o)==TOP_mvtc) )

static BOOL
Is_BB_Empty (BB *bb)
{
  for (OP *op = BB_first_op(bb); op != NULL; op = OP_next(op)) {
    if (OP_Real_Ops(op) != 0) return FALSE;
  }
  return TRUE;
}

static OP* BB_loop_op( BB *bb )
{
  OP *loop = NULL;
  FOR_ALL_BB_OPs_REV( bb, loop ){
    if( OP_is_loop(loop) )
      break;
  }
  return loop;
}


