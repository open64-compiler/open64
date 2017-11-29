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

/* ====================================================================
 * ====================================================================
 *
 *  CGIR operation structure (OP) and utility routines which include target
 *  dependencies.
 *
 *  THIS FILE IS ONLY TO BE INCLUDE BY ../op.h!!!!
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef op_targ_INCLUDED
#define op_targ_INCLUDED

#define OP_COPY_OPND 0
#define OP_PREDICATE_OPND 0

/* -----------------------------------------------------------------------
 * See be/cg/op.h for interface
 * -----------------------------------------------------------------------
 */
inline BOOL 
OP_same_res(OP *op)
{
  return TOP_is_same_res(OP_code(op));
}

/* -----------------------------------------------------------------------
 * check if an operation that saves all the predicate registers
 * -----------------------------------------------------------------------
 */
inline BOOL
OP_save_predicates(OP *op)
{
  return FALSE;
}

/* -----------------------------------------------------------------------
 * check if an operation that restores all the predicate registers
 * -----------------------------------------------------------------------
 */
inline BOOL
OP_restore_predicates(OP *op)
{
  return FALSE;
}

#endif /* op_targ_INCLUDED */
