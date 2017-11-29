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
// ====================================================================
// ====================================================================
//
//  Module: cg_grouping.h
//  $Revision: 1.1 $
//  $Date: 2005/07/27 02:13:45 $
//  $Author: kevinlo $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/MIPS/cg_grouping.h,v $
//
//  Synopsis:
//
//      A package for inquiring about grouping of ops, where the aim is
//      to group a given set of ops such that we do not get an issue-split.
//      This interface combines information from the targ_isa_bundle.h
//      interface and the ti_si.h interface. I.e. it combines bundling
//      and resource information into grouping information.
//
//  Interface Description:
//
//	Exported types:
//
//      Exported functions:
//
//      Usage:
//
//         This interface should only be used within the IA-64 target
//         directory, and all defs are therefore in the header file
//         such that we do not have to introduce cg_grouping to other
//         target directories.
//
// ====================================================================
// ====================================================================

#ifndef cg_grouping_INCLUDED
#define cg_grouping_INCLUDED

#include <vector>
#include "mempool_allocator.h"
#include "ti_si.h"
#include "topcode.h"

typedef mempool_allocator<INT32>           INT32_MEMALLOC;
typedef std::vector<INT32, INT32_MEMALLOC> INT32_VECTOR;

class CG_GROUPING
{
private:

public:
}; // class CG_GROUPING
  

#endif // cg_grouping_INCLUDED
