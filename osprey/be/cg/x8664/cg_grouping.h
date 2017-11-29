/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


//-*-c++-*-
// ====================================================================
// ====================================================================
//
//  Module: cg_grouping.h
//  $Revision$
//  $Date$
//  $Author$
//  $Source$
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
