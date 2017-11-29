/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


#include <stdint.h>
#include <sys/types.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <ctype.h>
#include "wn.h"
#include "wn_map.h"
#include "wn_util.h"
#include <stdio.h>
#include "wb_util.h" 
#include "cgb_carray.h" 

CGB_CARRAY CGB_carray; 

//-----------------------------------------------------------------------
// NAME: CGB_CARRAY::Enter_This_Pair
// FUNCTION: Enter the node 'ipan' into the saved node array '_nodes',
//   and the vertex 'v' into into the saved vertex array '_vertices'. 
//   Update the value of '_next_index', the index of the next available 
//   entry in '_carray'.  
// NOTE: The 'wn' is not entered into the 'carray' if it is already full.
//-----------------------------------------------------------------------

void CGB_CARRAY::Enter_This_Pair(IPA_NODE* ipan,
				 NODE_INDEX v) 
{
  if (_next_index < CGB_MAX_SAVED_NODES) {
    _nodes[_next_index] = ipan;
    _vertices[_next_index] = v;
  } 
  _next_index++;
}

//-----------------------------------------------------------------------
// NAME: CGB_CARRAY::Enter_This_Node_Unique
// FUNCTION: Same as 'enter_this_node', except the 'ipan' and 'v' are not 
//   entered if it is already there.
//-----------------------------------------------------------------------

INT CGB_CARRAY::Enter_This_Pair_Unique(IPA_NODE* ipan,
				       NODE_INDEX v)
{
  INT i;

  for (i = 0; i < _next_index; i++)
    if (ipan == _nodes[i] && v == _vertices[i])
      break;
  if (i < _next_index)
    return i;
  if (_next_index < CGB_MAX_SAVED_NODES) {
    _nodes[_next_index] = ipan;
    _vertices[_next_index] = v; 
  } 

  _next_index++;
  return _next_index - 1; 
}

//-----------------------------------------------------------------------
// NAME: CGB_CARRAY::Find_This_Pair
// FUNCTION: Return the index of the carray entry which contains the given
//   'ipan' and 'v' entries, if such an entry exists.  Otherwise, return -1.
//-----------------------------------------------------------------------

INT CGB_CARRAY::Find_This_Pair(IPA_NODE* ipan,
			       NODE_INDEX v)
{
  for (INT i = 0; i < _next_index; i++)
    if (ipan == _nodes[i]  && v == _vertices[i])
       return i; 
  return -1; 
} 

//-----------------------------------------------------------------------
// NAME: CGB_CARRAY::List_All_Pairs
// FUNCTION: List all of the pairs of nodes and vertices in the 
//   CGB_CARRAY on the file 'fp'. 
//-----------------------------------------------------------------------

void CGB_CARRAY::List_All_Pairs(FILE* fp)
{
  for (INT i = 0; i < _next_index; i++)
    fprintf(fp, "[%d] %p V#%d\n", i, _nodes[i], _vertices[i]);
} 

