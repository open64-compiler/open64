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


// Tag Utilities

#include "defs.h"
#include "symtab.h"
#include "op_map.h"

extern OP_MAP OP_Tag_Map;

extern LABEL_IDX Gen_Tag (void);

inline void Set_OP_Tag (OP *op, LABEL_IDX tag)
{
  Set_OP_has_tag(op);
  // create map on the fly, so no cost if no tags
  if (OP_Tag_Map == NULL)
	OP_Tag_Map = OP_MAP32_Create();
  OP_MAP32_Set (OP_Tag_Map, op, tag);
}

inline LABEL_IDX Get_OP_Tag (const OP *op)
{
  Is_True(OP_has_tag(op), ("OP doesn't have tag"));
  Is_True(OP_Tag_Map != NULL, ("OP_Tag_Map not created"));
  return (LABEL_IDX) OP_MAP32_Get (OP_Tag_Map, op);
}

// call at end of PU
extern void TAG_Finish (void);
