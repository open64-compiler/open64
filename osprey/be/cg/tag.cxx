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

#include <alloca.h>
#include "tag.h"
#include "symtab.h"
#include "be_util.h"

OP_MAP OP_Tag_Map;
#ifdef TARG_SL
BB_MAP BB_Tag_Map;
#endif

LABEL_IDX Gen_Tag (void)
{
  enum { maxint_digits = 10 };
  char *name;
  LABEL_IDX labx;
  LABEL &label = New_LABEL(CURRENT_SYMTAB, labx);

  name = (char *)alloca(  strlen(".tag__")
                        + maxint_digits
                        + maxint_digits
                        + 1);
  // use underscores cause intel's asm doesn't like dots in name
  sprintf(name, ".tag_%d_%d", Current_PU_Count(), labx);
  LABEL_Init (label, Save_Str(name), LKIND_TAG);

  return labx;
}

extern void TAG_Finish (void)
{
  if (OP_Tag_Map) {
	OP_MAP_Delete (OP_Tag_Map);
	OP_Tag_Map = NULL;
  }

#ifdef TARG_SL
  if (BB_Tag_Map) {
        BB_MAP_Delete (BB_Tag_Map);
        BB_Tag_Map = NULL;
  }
#endif
}
