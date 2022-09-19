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
// Module: fb_info.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/fb_info.cxx,v $
//
// Description:
//
// The file fb_info.cxx defines the string array FB_EDGE_NAMES, and
// implements the procedures FB_EDGE_TYPE_fprintf and
// FB_EDGE_TYPE_sprintf.
//
// ====================================================================
// ====================================================================

#include "fb_info.h"

const char *FB_EDGE_NAMES[]
    = { "------", "INCOMING", "OUTGOING", "ENTRY_OUTGOING",
	"BRANCH_TAKEN", "BRANCH_NOT_TAKEN",
	"LOOP_ZERO", "LOOP_POSITIVE", "LOOP_OUT", "LOOP_BACK",
	"LOOP_EXIT", "LOOP_ITERATE",
	"CIRCUIT_LEFT", "CIRCUIT_RIGHT", "CIRCUIT_NEITHER",
	"CALL_INCOMING", "CALL_OUTGOING", "CALL_INOUTSAME",
	"IO_OUTGOING", "IO_ESCAPE[1]", "IO_ESCAPE[2]", "IO_ESCAPE[3]",
	"SWITCH_DEFAULT" };

void
FB_EDGE_TYPE_fprintf( FILE *fp, const FB_EDGE_TYPE fb_type )\
{
  if ( fb_type < FB_EDGE_SWITCH_BASE ) {
    fprintf( fp, "%s", FB_EDGE_NAMES[fb_type] );
  } else {
    fprintf( fp, "SWITCH[%d]", fb_type - FB_EDGE_SWITCH_BASE );
  }
}

INT
FB_EDGE_TYPE_sprintf( char *buffer, const FB_EDGE_TYPE fb_type )
{
  if ( fb_type < FB_EDGE_SWITCH_BASE ) {
    return sprintf( buffer, "%s", FB_EDGE_NAMES[fb_type] );
  } else {
    return sprintf( buffer, "SWITCH[%d]", fb_type - FB_EDGE_SWITCH_BASE );
  }
}
