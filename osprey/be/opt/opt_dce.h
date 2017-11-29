//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_dce.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_dce.h,v $
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
// ====================================================================


#ifndef opt_dce_INCLUDED
#define opt_dce_INCLUDED "opt_dce.h"

// ====================================================================
// Used to deal with redundant conditions
// ====================================================================

enum COND_EVAL {
  EVAL_UNINIT = 0,		// uninitialized
  EVAL_TRUE,		// condition evals to true
  EVAL_FALSE,		// condition evals to false
  EVAL_UNKNOWN,		// condition is unknown, but is init
  EVAL_DEAD		// condition is both true and false (used by
};			//   Eval_redundant_cond_br to indicate dead code)

#ifdef DEBUGGING_COND_EVAL
static 
const char *Cond_eval_name( COND_EVAL eval )
{
  switch ( eval ) {
    case EVAL_UNINIT : return "UNINIT";
    case EVAL_TRUE   : return "TRUE";
    case EVAL_FALSE  : return "FALSE";
    case EVAL_UNKNOWN: return "UNKNOWN";
    case EVAL_DEAD   : return "DEAD";
    default          : return "";
  }
}
#endif // DEBUGGING COND_EVAL

extern COND_EVAL Eval_redundant_cond_br( CODEREP *orig, CODEREP *evalcond, COND_EVAL eval );

#endif
