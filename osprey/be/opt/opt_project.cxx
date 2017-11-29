//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_project.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_project.cxx,v $
//
// Revision history:
//  26-MAR-97 rkennedy - Original Version
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
//
// Description:
//
//  Routines for special handling of operations that are "projectable"
//  in the sense of OPR_DIVREM.  See opt_project.h for details.
//
// ====================================================================
// ====================================================================


#include "erglob.h"
#include "opt_project.h"

// Given a CK_VAR coderep, find the RHS of a statement (if any) that
// whose value ultimately gets stored to that variable through a
// sequence of STID's whose LHS's are all EPRE_temp's that does not
// pass through any phi. If no such sequence exists, we return NULL.
STMTREP *
Proj_defstmt(const CODEREP *const var,
	     const OPT_STAB *const opt_stab)
{
  const CODEREP *rhs = var;
  STMTREP *def;

  do {
    // Because of copy-propagation and folding after SSAPRE, we can
    // see variables defined by chi assigned directly to EPRE
    // temporaries. In this case, this version of the EPRE temporary
    // cannot hold the result of a projectable operation.
    if (rhs->Is_flag_set(CF_DEF_BY_CHI) ||
        rhs->Is_flag_set(CF_IS_ZERO_VERSION) )
    {
      return NULL;
    }
    def = rhs->Defstmt();
    if (def != NULL) {
      if (opt_stab->Aux_stab_entry(def->Lhs()->Aux_id())->EPRE_temp()) {
	rhs = def->Rhs();
      }
      else {
	return NULL;
      }
    }
  } while ((def != NULL) &&
	   (rhs->Kind() == CK_VAR));
  if ((def != NULL) &&
      (rhs->Kind() == CK_OP)) {
    return def;
  }
  return NULL;
}
