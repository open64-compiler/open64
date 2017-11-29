//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_stmt.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_stmt.h,v $
//
// Revision history:
//  27-SEP-94 shin - Original Version
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
// Declare classes STMT_LIST, STMTREP_ITER, and STMTREP_CONST_ITER
// to provide for lists of STMTREP nodes and for interation through
// these lists.
//
// ====================================================================
// ====================================================================


#ifndef opt_stmt_INCLUDED
#define opt_stmt_INCLUDED "opt_stmt.h"
#ifdef _KEEP_RCS_ID
static char *opt_stmtrcs_id = opt_stmt_INCLUDED"$ $Revision$";
#endif /* _KEEP_RCS_ID */


#ifndef defs_INCLUDED
#include "defs.h"
#endif
#ifndef cxx_base_INCLUDED
#include "cxx_base.h"
#endif


class STMT_CONTAINER;
class STMTREP;
class EMITTER;
class DU_MANAGER;


// ====================================================================


class STMT_LIST : public CHAIN {
  DECLARE_CHAIN_CLASS( STMT_LIST, STMTREP )
public:
  void     Print(FILE *fp = stderr) const;         // print out the STMT_LIST
  void     Gen_wn(STMT_CONTAINER *, EMITTER *);    // WN list from STMT_LIST
};


class STMTREP_ITER : public CHAIN_ITER {
  DECLARE_CHAIN_ITER_CLASS( STMTREP_ITER, STMTREP, STMT_LIST )
public:
  STMTREP_ITER(void)  {}
  ~STMTREP_ITER(void) {}
  void Init(void)     {}
  void Set_Cur(STMTREP *s) { CHAIN_ITER::Set_Cur((CHAIN_NODE *) s); }
};


class STMTREP_CONST_ITER : public CHAIN_ITER {
  DECLARE_CHAIN_CONST_ITER_CLASS( STMTREP_CONST_ITER, STMTREP, STMT_LIST )
public:
  STMTREP_CONST_ITER(void)  {}
  ~STMTREP_CONST_ITER(void) {}
  void Init(void)     {}
  void Set_Cur(STMTREP *s) { CHAIN_ITER::Set_Cur((CHAIN_NODE *) s); }
};


// ====================================================================


#endif  // opt_stmt_INCLUDED
