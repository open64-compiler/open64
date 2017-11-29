//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_tail.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_tail.h,v $
//
// Revision history:
//  14-MAR-96 - Original Version
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
// ====================================================================
// ====================================================================


#ifndef opt_tail_INCLUDED
#define opt_tail_INCLUDED "opt_tail.h"


#ifdef _KEEP_RCS_ID
static char *opt_tail_rcs_id = opt_tail_INCLUDED" $Revision$";
#endif /* _KEEP_RCS_ID */


#include "defs.h"
#include "opt_cfg.h"
#include "opt_sym.h"
#include "wn.h"


//  OPT_TAIL: container for tail recursion optimization

class OPT_TAIL {
private:
  OPT_TAIL(void);
  OPT_TAIL(const OPT_TAIL&);
  OPT_TAIL& operator = (const OPT_TAIL&);

  BOOL		_do_trace;
  CFG 		*_cfg;
  OPT_STAB	*_opt_stab;
  BB_NODE	*_entry_bb;
  WN		*_entry_wn;
  WN		*_call_wn;
  WN		*_ret_ldid_wn;
  WN            *_ret_ldid_wn1;
  WN		*_ret_stid_wn;
  WN            *_ret_stid_wn1;
  BB_NODE	*_label_bb;
  WN		*_top_label;

protected:
  BOOL Entry_is_well_behaved();
  BOOL Exit_is_well_behaved(BB_NODE*);
  void Create_top_label();
  void Fixup_exit(BB_NODE*);

public:
  OPT_TAIL(CFG*, OPT_STAB*);
  ~OPT_TAIL();

  void Mutate();
};


#endif  // opt_tail_INCLUDED
