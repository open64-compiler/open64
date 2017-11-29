//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_dbg.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_dbg.h,v $
//
// Revision history:
//  28-NOV-94 fchow - Original Version
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
//  This file provides things that are helpful inside dbx when
//  debugging WOPT.
//
// ====================================================================
// ====================================================================


#ifndef opt_dbg_INCLUDED
#define opt_dbg_INCLUDED     "opt_dbg.h"
#ifdef _KEEP_RCS_ID
static char *opt_dbgrcs_id = opt_dbg_INCLUDED"$Revision$";
#endif /* _KEEP_RCS_ID */

#ifdef Is_True_On

class BB_NODE;
class CODEREP;
class STMTREP;

extern FILE *DFile;
extern COMP_UNIT *g_comp_unit;
extern OPT_STAB *g_opt_stab;

extern "C" void Dump_cfg(void);
extern "C" void Dump_bbs_reached(void);
extern "C" void Dump_bb(BB_NODE *bb);
extern "C" void Dump_cr(CODEREP *cr);
extern "C" void Dump_sr(STMTREP *sr);
extern "C" void show_cfg(CFG *cfg);

#endif	// Is_True_On
#endif  // opt_main_INCLUDED
