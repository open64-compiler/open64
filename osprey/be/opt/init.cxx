/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: init.cxx
// $Revision: 1.5 $
// $Date: 04/12/21 14:57:16-08:00 $
// $Author: bos@eng-25.internal.keyresearch.com $
// $Source: /home/bos/bk/kpro64-pending/be/opt/SCCS/s.init.cxx $
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
// This file contains only Linux-specific code and should be entirely
// #ifdef'd out for Irix.  It implements a workaround for the
// "undefined weak symbol" bug in Linux.  See comments in
// be/com/weak.cxx for details.
//
// This file initializes pointer variables to symbols defined in
// wopt.so but referenced in be, be.so, and other dso's.
//
// ====================================================================
// ====================================================================


#if defined(__linux__) || defined(BUILD_OS_DARWIN)

#include "defs.h"
#include "optimizer.h"
#include "opt_alias_interface.h"
#include "opt_wn.h"
#include "opt_du.h"
#include "erglob.h"
#include "opt_htable.h"
#include "wodriver.h"

extern void (*wopt_main_p) (INT argc, char **argv, INT, char **);
extern void (*Wopt_Init_p) ();
extern void (*Wopt_Fini_p) ();
extern WN* (*Perform_Preopt_Optimization_p) (WN *, WN *);
extern WN* (*Perform_Global_Optimization_p) (WN *, WN *, ALIAS_MANAGER *);

// from be/opt/optimizer.h
extern WN* (*Pre_Optimizer_p) (INT32, WN*, DU_MANAGER*, ALIAS_MANAGER*);
extern void (*choose_from_complete_struct_for_relayout_candidates_p)();
extern DU_MANAGER* (*Create_Du_Manager_p) (MEM_POOL *);
extern void (*Delete_Du_Manager_p) (DU_MANAGER *, MEM_POOL *);

// from be/com/opt_alias_interface.h
extern BOOL (*Verify_alias_p) (ALIAS_MANAGER *, WN *);

// from be/opt/opt_alias_analysis.cxx
extern void (*Print_points_to_p) (FILE *fp, POINTS_TO *ptmp);

// from be/opt/opt_wn.h
extern AUX_ID (*WN_aux_p) (const WN*);

// from be/opt/opt_du.h
extern BOOL (DU_MANAGER::*CD_is_br_taken_p) (IDTYPE);
extern BOOL (DU_MANAGER::*CD_is_fall_thru_p) (IDTYPE);
extern BOOL (DU_MANAGER::*Dominate_p) (IDTYPE, IDTYPE);
extern IDTYPE (DU_MANAGER::*Get_cd_p) (IDTYPE);
extern WN* (DU_MANAGER::*Get_last_stmt_p) (IDTYPE);

// from be/opt/opt_htable.h
extern BOOL (CODEREP::*Def_at_entry_p) () const;
extern BB_NODE* (CODEREP::*Defbb_p) () const;

struct WOPT_INIT
{
    WOPT_INIT () {
	wopt_main_p = wopt_main;
	Wopt_Init_p = Wopt_Init;
	Wopt_Fini_p = Wopt_Fini;
	Perform_Preopt_Optimization_p = Perform_Preopt_Optimization;
	Perform_Global_Optimization_p = Perform_Global_Optimization;

	Pre_Optimizer_p = Pre_Optimizer;
 choose_from_complete_struct_for_relayout_candidates_p = choose_from_complete_struct_for_relayout_candidates;
	Create_Du_Manager_p = Create_Du_Manager;
	Delete_Du_Manager_p = Delete_Du_Manager;

	Verify_alias_p = Verify_alias;

	Print_points_to_p = Print_points_to;

	WN_aux_p = WN_aux;

	CD_is_br_taken_p = &DU_MANAGER::CD_is_br_taken;
	CD_is_fall_thru_p = &DU_MANAGER::CD_is_fall_thru;
	Dominate_p = &DU_MANAGER::Dominate;
	Get_cd_p = &DU_MANAGER::Get_cd;
	Get_last_stmt_p = &DU_MANAGER::Get_last_stmt;

	Def_at_entry_p = &CODEREP::Def_at_entry;
	Defbb_p = &CODEREP::Defbb;
    }
} Wopt_Initializer;

#endif // __linux__
