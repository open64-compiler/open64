// ====================================================================
// ====================================================================
//
// Module: wodriver.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/wodriver.cxx,v $
//
// Revision history:
//  08-Sep-94 - Original Version
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
// Main driver -- command line processing and file name manipulation --
// for the Whirl Optimizer
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#include <sys/elf_whirl.h>	    /* for WHIRL_REVISION */

#include "defs.h"
#include "config.h"		/* for Run_wopt, etc.		*/
#include "config_debug.h"	/* for DEBUG_Ir_Version_Check	*/
#include "glob.h"		/* for Irb_File_Name, Ipa_File_Name */
#include "timing.h"		/* for T_Optimize_CU		*/
#include "tracing.h"		/* for Get_Trace, etc.		*/

#include "wn.h"			/* WHIRL Node descriptor	*/
#include "stab.h"		/* WHIRL symtab global variables*/
#include "optimizer.h"		/* External interface for optimizer phase */

#include "opt_defs.h"
#include "opt_config.h"
#include "region_util.h"
#include "region_main.h"	/* for RBI and RAIL prototypes	*/
#include "be_version.h"         /* for Get_BE_Version */

/* The global -WOPT option group flag variables are now defined in: */
#include "config_wopt.h"

#include "wodriver.h"		// So we pick up 'extern "C"'

#ifdef SHARED_BUILD
#if defined(BUILD_OS_DARWIN)
extern char * Ipa_File_Name;
#endif /* defined(BUILD_OS_DARWIN) */
#endif


/* ====================================================================
 *
 * main
 *
 * Main entry point and driver for WOPT.  We no longer need to do
 * processing of -WOPT here -- it is handled in config.c.
 *
 * ====================================================================
 */


/*ARGSUSED*/
void
wopt_main (INT wopt_argc, char **wopt_argv, INT be_argc, char **be_argv)
{
  extern const char *Whirl_Revision;

  if (strcmp (Whirl_Revision, WHIRL_REVISION) != 0)
    FmtAssert (!DEBUG_Ir_Version_Check,
	       ("WHIRL revision mismatch between be.so (%s) and wopt.so (%s)", 
		Whirl_Revision, WHIRL_REVISION));
  
  if (&Get_BE_Version == NULL ||
      strcmp (Get_BE_Version(), BE_VERSION) != 0)
    FmtAssert (FALSE,
	       ("BE version (in be/com/be_version.h) mismatch between be.so (%s) and wopt.so (%s)", 
		&Get_BE_Version != NULL ? Get_BE_Version() : "undef", BE_VERSION));
  /* Construct a skip list from the -WOPT:skip_* options: */
  WOPT_Skip_List = Build_Skiplist ( WOPT_Skip );
  WOPT_Unroll_Skip_List = Build_Skiplist ( WOPT_Unroll_Skip );
  WOPT_ZDL_Skip_List = Build_Skiplist ( WOPT_ZDL_Skip);
} /* wopt_main */


/* Initialization that needs to be done after the global symtab is read */
void
Wopt_Init (void)
{
  Set_Error_Phase ( "Wopt Initialization" );

  return;
} /* Wopt_Init */



/* Per program unit global optimization entry point	*/
/* if region_wn != pu_wn, optimize the region		*/
WN *
Perform_Global_Optimization (WN *pu_wn, WN *region_wn, 
			     struct ALIAS_MANAGER *alias_mgr)
{
    WN *opt_pu;
    struct DU_MANAGER *du_mgr;

    Is_True(region_wn != NULL,
	    ("Perform_Global_Optimization, NULL region found"));
    OPT_POOL_Push ( &MEM_local_pool, MEM_DUMP_FLAG+17 );

    Start_Timer ( T_Wopt_CU );
    Set_Error_Phase ( "Global Optimizer" );

    du_mgr = Create_Du_Manager(MEM_pu_nz_pool_ptr);

    opt_pu = Pre_Optimizer(MAINOPT_PHASE, region_wn, du_mgr, alias_mgr);

    Delete_Du_Manager(du_mgr,MEM_pu_nz_pool_ptr);

    Stop_Timer ( T_Wopt_CU );

    OPT_POOL_Pop ( &MEM_local_pool, MEM_DUMP_FLAG+17 );

    return opt_pu;
} /* Perform_Global_Optimization */


/* Per program unit preopt optimization entry point */
/* called in place of LNO if -PHASE:p is set */
WN *
Perform_Preopt_Optimization(WN *pu_wn, WN *region_wn)
{
    WN *opt_pu = NULL;
    DU_MANAGER *du_mgr;
    ALIAS_MANAGER *alias_mgr;
    
    OPT_POOL_Push ( &MEM_local_pool, MEM_DUMP_FLAG+19 );

    Start_Timer ( T_Preopt_CU );
    Set_Error_Phase ( "Global Optimizer" );

    du_mgr = Create_Du_Manager(MEM_pu_nz_pool_ptr);

    alias_mgr = Create_Alias_Manager(MEM_pu_nz_pool_ptr,pu_wn);
    opt_pu = Pre_Optimizer(PREOPT_PHASE, region_wn, du_mgr, alias_mgr);

    Delete_Du_Manager(du_mgr,MEM_pu_nz_pool_ptr);
    Delete_Alias_Manager(alias_mgr,MEM_pu_nz_pool_ptr);

    Stop_Timer ( T_Preopt_CU );

    OPT_POOL_Pop ( &MEM_local_pool, MEM_DUMP_FLAG+19 );

    return opt_pu;
}


void
Wopt_Fini (void)
{
    return;
} /* Wopt_Fini */
