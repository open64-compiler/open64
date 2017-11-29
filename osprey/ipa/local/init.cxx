/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


// This file contains only Linux-specific code and should be entirely
// #ifdef'd out for Irix.

// Work around the "undefined weak symbol" bug in Linux.
//
// see comments in be/com/weak.cxx.
//
// This file define initialization of pointer variables to symbols defined
// in ipl.so but referenced in be/be.so.

#ifdef __linux__

#include "defs.h"
#include "wn.h"
#include "pu_info.h"
#include "ir_bwrite.h"
#include "ipl_driver.h"
#ifdef KEY
#include "ipl_reorder.h"
#endif
#include "wb_browser.h"
#include "loop_info.h"

// from ipa/local/ipl_main.cxx
extern void (*Ipl_Extra_Output_p) (Output_File *);
extern void (*Ipl_Init_p) ();
extern void (*Ipl_Fini_p) ();
extern void (*ipl_main_p) (INT, char **);
extern void (*Perform_Procedure_Summary_Phase_p) (WN*, DU_MANAGER*,
						  ALIAS_MANAGER*, void*);
extern void (*WB_BROWSER_Summary_p) (FILE*, WB_BROWSER*);
extern void (*Print_DO_LOOP_INFO_BASE_p) (FILE*, DO_LOOP_INFO_BASE*);
#ifdef KEY
extern void (*Preprocess_struct_access_p) (void);
#endif

struct IPL_INIT
{
    IPL_INIT () {
	Ipl_Extra_Output_p = Ipl_Extra_Output;
	Ipl_Init_p = Ipl_Init;
	Ipl_Fini_p = Ipl_Fini;
	ipl_main_p = ipl_main;
	Perform_Procedure_Summary_Phase_p = Perform_Procedure_Summary_Phase;
	WB_BROWSER_Summary_p = WB_BROWSER_Summary;
	Print_DO_LOOP_INFO_BASE_p = Print_DO_LOOP_INFO_BASE;
#ifdef KEY	// bug 3672
	Preprocess_struct_access_p = Preprocess_struct_access;
#endif
    }
} Ipl_Initializer;

#endif // __linux__
