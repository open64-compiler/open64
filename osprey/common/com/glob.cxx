/*
 * Copyright (C) 2008 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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


/* ====================================================================
 * ====================================================================
 *
 * Module: glob.c
 *
 * Revision history:
 *  15-Sep-93 - Original Version
 *
 * Description:
 *
 * This file contains miscellaneous global data and utility functions
 * for the compiler which used to be part of each pass driver.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include "defs.h"
#include "errors.h"
#include "glob.h"
#include "tracing.h"

#include "mempool.h"
#if !(defined(SGI_FRONT_END_CPP) && !defined(FFE))
#include "wn.h"
#endif /* if !(defined(SGI_FRONT_END_CPP) && !defined(FFE)) */
#include "stab.h"
#include "const.h"
#include "irbdata.h"

/* The current program unit name -- */
char *Cur_PU_Name = NULL;
char *Orig_PU_Name = NULL;

/* Type	of the current program unit -- set similarly to	Cur_PU_Name */
PU_KIND	Cur_PU_Kind = PU_UNKNOWN;

BOOL Symbol_Table_Out = FALSE;	/* Symbol table output (list or trace) */
BOOL Show_Progress = FALSE;     /* Report progress to stdout */
BOOL Create_Cycle_Output = FALSE;  
BOOL OpenMP_Profiling = FALSE;

/* ====================================================================
 *
 * Front End process info: for consistency checking 
 *
 * The first two are set in the	front end, and examined	in the back end.
 * They	are also put out to the	ipa file.  The third is	only used in the
 * back	end to store the Version number	of the front end.  (In the front
 * end,	this information is stored in "Version".  In the back end,
 * "Version" contains the version number of the	*back* end.  Those
 * variables are initialized in	<machine>/<process>/version.c
 *
 * ====================================================================
 */

INT32 Fe_Process_Id = -1;
INT32 Fe_Process_Time =	-1;
char *Fe_Version = NULL;

/* ====================================================================
 *
 * File names and handles.
 *
 * ====================================================================
 */
/*add by cbq */
int pu_number = 0;
int bb_number = 0;
const char *Output_h_File_Name = "cycle_output.h";        /* Cycle_Counting Output.h file */
FILE *Output_h_File = NULL;           /* Cycle counting output.h file */
FILE *Call_graph_file = NULL; 	      /* Call graph file */
char * pu_string[1000]; 
char * bb_string[1000]; 

/* Current file	names: */
char *Src_File_Name = NULL;	/* Source file */
char *Orig_Src_File_Name = NULL; /* Original source file */
char *Cpp_File_Name = NULL;	/* cpp-preprocessed file */
char *Err_File_Name = NULL;	/* Error file */
char *Lst_File_Name = NULL;	/* Listing file	*/
char *Trc_File_Name = NULL;	/* Trace file */
char *Tlog_File_Name = NULL;	/* Transformation log file */
char *IR_File_Name  = NULL;	/* SGIR	file */
char *Irb_File_Name = NULL;	/* ACIR	intermediate file */
char *Asm_File_Name = NULL;	/* Assembly file */
char *Obj_File_Name = NULL;	/* Relocatable object file */
char *Instrumentation_File_Name = NULL; /* instrumentation file */
char *Feedback_File_Name = NULL; /* Feedback file */
char *call_graph_file_name = NULL; /* Function call graph file */
char *cord_output_file_name = NULL; /* Output file name after function layout */
char *cord_obj_file_name = NULL;  /* Object file name which will be reorder function layout */
#ifndef MONGOOSE_BE
char *Lib_File_Name = NULL;	/* Program library file	*/
#endif 
char *Lib_Lock_Name = NULL;	/* Program library lock	file */
char *DSTdump_File_Name = NULL; /* Dwarf (i.e. DST) dump file */
char *Global_File_Name = NULL;	/* Global symbol table file */
void *Global_PU_Tree = NULL; /* global tree of all the pu's */

char *License_File_Name = NULL ;	/* license file */
char *Whirl2C_File_Name = NULL ;	/* whirl2c output file */

/* Current file	handles	if open, NULL otherwise: */
FILE *Src_File = NULL;		/* Source file */
FILE *Cpp_File = NULL;		/* cpp-preprocessed file */
FILE *Err_File = NULL;		/* Error file */
FILE *Lst_File = NULL;		/* Listing file	*/
FILE *Trc_File = NULL;		/* Trace file */
FILE *Tlog_File = NULL;		/* Transformation log file */
FILE *IR_File  = NULL;		/* SGIR	file */
FILE *Irb_File = NULL;		/* ACIR	intermediate file */
FILE *Asm_File = NULL;		/* Assembly file */
FILE *Obj_File = NULL;		/* Relocatable object file */
FILE *Lib_File = NULL;		/* Program library file	*/
FILE *Tim_File = NULL;		/* Timer report	file, usually TFile */

#ifdef SPECMT_LT
FILE *ExchangeFile = NULL;              /* exchange file for pseudo-specmt partition */
FILE *LoopMappingFile = NULL;           /* loop mapping file for two pass compilation */
#endif

