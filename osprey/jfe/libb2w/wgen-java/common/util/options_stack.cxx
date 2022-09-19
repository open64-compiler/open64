/*
 * Copyright 2005 PathScale, Inc.  All Rights Reserved.
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


//-*-c++-*-
//============================================================================
//
// Module: options_stack.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/options_stack.cxx,v $
//
// Revision history:
//  23-SEP-97 dahl - Original Version
//
// Description:
//	Allow the user to set command line options via pragmas in the source.
//
//============================================================================

#include "defs.h"		// INT32
#include "region_util.h"	// debug flags
#include "tracing.h"		// Get_Trace
#include "config.h"		// Opt_Level
#include "phase.h"		// BE_PHASES
#include "driver_util.h"	// Process_Command_Line
#include "cxx_memory.h"		// CXX_NEW_ARRAY
#include "config_wopt.h"	// WOPT_Enable_LFTR2
#include "flags.h"		// Common_Option_Group
#include "options_stack.h"	// options stack

#pragma weak Process_Command_Line

//============================================================================
// Push_Current_Options
// allocates stack slot and then copies global's values into that space
//============================================================================
void
OPTIONS_STACK::Push_Current_Options(void)
{
  // allocate memory_pool, need guess on size
  char *tmp = CXX_NEW_ARRAY(char, OPTIONS_SIZE, &MEM_src_nz_pool);

  // save all options to temp location
  Save_or_restore_options(tmp, OPTIONS_SIZE, TRUE);

  // push current options onto stack
  _options_stack.Push(tmp);

  Is_Trace(Trace(), (TFile,"OPTIONS_STACK::Push_Current_Options, size=%d\n",
		     _options_stack.Elements()));
}

//============================================================================
// Pop_Current_Options
// copies stack contents into global values, deallocates stack slot
//============================================================================
void
OPTIONS_STACK::Pop_Current_Options(void)
{
  // delete old options memory
  char *tmp = _options_stack.Top();
  _options_stack.Pop();
  CXX_DELETE_ARRAY(tmp, &MEM_src_nz_pool);

  // pointer to space where old options are
  tmp = _options_stack.Top();

  // copy options back from stack
  Save_or_restore_options(tmp, OPTIONS_SIZE, FALSE);

  Is_Trace(Trace(), (TFile,"OPTIONS_STACK::Pop_Current_Options, size=%d\n",
		     _options_stack.Elements()));
}

// ====================================================================
// str2argv (private)
// convert options string to argv format, returns argc
// ====================================================================
INT32
OPTIONS_STACK::str2argv(char *str, char ***argv, MEM_POOL *pool)
{
  // first count number of options, add 2 for command and filename
  INT32 argc = 2;
  INT32 i;
  for (i=0; i<strlen(str); i++)
    if (str[i] == '-')
      argc++;

  // allocate array and fill in command and filename
  *argv = CXX_NEW_ARRAY(char *, argc, pool);
  (*argv)[0] = ""; // command
  (*argv)[argc-1] = ""; // filename

  // fill in options
  INT32 pos = 0;
  for (i=1; i<argc-1; i++) {
    // eat up white space
    while (str[pos] == ' ')
      pos++;
    // first find length of option
    INT32 len;
    for (len=0; str[pos+len] != ' ' && str[pos+len] != '\0'; len++)
      ; // empty body
    // allocate
    (*argv)[i] = CXX_NEW_ARRAY(char, len+1, pool); // the 1 is for the '\0'
    // copy and NULL terminate
    strncpy((*argv)[i], &str[pos], len);
    (*argv)[i][len] = '\0';
    // point to beginning of next option
    pos += len + 1; // the 1 is to skip the required space
  }

#ifdef Is_True_On
  if (Trace()) {
    fprintf(TFile, "OPTIONS_STACK::str2argv, argc=%d\n", argc);
    for (i=0; i<argc; i++)
      fprintf(TFile, "  %d: %s\n", i, (*argv)[i]);
  }
#endif

  return argc;
}

// ====================================================================
// Process_Pragma_Options
// process any region or PU level options pragmas
// ====================================================================
void
OPTIONS_STACK::Process_Pragma_Options(char *options_string)
{
  if (options_string == NULL) // no options pragma in this PU or region
    return;
  
  Is_Trace(Trace(),
	   (TFile,"OPTIONS_STACK::Process_Pragma_Options, options = %s\n",
	    options_string));

  // malloc memory and save old options on stack
  Push_Current_Options();

  // set variables based on options_string
  MEM_POOL Options_pool;
  MEM_POOL_Initialize(&Options_pool, "Options_pool", FALSE);
  MEM_POOL_Push(&Options_pool);
  char **argv;
  // create an argv from the options string
  INT32 argc = str2argv(options_string, &argv, &Options_pool);
  // set the various options based on options string
  Process_Command_Line(argc, argv);
  MEM_POOL_Pop(&Options_pool);

#ifdef Is_True_On
  if (Trace()) {
    OPTION_GROUP *og = Get_Command_Line_Group(Common_Option_Groups, "WOPT");
    Is_True(og != NULL, ("OPTIONS_STACK::Process_Pragma_Options, "
			 "could not find WOPT group"));
    Trace_Command_Line_Group(TFile, og); // only prints differences
  }
#endif
}
