//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: profile_interface.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/instrumentation/libinstr/profile_interface.cxx,v $
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
// During instrumentation, calls to the following procedures are
// inserted into the WHIRL code.  When invoked, these procedures
// initialize, perform, and finalize frequency counts.
//
// ====================================================================
// ====================================================================


#include <stdio.h>
#include <vector>

using std::vector;

#include "profile.h"
#include "profile_interface.h"
#include "profile_errors.h"
#include "dump.h"


// ====================================================================


static char *output_filename = NULL;
static BOOL unique_output_filename = FALSE; 


// ====================================================================


// One time initialization

void __profile_init(char *fname, int phase_num, BOOL unique_name)
{

  PROFILE_PHASE curr_phase_num;

  void (*pf)() = __profile_finish;
  static bool first_call_to_profile_init = true;
  if (first_call_to_profile_init) {

     atexit(pf);

     output_filename = new char[strlen(fname) + 7 + 1];
     strcpy(output_filename, fname); 
     if (unique_name)
	 strcat (output_filename, ".XXXXXX");
     unique_output_filename = unique_name;
     first_call_to_profile_init = false;
  }

  curr_phase_num = Instrumentation_Phase_Num();

  if (curr_phase_num == PROFILE_PHASE_NONE) {
     Set_Instrumentation_Phase_Num((PROFILE_PHASE) phase_num);
  } else if(curr_phase_num != (PROFILE_PHASE) phase_num) {
    profile_warn("Phase Number already set to a different value in: %s",
		 output_filename);
  }
}


// PU level initialization to gather profile information for the PU.
// We call atexit during the first call to this routine to ensure
// that at exit we remember to destroy the data structures and dump
// profile information.
// Also, during the first call, a profile handle for the PU is created.
// During subsequent calls, teh PC address of the PU is used to access
// a hash table and return the profile handle that was created during the 
// first call.

void *
__profile_pu_init(char *file_name, char* pu_name, long current_pc,
		  INT32 pusize, INT32 checksum)
{
  PU_PROFILE_HANDLE pu_handle
    = Get_PU_Handle(file_name, pu_name, current_pc, pusize, checksum);
  pu_handle->pu_size = pusize;
  pu_handle->runtime_fun_address = current_pc;
  return (void *) pu_handle;
}


// For a PU, initialize the data structures that maintain 
// invokation profile information.

void
__profile_invoke_init(void *pu_handle, INT32 num_invokes)
{
  Profile_Invoke_Init((PU_PROFILE_HANDLE) pu_handle, num_invokes);
}


// Gather profile information for a conditional invoke

void
__profile_invoke(void *pu_handle, INT32 invoke_id)
{
  Profile_Invoke((PU_PROFILE_HANDLE) pu_handle, invoke_id);
}


// For a PU, initialize the data structures that maintain 
// conditional branch profile information.

void
__profile_branch_init(void *pu_handle, INT32 num_branches)
{
  Profile_Branch_Init((PU_PROFILE_HANDLE) pu_handle, num_branches);
}


// Gather profile information for a conditional branch

void
__profile_branch(void *pu_handle, INT32 branch_id, bool taken)
{
  Profile_Branch((PU_PROFILE_HANDLE) pu_handle, branch_id, taken);
}


// For a PU, initialize the data structures that maintain 
// switch profile information.

void
__profile_switch_init(void *pu_handle,
		      INT32 num_switches,    INT32 *switch_num_targets,
		      INT32 num_case_values, INT64 *case_values)
{
  Profile_Switch_Init((PU_PROFILE_HANDLE) pu_handle,
		      num_switches,    switch_num_targets,
		      num_case_values, case_values);
}


// Gather profile information for an Switch

void
__profile_switch(void *pu_handle, INT32 switch_id, INT32 target,
		   INT32 num_targets)
{
  Profile_Switch((PU_PROFILE_HANDLE) pu_handle, switch_id, target,
		   num_targets);
}


// For a PU, initialize the data structures that maintain 
// compgoto profile information.

void
__profile_compgoto_init(void *pu_handle, INT32 num_compgotos,
			INT32 *compgoto_num_targets)
{
  Profile_Compgoto_Init((PU_PROFILE_HANDLE) pu_handle, num_compgotos,
			compgoto_num_targets);
}


// Gather profile information for an Compgoto

void
__profile_compgoto(void *pu_handle, INT32 compgoto_id, INT32 target,
		   INT32 num_targets)
{
  Profile_Compgoto((PU_PROFILE_HANDLE) pu_handle, compgoto_id, target,
		   num_targets);
}

#ifdef KEY
// For a PU, initialize the data structures that maintain 
// value profile information.

void __profile_value_init( void *pu_handle, INT32 num_values )
{
  Profile_Value_Init( (PU_PROFILE_HANDLE) pu_handle, num_values );
}


// Gather profile information for a Value

void
__profile_value( void *pu_handle, INT32 inst_id, INT64 value )
{
  Profile_Value( (PU_PROFILE_HANDLE) pu_handle, inst_id, value );
}

// For a PU, initialize the data structures that maintain value (FP) profile 
// information for both the operands of a binary operator.

void __profile_value_fp_bin_init( void *pu_handle, INT32 num_values )
{
  Profile_Value_FP_Bin_Init( (PU_PROFILE_HANDLE) pu_handle, num_values );
}


// Gather profile information for Values (FP)

void
__profile_value_fp_bin( void *pu_handle, INT32 inst_id, 
			double value_fp_0, double value_fp_1 )
{
  Profile_Value_FP_Bin( (PU_PROFILE_HANDLE) pu_handle, inst_id, 
			value_fp_0, value_fp_1 );
}
#endif


// For a PU, initialize the data structures that maintain 
// loop profile information.

void 
__profile_loop_init(void *pu_handle, INT32 num_loops)
{
  Profile_Loop_Init((PU_PROFILE_HANDLE) pu_handle, num_loops);
}


// Gather profile information for a loop entry

void
__profile_loop(void *pu_handle, INT32 loop_id)
{
  Profile_Loop((PU_PROFILE_HANDLE) pu_handle, loop_id);
}


// Gather profile information from a Loop Iteration

void
__profile_loop_iter(void *pu_handle, INT32 loop_id) 
{
  Profile_Loop_Iter((PU_PROFILE_HANDLE) pu_handle, loop_id);  
}


// For a PU, initialize the data structures that maintain
// short circuit profile information.

void 
__profile_short_circuit_init(void *pu_handle, INT32 num_short_circuit_ops)
{
  Profile_Short_Circuit_Init((PU_PROFILE_HANDLE) pu_handle,
			     num_short_circuit_ops);
}


// Gather profile information for the right operand of a short circuit op

void 
__profile_short_circuit(void *pu_handle, INT32 short_circuit_id, bool taken)
{
  Profile_Short_Circuit((PU_PROFILE_HANDLE) pu_handle,
			short_circuit_id, taken);
}


// For a PU, initialize the data structures that maintain
// call profiles.

void 
__profile_call_init(void *pu_handle, int num_calls)
{
  Profile_Call_Init((PU_PROFILE_HANDLE) pu_handle, num_calls);
}

// For a PU, initialize the data structures that maintain
// icall profiles.

void 
__profile_icall_init(void *pu_handle, int num_icalls)
{
  Profile_Icall_Init((PU_PROFILE_HANDLE) pu_handle, num_icalls);
}

// Gather the entry count for this call id

void 
__profile_call_entry(void *pu_handle, int call_id)
{
  Profile_Call_Entry((PU_PROFILE_HANDLE) pu_handle, call_id);
}


// Gather the exit count for this call id

void 
__profile_call_exit(void *pu_handle, int call_id)
{
  Profile_Call_Exit((PU_PROFILE_HANDLE) pu_handle, call_id);
}

void
__profile_icall(void * pu_handle, int icall_id, void * called_fun_address)
{
  Profile_Icall((PU_PROFILE_HANDLE) pu_handle, icall_id, called_fun_address);
}

// At exit processing to destroy data structures and dump profile
// information.

void __profile_finish(void)
{
  FILE *fp;
  HASH_MAP::iterator i;

  if (unique_output_filename) {
      int file_id = mkstemp (output_filename);
      fp = fdopen (file_id, "w+");
  } else
      fp = fopen (output_filename, "w+");

  if (fp == NULL) {
     profile_error("Unable to open file: %s", output_filename);
  } 

  Dump_all(fp, output_filename);
  

  fclose(fp);

  delete [] output_filename;
}
