//-*-c++-*-

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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
#include <stdlib.h>
#include "hash_map.h"
#include "vector.h"
#include "utils.h"


#include "profile.h"

namespace Instr {

// Hash Table that can be accessed with the PC address of a PU
// and returns a pointer to the Profile handle for that PU.

HASH_MAP PU_Profile_Handle_Table;

static PROFILE_PHASE instrumentation_phase_num = PROFILE_PHASE_NONE;

// ====================================================================

// Given the PC address of a PU, index into a hash table and 
// retrieve the profile handle for the PU. If the handle is
// NULL, create it.

PU_PROFILE_HANDLE
Get_PU_Handle(char *file_name, char* pu_name, long current_pc, INT32 pu_size, INT32 checksum)
{
  PU_PROFILE_HANDLE & pu_handle = PU_Profile_Handle_Table[current_pc];

  if (pu_handle == NULL) {
     
     pu_handle = new PU_Profile_Handle(file_name, pu_name, current_pc, pu_size, checksum); 

  }

  return pu_handle;
}

// Given a PU handle, set the file name in which the PU appears

void
PU_Profile_Handle::Set_file_name(char *s)
{
  this->file_name = (char *) malloc (sizeof(char) * (strlen(s) + 1));
  strcpy(this->file_name, s);
}

// Given a PU handle, set the name of the PU

void
PU_Profile_Handle::Set_pu_name( char *s)
{
  this->pu_name = (char *) malloc (sizeof(char) * (strlen(s) + 1));
  strcpy(this->pu_name, s);
}

// ====================================================================

// For each PU, we want to a one-time initialization of the
// tables that maintain profile information of invoke nodes.
// Given a pu_handle and the number of invoke nodes in that PU, we
// initialize the Call_Profile_Table in the pu handle with the appropriate
// number of entries. This routine may be invoked multiple times from
// a PU, but the initialization is done only the first time.

void 
Profile_Invoke_Init(PU_PROFILE_HANDLE pu_handle, INT32 num_invokes)
{
  Invoke_Profile_Vector& Inv_Table = pu_handle->Get_Invoke_Table();

  if (Inv_Table.empty()) {
     Inv_Table.resize(num_invokes);
  }
}

// Update entry count for a invoke

void 
Profile_Invoke(PU_PROFILE_HANDLE pu_handle, INT32 invoke_id)
{
  Invoke_Profile_Vector& Inv_Table = pu_handle->Get_Invoke_Table();

  Inv_Table[invoke_id].invoke_count++;
}

// ====================================================================

// For each PU, we want to a one-time initialization of the 
// tables that maintain profile information of branch nodes. 
// Given a pu_handle and the number of conditional branches
// in that PU, we initialize the Br_Table in the pu handle
// with the appropriate number of entries. 
// This routine may be invoked multiple times from a PU, but the
// initialization is done only the first time.

void
Profile_Branch_Init(PU_PROFILE_HANDLE pu_handle, INT32 num_branches)
{
  Branch_Profile_Vector& Br_Table = pu_handle->Get_Branch_Table();

  if (Br_Table.empty()) {
     Br_Table.resize(num_branches);
  }
}

// Update appropriate profile information for a branch.

void
Profile_Branch(PU_PROFILE_HANDLE pu_handle, INT32 branch_id, bool taken)
{
  if (taken)
       Incr_Branch_Taken(pu_handle, branch_id);
  else 
       Incr_Branch_Not_Taken(pu_handle, branch_id);
}

// Given a PU handle, retrieve the branch table and increment
// taken count for id.

void Incr_Branch_Taken(PU_PROFILE_HANDLE pu_handle, INT32 id) 
{
  Branch_Profile_Vector& Br_Table = pu_handle->Get_Branch_Table();
  Br_Table[id].taken++;
}

// Given a PU handle, retrieve the branch table and increment
// not-taken count for id.

void Incr_Branch_Not_Taken(PU_PROFILE_HANDLE pu_handle, INT32 id) 
{
  Branch_Profile_Vector& Br_Table = pu_handle->Get_Branch_Table();
  Br_Table[id].not_taken++;
}

// ====================================================================

// For each PU, we want a one-time initialization of the 
// tables that maintain profile information of Switch nodes. 
// Given a pu_handle, the number of Switches in that PU and an
// array that reprents the number of targets for each Switch in the PU,
// we initialize the Switch_Table in the pu handle with the appropriate 
// number of entries. This routine may be invoked multiple times from a 
// PU, but the initialization is done only the first time.

// switch_num_targets[i] gives the number of targets for the ith Switch
// in the PU.

void
Profile_Switch_Init(PU_PROFILE_HANDLE pu_handle,
		    INT32 num_switches, INT32 *switch_num_targets,
		    INT32 num_case_values, FB_NUM_TYPE *case_values)
{
  Switch_Profile_Vector& Switch_Table = pu_handle->Get_Switch_Table();

  if (Switch_Table.empty()) {

    Switch_Table.resize(num_switches);

    for (Switch_Profile_Vector::iterator first (Switch_Table.begin ());
	 first != Switch_Table.end (); ++first) {

	Switch_Profile& sp = *first;

	INT32 num_targets = *switch_num_targets;

	sp.targets_profile.resize (num_targets + 1);

	sp.targets_case_value.reserve (num_targets);
	sp.targets_case_value.insert (sp.targets_case_value.begin (),
				      case_values,
				      case_values + num_targets);
	case_values += num_targets;

	++switch_num_targets;
    }
    // Should now be true:  case_index == num_case_values
  }
}

// Update appropriate profile information for an Switch
// Given a PU handle, rtrieve the Switch table; use this table 
// and the switch_id to retrieve the vector representing the
// possible targets for this Switch; target takes a 
// value between 0 and n-1 where n is the number of targets
// for the Switch. Increment the profile information for 
// the appropriate target represented by 'target'.

void
Profile_Switch(PU_PROFILE_HANDLE pu_handle, INT32 switch_id, INT32 case_value, 
	       INT32 num_targets)
{
  Switch_Profile_Vector& switch_table = pu_handle->Get_Switch_Table();
  Switch_Profile& switch_entry  = switch_table[switch_id];
  Switch_Profile::value_type& targets_profile =
      switch_entry.Get_Targets_Profile();
  Switch_Profile::value_type& targets_case_value =
      switch_entry.Get_Targets_Case_Value();

  // Which branch corresponds to case_value?
  INT32 t, target = -1;  // default branch
  for (t = 0; t < num_targets; t++) {
    if (targets_case_value[t] == case_value) {
      target = t;
    }
  }

  targets_profile[target + 1]++;  // 0 is default
}

// ====================================================================

// For each PU, we want to a one-time initialization of the 
// tables that maintain profile information of Compgoto nodes. 
// Given a pu_handle, the number of Compgotos in that PU and an
// array that reprents the number of targets for each Compgoto in the PU,
// we initialize the Compgoto_Table in the pu handle with the appropriate 
// number of entries. This routine may be invoked multiple times from a 
// PU, but the initialization is done only the first time.

// compgoto_num_targets[i] gives the number of targets for the ith Compgoto
// in the PU.

void
Profile_Compgoto_Init(PU_PROFILE_HANDLE pu_handle, INT32 num_compgotos,
		      INT32 *compgoto_num_targets)
{
  Compgoto_Profile_Vector& Compgoto_Table = pu_handle->Get_Compgoto_Table();

  if (Compgoto_Table.empty()) {

     Compgoto_Table.resize(num_compgotos);

     for (INT32 i = 0; i < num_compgotos; i++) {

	 INT32 num_targets = *compgoto_num_targets + 1;

	 Compgoto_Table[i].Get_Targets_Profile ().resize(num_targets);

	 ++compgoto_num_targets;
     }
  }
}

// Update appropriate profile information for an Compgoto
// Given a PU handle, rtrieve the Compgoto table; use this table 
// and the compgoto_id to retrieve the vector representing the
// possible targets for this Compgoto; target takes a 
// value between 0 and n-1 where n is the number of targets
// for the Compgoto. Increment the profile information for 
// the appropriate target represented by 'target'.

void
Profile_Compgoto(PU_PROFILE_HANDLE pu_handle, INT32 compgoto_id, INT32 target, 
		 INT32 num_targets)
{
 if (target < 0 || target >= num_targets)
    target = -1;

 Compgoto_Profile_Vector& compgoto_table = pu_handle->Get_Compgoto_Table();

 Compgoto_Profile& cgoto = compgoto_table[compgoto_id];

 ++(cgoto.Get_Targets_Profile ()[target + 1]);  // 0 is default
}

// ====================================================================

// For each PU, we want to a one-time initialization of the 
// tables that maintain profile information of loop nodes. 
// Given a pu_handle and the number of loops in that PU, we 
// initialize the Loop_Table in the pu handle with the appropriate 
// number of entries. This routine may be invoked multiple times from 
// a PU, but the initialization is done only the first time.

void
Profile_Loop_Init(PU_PROFILE_HANDLE pu_handle, INT32 num_loops)
{

  Loop_Profile_Vector& Loop_Table = pu_handle->Get_Loop_Table();

  if (Loop_Table.empty()) {
     Loop_Table.resize(num_loops);
  }
}

// Update appropriate profile information at a loop entry.

void
Profile_Loop(PU_PROFILE_HANDLE pu_handle, INT32 loop_id)
{
  Loop_Profile_Vector& Loop_Table = pu_handle->Get_Loop_Table();
  Loop_Profile& loop_info = Loop_Table[loop_id];

  if (loop_info.invocation_count == 1) {
     loop_info.min_trip_count = loop_info.last_trip_count;
     loop_info.max_trip_count = loop_info.last_trip_count;
  } else if (loop_info.invocation_count != 0) {
     loop_info.min_trip_count = min(loop_info.min_trip_count,
				    loop_info.last_trip_count);
     loop_info.max_trip_count = max(loop_info.max_trip_count,
				    loop_info.last_trip_count);
  }

  // Count num_zero_trips -- NOTE: The code does not check whether or not
  // the very last trip through the loop is a zero trip.  Instead, code
  // in procedure Convert_Loop_Profile of the file dump.cxx handles that
  // responsibility.
  if (loop_info.invocation_count > 0 && loop_info.last_trip_count == 0) {
     loop_info.num_zero_trips++;
  }

  loop_info.invocation_count++;
  loop_info.last_trip_count = 0;
}

// Update appropriate profile information at a loop iteration.

void
Profile_Loop_Iter(PU_PROFILE_HANDLE pu_handle, INT32 loop_id)
{
  Loop_Profile_Vector& Loop_Table = pu_handle->Get_Loop_Table();
  Loop_Profile& loop_info = Loop_Table[loop_id];

  loop_info.last_trip_count++;
  loop_info.total_trip_count++;
}

// ====================================================================

// For each PU, we want to a one-time initialization of the 
// tables that maintain profile information of CAND/COR nodes. 
// Given a pu_handle and the number of CAND/COR in that PU, we 
// initialize the Short_Circuit_Table in the pu handle with the appropriate 
// number of entries. This routine may be invoked multiple times from 
// a PU, but the initialization is done only the first time.

void
Profile_Short_Circuit_Init(PU_PROFILE_HANDLE pu_handle,
			   INT32 num_short_circuit_ops)
{
  Short_Circuit_Profile_Vector& Short_Circuit_Table
    = pu_handle->Get_Short_Circuit_Table();

  if (Short_Circuit_Table.empty()) {
     Short_Circuit_Table.resize(num_short_circuit_ops);
  }
}

// Update appropriate profile information for right operand of a CAND/COR

void
Profile_Short_Circuit(PU_PROFILE_HANDLE pu_handle, INT32 short_circuit_id,
		      bool taken)
{
  if (taken) 
       Incr_Right_Taken(pu_handle, short_circuit_id);
  else 
       Incr_Neither_Taken(pu_handle, short_circuit_id);
}

// Given a PU handle, retrieve the CAND/COR table and increment
// right_taken_count for id.

void Incr_Right_Taken(PU_PROFILE_HANDLE pu_handle, INT32 id)
{
  Short_Circuit_Profile_Vector& Short_Circuit_Table
    = pu_handle->Get_Short_Circuit_Table();
  Short_Circuit_Table[id].right_taken_count++;
}

// Given a PU handle, retrieve the CAND/COR table and increment
// neither_taken_count for id.

void Incr_Neither_Taken(PU_PROFILE_HANDLE pu_handle, INT32 id)
{
  Short_Circuit_Profile_Vector& Short_Circuit_Table
    = pu_handle->Get_Short_Circuit_Table();
  Short_Circuit_Table[id].neither_taken_count++;
}

// ====================================================================

// For each PU, we want to a one-time initialization of the
// tables that maintain profile information of CALL nodes.
// Given a pu_handle and the number of CALL nodes in that PU, we
// initialize the Call_Profile_Table in the pu handle with the appropriate
// number of entries. This routine may be invoked multiple times from
// a PU, but the initialization is done only the first time.

void 
Profile_Call_Init(PU_PROFILE_HANDLE pu_handle, INT32 num_calls)
{
  Call_Profile_Vector& Call_Table = pu_handle->Get_Call_Table();

  if (Call_Table.empty()) {
     Call_Table.resize(num_calls);
  }
}

void 
Profile_Icall_Init(PU_PROFILE_HANDLE pu_handle, INT32 num_icalls)
{
  Icall_Profile_Vector& Icall_Table = pu_handle->Get_Icall_Table();

  if (Icall_Table.empty()) {
     Icall_Table.resize(num_icalls);
	 for (int i=0;i<num_icalls;i++)
	 {
		 memset(&(Icall_Table[i].fb_tnv),0,sizeof(FB_TNV));
	 }
  }
}

// Update entry count for a call

void 
Profile_Call_Entry(PU_PROFILE_HANDLE pu_handle, INT32 call_id)
{
  Call_Profile_Vector& Call_Table = pu_handle->Get_Call_Table();

  Call_Table[call_id].entry_count++;
}

// Update exit count for a call

void 
Profile_Call_Exit(PU_PROFILE_HANDLE pu_handle, INT32 call_id)
{
  Call_Profile_Vector& Call_Table = pu_handle->Get_Call_Table();

  Call_Table[call_id].exit_count++;
}

void 
Profile_Icall(PU_PROFILE_HANDLE pu_handle, INT32 icall_id, void * called_fun_address)
{
  Icall_Profile_Vector& Icall_Table = pu_handle->Get_Icall_Table();

  FB_TNV * ptnv = &(Icall_Table[icall_id].fb_tnv);
  ptnv->_id = icall_id; //actually this is no use.
  ptnv->_exec_counter++; //execution counter.
  ptnv->_flag = 0;
  ptnv->_clear_counter++;

  FB_VALUE_TYPE value = (UINTPS)called_fun_address;
 
       //now the tnv table info update.
       //We use the first 6 items as "steady part", the last 4 items as "clear part".
       // clear_interval is the sum of _exec_counter of the middle two in the steady part.
        INT i, j;
        FB_VALUE_TYPE clear_interval = ptnv->_counters[3] + ptnv->_counters[4]; 
        if (ptnv->_clear_counter >= clear_interval)
        {
        	ptnv->_clear_counter = 0;
        	//resort tnv
        	FB_VALUE_TYPE tmpvalues[10], tmpcounters[10];
        	for (i=0; i<10; i++)
        	{
        		tmpvalues[i] = ptnv->_values[i];
        		tmpcounters[i] = ptnv->_counters[i];
        	}
        	INT a, b;
        	a = 0; 
        	b = 6;
        	i = 0;
        	while ( a < 6 && b < 10 )
        	{
	        	while ( a < 6 && tmpcounters[a] >= tmpcounters[b] )
    	    	{
        			ptnv->_values[i] = tmpvalues[a];
        			ptnv->_counters[i] = tmpcounters[a];
        			i++; 
	        		a++;
    	    	}
        		while ( b < 10 && tmpcounters[b] >= tmpcounters[a] )
        		{
	        		ptnv->_values[i] = tmpvalues[b];
    	    		ptnv->_counters[i] = tmpcounters[b];
        			i++; 
        			b++;
	        	}
        	}
        	while ( a < 6 )
   	    	{
       			ptnv->_values[i] = tmpvalues[a];
       			ptnv->_counters[i] = tmpcounters[a];
       			i++; 
        		a++;
   	    	}
       		while ( b < 10 )
       		{
        		ptnv->_values[i] = tmpvalues[b];
   	    		ptnv->_counters[i] = tmpcounters[b];
       			i++; 
       			b++;
        	}
       		//clear the clear_part
           	for (i=6; i< 10; i++)
        	{
        		ptnv->_values[i] = 0;
        		ptnv->_counters[i] = 0;
        	}
        }

        //see if the value can be put into first 6 values (steady part)
        for (i=0;i<6;i++)
        {
                if (value == ptnv->_values[i] && ptnv->_counters[i]>0)
                {
                        ptnv->_counters[i]++;
                        j = i;
                        while (j>0 && ptnv->_counters[j-1]<ptnv->_counters[j])
                        {
                                FB_VALUE_TYPE tmp;
                                tmp = ptnv->_values[j-1];
                                ptnv->_values[j-1] = ptnv->_values[j];
                                ptnv->_values[j] = tmp;

                                tmp = ptnv->_counters[j-1];
                                ptnv->_counters[j-1] = ptnv->_counters[j];
                                ptnv->_counters[j] = tmp;

                                j--;
                        }
                        break;
                }
                else if (ptnv->_counters[i]==0)
                {
                        ptnv->_values[i] = value;
                        ptnv->_counters[i] = 1;
                        break;
                }
        }

        //if the value can be put in first 6 values (steady part)
        //then it is ok. 
        if (i < 6)
		{
        	return;
		}
        
        //put the value into last 4 values (clear part)
        for (i=6;i<10;i++)
        {
               if (value == ptnv->_values[i] && ptnv->_counters[i]>0)
                {
                        ptnv->_counters[i]++;
                        j = i;
                        while (j>6 && ptnv->_counters[j-1]<ptnv->_counters[j])
                        {
                                FB_VALUE_TYPE tmp;
                                tmp = ptnv->_values[j-1];
                                ptnv->_values[j-1] = ptnv->_values[j];
                                ptnv->_values[j] = tmp;

                                tmp = ptnv->_counters[j-1];
                                ptnv->_counters[j-1] = ptnv->_counters[j];
                                ptnv->_counters[j] = tmp;

                                j--;
                        }
                        break;
                }
                else if (ptnv->_counters[i]==0)
                {
                        ptnv->_values[i] = value;
                        ptnv->_counters[i] = 1;
                        break;
                }
        }
} 

void
Set_Instrumentation_Phase_Num(PROFILE_PHASE phase_num)
{
  instrumentation_phase_num = phase_num;
}

// ====================================================================

PROFILE_PHASE
Instrumentation_Phase_Num()
{
  return instrumentation_phase_num;
}

// ====================================================================


#ifdef KEY
void Profile_Value_Init( PU_PROFILE_HANDLE pu_handle, INT32 num_values )
{
  Value_Profile_Vector& Value_Table = pu_handle->Get_Value_Table();

  if( Value_Table.empty() ){
    Value_Table.resize(num_values);
  }
}

// Update appropriate profile information for a value.

void Profile_Value( PU_PROFILE_HANDLE pu_handle, INT32 inst_id, FB_NUM_TYPE value )
{
  Value_Profile_Vector& Value_Table = pu_handle->Get_Value_Table();
  Value_Profile* entry = &Value_Table[inst_id];

  entry->exe_counter++;

  for( int i = 0; i < entry->num_values; i++ ){
    if( entry->value[i] == value ){
      entry->freq[i]++;
      for( int j = i - 1; j >= 0; j-- ){
	if( entry->freq[j] >= entry->freq[i] )
	  break;

	const FB_NUM_TYPE tmp_value = entry->value[j];
	const FB_NUM_TYPE tmp_freq  = entry->freq[j];

	entry->freq[j] = entry->freq[i];
	entry->value[j] = entry->value[i];
	entry->freq[i] = tmp_freq;
	entry->value[i] = tmp_value;

	i = j;
      }

      return;
    }
  }

  if( entry->num_values < TNV ){
    entry->value[entry->num_values] = value;
    entry->freq[entry->num_values]  = 1;
    entry->num_values++;

  } else {
    // Clean up the lower half TNV entries.
    if( entry->exe_counter > ( 2 * entry->freq[0] ) ){
      entry->num_values = TNV / 2;
    }
  }    
}

#if !(defined(TARG_SL) && defined(__SL__))
void Profile_Value_FP_Bin_Init( PU_PROFILE_HANDLE pu_handle, INT32 num_values )
{
  Value_FP_Bin_Profile_Vector& Value_FP_Bin_Table = 
    pu_handle->Get_Value_FP_Bin_Table();

  if( Value_FP_Bin_Table.empty() ){
    Value_FP_Bin_Table.resize(num_values);
  }
}

// Update appropriate profile information for a value.

void Profile_Value_FP_Bin( PU_PROFILE_HANDLE pu_handle, INT32 inst_id, 
			   double value_fp_0, double value_fp_1 )
{
  Value_FP_Bin_Profile_Vector& Value_FP_Bin_Table = 
    pu_handle->Get_Value_FP_Bin_Table();
  Value_FP_Bin_Profile* entry = &Value_FP_Bin_Table[inst_id];

  entry->exe_counter++;
  if (value_fp_0 == 0.0) entry->zopnd0 ++;
  if (value_fp_1 == 0.0) entry->zopnd1 ++;
  if (value_fp_0 == 1.0) entry->uopnd0 ++;
  if (value_fp_1 == 1.0) entry->uopnd1 ++;
}
#endif // !TARG_sl
#endif

}
