//-*-c++-*-

/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

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
// These data structures are used to store frequency counts during
// the run of an instrumented program unit.
//
// ====================================================================
// ====================================================================


#ifndef profile_aux_INCLUDED
#define profile_aux_INCLUDED

#include "vector.h"
#include "hash_map.h"
#include <fb_tnv.h>

// ====================================================================

namespace Instr {

#if defined(TARG_SL) && defined(__SL__)
typedef INT32 FB_NUM_TYPE;
typedef UINT32 FB_VALUE_TYPE;
#else
typedef INT64 FB_NUM_TYPE;
typedef UINT64 FB_VALUE_TYPE;
#endif
// All the information about an invoke node is stored in the
// Invoke_Profile structure. Currently for each invokation
// we maintain information on the number of time the invokation
// occured

struct Invoke_Profile {
   FB_NUM_TYPE invoke_count;
   Invoke_Profile() : invoke_count(0) {}
};


// All the information about a branch node is stored in the
// Branch_Profile structure. Currently for each conditional
// branch we maintain information on whether the branch was 
// taken or not

struct Branch_Profile {
   FB_NUM_TYPE taken;
   FB_NUM_TYPE not_taken;
   Branch_Profile() : taken(0), not_taken(0) {}
};

#ifdef KEY
#define TNV 10
// All the information about a rem/div node is stored in the
// Value_Profile structure. Currently for each value of a
// divisor we maintain information on how many times this value
// is accessed through the lifetime of the program.

struct Value_Profile {
  FB_NUM_TYPE num_values;
  FB_NUM_TYPE exe_counter;
  FB_NUM_TYPE value[TNV];
  FB_NUM_TYPE freq[TNV];
  Value_Profile() : num_values(0), exe_counter(0) {}
};

// All the information about a F8MPY node is stored in the
// Value_FP_Profile structure. Currently for each value of a
// multiplicand we maintain information on how many times this value
// is accessed through the lifetime of the program.

struct Value_FP_Bin_Profile {
  INT64 exe_counter;
  INT64 zopnd0;
  INT64 zopnd1;
  INT64 uopnd0;
  INT64 uopnd1;
  Value_FP_Bin_Profile() : exe_counter(0),
  			   zopnd0(0), zopnd1(0), uopnd0(0), uopnd1(0) {}
};
#endif


// All the information about an Compgoto node is maintained
// in the Compgoto_Profile structure. For each possible jump
// target of an Compgoto, we want to gather information on how
// many times we branched to that target from this Compgoto.

// The data member targets_profile is a vector; the ith element
// of this vector gives a count of how many times we jumped to
// target i of the Compgoto.

struct Compgoto_Profile {
    typedef vector<FB_NUM_TYPE> value_type;
    value_type targets_profile;

    value_type& Get_Targets_Profile () {
	return targets_profile;
    }

    INT32 size () const { return targets_profile.size (); }
};
	

// All the information about a CAND and COR is stored
// in the Short_Circuit_Profile structure.

struct Short_Circuit_Profile {
   FB_NUM_TYPE right_taken_count;
   FB_NUM_TYPE neither_taken_count;

   Short_Circuit_Profile() : right_taken_count(0), neither_taken_count(0) {}
};

// All the information about a CALL is stored in 
// Call_Profile structure.

struct Call_Profile {
   FB_NUM_TYPE entry_count;
   FB_NUM_TYPE exit_count;

   Call_Profile() : entry_count(0), exit_count(0) {}
};

struct Icall_Profile {
	FB_TNV fb_tnv;
};

// All the information about a DO LOOP or WHILE LOOP is stored
// in the Loop_Profile structure.

struct Loop_Profile {
   FB_NUM_TYPE invocation_count; // total times the loop is invoked
   FB_NUM_TYPE total_trip_count; // total trip count from all invocations
   FB_NUM_TYPE last_trip_count;  // trip count from last invocation
   FB_NUM_TYPE min_trip_count;   // minimum trip count from previous invocations
   FB_NUM_TYPE max_trip_count;   // maximum trip count from previous invocations
   FB_NUM_TYPE num_zero_trips;   // Number of times the loop had a zero trip

   Loop_Profile () :
       invocation_count(0), total_trip_count(0), last_trip_count(0),
       min_trip_count(0), max_trip_count(0), num_zero_trips(0) {}
};


// All the information about an Switch node is maintained
// in the Switch_Profile structure. For each possible jump
// target of a Switch, we want to gather information on how
// many times we branched to that target from this Switch.

// The ith element of the vector targets_profile counts how many
// times we jump to target i of the Switch.
// The ith element of the vector targets_case_value records the
// index value on which the ith branch is taken.
// Note that   targets_profile.size () == targets_case_value.size () + 1

struct Switch_Profile {
    typedef vector<FB_NUM_TYPE> value_type;
    value_type targets_profile;
    value_type targets_case_value;
    Switch_Profile() {}

    value_type& Get_Targets_Profile() {
	return targets_profile;
    }

    value_type& Get_Targets_Case_Value() {
	return targets_case_value;
    }

    INT32 size () const { return targets_profile.size (); }
};

// ====================================================================

// For each PU, we maintain a handle that can be retrieved from
// a hash table using the PC address of the PU. This handle 
// contains pointers to all the profile information for that PU.
// Currently, we only have a Branch_Profile_Table and a
// Compgoto_Profile_Table in this handle to keep profile information
// of conditional branches and Compgotos. The handle also contains the
// file name and the name of the PU which are set when the handle is
// first created.

typedef vector<Invoke_Profile>		Invoke_Profile_Vector;
typedef vector<Branch_Profile>		Branch_Profile_Vector;
typedef vector<Switch_Profile>		Switch_Profile_Vector;
typedef vector<Compgoto_Profile>	Compgoto_Profile_Vector;
typedef vector<Loop_Profile>		Loop_Profile_Vector;
typedef vector<Short_Circuit_Profile>	Short_Circuit_Profile_Vector;
typedef vector<Call_Profile>		Call_Profile_Vector;
typedef vector<Icall_Profile>		Icall_Profile_Vector;
#ifdef KEY
typedef vector<Value_Profile>		Value_Profile_Vector;
typedef vector<Value_FP_Bin_Profile>	Value_FP_Bin_Profile_Vector;
#endif

struct PU_Profile_Handle {
    Invoke_Profile_Vector	Invoke_Profile_Table;
    Branch_Profile_Vector	Branch_Profile_Table;
    Switch_Profile_Vector	Switch_Profile_Table;
    Compgoto_Profile_Vector	Compgoto_Profile_Table;
    Loop_Profile_Vector		Loop_Profile_Table;
    Short_Circuit_Profile_Vector Short_Circuit_Profile_Table;
    Call_Profile_Vector		Call_Profile_Table;
    Icall_Profile_Vector	Icall_Profile_Table;
#ifdef KEY
    Value_Profile_Vector	Value_Profile_Table;
    Value_FP_Bin_Profile_Vector	Value_FP_Bin_Profile_Table;
#endif

    INT32 checksum;
    char *file_name;
    char *pu_name;
    INT32 pu_size;
    FB_VALUE_TYPE runtime_fun_address;

    PU_Profile_Handle() : checksum(0) {}
    
    PU_Profile_Handle(char *fname, char *pname, long current_pc, INT32 pusize, INT32 c_sum) {
	checksum = c_sum;
	file_name = (char *) malloc (sizeof(char) * (strlen(fname) + 1));
	pu_name = (char *) malloc (sizeof(char) * (strlen(pname) + strlen("/") + strlen(fname) + 1));
	strcpy(file_name, fname);
	strcpy(pu_name, fname);
	strcat(pu_name,"/");
	strcat(pu_name,pname);
	pu_size = pusize;
	runtime_fun_address = current_pc;
    }

    ~PU_Profile_Handle() {
	free (file_name);
	free (pu_name);
    }

    Invoke_Profile_Vector& Get_Invoke_Table () {
	return Invoke_Profile_Table;
    }

    Branch_Profile_Vector& Get_Branch_Table () {
	return Branch_Profile_Table;
    }

    Switch_Profile_Vector& Get_Switch_Table () {
	return Switch_Profile_Table;
    }

    Compgoto_Profile_Vector& Get_Compgoto_Table () {
	return Compgoto_Profile_Table;
    }

    Loop_Profile_Vector& Get_Loop_Table () {
	return Loop_Profile_Table;
    }

    Short_Circuit_Profile_Vector& Get_Short_Circuit_Table () {
	return Short_Circuit_Profile_Table;
    }

    Call_Profile_Vector& Get_Call_Table () {
	return Call_Profile_Table;
    }

    Icall_Profile_Vector& Get_Icall_Table () {
	return Icall_Profile_Table;
    }

#ifdef KEY
    Value_Profile_Vector& Get_Value_Table () {
      return Value_Profile_Table;
    }

    Value_FP_Bin_Profile_Vector& Get_Value_FP_Bin_Table () {
      return Value_FP_Bin_Profile_Table;
    }
#endif

    void Set_file_name(char *s);
    void Set_pu_name(char *s);

#ifdef KEY
    void * operator new (size_t size) {
      void * p = malloc (size);
      return p;
    }

    void operator delete (void *p) {
      free (p);
    }
#endif

};

typedef PU_Profile_Handle * PU_PROFILE_HANDLE;

// Define a hash table that can be accessed with a PC address of
// a PU to get a pointer to the handle of the PU back

typedef hash_map<long, PU_PROFILE_HANDLE> HASH_MAP;

extern HASH_MAP PU_Profile_Handle_Table;
}

#endif /* profile_aux_INCLUDED */
