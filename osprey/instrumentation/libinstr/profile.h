//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: profile.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/instrumentation/libinstr/profile.h,v $
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


#ifndef profile_INCLUDED
#define profile_INCLUDED

#ifndef profile_com_INCLUDED
#include "profile_com.h"
#endif

#ifndef profile_aux_INCLUDED
#include "profile_aux.h"
#endif 


// ====================================================================


extern PU_PROFILE_HANDLE Get_PU_Handle(char *file_name, char* pu_name,
				       long current_pc, INT32 pu_size, INT32 checksum);

extern void Profile_Invoke_Init(PU_PROFILE_HANDLE pu_handle,
				INT32 num_invokes);

extern void Profile_Invoke(PU_PROFILE_HANDLE pu_handle, INT32 invoke_id);


extern void Profile_Branch_Init(PU_PROFILE_HANDLE pu_handle,
				INT32 num_branches);

extern void Profile_Branch(PU_PROFILE_HANDLE pu_handle, INT32 id, bool taken);

extern void Incr_Branch_Taken(PU_PROFILE_HANDLE pu_handle, INT32 id);
extern void Incr_Branch_Not_Taken(PU_PROFILE_HANDLE pu_handle, INT32 id);


extern void Profile_Switch_Init(PU_PROFILE_HANDLE pu_handle,
				INT32 num_switches, INT32 *switch_num_targets,
				INT32 num_case_values, INT64 *case_values);

extern void Profile_Switch(PU_PROFILE_HANDLE pu_handle, INT32 switch_id,
			   INT32 target, INT32 num_targets);


extern void Profile_Compgoto_Init(PU_PROFILE_HANDLE pu_handle,
				  INT32 num_compgotos,
				  INT32 *compgoto_num_targets);

extern void Profile_Compgoto(PU_PROFILE_HANDLE pu_handle, INT32 compgoto_id,
			     INT32 target, INT32 num_targets);

#ifdef KEY
extern void Profile_Value_Init( PU_PROFILE_HANDLE pu_handle, INT32 num_values );

extern void Profile_Value( PU_PROFILE_HANDLE pu_handle,
			   INT32 inst_id, INT64 value );

extern void Profile_Value_FP_Bin_Init( PU_PROFILE_HANDLE pu_handle, 
				       INT32 num_values );

extern void Profile_Value_FP_Bin( PU_PROFILE_HANDLE pu_handle,
			          INT32 inst_id, 
				  double value0, double value1 );
#endif

extern void Profile_Loop_Init(PU_PROFILE_HANDLE pu_handle, INT32 num_loops);

extern void Profile_Loop(PU_PROFILE_HANDLE pu_handle, INT32 id);

extern void Profile_Loop_Iter(PU_PROFILE_HANDLE pu_handle, INT32 id);


extern void Profile_Short_Circuit_Init(PU_PROFILE_HANDLE pu_handle,
				       INT32 num_short_circuit_ops);

extern void Profile_Short_Circuit(PU_PROFILE_HANDLE pu_handle,
				  INT32 short_circuit_id, bool taken);

extern void Incr_Right_Taken(PU_PROFILE_HANDLE pu_handle, INT32 id);
extern void Incr_Neither_Taken(PU_PROFILE_HANDLE pu_handle, INT32 id);


extern void Profile_Call_Init(PU_PROFILE_HANDLE pu_handle, INT32 num_calls);

extern void Profile_Call_Entry(PU_PROFILE_HANDLE pu_handle, INT32 call_id);

extern void Profile_Call_Exit(PU_PROFILE_HANDLE pu_handle, INT32 call_id);

extern void Profile_Icall_Init(PU_PROFILE_HANDLE pu_handle, INT32 num_icalls);

extern void Profile_Icall(PU_PROFILE_HANDLE pu_handle, INT32 icall_id, void * called_fun_address);

extern void Set_Instrumentation_Phase_Num(PROFILE_PHASE phase_num);

extern PROFILE_PHASE Instrumentation_Phase_Num(void);


// ====================================================================


#endif /* profile_INCLUDED */
