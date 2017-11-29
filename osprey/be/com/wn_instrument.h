//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: wn_instrument.h
// $Revision: 1.8 $
// $Date: 05/12/05 08:59:15-08:00 $
// $Author: bos@eng-24.pathscale.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.wn_instrument.h $
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
// The procedures WN_Instrument and WN_Annotate, defined in
// wn_instrument.cxx, invoke the instrumentation and annotation phases
// of feedback collection.
//
// Interface:
//
//   void WN_Instrument( WN *wn, PROFILE_PHASE phase );
//
//     WN_Instrument performs feedback instrumentation on the WHIRL
//     tree rooted at wn (which must be a FUNC_ENTRY node).  This
//     involves inserting into the WHIRL tree invocations to
//     libinstr.so procedures that will collect flow edge frequency
//     counts and store them into a feedback data file when then
//     instrumented program is run.  The PROFILE_PHASE phase is the
//     current point of compilation.
//
//   void WN_Annotate( WN *wn, PROFILE_PHASE phase,
//                     MEM_POOL *MEM_pu_pool );
//
//     WN_Annotate reads frequency counts from one or more previously
//     generated feedback data files and stores the feedback data into
//     the FEEDBACK object Cur_PU_Feedback.  If Cur_PU_Feedback is
//     currently NULL, then a new FEEDBACK object is allocated from
//     the MEM_POOL *MEM_pu_pool.  The PROFILE_PHASE phase is the
//     current point of compilation.  Any FEEDBACK data previously read
//     during an earlier PROFILE_PHASE will be overwritten by the new
//     data.
//
//   void Set_Instrumentation_File_Name( char *fname );
//
//     Set_Instrumentation_File_Name records the prefix for the names
//     of the files from/into which feedback data frequencies are to be
//     read/stored.  The filename prefix is fname concatenated to the
//     PROFILE_PHASE number.
//
// PROFILE_PHASE values are listed in common/com/profile_com.h
//
// Other Important Instrumentation Files:
//
//   common/com/profile_com.h
//   common/com/instr_reader.h
//   common/instrument/instr_reader.cxx
//
// Invokes instrumentation:
//   be/be/driver.cxx (through WN_Instrument and wiw_wopt.Tree_Walk)
//
// ====================================================================
// ====================================================================


#ifndef wn_instrument_INCLUDED
#define wn_instrument_INCLUDED

#include "wn.h"
#include "profile_com.h"            // for PROFILE_PHASE


// ====================================================================
//
// Export the following procedures (defined in wn_instrument.cxx)
// 
// ====================================================================


extern void WN_Instrument( WN *wn, PROFILE_PHASE phase );

extern void WN_Annotate(   WN *wn, PROFILE_PHASE phase,
			   MEM_POOL *MEM_pu_pool );

extern void Set_Instrumentation_File_Name( char *fname );


// ====================================================================
//
// Profile function names
//
// These macros are the names of the libinstr.so procedures that are
// invoked by the calls inserted into an instrumented WHIRL tree.
//
// ====================================================================


#define INST_INIT_NAME            "__profile_init"
#define PU_INIT_NAME              "__profile_pu_init"

#define INVOKE_INIT_NAME          "__profile_invoke_init"
#define INVOKE_INSTRUMENT_NAME    "__profile_invoke"
#define BRANCH_INIT_NAME          "__profile_branch_init"
#define BRANCH_INSTRUMENT_NAME    "__profile_branch"
#define LOOP_INIT_NAME            "__profile_loop_init"
#define LOOP_INSTRUMENT_NAME      "__profile_loop"
#define LOOP_INST_ITER_NAME       "__profile_loop_iter"
#define SHORT_CIRCUIT_INIT_NAME   "__profile_short_circuit_init"
#define SHORT_CIRCUIT_INST_NAME   "__profile_short_circuit"
#define CALL_INIT_NAME            "__profile_call_init"
#define ICALL_INIT_NAME           "__profile_icall_init"
#define ICALL_INSTRUMENT_NAME     "__profile_icall"
#define CALL_INST_ENTRY_NAME      "__profile_call_entry"
#define CALL_INST_EXIT_NAME       "__profile_call_exit"
#define SWITCH_INIT_NAME          "__profile_switch_init"
#define SWITCH_INSTRUMENT_NAME    "__profile_switch"
#define COMPGOTO_INIT_NAME        "__profile_compgoto_init"
#define COMPGOTO_INSTRUMENT_NAME  "__profile_compgoto"
#ifdef KEY
#define VALUE_FP_BIN_INIT_NAME          "__profile_value_fp_bin_init"
#define VALUE_FP_BIN_INSTRUMENT_NAME    "__profile_value_fp_bin"

#define VALUE_INIT_NAME          "__profile_value_init"
#define VALUE_INSTRUMENT_NAME    "__profile_value"
#endif


// ====================================================================

#endif


