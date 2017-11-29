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


#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */

#include "errors.h"			// for ipa_inline.h
#include "mtypes.h"			// for const.h
#define USE_DST_INTERNALS
#include "dwarf_DST_mem.h"		// for ipc_file.h

#include "ipc_file.h"			// for IP_FILE_HDR
#include "ipa_inline.h"			// for IPA_NODE
#include "ipa_option.h"			// for Trace_IPA
#include "ipa_section_annot.h"          // for section annot
// Mark the reference parameters as not euse and/or kill
void
Mark_use_kill_param (IPA_NODE *caller, IPA_NODE *callee,
		     IPA_EDGE *edge)
{
  // first test this out for FORTRAN 
  // there are more issues for C/C++

  if (!(caller->Summary_Proc()->Is_alt_entry() || 
	caller->Summary_Proc()->Has_alt_entry() || 
	(caller->Get_PU().flags & PU_IS_NESTED_FUNC)))
    {
      WN *call = edge->Whirl_Node();
      if (callee->Get_PU().src_lang == PU_F77_LANG)
	{
#ifdef TODO
	  INT actual_count = WN_num_actuals (call);
	  INT formal_count = callee->Num_Formals();
	  INT count = 0;

	  if (actual_count < formal_count)
	    count = actual_count;
	  else
	    count = formal_count;

	  IPA_NODE_SECTION_INFO *callee_info = callee->Section_Annot();
	  STATE_ARRAY *callee_formals_state = callee_info->Get_formals();
  
	  for (INT i = 0; i < count; ++i) 
	    {
	      STATE *state_info = &(*callee_formals_state)[i];
	      WN *parm = WN_kid (call, i);
	      Is_True (WN_operator (parm) == OPR_PARM,
		       ("Invalid kid from a call node"));
	      if ((Trace_IPA || Trace_Perf) && ((state_info->Is_must_kill() ||
						 !state_info->Is_euse()) &&
						state_info->Is_scalar()))
		{
		  fprintf (TFile, "Setting parameters in %s call ",
			   DEMANGLE (ST_name(caller->Func_ST())));
		  fprintf (TFile, "%s: position = ",
			   DEMANGLE (ST_name(cg->Callee(edge)->Func_ST())));
		  fprintf (TFile, "%d ", i);
		}
	      if (!state_info->Is_euse() && state_info->Is_scalar()) {
		WN_Set_Parm_Not_Exposed_Use (parm);
		if (Trace_IPA || Trace_Perf)
		  fprintf (TFile, "is not exposed use : ");
	      }
	      if (state_info->Is_must_kill() && state_info->Is_scalar()) 
		{
		  WN_Set_Parm_Is_Killed(parm);
		  if (Trace_IPA || Trace_Perf)
		    fprintf (TFile, "is killed ");
		}
	      if (Trace_IPA || Trace_Perf)
		fputc ('\n', TFile);
	    }
#endif
	}
    }
} // Mark_use_kill_param




