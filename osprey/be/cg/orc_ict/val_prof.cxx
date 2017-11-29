/*
  Copyright (c) 2001, Institute of Computing Technology, Chinese Academy of Sciences
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:
  
  Redistributions of source code must retain the above copyright notice, this list
  of conditions and the following disclaimer. 

  Redistributions in binary form must reproduce the above copyright notice, this list
  of conditions and the following disclaimer in the documentation and/or other materials
  provided with the distribution. 

  Neither the name of the owner nor the names of its contributors may be used to endorse or
  promote products derived from this software without specific prior written permission. 

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
 
//-*-c++-*-

#include <stdlib.h>
#include <algorithm>
#include <vector>
#include <stack>
#include <list>
#include <set>

#include "bb.h"
#include "defs.h"
#include "cg_region.h"
#include "label_util.h"
#include "symtab.h"
#include "data_layout.h"
#include "symtab_access.h"
#include "cg_flags.h"
#include "vt_region.h"
#include "tracing.h"
#include "instr_reader.h"
#include "dump_feedback.h"
#include "freq.h"
#include "ipfec_defs.h"
#include "gra_live.h"
#include "region.h"
#include "region_bb_util.h"
#include "region_update.h"
#include "region_verify.h"
#include "ipfec_options.h"

#include "val_prof.h"

#define GEN_NEWBB_FOR_RESTORE___Should_disable_ebo
//#define VALUE_PROFILE_INTERNAL_DEBUG		
//#define _maybe_wrong_test
//#define _delete_but_not_fully_tested_yet
#define	VALUE_PROFILE_DETAIL

#define My_OP_to_split_entry1 TOP_mov_f_pr
#define My_OP_to_split_entry2 TOP_mov_f_ar
#define My_OP_to_split_entry3 TOP_mov_f_br
#define My_OP_to_split_entry4 TOP_spadjust
#define TNV_N 10

INST2PROFLIST inst2prof_list;

//OP_TNV_MAP op_tnv_map;
OP_MAP op_tnv_map; 
OP_MAP op_stride_tnv_map;

/* The FB_Info_Value definition in ORC conflicit with its def
 * in current compiler. Turn off val-prof for the time-being.
 */
