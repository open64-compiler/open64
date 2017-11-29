/*
  Copyright (C) 2000-2003, Institute of Computing Technology, Chinese Academy of Sciences
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
 
#ifndef value_profile_INCLUDED
#define value_profile_INCLUDED "val_prof.h"

// some include files here.
#include "cxx_memory.h"
#include "profile_com.h"
#include "tn.h"
#include "instr_reader.h"
#include "glob.h"
#include "fb_freq.h"
#include "fb_info.h"
#include "const.h"
#include "op.h"
#include "mempool.h"
#include "profile_util.h"
#include "calls.h"
#include "bb_map.h"

struct INST_TO_PROFILE {
private:
    TOP _opcode;	// instruction kind to instrument 
    INT _count;		// counter for each opcode
    BOOL _instr_before;	// instrumentation needs to be before the instruction?

public:
	INST_TO_PROFILE(TOP opcode, INT count, BOOL instr_before):
		_opcode(opcode), _count(count), _instr_before(instr_before)
		{}
	INST_TO_PROFILE(TOP opcode, BOOL instr_before):
		_opcode(opcode), _count(0), _instr_before(instr_before)
		{}
	~INST_TO_PROFILE(){}

    TOP Opcode() const { return _opcode; }
    BOOL Is_instr_before() const { return _instr_before; }
    void Increase_count() { _count ++; }
};


typedef mempool_allocator<struct INST_TO_PROFILE *> INST2PROF_ALLOC_TYPE;
typedef std::vector<INST_TO_PROFILE *, INST2PROF_ALLOC_TYPE> INST2PROFLIST;
//typedef hash_map<INT32, FB_Info_Value *, hash<INT32> > OP_TNV_MAP;


extern INST2PROFLIST inst2prof_list;
extern OP_MAP op_tnv_map;
extern OP_MAP op_stride_tnv_map;
extern void CG_VALUE_Instrument(CGRIN *rin, PROFILE_PHASE phase,BOOL stride_profiling_flag, BOOL do_value);
extern void CG_VALUE_Annotate(CGRIN *rin, PROFILE_PHASE phase);


#define VAL_PROF_FLAG 121
#define SRD_PROF_FLAG 131
#define INVOKE_VALUE_INSTRUMENT_NAME    "__value_profile_invoke"
#define INVOKE_VALUE_INSTRUMENT_INIT_NAME "__value_profile_pu_init"
#define INVOKE_STRIDE_INSTRUMENT_NAME    "__stride_profile_invoke"
#define INVOKE_STRIDE_INSTRUMENT_INIT_NAME "__stride_profile_pu_init"

#define FIRST_OUTPUT_REG (127+REGISTER_MIN)
#define RETURN_REG   (8+REGISTER_MIN)
#define FLOAT_RETURN_REG   (8+REGISTER_MIN)
#define RETURN_ADDRESS_REGNUM 329

#define FIRST_INPUT_REG (32+REGISTER_MIN)


#endif
