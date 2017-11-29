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

#ifndef  edge_profile_INCLUDED
#define  edge_profile_INCLUDED

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
#include "wn.h"

//============================================================================
//
// Profile function names
//
// These macros are the names of the libcginstr.a. Procedures that are
// invoked by the calls inserted into instrumented IR.
//
//============================================================================
#define INSTR_INIT_NAME             "__profile_init"
#define PU_INIT_NAME                "__profile_pu_init"
#define EDGE_INST_NAME              "__profile_edge"

//============================================================================
//
// registers
//
// These macros are the registers of that are used when a BB (basic block)
// is created. The BB invokes a procedure in instrumenation library.
//
//============================================================================
#define ARG_REG1 (4+REGISTER_MIN)
#define ARG_REG2 (5+REGISTER_MIN)

#define RETURN_REG   (2+REGISTER_MIN)

#define FLOAT_RETURN_REG   (0+REGISTER_MIN)

#define ERROR_FREQ(freq)  freq < 0.0
#define ERROR_PROB(prob)  prob < 0.0
#define UNINIT_FREQ(freq) !(freq == freq)
#define ZERO_FREQ(freq)   freq == 0.0
#define ZERO_PROB(prob)   prob == 0.0

#define  FB_FREQ_UNINIT     -1.0
#define  FB_FREQ_UNKNOWN    -2.0
#define  FB_FREQ_ERROR      -3.0
#define  LABEL_IDX_NONE     0

#define RESTORE_NONE 0
#define RESTORE_INT_RETURN_TN 1
#define RESTORE_FLOAT_RETURN_TN 2

extern INT32 current_PU_handle;
extern BOOL CG_PU_Has_Feedback;

//============================================================================
//  edge_profile.h
//
//  This file contains the interface for edge profile.
//  In our design, we will instrument and annotation at every branch.
//
//    
//
//============================================================================
class PROFILE_MEM;
class PROFILE;
class EDGE_PROFILE;

//============================================================================
//
//   Class Name: PROFILE_MEM
//
//   Base Class: <none>
//
//   Derived Class: PROFILE
//
//   Class Description: 
//    
//   Note:  
//
//============================================================================   
class PROFILE_MEM {
protected:
  MEM_POOL _m;

  PROFILE_MEM() {
    MEM_POOL_Initialize( &_m, "PROFILE_MEM", true );
    MEM_POOL_Push( &_m );
  }
    
  ~PROFILE_MEM() {
    MEM_POOL_Pop( &_m );
    MEM_POOL_Delete( &_m );
  }
};
        
//============================================================================
//
//   Class Name: PROFILE
//
//   Base Class: PROFILE_MEM
//
//   Derived Class: <none>
//
//   Class Description: 
//    
//   Note:  
//
//============================================================================   
class PROFILE : public PROFILE_MEM{
protected:    

  // Phase at which instrumentation/annotation is occuring
  //  (PROFILE_PHASE declared in common/com/profile_com.h)
  PROFILE_PHASE      _phase;

  // _instrumenting is TRUE  if inserting instrumentation
  // _instrumenting is FALSE if annotatation with feedback data
  BOOL               _instrumenting;

public:    
  PROFILE() {}
  ~PROFILE() {}
};

class EDGE_PROFILE : public PROFILE{
private:    
  CGRIN*			       _rin;
  BB*                _pu_first_bb;
  BB*                _pu_last_bb;
  // Counter for each type of instrumentation
  UINT32             _count_branch;
  UINT32			       _count_indirect_cond_branch;
  UINT32             _count_cloop;
  UINT32             _count_call;
  UINT32             _count_top;

  // _instrument_count is the total number of edges that have been
  //   instrumented/annotated; it is used as a checksum
  UINT32             _instrument_count;

  // _pu_handle is used as a Handle for all profile calls in PU
  // _fb_handle is a list of handles for feedback info for this PU
  INT                _pu_handle;
  PU_PROFILE_HANDLES _fb_handles;

  char * _srcfile_pu_name;
  TN * _pu_hdr_return_tn; // store the value of pu init call
  
public:    
  EDGE_PROFILE( CGRIN* rin, BOOL instru, PROFILE_PHASE phase, PU_PROFILE_HANDLES fb_handles ) 
  {
    _rin = rin;
    _instrumenting = instru;
    _phase = phase;
    _fb_handles = fb_handles;
    _pu_first_bb = REGION_First_BB;;
    _pu_last_bb = Find_Last_BB(_pu_first_bb);
    _pu_handle = current_PU_handle;
    _instrument_count = 0;
    _srcfile_pu_name = CXX_NEW_ARRAY(char, strlen(Src_File_Name) + strlen("/") + strlen(Cur_PU_Name)
		                     + 1, &_m);
    strcpy(_srcfile_pu_name,Src_File_Name);
    strcat(_srcfile_pu_name,"/");
    strcat(_srcfile_pu_name,Cur_PU_Name);  
    _pu_hdr_return_tn = Gen_Register_TN( ISA_REGISTER_CLASS_integer, 8 );
  }
   
  ~EDGE_PROFILE() {}
  PU_PROFILE_HANDLES& FB_Handle() { return _fb_handles; }

  // --------------------------------------------------------------------------
  // Instrumentation of each type of BB
  // --------------------------------------------------------------------------
  void Instrument_Indirect_Cond_Branch( BB *bb );
  void Instrument_Ip_Rel_Branch( BB  *bb );
  void Instrument_Cloop_Branch( BB *bb );
  void Instrument_Top_Branch( BB *bb );

  void Instrument_None_and_Other_Xfer( BB *bb );

  void Instrument_Mul_Target_Branch(BB* bb);
  void Instrument_Call_Branch( BB *bb );

  void Annotate_Mul_Target_Branch(BB* bb);
  void Annotate_Call_Branch(BB* bb);

  
  // --------------------------------------------------------------------------
  // annotation of each type of BB
  // --------------------------------------------------------------------------
  void Annotate_Indirect_Cond_Branch( BB *bb );
  void Annotate_Ip_Rel_Branch( BB  *bb );
  void Annotate_Cloop_Branch( BB *bb );
  void Annotate_Top_Branch( BB *bb );
  void Annotate_None_and_Other_Xfer( BB *bb );
  
  // --------------------------------------------------------------------------
  // Utility functions 
  // --------------------------------------------------------------------------
  inline void Increase_Instru_Counter(void)
  {
    _instrument_count++;
  }
  
  inline void Set_Pu_First_BB(BB* bb)
  {
    _pu_first_bb = bb;
  }
  
  inline BB* Get_Pu_First_BB(void)
  {
    return _pu_first_bb;
  }
  
  inline void Set_Pu_Last_BB(BB* bb)
  {
    _pu_last_bb = bb;
  }
  
  inline BB* Get_Pu_Last_BB(void)
  {
    return _pu_last_bb;
  }
  
  inline char* Get_Srcfile_Pu_Name(void)
  {
    return _srcfile_pu_name;
  }
  
  // --------------------------------------------------------------------------
  // perform instrumentation/annotation
  // --------------------------------------------------------------------------
  void CG_Instrument_Node(BB *bb);
  void CG_Annotate_Node(BB *bb);

  // perform instrumentation or annotate feedback.
  void CG_Instrument(void);
  void CG_Annotate(void);

  // other functions
  void Dump_Feedback(void);
  void Instrument_Entry(void);
  void Change_Tgt_Label(op* branch_op,LABEL_IDX old_label, LABEL_IDX new_label);
  void Prepare_Call_Init_Return_BB(BB * bb);
  void Gen_Instr_Init_Call_BB(char* function_name,char* outputfile_name, int phase);
  BB* Gen_PU_Init_Call_BB(char* function_name,char* file_pu_name, INT64 _instrument_count);
  BB* Gen_Instru_Call_BB(char* function_name, char* file_pu_name,INT64 id); 
  BB* Gen_Call_BB(char* function_name, char* str_arg, INT64 int_arg, int restore_type, int restore_sum = 0);

  // Find out the TN in the op;op is in NO i bb and op's code is opc , 
  // the op's NO i1 TN is t1, and return the NO index TN, if no such TN
  // return NULL
  TN* Find_TN(TOP opc,int iBB,int i1, TN* t1, int index); 
  TN* Find_TN_in_BB(TOP opc, BB* in, int i1, TN* t1, int index);

  // Check whether bb is in bblst from bblst's beginning to before_bb
  BOOL BB_Is_In_BBLIST(BBLIST* bblst,BB* bb,BB* before_bb);
  // Check whether bb is not added or changed by other profiler
  BOOL BB_Is_For_Instru(BB* bb);  
  void Propagate(void);
  void Propagate_Node_In(BB* bb);
  void Propagate_Node_Out(BB* bb);
};
extern BOOL CG_PU_Has_Feedback;
extern BOOL FREQ_freqs_computed;
extern void Compute_Edge_Freq_Base_Prob(void);
extern void Compute_Edge_Prob_Base_Freq(void);
extern void CG_Edge_Profile_Annotation( CGRIN* cgrin,PROFILE_PHASE phase );
extern void CG_Edge_Profile_Instrument( CGRIN* cgrin,PROFILE_PHASE phase );
extern INT32 Curr_PU_BB_Count(void);
extern void Init_CFG(void);
extern void Change_BB_Label(BB* bb,LABEL_IDX i);
extern BB* First_Branch_BB(void);
extern BOOL Result_Tn_In_BB(BB* bb, TN* tn);
extern LABEL_IDX Get_Br_Op_Tgt_Label(OP* branch_op);
extern INT32 Get_Return_Reg_Sum(BB* bb);
extern INT32 Get_Float_Return_Reg_Sum(BB* bb);
inline WN * Gen_Param( WN *arg, UINT32 flag )
{
  return WN_CreateParm( WN_rtype( arg ), arg,
			MTYPE_To_TY( WN_rtype( arg ) ), flag );
}

#endif    


