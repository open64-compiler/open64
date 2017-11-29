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
 
//-*-c++-*-

//*********************************************************************
//
// Module: dump_feedback.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_ict/dump_feedback.cxx,v $
//
// Description:
// 
// Dump feedback file info.See dump_feedback.h for the external 
// interface.
//    
//
//*********************************************************************
#include "dump_feedback.h"
#include <stdio.h>

extern char* Cur_PU_Name;
extern char* Src_File_Name;

/* dump an edge's feedback info  */
void Dump_Edge_Info(BB* source_bb,BBLIST* edge,FILE* fp)
{
  BB* target_bb = BBLIST_item(edge);
  fprintf(fp,"Freq:%f Source BB:%d-->Target BB:%d\n",BBLIST_freq(edge),
                                       BB_id(source_bb),BB_id(target_bb));
}

/* dump current PU's feedback data */
void Dump_Fb_Data(PU_PROFILE_HANDLES fb_handles,FILE* fp)
{
  
  fprintf(fp,"\n======= Cur PU Name:");
  fprintf(fp,Cur_PU_Name);
  fprintf(fp,"=========\n");
  
  FB_Info_Call info_call( FB_FREQ_ZERO );
  FB_Info_Branch info_branch( FB_FREQ_ZERO, FB_FREQ_ZERO );
  FB_Info_Invoke info_invoke( FB_FREQ_ZERO );
  FB_Info_Edge info_edge( FB_FREQ_ZERO );
  FB_Info_Value info_value;
  FB_Info_Value info_stride;
  
  for (PU_PROFILE_ITERATOR i( fb_handles.begin() ); i != fb_handles.end (); ++i)
  {
    PU_Profile_Handle * handle=*i;
    for(int id=0;id < handle->Call_Profile_Table.size();id++){
      info_call = FB_FREQ_ZERO;
      FB_Info_Call& info = Get_Call_Profile( *i, id );
      info_call.freq_entry += info.freq_entry;
      info_call.freq_exit += info.freq_exit;
      info_call.Print(fp);
      fprintf(fp,"\n");
    }
    for(int id=0;id< handle->Branch_Profile_Table.size();id++){
      FB_Info_Branch& info = Get_Branch_Profile( *i, id );
      info_branch.freq_taken += info.freq_taken;
      info_branch.freq_not_taken += info.freq_not_taken;
      info_branch.Print(fp);
      fprintf(fp,"\n");
    }
    
    for(int id=0;id< handle->Invoke_Profile_Table.size();id++){
      FB_Info_Invoke& info = Get_Invoke_Profile( *i, id );
      info_invoke = FB_FREQ_ZERO;
      info_invoke.freq_invoke += info.freq_invoke;
      info_invoke.Print(fp);
      fprintf(fp,"\n");
    }
   for(int id=0;id< handle->Edge_Profile_Table.size();id++){
      FB_Info_Edge& info = Get_Edge_Profile( *i, id );
      info_edge = FB_FREQ_ZERO;
      info_edge.freq_edge += info.freq_edge;
      info_edge.Print(fp);
      fprintf(fp,"\n");
    }
   for(int id=0;id< handle->Value_Profile_Table.size();id++){
      FB_Info_Value& info = Get_Value_Profile( *i, id );
      info_value = info;
      info_value.Print(fp);
      fprintf(fp,"\n");
    }
   for(int id=0;id< handle->Stride_Profile_Table.size();id++){
      FB_Info_Value& info = Get_Stride_Profile( *i, id );
      info_stride = info;
      info_stride.Print(fp);
      fprintf(fp,"\n");
    }
  }
}
