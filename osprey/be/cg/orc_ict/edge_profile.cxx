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
// Module: edge_profile.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_ict/edge_profile.cxx,v $
//
// Description:
//
// Edge profile is used to get the BB's frequency and branch's prob.
// void CG_Edge_Profile_Instrument( CGRIN* cgrin,PROFILE_PHASE phase )
//   Insert some function calls of instrumentation library.
// void CG_Edge_Profile_Annotation( CGRIN* cgrin,PROFILE_PHASE phase )
//   Map feedback info to BB's frequency  BB's outcoming edge's 
//   probability.
//
//*********************************************************************
#include "bb.h"
#include "cg.h"
#include "defs.h"
#include "cg_region.h"
#include "label_util.h"
#include "edge_profile.h"
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
#include <ipfec_options.h>

WN *
Gen_Call_Shell( char *name, TYPE_ID rtype, INT32 argc )
{
  TY_IDX  ty = Make_Function_Type( MTYPE_To_TY( rtype ) );
  ST     *st = Gen_Intrinsic_Function( ty, name );

  Clear_PU_no_side_effects( Pu_Table[ST_pu( st )] );
  Clear_PU_is_pure( Pu_Table[ST_pu( st )] );
  Set_PU_no_delete( Pu_Table[ST_pu( st )] );

  WN *wn_call = WN_Call( rtype, MTYPE_V, argc, st );

  WN_Set_Call_Default_Flags(  wn_call );
  return wn_call;
}  

WN *
Gen_Call( char *name, WN *arg1, WN *arg2, TYPE_ID rtype )
{
  WN *call = Gen_Call_Shell( name, rtype, 2 );
  WN_actual( call, 0 ) = Gen_Param( arg1, WN_PARM_BY_VALUE );
  WN_actual( call, 1 ) = Gen_Param( arg2, WN_PARM_BY_VALUE );
  return call;
}

WN *
Gen_Call( char *name, WN *arg1,  TYPE_ID rtype )
{
  WN *call = Gen_Call_Shell( name, rtype, 1 );
  WN_actual( call, 0 ) = Gen_Param( arg1, WN_PARM_BY_VALUE );
  return call;
}

void CG_Edge_Profile_Instrument( CGRIN* cgrin,PROFILE_PHASE phase )
{
  // When the users don't specify the instru file, the instru file's name 
  // will be defaulted to the source file's name. and the instru file name's 
  // postfix is ".instr".
  extern char* Instrumentation_File_Name;
  if (Instrumentation_File_Name != NULL)
     Instru_File_Name = Instrumentation_File_Name;
  if ( Instru_File_Name == NULL )
  {
    DevWarn( "not specify the feedback file!use default feedback file\n" );
    Instru_File_Name = Src_File_Name;
  }
  PU_PROFILE_HANDLES fb_handles; // empty for instrumentation
  EDGE_PROFILE edge_prof( cgrin,TRUE, phase, fb_handles );
  edge_prof.CG_Instrument( );
}
	
void CG_Edge_Profile_Annotation( CGRIN* cgrin,PROFILE_PHASE phase )
{
  // srcfile_pu_name consists of  the source file name ,"/", and the current
  // PU name. 
  char* srcfile_pu_name = CXX_NEW_ARRAY( char, strlen( Src_File_Name ) + 
              strlen( "/" ) + strlen( Cur_PU_Name ) + 1, &MEM_local_nz_pool );
	strcpy( srcfile_pu_name,Src_File_Name );
  strcat( srcfile_pu_name,"/" );
  strcat( srcfile_pu_name,Cur_PU_Name);

  FREQ_Compute_BB_Frequencies( );
  PU_PROFILE_HANDLES fb_handles
      = Get_CG_PU_Profile( srcfile_pu_name, Feedback_File_Info[phase] );
  if( fb_handles.empty( ) )
  {
    // if there is no feedback info, use static profile 
    DevWarn( "Cannot find expected feedback data - function not called?" );
    CG_PU_Has_Feedback = FALSE;
    FREQ_Compute_BB_Frequencies( );
    Compute_Edge_Freq_Base_Prob( );
    return;
  }

  FREQ_freqs_computed = TRUE;
  CG_PU_Has_Feedback = TRUE;

  edge_done= TRUE;
  
  if( Get_Trace( TP_A_PROF,TT_PROF_FEEDBACK_DUMP ) )
  {
    Dump_Fb_Data( fb_handles );
    DevWarn( "find expected feedback data!" );
  }
  Init_CFG( );			 	
  EDGE_PROFILE edge_prof( cgrin,FALSE, phase, fb_handles );
  edge_prof.CG_Annotate( );
}

void EDGE_PROFILE::CG_Instrument( )
{
  // if PU has only BB and BB's xfer op just is ret, do not 
  // instrument
  int BB_Count;
  if ( ( Curr_PU_BB_Count( ) <= 1 ) 
      && ( ( BB_xfer_op( _pu_first_bb ) == NULL )
      ||( OP_code( BB_xfer_op( _pu_first_bb ) ) == TOP_br_ret )
      ||( OP_code( BB_xfer_op( _pu_first_bb ) ) == TOP_brp_ret ) ) )
    return;  
  if( Get_Trace( TP_A_PROF,TT_PROF_CFG ) )
    draw_global_cfg( "before edge profile" );    
  Prepare_Instrument( );
 
  // this is the main function for doing instrumentation.
  // It instruments in a PU.
  _instrument_count = 0;
  // instrument at each bb branch 
  for( BB* bb =_pu_last_bb; bb != NULL; 
        bb = BB_prev( bb ) )
  {
    if(BB_Is_For_Instru(bb))
      CG_Instrument_Node( bb );
  } 	
  
  Instrument_Entry( );
  if( Get_Trace( TP_A_PROF,TT_PROF_CFG ) )
  {
    FREQ_Check_Consistency( "edge profile instrument" );
    draw_global_cfg( "after edge profile" ); 
  }  
}

// ------------------------------------------------------------------
// Instrument before the PU's entry. Instrument BBs that call two 
// functions "__profile_init" and "__profile_pu_init"
// ------------------------------------------------------------------
void EDGE_PROFILE::Instrument_Entry( )
{
  char* outputfile_name = Instru_File_Name;
  PU_Has_Calls = TRUE;
  BB* bb = _pu_first_bb;
  BB_LIST *elist;
  for ( elist = Entry_BB_Head; elist != NULL; elist = BB_LIST_rest(elist) ) 
  {
    bb = BB_LIST_first(elist);
    if ( bb != NULL && !BB_call( bb ) )
    {
      op* op = BB_xfer_op( bb );
      if ( op != NULL )
      {
        BBLIST* edge = BB_succs( bb );
        INT32 bblist_len =BBlist_Len( edge );
        while ( edge != NULL && bblist_len-- )
        {
          BBLIST* nedge = edge;
          edge = BBLIST_next( edge );

          // generate two bb that call instrumentation lib function
          BB* target_bb = BBLIST_item( nedge );
          BB* instru_init_call_bb = Gen_Call_BB( INSTR_INIT_NAME,
                                      outputfile_name,_phase,0 );
          Set_BB_profile_added(instru_init_call_bb);
          BB* instru_pu_init_call_bb = Gen_Call_BB( PU_INIT_NAME,
                            _srcfile_pu_name,_instrument_count,0 );
          Set_BB_profile_added(instru_pu_init_call_bb);
          LABEL_IDX instru_bb_label = Gen_Label_For_BB( instru_init_call_bb );

          // tgt_label is the branch target bb's label
          LABEL_IDX tgt_label;
          if ( Get_Label_BB( Get_Br_Op_Tgt_Label( op ) ) == target_bb )
            tgt_label = Get_Br_Op_Tgt_Label( op );
          else
            tgt_label = Gen_Label_For_BB( target_bb );
  
          if( BB_next( bb ) == target_bb )
          {
            Insert_BB( instru_init_call_bb,bb );
            Insert_BB( instru_pu_init_call_bb,instru_init_call_bb );
          
            // Update CFG  after and before the region formation
            if (IPFEC_Enable_Region_Formation && 
                 _phase != PROFILE_PHASE_BEFORE_REGION)
            {
              REGIONAL_CFG_NODE* node = Regional_Cfg_Node(bb);
              REGION* reg = node->Home_Region();
              REGIONAL_CFG* regional_cfg = reg->Regional_Cfg();
            
              float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
              float original_prob = Prob_Local(bb,target_bb);
              RGN_Gen_And_Insert_Node(instru_init_call_bb,bb,
                                                 target_bb,regional_cfg);
              RGN_Gen_And_Insert_Node(instru_pu_init_call_bb,
                             instru_init_call_bb,target_bb,regional_cfg);
              RGN_Unlink_Pred_Succ( bb,target_bb,regional_cfg );
              RGN_Link_Pred_Succ_With_Prob( bb,instru_init_call_bb,
                                            original_prob,regional_cfg );
              RGN_Link_Pred_Succ_With_Prob( instru_init_call_bb,
                          instru_pu_init_call_bb,1.0,regional_cfg);
              RGN_Link_Pred_Succ_With_Prob( instru_pu_init_call_bb,target_bb,
                                                  1.0 );

              BB_freq( instru_init_call_bb ) = original_freq;
              BB_freq( instru_pu_init_call_bb ) = original_freq;              
            }else
            { 
              float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
              float original_prob = Prob_Local(bb,target_bb);
              Unlink_Pred_Succ( bb,target_bb );
              Link_Pred_Succ( bb,instru_init_call_bb );
              Link_Pred_Succ( instru_init_call_bb,instru_pu_init_call_bb );
              Link_Pred_Succ( instru_pu_init_call_bb,target_bb );

              BB_freq( instru_init_call_bb ) = original_freq;
              BB_freq( instru_pu_init_call_bb ) = original_freq;
              Set_Prob( bb,instru_init_call_bb,original_prob );
              Set_Prob( instru_init_call_bb,instru_pu_init_call_bb, 1.0);
              Set_Prob( instru_pu_init_call_bb,target_bb,1.0 );                     
            }  
          }else
          {
            BB* goto_bb = Gen_BB( );
            Set_BB_profile_added(goto_bb);
            BB* old_last_bb = Get_Pu_Last_BB( );
            Set_Pu_Last_BB( goto_bb );
 
            Insert_BB( instru_init_call_bb,old_last_bb );
            Insert_BB( instru_pu_init_call_bb,instru_init_call_bb );
            Insert_BB( goto_bb,instru_pu_init_call_bb );
            Add_Goto( goto_bb, target_bb );

            // Update CFG
            if (IPFEC_Enable_Region_Formation && 
                 _phase != PROFILE_PHASE_BEFORE_REGION)
            { 
              REGIONAL_CFG_NODE* node = Regional_Cfg_Node(bb);
              REGION* reg = node->Home_Region();
              REGIONAL_CFG* regional_cfg = reg->Regional_Cfg();
            
              float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
              float original_prob = Prob_Local(bb,target_bb);
            
              RGN_Gen_And_Insert_Node(instru_init_call_bb,bb,
                                                 target_bb,regional_cfg);
              RGN_Gen_And_Insert_Node(instru_pu_init_call_bb,
                               instru_init_call_bb,target_bb,regional_cfg);
              RGN_Gen_And_Insert_Node(goto_bb,instru_pu_init_call_bb,
                                                 target_bb,regional_cfg);
              RGN_Unlink_Pred_Succ( bb,target_bb );
              RGN_Link_Pred_Succ_With_Prob( bb,instru_init_call_bb,
                                     original_prob, regional_cfg );
              RGN_Link_Pred_Succ_With_Prob( instru_init_call_bb,
                                  instru_pu_init_call_bb, 1.0,regional_cfg );
              RGN_Link_Pred_Succ_With_Prob( instru_pu_init_call_bb,goto_bb,1.0,
                                                               regional_cfg );
              RGN_Link_Pred_Succ_With_Prob( goto_bb,target_bb,1.0 );

              BB_freq( instru_init_call_bb ) = original_freq;
              BB_freq( instru_pu_init_call_bb ) = original_freq;
              BB_freq( goto_bb ) = original_freq;

            }else
            {
              float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
              float original_prob = Prob_Local(bb,target_bb);
              Unlink_Pred_Succ( bb,target_bb );
              Link_Pred_Succ( bb,instru_init_call_bb );
              Link_Pred_Succ( instru_init_call_bb,instru_pu_init_call_bb );
              Link_Pred_Succ( instru_pu_init_call_bb,goto_bb );
              Link_Pred_Succ( goto_bb,target_bb );

              BB_freq( instru_init_call_bb ) = original_freq;
              BB_freq( instru_pu_init_call_bb ) = original_freq;
              BB_freq( goto_bb ) = original_freq;
              Set_Prob( bb,instru_init_call_bb,original_prob );
              Set_Prob( instru_init_call_bb,instru_pu_init_call_bb, 1.0 );
              Set_Prob( instru_pu_init_call_bb,goto_bb, 1.0 );
              Set_Prob( goto_bb,target_bb,1.0 );            
           }
          // TODO:check and use BB_branch_op or BB_xfer_op
          Change_Tgt_Label( op,tgt_label,instru_bb_label );
         }
        Prepare_Call_Init_Return_BB(BB_next(instru_pu_init_call_bb));
       }
      }else
      {
        BB* instru_init_call_bb = Gen_Call_BB( INSTR_INIT_NAME,
                                            outputfile_name,_phase,0 );
        Set_BB_profile_added(instru_init_call_bb);
        BB* instru_pu_init_call_bb = Gen_Call_BB( PU_INIT_NAME,
                                 _srcfile_pu_name, _instrument_count,0 );
        Set_BB_profile_added(instru_pu_init_call_bb);
        BB* target_bb = BBLIST_item( BB_succs( bb ) );
        Insert_BB( instru_init_call_bb,bb );
        Insert_BB( instru_pu_init_call_bb,instru_init_call_bb );
      
        if (IPFEC_Enable_Region_Formation && 
             _phase != PROFILE_PHASE_BEFORE_REGION)
        {
          REGIONAL_CFG_NODE* node = Regional_Cfg_Node(bb);
          REGION* reg = node->Home_Region();
          REGIONAL_CFG* regional_cfg = reg->Regional_Cfg();
        
          float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
          float original_prob = Prob_Local(bb,target_bb);
          RGN_Gen_And_Insert_Node(instru_init_call_bb,bb,
                                             target_bb,regional_cfg);
          RGN_Gen_And_Insert_Node(instru_pu_init_call_bb,
                         instru_init_call_bb,target_bb,regional_cfg);
        
          RGN_Link_Pred_Succ_With_Prob( bb,instru_init_call_bb,
                                        original_prob,regional_cfg );
          RGN_Link_Pred_Succ_With_Prob( instru_init_call_bb,
                   instru_pu_init_call_bb,1.0,regional_cfg);
          RGN_Link_Pred_Succ_With_Prob( instru_pu_init_call_bb,target_bb,
                                              1.0);
                                              
          BB_freq( instru_init_call_bb ) = original_freq;
          BB_freq( instru_pu_init_call_bb ) = original_freq;              
        }else
        { 
          float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
          float original_prob = Prob_Local(bb,target_bb);
          Unlink_Pred_Succ( bb,target_bb );
          Link_Pred_Succ( bb,instru_init_call_bb );
          Link_Pred_Succ( instru_init_call_bb,instru_pu_init_call_bb );
          Link_Pred_Succ( instru_pu_init_call_bb,target_bb );
          BB_freq( instru_init_call_bb ) = original_freq;
          BB_freq( instru_pu_init_call_bb ) = original_freq;
          Set_Prob( bb,instru_init_call_bb,original_prob );
          Set_Prob( instru_init_call_bb,instru_pu_init_call_bb, 1.0);
          Set_Prob( instru_pu_init_call_bb,target_bb,1.0 );                     
        }  
       Prepare_Call_Init_Return_BB(BB_next(instru_pu_init_call_bb));
      }
    }else if ( bb != NULL && BB_call( bb ) )
    {
      BB* target_bb = BB_next( bb ); 
      if ( target_bb == NULL ) return;
  
      // Check whether the call function has return value. If it has, save 
      // and restore it
      BB* restore_bb = NULL;
      BB* instru_init_call_bb;
      int ret_reg_num = Get_Return_Reg_Sum( target_bb );
      int f_ret_reg_num = Get_Float_Return_Reg_Sum( target_bb );
      Is_True( ret_reg_num <= 0 || f_ret_reg_num <= 0 , 
            ( " return reg sum %d, float return reg sum %d" ,
                                ret_reg_num, f_ret_reg_num ) );
      if ( ret_reg_num > 0 )
      {
        restore_bb = Gen_BB( );
        Set_BB_profile_added(restore_bb);
        instru_init_call_bb = Gen_Call_BB( INSTR_INIT_NAME,
                               outputfile_name,_phase,1, ret_reg_num );
        Set_BB_profile_added(instru_init_call_bb);                          
        for ( int i = 0; i < ret_reg_num; i++ )
        {
          BB_Move_Op_To_Start( restore_bb,instru_init_call_bb,
                                        instru_init_call_bb->ops.last );
        }                               
      }else if ( f_ret_reg_num > 0 )
      {
        restore_bb = Gen_BB( );
        Set_BB_profile_added(restore_bb);
        instru_init_call_bb = Gen_Call_BB( INSTR_INIT_NAME,
                              outputfile_name,_phase,2, f_ret_reg_num );
        Set_BB_profile_added(instru_init_call_bb);
        for ( int i = 0; i < f_ret_reg_num; i++ )
        {
          BB_Move_Op_To_Start( restore_bb,instru_init_call_bb,
                                          instru_init_call_bb->ops.last );
        }                                          
      }else
      {
        instru_init_call_bb = Gen_Call_BB( INSTR_INIT_NAME,
                                                outputfile_name,_phase,0 );
        Set_BB_profile_added(instru_init_call_bb);
      }

      BB* instru_pu_init_call_bb = Gen_Call_BB( PU_INIT_NAME,
                               _srcfile_pu_name, _instrument_count,FALSE );
      Set_BB_profile_added(instru_pu_init_call_bb);
      Insert_BB( instru_init_call_bb,bb );
      Insert_BB( instru_pu_init_call_bb,instru_init_call_bb );
      if ( restore_bb != NULL )
        Insert_BB( restore_bb,instru_pu_init_call_bb );
      
      // update cfg 
      if (IPFEC_Enable_Region_Formation && 
           _phase != PROFILE_PHASE_BEFORE_REGION)
      {
        REGIONAL_CFG_NODE* node = Regional_Cfg_Node(bb);
        REGION* reg = node->Home_Region();
        REGIONAL_CFG* regional_cfg = reg->Regional_Cfg();
        
        float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
        float original_prob = Prob_Local(bb,target_bb);
        RGN_Gen_And_Insert_Node(instru_init_call_bb,bb,target_bb,regional_cfg);
        RGN_Gen_And_Insert_Node(instru_pu_init_call_bb,instru_init_call_bb,
                                                     target_bb,regional_cfg);
        RGN_Unlink_Pred_Succ( bb,target_bb );
        RGN_Link_Pred_Succ_With_Prob( bb,instru_init_call_bb,
                          original_prob,regional_cfg );
        RGN_Link_Pred_Succ_With_Prob( instru_init_call_bb,
                       instru_pu_init_call_bb, 1.0 );
        BB_freq( instru_init_call_bb ) = original_freq;
        BB_freq( instru_pu_init_call_bb ) = original_freq;

        if ( restore_bb != NULL )
        {
          RGN_Gen_And_Insert_Node(restore_bb,instru_pu_init_call_bb,
                   target_bb,regional_cfg);
          RGN_Link_Pred_Succ_With_Prob( instru_pu_init_call_bb,restore_bb,
                                            1.0,regional_cfg );
          RGN_Link_Pred_Succ_With_Prob( restore_bb,target_bb,original_prob );
          BB_freq( restore_bb ) = original_freq;
        }else
        {
          RGN_Link_Pred_Succ_With_Prob( instru_pu_init_call_bb,target_bb,
                                                              1.0 );
        }      
      }else
      {
        float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
        float original_prob = Prob_Local(bb,target_bb);
        Unlink_Pred_Succ( bb,target_bb );
        Link_Pred_Succ( bb,instru_init_call_bb );
        Link_Pred_Succ( instru_init_call_bb,instru_pu_init_call_bb );

        BB_freq( instru_init_call_bb ) = original_freq;
        BB_freq( instru_pu_init_call_bb ) = original_freq;
        Set_Prob( bb,instru_init_call_bb,original_prob );
        Set_Prob( instru_init_call_bb,instru_pu_init_call_bb,1.0 );

        if ( restore_bb != NULL )
        {
          Link_Pred_Succ( instru_pu_init_call_bb,restore_bb );
          Link_Pred_Succ( restore_bb,target_bb );
          BB_freq( restore_bb ) = original_freq;
          Set_Prob( instru_pu_init_call_bb,restore_bb,1.0 );
          Set_Prob( restore_bb,target_bb,1.0 );
        }else
        {
          Link_Pred_Succ( instru_pu_init_call_bb,target_bb );
          Set_Prob( instru_pu_init_call_bb,target_bb ,1.0);
        }
      }
      Prepare_Call_Init_Return_BB(BB_next(instru_pu_init_call_bb));
    }
  }
}
// ------------------------------------------------------------------
// perform the annotatation
// ------------------------------------------------------------------
void EDGE_PROFILE::CG_Annotate( )
{
  // This is the main function by which annotation is completed 
  // it annotations in a PU.
  if( Get_Trace( TP_A_PROF,TT_PROF_CFG ) )
    draw_global_cfg( "before edge profile annot" ); 
  INT32 _edge_instrument_count = 0;

  for( BB* bb = _pu_last_bb; bb != NULL;
        bb = BB_prev( bb ) )
  {
    if(BB_Is_For_Instru(bb))
    {
      Set_BB_freq_fb_based(bb);
      CG_Annotate_Node( bb );
    }
  } 

   if( Get_Trace( TP_A_PROF,TT_PROF_CFG ) )
     draw_global_cfg( "before edge profile propagate" ); 

   Propagate( );
   if( Get_Trace( TP_A_PROF,TT_PROF_CFG ) )
     draw_global_cfg( "before compute edge prob" ); 

   Compute_Edge_Prob_Base_Freq( );
    
   Check_CFG_Consistency( "edge profile annotation" );
   if( Get_Trace( TP_A_PROF,TT_PROF_CFG ) )
     draw_global_cfg( "after edge profile annot" ); 
}

// ------------------------------------------------------------------
// perform the instrumentation in a BB node
// ------------------------------------------------------------------
void EDGE_PROFILE::CG_Instrument_Node( BB *bb )
{
  // get the branch op of bb
  op* op = BB_xfer_op( bb );
  if ( op != NULL )
  {
    switch ( OP_code( op ) )
    {
      // TODO: find all kind of branch op 
      case  TOP_br_cond:
      case  TOP_br_r_cond:
      case  TOP_br:
        Instrument_Ip_Rel_Branch( bb );
        break;

      case  TOP_br_cloop:
        Instrument_Cloop_Branch( bb );
        break;

      case  TOP_br_ctop:
      case  TOP_br_wtop:
        Instrument_Top_Branch( bb );
        break;

      case TOP_brp_r:
      case TOP_br_r:
        Instrument_Indirect_Cond_Branch( bb );
        break;

      default:
        Instrument_None_and_Other_Xfer( bb );
        break;
    }
  }else
    Instrument_None_and_Other_Xfer( bb );
}

// ------------------------------------------------------------------
// perform the annotatation in a BB node
// ------------------------------------------------------------------
void EDGE_PROFILE::CG_Annotate_Node( BB *bb )
{
  op* op = BB_xfer_op( bb );
  if ( op != NULL )
  {
    switch ( OP_code( op ) )
    {
      // TODO: find all kind of branch op 
      case  TOP_br_cond:
      case  TOP_br_r_cond:
      case  TOP_br:
        Annotate_Ip_Rel_Branch( bb );
        break;

      case  TOP_br_cloop:
        Annotate_Cloop_Branch( bb );
        break;

      case  TOP_br_ctop:
      case  TOP_br_wtop:
        Annotate_Top_Branch( bb );
        break;

      case TOP_brp_r:
      case TOP_br_r:
        Annotate_Indirect_Cond_Branch( bb );
        break;

      default:
       Annotate_None_and_Other_Xfer( bb );
       break;
    }
  }else
    Annotate_None_and_Other_Xfer( bb );
}

void EDGE_PROFILE::Instrument_Indirect_Cond_Branch( BB* bb )
{
  BBLIST* edge = BB_succs( bb );
  INT32 bblist_len =BBlist_Len( edge );
  OP* op = BB_xfer_op( bb );
  while ( edge != NULL && bblist_len-- )
  {
    BBLIST* nedge = edge;
    edge = BBLIST_next( edge );

    BB* target_bb = BBLIST_item( nedge );
    if ( BB_profile_added(target_bb) )
    	continue;

    INT32 id = _instrument_count++;
    ++_count_indirect_cond_branch;
    
    BB* instru_bb = Gen_Instru_Call_BB( EDGE_INST_NAME,_srcfile_pu_name,id );
    Set_BB_profile_added(instru_bb);
                    
    // Generate a label for new BB
    LABEL_IDX instru_bb_label = Gen_Label_For_BB( instru_bb );
    LABEL_IDX tgt_label;
    if ( Get_Br_Op_Tgt_Label( op ) != LABEL_IDX_NONE &&
           Get_Label_BB( Get_Br_Op_Tgt_Label( op ) ) == target_bb )
        tgt_label = Get_Br_Op_Tgt_Label( op );
    else
        tgt_label = Gen_Label_For_BB( target_bb );
         
    Change_BB_Label( instru_bb,tgt_label );
    Change_BB_Label( target_bb,instru_bb_label );
      
    if( BB_next( bb ) == target_bb )
    {
      Insert_BB( instru_bb,bb );
      
      // Update CFG
      if (IPFEC_Enable_Region_Formation && 
            _phase != PROFILE_PHASE_BEFORE_REGION)
      {
        REGIONAL_CFG_NODE* node = Regional_Cfg_Node(bb);
        REGION* reg = node->Home_Region();
        REGIONAL_CFG* regional_cfg = reg->Regional_Cfg();
        
        float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
        float original_prob = Prob_Local(bb,target_bb);
        
        RGN_Gen_And_Insert_Node(instru_bb,bb,target_bb,regional_cfg);
        RGN_Unlink_Pred_Succ( bb,target_bb );
        RGN_Link_Pred_Succ_With_Prob( bb,instru_bb,original_prob,regional_cfg );
        RGN_Link_Pred_Succ_With_Prob( instru_bb,target_bb,original_prob );
        BB_freq( instru_bb ) = original_freq;              
        
      }else
      {
        float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
        float original_prob = Prob_Local(bb,target_bb);
        Unlink_Pred_Succ( bb,target_bb );
        Link_Pred_Succ( bb,instru_bb );
        Link_Pred_Succ( instru_bb,target_bb );
        BB_freq( instru_bb ) = original_freq;
        Set_Prob( bb,instru_bb,original_prob );
        Set_Prob( instru_bb,target_bb,1.0 );
      }   
    }else
    {
      BB* goto_bb = Gen_BB( );
      Set_BB_profile_added(goto_bb);
      BB* old_last_bb = _pu_last_bb;
      _pu_last_bb = goto_bb;
      Insert_BB( instru_bb,old_last_bb );
      Insert_BB( goto_bb,instru_bb );
      Add_Goto( goto_bb, target_bb );
      
      // Update CFG
      if (IPFEC_Enable_Region_Formation && 
            _phase != PROFILE_PHASE_BEFORE_REGION)
      {
        REGIONAL_CFG_NODE* node = Regional_Cfg_Node(bb);
        REGION* reg = node->Home_Region();
        REGIONAL_CFG* regional_cfg = reg->Regional_Cfg();
        
        float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
        float original_prob = Prob_Local(bb,target_bb);
        
        RGN_Gen_And_Insert_Node(instru_bb,bb,target_bb,regional_cfg);
        RGN_Gen_And_Insert_Node(goto_bb,bb,instru_bb,regional_cfg);
        RGN_Unlink_Pred_Succ( bb,target_bb );
        RGN_Link_Pred_Succ_With_Prob( bb,instru_bb,original_prob,regional_cfg );
        RGN_Link_Pred_Succ_With_Prob( instru_bb,goto_bb,original_prob,regional_cfg );
        RGN_Link_Pred_Succ_With_Prob( goto_bb,target_bb,original_prob ); 
        BB_freq( instru_bb ) = original_freq;
        BB_freq( goto_bb ) = original_freq;
      }else
      {
        float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
        float original_prob = Prob_Local(bb,target_bb);
        Unlink_Pred_Succ( bb,target_bb );
        Link_Pred_Succ( bb,instru_bb );
        Link_Pred_Succ( instru_bb,goto_bb );
        Link_Pred_Succ( goto_bb,target_bb ); 
        BB_freq( instru_bb ) = original_freq;
        BB_freq( goto_bb ) = original_freq;
        Set_Prob( bb,instru_bb,original_prob );
        Set_Prob( instru_bb,goto_bb,1.0 );
        Set_Prob( goto_bb,target_bb,1.0 );      
     }
    // change jump target label
    OP *branch_op = BB_branch_op( bb );
    Change_Tgt_Label( branch_op,tgt_label,instru_bb_label );
  }
 }
}

void EDGE_PROFILE::Instrument_Ip_Rel_Branch( BB *bb )
{
  BBLIST* edge = BB_succs( bb );
  INT32 bblist_len =BBlist_Len( edge );
  OP* op = BB_xfer_op( bb );
  while ( edge != NULL && bblist_len-- )
  {
    BBLIST* nedge = edge;
    edge = BBLIST_next( edge );
    BB* target_bb = BBLIST_item( nedge );
    if ( BB_profile_added(target_bb) )
    	continue;
    LABEL_IDX tgt_label;
    if ( Get_Label_BB( Get_Br_Op_Tgt_Label( op ) ) == target_bb )
        tgt_label = Get_Br_Op_Tgt_Label( op );
    else
        tgt_label = Gen_Label_For_BB( target_bb );
        
    if( BB_next( bb ) == target_bb )
    { 
      INT32 id = _instrument_count++;
      BB* instru_bb = Gen_Instru_Call_BB( EDGE_INST_NAME,
                                          _srcfile_pu_name,id );
      Set_BB_profile_added(instru_bb);
      // Generate a label for new BB
      LABEL_IDX instru_bb_label = Gen_Label_For_BB( instru_bb );
      Insert_BB( instru_bb,bb );
    
      // Update CFG
      if (IPFEC_Enable_Region_Formation && 
            _phase != PROFILE_PHASE_BEFORE_REGION)
      {
        REGIONAL_CFG_NODE* node = Regional_Cfg_Node( target_bb );
        REGION* reg = node->Home_Region();
        REGIONAL_CFG* regional_cfg = reg->Regional_Cfg();        
          
        float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
        float original_prob = Prob_Local(bb,target_bb);
        RGN_Gen_And_Insert_Node(instru_bb,bb,target_bb,regional_cfg);
        RGN_Unlink_Pred_Succ( bb,target_bb );
        RGN_Link_Pred_Succ_With_Prob( bb,instru_bb,original_prob,regional_cfg );
        RGN_Link_Pred_Succ_With_Prob( instru_bb,target_bb,1.0 );
        BB_freq( instru_bb ) = original_freq;          
      }else
      {  
         float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
         float original_prob = Prob_Local(bb,target_bb);
         Unlink_Pred_Succ( bb,target_bb );
         Link_Pred_Succ( bb,instru_bb );
         Link_Pred_Succ( instru_bb,target_bb );
         BB_freq( instru_bb ) = original_freq;
         Set_Prob( bb,instru_bb,original_prob );
         Set_Prob( instru_bb,target_bb,1.0 );
      }  
 
      OP *branch_op = BB_branch_op( bb );
      if ( tgt_label == Get_Op_Label( branch_op ) )
        Change_Tgt_Label( branch_op,tgt_label,instru_bb_label );
      }else
      { 
        INT32 id = _instrument_count++;
        BB* instru_bb = Gen_Instru_Call_BB( EDGE_INST_NAME,
                                            _srcfile_pu_name,id );
        Set_BB_profile_added(instru_bb);
        // Generate a label for new BB
        LABEL_IDX instru_bb_label = Gen_Label_For_BB( instru_bb );

        BB* goto_bb = Gen_BB( );
        Set_BB_profile_added(goto_bb);
        BB* old_last_bb = _pu_last_bb;
        _pu_last_bb = goto_bb;
        Insert_BB( instru_bb,old_last_bb );
        Insert_BB( goto_bb,instru_bb );
        Add_Goto( goto_bb, target_bb );
      
        // Update CFG
        if (IPFEC_Enable_Region_Formation && 
              _phase != PROFILE_PHASE_BEFORE_REGION)
        {
          REGIONAL_CFG_NODE* node = Regional_Cfg_Node(bb);
          REGION* reg = node->Home_Region();
          REGIONAL_CFG* regional_cfg = reg->Regional_Cfg();
        
          float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
          float original_prob = Prob_Local(bb,target_bb);
         
          RGN_Gen_And_Insert_Node(instru_bb,bb,target_bb,regional_cfg);
          RGN_Gen_And_Insert_Node(goto_bb,instru_bb,target_bb,regional_cfg);
          RGN_Unlink_Pred_Succ( bb,target_bb );
          RGN_Link_Pred_Succ_With_Prob( bb,instru_bb,original_prob,regional_cfg );
          RGN_Link_Pred_Succ_With_Prob( instru_bb,goto_bb,1.0,regional_cfg );
          RGN_Link_Pred_Succ_With_Prob( goto_bb,target_bb,1.0 ); 
          BB_freq( instru_bb ) = original_freq;
          BB_freq( goto_bb ) = original_freq;
        }else
        {
          float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
          float original_prob = Prob_Local(bb,target_bb);
          Unlink_Pred_Succ( bb,target_bb );
          Link_Pred_Succ( bb,instru_bb );
          Link_Pred_Succ( instru_bb,goto_bb );
          Link_Pred_Succ( goto_bb,target_bb ); 
          BB_freq( instru_bb ) = original_freq;
          BB_freq( goto_bb ) = original_freq;
          Set_Prob( bb,instru_bb,original_prob );
          Set_Prob( instru_bb,goto_bb,1.0 );
          Set_Prob( goto_bb,target_bb,1.0 );      
        }   

        OP *branch_op = BB_branch_op( bb );
        if ( tgt_label == Get_Op_Label( branch_op ) )
           Change_Tgt_Label( branch_op,tgt_label,instru_bb_label ); 
     }
  }
}

void EDGE_PROFILE::Instrument_Cloop_Branch( BB *bb )
{
  BBLIST* edge = BB_succs( bb );
  INT32 bblist_len =BBlist_Len( edge );
  OP* op = BB_xfer_op( bb );
  while ( edge != NULL && bblist_len-- )
  {
    BBLIST* nedge = edge;
    edge = BBLIST_next( edge );

    BB* target_bb = BBLIST_item( nedge );

     if ( BB_profile_added(target_bb) )
    	continue;

    INT32 id = _instrument_count++;
    ++_count_cloop;

    BB* instru_bb = Gen_Instru_Call_BB( EDGE_INST_NAME,_srcfile_pu_name,id );
    Set_BB_profile_added(instru_bb);
    // Generate a label for new BB
    LABEL_IDX instru_bb_label = Gen_Label_For_BB( instru_bb );

    LABEL_IDX tgt_label;
    
    if( Get_Label_BB( Get_Br_Op_Tgt_Label( op ) ) == target_bb )
       tgt_label = Get_Br_Op_Tgt_Label( op );
    else
       tgt_label = Gen_Label_For_BB( target_bb );
  
    // TODO:juadge the target_BB is not out of the PU
    if( BB_next( bb ) == target_bb )
    {
      Insert_BB( instru_bb,bb );
      // Update CFG
      if (IPFEC_Enable_Region_Formation && 
            _phase != PROFILE_PHASE_BEFORE_REGION)
      {
        REGIONAL_CFG_NODE* node = Regional_Cfg_Node(bb);
        REGION* reg = node->Home_Region();
        REGIONAL_CFG* regional_cfg = reg->Regional_Cfg();        
        
        float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
        float original_prob = Prob_Local(bb,target_bb);
        RGN_Gen_And_Insert_Node(instru_bb,bb,target_bb,regional_cfg);
        RGN_Unlink_Pred_Succ( bb,target_bb );
        RGN_Link_Pred_Succ_With_Prob( bb,instru_bb,original_prob,regional_cfg );
        RGN_Link_Pred_Succ_With_Prob( instru_bb,target_bb,1.0 );
        BB_freq( instru_bb ) = original_freq;          
      }else
      {  
        float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
        float original_prob = Prob_Local(bb,target_bb);
        Unlink_Pred_Succ( bb,target_bb );
        Link_Pred_Succ( bb,instru_bb );
        Link_Pred_Succ( instru_bb,target_bb );
        BB_freq( instru_bb ) = original_freq;
        Set_Prob( bb,instru_bb,original_prob );
        Set_Prob( instru_bb,target_bb,1.0 );
      } 
    }else
    {
      BB* goto_bb = Gen_BB( );
      Set_BB_profile_added(goto_bb);
      BB* old_last_bb = _pu_last_bb;
      _pu_last_bb = goto_bb;
      Insert_BB( instru_bb,old_last_bb );
      Insert_BB( goto_bb,instru_bb );
      Add_Goto( goto_bb, target_bb );
      
      // Update CFG
      if (IPFEC_Enable_Region_Formation && 
            _phase != PROFILE_PHASE_BEFORE_REGION)
      {
        REGIONAL_CFG_NODE* node = Regional_Cfg_Node(bb);
        REGION* reg = node->Home_Region();
        REGIONAL_CFG* regional_cfg = reg->Regional_Cfg();
      
        float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
        float original_prob = Prob_Local(bb,target_bb);
       
        RGN_Gen_And_Insert_Node(instru_bb,bb,target_bb,regional_cfg);
        RGN_Gen_And_Insert_Node(goto_bb,bb,instru_bb,regional_cfg);
        RGN_Unlink_Pred_Succ( bb,target_bb );
        RGN_Link_Pred_Succ_With_Prob( bb,instru_bb,original_prob,regional_cfg );
        RGN_Link_Pred_Succ_With_Prob( instru_bb,goto_bb,1.0,regional_cfg );
        RGN_Link_Pred_Succ_With_Prob( goto_bb,target_bb,1.0 ); 
        BB_freq( instru_bb ) = original_freq;
        BB_freq( goto_bb ) = original_freq;
      }else
      {
        float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
        float original_prob = Prob_Local(bb,target_bb);
        Unlink_Pred_Succ( bb,target_bb );
        Link_Pred_Succ( bb,instru_bb );
        Link_Pred_Succ( instru_bb,goto_bb );
        Link_Pred_Succ( goto_bb,target_bb ); 
        BB_freq( instru_bb ) = original_freq;
        BB_freq( goto_bb ) = original_freq;
        Set_Prob( bb,instru_bb,original_prob );
        Set_Prob( instru_bb,goto_bb,1.0 );
        Set_Prob( goto_bb,target_bb,1.0 );      
      }         
    }

    // Change jump target label
    OP *branch_op = BB_branch_op( bb );
    Change_Tgt_Label( branch_op,tgt_label,instru_bb_label );
  }
}

void EDGE_PROFILE::Instrument_Top_Branch( BB *bb )
{
  BBLIST* edge = BB_succs( bb );
  INT32 bblist_len =BBlist_Len( edge );
  OP* op = BB_xfer_op( bb );
  while ( edge != NULL && bblist_len-- )
  {
    BBLIST* nedge = edge;
    edge = BBLIST_next( edge );

    
    BB* target_bb = BBLIST_item( nedge );

    if ( BB_profile_added(target_bb) )
    	continue;
  
    INT32 id = _instrument_count++;
    ++_count_top;

    BB* instru_bb = Gen_Instru_Call_BB( EDGE_INST_NAME,_srcfile_pu_name,id );
    Set_BB_profile_added(instru_bb); 
    // Generate a label for new BB
    LABEL_IDX instru_bb_label = Gen_Label_For_BB( instru_bb );
    LABEL_IDX tgt_label;
    if ( Get_Label_BB( Get_Br_Op_Tgt_Label( op ) ) == target_bb )
       tgt_label = Get_Br_Op_Tgt_Label( op );
    else
       tgt_label = Gen_Label_For_BB( target_bb );
       
    // TODO:juadge the target_BB is not out of the PU
    if( BB_next( bb ) == target_bb )
    {
      Insert_BB( instru_bb,bb );
      // Update CFG
      if (IPFEC_Enable_Region_Formation && 
            _phase != PROFILE_PHASE_BEFORE_REGION)
      {
        REGIONAL_CFG_NODE* node = Regional_Cfg_Node(bb);
        REGION* reg = node->Home_Region();
        REGIONAL_CFG* regional_cfg = reg->Regional_Cfg();        
        
        float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
        float original_prob = Prob_Local(bb,target_bb);
        RGN_Gen_And_Insert_Node(instru_bb,bb,target_bb,regional_cfg);
        RGN_Unlink_Pred_Succ( bb,target_bb );
        RGN_Link_Pred_Succ_With_Prob( bb,instru_bb,original_prob,regional_cfg );
        RGN_Link_Pred_Succ_With_Prob( instru_bb,target_bb,original_prob );
        BB_freq( instru_bb ) = original_freq;          
      }else
      {  
        float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
        float original_prob = Prob_Local(bb,target_bb);
        Unlink_Pred_Succ( bb,target_bb );
        Link_Pred_Succ( bb,instru_bb );
        Link_Pred_Succ( instru_bb,target_bb );
        BB_freq( instru_bb ) = original_freq;
        Set_Prob( bb,instru_bb,original_prob );
        Set_Prob( instru_bb,target_bb,1.0 );
      } 
    }else
    {
      BB* goto_bb = Gen_BB( );
      Set_BB_profile_added(goto_bb);
      BB* old_last_bb = _pu_last_bb;
      _pu_last_bb = goto_bb;
      Insert_BB( instru_bb,old_last_bb );
      Insert_BB( goto_bb,instru_bb );
      Add_Goto( goto_bb, target_bb );
      
      // Update CFG
      if (IPFEC_Enable_Region_Formation && 
            _phase != PROFILE_PHASE_BEFORE_REGION)
      {
        REGIONAL_CFG_NODE* node = Regional_Cfg_Node(bb);
        REGION* reg = node->Home_Region();
        REGIONAL_CFG* regional_cfg = reg->Regional_Cfg();
      
        float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
        float original_prob = Prob_Local(bb,target_bb);
      
        RGN_Gen_And_Insert_Node(instru_bb,bb,target_bb,regional_cfg);
        RGN_Gen_And_Insert_Node(goto_bb,bb,instru_bb,regional_cfg);
        RGN_Unlink_Pred_Succ( bb,target_bb );
        RGN_Link_Pred_Succ_With_Prob( bb,instru_bb,original_prob,regional_cfg );
        RGN_Link_Pred_Succ_With_Prob( instru_bb,goto_bb,original_prob,regional_cfg );
        RGN_Link_Pred_Succ_With_Prob( goto_bb,target_bb,original_prob ); 
        BB_freq( instru_bb ) = original_freq;
        BB_freq( goto_bb ) = original_freq;
      }else
      {
        float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
        float original_prob = Prob_Local(bb,target_bb);
        Unlink_Pred_Succ( bb,target_bb );
        Link_Pred_Succ( bb,instru_bb );
        Link_Pred_Succ( instru_bb,goto_bb );
        Link_Pred_Succ( goto_bb,target_bb ); 
        BB_freq( instru_bb ) = original_freq;
        BB_freq( goto_bb ) = original_freq;
        Set_Prob( bb,instru_bb,original_prob );
        Set_Prob( instru_bb,goto_bb,1.0 );
        Set_Prob( goto_bb,target_bb,1.0 );      
      }              
    }
    
    // TODO:check use BB_branch_op or BB_xfer_op
    OP *branch_op = BB_branch_op( bb );
    Change_Tgt_Label( branch_op,tgt_label,instru_bb_label );
  }
}

void EDGE_PROFILE::Instrument_None_and_Other_Xfer( BB *bb )
{
  
  Is_True( bb!=NULL,( "can not instrument after a NULL BB" ) );
  if( BB_succs_len( bb ) > 1 )
  {
    DevWarn( "BB %d does not have only succeed BB!",BB_id( bb ) );
    return;
  }
  if( BB_succs_len( bb ) == 0 )
    return;
  if ( BBLIST_item( BB_succs( bb ) ) != BB_next( bb ) )
  {
    DevWarn( "the next BB of BB %d is not succeed BB!",BB_id( bb ) );
    return;
  }

 if ( BB_profile_added(BB_next( bb )) )
	return;
  OP* branch_op = BB_xfer_op( bb );
  INT32 id = _instrument_count++;

  if ( BB_call( bb ) )
  {
    BB* bp;
    BB* target_bb = BB_next( bb );
    // Check whether the call function has return value. if has, save 
    // and restore it
    BB* restore_bb = NULL;
    int ret_reg_num = Get_Return_Reg_Sum( target_bb );
    int f_ret_reg_num = Get_Float_Return_Reg_Sum( target_bb );
    Is_True( ret_reg_num <= 0 || f_ret_reg_num <= 0 , 
          ( " return reg sum %d, float return reg sum %d" ,
                                ret_reg_num, f_ret_reg_num ) );
    if ( ret_reg_num > 0 )
    {
      restore_bb = Gen_BB( );
      Set_BB_profile_added(restore_bb);
      bp = Gen_Call_BB( EDGE_INST_NAME,_pu_hdr_return_tn, id, 1, ret_reg_num );
      Set_BB_profile_added(bp);                          
      for ( int i = 0; i < ret_reg_num; i++ )
      {
        BB_Move_Op_To_Start( restore_bb,bp,
                                      bp->ops.last );
      }                               
    }else if ( f_ret_reg_num > 0 )
    {
      restore_bb = Gen_BB( );
      Set_BB_profile_added(restore_bb);
      bp = Gen_Call_BB( EDGE_INST_NAME,_pu_hdr_return_tn, 
                                        id, 2, f_ret_reg_num );
      Set_BB_profile_added(bp);
      for ( int i = 0; i < f_ret_reg_num; i++ )
      {
        BB_Move_Op_To_Start( restore_bb,bp,
                                        bp->ops.last );
      }                                  
    }else
    {
      bp = Gen_Call_BB( EDGE_INST_NAME,_pu_hdr_return_tn, id, 0);
      Set_BB_profile_added(bp);
    }

    // Update CFG   
    Insert_BB( bp,bb );
    if ( restore_bb != NULL )
      Insert_BB( restore_bb,bp );

    if (IPFEC_Enable_Region_Formation && 
          _phase != PROFILE_PHASE_BEFORE_REGION)
    {
      REGIONAL_CFG_NODE* node = Regional_Cfg_Node(bb);
      REGION* reg = node->Home_Region();
      REGIONAL_CFG* regional_cfg = reg->Regional_Cfg(); 

      float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
      float original_prob = Prob_Local(bb,target_bb);
      RGN_Gen_And_Insert_Node(bp,bb,target_bb,regional_cfg);
      RGN_Unlink_Pred_Succ( bb,target_bb );
      RGN_Link_Pred_Succ_With_Prob( bb,bp,1.0,regional_cfg );  
      BB_freq( bp ) = original_freq;
      if ( restore_bb != NULL )
      {
        RGN_Gen_And_Insert_Node(restore_bb,bb,bp,regional_cfg);
        RGN_Link_Pred_Succ_With_Prob( bp,restore_bb,1.0,regional_cfg );
        RGN_Link_Pred_Succ_With_Prob( restore_bb,target_bb,1.0 );
        BB_freq( restore_bb ) = original_freq;
      }else
      {
        RGN_Link_Pred_Succ_With_Prob( bp,target_bb,1.0 );
      }      
    }else
    {  
      float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
      float original_prob = Prob_Local(bb,target_bb);
      Unlink_Pred_Succ( bb,target_bb );
      Link_Pred_Succ( bb,bp );  
      BB_freq( bp ) = original_freq;
      Set_Prob( bb,bp,original_prob );
      if ( restore_bb != NULL )
      {
        Link_Pred_Succ( bp,restore_bb );
        Link_Pred_Succ( restore_bb,target_bb );
        BB_freq( restore_bb ) = original_freq;
        Set_Prob( bp,restore_bb,1.0 );
        Set_Prob( restore_bb,target_bb,1.0 );
      }else
      {
        Link_Pred_Succ( bp,target_bb);
        Set_Prob( bp,target_bb,1.0 );
      }
    } 
  }else
  {
    BB* bp = Gen_Call_BB( EDGE_INST_NAME,_pu_hdr_return_tn, id,0 );
    Set_BB_profile_added(bp);
    Insert_BB( bp,bb );
    BB* succ = BBLIST_item( BB_succs( bb ) );
     
    //update cfg
    if (IPFEC_Enable_Region_Formation && 
          _phase != PROFILE_PHASE_BEFORE_REGION)
    {
      REGIONAL_CFG_NODE* node = Regional_Cfg_Node(bb);
      REGION* reg = node->Home_Region();
      REGIONAL_CFG* regional_cfg = reg->Regional_Cfg();
      
      float original_freq = Prob_Local(bb,succ) * BB_freq(bb);
      float original_prob = Prob_Local(bb,succ);
      RGN_Gen_And_Insert_Node(bp,bb,succ,regional_cfg);
      RGN_Unlink_Pred_Succ( bb,succ );
      RGN_Link_Pred_Succ_With_Prob( bb,bp,1.0,regional_cfg );  
      RGN_Link_Pred_Succ_With_Prob( bp,succ,1.0);
      BB_freq( bp ) = original_freq;
      
    }else
    {       
      float original_freq = Prob_Local(bb,succ) * BB_freq(bb);
      float original_prob = Prob_Local(bb,succ);
      Unlink_Pred_Succ( bb,succ );
      Link_Pred_Succ( bp,succ );
      Link_Pred_Succ( bb,bp );  
      BB_freq( bp ) = original_freq;
      Set_Prob( bp,succ,1.0 );
      Set_Prob( bb,bp,original_prob );
    }
  }
}

// ------------------------------------------------------------------
// Change the branch op's label from old_label to new_lable.
// If branch_op has not old_label, don't change it.
// ------------------------------------------------------------------
void EDGE_PROFILE::Change_Tgt_Label( op* branch_op,
                    LABEL_IDX old_label,LABEL_IDX new_label )
{
  
  INT tn_num = OP_opnds( branch_op );
  for( int i = 0; i<tn_num; i++ )
  {
    TN* tgt_tn = OP_opnd( branch_op, i );
    if( TN_is_label( tgt_tn ) && ( TN_label( tgt_tn ) == old_label ) )
    {
      Set_TN_label( tgt_tn, new_label );
      break;
    }
  }
}

void EDGE_PROFILE::Annotate_Indirect_Cond_Branch( BB *bb )
{ 
  for( BBLIST* edge = BB_succs( bb ); edge != NULL; 
                                        edge = BBLIST_next( edge ) )
  {
    INT32 id =  _instrument_count++;
    
    PU_PROFILE_HANDLES& handles = _fb_handles;
    FB_Info_Edge info_branch( FB_FREQ_ZERO );
    for ( PU_PROFILE_ITERATOR i( handles.begin( ) ); i != handles.end( );
           ++i ) 
    {
      FB_Info_Edge& info = Get_Edge_Profile ( *i, id ); 
      if ( &info != NULL )
        info_branch.freq_edge += info.freq_edge;
    }
    // attach info to edge
    BBLIST_freq( edge ) = info_branch.freq_edge.Value( );
    
    if( Get_Trace( TP_A_PROF,TT_PROF_FEEDBACK_DUMP ) )
      Dump_Edge_Info(bb,edge);
  } 
}

void EDGE_PROFILE::Annotate_Ip_Rel_Branch( BB *bb )
{
  for( BBLIST* edge = BB_succs( bb ); edge != NULL; 
                                          edge = BBLIST_next( edge ) )
  {
    BB* target_bb = BBLIST_item( edge );
    {
      INT32 id = _instrument_count++;

      PU_PROFILE_HANDLES& handles = FB_Handle( );
      FB_Info_Edge info_Ip_Rel( FB_FREQ_ZERO );
      for ( PU_PROFILE_ITERATOR i( handles.begin( ) ); i != handles.end( );
           ++i ) 
      {
        FB_Info_Edge& info = Get_Edge_Profile ( *i, id ); 
        if ( &info != NULL )
          info_Ip_Rel.freq_edge += info.freq_edge;
      } 
      // attach info to edge
      BBLIST_freq( edge ) = info_Ip_Rel.freq_edge.Value( );
      if( Get_Trace( TP_A_PROF,TT_PROF_FEEDBACK_DUMP ) )
        Dump_Edge_Info(bb,edge);
    }  
  } 
}

void EDGE_PROFILE::Annotate_Cloop_Branch( BB *bb )
{
  for( BBLIST* edge = BB_succs( bb ); edge != NULL; 
                                      edge = BBLIST_next( edge ) )
  {
    INT32 id = _instrument_count++;

    PU_PROFILE_HANDLES& handles = FB_Handle( );
    FB_Info_Edge info_loop( FB_FREQ_ZERO );
    for ( PU_PROFILE_ITERATOR i( handles.begin( ) ); i != handles.end( );
           ++i ) 
    {
      FB_Info_Edge& info = Get_Edge_Profile ( *i, id ); 
      if ( &info != NULL )
        info_loop.freq_edge += info.freq_edge;
    }
    // attach info to edge
    BBLIST_freq( edge ) = info_loop.freq_edge.Value( );
    if( Get_Trace( TP_A_PROF,TT_PROF_FEEDBACK_DUMP ) )
      Dump_Edge_Info(bb,edge);
  }
}

void EDGE_PROFILE::Annotate_Top_Branch( BB *bb )
{
  for( BBLIST* edge = BB_succs( bb ); edge != NULL; 
                                          edge = BBLIST_next( edge ) )
  {
    INT32 id = _instrument_count++;
    ++_count_top;

    PU_PROFILE_HANDLES& handles = FB_Handle( );
    FB_Info_Edge info_top( FB_FREQ_ZERO );
    for ( PU_PROFILE_ITERATOR i( handles.begin( ) ); i != handles.end( );
           ++i ) 
    {
      FB_Info_Edge& info = Get_Edge_Profile ( *i, id ); 
      if ( &info != NULL )
        info_top.freq_edge += info.freq_edge;
    }
    // attach info to edge
    BBLIST_freq( edge ) = info_top.freq_edge.Value( );
    if( Get_Trace( TP_A_PROF,TT_PROF_FEEDBACK_DUMP ) )
      Dump_Edge_Info(bb,edge);
  } 
}

void EDGE_PROFILE::Annotate_Call_Branch( BB *bb )
{ 
  INT32 id = _instrument_count++;
  ++_count_call;
  
  id = _instrument_count++;
  ++_count_call;

  PU_PROFILE_HANDLES& handles = FB_Handle( );
  FB_Info_Edge info_call( FB_FREQ_ZERO );
  for ( PU_PROFILE_ITERATOR i( handles.begin( ) ); 
      i != handles.end ( ); ++i ) 
  {
    FB_Info_Edge& info = Get_Edge_Profile( *i, id );
    if ( &info != NULL )
      info_call.freq_edge += info.freq_edge;
  }

  BBLIST* edge = BB_succs( bb );
  if( edge != NULL )
    BBLIST_freq( edge ) = info_call.freq_edge.Value( ); 
  if( Get_Trace( TP_A_PROF,TT_PROF_FEEDBACK_DUMP ) )
    Dump_Edge_Info(bb,edge);
}

void EDGE_PROFILE::Annotate_None_and_Other_Xfer( BB *bb )
{
  Is_True( bb!=NULL,( "can not annotate after a NULL BB" ) );
  if( BB_succs_len( bb ) > 1 )
  {
    DevWarn( "BB %d does not have only succeed BB!",BB_id( bb ) );
    return;
  }
  if( BB_succs( bb ) == NULL )
    return;
  
  OP* branch_op = BB_xfer_op( bb );
  INT32 id = _instrument_count++;

  PU_PROFILE_HANDLES& handles = FB_Handle( );
  FB_Info_Edge info_top( FB_FREQ_ZERO );
  for ( PU_PROFILE_ITERATOR i( handles.begin( ) ); i != handles.end( );
           ++i ) 
  {
    FB_Info_Edge& info = Get_Edge_Profile ( *i, id ); 
    if ( &info != NULL )
      info_top.freq_edge += info.freq_edge;
  }
  // attach info to edge
  BBLIST* edge = BB_succs( bb );
  if( edge !=NULL)
    BBLIST_freq( edge ) = info_top.freq_edge.Value( );
  
  if( Get_Trace( TP_A_PROF,TT_PROF_FEEDBACK_DUMP ) )
    Dump_Edge_Info(bb,edge);

}
void EDGE_PROFILE::Prepare_Call_Init_Return_BB(BB * bb)
{
    TN * return_reg_tn;
    OP * save_op;
    return_reg_tn = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, RETURN_REG, 8);
    save_op = Mk_OP(TOP_mov, _pu_hdr_return_tn, True_TN, return_reg_tn);
	if ( TN_is_register(_pu_hdr_return_tn) && TN_is_register(return_reg_tn) )
		Set_OP_copy(save_op);
    BB_Prepend_Op(bb, save_op);
}

BB* EDGE_PROFILE::Gen_Call_BB( char* function_name, char* str_arg, 
                    INT64 int_arg,int restore_type,int restore_sum )
{
  BB *bb=Gen_BB( );
  SRCPOS srcpos = REGION_First_BB->ops.last->srcpos;
  
  OP* restore_f_reg;
  OP* restore_reg;
  OPS* restore_reg_ops = OPS_Create( );
  OPS* restore_f_reg_ops = OPS_Create( );
  
  if ( restore_type == 1 )
  {
    for ( int i = 0; i < restore_sum; i++ )  
    {
      TN* save_reg_tn1 = Gen_Register_TN( ISA_REGISTER_CLASS_integer,8 );
      TN* return_result_reg = 
         Build_Dedicated_TN( ISA_REGISTER_CLASS_integer,RETURN_REG + i ,8 );
      Set_TN_is_global_reg( save_reg_tn1 );
      restore_reg = Mk_OP( TOP_mov,return_result_reg,True_TN,save_reg_tn1 );
      OPS_Insert_Op(restore_reg_ops,NULL,restore_reg,FALSE);
      OP* mov_op = Mk_OP( TOP_mov,save_reg_tn1,True_TN,return_result_reg );
      BB_Append_Op( bb,mov_op );
    }  
  }
  
  if ( restore_type == 2 )
  { 
    for ( int i = 0 ; i < restore_sum ; i++)
    {
      TN* save_reg_tn2 = Gen_Register_TN( ISA_REGISTER_CLASS_float,8 );
      TN* f_return_result_reg = Build_Dedicated_TN( 
                    ISA_REGISTER_CLASS_float,FLOAT_RETURN_REG + i, 0 );

      Set_TN_is_global_reg( save_reg_tn2 );
      restore_f_reg = Mk_OP( TOP_mov_f,f_return_result_reg,
                                                True_TN,save_reg_tn2 );
      OPS_Insert_Op( restore_f_reg_ops,NULL,restore_f_reg,FALSE );
      OP* mov_f_op = Mk_OP( TOP_mov_f,save_reg_tn2,
                                         True_TN,f_return_result_reg );
      BB_Append_Op( bb,mov_f_op );
    }  
  }

  // Step I: get gp value
  // The Generated opcode is like:
  //    GTN585( gp ) :- mov TN257( p0 ) GTN715 ; copy
  TN* save_gp_TN = NULL;
  save_gp_TN = Find_TN( TOP_mov,1,1,GP_TN,2 );
  if ( save_gp_TN != NULL )
  {
    OP* mov_op = Mk_OP( TOP_mov,GP_TN,True_TN,save_gp_TN );
    BB_Append_Op( bb,mov_op );
  }  
  
  // Step II:genereate arg 1 TN
  // The Generate opcode is like:
  //    TN727 :- addl TN257( p0 ) ( sym#ltoff22:.rodata+0 ) GTN585( gp ) ;		
  //    TN_FIRST_OUTPUT_REG( r127 ) :- ld8 TN257( p0 ) ( enum: ) ( enum: ) TN727 ;
  TN* tgt_TN1 = Gen_Register_TN( ISA_REGISTER_CLASS_integer,8 );
  // create the string symtal constant
  TCON tcon = Host_To_Targ_String ( MTYPE_STRING,str_arg,
                      strlen( str_arg )+1 );
  TY_IDX ty = MTYPE_To_TY( MTYPE_STRING );
  ST* st = Gen_String_Sym ( &tcon, ty, FALSE );
  Allocate_Object( st );  
  INT64 offset = 0;
  INT32 relocs = TN_RELOC_IA_LTOFF22;
  TN* var_name_tn = Gen_Symbol_TN ( st , offset, relocs );
  OP* addl_op = Mk_OP( TOP_addl,tgt_TN1,True_TN,var_name_tn,GP_TN );
  OP_srcpos( addl_op ) = srcpos;
  BB_Append_Op( bb,addl_op );
  
  TN * TN_FIRST_OUTPUT_REG = Build_Dedicated_TN( 
                    ISA_REGISTER_CLASS_integer,FIRST_OUTPUT_REG,8 );
  TN* enum_ldtype = Gen_Enum_TN( ECV_ldtype );
  TN* enum_ldhint = Gen_Enum_TN( ECV_ldhint );
  OP* ld8_op = Mk_OP( TOP_ld8,TN_FIRST_OUTPUT_REG,True_TN,
                                  enum_ldtype,enum_ldhint,tgt_TN1 );
  OP_srcpos( ld8_op ) = srcpos;
  BB_Append_Op( bb,ld8_op );
  
  // Step III:generate arg3
  // The Generate opcode is like:
  //    TN126( r125 ) :- mov_i TN257( p0 ) ( 0xj )
  TN* TN_second_output_reg = Build_Dedicated_TN( ISA_REGISTER_CLASS_integer,
                        FIRST_OUTPUT_REG - 1, 8 );
  TN* literal_TN = Gen_Literal_TN( int_arg,8 );
  OP* mov_i_op = Mk_OP( TOP_mov_i,TN_second_output_reg,True_TN,literal_TN );
  BB_Append_Op( bb,mov_i_op );
   
  // Step IV: Generate call
  // The Generated opcode is like:
  //  GTN321(b0) :- br.call TN257(p0)(enum:.sptk) (enum:.many) (enum:)
  //        (sym:__profile_call_exit+0) GTN395(ar.ec) ; WN=0x809cf60
  TN* enum_sptk = Gen_Enum_TN( ECV_bwh_sptk );
  TN* enum_many = Gen_Enum_TN( ECV_ph_many );
  TN* enum_dh = Gen_Enum_TN( ECV_dh );
  
  TN *ar_ec = Build_Dedicated_TN ( ISA_REGISTER_CLASS_application,
				   ( REGISTER )( REGISTER_MIN + 66 ), 
				   8 );
  ty = Make_Function_Type( MTYPE_To_TY( MTYPE_V ) );
  ST *call_st = Gen_Intrinsic_Function( ty, function_name );
  Clear_PU_no_side_effects( Pu_Table[ST_pu( call_st )] );
  Clear_PU_is_pure( Pu_Table[ST_pu( call_st )] );
  Set_PU_no_delete( Pu_Table[ST_pu( call_st )] );
  TN *function_name_tn = Gen_Symbol_TN( call_st,0,0 );
  OP* call_op =Mk_OP( TOP_br_call,RA_TN,True_TN,enum_sptk,enum_many,enum_dh,
                      function_name_tn,ar_ec );
                      
  BB_Append_Op( bb,call_op );
  if ( restore_type == 1 )
  {
    BB_Append_Ops( bb,restore_reg_ops );
  }else if ( restore_type == 2 )  
  {
     BB_Append_Ops( bb,restore_f_reg_ops );  
  }
  
  Set_BB_call( bb );
  
  // add annotations about callinfo 
  WN *count = WN_Intconst( MTYPE_I4, int_arg );
  WN *str_arg_tn = WN_LdaString( str_arg, 0,
			strlen( str_arg ) + 1 );
  WN* call = Gen_Call( function_name, str_arg_tn ,count );
  call_st = WN_st( call );
  CALLINFO* call_info = TYPE_PU_ALLOC ( CALLINFO );
  CALLINFO_call_st( call_info ) = call_st;
  CALLINFO_call_wn( call_info ) = call;
  BB_Add_Annotation ( bb, ANNOT_CALLINFO, call_info );
  return bb;
}

BB* EDGE_PROFILE::Gen_Call_BB( char* function_name, TN * PU_Handle, 
                    INT64 int_arg,int restore_type,int restore_sum )
{
  BB *bb=Gen_BB( );
  SRCPOS srcpos = REGION_First_BB->ops.last->srcpos;
  
  OP* restore_f_reg;
  OP* restore_reg;
  OPS* restore_reg_ops = OPS_Create( );
  OPS* restore_f_reg_ops = OPS_Create( );
  
  if ( restore_type == 1 )
  {
    for ( int i = 0; i < restore_sum; i++ )  
    {
      TN* save_reg_tn1 = Gen_Register_TN( ISA_REGISTER_CLASS_integer,8 );
      TN* return_result_reg = 
         Build_Dedicated_TN( ISA_REGISTER_CLASS_integer,RETURN_REG + i ,8 );
      Set_TN_is_global_reg( save_reg_tn1 );
      restore_reg = Mk_OP( TOP_mov,return_result_reg,True_TN,save_reg_tn1 );
      OPS_Insert_Op(restore_reg_ops,NULL,restore_reg,FALSE);
      OP* mov_op = Mk_OP( TOP_mov,save_reg_tn1,True_TN,return_result_reg );
      BB_Append_Op( bb,mov_op );
    }  
  }
  
  if ( restore_type == 2 )
  { 
    for ( int i = 0 ; i < restore_sum ; i++)
    {
      TN* save_reg_tn2 = Gen_Register_TN( ISA_REGISTER_CLASS_float,8 );
      TN* f_return_result_reg = Build_Dedicated_TN( 
                    ISA_REGISTER_CLASS_float,FLOAT_RETURN_REG + i, 0 );

      Set_TN_is_global_reg( save_reg_tn2 );
      restore_f_reg = Mk_OP( TOP_mov_f,f_return_result_reg,
                                                True_TN,save_reg_tn2 );
      OPS_Insert_Op( restore_f_reg_ops,NULL,restore_f_reg,FALSE );
      OP* mov_f_op = Mk_OP( TOP_mov_f,save_reg_tn2,
                                         True_TN,f_return_result_reg );
      BB_Append_Op( bb,mov_f_op );
    }  
  }

  // Step I: get gp value
  // The Generated opcode is like:
  //    GTN585( gp ) :- mov TN257( p0 ) GTN715 ; copy
  TN* save_gp_TN = NULL;
  save_gp_TN = Find_TN( TOP_mov,1,1,GP_TN,2 );
  if ( save_gp_TN != NULL )
  {
    OP* mov_op = Mk_OP( TOP_mov,GP_TN,True_TN,save_gp_TN );
    BB_Append_Op( bb,mov_op );
  }  
  
  // Step II:genereate arg 1 TN
  // The Generate opcode is like:
  //    TN727 :- addl TN257( p0 ) ( sym#ltoff22:.rodata+0 ) GTN585( gp ) ;		
  //    TN_FIRST_OUTPUT_REG( r127 ) :- ld8 TN257( p0 ) ( enum: ) ( enum: ) TN727 ;
  TN* TN_FIRST_OUTPUT_REG = Build_Dedicated_TN( ISA_REGISTER_CLASS_integer,
                        FIRST_OUTPUT_REG, 8 );
  OP* mov_op = Mk_OP( TOP_mov,TN_FIRST_OUTPUT_REG,True_TN,_pu_hdr_return_tn);
  BB_Append_Op( bb,mov_op );
  
  // Step III:generate arg3
  // The Generate opcode is like:
  //    TN126( r125 ) :- mov_i TN257( p0 ) ( 0xj )
  TN* TN_second_output_reg = Build_Dedicated_TN( ISA_REGISTER_CLASS_integer,
                        FIRST_OUTPUT_REG - 1, 8 );
  TN* literal_TN = Gen_Literal_TN( int_arg,8 );
  OP* mov_i_op = Mk_OP( TOP_mov_i,TN_second_output_reg,True_TN,literal_TN );
  BB_Append_Op( bb,mov_i_op );
   
  // Step IV: Generate call
  // The Generated opcode is like:
  //  GTN321(b0) :- br.call TN257(p0)(enum:.sptk) (enum:.many) (enum:)
  //        (sym:__profile_call_exit+0) GTN395(ar.ec) ; WN=0x809cf60
  TN* enum_sptk = Gen_Enum_TN( ECV_bwh_sptk );
  TN* enum_many = Gen_Enum_TN( ECV_ph_many );
  TN* enum_dh = Gen_Enum_TN( ECV_dh );
  
  TN *ar_ec = Build_Dedicated_TN ( ISA_REGISTER_CLASS_application,
				   ( REGISTER )( REGISTER_MIN + 66 ), 
				   8 );
  TY_IDX ty = MTYPE_To_TY( MTYPE_STRING );
  ty = Make_Function_Type( MTYPE_To_TY( MTYPE_V ) );
  ST *call_st = Gen_Intrinsic_Function( ty, function_name );
  Clear_PU_no_side_effects( Pu_Table[ST_pu( call_st )] );
  Clear_PU_is_pure( Pu_Table[ST_pu( call_st )] );
  Set_PU_no_delete( Pu_Table[ST_pu( call_st )] );
  TN *function_name_tn = Gen_Symbol_TN( call_st,0,0 );
  OP* call_op =Mk_OP( TOP_br_call,RA_TN,True_TN,enum_sptk,enum_many,enum_dh,
                      function_name_tn,ar_ec );
                      
  BB_Append_Op( bb,call_op );
  if ( restore_type == 1 )
  {
    BB_Append_Ops( bb,restore_reg_ops );
  }else if ( restore_type == 2 )  
  {
     BB_Append_Ops( bb,restore_f_reg_ops );  
  }
  
  Set_BB_call( bb );
  
  // add annotations about callinfo 
  WN * arg1 = WN_Intconst( MTYPE_I8, 0 );
  WN * arg2 = WN_Intconst( MTYPE_I8, int_arg);
  WN* call = Gen_Call( function_name, arg1 ,arg2 );
  call_st = WN_st( call );
  CALLINFO* call_info = TYPE_PU_ALLOC ( CALLINFO );
  CALLINFO_call_st( call_info ) = call_st;
  CALLINFO_call_wn( call_info ) = call;
  BB_Add_Annotation ( bb, ANNOT_CALLINFO, call_info );
  return bb;
}
   

BB* EDGE_PROFILE::Gen_Instru_Call_BB( char* function_name,
                char* file_pu_name,INT64 id ) 
{
  return Gen_Call_BB( function_name,_pu_hdr_return_tn,id,0 );
}

BB* EDGE_PROFILE::Gen_PU_Init_Call_BB( char* function_name,char* file_pu_name,
                          INT64 _instrument_count )
{
  return Gen_Call_BB( function_name,file_pu_name,_instrument_count,0 );
}

// Find the TN which opcode is opc,return the No index TN
TN* EDGE_PROFILE::Find_TN( TOP opc,int bb_num,int TN_num, TN* t1, int index )
{
  Is_True( bb_num>0,( "the BB num is not less than 0" ) );
  Is_True( TN_num>=0,( " the TN num is not less than 0" ) );
  Is_True( index>=0,( " the TN num is not less than 0" ) );
  Is_True( index<=9,( " the TN num is not less than 9" ) );
  BB* bb=_pu_first_bb;
  for( int i=1; i<bb_num; i++ )
    if( bb != NULL )
      bb = BB_next( bb );
  
  if ( bb == NULL )
    return NULL;
  
  OP* opcode = bb->ops.first;
  for( ; opcode != NULL; opcode = OP_next( opcode ) )
  {
    if ( OP_code( opcode ) != opc )
      continue;
      
    if ( OP_opnd( opcode,TN_num ) != t1 )
      continue;
    else
      return OP_opnd( opcode,index );
  }
  
  if ( opcode == NULL )
    return NULL;
}

BOOL EDGE_PROFILE::BB_Is_In_BBLIST( BBLIST* bblst,BB* bb,BB* before_bb )
{
  for( BBLIST* tbblist = bblst; 
          tbblist != NULL && BBLIST_item( tbblist ) !=  before_bb; 
                                   tbblist = BBLIST_next( tbblist ) )
  {
      if( BBLIST_item( tbblist ) == bb )
        return TRUE;
  }
  return  FALSE;
}

BOOL EDGE_PROFILE::BB_Is_For_Instru(BB* bb)
{
  BB* bb1;
  if(BB_profile_added(bb))
    return FALSE;

  if(BB_profile_splitted(bb))
  {
    mBB_NUM id_before_profile = BB_id_before_profile(bb);
    for(bb1=bb; bb1 !=NULL; bb1=BB_next(bb1))
    {
      if (id_before_profile == BB_id_before_profile(bb1))
        break;
    }
    if (id_before_profile == BB_id_before_profile(bb1))
      return FALSE;
  }
  return TRUE;
}
TN* EDGE_PROFILE::Find_TN_in_BB( TOP opc, BB* in, int i1, TN* t1, int index )
{
  Is_True( in != NULL,( "the BB should not be NULL" ) );
  Is_True( i1>=0,( " the TN num is not less than 0" ) );
  Is_True( index>=0,( " the TN num is not less than 0" ) );
  Is_True( index<=9,( " the TN num is not less than 9" ) );

  OP* opcode = in->ops.first;
  for( ; opcode != NULL; opcode = OP_next( opcode ) )
  {
    if ( OP_code( opcode ) != opc )
      continue;
      
    if ( OP_opnd( opcode,i1 ) != t1 )
      continue;
    else
      return OP_opnd( opcode,index );
  }
  
  if ( opcode == NULL )
    return NULL;
}

void EDGE_PROFILE::Prepare_Instrument( )
{
  Require_And_Alloc_Reg( );
  // TODO:
  // Check the first op of the first BB of current PU is 
  // "alloc", if have not this op,gen and insert

  // Check whether the op that restores ar.pfs, if not, gen and 
  // insert the op
  OP* save_ar_pfs = REGION_First_BB->ops.first;
  TN* save_ar_pfs_TN = OP_result( save_ar_pfs,0 );
  TN* save_gp_TN = NULL;
  OP* save_gp_op = NULL;
  save_gp_TN = Find_TN( TOP_mov,1,1,GP_TN,2 );
  if(save_gp_TN == NULL)
  {
    save_gp_TN = Gen_Register_TN( ISA_REGISTER_CLASS_integer,8 );
    Set_TN_is_global_reg(save_gp_TN);
    for ( BB_LIST *elist = Entry_BB_Head; elist; elist = BB_LIST_rest( elist ) )
    {
      BB* entry_bb= BB_LIST_first( elist );
      save_gp_op = Mk_OP(TOP_mov,save_gp_TN,True_TN,GP_TN);
      save_gp_op->bb = entry_bb;
      BB_Insert_Op(entry_bb,entry_bb->ops.first->next,
                                               save_gp_op,false);  
    }
  }
    
  Is_True( save_ar_pfs_TN != NULL,( "there should be a IR that alloc pfs" ) );
  
  BB* exit_bb = _pu_last_bb;  
  TN* pfs_tn = Find_TN( TOP_mov_t_ar_r_i,PU_BB_Count,2,Pfs_TN,2 );
  OP* mov_t_ar_r_op;
  if ( NULL == pfs_tn )
  {
    mov_t_ar_r_op = Mk_OP( TOP_mov_t_ar_r_i,Pfs_TN,True_TN,save_ar_pfs_TN );
    for ( BB_LIST *elist = Exit_BB_Head; elist; elist = BB_LIST_rest( elist ) )
    {
      exit_bb = BB_LIST_first( elist );
      mov_t_ar_r_op = Mk_OP( TOP_mov_t_ar_r_i,Pfs_TN,True_TN,save_ar_pfs_TN );
      if ( exit_bb->ops.last->prev != NULL )
        BB_Insert_Op_After( exit_bb, exit_bb->ops.last->prev ,mov_t_ar_r_op );
      else
        BB_Insert_Op_After( exit_bb, exit_bb->ops.last,mov_t_ar_r_op );
    }   
  }  
}

void EDGE_PROFILE::Require_And_Alloc_Reg( )
{
  REGISTER_Allocate_Stacked_Register( ABI_PROPERTY_caller,
                  ISA_REGISTER_CLASS_integer, FIRST_OUTPUT_REG );
  REGISTER_Allocate_Stacked_Register( ABI_PROPERTY_caller,
                  ISA_REGISTER_CLASS_integer, FIRST_OUTPUT_REG - 1 );
  REGISTER_Allocate_Stacked_Register( ABI_PROPERTY_caller,
                  ISA_REGISTER_CLASS_integer, FIRST_OUTPUT_REG - 2 );  
}

// ------------------------------------------------------------------
// Get the current PU's Basic Block Num ( the value of global variable
// "PU_BB_Count"  is not the right number of BBs in current PU ) 
// ------------------------------------------------------------------
INT32 Curr_PU_BB_Count( )
{
  INT32 count = 0;
  for( BB* bb = REGION_First_BB; bb != NULL; bb = BB_next( bb ) )  
    ++count;
  return count;
}

void Init_CFG( )
{
  for( BB* bb = REGION_First_BB; bb != NULL; bb = BB_next( bb ) )
   for( BBLIST* out_edge = BB_succs( bb ); out_edge != NULL; 
            out_edge = BBLIST_next( out_edge ) )
     BBLIST_freq( out_edge ) = FB_FREQ_UNINIT;         
}

void EDGE_PROFILE::Propagate( )
{
 for( BB* bb = _pu_first_bb; bb != NULL; bb = BB_next( bb ) )
 {
  Propagate_Node_In( bb );
  Propagate_Node_Out( bb );
 }
}

// ------------------------------------------------------------------
// propagate the incoming  edge's freq of bb
// ------------------------------------------------------------------
void EDGE_PROFILE::Propagate_Node_In( BB* bb )
{
  FB_FREQ total_in,total_out;
  int uninit_in, uninit_out;
  total_in = total_out = FB_FREQ_ZERO;
  uninit_in = uninit_out = 0;
  BOOL has_in_edge = FALSE;
 
  // Check how many edges that is not inited
  for( BBLIST* in_edge = BB_preds( bb ); in_edge != NULL; 
    in_edge = BBLIST_next( in_edge ) )
   {
     has_in_edge = TRUE;
     float freq = Freq( BBLIST_item( in_edge ),bb );
     if ( UNINIT_FREQ( freq ) )
     {
       ++uninit_in;
       break;
     }  
     else
        total_in += FB_FREQ( freq,TRUE );
   }
   
   // If all the incoming edges have values, the BB's freq is equal to 
   // total_in
   if( uninit_in  == 0 && has_in_edge )
   {
     BB_freq( bb ) = total_in.Value( );
     // Find out how many outcoming edges have no value. If only one, 
     // outcoming edge has no value. Compute the outcoming edge's freq
     for ( BBLIST* out_edge = BB_succs( bb ); out_edge != NULL; 
            out_edge = BBLIST_next( out_edge ) )
     {
      float freq = Freq( bb,BBLIST_item( out_edge ) );
      if( UNINIT_FREQ( freq ) )
        ++uninit_out;
      else
        total_out += FB_FREQ( freq,TRUE );    
     }
     
     if ( uninit_out == 1 )
     {
       for ( BBLIST* out_edge = BB_succs( bb ); out_edge != NULL; 
           out_edge = BBLIST_next( out_edge ) )
       {
         float freq = Freq( bb,BBLIST_item( out_edge ) );
         if( UNINIT_FREQ( freq ) )
         {
             Set_Freq( bb,BBLIST_item( out_edge ),
                        total_in.Value( ) - total_out.Value( ) );
             Propagate_Node_In( BBLIST_item( out_edge ) );
             break;
         }    
       }
     }
   }
}

// ------------------------------------------------------------------
// propagate the outcoming  edge's freq of bb
// ------------------------------------------------------------------
void EDGE_PROFILE::Propagate_Node_Out( BB* bb )
{
  FB_FREQ total_in,total_out;
  int uninit_in, uninit_out;
  total_in = total_out = FB_FREQ_ZERO;
  uninit_in = uninit_out = 0;
  BOOL has_out_edge = FALSE;
  
  // Check how many edges that is not inited
  for( BBLIST* out_edge = BB_succs( bb ); out_edge != NULL; 
    out_edge = BBLIST_next( out_edge ) )
   {
     has_out_edge = TRUE;
     float freq = Freq( bb,BBLIST_item( out_edge ) );
     if ( UNINIT_FREQ( freq ) )
     {
       ++uninit_out;
       break;
     }  
     else
        total_out += FB_FREQ( freq,TRUE );
   }
   
   // if all the incoming edges has value, then the BB's freq is equal to 
   // total_in
   if( uninit_out == 0 && has_out_edge )
   {
     BB_freq( bb ) = total_out.Value( );
     // find how many outcoming edges that has not value. if just one 
     // outcoming edge has not value, compute the outcoming edge's freq
     for ( BBLIST* in_edge = BB_preds( bb ); in_edge != NULL; 
            in_edge = BBLIST_next( in_edge ) )
     {
      float freq = Freq( BBLIST_item( in_edge ),bb );
      if( UNINIT_FREQ( freq ) )
        ++uninit_in;
      else
        total_in += FB_FREQ( freq,TRUE );    
     }
     
     if ( uninit_in == 1 )
     {
       for ( BBLIST* in_edge = BB_preds( bb ); in_edge != NULL; 
           in_edge = BBLIST_next( in_edge ) )
       {
         float freq = Freq( BBLIST_item( in_edge ),bb );
         if( UNINIT_FREQ( freq ) )
         {
             Set_Freq( BBLIST_item( in_edge ),bb,
                      total_out.Value( ) - total_in.Value( ) );
             Propagate_Node_Out( BBLIST_item( in_edge ) );
             break;
         }    
       }
       
     }
   }
}

// ------------------------------------------------------------------
// Compute the edge's freq, and the premise is that the edge has its 
// probability. IF the edge's freq is equal to edge's probability, 
// multiply the BB's freq
// ------------------------------------------------------------------
void Compute_Edge_Freq_Base_Prob( )
{
  for( BB* bb = REGION_First_BB; bb != NULL; bb = BB_next( bb ) )
  {
    for( BBLIST* out_edge = BB_succs( bb ); out_edge != NULL;
        out_edge = BBLIST_next( out_edge ) )
    {
      if ( ERROR_FREQ( BB_freq( bb ) ) )
        DevWarn( "there a bb with invalid freq,BB id %d",BB_id( bb ) );
      else if ( ERROR_PROB( BBLIST_prob( out_edge ) ) )
      {
        DevWarn( "there a edge with invalid prob" );
        BBLIST_prob( out_edge ) = 0.0;
        BBLIST_freq( out_edge ) = 0.0;
      }
      else
        BBLIST_freq( out_edge ) = BBLIST_prob( out_edge ) * BB_freq( bb );
    }
  }
}

// ------------------------------------------------------------------
// Compute the edge's prob, and the premise is that the edge has its 
// freq. The edge's prob is equal to bb's freq divided by edge's freq
// ------------------------------------------------------------------

void Compute_Edge_Prob_Base_Freq( )
{
  INT32 len = 1;
  for( BB* bb = REGION_First_BB; bb != NULL; bb = BB_next( bb ) )
  {
    BBLIST* out_edge =  BB_succs( bb );
    if ( out_edge != NULL )
      len =  BBlist_Len( out_edge );
    for( out_edge = BB_succs( bb ); out_edge != NULL;
        out_edge = BBLIST_next( out_edge ) )
    {
      if ( ERROR_FREQ( BB_freq( bb ) ) )
        DevWarn( "there a bb with invalid freq,BB id %d",BB_id( bb ) );
      else if ( ERROR_FREQ( BBLIST_freq( out_edge ) ) )
        DevWarn( "there a edge with invalid prob" );
      else if ( ZERO_FREQ( BB_freq( bb ) ) )
      {
        BBLIST_prob( out_edge ) = 1.0F / len;
      }
      else
        BBLIST_prob( out_edge ) = BBLIST_freq( out_edge ) / BB_freq( bb );
    }
  }
}

void Change_BB_Label( BB* bb,LABEL_IDX i )
{
  Set_Label_BB( i,bb );
  ANNOTATION* annt  = ANNOT_Get( BB_annotations( bb ),ANNOT_LABEL );
  if ( annt != NULL )
  {
    BB_annotations( bb ) = ANNOT_Unlink( BB_annotations( bb ), annt );
    BB_Add_Annotation( bb,ANNOT_LABEL,(void *)(INTPTR)i);
  }
}

BB* First_Branch_BB( )
{
  BB* bb = NULL;
  for( bb = REGION_First_BB; bb != NULL; bb =BB_next( bb ) )
  {
    if( BB_kind( bb ) == BBKIND_LOGIF || BB_kind( bb ) == BBKIND_VARGOTO ||
        BB_kind( bb ) == BBKIND_INDGOTO )
    return bb;    
  }
  return bb;
}

BOOL Result_Tn_In_BB( BB* bb, TN* tn )
{
  OP* opcode = bb->ops.first;
  int i;
  for( ; opcode != NULL; opcode = OP_next( opcode ) )
  {
    for ( i = 0; i < OP_results( opcode ); i++ ) 
    {
      if ( tn == OP_result( opcode,i ) ) 
        return TRUE; 
    }
  }
  return FALSE;
}

BOOL Opnd_Tn_In_BB( BB* bb, TN* tn )
{
  OP* opcode = bb->ops.first;
  int i;
  for( ; opcode != NULL; opcode = OP_next( opcode ) )
  {
    for ( i = 0; i < OP_opnds( opcode ); i++ )  
    {
      if ( tn == OP_opnd( opcode,i ) ) 
      {
         return TRUE; 
      }  
    }
  }
  return FALSE;
}

LABEL_IDX Get_Br_Op_Tgt_Label( OP* branch_op )
{
  INT tn_num = OP_opnds( branch_op );
  for( int i = 0; i<tn_num; i++ )
  {
    TN* tgt_tn = OP_opnd( branch_op, i );
    if( TN_is_label( tgt_tn ) )
    {
      return TN_label( tgt_tn );
    }
  }
  return LABEL_IDX_NONE;
}

// Return the sum of the procedure return register in BB bb
INT32 Get_Return_Reg_Sum(BB* bb)
{
  for ( int i = 3; i >= 0; i-- )
  {
    TN* return_result_reg = 
            Build_Dedicated_TN( ISA_REGISTER_CLASS_integer,RETURN_REG + i, 8);
    
    OP* opcode = bb->ops.first;
    int j;
    for( ; opcode != NULL; opcode = OP_next( opcode ))
    {
      for ( j = 0; j < OP_opnds( opcode ); j++ )  
      {
        if ( return_result_reg == OP_opnd( opcode,j ) ) 
        {
          return i+1; 
        }  
      }
      for ( j = 0; j < OP_results( opcode ); j++)
      {
        if ( return_result_reg == OP_result( opcode,j ) )
	{
          opcode = bb->ops.last;	  
          break;
	}
      }
    }
  }                  
  return 0;
}

// return the sum of the procedure return float register in BB bb
INT32 Get_Float_Return_Reg_Sum(BB* bb)
{
  for ( int i = 7 ; i >= 0; i-- )
  {
    TN* f_return_result_reg = 
        Build_Dedicated_TN( ISA_REGISTER_CLASS_float,FLOAT_RETURN_REG + i, 0);

    OP* opcode = bb->ops.first;
    int j;
    for( ; opcode != NULL; opcode = OP_next( opcode ))
    {
      for ( j = 0; j < OP_opnds( opcode ); j++ )  
      {
        if ( f_return_result_reg == OP_opnd( opcode,j ) ) 
        {
          return i+1; 
        }  
      }
      for ( j = 0; j < OP_results( opcode ); j++)
      {
        if ( f_return_result_reg == OP_result( opcode,j ) )
	{
          opcode = bb->ops.last;	  
          break;
	}
      }
    }
  }                  
  return 0;
}
