/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */

/* 
 * A local instruction scheduler dedicated to family 15h amd processors.
 * May 29, 2003, Augmented by AMD in January 2010.
 */

#include "cgir.h"
#include "glob.h"
#include "tn_map.h"
#include "cgtarget.h"
#include "cg_vector.h"
#include "gra_live.h"
#include "freq.h"
#include "ti_res.h"
#include "register.h"
#include "tracing.h"
#include "config_asm.h"
#include "note.h"
#include "cgexp.h"
#include "lra.h"
#include "wn_util.h"
#include "hb_hazards.h"
#include "reg_live.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <math.h>

#include "cg_sched.h"

enum ICU { NONE = 0, ALU, AGU, FPU };

#define FU_NONE  0x0000
#define FPU_pn   0x0010
#define ALU_pn   0x0020
#define AGU_pn   0x0040

static unsigned int BBs_Processed = 0;
static unsigned int TOP_2_Res[TOP_count];
static uint16_t sched_order = 0;
static uint16_t sched_group = 0;
static uint8_t  sched_group_imm_size = 0;
static uint8_t  sched_group_64bit_imm_count = 0;
static bool     BB_contains_x87 = false;
static bool     BB_fails_rid = false;
static bool     sched_trace;

typedef struct {
  TN*      mem_base;
  TN*      mem_index;
  ST*      mem_sym;
  uint64_t mem_ofst;
  uint8_t  mem_scale;
  uint8_t  imm_size;
  uint8_t  insn_size;

  uint16_t uses;
  uint16_t latency;

  uint16_t sched_order;
  uint16_t sched_group;
  uint16_t pred_order;  // the order of the last scheduled predecessor
  int16_t release_time; // Aka the dispatch time. Determined by data dependences.
  int16_t issue_time;   // Determined dynamically by the current hw resources.
  int16_t deadline;

  uint16_t num_succs;
  uint16_t num_preds;

  int bb_id;
  int num_pf_bytes;
  bool is_scheduled;
  bool has_64bit_imm;
} OPR;

static OPR* opr_array = NULL;

typedef struct {
  uint16_t sched_group;
  uint16_t opr_count;
  uint16_t total_pf_bytes;
  uint16_t size;
  long     offset_start;
  long     offset_end;
  long     members[4];
} GROUP_INFO;

typedef struct {
  GROUP_INFO window_pair[2];
  long     cur_offset;
  uint16_t index;
  int      bb_id;
  bool     term_pair_for_fit;
} DECODE_INFO;

static DECODE_INFO window_data;

#define ASSERT(c)   FmtAssert( c, ("DSP_SCH error") );

#define Get_OPR(op)               (&opr_array[OP_map_idx(op)])
#define OPR_release_time(o)       ((o)->release_time)
#define OPR_deadline(o)           ((o)->deadline)
#define OPR_issue_time(o)         ((o)->issue_time)
#define OPR_is_scheduled(o)       ((o)->is_scheduled == true)
#define Set_OPR_is_scheduled(o)   ((o)->is_scheduled = true)
#define Reset_OPR_is_scheduled(o) ((o)->is_scheduled = false)
#define OPR_has_64bit_imm(o)      ((o)->has_64bit_imm == true)
#define Set_OPR_has_64bit_imm(o)  ((o)->has_64bit_imm = true)
#define OPR_num_pf_bytes(o)       ((o)->num_pf_bytes)
#define OPR_num_preds(o)          ((o)->num_preds)
#define OPR_num_succs(o)          ((o)->num_succs)
#define OPR_sched_order(o)        ((o)->sched_order)
#define OPR_sched_group(o)        ((o)->sched_group)
#define OPR_pred_order(o)         ((o)->pred_order)
#define OPR_mem_base(o)           ((o)->mem_base)
#define OPR_mem_index(o)          ((o)->mem_index)
#define OPR_mem_ofst(o)           ((o)->mem_ofst)
#define OPR_mem_scale(o)          ((o)->mem_scale)
#define OPR_mem_sym(o)            ((o)->mem_sym)
#define OPR_imm_size(o)           ((o)->imm_size)
#define OPR_insn_size(o)          ((o)->insn_size)
#define OPR_uses(o)               ((o)->uses)
#define OPR_latency(o)            ((o)->latency)
#define OPR_bb_id(o)              ((o)->bb_id)

static const int num_fu[] = {
  0,   /* NONE  */
  4,   /* ALU   */
  3,   /* AGU   */
  4,   /* FPU   */
};

class MRT {
private:
  bool trace;
  BB  *bb;
  DSP_SCH *sched;

  REGISTER_SET live_in[ISA_REGISTER_CLASS_MAX+1];
  REGISTER_SET live_out[ISA_REGISTER_CLASS_MAX+1];
  REGISTER_SET avail_regs[ISA_REGISTER_CLASS_MAX+1];

  static const int load_ops_rate = 2;
  static const int store_ops_rate = 1;

  struct Resource_Table_Entry {
    uint8_t dispatched_ops; /* the number of dispatched ops. */
    uint8_t load_ops;       /* how many <dispatched_ops> are load mem ops. */
    uint8_t store_ops;      /* how many <dispatched_ops> are store mem ops. */
    uint8_t fp_dbl_ops;     /* how many <dispatched_ops> are fastpath dbl ops */
    uint8_t fpu_ops;        /* how many <dispatched_ops> are fpu ops.  */
    uint8_t agu_ops;        /* how many <dispatched_ops> are alu ops.  */
    uint8_t alu_ops;        /* how many <dispatched_ops> are agu ops.  */
    bool resources[4][4];
    bool has_load_op_store;
  };

  void Init_Table_Entry( Resource_Table_Entry* e )
  {
    e->dispatched_ops = e->load_ops = e->store_ops = e->fp_dbl_ops = 0;
    e->agu_ops = e->alu_ops = e->fpu_ops = 0;
    e->has_load_op_store = false;

    bzero( e->resources, sizeof(e->resources) );
    for( int i = NONE; i <= FPU; i++ ){
      for( int j = 0; j < num_fu[i]; j++ ){
	e->resources[i][j] = true;
      }
    }
  }

  bool Probe_Resources( int cycle, OP* op, int, bool take_it );
  int Get_Dispatch_Unit( OP*, int, bool );
  ICU Lookup_Property_By_Pipeinfo( OP*, int, bool );

  bool TOP_is_lea( const TOP top )
  {
    return ( top == TOP_lea32   || top == TOP_lea64  ||
             top == TOP_leax32  || top == TOP_leax64 ||
             top == TOP_leaxx32 || top == TOP_leaxx64 );
  }

  int entries;
  Resource_Table_Entry** Resource_Table;


public:
  MRT() {};
  ~MRT() {};

  static const int issue_rate = 4;
  ICU cur_res;

  void Init( BB*, DSP_SCH *sched, int, bool, MEM_POOL* );
  void Reserve_Resources( OP*, int );
  void Set_Completion_Time( OP* );
  void Compute_Issue_Time( OP*, int );
  BB  *Get_Cur_BB( void ) { return bb; }
  DSP_SCH *Get_Cur_Sched( void ) { return sched; }

  bool TOP_is_push( const TOP top )
  {
    return ( top == TOP_pushl || top == TOP_pushq );
  }

  bool TOP_is_pop( const TOP top )
  {
    return ( top == TOP_popl || top == TOP_popq );
  }

  bool OP_is_memory_load( OP *op )
  {
    int ret_val = false;
    const TOP top = OP_code(op);

    if( TOP_is_load(top)       || 
        TOP_is_prefetch(top)   || 
        TOP_is_pop( top )      ||
        TOP_is_lea( top )      ||
        TOP_is_load_exe(top) ) {
      ret_val = true;
    } else if( top == TOP_ldc64 ) {
      TN *base_tn = OP_opnd(op,0);
      if( TN_is_symbol(base_tn) ){
        ret_val = true;
      }
    }

    return ret_val;
  }

  bool OP_is_memory_store( OP *op )
  {
    int ret_val = false;
    const TOP top = OP_code(op);

    if( TOP_is_store(top)          || 
        TOP_is_load_exe_store(top) ||
        TOP_is_push ( top ) ) {
      ret_val = true;
    }

    return ret_val;
  }

  bool Decoder_is_Saturated( int c )
  {
    Resource_Table_Entry* entry = Resource_Table[c];
    if( entry->fp_dbl_ops ){
      switch( entry->fp_dbl_ops ){
      case 1:
      {
        uint8_t non_fp_dbl_ops = entry->dispatched_ops - entry->fp_dbl_ops;
        if( ( ( entry->fp_dbl_ops * 2 ) + non_fp_dbl_ops ) == issue_rate )
          return true;
        else
          return false;  
      }
      case 2:
        return true;
      }
    }
    
    return entry->dispatched_ops == issue_rate;
  }

  bool Memory_Saturated( OP *op, int c )
  {
    Resource_Table_Entry* entry = Resource_Table[c];
    bool op_type_store = OP_is_memory_store( op );

    if( OP_is_memory_load( op ) ||  op_type_store ) {
      // except where the current op is a part of a load_op_store operation
      if( ( op_type_store == true ) && 
          ( entry->store_ops == store_ops_rate ) ){
        return true;
      } else if( ( OP_is_memory_load( op ) ) && 
               ( entry->load_ops == load_ops_rate ) ){
        return true;
      } 
    }

    return false;
  }

  bool Can_Reserve( OP *op, int clock )
  {
    Resource_Table_Entry* entry = Resource_Table[clock];
    OPR* opr = Get_OPR(op);
    int c = OPR_issue_time(opr);
    int dispatch_unit = Get_Dispatch_Unit( op, clock, true );
    int new_imm_size = sched_group_imm_size + OPR_imm_size(opr);
    const TOP top = OP_code(op);
    uint8_t non_fp_dbl_ops = entry->dispatched_ops - entry->fp_dbl_ops;
    uint8_t total_ops = ( entry->fp_dbl_ops * 2 ) + non_fp_dbl_ops;

    // could not map
    if (cur_res == NONE)
      return false;

    if( TOP_is_fastpath_db(top) ){
      if( total_ops > 2 )
        return false;
    }

    if( sched_group_64bit_imm_count ) {
      if( OPR_has_64bit_imm(opr) && sched_group_64bit_imm_count == 2 )
        return false;

      if(( sched_group_64bit_imm_count == 1) && (total_ops == 3))
        return false;

      if(( sched_group_64bit_imm_count == 2) && (total_ops == 2))
        return false;

      if(( sched_group_64bit_imm_count == 1) && (total_ops == 2)) {
        if( OPR_has_64bit_imm(opr) )
          return false;
      }
    }
   
    // imm size is normalized to bytes
    if( new_imm_size > 16 ) {
      return false;
    }

    if( OP_is_memory_load( op ) || OP_is_memory_store( op ) ) {
      if( Memory_Saturated( op, clock ) )
        return false;
    }

    if( ( window_data.index == 1 ) && 
        ( window_data.window_pair[1].opr_count ) ){
      long window_start = window_data.window_pair[0].offset_start;
      long window_end = window_data.cur_offset;
      int total_pf_bytes = window_data.window_pair[1].total_pf_bytes;
      bool pf_bytes_enough = true;
      window_end = 16 + ( ( window_data.cur_offset - 1) & ~ 15 );

      int padding = window_end - window_data.cur_offset;
      if( TOP_is_fastpath_db(top) )
        total_ops += 2;
      else
        total_ops++;

      // if this is true, we need to see if the current instruction can fit
      if( ( (window_end - window_start) & 0x30 ) == 0x30 ){
        int new_offset = window_data.cur_offset + OPR_insn_size(opr);
        if( new_offset > window_end ){
          window_data.term_pair_for_fit = true;
          return false;
        }
      }
    }

    return Resource_Table[clock]->resources[cur_res][dispatch_unit];
  }

  int  Dispatched_Ops( int c ) { return Resource_Table[c]->dispatched_ops; }
};

static MRT mrt;

ICU MRT::Lookup_Property_By_Pipeinfo( OP *op,
                                      int cycle,
                                      bool allow_none )
{
  Resource_Table_Entry* entry = Resource_Table[cycle];
  unsigned int res = TOP_2_Res[OP_code( op )];
  ICU matched_res = NONE;

  switch( res ){
  case FPU_pn:
    matched_res = FPU;
    break;
  case ALU_pn:
    matched_res = ALU;
    break;
  case AGU_pn:
    matched_res = AGU;
    break;
  }

  // If we are checking the resource for reservation, we want to allow
  // NONE to tell us no mapping is available.
  if( allow_none == false )
    ASSERT( matched_res != NONE );

  return matched_res;
}


static void Add_Nop_To_Schedule( BB *bb, OP *target, OP *nop, DSP_SCH *sched )
{
  BB_Insert_Op_After( bb, target, nop );
  OPR *opr = Get_OPR(nop);
  OPR_insn_size(opr) = OP_dpadd(nop);
  OPR_sched_order(opr) = sched_order++;
  VECTOR_Add_Element( sched->_sched_vector, nop );
  if( sched_trace ) {
    fprintf(TFile,
            "window[%d] : op(sched=%d) = %s, with size = %d\n",
            window_data.index, 
            OPR_sched_order(opr),
            TOP_Name(OP_code(nop)),
            OPR_insn_size(opr));
  }
}


static void Init_Window_Data( void )
{
  int i;

  for( i = 0; i < 2; i ++ ){
    window_data.window_pair[i].sched_group = 0;
    window_data.window_pair[i].opr_count = 0;
    window_data.window_pair[i].total_pf_bytes = 0;
    window_data.window_pair[i].size = 0;
    window_data.window_pair[i].offset_start = 0;
    window_data.window_pair[i].offset_end = 0;
    window_data.window_pair[i].members[0] = 0;
    window_data.window_pair[i].members[1] = 0;
    window_data.window_pair[i].members[2] = 0;
    window_data.window_pair[i].members[3] = 0;
  }

  window_data.index = 0;
  window_data.term_pair_for_fit = false;
  window_data.bb_id = 0;
}


static void Add_OPR_To_Current_Window( OPR *opr, OP *op )
{
  uint16_t window_idx = window_data.index;
  uint16_t opr_idx = window_data.window_pair[window_idx].opr_count;
  if( window_data.window_pair[window_idx].opr_count == 0) {
    window_data.window_pair[window_idx].offset_start = window_data.cur_offset;
  }
  window_data.window_pair[window_idx].sched_group = OPR_sched_group(opr);
  window_data.window_pair[window_idx].members[opr_idx] = (long)op;
  window_data.window_pair[window_idx].opr_count++;
  window_data.window_pair[window_idx].total_pf_bytes += OPR_num_pf_bytes(opr);
  window_data.window_pair[window_idx].size += OPR_insn_size(opr);

  window_data.cur_offset += OPR_insn_size(opr);
  window_data.bb_id = OPR_bb_id(opr);
}


int Calculate_Imm_Range(long val);


int Update_Branch_Insn_Size( int padding, OPR *opr )
{
  int branch_distance = window_data.cur_offset; 
  uint16_t imm_size = Calculate_Imm_Range( branch_distance ); 
  uint16_t size_change = 0;

  switch( imm_size ) {
  case 2:
  case 4:
    // 3 extra bytes for the imm, 1 for the opcode
    // because the encoded imm is either 1 or 4 for branches
    size_change = 4;
    break;
  default:
    break;
  }

  OPR_insn_size(opr) += size_change;
  padding = ( padding > size_change ) ? ( padding - size_change ) : 0;

  return padding;
}


int Pad_Windows( int padding, int total_insns )
{
  uint16_t window_idx = window_data.index;
  uint16_t pad_left;
  int i, j, k, n;
  int pf_iter = 0;
  int bb_len = 0;
  bool has_branch = false;
  OP *br_op = NULL;

  if( padding == 0 ) return padding;

  for( i = 0; i <= window_idx; i++ ){
    n = window_data.window_pair[i].opr_count;
    BB* bb = mrt.Get_Cur_BB();

    if( bb_len == 0 )
      bb_len = BB_length(bb);

    for( j = 0; j < n; j++ ){
      OP* op = (OP*)window_data.window_pair[i].members[j];
      if( op != NULL ){
        OPR* opr = Get_OPR(op);
        int opr_pad = OPR_num_pf_bytes(opr);
        if( opr_pad > pf_iter ) pf_iter = opr_pad;
        if( OP_br(op) ){
          has_branch = true;
          padding = Update_Branch_Insn_Size( padding, opr );
        }
      }
    }
  }

  if( padding == 0 ) return padding;

  pad_left = padding;

  // Iteratively add padding to all insns which can take padding until
  // we either run out of required padding or cannot pad any more insns. 
  for( k = 0; k < pf_iter; k++ ){
    for( i = 0; i <= window_idx; i++ ){
      n = window_data.window_pair[i].opr_count;
      for( j = 0; j < n; j++ ){
        OP* op = (OP*)window_data.window_pair[i].members[j];
        if( OP_br(op) ) continue;
        if( op != NULL ){
          OPR* opr = Get_OPR(op);
          int opr_pad = OPR_num_pf_bytes(opr);
          if( ( pad_left > 0 ) && ( opr_pad >= ( k + 1 ) ) ){
            OP_dpadd(op) = k + 1 ;
            pad_left--;
          }
        }
        if( pad_left == 0 ) break;
      }
      if( pad_left == 0 ) break;
    }
    if( pad_left == 0 ) break;
  }

  // Add nops if pad_left is non zero
  if( ( pad_left != 0 ) && ( has_branch == false ) ){
    int k = window_data.window_pair[window_idx].opr_count - 1;
    OP *op = (OP*)window_data.window_pair[window_idx].members[k];
    BB* bb = mrt.Get_Cur_BB();

    // return if we have no place to put a nop
    if( ( window_idx == 1 ) && ( total_insns == 8 ) )
      return padding;

    // nothing to do if this is a ret insn
    TOP top = OP_code(op);
    if (OP_xfer(op) && ((top == TOP_ret) || top == TOP_reti))
      return padding;

    OP* nop = Mk_OP(TOP_nop);
    DSP_SCH *sched = mrt.Get_Cur_Sched();
 
    if( sched_trace )
      fprintf(TFile, "creating nop(s) programed to emit %d bytes\n", pad_left);

    // Insert nop(s) and preprogram them to be of size pad_left.
    // Keep in mind that nops of size 1..12 should be the limit, if
    // we need 13 bytes or more, we emit a 2nd nop.
    OP_dgroup(nop) = sched_group;
    if( ( pad_left > 8 ) && ( pad_left < 13 ) ){
      OP_dpadd(nop) = 2;
      pad_left = 0;
    } else if( pad_left >= 13 ){
      OP_dpadd(nop) = 1;
      pad_left -= 8;
    } else {
      OP_dpadd(nop) = 1;
      pad_left = 0;
    }
    INT op_map_idx = OP_map_idx(op);
    Set_OP_unrolling( nop, OP_unrolling(op) );
    Set_OP_unroll_bb( nop, OP_unroll_bb(op) ); 
    Add_Nop_To_Schedule( bb, op, nop, sched );
    if( pad_left > 0 ){
      OP *prev = nop;
      nop = Mk_OP(TOP_nop);
      OP_dpadd(nop) = 2;
      Set_OP_unrolling( nop, OP_unrolling(op) );
      Set_OP_unroll_bb( nop, OP_unroll_bb(op) ); 
      Add_Nop_To_Schedule( bb, op, nop, sched );
    }
  }

  return padding;
}


static void Trace_Close_Current_Window( int pair_len, 
                                        int padding, 
                                        int total_insns )
{
  if( sched_trace ) {
    int i, j;
    uint16_t window_idx = window_data.index;
    fprintf( TFile, 
             "terminating dispatch pair(bb=%d, num windows=%d): info...\n", 
             window_data.bb_id, ( window_idx + 1 ) );
    fprintf( TFile, 
             "pair length = %d : padding needed = %d : total insns = %d\n", 
             pair_len, padding, total_insns );
    fprintf( TFile, 
             "padding available = %d\n", 
             window_data.window_pair[0].total_pf_bytes + 
             window_data.window_pair[1].total_pf_bytes );
    fprintf( TFile, 
             "window pair starts at offset = %p, ends at %p, cur off = %p\n", 
             (void*)window_data.window_pair[0].offset_start,
             (void*)window_data.window_pair[window_idx].offset_end,
             (void*)window_data.cur_offset ); 
    for (i = 0; i <= window_idx; i++) {
      for (j = 0; j < window_data.window_pair[i].opr_count; j++) {
        OP *op = (OP*)window_data.window_pair[i].members[j];
        OPR *opr = Get_OPR(op);
        fprintf(TFile,
                "window[%d] : op(sched=%d,group=%d) = %s, with size = %d\n",
                i, 
                OPR_sched_order(opr),
                OPR_sched_group(opr),
                TOP_Name(OP_code(op)),
                OPR_insn_size(opr));
      }
    }
  }
}


static void Close_Current_Window( bool full_close )
{
  uint16_t window_idx = window_data.index;
  window_data.window_pair[window_idx].offset_end = window_data.cur_offset;
  if( ( window_idx == 0 ) && ( full_close == false ) ) {
    int total_pf_bytes = window_data.window_pair[window_idx].total_pf_bytes;
    // Now calculate the amount of padding bytes and configure the new offset
    int pair_len = window_data.window_pair[window_idx].offset_end -
                     window_data.window_pair[window_idx].offset_start;
    int total_insns = window_data.window_pair[window_idx].opr_count;

    // defer padding until close
    window_data.index++;
  } else if( ( window_idx == 1 ) || ( full_close  == true ) ){
    // Now calculate the amount of padding bytes and configure the new offset
    int pair_len = window_data.window_pair[window_idx].offset_end -
                     window_data.window_pair[0].offset_start;
    long new_offset = 16 + ( ( window_data.cur_offset - 1) & ~ 15 );
    int padding = new_offset - window_data.cur_offset;
    int total_insns = window_data.window_pair[0].opr_count +
                        window_data.window_pair[window_idx].opr_count;

    // Now update to the next 16B aligned addr
    window_data.cur_offset = new_offset;
    padding = Pad_Windows( padding, total_insns );
    Trace_Close_Current_Window( pair_len, padding, total_insns );
    Init_Window_Data();
  }
}


static void Print_Register_Set( const char* name, 
                                REGISTER_SET reg_set, 
                                ISA_REGISTER_CLASS cl )
{
  if( REGISTER_SET_EmptyP(reg_set) )
    return;

  fprintf( TFile, "%s: \n\t", name );
  REGISTER_SET_Print(reg_set, TFile);
  fprintf( TFile, "\n\t(Registers):" );

  REGISTER reg;
  FOR_ALL_REGISTER_SET_members(reg_set, reg) {
    fprintf( TFile, " %s", REGISTER_name(cl, reg) );
  }

  fprintf( TFile, "\n" );
}


void MRT::Init( BB* cur_bb, DSP_SCH *cur_sched, int size, 
                bool trace, MEM_POOL* mem_pool )
{
  static bool TOP_2_Res_is_valid = false;

  bb = cur_bb;
  sched = cur_sched;
  cur_res = NONE;
  entries = size;
  this->trace = trace;
  Resource_Table = (MRT::Resource_Table_Entry**)
    MEM_POOL_Alloc( mem_pool, ( sizeof( Resource_Table[0] ) * entries) );

  for( int i = 0; i < entries; i++ ){
    Resource_Table[i] = (MRT::Resource_Table_Entry*)
      MEM_POOL_Alloc(mem_pool, sizeof(Resource_Table[i][0]));
    Init_Table_Entry( Resource_Table[i] );
  }

  // Setup the table for resource usage by each top.
  if( !TOP_2_Res_is_valid ){
    TOP_2_Res_is_valid = true;

    for( int i = 0; i < TOP_count; i++ ){
      const TOP top = (TOP)i;
      unsigned int res = FU_NONE;

      if( TOP_is_flop(top) ) {
        res = FPU_pn;
      } else {
	res = ALU_pn;
	if( TOP_is_store(top)          ||
            TOP_is_load_exe_store(top) ||
            TOP_is_load(top)           ||
            TOP_is_load_exe(top)       ||
            TOP_is_prefetch(top)       ||
            TOP_is_pop ( top )         ||
            TOP_is_push ( top )        ||
	    TOP_is_lea( top ) ){
	  res = AGU_pn;
	}
      }
      TOP_2_Res[top] = res;
    }
  }

  // Compute the registers usage.
  ISA_REGISTER_CLASS cl;
  FOR_ALL_ISA_REGISTER_CLASS(cl){
    live_in[cl] = live_out[cl] = REGISTER_SET_EMPTY_SET;
    avail_regs[cl] = REGISTER_CLASS_allocatable(cl);

    for( REGISTER reg = REGISTER_MIN; reg <= REGISTER_MAX; reg++ ){
      if( REG_LIVE_Into_BB(cl, reg, bb) )
	live_in[cl] = REGISTER_SET_Union1(live_in[cl], reg);

      if( REG_LIVE_Outof_BB(cl, reg, bb) )
	live_out[cl] = REGISTER_SET_Union1(live_out[cl], reg);
    }

    avail_regs[cl] = REGISTER_SET_Difference(avail_regs[cl], live_in[cl]);
    avail_regs[cl] = REGISTER_SET_Difference(avail_regs[cl], live_out[cl]);

    if( trace ){
      Print_Register_Set( "live_in", live_in[cl], cl );
      Print_Register_Set( "live_out", live_out[cl], cl );
      Print_Register_Set( "avail_regs", avail_regs[cl], cl );
    }
  }
}


int MRT::Get_Dispatch_Unit( OP* op, int cycle, bool allow_none )
{
  cur_res = Lookup_Property_By_Pipeinfo( op, cycle, allow_none );
  int dispatch_unit;

  switch( cur_res ){
  case FPU:
    dispatch_unit = Resource_Table[cycle]->fpu_ops;
    break;
  case ALU:
    dispatch_unit = Resource_Table[cycle]->alu_ops;
    break;
  case AGU:
    dispatch_unit = Resource_Table[cycle]->agu_ops;
    break;
  }

  // we are returning the next available dispatch unit in a FU.
  return dispatch_unit;
}


/*
 * Compute the issue time of <op>, given that <op> is dispatched at
 * cycle <clock>.
 */
void MRT::Compute_Issue_Time( OP* op, int clock )
{
  OPR* opr = Get_OPR(op);
  const int dispatch_unit = Get_Dispatch_Unit( op, clock, true );

  // Do not update the issue time if the fu resource if full.
  if( num_fu[cur_res] <= dispatch_unit )
    return;

  if( clock < OPR_release_time(opr) )
    clock = OPR_release_time(opr);

  while( clock <= OPR_deadline(opr) ){
    if( Probe_Resources( clock, op, dispatch_unit, false ) )
      break;
    clock++;
  }

  ASSERT( clock <= OPR_deadline(opr) );
  OPR_issue_time(opr) = clock;
}


/*
 * Check whether <op> can be scheduled at time <cycle> at <dispatch_unit>.
 * Or check the availability of resources[<cycle>] at <dispatch_unit>.
 */
bool MRT::Probe_Resources( int cycle, OP* op, int dispatch_unit, bool take_it )
{
  const TOP top = OP_code(op);
  Resource_Table_Entry* entry = Resource_Table[cycle];

  const ICU res = Lookup_Property_By_Pipeinfo( op, cycle, true );

  if( !entry->resources[res][dispatch_unit] )
    return false;

  if( take_it )
    entry->resources[res][dispatch_unit] = false;

  return true;
}


/* 
 * <op> is dispatched at time <cycle>. But its won't be issued until
 * its issue time due.
 */
void MRT::Reserve_Resources( OP* op, int cycle )
{
  OPR* opr = Get_OPR( op );
  const int dispatch_unit = Get_Dispatch_Unit( op, cycle, false );

  if( !Probe_Resources( cycle, op, dispatch_unit, true ) ){
    ASSERT( false );
  }

  // Update the decoding info.
  Resource_Table_Entry* entry = Resource_Table[cycle];
  const TOP top = OP_code( op );

  ASSERT( entry->dispatched_ops < issue_rate );
  entry->dispatched_ops++;

  switch( cur_res ){
  case FPU:
    entry->fpu_ops++;
    break;
  case ALU:
    entry->alu_ops++;
    break;
  case AGU:
    entry->agu_ops++;
    break;
  }

  if( OPR_imm_size(opr) ) {
    sched_group_imm_size += OPR_imm_size(opr);
    if( OPR_has_64bit_imm(opr) )
      sched_group_64bit_imm_count++;
  }

  if( TOP_is_fastpath_db(top) )
    entry->fp_dbl_ops++;

  if( OP_is_memory_store( op ) )
    entry->store_ops++;

  if (OP_is_memory_load( op ) )
    entry->load_ops++;

  OP_dgroup(op) = OPR_sched_group(opr);
  OPR_bb_id(opr) = BB_id(OP_bb(op));
  Add_OPR_To_Current_Window( opr, op );
}


void DSP_SCH::Build_Ready_Vector()
{
  OP* op;

  FOR_ALL_BB_OPs_FWD (bb, op){
    OPR* opr = Get_OPR(op);
    if( OPR_num_preds(opr) == 0 ){
      VECTOR_Add_Element( _ready_vector, op );
    }
  }
}


void DSP_SCH::Init()
{
  OP* op = NULL;
  RID* rid = BB_rid(bb);

  // Init schedule info.
  _ready_vector = VECTOR_Init( BB_length(bb), mem_pool );
  _sched_vector = VECTOR_Init( BB_length(bb)*2, mem_pool );

  can_sched = true;
  sched_order = 0;
  BB_contains_x87 = false;
  BB_fails_rid = false;
  sched_group_64bit_imm_count = 0;

  Init_Window_Data();

  BB_offset(bb) = 0;

  int max_indx = 0;
  FOR_ALL_BB_OPs(bb, op){
    max_indx = std::max( max_indx, OP_map_idx(op) );
    if( OP_x87(op) )
      BB_contains_x87 = true;
  }

  // Also true of rid blocks which fail the sched test.
  if( rid != NULL && RID_level( rid ) >= RL_CGSCHED ){
    BB_fails_rid = true;
  }

  if( BB_contains_x87 || BB_call(bb) || BB_fails_rid )
    can_sched = false; 

  // make some room for nops
  max_indx += (max_indx / 2);

  opr_array = (OPR*) MEM_POOL_Alloc( mem_pool,
				     sizeof(opr_array[0]) * max_indx );
  bzero( opr_array, ( sizeof(opr_array[0]) * max_indx ) );

  // Init resource table.
  int rtable_size = 0;
  int max_resource_cycles = 0;

  FOR_ALL_BB_OPs_FWD(bb, op){
    INT cur_resource_cycles = TI_RES_Cycle_Count( OP_code(op) );
    if( cur_resource_cycles > max_resource_cycles ){
      max_resource_cycles = cur_resource_cycles;
    }
    INT op_latency = cur_resource_cycles;
    for( ARC_LIST* arcs = OP_succs(op); arcs != NULL; 
         arcs = ARC_LIST_rest(arcs) ){
      ARC *arc = ARC_LIST_first(arcs);
      if( ARC_latency(arc) > op_latency ){
	op_latency = ARC_latency(arc);
      }
    }
    rtable_size += op_latency;
  }

  // Increase table size by the maximum number of resource cycles needed by
  // any OP.
  rtable_size += max_resource_cycles;
  _U = rtable_size;

  mrt.Init( bb, this, _U, trace, mem_pool );
}


void DSP_SCH::Reorder_BB()
{
  ASSERT( VECTOR_count(_sched_vector) == BB_length(bb) );
  BB_Remove_All( bb );

  for( int i = 0; i < VECTOR_count( _sched_vector ); i++ ){
    OP* op = OP_VECTOR_element( _sched_vector, i );
    OPR* opr = Get_OPR(op);
    BB_Append_Op( bb, op );
  }
}

int Find_Imm_Specific( const TOP top )
{
  int specific_imm_size = 0;
  switch( top ){
  case TOP_cmpi8:
  case TOP_cmpxi8:
  case TOP_cmpxxi8:
  case TOP_cmpxxxi8:
  case TOP_rori8:
  case TOP_roli8:
  case TOP_addi8:
  case TOP_subi8:
  case TOP_andi8:
  case TOP_ori8:
  case TOP_xori8:
  case TOP_sari32:
  case TOP_sari64:
  case TOP_shli32:
  case TOP_shri32:
  case TOP_shli64:
  case TOP_shri64:
  case TOP_rori32:
  case TOP_roli32:
  case TOP_rori64:
  case TOP_roli64:
  case TOP_shldi32:
  case TOP_shrdi32:
    specific_imm_size = 1;
    break;

  case TOP_cmpi16:
  case TOP_cmpxi16:
  case TOP_cmpxxi16:
  case TOP_cmpxxxi16:
  case TOP_rori16:
  case TOP_roli16:
    specific_imm_size = 2;
    break;

  case TOP_adci32:
  case TOP_cmpi32:
  case TOP_cmpxi32:
  case TOP_cmpxxi32:
  case TOP_cmpxxxi32:
  case TOP_imuli32:
  case TOP_storenti32:
  case TOP_sbbi32:
  case TOP_testi32:
  case TOP_movi32_2m:
  case TOP_movm_2i32:
  case TOP_cmpi64:
  case TOP_cmpxi64:
  case TOP_cmpxxi64:
  case TOP_cmpxxxi64:
  case TOP_imuli64:
  case TOP_storenti64:
  case TOP_testi64:
  case TOP_movi64_2m:
  case TOP_movm_2i64:
    specific_imm_size = 4;
    break;

  default:
    break;
  }

  return specific_imm_size;
}


int Calculate_Imm_Range(long val)
{
  int cur_imm_size = 0;
  if( ( val >= -128 ) && ( val <= 127 ) ){
    cur_imm_size = 1;
  } else if( ( val >= -256 ) && ( val <= 255 ) ){
    cur_imm_size = 2;
  } else if( val != 0 ){
    val = (val < 0) ? (val * -1) : val;
    for( int j = 0; j < 8; j++ ){
      if( val > 0 ) 
        cur_imm_size++;
      else
        break;

      // Shift by a byte each time
      val = val >> 8; 
    }
  }

  return cur_imm_size;
}


uint8_t Size_Of_Immediates( OP *op, bool is_memory_op )
{
  int base_idx   = -1;
  int index_idx  = -1;
  int offset_idx = -1;
  int scale_idx  = -1;
  int imm_size = 0; 
  long val;
  OPR* opr = Get_OPR(op);
  const TOP top = OP_code(op);
  int specific_imm_size = Find_Imm_Specific( top );
  bool has_rip_addr = false;
  bool requires_disp = false;

  if( is_memory_op ){
    base_idx   = TOP_Find_Operand_Use(OP_code(op), OU_base);
    index_idx  = TOP_Find_Operand_Use(OP_code(op), OU_index);
    offset_idx = TOP_Find_Operand_Use(OP_code(op), OU_offset);
    scale_idx  = TOP_Find_Operand_Use(OP_code(op), OU_scale);
  }

  // Calculate the number of bytes needed to support displacement and 
  // immediate fields of a given op.
  for (int i = 0; i < OP_opnds(op) ; i++) {
    int cur_imm_size = 0; 

    // Skip inconsequential components
    if( i == base_idx ){
      TN *tn = OP_opnd(op, i);  
      if( TN_is_register(tn) ){
        if( tn == Rip_TN() )
          has_rip_addr = true;
        else if( TN_register(tn) == RBP )
          requires_disp = true;
        else if( TN_register(tn) == R13 )
          requires_disp = true;
      }
      continue;
    }
    if( i == index_idx ) continue;
    if( i == scale_idx ) continue;

    val = 0;
    if( i == offset_idx ){
      if( has_rip_addr == false ) {
        val = (long)OPR_mem_ofst(opr);
        if( ( val == 0 ) && ( requires_disp == true ) ){
          cur_imm_size = 1;
        } else if( ( val >= -128 ) && ( val <= 127 ) ){
          if( ( requires_disp == false ) && ( val != 0 ) )
            cur_imm_size = 1;
        }
      } else {
        cur_imm_size = 4;
      }
    } else {
      TN *tn = OP_opnd(op, i);
      if( TN_has_value(tn) ) {
        if( ( OP_code(op) == TOP_ldc64 ) || ( OP_code(op) == TOP_ldc32 ) ){
          int range_val = TN_value(tn);
          cur_imm_size = ( Calculate_Imm_Range( range_val ) > 4 ) ? 8 : 4;
        } else {
          val = TN_value(tn);
        }
      } else {
        if( ( OP_code(op) == TOP_ldc64 ) || ( OP_code(op) == TOP_ldc32 ) ) {
          // these kinds of syms require full 32bit imm
          if( TN_is_symbol(tn) ){
            ST* st = TN_var(tn);
            if( strcmp (ST_name(st), ".rodata") == 0 )
              cur_imm_size = 4;
          }
        } else if ( OP_call(op) ) {
          if( TN_is_symbol(tn) )
            cur_imm_size = 4;
        }
      }
    }

    if( val != 0 )
      cur_imm_size = Calculate_Imm_Range( val );

    if( ( is_memory_op ) && ( i == offset_idx ) ) {
      // displacment ops come int 1 and 4 byte imm sizes
      if( cur_imm_size > 1 ) 
        imm_size += 4;
      else if( cur_imm_size == 1 )
        imm_size++;
    } else if ( specific_imm_size == 0 ) {
      // all others are 1, 2, or 4
      switch( cur_imm_size ) {
      case 0:
        break;
      case 1:
        imm_size += cur_imm_size;
        break;
      case 8:
        Set_OPR_has_64bit_imm(opr); 
        imm_size += cur_imm_size;
        break;
      default:
        if( cur_imm_size > 1) imm_size += 4;
        break;
      }
    }
  }

  // Use immediate specific size info when applicable.
  if( specific_imm_size > 0 ){
    imm_size += specific_imm_size;
  }

  // Labels will fail the value test for a const value
  if( OP_br(op) && !OP_ijump(op) ) {
    // we will be pessimistic and assume 1 byte, then adjust when
    // we know how big the block is.  We are only doing single
    // nested innermost loops for this, to the offset if the loop size.
    imm_size += 1;
  }

  // instructions of this form with the is4 byte need to be counted too
  if( OP_is4(op) )
    imm_size++;

  return imm_size;
}


bool Uses_AVX_W_bit( const TOP top )
{
  bool uses_w_bit = false;

  switch ( top ) {
  // c4 encoded instructions
  case TOP_vfmaddss:
  case TOP_vfmaddxss:
  case TOP_vfmaddxxss:
  case TOP_vfmaddxxxss:
  case TOP_vfmaddxrss:
  case TOP_vfmaddxxrss:
  case TOP_vfmaddxxxrss:
  case TOP_vfmaddsd:
  case TOP_vfmaddxsd:
  case TOP_vfmaddxxsd:
  case TOP_vfmaddxxxsd:
  case TOP_vfmaddxrsd:
  case TOP_vfmaddxxrsd:
  case TOP_vfmaddxxxrsd:
  case TOP_vfnmaddss:
  case TOP_vfnmaddxss:
  case TOP_vfnmaddxxss:
  case TOP_vfnmaddxxxss:
  case TOP_vfnmaddxrss:
  case TOP_vfnmaddxxrss:
  case TOP_vfnmaddxxxrss:
  case TOP_vfnmaddsd:
  case TOP_vfnmaddxsd:
  case TOP_vfnmaddxxsd:
  case TOP_vfnmaddxxxsd:
  case TOP_vfnmaddxrsd:
  case TOP_vfnmaddxxrsd:
  case TOP_vfnmaddxxxrsd:
  case TOP_vfmaddps:
  case TOP_vfmaddxps:
  case TOP_vfmaddxxps:
  case TOP_vfmaddxxxps:
  case TOP_vfmaddxrps:
  case TOP_vfmaddxxrps:
  case TOP_vfmaddxxxrps:
  case TOP_vfmaddpd:
  case TOP_vfmaddxpd:
  case TOP_vfmaddxxpd:
  case TOP_vfmaddxxxpd:
  case TOP_vfmaddxrpd:
  case TOP_vfmaddxxrpd:
  case TOP_vfmaddxxxrpd:
  case TOP_vfmaddsubps:
  case TOP_vfmaddsubxps:
  case TOP_vfmaddsubxxps:
  case TOP_vfmaddsubxxxps:
  case TOP_vfmaddsubxrps:
  case TOP_vfmaddsubxxrps:
  case TOP_vfmaddsubxxxrps:
  case TOP_vfmaddsubpd:
  case TOP_vfmaddsubxpd:
  case TOP_vfmaddsubxxpd:
  case TOP_vfmaddsubxxxpd:
  case TOP_vfmaddsubxrpd:
  case TOP_vfmaddsubxxrpd:
  case TOP_vfmaddsubxxxrpd:
  case TOP_vfnmaddps:
  case TOP_vfnmaddxps:
  case TOP_vfnmaddxxps:
  case TOP_vfnmaddxxxps:
  case TOP_vfnmaddxrps:
  case TOP_vfnmaddxxrps:
  case TOP_vfnmaddxxxrps:
  case TOP_vfnmaddpd:
  case TOP_vfnmaddxpd:
  case TOP_vfnmaddxxpd:
  case TOP_vfnmaddxxxpd:
  case TOP_vfnmaddxrpd:
  case TOP_vfnmaddxxrpd:
  case TOP_vfnmaddxxxrpd:
  case TOP_vfmsubss:
  case TOP_vfmsubxss:
  case TOP_vfmsubxxss:
  case TOP_vfmsubxxxss:
  case TOP_vfmsubxrss:
  case TOP_vfmsubxxrss:
  case TOP_vfmsubxxxrss:
  case TOP_vfmsubsd:
  case TOP_vfmsubxsd:
  case TOP_vfmsubxxsd:
  case TOP_vfmsubxxxsd:
  case TOP_vfmsubxrsd:
  case TOP_vfmsubxxrsd:
  case TOP_vfmsubxxxrsd:
  case TOP_vfnmsubss:
  case TOP_vfnmsubxss:
  case TOP_vfnmsubxxss:
  case TOP_vfnmsubxxxss:
  case TOP_vfnmsubxrss:
  case TOP_vfnmsubxxrss:
  case TOP_vfnmsubxxxrss:
  case TOP_vfnmsubsd:
  case TOP_vfnmsubxsd:
  case TOP_vfnmsubxxsd:
  case TOP_vfnmsubxxxsd:
  case TOP_vfnmsubxrsd:
  case TOP_vfnmsubxxrsd:
  case TOP_vfnmsubxxxrsd:
  case TOP_vfmsubps:
  case TOP_vfmsubxps:
  case TOP_vfmsubxxps:
  case TOP_vfmsubxxxps:
  case TOP_vfmsubxrps:
  case TOP_vfmsubxxrps:
  case TOP_vfmsubxxxrps:
  case TOP_vfmsubpd:
  case TOP_vfmsubxpd:
  case TOP_vfmsubxxpd:
  case TOP_vfmsubxxxpd:
  case TOP_vfmsubxrpd:
  case TOP_vfmsubxxrpd:
  case TOP_vfmsubxxxrpd:
  case TOP_vfmsubaddps:
  case TOP_vfmsubaddxps:
  case TOP_vfmsubaddxxps:
  case TOP_vfmsubaddxxxps:
  case TOP_vfmsubaddxrps:
  case TOP_vfmsubaddxxrps:
  case TOP_vfmsubaddxxxrps:
  case TOP_vfmsubaddpd:
  case TOP_vfmsubaddxpd:
  case TOP_vfmsubaddxxpd:
  case TOP_vfmsubaddxxxpd:
  case TOP_vfmsubaddxrpd:
  case TOP_vfmsubaddxxrpd:
  case TOP_vfmsubaddxxxrpd:
  case TOP_vfnmsubps:
  case TOP_vfnmsubxps:
  case TOP_vfnmsubxxps:
  case TOP_vfnmsubxxxps:
  case TOP_vfnmsubxrps:
  case TOP_vfnmsubxxrps:
  case TOP_vfnmsubxxxrps:
  case TOP_vfnmsubpd:
  case TOP_vfnmsubxpd:
  case TOP_vfnmsubxxpd:
  case TOP_vfnmsubxxxpd:
  case TOP_vfnmsubxrpd:
  case TOP_vfnmsubxxrpd:
  case TOP_vfnmsubxxxrpd:
  // instructions with Of38h or 0f3ah encoding
  case TOP_xfmadd132xpd:
  case TOP_xfmadd132xxpd:
  case TOP_xfmadd132xxxpd:
  case TOP_xfmadd213xpd:
  case TOP_xfmadd213xxpd:
  case TOP_xfmadd213xxxpd:
  case TOP_xfmadd231xpd:
  case TOP_xfmadd231xxpd:
  case TOP_xfmadd231xxxpd:
  case TOP_xfmadd132xps:
  case TOP_xfmadd132xxps:
  case TOP_xfmadd132xxxps:
  case TOP_xfmadd213xps:
  case TOP_xfmadd213xxps:
  case TOP_xfmadd213xxxps:
  case TOP_xfmadd231xps:
  case TOP_xfmadd231xxps:
  case TOP_xfmadd231xxxps:
  case TOP_xfmadd132xsd:
  case TOP_xfmadd132xxsd:
  case TOP_xfmadd132xxxsd:
  case TOP_xfmadd213xsd:
  case TOP_xfmadd213xxsd:
  case TOP_xfmadd213xxxsd:
  case TOP_xfmadd231xsd:
  case TOP_xfmadd231xxsd:
  case TOP_xfmadd231xxxsd:
  case TOP_xfmadd132xss:
  case TOP_xfmadd132xxss:
  case TOP_xfmadd132xxxss:
  case TOP_xfmadd213xss:
  case TOP_xfmadd213xxss:
  case TOP_xfmadd213xxxss:
  case TOP_xfmadd231xss:
  case TOP_xfmadd231xxss:
  case TOP_xfmadd231xxxss:
  case TOP_xfmaddsub132xpd:
  case TOP_xfmaddsub132xxpd:
  case TOP_xfmaddsub132xxxpd:
  case TOP_xfmaddsub213xpd:
  case TOP_xfmaddsub213xxpd:
  case TOP_xfmaddsub213xxxpd:
  case TOP_xfmaddsub231xpd:
  case TOP_xfmaddsub231xxpd:
  case TOP_xfmaddsub231xxxpd:
  case TOP_xfmaddsub132xps:
  case TOP_xfmaddsub132xxps:
  case TOP_xfmaddsub132xxxps:
  case TOP_xfmaddsub213xps:
  case TOP_xfmaddsub213xxps:
  case TOP_xfmaddsub213xxxps:
  case TOP_xfmaddsub231xps:
  case TOP_xfmaddsub231xxps:
  case TOP_xfmaddsub231xxxps:
  case TOP_xfmsubadd132xpd:
  case TOP_xfmsubadd132xxpd:
  case TOP_xfmsubadd132xxxpd:
  case TOP_xfmsubadd213xpd:
  case TOP_xfmsubadd213xxpd:
  case TOP_xfmsubadd213xxxpd:
  case TOP_xfmsubadd231xpd:
  case TOP_xfmsubadd231xxpd:
  case TOP_xfmsubadd231xxxpd:
  case TOP_xfmsubadd132xps:
  case TOP_xfmsubadd132xxps:
  case TOP_xfmsubadd132xxxps:
  case TOP_xfmsubadd213xps:
  case TOP_xfmsubadd213xxps:
  case TOP_xfmsubadd213xxxps:
  case TOP_xfmsubadd231xps:
  case TOP_xfmsubadd231xxps:
  case TOP_xfmsubadd231xxxps:
  case TOP_xfmsub132xpd:
  case TOP_xfmsub132xxpd:
  case TOP_xfmsub132xxxpd:
  case TOP_xfmsub213xpd:
  case TOP_xfmsub213xxpd:
  case TOP_xfmsub213xxxpd:
  case TOP_xfmsub231xpd:
  case TOP_xfmsub231xxpd:
  case TOP_xfmsub231xxxpd:
  case TOP_xfmsub132xps:
  case TOP_xfmsub132xxps:
  case TOP_xfmsub132xxxps:
  case TOP_xfmsub213xps:
  case TOP_xfmsub213xxps:
  case TOP_xfmsub213xxxps:
  case TOP_xfmsub231xps:
  case TOP_xfmsub231xxps:
  case TOP_xfmsub231xxxps:
  case TOP_xfmsub132xsd:
  case TOP_xfmsub132xxsd:
  case TOP_xfmsub132xxxsd:
  case TOP_xfmsub213xsd:
  case TOP_xfmsub213xxsd:
  case TOP_xfmsub213xxxsd:
  case TOP_xfmsub231xsd:
  case TOP_xfmsub231xxsd:
  case TOP_xfmsub231xxxsd:
  case TOP_xfmsub132xss:
  case TOP_xfmsub132xxss:
  case TOP_xfmsub132xxxss:
  case TOP_xfmsub213xss:
  case TOP_xfmsub213xxss:
  case TOP_xfmsub213xxxss:
  case TOP_xfmsub231xss:
  case TOP_xfmsub231xxss:
  case TOP_xfmsub231xxxss:
  case TOP_xfnmadd132xpd:
  case TOP_xfnmadd132xxpd:
  case TOP_xfnmadd132xxxpd:
  case TOP_xfnmadd213xpd:
  case TOP_xfnmadd213xxpd:
  case TOP_xfnmadd213xxxpd:
  case TOP_xfnmadd231xpd:
  case TOP_xfnmadd231xxpd:
  case TOP_xfnmadd231xxxpd:
  case TOP_xfnmadd132xps:
  case TOP_xfnmadd132xxps:
  case TOP_xfnmadd132xxxps:
  case TOP_xfnmadd213xps:
  case TOP_xfnmadd213xxps:
  case TOP_xfnmadd213xxxps:
  case TOP_xfnmadd231xps:
  case TOP_xfnmadd231xxps:
  case TOP_xfnmadd231xxxps:
  case TOP_xfnmadd132xsd:
  case TOP_xfnmadd132xxsd:
  case TOP_xfnmadd132xxxsd:
  case TOP_xfnmadd213xsd:
  case TOP_xfnmadd213xxsd:
  case TOP_xfnmadd213xxxsd:
  case TOP_xfnmadd231xsd:
  case TOP_xfnmadd231xxsd:
  case TOP_xfnmadd231xxxsd:
  case TOP_xfnmadd132xss:
  case TOP_xfnmadd132xxss:
  case TOP_xfnmadd132xxxss:
  case TOP_xfnmadd213xss:
  case TOP_xfnmadd213xxss:
  case TOP_xfnmadd213xxxss:
  case TOP_xfnmadd231xss:
  case TOP_xfnmadd231xxss:
  case TOP_xfnmadd231xxxss:
  case TOP_xfnmsub132xpd:
  case TOP_xfnmsub132xxpd:
  case TOP_xfnmsub132xxxpd:
  case TOP_xfnmsub213xpd:
  case TOP_xfnmsub213xxpd:
  case TOP_xfnmsub213xxxpd:
  case TOP_xfnmsub231xpd:
  case TOP_xfnmsub231xxpd:
  case TOP_xfnmsub231xxxpd:
  case TOP_xfnmsub132xps:
  case TOP_xfnmsub132xxps:
  case TOP_xfnmsub132xxxps:
  case TOP_xfnmsub213xps:
  case TOP_xfnmsub213xxps:
  case TOP_xfnmsub213xxxps:
  case TOP_xfnmsub231xps:
  case TOP_xfnmsub231xxps:
  case TOP_xfnmsub231xxxps:
  case TOP_xfnmsub132xsd:
  case TOP_xfnmsub132xxsd:
  case TOP_xfnmsub132xxxsd:
  case TOP_xfnmsub213xsd:
  case TOP_xfnmsub213xxsd:
  case TOP_xfnmsub213xxxsd:
  case TOP_xfnmsub231xsd:
  case TOP_xfnmsub231xxsd:
  case TOP_xfnmsub231xxxsd:
  case TOP_xfnmsub132xss:
  case TOP_xfnmsub132xxss:
  case TOP_xfnmsub132xxxss:
  case TOP_xfnmsub213xss:
  case TOP_xfnmsub213xxss:
  case TOP_xfnmsub213xxxss:
  case TOP_xfnmsub231xss:
  case TOP_xfnmsub231xxss:
  case TOP_xfnmsub231xxxss:
  case TOP_vaesenc:
  case TOP_vaesencx:
  case TOP_vaesencxx:
  case TOP_vaesencxxx:
  case TOP_vaesenclast:
  case TOP_vaesenclastx:
  case TOP_vaesenclastxx:
  case TOP_vaesenclastxxx:
  case TOP_vaesdec:
  case TOP_vaesdecx:
  case TOP_vaesdecxx:
  case TOP_vaesdecxxx:
  case TOP_vaesdeclast:
  case TOP_vaesdeclastx:
  case TOP_vaesdeclastxx:
  case TOP_vaesdeclastxxx:
  case TOP_vaesimc:
  case TOP_vaesimcx:
  case TOP_vaesimcxx:
  case TOP_vaesimcxxx:
  case TOP_vaeskeygenassist:
  case TOP_vaeskeygenassistx:
  case TOP_vaeskeygenassistxx:
  case TOP_vaeskeygenassistxxx:
  case TOP_vfblend128v64:
  case TOP_vfblendx128v64:
  case TOP_vfblendxx128v64:
  case TOP_vfblendxxx128v64:
  case TOP_vfblend128v32:
  case TOP_vfblendx128v32:
  case TOP_vfblendxx128v32:
  case TOP_vfblendxxx128v32:
  case TOP_vfblendv128v64:
  case TOP_vfblendvx128v64:
  case TOP_vfblendvxx128v64:
  case TOP_vfblendvxxx128v64:
  case TOP_vfblendv128v32:
  case TOP_vfblendvx128v32:
  case TOP_vfblendvxx128v32:
  case TOP_vfblendvxxx128v32:
  case TOP_vfdp128v64:
  case TOP_vfdpx128v64:
  case TOP_vfdpxx128v64:
  case TOP_vfdpxxx128v64:
  case TOP_vfdp128v32:
  case TOP_vfdpx128v32:
  case TOP_vfdpxx128v32:
  case TOP_vfdpxxx128v32:
  case TOP_vfextrf128:
  case TOP_vfextrxf128:
  case TOP_vfextrxxf128:
  case TOP_vfextrxxxf128:
  case TOP_vfextr128v32:
  case TOP_vfextrx128v32:
  case TOP_vfextrxx128v32:
  case TOP_vfextrxxx128v32:
  case TOP_vfinsrf128:
  case TOP_vfinsrxf128:
  case TOP_vfinsrxxf128:
  case TOP_vfinsrxxxf128:
  case TOP_vfinsr128v32:
  case TOP_vfinsrx128v32:
  case TOP_vfinsrxx128v32:
  case TOP_vfinsrxxx128v32:
  case TOP_vmpsadbw:
  case TOP_vmpsadbwx:
  case TOP_vmpsadbwxx:
  case TOP_vmpsadbwxxx:
  case TOP_vpalignr128:
  case TOP_vpalignrx128:
  case TOP_vpalignrxx128:
  case TOP_vpalignrxxx128:
  case TOP_vblendv128v8:
  case TOP_vblendvx128v8:
  case TOP_vblendvxx128v8:
  case TOP_vblendvxxx128v8:
  case TOP_vblend128v16:
  case TOP_vblendx128v16:
  case TOP_vblendxx128v16:
  case TOP_vblendxxx128v16:
  case TOP_vpclmulqdq:
  case TOP_vpclmulqdqx:
  case TOP_vpclmulqdqxx:
  case TOP_vpclmulqdqxxx:
  case TOP_vcmpestri:
  case TOP_vcmpestrix:
  case TOP_vcmpestrixx:
  case TOP_vcmpestrixxx:
  case TOP_vcmpestrm:
  case TOP_vcmpestrmx:
  case TOP_vcmpestrmxx:
  case TOP_vcmpestrmxxx:
  case TOP_vfperm128v64:
  case TOP_vfpermx128v64:
  case TOP_vfpermxx128v64:
  case TOP_vfpermxxx128v64:
  case TOP_vfpermi128v64:
  case TOP_vfpermix128v64:
  case TOP_vfpermixx128v64:
  case TOP_vfpermixxx128v64:
  case TOP_vfperm128v32:
  case TOP_vfpermx128v32:
  case TOP_vfpermxx128v32:
  case TOP_vfpermxxx128v32:
  case TOP_vfpermi128v32:
  case TOP_vfpermix128v32:
  case TOP_vfpermixx128v32:
  case TOP_vfpermixxx128v32:
  case TOP_vfperm2f128:
  case TOP_vfperm2xf128:
  case TOP_vfperm2xxf128:
  case TOP_vfperm2xxxf128:
  case TOP_vextr128v8:
  case TOP_vextrx128v8:
  case TOP_vextrxx128v8:
  case TOP_vextrxxx128v8:
  case TOP_vextr128v32:
  case TOP_vextrx128v32:
  case TOP_vextrxx128v32:
  case TOP_vextrxxx128v32:
  case TOP_vextr128v64:
  case TOP_vextrx128v64:
  case TOP_vextrxx128v64:
  case TOP_vextrxxx128v64:
  case TOP_vextr128v16:
  case TOP_vextrx128v16:
  case TOP_vextrxx128v16:
  case TOP_vextrxxx128v16:
  case TOP_vinsr128v8:
  case TOP_vinsrx128v8:
  case TOP_vinsrxx128v8:
  case TOP_vinsrxxx128v8:
  case TOP_vinsr128v32:
  case TOP_vinsrx128v32:
  case TOP_vinsrxx128v32:
  case TOP_vinsrxxx128v32:
  case TOP_vinsr128v64:
  case TOP_vinsrx128v64:
  case TOP_vinsrxx128v64:
  case TOP_vinsrxxx128v64:
  case TOP_vinsr128v16:
  case TOP_vinsrx128v16:
  case TOP_vinsrxx128v16:
  case TOP_vinsrxxx128v16:
  case TOP_vround128v64:
  case TOP_vroundx128v64:
  case TOP_vroundxx128v64:
  case TOP_vroundxxx128v64:
  case TOP_vround128v32:
  case TOP_vroundx128v32:
  case TOP_vroundxx128v32:
  case TOP_vroundxxx128v32:
  case TOP_vroundsd:
  case TOP_vroundxsd:
  case TOP_vroundxxsd:
  case TOP_vroundxxxsd:
  case TOP_vroundss:
  case TOP_vroundxss:
  case TOP_vroundxxss:
  case TOP_vroundxxxss:
  case TOP_vfbroadcastss:
  case TOP_vfbroadcastxss:
  case TOP_vfbroadcastxxss:
  case TOP_vfbroadcastsd:
  case TOP_vfbroadcastxsd:
  case TOP_vfbroadcastxxsd:
  case TOP_vfbroadcastf128:
  case TOP_vfbroadcastxf128:
  case TOP_vfbroadcastxxf128:
  case TOP_vcmpeq128v64:
  case TOP_vcmpeqx128v64:
  case TOP_vcmpeqxx128v64:
  case TOP_vcmpeqxxx128v64:
  case TOP_vcmpgt128v64:
  case TOP_vcmpgtx128v64:
  case TOP_vcmpgtxx128v64:
  case TOP_vcmpgtxxx128v64:
    uses_w_bit = true;
    break;
  default:
    break;
  }
 
  return uses_w_bit;
}


bool Instruction_Uses_VEX( const TOP top )
{
  bool uses_avx = false;

  if( Is_Target_AVX() ){
    uses_avx = TOP_is_avx( top );
  }

  return uses_avx;
}

bool Vex_B_Needed( TOP top )
{
  bool vex_b_needed = false;

  switch ( top ) {
  case TOP_vmovdqa:
  case TOP_vmovaps:
  case TOP_vmovapd:
    vex_b_needed = true;
    break;
  default:
    break;
  }

  return vex_b_needed;
}

bool Vex_W_Needed( TOP top )
{
  bool vex_w_needed = true;

  switch ( top ) {
  case TOP_vcvtdq2pd:
  case TOP_vcvtdq2pdx:
  case TOP_vcvtdq2pdxx:
  case TOP_vcvtdq2pdxxx:
  case TOP_vcvtdq2ps:
  case TOP_vcvtdq2psx:
  case TOP_vcvtdq2psxx:
  case TOP_vcvtdq2psxxx:
  case TOP_vcvtpd2dq:
  case TOP_vcvtpd2dqx:
  case TOP_vcvtpd2dqxx:
  case TOP_vcvtpd2dqxxx:
  case TOP_vcvtpd2dqy:
  case TOP_vcvtpd2dqyx:
  case TOP_vcvtpd2dqyxx:
  case TOP_vcvtpd2dqyxxx:
  case TOP_vcvtpd2ps:
  case TOP_vcvtpd2psx:
  case TOP_vcvtpd2psy:
  case TOP_vcvtpd2psyx:
  case TOP_vcvtpd2psyxx:
  case TOP_vcvtpd2psyxxx:
  case TOP_vcvtps2dq:
  case TOP_vcvtps2dqx:
  case TOP_vcvtps2dqxx:
  case TOP_vcvtps2dqxxx:
  case TOP_vcvtps2pd:
  case TOP_vcvtps2pdx:
  case TOP_vcvtps2pdxx:
  case TOP_vcvtps2pdxxx:
    vex_w_needed = false;
    break;
  default:
    break;
  }

  return vex_w_needed;
}


bool Reg_Uses_REX( TN* opnd, bool constrain_to_upper, 
                   bool uses_avx, TOP top, int opnd_idx, bool is_src_opnd )
{
  bool uses_rex = false;

  if( constrain_to_upper == false ) {
    constrain_to_upper = !Vex_W_Needed( top );
  }

  if( Is_Target_32bit() )
    return uses_rex;

  if( TN_register_class(opnd) == ISA_REGISTER_CLASS_integer ) {
    switch( TN_register(opnd) ) {
    case RAX:
    case RBX:
    case RCX:
    case RDX:
    case RDI:
    case RSI:
    case RBP:
    case RSP:
      if( ( TN_size(opnd) == 8 ) && ( constrain_to_upper == false ) )
        uses_rex = true;
      break;
    case R8:
    case R9:
    case R10:
    case R11:
    case R12:
    case R13:
    case R14:
    case R15:
    default:
      uses_rex = true;
      break;
    }
  } else if( TN_register_class(opnd) == ISA_REGISTER_CLASS_float ) {
    int reg_num = TN_register(opnd) + Float_Preg_Min_Offset - 1;
    switch( reg_num ) {
    case XMM8:
    case XMM9:
    case XMM10:
    case XMM11:
    case XMM12:
    case XMM13:
    case XMM14:
    case XMM15:
    {
      if( uses_avx ){
        // look for REX.X or REX.W, else REX.R or vvvv handles this for AVX
        if( is_src_opnd && ( opnd_idx != 0 ) )
          uses_rex = true;
        else if ( is_src_opnd && Vex_B_Needed( top ) )
          uses_rex = true;
      } else {
        uses_rex = true;
      }
      break; 
    }
    default:
      break;
    }
  }

  return uses_rex;
}


bool OP_has_multi_result( const TOP top )
{
  bool has_multi_res = false;

  if( Is_Target_32bit() )
    return has_multi_res;

  switch( top ){
  case TOP_vmovlhps:
    has_multi_res = true; 
    break;
  default:
    break;
  }

  return has_multi_res;
}

bool OP_has_implicit_REX( const TOP top )
{
  bool has_rex = false;

  if( Is_Target_32bit() )
    return has_rex;

  switch( top ){
  case TOP_crc32w:
  case TOP_crc32wx:
  case TOP_crc32wxx:
  case TOP_crc32wxxx:
  case TOP_crc32d:
  case TOP_crc32dx:
  case TOP_crc32dxx:
  case TOP_crc32dxxx:
  case TOP_crc32q:
  case TOP_crc32qx:
  case TOP_crc32qxx:
  case TOP_crc32qxxx:
  case TOP_crc32b:
  case TOP_crc32bx:
  case TOP_crc32bxx:
  case TOP_crc32bxxx:
  case TOP_mov64:
  case TOP_movslq:
    has_rex = true; 
    break;
  default:
    break;
  }

  return has_rex;
}


void DSP_SCH::Compute_Insn_Size( OP *op )
{
  OPR* opr = Get_OPR(op);
  const TOP top = OP_code(op);
  uint8_t insn_size = 0;
  uint8_t vex_encoding_size = 0;
  bool uses_avx = Instruction_Uses_VEX( top );
  bool rex_seen = false;
  bool has_modrm = false;
  bool has_sib = false;
  bool is_memory = false;
  bool constrain_to_upper = false;

  // Do not determine the insn size if we do not emit it.
  if ( OP_dummy(op) )
    return;
  
  // What is opcode size?
  insn_size = TOP_is_opcode_1(top) ? 1 : 
                TOP_is_opcode_2(top) ? 2 : 
                  TOP_is_opcode_3(top) ? 3 : 
                    TOP_is_opcode_4(top) ? 4 : 0;

  // TODO: assert if insn_size still 0

  // Test for existance of modrm byte.
  // A modrm byte is used for any source which 
  // utitilizes a register for either for holding a value
  // or reading one from memory and is not an imm field.
  if( !mrt.TOP_is_push( top ) && !mrt.TOP_is_pop( top ) ) {
    for (int i = 0; i < OP_opnds(op) ; i++) {
      TN* opnd = OP_opnd(op, i);
      if( TN_is_register(opnd) ){
        if( ( TN_register_class(opnd) == ISA_REGISTER_CLASS_integer ) || 
            ( TN_register_class(opnd) == ISA_REGISTER_CLASS_float ) ) {
          bool need_modrm = true;
          // TOP_adci64 TOP_sbbi64 are not valid TOPs. Should they be?
          if (TN_register(opnd) == RAX) {
            switch(top) {
            case TOP_adci32:
            case TOP_addi32:
            case TOP_addi64:
            case TOP_andi32:
            case TOP_andi64:
            case TOP_cmpi32:
            case TOP_cmpi64:
            case TOP_ori32:
            case TOP_ori64:
            case TOP_sbbi32:
            case TOP_subi32:
            case TOP_subi64:
            case TOP_testi32:
            case TOP_testi64:
            case TOP_xori32:
            case TOP_xori64:
              need_modrm = false;
              break;
            default:
              ;
            }
          }
          if( need_modrm && has_modrm == false ){
            has_modrm = true;
            insn_size++;
            break;
          }
        } 
      }
    }
    // xor insns are recorded as result only, but really do have modrm form
    // load const is also a modrm form
    if( ( has_modrm == false ) && 
        ( ( top == TOP_zero32 ) ||
          ( top == TOP_zero64 ) ||
          ( top == TOP_ldc64  ) ) ){
      has_modrm = true;
      insn_size++;
    }
  }

  // Do we have a SIB byte?
  int index_op = TOP_Find_Operand_Use( top, OU_index );
  if ( index_op != -1 ){
    TN* opnd = OP_opnd(op, index_op);
    WN *mem_wn = Get_WN_From_Memory_OP( op );
    if( mem_wn ){
      TYPE_ID rtype = WN_rtype(mem_wn);
      if ( uses_avx == false )
        constrain_to_upper = ( MTYPE_byte_size(rtype) < 8 );
      else
        constrain_to_upper = true;

      // now configure usage REX.X or not based on rtype
      rex_seen = Reg_Uses_REX( opnd, constrain_to_upper, 
                               uses_avx, top, index_op, true );
    } else {
      // no mem op rtype info, rely on operand size for REX.X case
      rex_seen = Reg_Uses_REX( opnd, false, uses_avx, top, index_op, true );
    }
    has_sib = true;
  } 

  // Count all bytes used for immediates
  insn_size += OPR_imm_size(opr);

  if( mrt.OP_is_memory_load( op ) || mrt.OP_is_memory_store( op ) ){
    is_memory = true;
  }
 
  if( !mrt.TOP_is_push( top ) && !mrt.TOP_is_pop( top ) ) {
    // Now see if a register utilizes the upper rank of regs, if so 
    // we have a rex byte.
    for( int i = 0; i < OP_opnds(op); i++ ){
      TN* opnd = OP_opnd(op, i);
      if( i == index_op ) continue;
      if( TN_is_register(opnd) ){
        // First check memory load/store size
        if( i == TOP_Find_Operand_Use( top, OU_base ) ){
          // first check for REX.X case in base reg(special SIB case)
          if ( TN_register_class(opnd) == ISA_REGISTER_CLASS_integer ) {
            if( TN_register(opnd) == R12 ) {
              has_sib = true;
            } else if(  TN_register(opnd) == RSP ) {
              has_sib = true;
              break;
            }
          }

          if( rex_seen ) continue;

          // Now do a general check
          WN *mem_wn = Get_WN_From_Memory_OP( op );
          if( mem_wn ){
            TYPE_ID rtype = WN_rtype(mem_wn);
            // This is the REX.B case
            // constrain_to_upper = ( MTYPE_byte_size(rtype) < 8 );
            if( Reg_Uses_REX( opnd, true, 
                              uses_avx, top, i, true ) )
              rex_seen = true;
          } else {
            // This is the REX.B case
            if( Reg_Uses_REX( opnd, true, uses_avx, top, i, true ) )
              rex_seen = true;
          }
        } else {
          // This is the REX.B or REX.W case
          if( Reg_Uses_REX( opnd, false, uses_avx, top, i, true ) )
            rex_seen = true;
        }
      }
    }

    // Now check the result for REX.R modrm regs 
    if( ( rex_seen == false ) && ( uses_avx == false ) ){
      int results = OP_results(op);
      for( int i = 0; i < results; i++) {
        TN *opnd = OP_result(op, i);
        if( TN_is_register(opnd) ){
          const ISA_OPERAND_INFO *oinfo = ISA_OPERAND_Info( top );
          const ISA_OPERAND_VALTYP *otype = 
            ISA_OPERAND_INFO_Operand( oinfo, i );
          int num_bits = ISA_OPERAND_VALTYP_Size(otype);

          // If this insn was defined to have a result that is larger
          // than 32bits, test it for REX bit usage via REX.R
          if(( num_bits > 32 ) && ( uses_avx == false ) ){
            if( Reg_Uses_REX( opnd, false, uses_avx, top, i, false ) ){
              rex_seen = true;
              break;
            }
          } else if ( TN_register_class(opnd) == ISA_REGISTER_CLASS_integer ){
            if( Reg_Uses_REX( opnd, true, uses_avx, top, i, false ) ){
              rex_seen = true;
              break;
            }
          } else if ( TN_register_class(opnd) == ISA_REGISTER_CLASS_float ){
            if( Reg_Uses_REX( opnd, true, uses_avx, top, i, false ) ){
              rex_seen = true;
              break;
            }
          }
        }
      }
    } else if( ( rex_seen == false ) && 
               ( uses_avx ) && 
               ( OP_has_multi_result(top) ) ){
      // The result operand is used in multiple locations, one as a source
      // operand.
      TN *opnd = OP_result(op, 0);
      if( TN_is_register(opnd) ){
        if( Reg_Uses_REX( opnd, true, uses_avx, top, 1, true ) )
          rex_seen = true;
      }
    } else if( ( rex_seen ) && 
               ( uses_avx ) && 
               ( Vex_B_Needed( top ) ) ) {
      TN *opnd = OP_result(op, 0);
      if( TN_is_register(opnd) ){
        // A special case of VEX.B encoding test: For cases like say
        // vmovdqa, if the source operand requires rex, it is not sufficient
        // that a rex encoding via the VEX 3rd byte is needed.  If both
        // a source operand and the dest are seen in the upper bank, VEX.B 
        // encoding via the 3rd VEX byte is required.
        // Reason: opcode encoding of these instructions map
        //         reg and r/m differently as one is a store and the other a
        //         load, even though both are reg-reg transfers.
        // In this case, if the result is not in the upper bank, we toggle
        // rex_seen to map the store form.
        rex_seen = Reg_Uses_REX( opnd, true, uses_avx, top, 1, true );
      }
    }
  } else if( mrt.TOP_is_push( top ) ) {
    for( int i = 0; i < OP_opnds(op); i++ ){
      TN* opnd = OP_opnd(op, i);
      if( TN_is_register(opnd) ){
        if( Reg_Uses_REX( opnd, true, uses_avx, top, i, true ) ){
          rex_seen = true;
          break;
        }
      }
    }
  } else if( mrt.TOP_is_pop( top ) ) {
    int results = OP_results(op);
    for( int i = 0; i < results; i++) {
      TN *opnd = OP_result(op, i);
      if( TN_is_register(opnd) ){
        if( Reg_Uses_REX( opnd, true, uses_avx, top, i, false ) ){
          rex_seen = true;
          break;
        }
      }
    }
  }

  if( has_sib ){
    insn_size++;
  }

  if( (rex_seen == false) && ( OP_has_implicit_REX( top ) ) ){
    rex_seen = true;
  }

  // If pure REX usage detected, increase the size
  // else use the VEX encoding logic
  if( rex_seen && !uses_avx )
    insn_size++;

  //
  // Count vex bytes. The rules are as follows:
  //
  // if VEX.W=1, VEX.X=0, VEX.B =0 or opts are in map with 0f3a or 0f38
  // then the form is a 3 byte form, else its a 2 byte form.
  // VEX.W = 1 toggles the source operand order for fma4 and vpermil insns
  //           else it is the width field of legacy Simd insns for the upper
  //           bank of gpr or xmm regs.
  // VEX.X = 0 toggles the use to on for the upper bank or xmm or gpr regs
  //           in the SIB.index field, and is bit inverted from the REX.X bit.
  // VEX.B = 0 toggles the use to on for the upper bank or xmm or gpr regs
  //           in the modrm.r/m field and is bit inverted from the REX.B bit.
  //           operand 3 uses this field typically.

  if( uses_avx ) {
    // Initially set the vex encoding size to 2 bytes and discover if 
    // we need the third byte
    vex_encoding_size = 2;
    if( Uses_AVX_W_bit( top ) ) {
      // These are instructions which must use the 3 byte vex encoding
      vex_encoding_size = 3;
    } else if( rex_seen ) {
      // if legacy code under avx uses the rex bits, we have 3 byte form
      vex_encoding_size = 3;
    }
    insn_size += vex_encoding_size;
  }

  OPR_insn_size(opr) = insn_size;
  if( uses_avx )
    OPR_num_pf_bytes(opr) = (vex_encoding_size) ? 2 : 3;
  else
    OPR_num_pf_bytes(opr) = (rex_seen) ? 2 : 3;
}


void DSP_SCH::Build_OPR()
{
  OP* op;

  int mem_ops = 0;
  _true_cp = _cp = 0;

  bzero( defop_by_reg, sizeof(defop_by_reg) );

  FOR_ALL_BB_OPs_FWD(bb, op){
    OPR* opr = Get_OPR(op);
    OPR_release_time(opr) = 0;
    OPR_deadline(opr) = _U-1;
    OPR_sched_order(opr) = _U;
    OPR_sched_group(opr) = _U;
    OPR_pred_order(opr) = _U;

    OPR_uses(opr) = 0;
    OPR_latency(opr) = 0;
    OPR_insn_size(opr) = 0;
    OPR_num_pf_bytes(opr) = 0;
    OPR_bb_id(opr) = 0;

    for( ARC_LIST* arcs = OP_succs(op); arcs != NULL; 
         arcs = ARC_LIST_rest(arcs) ){
      ARC* arc = ARC_LIST_first(arcs);
      if( ARC_kind(arc) == CG_DEP_REGIN ||
	  ARC_kind(arc) == CG_DEP_MEMIN ){
	OPR_uses(opr)++;
      }

      if( ARC_latency(arc) > OPR_latency(opr) )
	OPR_latency(opr) = ARC_latency(arc);
    }

    OPR_mem_index(opr) = OPR_mem_base(opr) = NULL; 
    OPR_mem_sym(opr) = NULL;
    OPR_mem_ofst(opr) = OPR_mem_scale(opr) = OPR_imm_size(opr) = 0;

    for( int i = 0; i < OP_results(op); i++ ){
      TN* result = OP_result(op, i);
      defop_by_reg[TN_register_class(result)][TN_register(result)] = op;
    }

    if( mrt.OP_is_memory_load( op ) || mrt.OP_is_memory_store( op ) ){
      const TOP top = OP_code(op);
      const int index_idx = TOP_Find_Operand_Use(top, OU_index);

      if( index_idx >= 0 ){
	OPR_mem_index(opr) = OP_opnd(op, index_idx);
      }

      const int base_idx = TOP_Find_Operand_Use(top, OU_base);
      if( base_idx >= 0 ){
	OPR_mem_base(opr) = OP_opnd(op, base_idx);
      }

      const int ofst_idx = TOP_Find_Operand_Use(top, OU_offset);
      TN* ofst_tn = ofst_idx >= 0 ? OP_opnd(op, ofst_idx) : NULL;
      int ofst = ofst_tn ? TN_value(ofst_tn) : 0;

      const int scale_idx = TOP_Find_Operand_Use(top, OU_scale);
      TN* scale_tn = scale_idx >= 0 ? OP_opnd(op, scale_idx) : NULL;
      int scale = scale_tn ? TN_value(scale_tn) : 0;
      OPR_mem_scale(opr) = scale;

      if( ofst_tn && TN_is_symbol(ofst_tn) ){
	ST* sym = TN_var(ofst_tn);
	ST* root_sym = NULL;
	INT64 root_offset = 0;
	Base_Symbol_And_Offset( sym, &root_sym, &root_offset );
	if( sym != root_sym ){
	  ofst += root_offset;
	}

	OPR_mem_sym(opr) = root_sym;
      }
      
      OPR_mem_ofst(opr) = ofst;
      OPR_imm_size(opr) += Size_Of_Immediates( op, true );

      mem_ops++;
    } else {
      OPR_imm_size(opr) += Size_Of_Immediates( op, false );
    }
    Compute_Insn_Size( op );
  }

  _true_cp = (int)ceil( mem_ops / 3 );
  mem_ops = (int)ceil( BB_length(bb) / mrt.issue_rate );
  _cp = _true_cp = std::max( _true_cp, mem_ops );

  // Compute the release time only thru true data dependence arcs.
  FOR_ALL_BB_OPs_FWD(bb, op){
    OPR* opr = Get_OPR(op);

    for( ARC_LIST* arcs = OP_succs(op); arcs != NULL; 
         arcs = ARC_LIST_rest(arcs) ){
      ARC* arc = ARC_LIST_first(arcs);

      if( ARC_kind(arc) != CG_DEP_REGIN &&
	  ARC_kind(arc) != CG_DEP_MEMIN )
	continue;

      OP* succ_op = ARC_succ(arc);
      OPR* succ_opr = Get_OPR(succ_op);

      int time = ARC_latency(arc) + OPR_release_time(opr);
      if( OPR_release_time(succ_opr) < time ){
	OPR_release_time(succ_opr) = time;
      }

      _true_cp = std::max( _true_cp, time );
    }
  }

  // Compute the release time.
  FOR_ALL_BB_OPs_FWD(bb, op){
    OPR* opr = Get_OPR(op);

    OPR_issue_time(opr) = OPR_release_time(opr);

    for( ARC_LIST* arcs = OP_succs(op); arcs != NULL; 
         arcs = ARC_LIST_rest(arcs) ){
      ARC* arc = ARC_LIST_first(arcs);
      OP* succ_op = ARC_succ(arc);
      OPR* succ_opr = Get_OPR(succ_op);

      int time = ARC_latency(arc) + OPR_release_time(opr);
      if( OPR_release_time(succ_opr) < time ){
	OPR_release_time(succ_opr) = time;
      }

      _cp = std::max( _cp, time );
      OPR_num_succs(opr)++;
    }
  }

  // Compute the deadline.
  FOR_ALL_BB_OPs_REV(bb, op){
    OPR* opr = Get_OPR(op);

    for( ARC_LIST* arcs = OP_preds(op); arcs != NULL; 
         arcs = ARC_LIST_rest(arcs) ){
      ARC* arc = ARC_LIST_first(arcs);
      OP* pred_op = ARC_pred(arc);
      OPR* pred_opr = Get_OPR(pred_op);

      int time = OPR_deadline(opr) - ARC_latency(arc);
      if( OPR_deadline(pred_opr) > time ){
	OPR_deadline(pred_opr) = time;
	ASSERT( time >= OPR_release_time(pred_opr) );
      }

      OPR_num_preds(opr)++;
    }
  }

  if( trace ){
    FOR_ALL_BB_OPs_FWD(bb, op){
      Print_OP_No_SrcLine(op);
      OPR* opr = Get_OPR(op);
      fprintf( TFile, "release_time:%d deadline:%d num_succs:%d num_preds:%d ",
	       OPR_release_time(opr), OPR_deadline(opr),
	       OPR_num_succs(opr), OPR_num_preds(opr) );
      fprintf( TFile, "uses:%d latency:%d ",
	       OPR_uses(opr), OPR_latency(opr) );
      fprintf( TFile, "\n" );
    }
  }
}


void DSP_SCH::Summary_BB()
{
  if( trace == false )
    return;

  if( _cp > _true_cp + 1 ){
    const bool is_loop_body = 
      ( ANNOT_Get( BB_annotations(bb), ANNOT_LOOPINFO ) != NULL &&
	BB_xfer_op(bb) != NULL );
    const int cycles = 1 + OP_scycle(BB_last_op(bb));

    fprintf( TFile, "%c%s[%d] ops:%d cycles:%d cp:%d true_cp:%d\n",
	     BB_innermost(bb) ? '*' : ' ', Cur_PU_Name, BB_id(bb),
	     BB_length(bb), cycles, _cp, _true_cp );
  }
}


void DSP_SCH::Tighten_Release_Time( OP* op )
{
  OPR* opr = Get_OPR(op);
  ASSERT( OPR_release_time(opr) <= OPR_deadline(opr) );

  for( ARC_LIST* arcs = OP_succs(op); arcs != NULL; 
       arcs = ARC_LIST_rest(arcs) ){
    ARC* arc = ARC_LIST_first(arcs);
    OP* succ_op = ARC_succ(arc);
    OPR* succ_opr = Get_OPR(succ_op);

    int time = ARC_latency(arc) + OPR_release_time(opr);

    if( OPR_release_time(succ_opr) < time ){
      OPR_release_time(succ_opr) = time;
      Tighten_Release_Time(succ_op);
    }
  }
}


int DSP_SCH::Addr_Generation( OP* op )
{
  int time = -1;
  OPR* opr = Get_OPR(op);
  // Assume the address will be calculated as <N> ops are elapsed.
  const unsigned int N = 100;

  if( OPR_mem_index(opr) != NULL ){
    TN* index = OPR_mem_index( opr );
    OP* pred_op = 
      defop_by_reg[TN_register_class(index)][TN_register(index)];
    if( pred_op != NULL ){
      OPR* pred_opr = Get_OPR(pred_op);
      if( sched_order - OPR_sched_order(pred_opr) < N )
	time = MAX( time, OPR_sched_order(pred_opr) );
    }
  }

  if( OPR_mem_base(opr) != NULL ){
    TN* base  = OPR_mem_base(opr);
    OP* pred_op = 
      defop_by_reg[TN_register_class(base)][TN_register(base)];
    if( pred_op != NULL ){
      OPR* pred_opr = Get_OPR(pred_op);
      if( sched_order - OPR_sched_order(pred_opr) < N )
	time = MAX( time, OPR_sched_order(pred_opr) );
    }
  }

  return time;
}


OP* DSP_SCH::Winner( OP* op_a, OP* op_b, int cycle )
{
  OPR* opr_a = Get_OPR(op_a);
  OPR* opr_b = Get_OPR(op_b);
  const TOP top_a = OP_code(op_a);
  const TOP top_b = OP_code(op_b);

  if( CG_branch_fuse ){
    // Try to defer compares, even if the 
    // selected op will break the dispatch group.
    if( OP_icmp(op_a) && !OP_flop(op_a) && !OP_icmp(op_b) ) {
      return op_b;
    }

    // Try to defer compares, even if the 
    // selected op will break the dispatch group.
    if ( OP_icmp(op_b) && !OP_flop(op_b) && !OP_icmp(op_a)) {
      return op_a;
    }
  }

  // Now we choose an op that will fit if one does not
  if( mrt.Decoder_is_Saturated( cycle ) == false ){
    bool can_reserve_a = mrt.Can_Reserve(op_a, cycle); 
    bool can_reserve_b = mrt.Can_Reserve(op_b, cycle); 
    if( can_reserve_a && !can_reserve_b )
      return op_a;
    if( can_reserve_b && !can_reserve_a )
      return op_b;
  }

  // Pick earlier unrolls if present in the loop
  if( OP_unrolling(op_a) > OP_unrolling(op_b) ){
    return op_b;
  }

  // Pick earlier unrolls if present in the loop
  if( OP_unrolling(op_b) > OP_unrolling(op_a) ){
    return op_a;
  }

  // Pick the one with the earliest completion time.
  if( OPR_issue_time(opr_a) != OPR_issue_time(opr_b) ){
    return ( OPR_issue_time(opr_a) < OPR_issue_time(opr_b) ? op_a : op_b );
  }

  // Pick the one to avoid address generation interlocking.
  if( ( mrt.OP_is_memory_load(op_a) || mrt.OP_is_memory_store(op_a) ) &&
      ( mrt.OP_is_memory_load(op_b) || mrt.OP_is_memory_store(op_b) ) ) {
    const int op_a_addr_cal = Addr_Generation(op_a);
    const int op_b_addr_cal = Addr_Generation(op_b);

    // Pick the one whose address is ready.
    if( op_a_addr_cal != op_b_addr_cal ){
      return ( op_a_addr_cal < op_b_addr_cal ? op_a : op_b );
    }

    // Pick the one whose address is close to <last_mem_op>
    if( last_mem_op != NULL ){
      const OPR* opr_last = Get_OPR(last_mem_op);
      const bool a_is_close =
	( ( OPR_mem_base(opr_a) == OPR_mem_base(opr_last) )   &&
	  ( OPR_mem_index(opr_a) == OPR_mem_index(opr_last) ) &&
	  ( OPR_mem_sym(opr_a) == OPR_mem_sym(opr_last) ) );
      const bool b_is_close =
	( ( OPR_mem_base(opr_b) == OPR_mem_base(opr_last) )   &&
	  ( OPR_mem_index(opr_b) == OPR_mem_index(opr_last) ) &&
	  ( OPR_mem_sym(opr_b) == OPR_mem_sym(opr_last) ) );

      if( a_is_close != b_is_close )
	return ( a_is_close ? op_a : op_b );
    }

    // Pick the one with lower address offset.
    if( ( OPR_mem_base(opr_a) == OPR_mem_base(opr_b) ) &&
	( OPR_mem_ofst(opr_a) != OPR_mem_ofst(opr_b) ) ){
      return ( OPR_mem_ofst(opr_a) < OPR_mem_ofst(opr_b) ? op_a : op_b );
    }

    // Pick the load first.
    if( OP_store(op_a) || OP_store(op_b) ){
      return ( OP_store(op_b) ? op_a : op_b );
    }
  }

  // Pick the one with longer latency.
  if( OPR_latency(opr_a) != OPR_latency(opr_b) ){
    return ( OPR_latency(opr_a) > OPR_latency(opr_b) ? op_a : op_b );
  }

  // Pick the one has more uses.
  if( OPR_uses(opr_a) != OPR_uses(opr_b) ){
    return ( OPR_uses(opr_a) > OPR_uses(opr_b) ? op_a : op_b );
  }

  // Pick the one with the earliest deadline.
  if( OPR_deadline(opr_a) != OPR_deadline(opr_b) ){
    return ( OPR_deadline(opr_a) < OPR_deadline(opr_b) ) ? op_a : op_b;
  }

  // Pick the one whose predecessors are scheduled earlier.
  if( OPR_pred_order(opr_a) != OPR_pred_order(opr_b) ){
    return OPR_pred_order(opr_a) < OPR_pred_order(opr_b) ? op_a : op_b;
  }

  // Finally, stick with the original order.
  return OP_map_idx(op_a) < OP_map_idx(op_b) ? op_a : op_b;
}


OP* DSP_SCH::Select_Variable( int cycle )
{
  const int num = VECTOR_count( _ready_vector );
  OP* best = NULL;

  ASSERT(num > 0);

  if( trace ){
    fprintf( TFile, "Ready list:\n" );
    for( int i = 0; i < num; i++ ){
      OP* op = (OP*)VECTOR_element( _ready_vector, i );
      Print_OP_No_SrcLine( op );
    }
  }

  for( int i = 0; i < num; i++ ){
    OP* op = (OP*)VECTOR_element( _ready_vector, i );
    OPR* opr = Get_OPR(op);

    if( OPR_release_time(opr) == cycle ){
      best = ( best == NULL ) ? op : Winner( best, op, cycle );
    } else if( ( best != NULL ) && 
               ( OP_unrolling(op) != OP_unrolling(best) ) ){
      best = Winner( best, op, cycle );
    }
  }

  if( CG_branch_fuse ) { 
    // Force comparison if only a compare was ready.  
    if( best && OP_icmp(best) && !OP_flop(best) )
      best = NULL;
  }

  if( best == NULL ){
    /* If none of the ops in _ready_vector has its operands available,
       then pick one whatever. */
    best = (OP*)VECTOR_element( _ready_vector, 0 );

    for( int i = 1; i < num; i++ ){
      OP* op = (OP*)VECTOR_element( _ready_vector, i );
      OPR* opr = Get_OPR(op);
      best = Winner( best, op, cycle );
    }
  }

  OP_scycle(best) = OPR_issue_time(Get_OPR(best));

  if( trace ){
    fprintf( TFile, "Select: " );
    Print_OP_No_SrcLine( best );
  }

  return best;
}


void DSP_SCH::Schedule_BB()
{
  Build_OPR();
  Build_Ready_Vector();
  int cur_clock = 0;
  bool can_reserve = true;
  OP *last_op;

  last_mem_op = NULL;
  sched_group = 0;
  sched_group_imm_size = 0;

  while( VECTOR_count(_ready_vector) > 0 ){
    OP* op;
    OPR* opr;

    // Carry over our last selected variable(op) when the prior dispatch
    // groups termination conditions were met.
    if( can_reserve ){
      op = Select_Variable( cur_clock );
    } else {
      op = last_op;
    }
    opr = Get_OPR(op);

    if( trace ){
      fprintf( TFile, "Cycle[%d] : issue_time %d",
	       cur_clock, OPR_issue_time( opr ) );
      Print_OP_No_SrcLine( op );
    }

    if( mrt.OP_is_memory_load( op ) || mrt.OP_is_memory_store( op ) ) {
      last_mem_op = op;
    }

    if ( OPR_release_time(opr) > OPR_issue_time(opr) )
      OPR_issue_time(opr) = OPR_release_time(opr);

    can_reserve = false;
    if( mrt.Decoder_is_Saturated( cur_clock ) == false ) {
      can_reserve = mrt.Can_Reserve( op, cur_clock );

      // Check for microcode bubble, these can only schedule alone
      if ( OP_mcode(op) && mrt.Dispatched_Ops( cur_clock ) )
        can_reserve = false;
    }

    // First before updating ready vector, if we can reserve, add to the ready 
    // vector for successors which have all preds scheduled, then
    // reserve the resource and mark our current op as scheduled.
    if( can_reserve ){
      // Update ready vector.
      for( ARC_LIST* arcs = OP_succs(op); 
           arcs != NULL; arcs = ARC_LIST_rest(arcs) ){
        ARC* arc = ARC_LIST_first(arcs);
        OP* succ_op = ARC_succ(arc);
        OPR* succ_opr = Get_OPR(succ_op);
        OPR_num_preds(succ_opr)--;
        if( OPR_num_preds(succ_opr) == 0 ){
	  VECTOR_Add_Element( _ready_vector, succ_op );
        }

        OPR_pred_order(succ_opr) = OPR_sched_order(opr);
        ASSERT(cur_clock + ARC_latency(arc) <= OPR_deadline(succ_opr));

        int time = OPR_issue_time(opr) + ARC_latency(arc);
        if( ARC_kind(arc) == CG_DEP_REGIN &&
	    time > OPR_release_time(succ_opr) ){
	  OPR_release_time(succ_opr) = time;
	  Tighten_Release_Time( succ_op );
        }
      }

      // Reserve the resource
      OPR_sched_group(opr) = sched_group;
      mrt.Reserve_Resources( op, cur_clock );

      // Now mark the op as scheduled
      VECTOR_Delete_Element( _ready_vector, op );
      Set_OPR_is_scheduled(opr);
      OPR_sched_order(opr) = sched_order++;
      VECTOR_Add_Element( _sched_vector, op );
    } else {
      cur_clock++;
      sched_group++;
      Close_Current_Window( false );
      sched_group_imm_size = 0;
      sched_group_64bit_imm_count = 0;

      Reset_OPR_is_scheduled(opr);
      last_op = op;

      // Tighten scheduling ranges based on the update release.
      for( int i = VECTOR_count( _ready_vector ) - 1; i >= 0; i-- ){
	OP* ready_op = (OP*)VECTOR_element( _ready_vector, i );
	OPR* ready_opr = Get_OPR(ready_op);

	if( OPR_release_time(ready_opr) < cur_clock ){
	  OPR_release_time(ready_opr) = cur_clock;
	  Tighten_Release_Time( ready_op );
	}
      }
    }

    // Given the update <cur_clock> or resources usage, update the issue time
    // for all the ready ops.
    for( int i = VECTOR_count( _ready_vector ) - 1; i >= 0; i-- ){
      OP* ready_op = (OP*)VECTOR_element( _ready_vector, i );
      mrt.Compute_Issue_Time( ready_op, cur_clock );
    }
  }
}


DSP_SCH::DSP_SCH( BB* bb, MEM_POOL* pool, bool trace )
  : bb( bb ), mem_pool( pool ), trace( trace )
{
  if( CG_skip_local_sched ){
    BBs_Processed++;
    if( BBs_Processed < CG_local_skip_before ||
	BBs_Processed > CG_local_skip_after  ||
	BBs_Processed == CG_local_skip_equal ){
      fprintf( TFile, "[%d] BB:%d processed in DSP_SCH\n", 
	       BBs_Processed, BB_id( bb ) );
      return;
    }
  }

  // only innermost loops
  if( (BB_innermost(bb) == false ) || ( BB_loop_head_bb(bb) == NULL ) )
    return;

  // only single block loops
  bool bb_has_same_backedge = false;
  for( BBLIST *lst = BB_succs(bb); lst != NULL; lst = BBLIST_next(lst) ) {
    BB *cur_bb = BBLIST_item(lst);
    if( cur_bb == bb )
      bb_has_same_backedge = true; 
  }
  if( !bb_has_same_backedge )
    return;

  if( BB_length(bb) > 2 ){
    CG_DEP_Compute_Graph( bb, 
			  INCLUDE_ASSIGNED_REG_DEPS,
			  NON_CYCLIC,
			  INCLUDE_MEMREAD_ARCS,
			  INCLUDE_MEMIN_ARCS,
			  INCLUDE_CONTROL_ARCS,
			  NULL );

    if( trace ){
      CG_DEP_Trace_Graph( bb );
    }

    Init();

    if( can_sched ) {
      Schedule_BB();

      if( window_data.window_pair[0].opr_count > 0 ){
        Close_Current_Window( true );
      }

      Reorder_BB();
      Summary_BB();
      Set_BB_dispatch(bb);
    }

    CG_DEP_Delete_Graph( bb );
  }

  Set_BB_scheduled( bb );
  if( Assembly && BB_length(bb) > 0 )
    Add_Scheduling_Note( bb, NULL );
}


void CG_Sched( MEM_POOL* pool, BOOL trace )
{
  int i;

  // Note: Only targets which have their machine descriptions modified
  //       to support some of the scheduling logic present should use this
  //       scheduler.
  if( Is_Target_Orochi() == false )
    return;

  if( CG_dispatch_schedule == false )
    return;

  // compute live-in sets for registers.
  sched_trace = trace;
  REG_LIVE_Analyze_Region();

  // The rid test is now incorperated so that we do not reorder
  // when we fail, but we still need to derive all other info.
  // This is done so that we exactly mimic code layout in emit.
  for( BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb) ){
    window_data.cur_offset = 0;
    DSP_SCH sched( bb, pool, trace );
  }

  // Finish with reg_live after cflow so that it can benefit from the info.
  REG_LIVE_Finish ();
}
