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
 

#include <stdlib.h>
#include <algorithm>
#include <vector>
#include <stack>
#include <list>
#include <set>

#include "bb.h"
#include "defs.h"
#include "cg_region.h"
#include "fb_tnv.h"
#include "op_map.h"
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
#include "cg.h"

#include "stride_prefetch.h"
#include "val_prof.h"

#include "cg_dep_graph.h"
#include "targ_cache_info.h"
#include "cache_analysis.h"


#define EMPT   (-1)
#define STRONG_SINGL_STRIDE  0x0001
#define PHASED_MULTI_STRIDE  0x0002

#define MIN_STRIDE 9
#define MAX_DISTANCE 0x1fff


extern OP_MAP OP_to_WN_map;

#ifndef  REGION_VECTOR_ITER
typedef REGION_VECTOR::iterator   REGION_VECTOR_ITER;
#endif

class REGION_STRIDE_PREFETCH {
private:
    MEM_POOL * _mempool;	// the local memory pool
    REGION *region;
    UINT64 pu_freq;
    UINT64 THRESH_FREQ;
    UINT32 PRE_DISTANCE;
    UINT32 LOAD_LATENCY;
    UINT64 CACHE_SIZE;
    float STRONG_SINGLE_STRIDE_THREHOLD;
    UINT32 PHASED_MULTI_STRIDE_THRESHOLD;
    UINT64 PMST_SECOD_FREQ;
    float PMST_ZERO_FREQ;
    UINT64 SINGLE_FIRST_FREQ;
    float  NTA_THRESHOLD;
    STRIDE_LOOP_HEADER *loop_header;
    INT32 flags;
    
   
public:
    REGION_STRIDE_PREFETCH(MEM_POOL *m  ,INT32 stride_flags):_mempool(m),region(NULL),pu_freq(0),THRESH_FREQ(10),
    	PRE_DISTANCE(16),LOAD_LATENCY(90),CACHE_SIZE(1000000),STRONG_SINGLE_STRIDE_THREHOLD(0.70),PHASED_MULTI_STRIDE_THRESHOLD(10000),
    	PMST_SECOD_FREQ(100),PMST_ZERO_FREQ(0.80),SINGLE_FIRST_FREQ(1000),
    	NTA_THRESHOLD(0.90),loop_header(NULL),flags(stride_flags)
    {
    }

    ~REGION_STRIDE_PREFETCH(){}


    void   Stride_A_Region(REGION * regn);

protected:
    void   Stride_Prefetch_Initial(void);
    UINT64   Find_Loop_Count(REGION *rgn );
    double Compute_Iteration_Cycles(UINT64 loop_count);
    double Compute_Region_Cycles(REGION * regn);
    float   Compute_Iteration_Data_Size(UINT64 loop_count);
    float   Compute_Region_Data_Size(REGION * regn);
    UINT32 BB_Ld_Count(BB * bb);
    void    Compute_Prefetch_Distance(INT32* distance);
    INT32  Min( double a, double b, double c);
    void    Insert_Prefetch_List(INT32* distance);
    void    Stride_Ins(BB *bb, OP *op, INT32 distance);
    OP *   Mk_Add_OP(mINT64 int_arg, TN *tn, TN * total_tn);
    OP *   Mk_Prefetch_OP(float zero_prob, TN * address);
    void    Strong_Single_Stride_Ins(BB *bb, OP *op, INT32 distance );
    void    Phased_Multi_Stride_Ins(BB *bb, OP *op, INT32 distance);
};

struct pref_dis_map{
  OP *op;
  mINT64 dis;
};
static std::vector<struct pref_dis_map> prefetch_list;


// do initial work
// the load instructions with stride pattern in a hot loop are major object of stride 
// prefetch, and loop back edge is a boundary of a region, so we try to find the loop 
// which encloses this region. We can get loop trip count by this loop_header, if edge
// profiling information is exactly, the count is exactly, we can use filter to ignore small
// count loop. If edge profiling is not invoked and this region is a part of a loop, we can
// use reference counter of each load to determine whether prefetch instruction should 
// be inserted, but this method is not good enough. If edge profiling shows that this 
// region is not hot.

void Stride_Region(REGION_TREE *region_tree, INT32 stride_flags)
{
    MEM_POOL local_mempool;
    MEM_POOL_Initialize( &local_mempool, "stride prefetch", FALSE );
    MEM_POOL_Push( &local_mempool );
    REGION_VECTOR region_set;
    REGION_VECTOR_ITER iter; 
    REGION *region;
    region_set = region_tree->Region_Set();
    REGION_STRIDE_PREFETCH region_stride( &local_mempool, stride_flags );
    for (iter = region_set.begin(); iter != region_set.end(); iter++) {
        region =*iter;
        region_stride.Stride_A_Region(region);
    }
    MEM_POOL_Pop( &local_mempool );
    MEM_POOL_Delete( &local_mempool );
}


void REGION_STRIDE_PREFETCH::Stride_A_Region(REGION * regn)
{
    region = regn ;
    BOOL  worth_prefetch;
    INT32 distance;
    //edge_done =FALSE;
    loop_header = CXX_NEW(STRIDE_LOOP_HEADER, _mempool);
    Stride_Prefetch_Initial();
    if ( loop_header != NULL ){
        // compute the prefetch load distance according to loop instruction
        Compute_Prefetch_Distance(&distance);
        // travel the region and Insert prefetch instructions
        Insert_Prefetch_List( &distance);
    }
    
}


// try to find loop type region enclosing this region
// in struct bb, field loop_head_bb seems to finger out the loop head bb, but is
// it efficient? If so, we can use it to find loop head bb directly.
void REGION_STRIDE_PREFETCH::Stride_Prefetch_Initial(void)
{

    BOOL found_loop_region = FALSE;
    UINT64 loop_count ;
    while ( !found_loop_region){
        if (region->Region_Type() == LOOP)
            found_loop_region = TRUE;
        else if (region->Next_Sibling() == NULL)
            break;
            else region = region->Next_Sibling();
    }

// PU's frequency can be got from loop_region's entry BB. THRESH_FREQ is the 
// threshold
    BB *bb = REGION_First_BB;
    pu_freq= (UINT64)bb->freq;
    if (!found_loop_region){
        if ( edge_done ){
            loop_header = NULL;
        }else{
            loop_header->trip_count = (UINT64)EMPT;
        }
        
        return ;
    }

// try to get loop trip count, if loop_count is -1, that means we can not got exactly 
// loop trip count
    if ( edge_done )
    	loop_count = Find_Loop_Count( region );
    else loop_count = (UINT64)EMPT;

    if ((loop_count != EMPT) && ( loop_count < THRESH_FREQ)){
        loop_header = NULL;
        return ;
    }

    loop_header->trip_count = loop_count;

// try to get average cycle number of loop's iteration, but it is not exactly. The
// average cycle count of each iteration and average access data set size of each 
// iteration, these information are used to compute prefetch distance

    double iteration_average_cycle;
    double iteration_average_data_size;
    if (loop_count !=EMPT){
        iteration_average_cycle = Compute_Iteration_Cycles( loop_count );
        loop_header->average_cycle_iter = (UINT64)iteration_average_cycle;
        iteration_average_data_size = Compute_Iteration_Data_Size( loop_count );
        loop_header->average_data_size_iter = (UINT64)iteration_average_data_size;
    }
    
    return ;
}


UINT64 REGION_STRIDE_PREFETCH::Find_Loop_Count(REGION *rgn ){
    UINT64 freq=0;
    NODE_VECTOR nodes;
    nodes = rgn->Entries();
    for(NODE_VECTOR_ITER iter = nodes.begin(); iter!=nodes.end(); iter++){
        if((*iter)->Is_Region())
            freq +=Find_Loop_Count((*iter)->Region_Node());
        else
            freq +=( UINT64)BB_freq((*iter)->BB_Node());
    }
    return freq;
}
// when this function is called, edge profiling is invoked, but cycle count of each bb is not 
// compute yet, so what we can do is try to estimate it

double REGION_STRIDE_PREFETCH::Compute_Iteration_Cycles(UINT64 loop_count)
{
    double total_cycles;
    total_cycles = Compute_Region_Cycles( region );
    return total_cycles / loop_count;

}


double REGION_STRIDE_PREFETCH::Compute_Region_Cycles(REGION * regn)
{
    double cycles_of_cur_region = 0;

    // compute the total cycle of the region recursively

    REGIONAL_CFG_NODE * node;
    REGIONAL_CFG *rgn_cfg;
    BB *bb;
    NODE_VECTOR _node_set;
    NODE_VECTOR_ITER iter;
    rgn_cfg = regn->Regional_Cfg();
    _node_set = rgn_cfg->Node_Set();
    for (iter = _node_set.begin(); iter != _node_set.end(); iter++) {
        node = *iter;
        if (node->Is_Region()){
    	    REGION * nested_region = node->Region_Node();
    	    cycles_of_cur_region += Compute_Region_Cycles(nested_region);
    	}
    	else{
    	    bb = node->BB_Node();
    	    Calculate_BB_Cycle( bb, FALSE);
    	if (BB_call(bb))
    		// in current phase, BB's cycle can not compute exactly, so we just to 
            // estimate by letting BB's op number divided with 2 ( we choice 2 because
            // of current orcc's used slot of each cycle is about 2)

            cycles_of_cur_region += BB_cycle(bb) * BB_freq(bb); 
    	else
    		// there is problems to get other PU's information at here, so we can not 
            // compute function cycles. If we can feed back each function's cycle count 
            // by using profiling, we can compute it more exactly

            cycles_of_cur_region += BB_cycle(bb) * BB_freq(bb); 
    
       }
    }
    
    return cycles_of_cur_region;
}

// when this function is called, edge profiling is invoked, but at here, we can not get exactly
// footprint size because of some load may access the same address and other PU's 
// information is not known at here, so what we can do is try to estimate it.
// The following two functions can be omitted, if so, we can use loop_header->trip_count to 
// compute prefetch distance

float REGION_STRIDE_PREFETCH::Compute_Iteration_Data_Size( UINT64 loop_count)
{
    double total_data_size;
    total_data_size = Compute_Region_Cycles(region);
    return total_data_size / loop_count;
}

float REGION_STRIDE_PREFETCH::Compute_Region_Data_Size(REGION * regn)
{
    double data_size_of_cur_region = 0;

    //compute the total data size of the region
    REGIONAL_CFG_NODE * node;
    REGIONAL_CFG *rgn_cfg;
    BB *bb;
    NODE_VECTOR _node_set;
    NODE_VECTOR_ITER iter;
    rgn_cfg = regn->Regional_Cfg();
    _node_set = rgn_cfg->Node_Set();
    node = region->Regional_Cfg_Node(); 
        for (iter = _node_set.begin(); iter != _node_set.end(); iter++) {
            node = *iter;
            if (node->Is_Region()) { // this node is a nested region
    		    REGION * nested_region = node->Region_Node();
    		    data_size_of_cur_region += Compute_Region_Data_Size(nested_region);
            }else{
                bb = node->BB_Node();
                if( BB_call(bb))
    		    // at here, BB's access data size is estimated by ld instruciont number
                    data_size_of_cur_region += BB_Ld_Count(bb) * BB_freq(bb); 
                else 
    		        data_size_of_cur_region += BB_Ld_Count(bb) * BB_freq(bb); 
            }
        }
        
    return data_size_of_cur_region;

}

UINT32 REGION_STRIDE_PREFETCH::BB_Ld_Count(BB * bb)
{
    OP *op, *tmp_op;
    UINT32 i = 0;
    for (op = BB_first_op(bb); op != NULL; op = tmp_op) 
    {
   	    tmp_op = OP_next(op);
        switch (OP_code(op)) {
        case TOP_ld1:
        case TOP_ld4:
        case TOP_ld2:
        case TOP_ld8:
            i++;
            break;
        default:
            //nothing need to do
            break;
        }
    
    }
    return  i;
}

void REGION_STRIDE_PREFETCH::Compute_Prefetch_Distance( INT32 *distance)
{
    double distance_of_cycle;
    double distance_of_data_size;
    if (loop_header->trip_count == EMPT)
        *distance = PRE_DISTANCE;
    else{
        if( loop_header->average_cycle_iter !=0)
           distance_of_cycle = LOAD_LATENCY / loop_header->average_cycle_iter;
        else
           distance_of_cycle = 100;

    	// if average_data_size_of_iteration is not computed, the following statement can 
        // be replaced with loop_header->trip_count/predefine_trip_threshold

        if( loop_header->average_data_size_iter!=0)
    	    distance_of_data_size = 100;//CACHE_SIZE / loop_header->average_data_size_iter;
        else 
        	distance_of_data_size =100;
        *distance = Min(distance_of_cycle, distance_of_data_size, PRE_DISTANCE);   
    }
}

INT32 REGION_STRIDE_PREFETCH::Min( double a, double b, double c)
{
    double i;
    if ((a < b)&& (a< c))
        i = a;
    else if ( b < c)
        i = b;
    else 
        i =c ;
    return (INT32) i;
} 

void REGION_STRIDE_PREFETCH::Insert_Prefetch_List(INT32* distance)
{
    UINT64 trip_count = loop_header->trip_count;

    for (SEQ_REGIONAL_CFG_ITER node_iter(region->Regional_Cfg()); node_iter!=0; ++node_iter){
        REGIONAL_CFG_NODE *node = *node_iter;
        if ( !node->Is_Region() ){
    		BB *bb = node->BB_Node();
    		OP *op, *tmp_op;
    		for (op = BB_first_op(bb); op != NULL; op = tmp_op) 
            {
   		        tmp_op = OP_next(op);
                switch (OP_code(op)) {
                case TOP_ld1:
                case TOP_ld4:
                case TOP_ld2:
                case TOP_ld8:
                    Stride_Ins(bb, op, *distance);
                    break;
                default:
                    //nothing need to do
                    break;
             }
          }
       }

    }

}

void REGION_STRIDE_PREFETCH::Stride_Ins(BB *bb, OP *op, INT32 distance)
{
  
    if (op_stride_tnv_map == NULL){
        return;
    }else if(OP_MAP_Is_Delete(op_stride_tnv_map))
        return;
    if (OP_Prefetched(op))
        return;
    FB_TNV *tnv = (FB_TNV *)OP_MAP_Get(op_stride_tnv_map, op);
    UINT64 first_val_freq;
    UINT64 secod_val_freq;
    UINT64 total_access;
    UINT64 total_freq = 0;
    UINT64 zeros_freq =0;

    if ( tnv == NULL){
	DevWarn("can not find tnv-info in op_stride_tnv_map! val_prof_id = %d",OP_val_prof_id(op));
        return;
    }
    first_val_freq = tnv->_counters[0]; 
    secod_val_freq = tnv->_counters[1];
    total_access = tnv->_exec_counter;
    
    int i;
    for( i = 0; i < 10; i++){
        total_freq += tnv->_counters[i];
    }
    
    zeros_freq = tnv->_zero_std_counter;

    if ( total_access == 0){
    	DevWarn("total_access can not equ 0");
        return;
    }
    
    float zeros_prob =((float)zeros_freq) / total_access;
    float stride_prob;



    stride_prob = ((float)first_val_freq) / total_access;
    if ( stride_prob > STRONG_SINGLE_STRIDE_THREHOLD && (first_val_freq > SINGLE_FIRST_FREQ))
        Strong_Single_Stride_Ins(bb, op, distance);
    else if (total_access > PHASED_MULTI_STRIDE_THRESHOLD
        && zeros_prob > PMST_ZERO_FREQ)
        Phased_Multi_Stride_Ins(bb, op, distance);
}


void REGION_STRIDE_PREFETCH::Strong_Single_Stride_Ins(BB *bb, OP *op, INT32 distance )
{   
    if (!( flags & STRONG_SINGL_STRIDE))
    	return;
    FB_TNV *tnv =(FB_TNV *)OP_MAP_Get(op_stride_tnv_map,op);
    mINT64 first_val;
    mINT64 prefetch_distance;
    float  zero_prob;
 
    first_val = tnv->_values[0];
    if( abs(first_val) < MIN_STRIDE) return;
    prefetch_distance = distance * first_val;
    if ( (prefetch_distance == 0) || ( prefetch_distance > MAX_DISTANCE ))
        return;

    // consider same cache line to adjust prefetch_distance;
    OP *cur_op;
    INT max_dis=0, min_dis=0;
    if (prefetch_list.size()>0) {
        max_dis = min_dis = prefetch_list[prefetch_list.size()-1].dis;
    }
    for(INT i= prefetch_list.size()-1; i>=0; i--){
        cur_op = prefetch_list[i].op;
        if (cur_op == op) break;
        INT diff;
        if (OP_Prefetched(cur_op)) { 
            INT cache_line_size = Cache_Line_Size(CACHE_L2);
            if (Cache_Access_Same_Line(cur_op, op, &diff) && diff <= cache_line_size/2) {
                max_dis = prefetch_list[i].dis > max_dis ? prefetch_list[i].dis : max_dis;
                min_dis = prefetch_list[i].dis < min_dis ? prefetch_list[i].dis : min_dis;
                if (abs(prefetch_distance - prefetch_list[i].dis)<cache_line_size) {
                  INT value = abs(cache_line_size/first_val)+1;
                  if (prefetch_distance < prefetch_list[i].dis || distance>8) { 
                    prefetch_distance -= value*first_val;
                  } else {
                    prefetch_distance += value*first_val;
                  }
                  INT boundary = first_val>0?max_dis:min_dis;
                  while(abs(prefetch_distance - boundary) < cache_line_size) {
                     prefetch_distance += value*first_val;
                  }
                  break;
               }
           }
        }
    }
    // push op and distance to prefetch list
    struct pref_dis_map pdm;
    pdm.op  = op;
    pdm.dis = prefetch_distance;
    prefetch_list.push_back(pdm);
    if (prefetch_list.size()>5) { prefetch_list.erase(prefetch_list.begin());}

    zero_prob = ((float)tnv->_zero_std_counter) / tnv->_exec_counter;
    TN *base = Gen_Register_TN( ISA_REGISTER_CLASS_integer, 8 );
    TN *distance_tn = Gen_Register_TN( ISA_REGISTER_CLASS_integer, 8 );
    OP *op_add = Mk_OP( TOP_adds, base, True_TN, Gen_Literal_TN(prefetch_distance, 8), OP_opnd(op, 3));
    OP *op_prefetch =Mk_Prefetch_OP( zero_prob, base);
    WN * wn = WN_Create(OPC_PREFETCH, 1);
    WN_offset(wn) = prefetch_distance;
    WN_pf_set_manual(wn) ;
    OP_MAP_Set(OP_to_WN_map, op_prefetch, wn);
    OP_srcpos(op_prefetch) = OP_srcpos(op);
    OP_srcpos(op_add) = OP_srcpos(op);
    BB_Insert_Op_Before(bb, op, op_prefetch);
    BB_Insert_Op_Before(bb, op_prefetch, op_add);
    Set_OP_Prefetched(op); 
 }



OP * REGION_STRIDE_PREFETCH::Mk_Add_OP(mINT64 int_arg, TN *tn, TN * total_tn)
{
    TN *arg_tn = Gen_Literal_TN(int_arg, 8);
    OP *op = Mk_OP( TOP_add, total_tn, True_TN, arg_tn, tn );
    return op;
}

OP * REGION_STRIDE_PREFETCH::Mk_Prefetch_OP(float zero_prob, TN * address)
{
    TN *lfhint;
    //if (zero_prob > NTA_THRESHOLD)
        lfhint = Gen_Enum_TN(ECV_lfhint);
    //else lfhint = Gen_Enum_TN(ECV_lfhint_nta);
    OP *op =Mk_OP(TOP_lfetch, True_TN, lfhint, address);
    return op;
}


void REGION_STRIDE_PREFETCH::Phased_Multi_Stride_Ins(BB *bb, OP *op, INT32 distance)
{   
     if (!( flags & PHASED_MULTI_STRIDE))
    	return;
    FB_TNV *tnv =(FB_TNV *)OP_MAP_Get(op_stride_tnv_map,op);
    mINT64 first_val;
    mINT64 prefetch_distance;
    float zero_prob;
    TN *address_tn = Gen_Register_TN( ISA_REGISTER_CLASS_integer, 8 );
    TN *base = Gen_Register_TN( ISA_REGISTER_CLASS_integer, 8 );
    TN *stride = Gen_Register_TN( ISA_REGISTER_CLASS_integer, 8 );
    prefetch_distance = 1;
    zero_prob = ((float) tnv->_zero_std_counter) / tnv->_exec_counter;
    OP *op_sub = Mk_OP( TOP_sub, stride, True_TN,  OP_opnd(op,3), address_tn );
    OP *op_add =Mk_OP( TOP_shladd, base, True_TN, stride, Gen_Literal_TN(prefetch_distance, 8),OP_opnd(op,3));
    OP *op_mov = Mk_OP(TOP_mov, address_tn, True_TN,  OP_opnd(op,3));
    OP *op_prefetch =Mk_Prefetch_OP( zero_prob, base);
    WN * wn = WN_Create(OPC_PREFETCH,1);
    WN_offset(wn) = prefetch_distance;
    WN_pf_set_manual(wn) ;
    OP_MAP_Set(OP_to_WN_map, op_prefetch, wn);
    OP_srcpos(op_prefetch) = OP_srcpos(op);
    OP_srcpos(op_add) = OP_srcpos(op);
    OP_srcpos(op_mov) = OP_srcpos(op);
    OP_srcpos(op_sub) = OP_srcpos(op);
    BB_Insert_Op_Before(bb, op, op_prefetch);
    BB_Insert_Op_Before(bb, op_prefetch, op_mov);
    BB_Insert_Op_Before(bb, op_mov, op_add);
    BB_Insert_Op_Before(bb, op_add, op_sub);
    Set_OP_Prefetched(op); 
}


