/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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

/*----------------------------------------------------------------------*/
/*In current implementation of field-reorder                            */
/*NOT introduce new types, i.e. a child struct has the same ordering    */
/*    in all occurances, as a type or as an enclosed field type         */
/*An example :------------where  structs have enclosing relationship    */
/*          sub_struct{int s1,s2; real s3}                              */
/*          top_struct { int t1,t2; sub_struct t3}                      */
/*currently, we will only get consistent ordering for enclosed struct   */
/*we cannot get  new ordering as follows                                */
/*              {s2,  s3,  s1} for sub_struct                           */
/*              {t1,  t3. s1,  t3. s3,  t3.s2,  t2 } for sub_struct     */
/*                    ^^^^^^^^^^^^^^^^^^                                */
/*Perhaps we'll lose some accuracy here.                                */
/*----------------------------------------------------------------------*/

#include <stdint.h>
#include <vector>
#include <stack>
#include <algorithm>    // for using of random_shuffle
#include <list>
#include <map>
#include <math.h>  //for div(x,y)

#include "defs.h"
#include "tracing.h"            // for DBar,TP_IPA
#include "ipa_trace.h"          //for IPA_TRACE_TUNING_NEW
#include <search.h>     //for qsort
#include <memory.h>     // for memset() 
#include "wn_util.h"    // for WN_WALK_TreeNext(), etc.
#include "cxx_template.h"   // for STACK
#include "wn_core.h"    //for WN_field_id(wn)
#include "mempool.h"    // for Malloc_Mem_Pool 's definition
#include "ipa_cg.h"     //for IPA_NODE, IPA_NODE_ITER
#include "ipa_reorder.h"
#include "ir_reader.h"  //fdump_tree() for IPO_Modify_WN_for_field_reorder()
#ifdef KEY
#include "ipo_parent.h"	// for WN_Get_Parent
#endif // KEY

typedef hash_map<mUINT32,CAND_ITEM*> TY_TO_CAND_MAP;
struct PTR_AND_TY
{
    mUINT32 ty_index;
    mUINT32 pt_index;
};
typedef vector<struct PTR_AND_TY> PTR_AND_TY_VECTOR;
extern TY_TO_CAND_MAP Ty_to_cand_map; //used by WN's modification

REORDER_CAND reorder_candidate; //identical in size with can_be_reorder_types
TYPE_LIST can_be_reordered_types;
TYPE_LIST invalid_reorder_types; //what cannot be reordered
//sometimes, above list maybe large, eg 200, Perhaps we need a hash-map
TY_TO_CAND_MAP Ty_to_cand_map;
PTR_AND_TY_VECTOR *Ptr_and_ty_vector;
//for each struct_ty, there maybe more than one pointer-types
//Some having names, one of them  is anonymous.

MERGED_ACCESS_VECTOR *merged_access;
MEM_POOL  reorder_local_pool;
typedef hash_map<mUINT32,MERGED_ACCESS*> TY_TO_ACCESS_MAP;
TY_TO_ACCESS_MAP *ty_to_access_map; //for Record_struct_access in template.h
BOOL* visited;
inline TY_IDX
TY_POINT_TO_NON_UNIONSTRUCT(TY &ty)
 {
      TY_IDX ret;
      ret=0;
      if(ty.kind==KIND_POINTER){
          TY_IDX point_to_ty=TY_pointed(ty);
          TY* cur_ty=&Ty_Table[point_to_ty];
         if(cur_ty->kind==KIND_STRUCT&& !((cur_ty->flags)&TY_IS_UNION))
            return point_to_ty;
     }
     return ret; //0 means not point to non_union_struct type
}    

/*-----------------------------------------------*/
/*init_merge_access()                            */
/*called in IPA when processing each source file */
/*-----------------------------------------------*/
void Init_merge_access()
{
  MEM_POOL_Initialize(&reorder_local_pool,"reorder_pool",TRUE);
  MEM_POOL_Push (&reorder_local_pool);
  merged_access=CXX_NEW(vector< MERGED_ACCESS*>,&reorder_local_pool);
  ty_to_access_map=CXX_NEW(TY_TO_ACCESS_MAP(),&reorder_local_pool);
  Ptr_and_ty_vector=CXX_NEW(PTR_AND_TY_VECTOR(),&reorder_local_pool);
  reorder_candidate.size=0; /*initialize it*/
  reorder_candidate.list=NULL;

}
void print_merged_access()
{
    MERGED_ACCESS_VECTOR::iterator iter;
    MERGED_ACCESS *cur_access;
    INDEX index,i;
    INDEX size=merged_access->size();
    fprintf(TFile,"STRUCT ACCESS: after merge:%d\n",size);
    for(iter=merged_access->begin();iter!=merged_access->end();iter++){
        cur_access=(*iter);
        index=cur_access->ty_index;
        fprintf(TFile,"\n type(%s)\n",Index_To_Str(Ty_tab[index].name_idx));
        for(i=0;i<cur_access->flatten_fields;i++){
            fprintf(TFile,"[%d]=%lld",i,cur_access->count[i]);
            if ((i+1)% 8==0)
                fprintf(TFile,"\n");

        }
    }
    fprintf(TFile,"\n");
    FmtAssert(size<=ty_to_access_map->size(),
        ("merged_access size <=ty_to_access_map!\n"));
    return;
}
/*----------------------------------------------------------------------*/
/*function name : Merge_struct_access()                                 */
/*called by IPA_update_summary_st_idx()  when processing each source file*/
/*-----------------------------------------------------------------------*/
void Merge_struct_access(SUMMARY_STRUCT_ACCESS *cur_summary, mUINT32 index)
{
    FIELD_ID fld_id,flatten_flds;
    COUNT count;
    MERGED_ACCESS *cur_access;
    TY_TO_ACCESS_MAP::iterator iter;
    BOOL Trace_it=Get_Trace ( TP_IPA,IPA_TRACE_TUNING_NEW );
    if ( Trace_it) 
       fprintf(TFile,"merging...!\n");
    flatten_flds=cur_summary->Get_flatten_flds();
    iter=ty_to_access_map->find(index);
    if(iter!=ty_to_access_map->end()) //found it!
        cur_access=iter->second;
    else{ //not found
        cur_access = (MERGED_ACCESS*)MEM_POOL_Alloc_P(&reorder_local_pool,
        sizeof(MERGED_ACCESS),TRUE,NULL);

        cur_access->ty_index=index;
        cur_access->flatten_fields=flatten_flds;
        //initialize the new one
        cur_access->count = (mUINT64*)MEM_POOL_Alloc_P(&reorder_local_pool,
            sizeof(mUINT64)*flatten_flds,TRUE,NULL);

        merged_access->push_back(cur_access);
        ty_to_access_map->insert(make_pair(index,cur_access));

    }
       FmtAssert(index==cur_access->ty_index,
        ("index is consistent, find is wrong!\n"));
    for(INT i=0;i<8;i++)
    {
        fld_id=cur_summary->Get_hot_fld_id(i);
        if(!fld_id) break;
        count=cur_summary->Get_hot_fld(i);
        cur_access->count[fld_id-1]+=count;
    }
    if (Trace_it) 
       print_merged_access();
    return;
}

/*----------------------------------------------------------*/
/*function :    find_merged_access()                        */
/*called once to fill in access-info into candidates        */
/*TODO: seem no need to use hash-search                     */
/*----------------------------------------------------------*/
MERGED_ACCESS* find_merged_access(INDEX ty_index)
    /*just check for struct_ty, not for pointer_list*/
{
    MERGED_ACCESS_VECTOR::iterator iter;
    MERGED_ACCESS* cur_access;
    MERGED_ACCESS* found_access=NULL;
    for(iter=merged_access->begin();
        iter!=merged_access->end();
        iter++)
    {
        cur_access=*iter;
        if(ty_index==cur_access->ty_index){
            found_access=cur_access;
            break;
        }
    }
    return found_access;
}

struct TOP_FIELD_INFO{
    FIELD_ID field_id;
    TY_IDX ty_idx; //0 means scalar type,else STRUCT type
    TY_SIZE size; //size of this field
#ifdef KEY
    mUINT32 align; // alignment of this field
#endif // KEY
}; //just for top field , used for creating new ordering 

typedef list<TOP_FIELD_INFO> TOP_FLD_LIST; //top field to field_id
struct TOP_FLD_DESC{
    TY_SIZE size;
    TOP_FLD_LIST list;
};
/*-----------------------------------------------------------------*/
/*Currently, reorder_candidate is now  search in linear-time  simply*/
/*TODO: will it need to be modified to hash approach?               */
/*reorder_candidate is usually a short list                         */
/*------------------------------------------------------------------*/
CAND_ITEM* find_in_reorder_candidate(INDEX ty_index)
{
    INDEX i;
    CAND_ITEM *found=NULL;
    CAND_ITEM *cur_cand;
    cur_cand=reorder_candidate.list;
    for(i=0;i<reorder_candidate.size;i++,cur_cand++)
    {
        if(cur_cand->ty_index==ty_index){
            found=cur_cand;
            break;
        }
    }
    return found;
}
/*----------------------------------------------------------------------*/
/*Handle_access-count ()                                                */
/*About access count:                                                   */
/*     1)access count of a non-top-level-field is accumulated to        */
/*       corresponding child (offspring) structure                       */
/*     2)Sum all access count of low-level-field, store it to a  top-level-field*/
/*     3) all access count of low-level-field is with the same count            */
/*       as its top-level-struct                                                */
/*------------------------------------------------------------------------------*/

mUINT64  Handle_access_count(INDEX ty_index, TOP_FLD_DESC&    idlist,
                         FIELD_ID &field_id, CAND_ITEM* p_cand)
{

    FLD_IDX start_fld_idx;
    TOP_FIELD_INFO top_field;
    BOOL is_struct;
    BOOL in_enclosed_struct=FALSE;
    CAND_ITEM* sub_cand=NULL;
    mUINT64 sum,sub_sum,this_count,local_sum;
    INDEX sub_fld_id,i;
    FIELD_ID pre_field_id,j;
    sum=0; //sum of all access_count for an enclosed struct
    if(p_cand->ty_index!=ty_index){
        in_enclosed_struct=TRUE;
        p_cand->flag.has_enclosed_struct=TRUE;
        sub_cand=find_in_reorder_candidate(ty_index);
    }
    //i+1 is ty_index's top-field-cnt
    for(i=0,start_fld_idx=Ty_tab[ty_index].u1.fld;;
            i++,start_fld_idx++){
        field_id++;
        TY_IDX  cur_idx= Fld_Table[start_fld_idx].type;
        TY*cur_ty=& Ty_Table[cur_idx];
        char *ty_name=Index_To_Str(cur_ty->name_idx); //just for debug!!!
        if(cur_ty->kind==KIND_STRUCT){ //a non-union struct, for this is legal type
            is_struct=TRUE;
        }else
            is_struct=FALSE;

        if(!in_enclosed_struct){ //outer level of struct type
            idlist.size++;
            top_field.field_id=field_id;
            top_field.size=cur_ty->size; //get its size, for later compute new offset
#ifdef KEY
            if( is_struct)
                top_field.ty_idx=cur_idx;
            else
                top_field.ty_idx=0;
	    top_field.align = TY_align (cur_idx);
#else
            if( is_struct)
                top_field.ty_idx=cur_idx;
            else
                top_field.ty_idx=0;
#endif // KEY
            idlist.list.push_back(top_field);
        }else{ //sum all access_count, disperse them 
            this_count=p_cand->fld_info.map[field_id-1].count;
            sum+=this_count;
            //add to sub_cand's corresponding fld
#ifdef KEY
// just checking for sub_cand is not enough. sub_cand is allocated before
// this function is called, but NOT sub_cand->top_field_info.list.
	    if (sub_cand && sub_cand->top_field_info.list) {
#else
            if(sub_cand){
#endif // KEY
                sub_fld_id=sub_cand->top_field_info.list[i].field_id;
                sub_cand->fld_info.map[sub_fld_id-1].count+=this_count; //add to sub_struct
            }
        }
        if( is_struct){
            pre_field_id=field_id;
            sub_sum=Handle_access_count(cur_idx>>8,idlist,field_id,p_cand);
            local_sum=p_cand->fld_info.map[pre_field_id-1].count+sub_sum;
            for(j=pre_field_id;j<=field_id;j++)
                p_cand->fld_info.map[j-1].count=local_sum;
            if(in_enclosed_struct)
               sum+=sub_sum;
        }
        if(Fld_Table[start_fld_idx].flags & FLD_LAST_FIELD)
            break;
    }//of for
    return sum;
}


 void check_reorder_legality_of_type(INDEX ty_index);
/*--------------------------------------------------------*/
/*function: find_in_invalid_types()                       */
/*Currently, invalid struct list is search in linear time */
/*just check for struct_ty, not for pointer_list          */
/*--------------------------------------------------------*/
BOOL find_in_invalid_types(INDEX ty_index)

{
    TYPE_LIST::iterator iter;
    BOOL found=FALSE;
    for(iter=invalid_reorder_types.begin();iter!=invalid_reorder_types.end();iter++)
    {
        if(ty_index==*iter){
            found=TRUE;
            break;
        }
    }
    return found;
}
/*-------------------------------------------------------------------------------*/
/*function_name invalidate_it()                                                  */
/*for an illegal type, invalidate this struct_ty and all of its embedding structs*/
/*-------------------------------------------------------------------------------*/
void invalidate_it(INDEX ty_index)
{
    visited[ty_index]=TRUE; //avoid TY-tab traversing to check twice!
    if(!find_in_invalid_types(ty_index)){
        invalid_reorder_types.push_back(ty_index);
        can_be_reordered_types.remove(ty_index);
    }
    TY *ty=&Ty_tab[ty_index];
    for(UINT cur_fld_idx=ty->u1.fld;;cur_fld_idx++){
        FLD & cur_fld=Fld_Table[cur_fld_idx];
        TY* cur_fld_ty=&Ty_Table[cur_fld.type];
        UINT flags=cur_fld.flags;
        if(cur_fld_ty->kind==KIND_STRUCT){
             invalidate_it(cur_fld.type>>8);
        } //else if STRUCT
        if(cur_fld.flags & FLD_LAST_FIELD) //last field
            break;
    } //end for all fields
    return;
}
/*-------------------------------------------------------------*/
/*There're two step to check type legality for field_reordering*/
/*First,  get_can_be_reordered_types()                         */
/*Second,  check_gsymbol_for_invalid_type()                    */
/*What is legal?                                               */
/*  non-union, having no-union-field, having no bit-field,     */
/*  all parent struct must be legal                            */
/*Algo:                                                        */
/*  Traverse Ty_tab[] to find  legal struct_types              */
/*      create the Ptr_and_ty_vector for all pointer types     */
/*the order of can_be_reorder_types:                           */
/*  Child struct must appear ahead of parent struct            */
/*-------------------------------------------------------------*/
void get_can_be_reordered_types()
{
    TY_TAB::iterator  iter;
    BOOL propagate;
    INDEX point_to;
    BOOL Trace_it=Get_Trace ( TP_IPA,IPA_TRACE_TUNING_NEW );
    MEM_POOL_Push (&reorder_local_pool);
    mUINT64 size=Ty_tab.size();
    visited = (BOOL*)MEM_POOL_Alloc_P(&reorder_local_pool,sizeof(BOOL)*size,TRUE,NULL);
    memset(visited,0,sizeof(BOOL)*size);
    if (Trace_it) 
       fprintf(TFile,"in get_can_be_reordered_types\n"); //debug ,later delete
    for(iter=Ty_tab.begin();iter!=Ty_tab.end();iter++)
    {
        if((*iter).kind==KIND_STRUCT){
            propagate=FALSE;
            check_reorder_legality_of_type(iter.Index()); //iter.Ptr() is TY*
        } //just handle struct type
        else if(point_to=TY_POINT_TO_NON_UNIONSTRUCT(*iter)>>8){
            //put an item to point_list
            INDEX ty_index=iter.Index();
            PTR_AND_TY item;
            item.ty_index=ty_index;
            item.pt_index=point_to;
            Ptr_and_ty_vector->push_back(item);
        }
    } //walk through all types
    if (Trace_it) 
       fprintf(TFile, "invalid-reorder_types' size=%d! \n", (INT)invalid_reorder_types.size());
    MEM_POOL_Pop (&reorder_local_pool);
    return;
}

/*-------------------------------------------------------------------------------*/
/*check_reorder_legality_of_type()                                               */
/*  check which type is legal in get_can_be_reordered_types()                    */
/*What is legal for reordering?                                                  */
/*  1) is a STRUCT type                                                          */
/*  2) is not a UNION;--no need to reorder                                       */
/*  3) has no BIT_FIELD --operator LDBITS behavoir cannot be controlled easily   */
/*  4) has UNION field-- not necessarily, just for simplicity                    */
/*further:                                                                       */
/*  5) all parent struct must be legal,                                          */
/*      i.e. child struct is legal => parent struct must be legal                */
/*NOTE:                                                                          */
/*   condition 5 is not necessary, just for simple implementation                */
/*Generally: four cases:    P(parent struct), C(child struct)                    */
/*   (I)    P is legal, C is illegal=>only type P will be reordereed in WN       */
/*   (II)   P is illegal, C is legal=> Both type should be reordered in WN!--    */
/*                          complicated! We''ll later support it!                */
/*   (III)  Both are legal  => Both should be reordered in WN.                   */
/*   (IV)   Neither is legal                                                     */
/*-------------------------------------------------------------------------------*/
// In addition, we do not include zero-sized structs. See comments below.
//
void check_reorder_legality_of_type(INDEX ty_index)// 
{
    TYPE_LIST::iterator iter;
    BOOL found, legal;
    BOOL invalidate=FALSE;
    TYPE_LIST low_level_tys; //index
    TY* ty=&Ty_tab[ty_index];
    if (visited[ty_index])
        return;
    FmtAssert(ty->kind==KIND_STRUCT,("the checked type must be STRUCT"));
#ifdef KEY
    if (!ty->size || !ty->u1.fld)
    {
	// don't set invalidate and let it be processed below.
	// if struct size == 0, it does not make sense reordering "fields",
	// it must not have any fields other than 0-width bit-fields. The
	// for loop below also does not check if ty->u1.fld == 0, which was
	// a bug.
    	visited[ty_index] = TRUE;
	return;
    }
    if ((ty->size <= 56) || (ty->size > 60))
    {// no reordering for greater sizes
    	visited[ty_index] = TRUE;
	return;
    }
#endif // KEY
    if (ty->flags &TY_IS_UNION)
        invalidate=TRUE;
    //check in all top-level-field, find if it is legal to reorder, if not, propagate it!
    for(UINT cur_fld_idx=ty->u1.fld;;cur_fld_idx++){
        FLD & cur_fld=Fld_Table[cur_fld_idx];
        TY* cur_fld_ty=&Ty_Table[cur_fld.type];
        mUINT16 flags=cur_fld.flags;
        if((flags & FLD_IS_BIT_FIELD)||(flags&FLD_BEGIN_UNION)){ // consider bit_size offset
               //consider bit fields, in fact, perhaps just need keep oringal relative ordering
               invalidate=TRUE;
               break;
            }
        if(cur_fld_ty->kind==KIND_STRUCT){
            //not to put twice!
            INDEX sub_index=cur_fld.type>>8;
            low_level_tys.remove(sub_index);
            low_level_tys.push_back(sub_index);
        } //else if STRUCT
        if(cur_fld.flags & FLD_LAST_FIELD) //last field
            break;
    } //end for all fields
    //now we get invalidate flag!, Then we'll handle all low-level structs!
    if(invalidate){
        invalid_reorder_types.push_back(ty_index);
        for (iter=low_level_tys.begin();iter!=low_level_tys.end();iter++)
            //if not found in invalid_list, invalidate it!
            if(!find_in_invalid_types(*iter))
                invalidate_it(*iter);
    }
    else{
#ifndef KEY
// We disable reordering of fields in a sub-struct
// Assumption: a struct sub-type points to the same entry in Ty_tab as the normal type
        for (iter=low_level_tys.begin();iter!=low_level_tys.end();iter++)
            check_reorder_legality_of_type(*iter);
#endif // !KEY
        if(!visited[ty_index]) //avoid to record twice
            can_be_reordered_types.push_back(ty_index);
    }
    low_level_tys.clear();
    visited[ty_index]=TRUE;
    return;
}// check all field 

/*-------------------------------------------------------------------------------------*/
/*WHat is illegal from global symbol table?                                            */
/*  (I) weak symbols cannot be reordered                                               */
/*  (II) For PU whose source code cannot be acquired,                                  */ 
/*      their parameter/ret STRUCT type cannot be reordered!                           */
/* Criteria:                                                                           */
/*	If a non-deleted PU satisfying:                                                */
/*		1) not appearing in CallGraph;                                         */
/*		2) export class is XPREEMPTIBLE|XOPTIONAL;                             */
/*	Then                                                                           */
/*	1) we cannot get the source code of it;                                        */
/*	2) All the types of its parameters and return values cannot be reordered!      */
/*	3) for a symbol with this type, it must be accessed through external functions!*/
/*	4) for a struct including fields of such type, it must be accessed             */
/*	through external functions! so, it must be included in the above criteria.     */
/*-------------------------------------------------------------------------------------*/
void check_gsymbol_for_invalid_type()
{
    TYPE_LIST::iterator ty_iter;
    PU_IDX cur_pu;
    TY_IDX cur_ty_idx,prototype;
    TYLIST tylist;
    BOOL found;
    ST_IDX i;
    
    // Then walk the global table--extract from cgemit.cxx  walk all CONSTANT
    for (i = 1; i < ST_Table_Size(GLOBAL_SYMTAB); ++i) {
        ST* cur_st = &St_Table(GLOBAL_SYMTAB,i);
        INDEX ty_index=cur_st->u2.type>>8;
        if(Ty_tab[ty_index].kind==KIND_STRUCT){ 
            if(ST_addr_saved (cur_st) || ST_addr_passed (cur_st)) //addr_taken
                invalidate_it(ty_index);
            else if(ST_is_weak_symbol(*cur_st))
                invalidate_it(ty_index);
        }
#ifdef KEY
	else if (Ty_tab[ty_index].kind == KIND_POINTER) {
	    INDEX pointed = TY_pointed (Ty_tab[ty_index]) >> 8;
	    if (Ty_tab[pointed].kind == KIND_STRUCT)
	    	invalidate_it (pointed);
	}
#endif // KEY
        else if(cur_st->sym_class == CLASS_FUNC 
            ||(cur_st->sym_class == CLASS_NAME 
            &&(cur_st->flags & ST_ASM_FUNCTION_ST)))
        {
            PU_IDX pu;
            IPA_NODE* node;
            pu=ST_pu(cur_st);
            //find cur_st in callgraph::name=Index_To_Str(cur_st->u1.name_idx)
            found=FALSE;
            IPA_NODE_ITER cg_iter(IPA_Call_Graph, PREORDER);
            for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
              node = cg_iter.Current();
              if (node) {
                  if(node->Func_ST()==cur_st){ //IPA_Node_Name(node)
                      found=TRUE;
                      break;
                  }
              } //if non_NULL node
            } // of traverse call graph
            if(found)
                continue; //next symbol
            // if (not found) get prototype, from Pu_Table[],
            // get Ty_table entry, get Tylist_Table netry
            tylist=Ty_Table[Pu_Table[pu].prototype].u1.tylist;
            // fist ret ,then parameter, the last is 0
            while(cur_ty_idx=Tylist_Table[tylist]){ // not the last parameter
                TY* cur_ty=&Ty_Table[cur_ty_idx];
                TY_KIND kinds=cur_ty->kind;
                INDEX index;
                if(cur_ty->kind==KIND_STRUCT&& !((cur_ty->flags)&TY_IS_UNION)){
                    if ( Get_Trace ( TP_IPA,IPA_TRACE_TUNING_NEW ) ) 
                       fprintf(TFile, "a invalid STRUCT   %s ! \n",
                               Index_To_Str(cur_ty->name_idx) );
                    index=cur_ty_idx>>8;
                    if(!find_in_invalid_types(index)){
                        invalidate_it(index);
                    }
                }
                else if(kinds==KIND_POINTER){
                    TY_IDX point_to_ty=TY_pointed(cur_ty_idx);
                    TY* cur_ty=&Ty_Table[point_to_ty];
                    if(cur_ty->kind==KIND_STRUCT&& !((cur_ty->flags)&TY_IS_UNION)){
                        index=point_to_ty>>8;
                        if(!find_in_invalid_types(index)){
                           invalidate_it(index);                           
                           if ( Get_Trace ( TP_IPA,IPA_TRACE_TUNING_NEW ) ) 
                             fprintf(TFile, "a invalid POINT_TO_STRUCT   %s ! \n",
                                     Index_To_Str(cur_ty->name_idx) );
                        }
                    }
                }
                else if(kinds==KIND_ARRAY){
                    TY_IDX base_ty=cur_ty->u2.etype;
                    TY* cur_ty=&Ty_Table[base_ty];
                    if(cur_ty->kind==KIND_STRUCT&& !((cur_ty->flags)&TY_IS_UNION)){
                        index=base_ty>>8;
                        if(!find_in_invalid_types(index)){
                           if ( Get_Trace ( TP_IPA,IPA_TRACE_TUNING_NEW ) ) 
                              fprintf(TFile, "a invalid ARRAY_TO_STRUCT   %s ! \n",
                                      Index_To_Str(cur_ty->name_idx) );
                           invalidate_it(index);                           
                        }
                    }
                }
                tylist++;
            }
        }
    }// for all symbols
    return;
}
void print_invalid_and_valid_type()
{
    TYPE_LIST::iterator iter;
    INDEX i;
    fprintf(TFile," invalid reorder types: ( size=%d)\n", (INT)invalid_reorder_types.size());
    for(i=1,iter=invalid_reorder_types.begin();
             iter!=invalid_reorder_types.end();i++,iter++)
    {
        TY& ty=Ty_tab[*iter];
        fprintf(TFile,"<%5d,%-10s>",*iter,Index_To_Str(ty.name_idx));
        if(i %4==0) fprintf(TFile,"\n");
    }
    fprintf(TFile,"\n");
    fprintf(TFile," valid reorder types: (size=%d)\n", 
            (INT)can_be_reordered_types.size());
    for(i=1, iter=can_be_reordered_types.begin();
        iter!=can_be_reordered_types.end();
        i++, iter++)
    {
        TY& ty=Ty_tab[*iter];
        fprintf(TFile,"<%5d,%-10s>",*iter,Index_To_Str(ty.name_idx));
        if(i%4==0)
            fprintf(TFile,"\n");
    }
    fprintf(TFile,"\n");
    return;
}
void Print_field_access_info()
{
    TY_SIZE size,struct_size;
    TYPE_LIST::iterator iter;
    CAND_ITEM* p_cand;
    size=reorder_candidate.size;
    p_cand=reorder_candidate.list;
    //find::BOOL find_in_reorder_candidate(TY_IDX ty_idx)
    fprintf(TFile,"reorder_candidate:<type name, flatten_fields, enclosed_struct>\n");
    for(UINT i=0;i<size;p_cand++,i++)
    {
        TY& ty=Ty_tab[p_cand->ty_index];
        struct_size=p_cand->fld_info.flatten_fields;
        fprintf(TFile,"<%-10s, %6lld, %2d>\n",
            Index_To_Str(ty.name_idx),
            struct_size,
            p_cand->flag.has_enclosed_struct);
        fprintf(TFile," fld count=(");
        for(UINT j=0;j<struct_size; j++){
            fprintf(TFile,"%10lld-",p_cand->fld_info.map[j].count);
            if(j%8==0 && j)
                fprintf(TFile,"\n");
        }
        //surely field_id 1~n, offset same with Ty_Table[]
        fprintf(TFile,")\n");
    }
}

/*------------------------------------------------------*/
/*public member 1)Get_unaltered_map()                   */
/*              2) Get_flatten_fields()                 */
/*used when 1) candidate  has no access-info(too cold)  */
/*          2) child struct of candidate is illegal     */
/*------------------------------------------------------*/
struct Handle_ty_map_and_flatten_fields{
private:
    FIELD_ID _field_id;
    INDEX _ty_index;
    FLD_ACCESS* _this_map;
    typedef list<FLD_ACCESS> MAP_LIST;
    MAP_LIST _map_list;
    MEM_POOL *_m;
    //assist function
    void Get_original_map_list(INDEX ty_index,mUINT64 cur_offset){
        FLD_ACCESS this_fld_map;
        TY *ty=&Ty_tab[ty_index];
        for(FLD_IDX cur_fld_idx=ty->u1.fld;;cur_fld_idx++){
            _field_id++;
            this_fld_map.new_field_id=_field_id;
            FLD & cur_fld=Fld_Table[cur_fld_idx];
            TY* cur_fld_ty=&Ty_Table[cur_fld.type];
            mUINT16 flags=cur_fld.flags;
            if (cur_fld.flags& FLD_IS_BIT_FIELD)
                this_fld_map.offset =cur_offset+cur_fld.bofst;
            else 
                this_fld_map.offset=cur_offset+cur_fld.ofst;
            _map_list.push_back(this_fld_map);
            if(cur_fld_ty->kind==KIND_STRUCT){
                 Get_original_map_list(cur_fld.type>>8,this_fld_map.offset);
            } //else if STRUCT
            if(cur_fld.flags & FLD_LAST_FIELD) //last field
                break;
        } //end for all fields
        return;
    }; //for non_valid struct types

    void Count_flatten_fields(INDEX ty_index)
    {

        FLD_IDX start_fld_idx=Ty_tab[ty_index].u1.fld;
        for(;;start_fld_idx++){
            _field_id++;
            TY_IDX  cur_idx= Fld_Table[start_fld_idx].type;
            TY*cur_ty=& Ty_Table[cur_idx];
            if(cur_ty->kind==KIND_STRUCT)
                Count_flatten_fields( cur_idx>>8);
            if(Fld_Table[start_fld_idx].flags & FLD_LAST_FIELD)
                break;
        } //of for
        return ;
    };
public:
    Handle_ty_map_and_flatten_fields
        (MEM_POOL *m,INDEX ty_index) {
        _m=m;   
        _field_id=0;
        _ty_index=ty_index;
    };
    FLD_ACCESS *Get_unaltered_map(){
        MAP_LIST::iterator iter;
        UINT i ;
        Get_original_map_list(_ty_index,0);
        mUINT16 size=_map_list.size();
        _this_map=(FLD_ACCESS*)MEM_POOL_Alloc_P(_m,
                    sizeof(FLD_ACCESS)*size,TRUE,NULL);
        for(iter=_map_list.begin(),i=0; iter!=_map_list.end();iter++,i++){
            _this_map[i].new_field_id=iter->new_field_id;
            _this_map[i].offset=iter->offset;
        }
        return _this_map;
    };
    FIELD_ID Get_flatten_fields() {
        if(!_field_id)
            Count_flatten_fields(_ty_index);
        return _field_id;
    };
};

#ifdef KEY

INT
Cmp_NAME_IDX(const void *p1,const void*p2)
{
  STR_IDX t1, t2;
  t1= Ty_tab [((CAND_ITEM*)p1)->ty_index].name_idx;
  t2= Ty_tab [((CAND_ITEM*)p2)->ty_index].name_idx;
  if (t1 < t2)
    return -1;
  else if (t1 > t2)
    return 1;
  else
    return 0;
}

static void
handle_duplicates (void)
{
    // remove the trace prints after debugging
    bool Trace_it=Get_Trace ( TP_IPA,IPA_TRACE_TUNING_NEW );
    qsort (reorder_candidate.list, reorder_candidate.size, sizeof(CAND_ITEM),
  					Cmp_NAME_IDX);

    CAND_ITEM *p_cand, *prev_p_cand, *start=NULL, *end=NULL;
    int i;
    for (i=0, p_cand = reorder_candidate.list; i<reorder_candidate.size; 
    						++i, ++p_cand)
    {
    	INDEX ind = p_cand->ty_index;	
	INDEX prev_ind;
	TY *t1, *t2;

	if (i) 
	{
	    prev_ind = prev_p_cand->ty_index;
	    t1 = &Ty_tab[prev_ind];
	    t2 = &Ty_tab[ind];

	}
	if (i && t1->name_idx == t2->name_idx)
	{
	    if (!start) start = prev_p_cand;

	    if (i == (reorder_candidate.size-1))
	    {
	    	FmtAssert (!end, ("Reorder list processing error"));
	    	end = p_cand;

	    	for (CAND_ITEM* j=start; j<=end; ++j)
	    	  for (int k=0; k<j->fld_info.flatten_fields; ++k)
		    j->fld_info.map[k].count = 0;
	    }
	}
	else if (start)
	{// reached the end of a stretch
	    FmtAssert (!end, ("Reorder list processing error"));
	    end = prev_p_cand;

	    // reset the hotness counts for these types.
	    // We do not want them reordered.
	    for (CAND_ITEM* j=start; j<=end; ++j)
	    	for (int k=0; k<j->fld_info.flatten_fields; ++k)
		    j->fld_info.map[k].count = 0;

	    start = end = NULL;
	}
	else // prev_ind name_idx != ind name_idx, start == NULL
	{
	    prev_p_cand = p_cand;
	    continue;
	}

	prev_p_cand = p_cand;
    }

    if (Trace_it)
    {
	fprintf (TFile, "After sorting and deleting duplicates\n");
    	for (int i=0; i<reorder_candidate.size; ++i)
	    fprintf (TFile, "%lld\n", Ty_tab[(reorder_candidate.list+i)->ty_index].name_idx);
    }
}

#endif // KEY

/*----------------------------------------------------------------------------*/
/*function : IPA_reorder_legality_process()                                   */
/*  get_can_be_reordered_types();                                             */
/*  check_gsymbol_for_invalid_type();                                         */
/*  init reorder_candidate with                                               */
/*      1)access-count from merged_access, and handling it                    */
/*      2)top_field info , and field_info such as flatten_fields              */
/*      3)prepare for new-ordering-sorting, get num of sub-fields,            */
/*          fill it in map's offset , to assist decision of new_offset        */
/*Input assumption:                                                            */
/*  in can_be_reorder_types,child struct appears ahead of any parent struct    */
/*Handling sequence:                                                            */
/*      first child struct, then parent struct                                  */
/*About access count:                                                            */
/*     1)access count of a non-top-level-field is accumulated to                 */
/*       corresponding child (offspring) struct                                  */
/*     2)all access count of low-level-field is summarized to its top-level-field*/
/*-------------------------------------------------------------------------------*/



void IPA_reorder_legality_process()
{
    INDEX size,ii,i,j,ty_num,cur_struct;
    TYPE_LIST::iterator iter; // for debug
    CAND_ITEM *p_cand,*sub_cand;
    TOP_FLD_DESC idlist;
    TOP_FLD_LIST ::iterator id_iter;
    FLD_ACCESS * this_map;
    BOOL Trace_it;
    FIELD_ID pre_id,cnt, flatten_flds;
    INDEX index, pre_ty_index;
    COUNT sum_count;
    MERGED_ACCESS* cur_merge;

    get_can_be_reordered_types();
    check_gsymbol_for_invalid_type();
    Trace_it=Get_Trace ( TP_IPA,IPA_TRACE_TUNING_NEW );
    if ( Trace_it)
       print_invalid_and_valid_type();
    size=can_be_reordered_types.size();
    reorder_candidate.size=size;
    if(size)
        reorder_candidate.list = (CAND_ITEM*)MEM_POOL_Alloc_P(&reorder_local_pool,
           sizeof(CAND_ITEM)*size,TRUE,NULL);
    for(iter=can_be_reordered_types.begin(), p_cand=reorder_candidate.list;
           iter!=can_be_reordered_types.end();
           iter++, p_cand++) 
    {
        cur_struct=*iter; /*for debug*/
        p_cand->ty_index=cur_struct;
        strcpy(p_cand->ty_name,Index_To_Str(Ty_tab[cur_struct].name_idx)); //just for debug

        cur_merge=find_merged_access(cur_struct);
        //if(!iter_merge)---no access info, i.e this struct is not a hot-access-struct
        // but perhaps its child struct is hot! we cannot delete this struct in reorder_candidate
        p_cand->flag.changed=FALSE;
        p_cand->top_field_info.size=0;
        Handle_ty_map_and_flatten_fields handle_ty(&reorder_local_pool,cur_struct);
        if(cur_merge)
            flatten_flds=cur_merge->flatten_fields;
        else{
            flatten_flds=handle_ty.Get_flatten_fields();
        }
        p_cand->fld_info.flatten_fields=flatten_flds;
        p_cand->flag.changed=FALSE;
        p_cand->top_field_info.size=0;
        p_cand->fld_info.map= (FLD_ACCESS*)MEM_POOL_Alloc_P(&reorder_local_pool,
                    sizeof(FLD_ACCESS)*flatten_flds,TRUE,NULL);
        memset(p_cand->fld_info.map,0,sizeof(FLD_ACCESS)*flatten_flds);

        this_map=p_cand->fld_info.map;
#ifdef KEY
	FLD_ACCESS * get_old_ofst = handle_ty.Get_unaltered_map();
#endif // KEY
        for(i=0;i<flatten_flds;i++){
            if(cur_merge)
                this_map[i].count=cur_merge->count[i];
            this_map[i].old_field_id=i+1;/*init all fld_info.map[].field_id ascending, offset =0*/
            this_map[i].offset=0;
#ifdef KEY
	    this_map[i].old_offset = get_old_ofst[i].offset;
#endif // KEY
        }
    }
#ifdef KEY
// We need to remove duplicates from reorder_candidate before we can
// do field-reordering, otherwise we will end up reordering the same struct
// fields differently in different instances!
    handle_duplicates ();
#endif // KEY
    for(cnt=1,p_cand=reorder_candidate.list;cnt<=size;
        p_cand++,cnt++) {
        cur_struct=p_cand->ty_index;
        this_map=p_cand->fld_info.map;
        flatten_flds=0;
        idlist.size=0;

        Handle_access_count(cur_struct, idlist, flatten_flds, p_cand);
        p_cand->top_field_info.size=idlist.size;
        p_cand->top_field_info.list= (TOP_FIELD*)MEM_POOL_Alloc_P(&reorder_local_pool,
                    sizeof(TOP_FIELD)*p_cand->top_field_info.size,
                    TRUE,NULL);
        UINT i=0;
        pre_id=0;
        pre_ty_index=0;
        sum_count=0;
        flatten_flds=p_cand->fld_info.flatten_fields;
        for(id_iter=idlist.list.begin();id_iter!=idlist.list.end();i++,id_iter++){
            mUINT16 field_id=id_iter->field_id;
            mUINT64 size=id_iter->size;
            p_cand->top_field_info.list[i].field_id=field_id;
            p_cand->top_field_info.list[i].ty_idx=id_iter->ty_idx;
            p_cand->fld_info.map[field_id-1].offset=size; //record num of child-fields here!
#ifdef KEY
            p_cand->fld_info.map[field_id-1].align=id_iter->align; //record alignment of field here
#endif // KEY
            pre_ty_index=id_iter->ty_idx>>8;
            pre_id=field_id;
        }
        idlist.list.clear();
      }
    if (Trace_it )
       Print_field_access_info();
    return;
}

/*assist function for  IPO_get_new_ordering() */
INT
Cmp_FLD_ACCESS(const void *p1,const void*p2)
{
  FLD_ACCESS* t1,*t2;
  t1=(FLD_ACCESS*)p1;
  t2=(FLD_ACCESS*)p2;
  if ( t1->count <t2->count )
    return 1;
  else if ( t1->count >t2->count  )
    return -1;
  else
    return 0;
}
INT
Cmp_old_field_id(const void *p1,const void*p2)
{
  FLD_ACCESS* t1,*t2;
  t1=(FLD_ACCESS*)p1;
  t2=(FLD_ACCESS*)p2;
  if ( t1->old_field_id >t2->old_field_id)
    return 1;
  else if ( t1->old_field_id<t2->old_field_id)
    return -1;
  else
    return 0;
}

#ifdef KEY
static void
undo_field_reordering (FLD_ACCESS * flds, int count)
{
  for (int i=0; i<count; ++i)
  {
    flds[i].new_field_id = flds[i].old_field_id;
    flds[i].offset = flds[i].old_offset;
  }
}
#endif // KEY

/*------------------------------------------------------------------*/
/*function name :   IPO_get_new_ordering()                          */
/*  1) get a new order according to field_access_info collected in IPL*/
/*  2)reorder hieruris algo: simple. just according to hottnes      */
/*                   acquire mapping for field-id and offset        */
/*HOW to get a new ordering?                                        */
/*  Firrst decide mapping for top-level field, then low-level fiel  */
/*INPUT:                                                            */
/*  1)in reorder_candidate, child type appears ahead of parent type */
/*  2)map<count, old_field-id,new_field_id,new_offset>,             */
/*      the former two is filled                                    */
/*      new_offset contains num-of-sub-fields                       */
/*  3)for flds with parent-child-relationship, they have the same access-count     */
/*SUppose:  two structs                                             */ 
/*  sub_struct{                                                      */
/*          int s1,s2;                                               */
/*          real s3                                                  */
/*         };        --New ordering is <1,3,2>                       */
/*  top_struct {                                                     */
/*          int t1,t2;                                               */
/*          sub_struct t3                                            */
/*          } ;      --New ordering for top-level-field is <3,1,2>  */
/*for top_struct, fld_info is (count,offset)=                        */
/*		<(100,2), (100,0), (100,0), (300,0), (200,0)>        */
/*So, mapping for field-id of  top_struct is<3,5,4,1,2>              */
/* Algo:                                                              */
/*      for each struct in reorder_candidate (child ty appears ahead of parent ty)*/
/*          sort according to access-count,                        */
/*          get new_field_id,                                      */
/*          compute new_offset for top-level-field,                 */
/*          note: sub-field firstly, having the same offset as their parent field     */
/*          compute sub-field's new offset by adding sub-struct's offset-mapping */
 /*i.e. ordering of low-level-fields is inherited from the  corresponding child (offspring) ty */
/*--------------------------------------------------------------------*/

void IPO_get_new_ordering()
{
    TYPE_LIST::iterator iter;
    INDEX i;
    FIELD_ID fld_num, sub_fld_num,top_size,k,kk;
    CAND_ITEM* p_cand, *sub_cand;
    FLD_ACCESS*be_sorted_list,*sub_map;
    TOP_FIELD *ref_list;
    COUNT count;
    TY_SIZE cur_size;
    FLD_OFST cur_offset;
    INDEX cur_ty_index,cur_struct,size;
    PTR_AND_TY_VECTOR::iterator ptr_iter;
    BOOL Trace_it=Get_Trace ( TP_IPA,IPA_TRACE_TUNING_NEW ) ;
    size=reorder_candidate.size;
    for(i=0,p_cand=reorder_candidate.list; i<size; i++,p_cand++)
    {
        ref_list=p_cand->top_field_info.list;
        be_sorted_list=p_cand->fld_info.map;
        top_size=p_cand->top_field_info.size;
        fld_num=p_cand->fld_info.flatten_fields;
        //sort according to hottness, fld-count in descending order
        qsort( be_sorted_list,fld_num, sizeof(FLD_ACCESS), Cmp_FLD_ACCESS);
        //fill in new_field_id , and offset
        cur_offset=0;
	int max_align = 0; // KEY
        for(k=0;k<fld_num;k++){
            be_sorted_list[k].new_field_id=k+1;
            cur_size=be_sorted_list[k].offset;
#ifdef KEY
            if(!cur_size && k) // a sub-field
	    {
                be_sorted_list[k].offset = be_sorted_list[k-1].offset;
	    }
            else
	    {
		if (be_sorted_list[k].align > max_align)
		    max_align = be_sorted_list[k].align;
	    	int rem = 0;
		if (be_sorted_list[k].align)
		    rem = cur_offset % be_sorted_list[k].align;
		if (rem)
		{
                    be_sorted_list[k].offset = cur_offset + 
		    			be_sorted_list[k].align - rem;
                    cur_offset += (cur_size + be_sorted_list[k].align - rem);
		}
		else
		{
                    be_sorted_list[k].offset=cur_offset;
                    cur_offset+=cur_size;
		}
            }
#else
            if(!cur_size && k) // a sub-field
                be_sorted_list[k].offset=be_sorted_list[k-1].offset;
            else{
                be_sorted_list[k].offset=cur_offset;
                cur_offset+=cur_size;
            }
#endif // KEY
        }
#ifdef KEY
	// cur_offset is now the size of the struct
	{
	    // The ABI says that the size must be multiple of the largest
	    // alignment.
	    int rem = 0;
	    if (max_align && (rem = cur_offset % max_align))
	    	cur_offset += max_align - rem;
	}
	// if size changes, we cannot reorder it
	if (Ty_tab [p_cand->ty_index].size != cur_offset)
	    undo_field_reordering (be_sorted_list, fld_num);
#endif // KEY
        //sort according to old_field_id in ascending order
        qsort( be_sorted_list,fld_num, sizeof(FLD_ACCESS), Cmp_old_field_id);
        // for all parent-fld, refill its mapping of fld_id& offset
            /*----------------------------------------------*/
            /*for each of its sub_struct{                   */
            /*  get its map for field-id, add to top_struct */
            /*  get its map for offset, add to top_struct   */
            /*}                                             */
            /*----------------------------------------------*/
        if(p_cand->flag.has_enclosed_struct)

        for(k=0;k<top_size;k++){
            if(cur_ty_index=ref_list[k].ty_idx>>8){
                sub_cand=find_in_reorder_candidate(cur_ty_index);
#ifdef KEY
		FmtAssert (!sub_cand || sub_cand->fld_info.map, 
			("Field reordering information not available"));
                if(!sub_cand || !sub_cand->fld_info.map[0].new_field_id){ 
		// sub struct is not a candidate, or new_field_id for this
		// candidate has not been assigned yet.
#else
                if(!sub_cand){ //sub struct is not a candidate!
#endif // KEY
                    Handle_ty_map_and_flatten_fields handle_ty(&reorder_local_pool,cur_ty_index);
                    sub_map=handle_ty.Get_unaltered_map();
                    sub_fld_num=handle_ty.Get_flatten_fields();
                }
                else {
                    sub_map=sub_cand->fld_info.map;
                    sub_fld_num=sub_cand->fld_info.flatten_fields;
                }
                mUINT32  sub_fld_start=be_sorted_list[ref_list[k].field_id-1].new_field_id;
                mUINT64  sub_off_start=be_sorted_list[ref_list[k].field_id-1].offset;
                mUINT32  old_fld_start=be_sorted_list[ref_list[k].field_id-1].old_field_id;
                for(kk=0;kk<sub_fld_num;kk++){
                    be_sorted_list[kk+old_fld_start].new_field_id=
                             sub_fld_start+sub_map[kk].new_field_id;
                    be_sorted_list[kk+old_fld_start].offset=
                             sub_off_start+sub_map[kk].offset;
                }

            }
        }
        //check if the ordering changed, fill in "changed"
        p_cand->flag.changed=FALSE;
        for(k=0;k<fld_num;k++){
            if(be_sorted_list[k].new_field_id!=k+1){
                p_cand->flag.changed=TRUE;
                break;
            }
        } //check if changed, new ordering!
        if( p_cand->flag.changed){
#ifdef KEY
	    if (Trace_it)
	    	fprintf (stderr,"CHANGED: Name: %s, size: %lld\n", 
			&Str_Table[Ty_tab [p_cand->ty_index].name_idx], 
			Ty_tab[p_cand->ty_index].size);
#endif // KEY
            cur_struct=p_cand->ty_index;
            Ty_to_cand_map.insert(make_pair(cur_struct,p_cand));
            //insert all pointer_types
            for(ptr_iter=Ptr_and_ty_vector->begin();
                ptr_iter!=Ptr_and_ty_vector->end();
                ptr_iter++){
                if(ptr_iter->pt_index==cur_struct){
                    Ty_to_cand_map.insert(make_pair(ptr_iter->ty_index,p_cand));
                    if(Trace_it)
                       fprintf(TFile,"ADD TO Ptr_and_ty_vector:<%d,%d>\n",
                             (*ptr_iter).ty_index,(*ptr_iter).pt_index);
                }
            } // fill in all such pointer_tys
        } 
    }
    //just to print the mapping of field_id and offset
    p_cand=reorder_candidate.list;
    if ( Trace_it){
        fprintf(TFile,"maping_info:\n");
        for( i=0;i<size;p_cand++,i++)
        {
            TY& ty=Ty_tab[p_cand->ty_index];
            fld_num=p_cand->fld_info.flatten_fields;
#ifdef KEY	// give some more information
// Note: Different entries in Ty_tab can have the same type_name
	    if (!p_cand->flag.changed) continue;
            fprintf(TFile,"type name: %s, type id: %d, flatten_fields: %d, changed: %d\n",
                Index_To_Str(ty.name_idx),
		p_cand->ty_index,
                fld_num,
                p_cand->flag.changed);
#else
            fprintf(TFile,"type name :%s, flatten_fields:%d, changed=%d\n",
                Index_To_Str(ty.name_idx),
                fld_num,
                p_cand->flag.changed);
#endif // KEY
            fprintf(TFile,"\nmapping for field_id:\n");
            for(UINT j=0;j<fld_num; j++){
                fprintf(TFile," %4d",p_cand->fld_info.map[j].new_field_id);
                if(j%20==0&& j)
                    fprintf(TFile,"\n");
    
            }
            fprintf(TFile,"\nmapping for offset:\n");
            for(UINT j=0;j<fld_num; j++){
#ifdef KEY
                fprintf(TFile," %d",p_cand->fld_info.map[j].offset);
#else
                fprintf(TFile,"%5d",p_cand->fld_info.map[j].offset);
#endif // KEY
                if(j%16==0 && j)
                    fprintf(TFile,"\n");
    
            }
            fprintf(TFile,"\n");
        }
    }

    return;
}
/*assist function for  IPO_get_new_ordering() */
typedef struct{
    FIELD_ID new_top_id;
    FIELD_ID old_top_id;
}TOP_FLD_ID_TRANS;
INT
Cmp_new_top_id(const void *p1,const void*p2)
{
  TOP_FLD_ID_TRANS* t1,*t2;
  t1=(TOP_FLD_ID_TRANS*)p1;
  t2=(TOP_FLD_ID_TRANS*)p2;
  if ( t1->new_top_id >t2->new_top_id)
    return 1;
  else if ( t1->new_top_id<t2->new_top_id)
    return -1;
  else
    return 0;
}
INT
Cmp_old_top_id(const void *p1,const void*p2)
{
  TOP_FLD_ID_TRANS* t1,*t2;
  t1=(TOP_FLD_ID_TRANS*)p1;
  t2=(TOP_FLD_ID_TRANS*)p2;
  if ( t1->old_top_id >t2->old_top_id)
    return 1;
  else if ( t1->old_top_id<t2->old_top_id)
    return -1;
  else
    return 0;
}

/*--------- ----------------------------------------*/
/*function name :   IPO_reorder_Fld_Tab()            */
/* for each fld_tab entry of this struct             */
/*   rearrange (reorder) the item to its new offset  */
/*   modify ofst, set flag bit about FLD_LAST_FIELD  */
/*---------------------------------------------------*/

void IPO_reorder_Fld_Tab()
{
    MEM_POOL_Popper popper(&reorder_local_pool);

    FIELD_ID j,ii;
    mUINT32 k;
    INDEX i;
    FIELD_ID *addr_list,top_size,size;
    TOP_FLD_ID_TRANS *trans_list;
    CAND_ITEM* p_cand;
    FLD *tmp_fld; // FLD has no constructer()
    FLD_ACCESS *ref_list;
    TOP_FIELD *top_list;
    tmp_fld= (FLD*)MEM_POOL_Alloc_P(&reorder_local_pool,
            sizeof(FLD),TRUE,NULL);
    size=reorder_candidate.size;
#ifdef KEY
    bool Trace_it=Get_Trace ( TP_IPA,IPA_TRACE_TUNING_NEW );
    FLD *tmp_fld1 = (FLD*)MEM_POOL_Alloc_P(&reorder_local_pool,
                sizeof(FLD),TRUE,NULL);
#endif // KEY
    for(i=0,p_cand=reorder_candidate.list; i<size; i++,p_cand++)
    {
#ifdef KEY
	bool changed = false;
#endif // KEY
        MEM_POOL_Push (&reorder_local_pool);
        UINT start_fld_idx=Ty_tab[p_cand->ty_index].u1.fld;
        top_size=p_cand->top_field_info.size;
        trans_list = (TOP_FLD_ID_TRANS*)MEM_POOL_Alloc_P(&reorder_local_pool,
            sizeof(TOP_FLD_ID_TRANS)*top_size,TRUE,NULL);
        for(k=0;k<top_size;k++){
            mUINT16 old_id=p_cand->top_field_info.list[k].field_id;
            //firstly, fill in field_id, later modify to top_id
            trans_list[k].new_top_id=p_cand->fld_info.map[old_id-1].new_field_id-1; 
            trans_list[k].old_top_id=k; 
#ifdef KEY
	    if (trans_list[k].old_top_id != trans_list[k].new_top_id)
	    	changed = true;
#endif // KEY
        }
#ifdef KEY
	if (!changed) continue;
#endif // KEY
        //sort according to new_fld_id in descending order
        qsort( trans_list,top_size, sizeof(TOP_FLD_ID_TRANS), Cmp_new_top_id);
        for(k=0;k<top_size;k++){
            trans_list[k].new_top_id=k;
        }
        qsort( trans_list,top_size, sizeof(TOP_FLD_ID_TRANS), Cmp_old_top_id);
        addr_list = (FIELD_ID*)MEM_POOL_Alloc_P(&reorder_local_pool,
            sizeof(FIELD_ID)*top_size,TRUE,NULL);
        for(k=0;k<top_size;k++)
            addr_list[k]=trans_list[k].new_top_id;

        //rearrange the addr_list synchronically with fld_tab
        ref_list=p_cand->fld_info.map;
        top_list=p_cand->top_field_info.list;

        for(ii=0;ii<top_size;ii++){
        //ii<top_size-1 will be right, but we'll fill in ofst for all!
#ifdef KEY
// the following loop is entered only once, so it does not change the 'Order' of the loop
	    for (j=0, k=0; (ii==0) && (j<top_size); j++) {
	    // move Fld_Table[start_fld_idx+j] to Fld_Table[start_fld_idx+addr_list[j]]

		if (addr_list[k] == k)	{ 
		    k++;
		    continue;
		}
                memcpy(tmp_fld,&Fld_Table[addr_list[k]+start_fld_idx],sizeof(FLD));
		if (j)
                    memcpy(&Fld_Table[addr_list[k]+start_fld_idx], tmp_fld1, sizeof(FLD));
		else
                    memcpy(&Fld_Table[addr_list[k]+start_fld_idx], &Fld_Table[k+start_fld_idx],
                           sizeof(FLD));
	    	k = addr_list[k];
                memcpy(tmp_fld1,tmp_fld, sizeof(FLD));
		FmtAssert (k != addr_list[k], ("Field reordering error"));
	    }
#else
            if(addr_list[ii]!=ii){
                j=ii;
                memcpy(tmp_fld,&Fld_Table[ii+start_fld_idx],sizeof(FLD));
                /*class FLD has no copy-constructor, so cannot use"assignment"*/
                while(addr_list[j]!=ii){
                    k=addr_list[j];
                    memcpy(&Fld_Table[j+start_fld_idx],
                           &Fld_Table[k+start_fld_idx],
                           sizeof(FLD));
                    addr_list[j]=j;
                    j=k;
                } //of while (addr_list[j]!=ii)
                memcpy(&Fld_Table[j+start_fld_idx],tmp_fld,sizeof(FLD));
                addr_list[j]=j;
            }//of if addr_list[ii]!=ii
#endif // KEY
            //ii is new_top_id
            //find its old_top_id;
            for(k=0;k<top_size;k++)
                if(trans_list[k].new_top_id==ii)
                    break;
            FIELD_ID old_top_id=trans_list[k].old_top_id; //trans_list[ii].old_top_id;
            FIELD_ID old_field_id=top_list[old_top_id].field_id;
            Fld_Table[start_fld_idx+ii].ofst=ref_list[old_field_id-1].offset;
#ifdef KEY
	    if (Trace_it)
	    {
#endif // KEY
            fprintf(stderr,"<%lld>",Fld_Table[start_fld_idx+ii].ofst);
            if((ii+1)%10==0)
                fprintf(stderr,"\n");
#ifdef KEY
	    }
#endif // KEY
            if(ii!=top_size-1)
                //clear last_field_flag
                Fld_Table[start_fld_idx+ii].flags &= ~FLD_LAST_FIELD;
            else
                //set last_field_flag
                Fld_Table[start_fld_idx+ii].flags |= FLD_LAST_FIELD;
        }//of while ii!=top_size
#ifdef KEY
	if (Trace_it)
#endif // KEY
        fprintf(stderr,"\n");
        FLD_OFST pre_ofst;
        for(j=0;j<top_size;j++){
            k=start_fld_idx+j;
            //Maybe not strictly ascending! 
            //its (first fld )is KIND_INVALID, size==0!!
            if(j)  //non-zero
               FmtAssert(Fld_Table[k].ofst>=pre_ofst,("fld offset must be ascending !\n"));
            else 
               FmtAssert(Fld_Table[k].ofst==0,("first fld offset must be zero !\n"));
            pre_ofst=Fld_Table[k].ofst;
        }
        FmtAssert(Fld_Table[k].flags &FLD_LAST_FIELD,("last fld flag is wrong !\n"));
        MEM_POOL_Pop(&reorder_local_pool);
    }
    return;
}

/*assist function for WN_Modify*/
BOOL map_field_id_and_offset(INDEX index, FIELD_ID old_field_id,WN_OFFSET old_offset, 
    FIELD_ID &new_field_id, WN_OFFSET &new_offset)
{
    CAND_ITEM * cur_cand;
    WN_ITER* itr;
    UINT i;
    FLD_ACCESS *map;
    TY_TO_CAND_MAP::const_iterator map_iter;
    WN_OFFSET struct_size;
    INT64 extra;
    char *p_ch=Index_To_Str(Ty_tab[index].name_idx);//just for debug
    TY &this_ty=Ty_tab[index]; //for debug
    map_iter=Ty_to_cand_map.find(index);
    if(map_iter==Ty_to_cand_map.end())//not find it
        return FALSE;
    else 
        cur_cand=map_iter->second; 
    struct_size=Ty_tab[cur_cand->ty_index].size;
     //get a positive offset, then map it!
     //offset=extra+positive_offset. where (positive-offset<struct_size)
     /*I have no floor():: extra=(INT64)floor(old_offset/struct_size)*struct_size;*/
     /*module=0 <=> field_id=1*/
     TY_SIZE module=old_offset%struct_size;
     if(module==0)
        extra=old_offset;
     else{ 
        WN_OFFSET  tmp=old_offset/struct_size;
        extra=tmp*struct_size;
        if(old_offset<0)
        extra-=struct_size;
     }
    /* extra=old_offset-WN_offset(wn)%struct_size;*/
    map=cur_cand->fld_info.map;
    new_field_id=map[old_field_id-1].new_field_id;
    new_offset=map[old_field_id-1].offset+extra;
    return TRUE;

}

/*---------------------------------------------------------------------*/
/*Note:  if(abs(offset)>struct_size, then offset_map need add an extra)*/
/*              floor() need id_map.h?                                 */
/*---------------------------------------------------------------------*/

void IPO_Modify_WN_for_field_reorder (IPA_NODE* node)
{
    WN * wn,*wn1;
    CAND_ITEM * cur_cand;
    WN_ITER* itr;
    FIELD_ID fld_id,new_field_id,i;
    FLD_ACCESS *map;
    TY_TO_CAND_MAP::const_iterator map_iter;
    PTR_AND_TY_VECTOR::iterator ptr_iter;
    WN_MAP Parent_Map;
    WN_OFFSET old_offset,new_offset;
    TY_SIZE struct_size;
    WN* parent,*wn_const ;
    INDEX  point_idx;

    Parent_Map=node->Parent_Map();
    wn1 = node->Whirl_Tree();
    Is_True(wn1 != NULL, (" NULL whirl encountered \n"));
    for ( itr = WN_WALK_TreeIter(wn1);  
        itr != NULL;
        itr = WN_WALK_TreeNext(itr)) {//preorder traverse!

        wn = itr->wn;
        old_offset=WN_offset(wn);

#ifdef KEY
	if (WN_operator(wn) == OPR_LDID)
	{
	    WN * kid;
	    INDEX addr = WN_ty(wn) >> 8;
	    if (Ty_tab[addr].kind == KIND_POINTER && 
	    		Ty_Table[TY_pointed (Ty_tab[addr])].kind == KIND_STRUCT)
	    	addr = TY_pointed (Ty_tab[addr]) >> 8;

    	    TY_TO_CAND_MAP::const_iterator map_iter;
	    if ((Ty_tab[addr].kind == KIND_STRUCT) && 
		(map_iter=Ty_to_cand_map.find(addr))!=Ty_to_cand_map.end())
	    {
	    	WN * tmp = WN_Get_Parent (wn, Parent_Map, Current_Map_Tab);
	    	if (tmp && WN_operator(tmp) == OPR_ADD)
	    	{
		    for (int i=0; i<WN_kid_count(tmp); ++i)
			if ((kid=WN_kid(tmp,i)) && WN_operator(kid) == OPR_INTCONST)
		    	{
			    CAND_ITEM * cur_cand = map_iter->second; 
			    FLD_ACCESS * fields = cur_cand->fld_info.map;
			    for (int j=0; j<cur_cand->fld_info.flatten_fields; ++j)
			    {
			    	if (fields[j].old_offset == WN_const_val (kid))
				{
				    WN_const_val (kid) = fields[j].offset;
				    break;
				}
			    }
		   	}
	    	}
	    }
	}
#endif // KEY

        switch (WN_operator(wn)) {
        case OPR_ILOAD:{
            OPCODE opcode;
            opcode = WN_opcode(wn);
            FmtAssert(OPCODE_has_2ty(opcode),("ILoad has two Tys!\n"));
            }
        case OPR_ISTORE:
        case OPR_ILDA:
        case OPR_LDA:
        case OPR_MSTORE:
        case OPR_MLOAD:
        case OPR_LDID:
        case OPR_STID:{
            TY_IDX ty_idx=WN_ty(wn); //ty_idx of loaded object
            INDEX index=ty_idx>>8;
            fld_id=WN_field_id(wn); //inc field access count
            if(fld_id<=0) continue; // 0 scalar;<0 rotate register
            else if (map_field_id_and_offset(index,fld_id,WN_load_offset(wn),
                                             new_field_id,new_offset))
            {
                 WN_set_field_id(wn,new_field_id);
                 WN_store_offset(wn)=new_offset;
            } //fld_id!=0
            break;
            }
        }// of al cases
    }
    #ifdef Is_True  
    WN_verifier(wn1);
    Verify_GLOBAL_SYMTAB();
    #endif
    node->Set_Whirl_Tree(wn1);
    if(Get_Trace ( TP_IPA,IPA_TRACE_TUNING_NEW ))
        fdump_tree(TFile, wn1);
    return;
}//of if reorder_cand
/*----------------------------------------------------------------------------*/
/*Compare_whirl_tree()                                                        */
/*just for debug, when comparing the difference of reordering and no-reordering*/
/*-----------------------------------------------------------------------------*/
void Compare_whirl_tree(IPA_NODE* node)
{
    WN *wn;
    CAND_ITEM * cur_cand;

    wn = node->Whirl_Tree();
    Is_True(wn != NULL, (" NULL whirl encountered \n"));
    if(Get_Trace ( TP_IPA,IPA_TRACE_TUNING_NEW ))
        fdump_tree(TFile, wn);
    return;
}

void IPO_Finish_reorder()
{
    MEM_POOL_Pop(&reorder_local_pool);
}
