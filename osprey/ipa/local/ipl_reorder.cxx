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


#include "ipl_reorder.h"    // for Ptr_to_ty_vector, local_cands
#include "mempool.h"
#include "cxx_memory.h"

MEM_POOL  reorder_ipl_pool;
PTR_TO_TY_VECTOR *Ptr_to_ty_vector; //mapping pointer_ty to tys
TY_TO_FLDNUM_MAP *local_cands;    //<ty_index,flatten_flds>


void Comput_flatten_flds(mUINT32 ty_index,mUINT32 &field_id)
{
    FLD_IDX start_fld_idx=Ty_tab[ty_index].u1.fld;
#ifdef KEY // check for 0-sized structs
    if (start_fld_idx == FLD_IDX_ZERO)
      return;
#endif
    for(;;start_fld_idx++){
        TY_IDX  cur_idx= Fld_Table[start_fld_idx].type;
        field_id++;
        if(Ty_Table[cur_idx].kind==KIND_STRUCT){
            Comput_flatten_flds( cur_idx>>8,field_id);
        }
        if(Fld_Table[start_fld_idx].flags & FLD_LAST_FIELD)
            break;
    } //of for
    return;
}

void Preprocess_struct_access(void)
{
    /*----------------------------------------------------------------------*/
    /*assumption: in Ty_tab[], a pointer type lies after the point_to type  */
    /*function: get local_cands(struct & point-to_struct), fill in fatten_flds*/
    /*          get Ptr_to_ty_map                                             */
    /*------------------------------------------------------------------------*/
    //TODO:
    //for each entry in Ty_tab[]
    //  if(STRUCT){
    //      get fld_num and size;
    //      if(fld_num>3 and size<cache_block)
    //          put struct_index into local_cands
    //  }else if(is POINTER & point_to_ty is in local_cands){
    //      put <ty, point_to_ty> to Ptr_to_ty_vector
    //      put <ty,0> to local_cand
    TY_TAB::iterator  iter;
    BOOL propagate;
    TY_TO_FLDNUM_MAP ::const_iterator iter_cand;
    PTR_TO_TY item;
    mUINT32  point_to, flatten_flds,struct_index,ty_index;
    TY *ty;
    MEM_POOL_Initialize(&reorder_ipl_pool,"reorder_ipl_pool",TRUE);
    MEM_POOL_Push (&reorder_ipl_pool);

    Ptr_to_ty_vector=CXX_NEW(PTR_TO_TY_VECTOR(),&reorder_ipl_pool);
    local_cands=CXX_NEW(TY_TO_FLDNUM_MAP(20),&reorder_ipl_pool);

    UINT size=Ty_tab.size();
    UINT i,j;
    for(i=1,j=1,iter=Ty_tab.begin();iter!=Ty_tab.end();iter++,i++)
    {
        if((*iter).kind==KIND_STRUCT&& ((*iter).size>=cache_block)){
            flatten_flds=0;
            struct_index=iter.Index();
            Comput_flatten_flds(struct_index,flatten_flds);
            if(flatten_flds>=min_fld_num_reorder &&
                    iter->size>cache_block){
#ifdef KEY
                local_cands->insert(std::make_pair(struct_index,flatten_flds));
#else
                local_cands->insert(make_pair(struct_index,flatten_flds));
#endif // KEY
            }
            else continue;
        } //just handle struct type
        else if((*iter).kind==KIND_POINTER){
            //find if pont_to STRUCT
            //  put <ty, point_to_ty> to Ptr_to_ty_vector
            //  put <ty,0> to local_cands
            ty_index=iter.Index();
            TY_IDX point_to_ty=TY_pointed(*iter)>>8;
            iter_cand=local_cands->find(point_to_ty);
            if (iter_cand==local_cands->end ()) // point_to_ty not a candidate
                continue;
#ifdef KEY
            local_cands->insert(std::make_pair(ty_index,0)); // a pointer_ty has a flatten_flds of 0;
#else
            local_cands->insert(make_pair(ty_index,0)); // a pointer_ty has a flatten_flds of 0;
#endif // KEY
            item.ty_index=ty_index;
            item.pt_index=point_to_ty;
            Ptr_to_ty_vector->push_back(item);
        }
    }//walk through all types
    return;
}

