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

#ifndef ipl_reorder_INCLUDED
#define ipl_reorder_INCLUDED
#include "defs.h" 	//for mUINT32
#include "symtab.h"
#include "symtab_defs.h"  //for all tables,such as Ty_tab[]and Ty_Table etc.
#ifdef KEY
#include <ext/hash_map>
#else
#include <hash_map.h>
#endif // KEY
#include <vector>
extern
MEM_POOL  reorder_ipl_pool;
#define cache_block 32	//bytes TODO: get from cache-model 
#define min_fld_num_reorder 3
struct PTR_TO_TY
{
	mUINT32 ty_index;
	mUINT32 pt_index;
};
typedef vector<struct PTR_TO_TY> PTR_TO_TY_VECTOR;
extern 
 PTR_TO_TY_VECTOR *Ptr_to_ty_vector; //mapping pointer_ty to tys
 typedef hash_map<mUINT32,mUINT32> TY_TO_FLDNUM_MAP;
 extern
 TY_TO_FLDNUM_MAP *local_cands; //<ty_index,flatten_lds>
extern
void  Preprocess_struct_access (void);
extern 
void Finish_collect_struct_access (void);

#endif
