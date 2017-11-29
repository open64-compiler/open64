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

#include <vector>
#include <list>
#include <map>

#include "symtab.h"
#include "symtab_defs.h" 	//for all tables,such as Ty_tab[]and Ty_Table etc.
#include "cxx_template.h" 	// for STACK
#include "wn_core.h" 	//for WN_field_id(wn)
#include "ipa_cg.h"	//for IPA_NODE, IPA_NODE_ITER
#ifdef KEY
#include <ext/hash_map>
#else
#include <hash_map.h>
#endif // KEY
#ifndef CXX_MEMORY_INCLUDED
#include "cxx_memory.h"
#endif
#define cache_block 32	//later will use machine model 
typedef mUINT32 INDEX;
typedef mINT16 FIELD_ID;	// from WN.kid_count, it is mINT16
typedef mUINT64 COUNT;
typedef mUINT64 FLD_OFST;
typedef FLD_OFST TY_SIZE;

#ifdef KEY
using namespace std;
#endif // KEY

typedef list<INDEX> TYPE_LIST;	//just contains struct_type, pointer_ty put to another list

struct FLD_ACCESS{
   COUNT count; //first as count, then ty_idx
   FIELD_ID old_field_id; //kid_count in WN is mINT16
   FIELD_ID new_field_id;
   WN_OFFSET offset; //new_offset
#ifdef KEY
   WN_OFFSET old_offset; //old_offset
   mUINT32 align; // field/sub-struct alignment
#endif // KEY
}; //using for storing mappings
struct TOP_FIELD{
   FIELD_ID field_id;
   TY_IDX ty_idx; //0 means scalar type,else STRUCT type
}; //just for top field , used for creating new ordering 
struct CAND_ITEM{
   INDEX  ty_index;
   char ty_name[20]; //just for debug
   struct{
        FIELD_ID flatten_fields;
        FLD_ACCESS* map; //fld_count; //each field's access count
   }fld_info;
   struct{
        FIELD_ID size;
       TOP_FIELD *list;
   }top_field_info;
   union {
       BOOL changed; //using when modifying whirl tree
       BOOL has_enclosed_struct; //using when computing new ordering
   }flag;
}; //accessed through cand_hashtab
struct REORDER_CAND{
   CAND_ITEM* list;
   INDEX size; //number of candidate
};
struct MERGED_ACCESS{
   INDEX  ty_index;
   FIELD_ID flatten_fields;
   COUNT *count;
};
typedef vector< struct MERGED_ACCESS*> MERGED_ACCESS_VECTOR;

extern MERGED_ACCESS_VECTOR *merged_access;
extern MEM_POOL reorder_local_pool;
extern REORDER_CAND reorder_candidate; //identical in size with can_be_reorder_types
class SUMMARY_STRUCT_ACCESS;

extern
void print_merged_access(); 
extern
void Init_merge_access();   
extern 
void Merge_struct_access(SUMMARY_STRUCT_ACCESS *cur_access, mUINT32 index);
extern
void IPA_reorder_legality_process();
extern 
void IPO_Modify_WN_for_field_reorder (IPA_NODE* node); //called by IPO_process_node()
extern
void IPO_get_new_ordering();
extern
void IPO_reorder_Fld_Tab();
extern
void IPO_Finish_reorder();
extern
void Compare_whirl_tree(IPA_NODE* node);


