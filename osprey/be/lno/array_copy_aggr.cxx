/* 
  Copyright (C) 2011 Advanced Micro Devices, Inc.  All Rights Reserved.

  Open64 is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License,
  or (at your option) any later version.

  Open64 is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
  02110-1301, USA.
*/

#include <stdint.h>
#include <math.h>

#include "defs.h"
#include "wn.h"
#include "wn_util.h"
#include "symtab.h"
#include "cxx_memory.h"
#include "lnopt_main.h"
#include "config_opt.h"
#include "config_lno.h"
#include "tracing.h"
#include "array_copy.h"
#include "lnopt_main.h"
#include "ir_reader.h"
#include "targ_sim.h"
#include "opt_du.h"
#include "lwn_util.h"
#include "lnoutils.h"


#define STRUCT_SPLIT_LOOP_DEPTH  5
#define ALIAS_CHECK_INNER_DEPTH  1
#define ALIAS_CHECK_LOOP_DEPTH (STRUCT_SPLIT_LOOP_DEPTH - ALIAS_CHECK_INNER_DEPTH) 

BOOL Trace_Struct_Split = FALSE;

INT32
SAC::check_wn_for_field_refs(ST* &st, WN* base, INT32 ofs)
{
/*
  check if ofs access field
  return 0 if fail, 
  return the field id if success. 
*/
  if (base && TY_kind(ST_type(WN_st(base))) == KIND_POINTER)
  {
    st = WN_st(base);
    TY_IDX orig_ty = TY_pointed(ST_type(st));
    if ( TY_kind(orig_ty) != KIND_STRUCT)
    {
      st = NULL;
      return 0;
    }
    FLD_ITER fld_itr = Make_fld_iter(TY_fld(orig_ty));
    INT32 fld_id=1;
    do
    {
      FLD_HANDLE fld_hl(fld_itr);
      if(ofs == FLD_ofst(fld_hl))
      {
	return fld_id;
      }
      fld_id++;
    }while(!FLD_last_field(fld_itr++));
  }
  st = NULL;
  return 0;
}

WN*
SAC::get_base_wn(WN* wn) 
{
  // return base wn for the following situation, return NULL for other situation. 
  // 1. LDID s 
  // 2.  LDID s
  //     DIMS
  //     INDEX
  //    ARRAY
  // 3.  LDID s
  //       INTCONST sizeof(s)
  //       INDEX
  //      MUL
  //     ADD

  switch (WN_operator(wn))
  {
    case OPR_LDID:
    {
      if( TY_kind(WN_ty(wn)) == KIND_POINTER || TY_kind(WN_ty(wn)) == KIND_ARRAY )
	return wn;
      else 
	return NULL;
    }
    case OPR_ARRAY:
    {
      WN* base = WN_array_base(wn);
      if (WN_operator(base) == OPR_LDID)
	return base;
      else
	return NULL;
    }
    case OPR_ADD:
    {
      WN *kid0, *kid1;
      kid0 = WN_kid0(wn);
      kid1 = WN_kid1(wn);
      if (WN_operator(kid0) == OPR_LDID)
      {
	if (TY_kind(WN_ty(kid0)) != KIND_POINTER)
	  return NULL;
	INT32 size = TY_size(TY_pointed(WN_ty(kid0)));
	if ( !WN_is_multiple_of_size(kid1, size))
	  return NULL; 
	return kid0;
      }
      else if(WN_operator(kid1) == OPR_LDID)
      {
	if (TY_kind(WN_ty(kid1)) != KIND_POINTER)
	  return NULL;
	INT32 size = TY_size(TY_pointed(WN_ty(kid1)));
	if ( ! WN_is_multiple_of_size(kid0, size))
	  return NULL; 
	return kid1;
      }
      else
	return NULL;
    }
    default:
      return NULL;
  }
}

INT32 
SAC::get_base_and_offset(WN* wn, WN* & base)
{
  /*
    pattern base[i] + offset
    
    return the int value of offset if offset is a constant
    use to calculate the access of structure (base) 's field. 
   */
  switch (WN_operator(wn))
  {
    case OPR_CVT:
      return get_base_and_offset(WN_kid0(wn), base);
    case OPR_ARRAY:
      // just ignore the array 
      return get_base_and_offset(WN_array_base(wn), base);
    case OPR_ADD:
    {
      WN *kid0, *kid1;
      INT32 ofs;
      kid0 = WN_kid0(wn);
      kid1 = WN_kid1(wn);
      if ( WN_is_constant( kid1 ) )
      {
	// kid1 may be field offset
	ofs = WN_get_constant_val(kid1);
	base = get_base_wn(kid0);
      }
      else if ( WN_is_constant(kid0) )
      {
	// kid0 may be field offset
	ofs = WN_get_constant_val(kid0);
	base = get_base_wn(kid1);
      }
      else
      {
	// may be zero offset
	ofs = 0;
	base = get_base_wn(wn);
      }
      return ofs;
    }
    default:
    {
      // may be zero offset
      base =  get_base_wn(wn);
      return 0;
    }
  }
}

void 
SAC::get_base_and_offset(WN* wn, WN* &base, WN* &of_wn)
{
  /* 
     match pattern base[i]+offset

     in:  wn
     out1:  base
     out2:  field offset
   */
  switch (WN_operator(wn))
  {
    case OPR_CVT:
    {
      get_base_and_offset(WN_kid0(wn), base, of_wn);
      break;
    }
    case OPR_ARRAY:
    {       
      // just ignore the array 
      get_base_and_offset(WN_array_base(wn), base, of_wn);
      break;
    }
    case OPR_ADD:
    {
      WN *kid0, *kid1, *base1, *base0;
      INT32 ofs;
      kid0 = WN_kid0(wn);
      kid1 = WN_kid1(wn);
      base0 = get_base_wn(kid0);
      base1 = get_base_wn(kid1);
      Is_True(!(base0 && base1), ("base base0 and base1 are pointer or array"));

      if ( base0 )
      {
	base=base0;
	of_wn=kid1;
      }
      else if ( base1 )
      {
	base=base1;
	of_wn=kid0;
      }
      else
	base = NULL;
      break;
    }
    default:
      base = NULL;
  }
}

BOOL 
SAC::get_base_and_offset(WN* wn, WN* &base, WN* &main_offset, WN* &field_offset, WN* &field_arr_ofs)
{
  // the original base, main_offset, and field_offset should be NULL
  // 1. (s+i)->field[j] :  base=s, main_offset=i, field_offset= offset of field, field_arr_ofs = j
  // 2. s->field[j]:       base=s, main_offset=NULL, field_offset = offset of field, field_arr_ofs = j
  // 3. s+i:            base=s, main_offset=i, field_offset=NULL, field_arr_ofs = NULL
  // return TRUE if successful

  switch(WN_operator(wn))
  {
    case OPR_CVT:
      // ignore CVT
      return get_base_and_offset(WN_kid0(wn), base, main_offset, field_offset, field_arr_ofs);
      break;
    case OPR_ARRAY:
      if(base) // already get base, can not deal with this case.
	return FALSE;
      base = get_base_wn(wn); 
      if(!base && WN_num_dim(wn) == 1)
	field_arr_ofs = WN_array_index(wn, 0);
      if (!base && !get_base_and_offset(WN_array_base(wn), base, main_offset, field_offset, field_arr_ofs))
	return FALSE;
      Is_True(base, ("get_base_and_offset, base is NULL"));

      // check form s[i] , we only check 1 dimension
      if( WN_array_base(wn) == base)
      {
	INT32 base_size;
	if ( TY_kind(WN_ty(base)) == KIND_POINTER )
	  base_size = TY_size(TY_pointed(WN_ty(base)));
	else if (TY_kind(WN_ty(base)) == KIND_ARRAY )
	  base_size = TY_size(TY_etype(WN_ty(base)));

	if ( WN_element_size(wn) == base_size)
	{
	  if( WN_num_dim(wn) != 1)
	    return FALSE;  // we do not deal with multiple dimenson
	  main_offset = WN_array_index(wn,0);
	}
      }
      break;
    case OPR_ADD:
      // only do tow level of OPR_ADD
      WN *kid0, *kid1, *base1, *base0;
      INT32 ofs;
      kid0 = WN_kid0(wn);
      kid1 = WN_kid1(wn);
      base = get_base_wn(wn);
      base0 = get_base_wn(kid0);
      base1 = get_base_wn(kid1);
      Is_True(!(base0 && base1), ("both base0 and base1 are pointer or array"));
      
      if(base == kid1 || base == kid0 )
      {
	// find main_offset from kid1 or kid1
	if(TY_kind(WN_ty(base)) != KIND_POINTER) 
	  return FALSE;
	INT32 base_size=TY_size(TY_pointed(WN_ty(base)));
	main_offset = (base == kid1) ? kid0:kid1;
	while(WN_operator(main_offset) == OPR_CVT) main_offset=WN_kid0(main_offset);
	if(WN_operator(main_offset) == OPR_MPY)
	{
	  if(WN_is_constant_val(WN_kid0(main_offset), base_size))
	  {
	    main_offset=WN_kid1(main_offset);
	  }
	  else if( WN_is_constant_val(WN_kid1(main_offset), base_size))
	  {
	    main_offset=WN_kid0(main_offset);
	  }
	  else
	    return FALSE;
	}
	else
	  return FALSE;
      }
      else
      {
	// kid0 or kid1 should be offset
	if (base0)
	{
	  field_offset = kid1;
	  if (!get_base_and_offset(kid0, base, main_offset, field_offset, field_arr_ofs))
	      return FALSE;
	}
	else if (base1)
	{
	  field_offset = kid0;
	  if (!get_base_and_offset(kid1, base, main_offset, field_offset, field_arr_ofs))
	      return FALSE;
	}
	else
	  return FALSE;
      }
      break;
    case OPR_LDID:
      base=get_base_wn(wn);
      if(!base)
	return FALSE;
      if ( !main_offset)
	main_offset = WN_Intconst(MTYPE_I4, 0);
      if ( !field_offset)
	field_offset = WN_Intconst(MTYPE_I4, 0); 
      break;
    default:
      return FALSE;
  }
  return TRUE;
}

BOOL 
SAC::SAC_INFORM::check_ty_recur( TY_IDX ref_ty, TY_IDX check_ty )
{
  /*
    The following situation return true
    1. ref_ty is check_ty
    2. ref_ty is a pointer that pointed to check_ty
    3. ref_ty is an array whose element is check_ty
   */
  Is_True( TY_kind(ref_ty) == KIND_STRUCT, ("ref_ty must be a struct\n") );
  while(TY_kind(check_ty) == KIND_POINTER || TY_kind(check_ty) == KIND_ARRAY)
  {
    if(TY_kind(check_ty) == KIND_POINTER)
      check_ty = TY_pointed(check_ty);
    else
      check_ty = TY_etype(check_ty);
  }
  if ( check_ty == ref_ty )
    return TRUE;
  return FALSE;
}

WN* 
SAC::SAC_INFORM::get_real_field_offset(WN* offset)
{
  /*

   case 1.
     struct S *p;
     offset = (p+i) - p; 
     return i 
   case 2. 
     v = (p+i) - p ; 
     ...
     offset = v;
     return i; 
   */
  WN* new_offset=offset;
  Is_True(offset, ("offset is NULL"));
  while(WN_operator(new_offset) == OPR_CVT)
    new_offset = WN_kid0(new_offset);
  
  WN* wn;
  if(WN_operator(new_offset) == OPR_LDID) 
  {
    // step 1 
    // for case 2 replace v with (p+i) - p 
    DEF_LIST* defs=Du_Mgr->Ud_Get_Def(new_offset);
    if(defs->Incomplete() || defs->Len() != 1) 
      return offset;
    wn = defs->Head()->Wn();
    if(WN_operator(wn)!= OPR_STID) 
      return offset;
    wn = WN_kid0(wn);
    while(WN_operator(wn) == OPR_CVT) 
      wn = WN_kid0(wn);
  }else
    wn = new_offset;
  if(WN_operator(wn) != OPR_SUB)
    return offset;		// fail return the original offset
  if(!MTYPE_is_integral(WN_rtype(wn)))
    return offset;		// fail return the original offset
  WN* kid0=WN_kid0(wn);
  WN* kid1=WN_kid1(wn);
  WN* base0=NULL;
  WN* base1=NULL;
  WN* offset_new=NULL;
  
  // check kid1 , kid1 should be p in (p+i) - p 
  while(WN_operator(kid1) == OPR_CVT)
    kid1 = WN_kid0(kid1);
  base1 = kid1;
  if(WN_operator(kid1) != OPR_LDID)
    return offset;		// fail return the original offset
  // check kid0
  get_base_and_offset(kid0, base0, offset_new);
  if(!base0 || WN_st(base0) != WN_st(base1) || WN_offset(base0) != WN_offset(base1))
    return offset;
  return offset_new;
  
}

void
SAC::collect_splited_field(WN* wn, WN* loop)
{
  /*
    collect structure field for split
    in the inner loop "loop",  check each ILOAD, find the following pattern
    base[offset].field, base is a pointer. 
    for each candidate, create a new SAC::SAC_INFORM and put it to sac_vec. 
   */
  ST* st;
  INT32 fd, ofs;
  SAC::SAC_INFORM* sac_info;
  st = NULL;
  if (WN_operator(wn) == OPR_ILOAD)
  {
    WN* base;
    ofs = get_base_and_offset(WN_kid0(wn), base);
    ofs = ofs?ofs:WN_offset(wn);
    fd = check_wn_for_field_refs(st, base, ofs);
  }
  if(st)
  {
    SAC_INFORM_VECTOR::iterator it;
    for(it=sac_vec.begin(); it != sac_vec.end(); it++)
    {
      if( (*it)->sym == st && (*it)->wn_loop == loop )
	break;
    }
    if ( it !=sac_vec.end())
    {
      sac_info = *it;
      Is_True( sac_info->orig_num_fields >= fd , ("field id must less than max field number "));
    }
    else
    {
      // find a new structure split candidate
      sac_info = CXX_NEW(SAC::SAC_INFORM, &LNO_local_pool);
      memset(sac_info, 0, sizeof(SAC::SAC_INFORM));
      sac_info->wn_loop = loop;
      sac_info->orig_ty = TY_pointed(ST_type(st));
      sac_info->sym = st;
      FLD_ITER fld_itr = Make_fld_iter(TY_fld(sac_info->orig_ty));
      do
      {
	sac_info->orig_num_fields++;
      }while(!FLD_last_field(fld_itr++));
      Is_True( sac_info->orig_num_fields >= fd , ("field id must less or equal to max field number "));
      sac_info->fld_info = CXX_NEW_ARRAY(SAC::SAC_FLD_INFORM, 
                                         sac_info->orig_num_fields+1, 
                                         &LNO_local_pool);
      memset(sac_info->fld_info, 0, ((sac_info->orig_num_fields+1) *
                                     sizeof(SAC::SAC_FLD_INFORM)));
      fld_itr = Make_fld_iter(TY_fld(sac_info->orig_ty));
      sac_vec.push_back(sac_info);
    }
    if(!sac_info->fld_info[fd].is_read)
    {
      sac_info->fld_info[fd].is_read = TRUE;
      Is_True(wn, ("wn should not be null"));
    }
  }
  
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK)
  {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
    {
      collect_splited_field(w, loop);
    }
  }
  else
  {
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++)
    {
      WN* kid = WN_kid(wn, kidno);
      collect_splited_field(kid, loop);
    }
  }
}

void 
SAC::find_struct_split_candidate( WN* wn, WN* out_loop, INT32 depth)
{
/*
  find the struct split candidate in inner loop, put each candidate to sac_vec
  steps.
  1. use function collect_splited_field in each inner loop to collect all struct field access,
     save in vector sv
     
  2. for each structure type in sv, if the marked field meeting the criteria condition ( the marked field 
     accumulated size is less than 30% of the whole structure size) then put in sac_vec. 
 */
  if(!wn) return;
  OPCODE opcode = WN_opcode(wn);

  if (opcode == OPC_DO_LOOP)
  {
    depth++;
    if( depth ==1 )
      out_loop=wn;
    if ( depth >= STRUCT_SPLIT_LOOP_DEPTH ) // only collect depth >= STRUCT_SPLIT_LOOP_DEPTH  candidate
    {
      if(Trace_Struct_Split)
      {
	fprintf(TFile, "FOUND CANDIDATE LOOP\n");
	fdump_wn(TFile, wn);
      }
      collect_splited_field(wn, wn);
      if(sac_vec.size() == 0)
	return;
      SAC_INFORM_VECTOR::iterator it;
      for ( it=sac_vec.begin(); it !=sac_vec.end(); it++ )
      {
	SAC::SAC_INFORM* sac_info=*it;
	int total_access_size=0;
	FLD_ITER fld_itr = Make_fld_iter(TY_fld(sac_info->orig_ty));
	for(int i=0; i < sac_info->orig_num_fields; i++, fld_itr++)
	{
	  int field_id = i+1;
	  if ( sac_info->fld_info[field_id].is_read )
	  {
	    FLD_HANDLE orig_fld(fld_itr);
	    total_access_size += TY_size(FLD_type(orig_fld));
	    if (Trace_Struct_Split)
	    {
	      fprintf(TFile, "Read struct field: number=%d name=%s \n", field_id, FLD_name(orig_fld));
	    }
	  }
	}

	double size_rate=((double)total_access_size)/TY_size(sac_info->orig_ty);
	if (size_rate < 0.5 )
	{
	  //  some criteria condition.
	  sac_info->out_loop = out_loop;
	  SAC_INFORM_VECTOR::iterator vit;
	  for(vit = sac_vec.begin(); vit != sac_vec.end(); vit++)
	  {
	    if( (*vit)->sym == sac_info->sym ) 
	    {
	      for(int i=1; i <= sac_info->orig_num_fields; i++ )
		if ( sac_info->fld_info[i].is_read )
		{
		  (*vit)->fld_info[i].is_read = TRUE;
		}
	      // already select this type and offset
	      break;
	    }
	  }
	  if ( vit == sac_vec.end() )
	    sac_vec.push_back(sac_info);
	  if(Trace_Struct_Split)
	    fprintf(TFile, "Total size of struct size = %d, accesed fields = %d, accessed raid=%f.2\n", (int)TY_size(sac_info->orig_ty),
		    total_access_size, size_rate);
	}
      }
      return;
    } 
  }
  else if (opcode == OPC_WHILE_DO || opcode == OPC_DO_WHILE)
  {
    depth++;
    if( depth ==1 )
      out_loop=wn;
  }
    
  if (opcode == OPC_BLOCK)
  {
    for(WN* w = WN_first(wn); w; w = WN_next(w))
    {
      find_struct_split_candidate(w, out_loop, depth);
    }
  }
  else
  {
    for(INT kidno = 0; kidno < WN_kid_count(wn); kidno++)
    {
      WN* kid=WN_kid(wn, kidno);
      find_struct_split_candidate(kid, out_loop, depth);
    }
  }
}

BOOL
SAC::SAC_INFORM::check_legality_and_collect_node(WN* wn, INT32 depth)
{
/* 
   give the candidate . check if it is legal for transformation,  and collect nodes to be splited. 

*/
  TY_IDX ty_idx = orig_ty;
  if(!wn) 
    return FALSE;
  Is_True( (TY_kind(ty_idx) == KIND_STRUCT) , ("must be struct type") );
  
  switch(WN_operator(wn))
  {
    case OPR_BLOCK:
    {
      for( WN* w = WN_first(wn); w; w = WN_next(w))
      {
	if (!check_legality_and_collect_node(w, depth))
	  return FALSE;
      }
      break;
    }
    case OPR_PARM:
    case OPR_ILOAD:
    case OPR_STID:
    {
      WN*kid0 = WN_kid0(wn);
      WN *base=NULL;
      WN *main_offset=NULL;
      WN *field_offset=NULL;
      WN *field_arr_ofs=NULL;
      INT32 ofs;
      
      if( get_base_and_offset(kid0, base, main_offset, field_offset, field_arr_ofs)
	  && TY_kind(ST_type(WN_st(base))) == KIND_POINTER
	  && TY_pointed(ST_type(WN_st(base))) == ty_idx )
      {
	if(field_offset && !WN_is_constant(field_offset))
	{
	  // for case: base[i] + v    while v=(base.field-base)
	  field_offset = get_real_field_offset(field_offset);
	}
	
	if(field_offset && !WN_is_constant(field_offset))
	  // we get 
	{
	  if (WN_operator(wn) == OPR_ILOAD )
	  {
	    INT32 offs = WN_offset(wn);
	    FLD_ITER fld_it = Make_fld_iter(TY_fld(ty_idx));
	    int i = 1;
	    do
	    {
	      if(offs < (*fld_it).ofst + TY_size((*fld_it).type))
	      {
		/*
		  case 1. Iload the offset through, set the offset within iload operator
		  check for case :
 		   LDID 0 <base> T<type>
		  Iload offset  T1<type> T2<type> 
		*/

		fld_info[i].is_read = FALSE;
		return TRUE;
		break;
	      }
	      i++;
	    }while(!FLD_last_field(fld_it++));
	  }
	  // foo(s + i) or x= *(s+i), variable offset
	  if (Trace_Struct_Split)
	  {
	    if (WN_operator(wn) == OPR_PARM)
	      fprintf(TFile, "....found parm, variable offset\n");
	    else if (WN_operator(wn) == OPR_ILOAD)
	      fprintf(TFile, "....found Iload, variable offset\n");
	    else
	      fprintf(TFile, "....found STID, variable offset\n");
	    fdump_tree(TFile, wn);
	  }
	  return FALSE;
	}
	else
	{
	  ST *st;
	  INT32 ofs = field_offset ? WN_get_constant_val(field_offset):0;
	  if(WN_operator(wn) == OPR_ILOAD && !ofs)
	    ofs = WN_offset(wn);

	  INT32 fld_id;
	  fld_id=SAC::check_wn_for_field_refs(st, base, ofs);

	  FLD_ITER fld_it=Make_fld_iter(TY_fld(ty_idx));
	  int i = 1;
	  do
	  {
	    if(i == fld_id)
	    {
	      FLD_HANDLE fld_hl(fld_it);
	      if(Trace_Struct_Split)
	      {
		if (WN_operator(wn) == OPR_PARM)
		  fprintf(TFile, "....found parm pass to field %s\n", FLD_name(fld_hl));
		else
		  fprintf(TFile, "....found access to field %s\n", FLD_name(fld_hl));
		fdump_tree(TFile, wn);
	      }
	      if (WN_operator(wn) == OPR_PARM)
		return FALSE;
	      else
	      {
		/* 
		   case 2.  Iload the offset through using adding the offset value, mark it
		     LDID base
		     LDID offset
		    ADD
		   ILOAD 
		   
		   case 3. store the address of Iload to a variable. mark it
		   this is an aggresive optimization. 
			     
		     LDID base
		     LDID offset
		    ADD
		   STID

		*/ 
		if(fld_info[i].is_read)
		{
		  if(WN_operator(wn) == OPR_STID)
		  {
		    // check legality of address taken
		    USE_LIST* uses = Du_Mgr->Du_Get_Use(wn);
		    if(uses->Incomplete()) 
		    {
		      fld_info[i].cands.clear();
		      fld_info[i].ac_cands.clear();
		      fld_info[i].is_read = FALSE;
		      if(Trace_Struct_Split)
		      {
			fprintf(stderr, "checking STID, uses incomplete\n");
			fdump_tree(stderr, wn);
		      }
		    }
		    else 
		    {
		      USE_LIST_ITER iter(uses);
		      for(DU_NODE* du = iter.First(); !iter.Is_Empty(); du = iter.Next())
		      {
			DEF_LIST* defs=Du_Mgr->Ud_Get_Def(du->Wn());
			if(defs->Incomplete() || defs->Len() != 1)
			{
			  fld_info[i].cands.clear();
			  fld_info[i].ac_cands.clear();
			  fld_info[i].is_read = FALSE;
			  if(Trace_Struct_Split)
			  {
			    fprintf(stderr, "checking STID, def incomplete or not equal to 1\n");
			    fdump_tree(stderr, du->Wn());
			  }
			  break;
			}
			if(defs->Head()->Wn() != wn)
			{
			  fld_info[i].cands.clear();
			  fld_info[i].ac_cands.clear();
			  fld_info[i].is_read = FALSE;
			  if(Trace_Struct_Split)
			  {
			    fprintf(stderr, "checking STID, use's def is not wn\n");
			    fdump_tree(stderr, du->Wn());
			  }
			  break;
			}
			// check that the address do not assign to other variable. 
			// we expect  p =  &(q->field)
			//            x = *p 

			WN* parent = LWN_Get_Parent(du->Wn());
			while(parent && WN_operator(parent) != OPR_BLOCK && WN_operator(parent) != OPR_ILOAD)
			  parent = LWN_Get_Parent(parent);
			if(!parent || WN_operator(parent) != OPR_ILOAD)
			{			  
			  fld_info[i].cands.clear();
			  fld_info[i].ac_cands.clear();
			  fld_info[i].is_read = FALSE;
			  if(Trace_Struct_Split)
			  {
			    fprintf(stderr, "checking STID, one of the uses is not for ILOAD\n");
			    fdump_tree(stderr, du->Wn());
			  }
			  break;
			}
		      }
		    }
		  }
		  if(Trace_Struct_Split)
		  {
		    if(WN_operator(wn) == OPR_ILOAD)
		      fprintf(TFile, "[verify] verify OK for case ILOAD\n");
		    else
		      fprintf(TFile, "[verify] verify OK for case STD\n");

		  }
		  fld_info[i].cands.push_back(base);
		  fld_info[i].ac_cands.push_back(wn);
		  if( WN_operator(wn) == OPR_ILOAD
		      && TY_kind((*fld_it).type) == KIND_ARRAY 
		      && fld_info[i].inner_depth < depth
		    )
		  {
		    while( field_arr_ofs && WN_operator(field_arr_ofs) == OPR_CVT )
		      field_arr_ofs = WN_kid0(field_arr_ofs);
		    fld_info[i].inner_depth = depth;
		    fld_info[i].inner_field_arr_ofs = field_arr_ofs;
		  }
		}
	      }
	      break;
	    }
	    i++;
	  }while(!FLD_last_field(fld_it++));
	  if (i!=fld_id)
	  {
	    if (Trace_Struct_Split)
	    {
	      fprintf(TFile, "----found access but not found field\n");
	      fdump_tree(TFile, wn);
	    }
	    return FALSE;
	  }
	}
	break;
      }
      else if(!check_legality_and_collect_node(WN_kid0(wn), depth))
	return FALSE;

      break;
    }
    case OPR_LDID:
    {
      
      if ( check_ty_recur( ty_idx, ST_type(WN_st(wn))) )
      {
	// can no handle this struct access pattern
	if (Trace_Struct_Split)
	{
	  fprintf(TFile, "----check_legality_and_collect_node, can not handle\n");
	  WN_st(wn)->Print(TFile);
	}
	return FALSE;
      }
      break;
    }
    case OPR_SUB:
    {
      /*
	find candidate for address substract, this expression is a int constant 
	of struct field offset value. 
	case 4.
	  LDID base
	  LDID offset  (constant)
	 ADD
	 LDID base
        SUB
       */
      WN* kid0, *kid1, *base0, *base1;
      kid0 = WN_kid0(wn);
      kid1 = WN_kid1(wn);
      base0=base1=NULL;

      if (MTYPE_is_integral(WN_rtype(wn)) ) 
      {
	INT32 offset0, offset1;
	offset0 = get_base_and_offset(kid0, base0);
	if(base0)
	{
	  offset1 = get_base_and_offset(kid1, base1);
	}
	if(base0 && WN_st(base0) == WN_st(base1))
	{
	  if (Trace_Struct_Split)
	  {
	    fprintf(TFile, "....found offset expression\n");
	    fdump_tree(TFile, wn);
	  }
	  return TRUE;
	}
      }
      if (!check_legality_and_collect_node(kid0, depth) || !check_legality_and_collect_node(kid1, depth))
	return FALSE;
      break;
    }
    case OPR_ISTORE:
    {
      WN *kid0, *kid1;
      kid0 = WN_kid0(wn);
      kid1 = WN_kid1(wn);
      WN *base=NULL;
      WN *main_offset=NULL;
      WN *field_offset=NULL;
      WN *field_arr_ofs=NULL;
      INT32 ofs;

      // check if store struct field address to other variable
      if(get_base_and_offset(kid0, base, main_offset, field_offset, field_arr_ofs) 
	 && TY_kind(ST_type(WN_st(base))) == KIND_POINTER
	 && TY_pointed(ST_type(WN_st(base))) == ty_idx)
      {
	if(field_offset && !WN_is_constant(field_offset))
	  field_offset = get_real_field_offset(field_offset);
	if(field_offset && !WN_is_constant(field_offset))
	{
	  if (Trace_Struct_Split)
	  {
	    fprintf(TFile, "....found ref, variable offset\n");
	    fdump_tree(TFile, wn);
	  }
	  return FALSE;
	}
	else
	{
	  ST *st;
	  INT32 ofs = field_offset ? WN_get_constant_val(field_offset) : 0;
	  INT32 fld_id;
	  fld_id = SAC::check_wn_for_field_refs(st, base, ofs);

	  FLD_ITER fld_it=Make_fld_iter(TY_fld(ty_idx));
	  int i = 1;
	  do
	  {
	    if(i == fld_id)
	    {
	      FLD_HANDLE fld_hl(fld_it);
	      if (Trace_Struct_Split)
	      {
		fprintf(TFile, "....found ref to field %s\n", FLD_name(fld_hl));
		fdump_tree(TFile, wn);
	      }
	      if(fld_info[i].is_read)
	      {
	      /*
		case 5. store the structure field address to other place
		        the field can be determin, marked it

		  LDID base
		  LDID offset
		 ADD 
		 LDID other
		ISTORE 0  T1<type> T2<type> 
	      */
		
		if(Trace_Struct_Split)
		{
		  fprintf(TFile, "[verify] verify OK for case ISTORE ref\n");
		}
		fld_info[i].cands.push_back(base);
		fld_info[i].ac_cands.push_back(wn);
	      }
	      break;
	    }
	    i++;
	  }while(!FLD_last_field(fld_it++));
	  if (i!=fld_id)
	  {
	    if (Trace_Struct_Split)
	    {
	      fprintf(TFile, "----found ref but not found filed\n");
	      fdump_tree(TFile, wn);
	    }
	    return FALSE;
	  }
	}
	break;
      }
      else if(!check_legality_and_collect_node(kid0, depth))
	return FALSE;

      // check if store value to struct field addrees
      base=main_offset=field_offset=NULL;
      if(get_base_and_offset(kid1, base, main_offset, field_offset, field_arr_ofs)
	 && TY_kind(ST_type(WN_st(base))) == KIND_POINTER
	 && TY_pointed(ST_type(WN_st(base))) == ty_idx)
      {
	if(field_offset && !WN_is_constant(field_offset))
	  field_offset = get_real_field_offset(field_offset);
	if(field_offset && !WN_is_constant(field_offset))
	{
	  INT32 offs = WN_offset(wn);
	  FLD_ITER fld_it = Make_fld_iter(TY_fld(ty_idx));
	  int i = 1;
	  do
	  {
	    if(offs < (*fld_it).ofst + TY_size((*fld_it).type))
	    {
	      /*
		case 6. reference structure field address, offset is set within istore operator
		        the field can be determin, but do not transform in this situation. 
		 LDID other
		 LDID base
		ISTORE offset  T1<type> T2<type> 
	      */
	      fld_info[i].is_read = FALSE;
	      return TRUE;
	    }
	    i++;
	  }while(!FLD_last_field(fld_it++));

	  if (Trace_Struct_Split)
	  {
	    fprintf(TFile, "....found def, variable offset\n");
	    fdump_tree(TFile, wn);
	  }
	  return FALSE;
	}
	else
	{
	  ST *st;
	  INT32 ofs = field_offset ? WN_get_constant_val(field_offset):0;
	  INT32 fld_id;
	  fld_id=SAC::check_wn_for_field_refs(st, base, ofs);

	  FLD_ITER fld_it=Make_fld_iter(TY_fld(ty_idx));
	  int i = 1;
	  do
	  {
	    if(i == fld_id)
	    {
	      if (Trace_Struct_Split)
	      {
		FLD_HANDLE fld_hl(fld_it);
		fprintf(TFile, "....found def to field %s\n", FLD_name(fld_hl));
		fdump_tree(TFile, wn);
	      }

	      if (fld_info[i].is_read)
	      {
		/*
		  case 6. store to the structure field, base plus field offset pattern
		  the field can be determin, marked it

		 LDID other
		  LDID base
 		  LDID offset
 		 ADD 
 		ISTORE 0  T1<type> T2<type> 
		*/
		if(Trace_Struct_Split)
		{
		  fprintf(TFile, "[verify] verify OK for case ISTORE def\n");
		}
		fld_info[i].cands.push_back(base);
		fld_info[i].ac_cands.push_back(wn);
	      }
	      break;
	    }
	    i++;
	  }while(!FLD_last_field(fld_it++));
	  if (i!=fld_id)
	  {
	    if (Trace_Struct_Split)
	    {
	      fprintf(TFile, "----found def but not found filed\n");
	      fdump_tree(TFile, wn);
	    }
	    return FALSE;
	  }
	}
	break;
      }
      else if(!check_legality_and_collect_node(kid1, depth))
	return FALSE;
      break;
    }
    case OPR_DO_LOOP:
    case OPR_WHILE_DO:
    case OPR_DO_WHILE:
      depth++;
      for( INT kidno = 0; kidno < WN_kid_count(wn); kidno++)
      {
	WN* kid = WN_kid(wn, kidno);
	if(!check_legality_and_collect_node(kid, depth))
	  return FALSE;
      }
      break;
    default:
      for( INT kidno = 0; kidno < WN_kid_count(wn); kidno++)
      {
	WN* kid = WN_kid(wn, kidno);
	if(!check_legality_and_collect_node(kid, depth))
	  return FALSE;
      }
  }
  return TRUE;
}
  

void
SAC::SAC_INFORM::replace_splited_field(WN* base, ST* new_st, WN* new_st_def_wn)
{
/*
  replace base[ofs].field with base_field[ofs]
 */
  Is_True( WN_operator(base) == OPR_LDID, 
	   ("base should be an ldid"));
  TY_IDX base_ty = WN_type(base);
  WN* parent= LWN_Get_Parent(base); // base[i]
  WN* grapa = LWN_Get_Parent(parent); // base[i].field
  
  Is_True(WN_operator(grapa) == OPR_ADD || WN_operator(grapa) == OPR_ILOAD, ("field access operator must be add"));

  if (Trace_Struct_Split)
  {
    fprintf(TFile, "before:\n");
    fdump_tree(TFile, grapa);
  }
 
  WN* new_base  = LWN_CreateLdid(OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type), new_st_def_wn);
  Du_Mgr->Add_Def_Use(new_st_def_wn, new_base);
  
  /*
    for base[ofs].field, get ofs. 
   */
  if ( WN_operator(parent) == OPR_ARRAY )
  {

    // case 1.  base[ofs] + field_offset
      
    WN* ofs = WN_array_index(parent, 0);
    WN_array_index(parent,0) = NULL;
    ofs = WN_CreateExp2(OPR_MPY, WN_rtype(ofs), MTYPE_V, ofs, 
			  LWN_Make_Icon(WN_rtype(ofs), TY_size(TY_pointed(ST_type(new_st)))));
    if ( WN_operator(grapa) == OPR_ADD )
    {
      LWN_Delete_Tree(WN_kid0(grapa));
      LWN_Delete_Tree(WN_kid1(grapa));
      WN_kid0(grapa) = new_base;
      WN_kid1(grapa) = ofs;
    }
    else if ( WN_operator(grapa) == OPR_ILOAD )
    {
      WN* add_wn = WN_CreateExp2(OPR_ADD, Pointer_type, MTYPE_V, new_base, ofs);
      LWN_Delete_Tree(WN_kid0(grapa));
      WN_kid0(grapa) = add_wn;
      WN_offset(grapa) = 0;
      WN_set_field_id(grapa, 0);
      WN_set_load_addr_ty(grapa, ST_type(new_st));
      WN_set_ty(grapa, TY_pointed(ST_type(new_st)));
    }
    LWN_Parentize(grapa);
  }
  else if (WN_operator(parent) == OPR_ADD)
  {
    // case 2. (base + (size_of_base * ofs) ) + field_offset
    WN *ofs;
    if(WN_kid0(parent) == base)
    {
      ofs = WN_kid1(parent);
      WN_kid0(parent) = new_base;
    }
    else if(WN_kid1(parent) == base)
    {
      ofs = WN_kid0(parent);
      WN_kid1(parent) = new_base;
    }
    else 
      Fail_FmtAssertion("one of parent's kid should be base\n");
    LWN_Set_Parent(new_base, parent);
    LWN_Delete_Tree(base);

    while( WN_operator(ofs) == OPR_CVT)
      ofs = WN_kid0(ofs);
    Is_True( WN_operator(ofs) == OPR_MPY , ("operator should be mpy"));

    WN * con;
    if(WN_is_constant_val(WN_kid0(ofs), TY_size(TY_pointed(base_ty))))
      con=WN_kid0(ofs);
    else if(WN_is_constant_val(WN_kid1(ofs), TY_size(TY_pointed(base_ty))))
      con=WN_kid1(ofs);

    while(WN_operator(con) == OPR_CVT)
      con = WN_kid0(con);
    WN_const_val(con) = TY_size(TY_pointed(ST_type(new_st)));
    
    WN * new_grapa  = parent;
    // get parent out of ancestor
    if(WN_kid0(grapa) == parent)
      WN_kid0(grapa) = NULL;
    else if(WN_kid1(grapa) == parent)
      WN_kid1(grapa) = NULL;
    else
      Is_True(FALSE, ("nother kid0 or kid1 of grapa is parent"));

    if ( WN_operator(grapa) == OPR_ADD)
    {
      WN * ancestor = LWN_Get_Parent(grapa);
      for ( int kidn = 0; kidn < WN_kid_count(ancestor); kidn++)
      {
	if( WN_kid(ancestor, kidn) == grapa)
	{
	  LWN_Delete_Tree(grapa);
	  WN_kid(ancestor, kidn) = new_grapa;
	  LWN_Parentize(ancestor);
	  grapa = new_grapa;
	  break;
	}
      }
    }
    else if ( WN_operator(grapa) == OPR_ILOAD )
    {
      LWN_Delete_Tree(WN_kid0(grapa));
      WN_kid0(grapa) = new_grapa;
      WN_offset(grapa) = 0;
      WN_set_field_id(grapa, 0);
      WN_set_load_addr_ty(grapa, ST_type(new_st));
      WN_set_ty(grapa, TY_pointed(ST_type(new_st)));
      LWN_Parentize(grapa);
    }
    else
      Fail_FmtAssertion("grapa should be add or iload with offset\n");
  }
  else
    Fail_FmtAssertion("parent should be array or add\n");
  if (Trace_Struct_Split)
  {
    fprintf(TFile, "after:\n");
    fdump_tree(TFile, grapa);
  }
}

void
SAC::SAC_INFORM::replace_splited_field(WN* base, ST* new_st, WN* size_wn, WN* new_st_def_wn)
{
/* 
   replace base[ofs].field[ofs2] with  base_field[(ofs2*size + ofs)]
 */
  Is_True( WN_operator(base) == OPR_LDID, 
	   ("base should be an ldid"));

  TY_IDX base_ty = WN_type(base);
  WN* parent= LWN_Get_Parent(base); // base[ofs]
  WN* grapa = LWN_Get_Parent(parent);  // base[ofs].field
  WN* ancestor = LWN_Get_Parent(grapa); // base[ofs].field[ofs2]

  Is_True(WN_operator(grapa) == OPR_ADD , ("field access operator must be add"));

  // 1. get field offset, offset is the splited field offset in base[i]
  WN* offset;
  if(WN_kid0(grapa) == parent)
    offset = WN_kid1(grapa); 
  else
    offset = WN_kid0(grapa);
  while( WN_operator(offset) == OPR_CVT )
    offset = WN_kid0(offset);
  WN* wn;
  if(WN_operator(offset) == OPR_LDID)
  {
    /* 
       case 1. (base[ofs] + v)   while v=(base+offset)-base
       
       transform to  base[ofs] + ((base+offset)-base)
     */
    DEF_LIST* defs=Du_Mgr->Ud_Get_Def(offset);
    Is_True(!defs->Incomplete() && defs->Len() == 1, ("defs is incomplete or len==1"));
    wn = defs->Head()->Wn();
    Is_True( WN_operator(wn)== OPR_STID, ("operator is not OPR_STID"));
    Du_Mgr->Delete_Def_Use(wn, offset);
    
    wn = WN_kid0(wn);
    while(WN_operator(wn) == OPR_CVT) 
      wn = WN_kid0(wn);
    
    // replace offset with it's defs
    WN* wn_copy = LWN_Copy_Tree(wn);
    LWN_Copy_Def_Use(wn, wn_copy, Du_Mgr);
    if(WN_kid0(grapa) == parent)
      WN_kid1(grapa)=wn_copy; 
    else
      WN_kid0(grapa)=wn_copy;
    LWN_Update_Def_Use_Delete_Tree(offset);
    LWN_Delete_Tree(offset);
    LWN_Parentize(grapa);
    offset=wn_copy;
  }
  
  if(WN_operator(offset) == OPR_SUB)
  {
    /*
      case 2
      replace  base[ofs] + (base+offset - base) 
      with     base[ofs] + offset
     */
    WN* base0, *tmpwn, *new_grapa;
    WN* base1=WN_kid1(offset);
    while(WN_operator(base1) == OPR_CVT)
      base1 = WN_kid0(base1);
    Is_True(WN_operator(base1) == OPR_LDID, ("base1 is not LDID"));
    Is_True(WN_st(base1) == WN_st(base), ("base1 and base is difference"));
    new_grapa=WN_kid0(offset);
    tmpwn=offset;
    while(WN_operator(new_grapa) == OPR_CVT)
    {
      tmpwn = new_grapa;
      new_grapa = WN_kid0(new_grapa);
    }
    WN_kid0(tmpwn) = NULL;
    LWN_Delete_Tree(offset);
    tmpwn=NULL;
    get_base_and_offset(new_grapa, base0, tmpwn);
    Is_True(base0 && WN_st(base0)== WN_st(base), ("base0 and base is difference"));
    
    WN* parent0=LWN_Get_Parent(base0);
    Is_True(WN_operator(parent0) == OPR_ADD, ("parent0's operator is not add"));
    if(WN_kid0(parent0) == base0)
    {
      LWN_Delete_Tree(base0); 
      WN_kid0(parent0) = parent; 
    }
    else if(WN_kid1(parent0) == base0)
    {
      LWN_Delete_Tree(base0); 
      WN_kid1(parent0) = parent; 
    }
    else
      Is_True(0, ("one of parent0's kid should be base0 "));
    for( int kidn=0; kidn < WN_kid_count(ancestor); kidn++)
      if(WN_kid(ancestor, kidn) == grapa)
      {
	WN_kid(ancestor, kidn) = new_grapa;
	WN_kid0(grapa) = NULL;
	WN_kid1(grapa) = NULL;
	LWN_Delete_Tree(grapa);
	break;
      }
    LWN_Parentize(ancestor);
    grapa = LWN_Get_Parent(parent);
    ancestor = LWN_Get_Parent(grapa);
  }

  
  WN* ancestor_parent = LWN_Get_Parent(ancestor);

  Is_True(WN_operator(ancestor) == OPR_ARRAY, ("ancestor should be array type"));

  if (Trace_Struct_Split)
  {
    fprintf(TFile, "replace_splited_field before:\n");
    fdump_tree(TFile, ancestor_parent);
  }
  
  WN* new_base  = LWN_CreateLdid(OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type), new_st_def_wn);
  Du_Mgr->Add_Def_Use(new_st_def_wn, new_base);
  WN* ofs;   // base[ofs].field[ofs2], the ofs
  WN* ofs2;  // base[ofs].field[ofs2], the ofs2
  
  /* 
     for base[ofs].field[ofs2], the following get ofs and translate. 
     
   */ 
  if ( WN_operator(parent) == OPR_ARRAY )
  {
    ofs = WN_array_index(parent,0);
    WN_array_index(parent,0) = NULL;
  }
  else if (WN_operator(parent) == OPR_ADD)
  {
    if(WN_kid0(parent) == base)
    {
      ofs = WN_kid1(parent);
      WN_kid1(parent) = NULL;
    }
    else if(WN_kid1(parent) == base)
    {
      ofs = WN_kid0(parent);
      WN_kid0(parent) = NULL;
    }
    else 
      Fail_FmtAssertion("one of parent's kid should be base\n");
    while( WN_operator(ofs) == OPR_CVT)
    {
      WN * ofs_parent;
      ofs_parent=ofs;
      ofs = WN_kid0(ofs);
      WN_kid0(ofs_parent) = NULL;
      WN_Delete(ofs_parent);
    }
    Is_True( WN_operator(ofs) == OPR_MPY , ("operator should be mpy"));

    if(WN_is_constant_val(WN_kid0(ofs), TY_size(TY_pointed(base_ty))))
    {
      WN * tmp_wn = ofs;
      ofs = WN_kid1(tmp_wn);
      WN_kid1(tmp_wn)=NULL;
      WN_DELETE_Tree(tmp_wn);
    }
    else if(WN_is_constant_val(WN_kid1(ofs), TY_size(TY_pointed(base_ty))))
    {
      WN * tmp_wn = ofs;
      ofs = WN_kid0(tmp_wn);
      WN_kid0(tmp_wn)=NULL;
      WN_DELETE_Tree(tmp_wn);
    }
    
  }
  else
    Fail_FmtAssertion("parent should be array or add\n");
  WN* calloc_size = LWN_Copy_Tree(size_wn); // 
  LWN_Copy_Def_Use(size_wn, calloc_size, Du_Mgr);

  /*
    for base[ofs].field[ofs2] get ofs2 and translate. 
  */
  ofs2 = LWN_CreateExp2(OPC_U8MPY,
			WN_array_index(ancestor, 0),
			calloc_size);
  ofs = LWN_CreateExp2(OPC_U8ADD, ofs2, ofs);
  
  ofs = LWN_CreateExp2(OPC_U8MPY, WN_Intconst(Pointer_Mtype, TY_size(TY_pointed(ST_type(new_st)))), ofs);
  
  WN_array_index(ancestor,0) = NULL;
  
  WN* new_ancestor = LWN_CreateExp2(OPC_U8ADD, new_base, ofs);
  
  for ( int kidn = 0; kidn < WN_kid_count(ancestor_parent); kidn++)
  {
    if( WN_kid(ancestor_parent, kidn) == ancestor)
    {
      LWN_Delete_Tree(ancestor);
      WN_kid(ancestor_parent, kidn) = new_ancestor;
      LWN_Parentize(ancestor_parent);
      break;
    }
  }
  if (Trace_Struct_Split)
  {
    fprintf(TFile, "replace_splited_field after:\n");
    fdump_tree(TFile, ancestor_parent);
  }
}

BOOL
SAC::SAC_INFORM::split_array_element(INT32 i)
{
/*
  funtion to determin if split the hold array of the array element 
  For example:
  struct S{
     int a;
     ...
     S1 field[0..3];
     ...
     int z;
  }
  I'm going to split  S1 field[4], this function determin if I should 
  split field[0..3] as on new struct or split field[0], field[1], field[2], field[3]
  to seprate struct.  if split to seperate structure, return TRUE. 
  
  TODO: more condition add
*/
  if(!fld_info[i].is_read) 
    return FALSE;
  FLD_ITER fld_it=Make_fld_iter(TY_fld(orig_ty));
  for(int j=1; j < i ; j++)
    fld_it++;
  if(TY_kind((*fld_it).type) == KIND_ARRAY
     && ARB_dimension(TY_arb((*fld_it).type)) == 1 
     && ARB_const_lbnd(TY_arb((*fld_it).type))
     && ARB_const_ubnd(TY_arb((*fld_it).type))
     && ARB_const_stride(TY_arb((*fld_it).type))
    )
  {
    WN* wn = fld_info[i].inner_field_arr_ofs;
    if ( wn && WN_operator(wn) == OPR_LDID )
    {
      DEF_LIST* defs = Du_Mgr->Ud_Get_Def(wn);
      if( defs->Incomplete() || defs->Len() != 2)
	return TRUE;
      WN* def1 = defs->Head()->Wn();
      WN* do_loop1 = LWN_Get_Parent(def1);
      while(do_loop1 && WN_operator(do_loop1) != OPR_DO_LOOP)
	do_loop1 = LWN_Get_Parent(do_loop1);
      WN* do_loop2 = LWN_Get_Parent(wn);
      while(do_loop2 && WN_operator(do_loop2) != OPR_DO_LOOP)
	do_loop2 = LWN_Get_Parent(do_loop2);
      if ( do_loop1 == do_loop2 )
      {
	// wn is loop induction in the inner most loop, 
	// do not split the array
	return FALSE;
      }
    }
    return TRUE;
  }
  return FALSE;
}

BOOL
SAC::SAC_INFORM::select_alias_check(WN* wn, BOOL check_in_loop )
{
/*
   this fuction check wn, if we can add alias check for it, put it into 
   the alias check vector acs and return TRUE. If can not deal with the case
   return false. 
*/
  WN *base, *offset;
  SAC::ALIAS_CHECK ac;
  if( WN_operator(wn) == OPR_LDA )
    return TRUE; // ignore fixed address
  else if(WN_operator(wn) == OPR_LDID)
  {    
    ac.alias_wn = wn;
    ac.check_in_loop = FALSE;
    ALIAS_CHECK_VECTOR::iterator aciter;
    for( aciter = acs.begin(); aciter != acs.end(); aciter++ )
    {
      if(Tree_Equiv((*aciter).alias_wn, ac.alias_wn)
	 && !(*aciter).check_in_loop  )
	break;
    }
    if(aciter == acs.end())
      acs.push_back(ac);
    return TRUE;

  }
  if( WN_operator(wn) != OPR_ILOAD )
    return FALSE;
  if(WN_operator(WN_kid0(wn)) == OPR_ADD )
  {
    /*
      for case  parent(wn) of 
        lda 
       Iload
      Iload
     stid 
      we should check inside the loop
     */
    WN * wn_add = WN_kid0(wn);
    if( WN_operator(WN_kid0(wn_add)) != OPR_ILOAD && 
	 WN_operator(WN_kid1(wn_add)) != OPR_ILOAD )
      return FALSE;

    WN *parent= LWN_Get_Parent(wn);
    WN *ancestor;
    if(parent)
      ancestor = LWN_Get_Parent(parent);
    if( check_in_loop && ( WN_operator(parent ) == OPR_STID || WN_operator(parent ) == OPR_ILOAD ))
    {
      ac.alias_wn = wn;
      ac.check_in_loop = TRUE;

      ALIAS_CHECK_VECTOR::iterator aciter;
      for( aciter = acs.begin(); aciter != acs.end(); aciter++ )
      {
	if ((*aciter).alias_wn == ac.alias_wn)
	  break;
      }
      if(aciter == acs.end())
	acs.push_back(ac);
      return TRUE;
    }
    return FALSE;

  }else if( WN_operator(WN_kid0(wn)) == OPR_ARRAY )
  {
    /*
      for case wn of 
         lda
	Iload

      can check ouside the loop
     */
    WN* wn0=WN_kid0(wn);
    if(WN_operator(WN_kid0(wn0)) != OPR_LDA )
      return FALSE;
    if(WN_num_dim(wn0) != 1 )
      return FALSE;
    if( !WN_is_constant(WN_kid2(wn0)) )
      return FALSE;
    ac.alias_wn = wn;
    ac.check_in_loop = FALSE;
    ALIAS_CHECK_VECTOR::iterator aciter;
    for( aciter = acs.begin(); aciter != acs.end(); aciter++ )
    {
      if(Tree_Equiv((*aciter).alias_wn, ac.alias_wn)
	 && !(*aciter).check_in_loop )
	break;
    }
    if(aciter == acs.end())
      acs.push_back(ac);
    return TRUE;
  }
  return FALSE;
}

BOOL
SAC::SAC_INFORM::def_by_calloc_or_free(WN* wn)
{
/*
  check_if wn is defined by calloc or free. 
*/
  if( !wn)
    return FALSE;
  // check free
  if(WN_operator(wn) == OPR_CALL && strcmp(ST_name(WN_st(wn)), "free") == 0 )
    return TRUE;
  if( !(WN_operator(wn) == OPR_STID))
    return FALSE;
  WN* wn0 = WN_kid0(wn);
  if( !(WN_operator(wn0) == OPR_LDID ))
    return FALSE;
  if( !(ST_class(WN_st(wn0)) == CLASS_PREG && Is_Return_Preg(WN_load_offset(wn0))) 
      && !( WN_st(wn0) == Return_Val_Preg ) )
    return FALSE;
  WN* prev=WN_prev(wn);
  if( prev && WN_operator(prev) == OPR_CALL 
      && strcmp(ST_name(WN_st(prev)), "calloc") == 0 )
    return TRUE;
  return FALSE;
}
INT32
SAC::SAC_INFORM::loop_distant(WN* outer, WN* inner)
/*
    return the do loop level between outer and inner.
 */
{
  INT32 d = 0;
  
  while(inner && inner != outer)
  {
    inner = LWN_Get_Parent(inner);
    if(inner && WN_operator(inner) == OPR_DO_LOOP)
      d++;
  }
  if(!inner) d = 0 ;
  return d; 
}

INT32
SAC::SAC_INFORM::loop_depth(WN* wn)
/*
  return the do loop number from the innermost loop to the outer most loop
 */
{
  int depth=0, max_depth=0;
  if(WN_operator(wn) == OPR_BLOCK)
  {
    for(WN* w = WN_first(wn); w; w = WN_next(w))
    {
      depth = loop_depth(w);
      if(depth > max_depth)
	max_depth = depth;
    }
  }
  else
  {
    for(INT kidno=0; kidno<WN_kid_count(wn); kidno++)
    {
      depth = loop_depth(WN_kid(wn, kidno));
      if(depth > max_depth)
	max_depth = depth;
    }
  }
  if(WN_operator(wn) == OPR_DO_LOOP)
    max_depth++;
  return max_depth;
}

BOOL
SAC::SAC_INFORM::check_base_st(WN* wn)
{
  WN *base, *offset1;
  base=offset1=NULL;
  SAC::get_base_and_offset(wn, base, offset1);
  if( base && WN_operator(base) == OPR_LDID && WN_st(base) == sym )
    return TRUE; 
  return FALSE;
}


BOOL
SAC::SAC_INFORM::check_addr_of_ldid(WN* addr_wn, WN* kid)
{
  WN* kid0 = kid;
  WN* def = NULL;
  BOOL check_in_loop = FALSE;
  while(WN_operator(kid0) == OPR_LDID)
  {
    DEF_LIST* defs=Du_Mgr->Ud_Get_Def(kid0);
    // check if def is outside out_loop
    DEF_LIST_ITER diter(defs);
    const DU_NODE* node = NULL;
    for(node = diter.First(); !diter.Is_Empty(); node = diter.Next())
    {
      if( LWN_Is_Ancestor(node->Wn(), out_loop))
	break;
    }
    if( diter.Is_Empty() )
      break;

    if(defs->Incomplete() || defs->Len() != 1)
    {
      def = defs->Head()->Wn();
      break;
    }
    else
    {
      def = defs->Head()->Wn();
      // if def is from calloc, no need to check 
      if ( def_by_calloc_or_free(def))
	break;
    }
    kid0 = WN_kid0(def);
  }
  if( def &&( loop_distant(LWN_Get_Parent(def), addr_wn) >= ALIAS_CHECK_INNER_DEPTH || 
	      loop_distant(out_loop, def) <= ALIAS_CHECK_LOOP_DEPTH) )
    check_in_loop = TRUE;
  if ( !def_by_calloc_or_free(def) && 
       ! check_base_st(kid0) &&
       !select_alias_check(kid0, check_in_loop ))
    return FALSE;
  else
    return TRUE;
}
BOOL
SAC::SAC_INFORM::collect_alias_wn(WN* wn, WN_VECTOR_SAC &ac_wns) 
{
/*
  recursively check wn and it's desendant, select all the whirl node that may alias with the splited field node that 
  is in acs. If meet any case that can not handle, return false. 
  there are 2 kinds of wn that need to be check , assume that we are going to split s[i].link to s_link
  a)If  *p  alias with s[i].link , check address p before LOOP1 in runtime, if p pointed to s[i].link, 
    make it pointed to s.link[i], select *p and unset check_in_loop be 
  b)If  *p[1][j] alias with s[i].link, because outside the loop, p[1][j] may not be defined, p[1][j] should check
    inside the loop, if the check point is not in the innerest loop, alias check code should add inside the out_loop, 
    select *p[1][j] and set check_in_loop

 */
  if(!wn) return TRUE;
  WN_VECTOR_SAC::iterator it;
  it = find(ac_wns.begin(), ac_wns.end(), wn); 
  if( it != ac_wns.end()) 
    return TRUE;
  
  WN* addr_wn;
  WN* split_wn;
  WN* def;
  int depth = loop_depth(out_loop);

  if(OPERATOR_is_load(WN_operator(wn)) || OPERATOR_is_store(WN_operator(wn)))
  {
    for(it = ac_wns.begin(); it != ac_wns.end(); it++)
    {
      split_wn = *it;
      ALIAS_RESULT asr=Aliased(Alias_Mgr, split_wn, wn);
      if (asr == POSSIBLY_ALIASED )
      {
	WN_VECTOR_SAC::iterator pa_it;
	switch(WN_operator(wn))
	{
	  case OPR_ISTORE:
	    addr_wn = WN_kid1(wn);
	    break;
	  case OPR_ILOAD:
	    addr_wn = WN_kid0(wn);
	    break;
	  default:
	    return FALSE;
	}

	// early exist if it has the same base of the 
	// splited structure
	if(check_base_st(addr_wn))
	  break;

	if( WN_operator(addr_wn) == OPR_ARRAY && WN_operator(WN_kid0(addr_wn)) == OPR_ADD)
	  addr_wn = WN_kid0(addr_wn);

	BOOL check_in_loop=FALSE;
	if( WN_operator(addr_wn) == OPR_ARRAY)
	{
	  if(WN_operator(WN_kid0(addr_wn)) == OPR_LDID)
	  {
	    if( !check_addr_of_ldid(addr_wn, WN_kid0(addr_wn)))
	      return FALSE;
	  }else if(WN_operator(WN_kid0(addr_wn)) == OPR_LDA)
	  {
	    if ( !select_alias_check(WN_kid0(addr_wn) ))
	      return FALSE;
	  }else 
	    return FALSE;
	}
	else if( WN_operator(addr_wn) == OPR_ADD )
	{
	  if( WN_operator(WN_kid0(addr_wn)) == OPR_LDID 
	      && WN_operator(WN_kid1(addr_wn)) == OPR_MPY )
	  {
	    if( !check_addr_of_ldid(addr_wn, WN_kid0(addr_wn)))
	      return FALSE;
	  }else if ( WN_operator(WN_kid1(addr_wn)) == OPR_LDID 
		     && WN_operator(WN_kid0(addr_wn)) == OPR_MPY )
	  {
	    if( !check_addr_of_ldid(addr_wn, WN_kid1(addr_wn)))
	      return FALSE;
	  }else if ( WN_operator(WN_kid0(addr_wn)) == OPR_ILOAD 
		     && WN_operator(WN_kid1(addr_wn)) == OPR_MPY )
	  {
	    if ( !select_alias_check(WN_kid0(addr_wn)))
	      return FALSE;
	  }else if ( WN_operator(WN_kid0(addr_wn)) == OPR_ILOAD 
		     && WN_operator(WN_kid1(addr_wn)) == OPR_MPY )
	  {
	    if ( !select_alias_check(WN_kid1(addr_wn)))
	      return FALSE;

	  }else
	    return FALSE;
	}else
	{
	  int d = loop_distant(out_loop, wn);
	  if( d < depth)
	    check_in_loop = TRUE;
	  if ( !select_alias_check(addr_wn, check_in_loop))
	    return FALSE;
	}
	break;
      }
      else if(asr == SAME_LOCATION)
      {
	DevWarn("NYI: has same_location");
	return FALSE;
      }
    }
  }else if(OPERATOR_is_call(WN_operator(wn)))
  {
    for(it = ac_wns.begin(); it != ac_wns.end(); it++)
    {
      split_wn = *it;
      // the mod_ref information is not accuracy enough, filter out function
      // calloc and free
      // TODO: when the alias anaysis get more accuracy result for function mod_ref
      //       information, the strcomp condition should be removed.
      if (strcmp(ST_name(WN_st(wn)), "calloc") != 0
         && strcmp(ST_name(WN_st(wn)), "free") != 0 )
      {
       if(Aliased_with_region(Alias_Mgr, split_wn, wn, WRITE) != NOT_ALIASED)
       {
         if( Trace_Struct_Split )
           fprintf(stderr, "function may alias: %s\n", ST_name(WN_st(wn)));
         return FALSE;         //collect alias meet call, has write
       }
       if(Aliased_with_region(Alias_Mgr, split_wn, wn, READ) != NOT_ALIASED)
       {
         if( Trace_Struct_Split )
           fprintf(stderr, "function may alias: %s\n", ST_name(WN_st(wn)));
         return FALSE; //collect alias meet call, has read
       }
      }
    }
  }
  if(WN_operator(wn) == OPR_BLOCK)
  {
    for(WN* w = WN_first(wn); w; w = WN_next(w))
    {
      if(!collect_alias_wn(w, ac_wns))
	return FALSE;
    }
  }
  else
  {
    for(INT kidno=0; kidno<WN_kid_count(wn); kidno++)
    {
      WN* kid = WN_kid(wn, kidno);
      if(!collect_alias_wn(kid, ac_wns))
	return FALSE;
    }
  }
  return TRUE;
}

void
SAC::SAC_INFORM::transform_candidate()
{
/*
  Do the struct split transformation. 
  For each field need to be splited, the function check_legality_and_collect_node have aready 
  collect the WN node which need to tranform in fld_info[i].cands 
  STEPS:
  1. Create new st for splited structure. 
  struct S{
  ...
  S1 f;
  ...
  } *sa; 
  we create type *S1 symbol name sa_f for storing file sa->f

  2. before the outside loop, allocate memory for splited field and copy the content of sa->f to sa_f
  sa_f = calloc( sizeof(S1), max_size_of_sa);
  for( i=0; i< max_size_of_sa; i++);
    memcpy(&sa_f[i], &sa[i].f, sizeof(sa->f));

  3. for each splited candidate in fld_info[i].cands, translate the candidate

  4. after the outside loop, revert the operation in step 2, copy back data from sa_f to sa->f. 
  
  5. add alias check before and after the outside loop
*/

  FLD_ITER fld_it=Make_fld_iter(TY_fld(orig_ty));
  
  if (Trace_Struct_Split)
  {
    fprintf(TFile, "transform_candidate\n");
    sym->Print(TFile);
  }

  WN* size_wn = WN_CreateLdid(OPCODE_make_op(OPR_LDID, 
					     TY_mtype(ST_type(size_st)), 
					     TY_mtype(ST_type(size_st))),
			      0,
			      ST_st_idx(size_st),
			      ST_type(size_st));
  Du_Mgr->Create_Def_List(size_wn);
  DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(size_wn);
  def_list->Set_Incomplete();

  // create a preg ldid for size st, use preg in loop body.
  TY_IDX size_ty = ST_type(size_st);
  TYPE_ID mtype = TY_mtype(size_ty);
  PREG_NUM size_preg = Create_Preg(mtype,"_sac_size");
  WN* stid_size_preg = WN_StidIntoPreg(mtype, size_preg, MTYPE_To_PREG(mtype), size_wn);
  LWN_Parentize(stid_size_preg);
  WN_INSERT_BlockBefore(LWN_Get_Parent(out_loop), out_loop, stid_size_preg);
  LWN_Set_Parent(stid_size_preg, LWN_Get_Parent(out_loop));
  size_wn = WN_LdidPreg(mtype, size_preg);
  Du_Mgr->Add_Def_Use(stid_size_preg, size_wn);

  WN* parent= LWN_Get_Parent(out_loop);
  int i=1;
  do
  {
    if( Query_Skiplist(Initial_LNO.Sac_Skip_List, i) )
    {
      fld_info[i].is_read = FALSE;
      fprintf(stderr, "skipping split field(%d): %s\n", i, Index_To_Str((*fld_it).name_idx) );
    }
    else if(!fld_info[i].is_read && Trace_Struct_Split )
      fprintf(stderr, "do not split field(%d): %s\n", i, Index_To_Str((*fld_it).name_idx) );
    else if ( Trace_Struct_Split )
      fprintf(stderr, "splitting field(%d): %s\n", i, Index_To_Str((*fld_it).name_idx) );

    if(!fld_info[i].is_read)
      continue;

    WN* wn_stid;

    if( split_array_element( i) )
    {
      /*
	lattice_field is the pointer of type   type(lattice[0].field[0])
	l = length_of(lattice)

	lattice[j].field[k]  ==>    lattice_field[ l*k + j ]

       */
      TY_IDX etype = TY_etype((*fld_it).type);
      ARB_HANDLE arb = TY_arb((*fld_it).type);
      int ubnd = ARB_ubnd_val(arb);
      int lbnd = ARB_lbnd_val(arb);
      Is_True(lbnd == 0 && ubnd > lbnd, ("lbnd must be zero!"));
      
      // 1. create new st for splite this structure field

      // make pointer type
      TY_IDX etype_ptr=TY_pointer(etype);
      if (!etype_ptr)
	etype_ptr = Make_Pointer_Type(etype);

      // create st
      char* name = (char*) malloc(strlen(ST_name(sym))
				  +strlen(Index_To_Str((*fld_it).name_idx)) + 10);
      sprintf(name, "%s_%s_", ST_name(sym), Index_To_Str((*fld_it).name_idx));

      ST* newst = New_ST(CURRENT_SYMTAB);
      ST_Init(newst, 
	      Save_Str(name),
	      CLASS_VAR,
	      SCLASS_AUTO,
	      EXPORT_LOCAL,
	      etype_ptr
	);
      free(name);
      fld_info[i].st_idx = ST_st_idx(newst);
      Set_ST_pt_to_unique_mem(newst);
      Set_ST_is_temp_var(newst);
      
      // 2. allocate and copy 
      // 2.1 calloc the new var to the apropriate size
      TY_IDX ty = Make_Function_Type(Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)));
      ST* calloc_st = Gen_Intrinsic_Function(ty, "calloc");
      WN* calloc_call = WN_Call(Pointer_type, MTYPE_V, 2, calloc_st);
	
      WN* calloc_size = LWN_Copy_Tree(size_wn);
      LWN_Copy_Def_Use(size_wn, calloc_size, Du_Mgr);
      calloc_size = LWN_CreateExp2(OPC_U8MPY, 
				   calloc_size,
				   WN_Intconst(MTYPE_U8, ubnd-lbnd+1));
		
      WN* parm0 = WN_CreateParm(MTYPE_U4, calloc_size, MTYPE_To_TY(MTYPE_U4), WN_PARM_BY_VALUE);
      WN* parm1 = WN_CreateParm(MTYPE_U4, WN_Intconst(MTYPE_U4, TY_size(etype)), 
				MTYPE_To_TY(MTYPE_U4), WN_PARM_BY_VALUE);
      WN_kid0(calloc_call) = parm0;
      WN_kid1(calloc_call) = parm1;

      LWN_Parentize(calloc_call);
      // save the return addr of the calloc call into the new variable

      RETURN_INFO return_info = Get_Return_Info(MTYPE_To_TY(Pointer_type), Complex_Not_Simulated);
      PREG_NUM reg_ret = RETURN_INFO_preg(return_info, 0);
      WN* ret_ldid = WN_Ldid(Pointer_type, reg_ret, Return_Val_Preg, Be_Type_Tbl(Pointer_type));

      PREG_NUM pointer_preg = Create_Preg(Pointer_type,"_sac_pointer");
      wn_stid = WN_StidIntoPreg(Pointer_type, pointer_preg, MTYPE_To_PREG(Pointer_type), ret_ldid);
      LWN_Parentize(wn_stid);
      WN_INSERT_BlockBefore(parent, out_loop, calloc_call);
      LWN_Set_Parent(calloc_call, parent);
      WN_INSERT_BlockBefore(parent, out_loop, wn_stid);
      LWN_Set_Parent(wn_stid, parent);
      fld_info[i].wn_stid = wn_stid;

      // free the calloc memory after the inner loop
      ST* free_st = Gen_Intrinsic_Function(ty, "free");
      WN* free_call = WN_Call(MTYPE_V, MTYPE_V, 1, free_st);
      WN* free_ldid = LWN_CreateLdid(OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type), wn_stid);
      Du_Mgr->Add_Def_Use(wn_stid, free_ldid);
      WN* free_parm = WN_CreateParm(Pointer_Mtype, free_ldid, Be_Type_Tbl(Pointer_Mtype), WN_PARM_BY_VALUE);
      WN_kid0(free_call) = free_parm;
      LWN_Parentize(free_call);
      LWN_Set_Parent(free_call, parent);
      WN_INSERT_BlockAfter(parent, out_loop, free_call);
      Do_DU_Update(free_call);

      fld_info[i].wn_stid = wn_stid;

      for (int j = lbnd; j <= ubnd ; j++)
      {
	// 2.2. copy the splitted field to the new memory

	// create a do_loop
      
	WN_OFFSET preg_num = Create_Preg(MTYPE_U4, "new_sac_loop_idx");
	WN *index, *start, *end, *step, *body, *do_loop;
	index = WN_CreateIdname(preg_num, MTYPE_To_PREG(MTYPE_U4));
	start = LWN_CreateStid(OPC_U4STID, preg_num, 
			       MTYPE_To_PREG(MTYPE_U4),
			       MTYPE_To_TY(MTYPE_U4),
			       LWN_Make_Icon(MTYPE_U4, 0));
	if ( WN_operator(size_wn) == OPR_LDID )
	  end = LWN_CreateExp2(OPC_U4U4LT, LWN_CreateLdid(OPC_U4U4LDID, start), LWN_CreateLdid(OPC_U4U4LDID, size_wn));
	else if ( WN_operator(size_wn) == OPR_INTCONST )
	  end = LWN_CreateExp2(OPC_U4U4LT, LWN_CreateLdid(OPC_U4U4LDID, start), LWN_Copy_Tree(size_wn));
	
	step = LWN_CreateStid(OPC_U4STID, start, 
			      LWN_CreateExp2(OPC_U4ADD, LWN_CreateLdid(OPC_U4U4LDID, start),
					     LWN_Make_Icon(MTYPE_U4,1)));
      
	body = WN_CreateBlock();
	do_loop = LWN_CreateDO( index, start, end, step, body);
      
	WN_INSERT_BlockBefore(parent, out_loop, do_loop);
	LWN_Set_Parent(do_loop, parent);

	// add the memcpy code to the do loop. 
	WN* parms[3];
	WN *mpy_wn, *mpy_wn1, *add_wn, *iload, *ldid;
      
	mpy_wn = LWN_CreateExp2(OPC_U8MPY, 
				LWN_CreateLdid(OPC_U8U4LDID, start),
				LWN_Make_Icon(Pointer_type,TY_size(etype)));
	WN* size=LWN_Copy_Tree(size_wn);
	LWN_Copy_Def_Use(size_wn, size, Du_Mgr);
	mpy_wn = LWN_CreateExp2(OPC_U8MPY,
				size,
				LWN_Make_Icon(Pointer_type, j));

	add_wn = LWN_CreateExp2(OPC_U8ADD,
				LWN_CreateLdid(OPC_U8U4LDID, start), 
				mpy_wn);
	mpy_wn = LWN_CreateExp2(OPC_U8MPY,
				LWN_Make_Icon(Pointer_type, TY_size(etype)),
				add_wn);

        WN* ldid_new_st = LWN_CreateLdid(OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type), wn_stid);
        Du_Mgr->Add_Def_Use(wn_stid, ldid_new_st);
    
	add_wn = LWN_CreateExp2(OPC_U8ADD, ldid_new_st, mpy_wn);
    
	parms[0] = WN_CreateParm(Pointer_type,
				 add_wn,
				 MTYPE_To_TY(Pointer_type),
				 WN_PARM_BY_VALUE);
	
	mpy_wn = LWN_CreateExp2(OPC_U8MPY,
				LWN_CreateLdid(OPC_U8U4LDID, start),
				LWN_Make_Icon(Pointer_type, TY_size(orig_ty)));
	add_wn = LWN_CreateExp2(OPC_U8ADD,
				WN_CreateLdid(OPR_LDID, Pointer_type, Pointer_type, 0, 
					      ST_st_idx(sym), orig_ty),
				mpy_wn);
	add_wn = LWN_CreateExp2(OPC_U8ADD,
				add_wn,
				LWN_Make_Icon(Pointer_type, (*fld_it).ofst + TY_size(etype)*j ));
	parms[1] = WN_CreateParm(Pointer_type, 
				 add_wn,
				 MTYPE_To_TY(Pointer_type), 
				 WN_PARM_BY_VALUE);

	parms[2] = WN_CreateParm(MTYPE_U4,
				 WN_Intconst(MTYPE_U4, TY_size(etype)),
				 MTYPE_To_TY(Pointer_type),
				 WN_PARM_BY_VALUE);
	WN* copy_to_new = WN_Create_Intrinsic(OPC_VINTRINSIC_CALL, INTRN_MEMCPY, 3, parms);
	LWN_Parentize(copy_to_new);
	WN_INSERT_BlockBefore(body, NULL, copy_to_new);
	LWN_Parentize(do_loop);

	DO_LOOP_INFO* dli =  CXX_NEW(DO_LOOP_INFO(&LNO_default_pool, NULL,NULL,NULL,
						  FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE) ,
				     &LNO_default_pool);
	dli->Depth = 0;
	WN_MAP_Set(LNO_Info_Map, do_loop, (void*)dli);
	DOLOOP_STACK *loop_stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool), &LNO_local_pool);
	Build_Doloop_Stack(parent, loop_stack);
	LNO_Build_Access(do_loop, loop_stack, &LNO_default_pool);
	LNO_Build_Do_Access(do_loop, loop_stack);
	Do_DU_Update(do_loop);
      
	if (Trace_Struct_Split)
	{
	  fdump_tree(TFile, do_loop);
	}
	// 4. copy back the new memory to splitted field
	// copy the do_loop
	WN *do_loop2 = LWN_Copy_Tree(do_loop);
	WN* call_wn2 = WN_first(WN_do_body(do_loop2)); 
	LWN_Set_Parent(do_loop2, parent);
	Is_True(WN_operator(call_wn2) == OPR_INTRINSIC_CALL,
		("call_wn2 is not a intrinsic_call"));
	WN* par=WN_kid0(call_wn2);
	// exchange dest and src of memcpy
	WN_kid0(call_wn2) = WN_kid1(call_wn2);
	WN_kid1(call_wn2) = par;
	WN_INSERT_BlockAfter(parent, out_loop, do_loop2);
	DO_LOOP_INFO* dli2 =  CXX_NEW(DO_LOOP_INFO(&LNO_default_pool, NULL,NULL,NULL,
						   FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE) ,
				      &LNO_default_pool);
	dli2->Depth = 0;
	WN_MAP_Set(LNO_Info_Map, do_loop2, (void*)dli2);
	DOLOOP_STACK *loop_stack2 = CXX_NEW(DOLOOP_STACK(&LNO_local_pool), &LNO_local_pool);
	Build_Doloop_Stack(parent, loop_stack2);
	LNO_Build_Access(do_loop2, loop_stack2, &LNO_default_pool);
	LNO_Build_Do_Access(do_loop2, loop_stack2);
	Do_DU_Update(do_loop2);

	if ( Trace_Struct_Split)
	{
	  fprintf(TFile, "copy back to the splitted field\n");
	  fdump_tree(TFile, do_loop2);
	}
      }
      // 3. replace splited field
      if(Trace_Struct_Split)
	fprintf(TFile, "replacing the splited field access \n");
      for(WN_VECTOR_SAC::iterator wn_it=fld_info[i].cands.begin();
	  wn_it != fld_info[i].cands.end(); wn_it++)
      {
	if ( Trace_Struct_Split)
	{
	  fprintf(TFile, "replacing wn: \n");
	}
	replace_splited_field( *wn_it , newst, size_wn, wn_stid);
      }
    }
    else if(fld_info[i].is_read)
    {
      /*
	lattice_field is the pointer of type   type(lattice[0].field)

	lattice[j].field  ==>    lattice_field[j]

       */

      // 1. create new st for splite this structure field
      char* name = (char*) malloc(strlen(ST_name(sym))
				  +strlen(Index_To_Str((*fld_it).name_idx)) + 10);
      sprintf(name, "%s_%s_", ST_name(sym), Index_To_Str((*fld_it).name_idx));

      ST* newst = New_ST(CURRENT_SYMTAB);
      ST_Init(newst, 
	      Save_Str(name),
	      CLASS_VAR,
	      SCLASS_AUTO,
	      EXPORT_LOCAL,
	      Make_Pointer_Type((*fld_it).type)
	);
      free(name);
      fld_info[i].st_idx = ST_st_idx(newst);
      Set_ST_pt_to_unique_mem(newst);
      Set_ST_is_temp_var(newst);
      
      // 2.1 calloc the new var to the apropriate size
      
      TY_IDX ty = Make_Function_Type(Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)));
      ST* calloc_st = Gen_Intrinsic_Function(ty, "calloc");
      WN* calloc_call = WN_Call(Pointer_type, MTYPE_V, 2, calloc_st);

      WN* calloc_size = LWN_Copy_Tree(size_wn);
      LWN_Copy_Def_Use(size_wn, calloc_size, Du_Mgr);
      WN* parm0 = WN_CreateParm(MTYPE_U4, calloc_size, MTYPE_To_TY(MTYPE_U4), WN_PARM_BY_VALUE);
      WN* parm1 = WN_CreateParm(MTYPE_U4, WN_Intconst(MTYPE_U4, TY_size((*fld_it).type)), 
				MTYPE_To_TY(MTYPE_U4), WN_PARM_BY_VALUE);
      WN_kid0(calloc_call) = parm0;
      WN_kid1(calloc_call) = parm1;

      LWN_Parentize(calloc_call);
      // save the return addr of the calloc call into the new variable

      RETURN_INFO return_info = Get_Return_Info(MTYPE_To_TY(Pointer_type), Complex_Not_Simulated);
      PREG_NUM reg_ret = RETURN_INFO_preg(return_info, 0);
      WN* ret_ldid = WN_Ldid(Pointer_type, reg_ret, Return_Val_Preg, Be_Type_Tbl(Pointer_type));

      // use preg hold the pointer value of new_st.
      PREG_NUM pointer_preg = Create_Preg(Pointer_type,"_sac_pointer");
      wn_stid = WN_StidIntoPreg(Pointer_type, pointer_preg, MTYPE_To_PREG(Pointer_type), ret_ldid);
      LWN_Parentize(wn_stid);
      WN_INSERT_BlockBefore(parent, out_loop, calloc_call);
      LWN_Set_Parent(calloc_call, parent);
      WN_INSERT_BlockBefore(parent, out_loop, wn_stid);
      LWN_Set_Parent(wn_stid, parent);
      fld_info[i].wn_stid = wn_stid;

      // free the calloc memory after the inner loop
      ST* free_st = Gen_Intrinsic_Function(ty, "free");
      WN* free_call = WN_Call(MTYPE_V, MTYPE_V, 1, free_st);
      WN* free_ldid = LWN_CreateLdid(OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type), wn_stid);
      Du_Mgr->Add_Def_Use(wn_stid, free_ldid);
      WN* free_parm = WN_CreateParm(Pointer_Mtype, 
				    free_ldid, 
				    Be_Type_Tbl(Pointer_Mtype), 
				    WN_PARM_BY_VALUE);
      WN_kid0(free_call) = free_parm;
      LWN_Parentize(free_call);
      LWN_Set_Parent(free_call, parent);
      WN_INSERT_BlockAfter(parent, out_loop, free_call);
      Do_DU_Update(free_call);

      fld_info[i].wn_stid = wn_stid;

      if (Trace_Struct_Split)
      {
	fprintf(TFile, "copy field to new location\n");
	fdump_tree(TFile, calloc_call);
	fdump_tree(TFile, wn_stid);
      }
      // 2.2. copy the splitted field to the new memory

      // create a do_loop
      
      WN_OFFSET preg_num = Create_Preg(MTYPE_U4, "new_sac_loop_idx");
      WN *index, *start, *end, *step, *body, *do_loop;
      index = WN_CreateIdname(preg_num, MTYPE_To_PREG(MTYPE_U4));
      start = LWN_CreateStid(OPC_U4STID, preg_num, 
			     MTYPE_To_PREG(MTYPE_U4),
			     MTYPE_To_TY(MTYPE_U4),
			     LWN_Make_Icon(MTYPE_U4, 0));
      if ( WN_operator(size_wn) == OPR_LDID )
	end = LWN_CreateExp2(OPC_U4U4LT, LWN_CreateLdid(OPC_U4U4LDID, start),
			     LWN_CreateLdid(OPC_U4U4LDID, size_wn));
      else if ( WN_operator(size_wn) == OPR_INTCONST )
	end = LWN_CreateExp2(OPC_U4U4LT, LWN_CreateLdid(OPC_U4U4LDID, start), LWN_Copy_Tree(size_wn));
	
      step = LWN_CreateStid(OPC_U4STID, start, 
			    LWN_CreateExp2(OPC_U4ADD, LWN_CreateLdid(OPC_U4U4LDID, start),
					   LWN_Make_Icon(MTYPE_U4,1)));
      
      body = WN_CreateBlock();
      do_loop = LWN_CreateDO( index, start, end, step, body);
      
      WN_INSERT_BlockBefore(parent, out_loop, do_loop);
      LWN_Set_Parent(do_loop, parent);

      // add the memcpy code to the do loop. 
      WN* parms[3];
      WN *mpy_wn, *add_wn;
      
      mpy_wn = LWN_CreateExp2(OPC_U8MPY, 
			      LWN_CreateLdid(OPC_U8U4LDID, start),
			      LWN_Make_Icon(Pointer_type,TY_size((*fld_it).type)));

      WN* ldid_new_st = LWN_CreateLdid(OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type), wn_stid);
      Du_Mgr->Add_Def_Use(wn_stid, ldid_new_st);
      add_wn = LWN_CreateExp2(OPC_U8ADD, ldid_new_st, mpy_wn);

      parms[0] = WN_CreateParm(Pointer_type,
			       add_wn,
			       MTYPE_To_TY(Pointer_type),
			       WN_PARM_BY_VALUE);
      LWN_Set_Parent(add_wn, parms[0]);

      mpy_wn = LWN_CreateExp2(OPC_U8MPY,
			      LWN_CreateLdid(OPC_U8U4LDID, start),
			      LWN_Make_Icon(Pointer_type, TY_size(orig_ty)));
      add_wn = LWN_CreateExp2(OPC_U8ADD,
			      WN_CreateLdid(OPR_LDID, Pointer_type, Pointer_type, 0, 
					    ST_st_idx(sym), orig_ty),
			      mpy_wn);
      add_wn = LWN_CreateExp2(OPC_U8ADD,
			      add_wn,
			      LWN_Make_Icon(Pointer_type, (*fld_it).ofst));
      parms[1] = WN_CreateParm(Pointer_type, 
			       add_wn,
			       MTYPE_To_TY(Pointer_type), 
			       WN_PARM_BY_VALUE);

      parms[2] = WN_CreateParm(MTYPE_U4,
			       WN_Intconst(MTYPE_U4, TY_size((*fld_it).type)),
			       MTYPE_To_TY(Pointer_type),
			       WN_PARM_BY_VALUE);
      WN* copy_to_new = WN_Create_Intrinsic(OPC_VINTRINSIC_CALL, INTRN_MEMCPY, 3, parms);
      LWN_Parentize(copy_to_new);
      WN_INSERT_BlockBefore(body, NULL, copy_to_new);
      LWN_Parentize(do_loop);

      DO_LOOP_INFO* dli =  CXX_NEW(DO_LOOP_INFO(&LNO_default_pool, NULL,NULL,NULL,
						FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE) ,
				   &LNO_default_pool);
      dli->Depth = 0;
      WN_MAP_Set(LNO_Info_Map, do_loop, (void*)dli);
      DOLOOP_STACK *loop_stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool), &LNO_local_pool);
      Build_Doloop_Stack(parent, loop_stack);
      LNO_Build_Access(do_loop, loop_stack, &LNO_default_pool);
      LNO_Build_Do_Access(do_loop, loop_stack);
      Do_DU_Update(do_loop);
      
      if (Trace_Struct_Split)
      {
	fdump_tree(TFile, do_loop);
      }

      // 3. replace the splited field access with new st. 
      
      if(Trace_Struct_Split)
	fprintf(TFile, "replacing the splited field access \n");
      for(WN_VECTOR_SAC::iterator wn_it=fld_info[i].cands.begin();
	  wn_it != fld_info[i].cands.end(); wn_it++)
      {
	if ( Trace_Struct_Split)
	{
	  fprintf(TFile, "replacing wn: \n");
	}
	replace_splited_field( *wn_it , newst, wn_stid);
      }
      
      // 4. copy back the new memory to splitted field
      // copy the do_loop
      WN *do_loop2 = LWN_Copy_Tree(do_loop);
      WN* call_wn2 = WN_first(WN_do_body(do_loop2)); 
      LWN_Set_Parent(do_loop2, parent);
      Is_True(WN_operator(call_wn2) == OPR_INTRINSIC_CALL,
	      ("call_wn2 is not a intrinsic_call"));
      WN* par=WN_kid0(call_wn2);
      // exchange dest and src of memcpy
      WN_kid0(call_wn2) = WN_kid1(call_wn2);
      WN_kid1(call_wn2) = par;
      WN_INSERT_BlockAfter(parent, out_loop, do_loop2);
      DO_LOOP_INFO* dli2 =  CXX_NEW(DO_LOOP_INFO(&LNO_default_pool, NULL,NULL,NULL,
						 FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE) ,
				    &LNO_default_pool);
      dli2->Depth = 0;
      WN_MAP_Set(LNO_Info_Map, do_loop2, (void*)dli2);
      DOLOOP_STACK *loop_stack2 = CXX_NEW(DOLOOP_STACK(&LNO_local_pool), &LNO_local_pool);
      Build_Doloop_Stack(parent, loop_stack2);
      LNO_Build_Access(do_loop2, loop_stack2, &LNO_default_pool);
      LNO_Build_Do_Access(do_loop2, loop_stack2);
      Do_DU_Update(do_loop2);

      if ( Trace_Struct_Split)
      {
	fprintf(TFile, "copy back to the splitted field\n");
	fdump_tree(TFile, do_loop2);
      }
    }
  }while(i++ && !FLD_last_field(fld_it++));

  // 5. check alias 
  parent = LWN_Get_Parent(out_loop);
  ALIAS_CHECK_VECTOR::iterator aciter;
  int tmpnumber=0;
  for( aciter = acs.begin(); aciter != acs.end(); aciter++)
  {
    WN* alias_wn = (*aciter).alias_wn;
    BOOL check_in_loop = (*aciter).check_in_loop;
    
    WN* alias_up_block;
    WN* stid_alias_preg;
    PREG_NUM alias_preg;
    WN* ldid_alias_preg;

    if( check_in_loop )
    {
      // check in loop
      // for case  like 
      //    ALIAS_WN
      //   PARENT_WN 
      // change to 
      //    ALIAS_WN
      //   STID alias_preg
      //    LIID alias_preg
      //   PARENT_WN

      TYPE_ID mtype = TY_mtype(WN_ty(alias_wn));
      alias_preg = Create_Preg(mtype, "_alias_preg");
      WN* alias_wn_parent = LWN_Get_Parent(alias_wn);
      stid_alias_preg = WN_StidIntoPreg(mtype, alias_preg, MTYPE_To_PREG(mtype), alias_wn);
      ldid_alias_preg = LWN_CreateLdid(OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type), 
					   stid_alias_preg);
      for ( INT kidno = 0; kidno < WN_kid_count(alias_wn_parent); kidno++)
      {
      	if( WN_kid(alias_wn_parent, kidno) == alias_wn )
      	{
      	  WN_kid(alias_wn_parent, kidno) = ldid_alias_preg;
      	}
      }
      LWN_Parentize(alias_wn_parent);
      LWN_Parentize(stid_alias_preg);
      Du_Mgr->Add_Def_Use(stid_alias_preg, ldid_alias_preg);

      alias_up_block = LWN_Get_Parent(alias_wn_parent);
      while( WN_operator(alias_up_block) != OPR_BLOCK )
      {
	alias_wn_parent = alias_up_block;
	alias_up_block = LWN_Get_Parent(alias_wn_parent);
      }
      
      WN_INSERT_BlockBefore(alias_up_block, alias_wn_parent, stid_alias_preg);
      LWN_Set_Parent(stid_alias_preg, alias_up_block);
    }
    /*
      check if p is alias with &lattice[j].field
      or check if p is alias with &lattice[j].field[k] if split_array_element

      a = (p - lattice) % sizeof(lattice[0]) ; 
      if ( p >= lattice && p < (lattice + max_size) ) 
      {
    	 for each splited field in lattice
    	  {
    	    if( a >= offset_of(lattice->field) && 
    		a < offset_of(lattice->field) + sizeof(lattice->field)) 
    	    {
    	      j = (p - lattice) / sizeof(lattice[0]);

    	      // if  split_array_element 
    	      {
    		k = (a - offset_of(lattice->field)) / sizeof(lattice[0].field[0]);
    		p = &lattice_field[ max_size*k + j ] + 
    		  (a - offset_of(lattice->field) - k * size_of(lattice[0].field[0]));
    	      }
        
    	      // not split_array_element
    	      {
    		p = &lattice_field[j] + (a - offset_of(lattice->field));
    	      }
    	    }
    	  }
      }
    */
    
    /* build the outer if 
       if ( p >= lattice && p < (lattice + max_size) ) */

    WN *at_p, *wn_p;
	
    // p
    wn_p = alias_wn;
    // &p
    if(WN_operator(alias_wn) == OPR_ILOAD)
    {
      at_p = WN_kid0(alias_wn);
    }else if(WN_operator(alias_wn) == OPR_LDID)
    {
      at_p = NULL; 
    }else
      Fail_FmtAssertion("unexpected operation type %d", WN_operator(alias_wn));
    
    // lattic
    WN * tmp_ldid = WN_CreateLdid(OPR_LDID, Pointer_type, Pointer_type, 0, 
				  ST_st_idx(sym), orig_ty );
    // p >= lattice
    WN * tmp_ge = LWN_CreateExp2(OPC_U4U8GE, LWN_Copy_Tree(wn_p), tmp_ldid);

    // lattice+max_size
    WN* tmp_mpy = LWN_CreateExp2(OPC_U8MPY, 
				 LWN_Copy_Tree(size_wn),
				 WN_Intconst(MTYPE_U8, TY_size(orig_ty)));
    WN * tmp_add = LWN_CreateExp2(OPC_U8ADD, LWN_Copy_Tree(tmp_ldid), tmp_mpy);

    // p < lattice + max_size
    WN * tmp_lt = LWN_CreateExp2(OPC_U4U8LT, LWN_Copy_Tree(wn_p), tmp_add);
				    
    // p >= lattice && p < (lattice + max_size)
    WN * tmp_and = LWN_CreateExp2(OPCODE_make_op(OPR_LAND, Boolean_type, MTYPE_V), tmp_ge, tmp_lt);
    
    // if (p >= lattice && p < (lattice + max_size)) {}
    WN * if_then0 = WN_CreateBlock();
    WN * wn_if0 = LWN_CreateIf( tmp_and, if_then0, WN_CreateBlock());
    IF_INFO * if_info = CXX_NEW(IF_INFO(&LNO_default_pool,FALSE,FALSE),&LNO_default_pool);
    WN_MAP_Set(LNO_Info_Map,wn_if0,(void *)if_info);
    
    // insert the outer if to proper position
    if(check_in_loop)
    {
      WN_INSERT_BlockAfter(alias_up_block, stid_alias_preg, wn_if0);
      LWN_Set_Parent(wn_if0, alias_up_block);
    }else{
      WN_INSERT_BlockBefore(parent, out_loop, wn_if0);
      LWN_Set_Parent(wn_if0, parent);
    }

    fld_it=Make_fld_iter(TY_fld(orig_ty));
    i = 1;

    do
    {
      if(fld_info[i].is_read )
      {
	// p - lattice
	WN * tmp_sub0 = LWN_CreateExp2(OPC_I8SUB, LWN_Copy_Tree(wn_p), LWN_Copy_Tree(tmp_ldid));
	
	// a = (p - lattice) % sizeof(lattice[0])
	WN * wn_a = LWN_CreateExp2(OPC_U8MOD, tmp_sub0, 
				   WN_Intconst(MTYPE_U8, TY_size(orig_ty)));
	
	// a >= offset_of(lattice->field)
	tmp_ge = LWN_CreateExp2(OPC_U4U8GE, wn_a, LWN_Make_Icon(MTYPE_U8, (*fld_it).ofst));

	// a < offset_of(lattice->field) + sizeof(lattice->field)
	tmp_lt = LWN_CreateExp2(OPC_U4U8LT, LWN_Copy_Tree(wn_a), 
				LWN_Make_Icon(MTYPE_U8, (*fld_it).ofst + TY_size((*fld_it).type)));

	// a >= offset_of(lattice->field) && 
	// a < offset_of(lattice->field) + sizeof(lattice->field)) 
	WN* if_test = LWN_CreateExp2(OPCODE_make_op(OPR_LAND, Boolean_type, MTYPE_V), tmp_ge, tmp_lt);

	// j = (p - lattice) / sizeof(lattice[0])
	WN * tmp_divfloor = LWN_CreateDivfloor(MTYPE_U8,
					       LWN_Copy_Tree(tmp_sub0),
					       LWN_Make_Icon(MTYPE_U8, TY_size(orig_ty)));

	// a - offset_of(lattice->field)
	WN * tmp_sub =  LWN_CreateExp2(OPC_U8SUB, 
				       LWN_Copy_Tree(wn_a),
				       LWN_Make_Icon(MTYPE_U8, (*fld_it).ofst));
	
	
	WN* tmp_store;
	if (split_array_element(i))
	{
	  // k = (a - offset_of(lattice->field)) / sizeof(lattice[0].field[0])
	  WN* tmp_divfloor2 = LWN_CreateDivfloor(MTYPE_U8, tmp_sub,
						 LWN_Make_Icon(MTYPE_U8, 
							       TY_size(TY_etype((*fld_it).type))));
	  // max_size * k 
	  tmp_mpy = LWN_CreateExp2(OPC_U8MPY, LWN_Copy_Tree(size_wn), tmp_divfloor2);

	  // max_size * k + j
	  tmp_add = LWN_CreateExp2(OPC_U8ADD, tmp_mpy, tmp_divfloor);

	  // &lattice_field[ max_size*k+j]
	  tmp_mpy = LWN_CreateExp2(OPC_U8MPY, tmp_add, 
				   LWN_Make_Icon(MTYPE_U8, TY_size(TY_etype((*fld_it).type))));
	  tmp_add = LWN_CreateExp2(OPC_U8ADD,
				   LWN_CreateLdid(OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type),
						  fld_info[i].wn_stid),
				   tmp_mpy);

	  // k * size_of(lattice[0].field[0])
	  tmp_mpy = LWN_CreateExp2(OPC_U8MPY, 
				   LWN_Copy_Tree(tmp_divfloor2), 
				   LWN_Make_Icon(MTYPE_U8, TY_size(TY_etype((*fld_it).type))));
	  
	  // a - offset_of(lattice->field) - k * size_of(lattice[0].field[0])
	  tmp_sub = LWN_CreateExp2(OPC_U8SUB, LWN_Copy_Tree(tmp_sub), tmp_mpy);

	  // &lattice_field[ max_size*k + j ] + (a - offset_of(lattice->field) 
	  // - k * size_of(lattice[0].field[0]))
	  tmp_add = LWN_CreateExp2(OPC_U8ADD, tmp_add, tmp_sub);
	  
	}
	else
	{
	  tmp_mpy = LWN_CreateExp2(OPC_U8MPY, 
				   tmp_divfloor, 
				   LWN_Make_Icon(Pointer_type, 
						 TY_size((*fld_it).type)));

	  // &lattice_field[j]
	  tmp_add = LWN_CreateExp2(OPC_U8ADD, 
				   LWN_CreateLdid(OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type),
						  fld_info[i].wn_stid),
				   tmp_mpy);

	  // &lattice_field[j] + (a - offset_of(lattice->field))
	  tmp_add = LWN_CreateExp2(OPC_U8ADD, tmp_add, tmp_sub);
	}

	// p = &lattice_field[ max_size*k + j ] + (a - offset_of(lattice->field) 
	//     - k * size_of(lattice[0].field[0]))
	// or p = &lattice_field[j] + (a - offset_of(lattice->field))

	
	if(check_in_loop)
	{
	  tmp_store = WN_StidIntoPreg(mtype, alias_preg, MTYPE_To_PREG(mtype), tmp_add);
	  Du_Mgr->Add_Def_Use(tmp_store, ldid_alias_preg);
	}
	else
	{
	  if(at_p)
	    tmp_store= LWN_CreateIstore(OPCODE_make_op(OPR_ISTORE, MTYPE_V, Pointer_Mtype),
					0, WN_ty(alias_wn), tmp_add, 
					LWN_Copy_Tree(at_p));
	  else
	    tmp_store = LWN_CreateStid( OPCODE_make_op(OPR_STID, MTYPE_V, Pointer_type),
					alias_wn, 
					tmp_add );
	}
					    
	WN* if_then = WN_CreateBlock();
	WN_INSERT_BlockBefore(if_then, NULL, tmp_store);
	WN* wn_if = LWN_CreateIf( if_test, if_then, WN_CreateBlock());
	if_info = CXX_NEW(IF_INFO(&LNO_default_pool,FALSE,FALSE),&LNO_default_pool);
	WN_MAP_Set(LNO_Info_Map,wn_if,(void *)if_info);

	// insert it to the outer if
	WN_INSERT_BlockBefore(if_then0, NULL, wn_if);

	if(!check_in_loop)
	{
	  /* 
	     add the alias check after outerloop
	     this is the opposite operation of alias check before outerloop
	   
	     // case not split_array_element
	     if( p >= lattice_field && p < &lattice_field[max_size])
	     {
	       a = (p - lattice_field) % sizeof(lattice[0].field)
	       b = offset_of(lattice[0].field)
	       j = (p - lattice_field) / sizeof(lattice[0].field)
	       p = &lattice[j] + b + a;
	     }
	   
	     // case split_array_element
	     if( p >= lattice_field && p < &lattice_field[max_size])
	     {
	       a = (p - lattice_field) % sizeof(lattice[0].field[0]) // offset inside the field
	       b = offset_of(lattice[0].field) // offset of field
	       c = (p - lattice_field) / sizeof(lattice[0].field[0])
	       k = c / max_size
	       j = c % max_size
	       p = &lattice[j] + b + a + k*size_of(lattice[0].field[0]) 
	     }
	  */
	
	  // p >= lattice_field
	  tmp_ge = LWN_CreateExp2(OPC_U4U8GE, LWN_Copy_Tree(wn_p), 
				  LWN_CreateLdid(OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type),
						 fld_info[i].wn_stid));
	
	  tmp_mpy = LWN_CreateExp2(OPC_U8MPY,
				   LWN_Copy_Tree(size_wn),
				   WN_Intconst(MTYPE_U8, TY_size((*fld_it).type)));
	  // &lattice_field[max_size] 
	  tmp_add = LWN_CreateExp2(OPC_U8ADD,
				   LWN_CreateLdid(OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type),
						  fld_info[i].wn_stid),
				   tmp_mpy);
	  // p < & lattice_field[max_size]
	  tmp_lt = LWN_CreateExp2(OPC_U4U8LT, LWN_Copy_Tree(wn_p), tmp_add);

	  // p >= lattice_field && p < &lattice_field[max_size]
	  if_test = LWN_CreateExp2(OPCODE_make_op(OPR_LAND, Boolean_type, MTYPE_V), tmp_ge, tmp_lt);
	
	  // p - lattice_field
	  tmp_sub = LWN_CreateExp2(OPC_I8SUB, 
				   LWN_Copy_Tree(wn_p),
				   LWN_CreateLdid(OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type),
						  fld_info[i].wn_stid));
	
	  if (split_array_element(i))
	    // a = (p - lattice_field) % sizeof(lattice[0].field[0])
	    wn_a = LWN_CreateExp2(OPC_U8MOD, 
				  LWN_Copy_Tree(tmp_sub), 
				  LWN_Make_Icon(MTYPE_U8, TY_size(TY_etype((*fld_it).type))));
	  else
	    // a = (p - lattice_field) % sizeof(lattice[0].field)
	    wn_a = LWN_CreateExp2(OPC_U8MOD, LWN_Copy_Tree(tmp_sub), 
				  LWN_Make_Icon(MTYPE_U8, TY_size((*fld_it).type)));
	
	  WN* tmp_j, *tmp_k;
	  if (split_array_element(i))
	  {
	    // c = (p - lattice_field) / sizeof(lattice[0].field[0])
	    tmp_divfloor = LWN_CreateDivfloor(MTYPE_U8, 
					      tmp_sub,
					      LWN_Make_Icon(MTYPE_U8, 
							    TY_size(TY_etype((*fld_it).type))));
	    // k = c / max_size
	    tmp_k = LWN_CreateDivfloor(MTYPE_U8, tmp_divfloor, LWN_Copy_Tree(size_wn));
	    // j = c % max_size
	    tmp_j = LWN_CreateExp2(OPC_U8MOD, LWN_Copy_Tree(tmp_divfloor), LWN_Copy_Tree(size_wn));
	  }
	  else
	    // j = (p - lattice_field) / sizeof(lattice[0].field)
	    tmp_j = LWN_CreateDivfloor(MTYPE_U8, 
				       tmp_sub,LWN_Make_Icon(MTYPE_U8, TY_size((*fld_it).type)));

	  tmp_mpy = LWN_CreateExp2(OPC_U8MPY, 
				   tmp_j, 
				   LWN_Make_Icon(MTYPE_U8, TY_size(orig_ty)));
	
	  // &lattice[j]
	  tmp_add = LWN_CreateExp2(OPC_U8ADD, LWN_Copy_Tree(tmp_ldid), tmp_mpy);
	
	  // &lattice[j] + b
	  tmp_add = LWN_CreateExp2(OPC_U8ADD, tmp_add, LWN_Make_Icon(MTYPE_U8, (*fld_it).ofst));
	
	  // &lattice[j] + b + a
	  tmp_add = LWN_CreateExp2(OPC_U8ADD, tmp_add, wn_a);
	
	  if ( split_array_element(i))
	  {
	    // k*size_of(lattice[0].field[0])
	    tmp_mpy = LWN_CreateExp2(OPC_U8MPY, 
				     tmp_k, 
				     LWN_Make_Icon(MTYPE_U8, TY_size(TY_etype((*fld_it).type))));
	    // &lattice[j] + b + a + k*size_of(lattice[0].field[0])
	    tmp_add = LWN_CreateExp2(OPC_U8ADD, tmp_add, tmp_mpy);
	  } 
	  // p = &lattice[j] + b + a  
	  // p = &lattice[j] + b + a + k*size_of(lattice[0].field[0]) 
	  if(at_p)
	    tmp_store = LWN_CreateIstore(OPCODE_make_op(OPR_ISTORE, MTYPE_V, Pointer_Mtype),
					 0, WN_ty(alias_wn), tmp_add, 
					 LWN_Copy_Tree(at_p));
	  else
	    tmp_store = LWN_CreateStid( OPCODE_make_op(OPR_STID, MTYPE_V, Pointer_type), 
					alias_wn, 
					tmp_add );

	  if_then = WN_CreateBlock();
	  WN_INSERT_BlockBefore(if_then, NULL, tmp_store);
	  wn_if = LWN_CreateIf( if_test, if_then, WN_CreateBlock());
	  if_info = CXX_NEW(IF_INFO(&LNO_default_pool,FALSE,FALSE),&LNO_default_pool);
	  WN_MAP_Set(LNO_Info_Map,wn_if,(void *)if_info);

	  WN_INSERT_BlockAfter(parent, out_loop, wn_if);
	  LWN_Parentize(wn_if);
	  LWN_Set_Parent(wn_if, parent);
	}
      }
    }while(i++ && !FLD_last_field(fld_it++));
    LWN_Parentize(wn_if0);
  }
}

void
SAC::Perform_Structure_Split_Opt()
{
/* Top level routine for optimization, called from Lnoptimizer
   steps.
   1. find candidate structure
   2. validate the candidate and collect WN node for translation.
   3. do the translation.
*/
  WN* tmp_wn;
  WN_VECTOR_SAC::iterator cand_it;

  if (OPT_Struct_Array_Copy < 2) return;
  
  if ( Get_Trace(TP_LNOPT2, TT_STRUCT_SPLIT_DUMP_IR))
  {
    fprintf(TFile, "------------------dump before Perform_Structure_Split_Opt -----------------------\n");
    fdump_tree(TFile, func_node);
  }

  Trace_Struct_Split = Get_Trace(TP_LNOPT2, TT_TRACE_STRUCT_SPLIT_TRANS /*0x10000000*/);

  // step 1
  find_struct_split_candidate(func_node, NULL, 0);
  std::vector<TY_IDX> ty_v;
  int i = 0;
  for(SAC_INFORM_VECTOR::iterator sac_it=sac_vec.begin(); sac_it!=sac_vec.end(); sac_it++)
  {
    SAC::SAC_INFORM* sac_info=*sac_it;
    
    // step 2
    if( sac_info->find_def_once() && sac_info->check_legality_and_collect_node(sac_info->out_loop, 0))
    {
      if (Trace_Struct_Split)
      {
	fprintf(TFile, "SAC translating %s(%d) \n", ST_name(sac_info->sym), i);
	fprintf(stderr, "SAC translating %s(%d) \n", ST_name(sac_info->sym), i);
      }
      if (Trace_Struct_Split)
      {
	fprintf(TFile, " before transform_candidate\n");
	fdump_tree(TFile, LWN_Get_Parent(sac_info->out_loop));
      }
      WN_VECTOR_SAC ac_cands;
      ac_cands.clear();
      int i=1;
      FLD_ITER fld_it=Make_fld_iter(TY_fld(sac_info->orig_ty));
      do
      {
	if(sac_info->fld_info[i].is_read)
	{
	  for ( WN_VECTOR_SAC::iterator it=sac_info->fld_info[i].ac_cands.begin(); 
		it != sac_info->fld_info[i].ac_cands.end(); it++)
	  {
	    if( WN_operator(*it) == OPR_ILOAD || WN_operator(*it) == OPR_ISTORE)
	      ac_cands.push_back(*it);
	  }
	}
	i++;
      }while(!FLD_last_field(fld_it++));

      BOOL ca_result; 
      ca_result = sac_info->collect_alias_wn(sac_info->out_loop, ac_cands);

      if(Trace_Struct_Split)
      {
	fprintf(TFile, ">>> found %d candidates, ca_result=%d\n", sac_info->acs.size(), ca_result);
      }

      // step 3
      if(ca_result)
      	sac_info->transform_candidate();

      if (Trace_Struct_Split)
      {
	fprintf(TFile, " after transform_candidate\n");
	fdump_tree(TFile, LWN_Get_Parent(sac_info->out_loop));
      }
    }
    else if (Trace_Struct_Split)
    {
      fprintf(TFile, "SAC not legal %s(%d) \n", ST_name(sac_info->sym), i);
      fprintf(stderr, "SAC not legal %s(%d) \n", ST_name(sac_info->sym), i);
    }
    i++;
  }

  if ( Get_Trace(TP_LNOPT2, TT_STRUCT_SPLIT_DUMP_IR ))
  {
    fprintf(TFile, "------------------dump after Perform_Structure_Split_Opt -----------------------\n");
    fdump_tree(TFile, func_node);
  }
}
