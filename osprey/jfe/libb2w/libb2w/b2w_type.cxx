/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

#include "b2w_common.h"
#include "b2w_handler.h"

/*****************************************************************************
 *  Create A Primitive Type 
 *  @param    UINT             kind
 *  @return   TY_IDX           the generated/previously_generated idx
 ******************************************************************************
*/
TY_IDX B2W_get_primitive_type(UINT kind){
    TY_IDX idx = 0;
    Is_Valid(MTYPE_FIRST <= kind && kind <= MTYPE_LAST,
      ("Type is not mtype : %d", kind));
    idx = MTYPE_To_TY (kind);

    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_ALL_EXEC), (TFile,
      "**** [B2W_get_primitive_type] -- M:%u -> IDX:%llu \n",
                       kind, (UINT64) idx));
    return idx;
}


/*****************************************************************************
 *  Set Attribute flag Const for Type
 *  @param    TY_IDX           index
 *  @return   TY_IDX           the updated type index with attributes
 ******************************************************************************
*/
TY_IDX B2W_set_ty_is_const(TY_IDX idx ){
    Set_TY_is_const(idx);
    return idx;
}

/*****************************************************************************
 *  Set Attribute flag Volatile for Type
 *  @param    TY_IDX           index
 *  @return   TY_IDX           the updated type index with attributes
 ******************************************************************************
*/
TY_IDX B2W_set_ty_is_volatile(TY_IDX idx ){
    Set_TY_is_volatile(idx);
    return idx;
}

/*****************************************************************************
 *  Set Attribute flag Volatile for Type
 *  @param    idx           ty_idx
 *  @param    flag          ty pu flag
 ******************************************************************************
*/
void B2W_set_ty_pu_flag(TY_IDX idx, int flag){
  Ty_Table[idx].Set_pu_flag((TY_PU_FLAGS) flag);
}

/******************************************************************************
 *  Create A Class Type
 *  @param name     Class_Name
 *  @param size     Constant = 1 (will be automatically calculated later)
 *  @param alignWidth  4 for 32bit and 8 for 64bit
 *  @return   TY_IDX           the generated/previously_generated idx
 ******************************************************************************
*/

TY_IDX B2W_create_class_type(CCHPTR name, INT32 size, INT32 alignWidth) {
    // new scope for local vars

    //if(size <= 0){
      //Is_Valid(FALSE,("[B2W_create_class_type] Invalid Class Size : %d",size));
    //}

    TY_IDX     idx   = 0;

    TY        &ty    = New_TY(idx);

    INT32      align = alignWidth;

    TY_Init (ty, (UINT64) size, KIND_STRUCT, MTYPE_M,
             Save_Str( name ));

    if (align == 0) {
        align = 1;    // in case incomplete type
        Set_TY_is_incomplete(idx);
    }

    Set_TY_align (idx, align);
    Set_TY_fld (ty, FLD_HANDLE());
    //Set_TY_return_in_mem(ty);

    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, "%s**** [B2W_create_class_type] : idx =  %llu , fld->Idx : %u \n%s",
                         DBar, (UINT64) idx, TY_fld(ty).Idx(), DBar));

    return idx;
}

/******************************************************************************
 *  Return Current Size Of Fld_Table (used as starter fld_idx);
 *  @return   FLD_IDX           the generated/previously_generated idx
 ******************************************************************************
*/
FLD_IDX B2W_get_current_field_idx(){
  FLD_IDX first_field_idx = Fld_Table.Size ();
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_ALL_EXEC), (TFile,
    "[B2W_get_current_field_idx] first is at : %d \n", first_field_idx));
  return first_field_idx;
}

/******************************************************************************
 * Make a pointer that points to the ty_idx given
 *****************************************************************************/
TY_IDX B2W_make_pointer_type(TY_IDX ty){
  return Make_Pointer_Type(ty, FALSE);
}


/******************************************************************************
 *  Create A Ref Type
 *  @param    char *           Pointer Name
 *  @param    TY_IDX           Pointed type Idx
 *  @return   TY_IDX           the generated/previously_generated idx
 ******************************************************************************
*/
TY_IDX B2W_create_ref_type(const char *name_utf8, TY_IDX pointedIdx_) {

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG), (TFile,  "[B2W_create_ref_type] : name = %s, pointedIdx = %u\n"
    , name_utf8, (UINT32) pointedIdx_));

  TY_IDX ty_ptr = TY_pointer (pointedIdx_, false);
  if (ty_ptr != 0) {
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
                (TFile, "[B2W_create_ref_type] TY_pointer output: %u\n" , ty_ptr));
    return ty_ptr;
  }

  TY &ty = New_TY (ty_ptr);
  TY_Init(ty, Pointer_Size, KIND_POINTER, Pointer_Mtype,
                  Save_Str (name_utf8));
  Set_TY_pointed (ty, pointedIdx_);
  Set_TY_align (ty_ptr, Pointer_Size);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG),
        (TFile, "[B2W_create_ref_type] output  : Ref_type = %u\n" , ty_ptr));
  return ty_ptr;
}


/******************************************************************************
 * Allocating A Spot for Field in a Class
 * @param class_ty class's ty-idx
 * @param fld_name  field_name
 * @param offset  fld's offset(not used in java, mostly for bitfield stuff)
 * @return FLD_IDX field created.
 ******************************************************************************/
FLD_IDX B2W_allocate_spot_in_class(TY_IDX class_ty, CCHPTR fld_name, UINT offset){

    FLD_IDX      fld_idx = Fld_Table.size();
    FLD_HANDLE   fld     = New_FLD ();
    FLD_Init     (fld, Save_Str(fld_name), 0, offset);  //No VFiled Type Needed

    if(NULLPTR == fld_name){
      Set_FLD_is_anonymous(fld);
    }

    return       fld_idx;
}

/*****************************************************************************
 * B2W_set_fld_as_pointer   Used to setup a field as a pointer,
 * @param class_ty
 * @param fld
 * @return
 ****************************************************************************/
FLD_IDX B2W_set_fld_as_pointer(TY_IDX class_ty, FLD_IDX fld){

  FLD_HANDLE     fld_handle (fld);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile,
    "[B2W_set_fld_as_pointer] class_ty: %u FLD_IDX::%u",
    class_ty, fld));

  Is_Valid(TY_kind(class_ty) == KIND_STRUCT, ("[B2W_set_fld_as_pointer] class_ ty is not a class ! \n"));

  Set_TY_content_seen(class_ty);

  TY_IDX p_idx = B2W_make_pointer_type(MTYPE_To_TY(MTYPE_U8));
  Set_FLD_type(fld_handle, p_idx);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, "::%u\n",fld));

  return fld;

}

FLD_IDX B2W_set_fld_type(TY_IDX class_ty,  FLD_IDX fld_idx,
                         TY_IDX typeidx,  UINT64 offset){

  FLD_HANDLE   fld                = FLD_HANDLE(fld_idx);
  INT32        anonymous_fields   = 0;

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile,
    "[B2W_set_fld_type] class_ty: %u <- [name: %s, fieldty: %u, offset: %llu] FLD_IDX::%u",
    class_ty, FLD_name(fld), typeidx, offset, fld_idx));

  Is_Valid(TY_kind(class_ty) == KIND_STRUCT,
    ("[B2W_add_field_to_class] class_ty is not a class ! \n"));

  Set_TY_content_seen(class_ty);

  // Set_FLD_is_base_class
  // Set_FLD_is_virtual

  //if ((TY_align (typeidx) > TY_align(class_ty)) || (TY_is_packed (typeidx)))
  //  Set_TY_is_packed (class_ty);

  Set_FLD_type(fld, typeidx);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, "::%u\n",fld.Idx()));

  return fld_idx;

}

void B2W_set_pu_constructor(ST_IDX func_symbol){
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
              (TFile,"[B2W_set_pu_constructor] sym:%u", func_symbol));
  ST      *st     = B2W_get_stptr(func_symbol);
  PU_IDX   pu_idx =  ST_pu(st);
  PU      *pu     = &Pu_Table[pu_idx];
  Set_PU_is_constructor(*pu);
}

void B2W_set_pu_no_return(ST_IDX func_symbol){
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
              (TFile,"[B2W_set_pu_no_return] sym:%u", func_symbol));
  ST      *st     = B2W_get_stptr(func_symbol);
  PU_IDX   pu_idx =  ST_pu(st);
  PU      *pu     = &Pu_Table[pu_idx];
  Set_PU_has_attr_noreturn(*pu);
}

void B2W_set_curr_pu_rbc(){
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
              (TFile,"[B2W_set_curr_pu_rbc]"));\
  Set_PU_is_rbc(Get_Current_PU());
}

void B2W_set_pu_java_lang(ST_IDX func_symbol) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
              (TFile,"[B2W_set_pu_java_lang] sym:%u", func_symbol));
  ST      *st     = B2W_get_stptr(func_symbol);
  PU_IDX   pu_idx =  ST_pu(st);
  PU      *pu     = &Pu_Table[pu_idx];
  Set_PU_java_lang(*pu);
  Clear_PU_cxx_lang(*pu);
  Clear_PU_c_lang(*pu);
}

void B2W_set_pu_is_mainpu(ST_IDX func_symbol) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
              (TFile,"[B2W_set_pu_is_mainpu] sym:%u", func_symbol));
  ST      *st     = B2W_get_stptr(func_symbol);
  PU_IDX   pu_idx =  ST_pu(st);
  PU      *pu     = &Pu_Table[pu_idx];
  Set_PU_is_mainpu(*pu);
}

void B2W_clear_pu_is_inline(ST_IDX func_symbol) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
              (TFile,"[B2W_clear_pu_is_inline] sym:%u", func_symbol));
  ST      *st     = B2W_get_stptr(func_symbol);
  PU_IDX   pu_idx =  ST_pu(st);
  PU      *pu     = &Pu_Table[pu_idx];
  Clear_PU_is_inline_function(*pu);
}

void B2W_set_pu_no_inline(ST_IDX func_symbol) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
              (TFile,"[B2W_set_pu_no_inline] sym:%u", func_symbol));
  ST      *st     = B2W_get_stptr(func_symbol);
  PU_IDX   pu_idx =  ST_pu(st);
  PU      *pu     = &Pu_Table[pu_idx];
  Set_PU_no_inline(*pu);
}

void B2W_set_pu_c_lang(ST_IDX func_symbol) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
              (TFile,"[B2W_set_pu_c_lang] sym:%u", func_symbol));
  ST      *st     = B2W_get_stptr(func_symbol);
  PU_IDX   pu_idx =  ST_pu(st);
  PU      *pu     = &Pu_Table[pu_idx];
  Set_PU_c_lang(*pu);
  Clear_PU_cxx_lang(*pu);
  Clear_PU_java_lang(*pu);
}

void B2W_set_pu_cxx_lang(ST_IDX func_symbol){
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
              (TFile,"[B2W_set_pu_cxx_lang] sym:%u", func_symbol));
  ST      *st     = B2W_get_stptr(func_symbol);
  PU_IDX   pu_idx =  ST_pu(st);
  PU      *pu     = &Pu_Table[pu_idx];
  Set_PU_cxx_lang(*pu);
  Clear_PU_c_lang(*pu);
  Clear_PU_java_lang(*pu);
}

// ============================================================================
// Set field as baseclass
// @param  FLD_IDX fld_idx
// ============================================================================
void  B2W_set_fld_baseclass(FLD_IDX fld_idx){
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
              (TFile,"[B2W_set_fld_baseclass] fld:%u", fld_idx));
    FLD_HANDLE fld(fld_idx);
    Set_FLD_is_base_class(fld);
}

TY  *B2W_get_typtr(TY_IDX idx){
  Is_Valid(TY_Table_Size() > Extract_index24(idx),
    ("[B2W_get_typtr] illegal TY_IDX:%u, (index: %u, flag:%u) "
     "please check idx is correct. ", idx, Extract_index24(idx),
     Extract_level8(idx, (INT *) 0)));
  return &Ty_Table[idx];
}

// ============================================================================
// Finish Class Fields, calculate field sizes
// @param  TY_IDX  ty
// @param  FLD_IDX first_fld_idx (very first one)
// ============================================================================
void B2W_finalize_fields(TY_IDX class_ty, FLD_IDX first_field_idx, FLD_IDX last_field_idx) {

  UINT          size           = 0;
  UINT          alignment      = B2W_CONTEXT::Get_align_width();

  B2W_get_typtr(class_ty);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DATA),
              (TFile, "[B2W_finalize_fields] <%s>(%u)  first:%u, last:%u \n",
                TY_name(class_ty), class_ty, first_field_idx, last_field_idx));

  if(TY_is_incomplete(class_ty)){
    Clear_TY_is_incomplete(class_ty);
    Set_TY_align(class_ty, B2W_CONTEXT::Get_align_width());
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
                (TFile, "[B2W_finalize_fields] default-(incomplete)-alignment:  class<%s>(%u) alignment = %d\n", TY_name(class_ty), class_ty, TY_align(class_ty)));
  }else if(TY_align(class_ty) > 0){
    // TODO should traverse the fields to determine Alignment by the largest field!
    //  @Jason Add more testcase to prove that we do it the correct way
    alignment = TY_align(class_ty);
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
                (TFile, "[B2W_finalize_fields] provided-alignment class<%s>(%u) alignment = %d\n", TY_name(class_ty), class_ty, TY_align(class_ty)));
  }else{
    Set_TY_align(class_ty, B2W_CONTEXT::Get_align_width());
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
      (TFile, "[B2W_finalize_fields] default-alignment:  class<%s>(%u) alignment = %d\n", TY_name(class_ty), class_ty, TY_align(class_ty)));
  }

  if (last_field_idx >= first_field_idx && first_field_idx > 0) {
    Set_TY_fld (class_ty, FLD_HANDLE (first_field_idx));
    Set_FLD_last_field (FLD_HANDLE (last_field_idx));

    Is_Valid(B2W_CONTEXT::Get_align_width() == 4 || B2W_CONTEXT::Get_align_width() == 8,
            ("B2W_CONTEXT::align_width = %d , should be 4 or 8. ",
              B2W_CONTEXT::Get_align_width()));

    B2W_traverse_struct_size(class_ty, &size, alignment);

    if(size % alignment > 0){
      Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_ALL_EXEC), (TFile,
        "-- final remain-padding , size => %d --> %d \n", size, size + size % alignment));
      size += (alignment - (size % alignment)) % alignment;
    }

  }else{
    // No field
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DATA), (TFile,
      "[B2W_finalize_fields] No field in class:<%s>(%u)\n",
      TY_name(class_ty), class_ty));
  }

  Is_Valid(size % alignment == 0,
    ("[B2W_finalize_fields] size = %d, align = %d is not properly aligned! \n",
      alignment,
      (B2W_get_typtr(class_ty)->Print(stderr),size)));
  Set_TY_size(class_ty, size);

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DATA),
    (TFile, "[B2W_finalize_fields] <%s>(%u) total size : %u \n%s\n",
      TY_name(class_ty), class_ty, size, DBar));

}

//BOOL B2W_calculate_field_offset(FLD_HANDLE fld, UINT64 &offset, UINT &cur_field_id, UINT targ_field_id){
//  do {
//    fld = FLD_next(fld);
//    cur_field_id ++;
//
//  }while (!FLD_last_field(fld));
//  return FALSE;
//}

void B2W_traverse_struct_size(TY_IDX class_ty, UINT *size, INT alignment){

  FLD_HANDLE    fld        = TY_fld(class_ty);
  FLD_IDX       cur_idx    = fld.Idx();
  I64VEC        field_type_list;
  BOOL          is_root    = (*size == 0);

  for(fld = FLD_HANDLE (cur_idx); TRUE;) {
    Is_True_Type((!fld.Is_Null()), ("[B2W_traverse_struct_size] Invalid fld Entry, fld<%d>", fld.Idx()), class_ty);
    Is_Valid(fld.Entry() != NULLPTR, ("[B2W_traverse_struct_size] Invalid fld Entry, fld<%d>", fld.Idx()));
    field_type_list.push_back(FLD_type(fld));
    if(FLD_last_field(fld)){
      break;
    }
    fld = FLD_next(fld);
  }

  for(UINT i = 0; i < field_type_list.size(); i++ ) {
    TY_IDX ty_idx     = (TY_IDX) field_type_list.at(i);
    UINT   field_size = 0;
    B2W_get_typtr(ty_idx);
    switch(TY_kind(ty_idx)){
      case KIND_POINTER:
        if(strstr(FLD_name(FLD_HANDLE(cur_idx + i)), ".vptr")){
          if(!FLD_last_field(FLD_HANDLE(cur_idx + i)) &&
          TY_kind(FLD_type(FLD_HANDLE(cur_idx + i + 1))) == KIND_STRUCT) {
            field_size = 0;
            break;
          }
        }
      case KIND_ARRAY:
      case KIND_SCALAR:
        field_size = (UINT) TY_size(ty_idx);
        break;
      case KIND_VOID: {
        Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_INSERT), (TFile,
          "-- field_size == empty(0) , size => %d, \n", *size));
        break;
      }
      case KIND_STRUCT: {
        if(is_root) Set_FLD_ofst(FLD_HANDLE(cur_idx + i), *size);
        B2W_traverse_struct_size(ty_idx, size, alignment);
        continue;
      }
      default:{
        B2W_get_typtr(class_ty)->Print(stderr);
        Is_Valid(FALSE, ("Class ty has a field cannot be parsed (%d) \n", i));
      }
    }
    //Padding
    UINT remain = (alignment - (*size % alignment)) % alignment;
    if (field_size > remain){
      *size += remain;
    }
    if(is_root) Set_FLD_ofst(FLD_HANDLE(cur_idx + i), *size);
    *size += field_size;
  }
}


// ============================================================================
// Create Function Type
// TY_IDX ret_idx arg_idx U32VEC(vecotr<TY_IDX>), UINT align = 4(32bit), 8(64bit)
// ============================================================================
TY_IDX B2W_create_func_type(UINT32 ret_idx, U32VEC arg_idx, UINT align){

    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
    (TFile, "[B2W_create_func_type] <%s> (%u) ( args: (Size:%lu) ) , align : %u\n",
            TY_name(ret_idx), ret_idx, arg_idx.size(), align));

    TY_IDX      idx         = TY_IDX_ZERO;
    TY          *ty         = &(New_TY (idx));
    U32VECI     it;

    Clear_TY_is_incomplete (idx);
    TY_Init (*ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
    Set_TY_align (idx, align);

    TYLIST      tylist_idx  =  0;

    // allocate TYs for return as well as parameters
    // this is needed to avoid mixing TYLISTs if one
    // of the parameters is a pointer to a function

    for (it = arg_idx.begin (); it != arg_idx.end (); it++)
    {
        if (TY_is_incomplete (*it) ||
            (TY_kind (*it) == KIND_POINTER &&
             TY_is_incomplete (TY_pointed (*it))))
            Set_TY_is_incomplete (idx);
    }

    Set_TYLIST_type (New_TYLIST (tylist_idx), ret_idx);
    Set_TY_tylist (*ty, tylist_idx); // Link TYLIST to TY

    for (it = arg_idx.begin (); it != arg_idx.end (); it++)
    {
        Is_True (!TY_is_incomplete (*it) ||
                 TY_is_incomplete (idx),
                 ("[B2W_create_func_type] unexpected TY flag : incomplete, TY_IDX 0x%x\n", idx));
        Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG), (TFile, "[B2W_create_func_type] arg: {%u <- %u} \n",
          tylist_idx, *it));
        Set_TYLIST_type (New_TYLIST (tylist_idx), *it);
    }

    if (arg_idx.size() > 0)
    {
        Set_TY_has_prototype (idx);
        if (arg_idx.back() != Be_Type_Tbl(MTYPE_V))
        {
            Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
        }
        else
            Set_TYLIST_type (Tylist_Table[tylist_idx], 0); // End it
    }
    else
        Set_TYLIST_type (New_TYLIST (tylist_idx), 0);

    // TODO: TARGET X8664 SSE Specification Ignored

    return idx;
}


//==============================================================================
// Create_Array_Type
// @param  UINT     length of array (count of elements)
// @param  TY_IDX   element's ty_idx
// @param  UINT     alignment
//==============================================================================

TY_IDX B2W_create_array_type(UINT size, TY_IDX elm_ty) {

    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
                (TFile,"[B2W_create_array_type]"));

    B2W_get_typtr(elm_ty);

    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile,
      " size: %u, elt_idx = <%s>(%u)", size, TY_name(elm_ty), elm_ty));

    TY_IDX     idx   = 0;

    TY        &ty    = New_TY(idx);
    INT32      align = TY_align(elm_ty);

    TY_Init (ty, 0, KIND_ARRAY, MTYPE_M,  Save_Str(B2W_CONTEXT::Get_Name(NULLPTR)));

    Clear_TY_is_incomplete (idx);

    if (align == 0) {
      align = 1;    // in case incomplete type
    }

    Set_TY_align (idx, align);
    Set_TY_etype (ty, elm_ty);
    // assumes 1 dimension
    // nested arrays are treated as arrays of arrays
    ARB_HANDLE arb = New_ARB ();

    ARB_Init (arb, 0, 0, 0);
    Set_TY_arb (ty, arb);
    Set_ARB_first_dimen (arb);
    Set_ARB_last_dimen (arb);
    Set_ARB_dimension (arb, 1);

    Set_ARB_const_stride(arb);
    Set_ARB_stride_val(arb, TY_size(elm_ty));

    // ================= Array lower bound =================
    Set_ARB_const_lbnd (arb);
    Set_ARB_lbnd_val (arb, 0);

    // ================= Array upper bound =================
    if(size != 0 && TY_size(elm_ty) == 0) {
      Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG), (TFile,
        "[B2W_create_array_type] warning ! elm_ty(%u) size(%u) == 0 yet TY_size > 0 !, "
        "permission array bound of INFINITY. \n", elm_ty, size));
      Set_ARB_const_ubnd (arb);
      Set_ARB_ubnd_val (arb, 0);
      Set_TY_size(ty, 0);
      Clear_TY_is_incomplete(ty);
    }else{
      Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG), (TFile,
        " upperbnd const: %u, ty_size: %llu \n", size, size * TY_size(elm_ty)));
      Set_ARB_const_ubnd (arb);
      Set_ARB_ubnd_val (arb, size);
      Set_TY_size(ty, size * TY_size(elm_ty));
      Clear_TY_is_incomplete(ty);
    }

    return idx;
}

//==============================================================================
// Create_Common_Array_Type (VLA)
// @param UINT    size (number of elements)
// @param TY_IDX  element's ty-idx
// @param UINT    align_width
// @param WN     *upper bound wn;
// @param WN     *block(wn to be put in)
// @param WN     *size  wn (normally (uwn) multiplies sizeof(ty))
// ==============================================================================

TY_IDX B2W_create_common_array_type(UINT size, TY_IDX element, INT alignWidth, WN * uwn, WN * block, WN* swn) {

  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile,
    "[B2W_create_array_type] size: %u, elt_idx = %u, align:%u, wn:%llu, block:%llu ",
    size, element, alignWidth, (UINT64) uwn, (UINT64) block));

  B2W_get_typtr(element);

  TY_IDX     idx   = 0;

  TY        &ty    = New_TY(idx);
  INT32      align = alignWidth;

  TY_Init (ty, 0, KIND_ARRAY, MTYPE_M,  Save_Str(B2W_CONTEXT::Get_Name(NULLPTR)));

  Clear_TY_is_incomplete (idx);

  if (align == 0) {
    align = TY_size(element) == 0 ? 1 : TY_size(element);    // in case incomplete type
  }

  Set_TY_align (idx, align);
  Set_TY_etype (ty, element);
  // assumes 1 dimension
  // nested arrays are treated as arrays of arrays
  ARB_HANDLE arb = New_ARB ();

  ARB_Init (arb, 0, 0, 0);
  Set_TY_arb (ty, arb);
  Set_ARB_first_dimen (arb);
  Set_ARB_last_dimen (arb);
  Set_ARB_dimension (arb, 1);

  Set_ARB_stride_var(arb, 0);

  // ================= Array lower bound =================
  Set_ARB_const_lbnd (arb);
  Set_ARB_lbnd_val (arb, 0);

  // ================= Array upper bound =================
  if(uwn != NULLPTR) {
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG), (TFile, "(vla)"));
    if (WN_opcode(uwn) == OPC_U4I4CVT ||
        WN_opcode(uwn) == OPC_U8I8CVT) {
      uwn = WN_kid0(uwn);
    }
    ST      *st;
    TY_IDX   ty_idx;
    WN      *wn;
    if (WN_operator(uwn) != OPR_LDID) {
      ty_idx = MTYPE_To_TY(WN_rtype(uwn));
      st = Gen_Temp_Symbol(ty_idx, "__vla_bound");
      //TODO: add_pragma_to_enclosing_regions(WN_PRAGMA_LOCAL, st);
      wn = WN_Stid(TY_mtype(ty_idx), 0, st, ty_idx, uwn);
      B2W_insert_block_last(block, wn);
      Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE), (TFile, ", saved to stid(%llu), temp_sym: %u",
        (UINT64) wn, st->st_idx));
    } else {
      st = WN_st(uwn);
      ty_idx = ST_type(st);

      Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE) , (TFile, ", using ldid(%llu)", (UINT64) wn));
    }

    wn = WN_CreateXpragma(WN_PRAGMA_COPYIN_BOUND, (ST_IDX) NULL, 1);
    WN_kid0(wn) = WN_Ldid(TY_mtype(ty_idx), 0, st, ty_idx);
    B2W_insert_block_last(block, wn);

    Clear_ARB_const_ubnd(arb);
    Set_ARB_ubnd_var(arb, ST_st_idx(st));
    Clear_TY_is_incomplete(idx);

    if (TY_size(TY_etype(ty))) {
      if (WN_opcode (swn) == OPC_U4I4CVT ||
          WN_opcode (swn) == OPC_U8I8CVT) {
        swn = WN_kid0 (swn);
      }
        Is_Valid(WN_operator (swn) == OPR_LDID,
                ("size operator for VLA not LDID"));
      st            = WN_st (swn);
      TY_IDX ty_idx = ST_type (st);
      TYPE_ID mtype = TY_mtype (ty_idx);
      wn  =  WN_Stid (mtype, 0, st, ty_idx, swn);
      B2W_insert_block_last(block, swn);
    }
    Clear_TY_is_incomplete (idx);

  }else if(size != 0 && TY_size(element) == 0) {
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile,
      "[B2W_create_array_type] warning ! element(%u) size(%u) != 0 yet TY_size == 0 !, "
      "permission array bound of INFINITY. ", element, size));
    Set_ARB_const_ubnd (arb);
    Set_ARB_ubnd_val (arb, 0);
    Set_TY_size(ty, 0);
    Clear_TY_is_incomplete(ty);
  }else{
    // array is variable length
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile,
      " upperbnd const: %u, ty_size: %llu", size, size * TY_size(element)));
    Set_ARB_ubnd_var (arb, 0); // unknown size, not sure right now!
    Set_TY_size(ty, size * TY_size(element)); // may not be zero
    Clear_TY_is_incomplete(ty);
  }
  return idx;
}


// ============================================================================
// Set array upper bound from const wn
// ============================================================================
void B2W_set_arb_upper_bound(TY_IDX arr_ptr, WN* size_wn)
{
  if(size_wn == NULL || WN_operator(size_wn) != OPR_INTCONST) {
    return;
  }
  TY_IDX arr_type = TY_pointed(arr_ptr);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG), (TFile, "Set upper bound"));
  if(TY_is_array_class(arr_type)) {
    // get data info at fld 5 (fixed id in JFE)
    UINT field_id = 0;
    const UINT data_fld_id = 5;
    FLD_HANDLE fld = FLD_get_to_field(arr_type, data_fld_id, field_id);
    TY_IDX data_arr = FLD_type(fld);
    ARB_HANDLE arb = TY_arb(data_arr);
    INT64 ub = WN_const_val(size_wn);
    if(ub > 0) {
      --ub;
      Set_ARB_const_ubnd(arb);
      Set_ARB_ubnd_val(arb, ub);
    }
  }
}

// ============================================================================
// Set field as baseclass
// @param  ty_idx : pointer ty idx
// ============================================================================
jlong B2W_get_pointee_ty_from_ty(long ty_idx)
{
  Is_Valid(TY_kind(ty_idx) == KIND_POINTER, ("TY: %llu not a pointer", ty_idx));
  long ret = TY_pointed(ty_idx);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_ALL_EXEC), (TFile,
    "**** [B2W_get_pointee_ty_from_ty] -- TY:%lu -> Pointee:%lu \n",
                     ty_idx, ret));
  return ret;
}

// below are global type
//==============================================================================
// java.lang.Object type
//==============================================================================
/*TY_IDX B2W_get_java_lang_object_ty()
{
    std::string  name("java.lang.Object");
    NAME_TY_MAP *ty_map = B2W_CONTEXT::Get_name_ty_map();
    NAME_TY_MAP::iterator iter = ty_map.find(name);
    if (iter != ty_map.end())
        return iter->second;
    else {
        TY_IDX ty_idx = B2W_create_class_type(name.c_str(), 4, 4);
        ty_map[name] = ty_idx;
    }
}*/

//=============================================================================
//=============================================================================
//  Up ahead are Non-JNI functions for b2wtest.  Following are JNI functions
//=============================================================================
//=============================================================================





/*******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    getPrimitiveType
 * Signature: (I)J
 ******************************************************************************/
jlong JNICALL
Java_io_xcalibyte_BGenDriver_getPrimitiveType
(JNIEnv * env_, jclass class_, jint kind_){
  jlong idx = B2W_get_primitive_type((UINT) kind_);
  return idx;
}

/*******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setTypeAttribute
 * Signature: (JI)J
 ******************************************************************************/
jlong JNICALL
Java_io_xcalibyte_BGenDriver_setTypeAttribute
(JNIEnv * env_, jclass class_, jlong idx, jint attribute){

    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
      (TFile, "%s%u%s%d\n"," *[JNI::setTypeAttribute] JNI entry. index :" ,
        (TY_IDX) idx, "attribute :", attribute));

    Is_True ((attribute == TY_CONST) || (attribute == TY_VOLATILE) || (attribute == 0),
             ("[JNI::setTypeAttribute] Create TYPE: Invalid type attribute"));

    TY_IDX retIdx = ((TY_IDX)(idx));
    if(attribute == TY_CONST) {
      retIdx = B2W_set_ty_is_const((TY_IDX) idx);
    }
    if(attribute == TY_VOLATILE) {
      retIdx = B2W_set_ty_is_volatile(retIdx);
    }

    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_VERBOSE),
      (TFile, "%s\n"," *[JNI::setTypeAttribute] JNI finished.  "));
    return (jlong) retIdx;
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setTypePuAttribute
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setTypePuAttribute
  (JNIEnv *, jclass, jlong idx, jint attribute)
{
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG),
      (TFile, "%s%u%s%d\n"," *[JNI::setTypePuAttribute] JNI entry. index :" ,
        (TY_IDX) idx, "attribute :", attribute));

    Is_True (TY_kind(idx) == KIND_FUNCTION,
             ("[JNI::setTypePuAttribute]: Not a function ty"));
    Is_True ((attribute == TY_RETURN_TO_PARAM) || (attribute == TY_IS_VARARGS) || (attribute == TY_HAS_PROTOTYPE),
             ("[JNI::setTypePuAttribute]: Invalid type attribute"));
    B2W_set_ty_pu_flag(idx, attribute);
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DEBUG),
      (TFile, "%s\n"," *[JNI::setTypePuAttribute] JNI finished.  "));
}


/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver    
 * Method:    createFunctionType
 * Signature: (J[JI)J
 *****************************************************************************/                                                         
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_createFunctionType    
  (JNIEnv * env_, jclass class_, jlong ret_idx_, jlongArray arg_idx_, jint align_){

    jlong   *arg_idx        = NULLPTR;
    jboolean isCopy         = TRUE;
    jint     array_length   = 0;
    U32VEC   args_vec;

    arg_idx      = env_->GetLongArrayElements(arg_idx_, &isCopy);
    array_length = env_->GetArrayLength(arg_idx_);

    for(int i = 0; i < array_length; i++){
      args_vec.push_back((UINT32) arg_idx[i]);
    }

    env_->ReleaseLongArrayElements(arg_idx_, arg_idx, 0);       
    return B2W_create_func_type((UINT32) ret_idx_, args_vec, (UINT) align_);
}



/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    createClassType
 * Signature: (Ljava/lang/String;II)J
 *****************************************************************************/

jlong JNICALL Java_io_xcalibyte_BGenDriver_createClassType
(JNIEnv * env_, jclass class_, jstring name_, jint size_, jint alignWidth_)
{

    CCHPTR name_utf8 = env_->GetStringUTFChars(name_, JNI_FALSE);
    UINT32 result = B2W_create_class_type(name_utf8, size_,
                                      alignWidth_);
    env_->ReleaseStringUTFChars(name_, name_utf8);

    return (jlong)result;


}

/******************************************************************************
 * Class:     io_xcalibyte_BGenDriver
 * Method:    createRefType
 * Signature: (Ljava/lang/String;)J
 *****************************************************************************/

jlong JNICALL Java_io_xcalibyte_BGenDriver_createRefType
(JNIEnv * env_, jclass class_, jstring name_, jlong pointedIdx_)
{
    CCHPTR name_utf8 = env_->GetStringUTFChars(name_, JNI_FALSE);
    UINT32 result = B2W_create_ref_type(name_utf8, (UINT32) pointedIdx_);
    env_->ReleaseStringUTFChars(name_, name_utf8);
    return (jlong)result;
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniGetCurrentFieldIdx
 * Signature: ()J
 *============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniGetCurrentFieldIdx
        (JNIEnv *, jclass){
    return (jlong) B2W_get_current_field_idx();
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    allocateSpotInClass
 * Signature: (JLjava/lang/String;JJ)J
 *============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_allocateSpotInClass
    (JNIEnv * env, jclass, jlong class_ty, jstring name, jlong offset){
    CCHPTR name_utf8 = B2W_verify_ascii_string(env, name);
    FLD_IDX result = B2W_allocate_spot_in_class((TY_IDX) class_ty, (CCHPTR)name_utf8,
                                            (UINT) offset);
    env->ReleaseStringUTFChars(name, name_utf8);
    return result;
}


/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    finalizeFields
 * Signature: (JJ)V
 *============================================================================*/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_finalizeFields
    (JNIEnv *, jclass, jlong ty, jlong first_fld_idx, jlong last_fld_idx){
   B2W_finalize_fields((TY_IDX) ty, (FLD_IDX) first_fld_idx, (FLD_IDX) last_fld_idx);
}


/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    createArrayType
 * Signature: (JJI)J
 *============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_createArrayType
    (JNIEnv *, jclass, jlong size, jlong elm_ty){
    return (jlong) B2W_create_array_type((UINT) size, (TY_IDX) elm_ty);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    createCommonArrayType
 * Signature: (JJIJJJ)J
 *============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_createCommonArrayType
    (JNIEnv *, jclass, jlong size, jlong elm_ty, jint align, jlong ubound, jlong block, jlong sizewn){
    return (jlong) B2W_create_common_array_type((UINT) size, (TY_IDX) elm_ty,
                                                (INT) align, (WN *) ubound,
                                                (WN *) block, (WN *) sizewn);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setArbUbd
 * Signature: (JJ)V
 *============================================================================*/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setArbUbd
  (JNIEnv *, jclass, jlong ty_idx, jlong ubound)
{
  B2W_set_arb_upper_bound((TY_IDX)ty_idx, (WN*)ubound);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    getRtypeFromWN
 * Signature: (J)I
 *============================================================================*/
JNIEXPORT jint JNICALL Java_io_xcalibyte_BGenDriver_getRtypeFromWN
  (JNIEnv *, jclass, jlong wn)
{
    return (jint) WN_rtype((WN*) wn);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    getMTypeFromTypeIdx
 * Signature: (J)I
 *============================================================================*/
JNIEXPORT jint JNICALL Java_io_xcalibyte_BGenDriver_getMTypeFromTypeIdx
  (JNIEnv *, jclass, jlong type_idx) {
    return (jint) TY_mtype((TY_IDX) type_idx);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniSetFldAsPointer
 * Signature: (JJ)J
 *============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniSetFldAsPointer
        (JNIEnv *, jclass, jlong class_ty, jlong fld){
    return (jlong) B2W_set_fld_as_pointer((TY_IDX) class_ty,(FLD_IDX) fld);
}


/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniMakePointerType
 * Signature: (J)J
 *============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniMakePointerType
        (JNIEnv *, jclass, jlong ty){
    return (jlong) B2W_make_pointer_type((TY_IDX) ty);

}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniSetFldBaseClass
 * Signature: (J)V
 *============================================================================*/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_jniSetFldBaseClass
        (JNIEnv *, jclass, jlong fld_idx){
    B2W_set_fld_baseclass((FLD_IDX) fld_idx);
}


/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    jniSetFldType
 * Signature: (JJJJ)J
 *============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_jniSetFldType
        (JNIEnv *, jclass, jlong class_ty, jlong fld_idx, jlong typeidx, jlong offset){
    return (jlong) B2W_set_fld_type((TY_IDX) class_ty, (FLD_IDX) fld_idx,
                                    (TY_IDX) typeidx, (UINT64) offset);
}


/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    getPointeeTyFromTY
 * Signature: (J)J
 *============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_getPointeeTyFromTY
  (JNIEnv *, jclass, jlong ty_idx)
{
  return (jlong) B2W_get_pointee_ty_from_ty(ty_idx);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    getTySize
 * Signature: (J)J
 *============================================================================*/
JNIEXPORT jlong JNICALL Java_io_xcalibyte_BGenDriver_getTySize
  (JNIEnv *, jclass, jlong ty_idx){
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_ALL_EXEC), (TFile, "[BGenDriver_getTySize] ty_idx:%u \n", (TY_IDX) ty_idx));
  B2W_get_typtr((TY_IDX) ty_idx);
  return (jlong) TY_size((TY_IDX) ty_idx);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    printTypeInfo
 * Signature: (J)V
 *============================================================================*/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_printTypeInfo
  (JNIEnv *, jclass, jlong ty_idx){
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_ALL_EXEC), (TFile, "[BGenDriver_printTypeInfo] ty_idx:%u \n", (TY_IDX) ty_idx));
  B2W_get_typtr((TY_IDX) ty_idx)->Print(TFile);
  fflush(TFile);
}

/*============================================================================*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setTyAlign
 * Signature: (JI)V
 *============================================================================*/
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setTyAlign
  (JNIEnv *, jclass, jlong typeIdx, jint align){
  TY_IDX ty = (TY_IDX) typeIdx;
  B2W_get_typtr(ty);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, "[BGenDriver_setTyAlign] idx:<%s> %u, align:%u\n",
    TY_name(ty), ty, (UINT32) align));
  Set_TY_align(ty, (UINT32) align);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    getTyAlign
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_io_xcalibyte_BGenDriver_getTyAlign
  (JNIEnv *, jclass, jlong typeIdx){
  TY_IDX ty = (TY_IDX) typeIdx;
  B2W_get_typtr(ty);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE), (TFile, "[BGenDriver_getTyAlign] idx:<%s> %u, align:%u\n",
    TY_name(ty), ty, (UINT32) TY_align(ty)));
  return (jint) TY_align(ty);
}