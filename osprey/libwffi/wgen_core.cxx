/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

// ==================================================================
// wgen_core.cxx
//
// implementation for WHIRL generator core API
// ==================================================================

#include "wgen_core.h"

#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "wn_simp.h"
#include "stab.h"
#include "const.h"
#include "pu_info.h"
#include "irbdata.h"
#include "ir_reader.h"
#include "ir_bwrite.h"
#include "ir_bread.h"
#include "erglob.h"
#include "errors.h"
#include "config.h"
#include "tracing.h"
#include "intrn_info.h"
#include "targ_sim.h"
#include "glob.h"
#include "controls.h"
#include "dwarf_DST.h"
#include "dwarf_DST_mem.h"
#include "dwarf_DST_producer.h"
#include <stdarg.h>
#include <stack>
#include <ext/hash_map>

// ==================================================================
// open64 global variables
// ==================================================================
extern const char *ABI_Name;
INT trace_verbose = FALSE;

// ==================================================================
// static variables
// ==================================================================
static PU_Info     *_pu_root = NULL;
static PU_Info     *_pu_last = NULL;
static PU_Info     *PU_Info_Table[258] = {0};
static BOOL         map_mempool_initialized = FALSE;
static MEM_POOL     _local_pool;
static Output_File *_irb_output_file;
bool _pu_has_very_high_whirl = true;
// max builtin call param count
enum { MAX_PARM_CNT = 80 };

std::stack<TY_IDX> _deferred_vtable; //vtables of record_ty

namespace {
  using __gnu_cxx::hash_map;
  struct ptrhash {
    size_t operator()(void* p) const { return reinterpret_cast<size_t>(p); }
  };
}
typedef hash_map<id_t, id_t> RTTI_ST_MAP;
RTTI_ST_MAP _rtti_st_map;
typedef hash_map<id_t, id_t> RTTI_TY_MAP;
RTTI_TY_MAP _rtti_ty_map;

typedef std::vector<id_t> FUNC_SYM;
FUNC_SYM _func_sym;
typedef hash_map<id_t, std::vector<id_t> > FUNC_ST_MAP;
FUNC_ST_MAP _func_st_map;
typedef hash_map<id_t, id_t> VTABLE_SIZE_MAP;;
VTABLE_SIZE_MAP _vtable_size_map;

// ==================================================================
// Initialize & Finalize
// ==================================================================

// Wgen_Initialize
// initialize whirl generator
void
Wgen_Initialize(cstr_t src_fname, cstr_t whirl_fname, cstr_t abi)
{
  // assign to global variables
  Src_File_Name = (char *)src_fname;
  Irb_File_Name = (char *)whirl_fname;
  ABI_Name = abi;

  // initialize all things
  MEM_Initialize();
  const char *const phase_name = "Whirl Generator";
  Set_Error_Phase(phase_name);
  Set_Error_File(NULL);
  Set_Error_Line(ERROR_LINE_UNKNOWN);

  Preconfigure();
  Init_Controls_Tbl();
  Configure();

  Initialize_Symbol_Tables(TRUE);
  IR_reader_init();
  _irb_output_file = Open_Output_Info(Irb_File_Name);

  DST_Init(NULL, 0);

  Is_True(!_pu_root && !_pu_last,
          ("pu_root or pu_last is already assigned?"));
}

static void Wgen_CreateVTable(TY_IDX ty_idx);

static void
Wgen_HandleDeferredVTables() {
  while (!_deferred_vtable.empty()) {
    Wgen_CreateVTable(_deferred_vtable.top());
    _deferred_vtable.pop();
  }
}

// Wgen_Finalize
// finalize whirl generator
void
Wgen_Finalize()
{
  Wgen_HandleDeferredVTables();

  // verufy global symtab
  Verify_SYMTAB(GLOBAL_SYMTAB);

  Restore_Cmd_Line_Ctrls();
  Write_Global_Info(_pu_root);
  Close_Output_Info();
  IR_reader_finish();
}

// ==================================================================
// String table related
// ==================================================================
static int _name_suffix = 0;
id_t
Wgen_EnterString(cstr_t str)
{
  STR_IDX str_name;
  if (!strcmp(str, ".anon"))
    str_name = Save_Str2i(str, "_", _name_suffix++);
  else
    str_name =Save_Str(str);
  return str_name;
}

// ==================================================================
// Type table related
// ==================================================================
id_t
Wgen_GetPrimitiveType(int size, bool is_unsigned, bool is_fp) {
  TYPE_ID mtype = MTYPE_UNKNOWN;
  switch (size) {
    case 0:
      mtype = MTYPE_V;
      break;
    case 1:
      if (is_unsigned)
        mtype = MTYPE_U1;
      else
        mtype = MTYPE_I1;
      break;
    case 4:
     if (is_fp)
       mtype = MTYPE_F4;
     else if (is_unsigned)
       mtype = MTYPE_U4;
     else
       mtype = MTYPE_I4;
     break;
    case 8:
     if (is_fp)
       mtype = MTYPE_F8;
     else if (is_unsigned)
       mtype = MTYPE_U8;
     else
       mtype = MTYPE_I8;
     break;
    default:
      Is_True(false,
              ("Unsupported primitive type for size: %d", size));
  }
  return MTYPE_To_TY(mtype);
}

id_t
Wgen_GetNumericType() {
  return MTYPE_To_TY(ABI_Name == "n32" ? MTYPE_I4 : MTYPE_I8);
}

// Wgen_GetArrayType: create array type as pointer to array_ty:
//   struct array_ty {
//      INT32/INT64 .length,
//      <elem_ty>*  .ptr_to_elem
//   }
id_t
Wgen_GetArrayType(id_t elem_ty, cstr_t name) {
  Is_True(elem_ty != TY_IDX_ZERO, ("invalid ty"));
  id_t field_types[4];
  field_types[0] = MTYPE_To_TY(ABI_Name == "n32" ? MTYPE_I4 : MTYPE_I8);
  field_types[1] = Wgen_EnterString(".length");
  field_types[2] = Make_Pointer_Type(elem_ty);
  field_types[3] = Wgen_EnterString(".ptr_to_elem");
  return Wgen_GetRecordType(name ? name : ".anon_array",
                            field_types, 4, false);
}

id_t
Wgen_GetStringType() {
  TY_IDX char_ty = MTYPE_To_TY(MTYPE_I1);
  return Wgen_GetArrayType(char_ty, NULL);
}

id_t
Wgen_GetAnyType() {
  id_t field_types[2];
  field_types[0] = Make_Pointer_Type(MTYPE_To_TY(MTYPE_V));
  field_types[1] = Wgen_EnterString(".value");
  return Wgen_GetRecordType(".anon.any", field_types, 2, false);
}

id_t
Wgen_GetVoidType() {
  return MTYPE_To_TY(MTYPE_V);
}

id_t
Wgen_GetDefaultPointerTy() {
  return MTYPE_To_TY(Pointer_Mtype);
}

id_t
Wgen_GetPointerTy(id_t ty) {
  Is_True(ty != TY_IDX_ZERO, ("invalid ty"));
  return Make_Pointer_Type(ty);
}

id_t
Wgen_GetPointedTy(id_t ty) {
  Is_True(ty != TY_IDX_ZERO &&
          TY_kind(ty) == KIND_POINTER,
          ("invalid ty"));
  return TY_pointed(ty);
}

// create ty for KIND_ARRAY
static TY_IDX
Wgen_CreateArrayTy(TY_IDX elem_ty, int length, char *name) {
  Is_True(elem_ty != TY_IDX_ZERO,
          ("invalid ty"));
  TY_IDX ty_idx;
  TY &ty = New_TY(ty_idx);
  UINT ty_size = length * TY_size(elem_ty);
  STR_IDX name_idx = name ? Wgen_EnterString(name) : (STR_IDX) 0;
  TY_Init(ty, ty_size, KIND_ARRAY, MTYPE_M, name_idx);
  Set_TY_etype(ty, elem_ty);
  Set_TY_align(ty_idx, TY_align(elem_ty));
  if (!name)
    Set_TY_anonymous(ty);

  ARB_HANDLE arb = New_ARB();
  ARB_Init(arb, 0, 0, 0);
  Set_TY_arb(ty, arb);
  Set_ARB_first_dimen(arb);
  Set_ARB_last_dimen(arb);
  Set_ARB_dimension(arb, 1);
  Set_ARB_const_stride(arb);
  Set_ARB_lbnd_val(arb, 0);
  Set_ARB_ubnd_val(arb, length - 1);

  return ty_idx;
}

static char *
Wgen_GetMangledName(char *name) {
  static char buf[64];
  snprintf(buf, sizeof(buf), "%ld%s", strlen(name), name);
  return buf;
}

// set vtable size
void
Wgen_SetVTableSize(id_t ty_idx, id_t vtable_size) {
  _vtable_size_map[ty_idx] = vtable_size;
}

// get vtable size from ty_idx
static int
Wgen_GetVTableSize(TY_IDX ty_idx) {
  VTABLE_SIZE_MAP::const_iterator it = _vtable_size_map.find(ty_idx);
  if (it != _vtable_size_map.end())
    return it->second;
  return 0;
}

// return TRUE if ty_idx is derived ty
static bool
Wgen_IsDerivedTy(TY_IDX ty_idx) {
  Is_True(ty_idx != TY_IDX_ZERO,
          ("invalid ty"));
  if (TY_kind(ty_idx) == KIND_STRUCT) {
    FLD_HANDLE fld = TY_fld(ty_idx);
    if (!fld.Is_Null() &&
        !strncmp(FLD_name(fld), "_base.", 6))
      return TRUE;
  }
  return FALSE;
}

// get base ty from derived ty
static TY_IDX
Wgen_GetBaseTyFromDerived(TY_IDX ty_idx) {
  if (Wgen_IsDerivedTy(ty_idx))
    return FLD_type(TY_fld(ty_idx));
  return TY_IDX_ZERO;
}

// create symbol for vtable
static ST_IDX
Wgen_GetVTableSym(TY_IDX record_ty, int vtable_size) {
  Is_True(record_ty != TY_IDX_ZERO &&
          TY_kind(record_ty) == KIND_STRUCT,
          ("invalid record ty"));
  ST *st = New_ST(GLOBAL_SYMTAB);
  TY_IDX ty_idx = Wgen_CreateArrayTy(Make_Pointer_Type(Wgen_GetNumericType()),
                                     vtable_size, (char *)".anon.array");
  STR_IDX vt_name = Save_Str2("_ZTV", Wgen_GetMangledName(TY_name(record_ty)));
  ST_Init(st, vt_name, CLASS_VAR, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, ty_idx);
  Set_ST_is_vtable(st);
  Set_ST_is_const_var(st);
  Set_ST_is_weak_symbol(st);
  return ST_st_idx(st);
}

// refine the code below into a RTTIBuilder
// Build the vtable pointer for the given type
// abi::__class_type_info.
static const char * const class_type_info =
    "_ZTVN10__cxxabiv117__class_type_infoE";
// abi::__si_class_type_info.
static const char * const si_class_type_info =
    "_ZTVN10__cxxabiv120__si_class_type_infoE";
// abi::__vmi_class_type_info.
static const char * const vmi_class_type_info =
    "_ZTVN10__cxxabiv121__vmi_class_type_infoE";
static const char * const si_class_type_info_ty =
    "__si_class_type_info";
static const char * const vmi_class_type_info_ty =
    "__vmi_class_type_info";

static ST_IDX
Wgen_BuildVTablePtr(TY_IDX ty_idx) {
  const char *vtable_name = class_type_info;;
  const char *type_info_name = "__class_type_info";

  // TODO: only support signal inhirt
  if (Wgen_IsDerivedTy(ty_idx)) {
    vtable_name = si_class_type_info;
    type_info_name = si_class_type_info_ty;
  }

  // TY for __class_type_info
  TY_IDX class_ty_info;
  TY &class_ty = New_TY(class_ty_info);
  TY_Init(class_ty, 0, KIND_STRUCT, MTYPE_M, Save_Str(type_info_name));

  // TY for VTable (an array type whose elem type is ptr of pu type (return I4))
  // pu type return MTYPE_I4
  TY_IDX pu_idx;
  TY &func = New_TY(pu_idx);
  TY_Init(func, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
  Set_TY_align(pu_idx, 1);
  TYLIST tylist_idx;
  TY_IDX rty = MTYPE_To_TY(MTYPE_I4);
  Set_TYLIST_type(New_TYLIST(tylist_idx), rty);
  Set_TY_tylist(pu_idx, tylist_idx);
  Set_TYLIST_type(New_TYLIST(tylist_idx), 0);

  // array type
  TY_IDX ptr_ty = Make_Pointer_Type(pu_idx);
  TY_IDX arr_ty_idx;
  TY &arr_ty = New_TY(arr_ty_idx);
  TY_Init(arr_ty, TY_size(ptr_ty), KIND_ARRAY, MTYPE_M, (STR_IDX) 0);
  Set_TY_etype(arr_ty, ptr_ty);
  Set_TY_align(arr_ty_idx, TY_align(ptr_ty));
  Set_TY_anonymous(arr_ty);
  ARB_HANDLE arb = New_ARB();
  ARB_Init(arb, 0, 0, 0);
  Set_TY_arb(arr_ty, arb);
  Set_ARB_first_dimen(arb);
  Set_ARB_last_dimen(arb);
  Set_ARB_dimension(arb, 1);
  Set_ARB_const_stride(arb);
  Set_ARB_lbnd_val(arb, 0);
  Set_ARB_ubnd_val(arb, TY_size(ptr_ty));

  // create vtable st
  ST *rtti_vt = New_ST(GLOBAL_SYMTAB);
  ST_Init(rtti_vt, Save_Str(vtable_name), CLASS_VAR, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, pu_idx);
  Set_ST_is_vtable(rtti_vt);
  Set_ST_vtable_ty_idx(rtti_vt, class_ty_info);
  return ST_st_idx(rtti_vt);
}

// create INITO for RTTI name
static ST_IDX
Wgen_GetSTForRTTIName(TY_IDX record_ty) {
  Is_True(record_ty != TY_IDX_ZERO &&
          TY_kind(record_ty) == KIND_STRUCT,
          ("invalid record ty"));
  char *mangled_name = Wgen_GetMangledName(TY_name(record_ty));
  STR_IDX type_name = Save_Str2("_ZTS", mangled_name);
  int len = strlen(mangled_name) + 1;
  TCON tcon = Host_To_Targ_String(MTYPE_STRING, mangled_name, len);
  TCON_IDX tcon_idx = Enter_tcon(tcon);
  INITV_IDX cur = New_INITV();
  INITV_Set_VAL(Initv_Table[cur], tcon_idx, 1);

  // create const array ty whose size is len
  TY_IDX ty_idx;
  TY &ty = New_TY(ty_idx);
  TY_IDX elem_ty = MTYPE_To_TY(MTYPE_I1);
  TY_Init(ty, len, KIND_ARRAY, MTYPE_M, (STR_IDX) 0);
  Set_TY_etype(ty, elem_ty);
  Set_TY_align(ty_idx, TY_align(elem_ty));
  Set_TY_anonymous(ty);
  ARB_HANDLE arb = New_ARB();
  ARB_Init(arb, 0, 0, 0);
  Set_TY_arb(ty, arb);
  Set_ARB_first_dimen(arb);
  Set_ARB_last_dimen(arb);
  Set_ARB_dimension(arb, 1);
  Set_ARB_const_stride(arb);
  Set_ARB_lbnd_val(arb, 0);
  Set_ARB_ubnd_val(arb, len - 1);

  ST *type_name_st = New_ST(GLOBAL_SYMTAB);
  ST_Init (type_name_st, type_name, CLASS_VAR,
           SCLASS_DGLOBAL, EXPORT_PREEMPTIBLE, ty_idx);
  Set_ST_is_initialized(type_name_st);
  New_INITO(type_name_st, cur);
  Set_ST_is_weak_symbol(type_name_st);

  return ST_st_idx(type_name_st);
}

// TY_IDXs for type info pseudo type
static TY_IDX type_info_pseudo = TY_IDX_ZERO;
static TY_IDX std_type_info = TY_IDX_ZERO;
static TY_IDX base_class_type_info_pseudo = TY_IDX_ZERO;
#define VMI_TI_PSEUDO_PREFIX   "__vmi_class_type_info_pseudo"

// generate struct ty: __type_info_pseudo
static TY_IDX
Wgen_GetTypeInfoPseudoTy() {
  if (type_info_pseudo)
    return type_info_pseudo;

  TY_IDX ty_idx;
  int first_field_idx = Fld_Table.Size();
  TY &pty = New_TY(ty_idx);
  TY_Init(pty, 0, KIND_STRUCT, MTYPE_M,
          Save_Str("__type_info_pseudo"));
  // first field
  FLD_HANDLE fld = New_FLD();
  TY_IDX ptr1 = Make_Pointer_Type(MTYPE_To_TY(MTYPE_V));
  FLD_Init(fld, Save_Str(".anonymous"), ptr1, 0);
  Set_FLD_is_anonymous(fld);
  // second field
  fld = New_FLD();
  TY_IDX ptr2 = Make_Pointer_Type(MTYPE_To_TY(MTYPE_I1));
  FLD_Init(fld, Save_Str(".anonymous"), ptr2, TY_size(ptr1));
  Set_FLD_is_anonymous(fld);
  Set_TY_size(pty, TY_size(ptr1) + TY_size(ptr2));
  Set_TY_align(ty_idx, TY_align(ptr1));
  Set_TY_fld(pty, FLD_HANDLE(first_field_idx));
  Set_FLD_last_field(FLD_HANDLE(Fld_Table.Size() - 1));

  type_info_pseudo = ty_idx;
  return ty_idx;
}

// gererate struct ty: std::type_info
static TY_IDX
Wgen_GetTypeInfoTy() {
  if (std_type_info)
    return std_type_info;

  TY_IDX ty_idx;
  TY &ptr_ty = New_TY(ty_idx);
  TY_Init(ptr_ty, 0, KIND_STRUCT, MTYPE_M,
          Save_Str("std::type_info"));
  std_type_info = ty_idx;
  return std_type_info;
}

// generate struct ty: __base_class_type_info_pseudo
static TY_IDX
Wgen_GetBaseClassTypeInfoPseudoTy() {
  if (base_class_type_info_pseudo)
    return base_class_type_info_pseudo;

  TY_IDX pseudo_idx;
  int first_field_idx = Fld_Table.Size();
  TY &pty = New_TY(pseudo_idx);
  TY_Init(pty, 0, KIND_STRUCT, MTYPE_M,
          Save_Str("__base_class_type_info_pseudo"));
  // first field
  FLD_HANDLE fld = New_FLD();
  TY_IDX ptr1 = Make_Pointer_Type(Wgen_GetTypeInfoTy());
  FLD_Init(fld, Save_Str(".anonymous"), ptr1, 0);
  Set_FLD_is_anonymous(fld);
  // second field
  fld = New_FLD();
  TY_IDX ptr2 = Make_Pointer_Type(MTYPE_To_TY(MTYPE_I8));
  FLD_Init(fld, Save_Str(".anonymous"), ptr2, TY_size(ptr1));
  Set_FLD_is_anonymous(fld);
  Set_TY_size(pty, TY_size(ptr1) + TY_size(ptr2));
  Set_TY_align(pseudo_idx, TY_align(ptr1));
  Set_TY_fld(pty, FLD_HANDLE(first_field_idx));
  Set_FLD_last_field(FLD_HANDLE(Fld_Table.Size() - 1));

  base_class_type_info_pseudo = pseudo_idx;
  return pseudo_idx;
}

// generate array ty whose size is number of base
// get elem type from Wgen_GetBaseClassTypeInfoPseudoTy()
static TY_IDX
Wgen_GetBaseArrayTy(int num_base) {
  TY_IDX ty_idx;
  TY &ty = New_TY(ty_idx);
  TY_IDX elem_ty = Wgen_GetBaseClassTypeInfoPseudoTy();
  TY_Init(ty, num_base * TY_size(elem_ty),
          KIND_ARRAY, MTYPE_M, Save_Str(".anonymous"));
  Set_TY_etype(ty, elem_ty);
  Set_TY_align(ty_idx, TY_align(elem_ty));
  Set_TY_anonymous(ty);
  ARB_HANDLE arb = New_ARB();
  ARB_Init(arb, 0, 0, 0);
  Set_TY_arb(ty, arb);
  Set_ARB_first_dimen(arb);
  Set_ARB_last_dimen(arb);
  Set_ARB_dimension(arb, 1);
  Set_ARB_const_stride(arb);
  Set_ARB_lbnd_val(arb, 0);
  Set_ARB_ubnd_val(arb, num_base - 1);
  return ty_idx;
}

// Generate __type_info_pseudo ty
static TY_IDX
Wgen_EmitTypeInfoPseudoType(STR_IDX name) {
  // generate base type info ty
  BOOL is_vmi = FALSE;
  BOOL is_si = FALSE;
  TY_IDX base_ti_ty;
  if (!strncmp(Index_To_Str(name), VMI_TI_PSEUDO_PREFIX,
               strlen(VMI_TI_PSEUDO_PREFIX))) {
    int num_base = atoi(Index_To_Str(name) + strlen(VMI_TI_PSEUDO_PREFIX));
    base_ti_ty = Wgen_GetBaseArrayTy(num_base);
    is_vmi = TRUE;
  } else if (!strcmp(Index_To_Str(name), "__si_class_type_info_pseudo")) {
    base_ti_ty = Make_Pointer_Type(Wgen_GetTypeInfoTy());
    is_si = TRUE;
  }

  int ty_size = 0;
  TY_IDX ty_idx;
  TY &ptr_ty = New_TY(ty_idx);
  TY_Init(ptr_ty, 0, KIND_STRUCT, MTYPE_M, name);
  // first field (__type_info_pseudo)
  TY_IDX pseudo_idx = Wgen_GetTypeInfoPseudoTy();
  int first_field_idx = Fld_Table.Size();
  FLD_HANDLE fld = New_FLD();
  FLD_Init(fld, Save_Str(".anonymous"), pseudo_idx, 0);
  Set_FLD_is_anonymous(fld);
  ty_size += TY_size(pseudo_idx);

  if (is_si) {
    fld = New_FLD();
    FLD_Init(fld, Save_Str(".anonymous"),
             Make_Pointer_Type(Wgen_GetTypeInfoTy()), ty_size);
    Set_FLD_is_anonymous(fld);
    ty_size += MTYPE_byte_size(Pointer_Mtype);
  } else if (is_vmi) {
    fld = New_FLD();
    FLD_Init(fld, Save_Str(".anonymous"), MTYPE_To_TY(MTYPE_I4), ty_size);
    Set_FLD_is_anonymous(fld);
    ty_size += MTYPE_byte_size(MTYPE_I4);

    fld = New_FLD();
    FLD_Init(fld, Save_Str(".anonymous"), MTYPE_To_TY(MTYPE_I4), ty_size);
    Set_FLD_is_anonymous(fld);
    ty_size += MTYPE_byte_size(MTYPE_I4);

    fld = New_FLD();
    FLD_Init(fld, Save_Str(".anonymous"), base_ti_ty, ty_size);
    Set_FLD_is_anonymous(fld);
    ty_size += TY_size(base_ti_ty);
  }

  Set_TY_size(ptr_ty, ty_size);
  Set_TY_align(ty_idx, TY_align(pseudo_idx));
  Set_TY_fld(ptr_ty, FLD_HANDLE(first_field_idx));
  Set_FLD_last_field(FLD_HANDLE(Fld_Table.Size() - 1));
  return ty_idx;
}

static TY_IDX
Wgen_GetClassTypeInfoPseudo(STR_IDX type_name) {
  RTTI_TY_MAP::const_iterator it = _rtti_ty_map.find(type_name);
  if (it != _rtti_ty_map.end())
    return it->second;

  TY_IDX ty_idx = Wgen_EmitTypeInfoPseudoType(type_name);
  _rtti_ty_map[type_name] = ty_idx;
  return ty_idx;
}

// get rtti st from _rtti_st_map
static ST_IDX
Wgen_GetRTTIForTy(TY_IDX ty_idx) {
  Is_True(ty_idx != TY_IDX_ZERO &&
          TY_kind(ty_idx) == KIND_STRUCT,
          ("invalid record ty"));
  RTTI_ST_MAP::const_iterator it = _rtti_st_map.find(ty_idx);
  if (it != _rtti_st_map.end())
    return it->second;
  return ST_IDX_ZERO;
}

// create rtti st for ty_idx
static ST_IDX
Wgen_CreateRTTIForTy(TY_IDX ty_idx) {
  Is_True(ty_idx != TY_IDX_ZERO &&
          TY_kind(ty_idx) == KIND_STRUCT,
          ("invalid record ty"));

  ST_IDX st_idx = Wgen_GetRTTIForTy(ty_idx);
  if (st_idx != ST_IDX_ZERO)
    return st_idx;

  // Add the vtable pointer
  ST_IDX rtti_vt = Wgen_BuildVTablePtr(ty_idx);

  // get addr of type name
  ST_IDX type_name = Wgen_GetSTForRTTIName(ty_idx);

  // type info name
  STR_IDX type_info_name = Save_Str("__class_type_info_pseudo");

  // Generate INITO
  INITV_IDX inv_blk = New_INITV();
  INITV_Init_Block(inv_blk, INITV_Next_Idx());
  // block for type_info
  INITV_IDX ti_blk = New_INITV();
  INITV_Init_Block(ti_blk, INITV_Next_Idx());
  INITV_IDX vtable_inv = New_INITV();
  INITV_Init_Symoff(vtable_inv, ST_ptr(rtti_vt), 16);
  INITV_IDX type_name_initv = New_INITV();
  INITV_Init_Symoff(type_name_initv, ST_ptr(type_name), 0);
  Set_INITV_next(vtable_inv, type_name_initv);

  // get rtti st of Base
  if (Wgen_IsDerivedTy(ty_idx)) {
    TY_IDX base_ty = Wgen_GetBaseTyFromDerived(ty_idx);
    Is_True(base_ty != TY_IDX_ZERO, ("invalid base ty"));
    ST_IDX base_rtti_st = Wgen_CreateRTTIForTy(base_ty);
    INITV_IDX rtti_inv = New_INITV();
    INITV_Init_Symoff(rtti_inv, ST_ptr(base_rtti_st), 0);
    Set_INITV_next(ti_blk, rtti_inv);
    type_info_name = Save_Str("__si_class_type_info_pseudo");
  }

  // generate global RTTI st
  STR_IDX str = Save_Str2("_ZTI", Wgen_GetMangledName(TY_name(ty_idx)));
  ST *st = New_ST(GLOBAL_SYMTAB);
  ST_Init(st, str, CLASS_VAR, SCLASS_DGLOBAL, EXPORT_PREEMPTIBLE,
          Wgen_GetClassTypeInfoPseudo(type_info_name));
  Set_ST_is_initialized(st);
  // set record ty for rtti st
  Set_ST_is_rtti(st);
  Set_ST_vtable_ty_idx(st, ty_idx);
  Set_ST_is_weak_symbol(st);


  New_INITO(st, inv_blk);
  st_idx = ST_st_idx(st);

  // add st_idx into _rtti_st_map
  _rtti_st_map[ty_idx] = st_idx;
  return st_idx;
}

void
Wgen_SetFuncSTForRecordTy(id_t ty_idx, id_t length, id_t func_st[]) {
  Is_True(ty_idx != TY_IDX_ZERO &&
          Wgen_IsRecordType(ty_idx),
          ("invalid record ty"));
  TY_IDX struct_ty = TY_pointed(ty_idx);
  _func_sym.clear();
  for (int i; i < length; i++) {
    _func_sym.push_back(func_st[i]);
  }
  _func_st_map.insert(std::pair<id_t, FUNC_SYM>(struct_ty, _func_sym));
}

FUNC_SYM
Wgen_GetFuncSTForRecordTy(id_t ty_idx) {
  FUNC_ST_MAP::const_iterator it = _func_st_map.find(ty_idx);
  if (it != _func_st_map.end())
    return it->second;
  Is_True(false, ("no func ptr for record ty"));
}

static void
Wgen_CreateVTableForFunctionPtr(FUNC_SYM func_obj, INITV_IDX last) {
  UINT i = 0;
  for (i; i < func_obj.size(); i++) {
    INITV_IDX cur = New_INITV();
    ST_IDX sym_idx = func_obj[i];
    INITV_Init_Symoff(cur, &St_Table[sym_idx], 0);
    Set_INITV_next(last, cur);
    last = cur;
  }
}

// create vtable for ty_idx
static void
Wgen_CreateVTable(TY_IDX ty_idx) {
  Is_True(ty_idx != TY_IDX_ZERO &&
          TY_kind(ty_idx) == KIND_STRUCT,
          ("invalid record ty"));
  ST_IDX sym_idx = TY_vtable(ty_idx);
  Is_True(sym_idx != ST_IDX_ZERO &&
          ST_vtable_ty_idx(&St_Table[sym_idx]) == ty_idx &&
          !ST_is_initialized(&St_Table[sym_idx]),
          ("vtable st not created or already generated"));

  INITV_IDX inv_blk = New_INITV();
  INITV_Init_Block(inv_blk, INITV_Next_Idx());

  // INITV for offset to top
  INITV_IDX top = New_INITV();
  INITV_Init_Integer(top, MTYPE_I8, 0);

  // INITV for RTTI
  INITV_IDX rtti = New_INITV();
  ST_IDX rtti_st = Wgen_CreateRTTIForTy(ty_idx);
  INITV_Init_Symoff(rtti, &St_Table[rtti_st], 0);
  Set_INITV_next(top, rtti);

  // INITV for function ptr
  FUNC_SYM tmp = Wgen_GetFuncSTForRecordTy(ty_idx);
  Wgen_CreateVTableForFunctionPtr(tmp, rtti);

  Set_ST_sclass(&St_Table[sym_idx], SCLASS_DGLOBAL);
  Set_ST_is_initialized(&St_Table[sym_idx]);
  New_INITO(sym_idx, inv_blk);
}

void
Wgen_SetVtableForDerivedTy(id_t ty_idx, id_t derived_ty) {
  Is_True(ty_idx != TY_IDX_ZERO &&
          TY_kind(ty_idx) == KIND_STRUCT,
          ("invalid ty_idx"));
  Is_True(derived_ty != TY_IDX_ZERO &&
          TY_kind(derived_ty) == KIND_STRUCT,
          ("invalid ty_idx"));

  // set vtable for derived_ty
  int vtable_size = Wgen_GetVTableSize(ty_idx);
  if (vtable_size) {
    ST_IDX vtable_st = Wgen_GetVTableSym(derived_ty, vtable_size);
    Set_TY_vtable(derived_ty, vtable_st);
    Set_ST_vtable_ty_idx(ST_ptr(vtable_st), derived_ty);
    _deferred_vtable.push(derived_ty);
  }
}

id_t
Wgen_GetDerivedTypeFromBase(cstr_t name, id_t ty_idx) {
  Is_True(ty_idx != TY_IDX_ZERO &&
          Wgen_IsRecordType(ty_idx),
          ("invalid record ty"));
  ty_idx = TY_pointed(ty_idx);

  TY_IDX derived_ty;
  TY &pty = New_TY(derived_ty);
  STR_IDX name_idx = Wgen_EnterString(name);
  TY_Init(pty, 0, KIND_STRUCT, MTYPE_M, name_idx);

  UINT first_field_idx = Fld_Table.Size();
  FLD_HANDLE fld = New_FLD();
  STR_IDX fld_name = Save_Str2("_base.", TY_name(ty_idx));
  FLD_Init(fld, fld_name, ty_idx, 0);
  Set_TY_size(derived_ty, TY_size(ty_idx));
  Set_TY_fld(derived_ty, FLD_HANDLE(first_field_idx));
  Set_FLD_last_field(FLD_HANDLE(Fld_Table.Size() - 1));

  // set vtable for derived_ty
  Wgen_SetVtableForDerivedTy(ty_idx, derived_ty);

  return Make_Pointer_Type(derived_ty);
}

id_t
Wgen_CreateIncompleteRecordType(cstr_t name, bool is_union) {
  TY_IDX ty_idx;
  TY &pty = New_TY(ty_idx);
  STR_IDX name_idx = Wgen_EnterString(name);
  TY_Init(pty, 0, KIND_STRUCT, MTYPE_M, name_idx);
  if (is_union)
    Set_TY_is_union(pty);
  Set_TY_is_incomplete(pty);
  return ty_idx;
}

id_t
Wgen_GetRecordType(cstr_t name, id_t field_types[], int field_num, bool is_union, id_t vtable_size, bool need_vtable, id_t ty_idx) {
  if (ty_idx == TY_IDX_ZERO) {
    ty_idx = Wgen_CreateIncompleteRecordType(name, is_union);
  }

  UINT ty_size = 0;
  UINT max_size = 0;
  UINT max_align = 0;
  UINT fld_align = 0;
  UINT first_field_idx = Fld_Table.Size();
  UINT next_field_id = 1;

  // set vptr
  if (vtable_size) {
    FLD_HANDLE fld = New_FLD();
    STR_IDX fld_name = Save_Str2("_vptr.", name);
    TY_IDX vptr_ty = Make_Pointer_Type(ty_idx);
    FLD_Init(fld, fld_name, vptr_ty, ty_size);
    ty_size += TY_size(vptr_ty);
    next_field_id++;
  }

  // field_types is a array which contains fld ty & fld name
  // get real field num
  UINT field_elems = field_num / 2;
  for (int n = 0; n < field_num; ++n) {
    TY_IDX fld_ty = field_types[n];
    STR_IDX fld_name = field_types[++n];
    FLD_HANDLE fld = New_FLD();
    next_field_id++;
    if (!strncmp(Index_To_Str(fld_name), ".anon", 4))
      Set_FLD_is_anonymous(fld);

    UINT field_align = TY_align(fld_ty);
    if (field_align > max_align)
      max_align = field_align;
    fld_align = ty_size % field_align;
    if (fld_align != 0)
      ty_size += field_align - fld_align;

    FLD_Init(fld, fld_name, fld_ty, ty_size);
    ty_size += TY_size(fld_ty);
    if (TY_size(fld_ty) > max_size)
      max_size = TY_size(fld_ty);
    if (is_union)
      Set_FLD_ofst(fld, 0);
  }
  // set ty_size & ty_align
  if (max_align != 0)
    fld_align = ty_size % max_align;
  if (fld_align != 0)
    ty_size += max_align - fld_align;
  if (is_union)
    Set_TY_size(ty_idx, max_size);
  else
    Set_TY_size(ty_idx, ty_size);

  if (next_field_id > 1) {
    FLD_IDX last_field_idx = Fld_Table.Size() - 1;
    if (last_field_idx >= first_field_idx) {
      Set_TY_fld(ty_idx, FLD_HANDLE (first_field_idx));
      Set_FLD_last_field(FLD_HANDLE(last_field_idx));
    }
  }
  if (TY_is_incomplete(ty_idx))
    Clear_TY_is_incomplete(ty_idx);

  // set vtable for ty_idx
  if (vtable_size) {
    Wgen_SetVTableSize(ty_idx, vtable_size);
    ST_IDX vtable_st = Wgen_GetVTableSym(ty_idx, vtable_size);
    Set_TY_vtable(ty_idx, vtable_st);
    Set_ST_vtable_ty_idx(ST_ptr(vtable_st), ty_idx);
    if (need_vtable)
      _deferred_vtable.push(ty_idx);
  }

  return Make_Pointer_Type(ty_idx);
}

void
Wgen_STSetOptionalArgs(id_t param_st) {
  Is_True(param_st != ST_IDX_ZERO, ("invalid st"));
  Set_ST_is_optional_argument(ST_ptr(param_st));
}

void
Wgen_TYSetOptionalArgs(id_t pu_ty) {
  Is_True(pu_ty != TY_IDX_ZERO, ("invalid ty"));
  Is_True(TY_kind(pu_ty) == KIND_FUNCTION, ("should be KIND_FUNCTION"));
  Set_TY_has_optional_args(pu_ty);
}

void
Wgen_TYSetVarArgs(id_t pu_ty) {
  Is_True(pu_ty != TY_IDX_ZERO, ("invalid ty"));
  Is_True(TY_kind(pu_ty) == KIND_FUNCTION, ("should be KIND_FUNCTION"));
  Set_TY_is_varargs(pu_ty);
}

id_t
Wgen_FunctionType(id_t ret_ty, id_t parm_types[], int length) {
  Is_True(ret_ty != TY_IDX_ZERO, ("invalid ret ty"));
  TY_IDX ty_idx;

  // function type
  TY &func = New_TY(ty_idx);
  TY_Init(func, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
  Set_TY_align(ty_idx, 1);

  TYLIST tylist_idx;
  Set_TYLIST_type(New_TYLIST(tylist_idx), ret_ty);
  Set_TY_tylist(ty_idx, tylist_idx);

  // set param type into TYLIST
  for (int n = 0; n < length; ++n) {
    Set_TYLIST_type(New_TYLIST(tylist_idx), parm_types[n]);
  }

  // end of param
  Set_TYLIST_type(New_TYLIST(tylist_idx), 0);
  return ty_idx;
}

id_t
Wgen_GetReturnTy(id_t func_ty) {
  Is_True(func_ty != TY_IDX_ZERO &&
          TY_kind(func_ty) == KIND_FUNCTION,
          ("invalid function ty"));
  return TY_ret_type(func_ty);
}

id_t
Wgen_GetParmTyFromFuncTy(id_t func_ty, id_t index) {
  Is_True(func_ty != TY_IDX_ZERO &&
          TY_kind(func_ty) == KIND_FUNCTION,
          ("invalid function ty"));
  // get parm type list
  TYLIST_IDX tl = TY_parms(func_ty);
  if (tl != (TYLIST_IDX)NULL) {
    UINT i = 0;
    for (; TYLIST_ty(tl); tl = TYLIST_next(tl), ++i) {
      if (i == index)
        break;
    }
    return TYLIST_ty(tl);
  }
  return (id_t)0;
}

id_t
Wgen_GetTyFromST(id_t st_idx) {
  Is_True(st_idx != ST_IDX_ZERO, ("invalid st"));
  if (ST_class(st_idx) == CLASS_FUNC)
    return ST_pu_type(st_idx);
  else
    return ST_type(st_idx);
}

id_t
Wgen_GetTyFromWN(ptr_t wn) {
  Is_True((WN *)wn != NULL, ("invalid wn"));
  TY_IDX ty_idx = TY_IDX_ZERO;
  if (WN_operator((WN *)wn) != OPR_INTCONST &&
      WN_operator((WN *)wn) != OPR_CONST) {
    ty_idx = WN_ty((WN *)wn);
    // get fld ty
    if (WN_field_id((WN *)wn) != 0 &&
        TY_kind(ty_idx) == KIND_STRUCT)
      ty_idx = Wgen_GetFldTy(Make_Pointer_Type(ty_idx),
                             WN_field_id((WN *)wn));
  }
  if (ty_idx == TY_IDX_ZERO)
    ty_idx = MTYPE_To_TY(WN_rtype((WN *)wn));
  return ty_idx;
}

id_t
Wgen_GetTYSize(id_t ty_idx) {
  Is_True(ty_idx != TY_IDX_ZERO, ("invalid ty"));
  return TY_size(ty_idx);
}

id_t
Wgen_GetElemTy(id_t ty_idx) {
  Is_True(Wgen_IsArrayType(ty_idx),
          ("invalid array ty"));
  TY_IDX struct_ty = TY_pointed(ty_idx);
  TY_IDX fld_ty = FLD_type(FLD_next(TY_fld(struct_ty)));
  Is_True(TY_kind(fld_ty) == KIND_POINTER,
          ("should be pointer ty"));
  return TY_pointed(fld_ty);
}

id_t
Wgen_GetFldCount(id_t ty_idx) {
  Is_True(ty_idx != TY_IDX_ZERO &&
          Wgen_IsRecordType(ty_idx),
          ("invalid record ty"));
  TY_IDX struct_ty = TY_pointed(ty_idx);
  return FLD_get_count(struct_ty) - 1;
}

FLD_HANDLE
Wgen_GetFldByFieldId(id_t ty_idx, id_t field_id) {
  Is_True(ty_idx != TY_IDX_ZERO &&
          Wgen_IsRecordType(ty_idx),
          ("invalid record ty"));
  TY_IDX struct_ty = TY_pointed(ty_idx);
  UINT64 fld_count = FLD_get_count(struct_ty);
  Is_True(field_id > 0 && field_id < fld_count,
          ("invalid field id"));

  UINT cur_id = 0;
  FLD_HANDLE fld_handle = FLD_get_to_field(struct_ty, field_id, cur_id);
  if (!fld_handle.Is_Null())
    return fld_handle;
  else
    return FLD_HANDLE();
}

id_t
Wgen_GetFldTy(id_t ty_idx, id_t field_id) {
  Is_True(ty_idx != TY_IDX_ZERO &&
          Wgen_IsRecordType(ty_idx),
          ("invalid record ty"));
  TY_IDX struct_ty = TY_pointed(ty_idx);
  UINT64 fld_count = FLD_get_count(struct_ty);
  Is_True(field_id > 0 && field_id < fld_count,
          ("invalid field id"));

  FLD_HANDLE fld = Wgen_GetFldByFieldId(ty_idx, field_id);
  if (fld == FLD_HANDLE())
    return TY_IDX_ZERO;
  else
    return FLD_type(fld);
}

cstr_t
Wgen_GetFldName(id_t ty_idx, id_t field_id) {
  FLD_HANDLE fld = Wgen_GetFldByFieldId(ty_idx, field_id);
  if (fld != FLD_HANDLE())
    return FLD_name(fld);
  return NULL;
}

id_t
Wgen_GetFieldIdByFldName(id_t ty_idx, cstr_t fld_name) {
  Is_True(ty_idx != TY_IDX_ZERO &&
          Wgen_IsRecordType(ty_idx),
          ("invalid record ty"));

  UINT field_id = 0;
  TY_IDX struct_ty = TY_pointed(ty_idx);
  FLD_HANDLE fld = FLD_get_to_field_name(struct_ty, (char *)fld_name, field_id);
  Is_True(!fld.Is_Null() && field_id, ("invalid fld & field id"));
  return field_id;
}

bool
Wgen_IsArrayType(id_t ty_idx) {
  Is_True(ty_idx != TY_IDX_ZERO, ("invalid ty"));
  if (TY_kind(ty_idx) == KIND_POINTER &&
      TY_kind(TY_pointed(ty_idx)) == KIND_STRUCT &&
      FLD_get_count(TY_pointed(ty_idx)) == 3 &&
      strstr(TY_name(TY_pointed(ty_idx)), ".anon_array")) {
    TY_IDX struct_ty = TY_pointed(ty_idx);
    // check fld name
    if (strstr(Get_fld_name(struct_ty, 1), ".length") &&
        strstr(Get_fld_name(struct_ty, 2), ".ptr_to_elem") &&
        TY_kind(FLD_type(FLD_next(TY_fld(struct_ty)))) == KIND_POINTER)
      return true;
  }
  return false;
}

bool
Wgen_IsStringType(id_t ty_idx) {
  Is_True(ty_idx != TY_IDX_ZERO, ("invalid ty"));
  if (Wgen_IsArrayType(ty_idx)) {
    TY_IDX elem_ty = Wgen_GetElemTy(ty_idx);
    if (elem_ty == MTYPE_To_TY(MTYPE_I1))
      return true;
  }
  return false;
}

bool
Wgen_IsRecordType(id_t ty_idx) {
  Is_True(ty_idx != TY_IDX_ZERO, ("invalid ty"));
  if (TY_kind(ty_idx) == KIND_POINTER &&
      TY_kind(TY_pointed(ty_idx)) == KIND_STRUCT)
    return true;
  return false;
}

bool
Wgen_IsUnionType(id_t ty_idx) {
  Is_True(ty_idx != TY_IDX_ZERO, ("invalid ty"));
  if (Wgen_IsRecordType(ty_idx) &&
      TY_is_union(TY_pointed(ty_idx)))
    return true;
  return false;
}

bool
Wgen_IsAnyType(id_t ty_idx) {
  Is_True(ty_idx != TY_IDX_ZERO, ("invalid ty"));
  if (TY_kind(ty_idx) == KIND_POINTER &&
      TY_kind(TY_pointed(ty_idx)) == KIND_STRUCT &&
      FLD_get_count(TY_pointed(ty_idx)) == 2 &&
      strstr(TY_name(TY_pointed(ty_idx)), ".anon.any")) {
    TY_IDX struct_ty = TY_pointed(ty_idx);
    // check fld name
    if (strstr(Get_fld_name(struct_ty, 1), ".value")) {
      TY_IDX fld_ty = FLD_type(TY_fld(struct_ty));
      if (TY_kind(fld_ty) == KIND_POINTER &&
          TY_mtype(TY_pointed(fld_ty)) == MTYPE_V)
      return true;
    }
  }
  return false;
}

bool
Wgen_IsFunctionTy(id_t ty_idx) {
  Is_True(ty_idx != TY_IDX_ZERO, ("invalid ty"));
  if (TY_kind(ty_idx) == KIND_FUNCTION)
    return true;
  return false;
}

bool
Wgen_TyHasVtable(id_t ty_idx) {
  Is_True(ty_idx != TY_IDX_ZERO &&
          TY_kind(ty_idx) == KIND_STRUCT,
          ("invalid ty"));
  if (TY_vtable(ty_idx))
    return true;
  return false;
}

// ==================================================================
// Symbol table related
// ==================================================================

id_t
Wgen_CreateSymbol(cstr_t str_name, id_t ty_idx) {
  Is_True(ty_idx != TY_IDX_ZERO, ("invalid ty_idx"));
  // TODO: sclass/eclass
  SYMTAB_IDX symtab = CURRENT_SYMTAB;
  ST_SCLASS sclass;
  ST_EXPORT eclass = EXPORT_PREEMPTIBLE;
  if (CURRENT_SYMTAB == GLOBAL_SYMTAB) {
    sclass = SCLASS_UGLOBAL;
  } else {
    sclass = SCLASS_AUTO;
    eclass = EXPORT_LOCAL;
  }
  id_t str_idx = Wgen_EnterString(str_name);

  ST *st = New_ST(symtab);
  ST_Init(st, str_idx, CLASS_VAR, sclass, eclass, ty_idx);
  return ST_st_idx(st);
}

id_t
Wgen_CreateFunctionSymbol(cstr_t str_name, id_t pu_ty_idx) {
  Is_True(pu_ty_idx!= TY_IDX_ZERO, ("invalid ty_idx"));
  id_t str_idx = Wgen_EnterString(str_name);

  // for function, ty_idx is pu_idx
  TY_IDX ty_idx;
  PU_IDX pu_idx;
  PU &pu = New_PU(pu_idx);

  PU_Init(pu, pu_ty_idx, GLOBAL_SYMTAB + 1);
  ty_idx = (TY_IDX) pu_idx;
  ST *st = New_ST(GLOBAL_SYMTAB);

  ST_EXPORT st_exp = EXPORT_PREEMPTIBLE;

  // TODO: pu flags & isDefined
  bool isDefined = TRUE;
  ST_Init(st, str_idx, CLASS_FUNC,
          isDefined ? SCLASS_TEXT : SCLASS_EXTERN,
          st_exp, ty_idx);

  // TODO: st weak ?
  return ST_st_idx(st);
}

// ==================================================================
// PU table related
// ==================================================================
// body wn of current function
static std::stack<WN *> _cur_func_body_wn;
// entry wn of current function
static std::vector<WN *> _cur_entry_wn;

static void
Wgen_StmtPush(WN *wn) {
  Is_True(wn != NULL &&
          WN_operator(wn) == OPR_BLOCK,
          ("should be OPR_BLOCK"));
  _cur_func_body_wn.push(wn);
}

static WN *
Wgen_StmtTop() {
  Is_True(!_cur_func_body_wn.empty(),
          ("no wn in _cur_func_body_wn"));
  return _cur_func_body_wn.top();
}

void
Wgen_StmtAppend(ptr_t wn) {
  Is_True((WN *)wn != NULL, ("invalid wn"));
  Is_True(!_cur_func_body_wn.empty(),
          ("no wn in _cur_func_body_wn"));
  WN_INSERT_BlockLast(Wgen_StmtTop(), (WN *)wn);
}

static void
Wgen_StmtPop() {
  Is_True(!_cur_func_body_wn.empty(),
          ("no wn in _cur_func_body_wn"));
  _cur_func_body_wn.pop();
}

static void
Wgen_PushCurrentEntryWN(WN *wn) {
  Is_True(wn != NULL &&
          WN_operator(wn) == OPR_FUNC_ENTRY,
          ("should be OPR_FUNC_ENTRY"));
  _cur_entry_wn.push_back(wn);
}

static void
Wgen_PopCurrentEntryWN() {
  Is_True(!_cur_entry_wn.empty(),
          ("no wn in _cur_entry_wn"));
  _cur_entry_wn.pop_back();
}

static WN *
Wgen_CurrentEntryWN(void) {
  if (_cur_entry_wn.size()==0)
    return NULL;
  else
    return _cur_entry_wn.back();
}

void
Wgen_CreateFunction(id_t st_idx, int num_params, id_t parms[], id_t record_ty) {
  Is_True(st_idx != ST_IDX_ZERO, ("invalid st"));

  UINT i = 0;
  WN *pu_tree = NULL;
  // create entry
  WN *body = WN_CreateBlock();
  WN *pragma = WN_CreatePragma(WN_PRAGMA_PREAMBLE_END, (ST_IDX)0, 0, 0);
  WN_INSERT_BlockLast(body, pragma);
  // push current body wn
  Wgen_StmtPush(body);

  // get PU_Info
  PU_Info *pu_info = PU_Info_Table[CURRENT_SYMTAB];

  // get num of  parm
  pu_tree = WN_CreateEntry(num_params, st_idx, body, NULL, NULL);
  Wgen_PushCurrentEntryWN(pu_tree);
  Set_PU_Info_tree_ptr(pu_info, pu_tree);

  // create this ptr for method function
  ST *this_st = NULL;
  if (record_ty) {
    Is_True(record_ty != TY_IDX_ZERO &&
            Wgen_IsRecordType(record_ty),
            ("invalid record ty"));
    TY_IDX this_ty = TY_pointed(record_ty); 
    SYMTAB_IDX symtab = CURRENT_SYMTAB;
    Is_True(symtab > GLOBAL_SYMTAB,
            ("invalid scope for function param"));
    this_st = New_ST(symtab); 
    STR_IDX str_idx = Save_Str("this");
    ST_Init(this_st, str_idx, CLASS_VAR, SCLASS_FORMAL,
            EXPORT_LOCAL, Make_Pointer_Type(this_ty));
    Set_ST_is_value_parm(this_st);
    Set_ST_is_this_ptr(this_st);
    WN_formal(pu_tree, i) = WN_CreateIdname(0, this_st);
    i++;
    num_params++;
  }

  // set params
  for (i; i < num_params; ++i) {
    UINT real_parm = i;
    if (record_ty) real_parm--;
    ST_IDX parm_st = parms[real_parm];
    TY_IDX parm_ty = ST_type(parm_st);
    WN_formal(pu_tree, i) = WN_CreateIdname(0, parm_st);
  }

  Is_True(pu_tree != NULL,
          ("invalid pu tree"));
  WN_verifier(pu_tree);
}

ptr_t
Wgen_CreatePUInfo (id_t st_idx) {
  Is_True(st_idx != ST_IDX_ZERO, ("invalid st"));

  PU_Info *pu_info = TYPE_MEM_POOL_ALLOC(PU_Info, Malloc_Mem_Pool);
  BZERO(pu_info, sizeof(PU_Info));
  Is_True(pu_info != NULL,
          ("Malloc PU_Info failed. Out of memory?"));

  // set up the mem pool for the map table and predefined mappings
  if (!map_mempool_initialized) {
      MEM_POOL_Initialize(&_local_pool,"_local_pool",FALSE);
      map_mempool_initialized = TRUE;
  } else {
      MEM_POOL_Pop(&_local_pool);
  }
  MEM_POOL_Push(&_local_pool);

  // create the map table for the next PU
  WN_MAP_TAB_Create(&_local_pool);

  Is_True(Current_Map_Tab != NULL,
          ("Current_Map_Tab not initialized?"));

  PU_Info_init(pu_info);
  PU_Info_maptab(pu_info) = Current_Map_Tab;
  PU_Info_proc_sym (pu_info) = st_idx;

  Set_PU_Info_state (pu_info, WT_SYMTAB, Subsect_Missing);
  Set_PU_Info_state (pu_info, WT_TREE, Subsect_Missing);
  Set_PU_Info_state (pu_info, WT_DEPGRAPH, Subsect_Missing);
  Set_PU_Info_state (pu_info, WT_PREFETCH, Subsect_Missing);
  Set_PU_Info_state (pu_info, WT_REGIONS, Subsect_Missing);
  Set_PU_Info_state (pu_info, WT_FEEDBACK, Subsect_Missing);
  Set_PU_Info_state (pu_info, WT_FREQ, Subsect_Missing);
  Set_PU_Info_state (pu_info, WT_AC_INTERNAL, Subsect_Missing);
  Set_PU_Info_state (pu_info, WT_ALIAS_CLASS, Subsect_Missing);

  return (ptr_t)pu_info;
}

id_t
Wgen_GetThisST() {
  WN *first_formal = WN_formal(Wgen_CurrentEntryWN(), 0);
  Is_True(first_formal != NULL, ("invalid wn"));
  ST_IDX parm_st = WN_st_idx(first_formal);
  Is_True(!strcmp(ST_name(parm_st), "this"), ("invalid st"));
  return parm_st;
}

// ==================================================================
// Initv & Inito table related
// ==================================================================

// ==================================================================
// Whirl node related
// ==================================================================

// The following functions are copied from common/com/wn.cxx,
// Wgen_CreateMstore/Wgen_CreateIstore/Wgen_CreateStid/
// WN_CreateComma/WN_CreateRcomma/Wgen_CreateSwitch
// they may call Set_PU_has_very_high_whirl(Get_Current_PU());
// we cannot get Get_Current_PU() from fake main,
// so the very high flag will be handled at the end of Wgen_FinishFunction().
static WN *
Wgen_CreateMstore(WN_OFFSET offset, TY_IDX ty, WN *value,
                  WN *addr, WN *num_bytes)
{
  WN *wn;
  Is_True(MTYPE_is_pointer(WN_rtype(addr)) ||
          WN_rtype(addr)==MTYPE_U8 ||
          WN_rtype(addr)==MTYPE_U4 ||
          WN_rtype(addr)==MTYPE_I8 ||
          WN_rtype(addr)==MTYPE_I4,
          ("Bad addr in Wgen_CreateMstore"));
  Is_True(OPCODE_is_expression(WN_opcode(value)),
          ("Bad value in Wgen_CreateMstore"));
  Is_True(OPCODE_is_expression(WN_opcode(num_bytes)),
          ("Bad num_bytes in Wgen_CreateMstore"));

#if (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77)

  addr = fe_combine_address_offset(&offset, addr);

#endif /* (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77) */

  wn = WN_Create(OPC_MSTORE, 3);
  WN_kid0(wn) = value;
  WN_kid1(wn) = addr;
  WN_kid(wn,2) = num_bytes;
  WN_store_offset(wn) = offset;
  WN_set_ty(wn,ty);

#ifdef FRONT_END
  _pu_has_very_high_whirl = true;
#endif /* FRONT_END */
  return(wn);
}

static WN *
Wgen_CreateIstore(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
                 WN_OFFSET offset, TY_IDX ty, WN *value, WN *addr,
                 UINT field_id)
{
  OPCODE opc = OPCODE_make_op(opr, rtype, desc);
  WN *wn;
  Is_True(MTYPE_is_pointer(WN_rtype(addr)) ||
          WN_rtype(addr)==MTYPE_U8 ||
          WN_rtype(addr)==MTYPE_U4 ||
          WN_rtype(addr)==MTYPE_I8 ||
          WN_rtype(addr)==MTYPE_I4,
          ("Bad addr in Wgen_CreateIstore"));
  Is_True(OPCODE_is_expression(WN_opcode(value)),
          ("Bad value in Wgen_CreateIstore"));
  Is_True(Types_Are_Compatible(OPCODE_desc(opc),value),
          ("Bad return type in Wgen_CreateIstore"));
  Is_True(opr == OPR_ISTORE || opr == OPR_ISTBITS,
          ("Bad opcode in Wgen_CreateIstore"));
  Is_True(ty != TY_IDX_ZERO && TY_kind(ty) == KIND_POINTER,
          ("Bad type in Wgen_CreateIstore"));
  Is_True(field_id == 0 ||
          (TY_kind(TY_pointed(ty)) == KIND_STRUCT &&
           field_id < FLD_get_count(TY_pointed(ty))),
          ("Bad field in Wgen_CreateIstore"));
#ifdef FRONT_END
 if (desc == MTYPE_M) {
  _pu_has_very_high_whirl = true;
 }
#endif /* FRONT_END */

#if (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77)

  addr = fe_combine_address_offset(&offset, addr);

  UINT64 ty_size;
  if (field_id == 0) {
    ty_size = TY_size(TY_pointed(ty));
  }
  else {
    UINT tmp = 0;
    ty_size = TY_size(FLD_type(FLD_get_to_field(TY_pointed(ty),field_id,tmp)));
  }

  if (FE_Cvtl_Opt) {

    OPCODE value_opc;

    value_opc = WN_opcode(value);

    if (    (    (    WN_operator(value) == OPR_CVTL
                   || WN_operator(value) == OPR_CVT )
              && WN_cvtl_bits(value) == ty_size * 8 )
         || (    (    value_opc == OPC_I4I8CVT
                   || value_opc == OPC_I4U8CVT
                   || value_opc == OPC_U4I8CVT
                   || value_opc == OPC_U4U8CVT )
              && ty_size < 8 ) )
      value = WN_kid0(value);
  }

  if (    FE_Store_Opt
       && WN_operator(value) == OPR_BAND ) {

    UINT64   mask;
    WN     * kid0;
    WN     * kid1;

    mask = masks[ty_size];
    kid0 = WN_kid0(value);
    kid1 = WN_kid1(value);

    if (    WN_operator(kid0) == OPR_INTCONST
         && ( ( WN_const_val(kid0) & mask ) == mask ) )
      value = kid1;

    else
    if (    WN_operator(kid1) == OPR_INTCONST
         && ( ( WN_const_val(kid1) & mask ) == mask ) )
      value = kid0;
  }
#endif /* (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77) */

  wn = WN_SimplifyIstore(opc,offset,ty,field_id,value,addr);

  if (!wn) {
     wn = WN_Create(opc,2);
     WN_kid0(wn) = value;
     WN_kid1(wn) = addr;
     WN_store_offset(wn) = offset;
     WN_set_ty(wn, ty);
     WN_set_field_id(wn, field_id);
  }
  else {
    /* Parent pointer (if it exists) for returned node must be NULL */
    if (WN_SimpParentMap != WN_MAP_UNDEFINED) {
      WN_MAP_Set(WN_SimpParentMap, wn, NULL);
    }
  }

  return(wn);
}

/* Create a STID, note this needs the opcode since there are many */
/* opcodes with the OPR_STID operator */
static WN *
Wgen_CreateStid(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
               WN_OFFSET offset, ST* st, TY_IDX ty, WN *value, UINT field_id)
{
    OPCODE opc = OPCODE_make_op(opr, rtype, desc);
    WN *wn;
    ST_IDX st_idx = ST_st_idx(st);

    Is_True(OPCODE_is_expression(WN_opcode(value)),
            ("Bad value in Wgen_CreateStid"));
    Is_True(ty != TY_IDX_ZERO,
            ("Bad type in Wgen_CreateStid"));
    Is_True(field_id == 0 ||
            (TY_kind(ty) == KIND_STRUCT &&
             field_id < FLD_get_count(ty)),
            ("Bad field id in Wgen_CreateStid"));
#ifndef KEY     // g++ can create cases where the actual parameter to a
                // function is a ptr, but the formal parameter is a struct.
    Is_True(Types_Are_Compatible(OPCODE_desc(opc),value),
            ("Bad return type in Wgen_CreateStid"));
#endif
    Is_True(opr == OPR_STID || opr == OPR_STBITS,
            ("Bad opcode in Wgen_CreateStid"));
#ifdef FRONT_END
    Is_True(!((offset == 0) && (st == Int32_Preg ||
                                st == Int64_Preg ||
                                st == Float32_Preg ||
                                st == Float64_Preg)),
            ("Preg offset 0 in Wgen_CreateStid"));
#endif /* FRONT_END */

#ifdef FRONT_END
   if (desc == MTYPE_M && CURRENT_SYMTAB > GLOBAL_SYMTAB) {
     _pu_has_very_high_whirl = true;
   }
   Set_ST_is_modified(st);
#endif /* FRONT_END */

#if (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77)

    UINT64 ty_size;
    if (field_id == 0) {
      ty_size = TY_size(ty);
    }
    else {
      UINT tmp = 0;
      ty_size = TY_size(FLD_type(FLD_get_to_field(ty, field_id, tmp)));
    }

    if (FE_Cvtl_Opt && ST_class(st) != CLASS_PREG ) {

        OPCODE value_opc;

        value_opc = WN_opcode(value);

        if (    (    (    WN_operator(value) == OPR_CVTL
                       || WN_operator(value) == OPR_CVT )
                  && WN_cvtl_bits(value) == ty_size * 8 )
             || (    (    value_opc == OPC_I4I8CVT
                       || value_opc == OPC_I4U8CVT
                       || value_opc == OPC_U4I8CVT
                       || value_opc == OPC_U4U8CVT )
                  && ty_size < 8 ) )
            value = WN_kid0(value);
    }

    if (FE_Store_Opt && ST_class(st) != CLASS_PREG &&
        WN_operator(value) == OPR_BAND ) {

        UINT64   mask;
        WN     * kid0;
        WN     * kid1;

        mask = masks [ty_size];
        kid0 = WN_kid0(value);
        kid1 = WN_kid1(value);

        if (    WN_operator(kid0) == OPR_INTCONST
            && ( ( WN_const_val(kid0) & mask ) == mask ) )
            value = kid1;

        else
            if (    WN_operator(kid1) == OPR_INTCONST
                && ( ( WN_const_val(kid1) & mask ) == mask ) )
                value = kid0;
    }

#endif /* (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77) */
#ifdef TARG_NVISA
    if ((MTYPE_byte_size(desc) == 8 && MTYPE_byte_size(WN_rtype(value)) == 4)
     || (MTYPE_byte_size(desc) == 4 && MTYPE_byte_size(WN_rtype(value)) == 8))
    {
        // Rather than create I8STID(I4*),
        // create I8STID(I8I4CVT(I4*).
        // We need explicit conversion
        // because we use different registers for the two sizes,
        DevWarn("insert cvt of stid, %d %d", desc, WN_rtype(value));
        value = WN_Cvt(WN_rtype(value), desc, value);
    }
#endif

    wn = WN_Create(opc,1);
    WN_kid0(wn) = value;
    WN_store_offset(wn) = offset;
    WN_st_idx(wn) = st_idx;
    WN_set_ty(wn,ty);
    WN_set_field_id(wn, field_id);

    return(wn);
}

static WN *
Wgen_CreateComma(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
                   WN *block, WN *value)
{
  OPCODE opc = OPCODE_make_op(opr, rtype, desc);
  WN *wn;

  Is_True(OPCODE_is_expression(WN_opcode(value)),
          ("Bad value in Wgen_CreateComma"));
  Is_True(WN_opcode(block) == OPC_BLOCK,
          ("Bad block in Wgen_CreateComma"));
  wn = WN_Create(opr,rtype,desc,2);
  WN_kid0(wn) = block;
  WN_kid1(wn) = value;

#ifdef FRONT_END
  _pu_has_very_high_whirl = true;
#endif /* FRONT_END */

  return(wn);
}

static WN *
Wgen_CreateRcomma(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
                    WN *value, WN *block)
{
  OPCODE opc = OPCODE_make_op(opr, rtype, desc);
  WN *wn;

  Is_True(OPCODE_is_expression(WN_opcode(value)),
          ("Bad value in Wgen_CreateRcomma"));
  Is_True(WN_opcode(block) == OPC_BLOCK,
          ("Bad block in Wgen_CreateRcomma"));
  wn = WN_Create(opr,rtype,desc,2);
  WN_kid0(wn) = value;
  WN_kid1(wn) = block;

#ifdef FRONT_END
  _pu_has_very_high_whirl = true;
#endif /* FRONT_END */

  return(wn);
}

static WN *
Wgen_CreateSwitch(INT32 num_entries, WN *value,
                    WN *block, WN *deflt, INT32 last_label)
{
  WN *wn;

  Is_True(OPCODE_is_expression(WN_opcode(value)),
          ("Bad value in Wgen_CreateSwitch"));
  Is_True(WN_opcode(block) == OPC_BLOCK,
          ("Bad block in Wgen_CreateSwitch"));
  if (deflt) Is_True(WN_opcode(deflt) == OPC_GOTO,
          ("Bad deflt in Wgen_CreateSwitch"));
  if (deflt) {
    wn = WN_Create(OPC_SWITCH,3);
  } else {
    wn = WN_Create(OPC_SWITCH,2);
  }
  WN_switch_test(wn) = value;
  WN_switch_table(wn) = block;
  if (deflt) WN_switch_default(wn) = deflt;
  WN_num_entries(wn) = num_entries;
  WN_last_label(wn) = last_label;

#ifdef FRONT_END
  _pu_has_very_high_whirl = true;
#endif /* FRONT_END */

  return(wn);
}

static WN *
Wgen_CreateParm(TYPE_ID rtype, WN *parm, TY_IDX ty) {
#ifdef Is_True_On
  Is_True(ty != TY_IDX_ZERO, ("ty is 0"));
  if (rtype == MTYPE_M) {
    Is_True(WN_rtype(parm) == MTYPE_M, ("parm rtype is not M"));
    Is_True(TY_mtype(ty) == MTYPE_M, ("ty is not M"));
  }
  else {
    Is_True(WN_rtype(parm) != MTYPE_M, ("parm rtype is M"));
  }
#endif
  return WN_CreateParm(rtype, parm, ty, WN_PARM_BY_VALUE);
}

static WN *
Wgen_StidTemp(TY_IDX ty, WN *val, const char *name) {
  Is_True(val && OPERATOR_is_expression(WN_operator(val)),
          ("bad val"));
  ST *st = NULL;
  PREG_NUM ofst = 0;
  TYPE_ID mtype = TY_mtype(ty);
  if (mtype == MTYPE_M) {
    st = Gen_Temp_Symbol(ty, name ? name : ".anon.sym.");
    ofst = 0;
  }
  else {
    st = MTYPE_To_PREG(mtype);
    ofst = Create_Preg(mtype, name ? name : ".anon.preg");
  }
  return WN_Stid(mtype, ofst, st, ty, val);
}

static WN *
Wgen_CreateComma(TYPE_ID rtype, WN* blk, WN* ldid) {
#ifdef Is_True_On
  WN* last = WN_last(blk);
  if (OPERATOR_is_call(WN_operator(last))) {
    if (rtype == MTYPE_M) {
      Is_True((WN_rtype(last) == MTYPE_M || WN_rtype(last) == MTYPE_V)
              && WN_rtype(ldid) == MTYPE_M,
              ("call or ldid not return M"));
    }
    else {
      Is_True(WN_rtype(last) != MTYPE_M && WN_rtype(ldid) != MTYPE_M,
              ("call or ldid returns M"));
    }
  }
#endif
  if (WN_operator(ldid) == OPR_COMMA) {
    WN_INSERT_BlockLast(blk, WN_kid0(ldid));
    ldid = WN_kid1(ldid);
  }
  return Wgen_CreateComma(OPR_COMMA, rtype, MTYPE_V, blk, ldid);
}

static TYPE_ID
Widen_Mtype(TYPE_ID t) {
  if (MTYPE_is_m(t))
    return t;
  if (MTYPE_is_void(t) || t == MTYPE_BS) {
    Is_True(false, ("Widen_Mtype: for MTYPE_V or MTYPE_BS"));
    return t;
  }
  if (MTYPE_byte_size(t) >= 4)
    return t;
  return Mtype_TransferSize(MTYPE_I4, t);
}

// For the specified symbol, generate a sequence of stores
static void
Wgen_EmitStoreForPad(WN *addr_wn, UINT pad, UINT current_offset, WN *blk) {
  Is_True(addr_wn != NULL, ("invalid addr"));
  WN *zero_wn = WN_Intconst(MTYPE_U4, 0);
  WN *pad_wn = WN_Intconst(MTYPE_U4, pad);

  TY_IDX mstore_ty = Make_Pointer_Type(MTYPE_To_TY(MTYPE_U1)); // char *
  WN *ret = Wgen_CreateMstore(current_offset, mstore_ty, zero_wn, addr_wn, pad_wn);
  WN_INSERT_BlockLast(blk, ret);
}

static WN *
Wgen_CreateCselect(TYPE_ID rtype, WN *cond, WN *twn, WN *fwn) {
#ifdef Is_True_On
  Is_True(OPERATOR_is_expression(WN_operator(cond)), ("cond not expr"));
  Is_True(WN_rtype(cond) != MTYPE_M, ("cond returns M"));
  if (rtype == MTYPE_M) {
    Is_True(WN_rtype(twn) == MTYPE_M && WN_rtype(fwn) == MTYPE_M, ("kid not return M"));
  }
  else {
    Is_True(WN_rtype(twn) != MTYPE_M && WN_rtype(fwn) != MTYPE_M, ("kid returns M"));
  }
#endif
  return WN_CreateExp3(OPR_CSELECT, Mtype_comparison(rtype),
                       MTYPE_V, cond, twn, fwn);
}

// create tmp st
static ST *
Wgen_CreateTmpSym(TY_IDX tyi, STR_IDX strIdx) {
  ST *tmpst = New_ST(CURRENT_SYMTAB);
  ST_Init(tmpst, strIdx,
          CLASS_VAR,
          CURRENT_SYMTAB != GLOBAL_SYMTAB ? SCLASS_AUTO : SCLASS_FSTATIC,
          EXPORT_LOCAL, tyi);
  if (CURRENT_SYMTAB != GLOBAL_SYMTAB)
    Set_ST_is_temp_var(tmpst);
  return tmpst;
}

static WN *
Wgen_HandleExprForCopy(WN *blk, WN *expr, TY_IDX ty, const char *name) {
  Is_True(OPERATOR_is_expression(WN_operator(expr)),
          ("not expression"));
  if (WN_operator(expr) != OPR_LDID &&
      WN_operator(expr) != OPR_INTCONST &&
      WN_operator(expr) != OPR_LDA) {
    TYPE_ID dscty = TY_mtype(ty);
    TYPE_ID rty = WN_rtype(expr);
    ST    *st;
    UINT64 ofst = 0;
    if (rty == MTYPE_M) {
      st = Wgen_CreateTmpSym(ty, Save_Str(name ? name : ".safe.copy"));
    }
    else {
      ofst = Create_Preg(rty, name);
      st = MTYPE_To_PREG(rty);
    }
    WN *stid = WN_Stid(dscty, ofst, st, ty, expr);
    WN_INSERT_BlockLast(blk, stid);
    expr = WN_Ldid(dscty, ofst, st, ty);
  }
  return expr;
}

// create cvt if needed
static WN *
Wgen_CreateCvt(WN *wn, TYPE_ID mtyp) {
  Is_True(wn != NULL, ("invalid wn"));
  Is_True(mtyp != 0, ("invalid ty"));
  if (WN_rtype(wn) != mtyp)
    wn = WN_Type_Conversion(wn, mtyp);
  return wn;
}

// --------------------------------------------------------------------
// This function mimics FLD_get_to_field from common/com/symtab.cxx,
// but it also computes the offset of <field_id> within <ty_idx>
// We need this because FLD_ofst only gives the offset within the first
// enclosing struct.
// --------------------------------------------------------------------
static FLD_HANDLE
get_fld_and_offset(TY_IDX ty_idx, UINT field_id, UINT& cur_field_id, UINT64& offset) {
  Is_True(field_id != 0, ("field_id should not be zero in get_fld_and_offset()"));
  if(TY_kind(ty_idx) == KIND_POINTER) {
    ty_idx = TY_pointed(ty_idx);
  }
  Is_True(TY_kind(ty_idx) == KIND_STRUCT, ("expecting KIND_STRUCT"));
  FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty_idx));
  do {
    FLD_HANDLE fld(fld_iter);
    cur_field_id++;
    if (cur_field_id == field_id) {
      offset += FLD_ofst(fld);
      return fld;
    }
    if (TY_kind(FLD_type(fld)) == KIND_STRUCT &&
        TY_fld(FLD_type(fld)) != FLD_HANDLE()) {
      UINT64 nested_offset = offset + FLD_ofst(fld);
      fld = get_fld_and_offset(FLD_type(fld), field_id,
                             cur_field_id, nested_offset);
      if (cur_field_id == field_id) {
        offset = nested_offset;
        return fld;
      }
    }
  } while (!FLD_last_field(fld_iter++));
  return FLD_HANDLE();
}

static WN*
Wgen_GenerateIlda(WN *addr, INT64 ofst, TY_IDX ty, UINT field_id) {
  Is_True(WN_operator(addr) == OPR_LDID ||
          WN_operator(addr) == OPR_ILOAD,
          ("wrong candidate for ilda"));
  Is_True(TY_kind(ty) == KIND_POINTER &&
          TY_kind(TY_pointed(ty)) == KIND_STRUCT &&
          field_id < FLD_get_count(TY_pointed(ty)),
          ("wrong type"));
  WN *ilda = WN_CreateExp1(OPR_ILDA, Pointer_Mtype, MTYPE_V, addr);
  WN_load_offset(ilda) = ofst;
  if (WN_has_sym(addr))
    WN_st_idx(ilda) = WN_st_idx(addr);
  WN_set_ty(ilda, ty);
  WN_set_field_id(ilda, field_id);
  return ilda;
}

// ==================================================================
// public whirl node related functions
// ==================================================================

void
Wgen_StartFunction(id_t st_idx) {
  Is_True(st_idx != ST_IDX_ZERO, ("invalid st"));
  New_Scope(CURRENT_SYMTAB + 1, Malloc_Mem_Pool, TRUE);

  Scope_tab[Current_scope].st = ST_ptr(st_idx);

  Set_ST_sclass(ST_ptr(st_idx), SCLASS_TEXT);
  Set_PU_lexical_level(Pu_Table[ST_pu(ST_ptr(st_idx))], CURRENT_SYMTAB);

  // deallocate the old map table
  if (Current_Map_Tab) {
    WN_MAP_TAB_Delete(Current_Map_Tab);
    Current_Map_Tab = NULL;
  }

  PU_Info *pu_info = (PU_Info*)Wgen_CreatePUInfo(st_idx);

  if (PU_Info_Table[CURRENT_SYMTAB])
    PU_Info_next (PU_Info_Table[CURRENT_SYMTAB]) = pu_info;
  else if (CURRENT_SYMTAB == GLOBAL_SYMTAB + 1)
    _pu_root = pu_info;
  else
    PU_Info_child (PU_Info_Table[CURRENT_SYMTAB -1]) = pu_info;

  PU_Info_Table[CURRENT_SYMTAB] = pu_info;
}

void
Wgen_FinishFunction() {
  if (_pu_has_very_high_whirl)
    Set_PU_has_very_high_whirl(Get_Current_PU());

  PU_Info *pu_info = PU_Info_Table [CURRENT_SYMTAB];

  // Insert a RETURN if it does not exist
  WN *wn = WN_last(Wgen_StmtTop());
  if (wn == NULL || WN_operator(wn) != OPR_RETURN &&
      WN_operator(wn) != OPR_RETURN_VAL)
    Wgen_StmtAppend((ptr_t)WN_CreateReturn());

  Wgen_StmtPop();
  Wgen_PopCurrentEntryWN();

  // TODO: dst
  //Set_PU_Info_pu_dst(pu_info, func_dst);
  Set_PU_Info_state (pu_info, WT_SYMTAB, Subsect_InMem);
  Set_PU_Info_state (pu_info, WT_TREE, Subsect_InMem);

  // deallocate the old map table
  if (Current_Map_Tab) {
    WN_MAP_TAB_Delete(Current_Map_Tab);
    Current_Map_Tab = NULL;
  }

  Write_PU_Info(pu_info);

  PU_Info_Table[CURRENT_SYMTAB+1] = NULL;

  Delete_Scope(CURRENT_SYMTAB);
  --CURRENT_SYMTAB;
}

ptr_t
Wgen_GetCurrentPUInfo() {
  return (ptr_t)(PU_Info_Table[CURRENT_SYMTAB]);
}

ptr_t
Wgen_GenNullConst(id_t ty_idx) {
  Is_True(ty_idx != TY_IDX_ZERO, ("invalid ty"));
  TCON tcon;
  TYPE_ID mtype = TY_mtype(ty_idx);
  if (MTYPE_is_integral(mtype)) {
    if (mtype == MTYPE_I1 || mtype == MTYPE_I2)
      mtype = MTYPE_I4;
    else if (mtype == MTYPE_U1 || mtype == MTYPE_U2)
      mtype = MTYPE_U4;
    return (ptr_t)WN_Intconst(mtype, 0);
  } else if (MTYPE_is_complex(mtype)) {
    switch(TY_size(ty_idx)) {
      case 8:
        tcon = Host_To_Targ_Complex_4(MTYPE_C4, 0, 0);
        break;
      case 16:
        tcon = Host_To_Targ_Complex(MTYPE_C8, 0, 0);
        break;
      case 24:
      case 32:
        tcon = Host_To_Targ_Complex_10(MTYPE_C10, 0, 0);
        break;
      default:
        Is_True(false, ("unexpected size for complex constant"));
        break;
    }
  } else if (MTYPE_is_float(mtype)) {
    switch (TY_size(ty_idx)) {
    case 4:
      tcon = Host_To_Targ_Float_4(MTYPE_F4, 0);
      break;
    case 8:
      tcon = Host_To_Targ_Float(MTYPE_F8, 0);
      break;
    case 12:
    case 16:
      tcon = Host_To_Targ_Float_10(MTYPE_F10, 0);
      break;
    default:
      Is_True(false, ("unexpected size for intconstant"));
      break;
    }
  } else if (MTYPE_is_void(mtype)) {
    Is_True(false, ("unsupported void type in Wgen_GenNullConst"));
  } else
    Is_True(false, ("unsupported type in Wgen_GenNullConst"));

   ST *st = New_Const_Sym(Enter_tcon(tcon), ty_idx);
   return (ptr_t)WN_CreateConst(OPR_CONST, mtype, MTYPE_V, st);
}

// convert condition wn
static WN *
Wgen_HandleCondWN(WN *wn) {
  Is_True(wn != NULL, ("null whirl node"));
  if (WN_operator(wn) == OPR_COMMA) {
    WN_kid1(wn) = Wgen_HandleCondWN(WN_kid1(wn));
    return wn;
  }
  if (WN_operator(wn) == OPR_RCOMMA) {
    WN_kid0(wn) = Wgen_HandleCondWN(WN_kid0(wn));
    return wn;
  }
  if (WN_rtype(wn) == MTYPE_B || OPERATOR_is_boolean(WN_operator(wn)))
    return wn;
  TYPE_ID mtype = WN_rtype(wn);
  WN *ret = WN_NE(mtype, wn, (WN *)Wgen_GenNullConst(MTYPE_To_TY(mtype)));
  return ret;
}

ptr_t
Wgen_GetConst(id_t ty, id_t init) {
  Is_True(ty != TY_IDX_ZERO, ("invalid ty"));
  switch (TY_mtype(ty)) {
    case MTYPE_I1:
    case MTYPE_U1:
    case MTYPE_I4:
    case MTYPE_U4:
    case MTYPE_I8:
    case MTYPE_U8:
      return (ptr_t)WN_Intconst(Widen_Mtype(TY_mtype(ty)), init);
    case MTYPE_F8: {
      TCON tcon = Host_To_Targ_Float(MTYPE_F8, init);
      ST *st = New_Const_Sym(Enter_tcon(tcon), ty);
      return (ptr_t)WN_CreateConst(OPR_CONST, TY_mtype(ty), MTYPE_V, st);
    }
    case MTYPE_STR: {
      ST *st = New_Const_Sym(init, ty);
      return (ptr_t)WN_Lda(Pointer_Mtype, ST_ofst(st), st);
    }
    default:
      Is_True(FALSE, ("unsupported ty in Wgen_GetConst"));
  }
}

ptr_t
Wgen_GetRValue(id_t st_idx, id_t ofst, int field) {
  Is_True(st_idx != ST_IDX_ZERO, ("invalid st"));
  TY_IDX ty = Wgen_GetTyFromST(st_idx);
  if (TY_kind(ty) == KIND_FUNCTION)
    return (ptr_t)WN_Lda(Pointer_Mtype, ofst, ST_ptr(st_idx), field);
  else
    return (ptr_t)WN_Ldid(TY_mtype(ty), ofst, st_idx, ty, field);
}

ptr_t
Wgen_GetLValue(ptr_t base, id_t ty_idx, int field) {
  WN *base_wn = (WN *)base;
  Is_True(base_wn != NULL, ("invalid wn"));
  Is_True(ty_idx != TY_IDX_ZERO, ("invalid ty"));

  if (field) {
    Is_True(TY_kind(ty_idx) == KIND_STRUCT, ("should be struct ty"));
    UINT64 ofst = 0;
    UINT cur_field = 0;
    FLD_HANDLE fh = get_fld_and_offset(ty_idx, field, cur_field, ofst);
    Is_True(!fh.Is_Null(), ("could not find the field"));
    if (WN_operator(base_wn) == OPR_LDID || WN_operator(base_wn) == OPR_ILOAD)
      base_wn = Wgen_GenerateIlda(base_wn, ofst,
                                  Make_Pointer_Type(ty_idx), field);
    else
      base_wn = WN_Add(Pointer_Mtype, base_wn, WN_Intconst(Pointer_Mtype, ofst));
  }
  return (ptr_t)base_wn;
}

ptr_t
Wgen_CreateBlock() {
  return (ptr_t)WN_CreateBlock();
}

ptr_t
Wgen_InsertBlockLast(ptr_t blk, ptr_t wn) {
  WN *blk_wn = (WN *)blk;
  Is_True(blk_wn != NULL && WN_operator(blk_wn) == OPR_BLOCK,
          ("invalid blk in Wgen_InsertBlockLast"));
  Is_True((WN *)wn != NULL,
          ("invalid wn in Wgen_InsertBlockLast"));
  WN_INSERT_BlockLast(blk_wn, (WN *)wn);
}

bool
Wgen_IsNullBlock(ptr_t blk) {
  WN *blk_wn = (WN *)blk;
  Is_True(blk_wn != NULL && WN_operator(blk_wn) == OPR_BLOCK,
          ("invalid blk"));
  if (WN_last(blk_wn))
    return false;
  return true;
}

bool
Wgen_OpcodeIsExpression(ptr_t wn) {
  Is_True((WN *)wn != NULL, ("invalid wn"));
  if (OPCODE_is_expression(WN_opcode((WN *)wn)))
    return true;
  return false;
}

ptr_t
Wgen_CreateEval(ptr_t wn) {
  Is_True((WN *)wn != NULL, ("invalid wn"));
  return (ptr_t)WN_CreateEval((WN *)wn);
}

ptr_t
Wgen_CreateStringWN(cstr_t str, int length, cstr_t name) {
  // get type of char
  TY_IDX elem_ty = MTYPE_To_TY(MTYPE_I1);

  // get ARRAY type for string
  TY_IDX ty_idx = Wgen_CreateArrayTy(elem_ty, length, (char *)name);

  // generate tcon
  TCON tcon = Host_To_Targ_String(MTYPE_STRING, str, length);
  TCON_IDX tcon_idx = Enter_tcon(tcon);

  // generate const st
  ST *st = New_Const_Sym(tcon_idx, ty_idx);
  WN *wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
  Set_ST_addr_saved(st);

  return (ptr_t)wn;
}

ptr_t
Wgen_EmitStringInit(ptr_t len, ptr_t str, ptr_t dest, id_t ty_idx) {
  WN *len_wn = (WN *)len;
  WN *str_wn = (WN *)str;
  WN *dest_wn = (WN *)dest;
  Is_True(len_wn != NULL && str_wn != NULL && dest_wn != NULL,
          ("invalid WN for Wgen_GenStringInit"));

  Is_True(ty_idx != TY_IDX_ZERO && Wgen_IsArrayType(ty_idx),
          ("invalid ty for Wgen_GenStringInit"));
  TY_IDX struct_ty = TY_pointed(ty_idx);

  WN *init_blk = WN_CreateBlock();

  TY_IDX array_ty = WN_ty(str_wn);
  Is_True(TY_kind(array_ty) == KIND_POINTER &&
          TY_kind(TY_pointed(array_ty)) == KIND_ARRAY,
          ("should be pointer ty for KIND_ARRAY"));
  WN *load_wn = WN_CreateMload(0, array_ty, WN_COPY_Tree(str_wn),
                               len_wn);
  UINT ofst = TY_size(MTYPE_To_TY(WN_rtype((WN *)len_wn)));
  WN *store_wn = Wgen_CreateMstore(ofst, array_ty, load_wn,
                                   WN_COPY_Tree(dest_wn),
                                   WN_COPY_Tree(len_wn));
  WN_INSERT_BlockLast(init_blk, store_wn);

  return (ptr_t)init_blk;
}

ptr_t
Wgen_EmitRecordInit(id_t st_idx, id_t length, ptr_t inits[]) {
  Is_True(st_idx != ST_IDX_ZERO, ("invalid st"));
  ST *st = ST_ptr(st_idx);
  TY_IDX ty_idx = ST_type(st_idx);
  WN *base_wn = NULL;
  if (TY_kind(ty_idx) == KIND_STRUCT) {
    base_wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
  } else if (TY_kind(ty_idx) == KIND_POINTER &&
           TY_kind(TY_pointed(ty_idx)) == KIND_STRUCT) {
    base_wn = WN_Ldid(Pointer_Mtype, 0, st, ty_idx);
    ty_idx = TY_pointed(ty_idx);
  }

  Is_True(TY_kind(ty_idx) == KIND_STRUCT, ("expecting KIND_STRUCT"));
  FLD_HANDLE fld = TY_fld(ty_idx);

  WN *init_blk = WN_CreateBlock();
  TY_IDX pty = Make_Pointer_Type(ty_idx);

  // Emit record initialization for derived ty
  if (strncmp("_base.", FLD_name(fld), 6) == 0) {
    // need cast from Derived to Base
    base_wn = (WN*)Wgen_GetLValue((ptr_t)base_wn, ty_idx, 1/*fld_id*/);
    UINT j = 0;
    for (j; j < length; ++j) {
      WN *init_wn  = (WN *)inits[j];
      if (init_wn == NULL)
        continue;
      FLD_HANDLE fld = Wgen_GetFldByFieldId(pty, j + 1);
      Is_True(!fld.Is_Null(), ("invalid fld"));
      init_wn = Wgen_CreateCvt(init_wn, TY_mtype(FLD_type(fld)));
      WN *ret_wn = Wgen_CreateIstore(OPR_ISTORE, MTYPE_V, TY_mtype(FLD_type(fld)),
                                     FLD_ofst(fld), pty,
                                     init_wn, WN_COPY_Tree(base_wn), j);
      WN_INSERT_BlockLast(init_blk, ret_wn);
    }
    return (ptr_t)init_blk;
  }

  INT pad = 0;
  INT emitted_bytes = 0;
  TY_IDX fld_ty;
  UINT i = 0;
  for (i; i < length; ++i) {
    // emit pad if needed
    pad = FLD_ofst(fld) - emitted_bytes;
    if (pad > 0) {
      Wgen_EmitStoreForPad(WN_COPY_Tree(base_wn), pad,
                         FLD_ofst(fld) - pad, init_blk);
      emitted_bytes += pad;
    }
    // get init wn
    WN *init_wn  = (WN *)inits[i];
    fld_ty = FLD_type(fld);
    init_wn = Wgen_CreateCvt(init_wn, TY_mtype(fld_ty));
    UINT field_id = i + 1;
    WN *ret_wn = Wgen_CreateIstore(OPR_ISTORE, MTYPE_V, TY_mtype(fld_ty),
                                   FLD_ofst(fld), pty,
                                   init_wn, WN_COPY_Tree(base_wn), field_id);
    WN_INSERT_BlockLast(init_blk, ret_wn);
    emitted_bytes += TY_size(fld_ty);
    fld = FLD_next(fld);
  }
  pad = TY_size(ty_idx) - emitted_bytes;
  if (pad > 0)
    Wgen_EmitStoreForPad(WN_COPY_Tree(base_wn), pad,
                       emitted_bytes, init_blk);

  return (ptr_t)init_blk;
}

// get field id from union ty
static UINT
Wgen_GetFieldForUnion(TY_IDX ty_idx, TY_IDX elem_ty) {
  Is_True(ty_idx != TY_IDX_ZERO && elem_ty != TY_IDX_ZERO,
          ("invalid ty"));
  Is_True(TY_kind(ty_idx) == KIND_STRUCT, ("expecting KIND_STRUCT"));

  UINT field_id = 0;
  FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty_idx));
  do {
    FLD_HANDLE fld(fld_iter);
    field_id++;
    TY_IDX fld_ty = FLD_type(fld);
    if (fld_ty == elem_ty ||
        Widen_Mtype(TY_mtype(fld_ty)) == TY_mtype(elem_ty))
      return field_id;
  } while (!FLD_last_field(fld_iter++));
  return 0;
}

static WN *
Wgen_ConvertToRValue(WN *wn, TY_IDX ty) {
  Is_True(wn != NULL, ("invalid wn"));
  Is_True(ty != TY_IDX_ZERO, ("invalid ty"));

  TYPE_ID desc = TY_mtype(ty);
  TYPE_ID rtype = Mtype_comparison(desc);
  // generate iload
  return WN_CreateIload(OPR_ILOAD, rtype, desc,
                        0/*ofst*/, ty, Make_Pointer_Type(ty),
                        wn, 0/*field_id*/);
}

// handle implicit cast
static WN *
Wgen_HandleImplicitCast(WN *wn, TY_IDX to_ty) {
  Is_True(wn != NULL, ("invalid wn"));
  Is_True(to_ty != TY_IDX_ZERO, ("invalid ty"));
  if (!TY_is_union(to_ty)) {
    // get rvalue
    if (TY_kind(to_ty) == KIND_STRUCT &&
        WN_rtype((WN *)wn) != MTYPE_M) {
      TYPE_ID desc = TY_mtype(to_ty);
      TYPE_ID rtype = Mtype_comparison(desc);
      // generate iload
      wn = WN_CreateIload(OPR_ILOAD, rtype, desc,
                          0/*ofst*/, to_ty, Make_Pointer_Type(to_ty),
                          wn, 0/*field_id*/);
    } else {
      // check if wn need conversion
      wn = Wgen_CreateCvt(wn, TY_mtype(to_ty));
    }
  }
  return wn;
}

ptr_t
Wgen_Stid(id_t st_idx, ptr_t init, id_t field_id = 0) {
  Is_True(st_idx != ST_IDX_ZERO, ("invalid st"));
  WN *init_wn = (WN *)init;
  Is_True(init_wn != NULL, ("invalid wn"));

  TY_IDX ty_idx = ST_type(st_idx);
  // get field id
  if (TY_kind(ty_idx) == KIND_STRUCT && TY_is_union(ty_idx)) {
    // get field id from ty
    TY_IDX elem_ty = Wgen_GetTyFromWN((ptr_t)init_wn);
    field_id = Wgen_GetFieldForUnion(ty_idx, elem_ty);
  }

  // need cast if necessary
  init_wn = Wgen_HandleImplicitCast(init_wn, ty_idx);

  TYPE_ID mtype = TY_mtype(ty_idx);
  WN *ret = Wgen_CreateStid(OPR_STID, MTYPE_V, mtype,
                            0/*ofst*/, ST_ptr((ST_IDX)st_idx),
                            ty_idx, init_wn, field_id);
  return (ptr_t)ret;
}

ptr_t
Wgen_Istore(id_t ty_idx, ptr_t addr, ptr_t init, id_t field_id = 0) {
  Is_True(ty_idx != TY_IDX_ZERO, ("invalid ty"));
  WN *addr_wn = (WN *)addr;
  WN *init_wn = (WN *)init;
  Is_True(addr_wn != NULL && init_wn != NULL,
          ("invalid addr_wn or init_wn"));

  TYPE_ID mtype = TY_mtype(ty_idx);
  TY_IDX pty = Make_Pointer_Type(ty_idx);
  UINT64 offset = 0;

  // get mtype & offset if field_id != 0
  if (field_id != 0) {
    IDTYPE fld = 0;
    FLD_HANDLE fh = get_fld_and_offset(TY_pointed(ty_idx), field_id, fld, offset);
    Is_True(!fh.Is_Null(), ("not find the field"));
    mtype = TY_mtype(FLD_type(fh));
    pty = ty_idx;
  }

  if (TY_is_union(ty_idx))
    mtype = WN_rtype(init_wn);
  else
    // need cast if necessary
    init_wn = Wgen_HandleImplicitCast(init_wn, MTYPE_To_TY(mtype));

  WN *ret = Wgen_CreateIstore(OPR_ISTORE, MTYPE_V, mtype, offset, pty,
                              init_wn, WN_COPY_Tree(addr_wn), field_id);
  return (ptr_t)ret;
}

// ==================================================================
// handle expression related functions
// ==================================================================

typedef enum {
  AddToken = 1,
  SubToken,
  MultiplyToken,
  DivideToken,
  ModuloToken,
  LeftShiftToken,
  RightShiftToken,
  UnsignedRightShiftToken,
  BitwiseANDToken,
  BitwiseOrToken,
  BitwiseXorToken,
  EqualsToken,
  NotEqualsToken,
  LessThanToken,
  LessThanEqualsToken,
  GreaterThanToken,
  GreaterThanEqualsToken,
  LogicalAndToken,
  LogicalOrToken,
  PowToken,
} BINARY_OPERATOR;

// check whether the WHIRL operator has subsumed cvtl in its semantics
// (intended only for integer operations)
static bool
Wgen_HasSubsumedCvtl(OPERATOR opr)
{
  if (OPERATOR_is_load(opr) || OPERATOR_is_leaf(opr))
    return TRUE;
  if (opr == OPR_CVT || opr == OPR_CVTL || opr == OPR_TAS)
    return TRUE;
  if (opr == OPR_EQ || opr == OPR_NE ||
      opr == OPR_GE || opr == OPR_GT ||
      opr == OPR_LE || opr == OPR_LT ||
      opr == OPR_LNOT || opr == OPR_LAND || opr == OPR_LIOR ||
      opr == OPR_CAND || opr == OPR_CIOR)
    return TRUE;
  return FALSE;
}

// generate wn for BO_LT/BO_LE/BO_GT/BO_GE/BO_EQ/BO_NE
// and check if conversion is needed
static WN *
Wgen_GenWNForCmp(OPERATOR opr, TYPE_ID mtyp, TYPE_ID lhs_mtyp, TYPE_ID rhs_mtyp,
               WN *lhs, WN *rhs) {
  WN *ret = NULL;

  if (MTYPE_size_min(rhs_mtyp) > MTYPE_size_min(lhs_mtyp) &&
      ! Wgen_HasSubsumedCvtl(WN_operator(lhs)))
    lhs = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(lhs_mtyp), MTYPE_V,
                        MTYPE_size_min(lhs_mtyp), lhs);
  if (MTYPE_size_min(lhs_mtyp) > MTYPE_size_min(rhs_mtyp) &&
      ! Wgen_HasSubsumedCvtl(WN_operator(rhs)))
    rhs = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(rhs_mtyp), MTYPE_V,
                        MTYPE_size_min(rhs_mtyp), rhs);

  if (MTYPE_is_integral(mtyp) && MTYPE_is_integral(lhs_mtyp) &&
      MTYPE_size_min(mtyp) > MTYPE_size_min(lhs_mtyp))
    lhs = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(lhs_mtyp), MTYPE_V,
                        MTYPE_size_min(lhs_mtyp), lhs);

  if (MTYPE_is_integral(mtyp) && MTYPE_is_integral(rhs_mtyp) &&
      MTYPE_size_min(mtyp) > MTYPE_size_min(rhs_mtyp))
    rhs = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(rhs_mtyp), MTYPE_V,
                        MTYPE_size_min(rhs_mtyp), rhs);

  ret = WN_CreateExp2(opr, Widen_Mtype(mtyp),
                      Widen_Mtype(lhs_mtyp), lhs, rhs);
  return ret;
}

// create function symbol from varargs
static ST_IDX
Wgen_CreateFunctionSymbol(const char *name, TY_IDX ret_ty, ...) {
  // create ty
  TY_IDX ty_idx;
  TY &func = New_TY(ty_idx);
  TY_Init(func, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
  Set_TY_align(ty_idx, 1);

  // set return type
  TYLIST tylist_idx;
  Set_TYLIST_type(New_TYLIST(tylist_idx), ret_ty);
  Set_TY_tylist(ty_idx, tylist_idx);

  // set parm type
  va_list ap;
  va_start(ap, ret_ty);
  TY_IDX parm_ty;
  do {
    parm_ty = va_arg(ap, TY_IDX);
    Set_TYLIST_type(New_TYLIST(tylist_idx), parm_ty);
  } while (parm_ty != TY_IDX_ZERO);
  va_end(ap);

  // create pu & st
  PU_IDX pu_idx;
  PU &pu = New_PU(pu_idx);
  TY_IDX pu_ty_idx = ty_idx;
  PU_Init(pu, pu_ty_idx, GLOBAL_SYMTAB + 1);
  ty_idx = (TY_IDX) pu_idx;
  ST *st = New_ST(GLOBAL_SYMTAB);
  ST_Init (st, Save_Str(name), CLASS_FUNC,
           SCLASS_EXTERN, EXPORT_PREEMPTIBLE, ty_idx);
  return ST_st_idx(st);
}

ptr_t
Wgen_ConvertBinaryExpr(id_t kind, id_t ty_idx, ptr_t lhs, ptr_t rhs) {
  Is_True(kind, ("invalid kind in Wgen_ConvertBinaryExpr"));
  Is_True(ty_idx != TY_IDX_ZERO, ("invalid ty in Wgen_ConvertBinaryExpr"));

  WN *ret = NULL;
  TYPE_ID mtyp = TY_mtype(ty_idx);
  WN *lhs_wn = (WN *)lhs;
  WN *rhs_wn = (WN *)rhs;
  Is_True(lhs_wn != NULL && rhs_wn != NULL,
          ("invalid wn in Wgen_ConvertBinaryExpr"));

  BINARY_OPERATOR opcode = (BINARY_OPERATOR)kind;
  OPERATOR opc;
  switch (opcode) {
    case AddToken:
      lhs_wn = Wgen_CreateCvt(lhs_wn, mtyp);
      rhs_wn = Wgen_CreateCvt(rhs_wn, mtyp);
      ret = WN_Add(mtyp, lhs_wn, rhs_wn);
      break;
    case SubToken:
      lhs_wn = Wgen_CreateCvt(lhs_wn, mtyp);
      rhs_wn = Wgen_CreateCvt(rhs_wn, mtyp);
      ret = WN_Sub(mtyp, lhs_wn, rhs_wn);
      break;
    case MultiplyToken:
      lhs_wn = Wgen_CreateCvt(lhs_wn, mtyp);
      rhs_wn = Wgen_CreateCvt(rhs_wn, mtyp);
      ret = WN_Mpy(mtyp, lhs_wn, rhs_wn);
      break;
    case DivideToken:
      lhs_wn = Wgen_CreateCvt(lhs_wn, mtyp);
      rhs_wn = Wgen_CreateCvt(rhs_wn, mtyp);
      ret = WN_Div(mtyp, lhs_wn, rhs_wn);
      break;
    case ModuloToken:
      lhs_wn = Wgen_CreateCvt(lhs_wn, mtyp);
      rhs_wn = Wgen_CreateCvt(rhs_wn, mtyp);
      ret = WN_Binary(OPR_REM, mtyp, lhs_wn, rhs_wn);
      break;
    case LeftShiftToken:
      lhs_wn = Wgen_CreateCvt(lhs_wn, mtyp);
      rhs_wn = Wgen_CreateCvt(rhs_wn, mtyp);
      ret = WN_Shl(mtyp, lhs_wn, rhs_wn);
      break;
    case RightShiftToken:
      lhs_wn = Wgen_CreateCvt(lhs_wn, mtyp);
      rhs_wn = Wgen_CreateCvt(rhs_wn, mtyp);
      ret = WN_Ashr(mtyp, lhs_wn, rhs_wn);
      break;
    case UnsignedRightShiftToken:
      lhs_wn = Wgen_CreateCvt(lhs_wn, mtyp);
      rhs_wn = Wgen_CreateCvt(rhs_wn, mtyp);
      ret = WN_Lshr(mtyp, lhs_wn, rhs_wn);
      break;
    case BitwiseANDToken:
      lhs_wn = Wgen_CreateCvt(lhs_wn, mtyp);
      rhs_wn = Wgen_CreateCvt(rhs_wn, mtyp);
      ret = WN_Band(mtyp, lhs_wn, rhs_wn);
      break;
    case BitwiseOrToken:
      lhs_wn = Wgen_CreateCvt(lhs_wn, mtyp);
      rhs_wn = Wgen_CreateCvt(rhs_wn, mtyp);
      ret = WN_Bior(mtyp, lhs_wn, rhs_wn);
      break;
    case BitwiseXorToken:
      lhs_wn = Wgen_CreateCvt(lhs_wn, mtyp);
      rhs_wn = Wgen_CreateCvt(rhs_wn, mtyp);
      ret = WN_Bxor(mtyp, lhs_wn, rhs_wn);
      break;
    case EqualsToken:
      ret = Wgen_GenWNForCmp(OPR_EQ, MTYPE_I4,
                             WN_rtype(lhs_wn), WN_rtype(rhs_wn),
                             lhs_wn, rhs_wn);
     break;
    case NotEqualsToken:
      ret = Wgen_GenWNForCmp(OPR_NE, MTYPE_I4,
                             WN_rtype(lhs_wn), WN_rtype(rhs_wn),
                             lhs_wn, rhs_wn);
      break;
    case LessThanToken:
      ret = Wgen_GenWNForCmp(OPR_LT, MTYPE_I4,
                             WN_rtype(lhs_wn), WN_rtype(rhs_wn),
                             lhs_wn, rhs_wn);
      break;
    case LessThanEqualsToken:
      ret = Wgen_GenWNForCmp(OPR_LE, MTYPE_I4,
                             WN_rtype(lhs_wn), WN_rtype(rhs_wn),
                             lhs_wn, rhs_wn);
      break;
    case GreaterThanToken:
      ret = Wgen_GenWNForCmp(OPR_GT, MTYPE_I4,
                             WN_rtype(lhs_wn), WN_rtype(rhs_wn),
                             lhs_wn, rhs_wn);
      break;
    case GreaterThanEqualsToken:
      ret = Wgen_GenWNForCmp(OPR_GE, MTYPE_I4,
                             WN_rtype(lhs_wn), WN_rtype(rhs_wn),
                             lhs_wn, rhs_wn);
      break;
    case LogicalAndToken:
    case LogicalOrToken:
    {
      lhs_wn = Wgen_HandleCondWN(lhs_wn);
      rhs_wn = Wgen_HandleCondWN(rhs_wn);
      if (opcode == LogicalAndToken)
        ret = WN_CAND(lhs_wn, rhs_wn);
      else
        ret = WN_CIOR(lhs_wn, rhs_wn);
      if (WN_operator(ret) != OPR_CAND &&
          WN_operator(ret) != OPR_CIOR) {
        TYPE_ID ret_mtyp = WN_rtype(ret);
        ret = WN_NE(ret_mtyp, ret,
                    (WN *)Wgen_GenNullConst(MTYPE_To_TY(ret_mtyp)));
      }
      break;
    }
    case PowToken:
    {
      ST_IDX pow_st = Wgen_CreateFunctionSymbol("pow",
                                                MTYPE_To_TY(MTYPE_F8),
                                                MTYPE_To_TY(MTYPE_F8),
                                                MTYPE_To_TY(MTYPE_F8),
                                                TY_IDX_ZERO);
      TY_IDX args_ty[2] = {MTYPE_To_TY(WN_rtype(lhs_wn)),
                           MTYPE_To_TY(WN_rtype(rhs_wn))};
      ptr_t args_wn[2] = {lhs, rhs};
      ret = (WN *)Wgen_ConvertCallExpr(pow_st, 2/* length */,
                                       args_ty, args_wn, TRUE/* retv */);
      break;
    }
    default:
      Is_True(FALSE, ("unsupported opcode in Wgen_ConvertBinaryExpr"));
  }
  return (ptr_t)ret;
}

ptr_t
Wgen_ConvertCompoundAssignOperator(id_t kind, id_t ty_idx, id_t st_idx,
                                   ptr_t lhs, ptr_t rhs) {
  Is_True(kind, ("invalid kind in Wgen_ConvertCompoundAssignOperator"));
  Is_True(ty_idx != TY_IDX_ZERO, ("invalid ty"));
  Is_True(st_idx != ST_IDX_ZERO, ("invalid st"));
  Is_True((WN *)lhs != NULL && (WN *)rhs != NULL, ("invalid lhs wn or rhs wn"));

  WN *base_wn = (WN *)Wgen_ConvertBinaryExpr(kind, ty_idx, lhs, rhs);
  Is_True(base_wn != NULL, ("invalid base wn"));
  // TODO: ISTORE?
  WN *ret = WN_Stid(TY_mtype(ty_idx), 0/*offset*/, ST_ptr(st_idx), ty_idx, base_wn);
  return (ptr_t)ret;
}

typedef enum {
  PlusToken = 1,
  MinusToken,
  LNotToken,
  NotToken,
} UNARY_OPERATOR;

ptr_t
Wgen_ConvertUnaryExpr(id_t kind, ptr_t sub) {
  Is_True(kind, ("invalid kind in Wgen_ConvertUnaryExpr"));
  WN *sub_wn = (WN *)sub;
  Is_True(sub_wn != NULL, ("invalid sub wn"));

  WN *ret;
  UNARY_OPERATOR opcode = (UNARY_OPERATOR)kind;
  switch (opcode) {
    case PlusToken:
      ret = sub_wn;
      break;
    case MinusToken:
      ret = WN_Unary(OPR_NEG, WN_rtype(sub_wn), sub_wn);
      break;
    case LNotToken:
      ret = WN_EQ(MTYPE_I4, Wgen_HandleCondWN(sub_wn),
                  WN_Intconst(MTYPE_I4, 0));
      break;
    case NotToken:
      ret = WN_Unary(OPR_BNOT, WN_rtype(sub_wn), sub_wn);
      break;
    default:
      Is_True(FALSE, ("unsupported opcode in Wgen_ConvertUnaryExpr"));
  }
  return (ptr_t)ret;
}

typedef enum {
  ArrayConcat = 1,
} INTRINSIC_OP;

// get INTRINSIC from INTRINSIC_OP
static INTRINSIC GetIntrinsic(INTRINSIC_OP intrn) {
  switch (intrn) {
    case ArrayConcat: return INTRN_TS_ARRAYCONCAT;
    default:
      Is_True(FALSE, ("unsupported intrinsic: %s", intrn));
  }
}

ptr_t
Wgen_GenerateIntrinsicCall(id_t intrinsic, id_t ret_ty,
                           id_t args_num, ptr_t args_wn[], bool retv) {
  WN *kids[MAX_PARM_CNT];
  // get param WNs
  for (int i = 0; i < args_num; ++i)
    kids[i] = (WN *)args_wn[i];

  INTRINSIC iopc = GetIntrinsic((INTRINSIC_OP)intrinsic);
  WN *intrn_wn = WN_Create_Intrinsic(OPR_INTRINSIC_CALL,
                                     Mtype_comparison(TY_mtype(ret_ty)), MTYPE_V,
                                     iopc, args_num, kids);
  if (TY_mtype(ret_ty) != MTYPE_V && retv) {
    WN *blk = WN_CreateBlock();
    WN_INSERT_BlockLast(blk, intrn_wn);

    WN *ldid = WN_Ldid(TY_mtype(ret_ty), -1, Return_Val_Preg, ret_ty);
    WN *stid = Wgen_StidTemp(ret_ty, ldid, "intrn.ret");
    WN_INSERT_BlockLast(blk, stid);

    ldid = WN_Ldid(TY_mtype(ret_ty), WN_offset(stid), WN_st(stid), ret_ty);
    intrn_wn = Wgen_CreateComma(Mtype_comparison(TY_mtype(ret_ty)), blk, ldid);
  }
  return (ptr_t)intrn_wn;
}


ptr_t
Wgen_ConvertCallExpr(id_t st_idx, id_t length, id_t args_ty[],
                     ptr_t args_wn[], bool retv) {
  Is_True(st_idx != ST_IDX_ZERO, ("invalid st"));
  bool indirect_call = false;
  if (St_Table[st_idx].sym_class != CLASS_FUNC)
    indirect_call = true;

  WN *call_wn = NULL;
  WN *ld_wn = NULL;
  TY_IDX ret_ty_idx;
  if (indirect_call) {
    ld_wn = (WN *)Wgen_GetRValue(st_idx, 0, 0);
    Is_True(OPERATOR_is_expression(WN_operator(ld_wn)) &&
            WN_rtype(ld_wn) == Pointer_Mtype, ("invalid wn"));
    TY_IDX ty = ST_type(st_idx);
    Is_True(TY_kind(ty) == KIND_POINTER &&
            TY_kind(TY_pointed(ty)) == KIND_FUNCTION,
            ("invalid ty"));
    TY_IDX pu_type = TY_pointed(ty);
    ret_ty_idx = TY_ret_type(pu_type);
    call_wn = WN_Icall(retv ? TY_mtype(ret_ty_idx) : MTYPE_V, MTYPE_V,
                       length + 1, pu_type);
  } else {
    // get right return type from callee type
    TY_IDX pu_type = ST_pu_type(st_idx);
    ret_ty_idx = TY_ret_type(pu_type);;
    call_wn = WN_Create(OPR_CALL, TY_mtype(ret_ty_idx), MTYPE_V, length);
    WN_st_idx(call_wn) = st_idx;
  }

  Is_True(call_wn != NULL, ("missing call whirl node"));
  WN_Set_Call_Default_Flags(call_wn);

  // set call parm
  int parm_idx = 0;
  for (int n = 0; n < length; ++n) {
    WN *arg_wn = (WN *)args_wn[n];
    Is_True(arg_wn != NULL, ("invalid args_wn"));
    TY_IDX arg_ty_idx = args_ty[n];
    Is_True(arg_ty_idx != TY_IDX_ZERO, ("invalid arg_ty_idx"));

    // need cast if necessary
    arg_wn = Wgen_HandleImplicitCast(arg_wn, arg_ty_idx);

    WN *parm = Wgen_CreateParm(Mtype_comparison(TY_mtype(arg_ty_idx)),
                               arg_wn, arg_ty_idx);
    WN_kid(call_wn, parm_idx++) = parm;
  }

  // set last kid for indirect call wn
  if (indirect_call) {
    WN_kid(call_wn, parm_idx) = ld_wn;
  }

  if (retv && TY_mtype(ret_ty_idx) != MTYPE_V) {
    WN *blk = WN_CreateBlock();
    WN_INSERT_BlockLast(blk, call_wn);
    WN *ret = WN_Ldid(TY_mtype(ret_ty_idx), -1, Return_Val_Preg, ret_ty_idx);
    WN *comma = Wgen_CreateComma(WN_rtype(ret), blk, ret);
    return (ptr_t)comma;
  } else
    return (ptr_t)call_wn;
}

ptr_t
Wgen_ConvertICallExpr(id_t func_ty, id_t length, id_t args_ty[], ptr_t args_wn[], id_t offset, bool retv) {
  Is_True(TY_kind(func_ty) == KIND_FUNCTION, ("invalid func ty"));
  TY_IDX ret_ty_idx = TY_ret_type(func_ty);

  WN *icall_wn = WN_Icall(TY_mtype(ret_ty_idx), MTYPE_V, length + 1, func_ty);

  // set call parm
  int parm_idx = 0;
  for (int n = 0; n < length; ++n) {
    WN *arg_wn = (WN *)args_wn[n];
    Is_True(arg_wn != NULL, ("invalid args_wn"));
    TY_IDX arg_ty_idx = args_ty[n];
    Is_True(arg_ty_idx != TY_IDX_ZERO, ("invalid arg_ty_idx"));

    // need cast if necessary
    arg_wn = Wgen_HandleImplicitCast(arg_wn, arg_ty_idx);

    WN *parm = Wgen_CreateParm(Mtype_comparison(TY_mtype(arg_ty_idx)),
                               arg_wn, arg_ty_idx);
    WN_kid(icall_wn, parm_idx++) = parm;
  }

  WN *ilod_vptr_wn = WN_Iload(Pointer_Mtype, 0, args_ty[0], (WN *)args_wn[0], 0);
  WN *ilod_func_ptr_wn = WN_Iload(Pointer_Mtype, offset, func_ty, ilod_vptr_wn, 0);
  WN_kid(icall_wn, length) = ilod_func_ptr_wn;

  if (retv && TY_mtype(ret_ty_idx) != MTYPE_V) {
    WN *blk = WN_CreateBlock();
    WN_INSERT_BlockLast(blk, icall_wn);
    WN *ret = WN_Ldid(TY_mtype(ret_ty_idx), -1, Return_Val_Preg, ret_ty_idx);
    WN *comma = Wgen_CreateComma(WN_rtype(ret), blk, ret);
    return (ptr_t)comma;
  } else
    return (ptr_t)icall_wn;
}

ptr_t
Wgen_ConvertConditionalExpr(id_t ty_idx, ptr_t cond,
                            ptr_t true_wn, ptr_t false_wn, bool retv) {
  Is_True(ty_idx != TY_IDX_ZERO, ("invalid ty"));

  WN *cond_wn = (WN *)cond;
  WN *true_real_wn = (WN *)true_wn;
  WN *false_real_wn = (WN *)false_wn;
  Is_True(cond_wn != NULL, ("invalid cond_wn"));
  Is_True(true_real_wn != NULL, ("invalid true wn"));
  Is_True(false_real_wn != NULL, ("invalid false wn"));
  WN *ret = NULL;

  // convert cond
  cond_wn = Wgen_HandleCondWN(cond_wn);
  Is_True(cond_wn != NULL &&
          OPCODE_is_expression(WN_opcode(cond_wn)) &&
          MTYPE_is_integral(WN_rtype(cond_wn)), ("bad cond_wn"));

  // special handle of INTCONST
  if (WN_operator(cond_wn) == OPR_INTCONST) {
    ret = WN_const_val(cond_wn) ? true_real_wn : false_real_wn;
  } else if (TY_mtype(ty_idx) != MTYPE_V) {
    ret = Wgen_CreateCselect(TY_mtype(ty_idx), cond_wn, true_real_wn, false_real_wn);
  } else {
    // generate if...then...else
    if (OPERATOR_is_expression(WN_operator(true_real_wn))) {
      true_real_wn = WN_CreateEval(true_real_wn);
    }
    if (OPERATOR_is_expression(WN_operator(false_real_wn))) {
      false_real_wn = WN_CreateEval(false_real_wn);
    }

    Is_True(!OPCODE_is_expression(WN_opcode(true_real_wn)) &&
            !OPCODE_is_expression(WN_opcode(false_real_wn)), ("bad stmt"));
    WN *then_blk = WN_CreateBlock();
    WN_INSERT_BlockLast(then_blk, true_real_wn);
    WN *else_blk = WN_CreateBlock();
    WN_INSERT_BlockLast(else_blk, false_real_wn);
    ret = WN_CreateIf(cond_wn, then_blk, else_blk);
  }
  Is_True(TY_mtype(ty_idx) == MTYPE_V || !retv ||
          OPERATOR_is_expression(WN_operator(ret)),
          ("bad return expr"));

  if (!retv) {
    WN *blk = WN_CreateBlock();
    if (OPERATOR_is_expression(WN_operator(ret))) {
      ret = WN_CreateEval(ret);
    }
    WN_INSERT_BlockLast(blk, ret);
    return (ptr_t)0;
  }

  return (ptr_t)ret;
}

ptr_t
Wgen_ConvertMemberExpr(ptr_t base, ptr_t index) {
  WN *base_wn = (WN *)base;
  Is_True(base_wn != NULL, ("invalid wn"));
  TY_IDX base_ty = TY_IDX_ZERO;
  if (WN_operator(base_wn) == OPR_LDID)
    base_ty = WN_type(base_wn);
  else if (WN_operator(base_wn) == OPR_ILOAD)
    base_ty = WN_load_addr_ty(base_wn);
  else
    Is_True(false, ("unsupported base wn"));
  Is_True(base_ty != TY_IDX_ZERO &&
          Wgen_IsRecordType(base_ty), ("invalid ty for record"));

  WN *index_wn = (WN *)index;
  Is_True(index_wn != NULL, ("invalid index_wn"));

  WN *ret_wn = NULL;
  TY_IDX struct_ty = TY_pointed(base_ty);
  if (WN_operator(index_wn) == OPR_INTCONST) {
    UINT field_id = WN_const_val(index_wn) + 1;
    IDTYPE fld = 0;
    UINT64 ofst = 0;
    FLD_HANDLE fh = get_fld_and_offset(struct_ty, field_id, fld, ofst);
    Is_True(!fh.Is_Null(), ("not find the field"));
    TYPE_ID desc = TY_mtype(FLD_type(fh));
    TYPE_ID rtype = Mtype_comparison(desc);
    ret_wn = WN_CreateIload(OPR_ILOAD, rtype, desc, ofst,
                            struct_ty, base_ty, base_wn, field_id);
  } else {
    TY_IDX index_ty = MTYPE_To_TY(WN_rtype(index_wn));
    ST_IDX func_st = Wgen_CreateFunctionSymbol("_builtin_get_element_from_tuple",
                                               MTYPE_To_TY(Pointer_Mtype),
                                               base_ty, index_ty,
                                               TY_IDX_ZERO);
    UINT args_num = 2;
    TY_IDX args_ty[] = {base_ty, index_ty};
    ptr_t args_wn[] = {base, index};
    ret_wn = (WN *)Wgen_ConvertCallExpr(func_st, args_num,
                                        args_ty, args_wn, TRUE/* retv */);
  }
  return (ptr_t)ret_wn;
}

ptr_t
Wgen_ConvertArrayExpr(ptr_t base_wn, ptr_t index) {
  Is_True((WN *)base_wn != NULL, ("invalid wn"));

  TY_IDX base_ty = TY_IDX_ZERO;
  if (WN_operator((WN *)base_wn) == OPR_ILOAD) {
    // get base ty from iload
    TY_IDX iload_ty = WN_load_addr_ty((WN *)base_wn);
    UINT field_id = WN_field_id((WN *)base_wn);
    base_ty = Wgen_GetFldTy(iload_ty, field_id);
  } else
    base_ty = WN_type((WN *)base_wn);
  Is_True(base_ty != TY_IDX_ZERO &&
          Wgen_IsArrayType(base_ty), ("invalid ty for array"));

  WN *index_wn = (WN *)index;
  Is_True(index_wn != NULL, ("invalid index_wn"));

  // get elem_wn
  TY_IDX struct_ty = TY_pointed(base_ty);
  UINT64 elem_ofst = FLD_ofst(FLD_next(TY_fld(struct_ty)));
  WN *elem_wn = WN_Iload(Pointer_Mtype, elem_ofst, struct_ty,
                         (WN *)base_wn, 2/*field_id*/);

  // get elem type & size
  TYPE_ID elem_type = WN_rtype(index_wn);
  if (!MTYPE_is_integral(elem_type)) {
    elem_type = TY_mtype(Wgen_GetNumericType());
    index_wn = WN_Type_Conversion(index_wn, elem_type);
  }

  UINT64 elem_size = MTYPE_byte_size(elem_type);
  WN *ofst_wn;
  // get offset from index_wn
  if (WN_operator(index_wn) == OPR_INTCONST)
    ofst_wn = WN_Intconst(elem_type, WN_const_val(index_wn) * elem_size);
  else
    ofst_wn = WN_Mpy(elem_type, index_wn, WN_Intconst(elem_type, elem_size));

  // adjust elem_wn
  WN *ret_wn = WN_Add(Pointer_Mtype, elem_wn, ofst_wn);

  return (ptr_t)ret_wn;
}

ptr_t
Wgen_GetLengthFromArray(ptr_t st_idx, id_t ty_idx) {
  Is_True(st_idx != ST_IDX_ZERO, ("invalid st"));
  Is_True(ty_idx != TY_IDX_ZERO && Wgen_IsArrayType(ty_idx),
          ("invalid ty for array"));

  WN *base_wn = WN_Ldid(TY_mtype(ty_idx), 0, st_idx, ty_idx);

  // get len_ty
  FLD_HANDLE fld = TY_fld(TY_pointed(ty_idx));
  TY_IDX len_ty = FLD_type(fld);
  WN *length_wn = WN_Iload(TY_mtype(len_ty), 0, TY_pointed(ty_idx),
                           base_wn, 1/*field_id*/);

  return (ptr_t)length_wn;
}

ptr_t
Wgen_ConvertPrePostIncDec(bool is_prefix, bool is_add, ptr_t base, bool retv) {
  WN *base_wn = (WN *)base;
  Is_True(base_wn != NULL, ("invalid wn"));

  OPERATOR opr = WN_operator(base_wn);
  UINT field_id = OPERATOR_has_field_id(opr)
                    ? WN_field_id(base_wn) : 0;
  INT64 offset = OPERATOR_has_offset(opr)
                    ? WN_offset(base_wn) : 0;
  TYPE_ID dtype = WN_rtype(base_wn);
  TYPE_ID rtype = Mtype_comparison(dtype);

  WN *block = WN_CreateBlock();
  BOOL gen_istore = FALSE; // istore or stid
  WN *rhs_wn = NULL;       // rhs wn for the store
  ST *st = NULL;           // st to be load/store
  WN *ret_value = NULL;    // return value
  TY_IDX load_ty = TY_IDX_ZERO;  // ty for iload/istore
  WN *addr = NULL;         // address to be load/store
  if (opr == OPR_ILOAD) {
    gen_istore = TRUE;
    load_ty = WN_load_addr_ty(base_wn);
    addr = WN_kid0(base_wn);
    Is_True(WN_rtype(addr) == Pointer_Mtype, ("not ptr mtype"));
  } else if (opr == OPR_LDID) {
    st = WN_st(base_wn);
  }

  // handle return value for postfix incr/decr
  if (is_prefix) {
    TY_IDX dty = MTYPE_To_TY(dtype);
    WN *st_wn = Wgen_StidTemp(dty, base_wn, ".anon.");
    WN_INSERT_BlockLast(block, st_wn);
    base_wn = WN_CreateLdid(OPR_LDID, rtype, WN_desc(st_wn),
                            WN_offset(st_wn), WN_st(st_wn), dty);
    ret_value = WN_COPY_Tree(base_wn);
  } else {
    ret_value = WN_COPY_Tree(base_wn);
  }

  if (MTYPE_is_boolean(rtype)) {
    rhs_wn = WN_Intconst(rtype, 1);
  } else {
    WN *one_wn = NULL;
    if (MTYPE_is_pointer(rtype)) {
      // TODO: variableArrayType ??
      one_wn = WN_Intconst(rtype, 8);
    }
    else if (MTYPE_is_integral(rtype) || MTYPE_is_float(rtype)) {
      one_wn = (WN *)Wgen_GetConst(MTYPE_To_TY(rtype), 1);
    }
    rhs_wn = WN_Binary(is_add ? OPR_ADD : OPR_SUB, rtype, base_wn, one_wn);
  }

  WN *stid = NULL;
  if (gen_istore) {
    Is_True(addr != NULL, ("addr is NULL"));
    stid = WN_CreateIstore(OPR_ISTORE, MTYPE_V, dtype,
                           offset, load_ty, rhs_wn, addr, field_id);
  } else {
    stid = WN_Stid(dtype, offset, st, ST_type(st), rhs_wn, field_id);
  }
  WN_INSERT_BlockLast(block, stid);

  if (retv)
    return (ptr_t)Wgen_CreateComma(rtype, block, ret_value);

  return (ptr_t)block;
}

ptr_t
Wgen_GetOperatorNewExpr(ptr_t args[], id_t arg_length, id_t ty_idx, ptr_t blk) {
  // arg_length == 0 && args[0] != NULL:
  //   means a numeric value that represents the size of the array
  // arg_length == 1 && args[0] != NULL:
  //   means an array initialized with one element

  Is_True(ty_idx != TY_IDX_ZERO,
          ("unsupported array ty"));
  Is_True((WN *)blk != NULL && WN_operator((WN *)blk) == OPR_BLOCK,
          ("invalid blk wn"));
  Is_True(!Wgen_IsAnyType(ty_idx),
          ("unsupported ty for Wgen_GetOperatorNewExpr yet"));

  // calculate size to be allocated
  WN *elem_count = NULL;
  WN *size_wn = NULL;

  TYPE_ID len_type = TY_mtype(Wgen_GetNumericType());
  TY_IDX ret_ty_idx = MTYPE_To_TY(Pointer_Mtype);
  TY_IDX elem_ty;

  if (Wgen_IsArrayType(ty_idx)) {
    elem_ty = Wgen_GetElemTy(ty_idx);

    // get elem count
    if (arg_length == 0 && args && args[0])
      elem_count = WN_COPY_Tree((WN *)args[0]);
    else
      elem_count = WN_Intconst(len_type, arg_length);
    Is_True(elem_count, ("wrong array size"));

    // check if elem_count need conversion
    elem_count = Wgen_CreateCvt(elem_count, Pointer_Mtype);
    Is_True(OPERATOR_is_expression(WN_operator(elem_count)),
            ("not expr"));

    elem_count = Wgen_HandleExprForCopy((WN *)blk, elem_count, ty_idx, ".size");

    // change size to 1 if it's 0
    INT64 ty_size = TY_size(elem_ty) ? TY_size(elem_ty) : 1;
    // size = elem_size + length_size
    WN *elem_size = TY_is_union(elem_ty)
                    ? WN_Intconst(Pointer_Mtype, ty_size)
                    : WN_Mpy(Pointer_Mtype, WN_COPY_Tree(elem_count),
                             WN_Intconst(Pointer_Mtype, ty_size));
    size_wn = WN_Add(Pointer_Mtype,
                     elem_size,
                     WN_Intconst(Pointer_Mtype, MTYPE_byte_size(len_type)));
  } else if (Wgen_IsRecordType(ty_idx)) {
    // new a struct obj
    size_wn = WN_Intconst(Pointer_Mtype, TY_size(TY_pointed(ty_idx)));
  } else {
    Is_True(false, ("unsupported ty for NewExpr"));
  }

  // generate tmp stid st
  Is_True(size_wn != NULL, ("invalid size wn"));
  WN *st_wn = Wgen_StidTemp(ty_idx, size_wn, ".anon.");
  WN_INSERT_BlockLast((WN *)blk, st_wn);
  WN *ret_wn = WN_Ldid(Pointer_Mtype, WN_offset(st_wn), WN_st(st_wn), ty_idx);

  // convert operator new
  ST_IDX new_st = Wgen_CreateFunctionSymbol("_Znam",
                                            Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)),
                                            MTYPE_To_TY(MTYPE_U8),
                                            TY_IDX_ZERO);
  Is_True(new_st, ("bad new st"));

  // create parm
  WN *call_blk = WN_CreateBlock();
  WN *call_wn = WN_Create(OPR_CALL, Pointer_Mtype, MTYPE_V, 1);
  WN_st_idx(call_wn) = new_st;
  WN_kid(call_wn, 0) = Wgen_CreateParm(Pointer_Mtype, ret_wn, ty_idx);
  WN_INSERT_BlockLast(call_blk, call_wn);

  WN *ret = WN_Ldid(TY_mtype(ret_ty_idx), -1, Return_Val_Preg, ret_ty_idx);
  WN *comma = Wgen_CreateComma(WN_rtype(ret), call_blk, ret);

  // create a tmp for return value
  Is_True(TY_kind(ty_idx) == KIND_POINTER, ("not ptr type"));
  ST *tmp_st = Wgen_CreateTmpSym(ty_idx, Save_Str(".nwm.mptr"));
  st_wn = WN_Stid(TY_mtype(ty_idx), 0, tmp_st, ty_idx, comma);
  WN_INSERT_BlockLast((WN *)blk, st_wn);
  WN *ldid_wn = WN_Ldid(TY_mtype(ty_idx), 0, tmp_st, ty_idx);

  if (Wgen_IsArrayType(ty_idx)) {
    // store length wn
    WN *store_len = Wgen_CreateIstore(OPR_ISTORE, MTYPE_V, len_type,
                                      0, ty_idx, WN_COPY_Tree(elem_count),
                                      ldid_wn, 1);
    WN_INSERT_BlockLast((WN *)blk, store_len);
  }

  // if no need to initialize, just return
  if (arg_length == 0) {
    if (Wgen_IsRecordType(ty_idx)) {
      // requires zero initialization
      WN *mstore = WN_CreateMstore(0, ty_idx, WN_Intconst(MTYPE_I4, 0),
                                   ldid_wn, WN_COPY_Tree(size_wn));
      WN_INSERT_BlockLast((WN *)blk, mstore);
    }
    return (ptr_t)ldid_wn;
  }

  // handle initialization
  if (Wgen_IsArrayType(ty_idx)) {
    // store elem wn
    UINT i = 0;
    UINT64 ofst = 0;
    for (i; i < arg_length; ++i) {
      WN *index_wn = WN_Intconst(len_type, i);
      ptr_t array_wn = Wgen_ConvertArrayExpr((ptr_t)ldid_wn, (ptr_t)index_wn);
      WN *istore_wn = (WN *)Wgen_Istore(elem_ty, array_wn, args[i]);
      WN_INSERT_BlockLast((WN *)blk, istore_wn);
    }
  } else {
    WN *init_blk = (WN *)Wgen_EmitRecordInit(ST_st_idx(tmp_st), arg_length, args);
    WN_INSERT_BlockLast((WN *)blk, init_blk);
  }

  return (ptr_t)ldid_wn;
}

ptr_t
Wgen_GetRestValueFromArray(id_t array_ty, ptr_t wn) {
  WN *base_wn = (WN *)wn;
  Is_True(base_wn != NULL, ("invalid base wn"));
  ST_IDX func_st = Wgen_CreateFunctionSymbol("_builtin_get_rest_value_from_array",
                                             array_ty, array_ty,
                                             TY_IDX_ZERO);
  UINT args_num = 1;
  TY_IDX args_ty[] = {array_ty};
  ptr_t args_wn[] = {wn};
  return Wgen_ConvertCallExpr(func_st, args_num,
                              args_ty, args_wn, TRUE/* retv */);
}

ptr_t
Wgen_ConvertArrayToSet(id_t ty, ptr_t array) {
  Is_True(ty != TY_IDX_ZERO, ("invalid ty"));
  WN *array_wn = (WN *)array;
  Is_True(array_wn != NULL, ("invalid array wn"));
  TY_IDX array_ty = MTYPE_To_TY(WN_rtype(array_wn));
  ST_IDX func_st = Wgen_CreateFunctionSymbol("_builtin_convert_array_to_set",
                                             ty, array_ty,
                                             TY_IDX_ZERO);
  UINT args_num = 1;
  TY_IDX args_ty[] = {array_ty};
  ptr_t args_wn[] = {array};
  return Wgen_ConvertCallExpr(func_st, args_num,
                              args_ty, args_wn, TRUE/* retv */);
}

ptr_t
Wgen_ConvertSetToArray(id_t ty, ptr_t set) {
  Is_True(ty != TY_IDX_ZERO, ("invalid ty"));
  WN *set_wn = (WN *)set;
  Is_True(set_wn != NULL, ("invalid set wn"));
  TY_IDX set_ty = MTYPE_To_TY(WN_rtype(set_wn));
  ST_IDX func_st = Wgen_CreateFunctionSymbol("_builtin_convert_set_to_array",
                                             ty, set_ty,
                                             TY_IDX_ZERO);
  UINT args_num = 1;
  TY_IDX args_ty[] = {set_ty};
  ptr_t args_wn[] = {set};
  return Wgen_ConvertCallExpr(func_st, args_num,
                              args_ty, args_wn, TRUE/* retv */);
}

// ==================================================================
// handle stmt related functions
// ==================================================================

ptr_t
Wgen_ConvertReturnStmt(ptr_t ret) {
  WN *ret_wn = (WN *)ret;
  Is_True(ret_wn != NULL, ("invalid ret wn"));
  return (ptr_t)WN_CreateReturn_Val(OPR_RETURN_VAL, WN_rtype(ret_wn),
                                    MTYPE_V, ret_wn);
}

ptr_t
Wgen_ConvertIfStmt(ptr_t cond, ptr_t then_blk, ptr_t else_blk) {
  WN *cond_wn = (WN *)cond;
  Is_True(cond_wn != NULL, ("invalid cond_wn"));

  WN *then_blk_wn = (WN *)then_blk;
  Is_True(then_blk_wn != NULL && WN_operator(then_blk_wn) == OPR_BLOCK,
          ("invalid then block"));
  WN *else_blk_wn = (WN *)else_blk;
  Is_True(else_blk_wn != NULL && WN_operator(else_blk_wn) == OPR_BLOCK,
          ("invalid else block"));
  return (ptr_t)WN_CreateIf(Wgen_HandleCondWN(cond_wn), then_blk_wn, else_blk_wn);
}

typedef enum {
  DoStatement = 1,
  ForStatement,
  SwitchStatement,
  WhileStatement,
  ForOfStatement,
} LOOP_STMT_KIND;

#define ENLARGE(x) (x + (x >> 1))

typedef struct case_info_t {
  INT64     case_lower_bound_value;
  INT64     case_upper_bound_value;
  LABEL_IDX case_label_idx;
} CASE_INFO;

static INT32        case_info_i = -1;
static INT32        case_info_max = 32;
static CASE_INFO   *case_info_stack =
  (CASE_INFO *) malloc (sizeof (CASE_INFO) * case_info_max);

typedef struct switch_info_t {
  WN        *index;
  INT32      start_case_index;
  LABEL_IDX  default_label_idx;
} SWITCH_INFO;

static INT32        switch_info_i = -1;
static INT32        switch_info_max = 32;
static SWITCH_INFO *switch_info_stack =
  (SWITCH_INFO *) malloc (sizeof (SWITCH_INFO) * switch_info_max);

typedef struct label_info_t {
  LABEL_IDX         label_idx;
  unsigned char     symtab_idx;
  unsigned char     defined;
} LABEL_INFO;

static INT32        undefined_labels_i = -1;
static INT32        undefined_labels_max = 32;
static LABEL_INFO  *undefined_labels_stack =
  (LABEL_INFO *) malloc (sizeof (LABEL_INFO) * undefined_labels_max);

typedef struct break_continue_info_t {
  LOOP_STMT_KIND tree_code;
  LABEL_IDX break_label_idx;
  LABEL_IDX continue_label_idx;
} BREAK_CONTINUE_INFO;

static INT32                break_continue_info_i = -1;
static INT32                break_continue_info_max = 32;
static BREAK_CONTINUE_INFO *break_continue_info_stack =
    (BREAK_CONTINUE_INFO *) malloc (sizeof (BREAK_CONTINUE_INFO) *
                                    break_continue_info_max);

id_t
Wgen_GetLabelIdx() {
  LABEL_IDX label_idx;
  New_LABEL(CURRENT_SYMTAB, label_idx);
  return label_idx;
}

ptr_t
Wgen_GenLabelWN(id_t label_idx) {
  Is_True(label_idx != 0, ("invalid label idx"));
  WN *wn = WN_CreateLabel((ST_IDX)0, label_idx, 0, NULL);
  return (ptr_t)wn;
}

void
Wgen_RecordLoopSwitch(id_t kind, id_t break_label_idx, id_t continue_label_idx) {
  LOOP_STMT_KIND stmt = (LOOP_STMT_KIND)kind;
  Is_True(stmt == DoStatement ||
          stmt == ForStatement ||
          stmt == SwitchStatement ||
          stmt == WhileStatement,
          ("unexpected stmt"));

  // realloc break_continue_info_stack if needed
  if (++break_continue_info_i == break_continue_info_max) {
    break_continue_info_max = ENLARGE(break_continue_info_max);
    break_continue_info_stack =
      (BREAK_CONTINUE_INFO *) realloc (break_continue_info_stack,
                                     break_continue_info_max *
                                       sizeof (BREAK_CONTINUE_INFO));
  }

  break_continue_info_stack
    [break_continue_info_i].tree_code          = stmt;
  break_continue_info_stack
    [break_continue_info_i].break_label_idx    = break_label_idx;
  break_continue_info_stack
    [break_continue_info_i].continue_label_idx = continue_label_idx;
}

static void
Wgen_PopLoopSwitch(id_t kind, id_t &break_label, id_t &cont_label) {
  LOOP_STMT_KIND stmt = (LOOP_STMT_KIND)kind;
  Is_True(break_continue_info_i >= 0 &&
          break_continue_info_stack[break_continue_info_i].tree_code == stmt,
          ("bad loop index"));

  break_label = break_continue_info_stack[break_continue_info_i].break_label_idx;
  cont_label = break_continue_info_stack[break_continue_info_i].continue_label_idx;
  --break_continue_info_i;
}

// handle cond wn in switch statement
ptr_t
Wgen_ConvertCondInSwitchStmt(ptr_t cond) {
  WN *cond_wn = (WN *)cond;
  Is_True(cond_wn != NULL, ("Child 0 of IF can not be NULL!"));

  WN *stid;
  // The switch index may be needed more than once if it contains case
  // range. As it may have side-effects like a function call, save the
  // index into a temporary, and used the saved value.
  // when the switch index is a constant,
  // we don't generate the index var, instead, we use constant the as switch
  // expression directly. This helps compiler eliminates the unreachable
  //  branch code even at O0 phase.
  TYPE_ID index_mtype = Mtype_comparison(WN_rtype(cond_wn));
  if (WN_operator(cond_wn) != OPR_INTCONST) {
    ST *save_expr_st = Gen_Temp_Symbol(MTYPE_TO_TY_array[index_mtype], "_switch_index");
    stid = WN_Stid(index_mtype, 0, save_expr_st,
                   MTYPE_TO_TY_array[index_mtype], cond_wn);
    cond_wn = WN_Ldid(index_mtype, 0, save_expr_st, MTYPE_TO_TY_array[index_mtype]);
  }

  if (++switch_info_i == switch_info_max) {
    switch_info_max   = ENLARGE(switch_info_max);
    switch_info_stack = (SWITCH_INFO *) realloc (switch_info_stack,
                               switch_info_max * sizeof (SWITCH_INFO));
  }

  switch_info_stack[switch_info_i].index             = cond_wn;
  switch_info_stack[switch_info_i].start_case_index  = case_info_i + 1;
  switch_info_stack[switch_info_i].default_label_idx = 0;

  return (ptr_t)stid;
}

ptr_t
Wgen_ConvertSwitchStmt(ptr_t body_blk, id_t body_exit_label_idx) {
  WN *switch_block = WN_CreateBlock();

  // generate casegoto statments
  LABEL_IDX exit_label_idx;
  WN *def_goto;
  WN *case_block;
  WN *case_range;
  WN *case_entry;
  WN *switch_wn;
  INT32 i;
  INT32 n = case_info_i - switch_info_stack[switch_info_i].start_case_index + 1;

  if (break_continue_info_stack[break_continue_info_i].break_label_idx)
    exit_label_idx = break_continue_info_stack[break_continue_info_i].break_label_idx;
  else
    New_LABEL (CURRENT_SYMTAB, exit_label_idx);
  if (switch_info_stack[switch_info_i].default_label_idx) {
    def_goto = WN_CreateGoto(switch_info_stack[switch_info_i].default_label_idx);
  } else {
    def_goto = WN_CreateGoto(exit_label_idx);
  }

  case_block = WN_CreateBlock();
  case_range = WN_CreateBlock();

  for (i = switch_info_stack[switch_info_i].start_case_index;
       i <= case_info_i;
       i++) {
    INT64     case_value;
    LABEL_IDX case_label_idx = case_info_stack[i].case_label_idx;
    INT64 low = case_info_stack[i].case_lower_bound_value;
    INT64 high = case_info_stack[i].case_upper_bound_value;
    if (low < high) {
      Is_True(false, ("TODO"));
    } else {
      case_entry = WN_CreateCasegoto(low, case_label_idx);
      WN_INSERT_BlockLast(case_block, case_entry);
    }
  }
  switch_wn = Wgen_CreateSwitch(n,
                                switch_info_stack[switch_info_i].index,
                                case_block,
                                def_goto,
                                exit_label_idx);

  WN_INSERT_BlockLast(switch_block, switch_wn);

  if (body_blk)
    WN_INSERT_BlockLast(switch_block, (WN *)body_blk);

  // TODO: duplicate label wn
  if (WN_operator(WN_last(switch_block)) != OPR_LABEL ||
      WN_label_number(WN_last(switch_block)) != exit_label_idx) {
    WN *wn = WN_CreateLabel(ST_IDX_ZERO, exit_label_idx, 0, NULL);
    WN_INSERT_BlockLast(switch_block, wn);
  }

  case_info_i = switch_info_stack[switch_info_i].start_case_index - 1;
  --switch_info_i;

  --break_continue_info_i;

  if (body_exit_label_idx != exit_label_idx) {
    WN *end = WN_CreateLabel(ST_IDX_ZERO, body_exit_label_idx, 0, NULL);
    WN_INSERT_BlockLast (switch_block, end);
  }

  return (ptr_t)switch_block;
}

ptr_t
Wgen_ConvertLoopStmt(id_t kind, ptr_t cond, ptr_t body_blk, ptr_t inc_blk,
                     id_t break_label, id_t continue_label) {
  WN *cond_wn = (WN *)cond;
  Is_True(cond_wn != NULL, ("invalid cond wn"));

  WN *body_blk_wn = (WN *)body_blk;
  Is_True(body_blk_wn != NULL && WN_operator(body_blk_wn) == OPR_BLOCK,
          ("invalid body blk"));

  WN *inc_blk_wn = (WN *)inc_blk;
  Is_True(inc_blk_wn != NULL && WN_operator(inc_blk_wn) == OPR_BLOCK,
          ("invalid inc blk"));

  LOOP_STMT_KIND stmt = (LOOP_STMT_KIND)kind;

  // get real label index for break and continue and
  // pop the loop structure
  Wgen_PopLoopSwitch(stmt, break_label, continue_label);

  // append continue label to body
  if (continue_label) {
    WN *label_wn = WN_CreateLabel((ST_IDX)0, continue_label, 0, NULL);
    WN_INSERT_BlockLast(body_blk_wn, label_wn);
  }

  // append inc block
  if (!WN_block_empty(inc_blk_wn))
    WN_INSERT_BlockLast(body_blk_wn, inc_blk_wn);

  // create loop block
  WN *loop_blk = WN_CreateBlock();

  // create while-do or do-while
  WN *loop_wn;
  if (stmt != DoStatement)
    loop_wn = WN_CreateWhileDo(cond_wn, body_blk_wn);
  else
    loop_wn = WN_CreateDoWhile(cond_wn, body_blk_wn);
  WN_INSERT_BlockLast(loop_blk, loop_wn);

  // append break label after while-do
  if (break_label) {
    WN *wn = WN_CreateLabel((ST_IDX)0, break_label, 0, NULL);
    WN_INSERT_BlockLast(loop_blk, wn);
  }

  return (ptr_t)loop_blk;
}

ptr_t
Wgen_ConvertCaseStmt(id_t val) {
  LABEL_IDX  case_label_idx;

  if (++case_info_i == case_info_max) {
    case_info_max   = ENLARGE(case_info_max);
    case_info_stack = (CASE_INFO *) realloc(case_info_stack,
                                            case_info_max * sizeof (CASE_INFO));
  }
  // TODO: lower & upper val
  case_info_stack
    [case_info_i].case_lower_bound_value = val;
  case_info_stack
    [case_info_i].case_upper_bound_value = val;

  New_LABEL(CURRENT_SYMTAB, case_label_idx);
  case_info_stack[case_info_i].case_label_idx = case_label_idx;

  WN *wn = WN_CreateLabel((ST_IDX) 0, case_label_idx, 0, NULL);
  return (ptr_t)wn;
}

ptr_t
Wgen_ConvertBreakStmt(id_t label_idx) {
  Is_True(break_continue_info_i >= 0, ("No break/continue info"));
  if (label_idx == 0) {
    INT32 i = break_continue_info_i;
    label_idx = break_continue_info_stack[i].break_label_idx;
    if (label_idx == 0) {
      // Control can reach here even while processing an
      // exception handler.
      New_LABEL(CURRENT_SYMTAB, label_idx);
      break_continue_info_stack[i].break_label_idx = label_idx;
    }
  }

  WN *wn = WN_CreateGoto((ST_IDX)NULL, label_idx);
  return (ptr_t)wn;
}

ptr_t
Wgen_ConvertContinueStmt(id_t label_idx) {
  Is_True(break_continue_info_i >= 0, ("No break/continue info"));
  INT32 i = break_continue_info_i;

  if (label_idx == 0) {
    // find the enclosing loop
    if (i != -1) {
      while (break_continue_info_stack[i].tree_code == SwitchStatement)
        --i;
      if (i != -1) {
        label_idx = break_continue_info_stack[i].continue_label_idx;
        if (label_idx == 0) {
          // control can reach here even while processing an exception handler
          New_LABEL (CURRENT_SYMTAB, label_idx);
          break_continue_info_stack[i].continue_label_idx = label_idx;
        }
      }
    }
  }

  Is_True(label_idx, ("null continue label idx"));
  WN *wn = WN_CreateGoto((ST_IDX)NULL, label_idx);
  return (ptr_t)wn;
}

ptr_t
Wgen_ConvertDefaultStmt() {
  LABEL_IDX def_label_idx;
  New_LABEL(CURRENT_SYMTAB, def_label_idx);
  switch_info_stack[switch_info_i].default_label_idx = def_label_idx;

  WN *wn = WN_CreateLabel((ST_IDX) 0, def_label_idx, 0, NULL);
  return (ptr_t)wn;
}
