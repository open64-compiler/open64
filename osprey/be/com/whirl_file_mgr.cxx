/*
   Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

#include "whirl_file_mgr.h"
#include "defs.h"
#include "wn.h"
#include "symtab.h"
#include "be_symtab.h"
#include "pu_info.h"
#include "ir_bread.h"
#include "ir_reader.h"
#include "driver_util.h"
#include "dwarf_DST_dump.h"
#include "glob.h"
#include "timing.h"
#include "symtab_access_global.h"

bool verbose = false;
FILE* trace_file = NULL;
/*
 * WHIRL_FILE_INFO
 */

// setup buffer for Read_Global_Info and Read_Local_Info
void
WHIRL_FILE_INFO::Prepare_buffer() {
  Set_Global_Info(_filename, _fhandle);
  Set_Local_Info(_filename, _fhandle);
  Reset_misc_symtab();
} 

// Read global symtab from whirl file
bool
WHIRL_FILE_INFO::Load_symtab() {
  MEM_POOL_Push (&MEM_src_pool);
  MEM_POOL_Push (&MEM_src_nz_pool);
  Set_Error_Source (Src_File_Name);
  //Options_Stack = CXX_NEW(OPTIONS_STACK(&MEM_src_nz_pool), &MEM_src_nz_pool);
  //Options_Stack->Push_Current_Options();
  Start_Timer (T_ReadIR_Comp);

  //Irb_File = (FILE *)Open_Input_Info (Irb_File_Name);
  //Set_Global_Info(_filename, _fhandle);
  //Set_Local_Info(_filename, _fhandle);
  Initialize_Symbol_Tables (FALSE);
  New_Scope (GLOBAL_SYMTAB, Malloc_Mem_Pool, FALSE);
  INT pu_num;
  PU_Info *pu_tree = Read_Global_Info(&pu_num);
  Global_PU_Tree = (void *)pu_tree;
  Stop_Timer (T_ReadIR_Comp);

  Initialize_Special_Global_Symbols();

  BE_symtab_alloc_scope_level(GLOBAL_SYMTAB);
  SYMTAB_IDX scope_level;
  for (scope_level = 0;
       scope_level <= GLOBAL_SYMTAB;
       ++scope_level) {
    // No need to deal with levels that don't have st_tab's. Currently
    // this should be only zero.
    if (Scope_tab[scope_level].st_tab != NULL) {
      Scope_tab[scope_level].st_tab->
        Register(*Be_scope_tab[scope_level].be_st_tab);
    }
    else {
      Is_True(scope_level == 0,
              ("Nonexistent st_tab for level %d", scope_level));
    }
  }
}

// Process file table, register files and set up local mapping
void
WHIRL_FILE_INFO::Process_file_table(WHIRL_FILE_MANAGER* mgr) {
  // ignore files in syslib
  const char* libpath = mgr->Get_libpath();
  if (libpath != NULL && strncmp(libpath, _filename, strlen(libpath)) == 0)
    return;

  INT i;
  char fname[1024];
  _file_no_map.push_back(0);  // reserve entry 0
  if (!FILE_INFO_is_vtable(Whirl_file_info()) && !FILE_INFO_is_rbc(Whirl_file_info())) {
    for (i = 1; i < Get_Local_File_Count(); ++i) {
      const char* fp = Get_Local_File_Full_Path(i, fname, sizeof(fname));
      UINT32 gid = mgr->Register_file(fname);
      _file_no_map.push_back(gid);
      Is_True(strcmp(fp, mgr->Get_file_name(gid)) == 0,
              ("global file name mismatch"));
    }
  }
}

WHIRL_FILE_INFO::~WHIRL_FILE_INFO() {
}

/*
 * WHIRL_FILE_INFO dump utilities
 */

// dump pu IR tree
static void
dump_pu_tree(PU_Info* pu_tree, FILE* f) {
  Is_True(pu_tree != NULL, ("invalid pu"));
  PU_Info* pu;
  WN* wn;
  for (pu = pu_tree; pu != NULL; pu = PU_Info_next(pu)) {
    wn = PU_Info_tree_ptr(pu);
    Is_True(wn != NULL, ("invalid wn"));
    Current_PU_Info = pu;
    IR_put_func (wn, f);
    if (PU_Info_child(pu))
      dump_pu_tree(PU_Info_child(pu), f);
    SYMTAB_IDX level =
      PU_lexical_level (&St_Table[PU_Info_proc_sym (pu)]);
    Print_local_symtab(f, Scope_tab[level]);
  }
}

// dump global symtab and pu IR tree for the file
void
WHIRL_FILE_INFO::Dump_irb(FILE* fp) const {
  bool close_file = false;
  if (fp == NULL) {
    char fn[256];
    snprintf(fn, 255, "%s.irb", _filename);
    fp = fopen(fn, "w");
    Is_True(fp != NULL, ("failed to open %s", fn));
    close_file = true;
  }
  Restore_context();
  fprintf(fp, "IR Dump for %s\n", _filename);
  dump_pu_tree((PU_Info *)Global_PU_Tree, fp);
  Print_global_symtab(fp); 
  Dump_DST(fp);
  if (close_file)
    fclose(fp);
}

// verify symtab and its access function
#define VERIFY_ACCESS_FUNCTION

#ifdef VERIFY_ACCESS_FUNCTION
template<typename _T>
struct verify_access_op {
  UINT file_id;
  verify_access_op(UINT32 f) : file_id(f) { }
  void operator() (UINT idx, _T* entry) const;
};

template<>
inline void
verify_access_op<ST>::operator() (UINT idx, ST* st) const
{
  Is_True(ST_st_idx(st) == make_ST_IDX(idx, GLOBAL_SYMTAB),
          ("ST_st_idx is wrong"));
  Is_True(st == St_ptr(file_id, make_ST_IDX(idx, GLOBAL_SYMTAB)),
          ("ST_ptr is wrong"));
  Is_True(ST_class(st) == CLASS_CONST ||
          ST_name(st) == Str_ptr(file_id, st->u1.name_idx),
          ("Str_ptr is wrong"));
}

template<>
inline void
verify_access_op<INITO>::operator() (UINT idx, INITO* ino) const
{
  Is_True(ino == Inito_ptr(file_id, make_INITO_IDX(idx, GLOBAL_SYMTAB)),
          ("Inito_ptr is wrong"));
}

template<>
inline void
verify_access_op<ST_ATTR>::operator() (UINT idx, ST_ATTR* attr) const
{
  Is_True(attr == St_attr_ptr(file_id, idx),
          ("St_attr_ptr is wrong"));
}

template<>
inline void
verify_access_op<TY>::operator() (UINT idx, TY* ty) const
{
  Is_True(ty == Ty_ptr(file_id, make_TY_IDX(idx)),
          ("Ty_ptr is wrong"));
  Is_True(TY_name(*ty) == Str_ptr(file_id, ty->name_idx),
          ("Str_ptr is wrong"));
}

template<>
inline void
verify_access_op<FLD>::operator() (UINT idx, FLD* fld) const
{
  Is_True(fld == Fld_ptr(file_id, idx),
          ("Fld_ptr is wrong"));
  Is_True(FLD_name(FLD_HANDLE(idx)) == Str_ptr(file_id, fld->name_idx),
          ("Str_ptr is wrong"));
}

template<>
inline void
verify_access_op<TYLIST>::operator() (UINT idx, TYLIST* tylist) const
{
  Is_True(tylist == Tylist_ptr(file_id, idx),
          ("Tylist_ptr is wrong"));
}

template<>
inline void
verify_access_op<ARB>::operator() (UINT idx, ARB* arb) const
{
  Is_True(arb == Arb_ptr(file_id, idx),
          ("Arb_ptr is wrong"));
}

template<>
inline void
verify_access_op<TCON>::operator() (UINT idx, TCON* tcon) const
{
  Is_True(tcon == Tcon_ptr(file_id, idx),
          ("Tcon_ptr is wrong"));
  if (TCON_ty(*tcon) == MTYPE_STRING) {
    Is_True(Targ_String_Address(*tcon) == Tcon_str_ptr(file_id, tcon->vals.sval.cp),
            ("Tcon_str_ptr is wrong"));
  }
}

template<>
inline void
verify_access_op<INITV>::operator() (UINT idx, INITV* inv) const
{
  Is_True(inv == Initv_ptr(file_id, idx),
          ("Initv_ptr is wrong"));
}
#endif

void
WHIRL_FILE_INFO::Verify_symtab() const {
  Restore_context();
  Verify_GLOBAL_SYMTAB();

  // verify access function
#ifdef VERIFY_ACCESS_FUNCTION
  if ( ST_Table_Size (GLOBAL_SYMTAB))
    For_all (St_Table, GLOBAL_SYMTAB, verify_access_op<ST>(_file_idx));
  if ( INITO_Table_Size (GLOBAL_SYMTAB))
    For_all (Inito_Table, GLOBAL_SYMTAB, verify_access_op<INITO>(_file_idx));
  if ( ST_ATTR_Table_Size (GLOBAL_SYMTAB))
    For_all (St_Attr_Table, GLOBAL_SYMTAB, verify_access_op<ST_ATTR>(_file_idx));
  if ( TY_Table_Size () > 1)
    For_all (Ty_Table, verify_access_op<TY>(_file_idx));
  if ( FLD_Table_Size () > 1)
    For_all (Fld_Table, verify_access_op<FLD>(_file_idx));
  if ( TYLIST_Table_Size () > 1)
    For_all (Tylist_Table, verify_access_op<TYLIST>(_file_idx));
  if ( ARB_Table_Size () > 1)
    For_all (Arb_Table, verify_access_op<ARB>(_file_idx));
  if ( TCON_Table_Size () )
    For_all (Tcon_Table, verify_access_op<TCON>(_file_idx));
  if ( INITV_Table_Size () > 1)
    For_all (Initv_Table, verify_access_op<INITV>(_file_idx));
#endif
}

/*
 * WHIRL_FILE_MANAGER
 */
WHIRL_FILE_MANAGER *WHIRL_FILE_MANAGER::_instance;

// dump symtab and ir for all files
void
WHIRL_FILE_MANAGER::Dump_irb(FILE* fp) const {
  std::vector<WHIRL_FILE_INFO>::const_iterator it = _files.begin();
  for (; it != _files.end(); ++it) {
    if (it->File_type() != file_whirl &&
        it->File_type() != file_ar_whirl)
      continue;
    it->Dump_irb(fp);
  }
}

// verify symtab
void
WHIRL_FILE_MANAGER::Verify_symtab() const
{
  std::vector<WHIRL_FILE_INFO>::const_iterator it = _files.begin();
  for (; it != _files.end(); ++it) {
    if (it->File_type() != file_whirl &&
        it->File_type() != file_ar_whirl)
      continue;
    it->Verify_symtab();
  }
}

