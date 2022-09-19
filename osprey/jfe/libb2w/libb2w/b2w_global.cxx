/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

/******************************************************************************
 * b2w_global.h
 * Context to be used
 */
#include "b2w_common.h"
#include "b2w_handler.h"
#include <stdio.h>
#include "dwarf_DST_producer.h"
#include <execinfo.h>

BOOL            TARGET_64BIT        = TRUE;
INT             Debug_Level         = 1;
CHPTR           Dash                = (CHPTR) "-";
PU_Info        *PU_Tree_Root        = NULLPTR;
BOOL            Run_vsaopt          = FALSE;
INT             trace_verbose       = 0;

//============= B2W_CONTEXT Var ================
WN_VECCXX       B2W_CONTEXT::_curr_entry_wn;
PU_Info**       B2W_CONTEXT::_pu_info_tab;
PU_Info*        B2W_CONTEXT::_pu_root;
CHPTR           B2W_CONTEXT::_current_file_name = NULLPTR;
INT             B2W_CONTEXT::_max_file_name_len = 0;
CUVEC           B2W_CONTEXT::_dir_dst_list;
CUUPAIRVEC      B2W_CONTEXT::_file_dst_list;
UINT            B2W_CONTEXT::_current_dir       = 1;
UINT            B2W_CONTEXT::_current_file      = 1;
INITO_IDX       B2W_CONTEXT::_last_initv        = 0;
INT             B2W_CONTEXT::_anon_num          = 0;
INT             B2W_CONTEXT::_log_level         = 0;
U6464MAP        B2W_CONTEXT::_temporaries;
UINT64          B2W_CONTEXT::_temps_count       = 1;
UINT            B2W_CONTEXT::_align_bytes       = 1;
W2WMAP          B2W_CONTEXT::_wn_parent_map;
DST_IDX         B2W_CONTEXT::_comp_unit_idx;

CCHPTR B2W_CONTEXT::Prepare_Source(CHPTR file_name) {

    INT64	i                   = 0;
    CHPTR       cp                  = NULLPTR;
    CHPTR       fname               = NULLPTR;
    INT64       len                 = 0;
    BOOL        dashdash_flag       = FALSE;

    Src_File_Name = New_Extension(file_name, CLASS_FILE_EXTENSION);;

    /*
     * Initialize error handler:
     *
     * */
    Init_Error_Handler ( 100 );
    Set_Error_Line ( ERROR_LINE_UNKNOWN );
    Set_Error_File ( NULLPTR );
    Set_Error_Phase ( "Front End Driver" );

    /* Clear file names: */
    Err_File_Name       = Dash;	/* Error file */
    DSTdump_File_Name   = NULLPTR; /* DST dump */

    fname = Last_Pathname_Component ( Src_File_Name );

    if (Err_File_Name == NULLPTR) {
      Err_File_Name = New_Extension(fname, ERR_FILE_EXTENSION);
    } else if (*Err_File_Name == '-') {
      Err_File_Name = NULLPTR;
    }
    Set_Error_File (Err_File_Name);

    if (Trc_File_Name == NULLPTR) {
      if (Tracing_Enabled) {
        /* Replace source file extension to get trace file: */
        Trc_File_Name = New_Extension(fname, TRC_FILE_EXTENSION);
      }
    } else if (*Trc_File_Name == '-') {
      Trc_File_Name = NULLPTR;
    }
    Set_Trace_File(Trc_File_Name);
    if (Get_Trace(TKIND_INFO, TINFO_TIME)) {
      Tim_File = TFile;
    }
    /* We're ready to pre-process: */
    IR_File_Name      =  Src_File_Name;
    Irb_File_Name     =  file_name;

    /* Open the IR file for compilation: */
    if (Irb_File_Name == NULLPTR) {
      /* Replace source file extension to get listing file: */
      Irb_File_Name = New_Extension(fname, IRB_FILE_EXTENSION);
    }

    if ((Irb_File = fopen(Irb_File_Name, "w")) == NULLPTR) {
      ErrMsg(EC_IR_Open, IR_File_Name, errno);
      B2W_CONTEXT::Cleanup_files(TRUE, FALSE);
      return Irb_File_Name;
    } else {
      if (Get_Trace(TP_MISC, 1)) {
        fprintf(TFile,
                "\n%sControl Values: Open_Dot_B_File\n%s\n", DBar, DBar);
        Print_Controls(TFile, "", TRUE);
      }
    }

    /* Configure internal options for this source file */
    Configure_Source(Src_File_Name);
    return Irb_File_Name;
}

void B2W_CONTEXT::Init_context(INT abi_bit_width){

  _align_bytes = (UINT) abi_bit_width / 8;
  if (abi_bit_width == 64) ABI_Name = (char *) "n64"; // TARGET_64BIT should be defined somewhere
  else ABI_Name = (char *) "n32";
  Div_Split_Allowed = FALSE;
  Recip_Allowed = FALSE;
  WHIRL_Mldid_Mstid_On = TRUE;
  WN_Simp_Fold_LDA = TRUE;  // fold (LDA offset) + const to LDA (offset+const)
  WHIRL_Keep_Cvt_On = TRUE; // so simplifier won't I8I4CVT

  //===========   Initializing Global Var  =============
  B2W_CONTEXT::_pu_info_tab = new PU_Info * [258] {0};
  B2W_CONTEXT::_pu_root     = NULLPTR;
  B2W_CONTEXT::_log_level   = B2W_LVL_DEBUG;
}

void B2W_CONTEXT::Cleanup_files(BOOL, BOOL) {

}

void Cleanup_Files(BOOL one, BOOL two) {
    B2W_CONTEXT::Cleanup_files(one, two);
}

PU_Info * B2W_CONTEXT::Get_pu_info_tab (UINT pos){
    Is_Valid(pos <= 257 && pos >= 0,
            ("[Get_pu_info_tab] [ERROR] while getting a node of PU_Info_Tab\n "
             "reason : limit exceeded %u > 257", pos));
    return B2W_CONTEXT::_pu_info_tab[pos];
}

void B2W_CONTEXT::Set_pu_info_tab (UINT pos, PU_Info * ptr){
    Is_Valid(pos <= 257 && pos >= 0,
            ("[Get_pu_info_tab] [ERROR] while getting a node of PU_Info_Tab\n "
             "reason : limit exceeded %u > 257", pos));
    B2W_CONTEXT::_pu_info_tab[pos] = ptr;
}

void B2W_CONTEXT::Push_current_entry_wn(WN *wn) {
    _curr_entry_wn.push_back(wn);
}

void B2W_CONTEXT::Pop_current_entry_wn() {
    _curr_entry_wn.pop_back();
}

WN * B2W_CONTEXT::Current_entry_wn(void) {
    if (B2W_CONTEXT::_curr_entry_wn.size() == 0) {
        return NULLPTR;
    } else {
        return B2W_CONTEXT::_curr_entry_wn.back();
    }
}

WN_VECCXX B2W_CONTEXT::_pu_stmt_list;

void B2W_CONTEXT::Push_pu_stmt(WN * stmt){
    Is_Valid(stmt != NULLPTR,("[B2W_CONTEXT::Push_pu_stmt] Null Pointer !"));
    _pu_stmt_list.push_back(stmt);
}

WN * B2W_CONTEXT::Pop_pu_stmt(){
    Is_Valid(_pu_stmt_list.size() > 0,
        ("[B2W_CONTEXT::Pop_pu_stmt] Empty List !"));
    WN *   ptr  = _pu_stmt_list.back();
    _pu_stmt_list.pop_back();
    return ptr;
}

WN * B2W_CONTEXT::Pu_last_stmt(){
    Is_Valid(_pu_stmt_list.size() > 0,
        ("[B2W_CONTEXT::Pu_last_stmt] Empty List !"));
    return _pu_stmt_list.back();
}

UINT B2W_CONTEXT::Get_align_width(){
    return _align_bytes;
}

MPPTR B2W_CONTEXT::_mem_pool_ptr = NULLPTR;

MPPTR B2W_CONTEXT::Get_pool() {
    return _mem_pool_ptr;
}

void B2W_CONTEXT::Set_pool(MPPTR memory_pool) {
    Is_Valid(memory_pool != NULL,
        ("[B2W_Set_pool] memory_pool is null\n"));
    _mem_pool_ptr = memory_pool;
}

PU_Info * B2W_CONTEXT::Get_pu_root() {
    Is_Trace_Ex(Tracing() && _pu_root == NULLPTR,
        (TFile, "%s", "[B2W_CONTEXT::Get_pu_root, returning nullptr "
                      "[probably because the lack of any PU ?] \n"));
    return _pu_root;
}

void B2W_CONTEXT::Set_pu_root(PU_Info * pu){
    Is_Valid(pu != NULLPTR,
              ("[B2W_CONTEXT::Set_pu_root, can't set nullptr"));
    _pu_root = pu;
}

/******************************************************************************
 * Extract the file base name (remove the pre-pending folder path)
 *****************************************************************************/
std::string
B2W_CONTEXT::Get_file_base_name(std::string &file_path, FILE_SYSTEM fs_flag)
{
  std::string full_path = file_path;
  char delimiter = '/';
  if (fs_flag == FILE_SYSTEM_WINDOWS) {
    delimiter = '\\';
  } else {
    delimiter = '/';
  }
  // Deal with situation like
  // /a/ -> a,
  // /a/b/c/d/ -> d
  UINT64 pos = full_path.rfind(delimiter, full_path.size());
  if (pos == full_path.length()) {
    return "."; // Marking that this is a folder itself
  }
  if (pos != std::string::npos) {
    return full_path.substr(pos + 1, std::string::npos);
  }
  return full_path;
} /*end: Last_Pathname_Component */


/******************************************************************************
 * Setting up the current file name
 * 1. Extract folder name and file base name
 * 2. Create folder DST info
 * 3. Create file DST info
 * 4. Set current file dst-id.
 *****************************************************************************/
void
B2W_CONTEXT::Set_file (CCHPTR absolute_path, FILE_SYSTEM fs_flag)
{
  if(absolute_path == NULL) return;
  // If this file is already set, skip redundant setting
  if(_current_file_name && strcmp(_current_file_name, absolute_path) == 0)
    return;
  UINT32 f_len = strlen(absolute_path);
  if(_max_file_name_len < f_len + 1) {
    _current_file_name = (CHPTR) realloc(_current_file_name, f_len + 1);
    _max_file_name_len = f_len;
  }
  strncpy(_current_file_name, absolute_path, f_len);
  _current_file_name[f_len] = '\0';

  // dir/file/compile unit for be phase
  std::string file_path(absolute_path);
  // Making sure it's a valid linux or java path starting with / or X:\\...
  Is_True(file_path.size() > 0, ("[B2W_CONTEXT::Set_file] Source file name is empty, fs_flag = %d", (INT32) fs_flag));
  std::string last_comp = Get_file_base_name(file_path, fs_flag);
  std::string folder_name = Get_file_folder_name(file_path, fs_flag);
  // Get the directory id
  UINT dir_num = B2W_get_dir_dst_info((CHPTR) folder_name.c_str());
  _current_file = B2W_get_file_dst_info((CHPTR) last_comp.c_str(), dir_num);
}

CUVEC *B2W_CONTEXT::Get_dir_dst_list() {
  return &_dir_dst_list;
}

CUUPAIRVEC *B2W_CONTEXT::Get_file_dst_list() {
  return &_file_dst_list;
}

UINT B2W_CONTEXT::Get_file_num()
{
  return (UINT) _current_file;
}

void B2W_CONTEXT::Set_last_initv(INITV_IDX idx) {
  _last_initv = idx;
}

INITO_IDX B2W_CONTEXT::Get_last_initv() {
  return _last_initv;
}

#include "stdio.h"
#include "stdlib.h"

CHPTR B2W_CONTEXT::Get_Name(CHPTR flawed_str) {
  if (flawed_str == NULL) {
    ++_anon_num;
    void * mem = MEM_POOL_Alloc(Malloc_Mem_Pool, 11 + sizeof(int) * 3 +1);
    sprintf((CHPTR) mem, ".anonymous.%d", _anon_num);
    return (CHPTR) mem;
  }
  return flawed_str;
}

INT B2W_CONTEXT::Get_default_align() {
  return 8;
}

void B2W_CONTEXT::Set_log_level(INT level) {
  _log_level = level;
}

INT B2W_CONTEXT::Get_log_level() {
  return _log_level;
}

/******************************************************************************
 * Getting the temporary buffer saved earlier
 *****************************************************************************/
void *B2W_CONTEXT::Get_temporaries(UINT64 def) {
  UINT64 temporary = 0;
  if(B2W_CONTEXT::_temporaries.count(def) <= 0) {
    Is_Valid(FALSE, ("[B2W_CONTEXT::Get_temporaries] TEMP_ID not found, given:%llu \n", def));
  }else{
    temporary = B2W_CONTEXT::_temporaries.at(def);
  }
  return (void *) temporary;
}


/******************************************************************************
 * Save a buffer to the temporary list
 * @return a temporary ID, in uint64
 *****************************************************************************/
UINT64 B2W_CONTEXT::Set_temporaries(void *data) {
  Is_Valid(data != NULLPTR, ("[B2W_CONTEXT::Set_temporaries] Please do not set temporaries with NULL value! \n"));
  UINT64 temporary = B2W_CONTEXT::_temps_count;
  B2W_CONTEXT::_temporaries.insert(std::make_pair(temporary, (UINT64) data));
  _temps_count ++;
  return temporary;
}

UINT B2W_CONTEXT::Get_target_ptr_size() {
  return 8;
}

void B2W_CONTEXT::Set_wn_parent(WN *child_or_block, WN *parent) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_WN_MAP), (TFile, "  [B2W_CONTEXT::Set_wn_parent] "));
  if(WN_operator(child_or_block) != OPR_BLOCK) {
    _wn_parent_map[child_or_block] = parent;

    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_INSERT), (TFile, " child(0x%llx)->parent(0x%llx)\n"
      , (UINT64) child_or_block, (UINT64) parent));
  } else {
    _wn_parent_map[child_or_block] = parent;
    WN *child = WN_first(child_or_block);
    while(child) {
      _wn_parent_map[child] = child_or_block;

      Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_INSERT), (TFile, " child(0x%llx)->parent(0x%llx)\n"
        , (UINT64) child, (UINT64) parent));
      child = WN_next(child);
    }
  }
}

void B2W_print_backtrace(){
  int j, nptrs;
  void *buffer[100];
  char **strings;

  nptrs = backtrace(buffer, 100);
  printf("backtrace() returned %d addresses\n", nptrs);

  /* The call backtrace_symbols_fd(buffer, nptrs, STDOUT_FILENO)
      would produce similar output to the following: */

  strings = backtrace_symbols(buffer, nptrs);
  if (strings == NULLPTR) {
    perror("backtrace_symbols");
    exit(EXIT_FAILURE);
  }

  for (j = 0; j < nptrs; j++)
    printf("%s\n", strings[j]);

  fflush(stdout);
}

/******************************************************************************
 * Get_wn_parent
 * @param stmt       WN * stmt
 * @param assert     bool if true, assert when not found
 * @return block     WN * parent block of the stmt
 *****************************************************************************/
WN *B2W_CONTEXT::Get_wn_parent(WN *child, BOOL is_assert) {
  W2WMAP::iterator iter = _wn_parent_map.find(child);
  if(iter != _wn_parent_map.end()) {
    return iter->second;
  }
  else {
    if(is_assert){
      B2W_print_backtrace();
    }
    Is_Valid(!is_assert, ("no parent block found for stmt (0x%llx)", child));
    return NULLPTR;
  }
}

void B2W_CONTEXT::Clear_wn_parent_map() {
  _wn_parent_map.clear();
}

void B2W_CONTEXT::Set_wn_parent_direct(WN *child, WN *parent) {
  _wn_parent_map[child] = parent;
}

void B2W_CONTEXT::Set_comp_unit_dst(DST_IDX idx) {
  _comp_unit_idx = idx;
}

DST_IDX B2W_CONTEXT::Get_comp_unit_dst() {
  return _comp_unit_idx;
}

/*******************************************************************************
 * Extract the folder path of an absolute file path
 * @param absolute_path
 * @return
 ******************************************************************************/
std::string B2W_CONTEXT::Get_file_folder_name(std::string &absolute_path, FILE_SYSTEM fs_flag) {
  if (absolute_path.size() <= 0) {
    // nothing we should do about, using curdir instead
    return Get_Current_Working_Directory();
  }
  UINT64 pos = 0;
  char file_path_separator = '\0';
  if (fs_flag == FILE_SYSTEM_UNIX) {
    // fully qualified Unix-style naming.
    file_path_separator = '/';
  } else if (fs_flag == FILE_SYSTEM_WINDOWS) {
    // fully qualified windows-style naming.
    file_path_separator = '\\';
  } else {
    Is_Valid(FALSE, ("Invalid fs_flag"));
  }
  pos = absolute_path.rfind(file_path_separator);
  if (pos != std::string::npos) {
    if (pos == 0 && fs_flag == FILE_SYSTEM_UNIX ||
    pos <= 1 && fs_flag == FILE_SYSTEM_WINDOWS ||
    pos == 2 && fs_flag == FILE_SYSTEM_WINDOWS && (absolute_path[1] == '\\' || absolute_path[1] == ':')) {
      pos += 1; // to get the leading character
    }
    std::string folder_name = absolute_path.substr(0, pos);
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_INFO),
                (TFile, "[B2W_CONTEXT::Get_file_folder_name] found file folder name = %s\n", folder_name.c_str()));
    return folder_name;
  } else {
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_INFO),
                (TFile, "[B2W_CONTEXT::Get_file_folder_name] failed to find folder name from '%s', use curdir instead\n", absolute_path.c_str()));
    return Get_Current_Working_Directory(); // Use current dir instead
  }
}

CCHPTR B2W_verify_ascii_string(JNIEnv *env, jstring val){
  CCHPTR out = env->GetStringUTFChars(val, JNI_FALSE);
  Is_Valid(strlen(out) == env->GetStringUTFLength(val), ("[Input string not a valid ASCII str, strlen==%d, UTF-len=%d]",
    strlen(out), env->GetStringUTFLength(val)));
  return out;
}

CCHPTR B2W_verify_utf8_string(JNIEnv *env, jstring val){
  CCHPTR out = env->GetStringUTFChars(val, JNI_FALSE);
  return out;
}
