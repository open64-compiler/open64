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
#include <sys/time.h>
#include <sys/resource.h>
#include "const.h"           // TCON...
#include "erglob.h"          // EC_Tlog_Open
#include "report.h"          // VsaRpt_File
#include "printsrc.h"        // Get_File_Full_Path
#include "java_defs.h"       // RT_VERSION
#include "ipsa_compile.h"

// attention, the same macro is defined in opt_defs.h so that this value
// won't be reused by others
#define LD_PLUGIN_TRACE_FLAG   0x400000 /* Dump ld plugin trace info */

// For Preorder_Process_PUs
// HACK!!! in this way we don't need to copy the code twice
#define LD_PLUGIN_REMOVE_MAIN
#include "driver.cxx"

/*
 * WHIRL DRIVER - drive the whole backend process in ld plugin
 */

/*
 * WHIRL_FILE_INFO Implementations
 */
// prepare file information
void
WHIRL_FILE_INFO::Prepare_file() {
  // TODO: write src information in WHIRL IR file
  Irb_File_Name = _filename;
  INT srclen = strlen(_filename);
  Src_File_Name = (char *)malloc(srclen + 3);
  strcpy(Src_File_Name, _filename);
  Src_File_Name[srclen - 1] = 'c';
  Obj_File_Name = (char *)malloc(srclen + 3);
  strcpy(Obj_File_Name, _filename);
  strcat(Obj_File_Name, ".o");
  if(Keep_Flag) {
    Ipsa_File_Name = (char *)malloc(srclen + 3);
    strcpy(Ipsa_File_Name, _filename);
    Ipsa_File_Name[srclen - 1] = 'I';
  } else {
    // generate file_id.I (1.I, 2.I...) for ipsa itermidate filename
    // without -kp, the _filename would be like /tmp/xxxx, for IPSA we'll
    // put all .I files into IPSA workdir, use fileIdx as the filename prefix
    Ipsa_File_Name = (char *)malloc(13); // max_int(10) + strlen(".I") + 1
    sprintf(Ipsa_File_Name, "%d.I", _file_idx);
  }
  //Trc_File_Name = (char *)malloc(srclen + 3);
  //strcpy(Trc_File_Name, _filename);
  //Trc_File_Name[srclen - 1] = 't';
  //Set_Trace_File(Trc_File_Name);
  //Prepare_Source ();
  //Configure_Source(Src_File_Name);
}

void
WHIRL_FILE_CONTEXT::Update_context(WHIRL_FILE_INFO* fi, BOOL syslib)
{
  if (VSA_Single_Report)
    return;
  if (syslib) {
    _vsa_rpt_file = NULL;
    VsaRpt_File = NULL;
    _vsa_txt_file = NULL;
    VsaTxt_File = NULL;
    return;
  }
  // update file names according to DST file table
  char fname[1024];
  Get_Local_File_Name(1, fname, sizeof(fname) - 32);
  char fnametxt[1024];
  strcpy(fnametxt, fname);
  INT fname_len = strlen(fname);
  INT len = fname_len - 1;   // skip last \0
  while (len > 0 &&
         fname[len] != '.' && fname[len] != '/') {
    -- len;
  }
  if (fname[len] == '.') {
    snprintf(fname + len, 30, ".%d.v", fi->File_index());
    snprintf(fnametxt + len, 30, ".%d.vtxt", fi->File_index());
  }
  else {
    snprintf(fname + fname_len, 30, ".%d.v", fi->File_index());
    snprintf(fnametxt + fname_len, 30, ".%d.vtxt", fi->File_index());
  }
  _vsa_rpt_file = fopen(fname, "w");
  _vsa_txt_file = fopen(fnametxt, "w");
  if (_vsa_rpt_file == NULL) {
    ErrMsg( EC_Tlog_Open, fname, errno);
  }
  else if (_vsa_txt_file == NULL) {
    ErrMsg( EC_Tlog_Open, fnametxt, errno);
  }
  else {
    VsaRpt_File = _vsa_rpt_file;
    VsaTxt_File = _vsa_txt_file;
    Write_vsarpt_header(_vsa_rpt_file);
    Write_vsarpt_source(_vsa_rpt_file);
    // write header and source for txt_file at the same time as rpt_file
  }
}

void
WHIRL_FILE_CONTEXT::Destroy_file_context()
{
  Delete_TCON_Merge_Map(Malloc_Mem_Pool, _tcon_merge_map);
  if (VSA_Single_Report)
    return;
  if (_vsa_rpt_file != NULL) {
    Write_vsarpt_footer(_vsa_rpt_file);
    _vsa_rpt_file = NULL;
  }
  if (_vsa_txt_file != NULL) {
    // write footer in  Write_vsarpt_footer
    _vsa_txt_file = NULL;
  }
}

/*
 * WHIRL_FILE_MANAGER Implementations
 */
// sym_info for mapping symbols
struct sym_info {
  UINT32 file_id;
  UINT32 sym_id;
  const char* name;
  sym_info(UINT32 f, UINT32 s, const char *n)
    : file_id(f), sym_id(s), name(n) {
  }
};

// update symbol mapping according to symbol resolution
bool
WHIRL_FILE_MANAGER::Update_mapping() {
  std::vector<sym_info> _def_set;
  std::vector<sym_info> _undef_set;

  // collect all def & undef information
  std::vector<WHIRL_FILE_INFO>::iterator it = _files.begin();
  for (; it != _files.end(); ++it) {
    WHIRL_FILE_INFO& info = *it;
    // skip archive or object file in archive
    if (info.File_type() == file_archive ||
        info.File_type() == file_ar_elf)
      continue;
    Is_True(info.File_type() == file_whirl ||
            info.File_type() == file_ar_whirl,
            ("only process whirl file"));
    ST_ID_MAPPING& map = info.Mapping();
    UINT32 count = map.Get_ld_count();
    BOOL in_ar = (info.File_type() == file_ar_whirl);
    for (UINT i = 0; i < count; ++i) {
      const ld_plugin_symbol* sym = map.Get_ld_symbol(i);
      if (sym->resolution == LDPR_PREVAILING_DEF ||
          sym->resolution == LDPR_PREVAILING_DEF_IRONLY ||
          sym->resolution == LDPR_PREVAILING_DEF_IRONLY_EXP)
        _def_set.push_back(sym_info(info.File_index(),
                                    map.Get_st_index(i),
                                    sym->name));
      else if (sym->resolution == LDPR_RESOLVED_IR ||
               sym->resolution == LDPR_PREEMPTED_IR ||
               sym->resolution == LDPR_PREEMPTED_REG ||
               (in_ar && sym->resolution == LDPR_UNDEF))
        _undef_set.push_back(sym_info(info.File_index(),
                                      map.Get_st_index(i),
                                      sym->name));
    }
  }

  // map from undef to def
  std::vector<sym_info>::iterator i = _undef_set.begin();
  for (; i != _undef_set.end(); ++i) {
    const sym_info& undef = *i;
    std::vector<sym_info>::iterator j = _def_set.begin();
    for (; j != _def_set.end(); ++j) {
      const sym_info& def = *j;
      if (undef.file_id == def.file_id)
        continue;
      if (strcmp(undef.name, def.name) == 0) {
        UINT64 undef_id = WHIRL_FILE_MANAGER::Encode_file_st(undef.file_id, undef.sym_id);
        UINT64 def_id = WHIRL_FILE_MANAGER::Encode_file_st(def.file_id, def.sym_id);
        _mapping.insert(std::pair<UINT64, UINT64>(undef_id, def_id));
        break;
      }
    }
  }

  return true;
}

// set options and load components
void
WHIRL_FILE_MANAGER::Load_components() {
  // hard-code parameters here
  Opt_Level = 3;
  Run_lno = Run_cg = 0;
  //Run_preopt = Run_wopt = Run_ipsaopt = 1;
  Run_preopt = Run_ipsaopt = 1;
  load_components(_args.size(), &_args[0]);
}

// global initialization when ld-plugin onload is called
void
WHIRL_FILE_MANAGER::Initialize() {
#ifdef __MINGW32__
  setvbuf(stdout, (char *)NULL, _IOLBF, 0);
  setvbuf(stderr, (char *)NULL, _IOLBF, 0);
#else
  setlinebuf (stdout);
  setlinebuf (stderr);
#endif
  Handle_Signals ();
  MEM_Initialize ();
  Cur_PU_Name = NULL;
  Init_Error_Handler ( 100 );
#if !defined(SHARED_BUILD)
  Set_Error_Tables ( Phases, host_errlist );
#endif
  Set_Error_Line ( ERROR_LINE_UNKNOWN );
  Set_Error_File ( NULL );
  Set_Error_Phase ( "Back End Driver" );
  Set_Error_Descriptor (EP_BE, EDESC_BE);
  Set_Error_Descriptor (EP_CG, EDESC_CG);

  Preconfigure ();

  // hard-code 3 dummy file name just for configuration
  Irb_File_Name = (char *)"xvsa-xfa-dummy.B";
  Src_File_Name = (char *)"xvsa-xfa-dummy.c";
  Obj_File_Name = (char *)"xvsa-xfa-dummy.o";
  strcpy(Vsa_Report_File, "xvsa-xfa-dummy.v");
  strcpy(Vsa_TxtRpt_File, "xvsa-xfa-dummy.vtxt");

  Add_arg((char *)"be"); // assume the executable is be

  // initialize be_symtab mempool
  BE_symtab_initialize_be_scopes();

  Run_vsaopt = FALSE;  // disable rtp file here
  Prepare_Source();
  Run_vsaopt = TRUE;
  Configure_Source(Src_File_Name);

#ifdef Is_True_On
  if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
    MEM_Tracing_Enable();
  }
#endif

  Reset_Timers ();
  Start_Timer(T_IPA_S_Comp);
}

// set output file name from linker plugin callback
void
WHIRL_FILE_MANAGER::Set_output_name(const char *str)
{
  // make a copy of output file name
  _outfile = str;

  // Vsa_Report_File length is FILENAME_MAX+1
  // set up .v file name
  const INT buf_len = FILENAME_MAX;
  INT str_len = strlen(str);
  if (str_len > buf_len - 2) {
    strncpy(Vsa_Report_File, str, buf_len - 2);
    Vsa_Report_File[buf_len - 2] = '.';
    Vsa_Report_File[buf_len - 1] = 'v';
    Vsa_Report_File[buf_len] = '\0';
  }
  if (str_len > buf_len - 5) {
    strncpy(Vsa_TxtRpt_File, str, buf_len - 5);
    Vsa_TxtRpt_File[buf_len - 5] = '.';
    Vsa_TxtRpt_File[buf_len - 4] = 'v';
    Vsa_TxtRpt_File[buf_len - 3] = 't';
    Vsa_TxtRpt_File[buf_len - 2] = 'x';
    Vsa_TxtRpt_File[buf_len - 1] = 't';
    Vsa_TxtRpt_File[buf_len] = '\0';
    return;
  }
  //INT len = str_len - 1;   // skip last \0
  //while (len > 0 &&
  //       str[len] != '.' && str[len] != '/') {
  //  -- len;
  //}
  //if (str[len] == '.') {
  //  strncpy(Vsa_Report_File, str, len + 1);
  //  Vsa_Report_File[len + 1] = 'v';
  //  Vsa_Report_File[len + 2] = '\0';
  //}
  //else {
    snprintf(Vsa_Report_File, buf_len, "%s.v", str);
    snprintf(Vsa_TxtRpt_File, buf_len, "%s.vtxt", str);
  //}
}
// initialize backend when all symbols are read
void
WHIRL_FILE_MANAGER::Initialize_backend() {
  // process command options
  Add_arg(Src_File_Name);
  Process_Command_Line(_args.size(), &_args[0]);

  // set Trc_File_Name if tracing is enabled
  if (Tracing_Enabled) {
    Trc_File_Name = (char *)"xvsa-xfa-dummy.t";
    Set_Trace_File (Trc_File_Name);
    trace_file = Get_Trace_File();
  }

  if (Show_Progress_Percent)
    Display_Progress(PS_BE_INIT, FALSE);   // progress 1%

  // other initializations
  Start_Timer(T_BE_Comp);
  Initialize_Stats ();
  Configure ();
#ifdef Is_True_On
  if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
    MEM_Tracing_Enable();
  }
  if (Get_Trace (TKIND_INFO, TINFO_TIME) ||
      Get_Trace (TKIND_INFO, TINFO_CTIME)) {
    Tim_File = TFile;
  }
#endif
  if ( List_Enabled ) {
    Prepare_Listing_File ();
    List_Compile_Options ( Lst_File, "", FALSE, List_All_Options, FALSE );
  }

  Init_Operator_To_Opcode_Table();

  // adjust options and load components
  Load_components();

  be_debug();
}

// the backend process driver
bool
WHIRL_FILE_MANAGER::Backend_process() {
  // if no valid input files, return true directly
  if (_files.size() == 0) {
    if (Show_Progress_Percent)
      Display_Progress(PS_DONE, TRUE);  // progress 100%
    xexit(0);
  }

  if (Show_Progress_Percent)
    Display_Progress(PS_BE_P1, FALSE);  // progress 5%

  bool ret;
  // update symtab resolution
  ret = Update_symtab();
  if (ret == false) {
    _ldpc.message(LDPL_ERROR, "failed to update symbol resolution information.");
    return false;
  }

  // set up mapping between global var and extern var
  ret = Update_mapping();
  if (ret == false) {
    _ldpc.message(LDPL_ERROR, "failed to update symbol mapping.");
    return false;
  }

  if (verbose) {
    // verify symtab
    Verify_symtab();
    if (Get_Trace(TP_WOPT2, LD_PLUGIN_TRACE_FLAG))
      Dump_symtab(trace_file);
  }

  // initialize ipsa context
  Init_ipsa_context();

  // initialize vsa report file
  if (VSA_Single_Report) {
    Is_True(VsaRpt_File == NULL, ("vsa report file is not NULL"));
    if ((VsaRpt_File = fopen(Vsa_Report_File, "w")) == NULL) {
        ErrMsg( EC_Tlog_Open, Vsa_Report_File, errno);
        Vsa_Report_File[0] = '\0';
    }
    else if ((VsaTxt_File = fopen(Vsa_TxtRpt_File, "w")) == NULL) {
        ErrMsg( EC_Tlog_Open, Vsa_TxtRpt_File, errno);
        Vsa_TxtRpt_File[0] = '\0';
    }
    else {
      Write_vsarpt_header(VsaRpt_File);
      Write_vsarpt_source(VsaRpt_File);
    }
  }

  if (Show_Progress_Percent)
    Display_Progress(PS_BE_P2, FALSE);  // progress 8%

  const char* libpath = _opts.Libpath();
  INT libpath_len = libpath != NULL ? strlen(libpath) : 0;

  IPSA_build_cha_begin();
  std::vector<WHIRL_FILE_INFO>::iterator it;
  // do addr_saved/passed/value_modified propagation
  for (it = _files.begin(); it != _files.end(); ++it) {
    WHIRL_FILE_INFO& info = *it;
    // skip all non-whirl files
    if (info.File_type() != file_whirl &&
        info.File_type() != file_ar_whirl)
      continue;

    File_Index = info.File_index();
    info.Restore_context();

    // build current file's class hierarchy
    IPSA_build_cha();

    // propagate addr_saved/passed/value_modified
    if (!FILE_INFO_is_vtable(info.Whirl_file_info())) {
      ST* st;
      int j;
      FOREACH_SYMBOL (GLOBAL_SYMTAB, st, j ) {
        BOOL addr_saved = ST_addr_saved(st);
        BOOL addr_passed = ST_addr_passed(st);
        BOOL value_modified = ST_is_modified(st);
        if (ST_sclass(st) == SCLASS_EXTERN &&
            (addr_saved || addr_passed || value_modified)) {
          UINT32 file_def;
          ST_IDX stid_def;
          if (Resolve(info.File_index(), ST_st_idx(st), file_def, stid_def)) {
            ST* st_def = Get_file(file_def).St_ptr(stid_def);
            if (addr_saved)
              Set_ST_addr_saved(st_def);
            if (addr_passed)
              Set_ST_addr_passed(st_def);
            if (value_modified)
              Set_ST_is_modified(st_def);
          }
        }
      }
    } else {
      // check vtable file version
      DST_IDX cu_idx = DST_get_compile_unit ();
      DST_INFO *cu_info = DST_INFO_IDX_TO_PTR (cu_idx);
      DST_STR_IDX producer_idx = DST_COMPILE_UNIT_producer(
        DST_ATTR_IDX_TO_PTR(DST_INFO_attributes(cu_info), DST_COMPILE_UNIT));
      char *producer_str = DST_STR_IDX_TO_PTR(producer_idx);
      if(VSA_Check_RT_version && strcmp(producer_str, RT_VERSION)) {
        Report_Message_Fmt(MSG_ERROR, E_SCAN_LIB_INCOMPATIBLE,
          "File %s version [%s] incompatible with xvsa required version [%s]\n",
          info.File_name() ? info.File_name() : "unknown",
          producer_str,
          RT_VERSION);
      }
    }
  }
  IPSA_build_cha_end();

  // for each file read-in WHIRL IR
  UINT file_count = _files.size();
  UINT file_done = 0;
  for (it = _files.begin(); it != _files.end(); ++it) {
    WHIRL_FILE_INFO& info = *it;
    ++ file_done;
    // skip all non-whirl files
    if (info.File_type() != file_whirl &&
        info.File_type() != file_ar_whirl)
      continue;
    BOOL is_syslib = (strncmp(info.File_name(), libpath, libpath_len) == 0);
    File_Index = info.File_index();
    info.Prepare_buffer();
    info.Restore_context();
    info.Update_context(is_syslib);
    // vtable file just for de-virtualization, needn't process pu
    if (!FILE_INFO_is_vtable(info.Whirl_file_info())) {
      // Now Global_PU_Tree points to pu_tree in this file
      BOOL is_rbc = FILE_INFO_is_rbc(info.Whirl_file_info());
      for (PU_Info *current_pu = (PU_Info *)Global_PU_Tree;
          current_pu != NULL;
          current_pu = PU_Info_next(current_pu)) {
        ST_IDX pu_st = PU_Info_proc_sym(current_pu);
        ST* st = ST_ptr(pu_st);
        if (!is_rbc &&
            ST_export(st) == EXPORT_PREEMPTIBLE &&
            !PU_is_rbc(Pu_Table[ST_pu(st)])) {
          UINT32 ld_idx = info.Mapping().Get_ld_index(ST_IDX_index(pu_st));
          const ld_plugin_symbol *sym = info.Mapping().Get_ld_symbol(ld_idx);
          if (sym->resolution == LDPR_PREEMPTED_IR) {
            // this function is preempted and should be skipped.
            // change it to extern. another choice is to add a new PU flag.
            Set_ST_sclass(st, SCLASS_EXTERN);
            continue;
          }
        }
        Preorder_Process_PUs(current_pu);
      }
    }
    info.Save_context();
    // show progress percent
    if (Show_Progress_Percent)
      Display_Progress(PS_WOPT_S + file_done * PS_WOPT_C / file_count, FALSE);
  }

  if (Show_Progress_Percent && file_done != file_count)
    Display_Progress(PS_WOPT_S + PS_WOPT_C, FALSE);

#if 0
  // dump IR for testing
  for (it = _files.begin(); it != _files.end(); ++it) {
    WHIRL_FILE_INFO& info = *it;
    info.Dump_irb(trace_file);
  }
#endif

  Stop_Timer(T_BE_Comp);
  // perform ipsa analyze
  IPSA_analyze();

  IPSA_COMP_DRIVER *ipsa_comp = NULL;
  if(Run_ipsacomp) {
    ipsa_comp = CXX_NEW(IPSA_COMP_DRIVER(this), Malloc_Mem_Pool);
    IPSA_emit(ipsa_comp ? ipsa_comp->Workdir() : NULL);
  }
  Finish_Compilation_Timing ( TFile, Src_File_Name );
#if 0
  // dump IR for testing
  std::vector<WHIRL_FILE_INFO>::reverse_iterator rt;
  for (rt = _files.rbegin(); rt != _files.rend(); ++rt) {
    WHIRL_FILE_INFO& info = *rt;
    info.Dump_irb(trace_file);
  }
#endif


  // destroy ipsa context
  Terminate_ipsa_context();

  // finalize vsa report file
  if (VSA_Single_Report) {
    Write_vsarpt_footer(VsaRpt_File);
    VsaRpt_File = NULL;
  }

  if(Run_ipsacomp) {
    Is_True(ipsa_comp, ("null IPSA_COMP_DRIVER"));
    ipsa_comp->Compile_with_make();
    CXX_DELETE(ipsa_comp, Malloc_Mem_Pool);
  }
  // clean files
  Clean();

#ifdef Is_True_On
  if (Get_Trace (TKIND_ALLOC, TP_MISC)) {
    fprintf (TFile, "\n%s\tMemory allocation information after xfa\n", DBar);
    MEM_Trace ();
  }
#endif

  // Report resource usage
  if ( Get_Trace (TKIND_INFO, TINFO_CTIME) ) {
    struct rusage u;
    if (getrusage(RUSAGE_SELF, &u) == 0) {
      long sec = u.ru_utime.tv_sec + u.ru_stime.tv_sec;
      long msec = u.ru_utime.tv_usec + u.ru_stime.tv_usec;
      if (msec >= 1000000) {
        sec += 1;
        msec -= 1000000;
      }
      fprintf (TFile, "\nreal\t%ld.%03lds\n", sec, msec/1000);
      fprintf (TFile, "user\t%ld.%03lds\n", u.ru_utime.tv_sec, u.ru_utime.tv_usec/1000);
      fprintf (TFile, "sys\t%ld.%03lds\n", u.ru_stime.tv_sec, u.ru_stime.tv_usec/1000);
      fprintf (TFile, "rss\t%ldKB\n", u.ru_maxrss);
    }
  }

  // remove output file
  if (_outfile && !Run_ipsacomp)
    unlink(_outfile);

  // terminate here???
  xexit(0);

  //
  return false;
}

