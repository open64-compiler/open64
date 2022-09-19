/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

//=============================================================================
// Created by lu.gt@163.com Jason on 10/8/2018.
//=============================================================================
#include "b2w_common.h"
#include "b2w_handler.h"
#include "dwarf_DST_producer.h"
#include <string.h>
#include <iostream>


//=============================================================================
// B2W_init Initialization of B2W
//=============================================================================
void
B2W_init (INT abi_bit_width)
{
     Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_TRACE),
             (stdout, "%s%s\n%s", DBar, "####### B2W (libb2w.so) is loaded and starting  #######", DBar));

    B2W_CONTEXT::Init_context(abi_bit_width);
    //Initialize_Java_Int_Model();
    Initialize_C_Int_Model();
    // Memory Initialization
    MEM_Initialize(); /// init memory
    Set_Error_Line(ERROR_LINE_UNKNOWN);
    Set_Error_Phase("Front End Driver"); //end driver
    Preconfigure(); /// what to configure
    Init_Controls_Tbl();
    Configure ();
    // IR Related *Initialization*
    IR_reader_init ();
    Initialize_Symbol_Tables (TRUE);
} /* B2W_init */

static CCHPTR
Get_producer()
{
  if(FILE_INFO_is_vtable(File_info)) {
    return (CCHPTR) RT_VERSION;
  } else {
    return (CCHPTR) JAVA_PRODUCER;
  }
}
//=============================================================================
//
// Initialization of DST info (setting up dst table, must be invoked before
// B2W_file_set_current_filename())
// 
//=============================================================================
void
B2W_file_init_dst (CHPTR ifn)
{
  DST_Init (NULLPTR, 0);
  // dir/file/compile unit for be phase
  ifn = Make_Absolute_Path(ifn);
  std::string ifname = ifn;
  // If filename is not in a proper directory, give up searching and assume it is in
  if (ifn[0] != '/' || (ifname.size() >= 2 && ifname[1] == ':')) {
    Is_True(FALSE, ("[B2W_CONTEXT::Set_file] Source file name '%s', should be absolute "
                    "path starting with / (on unix) or X: (on-windows)\n", ifn));
    ifname = Get_Current_Working_Directory() + ifname;
  }
  ifname = ifname.substr(0, ifname.find_last_of('/'));
  CHPTR dir = (CHPTR) ifname.c_str();
  CHPTR ibfn = Last_Pathname_Component(ifn);

  UINT dir_num = B2W_get_dir_dst_info(dir);
  // TODO: Extraction of the following line.
  // B2W_get_file_dst_info(ibfn, dir_num);

  CCHPTR producer = Get_producer();
  DST_IDX dst_idx = DST_mk_compile_unit(ibfn, dir, (CHPTR) producer, DW_LANG_Java, DW_ID_case_sensitive);
  B2W_CONTEXT::Set_comp_unit_dst(dst_idx);

  Is_Trace_Ex(Tracing(),
	      (TFile, "%s\n %s %s\n %s\n",
	       DBar,
	       "####### B2W File Set DST  ####### \n"
	       " input file path : ",
	       ifn, DBar));
}

void
B2W_file_open_ir(CHPTR ir_file_name) {
  // Write Irb_File_Name
  CCHPTR ir_real_name = B2W_CONTEXT::Prepare_Source (ir_file_name);
  MEM_POOL_Push (&MEM_src_pool);
  // Open output file
  Open_Output_Info ((CHPTR) ir_real_name);
  Is_Trace_Ex(Tracing(),
	      (TFile, "%s\n%s\n", DBar,
	       "####### B2W File Open  #######"));
}

//=============================================================================
// B2W_file_close(void)
// Verify the IR to be dumped and then
// close the ir whirl file with memory left open.(Not popped)
//=============================================================================
void
B2W_file_close(void) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DATA),
	      (TFile, "%s\n%s\n%s", DBar, "########### B2W file closing ###########", DBar));
  Verify_SYMTAB (GLOBAL_SYMTAB);
  // Write Global Info.
  Write_Global_Info (B2W_CONTEXT::Get_pu_root());
  Close_Output_Info ();
}

//=============================================================================
// B2W_file_finish
//=============================================================================
void
B2W_file_finish (void)
{
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DATA),
             (TFile, "%s%s\n%s", DBar, "########### B2W file finishing ###########", DBar));
    // Memory deallocations
    // Just Mem_pop on mem_local_pool
    IR_reader_finish ();
    MEM_POOL_Pop (&MEM_src_pool);
}

//=============================================================================
// B2W_finish
//=============================================================================
void
B2W_finish ()
{
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DATA),
             (TFile, "%s\n%s\n", "########### B2W all finished   ###########", DBar));
    // WGEN_Stmt_Stack_Free ();
    // TODO: Global Finish, pop or other methods.
}

void
B2W_set_file_rbc()
{
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DATA),
             (TFile, "%s\n%s\n", "########### B2W set rbc file   ###########", DBar));
  Set_FILE_INFO_is_rbc(File_info);
}

void
B2W_set_file_vtable()
{
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DATA),
             (TFile, "%s\n%s\n", "########### B2W set vtable file   ###########", DBar));
  Set_FILE_INFO_is_vtable(File_info);
}

//=============================================================================
// B2W_check_errors
//=============================================================================
void
B2W_check_errors (int *error_count, int *warning_count, BOOL *need_inliner)
{
  /* If we've seen errors, note them and terminate: */
  Get_Error_Count (error_count, warning_count);
  //*need_inliner = B2W::Config::need_inliner;
}

/******************************************************************************
 *  Initialize a new pool
 ******************************************************************************/
MPPTR B2W_pool_initialize() {
    MPPTR map_mem_pool      = new MEM_POOL();

    MEM_POOL_Initialize(map_mem_pool, "Map_Mem_Pool", FALSE);
    MEM_POOL_Push(map_mem_pool);

    Is_Valid(map_mem_pool != NULLPTR, ("%s", "[Error] [B2W_pool_initialize]"
                                      " : Cannot Initialize Pool \n"));
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DATA), (TFile, "[B2W_pool_initialize] initialized a pool %llu\n",
        (UINT64) map_mem_pool));

    B2W_CONTEXT::Set_pool(map_mem_pool);

    return (MPPTR) map_mem_pool;
}

/******************************************************************************
 *  Close a pool
 ******************************************************************************/
MPPTR B2W_pool_release(void * mem_pool) {
    MPPTR map_mem_pool      = (MPPTR) mem_pool;
    Is_Valid(map_mem_pool != NULLPTR, ("%s", "[Error] [B2W_pool_release]"
                                   " : Cannot deallocate a NULLPTR Pool "));
    printf("[B2W_pool_release] %llu", (UINT64) map_mem_pool);
    return (MPPTR) map_mem_pool;
}

/******************************************************************************
 *  Delete_Map_Tab
 ******************************************************************************/
void B2W_delete_map(void * Current_Map_Tab) {
    WN_MAP_TAB_Delete((WN_MAP_TAB *) Current_Map_Tab);
}

/******************************************************************************
 *  Delete_Map_Tab
 ******************************************************************************/
void B2W_mem_pool_push(void * Map_Mem_Pool) {
    MEM_POOL_Push((MEM_POOL *) Map_Mem_Pool);
}

void B2W_mem_pool_pop(void * Map_Mem_Pool) {
    MEM_POOL_Pop((MEM_POOL *) Map_Mem_Pool);
}

/******************************************************************************
  *
  *  Starts the JNI/Java Wrapped part
  *
  ****************************************************************************/


/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    bgenInit
 * Signature: (ZII)V
 */
void JNICALL Java_io_xcalibyte_BGenDriver_bgenInit
(JNIEnv *env, jclass clazz, jboolean trace, jint trace_level, jint abiBitWidth) {
    Tracing_Enabled = trace;
    B2W_init(abiBitWidth);
    B2W_CONTEXT::Set_log_level(trace_level);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    bgenInitOpenIrFile
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_bgenInitOpenIrFile
  (JNIEnv *env, jclass, jstring output) {
  Is_Valid(output != NULLPTR, ("JNI call to bgenInitOpenIrFile failed because null string"));
  CCHPTR op = B2W_verify_ascii_string(env, output);
  CCHPTR ofname = strdup(op);            // for longer lifespan
  B2W_file_open_ir((CHPTR) ofname);
  env->ReleaseStringUTFChars(output, op);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    bgenInitSetDST
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_bgenInitSetDST
(JNIEnv *env, jclass, jstring input) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DATA), (TFile, "[B2W_init_open_set_dst] \n"));
  Is_Valid(input != NULLPTR, ("JNI call to bgenInitDST failed because null string"));

  CCHPTR ip     = B2W_verify_ascii_string(env, input);
  CCHPTR ifname = strdup(ip);            // for longer lifespan
  B2W_file_init_dst((CHPTR) ifname);
  env->ReleaseStringUTFChars(input, ip);
}


/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    bgenFileClose
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_bgenFileClose
  (JNIEnv *, jclass) {
    Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DATA), (TFile, "[B2W_bgen_file_close] \n"));
  B2W_file_close();
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    bgenFileFinish
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_bgenFileFinish
  (JNIEnv *, jclass) {
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_DATA), (TFile, "[B2W_bgen_file_finish] \n"));
  B2W_file_finish();
}


/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    outputTrace
 * Signature: (Ljava/lang/String;I)V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_outputTrace
(JNIEnv * env, jclass, jstring trace_output){
  const char *p = B2W_verify_ascii_string(env, trace_output);
  Is_Trace_Ex(Tracing() && B2W_LOG(B2W_LVL_INFO),
    (TFile, "%s %s \n", "###", p));
  env->ReleaseStringUTFChars(trace_output, p);
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setFileRbc
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setFileRbc
  (JNIEnv *, jclass) {
  B2W_set_file_rbc();
}

/*
 * Class:     io_xcalibyte_BGenDriver
 * Method:    setFileVTable
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_io_xcalibyte_BGenDriver_setFileVTable
  (JNIEnv *, jclass) {
  B2W_set_file_vtable();
}
