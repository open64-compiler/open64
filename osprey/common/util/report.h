/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */


#ifndef report_INCLUDED
#define report_INCLUDED

#ifndef defs_INCLUDED

#include "defs.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 *
 * Description:
 *
 * External interface for program analysis report done by the compiler.
 * The basic methodology assumed is the use of
 * fprintf to a report file (which may be, and defaults to, stdout).
 * The support provided by the report package is primarily managing
 * the report file.
 *
 * ====================================================================
 * ====================================================================
 */


//
// any message prinedt out by the analysis phase is to get
// 1. component name - e.g. VSA - middle end name, G++SA - frontend name etc
// 2. category - sensitivity category
// 3. type     - type of analysis
// 4. message  - whatever used by the phase
// All thse 4 components together will form one message and send to a receptor
// in this current implementation, the receptor is simply a "named" file
// Analysis messages are considered as part of regular warning/error output
// This package is to enable an additional output to an external receptor
//
// No constructor/destructor is done here to handle file open etc.
// We follow the same style as original compiler to be consistent
//

#define VSA_FILE_EXTENSION ".v"   // used in driver_util.cxx/Prepare_Source
#define RFile Get_VsaRpt_File()
#define VSA_TXTFILE_EXTENSION ".vtxt" // used in driver_util.cxx/Prepare_Source
#define TXTFile Get_VsaTxt_File()
// VsaRpt_File_Name and VsaTxt_File_name are set in driver_util and
// declared in same place as other files like trace, log, ..


extern FILE* Get_VsaRpt_File(void);
extern FILE* Get_VsaTxt_File(void);
extern void  Write_vsarpt_header(FILE*);
extern void  Write_vsarpt_source(FILE*);
extern void  Write_vsarpt_footer(FILE*);
extern void  Write_vsarpt_issue(FILE*, const char*, const char*);
extern void  Write_vsarpt_environment(FILE*);
extern char  Vsa_Report_File[FILENAME_MAX+1];
extern char  Vsa_TxtRpt_File[FILENAME_MAX+1];
extern FILE* VsaRpt_File;
extern FILE* VsaTxt_File;
extern BOOL  Run_vsaopt;
  
// used in driver.cxx and driver_util.cxx
inline BOOL Need_vsafile(void) { return Run_vsaopt; }
  
typedef struct Analysis_Rpt {
  char        cat[25];       // category (sensitivity), display as bit in string
  UINT32      ty;            // analysis type
  const char *vulname;
  BOOL        fs;
  BOOL        cs;
  BOOL        os;
public:
  Analysis_Rpt(const char*paname, BOOL f, BOOL c, BOOL o)
    : vulname(paname), fs(f), cs(c), os(o)
  {
  }
  const char*  getCat() {
    cat[0] = '[';
    UINT32 len = strlen(vulname);
    for (UINT32 i = 0; i < len; i++)
      cat[i + 1] = vulname[i];
    cat[len + 1] = ']';
    cat[len + 2] = ',';
    // f == Flow, c = Context, o = Object
    cat[len + 3] = '[';
    cat[len + 9] = ']';
    cat[len + 10] = '\0';
    cat[len + 4] = cat[len + 6] = cat[len + 8] = '0';
    if (fs) cat[len + 4] = '1';
    cat[len + 5] = ',';
    if (cs) cat[len + 6] = '1';
    cat[len + 7] = ',';
    if (os) cat[len + 8] = '1';
    return &cat[0];
  }
  const INT32 getType() const     { return ty; }
  const char *getVul() const      { return vulname; }
  BOOL        flowSensitive() const    { return fs; }
  BOOL        contextSensitive() const { return cs; }
  BOOL        objectSensitive() const  { return os; }
} ARptMsg;

#ifdef __cplusplus
}
#endif
#endif /* report_INCLUDED */
