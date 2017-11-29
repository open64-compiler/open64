/*
 * Copyright (C) 2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* ====================================================================
 * ====================================================================
 *
 * Module: w2f_driver.c
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:42 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/w2f_driver.cxx,v $
 *
 * Revision history:
 *  07-Oct-95 - Original Version
 *
 * Description:
 *
 *  Defines an interface for a DSO version of whirl2f, with facilities
 *  for translating a PU at a time or arbitrary subexpressions.
 *
 *  Note that before any of these routines can be called, general
 *  WHIRL accessor routine initiation must have occurred.  
 *  Furthermore, there is no longer any concept of a .B input file
 *  (although we assume there may be a .B input file-name), only a
 *  WN* input tree and SYMTABs.
 *
 * ====================================================================
 * ====================================================================
 */
#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/w2f_driver.cxx,v $ $Revision: 1.1 $";
#endif

#include <sys/elf_whirl.h>  /* for WHIRL_REVISION */
#include <time.h>
#include <errno.h>          /* For sys_errlist */
#include "whirl2f_common.h" /* For defs.h, config.h, erglob.h, etc. */
#include "config_flist.h"   /* For FLIST command line parameters */
#include "config_list.h"    /* For List_Cite */
#include "w2cf_parentize.h" /* For W2CF_Parent_Map and W2FC_Parentize */
#include "file_util.h"      /* For Last_Pathname_Component */
#include "flags.h"          /* for OPTION_GROUP */
#include "timing.h"         /* Start/Stop Timer */
#include "wn_lower.h"       /* WN_lower() */

#include "const.h"          /* For FOR_ALL_CONSTANTS */
#include "PUinfo.h"
#include "st2f.h"
#include "wn2f.h"
#include "wn2f_stmt.h"

#include "../whirl2f/init.cxx"      /* force include of W2F_Initializer */

#if !defined(SHARED_BUILD)
/* no weak version, so need stub to compile (real version is in libwopt) */
#include "opt_defs.h"
AUX_ID WN_aux (const WN*) { return 0; }

/* from whirl2c */
extern "C" {
void W2C_Cleanup(void) {}
void W2C_Push_PU(const WN* a, WN* b) {}
void W2C_Pop_PU(void) {}
void W2C_Translate_Wn(FILE* a, const WN* b) {}
}
#endif

/* Avoid errors due to uses of "int" in stdio.h macros.
 */
#undef int


/* ====================================================================
 *
 * Local data and macros.
 *
 * ====================================================================
 */

/* Default extensions for input/outfile files: */
static const char *W2F_File_Extension[W2F_NUM_FILES] = 
{
   ".f",       /* Fortran input file */
   ".w2f.f",   /* Fortran output file */
   ".w2f.loc"  /* .loc output file */
};

/* CITE extensions for input/outfile files: */
static const char *W2F_Cite_Extension[W2F_NUM_FILES] = 
{
   ".c",	        /* original input file */
   "-after-lno.f",	/* transformed source */
   ".loc"	        /* .loc output file */
};

/* Get the right extension: */
#define W2F_Extension(i) \
	(List_Cite ? W2F_Cite_Extension[i] : W2F_File_Extension[i])


/* statements that can be skipped in w2f translation
 */
#define W2F_MAX_SKIP_ITEMS 128
static W2CF_SKIP_ITEM Skip[W2F_MAX_SKIP_ITEMS+1];
static INT Next_Skip_Item = 0;


/* W2F status information */
static BOOL         W2F_Initialized = FALSE;
static BOOL         W2F_Outfile_Initialized = FALSE;
static FORMAT_KIND  W2F_Format_Kind = F77_TAB_FORMAT;
static WN2F_CONTEXT Global_Context = INIT_WN2F_CONTEXT;
static const char  *W2F_Progname = "";
static const char  *W2F_File_Name[W2F_NUM_FILES] = {NULL, NULL, NULL};
static BOOL         File_Is_Created[W2F_NUM_FILES] = {FALSE, FALSE, FALSE};
static MEM_POOL     W2F_Parent_Pool;

/* External data set through -FLIST command-line options */
FILE  *W2F_File[W2F_NUM_FILES] = {NULL, NULL, NULL};
BOOL   W2F_Enabled = TRUE;          /* Invoke W2F */
BOOL   W2F_Verbose = TRUE;          /* Show translation information */
BOOL   W2F_Old_F77 = FALSE;         /* Use macros for new intrinsics */
BOOL   W2F_Ansi_Format = FALSE;     /* Line-formatting to f77 standard */
BOOL   W2F_No_Pragmas = FALSE;      /* By default, emit pragmas */
BOOL   W2F_Emit_Prefetch = FALSE;   /* Emit comments for prefetches */
BOOL   W2F_Emit_All_Regions = FALSE;/* Emit cmplr-generated regions */
BOOL   W2F_Emit_Linedirs = FALSE;   /* Emit preproc line-directives */
BOOL   W2F_Emit_Nested_PUs = FALSE; /* Emit code for nested PUs */
BOOL   W2F_Emit_Frequency = FALSE;  /* Emit feedback frequency info */
BOOL   W2F_Emit_Cgtag = FALSE;      /* Emit codegen tags for loop */
BOOL   W2F_Emit_Pcf = FALSE;        /* Force Pcf pragmas wherever possible */
BOOL   W2F_Emit_Omp = FALSE;        /* Force OMP pragmas wherever possible */
INT32  W2F_Line_Length = 0;         /* 'zero' means: use the default */

/* External data set through the API or otherwise */
BOOL    WN2F_F90_pu = FALSE;        /* Global variable indicating F90 or F77 */
WN_MAP  W2F_Frequency_Map = WN_MAP_UNDEFINED; /* Frequency mapping */


/* ====================================================================
 *
 * Process_Filename_Options()
 *
 *    Once the command line is parsed, check for option interaction.
 *
 * Open_Read_File()
 *    Opens the file with the given name and path for reading.
 *
 * Open_Append_File()
 *
 *    Opens the file with the given name for appending more to its
 *    end if it already exists, or to create it if it does not
 *    already exist.
 *
 * Open_Create_File()
 *
 *    Same as Open_Append_File(), but a new file is always created,
 *    possibly overwriting an existing file.
 *
 * Close_File()
 *
 *    Closes the given file if different from NULL and not stdout or
 *    stderr.
 *
 * Open_W2f_Output_File()
 *
 *    Assuming that Process_Filename_Options() has been called, open
 *    the given kind of output file.  No effect if the file is already
 *    open, and the output will be appended to the output file if the
 *    file has already been created by this process.
 *
 * Close_W2f_Output_File()
 *
 *    If the file-pointer is non-NULL, we assume the file is open and
 *    close it.  Otherwise, this operation has no effect.
 *
 * ====================================================================
 */

static void 
Process_Filename_Options(const char *src_filename, const char *irb_filename)
{
   /* The name of the original source can be used to derive the 
    * names of the output .c and .h files when these are not 
    * explicitly provided.  If no original source file-name 
    * (-W2F:src_file) has been specified, then we assume the name
    * given by src_file_name, irb_file_name, or "anonymous.c".  
    * The -W2F:ftn_file name is derived from the resultant
    * -W2F:src_file, unless it is explicitly provided.
    *
    * Postcondition:
    *
    *   (W2F_File_Name[W2F_ORIG_FILE] != NULL) &&
    *   (W2F_File_Name[W2F_FTN_FILE] != NULL)
    */
   #define MAX_FNAME_LENGTH 256-7 /* allow for suffix ".w2f.f\0" */
   static char filename[MAX_FNAME_LENGTH+7];
   char       *fname;
   
   /* If we do not have an original source file name, then invent one */
   if (W2F_File_Name[W2F_ORIG_FILE] == NULL)
   {
      if (src_filename != NULL && src_filename[0] != '\0')
	 W2F_File_Name[W2F_ORIG_FILE] = src_filename;
      else if (irb_filename != NULL && irb_filename[0] != '\0')
	 W2F_File_Name[W2F_ORIG_FILE] = irb_filename;
      else
	 W2F_File_Name[W2F_ORIG_FILE] = "anonymous.f";
   }

   /* Copy the original file-name to a static buffer */
   if (strlen(W2F_File_Name[W2F_ORIG_FILE]) > MAX_FNAME_LENGTH)
   {
      W2F_File_Name[W2F_ORIG_FILE] = 
	 strncpy(filename, W2F_File_Name[W2F_ORIG_FILE], MAX_FNAME_LENGTH);
      filename[MAX_FNAME_LENGTH] = '\0';
      fprintf(stderr, 
	      "WARNING: src_file name truncated to (max=%d chars): \"%s\"\n",
	      MAX_FNAME_LENGTH, W2F_File_Name[W2F_ORIG_FILE]);
   }
   else
      W2F_File_Name[W2F_ORIG_FILE] =
	 strcpy(filename, W2F_File_Name[W2F_ORIG_FILE]);

   /* We want the output files to be created in the current directory,
    * so strip off any directory path, and substitute the suffix 
    * appropriately.
    */
   fname = Last_Pathname_Component(filename);
   if (W2F_File_Name[W2F_FTN_FILE] == NULL)
   {
      W2F_File_Name[W2F_FTN_FILE] = 
	 New_Extension(fname, W2F_Extension(W2F_FTN_FILE));
   }
   if (W2F_File_Name[W2F_LOC_FILE] == NULL)
   {
      if (List_Cite)
      {
	 W2F_File_Name[W2F_LOC_FILE] =
	    New_Extension(fname, W2F_Extension(W2F_LOC_FILE));
      }
   }
} /* Process_Filename_Options */


static FILE *
Open_Read_File(const char *filename)
{
   FILE *f = NULL;

   /* Open the input file */
   if (filename == NULL || 
       (f = fopen(filename, "r")) == NULL)
   {
      ErrMsg(EC_IR_Open, filename, errno);
   }
   return f;
} /* Open_Read_File */


static FILE *
Open_Append_File(const char *filename)
{
   FILE *f = NULL;

   /* Open the input file */
   if (filename == NULL || 
       (f = fopen(filename, "a")) == NULL)
   {
      ErrMsg(EC_IR_Open, filename, errno);
   }
   return f;
} /* Open_Append_File */


static FILE *
Open_Create_File(const char *filename)
{
   FILE *f = NULL;

   /* Open the input file */
   if (filename == NULL || 
       (f = fopen(filename, "w")) == NULL)
   {
      ErrMsg(EC_IR_Open, filename, errno);
   }
   return f;
} /* Open_Create_File */


static void
Close_File(const char *filename, FILE *afile)
{
  if (afile != NULL             && 
      !Same_File(afile, stdout) && 
      !Same_File(afile, stderr) && 
      fclose(afile) != 0)
  {
     Set_Error_Line(ERROR_LINE_UNKNOWN); /* No line number for error */
     ErrMsg(EC_Src_Close, filename, errno);
  }
} /* Close_File */


static void 
Open_W2f_Output_File(W2F_FILE_KIND kind)
{
   if (W2F_File[kind] == NULL)
   {
      if (File_Is_Created[kind])
      {
	 W2F_File[kind] = Open_Append_File(W2F_File_Name[kind]);
      }
      else
      {
	 W2F_File[kind] = Open_Create_File(W2F_File_Name[kind]);
	 File_Is_Created[kind] = TRUE;
      }
   } /* if (!files_are_open) */
} /* Open_W2f_Output_File */


static void
Close_W2f_Output_File(W2F_FILE_KIND kind)
{
   Close_File(W2F_File_Name[kind], W2F_File[kind]);
   W2F_File[kind] = NULL;
} /* Close_W2f_Output_File */


/* ====================================================================
 * Begin_New_Location_File: should be called when we start processing
 *     a new file for which we want locations information.
 *
 * End_Locations_File: should be called when we end processing of
 *     a new file for which we want locations information.
 *
 * Continue_Locations_File: should be called for each PU translation 
 *     for which we want locations information.  After the PU
 *     translation we should call Close_W2f_Output_File(W2F_LOC_FILE).
 *
 * ==================================================================== 
 */

static void
Begin_New_Locations_File(void)
{
   /* This should only be called every time a new output file
    * is to be generated by whirl2f.  Note that we open and
    * close the file here, to make sure we only generate entries
    * in the location file when W2F_Outfile_Translate_Pu() is called!
    */
   if (W2F_File_Name[W2F_LOC_FILE] != NULL)
   {
      /* Need to do this before writing to a file for which a 
       * SRCPOS mapping should be maintained.
       */
      Open_W2f_Output_File(W2F_LOC_FILE);
      Write_String(W2F_File[W2F_LOC_FILE], NULL/* No srcpos map */,
                   "(SRCPOS-MAP\n");
    }
} /* Begin_New_Locations_File */


static void
End_Locations_File(void)
{
   /* This should only be called every time we are done with *all*
    * location information for a file.
    */
   if (W2F_File_Name[W2F_LOC_FILE] != NULL)
   {
      /* Need to do this before writing to a file for which a 
       * SRCPOS mapping should be maintained.
       */
      Open_W2f_Output_File(W2F_LOC_FILE);
      Write_String(W2F_File[W2F_LOC_FILE], NULL/* No srcpos map */, ")\n");
      Terminate_Token_Buffer(W2F_File[W2F_LOC_FILE]);
      Close_W2f_Output_File(W2F_LOC_FILE);
   }
} /* End_Locations_File */


static void
Continue_Locations_File(void)
{
   /* Need to do this for every PU, i.e. every time 
    * W2F_Outfile_Translate_Pu() is called.
    */
   if (W2F_File_Name[W2F_LOC_FILE] != NULL)
   {
      Open_W2f_Output_File(W2F_LOC_FILE);
   }
} /* Continue_Locations_File */


/* ====================================================================
 *                   Undo side-effects to the incoming WHIRL
 *                   ---------------------------------------
 *
 * W2F_Undo_Whirl_Side_Effects: This subroutine must be called after
 * every translation phase which may possibly create side-effects
 * in the incoming WHIRL tree.  The translation should thus become
 * side-effect free as far as concerns the incoming WHIRL tree.
 *
 * ==================================================================== 
 */

static void
W2F_Undo_Whirl_Side_Effects(void)
{
   Stab_Free_Tmpvars();
   Stab_Free_Namebufs();
}


/* ====================================================================
 *                   Setting up the global symbol table
 *                   ----------------------------------
 *
 * W2F_Enter_Global_Symtab: Enter the global symbols into the w2f
 * symbol table.  The global symbol-table should be the top-of the
 * stack at this point.
 *
 * ==================================================================== 
 */

// function object to enter FLD names in w2f symbol table. 
// See call in W2F_Enter_Global_Symbols

struct enter_fld
{
  void operator() (UINT32 ty_idx, const TY* typ) const 
    {

    TY & ty = (TY &) typ;

    if (TY_kind(ty) == KIND_STRUCT)
      {
	(void)W2CF_Symtab_Nameof_Ty(ty_idx);

	FLD_HANDLE fld = TY_flist(ty);
	FLD_ITER fld_iter = Make_fld_iter(fld);
	do
	  {
	    FLD_HANDLE fld_rt (fld_iter);
	    
	    if (TY_Is_Pointer(FLD_type(fld_rt)))
	      (void)W2CF_Symtab_Nameof_Fld_Pointee(fld);
	    (void)W2CF_Symtab_Nameof_Fld(fld);

	  } while (!FLD_last_field (fld_iter++)); 
      }
  }
} ;

// function object to enter fn & varbl names in w2f symbol table. 
// See call in W2F_Enter_Global_Symbols.

struct enter_st 
{
  void operator() (UINT32 idx, const ST * st) const 
    {
      if ((ST_sym_class(st) == CLASS_VAR && !ST_is_not_used(st)) || 
	   ST_sym_class(st) == CLASS_FUNC)
      {
	TY_IDX ty ;

	(void)W2CF_Symtab_Nameof_St(st);
	
	if (ST_sym_class(st) == CLASS_VAR)
	  ty = ST_type(st);
	else
	  ty = ST_pu_type(st);
	
	if (TY_Is_Pointer(ty))
	  (void)W2CF_Symtab_Nameof_St_Pointee(st);
      }
    }
};


static void
W2F_Enter_Global_Symbols(void)
{

   /* Enter_Sym_Info for every struct or class type in the current 
    * symbol table, with associated fields.  Do this prior to any
    * variable declarations, just to ensure that field names and
    * common block names retain their names to the extent this is
    * possible.
    */

   For_all(Ty_Table,enter_fld());

   /* Enter_Sym_Info for every variable and function in the current 
    * (global) symbol table.  Note that we always invent names for the
    * pointees of Fortran pointers, since the front-end invariably
    * does not generate ST entries for these.
    */

   For_all(St_Table,GLOBAL_SYMTAB,enter_st());



} /* W2F_Enter_Global_Symbols */

/* =================================================================
 * Routines for checking correct calling order of excported routines
 * =================================================================
 */

static BOOL
Check_Outfile_Initialized(const char *caller_name)
{
   if (!W2F_Outfile_Initialized)
      fprintf(stderr, 
	      "NOTE: Ignored call to %s(); call W2F_Outfile_Init() first!\n",
	      caller_name);
   return W2F_Outfile_Initialized;
} /* Check_Outfile_Initialized */

static BOOL
Check_Initialized(const char *caller_name)
{
   if (!W2F_Initialized)
      fprintf(stderr, 
	      "NOTE: Ignored call to %s(); call W2F_Init() first!\n",
	      caller_name);
   return W2F_Initialized;
} /* Check_Initialized */

static BOOL
Check_PU_Pushed(const char *caller_name)
{
   if (PUinfo_current_func == NULL)
      fprintf(stderr, 
	      "NOTE: Ignored call to %s(); call W2F_Push_PU() first!\n",
	      caller_name);
   return (PUinfo_current_func != NULL);
} /* Check_PU_Pushed */


/* =================================================================
 *                 EXPORTED LOW-LEVEL W2F INTERFACE
 *                 --------------------------------
 *
 * See comments in w2f_driver.h
 *
 * =================================================================
 */

BOOL
W2F_Should_Emit_Nested_PUs(void)
{
   return W2F_Emit_Nested_PUs;
} /* W2F_Should_Emit_Nested_PUs */


void
W2F_Process_Command_Line (INT phase_argc, const char *phase_argv[],
			  INT argc, const char *argv[])
{
    /* Get the program name
     */
    if (argv[0] != NULL)
       W2F_Progname = argv[0];

    /* The processing of the FLIST group was moved out to the back-end.
     * Instead of directly using the Current_FLIST, we initiate
     * whirl2f specific variables to hold the values.
     */
    W2F_File_Name[W2F_ORIG_FILE] = FLIST_orig_filename;
    W2F_File_Name[W2F_FTN_FILE] = FLIST_ftn_filename;
    W2F_File_Name[W2F_LOC_FILE] = FLIST_loc_filename;
    W2F_Enabled = FLIST_enabled;
    W2F_Verbose = FLIST_verbose;
    W2F_Old_F77 = FLIST_old_f77;
    W2F_Ansi_Format = FLIST_ansi_format;
    W2F_No_Pragmas = FLIST_no_pragmas;
    W2F_Emit_Prefetch = FLIST_emit_prefetch;
    W2F_Emit_Linedirs = FLIST_emit_linedirs;
    W2F_Emit_All_Regions = FLIST_emit_all_regions;
    W2F_Emit_Nested_PUs = FLIST_emit_nested_pus;
    W2F_Emit_Frequency = FLIST_emit_frequency;
    W2F_Emit_Cgtag = FLIST_emit_cgtag;
    W2F_Emit_Pcf = FLIST_emit_pcf;
    W2F_Emit_Omp = FLIST_emit_omp;
    W2F_Line_Length = FLIST_line_length;

    Process_Filename_Options(Src_File_Name, Irb_File_Name);

    if (W2F_Ansi_Format)
       W2F_Format_Kind = F77_ANSI_FORMAT;
    else
       W2F_Format_Kind = F77_TAB_FORMAT;

} /* W2F_Process_Command_Line */


void
W2F_Init(void)
{
   const char * const caller_err_phase = Get_Error_Phase ();

   /* Initialize the various whirl2c subcomponents, unless they
    * are already initialized.
    */
   if (W2F_Initialized)
      return; /* Already initialized */

   Diag_Init();
   if (W2F_Progname != NULL)
      Diag_Set_Phase(W2F_Progname);
   else
      Diag_Set_Phase("FLIST");
   Diag_Set_Max_Diags(100); /* Maximum 100 warnings by default */

   /* Create a pool to hold the parent map for every PU, one at a time.
    */
   MEM_POOL_Initialize(&W2F_Parent_Pool, "W2f_Parent_Pool", FALSE);
   MEM_POOL_Push(&W2F_Parent_Pool);


   /* Always do this first!*/

   Initialize_Token_Buffer(W2F_Format_Kind);
   if (W2F_Line_Length > 0)
      Set_Maximum_Linelength(W2F_Line_Length);

   /* Enter the global symbols into the symbol table, since that
    * ensures these get the first priority at keeping their names 
    * unchanged (note that a w2f invented name-suffix is added to 
    * disambiguate names).  Note that we can only emit 
    * declarations locally to PUs for Fortran, but we can still 
    * keep a global symbol-table for naming purposes.
    */
   Stab_initialize_flags();

   W2CF_Symtab_Push(); /* Push global (i.e. first ) symbol table */
   W2F_Enter_Global_Symbols();

   /* Initiate the various W2F modules.
    */
   reset_WN2F_CONTEXT(Global_Context);
   ST2F_initialize();
   PUinfo_initialize();
   WN2F_initialize();

   W2F_Initialized = TRUE;
   Diag_Set_Phase(caller_err_phase);
} /* W2F_Init */


void 
W2F_Push_PU(WN *pu, WN *body_part_of_interest)
{
   if (!Check_Initialized("W2F_Push_PU"))
      return;

   Is_True(WN_opcode(pu) == OPC_FUNC_ENTRY, 
	   ("Invalid opcode for W2F_Push_PU()"));

   Stab_initialize();
   Clear_w2fc_flags()  ;

   /* Set up the parent mapping
    */
   MEM_POOL_Push(&W2F_Parent_Pool);
   W2CF_Parent_Map = WN_MAP_Create(&W2F_Parent_Pool);
   W2CF_Parentize(pu);


   /* See if the body_part_of_interest has any part to be skipped
    * by the w2f translator.
    */
   if (WN_opc_operator(body_part_of_interest) == OPR_BLOCK)
   {
      Remove_Skips(body_part_of_interest, 
                   Skip,
                   &Next_Skip_Item,
                   W2F_MAX_SKIP_ITEMS,
                   FALSE /*Not C*/);
   }

   /* Get the current PU name and ST.
    */
   PUinfo_init_pu(pu, body_part_of_interest);
} /* W2F_Push_PU */


void 
W2F_Pop_PU(void)
{
   if (!Check_Initialized("W2F_Pop_PU") ||
       !Check_PU_Pushed("W2F_Pop_PU"))
      return;

   PUinfo_exit_pu();

   /* Restore any removed statement sequence
    */
   if (Next_Skip_Item > 0)
   {
      Restore_Skips(Skip, Next_Skip_Item, FALSE /*Not C*/);
      Next_Skip_Item = 0;
   }

   Stab_finalize();

   WN_MAP_Delete(W2CF_Parent_Map);
   W2CF_Parent_Map = WN_MAP_UNDEFINED;
   MEM_POOL_Pop(&W2F_Parent_Pool);

   W2F_Frequency_Map = WN_MAP_UNDEFINED;
} /* W2F_Pop_PU */


void 
W2F_Set_Frequency_Map(WN_MAP frequency_map)
{
   W2F_Frequency_Map = frequency_map; /* Feedback frequency mapping for wopt */
} /* W2F_Set_Frequency_Map */


const char *
W2F_Get_Transformed_Src_Path(void)
{
   return W2F_File_Name[W2F_FTN_FILE];
} /* W2F_Get_Transformed_Src_Path */


void
W2F_def_ST(FILE *outfile, ST *st)
{
   TOKEN_BUFFER tokens;

   if (!Check_Initialized("W2F_def_ST"))
      return;

   tokens = New_Token_Buffer();
   ST2F_decl_translate(tokens, st);
   Write_And_Reclaim_Tokens(outfile, W2F_File[W2F_LOC_FILE], &tokens);
   W2F_Undo_Whirl_Side_Effects();
} /* W2F_def_ST */


const char * 
W2F_Object_Name(ST *func_st)
{
   return W2CF_Symtab_Nameof_St(func_st);
} /* W2F_Object_Name */


void 
W2F_Translate_Stid_Lhs(char       *strbuf,
		       UINT        bufsize,
		       ST         *stid_st, 
		       STAB_OFFSET stid_ofst, 
		       TY_IDX      stid_ty, 
		       TYPE_ID     stid_mtype)
{
   TOKEN_BUFFER tokens;
   WN2F_CONTEXT context = INIT_WN2F_CONTEXT;

   tokens = New_Token_Buffer();
   if (ST_class(stid_st) == CLASS_PREG)
   {
      ST2F_Use_Preg(tokens, ST_type(stid_st), stid_ofst);
   }
   else
   {
      WN2F_Offset_Symref(tokens, 
			 stid_st,                         /* base-symbol */
			 Stab_Pointer_To(ST_type(stid_st)), /* base-type */
			 stid_ty,                         /* object-type */
			 stid_ofst,                       /* object-ofst */
			 context);
   }
   Str_Write_And_Reclaim_Tokens(strbuf, bufsize, &tokens);
   W2F_Undo_Whirl_Side_Effects();
} /* W2F_Translate_Stid_Lhs */


void 
W2F_Translate_Istore_Lhs(char       *strbuf, 
			 UINT        bufsize,
			 WN         *lhs,
			 STAB_OFFSET istore_ofst, 
			 TY_IDX      istore_addr_ty, 
			 TYPE_ID     istore_mtype)
{
   TOKEN_BUFFER tokens;
   TY_IDX       base_ty;
   WN2F_CONTEXT context = INIT_WN2F_CONTEXT;

   /* Get the base address into which we are storing a value */
   base_ty = WN_Tree_Type(lhs);
   if (!TY_Is_Pointer(base_ty))
      base_ty = istore_addr_ty;

   /* Get the lhs of the assignment (dereference address) */
   tokens = New_Token_Buffer();
   WN2F_Offset_Memref(tokens, 
		      lhs,                   /* base-symbol */
		      base_ty,               /* base-type */
		      TY_pointed(istore_addr_ty), /* object-type */
		      istore_ofst,           /* object-ofst */
		      context);
   Str_Write_And_Reclaim_Tokens(strbuf, bufsize, &tokens);
   W2F_Undo_Whirl_Side_Effects();
} /* W2F_Translate_Istore_Lhs */


void 
W2F_Translate_Wn(FILE *outfile, WN *wn)
{
   TOKEN_BUFFER       tokens;
   WN2F_CONTEXT       context = INIT_WN2F_CONTEXT;
   const char * const caller_err_phase = Get_Error_Phase ();

   if (!Check_Initialized("W2F_Translate_Wn") ||
       !Check_PU_Pushed("W2F_Translate_Wn"))
      return;

   Start_Timer(T_W2F_CU);
   if (W2F_Progname != NULL)
      Diag_Set_Phase(W2F_Progname);
   else
      Diag_Set_Phase("FLIST");

   tokens = New_Token_Buffer();
   (void)WN2F_translate(tokens, wn, context);
   Write_And_Reclaim_Tokens(outfile, W2F_File[W2F_LOC_FILE], &tokens);
   W2F_Undo_Whirl_Side_Effects();

   Stop_Timer (T_W2F_CU);
   Diag_Set_Phase(caller_err_phase);
} /* W2F_Translate_Wn */


void 
W2F_Translate_Wn_Str(char *strbuf, UINT bufsize, WN *wn)
{
   TOKEN_BUFFER       tokens;
   WN2F_CONTEXT       context = INIT_WN2F_CONTEXT;
   const char * const caller_err_phase = Get_Error_Phase ();

   if (!Check_Initialized("W2F_Translate_Wn_Str") ||
       !Check_PU_Pushed("W2F_Translate_Wn_Str"))
      return;

   Start_Timer (T_W2F_CU);
   if (W2F_Progname != NULL)
      Diag_Set_Phase(W2F_Progname);
   else
      Diag_Set_Phase("FLIST");

   tokens = New_Token_Buffer();
   (void)WN2F_translate(tokens, wn, context);
   Str_Write_And_Reclaim_Tokens(strbuf, bufsize, &tokens);
   W2F_Undo_Whirl_Side_Effects();

   Stop_Timer (T_W2F_CU);
   Diag_Set_Phase(caller_err_phase);
} /* W2F_Translate_Wn_Str */


void
W2F_Fini(void)
{
   /* Ignore this call if W2F_Outfile_Initialized, since a call to
    * W2F_Outfile_Fini() must take care of the finalization for
    * this case.
    */
   INT i;

   if (!Check_Initialized("W2F_Fini"))
      return;
   else if (!W2F_Outfile_Initialized)
   {

      ST2F_finalize();
      PUinfo_finalize();
      WN2F_finalize();
      W2CF_Symtab_Terminate();
      Stab_finalize_flags();

      if (W2F_File_Name[W2F_LOC_FILE] != NULL)
	 End_Locations_File(); /* Writes filenames-table to file */
      else
	 Terminate_Token_Buffer(NULL);
      Diag_Exit();

      /* Reset all global variables */

      W2F_Initialized = FALSE;
      W2F_Format_Kind = F77_TAB_FORMAT;
      reset_WN2F_CONTEXT(Global_Context);
      W2F_Progname = "";
      for (i=0;i<W2F_NUM_FILES;i++) W2F_File_Name[i] = NULL;
      for (i=0;i<W2F_NUM_FILES;i++) File_Is_Created[i] = FALSE;
      for (i=0;i<W2F_NUM_FILES;i++) W2F_File[i] = NULL;
      W2F_Enabled = TRUE;          /* Invoke W2F */
      W2F_Verbose = TRUE;          /* Show translation information */
      W2F_Old_F77 = FALSE;         /* Use macros for new intrinsics */
      W2F_Ansi_Format = FALSE;     /* Line-formatting to f77 standard */
      W2F_No_Pragmas = FALSE;      /* By default, emit pragmas */
      W2F_Emit_Prefetch = FALSE;   /* Emit comments for prefetches */
      W2F_Emit_All_Regions = FALSE;/* Emit cmplr-generated regions */
      W2F_Emit_Linedirs = FALSE;   /* Emit preproc line-directives */
      W2F_Emit_Nested_PUs = FALSE; /* Emit code for nested MP PUs */
      W2F_Emit_Frequency = FALSE;  /* Emit feedback frequency info */
      W2F_Line_Length = 0;         /* 'zero' means: use the default */

      MEM_POOL_Pop(&W2F_Parent_Pool);
      MEM_POOL_Delete(&W2F_Parent_Pool);
   } /* if (initialized) */
} /* W2F_Fini */


/* =================================================================
 *                  EXPORTED OUTPUT-FILE INTERFACE
 *                  ------------------------------
 *
 * This is the easiest interface for using whirl2f.so, and it
 * maintains output file status and produces a uniform output
 * format based on the -FLIST options.  There are strict rules
 * as to how this interface interacts with the lower-level 
 * interface described above (see .h file for details).
 *
 * W2F_Outfile_Init()
 *    Initializes the output-files for whirl2f, based on the
 *    command-line options.  Will call W2F_Init() is this has
 *    not already been done.
 *
 * W2F_Outfile_Translate_Pu()
 *    Translates a PU, presupposing W2F_Outfile_Init() has been 
 *    called.  The PU will be lowered (for Fortran) and the 
 *    output Fortran code will be appended to the output files.
 *
 * W2F_Output_Fini()
 *    Finalizes a W2F translation by closing the output-files, and
 *    calling W2F_Fini().
 *
 * =================================================================
 */

void
W2F_Outfile_Init(void)
{
   /* Initialize the various whirl2f subcomponents.  It is not
    * always desirable to open a .h file for global declarations,
    * and this can be suppressed (no global declarations will be
    * emitted) by passing emit_global_decls=FALSE.  For even more
    * control over the W2F translation process, use the lower-level
    * translation routines, instead of this more abstract
    * interface.
    */
   time_t systime;

   if (W2F_Outfile_Initialized)
      return; /* Already initialized */

   W2F_Outfile_Initialized = TRUE;
   if (W2F_Verbose)
   {
      if (W2F_File_Name[W2F_LOC_FILE] == NULL)
	 fprintf(stderr, 
		 "%s translates %s into %s, based on source %s\n",
		 W2F_Progname,
		 Irb_File_Name,
		 W2F_File_Name[W2F_FTN_FILE], 
		 W2F_File_Name[W2F_ORIG_FILE]);
      else
	 fprintf(stderr, 
		 "%s translates %s into %s and %s, based on source %s\n", 
		 W2F_Progname,
		 Irb_File_Name,
		 W2F_File_Name[W2F_FTN_FILE],
		 W2F_File_Name[W2F_LOC_FILE],
		 W2F_File_Name[W2F_ORIG_FILE]);
   } /* if verbose */

   /* Initialize the whirl2f modules!
    */
   if (!W2F_Initialized)
      W2F_Init();

   /* Open the output files (and write location mapping header).
    */
   Begin_New_Locations_File();
   Open_W2f_Output_File(W2F_FTN_FILE);

   /* Write out a header-comment into the whirl2f generated 
    * source file.
    */
   systime = time(NULL);
   Write_String(W2F_File[W2F_FTN_FILE], W2F_File[W2F_LOC_FILE],
		"C ********************************************"
		"***************\n"
		"C Fortran file translated from WHIRL ");
   Write_String(W2F_File[W2F_FTN_FILE], W2F_File[W2F_LOC_FILE],
		((systime != (time_t)-1)? 
		 ctime(&systime) : "at unknown time\n"));
   Write_String(W2F_File[W2F_FTN_FILE], W2F_File[W2F_LOC_FILE],
		"C **********************************************"
		"*************\n");

   if (W2F_Old_F77)
   {
      Write_String(W2F_File[W2F_FTN_FILE], W2F_File[W2F_LOC_FILE],
		   "C Include builtin operators "
		   "(TODO: add missing ones into this included file)\n"
		   "#include <whirl2f.h>\n\n");
   }

   W2F_Outfile_Initialized = TRUE;

} /* W2F_Outfile_Init */


void
W2F_Outfile_Translate_Pu(WN *pu)
{
   TOKEN_BUFFER       tokens;
   LOWER_ACTIONS      lower_actions = LOWER_NULL;
   const BOOL         pu_is_pushed = (PUinfo_current_func == pu);
   const char * const caller_err_phase = Get_Error_Phase ();

   if (!Check_Outfile_Initialized("W2F_Outfile_Translate_Pu"))
      return;

   Is_True(WN_opcode(pu) == OPC_FUNC_ENTRY, 
	   ("Invalid opcode for W2F_Outfile_Translate_Pu()"));

   /* Make sure all necessary output files are open.
    */
   Continue_Locations_File();
   Open_W2f_Output_File(W2F_FTN_FILE);

   if (W2F_Emit_Nested_PUs)
      lower_actions = LOWER_MP;

   if (lower_actions != LOWER_NULL)
      pu = WN_Lower(pu, lower_actions, NULL, "W2F Lowering");

   Start_Timer(T_W2F_CU);
   if (W2F_Progname != NULL)
      Diag_Set_Phase(W2F_Progname);
   else
      Diag_Set_Phase("FLIST");

   if (!pu_is_pushed)
      W2F_Push_PU(pu, WN_func_body(pu));

   /* Set the flag for an F90 program unit */

   PU & pucur  = Pu_Table[ST_pu(PUINFO_FUNC_ST)];
   WN2F_F90_pu = PU_f90_lang(pucur) != 0;

   // indented nested f90 routines, close CONTAINS for f90, if reqd..

   BOOL nested = PU_is_nested_func(pucur) && !(PU_mp(pucur)) ;

   tokens = New_Token_Buffer();

   if (nested) 
   {
      WN2F_Emit_End_Stmt(tokens,TRUE);
      Increment_Indentation();
   } else
      WN2F_Emit_End_Stmt(tokens,FALSE);

   

   (void)WN2F_translate(tokens, pu, Global_Context);
   Write_And_Reclaim_Tokens(W2F_File[W2F_FTN_FILE], 
			    W2F_File[W2F_LOC_FILE], 
			    &tokens);

   if (nested)
      Decrement_Indentation();

   if (!pu_is_pushed)
      W2F_Pop_PU();

   W2F_Undo_Whirl_Side_Effects();

   Stop_Timer(T_W2F_CU);
   Diag_Set_Phase(caller_err_phase);
} /* W2F_Outfile_Translate_Pu */


void
W2F_Outfile_Fini(void)
{
   TOKEN_BUFFER  tokens;

   /* This finalization must be complete enough to allow repeated
    * invocations of whirl2c during the same process life-time.
    */
   const char *loc_fname = W2F_File_Name[W2F_LOC_FILE];

   if (!Check_Outfile_Initialized("W2F_Outfile_Fini"))
      return;

   Clear_w2fc_flags()  ;

   // Emit END for nested routines if reqd. 
   // Look at global symtab for initalized COMMON.

   tokens = New_Token_Buffer();

   WN2F_Emit_End_Stmt(tokens,FALSE);

   WN2F_Append_Block_Data(tokens);
   Write_And_Reclaim_Tokens(W2F_File[W2F_FTN_FILE], 
			    W2F_File[W2F_LOC_FILE], 
			    &tokens);

   /* All files must be closed before doing a partial 
    * finalization.
    */
   Close_W2f_Output_File(W2F_FTN_FILE);
   W2F_Outfile_Initialized = FALSE;
   W2F_Fini(); /* End_Locations_File() and sets W2F_Initialized to FALSE */
} /* W2F_Outfile_Fini */


void
W2F_Cleanup(void)
{
   /* Cleanup in case of error condition (or a forgotten call to Anl_Fini())
    */
   Close_W2f_Output_File(W2F_LOC_FILE);
   Close_W2f_Output_File(W2F_FTN_FILE);
   if (W2F_File_Name[W2F_LOC_FILE] != NULL)
      unlink(W2F_File_Name[W2F_LOC_FILE]);
}
