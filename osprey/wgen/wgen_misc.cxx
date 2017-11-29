/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007 PathScale, LLC.  All Rights Reserved.
 */
/*
 * Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

/* 
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
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

extern "C" {
#include "gspin-wgen-interface.h"
}

#if defined(BUILD_OS_DARWIN)
#include <limits.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <values.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/types.h>
#include <errno.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include "defs.h"
#include "config.h"
#include "config_opt.h"	// for Div_Split_Allowed
#include "config_debug.h"
#include "config_list.h"
#include "config_targ_opt.h"
#include "controls.h"
#include "erglob.h"
#include "erlib.h"
#include "file_util.h"
#include "flags.h"
#include "glob.h"
#include "mempool.h"
#include "tracing.h"
#include "util.h"
#include "errors.h"
#include <stdarg.h>

#include "wn.h"
#include "wn_util.h"
#include "wn_simp.h"
#include "symtab.h"
#include "pu_info.h"
#include "ir_reader.h"
#include "ir_bwrite.h"
#include "wgen_decl.h"
#include "wgen_expr.h"
#include "wgen_dst.h"
#include "wgen_misc.h"
#include "wgen_stmt.h"
#include "c_int_model.h"
#include "wgen_spin_symbol.h"

int  WGEN_Keep_Zero_Length_Structs = TRUE;
PU_Info *PU_Tree_Root = NULL;
#ifdef TARG_X8664
int Reg_Parm_Count = 0;
BOOL SSE_Reg_Parm = FALSE;
#endif

extern void Initialize_IRB (void);	/* In lieu of irbutil.h */

#ifdef FE_GNU_4_2_0
extern void WGEN_Omp_Init (void);
#endif

static void WGEN_Stmt_Stack_Init (void);
static void WGEN_Stmt_Stack_Free (void);
#ifdef KEY
static void WGEN_Guard_Var_Init();
// When region optimization is enabled using -foptimize-regions, we try not
// to close a region as soon as a call stmt finishes. We try to keep it open
// and include as many calls as possible.
// If we got an opportunity but did not close a region in WGEN_Stmt_Append,
// we set the following flag.
bool Did_Not_Terminate_Region = FALSE;

extern int pstatic_as_global;
#endif

// The following taken from gnu/flags.h
enum debug_info_level
{
  DINFO_LEVEL_NONE,     /* Write no debugging info.  */
  DINFO_LEVEL_TERSE,    /* Write minimal info to support tracebacks only.  */
  DINFO_LEVEL_NORMAL,   /* Write info for all declarations (and line table). */
  DINFO_LEVEL_VERBOSE   /* Write normal info plus #define/#undef info.  */
};

/* Specify how much debugging info to generate.  */
enum debug_info_level debug_info_level;
// End gnu/flags.h data decl

BOOL gv_cond_expr = FALSE;


/* ====================================================================
 *
 * Local data.
 *
 * ====================================================================
 */

# define MAX_MSG_LEVEL 2
# define DEF_MSG_LEVEL 2

#ifdef MONGOOSE_CIF
mUINT32 Cif_Level = 0;       	/* CIF level */
#define MAX_CIF_LEVEL 3 
#define DEF_CIF_LEVEL 2 
#endif /* MONGOOSE_CIF */

/* Default file	extensions: */
#define	IRB_FILE_EXTENSION ".B"	/* ACIR file */
#define	IRD_FILE_EXTENSION ".D"	/* Intermediate data file */
#define	ERR_FILE_EXTENSION ".e"	/* Error file */
#define	LST_FILE_EXTENSION ".l"	/* Listing file */
#define	TRC_FILE_EXTENSION ".t"	/* Trace file */
#define DSTDUMP_FILE_EXTENSION ".fe.dst" /* DST dump-file extension */

int trace_verbose = FALSE;
// an_error_severity error_threshold = es_warning;

/* Static data:	command	line information: */
static INT32 Argc;		/* Copy of argc */
static char **Argv;		/* Copy of argv */
static INT32 Source_Arg = 0;	/* Number of current source arg */
static char Dash [] = "-";

/* Internal flags: */
static BOOL Echo_Flag =	FALSE;	/* Echo command	lines */
static BOOL Delete_IR_File = FALSE;	/* Delete SGIR file when done */


/* ====================================================================
 *
 * Cleanup_Files
 *
 * Close all per-source	files involved in a compilation	and prepare the
 * global variables for	the next source.  This routine is externalized
 * for signal cleanup; the report parameter allows suppressing of error
 * reporting during such cleanup.
 *
 * ====================================================================
 */

void
Cleanup_Files (	BOOL report, BOOL delete_dotofile )
{
  /* No	current	line number for	errors:	*/
  Set_Error_Line (ERROR_LINE_UNKNOWN);

  /* Close source file:	*/
  if ( Src_File	!= NULL	&& Src_File != stdin &&	fclose (Src_File) ) {
    if ( report	) ErrMsg ( EC_Src_Close, Src_File_Name,	errno );
  }
  Src_File = NULL;

  /* Close and delete SGIR file: */
  if ( IR_File != NULL && fclose (IR_File) ) {
    if ( report	) ErrMsg ( EC_IR_Close,	IR_File_Name, errno );
  }
  IR_File = NULL;
  if ( Delete_IR_File && unlink	(IR_File_Name) ) {
    if ( report	) ErrMsg ( EC_IR_Delete, IR_File_Name, errno );
  }

  /* Close listing file: */
  if ( Lst_File	!= NULL	&& Lst_File != stdout && fclose	(Lst_File) ) {
    if ( report	) ErrMsg ( EC_Lst_Close, Lst_File_Name,	errno );
  }
  Lst_File = NULL;

  /* Close trace file: */
  Set_Trace_File ( NULL	);

  /* Disable timing file: */
  Tim_File = NULL;

  /* Finally close error file: */
  Set_Error_File ( NULL	);
  Set_Error_Source ( NULL );
}

/* ====================================================================
 *
 * Terminate
 *
 * Do any necessary cleanup and	terminate the program with the given
 * status.
 *
 * ====================================================================
 */

void
Terminate ( INT status )
{
  /* Close and delete files as necessary: */
  Cleanup_Files	( FALSE, FALSE);

  exit (status);
}

/* ====================================================================
 *
 * Warning
 *
 * Utility function to generate a warning message.
 *
 * ====================================================================
 */
void
Warning ( const char * msg )
{
  fprintf ( stderr, "%s: In function %s:\n", Orig_Src_File_Name, ST_name (Scope_tab[CURRENT_SYMTAB].st) );
  fprintf ( stderr, "%s:%d: warning: %s\n", Orig_Src_File_Name, lineno, msg );
}

/* ====================================================================
 *
 * Prepare_Source
 *
 * Process the next source argument and	associated file	control	flags
 * from	the command line.  Pre-process the source file unless
 * suppressed, and initialize output files as required.	 Return	TRUE
 * iff we have a successfully pre-processed source file	left to


 * compile.
 *
 * ====================================================================
 */

static void
Prepare_Source ( void )
{
  INT16	i;
  char *cp;
  char *fname;
  INT16 len;
  BOOL  dashdash_flag = FALSE;

  /* Initialize error handler: */
  Init_Error_Handler ( 100 );
  Set_Error_Line ( ERROR_LINE_UNKNOWN );
  Set_Error_File ( NULL );
  Set_Error_Phase ( "Front End Driver" );

  /* Clear file names: */
  Err_File_Name = Dash;	/* Error file */
  DSTdump_File_Name = NULL; /* DST dump */

  Delete_IR_File = FALSE;
  
  /* We've got a source file name -- open other files.
   * We want them to be created in the current directory, so we
   * strip off the filename only from Src_File_Name for use:
   */
  fname = Last_Pathname_Component ( Src_File_Name );

  /* Error file first to get error reports: */
  if ( Err_File_Name == NULL ) {
    /* Replace source file extension to get error file: */
    Err_File_Name = New_Extension
			( fname, ERR_FILE_EXTENSION	);
  } else if ( *Err_File_Name == '-' ) {
    /* Disable separate error file: */
    Err_File_Name = NULL;
  }
  Set_Error_File ( Err_File_Name );

  /* Trace file next: */
  if ( Trc_File_Name == NULL ) {
    if ( Tracing_Enabled ) {
      /* Replace source file extension to get trace file: */
      Trc_File_Name = New_Extension
			( fname, TRC_FILE_EXTENSION	);
    }
  } else if ( *Trc_File_Name == '-' ) {
    /* Leave trace file on stdout: */
    Trc_File_Name = NULL;
  }
  Set_Trace_File ( Trc_File_Name );
  if ( Get_Trace (TKIND_INFO, TINFO_TIME) ) Tim_File = TFile;

  /* We're ready to pre-process: */
  IR_File_Name = Src_File_Name;

  /* Open the IR file for compilation: */
  if ( Irb_File_Name == NULL ) {
    /* Replace source file extension to get listing file: */
    Irb_File_Name = New_Extension (	fname, IRB_FILE_EXTENSION );
  }

    if ( (Irb_File = fopen ( Irb_File_Name, "w" )) == NULL ) {
      ErrMsg ( EC_IR_Open, IR_File_Name, errno );
      Cleanup_Files ( TRUE, FALSE );	/* close opened files */
      return;
    } else {
      if ( Get_Trace ( TP_MISC, 1) ) {
	fprintf ( TFile, 
	  "\n%sControl Values: Open_Dot_B_File\n%s\n", DBar, DBar );
	Print_Controls ( TFile, "", TRUE );
      }
    }

  /* Configure internal options for this source file */
  Configure_Source ( Src_File_Name );
}


void
WGEN_Init (INT argc, char **argv, char **envp )
{
  Initialize_C_Int_Model();

  MEM_Initialize(); /// init memory

  Handle_Signals(); //// handle signals

  /* Perform preliminary command line processing: */
  Set_Error_Line ( ERROR_LINE_UNKNOWN );
  Set_Error_Phase ( "Front End Driver" ); // say WGEN front end driver
  Preconfigure (); /// what to configure

#ifdef TARG_MIPS
  ABI_Name = "n64";
#endif

#ifdef TARG_IA64
  ABI_Name = "i64";
#endif

#if defined(TARG_IA32) || defined(TARG_X8664)
  if (TARGET_64BIT)
    ABI_Name = "n64"; // TARGET_64BIT should be defined somewhere
  else ABI_Name = "n32";
#endif

#ifdef KEY
  if (lang_cplus)
    pstatic_as_global = TRUE;
  else
    pstatic_as_global = FALSE;
#endif

  Init_Controls_Tbl();
  Argc = argc;
  Argv = argv;
  Configure ();
  IR_reader_init();
  Initialize_Symbol_Tables (TRUE);
  WGEN_Stmt_Stack_Init (); 
  WGEN_Stmt_Init (); 
#ifdef FE_GNU_4_2_0
  WGEN_Omp_Init ();
#endif
  WGEN_Expr_Init (); 
  WHIRL_Mldid_Mstid_On = TRUE;
  WN_Simp_Fold_LDA = TRUE;  // fold (LDA offset) + const to LDA (offset+const)
			    // since the static initialization code relies on it
  WHIRL_Keep_Cvt_On = TRUE; // so simplifier won't I8I4CVT

#ifdef KEY
  WGEN_Guard_Var_Init ();
  WGEN_Guard_Block_Stack_Init(); 
#endif

  Init_Deferred_Function_Stack();
  Init_Deferred_Decl_Init_Stack();
#ifdef KEY // bug 11406: force -OPT:div_split to false in wgen phase
  Div_Split_Allowed = FALSE;
  Recip_Allowed = FALSE;
#endif
} /* WGEN_Init */

void
WGEN_File_Init (INT argc, char **argv)
{
  /* Process each source file: */
  Prepare_Source();
  MEM_POOL_Push (&MEM_src_pool);

  Restore_Cmd_Line_Ctrls();

  /* open output file */
  Open_Output_Info ( Irb_File_Name );
  DST_build(argc, argv); // do initial setup of dst
}

void
WGEN_File_Finish (void)
{
    Verify_SYMTAB (GLOBAL_SYMTAB);
    Write_Global_Info (PU_Tree_Root);
    Close_Output_Info ();
    IR_reader_finish ();
    MEM_POOL_Pop (&MEM_src_pool);
}

void
WGEN_Finish ()
{
  WGEN_Stmt_Stack_Free ();
}

void
WGEN_Check_Errors (int *error_count, int *warning_count, BOOL *need_inliner)
{
  
  /* If we've seen errors, note them and terminate: */
  Get_Error_Count ( error_count, warning_count);
  *need_inliner = wgen_invoke_inliner;
}

#define ENLARGE(x) (x + (x >> 1))
#define WN_STMT_STACK_SIZE 32

typedef struct wn_stmt {
  WN            *wn;
  WGEN_STMT_KIND  kind;
} WN_STMT;

static WN_STMT *wn_stmt_stack;
static WN_STMT *wn_stmt_sp;
static WN_STMT *wn_stmt_stack_last;
static INT      wn_stmt_stack_size;

char * WGEN_Stmt_Kind_Name [wgen_stmk_last+1] = {
  "'unknown'",
  "'function entry'",
  "'function pragma'",
  "'function body'",
  "'region pragmas'",
#ifdef KEY
  "'region body'",
  "'region exits'",
  "'call region pragmas'",
  "'call region body'",
  "'call region exits'",
#endif // KEY
  "'scope'",
  "'if condition'",
  "'if then clause'",
  "'if else clause'",
  "'while condition'",
  "'while body'",
  "'dowhile condition'",
  "'dowhile body'",
  "'for condition'",
  "'for body'",
  "'switch'",
  "'comma'",
  "'rcomma'",
#ifdef KEY
  "'temp_cleanup'",
  "'dummy'",
#endif // KEY
  "'last'"
};

static void
WGEN_Stmt_Stack_Init (void)
{
  wn_stmt_stack_size = WN_STMT_STACK_SIZE;
  wn_stmt_stack      = (WN_STMT *) malloc (sizeof (WN_STMT) *
                                           wn_stmt_stack_size );
  wn_stmt_sp         = wn_stmt_stack - 1;
  wn_stmt_stack_last = wn_stmt_stack + wn_stmt_stack_size - 1;
} /* WGEN_Stmt_Stack_Init */

static void
WGEN_Stmt_Stack_Free (void)
{
  free (wn_stmt_stack);
  wn_stmt_stack = NULL;
} /* WGEN_Stmt_stack_free */

void
WGEN_Stmt_Push (WN* wn, WGEN_STMT_KIND kind, SRCPOS srcpos)
{
  INT new_stack_size;
  
#ifdef KEY
  // Close any existing EH region before we push a new stmt, since we don't
  // know what the new stmt offers, and may have difficulty closing the region
  // then.
  if (opt_regions && wn_stmt_sp)
    Check_For_Call_Region ();
#endif

  if (wn_stmt_sp == wn_stmt_stack_last) {
    new_stack_size = ENLARGE(wn_stmt_stack_size);
    wn_stmt_stack =
      (WN_STMT *) realloc (wn_stmt_stack, new_stack_size * sizeof (WN_STMT));
    wn_stmt_sp = wn_stmt_stack + wn_stmt_stack_size - 1;
    wn_stmt_stack_size = new_stack_size;
    wn_stmt_stack_last = wn_stmt_stack + wn_stmt_stack_size - 1;
  }
  ++wn_stmt_sp;
  wn_stmt_sp->wn   = wn;
  wn_stmt_sp->kind = kind;

  if (srcpos)
    WN_Set_Linenum ( wn, srcpos );
} /* WGEN_Stmt_Push */


WN*
WGEN_Stmt_Top (void)
{
  FmtAssert (wn_stmt_sp >= wn_stmt_stack,
             ("no more entries on stack in function WGEN_Stmt_Top"));

  return (wn_stmt_sp->wn);
} /* WGEN_Stmt_Top */

#ifdef KEY
// A region has started before a call stmt, and it seems its difficult 
// to close the region cleanly. For the time being, we have this function 
// that closes the call region.
// Return 1 if we did close a region.
bool
Check_For_Call_Region (void)
{
  if (emit_exceptions) {
	if (wn_stmt_sp->kind == wgen_stmk_call_region_body) {
		Setup_EH_Region();
		return TRUE;
	}
  }
  return FALSE;
}
#endif // KEY

void
WGEN_Stmt_Append (WN* wn, SRCPOS srcpos)
{
  WN * body;
  WN * last;

#ifdef KEY
  // Bug 11005: This would have been handled later in wn_util, but
  // adding the check here helps prevent generating empty exception
  // regions and helps ensure the call-stmt is appropriately enclosed
  // by the region.
  if (WN_operator(wn) == OPR_BLOCK && WN_first(wn) == NULL)
    return;
#endif

  if (srcpos) {
    WN_Set_Linenum ( wn, srcpos );
    if (WN_operator(wn) == OPR_BLOCK && WN_first(wn) != NULL)
    	WN_Set_Linenum ( WN_first(wn), srcpos );
  }

  body = WGEN_Stmt_Top ();

  if (body) {

    last = WN_last(body);
    WN_INSERT_BlockAfter (body, last, wn);
  }

#ifdef KEY
// This should not ideally be mixed with this function code, but ...
  if (!opt_regions)
    Check_For_Call_Region();
  else if (wn_stmt_sp->kind == wgen_stmk_call_region_body)
    Did_Not_Terminate_Region = TRUE;
#endif // KEY
} /* WGEN_Stmt_Append */


WN*
WGEN_Stmt_Last (void)
{
  WN * body;

  body = WGEN_Stmt_Top ();
  return (WN_last(body));
} /* WGEN_Stmt_Last */


WN *
WGEN_Stmt_Delete ()
{
  WN * body;
  WN * last;
  WN * prev;

  body = WGEN_Stmt_Top ();
  last = WN_last(body);
  prev = WN_prev(last);
  if (prev)
    WN_next(prev)  = NULL;
  else
    WN_first(body) = NULL;
  WN_last(body) = prev;
  WN_prev(last) = NULL;

  return last;
} /* WGEN_Stmt_Delete */


WN*
WGEN_Stmt_Pop (WGEN_STMT_KIND kind)
{
  WN * wn;
  FmtAssert (wn_stmt_sp >= wn_stmt_stack,
             ("no more entries on stack in function WGEN_Stmt_Pop"));

#ifdef KEY
// another hack.
  WN * to_be_pushed = 0;
  if (emit_exceptions && wn_stmt_sp->kind != kind)
  {
    if (!opt_regions || !Did_Not_Terminate_Region)
    {
  	FmtAssert (wn_stmt_sp->kind == wgen_stmk_call_region_body,
             ("mismatch in statements: expected %s, got %s\n",
              WGEN_Stmt_Kind_Name [kind],
              WGEN_Stmt_Kind_Name [wn_stmt_sp->kind]));

	to_be_pushed = WGEN_Stmt_Pop (wgen_stmk_call_region_body);
    }
    else
    { // If we got an opportunity but did not close the region earlier in
      // WGEN_Stmt_Append, then close it now.
    	Check_For_Call_Region ();
	Did_Not_Terminate_Region = FALSE;
    }
  }
#endif // KEY

  FmtAssert (wn_stmt_sp->kind == kind,
             ("mismatch in statements: expected %s, got %s\n",
              WGEN_Stmt_Kind_Name [kind],
              WGEN_Stmt_Kind_Name [wn_stmt_sp->kind]));

  wn = wn_stmt_sp->wn;
  wn_stmt_sp--;

#ifdef KEY
  if (to_be_pushed) 
  	WGEN_Stmt_Push (to_be_pushed, wgen_stmk_call_region_body, Get_Srcpos()); 
#endif

  return (wn);
} /* WGEN_Stmt_Pop */

/*
void process_diag_override_option(an_option_kind kind,
                                  char          *opt_arg)
{
}
*/

#ifdef KEY
// To assist in WGEN_Lhs_Of_Modify_Expr
// This was included as a special case for the torture test
// 990130-1.c where the call to the function bar() has to happen
// before the asm is executed. But, the front-end would
// append the call after the asm using WGEN_Stmt_Append (without
// this function).
void
WGEN_Stmt_Prepend_Last (WN* wn, SRCPOS srcpos)
{
  WN * body;
  WN * last;

  if (srcpos) {
    WN_Set_Linenum ( wn, srcpos );
    if (WN_operator(wn) == OPR_BLOCK && WN_first(wn) != NULL)
    	WN_Set_Linenum ( WN_first(wn), srcpos );
  }

  body = WGEN_Stmt_Top ();

  if (body) {

    last = WN_last(body);
    WN_INSERT_BlockBefore (body, last, wn);
  }
} /* WGEN_Stmt_Prepend_Last */
#endif /* KEY */

#ifdef KEY
// Stack of VAR_DECLs representing guard variables.  A NULL_TREE means the
// guard variable is needed but not yet allocated.
std::vector<gs_t > guard_vars;

std::vector<WN *> guard_init_block_stack;

void
WGEN_Guard_Block_Stack_Init()
{ 
  // Clear the stack.
  if (lang_cplus)
    guard_init_block_stack.clear();
}

// Indicate a new guard variable is needed.
void
WGEN_Guard_Init_Block_Push()
{
  // treat the current top of stack
  // for guard init. 

  if (lang_cplus)
  { 
    WN * top = WGEN_Stmt_Top(); 
    if (top == NULL)
    { 
       WN *init = WN_CreateBlock(); 
       WGEN_Stmt_Push (init, wgen_stmk_guard_init, Get_Srcpos());
    } 
    guard_init_block_stack.push_back(WGEN_Stmt_Top());
  } 
}

WN *
WGEN_Guard_Init_Block_Pop()
{
  if (!lang_cplus) return NULL;

  FmtAssert (!guard_init_block_stack.empty(), ("WGEN_Guard_Init_Block_Var_Pop: no init blocks to pop "));
  WN *t = guard_init_block_stack.back();
  guard_init_block_stack.pop_back();
  return t;
}

WN *
WGEN_Guard_Init_Block_Stack_Top()
{ 
  // Clear the stack.
  if (lang_cplus)
    return guard_init_block_stack.back(); 
  return NULL; 
}
 
static void
WGEN_Guard_Var_Init()
{
  // Clear the stack.
  if (lang_cplus)
    guard_vars.clear();
}


// Indicate a new guard variable is needed.
void
WGEN_Guard_Var_Push()
{
  if (lang_cplus)
    guard_vars.push_back(NULL);
}


// Get the guard variable currently in effect.  NULL_TREE if guard variable is
// needed but not yet allocated.
gs_t 
WGEN_Guard_Var_Pop()
{
  if (!lang_cplus) return NULL;

  FmtAssert (!guard_vars.empty(), ("WGEN_Guard_Var_Pop: no guard vars to pop"));
  gs_t t = guard_vars.back();
  guard_vars.pop_back();
  return t;
}

// Return the current guard variable.  Allocate one if it doesn't exist.
gs_t 
WGEN_Get_Guard_Var()
{
  gs_t t;

  // Do nothing for C.
  if (!lang_cplus)
    return NULL;

  // Empty stack means no guard variable is needed.
  if (guard_vars.empty())
    return NULL;

  // If top of stack is not NULL_TREE, then it is a valid guard variable.
  t = guard_vars.back();
  if (t != NULL)
    return t;

  // Top of stack is NULL_TREE.  Replace top of stack with a real guard
  // variable.
  guard_vars.pop_back();	// Pop off the NULL_TREE.
  //Set flag gv_cond_expr to indicate a new guard variable for a 
  //conditional expression is being created.
  //See comments for WGEN_add_guard_var in wgen_expr.cxx 
  //for information on conditional expressions.
  gv_cond_expr = TRUE;
  t = gs_build_decl(GS_VAR_DECL, gs_integer_type_node());
  Get_ST(t);
  gv_cond_expr = FALSE;
  guard_vars.push_back(t);	// Push new guard variable onto stack.
  return t;
}

// Currently this function is only used for searching func_entry, but it
// is intended to be a general utility function.
WN *
WGEN_Find_Stmt_In_Stack (WGEN_STMT_KIND kind)
{
  WN_STMT * sp = wn_stmt_sp;
  Is_True (sp, ("Null WN stack pointer"));

  while (sp->kind != wgen_stmk_func_entry)
  {
    if (sp->kind == kind)
      break;
    sp--;
    Is_True (sp, ("FUNC_ENTRY node not found"));
  }
  FmtAssert (sp->kind == kind, ("Stmt kind not found in stack"));
  Is_True (sp->wn, ("Null WN stmt in apparently valid stack location"));
  return sp->wn;
}

#ifdef FE_GNU_4_2_0
// same as in wn_util.cxx
static inline BOOL Pragma_is_Parallel_Region (WN_PRAGMA_ID pragma) {

  switch (pragma) {
  case WN_PRAGMA_PARALLEL_BEGIN:
  case WN_PRAGMA_PARALLEL_SECTIONS:
  case WN_PRAGMA_PARALLEL_DO:
  case WN_PRAGMA_PARALLEL_WORKSHARE:
  case WN_PRAGMA_DOACROSS:
    return TRUE;
  default:
    return FALSE;
  }
}

// Similar to the function with same name in wn_util.cxx, except for the
// last argument "parallel_only". This argument may be set to TRUE for
// certain exception handling objects, that are LOCAL, and need to
// be marked "private" only in the innermost enclosing parallel region.
void Add_Pragma_To_MP_Regions (WN_VECTOR *wnv,
                               WN_PRAGMA_ID pragma_id,
                               ST *st, WN_OFFSET ofst,
                               WN_MAP parent_map,
                               BOOL make_compiler_generated,
                               BOOL parallel_only)
{
  if (!parallel_only) {
    // regular function call
    Add_Pragma_To_MP_Regions (wnv, pragma_id, st, ofst, parent_map,
                              make_compiler_generated);
    return;
  }

  Is_True (pragma_id == WN_PRAGMA_LOCAL,
           ("Add_Pragma_To_MP_Regions: Unexpected pragma"));

  for (WN_VECTOR::iterator wni = wnv->begin();
       wni != wnv->end();
       wni++) {
    //
    // iterate over all the elements, first to last
    // (i.e. from inner-most enclosing WHIRL-MP-REGION to outermost
    //

    WN *region_wn = *wni;

    WN *pragma_wn = WN_first(WN_region_pragmas(region_wn));

    Is_True (WN_opcode(pragma_wn) == OPC_PRAGMA,
             ("Add_Pragma: Expected a pragma node"));
    WN_PRAGMA_ID pragma = (WN_PRAGMA_ID) WN_pragma(pragma_wn);

    if (Pragma_is_Parallel_Region(pragma)) {

      // Don't insert the pragma if it is already there.
      // This is, however, not required for correctness/performance
      {
        WN * pwn = pragma_wn;
        BOOL match = FALSE;
        while (pwn)
        {
          if (WN_st_idx (pwn) == ST_st_idx (st) &&
              (WN_PRAGMA_ID) WN_pragma (pwn) == pragma_id)
          {
            match = TRUE;
            break;
          }
          pwn = WN_next (pwn);
        }
        if (match)
          break; // do nothing
      }

      WN *local_pwn = WN_CreatePragma (pragma_id, st, ofst, 0);
      if (make_compiler_generated) {
        WN_set_pragma_compiler_generated(local_pwn);
      }
      WN *last = WN_last(WN_region_pragmas(region_wn));
      if (last &&
          WN_opcode(last) == OPC_PRAGMA &&
          WN_pragma(last) == WN_PRAGMA_END_MARKER)
        WN_INSERT_BlockBefore (WN_region_pragmas(region_wn), last, local_pwn);        else
        WN_INSERT_BlockBefore (WN_region_pragmas(region_wn), NULL, local_pwn);
      if (parent_map != WN_MAP_UNDEFINED)
      {
        WN_MAP_Set(parent_map,local_pwn,(void*)WN_region_pragmas(region_wn));
      }
      // No need to insert in outer enclosing parallel regions.
      break;
    }
  }
}
#endif // FE_GNU_4_2_0
#endif // KEY
