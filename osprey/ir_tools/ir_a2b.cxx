/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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


#include <errno.h>		    /* for sys_errlist */
#include <stdio.h>		    /* for stderr */
#ifdef __MINGW32__
#include <WINDOWS.h>
#else
#include <libgen.h>		    /* for basename() */
#endif /* __MINGW32__ */
#include <sys/stat.h>
#if ! defined(BUILD_OS_DARWIN)
#include <elf.h>
#endif /* ! defined(BUILD_OS_DARWIN) */
#include "defs.h"
#include "mempool.h"
#include "wn.h"			    /* for ir_reader.h */
#include "wn_simp.h"
#include "stab.h"
#include "const.h"
#include "pu_info.h"
#include "irbdata.h"
#include "ir_reader.h"		    /* for IR_reader_init(), etc. */
#include "ir_bwrite.h"		    /* for WN_open_output(), etc. */
#include "ir_bread.h"		    /* for WN_open_input(), etc. */
#include "dwarf_DST_dump.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "config_opt.h"
#include "tracing.h"
#include "profile_com.h"
#include "fb_info.h"

static BOOL simplify_tree = FALSE; /* Should we run the simplifier (for testing purposes) */

extern BOOL 
file_exists (char *path)
{
        INT st;
        struct stat sbuf;
        st = stat(path, &sbuf);
        if (st == -1 && (errno == ENOENT || errno == ENOTDIR))
                return FALSE;
        else
                return TRUE;
}


template <class T>
void
FB_Print (const T* info, INT size, const char* name)
{
    if (size != 0)
	printf ("%s Profile:\n", name);
    for (INT i = 0; i < size; ++i) {
	printf("\t%s id = %d\t", name, i);
	info[i].Print (stdout);
	fputc ('\n', stdout);
    }
}

static void
Print_Feedback_Info (const Pu_Hdr* pu_hdr)
{
    const char* base = (const char*) pu_hdr;
    
    const FB_Info_Invoke* fb_invoke =
	(const FB_Info_Invoke*) (base + pu_hdr->pu_inv_offset);
    FB_Print (fb_invoke, pu_hdr->pu_num_inv_entries, "Invoke");

    const FB_Info_Branch* fb_branch =
	(const FB_Info_Branch*) (base + pu_hdr->pu_br_offset);
    FB_Print (fb_branch, pu_hdr->pu_num_br_entries, "Branch");

    const FB_FREQ* fb_switch =
	(const FB_FREQ*) (base + pu_hdr->pu_switch_offset);
    const INT32* fb_switch_target =
	(const INT32*) (base + pu_hdr->pu_switch_target_offset);
    if (pu_hdr->pu_num_switch_entries != 0)
	printf ("Switch Profile:\n");
    for (INT32 i = 0; i < pu_hdr->pu_num_switch_entries; ++i) {
	FB_Info_Switch info;
	info.freq_targets.insert (info.freq_targets.begin (),
				  fb_switch, fb_switch + *fb_switch_target);
	info.Print (stdout);
	fb_switch += *fb_switch_target;
	++fb_switch_target;
    }
    
    const FB_Info_Loop* fb_loop =
	(const FB_Info_Loop*) (base + pu_hdr->pu_loop_offset);
    FB_Print (fb_loop, pu_hdr->pu_num_loop_entries, "Loop");

    const FB_Info_Circuit* fb_circuit =
	(const FB_Info_Circuit*) (base + pu_hdr->pu_scircuit_offset);
    FB_Print (fb_circuit, pu_hdr->pu_num_scircuit_entries, "Short Circuit");

    const FB_Info_Call* fb_call =
	(const FB_Info_Call*) (base + pu_hdr->pu_call_offset);
    FB_Print (fb_call, pu_hdr->pu_num_call_entries, "Call");
} 


/* Binary to ASCII conversion */
static void
ir_b2a_process_PUs (PU_Info *pu_tree, BOOL stflag, BOOL fbflag)
{
    PU_Info *pu;
    WN *wn;

    for (pu = pu_tree; pu != NULL; pu = PU_Info_next (pu)) {
	if (!stflag) {
	    MEM_POOL_Push(MEM_pu_nz_pool_ptr);
	    MEM_POOL_Push(MEM_pu_pool_ptr);
	}
	Read_Local_Info (MEM_pu_nz_pool_ptr, pu);

	wn = PU_Info_tree_ptr(pu);
	if (simplify_tree) wn = WN_Simplify_Tree(wn);

	Current_PU_Info = pu;
	IR_put_func (wn, NULL);

	if (PU_Info_child(pu)) {
	    ir_b2a_process_PUs(PU_Info_child(pu), stflag, fbflag);
	}

	if (stflag) {
	    SYMTAB_IDX level =
		PU_lexical_level (&St_Table[PU_Info_proc_sym (pu)]);
	    Print_local_symtab (stdout, Scope_tab[level]);
	}

	if (fbflag) {
	    if (PU_Info_feedback_ptr (pu)) {
		const Pu_Hdr* pu_hdr = (const Pu_Hdr*)
		    PU_Info_feedback_ptr (pu);
		printf ("\n********* Feedback Info *********\n");
		Print_Feedback_Info (pu_hdr);
		printf ("=======================================================================\n");
	    }
	}

	if (!stflag) {
	    Free_Local_Info (pu);
	    MEM_POOL_Pop(MEM_pu_nz_pool_ptr);
	    MEM_POOL_Pop(MEM_pu_pool_ptr);
	}
    }
}


static void
ir_b2a (char *global_file, char *input_file, char *output_file, BOOL stflag, 
	BOOL fbflag)
{
    PU_Info *pu_tree;

    if (global_file == NULL) {
	(void)Open_Input_Info (input_file);
    } else {
        (void)Open_Global_Input (global_file);
        (void)Open_Local_Input (input_file);
    }

    Initialize_Symbol_Tables (FALSE);
    New_Scope (GLOBAL_SYMTAB, Malloc_Mem_Pool, FALSE);
    pu_tree = Read_Global_Info (NULL);

    if (output_file != NULL) {
    	if (freopen (output_file, "w+", stdout) == NULL) {
		fprintf (stderr, "Can't open output file %s\n", output_file);
		exit (1);
	}
    }

    IR_reader_init();
    IR_Dwarf_Gen_File_Table(TRUE);

    ir_b2a_process_PUs(pu_tree, stflag, fbflag);

    if (stflag) {
	/* print the symbol tables */
	Print_global_symtab (stdout);
	Dump_DST (stdout);
    }

    Free_Input_Info ();

} // ir_b2a


/* Binary to ASCII conversion */
static void
ir_sel (char *input_file, char *output_file, char *func_name)
{
    WN *wn;
    PU_Info *pu_tree, *pu = NULL;
    BOOL found = FALSE;
    
    (void)Open_Input_Info (input_file);
#ifdef KEY
    Initialize_Symbol_Tables (FALSE);
    New_Scope (GLOBAL_SYMTAB, Malloc_Mem_Pool, FALSE);
#endif
    pu_tree = Read_Global_Info (NULL);

    Open_Output_Info (output_file);

    /* don't bother searching through nested procedures since they can't
       be written out by themselves anyway */

    for (pu = pu_tree; pu != NULL; pu = PU_Info_next (pu)) {
	MEM_POOL_Push(MEM_pu_nz_pool_ptr);
	MEM_POOL_Push(MEM_pu_pool_ptr);
	Read_Local_Info (MEM_pu_nz_pool_ptr, pu);

	wn = PU_Info_tree_ptr(pu);
	if ((WN_opcode(wn) == OPC_FUNC_ENTRY) &&
	    (strcmp(func_name, ST_name(WN_st(wn))) == 0)) {

	    Write_PU_Info (pu);
	    found = TRUE;

	    Free_Local_Info (pu);
	    MEM_POOL_Pop(MEM_pu_nz_pool_ptr);
	    MEM_POOL_Pop(MEM_pu_pool_ptr);

	    /* remove all the other PUs from the list */
	    PU_Info_next(pu) = NULL;
	    PU_Info_child(pu) = NULL;
	    pu_tree = pu;

	    break;
	}

	Free_Local_Info (pu);
	MEM_POOL_Pop(MEM_pu_nz_pool_ptr);
	MEM_POOL_Pop(MEM_pu_pool_ptr);
    }

    if (!found) {
      fprintf (stderr, "Can't find function %s\n", func_name);
      exit (1);
    }

    Write_Global_Info(pu_tree);
    Close_Output_Info ();
    Free_Input_Info ();

} /* ir_sel */

static void
ir_all (char *input_file, char *output_file)
{
    WN *wn;
    PU_Info *pu_tree, *pu = NULL;
    
    (void)Open_Input_Info (input_file);
#ifdef KEY
    Initialize_Symbol_Tables (FALSE);
    New_Scope (GLOBAL_SYMTAB, Malloc_Mem_Pool, FALSE);
#endif
    pu_tree = Read_Global_Info (NULL);

    Open_Output_Info (output_file);

    /* don't bother searching through nested procedures since they can't
       be written out by themselves anyway */

    for (pu = pu_tree; pu != NULL; pu = PU_Info_next (pu)) {
	MEM_POOL_Push(MEM_pu_nz_pool_ptr);
	MEM_POOL_Push(MEM_pu_pool_ptr);
	Read_Local_Info (MEM_pu_nz_pool_ptr, pu);

	wn = PU_Info_tree_ptr(pu);
	Write_PU_Info (pu);

	Free_Local_Info (pu);
	MEM_POOL_Pop(MEM_pu_nz_pool_ptr);
	MEM_POOL_Pop(MEM_pu_pool_ptr);
    }

    Write_Global_Info(pu_tree);
    Close_Output_Info ();
    Free_Input_Info ();

} /* ir_all */

static void
usage (char *progname)
{
  INT a2b, b2a, sel, all;

  // programes on windows are suffixed with .exe, so ignore that part
  a2b = (strncmp (progname, "ir_a2b",6) == 0);
  b2a = (strncmp (progname, "ir_b2a",6) == 0);
  sel = (strncmp (progname, "ir_sel",6) == 0);
  all = (strncmp (progname, "ir_all",6) == 0);
  
  if (a2b) {
      fprintf (stderr, "New symbol table format not supported by ir_a2b (yet)\n");
  } else if (b2a) {
    fprintf (stderr, "Usage: %s [-st] [-v] <Binary IR> [<ASCII IR>]\n",
	     progname);
    fprintf (stderr, "\t(will write to stdout if ascii file not given)\n");
    fprintf (stderr, "\t-st option will also print out the symbol table\n");
    fprintf (stderr, "\t-opcodes option will print out opcodes as seen\n");
    fprintf (stderr, "\t-lines option will print out line numbers\n");
    fprintf (stderr, "\t-global_local <.G file> option will use separate global table\n");
    fprintf (stderr, "\t-sym <.G file> is the same as -global_local\n");
  } else if (sel) {
    fprintf (stderr, "Usage: %s <function> <Binary IR input> [<Binary IR output>]\n", progname);

  } else if (all) {
    fprintf (stderr, "Usage: %s <Binary IR input> [<Binary IR output>]\n", progname);
  }

  exit (1);
}

main (INT argc, char *argv[])
{
    register char *progname;
    register INT a2b, b2a, sel, all;
    char *infile;
    char *outfile;
    INT binarg = 1;
    BOOL stflag = FALSE;
    BOOL fbflag = FALSE;

    
    MEM_Initialize();
    Set_Error_Tables (Phases, host_errlist);
    Init_Error_Handler (10);
    Set_Error_File(NULL);
    Set_Error_Line(ERROR_LINE_UNKNOWN);
    WHIRL_Mldid_Mstid_On = TRUE;
#ifdef __MINGW32__
    progname = argv[0] + strlen (argv[0]);
    while (progname >= argv[0]) {
	if (*progname == '/' || *progname == '\\') {
	    progname++;
	    break;
	}
	progname--;
    }
#else
    progname = basename (argv[0]);
#endif /* __MINGW32__ */

    // weird linux bug with basename where it doesn't strip the leading /
    if (*progname == '/') ++progname;
    // programes on windows are suffixed with .exe, so ignore that part
    a2b = (strncmp (progname, "ir_a2b",6) == 0);
    b2a = (strncmp (progname, "ir_b2a",6) == 0);
    sel = (strncmp (progname, "ir_sel",6) == 0);
    all = (strncmp (progname, "ir_all",6) == 0);

    if (a2b) {
	usage (progname);
    }
    else if (b2a) {
	Read_Global_Data = NULL;
	if (argc < 2)
	    usage(progname);

	while (*argv[binarg] == '-') {
	   if (strncmp(argv[binarg], "-st", 3) == 0) {
	      stflag = TRUE;
	   } else if (strcmp(argv[binarg], "-fb") == 0) {
	       fbflag = TRUE;
	   } else if (strcmp(argv[binarg], "-v") == 0) {
		IR_dump_map_info = TRUE;
	   } else if (strcmp(argv[binarg], "-opcodes") == 0) {
		/* this option is for tracing errors when reading the whirl.
		 * should probably rename Set_Verbose_Info to something
		 * more descriptive. */
    		Set_Verbose_Info(TRUE);	/* print opcodes as seen */
		IR_dump_map_info = TRUE;
	   } else if (strcmp(argv[binarg], "-ws") == 0) {
	      simplify_tree = TRUE;
	      (void) WN_Simplifier_Enable(TRUE);
	   }  else if (strcmp(argv[binarg], "-wsa") == 0) {
	      simplify_tree = TRUE;
	      (void) WN_Simplifier_Enable(TRUE);
	      
	      Roundoff_Level=ROUNDOFF_ANY;
	      Enable_Cfold_Reassociate=TRUE;  /* Re-association allowed? */
	      Rsqrt_Allowed=TRUE;	/* Generate RSQRT instruction? */
	      Recip_Allowed=TRUE;		/* Generate RECIP instruction? */
	      Div_Split_Allowed=TRUE;		/* Change a/b --> a*1/b ? */
	      Enable_Cfold_Aggressive=TRUE;	/* Complex constant folding? */
	      IEEE_Arithmetic = IEEE_ANY;
	      Simp_Multiply_To_Shift = TRUE;
	   }  else if (strcmp(argv[binarg], "-wst") == 0) {
	      Set_Trace(TP_WHIRLSIMP,1);
	      Set_Trace(TP_WHIRLSIMP,2);
	   }  else if (strcmp(argv[binarg], "-pre") == 0) {
	      (void) IR_set_dump_order(TRUE);
	   } else if (strcmp(argv[binarg], "-lines") == 0) {
		IR_dump_line_numbers = TRUE;
	   } else if (strcmp(argv[binarg], "-global_local") == 0 ||
		      strcmp(argv[binarg], "-sym") == 0) {
		++binarg;
		Read_Global_Data = argv[binarg];
	   } else {
	      usage(progname);
	   }
	   ++binarg;
	}
	
	if (argc < binarg+1)
	    usage(progname);
	if (!file_exists(argv[binarg]))
	    usage(progname);

	infile = argv[binarg];
	outfile = argv[binarg+1];
	ir_b2a (Read_Global_Data, infile, outfile, stflag, fbflag);
    }
    else if (sel) {
	if (argc < 3)
	    usage(progname);
	if (!file_exists(argv[2]))
	    usage(progname);

	if (argc == 3) {
	  outfile = (char *) malloc(strlen(argv[1])+3);
	  strcpy(outfile, argv[1]);
	  strcat(outfile, ".B");
	} else {
	  outfile = argv[3];
	}
	ir_sel (argv[2], outfile, argv[1]);
    }
    else if (all) {
	if (argc < 2)
	    usage(progname);
	if (!file_exists(argv[1]))
	    usage(progname);
	outfile = argv[2];
	ir_all (argv[1], outfile);
    }
    else 
	fprintf(stderr, "unrecognized command %s\n", progname);

    exit (0);
} /* main */


/* Dummy definitions to satisify references from routines that got pulled
 * in by the header files but are never called
 */
void
Signal_Cleanup (INT sig) { }

const char *
Host_Format_Parm (INT kind, MEM_PTR parm)
{
    fprintf (stderr, "Internal: Host_Format_Parm () not implemented\n");
    return "";
}

INT8 Debug_Level = 0;
