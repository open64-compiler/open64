//
// Created by xc5 on 27/7/2018.
//

#include "jgen_include.h"
#include "jgen_global.h"
#include "json_reader.h"
#include "jgen_node.h"
#include "jgen_type.h"
#include "jgen_visitor.h"
#include <string>

using std::string;


// *****  Misc Extra Start

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

void
PPrepare_Source ( char * Src_File_Name )
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

// *****  Misc Extra End
extern BOOL c_omit_external; // for C programs, omit generating functions with
// gs_decl_external = TRUE, which is the default

/***

    Wgen_Decl Prefix ended.

**/

extern char *ABI_Name;

namespace JGEN
{
    void
    JGEN_Init (char *fn)
    {
      //Initialize_Java_Int_Model();
      Initialize_C_Int_Model ();
      MEM_Initialize (); /// init memory
      Handle_Signals (); //// handle signals
      /* Perform preliminary command line processing: */
      Set_Error_Line (ERROR_LINE_UNKNOWN);
      Set_Error_Phase ("Front End Driver"); //end driver
      Preconfigure (); /// what to configure

      if (TARGET_64BIT)
        ABI_Name = (char *) "n64"; // TARGET_64BIT should be defined somewhere
      else ABI_Name = (char *) "n32";

      Init_Controls_Tbl ();
      int argc = 1;
      const char *str = "jwtest";
      char **argv = new char *[2];
      argv[0] = (char *) str;
      Configure ();
      IR_reader_init ();

      // Initialize ST/TY/PU
      Initialize_Symbol_Tables (TRUE);

      //WGEN_Stmt_Stack_Init ();
      //WGEN_Stmt_Init ();

      //WGEN_Omp_Init ();

      //WGEN_Expr_Init ();
      WHIRL_Mldid_Mstid_On = TRUE;
      WN_Simp_Fold_LDA = TRUE;  // fold (LDA offset) + const to LDA (offset+const)
      // since the static initialization code relies on it
      WHIRL_Keep_Cvt_On = TRUE; // so simplifier won't I8I4CVT

      //WGEN_Guard_Var_Init ();
      //WGEN_Guard_Block_Stack_Init();

      // Defered
      //  Init_Deferred_Function_Stack();
      //Init_Deferred_Decl_Init_Stack();
      Div_Split_Allowed = FALSE;
      Recip_Allowed = FALSE;

    } /* JGEN_Init */

    void
    JGEN_File_Init (char *fn)
    {
      /* Process each source file: */
      PPrepare_Source (fn);
      MEM_POOL_Push (&MEM_src_pool);

      Restore_Cmd_Line_Ctrls ();

      /* open output file */
      Open_Output_Info (Irb_File_Name);
      DST_Init (NULL, 0);
      //DST_build(Argc, Argv); // do initial setup of dst
    }

    void
    JGEN_File_Finish (void)
    {
      Verify_SYMTAB (GLOBAL_SYMTAB);
      Write_Global_Info (PU_Tree_Root);
      Close_Output_Info ();
      IR_reader_finish ();
      MEM_POOL_Pop (&MEM_src_pool);
    }

    void
    JGEN_Finish ()
    {
      // WGEN_Stmt_Stack_Free ();
    }

    void
    JGEN_Check_Errors (int *error_count, int *warning_count, BOOL *need_inliner)
    {

      /* If we've seen errors, note them and terminate: */
      Get_Error_Count (error_count, warning_count);
      *need_inliner = JGEN::Config::need_inliner;
    }

    void JGEN_Entry (string fn)
    {

      char *buf = new char[2000];
      if (fn.size () < 1900)
        {
          strcpy (buf, fn.c_str ());
        }

      //assert buf not empty;
      INT error_count, sorry_count;
      BOOL need_inliner;
      struct stat sbuf;
      int st;
      get_err_tables ();
      //  Process_Cc1_Command_Line(gs_cc1_command_line_args(program));
      JGEN_Init (buf);
      JGEN_File_Init (buf);
    }

    void JGEN_Root::finish ()
    {
      JGEN_File_Finish ();
    };

    void JGEN_Root::init ()
    {
      JGEN_Entry (this->output_file);
    }

    void JGEN_Root::init (string &fn)
    {
      this->output_file = fn;
      JGEN_Entry (this->output_file);
    };

    void JGEN_Root::write_types (JGEN::JGEN_Typetree_Base type_tree)
    {
      string q = "SomeWhatType";
      string kind_default_name = "kindDefault";
      // What to DO?
    }

    void JGEN_Root::traverse_decl (JGEN_IR_Decl * decl)
    {
      if(decl != nullptr) {
          logger("-- [Jgen_Root::traverseDecl]");
          JGEN_Visitor visitor{};
          visitor.visit_top_decl ( * decl );
      }
    }

    const string &JGEN_Root::getOutput_file () const
    {
      return output_file;
    }

    void JGEN_Root::setOutput_file (const string &output_file)
    {
      JGEN_Root::output_file = output_file;
    }

    void JGEN_NODE::set_line_info_and_file (int line, string &fn)
    {

    }
}

