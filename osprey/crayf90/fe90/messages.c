/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*

  Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.

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



static char USMID[] = "\n@(#)5.0_pl/sources/messages.c	5.9	10/14/99 14:08:59\n";

# include <stdarg.h>

# include "defines.h"		/* Machine dependent ifdefs */


# define  __NLS_INTERNALS  1

# if defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN)
/*  typedef __int32_t;  */
# include <nl_types.h>
# include <nlcatmsg.h>

# else
# include "nl_types.h"
# endif



# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "debug.m"
# include "messages.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "pathscale_defs.h"
# include "messages.h"
#ifdef KEY /* Bug 3632 */
# include "src_input.m"
#endif /* KEY Bug 3632 */
#ifdef KEY /* Bug 6673 */
#include "../liberrno.h"
/* Can remove this pragma and the two "if (verbose_message)" tests later on,
 * once we have a new toolroot libpathfortran.a containing verbose_message. */
#pragma weak verbose_message
#endif /* KEY Bug 3632 */

#  define	 CIF_VERSION	       3    /* Must be defined before         */
                    			    /*   including cif.h	      */

# include "cif.h"


/*****************************************************************\
|* Function prototypes of static functions declared in this file *|
\*****************************************************************/

static int      compare_message_recs(const void *, const void *);
static void     flush_msg_file(void);


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	init_msg_processing is called by compiler initialization in main to   *|
|*	determine the name used when the compiler was invoked.	It can either *|
|*	be passed by a script via an environment variable or is the name used *|
|*	when the compiler is directly invoked.				      *|
|*									      *|
|* Input parameters:							      *|
|*	*argv - pointer to command line argument string			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	value of NLSPATH						      *|
|*									      *|
\******************************************************************************/

#ifdef KEY /* Bug 5089 */
char *init_msg_processing (char *argv[])
#else /* KEY Bug 5089 */
void init_msg_processing (char *argv[])
#endif /* KEY Bug 5089 */

{
   static char	      *allocstr;
#ifdef KEY /* Bug 5089 */
   char *result = getenv("NLSPATH");
#endif /* KEY Bug 5089 */


   TRACE (Func_Entry, "init_msg_processing", NULL);

   /* Unconditionally open the message system */

#if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(TARGET_OS_DARWIN))
#ifdef KEY /* Bug 5089 */
  if (result == NULL)
#else /* KEY Bug 5089 */
  if (getenv("NLSPATH") == NULL)
#endif /* KEY Bug 5089 */
  {
    /* NLSPATH is not set. */
    const char * const toolroot = getenv("TOOLROOT");
    const char * const env_name = "NLSPATH=";
    const char * const env_val = "/usr/lib/locale/C/LC_MESSAGES/%N.cat";
    int len = strlen(env_name) + strlen(env_val) + 1;
    char * new_env;
    if (toolroot != NULL) len += strlen(toolroot);
    new_env = malloc(len);
    if (toolroot == NULL)
      sprintf(new_env, "%s%s", env_name, env_val);
    else
      sprintf(new_env, "%s%s%s", env_name, toolroot, env_val);
    putenv(new_env);
#ifdef KEY /* Bug 5089 */
    result = new_env;
#else /* KEY Bug 5089 */
    free(new_env);
#endif /* KEY Bug 5089 */
  }
#endif

   msg_sys = catopen (group_code, 0);

   if (msg_sys == (nl_catd) -1) {  /* (nl_catd) is msg_sys's type. */

# if defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN)
#ifdef PSC_TO_OPEN64
      fprintf (stderr, OPEN64_NAME_PREFIX "f95 INTERNAL: Unable to open message system.\n");
#endif
# else
      fprintf (stderr, "cf90 INTERNAL: Unable to open message system.\n");
# endif
      exit_compiler(RC_USER_ERROR);

   }

   save_glb_line_num = 0;
   save_column_num   = 0;


   /* Determine the command name used to access the compiler.  This is	      */
   /* needed for message processing and therefore must be done		      */
   /* before any messages are printed.					      */
   /* See if ORIG_CMD_NAME environment variable exists			      */

   command_name = getenv ("ORIG_CMD_NAME");

   if (command_name == NULL) {	/* ORIG_CMD_NAME is not defined; set it.  */ 
      command_name = argv[0];
      command_name = strrchr (argv[0], SLASH);	/* search for rightmost slash */
      command_name = (command_name == NULL) ? argv[0] : command_name+1;
   }
   else {  /* ORIG_CMD_NAME is defined */
      /* Copy it.  Note that allocstr gets the result of memory allocation    */
      /* not command_name.  If an error occurs during memory allocation,      */
      /* command_name is still valid and the message printed will have the    */
      /* correct command name.						      */
      MEM_ALLOC (allocstr, char, strlen(command_name)+1);

      if (allocstr != NULL) {  /* if no memory allocation error... */
	 strcpy (allocstr, command_name); 

	 /* copy is necessary since allocstr could be over written later.     */
	 command_name = allocstr;  
      }
   }

   TRACE (Func_Exit, "init_msg_processing", NULL);

#ifdef KEY /* Bug 5089 */
   return result;
#else /* KEY Bug 5089 */
   return;
#endif /* KEY Bug 5089 */

}  /* init_msg_processing */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine is the 'common message processor' for f90.  It is        *|
|*	called by the f90 front-end.				
|*									      *|
|* Input parameters:							      *|
|*	glb_line_num	 If non-zero:  Global line number of source; this     *|
|*			   number maps onto a line number within the current  *|
|*			   file.					      *|
|*			 If zero    :  Indicates that a message header should *|
|*			   should not be output.			      *|
|*	msg_num		 The message number of the text to be output. 	      *|
|*	msg_severity	 The severity level.				      *|
|*	column_num       If non-zero:  Column is put into the header.         *|
|*			 If zero    :  Column is not put into header.         *|
|*	arg1, arg2, 	 Four optional arguments that can be inserted into    *|
|*	arg3, arg4	 into the message text.				      *|
|*									      *|
|*			 Notes:						      *|
|*                         - No validation check is made for missing or extra *|
|*			     arguments in the message text.  It is up to the  *|
|*		 	     developer to insure that the same number of      *|
|*			     arguments passed to PRINTMSG are in the message  *|
|*			     text.			 		      *|
|*	               							      *|
|*                         - Function cif_message_rec must scan the message   *|
|*                           text for the format descriptors.  To make its    *|
|*                           life easier, the formats are restricted to:      *|
|*                             %c  %d  %f  %s                                 *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void PRINTMSG (int			glb_line_num,
	       int			msg_num,
	       msg_severities_type	msg_severity, 
	       int			column_num, 
	       ... )

{
   long		arg1;
   long		arg2;
   long		arg3;
   long		arg4;
   va_list	arg_ptr;
   boolean	error;
   boolean	exit_now;
   boolean	warning;

# if defined(_DEBUG)
   boolean	issue_line_out_of_range_msg	= FALSE;
# endif


/* END_UNIT is stolen from fecif.m so that the whole header need not be       */
/* included.								      */

# define END_UNIT                18             /* End Unit                   */


   TRACE (Func_Entry, "PRINTMSG", NULL);

# ifdef _DEBUG
   /* Validate message number range. */

   if (msg_num > MAX_MSG || msg_num < 0) {
      PRINTMSG (glb_line_num, 1, Internal, 0, msg_num);
   }

# endif

   exit_now = TRUE;

   error	= GET_MESSAGE_TBL(message_error_tbl, msg_num);
   warning	= GET_MESSAGE_TBL(message_warning_tbl, msg_num);

   if ((error && (msg_severity != Error)) || 
       (warning && msg_severity != Warning)) {  /* Switching msg_severity */

      switch (msg_severity) {
      case Ansi:
      case Comment:
      case Note:
      case Caution:
      case Warning:
         msg_severity = (error) ? Error : Warning;
         break;

      case Error:

         if (warning) {
            PRINTMSG (glb_line_num, 1653, Warning, 0, msg_num, "warning");
         }
         error = FALSE; /* This is already an error level */
         break;

      case Vector:
      case Scalar:
      case Table:
      case Inline:
      case Info:
      case Tasking:
      case Optimization:
      case Stream:

      case Internal:
      case Limit:
      case Log_Error:
      case Log_Warning:
      case Log_Summary:
         PRINTMSG (glb_line_num, 1653, Warning, 0, msg_num, 
                   (error) ? "error" : "warning");
         break;
      }
   }

   if (msg_severity != Log_Warning  &&
       msg_severity != Log_Error    &&
       msg_severity != Log_Summary) {

      /* If the line number for the message is 0, it needs extra          */
      /* considerations.  If the message was produced by the front-end,   */
      /* issue the 0 line number message and then issue message 296 to    */
      /* diagnose the 0 line number.  If the message was produced by any  */
      /* component OTHER THAN the front-end and it's an Internal or Limit */
      /* message, just issue the 0 line number message.	If it's NOT an    */
      /* Internal or Limit message, also issue 296.  In all 0 line number */
      /* cases, use stmt_start_line for the line number.  Note that a     */
      /* line number of 0 is acceptable for the Log_ messages.            */


      if (glb_line_num == 0) {

         if (msg_num <= MAX_FE_MSG  ||
             (msg_severity != Limit  &&  msg_severity != Internal)) {

            glb_line_num = (stmt_start_line > 0) ? stmt_start_line : 1;

            /* Set exit_now to FALSE in the event the message is an Internal  */
            /* or Limit message to prevent the call to exit_compiler.         */
            /* exit_compiler will be called when message 296 is issued later. */
     
            exit_now = FALSE;
         }
# if defined(_DEBUG)

         if (dump_flags.msg_checking) {

            /* We are doing extra message checking.      */
            /* Issue an abort for all zero line numbers. */

            exit_now = FALSE;
         }
# endif
      }
      else if (glb_line_num < pgm_unit_start_line && msg_num > MAX_FE_MSG) {

         /* In some cases, a line number outside of the current program unit  */
         /* may be sent to the frontend.  Technically this should be fixed,   */
         /* but to prevent an internal abort we will adjust the line number   */
         /* to be set the the start of the current program unit for a non     */
         /* frontend message.                                                 */

         glb_line_num = pgm_unit_start_line;

# if defined(_DEBUG)

         /* We are doing extra message checking.         */
         /* Issue a message about bad line number later. */

         issue_line_out_of_range_msg = (dump_flags.msg_checking) ? TRUE : FALSE;
# endif
      }
   }

   switch (msg_severity) {

      case Ansi:

         if (on_off_flags.issue_ansi_messages) {

            if (GET_MESSAGE_TBL(message_suppress_tbl, msg_num)) {
               break;
            }

            if (cif_tmp_so_no_msg) {
               break;
            }

            num_ansi++;
            va_start (arg_ptr, column_num);
            arg1 = va_arg (arg_ptr, long);
            arg2 = va_arg (arg_ptr, long);
            arg3 = va_arg (arg_ptr, long);
            arg4 = va_arg (arg_ptr, long);
            va_end (arg_ptr);
            output_msg (glb_line_num, msg_num, msg_severity,
                        column_num, arg1, arg2, arg3, arg4);

# ifdef _DEBUG
            if (dump_flags.abort_on_ansi) {

               output_msg (glb_line_num, 597, Limit, 0, 0, 0, 0, 0);
   
               exit_compiler(RC_USER_ERROR);
            }
# endif

	 }

	 break;
	 

      case Comment:

         if (cmd_line_flags.msg_lvl_suppressed <= Comment_Lvl) {

            if (GET_MESSAGE_TBL(message_suppress_tbl, msg_num)) {
               break;
            }

            if (cif_tmp_so_no_msg) {
               break;
            }

            num_comments++;
            va_start (arg_ptr, column_num);
            arg1 = va_arg (arg_ptr, long);
            arg2 = va_arg (arg_ptr, long);
            arg3 = va_arg (arg_ptr, long);
            arg4 = va_arg (arg_ptr, long);
            va_end (arg_ptr);
            output_msg (glb_line_num, msg_num, msg_severity,  /*PRINTMSG */
                        column_num, arg1, arg2, arg3, arg4);
	 }

	 break;

	 
      case Note:

         if (cmd_line_flags.msg_lvl_suppressed <= Note_Lvl) {

            if (GET_MESSAGE_TBL(message_suppress_tbl, msg_num)) {
               break;
            }

            if (cif_tmp_so_no_msg) {
               break;
            }

            num_notes++;
            va_start (arg_ptr, column_num);
            arg1 = va_arg (arg_ptr, long);
            arg2 = va_arg (arg_ptr, long);
            arg3 = va_arg (arg_ptr, long);
            arg4 = va_arg (arg_ptr, long);
            va_end (arg_ptr);
            output_msg (glb_line_num, msg_num, msg_severity,  
                        column_num, arg1, arg2, arg3, arg4);
	 }

	 break;

	 
      case Caution:

         if (cmd_line_flags.msg_lvl_suppressed <= Caution_Lvl) {

            if (GET_MESSAGE_TBL(message_suppress_tbl, msg_num)) {
               break;
            }

            if (cif_tmp_so_no_msg) {
               break;
            }

            num_cautions++;
            va_start (arg_ptr, column_num);
            arg1 = va_arg (arg_ptr, long);
            arg2 = va_arg (arg_ptr, long);
            arg3 = va_arg (arg_ptr, long);
            arg4 = va_arg (arg_ptr, long);
            va_end (arg_ptr);
            output_msg (glb_line_num, msg_num, msg_severity,
                        column_num, arg1, arg2, arg3, arg4);
	 }

	 break;


      case Warning:

         if (cmd_line_flags.msg_lvl_suppressed <=  Warning_Lvl) {

            if (GET_MESSAGE_TBL(message_suppress_tbl, msg_num)) {
               break;
            }

            if (cif_tmp_so_no_msg) {
               break;
            }

            num_warnings++;
            va_start (arg_ptr, column_num);
            arg1 = va_arg (arg_ptr, long);
            arg2 = va_arg (arg_ptr, long);
            arg3 = va_arg (arg_ptr, long);
            arg4 = va_arg (arg_ptr, long);
            va_end (arg_ptr);
            output_msg (glb_line_num, msg_num, msg_severity,
                        column_num, arg1, arg2, arg3, arg4);
         }

	 break;


      case Error:

         if (cif_tmp_so_no_msg) {
            break;
         }

         num_errors++;
         num_prog_unit_errors++;

         va_start (arg_ptr, column_num);
         arg1 = va_arg (arg_ptr, long);
         arg2 = va_arg (arg_ptr, long);
         arg3 = va_arg (arg_ptr, long);
         arg4 = va_arg (arg_ptr, long);
         va_end (arg_ptr);
         output_msg (glb_line_num, msg_num, msg_severity,  /*PRINTMSG */
                         column_num, arg1, arg2, arg3, arg4);

         if (!error                                         &&
             curr_stmt_sh_idx > NULL_IDX                    && 
             curr_stmt_sh_idx <= sh_tbl_idx                 &&
             glb_line_num >= SH_GLB_LINE(curr_stmt_sh_idx)) {

            /* only set error flag if have statement header to set it on. */
            /* Since src_input delays issuing messages until the correct  */
            /* line is seen, there will be no header for this line if not */
            /* already created.                                           */

            /* Do not set if this is a forced error with -M option.       */

            SH_ERR_FLG(curr_stmt_sh_idx) = TRUE;
         }

         if (on_off_flags.abort_if_any_errors) {

            if (c_i_f != NULL) {
               flush_msg_file();
            }

            /* Put the option into the message as a string substitution       */
            /* because our Technology Partners might use a different option.  */

            output_msg (glb_line_num, 1226, Limit, 0, 0, 0, 0, 0);

            PRINT_GL_TBL;      /* Prints to dump_file ifdef _DEBUG and -u glt */


            /* Output a dummy Summary record signalling the abort because     */
            /* libcif has a very difficult time dealing with a "truncated"    */
            /* CIF.  The CIF must be reopened and c_i_f must be reset so that */
            /* cif_summary_rec will work.  The CIF was closed and c_i_f was   */
            /* set to NULL when flush_msg_file (just above) called            */
            /* print_buffered_messages.  It needs to be reset to NULL to      */
            /* signal to exit_compiler that the messages have been printed.   */

            c_i_f = fopen(cif_name, "a+");

            cif_summary_rec("Ignore this",
                            "Ignore this",
                            "Ignore this",
                            0,
                            0,
                            -1);                               /* Abort flag. */

            c_i_f = NULL;

	    exit_compiler(RC_USER_ERROR);
	 }

         if (on_off_flags.abort_on_100_errors  &&
             num_errors >= MAX_ERR_LIMIT) {

            if (c_i_f != NULL) {
               flush_msg_file();
            }

            output_msg(glb_line_num, 214, Limit, 0, MAX_ERR_LIMIT, 0, 0, 0);

            PRINT_GL_TBL;      /* Prints to dump_file ifdef _DEBUG and -u glt */


            /* Output a dummy Summary record signalling the abort because     */
            /* libcif has a very difficult time dealing with a "truncated"    */
            /* CIF.  The CIF must be reopened and c_i_f must be reset so that */
            /* cif_summary_rec will work.  The CIF was closed and c_i_f was   */
            /* set to NULL when flush_msg_file (just above) called            */
            /* print_buffered_messages.  It needs to be reset to NULL to      */
            /* signal to exit_compiler that the messages have been printed.   */

            c_i_f = fopen(cif_name, "a+");

            cif_summary_rec("Ignore this",
                            "Ignore this",
                            "Ignore this",
                            0,
                            0,
                            -1);                               /* Abort flag. */

            c_i_f = NULL;

	    exit_compiler(RC_USER_ERROR);
	 }

	 break;


      case Log_Error:

         /* Log_Error is seperate from Error because we do not want to do     */
         /* most of the checks that are done in Error, like -ea.              */

         num_errors++;
         num_prog_unit_errors++;

         va_start (arg_ptr, column_num);
         arg1 = va_arg (arg_ptr, long);
         arg2 = va_arg (arg_ptr, long);
         arg3 = va_arg (arg_ptr, long);
         arg4 = va_arg (arg_ptr, long);
         va_end (arg_ptr);
         output_msg (glb_line_num, msg_num, msg_severity,  /*PRINTMSG */
                     column_num, arg1, arg2, arg3, arg4);

         /* VERY SPECIAL CASE:  If message 49 				      */
         /*								      */
         /*                Cannot open source file "%s". 		      */
         /*								      */
         /* is being issued, it means there is no source file so there is no  */
         /* information we can use to construct a valid message file.  So     */
         /* just dump the message file entirely so no other part of the       */
         /* compiler tries to do anything with it.			      */

         if (msg_num == 49) {
            fclose(cif_actual_file);
            remove(cif_name);
            c_i_f = NULL;
            cif_actual_file = NULL;
            fclose(cif_tmp_file);
            remove(cif_tmp_file_name);
            cif_tmp_file = NULL;
         }

         break;


      case Internal:
      case Limit:

         if (c_i_f != NULL) {
            flush_msg_file();
         }

         va_start (arg_ptr, column_num);
         arg1 = va_arg (arg_ptr, long);
         arg2 = va_arg (arg_ptr, long);
         arg3 = va_arg (arg_ptr, long);
         arg4 = va_arg (arg_ptr, long);
         va_end (arg_ptr);
         output_msg (glb_line_num, msg_num, msg_severity,  /*PRINTMSG */
	                 column_num, arg1, arg2, arg3, arg4);


         if (comp_phase == Cmdline_Parsing) {

            /* This message is found in commandline parsing before all */
            /* proper start up is done.  Just abort compilation here.  */

            exit_compiler(RC_INTERNAL_ERROR);
            break;
         }

         /* Output a dummy Summary record signalling the abort because libcif */
         /* has a very difficult time dealing with a "truncated" CIF.         */
         /* The CIF must be reopened and c_i_f must be reset so that          */
         /* cif_summary_rec will work.  The CIF was closed and c_i_f was set  */
         /* to NULL when flush_msg_file (just above) called print_buffered_   */
         /* _messages.  It needs to be reset to NULL to signal to             */
         /* exit_compiler that the messages have been printed.     	      */
      
         c_i_f = fopen(cif_name, "a+");

         cif_summary_rec("Ignore this", 
                         "Ignore this",
                         "Ignore this",
                         0,
                         0,
                         -2);                                  /* Abort flag. */

         c_i_f = NULL;


# ifdef _DEBUG

         /* Will dump tables, if cmdline option -u requests any dumps and */
         /* the dump calls have not been hit int p_driver.c yet.          */

         if (comp_phase < Pdg_Conversion) {
            PRINT_ALL_SYM_TBLS;
            PRINT_BLK_STK;
            PRINT_CMD_LINE_TBLS;
            PRINT_GL_TBL;
            PRINT_GN_TBL;

            while (curr_scp_idx != NULL_IDX) {
               PRINT_DBG_SYTB;
               PRINT_DBG_STMT;
               curr_scp_idx = SCP_PARENT_IDX(curr_scp_idx);
            }

            if (dump_flags.mem_report) {
               free_tables();  /* See what the tables look at, at abort. */
            }
         }
# endif

# ifdef _DEBUG
# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
         if (msg_severity == Internal) {
            TRBK ();	
         }
# endif
# endif

         /* Do not exit yet if the message has a 0 line number and it was     */
         /* produced by the front-end.					      */

         if (exit_now) {
            exit_compiler(RC_INTERNAL_ERROR);
         }

         break;


      case Log_Warning:

         if (GET_MESSAGE_TBL(message_suppress_tbl, msg_num)) {
            break;
         }

         num_warnings++;

         va_start (arg_ptr, column_num);
         arg1 = va_arg (arg_ptr, long);
         arg2 = va_arg (arg_ptr, long);
         arg3 = va_arg (arg_ptr, long);
         arg4 = va_arg (arg_ptr, long);
         va_end (arg_ptr);
         output_msg (glb_line_num, msg_num, msg_severity,  /*PRINTMSG */
                         column_num, arg1, arg2, arg3, arg4);
         break;


      case Log_Summary:

         if (GET_MESSAGE_TBL(message_suppress_tbl, msg_num)) {
            break;
         }

         va_start (arg_ptr, column_num);
         arg1 = va_arg (arg_ptr, long);
         arg2 = va_arg (arg_ptr, long);
         arg3 = va_arg (arg_ptr, long);
         arg4 = va_arg (arg_ptr, long);
         va_end (arg_ptr);
         output_msg (glb_line_num, msg_num, msg_severity,  /*PRINTMSG */
                         column_num, arg1, arg2, arg3, arg4);
         break;


      case Vector:
      case Scalar:
      case Table:
      case Inline:
      case Info:
      case Tasking:
      case Optimization:
      case Stream:

         if (GET_MESSAGE_TBL(message_suppress_tbl, msg_num)) {
            break;
         }

         num_optz_msgs++;
         
         if ((cif_flags & MESSAGE_RECS) || opt_flags.msgs) {
            va_start (arg_ptr, column_num);
            arg1 = va_arg (arg_ptr, long);
            arg2 = va_arg (arg_ptr, long);
            arg3 = va_arg (arg_ptr, long);
            arg4 = va_arg (arg_ptr, long);
            va_end (arg_ptr);
            output_msg (glb_line_num, 
                        msg_num,
                        msg_severity,
                        column_num,
                        arg1,
                        arg2,
                        arg3,
                        arg4);
         }

         break;


      default:   /* Invalid message severity */
         PRINTMSG (glb_line_num, 2, Internal, 0, msg_severity);
         break;
   }  /* switch */


   /* If the message had a 0 line number and the message was produced by the  */
   /* front-end, complain about the 0 line number.			      */

   if (! exit_now) {
      exit_now = TRUE;
      PRINTMSG (glb_line_num, 296, Internal, 0, msg_num, 0);
   }

# if defined(_DEBUG)
   if (issue_line_out_of_range_msg) {
      PRINTMSG (glb_line_num, 626, Internal, 0, "valid line number","PRINTMSG");
   }
# endif

   TRACE (Func_Exit, "PRINTMSG", NULL);

   return; 

}  /* PRINTMSG */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine is called by PRINTMSG.  It formats the message and	      *|
|*      outputs it to stderr.						      *|
|*									      *|
|* Input parameters:							      *|
|*	glb_line_num	 If non-zero:  Global line number of source; this     *|
|*			   number maps onto a line number within the current  *|
|*			   file.					      *|
|*			 If zero    :  Indicates that a message header should *|
|*			   should not be output.			      *|
|*	msg_num		 The message number of the text to be output. 	      *|
|*	msg_severity	 The severity level.				      *|
|*	column_num       If non-zero:  Column is put into the header.         *|
|*			 If zero    :  Column is not put into header.         *|
|*	arg1, arg2, 	 Four optional arguments that can be inserted into    *|
|*	arg3, arg4	 into the message text.				      *|
|*									      *|
|*			 Note: no validation check is made for missing or     *|
|*			       or extra arguments in the message text.  It is *|
|*			       up to the developer to insure that the same    *|
|*			       number of arguments passed to PRINTMSG are in  *|
|*			       the message text.	 		      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

/* This error number will be tested as a special case to avoid infinite
 * recursion that would occur due to printing an error while printing an
 * error. */
#define ERROR_ECHOING_SOURCE	995

void output_msg (int				glb_line_num,
        	 int				msg_num,
		 msg_severities_type		msg_severity, 
		 int				column_num, 
		 long				arg1,
		 long				arg2,
		 long				arg3,
		 long				arg4)

{
   	  int			 act_file_line;
   	  char		 	*act_file_name;
   	  char		 	 expanded_text[EXPANDED_MSG_SIZE];
   	  char		 	 final_text[FINAL_MSG_SIZE];
   	  int			 glb_idx;
   static int			 last_msg_idx			= NULL_IDX;
	  int			 msg_idx;
   	  char		 	 orig_text[ORIG_MSG_SIZE];
   	  char		  	 position_buff[MAX_HDR_SIZE];
	  boolean		 print_directly_to_stderr;
#ifdef KEY /* Bug 10177 */
          char                  *scoping_unit_name = 0;
#else /* KEY Bug 10177 */
          char                  *scoping_unit_name;
#endif /* KEY Bug 10177 */
   	  char		 	*text_ptr;
/*
   fprintf (stderr, "output_msg: line = %d, num = %d, severity = %d, col = %d\n",
            glb_line_num, msg_num, msg_severity, column_num);
   return;
*/
# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
	  last_msg_queue_type	 msg_desc;
# endif


   TRACE (Func_Entry, "output_msg", NULL);

   if (comp_phase == Lex_Parsing) {

      /* See if message has already been issued. */

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
      msg_desc.msg_num	= msg_num;
      msg_desc.line_num	= glb_line_num;
      msg_desc.col_num	= column_num;
# endif

      for (msg_idx = 0; msg_idx < LAST_MSG_QUEUE_SIZE; msg_idx++) {

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
         if (*((long *) (&msg_desc)) == 
             *((long *) (&last_msg_queue[msg_idx]))){
            TRACE (Func_Exit, "output_msg", NULL);
            return;
         }
# else
         if (last_msg_queue[msg_idx].msg_num  == msg_num &&
             last_msg_queue[msg_idx].line_num == glb_line_num &&
             last_msg_queue[msg_idx].col_num  == column_num) {
            TRACE (Func_Exit, "output_msg", NULL);
            return;
         }
# endif
      }

      last_msg_queue[last_msg_idx].msg_num	= msg_num;
      last_msg_queue[last_msg_idx].line_num	= glb_line_num;
      last_msg_queue[last_msg_idx].col_num	= column_num;

      if (++last_msg_idx == LAST_MSG_QUEUE_SIZE) {
         last_msg_idx = NULL_IDX;
      }
   }


   /* The Syntax Pass depends on the messages coming out in the same order    */
   /* they were issued.  Since the sort routine used to sort messages by line,*/
   /* column, and message number has different internal algorithms on         */
   /* different architectures, we need to use the "relative order" field of   */
   /* the Message record to keep Syntax Pass messages in the intended order.  */

   if (comp_phase == Pass1_Parsing) {

      if (save_glb_line_num == glb_line_num  &&
          save_column_num == column_num) {
         ++relative_order;
      }
      else {
         save_glb_line_num = glb_line_num;
         save_column_num   = column_num;
         relative_order    = 1;
      }
   }
   else {
      relative_order = 0;
   }


   switch (msg_severity) {

      case Log_Warning:
      case Log_Error:
         print_directly_to_stderr = TRUE;
         sprintf (position_buff, "in command line"); 
         break;

      case Log_Summary:
         print_directly_to_stderr = TRUE;
         break;

      default:

         if (comp_phase == Cmdline_Parsing) {
            print_directly_to_stderr = TRUE;
            sprintf (position_buff, "in command line"); 
            break;
         }

         print_directly_to_stderr =

# ifdef _DEBUG
            dump_flags.std_err             ||
# endif
            (msg_severity == Internal  ||  msg_severity == Limit);

         if (print_directly_to_stderr) {

            /* Print source line and ^ before printing the message.	*/

            print_err_line(glb_line_num, column_num);
         }

#ifdef KEY /* Bug 7247 */
	 /* Error while printing error: print it now, while we're in the
	  * process of printing the error that provoked it. */
	 if (ERROR_ECHOING_SOURCE == msg_num) {
	   print_directly_to_stderr = TRUE;
	 }
#endif /* KEY Bug 7247 */

         GLOBAL_LINE_TO_FILE_LINE(glb_line_num, glb_idx, act_file_line);
         act_file_name = GL_FILE_NAME_PTR(glb_idx);

         if (scp_tbl == NULL_IDX) {
            scoping_unit_name = program_unit_name;

            /* Error condition exists before init_parser has been       */
            /* invoked or the tables have already been freed.           */

            if (print_directly_to_stderr) {
               sprintf (position_buff, "%s, File = %s, Line = %d ",
                        program_unit_name,
                        act_file_name,
                        act_file_line);
            }
         }
         else {
            scoping_unit_name = AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx));

            if (print_directly_to_stderr) {

               if (column_num == 0) {
                  sprintf (position_buff, "%s, File = %s, Line = %d ",
                           AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)), 
                           act_file_name,
                           act_file_line);
               }
               else {
                  sprintf (position_buff,
                           "%s, File = %s, Line = %d, Column = %d ",
                           AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)), 
                           act_file_name,
                           act_file_line,
                           column_num);
               }
            }
         }

   }  /* End switch */


   text_ptr = (char *) catgetmsg(msg_sys,
                                 NL_MSGSET,
                                 msg_num,
                                 orig_text,
                                 ORIG_MSG_SIZE);
	     
   if (!*text_ptr) {	/* Unable to retrieve message. */

      if (msg_num == 3) {

         /* The message catalog must really be messed up, because we cannot   */
         /* find the message that says we cannot find the message.  fprintf   */
         /* to stderr and abort compilation.  If the driver provided the      */
         /* message file, get rid of it so the driver won't get confused.     */
         /* exit_compiler will handle the rest.				      */
         /* 								      */
         /* LRR   4/10/97						      */
         /* SPRs 704082 and 700504 complained about message 3 being issued    */
         /* when the compiler ran out of memory and not giving the user a     */
         /* clue as to what happened.  I enhanced message 3 to give users     */
         /* more of a clue as to what could possibly have happened and then   */
         /* added the specific check later for the "out of memory" case.      */

# if defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN)
#ifdef PSC_TO_OPEN64
         fprintf(stderr, OPEN64_NAME_PREFIX "f95 INTERNAL: "
#endif
#else       
         fprintf(stderr, "cft90 INTERNAL: "
#endif
                         "Message system failed trying to issue message %ld\n",
                         arg1);
         fprintf(stderr, "  Possible reasons include:\n");
         fprintf(stderr, "   * The message catalog has been corrupted.\n");
         fprintf(stderr, "   * The message catalog is out of date or "
                         "does not match the compiler release.\n");

         if (c_i_f != NULL  &&  (cif_C_opts & CMD_PROVIDED_CIF)) {
            fclose(cif_actual_file);
            cif_actual_file = NULL;
            remove(cif_name);
         }

         exit_compiler(RC_USER_ERROR);
      }

      /* LRR   4/22/97                                                        */
      /* SPRs 704082 and 700504 complained about message 3 being issued when  */
      /* the compiler ran out of memory and not giving the user a clue as to  */
      /* what happened.  Jeff Drummond and Brian Forney implemented error     */
      /* codes and the entry points to retrieve them but only in UNICOS 9.2   */
      /* and later.  Because Jon and other SL's decided they wanted to use    */
      /* the entry points, MWM changed the build script to copy nl_types.h to */
      /* the local build directory so that we can build for releases prior to */
      /* 9.2.								      */
      /* The code below currently only checks for the "out of memory" case.   */
      /* If, in the future, someone really wants to get fancy with this, then */
      /* the "if" should be changed to a "switch" on the codes returned from  */
      /* __catgetmsg_error_code().					      */

# if !(defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))

      if (__catgetmsg_error_code() == NL_ERR_MALLOC) {
         fprintf(stderr, "cft90 INTERNAL: "
                         "Message system failed trying to issue message %d\n",
                         msg_num);
         fprintf(stderr, "  User memory space has been exhausted - the "
                         "message system has no space in which to work.\n");

         if (c_i_f != NULL  &&  (cif_C_opts & CMD_PROVIDED_CIF)) {
            fclose(cif_actual_file);
            cif_actual_file = NULL;
            remove(cif_name);
         }

         exit_compiler(RC_USER_ERROR);
      }
      else {
         PRINTMSG (glb_line_num, 3, Internal, 0, msg_num); 
      }
# else
      PRINTMSG (glb_line_num, 3, Internal, 0, msg_num); 
# endif
   }

   if (print_directly_to_stderr) {

      /* Expand optional arguments, if any, into the message text.  	      */

#ifdef KEY /* Bug 4469 */
      snprintf (expanded_text, sizeof expanded_text, orig_text, arg1, arg2,
        arg3, arg4);
#else
      sprintf (expanded_text, orig_text, arg1, arg2, arg3, arg4);
#endif /* KEY Bug 4469 */
   }


   /* Format and output the message header and text (unless the message is */
   /* type Log_Summary; then just output the message text).                */

   if (msg_severity != Log_Summary) {

      if (print_directly_to_stderr) {
         text_ptr = expanded_text;
         catmsgfmt (command_name,
                    message_prefix,
                    msg_num, 
                    msg_severity_name[msg_severity],
                    text_ptr,
                    final_text,
                    FINAL_MSG_SIZE,
                    position_buff,
                    (char *) NULL);
         fputs (final_text, stderr);
#ifdef KEY /* Bug 6673 */
	 if (verbose_message) {
	   verbose_message(group_code, msg_num);
	 }
#endif /* KEY Bug 6673 */
      }
   }
   else {
      fputs (expanded_text, stderr);
      fputc (NEWLINE, stderr);
   }

   if (msg_severity != Log_Warning  &&
       msg_severity != Log_Error    &&
       msg_severity != Log_Summary  &&
       msg_severity != Internal     &&
#ifdef KEY /* Bug 7247 */
       /* Error while printing error: don't recurse */
       msg_num != ERROR_ECHOING_SOURCE &&
#endif /* KEY Bug 7247 */
       msg_severity != Limit) {
      cif_message_rec(msg_num,
                      glb_line_num,
                      column_num,
                      msg_severity,
                      orig_text,
                      arg1,
                      arg2,
                      arg3,
                      arg4,
                      scoping_unit_name,
                      relative_order);
   }

   TRACE (Func_Exit, "output_msg", NULL);

   return;

}  /* output_msg */ 


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Routine that closes necessary files and exits from the compiler	      *|
|*									      *|
|* Input parameters:							      *|
|*	Number that is output with exit					      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void exit_compiler (int		code)

{

   TRACE (Func_Entry, "exit_compiler", NULL);

   if (cif_tmp_file != NULL  &&  c_i_f == cif_tmp_file) {
      flush_msg_file();
   }

   if (c_i_f != NULL) {
      print_buffered_messages();
   }

   if (cif_actual_file) {
      fclose(cif_actual_file);
   }

   if (cif_flags == 0) {
      remove(cif_name);
   }

   catclose (msg_sys);

   clean_up_module_files();

   if (cif_flags != 0) {
      close_cif();
   }


# ifdef _DEBUG

# if defined(_HOST_OS_SOLARIS) || (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))

   /* Abort if this is a Limit or an Internal error.			      */

   if (code == RC_INTERNAL_ERROR) {
      abort();
   }

# endif

# endif

   exit (code);

   TRACE (Func_Exit, "exit_compiler", NULL);

}  /* exit_compiler */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Routine that finds the file to which a line belongs.  This is needed  *|
|*      for SPARCs.						     	      *|
|*									      *|
|* Input parameters:							      *|
|*	search_line : the global line number				      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

char *global_to_local_file (int search_line)
{
   int		 idx;
   int  	 line;
   char		*act_file_name;


   TRACE (Func_Entry, "global_to_local_file", NULL);

   GLOBAL_LINE_TO_FILE_LINE(search_line, idx, line);
   act_file_name = GL_FILE_NAME_PTR(idx);

   TRACE (Func_Exit, "global_to_local_file", NULL);

   return(act_file_name);

}  /* global_to_local_file */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Routine that finds the path to which a global line belongs.           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      search_line : the global line number                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

char *global_to_local_path (int search_line)
{
   int           idx;
   int           line;
   char         *act_path_name;


   TRACE (Func_Entry, "global_to_local_path", NULL);

   GLOBAL_LINE_TO_FILE_LINE(search_line, idx, line);
   act_path_name = GL_PATH_NAME_PTR(idx);

   TRACE (Func_Exit, "global_to_local_path", NULL);

   return(act_path_name);

}  /* global_to_local_path */





/******************************************************************************\
|*									      *|
|*	   * * *   THE FRONTEND DOES NOT USE THIS ROUTINE.   * * *            *|
|*									      *|
|* Description:								      *|
|*	Routine that finds a CIF file id for a file containing a given        *|
|*      global line number.						      *|
|*									      *|
|* Input parameters:							      *|
|*	search_line : the global line number				      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	The CIF file id.						      *|
|*									      *|
\******************************************************************************/

int global_to_file_id (int	search_line)
{
   int		 idx;
   int  	 line;


   TRACE (Func_Entry, "global_to_file_id", NULL);

   GLOBAL_LINE_TO_FILE_LINE(search_line, idx, line);

   TRACE (Func_Exit, "global_to_file_id", NULL);

   return(GL_CIF_FILE_ID(idx));

}  /* global_to_file_id */


/******************************************************************************\
|*									      *|
|*	   * * *   THE FRONTEND DOES NOT USE THIS ROUTINE.   * * *            *|
|*									      *|
|* Description:								      *|
|*	This routine converts a global line number to a file line number.     *|
|*	It is called by PDGCS to convert the global line for inserts in       *|
|*	optimization messages.  It is called by CCG for line numbers for      *|
|*	CAL listings and the traceback.                                       *|
|*									      *|
|* Input parameters:							      *|
|*	search_line : a global line number 				      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	The file line number.						      *|
|*									      *|
\******************************************************************************/

int global_to_file_line_number (int	search_line)
{
   int	idx;
   int	line;


   TRACE (Func_Entry, "global_to_file_line_number", NULL);

   GLOBAL_LINE_TO_FILE_LINE(search_line, idx, line);

   TRACE (Func_Exit, "global_to_file_line_number", NULL);

   return(line);

}  /* global_to_file_line_number */


/******************************************************************************\
|*									      *|
|*	   * * *   THE FRONTEND DOES NOT USE THIS ROUTINE.   * * *            *|
|*									      *|
|* Description:								      *|
|*	This routine converts a global line number to a file line number.     *|
|*	It is called by PDGCS and code-gen routines that need to convert the  *|
|*	global line number for optional arguments to PRINTMSG.		      *|
|*									      *|
|*      This is simply a different name for global_to_file_line_number.       *|
|*									      *|
|* Input parameters:							      *|
|*	search_line : a global line number				      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	The file line number.						      *|
|*									      *|
\******************************************************************************/

int global_to_local_line_number (int	search_line)
{
   int	idx;
   int	line;


   TRACE (Func_Entry, "global_to_local_line_number", NULL);

   GLOBAL_LINE_TO_FILE_LINE(search_line, idx, line);

   TRACE (Func_Exit, "global_to_local_line_number", NULL);

   return(line);

}  /* global_to_local_line_number */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      The compiler has encountered a condition where it must terminate the  *|
|*      the compilation.  Output all the messages that have been sent to the  *|
|*      message file first.                                                   *|
|*									      *|
|* Input parameters:							      *|
|*      NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*      NONE								      *|
|*									      *|
|* Returns:								      *|
|*      NOTHING								      *|
|*									      *|
\******************************************************************************/

static void flush_msg_file(void)
{
   int        scp_idx;


   TRACE (Func_Entry, "flush_msg_file", NULL);

   if (c_i_f == cif_tmp_file) {
      cif_fake_a_unit();
   }

   if (last_msg_file_rec != END_UNIT) {

      if (curr_scp_idx == 0) {
         cif_end_unit_rec(program_unit_name);
      }
      else {
         scp_idx = curr_scp_idx;

         while (SCP_PARENT_IDX(scp_idx) != 0) {
            scp_idx = SCP_PARENT_IDX(scp_idx);
         }

         cif_end_unit_rec(AT_OBJ_NAME_PTR(SCP_ATTR_IDX(scp_idx)));
      }
   }

   print_buffered_messages();

   TRACE (Func_Exit, "flush_msg_file", NULL);

   return;

}  /* flush_msg_file */

#ifdef KEY /* Bug 7247 */

/* The error message machinery tries very hard to echo the source line
 * before it prints the message itself. But it's possible that a "#" line
 * in a .i file mentions a file which doesn't exist any more, or which has
 * been changed in such a way that we encounter an EOF when we try to find
 * the source line. Whenever something prevents us from recovering the
 * source line, we want to give a warning, omit the printing of the source
 * line, and then print the underlying error message anyway.
 */

static void
cannot_open_include_file(int line_num)
{
  PRINTMSG(line_num, ERROR_ECHOING_SOURCE, Warning, 0,
    "cannot open INCLUDE file");
}

static void
files_too_deeply_nested(int line_num)
{
  PRINTMSG(line_num, ERROR_ECHOING_SOURCE, Warning, 0,
    "source files too deeply nested");
}

static void
cannot_read_source_file(int line_num)
{
  PRINTMSG(line_num, ERROR_ECHOING_SOURCE, Warning, 0,
    "cannot read source file");
}
#endif /* KEY Bug 7247 */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Print the message file.                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void print_buffered_messages(void)
{


#  define	 FILE_STK_MAX	     500    /* Just make something up.	      */
#ifdef KEY /* Bug 3632 */
#else
#  define	 MAX_SRC_LINE_SIZE   256    /* Stolen from src_input.m        */
#endif /* KEY Bug 3632 */

   char		 carat[MAX_SRC_LINE_SIZE];
   char		 expanded_text[EXPANDED_MSG_SIZE];
   int        	 fd;
   int		 file_line;
   char          final_text[FINAL_MSG_SIZE];
   int		 global_idx;
   int		 i;
   int		 line_num;
   nl_catd	 message_catalog;
   char          message_hdr[MAX_HDR_SIZE];
   int		 msg_idx;
   char         *msg_insert;
   int		 my_file_id;
   int		 num_msgs;
   int		 num_recs;
   char    	 orig_text[ORIG_MSG_SIZE];
   int		 rec_type;
   int		 search_idx;
   char		 source_line[MAX_SRC_LINE_SIZE];
   int		 stk_idx;
   int		 unit_idx;

   struct        file_stk_frame	{int	file_id;
			 	 int	file_line;
			 	 FILE  *file_ptr;
               			};

   typedef  struct  file_stk_frame	file_stk_type;

   file_stk_type file_stk[FILE_STK_MAX];


   struct	 Cif_filedir	*filedir;
   struct	 Cif_unitdir	*unitdir;

   struct	 Cif_cifhdr	*header_rec;
   struct	 Cif_file	*file_rec;
   struct	 Cif_message	*message_rec;


# ifdef _DEBUG

   int		 		dbg_global_line;
   struct	 Cif_message	dbg_message_rec;

# endif


   TRACE (Func_Entry, "print_buffered_messages", NULL);

   if (c_i_f == cif_actual_file) {
      /* prevent closing the same file twice. Linux does not handle it */
      cif_actual_file = NULL;
   }

   fclose(c_i_f);

   stk_idx = 0;


   num_msgs = num_ansi  + num_cautions + num_comments + num_errors +
              num_notes + num_warnings;

   if (opt_flags.msgs) {
      num_msgs += num_optz_msgs;
   }


   /* It is possible for num_msgs to be nonzero but also have no messages in  */
   /* in the message file.  This happens when a Log_ message is the only kind */
   /* of message issued (they are counted in num_errors and num_warnings).    */
   /* One of the cases where this can happen is when the compiler is asked to */
   /* compile a file that does not exist.  The next check weeds this case out.*/
   /* If it's not this particular case, we'll just exercise the code in this  */
   /* procedure up to the point it figures out there are no Message records   */
   /* in the file and then it'll just exit.                                   */

   if (global_line_tbl_idx == 0) {
      goto EXIT;
   }


# ifdef _DEBUG

   if (dump_flags.std_err) {
      TRACE (Func_Exit, "print_buffered_messages", NULL);
      return;
   }

   if (num_msgs == 0) {

      if (dump_flags.stmt_dmp) {
         dbg_global_line    = 1;
         message_rec        = &dbg_message_rec;
         message_rec->uline = GL_GLOBAL_LINE(global_line_tbl_idx) + 1;
         goto INIT_FILE_STK;
      }
      else {
         goto EXIT;
      }
   }

# else

   if (num_msgs == 0) {
      goto EXIT;
   }

# endif


   /* Convert the ASCII CIF to the more efficient binary form and open the    */
   /* binary form of the CIF.  The last argument to Cif_Cifconv being FALSE   */
   /* means that the binary output file will be deleted by Cif_Close.         */
   /* DEBUG note:  When a scoping unit has problems to the point that         */
   /* SCP_IN_ERR is set to TRUE, we set a field in the CIF Summary record to  */
   /* -3 to indicate that the CIF is incomplete.  However, Cif_Cifconv does   */
   /* not read to the end of the CIF first to read the Summary record, so it  */
   /* will likely print out a bunch of messages complaining about sym id's    */
   /* not being resolved.  The 0x100 flag allows these messages to come out   */
   /* (in a debug compiler); if this flag is FALSE, it tells Cif_Cifconv not  */
   /* to print them.							      */
  
#ifdef _DEBUG

   fd = Cif_Cifconv(cif_name,
                    "r",
                    NULL,
                    CIF_VERSION,
                    0x100);

#else

   fd = Cif_Cifconv(cif_name,
                    "r",
                    NULL,
                    CIF_VERSION,
                    FALSE);

#endif


   if (fd < 0) {
      if (c_i_f == cif_actual_file) {
         /* prevent closing the same file twice. Linux does not handle it */
         cif_actual_file = NULL;
      }
      fclose(c_i_f);
      c_i_f = NULL;
      remove(cif_name);
      fclose(cif_tmp_file);
      remove(cif_tmp_file_name);
      PRINTMSG(1, 1060, Internal, 0, Cif_Errstring(fd));
   }


   /* Set up the memory management and get the initial records.		      */

   Cif_Memmode(fd, CIF_MEM_MANAGED);

   rec_type = Cif_Getrecord(fd, (struct Cif_generic **) &header_rec);

   if (rec_type != CIF_CIFHDR) {
      line_num = 1;
      msg_insert = "first rec is not hdr rec.";
      goto EMERGENCY_EXIT;
   }

#if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
  if (getenv("NLSPATH") == NULL) {
    /* NLSPATH is not set. */
    const char * const toolroot = getenv("TOOLROOT");
    const char * const env_name = "NLSPATH=";
    const char * const env_val = "/usr/lib/locale/C/LC_MESSAGES/%N.cat";
    int len = strlen(env_name) + strlen(env_val) + 1;
    char * new_env;
    if (toolroot != NULL) len += strlen(toolroot);
    new_env = malloc(len);
    if (toolroot == NULL)
      sprintf(new_env, "%s%s", env_name, env_val);
    else
      sprintf(new_env, "%s%s%s", env_name, toolroot, env_val);
    putenv(new_env);
    free(new_env);
  }
#endif

   message_catalog = catopen(header_rec->group, 0);

   Cif_Getfiledir(fd, &filedir);


   /* File Name records are grouped outside of the Unit records.              */

   Cif_Recgroup(fd, 
                NULL,                  
                CIF_FILE,
                (struct Cif_generic **) &file_rec);


   /* Initialize the first file stack frame with the information about the    */
   /* source file.							      */

INIT_FILE_STK:

   global_idx            = 1;

   file_stk[0].file_id   = GL_CIF_FILE_ID(1);
   file_stk[0].file_line = 0;

   if ((file_stk[0].file_ptr = fopen(GL_FILE_NAME_PTR(1), "r")) == NULL) {
      line_num = 1;
#ifdef KEY /* Bug 7247 */
      cannot_read_source_file(line_num);
#else /* KEY Bug 7247 */
      msg_insert = "can not open source file.";
      goto EMERGENCY_EXIT;
#endif /* KEY Bug 7247 */
   }
         

# ifdef _DEBUG

   if (dump_flags.stmt_dmp  &&  num_msgs == 0) {
      goto NO_MSGS_STMT_DUMP;
   }

# endif
      

   /* Go through all the Unitdir records in order to process the messages     */
   /* associated with each program unit.				      */

   for (unit_idx = 0;  unit_idx < filedir->nunits;  ++unit_idx) {

      Cif_Getunitdir(fd, &filedir->ut[unit_idx], &unitdir);

      num_recs = Cif_Recgroup(fd,
                              unitdir,
                              CIF_MESSAGE,
                              (struct Cif_generic **) &message_rec);

      /* Sort the records by global line and column. 			      */
      /* Note:  Any Message records that precede the first Unit record are    */
      /*        included with the first unit by Cif_Cifconv.                  */

      if (num_recs > 1) {
         (void) qsort ( (char *) message_rec,
                        num_recs,
                        sizeof(struct Cif_message),
                       &compare_message_recs);
      }


      /* Go through all the Message records associated with this program      */
      /* unit.								      */

      for (msg_idx = 0;  msg_idx < num_recs;  ++msg_idx) {

         if ((message_rec->severity == Vector   ||
              message_rec->severity == Scalar   ||
              message_rec->severity == Table    ||
              message_rec->severity == Inline   ||
              message_rec->severity == Info     ||
              message_rec->severity == Tasking  ||
              message_rec->severity == Stream   ||
              message_rec->severity == Optimization)  && 
             ! opt_flags.msgs) {
            ++message_rec;
            continue;
         }

# ifdef _DEBUG

         if (dump_flags.stmt_dmp) {
            goto CHECK_GL_IDX;
         }

# endif
         /* KAY - file_line is set in macro but never used.  Bug???? */

         GLOBAL_LINE_TO_FILE_LINE(message_rec->uline,
				  global_idx,
				  file_line);       
         my_file_id = GL_CIF_FILE_ID(global_idx);

         if (my_file_id != file_stk[stk_idx].file_id) {

            for (search_idx = stk_idx - 1;  search_idx >= 0;  --search_idx) {
           
               if (my_file_id == file_stk[search_idx].file_id) {

#ifdef KEY /* Bug 7247 */
                  for (; stk_idx != search_idx; stk_idx -= 1) {
                    if (NULL != file_stk[stk_idx].file_ptr) {
		      fclose(file_stk[stk_idx].file_ptr);
		    }
		  }
#else /* KEY Bug 7247 */
                  while (stk_idx != search_idx) {
                     fclose(file_stk[stk_idx--].file_ptr);
                  }
#endif /* KEY Bug 7247 */

                  goto HAVE_FILE;
               }
            }

            ++stk_idx;

            if (stk_idx < FILE_STK_MAX) {
               file_stk[stk_idx].file_id = my_file_id;

               for (i = 2;  i < filedir->nfiles;  ++i) {

                  if (file_rec[i].fid == my_file_id) {
                     break;
                  }
               }

               if ((file_stk[stk_idx].file_ptr =
                       fopen(file_rec[i].name, "r")) != NULL) { 
                  file_stk[stk_idx].file_line = 0;
               }
               else {
                  line_num = message_rec->uline;
#ifdef KEY /* Bug 7247 */
		  cannot_open_include_file(line_num);
		  goto JUST_PRINT_MESSAGE;
#else /* KEY Bug 7247 */
                  msg_insert = "can not open INCLUDE file.";
                  goto EMERGENCY_EXIT;
#endif /* KEY Bug 7247 */
               }
            }
            else {
               line_num = message_rec->uline;
#ifdef KEY /* Bug 7247 */
	       files_too_deeply_nested(line_num);
	       goto JUST_PRINT_MESSAGE;
#else /* KEY Bug 7247 */
               msg_insert = "file_stk size exceeded.";
               goto EMERGENCY_EXIT;
#endif /* KEY Bug 7247 */
            }
         }

# ifdef _DEBUG

CHECK_GL_IDX:

         if (global_idx < global_line_tbl_idx) {
        
            if (message_rec->uline >= GL_GLOBAL_LINE(global_idx + 1)) {

               while (dbg_global_line < GL_GLOBAL_LINE(global_idx + 1)) {

#ifdef KEY /* Bug 7247 */
                  if (NULL == file_stk[stk_idx].file_ptr) {
		     goto JUST_PRINT_MESSAGE;
		  }
#endif /* KEY Bug 7247 */
                  if (fgets(source_line,
                             MAX_SRC_LINE_SIZE,
                             file_stk[stk_idx].file_ptr) != NULL) {
                     ++dbg_global_line;
                     ++file_stk[stk_idx].file_line;
                     fprintf(stderr, "%s", source_line);
                  }
                  else {
                     line_num = dbg_global_line;
#ifdef KEY /* Bug 7247 */
                     cannot_read_source_file(line_num);
		     goto JUST_PRINT_MESSAGE;
#else /* KEY Bug 7247 */
                     msg_insert = "can not read source file.";
                     goto EMERGENCY_EXIT;
#endif /* KEY Bug 7247 */
                  }
               }

               ++global_idx;

               for (search_idx = stk_idx - 1;  search_idx >=0;  --search_idx) {
      
                  if (GL_CIF_FILE_ID(global_idx) ==
                         file_stk[search_idx].file_id) {

                     while (stk_idx != search_idx) {
                        fclose(file_stk[stk_idx--].file_ptr);
                     }
  
                     goto CHECK_GL_IDX;
                  }
               }

               ++stk_idx;

               if (stk_idx < FILE_STK_MAX) {
                  file_stk[stk_idx].file_id = GL_CIF_FILE_ID(global_idx);

                  for (i = 2;  i < filedir->nfiles;  ++i) {

                     if (file_rec[i].fid == file_stk[stk_idx].file_id) {
                        break;
                     }
                  }

                  if ((file_stk[stk_idx].file_ptr =
                          fopen(file_rec[i].name, "r")) != NULL) {
                     file_stk[stk_idx].file_line = 0;
                  }
                  else {
                     line_num = message_rec->uline;
#ifdef KEY /* Bug 7247 */
		     cannot_open_include_file(line_num);
		     goto JUST_PRINT_MESSAGE;
#else /* KEY Bug 7247 */
                     msg_insert = "can not open INCLUDE file.";
                     goto EMERGENCY_EXIT;
#endif /* KEY Bug 7247 */
                  }
               }
               else {
                  line_num = message_rec->uline;
#ifdef KEY /* Bug 7247 */
		  files_too_deeply_nested(line_num);
		  goto JUST_PRINT_MESSAGE;
#else /* KEY Bug 7247 */
                  msg_insert = "file_stk size exceeded.";
                  goto EMERGENCY_EXIT;
#endif /* KEY Bug 7247 */
               }

               goto CHECK_GL_IDX;
            }
         }

# endif /* _DEBUG */


         /* We have the file, now find the line within the file.              */
         /* If the message line number matches the file line number, we're    */
         /* done.  If the message line number is greater than the file line   */
         /* number and we're processing an INCLUDE file, then the user must   */
         /* have included the file more than once so rewind it and start over.*/
         /* If it's not an INCLUDE file, abort.				      */

HAVE_FILE:

#ifdef KEY /* Bug 7247 */
         if (NULL == file_stk[stk_idx].file_ptr) {
	   goto JUST_PRINT_MESSAGE;
	 }
#endif /* KEY Bug 7247 */

         if (file_stk[stk_idx].file_line != message_rec->fline) {

            if (file_stk[stk_idx].file_line > message_rec->fline  &&
                stk_idx > 0) {
               rewind(file_stk[stk_idx].file_ptr);
               file_stk[stk_idx].file_line = 0;
            }

            while (file_stk[stk_idx].file_line != message_rec->fline) {

               if (fgets(source_line,
                         MAX_SRC_LINE_SIZE,
                         file_stk[stk_idx].file_ptr) != NULL) {
                  ++file_stk[stk_idx].file_line;

# ifdef _DEBUG
                  if (dump_flags.stmt_dmp) {
                     fprintf(stderr, "%s", source_line);
                     ++dbg_global_line;
                  }
# endif

               }
               else {
                  line_num = message_rec->uline;
#ifdef KEY /* Bug 7247 */
                  cannot_read_source_file(line_num);
		  goto JUST_PRINT_MESSAGE;
#else /* KEY Bug 7247 */
                  sprintf(final_text, 
                         "hit EOF while trying to issue message %d at line %d.",
                         message_rec->msgno,
                         line_num);
                  msg_insert = final_text;
                  goto EMERGENCY_EXIT;
#endif /* KEY Bug 7247 */
               }
            }
   
            fprintf(stderr, "\n%s", source_line);
         }


         /* If a character position was given in the record, output the "^".  */
         /* The algorithm is pretty stupid.  It should be changed to just     */
         /* blank the line from 0 to (message_rec->cpos - 1) but because it   */
         /* was originally written to blank to the NULL character, it would   */
         /* really annoy Testing to change it now because every test that has */
         /* a message with a carat would have to have a new base because the  */
         /* carat line would have fewer blanks in it.  So the algorithm       */
         /* is this:  blank out the carat line to the same length as the      */
         /* source line.  If the character position is beyond that (this      */
         /* should only happen if the user did something like forget to close */
         /* a character string), then continue blanking to the character      */
         /* position.							      */
     
         if (message_rec->cpos != 0) {

            for (i = 0;  source_line[i] != '\0';  ++i) {
               carat[i] = (source_line[i] == '\t') ? '\t' : ' ';
            }

            for ( ;  i < message_rec->cpos;  ++i) {
               carat[i] = ' ';
            }

            carat[i]                     = '\0';
            carat[message_rec->cpos - 1] = '^';
            fprintf(stderr, "%s\n", carat);
         }

#ifdef KEY /* Bug 7247 */
/* Either we've finished echoing the source or we can't, so now it's time
 * to print the error message itself. */
JUST_PRINT_MESSAGE:
#endif /* KEY Bug 7247 */

         catgetmsg(message_catalog,
                   NL_MSGSET,
                   message_rec->msgno,
                   orig_text,
                   ORIG_MSG_SIZE);

         Cif_Msginsert(orig_text,
                       (struct Cif_generic *) message_rec,
                       expanded_text,
                       EXPANDED_MSG_SIZE);
            
         if (message_rec->cpos == 0) {
            sprintf(message_hdr, "%s, File = %s, Line = %d ",
                    message_rec->name,
                    GL_FILE_NAME_PTR(global_idx),
                    message_rec->fline);
         }
         else {
            sprintf(message_hdr, "%s, File = %s, Line = %d, Column = %d ",
                    message_rec->name,
                    GL_FILE_NAME_PTR(global_idx),
                    message_rec->fline,
                    message_rec->cpos);
         }

         catmsgfmt(command_name,
                   message_prefix,
                   message_rec->msgno,
                   msg_severity_name[message_rec->severity],
                   expanded_text,
                   final_text,
                   FINAL_MSG_SIZE,
                   message_hdr,
                   (char *) NULL);

         fputs(final_text, stderr);
#ifdef KEY /* Bug 6673 */
	 if (verbose_message) {
	   verbose_message(group_code, message_rec->msgno);
	 }
#endif /* KEY Bug 6673 */

# ifdef _DEBUG
         if (dump_flags.stmt_dmp) {
            fprintf(stderr, "\n");
         }
# endif

         ++message_rec;

      }  /* End Message record loop. */

   }  /* End Unitdir record loop. */


# ifdef _DEBUG

NO_MSGS_STMT_DUMP:

#ifdef KEY /* Bug 7247 */
   if (NULL == file_stk[stk_idx].file_ptr) {
     goto CLEANUP;
   }
#endif /* KEY Bug 7247 */
   if (dump_flags.stmt_dmp) {
   
CHECK_GL_IDX_AGAIN:

      if (global_idx < global_line_tbl_idx) {
        
         while (dbg_global_line < GL_GLOBAL_LINE(global_idx + 1)) {

            if (fgets(source_line,
                       MAX_SRC_LINE_SIZE,
                       file_stk[stk_idx].file_ptr) != NULL) {
               ++dbg_global_line;
               fprintf(stderr, "%s", source_line);
            }
            else {
               line_num = dbg_global_line;
#ifdef KEY /* Bug 7247 */
               cannot_read_source_file(line_num);
	       goto CLEANUP;
#else /* KEY Bug 7247 */
               msg_insert = "can not read source file.";
               goto EMERGENCY_EXIT;
#endif /* KEY Bug 7247 */
            }
         }

         ++global_idx;

         for (search_idx = stk_idx - 1;  search_idx >=0;  --search_idx) {
      
            if (GL_CIF_FILE_ID(global_idx) == file_stk[search_idx].file_id) {

               while (stk_idx != search_idx) {
                  fclose(file_stk[stk_idx--].file_ptr);
               }
  
               goto CHECK_GL_IDX_AGAIN;
            }
         }

         ++stk_idx;

         if (stk_idx < FILE_STK_MAX) {
            file_stk[stk_idx].file_id = GL_CIF_FILE_ID(global_idx);

            for (i = 2;  i < filedir->nfiles;  ++i) {

               if (file_rec[i].fid == file_stk[stk_idx].file_id) {
                  break;
               }
            }

            if ((file_stk[stk_idx].file_ptr =
                    fopen(file_rec[i].name, "r")) != NULL) {
               file_stk[stk_idx].file_line = 0;
            }
            else {
#ifdef KEY /* Bug 7247 */
	       cannot_open_include_file(message_rec->uline);
	       goto CLEANUP;
#else /* KEY Bug 7247 */
               line_num = message_rec->uline;
               msg_insert = "can not open INCLUDE file.";
               goto EMERGENCY_EXIT;
#endif /* KEY Bug 7247 */
            }
         }
         else {
#ifdef KEY /* Bug 7247 */
	    files_too_deeply_nested(message_rec->uline);
	    goto CLEANUP;
#else /* KEY Bug 7247 */
            line_num = message_rec->uline;
            msg_insert = "file_stk size exceeded.";
            goto EMERGENCY_EXIT;
#endif /* KEY Bug 7247 */
         }

         goto CHECK_GL_IDX_AGAIN;
      }

      while (feof(file_stk[0].file_ptr) == 0) {

         if (fgets(source_line,
                   MAX_SRC_LINE_SIZE,
                   file_stk[0].file_ptr) != NULL  && 
             feof(file_stk[0].file_ptr) == 0) {
            fprintf(stderr, "%s", source_line);
         }
      }
   }

# endif

#ifdef KEY /* Bug 7247 */
/* Here is some work we must perform even if we just encountered an error
 * while dumping source statements.  */
CLEANUP:
#endif /* KEY Bug 7247 */

   fprintf(stderr, "\n");


# ifdef _DEBUG

   if (num_msgs != 0) { 
      Cif_Close(fd, CIF_MEM_FREE);
   }

# else

   Cif_Close(fd, CIF_MEM_FREE);

# endif


#ifdef KEY /* Bug 7247 */
   for (; stk_idx >= 0; stk_idx -= 1) {
      if (NULL != file_stk[stk_idx].file_ptr) {
	fclose(file_stk[stk_idx].file_ptr);
      }
   }
#else /* KEY Bug 7247 */
   while (stk_idx >= 0) {
      fclose(file_stk[stk_idx--].file_ptr);
   }
#endif /* KEY Bug 7247 */

EXIT:
   
   /* Set c_i_f to NULL to indicate the messages have been output but DON'T   */
   /* close it or delete it yet because if the compilation is being           */
   /* terminated by an Internal or Limit error, we need to output a special   */
   /* Summary record yet.						      */

   c_i_f = NULL;


   fclose(cif_tmp_file);
   remove(cif_tmp_file_name);

   TRACE (Func_Exit, "print_buffered_messages", NULL);

   return;


EMERGENCY_EXIT:

   if (c_i_f == cif_actual_file) {
      /* prevent closing the same file twice. Linux does not handle it */
      cif_actual_file = NULL;
   }
   fclose(c_i_f);
   c_i_f = NULL;
   remove(cif_name);
   remove(cif_tmp_file_name);
   Cif_Close(fd, CIF_MEM_FREE);
   PRINTMSG(line_num, 995, Internal, 0, msg_insert);

} /*  print_buffered_messages */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This is a compare routine for qsort to get the Message records        *|
|*      sorted in global line number then column number order.		      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

static int compare_message_recs(const void	 *p1,
			        const void 	 *p2)
{
   register int result;
   register int order1;
   register int order2;


   TRACE (Func_Entry, "compare_message_recs", NULL);

   result = ((struct Cif_message *) p1)->uline -
            ((struct Cif_message *) p2)->uline; 

   if (result == 0) {
      result = ((struct Cif_message *) p1)->cpos -
               ((struct Cif_message *) p2)->cpos;


      /* If both the line number and column number are the same, use the      */
      /* relative order field if it is set to break the tie.  If it's not set,*/
      /* use the message number to break the tie to avoid different "same     */
      /* key" algorithms across platforms.  This presumes, of course, that    */
      /* there will (almost?) never be a case where the same message is       */
      /* issued for the same line and column.                                 */
      /* Careful!  The messages could come from different phases.             */

      if (result == 0) {
         order1 = ((struct Cif_message *) p1)->order;
         order2 = ((struct Cif_message *) p2)->order;

         if ( (order1 == order2)  ||  (order1 == 0)  ||  (order2 == 0) ) {
            result = ((struct Cif_message *) p1)->msgno -
                     ((struct Cif_message *) p2)->msgno;
         }
         else {
	    result = ((struct Cif_message *) p1)->order -
                     ((struct Cif_message *) p2)->order;
         }
      }
   }

   TRACE (Func_Exit, "compare_message_recs", NULL);

   return (result);

}  /* compare_message_recs */
# if defined(_USE_FOLD_DOT_f)

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

#define FOLD_ABORT fold_f_abort__

void FOLD_ABORT(int *oper)

{


   TRACE (Func_Entry, "fold_f_abort_", NULL);

#ifdef KEY /* Bug 4867 */
   /* Message 626 (which is used in a lot of other places) doesn't take an
    * integer argument; also, the caller fold_operator_ is schizoid and
    * sometimes passes a simple integer operator enum, while other times it
    * shifts and adds and packed several pieces of data into the integer. */
   PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
            "supported operator or type", "FOLD_OPERATION");
#else /* KEY Bug 4867 */
   PRINTMSG(stmt_start_line, 626, Internal, *oper,
            "supported operator or type", "FOLD_OPERATION");
#endif /* KEY Bug 4867 */

   TRACE (Func_Exit, "fold_f_abort_", NULL);

   return;

}  /* fold_f_abort_ */
# endif
#ifdef KEY /* Bug 5040 */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	For use in calls to PRINTMSG					      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	Return "Ansi" if -ansi is in effect, else "Warning"                   *|
|*									      *|
\******************************************************************************/
msg_severities_type
ansi_or_warning(void) {
  return on_off_flags.issue_ansi_messages ? Ansi : Warning ;
  }
#endif /* KEY Bug 5040 */
