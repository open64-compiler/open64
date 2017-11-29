/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/debug.c	5.17	10/14/99 12:53:57\n";

# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/
# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "p_globals.m"
# include "debug.m"
# include "s_utils.m"
# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "p_globals.h"
# include "debug.h"

static void build_fake_token	(char *);
static void chain_thru_sn_ntries(FILE *, int, boolean);
static void dump_al_ntry	(FILE *, int);
static void dump_at_ntry	(FILE *, int, boolean);
static void dump_blk_ntry	(FILE *, int);
static void dump_bd_ntry	(FILE *, int);
static void dump_cn_ntry        (FILE *, int);
static void dump_dv		(FILE *, int_dope_type *, boolean);
static void dump_ga_ntry	(FILE *, int);
static void dump_gb_ntry	(FILE *, int);
static void dump_gl_ntry	(FILE *, int);
static void dump_gn_ntry	(FILE *, int);
static void dump_gt_ntry	(FILE *, int);
static void dump_hn_ntry	(FILE *, int, boolean);
static void dump_il_ntry        (FILE *, int);
static void dump_ir_ntry        (FILE *, int, int);
static void dump_ln_ntry	(FILE *, int, boolean);
static void dump_fp_ntry	(FILE *, int, boolean);
static void dump_ml_ntry	(FILE *, int);
static void dump_ro_ntry	(FILE *, int);
static void dump_sb_ntry	(FILE *, int);
static void dump_scp_ntry	(FILE *, int, int, boolean, boolean);
static void dump_sn_ntry	(FILE *, int);
static void dump_eq_ntry	(FILE *, int);
static void dump_stmt_ntry	(FILE *, boolean);
static void dump_typ_ntry	(FILE *, int);
static void dump_trace_info     (FILE *, trace_type, char *, char *); 
static void loop_thru_sn_ntries (FILE *, int, boolean);
static void print_all_text      (boolean);
static char *print_at_name	(int);
static void print_attr_name     (FILE *, int, int);
static void print_const_entry   (FILE *, int, int);
static void print_list          (FILE *, int, int, int, boolean);
#ifdef KEY /* Bug 6845 */
static void print_Dv_Whole_Def_Opr          (FILE *, int, int, int, int);
#else /* KEY Bug 6845 */
static void print_Dv_Whole_Def_Opr          (FILE *, int, int, int);
#endif /* KEY Bug 6845 */
static void print_mp_dir_opr          (FILE *, int, int, int);
static void print_open_mp_dir_opr(FILE *, int, int, int);
static void print_expanded_stmt_for_scp(void);
static void print_expanded_ir	(int);
static void print_expanded_il	(int);
static void print_expanded_opnd	(opnd_type);
static void print_expanded_const(int);
static void print_fld_idx	(FILE *, char *, fld_type,int);
static char *print_global_type_f(int);
static void print_tbl_header	(char *);
static void dump_io_type_code_ntry(FILE *, long_type *, int);

static boolean	full_debug_dump;

    
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      If this is a debug compiler, open the debug file to hold output.      *|
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

FILE * init_debug_file (void)

{
   if (debug_file == NULL) {
      full_debug_dump = TRUE;

      /* Set the name to cft90_dump, if this is called before or during */
      /* command line processing.  Usually this should be filename.l    */

      if (debug_file_name[0] == NULL_CHAR) {
         strcpy(debug_file_name, "cft90_dump");
      }

      debug_file = fopen(debug_file_name, "w");

      if (debug_file == NULL) {
         PRINTMSG(1, 17, Error, 0, debug_file_name);
         exit_compiler(RC_USER_ERROR);
      }
   }

   return(debug_file);

}  /* init_debug_file */

# ifdef _DEBUG


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	The following are routines to print full tables.  They are called     *|
|*	by using the -u commandline option with the table identifier.         *|
|*	The attr table is an exception.  It is called via -u sytb             *|
|*	With a few exceptions, there are no input parameters.  There are      *|
|*	no output parameters and these routines return nothing.               *|
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


/******************************************************************************\
|*									      *|
|*	Print bounds table.                                                   *|
|*									      *|
\******************************************************************************/

void print_bd_tbl (void)

{
   int	bd_idx;


   print_tbl_header("Bounds Table");

   bd_idx = 1;

   while (bd_idx < bounds_tbl_idx) {
       dump_bd_ntry(debug_file, bd_idx);
       bd_idx += BD_NTRY_SIZE(bd_idx);
   }

   bd_idx = BD_NEXT_FREE_NTRY(BD_FREE_LIST_IDX);

   while (bd_idx != NULL_IDX) {
      dump_bd_ntry(debug_file, bd_idx);
      bd_idx = BD_NEXT_FREE_NTRY(bd_idx);
   }

   putc ('\n', debug_file);
   fflush(debug_file);
   return;

}  /* print_bd_tbl */


/******************************************************************************\
|*									      *|
|*	Print block stack.                                                    *|
|*									      *|
\******************************************************************************/

void print_blk_tbl (void)

{
   int		blk_idx;


   print_tbl_header("BLOCK STACK");

   for (blk_idx = 1;  blk_idx <= blk_stk_idx;	blk_idx++) {
       dump_blk_ntry(debug_file, blk_idx);
   }

   putc ('\n', debug_file);
   fflush (debug_file);
   return;

}  /* print_blk_tbl */


/******************************************************************************\
|*									      *|
|*	Print constant table                                                  *|
|*									      *|
\******************************************************************************/

void print_cn_tbl (void)

{
   int			cn_idx;	


   print_tbl_header("Constant Table");

   for (cn_idx = 1; cn_idx <= const_tbl_idx; cn_idx++) {
       dump_cn_ntry(debug_file, cn_idx);
   }

   putc ('\n', debug_file);
   fflush (debug_file);
   return;

}  /* print_cn_tbl */
    
/******************************************************************************\
|*									      *|
|*	Print equivalence table                                               *|
|*									      *|
\******************************************************************************/

void print_eq_tbl (void)
{
   int      next_group;
   int      next_item;


   if (SCP_FIRST_EQUIV_GRP(curr_scp_idx) == NULL_IDX) {
      print_tbl_header("Equivalence Table is empty");
   }
   else {
      print_tbl_header("Equivalence Table");

      next_group = SCP_FIRST_EQUIV_GRP(curr_scp_idx);

      while (next_group != NULL_IDX) {
         fprintf(debug_file, "%21s\n", "NEW EQUIVALENCE GROUP");
         next_item = next_group;

         while (next_item != NULL_IDX) {
            dump_eq_ntry(debug_file, next_item);
            next_item = EQ_NEXT_EQUIV_OBJ(next_item);
         }
         next_group = EQ_NEXT_EQUIV_GRP(next_group);
      }
   }

   putc ('\n', debug_file);
   fflush (debug_file);
   return;

}  /* print_eq_tbl */

    
/******************************************************************************\
|*									      *|
|*	Print file path table                                                 *|
|*									      *|
\******************************************************************************/

void print_fp_tbl (void)

{
   int		fp_idx;

   print_tbl_header("File Path Table");

   fprintf(debug_file, "%s\n\n", "Module paths:");

   fp_idx = module_path_idx;

   while (fp_idx != NULL_IDX) {
      dump_fp_ntry(debug_file, fp_idx, TRUE);
      fp_idx = FP_NEXT_FILE_IDX(fp_idx);
   }

#ifdef KEY /* Bug 5089 */
   fprintf(debug_file, "%s\n\n", "Intrinsic module paths:");

   fp_idx = intrinsic_module_path_idx;

   while (fp_idx != NULL_IDX) {
      dump_fp_ntry(debug_file, fp_idx, TRUE);
      fp_idx = FP_NEXT_FILE_IDX(fp_idx);
   }
#endif /* KEY Bug 5089 */

   fprintf(debug_file, "%s\n\n", "Implicit Use Module Paths:");

   fp_idx = cmd_line_flags.implicit_use_idx;

   while (fp_idx != NULL_IDX) {
      dump_fp_ntry(debug_file, fp_idx, TRUE);
      fp_idx = FP_NEXT_FILE_IDX(fp_idx);
   }

   fprintf(debug_file, "%s\n\n", "Inline paths:");

   fp_idx = inline_path_idx;

   while (fp_idx != NULL_IDX) {
      dump_fp_ntry(debug_file, fp_idx, TRUE);
      fp_idx = FP_NEXT_FILE_IDX(fp_idx);
   }

   fprintf(debug_file, "%s\n\n", "Include paths:");

   print_fp_includes();

   putc ('\n', debug_file);
   fflush(debug_file);
   return;

}  /* print_fp_tbl */
    
/******************************************************************************\
|*									      *|
|*	Print global bounds table                                             *|
|*									      *|
\******************************************************************************/

void print_gb_tbl (void)

{
   int	 gb_idx;

   
   print_tbl_header("Global Bounds Table");

   gb_idx	= 1;

   while (gb_idx != NULL_IDX && gb_idx <= global_bounds_tbl_idx) {
      dump_gb_ntry(debug_file, gb_idx);

      if (GB_ARRAY_SIZE(gb_idx) == Constant_Size &&
          GB_ARRAY_CLASS(gb_idx) == Explicit_Shape) {
         gb_idx	+= (GB_RANK(gb_idx) * 2);
      }
      else {
         gb_idx++;
      }
       
   }

   putc ('\n', debug_file);
   fflush (debug_file);
   return;

}  /* print_gb_tbl */

    
/******************************************************************************\
|*									      *|
|*	Print global line table                                               *|
|*									      *|
\******************************************************************************/

void print_gl_tbl (void)

{
   int	 gl_idx;

   
   print_tbl_header("Global Line Table");

   for (gl_idx = 1; gl_idx <= global_line_tbl_idx; gl_idx++) {
       dump_gl_ntry(debug_file, gl_idx);
   }

   fprintf(debug_file,"\n    %-22s= %-10d  %-20s= %-10d\n",
                      "num_prog_unit_err", num_prog_unit_errors,
                      "num_ansi", num_ansi);

   fprintf(debug_file,"    %-22s= %-10d  %-20s= %-10d\n",
                      "num_warnings", num_warnings,
                      "num_cautions", num_cautions);

   fprintf(debug_file,"    %-22s= %-10d  %-20s= %-10d\n",
                      "num_notes", num_notes,
                      "num_comments", num_comments);

   putc ('\n', debug_file);
   fflush (debug_file);
   return;

}  /* print_gl_tbl */

    
/******************************************************************************\
|*									      *|
|*	Print global name table                                               *|
|*									      *|
\******************************************************************************/

void print_gn_tbl (void)

{
   int	 gn_idx;
   int	 gt_idx;

   
   print_tbl_header("Global Name Table");

   for (gn_idx = 2; gn_idx < global_name_tbl_idx; gn_idx++) {
       fprintf(debug_file, "\n****************************************"
                           "****************************************\n");
       dump_gn_ntry(debug_file, gn_idx);
       dump_ga_ntry(debug_file, GN_ATTR_IDX(gn_idx));
   }
   fprintf(debug_file, "\n****************************************"
                       "****************************************\n");

   for (gt_idx = 1; gt_idx <= global_type_tbl_idx; gt_idx++) {

      if (GT_TYPE(gt_idx) == Structure) {
         dump_ga_ntry(debug_file, GT_STRUCT_IDX(gt_idx));
      }
   }

   putc ('\n', debug_file);
   fflush (debug_file);
   return;

}  /* print_gn_tbl */

    
/******************************************************************************\
|*									      *|
|*	Print global type table                                               *|
|*									      *|
\******************************************************************************/

void print_gt_tbl (void)

{
   int	 gt_idx;

   
   print_tbl_header("Global Type Table");

   for (gt_idx = 1; gt_idx <= global_type_tbl_idx; gt_idx++) {
       dump_gt_ntry(debug_file, gt_idx);
   }

   putc ('\n', debug_file);
   fflush (debug_file);
   return;

}  /* print_gt_tbl */
    
/******************************************************************************\
|*									      *|
|*	Print hidden name table                                               *|
|*									      *|
\******************************************************************************/

void print_hn_tbl()

{
   int          hn_idx;


   print_tbl_header("Hidden Name Table");

   for (hn_idx = SCP_HN_FW_IDX(curr_scp_idx) + 1; 
        hn_idx < SCP_HN_LW_IDX(curr_scp_idx); 
        hn_idx++) {
       dump_hn_ntry(debug_file, hn_idx, FALSE);        /* Don't print attrs */
   }

   putc ('\n', debug_file);
   fflush (debug_file);
   return;

}  /* print_hn_tbl */

    
/******************************************************************************\
|*									      *|
|*	Print local name table without attr entries.                          *|
|*									      *|
\******************************************************************************/

void print_ln_tbl()

{
   int          ln_idx;

   print_tbl_header("Local Name Table");

   for (ln_idx = SCP_LN_FW_IDX(curr_scp_idx) + 1; 
        ln_idx < SCP_LN_LW_IDX(curr_scp_idx); 
        ln_idx++) {
       dump_ln_ntry(debug_file, ln_idx, FALSE);        /* Don't print attrs */
   }

   putc ('\n', debug_file);
   fflush (debug_file);
   return;

}  /* print_ln_tbl */

    
/******************************************************************************\
|*									      *|
|*	Print module link table                                               *|
|*									      *|
\******************************************************************************/

void print_ml_tbl()

{
   int          ml_idx;


   print_tbl_header("Module Link Table");

   fprintf(debug_file, 
       "     NOTE:  Only print entries that have at least one nonzero index\n");

   for (ml_idx = 0; ml_idx <= mod_link_tbl_idx; ml_idx++) {

      if (ML_AT_IDX(ml_idx) != NULL_IDX ||
          ML_BD_IDX(ml_idx) != NULL_IDX ||
          ML_CN_IDX(ml_idx) != NULL_IDX ||
          ML_LN_IDX(ml_idx) != NULL_IDX ||
          ML_NP_IDX(ml_idx) != NULL_IDX ||
          ML_SB_IDX(ml_idx) != NULL_IDX ||
          ML_IL_IDX(ml_idx) != NULL_IDX ||
          ML_IR_IDX(ml_idx) != NULL_IDX ||
          ML_CP_IDX(ml_idx) != NULL_IDX ||
          ML_SH_IDX(ml_idx) != NULL_IDX ||
          ML_TYP_IDX(ml_idx) != NULL_IDX ||
          ML_SN_IDX(ml_idx) != NULL_IDX) {
         dump_ml_ntry(debug_file, ml_idx);
      }
   }

   putc ('\n', debug_file);
   fflush (debug_file);
   return;

}  /* print_ml_tbl */

    
/******************************************************************************\
|*									      *|
|*	Print rename only table                                               *|
|*									      *|
\******************************************************************************/

void print_ro_tbl (int ro_start_idx)

{
   int	ro_idx;


   print_tbl_header("Rename Only Table");

   ro_idx = ro_start_idx;

   while (ro_idx != NULL_IDX) {
       dump_ro_ntry(debug_file, ro_idx);
       ro_idx = RO_NEXT_IDX(ro_idx);
   }

   putc ('\n', debug_file);
   fflush(debug_file);
   return;

}  /* print_ro_tbl */

    
/******************************************************************************\
|*									      *|
|*	Print storage block table                                             *|
|*									      *|
\******************************************************************************/

void print_sb_tbl (void)

{
   int	sb_idx;


   print_tbl_header("Storage Block Table");

   for (sb_idx = 1; sb_idx <= stor_blk_tbl_idx; sb_idx++) {
       dump_sb_ntry(debug_file, sb_idx);
   }

   putc ('\n', debug_file);
   fflush(debug_file);
   return;

}  /* print_sb_tbl */

    
/******************************************************************************\
|*									      *|
|*	Print scope table                                                     *|
|*									      *|
\******************************************************************************/

void print_scp_tbl(void)

{
   print_tbl_header("Scope Table");

   dump_scp_ntry(debug_file, 0, 0, FALSE, TRUE);

   putc ('\n', debug_file);
   fflush (debug_file);
   return;

}  /* print_scp_tbl */

    
/******************************************************************************\
|*									      *|
|*	Print statement header table                                          *|
|*									      *|
|*      This procedure is the driver for printing all Statement Headers and   *|
|*      all their IR.  A separate driver is required, like the Semantics Pass *|
|*      driver, because the underlying routine is called recursively and      *|
|*      because we always want to start with the first SCP entry.             *|
|*                                                                            *|
\******************************************************************************/

void  print_sh_tbl (boolean	print_all_scps)

{
   int	save_curr_scp_idx;
   int  save_curr_stmt_sh_idx;


   print_tbl_header("Statement Header Table");

   if (print_all_scps) {
      save_curr_scp_idx     = curr_scp_idx;
      curr_scp_idx          = 1;
      save_curr_stmt_sh_idx = curr_stmt_sh_idx;

      print_all_text(TRUE);

      curr_scp_idx     = save_curr_scp_idx;
      curr_stmt_sh_idx = save_curr_stmt_sh_idx;
   }
   else {
      print_all_text(FALSE);		/* Just print current scope */
   }

   return;

}  /* print_sh_tbl */

    
/******************************************************************************\
|*									      *|
|*	Print type table                                                      *|
|*									      *|
\******************************************************************************/

void print_typ_tbl (void)

{
   int		type_idx;


   print_tbl_header("Type Table");

   for (type_idx = 1; type_idx <= type_tbl_idx; type_idx++) {
      dump_typ_ntry(debug_file, type_idx);
   }

   putc ('\n', debug_file);
   fflush(debug_file);
   return;

}  /* print_typ_tbl */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Print commandline information to debug_file.                          *|
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

void print_cmd_tbl (void)

{
   extern	char	       *directive_str[];
   		int		dir_idx;
		boolean		first;
   		int		fp_idx;
		int		i,j;


   print_tbl_header("Commandline Flags");

   fprintf(debug_file, "  %-17s = %-s\n",
           "   source file", src_file);
   fprintf(debug_file, "  %-17s = %-s\n",
           "-b binary_output", (cmd_line_flags.binary_output)?bin_file:"NONE");
   fprintf(debug_file, "  %-17s= %-s\n",
           "-S assembly_output", (cmd_line_flags.assembly_output) ? 
                                                 assembly_file:"NONE");

   if (include_path_idx == NULL_IDX) {
      fprintf(debug_file, "  %-17s = %-s\n", "-I include paths", "NONE");
   }
   else {
      fprintf(debug_file, "  %-17s\n", "-I include paths");
      print_fp_includes();
   }

   if (module_path_idx == NULL_IDX) {
      fprintf(debug_file, "  %-17s = %-s\n", "-p module paths", "NONE");
   }
   else {
      fprintf(debug_file, "  %-17s\n", "-p module paths");
      fp_idx = module_path_idx;

      while (fp_idx != NULL_IDX) {
         fprintf(debug_file, "%4s%-s\n", " ", FP_NAME_PTR(fp_idx));
         fp_idx = FP_NEXT_FILE_IDX(fp_idx);
      }
   }
   fprintf(debug_file, "  %-17s = %-2s  %-18s = %-2s  %-18s = %-7s\n",
           "-align8", boolean_str[cmd_line_flags.align8],
           "-align16", boolean_str[cmd_line_flags.align16],
           "-align32", boolean_str[cmd_line_flags.align32]);

   fprintf(debug_file, "  %-17s = %-2s\n",
           "-align64", boolean_str[cmd_line_flags.align64]);

   fprintf(debug_file, "  %-17s = %-2s  %-18s = %-2s  %-18s = %-7s\n",
           "-a dalign", boolean_str[cmd_line_flags.dalign],
           "-a taskcommon", boolean_str[cmd_line_flags.taskcommon],
           "-f ", src_form_str[cmd_line_flags.src_form]);

   fprintf(debug_file, "  %-17s = %-27s  %-18s = %-7s\n",
           "-i ", integer_size_str[cmd_line_flags.integer_32],
           "-k solaris_profile", boolean_str[cmd_line_flags.solaris_profile]);

   fprintf(debug_file, "  %-17s = %-27s  %-18s = %-7s\n",
           "-m ", msg_lvl_str[cmd_line_flags.msg_lvl_suppressed],
           "-s float64", boolean_str[cmd_line_flags.s_float64]);

   fprintf(debug_file, "  %-17s = %-2s  %-18s = %-2s  %-18s = %-7s\n",
           "-s default64", boolean_str[cmd_line_flags.s_default64],
           "-s default32", boolean_str[cmd_line_flags.s_default32],
           "-s cf77types", boolean_str[cmd_line_flags.s_cf77types]);

   fprintf(debug_file, "  %-17s = %-2s  %-18s = %-2s  %-18s = %-7s\n",
           "-s integer8", boolean_str[cmd_line_flags.s_integer8],
           "-s logical8", boolean_str[cmd_line_flags.s_logical8],
           "-s real8", boolean_str[cmd_line_flags.s_real8]);


   fprintf(debug_file, "  %-17s = %-2s  %-18s = %-2s  %-18s = %-7s\n",
           "-s complex8", boolean_str[cmd_line_flags.s_complex8],
           "-s pointer8", boolean_str[cmd_line_flags.s_pointer8],
           "-s doublecomplex16", boolean_str[cmd_line_flags.s_doublecomplex16]);

   fprintf(debug_file, "  %-42s = %-2s\n",
           "-s doubleprecision16",
                     boolean_str[cmd_line_flags.s_doubleprecision16]);

   fprintf(debug_file, "  %-17s = %-2s  %-18s = %-s\n",
	   "-t truncate_bits", boolean_str[cmd_line_flags.truncate_bits],
           "-G ", debug_lvl_str[cmd_line_flags.debug_lvl]);

   fprintf(debug_file, "  %-17s = ", "-N line_size");

   if (cmd_line_flags.line_size_80) {
      fprintf(debug_file, "%-7d\n", 80);
   }
   else if (cmd_line_flags.line_size_132) {
      fprintf(debug_file, "%-7d\n", 132);
   }
   else {
      fprintf(debug_file, "%-7d\n", 72);
   }

   fprintf(debug_file, "  %-17s = %-2s  %-18s = %-2s  %-18s = %-7s\n",
           "-Ps small pic", boolean_str[(cmd_line_flags.small_pic_model)],
           "-Pl large pic", boolean_str[(cmd_line_flags.large_pic_model)],
           "-R a", boolean_str[cmd_line_flags.runtime_argument]);

   fprintf(debug_file, "  %-17s = %-2s  %-18s = %-2s  %-18s = %-7s\n",
           "-R b", boolean_str[cmd_line_flags.runtime_bounds],
           "-R c", boolean_str[cmd_line_flags.runtime_conformance],
           "-R C", boolean_str[cmd_line_flags.runtime_arg_call]);

   fprintf(debug_file, "  %-17s = %-2s  %-18s = %-2s  %-18s = %-7s\n",
           "-R E", boolean_str[cmd_line_flags.runtime_arg_entry],
           "-R s", boolean_str[cmd_line_flags.runtime_substring],
           "-R n", boolean_str[cmd_line_flags.runtime_arg_count_only]);

   fprintf(debug_file, "  %-17s = %-2s  %-18s = %-2s  %-18s = %-d\n",
           "-V verify_option", boolean_str[(cmd_line_flags.verify_option)],
           "-X m", boolean_str[(cmd_line_flags.malleable)],
           "-X npes", cmd_line_flags.MPP_num_pes);


   fprintf(debug_file, "\n%s On/Off Flags  (-e/-d)%s\n\n",
                       "  -------------------------- ",
                       "  -------------------------- ");

   fprintf(debug_file, "  %-26s = %-2s    %-26s = %-2s\n",
                       "a  abort_if_any_errors",
			  boolean_str[on_off_flags.abort_if_any_errors],
                       "e  ieee",
			  boolean_str[on_off_flags.ieee]);
   fprintf(debug_file, "  %-26s = %-2s    %-26s = %-2s\n",
                       "f  flowtrace_option",
			  boolean_str[on_off_flags.flowtrace_option],
                       "g  assembly_listing_file",
			  boolean_str[on_off_flags.assembly_listing_file]);
   fprintf(debug_file, "  %-26s = %-2s    %-26s = %-2s\n",
                       "i  indef_init",
			  boolean_str[on_off_flags.indef_init],
                       "j  exec_doloops_once",
			  boolean_str[on_off_flags.exec_doloops_once]);
   fprintf(debug_file, "  %-26s = %-2s    %-26s = %-2s\n",
                       "n  issue_ansi_messages",
			  boolean_str[on_off_flags.issue_ansi_messages],
                       "p  enable_double_precision",
			  boolean_str[on_off_flags.enable_double_precision]);
   fprintf(debug_file, "  %-26s = %-2s    %-26s = %-2s\n",
                       "q  abort_on_100_errors",
			  boolean_str[on_off_flags.abort_on_100_errors],
                       "r  round_mult_operations",
			  boolean_str[on_off_flags.round_mult_operations]);
   fprintf(debug_file, "  %-26s = %-2s    %-26s = %-2s\n",
                       "t  alloc_autos_on_stack",
			  boolean_str[on_off_flags.alloc_autos_on_stack],
                       "u  round_integer_divide",
			  boolean_str[on_off_flags.round_integer_divide]);
   fprintf(debug_file, "  %-26s = %-2s    %-26s = %-2s\n",
                       "u  reciprical_divide",
			  boolean_str[on_off_flags.reciprical_divide],
                       "v  save_all_vars",
			  boolean_str[on_off_flags.save_all_vars]);
   fprintf(debug_file, "  %-26s = %-2s    %-26s = %-2s\n",
                       "x  allow_leading_uscore",
			  boolean_str[on_off_flags.allow_leading_uscore],
                       "A  MPP_apprentice",
			  boolean_str[on_off_flags.MPP_apprentice]);
   fprintf(debug_file, "  %-26s = %-2s    %-26s = %-2s\n",
                       "B  binary_output",
			  boolean_str[on_off_flags.binary_output],
                       "C  shared_to_private_coer",
			  boolean_str[on_off_flags.shared_to_private_coer]);
   fprintf(debug_file, "  %-26s = %-2s    %-26s = %-2s\n",
                       "I  implicit_none",
			  boolean_str[on_off_flags.implicit_none],
                       "P  preprocess_only",
			  boolean_str[on_off_flags.preprocess_only]);
   fprintf(debug_file, "  %-26s = %-2s    %-26s = %-2s\n",
                       "Q  allow_leading_uscore",
			  boolean_str[on_off_flags.allow_leading_uscore],
                       "R  recursive",
			  boolean_str[on_off_flags.recursive]);
   fprintf(debug_file, "  %-26s = %-2s    %-26s = %-2s\n",
                       "S  assembly_output",
			  boolean_str[on_off_flags.assembly_output],
                       "U  upper_case_names",
			  boolean_str[on_off_flags.upper_case_names]);
   fprintf(debug_file, "  %-26s = %-2s    %-26s = %-2s\n",
                       "X  atexpert",
			  boolean_str[on_off_flags.atexpert],
                       "Z  save_dot_i",
			  boolean_str[on_off_flags.save_dot_i]);

   fprintf(debug_file, "\n%s Optimization Flags (-O)%s\n\n",
                       "  ------------------------- ",
                       "  ------------------------- ");

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2s\n",
                       "aggress", boolean_str[opt_flags.aggress],
                       "bottom_load", boolean_str[opt_flags.bottom_load]);

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2s  %-18s = %-2d\n",
                       "fusion", boolean_str[opt_flags.fusion],
                       "ieeeconform", boolean_str[opt_flags.ieeeconform],
                       "inline_lvl", opt_flags.inline_lvl);

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2s  %-18s = %-2s\n",
                       "jump", boolean_str[opt_flags.jump],
                       "loopalign", boolean_str[opt_flags.loopalign],
                       "mark", boolean_str[opt_flags.mark]);

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2s  %-18s = %-2s\n",
                       "modinline", boolean_str[opt_flags.modinline],
                       "msgs", boolean_str[opt_flags.msgs],
                       "neg_msgs", boolean_str[opt_flags.neg_msgs]);

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2s  %-18s = %-2s\n",
                       "nointerchange", boolean_str[opt_flags.nointerchange],
                       "overindex", boolean_str[opt_flags.over_index],
                       "pattern", boolean_str[opt_flags.pattern]);

   fprintf(debug_file, "  %-17s = %-2d\n",
                       "pipeline", opt_flags.pipeline_lvl);

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2d  %-18s = %-2d\n",
                       "recurrence", boolean_str[opt_flags.recurrence],
                       "scalar", opt_flags.scalar_lvl,
                       "split", opt_flags.split_lvl);

   fprintf(debug_file, "  %-17s = %-2d   %-18s = %-2d  %-18s = %-2s\n",
                       "support_lvl", opt_flags.support_lvl,
                       "task", opt_flags.task_lvl,
                       "taskinner", boolean_str[opt_flags.taskinner]);

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2s  %-18s = %-2d\n",
                       "threshold", boolean_str[opt_flags.threshold],
                       "vsearch", boolean_str[opt_flags.vsearch],
                       "unroll", opt_flags.unroll_lvl);

   fprintf(debug_file, "  %-17s = %-2d   %-18s = %-2s  %-18s = %-2s\n",
                       "vector", opt_flags.vector_lvl,
                       "vsearch", boolean_str[opt_flags.vsearch],
                       "zeroinc", boolean_str[opt_flags.zeroinc]);


   fprintf(debug_file, "\n%s Disregard Flags  (-x)%s\n\n",
                       "  -------------------------- ",
                       "  -------------------------- ");

   fprintf(debug_file, " ");

   /* 0 is Tok_Dir_Start - so skip it. */

   for (dir_idx = 1; (dir_idx < (Tok_Dir_End - Tok_Dir_Start -1)); dir_idx++) {
      fprintf(debug_file, "%-20s = %-2s ", directive_str[dir_idx], 
                          boolean_str[disregard_directive[dir_idx]]);

      if ((dir_idx%3) == 0) {
         fprintf(debug_file, "\n ");
      }
   }

   fprintf(debug_file, "\n\n%s Dump Flags (-u)%s\n\n",
                       "  ----------------------------- ",
                       "  ----------------------------- ");

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2s  %-18s = %-2s\n",
                     "abort_ansi", boolean_str[dump_flags.abort_on_ansi],
                     "no_dim_pad", boolean_str[dump_flags.no_dimension_padding],
                     "no_mod_output", boolean_str[dump_flags.no_module_output]);

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2s\n",
                     "bd", boolean_str[dump_flags.bd_tbl],
                     "blk", boolean_str[dump_flags.blk_stk]);

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2s  %-18s = %-2s\n",
                       "cmd", boolean_str[dump_flags.cmd_line_tbls],
                       "cn", boolean_str[dump_flags.cn_tbl],
                       "defines", boolean_str[dump_flags.defines]);

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2s  %-18s = %-2s\n",
                       "fortran", boolean_str[dump_flags.fort_out],
                       "fp", boolean_str[dump_flags.fp_tbl],
                       "ftrace", boolean_str[dump_flags.ftrace_info]);

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2s  %-18s = %-2s\n",
                       "gl", boolean_str[dump_flags.gl_tbl],
                       "intrin", boolean_str[dump_flags.intrin_tbl],
                       "ir1", boolean_str[dump_flags.ir1_tbl]);

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2s  %-18s = %-2s\n",
                       "ir2", boolean_str[dump_flags.ir2_tbl],
                       "ir3", boolean_str[dump_flags.ir3_tbl],
                       "ir4", boolean_str[dump_flags.ir4_tbl]);

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2s  %-18s = %-2s\n",
                       "mem_report", boolean_str[dump_flags.mem_report],
                       "mtrace", boolean_str[dump_flags.mtrace_info],
                       "names", boolean_str[dump_flags.name_tbls]);

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2s  %-18s = %-2s\n",
                       "pdg", boolean_str[dump_flags.pdgcs],
                       "pdt", boolean_str[dump_flags.pdt_dump],
                       "sb", boolean_str[dump_flags.sb_tbl]);

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2s  %-18s = %-2s\n",
                       "scp", boolean_str[dump_flags.scp_tbl],
                       "src", boolean_str[dump_flags.src_dmp],
                       "stderr", boolean_str[dump_flags.std_err]);

   fprintf(debug_file, "  %-17s = %-2s   %-18s = %-2s  %-18s = %-2s\n",
                       "stmt", boolean_str[dump_flags.stmt_dmp],
                       "sytb", boolean_str[dump_flags.sytb],
                       "typ", boolean_str[dump_flags.typ_tbl]);

   fprintf(debug_file, "\n\n%s Message Options %s \n\n",
                       "  ----------------------------- ",
                       "  ----------------------------- ");

   first	= TRUE;

   for (i = 0;  i < MAX_MSG_SIZE;  ++i) {

      if (message_suppress_tbl[i] != 0) {

         for (j = i * HOST_BITS_PER_WORD; j < (i+1) * HOST_BITS_PER_WORD; ++j) {

            if (GET_MESSAGE_TBL(message_suppress_tbl, j)) {

               if (!first) {
                  first = FALSE;
                  fprintf(debug_file, ",");
               }
               fprintf(debug_file, "  %d", j);
            }
         }
      }

      if (message_warning_tbl[i] != 0) {

         for (j = i * HOST_BITS_PER_WORD; j < (i+1) * HOST_BITS_PER_WORD; ++j) {

            if (GET_MESSAGE_TBL(message_warning_tbl, j)) {

               if (!first) {
                  first = FALSE;
                  fprintf(debug_file, ",");
               }
               fprintf(debug_file, "  W%d", j);
            }
         }
      }

      if (message_error_tbl[i] != 0) {

         for (j = i * HOST_BITS_PER_WORD; j < (i+1) * HOST_BITS_PER_WORD; ++j) {

            if (GET_MESSAGE_TBL(message_error_tbl, j)) {

               if (!first) {
                  first = FALSE;
                  fprintf(debug_file, ",");
               }
               fprintf(debug_file, "  E%d", j);
            }
         }
      }
   }

   putc ('\n', debug_file);

   putc ('\n', debug_file);
   fflush (debug_file);
   return;

}  /* print_cmd_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Using the scope table to find the start and stop of the local name    *|
|*	table, print all the local name entries each scope.  Use dump_ln_ntry *|
|*	and have it print all the complete attr entries for each local name.  *|
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

void print_sytb (int		scp_idx,
		 boolean	print_all_scps,
		 boolean	dump_all)

{
   int	al_idx;
   char header[60];
   int	ln_idx;
   int	save_scp_idx;


PROCESS_SIBLING:

   if (scp_idx == INTRINSIC_SCP_IDX) {  /* Intrinsic scope */
      print_tbl_header("Intrinsic Symbol Table Dump");
   }
   else if (SCP_ATTR_IDX(scp_idx) != NULL_IDX) {
      header[0] = '\0';
      strcat(header, AT_OBJ_NAME_PTR(SCP_ATTR_IDX(scp_idx)));
      print_tbl_header(strcat(header, " Symbol Table Dump"));
   }
   else {
      print_tbl_header("Unnamed Symbol Table Dump");
   }

   for (ln_idx = SCP_LN_FW_IDX(scp_idx)+1; ln_idx < SCP_LN_LW_IDX(scp_idx);
                                                    ln_idx++) {
      fprintf(debug_file, "****************************************"
                          "****************************************\n");
      dump_ln_ntry(debug_file, ln_idx, dump_all);
   }

   al_idx = SCP_ATTR_LIST(scp_idx);

   while (al_idx != NULL_IDX) {
      fprintf(debug_file, "****************************************"
                          "****************************************\n");
      dump_at_ntry(debug_file, AL_ATTR_IDX(al_idx), dump_all);
      al_idx = AL_NEXT_IDX(al_idx);
   }

   fprintf(debug_file, "****************************************"
                       "****************************************\n");

   if (print_all_scps) {

      if (SCP_FIRST_CHILD_IDX(scp_idx) != NULL_IDX) {
         save_scp_idx	= scp_idx;
         scp_idx	= SCP_FIRST_CHILD_IDX(scp_idx);
         print_sytb(scp_idx, TRUE, TRUE);
         scp_idx	= save_scp_idx;
      }

      if (SCP_SIBLING_IDX(scp_idx) != NULL_IDX) {
         scp_idx	= SCP_SIBLING_IDX(scp_idx);
         goto PROCESS_SIBLING;
      }
   }

   return;

}  /* print_sytb */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints the compressed table.                                          *|
|*									      *|
|* Input parameters:							      *|
|*      loc name tbl index to start.                                          *|
|*	loc name tbl index for end.                                           *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
void print_compressed_sytb(int	ln_start,
			   int	ln_end)
{
   int		ln_idx;

   print_tbl_header("Compressed Symbol Table");

   for (ln_idx = ln_start; ln_idx <= ln_end; ln_idx++) {
      dump_ln_ntry(debug_file, ln_idx, TRUE);  /* Print the attr too */
   }

   putc ('\n', debug_file);
   fflush (debug_file);

   return;

}  /* print_compressed_sytb */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	The following set of utilities dump one entry to stderr.  There is    *|
|*	one for each table.  Input is the table index to print.   These are   *|
|*	external routines so they may be called from the debugger or inserted *|
|*	in code as 'print' statements.                                        *|
|*									      *|
|* Input parameters:							      *|
|*	Index of table entry to print.					      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

/******************************************************************************\
|*									      *|
|* Attribute list table							      *|
|*									      *|
\******************************************************************************/

void	print_al (int	al_idx)

{
   if (al_idx <= attr_list_tbl_idx) {
      dump_al_ntry(stderr, al_idx);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid Attribute List Table index.\n");
   }

   return;

}  /* print_al */

/******************************************************************************\
|*									      *|
|* Attribute Table  							      *|
|*									      *|
\******************************************************************************/

void	print_at(int		at_idx)

{

   if (at_idx <= attr_tbl_idx) {
      dump_at_ntry(stderr, at_idx, FALSE);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid Attribute Table index.\n");
   }

   return;

}  /* print_at */

#ifdef KEY
/******************************************************************************\
|* Like print_at_all, but for entire table                                    *|
|* If "file" is null, use ".l" file                                           *|
\******************************************************************************/
void	print_at_table(FILE *file)
{
   init_debug_file();
   dump_trace_info (debug_file, PU_Start, NULL, "print_at_all");
   for (int i = 0; i < attr_tbl_idx; i += 1) {
      dump_at_ntry(debug_file, i, TRUE);
   }
}
#endif /* KEY */

/******************************************************************************\
|*									      *|
|* Attribute Table  							      *|
|*									      *|
\******************************************************************************/

void	print_at_all(int		at_idx)

{
   if (at_idx <= attr_tbl_idx) {
      dump_at_ntry(stderr, at_idx, TRUE);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid Attribute Table index.\n");
   }

   return;
}  /* print_at_all */

/******************************************************************************\
|*									      *|
|* Bounds table 							      *|
|*									      *|
\******************************************************************************/

void print_bd (int	bd_idx)

{
   if (bd_idx <= bounds_tbl_idx) {
      dump_bd_ntry(stderr, bd_idx);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid bounds table index.\n");
   }

   return;

}  /* print_bd */

/******************************************************************************\
|*									      *|
|* Block stack					          		      *|
|*									      *|
\******************************************************************************/

void	print_blk (int	blk_idx)

{
   if (blk_idx <= blk_stk_idx) {
      dump_blk_ntry(stderr, blk_idx);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid block stack index.\n");
   }

   return;

}  /* print_blk */

/******************************************************************************\
|*									      *|
|* Constant table							      *|
|*									      *|
\******************************************************************************/

void	print_cn (int	cn_idx)

{
   if (cn_idx <= const_tbl_idx) {
      dump_cn_ntry(stderr, cn_idx);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid constant table index.\n");
   }

   return;

}  /* print_cn */

/******************************************************************************\
|*                                                                            *|
|* Equivalence Table							      *|
|*                                                                            *|
\******************************************************************************/

void	print_eq (int	eq_idx)

{

   if (eq_idx <= equiv_tbl_idx) {
      dump_eq_ntry(stderr, eq_idx);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid equivalence table index.\n");
   }

   return;

}  /* print_eq */

/******************************************************************************\
|*									      *|
|* file path table      			 			      *|
|*									      *|
\******************************************************************************/

void	print_fp (int	fp_idx)

{
   if (fp_idx <= file_path_tbl_idx) {
      dump_fp_ntry(stderr, fp_idx, FALSE);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid file path table index.\n");
   }

   return;

}  /* print_fp */

/******************************************************************************\
|*                                                                            *|
|* Print a single list table entry.                                           *|
|*                                                                            *|
\******************************************************************************/

void	print_il (int	il_idx)

{
   if (il_idx <= ir_list_tbl_idx) {
      dump_il_ntry(stderr, il_idx);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid ir list table index %d.\n", 
                      il_idx);
   }

   return;
} 

/******************************************************************************\
|*                                                                            *|
|* IR tree								      *|
|*                                                                            *|
\******************************************************************************/

void  print_ir (int	ir_idx)

{
   if (ir_idx <= ir_tbl_idx) {
      dump_ir_ntry(stderr, ir_idx, 1);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid ir table index.\n");
   }

   return;

}  /* print_ir */

/******************************************************************************\
|*									      *|
|* Global attr table							      *|
|*									      *|
\******************************************************************************/

void	print_ga (int	ga_idx)

{
   if (ga_idx <= global_attr_tbl_idx) {
      dump_ga_ntry(stderr, ga_idx);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid global attr table index.\n");
   }

   return;

}  /* print_ga */

/******************************************************************************\
|*									      *|
|* Global bounds table							      *|
|*									      *|
\******************************************************************************/

void	print_gb (int	gb_idx)

{
   if (gb_idx <= global_bounds_tbl_idx) {
      dump_gb_ntry(stderr, gb_idx);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid global bounds table index.\n");
   }

   return;

}  /* print_gb */

/******************************************************************************\
|*									      *|
|* Global line table							      *|
|*									      *|
\******************************************************************************/

void	print_gl (int	gl_idx)

{
   if (gl_idx <= global_line_tbl_idx) {
      dump_gl_ntry(stderr, gl_idx);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid global name table index.\n");
   }

   return;

}  /* print_gl */

/******************************************************************************\
|*									      *|
|* Global name table							      *|
|*									      *|
\******************************************************************************/

void	print_gn (int	gn_idx)

{
   if (gn_idx <= global_name_tbl_idx) {
      dump_gn_ntry(stderr, gn_idx);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid global name table index.\n");
   }

   return;

}  /* print_gn */

/******************************************************************************\
|*									      *|
|* Global type table							      *|
|*									      *|
\******************************************************************************/

void	print_gt (int	gt_idx)

{
   if (gt_idx <= global_type_tbl_idx) {
      dump_gt_ntry(stderr, gt_idx);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid global type table index.\n");
   }

   return;

}  /* print_gt */

/******************************************************************************\
|*									      *|
|* Hidden name table							      *|
|*									      *|
\******************************************************************************/

void	print_hn (int	hn_idx)
{

   if (hn_idx <= hidden_name_tbl_idx) {
      dump_hn_ntry(stderr, hn_idx, FALSE);         /* Don't print attrs */
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid hidden name table index.\n");
   }

   return;
     
}  /* print_hn */

/******************************************************************************\
|*									      *|
|* Local name table							      *|
|*									      *|
\******************************************************************************/

void print_ln (int	ln_idx)
{
   if (ln_idx <= loc_name_tbl_idx) {
      dump_ln_ntry(stderr, ln_idx, FALSE);         /* Don't print attrs */
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid local name table index.\n");
   }

   return;
     
}  /* print_ln */

/******************************************************************************\
|*									      *|
|* Local name table							      *|
|*									      *|
\******************************************************************************/

void print_lnr (int	ln_idx,
		int	end_idx)
{
   while (ln_idx <= end_idx) {

      if (ln_idx <= loc_name_tbl_idx) {
         dump_ln_ntry(stderr, ln_idx, FALSE);         /* Don't print attrs */
      }
      else {
         fprintf(stderr, "\n*FE90-ERROR* Invalid local name table index.\n");
      }
      ++ln_idx;
   }

   return;
     
}  /* print_lnr */

/******************************************************************************\
|*									      *|
|* Module link table      			 			      *|
|*									      *|
\******************************************************************************/

void	print_ml (int	ml_idx)

{
   if (ml_idx <= mod_link_tbl_idx) {
      dump_ml_ntry(stderr, ml_idx);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid module link table index.\n");
   }

   return;
     
}  /* print_ml */

/******************************************************************************\
|*									      *|
|* Rename only table      			 			      *|
|*									      *|
\******************************************************************************/

void print_ro (int	ro_idx)

{
   if (ro_idx <= rename_only_tbl_idx) {
      dump_ro_ntry(stderr, ro_idx);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid rename only table index.\n");
   }

   return;

}  /* print_ro */

/******************************************************************************\
|*									      *|
|* Storage Block Table							      *|
|*									      *|
\******************************************************************************/

void	print_sb (int	sb_idx)

{
   if (sb_idx <= stor_blk_tbl_idx) {
      dump_sb_ntry(stderr, sb_idx);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid Storage Block Table index.\n");
   }

   return;

}  /* print_sb */

/******************************************************************************\
|*									      *|
|* Scope table  (print_impl_tbl - if TRUE print implicit table) 	      *|
|*									      *|
\******************************************************************************/

void	print_scp(int		scp_idx,
                  boolean	print_impl_tbl)

{
   if (scp_idx <= scp_tbl_idx) {
      dump_scp_ntry(stderr, scp_idx, 0, print_impl_tbl, FALSE);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid scope table index.\n");
   }

   return;

}  /* print_scp */

/******************************************************************************\
|*                                                                            *|
|*      Print one statement and its IR.                                       *|
|*                                                                            *|
\******************************************************************************/

void  print_sh (int	stmt_idx)

{
   int		save_curr_stmt_sh_idx;


   if (stmt_idx <= sh_tbl_idx) {
      save_curr_stmt_sh_idx = curr_stmt_sh_idx;
      curr_stmt_sh_idx      = stmt_idx;

      dump_stmt_ntry(stderr, TRUE);  /* Print  IR for this statement */

      curr_stmt_sh_idx = save_curr_stmt_sh_idx;
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* Invalid statement header index.\n");
   }

   return;

} /* print_sh */

/******************************************************************************\
|*									      *|
|* Secondary Name Table							      *|
|*									      *|
\******************************************************************************/

void	print_sn (int	sn_idx)

{
   dump_sn_ntry(stderr, sn_idx);

   return;

}  /* print_sn */

/******************************************************************************\
|*									      *|
|* Type table           			 			      *|
|*									      *|
\******************************************************************************/

void print_typ (int	type_idx)

{
   dump_typ_ntry(stderr, type_idx);

   return;

}  /* print_typ */


/******************************************************************************\
|*									      *|
|* Miscellaneous routines and utility routines.			 	      *|
|*									      *|
\******************************************************************************/

/******************************************************************************\
|*                                                                            *|
|* Input parameters:                                                          *|
|*      address of internal dope vector.                                      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void print_dv (int_dope_type    *dv,
               boolean          dump_it)

{
   dump_dv(stderr, dv, dump_it);

   return;

}  /* print_dv */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Given the name of a local entity, this procedure prints a single      *|
|*      Local Name Table entry to stderr.   Does not print the attr entry.    *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*                                         				      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void print_ln_by_name (void)      

{
   char		name_string[MAX_ID_LEN + 1];
   int  	ln_tbl_idx;


   printf("Enter LOCAL name->");
#ifdef KEY
   fgets(name_string, MAX_ID_LEN, stdin);
#else /* KEY */
   gets(name_string);
#endif /* KEY */
   build_fake_token(name_string);

   if (srch_sym_tbl(TOKEN_STR(fake_token), TOKEN_LEN(fake_token),
                                           &ln_tbl_idx) != NULL_IDX) {
      dump_ln_ntry(stderr, ln_tbl_idx, FALSE);
   }
   else {
      fprintf(stderr, "\n*FE90-ERROR* No such name in the current scope.");
   }
   return;

}  /* print_ln_by_name */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Given an entity name, this function prints the Attribute entry for    *|
|*      the entity on standard output.                                        *|
|*									      *|
|* Input parameters:							      *|
|*	The entity's name.                                                    *|
|*      NOTE:  This function currently only knows how to dump the attribute   *|
|*             entry for a simple name.  It will need to be upgraded to break *|
|*             down qualified names later.  The declaration for name_string   *|
|*             will have to be expanded, the qualified name will have to be   *|
|*             broken down, and srch_cpnt_name will have to be called to      *|
|*             find the attribute entry for the rightmost component name.     *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void print_at_by_name(void)

{
   int		attr_idx;
   int		ln_tbl_idx;
   char 	name_string[MAX_ID_LEN + 1];
   char		reply;

   printf("Entity name->");
#ifdef KEY
   fgets(name_string, MAX_ID_LEN, stdin);
#else /* KEY */
   gets(name_string);
#endif /* KEY */
   
   /* This is where the code should go to break down a qualified name.       */

   build_fake_token(name_string);
   attr_idx = srch_sym_tbl(TOKEN_STR(fake_token), TOKEN_LEN(fake_token),
                           &ln_tbl_idx);

   if (attr_idx != NULL_IDX) {
      dump_at_ntry(stderr, attr_idx, FALSE);
   }
   else {
      printf( 
           "\n*POSSIBLE FE90-ERROR* No such entity name in the local scope.\n");
      printf("Search host scope? (y) ");
      reply = getchar();

      if (reply == '\n'  ||  reply == 'y') {

         if (reply == 'y') {
            reply = getchar();    /* Get rid of newline char. */
         }
         attr_idx = srch_host_sym_tbl(TOKEN_STR(fake_token), 
                                      TOKEN_LEN(fake_token), 
                                      &ln_tbl_idx,
                                      TRUE);

         if (attr_idx != NULL_IDX) {
            dump_at_ntry(stderr, attr_idx, FALSE);
         }
         else {
            printf("\n*FE90-ERROR* No such entity name in the host either.\n");
         }
      }
      else {
         reply = getchar();       /* Get rid of newline char. */
      }
   }

   return;

}  /* print_at_by_name */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Given a common block or module name, this function prints the Storage *|
|*      Block Table entry for the specified common block or module to stderr. *|
|*									      *|
|* Input parameters:							      *|
|*	The name of the common block or module.                               *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void print_sb_by_name (void)

{
   int   		sb_idx;
   char 		name_string[MAX_ID_LEN + 1];

   printf("Enter common block or module name->");
#ifdef KEY
   fgets(name_string, MAX_ID_LEN, stdin);
#else /* KEY */
   gets(name_string);
#endif /* KEY */

   if (strlen(name_string) > 0) {
      build_fake_token(name_string);
   }
   else {
      build_fake_token("//");
   }

   sb_idx = srch_stor_blk_tbl(TOKEN_STR(fake_token),
                              TOKEN_LEN(fake_token),
                              curr_scp_idx);

   if (sb_idx != NULL_IDX) {
      dump_sb_ntry(stderr, sb_idx);
   }
   else {
      fprintf(stderr,"\n*FE90-ERROR* No such common block or module name.\n");
   }

   return;

}  /* print_sb_by_name */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints an attr list to stderr.		                              *|
|*									      *|
|* Input parameters:							      *|
|*	Index of attribute list entry to start printing with.                 *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void print_al_list(FILE		*out_file,
	  	   int		 al_idx)

{

   while (al_idx != NULL_IDX) {
      dump_al_ntry(out_file, al_idx);
      al_idx = AL_NEXT_IDX(al_idx);
   }

   return;

}  /* print_al_list */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints the Secondary Name table entries that belong to the indexed    *|
|*      entity to stderr.                                                     *|
|*									      *|
|* Input parameters:							      *|
|*	Index of Attribute entry that should have Secondary Name table        *|
|*        entries associated with it.                 			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void print_sn_list (int 	attr_idx)

{
   if (attr_idx > attr_tbl_idx) {
      fprintf(stderr, 
              "\n*FE90-ERROR* Attribute entry index [%d] is too large.\n",
              attr_idx);
    return;
   }
  
   /* Dummy arguments and keyword names for an explicit interface are in a  */
   /* in a contiguous grouping.  Procedure names in a generic or operator   */
   /* interface, namelist group members and component names are linked      */
   /* together (not necessarily contiguous).   			            */

   if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit  &&
       Pgm_Unknown < ATP_PGM_UNIT(attr_idx) < Program) {
      loop_thru_sn_ntries(stderr, attr_idx, FALSE);
   }
   else if (AT_OBJ_CLASS(attr_idx) == Interface ||
            AT_OBJ_CLASS(attr_idx) == Namelist_Grp ||
            AT_OBJ_CLASS(attr_idx) == Derived_Type) {
      chain_thru_sn_ntries(stderr, attr_idx, FALSE);
   }
   else {
      fprintf(stderr,
              "\n*FE90-ERROR* %s can not have Secondary Name table entries.\n",
              AT_OBJ_NAME_PTR(attr_idx));
   }
   return;

}  /* print_sn_list */
    
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints just include entries from the file path table.                 *|
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

void print_fp_includes (void)

{
   int		fp_idx;

   fp_idx = include_path_idx;

   while (fp_idx != NULL_IDX) {
      fprintf(debug_file, "%4s%-s\n", " ", FP_NAME_PTR(fp_idx));

      fp_idx = FP_NEXT_FILE_IDX(fp_idx);
   }

   fflush(debug_file);
       
   return;

}  /* print_fp_includes */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Prints just the name field of an Attr entry.  Useful when you know    *|
|*      the index of an Attr and you just want to know what identifier the    *|
|*      Attr is for.                                                          *|
|*                                                                            *|
|* Input parameters:							      *|
|*      idx : the Attr index                                                  *|
|*									      *|
|* Output parameters:							      *|
|*      NONE								      *|
|*									      *|
|* Returns:								      *|
|*      NOTHING								      *|
|*									      *|
\******************************************************************************/
void  print_name(int   idx)
{

   fprintf(stderr, "%d is %s\n", idx, AT_OBJ_NAME_PTR(idx));

   return;

} /* print_name */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Loops through a contiguous set of Secondary Name table entries and    *|
|*      writes them to stderr or a dump file.                                 *|
|*									      *|
|* Input parameters:							      *|
|*    - A pointer to the file to which the output is to be sent.              *|
|*    - Index of Attribute entry that should have Secondary Name table        *|
|*        entries associated with it.                 			      *|
|*    - A flag that indicates whether or not the Attribute entry for the      *|
|*        Secondary Name table item should be output.                         *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void loop_thru_sn_ntries (FILE		*out_file,
                                 int    	attr_idx,
				 boolean        output_attr)

{
   int          count;
   int          first_idx;
   int		i;


   if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {
      first_idx = ATP_FIRST_IDX(attr_idx);
      count     = ATP_NUM_DARGS(attr_idx);

      if (first_idx == NULL_IDX) {
         fprintf(out_file, "\n  %s\n",
                 "  ** No Dummy Arguments - ATP_FIRST_IDX = 0.");
         return;
      }
   }
   else {
      first_idx = ATP_FIRST_IDX(attr_idx);
      count     = ATP_NUM_DARGS(attr_idx);

      if (first_idx == NULL_IDX) {
         fprintf(out_file, "\n  %s\n",
                 "  ** No Dummy Arguments - ATP_FIRST_IDX = 0.");
         return;
      }
   }

   fprintf(out_file, "\n  %s %s:\n\n",
                     "Dummy Arguments for", AT_OBJ_NAME_PTR(attr_idx));

   for (i = first_idx;
        i < (first_idx + count);
        i++) {
 
      dump_sn_ntry(out_file, i);

      if (output_attr) {
         putc('\n', out_file);
         dump_at_ntry(out_file, SN_ATTR_IDX(i), FALSE);
      }
      else if (AT_OBJ_CLASS(SN_ATTR_IDX(i)) == Data_Obj &&
               ATD_CLASS(SN_ATTR_IDX(i)) == Dummy_Argument) {

         fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8d\n",
                "ATD_ARRAY_IDX", ATD_ARRAY_IDX(SN_ATTR_IDX(i)),
                "ATD_POINTER", boolean_str[ATD_POINTER(SN_ATTR_IDX(i))],
                "ATD_TYPE_IDX", ATD_TYPE_IDX(SN_ATTR_IDX(i)));

# ifdef _F_MINUS_MINUS
         fprintf(out_file, "  %-16s= %-7d \n",
                "ATD_PE_ARRAY_IDX", ATD_PE_ARRAY_IDX(SN_ATTR_IDX(i)));
# endif


         putc('\n', out_file);
      }
      else if (AT_OBJ_CLASS(SN_ATTR_IDX(i)) == Pgm_Unit &&
               ATP_PROC(SN_ATTR_IDX(i)) == Dummy_Proc) {
         fprintf(out_file, "  %-25s\n", "Dummy_Proc");
         putc('\n', out_file);
      }
      else {
         putc('\n', out_file);
      }
   }

   fflush(out_file);

   return;

}  /* loop_thru_sn_ntries */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Chains through a set of Secondary Name table entries using            *|
|*      SN_SIBLING_LINK and writes them to stderr or a dump file.             *|
|*									      *|
|* Input parameters:							      *|
|*    - A pointer to the file to which the output is to be sent.              *|
|*    - Index of Attribute entry that should have Secondary Name table        *|
|*        entries associated with it.                 			      *|
|*    - A flag that indicates whether or not the Attribute entry for the      *|
|*        Secondary Name table item should be output.                         *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void chain_thru_sn_ntries (FILE		*out_file,
         		          int	 	 attr_idx,
				  boolean        output_attr)
				 
{
   char		conv_str[80];
   int		first_idx;
   int		i;
   int		idx;
   char		str[80];



   if (AT_OBJ_CLASS(attr_idx) == Derived_Type) {
      first_idx = ATT_FIRST_CPNT_IDX(attr_idx);

      if (first_idx == NULL_IDX) {
         fprintf(out_file, "\n  %s\n",
                 "** No Secondary Name table entries - ATT_FIRST_CPNT_IDX = 0");
         return;
      }
      fprintf(out_file, "\n  %s %s:\n\n",
                        "Component entries for", AT_OBJ_NAME_PTR(attr_idx));
   }
   else if (AT_OBJ_CLASS(attr_idx) == Interface) {
      first_idx = ATI_FIRST_SPECIFIC_IDX(attr_idx);

      if (first_idx == NULL_IDX) {
         fprintf(out_file, "\n  %s\n",
             "** No Secondary Name table entries - ATI_FIRST_SPECIFIC_IDX = 0");
         return;
      }
      fprintf(out_file, "\n  %s %s:\n\n",
                        "Interface bodies for", AT_OBJ_NAME_PTR(attr_idx));
   }
   else if (AT_OBJ_CLASS(attr_idx) == Namelist_Grp) {
      first_idx = ATN_FIRST_NAMELIST_IDX(attr_idx);

      if (first_idx == NULL_IDX) {
         fprintf(out_file, "\n  %s\n",
            " ** No Secondary Name table entries - ATN_FIRST_NAMELIST_IDX = 0");
         return;
      }
      fprintf(out_file, "\n  %s %s:\n\n",
                        "Namelist objects for", AT_OBJ_NAME_PTR(attr_idx));
   }
   else {
      fprintf(out_file, "\n  %s %s:\n\n",
                        "Invalid attribute entry ", AT_OBJ_NAME_PTR(attr_idx));
      return;
   }

   for (i = first_idx;  i != NULL_IDX;  i = SN_SIBLING_LINK(i)) {
      dump_sn_ntry(out_file, i);

      if (output_attr) {
         putc('\n', out_file);
         dump_at_ntry(out_file, SN_ATTR_IDX(i), FALSE);

      }
      else if (AT_OBJ_CLASS(attr_idx) == Interface &&
               AT_OBJ_CLASS(SN_ATTR_IDX(i)) == Pgm_Unit) {

         fprintf(out_file, "  %-25s %-16s= %-7d %-16s= %-8d\n",
                atp_pgm_unit_str[ATP_PGM_UNIT(SN_ATTR_IDX(i))],
                "ATP_FIRST_IDX", ATP_FIRST_IDX(SN_ATTR_IDX(i)),
                "ATP_NUM_DARGS", ATP_NUM_DARGS(SN_ATTR_IDX(i)));

         if (ATP_PGM_UNIT(SN_ATTR_IDX(i)) == Function) {
            idx = ATP_RSLT_IDX(SN_ATTR_IDX(i));
            fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8d\n",
                   "ATD_ARRAY_IDX", ATD_ARRAY_IDX(idx),
                   "ATD_POINTER", boolean_str[ATD_POINTER(idx)],
                   "ATD_TYPE_IDX", ATD_TYPE_IDX(idx));
         }

         if (ATP_FIRST_IDX(SN_ATTR_IDX(i)) != NULL_IDX) {
            loop_thru_sn_ntries(out_file, SN_ATTR_IDX(i), FALSE);
         }

         putc('\n', out_file);
      }
      else if (AT_OBJ_CLASS(SN_ATTR_IDX(i)) == Data_Obj &&
               ATD_CLASS(SN_ATTR_IDX(i)) == Struct_Component) {

         if (ATD_OFFSET_FLD(SN_ATTR_IDX(i)) == CN_Tbl_Idx ||
                ATD_OFFSET_FLD(SN_ATTR_IDX(i)) == NO_Tbl_Idx) {
            sprintf(str, "(%10s)", convert_to_string(
                         &CN_CONST(ATD_CPNT_OFFSET_IDX(SN_ATTR_IDX(i))),
                         CN_TYPE_IDX(ATD_CPNT_OFFSET_IDX(SN_ATTR_IDX(i))),
                         conv_str));
         }
         else if (ATD_OFFSET_FLD(SN_ATTR_IDX(i)) == AT_Tbl_Idx) {
            sprintf(str, "(%10s)", 
                         AT_OBJ_NAME_PTR(ATD_CPNT_OFFSET_IDX(SN_ATTR_IDX(i))));
         }
         else {
            sprintf(str,"%12s", " ");
         }


         fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-7s\n",
                 "ATD_ALIGNMENT", align_str[ATD_ALIGNMENT(SN_ATTR_IDX(i))],
                "ATD_ARRAY_IDX", ATD_ARRAY_IDX(SN_ATTR_IDX(i)),
                "AT_DCL_ERR", boolean_str[AT_DCL_ERR(SN_ATTR_IDX(i))]);

         print_fld_idx(out_file, "ATD_CPNT_OFFSET_",
                       ATD_OFFSET_FLD(SN_ATTR_IDX(i)),
                       ATD_CPNT_OFFSET_IDX(SN_ATTR_IDX(i)));

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s \n",
                "ATD_IM_A_DOPE", boolean_str[ATD_IM_A_DOPE(SN_ATTR_IDX(i))],
                "ATD_POINTER", boolean_str[ATD_IM_A_DOPE(SN_ATTR_IDX(i))]);

         fprintf(out_file, "  %-16s= %-7d %-s\n",
                     "ATD_TYPE_IDX", ATD_TYPE_IDX(SN_ATTR_IDX(i)),
                      print_type_f(ATD_TYPE_IDX(SN_ATTR_IDX(i))));

         if (ATD_CPNT_INIT_IDX(SN_ATTR_IDX(i)) != NULL_IDX) {
            print_fld_idx(out_file, "ATD_CPNT_INIT_ID",
                          (fld_type) ATD_FLD(SN_ATTR_IDX(i)),
                          ATD_CPNT_INIT_IDX(SN_ATTR_IDX(i)));

            if (ATD_FLD(SN_ATTR_IDX(i)) == IR_Tbl_Idx) {
               dump_ir_ntry(out_file, ATD_CPNT_INIT_IDX(SN_ATTR_IDX(i)), 5);
            }
            else if (ATD_FLD(SN_ATTR_IDX(i)) == CN_Tbl_Idx) {
               dump_cn_ntry(out_file, ATD_CPNT_INIT_IDX(SN_ATTR_IDX(i)));
            }
         }


         putc('\n', out_file);
      }
   }

   fflush(out_file);

   return;

}  /* chain_thru_sn_ntries */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This function builds a fake token which may be used as an actual      *|
|*      argument to a symbol table search function.  This function is         *|
|*      typically called by the routines that look up items in the symbol     *|
|*      table by their names (rather than by an index into a table).          *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      A string containing the name of the item to be found.                 *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
|* Note:  This function builds the fake token in the file-global structure    *|
|*        fake_token.                                                         *|
|*                                                                            *|
\******************************************************************************/

static void build_fake_token (char     *name_string)

{
   int		i;
   int		len;


   /* Initialize the token string to all zeroes so the long word search  */
   /* used by the search routines will work correctly.                   */

   len	= strlen(name_string);
   CREATE_ID(TOKEN_ID(fake_token), name_string, len);

   TOKEN_LEN(fake_token) = len;

   for (i = 0;  i < len;  i++) {
      TOKEN_STR(fake_token)[i] = toupper(TOKEN_STR(fake_token)[i]);
   }

   return;

}  /* build_fake_token */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Used to trace entry and exit into a function.			      *|
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

void dump_func_trace_info (trace_type	 trace,
			   char		*func_name,
			   char		*info)
{

   if (trace_file == NULL) {
      trace_file = fopen (trace_file_name, "w");
      
      if (trace_file == NULL) {
         PRINTMSG(1, 17, Error, 0, trace_file_name);
	 exit_compiler(RC_USER_ERROR);
      }

      fprintf (trace_file, "\nTRACE DUMP OF PROGRAM %s:\n\n", src_file);
   }

   dump_trace_info(trace_file, trace, func_name, info);

   return;

}  /* dump_func_trace_info */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Used to output a variety of information to the screen, the dump file, *|
|*      or the trace file.						      *|
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

static void dump_trace_info (FILE	  *out_file,
			     trace_type    trace,
      		             char         *func_name,
                      	     char         *info)
{

   int	 idx;
   char *indent_str;


   switch (trace) {

      case Func_Entry:
      case Func_Exit:
	 if (trace == Func_Exit) { 
	    trace_indent -= trace_indent_len;

	    if (trace_indent < 0) {
	       trace_indent = 0;
	    }
	 }

	 indent_str = (char *) malloc (trace_indent+1);

	 for (idx = 0;	idx < trace_indent;  idx++) {
	    indent_str[idx] = (idx % trace_indent_len  ==  0)  ?  '|'  :  BLANK;
	 }

	 indent_str[idx] = NULL_CHAR;

	 if (trace == Func_Entry) { 
	    fprintf (out_file, "%sIN  %s", indent_str, func_name);
	    trace_indent += trace_indent_len;
	 } 
	 else { 
	    fprintf (out_file, "%sOUT %s", indent_str, func_name);
	 }
	 if (info == NULL) {
	    putc (NEWLINE, out_file);
	 }
	 else {
	    fprintf (out_file, " (%s)\n", info);
	 }

	 free (indent_str);
         indent_str = NULL;
	 break;

      case Syntax_Pass:
         fprintf(out_file, "\n> > > > > > > > > >   B e g i n   S y n t a x "
                           "  P a s s   < < < < < < < < < <\n\n");
         trace_indent = 0;
         break;

      case Semantics_Pass:
         fprintf(out_file, "\n> > > > > > > >   B e g i n   S e m a n t i c s"
                           "   P a s s   < < < < < < < <\n\n");
         trace_indent = 0;
         break;

      case PU_Start:
         if (info == NULL) {
	    fprintf (out_file, "\n\n# NEW PROGRAM UNIT ########################"
	   		       "####################################\n");
    	 }
         else {
            fprintf (out_file, "\n\n# NEW PROGRAM UNIT # %s ##################"
			       "#######\n",
			       AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));

            if (SCP_IN_ERR(curr_scp_idx)) {
               fprintf (out_file,
                        "# # #  SCP entry is marked in error  # # #\n");
            }
         }

	 break;

      case Stmt_Start:
         if (info == NULL) {
	    fprintf (out_file, "\n- NEW STMT ---------------------------"
       		               "-----------------------------------------\n");
         }
         else {
            fprintf (out_file, "\n- %s%s - %d %s-------------------------"
                               "-------------------------\n",
                               (SH_COMPILER_GEN(curr_stmt_sh_idx)) ? "CG " : "",
			       stmt_type_str[SH_STMT_TYPE(curr_stmt_sh_idx)],
			       SH_GLB_LINE(curr_stmt_sh_idx),
                               (SH_P2_SKIP_ME(curr_stmt_sh_idx)) ? "SKIP ME " :
                                                                   "--------");

            if (SH_ERR_FLG(curr_stmt_sh_idx)) {
               fprintf (out_file,
                        "\n* * *  Stmt Header is marked in error  * * *\n");
            }
         }

	 break;

   }  /* switch */

   return;

}  /* dump_trace_info */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Used to trace memory usage.                 			      *|
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

void dump_mem_trace_info (trace_type	 trace,
			  char		*struct_name,
			  void		*new_struct_ptr,
			  void		*old_struct_ptr,
			  long		 struct_bsize_or_num_used,
			  int		 num_entries)
{
   int	 idx;
   char *indent_str	= NULL;


   if (trace_file == NULL) {
      trace_file = fopen (trace_file_name, "w");
      
      if (trace_file == NULL) {
         PRINTMSG(1, 17, Error, 0, trace_file_name);
	 exit_compiler(RC_USER_ERROR);
      }

      fprintf (trace_file, "\nTRACE DUMP OF PROGRAM %s:\n\n", src_file);
   }

   if (trace_indent > 0) {
      indent_str = (char *) malloc (trace_indent+1);

      for (idx = 0;  idx < trace_indent;  idx++) {
	 indent_str[idx] = (idx % trace_indent_len  ==	0)  ?  '|'  :  BLANK;
      }

      indent_str[idx] = NULL_CHAR;

      fprintf (trace_file, "%s", indent_str);
   }

   switch (trace) {
      case Mem_Alloc:
	 fprintf (trace_file, "ALLOC           %s (%#o) BSIZE=%ld(%d ENTRIES)\n",
			      struct_name, (uint) new_struct_ptr,
			      struct_bsize_or_num_used, num_entries);
	 break;

      case Mem_Realloc:
	 if (new_struct_ptr == old_struct_ptr) {
	    fprintf (trace_file, "REALLOC      %s (%#o) BSIZE=%ld(%d ENTRIES)\n",
				 struct_name, (uint) new_struct_ptr,
				 struct_bsize_or_num_used, num_entries);
	 }
	 else {			/* realloced with move */
	    fprintf (trace_file, "REALLOC/MOVE %s (%#o->%#o) "
				 "BSIZE=%ld(%d ENTRIES)\n",
				 struct_name, (uint) old_struct_ptr, (uint) new_struct_ptr,
				 struct_bsize_or_num_used, num_entries);
	 }
	 break;

      case Mem_Free:
	 fprintf (trace_file, "FREE         %s (%#o) (%d ENTRIES) "
                              "(%ld USED ENTRIES)\n", 
                              struct_name,
                              (uint) new_struct_ptr,
                              num_entries,
                              struct_bsize_or_num_used);
	 break;

      case Mem_Compress:
	 fprintf (trace_file, "COMPRESS     %s (%ld BEFORE ENTRIES) "
                              "(%d AFTER ENTRIES)\n", 
                              struct_name,
                              struct_bsize_or_num_used,
                              num_entries);
	 break;
   }  /* switch */

   if (indent_str != NULL) {
      free (indent_str);
      indent_str = NULL;
   }

   return;

}  /* dump_mem_trace_info */

/******************************************************************************\
|*									      *|
|* Description:								      *|
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

void	print_mem_usage_report(char	*name,
				int	 final_size,
				int	 largest_idx)
 
{

   static	boolean		first_call	= TRUE;

   print_tbl_header("Memory Report");

   if (first_call == TRUE) {
      first_call = FALSE;

      /* Dump initial size and increment for each table. */

      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "attr_list_tbl",
                           "init size", attr_list_tbl_init_size,
                           "increment", attr_list_tbl_inc,
                           "num words", attr_list_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "attr_tbl",
                           "init size", attr_tbl_init_size,
                           "increment", attr_tbl_inc,
                           "num words", attr_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "blk_stk", 
                           "init size", blk_stk_init_size,
                           "increment", blk_stk_inc,
                           "num words", blk_stk_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "bounds_tbl",
                           "init size", bounds_tbl_init_size,
                           "increment", bounds_tbl_inc,
                           "num words", bounds_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "const_tbl",
                           "init size", const_tbl_init_size,
                           "increment", const_tbl_inc,
                           "num words", const_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "const_pool",
                           "init size", const_pool_init_size,
                           "increment", const_pool_inc,
                           "num words", const_pool_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "equiv_tbl",
                           "init size", equiv_tbl_init_size,
                           "increment", equiv_tbl_inc,
                           "num words", equiv_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "file_path_tbl",
                           "init size", file_path_tbl_init_size,
                           "increment", file_path_tbl_inc,
                           "num words", file_path_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "global_line_tbl",
                           "init size", global_line_tbl_init_size,
                           "increment", global_line_tbl_inc,
                           "num words", global_line_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "global_name_tbl",
                           "init size", global_name_tbl_init_size,
                           "increment", global_name_tbl_inc,
                           "num words", global_name_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "hidden_name_tbl",
                           "init size", hidden_name_tbl_init_size,
                           "increment", hidden_name_tbl_inc,
                           "num words", hidden_name_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "ir_tbl",
                           "init size", ir_tbl_init_size,
                           "increment", ir_tbl_inc,
                           "num words", ir_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "ir_list_tbl",
                           "init size", ir_list_tbl_init_size,
                           "increment", ir_list_tbl_inc,
                           "num words", ir_list_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "loc_name_tbl",
                           "init size", loc_name_tbl_init_size,
                           "increment", loc_name_tbl_inc,
                           "num words", loc_name_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "mod_link_tbl",
                           "init size", mod_link_tbl_init_size,
                           "increment", mod_link_tbl_inc,
                           "num words", mod_link_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "name_pool",
                           "init size", name_pool_init_size,
                           "increment", name_pool_inc,
                           "num words", name_pool_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "rename_only_tbl",
                           "init size", rename_only_tbl_init_size,
                           "increment", rename_only_tbl_inc,
                           "num words", rename_only_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "scp_tbl",
                           "init size", scp_tbl_init_size,
                           "increment", scp_tbl_inc,
                           "num words", scp_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "sec_name_tbl",
                           "init size", sec_name_tbl_init_size,
                           "increment", sec_name_tbl_inc,
                           "num words", sec_name_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "sh_tbl",
                           "init size", sh_tbl_init_size,
                           "increment", sh_tbl_inc,
                           "num words", sh_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "stor_blk_tbl",
                           "init size", stor_blk_tbl_init_size,
                           "increment", stor_blk_tbl_inc,
                           "num words", stor_blk_tbl_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "str_pool",
                           "init size", str_pool_init_size,
                           "increment", str_pool_inc,
                           "num words", str_pool_num_wds);
      fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d  %-9s= %-7d\n",
                           "type_tbl",
                           "init size", type_tbl_init_size,
                           "increment", type_tbl_inc,
                           "num words", type_tbl_num_wds);
      print_src_input_tbls();
   }


   fprintf (debug_file, "%-20s  %-9s= %-8d  %-9s= %-6d\n",
                        name,
                        "finalsize", final_size,
                        "large idx", largest_idx);

   fflush (debug_file);
   return;

}  /* print_mem_usage_report */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Print the defines that are set in this compiler.                      *|
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

void	print_defines(void)

{

   print_tbl_header("Compiler Defines Dump");

# ifdef _PVP_PVP
   fprintf(debug_file, "\t\t\t_PVP_PVP\n");
# endif

# ifdef _MPP_MPP
   fprintf(debug_file, "\t\t\t_MPP_MPP\n");
# endif

# ifdef _SOLARIS_SOLARIS
   fprintf(debug_file, "\t\t\t_SOLARIS_SOLARIS\n");
# endif

# ifdef _SOLARIS_MPP
   fprintf(debug_file, "\t\t\t_SOLARIS_MPP\n");
# endif

# ifdef _SOLARIS_PVP
   fprintf(debug_file, "\t\t\t_SOLARIS_PVP\n");
# endif

# ifdef _PVP_MPP
   fprintf(debug_file, "\t\t\t_PVP_MPP\n");
# endif

   fprintf(debug_file, "\n");

# ifdef _PDGCS
   fprintf(debug_file, "\t\t\t_PDGCS\n");
# endif

# ifdef _HOST32
   fprintf(debug_file, "\t\t\t_HOST32\n");
# endif

# ifdef _HOST64
   fprintf(debug_file, "\t\t\t_HOST64\n");
# endif

# ifdef _TARGET32
   fprintf(debug_file, "\t\t\t_TARGET32\n");
# endif

# ifdef _TARGET64
   fprintf(debug_file, "\t\t\t_TARGET64\n");
# endif

# if defined(_HOST_OS_IRIX)
   fprintf(debug_file, "\t\t\t_HOST_OS_IRIX\n");
# endif

# if defined(_HOST_OS_LINUX)
   fprintf(debug_file, "\t\t\t_HOST_OS_LINUX\n");
# endif

# if defined(_HOST_OS_DARWIN)
   fprintf(debug_file, "\t\t\t_HOST_OS_DARWIN\n");
# endif
# ifdef _HOST_OS_MAX
   fprintf(debug_file, "\t\t\t_HOST_OS_MAX\n");
# endif

# ifdef _HOST_OS_SOLARIS
   fprintf(debug_file, "\t\t\t_HOST_OS_SOLARIS\n");
# endif

# ifdef _HOST_OS_UNICOS
   fprintf(debug_file, "\t\t\t_HOST_OS_UNICOS\n");
# endif


# if defined(_TARGET_OS_IRIX)
   fprintf(debug_file, "\t\t\t_TARGET_OS_IRIX\n");
# endif

# if defined(_TARGET_OS_LINUX)
   fprintf(debug_file, "\t\t\t_TARGET_OS_LINUX\n");
# endif

# if defined(_TARGET_OS_DARWIN)
   fprintf(debug_file, "\t\t\t_TARGET_OS_DARWIN\n");
# endif

# ifdef _TARGET_OS_MAX
   fprintf(debug_file, "\t\t\t_TARGET_OS_MAX\n");
# endif

# ifdef _TARGET_OS_SOLARIS
   fprintf(debug_file, "\t\t\t_TARGET_OS_SOLARIS\n");
# endif

# ifdef _TARGET_OS_UNICOS
   fprintf(debug_file, "\t\t\t_TARGET_OS_UNICOS\n");
# endif

# ifdef _TARGET_SV2
   fprintf(debug_file, "\t\t\t_TARGET_SV2\n");
# endif

# ifdef _TARGET_IEEE
   fprintf(debug_file, "\t\t\t_TARGET_IEEE\n");
# endif

# ifdef _TARGET_BYTE_ADDRESS
   fprintf(debug_file, "\t\t\t_TARGET_BYTE_ADDRESS\n");
# endif

# ifdef _TARGET_WORD_ADDRESS
   fprintf(debug_file, "\t\t\t_TARGET_WORD_ADDRESS\n");
# endif

# ifdef _HEAP_REQUEST_IN_BYTES
   fprintf(debug_file, "\t\t\t_HEAP_REQUEST_IN_BYTES\n");
# endif

# ifdef _HEAP_REQUEST_IN_WORDS
   fprintf(debug_file, "\t\t\t_HEAP_REQUEST_IN_WORDS\n");
# endif

# ifdef _MODULE_DOT_TO_o
   fprintf(debug_file, "\t\t\t_MODULE_DOT_TO_o\n");
# endif

# ifdef _MODULE_DOT_TO_M
   fprintf(debug_file, "\t\t\t_MODULE_DOT_TO_M\n");
# endif

# ifdef _MODULE_DOT_TO_mod
   fprintf(debug_file, "\t\t\t_MODULE_DOT_TO_mod\n");
# endif

# ifdef _ARITH_INPUT_CONV
   fprintf(debug_file, "\t\t\t_ARITH_INPUT_CONV\n");
# endif

# ifdef _ARITH_H
   fprintf(debug_file, "\t\t\t_ARITH_H\n");
# endif

# ifdef _ALLOCATE_IS_CALL
   fprintf(debug_file, "\t\t\t_ALLOCATE_IS_CALL\n");
# endif

# ifdef _SEPARATE_FUNCTION_RETURNS
   fprintf(debug_file, "\t\t\t_SEPARATE_FUNCTION_RETURNS\n");
# endif

# ifdef _TARGET_DOUBLE_ALIGN
   fprintf(debug_file, "\t\t\t_TARGET_DOUBLE_ALIGN\n");
# endif

# ifdef _ERROR_DUPLICATE_GLOBALS
   fprintf(debug_file, "\t\t\t_ERROR_DUPLICATE_GLOBALS\n");
# endif


   if (char_len_in_bytes) {
      fprintf(debug_file, "\t\t\t_CHAR_LEN_IN_BYTES\n");
   }
# ifdef _NO_BINARY_OUTPUT
   fprintf(debug_file, "\t\t\t_NO_BINARY_OUTPUT\n");
# endif

# ifdef _CHECK_MAX_MEMORY
   fprintf(debug_file, "\t\t\t_CHECK_MAX_MEMORY\n");
# endif

# ifdef _TASK_COMMON_EXTENSION
   fprintf(debug_file, "\t\t\t_TASK_COMMON_EXTENSION\n");
# endif

# ifdef _TWO_WORD_FCD
   fprintf(debug_file, "\t\t\t_TWO_WORD_FCD\n");
# endif

# ifdef _TRANSFORM_CHAR_SEQUENCE
   fprintf(debug_file, "\t\t\t_TRANSFORM_CHAR_SEQUENCE\n");
# endif

# ifdef _TMP_GIVES_COMMON_LENGTH
   fprintf(debug_file, "\t\t\t_TMP_GIVES_COMMON_LENGTH\n");
# endif

# ifdef _SPLIT_STATIC_STORAGE_2
   fprintf(debug_file, "\t\t\t_SPLIT_STATIC_STORAGE_2\n");
# endif

# ifdef _SPLIT_STATIC_STORAGE_3
   fprintf(debug_file, "\t\t\t_SPLIT_STATIC_STORAGE_3\n");
# endif

# ifdef _ALLOW_DATA_INIT_OF_COMMON
   fprintf(debug_file, "\t\t\t_ALLOW_DATA_INIT_OF_COMMON\n");
# endif

# ifdef _FRONTEND_CONDITIONAL_COMP
   fprintf(debug_file, "\t\t\t_FRONTEND_CONDITIONAL_COMP\n");
# endif


   fprintf(debug_file, "\n\n\t\t\tINTEGER_DEFAULT_TYPE\t%s\n",
                       lin_type_str[INTEGER_DEFAULT_TYPE]);

   fprintf(debug_file, "\t\t\tREAL_DEFAULT_TYPE\t%s\n",
                       lin_type_str[REAL_DEFAULT_TYPE]);

   fprintf(debug_file, "\t\t\tDOUBLE_DEFAULT_TYPE\t%s\n",
                       lin_type_str[DOUBLE_DEFAULT_TYPE]);

   fprintf(debug_file, "\t\t\tCOMPLEX_DEFAULT_TYPE\t%s\n",
                       lin_type_str[COMPLEX_DEFAULT_TYPE]);

   fprintf(debug_file, "\t\t\tLOGICAL_DEFAULT_TYPE\t%s\n",
                       lin_type_str[LOGICAL_DEFAULT_TYPE]);

   fprintf(debug_file, "\t\t\tTRUE_VALUE  = %d\n", TRUE_VALUE);
   fprintf(debug_file, "\t\t\tFALSE_VALUE = %d\n", FALSE_VALUE);

   if (target_triton) {
      fprintf(debug_file, "\n\t\t\ttarget_triton\n");
   }

   if (target_ieee) {
      fprintf(debug_file, "\n\t\t\ttarget_ieee\n");
   }

   if (char_len_in_bytes) {
      fprintf(debug_file, "\n\t\t\tchar_len_in_bytes\n");
   }

   putc ('\n', debug_file);
   fflush(debug_file);

   return;

}  /* print_defines */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
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

static void dump_dv(FILE                *out_file,
                    int_dope_type       *dv,
                    boolean             dump_it)

{
   long *lptr;
   int  k;
   int  i;
   int  idx;
   int  dec_len = 0;
   int  dp_flag = 0;
   int  dv_type;
   int  int_len = 0;
   int  kind_star = 0;
   int  type_idx;
   int  num_chars;
   char *char_ptr;



   if (dv == NULL) {
      fprintf(out_file, "\nDOPE VECTOR ADDRESS IS NULL\n\n");
      return;
   }
#if defined(_HOST32) && defined(_TARGET64)
   fprintf(out_file, "base_addr = 0x%x\n", dv->base_addr);
   fprintf(out_file, "el_len    = %d\n", dv->el_len);
#else
   fprintf(out_file, "base_addr = 0x%" LONG_TYPE_X_FMT "\n", dv->base_addr);
   fprintf(out_file, "el_len    = %" LONG_TYPE_FMT "\n", dv->el_len);
#endif
   fprintf(out_file, "assoc     = %d\n", dv->assoc);
   fprintf(out_file, "ptr_alloc = %d\n", dv->ptr_alloc);
   fprintf(out_file, "p_or_a    = %s\n", (dv->p_or_a == 2 ? "ALLOCATABLE" :
                                  (dv->p_or_a == 1 ? "POINTER" : "OTHER")));
   fprintf(out_file, "a_contig  = %s\n",
                          (dv->a_contig == 1 ? "TRUE" : "FALSE"));
   fprintf(out_file, "unused_1  = %d\n", dv->unused_1);
# if defined(_TARGET64)
   fprintf(out_file, "unused_2  = %d\n", dv->unused_2);
# endif
   fprintf(out_file, "num_dims  = %d\n", dv->num_dims);


#if defined(_HOST32) && defined(_TARGET64)
   fprintf(out_file, "orig_base = 0x%x\n", dv->orig_base);
   fprintf(out_file, "orig_size = %d\n", dv->orig_size);

   for(k = 0; k < (int)(dv->num_dims); k++) {
      fprintf(out_file, "low_bound[%d]   = %d\n",k+1,
                                          dv->dim[k].low_bound);
      fprintf(out_file, "extent[%d]      = %d\n",
                                            k+1, dv->dim[k].extent);
      fprintf(out_file, "stride_mult[%d] = %d\n\n",k+1,
                                          dv->dim[k].stride_mult);
   }
#else
   fprintf(out_file, "orig_base = 0x%" LONG_TYPE_X_FMT "\n", dv->orig_base);
   fprintf(out_file, "orig_size = %" LONG_TYPE_FMT "\n", dv->orig_size);

   for(k = 0; k < (int)(dv->num_dims); k++) {
      fprintf(out_file, "low_bound[%d]   = %" LONG_TYPE_FMT "\n",k+1,
                                          dv->dim[k].low_bound);
      fprintf(out_file, "extent[%d]      = %" LONG_TYPE_FMT "\n",
                                            k+1, dv->dim[k].extent);
      fprintf(out_file, "stride_mult[%d] = %" LONG_TYPE_FMT "\n\n",k+1,
                                          dv->dim[k].stride_mult);
   }
#endif



   return;

}  /* dump_dv */

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

void print_so (size_offset_type so)

{
   char		str[80];

   switch(so.fld) {

   case NO_Tbl_Idx:
      print_fld_idx(stderr, "Idx", (fld_type) so.fld, 0);
      fprintf(stderr, "Type = (%d)  %s",  so.type_idx, 
                                          print_type_f(so.type_idx));

      switch (TYP_TYPE(so.type_idx)) {
      case Typeless:
         convert_to_string_fmt = Hex_Fmt;
         fprintf(stderr,"0x%s", 
                 convert_to_string((long_type *)&(so.constant), 
                                     so.type_idx, str));

         if (TYP_BIT_LEN(so.type_idx) > TARGET_BITS_PER_WORD) {
            convert_to_string_fmt = Hex_Fmt;
            fprintf(stderr, "  %s",
                    convert_to_string((long_type *)&(so.constant[1]),
                                      so.type_idx, str));
         }

         break;

      case Integer:
         fprintf(stderr,"%s",
                 convert_to_string((long_type *)&(so.constant), 
                                    so.type_idx, str));
         break;
   
      case Real:
         fprintf(stderr, "%s",
                 convert_to_string((long_type *)&(so.constant), 
                                    so.type_idx, str));
         break;
         
      case Character:
         break;
         
      case Logical:
         fprintf(stderr, "%s",
                (THIS_IS_TRUE((long_type *)&(so.constant), so.type_idx) ?
                                             ".TRUE." : ".FALSE."));
         break;
         
      case Complex:
         fprintf(stderr, "%s", 
                 convert_to_string((long_type *)&(so.constant),
                                   so.type_idx, str));
         break;
      }

      fprintf(stderr,"\n");
      break;

   case CN_Tbl_Idx:
      print_fld_idx(stderr, "Idx", (fld_type) so.fld, so.idx);
      fprintf(stderr, "Constant = *Unset*\n");

      print_const_entry(stderr, so.idx, 0);
      break;

   case AT_Tbl_Idx:
      print_fld_idx(stderr, "Idx", (fld_type) so.fld, so.idx);
      fprintf(stderr, "Constant = *Unset*\n");
      print_attr_name(stderr, so.idx, 0);
      break;

   case IR_Tbl_Idx:
      print_fld_idx(stderr, "Idx", (fld_type) so.fld, so.idx);
      fprintf(stderr, "Constant = *Unset*\n");
      dump_ir_ntry(stderr, so.idx, 0);
      break;
   }

   return;

}  /* print_so */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Makes a field/index printable into a character string.                *|
|*									      *|
|* Input parameters:							      *|
|*	fld  ->  Field describing index to print.                             *|
|*	idx  ->  Index to print.                                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	Pointer to a character string of item to print.			      *|
|*									      *|
|*	NOTE:  This reuses the same output character area, so each successive *|
|*	       call will clear out the previous returned value.               *|
|*									      *|
\******************************************************************************/

static void	print_fld_idx(FILE		*out_file,
			      char		*name,
			      fld_type		 fld,
			      int		 idx)

{
   static	char		str[80];
   		char		conv_str[80];

   if (idx == NULL_IDX) {
      fprintf(out_file, "  %-16s= %-7d %-9s\n", name, idx, field_str[fld]);
   }
   else {
      switch (fld) {

      case CN_Tbl_Idx:
         sprintf(str,"( %-s )",
                 convert_to_string(&CN_CONST(idx), CN_TYPE_IDX(idx), conv_str));
         break;

      case AT_Tbl_Idx:
         sprintf(str,"( %-s )", AT_OBJ_NAME_PTR(idx));
         break;

      case IR_Tbl_Idx:
      case IL_Tbl_Idx:
      case SH_Tbl_Idx:
      case SB_Tbl_Idx:
         sprintf(str,"%s", " ");
         break;

      default:
         sprintf(str,"%s", " ");
         break;
      }

      fprintf(out_file, "  %-16s= %-7d %-9s%15s %-s\n",
             name, idx,
             field_str[fld], " ",
             str);
   }

   return;

}  /* print_fld_idx */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Should be inlined.  Returns the name for an attr or ** NONE **        *|
|*									      *|
|* Input parameters:							      *|
|*	idx  ->  Attr index of name                                           *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	Pointer to a character string of item to print.			      *|
|*									      *|
|*	NOTE:  This reuses the same output character area, so each successive *|
|*	       call will clear out the previous returned value.               *|
|*									      *|
\******************************************************************************/

static char *print_at_name(int		idx)

{
   static	char	str[1] = "0";


   if (idx == NULL_IDX) {
      return(str);
   }
   else {
      return(AT_OBJ_NAME_PTR(idx));
   }

}  /* print_at_name */
    
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints header for a table in the debug output.                        *|
|*									      *|
|* Input parameters:							      *|
|*	Table name to print						      *|
|*									      *|
\******************************************************************************/

static	void print_tbl_header (char	*table_name)

{
   init_debug_file();

   fprintf(debug_file, "****************************************"
                       "****************************************\n");
   fprintf(debug_file, "\n\t\t\t%s\n\n", table_name);
   fprintf(debug_file, "****************************************"
                       "****************************************\n");
   return;

}  /* print_tbl_header */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Print global type in a Fortran format.                                *|
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

static	char  *print_global_type_f(int	 gt_idx)

{
		int	kind;
   static	char	str[80];
  	 	char	str1[80];


   if (gt_idx == NULL_IDX) {
      sprintf(str, "NULL");
   }
   else if (GT_TYPE(gt_idx) <= Last_Linear_Type) {

      if (GT_DESC(gt_idx) == Star_Typed) {
         sprintf(str, "%s * %d", 
                       basic_type_str[GT_TYPE(gt_idx)],
                       GT_DCL_VALUE(gt_idx));
      }
      else if (GT_DESC(gt_idx) == Kind_Typed) {
         sprintf(str, "%s (kind=%d)", 
                       basic_type_str[GT_TYPE(gt_idx)],
                       GT_DCL_VALUE(gt_idx));
      }
      else {  /* Default Typed */

         /* Print a kind type, so we know exactly what we've got. */

         switch (GT_LINEAR_TYPE(gt_idx)) {
         case Integer_1:
         case Logical_1:
            kind	= 1;
            break;
         case Integer_2:
         case Logical_2:
            kind	= 2;
            break;
         case Integer_4:
         case Logical_4:
         case Real_4:
         case Complex_4:
            kind	= 4;
            break;
         case Integer_8:
         case Logical_8:
         case Real_8:
         case Complex_8:
            kind	= 8;
            break;
         case Real_16:
         case Complex_16:
            kind	= 16;
            break;
         default:
            kind	= 0;
            break;
         }

         if (kind == 0) {
            sprintf(str, "%s", basic_type_str[GT_TYPE(gt_idx)]);
         }
         else {
            sprintf(str, "%s (%d)", basic_type_str[GT_TYPE(gt_idx)], kind);
         }
      }
   }
   else if (GT_TYPE(gt_idx) == Typeless) {
      sprintf(str, "Typeless * %s", 
                    CONVERT_CVAL_TO_STR(&TYP_BIT_LEN(gt_idx), Integer_8, str1));
   }
   else if (GT_TYPE(gt_idx) != Character) {
      sprintf(str, "type(%s)", GA_OBJ_NAME_PTR(GT_STRUCT_IDX(gt_idx)));
   }
   else if (GT_CHAR_CLASS(gt_idx) == Assumed_Size_Char) {
      sprintf(str, "CHARACTER*(*)");
   }
   else if (GT_CHAR_CLASS(gt_idx) == Const_Len_Char) {
      sprintf(str, "CHARACTER*(Const_Len_Char)");
      /* 07Dec00[sos] next line aborts  (PV 802511)                       */
      /* sprintf(str, "CHARACTER*(%s)",                                   */
      /*         convert_to_string(GT_LENGTH(gt_idx), Integer_8, str1)"); */
   }
   else {  /* Variable or unknown length char - print (tmp_idx = idx) */
      sprintf(str, "CHARACTER*(tmp)");
   }

   return(str);

} /* print_global_type_f */

/******************************************************************************\
|*                                                                            *|
|*      Utility routines specifically for the IR/IL dump.                     *|
|*                                                                            *|
\******************************************************************************/

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Print all the statements and all their IR.                            *|
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

static void  print_all_text (boolean	print_all_scps)

{
   int  save_curr_scp_idx;


PROCESS_SIBLING:

   init_debug_file();

   dump_trace_info (debug_file, PU_Start, NULL, "IR_dump");

   if (!SCP_IN_ERR(curr_scp_idx) ) {

      curr_stmt_sh_idx = SCP_FIRST_SH_IDX(curr_scp_idx);

      while (curr_stmt_sh_idx != NULL_IDX) {

         dump_stmt_ntry(debug_file, TRUE);

         if (SH_NEXT_IDX(curr_stmt_sh_idx) == curr_stmt_sh_idx) {

            /* Turn off ir table dump, so this doesn't loop forever */

            dump_flags.ir1_tbl	= FALSE;
            dump_flags.ir2_tbl	= FALSE;
            dump_flags.ir3_tbl	= FALSE;
            dump_flags.ir4_tbl	= FALSE;
            dump_flags.sytb 	= FALSE;

            PRINTMSG(SH_GLB_LINE(curr_stmt_sh_idx), 871, Internal,
                     SH_COL_NUM(curr_stmt_sh_idx), "SH_NEXT_IDX",
                     curr_stmt_sh_idx);
         }
         else if (SH_PREV_IDX(curr_stmt_sh_idx) == curr_stmt_sh_idx) {

            /* Turn off ir table dump, so this doesn't loop forever */

            dump_flags.ir1_tbl	= FALSE;
            dump_flags.ir2_tbl	= FALSE;
            dump_flags.ir3_tbl	= FALSE;
            dump_flags.ir4_tbl	= FALSE;
            dump_flags.sytb 	= FALSE;

            PRINTMSG(SH_GLB_LINE(curr_stmt_sh_idx), 871, Internal,
                     SH_COL_NUM(curr_stmt_sh_idx), "SH_PREV_IDX",
                     curr_stmt_sh_idx);
         }

         curr_stmt_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
      }
   }

   if (!print_all_scps) {
      return;
   }

   if (SCP_FIRST_CHILD_IDX(curr_scp_idx) != NULL_IDX) {
      save_curr_scp_idx = curr_scp_idx;
      curr_scp_idx = SCP_FIRST_CHILD_IDX(curr_scp_idx);
      print_all_text(TRUE);
      curr_scp_idx = save_curr_scp_idx;
   }

   if (SCP_SIBLING_IDX(curr_scp_idx) != NULL_IDX) {
      curr_scp_idx = SCP_SIBLING_IDX(curr_scp_idx);
      goto PROCESS_SIBLING;
   }

   fflush(debug_file);
   return;

} /* print_all_text */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      prints list texts and what they point to that are from a              *|
|*      Dv_Whole_Def_Opr.                                                     *|
|*      called by dump_ir_ntry                                                *|
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

static void  print_Dv_Whole_Def_Opr(FILE    *out_file,
                                    int      idx,
                                    int      indent,
                                    int      cnt,
#ifdef KEY /* Bug 6845 */
                                    int      n_dim
#endif /* KEY Bug 6845 */
				    )

{
   int	dim;
   char shift[80];
   char n_shift[INDENT_SIZE + 1];
   char str[16];
   int  i;

   for (i = 0; i < INDENT_SIZE * indent; i++) {
      shift[i] = ' ';
      if (i == 79)
         break;
   }
   shift[i] = '\0';
   for (i = 0; i < INDENT_SIZE; i++) {
      n_shift[i] = ' ';
   }
   n_shift[i] = '\0';

   dim = 0;

   for (i = 0; i < cnt; i++) {

      if (idx == NULL_IDX) {
         break;
      }

#ifdef KEY /* Bug 6845 */
      /* Print dope vector "header", then array bounds info if any, then
       * count and offsets for allocatable components if any */
      int first_bound =
        ((sizeof(dv_whole_def_str))/(sizeof(*dv_whole_def_str)));
      int first_alloc_cpnt = first_bound + 3 * n_dim;
      if (i < first_bound) {
         strcpy(str, dv_whole_def_str[i]);
      }
      else if (i < first_alloc_cpnt) {
         sprintf(str, dv_whole_def_bound_str[(i - first_bound) % 3],
	   (i - first_bound) / 3);
      }
      else {
         sprintf(str, dv_whole_def_alloc_cpnt_str, i - first_alloc_cpnt);
      }
#else /* KEY Bug 6845 */
      if (i < 10) {
         strcpy(str, dv_whole_def_str[i]);
      }
      else {
         if ((i - 10)%3 == 0) {
            dim++;
         }
         sprintf(str, dv_whole_def_str[i], dim);
      }
#endif /* KEY Bug 6845 */

      fprintf(out_file,"%s%-15s, idx = %d, %s",shift, str, idx,
                                         field_str[IL_FLD(idx)]);

      if (i == DEBUG_STR_TYPE_CODE) {
         convert_to_string_fmt = Hex_Fmt;
      }

      switch (IL_FLD(idx)) {
         case CN_Tbl_Idx :
         case AT_Tbl_Idx   :
                 fprintf(out_file," line = %d col = %d\n",IL_LINE_NUM(idx),
                                 IL_COL_NUM(idx));
                 break;
         case IL_Tbl_Idx  :
                 fprintf(out_file," list cnt = %d\n", IL_LIST_CNT(idx));
                 break;
         default         :
                 fprintf(out_file,"\n");
                 break;
      }


      switch (IL_FLD(idx)) {
         case NO_Tbl_Idx  :
                 break;
         case CN_Tbl_Idx :
                 print_const_entry(out_file, IL_IDX(idx), indent + 1);
                 break;
         case AT_Tbl_Idx   :
                 print_attr_name(out_file, IL_IDX(idx), indent + 1);
                 break;
         case IR_Tbl_Idx  :
                 dump_ir_ntry(out_file, IL_IDX(idx), indent + 1);
                 break;
         case IL_Tbl_Idx  :
                 print_list(out_file, IL_IDX(idx),
                            indent + 1, IL_LIST_CNT(idx), FALSE);
                 break;
         case SH_Tbl_Idx  :
                 fprintf(out_file, "%s%sstmt header idx = %d\n",shift,n_shift,
                         IL_IDX(idx));
                 break;
      }
      idx = IL_NEXT_LIST_IDX(idx);
   }

   return;

} /* print_Dv_Whole_Def_Opr */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      prints list texts and what they point to that are from a              *|
|*      mp directive opr.                                                     *|
|*      called by dump_ir_ntry                                                *|
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

static void  print_mp_dir_opr(FILE    *out_file,
                              int      idx,
                              int      indent,
                              int      cnt)

{
   char shift[80];
   char n_shift[INDENT_SIZE + 1];
   char str[80];
   int  i;


   for (i = 0; i < INDENT_SIZE * indent; i++) {
      shift[i] = ' ';
      if (i == 79)
         break;
   }
   shift[i] = '\0';
   for (i = 0; i < INDENT_SIZE; i++) {
      n_shift[i] = ' ';
   }
   n_shift[i] = '\0';


   for (i = 0; i < cnt; i++) {

      if (idx == NULL_IDX) {
         break;
      }

      strcpy(str, mp_dir_opr_str[i]);

      fprintf(out_file,"%s%-15s, idx = %d, %s",shift, str, idx,
                                         field_str[IL_FLD(idx)]);

      switch (IL_FLD(idx)) {
         case CN_Tbl_Idx :
         case AT_Tbl_Idx   :
                 fprintf(out_file," line = %d col = %d\n",IL_LINE_NUM(idx),
                                 IL_COL_NUM(idx));
                 break;
         case IL_Tbl_Idx  :
                 fprintf(out_file," list cnt = %d\n", IL_LIST_CNT(idx));
                 break;
         default         :
                 fprintf(out_file,"\n");
                 break;
      }


      switch (IL_FLD(idx)) {
         case NO_Tbl_Idx  :
                 break;
         case CN_Tbl_Idx :
                 print_const_entry(out_file, IL_IDX(idx), indent + 1);
                 break;
         case AT_Tbl_Idx   :
                 print_attr_name(out_file, IL_IDX(idx), indent + 1);
                 break;
         case IR_Tbl_Idx  :
                 dump_ir_ntry(out_file, IL_IDX(idx), indent + 1);
                 break;
         case IL_Tbl_Idx  :
                 print_list(out_file, IL_IDX(idx),
                            indent + 1, IL_LIST_CNT(idx), FALSE);
                 break;
         case SH_Tbl_Idx  :
                 fprintf(out_file, "%s%sstmt header idx = %d\n",shift,n_shift,
                         IL_IDX(idx));
                 break;
      }
      idx = IL_NEXT_LIST_IDX(idx);
   }

   return;

} /* print_mp_dir_opr */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      prints list texts and what they point to that are from a              *|
|*      open mp directive opr.                                                *|
|*      called by dump_ir_ntry                                                *|
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

static void  print_open_mp_dir_opr(FILE    *out_file,
                                   int      idx,
                                   int      indent,
                                   int      cnt)

{
   char shift[80];
   char n_shift[INDENT_SIZE + 1];
   char str[80];
   int  i;


   for (i = 0; i < INDENT_SIZE * indent; i++) {
      shift[i] = ' ';
      if (i == 79)
         break;
   }
   shift[i] = '\0';
   for (i = 0; i < INDENT_SIZE; i++) {
      n_shift[i] = ' ';
   }
   n_shift[i] = '\0';

   for (i = 0; i < cnt; i++) {

      if (idx == NULL_IDX) {
         break;
      }

      strcpy(str, open_mp_dir_opr_str[i]);

      fprintf(out_file,"%s%-15s, idx = %d, %s",shift, str, idx,
                                         field_str[IL_FLD(idx)]);

      switch (IL_FLD(idx)) {
         case CN_Tbl_Idx :
         case AT_Tbl_Idx   :
                 fprintf(out_file," line = %d col = %d\n",IL_LINE_NUM(idx),
                                 IL_COL_NUM(idx));
                 break;
         case IL_Tbl_Idx  :
                 fprintf(out_file," list cnt = %d\n", IL_LIST_CNT(idx));
                 break;
         default         :
                 fprintf(out_file,"\n");
                 break;
      }


      switch (IL_FLD(idx)) {
         case NO_Tbl_Idx  :
                 break;
         case CN_Tbl_Idx :
                 print_const_entry(out_file, IL_IDX(idx), indent + 1);
                 break;
         case AT_Tbl_Idx   :
                 print_attr_name(out_file, IL_IDX(idx), indent + 1);
                 break;
         case IR_Tbl_Idx  :
                 dump_ir_ntry(out_file, IL_IDX(idx), indent + 1);
                 break;
         case IL_Tbl_Idx  :
                 print_list(out_file, IL_IDX(idx),
                            indent + 1, IL_LIST_CNT(idx), FALSE);
                 break;
         case SH_Tbl_Idx  :
                 fprintf(out_file, "%s%sstmt header idx = %d\n",shift,n_shift,
                         IL_IDX(idx));
                 break;
      }
      idx = IL_NEXT_LIST_IDX(idx);
   }

   return;

} /* print_open_mp_dir_opr */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      prints attr name and index.                                           *|
|*      Used by dump_ir_ntry                                                  *|
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

static void  print_attr_name(FILE	*out_file,
			     int	 idx,
			     int         indent)

{
   int  i;
   char shift[80];
   char	str[80];
   int  type_idx;


   for (i = 0; i < INDENT_SIZE * indent; i++) {
      shift[i] = ' ';
      if (i == 79)
         break;
   }

   shift[i] = '\0';

   fprintf(out_file,"%s%s  idx = %d",shift, AT_OBJ_NAME_PTR(idx), idx);

   if (AT_OBJ_CLASS(idx) == Data_Obj) {
      type_idx	= ATD_TYPE_IDX(idx);

      if (type_idx == NULL_IDX && AT_ATTR_LINK(idx) == NULL_IDX) { 

         /* Turn off ir table dump, so this doesn't loop forever */

         dump_flags.ir1_tbl	= FALSE;
         dump_flags.ir2_tbl	= FALSE;
         dump_flags.ir3_tbl	= FALSE;
         dump_flags.ir4_tbl	= FALSE;
         dump_flags.sytb 	= FALSE;

         PRINTMSG(AT_DEF_LINE(idx), 891, Internal, AT_DEF_COLUMN(idx),
                  idx, AT_OBJ_NAME_PTR(idx));
      }

      fprintf(out_file,"  %s * ", basic_type_str[TYP_TYPE(type_idx)]);

      if (TYP_TYPE(type_idx) <= Last_Linear_Type) {
         fprintf(out_file, "%s  ", lin_type_str[TYP_LINEAR(type_idx)]);
      }
      else if (TYP_TYPE(type_idx) == Typeless) {
         fprintf(out_file, "%s  ", CONVERT_CVAL_TO_STR(&TYP_BIT_LEN(type_idx),
                                                        Integer_8,
                                                        str));
      }
      else if (TYP_TYPE(type_idx) != Character) {
         fprintf(out_file, "%d  ", TYP_IDX(type_idx));
      }
      else if (TYP_CHAR_CLASS(type_idx) == Assumed_Size_Char) {
         fprintf(out_file, "(*)  ");
      }
      else if (TYP_CHAR_CLASS(type_idx) == Const_Len_Char) {
         fprintf(out_file, "%s  ",
                 convert_to_string(&CN_CONST(TYP_IDX(type_idx)),
                                    CN_TYPE_IDX(TYP_IDX(type_idx)),
                                    str));
      }
      else {  /* Variable or unknown length char - print (tmp_idx = idx) */
         fprintf(out_file, "(tmp_idx = %d)  ", TYP_IDX(type_idx));
      }
   }

   if (AT_ATTR_LINK(idx)) {
      fprintf(out_file,"  AT_ATTR_LINK = %d\n",AT_ATTR_LINK(idx));
   }
   else {
      fprintf(out_file,"\n");
   }

   return;
} /* print_attr_name */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      prints list texts and what they point to.                             *|
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

static void  print_list(FILE	*out_file,
		        int	 idx,
		        int	 indent,
			int      cnt,
			boolean  io_list)

{
   char shift[80];
   char n_shift[INDENT_SIZE + 1];
   int  i;

   for (i = 0; i < INDENT_SIZE * indent; i++) {
      shift[i] = ' ';
      if (i == 79)
         break;
   }
   shift[i] = '\0';
   for (i = 0; i < INDENT_SIZE; i++) {
      n_shift[i] = ' ';
   }
   n_shift[i] = '\0';

   for (i = 0; i < cnt; i++) {

      if (idx == NULL_IDX) {
         break;
      }

      fprintf(out_file,"%slist item #%d, idx = %d, %s",shift, i + 1, idx,
                                         field_str[IL_FLD(idx)]);

      if (IL_ARG_DESC_VARIANT(idx)) {
         fprintf(out_file, "  IL_ARG_DESC_VARIANT ");
      }

      if (io_list) {
         if (IL_HAS_FUNCTIONS(idx)) {
            fprintf(out_file, "  IL_HAS_FUNCTIONS ");
         }

         if (IL_MUST_FLATTEN(idx)) {
            fprintf(out_file, "  IL_MUST_FLATTEN ");
         }

         if (IL_MUST_BE_LOOP(idx)) {
            fprintf(out_file, "  IL_MUST_BE_LOOP ");
         }
      }
      else {
         if (IL_VECTOR_SUBSCRIPT(idx)) {
            fprintf(out_file, "  IL_VECTOR_SUBSCRIPT ");
         }

         if (IL_CONSTANT_SUBSCRIPT(idx)) {
            fprintf(out_file, "  IL_CONSTANT_SUBSCRIPT ");
         }

         if (IL_PE_SUBSCRIPT(idx)) {
            fprintf(out_file, "  IL_PE_SUBSCRIPT ");
         }
      }

      if (IL_DISTRIBUTION_VARIANT(idx)) {
         fprintf(out_file, " %s ", distribution_str[IL_DISTRIBUTION(idx)]);
      }

      switch (IL_FLD(idx)) {
         case CN_Tbl_Idx :
         case AT_Tbl_Idx   :
         case SB_Tbl_Idx :
                 fprintf(out_file," line = %d col = %d\n",IL_LINE_NUM(idx),
                                 IL_COL_NUM(idx));
                 break;
         case IL_Tbl_Idx  :
                 fprintf(out_file," list cnt = %d\n", IL_LIST_CNT(idx));
                 break;
         default         :
                 fprintf(out_file,"\n");
                 break;
      }


      switch (IL_FLD(idx)) {
         case NO_Tbl_Idx  :
                 break;
         case CN_Tbl_Idx :
                 print_const_entry(out_file, IL_IDX(idx), indent + 1);
                 break;
         case AT_Tbl_Idx   :
                 print_attr_name(out_file, IL_IDX(idx), indent + 1);
                 break;
         case SB_Tbl_Idx :
                 fprintf(out_file,"%s%s%s\n", shift, n_shift,
                         SB_NAME_PTR(IL_IDX(idx)));
                 break;
         case IR_Tbl_Idx  :
                 dump_ir_ntry(out_file, IL_IDX(idx), indent + 1);
                 break;
         case IL_Tbl_Idx  :
                 print_list(out_file, IL_IDX(idx), 
                            indent + 1, IL_LIST_CNT(idx), io_list);
                 break;
         case SH_Tbl_Idx  :
                 fprintf(out_file, "%s%sstmt header idx = %d\n",shift,n_shift,
                         IL_IDX(idx));
                 break;
      }
      idx = IL_NEXT_LIST_IDX(idx);
   }

   if (idx != NULL_IDX) {

      /* Turn off symbol table dump, so this doesn't loop forever */

      dump_flags.sytb		= FALSE;
      dump_flags.ir1_tbl	= FALSE;
      dump_flags.ir2_tbl	= FALSE;
      dump_flags.ir3_tbl	= FALSE;
      dump_flags.ir4_tbl	= FALSE;

      PRINTMSG(1, 670, Internal, 0);
   }

   return;
  
} /* print_list */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      prints constant table entry.                                          *|
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

static void  print_const_entry(FILE	*out_file,
		               int	 idx,
			       int      indent)

{
   long		i;
   char 	shift[80];
   int		type_idx;
   char		str[80];


   if (idx == 0 || idx > const_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* CN index value [%d] is out of range.\n",
              idx);
      return;
   }

   type_idx = CN_TYPE_IDX(idx);

   for (i = 0; i < INDENT_SIZE * indent; i++) {
      shift[i] = ' ';
      if (i == 79)
         break;
   }

   shift[i] = '\0';
   fprintf(out_file,"%s", shift);
   print_const_f(out_file, idx);
   fprintf(out_file, "  IDX = %d", idx);

   if (TYP_TYPE(type_idx) == Character) {
      fprintf(out_file, "   LEN = %s", 
              convert_to_string(&CN_CONST(TYP_IDX(type_idx)),
                                CG_INTEGER_DEFAULT_TYPE,
                                str));

   }

   if (TYP_TYPE(type_idx) == Typeless) {
      fprintf(out_file, "  %s  BIT LEN = %s\n",
                        basic_type_str[TYP_TYPE(type_idx)],
                        CONVERT_CVAL_TO_STR(&TYP_BIT_LEN(type_idx),
                                             Integer_8,
                                             str));
   }
   else if (TYP_TYPE(type_idx) <= Last_Linear_Type) {
      fprintf(out_file, "  %s * %s\n",
                        basic_type_str[TYP_TYPE(type_idx)],
                        lin_type_str[TYP_LINEAR(type_idx)]);
   }
   else {
      /* 27Nov00[sos} Was:                                                      */
      /* fprintf(out_file, "  %s*(%d)\n", basic_type_str[TYP_TYPE(type_idx)],   */
      /*                                  TYP_IDX(type_idx));                   */
      fprintf(out_file, "  %s\n", print_type_f(type_idx));
   }

   return;

} /* print_const_entry */

/******************************************************************************\
|*									      *|
|*	This section contains routines to print an expanded IR/IL format.     *|
|*	This format is FORTRAN like.                                          *|
|*									      *|
\******************************************************************************/

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	NEED DESCRIPTION						      *|
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

void	print_expanded_stmt(void)
{
   int  save_curr_scp_idx;
   int  save_curr_stmt_sh_idx;

   save_curr_scp_idx     = curr_scp_idx;
   curr_scp_idx          = 1;
   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   print_expanded_stmt_for_scp();

   curr_scp_idx     = save_curr_scp_idx;
   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   return;
}


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	NEED DESCRIPTION						      *|
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

static void	print_expanded_stmt_for_scp(void)
{
   int		sh_idx;
   int		save_curr_scp_idx;

   init_debug_file();

PROCESS_SIBLING:

   fprintf(debug_file, "\n****************************************"
                      "****************************************\n");
   fprintf(debug_file, "\n\t\t\t EXPANDED IR FOR %s\n\n",
          AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
   fprintf(debug_file, "****************************************"
                      "****************************************\n\n");


   sh_idx = SCP_FIRST_SH_IDX(curr_scp_idx);

   while (sh_idx != NULL_IDX) {
      print_expanded_ir(SH_IR_IDX(sh_idx));
      fprintf(debug_file, "\n");
      sh_idx	= SH_NEXT_IDX(sh_idx);
   }

   fprintf(debug_file, "\n****************************************"
                      "****************************************\n");

   if (SCP_FIRST_CHILD_IDX(curr_scp_idx) != NULL_IDX) {
      save_curr_scp_idx = curr_scp_idx;
      curr_scp_idx = SCP_FIRST_CHILD_IDX(curr_scp_idx);
      print_expanded_stmt_for_scp();
      curr_scp_idx = save_curr_scp_idx;
   }

   if (SCP_SIBLING_IDX(curr_scp_idx) != NULL_IDX) {
      curr_scp_idx = SCP_SIBLING_IDX(curr_scp_idx);
      goto PROCESS_SIBLING;
   }

   return;
}


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	NEED DESCRIPTION						      *|
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

static void  print_expanded_const(int	 idx)

{
   long64	i;
   int		type_idx;
   char		str[80];


   type_idx = CN_TYPE_IDX(idx);

   switch (TYP_TYPE(type_idx)) {
   case Typeless:
      convert_to_string_fmt = Hex_Fmt;
      fprintf(debug_file,"0x%s",
              convert_to_string(&CN_CONST(idx), type_idx, str));

      if (TYP_BIT_LEN(type_idx) > TARGET_BITS_PER_WORD) {

         for (i = 1; 
              i < (TYP_BIT_LEN(type_idx) + TARGET_BITS_PER_WORD - 1) / 
                                           TARGET_BITS_PER_WORD;
              i++) {
            convert_to_string_fmt = Hex_Fmt;
            fprintf(debug_file, "%s",
                    convert_to_string(&CP_CONSTANT(CN_POOL_IDX(idx)+i),
                                       type_idx,
                                       str));
         }
      }
      break;

   case Integer:
      fprintf(debug_file, "%s", convert_to_string(&CN_CONST(idx), 
                                type_idx, str));
      break;

   case Real:
      fprintf(debug_file, "%s", convert_to_string(&CN_CONST(idx), 
                                type_idx, str));
      break;

   case Character:
      fprintf(debug_file,"\"%s\"", (char *) &CN_CONST(idx));
      break;

   case Logical:
      fprintf(debug_file, "%s", (THIS_IS_TRUE(&(CN_CONST(idx)),
                                              CN_TYPE_IDX(idx)) ? 
                                         ".TRUE." :  ".FALSE."));
      break;

   case Complex:
      fprintf(debug_file, "%s", convert_to_string(&CN_CONST(idx),
                                                   CN_TYPE_IDX(idx),
                                                   str));
      break;
   }

   fprintf(debug_file, " ");

   return;

} /* print_expanded_const */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	NEED DESCRIPTION						      *|
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

static	void	print_expanded_ir(int	ir_idx)

{
   switch (IR_OPR(ir_idx)) {
      case Null_Opr:
      case Defined_Un_Opr:
      case Alloc_Opr:
      case SSD_Alloc_Opr:
      case Cvrt_Opr:
      case Dealloc_Opr:
         fprintf(debug_file, "%s ", opr_str[IR_OPR(ir_idx)]);
         break;

/* Unary operators - Opr Left */

      case Uplus_Opr:       
      case Uminus_Opr:
      case Not_Opr:
         fprintf(debug_file, "%s ", opr_str[IR_OPR(ir_idx)]);
         print_expanded_opnd(IR_OPND_L(ir_idx));
         break;

/* Binary operators - Left opr Right */

      case Power_Opr:
      case Mult_Opr:        
      case Div_Opr:
      case Plus_Opr:        
      case Minus_Opr:
      case Concat_Opr:
      case Eq_Opr:          
      case Ne_Opr:
      case Lt_Opr:          
      case Le_Opr:
      case Gt_Opr:          
      case Ge_Opr:
      case And_Opr:
      case Or_Opr:
      case Eqv_Opr:         
      case Neqv_Opr:       
      case Asg_Opr:
         print_expanded_opnd(IR_OPND_L(ir_idx));
         fprintf(debug_file, "%s ", opr_str[IR_OPR(ir_idx)]);
         print_expanded_opnd(IR_OPND_R(ir_idx));
         break;

/* Call operators */

      case Bnot_Opr:
      case Bor_Opr:
      case Beqv_Opr:         
      case Bneqv_Opr:       

      case Abs_Opr:
      case Cos_Opr:
      case Sin_Opr:
      case Log_E_Opr:
      case Log_10_Opr:
      case Tan_Opr:
      case Tanh_Opr:
      case Sinh_Opr:
      case Acos_Opr:
      case Asin_Opr:
      case Atan_Opr:
      case Cosh_Opr:
      case Atan2_Opr:
      case Aimag_Opr:
      case Sqrt_Opr:
      case Cot_Opr:
      case Exp_Opr:
      case Int_Opr:
      case Band_Opr:
      case Mod_Opr:
      case Anint_Opr:
      case Nint_Opr:
      case Sign_Opr:
      case Modulo_Opr:
      case Shift_Opr:
      case Shiftl_Opr:
      case Shiftr_Opr:
      case Leadz_Opr:
      case Popcnt_Opr:
      case Poppar_Opr:
      case Aint_Opr:
      case Dim_Opr:
      case Ranget_Opr:
      case Ranset_Opr:
      case Ranf_Opr:
      case Real_Opr:
      case Dble_Opr:
      case Mask_Opr:
      case Conjg_Opr:
      case Dprod_Opr:
      case Length_Opr:
      case Getpos_Opr:
      case Unit_Opr:
      case Cmplx_Opr:
      case Ichar_Opr:
      case Char_Opr:
      case Index_Opr:
      case Lge_Opr:
      case Lgt_Opr:
      case Lle_Opr:
      case Llt_Opr:
      case Fcd_Opr:
      case Numarg_Opr:
      case Rtc_Opr:
      case Cvmgp_Opr:
      case Cvmgm_Opr:
      case Cvmgz_Opr:
      case Cvmgn_Opr:
      case Cvmgt_Opr:
      case Csmg_Opr:
      case Adjustl_Opr:
      case Adjustr_Opr:
      case Ceiling_Opr:
      case Exponent_Opr:
      case Floor_Opr:
      case Fraction_Opr:
      case Spacing_Opr:
      case Logical_Opr:
      case Nearest_Opr:
      case Rrspacing_Opr:
      case Scale_Opr:
      case Scan_Opr:
      case Set_Exponent_Opr:
      case Verify_Opr:
      case Len_Trim_Opr:
      case Dshiftl_Opr:
      case Dshiftr_Opr:
      case Mmx_Opr:
      case Mldmx_Opr:
      case Mld_Opr:
      case Mul_Opr:
      case Mcbl_Opr:
      case Cshift_Opr:
      case Dot_Product_Opr:
      case Matmul_Opr:
      case Spread_Opr:
      case Transpose_Opr:
      case All_Opr:
      case Any_Opr:
      case Count_Opr:
      case Product_Opr:
      case Sum_Opr:
      case Eoshift_Opr:
      case Maxval_Opr:
      case Minval_Opr:
      case Maxloc_Opr:
      case Minloc_Opr:
      case Reshape_Opr:
      case SRK_Opr:
      case SIK_Opr:
      case Repeat_Opr:
      case Trim_Opr:
      case Transfer_Opr:
#ifdef KEY /* Bug 1324 */
      case Erf_Opr:
      case Erfc_Opr:
#endif /* KEY Bug 1324 */
#ifdef KEY /* Bug 10410 */
      case Cselect_Opr:
#endif /* KEY Bug 10410 */
# ifdef _TARGET_OS_MAX
      case My_Pe_Opr:
# endif
         fprintf(debug_file, "%s ", opr_str[IR_OPR(ir_idx)]);
         print_expanded_opnd(IR_OPND_L(ir_idx));
         print_expanded_opnd(IR_OPND_R(ir_idx));
         fprintf(debug_file, ")");
         break;

      case Call_Opr:
         fprintf(debug_file, "%s ", opr_str[IR_OPR(ir_idx)]);
         print_expanded_opnd(IR_OPND_L(ir_idx));
         fprintf(debug_file, "(");
         print_expanded_opnd(IR_OPND_R(ir_idx));
         fprintf(debug_file, ")");
         break;

      case Defined_Bin_Opr:

      case Alt_Return_Opr:
      case Case_Opr:
      case Allocate_Opr:
      case Deallocate_Opr:
      case End_Opr:
      case Entry_Opr:
      case Nullify_Opr:
      case Pause_Opr:
      case Ptr_Asg_Opr:
      case Flat_Array_Asg_Opr:
      case Return_Opr:
      case Select_Opr:
      case Stmt_Func_Call_Opr:
      case Stop_Opr:
      case Max_Opr:
      case Min_Opr:
      case Read_Formatted_Opr:
      case Read_Unformatted_Opr:
      case Read_Namelist_Opr:
      case Write_Formatted_Opr:
      case Write_Unformatted_Opr:
      case Write_Namelist_Opr:
      case Inquire_Iolength_Opr:
      case Dv_Whole_Copy_Opr:
      case Dv_Whole_Def_Opr:
      case Dv_Deref_Opr:
      case Dv_Access_Base_Addr:
      case Dv_Set_Base_Addr:
      case Dv_Access_El_Len:
      case Dv_Set_El_Len:
      case Dv_Access_Assoc:
      case Dv_Set_Assoc:
      case Dv_Access_Ptr_Alloc:
      case Dv_Set_Ptr_Alloc:
      case Dv_Access_P_Or_A:
      case Dv_Set_P_Or_A:
      case Dv_Access_A_Contig:
      case Dv_Set_A_Contig:
      case Dv_Access_N_Dim:
      case Dv_Set_N_Dim:
      case Dv_Access_Typ_Code:
      case Dv_Set_Typ_Code:
      case Dv_Access_Orig_Base:
      case Dv_Set_Orig_Base:
      case Dv_Access_Orig_Size:
      case Dv_Set_Orig_Size:
      case Dv_Access_Low_Bound:
      case Dv_Set_Low_Bound:
      case Dv_Access_Extent:
      case Dv_Set_Extent:
      case Dv_Access_Stride_Mult:
      case Dv_Set_Stride_Mult:
      case Br_Aif_Opr:
      case Br_Asg_Opr:
      case Br_Index_Opr:
      case Br_True_Opr:
      case Br_Uncond_Opr:
      case Case_Range_Opr:
      case Implied_Do_Opr:
      case Kwd_Opr:
      case Loc_Opr:
      case Aloc_Opr:
      case Const_Tmp_Loc_Opr:
      case Len_Opr:
      case Clen_Opr:
      case Paren_Opr:
      case Struct_Opr:
      case Struct_Construct_Opr:
      case Array_Construct_Opr:
      case Constant_Struct_Construct_Opr:
      case Constant_Array_Construct_Opr:
      case Subscript_Opr:
      case Whole_Subscript_Opr:
      case Section_Subscript_Opr:
      case Alloc_Obj_Opr:
      case Dealloc_Obj_Opr:
      case Substring_Opr:
      case Whole_Substring_Opr:
      case Triplet_Opr:
      case Label_Opr:
      case Loop_Info_Opr:
      case Loop_End_Opr:
      case Init_Opr:
      case Init_Reloc_Opr:
      case Use_Opr:
      case Where_Opr:
      case Real_Div_To_Int_Opr:
      case Suppress_Opr:
      case Cache_Bypass_Cdir_Opr:
      case Vector_Cdir_Opr:
      case Novector_Cdir_Opr:
      case Task_Cdir_Opr:
      case Notask_Cdir_Opr:
      case Bounds_Cdir_Opr:
      case Nobounds_Cdir_Opr:
      case Recurrence_Cdir_Opr:
      case Norecurrence_Cdir_Opr:
      case Vsearch_Cdir_Opr:
      case Novsearch_Cdir_Opr:
      case Bl_Cdir_Opr:
      case Nobl_Cdir_Opr:
      case Inline_Cdir_Opr:
      case Noinline_Cdir_Opr:
      case Ivdep_Cdir_Opr:
      case Nextscalar_Cdir_Opr:
      case Prefervector_Cdir_Opr:
      case Prefertask_Cdir_Opr:
      case Shortloop_Cdir_Opr:
      case Shortloop128_Cdir_Opr:
      case Cachealign_Cdir_Opr:
      case Nounroll_Cdir_Opr:
      case Unroll_Cdir_Opr:
      case Align_Cdir_Opr:
      case Case_Cmic_Opr:
      case Endcase_Cmic_Opr:
      case Continue_Cmic_Opr:
      case Cncall_Cmic_Opr:
      case Doall_Cmic_Opr:
      case Doparallel_Cmic_Opr:
      case Enddo_Cmic_Opr:
      case Guard_Cmic_Opr:
      case Endguard_Cmic_Opr:
      case Numcpus_Cmic_Opr:
      case Parallel_Cmic_Opr:
      case Endparallel_Cmic_Opr:
      case Permutation_Cmic_Opr:
      case Taskcommon_Cmic_Opr:
      case Wait_Cmic_Opr:
      case Send_Cmic_Opr:
      case The_Last_Opr:
         fprintf(debug_file, "%s ", opr_str[IR_OPR(ir_idx)]);
         print_expanded_opnd(IR_OPND_L(ir_idx));
         print_expanded_opnd(IR_OPND_R(ir_idx));
         break;

      }
   return;
}


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	NEED DESCRIPTION						      *|
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

static	void	print_expanded_opnd(opnd_type	the_opnd)

{
   switch(OPND_FLD(the_opnd)) {

   case AT_Tbl_Idx:
      fprintf(debug_file, "%s ", AT_OBJ_NAME_PTR(OPND_IDX(the_opnd)));
      break;

   case CN_Tbl_Idx:
      print_expanded_const(OPND_IDX(the_opnd));
      break;

   case IR_Tbl_Idx:
      print_expanded_ir(OPND_IDX(the_opnd));
      break;

   case IL_Tbl_Idx:
      print_expanded_il(OPND_IDX(the_opnd));
      break;

   }

   return;
}


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	NEED DESCRIPTION						      *|
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

static	void	print_expanded_il(int	il_idx)

{
   while (il_idx != NULL_IDX) {
      switch (IL_FLD(il_idx)) {
      case AT_Tbl_Idx:
         fprintf(debug_file, "%s ", AT_OBJ_NAME_PTR(IL_IDX(il_idx)));
         break;

      case CN_Tbl_Idx:
         print_expanded_const(IL_IDX(il_idx));
         break;

      case IR_Tbl_Idx:
         print_expanded_ir(IL_IDX(il_idx));
         break;

      case IL_Tbl_Idx:
         print_expanded_il(IL_IDX(il_idx));
         break;

      case SH_Tbl_Idx:
         break;
      }
      il_idx = IL_NEXT_LIST_IDX(il_idx);

      if (il_idx != NULL_IDX) {
         fprintf(debug_file, ", ");
      }
   }

   return;

}  /* print_expanded_il */

/******************************************************************************\
|*									      *|
|* The following are the actual dump routines for each table.  Global variable*|
|* full_debug_dump is used to control how much is dumped.  If it is TRUE, all *|
|* fields are printed in whatever routine is being called.  If it is FALSE,   *|
|* fields are printed except for index fields.  This is to allow for better   *|
|* comparisons.  Default is FALSE.                                            *|
|*									      *|
\******************************************************************************/

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints one attr list table entry to the specified output file.        *|
|*									      *|
|* Input parameters:							      *|
|*	FILE to print to - Should be debug_file, stderr, or stdout.           *|
|*	Index of attribute list entry to print.                               *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void dump_al_ntry (FILE		*out_file,
   		          int	 	 al_idx)

{
   if (al_idx > attr_list_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* AL index value [%d] is out of range.\n",
              al_idx);
      return;
   }

   if (AL_IDX_IS_EQ(al_idx)) {
      fprintf(out_file, " %-10s= %-6d  %-5s= %-6d\n",
              "AL_EQ_IDX", AL_EQ_IDX(al_idx),
              "NEXT", AL_NEXT_IDX(al_idx));
   }
   else if (AL_FREE(al_idx)) {
      fprintf(out_file, " %-10s= %-6s  %-5s= %-6d\n",
              "AL_FREE", boolean_str[AL_FREE(al_idx)],
              "NEXT", AL_NEXT_IDX(al_idx));
   }
   else {
      fprintf(out_file, " %-4s= %-6d %-4s= %-6d %-14s= %-6d %-s\n",
                        "ATTR", AL_ATTR_IDX(al_idx),
                        "NEXT", AL_NEXT_IDX(al_idx),
                        "Special field", AL_ENTRY_COUNT(al_idx),
                         AT_OBJ_NAME_PTR(AL_ATTR_IDX(al_idx)));
   }

   return;

}  /* dump_al_ntry */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints one attr table entry to the specified output file.             *|
|*									      *|
|* Input parameters:							      *|
|*	FILE to print to - Should be debug_file, stderr, or stdout.           *|
|*	Index of attribute entry to print.                                    *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void dump_at_ntry (FILE		*out_file,
   		          int	 	 at_idx,
			  boolean	 dump_all)

{
   int		il_idx;
   int		ro_idx;
   char		str[80];
   char		conv_str[80];


   if (at_idx > attr_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* AT index value [%d] is out of range.\n",
              at_idx);
      return;
   }

   /* Note that the fields are displayed in alphabetical order.         */

   fprintf(out_file, "%-s\n", AT_OBJ_NAME_PTR(at_idx));

   fprintf(out_file, "  %-25s %-25s %-16s= %-8d\n",
                     obj_class_str[AT_OBJ_CLASS(at_idx)],
                     reference_str[AT_REFERENCED(at_idx)],
                     "IDX", at_idx);

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                     "AT_ACCESS_SET", boolean_str[AT_ACCESS_SET(at_idx)],
                     "AT_ACTUAL_ARG", boolean_str[AT_ACTUAL_ARG(at_idx)],
                     "AT_ALT_DARG",   boolean_str[AT_ALT_DARG(at_idx)]);

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-8s\n",
                     "AT_ARG_TO_KIND",   boolean_str[AT_ARG_TO_KIND(at_idx)],
                     "AT_ATTR_LINK",     AT_ATTR_LINK(at_idx),
                     "AT_CIF_DONE",      boolean_str[AT_CIF_DONE(at_idx)]);

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-8s\n",
                    "AT_CIF_IN_USAGE_",boolean_str[AT_CIF_IN_USAGE_REC(at_idx)],
                    "AT_CIF_SYMBOL_ID", AT_CIF_SYMBOL_ID(at_idx),
                    "AT_CIF_USE_IN_BN", boolean_str[AT_CIF_USE_IN_BND(at_idx)]);

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8d\n",
                     "AT_COMPILER_GEND", boolean_str[AT_COMPILER_GEND(at_idx)], 
                     "AT_DCL_ERR", boolean_str[AT_DCL_ERR(at_idx)],
                     "AT_DEF_COLUMN", AT_DEF_COLUMN(at_idx));

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
                     "AT_DEF_LINE", AT_DEF_LINE(at_idx),
                     "AT_DEF_IN_CHILD", boolean_str[AT_DEF_IN_CHILD(at_idx)],
                     "AT_DEFINED", boolean_str[AT_DEFINED(at_idx)]);

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                   "AT_ELEMENTAL_INT", boolean_str[AT_ELEMENTAL_INTRIN(at_idx)],
                   "AT_HOST_ASSOCIAT",boolean_str[AT_HOST_ASSOCIATED(at_idx)],
                   "AT_IGNORE_ATTR_L",boolean_str[AT_IGNORE_ATTR_LINK(at_idx)]);

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                     "AT_IS_DARG", boolean_str[AT_IS_DARG(at_idx)],
                     "AT_IS_INTRIN", boolean_str[AT_IS_INTRIN(at_idx)],
                     "AT_LOCKED_IN", boolean_str[AT_LOCKED_IN(at_idx)]);

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8d\n",
                     "AT_MODULE_IDX", AT_MODULE_IDX(at_idx),
                     "AT_MODULE_OBJECT", boolean_str[AT_MODULE_OBJECT(at_idx)],
                     "AT_NAME_LEN", AT_NAME_LEN(at_idx));

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8d\n",
                     "AT_NAMELIST_OBJ", boolean_str[AT_NAMELIST_OBJ(at_idx)],
                     "AT_NOT_VISIBLE", boolean_str[AT_NOT_VISIBLE(at_idx)],
                     "AT_ORIG_MODULE_I", AT_ORIG_MODULE_IDX(at_idx));

   fprintf(out_file, "  %-16s= %-7d %-16s= %-s\n",
                     "AT_ORIG_NAME_LEN", AT_ORIG_NAME_LEN(at_idx),
                     "AT_ORIG_NAME_IDX", (AT_ORIG_NAME_IDX(at_idx) == NULL_IDX)
                                         ? "0": AT_ORIG_NAME_PTR(at_idx));

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                     "AT_OPTIONAL", boolean_str[AT_OPTIONAL(at_idx)],
                     "AT_PRIVATE", access_str[AT_PRIVATE(at_idx)],
                     "AT_REF_IN_CHILD", boolean_str[AT_REF_IN_CHILD(at_idx)]);

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                     "AT_SEMANTICS_DON", boolean_str[AT_SEMANTICS_DONE(at_idx)],
                     "AT_TYPED", boolean_str[AT_TYPED(at_idx)],
                     "AT_USE_ASSOCIATE",boolean_str[AT_USE_ASSOCIATED(at_idx)]);

   /* Note that the fields are displayed in alphabetical order.         */

   switch (AT_OBJ_CLASS(at_idx)) {

      case Data_Obj:

         fprintf(out_file, "  %-25s %-16s= %-7s %-16s= %-8s\n",
                 atd_class_str[ATD_CLASS(at_idx)],
                 "ATD_ALIGN_SYMBOL", boolean_str[ATD_ALIGN_SYMBOL(at_idx)],
                 "ATD_ALIGNMENT", align_str[ATD_ALIGNMENT(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-8s\n",
                 "ATD_ALLOCATABLE", boolean_str[ATD_ALLOCATABLE(at_idx)],
                 "ATD_ARRAY_IDX", ATD_ARRAY_IDX(at_idx),
                 "ATD_AUTOMATIC", boolean_str[ATD_AUTOMATIC(at_idx)]);

         if (ATD_AUTOMATIC(at_idx)) {
            fprintf(out_file, "  %-16s= %-7d %-33s\n",
                   "ATD_AUTO_BASE_ID", ATD_AUTO_BASE_IDX(at_idx),
                   print_at_name(ATD_AUTO_BASE_IDX(at_idx)));
         }

#ifdef KEY /* Bug 14150 */
	 if (ATD_CLASS(at_idx) != Dummy_Argument) {
            fprintf(out_file, "  %-16s= %-7s\n",
                    "AT_BIND_ATTR", boolean_str[AT_BIND_ATTR(at_idx)]);
	 }
#endif /* KEY Bug 14150 */
         if (ATD_CLASS(at_idx) == Variable) {
            fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
                    "ATD_ASSIGN_TMP_I", ATD_ASSIGN_TMP_IDX(at_idx),
                    "ATD_AUXILIARY", boolean_str[ATD_AUXILIARY(at_idx)],
                    "ATD_BOUNDS_CHECK", boolean_str[ATD_BOUNDS_CHECK(at_idx)]);

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                "ATD_CACHE_ALIGN", boolean_str[ATD_CACHE_ALIGN(at_idx)],
                "ATD_CACHE_BYPASS", boolean_str[ATD_CACHE_BYPASS_ARRAY(at_idx)],
                "ATD_CACHE_NOALLO", boolean_str[ATD_CACHE_NOALLOC(at_idx)]);

            fprintf(out_file, "  %-16s= %-7s\n",
                "ATD_CHAR_LEN_IN_", boolean_str[ATD_CHAR_LEN_IN_DV(at_idx)]);
         }
         else {
            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
               "ATD_AUXILIARY", boolean_str[ATD_AUXILIARY(at_idx)],
               "ATD_BOUNDS_CHECK", boolean_str[ATD_BOUNDS_CHECK(at_idx)],
               "ATD_CACHE_BYPASS", boolean_str[ATD_CACHE_BYPASS_ARRAY(at_idx)]);

            if (ATD_CLASS(at_idx) == Compiler_Tmp) {
               fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8d\n",
                  "ATD_CACHE_NOALLO", boolean_str[ATD_CACHE_NOALLOC(at_idx)],
                  "ATD_CHAR_LEN_IN_", boolean_str[ATD_CHAR_LEN_IN_DV(at_idx)],
                  "ATD_DEFINING_ATT", ATD_DEFINING_ATTR_IDX(at_idx));
            }
            else {
               fprintf(out_file, "  %-16s= %-7s %-16s= %-7s\n",
                  "ATD_CACHE_NOALLO", boolean_str[ATD_CACHE_NOALLOC(at_idx)],
                  "ATD_CHAR_LEN_IN_", boolean_str[ATD_CHAR_LEN_IN_DV(at_idx)]);
            }
         }


         if (ATD_CLASS(at_idx) == Struct_Component) {
           fprintf(out_file, "  %-16s= %-7d \n",
                   "ATD_DERIVED_TYPE", ATD_DERIVED_TYPE_IDX(at_idx));

            print_fld_idx(out_file, "ATD_CPNT_OFFSET_",
                          ATD_OFFSET_FLD(at_idx),
                          ATD_CPNT_OFFSET_IDX(at_idx));

            print_fld_idx(out_file, "ATD_CPNT_INIT_ID",
                          (fld_type) ATD_FLD(at_idx),
                          ATD_CPNT_INIT_IDX(at_idx));

            if (ATD_CPNT_INIT_IDX(at_idx) != NULL_IDX) { 

               if (ATD_FLD(at_idx) == IR_Tbl_Idx) {
                  dump_ir_ntry(out_file, ATD_CPNT_INIT_IDX(at_idx), 5);
               }
               else if (ATD_FLD(at_idx) == CN_Tbl_Idx) {
                  dump_cn_ntry(out_file, ATD_CPNT_INIT_IDX(at_idx));
               }
            }
         }
         else if (ATD_CLASS(at_idx) == Constant) {
             print_fld_idx(out_file, "ATD_CONST_IDX",
                           (fld_type) ATD_FLD(at_idx),
                           ATD_CONST_IDX(at_idx));
         }

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATD_COPY_ASSUMED",boolean_str[ATD_COPY_ASSUMED_SHAPE(at_idx)],
                 "ATD_DATA_INIT", boolean_str[ATD_DATA_INIT(at_idx)],
                 "ATD_DCL_EQUIV", boolean_str[ATD_DCL_EQUIV(at_idx)]);

         fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
                 "ATD_DISTRIBUTION", ATD_DISTRIBUTION_IDX(at_idx),
                 "ATD_WAS_SCOPED", boolean_str[ATD_WAS_SCOPED(at_idx)],
                 "ATD_DYNAMIC", boolean_str[ATD_DYNAMIC(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8d\n",
                 "ATD_EQUIV", boolean_str[ATD_EQUIV(at_idx)],
                 "ATD_EQUIV_IN_BND",boolean_str[ATD_EQUIV_IN_BNDS_EXPR(at_idx)],
                 "ATD_EQUIV_LIST", ATD_EQUIV_LIST(at_idx));

         if (ATD_EQUIV_LIST(at_idx) != NULL_IDX) {
            print_al_list(out_file, ATD_EQUIV_LIST(at_idx));
         }

# if defined(_EXPRESSION_EVAL)

         if (cmd_line_flags.expression_eval_stmt || 
             cmd_line_flags.expression_eval_expr) {
            fprintf(out_file, "  %-16s= %-7s\n",
                   "ATD_EXPR_EVAL_TMP", boolean_str[ATD_EXPR_EVAL_TMP(at_idx)]);
         }
# endif

         fprintf(out_file,"  %-16s= %-7s %-16s= %-7d %-16s= %-8s\n",
                 "ATD_FILL_SYMBOL", boolean_str[ATD_FILL_SYMBOL(at_idx)],
                 "ATD_FIRST_SEEN_I", ATD_FIRST_SEEN_IL_IDX(at_idx),
                 "ATD_FORALL_INDEX",boolean_str[ATD_FORALL_INDEX(at_idx)]);

         if (ATD_CLASS(at_idx) == Function_Result) {
            fprintf(out_file, "  %-16s= %-s\n",
                    "Function Name", print_at_name(ATD_FUNC_IDX(at_idx)));
         }
         else if (ATD_CLASS(at_idx) == Dummy_Argument) {
#ifdef KEY /* Bug 14150 */
            fprintf(out_file,"  %-16s= %-7s %-16s= %-7s %-16s= %-7s\n",
                    "ATD_INTENT",intent_str[ATD_INTENT(at_idx)],
                    "ATD_INTRIN_DARG", boolean_str[ATD_INTRIN_DARG(at_idx)],
		    "ATD_VALUE_ATTR", boolean_str[ATD_VALUE_ATTR(at_idx)]
		    );
#else /* KEY Bug 14150 */
            fprintf(out_file,"  %-16s= %-7s %-16s= %-7s\n",
                    "ATD_INTENT",intent_str[ATD_INTENT(at_idx)],
                    "ATD_INTRIN_DARG", boolean_str[ATD_INTRIN_DARG(at_idx)]);
#endif /* KEY Bug 14150 */

            if (ATD_INTRIN_DARG(at_idx)) {
               fprintf(out_file,"  %-20s= %-22o\n",
                       "ATD_INTRIN_DARG_TYPE", ATD_INTRIN_DARG_TYPE(at_idx));
            }
         }

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
		 "ATD_IGNORE_TKR", boolean_str[ATD_IGNORE_TKR(at_idx)],
		 "ATD_IM_A_DOPE", boolean_str[ATD_IM_A_DOPE(at_idx)],
                 "ATD_IMP_DO_LCV", boolean_str[ATD_IMP_DO_LCV(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATD_IN_ASSIGN", boolean_str[ATD_IN_ASSIGN(at_idx)],
                 "ATD_IN_COMMON", boolean_str[ATD_IN_COMMON(at_idx)],
                 "ATD_LCV_IS_CONST", boolean_str[ATD_LCV_IS_CONST(at_idx)]);

         if (ATD_CLASS(at_idx) == Compiler_Tmp ||
             ATD_CLASS(at_idx) == Dummy_Argument) {

            if (ATD_NO_ENTRY_LIST(at_idx) != NULL_IDX) {
               fprintf(out_file,"\n");
               print_al_list(out_file, ATD_NO_ENTRY_LIST(at_idx));
            }
         }

         if ((ATD_CLASS(at_idx) == Variable ||
              ATD_CLASS(at_idx) == Compiler_Tmp) &&
              ATD_IN_COMMON(at_idx)) {
            fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-8s\n",
                "ATD_LIVE_DO_VAR", boolean_str[ATD_LIVE_DO_VAR(at_idx)],
                "ATD_NEXT_MEMBER_", ATD_NEXT_MEMBER_IDX(at_idx),
                "ATD_NOBOUNDS_CHE",boolean_str[ATD_NOBOUNDS_CHECK(at_idx)]);

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s\n",
                "ATD_NOT_PT_UNIQU", boolean_str[ATD_NOT_PT_UNIQUE_MEM(at_idx)],
                 "ATD_OFFSET_ASSIG",boolean_str[ATD_OFFSET_ASSIGNED(at_idx)]);
         }
         else {
            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-7s\n",
                "ATD_LIVE_DO_VAR", boolean_str[ATD_LIVE_DO_VAR(at_idx)],
                "ATD_NOBOUNDS_CHE",boolean_str[ATD_NOBOUNDS_CHECK(at_idx)],
                "ATD_NOT_PT_UNIQU", boolean_str[ATD_NOT_PT_UNIQUE_MEM(at_idx)]);

            fprintf(out_file, "  %-16s= %-7s\n",
                "ATD_OFFSET_ASSIG",boolean_str[ATD_OFFSET_ASSIGNED(at_idx)]);
         }

         if ((ATD_CLASS(at_idx) == Variable && !ATD_AUTOMATIC(at_idx)) ||
             ((ATD_CLASS(at_idx) == Dummy_Argument ||
               ATD_CLASS(at_idx) == Compiler_Tmp) && 
               ATD_OFFSET_ASSIGNED(at_idx)) ||
             ATD_CLASS(at_idx) == Function_Result) {
            print_fld_idx(out_file, "ATD_OFFSET_IDX",
                          ATD_OFFSET_FLD(at_idx),
                          ATD_OFFSET_IDX(at_idx));
         }
         else if (ATD_CLASS(at_idx) == CRI__Pointee) {
            fprintf(out_file,"  %-16s= %-7d %-s\n",
                   "Pointer Name", ATD_PTR_IDX(at_idx),
                   print_at_name(ATD_PTR_IDX(at_idx)));
         }

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-8s\n",
                 "ATD_PARENT_OBJEC",boolean_str[ATD_PARENT_OBJECT(at_idx)],
                 "ATD_PE_ARRAY_IDX", ATD_PE_ARRAY_IDX(at_idx),
                 "ATD_PERMUTATION", boolean_str[ATD_PERMUTATION(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATD_POINTER", boolean_str[ATD_POINTER(at_idx)],
                 "ATD_PTR_ASSIGNED", boolean_str[ATD_PTR_ASSIGNED(at_idx)],
                 "ATD_PTR_HALF_WOR", boolean_str[ATD_PTR_HALF_WORD(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATD_PTR_TYPE_SET",boolean_str[ATD_PTR_TYPE_SET(at_idx)],
                 "ATD_PURE",boolean_str[ATD_PURE(at_idx)],
                 "ATD_RESHAPE ARRA",boolean_str[ATD_RESHAPE_ARRAY_OPT(at_idx)]);

         fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
                 "ATD_RESHAPE_IDX", ATD_RESHAPE_ARRAY_IDX(at_idx),
                 "ATD_SAVED", boolean_str[ATD_SAVED(at_idx)],
                 "ATD_SECTION_GP", boolean_str[ATD_SECTION_GP(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATD_SECTION_NON_", boolean_str[ATD_SECTION_NON_GP(at_idx)],
                 "ATD_SEEN_AS_LCV", boolean_str[ATD_SEEN_AS_LCV(at_idx)],
                 "ATD_SEEN_AS_IO_", boolean_str[ATD_SEEN_AS_IO_LCV(at_idx)]);


         if (ATD_CLASS(at_idx) == Dummy_Argument) {

            if (ATD_SF_DARG(at_idx)) {
               print_fld_idx(out_file, "ATD_SF_ARG_IDX",
                             (fld_type) ATD_FLD(at_idx),
                             ATD_SF_ARG_IDX(at_idx));

               fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
                 "ATD_SF_LINK", ATD_SF_LINK(at_idx),
                 "ATD_SYMBOLIC_CON",boolean_str[ATD_SYMBOLIC_CONSTANT(at_idx)],
                 "ATD_SYMMETRIC", boolean_str[ATD_SYMMETRIC(at_idx)]);
            }
            else {
                fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                  "ATD_SF_DARG", boolean_str[ATD_SF_DARG(at_idx)],
                  "ATD_SYMBOLIC_CON",boolean_str[ATD_SYMBOLIC_CONSTANT(at_idx)],
                  "ATD_SYMMETRIC", boolean_str[ATD_SYMMETRIC(at_idx)]);
            }

            fprintf(out_file, "  %-16s= %-7s\n",
                   "ATD_SEEN_IN_IMP", boolean_str[ATD_SEEN_IN_IMP_DO(at_idx)]);
         }
         else {
             fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                  "ATD_SEEN_IN_IMP", boolean_str[ATD_SEEN_IN_IMP_DO(at_idx)],
                  "ATD_SYMBOLIC_CON",boolean_str[ATD_SYMBOLIC_CONSTANT(at_idx)],
                  "ATD_SYMMETRIC", boolean_str[ATD_SYMMETRIC(at_idx)]);
         }

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8d\n",
                 "ATD_SEEN_OUTSID",boolean_str[ATD_SEEN_OUTSIDE_IMP_DO(at_idx)],
                 "ATD_STACK", boolean_str[ATD_STACK(at_idx)],
                 "ATD_STOR_BLK_IDX", ATD_STOR_BLK_IDX(at_idx));

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATD_TARGET", boolean_str[ATD_TARGET(at_idx)],
                 "ATD_TASK_COPYIN", boolean_str[ATD_TASK_COPYIN(at_idx)],
                 "ATD_TASK_FIRSTPR",boolean_str[ATD_TASK_FIRSTPRIVATE(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATD_TASK_GETFIRS", boolean_str[ATD_TASK_GETFIRST(at_idx)],
                 "ATD_TASK_LASTLOC",boolean_str[ATD_TASK_LASTLOCAL(at_idx)],
                 "ATD_TASK_LASTPRI", boolean_str[ATD_TASK_LASTPRIVATE(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATD_TASK_LASTTHR",boolean_str[ATD_TASK_LASTTHREAD(at_idx)],
                 "ATD_TASK_PRIVATE", boolean_str[ATD_TASK_PRIVATE(at_idx)],
                 "ATD_TASK_SHARED", boolean_str[ATD_TASK_SHARED(at_idx)]);


         if (ATD_CLASS(at_idx) == Compiler_Tmp) {
            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATD_TMP_GEN_ZERO", boolean_str[ATD_TMP_GEN_ZERO(at_idx)],
                 "ATD_TMP_HAS_CVRT", boolean_str[ATD_TMP_HAS_CVRT_OPR(at_idx)],
                 "ATD_TMP_INIT_NOT",boolean_str[ATD_TMP_INIT_NOT_DONE(at_idx)]);

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s=%-7s\n",
                "ATD_TMP_NEEDS_CI",boolean_str[ATD_TMP_NEEDS_CIF(at_idx)],
                "ATD_TMP_SEMANTIC",boolean_str[ATD_TMP_SEMANTICS_DONE(at_idx)],
                "ATD_TOO_BIG_FOR_",boolean_str[ATD_TOO_BIG_FOR_DV(at_idx)]);

            print_fld_idx(out_file, "ATD_TMP_IDX",
                          (fld_type) ATD_FLD(at_idx),
                          ATD_TMP_IDX(at_idx));

            if (ATD_SYMBOLIC_CONSTANT(at_idx) && ATD_FLD(at_idx) == IR_Tbl_Idx){
               dump_ir_ntry(out_file, ATD_TMP_IDX(at_idx), 4);
            }

            if (ATD_TMP_INIT_NOT_DONE(at_idx)) { /* dump tmp's constant */
               fprintf(out_file, "\nCONSTANT FOR INIT\n");

               if (ATD_FLD(at_idx) == CN_Tbl_Idx) {
                  dump_cn_ntry(out_file, ATD_TMP_IDX(at_idx));
               }
               else {
                  fprintf(out_file, "COUNT = \n");
                  dump_cn_ntry(out_file, IR_IDX_L(ATD_TMP_IDX(at_idx)));
                  fprintf(out_file, "VALUE = \n");
                  dump_cn_ntry(out_file, IR_IDX_R(ATD_TMP_IDX(at_idx)));
               }

            }
         }
         else {
            fprintf(out_file, "  %-16s= %-7s\n",
                "ATD_TOO_BIG_FOR_",boolean_str[ATD_TOO_BIG_FOR_DV(at_idx)]);
         }

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-s\n",
                           "ATD_VOLATILE",boolean_str[ATD_VOLATILE(at_idx)],
                           "ATD_TYPE_IDX",  ATD_TYPE_IDX(at_idx),
                            print_type_f(ATD_TYPE_IDX(at_idx)));

         if (ATD_CLASS(at_idx) == Variable) {  
            fprintf(out_file,"  %-16s= %-7d\n",
                    "ATD_VARIABLE_TMP", ATD_VARIABLE_TMP_IDX(at_idx));
         }

         if (dump_all && ATD_STOR_BLK_IDX(at_idx) != NULL_IDX) {
            fprintf(out_file, "\n");
            dump_sb_ntry(out_file, ATD_STOR_BLK_IDX(at_idx));
         }

         if (dump_all && ATD_ARRAY_IDX(at_idx) != NULL_IDX) {
            fprintf(out_file, "\n");
            dump_bd_ntry(out_file, ATD_ARRAY_IDX(at_idx));
         }

         if (dump_all && ATD_DISTRIBUTION_IDX(at_idx) != NULL_IDX) {
            fprintf(out_file, "\n");
            fprintf(out_file, "ATD_DISTRIBUTION_IDX bounds table dump\n");
            dump_bd_ntry(out_file, ATD_DISTRIBUTION_IDX(at_idx));
         }

#ifdef _F_MINUS_MINUS
         if (dump_all && ATD_PE_ARRAY_IDX(at_idx) != NULL_IDX) {
            fprintf(out_file, "\n");
            fprintf(out_file, "ATD_PE_ARRAY_IDX bounds table dump\n");
            dump_bd_ntry(out_file, ATD_PE_ARRAY_IDX(at_idx));
         }
# endif

         break;


      case Pgm_Unit:
         fprintf(out_file, "  %-25s %-25s %-16s= %-8s\n",
                 atp_pgm_unit_str[ATP_PGM_UNIT(at_idx)],
                 atp_proc_str[ATP_PROC(at_idx)],
                 "ATP_ALIGN", boolean_str[ATP_ALIGN(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATP_ALL_INTENT_I", boolean_str[ATP_ALL_INTENT_IN(at_idx)],
                 "ATP_ALT_ENTRY", boolean_str[ATP_ALT_ENTRY(at_idx)],
                 "ATP_ARGCHCK_CALL", boolean_str[ATP_ARGCHCK_CALL(at_idx)]);

         if (ATP_PGM_UNIT(at_idx) != Module) {
            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8d\n",
                   "ATP_ARGCHCK_ENTR", boolean_str[ATP_ARGCHCK_ENTRY(at_idx)],
                   "ATP_DCL_EXTERNAL", boolean_str[ATP_DCL_EXTERNAL(at_idx)],
                   "ATP_DUPLICATE_IN", ATP_DUPLICATE_INTERFACE_IDX(at_idx));

#ifdef KEY /* Bug 14150 */
	    if (ATP_PGM_UNIT(at_idx) == Function ||
	      ATP_PGM_UNIT(at_idx) == Pgm_Unknown ||
	      ATP_PGM_UNIT(at_idx) == Subroutine) {
	      fprintf(out_file, "  %-16s= %-7s\n",
                   "AT_BIND_ATTR", boolean_str[AT_BIND_ATTR(at_idx)]);
	    }
#endif /* KEY Bug 14150 */
            if (ATP_PROC(at_idx) == Dummy_Proc) {
               fprintf(out_file, "  %-16s= %-7s %-16s= %-7d\n",
                     "ATP_CIF_DARG_PRO", boolean_str[ATP_CIF_DARG_PROC(at_idx)],
                     "ATP_DUMMY_PROC_L", ATP_DUMMY_PROC_LINK(at_idx));
            }

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-8s\n",
                    "ATP_ELEMENTAL", boolean_str[ATP_ELEMENTAL(at_idx)],
                    "ATP_ENTRY_LABEL_", ATP_ENTRY_LABEL_SH_IDX(at_idx),
                    "ATP_EXPL_ITRFC", boolean_str[ATP_EXPL_ITRFC(at_idx)]);

            fprintf(out_file, "  %-16s= %-7d %-16s= %-7d %-s\n",
                   "ATP_EXT_NAME_IDX", ATP_EXT_NAME_IDX(at_idx),
                   "ATP_EXT_NAME_LEN", ATP_EXT_NAME_LEN(at_idx),
                    ATP_EXT_NAME_PTR(at_idx));

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8d\n",
                    "ATP_EXTERNAL_INT",boolean_str[ATP_EXTERNAL_INTRIN(at_idx)],
                    "ATP_EXTRA_DARG", boolean_str[ATP_EXTRA_DARG(at_idx)],
                    "ATP_FIRST_IDX", ATP_FIRST_IDX(at_idx));

            if (ATP_PROC(at_idx) == Extern_Proc) {
               fprintf(out_file, "  %-16s= %-7d %-16s= %-7d\n",
                       "ATP_FIRST_SH_IDX", ATP_FIRST_SH_IDX(at_idx),
                       "ATP_GLOBAL_ATTR_", ATP_GLOBAL_ATTR_IDX(at_idx));
            }
            else {
               fprintf(out_file, "  %-16s= %-7d\n",
                       "ATP_GLOBAL_ATTR_", ATP_GLOBAL_ATTR_IDX(at_idx));
            }

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
               "ATP_HAS_ALT_RET", boolean_str[ATP_HAS_ALT_RETURN(at_idx)],
               "ATP_HAS_OVER_IND",boolean_str[ATP_HAS_OVER_INDEXING(at_idx)],
               "ATP_HAS_TASK_DIR", boolean_str[ATP_HAS_TASK_DIRS(at_idx)]);

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
               "ATP_IN_INTERFACE",boolean_str[ATP_IN_INTERFACE_BLK(at_idx)],
               "ATP_IN_UNNAMED_I",boolean_str[ATP_IN_UNNAMED_INTERFACE(at_idx)],
               "ATP_INLINE_ALWAY",boolean_str[ATP_INLINE_ALWAYS(at_idx)]);

            if (ATP_PROC(at_idx) == Intrin_Proc) {
               fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-8s\n",
                    "ATP_INLINE_NEVER",boolean_str[ATP_INLINE_NEVER(at_idx)],
                    "ATP_INTERFACE_ID", ATP_INTERFACE_IDX(at_idx),
                    "ATP_INTRIN_ENUM", intrin_str[ATP_INTRIN_ENUM(at_idx)]);

               fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8d\n",
                     "ATP_MAY_INLINE", boolean_str[ATP_MAY_INLINE(at_idx)],
                     "ATP_NAME_IN_STON", boolean_str[ATP_NAME_IN_STONE(at_idx)],
                     "ATP_NO_ENTRY_LIS", ATP_NO_ENTRY_LIST(at_idx));

               if (ATP_NO_ENTRY_LIST(at_idx) != NULL_IDX) {
                  print_al_list(out_file, ATP_NO_ENTRY_LIST(at_idx));
               }

               fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8d\n",
                    "ATP_NON_ANSI_INT",boolean_str[ATP_NON_ANSI_INTRIN(at_idx)],
                    "ATP_NOSIDE_EFFEC", boolean_str[ATP_NOSIDE_EFFECTS(at_idx)],
                    "ATP_NUM_DARGS", ATP_NUM_DARGS(at_idx));

               fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                       "ATP_OPTIONAL_DIR",boolean_str[ATP_OPTIONAL_DIR(at_idx)],
                       "ATP_PURE", boolean_str[ATP_PURE(at_idx)],
                       "ATP_RECURSIVE", boolean_str[ATP_RECURSIVE(at_idx)]);
            }
            else {
               fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                    "ATP_INLINE_NEVER",boolean_str[ATP_INLINE_NEVER(at_idx)],
                    "ATP_MAY_INLINE", boolean_str[ATP_MAY_INLINE(at_idx)],
                    "ATP_NAME_IN_STON", boolean_str[ATP_NAME_IN_STONE(at_idx)]);

               fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
                   "ATP_NO_ENTRY_LIS", ATP_NO_ENTRY_LIST(at_idx),
                   "ATP_NON_ANSI_INT",boolean_str[ATP_NON_ANSI_INTRIN(at_idx)],
                   "ATP_NOSIDE_EFFEC", boolean_str[ATP_NOSIDE_EFFECTS(at_idx)]);

               if (ATP_NO_ENTRY_LIST(at_idx) != NULL_IDX) {
                  print_al_list(out_file, ATP_NO_ENTRY_LIST(at_idx));
               }

               fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8d\n",
                       "ATP_NUM_DARGS", ATP_NUM_DARGS(at_idx),
                       "ATP_OPTIONAL_DIR",boolean_str[ATP_OPTIONAL_DIR(at_idx)],
                       "ATP_PARENT_IDX", ATP_PARENT_IDX(at_idx));

               fprintf(out_file, "  %-16s= %-7s %-16s= %-7s\n",
                       "ATP_PURE", boolean_str[ATP_PURE(at_idx)],
                       "ATP_RECURSIVE", boolean_str[ATP_RECURSIVE(at_idx)]);
            }

            if (ATP_RSLT_IDX(at_idx) != NULL_IDX) {
               fprintf(out_file, "  %-16s= %-7s %-s\n",
                       "ATP_RSLT_NAME", boolean_str[ATP_RSLT_NAME(at_idx)],
                        AT_OBJ_NAME_PTR(ATP_RSLT_IDX(at_idx)));
            }

            fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
                    "ATP_RSLT_IDX", ATP_RSLT_IDX(at_idx),
                    "ATP_SAVE_ALL", boolean_str[ATP_SAVE_ALL(at_idx)],
                    "ATP_SCP_ALIVE", boolean_str[ATP_SCP_ALIVE(at_idx)]);

            fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
                    "ATP_SCP_IDX", ATP_SCP_IDX(at_idx),
              "ATP_SGI_RTN_INL", boolean_str[ATP_SGI_ROUTINE_INLINE(at_idx)],
              "ATP_SGI_RTN_NOIN",boolean_str[ATP_SGI_ROUTINE_NOINLINE(at_idx)]);

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                "ATP_SGI_GLB_INL",boolean_str[ATP_SGI_GLOBAL_INLINE(at_idx)],
                "ATP_SGI_GLB_NOIN",boolean_str[ATP_SGI_GLOBAL_NOINLINE(at_idx)],
                "ATP_SGI_LOC_INL", boolean_str[ATP_SGI_LOCAL_INLINE(at_idx)]);

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATP_SGI_LOC_NOIN",boolean_str[ATP_SGI_LOCAL_NOINLINE(at_idx)],
                 "ATP_STACK_DIR",boolean_str[ATP_STACK_DIR(at_idx)],
                 "ATP_SYMMETRIC",boolean_str[ATP_SYMMETRIC(at_idx)]);

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                    "ATP_TASK_SHARED",boolean_str[ATP_TASK_SHARED(at_idx)],
                    "ATP_USES_EREGS",boolean_str[ATP_USES_EREGS(at_idx)],
                    "ATP_VFUNCTION",boolean_str[ATP_VFUNCTION(at_idx)]);
         }
         else {  /* MODULE */

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8d\n",
                    "ATP_ARGCHCK_ENTR", boolean_str[ATP_ARGCHCK_ENTRY(at_idx)],
                    "ATP_DCL_EXTERNAL",boolean_str[ATP_DCL_EXTERNAL(at_idx)],
                    "ATP_ENTRY_LABEL_", ATP_ENTRY_LABEL_SH_IDX(at_idx));

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                    "ATP_EXPL_ITRFC", boolean_str[ATP_EXPL_ITRFC(at_idx)],
                    "ATP_EXTERNAL_INT",boolean_str[ATP_EXTERNAL_INTRIN(at_idx)],
                    "ATP_EXTRA_DARG", boolean_str[ATP_EXTRA_DARG(at_idx)]);

            fprintf(out_file, "  %-16s= %-7d %-16s= %-7d %-s\n",
                   "ATP_EXT_NAME_IDX", ATP_EXT_NAME_IDX(at_idx),
                   "ATP_EXT_NAME_LEN", ATP_EXT_NAME_LEN(at_idx),
                    ATP_EXT_NAME_PTR(at_idx));

            fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
                "ATP_GLOBAL_ATTR_", ATP_GLOBAL_ATTR_IDX(at_idx),
                "ATP_HAS_ALT_RET", boolean_str[ATP_HAS_ALT_RETURN(at_idx)],
                "ATP_HAS_OVER_IND", boolean_str[ATP_HAS_OVER_INDEXING(at_idx)]);

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-7s\n",
                 "ATP_HAS_TASK_DIR", boolean_str[ATP_HAS_TASK_DIRS(at_idx)],
               "ATP_IMPLICIT_USE", boolean_str[ATP_IMPLICIT_USE_MODULE(at_idx)],
               "ATP_INDIRECT_MOD", boolean_str[ATP_INDIRECT_MODULE(at_idx)]);

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATP_IN_CURRENT_", boolean_str[ATP_IN_CURRENT_COMPILE(at_idx)],
                 "ATP_IN_INTERFACE", boolean_str[ATP_IN_INTERFACE_BLK(at_idx)],
                 "ATP_INLINE_ALWAY", boolean_str[ATP_INLINE_ALWAYS(at_idx)]);

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8d\n",
                   "ATP_INLINE_NEVER", boolean_str[ATP_INLINE_NEVER(at_idx)],
                   "ATP_MAY_INLINE", boolean_str[ATP_MAY_INLINE(at_idx)],
                   "ATP_MODULE_STR_I", ATP_MODULE_STR_IDX(at_idx));

            if (ATP_MOD_PATH_IDX(at_idx) != NULL_IDX) {
               fprintf(out_file,"  %-16s= %-7d %-16s= %-s\n",
                       "ATP_MOD_PATH_LEN", ATP_MOD_PATH_LEN(at_idx),
                       "ATP_MOD_PATH_IDX", ATP_MOD_PATH_NAME_PTR(at_idx));
            }

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                    "ATP_NOSIDE_EFFEC", boolean_str[ATP_NOSIDE_EFFECTS(at_idx)],
                    "ATP_RECURSIVE", boolean_str[ATP_RECURSIVE(at_idx)],
                    "ATP_RSLT_NAME", boolean_str[ATP_RSLT_NAME(at_idx)]);

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8d\n",
                    "ATP_SAVE_ALL", boolean_str[ATP_SAVE_ALL(at_idx)],
                    "ATP_SCP_ALIVE", boolean_str[ATP_SCP_ALIVE(at_idx)],
                    "ATP_SCP_IDX", ATP_SCP_IDX(at_idx));

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                    "ATP_STACK_DIR",boolean_str[ATP_STACK_DIR(at_idx)],
                    "ATP_SYSTEM_MODUL", boolean_str[ATP_SYSTEM_MODULE(at_idx)],
                    "ATP_TASK_SHARED",boolean_str[ATP_TASK_SHARED(at_idx)]);

            fprintf(out_file, "  %-16s= %-7d %-25s\n",
                    "ATP_USE_LIST", ATP_USE_LIST(at_idx),
                    use_type_str[ATP_USE_TYPE(at_idx)]);

#ifdef KEY /* Bug 5089 */
            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-7s\n",
                    "ATP_USES_EREGS",boolean_str[ATP_USES_EREGS(at_idx)],
                    "ATP_VFUNCTION",boolean_str[ATP_VFUNCTION(at_idx)],
                    "ATT_NON_INTRIN",boolean_str[ATT_NON_INTRIN(at_idx)]);
            fprintf(out_file, "  %-16s= %-7s\n", "ATT_NO_MODULE_NA",
		    boolean_str[ATT_NO_MODULE_NATURE(at_idx)]);
#else /* KEY Bug 5089 */
            fprintf(out_file, "  %-16s= %-7s %-16s= %-7s\n",
                    "ATP_USES_EREGS",boolean_str[ATP_USES_EREGS(at_idx)],
                    "ATP_VFUNCTION",boolean_str[ATP_VFUNCTION(at_idx)]);
#endif /* KEY Bug 5089 */

            if (ATP_USE_LIST(at_idx) != NULL_IDX) {
               ro_idx = ATP_USE_LIST(at_idx);

               while (ro_idx != NULL_IDX) {
                  dump_ro_ntry(out_file, ro_idx);
                  ro_idx = RO_NEXT_IDX(ro_idx);
               }
            }
         }

         fprintf(out_file, "\n");

	 if (dump_all) {

            /* Note that the output_attr flag is set to FALSE because it      */
            /* produces quite a bit of output.  The code is left in in case   */
            /* we find in future debugging sessions that also dumping the     */
            /* Attribute entry for the Secondary Name table item would be     */
            /* useful.                                                        */

            if (ATP_PGM_UNIT(at_idx) <= Subroutine) {

	       if (ATP_RSLT_IDX(at_idx) != NULL_IDX) {
                  fprintf(out_file, "\n");
                  dump_at_ntry (out_file, ATP_RSLT_IDX(at_idx), dump_all);
      	       }

               if (ATP_FIRST_IDX(at_idx) != NULL_IDX) {
                  loop_thru_sn_ntries(out_file, at_idx, FALSE);
	       }
	    }
	 }

         break;

      case Label:
         fprintf(out_file, "  %-25s %-16s= %-7s %-16s= %-8s\n",
                 atl_class_str[ATL_CLASS(at_idx)],
                 "ATL_ALIGN", boolean_str[ATL_ALIGN(at_idx)],
                 "ATL_AGGRESSIVEIN", 
                      boolean_str[ATL_AGGRESSIVEINNERLOOPFISSION(at_idx)]);


         if (ATL_CLASS(at_idx) <= Lbl_User) {
            fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8d\n",
                    "ATL_ASG_LBL_CHAI",ATL_ASG_LBL_CHAIN_START(at_idx),
                    "ATL_BL", boolean_str[ATL_BL(at_idx)],
                    "ATL_BLK_STMT_IDX", ATL_BLK_STMT_IDX(at_idx));

            fprintf(out_file, "  %-16s= %-7s %-16s= %-7d\n",
                    "ATL_CASE_LABEL", boolean_str[ATL_CASE_LABEL(at_idx)],
                    "ATL_CMIC_BLK_STM", ATL_CMIC_BLK_STMT_IDX(at_idx));
         }
         else {
            fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
                    "ATL_ASG_LBL_CHAI",ATL_ASG_LBL_CHAIN_START(at_idx),
                    "ATL_BL", boolean_str[ATL_BL(at_idx)],
                    "ATL_CASE_LABEL", boolean_str[ATL_CASE_LABEL(at_idx)]);
         }

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATL_CNCALL", boolean_str[ATL_CNCALL(at_idx)],
                 "ATL_CONSTRUCTOR_", boolean_str[ATL_CONSTRUCTOR_LOOP(at_idx)],
                 "ATL_CYCLE_LBL", boolean_str[ATL_CYCLE_LBL(at_idx)]);

         fprintf(out_file, "  %-16s= %-33s %-16s= %-8d\n",
                "ATL_DEBUG_CLASS", atl_debug_class_str[ATL_DEBUG_CLASS(at_idx)],
                "ATL_DIRECTIVE_LI", ATL_DIRECTIVE_LIST(at_idx));

         if (ATL_DIRECTIVE_LIST(at_idx) != NULL_IDX) {
            il_idx = IL_IDX(ATL_DIRECTIVE_LIST(at_idx)) + Safevl_Dir_Idx;

            fprintf(out_file, "  %-16s= %-7s", "safevl",
                    (IL_FLD(il_idx) == CN_Tbl_Idx) ? convert_to_string(
                                                   &CN_CONST(IL_IDX(il_idx)),
                                                    CN_TYPE_IDX(IL_IDX(il_idx)),
                                                    conv_str) : "0");

            il_idx = IL_IDX(ATL_DIRECTIVE_LIST(at_idx)) + Unroll_Dir_Idx;

            fprintf(out_file, "  %-16s= %-7s", "unroll",
                    (IL_FLD(il_idx) == CN_Tbl_Idx) ? convert_to_string(
                                                    &CN_CONST(IL_IDX(il_idx)),
                                                    CN_TYPE_IDX(IL_IDX(il_idx)),
                                                    conv_str) : "0");

            il_idx = IL_IDX(ATL_DIRECTIVE_LIST(at_idx)) + Mark_Dir_Idx;

            fprintf(out_file, " %-16s= %-s\n", "mark",
                    (IL_FLD(il_idx) == CN_Tbl_Idx) ?
                                      (char *) &CN_CONST(IL_IDX(il_idx)) : " ");

            il_idx = IL_IDX(ATL_DIRECTIVE_LIST(at_idx)) + Maxcpus_Dir_Idx;

            fprintf(out_file, "  %-16s= %-7d %-16s= %-25s\n", 
                    "maxcpus idx", IL_IDX(il_idx),
                    "maxcpus fld", field_str[IL_FLD(il_idx)]);

            il_idx = IL_IDX(ATL_DIRECTIVE_LIST(at_idx)) + Cache_Bypass_Dir_Idx;

            if (IL_FLD(il_idx) == IL_Tbl_Idx) {  /* Cache bypass */
               il_idx                 = IL_IDX(il_idx);

               while (il_idx != NULL_IDX) {
                  fprintf(out_file, "  %-16s= %-25s \n", 
                  "cache_bypass", AT_OBJ_NAME_PTR(IL_IDX(il_idx)));
                  il_idx = IL_NEXT_LIST_IDX(il_idx);
               }
            }
         }

         if (AT_DEFINED(at_idx)) {
            fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
                    "ATL_DEF_STMT_IDX", ATL_DEF_STMT_IDX(at_idx),
                    "ATL_EXECUTABLE", boolean_str[ATL_EXECUTABLE(at_idx)],
                    "ATL_FISSIONABLE", boolean_str[ATL_FISSIONABLE(at_idx)]);
         }
         else {
            fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-8s\n",
                    "ATL_EXECUTABLE", boolean_str[ATL_EXECUTABLE(at_idx)],
                    "ATL_FWD_REF_IDX", ATL_FWD_REF_IDX(at_idx),
                    "ATL_FISSIONABLE", boolean_str[ATL_FISSIONABLE(at_idx)]);
         }

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-7s\n",
                 "ATL_FUSABLE", boolean_str[ATL_FUSABLE(at_idx)],
                 "ATL_FUSION", boolean_str[ATL_FUSION(at_idx)],
                 "ATL_IN_ASSIGN", boolean_str[ATL_IN_ASSIGN(at_idx)]);

         if (ATL_CLASS(at_idx) == Lbl_Format) {

            if (ATL_FORMAT_TMP(at_idx) == NULL_IDX)  {
                fprintf(out_file,"  %-16s= %-7d\n",
                        "ATL_FORMAT_TM", ATL_FORMAT_TMP(at_idx));
            }
            else {
                fprintf(out_file,"  %-16s= %-7d %-16s= \"%s\"\n\n",
                        "ATL_FORMAT_TM", ATL_FORMAT_TMP(at_idx),
                        "FORMAT CONSTANT",
                        (char *)&CN_CONST(ATD_TMP_IDX(ATL_FORMAT_TMP(at_idx))));
            }
         }

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                "ATL_IN_ASSIGN_LB",boolean_str[ATL_IN_ASSIGN_LBL_CHAIN(at_idx)],
                "ATL_INFORM_ONLY",boolean_str[ATL_INFORM_ONLY(at_idx)],
                "ATL_IVDEP",boolean_str[ATL_IVDEP(at_idx)]);

         if (ATL_CLASS(at_idx) == Lbl_Internal) {
            fprintf(out_file, "  %-16s= %-7d\n",
                   "ATL_NEW_LBL_IDX", ATL_NEW_LBL_IDX(at_idx));
         }

         fprintf(out_file, "  %-16s= %-7s %-16s  %-7d %-16s= %-8s\n",
                "ATL_MAXCPUS",boolean_str[ATL_MAXCPUS(at_idx)],
                "ATL_NEXT_ASG_LBL", ATL_NEXT_ASG_LBL_IDX(at_idx),
                "ATL_NEXTSCALAR", boolean_str[ATL_NEXTSCALAR(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                "ATL_NOBLOCKING", boolean_str[ATL_NOBLOCKING(at_idx)],
                "ATL_NOFISSION", boolean_str[ATL_NOFISSION(at_idx)],
                "ATL_NOFUSION", boolean_str[ATL_NOFUSION(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                "ATL_NOINTERCHANG", boolean_str[ATL_NOINTERCHANGE(at_idx)],
                "ATL_NORECURRENCE", boolean_str[ATL_NORECURRENCE(at_idx)],
		"ATL_NOTASK",boolean_str[ATL_NOTASK(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                "ATL_NOVECTOR", boolean_str[ATL_NOVECTOR(at_idx)],
                "ATL_NOVSEARCH", boolean_str[ATL_NOVSEARCH(at_idx)],
                "ATL_PATTERN", boolean_str[ATL_PATTERN(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
              "ATL_PERMUTATION", boolean_str[ATL_PERMUTATION(at_idx)],
              "ATL_PREFERSTREAM", boolean_str[ATL_PREFERSTREAM(at_idx)],
              "ATL_PREFER_NOCIN", boolean_str[ATL_PREFERSTREAM_NOCINV(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s\n",
                "ATL_PREFERTASK", boolean_str[ATL_PREFERTASK(at_idx)],
                "ATL_PREFERVECTOR", boolean_str[ATL_PREFERVECTOR(at_idx)]);

         if (ATL_CLASS(at_idx) == Lbl_Format) {
            fprintf(out_file, " %-16s= %-7d\n",
                    "ATL_PP_FORMAT_TM", ATL_PP_FORMAT_TMP(at_idx));
         }

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATL_SHORTLOOP", boolean_str[ATL_SHORTLOOP(at_idx)],
                 "ATL_SHORTLOOP128", boolean_str[ATL_SHORTLOOP128(at_idx)],
                 "ATL_SPLIT", boolean_str[ATL_SPLIT(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATL_STREAM",boolean_str[ATL_STREAM(at_idx)],
                 "ATL_TOP_OF_LOOP", boolean_str[ATL_TOP_OF_LOOP(at_idx)],
                 "ATL_UNROLL_DIR", boolean_str[ATL_UNROLL_DIR(at_idx)]);

         break;

      case Derived_Type:

#ifdef KEY /* Bug 14150 */
         fprintf(out_file, "  %-16s= %-7s\n",
                 "AT_BIND_ATTR", boolean_str[AT_BIND_ATTR(at_idx)]);
#endif /* KEY Bug 14150 */

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATT_CHAR_CPNT", boolean_str[ATT_CHAR_CPNT(at_idx)],
                 "ATT_CHAR_SEQ", boolean_str[ATT_CHAR_SEQ(at_idx)],
                 "ATT_ALIGNMENT", align_str[ATT_ALIGNMENT(at_idx)]);

         fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
                 "ATT_CIF_DT_ID", ATT_CIF_DT_ID(at_idx),
                 "ATT_DALIGN_ME", boolean_str[ATT_DALIGN_ME(at_idx)],
                 "ATT_DCL_NUMERIC_", boolean_str[ATT_DCL_NUMERIC_SEQ(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-8d\n",
               "ATT_DEFAULT_INIT", boolean_str[ATT_DEFAULT_INITIALIZED(at_idx)],
               "ATT_FIRST_CPNT_I", ATT_FIRST_CPNT_IDX(at_idx),
               "ATT_GLOBAL_TYPE_", ATT_GLOBAL_TYPE_IDX(at_idx));

         fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8d\n",
                 "ATT_LABEL_LIST_I", ATT_LABEL_LIST_IDX(at_idx),
                 "ATT_NON_DEFAULT_", boolean_str[ATT_NON_DEFAULT_CPNT(at_idx)],
                 "ATT_NUM_CPNTS", ATT_NUM_CPNTS(at_idx));

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATT_NUMERIC_CPNT", boolean_str[ATT_NUMERIC_CPNT(at_idx)],
                 "ATT_POINTER_CPNT", boolean_str[ATT_POINTER_CPNT(at_idx)],
                 "ATT_PRIVATE_CPNT", boolean_str[ATT_PRIVATE_CPNT(at_idx)]);

#ifdef KEY /* Bug 6845 */
         fprintf(out_file, "  %-16s= %-7s\n",
                 "ATT_ALLOCAT_CPNT", boolean_str[ATT_ALLOCATABLE_CPNT(at_idx)]);
#endif /* KEY Bug 6845 */

         fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8d\n",
                 "ATT_SCP_IDX", ATT_SCP_IDX(at_idx),
                 "ATT_SEQUENCE_SET", boolean_str[ATT_SEQUENCE_SET(at_idx)],
                 "ATT_UNIQUE_ID", ATT_UNIQUE_ID(at_idx));

         if (ATT_STRUCT_BIT_LEN_IDX(at_idx) != NULL_IDX) {
            sprintf(str, "(%10s)",
                 convert_to_string(&CN_CONST(ATT_STRUCT_BIT_LEN_IDX(at_idx)),
                                    CN_TYPE_IDX(ATT_STRUCT_BIT_LEN_IDX(at_idx)),
                                    conv_str));
         }
         else {
            sprintf(str,"%12s", " ");
         }

         fprintf(out_file, "  %-16s= %-7d %-25s %-26s\n",
                 "ATT_STRUCT_SIZE", ATT_STRUCT_BIT_LEN_IDX(at_idx),
                 field_str[CN_Tbl_Idx],
                 str);

         /* Note that the output_attr flag is set to FALSE because it   */
         /* produces quite a bit of output.  The code is left in in     */
         /* case we find in future debugging sessions that also dumping */
         /* the Attribute entry for the Secondary Name table item       */
         /* would be useful.                                            */

         if (dump_all) {
            chain_thru_sn_ntries(out_file, at_idx, FALSE);
         }

         break;


      case Interface:

         fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
                "ATI_CIF_SCOPE_ID", ATI_CIF_SCOPE_ID(at_idx),
                "ATI_CIF_SEEN_IN_", boolean_str[ATI_CIF_SEEN_IN_CALL(at_idx)],
                "ATI_DCL_INTRINSI", boolean_str[ATI_DCL_INTRINSIC(at_idx)]);

         fprintf(out_file, "  %-16s= %-33s %-16s= %-8d\n",
                "ATI_DEFINED_OPR", operator_str[ATI_DEFINED_OPR(at_idx)],
                "ATI_FIRST_SPECIF", ATI_FIRST_SPECIFIC_IDX(at_idx));

         fprintf(out_file, "  %-16s= %-7s %-16s= %-33s\n",
                "ATI_HAS_NON_MOD_", boolean_str[ATI_HAS_NON_MOD_PROC(at_idx)],
                "ATI_INTERFACE_CL", interface_str[ATI_INTERFACE_CLASS(at_idx)]);

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                 "ATI_INLINE_ALWAY",boolean_str[ATI_INLINE_ALWAYS(at_idx)],
                 "ATI_INLINE_NEVER",boolean_str[ATI_INLINE_NEVER(at_idx)],
                 "ATI_INTRIN_PASSA", boolean_str[ATI_INTRIN_PASSABLE(at_idx)]);

         fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8d\n",
                 "ATI_INTRIN_TBL_I", ATI_INTRIN_TBL_IDX(at_idx),
                 "ATI_IPA_DIR_SPEC",boolean_str[ATI_IPA_DIR_SPECIFIED(at_idx)],
                 "ATI_NUM_SPECIFIC", ATI_NUM_SPECIFICS(at_idx));

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7d\n",
                "ATI_GENERIC_INT", boolean_str[ATI_GENERIC_INTRINSIC(at_idx)],
                "ATI_PROC_IDX", ATI_PROC_IDX(at_idx));

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
             "ATI_UNNAMED_INTE", boolean_str[ATI_UNNAMED_INTERFACE(at_idx)],
             "ATI_SGI_RTN_INLI", boolean_str[ATI_SGI_ROUTINE_INLINE(at_idx)],
             "ATI_SGI_RTN_NOIN", boolean_str[ATI_SGI_ROUTINE_NOINLINE(at_idx)]);
 
         if (ATD_TYPE_IDX(at_idx) != NULL_IDX) {
            fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-s\n",
                   "ATI_USER_SPECIFI", boolean_str[ATI_USER_SPECIFIED(at_idx)],
                   "ATD_TYPE_IDX", ATD_TYPE_IDX(at_idx),
                    print_type_f(ATD_TYPE_IDX(at_idx)));
         }
         else {
            fprintf(out_file, "  %-16s= %-7s %-16s= %-7d\n",
                   "ATI_USER_SPECIFI", boolean_str[ATI_USER_SPECIFIED(at_idx)],
                   "ATD_TYPE_IDX", ATD_TYPE_IDX(at_idx));
         }

         if (ATI_PROC_IDX(at_idx) != NULL_IDX) {
            fprintf(out_file, "\n");
            dump_at_ntry(out_file, ATI_PROC_IDX(at_idx), dump_all);
         }

         /* Note that the output_attr flag is set to FALSE because it   */
         /* produces quite a bit of output.  The code is left in in     */
         /* case we find in future debugging sessions that also dumping */
         /* the Attribute entry for the Secondary Name table item would */
         /* be useful.                                                  */

         if (dump_all) {
            chain_thru_sn_ntries(out_file, at_idx, FALSE);
         }

         break;


      case Namelist_Grp:

         fprintf(out_file, "  %-16s= %-7d %-16s= %-7d %-16s= %-8d\n",
                 "ATN_FIRST_NAMELI", ATN_FIRST_NAMELIST_IDX(at_idx),
                 "ATN_LAST_NAMELIS", ATN_LAST_NAMELIST_IDX(at_idx),
                 "ATN_NUM_NAMELIST", ATN_NUM_NAMELIST(at_idx));

         fprintf(out_file, "  %-16s= %-7d (%-s)\n",
                 "ATN_NAMELIST_DES", ATN_NAMELIST_DESC(at_idx),
                 ((ATN_NAMELIST_DESC(at_idx) == NULL_IDX) ? " " :
                  AT_OBJ_NAME_PTR(ATN_NAMELIST_DESC(at_idx))));


         /* Note that the output_attr flag is set to FALSE because it   */
         /* produces quite a bit of output.  The code is left in in     */
         /* case we find in future debugging sessions that also dumping */
         /* the Attribute entry for the Secondary Name table item would */
         /* be useful.                                                  */

         if (dump_all) {
            chain_thru_sn_ntries(out_file, at_idx, FALSE);
         }
         break;


      case Stmt_Func:

         fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-s\n",
                 "ATS_SF_ACTIVE", boolean_str[ATS_SF_ACTIVE(at_idx)],
                 "ATP_FIRST_IDX", ATP_FIRST_IDX(at_idx),
                 field_str[ATS_SF_FLD(at_idx)]);

         fprintf(out_file, "  %-16s= %-7d %-16s= %-7d %-16s= %-8s\n",
                "ATS_SF_IDX", ATS_SF_IDX(at_idx),
                "ATP_NUM_DARGS", ATP_NUM_DARGS(at_idx),
                "ATS_SF_SEMANTICS", boolean_str[ATS_SF_SEMANTICS_DONE(at_idx)]);

         fprintf(out_file, "  %-16s= %-7d %-s\n",
                           "ATD_TYPE_IDX", ATD_TYPE_IDX(at_idx),
                            print_type_f(ATD_TYPE_IDX(at_idx)));

         if (dump_all && ATP_FIRST_IDX(at_idx) != NULL_IDX) {
            loop_thru_sn_ntries(out_file, at_idx, FALSE);
         }

         if (ATS_SF_FLD(at_idx) == IR_Tbl_Idx) {
            dump_ir_ntry(out_file, ATS_SF_IDX(at_idx), 5);
         }

         break;

   }  /* End switch */

   putc ('\n', out_file);
   fflush (out_file);

   return;

}  /* dump_at_ntry */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints one bounds table entry to the specified output file.           *|
|*									      *|
|* Input parameters:							      *|
|*	FILE to print to - Should be debug_file, stderr, or stdout.           *|
|*	Index of bounds entry to print.                                       *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void dump_bd_ntry (FILE	*out_file,
   		          int	 bd_idx)

{
   int		i;


   if (bd_idx > bounds_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* BD index value [%d] is out of range.\n",
              bd_idx);
      goto EXIT;
   }

   if (BD_DIST_NTRY(bd_idx)) {
      fprintf(out_file, "  %-16s= %-7d %-16s= %-7d %-16s= %-8s\n",
                        "BD_RANK", BD_RANK(bd_idx),
                        "BD_COLUMN_NUM", BD_COLUMN_NUM(bd_idx),
                        "BD_DISTRIBUTE_RE",
                            boolean_str[BD_DISTRIBUTE_RESHAPE(bd_idx)]);

      fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8d\n",
                        "BD_LINE_NUM", BD_LINE_NUM(bd_idx),
                        "BD_RESOLVED", boolean_str[BD_RESOLVED(bd_idx)],
                        "IDX", bd_idx);

      for (i = 1; i <=  BD_RANK(bd_idx); i++) {
         fprintf(out_file, "  %-16s= %-7d %-16s= %-25s\n",
                           "Dimension", i,
                           "Distribution", 
                              distribution_str[BD_DISTRIBUTION(bd_idx,i)]);

         if (BD_CYCLIC_FLD(bd_idx, i) != NO_Tbl_Idx) {
            print_fld_idx(out_file,
                          "   BD_CYCLIC_IDX",
                           BD_CYCLIC_FLD(bd_idx, i),
                           BD_CYCLIC_IDX(bd_idx, i));
         }

         if (BD_ONTO_FLD(bd_idx, i) != NO_Tbl_Idx) {
            print_fld_idx(out_file,
                          "   BD_ONTO_IDX",
                           BD_ONTO_FLD(bd_idx, i),
                           BD_ONTO_IDX(bd_idx, i));
         }
      }
      goto EXIT;
   }

   if (!BD_USED_NTRY(bd_idx)) {
      fprintf(out_file, "  %-16s= %-7d %-16s= %-7d %-16s= %-8d\n",
                        "IDX", bd_idx, 
                        "BD_NEXT_FREE_NTR", BD_NEXT_FREE_NTRY(bd_idx),
                        "BD_NTRY_SIZE", BD_NTRY_SIZE(bd_idx));
      goto EXIT;
   }

   fprintf(out_file, "  %-16s= %-7d %-25s %-26s\n",
                     "BD_RANK", BD_RANK(bd_idx),
                                bd_array_class_str[BD_ARRAY_CLASS(bd_idx)],
                                bd_array_size_str[BD_ARRAY_SIZE(bd_idx)]);

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8d\n",
                     "BD_COLUMN_NUM", BD_COLUMN_NUM(bd_idx),
                     "BD_DCL_ERR", boolean_str[BD_DCL_ERR(bd_idx)],
                     "BD_GLOBAL_IDX", BD_GLOBAL_IDX(bd_idx));

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8d\n",
                     "BD_LINE_NUM", BD_LINE_NUM(bd_idx),
                     "BD_RESOLVED", boolean_str[BD_RESOLVED(bd_idx)],
                     "IDX", bd_idx);

   print_fld_idx(out_file, "BD_LEN_IDX",
                 BD_LEN_FLD(bd_idx),
                 BD_LEN_IDX(bd_idx));

   if (BD_ARRAY_CLASS(bd_idx) != Deferred_Shape) {

      for (i = 1; i <=  BD_RANK(bd_idx); i++) {
         fprintf(out_file, "  %-16s= %-7d\n", "Dimension", i);

         print_fld_idx(out_file,
                       "   BD_LB_IDX",
                        BD_LB_FLD(bd_idx, i),
                        BD_LB_IDX(bd_idx, i));

         print_fld_idx(out_file,
                       "   BD_UB_IDX",
                        BD_UB_FLD(bd_idx, i),
                        BD_UB_IDX(bd_idx, i));

         print_fld_idx(out_file,
                       "   BD_XT_IDX",
                        BD_XT_FLD(bd_idx, i),
                        BD_XT_IDX(bd_idx, i));

         print_fld_idx(out_file,
                       "   BD_SM_IDX",
                        BD_SM_FLD(bd_idx, i),
                        BD_SM_IDX(bd_idx, i));
      }
   }

   EXIT:

   putc ('\n', out_file);

   fflush (out_file);
   return;

}  /* dump_bd_ntry */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints the actual block entry.                                        *|
|*									      *|
|* Input parameters:							      *|
|*	File to print entry to.       	 				      *|
|*	Index of block entry to print.					      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void dump_blk_ntry(FILE	*out_file,
   		          int 	 blk_idx)
{

   /* Sometimes when debugging, you really want to print a Block Stack        */
   /* entry that's been popped.  But print a warning in case that's not what  */
   /* was intended.							      */

   if (blk_idx > blk_stk_idx) {
      fprintf(stderr,
              "\n*FE90-WARNING* Blk index value [%d] is out of range.\n",
              blk_idx);
   }


   fprintf(out_file,"\n%-32.32s  ", blk_struct_str[BLK_TYPE(blk_idx)]);
   fprintf(out_file,"%-8s= %-7d", "IDX", blk_idx);

   if (blk_idx == blk_stk_idx) {
      fprintf(out_file,"%4s%-20.20s\n", " ", "CURRENT BLOCK");
      fprintf(out_file,"%4s%-19s= %-27s\n", " ", "curr_stmt_category",
                                          context_str[curr_stmt_category]);
   }
   else {
      fprintf(out_file,"\n");
   }

   if (BLK_NAME(blk_idx) != NULL_IDX) {
      fprintf(out_file,"%4s%-19s= (%d)  %-32.32s\n", " ",
              "BLK_NAME", BLK_NAME(blk_idx),AT_OBJ_NAME_PTR(BLK_NAME(blk_idx)));
   }

   fprintf(out_file, "%4s%-19s= %-29d  %-16s= %-7d\n", " ",
                     "BLK_DEF_LINE",   BLK_DEF_LINE(blk_idx),
                     "BLK_DEF_COLUMN", BLK_DEF_COLUMN(blk_idx));

   fprintf(out_file, "%4s%-19s= %-29d  %-16s= %-7d\n", " ",
                     "BLK_FIRST_SH_IDX", BLK_FIRST_SH_IDX(blk_idx),
                     "BLK_LABEL",        BLK_LABEL(blk_idx));

   fprintf(out_file, "%4s%-19s= %s     %-16s= %s      %-16s= %s\n", " ",
                     "BLK_ERR",          boolean_str[BLK_ERR(blk_idx)],
                     "BLK_FND_DEFAULT",  boolean_str[BLK_FND_DEFAULT(blk_idx)],
                     "BLK_NO_EXEC",      boolean_str[BLK_NO_EXEC(blk_idx)]);

   if (BLK_TYPE(blk_idx) == Do_Blk) {

      if (BLK_DO_TYPE(blk_idx) == Iterative_Loop) {
         fprintf(out_file, "%4sDO-var: Line= %-7d  Col= %-3d  Fld= %-10s\n",
                           " ",
                           BLK_DO_VAR_LINE_NUM(blk_idx),
                           BLK_DO_VAR_COL_NUM(blk_idx),
                           field_str[BLK_DO_VAR_FLD(blk_idx)]);

         print_attr_name(out_file, BLK_DO_VAR_IDX(blk_idx), 4);
      }
      else if (BLK_DO_TYPE(blk_idx) == While_Loop) {
         fprintf(out_file, "%4sWHILE expr: Line= %-7d  Col= %-3d  Fld= %-10s",
                           " ", 
                           BLK_DO_VAR_LINE_NUM(blk_idx),
                           BLK_DO_VAR_COL_NUM(blk_idx),
                           field_str[BLK_DO_VAR_FLD(blk_idx)]);

         if (BLK_DO_VAR_FLD(blk_idx) == AT_Tbl_Idx) {
            fputc('\n', out_file);
            print_attr_name(out_file, BLK_DO_VAR_IDX(blk_idx), 4);
         }
         else {
            fprintf(out_file, "  Idx= %d\n", BLK_DO_VAR_IDX(blk_idx));
         }
      }

      fprintf(out_file, "%4s%-18s= %-27s  %-16s= %-7d\n", " ",
                        "BLK_DO_TYPE",	   do_type_str[BLK_DO_TYPE(blk_idx)],
                        "BLK_LOOP_NUM",    BLK_LOOP_NUM(blk_idx));

      fprintf(out_file, "%4s%-18s= %-27s  %-16s= %s\n", " ",
                        "BLK_CYCLE_STMT",  boolean_str[BLK_CYCLE_STMT(blk_idx)],
                        "BLK_EXIT_STMT",   boolean_str[BLK_EXIT_STMT(blk_idx)]);
                        
      fprintf(out_file, "%4s%-18s= %-27d  %-16s= %-12d\n", " ",
                        "BLK_TOP_LBL_IDX",    BLK_TOP_LBL_IDX(blk_idx),
                        "BLK_SKIP_LBL_IDX",   BLK_SKIP_LBL_IDX(blk_idx));

      fprintf(out_file, "%4s%-19s= %s\n", " ",
                        "BLK_IS_PARALLEL_REG",     
                        boolean_str[BLK_IS_PARALLEL_REGION(blk_idx)]);

      if (BLK_DO_TYPE(blk_idx) == Iterative_Loop) { 
         fprintf(out_file, "%4s%-18s= %-27d  %-16s= %-12d\n", " ",
                           "BLK_START_TEMP_IDX", BLK_START_TEMP_IDX(blk_idx),
                           "BLK_INC_TEMP_IDX",   BLK_INC_TEMP_IDX(blk_idx)); 

         fprintf(out_file, "%4s%-18s= %-27d  %-16s= %-12d\n", " ",
                           "BLK_INDUC_TEMP_IDX", BLK_INDUC_TEMP_IDX(blk_idx),
                           "BLK_TC_TEMP_IDX",    BLK_TC_TEMP_IDX(blk_idx));

         fprintf(out_file, "%4s%-26s= %-14s  %-24s= %-9s\n", " ",
                           "BLK_HAS_NESTED_LOOP",
			      boolean_str[BLK_HAS_NESTED_LOOP(blk_idx)],
                           "BLK_BLOCKABLE_NEST_OK ",
			      boolean_str[BLK_BLOCKABLE_NEST_OK(blk_idx)]);
                           
         fprintf(out_file, "%4s%-26s= %-14d  %-24s= %-9d\n", " ",
                           "BLK_BLOCKABLE_DIR_SH_IDX",
                              BLK_BLOCKABLE_DIR_SH_IDX(blk_idx),
                           "BLK_BLOCKABLE_NUM_LCVS",
                              BLK_BLOCKABLE_NUM_LCVS(blk_idx));
                           
         fprintf(out_file, "%4s%-26s= %-14d  %-24s= %-9d\n", " ",
                           "BLK_INTERCHANGE_DIR_SH_IDX",
                              BLK_INTERCHANGE_DIR_SH_IDX(blk_idx),
                           "BLK_INTERCHANGE_NUM_LCVS",
                              BLK_INTERCHANGE_NUM_LCVS(blk_idx));
                           
         fprintf(out_file, "%4s%-26s= %-14d  %-24s= %-9d\n", " ",
                           "BLK_DIR_NEST_CHECK_SH_IDX",
                              BLK_DIR_NEST_CHECK_SH_IDX(blk_idx),
                           "BLK_DIR_NEST_CHECK_NUM_LCVS",
                              BLK_DIR_NEST_CHECK_NUM_LCVS(blk_idx));
      }
   }
   else if (BLK_TYPE(blk_idx) == Select_Blk) {
      fprintf(out_file, "%4s%-19s= %-29d\n", " ",
                        "BLK_NUM_CASES", BLK_NUM_CASES(blk_idx));

      fprintf(out_file, "%4s%s%-9d   %s%-3d   %s   %s%-7d\n", " ",
                        "BLK_CASE_DEFAULT_LBL_OPND:  line = ", 
                        BLK_CASE_DEFAULT_LBL_LINE_NUM(blk_idx),
                        "col = ",  BLK_CASE_DEFAULT_LBL_COL_NUM(blk_idx),
                        field_str[BLK_CASE_DEFAULT_LBL_FLD(blk_idx)],
                        "idx = ", BLK_CASE_DEFAULT_LBL_IDX(blk_idx));
   }
   else if (cif_flags & BASIC_RECS) {
      fprintf(out_file, "%4s%-19s= %d\n", " ",
                        "BLK_CIF_SCOPE_ID", BLK_CIF_SCOPE_ID(blk_idx));
   }

   if (BLK_TYPE(blk_idx) == Derived_Type_Blk) {
      fprintf(out_file, "%4s%-18s= %-27d  \n", " ",
                        "BLK_LAST_CPNT_IDX",   BLK_LAST_CPNT_IDX(blk_idx));
   }
   else if (BLK_TYPE(blk_idx) == Interface_Body_Blk) {
      fprintf(out_file, "%4s%-18s= %-27d  %-16s= %-12d\n", " ",
                        "BLK_AT_IDX",   BLK_AT_IDX(blk_idx),
                        "BLK_BD_IDX",   BLK_BD_IDX(blk_idx));
      fprintf(out_file, "%4s%-18s= %-27d  %-16s= %-12d\n", " ",
                        "BLK_CN_IDX",   BLK_CN_IDX(blk_idx),
                        "BLK_CP_IDX",   BLK_CP_IDX(blk_idx));
      fprintf(out_file, "%4s%-18s= %-27d  %-16s= %-12d\n", " ",
                        "BLK_NP_IDX",   BLK_NP_IDX(blk_idx),
                        "BLK_SB_IDX",   BLK_SB_IDX(blk_idx));
      fprintf(out_file, "%4s%-18s= %-27d  %-16s= %-12d\n", " ",
                        "BLK_SN_IDX",   BLK_SN_IDX(blk_idx),
                        "BLK_TYP_IDX",   BLK_TYP_IDX(blk_idx));
   }

   fflush (out_file);

   return;

}  /* dump_blk_ntry */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints one constant table entry to the specified output file.         *|
|*									      *|
|* Input parameters:							      *|
|*	FILE to print to - Should be debug_file, stderr, or stdout.           *|
|*	Index of constant entry to print.                                     *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void dump_cn_ntry (FILE	*out_file,
   		          int	 cn_idx)

{
   int		type_idx;


   if (cn_idx > const_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* CN index value [%d] is out of range.\n",
              cn_idx);
      return;
   }

   type_idx = CN_TYPE_IDX(cn_idx);

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8d\n",
                   "CN_BOOLEAN_CONST", boolean_str[CN_BOOLEAN_CONSTANT(cn_idx)],
                   "CN_BOZ_CONSTANT", boolean_str[CN_BOZ_CONSTANT(cn_idx)],
                   "IDX", cn_idx);
                     
   fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s\n",
               "CN_EXTRA_ZERO_WO", boolean_str[CN_EXTRA_ZERO_WORD(cn_idx)],
               "CN_POOL_IDX", CN_POOL_IDX(cn_idx),
                cn_hollerith_str[CN_HOLLERITH_TYPE(cn_idx)]);

# if defined(_TARGET_LITTLE_ENDIAN)
   fprintf(out_file, "  %-16s= %-2s\n",
               "CN_HOLLERITH_END", boolean_str[CN_HOLLERITH_ENDIAN(cn_idx)]);
# endif

   fprintf(out_file, "  %s%d] = %-s\n",
                     "CN_TYPE_IDX[", CN_TYPE_IDX(cn_idx),
                     print_type_f(type_idx));

   /* The old call was     dump_typ_ntry(out_file, type_idx); */

   print_const_f(out_file, cn_idx);
   fprintf(out_file, "\n\n");
   fflush (out_file);

   return;

}  /* dump_cn_ntry */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Prints a single Equivalence table entry.                              *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      Index of Equivalence table entry to print.                            *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

static void dump_eq_ntry (FILE  *out_file,
                          int    eq_idx)

{

   if (eq_idx > equiv_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* EQ index value [%d] is out of range.\n",
              eq_idx);
      return;
   }

   fprintf(out_file, "%-53.53s %-16s= %-8d\n",
                      AT_OBJ_NAME_PTR(EQ_ATTR_IDX(eq_idx)),
                     "IDX", eq_idx);

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7d %-16s= %-8s\n",
                     "EQ_ATTR_IDX", EQ_ATTR_IDX(eq_idx),
                     "EQ_COLUMN_NUM", EQ_COLUMN_NUM(eq_idx),
                     "EQ_DALIGN_ME", boolean_str[EQ_DALIGN_ME(eq_idx)]);

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                     "EQ_DALIGN_SHIFT", boolean_str[EQ_DALIGN_SHIFT(eq_idx)],
                     "EQ_DO_NOT_DALIGN", boolean_str[EQ_DO_NOT_DALIGN(eq_idx)],
                     "EQ_ERROR", boolean_str[EQ_ERROR(eq_idx)]);

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7d %-16s= %-8d\n",
                     "EQ_GRP_END_IDX", EQ_GRP_END_IDX(eq_idx),
                     "EQ_GRP_IDX", EQ_GRP_IDX(eq_idx),
                     "EQ_LINE_NUM", EQ_LINE_NUM(eq_idx));


   fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8d\n",
                     "EQ_LIST_IDX", EQ_LIST_IDX(eq_idx),
                     "EQ_MERGED", boolean_str[EQ_MERGED(eq_idx)],
                     "EQ_NEXT_EQUIV_GR", EQ_NEXT_EQUIV_GRP(eq_idx));

   print_fld_idx(out_file, "EQ_OFFSET_IDX",
                 EQ_OFFSET_FLD(eq_idx),
                 EQ_OFFSET_IDX(eq_idx));

   print_fld_idx(out_file, "EQ_OPND_IDX",
                 EQ_OPND_FLD(eq_idx),
                 EQ_OPND_IDX(eq_idx));

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
                     "EQ_NEXT_EQUIV_OB", EQ_NEXT_EQUIV_OBJ(eq_idx),
                     "EQ_SEARCH_DONE", boolean_str[EQ_SEARCH_DONE(eq_idx)],
                     "EQ_SUBSTRINGED", boolean_str[EQ_SUBSTRINGED(eq_idx)]);

   putc ('\n', out_file);
   fflush(out_file);

   return;

}  /* dump_eq_ntry */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*									      *|
|* Input parameters:							      *|
|*	FILE to print to - Should be debug_file, stderr, or stdout.           *|
|*	Index of file path table entry to print.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void dump_fp_ntry(FILE	*out_file,
   		         int	 fp_idx,
			 boolean print_list)
 
{

   if (fp_idx > file_path_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* FP index value [%d] is out of range.\n",
              fp_idx);
      return;
   }

   do {
      fprintf(out_file, "%-s\n\n", FP_NAME_PTR(fp_idx));

      fprintf(out_file, "  %-16s= %-6d  %-16s= %-s\n",
              "IDX", fp_idx,
              "FP_CLASS", file_path_str[FP_CLASS(fp_idx)]);

      if (FP_FILE_IDX(fp_idx) != NULL_IDX &&
           FP_NAME_IDX(FP_FILE_IDX(fp_idx)) != NULL_IDX) {
         fprintf(out_file, "  %-16s= %-s\n",
                 "FP_FILE_IDX", FP_NAME_PTR(FP_FILE_IDX(fp_idx)));
      }
      else {
         fprintf(out_file, "  %-16s= %-s\n", "FP_FILE_IDX", "0");
      }

      if (FP_MODULE_IDX(fp_idx) != NULL_IDX &&
           FP_NAME_IDX(FP_MODULE_IDX(fp_idx)) != NULL_IDX) {
         fprintf(out_file, "  %-16s= %-s\n",
                 "FP_MODULE_IDX", FP_NAME_PTR(FP_MODULE_IDX(fp_idx)));
      }
      else {
         fprintf(out_file, "  %-16s= %-s\n", "FP_MODULE_IDX", "0");
      }

      if (FP_NEXT_FILE_IDX(fp_idx) != NULL_IDX &&
           FP_NAME_IDX(FP_NEXT_FILE_IDX(fp_idx)) != NULL_IDX) {
         fprintf(out_file, "  %-16s= %-6d  (%-s)\n",
                 "FP_NEXT_FILE_IDX", FP_NEXT_FILE_IDX(fp_idx),
                                     FP_NAME_PTR(FP_NEXT_FILE_IDX(fp_idx)));
      }
      else {
         fprintf(out_file, "  %-16s= %-s\n", "FP_NEXT_FILE_IDX", "0");
      }

      fprintf(out_file, "  %-16s= %-6d  %-16s= %-7d  %-16s= %-7s\n",
              "FP_MODULE_INLINE", FP_MODULE_INLINE_IDX(fp_idx),
              "FP_NAME_LEN", FP_NAME_LEN(fp_idx),
              "FP_OUTPUT_TO_O", boolean_str[FP_OUTPUT_TO_O(fp_idx)]);
#if defined(_HOST32) && defined(_TARGET64)
      fprintf(out_file, "  %-16s= %-20Ld \n",
              "FP_OFFSET", FP_OFFSET(fp_idx));
#else
      fprintf(out_file, "  %-16s= %-20ld \n",
              "FP_OFFSET", FP_OFFSET(fp_idx));
#endif

      fprintf(out_file, "  %-16s= %-6s  %-16s= %-7s  %-16s= %-7s\n",
              "FP_SRCH_THE_FILE", boolean_str[FP_SRCH_THE_FILE(fp_idx)],
              "FP_SYSTEM_FILE", boolean_str[FP_SYSTEM_FILE(fp_idx)],
              "FP_TMP_FILE", boolean_str[FP_TMP_FILE(fp_idx)]);

      fp_idx = FP_MODULE_IDX(fp_idx);
   }
   while (print_list && fp_idx != NULL_IDX);

   fprintf(out_file, "\n");

   fflush (out_file);
   return;

}  /* dump_fp_ntry */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints one global attr table entry to the specified output file.      *|
|*									      *|
|* Input parameters:							      *|
|*	FILE to print to - Should be debug_file, stderr, or stdout.           *|
|*	Index of global attr entry to print.                                  *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void dump_ga_ntry (FILE		*out_file,
   		          int	 	 ga_idx)

{
   char		conv_str[80];
   int		ga_idx2;
   int		i;


   if (ga_idx > global_attr_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* GA index value [%d] is out of range.\n",
              ga_idx);
      return;
   }

   /* Note that the fields are displayed in alphabetical order.         */

   fprintf(out_file, "%-s\n", GA_OBJ_NAME_PTR(ga_idx));

   if (GA_OBJ_CLASS(ga_idx) == Common_Block) {
      fprintf(out_file, "  %-25s %-16s= %-7d %-16s= %-8d\n",
                     "Common_Blk",
                     "IDX", ga_idx,
                     "GA_DEF_COLUMN", GA_DEF_COLUMN(ga_idx));

      fprintf(out_file, "  %-16s= %-7d %-16s= %-7d %-s\n",
                        "GA_DEF_LINE", GA_DEF_LINE(ga_idx),
                        "GA_MODULE_IDX", GA_MODULE_IDX(ga_idx),
                        (GA_MODULE_IDX(ga_idx) == NULL_IDX) ? " ":
                                       GA_OBJ_NAME_PTR(GA_MODULE_IDX(ga_idx)));

      fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
                      "GA_NAME_LEN", GA_NAME_LEN(ga_idx),
                      "GA_USE_ASSOCIATE",boolean_str[GA_USE_ASSOCIATED(ga_idx)],
                      "GAC_AUXILIARY", boolean_str[GAC_AUXILIARY(ga_idx)]);

      fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                     "GAC_ALIGN_SYMBOL", boolean_str[GAC_ALIGN_SYMBOL(ga_idx)],
                     "GAC_CACHE_ALIGN", boolean_str[GAC_CACHE_ALIGN(ga_idx)],
                     "GAC_EQUIVALENCED", boolean_str[GAC_EQUIVALENCED(ga_idx)]);

      fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-8s\n",
                      "GAC_FILL_SYMBOL", boolean_str[GAC_FILL_SYMBOL(ga_idx)],
                      "GAC_FIRST_MEMBER", GAC_FIRST_MEMBER_IDX(ga_idx),
                      "GAC_FOUND_DIFFS", boolean_str[GAC_FOUND_DIFFS(ga_idx)]);

      fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
                    "GAC_SECTION_GP", boolean_str[GAC_SECTION_GP(ga_idx)],
                    "GAC_SECTION_NON_", boolean_str[GAC_SECTION_NON_GP(ga_idx)],
                    "GAC_TASK_COMMON", boolean_str[GAC_TASK_COMMON(ga_idx)]);
#ifdef KEY /* Bug 14150 */
      const char *bl = GA_BINDING_LABEL(ga_idx);
      fprintf(out_file, "  %-16s= %-7s %-16s= %-s\n",
		    "GA_BIND_ATTR",boolean_str[GA_BIND_ATTR(ga_idx)],
		    "GA_BIND_LABEL", (bl ? bl : ""));
#endif /* KEY Bug 14150 */

      ga_idx2	= GAC_FIRST_MEMBER_IDX(ga_idx);

      while (ga_idx2 != NULL_IDX) {
         dump_ga_ntry(out_file, ga_idx2);
         ga_idx2 = GAD_NEXT_IDX(ga_idx2);
      }
      return;
   }


   fprintf(out_file, "  %-25s %-16s= %-7s %-16s= %-8d\n",
                     obj_class_str[GA_OBJ_CLASS(ga_idx)],
                     "GA_REFERENCED", boolean_str[GA_REFERENCED(ga_idx)],
                     "IDX", ga_idx);

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-8d\n",
                     "GA_COMPILER_GEND", boolean_str[GA_COMPILER_GEND(ga_idx)],
                     "GA_DEF_COLUMN", GA_DEF_COLUMN(ga_idx),
                     "GA_DEF_LINE", GA_DEF_LINE(ga_idx));

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-s\n",
                     "GA_DEFINED", boolean_str[GA_DEFINED(ga_idx)],
                     "GA_MODULE_IDX", GA_MODULE_IDX(ga_idx),
                     (GA_MODULE_IDX(ga_idx) == NULL_IDX) ? " ":
                                       GA_OBJ_NAME_PTR(GA_MODULE_IDX(ga_idx)));

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8d\n",
                     "GA_NAME_LEN", GA_NAME_LEN(ga_idx),
                     "GA_OPTIONAL", boolean_str[GA_OPTIONAL(ga_idx)],
                     "GA_ORIG_NAME_LEN", GA_ORIG_NAME_LEN(ga_idx));

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-s\n",
                     "GA_USE_ASSOCIATE",boolean_str[GA_USE_ASSOCIATED(ga_idx)],
                     "GA_ORIG_NAME_IDX", GA_ORIG_NAME_IDX(ga_idx),
                              (GA_ORIG_NAME_IDX(ga_idx) == NULL_IDX)
                                         ? " ": GA_ORIG_NAME_PTR(ga_idx));


   switch (GA_OBJ_CLASS(ga_idx)) {
   case Data_Obj:

      fprintf(out_file, "  %-25s %-16s= %-7s %-16s= %-8d\n",
              atd_class_str[GAD_CLASS(ga_idx)],
              "GAD_ARRAY_ELEMEN", boolean_str[GAD_ARRAY_ELEMENT_REF(ga_idx)],
              "GAD_ARRAY_IDX", GAD_ARRAY_IDX(ga_idx));

      if (GAD_CLASS(ga_idx) == Dummy_Argument) {
         fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8d\n",
               "GAD_ASSUMED_SHAP", boolean_str[GAD_ASSUMED_SHAPE_ARRAY(ga_idx)],
               "GAD_INTENT", intent_str[GAD_INTENT(ga_idx)],
               "GAD_NEXT_IDX", GAD_NEXT_IDX(ga_idx));
      }
      else if (GAD_CLASS(ga_idx) == Constant) {
         fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-s\n",
               "GAD_ASSUMED_SHAP", boolean_str[GAD_ASSUMED_SHAPE_ARRAY(ga_idx)],
               "GAD_NEXT_IDX", GAD_NEXT_IDX(ga_idx),
               "GAD_HOLLERITH", cn_hollerith_str[GAD_HOLLERITH(ga_idx)]);
      }
      else {
         fprintf(out_file, "  %-16s= %-7s %-16s= %-7d\n",
               "GAD_ASSUMED_SHAP", boolean_str[GAD_ASSUMED_SHAPE_ARRAY(ga_idx)],
               "GAD_NEXT_IDX", GAD_NEXT_IDX(ga_idx));
      }

      fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-8s\n",
              "GAD_POINTER", boolean_str[GAD_POINTER(ga_idx)],
              "GAD_RANK", GAD_RANK(ga_idx),
              "GAD_TARGET", boolean_str[GAD_TARGET(ga_idx)]);

#ifdef KEY /* Bug 14110 */
      fprintf(out_file, "  %-16s= %-7s\n",
	      "GAD_VOLATILE", boolean_str[GAD_VOLATILE(ga_idx)]);
#endif /* KEY /* Bug 14110 */
      fprintf(out_file, "  %-16s= %-7d %-s\n",
              "GAD_TYPE_IDX",  GAD_TYPE_IDX(ga_idx),
               print_global_type_f(GAD_TYPE_IDX(ga_idx)));

      if (GAD_ARRAY_IDX(ga_idx) != NULL_IDX) {
         dump_gb_ntry(out_file, GAD_ARRAY_IDX(ga_idx));
      }

      break;


   case Pgm_Unit:

      fprintf(out_file, "  %-25s %-16s= %-7s %-16s= %-8d\n",
              atp_pgm_unit_str[GAP_PGM_UNIT(ga_idx)],
              "GAP_ELEMENTAL", boolean_str[GAP_ELEMENTAL(ga_idx)],
              "GAP_FIRST_IDX", GAP_FIRST_IDX(ga_idx));

      fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
              "GAP_FP_IDX", GAP_FP_IDX(ga_idx),
              "GAP_GLOBAL_DIR", boolean_str[GAP_GLOBAL_DIR(ga_idx)],
              "GAP_IN_INTERFACE", boolean_str[GAP_IN_INTERFACE_BLK(ga_idx)]);

      fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-8s\n",
              "GAP_NEEDS_EXPL_I", boolean_str[GAP_NEEDS_EXPL_ITRFC(ga_idx)],
              "GAP_NEXT_PGM_IDX", GAP_NEXT_PGM_UNIT_IDX(ga_idx),
              "GAP_NOSIDE_EFFEC", boolean_str[GAP_NOSIDE_EFFECTS(ga_idx)]);

      fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8s\n",
              "GAP_NUM_DARGS", GAP_NUM_DARGS(ga_idx),
              "GAP_PGM_UNIT_DEF", boolean_str[GAP_PGM_UNIT_DEFINED(ga_idx)],
              "GAP_PURE", boolean_str[GAP_PURE(ga_idx)]);

#ifdef KEY /* Bug 14150 */
      fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
              "GAP_RECURSIVE", boolean_str[GAP_RECURSIVE(ga_idx)],
              "GAP_VFUNCTION",boolean_str[GAP_VFUNCTION(ga_idx)],
	      "GA_BIND_ATTR",boolean_str[GA_BIND_ATTR(ga_idx)]);
      const char *bl = GA_BINDING_LABEL(ga_idx);
      fprintf(out_file, "  %-16s= %-s\n",
      	      "GA_BIND_LABEL", (bl ? bl : ""));
#else /* KEY */
      fprintf(out_file, "  %-16s= %-7s %-16s= %-7s\n",
              "GAP_RECURSIVE", boolean_str[GAP_RECURSIVE(ga_idx)],
              "GAP_VFUNCTION",boolean_str[GAP_VFUNCTION(ga_idx)]);
#endif /* KEY Bug 14150 */

      if (GAP_RSLT_IDX(ga_idx) != NULL_IDX) {
         dump_ga_ntry(out_file, GAP_RSLT_IDX(ga_idx));
      }

      ga_idx2	= GAP_FIRST_IDX(ga_idx);

      for (i = GAP_NUM_DARGS(ga_idx); i > 0; i--) {
         dump_ga_ntry(out_file, ga_idx2);
         ga_idx2++;
      }

      break;

   case Derived_Type:

      fprintf(out_file, "  %-16s= %-7d %-16s= %-7d %-16s= %-8s\n",
              "GAT_FIRST_CPNT_I", GAT_FIRST_CPNT_IDX(ga_idx),
              "GAT_NUM_CPNTS", GAT_NUM_CPNTS(ga_idx),
              "GAT_PRIVATE_CPNT", boolean_str[GAT_PRIVATE_CPNT(ga_idx)]);

#if ! (defined(_HOST32) && defined(_TARGET64))
      /* 31Jan01[sos]: Following call to convert_to_string gets SIGSEGV w/PV 761865 */
      /* fprintf(out_file, "  %-16s= %-7s %-16s= %-33s\n",                          */
      /*         "GAT_SEQUENCE_SET", boolean_str[GAT_SEQUENCE_SET(ga_idx)],         */
      /*         "GAT_STRUCT_BIT_L",                                                */
      /*              convert_to_string(GAT_STRUCT_BIT_LEN(ga_idx),                 */
      /*                                GAT_STRUCT_LIN_TYPE(ga_idx), conv_str));    */
      /* Fix follows...                                                             */
      fprintf(out_file, "  %-16s= %-7s %-16s= %-33ld\n",
              "GAT_SEQUENCE_SET", boolean_str[GAT_SEQUENCE_SET(ga_idx)],
              "GAT_STRUCT_BIT_L", *GAT_STRUCT_BIT_LEN(ga_idx));
#else
      fprintf(out_file, "  %-16s= %-7s %-16s= %-33Ld\n",
              "GAT_SEQUENCE_SET", boolean_str[GAT_SEQUENCE_SET(ga_idx)],
              "GAT_STRUCT_BIT_L", *GAT_STRUCT_BIT_LEN(ga_idx));
#endif

      ga_idx2	= GAT_FIRST_CPNT_IDX(ga_idx);

      for (i = GAT_NUM_CPNTS(ga_idx); i > 0; i--) {
         dump_ga_ntry(out_file, ga_idx2);
         ga_idx2++;
      }

      break;


   default:
      break;

   }  /* End switch */

   putc ('\n', out_file);
   fflush (out_file);

   return;

}  /* dump_ga_ntry */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints one global bounds table entry to the specified output file.    *|
|*									      *|
|* Input parameters:							      *|
|*	FILE to print to - Should be debug_file, stderr, or stdout.           *|
|*	Index of global bound entry to print.                                 *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void dump_gb_ntry (FILE		*out_file,
   		          int	 	 gb_idx)

{
   char		conv_str[80];
   char		conv_str2[80];
   int		i;


   if (gb_idx > global_bounds_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* GB index value [%d] is out of range.\n",
              gb_idx);
      goto EXIT;
   }

   fprintf(out_file, "  %-16s= %-7d %-15s %-25s %-4s= %d\n",
                     "GB_RANK", GB_RANK(gb_idx),
                                bd_array_class_str[GB_ARRAY_CLASS(gb_idx)],
                                bd_array_size_str[GB_ARRAY_SIZE(gb_idx)],
                     "IDX", gb_idx);

   if (GB_ARRAY_CLASS(gb_idx) == Explicit_Shape && 
       GB_ARRAY_SIZE(gb_idx) == Constant_Size) {

      for (i = 1; i <=  GB_RANK(gb_idx); i++) {
         fprintf(out_file, "  %-16s= %-7d %-15s %-15s %-15s %-15s\n",
                           "Dimension", i,
                            convert_to_string(GB_LOWER_BOUND(gb_idx,i),
                                       GT_LINEAR_TYPE(GB_LB_TYPE(gb_idx,i)),
                                       conv_str),
                            print_global_type_f(GB_LB_TYPE(gb_idx,i)),
                            convert_to_string(GB_UPPER_BOUND(gb_idx,i),
                                       GT_LINEAR_TYPE(GB_UB_TYPE(gb_idx,i)),
                                       conv_str2),
                            print_global_type_f(GB_UB_TYPE(gb_idx,i)));
      }
   }

   putc ('\n', out_file);

EXIT:

   fflush (out_file);
   return;

}  /* dump_gb_ntry */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints a global line table entry.                                     *|
|*									      *|
|* Input parameters:							      *|
|*	Index of global line table entry to print.                            *|
|*									      *|
\******************************************************************************/

static	void dump_gl_ntry(FILE		*out_file,
			  int		 gl_idx)

{
   if (gl_idx > global_line_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* GL index value [%d] is out of range.\n",
              gl_idx);
      return;
   }

   fprintf(out_file,"%-s\n", GL_FILE_NAME_PTR(gl_idx));
   fprintf(out_file,"%-s\n", GL_PATH_NAME_PTR(gl_idx));

   if (full_debug_dump) {
      fprintf(out_file,"    %-22s= %-10d\n",
              "IDX", gl_idx);

      fprintf(out_file,"    %-22s= %-10d  %-20s= %-10d\n",
              "GL_PATH_NAME_IDX", GL_PATH_NAME_IDX(gl_idx),
              "GL_FILE_NAME_IDX", GL_FILE_NAME_IDX(gl_idx));
   }

   fprintf(out_file,"    %-22s= %-10d  %-20s= %-10d\n",
           "GL_CIF_FILE_ID", GL_CIF_FILE_ID(gl_idx),
           "GL_FILE_LINE",   GL_FILE_LINE(gl_idx));

   fprintf(out_file,"    %-22s= %-10d  %-20s= %-10d\n",
           "GL_FILE_NAME_LEN", GL_FILE_NAME_LEN(gl_idx),
           "GL_GLOBAL_LINE",   GL_GLOBAL_LINE(gl_idx));

   fprintf(out_file,"    %-22s= %-10d  %-20s= %-10d\n",
           "GL_INCLUDE_FILE_COL", GL_INCLUDE_FILE_COL(gl_idx),
           "GL_INCLUDE_FILE_LINE", GL_INCLUDE_FILE_LINE(gl_idx));


   fprintf(out_file,"    %-22s= %-10d  %-20s= %-10d\n",
           "GL_PATH_NAME_LEN", GL_PATH_NAME_LEN(gl_idx),
           "GL_SOURCE_LINES", GL_SOURCE_LINES(gl_idx));

   return;

}  /* dump_gl_ntry */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints a global name table entry.                                     *|
|*									      *|
|* Input parameters:							      *|
|*	Index of global name table entry to print.                            *|
|*									      *|
\******************************************************************************/
static	void dump_gn_ntry(FILE		*out_file,
			  int		 gn_idx)

{
   if (gn_idx > global_name_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* GN index value [%d] is out of range.\n",
              gn_idx);
      return;
   }

   fprintf(out_file, "%-s\n", GN_NAME_PTR(gn_idx));

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7d \n",
           "IDX", gn_idx,
           "GN_ATTR_IDX", GN_ATTR_IDX(gn_idx));

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7d\n",
           "GN_NAME_IDX", GN_NAME_IDX(gn_idx),
           "GN_NAME_LEN", GN_NAME_LEN(gn_idx));
   return;

}  /* dump_gn_ntry */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*									      *|
|* Input parameters:							      *|
|*	FILE to print to - Should be debug_file, stderr, or stdout.           *|
|*	Index of type table entry to print.                                   *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void dump_gt_ntry(FILE	*out_file,
   		         int	 gt_idx)
 
{
   char		conv_str[80];


   if (gt_idx > global_type_tbl_idx) {
      fprintf(out_file,"\n*FE90-ERROR* GT index value [%d] is out of range.\n",
              gt_idx);
      return;
   }

   fprintf(out_file,"  %-25s %-25s %-26s\n",
           basic_type_str[GT_TYPE(gt_idx)],
           lin_type_str[GT_LINEAR_TYPE(gt_idx)],
           type_desc_str[GT_DESC(gt_idx)]);

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7d",
           "GT_DCL_VALUE", GT_DCL_VALUE(gt_idx),
           "IDX", gt_idx);

   if (GT_TYPE(gt_idx) == Character) {
      fprintf(out_file," %-25s \n  %-16s= %-s\n",
              type_char_class_str[GT_CHAR_CLASS(gt_idx)],
              "GT_LENGTH",
              convert_to_string(GT_LENGTH(gt_idx),
                                GT_LENGTH_LIN_TYPE(gt_idx),
                                conv_str));
   }
   else if (GT_TYPE(gt_idx) == CRI_Ptr) {
#if defined(_HOST32) && defined(_TARGET64)
      fprintf(out_file, " %-16s= %-7Ld\n",
           "GT_PTR_INCREMENT", GT_PTR_INCREMENT(gt_idx));
#else
      fprintf(out_file, " %-16s= %-7ld\n",
           "GT_PTR_INCREMENT", GT_PTR_INCREMENT(gt_idx));
#endif
   }
   else if (GT_TYPE(gt_idx) == Structure) {
      fprintf(out_file, " %-16s= %-7d\n",
                        "GT_STRUCT_IDX", GT_STRUCT_IDX(gt_idx));

      dump_ga_ntry(out_file, GT_STRUCT_IDX(gt_idx));
   }
   else {
      fprintf(out_file, " %-16s= %-33s\n",
              "GT_BIT_LEN", CONVERT_CVAL_TO_STR(&GT_BIT_LEN(gt_idx),
                                                 Integer_8,
                                                 conv_str));
   }

   putc ('\n', out_file);

   fflush (out_file);
   return;

}  /* dump_gt_ntry */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Dumps a single Hidden Name Table entry to the specified file.         *|
|*									      *|
|* Input parameters:							      *|
|*      Pointer to dump file.						      *|
|*	Index of Hidden Name Table entry to print.                            *|
|*      Flag to indicate whether to print the attr entry or not.  If the attr *|
|*         entry is printed, it is a full attr entry dump.                    *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void dump_hn_ntry(FILE		*out_file,
			 int		idx,
                         boolean	print_the_attr)

{

   if (idx > hidden_name_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* HN index value [%d] is out of range.\n",
              idx);
      return;
   }

   if (HN_ATTR_IDX(idx) != NULL_IDX) {

      if (HN_NAME_IDX(idx) != NULL_IDX) {
         fprintf(out_file, "%-32.32s  ", HN_NAME_PTR(idx));
      }
      else {
         fprintf(out_file, "%-32.32s  ", "**No name - HN_NAME_IDX is 0**");
      }
   }
   else {
      fprintf(out_file, "%-32.32s  ", "**Error** - HN_ATTR_IDX = 0**");
   }

   fprintf(out_file, "%-8s= %-7d %-16s= %-8d\n",
           "IDX", idx, 
           "HN_ATTR_IDX", HN_ATTR_IDX(idx));

   fprintf(out_file, "  %-16s= %-7d %-16s= %-8d\n",
           "HN_NAME_IDX", HN_NAME_IDX(idx),
           "HN_NAME_LEN", HN_NAME_LEN(idx));

   if (print_the_attr && HN_ATTR_IDX(idx) != NULL_IDX) {
      putc ('\n', out_file);
      dump_at_ntry(out_file, HN_ATTR_IDX(idx), TRUE);
   }

   putc ('\n', out_file);

   return;

}  /* dump_hn_ntry */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Dumps a single List Table entry to the specified file.                *|
|*      See also print_list (used to dump a chain of List entries when the    *|
|*        length of the chain is known).                                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      Pointer to dump file.                                                 *|
|*      Index of List Table entry to print.                                   *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

static void dump_il_ntry(FILE         *out_file,
                         int          idx)

{
   if (idx > ir_list_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* IL index value [%d] is out of range.\n",
              idx);
      return;
   }

   fprintf(out_file, "%s= %-8d",
           "IL_NEXT_LIST_IDX", IL_NEXT_LIST_IDX(idx));

   if (! IL_ARG_DESC_VARIANT(idx)) {
      fprintf(out_file, "    %s= %-8d\n",
              "IL_PREV_LIST_IDX", IL_PREV_LIST_IDX(idx));
   }
   else {
      fprintf(out_file, "    %s= %-8d\n",
              "IL_ARG_DESC_IDX", IL_ARG_DESC_IDX(idx));
      putc('\n', out_file);
   }

   if (IL_FLD(idx) == IL_Tbl_Idx) {
      fprintf(out_file, "%s= %-8d   ",
              "IL_LIST_CNT", IL_LIST_CNT(idx));
   }
   else {
      fprintf(out_file, "%s= %-8d   %s= %-3d   ",
              "IL_LINE_NUM", IL_LINE_NUM(idx),
              "IL_COL_NUM",  IL_COL_NUM(idx));
   }

   fprintf(out_file, "%s= %s   %s= ",
           "IL_FLD", field_str[IL_FLD(idx)], "IL_IDX");

   if (IL_IDX(idx) != NULL_IDX) {
      fprintf(out_file, "%-8d\n", IL_IDX(idx));
   }
   else {
      fprintf(out_file, "%s\n", "*NULL_IDX*");
   }

   putc ('\n', out_file);

}  /* dump_il_ntry */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      recursively prints ir text. Not intended for stmt headers.            *|
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

static void dump_ir_ntry(FILE 	*out_file,
                         int	 idx,
		         int	 indent)

{
   int  	i;
   boolean	io_list = FALSE;
   char 	n_shift[INDENT_SIZE + 1];
   char		shift[80];
   int  	type_idx;
   long_type	io_type_code[2];


   if (idx > ir_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* IR index value [%d] is out of range.\n",
              idx);
      return;
   }

   for (i = 0; i < INDENT_SIZE * indent; i++) {
      shift[i] = ' ';
      if (i == 79)
         break;
   }
   shift[i] = '\0';
   for (i = 0; i < INDENT_SIZE; i++) {
      n_shift[i] = ' ';
   }

   n_shift[i]	= '\0';
   type_idx	= IR_TYPE_IDX(idx);

   fprintf(out_file, "%s%s  idx = %d", shift, operator_str[IR_OPR(idx)], idx);

   if (type_idx == NULL_IDX) {
      fprintf(out_file, "  %s  ", "NO TYPE");
   }
   else {
      fprintf(out_file, "  %s  ", print_type_f(type_idx));
   }

   if (IR_OPR(idx) >= Dv_Whole_Copy_Opr && 
       IR_OPR(idx) <= Dv_Set_Stride_Mult) {
      fprintf(out_file, " dim = %d", IR_DV_DIM(idx));
   }
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   else if (IR_OPR(idx) == Call_Opr) {
      if (IR_INLINE_STATE(idx) == Inline_Sgi) {
         fprintf(out_file, " INLINE ");
      }
      else if (IR_INLINE_STATE(idx) == Noinline_Sgi) {
         fprintf(out_file, " NOINLINE ");
      }
   }
# endif

   if ((IR_OPR(idx) == Subscript_Opr ||
        IR_OPR(idx) == Section_Subscript_Opr ||
        IR_OPR(idx) == Substring_Opr) &&
       IR_BOUNDS_DONE(idx)) {

      fprintf(out_file, " BOUNDS DONE ");
   }

   if (IR_OPR(idx) == Whole_Subscript_Opr &&
       IR_CONTIG_ARRAY(idx)) {

     fprintf(out_file, " CONTIGUOUS ARRAY ");
   }

   if (IR_OPR(idx) == Subscript_Opr &&
       IR_WHOLE_ARRAY(idx)) {

     fprintf(out_file, " WHOLE ARRAY ");
   }

   if (IR_OPR(idx) == Read_Formatted_Opr ||
       IR_OPR(idx) == Read_Unformatted_Opr ||
       IR_OPR(idx) == Read_Namelist_Opr ||
       IR_OPR(idx) == Write_Formatted_Opr ||
       IR_OPR(idx) == Write_Unformatted_Opr ||
       IR_OPR(idx) == Write_Namelist_Opr ||
       IR_OPR(idx) == Inquire_Iolength_Opr) {

      io_list = TRUE;
   }

   fprintf(out_file, " rank = %d;", IR_RANK(idx));
   fprintf(out_file, " line = %d, col = %d\n", IR_LINE_NUM(idx),
                                                IR_COL_NUM(idx));

   if (IR_OPR(idx) == Dv_Whole_Def_Opr) {

      fprintf(out_file, "%sLeft opnd is %s;", shift, field_str[IR_FLD_L(idx)]);

      switch (IR_FLD_L(idx)) {
         case CN_Tbl_Idx :
         case AT_Tbl_Idx   :
                 fprintf(out_file,"  line = %d, col = %d\n",IR_LINE_NUM_L(idx),
                                 IR_COL_NUM_L(idx));
                 break;
         case IL_Tbl_Idx  :
                 fprintf(out_file,"  list cnt = %d\n", IR_LIST_CNT_L(idx));
                 break;
         default         :
                 fprintf(out_file,"\n");
                 break;
      }

#ifdef KEY /* Bug 6845 */
      print_Dv_Whole_Def_Opr(out_file, IR_IDX_L(idx),
                            indent + 1, IR_LIST_CNT_L(idx), IR_DV_DIM(idx));
#else /* KEY Bug 6845 */
      print_Dv_Whole_Def_Opr(out_file, IR_IDX_L(idx),
                            indent + 1, IR_LIST_CNT_L(idx));
#endif /* KEY Bug 6845 */
   }
   else if (IR_OPR(idx) == Doacross_Dollar_Opr ||
            IR_OPR(idx) == Psection_Par_Opr ||
            IR_OPR(idx) == Singleprocess_Par_Opr ||
            IR_OPR(idx) == Parallel_Do_Par_Opr ||
            IR_OPR(idx) == Parallel_Par_Opr ||
            IR_OPR(idx) == Pdo_Par_Opr) {


      fprintf(out_file, "%sLeft opnd is %s;", shift, field_str[IR_FLD_L(idx)]);

      switch (IR_FLD_L(idx)) {
         case CN_Tbl_Idx :
         case AT_Tbl_Idx   :
                 fprintf(out_file,"  line = %d, col = %d\n",IR_LINE_NUM_L(idx),
                                 IR_COL_NUM_L(idx));
                 break;
         case IL_Tbl_Idx  :
                 fprintf(out_file,"  list cnt = %d\n", IR_LIST_CNT_L(idx));
                 break;
         default         :
                 fprintf(out_file,"\n");
                 break;
      }

      print_mp_dir_opr(out_file, IR_IDX_L(idx),
                            indent + 1, IR_LIST_CNT_L(idx));
   }
   else if (IR_OPR(idx) == Do_Open_Mp_Opr ||
            IR_OPR(idx) == Parallel_Open_Mp_Opr ||
            IR_OPR(idx) == Paralleldo_Open_Mp_Opr ||
            IR_OPR(idx) == Parallelsections_Open_Mp_Opr ||
            IR_OPR(idx) == Sections_Open_Mp_Opr ||
            IR_OPR(idx) == Single_Open_Mp_Opr) {


      fprintf(out_file, "%sLeft opnd is %s;", shift, field_str[IR_FLD_L(idx)]);

      switch (IR_FLD_L(idx)) {
         case CN_Tbl_Idx :
         case AT_Tbl_Idx   :
                 fprintf(out_file,"  line = %d, col = %d\n",IR_LINE_NUM_L(idx),
                                 IR_COL_NUM_L(idx));
                 break;
         case IL_Tbl_Idx  :
                 fprintf(out_file,"  list cnt = %d\n", IR_LIST_CNT_L(idx));
                 break;
         default         :
                 fprintf(out_file,"\n");
                 break;
      }

      print_open_mp_dir_opr(out_file, IR_IDX_L(idx),
                            indent + 1, IR_LIST_CNT_L(idx));
   }
   else {

      if (IR_OPR(idx) == Io_Item_Type_Code_Opr) {
         make_io_type_code(IR_TYPE_IDX(idx), io_type_code);
         dump_io_type_code_ntry(out_file, io_type_code, indent + 1);
      }

      fprintf(out_file, "%sLeft opnd is %s;", shift, field_str[IR_FLD_L(idx)]);

      switch (IR_FLD_L(idx)) {
         case CN_Tbl_Idx :
         case AT_Tbl_Idx   :
         case SB_Tbl_Idx   :
                 fprintf(out_file,"  line = %d, col = %d\n",IR_LINE_NUM_L(idx),
                                 IR_COL_NUM_L(idx));
                 break;
         case IL_Tbl_Idx  :
                 fprintf(out_file,"  list cnt = %d\n", IR_LIST_CNT_L(idx));
                 break;
         default         :
                 fprintf(out_file,"\n");
                 break;
      }

      switch (IR_FLD_L(idx)) {
         case NO_Tbl_Idx :
                 break;
         case CN_Tbl_Idx : 
                 print_const_entry(out_file, IR_IDX_L(idx), indent + 1);
                 break;
         case AT_Tbl_Idx : 
                 print_attr_name(out_file, IR_IDX_L(idx), indent + 1);
                 break;
         case SB_Tbl_Idx : 
                 fprintf(out_file,"%s\n", SB_NAME_PTR(IR_IDX_L(idx)));
                 break;
         case IR_Tbl_Idx :
                 dump_ir_ntry(out_file, IR_IDX_L(idx), indent + 1);
                 break;
         case IL_Tbl_Idx :
                 if (IR_IDX_L(idx) != NULL_IDX && IR_LIST_CNT_L(idx) > 0) {
                    print_list(out_file, IR_IDX_L(idx), 
                            indent + 1, IR_LIST_CNT_L(idx), io_list);
                 }
                 break;
         case SH_Tbl_Idx :
                 fprintf(out_file, "%s%sStmt Header idx = %d\n", shift, 
                         n_shift, IR_IDX_L(idx));
                 break;
      }
   
      fprintf(out_file,"%sRight operand is %s;", shift,
                                                 field_str[IR_FLD_R(idx)]);
   
      switch (IR_FLD_R(idx)) {
         case CN_Tbl_Idx :
         case AT_Tbl_Idx :
         case SB_Tbl_Idx :
                 fprintf(out_file,"  line = %d, col = %d\n",
                         IR_LINE_NUM_R(idx), IR_COL_NUM_R(idx));
                 break;
         case IL_Tbl_Idx  :
                 fprintf(out_file,"  list cnt = %d\n", IR_LIST_CNT_R(idx));
                 break;
         default         :
                 fprintf(out_file,"\n");
                 break;
      }
   
   
      switch (IR_FLD_R(idx)) {
         case NO_Tbl_Idx  :
                 break;
         case CN_Tbl_Idx :
                 print_const_entry(out_file, IR_IDX_R(idx), indent + 1);
                 break;
         case AT_Tbl_Idx   :
                 print_attr_name(out_file, IR_IDX_R(idx), indent + 1);
                 break;
         case SB_Tbl_Idx : 
                 fprintf(out_file,"%s\n", SB_NAME_PTR(IR_IDX_R(idx)));
                 break;
         case IR_Tbl_Idx  :
                 dump_ir_ntry(out_file, IR_IDX_R(idx), indent + 1);
                 break;
         case IL_Tbl_Idx  :
                 if (IR_IDX_R(idx) != NULL_IDX && IR_LIST_CNT_R(idx) > 0) {
                    print_list(out_file, IR_IDX_R(idx),
                            indent + 1, IR_LIST_CNT_R(idx), io_list);
                 }
                 break;
         case SH_Tbl_Idx  :
                 fprintf(out_file, "%s%sStmt Header idx = %d\n", shift, 
                         n_shift, IR_IDX_R(idx));
                 break;
      }
   }

   return;

} /* dump_ir_ntry */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Dumps a single Local Name Table entry to the specified file.          *|
|*									      *|
|* Input parameters:							      *|
|*      Pointer to dump file.						      *|
|*	Index of Local Name Table entry to print.                             *|
|*      Flag to indicate whether to print the attr entry or not.  If the attr *|
|*         entry is printed, it is a full attr entry dump.                    *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void dump_ln_ntry(FILE		*out_file,
			 int		idx,
                         boolean	print_the_attr)

{

   if (idx > loc_name_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* LN index value [%d] is out of range.\n",
              idx);
      return;
   }

   if (LN_ATTR_IDX(idx) != NULL_IDX) {

      if (LN_NAME_IDX(idx) != NULL_IDX) {
         fprintf(out_file, "%-32.32s  ", LN_NAME_PTR(idx));
      }
      else {
         fprintf(out_file, "%-32.32s  ", "**No name - LN_NAME_IDX is 0**");
      }
   }
   else {
      fprintf(out_file, "%-32.32s  ", "**Error** - LN_ATTR_IDX = 0**");
   }

   fprintf(out_file, "%-10s= %-7d %-16s= %-8d\n",
           "IDX", idx, 
           "LN_ATTR_IDX", LN_ATTR_IDX(idx));
   fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-8d\n",
           "LN_DEF_LOC", boolean_str[LN_DEF_LOC(idx)],
           "LN_NAME_LEN", LN_NAME_LEN(idx),
           "LN_NAME_IDX", LN_NAME_IDX(idx));
   fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
           "LN_IN_ONLY_LIST", boolean_str[LN_IN_ONLY_LIST(idx)],
           "LN_NEW_NAME", boolean_str[LN_NEW_NAME(idx)],
           "LN_RENAMED", boolean_str[LN_RENAMED(idx)]);

   if (print_the_attr && LN_ATTR_IDX(idx) != NULL_IDX) {
      putc ('\n', out_file);
      dump_at_ntry(out_file, LN_ATTR_IDX(idx), TRUE);
   }

   putc ('\n', out_file);

   return;

}  /* dump_ln_ntry */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Dumps a single module link table entry to the specified file.         *|
|*									      *|
|* Input parameters:							      *|
|*      Pointer to dump file.						      *|
|*	Index of module link table entry to print.                            *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void dump_ml_ntry(FILE		*out_file,
			 int		idx)

{
   if (idx > mod_link_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* ML index value [%d] is out of range.\n",
              idx);
      return;
   }

   fprintf(out_file, "%4s%-15s= %-2s  %-15s= %-8d  %-16s= %-9d\n", " ",
           "ML_AT_COMPRESSED_IDX", boolean_str[ML_AT_COMPRESSED_IDX(idx)],
           "ML_AT_IDX", ML_AT_IDX(idx),
           "IDX",	idx);

   fprintf(out_file, "%4s%-15s= %-2s  %-15s= %-8s  %-16s= %-9s\n", " ",
           "ML_AT_KEEP_ME", boolean_str[ML_AT_KEEP_ME(idx)],
           "ML_AT_LN_NAME", boolean_str[ML_AT_LN_NAME(idx)],
           "ML_AT_SEARCHED", boolean_str[ML_AT_SEARCHED(idx)]);

   fprintf(out_file, "%4s%-15s= %-2s  %-15s= %-8d\n", " ",
           "ML_BD_KEEP_ME", boolean_str[ML_BD_KEEP_ME(idx)],
           "ML_BD_IDX", ML_BD_IDX(idx));

   fprintf(out_file, "%4s%-15s= %-2s  %-15s= %-8d  %-16s= %-9s\n", " ",
           "ML_CN_KEEP_ME", boolean_str[ML_CN_KEEP_ME(idx)],
           "ML_CN_IDX", ML_CN_IDX(idx),
           "ML_CP_DALIGN_ME", boolean_str[ML_CP_DALIGN_ME(idx)]);

   fprintf(out_file, "%4s%-15s= %-2s  %-15s= %-8d  %-16s= %-9d\n", " ",
           "ML_CP_KEEP_ME", boolean_str[ML_CP_KEEP_ME(idx)],
           "ML_CP_IDX", ML_CP_IDX(idx),
           "ML_CP_LEN", ML_CP_LEN(idx));

   fprintf(out_file, "%4s%-15s= %-2s  %-15s= %-8d\n", " ",
           "ML_IL_KEEP_ME", boolean_str[ML_IL_KEEP_ME(idx)],
           "ML_IL_IDX", ML_IL_IDX(idx));

   fprintf(out_file, "%4s%-15s= %-2s  %-15s= %-8d\n", " ",
           "ML_IR_KEEP_ME", boolean_str[ML_IR_KEEP_ME(idx)],
           "ML_IR_IDX", ML_IR_IDX(idx));

   fprintf(out_file, "%4s%-15s= %-2s  %-15s= %-8d\n", " ",
           "ML_LN_KEEP_ME", boolean_str[ML_LN_KEEP_ME(idx)],
           "ML_LN_IDX", ML_LN_IDX(idx));

   fprintf(out_file, "%4s%-15s= %-2s  %-15s= %-8d  %-16s= %-9d\n", " ",
           "ML_NP_KEEP_ME", boolean_str[ML_NP_KEEP_ME(idx)],
           "ML_NP_IDX", ML_NP_IDX(idx),
           "ML_NP_LEN", ML_NP_LEN(idx));

   fprintf(out_file, "%4s%-15s= %-2s  %-15s= %-8d\n", " ",
           "ML_SB_KEEP_ME", boolean_str[ML_SB_KEEP_ME(idx)],
           "ML_SB_IDX", ML_SB_IDX(idx));

   fprintf(out_file, "%4s%-15s= %-2s  %-15s= %-8d\n", " ",
           "ML_SH_KEEP_ME", boolean_str[ML_SH_KEEP_ME(idx)],
           "ML_SH_IDX", ML_SH_IDX(idx));

   fprintf(out_file, "%4s%-15s= %-2s  %-15s= %-8d\n", " ",
           "ML_SN_KEEP_ME", boolean_str[ML_SN_KEEP_ME(idx)],
           "ML_SN_IDX", ML_SN_IDX(idx));

   fprintf(out_file, "%4s%-15s= %-2s  %-15s= %-8d\n", " ",
           "ML_TYP_KEEP_ME", boolean_str[ML_TYP_KEEP_ME(idx)],
           "ML_TYP_IDX", ML_TYP_IDX(idx));

   putc ('\n', out_file);

   return;

}  /* dump_ml_ntry */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints one Rename Only Table entry to the specified output file.      *|
|*									      *|
|* Input parameters:							      *|
|*	FILE to print to - Should be debug_file, stderr, or stdout.           *|
|*	Index of Rename Only Table entry to print.                            *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void dump_ro_ntry(FILE	*out_file,
   		         int	 ro_idx)
 
{

   if (ro_idx > rename_only_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* ML index value [%d] is out of range.\n",
              ro_idx);
      return;
   }

   fprintf(out_file, "%4s%-32.32s", " ", RO_NAME_PTR(ro_idx));

   if (full_debug_dump) {
      fprintf(out_file, "  %-4s= %-7d  %-16s= %-9d\n",
              "IDX", ro_idx,
              "RO_NEXT_IDX", RO_NEXT_IDX(ro_idx));
   }
   else {
      fprintf(out_file, "\n");
   }

   fprintf(out_file, "%4s%-16s= %-2d  %-16s= %-7d  %-16s= %-9d\n", " ",
           "RO_COLUMN_NUM", RO_COLUMN_NUM(ro_idx),
           "RO_LINE_NUM", RO_LINE_NUM(ro_idx),
           "RO_NAME_LEN", RO_NAME_LEN(ro_idx));

   if (RO_RENAME_IDX(ro_idx) != NULL_IDX) {
      ro_idx = RO_RENAME_IDX(ro_idx);

      fprintf(out_file, "%4s%-16s  %-2s  %-16s= %-s\n", " ",
              "RENAMED", " ",
              "RO_NAME_PTR", RO_NAME_PTR(ro_idx));

      fprintf(out_file, "%4s%-16s= %-2d  %-16s= %-7d  %-16s= %-9d\n", " ",
              "RO_COLUMN_NUM", RO_COLUMN_NUM(ro_idx),
              "RO_LINE_NUM", RO_LINE_NUM(ro_idx),
              "RO_NAME_LEN", RO_NAME_LEN(ro_idx));

      fprintf(out_file, "%4s%-16s= %-2s  %-16s= %-7s\n", " ",
              "RO_RENAME_NAME", boolean_str[RO_RENAME_NAME(ro_idx)],
              "RO_DUPLICATE_REN", boolean_str[RO_DUPLICATE_RENAME(ro_idx)]);
   }

   fflush (out_file);
   return;

}  /* dump_ro_ntry */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints one Storage Block Table entry to the specified output file.    *|
|*									      *|
|* Input parameters:							      *|
|*	FILE to print to - Should be debug_file, stderr, or stdout.           *|
|*	Index of Storage Block Table entry to print.                          *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void dump_sb_ntry(FILE	*out_file,
   		         int	 sb_idx)
 
{


   if (sb_idx > stor_blk_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* SB index value [%d] is out of range.\n",
              sb_idx);
      return;
   }

   if (SB_NAME_IDX(sb_idx) != NULL_IDX) {
      fprintf(out_file, "  %s\n", SB_NAME_PTR(sb_idx));
   }

   fprintf(out_file, "  %-16s= %-33s %-16s= %-8d\n",
           "SB_BLK_TYPE", sb_blk_type_str[SB_BLK_TYPE(sb_idx)],
           "IDX", sb_idx);

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-7s\n",
           "SB_ALIGN_SYMBOL", boolean_str[SB_ALIGN_SYMBOL(sb_idx)],
           "SB_AUXILIARY", boolean_str[SB_AUXILIARY(sb_idx)],
           "SB_BLANK_COMMON", boolean_str[SB_BLANK_COMMON(sb_idx)]);

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-8s\n",
           "SB_CACHE_ALIGN", boolean_str[SB_COMMON_NEEDS_OFFSET(sb_idx)],
           "SB_CIF_SYMBOL_ID", SB_CIF_SYMBOL_ID(sb_idx),
           "SB_COMMON_NEEDS_", boolean_str[SB_COMMON_NEEDS_OFFSET(sb_idx)]);

   fprintf(out_file,
#ifdef KEY /* Bug 14150 */
           "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
	   "SB_BIND_ATTR", boolean_str[SB_BIND_ATTR(sb_idx)],
#else /* KEY Bug 14150 */
           "  %-16s  %-7s %-16s= %-7s %-16s= %-8s\n",
           " ", " ",
#endif /* KEY Bug 14150 */
           "SB_DCL_COMMON_DI", boolean_str[SB_DCL_COMMON_DIR(sb_idx)],
           "SB_DCL_ERR", boolean_str[SB_DCL_ERR(sb_idx)]);

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7d %-16s= %-8s\n",
           "SB_DEF_COLUMN", SB_DEF_COLUMN(sb_idx),
           "SB_DEF_LINE", SB_DEF_LINE(sb_idx),
           "SB_DEF_MULT_SCPS", boolean_str[SB_DEF_MULT_SCPS(sb_idx)]);

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8d\n",
           "SB_DUPLICATE_COM", boolean_str[SB_DUPLICATE_COMMON(sb_idx)],
           "SB_EQUIVALENCED", boolean_str[SB_EQUIVALENCED(sb_idx)],
           "SB_FIRST_ATTR_ID", SB_FIRST_ATTR_IDX(sb_idx));

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
           "SB_HAS_RENAMES", boolean_str[SB_HAS_RENAMES(sb_idx)],
           "SB_HIDDEN", boolean_str[SB_HIDDEN(sb_idx)],
           "SB_HOST_ASSOCIAT", boolean_str[SB_HOST_ASSOCIATED(sb_idx)]);

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
           "SB_HOSTED_STACK", boolean_str[SB_HOSTED_STACK(sb_idx)],
           "SB_HOSTED_STATIC", boolean_str[SB_HOSTED_STATIC(sb_idx)],
           "SB_IS_COMMON", boolean_str[SB_IS_COMMON(sb_idx)]);

   print_fld_idx(out_file, "SB_LEN_IDX",
                 SB_LEN_FLD(sb_idx), SB_LEN_IDX(sb_idx));

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7d %-16s= %-8s\n",
           "SB_LAST_ATTR_LIS", SB_LAST_ATTR_LIST(sb_idx),
           "SB_MERGED_BLK_ID", SB_MERGED_BLK_IDX(sb_idx),
           "SB_MODULE", boolean_str[SB_MODULE(sb_idx)]);

   if (SB_MODULE(sb_idx)) {

      if (SB_MODULE_IDX(sb_idx) == NULL_IDX) {
         fprintf(out_file, "  %-16s= %-7d\n",
                 "SB_MODULE_IDX", SB_MODULE_IDX(sb_idx));
      }
      else {
         fprintf(out_file, "  %-16s= %-7d %-33s\n",
                 "SB_MODULE_IDX", SB_MODULE_IDX(sb_idx),
                 print_at_name(SB_MODULE_IDX(sb_idx)));
      }
   }

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7d %-16s= %-8d\n",
           "SB_NAME_LEN", SB_NAME_LEN(sb_idx),
           "SB_ORIG_SCP_IDX", SB_ORIG_SCP_IDX(sb_idx),
#ifdef KEY /* Bug 14150 */
           "SB_EXT_NAME_IDX", SB_EXT_NAME_IDX(sb_idx)
#else /* KEY Bug 14150 */
           "SB_PAD_AMOUNT", SB_PAD_AMOUNT(sb_idx)
#endif /* KEY Bug 14150 */
	   );
#ifdef KEY /* Bug 14150 */
   if (SB_EXT_NAME_IDX(sb_idx)) {
     fprintf(out_file, "  %-16s= %-7d %-16s= %-16s\n",
       "SB_EXT_NAME_LEN", SB_EXT_NAME_LEN(sb_idx),
       "SB_EXT_NAME", SB_EXT_NAME_PTR(sb_idx));
   }
#endif /* KEY Bug 14150 */

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
           "SB_PAD_AMOUNT_SE", boolean_str[SB_PAD_AMOUNT_SET(sb_idx)],
           "SB_PAD_BLK", boolean_str[SB_PAD_BLK(sb_idx)],
           "SB_RUNTIME_INIT", boolean_str[SB_RUNTIME_INIT(sb_idx)]);

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7d %-16s= %-8s\n",
           "SB_SAVED", boolean_str[SB_SAVED(sb_idx)],
           "SB_SCP_IDX", SB_SCP_IDX(sb_idx),
           "SB_SECTION_GP", boolean_str[SB_SECTION_GP(sb_idx)]);

   fprintf(out_file, "  %-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
           "SB_SECTION_NON_", boolean_str[SB_SECTION_NON_GP(sb_idx)],
           "SB_SYMMETRIC", boolean_str[SB_SYMMETRIC(sb_idx)],
           "SB_USE_ASSOCIATE", boolean_str[SB_USE_ASSOCIATED(sb_idx)]);

   fprintf(out_file, "  %-16s= %-7s\n",
           "SB_VOLATILE", boolean_str[SB_VOLATILE(sb_idx)]);

   putc ('\n', out_file);

   fflush (out_file);
   return;

}  /* dump_sb_ntry */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints one scope stack entry to the specified output file.            *|
|*									      *|
|* Input parameters:							      *|
|*	FILE to print to - Should be debug_file, stderr, or stdout.           *|
|*	Index of scope stack entry to print.                                  *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void dump_scp_ntry (FILE		*out_file,
   		    	   int		scp_idx,
	                   int		shift_cnt,
		    	   boolean	print_impl_tbl,
			   boolean	print_all_children)

{
   char		ch;
   int		idx;
   int		save_scp_idx;
   char		shift[80];


PROCESS_SIBLING:

   if (scp_idx > scp_tbl_idx) {
      fprintf(out_file,"\n*FE90-ERROR* SCP index value [%d] is out of range.\n",
              scp_idx);
      return;
   }

   if (shift_cnt > 45) {
      fprintf(out_file, "\nFE90 - NESTING is too DEEP\n");
      shift_cnt = 45;
   }

   for (idx = 0; idx < shift_cnt; idx++) {
      shift[idx] = ' ';
   }
   shift[shift_cnt] = '\0';

   if (SCP_ATTR_IDX(scp_idx) != NULL_IDX) {
      fprintf(out_file, "%s%-32.32s", shift,
              AT_OBJ_NAME_PTR(SCP_ATTR_IDX(scp_idx)));
   }
   else if (scp_idx == INTRINSIC_SCP_IDX) {
      fprintf(out_file, "%s%-32.32s", shift, "*** INTRINSIC SCOPE ***");
   }
   else {
      fprintf(out_file, "%s%-32.32s", shift, "*** scope has no name ***");
   }

   if (SCP_IN_ERR(scp_idx)) {
      fprintf(out_file, "%5s%s", " ", "*** SCOPE IN ERROR ***");
   }

   fprintf(out_file,"\n%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "IDX", scp_idx,
           "SCP_ALT_ENTRY_CNT", SCP_ALT_ENTRY_CNT(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_ASSIGN_LBL_CHAIN", SCP_ASSIGN_LBL_CHAIN(scp_idx),
           "SCP_ATTR_IDX", SCP_ATTR_IDX(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_ATTR_LIST", SCP_ATTR_LIST(scp_idx),
           "SCP_ATTR_LIST_EN", SCP_ATTR_LIST_END(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d\n", " ",
           "SCP_CIF_ERR_LIST", SCP_CIF_ERR_LIST(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9s\n", " ",
           "SCP_CIF_ID", SCP_CIF_ID(scp_idx),
           "SCP_COPY_ASSUMED_SHA",boolean_str[SCP_COPY_ASSUMED_SHAPE(scp_idx)]);

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_COPY_ASSUMED_LIS", SCP_COPY_ASSUMED_LIST(scp_idx),
           "SCP_DARG_LIST", SCP_DARG_LIST(scp_idx));


   fprintf(out_file,"%18s%-20s= %-27s\n", " ",
           "SCP_DEFAULT_STORAGE", 
           sb_blk_type_str[SCP_DBG_PRINT_SYTB(scp_idx)]);

   fprintf(out_file,"%18s%-20s= %-7s  %-20s= %-9d\n", " ",
           "SCP_DOES_IO", boolean_str[SCP_DOES_IO(scp_idx)],
           "SCP_ENTRY_IDX", SCP_ENTRY_IDX(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_EXIT_IR_SH_IDX", SCP_EXIT_IR_SH_IDX(scp_idx),
           "SCP_FILE_PATH_IDX", SCP_FILE_PATH_IDX(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_FIRST_CHILD_IDX", SCP_FIRST_CHILD_IDX(scp_idx),
           "SCP_FIRST_EQUIV_GRP", SCP_FIRST_EQUIV_GRP(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9s\n", " ",
           "SCP_FIRST_SH_IDX", SCP_FIRST_SH_IDX(scp_idx),
           "SCP_HAS_CALLS", boolean_str[SCP_HAS_CALLS(scp_idx)]);

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_HN_FW_IDX", SCP_HN_FW_IDX(scp_idx),
           "SCP_HN_LW_IDX", SCP_HN_LW_IDX(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7s  %-20s= %-9s\n", " ",
           "SCP_IGNORE_TKR", boolean_str[SCP_IGNORE_TKR(scp_idx)],
           "SCP_IMPL_NONE", boolean_str[SCP_IMPL_NONE(scp_idx)]);

   fprintf(out_file,"%18s%-20s= %-7s  %-20s= %-9s\n", " ",
           "SCP_IN_ERR", boolean_str[SCP_IN_ERR(scp_idx)],
           "SCP_IS_INTERFACE", boolean_str[SCP_IS_INTERFACE(scp_idx)]);

   fprintf(out_file,"%18s%-20s= %-7s  %-20s= %-9d\n", " ",
           "SCP_IS_USED_PROC", boolean_str[SCP_IS_USED_PROC(scp_idx)],
           "SCP_LAST_CHILD_IDX", SCP_LAST_CHILD_IDX(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_LAST_SH_IDX", SCP_LAST_SH_IDX(scp_idx),
           "SCP_LEVEL", SCP_LEVEL(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_LN_FW_IDX", SCP_LN_FW_IDX(scp_idx),
           "SCP_LN_LW_IDX", SCP_LN_LW_IDX(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_NUM_CHILDREN", SCP_NUM_CHILDREN(scp_idx),
           "SCP_OPTIONAL_CHAR_TM", SCP_OPTIONAL_CHAR_TMP(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9s\n", " ",
           "SCP_PARENT_IDX", SCP_PARENT_IDX(scp_idx),
           "SCP_PARENT_NONE", boolean_str[SCP_PARENT_NONE(scp_idx)]);

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_RETURN_LABEL", SCP_RETURN_LABEL(scp_idx),
           "SCP_SB_BASED_IDX", SCP_SB_BASED_IDX(scp_idx));


   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_SB_HOSTED_DATA", SCP_SB_HOSTED_DATA_IDX(scp_idx),
           "SCP_SB_HOSTED_STAC", SCP_SB_HOSTED_STACK_IDX(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_SB_HOSTED_STAT", SCP_SB_HOSTED_STATIC_IDX(scp_idx),
           "SCP_SB_STACK_IDX", SCP_SB_STACK_IDX(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_SB_STATIC_IDX", SCP_SB_STATIC_IDX(scp_idx),
           "SCP_SB_STATIC_INIT", SCP_SB_STATIC_INIT_IDX(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_SB_STATIC_UNINIT", SCP_SB_STATIC_UNINIT_IDX(scp_idx),
           "SCP_SB_SYMMETRIC", SCP_SB_SYMMETRIC_IDX(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_SIBLING_IDX", SCP_SIBLING_IDX(scp_idx),
           "SCP_RESHAPE_ARRA", SCP_RESHAPE_ARRAY_LIST(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_TMP_FW_IDX", SCP_TMP_FW_IDX(scp_idx),
           "SCP_TMP_FW_IDX2", SCP_TMP_FW_IDX2(scp_idx));

   fprintf(out_file,"%18s%-20s= %-7d  %-20s= %-9d\n", " ",
           "SCP_TMP_LIST", SCP_TMP_LIST(scp_idx),
           "SCP_USED_MODULE_LIST", SCP_USED_MODULE_LIST(scp_idx));
#ifdef KEY /* Bug 5089 */
   fprintf(out_file,"%18s%-20s= %-7s\n", " ",
           "SCP_USES_IEEE", boolean_str[SCP_USES_IEEE(scp_idx)]);
#endif /* KEY Bug 5089 */


   if (print_impl_tbl) {
      for (idx = 0; idx < MAX_IMPL_CHS; idx++) {
         ch = 'A' + idx;
         fprintf(out_file,"%18s%c %24s %-16s= %-9s\n", " ",
  	            ch, " ", "IM_SET", boolean_str[IM_SET(scp_idx, idx)]);
         dump_typ_ntry(out_file, IM_TYPE_IDX(scp_idx,idx));
         fprintf(out_file,"%44s %-16s= %-9s\n", " ",
                 "IM_STORAGE", implicit_storage_str[IM_STORAGE(scp_idx, idx)]);
      }
   }
   putc ('\n', out_file);

   if (print_all_children) {

      if (SCP_FIRST_CHILD_IDX(scp_idx) != NULL_IDX) {
         save_scp_idx	= scp_idx;
         scp_idx	= SCP_FIRST_CHILD_IDX(scp_idx);
         shift_cnt	= shift_cnt + 5;
         dump_scp_ntry(out_file, 
                       scp_idx,
                       shift_cnt,
                       print_impl_tbl,
                       TRUE);
         scp_idx = save_scp_idx;
      }

      if (SCP_SIBLING_IDX(scp_idx) != NULL_IDX) {
         scp_idx = SCP_SIBLING_IDX(scp_idx);
         goto PROCESS_SIBLING;
      }
   }

   fflush (out_file);
   return;

}  /* dump_scp_ntry */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints a single Secondary Name table entry.                           *|
|*									      *|
|* Input parameters:							      *|
|*	Index of Secondary Name table entry to print.                         *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void dump_sn_ntry (FILE 	*out_file,
			  int 	 sn_idx)

{
   if (sn_idx > sec_name_tbl_idx) {
      fprintf(out_file, "\n*FE90-ERROR* SN index value [%d] is out of range.\n",
              sn_idx);
      return;
   }

   fprintf(out_file, "  %-51s", SN_NAME_PTR(sn_idx)); 

   fprintf(out_file, " %-16s= %-8d\n", "            IDX", sn_idx);

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7d %-16s= %-8d\n",
           "SN_COLUMN_NUM", SN_COLUMN_NUM(sn_idx),
           "SN_LINE_NUM",   SN_LINE_NUM(sn_idx),
           "SN_NAME_LEN",   SN_NAME_LEN(sn_idx));

  /* Only generic interface, operator interface, and namelist group   */
  /* Secondary Name table entries are linked together via the         */
  /* SN_SIBLING_LINK field.                                           */


   if (comp_phase == Decl_Semantics) {
      fprintf(out_file, "  %-16s= %-7s\n",
        "SN_MATCHED_DARG", boolean_str[SN_MATCHED_DARG(sn_idx)]);
   }

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7d %-16s= %-8d\n",
        "SN_NAME_IDX",     SN_NAME_IDX(sn_idx),
        "SN_ATTR_IDX",     SN_ATTR_IDX(sn_idx),
        "SN_SIBLING_LINK", SN_SIBLING_LINK(sn_idx));

   fflush(out_file);

   return;

}  /* dump_sn_ntry */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
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

static void dump_stmt_ntry(FILE		*out_file,
			   boolean	 print_stmt_ir)
{
  
   dump_trace_info(out_file, Stmt_Start, NULL, "SH_dump");

   fprintf(out_file, "IDX = %-7d   %s = %-7d     %s = %d   %s = %d\n",
                     curr_stmt_sh_idx,
                     "PREV SH IDX", SH_PREV_IDX(curr_stmt_sh_idx),
                     "NEXT SH IDX", SH_NEXT_IDX(curr_stmt_sh_idx),
                     "COL NUM",     SH_COL_NUM(curr_stmt_sh_idx));

   if (SH_LABELED(curr_stmt_sh_idx)  && 
       ! (SH_COMPILER_GEN(curr_stmt_sh_idx)  &&
          SH_STMT_TYPE(curr_stmt_sh_idx) == Continue_Stmt)) {
      fprintf(out_file, "   *Stmt is labeled*\n");
   }

   fprintf(out_file, "%16s%s = %-7d  %-11s = %s  %15s = %s\n", " ",
                     "PARENT BLK IDX", SH_PARENT_BLK_IDX(curr_stmt_sh_idx),
                     "LOOP END", boolean_str[SH_LOOP_END(curr_stmt_sh_idx)],
                     "DOALL LOOP END", 
                     boolean_str[SH_DOALL_LOOP_END(curr_stmt_sh_idx)]);

   if (print_stmt_ir  &&  SH_IR_IDX(curr_stmt_sh_idx) != NULL_IDX) {
      dump_ir_ntry(out_file, SH_IR_IDX(curr_stmt_sh_idx), 1);
   }

   return;

} /* dump_stmt_ntry */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*									      *|
|* Input parameters:							      *|
|*	FILE to print to - Should be debug_file, stderr, or stdout.           *|
|*	Index of module file table entry to print.                            *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void dump_typ_ntry(FILE	*out_file,
   		          int	 type_idx)
 
{
   char		conv_str[80];


   if (type_idx > type_tbl_idx) {
      fprintf(out_file,"\n*FE90-ERROR* TYP index value [%d] is out of range.\n",
              type_idx);
      return;
   }

   fprintf(out_file,"  %-25s %-25s %-26s\n",
           basic_type_str[TYP_TYPE(type_idx)],
           lin_type_str[TYP_LINEAR(type_idx)],
           type_desc_str[TYP_DESC(type_idx)]);

   fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8d\n",
           "TYP_DCL_VALUE", TYP_DCL_VALUE(type_idx),
           "TYP_DP_HIT_ME", boolean_str[TYP_DP_HIT_ME(type_idx)],
           "IDX", type_idx);

   if (TYP_TYPE(type_idx) == Character) {

      fprintf(out_file,"  %-25s %-16s= %-8s\n",
              type_char_class_str[TYP_CHAR_CLASS(type_idx)],
              "TYP_RESOLVED", boolean_str[TYP_RESOLVED(type_idx)]);

      print_fld_idx(out_file, "TYP_IDX",
                    TYP_FLD(type_idx),
                    TYP_IDX(type_idx));

      if (TYP_ORIG_LEN_IDX(type_idx) != NULL_IDX) {
         print_fld_idx(out_file, "TYP_ORIG_LEN_IDX",
                       TYP_FLD(type_idx),
                       TYP_ORIG_LEN_IDX(type_idx));
      }
   }
   else if (TYP_TYPE(type_idx) == CRI_Ptr) {
      fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-8d\n",
           "TYP_IDX", TYP_IDX(type_idx),
           "TYP_RESOLVED", boolean_str[TYP_RESOLVED(type_idx)],
           "TYP_PTR_INCREMEN", (int) TYP_PTR_INCREMENT(type_idx));
   }
   else {
      fprintf(out_file, "  %-16s= %-7d %-16s= %-7s %-16s= %-7s\n",
              "TYP_IDX", TYP_IDX(type_idx),
              "TYP_KIND_CONST", boolean_str[TYP_KIND_CONST(type_idx)],
              "TYP_KIND_DOUBLE", boolean_str[TYP_KIND_DOUBLE(type_idx)]);

      fprintf(out_file, "  %-16s= %-7s %-16s= %-s\n",
              "TYP_RESOLVED", boolean_str[TYP_RESOLVED(type_idx)],
              "TYP_BIT_LEN", CONVERT_CVAL_TO_STR(&TYP_BIT_LEN(type_idx),
                                                  Integer_8,
                                                  conv_str));
   }

   putc ('\n', out_file);

   fflush (out_file);
   return;

}  /* dump_typ_ntry */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
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

static void dump_io_type_code_ntry(FILE         *out_file,
                                   long_type    *value,
                                   int          indent)

{
   long_type    dec_len = 0;
   int          dp_flag = 0;
   int          dv_type;
   long_type    int_len = 0;
   int          kind_star = 0;
   char         shift[80];
   int          i;

   f90_type_t   *type_code;

   TRACE (Func_Entry, "dump_io_type_code_ntry", NULL);

   for (i = 0; i < INDENT_SIZE * indent; i++) {
      shift[i] = ' ';
      if (i == 79)
         break;
   }
   shift[i] = '\0';

# ifdef _TYPE_CODE_64_BIT

   type_code = (f90_type_t *)value;


   dv_type = type_code->type;

   dp_flag = type_code->dpflag;

   kind_star = type_code->kind_or_star;

   int_len = type_code->int_len;

   dec_len = type_code->int_len;
# else

   dv_type = ((*value) >> DV_TYPE_SHIFT) & 0xFF;

   dp_flag = ((*value) >> DV_DP_SHIFT) & 1;

   kind_star = ((*value) >> DV_KIND_STAR_SHIFT) & 07;

   int_len = ((*value) >> DV_INT_LEN_SHIFT) & 0xFFF;

   dec_len = ((*value) >> DV_DEC_LEN_SHIFT) & 0xFF;
# endif

   switch (dv_type) {
   case DV_TYPELESS:
      fprintf(out_file, "%sDV_TYPELESS  ", shift);
      break;
   case DV_INTEGER:
      fprintf(out_file, "%sDV_INTEGER  ", shift);
      break;
   case DV_REAL:
      fprintf(out_file, "%sDV_REAL  ", shift);
      break;
   case DV_COMPLEX:
      fprintf(out_file, "%sDV_COMPLEX  ", shift);
      break;
   case DV_LOGICAL:
      fprintf(out_file, "%sDV_LOGICAL  ", shift);
      break;
   case DV_ASCII_CHAR:
      fprintf(out_file, "%sDV_ASCII_CHAR  ", shift);
      break;
   case DV_ASCII_CHAR_SEQUENCE_STRUCT:
      fprintf(out_file, "%sDV_ASCII_CHAR_SEQUENCE_STRUCT  ", shift);
      break;
   case DV_STRUCT:
      fprintf(out_file, "%sDV_STRUCT  ", shift);
      break;
   case DV_BIT:
      fprintf(out_file, "%sDV_BIT  ", shift);
      break;
   case DV_2_BYTE_CHAR:
      fprintf(out_file, "%sDV_2_BYTE_CHAR  ", shift);
      break;
   case DV_2_BYTE_CHAR_SEQUENCE_STRUCT:
      fprintf(out_file, "%sDV_2_BYTE_CHAR_SEQUENCE_STRUCT  ", shift);
      break;
   case DV_4_BYTE_CHAR:
      fprintf(out_file, "%sDV_4_BYTE_CHAR  ", shift);
      break;
   case DV_4_BYTE_CHAR_SEQUENCE_STRUCT:
      fprintf(out_file, "%sDV_4_BYTE_CHAR_SEQUENCE_STRUCT  ", shift);
      break;
   default:
      fprintf(out_file, "\n*FE90-ERROR* bad dv_type from io_type code\n");
      break;
   }

   if (dp_flag) {
      fprintf(out_file, "DP = 1 ");
   }
   else {
      fprintf(out_file, "DP = 0 ");
   }

   switch (kind_star) {
   case DV_DEFAULT_TYPED :
      fprintf(out_file, "DEFAULT TYPED ");
      break;

   case DV_KIND_TYPED :
      fprintf(out_file, "KIND_TYPED ");
      break;

   case DV_STAR_TYPED :
      fprintf(out_file, "STAR_TYPED ");
      break;

   case DV_KIND_CONST :
      fprintf(out_file, "KIND_CONST ");
      break;

   case DV_KIND_DOUBLE :
      fprintf(out_file, "KIND_DOUBLE ");
      break;


   default :
      fprintf(out_file, "***INVALID*** ");
      break;

   }

#if defined(_HOST32) && defined(_TARGET64)
   fprintf(out_file,"INT_LEN = %" LONG_TYPE_FMT " ", int_len);
   fprintf(out_file,"DEC_LEN = %" LONG_TYPE_FMT " ", dec_len);
#else
   fprintf(out_file,"INT_LEN = %ld ", int_len);
   fprintf(out_file,"DEC_LEN = %ld ", dec_len);
#endif


   fprintf(out_file, "\n");

   TRACE (Func_Exit, "dump_io_type_code_ntry", NULL);

   return;

}  /* dump_io_type_code_ntry */

#ifdef KEY
void debug_to_stderr() {
  debug_file = stderr;
}
#endif /* KEY */

# endif
