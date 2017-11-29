/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/fecif.c	5.9	10/14/99 12:53:57\n";

# include "defines.h"		/* Machine dependent ifdefs */


# define __NLS_INTERNALS 1  /* Obtain internal <nl_types.h> definitions.      */
			    /* (Required to get at prototype for              */
			    /* __cat_path_name.)			      */
# include <nl_types.h>      /* Contains typedef for nl_catd and prototype for */
			    /* __cat_path_name.				      */

# if defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN)
# include <nlcatmsg.h>
# endif



# include <time.h>


# define CIF_VERSION     3

# include "cif.h"

# include "cifprocs.h"


# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "p_globals.m"
# include "s_globals.m"
# include "debug.m"
# include "cif.m"
# include "fecif.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "p_globals.h"
# include "s_globals.h"
# include "fecif.h"

# if defined(_HOST_OS_LINUX)
#   include <sys/sysinfo.h>
# elif defined(_HOST_OS_SOLARIS) || defined(_HOST_OS_IRIX)
#   include <sys/systeminfo.h>
# elif defined(_HOST_OS_DARWIN)
#   include <sys/param.h>
# endif


/*****************************************************************\
|* Function prototypes of static functions declared in this file *|
\*****************************************************************/

static int	cif_data_type(int);
static void 	cif_flush_include_recs (void);
static int  	get_line_and_file_id (int, int *);
static void	output_minimal_object_rec (int);
static void	process_attr_list (int, boolean);
static boolean  output_struct_ids(opnd_type *);

static char	output_buf[2][64];

# define outbuf1 output_buf[0]
# define outbuf2 output_buf[1]


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Open the Compiler Information File and output the header record.      *|
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

void init_cif(char *comp_date_time, char *release_level)
{
   		char		 cif_date[9];
   		char		 cif_time[9];
   		char		 cpu_name[MAXHOSTNAMELEN + 1];
   		char		 month[4];
   		int		 save_cif_file_id;
   		char		*msg_cat_name;

# if defined(_GETPMC_AVAILABLE)
   extern        int      GETPMC(long *, char *);   /* UNICOS library routine */

   union  {long   int_form;
           char   char_form[9];
           } host_cpu_type;

   union  host_machine_entry	{struct  {long		mcpmt;
					  Ulong		unused[127];
					  } fld;
				long     host_tbl[128];
				};

   typedef union host_machine_entry	host_machine_type;

   host_machine_type	host_machine_info;

# elif defined(_HOST_OS_SOLARIS) || (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))
   char		host_cpu_type[9];              /* Max of 8 chars (plus NULL). */
# endif


   TRACE (Func_Entry, "init_cif", NULL);

   cif_end_unit_column	       = 0;
   cif_file_id                 = 2;	       /* Reserve 1 for msg cat name. */
   cif_first_pgm_unit          = TRUE;
   cif_need_unit_rec           = TRUE;
   cif_pgm_unit_error_recovery = FALSE;
   cif_pgm_unit_start_line     = 1;


   /* If the CIF name was provided by the command line processor, open it.    */
   /* If the user specified -C, set_prog_file_names has already created the   */
   /* .T name in cif_name. 						      */
   /* Otherwise, get a temporary file.					      */

   if ((cif_C_opts & CMD_PROVIDED_CIF)  ||  cif_flags != 0) {

      if ((cif_actual_file = fopen(cif_name, "w")) == NULL) {
         PRINTMSG(0, 556, Log_Error, 0);
         perror("Reason");

# ifdef _DEBUG

         fprintf(stderr, "  Trying to open file %s\n", cif_name);
         system("df /tmp");

# endif

         exit_compiler(RC_USER_ERROR);
      }
   }
   else {

      if (! get_temp_file("w+", &cif_actual_file, cif_name)) {
         PRINTMSG(1, 556, Log_Error, 0);
         perror("  Reason");

# ifdef _DEBUG

         fprintf(stderr, "  Trying to open file %s\n", cif_name);
         system("df /tmp");

# endif

         exit_compiler(RC_USER_ERROR);
      }
   }

   c_i_f = cif_actual_file;


   /* Create a temporary file to save records that are output while the       */
   /* first stmt of a program unit is being parsed.  (All records for a       */
   /* program unit must be between the Unit and End Unit records for the      */
   /* program unit.)					          	      */

   if (! get_temp_file("w+", &cif_tmp_file, cif_tmp_file_name)) {
      PRINTMSG(0, 556, Log_Error, 0);
      perror("Reason");

# ifdef _DEBUG

      fprintf(stderr, "  Trying to open file %s\n", cif_name);
      system("df /tmp");

# endif

      if (c_i_f == cif_actual_file) {
         /* prevent closing the same file twice. Linux does not handle it */
         cif_actual_file = NULL;
      }

      fclose(c_i_f);

      if (! (cif_C_opts & CMD_PROVIDED_CIF)) {
         remove(cif_name);
      }

      exit_compiler(RC_USER_ERROR);
   }


   /* ----------------------------------------------------------------------- */
   /* Output the CIF header record.					      */
   /*									      */
   /* First, brute-force the date from the format in comp_date_time to the    */
   /* format CIF expects:  Ddd Mmm dd, yyyy  ->  mm/dd/yy                     */
   /* ----------------------------------------------------------------------- */

   memcpy(month, comp_date_time+4, 3);

   switch (month[0]) {

      case 'A': 
         strcpy(cif_date, (month[1] == 'p') ? "04/" : "08/");
         break;

      case 'D':
         strcpy(cif_date, "12/");
         break;

      case 'F':
         strcpy(cif_date, "02/");
         break;

      case 'J': 
         if (month[1] == 'a') {
            strcpy(cif_date, "01/");
         }
         else {
            strcpy(cif_date, (month[2] == 'n') ? "06/" : "07/");
         }
         break;

      case 'M':
         strcpy(cif_date, (month[2] == 'r') ? "03/" : "05/");
         break;
   
      case 'N':
         strcpy(cif_date, "11/");
         break;

      case 'O': 
         strcpy(cif_date, "10/");
         break;

      case 'S':
         strcpy(cif_date, "09/");
   }

   cif_date[3] = (comp_date_time[8] == ' ') ? '0' : comp_date_time[8];
   cif_date[4] = comp_date_time[9];
   cif_date[5] = '/';
   cif_date[6] = comp_date_time[14];
   cif_date[7] = comp_date_time[15];
   cif_date[8] = EOS;

   memcpy(cif_time, comp_date_time+18, 8);
   cif_time[8] = NULL_CHAR;

# if defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN)
   msg_cat_name = "shouldnotgethere";
# else
   msg_cat_name = (char *) __cat_path_name(msg_sys);
# endif
# if defined(_HOST_OS_LINUX)
   strcpy(cpu_name, "LINUX");


# elif defined(_HOST_OS_DARWIN)
   strcpy(cpu_name, "DARWIN");
# elif defined(_HOST_OS_SOLARIS) || defined(_HOST_OS_IRIX)

   if (sysinfo(SI_HOSTNAME, cpu_name, ((long int) MAXHOSTNAMELEN)) < 0L) {
      Cif_Error();
   }

# else

   if (gethostname(cpu_name, (MAXHOSTNAMELEN + 1)) < 0) {
      Cif_Error();
   }

# endif


# if defined(_GETPMC_AVAILABLE)
   GETPMC (host_machine_info.host_tbl, "HOST");
   host_cpu_type.int_form = host_machine_info.fld.mcpmt;
   host_cpu_type.char_form[8] = NULL_CHAR;
# elif defined(_HOST_OS_SOLARIS)
   strcpy(host_cpu_type, "SPARC");
# elif (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX))
   strcpy(host_cpu_type, "SGI");
# elif defined(_HOST_OS_DARWIN)
   strcpy(host_cpu_type, "MAC");
# endif

   Cif_Cifhdr_Rec(c_i_f,
                  CIF_LG_F90,
                  release_level,
                  cif_date,
                  cif_time,
                  group_code,
                  1,			/* Message catalog file id.	      */
                  cpu_name,

# if defined(_GETPMC_AVAILABLE)
                  host_cpu_type.char_form);
# elif defined(_HOST_OS_SOLARIS) || defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN)
                  host_cpu_type);
# endif

   Cif_Src_Pos_Rec(c_i_f,
                   CIF_SRC_KIND_MAIN,
                   2,
                   0,
                   0,
		   0,
		   2,
                   0,
                   0,
                   0);
		   

   save_cif_file_id = cif_file_id;
   cif_file_id      = 1;
   cif_file_name_rec(msg_cat_name, (char *) NULL);
   cif_file_id      = save_cif_file_id;

   if (cif_flags & COMPILER_RECS) {
      cif_enable_disable_rec();
      cif_misc_compiler_opts_rec();
      cif_optimization_opts_rec();
      cif_machine_characteristics_rec();
   }

   
   /* orig_cmd_line should only be NULL if the compiler is called directly    */
   /* (like in debug mode).  Even if it is NULL for another reason, there is  */
   /* no great harm; the Original Command Line record will just not be        */
   /* produced in the CIF.						      */

   if (orig_cmd_line != NULL) {
      Cif_Orig_Cmd(c_i_f, orig_cmd_line);
      MEM_FREE(orig_cmd_line);
   }


   /* Set the CIF to the temp file so that all records preceding the Unit     */
   /* record will go to the temp file.  cif_unit_rec will copy the temp       */
   /* file to the actual file so that these records will properly follow      */
   /* the Unit record.                                                        */

   c_i_f = cif_tmp_file;

   TRACE (Func_Exit, "init_cif", NULL);

   return;

} /*  init_cif  */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Perform initializations that need to be done for each program unit.   *|
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

void cif_prog_unit_init(void)
{

   TRACE (Func_Entry, "cif_prog_unit_init", NULL);

   cif_derived_type_id    = 101;
   cif_symbol_or_scope_id = 3;                 /* Reserve 1 for main program  */
                                               /*   scope ID.                 */
                                               /* Reserve 2 for main pgm name.*/
   SCP_CIF_ID(curr_scp_idx) =
      (BLK_TYPE(blk_stk_idx) == Program_Blk) ? 1 : NEXT_SCOPE_ID;

   cif_end_unit_column         = 0;
   cif_need_unit_rec           = TRUE;
   cif_pgm_unit_error_recovery = FALSE;

   c_i_f = cif_tmp_file;

   TRACE (Func_Exit, "cif_prog_unit_init", NULL);

   return;

}  /* cif_prog_unit_init */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Send the symbol table to CIF:					      *|
|*        - Go through the Storage Block table to find all storage blocks     *|
|*          that belong to the current scoping unit.  Produce a Common Block  *|
|*          record for each common block declared in the scoping unit.        *|
|*        - If "-ci" (or an option that includes "i") was specified, go       *|
|*          through the Local Name table to find ALL entities associated with *|
|*          the current scoping unit.  If "-Cf" was specified, go through the *|
|*          Local Name table to find all Pgm_Unit and Stmt_Func Attrs (to     *|
|*          produce Entry Point records) and to find all the interface blocks *|
|*          (to produce Interface Block records).			      *|
|*          Note:  Common Block records are also produced when only "-Cf" is  *|
|*                 specified.						      *|
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

void	cif_send_sytb()
{
   int		al_idx;
#ifdef KEY /* Bug 10177 */
   int		attr_idx = 0;
#else /* KEY Bug 10177 */
   int		attr_idx;
#endif /* KEY Bug 10177 */
   long_type	blk_len;
   int		module_symbol_id;
   int		name_idx;
   long_type	result[MAX_WORDS_FOR_INTEGER];
   int		sb_idx;
   int		stor_class;
   int		type_idx;


   TRACE (Func_Entry, "cif_send_sytb", NULL);

   for (sb_idx = 1; sb_idx <= stor_blk_tbl_idx; sb_idx++) {

      if (SB_SCP_IDX(sb_idx) != curr_scp_idx) { 
         continue;
      }

      if (SB_CIF_SYMBOL_ID(sb_idx) == 0) {
         SB_CIF_SYMBOL_ID(sb_idx) = NEXT_SYMBOL_ID;
      }

      if (SB_BLK_TYPE(sb_idx) == Common) {
         stor_class = CIF_CB_REG;
      }
      else if (SB_BLK_TYPE(sb_idx) == Task_Common) {
         stor_class = CIF_CB_TASK;
      }
      else {
         continue;
      }

      if (SB_USE_ASSOCIATED(sb_idx)) {

         if (AT_CIF_SYMBOL_ID(SB_MODULE_IDX(sb_idx)) == 0) {
            AT_CIF_SYMBOL_ID(SB_MODULE_IDX(sb_idx)) = NEXT_SYMBOL_ID;
         }

         module_symbol_id = AT_CIF_SYMBOL_ID(SB_MODULE_IDX(sb_idx));
      }
      else {
    
         /* If the common block is defined in a module, get the symbol id of  */
         /* the module name.  Each common block, whether it is host           */
         /* associated or not, will carry it's original SCP_ID.  Use this to  */
         /* determine if it came from a module.                               */
         /* Need to check for NULL_IDX because common blocks from interface   */
         /* bodies will have NULL_IDX for SB_ORIG_SCP_IDX.                    */

         module_symbol_id = 0;

         if (SB_ORIG_SCP_IDX(sb_idx) != NULL_IDX) {
            attr_idx = SCP_ATTR_IDX(SB_ORIG_SCP_IDX(sb_idx));

            if (ATP_PGM_UNIT(attr_idx) == Module) {
               module_symbol_id = AT_CIF_SYMBOL_ID(attr_idx);
            }
         }
      }

      /* The SB_LEN_IDX may not always be a constant.  */
      /* 0 is issued, if the length is variable.       */

      blk_len = 0;

      if (SB_LEN_FLD(sb_idx) == CN_Tbl_Idx) {
         type_idx = CN_TYPE_IDX(SB_LEN_IDX(sb_idx));

         if (folder_driver((char *) &CN_CONST(SB_LEN_IDX(sb_idx)),
                                     CN_TYPE_IDX(SB_LEN_IDX(sb_idx)),
                           (char *) &CN_CONST(CN_INTEGER_THREE_IDX),
                                     CN_TYPE_IDX(CN_INTEGER_THREE_IDX),
                                     result,
                                    &type_idx,
                                     SB_DEF_LINE(sb_idx),
                                     SB_DEF_COLUMN(sb_idx),
                                     2,
                                     Shiftr_Opr)) {
            blk_len = (long) F_INT_TO_C(result, TYP_LINEAR(type_idx));
         }
      }

      /* On the receiving end, blk_len is an int, so we can loose precision. */
      /* KAY */

      Cif_F90_Comblk_Rec(c_i_f,
                         SB_NAME_PTR(sb_idx),
                         SB_CIF_SYMBOL_ID(sb_idx),
                         SCP_CIF_ID(curr_scp_idx),
                         stor_class,
                         module_symbol_id,
                         blk_len,
                         0);
   }

   if (cif_flags & INFO_RECS) {

      for (name_idx = SCP_LN_FW_IDX(curr_scp_idx) + 1;
           name_idx < SCP_LN_LW_IDX(curr_scp_idx); name_idx++) {

         attr_idx = LN_ATTR_IDX(name_idx);

         /* Attrs that have AT_DCL_ERR set to TRUE will be dealt with in      */
         /* cif_send_attr.						      */
         /* A variable used as a loop control variable (LCV) in a DATA or     */
         /* array constructor implied-DO is local to thta implied-DO nest.    */
         /* If no variable with the same name exists outside the implied-DOs, */
         /* implied-DO processing creates an Attr entry for the variable in   */
         /* order to "borrow" the data type for the implied-DO LCV.  Implied- */
         /* DO processing then creates a temp with the same name as the       */
         /* variable (to make the implied-DO LCV local to the implied-DO) and */
         /* uses the temp as the LCV.  This special case temp must be sent    */
         /* through cif_send_attr to have an Object record produced for it.   */
         /* If no variable of the same name ever appears anywhere else in     */
         /* the program unit outside the implied-DOs, the "master" Attr       */
         /* becomes redundant and must not be sent through cif_send_attr.     */
         /* (If this extra Attr is sent through cif_send_attr, it will appear */
         /* in a xref listing as an implicitly declared variable with no      */
         /* references - an impossibility).  So, to weed out such extra Attrs,*/
         /* we look for:						      */
         /*   * a variable   [ AT_OBJ_CLASS == Data_Obj  and 		      */
         /*                    ATD_CLASS == Variable                          */
         /*   * that only appeared as an implied-DO LCV			      */
         /*                  [ ATD_SEEN_OUTSIDE_IMP_DO == FALSE ]	      */
         /*   * and, in particular, appeared only as a DATA or array          */
         /*     constructor implied-DO LCV [ ATD_SEEN_AS_IO_LCV == FALSE ]    */

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj   &&
             ATD_CLASS(attr_idx) == Variable      &&
             ! ATD_SEEN_OUTSIDE_IMP_DO(attr_idx)  &&
             ! ATD_SEEN_AS_IO_LCV(attr_idx)) {       
            continue;
         }

         cif_send_attr(attr_idx, NULL_IDX);
      }

      process_attr_list(SCP_ATTR_LIST(curr_scp_idx), FALSE);
      process_attr_list(SCP_CIF_ERR_LIST(curr_scp_idx), TRUE);
   }
   else {

      /* "-Cf" was specified.						      */

      for (name_idx = SCP_LN_FW_IDX(curr_scp_idx) + 1;
           name_idx < SCP_LN_LW_IDX(curr_scp_idx); name_idx++) {

         attr_idx = LN_ATTR_IDX(name_idx);

         /* Only want to generate entry records. */

         if (AT_OBJ_CLASS(attr_idx) != Pgm_Unit   &&  
             AT_OBJ_CLASS(attr_idx) != Stmt_Func  &&
             AT_OBJ_CLASS(attr_idx) != Interface) {
            continue;
         }

         cif_send_attr(attr_idx, NULL_IDX);
      }

      /* Go through the Attr list to pick up Attrs for nongeneric (unnamed)   */
      /* interface blocks (and interface bodies inherited from a host scope   */
      /* when interface blocks with the same name are merged?).               */
      /* See also the comments preceding the analogous loop above.	      */

      al_idx = SCP_ATTR_LIST(curr_scp_idx);

      while (al_idx != NULL_IDX) {

         if ((AT_OBJ_CLASS(attr_idx) == Pgm_Unit  &&
              AT_CIF_SYMBOL_ID(AL_ATTR_IDX(al_idx)) == 0)  ||
             AT_OBJ_CLASS(attr_idx) == Interface) {
            cif_send_attr(AL_ATTR_IDX(al_idx), NULL_IDX);
         }

         al_idx = AL_NEXT_IDX(al_idx);
      }
   }

   TRACE (Func_Exit, "cif_send_sytb", NULL);

   return;

}  /* cif_send_sytb */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Produce records representing the entities associated with the current *|
|*      scoping unit.  							      *|
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

void	cif_send_attr(int	attr_idx,
		      int	dt_attr_idx)

{
#ifdef KEY /* Bug 10177 */
   long			attributes = 0;
#else /* KEY Bug 10177 */
   long			attributes;
#endif /* KEY Bug 10177 */
   int			bd_idx;
   char			buffer[160];
   char			char_len[20];
   int			darg_idx;
   linear_type_type	data_type;
   int			derived_type;
#ifdef KEY /* Bug 10177 */
   int			dt_idx = 0;
#else /* KEY Bug 10177 */
   int			dt_idx;
#endif /* KEY Bug 10177 */
   int			i;
   int			interface_idx;
   int			interface_type;
   int			namelist_idx;
#ifdef KEY /* Bug 10177 */
   int			num_dargs = 0;
#else /* KEY Bug 10177 */
   int			num_dargs;
#endif /* KEY Bug 10177 */
   int			num_namelist;
   char		       *obj_name_ptr;
   long64		offset;
   char			offset_buf[20];
#ifdef KEY /* Bug 10177 */
   int			pgm_unit_type = 0;
#else /* KEY Bug 10177 */
   int			pgm_unit_type;
#endif /* KEY Bug 10177 */
   int			pointer_id;
   int			rslt_id;
   int			rslt_idx;
   boolean		save_cif_done;
#ifdef KEY /* Bug 10177 */
   int			scope_id = 0;
#else /* KEY Bug 10177 */
   int			scope_id;
#endif /* KEY Bug 10177 */
   int			sn_idx;
   int			storage_class;
   int			storage_id;
   char			string[20];
   int			symbol_class;
   int			type_idx;


   TRACE (Func_Entry, "cif_send_attr", NULL);

   /* Skip this Attribute entry if:					      */
   /*   - It has already been processed.				      */
   /*   - It's for a compiler generated variable.			      */
   /*   - It is host associated and is not a program unit Attr.  Even if a    */
   /*     program unit Attr is host associated, it must be processed because  */
   /*     CIF needs a record for both the reference to the program unit and   */
   /*     the definition of the program unit.  Example:			      */
   /*									      */
   /*        module mod							      */
   /*									      */
   /*        contains							      */
   /*									      */
   /*           subroutine sub1(i)					      */
   /*              integer i						      */
   /*              ...							      */
   /*           end subroutine     					      */
   /*									      */
   /*           subroutine sub2(j)					      */
   /*              integer j						      */
   /*              call sub1(2)						      */
   /*           end subroutine						      */
   /*									      */
   /*        end module 						      */
   /*									      */
   /*     An Attr for SUB1 will exist at the module level so SUB2 can call it.*/
   /*     An Attr for SUB1 will also exist in SUB2 and be attr-linked to the  */
   /*     the Attr for SUB1 at the module level.  The module does not actually*/
   /*     reference SUB1 so no Entry record will be generated for SUB1 there. */
   /*     But there will be a definition Entry record generated within SUB1   */
   /*     and there must be a reference Entry record generated within SUB2 for*/
   /*     SUB1.  Thus, even though the Attr for SUB1 in SUB2 is attr-linked to*/
   /*     the Attr for SUB1 at the module level, the Attr in SUB2 must be     */
   /*     sent through cif_send_attr so that the reference Entry record will  */
   /*     show up in the proper scoping unit.				      */

   if (AT_CIF_DONE(attr_idx)) {
      goto EXIT;
   }

   if (!AT_CIF_IN_USAGE_REC(attr_idx) &&
       ((AT_COMPILER_GEND(attr_idx)  &&
        (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
         ATD_CLASS(attr_idx) != Compiler_Tmp  || 
         ! ATD_TMP_NEEDS_CIF(attr_idx))) ||
       (AT_ATTR_LINK(attr_idx) != NULL_IDX  &&
        AT_OBJ_CLASS(attr_idx) != Pgm_Unit))) {
      goto EXIT;
   }


   AT_CIF_DONE(attr_idx) = TRUE;


   switch (AT_OBJ_CLASS(attr_idx)) {

   /* ----------------------------------------------------------------------- */
   /*                              Data_Obj				      */
   /* ----------------------------------------------------------------------- */

   case Data_Obj:

      /* If the Attr entry is marked in error, produce an Object record       */
      /* anyway (with "symbol class" and most other fields set to 0 to        */
      /* indicate the record is incomplete) because we can't stop all Usage   */
      /* records from being produced (and Usage records must have a "defining"*/
      /* record).  All Usage records can not be stopped primarily because     */
      /* they are produced in the Syntax Pass while the Object record is      */
      /* produced in the Semantics Pass, and because they are generally       */
      /* produced as the objects are seen.  Example:			      */
      /*                         REAL i					      */
      /*                         INTEGER i				      */
      /* A Usage record is produced for I when the REAL stmt is parsed.  When */
      /* the INTEGER stmt is parsed, I has already been typed so an error is  */
      /* issued and AT_DCL_ERR for I is set to TRUE.  Since the Usage record  */
      /* already exists, an Object record must also exist.  libcif will       */
      /* ignore all records that relate to an Object record that is marked    */
      /* in error.							      */

      if (AT_DCL_ERR(attr_idx)) {
         output_minimal_object_rec(attr_idx);
         goto EXIT;
      }

      if (AT_CIF_SYMBOL_ID(attr_idx) == 0) {
         AT_CIF_SYMBOL_ID(attr_idx) = NEXT_SYMBOL_ID;
      }

      char_len[0]	= NULL_CHAR;
      type_idx		= ATD_TYPE_IDX(attr_idx);

      if (TYP_TYPE(type_idx) == Structure) {

         if (! AT_DCL_ERR(TYP_IDX(type_idx))) {
            dt_idx = (AT_ATTR_LINK(TYP_IDX(type_idx)) == NULL_IDX) ?
                     TYP_IDX(type_idx) : AT_ATTR_LINK(TYP_IDX(type_idx));
         }
         else {
            output_minimal_object_rec(attr_idx);
            goto EXIT;
         }


         /* If the CIF derived type id is 0, it means that the derived type   */
         /* was use associated (or made available by other means?) and not    */
         /* included in the LN table, and thus we have to send it through now */
         /* to get all of its members processed.  Normally, the declaration   */
         /* rules of Fortran 90 would require the derived type to have been   */
         /* defined prior to its being used in a declaration (which would     */
         /* normally mean that it would already have been seen and processed).*/

         if (ATT_CIF_DT_ID(dt_idx) == 0) {
            cif_send_attr(dt_idx, NULL_IDX);
         }


         data_type = (linear_type_type) ATT_CIF_DT_ID(dt_idx);
      }
      else {
         data_type = TYP_LINEAR(type_idx);

         if (TYP_TYPE(type_idx) == Character) {
 
            if (TYP_FLD(type_idx) == CN_Tbl_Idx) {
               convert_to_string(&CN_CONST(TYP_IDX(type_idx)),
                                  CN_TYPE_IDX(TYP_IDX(type_idx)),
                                  char_len);
            }
            else {
               char_len[0] = (TYP_CHAR_CLASS(type_idx) == Assumed_Size_Char) ?
                                                          ASSUMED_SIZE_CHAR :
                                                          VAR_LEN_CHAR;
               char_len[1] = NULL_CHAR;
            }
         }
      }


      obj_name_ptr = AT_OBJ_NAME_PTR(attr_idx);

      switch (ATD_CLASS(attr_idx)) {

      case Struct_Component:
         storage_class	= CIF_F90_ST_NO_STORAGE;
         storage_id	= 0;
         offset		= (ATD_OFFSET_FLD(attr_idx) == CN_Tbl_Idx) ? 
                           CN_INT_TO_C(ATD_CPNT_OFFSET_IDX(attr_idx)) : -1;
         symbol_class	= CIF_F90_SC_STRUCT;
         attributes	= 0;
         derived_type	= ATT_CIF_DT_ID(dt_attr_idx);
         break;

      case Constant:
         storage_class	= CIF_F90_ST_NO_STORAGE;

         if (AT_USE_ASSOCIATED(attr_idx)) {

            if (AT_CIF_SYMBOL_ID(AT_MODULE_IDX(attr_idx)) == 0) {

               /* If the module's symbol ID is 0, it probably means the       */
               /* module is being used indirectly.  If so, the module Attr    */
               /* won't exist in the current scope (or any parent scope) so   */
               /* send it through to get an Entry Point record generated to   */
               /* resolve the storage id field of the Object record currently */
               /* being constructed.					      */
 
               cif_send_attr(AT_MODULE_IDX(attr_idx), NULL_IDX);
            }

            storage_id = AT_CIF_SYMBOL_ID(AT_MODULE_IDX(attr_idx));
         }
         else if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {
            storage_id = AT_CIF_SYMBOL_ID(SCP_ATTR_IDX(curr_scp_idx)); 
         }
         else {
            storage_id = 0;
         }

         offset		= -1;
         symbol_class	= CIF_F90_SC_NAMED_CONST;
         attributes	= 0;
         derived_type	= 0;

         /* If the named constant is use associated, spit out a dummy Named   */
         /* Constant record (line and column numbers will be zero and value   */
         /* is meaningful only if it is a simple constant).		      */

         if (AT_USE_ASSOCIATED(attr_idx)) {
            cif_named_constant_rec(attr_idx, 0, 0);
         }

         break;

      case Function_Result:

         /* If get_other_func_rslt_info is TRUE, it means we're processing    */
         /* a function reference from one internal function to another or     */
         /* from one module function to another.  If the function result of   */
         /* the CALLED function is of derived type, we need to produce all    */
         /* the records necessary to represent the derived type (and any      */
         /* nested derived types) in the scoping unit of the CALLING function */
         /* because the referencing Entry Point record points to the Object   */
         /* record for the called function result which in turn contains the  */
         /* CIF derived type id.  We must satisfy the derived type id which   */
         /* means we need to provide all the records to do so.  To do this,   */
         /* we need to send all the Attrs representing the derived type in    */
         /* the CALLED function's scoping unit through cif_send_attr again.   */

         if (get_other_func_rslt_info) {

            if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure) {
               AT_CIF_DONE(dt_idx)      = FALSE;
               AT_CIF_SYMBOL_ID(dt_idx) = 0;
               ATT_CIF_DT_ID(dt_idx)    = 0;
               cif_send_attr(dt_idx, NULL_IDX);
               data_type = (linear_type_type) ATT_CIF_DT_ID(dt_idx);
            }
         }

           
         if (ATD_STOR_BLK_IDX(attr_idx) == NULL_IDX) {  /* Stmt func result */
            storage_class = CIF_F90_ST_NO_STORAGE;
            storage_id    = 0;
         }
         else {

            /* May be the hidden first dummy arg, rather than the result.     */

            storage_class =
               (SB_BLK_TYPE(ATD_STOR_BLK_IDX(attr_idx)) == Formal) ?
               CIF_F90_ST_DUMMY : CIF_F90_ST_STACK;

            /* A function result can be use associated if the function        */
            /* belongs to an interface block that was use associated.	      */

            storage_id = (AT_USE_ASSOCIATED(attr_idx)) ?
                            AT_CIF_SYMBOL_ID(AT_MODULE_IDX(attr_idx)) : 0;
         }

         offset		= -1;
         symbol_class	= CIF_F90_SC_FUNC_RESULT;
         attributes	= 0;
         derived_type	= 0;

         if (ATP_PROC(ATD_FUNC_IDX(attr_idx)) == Intrin_Proc  &&
             AT_OBJ_NAME(attr_idx) == '_') {
            ++obj_name_ptr;
         }

         break;

      case Dummy_Argument:
         derived_type	= 0;
         symbol_class	= CIF_F90_SC_VARIABLE;
         attributes	= (AT_CIF_USE_IN_BND(attr_idx)) ? CIF_DARG_IN_BND : 0;
         storage_id	= 0;

         if (ATD_SF_DARG(attr_idx)) {
            offset  	  = -1;
            storage_class = CIF_F90_ST_NO_STORAGE;
         }
         else {

            /* If the dummy arg name is in the main entry point's dummy arg   */
            /* list, make its offset its position within the list.  (If the   */
            /* same dummy arg is named in an alternate entry, the offset is   */
            /* meaningless.)  If the dummy arg appeared in an alternate entry */
            /* dummy arg list but not in the main dummy arg list, set its     */
            /* offset to 0.  						      */

            storage_class = CIF_F90_ST_DUMMY;
            offset        = 0;
            sn_idx        = ATP_FIRST_IDX(SCP_ATTR_IDX(curr_scp_idx));

            for (i = 1;  i <= ATP_NUM_DARGS(SCP_ATTR_IDX(curr_scp_idx));  ++i) {
               
               if (attr_idx == SN_ATTR_IDX(sn_idx)) {
                  offset = (ATP_EXTRA_DARG(SCP_ATTR_IDX(curr_scp_idx))) ?
                           (i - 1) : i;
                  break;
               }
               else {
                  ++sn_idx;
               }
            }
         }

         break;

      default:
         derived_type	= 0;
         symbol_class	= CIF_F90_SC_VARIABLE;
         attributes	= 0;
         offset		= (ATD_OFFSET_ASSIGNED(attr_idx) && 
                           ATD_OFFSET_FLD(attr_idx) == CN_Tbl_Idx) ?
                             CN_INT_TO_C(ATD_OFFSET_IDX(attr_idx)) : -1;

         storage_id = SB_CIF_SYMBOL_ID(ATD_STOR_BLK_IDX(attr_idx));

         switch (SB_BLK_TYPE(ATD_STOR_BLK_IDX(attr_idx))) {

         case Static:
         case Static_Local:
         case Static_Named:

            if (ATD_ALLOCATABLE(attr_idx)  ||  ATD_POINTER(attr_idx)) {
               storage_class = CIF_F90_ST_BASED;
            }
            else {
               storage_class = CIF_F90_ST_STATIC;
            }

            if (SB_MODULE(ATD_STOR_BLK_IDX(attr_idx))) {
               symbol_class = CIF_F90_SC_MODULE;

               storage_id = (AT_USE_ASSOCIATED(attr_idx)) ?
                               AT_CIF_SYMBOL_ID(AT_MODULE_IDX(attr_idx)) :
                               AT_CIF_SYMBOL_ID(SCP_ATTR_IDX(curr_scp_idx)); 
            }

            break;

         case Stack:
         case Equivalenced:
            if (ATD_ALLOCATABLE(attr_idx)  ||  ATD_POINTER(attr_idx)) {
               storage_class = CIF_F90_ST_BASED;
            }
            else {
               storage_class = (ATD_AUXILIARY(attr_idx) == 0) ?
                                  CIF_F90_ST_STACK : CIF_F90_ST_AUXILIARY;
            }

            break;

         case Common:
         case Task_Common:
            symbol_class = (ATD_EQUIV(attr_idx) && !ATD_IN_COMMON(attr_idx)) ?
                                                    CIF_F90_SC_EQUIV :
                                                    CIF_F90_SC_COMMON;
            storage_class = (ATD_AUXILIARY(attr_idx) == 0) ?
                               CIF_F90_ST_COMMON : CIF_F90_ST_AUXILIARY;
            break;

         case Formal:
            storage_class = CIF_F90_ST_DUMMY;
            break;

         case Based:
            storage_class = (ATD_CLASS(attr_idx) == CRI__Pointee) ?
                               CIF_F90_ST_POINTEE : CIF_F90_ST_BASED;
            break;

         default:
            storage_class = CIF_F90_ST_ERROR;
            break;
         }
         break;
      }

      /* Set attributes */

      switch (TYP_DESC(type_idx)) {
         case Default_Typed:
            attributes = attributes | CIF_DEFAULT_TYPED;
            break;

         case Star_Typed:
            attributes = attributes | CIF_STAR_TYPED;
            break;

         case Kind_Typed:
            attributes = attributes | CIF_KIND_TYPED;
            break;
      }

      if (!AT_TYPED(attr_idx)) {
         attributes = attributes | CIF_IMPLICITLY_TYPED;
      }

      if (ATD_SAVED(attr_idx)) {
         attributes = attributes | CIF_SAVED;
      }
                                             
      if (ATD_DATA_INIT(attr_idx)) {
         attributes = attributes | CIF_DATA_INIT;
         attributes = attributes | CIF_SAVED;	/* Implied by initialization. */
      }

      if (ATD_DCL_EQUIV(attr_idx)) {
         attributes = attributes | CIF_EQUIVALENCED;
      }

      if (ATD_ALLOCATABLE(attr_idx)) {
         attributes = attributes | CIF_ALLOCATABLE;
      }

      if (ATD_CLASS(attr_idx) == Dummy_Argument) {
       
         switch (ATD_INTENT(attr_idx)) {
           case Intent_Unseen:
              break;

           case Intent_In:
              attributes = attributes | CIF_INTENT_IN;
              break;

           case Intent_Out:
              attributes = attributes | CIF_INTENT_OUT;
              break;

           case Intent_Inout:
              attributes = attributes | CIF_INTENT_INOUT;
              break;
         }

         if (AT_OPTIONAL(attr_idx)) {
            attributes = attributes | CIF_OPTIONAL;
         }
      }

      pointer_id = 0;

      if (ATD_POINTER(attr_idx)) {
         attributes = attributes | CIF_POINTER;
      }
      else if (ATD_CLASS(attr_idx) == CRI__Pointee) { 
         attributes = attributes | CIF_CRI_POINTEE;

         if (AT_CIF_SYMBOL_ID(ATD_PTR_IDX(attr_idx)) == 0) {
            AT_CIF_SYMBOL_ID(ATD_PTR_IDX(attr_idx)) = NEXT_SYMBOL_ID;
         }

         pointer_id = AT_CIF_SYMBOL_ID(ATD_PTR_IDX(attr_idx));
      }

      if (AT_PRIVATE(attr_idx)) {
         attributes = attributes | CIF_PRIVATE;
      }

      if (ATD_TARGET(attr_idx)) {
         attributes = attributes | CIF_TARGET;
      }

      if (AT_USE_ASSOCIATED(attr_idx) && 
          AT_ORIG_NAME_IDX(attr_idx) != AT_NAME_IDX(attr_idx)) {
         attributes = attributes | CIF_RENAMED;
      }

      scope_id = SCP_CIF_ID(curr_scp_idx);

      if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX) {
         CONVERT_CVAL_TO_STR(&offset, Integer_8, offset_buf);

         if (fprintf(c_i_f, 
         "%d%c%s%c%d%c%d%c%d%c%d%c%d%c%d%c%s%c%lx%c%d%c%s%c%d%c%d%c%d%c%d%c%d%c",
                     CIF_F90_OBJECT, EOI, 
                     obj_name_ptr, EOI,
                     AT_CIF_SYMBOL_ID(attr_idx), EOI,
                     scope_id, EOI,
                     cif_data_type(data_type), EOI,
                     symbol_class, EOI,
                     storage_class, EOI,
                     storage_id, EOI,
                     offset_buf, EOI,
                     attributes, EOI,
                     derived_type, EOI,
                     char_len, EOI,
                     0, EOI,                                  /* num dims     */
                     0, EOI,				      /* array type   */
                     0, EOI,		   	              /* distribution */
                     0, EOI,				      /* geometry id  */
                     pointer_id, EOR) < 0) {
            Cif_Error();
         }
      }
      else {
         bd_idx    = ATD_ARRAY_IDX(attr_idx);
         buffer[0] = NULL_CHAR;

         if (BD_ARRAY_CLASS(bd_idx) == Assumed_Shape) {

            for (i = 1; i <= BD_RANK(bd_idx); i++) {

               if (BD_LB_FLD(bd_idx,i) == CN_Tbl_Idx) {
                  sprintf(string, "%c%s",
                          EOI,
                          convert_to_string(&CN_CONST(BD_LB_IDX(bd_idx,i)),
                                             CN_TYPE_IDX(BD_LB_IDX(bd_idx,i)),
                                             outbuf1));
               }
               else {
                  string[0] = EOI;
                  string[1] = VAR_LEN_CHAR;
                  string[2] = NULL_CHAR;
               }
               strcat(buffer, string);
            }
         }
         else if (BD_ARRAY_CLASS(bd_idx) != Deferred_Shape) {

            for (i = 1; i <= BD_RANK(bd_idx); i++) {

               if (BD_LB_FLD(bd_idx,i) == CN_Tbl_Idx) {
                  sprintf(string, "%c%s", 
                          EOI,
                          convert_to_string(&CN_CONST(BD_LB_IDX(bd_idx,i)),
                                             CN_TYPE_IDX(BD_LB_IDX(bd_idx,i)),
                                             outbuf1));
               }
               else if (BD_LB_FLD(bd_idx,i) != NO_Tbl_Idx) {
                  string[0] = EOI;
                  string[1] = VAR_LEN_CHAR;
                  string[2] = NULL_CHAR;
               }

               strcat(buffer, string);

               if (BD_ARRAY_CLASS(bd_idx) == Assumed_Size && 
                   BD_RANK(bd_idx) == i) {
                  string[0] = EOI;
                  string[1] = ASSUMED_SIZE_CHAR;
                  string[2] = NULL_CHAR;
               }
               else if (BD_UB_FLD(bd_idx,i) == CN_Tbl_Idx) {
                  sprintf(string, "%c%s", 
                          EOI, 
                          convert_to_string(&CN_CONST(BD_UB_IDX(bd_idx,i)),
                                             CN_TYPE_IDX(BD_UB_IDX(bd_idx,i)),
                                             outbuf1));
               }
               else {
                  string[0] = EOI;
                  string[1] = (BD_UB_FLD(bd_idx,i) != NO_Tbl_Idx) ?
                                                      VAR_LEN_CHAR :
                                                      ASSUMED_SIZE_CHAR;
                  string[2] = NULL_CHAR;
               }
               strcat(buffer, string);
            }
         }

         CONVERT_CVAL_TO_STR(&offset, Integer_8, offset_buf);

         if (fprintf(c_i_f,
       "%d%c%s%c%d%c%d%c%d%c%d%c%d%c%d%c%s%c%lx%c%d%c%s%c%d%c%d%s%c%d%c%d%c%d%c",
                   CIF_F90_OBJECT, EOI, 
                   AT_OBJ_NAME_PTR(attr_idx), EOI,
                   AT_CIF_SYMBOL_ID(attr_idx), EOI,
                   SCP_CIF_ID(curr_scp_idx), EOI,
                   cif_data_type(data_type), EOI,
                   symbol_class, EOI,
                   storage_class, EOI,
                   storage_id, EOI,
                   offset_buf, EOI,
                   attributes, EOI,
                   derived_type, EOI,
                   char_len, EOI,
                   BD_RANK(bd_idx), EOI, 
                   BD_ARRAY_CLASS(bd_idx),
                   buffer, EOI,
                   0, EOI,		   	              /* distribution */
                   0, EOI,				      /* geometry id  */
                   pointer_id, EOR) < 0) {
            Cif_Error();
         }
      }

      break;


   /* ----------------------------------------------------------------------- */
   /*                              Pgm_Unit				      */
   /* ----------------------------------------------------------------------- */

   case Pgm_Unit:

      if (ATP_PROC(attr_idx) != Intrin_Proc &&
          ((name_pool[AT_NAME_IDX(attr_idx)].name_char == '$'  &&
           attr_idx != glb_tbl_idx[Main_Attr_Idx])  ||
           name_pool[AT_NAME_IDX(attr_idx)].name_char == '_')) {  /* Lib call */
         break;
      }

      /* If the Attr entry is marked in error, produce an Entry Point record  */
      /* anyway so that its symbol id will be defined.  See the comments at   */
      /* the head of the Data_Obj case for details.			      */

      if (AT_DCL_ERR(attr_idx)) {

         if (AT_CIF_SYMBOL_ID(attr_idx) == 0) {
            AT_CIF_SYMBOL_ID(attr_idx) = NEXT_SYMBOL_ID;
         }

         Cif_F90_Entry_Rec(c_i_f, 
                           AT_OBJ_NAME_PTR(attr_idx),
                           AT_CIF_SYMBOL_ID(attr_idx),
		           SCP_CIF_ID(curr_scp_idx), 
                           0, 
                           0,
                           0,
                           0,
		           0,
		           0,
                           NULL);
         goto EXIT;
      }

      if (ATP_IN_INTERFACE_BLK(attr_idx)) {

         if (ATP_SCP_ALIVE(attr_idx)) {
            attributes = CIF_PGM_IN_INTERFACE;
         }
         else if (AT_REFERENCED(attr_idx) != Not_Referenced) {
            attributes = CIF_PGM_REFERENCE;
         }
         else {
    
            /* This stops interface body records from being produced in the   */
            /* host procedure if the interface body is not referenced.        */
    
            AT_CIF_DONE(attr_idx) = FALSE;
            goto EXIT;
         }
      }
      else if (ATP_SCP_ALIVE(attr_idx)) {
         attributes = CIF_PGM_DEFINITION;
      }
      else if (AT_REFERENCED(attr_idx) != Not_Referenced  &&
               ! AT_REF_IN_CHILD(attr_idx)) {

         /* The program unit is being referenced in some fashion but its      */
         /* scope is not alive.  If Attr for the entry point exists because   */
         /* it was referenced in a child scope (for example, one internal     */
         /* procedure references another so an Attr for the referenced        */
         /* internal procedure also exists at the parent level), then DON'T   */
         /* send it through again or it will mess up the Usage records that   */
         /* have already been generated (their symbol ids won't match the     */
         /* symbol id in the Attr we're currently processing).   	      */
         /* In contrast, if one sibling is referencing another and we're in   */
         /* of the sibling scopes, reset AT_CIF_DONE back to FALSE so the     */
         /* definitional Entry Point record will be produced when the Attr is */
         /* sent through cif_send_attr a little later.			      */

         AT_CIF_DONE(attr_idx) = FALSE;


         /* If the program unit name is a dummy argument AND it's referenced  */
         /* somewhere in the current procedure, we don't want to produce      */
         /* multiple Entry Point records for it but for the above reasons we  */
         /* can't use AT_CIF_DONE so use ATP_CIF_DARG_PROC.  If this flag is  */
         /* TRUE, we're processing the dummy arg so clear it and continue.    */
         /* If it's FALSE, we're here the second time for the reference so    */
         /* quit.						              */

         if (AT_IS_DARG(attr_idx)) {

            if (ATP_CIF_DARG_PROC(attr_idx)) {
               ATP_CIF_DARG_PROC(attr_idx) = FALSE;
            }
            else {
              goto EXIT;
            }
         }


         /* If this is a module procedure and it was referenced by another    */
         /* module procedure but we are now processing the Attrs in the       */
         /* specification part of the module, do not produce an Entry Point   */
         /* record for the module procedure now.  Produce it when the module  */
         /* procedure is being processed.				      */
         /* The second case covers a module that's been used indirectly.      */

         if (ATP_PROC(attr_idx) == Module_Proc  &&
             ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {
            goto EXIT;
         }
         else if (ATP_PGM_UNIT(attr_idx) == Module  &&
                  AT_USE_ASSOCIATED(attr_idx)) {
            attributes = CIF_PGM_USE_ASSOCIATED;
         }
         else {
            attributes = CIF_PGM_REFERENCE;
         }
      }
      else if (AT_IS_DARG(attr_idx)) {

         /* If the program unit name is a dummy argument and its name is not  */
         /* "referenced" in the Fortran 90 sense, an Entry Point record still */
         /* must be generated to satisfy the symbol ID in the Usage record    */
         /* generated for the appearance of the name.			      */

         attributes = CIF_PGM_REFERENCE;
      }
      else if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit && 
               (ATP_VFUNCTION(attr_idx) ||
                ATP_NOSIDE_EFFECTS(attr_idx) ||
                ATP_NAME_IN_STONE(attr_idx) ||
                ATP_DCL_EXTERNAL(attr_idx))) {

         /* Same idea as above for a dummy arg procedure.  If the name only   */
         /* appears in an EXTERNAL stmt, we must still generate an Entry      */
         /* Point record to satisfy the Usage record.		              */
         /* Also VFUNCTION, NOSIDE EFFECTS and NAME dirs.                     */
  
         attributes = CIF_PGM_REFERENCE;
      }
      else if (AT_USE_ASSOCIATED(attr_idx)) {

         /* This case catches a module procedure that was brought in by use   */
         /* association but never referenced.  Without this case, if it was   */
         /* brought in and never invoked but WAS named in a PUBLIC stmt, for  */
         /* instance, a Usage record for the PUBLIC stmt appearance would be  */
         /* generated but no Entry Point record would be generated to satisfy */
         /* the symbol id in the Usage record.				      */
         /* CIF_PGM_USE_ASSOCIATED is added to "attributes" later.	      */

      }
      else {

         /* This stops internal and module procedure records from being       */
         /* produced in the host procedure if the internal or module          */
         /* procedure was not referenced.                                     */

         AT_CIF_DONE(attr_idx) = FALSE;
         goto EXIT;
      }


      /* If AT_ATTR_LINK is not NULL_IDX, it means we're processing:	      */
      /*   - a reference from one module procedure to another module          */
      /*     procedure in the same module,				      */
      /*   - a reference from one module procedure to another module          */
      /*     procedure but the other module procedure name was use associated */
      /*     into the module specification part,			      */
      /*   - a reference from one internal procedure to another internal      */
      /*     procedure within the same program unit,			      */
      /*   - a reference from an internal procedure to another procedure      */
      /*     where the other procedure name is host associated into the       */
      /*     internal procedure, or					      */
      /*   - a procedure that belongs to an interface block.		      */
      /* If the reference is to another procedure whose name is known in an   */
      /* outer scope, get some of the info from the outer Attr.		      */
 
      if (AT_ATTR_LINK(attr_idx) == NULL_IDX) {
         get_other_func_rslt_info = FALSE;
      }
      else if (AT_OBJ_CLASS(AT_ATTR_LINK(attr_idx)) != Interface) {
         get_other_func_rslt_info = TRUE;
         attr_idx                 = AT_ATTR_LINK(attr_idx);
      }
      else {
        goto EXIT;
      }

      if (AT_CIF_SYMBOL_ID(attr_idx) == 0) {
         AT_CIF_SYMBOL_ID(attr_idx) = NEXT_SYMBOL_ID;
      }

      switch (ATP_PGM_UNIT(attr_idx)) {

         case Pgm_Unknown:
            pgm_unit_type = CIF_F90_ET_UNKNOWN;
            num_dargs	  = ATP_NUM_DARGS(attr_idx);
            break;

         case Function:
            pgm_unit_type =
               (ATP_ALT_ENTRY(attr_idx)) ? CIF_F90_ET_ALT_ENTRY :
                                           CIF_F90_ET_FUNCTION;

            /* This may be a reference only.  Then there is no darg list */
            /* but ATP_EXTRA_DARG will be set.                           */

            if (ATP_EXPL_ITRFC(attr_idx) && ATP_EXTRA_DARG(attr_idx)) {
               num_dargs = ATP_NUM_DARGS(attr_idx) - 1;
            }
            else {
               num_dargs = ATP_NUM_DARGS(attr_idx);
            }
            
            if (AT_USE_ASSOCIATED(attr_idx)) {
               attributes = attributes | CIF_PGM_USE_ASSOCIATED;
            }

            break;

         case Subroutine:
            pgm_unit_type =
               (ATP_ALT_ENTRY(attr_idx)) ? CIF_F90_ET_ALT_ENTRY :
                                           CIF_F90_ET_SUBROUTINE;

            num_dargs = ATP_NUM_DARGS(attr_idx);

            if (AT_USE_ASSOCIATED(attr_idx)) {
               attributes = attributes | CIF_PGM_USE_ASSOCIATED;
            }

            break;

         case Program:
            num_dargs = 0;
            pgm_unit_type= CIF_F90_ET_PROGRAM;
            break;

         case Blockdata:
            num_dargs = 0;
            pgm_unit_type = CIF_F90_ET_BLOCKDATA;
            break;

         case Module:
            num_dargs = 0;
            pgm_unit_type= CIF_F90_ET_MODULE;
      }

      if ((attributes & CIF_PGM_REFERENCE)  ||
          AT_USE_ASSOCIATED(attr_idx)       ||
          get_other_func_rslt_info) {
         num_dargs = 0;
      }


      if (AT_OPTIONAL(attr_idx)) {
         attributes = attributes | CIF_PGM_OPTIONAL;
      }


      /* The Attr entry associated with the SCP for a module is marked        */
      /* private if the module contains a bare PRIVATE statement.  But the    */
      /* name of the module does not have an accessibility attribute          */
      /* associated with it - only the names within the module.  So don't set */
      /* the CIF PRIVATE attribute in this case.			      */

      if (AT_PRIVATE(attr_idx)  &&  ATP_PGM_UNIT(attr_idx) != Module) {
         attributes = attributes | CIF_PGM_PRIVATE;
      }

      if (ATP_RECURSIVE(attr_idx)) {
         attributes = attributes | CIF_PGM_RECURSIVE;
      }

      if (ATP_PGM_UNIT(attr_idx) == Function) {
         rslt_idx = ATP_RSLT_IDX(attr_idx);

         if (ATP_SCP_ALIVE(attr_idx)) {

            if (! AT_CIF_DONE(rslt_idx)) {
               cif_send_attr(rslt_idx, NULL_IDX);
            }

            rslt_id = AT_CIF_SYMBOL_ID(rslt_idx);
         }
         else {

            /* We might be getting information from an outer Attr for         */
            /* multiple references to a function whose name is known in an    */
            /* outer scope.  We need to produce an Object record for the      */
            /* result to satisfy the symbol id in the Entry Point record.     */
            /* If the function has been called before, the result would have  */
            /* already been processed which means we need to clear            */
            /* AT_CIF_DONE so an Object record can be produced in the current */
            /* scope.  Since the function's scope is not alive, it may well   */
            /* be processed AFTER this (that is, the Pgm_Unit attr we're      */
            /* processing now could be due to a forward reference).  This     */
            /* means we need to save the symbol id and restore it so that if  */
            /* we later process the result when its scope is alive, the	      */
            /* symbol id will match the symbol ids in the Usage records that  */
            /* were generated for the result.	      			      */

            AT_CIF_DONE(rslt_idx)      = FALSE;

            cif_send_attr(rslt_idx, NULL_IDX);
            rslt_id                    = AT_CIF_SYMBOL_ID(rslt_idx);
            AT_CIF_DONE(rslt_idx)      = FALSE;
         }
      }
      else {
         rslt_id = 0;
      }

      if (ATP_PROC(attr_idx) == Module_Proc) {

         if (AT_MODULE_IDX(attr_idx) == 0) {
            storage_id = (SCP_LEVEL(curr_scp_idx) == 0) ?
                            AT_CIF_SYMBOL_ID(SCP_ATTR_IDX(curr_scp_idx)) :
                            AT_CIF_SYMBOL_ID(
                               SCP_ATTR_IDX(SCP_PARENT_IDX(curr_scp_idx)));
         }
         else {

            if (AT_CIF_SYMBOL_ID(AT_MODULE_IDX(attr_idx)) == 0) {

               /* If the module's symbol id is 0, it probably means the       */
               /* module is being used indirectly.  If so, the module Attr    */
               /* won't exist in the current scope (or any parent scope) so   */
               /* send it through to get an Entry record generated to resolve */
               /* the storage id field of the Object record currently being   */
               /* constructed.	                                              */

               cif_send_attr(AT_MODULE_IDX(attr_idx), NULL_IDX);
            }

            storage_id = AT_CIF_SYMBOL_ID(AT_MODULE_IDX(attr_idx));
         }
      }
      else if (AT_USE_ASSOCIATED(attr_idx)) {

         if (AT_CIF_SYMBOL_ID(AT_MODULE_IDX(attr_idx)) == 0) {

            /* See reasoning above for sending the module's Attr through      */
            /* cif_send_attr.						      */

            cif_send_attr(AT_MODULE_IDX(attr_idx), NULL_IDX);
         }

         storage_id = AT_CIF_SYMBOL_ID(AT_MODULE_IDX(attr_idx));
      }
      else {
         storage_id = 0;
      }

      if (num_dargs != 0) { 
         sn_idx	= ATP_FIRST_IDX(attr_idx);

         if (ATP_EXTRA_DARG(attr_idx)) {
            ++sn_idx;
         }

         for (i = 0; i < num_dargs; i++) {
            darg_idx = SN_ATTR_IDX(sn_idx++);

            if (! AT_COMPILER_GEND(darg_idx)) { 

               /* Not an alternate return dummy arg.			      */

               /* If the program unit name is a dummy arg AND it's referenced */
               /* somewhere in the current procedure, we don't want to        */
               /* produce multiple Entry Point records for it but for reasons */
               /* documented earlier in the Pgm_Unit case we can't use        */
               /* AT_CIF_DONE.  So set ATP_CIF_DARG_PROC to indicate we should*/
               /* process the Attr as a dummy arg.  The flag is cleared       */
               /* farther up in this case code.				      */

               if (AT_OBJ_CLASS(darg_idx) == Pgm_Unit) {
                  ATP_CIF_DARG_PROC(darg_idx) = TRUE;
               }
 
               cif_send_attr(darg_idx, NULL_IDX);
            }
         }
      }

      if (fprintf(c_i_f, 
                  "%d%c%s%c%d%c%d%c%d%c%d%c%lx%c%d%c%d%c%d",
                  CIF_F90_ENTRY, EOI, 
                  AT_OBJ_NAME_PTR(attr_idx), EOI,
                  AT_CIF_SYMBOL_ID(attr_idx), EOI,
		  SCP_CIF_ID(curr_scp_idx), EOI,
                  pgm_unit_type, EOI,
                  ATP_PROC(attr_idx), EOI,
                  attributes, EOI,
                  rslt_id, EOI,
		  storage_id, EOI,
                  num_dargs) < 0) {
         Cif_Error();
      }

      if (num_dargs != 0) {
         sn_idx	= ATP_FIRST_IDX(attr_idx);

         if (ATP_EXTRA_DARG(attr_idx)) {
            ++sn_idx;
         }

         for (i = 0; i < num_dargs; i++) {
            darg_idx = SN_ATTR_IDX(sn_idx++);

            if (AT_COMPILER_GEND(darg_idx)) {   /* An alternate return darg.  */
               darg_idx = 0;
            }
            else {
               darg_idx = AT_CIF_SYMBOL_ID(darg_idx);
            }

            if (fprintf(c_i_f, "%c%d", EOI, darg_idx) < 0) {
               Cif_Error();
            }
         }
      }

      if (fprintf(c_i_f, "\n") < 0) {
         Cif_Error();
      }

      get_other_func_rslt_info = FALSE;

      break;


   /* ----------------------------------------------------------------------- */
   /*                              Label				      */
   /* ----------------------------------------------------------------------- */

   case Label:
      cif_label_rec(attr_idx);
      break;


   /* ----------------------------------------------------------------------- */
   /*                            Derived_Type				      */
   /* ----------------------------------------------------------------------- */

   case Derived_Type:


      if (AT_CIF_SYMBOL_ID(attr_idx) == 0) {
         AT_CIF_SYMBOL_ID(attr_idx) = NEXT_SYMBOL_ID;
      }

      if (ATT_CIF_DT_ID(attr_idx) == 0) {
         ATT_CIF_DT_ID(attr_idx) = NEXT_DERIVED_TYPE_ID;
      }

      attributes = (ATT_SEQUENCE_SET(attr_idx)) ? (CIF_DRT_SEQUENCE) : 0;

      if (AT_PRIVATE(attr_idx)) {
         attributes = attributes | CIF_DRT_PRIVATE;
      }

      if (ATT_PRIVATE_CPNT(attr_idx)) {
         attributes = attributes | CIF_DRT_COMP_PRIVATE;
      }

      sn_idx = ATT_FIRST_CPNT_IDX(attr_idx);

      while (sn_idx != NULL_IDX) {

         if (get_other_func_rslt_info) {
            AT_CIF_DONE(SN_ATTR_IDX(sn_idx))      = FALSE;
            AT_CIF_SYMBOL_ID(SN_ATTR_IDX(sn_idx)) = 0;
         }

         cif_send_attr(SN_ATTR_IDX(sn_idx), attr_idx);
         sn_idx	= SN_SIBLING_LINK(sn_idx);
      }

      if (fprintf(c_i_f, "%d%c%s%c%d%c%d%c%d%c%lx%c%d",
                         CIF_F90_DERIVED_TYPE, EOI, 
                         AT_OBJ_NAME_PTR(attr_idx), EOI,
                         AT_CIF_SYMBOL_ID(attr_idx), EOI,
                         SCP_CIF_ID(curr_scp_idx), EOI,
                         ATT_CIF_DT_ID(attr_idx), EOI,
                         attributes, EOI,
                         ATT_NUM_CPNTS(attr_idx)) < 0) {
         Cif_Error();
      }

      sn_idx = ATT_FIRST_CPNT_IDX(attr_idx);

      while (sn_idx != NULL_IDX) {

         if (fprintf(c_i_f, "%c%d",
                            EOI, AT_CIF_SYMBOL_ID(SN_ATTR_IDX(sn_idx))) < 0) {
            Cif_Error();
         }

         sn_idx	= SN_SIBLING_LINK(sn_idx);
      }

      if (fprintf(c_i_f, "\n") < 0) {
         Cif_Error();
      }

      break;


   /* ----------------------------------------------------------------------- */
   /*                            Interface				      */
   /* ----------------------------------------------------------------------- */

   case Interface:

      /* If the interface identifier is marked in error but its symbol id has */
      /* been referenced somewhere (like in a Usage record), output a dummy   */
      /* Interface Block record to define the symbol id).		      */
   
      if (AT_DCL_ERR(attr_idx)  &&  AT_CIF_SYMBOL_ID(attr_idx) != 0) {
         scope_id = (AT_USE_ASSOCIATED(attr_idx)) ?
                       SCP_CIF_ID(curr_scp_idx) :
                       ATI_CIF_SCOPE_ID(attr_idx);

         switch (ATI_INTERFACE_CLASS(attr_idx)) {

            case Defined_Assign_Interface:
               interface_type = CIF_IB_ASSIGNMENT;
               break;

            case Generic_Unknown_Interface:
            case Generic_Function_Interface:
            case Generic_Subroutine_Interface:
               interface_type = CIF_IB_GENERIC;
               break;

            default:
               interface_type = CIF_IB_OPERATOR;
               break;
         }

         Cif_F90_Int_Block_Rec(c_i_f,
                               AT_OBJ_NAME_PTR(attr_idx),
                               AT_CIF_SYMBOL_ID(attr_idx),
                               scope_id, 
                               interface_type,
                               0, 
                               0, 
                               NULL,
                               0);

         goto EXIT;
      }


      if (AT_CIF_SYMBOL_ID(attr_idx) == 0) {
         AT_CIF_SYMBOL_ID(attr_idx) = NEXT_SYMBOL_ID;
      }


      /* If the interface block was brought in from a module and the module's */
      /* Attr entry has not yet been produced, normally the module's Attr     */
      /* entry would be sent through cif_send_attr to get an Entry Point      */
      /* record produced.  But if the interface is for an intrinsic procedure,*/
      /* don't do this because it will cause redundant Entry Point records to */
      /* be generated.  See the "if" stmt immediately following	the one this  */
      /* comment is documenting.					      */

      if (AT_USE_ASSOCIATED(attr_idx)          && 
          AT_MODULE_IDX(attr_idx) != NULL_IDX  &&
          ! AT_CIF_DONE(AT_MODULE_IDX(attr_idx))) {

         if (AT_IS_INTRIN(attr_idx)          &&
             ! ATI_USER_SPECIFIED(attr_idx)  &&
             ATI_CIF_SEEN_IN_CALL(attr_idx)) {

            /* Do nothing.  Easier to visualize the code this way.            */

         }
         else {
            cif_send_attr(AT_MODULE_IDX(attr_idx), NULL_IDX);
         }
      }


      if (AT_IS_INTRIN(attr_idx)  &&  ! ATI_USER_SPECIFIED(attr_idx)) {

         /* If this intrinsic procedure has been referenced and a Call Site   */
         /* record was output to record the call, then the Entry record (and  */
         /* Object record if it is a function) have already been output.      */

         if (ATI_CIF_SEEN_IN_CALL(attr_idx)) {
            goto EXIT;
         }


         rslt_id = 0;

         if (ATI_INTERFACE_CLASS(attr_idx) == Generic_Function_Interface) {
            pgm_unit_type = CIF_F90_ET_FUNCTION;


            /* Count on the fact that the last specific Attr in the intrinsic */
            /* interface block is the "default" type Attr.  That is, if the   */
            /* specific form of the intrinsic is passed as an actual argument,*/
            /* this is the one that records what the specific's argument type */
            /* must be (and thus the result type of the specific).  Generics  */
            /* can't be passed as arguments but some, like SQRT, have the     */
            /* same generic and specific name and cflint needs to have a      */
            /* result type to compare the characteristics of actual procedure */
            /* arguments to how the dummy procedure argument is invoked.      */

            sn_idx = ATI_FIRST_SPECIFIC_IDX(attr_idx);

            for (i = 2;  i <= ATI_NUM_SPECIFICS(attr_idx);  ++i) {
               sn_idx = SN_SIBLING_LINK(sn_idx);
            }

            rslt_idx = ATP_RSLT_IDX(SN_ATTR_IDX(sn_idx));
              

            /* Always send the function result Attr through even though its   */
            /* symbol id might already be nonzero and AT_CIF_DONE might       */
            /* already be TRUE because the Entry Point record generated for   */
            /* each intrinsic function in each CIF scope must have an         */
            /* associated Object record to pass on to CIF the function        */
            /* result type.  For example, if a module contains several module */
            /* procedures, each of which reference BIT_SIZE, each module      */
            /* procedure has its own Attr for BIT_SIZE but they all refer to  */
            /* the same function result Attr so we can't go by CIF_SYMBOL_ID  */
            /* being 0 in that function result Attr.			      */
            
            save_cif_done         = AT_CIF_DONE(rslt_idx);
            AT_CIF_DONE(rslt_idx) = FALSE;
            cif_send_attr(rslt_idx, NULL_IDX);
            AT_CIF_DONE(rslt_idx) = save_cif_done;

            rslt_id = AT_CIF_SYMBOL_ID(rslt_idx);
         }
         else if (ATI_INTERFACE_CLASS(attr_idx) ==Generic_Subroutine_Interface){
            pgm_unit_type = CIF_F90_ET_SUBROUTINE;
         }
         else {
            pgm_unit_type = CIF_F90_ET_UNKNOWN;
         }
   
         attributes = CIF_PGM_REFERENCE;
   
         if (AT_PRIVATE(attr_idx)) {
            attributes = attributes | CIF_PGM_PRIVATE;
         }
   
         Cif_F90_Entry_Rec(c_i_f,
                           AT_OBJ_NAME_PTR(attr_idx),
                           AT_CIF_SYMBOL_ID(attr_idx),
                           SCP_CIF_ID(curr_scp_idx), 
                           pgm_unit_type, 
                           CIF_F90_PT_INTRINSIC,
                           attributes, 
                           rslt_id, 
		           0, 
		           0, 
                           NULL);

         break;
      }

      if (ATI_UNNAMED_INTERFACE(attr_idx)) {

         if (fprintf(c_i_f, 
                     "%d%c%c%d%c%d%c%d%c%x%c%d",
                     CIF_F90_INT_BLOCK, EOI, 
                     EOI,                              /* Do not put out name */
                     AT_CIF_SYMBOL_ID(attr_idx), EOI,
                     ATI_CIF_SCOPE_ID(attr_idx), EOI,
                     CIF_IB_SPECIFIC, EOI,
                     0, EOI,
                     ATI_NUM_SPECIFICS(attr_idx)) < 0) {
            Cif_Error();
         }
      }
      else {

         if (ATI_PROC_IDX(attr_idx) != NULL_IDX) {
            cif_send_attr(ATI_PROC_IDX(attr_idx), NULL_IDX);
         }

         attributes = (AT_PRIVATE(attr_idx)) ? 1 : 0;

         switch (ATI_INTERFACE_CLASS(attr_idx)) {
           case Defined_Assign_Interface:
              interface_type = CIF_IB_ASSIGNMENT;
              break;

           case Generic_Unknown_Interface:
           case Generic_Function_Interface:
           case Generic_Subroutine_Interface:
              interface_type = CIF_IB_GENERIC;
              break;

           default:
              interface_type = CIF_IB_OPERATOR;
              break;
         }

         /* If the interface block is pulled in from a module, there are no   */
         /* Begin Scope or End Scope records associated with it (they only    */
         /* occur where the interface block was defined).  So give it the     */
         /* scope id of the current scope (the USEing scope) so that libcif   */
         /* some kind of valid scope id to sort by.			      */
         /* LRR:  Could this same thing happen if the interface block is      */
         /*       pulled in from a host scoping unit?			      */
         /* LRR:  We may need to add a flag to the Interface Block record in  */
         /*       the future to flag the fact that scope id is the containing */
         /*       scope rather than the scope that the interface blk defines. */

         scope_id = (AT_USE_ASSOCIATED(attr_idx)) ?
                       SCP_CIF_ID(curr_scp_idx) :
                       ATI_CIF_SCOPE_ID(attr_idx);

         if (fprintf(c_i_f, 
                     "%d%c%s%c%d%c%d%c%d%c%lx%c%d",
                     CIF_F90_INT_BLOCK, EOI, 
                     AT_OBJ_NAME_PTR(attr_idx), EOI,
                     AT_CIF_SYMBOL_ID(attr_idx), EOI,
                     scope_id, EOI,
                     interface_type, EOI,
                     attributes, EOI,
                     ATI_NUM_SPECIFICS(attr_idx)) < 0) {
            Cif_Error();
         }
      }


      /* Go through the SN list to get the symbol IDs for the procedure names */
      /* in the interface block (finishes off the Interface Block record).    */

      sn_idx = ATI_FIRST_SPECIFIC_IDX(attr_idx);

      while (sn_idx != NULL_IDX) {
         interface_idx	= SN_ATTR_IDX(sn_idx);
         sn_idx		= SN_SIBLING_LINK(sn_idx);

         if (AT_CIF_SYMBOL_ID(interface_idx) == 0) {
            AT_CIF_SYMBOL_ID(interface_idx) = NEXT_SYMBOL_ID;
         }

         if (fprintf(c_i_f, "%c%d",
                            EOI,
                            AT_CIF_SYMBOL_ID(interface_idx)) < 0) {
            Cif_Error();
         }
      }

      if (fprintf(c_i_f, "%c", EOR) < 0) {
         Cif_Error();
      }


      /* Go through the SN list again to produce an Entry record for each     */
      /* module procedure and/or intrinsic procedure named in the (possibly   */
      /* user extended intrinsic) interface block.  All interface bodies      */
      /* were taken care of when the interface scope was still alive.         */
      /* Note, however, that if an interface body was brought in by use       */
      /* association (because it belongs to an interface block that was use   */
      /* associated), the interface body has NOT been taken care of so we     */
      /* to produce an Entry Point record for it here.			      */

      sn_idx = ATI_FIRST_SPECIFIC_IDX(attr_idx);

      while (sn_idx != NULL_IDX) {
         interface_idx	= SN_ATTR_IDX(sn_idx);
         sn_idx		= SN_SIBLING_LINK(sn_idx);

         if (ATP_PROC(interface_idx) == Module_Proc       ||
             ATP_PROC(interface_idx) == Intrin_Proc       ||
             (ATP_PROC(interface_idx) == Extern_Proc  &&
              AT_USE_ASSOCIATED(interface_idx))) {

            if (ATP_PGM_UNIT(interface_idx) == Function) {
               pgm_unit_type = CIF_F90_ET_FUNCTION;
               rslt_id       = ATP_RSLT_IDX(interface_idx);
               cif_send_attr(rslt_id, NULL_IDX);
               rslt_id       = AT_CIF_SYMBOL_ID(rslt_id);
            }
            else {
               pgm_unit_type = CIF_F90_ET_SUBROUTINE;
               rslt_id       = 0;
            }

            if (AT_MODULE_IDX(interface_idx) == NULL_IDX) {

               if (SCP_LEVEL(curr_scp_idx) == 0) {
                  storage_id = AT_CIF_SYMBOL_ID(SCP_ATTR_IDX(curr_scp_idx));
               }
               else {
                  i = SCP_PARENT_IDX(curr_scp_idx);

                  while (SCP_LEVEL(i) != 0) {
                     i = SCP_PARENT_IDX(i);
                  }

                  storage_id = AT_CIF_SYMBOL_ID(SCP_ATTR_IDX(i));
               }
            }
            else {

               if (AT_CIF_SYMBOL_ID(AT_MODULE_IDX(interface_idx)) == 0) {
                  cif_send_attr(AT_MODULE_IDX(interface_idx), NULL_IDX);
               }

               storage_id = AT_CIF_SYMBOL_ID(AT_MODULE_IDX(interface_idx));
            }


            attributes = CIF_PGM_REFERENCE;

            if (AT_OPTIONAL(interface_idx)) {
               attributes = attributes | CIF_PGM_OPTIONAL;
            }

            if (AT_PRIVATE(interface_idx)) {
               attributes = attributes | CIF_PGM_PRIVATE;
            }

            if (AT_USE_ASSOCIATED(interface_idx)) {
               attributes = attributes | CIF_PGM_USE_ASSOCIATED;
            }

            if (ATP_RECURSIVE(interface_idx)) {
               attributes = attributes | CIF_PGM_RECURSIVE;
            }


            Cif_F90_Entry_Rec(c_i_f,
                              AT_OBJ_NAME_PTR(interface_idx),
                              AT_CIF_SYMBOL_ID(interface_idx),
                              scope_id, 
                              pgm_unit_type,
                              ATP_PROC(interface_idx),
                              attributes, 
                              rslt_id,
                              storage_id,
                              0,
                              NULL);
         }
      }

      break;


   /* ----------------------------------------------------------------------- */
   /*                            Namelist_Grp				      */
   /* ----------------------------------------------------------------------- */

   case Namelist_Grp:
     
      if (AT_CIF_SYMBOL_ID(attr_idx) == 0) {
         AT_CIF_SYMBOL_ID(attr_idx) = NEXT_SYMBOL_ID;
      }

      if (AT_USE_ASSOCIATED(attr_idx)) {
        
         if (AT_CIF_SYMBOL_ID(AT_MODULE_IDX(attr_idx)) == 0) {
            cif_send_attr(AT_MODULE_IDX(attr_idx), NULL_IDX);
         }

         storage_id = AT_CIF_SYMBOL_ID(AT_MODULE_IDX(attr_idx));
      }
      else if (AT_HOST_ASSOCIATED(attr_idx)) {

         /* If the namelist group name is host associated, don't produce      */
         /* another Namelist record.  (AT_ATTR_LINK was broken earlier in     */
         /* the Semantics Pass driver so there's an independent local Attr    */
         /* for the namelist group name.  That's how we got here.)            */

         goto EXIT;
      }
      else {

         if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {
            storage_id = AT_CIF_SYMBOL_ID(SCP_ATTR_IDX(curr_scp_idx));
         }
         else {
            storage_id = 0;
         }
      }

      num_namelist = AT_DCL_ERR(attr_idx) ? 0: ATN_NUM_NAMELIST(attr_idx);

      if (fprintf(c_i_f, 
                  "%d%c%s%c%d%c%d%c%d%c%d",
                  CIF_F90_NAMELIST, EOI, 
                  AT_OBJ_NAME_PTR(attr_idx), EOI,
                  AT_CIF_SYMBOL_ID(attr_idx), EOI,
                  SCP_CIF_ID(curr_scp_idx), EOI,
   		  storage_id, EOI,
                  num_namelist) < 0) {
         Cif_Error();
      }

      if (num_namelist > 0) {
         sn_idx = ATN_FIRST_NAMELIST_IDX(attr_idx);

         while (sn_idx != NULL_IDX) {
            namelist_idx = SN_ATTR_IDX(sn_idx);
            sn_idx	 = SN_SIBLING_LINK(sn_idx);

            if (AT_CIF_SYMBOL_ID(namelist_idx) == 0) {
               AT_CIF_SYMBOL_ID(namelist_idx) = NEXT_SYMBOL_ID;
            }

            if (fprintf(c_i_f,"%c%d",EOI,AT_CIF_SYMBOL_ID(namelist_idx)) < 0) {
               Cif_Error();
            }
         }
      }

      if (fprintf(c_i_f, "%c", EOR) < 0) {
         Cif_Error();
      }

      break;


   /* ----------------------------------------------------------------------- */
   /*                            Stmt_Func				      */
   /* ----------------------------------------------------------------------- */

   case Stmt_Func:

 
      if (AT_CIF_SYMBOL_ID(attr_idx) == 0) {
         AT_CIF_SYMBOL_ID(attr_idx) = NEXT_SYMBOL_ID;
      }

      if (fprintf(c_i_f, 
                  "%d%c%s%c%d%c%d%c%d%c%d%c%lx%c%d%c%d%c%d",
                  CIF_F90_ENTRY, EOI, 
                  AT_OBJ_NAME_PTR(attr_idx), EOI,
                  AT_CIF_SYMBOL_ID(attr_idx), EOI,
                  SCP_CIF_ID(curr_scp_idx), EOI,
                  CIF_F90_ET_STMT, EOI,
                  Unknown_Proc, EOI,
                  attributes, EOI,
                  0, EOI,
   		  0, EOI,
                  ATP_NUM_DARGS(attr_idx)) < 0) {
         Cif_Error();
      }

      if (ATP_NUM_DARGS(attr_idx) != NULL_IDX) {
         sn_idx	= ATP_FIRST_IDX(attr_idx);

         for (i = 0; i < ATP_NUM_DARGS(attr_idx); i++) {

            if (AT_CIF_SYMBOL_ID(SN_ATTR_IDX(sn_idx)) == 0) {
               AT_CIF_SYMBOL_ID(SN_ATTR_IDX(sn_idx)) = NEXT_SYMBOL_ID;
            }

            darg_idx = AT_CIF_SYMBOL_ID(SN_ATTR_IDX(sn_idx++));

            if (fprintf(c_i_f, "%c%d", EOI, darg_idx) < 0) {
               Cif_Error();
            }
         }

         if (fprintf(c_i_f, "%c", EOR) < 0) {
            Cif_Error();
         }

         sn_idx	= ATP_FIRST_IDX(attr_idx);

         for (i = 0; i < ATP_NUM_DARGS(attr_idx); i++) {
            cif_send_attr(SN_ATTR_IDX(sn_idx++), NULL_IDX);
         }
      }
      else if (fprintf(c_i_f, "%c", EOR) < 0) {
         Cif_Error();
      }

      break;

   }


EXIT:

   TRACE (Func_Exit, "cif_send_attr", NULL);

   return;

}  /* cif_send_attr */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a CDIR$ record [5].                                            *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      dir:   The CDIR$ type.                                                *|
|*      line:  Global line number of the line containing the CDIR$.           *|
|*      col:   Column in which the CDIR$ begins.			      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void cif_directive_rec(cif_directive_code_type  dir,
		       int			line,
		       int			col)

{
   int          file_line_num;
   int          local_file_id;

   TRACE (Func_Entry, "cif_directive_rec", NULL);

   file_line_num = get_line_and_file_id(line, &local_file_id);

   Cif_Cdir_Rec(c_i_f, dir, local_file_id, file_line_num, col, 0, NULL);

   TRACE (Func_Exit, "cif_directive_rec", NULL);

   return;

}  /* cif_directive_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a File Name record [7].  The file name is assigned the next    *|
|*      file id value.							      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      expanded_file_name:       The fully expanded path name for the file.  *|
|*      user_specified_file_name: The file name as the user wrote it on the   *|
|*                                command line or in an INCLUDE.              *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      The CIF file id.						      *|
|*                                                                            *|
\******************************************************************************/

int cif_file_name_rec(char       *file_name,
		      char	 *user_specified_file_name)
{
   int		 return_val;


   TRACE (Func_Entry, "cif_file_name_rec", NULL);

   return_val = NEXT_FILE_ID;

   Cif_File_Rec(c_i_f,
                file_name,
                return_val,
                user_specified_file_name); 

   TRACE (Func_Exit, "cif_file_name_rec", NULL);

   return(return_val);

}  /* cif_file_name_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output an Include record [9].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      line_num:							      *|
|*      col_num:							      *|
|*      include_file_id:						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_include_rec(int	line_num,
		     int	col_num,
                     int	include_file_id)
{
   int          file_line_num;
   int          parent_file_id;


   TRACE (Func_Entry, "cif_include_rec", NULL);

   file_line_num = get_line_and_file_id(line_num, &parent_file_id);

   Cif_Include_Rec(c_i_f,
                   parent_file_id,
                   file_line_num,
                   col_num, 
                   include_file_id);
   
   Cif_Src_Pos_Rec(c_i_f,
                   CIF_SRC_KIND_INCLUDE,
                   include_file_id,
                   parent_file_id,
                   file_line_num,
                   col_num,
                   include_file_id,
                   0,
                   0,
                   0);

   TRACE (Func_Exit, "cif_include_rec", NULL);

   return;

}  /* cif_include_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Message record [11].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      msg_num		The message number.				      *|
|*      glb_line_num    The global source line number.			      *|
|*      col_num         The column number of the offending text (may be 0).   *|
|*      msg_severity    The severity level of the message.		      *|
|*      msg_text        A pointer to the character string containing the      *|
|*                        message text (from the message catalog).            *|
|*      arg1, arg2,     The 4 optional arguments that may be used to insert   *|
|*      arg3, arg4        text into a message.				      *|
|*									      *|
|*      scoping_unit_name  Only used for the buffered message file.           *|
|*      file_name          Only used for the buffered message file. 	      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
|* Algorithm notes:							      *|
|*   The size of "insert" was chosen because it seemed "big enough".  See the *|
|*   definitions of ORIG_MSG_SIZE and EXPANDED_MSG_SIZE in messages.m.        *|
|*									      *|
\******************************************************************************/

void cif_message_rec(int		 	 msg_num,
		     int		 	 glb_line_num,
		     int		 	 col_num,
		     msg_severities_type	 msg_severity,
		     char			*msg_text,
                     long			 arg0,
 		     long			 arg1,
		     long			 arg2,
		     long			 arg3,
		     char		        *scoping_unit_name,
		     int			 relative_order)

{
   char        *char_ptr;
   int		file_line_num;
   char	       *format[4] = { "%c", "%d", "%f", "%s" };
#ifdef KEY /* Bug 10177 */
   int	 	format_idx = 0;
#else /* KEY Bug 10177 */
   int	 	format_idx; 
#endif /* KEY Bug 10177 */
   char	        insert[4][128];
   char	       *insert_ptr[4];
   int		local_file_id;
   int		num_inserts	= 0;


   TRACE (Func_Entry, "cif_message_rec", NULL);

   if (msg_severity == Log_Error  ||  msg_severity == Log_Warning  ||
       glb_line_num == 0) {
      goto EXIT;
   }

   file_line_num = get_line_and_file_id(glb_line_num, &local_file_id);
 
   char_ptr  = msg_text;

   while ((char_ptr = strchr(char_ptr, '%')) != NULL) {
      ++char_ptr;

      switch (*char_ptr++) {

         case 'c':
            format_idx = 0;
            break;

         case 'd':
            format_idx = 1;
            break;

         case 'f':
            format_idx = 2;
            break;

         case 's':
            format_idx = 3;
            break;

         case '%':
            continue;

         case EOS:
            goto LOOP_EXIT;

         default:
            PRINTMSG(glb_line_num, 179, Internal, 0, "cif_message_rec");
      }  

      switch (num_inserts) {

         case 0:
            sprintf(insert[0], format[format_idx], arg0);
            break;

         case 1:
            sprintf(insert[1], format[format_idx], arg1);
            break;

         case 2:
            sprintf(insert[2], format[format_idx], arg2);
            break;

         case 3:
            sprintf(insert[3], format[format_idx], arg3);
      }

      insert_ptr[num_inserts] = insert[num_inserts];

      ++num_inserts;
   }

LOOP_EXIT:

   Cif_Message_Rec(c_i_f,
                   msg_severity,
                   msg_num, 
                   local_file_id,
                   glb_line_num,
                   col_num, 
                   file_line_num,
                   num_inserts,
                   insert_ptr,
                   scoping_unit_name,
      	           relative_order,
		   0,
		   local_file_id);
   
   last_msg_file_rec = CIF_MESSAGE;

EXIT:

   TRACE (Func_Exit, "cif_message_rec", NULL);

   return;

}  /* cif_message_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Source File record [14]. 				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      source_file_id  The file id of the file containing the program.       *|
|*      source_form     The source form in which the program is written (at   *|
|*                        least initially).				      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_source_file_rec(int		source_file_id,
		         src_form_type	source_form)
{

   TRACE (Func_Entry, "cif_source_file_rec", NULL);

   Cif_Srcfile_Rec(c_i_f,
                   source_file_id,
                   (source_form == Fixed_Form) ? CIF_F90_FORM_FIXED :
	   			                 CIF_F90_FORM_FREE);

   TRACE (Func_Exit, "cif_source_file_rec", NULL);

   return;

}  /* cif_source_file_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Summary record [15].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      release_level    : compiler release level			      *|
|*      gen_date         : date the compiler was created      		      *|
|*      gen_time         : time of day the compiler was created 	      *|
|*      elapsed_time     : Cray    : compilation time in microseconds	      *|
|*                         non-Cray: compilation time in microseconds for a   *|
|*                                   "short" compilation, otherwise in        *|
|*                                   seconds; see Algorithm notes below       *|
|*      aux_elapsed_time : Cray    : not used (always 0) 	              *|
|*                         non-Cray: if compilation time is "short", this is  *|
|*                                   the compilation time in microseconds     *|
|*      max_field_len    : maximum amount of memory used		      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
|* Algorithm notes:							      *|
|*    LRR notes:  URGENT SPR 701346 demonstrated that the compile time info   *|
|*    we were printing in the summary lines was grossly wrong for some        *|
|*    compilations on the T3E.  TAM told me that SECOND had not been ported   *|
|*    to T3E's and advised me to use SECONDR.  However, when I tried it, it   *|
|*    also failed.  I then checked with KRZ to find out what the C front-end  *|
|*    uses.  They use clock() on all platforms.  In order to perturb the      *|
|*    front-end as little as possible with this change, I've only changed     *|
|*    the MPP timing to use clock().  If someone has the time sometime in     *|
|*    the future, someone should probably investigate using clock() where     *|
|*    we now use SECOND.						      *|
|*    The first sentence of the following paragraph has existed for a very    *|
|*    long time.  Knowing what we now know about clock(), the description and *|
|*    concluseion no longer seem to make sense.  If anyone ever gets around   *|
|*    to changing from SECOND to clock() for general use, the following       *|
|*    paragraph should be corrected as well.                  4 March 1997    *|
|*    The clock() function on both Crays and Suns counts in microseconds.     *|
|*    For this reason, SECOND is used on Crays.  It returns a floating point  *|
|*    value of the number of seconds (down to at least a microsecond) of CPU  *|
|*    time.								      *|
|*    Since the Sun word is 32 bits, the counter rolls over in just over 2147 *|
|*    seconds (about 36 minutes).  main.c captures the compilation start and  *|
|*    end times from both clock() and time().  time()'s granularity is in     *|
|*    seconds.  For a Sun, (end_time - start_time) is passed to elapsed_time. *|
|*    If elapsed_time <= 2147 seconds, the compilation time is taken from     *|
|*    aux_elapsed_time (the value of the second call to clock() in main.c)    *|
|*    and is reported in milliseconds.  If the counter rolled over, the time  *|
|*    is reported in seconds.  Since the compilation time would have to be    *|
|*    at least 36 minutes, it was thought that 1 part in 36*3600 is           *|
|*    sufficiently accurate.						      *|
|*									      *|
|*    CLOCKS_PER_SEC is defined in time.h.				      *|
|*									      *|
\******************************************************************************/

void cif_summary_rec(char	*release_level,
		     char	*gen_date,
		     char	*gen_time,
		     float	 elapsed_time,
              	     long        aux_elapsed_time,
		     long        max_field_len)
{
   char		comp_time[13];
   int		hms;
   int		hours;
   int		milliseconds;
   int          minutes;
   int		seconds;


   TRACE (Func_Entry, "cif_summary_rec", NULL);

   if (max_field_len == -1) {              /* Signalling abort on 100 errors. */
      comp_time[0] = '0';
      comp_time[1] = NULL_CHAR;
   }
   else {
      
      hms = elapsed_time;


# if defined(_HOST_OS_UNICOS)  ||  (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX))  || defined(_HOST_OS_DARWIN)

      elapsed_time = elapsed_time - hms;
      milliseconds = (elapsed_time + .0005) * 1000;
      
# elif defined(_HOST_OS_MAX)

      milliseconds = ((aux_elapsed_time % CLOCKS_PER_SEC) + 500L) / 1000L;
      hms          = aux_elapsed_time / CLOCKS_PER_SEC;

# else /* defined(_HOST_OS_SOLARIS) */   

      if (hms <= 2147) {
         milliseconds = ((aux_elapsed_time % CLOCKS_PER_SEC) + 500L) / 1000L;
         hms          = aux_elapsed_time / CLOCKS_PER_SEC;
      }
      else {
         milliseconds = -1;
      }

# endif


      hours   = hms / 3600;
      hms     = hms % 3600;
      minutes = hms / 60;
      seconds = hms % 60;


# ifndef _HOST_OS_SOLARIS

      sprintf(comp_time, "%2.2d:%2.2d:%2.2d.%3.3d",
                         hours, minutes, seconds, milliseconds);

# else

      if (milliseconds >= 0) {
         sprintf(comp_time, "%2.2d:%2.2d:%2.2d.%3.3d",
                            hours, minutes, seconds, milliseconds);
      }
      else {
         sprintf(comp_time, "%2.2d:%2.2d:%2.2d", hours, minutes, seconds);
      }

# endif

   }

   Cif_Summary_Rec(c_i_f,
                   release_level,
                   gen_date, 
                   gen_time,
                   comp_time, 
                   max_field_len,
                   --curr_glb_line,
                   code_size, 
                   data_size);

   /* Don't leave contents of curr_glb_line destroyed.			      */

   ++curr_glb_line;


   TRACE (Func_Exit, "cif_summary_rec", NULL);

   return;

}  /* cif_summary_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Unit record [17].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_unit_rec(void)
{
   int		cif_col_num;
   int		file_line_num;
   int		glb_line_num;
   int		local_file_id;


   TRACE (Func_Entry, "cif_unit_rec", NULL);

   if (cif_pgm_unit_start_line == stmt_start_line) {

      /* For a program that is tremendously screwed up, it is possible for    */
      /* cif_unit_rec to be called from cif_fake_a_unit even before the Block */
      /* Stack has been set up.  If this is so, just fake the line and column.*/

      if (blk_stk_idx > 0) {
         glb_line_num = CURR_BLK_DEF_LINE;
         cif_col_num  = CURR_BLK_DEF_COLUMN;
      }
      else {
         glb_line_num = 1;
         cif_col_num  = 1;
      }
   }
   else {

      /* For the pathological case where the only significant line in the     */
      /* program contains an END statement, cif_pgm_unit_start_line is        */
      /* incremented before we get here.				      */

      glb_line_num = (cif_pgm_unit_start_line < stmt_start_line) ?
                        cif_pgm_unit_start_line : stmt_start_line;
      cif_col_num  = 1;
   }

   file_line_num = get_line_and_file_id(glb_line_num, &local_file_id);
   

   /* Write the Unit record in the actual CIF.				      */

   c_i_f = cif_actual_file;


   Cif_Unit_Rec(c_i_f,
                (scp_tbl != NULL) ?
                   AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)) : 
                   UNNAMED_PROGRAM_NAME,
                local_file_id,
                file_line_num,
                cif_col_num);

   cif_need_unit_rec  = FALSE; 
   cif_first_pgm_unit = FALSE; 

   last_msg_file_rec = CIF_UNIT;

   if (! cif_pgm_unit_error_recovery) {
      cif_copy_temp_to_actual_CIF();
   }

   TRACE (Func_Exit, "cif_unit_rec", NULL);

   return;

}  /* cif_unit_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Copy records from the temporary CIF to the actual CIF.  	      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_copy_temp_to_actual_CIF(void)
{
   char		cif_rec[256];                          /* Arbitrary size.     */


   TRACE (Func_Entry, "cif_copy_temp_to_actual_CIF", NULL);

   /* Copy any records in the temporary file to the actual CIF.  Rewind the   */
   /* temporary CIF file so it's ready for the next program unit (if one      */
   /* exists).			              				      */

   fprintf(cif_tmp_file, "%d\n", EOF);
   fflush(cif_tmp_file);
   rewind(cif_tmp_file);

   while (fgets(cif_rec, 256, cif_tmp_file) != NULL  &&  atoi(cif_rec) != EOF) {
      fputs(cif_rec, c_i_f);
   }

   rewind(cif_tmp_file);

   TRACE (Func_Exit, "cif_copy_temp_to_actual_CIF", NULL);
 
   return;

}  /* cif_copy_temp_to_actual_CIF */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output an End Unit record [18].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_end_unit_rec(char	*name_ptr)
{
   int		file_line_num;
   int		local_file_id;


   TRACE (Func_Entry, "cif_end_unit_rec", NULL);

   file_line_num = get_line_and_file_id(cif_end_unit_line, &local_file_id);

   cif_flush_include_recs();

   Cif_Endunit_Rec(c_i_f, 
                   name_ptr,
                   local_file_id,
                   file_line_num,
                   (cif_end_unit_column > 0) ?
                      cif_end_unit_column : stmt_start_col);

   last_msg_file_rec = CIF_ENDUNIT;

   TRACE (Func_Exit, "cif_end_unit_rec", NULL);

   return;

}  /* cif_end_unit_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Usage record [19].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      obj_idx     : An Attr index, IR index or symbol id depending on the   *|
|*		      value of obj_fld					      *|
|*      obj_fld     : AT_Tbl_Idx, IR_Tbl_Idx or NO_Tbl_Idx		      *|
|*      line_num    : the line number containing the symbol for which this    *|
|*                    Usage record is being produced			      *|
|*      col_num     : the column number for the symbol			      *|
|*      usage_code  : the CIF-defined usage code			      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_usage_rec(int			obj_idx,
		   fld_type		obj_fld,
		   int			line_num,
		   int			col_num,
		   int			usage_code)
{
   int		attr_idx;
   int		cif_symbol_id;
   int		file_line_num;
   int		local_file_id;
   opnd_type    opnd;


   TRACE (Func_Entry, "cif_usage_rec", NULL);
/*
   if (SH_CIF_SKIP_ME(curr_stmt_sh_idx) || usage_code == CIF_No_Usage_Rec) {
      goto EXIT;
   }
*/

   if (usage_code == CIF_No_Usage_Rec) {
      goto EXIT;
   }


   switch (obj_fld) {

   case AT_Tbl_Idx:
      attr_idx = obj_idx;
      AT_CIF_IN_USAGE_REC(attr_idx) = TRUE;

      if (AT_DCL_ERR(attr_idx)                                  || 
          (AT_COMPILER_GEND(attr_idx)                       &&
           (AT_OBJ_CLASS(attr_idx) != Data_Obj          ||
            ! (ATD_CLASS(attr_idx) == Compiler_Tmp  &&
               ATD_TMP_NEEDS_CIF(attr_idx))))) {
         goto EXIT;
      }

      /* If this is the specific name of an intrinsic procedure, we are       */
      /* looking at the compiler-generated name (like _NABS_ for IABS).  Get  */
      /* back to the interface Attr entry that describes the specific         */
      /* intrinsic (and that contains the name the user wrote in the program) */
      /* and use it instead.						      */

      if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit  &&  
          ATP_PROC(attr_idx) == Intrin_Proc   &&
          ! ATP_IN_INTERFACE_BLK(attr_idx)) {
         attr_idx = ATP_INTERFACE_IDX(attr_idx);
      }

           
      if (AT_CIF_SYMBOL_ID(attr_idx) == 0) {
         AT_CIF_SYMBOL_ID(attr_idx) = NEXT_SYMBOL_ID;
      }

      file_line_num = get_line_and_file_id(line_num, &local_file_id);

      Cif_Usage_Rec(c_i_f,
                    AT_CIF_SYMBOL_ID(attr_idx),
	            local_file_id,
		    file_line_num,
		    col_num, 
                    usage_code,
                    0,
                    NULL);

      break;

   case NO_Tbl_Idx:
      file_line_num = get_line_and_file_id(line_num, &local_file_id);

      Cif_Usage_Rec(c_i_f,
                    obj_idx,
	            local_file_id,
		    file_line_num,
		    col_num, 
                    usage_code,
                    0,
                    NULL);

      break;

   default:
      skip_struct_base	= TRUE;
      OPND_FLD(opnd)	= obj_fld;
      OPND_IDX(opnd)	= obj_idx;

      /* Get line and column of last component.				      */

      attr_idx = find_base_attr(&opnd, &line_num, &col_num);


      /* Get base attr.							      */

      attr_idx = find_left_attr(&opnd);

      if (AT_DCL_ERR(attr_idx)) {
         goto EXIT;
      }

      if (AT_CIF_SYMBOL_ID(attr_idx) == 0) {
         AT_CIF_SYMBOL_ID(attr_idx) = NEXT_SYMBOL_ID;
      }

      cif_symbol_id = AT_CIF_SYMBOL_ID(attr_idx);
      AT_CIF_IN_USAGE_REC(attr_idx) = TRUE;

      file_line_num = get_line_and_file_id(line_num, &local_file_id);

      if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%d%c%d",
                         CIF_USAGE, EOI,
		         cif_symbol_id, EOI,
		         local_file_id, EOI,
		         file_line_num, EOI,
		         col_num, EOI,
                         usage_code) < 0) {
         Cif_Error();
      }

      cif_number_of_struct_ids = 0;
    
      output_struct_ids(&opnd);

      if (fprintf(c_i_f, "%c%d", EOI, cif_number_of_struct_ids) < 0) {
         Cif_Error();
      }

      cif_number_of_struct_ids = -1;

      if (! output_struct_ids(&opnd)) {
         Cif_Error();
      }

      if (fprintf(c_i_f, "%c", EOR) < 0) {
         Cif_Error();
      }

      break;
   }

EXIT:

   TRACE (Func_Exit, "cif_usage_rec", NULL);

   return;

}  /* cif_usage_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Usage record [19] for a common block name.		      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      sb_idx      : stor blk index                                          *|
|*      line_num    : the line number containing the symbol for which this    *|
|*                    Usage record is being produced			      *|
|*      col_num     : the column number for the block			      *|
|*      usage_code  : the CIF-defined usage code			      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_sb_usage_rec(int			sb_idx,
	 	      int			line_num,
		      int			col_num,
		      cif_usage_code_type	usage_code)
{
   int		file_line_num;
   int		local_file_id;


   TRACE (Func_Entry, "cif_sb_usage_rec", NULL);

   file_line_num = get_line_and_file_id(line_num, &local_file_id);

   if (SB_CIF_SYMBOL_ID(sb_idx) == 0) {
      SB_CIF_SYMBOL_ID(sb_idx) = NEXT_SYMBOL_ID;
   }

   Cif_Usage_Rec(c_i_f,
                 SB_CIF_SYMBOL_ID(sb_idx),
		 local_file_id, 
		 file_line_num,
		 col_num, 
                 usage_code,
                 0,
                 NULL);

   TRACE (Func_Exit, "cif_sb_usage_rec", NULL);

   return;

}  /* cif_sb_usage_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output an Enable/Disable Compiler Options record [21].		      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_enable_disable_rec(void)
{
   long		enable_disable_opts;


   TRACE (Func_Entry, "cif_enable_disable_rec", NULL);

   enable_disable_opts = 0;

   if (on_off_flags.abort_if_any_errors) {
      enable_disable_opts = enable_disable_opts | CIF_F90_EDF_OPTa;
   }


# ifdef _ACCEPT_FLOW

   if (on_off_flags.flowtrace_option) {
      enable_disable_opts = enable_disable_opts | CIF_F90_EDF_OPTf;
   }

# endif


# ifdef _ACCEPT_CMD_ed_i

   if (on_off_flags.indef_init) {
      enable_disable_opts = enable_disable_opts | CIF_F90_EDF_OPTi;
   }

# endif


# ifdef _ACCEPT_CMD_ed_j

   if (on_off_flags.exec_doloops_once) { 
      enable_disable_opts = enable_disable_opts | CIF_F90_EDF_OPTj;
   }

# endif


   if (on_off_flags.issue_ansi_messages) {
      enable_disable_opts = enable_disable_opts | CIF_F90_EDF_OPTn;
   }

   if (on_off_flags.enable_double_precision) {
      enable_disable_opts = enable_disable_opts | CIF_F90_EDF_OPTp;
   }

   if (on_off_flags.abort_on_100_errors) {
      enable_disable_opts = enable_disable_opts | CIF_F90_EDF_OPTq;
   }


# ifdef _ACCEPT_CMD_ed_r

   if (on_off_flags.round_mult_operations) {
      enable_disable_opts = enable_disable_opts | CIF_F90_EDF_OPTr;
   }

# endif


   if (on_off_flags.alloc_autos_on_stack) {
      enable_disable_opts = enable_disable_opts | CIF_F90_EDF_OPTt;
   }

   if (on_off_flags.eu) { 
      enable_disable_opts = enable_disable_opts | CIF_F90_EDF_OPTu;
   }

   if (on_off_flags.save_all_vars) {
      enable_disable_opts = enable_disable_opts | CIF_F90_EDF_OPTv;
   }


# ifdef _ACCEPT_CMD_ed_A

   if (on_off_flags.MPP_apprentice) {
      enable_disable_opts = enable_disable_opts | CIF_F90_EDF_OPTA;
   }

# endif


   if (cmd_line_flags.binary_output) {
      enable_disable_opts = enable_disable_opts | CIF_F90_EDF_OPTB;
   }

   if (cmd_line_flags.assembly_output) {
      enable_disable_opts = enable_disable_opts | CIF_F90_EDF_OPTS;
   }


# ifdef _ACCEPT_CMD_ed_X

   if (on_off_flags.atexpert) {
      enable_disable_opts = enable_disable_opts | CIF_F90_EDF_OPTX;
   }

# endif


   Cif_EDopts_Rec(c_i_f, enable_disable_opts);

   TRACE (Func_Exit, "cif_enable_disable_rec", NULL);

   return;

}  /* cif_enable_disable_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Machine Characteristics record [22].			      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_machine_characteristics_rec(void)
{


# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)

   int          characteristics;

   union        {long   int_form;
                 char   char_form[9];
                } cpu_type;

# endif


   TRACE (Func_Entry, "cif_machine_characteristics_rec", NULL);


# ifdef _TARGET_OS_UNICOS

# ifdef _GETPMC_AVAILABLE

   /* Get the "primary" name of the machine.                                  */

   cpu_type.int_form     = target_machine.fld.mcpmt;
   cpu_type.char_form[8] = NULL_CHAR;


   /* Get the "characteristics" bitmask values.				      */

   characteristics = 0;

   if (target_machine.fld.mcvpop) {
      characteristics = characteristics | CIF_MC_VPOP;
   }

   if (target_machine.fld.mcema) {
      characteristics = characteristics | CIF_MC_EMA;
   }

   if (target_machine.fld.mccigs) {
      characteristics = characteristics | CIF_MC_CIGS;
   }

   if (target_machine.fld.mcpc) {
      characteristics = characteristics | CIF_MC_PCF;
   }

   if (target_machine.fld.mcrdvl) {
      characteristics = characteristics | CIF_MC_READVL;
   }

   if (target_machine.fld.mcvrcr) {
      characteristics = characteristics | CIF_MC_VRECUR;
   }

   if (target_machine.fld.mcavl) {
      characteristics = characteristics | CIF_MC_AVL;
   }

   if (target_machine.fld.mchpm) {
      characteristics = characteristics | CIF_MC_HPF;
   }

   if (target_machine.fld.mcbdm) {
      characteristics = characteristics | CIF_MC_BDM;
   }

   if (target_machine.fld.mcstr) {
      characteristics = characteristics | CIF_MC_SREG;
   }

   if (target_machine.fld.mcstr) {
      characteristics = characteristics | CIF_MC_CLUSTER;
   }

   if (target_machine.fld.mccori) {
      characteristics = characteristics | CIF_MC_COR;
   }

   if (target_machine.fld.mcaddr32) {
      characteristics = characteristics | CIF_MC_ADDR32;
   }

   if (target_machine.fld.mcbmm) {
      characteristics = characteristics | CIF_MC_BMM;
   }

   if (target_machine.fld.mcxea) {
      characteristics = characteristics | CIF_MC_XEA;
   }

   if (target_machine.fld.mcavpop) {
      characteristics = characteristics | CIF_MC_AVPOP;
   }

   if (target_machine.fld.mcfullsect) {
      characteristics = characteristics | CIF_MC_FULLSECT;
   }

   if (target_machine.fld.mcieee) {
      characteristics = characteristics | CIF_MC_IEEE;
   }

   if (target_machine.fld.mccmrreq) {
      characteristics = characteristics | CIF_MC_CMRREQ;
   }

   if (target_machine.fld.mccache) {
      characteristics = characteristics | CIF_MC_CACHE;
   }

   Cif_Mach_Char_Rec(c_i_f,
		     cpu_type.char_form,
		     target_machine.fld.mcmspd,
		     target_machine.fld.mcmsz, 
		     characteristics, 
		     target_machine.fld.mcbank,
 		     target_machine.fld.mcncpu,
		     target_machine.fld.mcibsz,
		     target_machine.fld.mcclk, 
		     target_machine.fld.mcncl,
		     target_machine.fld.mcbbsy,
                     TARGET_BITS_PER_WORD);


# else


   /* Assume since target is UNICOS and host is not that this is a DPE        */
   /* compiler.								      */
   /* Dummy up a record until the machine characteristics library routine has */
   /* been ported.  mcpmt already has the target machine name in it in char   */
   /* form.								      */
 
   Cif_Mach_Char_Rec(c_i_f,
                     target_machine.fld.mcpmt, 
                     -1L,
                     -1L,
                     0, 
                     -1L,
                     -1L,
                     -1L,
                     -1L,
                     -1L,
                     -1L,
                     TARGET_BITS_PER_WORD);

# endif


# endif


# ifdef _TARGET_OS_MAX


# if defined(_GETPMC_AVAILABLE)

   /* Get the "primary" name of the machine.                                  */

   cpu_type.int_form     = target_machine.fld.mcpmt;
   cpu_type.char_form[8] = NULL_CHAR;


   Cif_Mach_Char_Rec(c_i_f,
		     cpu_type.char_form,
		     -1L, 
		     target_machine.fld.mcmsz,
		     0, 
		     -1L,
 		     -1L,
		     -1L,
		     -1L,
		     -1L,
		     -1L,
                     TARGET_BITS_PER_WORD);

# else


   /* Assume since target is MAX and host is not UNICOS that this is a cross  */
   /* compiler.								      */
   /* Dummy up a record until the machine characteristics library routine has */
   /* been ported.  mcpmt already has the target machine name in it in char   */
   /* form.        							      */
 
   Cif_Mach_Char_Rec(c_i_f,
                     target_machine.fld.mcpmt,
                     -1L, 
                     -1L, 
                     0, 
                     -1L,
                     -1L, 
                     -1L, 
                     -1L, 
                     -1L, 
                     -1L,
                     TARGET_BITS_PER_WORD);


# endif


# endif


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)


   /* Produce a dummy Machine Characteristics record for a IRIX.             */

   Cif_Mach_Char_Rec(c_i_f,
		     "IRIX",
		     -1L, 
		     -1L, 
		     0, 
		     -1L,
 		     -1L,
		     -1L,
		     -1L,
		     -1L,
		     -1L,
                     TARGET_BITS_PER_WORD);

# elif defined(_TARGET_OS_SOLARIS)

   /* Produce a dummy Machine Characteristics record for a SPARC.             */

   Cif_Mach_Char_Rec(c_i_f,
		     "SPARC",
		     -1L, 
		     -1L, 
		     0, 
		     -1L,
 		     -1L,
		     -1L,
		     -1L,
		     -1L,
		     -1L,
                     TARGET_BITS_PER_WORD);
# endif


   TRACE (Func_Exit, "cif_machine_characteristics_rec", NULL);

   return;

}  /* cif_machine_characteristics_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Statement Type record [25].				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      exact_stmt_type_known :						      *|
|*      exact_stmt_type       :						      *|
|*      stmt_number           :						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_stmt_type_rec(boolean		exact_stmt_type_known,
		       cif_stmt_type	exact_stmt_type,
		       int		stmt_number)
{
   int			file_line_num;
   int			local_file_id;
   cif_stmt_type	local_stmt_type;


   TRACE (Func_Entry, "cif_stmt_type_rec", NULL);

   local_stmt_type = (exact_stmt_type_known) ?
                        exact_stmt_type : mapped_stmt_type[stmt_type];

   switch (local_stmt_type) {

      case CIF_Not_Exact:
         if (comp_phase < Decl_Semantics  &&  stmt_type == Assignment_Stmt) {

            /* Place a Statement_Num_Stmt before this to hold the value of    */
            /* stmt_number in its SH_PARENT_BLK_IDX field.                    */

            gen_sh(Before, Statement_Num_Stmt, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);
            SH_PARENT_BLK_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = stmt_number;
         }

         break;

      case CIF_Stmt_Type_Error:
         PRINTMSG(stmt_start_line, 776, Internal, stmt_start_col);

      default:
         file_line_num = get_line_and_file_id(stmt_start_line, &local_file_id);

         Cif_Stmt_Type_Rec(c_i_f,
		           local_stmt_type,
			   local_file_id, 
			   file_line_num,
			   stmt_start_col,
                           stmt_number);
   }

   TRACE (Func_Exit, "cif_stmt_type_rec", NULL);

   return;

}  /* cif_stmt_type_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Continuation Line record [27].				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      continuation_type : 0 = source line, 1 = CDIR$ line		      *|
|*      line_number       : line number of the continuation line    	      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_cont_line_rec(int	continuation_type,
                       int   	line_number)
{
   int		file_line_num;  /* placeholder */
   int		local_file_id;


   file_line_num = get_line_and_file_id(line_number, &local_file_id);

   Cif_Continuation_Rec(c_i_f,
                        continuation_type,
                        local_file_id,
                        line_number,
                        1);

   return;

}  /* cif_cont_line_rec */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*    Output a Call Site [28] record.                                         *|
|*									      *|
|* Input parameters:							      *|
|*	ir_idx - ir idx for call operator.                                    *|
|*      gen_idx - attr_idx of original call attr.                             *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void  cif_call_site_rec(int	ir_idx,
			int	gen_idx)

{
   int		array_type;
   long         attributes;
   int		attr_idx;
   int		bd_idx;
   char		buffer[160];			/* Copied from cif_send_attr. */
   char		char_len[20];
   int          column;
   int          derived_type_id;
   int		file_line_num;
   int		i;
   int		info_idx;
   int		k;
   int		list_idx;
   int		local_file_id;
   int          misc_attrs;
   int		num_args;
   int          num_dims;
   opnd_type    opnd;
   int          pgm_unit_type;
   int          rslt_id;
   int		save_reference;
   int		spec_idx;
   int		specific_symbol_id;
   char		string[20];                     /* Copied from cif_send_attr. */
   int		symbol_id;
   int		type;
   char		var_len_bound[3];


   TRACE (Func_Entry, "cif_call_site_rec", NULL);

/*
   if (SH_CIF_SKIP_ME(curr_stmt_sh_idx)) {
      TRACE (Func_Exit, "cif_call_site_rec", NULL);
      return;
   }
*/

   skip_struct_base	= FALSE;
   file_line_num	= get_line_and_file_id(IR_LINE_NUM_L(ir_idx), 
                                               &local_file_id);
   spec_idx		= IR_IDX_L(ir_idx);

   /* If this is a generic procedure reference and there is something wrong   */
   /* with the generic procedure Attr, then don't output the Call Site record */
   /* because other records might not be generated to resolve symbol ids in   */
   /* the Call Site record.						      */

   if (spec_idx != gen_idx  &&  AT_DCL_ERR(gen_idx)) {
      goto EXIT;
   }

   num_args = IR_LIST_CNT_R(ir_idx);
   list_idx = IR_IDX_R(ir_idx);

   for (i = 1; i <= num_args; i++) {

      info_idx = IL_ARG_DESC_IDX(list_idx);

      if (info_idx == 0) {
         /* cif id is 0 */
      }
      else if (arg_info_list[info_idx].ed.component) {
         arg_info_list[info_idx].ed.cif_id = list_idx;
      }
      else if (arg_info_list[info_idx].ed.cif_id != 0) {
         /* intentionally blank */
      }
      else if (arg_info_list[info_idx].ed.reference  || 
               (IL_FLD(list_idx) == AT_Tbl_Idx  && 
                ! AT_COMPILER_GEND(IL_IDX(list_idx)))) {

         /* If no CIF symbol id yet, get one. */

         COPY_OPND(opnd, IL_OPND(list_idx));
         attr_idx = find_left_attr(&opnd);

         if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit  &&
             ATP_PROC(attr_idx) == Intrin_Proc) {
            attr_idx = ATP_INTERFACE_IDX(attr_idx);
         }

         if (AT_CIF_SYMBOL_ID(attr_idx) == 0) {
            AT_CIF_SYMBOL_ID(attr_idx) = NEXT_SYMBOL_ID;
         }

         arg_info_list[info_idx].ed.cif_id = AT_CIF_SYMBOL_ID(attr_idx);
      }
      else {
         symbol_id	= NEXT_SYMBOL_ID;
         char_len[0]	= NULL_CHAR;

         COPY_OPND(opnd, IL_OPND(list_idx));
         attr_idx	= find_left_attr(&opnd);

         type = (arg_info_list[info_idx].ed.type == Structure) ?
                 ATT_CIF_DT_ID(TYP_IDX(arg_info_list[info_idx].ed.type_idx)) :
                 arg_info_list[info_idx].ed.linear_type;

         if (arg_info_list[info_idx].ed.type == Character) {

            if (arg_info_list[info_idx].ed.char_len.fld == CN_Tbl_Idx) {
               convert_to_string(
                          &CN_CONST(arg_info_list[info_idx].ed.char_len.idx),
                           CN_TYPE_IDX(arg_info_list[info_idx].ed.char_len.idx),
                           char_len);
            }
            else {
               char_len[0] = VAR_LEN_CHAR;
               char_len[1] = NULL_CHAR;
            }
         }

         misc_attrs	 = 0;
         derived_type_id = 0;

         if (arg_info_list[info_idx].ed.constant) {

            if (IL_FLD(list_idx) == CN_Tbl_Idx) {
               attr_idx = IL_IDX(list_idx);
            }
         }
             
         num_dims = arg_info_list[info_idx].ed.rank;

         array_type = (num_dims > 0) ? 1   /* explicit shape */  : 0;

         if (fprintf(c_i_f, 
                     "%d%c%s%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%s%c%d%c%d",
                     CIF_F90_OBJECT, EOI,
                     "", EOI,
                     symbol_id, EOI,
                     SCP_CIF_ID(curr_scp_idx), EOI,
                     cif_data_type(type), EOI,
                     2, EOI,
                     7,EOI,
                     0,EOI,
                     -1, EOI,
                     misc_attrs, EOI,
                     derived_type_id, EOI,
                     char_len, EOI,
                     num_dims, EOI,
                     array_type) < 0) {
            Cif_Error();
            goto EXIT;
         }

         buffer[0] = NULL_CHAR;

         var_len_bound[0] = EOI;
         var_len_bound[1] = VAR_LEN_CHAR;
         var_len_bound[2] = NULL_CHAR;

         for (k = 0; k < num_dims; k++) {

            if (arg_info_list[info_idx].ed.constant &&
                attr_idx != NULL_IDX)               {

               bd_idx = ATD_ARRAY_IDX(attr_idx);

               if (BD_LB_FLD(bd_idx, k+1) == CN_Tbl_Idx) {
                  sprintf(string, "%c%s",
                          EOI,
                          convert_to_string(&CN_CONST(BD_LB_IDX(bd_idx,k+1)),
                                             CN_TYPE_IDX(BD_LB_IDX(bd_idx,k+1)),
                                             outbuf1));
                  strcat(buffer, string);
               }
               else {
                  strcat(buffer, var_len_bound);
               }

               if (BD_UB_FLD(bd_idx, k+1) == CN_Tbl_Idx) {
                  sprintf(string, "%c%s",
                          EOI,
                          convert_to_string(&CN_CONST(BD_UB_IDX(bd_idx,k+1)),
                                             CN_TYPE_IDX(BD_UB_IDX(bd_idx,k+1)),
                                             outbuf1));
                  strcat(buffer, string);
               }
               else {
                  strcat(buffer, var_len_bound);
               }
            }
            else {
               buffer[0] = EOI;
               buffer[1] = '1';
               buffer[2] = NULL_CHAR;
          
               if (OPND_FLD(arg_info_list[info_idx].ed.shape[k]) == 
                                                                  CN_Tbl_Idx) {
                  sprintf(string, "%c%s",
                          EOI,
                          convert_to_string(
                             &CN_CONST(OPND_IDX(
                                          arg_info_list[info_idx].ed.shape[k])),
                              CN_TYPE_IDX(OPND_IDX(
                                         arg_info_list[info_idx].ed.shape[k])),
                              outbuf1));
                  strcat(buffer, string);
               }
               else {
                  strcat(buffer, var_len_bound);
               }
            }

            if (fprintf(c_i_f, "%s", buffer) < 0) {
               Cif_Error();
               goto EXIT;
            }
         }

         if (fprintf(c_i_f, "%c%d%c%d%c%d%c",
                            EOI,
                            0, EOI,                         /*  Distribution  */
                            0, EOI,			    /*  Geometry id   */
                            0, EOR) < 0) {		    /*  CRI ptr id    */
            Cif_Error();
            goto EXIT;
         }

         arg_info_list[info_idx].ed.cif_id = symbol_id;
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   if (ATP_PROC(spec_idx) == Intrin_Proc && !ATI_USER_SPECIFIED(gen_idx)) {

      /* Intrinsic call where the intrinsic name was NOT specified as the     */
      /* generic name on a user interface block.			      */

      if (AT_CIF_SYMBOL_ID(gen_idx) == 0) {
         AT_CIF_SYMBOL_ID(gen_idx) = NEXT_SYMBOL_ID;
      }

      symbol_id          = AT_CIF_SYMBOL_ID(gen_idx);

      if (AT_CIF_SYMBOL_ID(spec_idx) == 0) {
         AT_CIF_SYMBOL_ID(spec_idx) = NEXT_SYMBOL_ID;
      }

      specific_symbol_id = 0;
      specific_symbol_id = AT_CIF_SYMBOL_ID(spec_idx);

      column = (ATP_PGM_UNIT(spec_idx) == Function) ? IR_COL_NUM_L(ir_idx) :
                                                      IR_COL_NUM(ir_idx);
      if (! ATI_CIF_SEEN_IN_CALL(gen_idx)) {

         /* Issue an entry record for the interface.  These do not go out */
         /* in cif_send_attr but need to be specially sent through here.  */

         rslt_id = 0;

         if (ATI_INTERFACE_CLASS(gen_idx) == Generic_Function_Interface) {
            pgm_unit_type = CIF_F90_ET_FUNCTION;

            /* Just set the result's symbol id for now.  The Object record    */
            /* will be produced via calls directly from call site processing  */
            /* code after all information about the function being called has */
            /* been resolved.  The specific entry will be sent at that time.  */

            if (AT_CIF_SYMBOL_ID(ATP_RSLT_IDX(spec_idx)) == 0) {
               AT_CIF_SYMBOL_ID(ATP_RSLT_IDX(spec_idx)) = NEXT_SYMBOL_ID;
            }
         
            rslt_id = AT_CIF_SYMBOL_ID(ATP_RSLT_IDX(spec_idx));
         }
         else if (ATI_INTERFACE_CLASS(gen_idx) == Generic_Subroutine_Interface){
            pgm_unit_type = CIF_F90_ET_SUBROUTINE;

            /* Send the specific entry as it will not get sent when the */
            /* symbol table gets sent as intrinsics are special cased.  */

            save_reference	 = AT_REFERENCED(spec_idx);
            AT_REFERENCED(spec_idx) = Referenced;
            cif_send_attr(spec_idx, NULL_IDX);
            AT_REFERENCED(spec_idx) = save_reference;
         
         }
         else {
            pgm_unit_type = CIF_F90_ET_UNKNOWN;
            cif_send_attr(spec_idx, NULL_IDX);
         }
  
         attributes = CIF_PGM_REFERENCE;
  
         if (AT_PRIVATE(gen_idx)) {
            attributes = attributes | CIF_PGM_PRIVATE;
         }

         Cif_F90_Entry_Rec(c_i_f,
                           AT_OBJ_NAME_PTR(gen_idx),
                           AT_CIF_SYMBOL_ID(gen_idx),
                           SCP_CIF_ID(curr_scp_idx), 
                           pgm_unit_type, 
                           CIF_F90_PT_INTRINSIC,
                           attributes, 
                           rslt_id, 
                           0, 
                           0, 
                           NULL);
      }
   }
   else if (spec_idx == gen_idx) {     /*  Specific call.	*/

      if (AT_CIF_SYMBOL_ID(spec_idx) == 0) {
         AT_CIF_SYMBOL_ID(spec_idx) = NEXT_SYMBOL_ID;
      }

      symbol_id = AT_CIF_SYMBOL_ID(spec_idx);
      specific_symbol_id = 0;

      if (ATP_PGM_UNIT(spec_idx) == Function) {
         column = IR_COL_NUM_L(ir_idx);
      }
      else {
         column = IR_COL_NUM(ir_idx);
      }
   }
   else if (ATI_INTERFACE_CLASS(gen_idx) == Defined_Assign_Interface) {
      
      if (AT_CIF_SYMBOL_ID(spec_idx) == 0) {
         AT_CIF_SYMBOL_ID(spec_idx) = NEXT_SYMBOL_ID;
      }

      specific_symbol_id = AT_CIF_SYMBOL_ID(spec_idx);

      if (AT_CIF_SYMBOL_ID(gen_idx) == 0) {
         AT_CIF_SYMBOL_ID(gen_idx) = NEXT_SYMBOL_ID;
      }

      symbol_id = AT_CIF_SYMBOL_ID(gen_idx);
      column    = IR_COL_NUM(ir_idx);
   }
   else {
      
      if (AT_CIF_SYMBOL_ID(spec_idx) == 0) {
         AT_CIF_SYMBOL_ID(spec_idx) = NEXT_SYMBOL_ID;
      }

      specific_symbol_id = AT_CIF_SYMBOL_ID(spec_idx);

      if (AT_CIF_SYMBOL_ID(gen_idx) == 0) {
         AT_CIF_SYMBOL_ID(gen_idx) = NEXT_SYMBOL_ID;
      }

      symbol_id = AT_CIF_SYMBOL_ID(gen_idx);

      if (ATI_INTERFACE_CLASS(gen_idx) == Generic_Function_Interface) {
         column = IR_COL_NUM_L(ir_idx);
      }
      else {
         column = IR_COL_NUM(ir_idx);
      }
   }

   if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d",
               CIF_F90_CALLSITE, EOI,
               symbol_id, EOI,
               SCP_CIF_ID(curr_scp_idx), EOI,
               local_file_id, EOI,
               file_line_num, EOI,
               column, EOI,
               specific_symbol_id, EOI,
               num_args) < 0) {

      Cif_Error();
      goto EXIT;
   }


   /* Output the symbol IDs for the actual arguments.			      */

   list_idx = IR_IDX_R(ir_idx);

   for (i = 1; i <= num_args; i++) {

      info_idx = IL_ARG_DESC_IDX(list_idx);

      if (info_idx == 0) {

         if (fprintf(c_i_f, "%c%d", EOI, 0) < 0) {
            Cif_Error();
            goto EXIT;
         }
      }
      else if (arg_info_list[info_idx].ed.component) {

         if (fprintf(c_i_f, "%c%c", EOI, '%') < 0) {
            Cif_Error();
            goto EXIT;
         }

         COPY_OPND(opnd, 
                   IL_OPND(arg_info_list[info_idx].ed.cif_id));

         cif_number_of_struct_ids = 0;

         output_struct_ids(&opnd);

         if (fprintf(c_i_f, "%c%d", EOI, cif_number_of_struct_ids) < 0) {
            Cif_Error();
            goto EXIT;
         }

         cif_number_of_struct_ids = -1;

         if (! output_struct_ids(&opnd)) {
            Cif_Error();
            goto EXIT;
         }

         if (fprintf(c_i_f, "%c%c", EOI, '%') < 0) {
            Cif_Error();
            goto EXIT;
         }
      }
      else {
         if (fprintf(c_i_f, "%c%d",
                     EOI, 
                     arg_info_list[info_idx].ed.cif_id) < 0) {
            Cif_Error();
            goto EXIT;
         }
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }


   /* Output the rank for the each actual argument.			      */

   list_idx = IR_IDX_R(ir_idx);

   for (i = 1; i <= num_args; i++) {

      info_idx = IL_ARG_DESC_IDX(list_idx);

      if (info_idx == 0) {

         if (fprintf(c_i_f, "%c%d", EOI, 0) < 0) {
            Cif_Error();
            goto EXIT;
         }
      }
      else {

         if (fprintf(c_i_f, "%c%d",
                     EOI, 
                     arg_info_list[info_idx].ed.rank) < 0) {
            Cif_Error();
            goto EXIT;
         }
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   if (fprintf(c_i_f,"%c", EOR) < 0) {
      Cif_Error();
      goto EXIT;
   }

EXIT:

   TRACE (Func_Exit, "cif_call_site_rec", NULL);

   return;

}  /* cif_call_site_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Named Constant record [30].				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      attr_idx     : AT table index of the named constant                   *|
|*      start_line   : line where the value starts			      *|
|*      start_column : column where the value starts 			      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_named_constant_rec(int		attr_idx,
			   int		start_line,
			   int		start_column)
{
   int		cn_idx;
   int		const_idx;
   int		end_col;
   int		end_line;
   int		file_id;
   long64	length;
   boolean	ok;
   long_type	result[MAX_WORDS_FOR_NUMERIC];
   char		str[80];
   int		type_idx;


   TRACE (Func_Entry, "cif_named_constant_rec", NULL);

   if (AT_CIF_SYMBOL_ID(attr_idx) == 0) {
      AT_CIF_SYMBOL_ID(attr_idx) = NEXT_SYMBOL_ID;
   }

   if (TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) != Structure_Type  &&
       ATD_ARRAY_IDX(attr_idx) == NULL_IDX) {
      const_idx = ATD_CONST_IDX(attr_idx);
   }
   else {
      const_idx = NULL_IDX;
   }

   get_line_and_file_id(start_line, &file_id);

   if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c",
                      CIF_F90_CONST, EOI,
                      AT_CIF_SYMBOL_ID(attr_idx), EOI,
                      (c_i_f == cif_actual_file) ? 
                                SCP_CIF_ID(curr_scp_idx) : 1, EOI,
                      (const_idx) ? 0 : 1, EOI) < 0) {
      Cif_Error();
   }

   if (const_idx) {

      switch (TYP_TYPE(ATD_TYPE_IDX(attr_idx))) {

         case Logical:
            if (fprintf(c_i_f, "%s", 
                               (THIS_IS_TRUE(&(CN_CONST(const_idx)),
                                             CN_TYPE_IDX(const_idx)) ?
                                           
                                  ".TRUE." : ".FALSE.")) < 0) {
               Cif_Error();
            }
   
            break;

         case Integer:
         case Real:
         case Complex:

            if (fprintf(c_i_f, "%s", convert_to_string(&CN_CONST(const_idx),
                                                       CN_TYPE_IDX(const_idx),
                                                       str)) < 0) {
               Cif_Error();
            }

            break;

         case Typeless:
            if (TYP_LINEAR(CN_TYPE_IDX(const_idx)) == Typeless_4 ||
                TYP_LINEAR(CN_TYPE_IDX(const_idx)) == Typeless_8) {

               if (fprintf(c_i_f, "%s",
                                  convert_to_string(&CN_CONST(const_idx),
                                                    CN_TYPE_IDX(const_idx),
                                                    str)) < 0) {
                  Cif_Error();
               }
            }
            else if (fprintf(c_i_f, "%s", (char *) &CN_CONST(const_idx)) < 0) {
               Cif_Error();
            }

            break;

         case Character:

            C_TO_F_INT(result, TARGET_CHARS_PER_WORD, CG_INTEGER_DEFAULT_TYPE);

            cn_idx	= TYP_IDX(CN_TYPE_IDX(const_idx));
            type_idx	= CG_INTEGER_DEFAULT_TYPE;

            ok		= folder_driver((char *) &CN_CONST(cn_idx),
                                        CN_TYPE_IDX(cn_idx),
                                        (char *) result,
                                        type_idx,
                                        result,
                                        &type_idx,
                                        stmt_start_line,
                                        stmt_start_col,
                                        2,
                                        Mod_Opr);

            ok	       |= folder_driver((char *) result,
                                        type_idx,
                                        (char *) &CN_CONST(CN_INTEGER_ZERO_IDX),
                                        CN_TYPE_IDX(CN_INTEGER_ZERO_IDX),
                                        result,
                                        &type_idx,
                                        stmt_start_line,
                                        stmt_start_col,
                                        2,
                                        Eq_Opr);
 

            if (ok && THIS_IS_TRUE(result, type_idx)) {
     
               if (fprintf(c_i_f, "%s", (char *) &CN_CONST(const_idx)) < 0) {
                  Cif_Error();
               }
            }
            else {
               length = CN_INT_TO_C(TYP_IDX(CN_TYPE_IDX(const_idx)));
               ((char *) &CN_CONST(const_idx)) [length] = NULL_CHAR;

               if (fprintf(c_i_f, "%s", (char *) &CN_CONST(const_idx)) < 0) {
                  Cif_Error();
               }

               ((char *) &CN_CONST(const_idx)) [length] = ' ';
            }

            break;

      } /* End switch */
   }

   /* If start_line is 0, it means cif_named_constant_rec is being called from*/
   /* the ATD_CLASS == Constant case of cif_send_attr to spit out a dummy     */
   /* Named Constant record for a use associated named constant.	      */

   if (start_line != 0) {
      prev_char_line_and_col(&end_line, &end_col);
   }
   else {
      file_id  = 0;
      end_line = 0;
      end_col  = 0;
   }

   if (fprintf(c_i_f, "%c%d%c%d%c%d%c%d%c%d%c",
                      EOI,
                      file_id, EOI,
                      start_line, EOI,
                      start_column, EOI,
                      end_line, EOI,
                      end_col, EOR) < 0) {
      Cif_Error();
   }

   TRACE (Func_Exit, "cif_named_constant_rec", NULL);

   return;

}  /* cif_named_constant_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Loop Definitions record [32].				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_loop_def_rec(void)
{
   int		construct_name_id;
   int		do_sh_idx;
   int		do_var_idx;
   int		end_file_id;
   int		end_line;
   int		il_idx;
   int          loop_info_il_idx;
   int		loop_ir_idx;
   int		loop_label_id;
   int		loop_type;
   int      	lcv_symbol_id;
   int 		start_file_id;
   int		start_line;


   TRACE (Func_Entry, "cif_loop_def_rec", NULL);

   do_sh_idx = SH_PARENT_BLK_IDX(curr_stmt_sh_idx);

   if (do_sh_idx == NULL_IDX || SH_COMPILER_GEN(do_sh_idx)) {
      TRACE (Func_Exit, "cif_loop_def_rec", NULL);
      return;
   }

   loop_ir_idx      = SH_IR_IDX(do_sh_idx);
   loop_info_il_idx = IR_IDX_R(loop_ir_idx);

   if (SH_STMT_TYPE(do_sh_idx) == Do_Iterative_Stmt) {
      loop_type = CIF_LP_DO;

      /* There are two version of this.  The first is what we need */
      /* The second should only be compiler generated so we should */
      /* not get here, but I have code here just in case.          */

      if (IL_FLD(loop_info_il_idx) == IL_Tbl_Idx) {
         il_idx    = IL_IDX(loop_info_il_idx);
      }
      else {
         il_idx    = loop_info_il_idx;
      }

      if (IL_FLD(il_idx) == AT_Tbl_Idx) {
         do_var_idx = IL_IDX(il_idx);
      }
      else {

         /* Had better be a Dv_Deref IR.                                      */

         do_var_idx = IR_IDX_L(IL_IDX(il_idx));
      }

      if (AT_CIF_SYMBOL_ID(do_var_idx) == 0) {
         AT_CIF_SYMBOL_ID(do_var_idx) = NEXT_SYMBOL_ID;
      }

      lcv_symbol_id = AT_CIF_SYMBOL_ID(do_var_idx);
   }
   else {
      loop_type = (SH_STMT_TYPE(do_sh_idx) == Do_While_Stmt) ?
                     CIF_LP_DOWHILE : CIF_LP_DO_INFINITE;
      lcv_symbol_id = 0;
   }


   /* Get the line number and file id of the DO statement and the file id of  */
   /* line that ends the DO loop.  For the ending line case, the line number  */
   /* is already known (obtained from the Statement_Number SH already         */
   /* processed by prog_unit_semantics).				      */

   start_line = get_line_and_file_id(SH_GLB_LINE(do_sh_idx),
			 	     &start_file_id);

   end_line = get_line_and_file_id(stmt_end_line, &end_file_id);


   loop_info_il_idx = IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(loop_info_il_idx));
   il_idx = IL_IDX(loop_info_il_idx);

   if (IL_FLD(il_idx) == NO_Tbl_Idx) {
      loop_label_id = 0;
   }
   else {

      if (AT_CIF_SYMBOL_ID(IL_IDX(il_idx)) == 0) {
         AT_CIF_SYMBOL_ID(IL_IDX(il_idx)) = NEXT_SYMBOL_ID;
      }

      loop_label_id = AT_CIF_SYMBOL_ID(IL_IDX(il_idx));
   }

   il_idx = IL_NEXT_LIST_IDX(il_idx);

   if (IL_FLD(il_idx) == NO_Tbl_Idx) {
      construct_name_id = 0;
   }
   else {

      if (AT_CIF_SYMBOL_ID(IL_IDX(il_idx)) == 0) {
         AT_CIF_SYMBOL_ID(IL_IDX(il_idx)) = NEXT_SYMBOL_ID;
      }

      construct_name_id = AT_CIF_SYMBOL_ID(IL_IDX(il_idx));
   }
      
   Cif_F90_Loop_Rec(c_i_f,
         	    SCP_CIF_ID(curr_scp_idx),
       		    loop_type, 
		    start_file_id,
		    start_line, 
		    SH_COL_NUM(do_sh_idx),
		    end_file_id, 
		    end_line, 
		    stmt_end_col,
		    lcv_symbol_id, 
		    loop_label_id,
		    construct_name_id,
                    statement_number);

   TRACE (Func_Exit, "cif_loop_def_rec", NULL);

   return;

}  /* cif_loop_def_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Label record [34].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      attr_idx : the label's Attr table index				      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_label_rec(int	attr_idx)
			
{
   int	label_class;


   TRACE(Func_Entry, "cif_label_rec", NULL);

   switch (ATL_CLASS(attr_idx)) {

      case Lbl_Unknown:
         label_class = CIF_LB_UNKNOWN;
         break;

      case Lbl_User:
         label_class = CIF_LB_STMT;
         break;

      case Lbl_Format:
         label_class = CIF_LB_FORMAT;
         break;

      case Lbl_Debug:
      case Lbl_Internal:
         goto EXIT;

      default:
         label_class = CIF_LB_CONSTRUCT;
   }

   if (AT_CIF_SYMBOL_ID(attr_idx) == 0) {
      AT_CIF_SYMBOL_ID(attr_idx) = NEXT_SYMBOL_ID;
   }

   Cif_F90_Label_Rec(c_i_f,
                     AT_OBJ_NAME_PTR(attr_idx),
                     AT_CIF_SYMBOL_ID(attr_idx),
                     SCP_CIF_ID(curr_scp_idx), 
                     label_class);

EXIT:

   TRACE(Func_Exit, "cif_label_rec", NULL);

   return;

}  /* cif_label_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Miscellaneous Compiler Options record [37].		      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_misc_compiler_opts_rec(void)
{
   char         char_msg_num[5];
   int		i;
   int		int_len			= 0;
   int		j;
#ifdef KEY /* Bug 10177 */
   int		msg_level = 0;
#else /* KEY Bug 10177 */
   int		msg_level;
#endif /* KEY Bug 10177 */
   char		work_buf[512];
   char		null_string[1] = "";
   int		num_items;
   int		num_paths;
   int		path_idx;


   TRACE (Func_Entry, "cif_misc_compiler_opts_rec", NULL);


   if (cmd_line_flags.integer_32) {
         int_len = 2;
   }

   switch (cmd_line_flags.msg_lvl_suppressed) {

      case Comment_Lvl:
         msg_level = 0;
         break;

      case Note_Lvl:
         msg_level = 1;
         break;

      case Caution_Lvl:
         msg_level = 2;
         break;

      case Warning_Lvl:
         msg_level = 3;
         break;

      case Error_Lvl:
         msg_level = 4;
   }


   /* Gather information about the -M (message suppress) option.              */

   num_items   = 0;
   work_buf[0] = NULL_CHAR;

   for (i = 0;  i < MAX_MSG_SIZE;  ++i) {

      if (message_suppress_tbl[i] != 0) { 

         for (j = i * HOST_BITS_PER_WORD;
              j < (i + 1) * HOST_BITS_PER_WORD;
              ++j) {
            
            if (GET_MESSAGE_TBL(message_suppress_tbl, j)) {
               ++num_items;
               sprintf(char_msg_num, "%d%c", j, EOI);
               strcat(work_buf, char_msg_num);
            }
         }
      }
   }

   if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%d%c%d%c%d%c", 
                      CIF_F90_MISC_OPTS, EOI,
		      int_len, EOI,
		      msg_level, EOI,
		      (cmd_line_flags.verify_option) ? 1 : 0, EOI,

		      (on_off_flags.round_mult_operations) ? 0 : 1, EOI,

		      (!on_off_flags.round_mult_operations) ? 
		         cmd_line_flags.truncate_bits : 0, EOI,
		      num_items, EOI) < 0) {
      Cif_Error();
   }

   if (num_items > 0) {
         
      if (fprintf(c_i_f, "%s", work_buf) < 0) {
         Cif_Error();
      }
   }


   /* Gather information about the directive suppress (-x) option.            */

   num_items   = 0;
   work_buf[0] = NULL_CHAR;

   if (cmd_line_flags.disregard_all_directives) {
      ++num_items; 
      strcat(work_buf, "all");
      strcat(work_buf, "\036");
   }

   if (cmd_line_flags.disregard_all_dirs) {
      ++num_items; 
      strcat(work_buf, "dir");
      strcat(work_buf, "\036");
   }

   if (cmd_line_flags.disregard_all_mics) {
      ++num_items; 
      strcat(work_buf, "mic");
      strcat(work_buf, "\036");
   }

   if (cmd_line_flags.disregard_conditional_omp) {
      ++num_items; 
      strcat(work_buf, "conditional_omp");
      strcat(work_buf, "\036");
   }

   if (cmd_line_flags.disregard_all_mpp_cdirs) {
      ++num_items; 
      strcat(work_buf, "mpp");
      strcat(work_buf, "\036");
   }

   if (cmd_line_flags.disregard_all_mips) {
      ++num_items; 
      strcat(work_buf, "mipspro");
      strcat(work_buf, "\036");
   }

   if (cmd_line_flags.disregard_all_omps) {
      ++num_items; 
      strcat(work_buf, "omp");
      strcat(work_buf, "\036");
   }

   for (i = 0; i < (Tok_Dir_End - Tok_Dir_Start); i++) {

      if (disregard_directive[i]) {
         ++num_items; 
         strcat(work_buf, directive_str[i]);
         strcat(work_buf, "\036");
      }
   }

   for (i = 0; i < (Tok_Mic_End - Tok_Mic_Start); i++) {

      if (disregard_mics[i]) {
         ++num_items; 
         strcat(work_buf, dir_mic_str[i]);
         strcat(work_buf, "\036");
      }
   }

   if (fprintf(c_i_f, "%d%c", num_items, EOI) < 0) {
      Cif_Error();
   }

   if (num_items > 0) {
         
      if (fprintf(c_i_f, "%s", work_buf) < 0) {
         Cif_Error();
      }
   }


   if (fprintf(c_i_f, "%s%c%s%c%s%c%s%c%x%c%d%c",
		      (cmd_line_flags.binary_output) ? bin_file : null_string,
                         EOI,
		      (cmd_line_flags.assembly_output) ? assembly_file : 
                                                         null_string, EOI,
		      null_string, EOI,			/* inline name	      */
		      cif_name, EOI,	
		      cif_C_opts, EOI,
		      (cmd_line_flags.line_size_80) ? 80 : 72, EOI) < 0) {
      Cif_Error();
   }


   /* If no INCLUDE file paths were specified, just output a 0 count and skip */
   /* the path field.  Otherwise, count them up, output the count, then       */
   /* output the paths.							      */

   if (include_path_idx == NULL_IDX) {

      if (fprintf(c_i_f, "%d%c", 0, EOI) < 0) {
         Cif_Error();
      }
   }
   else {
 
      path_idx  = include_path_idx;
      num_paths = 0;

      while (path_idx != NULL_IDX) {
         ++num_paths;
         path_idx = FP_NEXT_FILE_IDX(path_idx);
      }

      if (fprintf(c_i_f, "%d%c", num_paths, EOI) < 0) {
         Cif_Error();
      }

      path_idx = include_path_idx;

      while (path_idx != NULL_IDX) {

         if (fprintf(c_i_f, "%s%c",
                            FP_NAME_PTR(path_idx), EOI) < 0) {
            Cif_Error();
         }

         path_idx = FP_NEXT_FILE_IDX(path_idx);
      }
   }

   /* If no module file paths were specified, just output a 0 count and skip  */
   /* the path field.  Otherwise, count them up, output the count, then       */
   /* output the paths.							      */

   if (module_path_idx == 0) {

      if (fprintf(c_i_f, "%d%c", 0, EOI) < 0) {
         Cif_Error();
      }
   }
   else {
      path_idx  = module_path_idx;
      num_paths = 0;

      while (path_idx != NULL_IDX) {
         ++num_paths;
         path_idx = FP_NEXT_FILE_IDX(path_idx);
      }

      /* Subtract 1 from num_paths because the first thing in the list is the */
      /* object file being created, not a module in a -p option.	      */

      --num_paths;

      if (fprintf(c_i_f, "%d%c", num_paths, EOI) < 0) {
         Cif_Error();
      }
      
      path_idx = FP_NEXT_FILE_IDX(module_path_idx);

      for (i = 1;  i <= num_paths;  ++i) {

         if (fprintf(c_i_f, "%s%c", FP_NAME_PTR(path_idx), EOI) < 0) {
            Cif_Error();
         }

         path_idx = FP_NEXT_FILE_IDX(path_idx);
      }
   }

   if (fprintf(c_i_f, "%d%c",
		      (cmd_line_flags.src_form == Fixed_Form) ? 0 : 1,
		         EOR) < 0) {
      Cif_Error();
   } 

   TRACE (Func_Exit, "cif_misc_compiler_opts_rec", NULL);

   return;

}  /* cif_misc_compiler_opts_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Optimization Options record [38].			      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_optimization_opts_rec(void)
{
   char		buffer[32];
   int		num_opts	= 0;
   char		opt_with_lvl[8];
   int		optz_opts;


   TRACE (Func_Entry, "cif_optimization_opts_rec", NULL);

   optz_opts = 0;

   if (opt_flags.aggress) {
      optz_opts = optz_opts | CIF_OOF_AGGRESS;
   }

# ifdef _ACCEPT_BL

   if (opt_flags.bottom_load) {
      optz_opts = optz_opts | CIF_OOF_BLOAD;
   }

# endif


# ifdef _ACCEPT_CMD_O_LOOPALIGN

   if (opt_flags.loopalign) {
      optz_opts = optz_opts | CIF_OOF_LOOPALIGN;
   }

# endif


   if (opt_flags.over_index) {
      optz_opts = optz_opts | CIF_OOF_OVERINDEX;
   }


# ifdef _ACCEPT_PATTERN

   if (opt_flags.pattern) {
      optz_opts = optz_opts | CIF_OOF_PATTERN;
   }

# endif


   if (opt_flags.recurrence) {
      optz_opts = optz_opts | CIF_OOF_RECURRENCE;
   }


# ifdef _ACCEPT_VSEARCH

   if (opt_flags.vsearch) {
      optz_opts = optz_opts | CIF_OOF_VSEARCH;
   }

# endif


# ifdef _ACCEPT_CMD_O_ZEROINC

   if (opt_flags.zeroinc) {
      optz_opts = optz_opts | CIF_OOF_ZEROINC;
   }

# endif


   if (fprintf(c_i_f, "%d%c%x%c",
                      CIF_F90_OPT_OPTS, EOI,
		      optz_opts, EOI) < 0) {
      Cif_Error();
   }

   buffer[0] = NULL_CHAR;


# ifdef _ACCEPT_INLINE

   if (opt_flags.inline_lvl > Inline_Lvl_0) {
      ++num_opts; 
      sprintf(opt_with_lvl, "%c%x%c%d",
			    EOI,
		            CIF_OOF_INLINE, EOI,
		            opt_flags.inline_lvl);
      strcat(buffer, opt_with_lvl);
   }

# endif


   ++num_opts;

   sprintf(opt_with_lvl, "%c%x%c%d",
                         EOI,
		         CIF_OOF_SCALAR, EOI,
		         opt_flags.scalar_lvl);
   strcat(buffer, opt_with_lvl);


# ifdef _ACCEPT_VECTOR

   ++num_opts;
   sprintf(opt_with_lvl, "%c%x%c%d",
			 EOI,
			 CIF_OOF_VECTOR, EOI,
			 opt_flags.vector_lvl);
   strcat(buffer, opt_with_lvl);

# endif


# ifdef _ACCEPT_TASK

   ++num_opts;
   sprintf(opt_with_lvl, "%c%x%c%d",
			 EOI,
			 CIF_OOF_TASK, EOI,
			 opt_flags.task_lvl);
   strcat(buffer, opt_with_lvl);

# endif


   if (num_opts == 0) {

      if (fprintf(c_i_f, "0%c", EOR) < 0) {
         Cif_Error();
      }
   }
   else {
   
      if (fprintf(c_i_f, "%d%s%c", num_opts, buffer, EOR) < 0) {
         Cif_Error();
      }
   }

   TRACE (Func_Exit, "cif_optimization_opts_rec", NULL);

   return;

}  /* cif_optimization_opts_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Begin Scope record [39].				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_begin_scope_rec(void)
{
   int		blk_idx;
#ifdef KEY /* Bug 10177 */
   int		cif_col_num = 0;
   int		file_line_num;
   int		glb_line_num = 0;
   int		level = 0;
#else /* KEY Bug 10177 */
   int		cif_col_num;
   int		file_line_num;
   int		glb_line_num;
   int		level;
#endif /* KEY Bug 10177 */
   int		local_blk_stk_idx;
   int		local_file_id;
#ifdef KEY /* Bug 10177 */
   int		parent_scope_id = 0;
   int		scope_type = 0;
#else /* KEY Bug 10177 */
   int		parent_scope_id;
   int		scope_type;
#endif /* KEY Bug 10177 */
   int		symbol_id;


   TRACE (Func_Entry, "cif_begin_scope_rec", NULL);

   /* Trick case:  If the program unit consists of nothing but END, the Block */
   /* Stack will already have been popped by the time we get here.	      */
  
   if (blk_stk_idx == 0  &&  BLK_TYPE(1) == Program_Blk) {
      local_blk_stk_idx = 1;   
   }
   else {
      local_blk_stk_idx = blk_stk_idx;
   }

   if (BLK_TYPE(local_blk_stk_idx) <= Interface_Body_Blk) {

      if (SCP_CIF_ID(curr_scp_idx) == 0) {
         SCP_CIF_ID(curr_scp_idx) =
            (BLK_TYPE(local_blk_stk_idx) == Program_Blk) ? 1 : NEXT_SCOPE_ID;
      }

      BLK_CIF_SCOPE_ID(local_blk_stk_idx) = SCP_CIF_ID(curr_scp_idx);
      level                               = SCP_LEVEL(curr_scp_idx);
   }  

   if (BLK_TYPE(local_blk_stk_idx) < Internal_Blk) {

      if (cif_pgm_unit_start_line == stmt_start_line) {
         glb_line_num = CURR_BLK_DEF_LINE; 
         cif_col_num  = CURR_BLK_DEF_COLUMN;
      }
      else {

         /* For the pathological case where the only significant line in the  */
         /* program contains an END statement, cif_pgm_unit_start_line is     */
         /* incremented before we get here.                                   */

         glb_line_num = (cif_pgm_unit_start_line < stmt_start_line) ?
                           cif_pgm_unit_start_line : stmt_start_line;

         cif_col_num  = 1;
      }
   }

   switch (BLK_TYPE(local_blk_stk_idx)) {

      case Blockdata_Blk:
         scope_type      = CIF_SCP_BLOCK;
         parent_scope_id = 0;
         break;

      case Module_Blk:
         scope_type 		             = CIF_SCP_MOD_SUB;
         parent_scope_id                     = 0;
         level                               = 0;

         if (AT_CIF_SYMBOL_ID(SCP_ATTR_IDX(curr_scp_idx)) == 0) {
            symbol_id                                    = NEXT_SYMBOL_ID;
            AT_CIF_SYMBOL_ID(SCP_ATTR_IDX(curr_scp_idx)) = symbol_id;
         }
         else {
            symbol_id = AT_CIF_SYMBOL_ID(SCP_ATTR_IDX(curr_scp_idx));
         }
 
         break;
      
      case Program_Blk:
         scope_type      = CIF_SCP_MAIN;
         parent_scope_id = 0;
         break;
        
      case Function_Blk:
      case Subroutine_Blk:
         scope_type      = CIF_SCP_EXTERNAL;
         parent_scope_id = 0;
         break;
        
      case Internal_Blk:
         scope_type      = CIF_SCP_INTERNAL;
         parent_scope_id = BLK_CIF_SCOPE_ID(blk_stk_idx - 1);

         if (cif_internal_proc_start_line == stmt_start_line) {
            glb_line_num = CURR_BLK_DEF_LINE; 
            cif_col_num  = CURR_BLK_DEF_COLUMN;
         }
         else {
            glb_line_num = cif_internal_proc_start_line + 1;
            cif_col_num  = 1;
         }

         break;
        
      case Module_Proc_Blk:
         scope_type      = CIF_SCP_MODULE;
         parent_scope_id = SCP_CIF_ID(SCP_PARENT_IDX(curr_scp_idx));

         if (cif_module_proc_start_line == stmt_start_line) {
            glb_line_num = CURR_BLK_DEF_LINE; 
            cif_col_num  = CURR_BLK_DEF_COLUMN;
         }
         else {
            glb_line_num = cif_module_proc_start_line + 1;
            cif_col_num  = 1;
         }

         break;
        
      case Interface_Body_Blk:
         scope_type      = CIF_SCP_INTERFACE;
         parent_scope_id = BLK_CIF_SCOPE_ID(blk_stk_idx - 1);
         glb_line_num    = BLK_DEF_LINE(local_blk_stk_idx);
         cif_col_num     = BLK_DEF_COLUMN(local_blk_stk_idx);
         file_line_num   = get_line_and_file_id(glb_line_num, &local_file_id);

         /* LRR:  Can we assume there are no invalid blocks between the       */
         /*       interface body block and the containing procedure block?    */

         level	 = 1;
         blk_idx = blk_stk_idx - 1;
         
         while (BLK_TYPE(blk_idx) > Module_Proc_Blk) {
            ++level;
            blk_idx--;
         }

         break;
        
      case Do_Blk:
      case If_Blk:
      case If_Then_Blk:
      case Select_Blk:
      case Where_Then_Blk:
      case Contains_Blk:
      case Derived_Type_Blk:

         /* If the current block type > Interface_Body_Blk it better be       */
         /* because the block is the first thing in the main program.         */

         if ((CURR_BLK == If_Then_Blk  && 
              BLK_TYPE(blk_stk_idx - 2) == Program_Blk)  ||
             (CURR_BLK != If_Then_Blk  &&
              BLK_TYPE(blk_stk_idx - 1) == Program_Blk)) {
            scope_type                          = CIF_SCP_MAIN;
            SCP_CIF_ID(curr_scp_idx)            = 1;

            local_blk_stk_idx = (CURR_BLK == If_Then_Blk) ?
                                blk_stk_idx - 2 : blk_stk_idx - 1;

            BLK_CIF_SCOPE_ID(local_blk_stk_idx) = 1;
            parent_scope_id                     = 0;
            level                               = 0;

            if (cif_pgm_unit_start_line == stmt_start_line) {
               glb_line_num = BLK_DEF_LINE(local_blk_stk_idx); 
               cif_col_num  = BLK_DEF_COLUMN(local_blk_stk_idx);
            }
            else {
               glb_line_num = cif_pgm_unit_start_line; 
               cif_col_num  = 1;
            }
         }
# ifdef _DEBUG
         else {
            PRINTMSG(stmt_start_line, 260, Internal, 0);
         }
# endif
         break;

# ifdef _DEBUG
      case If_Else_If_Blk:
      case Case_Blk:
      case Where_Else_Blk:
      case Where_Else_Mask_Blk:
         PRINTMSG(stmt_start_line, 260, Internal, 0);
# endif

      case Interface_Blk:
         if (BLK_TYPE(blk_stk_idx - 1) == Program_Blk  &&
             BLK_CIF_SCOPE_ID(blk_stk_idx - 1) == 0) {
            scope_type                        = CIF_SCP_MAIN;
            SCP_CIF_ID(curr_scp_idx)          = 1;
            BLK_CIF_SCOPE_ID(blk_stk_idx - 1) = 1;
            parent_scope_id                   = 0;
            level                             = 0;

            if (cif_pgm_unit_start_line == stmt_start_line) {
               glb_line_num = BLK_DEF_LINE(blk_stk_idx - 1); 
               cif_col_num  = BLK_DEF_COLUMN(blk_stk_idx - 1);
            }
            else {
               glb_line_num = cif_pgm_unit_start_line; 
               cif_col_num  = 1;
            }

            file_line_num = get_line_and_file_id(glb_line_num, &local_file_id);

            /* Symbol ID 2 is reserved for the name of the main program.      */

            symbol_id = 2; 

            if (BLK_NAME(blk_stk_idx - 1) == NULL_IDX) {

               if (AT_CIF_SYMBOL_ID(SCP_ATTR_IDX(curr_scp_idx)) == 0) {
                  AT_CIF_SYMBOL_ID(SCP_ATTR_IDX(curr_scp_idx)) = symbol_id;
               }
            }
            else if (AT_CIF_SYMBOL_ID(BLK_NAME(blk_stk_idx - 1)) == 0) {
               AT_CIF_SYMBOL_ID(BLK_NAME(blk_stk_idx - 1)) = symbol_id;
            }

            if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c",
                               CIF_F90_BEGIN_SCOPE, EOI,
                               BLK_CIF_SCOPE_ID(blk_stk_idx - 1), EOI,
                               symbol_id, EOI,
                               local_file_id, EOI,
                               file_line_num, EOI,
                               cif_col_num, EOI,
                               scope_type, EOI,
                               level, EOI,
                               parent_scope_id, EOR) < 0) {
               Cif_Error();
            }

         }
         
         scope_type                    = CIF_SCP_INT_BLOCK;
         local_blk_stk_idx	       = blk_stk_idx;
         BLK_CIF_SCOPE_ID(blk_stk_idx) = NEXT_SCOPE_ID;
         parent_scope_id               = BLK_CIF_SCOPE_ID(blk_stk_idx - 1);
         level 			       = SCP_LEVEL(curr_scp_idx) + 1;
         glb_line_num                  = BLK_DEF_LINE(local_blk_stk_idx);
         cif_col_num                   = BLK_DEF_COLUMN(local_blk_stk_idx);
         break;
        
      default:
         PRINTMSG(stmt_start_line, 179, Internal, 0, "cif_begin_scope_rec");
   }
  
   if (BLK_NAME(local_blk_stk_idx) == NULL_IDX) {

      if (BLK_TYPE(local_blk_stk_idx) == Program_Blk  ||
          BLK_TYPE(local_blk_stk_idx) == Blockdata_Blk) {
   
         if (AT_CIF_SYMBOL_ID(SCP_ATTR_IDX(curr_scp_idx)) == 0) {
            symbol_id                                    = NEXT_SYMBOL_ID;
            AT_CIF_SYMBOL_ID(SCP_ATTR_IDX(curr_scp_idx)) = symbol_id;
         }
         else {
            symbol_id = AT_CIF_SYMBOL_ID(SCP_ATTR_IDX(curr_scp_idx));
         }
      }
      else { 
         symbol_id = 0;
      }
   }
   else {
      if (AT_CIF_SYMBOL_ID(BLK_NAME(local_blk_stk_idx)) == 0) {
         symbol_id                                     = NEXT_SYMBOL_ID;
         AT_CIF_SYMBOL_ID(BLK_NAME(local_blk_stk_idx)) = symbol_id;
      }
      else {
         symbol_id = AT_CIF_SYMBOL_ID(BLK_NAME(local_blk_stk_idx));
      }

   }

   file_line_num = get_line_and_file_id(glb_line_num, &local_file_id);

   Cif_F90_Begin_Scope_Rec(c_i_f,
	                   BLK_CIF_SCOPE_ID(local_blk_stk_idx),
                           symbol_id, 
		           local_file_id,
		           file_line_num,
		           cif_col_num, 
		           scope_type, 
		           level, 
                           parent_scope_id);

   TRACE (Func_Exit, "cif_begin_scope_rec", NULL);

   return;

}  /* cif_begin_scope_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a End Scope record [40].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_end_scope_rec(void)
{
   int          file_line_num;
   int          local_file_id;


   TRACE (Func_Entry, "cif_end_scope_rec", NULL);

   /* cif_end_scope_rec assumes that it is called AFTER the scope ending stmt */
   /* has been parsed but BEFORE the EOS has been eaten.  Thus, the LA_CH     */
   /* should be for the EOS.						      */

   file_line_num = get_line_and_file_id(LA_CH_LINE, &local_file_id);

   if (cif_pgm_unit_error_recovery) {
      BLK_CIF_SCOPE_ID(blk_stk_idx) = 1;
   }
   else {

      /* Pathological case:  the program unit is nothing but an END stmt.     */

      if (CURR_BLK <= Interface_Body_Blk) {

         if (SCP_CIF_ID(curr_scp_idx) == 0) {
            SCP_CIF_ID(curr_scp_idx) =
               (CURR_BLK == Program_Blk) ? 1 : NEXT_SCOPE_ID;
         }

         BLK_CIF_SCOPE_ID(blk_stk_idx) = SCP_CIF_ID(curr_scp_idx);
      }
   }

   Cif_F90_End_Scope_Rec(c_i_f,
    		         BLK_CIF_SCOPE_ID(blk_stk_idx),
                         local_file_id, 
                         file_line_num,
                         LA_CH_COLUMN - 1,
                         CURR_BLK_ERR);

   if (CURR_BLK == Internal_Blk) {
      cif_internal_proc_start_line = LA_CH_LINE;
   }
   else if (CURR_BLK == Module_Proc_Blk) {
      cif_module_proc_start_line = LA_CH_LINE;
   }
   
   TRACE (Func_Exit, "cif_end_scope_rec", NULL);

   return;

}  /* cif_end_scope_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Scope Info record [41].				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_scope_info_rec(void)
{
   int		al_idx;
   int		attributes;
   char		buffer[160];
   int		str_len;
   char		string[10];


   TRACE (Func_Entry, "cif_scope_info_rec", NULL);

   attributes = (SCP_IMPL_NONE(curr_scp_idx)) ? CIF_SCP_IMPL_NONE : 0;

   if (SCP_DOES_IO(curr_scp_idx)) {
      attributes = attributes | CIF_SCP_DOES_IO;
   }

   if (SCP_HAS_CALLS(curr_scp_idx)) {
      attributes = attributes | CIF_SCP_HAS_CALLS;
   }

   if (SCP_ALT_ENTRY_CNT(curr_scp_idx) == 0) {
      buffer[0] = EOR;
      buffer[1] = NULL_CHAR;
   }
   else {
      buffer[0] = NULL_CHAR;
      al_idx = SCP_ENTRY_IDX(curr_scp_idx);

      do {
         sprintf(string, "%c%d",
			 EOI, AT_CIF_SYMBOL_ID(AL_ATTR_IDX(al_idx))); 
         strcat(buffer, string);
         al_idx  = AL_NEXT_IDX(al_idx);
      }
      while (al_idx != NULL_IDX);

      str_len             = strlen(buffer);
      buffer[str_len]     = EOR;
      buffer[str_len + 1] = NULL_CHAR;
   }

   if (fprintf(c_i_f, "%d%c%d%c%x%c%d%s",
                      CIF_F90_SCOPE_INFO, EOI,
                      SCP_CIF_ID(curr_scp_idx), EOI,
                      attributes, EOI,
                      SCP_ALT_ENTRY_CNT(curr_scp_idx),
                      buffer) < 0) {
      Cif_Error();
   }

   TRACE (Func_Exit, "cif_scope_info_rec", NULL);

   return;

}  /* cif_scope_info_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Use Module record [42].				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void cif_use_module_rec(int	attr_idx,
			int	mf_tbl_idx,
			boolean	send_attr)
{
   int	 cif_file_id;
   int	 flag;


   TRACE (Func_Entry, "cif_use_module_rec", NULL);

   if (mf_tbl_idx == NULL_IDX) {

      /* This is an indirect reference to a module.  If there is more than    */
      /* one reference to the same module, we call cif_send_attr for all the  */
      /* duplicate entries.  We do this because we only keep one attr around  */
      /* for each module name and CIF wants to know where the module came from*/

      if (send_attr) {
         cif_send_attr(attr_idx, NULL_IDX);
      }
      else if (AT_CIF_SYMBOL_ID(attr_idx) == 0) {
         AT_CIF_SYMBOL_ID(attr_idx) = NEXT_SYMBOL_ID;
      }

      cif_file_id = cif_file_name_rec(ATP_MOD_PATH_NAME_PTR(attr_idx),
                                      (char *) NULL);
      flag	  = CIF_USE_MODULE_INDIRECT;
   }
   else {

      if (AT_CIF_SYMBOL_ID(attr_idx) == 0) {
         AT_CIF_SYMBOL_ID(attr_idx) = NEXT_SYMBOL_ID;
      }

      if (FP_CIF_ID(mf_tbl_idx) == 0) {
         FP_CIF_ID(mf_tbl_idx) = cif_file_name_rec(FP_NAME_PTR(mf_tbl_idx),
                                                   (char *) NULL);
      }

      cif_file_id = FP_CIF_ID(mf_tbl_idx);
      flag	  = CIF_USE_MODULE_DIRECT;
   }

   Cif_F90_Use_Module_Rec(c_i_f,
                          AT_CIF_SYMBOL_ID(attr_idx),
                          cif_file_id, 
                          flag);

   TRACE (Func_Exit, "cif_use_module_rec", NULL);

   return;

}  /* cif_use_module_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Rename record [43].					      *|
|*      Note:  We've gone back and forth a couple of times on whether this    *|
|*             record should also record names in ONLY clauses.  The current  *|
|*             is that it does NOT.  But we're leaving the external name      *|
|*             as "cif_rename_only_rec" in case we change our minds again.    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

int	cif_rename_rec(int	ro_idx,
		       int	cif_symbol_id,
		       int	attr_idx,
		       int	module_attr_idx)
{

   TRACE (Func_Entry, "cif_rename_rec", NULL);

   /* This routine issues a Rename record and generates a symbol id for the   */
   /* name in the module if one is needed.  If cif_symbol_id is zero, a new   */
   /* symbol id is generated and returned.                                    */

   if (cif_symbol_id == 0) {
      cif_symbol_id = NEXT_SYMBOL_ID;
   }

   if (AT_CIF_SYMBOL_ID(AT_MODULE_IDX(attr_idx)) == 0) {
      AT_CIF_SYMBOL_ID(AT_MODULE_IDX(attr_idx)) = NEXT_SYMBOL_ID;
   }

   if (AT_CIF_SYMBOL_ID(attr_idx) == 0) {
      AT_CIF_SYMBOL_ID(attr_idx) = NEXT_SYMBOL_ID;
   }

   /* AT_ORIG_NAME_PTR and AT_MODULE_IDX will always be set.  If the object   */
   /* originated in the module being read in, AT_MODULE_IDX is set to the     */
   /* module being read in.  						      */

   Cif_F90_Rename_Rec(c_i_f,
                      SCP_CIF_ID(curr_scp_idx),
                      RO_NAME_PTR(ro_idx), 
                      cif_symbol_id, 
                      AT_CIF_SYMBOL_ID(module_attr_idx),
                      AT_ORIG_NAME_PTR(attr_idx), 
                      AT_CIF_SYMBOL_ID(AT_MODULE_IDX(attr_idx)),
                      (long) AT_CIF_SYMBOL_ID(attr_idx));

   TRACE (Func_Exit, "cif_rename_rec", NULL);

   return(cif_symbol_id);

}  /* cif_rename_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is called from main.c when it discovers that no Unit   *|
|*      record has been generated and we're at the end of the source file.    *|
|*      This can happen in cases like a free source form program being        *|
|*      compiled in fixed source form mode.  Fake up enough records in the    *|
|*      CIF (if one is being produced) and the buffered message file so that  *|
|*      the files are usable.						      *|
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

void cif_fake_a_unit()
{
   int	file_line_num;
   int	glb_line_num;
   int	local_file_id;
   int	scope_id;
   int 	symbol_id;


   TRACE (Func_Entry, "cif_fake_a_unit", NULL);

   stmt_start_line = 1;

   cif_unit_rec();

   cif_symbol_or_scope_id = 3;                /* First 2 values are reserved. */
					      /* See cif_prog_unit_init.      */
   symbol_id              = NEXT_SYMBOL_ID;
   scope_id		  = NEXT_SCOPE_ID;

   /* The following pieces of code were lifted from cif_begin_scope_rec.      */
   /* That procedure can't be used directly because it relies on the Block    */
   /* Stack (which at this point contains junk).			      */

   glb_line_num  = cif_pgm_unit_start_line; 
   file_line_num = get_line_and_file_id(glb_line_num, &local_file_id);

   if (cif_flags & BASIC_RECS) {

      Cif_F90_Begin_Scope_Rec(c_i_f,
                              scope_id,
                              symbol_id,
                              local_file_id,
                              file_line_num,
                              1, 
                              CIF_SCP_MAIN,
                              0, 
                              0);

      /* Now dummy up an Entry Point record for $MAIN.			      */

      Cif_F90_Entry_Rec(c_i_f,
                        UNNAMED_PROGRAM_NAME,
                        symbol_id,
                        scope_id,
                        0, 
                        0,
                        0,
                        0,
                        0,
                        0,
                        NULL);

      /* The following pieces of code were lifted from cif_end_scope_rec.     */
      /* That procedure can't be used directly because it relies on the Block */
      /* Stack (which at this point contains junk).			      */

      file_line_num = get_line_and_file_id(curr_glb_line - 1, &local_file_id);

      Cif_F90_End_Scope_Rec(c_i_f,
                            scope_id,
                            local_file_id,
                            file_line_num,
                            stmt_start_col,
                            1);
   }

   /* Now dummy up an End Unit record.					      */

   stmt_start_line = (curr_glb_line > 1) ? curr_glb_line - 1 : 1;
   stmt_start_col  = 1;
   cif_end_unit_rec(UNNAMED_PROGRAM_NAME);

   TRACE (Func_Exit, "cif_fake_a_unit", NULL);

   return;

}  /* cif_fake_a_unit */ 


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Print a message and give up if an error is detected when writing to   *|
|*      the CIF. 							      *|
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

void Cif_Error()
{

   TRACE (Func_Entry, "Cif_Error", NULL);

   PRINTMSG((curr_stmt_sh_idx > 0) ? SH_GLB_LINE(curr_stmt_sh_idx) : 1,
            383, Error, 0);

   exit_compiler(RC_USER_ERROR);

   TRACE (Func_Exit, "Cif_Error", NULL);

} /*  Cif_Error  */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This routine converts a global line number to a line number within a  *|
|*      file and also gets the file's CIF file id.                            *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      search_line	Global line number to be translated.		      *|
|*                                                                            *|
|* Global Input                                                               *|
|*      glb_line tbl	An entry is made to this table whenever the compiler  *|
|*                      compiler starts inputting source from a file or       *|
|*                      returns to inputting source from a file (after an     *|
|*                      INCLUDE, for example).				      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      file_id         The CIF file id for the translated line.              *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      act_line        Line number within the file.		              *|
|*                                                                            *|
\******************************************************************************/

static int get_line_and_file_id (int	 search_line,
                                 int    *file_id)
{
   int  idx;                    /* Index to global line table for line.       */
   int	actual_line;		/* The line number within the file.           */


   TRACE (Func_Entry, "get_line_and_file_id", NULL);

   GLOBAL_LINE_TO_FILE_LINE(search_line, idx, actual_line);

   *file_id = GL_CIF_FILE_ID(idx);

   TRACE (Func_Exit, "get_line_and_file_id", NULL);

   return(actual_line);

}  /* get_line_and_file_id */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine recursively prints out structure component symbol ids    *|
|*      in the proper left to right order.  If the global variable            *|
|*      cif_number_of_struct_ids is set to zero or greater, it will only      *|
|*      make a count of these components.  It is therefore meant to be called *|
|*      twice for the same tree.  Once for the count, (which is output first) *|
|*      and then for the output of the symbol ids.                            *|
|*									      *|
|* Input parameters:							      *|
|*	opnd - root of reference tree.                                        *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	FALSE if cif error                                                    *|
|*									      *|
\******************************************************************************/

static boolean output_struct_ids(opnd_type	*opnd)

{
   opnd_type    loc_opnd;
   boolean	ok = TRUE;

   TRACE (Func_Entry, "output_struct_ids", NULL);

   if (OPND_FLD((*opnd)) == IR_Tbl_Idx) {

      if (IR_OPR(OPND_IDX((*opnd))) == Struct_Opr) {
         COPY_OPND(loc_opnd, IR_OPND_L(OPND_IDX((*opnd))));
         ok = output_struct_ids(&loc_opnd);

         if (ok) {
            COPY_OPND(loc_opnd, IR_OPND_R(OPND_IDX((*opnd))));
            ok = output_struct_ids(&loc_opnd);
         }
      }
      else {
         COPY_OPND(loc_opnd, IR_OPND_L(OPND_IDX((*opnd))));
         ok = output_struct_ids(&loc_opnd);
      }
   }
   else if (OPND_FLD((*opnd)) == AT_Tbl_Idx) {

      if (skip_struct_base                           &&
          ATD_CLASS(OPND_IDX((*opnd))) != Struct_Component) {

         /* intentionally blank */
      }
      else if (cif_number_of_struct_ids >= 0) {
         cif_number_of_struct_ids++;
      }
      else {
         if (AT_CIF_SYMBOL_ID(OPND_IDX((*opnd))) == 0) {
            AT_CIF_SYMBOL_ID(OPND_IDX((*opnd))) = NEXT_SYMBOL_ID;
         }

         ok = fprintf(c_i_f, "%c%d", EOI, 
                                     AT_CIF_SYMBOL_ID(OPND_IDX((*opnd)))) >= 0;
      }
   }

   TRACE (Func_Exit, "output_struct_ids", NULL);

   return(ok);

}  /* output_struct_ids */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This routine outputs an Object record with just enough information    *|
|*      in it so that the visual tools can use it.  Such a record should      *|
|*      only be output if the object it is trying to describe is in error or  *|
|*      something the object depends upon (such as a derived type definition) *|
|*      is in error.							      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      attr_idx - Attr index of the object that the record is to record.     *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

static void output_minimal_object_rec(int	attr_idx)

{
   char		char_len[1];


   TRACE (Func_Entry, "output_minimal_object_rec", NULL);

   char_len[0] = NULL_CHAR;

   if (AT_CIF_SYMBOL_ID(attr_idx) == 0) {
      AT_CIF_SYMBOL_ID(attr_idx) = NEXT_SYMBOL_ID;
   }  

   if (fprintf(c_i_f,
         "%d%c%s%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%x%c%d%c%s%c%d%c%d%c%d%c%d%c%d%c",
                      CIF_F90_OBJECT, EOI,
                      AT_OBJ_NAME_PTR(attr_idx), EOI,
                      AT_CIF_SYMBOL_ID(attr_idx), EOI,
                      SCP_CIF_ID(curr_scp_idx), EOI,
                      0, EOI,
                      0, EOI,
                      0, EOI,
                      0, EOI,
                      -1, EOI,
                      0, EOI,
                      0, EOI,
                      char_len, EOI,
                      0, EOI,                           /* Num dims           */
                      0, EOI,				/* Array type         */
                      0, EOI,				/* Distribution       */
                      0, EOI,				/* Geometry id        */
                      0, EOR) < 0) {			/* CRI pointer id     */
      Cif_Error();
   }

   TRACE (Func_Exit, "output_minimal_object_rec", NULL);

   return;

}  /* output_minimal_object_rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This routine flushes records associated with INCLUDE lines at the end *|
|*      of a program unit.  It is ugly, it is nasty, but unfortunately it (or *|
|*      something like it) is required because INCLUDE lines are always       *|
|*      processed in "lookahead" mode and all the records are immediately     *|
|*      produced when the INCLUDE line is processed.  Consider:               *|
|*									      *|
|*                ...							      *|
|*                END    ! a program unit				      *|
|*                SUBROUTINE sub					      *|
|*                INCLUDE '...'						      *|
|*									      *|
|*      The source input routines buffer up the current line and its following*|
|*      line.  When the current line is completed, its following line is made *|
|*      the current line and a new "following" line is obtained.  In the above*|
|*      example, when the END line is the current line, the SUBROUTINE line is*|
|*      the "following" line.  When the EOS at the end of the END line is     *|
|*      eaten, the source input routines move the SUBROUTINE line to the      *|
|*      "current line" buffer and read the INCLUDE line into the "following   *|
|*      line" buffer.  BUT the source input routines realize it's an INCLUDE  *|
|*      line, so they get the first line of the INCLUDE file and make IT the  *|
|*      "following line".  As a part of this all the CIF records associated   *|
|*      with the INCLUDE line are produced.  But in the above example, they   *|
|*      would fall into the first subprogram's record group because the End   *|
|*      Unit record for that subprogram has not yet been produced.  Therefore,*|
|*      the records associated with the INCLUDE line must be "buffered" in the*|
|*      temporary CIF until the next unit begins (this is the same thing that *|
|*      happens at the beginning of the first program unit in a file).        *|
|*  									      *|
|*      However, if we have a case like					      *|
|*									      *|
|*                ...							      *|
|*                INCLUDE '...'						      *|
|*                ...							      *|
|*                END							      *|
|*									      *|
|*      the CIF records associated with the INCLUDE DO belong to the current  *|
|*      unit.  Since these are records MUST be buffered (because they are     *|
|*      produced before they actually should be), we need to know when to     *|
|*      UNbuffer them.  In this second case, they need to be unbuffered while *|
|*      the current unit is still active.  At the END statement is good       *|
|*      enough.								      *|
|*									      *|
|*      Now, suppose we have						      *|
|*									      *|
|*                ...							      *|
|*                INCLUDE '...'						      *|
|*                ...							      *|
|*                END							      *|
|*                SUBROUTINE sub					      *|
|*                INCLUDE '...'						      *|
|*									      *|
|*      At the END statement, we want to unbuffer any INCLUDE records that    *|
|*      are associated with the current unit but NOT those associated with a  *|
|*      following unit.  And that's the raison d'etre of this ugly little     *|
|*      procedure.  When called from END stmt processing, it unbuffers the    *|
|*      right sets of records as follows:     				      *|
|*									      *|
|*        - Write an EOF to the temp CIF and rewind it.			      *|
|*        - Read records from the temp CIF file and write them to the actual  *|
|*          CIF until the line number is greater than the line number of the  *|
|*          END statement.						      *|
|*        - Write the remainder of the records (they belong to the next unit) *|
|*          to another (secondary) temp file.  When done, write an EOF to     *|
|*          this file and rewind it.					      *|
|*        - Rewind the temp CIF.					      *|
|*        - Copy each record in the secondary temp file to the temp CIF.      *|
|*        - Get rid of the secondary temp file.				      *|
|*									      *|
|* Input parameters:                                                          *|
|*      NONE								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

static void cif_flush_include_recs(void)

{

# define FILE_ID_LIST_SIZE	1000

   FILE		*aux_file;
   char		 aux_file_name[MAX_FILE_NAME_SIZE];
   char		 buf[9];
   int		 end_stmt_line;
   int		 file_id;
   int		 file_id_list[FILE_ID_LIST_SIZE];
   int		 file_id_list_idx	= 0;
   boolean	 first_record		= TRUE;
   char		 generic_rec[512];                     /* Arbitrary size.     */
   int		 gr_idx;
   boolean	 have_file_name_rec	= FALSE;
   boolean	 have_rec		= FALSE;
   char		 holding_pen[512];
   int		 i;
   int		 line_num;
   int		 rec_type;
   char		 rec_type_str[3];


   TRACE (Func_Entry, "cif_flush_include_recs", NULL);

   fprintf(cif_tmp_file, "%d\n", EOF);
   rewind(cif_tmp_file);

   end_stmt_line   = global_to_local_line_number(stmt_start_line);
   file_id_list[0] = GL_CIF_FILE_ID(1);

   while (fgets(generic_rec, 512, cif_tmp_file) != NULL  &&
          atoi(generic_rec) != EOF) {

# ifdef _DEBUG
      if (file_id_list_idx >= FILE_ID_LIST_SIZE - 1) {
         PRINTMSG(stmt_start_line, 1406, Internal, 1);
      }
# endif 

      rec_type_str[0] = generic_rec[0];

      if (generic_rec[1] == EOI) {
         rec_type_str[1] = NULL_CHAR;
      }
      else {
         rec_type_str[1] = generic_rec[1];
         rec_type_str[2] = NULL_CHAR;
      }

      rec_type = atoi(rec_type_str);

      switch (rec_type) {

         case CIF_FILE:
            strcpy(holding_pen, generic_rec);
            have_file_name_rec = TRUE;
            break;


         case CIF_INCLUDE:

            /* Start at the third character of the Include record and get the */
            /* file id.  Make sure the record belongs to either the current   */
            /* source file or any INCLUDE file that was opened while the      */
            /* current source file was being processed (this happens due to   */
            /* the lookahead in src_input.c).                  		      */

            buf[0] = generic_rec[2];
            gr_idx = 3;
            i      = 1;

            while (generic_rec[gr_idx] != EOI) {
               buf[i++] = generic_rec[gr_idx++];
            }

            buf[i]  = NULL_CHAR;
            file_id = atoi(buf);

            for (i = file_id_list_idx;  i >= 0;  i--) {
          
               if (file_id == file_id_list[i]) {
                  break;
               }
            }

            if (i < 0) {

               /* Sanity check.  The first non-File Name record had better be */
               /* a Stmt Type record (if -Ca was specified) or an Include     */
               /* record (if -Ca was not specified).  The file id might not   */
               /* be the same as for the program unit being compiled due to   */
               /* nesting of INCLUDE files and the way the source lines       */
               /* happen to lay out (all kinds of weirdness happens with      */
               /* source line lookahead).  If it's file id is *not* the same  */
               /* as the program unit's file id, add it to the list so that   */
               /* it will act as another "parent" file id.                    */

               if (first_record) {
                  file_id_list[++file_id_list_idx] = file_id;
                  first_record                     = FALSE;
               }
               else {
                  have_rec = TRUE;
                  goto RECORDS_FOR_NEXT_UNIT;
               }
            }
            else if (i == 0) {

               /* We now know that the Include record refers to a line that is*/
               /* contained in the source file being compiled.  Now need to   */
               /* see if it belongs to the current program unit.  Get the line*/
               /* number and compare to the line number of the last line of   */
               /* the current program unit.                                   */
               /* Note:  When i > 0, it means the Include record belongs to   */
               /* an INCLUDE file.  Checking the line number against the      */
               /* current program unit is meaningless.  Just fall into the    */
               /* code that moves the record to the actual CIF.	              */

               ++gr_idx;
               buf[0] = generic_rec[gr_idx++];
               i      = 1;

               while (generic_rec[gr_idx] != EOI) {
                  buf[i++] = generic_rec[gr_idx++];
               }

               buf[i]   = NULL_CHAR;
               line_num = atoi(buf);

               if (line_num > end_stmt_line) {
                  have_rec = TRUE;                  
                  goto RECORDS_FOR_NEXT_UNIT;
               }
            }

            if (have_file_name_rec) {
               fputs(holding_pen, cif_actual_file);
               have_file_name_rec = FALSE;
            }

            fputs(generic_rec, cif_actual_file);


            /* The Include record had better be followed by a Source          */
            /* Position record.  The Source Position record is also written   */
            /* to the actual CIF now.					      */

            if (fgets(generic_rec, 512, cif_tmp_file) != NULL  &&
                atoi(generic_rec) != EOF) {
               rec_type_str[0] = generic_rec[0];

               if (generic_rec[1] != EOI) {
                  rec_type_str[1] = generic_rec[1];
                  rec_type_str[2] = NULL_CHAR;
               }
               else {
                  PRINTMSG(end_stmt_line, 1148, Internal, 0);
               }

               rec_type = atoi(rec_type_str);

               if (rec_type == CIF_SRC_POS) {
                  fputs(generic_rec, cif_actual_file);
               }
               else {
                  PRINTMSG(end_stmt_line, 1148, Internal, 0);
               }


               /* Now get the file id for the INCLUDE file being opened.  If  */
               /* it's already in the list, pop down to it.  Otherwise, add   */
               /* it to the list.					      */

               gr_idx = 3;

               while (generic_rec[gr_idx++] != EOI) {
               }

               buf[0] = generic_rec[gr_idx++];
               i      = 1;

               while (generic_rec[gr_idx] != EOI) {
                  buf[i++] = generic_rec[gr_idx++];
               }

               buf[i]  = NULL_CHAR;
               file_id = atoi(buf);

               for (i = file_id_list_idx;  i > 0;  --i) {  
                 
                  if (file_id == file_id_list[i]) {
                     break;
                  }
               }

               if (i > 0) {
                  file_id_list_idx = i;
               }
               else {
                  file_id_list[++file_id_list_idx] = file_id;
               }
            }
            else {
               PRINTMSG(end_stmt_line, 1148, Internal, 0);
            }
 
            break;


         case CIF_MESSAGE:

            /* Start at the fourth (gr_idx = 3) character of the Message      */
            /* record (which is the first character of the message type).     */
            /* Skip it.  Skip the next field (the message number) as well.    */
            /* Get the file id and make sure the record belongs to either     */
            /* the current source file or any INCLUDE file that was opened    */
            /* while the current source file was being processed (this        */
            /* happens due to the lookahead in src_input.c).		      */

            gr_idx = 3;

            while (generic_rec[gr_idx++] != EOI) {
            }

            ++gr_idx;

            while (generic_rec[gr_idx++] != EOI) {
            }

            buf[0] = generic_rec[gr_idx++];
            i      = 1;

            while (generic_rec[gr_idx] != EOI) {
               buf[i++] = generic_rec[gr_idx++];
            }

            buf[i]  = NULL_CHAR;
            file_id = atoi(buf);

            for (i = file_id_list_idx;  i >= 0;  i--) {
           
               if (file_id == file_id_list[i]) {
                  break;
               }
            }

            if (i < 0) {
               have_rec = TRUE;
               goto RECORDS_FOR_NEXT_UNIT;
            }
            else if (i > 0) {

               /* The Message record belongs to an INCLUDE file.  Checking    */
               /* the line number against the current program unit is         */
               /* meaningless.  Just move the record to the actual CIF.       */

               fputs(generic_rec, cif_actual_file);
               break;
            }


            /* We now know that the Message record refers to a line that is   */
            /* contained in the source file being compiled.  Now need to see  */
            /* if it belongs to the current program unit.  Get the line number*/
            /* and compare to the line number of the last line of the current */
            /* program unit.						      */
           
            ++gr_idx;
            buf[0] = generic_rec[gr_idx++];
            i      = 1;

            while (generic_rec[gr_idx] != EOI) {
               buf[i++] = generic_rec[gr_idx++];
            }

            buf[i]   = NULL_CHAR;
            line_num = atoi(buf);

            if (line_num <= end_stmt_line) {
               fputs(generic_rec, cif_actual_file);
            }
            else {
               have_rec = TRUE;                  
               goto RECORDS_FOR_NEXT_UNIT;
            }
 
            break;


         case CIF_STMT_TYPE:

            /* Start at the fourth (gr_idx = 3) character of the Stmt Type    */
            /* record (which is the first character of the stmt type).  Skip  */
            /* it.  Get the file id and make sure the record belongs to       */
            /* either the current source file or any INCLUDE file that was    */
            /* opened while the current source file was being processed       */
            /* (this happens due to the lookahead in src_input.c).	      */

            gr_idx = 3;

            while (generic_rec[gr_idx++] != EOI) {
            }

            buf[0] = generic_rec[gr_idx++];
            i      = 1;

            while (generic_rec[gr_idx] != EOI) {
               buf[i++] = generic_rec[gr_idx++];
            }

            buf[i]  = NULL_CHAR;
            file_id = atoi(buf);

            for (i = file_id_list_idx;  i >= 0;  i--) {
           
               if (file_id == file_id_list[i]) {
                  break;
               }
            }

            if (i < 0) {

               /* Sanity check.  The first non-File Name record had better be */
               /* a Stmt Type record (if -Ca was specified) or an Include     */
               /* record (if -Ca was not specified).  The file id might not   */
               /* be the same as for the program unit being compiled due to   */
               /* nesting of INCLUDE files and the way the source lines       */
               /* happen to lay out (all kinds of weirdness happens with      */
               /* source line lookahead).  If it's file id is *not* the same  */
               /* as the program unit's file id, add it to the list so that   */
               /* it will act as another "parent" file id.                    */

               if (first_record) {
                  file_id_list[++file_id_list_idx] = file_id;
                  first_record                     = FALSE;
               }
               else {
                  have_rec = TRUE;
                  goto RECORDS_FOR_NEXT_UNIT;
               }
            }
            else if (i > 0) {

               /* The Stmt Type record belongs to an INCLUDE file.  Checking  */
               /* the line number against the current program unit is         */
               /* meaningless.  Just move the record to the actual CIF.       */

               fputs(generic_rec, cif_actual_file);
               break;
            }


            /* We now know that the Stmt Type record refers to a line that is */
            /* contained in the source file being compiled.  Now need to see  */
            /* if it belongs to the current program unit.  Get the line number*/
            /* and compare to the line number of the last line of the current */
            /* program unit.						      */
           
            ++gr_idx;
            buf[0] = generic_rec[gr_idx++];
            i      = 1;

            while (generic_rec[gr_idx] != EOI) {
               buf[i++] = generic_rec[gr_idx++];
            }

            buf[i]   = NULL_CHAR;
            line_num = atoi(buf);

            if (line_num < end_stmt_line) {
               fputs(generic_rec, cif_actual_file);
            }
            else {
               have_rec = TRUE;                  
               goto RECORDS_FOR_NEXT_UNIT;
            }
 
            break;


         default:
            PRINTMSG(end_stmt_line, 179, Internal, 0, "cif_flush_include_recs");
      }
   }

RECORDS_FOR_NEXT_UNIT:

   if (have_rec  ||  have_file_name_rec) {

      if (! get_temp_file("w+", &aux_file, aux_file_name)) {
         PRINTMSG(stmt_start_line, 382, Log_Error, 0, "<aux CIF>");
         perror("Reason");
         goto EXIT;
      }

      if (have_file_name_rec) {
         fputs(holding_pen, aux_file);
      }

      if (have_rec) {
         fputs(generic_rec, aux_file);
      }

      while (fgets(generic_rec, 512, cif_tmp_file) != NULL  &&
             atoi(generic_rec) != EOF) {
         fputs(generic_rec, aux_file);
      }

      fprintf(aux_file, "%d\n", EOF);
      rewind(aux_file);
      rewind(cif_tmp_file);

      while (fgets(generic_rec, 512, aux_file) != NULL  &&
             atoi(generic_rec) != EOF) {
         fputs(generic_rec, cif_tmp_file);
      }

      fclose(aux_file);
      remove(aux_file_name);
   }
   else {

      /* If there were no records in the CIF temp file or no records for the  */
      /* next program unit, rewind it so it's ready for the next program unit.*/

      rewind(cif_tmp_file);
   }

EXIT:

   TRACE (Func_Entry, "cif_flush_include_recs", NULL);

   return;

}  /* cif_flush_include_recs */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Called by main.c to close the CIF file.  This procedure is used       *|
|*      rather than just putting the code in main.c in order to isolate       *|
|*      knowledge of the CIF file (no code other than code in this file need  *|
|*      know about it).                                                       *|
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

void close_cif()
{

   TRACE (Func_Entry, "close_cif", NULL);

   fflush(c_i_f);
   if (c_i_f == cif_actual_file) {
      /* prevent closing the same file twice. Linux does not handle it */
      cif_actual_file = NULL;
   }
   fclose(c_i_f);
   fclose(cif_tmp_file);
   remove(cif_tmp_file_name);

   TRACE (Func_Exit, "close_cif", NULL);

} /*  close_cif  */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is used to translate from our enum that represents a   *|
|*      specific data type to the define constant value that libcif uses.     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      data_type:  The linear data type of the object or the CIF derived     *|
|*                  type id.  Note:  It's probably not very elegant to not    *|
|*                  linear_type_type for data_type but it really can't        *|
|*                  conveniently be used because sometimes it's the CIF       *|
|*                  derived type id that's being passed in.  See, in          *|
|*                  particular, cif_call_site_rec and you will see why the    *|
|*                  linear type or Attr index can't always be passed to this  *|
|*                  routine (try the example   CALL sub(SQRT(x))  ).          *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      The integer value that libcif uses to represent the data type.        *|
|*                                                                            *|
\******************************************************************************/

static int cif_data_type(int	data_type)
{
#ifdef KEY /* Bug 10177 */
   int 			cif_value = 0;
#else /* KEY Bug 10177 */
   int 			cif_value;
#endif /* KEY Bug 10177 */

  
   TRACE (Func_Entry, "cif_data_type", NULL);

   if (data_type > 100) {
      TRACE (Func_Exit, "cif_data_type", NULL);
      return(data_type); 
   }


   switch (data_type) {

      case Err_Res:
         cif_value = CIF_F90_DT_UNKNOWN;
         break;

      case Short_Char_Const:
         cif_value = CIF_F90_DT_CHARACTER_1;
         break;

      case Short_Typeless_Const:
      case Typeless_4:
      case Typeless_8:
      case Long_Typeless:

         /* Need a new libcif define constant for this case.                  */

         cif_value = CIF_F90_DT_TYPELESS;
         break;
 
      case Integer_1:
         cif_value = CIF_F90_DT_INTEGER_1;
         break;

      case Integer_2:
         cif_value = CIF_F90_DT_INTEGER_2;
         break;

      case Integer_4:
         cif_value = CIF_F90_DT_INTEGER_4;
         break;

      case Integer_8:
         cif_value = CIF_F90_DT_INTEGER_8;
         break;

      case Real_4:
         cif_value = CIF_F90_DT_REAL_4;
         break;

      case Real_8:
         cif_value = CIF_F90_DT_REAL_8;
         break;

      case Real_16:
         cif_value = CIF_F90_DT_REAL_16;
         break;

      case Complex_4:
         cif_value = CIF_F90_DT_COMPLEX_4;
         break;

      case Complex_8:
         cif_value = CIF_F90_DT_COMPLEX_8;
         break;

      case Complex_16:
         cif_value = CIF_F90_DT_COMPLEX_16;
         break;

      case CRI_Ptr_8:
         cif_value = CIF_F90_DT_FPTR;
         break;

      case Logical_1:
         cif_value = CIF_F90_DT_LOGICAL_1;
         break;

      case Logical_2:
         cif_value = CIF_F90_DT_LOGICAL_2;
         break;

      case Logical_4:
         cif_value = CIF_F90_DT_LOGICAL_4;
         break;

      case Logical_8:
         cif_value = CIF_F90_DT_LOGICAL_8;
         break;

      case Character_1:
         cif_value = CIF_F90_DT_CHARACTER_1;
         break;

      case Character_2:
         cif_value = CIF_F90_DT_CHARACTER_2;
         break;

      case Character_4:
         cif_value = CIF_F90_DT_CHARACTER_4;
         break;

      case CRI_Ch_Ptr_8:
         cif_value = CIF_F90_DT_FCPTR;
         break;

      case Structure_Type:

         /* Taken care of at the top of this routine.			      */

         PRINTMSG(stmt_start_line, 179, Internal, 0,
                  "cif_data_type (Structure_Type)");
         break;

      case CRI_Parcel_Ptr_8:

         /* Should never get here because there should be no user item that   */
         /* could have this type.  Used for passing a procedure as an arg.    */

         PRINTMSG(stmt_start_line, 179, Internal, 0,
                  "cif_data_type (parcel ptr)");
   }

   TRACE (Func_Exit, "cif_data_type", NULL);

   return(cif_value);

} /*  cif_data_type  */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is used by call site semantics processing to output an *|
|*      Object record for a function  result after all the characteristics    *|
|*      about the function result have been resolved.			      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      rslt_idx : Attr index for the result of the specific function being   *|
|*                 called					              *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void cif_object_rec_for_func_result(int  attr_idx)
  
{
   int		rslt_idx;
   boolean      save_cif_done;
   boolean      save_cif_done1;
   int		save_reference;


   TRACE (Func_Entry, "cif_object_rec_for_func_result", NULL);

   /* How this works: This is only for calls to specific intrinsics.   */
   /*                 First cif_call_site_rec is called.  A cif rec is */
   /*                 issued for the interface.   symbol id's are      */
   /*                 assigned to the specific and the result.  Then   */
   /*                 later on in processing from call_site_semantics  */
   /*                 this routine is called where the specific and    */
   /*                 the result records are issued.                   */

   /* We need to send the function attr through as well, because it will not  */
   /* be sent via the cif_send_attr mechanism because only the interface is   */
   /* in the symbol table and it is specially sent during cif_call_site       */
   /* Always send the function result Attr through even though its symbol id  */
   /* might already be nonzero and AT_CIF_DONE might already be TRUE because  */
   /* the Entry Point record generated for each intrinsic function in each    */
   /* CIF scope must have an associated Object record to pass on to CIF the   */
   /* function result type.  For example, if a module contains several module */
   /* procedures, each of which reference BIT_SIZE, each module procedure has */
   /* its own Attr for BIT_SIZE but they all refer to the same function       */
   /* result Attr so we can't go by CIF_SYMBOL_ID being 0 in that function    */
   /* result Attr.	                                                      */

   rslt_idx		 = ATP_RSLT_IDX(attr_idx);
   save_cif_done         = AT_CIF_DONE(rslt_idx);
   save_cif_done1        = AT_CIF_DONE(attr_idx);
   save_reference	 = AT_REFERENCED(attr_idx);
   AT_REFERENCED(attr_idx) = Referenced;
   AT_CIF_DONE(rslt_idx) = FALSE;
   AT_CIF_DONE(attr_idx) = FALSE;
   cif_send_attr(attr_idx, NULL_IDX);
   cif_send_attr(rslt_idx, NULL_IDX);
   AT_CIF_DONE(rslt_idx) = save_cif_done;
   AT_CIF_DONE(attr_idx) = save_cif_done1;
   AT_REFERENCED(attr_idx) = save_reference;

   TRACE (Func_Exit, "cif_object_rec_for_func_result", NULL);

   return;

}  /* cif_object_rec_for_func_result */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Process lists of attrs from the al table.                             *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      al_idx : Attr list index to list to process.                          *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
static void process_attr_list(int	al_idx,
			      boolean	error_list)
{
   int		attr_idx;


   TRACE (Func_Entry, "process_attr_list", NULL);

   /* Pgm_Unit Attr entries (among others) end up in the AL list.  For     */
   /* reasons documented elsewhere in this file, Pgm_Unit Attr entries may */
   /* need to be processed more than once so we can't go by the flag       */
   /* AT_CIF_DONE.  However, by the time the AL is being scanned,          */
   /* processing for Pgm_Unit Attr entries should be essentially complete. */
   /* Therefore, we can now go by AT_CIF_SYMBOL_ID to determine whether or */
   /* not the Attr has been processed.  If it has a symbol ID then we      */
   /* don't want to produce another Entry Point record for it.   	   */

   /* The AL list also contains compiler temp Attr entries.  Although      */
   /* cif_send_attr has a check to ignore these Attr entries (for          */
   /* recursive calls to cif_send_attr), we check for them here to avoid   */
   /* procedure call overhead.					      */


   while (al_idx != NULL_IDX) {
      attr_idx = AL_ATTR_IDX(al_idx);

      if (!error_list &&
          AT_OBJ_CLASS(attr_idx) == Pgm_Unit  &&
          AT_CIF_SYMBOL_ID(attr_idx) != 0) {
         
          /* Just want this to fall through to next AL list item.          */

      }
      else if (AT_OBJ_CLASS(attr_idx) == Data_Obj   &&
               ATD_CLASS(attr_idx) == Compiler_Tmp  &&
               ATD_TMP_NEEDS_CIF(attr_idx)) {

         /* It's a compiler temp that got generated for a DATA or array    */
         /* constructor implied-DO variable, so produce an Object record   */
         /* for it.	         					   */

         cif_send_attr(attr_idx, NULL_IDX);
      }
      else if (! AT_COMPILER_GEND(attr_idx)) {
         cif_send_attr(attr_idx, NULL_IDX);
      }

      al_idx = AL_NEXT_IDX(al_idx);
   }

   TRACE (Func_Exit, "process_attr_list", NULL);

   return;

}  /* process_attr_list */
