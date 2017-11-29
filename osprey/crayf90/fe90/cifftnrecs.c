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



static char USMID[] = "\n@(#)5.0_pl/sources/cifftnrecs.c	5.2	06/17/99 09:28:10\n";

#include <stdio.h>


#define CIF_VERSION     3

#include "cif.m"

#include "cif.h"

extern void     Cif_Error (void);


/* ************************************************************************** */
/*                                                                            */
/*   The following definitions are temporarily included directly in this      */
/*   until such time as this file is integrated into libcif.  At that point,  */
/*   these definitions should probably be split out into a libcif header      */
/*   file.								      */
/*                                                                            */
/* ************************************************************************** */

/* MPP Geometry record [26].						      */

/*   Note:  This record is not yet output by CF90 and may have a different    */
/*          form depending on what happens to CRAFT.			      */
/*									      */
/*          It will also need a declaration of the following type because a   */
/*          number of these can be passed:				      */

struct  Cif_geometry_dim_entry 
		     {
		       int	distribution; 	/* Dimension distribution.    */
		       char    *weight;		/* Weight.		      */
		       int      weight_file_id;	/* Weight file id.	      */
      		       int      weight_line_number;
						/* Weight file line number.   */
                       int      weight_column_number;
						/* Weight column number.      */
		       char    *block_size;	/* Block size.		      */
    		       int      block_size_file_id;
						/* Block size file id.	      */
    		       int      block_size_line_number;
						/* Block size file line num.  */
                       int 	block_size_column_number;
						/* Block size column number.  */
                     };

typedef struct  Cif_geometry_dim_entry   Cif_geometry_dim;




/* Object record [36].							      */

/*    Note:  The Distribution and Geometry Id fields are not currently being  */
/*           output by the front-end.  These fields may change depending on   */
/*           the final CRAFT design.					      */

/* The bound information must be in character form because a bound can be an  */
/* expression which is represented by the character "E".  		      */

struct Cif_f90_dim_entry { char  *lower_bound;
                           char  *upper_bound;
                         };

typedef struct Cif_f90_dim_entry Cif_f90_dim;
 



/*       Note:  Since we've added many new options, surely this record        */
/*              will need to be expanded.				      */


/* Optimization Options record [38].					      */


struct Cif_f90_level_opt_entry {
                                int   option;   /* Option flag.	      */
                                int   level;    /* The optimization level.  */
                               };

typedef struct  Cif_f90_level_opt_entry  Cif_f90_level_opt;



/* ************************************************************************** */
/*									      */
/*    			End temporary definitions.			      */
/*									      */
/* ************************************************************************** */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a CDIR$ record [5].                                            *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:            The file pointer to the CIF being produced.	      *|
|*      directive_type:							      *|
|*      file_id:							      *|
|*      line_number:							      *|
|*      column_number:							      *|
|*      num_copy_vars:							      *|
|*      copy_var_sym_id:						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void Cif_Cdir_Rec(FILE		*c_i_f,
		  int		 directive_type,
		  int		 file_id,
	          int		 line_number,
		  int		 column_number,
      		  int		 num_copy_vars,
		  long		*copy_var_sym_id)

{
   int	i;


   if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%d%c%d",
	              CIF_CDIR, EOI,
		      directive_type, EOI,
                      file_id, EOI,
		      line_number, EOI,
		      column_number, EOI,
		      num_copy_vars) < 0) {
       Cif_Error();
   }


   for (i = 0;  i < num_copy_vars;  ++i) {

      if (fprintf(c_i_f, "%c%ld",
                         EOI, copy_var_sym_id[i]) < 0) {
         Cif_Error();
      }
   }

   if (fprintf(c_i_f, "%c", EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_Cdir_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output an Include record [9].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:     The file pointer to the CIF being produced.                *|
|*      parent_file_id:							      *|
|*      line_number:							      *|
|*      column_number:							      *|
|*      include_file_id:						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_Include_Rec(FILE	*c_i_f,
		     int	 parent_file_id,
		     int 	 line_number,
		     int	 column_number,
                     int	 include_file_id)
{
   if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%d%c",
                      CIF_INCLUDE, EOI,
                      parent_file_id,  EOI,
                      line_number, EOI,
                      column_number, EOI,
                      include_file_id, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_Include_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Source File record [14]. 				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:       The file pointer to the CIF being produced.              *|
|*      file_id:     The file id of the file containing the program.          *|
|*      source_form: The source form in which the program is written (at      *|
|*                   least initially).					      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_Srcfile_Rec(FILE	*c_i_f,
		     int	 file_id,
		     int	 source_form)
{
   if (fprintf(c_i_f, "%d%c%d%c%d%c",
                      CIF_SRCFILE, EOI,
                      file_id, EOI,
                      source_form, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_Srcfile_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a DOSHARED CDIR$ record [16].				      *|
|*                                                                            *|
|*      ***  NOT YET PRODUCED BY THE FRONT-END.  INCOMPLETE  ***	      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:            The file pointer to the CIF being produced.         *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Usage record [19].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:       The file pointer to the CIF being produced.              *|
|*      sym_id:      							      *|
|*      file_id:							      *|
|*      line_number: The line number containing the symbol for which this     *|
|*                   Usage record is being produced.			      *|
|*      col_number:  The column number for the symbol.			      *|
|*      usage_code:  The CIF-defined usage code.			      *|
|*      num_other_sym_ids: 						      *|
|*      component_sym_id:						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_Usage_Rec(FILE		*c_i_f,
		   int		 sym_id,
		   int		 file_id,
		   int		 line_number,
		   int		 col_number,
		   int		 usage_code,
		   int		 num_other_sym_ids,
		   long		*component_sym_id)
{
   int	i;


   if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%d%c%d%c%d",
                      CIF_USAGE, EOI,
           	      sym_id, EOI,
   		      file_id, EOI,
		      line_number, EOI,
		      col_number, EOI,
                      usage_code, EOI,
                      num_other_sym_ids) < 0) {
      Cif_Error();
   }

   for (i = 0;  i < num_other_sym_ids;  ++i) {

      if (fprintf(c_i_f, "%c%ld",
                         EOI, component_sym_id[i]) < 0) {
         Cif_Error();
      }
   }

   if (fprintf(c_i_f, "%c", EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_Usage_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output an Enable/Disable Compiler Options record [21].		      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:               The file pointer to the CIF being produced.      *|
|*      enable_disable_opts:						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_EDopts_Rec(FILE	*c_i_f,
		    int		 enable_disable_opts)
{
   if (fprintf(c_i_f, "%d%c%x%c",
		      CIF_EDOPTS, EOI,
		      enable_disable_opts, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_EDopts_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Machine Characteristics record [22].			      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:       The file pointer to the CIF being produced.              *|
|*      cpu_type:							      *|
|*      memory_speed:							      *|
|*      memory_size:							      *|
|*      characteristics:						      *|
|*      num_memory_banks:						      *|
|*      num_cpus:							      *|
|*      instruction_buffer_size:					      *|
|*      clock_period:							      *|
|*      num_cluster_reg_sets:						      *|
|*      bank_busy_time:							      *|
|*      word_bit_len:							      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_Mach_Char_Rec(FILE	*c_i_f,
		       char	*cpu_type,
		       long	 memory_speed,
		       long	 memory_size,
		       int 	 characteristics,
		       long	 num_memory_banks,
		       long	 num_cpus,
		       long 	 instruction_buffer_size,
		       long 	 clock_period,
		       long	 num_cluster_reg_sets,
		       long	 bank_busy_time,
		       int	 word_bit_len)
{
   if (fprintf(c_i_f, "%d%c%s%c%ld%c%ld%c%x%c%ld%c%ld%c%ld%c%ld%c%ld%c%ld%c%d%c",
		      CIF_MACH_CHAR, EOI,
		      cpu_type, EOI,
		      memory_speed, EOI,
		      memory_size, EOI,
		      characteristics, EOI,
		      num_memory_banks, EOI,
 		      num_cpus, EOI,
		      instruction_buffer_size, EOI,
		      clock_period, EOI,
		      num_cluster_reg_sets, EOI,
		      bank_busy_time, EOI,
                      word_bit_len, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_Mach_Char_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Statement Type record [25].				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:       The file pointer to the CIF being produced.              *|
|*      stmt_type:							      *|
|*      file_id:							      *|
|*      line_number:							      *|
|*      column_number:							      *|
|*  	stmt_number:							      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_Stmt_Type_Rec(FILE	*c_i_f,
		       int	 stmt_type,
		       int 	 file_id,
		       int 	 line_number,
		       int	 column_number,
		       int	 stmt_number)
{
   /* The definition of the Stmt Type record has an "end file id" (efid)      */
   /* field, an "end line number" (eline) field, and "end character position" */
   /* (ecpos) field.  The full layout of the ASCII Stmt Type record is:       */
   /*   rectype, type, fid, line, cpos, efid, eline, ecpos                    */
   /* Fortran does not currently use the efid, eline, or ecpos fields.  Since */
   /* we don't use them, we overload the eline field with the statement       */
   /* number.								      */
   
   if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c",
           	      CIF_STMT_TYPE, EOI,
		      stmt_type, EOI,
		      file_id, EOI,
		      line_number, EOI,
		      column_number, EOI,
		      0, EOI,
		      stmt_number, EOI,
		      0, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_Stmt_Type_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output an MPP Geometry record [26].  **  NOT YET IMPLEMENTED **       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:       The file pointer to the CIF being produced.              *|
|*      name:								      *|
|*      sym_id:								      *|
|*      num_dims:							      *|
|*      dim:								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Continuation Line record [27].    			      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:             The file pointer to the CIF being produced.        *|
|*      continuation_type:                                                    *|
|*      file_id:                                                              *|
|*      line_number:                                                          *|
|*      column_number:                                                        *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void Cif_Continuation_Rec(FILE	*c_i_f,
                          int    continuation_type,
                          int    file_id,
		          int	 line_number,
			  int	 column_number)
{
   if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%d%c",
     		      CIF_CONTINUATION, EOI,
                      continuation_type, EOI,
		      file_id, EOI,
		      line_number, EOI,
                      column_number, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_Continuation_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Call Site record [28].  				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:       The file pointer to the CIF being produced.              *|
|*      sym_id:								      *|
|*      scope_id:							      *|
|*      file_id:                                                              *|
|*      line_number:                                                          *|
|*      column_number:                                                        *|
|*      specific_proc_sym_id:						      *|
|*      max_num_actual_args:						      *|
|*      arg_sym_id:							      *|
|*      arg_rank:							      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void Cif_F90_Callsite_Rec(FILE	 *c_i_f,
			  int	  sym_id,
			  int	  scope_id,
                          int     file_id,
       	                  int	  line_number,
			  int	  column_number,
 			  int	  specific_proc_sym_id,
			  int	  max_num_actual_args,
			  char	 *arg_sym_id[],
			  int	 *arg_rank)
{
   int		i;


   if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d",
     		      CIF_F90_CALLSITE, EOI,
		      sym_id, EOI,
		      scope_id, EOI,
		      file_id, EOI,
		      line_number, EOI,
                      column_number, EOI,
		      specific_proc_sym_id, EOI,
		      max_num_actual_args) < 0) {
      Cif_Error();
   }

   if (max_num_actual_args > 0) {

      for (i = 0;  i < max_num_actual_args;  ++i) {

         if (fprintf(c_i_f, "%c%s",
                            EOI, arg_sym_id[i]) < 0) {
            Cif_Error();
         }
      }

      for (i = 0;  i < max_num_actual_args;  ++i) {

         if (fprintf(c_i_f, "%c%d",
                            EOI, arg_rank[i]) < 0) {
            Cif_Error();
         }
      }
   }

   if (fprintf(c_i_f, "%c", EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_F90_Callsite_Rec */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*    Output a Common Block [29] record.                                      *|
|*									      *|
|* Input parameters:							      *|
|*      c_i_f:       The file pointer to the CIF being produced.              *|
|*      name:								      *|
|*      sym_id:								      *|
|*      scope_id:							      *|
|*      storage_class:							      *|
|*      module_sym_id:							      *|
|*      common_block_length:						      *|
|*      distribution:							      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void Cif_F90_Comblk_Rec(FILE		*c_i_f,
			char		*name,
                        int 		 sym_id,
		        int		 scope_id,
		        int	 	 storage_class,
		        int		 module_sym_id,
		        int 		 common_block_length,
		        int	  	 distribution)
{
   if (fprintf(c_i_f, "%d%c%s%c%d%c%d%c%d%c%d%c%d%c%d%c",
                      CIF_F90_COMBLK, EOI,
	              name, EOI, 
                      sym_id, EOI,
                      scope_id, EOI,
               	      storage_class, EOI,
                      module_sym_id, EOI,
               	      common_block_length, EOI,
                      distribution, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_F90_Comblk_Rec */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*    Output a Named Constant [30] record.                                    *|
|*									      *|
|* Input parameters:							      *|
|*      c_i_f:       The file pointer to the CIF being produced.              *|
|*      sym_id:								      *|
|*      scope_id:							      *|
|*      scalar_aggregate:						      *|
|*      value:								      *|
|*      file_id:							      *|
|*      start_line_number:						      *|
|*      start_column_number:						      *|
|*      end_line_number:						      *|
|*      end_column_number:						      *|
|*      original_form_flag:  These two will be output maybe some day but      *|
|*      original_form:	     are not being output for the time being.         *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void Cif_F90_Const_Rec(FILE	*c_i_f,
                       int	 sym_id,
            	       int	 scope_id,
		       int 	 scalar_aggregate,
		       char	*value,
		       int	 file_id,
		       int 	 start_line_number,
		       int	 start_column_number,
		       int 	 end_line_number,
		       int	 end_column_number)
{
   if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%s%c%d%c%d%c%d%c%d%c%d%c",
                      CIF_F90_CONST, EOI,
                      sym_id, EOI,
                      scope_id, EOI,
		      scalar_aggregate, EOI,
	              value, EOI, 
		      file_id, EOI,
		      start_line_number, EOI,
		      start_column_number, EOI,
		      end_line_number, EOI,
		      end_column_number, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_F90_Const_Rec */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*    Output an Entry Point [31] record.                                      *|
|*									      *|
|* Input parameters:							      *|
|*      c_i_f:       The file pointer to the CIF being produced.              *|
|*      name:								      *|
|*      sym_id:								      *|
|*      scope_id:							      *|
|*      program_unit_type:						      *|
|*      procedure_type:							      *|
|*      attributes:							      *|
|*      result_sym_id:							      *|
|*      module_sym_id:							      *|
|*      num_dummy_args:							      *|
|*      dummy_arg_sym_id:						      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void Cif_F90_Entry_Rec(FILE	*c_i_f,
		       char	*name,
                       int	 sym_id,
		       int	 scope_id,
		       int	 program_unit_type,
		       int	 procedure_type,
		       int 	 attributes,
		       int  	 result_sym_id,
		       int 	 module_sym_id,
		       int	 num_dummy_args,
		       long	*dummy_arg_sym_id)
{
   int		i;


   if (fprintf(c_i_f,
               "%d%c%s%c%d%c%d%c%d%c%d%c%x%c%d%c%d%c%d",
               CIF_F90_ENTRY, EOI,
               name, EOI,
               sym_id, EOI,
               scope_id, EOI,
               program_unit_type, EOI,
               procedure_type, EOI,
               attributes, EOI,
               result_sym_id, EOI,
               module_sym_id, EOI,
               num_dummy_args) < 0) {
      Cif_Error();
   }

   for (i = 0;  i < num_dummy_args;  i++) {

      if (fprintf(c_i_f, "%c%ld", EOI, dummy_arg_sym_id[i]) < 0) {
         Cif_Error();
      }
   }

   if (fprintf(c_i_f, "%c", EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_F90_Entry_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Loop Definitions record [32].				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:       The file pointer to the CIF being produced.              *|
|*      scope_id:							      *|
|*      loop_type:							      *|
|*      start_file_id:							      *|
|*      start_line_number:						      *|
|*      start_column_number:						      *|
|*      end_file_id:							      *|
|*      end_line_number:						      *|
|*      end_column_number:						      *|
|*      do_var_sym_id:							      *|
|*      term_label_sym_id:						      *|
|*      construct_name_sym_id:						      *|
|*	end_stmt_num:							      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_F90_Loop_Rec(FILE	*c_i_f,
		      int	 scope_id,
		      int	 loop_type,
		      int	 start_file_id,
		      int	 start_line_number,
		      int	 start_column_number,
		      int	 end_file_id,
		      int	 end_line_number,
		      int	 end_column_number,
		      int	 do_var_sym_id,
		      int	 term_label_sym_id,
		      int	 construct_name_sym_id,
 		      int	 end_stmt_num)
{
   if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c",
                      CIF_F90_LOOP, EOI,
          	      scope_id, EOI,
       		      loop_type, EOI,
		      start_file_id, EOI,
		      start_line_number, EOI,
		      start_column_number, EOI,
		      end_file_id, EOI, 
		      end_line_number, EOI, 
		      end_column_number, EOI,
		      do_var_sym_id, EOI,
		      term_label_sym_id, EOI,
		      construct_name_sym_id, EOI,
                      end_stmt_num, EOR) < 0) {
      Cif_Error();
   }  

   return;

}  /* Cif_F90_Loop_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Derived Type record [33].				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:       The file pointer to the CIF being produced.              *|
|*      name:								      *|
|*      sym_id:								      *|
|*      scope_id:							      *|
|*      derived_type_id:						      *|
|*      attributes:							      *|
|*      num_components:							      *|
|*      component_sym_id:						      *|
|*      module_sym_id:							      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_F90_Derived_Type_Rec(FILE	*c_i_f,
			      char	*name,
			      int	 sym_id,
			      int	 scope_id,
			      int	 derived_type_id,
			      int	 attributes,
			      int	 num_components,
		              long	*component_sym_id, 
			      int	 module_sym_id)
{
   int		i;

   if (fprintf(c_i_f, "%d%c%s%c%d%c%d%c%d%c%x%c%d",
                      CIF_F90_DERIVED_TYPE, EOI, 
                      name, EOI,
                      sym_id, EOI,
                      scope_id, EOI,
                      derived_type_id, EOI,
                      attributes, EOI,
                      num_components) < 0) {
      Cif_Error();
   }

   for (i = 0;  i < num_components;  ++i) {

      if (fprintf(c_i_f, "%c%ld",
                         EOI, component_sym_id[i]) < 0) {
         Cif_Error();
      }
   }

   if (fprintf(c_i_f, "%c%d%c",
                      EOI,
                      module_sym_id, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_F90_Derived_Type_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Label record [34].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:       The file pointer to the CIF being produced.              *|
|*      name:								      *|
|*      sym_id:								      *|
|*      scope_id:							      *|
|*      label_class:							      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_F90_Label_Rec(FILE	*c_i_f,
		       char	*name,
		       int 	 sym_id,
		       int	 scope_id,
		       int	 label_class)
			
{
   if (fprintf(c_i_f, 
               "%d%c%s%c%d%c%d%c%d%c",
               CIF_F90_LABEL, EOI, 
               name, EOI,
               sym_id, EOI,
               scope_id, EOI,
               label_class, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_F90_Label_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Namelist record [35].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:       The file pointer to the CIF being produced.              *|
|*      name:								      *|
|*      sym_id:								      *|
|*      scope_id:							      *|
|*      module_sym_id:							      *|
|*      num_members:							      *|
|*      member_sym_id:							      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_F90_Namelist_Rec(FILE		*c_i_f,
		          char		*name,
		          int 		 sym_id,
		          int		 scope_id,
		          int		 module_sym_id,
		          int		 num_members,
		          long		*member_sym_id)
			
{
   int		i;


   if (fprintf(c_i_f, "%d%c%s%c%d%c%d%c%d%c%d",
                      CIF_F90_NAMELIST, EOI, 
                      name, EOI,
                      sym_id, EOI,
                      scope_id, EOI,
   		      module_sym_id, EOI,
                      num_members) < 0) {
      Cif_Error();
   }

   for (i = 0;  i < num_members;  ++i) {

      if (fprintf(c_i_f, "%c%ld",
                         EOI, member_sym_id[i]) < 0) {
         Cif_Error();
      }
   }

   if (fprintf(c_i_f, "%c", EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_F90_Namelist_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Object record [36].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:       The file pointer to the CIF being produced.              *|
|*      name:								      *|
|*      sym_id:								      *|
|*      scope_id:							      *|
|*      data_type:							      *|
|*      symbol_class:							      *|
|*      storage_class:							      *|
|*      storage_sym_id:							      *|
|*      offset:								      *|
|*      attributes:							      *|
|*      derived_type_id:						      *|
|*      char_len:							      *|
|*      num_dimensions:							      *|
|*      array_type:							      *|
|*      dim:								      *|
|*      distribution:							      *|
|*      geometry_id:							      *|
|*      cri_ptr_sym_id:							      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_F90_Object_Rec(FILE			*c_i_f,
           	        char			*name,
			int		  	 sym_id,
			int		 	 scope_id,
			int		 	 data_type,
			int		 	 symbol_class,
			int		 	 storage_class,
			int		 	 storage_sym_id,
			int		 	 offset,
			int		 	 attributes,
			int		 	 derived_type_id,
			char       	 	*char_len,
			int		 	 num_dimensions,
			int		 	 array_type,
			Cif_f90_dim		*dim,
			int		 	 distribution,
			int			 geometry_id,
			int			 cri_ptr_sym_id)
{
   int		i;

   if (fprintf(c_i_f,
               "%d%c%s%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%x%c%d%c%s%c%d%c%d%c",
                      CIF_F90_OBJECT, EOI, 
                      name, EOI,
                      sym_id, EOI,
                      scope_id, EOI,
                      data_type, EOI,
                      symbol_class, EOI,
                      storage_class, EOI,
                      storage_sym_id, EOI,
                      offset, EOI,
                      attributes, EOI,
                      derived_type_id, EOI,
                      char_len, EOI,
                      num_dimensions, EOI, 
                      array_type, EOI) < 0) {
      Cif_Error();
   }

   if (num_dimensions > 0  &&  array_type != CIF_AT_DEFERRED) {

      for (i = 0;  i < num_dimensions;  ++i) {

         if (dim[i].lower_bound != NULL) {

            if (fprintf(c_i_f, "%s%c",
                               dim[i].lower_bound, EOI) < 0) {
               Cif_Error();
            }
         }

         if (dim[i].upper_bound != NULL) {

            if (fprintf(c_i_f, "%s%c",
                               dim[i].upper_bound, EOI) < 0) {
               Cif_Error();
            }
         }
      } 
   }

   if (fprintf(c_i_f, "%d%c%d%c%d%c",
                      distribution, EOI,
                      geometry_id, EOI,
                      cri_ptr_sym_id, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_F90_Object_Rec */
			

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Miscellaneous Compiler Options record [37].		      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:               The file pointer to the CIF being produced.      *|
|*      i_opt_value:							      *|
|*	m_opt_value:							      *|
|*	V_opt:								      *|
|*	t_opt_enabled:							      *|
|*	t_opt_value:							      *|
|*	num_disabled_msgs:						      *|
|*	msg_num:							      *|
|*	num_disabled_cdirs:						      *|
|*	cdir_name:							      *|
|*	dot_o_name:							      *|
|*	cal_file_name:							      *|
|*	inline_file_name:						      *|
|*      cif_name:							      *|
|*	C_opt_flags:							      *|
|*	N_opt_value:							      *|
|*	num_I_opts:							      *|
|*	I_opt_path_name:						      *|
|*	num_p_opts:							      *|
|*	p_opt_path_name:						      *|
|*	source_form:							      *|
|*	R_opt_flags:							      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING 							      *|
|*                                                                            *|
\******************************************************************************/

void Cif_F90_Misc_Opts_Rec(FILE		*c_i_f,
			   int		 i_opt_value,
			   int		 m_opt_value,
			   int		 V_opt,
			   int		 t_opt_enabled,
			   int		 t_opt_value,
			   int		 num_disabled_msgs,
			   long		*msg_num,
			   int		 num_disabled_cdirs,
			   char	        *cdir_name[],
			   char		*dot_o_name,
			   char		*cal_file_name,
			   char		*inline_file_name,
			   char	 	*cif_name,
			   int		 C_opt_flags,
			   int           N_opt_value,
			   int		 num_I_opts,
			   char	        *I_opt_path_name[],
			   int		 num_p_opts,
			   char         *p_opt_path_name[],
			   int		 source_form,
			   int		 R_opt_flags)
{
   int		i;


   /* KAY - check why R_opt_flags isn't being used. */

   if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%d%c%d%c%d%c", 
                      CIF_F90_MISC_OPTS, EOI,
		      i_opt_value, EOI,
		      m_opt_value, EOI,
		      V_opt, EOI,
		      t_opt_enabled, EOI,
		      t_opt_value, EOI,
		      num_disabled_msgs, EOI) < 0) {
      Cif_Error();
   }

   for (i = 0;  i < num_disabled_msgs;  ++i) {
         
      if (fprintf(c_i_f, "%ld%c",
                         msg_num[i], EOI) < 0) {
         Cif_Error();
      }
   }

   if (fprintf(c_i_f, "%d%c", num_disabled_cdirs, EOI) < 0) {
      Cif_Error();
   }

   for (i = 0;  i < num_disabled_cdirs;  ++i) {
         
      if (fprintf(c_i_f, "%s%c",
                         cdir_name[i], EOI) < 0) {
         Cif_Error();
      }
   }

   if (fprintf(c_i_f, "%s%c%s%c%s%c%s%c%x%c%d%c%d%c",
		      dot_o_name, EOI,
		      cal_file_name, EOI,
		      inline_file_name, EOI,
		      cif_name, EOI,	
		      C_opt_flags, EOI,
		      N_opt_value, EOI,
                      num_I_opts, EOI) < 0) {
                      
      Cif_Error();
   }

   for (i = 0;  i < num_I_opts;  ++i) {

      if (fprintf(c_i_f, "%s%c",
                         I_opt_path_name[i], EOI) < 0) {
         Cif_Error();
      }
   }

   if (fprintf(c_i_f, "%d%c",
                      num_p_opts, EOI) < 0) {
      Cif_Error();
   }

   for (i = 0;  i < num_p_opts;  ++i) {

      if (fprintf(c_i_f, "%s%c",
                         p_opt_path_name[i], EOI) < 0) {
         Cif_Error();
      }
   }

   if (fprintf(c_i_f, "%d%c",
		      source_form, EOR) < 0) {
      Cif_Error();
   } 

   return;

}  /* Cif_F90_Misc_Opts_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Optimization Options record [38].			      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:               The file pointer to the CIF being produced.      *|
|*      options:							      *|
|*      num_level_opts:							      *|
|*      level_opt:							      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_F90_Opt_Opts_Rec(FILE			*c_i_f,
			  int		 	 options,
			  int		 	 num_level_opts,
		          Cif_f90_level_opt	*level_opt)
{
   int		i;


   if (fprintf(c_i_f, "%d%c%x%c%d",
                      CIF_F90_OPT_OPTS, EOI,
		      options, EOI,
                      num_level_opts) < 0) {
      Cif_Error();
   }

   for (i = 0;  i < num_level_opts;  ++i) {

      if (fprintf(c_i_f, "%c%x%c%d",
                         EOI, level_opt[i].option, 
                         EOI, level_opt[i].level) < 0) {
         Cif_Error();
      }
   }
   
   if (fprintf(c_i_f, "%c", EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_Opt_Opts_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Begin Scope record [39].				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:               The file pointer to the CIF being produced.      *|
|*      scope_id:							      *|
|*      sym_id:								      *|
|*      file_id:							      *|
|*      line_number:							      *|
|*      column_number:							      *|
|*      scope_type:							      *|
|*      nesting_level:							      *|
|*      parent_scope_id:						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_F90_Begin_Scope_Rec(FILE	*c_i_f,
			     int	 scope_id,
 			     int	 sym_id,
			     int	 file_id,
			     int	 line_number,	
			     int	 column_number,	
			     int	 scope_type,
			     int	 nesting_level,
			     int	 parent_scope_id)
{
   if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c",
                      CIF_F90_BEGIN_SCOPE, EOI,
                      scope_id, EOI,
                      sym_id, EOI,
                      file_id, EOI,
                      line_number, EOI,
                      column_number, EOI,
                      scope_type, EOI,
                      nesting_level, EOI,
                      parent_scope_id, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_F90_Begin_Scope_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a End Scope record [40].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:               The file pointer to the CIF being produced.      *|
|*      scope_id:                                                             *|
|*      file_id:                                                              *|
|*      line_number:                                                          *|
|*      column_number:                                                        *|
|*      scope_in_error:                                                       *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING 							      *|
|*                                                                            *|
\******************************************************************************/

void Cif_F90_End_Scope_Rec(FILE		*c_i_f,
                           int           scope_id,
                           int           file_id,
                           int           line_number,
                           int           column_number,
                           int           scope_in_error)

{
   if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%d%c%d%c",
                      CIF_F90_END_SCOPE, EOI,
    		      scope_id, EOI,
                      file_id, EOI,
                      line_number, EOI,
                      column_number, EOI,
                      scope_in_error, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_F90_End_Scope_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Scope Info record [41].				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:               The file pointer to the CIF being produced.      *|
|*      scope_id:                                                             *|
|*      attributes:                                                           *|
|*      num_alt_entries:                                                      *|
|*      alt_entry_sym_id:                                                     *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_F90_Scope_Info_Rec(FILE	*c_i_f,
                            int          scope_id,
                            int          attributes,
                            int          num_alt_entries,
                            long        *alt_entry_sym_id) 
{
   int		i;


   if (fprintf(c_i_f, "%d%c%d%c%x%c%d",
                      CIF_F90_SCOPE_INFO, EOI,
                      scope_id, EOI,
                      attributes, EOI,
                      num_alt_entries) < 0) {
      Cif_Error();
   }

   for (i = 0;  i < num_alt_entries;  ++i) {

      if (fprintf(c_i_f, "%c%ld",
                         EOI, alt_entry_sym_id[i]) < 0) {
         Cif_Error();
      }
   }

   if (fprintf(c_i_f, "%c", EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_F90_Scope_Info_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Use Module record [42].				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:               The file pointer to the CIF being produced.      *|
|*      module_sym_id:							      *|
|*      module_file_id:							      *|
|*      flag:								      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_F90_Use_Module_Rec(FILE	*c_i_f,
			    int		 module_sym_id,
			    int		 module_file_id,
			    int		 flag)
{
   if (fprintf(c_i_f,
               "%d%c%d%c%d%c%d%c",
               CIF_F90_USE_MODULE, EOI,
               module_sym_id, EOI,
               module_file_id, EOI,
               flag, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_F90_Use_Module_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Rename record [43].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:               The file pointer to the CIF being produced.      *|
|*      scope_id:						              *|
|*      name_in_module:							      *|
|*      name_in_module_sym_id:						      *|
|*      module_sym_id:							      *|
|*      original_name:							      *|
|*      original_module_sym_id:						      *|
|*      local_name_sym_id:						      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_F90_Rename_Rec(FILE	*c_i_f,
			int	 scope_id,
			char	*name_in_module,
		        int	 name_in_module_sym_id,
		        int	 module_sym_id,
			char	*original_name,
		        int	 original_module_sym_id,
			long	 local_name_sym_id)
{
   if (fprintf(c_i_f,
               "%d%c%d%c%s%c%d%c%d%c%s%c%d%c%ld%c",
               CIF_F90_RENAME, EOI,
               scope_id, EOI,
               name_in_module, EOI,
               name_in_module_sym_id, EOI,
               module_sym_id, EOI,
               original_name, EOI,
               original_module_sym_id, EOI,
               local_name_sym_id, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_F90_Rename_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output an Interface Block record [44].                                *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:               The file pointer to the CIF being produced.      *|
|*      name:		                                                      *|
|*      sym_id: 	                                                      *|
|*      scope_id:                                                             *|
|*      int_block_type:                                                       *|
|*      attributes: 	                                                      *|
|*      num_interfaces:	                                                      *|
|*      specific_proc_sym_id:                                                 *|
|*      module_sym_id:	                                                      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void Cif_F90_Int_Block_Rec(FILE		*c_i_f,
			   char		*name,
			   int		 sym_id,
                           int      	 scope_id,
			   int		 int_block_type,
			   int		 attributes,
			   int		 num_interfaces,
			   long		*specific_proc_sym_id,
		           int		 module_sym_id)
{
   int		i;


   if (fprintf(c_i_f, "%d%c%s%c%d%c%d%c%d%c%x%c%d",
                      CIF_F90_INT_BLOCK, EOI, 
                      name, EOI,
                      sym_id, EOI,
                      scope_id, EOI,
                      int_block_type, EOI,
                      attributes, EOI,
                      num_interfaces) < 0) {
      Cif_Error();
   }

   for (i = 0;  i < num_interfaces;  ++i) {

      if (fprintf(c_i_f, "%c%ld",
                     EOI, specific_proc_sym_id[i]) < 0) {
         Cif_Error();
      }
   }

   if (fprintf(c_i_f, "%c%d%c",
                      EOI, 
                      module_sym_id, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_F90_Int_Block_Rec */

