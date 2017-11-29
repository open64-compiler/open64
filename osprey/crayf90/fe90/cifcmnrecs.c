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



static char USMID[] = "\n@(#)5.0_pl/sources/cifcmnrecs.c	5.2	06/17/99 09:28:10\n";

#include <stdio.h>


#define CIF_VERSION     3

#include "cif.m"

#include "cif.h"

extern void     Cif_Error (void);



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output the CIF Header record [2].				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:            The file pointer to the CIF being produced.	      *|
|*      language_code:     A numeric representation of the programming	      *|
|*                         language used in the source program.		      *|
|*      compiler_version:  						      *|
|*      cif_creation_date:						      *|
|*      cif_creation_time:						      *|
|*      group_id:     							      *|
|*      message_catalog_file_id:					      *|
|*      host_cpu_name:							      *|
|*      host_cpu_type:							      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void  Cif_Cifhdr_Rec(FILE	*c_i_f,
		    int		 language_code,
		    char	*compiler_version,
		    char        *cif_creation_date,
		    char        *cif_creation_time,
		    char	*group_id,
		    int		 message_catalog_file_id,
		    char	*host_cpu_name,
		    char	*host_cpu_type)
{

   if (fprintf(c_i_f,
               "%d%c%s%c%s%c%d%c%s%c%s%c%s%c%s%c%d%c%s%c%s%c",
               CIF_CIFHDR, EOI,
               "cif", EOI,
               "V03", EOI,
               language_code, EOI,
               compiler_version, EOI,
               cif_creation_date, EOI,
               cif_creation_time, EOI,
               group_id, EOI,
               message_catalog_file_id, EOI,
               host_cpu_name, EOI,
               host_cpu_type, EOR) < 0) {
      Cif_Error();
   }

   return;

} /*  Cif_Cifhdr_Rec  */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a File Name record [7].  				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:     The file pointer to the CIF being produced.	              *|
|*      file_name: A character array containing the file name.		      *|
|*      file_id:   Unique numeric value associated with the file name.        *|
|*      user_specified_file_name:					      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_File_Rec(FILE	*c_i_f,
	          char	*file_name,
	          int    file_id,
		  char	*user_specified_file_name)
{
   if (fprintf(c_i_f, "%d%c%s%c%d%c%s%c",
                      CIF_FILE, EOI,
                      file_name, EOI,
                      file_id, EOI,
		      (user_specified_file_name == (char *) NULL) ? "" :
                         user_specified_file_name, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_File_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Message record [11].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:              The file pointer to the CIF being produced.       *|
|*      message_severity:   The severity level of the message.		      *|
|*      message_number:     The message number.				      *|
|*      file_id:        						      *|
|*      global_line_number: The global source line number.		      *|
|*      column_number:      The column number of the offending text (may be   *|
|*			    0). 					      *|
|*      file_line_number:						      *|
|*      number_of_inserts:						      *|
|*      message_insert:     A pointer to an array of character strings        *|
|*                          containing the message inserts.		      *|
|*      procedure_name:     						      *|
|*      relative_message_order:						      *|
|*      flags:								      *|
|*      physical_file_id:
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_Message_Rec(FILE	*c_i_f,
		     int	 msg_severity,
		     int 	 msg_number,
		     int	 file_id,
		     int	 global_line_number,
		     int	 column_number,
		     int	 file_line_number,
                     int	 number_of_inserts,
		     char       *message_insert[],
		     char	*procedure_name,
		     int	 relative_message_order,
		     int	 flags,
		     int	 physical_file_id)

{
   int	i;


   if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d",
                      CIF_MESSAGE, EOI,
                      msg_severity, EOI,
                      msg_number, EOI,
                      file_id, EOI,
                      global_line_number, EOI,
                      column_number, EOI,
                      file_line_number, EOI,
                      number_of_inserts) < 0) {
      Cif_Error();
   }

   for (i = 0;  i < number_of_inserts;  ++i) {
    
      if (fprintf(c_i_f, "%c%s",
                         EOI, message_insert[i]) < 0) {
         Cif_Error();
      }
   }

   if (fprintf(c_i_f, "%c%s%c%d%c%d%c%d%c",
		      EOI,
		      procedure_name, EOI,
                      relative_message_order, EOI,
		      flags, EOI,
                      physical_file_id, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_Message_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Summary record [15].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:            The file pointer to the CIF being produced.         *|
|*      release_level:    Compiler release level			      *|
|*      gen_date:         Date the compiler was created      		      *|
|*      gen_time:         Time of day the compiler was created 	      	      *|
|*      compilation_time: Cray    : compilation time in microseconds	      *|
|*                        non-Cray: compilation time in microseconds for a    *|
|*                                   "short" compilation, otherwise in        *|
|*                                   seconds; see Algorithm notes below       *|
|*      max_field_length: Maximum amount of memory used			      *|
|*      num_source_lines: Total number of source lines compiled including     *|
|*                        lines in INCLUDE files			      *|
|*      code_size:        Number of words of code generated.		      *|
|*      data_size:        Number of words of static data generated.	      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_Summary_Rec(FILE	*c_i_f,
		     char	*release_level,
		     char	*gen_date,
		     char	*gen_time,
		     char	*compilation_time,
		     long        max_field_length,
		     int	 num_source_lines,
		     int	 code_size,
		     int	 data_size)
{
   if (fprintf(c_i_f, "%d%c%s%c%s%c%s%c%s%c%ld%c%d%c%d%c%d%c",
                      CIF_SUMMARY, EOI,
                      release_level, EOI,
                      gen_date, EOI,
                      gen_time, EOI, 
                      compilation_time, EOI,
                      max_field_length, EOI,
                      num_source_lines, EOI,
                      code_size, EOI,
                      data_size, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_Summary_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Unit record [17].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:            The file pointer to the CIF being produced.         *|
|*      comp_unit_name: 						      *|
|*      file_id:							      *|
|*      line_number:							      *|
|*      column_number:							      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void  Cif_Unit_Rec(FILE		*c_i_f,
		  char		*comp_unit_name,
   		  int		 file_id,
   		  int		 line_number,
		  int		 column_number)
{
   if (fprintf(c_i_f, "%d%c%s%c%d%c%d%c%d%c",
                      CIF_UNIT, EOI,
                      comp_unit_name, EOI, 
                      file_id, EOI,
                      line_number, EOI,
                      column_number, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_Unit_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output an End Unit record [18].					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:            The file pointer to the CIF being produced.         *|
|*      comp_unit_name: 						      *|
|*      file_id:							      *|
|*      line_number:							      *|
|*      column_number:							      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_Endunit_Rec(FILE      *c_i_f,
                     char      *comp_unit_name,
                     int        file_id,
                     int        line_number,
                     int        column_number)
{
   if (fprintf(c_i_f, "%d%c%s%c%d%c%d%c%d%c",
                      CIF_ENDUNIT, EOI,
                      comp_unit_name, EOI,
                      file_id, EOI,
                      line_number, EOI,
                      column_number, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_Endunit_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Command Line Options record [70].			      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:            The file pointer to the CIF being produced.         *|
|*      options:							      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_Orig_Cmd(FILE      *c_i_f,
                  char      *options)
{
   if (fprintf(c_i_f, "%d%c%s%c",
                      CIF_ORIG_CMD, EOI,
                      options, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_Orig_Cmd_Rec */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Output a Source Position record [86].				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      c_i_f:            The file pointer to the CIF being produced.         *|
|*      record_type:							      *|
|*      source_pos_id:						              *|
|*      parent_source_pos_id:						      *|
|*      start_line_number:						      *|
|*      start_column_number:						      *|
|*      file_id:							      *|
|*      end_line_number:						      *|
|*      end_column_number:						      *|
|*      macro_sym_id:							      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING								      *|
|*                                                                            *|
\******************************************************************************/

void Cif_Src_Pos_Rec(FILE      *c_i_f,
 		     int	record_type,
		     int	source_pos_id,
		     int	parent_source_pos_id,
                     int        start_line_number,
                     int        start_column_number,
                     int        file_id,
                     int        end_line_number,
                     int        end_column_number,
		     int	macro_sym_id)
{
   if (fprintf(c_i_f, "%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c%d%c",
                      CIF_SRC_POS, EOI,
                      record_type, EOI,
		      source_pos_id, EOI,
                      parent_source_pos_id, EOI,
                      start_line_number, EOI,
                      start_column_number, EOI,
                      file_id, EOI,
                      end_line_number, EOI,
                      end_column_number, EOI,
                      macro_sym_id, EOR) < 0) {
      Cif_Error();
   }

   return;

}  /* Cif_Source_Position_Rec */
