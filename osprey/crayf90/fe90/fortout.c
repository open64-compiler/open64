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



static char USMID[] = "\n@(#)5.0_pl/sources/fortout.c	5.2	05/27/99 10:30:26\n";

# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"

static	void print_attr_f(int, FILE *);
static	char start[20];
static	int  start_column;

    
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

void	print_scp_to_fortran(int	ln_fw_idx,
			     int	ln_lw_idx,
			     int	pgm_attr_idx,
			     FILE	*outfile)

{
   int		attr_idx;
   int		i;
   int		ln_idx;


   for (i=1; i < 20; i++) start[i]	= '\0';
   start[0]	= '\n';
   start_column	= 1;  /* Points to column containing first NULL. */

   fprintf(outfile, "%s!DIR$ FREE", start);

   print_attr_f(pgm_attr_idx, outfile);

   for (ln_idx = ln_fw_idx; ln_idx <= ln_lw_idx; ln_idx++) {
      attr_idx	= LN_ATTR_IDX(ln_idx);

      switch (AT_OBJ_CLASS(attr_idx)) {
      case Data_Obj:

         if (ATD_CLASS(attr_idx) != Dummy_Argument) {
            print_attr_f(attr_idx, outfile);
         }
         break;

      case Pgm_Unit:

         if (ATP_IN_INTERFACE_BLK(attr_idx)) {

            if (ATP_IN_UNNAMED_INTERFACE(attr_idx)) {
               print_attr_f(attr_idx, outfile);
            }
         }
         else if (attr_idx != pgm_attr_idx) {
            print_attr_f(attr_idx, outfile);
         }
         break;

      case Label:  /* Done during IR printing */
         break;

      case Derived_Type:
      case Interface:
      case Namelist_Grp:
      case Stmt_Func:
         print_attr_f(attr_idx, outfile);
         break;
      }
   }

   fprintf(outfile, "%sEND\n", start);

   return;
}

    
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

static	void print_attr_f (int		 attr_idx,
			   FILE		*outfile)

{
   char		*comma;
   int		 i;
   int		 len;
   int		 newlen;
   int		 num_dargs;
#ifdef KEY /* Bug 10177 */
   int		 save_start_column = 0;
#else /* KEY Bug 10177 */
   int		 save_start_column;
#endif /* KEY Bug 10177 */
   int		 sn_idx;


   switch (AT_OBJ_CLASS(attr_idx)) {
   case Data_Obj:

      if (ATD_IN_COMMON(attr_idx) && 
          SB_FIRST_ATTR_IDX(ATD_STOR_BLK_IDX(attr_idx)) == attr_idx) {

         /* First attr in the common block - print block */

         if (SB_BLANK_COMMON(ATD_STOR_BLK_IDX(attr_idx))) {
            fprintf(outfile, "%sCOMMON // ", start);
            fprintf(outfile, "%sCOMMON /%s/ ", start,
                             SB_NAME_PTR(ATD_STOR_BLK_IDX(attr_idx)));
         }
      }

      if (ATD_TYPE_IDX(attr_idx) != NULL_IDX) {
         fprintf(outfile, "%s%s :: ", start,
                 print_type_f(ATD_TYPE_IDX(attr_idx)));
      }
      else {
         fprintf(outfile, "%s", start);
      }
      fprintf(outfile, "%s", AT_OBJ_NAME_PTR(attr_idx));
      break;

   case Pgm_Unit:
      num_dargs	= 0;

      switch (ATP_PGM_UNIT(attr_idx)) {
      case Pgm_Unknown:
         break;

      case Function:
      case Subroutine:
         fprintf(outfile, "%s", start);
         len = start_column - 1;

         if (ATP_IN_UNNAMED_INTERFACE(attr_idx)) {
            fprintf(outfile, "INTERFACE ");
            save_start_column	= start_column;

            if (start_column <= 18) {
               start[start_column++]	= ' ';
               start[start_column++]	= ' ';
            }
            fprintf(outfile, "%s", start);
            len = start_column - 1;
         }

         if (ATP_RECURSIVE(attr_idx)) {
            fprintf(outfile, "RECURSIVE ");
            len = 10;
         }

         if (ATP_PURE(attr_idx)) {
            fprintf(outfile, "PURE ");
            len += 5;
         }

         if (ATP_ELEMENTAL(attr_idx)) {
            fprintf(outfile, "ELEMENTAL ");
            len += 10;
         }

         if (ATP_PGM_UNIT(attr_idx) == Function) {
            fprintf(outfile, "FUNCTION ");
            len += 9;
         }
         else {
            fprintf(outfile, "SUBROUTINE ");
            len += 11;
         }

         fprintf(outfile,"%s(", AT_OBJ_NAME_PTR(attr_idx));
         len   += AT_NAME_LEN(attr_idx) + 2;

         if (ATP_EXPL_ITRFC(attr_idx) && ATP_EXTRA_DARG(attr_idx)) {
            num_dargs = ATP_NUM_DARGS(attr_idx) - 1;
            sn_idx    = ATP_FIRST_IDX(attr_idx) + 1;
         }
         else {
            num_dargs = ATP_NUM_DARGS(attr_idx);
            sn_idx    = ATP_FIRST_IDX(attr_idx);
         }

         comma	= " ";

         for (i = num_dargs; i > 0; i--) {
            newlen	= len + AT_NAME_LEN(SN_ATTR_IDX(sn_idx)) + 1;

            if (newlen > 78) {
               fprintf(outfile, "%s &%s    & ", comma, start);
               len = 6 + start_column - 1;
            }
            else {
               fprintf(outfile, "%s", comma);
               len++;
            }

            fprintf(outfile, "%s", AT_OBJ_NAME_PTR(SN_ATTR_IDX(sn_idx)));
            len   += AT_NAME_LEN(SN_ATTR_IDX(sn_idx));
            sn_idx	= sn_idx++;
            comma	= ",";
         }

         fprintf(outfile, ")");

         if (ATP_RSLT_NAME(attr_idx)) {

            if ((len + 10 + AT_NAME_LEN(ATP_RSLT_IDX(attr_idx))) > 80) {
               fprintf(outfile, " &%s    & ", start);
            }
            fprintf(outfile, " RESULT(%s)", 
                             AT_OBJ_NAME_PTR(ATP_RSLT_IDX(attr_idx)));
         }
   
         if (num_dargs > 0) {
            sn_idx    = ATP_EXTRA_DARG(attr_idx) ? ATP_FIRST_IDX(attr_idx) + 1:
                                                   ATP_FIRST_IDX(attr_idx);

            for (i = ATP_NUM_DARGS(attr_idx); i > 0; i--) {
               print_attr_f(SN_ATTR_IDX(sn_idx), outfile);
               sn_idx	= sn_idx++;
            }
         }

         if (ATP_PGM_UNIT(attr_idx) == Function) {
            fprintf(outfile, "%sEND FUNCTION", start);
         }
         else {
            fprintf(outfile, "%sEND SUBROUTINE", start);
         }

         if (ATP_IN_UNNAMED_INTERFACE(attr_idx)) {

           if (save_start_column != start_column) {
              start[--start_column]	= '\0';
              start[--start_column]	= '\0';
           }
           fprintf(outfile, "%sEND INTERFACE", start);
         }
         
         break;

      case Program:
         fprintf(outfile, "%sPROGRAM %s", start, AT_OBJ_NAME_PTR(attr_idx));
         break;

      case Blockdata:
         fprintf(outfile, "%sBLOCKDATA %s", start, AT_OBJ_NAME_PTR(attr_idx));

         break;

      case Module:
         fprintf(outfile, "%sMODULE %s", start, AT_OBJ_NAME_PTR(attr_idx));
         break;
      }

      if (ATP_DCL_EXTERNAL(attr_idx)) {
         fprintf(outfile, "%sEXTERNAL %s", start, AT_OBJ_NAME_PTR(attr_idx));
      }

      if (ATP_STACK_DIR(attr_idx)) {
         fprintf(outfile, "%s!DIR$ STACK", start);
      }

      if (ATP_SAVE_ALL(attr_idx)) {
         fprintf(outfile, "%sSAVE", start);
      }

      if (ATP_SYMMETRIC(attr_idx)) {
         fprintf(outfile, "%s!DIR$ SYMMETRIC", start);
      }

      if (ATP_USES_EREGS(attr_idx)) {
         fprintf(outfile, "%s!DIR$ USES_EREGS", start);
      }
      break;

   case Label:
      break;

   case Derived_Type:
      sn_idx	= ATT_FIRST_CPNT_IDX(attr_idx);
      fprintf(outfile, "%sTYPE :: %s", start, AT_OBJ_NAME_PTR(attr_idx));
      save_start_column	= start_column;

      if (start_column <= 18) {
         start[start_column++]	= ' ';
         start[start_column++]	= ' ';
      }

      for (i = ATT_NUM_CPNTS(attr_idx); i > 0; i--) {
         print_attr_f(SN_ATTR_IDX(sn_idx), outfile);
         sn_idx	= SN_SIBLING_LINK(sn_idx);
      }

      if (save_start_column != start_column) {
         start[--start_column]	= '\0';
         start[--start_column]	= '\0';
      }

      fprintf(outfile, "%sEND TYPE %s", start, AT_OBJ_NAME_PTR(attr_idx));
      break;

   case Interface:
      sn_idx	= ATI_FIRST_SPECIFIC_IDX(attr_idx);
      fprintf(outfile, "%sINTERFACE %s", start,
              (ATI_UNNAMED_INTERFACE(attr_idx) ? " " :
               AT_OBJ_NAME_PTR(attr_idx)));

      save_start_column	= start_column;

      if (start_column <= 18) {
         start[start_column++]	= ' ';
         start[start_column++]	= ' ';
      }


      for (i = ATI_NUM_SPECIFICS(attr_idx); i > 0; i--) {

         if (ATP_PROC(SN_ATTR_IDX(sn_idx)) == Module_Proc) {
            fprintf(outfile, "%sMODULE PROCEDURE %s", start,
                    AT_OBJ_NAME_PTR(SN_ATTR_IDX(sn_idx)));
         }
         else {
            print_attr_f(SN_ATTR_IDX(sn_idx), outfile);
         }
         sn_idx	= SN_SIBLING_LINK(sn_idx);
      }

      if (save_start_column != start_column) {
         start[--start_column]	= '\0';
         start[--start_column]	= '\0';
      }

      fprintf(outfile, "%sEND INTERFACE", start);

      if (ATI_DCL_INTRINSIC(attr_idx)) {
         fprintf(outfile, "%sINTRINSIC :: %s", start, 
                 AT_OBJ_NAME_PTR(attr_idx));
      }

      break;

   case Namelist_Grp:

      sn_idx	= ATN_FIRST_NAMELIST_IDX(attr_idx);
      len	= 80;

      for (i = ATN_NUM_NAMELIST(attr_idx); i > 0; i--) {
         newlen	= len + AT_NAME_LEN(SN_ATTR_IDX(sn_idx)) + 1;

         if (newlen > 80) {
            fprintf(outfile, "%sNAMELIST /%s/ ", start,
                    AT_OBJ_NAME_PTR(attr_idx));
            len = AT_NAME_LEN(attr_idx) + 12 + start_column - 1;
         }
         else {
            fprintf(outfile, "%s", ",");
            len++;
         }

         fprintf(outfile, "%s", AT_OBJ_NAME_PTR(SN_ATTR_IDX(sn_idx)));
         len   += AT_NAME_LEN(SN_ATTR_IDX(sn_idx));
         sn_idx	= SN_SIBLING_LINK(sn_idx);
      }

#     ifdef _DEBUG

      if (ATN_NAMELIST_DESC(attr_idx) != NULL_IDX) {

         if (len > 64) {
            fprintf(outfile, "%s", start);
         }

         fprintf(outfile, "  ! (%s)", 
                          AT_OBJ_NAME_PTR(ATN_NAMELIST_DESC(attr_idx)));
      }
#     endif
      break;

   case Stmt_Func:
      break;

   }

   fflush(outfile);
   return;

}  /* print_attr_f */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Print type in a Fortran format.                                       *|
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

char  *print_type_f(int	 type_idx)

{
		int	kind;
   static	char	str[80];
  	 	char	str1[80];


   if (type_idx == NULL_IDX) {
      sprintf(str, "NULL");
   }
   else if (TYP_TYPE(type_idx) <= Last_Linear_Type) {

      if (TYP_DESC(type_idx) == Star_Typed) {
         sprintf(str, "%s * %d", 
                       basic_type_str[TYP_TYPE(type_idx)],
                       TYP_DCL_VALUE(type_idx));
      }
      else if (TYP_DESC(type_idx) == Kind_Typed) {
         sprintf(str, "%s (kind=%d)", 
                       basic_type_str[TYP_TYPE(type_idx)],
                       TYP_DCL_VALUE(type_idx));
      }
      else {  /* Default Typed */

         /* Print a kind type, so we know exactly what we've got. */

         switch (TYP_LINEAR(type_idx)) {
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
            sprintf(str, "%s", basic_type_str[TYP_TYPE(type_idx)]);
         }
         else {
            sprintf(str, "%s (%d)", basic_type_str[TYP_TYPE(type_idx)], kind);
         }
      }
   }
   else if (TYP_TYPE(type_idx) == Typeless) {
      sprintf(str, "Typeless * %s", 
                    CONVERT_CVAL_TO_STR((&TYP_BIT_LEN(type_idx)),
                                         Integer_8,
                                         str1));
   }
   else if (TYP_TYPE(type_idx) != Character) {
      sprintf(str, "type(%s)", AT_OBJ_NAME_PTR(TYP_IDX(type_idx)));
   }
   else if (TYP_CHAR_CLASS(type_idx) == Assumed_Size_Char) {
      sprintf(str, "CHARACTER*(*)");
   }
   else if (TYP_CHAR_CLASS(type_idx) == Const_Len_Char) {
      sprintf(str, "CHARACTER*(%s)",
              convert_to_string(&CN_CONST(TYP_IDX(type_idx)),
                                 CN_TYPE_IDX(TYP_IDX(type_idx)),
                                 str1));
   }
   else {  /* Variable or unknown length char - print (tmp_idx = idx) */
      sprintf(str, "CHARACTER*(%s)", AT_OBJ_NAME_PTR(TYP_IDX(type_idx)));
   }

   return(str);

} /* print_type_f */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Prints a constant from the constant table.                            *|
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

void  print_const_f(FILE	*outfile,
	           int		 cn_idx)

{
   long64	i;
   long64	length;
   int		type_idx;
   char		str[80];


   type_idx = CN_TYPE_IDX(cn_idx);

   switch (TYP_TYPE(type_idx)) {
   case Typeless:
      convert_to_string_fmt	= Hex_Fmt;
      fprintf(outfile, "0x%s", convert_to_string(&CN_CONST(cn_idx), 
                                                 type_idx,
                                                 str));

      if (TYP_BIT_LEN(type_idx) > TARGET_BITS_PER_WORD) {

         length = (TYP_BIT_LEN(type_idx) + TARGET_BITS_PER_WORD - 1) / 
                                           TARGET_BITS_PER_WORD;
         for (i = 1; i < length; i++) {
            convert_to_string_fmt = Hex_Fmt;
            fprintf(outfile, "  %s",
                    convert_to_string(&CP_CONSTANT(CN_POOL_IDX(cn_idx)+i),
                                      type_idx,
                                      str));
         }
      }

      break;

   case Integer:
      fprintf(outfile, "%s", convert_to_string(&CN_CONST(cn_idx),type_idx,str));
      break;

   case Real:
      fprintf(outfile, "%s", convert_to_string(&CN_CONST(cn_idx),type_idx,str));
      break;

   case Character:
      fprintf(outfile, "\"%s\"", (char *) &CN_CONST(cn_idx));
      break;

   case Logical:
      fprintf(outfile, "%s", (THIS_IS_TRUE(&(CN_CONST(cn_idx)), type_idx) ?
                              ".TRUE." : ".FALSE."));
      break;

   case Complex:
      fprintf(outfile, "%s", convert_to_string(&CN_CONST(cn_idx),type_idx,str));
      break;
   }

   return;

} /* print_const_f */
