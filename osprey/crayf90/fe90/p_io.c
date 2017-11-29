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



static char USMID[] = "\n@(#)5.0_pl/sources/p_io.c	5.3	06/17/99 09:28:10\n";

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
# include "p_io.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "p_globals.h"
# include "p_io.h"

/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/
extern	long	*_fmt_parse(void (**msg_rtn)(), char *, int, long *, boolean *);
	void	 emit_format_msg(int, int, int);
static  int      find_ciitem_idx (io_stmt_type);
static  boolean  parse_io_control_list (opnd_type *, io_stmt_type);
static	int	 pre_parse_format(int, int);
static  int      create_format_tmp (int);


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE                          *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
void parse_backspace_stmt (void)

{
   int        call_idx;
   int        list_idx;
   opnd_type  opnd;
   boolean    parsed_ok		= TRUE;


   TRACE (Func_Entry, "parse_backspace_stmt", NULL);

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   if (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ||
       ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))) {
      PRINTMSG(TOKEN_LINE(token), 1262, Error, TOKEN_COLUMN(token),
               stmt_type_str[stmt_type],
               ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ? "pure" : "elemental",
               AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
   }

   INSERT_IO_START;

   NTR_IR_TBL(call_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = call_idx;

   IR_OPR(call_idx) = Call_Opr;
   IR_TYPE_IDX(call_idx) = TYPELESS_DEFAULT_TYPE;
   IR_COL_NUM(call_idx) = TOKEN_COLUMN(token);
   IR_LINE_NUM(call_idx) = TOKEN_LINE(token);

   /* left child is $BACK attr */

   if (glb_tbl_idx[Backspace_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[Backspace_Attr_Idx] = create_lib_entry_attr(
                                                   BACKSPACE_LIB_ENTRY,
                                                   BACKSPACE_NAME_LEN,
                                                   TOKEN_LINE(token),
                                                   TOKEN_COLUMN(token));
   }

   ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Backspace_Attr_Idx]);
   
   IR_FLD_L(call_idx)      = AT_Tbl_Idx;
   IR_IDX_L(call_idx)      = glb_tbl_idx[Backspace_Attr_Idx];
   IR_LINE_NUM_L(call_idx) = TOKEN_LINE(token);
   IR_COL_NUM_L(call_idx)  = TOKEN_COLUMN(token);

   if (LA_CH_VALUE == LPAREN) {
      parsed_ok = parse_io_control_list(&opnd, Backspace);
      COPY_OPND(IR_OPND_R(call_idx), opnd);
   }
   else {
      /* call parse_expr to get external file unit */
      parsed_ok = parse_expr(&opnd);
      NTR_IR_LIST_TBL(list_idx);
      IL_ARG_DESC_VARIANT(list_idx) = TRUE;
      IR_FLD_R(call_idx) = IL_Tbl_Idx;
      IR_IDX_R(call_idx) = list_idx;
      COPY_OPND(IL_OPND(list_idx), opnd);
      IR_LIST_CNT_R(call_idx) = 3;

      /* add the blank arguments */
      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      IL_ARG_DESC_VARIANT(list_idx) = TRUE;


      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      IL_ARG_DESC_VARIANT(list_idx) = TRUE;
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
      parsed_ok = FALSE;
   }

   matched_specific_token(Tok_EOS, Tok_Class_Punct);

   SH_ERR_FLG(curr_stmt_sh_idx) = ! parsed_ok;

   INSERT_IO_END;

   TRACE (Func_Exit, "parse_backspace_stmt", NULL);

   return;

}  /* parse_backspace_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_buffer_stmt (void)

{
   boolean   buffer_in;
   int       ir_idx;
   int       list1_idx;
   int       list2_idx;
   int       list3_idx;
   int       list4_idx;
   opnd_type opnd;
   boolean   parsed_ok = TRUE;


   TRACE (Func_Entry, "parse_buffer_stmt", NULL);

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   INSERT_IO_START;

   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
   IR_COL_NUM(ir_idx)          = TOKEN_COLUMN(token);
   IR_LINE_NUM(ir_idx)         = TOKEN_LINE(token);

   if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
      
      IR_OPR(ir_idx) = Call_Opr;
      IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;

      if (strcmp(TOKEN_STR(token), "IN") == 0) {

         buffer_in = TRUE;

         if (glb_tbl_idx[Buffer_In_Attr_Idx] == NULL_IDX) {
            glb_tbl_idx[Buffer_In_Attr_Idx] = 
                       create_lib_entry_attr(BUFFER_IN_LIB_ENTRY,
                                             BUFFER_IN_NAME_LEN,
                                             TOKEN_LINE(token),
                                             TOKEN_COLUMN(token));
         }

         ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Buffer_In_Attr_Idx]);

         IR_FLD_L(ir_idx)      = AT_Tbl_Idx;
         IR_IDX_L(ir_idx)      = glb_tbl_idx[Buffer_In_Attr_Idx];
         IR_COL_NUM_L(ir_idx)  = TOKEN_COLUMN(token);
         IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
      }
      else if (strcmp(TOKEN_STR(token), "OUT") == 0) {

         buffer_in = FALSE;

         if (glb_tbl_idx[Buffer_Out_Attr_Idx] == NULL_IDX) {
            glb_tbl_idx[Buffer_Out_Attr_Idx] = 
                       create_lib_entry_attr(BUFFER_OUT_LIB_ENTRY,
                                             BUFFER_OUT_NAME_LEN,
                                             TOKEN_LINE(token),
                                             TOKEN_COLUMN(token));
         }

         ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Buffer_Out_Attr_Idx]);

         IR_FLD_L(ir_idx)      = AT_Tbl_Idx;
         IR_IDX_L(ir_idx)      = glb_tbl_idx[Buffer_Out_Attr_Idx];
         IR_COL_NUM_L(ir_idx)  = TOKEN_COLUMN(token);
         IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
      }
      else {
         parsed_ok = FALSE;
         PRINTMSG(TOKEN_LINE(token), 197, Error, TOKEN_COLUMN(token),
                  "IN or OUT",TOKEN_STR(token));
         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }

      if (LA_CH_VALUE != LPAREN) {
         parse_err_flush(Find_EOS, "(");
         parsed_ok = FALSE;
         goto EXIT;
      }

      if (cif_flags & MISC_RECS) {
         cif_stmt_type_rec(TRUE,
                           (buffer_in) ?
                              CIF_Buffer_In_Stmt : CIF_Buffer_Out_Stmt, 
                           statement_number);
      }

      NEXT_LA_CH;

      NTR_IR_LIST_TBL(list1_idx);
      NTR_IR_LIST_TBL(list2_idx);
      NTR_IR_LIST_TBL(list3_idx);
      NTR_IR_LIST_TBL(list4_idx);
      IR_FLD_R(ir_idx)            = IL_Tbl_Idx;
      IR_LIST_CNT_R(ir_idx)       = 4;
      IR_IDX_R(ir_idx)            = list1_idx;
      IL_NEXT_LIST_IDX(list1_idx) = list2_idx;
      IL_NEXT_LIST_IDX(list2_idx) = list3_idx;
      IL_NEXT_LIST_IDX(list3_idx) = list4_idx;

      IL_ARG_DESC_VARIANT(list1_idx) = TRUE;
      IL_ARG_DESC_VARIANT(list2_idx) = TRUE;
      IL_ARG_DESC_VARIANT(list3_idx) = TRUE;
      IL_ARG_DESC_VARIANT(list4_idx) = TRUE;

      parsed_ok = parse_expr(&opnd) && parsed_ok;
      COPY_OPND(IL_OPND(list1_idx), opnd);

      if (LA_CH_VALUE != COMMA) {
         parse_err_flush(Find_EOS, ",");
         parsed_ok = FALSE;
         goto EXIT;
      }

      NEXT_LA_CH;
    
      parsed_ok = parse_expr(&opnd) && parsed_ok;
      COPY_OPND(IL_OPND(list2_idx), opnd);

      if (LA_CH_VALUE != RPAREN) {
         parse_err_flush(Find_EOS, ")");
         parsed_ok = FALSE;
         goto EXIT;
      }

      NEXT_LA_CH;

      if (LA_CH_VALUE != LPAREN) {
         parse_err_flush(Find_EOS, "(");
         parsed_ok = FALSE;
         goto EXIT;
      }

      NEXT_LA_CH;

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         parsed_ok = parse_deref(&opnd, NULL_IDX) && parsed_ok;
         COPY_OPND(IL_OPND(list3_idx), opnd);

         if (buffer_in) {
            mark_attr_defined(&opnd);
         }
      }
      else {
         parse_err_flush(Find_EOS, "IDENTIFIER");
         parsed_ok = FALSE;
         goto EXIT;
      }

      if (LA_CH_VALUE != COMMA) {
         parse_err_flush(Find_EOS, ",");
         parsed_ok = FALSE;
         goto EXIT;
      }

      NEXT_LA_CH;

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         parsed_ok = parse_deref(&opnd, NULL_IDX) && parsed_ok;
         COPY_OPND(IL_OPND(list4_idx), opnd);

         if (buffer_in) {
            mark_attr_defined(&opnd);
         }
      }
      else {
         parse_err_flush(Find_EOS, "IDENTIFIER");
         parsed_ok = FALSE;
         goto EXIT;
      }

      if (LA_CH_VALUE != RPAREN) {
         parse_err_flush(Find_EOS, ")");
         parsed_ok = FALSE;
      }
      else {
         NEXT_LA_CH;
      }
   }
   else {
      parse_err_flush(Find_EOS, "IN or OUT");
      parsed_ok = FALSE;
   }

EXIT:

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
      parsed_ok = FALSE;
   }

   NEXT_LA_CH;

   INSERT_IO_END;

   TRACE (Func_Exit, "parse_buffer_stmt", NULL);

   return;

}  /* parse_buffer_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_close_stmt (void)

{
   int        call_idx;
   opnd_type  opnd;
   boolean    parsed_ok		= TRUE;


   TRACE (Func_Entry, "parse_close_stmt", NULL);

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   if (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ||
       ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))) {
      PRINTMSG(TOKEN_LINE(token), 1262, Error, TOKEN_COLUMN(token),
               stmt_type_str[stmt_type],
               ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ? "pure" : "elemental",
               AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
   }

   INSERT_IO_START;

   NTR_IR_TBL(call_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = call_idx;

   IR_OPR(call_idx) = Call_Opr;
   IR_TYPE_IDX(call_idx) = TYPELESS_DEFAULT_TYPE;
   IR_COL_NUM(call_idx) = TOKEN_COLUMN(token);
   IR_LINE_NUM(call_idx) = TOKEN_LINE(token);

   /* left child is $CLS attr */

   if (glb_tbl_idx[Close_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[Close_Attr_Idx] = create_lib_entry_attr(CLOSE_LIB_ENTRY,
                                                          CLOSE_NAME_LEN,
                                                          TOKEN_LINE(token),
                                                          TOKEN_COLUMN(token));
   }

   ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Close_Attr_Idx]);

   IR_FLD_L(call_idx)      = AT_Tbl_Idx;
   IR_IDX_L(call_idx)      = glb_tbl_idx[Close_Attr_Idx];
   IR_LINE_NUM_L(call_idx) = TOKEN_LINE(token);
   IR_COL_NUM_L(call_idx)  = TOKEN_COLUMN(token);

   parsed_ok = parse_io_control_list(&opnd, Close);
   COPY_OPND(IR_OPND_R(call_idx), opnd);

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
      parsed_ok = FALSE;
   }

   matched_specific_token(Tok_EOS, Tok_Class_Punct);

   SH_ERR_FLG(curr_stmt_sh_idx) = ! parsed_ok;

   INSERT_IO_END;

   TRACE (Func_Exit, "parse_close_stmt", NULL);

   return;

}  /* parse_close_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_decode_stmt (void)

{

   /*     This is legal code.                                                 */
   /*     character*8 old_char(10)					      */
   /*     encode(8, 10, char) (old_char(i),i=1,8)			      */
   /* 10     format(a)							      */
   /*     end								      */

   int       attr_idx;
   int       buf_idx;
   int       column;
   int	     idx;
   int       ir_idx;
   int       line;
   int	     list_idx;
   int       list1_idx;
   int       list2_idx;
   int       list3_idx;
   int       name_idx;
   opnd_type opnd;
   boolean   parsed_ok			= TRUE;
   int	     pre_parse_format_idx;


   TRACE (Func_Entry, "parse_decode_stmt", NULL);

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   INSERT_IO_START;

   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;

   IR_OPR(ir_idx)              = Read_Formatted_Opr;
   IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
   column                      = TOKEN_COLUMN(token);
   IR_COL_NUM(ir_idx)          = column;
   line                        = TOKEN_LINE(token);
   IR_LINE_NUM(ir_idx)         = line;

   if (LA_CH_VALUE == LPAREN) {
      IR_FLD_L(ir_idx) = IL_Tbl_Idx;
      IR_LIST_CNT_L(ir_idx) = 3;
      NTR_IR_LIST_TBL(list1_idx);
      NTR_IR_LIST_TBL(list2_idx);
      NTR_IR_LIST_TBL(list3_idx);
      IR_IDX_L(ir_idx) = list1_idx;
      IL_NEXT_LIST_IDX(list1_idx) = list2_idx;
      IL_NEXT_LIST_IDX(list2_idx) = list3_idx;
      IL_PREV_LIST_IDX(list2_idx) = list1_idx;
      IL_PREV_LIST_IDX(list3_idx) = list2_idx;

      NEXT_LA_CH;

      parsed_ok = parse_expr(&opnd);
      COPY_OPND(IL_OPND(list1_idx), opnd);

      if (LA_CH_VALUE != COMMA) {
         parse_err_flush(Find_Rparen, ",");
         parsed_ok = FALSE;
      }
      else {

         NEXT_LA_CH;

         buf_idx = LA_CH_BUF_IDX;

         if (LA_CH_CLASS == Ch_Class_Digit &&
             digit_is_format_label())      {

            if (MATCHED_TOKEN_CLASS(Tok_Class_Label) &&
                ! TOKEN_ERR(token)) {

               attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                       &name_idx);

               if (attr_idx == NULL_IDX) {
                  attr_idx               = ntr_sym_tbl(&token, name_idx);
                  AT_OBJ_CLASS(attr_idx) = Label;
                  LN_DEF_LOC(name_idx)   = TRUE;
                  build_fwd_ref_entry(attr_idx, Format_Ref);
               }
               else if ( ! AT_DCL_ERR(attr_idx) ) {

                    if (!AT_DEFINED(attr_idx)) {
                       build_fwd_ref_entry(attr_idx, Format_Ref);
                    }
                    else if (ATL_CLASS(attr_idx) != Lbl_Format) {
                    /* error .. label used previously as something else */
                       PRINTMSG(TOKEN_LINE(token), 328, Error, 
                                TOKEN_COLUMN(token), 
                                AT_OBJ_NAME_PTR(attr_idx));
                       parsed_ok = FALSE;
                    }

               }
               else {
                  /* no message , at_dcl_err is set */
                  parsed_ok = FALSE;
               }

               IL_FLD(list2_idx)      = AT_Tbl_Idx;
               IL_IDX(list2_idx)      = attr_idx;
               IL_LINE_NUM(list2_idx) = TOKEN_LINE(token);
               IL_COL_NUM(list2_idx)  = TOKEN_COLUMN(token);

               if (cif_flags & XREF_RECS) {
                  cif_usage_rec(attr_idx, AT_Tbl_Idx,
				TOKEN_LINE(token), TOKEN_COLUMN(token),
				CIF_Label_Referenced_As_Format);
               }

            }
            else if (TOKEN_ERR(token)) {
               parse_err_flush(Find_Comma, NULL);
               parsed_ok = FALSE;
            }
            else {
               parse_err_flush(Find_Comma, "LABEL");
               parsed_ok = FALSE;
            }
         }
         else {
            parsed_ok = parse_expr(&opnd) && parsed_ok;
            COPY_OPND(IL_OPND(list2_idx), opnd);
         }

         if (parsed_ok                                     &&
             IL_FLD(list2_idx)               == CN_Tbl_Idx &&
             TYP_TYPE(CN_TYPE_IDX(IL_IDX(list2_idx))) == Character) {

            /* preparse format constant */
            set_format_start_idx(buf_idx);

            format_cn_idx = IL_IDX(list2_idx);
            ignore_trailing_chars = TRUE;
            pre_parse_format_idx  = pre_parse_format(format_cn_idx, 0);
            ignore_trailing_chars = FALSE;

            NTR_IR_LIST_TBL(list_idx);
            IL_FLD(list2_idx)      = IL_Tbl_Idx;
            IL_IDX(list2_idx)      = list_idx;
            IL_LIST_CNT(list2_idx) = 2;

            IL_FLD(list_idx) = AT_Tbl_Idx;
            idx = create_format_tmp(format_cn_idx);
            IL_IDX(list_idx) = idx;
            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx)  = column;

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
            list_idx = IL_NEXT_LIST_IDX(list_idx);

            if (pre_parse_format_idx != NULL_IDX) {
               IL_FLD(list_idx) = AT_Tbl_Idx;
               idx = create_format_tmp(pre_parse_format_idx);
               IL_IDX(list_idx) = idx;
               IL_LINE_NUM(list_idx) = line;
               IL_COL_NUM(list_idx)  = column;
            }
         }

         if (LA_CH_VALUE != COMMA) {
            parse_err_flush(Find_Rparen, ",");
            parsed_ok = FALSE;
         }
         else {
            NEXT_LA_CH;

            if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
               parsed_ok = parse_deref(&opnd, NULL_IDX) && parsed_ok;
               COPY_OPND(IL_OPND(list3_idx), opnd);
            }
            else {
               parse_err_flush(Find_Rparen, "IDENTIFIER");
               parsed_ok = FALSE;
            }
         }

         if (LA_CH_VALUE != RPAREN) {

            if (parse_err_flush(Find_Rparen, ")")) {
               NEXT_LA_CH;
            }
            parsed_ok = FALSE;
         }
         else {
            NEXT_LA_CH;
         }
      }

      if (LA_CH_VALUE != EOS) {

         parsed_ok = parse_io_list(&opnd) && parsed_ok;
         COPY_OPND(IR_OPND_R(ir_idx), opnd);
      }
   }
   else {
      parse_err_flush(Find_EOS, "(");
      parsed_ok = FALSE;
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
      parsed_ok = FALSE;
   }

   matched_specific_token(Tok_EOS, Tok_Class_Punct);

   INSERT_IO_END;

   TRACE (Func_Exit, "parse_decode_stmt", NULL);

   return;

}  /* parse_decode_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_encode_stmt (void)

{
   int       attr_idx;
   int       buf_idx;
   int       column;
   int	     idx;
   int       ir_idx;
   int       line;
   int	     list_idx;
   int       list1_idx;
   int       list2_idx;
   int       list3_idx;
   int       name_idx;
   opnd_type opnd;
   boolean   parsed_ok			= TRUE;
   int	     pre_parse_format_idx;


   TRACE (Func_Entry, "parse_encode_stmt", NULL);

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   INSERT_IO_START;

   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;

   IR_OPR(ir_idx)              = Write_Formatted_Opr;
   IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
   column                      = TOKEN_COLUMN(token);
   IR_COL_NUM(ir_idx)          = column;
   line                        = TOKEN_LINE(token);
   IR_LINE_NUM(ir_idx)         = line;

   if (LA_CH_VALUE == LPAREN) {
      IR_FLD_L(ir_idx) = IL_Tbl_Idx;
      IR_LIST_CNT_L(ir_idx) = 3;
      NTR_IR_LIST_TBL(list1_idx);
      NTR_IR_LIST_TBL(list2_idx);
      NTR_IR_LIST_TBL(list3_idx);
      IR_IDX_L(ir_idx) = list1_idx;
      IL_NEXT_LIST_IDX(list1_idx) = list2_idx;
      IL_NEXT_LIST_IDX(list2_idx) = list3_idx;
      IL_PREV_LIST_IDX(list2_idx) = list1_idx;
      IL_PREV_LIST_IDX(list3_idx) = list2_idx;

      NEXT_LA_CH;
      
      parsed_ok = parse_expr(&opnd);
      COPY_OPND(IL_OPND(list1_idx), opnd);

      if (LA_CH_VALUE != COMMA) {
         parse_err_flush(Find_Rparen, ",");
         parsed_ok = FALSE;
      }
      else {

         NEXT_LA_CH;

         buf_idx = LA_CH_BUF_IDX;

         if (LA_CH_CLASS == Ch_Class_Digit &&
             digit_is_format_label())      {

            if (MATCHED_TOKEN_CLASS(Tok_Class_Label) &&
                ! TOKEN_ERR(token)) {

               attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                       &name_idx);

               if (attr_idx == NULL_IDX) {
                  attr_idx               = ntr_sym_tbl(&token, name_idx);
                  AT_OBJ_CLASS(attr_idx) = Label;
                  LN_DEF_LOC(name_idx)   = TRUE;
                  build_fwd_ref_entry(attr_idx, Format_Ref);
               }
               else if ( ! AT_DCL_ERR(attr_idx) ) {
   
                    if (!AT_DEFINED(attr_idx)) {
                       build_fwd_ref_entry(attr_idx, Format_Ref);
                    }
                    else if (ATL_CLASS(attr_idx) != Lbl_Format) {
                    /* error .. label used previously as something else */
                       PRINTMSG(TOKEN_LINE(token), 328, Error,
                                TOKEN_COLUMN(token),
                                AT_OBJ_NAME_PTR(attr_idx));
                       parsed_ok = FALSE;
                    }

               }
               else {
                  /* no message, at_dcl_err is set */
                  parsed_ok = FALSE;
               }

               IL_FLD(list2_idx)      = AT_Tbl_Idx;
               IL_IDX(list2_idx)      = attr_idx;
               IL_LINE_NUM(list2_idx) = TOKEN_LINE(token);
               IL_COL_NUM(list2_idx)  = TOKEN_COLUMN(token);

               if (cif_flags & XREF_RECS) {
                  cif_usage_rec(attr_idx, AT_Tbl_Idx,
				TOKEN_LINE(token), TOKEN_COLUMN(token),
				CIF_Label_Referenced_As_Format);
               }
            }
            else if (TOKEN_ERR(token)) {
               parse_err_flush(Find_Comma, NULL);
               parsed_ok = FALSE;
            }
            else {
               parse_err_flush(Find_Comma, "LABEL");
               parsed_ok = FALSE;
            }
         }
         else {
            parsed_ok = parse_expr(&opnd) && parsed_ok;
            COPY_OPND(IL_OPND(list2_idx), opnd);
         }

         if (parsed_ok                                     &&
             IL_FLD(list2_idx)               == CN_Tbl_Idx &&
             TYP_TYPE(CN_TYPE_IDX(IL_IDX(list2_idx))) == Character) {

            /* preparse format constant */
            set_format_start_idx(buf_idx);

            format_cn_idx = IL_IDX(list2_idx);
            ignore_trailing_chars = TRUE;
            pre_parse_format_idx  = pre_parse_format(format_cn_idx, 0);
            ignore_trailing_chars = FALSE;

            NTR_IR_LIST_TBL(list_idx);
            IL_FLD(list2_idx)      = IL_Tbl_Idx;
            IL_IDX(list2_idx)      = list_idx;
            IL_LIST_CNT(list2_idx) = 2;

            IL_FLD(list_idx) = AT_Tbl_Idx;
            idx = create_format_tmp(format_cn_idx);
            IL_IDX(list_idx) = idx;
            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx)  = column;

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
            list_idx = IL_NEXT_LIST_IDX(list_idx);

            if (pre_parse_format_idx != NULL_IDX) {
               IL_FLD(list_idx) = AT_Tbl_Idx;
               idx = create_format_tmp(pre_parse_format_idx);
               IL_IDX(list_idx) = idx;
               IL_LINE_NUM(list_idx) = line;
               IL_COL_NUM(list_idx)  = column;
            }
         }

         if (LA_CH_VALUE != COMMA) {
            parse_err_flush(Find_Rparen, ",");
            parsed_ok = FALSE;
         }
         else {
            NEXT_LA_CH;

            if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
               parsed_ok = parse_deref(&opnd, NULL_IDX) && parsed_ok;
               COPY_OPND(IL_OPND(list3_idx), opnd);

               mark_attr_defined(&opnd);
            }
            else {
               parse_err_flush(Find_Rparen, "IDENTIFIER");
               parsed_ok = FALSE;
            }
         }
        
         if (LA_CH_VALUE != RPAREN) {

            if (parse_err_flush(Find_Rparen, ")")) {
               NEXT_LA_CH;
            }
            parsed_ok = FALSE;
         }
         else {
            NEXT_LA_CH;
         }
      }

      if (LA_CH_VALUE != EOS) {

         parsed_ok = parse_io_list(&opnd) && parsed_ok;
         COPY_OPND(IR_OPND_R(ir_idx), opnd);
      }
   }
   else {
      parse_err_flush(Find_EOS, "(");
      parsed_ok = FALSE;
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
      parsed_ok = FALSE;
   }

   matched_specific_token(Tok_EOS, Tok_Class_Punct);

   INSERT_IO_END;

   TRACE (Func_Exit, "parse_encode_stmt", NULL);

   return;

}  /* parse_encode_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parses the ENDFILE statement.   At entry the TOKEN is FILE.           *|
|*	Called from parse_end_stmt.                                           *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_endfile_stmt (void)

{
   int        call_idx;
   int        list_idx;
   opnd_type  opnd;
   boolean    parsed_ok		= TRUE;


   TRACE (Func_Entry, "parse_endfile_stmt", NULL);

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   if (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ||
       ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))) {
      PRINTMSG(TOKEN_LINE(token), 1262, Error, TOKEN_COLUMN(token),
               stmt_type_str[stmt_type],
               ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ? "pure" : "elemental",
               AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
   }

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_Endfile_Stmt, statement_number);
   }

   INSERT_IO_START;

   NTR_IR_TBL(call_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = call_idx;

   IR_OPR(call_idx) = Call_Opr;
   IR_TYPE_IDX(call_idx) = TYPELESS_DEFAULT_TYPE;
   IR_COL_NUM(call_idx) = TOKEN_COLUMN(token);
   IR_LINE_NUM(call_idx) = TOKEN_LINE(token);

   /* left child is endfile attr */

   if (glb_tbl_idx[Endfile_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[Endfile_Attr_Idx] = create_lib_entry_attr(ENDFILE_LIB_ENTRY,
                                                           ENDFILE_NAME_LEN,
                                                           TOKEN_LINE(token),
                                                           TOKEN_COLUMN(token));
   }

   ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Endfile_Attr_Idx]);

   IR_FLD_L(call_idx)      = AT_Tbl_Idx;
   IR_IDX_L(call_idx)      = glb_tbl_idx[Endfile_Attr_Idx];

   IR_LINE_NUM_L(call_idx) = TOKEN_LINE(token);
   IR_COL_NUM_L(call_idx)  = TOKEN_COLUMN(token);

   if (LA_CH_VALUE == LPAREN) {
      parsed_ok = parse_io_control_list(&opnd, Endfile);
      COPY_OPND(IR_OPND_R(call_idx), opnd);
   }
   else {
      /* call parse_expr to get external file unit */
      parsed_ok = parse_expr(&opnd);
      NTR_IR_LIST_TBL(list_idx);
      IL_ARG_DESC_VARIANT(list_idx) = TRUE;
      IR_FLD_R(call_idx)      = IL_Tbl_Idx;
      IR_IDX_R(call_idx)      = list_idx;
      COPY_OPND(IL_OPND(list_idx), opnd);
      IR_LIST_CNT_R(call_idx) = 3;

      /* add the blank arguments */
      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      IL_ARG_DESC_VARIANT(list_idx) = TRUE;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      IL_ARG_DESC_VARIANT(list_idx) = TRUE;
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
      parsed_ok = FALSE;
   }

   matched_specific_token(Tok_EOS, Tok_Class_Punct);

   SH_ERR_FLG(curr_stmt_sh_idx) = ! parsed_ok;

   INSERT_IO_END;

   TRACE (Func_Exit, "parse_endfile_stmt", NULL);

   return;

}  /* parse_endfile_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parse FORMAT statement.                      		 	      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_format_stmt (void)

{
   int		pre_parse_format_idx;
   int		tmp_idx;


   TRACE (Func_Entry, "parse_format_stmt", NULL);

   if (LA_CH_VALUE == LPAREN) {

      if (CURR_BLK_NO_EXEC && iss_blk_stk_err()) {

         /* FORMAT statements are only allowed in contexts  */
         /*        where executable statements are allowed. */

         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }

      if (curr_stmt_category < Implicit_None_Stmt_Cat) {
         curr_stmt_category = Implicit_None_Stmt_Cat;
      }

      if (stmt_label_idx == NULL_IDX) {
         PRINTMSG(TOKEN_LINE(token), 135, Error, TOKEN_COLUMN(token));
         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }

      ATL_CLASS(stmt_label_idx)	= Lbl_Format;

      if (MATCHED_TOKEN_CLASS(Tok_Class_Format_Str)) {
         set_format_start_idx(TOKEN_BUF_IDX(token) - 1);
         format_cn_idx = TOKEN_CONST_TBL_IDX(token);

         pre_parse_format_idx = pre_parse_format(format_cn_idx,
                                                 AT_NAME_LEN(stmt_label_idx));

         tmp_idx = create_format_tmp(format_cn_idx);

         ATL_FORMAT_TMP(stmt_label_idx) = tmp_idx;

         if (pre_parse_format_idx != NULL_IDX) {
            tmp_idx = create_format_tmp(pre_parse_format_idx);
            ATL_PP_FORMAT_TMP(stmt_label_idx) = tmp_idx;
         }
         else {
            ATL_PP_FORMAT_TMP(stmt_label_idx) = NULL_IDX;
         }

         if (LA_CH_VALUE != EOS) {
            PRINTMSG(LA_CH_LINE, 166, Error, LA_CH_COLUMN);
            parse_err_flush(Find_EOS, NULL);
         }
      }
      else {
         /* ERROR */
      }
   }
   else {		/* Not FORMAT(... */
      parse_err_flush(Find_EOS, "(");
   }
EXIT:

   matched_specific_token(Tok_EOS, Tok_Class_Punct);
   TRACE (Func_Exit, "parse_format_stmt", NULL);

   return;

}  /* parse_format_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine is called directly by the format parser to print out     *|
|*	format messages.  (This includes ANSI's).                             *|
|*									      *|
|* Input parameters:							      *|
|*	msg_num	-> Msg number to print out.  (This is the format parser msg   *|
|*	           number.  It is translated to a cft90 msg, using the        *|
|*                 msg_num_tbl in p_io.h.                                     *|
|*	col     -> Column number where msg is found.                          *|
|*	ed_col  -> Column number where edit descriptor is found.              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
void emit_format_msg(int	msg_num,
		     int	column,
		     int	ed_column)

{
   int	line;
   char	ch;
   int	col;
   int	ed_idx;


   switch (msg_num) {
      case TRAILING_CHARS:

         if (ignore_trailing_chars) {
            goto EXIT;
         }

         format_line_n_col(&line, &col, ed_column);
         ed_idx = column;
         break;

      case ANSI_EMPTY_PAREN_MSG:
      case MINUS_X_NON_ANSI:
      case H_IS_OBSOLETE_IN_F90:
      case EXPECTING_RIGHT_PAREN:
      case NON_ANSI_NULL_DESCRIPTOR:
      case E_WITH_D_NON_ANSI:
     
         format_line_n_col(&line, &col, ed_column);
         ed_idx = column;
         break;

      case REP_SLASH_NON_ANSI: 

         /* This is rep count on slash is not standard message. Does not */
         /* apply to cft90.                                              */

         goto EXIT;

      case MISSING_WIDTH_NON_ANSI:
      case ZERO_WIDTH_NON_ANSI:
         format_line_n_col(&line, &col, ed_column);
         ed_idx = ed_column;
         break;

      case NON_ANSI_EDIT_DESCRIPTOR:
         format_line_n_col(&line, &col, ed_column);
         ed_idx = ed_column;

         if (stmt_type == Format_Stmt) {
            ch = ((char *)&CN_CONST(format_cn_idx) +
                                    AT_NAME_LEN(stmt_label_idx))[ed_idx - 1];
         }
         else {
            ch = ((char *)&CN_CONST(format_cn_idx))[ed_idx - 1];
         }

         switch (ch) {
            case '*':
            case '$':
            case 'R':
            case 'r':
            case 'X':
            case 'x':
#ifdef KEY /* Bug 318, 321 */
            case 'H':
            case 'h':
#endif /* KEY Bug 318, 321 */
               break;

            default:
               goto EXIT;       /* B, O, Z, ", R are standard in Fortran 90 */

         }  /* End switch */

         break;

      case INVALID_REP_COUNT:
         format_line_n_col(&line, &col, column);
         ed_idx = ed_column;
         break;

      default :
         format_line_n_col(&line, &col, column);
         ed_idx = column;
         break;
   }

   switch (msg_num_tbl[msg_num].num_args) {
      case 0:
         PRINTMSG(line,
                  msg_num_tbl[msg_num].msg_num, 
                  msg_num_tbl[msg_num].msg_severity,
                  col);
         break;
   
      case 1:
         if (stmt_type == Format_Stmt) {
            PRINTMSG(line,
                     msg_num_tbl[msg_num].msg_num,
                     msg_num_tbl[msg_num].msg_severity,
                     col,
                     ((char *)&CN_CONST(format_cn_idx))
                                  [AT_NAME_LEN(stmt_label_idx) + ed_idx - 1]); 
         }
         else {
            PRINTMSG(line,
                     msg_num_tbl[msg_num].msg_num,
                     msg_num_tbl[msg_num].msg_severity,
                     col,
                     ((char *)&CN_CONST(format_cn_idx))[ed_idx - 1]); 
         }
         break;
   }

EXIT:

   return;
}


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_inquire_stmt (void)

{
   int        buf_idx;
   int        call_idx;
   int        list_idx;
   opnd_type  opnd;
   boolean    parsed_ok = TRUE;
   int        stmt_num;


   TRACE (Func_Entry, "parse_inquire_stmt", NULL);

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   if (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ||
       ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))) {
      PRINTMSG(TOKEN_LINE(token), 1262, Error, TOKEN_COLUMN(token),
               stmt_type_str[stmt_type],
               ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ? "pure" : "elemental",
               AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
   }

   INSERT_IO_START;

   NTR_IR_TBL(call_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = call_idx;

   IR_COL_NUM(call_idx) = TOKEN_COLUMN(token);
   IR_LINE_NUM(call_idx) = TOKEN_LINE(token);
   IR_TYPE_IDX(call_idx) = TYPELESS_DEFAULT_TYPE;

   IR_LINE_NUM_L(call_idx) = TOKEN_LINE(token);
   IR_COL_NUM_L(call_idx)  = TOKEN_COLUMN(token);

   if (LA_CH_VALUE == LPAREN) {
      buf_idx = LA_CH_BUF_IDX;
      stmt_num = LA_CH_STMT_NUM;
      NEXT_LA_CH;

      if (LA_CH_VALUE == 'I'                      &&
         MATCHED_TOKEN_CLASS(Tok_Class_Id)        &&
         strcmp(TOKEN_STR(token),"IOLENGTH") == 0 &&
         LA_CH_VALUE == EQUAL)                    {

         IR_OPR(call_idx) = Inquire_Iolength_Opr;

         NEXT_LA_CH;
         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_L(call_idx) = IL_Tbl_Idx;
         IR_IDX_L(call_idx) = list_idx;
         IR_LIST_CNT_L(call_idx) = 1;
        
         if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
            parsed_ok = parse_deref(&opnd, NULL_IDX);
            COPY_OPND(IL_OPND(list_idx), opnd);

            mark_attr_defined(&opnd);
         }
         else {
            parse_err_flush(Find_Rparen, "IDENTIFIER");
            parsed_ok = FALSE;
         }
        
         if (LA_CH_VALUE == RPAREN) {
            NEXT_LA_CH;
            if (LA_CH_VALUE != EOS) {
               parsed_ok = parse_io_list(&opnd) && parsed_ok;
               COPY_OPND(IR_OPND_R(call_idx), opnd);
            }
         }
         else {
            parse_err_flush(Find_EOS, ")");
            parsed_ok = FALSE;
         }
      }   
      else {
         reset_lex(buf_idx, stmt_num);

         IR_OPR(call_idx) = Call_Opr;

         /* left child is inquire attr */

         if (glb_tbl_idx[Inquire_Attr_Idx] == NULL_IDX) {
            glb_tbl_idx[Inquire_Attr_Idx] = create_lib_entry_attr(
                                                       INQUIRE_LIB_ENTRY,
                                                       INQUIRE_NAME_LEN,
                                                       TOKEN_LINE(token),
                                                       TOKEN_COLUMN(token));
         }

         ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Inquire_Attr_Idx]);

         IR_FLD_L(call_idx)      = AT_Tbl_Idx;
         IR_IDX_L(call_idx)      = glb_tbl_idx[Inquire_Attr_Idx];
         IR_LINE_NUM_L(call_idx) = IR_LINE_NUM(call_idx);
         IR_COL_NUM_L(call_idx)  = IR_COL_NUM(call_idx);
     
         parsed_ok = parse_io_control_list(&opnd, Inquire);
         COPY_OPND(IR_OPND_R(call_idx), opnd);
      }
   }
   else {
      parse_err_flush(Find_EOS, "(");
      parsed_ok = FALSE;
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
      parsed_ok = FALSE;
   }

   matched_specific_token(Tok_EOS, Tok_Class_Punct);

   SH_ERR_FLG(curr_stmt_sh_idx) = ! parsed_ok;

   INSERT_IO_END;

   TRACE (Func_Exit, "parse_inquire_stmt", NULL);

   return;

}  /* parse_inquire_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_open_stmt (void)

{
   int        call_idx;
   opnd_type  opnd;
   boolean    parsed_ok = TRUE;


   TRACE (Func_Entry, "parse_open_stmt", NULL);

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   if (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ||
       ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))) {
      PRINTMSG(TOKEN_LINE(token), 1262, Error, TOKEN_COLUMN(token),
               stmt_type_str[stmt_type],
               ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ? "pure" : "elemental",
               AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
   }

   INSERT_IO_START;

   NTR_IR_TBL(call_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = call_idx;

   IR_OPR(call_idx) = Call_Opr;
   IR_TYPE_IDX(call_idx) = TYPELESS_DEFAULT_TYPE;
   IR_COL_NUM(call_idx) = TOKEN_COLUMN(token);
   IR_LINE_NUM(call_idx) = TOKEN_LINE(token);

   /* left child is $OPN attr */

   if (glb_tbl_idx[Open_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[Open_Attr_Idx] = create_lib_entry_attr(OPEN_LIB_ENTRY,
                                                         OPEN_NAME_LEN,
                                                         TOKEN_LINE(token),
                                                         TOKEN_COLUMN(token));
   }

   ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Open_Attr_Idx]);

   IR_FLD_L(call_idx)      = AT_Tbl_Idx;
   IR_IDX_L(call_idx)      = glb_tbl_idx[Open_Attr_Idx];
   IR_COL_NUM_L(call_idx)  = TOKEN_COLUMN(token);
   IR_LINE_NUM_L(call_idx) = TOKEN_LINE(token);

   parsed_ok = parse_io_control_list(&opnd, Open);
   COPY_OPND(IR_OPND_R(call_idx), opnd);

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
      parsed_ok = FALSE;
   }

   matched_specific_token(Tok_EOS, Tok_Class_Punct);

   SH_ERR_FLG(curr_stmt_sh_idx) = ! parsed_ok;

   INSERT_IO_END;

   TRACE (Func_Exit, "parse_open_stmt", NULL);

   return;

}  /* parse_open_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_print_stmt (void)

{
   int       attr_idx;
   int       buf_idx;
   int       column;
   int       i;
   int	     idx;
   int       ir_idx;
   int       line;
   int       list_idx;
   int       list2_idx;
   int       name_idx;
   opnd_type opnd;
   boolean   parsed_ok = TRUE;
   int	     pre_parse_format_idx;


   TRACE (Func_Entry, "parse_print_stmt", NULL);

   /* The following is legal - print a(1)(1:2), "ab"   ! a is char array   */
   /* The following is also legal - print a(1)(1:2) = ! where reada is arr */

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   if (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ||
       ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))) {
      PRINTMSG(TOKEN_LINE(token), 1262, Error, TOKEN_COLUMN(token),
               stmt_type_str[stmt_type],
               ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ? "pure" : "elemental",
               AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
   }

   INSERT_IO_START;

   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;

   IR_OPR(ir_idx)              = Write_Formatted_Opr;
   IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
   column                      = TOKEN_COLUMN(token);
   IR_COL_NUM(ir_idx)          = column;
   line                        = TOKEN_LINE(token);
   IR_LINE_NUM(ir_idx)         = line;

   IR_FLD_L(ir_idx)            = IL_Tbl_Idx;
   IR_LIST_CNT_L(ir_idx)       = ciitem_tbl[Write].num_ciitems;
   NTR_IR_LIST_TBL(list_idx);
   IR_IDX_L(ir_idx)            = list_idx;

   /* set first one to CN_Tbl_Idx for default unit */
   IL_FLD(list_idx)            = CN_Tbl_Idx;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = column;

   for (i = 2; i <= ciitem_tbl[Write].num_ciitems; i++) {
      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   list_idx = IL_NEXT_LIST_IDX(IR_IDX_L(ir_idx));

   if (LA_CH_VALUE == STAR) {
      /* CN_Tbl_Idx and idx = NULL is list directed */
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = column;
      NEXT_LA_CH;
   }
   else if (LA_CH_CLASS == Ch_Class_Digit &&
            digit_is_format_label())      {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Label) &&
          ! TOKEN_ERR(token)) {

         attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx               = ntr_sym_tbl(&token, name_idx);
            AT_OBJ_CLASS(attr_idx) = Label;
            LN_DEF_LOC(name_idx)   = TRUE;
            build_fwd_ref_entry(attr_idx, Format_Ref);
         }
         else if ( ! AT_DCL_ERR(attr_idx) ) {

              if (!AT_DEFINED(attr_idx)) {
                 build_fwd_ref_entry(attr_idx, Format_Ref);
              }
              else if (ATL_CLASS(attr_idx) != Lbl_Format) {
              /* error .. label used previously as something else */
                 PRINTMSG(TOKEN_LINE(token), 328, Error,
                          TOKEN_COLUMN(token),
                          AT_OBJ_NAME_PTR(attr_idx));
                 parsed_ok = FALSE;
              }

         }
         else {
            /* no message, at_dcl_err is set */
            parsed_ok = FALSE;
         }
         
         if (parsed_ok) {
            IL_FLD(list_idx)      = AT_Tbl_Idx;
            IL_IDX(list_idx)      = attr_idx;
            IL_LINE_NUM(list_idx) = TOKEN_LINE(token);
            IL_COL_NUM(list_idx)  = TOKEN_COLUMN(token);

            if (cif_flags & XREF_RECS) {
               cif_usage_rec(IL_IDX(list_idx), AT_Tbl_Idx,
	        	     IL_LINE_NUM(list_idx), IL_COL_NUM(list_idx),
			     CIF_Label_Referenced_As_Format);
            }
         }
      }
      else if (TOKEN_ERR(token)) {
         parse_err_flush(Find_Comma, NULL);
         parsed_ok = FALSE;
      }
      else {
         parse_err_flush(Find_Comma, "LABEL");
         parsed_ok = FALSE;
      }
   } 
   else {

      buf_idx = LA_CH_BUF_IDX;

      parsed_ok = parse_expr(&opnd);
      COPY_OPND(IL_OPND(list_idx), opnd);

      if (IL_FLD(list_idx) == CN_Tbl_Idx &&
          TYP_TYPE(CN_TYPE_IDX(IL_IDX(list_idx))) == Character) {

         set_format_start_idx(buf_idx);

         format_cn_idx = IL_IDX(list_idx);

         ignore_trailing_chars = TRUE;
         pre_parse_format_idx = pre_parse_format(format_cn_idx, 0);
         ignore_trailing_chars = FALSE;

         NTR_IR_LIST_TBL(list2_idx);
         IL_FLD(list_idx)      = IL_Tbl_Idx;
         IL_IDX(list_idx)      = list2_idx;
         IL_LIST_CNT(list_idx) = 2;

         IL_FLD(list2_idx) = AT_Tbl_Idx;
         idx = create_format_tmp(format_cn_idx);
         IL_IDX(list2_idx) = idx;
         IL_LINE_NUM(list2_idx) = line;
         IL_COL_NUM(list2_idx)  = column;

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list2_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) = list2_idx;
         list2_idx = IL_NEXT_LIST_IDX(list2_idx);


         if (pre_parse_format_idx != NULL_IDX) {
            IL_FLD(list2_idx) = AT_Tbl_Idx;
            idx = create_format_tmp(pre_parse_format_idx);
            IL_IDX(list2_idx) = idx;
            IL_LINE_NUM(list2_idx) = line;
            IL_COL_NUM(list2_idx)  = column;
         }
      }
   }

   if (LA_CH_VALUE != EOS) {
      
      if (LA_CH_VALUE != COMMA) {
         parse_err_flush(Find_EOS, ",");
         parsed_ok = FALSE;
      }
      else {
  
         NEXT_LA_CH;

         parsed_ok = parse_io_list(&opnd) && parsed_ok;
         COPY_OPND(IR_OPND_R(ir_idx), opnd);
      }
   }
 
   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
      parsed_ok = FALSE;
   }

   matched_specific_token(Tok_EOS, Tok_Class_Punct);

   INSERT_IO_END;

   TRACE (Func_Exit, "parse_print_stmt", NULL);

   return;

}  /* parse_print_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_read_stmt (void)

{
   int       attr_idx;
   int       buf_idx;
   int       column;
   int       i;
   int       idx;
   int       ir_idx;
   int       line;
   int	     list_idx;
   int	     list2_idx;
   int       name_idx;
   opnd_type opnd;
   boolean   parsed_ok = TRUE;
   int	     pre_parse_format_idx;


   TRACE (Func_Entry, "parse_read_stmt", NULL);

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   INSERT_IO_START;

   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;

   IR_OPR(ir_idx)              = Read_Formatted_Opr;
   IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
   column                      = TOKEN_COLUMN(token);
   IR_COL_NUM(ir_idx)          = column;
   line                        = TOKEN_LINE(token);
   IR_LINE_NUM(ir_idx)         = line;

   if (LA_CH_VALUE == LPAREN) {
      parsed_ok = parse_io_control_list(&opnd, Read);
      COPY_OPND(IR_OPND_L(ir_idx), opnd);

#ifdef KEY /* Bug 10573 */
      /* Stupid extension allows comma here */
      if (LA_CH_VALUE == COMMA && ! on_off_flags.issue_ansi_messages) {
        NEXT_LA_CH;
      }
#endif /* KEY Bug 10573 */

      if (LA_CH_VALUE != EOS) {

         parsed_ok = parse_io_list(&opnd) && parsed_ok;
         COPY_OPND(IR_OPND_R(ir_idx), opnd);
      }
   }
   else {

      IR_FLD_L(ir_idx)            = IL_Tbl_Idx;
      IR_LIST_CNT_L(ir_idx)       = ciitem_tbl[Read].num_ciitems;
      NTR_IR_LIST_TBL(list_idx);
      IR_IDX_L(ir_idx)            = list_idx;

      /* set first one to CN_Tbl_Idx for default unit */
      IL_FLD(list_idx)            = CN_Tbl_Idx;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = column;

      for (i = 2; i <= ciitem_tbl[Read].num_ciitems; i++) {
         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

      list_idx = IL_NEXT_LIST_IDX(IR_IDX_L(ir_idx));

      if (LA_CH_VALUE == STAR) {
         /* CN_Tbl_Idx and idx = NULL is list directed */
         IL_FLD(list_idx) = CN_Tbl_Idx;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = column;
         NEXT_LA_CH;
      }
      else if (LA_CH_CLASS == Ch_Class_Digit &&
               digit_is_format_label())      {

         if (MATCHED_TOKEN_CLASS(Tok_Class_Label) &&
             ! TOKEN_ERR(token)) {

            attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                    &name_idx);

            if (attr_idx == NULL_IDX) {
               attr_idx               = ntr_sym_tbl(&token, name_idx);
               AT_OBJ_CLASS(attr_idx) = Label;
               LN_DEF_LOC(name_idx)   = TRUE;
               build_fwd_ref_entry(attr_idx, Format_Ref);
            }
            else if ( ! AT_DCL_ERR(attr_idx) ) {

                 if (!AT_DEFINED(attr_idx)) {
                    build_fwd_ref_entry(attr_idx, Format_Ref);
                 }
                 else if (ATL_CLASS(attr_idx) != Lbl_Format) {
                 /* error .. label used previously as something else */
                    PRINTMSG(TOKEN_LINE(token), 328, Error,
                             TOKEN_COLUMN(token),
                             AT_OBJ_NAME_PTR(attr_idx));
                    parsed_ok = FALSE;
                 }
            }
            else {
               /* no message, at_dcl_err is set */
               parsed_ok = FALSE;
            }

            if (parsed_ok) {
               IL_FLD(list_idx)      = AT_Tbl_Idx;
               IL_IDX(list_idx)      = attr_idx;
               IL_LINE_NUM(list_idx) = TOKEN_LINE(token);
               IL_COL_NUM(list_idx)  = TOKEN_COLUMN(token);

               if (cif_flags & XREF_RECS) {
                  cif_usage_rec(IL_IDX(list_idx), AT_Tbl_Idx,
			        IL_LINE_NUM(list_idx), IL_COL_NUM(list_idx),
				CIF_Label_Referenced_As_Format);
               }
            }
         }
         else if (TOKEN_ERR(token)) {
            parse_err_flush(Find_Comma, NULL);
            parsed_ok = FALSE;
         }
         else {
            parse_err_flush(Find_Comma, "LABEL");
            parsed_ok = FALSE;
         }
      }
      else {

         buf_idx = LA_CH_BUF_IDX;

         parsed_ok = parse_expr(&opnd);
         COPY_OPND(IL_OPND(list_idx), opnd);

         if (IL_FLD(list_idx) == CN_Tbl_Idx &&
             TYP_TYPE(CN_TYPE_IDX(IL_IDX(list_idx))) == Character) {

            set_format_start_idx(buf_idx);

            format_cn_idx = IL_IDX(list_idx);

            ignore_trailing_chars = TRUE;
            pre_parse_format_idx = pre_parse_format(format_cn_idx, 0);
            ignore_trailing_chars = FALSE;

            NTR_IR_LIST_TBL(list2_idx);
            IL_FLD(list_idx)      = IL_Tbl_Idx;
            IL_IDX(list_idx)      = list2_idx;
            IL_LIST_CNT(list_idx) = 2;

            IL_FLD(list2_idx) = AT_Tbl_Idx;
            idx = create_format_tmp(format_cn_idx);
            IL_IDX(list2_idx) = idx;
            IL_LINE_NUM(list2_idx) = line;
            IL_COL_NUM(list2_idx)  = column;

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list2_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) = list2_idx;
            list2_idx = IL_NEXT_LIST_IDX(list2_idx);

            if (pre_parse_format_idx != NULL_IDX) {
               IL_FLD(list2_idx) = AT_Tbl_Idx;
               idx = create_format_tmp(pre_parse_format_idx);
               IL_IDX(list2_idx) = idx;
               IL_LINE_NUM(list2_idx) = line;
               IL_COL_NUM(list2_idx)  = column;
            }
         }
      }

      if (LA_CH_VALUE != EOS) {

         if (LA_CH_VALUE != COMMA) {
            parse_err_flush(Find_EOS, ",");
            parsed_ok = FALSE;
         }
         else {
            NEXT_LA_CH;

            parsed_ok = parse_io_list(&opnd) && parsed_ok;
            COPY_OPND(IR_OPND_R(ir_idx), opnd);
         }
      }
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
      parsed_ok = FALSE;
   }

   matched_specific_token(Tok_EOS, Tok_Class_Punct);

   INSERT_IO_END;

   TRACE (Func_Exit, "parse_read_stmt", NULL);

   return;

}  /* parse_read_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_rewind_stmt (void)

{
   int        call_idx;
   int        list_idx;
   opnd_type  opnd;
   boolean    parsed_ok = TRUE;


   TRACE (Func_Entry, "parse_rewind_stmt", NULL);

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   if (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ||
       ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))) {
      PRINTMSG(TOKEN_LINE(token), 1262, Error, TOKEN_COLUMN(token),
               stmt_type_str[stmt_type],
               ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ? "pure" : "elemental",
               AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
   }

   INSERT_IO_START;

   NTR_IR_TBL(call_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = call_idx;

   IR_OPR(call_idx) = Call_Opr;
   IR_TYPE_IDX(call_idx) = TYPELESS_DEFAULT_TYPE;
   IR_COL_NUM(call_idx) = TOKEN_COLUMN(token);
   IR_LINE_NUM(call_idx) = TOKEN_LINE(token);

   /* left child is rewind attr */

   if (glb_tbl_idx[Rewind_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[Rewind_Attr_Idx] = create_lib_entry_attr(REWIND_LIB_ENTRY,
                                                           REWIND_NAME_LEN,
                                                           TOKEN_LINE(token),
                                                           TOKEN_COLUMN(token));
   }

   ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Rewind_Attr_Idx]);

   IR_FLD_L(call_idx)      = AT_Tbl_Idx;
   IR_IDX_L(call_idx)      = glb_tbl_idx[Rewind_Attr_Idx];

   IR_LINE_NUM_L(call_idx) = TOKEN_LINE(token);
   IR_COL_NUM_L(call_idx)  = TOKEN_COLUMN(token);

   if (LA_CH_VALUE == LPAREN) {
      parsed_ok = parse_io_control_list(&opnd, Rewind);
      COPY_OPND(IR_OPND_R(call_idx), opnd);
   }
   else {
      /* call parse_expr to get external file unit */
      parsed_ok = parse_expr(&opnd);
      NTR_IR_LIST_TBL(list_idx);
      IL_ARG_DESC_VARIANT(list_idx) = TRUE;
      IR_FLD_R(call_idx) = IL_Tbl_Idx;
      IR_IDX_R(call_idx) = list_idx;
      COPY_OPND(IL_OPND(list_idx), opnd);
      IR_LIST_CNT_R(call_idx) = 3;

      /* add the blank arguments */
      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      IL_ARG_DESC_VARIANT(list_idx) = TRUE;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      IL_ARG_DESC_VARIANT(list_idx) = TRUE;
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
      parsed_ok = FALSE;
   }

   matched_specific_token(Tok_EOS, Tok_Class_Punct);

   SH_ERR_FLG(curr_stmt_sh_idx) = ! parsed_ok;

   INSERT_IO_END;

   TRACE (Func_Exit, "parse_rewind_stmt", NULL);

   return;

}  /* parse_rewind_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_write_stmt (void)

{
   int       attr_idx;
   int       buf_idx;
   int       column;
   int       i;
   int	     idx;
   int       ir_idx;
   int       line;
   int       list_idx;
   int       list2_idx;
   int       name_idx;
   opnd_type opnd;
   boolean   parsed_ok = TRUE;
   int       pre_parse_format_idx;


   TRACE (Func_Entry, "parse_write_stmt", NULL);

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   INSERT_IO_START;

   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;

   IR_OPR(ir_idx)              = Write_Formatted_Opr;
   IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
   column                      = TOKEN_COLUMN(token);
   IR_COL_NUM(ir_idx)          = column;
   line                        = TOKEN_LINE(token);
   IR_LINE_NUM(ir_idx)         = line;

   if (LA_CH_VALUE == LPAREN) {
      parsed_ok = parse_io_control_list(&opnd, Write);
      COPY_OPND(IR_OPND_L(ir_idx), opnd);

#ifdef KEY /* Bug 10573 */
      /* Stupid extension allows comma here */
      if (LA_CH_VALUE == COMMA && ! on_off_flags.issue_ansi_messages) {
        NEXT_LA_CH;
      }
#endif /* KEY Bug 10573 */

      if (LA_CH_VALUE != EOS) {

         parsed_ok = parse_io_list(&opnd) && parsed_ok;
         COPY_OPND(IR_OPND_R(ir_idx), opnd);
      }
   }
   else {

      /* issue ansi msg for nonstandard write */
      PRINTMSG(LA_CH_LINE, 174, Ansi, LA_CH_COLUMN, NULL);

      IR_FLD_L(ir_idx)            = IL_Tbl_Idx;
      IR_LIST_CNT_L(ir_idx)       = ciitem_tbl[Write].num_ciitems;
      NTR_IR_LIST_TBL(list_idx);
      IR_IDX_L(ir_idx)            = list_idx;

      /* set first one to CN_Tbl_Idx for default unit */
      IL_FLD(list_idx)            = CN_Tbl_Idx;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = column;

      for (i = 2; i <= ciitem_tbl[Write].num_ciitems; i++) {
         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

      list_idx = IL_NEXT_LIST_IDX(IR_IDX_L(ir_idx));

      if (LA_CH_VALUE == STAR) {
         /* CN_Tbl_Idx and idx = NULL is list directed */
         IL_FLD(list_idx) = CN_Tbl_Idx;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = column;
         NEXT_LA_CH;
      }
      else if (LA_CH_CLASS == Ch_Class_Digit &&
               digit_is_format_label())      {

         if (MATCHED_TOKEN_CLASS(Tok_Class_Label) &&
             ! TOKEN_ERR(token)) {

            attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                    &name_idx);

            if (attr_idx == NULL_IDX) {
               attr_idx               = ntr_sym_tbl(&token, name_idx);
               AT_OBJ_CLASS(attr_idx) = Label;
               LN_DEF_LOC(name_idx)   = TRUE;
               build_fwd_ref_entry(attr_idx, Format_Ref);
            }
            else if ( ! AT_DCL_ERR(attr_idx) ) {

                 if (!AT_DEFINED(attr_idx)) {
                    build_fwd_ref_entry(attr_idx, Format_Ref);
                 }
                 else if (ATL_CLASS(attr_idx) != Lbl_Format) {
                 /* error .. label used previously as something else */
                    PRINTMSG(TOKEN_LINE(token), 328, Error,
                             TOKEN_COLUMN(token),
                             AT_OBJ_NAME_PTR(attr_idx));
                    parsed_ok = FALSE;
                 }
            }
            else {
               /* no message, at_dcl_err is set */
               parsed_ok = FALSE;
            }

            if (parsed_ok) {
               IL_FLD(list_idx)      = AT_Tbl_Idx;
               IL_IDX(list_idx)      = attr_idx;
               IL_LINE_NUM(list_idx) = TOKEN_LINE(token);
               IL_COL_NUM(list_idx)  = TOKEN_COLUMN(token);
            }
         }
         else if (TOKEN_ERR(token)) {
            parse_err_flush(Find_Comma, NULL);
            parsed_ok = FALSE;
         }
         else {
            parse_err_flush(Find_Comma, "LABEL");
            parsed_ok = FALSE;
         }
      }
      else {

         buf_idx = LA_CH_BUF_IDX;

         parsed_ok = parse_expr(&opnd);
         COPY_OPND(IL_OPND(list_idx), opnd);

         if (IL_FLD(list_idx) == CN_Tbl_Idx &&
             TYP_TYPE(CN_TYPE_IDX(IL_IDX(list_idx))) == Character) {

            set_format_start_idx(buf_idx);

            format_cn_idx = IL_IDX(list_idx);

            ignore_trailing_chars = TRUE;
            pre_parse_format_idx = pre_parse_format(format_cn_idx, 0);
            ignore_trailing_chars = FALSE;

            NTR_IR_LIST_TBL(list2_idx);
            IL_FLD(list_idx)      = IL_Tbl_Idx;
            IL_IDX(list_idx)      = list2_idx;
            IL_LIST_CNT(list_idx) = 2;

            IL_FLD(list2_idx) = AT_Tbl_Idx;
            idx = create_format_tmp(format_cn_idx);
            IL_IDX(list2_idx) = idx;
            IL_LINE_NUM(list2_idx) = line;
            IL_COL_NUM(list2_idx)  = column;

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list2_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) = list2_idx;
            list2_idx = IL_NEXT_LIST_IDX(list2_idx);

            if (pre_parse_format_idx != NULL_IDX) {
               IL_FLD(list2_idx) = AT_Tbl_Idx;
               idx = create_format_tmp(pre_parse_format_idx);
               IL_IDX(list2_idx) = idx;
               IL_LINE_NUM(list2_idx) = line;
               IL_COL_NUM(list2_idx)  = column;
            }
         }
      }

      if (LA_CH_VALUE != EOS) {

         if (LA_CH_VALUE != COMMA) {
            parse_err_flush(Find_EOS, ",");
            parsed_ok = FALSE;
         }
         else {
            NEXT_LA_CH;

            parsed_ok = parse_io_list(&opnd) && parsed_ok;
            COPY_OPND(IR_OPND_R(ir_idx), opnd);
         }
      }
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
      parsed_ok = FALSE;
   }

   matched_specific_token(Tok_EOS, Tok_Class_Punct);

   INSERT_IO_END;

   TRACE (Func_Exit, "parse_write_stmt", NULL);
   
   return;

}  /* parse_write_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	parse io list which may include implied do loops.                     *|
|*									      *|
|* Input parameters:							      *|
|*	NONE                                                                  *|
|*									      *|
|* Output parameters:							      *|
|*	result_opnd - opnd pointing to root of list tree produced.            *|
|*									      *|
|* Returns:								      *|
|*	TRUE if parsed ok.					              *|
|*									      *|
\******************************************************************************/

boolean parse_io_list (opnd_type *result_opnd)

{
   int       buf_idx;
   int       list_idx;
#ifdef KEY /* Bug 10177 */
   int       list2_idx = 0;
#else /* KEY Bug 10177 */
   int       list2_idx;
#endif /* KEY Bug 10177 */
   char	     next_char;
   opnd_type opnd;
   int       paren_level = 0;
   boolean   parsed_ok = TRUE;
   int       stmt_num;


   TRACE (Func_Entry, "parse_io_list", NULL);

   OPND_FLD((*result_opnd))      = IL_Tbl_Idx;
   OPND_IDX((*result_opnd))      = NULL_IDX;
   OPND_LIST_CNT((*result_opnd)) = 0;

   do {

      if (LA_CH_VALUE == LPAREN) { 

         if (next_tok_is_paren_slash ()) {
            parsed_ok = parse_expr(&opnd) && parsed_ok;
         }
         else if (is_implied_do ()) {
            parsed_ok = parse_imp_do(&opnd) && parsed_ok;
         }
         else {
            next_char = scan_thru_close_paren(0,0,1);

            if (next_char == COMMA ||
                next_char == EOS   ||
                next_char == RPAREN) {

               buf_idx = LA_CH_BUF_IDX;
               stmt_num = LA_CH_STMT_NUM;

               NEXT_LA_CH;

               if (LA_CH_VALUE == LPAREN ||
                   LA_CH_VALUE == RPAREN ||
                   LA_CH_VALUE == EOS)   {

                  paren_level++;
                  continue;
               }
               else if (paren_grp_is_cplx_const()) {
                  /* this is a complex constant */
                  reset_lex(buf_idx,stmt_num);
                  parsed_ok = parse_expr(&opnd) && parsed_ok;
               }
               else {
                  /* go back and swallow beginning ( */
                  reset_lex(buf_idx,stmt_num);
                  NEXT_LA_CH;
                  paren_level++;
                  continue;
               }
            }
            else {
               parsed_ok = parse_expr(&opnd) && parsed_ok;

               if (stmt_type == Read_Stmt    ||
                   stmt_type == Decode_Stmt) {
                  mark_attr_defined(&opnd);
               }
            }
         }
      }
      else {

         parsed_ok = parse_expr(&opnd) && parsed_ok;
      
         if (stmt_type == Read_Stmt    ||
             stmt_type == Decode_Stmt) {
            mark_attr_defined(&opnd);
         }
      }

      ++OPND_LIST_CNT((*result_opnd));

      NTR_IR_LIST_TBL(list_idx);
      COPY_OPND(IL_OPND(list_idx), opnd);

      if (OPND_IDX((*result_opnd)) == NULL_IDX) {
         OPND_IDX((*result_opnd)) = list_idx;
      }
      else {
         IL_NEXT_LIST_IDX(list2_idx) = list_idx;
         IL_PREV_LIST_IDX(list_idx) = list2_idx;
      }

      list2_idx = list_idx;

      while (LA_CH_VALUE == RPAREN && paren_level) {
         NEXT_LA_CH;
         paren_level--;
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else {
         break;
      }
   }
   while (TRUE);

   if (paren_level) {
      parse_err_flush(Find_EOS, ")");
   }

   TRACE (Func_Exit, "parse_io_list", NULL);
   return(parsed_ok);
} /* parse_io_list */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      search ciitem table for ciitem entry.                                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      stmt_type - idx into ciitem table                                     *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      None.                                                                 *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      idx into ciitem list for io stmt.                                     *|
|*                                                                            *|
\******************************************************************************/

static int  find_ciitem_idx (io_stmt_type stmt_type)

{
   int     finish;
   int     i;
   int     idx		= -1;
   int     start;
   int     test;


   TRACE (Func_Entry, "find_ciitem_idx", NULL);

   start = 0;
   finish = ciitem_tbl[stmt_type].num_diff_ciitems;
   while (TRUE) {
      test = (finish - start) / 2 + start;

      if ((i = strncmp(TOKEN_STR(token),ciitem_tbl[stmt_type].ciitem_list[test].
           name, ciitem_tbl[stmt_type].ciitem_list[test].name_length)) == 0) {
         /* found match */

         if (TOKEN_LEN(token) == ciitem_tbl[stmt_type].ciitem_list[test].
             name_length) {
            idx = test;
            break;
         }
         else if (start == test) {
            break;
         }
         else {
            start = test;
         }
      }
      else if (i < 0) {
         if (finish == test) {
            break;
         }
         finish = test;
      }
      else {
         if (start == test) {
            break;
         }
         start = test;
      }

      if (finish <= start) {
         break;
      }
   }
   TRACE (Func_Exit, "find_ciitem_idx", NULL);

   return(idx);
} /* find_ciitem_idx */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      parse io list which may include implied do loops.                     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      can_have_expression - Boolean flag for input or output list.          *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      result_opnd - opnd pointing to root of list tree produced.            *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if parsed ok.                                                    *|
|*                                                                            *|
\******************************************************************************/

static boolean  parse_io_control_list (opnd_type   *result_opnd,
       				       io_stmt_type stmt_type)

{
   int          arg_array[26];
   int          arg_cnt = 0;
   int          arg_idx;
   int          attr_idx;
   int          buf_idx;
   char         *ch_ptr1;
   char         *ch_ptr2;
   int          ciitem_idx;
#ifdef KEY /* Bug 10177 */
   boolean      found = FALSE;
#else /* KEY Bug 10177 */
   boolean      found;
#endif /* KEY Bug 10177 */
   boolean	had_fmt = FALSE;
   boolean      had_keyword = FALSE;
   boolean      had_nml = FALSE;
   long         i;
   int		idx;
   boolean      item_has_keyword;
#ifdef KEY /* Bug 10177 */
   int          kwd_col = 0;
   int          kwd_line = 0;
#else /* KEY Bug 10177 */
   int          kwd_col;
   int          kwd_line;
#endif /* KEY Bug 10177 */
   int          list_idx;
   int          list2_idx;
   int          name_idx;
   int          num_args;
   opnd_type    opnd;
   int		opnd_column;
   int		opnd_line;
   boolean      parsed_ok		= TRUE;
   int          pre_parse_format_idx;


   TRACE (Func_Entry, "parse_io_control_list", NULL);

   if (LA_CH_VALUE != LPAREN) {
      /* shouldn't be here */
      parse_err_flush(Find_EOS, "(");
      parsed_ok = FALSE;
   }
   else {
      OPND_FLD((*result_opnd))      = IL_Tbl_Idx;
      num_args                      = ciitem_tbl[stmt_type].num_ciitems;
      OPND_LIST_CNT((*result_opnd)) = num_args;
      list2_idx = NULL_IDX;

      for (i = 1; i <= num_args; i++) {
         NTR_IR_LIST_TBL(list_idx)
         arg_array[i] = list_idx;

         if (stmt_type == Backspace ||
             stmt_type == Close     ||
             stmt_type == Endfile   ||
             stmt_type == Inquire   ||
             stmt_type == Open      ||
             stmt_type == Rewind)   {
 
            IL_ARG_DESC_VARIANT(list_idx) = TRUE;

            if (list2_idx) {
               IL_NEXT_LIST_IDX(list2_idx) = list_idx;
            }
         }
         else if (list2_idx) {
            IL_NEXT_LIST_IDX(list2_idx) = list_idx;
            IL_PREV_LIST_IDX(list_idx)  = list2_idx;
         }
         list2_idx = list_idx;
      }
      OPND_IDX((*result_opnd)) = arg_array[1];

      do {
         NEXT_LA_CH;

         if (LA_CH_VALUE == RPAREN && arg_cnt == 0) {
            break;
         }

         arg_cnt++;

         item_has_keyword = FALSE;

         if (next_arg_is_kwd_equal()) {
            MATCHED_TOKEN_CLASS(Tok_Class_Id);

            kwd_line = TOKEN_LINE(token);
            kwd_col  = TOKEN_COLUMN(token);

            /* have keyword */
            had_keyword = TRUE;
            item_has_keyword = TRUE;
            ciitem_idx = find_ciitem_idx(stmt_type);

            if (ciitem_idx < 0) {
               /* ciitem not found */
               PRINTMSG(TOKEN_LINE(token), 73, Error, 
                        TOKEN_COLUMN(token), NULL);
               parsed_ok = FALSE;
               parse_err_flush(Find_Comma_Rparen, NULL);
               continue;
            }

            NEXT_LA_CH;
         }
         else { /* had id but not kwd, must reparse as expression */

            if (arg_cnt == 2 &&
                had_keyword &&
                ciitem_tbl[stmt_type].num_without_kwd == 2 &&
                IL_FLD(arg_array[UNIT_IDX]) != NO_Tbl_Idx) {

               /* this is an extension "write (unit = 100, *)" */
             
               PRINTMSG(LA_CH_LINE, 1208, Ansi, LA_CH_COLUMN);
            }
            else if (arg_cnt > ciitem_tbl[stmt_type].num_without_kwd ||
                     had_keyword)                                    {
               /* keyword missing for something other than UNIT or FMT */
               PRINTMSG(LA_CH_LINE, 139, Error, LA_CH_COLUMN);
               parsed_ok = FALSE;
               parse_err_flush(Find_Comma_Rparen, NULL);
               continue;
            }
            ciitem_idx = arg_idx_tbl[stmt_type][arg_cnt];
         }

         arg_idx = ciitem_tbl[stmt_type].ciitem_list[ciitem_idx].arg_position;

         if (stmt_type == Write       &&
             (arg_idx == END_IDX  || 
              arg_idx == SIZE_IDX || 
              arg_idx == EOR_IDX))   {

            PRINTMSG(kwd_line, 445, Error, kwd_col, 
                     ciitem_tbl[stmt_type].ciitem_list[ciitem_idx].name);
            parsed_ok = FALSE;
         }
         else if (IL_FLD(arg_array[arg_idx]) != NO_Tbl_Idx) {
            /* can't have two args the same */

            if (arg_idx == FMT_IDX &&
                (stmt_type == Read || stmt_type == Write)) {

               if ((had_fmt && 
                    strcmp(ciitem_tbl[stmt_type].ciitem_list[ciitem_idx].name,
                           "NML") == 0)           ||
                   (had_nml &&
                    strcmp(ciitem_tbl[stmt_type].ciitem_list[ciitem_idx].name, 
                           "FMT") == 0))          {
 
                  PRINTMSG(TOKEN_LINE(token), 443, Error, TOKEN_COLUMN(token));
               }
               else {
                  PRINTMSG(TOKEN_LINE(token), 70, Error, TOKEN_COLUMN(token));
               }
            }
            else {
               PRINTMSG(TOKEN_LINE(token), 70, Error, TOKEN_COLUMN(token));
            }
            parsed_ok = FALSE;
            parse_err_flush(Find_Comma_Rparen, NULL);
            continue;
         }

         if (LA_CH_VALUE == STAR) {
         
            if (ciitem_tbl[stmt_type].ciitem_list[ciitem_idx].allowed_form ==
                Format_Form 						   ||
                arg_idx == UNIT_IDX)					   {
               IL_FLD(arg_array[arg_idx])      = CN_Tbl_Idx;
               IL_IDX(arg_array[arg_idx])      = NULL_IDX;
               IL_LINE_NUM(arg_array[arg_idx]) = LA_CH_LINE;
               IL_COL_NUM(arg_array[arg_idx])  = LA_CH_COLUMN;
            }
            else {
               PRINTMSG(LA_CH_LINE, 47, Error, LA_CH_COLUMN, NULL);
               parsed_ok = FALSE;
            }
            NEXT_LA_CH;
            continue;
         }


         switch (ciitem_tbl[stmt_type].ciitem_list[ciitem_idx].allowed_form) {
            case Exp_Form :

               parsed_ok = parse_expr(&opnd) && parsed_ok;
               COPY_OPND(IL_OPND(arg_array[arg_idx]), opnd);
               break;

            case Label_Form :

               switch (stmt_type) {
                  case Backspace :
                     ATP_HAS_ALT_RETURN(glb_tbl_idx[Backspace_Attr_Idx]) = TRUE;
                     break;

                  case Close     :
                     ATP_HAS_ALT_RETURN(glb_tbl_idx[Close_Attr_Idx]) = TRUE;
                     break;

                  case Endfile   :
                     ATP_HAS_ALT_RETURN(glb_tbl_idx[Endfile_Attr_Idx]) = TRUE;
                     break;

                  case Inquire   :
                     ATP_HAS_ALT_RETURN(glb_tbl_idx[Inquire_Attr_Idx]) = TRUE;
                     break;

                  case Open      :
                     ATP_HAS_ALT_RETURN(glb_tbl_idx[Open_Attr_Idx]) = TRUE;
                     break;

                  case Rewind    :
                     ATP_HAS_ALT_RETURN(glb_tbl_idx[Rewind_Attr_Idx]) = TRUE;
                     break;

                  default        :
                     break;
               }

               if (LA_CH_CLASS == Ch_Class_Digit) {
                  /* label */
                  
                  if (MATCHED_TOKEN_CLASS(Tok_Class_Label) &&
                      ! TOKEN_ERR(token)) {
                    
                     attr_idx = check_label_ref();

                     AT_REFERENCED(attr_idx) 	     = Referenced;
                     IL_FLD(arg_array[arg_idx])      = AT_Tbl_Idx;
                     IL_IDX(arg_array[arg_idx])      = attr_idx;
                     IL_LINE_NUM(arg_array[arg_idx]) = TOKEN_LINE(token);
                     IL_COL_NUM(arg_array[arg_idx])  = TOKEN_COLUMN(token);
                  }
                  else if (TOKEN_ERR(token)) {
                     parse_err_flush(Find_Comma_Rparen, NULL);
                     parsed_ok = FALSE;
                  }
                  else {
                     parse_err_flush(Find_Comma_Rparen, "LABEL");
                     parsed_ok = FALSE;
                  }
               }
               else {
                  parsed_ok = parse_expr(&opnd) && parsed_ok;
                  COPY_OPND(IL_OPND(arg_array[arg_idx]), opnd);
               }
               break;

            case Namelist_Form :

               if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
                  parsed_ok = parse_deref(&opnd, NULL_IDX) && parsed_ok;
                  COPY_OPND(IL_OPND(arg_array[arg_idx]), opnd);
               }
               else {
                  /* error .. no namelist group name */
                  PRINTMSG(LA_CH_LINE, 173, Error, LA_CH_COLUMN, NULL);
                  parse_err_flush(Find_Comma_Rparen, NULL);
                  parsed_ok = FALSE;
               }

               IL_NAMELIST_EXPECTED(arg_array[arg_idx]) = TRUE;
               IL_FORMAT_EXPECTED(arg_array[arg_idx])   = FALSE;

               had_nml = TRUE;

               break;

            case Var_Only_Form :

               parsed_ok = parse_expr(&opnd) && parsed_ok;
               COPY_OPND(IL_OPND(arg_array[arg_idx]), opnd);

               mark_attr_defined(&opnd);

               break;

            case Format_Form :

               buf_idx = LA_CH_BUF_IDX;

               if (LA_CH_CLASS == Ch_Class_Digit &&
                   digit_is_format_label())      {

                  /* label */

                  if (MATCHED_TOKEN_CLASS(Tok_Class_Label) &&
                      ! TOKEN_ERR(token)) {

                     attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                             &name_idx);

                     if (attr_idx == NULL_IDX) {
                        attr_idx               = ntr_sym_tbl(&token, name_idx);
                        AT_OBJ_CLASS(attr_idx) = Label;
                        LN_DEF_LOC(name_idx)   = TRUE;
                        build_fwd_ref_entry(attr_idx, Format_Ref);
                     }
                     else if ( ! AT_DCL_ERR(attr_idx) ) {

                        if (!AT_DEFINED(attr_idx)) {
                           build_fwd_ref_entry(attr_idx, Format_Ref);
                        }
                        else if (ATL_CLASS(attr_idx) != Lbl_Format) {
                        /* error .. label used previously as something else */
                           PRINTMSG(TOKEN_LINE(token), 328, Error, 
                                    TOKEN_COLUMN(token), 
                                    AT_OBJ_NAME_PTR(attr_idx));
                           parsed_ok = FALSE;
                           break;
                        }
                     }
                     else {
                        /* no message, at_dcl_err is set */
                        parsed_ok = FALSE;
                        break;
                     }

                     IL_FLD(arg_array[arg_idx])      = AT_Tbl_Idx;
                     IL_IDX(arg_array[arg_idx])      = attr_idx;
                     IL_LINE_NUM(arg_array[arg_idx]) = TOKEN_LINE(token);
                     IL_COL_NUM(arg_array[arg_idx])  = TOKEN_COLUMN(token);

                     if (cif_flags & XREF_RECS) {
                        cif_usage_rec(attr_idx, AT_Tbl_Idx,
				      TOKEN_LINE(token), TOKEN_COLUMN(token),
				      CIF_Label_Referenced_As_Format);
                     }
                  }
                  else if (TOKEN_ERR(token)) {
                     parse_err_flush(Find_Comma_Rparen, NULL);
                     parsed_ok = FALSE;
                  }
                  else {
                     parse_err_flush(Find_Comma_Rparen, "LABEL");
                     parsed_ok = FALSE;
                  }
               }
               else {
                  parsed_ok = parse_expr(&opnd) && parsed_ok;
                  COPY_OPND(IL_OPND(arg_array[arg_idx]), opnd);
               }

               IL_FORMAT_EXPECTED(arg_array[arg_idx])   = item_has_keyword;
               IL_NAMELIST_EXPECTED(arg_array[arg_idx]) = FALSE;

               if (!item_has_keyword                                     &&
                   IL_FLD(arg_array[arg_idx]) == AT_Tbl_Idx              &&
                   AT_OBJ_CLASS(IL_IDX(arg_array[arg_idx])) == Namelist_Grp) {
                  had_nml = TRUE;
               }
               else {
                  had_fmt = TRUE;
               }

               if (had_fmt                                                &&
                   IL_FLD(arg_array[arg_idx])               == CN_Tbl_Idx &&
                   TYP_TYPE(CN_TYPE_IDX(IL_IDX(arg_array[arg_idx]))) == 
                                                                    Character) {

                  /* preparse format constant */
                  set_format_start_idx(buf_idx);

                  format_cn_idx = IL_IDX(arg_array[arg_idx]);

                  ignore_trailing_chars = TRUE;
                  pre_parse_format_idx  = pre_parse_format(format_cn_idx, 0);
                  ignore_trailing_chars = FALSE;

                  NTR_IR_LIST_TBL(list_idx);
                  IL_FLD(arg_array[arg_idx])      = IL_Tbl_Idx;
                  IL_IDX(arg_array[arg_idx])      = list_idx;
                  IL_LIST_CNT(arg_array[arg_idx]) = 2;

                  IL_FLD(list_idx) = AT_Tbl_Idx;
                  idx = create_format_tmp(format_cn_idx);
                  IL_IDX(list_idx) = idx;
                  IL_LINE_NUM(list_idx) = stmt_start_line;
                  IL_COL_NUM(list_idx)  = stmt_start_col;
                  
                  NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
                  IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
                  list_idx = IL_NEXT_LIST_IDX(list_idx);

                  if (pre_parse_format_idx != NULL_IDX) {
                     IL_FLD(list_idx) = AT_Tbl_Idx;
                     idx = create_format_tmp(pre_parse_format_idx);
                     IL_IDX(list_idx) = idx;
                     IL_LINE_NUM(list_idx) = stmt_start_line;
                     IL_COL_NUM(list_idx)  = stmt_start_col;
                  }
               }

               break;
         } /* switch */


         if (ciitem_tbl[stmt_type].ciitem_list[ciitem_idx].has_const_opts &&
             IL_FLD(arg_array[arg_idx]) == CN_Tbl_Idx                     &&
             TYP_TYPE(CN_TYPE_IDX(IL_IDX(arg_array[arg_idx]))) == Character) {

             /* make character const upper case */
             for (i = 0; 
              i < CN_INT_TO_C(TYP_IDX(CN_TYPE_IDX(IL_IDX(arg_array[arg_idx]))));
                 i++) {

                if (islower(((char *)
                    &CN_CONST(IL_IDX(arg_array[arg_idx])))[i])) {
                   ((char *)&CN_CONST(IL_IDX(arg_array[arg_idx])))[i] = 
                   TOUPPER(((char *)&CN_CONST(IL_IDX(arg_array[arg_idx])))[i]);
                }
             }
             /* check for correct character constant */
             for (i = 0; i < ciitem_tbl[stmt_type].ciitem_list[ciitem_idx].
                  num_const_opts; i++) {

                ch_ptr1 = (char *)&CN_CONST(IL_IDX(arg_array[arg_idx]));
                ch_ptr2 = ciitem_tbl[stmt_type].ciitem_list[ciitem_idx].
                          const_opts[i];
                found = TRUE;
                while (TRUE) {
                   
                   if (*ch_ptr2 == '\0') {
                      break;
                   }
                   else if (*ch_ptr1 != *ch_ptr2) {
                      found = FALSE;
                      break;
                   }
                   ch_ptr1++;
                   ch_ptr2++;
                }

                if (found) {
                   
                   while (*ch_ptr1 != '\0') {
                      
                      if (*ch_ptr1 != ' ') {
                         found = FALSE;
                         break;
                      }
                      ch_ptr1++;
                   }
                }

                if (found) {
                   break;
                }
            }
           
            if (! found) {
               /* error .. string constant not right one. */
               PRINTMSG(IL_LINE_NUM(arg_array[arg_idx]), 24, Error,
                        IL_COL_NUM(arg_array[arg_idx]),
                        (char *)&CN_CONST(IL_IDX(arg_array[arg_idx])),
                        ciitem_tbl[stmt_type].ciitem_list[ciitem_idx].name);
               parsed_ok = FALSE;
            }
         }

         if (LA_CH_VALUE != COMMA &&
             LA_CH_VALUE != RPAREN) {

            if (!parse_err_flush(Find_Comma_Rparen, ", or )")) {
               parsed_ok = FALSE;
               goto EXIT;
            }
            parsed_ok = FALSE;
         }
      }
      while (LA_CH_VALUE == COMMA);

      if (LA_CH_VALUE != RPAREN) {
         parse_err_flush(Find_EOS,")");
         parsed_ok = FALSE;
         goto EXIT;
      }
      else {
         NEXT_LA_CH;
      }

      /* do some checks here */
      if (IL_FLD(arg_array[UNIT_IDX]) == NO_Tbl_Idx) {
         /* no UNIT */
         if (stmt_type == Inquire) {
            if (IL_FLD(arg_array[FILE_IDX]) == NO_Tbl_Idx) {
               /* error .. must have UNIT or FILE */
               PRINTMSG(SH_GLB_LINE(curr_stmt_sh_idx), 440, Error,
                        SH_COL_NUM(curr_stmt_sh_idx));
               parsed_ok = FALSE;
            }
         }
         else {
            /* error .. must have unit */
            PRINTMSG(SH_GLB_LINE(curr_stmt_sh_idx), 439, Error,
                     SH_COL_NUM(curr_stmt_sh_idx),
                     io_stmt_str[stmt_type]);
            parsed_ok = FALSE;
         }
      }

      if (stmt_type == Inquire               &&
          IL_FLD(arg_array[UNIT_IDX]) != NO_Tbl_Idx   &&
          IL_FLD(arg_array[FILE_IDX]) != NO_Tbl_Idx) {

         /* error.. INQUIRE can't have both UNIT and FILE */

         find_opnd_line_and_column((opnd_type *)&IL_OPND(arg_array[1]),
                                   &opnd_line,
                                   &opnd_column);

         PRINTMSG(opnd_line, 442, Error, opnd_column);
         parsed_ok = FALSE;
      }

      if (stmt_type == Read || stmt_type == Write) {

         if (IL_FLD(arg_array[REC_IDX]) != NO_Tbl_Idx) {

            if (IL_FLD(arg_array[END_IDX]) != NO_Tbl_Idx) {

               /* if rec = , can't have end = */

               find_opnd_line_and_column((opnd_type *)
                                         &IL_OPND(arg_array[END_IDX]),
                                         &opnd_line,
                                         &opnd_column);

               PRINTMSG(opnd_line, 463, Error, opnd_column,
                        io_stmt_str[stmt_type]);
               parsed_ok = FALSE;
            }

            if (IL_FLD(arg_array[FMT_IDX]) == CN_Tbl_Idx &&
                IL_IDX(arg_array[FMT_IDX]) == NULL_IDX)  {

               /* if rec = , can't have list directed */

               find_opnd_line_and_column((opnd_type *)
                                         &IL_OPND(arg_array[FMT_IDX]),
                                         &opnd_line,
                                         &opnd_column);
               PRINTMSG(opnd_line, 464, Error, opnd_column,
                        io_stmt_str[stmt_type]);
               parsed_ok = FALSE;
            }

            if (IL_FLD(arg_array[ADVANCE_IDX]) != NO_Tbl_Idx) {

               /* if ADVANCE, can't have REC= (direct access) */

               find_opnd_line_and_column((opnd_type *)
                                         &IL_OPND(arg_array[REC_IDX]),
                                         &opnd_line,
                                         &opnd_column);
               PRINTMSG(opnd_line, 473, Error, opnd_column);
               parsed_ok = FALSE;
            }
         }

         /*  if EOR then must have ADVANCE */

         if (IL_FLD(arg_array[EOR_IDX]) != NO_Tbl_Idx &&
             IL_FLD(arg_array[ADVANCE_IDX]) == NO_Tbl_Idx) {
            find_opnd_line_and_column((opnd_type *)&IL_OPND(arg_array[EOR_IDX]),
                                      &opnd_line,
                                      &opnd_column);
            PRINTMSG(opnd_line, 465, Error, opnd_column,
                     io_stmt_str[stmt_type]);
            parsed_ok = FALSE;
         }

         /*  if SIZE then must have ADVANCE */

         if (IL_FLD(arg_array[SIZE_IDX]) != NO_Tbl_Idx &&
             IL_FLD(arg_array[ADVANCE_IDX]) == NO_Tbl_Idx) {
            find_opnd_line_and_column((opnd_type *)
                                      &IL_OPND(arg_array[SIZE_IDX]),
                                      &opnd_line,
                                      &opnd_column);
            PRINTMSG(opnd_line, 946, Error, opnd_column,
                     io_stmt_str[stmt_type]);
            parsed_ok = FALSE;
         }

         /* if UNIT == STAR, can't be unformatted */
         
         if (IL_FLD(arg_array[UNIT_IDX]) == CN_Tbl_Idx &&
             IL_IDX(arg_array[UNIT_IDX]) == NULL_IDX   &&
             IL_FLD(arg_array[FMT_IDX]) == NO_Tbl_Idx) {
            
            PRINTMSG(IL_LINE_NUM(arg_array[UNIT_IDX]),
                     1207, Error,
                     IL_COL_NUM(arg_array[UNIT_IDX]));

            parsed_ok = FALSE;
         }
      }
      else {
         if (IL_FLD(arg_array[UNIT_IDX]) == CN_Tbl_Idx &&
             IL_IDX(arg_array[UNIT_IDX]) == NULL_IDX) {

            /* can't have * for UNIT on anything but read and write */

            PRINTMSG(IL_LINE_NUM(arg_array[UNIT_IDX]), 
                     1206, Error, 
                     IL_COL_NUM(arg_array[UNIT_IDX]),
                     io_stmt_str[stmt_type]);

            parsed_ok = FALSE;
         }
      }
   } /* else */

EXIT:

   TRACE (Func_Exit, "parse_io_control_list", NULL);

   return(parsed_ok);
} /* parse_io_control_list */

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

static int pre_parse_format(int	const_idx,
                            int	lbl_name_len)


{
   int           caller_flag;
   long          format_len;
   long         *new_fmt;
   int		 pre_parse_idx;
   int		 type_idx;
   boolean       unused_boolean;
   void         (*the_func)();

# if defined(_HOST32) && defined(_TARGET64)
   int		 i;
   long		*long_const;
# endif


   TRACE (Func_Entry, "pre_parse_format", NULL);

  /* KAY - should I always ask for ANSI because of the disable message stuff */

   caller_flag = (on_off_flags.issue_ansi_messages) ? COMPILER_CALL_ANSI_95 : 
                                                      COMPILER_CALL_NO_ANSI;
   format_len  = (long) CN_INT_TO_C(TYP_IDX(CN_TYPE_IDX(const_idx))) - 
                                    lbl_name_len;
   the_func    = &emit_format_msg;

/* bhj wants this to hang around please */

   new_fmt = _fmt_parse(&the_func,
                        (char *)&CN_CONST(const_idx) + lbl_name_len,
                        caller_flag,
                        &format_len,
                        &unused_boolean);

   /* Word 1 of the new_fmt is reserved for use by the compiler.      */
   /* Currently, it is unused.   Word 2 of the new_fmt is reserved    */
   /* for use by the library to keep track of what level it is.       */

   /* We put the pre-parsed format into the constant table as a       */
   /* Typeless constant. The argument format_len is the word length   */
   /* of the pre-parsed format.                                       */

   if (new_fmt != NULL) {


# if defined(_HOST32) && defined(_TARGET64)
      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)       = Typeless;
      TYP_BIT_LEN(TYP_WORK_IDX)    = format_len * HOST_BITS_PER_WORD;
      type_idx                     = ntr_type_tbl();

      pre_parse_idx = ntr_const_tbl(type_idx, FALSE, NULL);

      long_const = (long *)&CN_CONST(pre_parse_idx);

      for (i = 0; i < format_len; i++) {
         long_const[i] = new_fmt[i];
      }
# else 
      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)	= Typeless;
      TYP_BIT_LEN(TYP_WORK_IDX)	= format_len * TARGET_BITS_PER_WORD;
      type_idx			= ntr_type_tbl();

      pre_parse_idx = ntr_const_tbl(type_idx, FALSE, (long_type *)new_fmt);
# endif

      MEM_FREE(new_fmt);
   }
   else {
      pre_parse_idx = NULL_IDX;
   }

   TRACE (Func_Exit, "pre_parse_format", NULL);

   return(pre_parse_idx);

}  /* pre_parse_format */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Create a tmp with a type of a one-dimensional array to use for the    *|
|*      format string and pre-parsed format.                                  *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

static int create_format_tmp (int      const_idx)

{
   int         		attr_idx;
   int         	 	bd_idx;
   int         	 	cn_idx;
   int			ir_idx;
   int			list1_idx;
   int			list2_idx;
   int			list3_idx;
   long64		num_bits;
#ifdef KEY /* Bug 10177 */
   long64		num_els = 0;
#else /* KEY Bug 10177 */
   long64		num_els;
#endif /* KEY Bug 10177 */
   size_offset_type	stride;


   TRACE (Func_Entry, "create_format_tmp", NULL);

   attr_idx                     = gen_compiler_tmp(stmt_start_line,
                                                   stmt_start_col,
                                                   Shared, TRUE);
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   ATD_TYPE_IDX(attr_idx)	= Integer_8;
# else
   ATD_TYPE_IDX(attr_idx)	= CG_INTEGER_DEFAULT_TYPE;
# endif
   ATD_SAVED(attr_idx)          = TRUE;
   ATD_DATA_INIT(attr_idx)	= TRUE;
   ATD_STOR_BLK_IDX(attr_idx)   = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
   AT_SEMANTICS_DONE(attr_idx)  = TRUE;
   ATD_READ_ONLY_VAR(attr_idx)  = TRUE;

   if (TYP_TYPE(CN_TYPE_IDX(const_idx)) == Character) {
      num_els = 1L + 
           TARGET_BYTES_TO_WORDS(CN_INT_TO_C(TYP_IDX(CN_TYPE_IDX(const_idx))));
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifndef _WHIRL_HOST64_TARGET64
      num_els = (num_els + 1) / 2;
# endif
      num_bits = num_els * 64;
# else
      num_bits			= num_els * TARGET_BITS_PER_WORD;
# endif
      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)		= Typeless;
      TYP_BIT_LEN(TYP_WORK_IDX)		= num_bits;
      CN_TYPE_IDX(const_idx)		= ntr_type_tbl();
      CN_EXTRA_ZERO_WORD(const_idx)	= FALSE;
   }
   else if (TYP_TYPE(CN_TYPE_IDX(const_idx)) == Typeless) {

      num_els = TARGET_BITS_TO_WORDS((long)TYP_BIT_LEN(CN_TYPE_IDX(const_idx)));

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifndef _WHIRL_HOST64_TARGET64
      num_els = (num_els + 1) / 2;
# endif
      num_bits = num_els * 64;                    /* BRIANJ num_bits not used.*/
# else
      num_bits = num_els * TARGET_BITS_PER_WORD;  /* BRIANJ num_bits not used.*/
# endif
   }

   cn_idx = C_INT_TO_CN(NULL_IDX, num_els);
   
   bd_idx			= reserve_array_ntry(1);

   set_stride_for_first_dim(ATD_TYPE_IDX(attr_idx), &stride);

   BD_RESOLVED(bd_idx)		= TRUE;
   BD_ARRAY_CLASS(bd_idx)	= Explicit_Shape;
   BD_ARRAY_SIZE(bd_idx)	= Constant_Size;
   BD_RANK(bd_idx)		= 1;
   BD_LEN_FLD(bd_idx)		= CN_Tbl_Idx;
   BD_LEN_IDX(bd_idx)		= cn_idx;
   BD_LINE_NUM(bd_idx)		= stmt_start_line;
   BD_COLUMN_NUM(bd_idx)	= stmt_start_col;
   BD_LB_FLD(bd_idx,1)		= CN_Tbl_Idx;
   BD_LB_IDX(bd_idx,1)		= CN_INTEGER_ONE_IDX;
   BD_UB_FLD(bd_idx,1)		= CN_Tbl_Idx;
   BD_UB_IDX(bd_idx,1)		= cn_idx;
   BD_XT_FLD(bd_idx,1)		= CN_Tbl_Idx;
   BD_XT_IDX(bd_idx,1)		= cn_idx;
   BD_SM_FLD(bd_idx,1)		= stride.fld;
   BD_SM_IDX(bd_idx,1)		= stride.idx;
   ATD_ARRAY_IDX(attr_idx)	= bd_idx;

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Init_Opr;
   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = stmt_start_line;
   IR_COL_NUM(ir_idx)  = stmt_start_col;

   IR_FLD_L(ir_idx) = AT_Tbl_Idx;
   IR_IDX_L(ir_idx) = attr_idx;
   IR_LINE_NUM_L(ir_idx) = stmt_start_line;
   IR_COL_NUM_L(ir_idx)  = stmt_start_col;

   NTR_IR_LIST_TBL(list1_idx);
   NTR_IR_LIST_TBL(list2_idx);
   NTR_IR_LIST_TBL(list3_idx);

   IR_FLD_R(ir_idx) = IL_Tbl_Idx;
   IR_IDX_R(ir_idx) = list1_idx;
   IR_LIST_CNT_R(ir_idx) = 3;

   IL_NEXT_LIST_IDX(list1_idx) = list2_idx;
   IL_PREV_LIST_IDX(list2_idx) = list1_idx;

   IL_NEXT_LIST_IDX(list2_idx) = list3_idx;
   IL_PREV_LIST_IDX(list3_idx) = list2_idx;

   IL_FLD(list1_idx) = CN_Tbl_Idx;
   IL_IDX(list1_idx) = const_idx;
   IL_LINE_NUM(list1_idx) = stmt_start_line;
   IL_COL_NUM(list1_idx)  = stmt_start_col;

   IL_FLD(list2_idx) = CN_Tbl_Idx;
   IL_IDX(list2_idx) = CN_INTEGER_ONE_IDX;
   IL_LINE_NUM(list2_idx) = stmt_start_line;
   IL_COL_NUM(list2_idx)  = stmt_start_col;

   IL_FLD(list3_idx) = CN_Tbl_Idx;
   IL_IDX(list3_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(list3_idx) = stmt_start_line;
   IL_COL_NUM(list3_idx)  = stmt_start_col;

   ATD_FLD(attr_idx)     = CN_Tbl_Idx;
   ATD_TMP_IDX(attr_idx) = const_idx;

   gen_sh(Before, Assignment_Stmt, stmt_start_line,
                   stmt_start_col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = ir_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   TRACE (Func_Exit, "create_format_tmp", NULL);

   return(attr_idx);

}  /* create_format_tmp */
