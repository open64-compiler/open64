/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


/* USMID:  "\n@(#)5.0_pl/macros/cond_comp.m	5.1	04/29/99 21:22:31\n" */


/*****************\
|* MISCELLANEOUS *|
\*****************/

# define CC_MAX_ID_LEN		133
# ifdef _HOST64
# define CC_NUM_ID_WDS		 17
# define CC_NUM_AT_WDS		  2
# else
# define CC_NUM_ID_WDS		 34
# define CC_NUM_AT_WDS		  4
# endif


/********************\
|* SIZES AND LIMITS *|
\********************/



/******************************\
|* OBJECT REPLACEMENT STRINGS *|
\******************************/

# define  CC_LA_CH_CLASS                cc_la_ch.ch_class
# define  CC_LA_CH_COLUMN               cc_la_ch.column
# define  CC_LA_CH_LINE                 cc_la_ch.line
# define  CC_LA_CH_VALUE                cc_la_ch.value

# define  CC_NEXT_LA_CH			cc_get_next_char()

/* LOCAL NAME TABLE */

# define CC_LN_ATTR_IDX(IDX)               cc_ln_tbl[IDX].attr_idx
# define CC_LN_NAME_IDX(IDX)               cc_ln_tbl[IDX].name_idx
# define CC_LN_NAME_LEN(IDX)               cc_ln_tbl[IDX].name_len
# define CC_LN_NAME_LONG(IDX)           ((long *)&str_pool[CC_LN_NAME_IDX(IDX)])
# define CC_LN_NAME_PTR(IDX)            ((char *)&str_pool[CC_LN_NAME_IDX(IDX)])


# define CC_AT_NAME_LEN(IDX)		cc_attr_tbl[IDX].name_len
# define CC_AT_NAME_IDX(IDX)		cc_attr_tbl[IDX].name_idx
# define CC_AT_DEFINED(IDX)		cc_attr_tbl[IDX].defined
# define CC_AT_DYNAMIC_PREDEF(IDX)	cc_attr_tbl[IDX].dynamic_predef
# define CC_AT_STR_IDX(IDX)		cc_attr_tbl[IDX].str_idx
# define CC_AT_STR_LEN(IDX)		cc_attr_tbl[IDX].str_len
# define CC_AT_NUM_ARGS(IDX)		cc_attr_tbl[IDX].num_args
# define CC_AT_START_LINE(IDX)		cc_attr_tbl[IDX].start_line
# define CC_AT_START_COL(IDX)		cc_attr_tbl[IDX].start_col
# define CC_AT_NAME_PTR(IDX)		((char *)&str_pool[CC_AT_NAME_IDX(IDX)])
# define CC_AT_STR_PTR(IDX)		((char *)&str_pool[CC_AT_STR_IDX(IDX)])

# define CC_CURR_BLK_TYPE	cc_blk_stk_tbl[cc_blk_stk_tbl_idx].blk_type
# define CC_CURR_BLK_IS_ACTIVE	cc_blk_stk_tbl[cc_blk_stk_tbl_idx].is_active
# define CC_CURR_BLK_IN_ERROR	cc_blk_stk_tbl[cc_blk_stk_tbl_idx].in_error
# define CC_CURR_BLK_DONE	cc_blk_stk_tbl[cc_blk_stk_tbl_idx].already_done


/***********************************\
|* CONDITIONAL REPLACEMENT STRINGS *|
\***********************************/

# define VALID_CC_ID_CHAR(CHAR)   (ch_class[CHAR] == Ch_Class_Letter  ||   \
                                   ch_class[CHAR] == Ch_Class_Digit   ||   \
                                   CHAR == underscore || CHAR == dollar || \
                                   CHAR == at_sign)


/***********************************************\
|* STATEMENT/FUNCTION-LIKE REPLACEMENT STRINGS *|
\***********************************************/

# define ADD_TO_CC_TOKEN_STR(CH,LEN)                                           \
                if ((LEN) < CC_MAX_ID_LEN) {                                   \
                   TOKEN_STR(cc_token)[(LEN)] = (CH);                          \
                }                                                              \
                (LEN)++

# define NTR_CC_ATTR_TBL(ATTR_IDX)                                             \
         TBL_REALLOC_CK(cc_attr_tbl, 1);                                       \
         CLEAR_TBL_NTRY(cc_attr_tbl, cc_attr_tbl_idx);                         \
         ATTR_IDX = cc_attr_tbl_idx;

# define NTR_CC_NAME_POOL(NAME, LEN, NP_IDX)                                   \
        {register long  *_name_id;                                             \
         register int    _np_idx;                                              \
         register int    _start_idx;                                           \
         register int    _wd_len;                                              \
         _name_id       = NAME;                                                \
         _wd_len        = WORD_LEN(LEN);                                       \
         _start_idx     = str_pool_idx + 1;                                    \
         NP_IDX         = _start_idx;                                          \
         TBL_REALLOC_CK(str_pool,_wd_len);                                     \
         for (_np_idx = 0; _np_idx < _wd_len; _np_idx++) {                     \
             str_pool[_start_idx+_np_idx].name_long = _name_id[_np_idx];       \
         }                                                                     \
        }


# define POP_CC_BLK_STK                                                        \
                cc_blk_stk_tbl_idx--;

# define PUSH_CC_BLK_STK(NEW_BLK)                                              \
                TBL_REALLOC_CK (cc_blk_stk_tbl, 1);                            \
                CLEAR_TBL_NTRY(cc_blk_stk_tbl, cc_blk_stk_tbl_idx);            \
                CC_CURR_BLK_TYPE = NEW_BLK;


# define PUT_VALUE_IN_AT_STR(ATTR_IDX, VALUE)                                  \
	{int _len; sprintf(temp_id_str.string,"%d",VALUE);                     \
         _len = strlen(temp_id_str.string);                                    \
        CC_AT_STR_LEN(ATTR_IDX) = _len;                                        \
	NTR_CC_NAME_POOL(temp_id_str.words, _len, CC_AT_STR_IDX(ATTR_IDX));}

# define CC_GET_ID_TOKEN                                                       \
         cc_token = cc_initial_token;                                          \
         TOKEN_LEN(cc_token) = 0;                                              \
         TOKEN_LINE(cc_token) = PP_LINE_NUM;                                   \
         TOKEN_COLUMN(cc_token) = nxt_line_col[cc_stmt_buf_idx];               \
         while (nxt_line[cc_stmt_buf_idx] > 0 &&                               \
                VALID_CC_ID_CHAR(nxt_line[cc_stmt_buf_idx])) {                 \
            if (TOKEN_LEN(cc_token) < CC_MAX_ID_LEN) {                         \
               ADD_TO_CC_TOKEN_STR(nxt_line[cc_stmt_buf_idx],                  \
                                   TOKEN_LEN(cc_token));                       \
            }                                                                  \
            cc_advance_idx();                                                  \
         }
