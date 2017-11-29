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


/* USMID:  "\n@(#)5.0_pl/macros/lex.m	5.3	07/28/99 12:17:30\n" */
 

/*****************\
|* MISCELLANEOUS *|
\*****************/

/********************\
|* SIZES AND LIMITS *|
\********************/

# define MAX_BIN_CONST_LEN	256		/* max binary const len	      */
# define MAX_HEX_CONST_LEN	64		/* max hex const len	      */
# define MAX_OCT_CONST_LEN	86		/* max octal const len	      */

# define MAX_CHAR_CONST_LEN	1320		/* max character const len    */
# define MAX_KWD_LEN		31		/* max keyword len	      */

/******************************\
|* OBJECT REPLACEMENT STRINGS *|
\******************************/


/***********************************\
|* CONDITIONAL REPLACEMENT STRINGS *|
\***********************************/

# define IS_BIN_DIGIT(CH)	((((CH) - '0') >> 1) == 0)
# define IS_OCT_DIGIT(CH)	((((CH) - '0') >> 3) == 0)

# define VALID_LA_CH		((LA_CH_CLASS == Ch_Class_Letter ||	       \
				 LA_CH_CLASS == Ch_Class_Digit	 ||	       \
				 LA_CH_VALUE == USCORE		 ||	       \
				 LA_CH_VALUE == DOLLAR		 ||	       \
				 LA_CH_VALUE == AT_SIGN)         &&            \
                                 !sig_blank)



/***********************************************\
|* STATEMENT/FUNCTION-LIKE REPLACEMENT STRINGS *|
\***********************************************/

# define CHECK_FOR_FREE_BLANK                                                  \
		if (source_form == Free_Form        &&                         \
                    VALID_LA_CH)                    {                          \
                   PRINTMSG(LA_CH_LINE, 415, Error, LA_CH_COLUMN);             \
                }
                     
# define ADD_TO_TOKEN_KIND_STR(CH,LEN)					       \
		if ((LEN) < MAX_ID_LEN) {				       \
		   TOKEN_KIND_STR(token)[(LEN)] = (CH);			       \
		}							       \
		(LEN)++

# define ADD_TO_TOKEN_STR(CH,LEN)					       \
		if ((LEN) < MAX_ID_LEN) {				       \
		   TOKEN_STR(token)[(LEN)] = (CH);			       \
		}							       \
		(LEN)++

# define ADD_TO_CONST_BUF(CH,LEN)                                              \
                if ((LEN) < MAX_CHAR_CONST_LEN) {                              \
                   const_buf[(LEN)] = (CH);                                    \
                }                                                              \
                (LEN)++

# define NEXT_LA_CH_LITERAL						       \
		(*get_char_literal) ()

# ifdef _WARNING_FOR_NUMERIC_INPUT_ERROR
# define OVERFLOW_MESSAGE(RESULT)                                              \
                PRINTMSG(TOKEN_LINE(token),1413,Warning,TOKEN_COLUMN(token));
# else
# define OVERFLOW_MESSAGE(RESULT)                                              \
                    PRINTMSG(TOKEN_LINE(token),374,Error,TOKEN_COLUMN(token)); \
                    RESULT = FALSE;
# endif

# ifdef _USE_FOLD_DOT_f

#ifdef KEY /* Bug 5554 */
# define CONVERT_INT_CONST(TYPE, LEN, RESULT)                                  \
       {const_buf[(LEN)] = '\0';                                               \
	RESULT = kludge_input_conversion(const_buf, TYPE, 0); }

/* Like CONVERT_INT_CONST, but sets NEW_TYPE to indicate the type resulting
 * from the conversion (we might have to promote to a higher precision to
 * avoid overflow.)
 */
# define CONVERT_INT_CONST_AND_PROMOTE(TYPE, LEN, RESULT)            \
       {const_buf[(LEN)] = '\0';                                               \
	RESULT = kludge_input_conversion(const_buf, TYPE, TRUE); }

# define CONVERT_REAL_CONST(TYPE, LEN, RESULT)                                 \
       {const_buf[(LEN)] = '\0';                                               \
	RESULT = kludge_input_conversion(const_buf, TYPE, FALSE); }

# define CONVERT_DBL_CONST(TYPE, LEN, RESULT)                                  \
       {const_buf[(LEN)] = '\0';                                               \
	RESULT = kludge_input_conversion(const_buf, TYPE, FALSE); }
#else /* KEY Bug 5554 */

# define CONVERT_INT_CONST(TYPE, LEN, RESULT)                                  \
       {const_buf[(LEN)] = '\0';                                               \
	kludge_input_conversion(const_buf, TYPE); }

# define CONVERT_REAL_CONST(TYPE, LEN, RESULT)                                 \
       {const_buf[(LEN)] = '\0';                                               \
	kludge_input_conversion(const_buf, TYPE); }

# define CONVERT_DBL_CONST(TYPE, LEN, RESULT)                                  \
       {const_buf[(LEN)] = '\0';                                               \
	kludge_input_conversion(const_buf, TYPE); }

#endif /* KEY Bug 5554 */

# else

# ifdef _ARITH_INPUT_CONV

# ifdef _TARGET64
# define CONVERT_INT_CONST(TYPE, LEN, RESULT)                                  \
                { aligned_value_type _const,_tmp_const;                        \
		  int _stat, _bits, _base, _cvrt_lin_type;                     \
                  const_buf[(LEN)] = '\0';                                     \
                 _base = 10;                                                   \
                 _stat = AR_convert_str_to_int((AR_DATA *)_tmp_const.v,        \
                         (const AR_TYPE *)&input_arith_type[TYP_LINEAR(TYPE)], \
                                                &_bits,                        \
                               (const char *)const_buf,                        \
                               (const int *)&_base);                           \
                 if (((_stat & AR_STAT_OVERFLOW) != 0)   ||                    \
                     ((_stat & AR_STAT_UNDERFLOW) != 0)  ||                    \
                     ((_stat & AR_STAT_SEMIVALID) != 0)  ||                    \
                     ((_stat & AR_STAT_UNDEFINED) != 0)) {                     \
                    OVERFLOW_MESSAGE(RESULT);                                  \
                 }                                                             \
                 else if ((_stat & AR_STAT_INVALID_TYPE) != 0) {               \
                    PRINTMSG(TOKEN_LINE(token),1016,Internal,                  \
                             TOKEN_COLUMN(token),                              \
                             arith_type_string[TYP_LINEAR(TYPE)]);             \
                 }                                                             \
                 if (RESULT == TRUE)  {                                        \
                    _cvrt_lin_type = TYP_LINEAR(TYPE);                         \
                    _stat = AR_convert((AR_DATA *)_const.v,                    \
                         (const AR_TYPE *)&linear_to_arith[_cvrt_lin_type],    \
                              (const AR_DATA *)_tmp_const.v,                   \
                       (const AR_TYPE *)&input_arith_type[TYP_LINEAR(TYPE)]);  \
                    SHIFT_ARITH_RESULT(_const.v, TYP_LINEAR(TYPE));            \
                    if (((_stat & AR_STAT_OVERFLOW) != 0)   ||                 \
                        ((_stat & AR_STAT_UNDERFLOW) != 0)  ||                 \
                        ((_stat & AR_STAT_SEMIVALID) != 0)  ||                 \
                        ((_stat & AR_STAT_UNDEFINED) != 0)) {                  \
                      OVERFLOW_MESSAGE(RESULT);                                \
                    }                                                          \
                    else if ((_stat & AR_STAT_INVALID_TYPE) != 0) {            \
                      PRINTMSG(TOKEN_LINE(token),1016,Internal,                \
                               TOKEN_COLUMN(token),                            \
                               arith_type_string[TYP_LINEAR(TYPE)]);           \
                    }                                                          \
                    if (RESULT == TRUE) {                                      \
                       TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(TYPE,        \
                                                             FALSE, _const.v); \
                    }                                                          \
                 }                                                             \
                }

# else

# define CONVERT_INT_CONST(TYPE, LEN, RESULT)                                  \
                { aligned_value_type _const,_tmp_const;                        \
		  int _stat, _bits, _base, _cvrt_lin_type;                     \
                  const_buf[(LEN)] = '\0';                                     \
                 _base = 10;                                                   \
                 _stat = AR_convert_str_to_int((AR_DATA *)_tmp_const.v,        \
                         (const AR_TYPE *)&input_arith_type[TYP_LINEAR(TYPE)], \
                                                &_bits,                        \
                               (const char *)const_buf,                        \
                               (const int *)&_base);                           \
                 if (((_stat & AR_STAT_OVERFLOW) != 0)   ||                    \
                     ((_stat & AR_STAT_UNDERFLOW) != 0)  ||                    \
                     ((_stat & AR_STAT_SEMIVALID) != 0)  ||                    \
                     ((_stat & AR_STAT_UNDEFINED) != 0)) {                     \
                    OVERFLOW_MESSAGE(RESULT);                                  \
                 }                                                             \
                 else if ((_stat & AR_STAT_INVALID_TYPE) != 0) {               \
                    PRINTMSG(TOKEN_LINE(token),1016,Internal,                  \
                             TOKEN_COLUMN(token),                              \
                             arith_type_string[TYP_LINEAR(TYPE)]);             \
                 }                                                             \
                 if (RESULT == TRUE) {                                         \
                    _cvrt_lin_type = TYP_LINEAR(TYPE);                         \
                    _stat = AR_convert((AR_DATA *)_const.v,                    \
                         (const AR_TYPE *)&linear_to_arith[_cvrt_lin_type],    \
                              (const AR_DATA *)_tmp_const.v,                   \
                       (const AR_TYPE *)&input_arith_type[TYP_LINEAR(TYPE)]);  \
                    SHIFT_ARITH_RESULT(_const.v, TYP_LINEAR(TYPE));            \
                    if (((_stat & AR_STAT_OVERFLOW) != 0)   ||                 \
                        ((_stat & AR_STAT_UNDERFLOW) != 0)  ||                 \
                        ((_stat & AR_STAT_SEMIVALID) != 0)  ||                 \
                        ((_stat & AR_STAT_UNDEFINED) != 0)) {                  \
                      OVERFLOW_MESSAGE(RESULT);                                \
                    }                                                          \
                    else if ((_stat & AR_STAT_INVALID_TYPE) != 0) {            \
                      PRINTMSG(TOKEN_LINE(token),1016,Internal,                \
                               TOKEN_COLUMN(token),                            \
                               arith_type_string[TYP_LINEAR(TYPE)]);           \
                    }                                                          \
                    if (RESULT == TRUE) {                                      \
                       TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(TYPE,        \
                                                             FALSE, _const.v); \
                    }                                                          \
                 }                                                             \
                }
# endif

# define CONVERT_REAL_CONST(TYPE, LEN, RESULT)                                 \
                { aligned_value_type _const;                                   \
                  int _stat;                                                   \
                  const_buf[(LEN)] = '\0';                                     \
                 _stat = AR_convert_str_to_float((AR_DATA *)_const.v,          \
                         (const AR_TYPE *)&input_arith_type[TYP_LINEAR(TYPE)], \
                               (const char *)const_buf);                       \
                 SHIFT_ARITH_RESULT(_const.v, TYP_LINEAR(TYPE));               \
                 if (((_stat & AR_STAT_OVERFLOW) != 0)   ||                    \
                     ((_stat & AR_STAT_UNDERFLOW) != 0)  ||                    \
                     ((_stat & AR_STAT_SEMIVALID) != 0)  ||                    \
                     ((_stat & AR_STAT_UNDEFINED) != 0)) {                     \
                    if (target_ieee) {                                         \
                       PRINTMSG(TOKEN_LINE(token),1189,Warning,                \
                                TOKEN_COLUMN(token));                          \
                       TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(TYPE,        \
                                                             FALSE, _const.v); \
                    } else {                                                   \
                       OVERFLOW_MESSAGE(RESULT);                               \
                    }                                                          \
                 }                                                             \
                 else if ((_stat & AR_STAT_INVALID_TYPE) != 0) {               \
                    PRINTMSG(TOKEN_LINE(token),1016,Internal,                  \
                             TOKEN_COLUMN(token),                              \
                             arith_type_string[TYP_LINEAR(TYPE)]);             \
                 }                                                             \
                 if (RESULT == TRUE) {                                         \
                    TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(TYPE,           \
                                                             FALSE, _const.v); \
                 }                                                             \
                }

# define CONVERT_DBL_CONST(TYPE, LEN, RESULT)                                  \
                { aligned_value_type _const;                                   \
                  int _stat;                                                   \
                  const_buf[(LEN)] = '\0';                                     \
                 _stat = AR_convert_str_to_float((AR_DATA *)_const.v,          \
                         (const AR_TYPE *)&input_arith_type[TYP_LINEAR(TYPE)], \
                               (const char *)const_buf);                       \
                 SHIFT_ARITH_RESULT(_const.v, TYP_LINEAR(TYPE));               \
                 if (((_stat & AR_STAT_OVERFLOW) != 0)   ||                    \
                     ((_stat & AR_STAT_UNDERFLOW) != 0)  ||                    \
                     ((_stat & AR_STAT_SEMIVALID) != 0)  ||                    \
                     ((_stat & AR_STAT_UNDEFINED) != 0)) {                     \
                    if (target_ieee) {                                         \
                       PRINTMSG(TOKEN_LINE(token),1189,Warning,                \
                                TOKEN_COLUMN(token));                          \
                       TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(TYPE,        \
                                                             FALSE, _const.v); \
                    } else {                                                   \
                       OVERFLOW_MESSAGE(RESULT);                               \
                    }                                                          \
                 }                                                             \
                 else if ((_stat & AR_STAT_INVALID_TYPE) != 0) {               \
                    PRINTMSG(TOKEN_LINE(token),1016,Internal,                  \
                             TOKEN_COLUMN(token),                              \
                             arith_type_string[TYP_LINEAR(TYPE)]);             \
                 }                                                             \
                 if (RESULT == TRUE) {                                         \
                    TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(TYPE,           \
                                                             FALSE, _const.v); \
                 }                                                             \
                }

# else

# if defined(_HOST_OS_SOLARIS)

# define CONVERT_INT_CONST(TYPE, LEN, RESULT)				       \
		{ long_type	_const;                                        \
		  const_buf[(LEN)] = '\0';                                     \
                  errno = 0;                                                   \
		  _const = strtol(const_buf, (char **) NULL, 10);              \
                  if (errno == 0) {                                            \
		     TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(TYPE,          \
                                                             FALSE, &_const);  \
                  } else {                                                     \
                    OVERFLOW_MESSAGE(RESULT);                                  \
                  }                                                            \
		}

# define CONVERT_REAL_CONST(TYPE, LEN, RESULT)	          		       \
		{ long_type _const[MAX_WORDS_FOR_NUMERIC];                     \
                  double _tmp_const;                                           \
                  int	_stat;                                                 \
		  const_buf[(LEN)] = '\0';                                     \
                  errno = 0;                                                   \
		  _tmp_const = strtod(const_buf, (char **)NULL);               \
                  if (errno != 0) {                                            \
                    OVERFLOW_MESSAGE(RESULT);                                  \
                  }                                                            \
                  else {                                                       \
                     _stat = AR_convert((AR_DATA *)_const,                     \
                          (const AR_TYPE *)&linear_to_arith[TYP_LINEAR(TYPE)], \
                               (const AR_DATA *)&_tmp_const,                   \
                     (const AR_TYPE *)&linear_to_arith[Real_8]);               \
                     SHIFT_ARITH_RESULT(_const, TYP_LINEAR(TYPE));             \
                    if (((_stat & AR_STAT_OVERFLOW) != 0)   ||                 \
                   /*   ((_stat & AR_STAT_UNDERFLOW) != 0)  ||   */            \
                        ((_stat & AR_STAT_SEMIVALID) != 0)  ||                 \
                        ((_stat & AR_STAT_UNDEFINED) != 0)) {                  \
                       OVERFLOW_MESSAGE(RESULT);                               \
                     }                                                         \
                     else if ((_stat & AR_STAT_INVALID_TYPE) != 0) {           \
                       PRINTMSG(TOKEN_LINE(token),1016,Internal,               \
                                TOKEN_COLUMN(token),                           \
                                arith_type_string[TYP_LINEAR(TYPE)]);          \
                     }                                                         \
                     else {                                                    \
		       TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(TYPE,        \
				                 FALSE, _const);               \
                     }                                                         \
                  }                                                            \
		}

# define CONVERT_DBL_CONST(TYPE, LEN, RESULT)			               \
		{ double _const;                                               \
		  const_buf[(LEN)] = '\0';                                     \
                  errno = 0;                                                   \
		  _const = strtod(const_buf, (char **)NULL);                   \
                  if (errno == 0) {                                            \
		    TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(TYPE,           \
                                                 FALSE, (long_type *)&_const); \
                  } else {                                                     \
                    OVERFLOW_MESSAGE(RESULT);                                  \
                  }                                                            \
		}
# elif defined(_HOST32) && defined(_TARGET64)

# define CONVERT_INT_CONST(TYPE, LEN, RESULT)				       \
		{ long_type	_const;                                        \
		  const_buf[(LEN)] = '\0';                                     \
                  errno = 0;                                                   \
		  _const = strtoll(const_buf, (char **) NULL, 10);             \
                  if (errno == 0) {                                            \
		     TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(TYPE,          \
                                                             FALSE, &_const);  \
                  } else {                                                     \
                    OVERFLOW_MESSAGE(RESULT);                                  \
                  }                                                            \
		}

# define CONVERT_REAL_CONST(TYPE_IDX, LEN, RESULT)			       \
		{ float_type _const;                                           \
		  const_buf[(LEN)] = '\0';                                     \
                  errno = 0;                                                   \
		  _const = (float_type)atof(const_buf);                        \
                  if (errno == 0) {                                            \
		    TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(TYPE_IDX,       \
				                 FALSE, (long_type *)&_const); \
                  } else {                                                     \
                    OVERFLOW_MESSAGE(RESULT);                                  \
                  }                                                            \
		}

# define CONVERT_DBL_CONST(TYPE_IDX, LEN, RESULT)			       \
		{ ldouble _const;                                              \
		  const_buf[(LEN)] = '\0';                                     \
                  errno = 0;                                                   \
		  _const = (ldouble)atof(const_buf);                           \
                  if (errno == 0) {                                            \
		    TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(TYPE_IDX,       \
                                                 FALSE, (long_type *)&_const); \
                  } else {                                                     \
                    OVERFLOW_MESSAGE(RESULT);                                  \
                  }                                                            \
		}

# else

# define CONVERT_INT_CONST(TYPE, LEN, RESULT)				       \
		{ long_type	_const;                                        \
		  const_buf[(LEN)] = '\0';                                     \
                  errno = 0;                                                   \
		  _const = LEX_STRTOL(const_buf, (char **) NULL, 10);          \
                  if (errno == 0) {                                            \
		     TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(TYPE,          \
                                                             FALSE, &_const);  \
                  } else {                                                     \
                    OVERFLOW_MESSAGE(RESULT);                                  \
                  }                                                            \
		}

# define CONVERT_REAL_CONST(TYPE, LEN, RESULT)			               \
		{ float _const;                                                \
		  const_buf[(LEN)] = '\0';                                     \
                  errno = 0;                                                   \
		  _const = (float)atof(const_buf);                             \
                  if (errno == 0) {                                            \
		    TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(TYPE,           \
				                 FALSE, (long_type *)&_const); \
                  } else {                                                     \
                    OVERFLOW_MESSAGE(RESULT);                                  \
                  }                                                            \
		}

# define CONVERT_DBL_CONST(TYPE, LEN, RESULT)			               \
		{ double _const;                                               \
		  const_buf[(LEN)] = '\0';                                     \
                  errno = 0;                                                   \
		  _const = atof(const_buf);                                    \
                  if (errno == 0) {                                            \
		    TOKEN_CONST_TBL_IDX(token) = ntr_const_tbl(TYPE,           \
                                                 FALSE, (long_type *)&_const); \
                  } else {                                                     \
                    OVERFLOW_MESSAGE(RESULT);                                  \
                  }                                                            \
		}
# endif
# endif
# endif
