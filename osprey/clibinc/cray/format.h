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

/* USMID @(#) clibinc/cray/format.h	92.0	10/08/98 14:28:05 */
#ifndef	_CRAY_FORMAT_H
#define	_CRAY_FORMAT_H

#ifndef	NULL
#   define	NULL	0
#endif

#ifndef	FALSE
#   define	FALSE	0
#endif

#ifndef	TRUE
#   define	TRUE	1
#endif

/*
 * The following symbol identifies the level of the current parser.  If
 * the library is passed a parsed format with a different level than it's
 * expecting, it will reparse the format.
 */

#define	PARSER_LEVEL	3	/* Identifying level of the parser */

/* Edit descriptors and operators which appear in the parsed format */

#define FIRST_DATA_ED	1
#define A_ED		1
#define B_ED		2
#define D_ED		3
#define E_ED		4
#define EN_ED		5
#define ES_ED		6
#define F_ED		7
#define G_ED		8
#define I_ED		9
#define L_ED		10
#define O_ED		11
#define R_ED		12	/* CRI extension */
#define Z_ED		13
#define	Q_ED		14	/* MIPSpro extension */
#define LAST_DATA_ED	14

#define FIRST_CNTL_ED	20
#define SLASH_ED	20
#define P_ED		21
#define STRING_ED	22
#define BN_ED		23
#define BZ_ED		24
#define COLON_ED	25
#define S_ED		26
#define SP_ED		27
#define SS_ED		28
#define TL_ED		29
#define TR_ED		30
#define T_ED		31
#define DOLLAR_ED	32	/* CRI extension */
#define LAST_CNTL_ED	32

#define	FIRST_OP	40
#define REPEAT_OP	40
#define ENDREP_OP	41
#define REVERT_OP	42
#define LAST_OP		42	/* Last valid format operator */

/* There are dependencies on this order */

#ifdef	_OLD_ERROR_NUMBERS

#define END_OF_MESSAGES			0
#define TRAILING_CHARS			1
#define FIELD_TOO_SMALL			2	/* Deprecated */

#define FIRST_NON_ANSI_MESSAGE		3
#define ANSI_EMPTY_PAREN_MSG		3
#define ANSI_COMMA_REQ			4
#define COMMA_NON_ANSI			5
#define REP_SLASH_NON_ANSI		6
#define NON_ANSI_EDIT_DESCRIPTOR	7
#define MINUS_X_NON_ANSI		8
#define E_WITH_D_NON_ANSI		9
#define	H_IS_OBSOLETE_IN_F90		10
#define LAST_NON_ANSI_MESSAGE		10

#define FIRST_FATAL_MESSAGE		11
#define EXPECTING_LEFT_PAREN		11
#define EXPECTING_RIGHT_PAREN		12
#define EXPECTING_INTEGER		13
#define EXPECTING_PERIOD		14
#define EXPECTING_P_OR_X		15
#define INVALID_REP_COUNT		16
#define ZERO_REP_COUNT			17
#define FIELD_WIDTH_ZERO		18
#define FIELD_TOO_LARGE			19
#define ZERO_OR_NO_HOLLERITH_CNT	20
#define UNKNOWN_EDIT_DESCRIPTOR		21
#define NONTERMINATED_LITERAL		22
#define UNABLE_TO_MALLOC_MEMORY		23

#else	/* _OLD_ERROR_NUMBERS */

#define END_OF_MESSAGES			0
#define FIRST_WARNING_MESSAGE		1
#define TRAILING_CHARS			1

#define FIRST_NON_ANSI_MESSAGE		10
#define ANSI_EMPTY_PAREN_MSG		10
#define ANSI_COMMA_REQ			11
#define COMMA_NON_ANSI			12
#define REP_SLASH_NON_ANSI		13
#define NON_ANSI_EDIT_DESCRIPTOR	14
#define MINUS_X_NON_ANSI		15
#define E_WITH_D_NON_ANSI		16
#define	H_IS_OBSOLETE_IN_F90		17
#define	NON_ANSI_NULL_DESCRIPTOR	18
#define	ZERO_WIDTH_NON_ANSI		19
#define	MISSING_WIDTH_NON_ANSI		20
#define LAST_NON_ANSI_MESSAGE		20

#define FIRST_FATAL_MESSAGE		30
#define EXPECTING_LEFT_PAREN		30
#define EXPECTING_RIGHT_PAREN		31
#define EXPECTING_INTEGER		32
#define EXPECTING_PERIOD		33
#define EXPECTING_P_OR_X		34
#define INVALID_REP_COUNT		35
#define ZERO_REP_COUNT			36
#define FIELD_WIDTH_ZERO		37
#define FIELD_TOO_LARGE			38
#define ZERO_OR_NO_HOLLERITH_CNT	39
#define UNKNOWN_EDIT_DESCRIPTOR		40
#define NONTERMINATED_LITERAL		41
#define UNABLE_TO_MALLOC_MEMORY		42

#endif	/* !_OLD_ERROR_NUMBERS */

#define DW_IS_NON_ANSI			E_WITH_D_NON_ANSI

/* Compiler level masks */

#define	FALL				~00	/* All levels */
#define	F77				001	/* Fortran 77 */
#define	F90				002	/* Fortran 90 */
#define	F95				004	/* Fortran 95 */

/* Format parser call flags */

#define LIB_CALL			0
#define COMPILER_CALL_ANSI		1	/* Obsolete */
#define COMPILER_CALL_NO_ANSI		2
#define COMPILER_CALL_ANSI_77		3
#define COMPILER_CALL_ANSI_90		4
#define COMPILER_CALL_ANSI_95		5
#define	MAX_CALL_FLAG			5

#define MAX_EXPONENT			077
#define MAX_FIELD_WIDTH			077777777
#define MAX_DECIMAL_FIELD		077777777
#define MAX_REP_COUNT			017777777777


struct	msg_entry {
	unsigned int	msg_number	: 7;
	unsigned int	reserved	: 9;
	unsigned int	msg_column	: 24;
	unsigned int	desc_column	: 24;
};

typedef struct	msg_entry	msg_type;

/*
 *	Note: If we ever have to change the structure of the
 *	fmt_entry, we should ensure that no fields span a
 *	32-bit boundary.  This will help ensure a more compact
 *	structure on _WORD32 systems.
 */

struct	fmt_entry {
	unsigned int	op_code		: 7;
	unsigned int	default_digits	: 1;
	unsigned int	digits_field 	: 24;	/* 1 32-bit word */
	unsigned int	exponent	: 6;
	unsigned int	reserved2	: 2;
	unsigned int	field_width	: 24;	/* 1 64-bit word */

	unsigned int	rgcdedf		: 1;
	unsigned int	reserved3	: 15;
	unsigned int	offset		: 16;	/* 3 32-bit words */
	signed	int	rep_count 	: 32;	/* 2 64-bit words*/
};

typedef	struct	fmt_entry	fmt_type;

#define FMT_ENTRY_BYTE_SIZE	sizeof(fmt_type)

/*
 * Caution: FMT_ENTRY_WORD_SIZE will vary on mips systems with the
 * sizeof(long).
 */

#define FMT_ENTRY_WORD_SIZE	(sizeof(fmt_type)/sizeof(long))

/* Function prototypes */

typedef void	_Error_function(const int _Msg_num,
		const int _Error_offset,
		const int _Edit_desc_offset);

extern fmt_type *
_parsfmt(
	_Error_function	**_Msg_rtn,
	char		*_Format_str,
	long int	_Routine_caller,
	long int	*_Fmt_str_len,
	msg_type	*_Lib_err_msg
);

extern fmt_type *
_fmt_parse(
	_Error_function	**_Msg_rtn,
	char		*_Format_str,
	long int	_Routine_caller,
	long int	*_Fmt_str_len,
	msg_type	*_Lib_err_msg
);

#endif /* !_CRAY_FORMAT_H */
