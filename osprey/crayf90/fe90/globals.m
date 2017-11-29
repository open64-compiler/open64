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


/* USMID:  "\n@(#)5.0_pl/macros/globals.m	5.20	10/27/99 17:06:38\n" */
 

/*****************\
|* MISCELLANEOUS *|
\*****************/

/* These are the compiler generated integer and logical types.  They  */
/* must always be set to the default types for the machine.           */

#ifdef KEY /* Bug 11922 */
/* Experiment shows that -i8 changes INTEGER_DEFAULT_TYPE to Integer_8, but
 * leaves CG_INTEGER_DEFAULT_TYPE at Integer_4. */
#endif /* KEY Bug 11922 */
# define CG_INTEGER_DEFAULT_TYPE init_default_linear_type[Fortran_Integer]
/* These macros are used for the default types.  They are set in init_type. */

# define CHARACTER_DEFAULT_TYPE      default_linear_type[Fortran_Character]
# define COMPLEX_DEFAULT_TYPE        default_linear_type[Fortran_Complex]
# define DOUBLE_COMPLEX_DEFAULT_TYPE TYP_LINEAR(DOUBLE_COMPLEX_TYPE_IDX)
# define DOUBLE_DEFAULT_TYPE         TYP_LINEAR(DOUBLE_PRECISION_TYPE_IDX)
# define INTEGER_DEFAULT_TYPE        default_linear_type[Fortran_Integer]
# define LOGICAL_DEFAULT_TYPE        default_linear_type[Fortran_Logical]
# define REAL_DEFAULT_TYPE           default_linear_type[Fortran_Real]

# if !defined(_HOST32) && !defined(_WHIRL_HOST64_TARGET64)
# define TYPELESS_DEFAULT_TYPE       Typeless_8
# else
# define TYPELESS_DEFAULT_TYPE       Typeless_4
# endif


# ifdef _TYPE_CODE_64_BIT
# define IO_TYPE_CODE_TYPE	     Integer_8
# else
# define IO_TYPE_CODE_TYPE	     TYPELESS_DEFAULT_TYPE
# endif

#ifdef KEY /* Bug 3635 */
/* Function srch_name_tbl would need to change if this exceeded 272 */
# define MAX_ID_LEN		63			/* Num of chars in ID */
# define ANSI90_ID_LEN		31			/* F90 std constraint */
#else
# define MAX_ID_LEN		31			/* Num of chars in ID */
#endif /* KEY Bug 3635 */
# define MAX_EXTERNAL_ID_LEN	255			/* Num chars in extern*/

# define FALSE			0
# define IDENTICAL		0
# define NULL_IDX		0
# define TRUE			1
 
# define DEFAULT_SAFEVL         _MAXVL

# define AMP			'&'
# define AT_SIGN		'@'
# define BANG			'!'
# define BK_SLASH		'\\'
# define BLANK			' '
# define COLON			':'
# define COMMA			','
# define DASH			'-'
# define DBL_QUOTE		'"'
# define DOLLAR			'$'
# define DOT			'.'
# define EOS			'\0'	/* Use this when you really mean EOS. */
# define EQUAL			'='
# define GT			'>'
# define LBRKT			'['
# define LPAREN			'('
# define LT			'<'
# define MINUS			'-'
# define NEWLINE		'\n'
# define NULL_CHAR		'\0'	/* Use this for generic NULL char.    */
# define PERCENT		'%'
# define PLUS			'+'
# define QUEST_MARK		'?'
# define QUOTE			'\''
# define RBRKT			']'
# define RPAREN			')'
# define SEMI_COLON		';'
# define SHARP			'#'
# define SLASH			'/'
# define STAR			'*'
# define TAB			'\t'
# define USCORE			'_'
# define ZERO			'0'

# define EOS_STR		"EOS"

/* -------------------------------------------------------------------------- */
/*  Define masks to describe Compiler Information File (CIF) command line     */
/*  options.  The combinations are recorded in cif_flags by process_c_option  */
/*  in cmd_line.c.                                                            */
/* -------------------------------------------------------------------------- */

# define  ALL_RECS              0001            /* a = f + c + i + m + o + x  */
# define  COMPILER_RECS         0002            /* c = c + f                  */
# define  BASIC_RECS            0004            /* f                          */
# define  INFO_RECS             0010            /* i = i + f                  */
# define  MESSAGE_RECS          0020            /* m = m + f                  */
# define  MISC_RECS             0040            /* o = o + f + i              */
# define  OUTPUT_TO_STDOUT      0100            /* s                          */
# define  XREF_RECS             0200            /* x = x + f + i              */
# define  CMD_PROVIDED_CIF      4000            /* The command processor      */
                                                /* passed the CIF name to us  */
                                                /* via -CZ <name>.            */


/*******************************************\
|* open mp directive argument index macros *|
\*******************************************/

# define OPEN_MP_LIST_CNT               19 /* by jhs, 02/7/20 */

# define OPEN_MP_IF_IDX                  0
# define OPEN_MP_NUM_THREADS			 1 /* by jhs, 02/7/20 */
# define OPEN_MP_PRIVATE_IDX             2
# define OPEN_MP_SHARED_IDX              3
# define OPEN_MP_FIRSTPRIVATE_IDX        4
# define OPEN_MP_DEFAULT_IDX             5
# define OPEN_MP_COPYIN_IDX              6
# define OPEN_MP_REDUCTION_OPR_IDX       7
# define OPEN_MP_REDUCTION_LIST_IDX      8
# define OPEN_MP_LASTPRIVATE_IDX         9
# define OPEN_MP_ORDERED_IDX            10
# define OPEN_MP_SCHEDULE_TYPE_IDX      11
# define OPEN_MP_SCHEDULE_CHUNK_IDX     12
# define OPEN_MP_AFFINITY_IDX           13
# define OPEN_MP_IS_THREAD_IDX          14
# define OPEN_MP_THREAD_DATA_IDX        15
# define OPEN_MP_ONTO_IDX               16
# define OPEN_MP_NEST_IDX               17
# define OPEN_MP_COPYPRIVATE_IDX        18

/***************************************\
|* sgi directive argument index macros *|
\***************************************/

# define MP_DIR_LIST_CNT	       14

# define MP_DIR_IF_IDX			0
# define MP_DIR_SHARE_IDX		1
# define MP_DIR_LASTLOCAL_IDX		2
# define MP_DIR_REDUCTION_IDX		3
# define MP_DIR_MP_SCHEDTYPE_IDX	4
# define MP_DIR_CHUNK_IDX		5
# define MP_DIR_AFFINITY_IDX		6
# define MP_DIR_IS_THREAD_IDX		7
# define MP_DIR_THREAD_DATA_IDX		8
# define MP_DIR_LOCAL_IDX		9
# define MP_DIR_ONTO_IDX	       10
# define MP_DIR_NEST_IDX	       11
# define MP_DIR_LASTTHREAD_IDX         12
# define MP_DIR_ORDERED_IDX            13

/* this is the whirl enum for DEFAULT values. */
/* It comes from common/com/wn_pragmas.h.     */
/*typedef enum {				*/
/*  WN_PRAGMA_DEFAULT_UNKNOWN,			*/
/*  WN_PRAGMA_DEFAULT_NONE,			*/
/*  WN_PRAGMA_DEFAULT_SHARED,			*/
/*  WN_PRAGMA_DEFAULT_PRIVATE,			*/
/* MAX_PRAGMA_DEFAULT				*/
/*} WN_PRAGMA_DEFAULT_KIND;			*/

# define OPEN_MP_DEFAULT_NONE		1
# define OPEN_MP_DEFAULT_SHARED		2
# define OPEN_MP_DEFAULT_PRIVATE	3

/* this is the whirl enum for mp_schedtype */
/* typedef enum { */
/*  WN_PRAGMA_SCHEDTYPE_UNKNOWN, */
/*  WN_PRAGMA_SCHEDTYPE_RUNTIME, */
/*  WN_PRAGMA_SCHEDTYPE_SIMPLE, */
/*  WN_PRAGMA_SCHEDTYPE_INTERLEAVE, */
/*  WN_PRAGMA_SCHEDTYPE_DYNAMIC, */
/*  WN_PRAGMA_SCHEDTYPE_GSS, */
/*  WN_PRAGMA_SCHEDTYPE_PSEUDOLOWERED, */
/*  MAX_PRAGMA_SCHEDTYPE */
/*} WN_PRAGMA_SCHEDTYPE_KIND; */

# define MP_SCHEDTYPE_RUNTIME				1
# define MP_SCHEDTYPE_SIMPLE				2
# define MP_SCHEDTYPE_INTERLEAVED			3
# define MP_SCHEDTYPE_DYNAMIC				4
# define MP_SCHEDTYPE_GUIDED				5

# define OPEN_MP_SCHEDULE_RUNTIME	1
# define OPEN_MP_SCHEDULE_STATIC	2
# define OPEN_MP_SCHEDULE_DYNAMIC	4
# define OPEN_MP_SCHEDULE_GUIDED	5

# define DOPREFER_SERIAL				1
# define DOPREFER_CONCURRENT				2
# define DOPREFER_VECTOR				3

# define ASSERT_ARGUMENTALIASING			1
# define ASSERT_NOARGUMENTALIASING			2
# define ASSERT_BOUNDSVIOLATIONS			3
# define ASSERT_NOBOUNDSVIOLATIONS			4
# define ASSERT_CONCURRENTCALL				5
# define ASSERT_NOCONCURRENTCALL			6
# define ASSERT_NORECURRENCE				7
# define ASSERT_DOPREFER				8
# define ASSERT_EQUIVALENCEHAZARD			9
# define ASSERT_NOEQUIVALENCEHAZARD			10
# define ASSERT_LASTVALUENEEDED				11
# define ASSERT_LASTVALUESNEEDED			12
# define ASSERT_NOLASTVALUENEEDED			13
# define ASSERT_NOLASTVALUESNEEDED			14
# define ASSERT_PERMUTATION				15
# define ASSERT_RELATION				16
# define ASSERT_NOSYNC					17
# define ASSERT_TEMPORARIESFORCONSTANTARGUMENTS		18
# define ASSERT_NOTEMPORARIESFORCONSTANTARGUMENTS	19
# define ASSERT_DO					20
# define ASSERT_BENIGN					21
# define ASSERT_DEPENDENCE				22
# define ASSERT_FREQUENCY				23
# define ASSERT_IGNOREANYDEPENDENCES			24
# define ASSERT_IGNOREANYDEPENDENCE			25
# define ASSERT_IGNOREASSUMEDDEPENDENCES		26
# define ASSERT_IGNOREASSUMEDDEPENDENCE			27
# define ASSERT_NOINTERCHANGE				28
# define ASSERT_USECOMPRESS				29
# define ASSERT_USEEXPAND				30
# define ASSERT_USECONTROLLEDSTORE			31
# define ASSERT_USEGATHER				32
# define ASSERT_USESCATTER				33

/*************************\
|* OPND TYPE initializer *|
\*************************/

# define INIT_OPND_TYPE	{0,0,0,0,0,NO_Tbl_Idx,0}


/* -------------------------------------------------------------------------- */
/* Table type to mark the module information table in the PDT                 */
/* -------------------------------------------------------------------------- */

# define COMPILER_INFO_TABLE_TYPE	026	/* Octal */


/* This is the test for dalign on IRIX and Solaris machines.		      */
/* It is used by s_cnstrct.c as well as sytb.c.  	  	     BHJ      */
/* Complex_4 was #if'd out on 4/29/97 by LRR per the following from Rich:     */
/*   I believe we want to do this.  In our compiler, the alignment for a      */
/*   COMPLEX type is the same as the alignment of the underlying REAL type;   */
/*   that is, COMPLEX(4) should be aligned on a 4 byte boundary and	      */
/*   COMPLEX(8) should be aligned on an 8 byte boundary.                      */

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)) 

# define DALIGN_TEST_CONDITION(TYPE_IDX)				      \
				TYP_LINEAR(TYPE_IDX) == Integer_8 ||          \
				TYP_LINEAR(TYPE_IDX) == Logical_8 ||          \
				TYP_LINEAR(TYPE_IDX) == Real_8    ||          \
            			TYP_LINEAR(TYPE_IDX) == Real_16   ||          \
            			TYP_LINEAR(TYPE_IDX) == Complex_8 ||          \
            			TYP_LINEAR(TYPE_IDX) == Complex_16 ||         \
                                TYP_LINEAR(TYPE_IDX) == Typeless_8 ||         \
                                TYP_LINEAR(TYPE_IDX) == Long_Typeless ||      \
                                (cmd_line_flags.s_pointer8 &&		      \
                                 (TYP_LINEAR(TYPE_IDX) == CRI_Ptr_8 ||	      \
                                  TYP_LINEAR(TYPE_IDX) == CRI_Ch_Ptr_8)) ||   \
            			(TYP_TYPE(TYPE_IDX) == Structure &&           \
             			ATT_DALIGN_ME(TYP_IDX(TYPE_IDX)))
# else

# define DALIGN_TEST_CONDITION(TYPE_IDX)				      \
				TYP_LINEAR(TYPE_IDX) == Integer_8 ||          \
				TYP_LINEAR(TYPE_IDX) == Logical_8 ||          \
				TYP_LINEAR(TYPE_IDX) == Real_8    ||          \
            			TYP_LINEAR(TYPE_IDX) == Real_16   ||          \
            			TYP_LINEAR(TYPE_IDX) == Complex_4 ||          \
            			TYP_LINEAR(TYPE_IDX) == Complex_8 ||          \
            			TYP_LINEAR(TYPE_IDX) == Complex_16 ||         \
                                TYP_LINEAR(TYPE_IDX) == Typeless_8 ||         \
                                TYP_LINEAR(TYPE_IDX) == Long_Typeless ||      \
            			(TYP_TYPE(TYPE_IDX) == Structure &&           \
             			ATT_DALIGN_ME(TYP_IDX(TYPE_IDX)))
# endif


# define PACK_HALF_WORD_TEST_CONDITION(TYPE_IDX)			       \
				TARGET_MAX_HALF_WORD_STORAGE_TYPE(TYPE_IDX) || \
            			(TYP_TYPE(TYPE_IDX) == Structure &&            \
             			ATT_ALIGNMENT(TYP_IDX(TYPE_IDX))== Align_32)

# define PACK_8_BIT_TEST_CONDITION(TYPE_IDX)				       \
            			(TYP_LINEAR(TYPE_IDX) == Integer_1 ||          \
            			 TYP_LINEAR(TYPE_IDX) == Logical_1 ||          \
            			(TYP_TYPE(TYPE_IDX) == Structure &&            \
             			ATT_ALIGNMENT(TYP_IDX(TYPE_IDX)) == Align_8))

# define PACK_16_BIT_TEST_CONDITION(TYPE_IDX)				       \
            			(TYP_LINEAR(TYPE_IDX) == Integer_1 ||          \
            			 TYP_LINEAR(TYPE_IDX) == Logical_1 ||          \
            			 TYP_LINEAR(TYPE_IDX) == Integer_2 ||          \
            			 TYP_LINEAR(TYPE_IDX) == Logical_2 ||          \
            			(TYP_TYPE(TYPE_IDX) == Structure &&            \
             			(ATT_ALIGNMENT(TYP_IDX(TYPE_IDX)) == Align_8 ||\
             			 ATT_ALIGNMENT(TYP_IDX(TYPE_IDX)) == Align_16)))

/********************\
|* SIZES AND LIMITS *|
\********************/

# define DATE_TIME_STR_SIZE	27
# define RELEASE_LEVEL_LEN      24


/********************\
|* IO GLOBAL MACROS *|
\********************/

# define MAX_CONST_OPT_LENGTH           16
# define MAX_CIITEM_NAME_LENGTH         16
# define MAX_NUM_EXP_FORMS              2
# define MAX_NUM_ALLOWED_TYPES          3
# define MAX_NUM_CONST_OPTS             5
# define NUM_IO_STMT_TYPES              8
# define MAX_NUM_CIITEM                 25


/**************************************\
|* MACROS TO HOLD LIBRARY ENTRY NAMES *|
|* THIS IS A MACHINE DEPENDENCY.      *|
\**************************************/

# define ALLOCATE_LIB_ENTRY		"_ALLOCATE"
# define ALLOCATE_NAME_LEN		9

# if defined(_TARGET_OS_UNICOS)
# define ARGCHCK_LIB_ENTRY		"$ARGCHCK"
# else
# define ARGCHCK_LIB_ENTRY		"_ARGCHCK"
# endif
# define ARGCHCK_NAME_LEN		8

# define DEALLOCATE_LIB_ENTRY		"_DEALLOCATE"
# define DEALLOCATE_NAME_LEN		11

/* this is the no error checking DEALLOCATE */
# define DEALLOC_LIB_ENTRY		"_DEALLOC"
# define DEALLOC_NAME_LEN		8

# define REALLOC_LIB_ENTRY		"_REALLOC"
# define REALLOC_NAME_LEN		8

# define BACKSPACE_LIB_ENTRY		"_BACK"
# define BACKSPACE_NAME_LEN		5

# if defined(_TARGET_OS_MAX)
# define END_LIB_ENTRY			"$END"
# else
# define END_LIB_ENTRY			"_END"
# endif
# define END_NAME_LEN			4

# define ENDFILE_LIB_ENTRY		"_EOFW"
# define ENDFILE_NAME_LEN		5

# define INQUIRE_LIB_ENTRY		"_INQUIRE"
# define INQUIRE_NAME_LEN		8

# define OPEN_LIB_ENTRY			"_OPEN"
# define OPEN_NAME_LEN			5

# define CLOSE_LIB_ENTRY		"_CLOSE"
# define CLOSE_NAME_LEN			6

# define BUFFER_IN_LIB_ENTRY		"_BUFFERIN"
# define BUFFER_IN_NAME_LEN		9

# define BUFFER_OUT_LIB_ENTRY		"_BUFFEROUT"
# define BUFFER_OUT_NAME_LEN		10

# define REWIND_LIB_ENTRY		"_REWF"
# define REWIND_NAME_LEN		5

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# define PAUSE_LIB_ENTRY		"_F90_PAUSE"
# define PAUSE_NAME_LEN			10

# define STOP_LIB_ENTRY			"_F90_STOP"
# define STOP_NAME_LEN			9
# else
# define PAUSE_LIB_ENTRY		"_PAUSE"
# define PAUSE_NAME_LEN			6

# define STOP_LIB_ENTRY			"_STOP"
# define STOP_NAME_LEN			5
# endif

# define STOP_ALL_LIB_ENTRY             "STOP_ALL"
# define STOP_ALL_NAME_LEN              8

# define CONFORM_LIB_ENTRY		"_CONFORM_ERROR"
# define CONFORM_NAME_LEN		14

# define BOUNDS_LIB_ENTRY		"_BOUNDS_ERROR"
# define BOUNDS_NAME_LEN		13

# define RBOUNDS_LIB_ENTRY		"_RBOUNDS_ERROR"
# define RBOUNDS_NAME_LEN		14

# define SBOUNDS_LIB_ENTRY		"_SBOUNDS_ERROR"
# define SBOUNDS_NAME_LEN		14

# define PTR_CHK_LIB_ENTRY		"_POINTER_ERROR"
# define PTR_CHK_NAME_LEN		14

# if defined(_TARGET_OS_UNICOS)
# define START_PES_LIB_ENTRY		"START_PES"
# define START_PES_NAME_LEN		9
# else
# define START_PES_LIB_ENTRY		"start_pes_"
# define START_PES_NAME_LEN		10
# endif

# define SET_NUMTHREADS_ENTRY		"mp_set_numthreads_"
# define SET_NUMTHREADS_NAME_LEN	18
/* OPND ACCESS */

#ifdef KEY /* Bug 8117 */
/* Runtime symbols for non-inline copyinout of arguments */
# define COPYIN_ENTRY			"_Copyin"
#ifdef PATHSCALE_MERGE
# define COPYIN_NAME_LEN		((sizeof COPYIN_ENTRY) - 1)
#endif
# define COPYOUT_ENTRY			"_Copyout"
#ifdef PATHSCALE_MERGE
# define COPYOUT_NAME_LEN		((sizeof COPYOUT_ENTRY) - 1)
#endif
#endif /* KEY Bug 8117 */
#ifdef KEY /* Bug 5089 */
/* Runtime symbols for Fortran 2000 IEEE prolog/epilog save/restore. 
 * IEEE_SAVE_SIZE is sizeof(fenv_t); it needs to be matched to the target OS
 * and architecture. */
# define IEEE_SAVE_ENTRY		"_Ieee_save"
# define IEEE_RESTORE_ENTRY		"_Ieee_restore"

#ifdef _LINUX_LINUX
# if (defined(TARG_X8664) || defined(TARG_IA64))
    /* The size of fenv_t on Linux systems for -m64 is 32 bytes. The size on
     * Fedora Core 1 systems for -m32 is 32 bytes, but on Fedora Core 2 and
     * later -m32 systems it's 28 bytes. For -m32, libpathfortran provides
     * its own definition which is always 32 bytes, both for consistency and
     * because SSE2 needs the member which was eliminated after FC1. */
#   define IEEE_SAVE_SIZE			32
# elif defined(TARG_MIPS) || defined(TARG_LOONGSON)
#   define IEEE_SAVE_SIZE			4
# endif /* defined(TARG_whatever) */
#endif /* _LINUX_LINUX */
#ifdef _DARWIN_DARWIN
/* -m32 gives 16, but for now we double it in case -m64 is larger or we have
 * to use our own structure anyway */
#  define IEEE_SAVE_SIZE			32
#endif /* _DARWIN_DARWIN */

#if !defined(IEEE_SAVE_SIZE)
# error "Need IEEE_SAVE_SIZE"
#endif /* !defined(IEEE_SAVE_SIZE) */

#endif /* KEY Bug 5089 */
#ifdef KEY /* Bug 6845 */
# define ASSIGN_ALLOCATABLE_ENTRY	"_ASSIGN_ALLOCATABLE"

/* Welcome to elementary C programming */
# define OPND_LINE_NUM(OPND)		((OPND).line_num)
# define OPND_COL_NUM(OPND)		((OPND).col_num)
# define OPND_FLD(OPND)			((OPND).fld)
# define OPND_IDX(OPND)			((OPND).idx)
# define OPND_LIST_CNT(OPND)		((OPND).line_num)
#else /* KEY Bug 6845 */

# define OPND_LINE_NUM(OPND)		OPND.line_num
# define OPND_COL_NUM(OPND)		OPND.col_num
# define OPND_FLD(OPND)			OPND.fld
# define OPND_IDX(OPND)			OPND.idx
# define OPND_LIST_CNT(OPND)            OPND.line_num
#endif /* KEY Bug 6845 */


/*********************************\
|* CMIC WORK DISTRIBUTION VALUES *|
\*********************************/

# define CMIC_WORK_DIST_SINGLE		1
# define CMIC_WORK_DIST_VECTOR		2
# define CMIC_WORK_DIST_GUIDED		3
# define CMIC_WORK_DIST_NUMCHUNKS	4
# define CMIC_WORK_DIST_CHUNKSIZE	5
# define CMIC_WORK_DIST_NCPUS_CHUNKS	8


/******************************************************************************\
|* Values used to signal the compiler exit status to the driver.              *|
\******************************************************************************/

/* Header file rcodes.h in the IRIX driver uses the following return codes:   */
/* (comment sentinels for each of their definition comments have been         */
/* with parens)								      */
/*									      */
/*  #define RC_OKAY                 0    (* executed successfully *)          */
/*  #define RC_INTERNAL_ERROR       1    (* a compiler error *)	      	      */
/*  #define RC_USER_ERROR           2    (* a user error *)		      */
/*  #define RC_NORECOVER_USER_ERROR 4    (* a user error can't recover from *)*/
/*  #define RC_UNIMPLEMENTED_ERROR  8    (* uses an unimplemented feature *)  */
/*  #define RC_NEED_INLINER         16   (* flag that we need the inliner *)  */
/*  #define RC_SYSTEM_ERROR         32   (* a O/S or environment error *)     */
/*  #define RC_OVERFLOW_ERROR       64   (* an overflow error;		      */
/*                                          try recompiling *)		      */
/* 								 	      */
/* Since the two chief statuses of "internal error" and "user error" are      */
/* exactly the opposite of our status values, we define the following         */
/* #define constants to be used in place of hard-coded values:		      */

# define RC_OKAY                 0    /* Compiler executed successfully.      */

# if (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))

# define RC_INTERNAL_ERROR       1    /* The compiler error terminated.       */
# define RC_USER_ERROR           2    /* The program contains at least one    */
				      /* user error.			      */

# else

# define RC_USER_ERROR           1    /* The program contains at least one    */
                                      /* user error.                          */
# define RC_INTERNAL_ERROR       2    /* The compiler error terminated.       */

# endif


/***********************************\
|* CONDITIONAL REPLACEMENT STRINGS *|
\***********************************/

# define EQUAL_STRS(S1,S2)	(strcmp (S1, S2) == IDENTICAL)

/***********************************************\
|* STATEMENT/FUNCTION-LIKE REPLACEMENT STRINGS *|
\***********************************************/

/* Create a word aligned, NULL filled identifier or keyword */
/* NOTE:  This cannot take a 0 for LEN */

# define CREATE_ID(ID, STR, LEN)					       \
	{	int	_idx;						       \
		for (_idx = 0; _idx < NUM_ID_WDS; _idx++) {		       \
		   ID.words[_idx]	= 0;				       \
		}							       \
		for (_idx = 0; _idx < LEN; _idx++) {			       \
		   ID.string[_idx]	= STR[_idx];			       \
		}							       \
	}

/* allocate memory and make sure nothing went wrong */
# define MEM_ALLOC(PTR, TYPE, SIZE)					       \
		PTR = (TYPE *) malloc ((SIZE)*sizeof(TYPE));		       \
		MALLOC_CHECK(0);					       \
		if (PTR == NULL) {					       \
		   char _struct_name[20];                                      \
                   strncpy (_struct_name, #PTR, 20);                           \
		   PRINTMSG (stmt_start_line, 14, Limit, 0, _struct_name);     \
		}							       \
		MEM_TRACE(Mem_Alloc,PTR,NULL,((SIZE)*(long)sizeof(TYPE)), SIZE)


/* free allocated memory */
# define MEM_FREE(PTR)							       \
	MALLOC_CHECK(0);						       \
	free (PTR);							       \
	MEM_TRACE (Mem_Free, PTR, NULL, 0, 0)				       \
	PTR = NULL_IDX;

# define TBL_FREE(STRUCT)						       \
	if (STRUCT != NULL) {		/* wasn't previously alloced */	       \
	   MEM_REPORT(STRUCT);						       \
	   CLEAR_LARGEST_IDX(STRUCT);					       \
	   MEM_FREE(STRUCT);						       \
	   STRUCT##_idx = 0;						       \
	   STRUCT##_size = 0;						       \
	}


/* reallocate memory and make sure nothing went wrong.                        */
/* 2 copies, so that we can run gcc using -Wall to check the compilation.     */
/* The _optr gets an unused message all over the compiler otherwise.          */

# ifdef _DEBUG
# define MEM_REALLOC(PTR, TYPE, NEW_SIZE)				       \
	{	void *_optr = PTR;					       \
		MALLOC_CHECK(0);					       \
		PTR = (TYPE *) realloc ((char *) PTR, (NEW_SIZE)*sizeof(TYPE));\
		if (PTR == NULL) {					       \
		   char _struct_name[20];                                      \
                   strncpy (_struct_name, #PTR, 20);                           \
		   PRINTMSG (stmt_start_line, 14, Limit, 0, _struct_name);     \
		}							       \
		MEM_TRACE (Mem_Realloc, PTR, _optr,			       \
			   ((NEW_SIZE)*(long)sizeof(TYPE)), NEW_SIZE);	       \
	}
# else
# define MEM_REALLOC(PTR, TYPE, NEW_SIZE)				       \
	{	MALLOC_CHECK(0);					       \
		PTR = (TYPE *) realloc ((char *) PTR, (NEW_SIZE)*sizeof(TYPE));\
		if (PTR == NULL) {					       \
		   char _struct_name[20];                                      \
                   strncpy (_struct_name, #PTR, 20);                           \
		   PRINTMSG (stmt_start_line, 14, Limit, 0, _struct_name);     \
		}							       \
	}
# endif


# define CHECK_INITIAL_ALLOC(STRUCT, STRUCT_INIT_IDX)			       \
		if (STRUCT##_size == 0) {	/* wasn't previously alloced */\
		   STRUCT##_size = STRUCT##_init_size;			       \
		   TBL_ALLOC (STRUCT);					       \
		}							       \
		else if (STRUCT##_size > STRUCT##_init_size){ /* too large */  \
		   MEM_REPORT(STRUCT);					       \
                   MEM_FREE(STRUCT);	 /*Don't need TBL_FREE's checks.  */   \
		   STRUCT##_size = STRUCT##_init_size;			       \
		   TBL_ALLOC (STRUCT);					       \
		}							       \
		SET_LARGEST_IDX(STRUCT, NULL_IDX, STRUCT_INIT_IDX);	       \
		STRUCT##_idx = STRUCT_INIT_IDX;


/* Careful when calling this, because all the tables are zero based, so the   */
/* size one wants, is one bigger than it seems.  WARNING: This macro does not */
/* change STRUCT##_idx.                                                       */

# define CHECK_TBL_ALLOC_SIZE(STRUCT, STRUCT_NEW_SIZE)			       \
		SET_LARGEST_IDX(STRUCT, STRUCT_NEW_SIZE, STRUCT_NEW_SIZE);     \
		if (STRUCT##_size == 0) {	/* wasn't previously alloced */\
		   STRUCT##_size = STRUCT_NEW_SIZE + 1;			       \
		   STRUCT##_init_size = STRUCT##_size;			       \
		   TBL_ALLOC (STRUCT);					       \
		}							       \
		else if (STRUCT##_size <= STRUCT_NEW_SIZE){ /*not big enough */\
		   STRUCT##_size = STRUCT_NEW_SIZE + 1;			       \
 		   if (STRUCT##_size > STRUCT##_limit) {                       \
		      char _struct_name[20];                                   \
                      strncpy (_struct_name, #STRUCT, 20);                     \
                      PRINTMSG(stmt_start_line, 237, Limit, 0, _struct_name);  \
                   }                                                           \
		   MEM_REALLOC (STRUCT, STRUCT##_type, STRUCT##_size);	       \
		}

/* NOTE:  These macros normally belong in debug.m but because they are        */
/*	  referenced from within this file, they must be defined here	      */

# ifdef _DEBUG
#    define MEM_TRACE(TRACE_TYPE, NPTR, OPTR, BYTE_SIZE, NUM_ENTRIES)	       \
		if (dump_flags.mtrace_info) {				       \
		   char _struct_name[20];				       \
		   strncpy (_struct_name, #NPTR, 20);			       \
		   dump_mem_trace_info (TRACE_TYPE, _struct_name, NPTR,	       \
					OPTR, BYTE_SIZE, NUM_ENTRIES);	       \
		}
# else
#    define MEM_TRACE(ARG1, ARG2, ARG3, ARG4, ARG5)
# endif

# ifdef _DEBUG
#    define MEM_REPORT(STRUCT)						       \
		if (dump_flags.mem_report) {				       \
		   char _struct_name[20];				       \
		   strncpy (_struct_name, #STRUCT, 20);			       \
		   print_mem_usage_report(_struct_name,			       \
		   			 STRUCT##_size,			       \
		   			 STRUCT##_largest_idx);		       \
		}
# else
#    define MEM_REPORT(STRUCT)
# endif

# ifdef _DEBUG
#    define SET_LARGEST_IDX(STRUCT, CHECK_IDX, NEW_IDX)			       \
		if (STRUCT##_largest_idx < CHECK_IDX) {			       \
 		   STRUCT##_largest_idx = NEW_IDX;			       \
		}
#    define CLEAR_LARGEST_IDX(STRUCT)					       \
 		STRUCT##_largest_idx = NULL_IDX;
# else
#    define SET_LARGEST_IDX(STRUCT, CHECK_IDX, NEW_IDX)
#    define CLEAR_LARGEST_IDX(STRUCT)
# endif

# if _DEBUG && ((defined _HOST_OS_UNICOS) || defined(_HOST_OS_MAX))
#    define MALLOC_CHECK(NUM)						       \
		if (malloc_check(NUM)) {				       \
		   PRINTMSG (stmt_start_line, 514, Internal, 0);	       \
		}
# else
#    define MALLOC_CHECK(NUM)
# endif

#define	NEXT_LA_CH							       \
		(*get_char)()


/******************************************************************************\
|* the TBL_ALLOC and TBL_REALLOC_CK macros assume the following declarations  *|
|* associated with PTR:	 typedef <type specifier> PTR_type;		      *|
|*			 PTR_type      *PTR;				      *|
|*			 int		PTR_idx	 = NULL_IDX;		      *|
|*			 int		PTR_inc	 = <init>;		      *|
|*			 int		PTR_size = <init>;		      *|
\******************************************************************************/
# define TBL_ALLOC(PTR)							       \
		MEM_ALLOC (PTR, PTR##_type, PTR##_size);
 
# define TBL_REALLOC_CK(PTR, NUM_ENTRIES)				       \
		PTR##_idx += (NUM_ENTRIES);				       \
		SET_LARGEST_IDX(PTR, PTR##_idx, PTR##_idx);		       \
		if (PTR##_idx >= PTR##_size) {				       \
		   PTR##_size += (PTR##_inc > (NUM_ENTRIES)) ?		       \
						PTR##_inc : (NUM_ENTRIES);     \
 		   if (PTR##_size > PTR##_limit) {                             \
		      char _struct_name[20];                                   \
                      strncpy (_struct_name, #PTR, 20);                        \
                      PRINTMSG(stmt_start_line, 237, Limit, 0, _struct_name);  \
                   }                                                           \
		   MEM_REALLOC (PTR, PTR##_type, PTR##_size);		       \
		}

/* This macro searches the global line table to find the entry for the input  */
/* line.  The global line index is returned in IDX.  The for loop vectorizes. */

# define GLOBAL_LINE_TO_FILE_LINE(LINE, IDX, ACT_LINE)			       \
	for (IDX = 1; IDX <= global_line_tbl_idx; IDX++) {		       \
	    if (GL_GLOBAL_LINE(IDX) > LINE) {				       \
	       break;							       \
	    }								       \
	}								       \
	IDX--;								       \
	ACT_LINE = LINE - GL_GLOBAL_LINE(IDX) + GL_FILE_LINE(IDX);

#ifdef KEY /* Bug 6845 */
/* Welcome to elementary C programming */
# define COPY_OPND(OPND_T, OPND_S)                                             \
         ((OPND_T) = (OPND_S))
#else /* KEY Bug 6845 */
# define COPY_OPND(OPND_T, OPND_S)                                             \
         OPND_T = OPND_S;
#endif /* KEY Bug 6845 */

# define SET_MESSAGE_TBL(TBL, MSG_NUM)					       \
	{int	_shift, _idx;                                                  \
		_idx   = MSG_NUM / HOST_BITS_PER_WORD;                         \
		_shift = HOST_BITS_PER_WORD - ((MSG_NUM%HOST_BITS_PER_WORD)+1);\
		TBL[_idx] |= 1 << _shift;                                      \
        }

# define GET_MESSAGE_TBL(TBL, MSG_NUM)					       \
	     (TBL[MSG_NUM/HOST_BITS_PER_WORD] &		                       \
                  1L << (HOST_BITS_PER_WORD-((MSG_NUM % HOST_BITS_PER_WORD)+1)))

# if defined(_TARGET32) && !defined(_USE_FOLD_DOT_f)
# define SHIFT_ARITH_ARG(VAR, TYPE)                                            \
	if (TYPE == Integer_1 ||                                               \
	    TYPE == Integer_2 ||                                               \
	    TYPE == Integer_4 ||                                               \
            TYPE == Typeless_1 ||                                              \
            TYPE == Typeless_2 ||                                              \
            TYPE == Typeless_4 ||                                              \
            TYPE == Short_Typeless_Const ||                                    \
            TYPE == Short_Char_Const ||                                        \
            TYPE == Logical_1 ||                                               \
            TYPE == Logical_2 ||                                               \
            TYPE == Logical_4 ||                                               \
	    TYPE == Real_4) {                                                  \
	   VAR[1] = VAR[0];                                                    \
           VAR[0] = 0;                                                         \
	}

# define SHIFT_ARITH_RESULT(VAR, TYPE)                                         \
	if (TYPE == Integer_1 ||                                               \
	    TYPE == Integer_2 ||                                               \
	    TYPE == Integer_4 ||                                               \
            TYPE == Typeless_1 ||                                              \
            TYPE == Typeless_2 ||                                              \
            TYPE == Typeless_4 ||                                              \
            TYPE == Short_Typeless_Const ||                                    \
            TYPE == Short_Char_Const ||                                        \
            TYPE == Logical_1 ||                                               \
            TYPE == Logical_2 ||                                               \
            TYPE == Logical_4 ||                                               \
	    TYPE == Real_4) {                                                  \
	   VAR[0] = VAR[1];                                                    \
	}

# elif defined(_TARGET_LITTLE_ENDIAN) && defined(_TARGET32)  && defined(_USE_FOLD_DOT_f) 
# define SHIFT_ARITH_ARG(VAR,TYPE)                                             \
        if (TYPE == Integer_1 ||                                               \
            TYPE == Integer_2 ||                                               \
            TYPE == Integer_4 ||                                               \
            TYPE == Typeless_1 ||                                              \
            TYPE == Typeless_2 ||                                              \
            TYPE == Typeless_4 ||                                              \
            TYPE == Short_Typeless_Const ||                                    \
            TYPE == Short_Char_Const ||                                        \
            TYPE == Logical_1 ||                                               \
            TYPE == Logical_2 ||                                               \
            TYPE == Logical_4 ||                                               \
            TYPE == Real_4){                                                   \
           VAR[1] = 0;                                                         \
        }
# define SHIFT_ARITH_RESULT(VAR, TYPE)
# else

/* these are blank since on _TARGET64, 32 bit things are not packed */
/* when they are there will be problems in input, fold and output   */
/* of constants. BHJ                                                */

# define SHIFT_ARITH_ARG(VAR, TYPE)

# define SHIFT_ARITH_RESULT(VAR, TYPE)
# endif

/********************************************************************\
|* The following macro is used to make sure that compiler temps are *|
|* the shared list if they are referenced inside a parallel region. *|
\********************************************************************/

# define ADD_TMP_TO_SHARED_LIST(ATTR_IDX)                                      \
   if (cdir_switches.parallel_region &&                                        \
       cdir_switches.shared_list_idx != NULL_IDX &&                            \
       comp_phase == Pass2_Semantics &&                                        \
       AT_OBJ_CLASS(ATTR_IDX) == Data_Obj &&                                   \
       ATD_CLASS(ATTR_IDX) == Compiler_Tmp &&                                  \
       ! ATD_TASK_PRIVATE(ATTR_IDX) &&                                         \
       ! ATD_TASK_SHARED(ATTR_IDX)) {                                          \
      int       _list_idx;                                                     \
      NTR_IR_LIST_TBL(_list_idx);                                              \
      IL_NEXT_LIST_IDX(_list_idx) = IL_IDX(cdir_switches.shared_list_idx);     \
      if (IL_IDX(cdir_switches.shared_list_idx) != NULL_IDX) {                 \
         IL_PREV_LIST_IDX(IL_IDX(cdir_switches.shared_list_idx)) = _list_idx;  \
      }                                                                        \
      IL_IDX(cdir_switches.shared_list_idx) = _list_idx;                       \
      IL_FLD(cdir_switches.shared_list_idx) = IL_Tbl_Idx;                      \
      IL_LIST_CNT(cdir_switches.shared_list_idx)++;                            \
      IL_FLD(_list_idx) = AT_Tbl_Idx;                                          \
      IL_IDX(_list_idx) = ATTR_IDX;                                            \
      ATD_TASK_SHARED(ATTR_IDX) = TRUE;                                        \
   }

# define ADD_TMP_TO_PRIVATE_LIST(ATTR_IDX)                                     \
   if (cdir_switches.parallel_region &&                                        \
       cdir_switches.private_list_idx != NULL_IDX &&                           \
       comp_phase == Pass2_Semantics &&                                        \
       AT_OBJ_CLASS(ATTR_IDX) == Data_Obj &&                                   \
       ATD_CLASS(ATTR_IDX) == Compiler_Tmp &&                                  \
       ! ATD_TASK_PRIVATE(ATTR_IDX) &&                                         \
       ! ATD_TASK_SHARED(ATTR_IDX)) {                                          \
      int       _list_idx;                                                     \
      NTR_IR_LIST_TBL(_list_idx);                                              \
      IL_NEXT_LIST_IDX(_list_idx) = IL_IDX(cdir_switches.private_list_idx);    \
      if (IL_IDX(cdir_switches.private_list_idx) != NULL_IDX) {                \
         IL_PREV_LIST_IDX(IL_IDX(cdir_switches.private_list_idx)) = _list_idx; \
      }                                                                        \
      IL_IDX(cdir_switches.private_list_idx) = _list_idx;                      \
      IL_FLD(cdir_switches.private_list_idx) = IL_Tbl_Idx;                     \
      IL_LIST_CNT(cdir_switches.private_list_idx)++;                           \
      IL_FLD(_list_idx) = AT_Tbl_Idx;                                          \
      IL_IDX(_list_idx) = ATTR_IDX;                                            \
      ATD_TASK_PRIVATE(ATTR_IDX) = TRUE;                                       \
   }

# define ADD_VAR_TO_SHARED_LIST(ATTR_IDX)                                      \
   if (cdir_switches.parallel_region &&                                        \
       comp_phase == Pass2_Semantics &&                                        \
       cdir_switches.shared_list_idx != NULL_IDX &&                            \
       AT_OBJ_CLASS(ATTR_IDX) == Data_Obj &&                                   \
       ! ATD_TASK_PRIVATE(ATTR_IDX) &&                                         \
       ! ATD_TASK_SHARED(ATTR_IDX)) {                                          \
      int       _list_idx;                                                     \
      NTR_IR_LIST_TBL(_list_idx);                                              \
      IL_NEXT_LIST_IDX(_list_idx) = IL_IDX(cdir_switches.shared_list_idx);     \
      if (IL_IDX(cdir_switches.shared_list_idx) != NULL_IDX) {                 \
         IL_PREV_LIST_IDX(IL_IDX(cdir_switches.shared_list_idx)) = _list_idx;  \
      }                                                                        \
      IL_IDX(cdir_switches.shared_list_idx) = _list_idx;                       \
      IL_FLD(cdir_switches.shared_list_idx) = IL_Tbl_Idx;                      \
      IL_LIST_CNT(cdir_switches.shared_list_idx)++;                            \
      IL_FLD(_list_idx) = AT_Tbl_Idx;                                          \
      IL_IDX(_list_idx) = ATTR_IDX;                                            \
      ATD_TASK_SHARED(ATTR_IDX) = TRUE;                                        \
      ATD_WAS_SCOPED(ATTR_IDX) = TRUE;                                         \
   }

# define ADD_VAR_TO_PRIVATE_LIST(ATTR_IDX)                                     \
   if (cdir_switches.parallel_region &&                                        \
       cdir_switches.private_list_idx != NULL_IDX &&                           \
       comp_phase == Pass2_Semantics &&                                        \
       AT_OBJ_CLASS(ATTR_IDX) == Data_Obj &&                                   \
       ! ATD_TASK_PRIVATE(ATTR_IDX) &&                                         \
       ! ATD_TASK_SHARED(ATTR_IDX)) {                                          \
      int       _list_idx;                                                     \
      NTR_IR_LIST_TBL(_list_idx);                                              \
      IL_NEXT_LIST_IDX(_list_idx) = IL_IDX(cdir_switches.private_list_idx);    \
      if (IL_IDX(cdir_switches.private_list_idx) != NULL_IDX) {                \
         IL_PREV_LIST_IDX(IL_IDX(cdir_switches.private_list_idx)) = _list_idx; \
      }                                                                        \
      IL_IDX(cdir_switches.private_list_idx) = _list_idx;                      \
      IL_FLD(cdir_switches.private_list_idx) = IL_Tbl_Idx;                     \
      IL_LIST_CNT(cdir_switches.private_list_idx)++;                           \
      IL_FLD(_list_idx) = AT_Tbl_Idx;                                          \
      IL_IDX(_list_idx) = ATTR_IDX;                                            \
      ATD_TASK_PRIVATE(ATTR_IDX) = TRUE;                                       \
      ATD_WAS_SCOPED(ATTR_IDX) = TRUE;                                         \
   }

# define GEN_MAX_ZERO_IR(MAX_IDX, THE_OPND, LINE, COL)                         \
	{ int	_list_idx;                                                     \
         NTR_IR_TBL(MAX_IDX);                                                  \
         IR_OPR(MAX_IDX) = Max_Opr;                                            \
         IR_TYPE_IDX(MAX_IDX) = CG_INTEGER_DEFAULT_TYPE;                       \
         IR_LINE_NUM(MAX_IDX) = LINE;                                          \
         IR_COL_NUM(MAX_IDX)  = COL;                                           \
         NTR_IR_LIST_TBL(_list_idx);                                           \
         IR_FLD_L(MAX_IDX) = IL_Tbl_Idx;                                       \
         IR_LIST_CNT_L(MAX_IDX) = 2;                                           \
         IR_IDX_L(MAX_IDX) = _list_idx;                                        \
         IL_FLD(_list_idx) = CN_Tbl_Idx;                                       \
         IL_IDX(_list_idx) = CN_INTEGER_ZERO_IDX;                              \
         IL_LINE_NUM(_list_idx) = LINE;                                        \
         IL_COL_NUM(_list_idx)  = COL;                                         \
         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(_list_idx));                         \
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(_list_idx)) = _list_idx;            \
         _list_idx = IL_NEXT_LIST_IDX(_list_idx);                              \
         COPY_OPND(IL_OPND(_list_idx), (THE_OPND));                            \
         IL_LINE_NUM(_list_idx) = LINE;                                        \
         IL_COL_NUM(_list_idx) = COL;                                          \
        }


#ifdef KEY /* Bug 5089, 14150 */
# define FUNCTION_MUST_BE_SUBROUTINE(FCN_IDX,RSLT_IDX)                         \
   (TYP_TYPE(ATD_TYPE_IDX(RSLT_IDX)) == Character     ||                       \
    (TYP_TYPE(ATD_TYPE_IDX(RSLT_IDX)) == Structure                             \
      && ! c_ptr_abi_trouble(TYP_IDX(ATD_TYPE_IDX(RSLT_IDX)))) ||              \
    ATD_ARRAY_IDX(RSLT_IDX) != NULL_IDX ||                                     \
    ATD_IM_A_DOPE(RSLT_IDX) ||						       \
    special_case_fcn_to_sub(FCN_IDX))
#else /* KEY Bug 5089 */
# define FUNCTION_MUST_BE_SUBROUTINE(ATTR_IDX)                                 \
   (TYP_TYPE(ATD_TYPE_IDX(ATTR_IDX)) == Character     ||                       \
    TYP_TYPE(ATD_TYPE_IDX(ATTR_IDX)) == Structure     ||                       \
    ATD_ARRAY_IDX(ATTR_IDX) != NULL_IDX ||                                     \
    ATD_IM_A_DOPE(ATTR_IDX))
#endif /* KEY Bug 5089 */


/***********************************************\
|* Reference macros for the Stmt_Expansion_Opr *|
\***********************************************/

# define STMT_EXPAND_BEFORE_START_SH(IDX)                                      \
         IL_IDX(IR_IDX_R(IDX))

# define STMT_EXPAND_BEFORE_END_SH(IDX)                                        \
         IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(IDX)))

# define STMT_EXPAND_AFTER_START_SH(IDX)                                       \
         IL_IDX(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(IDX))))

# define STMT_EXPAND_AFTER_END_SH(IDX)                                         \
  IL_IDX(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(IDX)))))

# define PUSH_CURR_STMT                                                        \
   { int _list_idx;                                                            \
      NTR_IR_LIST_TBL(_list_idx);                                              \
      IL_FLD(_list_idx) = SH_Tbl_Idx;                                          \
      IL_IDX(_list_idx) = curr_stmt_sh_idx;                                    \
      if (curr_stmt_stk_il_idx == NULL_IDX) {                                  \
         curr_stmt_stk_il_idx = _list_idx;                                     \
      } else {                                                                 \
         IL_NEXT_LIST_IDX(_list_idx) = curr_stmt_stk_il_idx;                   \
         curr_stmt_stk_il_idx = _list_idx;                                     \
      }                                                                        \
   }

# define POP_CURR_STMT                                                         \
   if (curr_stmt_stk_il_idx) { int _save_list_idx;                             \
      curr_stmt_sh_idx = IL_IDX(curr_stmt_stk_il_idx);                         \
      _save_list_idx = curr_stmt_stk_il_idx;                                   \
      curr_stmt_stk_il_idx = IL_NEXT_LIST_IDX(curr_stmt_stk_il_idx);           \
      FREE_IR_LIST_NODE(_save_list_idx);                                       \
   }

/***************************************************\
|* Macro to increment the globals statement_number *|
\***************************************************/

# define INCREMENT_STATEMENT_NUMBER 					       \
	prev_statement_number++;					       \
	statement_number = prev_statement_number;
