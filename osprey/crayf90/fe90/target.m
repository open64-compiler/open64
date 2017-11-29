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

/* USMID:  "\n@(#)5.0_pl/macros/target.m	5.10	10/08/99 08:26:21\n" */

/* This module is for target specific information.                        */   

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
# define      RESTRICT        restrict
# else
# define      RESTRICT
# endif

# if defined(_TARGET64)

#    define TARGET_CHARS_PER_WORD		8	/* Chars per word     */
#    define TARGET_BITS_PER_WORD		64	/* Bits per word      */
#    define TARGET_BYTES_PER_WORD		8	/* Bytes per word     */

#    define TARGET_BYTES_TO_WORDS(BYTE_SIZE)    (((BYTE_SIZE)+7) >> 3)
#    define TARGET_BITS_TO_WORDS(BIT_SIZE)	(((BIT_SIZE)+63) >> 6)
#    define WORD_ALIGNED_BIT_LENGTH(BIT_SIZE)   ((((BIT_SIZE) + 63) >> 6) << 6)

#    define MAX_WORDS_FOR_INTEGER		1
#    define MAX_WORDS_FOR_NUMERIC		4
#    define MAX_SHORT_TYPELESS_BITS		256
#    define MAX_CHARS_IN_TYPELESS		32

#    define WORD_ALIGN				Align_64

# elif defined(_TARGET32)

#    define TARGET_CHARS_PER_WORD		4	/* Chars per word     */
#    define TARGET_BITS_PER_WORD		32	/* Bits per word      */
#    define TARGET_BYTES_PER_WORD		4	/* Bytes per word     */

#    define TARGET_BYTES_TO_WORDS(BYTE_SIZE)	(((BYTE_SIZE)+3) >> 2)
#    define TARGET_BITS_TO_WORDS(BIT_SIZE)	(((BIT_SIZE)+31) >> 5)
#    define WORD_ALIGNED_BIT_LENGTH(BIT_SIZE)   ((((BIT_SIZE) + 31) >> 5) << 5)

#    define MAX_WORDS_FOR_INTEGER		2
#    define MAX_WORDS_FOR_NUMERIC		8
#    define MAX_SHORT_TYPELESS_BITS		256
#    define MAX_CHARS_IN_TYPELESS		32

#    define WORD_ALIGN				Align_32

# endif

# if defined(_TARGET64)
# define MAX_DV_EL_LEN		4611686018427387904     /* 2**62  bits  */
# elif defined(_TARGET32)
# define MAX_DV_EL_LEN		1073741823		/* (2**30) -1 bits  */
# endif

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# define UNNAMED_PROGRAM_NAME		"MAIN__"
# define UNNAMED_PROGRAM_NAME_LEN	6
# define BLANK_COMMON_NAME		"_BLNK__"
# define BLANK_COMMON_NAME_LEN		7
# else
# define UNNAMED_PROGRAM_NAME		"$MAIN"
# define UNNAMED_PROGRAM_NAME_LEN	5
# define BLANK_COMMON_NAME		"//"
# define BLANK_COMMON_NAME_LEN		2
# endif

# if 0
/* This is the string that gives unique names to module and internal procs.   */
# endif

# if defined(_TARGET_OS_UNICOS) || defined(_TARGET_OS_MAX)
#     define UNIQUE_PROC_CONNECTOR		"_in_"
#     define UNIQUE_PROC_LEN			4
# else
#     define UNIQUE_PROC_CONNECTOR		".in."
#     define UNIQUE_PROC_LEN			4
# endif

# if 0
/*  On Cray systems, the environment variable CRAYLIBS is used as the path to */
/*  libmodules.a.  The file libmodules.a contains optional system modules.    */
/*  On IRIX systems, the system modules are not built into a library and the  */
/*  CRAYLIBS environment variable is not used; environment variable           */
/*  FTN_SYSTEM_MODULES is used instead as the path and the modules are        */
/*  individually available in the specified directory.  Some time in the      */
/*  future, Cray systems should probably also phase over to using             */
/*  FTN_SYSTEM_MODULES so the location of the system modules can be decoupled */
/*  from the location of the Cray libs.					      */
/*  Module processing checks these system modules after searching user        */
/*  specified paths and the current directory.  If the shell variable is      */
/*  unset, the compiler will just not look there.  No message is issued.      */
/*  The MODULE_USE_SYSTEM_PATH stuff should be phased out.                    */
# endif

# if (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))
# define MODULE_USE_SYSTEM_PATH_VAR		"FTN_SYSTEM_MODULES"
# else
# define MODULE_USE_SYSTEM_PATH_VAR		"CRAYLIBS"
# define MODULE_USE_SYSTEM_FILE			"libmodules.a"
# endif

# define SYSTEM_MODULE_USE_VAR			"FORTRAN_SYSTEM_MODULES"


# if defined(KEY) /* Bug 4469 */ && (defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))
/* Standard C stdio guarantees FILENAME_MAX will be defined (we don't need to
 * know the value for a particular filesystem, just the max which this stdio
 * supports); Posix guarantees HOST_NAME_MAX */
# define MAX_FILE_NAME_SIZE     FILENAME_MAX + 1
# define MAX_PATH_NAME_SIZE     FILENAME_MAX + 1
# define MACHINENAMELEN		HOST_NAME_MAX + 1

# elif defined(_HOST_OS_MAX) || defined(_HOST_OS_UNICOS)
# define MAX_FILE_NAME_SIZE     PATH_MAX+1
# define MAX_PATH_NAME_SIZE     PATH_MAX+1
# define MACHINENAMELEN		65

# elif (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX)) || defined(_HOST_OS_SOLARIS)
/* Irix: Filename limit defined in /usr/include/limits.h as "FILENAME_MAX" */
# define MAX_FILE_NAME_SIZE     1025
# define MAX_PATH_NAME_SIZE     1025
# define MACHINENAMELEN		65

# else
/* Linux:                                                                    */
/* 27Dec00[sos]: PV 764378 -- increase to match _POSIX_PATH_MAX as defined   */
/*                            in /usr/include/bits/posix1_lim.h (included by */
/*                            /usr/include/limits.h).                        */
# define MAX_FILE_NAME_SIZE	256
# define MAX_PATH_NAME_SIZE	256
# define MACHINENAMELEN		65
# endif


# if defined(_HOST32) && defined(_TARGET64)
# define LEX_STRTOL		strtoll

# else
# define LEX_STRTOL		strtol
# endif

# if 0
/* Hard code for now, as gcc can't seem to get it correct. */
# endif

# if defined(_TARGET_SV2)
# define	OUR_LONG_MAX		2147483647
# else
# define	OUR_LONG_MAX		LONG_MAX
# endif


# if 0
/* Used to fill in the large word for table searches */
# endif

# if defined(_HOST_LITTLE_ENDIAN)

     /* Initialize byte storage with long data, swap it (without assuming    */
     /* any particular representation) - else symbol table lookups will fail */

# ifdef _HOST64
# define	LARGE_WORD_FOR_TBL_SRCH					       \
		 ( (LONG_MAX &               0xffUL) << 56)		       \
		|( (LONG_MAX &             0xff00UL) << 40)		       \
		|( (LONG_MAX &           0xff0000UL) << 24)		       \
		|( (LONG_MAX &         0xff000000UL) <<  8)		       \
		|( (LONG_MAX                         >>  8) & 0xff000000UL)    \
		|( (LONG_MAX                         >> 24) & 0x00ff0000UL)    \
		|( (LONG_MAX                         >> 40) & 0x0000ff00UL)    \
		|( (LONG_MAX                         >> 56) & 0x000000ffUL)
# else
# define	LARGE_WORD_FOR_TBL_SRCH					       \
		 ( (LONG_MAX &           0xffUL) << 24)			       \
		|( (LONG_MAX &         0xff00UL) <<  8)			       \
		|( (LONG_MAX                     >>  8) & 0xff00UL)	       \
		|( (LONG_MAX                     >> 24) & 0x00ffUL)
# endif

# else
# define	LARGE_WORD_FOR_TBL_SRCH		OUR_LONG_MAX
# endif

# if 0
/* TYPE - defaults.  Please see type.h.                            */
/* CG_LOGICAL_DEFAULT_TYPE is the compiler generated logical type. */
# endif

# define	LARGEST_INTEGER_TYPE	Integer_8

# if defined(_TARGET32) || defined(_WHIRL_HOST64_TARGET64)
# define CG_LOGICAL_DEFAULT_TYPE	Logical_4
# elif defined(_TARGET64)
# define CG_LOGICAL_DEFAULT_TYPE	Logical_8
# endif

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

# define SA_INTEGER_DEFAULT_TYPE ((cmd_line_flags.s_pointer8) ? \
					 Integer_8 : Integer_4)
# elif defined(_TARGET_OS_MAX)
# define SA_INTEGER_DEFAULT_TYPE        CG_INTEGER_DEFAULT_TYPE
# else
# define SA_INTEGER_DEFAULT_TYPE	INTEGER_DEFAULT_TYPE
# endif

# define TARGET_MAX_HALF_WORD_STORAGE_TYPE(type_idx)			       \
	((TYP_LINEAR(type_idx) == Real_4) ||				       \
	 (TYP_LINEAR(type_idx) == Integer_1) ||				       \
	 (TYP_LINEAR(type_idx) == Integer_2) ||				       \
	 (TYP_LINEAR(type_idx) == Integer_4) ||				       \
	 (TYP_LINEAR(type_idx) == Logical_1) ||				       \
	 (TYP_LINEAR(type_idx) == Logical_2) ||				       \
	 (TYP_LINEAR(type_idx) == Logical_4))

# define TARGET_32BIT_DOUBLE_WORD_STORAGE_TYPE(type_idx)		       \
	((TYP_LINEAR(type_idx) == Real_8) ||				       \
	 (TYP_LINEAR(type_idx) == Real_16) ||				       \
	 (TYP_LINEAR(type_idx) == Complex_8) ||				       \
	 (TYP_LINEAR(type_idx) == Complex_16) ||			       \
	 (TYP_LINEAR(type_idx) == Integer_8) ||				       \
	 (TYP_LINEAR(type_idx) == Logical_8))
 
# ifdef _TARGET_OS_UNICOS
#    define TRUE_VALUE          true_value
#    define FALSE_VALUE         0
# else
#    define TRUE_VALUE          1
#    define FALSE_VALUE         0
# endif


/*  Used for machine characteristics call */

# ifdef _TARGET_OS_UNICOS

#    ifdef _HOST_OS_UNICOS
#       define MAX_STORAGE_SIZE_IN_WORDS	target_machine.fld.mcmsz
#    else
#       define MAX_STORAGE_SIZE_IN_WORDS	8388608		/* 2**23 */
#    endif

# elif (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#    undef MAX_STORAGE_SIZE_IN_WORDS		/* Hard coded inline. */

# elif defined(_TARGET_OS_MAX)
#    define MAX_STORAGE_SIZE_IN_WORDS		target_machine.fld.mcmsz
# elif defined(_TARGET32)
#    define MAX_STORAGE_SIZE_IN_WORDS		67108864-1	/* 2**26 */
# else
#    define MAX_STORAGE_SIZE_IN_WORDS		8388608		/* 2**23 */
# endif

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# define	ISSUE_STORAGE_SIZE_EXCEEDED_MSG(ATTR_IDX, ERR_LVL)	       \
		PRINTMSG(AT_DEF_LINE(ATTR_IDX), 1435, ERR_LVL, 		       \
                         AT_DEF_COLUMN(ATTR_IDX),                              \
                         AT_OBJ_NAME_PTR(ATTR_IDX),                            \
                         (cmd_line_flags.s_pointer8) ?                         \
                         "9,007,199,254,740,992" : "536,870,912");


# define	ISSUE_EXPR_SIZE_EXCEEDED_MSG(LINE, COLUMN, ERR_LVL)	       \
		PRINTMSG(LINE, 1434, ERR_LVL, COLUMN,                          \
                         (cmd_line_flags.s_pointer8) ?                         \
                         "9,007,199,254,740,992" : "536,870,912");
# else
# define	ISSUE_STORAGE_SIZE_EXCEEDED_MSG(ATTR_IDX, ERR_LVL)	       \
		PRINTMSG(AT_DEF_LINE(ATTR_IDX), 614, ERR_LVL, 		       \
                         AT_DEF_COLUMN(ATTR_IDX),                              \
                         AT_OBJ_NAME_PTR(ATTR_IDX),                            \
                         MAX_STORAGE_SIZE_IN_WORDS);

# define	ISSUE_EXPR_SIZE_EXCEEDED_MSG(LINE, COLUMN, ERR_LVL)	       \
		PRINTMSG(LINE, 615, ERR_LVL, COLUMN,                           \
                         MAX_STORAGE_SIZE_IN_WORDS);
# endif

/* Used for IO processing */

# if defined(_TARGET_OS_UNICOS) || defined(_TARGET_OS_MAX)
#    define MAX_NUM_CILIST_WORDS        7
# else
#    define MAX_NUM_CILIST_WORDS        8
# endif

/* Used to set ATP_EXT_NAME_IDX */

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# define MAKE_EXTERNAL_NAME(ATTR_IDX, NAME_IDX, NAME_LEN)		       \
	make_external_name(ATTR_IDX, NAME_IDX, NAME_LEN);
# else
# define MAKE_EXTERNAL_NAME(ATTR_IDX, NAME_IDX, NAME_LEN)		       \
	ATP_EXT_NAME_IDX(ATTR_IDX)	= NAME_IDX;			       \
	ATP_EXT_NAME_LEN(ATTR_IDX)	= NAME_LEN;
# endif
	 

/* Following are commandline options and directives that are not accepted   */
/* on all supported platforms.  undef is not accepted, defined is accepted. */
/* If a commandline option or a directive is added (or changed) so that it  */
/* is not accepted on one or more platforms, it should be added here and    */
/* the new ifdef used where ever platform dependent code is found.          */

/* NOTE:  The PDGCS interface makes -ef/-df and -k mutually exclusive.      */
/* NOTE:  On UNICOS, the unroll commandline options are not accepted, but   */
/*        the directives are accepted.                                      */

# if defined(_TARGET_SV2)
#	define		_INLINE_INTRINSICS		TRUE

#	undef		_ACCEPT_BL
#	define		_ACCEPT_FLOW			TRUE
#	define		_ACCEPT_INLINE			TRUE
#	define		_ACCEPT_MIC_SEND		TRUE
#	define		_ACCEPT_MIC_WAIT		TRUE
#	define		_ACCEPT_PATTERN			TRUE
#       undef           _ACCEPT_SHORTCIRCUIT
#	undef		_ACCEPT_SPLIT
#	define		_ACCEPT_STREAM			TRUE
#	undef		_ACCEPT_TASK
#	define		_ACCEPT_VECTOR			TRUE
#	define		_ACCEPT_VSEARCH			TRUE

#	undef		_ACCEPT_CMD_align
#	undef		_ACCEPT_CMD_a_static_threadprivate
#	undef		_ACCEPT_CMD_a_dalign
#	undef		_ACCEPT_CMD_a_pad
#	undef		_ACCEPT_CMD_k
#	undef		_ACCEPT_CMD_Gd
#	define		_ACCEPT_CMD_J			TRUE
#	undef		_ACCEPT_CMD_P
#	define		_ACCEPT_CMD_s_cf77types		TRUE
#	undef		_ACCEPT_CMD_s_32
#	define		_ACCEPT_CMD_s_64		TRUE
#	undef		_ACCEPT_CMD_X

#	define		_ACCEPT_CMD_ed_g		TRUE

# if defined(_INTEGER_1_AND_2)
#	define		_ACCEPT_CMD_ed_h		TRUE
# else
#	undef		_ACCEPT_CMD_ed_h
# endif
#	define		_ACCEPT_CMD_ed_i		TRUE
#	define		_ACCEPT_CMD_ed_j		TRUE
#	define		_ACCEPT_CMD_ed_r		TRUE
#	undef		_ACCEPT_CMD_ed_z
#	undef		_ACCEPT_CMD_ed_A
#	undef		_ACCEPT_CMD_ed_C
#	define		_ACCEPT_CMD_ed_D		TRUE
#	undef		_ACCEPT_CMD_ed_U
#	define		_ACCEPT_CMD_ed_X		TRUE
#	define		_ACCEPT_CMD_ed_0		TRUE

#	define		_ACCEPT_CMD_O_FASTINT		TRUE
#	define		_ACCEPT_CMD_O_FUSION		TRUE
#	define		_ACCEPT_CMD_O_JUMP		TRUE
#	define		_ACCEPT_CMD_O_LOOPALIGN		TRUE
#	undef		_ACCEPT_CMD_O_MATMUL_INLINE
#	define		_ACCEPT_CMD_O_OPT_INFO		TRUE
#	undef		_ACCEPT_CMD_O_PIPELINE
#       undef           _ACCEPT_CMD_O_RESHAPE
#	undef		_ACCEPT_CMD_O_UNROLL
#	define		_ACCEPT_CMD_O_ZEROINC		TRUE

#	undef		_ACCEPT_DIR_SHORTLOOP128

# elif defined(_TARGET_OS_UNICOS)
#	define		_INLINE_INTRINSICS		TRUE

#	define		_ACCEPT_BL			TRUE
#	define		_ACCEPT_FLOW			TRUE
#	define		_ACCEPT_INLINE			TRUE
#	define		_ACCEPT_MIC_SEND		TRUE
#	define		_ACCEPT_MIC_WAIT		TRUE
#	define		_ACCEPT_PATTERN			TRUE
#       undef           _ACCEPT_SHORTCIRCUIT
#	undef		_ACCEPT_SPLIT
#	define		_ACCEPT_STREAM			TRUE
#	define		_ACCEPT_TASK			TRUE
#	define		_ACCEPT_VECTOR			TRUE
#	define		_ACCEPT_VSEARCH			TRUE

#	undef		_ACCEPT_CMD_align
#	undef		_ACCEPT_CMD_a_static_threadprivate
#	undef		_ACCEPT_CMD_a_dalign
#	undef		_ACCEPT_CMD_a_pad
#	undef		_ACCEPT_CMD_k
#	undef		_ACCEPT_CMD_Gd
#	define		_ACCEPT_CMD_J			TRUE
#	undef		_ACCEPT_CMD_P
#	define		_ACCEPT_CMD_s_cf77types		TRUE
#	undef		_ACCEPT_CMD_s_32
#	undef		_ACCEPT_CMD_s_64
#	undef		_ACCEPT_CMD_X

#	define		_ACCEPT_CMD_ed_g		TRUE

# if defined(_INTEGER_1_AND_2)
#	define		_ACCEPT_CMD_ed_h		TRUE
# else
#	undef		_ACCEPT_CMD_ed_h
# endif
#	define		_ACCEPT_CMD_ed_i		TRUE
#	define		_ACCEPT_CMD_ed_j		TRUE
#	define		_ACCEPT_CMD_ed_r		TRUE
#	undef		_ACCEPT_CMD_ed_z
#	undef		_ACCEPT_CMD_ed_A
#	undef		_ACCEPT_CMD_ed_C
#	define		_ACCEPT_CMD_ed_D		TRUE
#	undef		_ACCEPT_CMD_ed_U
#	define		_ACCEPT_CMD_ed_X		TRUE
#	define		_ACCEPT_CMD_ed_0		TRUE

#	define		_ACCEPT_CMD_O_FASTINT		TRUE
#	undef		_ACCEPT_CMD_O_FUSION
#	undef		_ACCEPT_CMD_O_JUMP
#	define		_ACCEPT_CMD_O_LOOPALIGN		TRUE
#	undef		_ACCEPT_CMD_O_MATMUL_INLINE
#	define		_ACCEPT_CMD_O_OPT_INFO		TRUE
#	undef		_ACCEPT_CMD_O_PIPELINE
#       undef           _ACCEPT_CMD_O_RESHAPE
#	undef		_ACCEPT_CMD_O_UNROLL
#	define		_ACCEPT_CMD_O_ZEROINC		TRUE

#	define		_ACCEPT_DIR_SHORTLOOP128	TRUE

# elif defined(_TARGET_OS_MAX)
#	define		_INLINE_INTRINSICS		TRUE

#	define		_ACCEPT_BL			TRUE
#	undef		_ACCEPT_FLOW
#	define		_ACCEPT_INLINE			TRUE
#	undef		_ACCEPT_MIC_SEND
#	undef		_ACCEPT_MIC_WAIT
#	define		_ACCEPT_PATTERN			TRUE
#	define		_ACCEPT_SPLIT			TRUE
#       undef           _ACCEPT_SHORTCIRCUIT
#	undef		_ACCEPT_STREAM
#	undef		_ACCEPT_TASK
#	define		_ACCEPT_VECTOR			TRUE
#	undef		_ACCEPT_VSEARCH

#	undef		_ACCEPT_CMD_align
#	undef		_ACCEPT_CMD_a_dalign
#	undef		_ACCEPT_CMD_a_static_threadprivate
#	define		_ACCEPT_CMD_a_pad		TRUE
#	undef		_ACCEPT_CMD_k
#	define		_ACCEPT_CMD_s_cf77types		TRUE
#	define		_ACCEPT_CMD_s_32		TRUE
#	undef		_ACCEPT_CMD_s_64
#	undef		_ACCEPT_CMD_Gd
#	define		_ACCEPT_CMD_J			TRUE
#	undef		_ACCEPT_CMD_P
#	define		_ACCEPT_CMD_X			TRUE

#	undef		_ACCEPT_CMD_ed_g
#	undef		_ACCEPT_CMD_ed_h
#	define		_ACCEPT_CMD_ed_i		TRUE
#	undef		_ACCEPT_CMD_ed_j
#	undef		_ACCEPT_CMD_ed_r
#	undef		_ACCEPT_CMD_ed_z
#	define		_ACCEPT_CMD_ed_A		TRUE
#	define		_ACCEPT_CMD_ed_C		TRUE
#	define		_ACCEPT_CMD_ed_D		TRUE
#	undef		_ACCEPT_CMD_ed_U
#	undef		_ACCEPT_CMD_ed_X
#	define		_ACCEPT_CMD_ed_0		TRUE

#	undef		_ACCEPT_CMD_O_FASTINT
#	define		_ACCEPT_CMD_O_FUSION		TRUE
#	define		_ACCEPT_CMD_O_JUMP		TRUE
#	undef		_ACCEPT_CMD_O_LOOPALIGN
#	undef		_ACCEPT_CMD_O_MATMUL_INLINE
#	define		_ACCEPT_CMD_O_PIPELINE		TRUE
#	define		_ACCEPT_CMD_O_RESHAPE		TRUE
#	define		_ACCEPT_CMD_O_OPT_INFO		TRUE
#	define		_ACCEPT_CMD_O_UNROLL		TRUE
#	define		_ACCEPT_CMD_O_ZEROINC		TRUE

#	undef		_ACCEPT_DIR_SHORTLOOP128

# elif (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#	define		_INLINE_INTRINSICS		TRUE

#	undef		_ACCEPT_BL
#	undef		_ACCEPT_FLOW
#	define		_ACCEPT_INLINE			TRUE
#	undef		_ACCEPT_MIC_SEND
#	undef		_ACCEPT_MIC_WAIT
#	undef		_ACCEPT_PATTERN
#	undef		_ACCEPT_SPLIT
#       define          _ACCEPT_SHORTCIRCUIT            TRUE
#	undef		_ACCEPT_STREAM
#	define		_ACCEPT_TASK			TRUE
#	undef		_ACCEPT_VECTOR
#	undef		_ACCEPT_VSEARCH

#	define		_ACCEPT_CMD_align		TRUE
#	define		_ACCEPT_CMD_a_dalign		TRUE
#	define		_ACCEPT_CMD_a_static_threadprivate	TRUE
#	undef		_ACCEPT_CMD_a_pad
#	define		_ACCEPT_CMD_k			TRUE
#	undef		_ACCEPT_CMD_s_cf77types
#	undef		_ACCEPT_CMD_s_32
#	define		_ACCEPT_CMD_s_64		TRUE
#	define		_ACCEPT_CMD_Gd			TRUE
#	undef		_ACCEPT_CMD_J
#	define		_ACCEPT_CMD_P			TRUE
#	undef		_ACCEPT_CMD_X

#	define		_ACCEPT_CMD_ed_g		TRUE
#	undef		_ACCEPT_CMD_ed_h
#	undef		_ACCEPT_CMD_ed_i
#	define		_ACCEPT_CMD_ed_j		TRUE
#	undef		_ACCEPT_CMD_ed_r
#	define		_ACCEPT_CMD_ed_z		TRUE
#	undef		_ACCEPT_CMD_ed_A
#	undef		_ACCEPT_CMD_ed_C
# ifdef KEY
#	define		_ACCEPT_CMD_ed_D                TRUE
# else
#	undef		_ACCEPT_CMD_ed_D
# endif
#	define		_ACCEPT_CMD_ed_U		TRUE
#	define		_ACCEPT_CMD_ed_X		TRUE
#	undef		_ACCEPT_CMD_ed_0

#	undef		_ACCEPT_CMD_O_FASTINT
#	undef		_ACCEPT_CMD_O_FUSION
#	undef		_ACCEPT_CMD_O_JUMP
#	undef		_ACCEPT_CMD_O_LOOPALIGN
#	define		_ACCEPT_CMD_O_MATMUL_INLINE	TRUE
#	undef		_ACCEPT_CMD_O_OPT_INFO
#	undef		_ACCEPT_CMD_O_PIPELINE
#       undef           _ACCEPT_CMD_O_RESHAPE
#	define		_ACCEPT_CMD_O_UNROLL		TRUE
#	define		_ACCEPT_CMD_O_ZEROINC		TRUE

#	undef		_ACCEPT_DIR_SHORTLOOP128

# elif defined(_TARGET_OS_SOLARIS)
#	define		_INLINE_INTRINSICS		TRUE

#	undef		_ACCEPT_BL
#	undef		_ACCEPT_FLOW
#	undef		_ACCEPT_INLINE
#	undef		_ACCEPT_MIC_SEND
#	undef		_ACCEPT_MIC_WAIT
#	define		_ACCEPT_PATTERN			TRUE
#	undef		_ACCEPT_SPLIT
#       undef           _ACCEPT_SHORTCIRCUIT
#	undef		_ACCEPT_STREAM
#	define		_ACCEPT_TASK			TRUE
#	undef		_ACCEPT_VECTOR
#	undef		_ACCEPT_VSEARCH

#	undef 		_ACCEPT_CMD_align
#	define		_ACCEPT_CMD_a_dalign		TRUE
#	undef		_ACCEPT_CMD_a_static_threadprivate
#	undef		_ACCEPT_CMD_a_pad
#	define		_ACCEPT_CMD_k			TRUE
#	undef		_ACCEPT_CMD_s_cf77types
#	undef		_ACCEPT_CMD_s_32
#	define		_ACCEPT_CMD_s_64		TRUE
#	undef		_ACCEPT_CMD_Gd
#	undef		_ACCEPT_CMD_J
#	define		_ACCEPT_CMD_P			TRUE
#	undef		_ACCEPT_CMD_X

#	define		_ACCEPT_CMD_ed_g		TRUE
#	undef		_ACCEPT_CMD_ed_h
#	undef		_ACCEPT_CMD_ed_i
#	define		_ACCEPT_CMD_ed_j		TRUE
#	undef		_ACCEPT_CMD_ed_r
#	undef		_ACCEPT_CMD_ed_z
#	undef		_ACCEPT_CMD_ed_A
#	undef		_ACCEPT_CMD_ed_C
#	undef		_ACCEPT_CMD_ed_D
#	define		_ACCEPT_CMD_ed_U		TRUE
#	define		_ACCEPT_CMD_ed_X		TRUE
#	undef		_ACCEPT_CMD_ed_0

#	undef		_ACCEPT_CMD_O_FASTINT
#	undef		_ACCEPT_CMD_O_FUSION
#	undef		_ACCEPT_CMD_O_JUMP
#	undef		_ACCEPT_CMD_O_LOOPALIGN
#	undef		_ACCEPT_CMD_O_MATMUL_INLINE
#	undef		_ACCEPT_CMD_O_OPT_INFO
#	undef		_ACCEPT_CMD_O_PIPELINE
#       undef           _ACCEPT_CMD_O_RESHAPE
#	define		_ACCEPT_CMD_O_UNROLL		TRUE
#	define		_ACCEPT_CMD_O_ZEROINC		TRUE

#	undef		_ACCEPT_DIR_SHORTLOOP128

# else 
#	undef		_INLINE_INTRINSICS	

#	undef		_ACCEPT_BL
#	undef		_ACCEPT_FLOW
#	undef		_ACCEPT_INLINE
#	undef		_ACCEPT_MIC_SEND
#	undef		_ACCEPT_MIC_WAIT
#	undef		_ACCEPT_PATTERN
#	undef		_ACCEPT_TASK
#	undef		_ACCEPT_STREAM
#	undef		_ACCEPT_VECTOR
#	undef		_ACCEPT_VSEARCH

#	undef 		_ACCEPT_CMD_align
#	undef		_ACCEPT_CMD_a_dalign
#	undef		_ACCEPT_CMD_a_static_threadprivate
#	undef		_ACCEPT_CMD_a_pad
#	undef		_ACCEPT_CMD_k
#	undef		_ACCEPT_CMD_s_32
#	undef		_ACCEPT_CMD_s_64
#	undef		_ACCEPT_CMD_Gd
#	undef		_ACCEPT_CMD_J
#	undef		_ACCEPT_CMD_P
#	undef		_ACCEPT_CMD_X

#	define		_ACCEPT_CMD_ed_g		TRUE
#	undef		_ACCEPT_CMD_ed_h
#	define		_ACCEPT_CMD_ed_i		TRUE
#	define		_ACCEPT_CMD_ed_j		TRUE
#	define		_ACCEPT_CMD_ed_r		TRUE
#	undef		_ACCEPT_CMD_ed_z
#	undef		_ACCEPT_CMD_ed_A
#	undef		_ACCEPT_CMD_ed_C
#	define		_ACCEPT_CMD_ed_X		TRUE
#	undef		_ACCEPT_CMD_ed_0

#	undef		_ACCEPT_CMD_O_FASTINT
#	undef		_ACCEPT_CMD_O_FUSION
#	undef		_ACCEPT_CMD_O_JUMP
#	undef		_ACCEPT_CMD_O_LOOPALIGN
#	undef		_ACCEPT_CMD_O_MATMUL_INLINE
#	undef		_ACCEPT_CMD_O_OPT_INFO
#	undef		_ACCEPT_CMD_O_PIPELINE
#       undef           _ACCEPT_CMD_O_RESHAPE
#	undef		_ACCEPT_CMD_O_UNROLL
#	define		_ACCEPT_CMD_O_ZEROINC		TRUE

#	undef		_ACCEPT_DIR_SHORTLOOP128

# endif


/* The following directive acceptance is based on command line */
/* option acceptance and other ifdefs.                         */

# if defined(_ACCEPT_BL)
#    define		CDIR_BL			TRUE
# else
#    define		CDIR_BL			FALSE
# endif

# if defined(_ACCEPT_FLOW)
#    define		CDIR_FLOW		TRUE
# else
#    define		CDIR_FLOW		FALSE
# endif

# if defined(_ACCEPT_INLINE)
#    define		CDIR_INLINE		TRUE
# else
#    define		CDIR_INLINE		FALSE
# endif

# if defined(_ACCEPT_STREAM)
#    define		CDIR_STREAM		TRUE
# else
#    define		CDIR_STREAM		FALSE
# endif

# if defined(_ACCEPT_PATTERN)
#    define		CDIR_PATTERN		TRUE
# else
#    define		CDIR_PATTERN		FALSE
# endif

# if defined(_ACCEPT_SPLIT)
#    define		CDIR_SPLIT		TRUE
# else
#    define		CDIR_SPLIT		FALSE
# endif

# if defined(_ACCEPT_TASK)
#    define		CDIR_TASK		TRUE
# else
#    define		CDIR_TASK		FALSE
# endif

# if defined(_ACCEPT_VECTOR)
#    define		CDIR_VECTOR		TRUE
# else
#    define		CDIR_VECTOR		FALSE
# endif

# if defined(_ACCEPT_VSEARCH)
#    define		CDIR_VSEARCH		TRUE
# else
#    define		CDIR_VSEARCH		FALSE
# endif

# if defined(_TASK_COMMON_EXTENSION)
#    define		CDIR_TASKCOMMON		TRUE
# else
#    define		CDIR_TASKCOMMON		FALSE
# endif


/* Directive acceptance for each platform */

# if defined(_TARGET_SV2)

#	define		CDIR_ALIGN			FALSE
#	define		CDIR_AUXILIARY			FALSE
#	define		CDIR_BLOCKABLE			TRUE
#	define		CDIR_CACHE_ALIGN		FALSE
#	define		CDIR_CACHE_BLOCK		TRUE
#	define		CDIR_CACHE_BYPASS		FALSE
#	define		CDIR_CACHE_NOALLOCATE		TRUE
#	define		CDIR_CONCURRENT			TRUE
#	define		CDIR_COPY_ASSUMED_SHAPE		TRUE
#	define		CDIR_INTERCHANGE		TRUE
#	define		CDIR_IVDEP			TRUE
#	define		CDIR_NOSIDEEFFECTS		TRUE
#	define		CDIR_PREFERTASK			TRUE
#	define		CDIR_PREFERVECTOR		TRUE
#	define		CDIR_RECURRENCE			TRUE
#	define		CDIR_SHORTLOOP			TRUE
#	define		CDIR_STACK			TRUE
#	define		CDIR_SUPPRESS			TRUE
#	define		CDIR_SYMMETRIC			FALSE
#	define		CDIR_UNROLL			TRUE
#	define		CDIR_USES_EREGS			FALSE
#	define		CDIR_VFUNCTION			TRUE

# elif defined(_TARGET_OS_UNICOS)

#	define		CDIR_ALIGN			TRUE
#	define		CDIR_AUXILIARY			TRUE
#	define		CDIR_BLOCKABLE			TRUE
#	define		CDIR_CACHE_ALIGN		FALSE
#	define		CDIR_CACHE_BLOCK		TRUE
#	define		CDIR_CACHE_BYPASS		FALSE
#	define		CDIR_CACHE_NOALLOCATE		FALSE
#	define		CDIR_CONCURRENT			TRUE
#	define		CDIR_COPY_ASSUMED_SHAPE		TRUE
#	define		CDIR_INTERCHANGE		TRUE
#	define		CDIR_IVDEP			TRUE
#	define		CDIR_NOSIDEEFFECTS		TRUE
#	define		CDIR_PREFERTASK			TRUE
#	define		CDIR_PREFERVECTOR		TRUE
#	define		CDIR_RECURRENCE			TRUE
#	define		CDIR_SHORTLOOP			TRUE
#	define		CDIR_STACK			TRUE
#	define		CDIR_SUPPRESS			TRUE
#	define		CDIR_SYMMETRIC			FALSE
#	define		CDIR_UNROLL			TRUE
#	define		CDIR_USES_EREGS			FALSE
#	define		CDIR_VFUNCTION			TRUE

# elif defined(_TARGET_OS_MAX)

#	define		CDIR_ALIGN			FALSE
#	define		CDIR_AUXILIARY			FALSE
#	define		CDIR_BLOCKABLE			TRUE
#	define		CDIR_CACHE_ALIGN		TRUE
#	define		CDIR_CACHE_BLOCK		TRUE
#	define		CDIR_CACHE_BYPASS		TRUE
#	define		CDIR_CACHE_NOALLOCATE		FALSE
#	define		CDIR_CONCURRENT			TRUE
#	define		CDIR_COPY_ASSUMED_SHAPE		TRUE
#	define		CDIR_INTERCHANGE		TRUE
#	define		CDIR_IVDEP			TRUE
#	define		CDIR_NOSIDEEFFECTS		FALSE
#	define		CDIR_PREFERTASK			FALSE
#	define		CDIR_PREFERVECTOR		FALSE
#	define		CDIR_RECURRENCE			TRUE
#	define		CDIR_SHORTLOOP			TRUE
#	define		CDIR_STACK			TRUE
#	define		CDIR_SUPPRESS			TRUE
#	define		CDIR_SYMMETRIC			TRUE
#	define		CDIR_UNROLL			TRUE
#	define		CDIR_USES_EREGS			TRUE
#	define		CDIR_VFUNCTION			FALSE

# elif (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#	define		CDIR_ALIGN			FALSE
#	define		CDIR_AUXILIARY			FALSE
#	define		CDIR_BLOCKABLE			FALSE
#	define		CDIR_CACHE_ALIGN		FALSE
#	define		CDIR_CACHE_BLOCK		FALSE
#	define		CDIR_CACHE_BYPASS		FALSE
#	define		CDIR_CACHE_NOALLOCATE		FALSE
#	define		CDIR_CONCURRENT			FALSE
#	define		CDIR_COPY_ASSUMED_SHAPE		FALSE
#	define		CDIR_INTERCHANGE		FALSE
#	define		CDIR_IVDEP			TRUE
#	define		CDIR_NOSIDEEFFECTS		TRUE
#	define		CDIR_PREFERTASK			TRUE
#	define		CDIR_PREFERVECTOR		FALSE
#	define		CDIR_RECURRENCE			FALSE
#	define		CDIR_SHORTLOOP			FALSE
#	define		CDIR_STACK			FALSE
#	define		CDIR_SUPPRESS			FALSE
#	define		CDIR_SYMMETRIC			FALSE
#	define		CDIR_UNROLL			TRUE
#	define		CDIR_USES_EREGS			FALSE
#	define		CDIR_VFUNCTION			FALSE

# elif defined(_TARGET_OS_SOLARIS)

#	define		CDIR_ALIGN			FALSE
#	define		CDIR_AUXILIARY			FALSE
#	define		CDIR_BLOCKABLE			FALSE
#	define		CDIR_CACHE_ALIGN		FALSE
#	define		CDIR_CACHE_BLOCK		FALSE
#	define		CDIR_CACHE_BYPASS		FALSE
#	define		CDIR_CACHE_NOALLOCATE		FALSE
#	define		CDIR_CONCURRENT			FALSE
#	define		CDIR_COPY_ASSUMED_SHAPE		FALSE
#	define		CDIR_INTERCHANGE		FALSE
#	define		CDIR_IVDEP			FALSE
#	define		CDIR_NOSIDEEFFECTS		FALSE
#	define		CDIR_PREFERTASK			TRUE
#	define		CDIR_PREFERVECTOR		FALSE
#	define		CDIR_RECURRENCE			TRUE
#	define		CDIR_SHORTLOOP			FALSE
#	define		CDIR_STACK			TRUE
#	define		CDIR_SUPPRESS			TRUE
#	define		CDIR_SYMMETRIC			FALSE
#	define		CDIR_UNROLL			TRUE
#	define		CDIR_USES_EREGS			FALSE
#	define		CDIR_VFUNCTION			FALSE

# endif 
