/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



# ifdef _PVP_PVP
#	define _ENABLE_FEI     			1
#	define _HOST64				1
#	define _TARGET64			1
#	define _HOST_OS_UNICOS			1
#	define _TARGET_OS_UNICOS		1
#	define _HEAP_REQUEST_IN_WORDS		1
#	define _TARGET_WORD_ADDRESS		1
#	define _MODULE_TO_DOT_o			1
#	define _ARITH_H				1
#	define _ARITH_INPUT_CONV		1
#	define _ALLOW_DATA_INIT_OF_COMMON	1
#	define _ASSIGN_OFFSETS_TO_HOSTED_STACK	1
#	define _CHECK_MAX_MEMORY		1
#	define _EXTENDED_CRI_CHAR_POINTER	1
#	define _F_MINUS_MINUS			1
#	define _FRONTEND_CONDITIONAL_COMP	1
#	define _GETPMC_AVAILABLE		1
#	define _NEED_AT_SIGN_INTRINSICS		1
#	define _POINTEES_CAN_BE_STRUCT		1
#	define _SPLIT_STATIC_STORAGE_3		1
#	define _TARGET_HAS_FAST_INTEGER		1
#	define _TASK_COMMON_EXTENSION		1
#	define _TRANSFORM_CHAR_SEQUENCE		1
#	define _TMP_GIVES_COMMON_LENGTH		1
#	define _FRONTEND_INLINER		1
#	define _D_LINES_SUPPORTED		1
# endif

# ifdef _MPP_MPP
#	define _ENABLE_FEI     			1
#	define _HOST64				1
#	define _TARGET64			1
#	define _HOST_OS_MAX			1
#	define _TARGET_OS_MAX			1
#	define _TARGET_IEEE			1
#	define _TARGET_BYTE_ADDRESS		1
#	define _HEAP_REQUEST_IN_WORDS		1
#	define _MODULE_TO_DOT_o			1
#	define _ARITH_H				1
#	define _ARITH_INPUT_CONV		1
#	define _ALLOW_DATA_INIT_OF_COMMON	1
#	define _ASSIGN_OFFSETS_TO_HOSTED_STACK	1
#	define _CHECK_MAX_MEMORY		1
#	define _ERROR_DUPLICATE_GLOBALS		1
#	define _EXTENDED_CRI_CHAR_POINTER	1
#	define _F_MINUS_MINUS			1
#	define _FRONTEND_CONDITIONAL_COMP	1
#	define _GETPMC_AVAILABLE		1
#	define _POINTEES_CAN_BE_STRUCT		1
#	define _SPLIT_STATIC_STORAGE_3		1
#	define _TARGET_PACK_HALF_WORD_TYPES	1
#	define _TMP_GIVES_COMMON_LENGTH		1
#	define _TRANSFORM_CHAR_SEQUENCE		1
#	define _TWO_WORD_FCD			1
#	define _FRONTEND_INLINER		1
#	define _INIT_RELOC_BASE_OFFSET		1
#	define _D_LINES_SUPPORTED		1
# endif

# ifdef _SGI_SV2
#       define _ENABLE_FEI                      1
#       define _HOST32                          1
#       define _TARGET32                        1
#       define _HOST_OS_IRIX                    1
#       define _TARGET_OS_UNICOS                1
#       define _TARGET_IEEE                     1
#       define _TARGET_SV2                      1
#	define _DOPE_VECTOR_32_OR_64		1
#       define _TARGET_BYTE_ADDRESS             1
#       define _HEAP_REQUEST_IN_BYTES           1
#	define _MODULE_TO_DOT_o			1
#       define _ARITH_H                         1
#       define _ARITH_INPUT_CONV                1
#	define _ALLOW_DATA_INIT_OF_COMMON	1
#	define _ASSIGN_OFFSETS_TO_HOSTED_STACK	1
#       define _CHAR_LEN_IN_BYTES               1
#       define _CHECK_MAX_MEMORY                1
#	define _ERROR_DUPLICATE_GLOBALS		1
#       define _F_MINUS_MINUS                   1
#       define _FRONTEND_CONDITIONAL_COMP       1
#       define _INTEGER_1_AND_2                 1
#	define _POINTEES_CAN_BE_STRUCT		1
#       define _QUAD_PRECISION                  1
#       define _SPLIT_STATIC_STORAGE_M          1
#       define _TARGET_DOUBLE_ALIGN             1
#	define _TMP_GIVES_COMMON_LENGTH		1
#       define _TWO_WORD_FCD                    1
#	define _FRONTEND_INLINER		1
#	define _SM_UNIT_IS_ELEMENT		1
#	define _D_LINES_SUPPORTED		1
# endif

# ifdef _SGI_SGI
#       define _ENABLE_FEI                      1
#	define _HOST32				1
#	define _TARGET32			1
#	define _HOST_OS_IRIX			1
#	define _TARGET_OS_IRIX			1
#	define _TARGET_IEEE			1
#	define _DOPE_VECTOR_32_OR_64		1
#	define _TARGET_BYTE_ADDRESS		1
#	define _HEAP_REQUEST_IN_BYTES		1
#	define _ARITH_H				1
#	define _ARITH_INPUT_CONV		1
#	define _ALIGN_REAL16_TO_16_BYTES	1
#	define _CHAR_IS_ALIGN_8			1
#	define _CHAR_LEN_IN_BYTES		1
#	define _CHECK_MAX_MEMORY		1
#	define _ERROR_DUPLICATE_GLOBALS		1
#	define _F_MINUS_MINUS			1
#	define _FILE_IO_OPRS			1
#	define _FRONTEND_CONDITIONAL_COMP	1
#	define _GEN_LOOPS_FOR_DV_WHOLE_DEF	1
#	define _HIGH_LEVEL_DO_LOOP_FORM		1
#	define _HIGH_LEVEL_IF_FORM		1
#	define _INTEGER_1_AND_2			1
#	define _NAME_SUBSTITUTION_INLINING	1
#	define _NO_CRAY_CHARACTER_PTR		1
#	define _NO_IO_ALTERNATE_RETURN		1
#	define _POINTEES_CAN_BE_STRUCT		1
#       define _QUAD_PRECISION                  1
#	define _SAVE_IO_STMT			1
#	define _SEPARATE_NONCOMMON_EQUIV_GROUPS	1
#	define _SPLIT_STATIC_STORAGE_M		1
#	define _SINGLE_ALLOCS_FOR_AUTOMATIC	1
#	define _TARGET_DOUBLE_ALIGN		1
#	define _TWO_WORD_FCD			1
#	define _WARNING_FOR_NUMERIC_INPUT_ERROR	1
#       define _ALTERNATIVE_INTERFACE_FOR_POINTEES 1
#	define _SEPARATE_DEALLOCATES		1
#	define _STOP_IS_OPR			1
#	define _TYPE_CODE_64_BIT		1
#	define _D_LINES_SUPPORTED		1
# endif

# ifdef _SOLARIS_SOLARIS
#	define _HOST32				1
#	define _TARGET32			1
#	define _HOST_OS_SOLARIS			1
#	define _TARGET_OS_SOLARIS		1
#	define _TARGET_IEEE			1
#	define _TARGET_BYTE_ADDRESS		1
#	define _HEAP_REQUEST_IN_BYTES		1
#	define _MODULE_TO_DOT_o			1
#	define _ARITH_H				1
#	define _ARITH_INPUT_CONV		1
#	define _ASSIGN_OFFSETS_TO_HOSTED_STACK	1
#	define _CHAR_LEN_IN_BYTES		1
#	define _CHECK_MAX_MEMORY		1
#	define _ERROR_DUPLICATE_GLOBALS		1
#	define _FRONTEND_CONDITIONAL_COMP	1
#	define _NO_BINARY_OUTPUT		1
#       define _QUAD_PRECISION                  1
#	define _SEPARATE_FUNCTION_RETURNS	1
#	define _SPLIT_STATIC_STORAGE_2		1
#	define _TARGET_DOUBLE_ALIGN		1
#	define _TASK_COMMON_EXTENSION		1
#	define _TMP_GIVES_COMMON_LENGTH		1
#	define _TRANSFORM_CHAR_SEQUENCE		1
#	define _TWO_WORD_FCD			1
#	define _FRONTEND_INLINER		1
#	define _INIT_RELOC_BASE_OFFSET		1
#	define _D_LINES_SUPPORTED		1
# endif

# ifdef _PVP_MPP
#	define _ENABLE_FEI     			1
#	define _HOST64				1
#	define _TARGET64			1
#	define _HOST_OS_UNICOS			1
#	define _TARGET_OS_MAX			1
#	define _TARGET_IEEE			1
#	define _TARGET_BYTE_ADDRESS		1
#	define _HEAP_REQUEST_IN_WORDS		1
#	define _MODULE_TO_DOT_o			1
#	define _ARITH_H				1
#	define _ARITH_INPUT_CONV		1
#	define _ALLOW_DATA_INIT_OF_COMMON	1
#	define _ASSIGN_OFFSETS_TO_HOSTED_STACK	1
#	define _CHECK_MAX_MEMORY		1
#	define _ERROR_DUPLICATE_GLOBALS		1
#	define _F_MINUS_MINUS			1
#	define _FRONTEND_CONDITIONAL_COMP	1
#	define _GETPMC_AVAILABLE		1
#	define _POINTEES_CAN_BE_STRUCT		1
#	define _SPLIT_STATIC_STORAGE_3		1
#	define _TARGET_PACK_HALF_WORD_TYPES	1
#	define _TRANSFORM_CHAR_SEQUENCE		1
#	define _TMP_GIVES_COMMON_LENGTH		1
#	define _TWO_WORD_FCD			1
#	define _FRONTEND_INLINER		1
#	define _INIT_RELOC_BASE_OFFSET		1
#	define _D_LINES_SUPPORTED		1
# endif




# if defined(_LINUX_LINUX) || defined(_DARWIN_DARWIN)



#	define _LITTLE_ENDIAN			1
#       define _GNU_SOURCE                      1
#       define _NO_XOPEN4                       1
#	define _HOST_LITTLE_ENDIAN		1
#	define _TARGET_LITTLE_ENDIAN		1
#       define _ENABLE_FEI                      1
# ifdef _LP64
#	define _HOST64				1
#	define _TARGET64			1
#	define _WHIRL_HOST64_TARGET64		1
#	define _TARGET_PACK_HALF_WORD_TYPES	1
# else
#	define _HOST32				1
#	define _TARGET32			1
#	define _DOPE_VECTOR_32_OR_64		1
#	define _TARGET_DOUBLE_ALIGN		1
# endif /* _LP64 */
#     if defined(_LINUX_LINUX)
#	define _HOST_OS_LINUX			1
#	define _TARGET_OS_LINUX			1
#     elif defined(_DARWIN_DARWIN)
#	define _HOST_OS_DARWIN			1
#	define _TARGET_OS_DARWIN		1
#     else /* _LINUX_LINUX */
#	error "Define _HOST_OS_XXX and _TARGET_OS_XXX" /* Just in case */
#     endif /* _LINUX_LINUX */
/* 30Jan01[sos] commented out: #	define _HOST_OS_IRIX			1 */
/* 30Jan01[sos] commented out: #	define _TARGET_OS_IRIX			1 */
#	define _TARGET_IEEE			1
#	define _TARGET_BYTE_ADDRESS		1
#	define _HEAP_REQUEST_IN_BYTES		1
#	define _ARITH_H				1
#	define _ALIGN_REAL16_TO_16_BYTES	1
#	define _CHAR_IS_ALIGN_8			1
#	define _CHAR_LEN_IN_BYTES		1
#	define _CHECK_MAX_MEMORY		1
#	define _ERROR_DUPLICATE_GLOBALS		1
#	define _F_MINUS_MINUS			1
#	define _FILE_IO_OPRS			1
#	define _FRONTEND_CONDITIONAL_COMP	1
#	define _GEN_LOOPS_FOR_DV_WHOLE_DEF	1
#	define _HIGH_LEVEL_DO_LOOP_FORM		1
#	define _HIGH_LEVEL_IF_FORM		1
#	define _INTEGER_1_AND_2			1
#	define _NAME_SUBSTITUTION_INLINING	1
#	define _NO_AT_SIGN_IN_NAMES		1
/* Bug 2001 */
#       ifdef KEY
#       define _EXTENDED_CRI_CHAR_POINTER       1
#       else
#	define _NO_CRAY_CHARACTER_PTR		1
#       endif
#	define _NO_IO_ALTERNATE_RETURN		1
#	define _POINTEES_CAN_BE_STRUCT		1
#       define _QUAD_PRECISION                  1
#	define _SAVE_IO_STMT			1
#	define _SEPARATE_NONCOMMON_EQUIV_GROUPS	1
#	define _SPLIT_STATIC_STORAGE_M		1
#	define _SINGLE_ALLOCS_FOR_AUTOMATIC	1
#	define _TWO_WORD_FCD			1
#	define _USE_FOLD_DOT_f			1
#	define _WARNING_FOR_NUMERIC_INPUT_ERROR	1
#       define _ALTERNATIVE_INTERFACE_FOR_POINTEES 1
#	define _SEPARATE_DEALLOCATES		1
#	define _STOP_IS_OPR			1
#	define _TYPE_CODE_64_BIT		1
#	define _SEPARATE_FUNCTION_RETURNS	1
#	define _D_LINES_SUPPORTED		1
# endif
