/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

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

#ifndef I_CVRT_INCLUDED
#define I_CVRT_INCLUDED

# ifdef __cplusplus
extern "C" {
# endif

/* USMID:  "\n@(#)5.0_pl/headers/i_cvrt.h	5.24	10/21/99 15:46:35\n" */

typedef long long int          INT64;
typedef int                    INT32;
typedef unsigned long          INTPTR;

#define PDGCS_MPP_INIT_APPRENTICE        0
#define PDGCS_MPP_INIT_S2P_COERCE        1
#define PDGCS_MPP_INIT_CCG_JUMPS         2

#define FEI_TASK_SAVELAST                0
#define FEI_TASK_AUTOSCOPE               1

#define FEI_MIF_TYPE_TABLE       	 0
#define FEI_MIF_OBJ_TABLE        	 1
#define FEI_MIF_TYPE_TABLE_MEM   	 2

#define STATIC_SUBSCRIPT_SIZE 7
#define IRIX_FILE_NAME_SIZE   129

#define WRITE_STMT      0
#define READ_STMT       1
#define WRITE_NML_STMT	2
#define READ_NML_STMT	3

#define FEI_DV_HDR_BASE_ADDR   1
#define FEI_DV_HDR_EL_LEN      2
#define FEI_DV_HDR_ASSOC       3
#define FEI_DV_HDR_PTR_ALLOC   4
#define FEI_DV_HDR_P_OR_A      5
#define FEI_DV_HDR_A_CONTIG    6
#define FEI_DV_HDR_N_DIM       7
#define FEI_DV_HDR_TYP_CODE    8
#define FEI_DV_HDR_ORIG_BASE   9
#define FEI_DV_HDR_ORIG_SIZE  10

#define MIPS_ASSERT_ARGUMENTALIASING                     1
#define MIPS_ASSERT_NOARGUMENTALIASING                   2
#define MIPS_ASSERT_BOUNDSVIOLATIONS                     3
#define MIPS_ASSERT_NOBOUNDSVIOLATIONS                   4
#define MIPS_ASSERT_CONCURRENTCALL                       5
#define MIPS_ASSERT_NOCONCURRENTCALL                     6
#define MIPS_ASSERT_NORECURRENCE                         7
#define MIPS_ASSERT_DOPREFER                             8
#define MIPS_ASSERT_EQUIVALENCEHAZARD                    9
#define MIPS_ASSERT_NOEQUIVALENCEHAZARD                 10
#define MIPS_ASSERT_LASTVALUENEEDED                     11
#define MIPS_ASSERT_LASTVALUESNEEDED                    12
#define MIPS_ASSERT_NOLASTVALUENEEDED                   13
#define MIPS_ASSERT_NOLASTVALUESNEEDED                  14
#define MIPS_ASSERT_PERMUTATION                         15
#define MIPS_ASSERT_RELATION                            16
#define MIPS_ASSERT_NOSYNC                              17
#define MIPS_ASSERT_TEMPORARIESFORCONSTANTARGUMENTS     18
#define MIPS_ASSERT_NOTEMPORARIESFORCONSTANTARGUMENTS   19
#define MIPS_ASSERT_DO                                  20
#define MIPS_ASSERT_BENIGN                              21
#define MIPS_ASSERT_DEPENDENCE                          22
#define MIPS_ASSERT_FREQUENCY                           23
#define MIPS_ASSERT_IGNOREANYDEPENDENCES                24
#define MIPS_ASSERT_IGNOREANYDEPENDENCE                 25
#define MIPS_ASSERT_IGNOREASSUMEDDEPENDENCES            26
#define MIPS_ASSERT_IGNOREASSUMEDDEPENDENCE             27
#define MIPS_ASSERT_NOINTERCHANGE                       28
#define MIPS_ASSERT_USECOMPRESS                         29
#define MIPS_ASSERT_USEEXPAND                           30
#define MIPS_ASSERT_USECONTROLLEDSTORE                  31
#define MIPS_ASSERT_USEGATHER                           32
#define MIPS_ASSERT_USESCATTER                          33

#define FEI_PROC_VFUNC      	 	 	 	 0
#define FEI_PROC_INLINE     	 	 	 	 1
#define FEI_PROC_TASKHD     	 	 	 	 2
#define FEI_PROC_UNIQMEM    	 	 	 	 3
#define FEI_PROC_CNGA       	 	 	 	 4
#define FEI_PROC_CNAA       	 	 	 	 5
#define FEI_PROC_CNGO       	 	 	 	 6 
#define FEI_PROC_RNGO       	 	 	 	 7 
#define FEI_PROC_NOIO       	 	 	 	 8
#define FEI_PROC_ERRNO      	 	 	 	 9
#define FEI_PROC_NOCALLS    				10 
#define FEI_PROC_SCHEDULE   				11
#define FEI_PROC_DNDA       				12
#define FEI_PROC_FAAT       				13
#define FEI_PROC_RECURSE    				14
#define FEI_PROC_SHORTSEQ   				15
#define FEI_PROC_TASKABLE   				16
#define FEI_PROC_MICROTASK  				17
#define FEI_PROC_BUFIN      				18
#define FEI_PROC_BUFOUT     				19
#define FEI_PROC_SCALAR     				20
#define FEI_PROC_VECTOR     				21
#define FEI_PROC_PARALLEL   				22
#define FEI_PROC_SERIAL     				23
#define FEI_PROC_TASKINNER  				24
#define FEI_PROC_NOTHRESHOLDS     			25
#define FEI_PROC_DEFINITION       			26
#define FEI_PROC_PARENT           			27
#define FEI_PROC_IMPORTED         			28
#define FEI_PROC_UNUSED_29        			29
#define FEI_PROC_UNUSED_30        			30
#define FEI_PROC_UNUSED_31        			31
#define FEI_PROC_PASARG     				32
#define FEI_PROC_LIMHEAP    				33
#define FEI_PROC_ENTRY      				34
#define FEI_PROC_HASRSLT    				35
#define FEI_PROC_SOFTEXT    				36
#define FEI_PROC_IMMORT     				37
#define FEI_PROC_DOFLOWTR   				38
#define FEI_PROC_INDEFSTK   				39
#define FEI_PROC_ALGNINST   				40
#define FEI_PROC_VARIADIC    				41
#define FEI_PROC_ZEROINC    				42
#define FEI_PROC_TOGGLE     				43
#define FEI_PROC_SHARELM    				44
#define FEI_PROC_LIMITLM    				45
#define FEI_PROC_TOLERANT   				46
#define FEI_PROC_STRROUND   				47
#define FEI_PROC_TRUNCATE   				48
#define FEI_PROC_FASTADDR   				49
#define FEI_PROC_NCOADDR    				50
#define FEI_PROC_SHAPECK    				51
#define FEI_PROC_MOD_FIRST  				52
#define FEI_PROC_MOD_JUST   				53
#define FEI_PROC_MOD_LAST   				54
#define FEI_PROC_PERM_NAME  				55
#define FEI_PROC_ARG_CHECK  				56
#define FEI_PROC_ZERO_INIT				57
#define FEI_PROC_ELEMENTAL				58
#define FEI_PROC_HAS_ALT_ENTRY				59
#define FEI_PROC_GLOBAL_INLINE				60
#define FEI_PROC_OPTIONAL_DIR				61
#define FEI_PROC_NOSIDE_EFFECTS				62
#define FEI_PROC_THISPTR       				63

#define FEI_OBJECT_DUPLICATE      	 	 	 0
#define FEI_OBJECT_DEFINED        	 	 	 1
#define FEI_OBJECT_TARGET         	 	 	 2
#define FEI_OBJECT_EQUIV          	 	 	 3
#define FEI_OBJECT_SAVED          	 	 	 4
#define FEI_OBJECT_ALIASED        	 	 	 5
#define FEI_OBJECT_REGISTER       	 	 	 6
#define FEI_OBJECT_ASSUMED_SIZE   	 	 	 7
#define FEI_OBJECT_AUXILIARY      	 	 	 8
#define FEI_OBJECT_INITIALD       	 	 	 9 
#define FEI_OBJECT_OFF_ASSIGNED  			10 
#define FEI_OBJECT_ALLOCATE      			11
#define FEI_OBJECT_AUTOMATIC     			12
#define FEI_OBJECT_ADDRTAKEN     			13
#define FEI_OBJECT_SYMCON_OFFST  			14
#define FEI_OBJECT_INNER_REF     			15
#define FEI_OBJECT_INNER_DEF     			16
#define FEI_OBJECT_WHERE_TEMP    			17
#define FEI_OBJECT_CACHE_ALIGN   			18
#define FEI_OBJECT_RESULT_TEMP   			19
#define FEI_OBJECT_DV_IS_PTR     			20
#define FEI_OBJECT_ASSUMD_SHAPE  			21
#define FEI_OBJECT_IS_DOPE_VEC   			22
#define FEI_OBJECT_DESC_DISTRIB  			23
#define FEI_OBJECT_PERMUTATION   			24
#define FEI_OBJECT_IS_CP_REF     			25
#define FEI_OBJECT_TASK_STORE    			26
#define FEI_OBJECT_SYMMETRIC     			27
#define FEI_OBJECT_ACTUAL_ARG    			28
#define FEI_OBJECT_PTR_ASSIGNED  			29
#define FEI_OBJECT_SF_DARG       			30
#define FEI_OBJECT_OPTIONAL      			31
#define FEI_OBJECT_NAMELIST_ITEM 			32
#define FEI_OBJECT_IN_COMMON     			33
#define FEI_OBJECT_NOT_PT_TO_UNIQUE_MEM			34
#define FEI_OBJECT_READ_ONLY				35
#ifdef KEY /* Bug 14150 */
#define FEI_OBJECT_PASS_BY_VALUE			36
#endif /* KEY Bug 14150 */

#define FEI_ARRAY_DIMEN_VARY_LB      	 	 	 0
#define FEI_ARRAY_DIMEN_VARY_EXT     	 	 	 1
#define FEI_ARRAY_DIMEN_SCON_LB      	 	 	 2
#define FEI_ARRAY_DIMEN_SCON_EXT     	 	 	 3
#define FEI_ARRAY_DIMEN_HOSTED_TYPE  	 	 	 4
#define FEI_ARRAY_DIMEN_ONTO_EXPR		 	 5
#define FEI_ARRAY_DIMEN_DIST_EXPR		 	 6
#define FEI_ARRAY_DIMEN_DIST_RESHAPE		 	 7
#define FEI_ARRAY_DIMEN_F_MINUS_MINUS_REMOTE		 8
#define FEI_ARRAY_DIMEN_VARY_UB                          9
#define FEI_ARRAY_DIMEN_SCON_UB                          10
#define FEI_ARRAY_DIMEN_FLOW_DEPENDENT			 11

#define FEI_DESCRIPTOR_CONST_C      	 	  	 0
#define FEI_DESCRIPTOR_SIGN_C       	 	 	 1
#define FEI_DESCRIPTOR_VOLAT_C      	 	 	 2
#define FEI_DESCRIPTOR_AUTO_F       	 	 	 3
#define FEI_DESCRIPTOR_RESTR_C      	 	 	 4
#define FEI_DESCRIPTOR_SHRD_PTEE   	 	 	 5
#define FEI_DESCRIPTOR_SHORT_C      	 	 	 7
#define FEI_DESCRIPTOR_LONG_C       	 	 	 8
#define FEI_DESCRIPTOR_BITFLD_C     	 	 	 9
#define FEI_DESCRIPTOR_HOSTED_TYPE 			10

#define FEI_LABEL_DEF_NAMED_IVDEP        		 0
#define FEI_LABEL_DEF_NAMED_NOREDUCE     		 1
#define FEI_LABEL_DEF_NAMED_NOVECTOR     		 2
#define FEI_LABEL_DEF_NAMED_SHORTLOOP    		 3
#define FEI_LABEL_DEF_NAMED_NOVSEARCH    		 4
#define FEI_LABEL_DEF_NAMED_SUPPRESS     		 5
#define FEI_LABEL_DEF_NAMED_LOOPCHK      		 6
#define FEI_LABEL_DEF_NAMED_DO_BL         		 7
#define FEI_LABEL_DEF_NAMED_NOSYNCBARR   		 8
#define FEI_LABEL_DEF_NAMED_EXTTABLES     		 9
#define FEI_LABEL_DEF_NAMED_EXTTIME       		10
#define FEI_LABEL_DEF_NAMED_CONCCALLS     		11
#define FEI_LABEL_DEF_NAMED_LPSPLITPOS    		12
#define FEI_LABEL_DEF_NAMED_SPLIT_CALL    		13
#define FEI_LABEL_DEF_NAMED_NOSAVELAST    		14
#define FEI_LABEL_DEF_NAMED_PERMUTATION   		15
#define FEI_LABEL_DEF_NAMED_MAXCPUS       		16
#define FEI_LABEL_DEF_NAMED_TASKSYNC      		17
#define FEI_LABEL_DEF_NAMED_EQUIVCHK      		18
#define FEI_LABEL_DEF_NAMED_JUMP_INTO     		19
#define FEI_LABEL_DEF_NAMED_NEXTSCALAR   		20
#define FEI_LABEL_DEF_NAMED_SHORTLOOP128 		21
#define FEI_LABEL_DEF_NAMED_SELECT_VEC    		22
#define FEI_LABEL_DEF_NAMED_SELECT_TASK   		23
#define FEI_LABEL_DEF_NAMED_128VL         		24
#define FEI_LABEL_DEF_NAMED_NOTASK        		25
#define FEI_LABEL_DEF_NAMED_INTERNAL      		26 
#define FEI_LABEL_DEF_NAMED_UNROLL        		27    
#define FEI_LABEL_DEF_NAMED_STREAMSPLIT   		28    
#define FEI_LABEL_DEF_NAMED_PATTERN       		29    
#define FEI_LABEL_DEF_NAMED_CASE   	 		30    
#define FEI_LABEL_DEF_NAMED_FISSIONABLE	 		31   
#define FEI_LABEL_DEF_NAMED_FUSABLE	 		32   
#define FEI_LABEL_DEF_NAMED_NOFISSION	 		33   
#define FEI_LABEL_DEF_NAMED_NOFUSION	 		34   
#define FEI_LABEL_DEF_NAMED_NOINTERCHANGE 		35   
#define FEI_LABEL_DEF_NAMED_NOBLOCKING    		36   
#define FEI_LABEL_DEF_NAMED_AGGRESSIVEINNERLOOPFISSION  37   
#define FEI_LABEL_DEF_NAMED_CONCURRENT                  38   
#define FEI_LABEL_DEF_NAMED_NOT_REFERENCED              39   
#define FEI_LABEL_DEF_NAMED_CONSTRUCTOR_LOOP            40
#define FEI_LABEL_DEF_NAMED_FUSION                      41
#define FEI_LABEL_DEF_NAMED_STREAM        		42
#define FEI_LABEL_DEF_NAMED_PREFERSTREAM       		43
#define FEI_LABEL_DEF_NAMED_INFORM_ONLY       		44
#define FEI_LABEL_DEF_NAMED_PSTREAM_NOCINV     		45

#define PDGCS_NEW_PROC_IGNORE_THROTTLE   	 	 0
#define PDGCS_NEW_PROC_ELIM_DEAD_CODE    	 	 1
#define PDGCS_NEW_PROC_PATTERN_MATCHING  	 	 2
#define PDGCS_NEW_PROC_TASK_INNER_LOOPS  	 	 3
#define PDGCS_NEW_PROC_IEEE_RECIPS       	 	 4
#define PDGCS_NEW_PROC_CONFORM_CHECK     	 	 5
#define PDGCS_NEW_PROC_IEEE_CONFORM      	 	 6
#define PDGCS_NEW_PROC_DO_UBD_ANALYSIS	 	 	 7

#define FEI_SEG_MODULE          	  	 	 0
#define FEI_SEG_SAVED           		 	 1
#define FEI_SEG_SCON_LEN        		 	 2
#define FEI_SEG_SHARED          		 	 3
#define FEI_SEG_INLINED         	 	 	 4
#define FEI_SEG_LOCAL_COMMON    		 	 5
#define FEI_SEG_EQUIVALENCED    		 	 6
/* 7 is available */
#define FEI_SEG_THREADPRIVATE			 	 8
#define FEI_SEG_DUPLICATE			 	 9
#define FEI_SEG_VOLATILE 			 	10

#define FEI_DOPE_VECTOR_HOSTED_TYPE		 	 0
#define FEI_DOPE_VECTOR_POINTER    		  	 1

#define FEI_NEXT_TYPE_IDX_HOSTED_TYPE		 	 0

#define PDGCS_INITIALIZE_INITS_DONE    		 	 0
#define PDGCS_INITIALIZE_FLOWTRACE     		 	 1
#define PDGCS_INITIALIZE_PART_DEBUG    		 	 2
#define PDGCS_INITIALIZE_LOCSTATICS    		 	 3
#define PDGCS_INITIALIZE_ATEXPERT      		 	 4
#define PDGCS_INITIALIZE_NOFASTMD      		 	 5
#define PDGCS_INITIALIZE_KERNEL        		 	 6
#define PDGCS_INITIALIZE_DBG_TABLES    		 	 7
#define PDGCS_INITIALIZE_OBJ_FILE      		 	 8
#define PDGCS_INITIALIZE_TRUNCATE      		 	 9
#define PDGCS_INITIALIZE_NO_OPT_MSGS  			10
#define PDGCS_INITIALIZE_MAX_VL64     			11
#define PDGCS_INITIALIZE_CALL_ARG_CK  			12
#define PDGCS_INITIALIZE_ENTRY_ARG_CK 			13
#define PDGCS_INITIALIZE_EMA_ADDRESS  			14
#define PDGCS_INITIALIZE_DOUBLE_ALIGN 			15
#define PDGCS_INITIALIZE_ENABLE_FEAT  			16 
#define PDGCS_INITIALIZE_MEM_HIER_OPT 			17
#define PDGCS_INITIALIZE_SMALL_PIC    			18  
#define PDGCS_INITIALIZE_BIG_PIC      			19  
#define PDGCS_INITIALIZE_NO_NEG_MSGS  			20
#define PDGCS_INITIALIZE_STREAMSPLIT  			21
#define PDGCS_INITIALIZE_PIPELINE     			22  
#define PDGCS_INITIALIZE_MP           			23  
#define PDGCS_INITIALIZE_APPRENTICE    			24
#define PDGCS_INITIALIZE_S2P_COERCE    			25
#define PDGCS_INITIALIZE_CCG_JUMPS     			26
#define PDGCS_INITIALIZE_OPT_DEBUG    		 	27
#define PDGCS_INITIALIZE_OPT_INFO    		 	28
#define PDGCS_INITIALIZE_INTEGER_1_2   		 	29

#define FEI_LABEL_ALGNINST      		 	 0
#define FEI_LABEL_REFINNER      		 	 1
#define FEI_LABEL_ALGNLOOP      		 	 2
#define FEI_LABEL_ASSIGN        		 	 3

#define FEI_STMT_ARY_BASE     		 	 	 0
#define FEI_STMT_ARY_SIZE     		 	 	 1
#define FEI_STMT_NO_BARRIER   		 	 	 2
#define FEI_STMT_ATOM_UPD     		 	 	 3
#define FEI_STMT_INTERNAL     		 	  	 4

#define FEI_CALL_DOES_NOT_RETURN			 0


typedef enum {
        Dbgclass_None,
        Dbgclass_Statement,
        Dbgclass_Exitprogram,
        Dbgclass_Endprologue,
        Dbgclass_Startepilogue,
        Dbgclass_F90construct,
        Dbgclass_F90loop,
        Dbgclass_User
} DBGCLASS;

typedef enum {
        Dbgtyp_Const_F,
        Dbgtyp_Volatile_F
} DTYPE_FLAGS_DBG;

typedef enum {
        Dbgtyp_Void = 1,
        Dbgtyp_Typeless,
        Dbgtyp_Bool,
        Dbgtyp_Int,
        Dbgtyp_Float,
        Dbgtyp_Complex,
        Dbgtyp_Fchar,
        Dbgtyp_Qual,
        Dbgtyp_Enum,
        Dbgtyp_Array,
        Dbgtyp_Struct,
        Dbgtyp_Addr,
        Dbgtyp_Dope,
        Dbgtyp_Vector,
        Dbgtyp_Func,
        Dbgtyp_Code
} DTYPE_CLASS_DBG;

typedef enum {
        Dbgobj_Struct_F
} DOBJ_FLAGS_DBG;

typedef enum {
        Dbgobj_Class_C = 1
} DOBJ_CLASS_DBG;

typedef enum {
        Dbgvis_Public = 1,
        Dbgvis_Private,
        Dbgvis_Protect,
        Dbgvis_Virtual
} DVISIBILITY_DBG;

typedef enum {
        Dbgobjm_Friend_F,
        Dbgobjm_Virtual_F,
        Dbgobjm_Pure_F,
        Dbgobjm_Class_F,
        Dbgobjm_Func_F
} DOBJMEM_FLAGS_DBG;

typedef enum C_kinds {
    CK_NONE=0,
    CK_CHAR=1,
    CK_SMALL=17,
    CK_MED=18,
    CK_LARGE=19,
    CK_XLARGE=20
} CKINDS;

typedef enum {
    PDGCS_Intent_Unseen,
    PDGCS_Intent_In,
    PDGCS_Intent_Out,
    PDGCS_Intent_Inout
} ARG_INTENT;

typedef enum {
    PDGCS_Unknown_Shape,
    PDGCS_Explicit_Shape,
    PDGCS_Assumed_Size,
    PDGCS_Assumed_Shape,
    PDGCS_Deferred_Shape
} ARRAY_CLASS;

typedef enum {
    PDGCS_WorkDist_NONE             =  0,
    PDGCS_WorkDist_Single           =  1,
    PDGCS_WorkDist_Vector           =  2,
    PDGCS_WorkDist_Guided           =  3,
    PDGCS_WorkDist_Num_Chunks       =  4,
    PDGCS_WorkDist_Chunk_Size       =  5,
    PDGCS_WorkDist_NCPUS_Chunks     =  7,

    PDGCS_WorkDist_MPP_Uniform      = 11,
    PDGCS_WorkDist_MPP_Chunk_Size   = 12,
    PDGCS_WorkDist_MPP_Num_Chunks   = 13,
    PDGCS_WorkDist_MPP_Guided       = 14,
    PDGCS_WorkDist_MPP_Align        = 15,
    PDGCS_WorkDist_MPP_Asyn         = 16,
    PDGCS_WorkDist_MPP_Reduc_Sum    = 17,
    PDGCS_WorkDist_MPP_Reduc_Prod   = 18,
    PDGCS_WorkDist_MPP_Reduc_Max    = 19,
    PDGCS_WorkDist_MPP_Reduc_Min    = 20,

    PDGCS_WorkDist_OMP_None         = 22,
    PDGCS_WorkDist_OMP_Runtime      = 23,
    PDGCS_WorkDist_OMP_Static       = 24,
    PDGCS_WorkDist_OMP_Dynamic      = 25,
    PDGCS_WorkDist_OMP_Guided       = 26
} WORK_DIST_TYPE;

typedef enum {	    
        Definition,
	Parent,
	Imported} PROC_CALLING_CLASS;

typedef enum {
	No_Dist,     
        Block_Dist,
	Cyclic_Dist,    
        Star_Dist } DISTRIBUTION;

typedef enum {
	Sym_Null,     
	Sym_Object,     
        Sym_Function,
        Sym_Unknown,
	Sym_Member,    
        Sym_Namelist } SYM_GROUP;

typedef enum {
        Unknown_Return,
        Normal_Return,
        Void_Return,
        Alternate_Return } RETURN_CLASS;

typedef enum {
	Unknown_Sym,
	User_Variable,
	Dummy_Arg,
	Function_Rslt,
	Compiler_Temp,
	CRI_Pointee,
	Component,
	Vector_Temp,
	Vector_Maskk,
	Vector_Iota,
	MPP_object,
	Call_Dummy,
	Dummy_Procedure,
	Hosted_User_Variable,
	Hosted_Dummy_Arg,
	Scratch_Symbol,
	Hosted_Dummy_Procedure,
        Hosted_Compiler_Temp,
        Name } OBJECT_SYM;

typedef enum {
	Context_None,
	Context_Private,
	Context_Shared,
	Context_Value,
	Context_Iterate,
	Context_Getfirst,
	Context_Control,
	Context_Induction,
	Context_PE_Res_Func,
	Context_PE_Res_Loop,
	Context_Lastlocal,
	Context_Affinity,
	Context_Nest,
        Context_Lastthread,
        Context_Omp_Reduction_Max,
        Context_Omp_Reduction_Min,
        Context_Omp_Reduction_Band,
        Context_Omp_Reduction_Bor,
        Context_Omp_Reduction_Bneqv,
        Context_Omp_Reduction_Plus,
        Context_Omp_Reduction_Mult,
        Context_Omp_Reduction_Minus,
        Context_Omp_Reduction_And,
        Context_Omp_Reduction_Or,
        Context_Omp_Reduction_Eqv,
        Context_Omp_Reduction_Neqv,
	Context_Omp_Private,
	Context_Omp_Shared,
	Context_Omp_Firstprivate,
	Context_Omp_Lastprivate,
	Context_Omp_Copyin,
	Context_Omp_Copyprivate, /* by jhs, 02/7/22 */
	Context_Omp_Affinity,
        Context_Omp_Nest } CONTEXT_TYPE;

typedef enum {
        Unknown_Table,
	Basic,
	Pointer,
	Array,
	Func_tion } TABLE_TYPE;

typedef enum {
        Unknown_Type,
	L_ogical,
	T_ypeless,
	V_oid,
	Char_Fortran,
	Char_C,
	S_tructure,
	U_nion,
	Integral,
	Floating_Pt,
	C_omplex,
	CRI_Pointer,
	CRI_Pointer_Char,
	BT_func_ptr,
	Vector_Mask } BASIC_TYPE;

typedef enum {
	Seq_Unknown,
	Seq_None,
	Seq_Numeric,
	Seq_Char,
	Seq_Mixed } FORT_SEQUENCE;

typedef enum {
        PDGCS_Proc_Unknown,
        PDGCS_Proc_Extern,
        PDGCS_Proc_Intern_Ref,
        PDGCS_Proc_Unused,
        PDGCS_Proc_Intern,
        PDGCS_Proc_Imported,
        PDGCS_Proc_Module,
        PDGCS_Proc_Host_Ref } PROC_CLASS;

typedef enum {
        Unknown_Func,
        C_Function,
        C_Function_Proto,
        Fort_Function,
        Fort_Subroutine,
        Main_Pgm,
        Fort_Blockdata,
        F90_Module } FUNCTION_SYM;

typedef enum {
        PDGCS_Lbl_Unknown,
        PDGCS_Lbl_User,
        PDGCS_Lbl_Format,
        PDGCS_Lbl_Internal,
        PDGCS_Lbl_Debug,
        PDGCS_Lbl_Cstrct } LABEL_SYM;

typedef enum {
	Seg_Unknown,
	Seg_Static,
	Seg_Stack,
	Seg_Formal,
	Seg_Common,
	Seg_Extern,
	Seg_Exported,
	Seg_Task_Common,
	Seg_Soft_External,
	Seg_Global_Breg,
	Seg_Global_Treg,
	Seg_Static_Name,
	Seg_Based,
	Seg_Equivalenced,
	Seg_Restricted,
	Seg_Distributed,
	Seg_LM_Static,
	Seg_LM_Common,
	Seg_LM_Extern,
	Seg_Auxiliary,
	Seg_Static_Local,
	Seg_Non_Local_Stack,
	Seg_Non_Local_Formal,
	Seg_Hosted_Stack,
	Seg_Shared_Stack,
	Seg_Threadprivate,
        Seg_Coment } SEGMENT_TYPE;

typedef enum {
        Error_Align  =  0,
	Bit_Align    =  1,
	Byte_Align   =  8,
	Parcel_Align = 16,
	HWord_Align  = 32,
	Word_Align   = 64,
	DWord_Align  = 65,
	FWord_Align  = 128 } ALIGN_TYPE;

typedef enum {
	No_Arg_Call,
	By_Value_Call,
	By_Addr_Call,
	By_Reg_Call,
	MPP_PAL_Call } CALL_TYPE;

typedef enum {
	No_Const,
	Arith_Const,
	Addr_Const,
	Pattern_Const,
	Array_Const,
	Struct_Const,
	Null_Arg_Const,
	Vector_Mask_Const } CONSTANT_CLASS;

typedef enum {
	Unknown_Lang,
	Ansi_C,
	Fortran_77,
	Fortran_90,
	Fortran_77_MPP,
	Fortran_90_MPP,
	Ansi_C_MPP,
	Other_Lang,
	C_pls_pls,
	C_pls_pls_MPP } LANG;


/********************\
|* TYPE DESCRIPTOR  *|
\********************/

typedef struct  type_descriptor  {

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)) || defined(_TARGET_MONGOOSE)
    unsigned     const_flag      :  1;
    unsigned     volatile_flag   :  1;
    unsigned     signed_flag     :  1;
    unsigned     automatic       :  1;
    unsigned     restricted      :  1;
    unsigned     short_flag      :  1;
    unsigned     long_flag       :  1;
    unsigned     bitfield        :  1;
    TABLE_TYPE   table_type      :  4;
    BASIC_TYPE   basic_type      :  5;
    unsigned     aux_info        : 15;
    unsigned     shrd_pointee    :  1;
    unsigned     table_index     : 31;
# else
    unsigned     const_flag      :  1;
    unsigned     volatile_flag   :  1;
    unsigned     signed_flag     :  1;
    unsigned     restricted      :  1;
    unsigned     short_flag      :  1;
    unsigned     long_flag       :  1;
    unsigned     bitfield        :  1;
    TABLE_TYPE   table_type      :  3;
    BASIC_TYPE   basic_type      :  4;
    int          aux_info        : 18;
    signed       table_index     : 32;
# endif

} TYPE;


/*
                     WARNING WARNING WARNING
If the size of TYPE is changed, a joint integration will be required.
Resizing, spliting or reordering bitfields may introduce unexpected
padding thereby changing overall structure size.
Be sure to verify that padding is not added when altering or adding
a component of TYPE.
*/



extern FILE  *init_debug_file             ( void );
extern char  *global_to_local_file        ( INT32 );
extern INT32  global_to_local_line_number ( INT32 );
extern void   PDGCS_initialize       	  ( LANG  language,
                                     	    INT32 init_flags, 
                                     	    char  *cmp_name,
                                     	    char  *cmp_rel,
                                     	    char  *obj_file_name,
                                     	    char  *list_file_name,
                                     	    INT32 trunc_bits,
                                     	    INT32 debug_opts,
                                     	    char  *src_path_name,
                                     	    char  *cif_file_name,
                                    	    char  *debug_file_name,  
                                     	    FILE  *debug_file, 
                                     	    FILE  *cif_file, 
                                    	    char  *src_fname,
                                     	    char  *cwd, 
                                     	    INT32 n_pes, 
                                     	    INT32 meta_test );
extern void  PDGCS_comp_unit        	  ( char  *comp_unit_name,
                                     	    INT32 module_node );
extern void  PDGCS_new_proc         	  ( INT32 ir_count,
                                     	    INTPTR func_st_idx,
                                     	    INT32 alt_entry_count,
                                     	    INT32 scalar_opt_level,
                                     	    INT32 vector_opt_level,
                                     	    INT32 task_opt_level,
                                     	    INT32 opt_flags,
                                     	    INT32 user_mobes,
                                    	    INT32 user_sades,
                                     	    INT32 pipeline_opt_level,   
                                     	    INT32 stream_opt_level );
#ifdef KEY /* Bug 3507 */
extern void  PDGCS_do_proc          	  ( int is_module);
#else /* KEY Bug 3507 */
extern void  PDGCS_do_proc          	  ( void );
#endif /* KEY Bug 3507 */
extern void  PDGCS_end_procs        	  ( INT32 *code_size,
                                    	    INT32 *data_size );
extern void  PDGCS_end_comp_unit    	  ( void );
extern void  PDGCS_terminate        	  ( void );
extern void  PDGCS_debug_init       	  ( char  *org_file_name,
                                     	    char  *comp_gen_date );
extern void  PDGCS_version_info     	  ( char  *cmp_rel, char  *cmp_ed );
extern void  fei_module_file        	  ( char  *file_name );
extern void  PDGCS_debug_gen_table        ( void );
extern void  fei_smt_actual_to_generic    ( INTPTR act_st_idx,
                                            char  *generic_name );
extern void  fei_smt_original_to_qualified( INTPTR qualified_st_idx,
                                            char  *original_name );
extern void  fei_null_expr                ( void );
extern void  fei_member_ref               ( INTPTR mem_idx );
extern void  fei_object_ref               ( INTPTR sym_idx,    
                                            INT32 whole_subscript,
                                            INT32 whole_substring);
extern void  fei_function_ref             ( INTPTR fnc_idx );
extern INTPTR fei_constant                 ( TYPE  type,
                                            INT32 Class,
                                            char  *start,
                                            INT64 bitsize );
extern INTPTR fei_arith_con               ( TYPE  type, 
                                            long  *start );
extern void  fei_push_arith_con           ( INTPTR cdx );
extern void  fei_push_pattern_con         ( INTPTR ndx );
extern void  fei_add_use_path             ( INTPTR st_idx,
                                            INT32 path_idx,
                                            INT32 module_idx );
extern INTPTR fei_pattern_con             ( TYPE  type,
                                            char  *start,
                                            INT64 bitsize );
extern INTPTR fei_smt_parameter           ( char  *name_string,
                                            TYPE  type,
                                            INTPTR con_idx,
                                            INT32 Class,
                                 	    INT32 line_num );
extern void  fei_entry_pt                 ( INTPTR st_idx );
extern void  fei_stmt                     ( INT32 lineno, INT32 flags );
extern void  fei_min                      ( INT32 n_opnds, TYPE type );
extern void  fei_max                      ( INT32 n_opnds, TYPE type );
extern void  fei_list                     ( INT32 n_opnds );
extern void  fei_concat                   ( INT32 n_opnds );
extern void  fei_index                    ( void );
extern void  fei_field_dot                ( TYPE  type );
extern void  fei_field_arrow              ( TYPE  type );
extern void  fei_deref                    ( TYPE  type );
extern void  fei_paren                    ( TYPE  type );
extern void  fei_cvtop                    ( TYPE  type );
extern void  fei_substr                   ( INT32 bound_chk );
extern void  fei_seq_subscr               ( TYPE  type );
extern void  fei_nseq_subscr              ( TYPE  type );
extern void  fei_subscr_size              ( TYPE  type, INT32 bounds_check );
extern void  fei_subscr_triplet           ( TYPE  type );
extern void  fei_dv_deref                 ( TYPE  type );
#ifdef KEY /* Bug 4602 */
extern void  fei_array_element_by_value	  ( void );
#endif /* KEY Bug 4602 */
extern void  fei_store                    ( TYPE  type );
extern void  fei_non_conform_store        ( TYPE  type );
extern void  fei_as_ref                   ( TYPE  type );
extern void  fei_return                   ( INT32 return_class, TYPE type );
extern void  fei_stop			  ( void );
extern void  fei_case                     ( INT32 case_follows );
extern void  fei_switch                   ( INT32 num_cases, 
 					    INTPTR default_lbl_idx );
extern void  fei_static_begin             ( void );
extern void  fei_static_init              ( INTPTR st_idx,
                                            INT64 offset,
                                            INT64 size,
                                            INT64 dups,
                                            INT64 stride,
                                            INT32 string_literal );
extern void  fei_static_reloc_init        ( INTPTR st_idx,
                                 	    INT64 offset,
                                 	    INT64 size,
                                 	    INT64 dups,
                                 	    INT64 stride,
                                 	    INT64 relocbitoffset );
extern void  fei_static_base              ( INTPTR st_idx );
extern void  fei_static_subscripts        ( INT64 subscripts[] );
extern void  fei_static_member            ( INTPTR st_idx );
extern void  fei_static_substr            ( INT32 start );
extern void  fei_static_simple_init       ( INT64 dup_count,
                                 	    INT64 stride,
                                 	    INT32 ignore_types,
                                 	    INT32 string_literal );
extern void  fei_static_next_simple_init  ( INT64 bit_increment, 
                                     	    INT64 dup_count,
                                     	    INT64 init_offset,
                                            INT32 string_literal );
extern void  fei_static_simple_reloc_init ( INT64 bit_offset, 
                                     	    INT64 dup_count,
                                            INT64 stride,
                                            INT64 bit_size,
                                            INT32 ignore_types );
extern void  fei_static_next_reloc_init   ( INT64 bit_offset, 
                                     	    INT64 bit_stride,
                                     	    INT64 dup_count,
                                     	    INT64 init_offset,
                                     	    INT64 bit_size );
extern void  fei_static_end               ( void );
extern void  fei_initializer              ( INT64 dup_count,
                                 	    INT64 stride,
                                 	    INT64 stride_precomputed );
extern void  fei_init_reloc               ( INT64 bit_offset,
                                            INT64 dup_count,
                                            INT64 stride );
extern void  fei_label_ref                ( INTPTR lbl_idx );
extern void  fei_label_def_named          ( INTPTR lbl_idx,
                                     	    INT64 label_flag_word,
                                     	    INT32 lineno,
                                     	    INT32 sup_cnt,
                                     	    INT32 keepme,
                                     	    INT32 storage_seg,
                                     	    INT32 safevl,
                                     	    INT32 unroll_cnt,
				     	    char  *mark_name,
                                     	    INT32 noncache_cnt,
                                     	    INT32 safedist,
                                            INT32 blockable_grp,
                                            INT32 interchange_grp,
                                            INT32 interchange_lvl);
extern void  fei_call                     ( INT32 num_args,
                                     	    TYPE  type,
                                 	    INT32 call_type,
                                 	    INT32 alt_return_flag,  
                                 	    INT32 inline_setting,
                                            INT64 flags );
extern void  fei_brtrue                	  ( INTPTR lbl_idx );
extern void  fei_if			  ( void );
extern void  fei_else			  ( void );
extern void  fei_endif			  ( void );
extern void  fei_goto                  	  ( INTPTR lbl_idx );
extern void  fei_label_addr            	  ( INTPTR idx );
extern void  fei_indirect_goto            ( INT32 num_labs,
                                            INT32 assign_goto_flag );
extern void  fei_arith_goto               ( INTPTR zero_lab,
                                            INTPTR pos_lab,
                                            INTPTR neg_lab );
extern void  fei_proc_body                ( INT32 lineno );
extern INT32 fei_get_segment              ( INTPTR pdgcs_st_idx,
                                            INT32 *is_data_segment );
extern INT32 fei_get_dist_info            ( INT32 seg_idx,
                                            INT32 dimension,
                                            INT32 *pe_type,
                                            INT32 *pe_val,
                                            INT32 *blk_type,
                                            INT32 *blk_val,
                                            INT32 *cycle_type,
                                            INT32 *cycle_val );
extern INT32 fei_get_redist               ( INT32 arg_syx );
extern void  fei_user_type                ( char  *name_string,
                                            INT32 nbr_components,
                                            INT32 first_idx,
                                            INT64 size,
                                            INT32 sequence,
                                            INTPTR st_idx,
                                            INT32 alignment );
extern TYPE  fei_descriptor               ( INT32 flag_matrix,
                                            INT32 table_type,
                                            INTPTR table_index,
                                            INT32 basic_type,
                                            INT32 aux_info,
                                            INT32 alignment);
#ifdef KEY /* Bug 14110 */
extern unsigned fei_set_volatile(unsigned);
#endif /* KEY Bug 14110 */
extern INTPTR fei_name                    ( char  *name_string,
                                            INT32 st_grp,
                                            INTPTR st_idx,
                                            INT32 prev,
                                            INT32 idx );
extern INT32 fei_next_name                ( INT32 increment );
extern INT32 fei_next_type_idx            ( INT32 flag,   
                                            INT32 alignment );
extern INTPTR fei_next_func_idx           ( INT32 pgm_unit,
                                            INT32 proc,
                                            INT32 alt_entry );
extern INT32 fei_next_symbol              ( INT32 increment );
extern void  fei_doacross	      	  ( INTPTR task_if_idx,
                  	          	    INT32 sched_type,
                   		  	    INT32 thread_count,
                   		  	    INT32 data_count,
                   		  	    INT32 onto_count,
                   		  	    INT32 reduction_count,
                   		  	    INT32 chunk_count );
extern void  fei_pdo		      	  ( INT32 sched_type,
                   		  	    INT32 ordered,
                   		  	    INT32 thread_count,
                   		  	    INT32 data_count,
                   		  	    INT32 onto_count,
                   		  	    INT32 reduction_count,
                   		  	    INT32 chunk_count );
extern void  fei_paralleldo	      	  ( INTPTR task_if_idx,
                  	          	    INT32 sched_type,
                   		  	    INT32 thread_count,
                   		  	    INT32 data_count,
                   		  	    INT32 onto_count,
                   		  	    INT32 reduction_count,
                   		  	    INT32 chunk_count );
extern void  fei_parallel	          ( INTPTR task_if_idx );
extern void  fei_singleprocess	          ( void );
extern void  fei_criticalsection          ( INT32 var_count );
extern void  fei_endsingleprocess         ( INT32 nowait );
extern void  fei_endpsection              ( INT32 nowait );
extern void  fei_endpdo                   ( INT32 nowait );
extern void  fei_endcriticalsection       ( void );
extern void  fei_endparallel              ( void );
extern void  fei_section                  ( void );
extern void  fei_barrier                  ( void );
extern void  fei_psection	          ( void );
extern void  fei_regionend	          ( void );
extern void  fei_regionbegin	          ( void );
extern void  fei_interchange	          ( INT32 expressions );
extern void  fei_blockable	          ( INT32 expressions );
extern void  fei_fuse                     ( INT32 level );
extern void  fei_flush                    ( INT32 list_count );
extern void  fei_assert                   ( INT32 assertion, INT32 list_count );
extern void  fei_fission                  ( void );
#ifdef KEY
extern void  fei_forall                   ( void );
#endif
extern void  fei_unroll                   ( void );
extern void  fei_section_gp               ( INT32 list_count );
extern void  fei_section_nongp            ( INT32 list_count );
extern void  fei_blocking_size            ( void );
extern void  fei_opaque                   ( void );
extern void  fei_copy_in                  ( INT32 list_count );
extern void  fei_concurrentize            ( INT32 state );
extern void  fei_seg_ref                  ( INTPTR sb_idx );
extern void  fei_page_place               ( void );
extern void  fei_dynamic                  ( INT32 list_count );
extern void  fei_fill_symbol              ( INT32 list_count, INT32 C_value );
extern void  fei_align_symbol             ( INT32 list_count, INT32 C_value );
extern void  fei_prefetch_ref_disable     ( INTPTR array, INT32 size );
extern void  fei_prefetch_ref	          ( INT32 stride,
                              	            INT32 level,
                               		    INT32 kind,
                               		    INT32 size );
extern void  fei_redistribute	      	  ( INTPTR array,
                               		    INT32 dim,
                               		    INT32 distribution,
                               		    INT32 cyclic_exists,
                               		    INT32 onto_exists );
#ifdef KEY /* Bug 2660 */
extern void  fei_options       	  	  ( char * n1);
#endif /* KEY Bug 2660 */
extern void  fei_prefetch       	  ( INT32 n1, INT32 n2 );
extern void  fei_prefetch_manual	  ( INT32 n );
#ifdef KEY /* Bug 3507 */
extern void cwh_dst_enter_module          ( char *module_name,
					    char *filename,
                                            INT32 local_lineno );
extern void cwh_dst_exit_module           ( void );
#endif /* KEY Bug 3507 */
extern INTPTR fei_proc            	  ( char  *name_string,
                                 	    INT32 lineno,
                                 	    INT32 sym_class,
                                 	    INT32 Class,
                                 	    INT32 num_dum_args,
                                 	    INT32 parent_stx,
                                 	    INT32 first_st_idx,
                                 	    INT32 aux_idx,
                                 	    TYPE  type,
                                 	    INT32 st_idx,   
                                 	    INT64 flags );
extern INTPTR fei_seg             	  ( char  *name_string,
                                 	    INT32 seg_type,
                                 	    INT32 owner,
                                 	    INT32 parent,
                                 	    INT32 aux_index,
                                 	    INT32 flag_bits,
                                 	    INT32 nest_level,
                                 	    INT64 block_length );
extern INT32 fei_label           	  ( char  *name_string,
                                 	    INT32 flag_bits,
                                 	    INT32 Class,
                                 	    char  *format_string, 
                                 	    INT32 debug_label_type );
extern INT32 fei_member          	  ( char  *name_string,
                                 	    TYPE  type,
                                 	    INT64 offset,
                                 	    INT64 size,
                                 	    INT32 alignment,
                                 	    INT32 line_num, 
				 	    INT64 flag_bits,
                                 	    INT64 io_code );
extern INTPTR fei_object          	  ( char  *name_string,
                                 	    TYPE  type,
                                 	    INT64 flag_bits,
                                 	    INT32 sym_class,
                                 	    INTPTR storage_idx,
                                 	    INT32 derive_st_idx,
                                 	    INTPTR ptr_st_idx,
                                 	    INT64 offset,
                                 	    INT32 arg_intent,
                                 	    INT64 size,
                                 	    INT32 type_aux,
                                 	    INT32 alignment,
                                 	    INT32 distr_idx,
                                 	    INT32 node_1,
                                 	    INT32 node_2,
                                 	    INT32 line_num );
extern INTPTR fei_array_dimen     	  ( INT32 flag_bits,
                                 	    INT64 low_bound,
                                 	    INT64 extent,
                                 	    INT32 rank,
                                 	    TYPE  span_type,
                                 	    INT64 bitsize,
                                 	    INT32 distribution,
                                            INT64 upper_bound );
extern INT32 fei_pointee         	  ( TYPE  pointee_type );
extern INT32 fei_parallel_region 	  ( INTPTR ifexpr,
                                 	    INTPTR maxcpus,
                                 	    INT32 context_start,
                                 	    INT32 context_end,
                                 	    INT32 lineno,
                                 	    INT32 flags );
extern void  fei_endparallel_region       ( INT32 task_x, INT32 lineno );
extern void  fei_doall          	  ( INTPTR ifexpr,
                                 	    INTPTR maxcpus,
                                 	    INT32 context_start,
                                 	    INT32 context_end,
                                 	    INT32 induc_symx,
                                 	    INT32 work_dist,
                                 	    INTPTR work_dist_opnd,
                                 	    INT32 flags,
                                 	    INT32 lineno );
extern INT32 fei_doparallel      	  ( INT32 induc_symx,
                                 	    INT32 work_dist,
                                 	    INTPTR work_dist_opnd,
                                 	    INT32 lineno );
extern void  fei_task_endloop   	  ( INT32 taskloop_x, 
                                            INT32 lineno,
                                            INT32 nowait );
extern INT32 fei_doall_wave      	  ( INT32 ifexpr,
                                 	    INT32 maxcpus,
                                 	    INT32 context_start,
                                 	    INT32 context_end,
                                 	    INT32 iter_start,
                                 	    INT32 iter_end,
                                 	    INT32 lineno );
extern INT32 fei_doparallel_wave 	  ( INT32 iter_start,
                                 	    INT32 iter_end,
                                 	    INT32 lineno );
extern void  fei_par_body       	  ( INT32 taskloop_x, INT32 lineno );
extern void  fei_endpar_body    	  ( INT32 taskloop_x, INT32 lineno );
extern INT32 fei_par_case        	  ( INT32 task_x, INT32 lineno );
extern void  fei_par_endcase    	  ( INT32 task_x, INT32 lineno );
extern void  fei_numcpus        	  ( INT32 lineno );
extern INT32 fei_guard           	  ( INT32 guard_num, INT32 lineno );
extern void  fei_endguard       	  ( INT32 task_x,
                                 	    INT32 guard_num,
                                 	    INT32 lineno );
extern INTPTR fei_task_var        	  ( INTPTR sym_idx, INT32 context );
extern INT32 fei_task_wait       	  ( INT32 point, INT32 span );
extern INT32 fei_task_send       	  ( INT32 point, INT32 task_if_idx );
extern INT32 fei_mpp_master      	  ( INT32 lineno );
extern void  fei_mpp_symmetric_alloc      ( void );
extern void  fei_mpp_symmetric_free       ( void );
extern INT32 fei_mpp_parallel    	  ( INT32 all_flg,
                                   	    INT32 first_ctxt_x,
                                   	    INT32 last_ctxt_x,
                                   	    INT32 lineno );
extern void  fei_mpp_endmaster  	  ( INT32 task_x,
                                   	    INT32 first_ctxt_x,
                                   	    INT32 last_ctxt_x,
                                   	    INT32 lineno );
extern void  fei_mpp_endparallel 	  ( INT32 task_x, INT32 lineno );
extern void  fei_mpp_doshared   	  ( INT32 first_induc_x,
                                  	    INT32 last_induc_x,
                           		    INT32 work_dist,
                                  	    INT32 random_flg,
                                  	    INT32 nobarrier_flg,
                                  	    INT32 lineno );
extern void  fei_mpp_barrier    	  ( INT32 lineno );
extern INT32 fei_mpp_critical    	  ( INT32 lineno );
extern void  fei_mpp_endcritical	  ( INT32 task_x, INT32 lineno );
extern INT32 fei_mpp_distrib_dim 	  ( INT32 prev_distrib_x,
                                 	    INT32 distrib_type,
                                 	    INT64 blk_sz,
                                 	    INT32 pe_ratio,
                                 	    INT32 orig_ext,
                                 	    INT32 flags );
extern void  cwh_add_to_used_files_table  ( char  *name, INT32  copy_name );
extern void  fei_critical_open_mp         ( char  *name );
extern void  fei_endcritical_open_mp      ( char  *name );
extern void  fei_parallelsections_open_mp ( INTPTR task_if_idx, INTPTR task_num_threads_idx, INT32 defaultt );
extern void  fei_paralleldo_open_mp       ( INTPTR task_if_idx,
                                            INTPTR task_num_threads_idx,
                                 	    INT32 defaultt,
                                  	    INT32 ordered,
                                  	    INT32 scheduletype,
                                  	    INTPTR schedulechunck,
                                            INT32 threadcount,
                                            INT32 datacount,
                                            INT32 ontocount );
extern void  fei_parallelworkshare_open_mp( INTPTR task_if_idx,
                                            INTPTR task_num_threads_idx,
                                            INT32 defaultt );
extern void  fei_workshare_open_mp        ( void );
extern void  fei_endworkshare_open_mp     ( INT32 nowait);
extern void  fei_endparallelworkshare_open_mp( void );

extern void  fei_single_open_mp           ( void );
extern void  fei_sections_open_mp         ( void );
extern void  fei_do_open_mp               ( INT32 ordered,
                                  	    INT32 scheduletype,
                                  	    INTPTR schedulechunck,
                                            INT32 threadcount,
                                            INT32 datacount,
                                            INT32 ontocount );
extern void  fei_parallel_open_mp         ( INTPTR task_if_idx, INTPTR task_num_threads_idx, INT32 defaultt );
extern void  fei_barrier_open_mp          ( void );
extern void  fei_section_open_mp          ( void );
extern void  fei_master_open_mp           ( void );
extern void  fei_endmaster_open_mp        ( void );
extern void  fei_ordered_open_mp          ( void );
extern void  fei_endsingle_open_mp        ( INT32 nowait );
extern void  fei_enddo_open_mp            ( INT32 nowait );
extern void  fei_endsections_open_mp      ( INT32 nowait );
extern void  fei_flush_open_mp            ( INT32 list_count );
extern void  fei_endordered_open_mp       ( void );
extern void  fei_endparalleldo_open_mp    ( void );
extern void  fei_endparallel_open_mp      ( void );
extern void  fei_atomic_open_mp           ( void );
extern void  fei_endparallelsections_open_mp( void );
extern void  fei_fetch_and_add            ( TYPE type );
extern void  fei_fetch_and_sub            ( TYPE type );
extern void  fei_fetch_and_xor            ( TYPE type );
extern void  fei_fetch_and_nand           ( TYPE type );
extern void  fei_fetch_and_or             ( TYPE type );
extern void  fei_fetch_and_and            ( TYPE type );
extern void  fei_add_and_fetch            ( TYPE type );
extern void  fei_sub_and_fetch            ( TYPE type );
extern void  fei_xor_and_fetch            ( TYPE type );
extern void  fei_nand_and_fetch           ( TYPE type );
extern void  fei_or_and_fetch             ( TYPE type );
extern void  fei_and_and_fetch            ( TYPE type );
extern void  fei_lock_test_and_set        ( TYPE type );
extern void  fei_compare_and_swap         ( TYPE type );
extern void  fei_lock_release             ( void );
extern void  fei_omp_set_lock             ( void );
extern void  fei_omp_unset_lock           ( void );
extern void  fei_omp_test_lock            ( void );
extern void  fei_synchronize              ( void );
extern void  fei_shloc                    ( void );
extern void  fei_shmalloc                 ( INT32 nargs );
extern void  fei_shfree                   ( void );
extern void  fei_user_code_start	  ( void );
extern void  fei_start_ioblock		  ( void );
extern void  fei_end_ioblock		  ( void );
extern void  fei_namelist_ref             ( INTPTR sym_idx );
extern void  fei_formatted_read           ( void );
extern void  fei_unformatted_read         ( void );
extern void  fei_namelist_read            ( void );
extern void  fei_formatted_write          ( void );
extern void  fei_unformatted_write        ( void );
extern void  fei_namelist_write           ( void );
extern void  fei_control_list             ( INT32 io_type );
extern void  fei_IO_list                  ( INT32 n_opnds, INT32 io_type );
extern void  fei_implied_do               ( void );
extern void  fei_iolength                 ( void );
extern void  fei_bit_len                  ( void );
extern void  fei_where                    ( INT32 defined_asg, 
                                            INT32 inline_state );
extern void  fei_ceiling                  ( TYPE type );
extern void  fei_exponent                 ( TYPE type );
extern void  fei_floor                    ( TYPE type );
extern void  fei_getpos                   ( void );
extern void  fei_length                   ( void );
extern void  fei_nearest                  ( void );
extern void  fei_near                     ( TYPE type);
extern void  fei_present                  ( void );
extern void  fei_rrspacing                ( void );
extern void  fei_spacing                  ( void );
extern void  fei_unit                     ( void );
extern void  fei_ridiv                    ( void );
extern void  fei_addr_con                 ( TYPE type );
extern void  fei_ex                       ( INT32 nargs );
extern void  fei_chanDI                   ( void );
extern void  fei_chanEI                   ( void );
extern void  fei_push_npex_expr           ( INT32 npdx );
extern void  fei_my_pe                    ( void );
extern void  fei_n_pes                    ( void );
extern void  fei_symmetric_alloc          ( void );
extern void  fei_symmetric_free           ( void );
extern void  fei_blkct            	  ( void );
extern void  fei_lowidx           	  ( void );
extern void  fei_hiidx            	  ( void );
extern void  fei_home             	  ( void );
extern void  fei_pes              	  ( void );
extern void  fei_geommap          	  ( void );
extern void  fei_taskmap          	  ( void );
extern void  fei_isshared         	  ( void );
extern void  fei_iscanon          	  ( void );
extern void  fei_inpar            	  ( void );
extern void  fei_indoshrd         	  ( void );
extern void  fei_mpp_sdd_read_base        ( void );
extern void  fei_mpp_sdd_write_base       ( void );
extern void  fei_mpp_sdd_read_offset      ( void );
extern void  fei_mpp_sdd_write_offset     ( void );
extern void  fei_mpp_sdd_read_canon       ( void );
extern void  fei_mpp_sdd_write_canon      ( void );
extern void  fei_mpp_sdd_read_blk_ebp     ( void );
extern void  fei_mpp_sdd_write_blk_ebp    ( void );
extern void  fei_mpp_sdd_read_pe_bcnt     ( void );
extern void  fei_mpp_sdd_write_pe_bcnt    ( void );
extern void  fei_mpp_sdd_read_cyc_ebp     ( void );
extern void  fei_mpp_sdd_write_cyc_ebp    ( void );
extern void  fei_cached_read      	  ( void );
extern void  fei_atomic_swap      	  ( TYPE type );
extern void  fei_set_cache_inv    	  ( void );
extern void  fei_clr_cache_inv    	  ( void );
extern void  fei_partition_type   	  ( void );
extern void  fei_get_bsr0         	  ( void );
extern void  fei_put_bsr0         	  ( void );
extern void  fei_loc_cmr          	  ( void );
extern void  fei_rem_cmr          	  ( void );
extern void  fei_wmb              	  ( void );
extern void  fei_read_unkn         	  ( void );
extern void  fei_write_unkn       	  ( void );
extern void  fei_read_fpcr        	  ( void );
extern void  fei_write_fpcr       	  ( void );
extern void  fei_len		 	  ( TYPE type );
extern void  fei_fraction	 	  ( void );
extern void  fei_len_trim        	  ( void );
extern void  fei_trunc           	  ( TYPE type );
extern void  fei_round           	  ( TYPE type );
extern void  fei_pos_diff        	  ( TYPE type );
extern void  fei_sign_xfer       	  ( TYPE type );
extern void  fei_ieee_sign_xfer       	  ( TYPE type );
extern void  fei_rtc             	  ( TYPE type );
extern void  fei_copyin          	  ( void );
extern void  fei_copyout         	  ( void );
extern void  fei_conjg           	  ( TYPE type );
extern void  fei_cot             	  ( TYPE type );
extern void  fei_atan2           	  ( TYPE type );
extern void  fei_atan2d           	  ( TYPE type );
extern void  fei_coss            	  ( void );
extern void  fei_ranf            	  ( TYPE type );
extern void  fei_wclrsm          	  ( void );
extern void  fei_getvl           	  ( void );
extern void  fei_setvl           	  ( void );
extern void  fei_setcln          	  ( void );
extern void  fei_rjump           	  ( void );
extern void  fei_setca           	  ( void );
extern void  fei_pci             	  ( void );
extern void  fei_cci             	  ( void );
extern void  fei_eci             	  ( void );
extern void  fei_dci             	  ( void );
extern void  fei_eciv            	  ( void );
extern void  fei_dciv            	  ( void );
extern void  fei_multiply_high   	  ( TYPE type );
extern void  fei_mc              	  ( void );
extern void  fei_di              	  ( void );
extern void  fei_ei              	  ( void );
extern void  fei_emi             	  ( void );
extern void  fei_dmi             	  ( void );
extern void  fei_setbp           	  ( void );
extern void  fei_esi             	  ( void );
extern void  fei_clrci           	  ( void );
extern void  fei_loadrt          	  ( void );
extern void  fei_readca          	  ( void );
extern void  fei_readce          	  ( void );
extern void  fei_readci          	  ( void );
extern void  fei_setcl           	  ( void );
extern void  fei_setip           	  ( void );
extern void  fei_clearip         	  ( void );
extern void  fei_setxa           	  ( void );
extern void  fei_slm             	  ( void );
extern void  fei_rlm             	  ( void );
extern void  fei_wlm             	  ( void );
extern void  fei_err             	  ( void );
extern void  fei_writebp         	  ( void );
extern void  fei_cmr             	  ( void );
extern void  fei_tsetsm          	  ( void );
extern void  fei_clrsm           	  ( void );
extern void  fei_setsm           	  ( void );
extern void  fei_getvm           	  ( void );
extern void  fei_setvm           	  ( void );
extern void  fei_jts             	  ( void );
extern void  fei_readb           	  ( void );
extern void  fei_writeb          	  ( void );
extern void  fei_readsb          	  ( void );
extern void  fei_readsbi         	  ( void );
extern void  fei_writesb         	  ( void );
extern void  fei_readst          	  ( void );
extern void  fei_writest         	  ( void );
extern void  fei_readsr          	  ( void );
extern void  fei_writesr         	  ( void );
extern void  fei_fincsr          	  ( void );
extern void  fei_getsem          	  ( void );
extern void  fei_numwords        	  ( void );
extern void  fei_argchk          	  ( void );
extern void  fei_argloc          	  ( void );
extern void  fei_readsm          	  ( void );
extern void  fei_writesm         	  ( void );
extern void  fei_mbclr           	  ( void );
extern void  fei_mbld            	  ( void );
extern void  fei_mbmx            	  ( void );
extern void  fei_mbmxl           	  ( void );
extern void  fei_mbul            	  ( void );
extern void  fei_abs             	  ( TYPE type );
extern void  fei_acos            	  ( TYPE type );
extern void  fei_acosd           	  ( TYPE type );
extern void  fei_asin            	  ( TYPE type );
extern void  fei_asind           	  ( TYPE type );
extern void  fei_atan            	  ( TYPE type );
extern void  fei_atand           	  ( TYPE type );
extern void  fei_cos             	  ( TYPE type );
extern void  fei_cosd            	  ( TYPE type );
extern void  fei_cosh            	  ( TYPE type );
extern void  fei_exp             	  ( TYPE type );
extern void  fei_log             	  ( TYPE type );
extern void  fei_log10           	  ( TYPE type );
extern void  fei_sin             	  ( TYPE type );
extern void  fei_erf             	  ( TYPE type, int complement );
extern void  fei_sind            	  ( TYPE type );
extern void  fei_sinh            	  ( TYPE type );
extern void  fei_sqrt            	  ( TYPE type );
extern void  fei_tan             	  ( TYPE type );
extern void  fei_tand            	  ( TYPE type );
extern void  fei_tanh            	  ( TYPE type );
extern void  fei_mask            	  ( TYPE type );
extern void  fei_readxa          	  ( void );
extern void  fei_readea          	  ( void );
extern void  fei_setea           	  ( void );
extern void  fei_esc             	  ( void );
extern void  fei_dsc             	  ( void );
extern void  fei_alloc           	  ( void );
extern void  fei_free            	  ( void );
extern void  fei_allocate        	  ( INT32 nargs );
extern void  fei_deallocate      	  ( INT32 nargs );
extern void  fei_ssd_alloc       	  ( void );
extern void  fei_ssd_free        	  ( void );
extern void  fei_readbpc         	  ( void );
extern void  fei_arg_addr        	  ( TYPE type );
extern void  fei_addr            	  ( TYPE type );
extern void  fei_real            	  ( TYPE type );
extern void  fei_imag            	  ( TYPE type );
extern void  fei_bneg            	  ( TYPE type );
extern void  fei_lneg            	  ( TYPE type );
extern void  fei_uminus          	  ( TYPE type );
extern void  fei_mult            	  ( TYPE type );
extern void  fei_plus            	  ( TYPE type );
extern void  fei_minus           	  ( TYPE type );
extern void  fei_div             	  ( TYPE type );
extern void  fei_mod             	  ( TYPE type );
extern void  fei_lt              	  ( TYPE type );
extern void  fei_eqv             	  ( TYPE type );
extern void  fei_leqv            	  ( TYPE type );
extern void  fei_eq              	  ( TYPE type );
extern void  fei_gt              	  ( TYPE type );
extern void  fei_or              	  ( TYPE type );
extern void  fei_lor             	  ( TYPE type );
extern void  fei_ge              	  ( TYPE type );
extern void  fei_ne              	  ( TYPE type );
extern void  fei_le              	  ( TYPE type );
extern void  fei_xor             	  ( TYPE type );
extern void  fei_lxor            	  ( TYPE type );
extern void  fei_and             	  ( TYPE type );
extern void  fei_land            	  ( TYPE type );
extern void  fei_lshift          	  ( TYPE type );
extern void  fei_rshift          	  ( TYPE type );
extern void  fei_ashift            	  ( TYPE type );
extern void  fei_hw_rshift       	  ( TYPE type );
extern void  fei_hw_lshift       	  ( TYPE type );
extern void  fei_exponentiate    	  ( TYPE type );
extern void  fei_complex         	  ( TYPE type );
#ifdef KEY /* Bug 10410 */
extern void  fei_select          	  ( TYPE type, int cselect );
#else /* KEY Bug 10410 */
extern void  fei_select          	  ( TYPE type );
#endif /* KEY Bug 10410 */
extern void  fei_mbits           	  ( TYPE type );
extern void  fei_dshiftl         	  ( TYPE type );
extern void  fei_dshiftr         	  ( TYPE type );
extern void  fei_ranf            	  ( TYPE type );
extern void  fei_ranget          	  ( TYPE type );
extern void  fei_ranset          	  ( TYPE type );
extern void  fei_leadz           	  ( TYPE type, TYPE arg );
extern void  fei_poppar          	  ( TYPE type, TYPE arg );
extern void  fei_popcnt          	  ( TYPE type, TYPE arg );
extern void  fei_new_binop_cshift 	  ( TYPE type );
extern void  fei_mvbits          	  ( TYPE type );
extern void  fei_ishftc          	  ( TYPE type );
extern void  fei_ibits           	  ( TYPE type );
extern void  fei__maxloc         	  ( TYPE type );
extern void  fei__minloc         	  ( TYPE type );
extern void  fei_backspace       	  ( void );
extern void  fei_close           	  ( void );
extern void  fei_endfile         	  ( void );
extern void  fei_inquire         	  ( void );
extern void  fei_open            	  ( void );
extern void  fei_rewind          	  ( void );
extern void  fei_iotype          	  ( void );
extern void  fei_malloc          	  ( void );
extern void  fei_mfree           	  ( void );
extern INTPTR fei_namelist         	  ( char   *name_string,
                                 	    INT32  nitems,
                                 	    INTPTR idx );
extern void  fei_fcd             	  ( TYPE type );
extern void  fei_rrspace         	  ( TYPE type );
extern void  fei_modulo          	  ( TYPE type );
extern void  fei_scale           	  ( TYPE type );
extern void  fei_scan            	  ( TYPE type );
extern void  fei_set_exponent    	  ( TYPE type );
extern void  fei_verify          	  ( TYPE type );
extern void  fei_space           	  ( TYPE type );
extern void  fei_adjustl         	  ( TYPE type );
extern void  fei_adjustr         	  ( TYPE type );
extern void  fei_doloop          	  ( INT32 line );
extern void  fei_dowhile         	  ( void );
extern void  fei_doforever       	  ( void );
extern void  fei_enddo           	  ( void );
extern void  fei_new_select_case 	  ( INT64 low_value_pres,
                                 	    INT64 high_value_pres,
                                 	    INT32 case_follows );
extern void  fei_new_select      	  ( INT32 num_cases,
#ifdef KEY /* Bug 12319 */
                                 	    INTPTR last_label_idx,
#endif /* KEY Bug 12319 */
                                 	    INTPTR default_label_idx );
extern TYPE  fei_dope_vector              ( INT32 num_dims, 
                                    	    TYPE  base_type,
                                     	    INT32 flags,
#ifdef KEY /* Bug 6845 */
                                     	    INT32 n_allocatable_cpnt
#endif /* KEY Bug 6845 */
					    );
extern void  fei_dv_ptr_asg               ( void );
#ifndef KEY /* Bug 6845 */
extern void  fei_set_dv_hdr_fld           ( INT32 field );
extern void  fei_get_dv_hdr_fld           ( INT32 field );
#endif /* KEY Bug 6845 */
extern void  fei_set_dv_low_bnd           ( INT32 dim );
extern void  fei_set_dv_extent            ( INT32 dim );
extern void  fei_set_dv_str_mult          ( INT32 dim );
extern void  fei_get_dv_low_bnd           ( INT32 dim, INT32 expand );
extern void  fei_get_dv_extent            ( INT32 dim, INT32 expand );
extern void  fei_get_dv_str_mult          ( INT32 dim, INT32 expand );
#ifdef KEY /* Bug 6845 */
extern void  fei_dv_def(INT32 num_dims, INT32 n_alloc_cpnt );
#else /* KEY Bug 6845 */
extern void  fei_dv_def                   ( INT32 num_dims );
#endif /* KEY Bug 6845 */
extern void  fei_all                      ( TYPE type );
extern void  fei_any                      ( TYPE type );
extern void  fei_count                    ( TYPE type );
extern void  fei_cshift                   ( TYPE type );
extern void  fei_dot_product              ( TYPE type );
extern void  fei_dot_product_logical      ( TYPE type );
extern void  fei_eoshift                  ( TYPE type );
extern void  fei_matmul                   ( TYPE type );
extern void  fei_maxloc                   ( TYPE type );
extern void  fei_maxval                   ( TYPE type );
extern void  fei_merge                    ( void );
extern void  fei_minloc                   ( TYPE type );
extern void  fei_minval                   ( TYPE type );
extern void  fei_pack                     ( TYPE type );
extern void  fei_premaxval                ( void );
extern void  fei_preminval                ( void );
extern void  fei_preproduct               ( void );
extern void  fei_presum                   ( void );
extern void  fei_product                  ( TYPE type );
extern void  fei_reshape                  ( TYPE type );
extern void  fei_scanmaxval               ( void );
extern void  fei_scanminval               ( void );
extern void  fei_scanproduct              ( void );
extern void  fei_scansum                  ( void );
extern void  fei_spread                   ( TYPE type );
extern void  fei_sum                      ( TYPE type );
extern void  fei_transpose                ( TYPE type );
extern void  fei_unpack                   ( TYPE type );
extern void  fei_fpclass                  ( TYPE type );
extern void  fei_signbit                  ( void );
extern void  fei_isfinite                 ( TYPE type );
extern void  fei_isnormal                 ( void );
extern void  fei_isnan                    ( TYPE type );
extern void  fei_isgreater                ( void );
extern void  fei_isgeq                    ( void );
extern void  fei_isless                   ( void );
extern void  fei_isleq                    ( void );
extern void  fei_islg                     ( TYPE type );
extern void  fei_isunordered              ( TYPE type );
extern void  fei_get_all_estat            ( void );
extern void  fei_set_all_estat            ( void );
extern void  fei_test_estat               ( void );
extern void  fei_set_estat                ( void );
extern void  fei_get_interupt             ( void );
extern void  fei_set_interupt             ( void );
extern void  fei_test_interupt            ( void );
extern void  fei_enbl_interupt            ( void );
extern void  fei_dsbl_interupt            ( void );
extern void  fei_get_rmode                ( void );
extern void  fei_set_rmode                ( void );
extern void  fei_scalb                    ( TYPE type );
extern void  fei_nextafter                ( TYPE type );
extern void  fei_set_ieee_stat            ( void );
extern void  fei_remainder                ( TYPE type );
extern void  fei_logb                     ( TYPE type );
extern void  fei_ieee_round               ( TYPE type );
extern void  fei_ieee_trunc               ( TYPE type );
extern void  fei_fort_reset               ( void );
extern void  fei_copyin_bound		  ( INTPTR sym_idx );
extern INT32 eval_npex     		  ( INT32 npdx, INT32 n_pes_val );
extern void  npex_constant 		  ( INT32 cdx );
extern void  npex_div      		  ( void );
extern INT32 npex_end      	  	  ( void );
extern void  npex_init     		  ( void );
extern void  npex_minus    		  ( void );
extern void  npex_mult     		  ( void );
extern void  npex_npes     		  ( void );
extern void  npex_plus     		  ( void );
extern void  npex_shiftl   		  ( void );
extern void  npex_shiftr   		  ( void );
extern void  npex_mod      		  ( void );
extern void  npex_min      		  ( void );
extern void  npex_max      		  ( void );
extern void  npex_uminus   		  ( void );
extern INT32 npex_to_dex   		  ( INT32 npdx );
extern void  npex_to_expr  		  ( INT32 npdx );
extern void  npex_dex_sync 		  ( void );
extern void  npex_print    		  ( INT32 npdx );
extern void  npex_paren    		  ( void );
extern void  npex_push     		  ( INT32 npdx );
extern void  fei_numargs                  ( TYPE  type );
const  char *opt_vers_name                ( void );
const  char *opt_vers_ID            	  ( void );
const  char *opt_vers_number        	  ( void );


extern void  fei_debug_mif_type_idx       ( INTPTR st_idx, 
                                            INT32 mif_type_idx,
                                            INT32 mif_table_type );
extern void  fei_debug_obj                ( INTPTR idx, 
                                            INT32 flags,
                                     	    INT32 classs, 
                                            char *name,
                                            INT32 insttype, 
                                            INT32 members,
                                            INT32 numinherit,
                                            INT32 firstobj );
extern void  fei_debug_objmem             ( INTPTR idx, 
					    INT32 flags,
                                            INT32 visibility, 
					    INT32 next,
                                            char *name );

extern INT32 fei_debug_obj_inherit        ( INTPTR objidx,
                                            INT32 visibility );

extern void  fei_debug_scope_info         ( INTPTR seg_idx, 
					    INT32 start_line,
                                            INT32 end_line );

extern void  fei_debug_type               ( INTPTR idx, 
					    INT32 flags,
                                            INT32 classs, 
					    INT32 base,
                                     	    INT32 list, 
				            char *name );

extern void  fei_debug_sym2               ( INTPTR idx, 
					    char *name,
                                            INT32 next, 
					    INT64 evalue );

extern void fei_return_addr               ( TYPE result_type );

extern void PDGCS_mpp_init                ( char        *src_fname,
                                            char  *cwd,
                                            FILE        *cif_ptr,
                                            char        *cmd_line,
                                            INT32        mpp_info_flgs,
                                            INT32        n_pes_val,
                                            INT32        num_barriers,
                                            INT32        num_eurekas,
                                            INT32        partition_type );






typedef struct		type_descriptor	pdg_type_tbl_type;
#ifdef KEY
extern int Check_FF2C_Script           (const char *callee_key,
                                         int  mangled ); 
#endif
# ifdef __cplusplus
}
# endif 

#endif /* I_CVRT_INCLUDED */
