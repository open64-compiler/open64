/*
 * Copyright (C) 2008. PathScale, LLC. All Rights Reserved.
 */
/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
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



/* USMID:  "\n@(#)5.0_pl/headers/tokens.h	5.3	08/30/99 11:18:28\n" */

 
/*******************************************************\
|* type specifiers used within lex and parser modules. *|
\*******************************************************/

enum	ch_class_values		{Ch_Class_Letter,
				 Ch_Class_Digit,
				 Ch_Class_Symbol,
				 Ch_Class_Dir1,		/* CDIR$ or CMIC$     */
				 Ch_Class_Dir2,		/* CDIR$ or CMIC$     */
				 Ch_Class_Dir3,		/* CDIR$ or CMIC$     */
				 Ch_Class_Dir4,		/* CDIR$ or CMIC$     */
				 Ch_Class_EOS,		/* End-Of-Stmt	      */
				 Ch_Class_EOF };	/* End-Of-File	      */

/* The tokens implied by the following token_class_values are:		      */
/*	Tok_Class_Label		==> Tok_Label				      */
/*	Tok_Class_Id		==> Tok_Id				      */
/*	Tok_Class_Keyword	==> Tok_Kwd_*  | Tok_Id			      */
/*	Tok_Class_Construct_Def	==> Tok_Id			              */
/*	Tok_Class_DO		==> Tok_Kwd_Do				      */
/*	Tok_Class_Dir_Kwd	==> Tok_Dir_*				      */
/*	Tok_Class_Mic_Kwd	==> Tok_Mic_*				      */
/*	Tok_Class_SGI_Dir_Kwd	==> Tok_SGI_Dir_* !$par !$ and !*$* dirs      */
/*	Tok_Class_Open_Mp_Dir_Kwd==> Tok_Open_Mp_* !$omp                      */
/*	Tok_Class_Mic_Kwd	==> Tok_Dbg_*		   DEBUG ONLY	      */
/*	Tok_Class_Punct		==> Tok_Punc_* | Tok_Eos		      */
/*	Tok_Class_Int_Spec	==> Tok_Const_Int			      */
/*	Tok_Class_Format_Str	==> tok str is chars between ( and matching ) */
/*	Tok_Class_Program_Str	==> tok str is chars between ( and matching ) */
/*	Tok_Class_Op		==> Tok_Op_*				      */
/*	Tok_Class_Opnd		==> Tok_Id | Tok_Const_* | Tok_Op_Defined     */

enum	token_class_values	{Tok_Class_Label,
				 Tok_Class_Id,
				 Tok_Class_Keyword,
                                 Tok_Class_Construct_Def,
				 Tok_Class_DO,
				 Tok_Class_Dir_Kwd,
				 Tok_Class_Mic_Kwd,
				 Tok_Class_SGI_Dir_Kwd,
				 Tok_Class_Open_Mp_Dir_Kwd,
				 Tok_Class_Dbg_Kwd,
				 Tok_Class_Punct,
				 Tok_Class_Int_Spec,
				 Tok_Class_Format_Str,
				 Tok_Class_Program_Str,
				 Tok_Class_Op,
				 Tok_Class_Opnd };
	
/* The following data objects are order/addition/deletion dependent on the    */
/* token_values enumeration (any changes to this definition must also be      */
/* reflected in these objects):						      */
/*		token_to_stmt_type   array in	p_driver.h		      */

enum	token_values {
		Tok_Label,		/*	     label		      */
		Tok_Id,			/*	     identifier,	      */
					/*	     construct-name,	      */
					/*	     kind-param-name	      */
		Tok_Kwd_Allocatable,	/* ALLOCATABLE			      */
		Tok_Kwd_Allocate,	/* ALLOCATE			      */
		Tok_Kwd_Assign,		/* ASSIGN			      */
		Tok_Kwd_Assignment,	/* ASSIGNMENT			      */
		Tok_Kwd_Automatic,	/* AUTOMATIC  			      */
		Tok_Kwd_Backspace,	/* BACKSPACE			      */
#ifdef KEY /* Bug 10572 */
		Tok_Kwd_Bind,		/* BIND				      */
#endif /* KEY Bug 10572 */
		Tok_Kwd_Block,		/* BLOCK     BLOCKDATA, ENDBLOCKDATA  */
		Tok_Kwd_Buffer,		/* BUFFER    BUFFERIN, BUFFEROUT      */
		Tok_Kwd_Byte,		/* BYTE                               */
		Tok_Kwd_Call,		/* CALL				      */
		Tok_Kwd_Case,		/* CASE	     SELECTCASE		      */
		Tok_Kwd_Character,	/* CHARACTER			      */
		Tok_Kwd_Close,		/* CLOSE			      */
		Tok_Kwd_Common,		/* COMMON			      */
		Tok_Kwd_Complex,	/* COMPLEX			      */
		Tok_Kwd_Contains,	/* CONTAINS			      */
		Tok_Kwd_Continue,	/* CONTINUE			      */
		Tok_Kwd_Cycle,		/* CYCLE			      */
		Tok_Kwd_Data,		/* DATA	     BLOCKDATA, ENDBLOCKDATA  */
		Tok_Kwd_Deallocate,	/* DEALLOCATE			      */
		Tok_Kwd_Decode,		/* DECODE			      */
		Tok_Kwd_Default,	/* DEFAULT   CASE DEFAULT	      */
		Tok_Kwd_Dimension,	/* DIMENSION			      */
		Tok_Kwd_Dir,		/* DIR$	     CDIR$, CDIR@,	      */
					/*	     !DIR$, !DIR@	      */
					/* MIC$	     CMIC$, !MIC$	      */
		Tok_Kwd_Do,		/* DO	     ENDDO		      */
		Tok_Kwd_Double,		/* DOUBLE    DOUBLEPRECISION,	      */
		Tok_Kwd_Elemental,	/* ELEMENTAL                          */
		Tok_Kwd_Else,		/* ELSE	     ELSEIF, ELSEWHERE	      */
		Tok_Kwd_Encode,		/* ENCODE			      */
		Tok_Kwd_End,		/* END	     ENDBLOCKDATA, ENDDO,     */
					/*	     ENDFILE, ENDFUNCTION,    */
					/*	     ENDIF, ENDINTERFACE,     */
					/*	     ENDMODULE, ENDPROGRAM,   */
					/*	     ENDSELECT, ENDSUBROUTINE,*/
					/*	     ENDTYPE, ENDWHERE	      */
		Tok_Kwd_Entry,		/* ENTRY			      */
#ifdef KEY /* Bug 10572 */
		Tok_Kwd_Enum,		/* ENUM 			      */
		Tok_Kwd_Enumerator,	/* ENUMERATOR 			      */
#endif /* KEY Bug 10572 */
		Tok_Kwd_Equivalence,	/* EQUIVALENCE			      */
		Tok_Kwd_Exit,		/* EXIT				      */
		Tok_Kwd_External,	/* EXTERNAL			      */
		Tok_Kwd_File,		/* FILE	     ENDFILE		      */
		Tok_Kwd_Forall,		/* FORALL			      */
		Tok_Kwd_Format,		/* FORMAT			      */
		Tok_Kwd_Function,	/* FUNCTION  ENDFUNCTION	      */
		Tok_Kwd_Go,		/* GO	     GOTO		      */
		Tok_Kwd_If,		/* IF	     ELSEIF, ENDIF	      */
		Tok_Kwd_Implicit,	/* IMPLICIT			      */
#ifdef KEY /* Bug 11741 */
		Tok_Kwd_Import,		/* IMPORT			      */
#endif /* KEY Bug 11741 */
		Tok_Kwd_In,		/* IN	     BUFFERIN, INOUT	      */
		Tok_Kwd_Inquire,	/* INQUIRE			      */
		Tok_Kwd_Integer,	/* INTEGER			      */
		Tok_Kwd_Intent,		/* INTENT			      */
		Tok_Kwd_Interface,	/* INTERFACE ENDINTERFACE	      */
		Tok_Kwd_Intrinsic,	/* INTRINSIC			      */
		Tok_Kwd_Kind,		/* KIND				      */
		Tok_Kwd_Len,		/* LEN				      */
		Tok_Kwd_Logical,	/* LOGICAL			      */
		Tok_Kwd_Module,		/* MODULE    ENDMODULE,		      */
					/*	     MODULEPROCEDURE	      */
#ifdef KEY /* Bug 10572 */
		Tok_Kwd_Name,		/* NAME				      */
#endif /* KEY Bug 10572 */
		Tok_Kwd_Namelist,	/* NAMELIST			      */
		Tok_Kwd_None,		/* NONE				      */
#ifdef KEY /* Bug 5089 */
		Tok_Kwd_Nonintrinsic,	/* NON_INTRINSIC                      */
#endif /* KEY Bug 5089 */
		Tok_Kwd_Nullify,	/* NULLIFY			      */
		Tok_Kwd_Only,		/* ONLY				      */
		Tok_Kwd_Open,		/* OPEN				      */
		Tok_Kwd_Operator,	/* OPERATOR			      */
		Tok_Kwd_Optional,	/* OPTIONAL			      */
		Tok_Kwd_Out,		/* OUT	     BUFFEROUT, INOUT	      */
		Tok_Kwd_Parameter,	/* PARAMETER			      */
		Tok_Kwd_Pause,		/* PAUSE			      */
		Tok_Kwd_Pointer,	/* POINTER			      */
		Tok_Kwd_Precision,	/* PRECISION DOUBLEPRECISION	      */
		Tok_Kwd_Print,		/* PRINT			      */
		Tok_Kwd_Private,	/* PRIVATE			      */
		Tok_Kwd_Procedure,	/* PROCEDURE MODULEPROCEDURE	      */
		Tok_Kwd_Program,	/* PROGRAM   ENDPROGRAM		      */
		Tok_Kwd_Public,		/* PUBLIC			      */
		Tok_Kwd_Pure,		/* PURE  			      */
		Tok_Kwd_Read,		/* READ				      */
		Tok_Kwd_Real,		/* REAL				      */
		Tok_Kwd_Recursive,	/* RECURSIVE			      */
		Tok_Kwd_Result,		/* RESULT			      */
		Tok_Kwd_Return,		/* RETURN			      */
		Tok_Kwd_Rewind,		/* REWIND			      */
		Tok_Kwd_Save,		/* SAVE				      */
		Tok_Kwd_Select,		/* SELECT    ENDSELECT, SELECTCASE    */
		Tok_Kwd_Sequence,	/* SEQUENCE			      */
		Tok_Kwd_Span,		/* SPAN                               */
		Tok_Kwd_Stat,		/* STAT				      */
		Tok_Kwd_Static,		/* STATIC			      */
		Tok_Kwd_Stop,		/* STOP				      */
		Tok_Kwd_Subroutine,	/* SUBROUTINE ENDSUBROUTINE	      */
		Tok_Kwd_Target,		/* TARGET			      */
                Tok_Kwd_Task,           /* TASK                               */
		Tok_Kwd_Then,		/* THEN				      */
		Tok_Kwd_To,		/* TO	     GOTO		      */
		Tok_Kwd_Type,		/* TYPE	     ENDTYPE		      */
		Tok_Kwd_Use,		/* USE				      */
		Tok_Kwd_Undefined,	/* UNDEFINED			      */
#ifdef KEY /* Bug 14150 */
		Tok_Kwd_Value,	        /* VALUE   			      */
#endif /* KEY Bug 14150 */
		Tok_Kwd_Volatile,	/* VOLATILE			      */
		Tok_Kwd_Where,		/* WHERE     ELSEWHERE, ENDWHERE      */
		Tok_Kwd_While,		/* WHILE			      */
		Tok_Kwd_Write,		/* WRITE			      */

                /* NOTE -> If a new Tok_Dir is added, directive_str in */
                /*         main.h needs to be updated.                 */

                /* NOTE again -> also tied to cdir_info in p_directiv.h */

		Tok_Dir_Start,		/* Need to mark start of directives.  */
		Tok_Dir_Align,		/* ALIGN			      */
		Tok_Dir_Atomicupdate,	/* ATOMIC UPDATE		      */
		Tok_Dir_Autoscope,	/* AUTOSCOPE			      */
		Tok_Dir_Auxiliary,	/* AUXILIARY			      */
		Tok_Dir_Barrier,	/* BARRIER			      */
		Tok_Dir_Bl,		/* BL				      */
		Tok_Dir_Block,		/* BLOCK			      */
		Tok_Dir_Blockable,	/* BLOCKABLE			      */
		Tok_Dir_Blockingsize,	/* BLOCKINGSIZE			      */
		Tok_Dir_Bounds,		/* BOUNDS			      */
		Tok_Dir_Cache_Align,	/* CACHE_ALIGN			      */
		Tok_Dir_Cache_Bypass,	/* CACHE_BYPASS    		      */
		Tok_Dir_Cache_Noalloc,	/* CACHE_NOALLOCATE   		      */
		Tok_Dir_Chunksize,	/* CHUNKSIZE			      */
                Tok_Dir_Cncall,         /* CNCALL                             */
		Tok_Dir_Code,           /* CODE                               */
		Tok_Dir_Common,         /* COMMON                             */
		Tok_Dir_Concurrent,	/* CONCURRENT			      */
		Tok_Dir_Control,	/* CONTROL			      */
                Tok_Dir_Copy_Assumed_Shape,     /* COPY_ASSUMED_SHAPE         */
		Tok_Dir_Critical,	/* CRITICAL			      */
		Tok_Dir_Doshared,	/* DOSHARED			      */
		Tok_Dir_Dynamic,	/* DYNAMIC			      */
		Tok_Dir_Eject,		/* EJECT			      */
		Tok_Dir_Endcritical,	/* END CRITICAL			      */
		Tok_Dir_Endmaster,	/* END MASTER			      */
                Tok_Dir_Fixed,          /* FIXED                              */
		Tok_Dir_Flow,		/* FLOW				      */
                Tok_Dir_Free,           /* FREE                               */
                Tok_Dir_Geometry,       /* GEOMETRY                           */
		Tok_Dir_Getfirst,	/* GETFIRST			      */
		Tok_Dir_Guided,		/* GUIDED			      */
		Tok_Dir_Id,		/* ID				      */
		Tok_Dir_If,		/* IF				      */
                Tok_Dir_Ignore_TKR,	/* IGNORE_TKR			      */
		Tok_Dir_Inline,		/* INLINE			      */
		Tok_Dir_Inline_Always,	/* INLINE ALWAYS		      */
		Tok_Dir_Inline_Never,	/* INLINE NEVER			      */
		Tok_Dir_Integer,	/* INTEGER			      */
		Tok_Dir_Interchange,	/* INTERCHANGE			      */
		Tok_Dir_Ivdep,		/* IVDEP			      */
		Tok_Dir_List,		/* LIST				      */
		Tok_Dir_Mark,		/* MARK				      */
		Tok_Dir_Master,		/* MASTER			      */
		Tok_Dir_Maxcpus,	/* MAXCPUS			      */
		Tok_Dir_Modinline,	/* MODINLINE			      */
		Tok_Dir_Name,		/* NAME				      */
		Tok_Dir_Ncpus_Chunks,	/* NCPUS_CHUNKS			      */
		Tok_Dir_Nextscalar,	/* NEXTSCALAR			      */
		Tok_Dir_Nobarrier,	/* NO BARRIER			      */
		Tok_Dir_Nobl,		/* NOBL				      */
		Tok_Dir_Noblocking,	/* NOBLOCKING			      */
		Tok_Dir_Nobounds,	/* NOBOUNDS			      */
                Tok_Dir_Nocinv,         /* NOCINV                             */
                Tok_Dir_Nocode,         /* NOCODE                             */
		Tok_Dir_Noflow,		/* NOFLOW			      */
		Tok_Dir_Noinline,	/* NOINLINE			      */
		Tok_Dir_Nointerchange,	/* NOINTERCHANGE		      */
		Tok_Dir_Nolist,		/* NOLIST			      */
		Tok_Dir_Nomark,		/* NOMARK			      */
		Tok_Dir_Nomodinline,	/* NOMODINLINE			      */
		Tok_Dir_Nopattern,	/* NOPATTERN			      */
		Tok_Dir_Norecurrence,	/* NORECURRENCE			      */
		Tok_Dir_Nosideeffects,	/* NOSIDEEFFECTS		      */
		Tok_Dir_Nosplit,	/* NOSPLIT  (T3E only)		      */
		Tok_Dir_Nostream,	/* NOSTREAM			      */
		Tok_Dir_Notask,		/* NOTASK			      */
		Tok_Dir_Nounroll,	/* NOUNROLL			      */
		Tok_Dir_Novector,	/* NOVECTOR			      */
		Tok_Dir_Novsearch,	/* NOVSEARCH			      */
		Tok_Dir_Numchunks,	/* NUMCHUNKS			      */
                Tok_Dir_Numcpus,        /* NUMCPUS                            */
		Tok_Dir_Parallel_Only,	/* PARALLEL_ONLY		      */
		Tok_Dir_Pattern,	/* PATTERN			      */
		Tok_Dir_Pe_Private,	/* PE_PRIVATE 			      */
		Tok_Dir_Pe_Resident,	/* PE_RESIDENT 			      */
                Tok_Dir_Permutation,    /* PERMUTATION                        */
		Tok_Dir_Preferstream,	/* PREFERSTREAM			      */
		Tok_Dir_Prefertask,	/* PREFERTASK			      */
		Tok_Dir_Prefervector,	/* PREFERVECTOR			      */
		Tok_Dir_Private,	/* PRIVATE			      */
		Tok_Dir_Recurrence,	/* RECURRENCE			      */
		Tok_Dir_Regfile,	/* REGFILE			      */
		Tok_Dir_Savelast,	/* SAVELAST			      */
		Tok_Dir_Semextern,	/* SEMEXTERN			      */
		Tok_Dir_Serial_Only,	/* SERIAL_ONLY			      */
		Tok_Dir_Shared,		/* SHARED			      */
		Tok_Dir_Shortloop,	/* SHORTLOOP			      */
		Tok_Dir_Shortsequence,	/* SHORTSEQUENCE		      */
		Tok_Dir_Single,		/* SINGLE			      */
		Tok_Dir_Split,		/* SPLIT  (T3E only)		      */
		Tok_Dir_Stack,		/* STACK			      */
		Tok_Dir_Static,		/* STATIC			      */
		Tok_Dir_Stream,		/* STREAM			      */
		Tok_Dir_Suppress,	/* SUPPRESS			      */
		Tok_Dir_Symmetric,	/* SYMMETRIC			      */
		Tok_Dir_System_Module,	/* SYSTEM_MODULE		      */
		Tok_Dir_Task,		/* TASK      			      */
		Tok_Dir_Taskcommon,	/* TASKCOMMON			      */
		Tok_Dir_Taskhead,	/* TASKHEAD			      */
		Tok_Dir_Unknown,	/* UNKNOWN			      */
		Tok_Dir_Unknown_Shared,	/* UNKNOWN_SHARED		      */
		Tok_Dir_Unroll,		/* UNROLL			      */
		Tok_Dir_Uses_Eregs,	/* USES EREGS			      */
		Tok_Dir_Vector,		/* VECTOR			      */
		Tok_Dir_Vfunction,	/* VFUNCTION			      */
		Tok_Dir_Vsearch,	/* VSEARCH			      */
		Tok_Dir_End,		/* Need to mark end of directives.    */

		Tok_Mic_Start,		/* Need to mark start of cmics.       */
		Tok_Mic_Case,		/* CASE                               */
		Tok_Mic_End_Case,	/* END CASE                           */
		Tok_Mic_Cncall,		/* CNCALL                             */
		Tok_Mic_Continue,	/* CONTINUE                           */
		Tok_Mic_Do_All,		/* DO ALL                             */
		Tok_Mic_Do_Parallel,	/* DO PARALLEL                        */
		Tok_Mic_End_Do,		/* END DO                             */
		Tok_Mic_Guard,		/* GUARD                              */
		Tok_Mic_End_Guard,	/* END GUARD                          */
		Tok_Mic_If,		/* IF		                      */
		Tok_Mic_Maxcpus,	/* MAXCPUS			      */
		Tok_Mic_Numcpus,	/* NUMCPUS			      */
		Tok_Mic_Parallel,	/* PARALLEL                           */
		Tok_Mic_End_Parallel,	/* END PARALLEL                       */
		Tok_Mic_Permutation,	/* PERMUTATION			      */
		Tok_Mic_Point,		/* POINT                              */
		Tok_Mic_Send,		/* SEND                               */
		Tok_Mic_Span,		/* SPAN                               */
		Tok_Mic_Taskcommon,	/* TASKCOMMON                         */
		Tok_Mic_Wait,		/* WAIT                               */
		Tok_Mic_End,		/* Need to mark end of cmics.	      */

		Tok_SGI_Dir_Start,		/* Start of SGI directives    */
		Tok_SGI_Dir_Affinity,		/* AFFINITY		      */
		Tok_SGI_Dir_Aggressiveinner,	/* AGGRESSIVEINNERLOOPFUSION  */
		Tok_SGI_Dir_Align_Symbol,	/* ALIGN SYMBOL		      */
		Tok_SGI_Dir_Argumentaliasing,	/* ARGUMENT ALIASING 	      */

		Tok_SGI_Dir_Assert,		/* ASSERT      		      */
		Tok_SGI_Dir_Barrier,		/* BARRIER		      */
		Tok_SGI_Dir_Benign,		/* BENIGN                     */
		Tok_SGI_Dir_Block,		/* BLOCK		      */
		Tok_SGI_Dir_Blockable,		/* BLOCKABLE   		      */
		Tok_SGI_Dir_Blocked,		/* BLOCKED		      */
		Tok_SGI_Dir_Blockingsize,	/* BLOCKING SIZE	      */
		Tok_SGI_Dir_Boundsviolations,	/* BOUNDS VIOLATIONS          */
		Tok_SGI_Dir_Chunk,		/* CHUNK		      */
		Tok_SGI_Dir_Concur,        	/* CONCUR                     */
		Tok_SGI_Dir_Concurrent,    	/* CONCURRENT                 */
		Tok_SGI_Dir_Concurrentcall,	/* CONCURRENT CALL            */
		Tok_SGI_Dir_Concurrentize,    	/* CONCURRENTIZE              */
		Tok_SGI_Dir_Copyin,		/* COPYIN		      */
		Tok_SGI_Dir_Criticalsection,	/* CRITICAL SECTION	      */
		Tok_SGI_Dir_Cyclic,		/* CYCLIC		      */
		Tok_SGI_Dir_Data,		/* DATA			      */
		Tok_SGI_Dir_Dependence,		/* DEPENDENCE                 */
		Tok_SGI_Dir_Distribute,		/* DISTRIBUTE		      */
		Tok_SGI_Dir_Distribute_Reshape,	/* DISTRIBUTE_RESHAPE	      */
		Tok_SGI_Dir_Do,			/* DO                         */
		Tok_SGI_Dir_Doacross,		/* DO ACROSS		      */
		Tok_SGI_Dir_Doprefer,		/* DO PREFER                  */
		Tok_SGI_Dir_Dynamic,		/* DYNAMIC		      */
		Tok_SGI_Dir_Endcriticalsection,	/* END CRITICAL SECTION	      */
		Tok_SGI_Dir_Endparallel,	/* END PARALLEL		      */
		Tok_SGI_Dir_Endpdo,		/* END PDO		      */
		Tok_SGI_Dir_Endpsection,	/* END PSECTION		      */
		Tok_SGI_Dir_Endpsections,	/* END PSECTIONS	      */
		Tok_SGI_Dir_Endsingleprocess,	/* END SINGLE PROCESS	      */
		Tok_SGI_Dir_Equivalencehazard,	/* EQUIVALENCE HAZARD         */
		Tok_SGI_Dir_Fill_Symbol,	/* FILL SYMBOL 		      */
		Tok_SGI_Dir_Fission,		/* FISSION     		      */
		Tok_SGI_Dir_Fissionable,	/* FISSIONABLE 		      */
		Tok_SGI_Dir_Flush,      	/* FLUSH       		      */
		Tok_SGI_Dir_Frequency,		/* FREQUENCY                  */
		Tok_SGI_Dir_Fusable,		/* FUSABLE      	      */
		Tok_SGI_Dir_Fuse,		/* FUSE        		      */
		Tok_SGI_Dir_Global,		/* GLOBAL      		      */
		Tok_SGI_Dir_Gss,		/* GSS			      */
		Tok_SGI_Dir_Guided,		/* GUIDED		      */
		Tok_SGI_Dir_Here,		/* HERE 	 	      */
		Tok_SGI_Dir_If,			/* IF			      */
		Tok_SGI_Dir_Ignoreanydependence,/* IGNORE ANY DEPENDENCE      */
		Tok_SGI_Dir_Ignoreanydependences,/* IGNORE ANY DEPENDENCES    */
		Tok_SGI_Dir_Ignoreassumeddependence,
                                                /* IGNORE ASSUMED DEPENDENCE  */
		Tok_SGI_Dir_Ignoreassumeddependences,
                                                /* IGNORE ASSUMED DEPENDENCES */
		Tok_SGI_Dir_Inline,		/* INLINE	       	      */
		Tok_SGI_Dir_Interchange,	/* INTERCHANGE         	      */
		Tok_SGI_Dir_Interleave,		/* INTERLEAVE		      */
		Tok_SGI_Dir_Interleaved,	/* INTERLEAVED		      */
		Tok_SGI_Dir_Ipa,		/* IPA	         	      */
		Tok_SGI_Dir_Kind,		/* KIND       	 	      */
		Tok_SGI_Dir_Lastlocal,		/* LASTLOCAL		      */
		Tok_SGI_Dir_Lastthread,		/* LAST THREAD		      */
		Tok_SGI_Dir_Lastvalueneeded,	/* LAST VALUE NEEDED          */
		Tok_SGI_Dir_Lastvaluesneeded,	/* LAST VALUES NEEDED         */
		Tok_SGI_Dir_Level,		/* LEVEL		      */
		Tok_SGI_Dir_Limit,		/* LIMIT		      */
		Tok_SGI_Dir_Local,		/* LOCAL		      */
		Tok_SGI_Dir_L1cacheline,	/* L1CACHELINE                */
		Tok_SGI_Dir_L2cacheline,	/* L2CACHELINE                */
		Tok_SGI_Dir_Minconcurrent,	/* MINCONCURRENT	      */
		Tok_SGI_Dir_Mp_Schedtype,	/* MP_SCHEDTYPE		      */
		Tok_SGI_Dir_Ncpus_Chunk,	/* NCPUS_CHUNK		      */
		Tok_SGI_Dir_Nest,		/* NEST			      */
		Tok_SGI_Dir_Noargumentaliasing,	/* NOARGUMENTALIASING	      */
		Tok_SGI_Dir_Noblocking,		/* NO BLOCKING  	      */
		Tok_SGI_Dir_Noboundsviolations,	/* NO BOUNDS VIOLATIONS       */
		Tok_SGI_Dir_Noconcurrentcall,	/* NO CONCURRENT CALL         */
		Tok_SGI_Dir_Noconcurrentize,   	/* NO CONCURRENTIZE           */
		Tok_SGI_Dir_Noequivalencehazard,/* NO EQUIVALENCE HAZARD      */
		Tok_SGI_Dir_Nofission,		/* NO FISSION   	      */
		Tok_SGI_Dir_Nofusion,		/* NO FUSION	   	      */
		Tok_SGI_Dir_Noinline,		/* NO INLINE    	      */
		Tok_SGI_Dir_Nointerchange,	/* NO INTERCHANGE	      */
		Tok_SGI_Dir_Noipa,		/* NO IPA      		      */
		Tok_SGI_Dir_Nolastvalueneeded,	/* NO LASTVALUE NEEDED        */
		Tok_SGI_Dir_Nolastvaluesneeded,	/* NO LASTVALUES NEEDED       */
		Tok_SGI_Dir_Norecurrence,	/* NO RECURRENCE              */
		Tok_SGI_Dir_Nosync,		/* NO SYNC                    */
		Tok_SGI_Dir_Notemporariesforconstantarguments,
                                     /* NO TEMPORARIES FOR CONSTANT ARGUMENTS */
		Tok_SGI_Dir_Nowait,		/* NOWAIT		      */
		Tok_SGI_Dir_Numchunks,		/* NUMCHUNKS		      */
		Tok_SGI_Dir_Onto,		/* ONTO			      */
		Tok_SGI_Dir_Opaque,		/* OPAQUE       	      */
		Tok_SGI_Dir_Optional,		/* OPTIONAL     	      */
#ifdef KEY /* Bug 2660 */
		Tok_SGI_Dir_Options,		/* OPTIONS     	              */
#endif /* KEY Bug 2660 */
		Tok_SGI_Dir_Ordered,		/* ORDERED		      */
		Tok_SGI_Dir_Page,      		/* PAGE      		      */
		Tok_SGI_Dir_Page_Place,		/* PAGE_PLACE		      */
		Tok_SGI_Dir_Parallel,		/* PARALLEL		      */
		Tok_SGI_Dir_Paralleldo,		/* PARALLEL DO		      */
		Tok_SGI_Dir_Pdo,		/* PDO			      */
		Tok_SGI_Dir_Permutation,	/* PERMUTATION                */
		Tok_SGI_Dir_Prefetch,		/* PREFETCH		      */
		Tok_SGI_Dir_Prefetch_Manual,	/* PREFETCH_MANUAL	      */
		Tok_SGI_Dir_Prefetch_Ref,	/* PREFETCH_REF		      */
		Tok_SGI_Dir_Prefetch_Ref_Disable,/* PREFETCH_REF_DISABLE      */
		Tok_SGI_Dir_Private,		/* PRIVATE		      */
		Tok_SGI_Dir_Psection,		/* PSECTION		      */
		Tok_SGI_Dir_Psections,		/* PSECTIONS		      */
		Tok_SGI_Dir_Redistribute,	/* REDISTRIBUTE		      */
		Tok_SGI_Dir_Reduction,		/* REDUCTION		      */
		Tok_SGI_Dir_Regionbegin,	/* REGIONBEGIN		      */
		Tok_SGI_Dir_Regionend,		/* REGIONEND		      */
		Tok_SGI_Dir_Relation,		/* RELATION                   */
		Tok_SGI_Dir_Routine,		/* ROUTINE		      */
		Tok_SGI_Dir_Runtime,		/* RUNTIME		      */
		Tok_SGI_Dir_Section,		/* SECTION		      */
		Tok_SGI_Dir_Section_Gp,		/* SECTION_GP		      */
		Tok_SGI_Dir_Section_Non_Gp,	/* SECTION_NON_GP	      */
		Tok_SGI_Dir_Serial,		/* SERIAL		      */
		Tok_SGI_Dir_Share,		/* SHARE		      */
		Tok_SGI_Dir_Shared,		/* SHARED		      */
		Tok_SGI_Dir_Simple,		/* SIMPLE		      */
		Tok_SGI_Dir_Single,		/* SINGLE		      */
		Tok_SGI_Dir_Singleprocess,	/* SINGLE PROCESS	      */
		Tok_SGI_Dir_Size,		/* SIZE			      */
		Tok_SGI_Dir_Static,		/* STATIC		      */
		Tok_SGI_Dir_Stride,		/* STRIDE		      */
		Tok_SGI_Dir_Unroll,		/* UNROLL		      */
		Tok_SGI_Dir_Usecompress,	/* USE COMPRESS               */
		Tok_SGI_Dir_Usecontrolledstore,	/* USE CONTROLLED STORE       */
		Tok_SGI_Dir_Useexpand,		/* USE EXPAND                 */
		Tok_SGI_Dir_Usegather,		/* USE GATHER                 */
		Tok_SGI_Dir_Usescatter,		/* USE SCATTER                */
		Tok_SGI_Dir_Temporariesforconstantarguments,
                                     /* TEMPORARIES FOR CONSTANT ARGUMENTS    */
		Tok_SGI_Dir_Thread,		/* THREAD		      */
		Tok_SGI_Dir_Tile,		/* TILE  		      */
		Tok_SGI_Dir_Vector,		/* VECTOR		      */
		Tok_SGI_Dir_End,		/* End of SGI directives      */


		Tok_Open_Mp_Dir_Start,		/* Start of open mp directives*/
                Tok_Open_Mp_Dir_Affinity,       /* AFFINITY                   */
                Tok_Open_Mp_Dir_Atomic,         /* ATOMIC                     */
                Tok_Open_Mp_Dir_Barrier,        /* BARRIER                    */
                Tok_Open_Mp_Dir_Copyin,         /* COPYIN                     */
                Tok_Open_Mp_Dir_Copyprivate, /* COPYPRIVATE          */ /* by jhs, 02/7/5 */
                Tok_Open_Mp_Dir_Critical,       /* CRITICAL                   */
                Tok_Open_Mp_Dir_Data,           /* DATA                       */
                Tok_Open_Mp_Dir_Default,        /* DEFAULT                    */
                Tok_Open_Mp_Dir_Distribute,     /* DISTRIBUTE                 */
                Tok_Open_Mp_Dir_Distribute_Reshape, /* DISTRIBUTE_RESHAPE     */
                Tok_Open_Mp_Dir_Do,             /* DO                         */
                Tok_Open_Mp_Dir_Dynamic,        /* DYNAMIC                    */
                Tok_Open_Mp_Dir_Endcritical,    /* END CRITICAL               */
                Tok_Open_Mp_Dir_Enddo,          /* END DO                     */
                Tok_Open_Mp_Dir_Endparallel,    /* END PARALLEL               */
                Tok_Open_Mp_Dir_Endparalleldo,  /* END PARALLEL DO            */
                Tok_Open_Mp_Dir_Endparallelsections, /* END PARALLEL SECTIONS */
                Tok_Open_Mp_Dir_Endparallelworkshare, /* END PARALLEL WORKSHARE */ /* by jhs, 02/7/5 */
                Tok_Open_Mp_Dir_Endmaster,      /* END MASTER                 */
                Tok_Open_Mp_Dir_Endordered,     /* END ORDERED                */
                Tok_Open_Mp_Dir_Endsections,    /* END SECTIONS               */
                Tok_Open_Mp_Dir_Endsingle,      /* END SINGLE                 */
                Tok_Open_Mp_Dir_Endworkshare, /* END WORKSHARE */ /* by jhs, 02/7/5 */
                Tok_Open_Mp_Dir_Firstprivate,   /* FIRSTPRIVATE               */
                Tok_Open_Mp_Dir_Flush,          /* FLUSH                      */
                Tok_Open_Mp_Dir_Guided,         /* GUIDED                     */
                Tok_Open_Mp_Dir_If,             /* IF                         */
                Tok_Open_Mp_Dir_Lastprivate,    /* LASTPRIVATE                */
                Tok_Open_Mp_Dir_Master,         /* MASTER                     */
                Tok_Open_Mp_Dir_Nest,           /* NEST                       */
                Tok_Open_Mp_Dir_None,           /* NONE                       */
                Tok_Open_Mp_Dir_Nowait,         /* NOWAIT                     */
                Tok_Open_Mp_Dir_Num_Threads, /* NUM_THREADS */ /* by jhs, 02/7/5 */
                Tok_Open_Mp_Dir_Onto,           /* ONTO                       */
                Tok_Open_Mp_Dir_Ordered,        /* ORDERED                    */
                Tok_Open_Mp_Dir_Page_Place,     /* PAGE PLACE                 */
                Tok_Open_Mp_Dir_Parallel,       /* PARALLEL                   */
                Tok_Open_Mp_Dir_Paralleldo,     /* PARALLEL DO                */
                Tok_Open_Mp_Dir_Parallelsections,  /* PARALLEL SECTIONS */
                Tok_Open_Mp_Dir_Parallelworkshare, /* PARALLEL WORKSHARE */ /* by jhs, 02/7/5 */
                Tok_Open_Mp_Dir_Private,        /* PRIVATE                    */
                Tok_Open_Mp_Dir_Redistribute,   /* REDISTRIBUTE               */
                Tok_Open_Mp_Dir_Reduction,      /* REDUCTION                  */
                Tok_Open_Mp_Dir_Runtime,        /* RUNTIME                    */
                Tok_Open_Mp_Dir_Schedule,       /* SCHEDULE                   */
                Tok_Open_Mp_Dir_Section,        /* SECTION                    */
                Tok_Open_Mp_Dir_Sections,       /* SECTIONS                   */
                Tok_Open_Mp_Dir_Shared,         /* SHARED                     */
                Tok_Open_Mp_Dir_Single,         /* SINGLE                     */
                Tok_Open_Mp_Dir_Static,         /* STATIC                     */
                Tok_Open_Mp_Dir_Thread,         /* THREAD                     */
                Tok_Open_Mp_Dir_Threadprivate,  /* THREADPRIVATE              */
                Tok_Open_Mp_Dir_Workshare, /* WORKSHARE */ /* by jhs, 02/7/5 */
                Tok_Open_Mp_Dir_End,            /* End of Open Mp directives  */

		Tok_Dbg_Sytb,			/* Symbol table               */
		Tok_Dbg_Stmt,			/* Statement                  */

		Tok_Punct_Colon,	/* :	     construct-name-sep,      */
					/*	     array-shape-spec-sep,    */
					/*	     substring-range-sep,     */
					/*	     subscript-triplet-sep,   */
					/*	     case-value-range-sep,    */
					/*	     only-list-sep	      */
		Tok_Punct_Colon_Colon,	/* ::	     declaration-spec-sep     */
		Tok_Punct_Comma,	/* ,	     list-item-sep	      */
		Tok_Punct_Dash,		/* -	     letter-spec-sep	      */
		Tok_Punct_Eq,		/* =	     initialization-exp-sep,  */
					/*	     kind-spec-sep,	      */
					/*	     char-len-spec-sep,	      */
					/*	     IO-spec-sep,	      */
					/*	     actual-arg-spec-sep,     */
					/*	     stmt-func-def-sep	      */
		Tok_Punct_Lbrkt,	/* (/ 	     array-constructor-delim  */
		Tok_Punct_Lparen,	/* (	     left-paren-delim	      */
		Tok_Punct_Rbrkt,	/* /)  	     array-constructor-delim  */
		Tok_Punct_Rename,	/* =>	     rename-sep		      */
		Tok_Punct_Rparen,	/* )	     right-paren-delim	      */
		Tok_Punct_Slash,	/* /	     common-name-sep,	      */
					/*	     data-value-list-sep,     */
					/*	     save-list-sep,	      */
					/*	     namelist-group-sep	      */
		Tok_Punct_Star,		/* *	     char-len-spec,	      */
					/*	     data-repeat-spec,	      */
					/*	     array-size-spec,	      */
					/*	     IO-unit-spec,	      */
					/*	     format-spec,	      */
					/*	     alt-return-spec,	      */
					/*	     dummy-arg-spec	      */

		Tok_Const_False,	/* .FALSE.   logical-false-constant,  */
					/* .F.	     logical-false-constant   */
		Tok_Const_True,		/* .TRUE.    logical-true-constant,   */
					/* .T.	     logical-true-constant    */
		Tok_Const_Hollerith,	/*	     hollerith-constant	      */
		Tok_Const_Boolean,	/*	     short-hollerith-constant,*/
					/*	     octal-constant,	      */
					/*	     hex-constant	      */
		Tok_Const_Boz,		/*	     binary-constant,	      */
					/*	     octal-constant,	      */
					/*	     hex-constant	      */
		Tok_Const_Char,		/*	     character-constant	      */
		Tok_Const_Int,		/*	     integer-constant	      */
		Tok_Const_Real,		/*	     real-constant	      */
		Tok_Const_Dbl,		/*	     double-constant	      */
		Tok_Const_Quad,		/*	     quad-constant            */

		Tok_Op_Add,		/* +	     addition-op	      */
		Tok_Op_Div,		/* /	     division-op,	      */
		Tok_Op_Mult,		/* *	     multiplication-op	      */
		Tok_Op_Power,		/* **	     exponentiation-op	      */
		Tok_Op_Sub,		/* -	     subtraction-op	      */

		Tok_Op_Concat,		/* //	     concatenation-op	      */

		Tok_Op_Eq,		/* .EQ. ==   equal-op		      */
		Tok_Op_Ge,		/* .GE. >=   greater-equal-op	      */
		Tok_Op_Gt,		/* .GT. >    greater-op		      */
		Tok_Op_Le,		/* .LE. <=   less-equal-op	      */
		Tok_Op_Lt,		/* .LT. <    less-op		      */
		Tok_Op_Ne,		/* .NE. /=   not-equal-op	      */
		Tok_Op_Lg,		/* .LG. <>   lg_op		      */

		Tok_Op_And,		/* .AND. .A. and-op		      */
		Tok_Op_Eqv,		/* .EQV.     equivalence-op	      */
		Tok_Op_Neqv,		/* .NEQV.    non-equivalence-op	      */
					/* .XOR. .X. non-equivalence-op	      */
		Tok_Op_Not,		/* .NOT. .N. negation-op	      */
		Tok_Op_Or,		/* .OR.	 .O. or-op		      */

		Tok_Op_Assign,		/* =	     assignment-op,	      */
					/*	     generic-spec-assign-op,  */
					/*	     implied-do-control-op    */
		Tok_Op_Defined,		/* .letters. defined-op		      */
		Tok_Op_Deref,		/* %	     struct-comp-derefer-op   */
		Tok_Op_Ptr_Assign,	/* =>	     pointer-assignment-op    */

		Tok_Unknown,		/* ?	     unknown symbol	      */
		Tok_EOS,		/* EOS ;     end-of-statement	      */
		Tok_EOF,		/* EOF	     end-of-file - end parse  */

		Tok_LAST };		/* THIS MUST BE THE LAST ENUM ENTRY   */

/******************************************************************************/

typedef enum	ch_class_values		ch_class_type;
typedef enum	token_class_values	token_class_type;
typedef enum	token_values		token_values_type;

typedef struct	la_entry		la_type;
typedef union	token_str_entry		token_str_type;
typedef struct	token_entry		token_type;

/******************************************************************************/

struct	la_entry	{char			value;
			 ch_class_type		ch_class;
			 int			line;
			 int			column;
			 int			stmt_buf_idx;
			 int			stmt_num; };

/* NOTE:  the id_str member of the following struct must be the first    */
/*	  member so that it is word aligned for symbol table searches    */
/*        id_str_type is declared in globals.h                           */

struct	token_entry	{id_str_type		token_str;
			 int			token_len;
                         boolean		token_err;
			 token_values_type	value;
			 char			kind_str[MAX_ID_LEN+1];
			 int			kind_len;
			 int			line;
			 int			column;
			 int			stmt_buf_idx;
			 int			stmt_num; };

/*********************************\
|* lex and parser interface data *|
\*********************************/

extern	la_type		la_ch; 
extern	token_type	token;

/************************************************\
|* lex and parser interface function prototypes *|
\************************************************/

extern	boolean		get_token (token_class_type);
extern	token_type      initial_token;

