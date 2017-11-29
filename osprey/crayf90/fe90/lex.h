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



/* USMID:  "\n@(#)5.0_pl/headers/lex.h	5.3	08/30/99 11:18:28\n" */

/********************************************\
|* type specifiers used within this module. *|
\********************************************/

typedef struct	alt_kwd_entry	alt_kwd_type;
typedef struct	kwd_entry	kwd_type;

struct	alt_kwd_entry  {char		       *name;
			int			len;
			token_values_type	value;
			int			val_len; };

struct	kwd_entry      {char		       *name;
			token_values_type	value; };

union aligned_value_entry	{
				long double	d;
				long_type	v[MAX_WORDS_FOR_NUMERIC];
				};

typedef union aligned_value_entry aligned_value_type;


/****************************************\
|* static data used within this module. *|
\****************************************/


static  char   			const_buf[MAX_CHAR_CONST_LEN];

token_type		initial_token;

/******************************************************************************\
|*                    GENERAL RULE FOR THESE STRING TABLES                    *|
|*                                                                            *|
|*   All strings are grouped by the beginning letter.  In this group, they    *|
|* are ordered longest to shortest.  That means FALSE comes before F.         *|
|*                                                                            *|
|*   The idx table is indexed by a zero based alphabet.  That is, the index = *|
|* character - 'A'.  If a letter has no corresponding keyword, it has the     *|
|* index of the next letter that has keywords.                                *|
|*                                                                            *|
\******************************************************************************/


static	int			*dot_op_len;

static	kwd_type		dot_op[] = {
				"AND",		     Tok_Op_And,
				"A",		     Tok_Op_And,

				"EQV",		     Tok_Op_Eqv,
				"EQ",		     Tok_Op_Eq,

				"FALSE",	     Tok_Const_False,
				"F",		     Tok_Const_False,

				"GE",		     Tok_Op_Ge,
				"GT",		     Tok_Op_Gt,

				"LE",		     Tok_Op_Le,
				"LG",		     Tok_Op_Lg,
				"LT",		     Tok_Op_Lt,

				"NEQV",		     Tok_Op_Neqv,
				"NOT",		     Tok_Op_Not,
				"NE",		     Tok_Op_Ne,
				"N",		     Tok_Op_Not,

				"OR",		     Tok_Op_Or,
				"O",		     Tok_Op_Or,
  
				"TRUE",		     Tok_Const_True,
				"T",		     Tok_Const_True,

				"XOR",		     Tok_Op_Neqv,
				"X",		     Tok_Op_Neqv,

				"",		     Tok_LAST };

static	int			dot_op_idx[27];

/******************************************************************************\
|*                    GENERAL RULES FOR THESE STRING TABLES                   *|
|*                                                                            *|
|*   All strings are grouped by the beginning letter.  In this group,         *|
|* they are ordered longest to shortest.  That means ALLOCATABLE comes before *|
|* ASSIGN.                                                                    *|
|*                                                                            *|
|*   The idx table is indexed by a zero based alphabet.  That is, the index = *|
|* character - 'A'.  If a letter has no corresponding keyword, it has the     *|
|* index of the next letter that has keywords.                                *|
|*                                                                            *|
\******************************************************************************/

static 	int			*kwd_len;

static	kwd_type		kwd[] = {
				"ALLOCATABLE",	     Tok_Kwd_Allocatable,
				"ASSIGNMENT",	     Tok_Kwd_Assignment,
				"AUTOMATIC",	     Tok_Kwd_Automatic,
				"ALLOCATE",	     Tok_Kwd_Allocate,
				"ASSIGN",	     Tok_Kwd_Assign,

				"BACKSPACE",	     Tok_Kwd_Backspace,
				"BUFFER",	     Tok_Kwd_Buffer,
				"BLOCK",	     Tok_Kwd_Block,
#ifdef KEY /* Bug 10572 */
				"BIND",		     Tok_Kwd_Bind,
#endif /* KEY Bug 10572 */
				"BYTE",		     Tok_Kwd_Byte,

				"CHARACTER",	     Tok_Kwd_Character,
				"CONTAINS",	     Tok_Kwd_Contains,
				"CONTINUE",	     Tok_Kwd_Continue,
				"COMPLEX",	     Tok_Kwd_Complex,
				"COMMON",	     Tok_Kwd_Common,
				"CLOSE",	     Tok_Kwd_Close,
				"CYCLE",	     Tok_Kwd_Cycle,
				"CALL",		     Tok_Kwd_Call,
				"CASE",		     Tok_Kwd_Case,

				"DEALLOCATE",	     Tok_Kwd_Deallocate,
				"DIMENSION",	     Tok_Kwd_Dimension,
				"DEFAULT",	     Tok_Kwd_Default,
				"DECODE",	     Tok_Kwd_Decode,
				"DOUBLE",	     Tok_Kwd_Double,
				"DATA",		     Tok_Kwd_Data,
				"DO",		     Tok_Kwd_Do,

				"EQUIVALENCE",	     Tok_Kwd_Equivalence,
#ifdef KEY /* Bug 10572 */
				"ENUMERATOR",	     Tok_Kwd_Enumerator,
#endif /* KEY Bug 10572 */
				"ELEMENTAL",	     Tok_Kwd_Elemental,
				"EXTERNAL",	     Tok_Kwd_External,
				"ENCODE",	     Tok_Kwd_Encode,
				"ENTRY",	     Tok_Kwd_Entry,
				"ELSE",		     Tok_Kwd_Else,
#ifdef KEY /* Bug 10572 */
				"ENUM",		     Tok_Kwd_Enum,
#endif /* KEY Bug 10572 */
				"EXIT",		     Tok_Kwd_Exit,
				"END",		     Tok_Kwd_End,

				"FUNCTION",	     Tok_Kwd_Function,
				"FORALL",	     Tok_Kwd_Forall,
				"FORMAT",	     Tok_Kwd_Format,
				"FILE",		     Tok_Kwd_File,

				"GO",		     Tok_Kwd_Go,

				"INTERFACE",	     Tok_Kwd_Interface,
				"INTRINSIC",	     Tok_Kwd_Intrinsic,
				"IMPLICIT",	     Tok_Kwd_Implicit,
				"INQUIRE",	     Tok_Kwd_Inquire,
				"INTEGER",	     Tok_Kwd_Integer,
#ifdef KEY /* Bug 11741 */
				"IMPORT",	     Tok_Kwd_Import,
#endif /* KEY Bug 11741 */
				"INTENT",	     Tok_Kwd_Intent,
				"IF",		     Tok_Kwd_If,
				"IN",		     Tok_Kwd_In,

				"KIND",		     Tok_Kwd_Kind,

				"LOGICAL",	     Tok_Kwd_Logical,
				"LEN",		     Tok_Kwd_Len,

				"MODULE",	     Tok_Kwd_Module,

#ifdef KEY /* Bug 5089 */
				"NON_INTRINSIC",     Tok_Kwd_Nonintrinsic,
#endif /* KEY Bug 5089 */
				"NAMELIST",	     Tok_Kwd_Namelist,
				"NULLIFY",	     Tok_Kwd_Nullify,
#ifdef KEY /* Bug 10572 */
				"NAME",		     Tok_Kwd_Name,
#endif /* KEY Bug 10572 */
				"NONE",		     Tok_Kwd_None,

				"OPERATOR",	     Tok_Kwd_Operator,
				"OPTIONAL",	     Tok_Kwd_Optional,
				"ONLY",		     Tok_Kwd_Only,
				"OPEN",		     Tok_Kwd_Open,
				"OUT",		     Tok_Kwd_Out,

				"PARAMETER",	     Tok_Kwd_Parameter,
				"PRECISION",	     Tok_Kwd_Precision,
				"PROCEDURE",	     Tok_Kwd_Procedure,
				"POINTER",	     Tok_Kwd_Pointer,
				"PRIVATE",	     Tok_Kwd_Private,
				"PROGRAM",	     Tok_Kwd_Program,
				"PUBLIC",	     Tok_Kwd_Public,
				"PAUSE",	     Tok_Kwd_Pause,
				"PRINT",	     Tok_Kwd_Print,
				"PURE",		     Tok_Kwd_Pure,

				"RECURSIVE",	     Tok_Kwd_Recursive,
				"RESULT",	     Tok_Kwd_Result,
				"RETURN",	     Tok_Kwd_Return,
				"REWIND",	     Tok_Kwd_Rewind,
				"READ",		     Tok_Kwd_Read,
				"REAL",		     Tok_Kwd_Real,

				"SUBROUTINE",	     Tok_Kwd_Subroutine,
				"SEQUENCE",	     Tok_Kwd_Sequence,
				"SELECT",	     Tok_Kwd_Select,
				"STATIC",	     Tok_Kwd_Static,
				"SAVE",		     Tok_Kwd_Save,
				"STAT",		     Tok_Kwd_Stat,
				"STOP",		     Tok_Kwd_Stop,
				"SPAN",		     Tok_Kwd_Span,
 
				"TARGET",	     Tok_Kwd_Target,
                                "TASK",              Tok_Kwd_Task,
				"THEN",		     Tok_Kwd_Then,
				"TYPE",		     Tok_Kwd_Type,
				"TO",		     Tok_Kwd_To,

				"UNDEFINED",	     Tok_Kwd_Undefined,
				"USE",		     Tok_Kwd_Use,

				"VOLATILE",	     Tok_Kwd_Volatile,
#ifdef KEY /* Bug 14150 */
				"VALUE",	     Tok_Kwd_Value,
#endif /* KEY Bug 14150 */
 
				"WHERE",	     Tok_Kwd_Where,
				"WHILE",	     Tok_Kwd_While,
				"WRITE",	     Tok_Kwd_Write,

				"",		     Tok_LAST };

static	int			kwd_idx[27];

/******************************************************************************\
|*                   GENERAL RULES FOR THESE STRING TABLES                    *|
|*                                                                            *|
|*   All strings are grouped by the beginning letter.  In this group,         *|
|* they are ordered longest to shortest.  That means ELSEWHERE comes before   *|
|* ENDFILE.                                                                   *|
|*                                                                            *|
|*   The idx table is indexed by a zero based alphabet.  That is, the index = *|
|* character - 'A'.  If a letter has no corresponding keyword, it has the     *|
|* index of the next letter that has keywords.                                *|
|*                                                                            *|
\******************************************************************************/

/* THIS TABLE IS NOT HANDLED THE SAME AS THE OTHERS !!!!!!!!!!!!!!!!! */
/* PAY ATTENTION  */
/* Note: additions/deletions to this table must be reflected in alt_kwd_idx.  */

static	alt_kwd_type		alt_kwd[] = {

                                "BLOCKDATA",     9,  Tok_Kwd_Block,     5,

                                "DOUBLEPRECISION",
                                                15,  Tok_Kwd_Double,    6,

                                "ENDSUBROUTINE",13,  Tok_Kwd_End,       3,
                                "ENDBLOCKDATA", 12,  Tok_Kwd_End,       3,
                                "ENDINTERFACE", 12,  Tok_Kwd_End,       3,
                                "ENDFUNCTION",  11,  Tok_Kwd_End,       3,
                                "ENDPROGRAM",   10,  Tok_Kwd_End,       3,
                                "ENDMODULE",     9,  Tok_Kwd_End,       3,
                                "ENDFORALL",     9,  Tok_Kwd_End,       3,
                                "ENDSELECT",     9,  Tok_Kwd_End,       3,
                                "ELSEWHERE",     9,  Tok_Kwd_Else,      4,
                                "ENDWHERE",      8,  Tok_Kwd_End,       3,
                                "ENDBLOCK",      8,  Tok_Kwd_End,       3,
                                "ENDFILE",       7,  Tok_Kwd_End,       3,
                                "ENDTYPE",       7,  Tok_Kwd_End,       3,
                                "ELSEIF",        6,  Tok_Kwd_Else,      4,
                                "ENDDO",         5,  Tok_Kwd_End,       3,
                                "ENDIF",         5,  Tok_Kwd_End,       3,

                                "GOTO",          4,  Tok_Kwd_Go,        2,

                                "INOUT",         5,  Tok_Kwd_In,        2,

                                "SELECTCASE",   10,  Tok_Kwd_Select,    6 };

static	int			alt_kwd_idx[] = {
				  0,   0,   1,	 1,   2,  18,  18,     /* A-G */
				 19,  19,  20,	20,  20,  20,  20,     /* H-N */
				 20,  20,  20,	20,  20,  21,  21,     /* O-U */
				 21,  21,  21,	21,  21,	       /* V-Z */
				 21 };				       /* end */


/******************************************************************************\
|*                    GENERAL RULES FOR THESE STRING TABLES                   *|
|*                                                                            *|
|*   All strings are grouped by the beginning letter.  In this group, they    *|
|* are ordered longest to shortest.  That means BOUNDS comes before BL.       *|
|*                                                                            *|
|*   The idx table is indexed by a zero based alphabet.  That is, the index = *|
|* character - 'A'.  If a letter has no corresponding keyword, it has the     *|
|* index of the next letter that has keywords.                                *|
|*                                                                            *|
|*                    * * * * *   ATTENTION   * * * * *           	      *|
|*                                                                            *|
|* Additions/deletions to this table must be reflected in kwd_dir_idx below   *|
|* and in procedure cif_misc_compiler_opts_rec in fecif.c		      *|
|*                                                                            *|
\******************************************************************************/

static	int		*kwd_dir_len;

static	kwd_type	kwd_dir[] = {

			"ATOMICUPDATE",		   	Tok_Dir_Atomicupdate,
			"AUTOSCOPE",		   	Tok_Dir_Autoscope,
			"AUXILIARY",		   	Tok_Dir_Auxiliary,
			"ALIGN",		   	Tok_Dir_Align,

			"BLOCKINGSIZE",		   	Tok_Dir_Blockingsize,
			"BLOCKABLE",		   	Tok_Dir_Blockable,
			"BARRIER",		   	Tok_Dir_Barrier,
			"BOUNDS",		   	Tok_Dir_Bounds,
			"BLOCK",		   	Tok_Dir_Block,
			"BL",			   	Tok_Dir_Bl,

                        "COPY_ASSUMED_SHAPE",       Tok_Dir_Copy_Assumed_Shape,
			"CACHE_NOALLOCATE",		Tok_Dir_Cache_Noalloc,
			"CACHE_BYPASS",		   	Tok_Dir_Cache_Bypass,
			"CACHE_ALIGN",		   	Tok_Dir_Cache_Align,
			"CONCURRENT",		   	Tok_Dir_Concurrent,
			"CHUNKSIZE",		   	Tok_Dir_Chunksize,
			"CRITICAL",		   	Tok_Dir_Critical,
			"CONTROL",		   	Tok_Dir_Control,
			"CNCALL",			Tok_Dir_Cncall,
			"COMMON",		   	Tok_Dir_Common,
			"CODE",			   	Tok_Dir_Code,

			"DOSHARED",		  	Tok_Dir_Doshared,
			"DYNAMIC",		  	Tok_Dir_Dynamic,
 
			"ENDCRITICAL",		   	Tok_Dir_Endcritical,
			"ENDMASTER",		   	Tok_Dir_Endmaster,
			"EJECT",		   	Tok_Dir_Eject,

			"FIXED",		   	Tok_Dir_Fixed,
			"FLOW",			   	Tok_Dir_Flow,
			"FREE",			   	Tok_Dir_Free,

			"GEOMETRY",		   	Tok_Dir_Geometry,
			"GETFIRST",		   	Tok_Dir_Getfirst,
			"GUIDED",		   	Tok_Dir_Guided,

			"INLINEALWAYS",		   	Tok_Dir_Inline_Always,
			"INLINENEVER",		   	Tok_Dir_Inline_Never,
			"INTERCHANGE",			Tok_Dir_Interchange,
			"IGNORE_TKR",			Tok_Dir_Ignore_TKR,
			"INTEGER",		   	Tok_Dir_Integer,
			"INLINE",		   	Tok_Dir_Inline,
			"IVDEP",		   	Tok_Dir_Ivdep,
			"ID",			   	Tok_Dir_Id,
			"IF",			   	Tok_Dir_If,

			"LIST",			   	Tok_Dir_List,

			"MODINLINE",		   	Tok_Dir_Modinline,
			"MAXCPUS",		   	Tok_Dir_Maxcpus,
			"MASTER",		   	Tok_Dir_Master,
			"MARK",			   	Tok_Dir_Mark,

			"NOSIDEEFFECTS",	   	Tok_Dir_Nosideeffects,
			"NOINTERCHANGE",	   	Tok_Dir_Nointerchange,
			"NORECURRENCE",		   	Tok_Dir_Norecurrence,
			"NCPUS_CHUNKS",		   	Tok_Dir_Ncpus_Chunks,
			"NOMODINLINE",		   	Tok_Dir_Nomodinline,
			"NEXTSCALAR",		   	Tok_Dir_Nextscalar,
			"NOBLOCKING",		   	Tok_Dir_Noblocking,
			"NOBARRIER",		   	Tok_Dir_Nobarrier,
			"NOPATTERN",		   	Tok_Dir_Nopattern,
			"NOVSEARCH",		   	Tok_Dir_Novsearch,
			"NUMCHUNKS",		   	Tok_Dir_Numchunks,
			"NOBOUNDS",		   	Tok_Dir_Nobounds,
			"NOINLINE",		   	Tok_Dir_Noinline,
			"NOVECTOR",		   	Tok_Dir_Novector,
			"NOSTREAM",			Tok_Dir_Nostream,
			"NOUNROLL",		   	Tok_Dir_Nounroll,
			"NOSPLIT",		   	Tok_Dir_Nosplit,
                        "NUMCPUS",                      Tok_Dir_Numcpus,
			"NOCINV",		   	Tok_Dir_Nocinv,
			"NOCODE",		   	Tok_Dir_Nocode,
			"NOFLOW",		   	Tok_Dir_Noflow,
			"NOLIST",		   	Tok_Dir_Nolist,
			"NOMARK",		   	Tok_Dir_Nomark,
			"NOTASK",		   	Tok_Dir_Notask,
			"NAME",			   	Tok_Dir_Name,
			"NOBL",			   	Tok_Dir_Nobl,

			"PARALLEL_ONLY",	   	Tok_Dir_Parallel_Only,
			"PREFERVECTOR",		   	Tok_Dir_Prefervector,
			"PREFERSTREAM",		   	Tok_Dir_Preferstream,
			"PE_RESIDENT",		   	Tok_Dir_Pe_Resident,
                        "PERMUTATION",                  Tok_Dir_Permutation,
			"PREFERTASK",		   	Tok_Dir_Prefertask,
			"PE_PRIVATE",		   	Tok_Dir_Pe_Private,
			"PATTERN",		   	Tok_Dir_Pattern,
			"PRIVATE",		   	Tok_Dir_Private,

			"RECURRENCE",		   	Tok_Dir_Recurrence,
			"REGFILE",		   	Tok_Dir_Regfile,

			"SYSTEM_MODULE",	   	Tok_Dir_System_Module,
			"SHORTSEQUENCE",	   	Tok_Dir_Shortsequence,
			"SERIAL_ONLY",		   	Tok_Dir_Serial_Only,
			"SEMEXTERN",		   	Tok_Dir_Semextern,
			"SHORTLOOP",		   	Tok_Dir_Shortloop,
			"SYMMETRIC",		   	Tok_Dir_Symmetric,
			"SUPPRESS",		   	Tok_Dir_Suppress,
			"SAVELAST",		   	Tok_Dir_Savelast,
			"STREAM",		   	Tok_Dir_Stream,
			"STATIC",		   	Tok_Dir_Static,
			"SHARED",		   	Tok_Dir_Shared,
			"SINGLE",		   	Tok_Dir_Single,
			"SPLIT",		   	Tok_Dir_Split,
			"STACK",		   	Tok_Dir_Stack,

			"TASKCOMMON",		   	Tok_Dir_Taskcommon,
			"TASKHEAD",		   	Tok_Dir_Taskhead,
			"TASK",			   	Tok_Dir_Task,

			"UNKNOWN_SHARED",	   	Tok_Dir_Unknown_Shared,
			"USES_EREGS",		   	Tok_Dir_Uses_Eregs,
			"UNKNOWN",		   	Tok_Dir_Unknown,
			"UNROLL",		   	Tok_Dir_Unroll,

			"VFUNCTION",		   	Tok_Dir_Vfunction,
			"VSEARCH",		   	Tok_Dir_Vsearch,
			"VECTOR",		   	Tok_Dir_Vector,

			"",		 		Tok_LAST };

static	int			kwd_dir_idx[27];

/**************************************************************************\
|*                 GENERAL RULE FOR THESE STRING TABLES                   *|
|*                                                                        *|
|*   All strings are grouped by the beginning letter. In this group,      *|
|* they are ordered longest to shortest. That means CONTINUE comes before *|
|* CASE.                                                                  *|
|*                                                                        *|
|*   The idx table is indexed by a zero based alphabet. That is, the      *|
|* index = character - 'A'. If a letter has no corresponding keyword,     *|
|* it has the index of the next letter that has keywords.                 *|
|*                                                                        *|
\**************************************************************************/

static	int			*kwd_mic_len;

static	kwd_type		kwd_mic[] = {
				"CONTINUE",          Tok_Mic_Continue,
				"CNCALL",	     Tok_Mic_Cncall,
				"CASE",		     Tok_Mic_Case,

				"DOPARALLEL",        Tok_Mic_Do_Parallel,
				"DOALL",             Tok_Mic_Do_All,

				"ENDPARALLEL",       Tok_Mic_End_Parallel,
				"ENDGUARD",          Tok_Mic_End_Guard,
				"ENDCASE",           Tok_Mic_End_Case,
				"ENDDO",             Tok_Mic_End_Do,

				"GUARD",             Tok_Mic_Guard,

				"IF",	             Tok_Mic_If,

				"MAXCPUS",	     Tok_Mic_Maxcpus,

				"NUMCPUS",	     Tok_Mic_Numcpus,

				"PERMUTATION",       Tok_Mic_Permutation,
				"PARALLEL",          Tok_Mic_Parallel,
				"POINT",	     Tok_Mic_Point,

				"SEND",              Tok_Mic_Send,
				"SPAN",		     Tok_Mic_Span,

				"TASKCOMMON",        Tok_Mic_Taskcommon,

				"WAIT",              Tok_Mic_Wait,

				"",		     Tok_LAST };

static	int			kwd_mic_idx[27];

/**************************************************************************\
|*                 GENERAL RULE FOR THESE STRING TABLES                   *|
|*                                                                        *|
|*   All strings are grouped by the beginning letter. In this group,      *|
|* they are ordered longest to shortest. That means CONTINUE comes before *|
|* CASE.                                                                  *|
|*                                                                        *|
|*   The idx table is indexed by a zero based alphabet. That is, the      *|
|* index = character - 'A'. If a letter has no corresponding keyword,     *|
|* it has the index of the next letter that has keywords.                 *|
|*                                                                        *|
\**************************************************************************/

static	int		*kwd_sgi_dir_len;

static  kwd_type	kwd_sgi_dir[] = {

	"AGGRESSIVEINNERLOOPFISSION",	  Tok_SGI_Dir_Aggressiveinner,
	"ARGUMENTALIASING",		  Tok_SGI_Dir_Argumentaliasing,
	"ALIGN_SYMBOL",			  Tok_SGI_Dir_Align_Symbol,
	"AFFINITY",			  Tok_SGI_Dir_Affinity,
	"ASSERT",			  Tok_SGI_Dir_Assert,

	"BOUNDSVIOLATIONS",		  Tok_SGI_Dir_Boundsviolations,
	"BLOCKINGSIZE",			  Tok_SGI_Dir_Blockingsize,
	"BLOCKABLE",			  Tok_SGI_Dir_Blockable,
	"BARRIER",			  Tok_SGI_Dir_Barrier,
	"BLOCKED",			  Tok_SGI_Dir_Blocked,
	"BENIGN",			  Tok_SGI_Dir_Benign,
	"BLOCK",			  Tok_SGI_Dir_Block,

	"CRITICALSECTION",		  Tok_SGI_Dir_Criticalsection,
	"CONCURRENTCALL",		  Tok_SGI_Dir_Concurrentcall,
	"CONCURRENTIZE",		  Tok_SGI_Dir_Concurrentize,
	"CONCURRENT",		  	  Tok_SGI_Dir_Concurrent,
	"CONCUR",		  	  Tok_SGI_Dir_Concur,
	"CYCLIC",			  Tok_SGI_Dir_Cyclic,
	"COPYIN",			  Tok_SGI_Dir_Copyin,
	"CHUNK",			  Tok_SGI_Dir_Chunk,

	"DISTRIBUTE_RESHAPE",		  Tok_SGI_Dir_Distribute_Reshape,
	"DISTRIBUTE",			  Tok_SGI_Dir_Distribute,
	"DEPENDENCE",			  Tok_SGI_Dir_Dependence,
	"DOPREFER",			  Tok_SGI_Dir_Doprefer,
	"DOACROSS",			  Tok_SGI_Dir_Doacross,
	"DYNAMIC",			  Tok_SGI_Dir_Dynamic,
	"DATA",		 		  Tok_SGI_Dir_Data,
	"DO",				  Tok_SGI_Dir_Do,

	"ENDCRITICALSECTION",		  Tok_SGI_Dir_Endcriticalsection,
	"EQUIVALENCEHAZARD",		  Tok_SGI_Dir_Equivalencehazard,
	"ENDSINGLEPROCESS",		  Tok_SGI_Dir_Endsingleprocess,
	"ENDPSECTIONS",			  Tok_SGI_Dir_Endpsections,
	"ENDPARALLEL",			  Tok_SGI_Dir_Endparallel,
	"ENDPSECTION",			  Tok_SGI_Dir_Endpsection,
	"ENDPDO",			  Tok_SGI_Dir_Endpdo,

	"FILL_SYMBOL",			  Tok_SGI_Dir_Fill_Symbol,
	"FISSIONABLE",			  Tok_SGI_Dir_Fissionable,
	"FREQUENCY",			  Tok_SGI_Dir_Frequency,
	"FISSION",	 		  Tok_SGI_Dir_Fission,
	"FUSABLE",	 		  Tok_SGI_Dir_Fusable,
	"FLUSH",	 		  Tok_SGI_Dir_Flush,
	"FUSE",		 		  Tok_SGI_Dir_Fuse,

	"GUIDED",	 		  Tok_SGI_Dir_Guided,
	"GLOBAL",	 		  Tok_SGI_Dir_Global,
	"GSS",	 	 		  Tok_SGI_Dir_Gss,

	"HERE",		 		  Tok_SGI_Dir_Here,

	"IGNOREASSUMEDDEPENDENCES",	  Tok_SGI_Dir_Ignoreassumeddependences,
	"IGNOREASSUMEDDEPENDENCE",	  Tok_SGI_Dir_Ignoreassumeddependence,
	"IGNOREANYDEPENDENCES",		  Tok_SGI_Dir_Ignoreanydependences,
	"IGNOREANYDEPENDENCE",		  Tok_SGI_Dir_Ignoreanydependence,
	"INTERLEAVED",			  Tok_SGI_Dir_Interleaved,
	"INTERCHANGE",			  Tok_SGI_Dir_Interchange,
	"INTERLEAVE",			  Tok_SGI_Dir_Interleave,
	"INLINE",	 		  Tok_SGI_Dir_Inline,
	"IPA",		 		  Tok_SGI_Dir_Ipa,
	"IF",		 		  Tok_SGI_Dir_If,

	"KIND",		 		  Tok_SGI_Dir_Kind,

	"LASTVALUESNEEDED",		  Tok_SGI_Dir_Lastvaluesneeded,
	"LASTVALUENEEDED",		  Tok_SGI_Dir_Lastvalueneeded,
	"L1CACHELINE",			  Tok_SGI_Dir_L1cacheline,
	"L2CACHELINE",			  Tok_SGI_Dir_L2cacheline,
	"LASTTHREAD",			  Tok_SGI_Dir_Lastthread,
	"LASTLOCAL",	 		  Tok_SGI_Dir_Lastlocal,
	"LOCAL",	 		  Tok_SGI_Dir_Local,
	"LIMIT",	 		  Tok_SGI_Dir_Limit,
	"LEVEL",	 		  Tok_SGI_Dir_Level,

	"MINCONCURRENT",		  Tok_SGI_Dir_Minconcurrent,
	"MP_SCHEDTYPE",			  Tok_SGI_Dir_Mp_Schedtype,

	"NOTEMPORARIESFORCONSTANTARGUMEN",
                                 Tok_SGI_Dir_Notemporariesforconstantarguments,
	"NOEQUIVALENCEHAZARD",		  Tok_SGI_Dir_Noequivalencehazard,
	"NOBOUNDSVIOLATIONS",		  Tok_SGI_Dir_Noboundsviolations,
	"NOARGUMENTALIASING", 		  Tok_SGI_Dir_Noargumentaliasing,
	"NOLASTVALUESNEEDED",		  Tok_SGI_Dir_Nolastvaluesneeded,
	"NOLASTVALUENEEDED",		  Tok_SGI_Dir_Nolastvalueneeded,
	"NOCONCURRENTCALL",		  Tok_SGI_Dir_Noconcurrentcall,
	"NOCONCURRENTIZE",		  Tok_SGI_Dir_Noconcurrentize,
	"NOINTERCHANGE",		  Tok_SGI_Dir_Nointerchange,
	"NORECURRENCE",			  Tok_SGI_Dir_Norecurrence,
	"NCPUS_CHUNK",			  Tok_SGI_Dir_Ncpus_Chunk,
	"NOBLOCKING",			  Tok_SGI_Dir_Noblocking,
	"NOFISSION",	 		  Tok_SGI_Dir_Nofission,
	"NUMCHUNKS",			  Tok_SGI_Dir_Numchunks,
	"NOFUSION",	 		  Tok_SGI_Dir_Nofusion,
	"NOINLINE",	 		  Tok_SGI_Dir_Noinline,
	"NOSYNC",			  Tok_SGI_Dir_Nosync,
	"NOWAIT",		 	  Tok_SGI_Dir_Nowait,
	"NOIPA",	 		  Tok_SGI_Dir_Noipa,
	"NEST",			 	  Tok_SGI_Dir_Nest,

	"OPTIONAL",	 		  Tok_SGI_Dir_Optional,
#ifdef KEY /* Bug 2660 */
	"OPTIONS",	 		  Tok_SGI_Dir_Options,
#endif /* KEY Bug 2660 */
	"ORDERED",	 		  Tok_SGI_Dir_Ordered,
	"OPAQUE",	 		  Tok_SGI_Dir_Opaque,
	"ONTO",		 		  Tok_SGI_Dir_Onto,

	"PREFETCH_REF_DISABLE", 	  Tok_SGI_Dir_Prefetch_Ref_Disable,
	"PREFETCH_MANUAL",		  Tok_SGI_Dir_Prefetch_Manual,
	"PREFETCH_REF", 		  Tok_SGI_Dir_Prefetch_Ref,
	"PERMUTATION",			  Tok_SGI_Dir_Permutation,
	"PAGE_PLACE",   		  Tok_SGI_Dir_Page_Place,
	"PARALLELDO",			  Tok_SGI_Dir_Paralleldo,
	"PSECTIONS",			  Tok_SGI_Dir_Psections,
	"PARALLEL",			  Tok_SGI_Dir_Parallel,
	"PREFETCH",	 		  Tok_SGI_Dir_Prefetch,
	"PSECTION",			  Tok_SGI_Dir_Psection,
	"PRIVATE",			  Tok_SGI_Dir_Private,
	"PAGE",				  Tok_SGI_Dir_Page,
	"PDO",		 		  Tok_SGI_Dir_Pdo,

	"REDISTRIBUTE",			  Tok_SGI_Dir_Redistribute,
	"REGIONBEGIN",			  Tok_SGI_Dir_Regionbegin,
	"REDUCTION",	 		  Tok_SGI_Dir_Reduction,
	"REGIONEND",	 		  Tok_SGI_Dir_Regionend,
	"RELATION",			  Tok_SGI_Dir_Relation,
	"RUNTIME",	 		  Tok_SGI_Dir_Runtime,
	"ROUTINE",	 		  Tok_SGI_Dir_Routine,

	"SECTION_NON_GP",		  Tok_SGI_Dir_Section_Non_Gp,
	"SINGLEPROCESS",		  Tok_SGI_Dir_Singleprocess,
	"SECTION_GP",			  Tok_SGI_Dir_Section_Gp,
	"SECTION",		 	  Tok_SGI_Dir_Section,
	"SERIAL",		 	  Tok_SGI_Dir_Serial,
	"SHARED",		 	  Tok_SGI_Dir_Shared,
	"SINGLE",		 	  Tok_SGI_Dir_Single,
	"STATIC",	 		  Tok_SGI_Dir_Static,
	"STRIDE",	 		  Tok_SGI_Dir_Stride,
	"SIMPLE",	 		  Tok_SGI_Dir_Simple,
	"SHARE",		 	  Tok_SGI_Dir_Share,
	"SIZE",		 		  Tok_SGI_Dir_Size,

	"TEMPORARIESFORCONSTANTARGUMENTS",
                                  Tok_SGI_Dir_Temporariesforconstantarguments,
	"THREAD",	 		  Tok_SGI_Dir_Thread,
	"TILE",	 		  	  Tok_SGI_Dir_Tile,

	"USECONTROLLEDSTORE", 		  Tok_SGI_Dir_Usecontrolledstore,
	"USECOMPRESS",			  Tok_SGI_Dir_Usecompress,
	"USESCATTER",			  Tok_SGI_Dir_Usescatter,
	"USEEXPAND",			  Tok_SGI_Dir_Useexpand,
	"USEGATHER",			  Tok_SGI_Dir_Usegather,
	"UNROLL",	 		  Tok_SGI_Dir_Unroll,

	"VECTOR",		 	  Tok_SGI_Dir_Vector,

	"",				  Tok_LAST };

static  int                     kwd_sgi_dir_idx[27];

/**************************************************************************\
|*                 GENERAL RULE FOR THESE STRING TABLES                   *|
|*                                                                        *|
|*   All strings are grouped by the beginning letter. In this group,      *|
|* they are ordered longest to shortest. That means CONTINUE comes before *|
|* CASE.                                                                  *|
|*                                                                        *|
|*   The idx table is indexed by a zero based alphabet. That is, the      *|
|* index = character - 'A'. If a letter has no corresponding keyword,     *|
|* it has the index of the next letter that has keywords.                 *|
|*                                                                        *|
\**************************************************************************/


static  int             *kwd_open_mp_dir_len;

static  kwd_type        kwd_open_mp_dir[] = {
	"AFFINITY",			Tok_Open_Mp_Dir_Affinity,
	"ATOMIC",			Tok_Open_Mp_Dir_Atomic,

	"BARRIER",			Tok_Open_Mp_Dir_Barrier,

	"COPYPRIVATE",		Tok_Open_Mp_Dir_Copyprivate, /* by jhs, 02/7/5 */
	"CRITICAL",			Tok_Open_Mp_Dir_Critical,
	"COPYIN",			Tok_Open_Mp_Dir_Copyin,

	"DISTRIBUTE_RESHAPE",		Tok_Open_Mp_Dir_Distribute_Reshape,
	"DISTRIBUTE",			Tok_Open_Mp_Dir_Distribute,
	"DEFAULT",			Tok_Open_Mp_Dir_Default,
	"DYNAMIC",			Tok_Open_Mp_Dir_Dynamic,
	"DATA",				Tok_Open_Mp_Dir_Data,
	"DO",				Tok_Open_Mp_Dir_Do,

	"ENDPARALLELWORKSHARE",	Tok_Open_Mp_Dir_Endparallelworkshare, /* by jhs, 02/7/5 */
	"ENDPARALLELSECTIONS",		Tok_Open_Mp_Dir_Endparallelsections,
	"ENDPARALLELDO",		Tok_Open_Mp_Dir_Endparalleldo,
	"ENDWORKSHARE",		Tok_Open_Mp_Dir_Endworkshare, /* by jhs, 02/7/5 */
	"ENDCRITICAL",			Tok_Open_Mp_Dir_Endcritical,
	"ENDPARALLEL",			Tok_Open_Mp_Dir_Endparallel,
	"ENDSECTIONS",			Tok_Open_Mp_Dir_Endsections,
	"ENDORDERED",			Tok_Open_Mp_Dir_Endordered,
	"ENDMASTER",			Tok_Open_Mp_Dir_Endmaster,
	"ENDSINGLE",			Tok_Open_Mp_Dir_Endsingle,
	"ENDDO",			Tok_Open_Mp_Dir_Enddo,

	"FIRSTPRIVATE",			Tok_Open_Mp_Dir_Firstprivate,
	"FLUSH",			Tok_Open_Mp_Dir_Flush,

	"GUIDED",			Tok_Open_Mp_Dir_Guided,

	"IF",				Tok_Open_Mp_Dir_If,

	"LASTPRIVATE",			Tok_Open_Mp_Dir_Lastprivate,

	"MASTER",			Tok_Open_Mp_Dir_Master,

	"NUM_THREADS",		Tok_Open_Mp_Dir_Num_Threads, /* by jhs, 02/7/5 */
	"NOWAIT",			Tok_Open_Mp_Dir_Nowait,
	"NEST",				Tok_Open_Mp_Dir_Nest,
	"NONE",				Tok_Open_Mp_Dir_None,

	"ORDERED",			Tok_Open_Mp_Dir_Ordered,
	"ONTO",				Tok_Open_Mp_Dir_Onto,

	"PARALLELWORKSHARE",	Tok_Open_Mp_Dir_Parallelworkshare, /* by jhs, 02/7/5 */
	"PARALLELSECTIONS",		Tok_Open_Mp_Dir_Parallelsections,
	"PAGE_PLACE",   		Tok_Open_Mp_Dir_Page_Place,
	"PARALLELDO",			Tok_Open_Mp_Dir_Paralleldo,
	"PARALLEL",			Tok_Open_Mp_Dir_Parallel,
	"PRIVATE",			Tok_Open_Mp_Dir_Private,

	"REDISTRIBUTE",			Tok_Open_Mp_Dir_Redistribute,
	"REDUCTION",			Tok_Open_Mp_Dir_Reduction,
	"RUNTIME",			Tok_Open_Mp_Dir_Runtime,

	"SCHEDULE",			Tok_Open_Mp_Dir_Schedule,
	"SECTIONS",			Tok_Open_Mp_Dir_Sections,
	"SECTION",			Tok_Open_Mp_Dir_Section,
	"SHARED",			Tok_Open_Mp_Dir_Shared,
	"SINGLE",			Tok_Open_Mp_Dir_Single,
	"STATIC",			Tok_Open_Mp_Dir_Static,

	"THREADPRIVATE",		Tok_Open_Mp_Dir_Threadprivate,
	"THREAD",			Tok_Open_Mp_Dir_Thread,

	"WORKSHARE",		Tok_Open_Mp_Dir_Workshare, /* by jhs, 02/7/5 */
	"",				Tok_LAST };

static  int                     kwd_open_mp_dir_idx[27];


# ifdef _DEBUG
static 	int			*kwd_dbg_len;

static	kwd_type		kwd_dbg[] = {
				"STMT",		     Tok_Dbg_Stmt,
				"SYTB",		     Tok_Dbg_Sytb,
				"",		     Tok_LAST };

static	int			kwd_dbg_idx[27];
# endif

/*************************************\
|* objects referenced by this module *|
\*************************************/
extern boolean  sig_blank;
extern la_type  stmt_EOS_la_ch;

extern int	nxt_line[];

# ifdef _ARITH_H
extern long input_arith_type[Num_Linear_Types];
extern char arith_type_string[Num_Linear_Types][25];
# endif

extern ch_class_type           ch_class[];


static boolean 	havent_issued_ndollarpes_ansi;
