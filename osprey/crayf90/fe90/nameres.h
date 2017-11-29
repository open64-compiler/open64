/*
 * Copyright (C) 2007, 2008. PathScale, LLC. All Rights Reserved.
 */
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



/* USMID:  "\n@(#)5.0_pl/headers/nameres.h	5.1	04/29/99 21:22:31\n" */

/******************************************************************************\
|*									      *|
|*  This module holds tables for semantic checking for declarations and       *|
|*  uses of attrs.  The routine fnd_semantic_err uses these tables to look    *|
|*  for problems and to issue errors.  Tables obj_to_attr, obj_to_name and    *|
|*  obj_to_others are used to detect errors.  There is an enum obj_values     *|
|*  (defined in globals.h).  This enum holds a value for things that can      *|
|*  be added to a symbol table attribute entry.  (ie: Obj_Cri_Pointee         *|
|*  means make this symbol table attribute entry a Cray_Pointee.)  So tbl     *|
|*  obj_to_attr says, can I add this obj_... info to a symbol table entry     *|
|*  that has this attribute?   Table obj_to_name says can I add this          *|
|*  obj_... to a symbol table attribute entry that is a Name_... ?  Table     *|
|*  obj_to_other says can I add this obj_... to a symbol table attribute      *|
|*  entry that already has this Other_... information?  The attributes,       *|
|*  names, and others each have their own enums.  (attr_obj_values	      *|
|*  defined in sytb.h, name_obj_values and other_obj_values defined in        *|
|*  this module.)  There are 2 reasons that there are 3 enums.  1)  It        *|
|*  makes issuing messages easier.  Common messages can be used for           *|
|*  attr to attr, attr to name, and name to attr.  Unique messages only       *|
|*  need to be written for others.  2)  We want common code for the           *|
|*  different machines we run on, so we want to keep the number of 'bits'     *|
|*  per entry less than 32.  Each entry in a obj_to_attr table is a long.     *|
|*  There is one entry for each Obj_.... enum value.  Each entry is a         *|
|*  bunch of bits.  Each bit stands for one of the attr_obj_values enums.     *|
|*  The obj_to_name and obj_to_other tables are set up the same way, and      *|
|*  use enums name_obj_values and other_obj_values, respectively.             *|
|*  The tables attr_msg_num, name_msg_num, and other_msg_num have the         *|
|*  error messages to be printed out for bad combinations.  These are         *|
|*  2 dimensional arrays.  (Arrays of arrays if you're speaking C.)           *|
|*  Each entry is a long and is addressed by [Obj_....][Attr...] for          *|
|*  attr_msg_num table.  name_msg_num and other_msg_num tables work the       *|
|*  same way.  There is DEBUG routine, verify_semantic_tbls, that runs        *|
|*  once each time a DEBUG compiler is executed.  This routine makes sure     *|
|*  that the obj_to_.... and the ...._msg_num tables are in sync.  If a       *|
|*  combination is bad, it better have an error message.  If the combo is     *|
|*  good it better have 0, as an error message.  A fatal error message is     *|
|*  issued for each bad entry in the message table.  Then an internal msg     *|
|*  is issued to stop compilation.					      *|
|* 									      *|
\******************************************************************************/

/****************************************\
|* static data used within this module. *|
\****************************************/

/* These are names that a symbol table entry can be. */

enum    attr_obj_values	       {Attr_Assumed_Type_Ch,	Attr_Dimension,
				Attr_Explicit_Shp_Arr,	Attr_Assumed_Size_Arr,
				Attr_Deferred_Shp_Arr,	Attr_Assumed_Shp_Arr,
				Attr_Allocatable,	Attr_Parameter,
				Attr_Intent,		Attr_Optional,
				Attr_Private,		Attr_Public,
				Attr_Target,		Attr_Equivalence,
				Attr_Save,		Attr_Pointer,
				Attr_External,		Attr_Intrinsic,
				Attr_Data_Init,		Attr_Type,
				Attr_Co_Array,		Attr_Automatic,
				Attr_Volatile,
#ifdef KEY /* Bug 14150 */
				Attr_Bind,		Attr_Value,
#endif /* KEY Bug 14150 */
				Attr_Done };

enum    dir_obj_values	       {Dir_Auxiliary,		Dir_Vfunction,
				Dir_No_Side_Effects,	Dir_Inline,
				Dir_Symmetric,		Dir_Copy_Assumed_Shape,
				Dir_Align_Symbol,	Dir_Fill_Symbol,
				Dir_Section_Gp,		Dir_Section_Non_Gp,
				Dir_Ignore_TKR,		Dir_Optional,
				Dir_Ipa,		Dir_Name,		
				Dir_Done };

enum	name_obj_values	       {Name_Variable,		Name_Common_Obj,
				Name_Cri_Pointer,	Name_Cri_Pointee,
				Name_Cri_Ch_Pointee,	Name_Func_Result,
				Name_Dummy_Arg,
				Name_Module_Proc,	Name_Derived_Type,
				Name_Generic_Interface,	Name_Namelist_Group,
				Name_Namelist_Group_Obj,Name_Statement_Func,
				Name_Construct,		Name_Intrinsic_Func,
				Name_Intrinsic_Subr,	Name_Module,
				Name_Blockdata,		Name_Program,
				Name_Function,		Name_Curr_Func,
				Name_Curr_Subr,		Name_Internal_Func,
				Name_Internal_Subr,	Name_Done };


/* These are other things that a symbol table can be.  For being in the      */
/* Curr_.. or Use_.., fnd_sem_err is smart enough to enter the kind of	     */
/* function or subroutine.  ie:  external function,  module subroutine ect.. */

enum	other_obj_values       {Other_Var_Len_Ch,	Other_Var_Len_Arr,	
				Other_Expl_Interface,
				Other_Use_Func,		Other_Use_Subr,
				Other_Use_Variable,	Other_Use_Dummy_Arg,
				Other_Host_Assoc,	Other_Use_Assoc,
				Other_Use_Char_Rslt,	Other_Not_Visible,
				Other_Npes,		Other_Done
			       };

/* These strings are used to put out error messages about these attributes.   */
/* The enumerations for attrs are found in sytb.h, because they are symbol    */
/* table related, and are kept in the symbol table.			      */
/* Note: Type is treated slightly different, so the error msgs make sense.    */

static  char    *attr_str[Attr_Done] =		{     
			"CHARACTER*(*)",
			"DIMENSION",
			"explicit-shape DIMENSION",
			"assumed-size DIMENSION",
			"deferred-shape DIMENSION",
			"assumed-shape DIMENSION",
			"ALLOCATABLE",
			"PARAMETER",
			"INTENT",
			"OPTIONAL",
			"PRIVATE",
			"PUBLIC",
			"TARGET",
			"EQUIVALENCE",
			"SAVE",
			"POINTER",
			"EXTERNAL",
			"INTRINSIC",
			"DATA initialized",
			"type-spec",		/* This turns into the     */
						/* actual type.  ie:  REAL */
			"co-array DIMENSION",
			"AUTOMATIC",
			"VOLATILE",
#ifdef KEY /* Bug 14150 */
			"BIND",
			"VALUE"
#endif /* KEY Bug 14150 */
			};

static	char	*dir_str[Dir_Done]	=	{
			"AUXILIARY",
			"VFUNCTION",
			"NO SIDE EFFECTS",
			"INLINE",
			"SYMMETRIC",
			"COPY_ASSUMED_SHAPE",
			"ALIGN_SYMBOL",
			"FILL_SYMBOL",
			"SECTION_GP",
			"SECTION_NON_GP",
			"IGNORE_TKR",
			"OPTIONAL",
			"IPA",
			"NAME",
			 };

/* These strings are used to put out error messages about these names.	*/

static	char	*name_str[Name_Done] =		{
			"variable",		 "common-block-object",
			"Cray pointer",		 "Cray pointee",
			"Cray character pointee","result-name",
			"dummy-argument",	 "module-subprogram",
			"type-name",		 "generic-name",
			"namelist-group-name",	 "namelist-group-object",
			"statement function",	 "construct-name",
			"intrinsic function",	 "intrinsic subroutine",
			"module",		 "block-data",
			"program-name",		 "function",
			"function",		 "subroutine",
			"internal function",	 "internal subroutine" };


/* These tables are used to add the Obj_ item to the current attr.	      */
/* The 1 << Attr item means that it is an error if this attribute is set in   */
/* the symbol table and you're trying to add the Obj_ attribute/name/other to */
/* this attribute entry.						      */

long	obj_to_attr[Obj_Done]	=	{

	/*  Obj_Assum_Type_Ch		ie: CHARACTER*(*) A 		*/
	/*  Must be a dummy argument or constant, but have to declare	*/
	/*  the type first, then make it a Parameter			*/
	/*  It can also be used to declare the result of the external	*/
	/*  function that is actually being compiled.  This function	*/
	/*  cannot be array-valued or a pointer.			*/

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (0 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (0 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (0 << Attr_Intent) |	
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(0 << Attr_Pointer) |
	 (0 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (1 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Expl_Shp_Arr		ie: DIMENSION A(1)	 */
	/*  NOTES: Must declare as array then data initialize it */

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |	
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),


	/*  Obj_Assum_Size_Arr		ie: DIMENSION A(*)	*/

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (1 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |	
	 (0 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Defrd_Shp_Arr		ie: DIMENSION A(:)	*/

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (1 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |	
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(0 << Attr_Allocatable) |
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(0 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Assum_Shp_Arr		ie: DIMENSION A (1:)	*/

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (1 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |	
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (0 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (0 << Attr_Intent) |	
	 (0 << Attr_Optional) |		(1 << Attr_Public) |
	 (1 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Co_Array		ie: DIMENSION A[3,4]	*/

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (0 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(0 << Attr_Allocatable) |
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Allocatable		ie: ALLOCATABLE A(:)	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (1 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(0 << Attr_Allocatable) |
#ifdef KEY /* Bug 6845 */
	 /* Allocatable now allowed on dummy args */
	 (0 << Attr_Intent) |           (0 << Attr_Optional) |
#else /* Bug 6845 */
	 (1 << Attr_Intent) |           (1 << Attr_Optional) |
#endif /* Bug 6845 */
	 (0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

#ifdef KEY /* Bug 14150 */
	/*  Obj_Bind		ie: BIND(C [, NAME=x ])	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |           (1 << Attr_Optional) |
	 (0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile) |
         (0 << Attr_Bind) |		(1 << Attr_Value)
	),

	/*  Obj_Value		ie: VALUE A	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (0 << Attr_Intent) |           (1 << Attr_Optional) |
	 (1 << Attr_Public) |
	 (1 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile) |
         (1 << Attr_Bind) |		(0 << Attr_Value)
	),
#endif /* KEY Bug 14150 */

	/*  Obj_Constant		ie: PARAMETER (A=1.0)	*/

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Intent			ie: INTENT (IN) A	  	    */
	/*	Intent can be specified for a deferred-shape array, because */
	/*	it may become an assumed-shape array.	If it doesn't       */

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (0 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
#ifdef KEY /* Bug 6845 */
	 /* Allocatable now allowed on dummy args */
	 (0 << Attr_Assumed_Shp_Arr) |	(0 << Attr_Allocatable) |
#else /* KEY Bug 6845 */
	 (0 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
#endif /* KEY Bug 6845 */
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
#ifdef KEY /* Bug 14150
	 /* F2003 allows intent and pointer together */
	 (1 << Attr_Save) |		(0 << Attr_Pointer) |
#else /* KEY Bug 14150 */
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
#endif /* KEY Bug 14150 */
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
	 /* Adding intent to value is special because intent(in) is ok */
         | (1 << Attr_Bind) |		(0 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Optional		ie: OPTIONAL A	*/

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (0 << Attr_Deferred_Shp_Arr) |	(0 << Attr_Assumed_Size_Arr) |
	 (0 << Attr_Assumed_Shp_Arr) |
#ifdef KEY /* Bug 6845 */
	 /* Allocatable now allowed on dummy args */
	 (0 << Attr_Allocatable) |
#else /* KEY Bug 6845 */
	 (1 << Attr_Allocatable) |
#endif /* KEY Bug 6845 */
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(0 << Attr_Pointer) |
	 (0 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(0 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Private			ie: PRIVATE A	*/

	((0 << Attr_Assumed_Type_Ch) |	(0 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (0 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (0 << Attr_Assumed_Shp_Arr) |	(0 << Attr_Allocatable) |	
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(1 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(0 << Attr_Pointer) |
	 (0 << Attr_External) |		(0 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Public			ie: PUBLIC A	*/

	((0 << Attr_Assumed_Type_Ch) |	(0 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (0 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (0 << Attr_Assumed_Shp_Arr) |	(0 << Attr_Allocatable) |
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (1 << Attr_Private) |		(0 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(0 << Attr_Pointer) |
	 (0 << Attr_External) |		(0 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Target			ie: TARGET A	*/

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (0 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (0 << Attr_Assumed_Shp_Arr) |	(0 << Attr_Allocatable) |
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(0 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Equiv			ie: EQUIVALENCE (AB)	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Saved			ie: SAVE A	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |	
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(0 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(0 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Automatic		ie: AUTOMATIC A	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |	
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(0 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Pointer			ie: POINTER :: A	*/

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |	
	 (1 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
#ifdef KEY /* Bug 14150 */
	 /* F2003 allows intent and pointer together */
	 (0 << Attr_Intent) |
#else /* KEY Bug 14150 */
	 (1 << Attr_Intent) |
#endif /* KEY Bug 14150 */
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(0 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Dcl_Extern		ie: EXTERNAL A	*/

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |	
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (0 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Dcl_Intrin		ie: INTRINSIC A	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |	
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(0 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Data_Init		ie: DATA A /1.0/	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(0 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Typed			ie: INTEGER A	*/

	/* If it's assumed_type char, it had to have been in a type stmt */
	/* so ATD_TYPED will be set.					 */

	((1 << Attr_Assumed_Type_Ch) |	(0 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (0 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (0 << Attr_Assumed_Shp_Arr) |	(0 << Attr_Allocatable) |
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(0 << Attr_Pointer) |
	 (0 << Attr_External) |		(0 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(0 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Volatile		ie: VOLATILE A	*/

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (0 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (0 << Attr_Assumed_Shp_Arr) |	(0 << Attr_Allocatable) |
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(0 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Copy_Assumed_Shape	ie: !DIR$ COPY_ASSUMED_SHAPE A	*/

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (1 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (0 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Auxiliary		ie: !DIR$ AUXILIARY A	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |	
	 (0 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |	
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Vfunction		ie: !DIR$ VFUNCTION A	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_No_Side_Effects		ie: !DIR$ NOSIDEEFFECTS A */

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(0 << Attr_Pointer) |
	 (0 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Symmetric		ie: !DIR$ SYMMETRIC A */

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |	
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |	
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Inline			ie: !DIR$ INLINE ALWAYS/NEVER A */

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |	
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |	
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(0 << Attr_Pointer) |
	 (0 << Attr_External) |		(0 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Ipa			ie: !*$* IPA  */

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |	
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |	
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(0 << Attr_Pointer) |
	 (0 << Attr_External) |		(0 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Align_Symbol		ie: !*$* ALIGN_SYMBOL A */

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |	
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |	
	 (0 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(0 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Fill_Symbol		ie: !*$* FILL_SYMBOL A */

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |	
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |	
	 (0 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(0 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Section_Gp		ie: !*$* SECTION_GP A */

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |	
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |	
	 (0 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(0 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Section_Non_Gp		ie: !*$* SECTION_NON_GP A */

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |	
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |	
	 (0 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(0 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Ignore_TKR		ie: !DIR$ IGNORE_TKR darg */

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (0 << Attr_Deferred_Shp_Arr) |	(0 << Attr_Assumed_Size_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
#ifdef KEY /* Bug 14150 */
	 /* Need ignore_tkr+pointer to implement iso_c_binding */
	 (1 << Attr_Save) |		(0 << Attr_Pointer) |
	 (0 << Attr_External) |		(1 << Attr_Intrinsic) |
#else /* KEY Bug 14150 */
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
#endif /* KEY Bug 14150 */
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Optional_Dir		ie: !*$* optional(External name) */

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |		
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |		(0 << Attr_Optional) |
	 (0 << Attr_Public) |		(0 << Attr_Private) |
	 (1 << Attr_Target) |		(1 << Attr_Data_Init) |
	 (1 << Attr_Equivalence) |	(1 << Attr_Save) |
	 (1 << Attr_Pointer) |		(0 << Attr_External) |
	 (1 << Attr_Intrinsic) |	(0 << Attr_Type) |
	 (1 << Attr_Co_Array) |		(1 << Attr_Automatic) |	
	 (1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Name			ie: !DIR$ Name(Fort_name="ext_name") */

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |		
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (0 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Cri_Ptr			ie: POINTER (A,B)	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (1 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Cri_Pointee		ie: POINTER (B,A)		*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
 	 (0 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Cri_Ch_Pointee		ie: POINTER (B,A)		*/
	/*	*(*) is	what makes an assumed-size character pointer    */

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |

# if defined(_EXTENDED_CRI_CHAR_POINTER)
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (0 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
# else
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
# endif
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Ntry_Func_Result	ie: ENTRY ABC() RESULT A	*/
	/* To be adjustable it must be a dummy arg.	Can't change a	*/
	/* dummy arg into a function result.			        */

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(0 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind)  | /* ? */	(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Dummy_Arg		ie: FUNCTION ABC(A)		*/
	/*		 		ie: ENTRY ABC(A)		*/

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (0 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (0 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(0 << Attr_Pointer) |
	 (0 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(0 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Common_Obj		ie: COMMON // A	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |	
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |	
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(0 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Namelist_Obj		ie: NAMELIST /G/ A	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(0 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Module_Proc		ie: MODULE PROCEDURE A	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (1 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Derived_Type		ie: TYPE A	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |	
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (1 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
	 /* Can't use type id in separate "BIND" statement */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Generic_Interface	ie: INTERFACE A	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |	
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |	
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(0 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),
	
	/*  Obj_Namelist_Grp		ie: NAMELIST /A/ B	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |	
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |	
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (1 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Stmt_Func		ie: A(XY) = ...		*/
	/*	Types must be scalar and have constant bounds   */

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |	
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |	
	 (1 << Attr_Optional) |		(1 << Attr_Public) |
	 (1 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Construct		ie: A : IF (I.EQ.J) THEN	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |	
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |	
	 (1 << Attr_Optional) |		(1 << Attr_Public) |
	 (1 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (1 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Entry_Func		ie: ENTRY A	*/

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |	
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(0 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Entry_Subr		ie: ENTRY A	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |	
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |	
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (1 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Intern_Func		ie: FUNCTION A()	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |	
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |	
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (1 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Intern_Subr		ie: SUBROUTINE A()	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |	
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |	
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (1 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Module_Func		ie: FUNCTION A()	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |	
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |	
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (1 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Module_Subr		ie: SUBROUTINE A()	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |		
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |		
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (1 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Sf_Darg			ie: sf(A) =	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |		
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (0 << Attr_Intent) |		
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(0 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Sf_Actual_Arg		ie: x = sf(A)		*/

	((0 << Attr_Assumed_Type_Ch) |	(0 << Attr_Parameter) |		
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (0 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (0 << Attr_Assumed_Shp_Arr) |	(0 << Attr_Allocatable) |
	 (0 << Attr_Intent) |		
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(0 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(0 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Var_Len_Ch		ie: CHARACTER*(N) A	    */
	/*  Covers both automatic and adjustable.		    */
	/*  Always called at end pass1.  Obj_type is called when the	    */
	/*  character statement is seen.  If the bounds do not resolve to a */ 
	/*  constant, then fnd_semantic_err is called with Obj_Var_Len_Ch.  */
	/*  That is why Attr_Type is allowed to be set.			    */

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |	
	 (0 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (0 << Attr_Assumed_Shp_Arr) |	(0 << Attr_Allocatable) |
	 (0 << Attr_Intent) |		
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(0 << Attr_Pointer) |
	 (0 << Attr_External) |		(0 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Var_Len_Arr		ie: DIMENSION A(N)		    */
	/*  Obj_Var_Len_Arr is used at declaration semantics. The thing is  */
	/*  already an explicit shape array, but now it is variable length. */
	/*  Because this is at end pass1, the item may have the Auxiliary   */
	/*  attribute.							    */

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |	
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (0 << Attr_Intent) |		
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Sym_Constant_Array	ie: DIMENSION A(N$PES)	 */

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |	
	 (0 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (0 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (0 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) /* ? */ |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Interface_Func		ie: FUNCTION ABC()	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (1 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Interface_Subr		ie: SUBROUTINE ABC()	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (1 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Use_Extern_Func		ie: B = A()		 */

	((0 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |	
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |	
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(0 << Attr_Pointer) |
	 (0 << Attr_External) |		(0 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Use_Extern_Subr		ie: CALL A()	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |	
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |	
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (0 << Attr_External) |		(0 << Attr_Intrinsic) |
	 (1 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Use_In_Expr		ie: X = A + B	*/

	((0 << Attr_Assumed_Type_Ch) |	(0 << Attr_Parameter) |	
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (0 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (0 << Attr_Assumed_Shp_Arr) |	(0 << Attr_Allocatable) |
	 (0 << Attr_Intent) |		
	 (0 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(0 << Attr_Pointer) |
	 (0 << Attr_External) |		(0 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(0 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Use_Derived_Type	ie: TYPE (A)	*/

	((1 << Attr_Assumed_Type_Ch) |	(1 << Attr_Parameter) |	
	 (1 << Attr_Explicit_Shp_Arr) |	(1 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (1 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (1 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Use_Spec_Expr		ie: DIMENSION ARRAY(A) */

	((0 << Attr_Assumed_Type_Ch) |	(0 << Attr_Parameter) |	
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (0 << Attr_Assumed_Size_Arr) |	(0 << Attr_Deferred_Shp_Arr) |
	 (0 << Attr_Assumed_Shp_Arr) |	(0 << Attr_Allocatable) |
	 (0 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(0 << Attr_Target) |
	 (0 << Attr_Data_Init) |	(0 << Attr_Equivalence) |
	 (0 << Attr_Save) |		(0 << Attr_Pointer) |
	 (0 << Attr_External) |		(0 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(0 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(0 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (0 << Attr_Bind) |		(0 << Attr_Value)
#endif /* KEY Bug 14150 */
	),

	/*  Obj_Use_Init_Expr		ie: PARAMETER (I=A)	*/

	((0 << Attr_Assumed_Type_Ch) |	(0 << Attr_Parameter) |
	 (0 << Attr_Explicit_Shp_Arr) |	(0 << Attr_Dimension) |
	 (1 << Attr_Assumed_Size_Arr) |	(1 << Attr_Deferred_Shp_Arr) |
	 (1 << Attr_Assumed_Shp_Arr) |	(1 << Attr_Allocatable) |
	 (1 << Attr_Intent) |
	 (1 << Attr_Optional) |		(0 << Attr_Public) |
	 (0 << Attr_Private) |		(1 << Attr_Target) |
	 (1 << Attr_Data_Init) |	(1 << Attr_Equivalence) |
	 (1 << Attr_Save) |		(1 << Attr_Pointer) |
	 (1 << Attr_External) |		(1 << Attr_Intrinsic) |
	 (0 << Attr_Type) |		(1 << Attr_Co_Array) |
	 (0 << Attr_Automatic) |	(1 << Attr_Volatile)
#ifdef KEY /* Bug 14150 */
         | (1 << Attr_Bind) |		(1 << Attr_Value)
#endif /* KEY Bug 14150 */

	) };


long	obj_to_dir[Obj_Done]	=	{

	/*  Obj_Assum_Type_Ch		ie: CHARACTER*(*) A		*/
	/*  Must be a dummy argument or constant, but have to declare	*/
	/*  the type first, then make it a Parameter.			*/
	/*  It can also be used to declare the result of the external	*/
	/*  function that is actually being compiled.	This function	*/
	/*  cannot be array-valued or a pointer.			*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(0 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (0 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(0 << Dir_Name)),

	/*	Obj_Expl_Shp_Arr	ie: DIMENSION A (1)	*/
	/*	NOTES: Must declare as array then data initialize it	*/

	((0 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (0 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (0 << Dir_Section_Gp) |	(0 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Assum_Size_Arr	ie: DIMENSION A(*)	*/

	((0 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Defrd_Shp_Arr	ie: DIMENSION A(:)	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(0 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (0 << Dir_Section_Gp) |	(0 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Assum_Shp_Arr	ie: DIMENSION A (1:)	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(0 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Co_Array	ie: DIMENSION A[3,4]	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Allocatable		ie: ALLOCATABLE A(:)	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

#ifdef KEY /* Bug 14150 */
	/*	Obj_Bind		ie: BIND(C [, NAME=x ] 	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Value		ie: VALUE A 	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),
#endif /* KEY Bug 14150 */

	/*	Obj_Constant		ie: PARAMETER (A=1.0)	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Intent		ie: INTENT (IN) A		*/
	/*  Intent can be specified for a deferred-shape array, because	*/
	/*  it may become an assumed-shape array.	If it doesn't	*/

	((0 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(0 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Optional		ie: OPTIONAL A	*/

	((0 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(0 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (0 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(0 << Dir_Name)),

	/*	Obj_Private		ie: PRIVATE A	*/

	((0 << Dir_Auxiliary) |		(0 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (0 << Dir_Symmetric) |		(0 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (0 << Dir_Section_Gp) |	(0 << Dir_Section_Non_Gp) |
	 (0 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(0 << Dir_Name)),

	/*	Obj_Public		ie: PUBLIC A	*/

	((0 << Dir_Auxiliary) |		(0 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (0 << Dir_Symmetric) |		(0 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (0 << Dir_Section_Gp) |	(0 << Dir_Section_Non_Gp) |
	 (0 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(0 << Dir_Name)),

	/*	Obj_Target		ie: TARGET A	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (0 << Dir_Section_Gp) |	(0 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Equiv		ie: EQUIVALENCE (AB)	*/

	((0 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Saved		ie: SAVE A	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (0 << Dir_Section_Gp) |	(0 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Automatic		ie: AUTOMATIC A */

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Pointer		ie: POINTER :: A	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (0 << Dir_Section_Gp) |	(0 << Dir_Section_Non_Gp) |
#ifdef KEY /* Bug 14150 */
	 /* Need pointer+ignore_tkr to implement iso_c_binding */
	 (1 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
#else /* KEY Bug 14150 */
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
#endif /* KEY Bug 14150 */
	 (0 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Dcl_Extern		ie: EXTERNAL A	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (0 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(0 << Dir_Name)),

	/*	Obj_Dcl_Intrin		ie: INTRINSIC A */

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (0 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Data_Init		ie: DATA A /1.0/	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*  Obj_Typed			ie: INTEGER A			*/

	/*  If it's assumed_type char, it had to have been in a type	*/
	/*  stmt so ATD_TYPED will be set.				*/

	((0 << Dir_Auxiliary) |		(0 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (0 << Dir_Symmetric) |		(0 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (0 << Dir_Section_Gp) |	(0 << Dir_Section_Non_Gp) |
	 (0 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(0 << Dir_Name)),

	/*  Obj_Volatile		ie: VOLATILE A			*/

	((0 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (0 << Dir_Symmetric) |		(0 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (0 << Dir_Section_Gp) |	(0 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Copy_Assumed_Shape	ie: !DIR$ COPY_ASSUMED_SHAPE A	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Auxiliary		ie: !DIR$ AUXILIARY A	*/

	((0 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Vfunction		ie: !DIR$ VFUNCTION A	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (0 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(0 << Dir_Name)),

	/*	Obj_No_Side_Effects	 ie: !DIR$ NO SIDE EFFECTS A	*/

	((1 << Dir_Auxiliary) |		(0 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (0 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(0 << Dir_Name)),

	/*	Obj_Symmetric		ie: !DIR$ SYMMETRIC A */

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |		
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Inline		ie: !DIR$ INLINE ALWAYS/NEVER A */

	((1 << Dir_Auxiliary) |		(0 << Dir_Vfunction) |		
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (0 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(0 << Dir_Name)),

	/*	Obj_Ipa   		ie: !*$* IPA */

	((1 << Dir_Auxiliary) |		(0 << Dir_Vfunction) |		
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (0 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(0 << Dir_Name)),

	/*	Obj_Align_Symbol	ie: !*$* ALIGN_SYMBOL A */

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |		
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (0 << Dir_Section_Gp) |	(0 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Fill_Symbol		ie: !*$* FILL_SYMBOL A */

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |		
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (0 << Dir_Section_Gp) |	(0 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Section_Gp		ie: !*$* SECTION_GP A */

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |		
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Section_Non_Gp	ie: !*$* SECTION_NON_GP A */

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |		
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/* Obj_Ignore_TKR		ie: !DIR$ IGNORE_TKR darg */

	((0 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(0 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Optional_Dir	ie: !*$* optional(extern)	*/

	((1 << Dir_Auxiliary) |		(0 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(0 << Dir_Name)),

	/*	Obj_Name		ie: !DIR$ Name(Fort_name="ext_name") */

	((1 << Dir_Auxiliary) |		(0 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Cri_Ptr		 ie: POINTER (A,B)	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (0 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Cri_Pointee		ie: POINTER (B,A)	 */

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Cri_Ch_Pointee	ie: POINTER (B,A)	 */
	/*	*(*) is	what makes an assumed-size character pointer */

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Ntry_Func_Result	ie: ENTRY ABC() RESULT A	*/
	/*	To be adjustable it must be a dummy arg. Can't change a	*/
	/*	dummy arg into a function result.			*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Dummy_Arg		ie: FUNCTION ABC(A)		*/
	/*				ie: ENTRY ABC(A)		*/

	((0 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(0 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (0 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(0 << Dir_Name)),

	/*	Obj_Common_Obj		ie: COMMON // A */

	((0 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (0 << Dir_Section_Gp) |	(0 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Namelist_Obj	ie: NAMELIST /G/ A	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (0 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (0 << Dir_Section_Gp) |	(0 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Module_Proc		ie: MODULE PROCEDURE A	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Derived_Type	ie: TYPE A	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Generic_Interface	ie: INTERFACE A */

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Namelist_Grp	ie: NAMELIST /A/ B	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Stmt_Func		ie: A(XY) = ...	*/
	/*	Types must be scalar and have constant bounds */

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Construct		ie: A : IF (I.EQ.J) THEN	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Entry_Func		ie: ENTRY A	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Entry_Subr		ie: ENTRY A	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Intern_Func		ie: FUNCTION A()	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Intern_Subr		ie: SUBROUTINE A()	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Module_Func		ie: FUNCTION A()	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Module_Subr		ie: SUBROUTINE A()	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Sf_Darg		ie: sf(A) =	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (0 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Sf_Actual_Arg	ie: x = sf(A)		*/

	((0 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (0 << Dir_Symmetric) |		(0 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (0 << Dir_Section_Gp) |	(0 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*  Obj_Var_Len_Ch		ie: CHARACTER*(N) A		    */

	/*  Covers both automatic and adjustable.			    */
	/*  Always called at end pass1.  Obj_type is called when the	    */
	/*  character statement is seen.  If the bounds do not resolve to a */
	/*  constant, then fnd_semantic_err is called with Obj_Var_Len_Ch.  */
	/*  That is why Attr_Type is allowed to be set.			    */

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (0 << Dir_Symmetric) |		(0 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(1 << Dir_Name)),

	/*  Obj_Var_Len_Arr		ie: DIMENSION A(N)		    */
	/*  Obj_Var_Len_Arr is used at declaration semantics.  The thing is */
	/*  already an explicit shape array, but now it is variable length. */
	/*  Because this is at end pass1, the item may have the Auxiliary   */
	/*  attribute.							    */

	((0 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (0 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Sym_Constant_Array	ie: DIMENSION A(N$PES)	*/

	((0 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (0 << Dir_Symmetric) |		(0 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Interface_Func	ie: FUNCTION ABC()	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Interface_Subr	ie: SUBROUTINE ABC()	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Use_Extern_Func	ie: B = A()		*/

	((1 << Dir_Auxiliary) |		(0 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (0 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(0 << Dir_Name)),

	/*	Obj_Use_Extern_Subr	 ie: CALL A()	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (0 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(0 << Dir_Name)),

	/*	Obj_Use_In_Expr		ie: X = A + B	*/

	((0 << Dir_Auxiliary) |		(0 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (0 << Dir_Symmetric) |		(0 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (0 << Dir_Section_Gp) |	(0 << Dir_Section_Non_Gp) |
	 (0 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(0 << Dir_Name)),

	/*	Obj_Use_Derived_Type	ie: TYPE (A)	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (1 << Dir_No_Side_Effects) |	(1 << Dir_Inline) |
	 (1 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (1 << Dir_Ipa) |		(1 << Dir_Name)),

	/*	Obj_Use_Spec_Expr	ie: DIMENSION ARRAY(A) */

	((0 << Dir_Auxiliary) |		(0 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (0 << Dir_Symmetric) |		(0 << Dir_Copy_Assumed_Shape) |
	 (0 << Dir_Align_Symbol) |	(0 << Dir_Fill_Symbol) |
	 (0 << Dir_Section_Gp) |	(0 << Dir_Section_Non_Gp) |
	 (0 << Dir_Optional) |		(0 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(0 << Dir_Name)),

	/*	Obj_Use_Init_Expr	ie: PARAMETER (I=A)	*/

	((1 << Dir_Auxiliary) |		(1 << Dir_Vfunction) |
	 (0 << Dir_No_Side_Effects) |	(0 << Dir_Inline) |
	 (0 << Dir_Symmetric) |		(1 << Dir_Copy_Assumed_Shape) |
	 (1 << Dir_Align_Symbol) |	(1 << Dir_Fill_Symbol) |
	 (1 << Dir_Section_Gp) |	(1 << Dir_Section_Non_Gp) |
	 (1 << Dir_Optional) |		(1 << Dir_Ignore_TKR) |
	 (0 << Dir_Ipa) |		(1 << Dir_Name))
	};


long	obj_to_name[Obj_Done]	=	{

	/*	Obj_Assum_Type_Ch	ie: CHARACTER*(*) A		   */
	/*	Have to declare the type before declaring it as a pointee. */
	/*	It can be the function result of an external program,	   */
	/*	so the others will have to be caught inline.  Allow it to  */
	/*	be a cri pointee.	An explicit message will be issued */
	/*	by merge_type.					   	   */

	((0 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |

# if defined(_EXTENDED_CRI_CHAR_POINTER)
	 (0 << Name_Cri_Pointee) |	(0 << Name_Cri_Ch_Pointee) |
# else
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
# endif
	 (0 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (0 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Expl_Shp_Arr	ie: DIMENSION A (1)	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |

# if defined(_EXTENDED_CRI_CHAR_POINTER)
	 (0 << Name_Cri_Pointee) |	(0 << Name_Cri_Ch_Pointee) |
# else
	 (0 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
# endif
	 (0 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (0 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Assum_Size_Arr	ie: DIMENSION A(*)	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |

# if defined(_EXTENDED_CRI_CHAR_POINTER)
	 (0 << Name_Cri_Pointee) |	(0 << Name_Cri_Ch_Pointee) |
# else
	 (0 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
# endif
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Defrd_Shp_Arr	ie: DIMENSION A(:)	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (0 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (0 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*  Obj_Assum_Shp_Arr	ie: DIMENSION A (1:)	*/
	/*  NOTE:  This can be a referenced variable, because assumed-shape */
	/*      is added at end_pass1, but must check if AT_REFERENCED set  */
	/*      if this is an original declaration of an assumed-shape arr  */

	((0 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Co_Array	ie: Dimension A[3,4]	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),


	/*	Obj_Allocatable		ie: ALLOCATABLE A(:)	*/

	((0 << Name_Variable) |		(1 << Name_Common_Obj) |
#ifdef KEY /* Bug 6845 */
	 /* Allocatable now allowed on dummy args, function result */
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (0 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
#else /* KEY Bug 6845 */
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
#endif /* KEY Bug 6845 */
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
#ifdef KEY /* Bug 6845 */
	 (0 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
#else /* KEY Bug 6845 */
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
#endif /* KEY Bug 6845 */
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

#ifdef KEY /* Bug 14150 */
	/*	Obj_Bind		ie: BIND(C [, NAME=x ] 	*/

	((0 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(0 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (0 << Name_Curr_Func) |	(0 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Value		ie: VALUE A 	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),
#endif /* KEY Bug 14150 */

	/*	Obj_Constant		ie: PARAMETER (A=1.0)	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Intent		ie: INTENT (IN) A	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Optional		ie: OPTIONAL A	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Private		ie: PRIVATE A	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (0 << Name_Cri_Pointee) |	(0 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (0 << Name_Intrinsic_Subr) |	(0 << Name_Module_Proc) |
	 (0 << Name_Derived_Type) |	(0 << Name_Generic_Interface) |
	 (0 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Public		ie: PUBLIC A	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (0 << Name_Cri_Pointee) |	(0 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (0 << Name_Intrinsic_Subr) |	(0 << Name_Module_Proc) |
	 (0 << Name_Derived_Type) |	(0 << Name_Generic_Interface) |
	 (0 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|	
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Target		ie: TARGET A	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (0 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (0 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Equiv		ie: EQUIVALENCE (AB)	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|	
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Saved			ie: SAVE A	*/

	((0 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Automatic		ie: AUTOMATIC A	*/

	((0 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (0 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(0 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (0 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (0 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Pointer		ie: POINTER :: A	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |		
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (0 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (0 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Dcl_Extern		ie: EXTERNAL A	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(0 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(0 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Dcl_Intrin		ie: INTRINSIC A	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (0 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(0 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|	
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Data_Init		ie: DATA A /1.0/	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Typed		ie: INTEGER A	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (0 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (0 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(0 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (0 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Volatile		ie: VOLATILE A	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (0 << Name_Cri_Pointee) |	(0 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Copy_Assumed_Shape	ie: !DIR$ COPY_ASSUMED_SHAPE A	*/

	((0 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Auxiliary		ie: !DIR$ AUXILIARY A	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Vfunction		ie: !DIR$ VFUNCTION A	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_No_Side_Effects	ie: !DIR$ NOSIDEEFFECTS A	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (0 << Name_Curr_Func) |	(0 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Symmetric		ie : !DIR$ SYMMETRIC A */

	((0 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Inline		ie: !DIR$ INLINE ALWAYS/NEVER A	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (0 << Name_Intrinsic_Subr) |	(0 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(0 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (0 << Name_Curr_Func) |	(0 << Name_Curr_Subr) |
	 (0 << Name_Internal_Func) |	(0 << Name_Internal_Subr)),

	/*	Obj_Ipa			ie: !*$* IPA */

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (0 << Name_Intrinsic_Subr) |	(0 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(0 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (0 << Name_Curr_Func) |	(0 << Name_Curr_Subr) |
	 (0 << Name_Internal_Func) |	(0 << Name_Internal_Subr)),

	/*	Obj_Align_Symbol	ie: !*$* ALIGN_SYMBOL A	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (0 << Name_Cri_Pointee) |	(0 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Fill_Symbol		ie: !*$* FILL_SYMBOL A	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (0 << Name_Cri_Pointee) |	(0 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Section_Gp		ie: !*$* SECTION_GP A	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (0 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Section_Non_Gp	ie: !*$* SECTION_NON_GP A	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (0 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*  Obj_Ignore_TKR		 ie: !DIR$ IGNORE_TKR	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Optional_Dir	ie !*$* optional(extern) */

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(0 << Name_Module_Proc) |
	 (1 << Name_Module) |		(0 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|	
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (0 << Name_Curr_Func) |	(0 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Name		ie: !DIR$ Name(Fort_name="ext_name") */

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|	
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Cri_Ptr		ie: POINTER (A,B)	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Cri_Pointee		ie: POINTER (B,A)	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Cri_Ch_Pointee	ie: POINTER (B,A)	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Ntry_Func_Result	ie: ENTRY ABC() RESULT A	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (0 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Dummy_Arg		ie: FUNCTION ABC(A)		*/

	((0 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|	
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Common_Obj		ie: COMMON // A	*/

	((0 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Namelist_Obj	ie: NAMELIST /G/ A	  */
	/*  The namelist object could be referenced by this time. */

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (0 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Module_Proc		ie: MODULE PROCEDURE A	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (0 << Name_Intrinsic_Subr) |	(0 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Derived_Type		ie: TYPE A	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Generic_Interface	ie: INTERFACE A	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (0 << Name_Intrinsic_Subr) |	(0 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(0 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Namelist_Grp	ie: NAMELIST /A/ B	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (0 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Stmt_Func		ie: A(XY) = ...	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Construct		ie: A : IF (I.EQ.J) THEN	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Entry_Func		ie: ENTRY A	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Entry_Subr		ie: ENTRY A	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Intern_Func		ie: FUNCTION A()	     */
	/* This can be a referenced variable, because of the forward */
	/* referencing problem.					     */

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (0 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Intern_Subr		ie: SUBROUTINE A()	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (0 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Module_Func		ie: FUNCTION A()	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (0 << Name_Intrinsic_Subr) |	(0 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(0 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Module_Subr		ie: SUBROUTINE A()	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (0 << Name_Intrinsic_Subr) |	(0 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(0 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Sf_Darg		ie: sfunc(A) =	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (0 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (0 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|	
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Sf_Actual_Arg		ie: = sf(A)	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (0 << Name_Cri_Pointee) |	(0 << Name_Cri_Ch_Pointee) |
	 (0 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*  Obj_Var_Len_Ch		ie: CHARACTER*(N) A		*/
	/*	This is added at end of pass1, so the variable 		*/
	/*	would be referenced by this time.			*/

	((0 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
# if defined(_EXTENDED_CRI_CHAR_POINTER)
	 (0 << Name_Cri_Pointee) |	(0 << Name_Cri_Ch_Pointee) |
# else
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
# endif
	 (0 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj) |	
 	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (0 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/* Obj_Var_Len_Arr		ie: DIMENSION A(N)		*/
	/*   This is added at end of pass1, so the variable would be	*/
	/*	referenced by this time.				*/

	((0 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
# if defined(_EXTENDED_CRI_CHAR_POINTER)
	 (0 << Name_Cri_Pointee) |	(0 << Name_Cri_Ch_Pointee) |
# else
	 (0 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
# endif
	 (0 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|	
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (0 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Sym_Constant_Array	ie: DIMENSION A(N$PES)	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (0 << Name_Cri_Pointee) |	(0 << Name_Cri_Ch_Pointee) |
	 (0 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (0 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Interface_Func	ie: FUNCTION ABC()	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Interface_Subr	ie: SUBROUTINE ABC()	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (0 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Use_Extern_Func	ie: B = A()	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(0 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(0 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (0 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Use_Extern_Subr	ie: CALL A()	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (0 << Name_Intrinsic_Subr) |	(0 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (1 << Name_Derived_Type) |	(0 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(0 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Use_In_Expr		ie: X = A + B	*/

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |
	 (0 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (0 << Name_Cri_Pointee) |	(0 << Name_Cri_Ch_Pointee) |
	 (0 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(0 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Derived_Type) |	(0 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (0 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (0 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (0 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Use_Derived_Type	ie: TYPE (A)	*/

	((1 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(1 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(1 << Name_Function) |
	 (0 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Use_Spec_Expr	ie: DIMENSION I(A) */

	((0 << Name_Variable) |		(0 << Name_Common_Obj) |		
	 (1 << Name_Cri_Pointer) |	(0 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(0 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr)),

	/*	Obj_Use_Init_Expr	ie: PARAMETER (I=A)	*/

	((0 << Name_Variable) |		(1 << Name_Common_Obj) |
	 (1 << Name_Cri_Pointer) |	(1 << Name_Dummy_Arg) |
	 (1 << Name_Cri_Pointee) |	(1 << Name_Cri_Ch_Pointee) |
	 (1 << Name_Func_Result) |	(0 << Name_Intrinsic_Func) |
	 (1 << Name_Intrinsic_Subr) |	(1 << Name_Module_Proc) |
	 (1 << Name_Derived_Type) |	(1 << Name_Generic_Interface) |
	 (1 << Name_Namelist_Group) |	(1 << Name_Namelist_Group_Obj)|
	 (1 << Name_Statement_Func) |	(1 << Name_Construct) |
	 (1 << Name_Module) |		(1 << Name_Blockdata) |
	 (1 << Name_Program) |		(0 << Name_Function) |
	 (1 << Name_Curr_Func) |	(1 << Name_Curr_Subr) |
	 (1 << Name_Internal_Func) |	(1 << Name_Internal_Subr))

	};


long	obj_to_other[Obj_Done]	=	{

	/* NOTE: An internal function/subroutine can be called in an outer   */
	/*	scope before it is defined as an internal procedure.	This */
	/*	is handled by using Other_Curr_Func when you're actually in  */
	/*	the routine declaring the FUNCTION.  Outside of the routine  */
	/*	you cannot declare anything for the internal function.	     */

	/*	Obj_Assum_Type_Ch		ie: CHARACTER*(*) A	*/

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Expl_Shp_Arr		ie: DIMENSION A (1)	*/

	((0 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Assum_Size_Arr		ie: DIMENSION A(*)	*/

	((0 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |		
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Defrd_Shp_Arr		ie: DIMENSION A(:)	*/

	((0 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Assum_Shp_Arr		ie: DIMENSION A (1:)	*/

	((0 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Co_Array		ie: DIMENSION A[3,4]	*/

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (0 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Allocatable		ie: ALLOCATABLE A(:)	*/

	((0 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

#ifdef KEY /* Bug 14150 */
	/*	Obj_Bind		ie: BIND(C [, NAME=x ]	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Value		ie: VALUE A	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),
#endif /* KEY Bug 14150 */

	/*	Obj_Constant		ie: PARAMETER (A=1.0)	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Intent		ie: INTENT (IN) A	*/

	((0 << Other_Var_Len_Ch) |		
	 (0 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Optional		ie: OPTIONAL A	*/

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (0 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Private			ie: PRIVATE A	*/

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (0 << Other_Expl_Interface) |	
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Public			ie: PUBLIC A	*/

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (0 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Target			ie: TARGET A	*/

	((0 << Other_Var_Len_Ch) |	
	 (0 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Equiv			ie: EQUIVALENCE (AB)	*/

	((1 << Other_Var_Len_Ch) |		
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Saved			ie: SAVE A	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |	
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Automatic			ie: AUTOMATIC A	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |	
	 (0 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (0 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Pointer			ie: POINTER :: A	*/

	((0 << Other_Var_Len_Ch) |	
	 (1 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Dcl_Extern		ie: EXTERNAL A	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Dcl_Intrin		ie: INTRINSIC A	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Data_Init		ie: DATA A /1.0/	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |	
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Typed			ie: INTEGER A	*/

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |	
	 (0 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Volatile			ie: VOLATILE A	*/

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |	
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
#ifdef KEY /* Bug 14110 */
	 (0 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
#else /* KEY Bug 14110 */
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
#endif /* KEY Bug 14110 */
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),


	/*	Obj_Copy_Assumed_Shape	ie: !DIR$ COPY_ASSUMED_SHAPE A	*/

	((0 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Auxiliary		ie: !DIR$ AUXILIARY A	*/

	((1 << Other_Var_Len_Ch) |	
	 (0 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Vfunction		ie: !DIR$ VFUNCTION A	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |	
	 (0 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_No_Side_Effects	ie: !DIR$ NOSIDEEFFECTS A */

	((1 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |	
	 (0 << Other_Use_Func) |
	 (0 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Symmetric		ie: !DIR$ SYMMETRIC A */

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |	
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Inline		ie: !DIR$ INLINE ALWAYS/NEVER A */

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |	
	 (0 << Other_Use_Func) |
	 (0 << Other_Use_Subr) |
	 (0 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Ipa		ie: !*$* IPA */

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |	
	 (0 << Other_Use_Func) |
	 (0 << Other_Use_Subr) |
	 (0 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Align_Symbol	ie: !*$* ALIGN_SYMBOL A */

	((1 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |	
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Fill_Symbol		ie: !*$* FILL_SYMBOL A */

	((1 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |	
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Section_Gp		ie: !*$* SECTION_GP A */

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |	
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Section_Non_Gp	ie: !*$* SECTION_NON_GP A */

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |	
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Ignore_TKR		ie: !DIR$ IGNORE_TKR darg */

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Optional_Dir	ie: !*$* optional(darg) */

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |	
	 (0 << Other_Use_Subr) |
	 (0 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Name		ie: !DIR$ Name(Fort_name="ext_name") */

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |	
	 (0 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Cri_Ptr		ie: POINTER (A,B)	*/

	((0 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Cri_Pointee		ie: POINTER (B,A)	*/

	((1 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Cri_Ch_Pointee	ie: POINTER (B,A)	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Ntry_Func_Result	ie: ENTRY ABC() RESULT A	*/

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Dummy_Arg		ie: FUNCTION ABC(A)	*/
	/*				ie: ENTRY ABC(A)	*/

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (0 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Common_Obj		ie: COMMON // A	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (0 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Namelist_Obj	ie: NAMELIST /G/ A	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (0 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Module_Proc		ie: MODULE PROCEDURE A	*/

	((1 << Other_Var_Len_Ch) |	
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Derived_Type		ie: TYPE A	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Generic_Interface	ie: INTERFACE A	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (0 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Namelist_Grp		ie: NAMELIST /A/ B	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |	
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Stmt_Func		ie: A(XY) = ...	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Construct		ie: A : IF (I.EQ.J) THEN	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Entry_Func		ie: ENTRY A	*/

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Entry_Subr		ie: ENTRY A	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (0 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Intern_Func		ie: FUNCTION A()	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (0 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Intern_Subr		ie: SUBROUTINE A()	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (0 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (0 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Module_Func		ie: FUNCTION A()	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Module_Subr		ie: SUBROUTINE A()	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Sf_Darg			ie: sfunc(A) =	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (0 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Sf_Actual_Arg		ie: = sfunc(A)		*/

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (0 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (0 << Other_Npes)),


	/*	Obj_Var_Len_Ch		ie: CHARACTER*(N) A	*/

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*  Obj_Var_Len_Arr		ie: DIMENSION A(N)		   */
	/*  This is added at end pass1, so the Function may have been used */
	/*  by that time.						   */

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Sym_Constant_Array	ie: DIMENSION A(N$PES)	*/

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Interface_Func		ie: FUNCTION ABC()	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Interface_Subr		ie: SUBROUTINE ABC()	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (1 << Other_Host_Assoc) |
	 (1 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Use_Extern_Func		ie: B = A()	*/

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (0 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (0 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Use_Extern_Subr		ie: CALL A()	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |	
	 (0 << Other_Use_Subr) |
	 (0 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (0 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Use_In_Expr		ie: X = A + B	*/

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (0 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (0 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (0 << Other_Npes)),

	/*	Obj_Use_Derived_Type	ie: TYPE (A)	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (1 << Other_Use_Variable) |
	 (1 << Other_Use_Dummy_Arg) |
	 (0 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
	 (1 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes)),

	/*	Obj_Use_Spec_Expr		ie: DIMENSION I(A) */

	((0 << Other_Var_Len_Ch) |
	 (0 << Other_Var_Len_Arr) |
	 (0 << Other_Use_Func) |	
	 (1 << Other_Use_Subr) |
	 (0 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (0 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (0 << Other_Npes)),

	/*	Obj_Use_Init_Expr		ie: PARAMETER (I=A)	*/

	((1 << Other_Var_Len_Ch) |
	 (1 << Other_Var_Len_Arr) |
	 (1 << Other_Use_Func) |
	 (1 << Other_Use_Subr) |
	 (1 << Other_Expl_Interface) |
	 (0 << Other_Use_Variable) |
	 (0 << Other_Use_Dummy_Arg) |
	 (0 << Other_Host_Assoc) |
	 (0 << Other_Use_Assoc) |
	 (0 << Other_Use_Char_Rslt) |
	 (1 << Other_Not_Visible) |
	 (1 << Other_Npes))

	};


/* These tables contain the error messages for bad combinations of new 	     */
/* information being added to an existing attribute.  There are 3 tables to  */
/* to match the 3 divisions that existing objects have.	(attributes, names,  */
/* and others.)	Before each Obj_... section will be the name and any kind of */
/* definition provided by the standard.	Beneath a specific message number    */
/* will be information as to why the message exists (or in some cases why    */
/* something is legal).	The numbers referred to are from the Fortran 90	     */
/* standard.	If any bits are flipped or messages are added or deleted,    */
/* update the documentation for this table.				     */

/* A basic rule used for alot of proof is 14.1.2.  Specifically this states  */
/* that a named variable, a named constant, a named construct, a statement   */
/* function, an internal procedure, a module procedure, a dummy procedure,   */
/* an intrinsic procedure, generic identifiers, derived types and namelist   */
/* group names are distinct objects and the same name may not be used for    */
/* more than one of these items in a scoping unit.  Further, program units   */
/* and external procedures are global entities and are distinct.  They may   */
/* have the same name as any other global or local entity in that scoping    */
/* unit.  This comes from 14.1.1.  A function result is a variable.          */
/* (12.5.2.2) discussion.  A dummy argument can be either a dummy procedure  */
/* (listed above) or a variable.  (2.4.3.1.1 and 2.5.1 discussion).  That    */
/* means that function results, dummy args and the things listed above are   */
/* all distinct names in the same scoping unit.	 This will be listed as      */
/* 14.1.2 distinct entities (see documentation before attr_msg_num table).   */

long	attr_msg_num[Obj_Done] [Attr_Done]	= {

	/********************** Obj_Assum_Type_Ch **************************/
	/* A length type value of star may only be used as a dummy	   */
	/* argument, a constant, or the name of the function result	   */
	/* in the external function begin compiled.	(5.1.1.5)	   */
	/* It also can be a Cray character pointer.			   */
	/*******************************************************************/

	{   0,	/* Obj_Assum_Type_Ch	 	Attr_Assumed_Type_Ch	*/

		/* The implicit type cannot be assumed type character   */
		/* by definition (5.1.1.5) and you cannot retype	*/
		/* something.	(Constraint in 5.1)			*/

	    0,	/* Obj_Assum_Type_Ch	 	Attr_Dimension		*/
	    0,	/* Obj_Assum_Type_Ch	 	Attr_Explicit_Shp_Arr	*/
	    0,	/* Obj_Assum_Type_Ch	 	Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Assum_Type_Ch	 	Attr_Deferred_Shp_Arr	*/
	    0,	/* Obj_Assum_Type_Ch	 	Attr_Assumed_Shp_Arr	*/

	  550,	/* Obj_Assum_Type_Ch	 	Attr_Allocatable        */
		/* Allocatable is not darg, constant, or func rslt.	*/
		/* Constraints in 5.2.6 and 5.1				*/

	  550,	/* Obj_Assum_Type_Ch	 	Attr_Parameter		*/
		/* Can't type something after it's a parameter, unless	*/
		/* the type agrees with the implicit typing.	Assumed */
		/* type character is not an implicit type (5.1.1.5),	*/
		/* so this is illegal. (5.2.10)				*/

	    0,	/* Obj_Assum_Type_Ch	 	Attr_Intent		*/
	    0,	/* Obj_Assum_Type_Ch	 	Attr_Optional		*/
	    0,	/* Obj_Assum_Type_Ch	 	Attr_Private		*/
	    0,	/* Obj_Assum_Type_Ch	 	Attr_Public		*/
	    0,	/* Obj_Assum_Type_Ch	 	Attr_Target		*/

	  550,	/* Obj_Assum_Type_Ch	 	Attr_Equivalence 	*/
		/* A constraint on equivalence (5.5.1) disallows an	*/
		/* equivalenced object from being a darg, a constant, 	*/
		/* or a function result.				*/

	  550,	/* Obj_Assum_Type_Ch	 	Attr_Save		*/

	    0,	/* Obj_Assum_Type_Ch	 	Attr_Pointer		*/
		/* A pointer can be a dummy argument or func result	*/

	    0,	/* Obj_Assum_Type_Ch	 	Attr_External		*/

	  550,	/* Obj_Assum_Type_Ch	 	Attr_Intrinsic		*/
		/* The only type of FUNCTION that can be assumed type   */
		/* character is the current one being compiled.		*/

	  550,	/* Obj_Assum_Type_Ch	 	Attr_Data_Init		*/
		/* Can't be darg, constant, or func rslt.	(5.1)	*/

	  550,	/* Obj_Assum_Type_Ch	 	Attr_Type		*/
		/* An item may only be given one type.	(5.1)	 	*/

	    0,	/* Obj_Assum_Type_Ch	 	Attr_Co_Array		*/
	  550,	/* Obj_Assum_Type_Ch	 	Attr_Automatic		*/
	    0	/* Obj_Assum_Type_Ch	 	Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 550	/* Obj_Assum_Type_Ch	 	Attr_Bind		*/
	, 550	/* Obj_Assum_Type_Ch	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/********************** Obj_Expl_Shp_Arr ************************/
	/* An explict shape array is a named array that is declared	*/
	/* with explicit values for the bounds in each dimension of	*/
	/* the array.	(5.1.2.4.1)	All explicit shape arrays (both	*/
	/* with constant bounds and with non-constant bounds), use this */
	/* Obj_... category.	At end pass1, array bounds are resolved.*/
	/* If they are constant, semantic checking is done, if they are */
	/* non-constant, fnd_semantic_err is called again for the	*/
	/* object, using Obj_Var_Len_Arr.  There is nothing that can	*/
	/* be a variable length array that cannot be an array with	*/
	/* constant bounds.						*/
	/****************************************************************/

	{   0,	/* Obj_Expl_Shp_Arr		Attr_Assumed_Type_Ch	*/

	    0,	/* Obj_Expl_Shp_Arr		Attr_Dimension		*/
	    0,	/* Obj_Expl_Shp_Arr		Attr_Explicit_Shp_Arr	*/
	  550,	/* Obj_Expl_Shp_Arr		Attr_Assumed_Size_Arr	*/
	  550,	/* Obj_Expl_Shp_Arr		Attr_Deferred_Shp_Arr	*/
	  550,	/* Obj_Expl_Shp_Arr		Attr_Assumed_Shp_Arr	*/
		/* 5.1 discussion - duplicate attribute.		*/

	  550,	/* Obj_Expl_Shp_Arr		Attr_Allocatable	*/
		/* Allocatable must be deferred shape.	(5.1)	 	*/

	  550,	/* Obj_Expl_Shp_Arr		Attr_Parameter		*/
		/* Shape must be specified before declaring something	*/
		/* as a constant. (5.2.10)				*/

	    0,	/* Obj_Expl_Shp_Arr		Attr_Intent		*/
	    0,	/* Obj_Expl_Shp_Arr		Attr_Optional		*/
	    0,	/* Obj_Expl_Shp_Arr		Attr_Private		*/
	    0,	/* Obj_Expl_Shp_Arr		Attr_Public		*/
	    0,	/* Obj_Expl_Shp_Arr		Attr_Target		*/
	    0,	/* Obj_Expl_Shp_Arr		Attr_Equivalence	*/
	    0,	/* Obj_Expl_Shp_Arr		Attr_Save		*/

	  550,	/* Obj_Expl_Shp_Arr		Attr_Pointer		*/
		/* Pointer must be deferred shape (5.2.7)		*/

	  550,	/* Obj_Expl_Shp_Arr		Attr_External		*/
	  550,	/* Obj_Expl_Shp_Arr		Attr_Intrinsic		*/
		/* In F77, a dummy arg could be declared EXTERNAL and 	*/
		/* typed.  This continues to be true for scalar dummy   */
		/* procedures which are functions.  However, if the	*/
		/* function is array valued, the only way it can be	*/
		/* called is if there is an explicit interface		*/
		/* (interface block) to describe it.	It must get all */
		/* type and shape information from the interface body.	*/
		/* So, a dummy arg with the EXTERNAL attribute can	*/
		/* be dimensioned only in the interface body.	If	*/
		/* something has no INTERFACE or EXTERNAL attribute,	*/
		/* but is dimensioned, it will appear to the compiler 	*/
		/* to be a local array or a darg which is an array.	*/

	  550,	/* Obj_Expl_Shp_Arr		Attr_Data_Init		*/
		/* Must have its array properties declared before	*/
		/* being data initialized. (5.2.9)			*/

	    0,	/* Obj_Expl_Shp_Arr		Attr_Type		*/

	    0,	/* Obj_Expl_Shp_Arr		Attr_Co_Array		*/
	    0,	/* Obj_Expl_Shp_Arr		Attr_Automatic		*/
	    0	/* Obj_Expl_Shp_Arr		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Expl_Shp_Arr	 	Attr_Bind		*/
	, 550	/* Obj_Expl_Shp_Arr	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/**********************	Obj_Assum_Size_Arr **********************/
	/* This is a dummy argument array.	(5.1.2.4.4)		*/
	/****************************************************************/

	{   0,	/* Obj_Assum_Size_Arr	Attr_Assumed_Type_Ch		*/

	    0,	/* Obj_Assum_Size_Arr	Attr_Dimension			*/
	  550,	/* Obj_Assum_Size_Arr	Attr_Explicit_Shp_Arr		*/
	    0,	/* Obj_Assum_Size_Arr	Attr_Assumed_Size_Arr		*/
	  550,	/* Obj_Assum_Size_Arr	Attr_Deferred_Shp_Arr		*/
	  550,	/* Obj_Assum_Size_Arr	Attr_Assumed_Shp_Arr		*/
		/* These are all illegal, because that would be		*/
		/* defining an object twice as an array.  (5.1)		*/

	  550,	/* Obj_Assum_Size_Arr	Attr_Allocatable		*/
		/* Allocatable must be	deferred shape.	(5.1)		*/

	  550,	/* Obj_Assum_Size_Arr	Attr_Parameter			*/
		/* Shape must be specified before declaring something	*/
		/* as a constant. (5.2.10)				*/

	    0,	/* Obj_Assum_Size_Arr	Attr_Intent			*/
	    0,	/* Obj_Assum_Size_Arr	Attr_Optional			*/

	    0,	/* Obj_Assum_Size_Arr	Attr_Private			*/
	    0,	/* Obj_Assum_Size_Arr	Attr_Public			*/
		/* These end up illegal eventually, because assumed	*/
		/* size arrays are dargs and dargs can't be in a	*/
		/* module.  Public and private can only be specified	*/
		/* in a module.	The message is caught by context	*/
		/* of the public/private stmts, or an error on the	*/
		/* assumed size array because it isn't a darg.		*/

	    0,	/* Obj_Assum_Size_Arr	Attr_Target			*/

	  550,	/* Obj_Assum_Size_Arr	Attr_Equivalence		*/
		/* Can't be a darg.	(5.1.1)				*/

	  550,	/* Obj_Assum_Size_Arr	Attr_Save			*/
		/* Illegal (5.1.2.5)					*/

	  550,	/* Obj_Assum_Size_Arr	Attr_Pointer			*/
		/* Pointer must be deferred shape (5.2.7)		*/

	  550,	/* Obj_Assum_Size_Arr	Attr_External			*/
	  550,	/* Obj_Assum_Size_Arr	Attr_Intrinsic			*/
		/* See [Obj_Expl_Shp_Arr,Attr_External]	comments.	*/

	  550,	/* Obj_Assum_Size_Arr	Attr_Data_Init			*/
		/* Can't be darg.	(5.2.9 constraint)		*/

	    0,	/* Obj_Assum_Size_Arr	Attr_Type			*/

	    0,	/* Obj_Assum_Size_Arr	Attr_Co_Array			*/
	  550,	/* Obj_Assum_Size_Arr	Attr_Automatic			*/
	    0	/* Obj_Assum_Size_Arr	Attr_Volatile			*/
#ifdef KEY /* Bug 14150 */
	, 550	/* Obj_Assum_Size_Arr	Attr_Bind			*/
	, 550	/* Obj_Assum_Size_Arr	Attr_Value			*/
#endif /* KEY Bug 14150 */
	},


	/********************* Obj_Defrd_Shp_Arr ************************/
	/* A deferred shape array is an array pointer or an		*/
	/* allocatable array. (5.1.2.4.3)				*/
	/****************************************************************/

	{   0,	/* Obj_Defrd_Shp_Arr	 	Attr_Assumed_Type_Ch	*/

	    0,	/* Obj_Defrd_Shp_Arr	 	Attr_Dimension		*/
	  550,	/* Obj_Defrd_Shp_Arr	 	Attr_Explicit_Shp_Arr	*/
	  550,	/* Obj_Defrd_Shp_Arr	 	Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Defrd_Shp_Arr	 	Attr_Deferred_Shp_Arr	*/
	  550,	/* Obj_Defrd_Shp_Arr	 	Attr_Assumed_Shp_Arr	*/

		/* These are all illegal, because that would be		*/
		/* defining an object twice as an array.	(5.1)	*/

	    0,	/* Obj_Defrd_Shp_Arr	 	Attr_Allocatable	*/

	  550,	/* Obj_Defrd_Shp_Arr	 	Attr_Parameter		*/
		/* Shape must be specified before declaring something   */
		/* as a constant. (5.2.10)				*/

	    0,	/* Obj_Defrd_Shp_Arr	 	Attr_Intent		*/

		/* To be a deferred shape array, it must be a pointer   */
		/* or allocatable.  Allocatable cannot be a darg.	*/
		/* Pointer can be a darg, but it cannot have the	*/
		/* intent attribute (5.1).  Leave this legal here.	*/
		/* The error will be caught by the pointer or		*/
		/* allocatable combination.  It will make more sense.   */
		/* If neither pointer or allocatable is set, error.	*/

	    0,	/* Obj_Defrd_Shp_Arr	 	Attr_Optional		*/
	    0,	/* Obj_Defrd_Shp_Arr	 	Attr_Private		*/
	    0,	/* Obj_Defrd_Shp_Arr	 	Attr_Public		*/
	    0,	/* Obj_Defrd_Shp_Arr	 	Attr_Target		*/

	  550,	/* Obj_Defrd_Shp_Arr	 	Attr_Equivalence	*/
		/* Illegal (5.5.1 constraint)				*/

	    0,	/* Obj_Defrd_Shp_Arr	 	Attr_Save		*/
		/* Assumed to be legal, because of rules in the		*/
		/* standard about the allocation status of a pointer	*/
		/* or allocatable array, if the object has the save	*/
		/* attribute. (6.3.3.1)					*/

	    0,	/* Obj_Defrd_Shp_Arr	 	Attr_Pointer		*/

	  550,	/* Obj_Defrd_Shp_Arr	 	Attr_External		*/
	  550,	/* Obj_Defrd_Shp_Arr	 	Attr_Intrinsic		*/

		/* See [Obj_Expl_Shp_Arr,Attr_External]	comments.	*/

	  550,	/* Obj_Defrd_Shp_Arr	 	Attr_Data_Init		*/

		/* Must have its array properties declared before	*/
		/* being data initialized. (5.2.9)			*/

	    0,	/* Obj_Defrd_Shp_Arr	 	Attr_Type		*/
	    0,	/* Obj_Defrd_Shp_Arr	 	Attr_Co_Array		*/
	    0,	/* Obj_Defrd_Shp_Arr	 	Attr_Automatic		*/
	    0	/* Obj_Defrd_Shp_Arr		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 550	/* Obj_Defrd_Shp_Arr	 	Attr_Bind		*/
	, 550	/* Obj_Defrd_Shp_Arr	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/********************* Obj_Assum_Shp_Arr ************************/
	/* This is a non-pointer dummy argument array.	(5.1.2.4.2)	*/
	/****************************************************************/

	{   0,	/* Obj_Assum_Shp_Arr	 	Attr_Assumed_Type_Ch	*/
		/* Assumed type character can be a dummy argument.	*/

	    0,	/* Obj_Assum_Shp_Arr	 	Attr_Dimension		*/
	  550,	/* Obj_Assum_Shp_Arr	 	Attr_Explicit_Shp_Arr	*/
	  550,	/* Obj_Assum_Shp_Arr	 	Attr_Assumed_Size_Arr	*/
	  550,	/* Obj_Assum_Shp_Arr	 	Attr_Deferred_Shp_Arr	*/
	    0,	/* Obj_Assum_Shp_Arr	 	Attr_Assumed_Shp_Arr	*/

		/* These are all illegal, because that would be		*/
		/* defining an object twice as an array. (5.1)		*/

	  550,	/* Obj_Assum_Shp_Arr	 	Attr_Allocatable	*/
		/* Allocatable must be	deferred shape.	(5.1)		*/

	  550,	/* Obj_Assum_Shp_Arr	 	Attr_Parameter		*/

		/* Shape must be specified before declaring something	*/
		/* as a constant. (5.2.10)				*/

	    0,	/* Obj_Assum_Shp_Arr	 	Attr_Intent		*/
	    0,	/* Obj_Assum_Shp_Arr	 	Attr_Optional		*/

	  550,	/* Obj_Assum_Shp_Arr	 	Attr_Private		*/
	  550,	/* Obj_Assum_Shp_Arr	 	Attr_Public		*/

		/* These end up illegal eventually, because assumed	*/
		/* shape arrays are dargs and dargs can't be in a	*/
		/* module.  Public and private can only be specified	*/
		/* in a module.	The message is caught by context	*/
		/* of the public/private stmts, or an error on the	*/
		/* assumed shape array because it isn't a darg.		*/

	    0,	/* Obj_Assum_Shp_Arr	 	Attr_Target		*/

	  550,	/* Obj_Assum_Shp_Arr	 	Attr_Equivalence	*/
		/* Equivalence can't be a dummy arg.	(5.5.1)		*/

	  550,	/* Obj_Assum_Shp_Arr	 	Attr_Save		*/
		/* Illegal (5.1.2.5)					*/

	  550,	/* Obj_Assum_Shp_Arr	 	Attr_Pointer		*/
		/* Pointer must be deferred shape (5.2.7)		*/

	  550,	/* Obj_Assum_Shp_Arr	 	Attr_External		*/
	  550,	/* Obj_Assum_Shp_Arr	 	Attr_Intrinsic		*/
		/* See [Obj_Expl_Shp_Arr,Attr_External]	comments.	*/

	  550,	/* Obj_Assum_Shp_Arr	 	Attr_Data_Init		*/
		/* Can't be darg.	(5.2.9 constraint)		*/

	    0,	/* Obj_Assum_Shp_Arr	 	Attr_Type		*/

	  550,	/* Obj_Assum_Shp_Arr	 	Attr_Co_Array		*/
	  550,	/* Obj_Assum_Shp_Arr	 	Attr_Automatic		*/
	    0	/* Obj_Assum_Shp_Arr		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 550	/* Obj_Assum_Shp_Arr	 	Attr_Bind		*/
	, 550	/* Obj_Assum_Shp_Arr	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/********************* Obj_Co_Array *************************/
	/* This is a pe dimension [3,4]				        */
	/****************************************************************/

	{   0,	/* Obj_Co_Array		Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Co_Array		Attr_Dimension		*/
	    0,	/* Obj_Co_Array		Attr_Explicit_Shp_Arr	*/
	    0,	/* Obj_Co_Array		Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Co_Array		Attr_Deferred_Shp_Arr	*/
	  550,	/* Obj_Co_Array		Attr_Assumed_Shp_Arr	*/
	    0,	/* Obj_Co_Array		Attr_Allocatable	*/
	  550,	/* Obj_Co_Array		Attr_Parameter		*/
	    0,	/* Obj_Co_Array		Attr_Intent		*/
	    0,	/* Obj_Co_Array		Attr_Optional		*/
	    0,	/* Obj_Co_Array		Attr_Private		*/
	    0,	/* Obj_Co_Array		Attr_Public		*/
	    0,	/* Obj_Co_Array		Attr_Target		*/
	    0,	/* Obj_Co_Array		Attr_Equivalence	*/
	    0,	/* Obj_Co_Array		Attr_Save		*/
	  550,	/* Obj_Co_Array		Attr_Pointer		*/
	  550,	/* Obj_Co_Array		Attr_External		*/
	  550,	/* Obj_Co_Array		Attr_Intrinsic		*/
	    0,	/* Obj_Co_Array		Attr_Data_Init		*/
	    0,	/* Obj_Co_Array		Attr_Type		*/
	  550,	/* Obj_Co_Array		Attr_Co_Array		*/
	  550,	/* Obj_Co_Array		Attr_Automatic		*/
	    0	/* Obj_Co_Array		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Co_Array	 	Attr_Bind		*/
	, 550	/* Obj_Co_Array	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	
	/*********************	Obj_Allocatable	***********************/
	/* This must be a deferred shape array and must not be a dummy */
	/* argument or function result. (5.2.6)			*/
	/***************************************************************/

	{  550,	/* Obj_Allocatable		Attr_Assumed_Type_Ch	*/
		/* Illegal (5.2.6) and 5.1 - Can't be a constant.	*/

	    0,	/* Obj_Allocatable		Attr_Dimension		*/
	  550,	/* Obj_Allocatable		Attr_Explicit_Shp_Arr	*/
	  550,	/* Obj_Allocatable		Attr_Assumed_Size_Arr	*/
		/* Allocatable must be a deferred shape array. (5.2.6)	*/

	    0,	/* Obj_Allocatable		Attr_Deferred_Shp_Arr	*/

	  550,	/* Obj_Allocatable		Attr_Assumed_Shp_Arr	*/
		/* (5.2.6) - Can't be a darg.			 	*/

	    0,	/* Obj_Allocatable		Attr_Allocatable	*/
		/* (5.1) - Can't have the same attribute twice. ANSI	*/

	  550,	/* Obj_Allocatable		Attr_Parameter		*/
		/* Illegal (5.1)					*/

#ifdef KEY /* Bug 6845 */
	    0,	/* Obj_Allocatable		Attr_Intent		*/
	    0,	/* Obj_Allocatable		Attr_Optional		*/
#else /* KEY Bug 6845 */
	  550,	/* Obj_Allocatable		Attr_Intent		*/
	  550,	/* Obj_Allocatable		Attr_Optional		*/
#endif /* KEY Bug 6845 */
		/* Illegal (5.2.6)					*/

	    0,	/* Obj_Allocatable		Attr_Private		*/
	    0,	/* Obj_Allocatable		Attr_Public		*/
	    0,	/* Obj_Allocatable		Attr_Target		*/

	  550,	/* Obj_Allocatable		Attr_Equivalence	*/
		/* Illegal (5.5.1 - constraint)				*/

	    0,	/* Obj_Allocatable		Attr_Save		*/
		/* Assumed to be legal, because of rules in the		*/
		/* standard about the allocation status of a pointer	*/
		/* or allocatable array, if the object has the save	*/
		/* attribute. (6.3.3.1)					*/

	  550,	/* Obj_Allocatable		Attr_Pointer		*/
		/* Illegal constraint 5.1				*/

	  550,	/* Obj_Allocatable		Attr_External		*/
	  550,	/* Obj_Allocatable		Attr_Intrinsic		*/
		/* Allocatable can't be a function result.	(5.2.6)	*/

	  550,	/* Obj_Allocatable		Attr_Data_Init		*/
		/* Illegal constraint 5.1				*/

	    0,	/* Obj_Allocatable		Attr_Type		*/

	    0,	/* Obj_Allocatable		Attr_Co_Array		*/
	  550,	/* Obj_Allocatable		Attr_Automatic		*/
	    0	/* Obj_Allocatable		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 550	/* Obj_Allocatable	 	Attr_Bind		*/
	, 550	/* Obj_Allocatable	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

#ifdef KEY /* Bug 14150 */
	/*********************	Obj_Bind	************************/
	/***************************************************************/

	{  550,	/* Obj_Bind			Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Bind			Attr_Dimension		*/
	    0,	/* Obj_Bind			Attr_Explicit_Shp_Arr	*/
	  550,	/* Obj_Bind			Attr_Assumed_Size_Arr	*/
	  550,	/* Obj_Bind			Attr_Deferred_Shp_Arr	*/
	  550,	/* Obj_Bind			Attr_Assumed_Shp_Arr	*/
	  550,	/* Obj_Bind			Attr_Allocatable	*/
	  550,	/* Obj_Bind			Attr_Parameter		*/
	  550,	/* Obj_Bind			Attr_Intent		*/
	  550,	/* Obj_Bind			Attr_Optional		*/
	    0,	/* Obj_Bind			Attr_Private		*/
	    0,	/* Obj_Bind			Attr_Public		*/
	    0,	/* Obj_Bind			Attr_Target		*/
	  550,	/* Obj_Bind			Attr_Equivalence	*/
	    0,	/* Obj_Bind			Attr_Save		*/
	  550,	/* Obj_Bind			Attr_Pointer		*/
	  550,	/* Obj_Bind			Attr_External		*/
	  550,	/* Obj_Bind			Attr_Intrinsic		*/
	    0,	/* Obj_Bind			Attr_Data_Init		*/
	    0,	/* Obj_Bind			Attr_Type		*/
	  550,	/* Obj_Bind			Attr_Co_Array		*/
	  550,	/* Obj_Bind			Attr_Automatic		*/
	    0	/* Obj_Bind			Attr_Volatile		*/
	,   0	/* Obj_Bind		 	Attr_Bind		*/
	, 550	/* Obj_Bind		 	Attr_Value		*/
	},

	/*********************	Obj_Value	************************/
	/***************************************************************/

	{  550,	/* Obj_Value			Attr_Assumed_Type_Ch	*/
	  550,	/* Obj_Value			Attr_Dimension		*/
	  550,	/* Obj_Value			Attr_Explicit_Shp_Arr	*/
	  550,	/* Obj_Value			Attr_Assumed_Size_Arr	*/
	  550,	/* Obj_Value			Attr_Deferred_Shp_Arr	*/
	  550,	/* Obj_Value			Attr_Assumed_Shp_Arr	*/
	  550,	/* Obj_Value			Attr_Allocatable	*/
	  550,	/* Obj_Value			Attr_Parameter		*/
	    0,	/* Obj_Value			Attr_Intent		*/
	  550,	/* Obj_Value			Attr_Optional		*/
	  550,	/* Obj_Value			Attr_Private		*/
	  550,	/* Obj_Value			Attr_Public		*/
	    0,	/* Obj_Value			Attr_Target		*/
	  550,	/* Obj_Value			Attr_Equivalence	*/
	  550,	/* Obj_Value			Attr_Save		*/
	  550,	/* Obj_Value			Attr_Pointer		*/
	  550,	/* Obj_Value			Attr_External		*/
	  550,	/* Obj_Value			Attr_Intrinsic		*/
	  550,	/* Obj_Value			Attr_Data_Init		*/
	    0,	/* Obj_Value			Attr_Type		*/
	  550,	/* Obj_Value			Attr_Co_Array		*/
	  550,	/* Obj_Value			Attr_Automatic		*/
	  550	/* Obj_Value			Attr_Volatile		*/
	, 550	/* Obj_Value		 	Attr_Bind		*/
	,   0	/* Obj_Value		 	Attr_Value		*/
	},
#endif /* KEY Bug 14150 */

	/********************* Obj_Constant *****************************/
	/* The named constant must have its type, shape and any type	*/
	/* parameters specified either by a previous occurrence in a	*/
	/* type declaration stmt in the same scoping unit, or by the	*/
	/* implicit typing rules in effect.	(5.2.10)		*/
	/****************************************************************/

	{   0,	/* Obj_Constant			Attr_Assumed_Type_Ch	*/
		/* legal - (5.1.1.5)					*/

	    0,	/* Obj_Constant			Attr_Dimension		*/
	    0,	/* Obj_Constant			Attr_Explicit_Shp_Arr	*/

	  550,	/* Obj_Constant			Attr_Assumed_Size_Arr	*/
		/* Constants can't be dargs.	(5.1.2.4.4, 14.1.2)	*/

	  550,	/* Obj_Constant			Attr_Deferred_Shp_Arr	*/
		/* Deferred shape must be allocatable or pointer.	*/
		/* Neither allocatable or pointer can be a constant.	*/

	  550,	/* Obj_Constant			Attr_Assumed_Shp_Arr	*/
		/* Constants can't be dargs.	(5.1.2.4.4, 14.1.2)	*/

	  550,	/* Obj_Constant			Attr_Allocatable	*/
		/* Illegal - 5.1 constraint				*/

	  554,	/* Obj_Constant			Attr_Parameter		*/
		/* This gives the same attribute twice.	(5.1)		*/

	  550,	/* Obj_Constant			Attr_Intent		*/
	  550,	/* Obj_Constant			Attr_Optional		*/
		/* Constants can't be dargs.	(5.1.2.4.4, 14.1.2)	*/

	    0,	/* Obj_Constant			Attr_Private		*/
	    0,	/* Obj_Constant			Attr_Public		*/

	  550,	/* Obj_Constant			Attr_Target		*/
		/* Constraint 5.2.8 - illegal				*/

	  550,	/* Obj_Constant			Attr_Equivalence	*/
		/* Constraint in 5.5.1					*/

	  550,	/* Obj_Constant			Attr_Save		*/

	  550,	/* Obj_Constant			Attr_Pointer		*/
		/* Illegal 5.2.7					*/

	  550,	/* Obj_Constant			Attr_External		*/
	  550,	/* Obj_Constant			Attr_Intrinsic		*/
		/* (5.1)						*/

	  550,	/* Obj_Constant			Attr_Data_Init		*/
		/* Constants can't be variables. (5.2.9, 14.1.2)	*/

	    0,	/* Obj_Constant			Attr_Type		*/
	  550,	/* Obj_Constant			Attr_Co_Array		*/
	  550,	/* Obj_Constant			Attr_Automatic		*/
	  550	/* Obj_Constant			Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 550	/* Obj_Constant		 	Attr_Bind		*/
	, 550	/* Obj_Constant		 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Intent ****************************/
	/* An intent statement may only appear in the specification-	*/
	/* part of a subprogram or interface body.  It must not be a	*/
	/* dummy procedure or dummy pointer.	(5.2.1)			*/
	/***************************************************************/

	{   0,	/* Obj_Intent			Attr_Assumed_Type_Ch	*/

	    0,	/* Obj_Intent			Attr_Dimension		*/
	    0,	/* Obj_Intent			Attr_Explicit_Shp_Arr	*/
	    0,	/* Obj_Intent			Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Intent			Attr_Deferred_Shp_Arr	*/
	    0,	/* Obj_Intent			Attr_Assumed_Shp_Arr	*/

#ifdef KEY /* Bug 6845 */
	    0,	/* Obj_Intent			Attr_Allocatable	*/
#else /* KEY Bug 6845 */
	  550,	/* Obj_Intent			Attr_Allocatable	*/
		/* Can't be a dummy arg (5.2.6)				*/
#endif /* KEY Bug 6845 */

	  550,	/* Obj_Intent			Attr_Parameter		*/
		/* Constants can't be dargs. (5.1.2.4.4, 14.1.2)	*/

	    0,	/* Obj_Intent			Attr_Intent		*/
		/* This is defining the attribute twice.(5.1) ANSI	*/

	    0,	/* Obj_Intent			Attr_Optional		*/

	    0,	/* Obj_Intent			Attr_Private		*/
	    0,	/* Obj_Intent			Attr_Public		*/
		/* These end up illegal eventually, because these	*/
		/* must be dargs and dargs can't be in a module.	*/
		/* Public and private can only be specified in a	*/
		/* module.	The message is caught by context of the	*/
		/* public/private stmts, or an error on the intent	*/
		/* object because it isn't a darg.			*/

	    0,	/* Obj_Intent			Attr_Target		*/

	  550,	/* Obj_Intent			Attr_Equivalence	*/
		/* Equivalence can't be a dummy arg.	(5.5.1)		*/

	  550,	/* Obj_Intent			Attr_Save		*/
		/* Save can't be a dummy arg.	(5.2.4)			*/

#ifdef KEY /* Bug 14150 */
	    0,	/* Obj_Intent			Attr_Pointer		*/
#else /* KEY Bug 14150 */
	  550,	/* Obj_Intent			Attr_Pointer		*/
		/* (5.1)						*/
#endif /* KEY Bug 14150 */

	  550,	/* Obj_Intent			Attr_External		*/
	  550,	/* Obj_Intent			Attr_Intrinsic		*/
		/* 5.2.1 - Intent can't specify a dummy procedure.	*/

	  550,	/* Obj_Intent			Attr_Data_Init		*/
		/* (5.1)						*/

	    0,	/* Obj_Intent			Attr_Type		*/

	    0,	/* Obj_Intent			Attr_Co_Array		*/
	  550,	/* Obj_Intent			Attr_Automatic		*/
	    0	/* Obj_Intent			Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 550	/* Obj_Intent		 	Attr_Bind		*/
	,   0	/* Obj_Intent		 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/*********************	Obj_Optional	**************************/
	/* The optional statement may only appear in the specification-*/
	/* part of a subprogram or interface body.	(5.2.2)		*/
	/***************************************************************/

	{   0,	/* Obj_Optional			Attr_Assumed_Type_Ch	*/

	    0,	/* Obj_Optional			Attr_Dimension		*/
	    0,	/* Obj_Optional			Attr_Explicit_Shp_Arr	*/
	    0,	/* Obj_Optional			Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Optional			Attr_Deferred_Shp_Arr	*/
	    0,	/* Obj_Optional			Attr_Assumed_Shp_Arr	*/

#ifdef KEY /* Bug 6845 */
	    0,	/* Obj_Optional			Attr_Allocatable	*/
#else /* KEY Bug 6845 */
	  550,	/* Obj_Optional			Attr_Allocatable	*/
		/* Can't be a dummy arg (5.2.6)				*/
#endif /* KEY Bug 6845 */

	  550,	/* Obj_Optional			Attr_Parameter		*/
		/* Constants can't be dargs. (5.1.2.4.4, 14.1.2)	*/

	    0,	/* Obj_Optional			Attr_Intent		*/

	    0,	/* Obj_Optional			Attr_Optional		*/
		/* This is defining the attribute twice (5.1)	ANSI	*/

	    0,	/* Obj_Optional			Attr_Private		*/
	    0,	/* Obj_Optional			Attr_Public		*/
		/* These end up illegal eventually, because these	*/
		/* must be dargs and dargs can't be in a module.	*/
		/* Public and private can only be specified in a	*/
		/* module.	The message is caught by context of the	*/
		/* public/private stmts, or an error on the optional	*/
		/* object because it isn't a darg.			*/

	    0,	/* Obj_Optional			Attr_Target		*/

	  550,	/* Obj_Optional			Attr_Equivalence	*/
		/* Equivalence can't be a dummy arg.	(5.5.1)		*/

	  550,	/* Obj_Optional			Attr_Save		*/
		/* 5.2.4						*/

		    0,	/* Obj_Optional		Attr_Pointer		*/
		    0,	/* Obj_Optional		Attr_External		*/

	  550,	/* Obj_Optional			Attr_Intrinsic		*/
		/* An intrinsic proc can't be a dummy proc.	(14.1.2)*/

	  550,	/* Obj_Optional			Attr_Data_Init		*/
		/* 5.2.9 constraint					*/

	    0,	/* Obj_Optional			Attr_Type		*/

	    0,	/* Obj_Optional			Attr_Co_Array		*/
	  550,	/* Obj_Optional			Attr_Automatic		*/
	    0	/* Obj_Optional			Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 550	/* Obj_Optional		 	Attr_Bind		*/
	,   0	/* Obj_Optional		 	Attr_Value		*/

#endif /* KEY Bug 14150 */
	},


	/*********************	Obj_Private	***************************/
	/* The private statement may only be in the scoping unit of a	  */
	/* module.	Each use-name must be a named variable, procedure,*/
	/* derived type, named constant or namelist group.		  */
	/******************************************************************/

	{   0,	/* Obj_Private			Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Private			Attr_Dimension		*/
	    0,	/* Obj_Private			Attr_Explicit_Shp_Arr	*/

	    0,	/* Obj_Private			Attr_Assumed_Size_Arr	*/
		/* These are caught because dargs can't be in modules.  */

	    0,	/* Obj_Private			Attr_Deferred_Shp_Arr	*/

	    0,	/* Obj_Private			Attr_Assumed_Shp_Arr	*/
		/* These are caught because dargs can't be in modules.  */

	    0,	/* Obj_Private			Attr_Allocatable	*/
	    0,	/* Obj_Private			Attr_Parameter		*/

	    0,	/* Obj_Private			Attr_Intent		*/
	    0,	/* Obj_Private			Attr_Optional		*/
		/* These are caught because dargs can't be in modules.  */

	    0,	/* Obj_Private			Attr_Private		*/
	  550,	/* Obj_Private			Attr_Public		*/
		/* (5.1) constraint, also attr defined twice.	ANSI	*/

	    0,	/* Obj_Private			Attr_Target		*/
	    0,	/* Obj_Private			Attr_Equivalence	*/
	    0,	/* Obj_Private			Attr_Save		*/
	    0,	/* Obj_Private			Attr_Pointer		*/
	    0,	/* Obj_Private			Attr_External		*/
	    0,	/* Obj_Private			Attr_Intrinsic		*/
	    0,	/* Obj_Private			Attr_Data_Init		*/
	    0,	/* Obj_Private			Attr_Type		*/

	    0,	/* Obj_Private			Attr_Co_Array		*/
	    0,	/* Obj_Private			Attr_Automatic		*/
	    0	/* Obj_Private			Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Private		 	Attr_Bind		*/
	, 550	/* Obj_Private		 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/*********************	Obj_Public	****************************/
	/* The public statement may only be in the scoping unit of a	   */
	/* module.	Each use-name must be a named variable, procedure, */
	/* derived type, named constant or namelist group.		   */
	/*******************************************************************/

	{   0,	/* Obj_Public			Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Public			Attr_Dimension		*/
	    0,	/* Obj_Public			Attr_Explicit_Shp_Arr	*/

	    0,	/* Obj_Public			Attr_Assumed_Size_Arr	*/
		/* These are caught because dargs can't be in modules.  */

	    0,	/* Obj_Public			Attr_Deferred_Shp_Arr	*/

	    0,	/* Obj_Public			Attr_Assumed_Shp_Arr	*/
		/* These are caught because dargs can't be in modules.  */

	    0,	/* Obj_Public			Attr_Allocatable	*/
	    0,	/* Obj_Public			Attr_Parameter		*/

	    0,	/* Obj_Public			Attr_Intent		*/
	    0,	/* Obj_Public			Attr_Optional		*/
		/* These are caught because dargs can't be in modules.  */

	  550,	/* Obj_Public			Attr_Private		*/
	    0,	/* Obj_Public			Attr_Public		*/
		/* (5.1) constraint, also attr defined twice.	ANSI	*/

	    0,	/* Obj_Public			Attr_Target		*/
	    0,	/* Obj_Public			Attr_Equivalence	*/
	    0,	/* Obj_Public			Attr_Save		*/
	    0,	/* Obj_Public			Attr_Pointer		*/
	    0,	/* Obj_Public			Attr_External		*/
	    0,	/* Obj_Public			Attr_Intrinsic		*/
	    0,	/* Obj_Public			Attr_Data_Init		*/
	    0,	/* Obj_Public			Attr_Type		*/

	    0,	/* Obj_Public			Attr_Co_Array		*/
	    0,	/* Obj_Public			Attr_Automatic		*/
	    0	/* Obj_Public			Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Public		 	Attr_Bind		*/
	, 550	/* Obj_Public		 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/*********************** Obj_Target ******************************/
	/* This specifies a list of object names that may have		 */
	/* pointers associated with them.				 */
	/*****************************************************************/

	{   0,	/* Obj_Target			Attr_Assumed_Type_Ch	*/
		/* Assumed type character can be a dummy argument.	*/
		/* Dummy args can be targets.			 	*/

	    0,	/* Obj_Target			Attr_Dimension		*/
	    0,	/* Obj_Target			Attr_Explicit_Shp_Arr	*/
	    0,	/* Obj_Target			Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Target			Attr_Deferred_Shp_Arr	*/
	    0,	/* Obj_Target			Attr_Assumed_Shp_Arr	*/
	    0,	/* Obj_Target			Attr_Allocatable	*/

	  550,	/* Obj_Target			Attr_Parameter		*/
		/* 5.2.8 constraint - illegal			 	*/

	    0,	/* Obj_Target			Attr_Intent		*/
	    0,	/* Obj_Target			Attr_Optional		*/
	    0,	/* Obj_Target			Attr_Private		*/
	    0,	/* Obj_Target			Attr_Public		*/

	    0,	/* Obj_Target			Attr_Target		*/
		/* Defined twice - constraint 5.1		ANSI	*/

	  550,	/* Obj_Target			Attr_Equivalence	*/
	    0,	/* Obj_Target			Attr_Save		*/

	  550,	/* Obj_Target			Attr_Pointer		*/
	  550,	/* Obj_Target			Attr_External		*/
	  550,	/* Obj_Target			Attr_Intrinsic		*/
		/* Illegal - 5.1 constraint				*/

	    0,	/* Obj_Target			Attr_Data_Init		*/
	    0,	/* Obj_Target			Attr_Type		*/
	    0,	/* Obj_Target			Attr_Co_Array		*/
	    0,	/* Obj_Target			Attr_Automatic		*/
	    0	/* Obj_Target			Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Target		 	Attr_Bind		*/
	,   0	/* Obj_Target		 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/*********************	Obj_Equiv	*****************************/
	/* An equivalence object must not be a dummy argument, a	*/
	/* pointer, an allocatable array, and object of a nonsequenced */
	/* derived type or of a sequence derived type containing a	*/
	/* pointer at any level of component selection, an automatic	*/
	/* object, a function name, an entry name, a result name, a	*/
	/* named constant, a structure component, or a subobject of any */
	/* of the preceeding objects. (5.5.1 constraint)		*/
	/****************************************************************/

	{  550,	/* Obj_Equiv		 Attr_Assumed_Type_Ch		*/
		/* Equivalence cannot be a darg, a constant or a	*/
		/* function result. (5.5.1).  These are the only	*/
		/* things that an assumed type char can be.  (5.1.1.)	*/

	    0,	/* Obj_Equiv		 Attr_Dimension			*/
	    0,	/* Obj_Equiv		 Attr_Explicit_Shp_Arr		*/

	  550,	/* Obj_Equiv		 Attr_Assumed_Size_Arr		*/
	  550,	/* Obj_Equiv		 Attr_Deferred_Shp_Arr		*/
	  550,	/* Obj_Equiv		 Attr_Assumed_Shp_Arr		*/
	  550,	/* Obj_Equiv		 Attr_Allocatable	 	*/
		/* (5.5.1 constraint) dargs, pointers, and allocatable	*/

	  550,	/* Obj_Equiv		 Attr_Parameter			*/
	  550,	/* Obj_Equiv		 Attr_Intent			*/
	  550,	/* Obj_Equiv		 Attr_Optional			*/
		/* (5.5.1 constraint) constants and dargs		*/

	    0,	/* Obj_Equiv		 Attr_Private			*/
	    0,	/* Obj_Equiv		 Attr_Public			*/
	  550,	/* Obj_Equiv		 Attr_Target			*/
	    0,	/* Obj_Equiv		 Attr_Equivalence	 	*/
	    0,	/* Obj_Equiv		 Attr_Save			*/

	  550,	/* Obj_Equiv		 Attr_Pointer			*/
	  550,	/* Obj_Equiv		 Attr_External			*/
	  550,	/* Obj_Equiv		 Attr_Intrinsic			*/
		/* 5.5.1 - function names and pointers.			*/

	    0,	/* Obj_Equiv		 Attr_Data_Init			*/
	    0,	/* Obj_Equiv		 Attr_Type			*/

	    0,	/* Obj_Equiv		 Attr_Co_Array			*/
	    0,	/* Obj_Equiv		 Attr_Automatic			*/
	    0	/* Obj_Equiv		 Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 550	/* Obj_Equiv		 Attr_Bind			*/
	, 550	/* Obj_Equiv		 Attr_Value			*/
#endif /* KEY Bug 14150 */
	},


	/**************************** Obj_Save **************************/
	/* An object-name must not be a dummy argument name, a	 	*/
	/* procedure name, a function result name, an automatic data	*/
	/* object, or the name of an entity in a common block. (5.2.4)  */
	/****************************************************************/

	{ 550,	/* Obj_Saved		 Attr_Assumed_Type_Ch		*/

	    0,	/* Obj_Saved		 Attr_Dimension			*/
	    0,	/* Obj_Saved		 Attr_Explicit_Shp_Arr		*/

	  550,	/* Obj_Saved		 Attr_Assumed_Size_Arr		*/
		/* (5.2.4 constraint) dargs				*/

	    0,	/* Obj_Saved		 Attr_Deferred_Shp_Arr		*/

	  550,	/* Obj_Saved		 Attr_Assumed_Shp_Arr		*/
		/* (5.2.4 constraint) dargs				*/

	    0,	/* Obj_Saved		 Attr_Allocatable	 	*/

	  550,	/* Obj_Saved		 Attr_Parameter			*/

	  550,	/* Obj_Saved		 Attr_Intent			*/
	  550,	/* Obj_Saved		 Attr_Optional			*/
		/* (5.2.4 constraint) dargs				*/

	    0,	/* Obj_Saved		 Attr_Private			*/
	    0,	/* Obj_Saved		 Attr_Public			*/
	    0,	/* Obj_Saved		 Attr_Target			*/
	    0,	/* Obj_Saved		 Attr_Equivalence	 	*/

	    0,	/* Obj_Saved		 Attr_Save			*/
		/* Defining attribute twice (5.1)		ANSI	*/

	    0,	/* Obj_Saved		 Attr_Pointer			*/

	  550,	/* Obj_Saved		 Attr_External			*/
	  550,	/* Obj_Saved		 Attr_Intrinsic			*/
		/* (5.2.4 constraint) procedures			*/

	    0,	/* Obj_Saved		 Attr_Data_Init			*/
	    0,	/* Obj_Saved		 Attr_Type			*/

	    0,	/* Obj_Saved		 Attr_Co_Array			*/
	  550,	/* Obj_Saved		 Attr_Automatic			*/
	    0	/* Obj_Saved		 Attr_Volatile			*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Saved	 	Attr_Bind			*/
	, 550	/* Obj_Saved	 	Attr_Value			*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Automatic *************************/
	/****************************************************************/

	{ 550,	/* Obj_Automatic		Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Automatic		Attr_Dimension		*/
	    0,	/* Obj_Automatic		Attr_Explicit_Shp_Arr	*/
	  550,	/* Obj_Automatic		Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Automatic		Attr_Deferred_Shp_Arr	*/
	  550,	/* Obj_Automatic		Attr_Assumed_Shp_Arr	*/
	  550,	/* Obj_Automatic		Attr_Allocatable	*/
	  550,	/* Obj_Automatic		Attr_Parameter		*/
	  550,	/* Obj_Automatic		Attr_Intent		*/
	  550,	/* Obj_Automatic		Attr_Optional		*/
	    0,	/* Obj_Automatic		Attr_Private		*/
	    0,	/* Obj_Automatic		Attr_Public		*/
	    0,	/* Obj_Automatic		Attr_Target		*/
	    0,	/* Obj_Automatic		Attr_Equivalence	*/
	  550,	/* Obj_Automatic		Attr_Save		*/
	    0,	/* Obj_Automatic		Attr_Pointer		*/
	  550,	/* Obj_Automatic		Attr_External		*/
	  550,	/* Obj_Automatic		Attr_Intrinsic		*/
	  550,	/* Obj_Automatic		Attr_Data_Init		*/
	    0,	/* Obj_Automatic		Attr_Type		*/
	  550,	/* Obj_Automatic		Attr_Co_Array		*/
	    0,	/* Obj_Automatic		Attr_Automatic		*/
	    0	/* Obj_Automatic		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 550	/* Obj_Automatic	 	Attr_Bind		*/
	, 550	/* Obj_Automatic	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Pointer ***************************/
	/* Cannot have the INTENT or PARAMETER attributes.  If it is an */
	/* array, it must be a deferred shape one. (5.2.7 constraints)  */
	/* Cannot be ALLOCATABLE, EXTERNAL, TARGET, or INTRINSIC. (5.1) */
	/****************************************************************/

	{   0,	/* Obj_Pointer			Attr_Assumed_Type_Ch	*/
		/* Assumed type character can be a darg or a function   */
		/* result.	A pointer can be either of these also.	*/

	    0,	/* Obj_Pointer			Attr_Dimension		*/
	  550,	/* Obj_Pointer			Attr_Explicit_Shp_Arr	*/
	  550,	/* Obj_Pointer			Attr_Assumed_Size_Arr	*/
		/* (5.2.7) Pointer must be a deferred shape array.	*/
		
	    0,	/* Obj_Pointer			Attr_Deferred_Shp_Arr	*/

	  550,	/* Obj_Pointer			Attr_Assumed_Shp_Arr	*/
		/* (5.2.7) Pointer must be a deferred shape array.	*/

	  550,	/* Obj_Pointer			Attr_Allocatable	*/
		/* 5.1 constraint					*/

	  550,	/* Obj_Pointer			Attr_Parameter		*/
#ifdef KEY /* Bug 14150 */
	    0,	/* Obj_Pointer			Attr_Intent		*/
#else /* KEY Bug 14150 */
	  550,	/* Obj_Pointer			Attr_Intent		*/
		/* 5.2.7						*/
#endif /* KEY Bug 14150 */

	    0,	/* Obj_Pointer			Attr_Optional		*/
	    0,	/* Obj_Pointer			Attr_Private		*/
	    0,	/* Obj_Pointer			Attr_Public		*/

	  550,	/* Obj_Pointer			Attr_Target		*/
		/* Constraint 5.1 - illegal				*/

	  550,	/* Obj_Pointer			Attr_Equivalence	*/
		/* 5.5.1 constraint					*/

	    0,	/* Obj_Pointer			Attr_Save		*/
		/* Assumed to be legal, because of rules in the		*/
		/* standard about the allocation status of a pointer	*/
		/* or allocatable array, if the object has the save	*/
		/* attribute. (6.3.3.1)					*/

	    0,	/* Obj_Pointer			Attr_Pointer		*/
		/* Double definition (5.1)		ANSI	 	*/

	  550,	/* Obj_Pointer			Attr_External		*/
	  550,	/* Obj_Pointer			Attr_Intrinsic		*/
		/* 5.1 constraint					*/

	    0,	/* Obj_Pointer			Attr_Data_Init		*/
		/* 5.2.9 discussion - extension	 now			*/

	    0,	/* Obj_Pointer			Attr_Type		*/
	  550,	/* Obj_Pointer			Attr_Co_Array		*/
	    0,	/* Obj_Pointer			Attr_Automatic		*/
	    0	/* Obj_Pointer			Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 550	/* Obj_Pointer		 	Attr_Bind		*/
	, 550	/* Obj_Pointer		 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Dcl_Extern ************************/
	/* External-name must be the name of an external procedure, a	*/
	/* darg, or a block data program unit.			 	*/
	/****************************************************************/

	{   0,	/* Obj_Dcl_Extern		Attr_Assumed_Type_Ch	*/
	  550,	/* Obj_Dcl_Extern		Attr_Dimension		*/
	  550,	/* Obj_Dcl_Extern		Attr_Explicit_Shp_Arr	*/
	  550,	/* Obj_Dcl_Extern		Attr_Assumed_Size_Arr	*/
	  550,	/* Obj_Dcl_Extern		Attr_Deferred_Shp_Arr	*/
	  550,	/* Obj_Dcl_Extern		Attr_Assumed_Shp_Arr	*/
	  550,	/* Obj_Dcl_Extern		Attr_Allocatable	*/
		/*	(12.3.1.1) If a procedure has an array-valued	*/
		/* result, it must have an explicit interface.	An	*/
		/* explicit interface for an external subprogram must 	*/
		/* be specified in an interface block.	(12.3.1)	*/


	  550,	/* Obj_Dcl_Extern		Attr_Parameter		*/
		/* 5.1 constraint					*/

	  550,	/* Obj_Dcl_Extern		Attr_Intent		*/
		/* 5.1.2.3 constraint				 	*/

	    0,	/* Obj_Dcl_Extern		Attr_Optional		*/
	    0,	/* Obj_Dcl_Extern		Attr_Private		*/
	    0,	/* Obj_Dcl_Extern		Attr_Public		*/

	  550,	/* Obj_Dcl_Extern		Attr_Target		*/
		/* Constraint 5.1 - illegal				*/

	  550,	/* Obj_Dcl_Extern		Attr_Equivalence	*/
		/* This must be a variable (5.5.1).	Variables and	*/
		/* externals cannot have the same name.	(14.1.2)	*/

	  550,	/* Obj_Dcl_Extern		Attr_Save		*/
		/* (5.2.4)						*/

	  550,	/* Obj_Dcl_Extern		Attr_Pointer		*/
	    0,	/* Obj_Dcl_Extern		Attr_External		*/
	  550,	/* Obj_Dcl_Extern		Attr_Intrinsic		*/
		/* 5.1 - constraint - also duplicate defines.	ANSI	*/

	  550,	/* Obj_Dcl_Extern		Attr_Data_Init		*/
		/* This must be a variable (5.2.9).	Variables and	*/
		/* externals cannot have the same name.	(14.1.2)	*/
	
	    0,	/* Obj_Dcl_Extern		Attr_Type		*/

	  550,	/* Obj_Dcl_Extern		Attr_Co_Array		*/
	  550,	/* Obj_Dcl_Extern		Attr_Automatic		*/
	  550	/* Obj_Dcl_Extenr		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Dcl_Extern	 	Attr_Bind		*/
	, 550	/* Obj_Dcl_Extern	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Dcl_Intrin ************************/
	/* Intrinsic-name must be the name of an intrinsic procedure.	*/
	/****************************************************************/

	{ 550,	/* Obj_Dcl_Intrin		Attr_Assumed_Type_Ch	*/
		/* This ends up being illegal because only current	*/
		/* function being compiled can be typed as assumed	*/
		/* type character.					*/

	  550,	/* Obj_Dcl_Intrin		Attr_Dimension		*/
	  550,	/* Obj_Dcl_Intrin		Attr_Explicit_Shp_Arr	*/
		/* 5.1 discussion only mentions type, not shape.	*/

	  550,	/* Obj_Dcl_Intrin		Attr_Assumed_Size_Arr	*/
		/* Intrinsic and dargs are distinct	(14.1.2)	*/

	  550,	/* Obj_Dcl_Intrin		Attr_Deferred_Shp_Arr	*/
		/* Deferred shape can only be allocatable or pointer.   */
		/* Allocatable can't be a function result.	Pointer	*/
		/* and INTERNAL are prohibited.	(5.1.2.4.3 and 5.1)	*/

	  550,	/* Obj_Dcl_Intrin		Attr_Assumed_Shp_Arr	*/
		/* Intrinsic and dargs are distinct	(14.1.2)	*/

	  550,	/* Obj_Dcl_Intrin		Attr_Allocatable	*/
		/* This can't be a function result. (5.1)		*/

	  550,	/* Obj_Dcl_Intrin		Attr_Parameter		*/
		/* 5.1 constraint					*/

	  550,	/* Obj_Dcl_Intrin		Attr_Intent		*/
	  550,	/* Obj_Dcl_Intrin		Attr_Optional		*/
		/* Dargs and intrinsic procedures are distinct 14.1.2 	*/

	    0,	/* Obj_Dcl_Intrin		Attr_Private		*/
	    0,	/* Obj_Dcl_Intrin		Attr_Public		*/

	  550,	/* Obj_Dcl_Intrin		Attr_Target		*/
		/* Constraint 5.1 - illegal				*/

	  550,	/* Obj_Dcl_Intrin		Attr_Equivalence	*/
		/* Variables and intrinsic procs are distinct 14.1.2	*/
		
	  550,	/* Obj_Dcl_Intrin		Attr_Save		*/
		/* (5.2.4)						*/

	  550,	/* Obj_Dcl_Intrin		Attr_Pointer		*/
	  550,	/* Obj_Dcl_Intrin		Attr_External		*/
	    0,	/* Obj_Dcl_Intrin		Attr_Intrinsic		*/
	  550,	/* Obj_Dcl_Intrin		Attr_Data_Init		*/
		/* 5.1 constraint or duplicate definition.	ANSI	*/

	    0,	/* Obj_Dcl_Intrin		Attr_Type		*/
	  550,	/* Obj_Dcl_Intrin		Attr_Co_Array		*/
	  550,	/* Obj_Dcl_Intrin		Attr_Automatic		*/
	  550	/* Obj_Dcl_Intrin		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 550	/* Obj_Dcl_Intrin	 	Attr_Bind		*/
	, 550	/* Obj_Dcl_Intrin	 	Attr_Value		*/
#endif /* KEY Bug 14150 */

	},


	/************************ Obj_Data_Init	*************************/
	/* The = initialization-expr must not appear if object-name is  */
	/* a darg, a function result, an object in a named common block */
	/* unless the type declaration is in a block data program unit, */
	/* an object in blank common, an allocatable array, a pointer,  */
	/* an external name, an intrinsic name, or an automatic object. */
	/* (5.1 constraint)						*/
	/****************************************************************/

	{  550,	/* Obj_Data_Init		Attr_Assumed_Type_Ch	*/
		/* An assumed type char may only be a darg, constant, 	*/
		/* or func rslt.  (5.1.1.5)  An object that is data	*/
		/* initialized may not be any of these.	(5.1)		*/

	    0,	/* Obj_Data_Init		Attr_Dimension		*/
	    0,	/* Obj_Data_Init		Attr_Explicit_Shp_Arr	*/

	  550,	/* Obj_Data_Init		Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Data_Init		Attr_Deferred_Shp_Arr	*/
	  550,	/* Obj_Data_Init		Attr_Assumed_Shp_Arr	*/
	  550,	/* Obj_Data_Init		Attr_Allocatable	*/
		/* 5.1 - dargs, pointer and allocatable.		*/
	  550,	/* Obj_Data_Init		Attr_Parameter		*/
		/* Variables and constants are distinct(5.2.9, 14.1.2)	*/

	  550,	/* Obj_Data_Init		Attr_Intent		*/
	  550,	/* Obj_Data_Init		Attr_Optional		*/
		/* Can't be a darg. (5.1)				*/

	    0,	/* Obj_Data_Init		Attr_Private		*/
	    0,	/* Obj_Data_Init		Attr_Public		*/
	    0,	/* Obj_Data_Init		Attr_Target		*/
	    0,	/* Obj_Data_Init		Attr_Equivalence	*/
	    0,	/* Obj_Data_Init		Attr_Save		*/

	    0,	/* Obj_Data_Init		Attr_Pointer		*/
	  550,	/* Obj_Data_Init		Attr_External		*/
	  550,	/* Obj_Data_Init		Attr_Intrinsic		*/
		/* 5.1 constraint					*/

	    0,	/* Obj_Data_Init		Attr_Data_Init		*/
	    0,	/* Obj_Data_Init		Attr_Type		*/

	    0,	/* Obj_Data_Init		Attr_Co_Array		*/
	  550,	/* Obj_Data_Init		Attr_Automatic		*/
	    0	/* Obj_Data_Init		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Data_Init	 	Attr_Bind		*/
	, 550	/* Obj_Data_Init	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Typed *****************************/
	/* This must be an object (which is a constant or a variable)	*/
	/* (2.4.3.1) or an external function, an intrinsic function, a  */
	/* function dummy procedure or a statement function.		*/
	/* Actually you can type an internal or module function also,	*/
	/* but strict adherence to the standard forces this typing to	*/
	/* be on the FUNCTION statement, not the type declaration stmt  */
	/****************************************************************/

	{  550,	/* Obj_Typed		 	Attr_Assumed_Type_Ch	*/
		/* Assumed type character can not be an implicit type.	*/
		/* (5.1.1.5).	An item cannot be typed twice. (5.1)	*/

	    0,	/* Obj_Typed		 	Attr_Dimension		*/
	    0,	/* Obj_Typed		 	Attr_Explicit_Shp_Arr	*/
	    0,	/* Obj_Typed		 	Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Typed		 	Attr_Deferred_Shp_Arr	*/
	    0,	/* Obj_Typed		 	Attr_Assumed_Shp_Arr	*/
	    0,	/* Obj_Typed		 	Attr_Allocatable	*/
	    0,	/* Obj_Typed		 	Attr_Parameter		*/
	    0,	/* Obj_Typed		 	Attr_Intent		*/
	    0,	/* Obj_Typed		 	Attr_Optional		*/
	    0,	/* Obj_Typed		 	Attr_Private		*/
	    0,	/* Obj_Typed		 	Attr_Public		*/
	    0,	/* Obj_Typed		 	Attr_Target		*/
	    0,	/* Obj_Typed		 	Attr_Equivalence	*/
	    0,	/* Obj_Typed		 	Attr_Save		*/
	    0,	/* Obj_Typed		 	Attr_Pointer		*/
	    0,	/* Obj_Typed		 	Attr_External		*/
	    0,	/* Obj_Typed		 	Attr_Intrinsic		*/
	    0,	/* Obj_Typed		 	Attr_Data_Init		*/
	    0,	/* Obj_Typed		 	Attr_Type		*/
		/* Defining twice - (5.1)			ANSI	*/

	    0,	/* Obj_Typed		 	Attr_Automatic		*/
	    0,	/* Obj_Typed		 	Attr_Co_Array		*/
	    0	/* Obj_Typed			Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Typed		 	Attr_Bind		*/
	,   0	/* Obj_Typed		 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/************************ Obj_Volatile	*************************/
	/* This is an extension and can be any kind of variable.        */
	/****************************************************************/

	{   0,	/* Obj_Volatile 		Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Volatile			Attr_Dimension		*/
	    0,	/* Obj_Volatile			Attr_Explicit_Shp_Arr	*/
	    0,	/* Obj_Volatile			Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Volatile			Attr_Deferred_Shp_Arr	*/
	    0,	/* Obj_Volatile			Attr_Assumed_Shp_Arr	*/
	    0,	/* Obj_Volatile			Attr_Allocatable	*/
	  550,	/* Obj_Volatile			Attr_Parameter		*/
	    0,	/* Obj_Volatile			Attr_Intent		*/
	    0,	/* Obj_Volatile			Attr_Optional		*/
	    0,	/* Obj_Volatile			Attr_Private		*/
	    0,	/* Obj_Volatile			Attr_Public		*/
	    0,	/* Obj_Volatile			Attr_Target		*/
	    0,	/* Obj_Volatile			Attr_Equivalence	*/
	    0,	/* Obj_Volatile			Attr_Save		*/
	    0,	/* Obj_Volatile			Attr_Pointer		*/
	  550,	/* Obj_Volatile			Attr_External		*/
	  550,	/* Obj_Volatile			Attr_Intrinsic		*/
	    0,	/* Obj_Volatile			Attr_Data_Init		*/
	    0,	/* Obj_Volatile			Attr_Type		*/
	    0,	/* Obj_Volatile			Attr_Co_Array		*/
	    0,	/* Obj_Volatile			Attr_Automatic		*/
	    0	/* Obj_Volatile			Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Volatile		 	Attr_Bind		*/
	, 550	/* Obj_Volatile		 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/******************* Obj_Copy_Assumed_Shape *********************/
	/****************************************************************/

	{   0,	/* Obj_Copy_Assumed_Shape	Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Copy_Assumed_Shape	Attr_Dimension		*/
	 1441,	/* Obj_Copy_Assumed_Shape	Attr_Explicit_Shp_Arr	*/
	 1441,	/* Obj_Copy_Assumed_Shape	Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Copy_Assumed_Shape	Attr_Deferred_Shp_Arr	*/
	    0,	/* Obj_Copy_Assumed_Shape	Attr_Assumed_Shp_Arr	*/
	 1441,	/* Obj_Copy_Assumed_Shape	Attr_Allocatable	*/
	 1441,	/* Obj_Copy_Assumed_Shape	Attr_Parameter		*/
	    0,	/* Obj_Copy_Assumed_Shape	Attr_Intent		*/
	    0,	/* Obj_Copy_Assumed_Shape	Attr_Optional		*/
	    0,	/* Obj_Copy_Assumed_Shape	Attr_Private		*/
	    0,	/* Obj_Copy_Assumed_Shape	Attr_Public		*/
	 1441,	/* Obj_Copy_Assumed_Shape	Attr_Target		*/
	 1441,	/* Obj_Copy_Assumed_Shape	Attr_Equivalence	*/
	 1441,	/* Obj_Copy_Assumed_Shape	Attr_Save		*/
	 1441,	/* Obj_Copy_Assumed_Shape	Attr_Pointer		*/
	 1441,	/* Obj_Copy_Assumed_Shape	Attr_External		*/
	 1441,	/* Obj_Copy_Assumed_Shape	Attr_Intrinsic		*/
	 1441,	/* Obj_Copy_Assumed_Shape	Attr_Data_Init		*/
	    0,	/* Obj_Copy_Assumed_Shape	Attr_Type		*/
	 1441,	/* Obj_Copy_Assumed_Shape	Attr_Co_Array		*/
	 1441, 	/* Obj_Copy_Assumed_Shape	Attr_Automatic		*/
	    0 	/* Obj_Copy_Assumed_Shape	Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,1441	/* Obj_Copy_Assumed_Shape	Attr_Bind		*/
	,1441	/* Obj_Copy_Assumed_Shape	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Auxiliary	*************************/
	/* Cray extension - Rules come from the CFT77 manual.		*/
	/* This must be a non-character array.	It can be a dummy arg, */
	/* but cannot be a function result.	It should not be mixed	*/
	/* with Fortran 90.						*/
	/***************************************************************/

	{1441,	/* Obj_Auxiliary		Attr_Assumed_Type_Ch	*/
		/* Definition says cannot be character.			*/

	    0,	/* Obj_Auxiliary		Attr_Dimension		*/
	    0,	/* Obj_Auxiliary		Attr_Explicit_Shp_Arr	*/
	    0,	/* Obj_Auxiliary		Attr_Assumed_Size_Arr	*/

	 1441,	/* Obj_Auxiliary		Attr_Deferred_Shp_Arr	*/
	 1441,	/* Obj_Auxiliary		Attr_Assumed_Shp_Arr	*/
	 1441,	/* Obj_Auxiliary		Attr_Allocatable	*/
		/* Cray extension - do not mix with Fortran 90		*/

	 1441,	/* Obj_Auxiliary		Attr_Parameter		*/
		/* Storage definition prohibits this.		 	*/

	    0,	/* Obj_Auxiliary		Attr_Intent		*/
	    0,	/* Obj_Auxiliary		Attr_Optional		*/
	    0,	/* Obj_Auxiliary		Attr_Private		*/
	    0,	/* Obj_Auxiliary		Attr_Public		*/

	 1441,	/* Obj_Auxiliary		Attr_Target		*/
		/* Cray extension - do not mix with Fortran 90		*/

	    0,	/* Obj_Auxiliary		Attr_Equivalence	*/
	 1441,	/* Obj_Auxiliary		Attr_Save		*/
		/* cft77 does allow this, but cft90 will disallow it.   */
		/* Something can't be in 2 seperate storage groups.	*/

	 1441,	/* Obj_Auxiliary		Attr_Pointer		*/
		/* Cray extension - do not mix with Fortran 90		*/

	 1441,	/* Obj_Auxiliary		Attr_External		*/
	 1441,	/* Obj_Auxiliary		Attr_Intrinsic		*/
	 1441,	/* Obj_Auxiliary		Attr_Data_Init		*/
		/* These are disallowed by cft77.			*/

	    0,	/* Obj_Auxiliary		Attr_Type		*/

	 1441,	/* Obj_Auxiliary		Attr_Co_Array		*/
	 1441,	/* Obj_Auxiliary		Attr_Automatic		*/
	    0	/* Obj_Auxiliary		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,1441	/* Obj_Auxiliary	 	Attr_Bind		*/
	,1441	/* Obj_Auxiliary	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/************************ Obj_Vfunction	*************************/
	/* This is a Cray extension.	It must be an external function,*/
	/* but cannot be declared EXTERNAL.  (From cft77 documentation) */
	/****************************************************************/

	{1441,	/* Obj_Vfunction		Attr_Assumed_Type_Ch	*/
	 1441,	/* Obj_Vfunction		Attr_Dimension		*/
	 1441,	/* Obj_Vfunction		Attr_Explicit_Shp_Arr	*/
	 1441,	/* Obj_Vfunction		Attr_Assumed_Size_Arr	*/
	 1441,	/* Obj_Vfunction		Attr_Deferred_Shp_Arr	*/
	 1441,	/* Obj_Vfunction		Attr_Assumed_Shp_Arr	*/
	 1441,	/* Obj_Vfunction		Attr_Allocatable	*/
	 1441,	/* Obj_Vfunction		Attr_Parameter		*/
	 1441,	/* Obj_Vfunction		Attr_Intent		*/
	 1441,	/* Obj_Vfunction		Attr_Optional		*/
	    0,	/* Obj_Vfunction		Attr_Private		*/
	    0,	/* Obj_Vfunction		Attr_Public		*/
	 1441,	/* Obj_Vfunction		Attr_Target		*/
	 1441,	/* Obj_Vfunction		Attr_Equivalence	*/
	 1441,	/* Obj_Vfunction		Attr_Save		*/
	 1441,	/* Obj_Vfunction		Attr_Pointer		*/
	 1441,	/* Obj_Vfunction		Attr_External		*/
	 1441,	/* Obj_Vfunction		Attr_Intrinsic		*/
	 1441,	/* Obj_Vfunction		Attr_Data_Init		*/
	    0,	/* Obj_Vfunction		Attr_Type		*/
	 1441,	/* Obj_Vfunction		Attr_Co_Array		*/
	 1441,	/* Obj_Vfunction		Attr_Automatic		*/
	 1441	/* Obj_Vfunction		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,1441	/* Obj_Vfunction	 	Attr_Bind		*/
	,1441	/* Obj_Vfunction	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_No_Side_Effects	*****************/
	/* This is a Cray extension.  It must be an external function,	*/
	/* and must not be a dummy procedure.	(From cft77 doc)	*/
	/****************************************************************/

	{   0,	/* Obj_No_Side_Effects		Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_No_Side_Effects		Attr_Dimension		*/
	    0,	/* Obj_No_Side_Effects		Attr_Explicit_Shp_Arr	*/
	 1441,	/* Obj_No_Side_Effects		Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_No_Side_Effects		Attr_Deferred_Shp_Arr	*/
	 1441,	/* Obj_No_Side_Effects		Attr_Assumed_Shp_Arr	*/
	 1441,	/* Obj_No_Side_Effects		Attr_Allocatable	*/
	 1441,	/* Obj_No_Side_Effects		Attr_Parameter		*/
	 1441,	/* Obj_No_Side_Effects		Attr_Intent		*/
	 1441,	/* Obj_No_Side_Effects		Attr_Optional		*/
	    0,	/* Obj_No_Side_Effects		Attr_Private		*/
	    0,	/* Obj_No_Side_Effects		Attr_Public		*/
	    0,	/* Obj_No_Side_Effects		Attr_Target		*/
	 1441,	/* Obj_No_Side_Effects		Attr_Equivalence	*/
	 1441,	/* Obj_No_Side_Effects		Attr_Save		*/
	    0,	/* Obj_No_Side_Effects		Attr_Pointer		*/
	    0,	/* Obj_No_Side_Effects		Attr_External		*/
	 1441,	/* Obj_No_Side_Effects		Attr_Intrinsic		*/
	 1441,	/* Obj_No_Side_Effects		Attr_Data_Init		*/
	    0,	/* Obj_No_Side_Effects		Attr_Type		*/
	 1441,	/* Obj_No_Side_Effects		Attr_Co_Array		*/
	 1441,	/* Obj_No_Side_Effects		Attr_Automatic		*/
	 1441	/* Obj_No_Side_Effects		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,1441	/* Obj_No_Side_Effects	 	Attr_Bind		*/
	,1441	/* Obj_No_Side_Effects	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/************************ Obj_Symmetric	*************************/
	/* This is a Cray extension.	Must be a local stack variable.	*/
	/****************************************************************/

	{1441,	/* Obj_Symmetric		Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Symmetric		Attr_Dimension		*/
	    0,	/* Obj_Symmetric		Attr_Explicit_Shp_Arr	*/
	 1441,	/* Obj_Symmetric		Attr_Assumed_Size_Arr	*/
	 1441,	/* Obj_Symmetric		Attr_Deferred_Shp_Arr	*/
	 1441,	/* Obj_Symmetric		Attr_Assumed_Shp_Arr	*/
	 1441,	/* Obj_Symmetric		Attr_Allocatable	*/
	 1441,	/* Obj_Symmetric		Attr_Parameter		*/
	 1441,	/* Obj_Symmetric		Attr_Intent		*/
	 1441,	/* Obj_Symmetric		Attr_Optional		*/
	    0,	/* Obj_Symmetric		Attr_Private		*/
	    0,	/* Obj_Symmetric		Attr_Public		*/
	 1441,	/* Obj_Symmetric		Attr_Target		*/
	 1441,	/* Obj_Symmetric		Attr_Equivalence	*/
	 1441,	/* Obj_Symmetric		Attr_Save		*/
	 1441,	/* Obj_Symmetric		Attr_Pointer		*/
	 1441,	/* Obj_Symmetric		Attr_External		*/
	 1441,	/* Obj_Symmetric		Attr_Intrinsic		*/
	 1441,	/* Obj_Symmetric		Attr_Data_Init		*/
	    0,	/* Obj_Symmetric		Attr_Type		*/
	 1441,	/* Obj_Symmetric		Attr_Co_Array		*/
	 1441,	/* Obj_Symmetric		Attr_Automatic		*/
	    0	/* Obj_Symmetric		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,1441	/* Obj_Symmetric	 	Attr_Bind		*/
	,1441	/* Obj_Symmetric	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************** Obj_Inline	*************************/
	/* This is a Cray extension.					*/
	/****************************************************************/

	{   0,	/* Obj_Inline			Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Inline			Attr_Dimension		*/
	    0,	/* Obj_Inline			Attr_Explicit_Shp_Arr	*/
	 1441,	/* Obj_Inline			Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Inline			Attr_Deferred_Shp_Arr	*/
	 1441,	/* Obj_Inline			Attr_Assumed_Shp_Arr	*/
	 1441,	/* Obj_Inline			Attr_Allocatable	*/
	 1441,	/* Obj_Inline			Attr_Parameter		*/
	 1441,	/* Obj_Inline			Attr_Intent		*/
	 1441,	/* Obj_Inline			Attr_Optional		*/
	    0,	/* Obj_Inline			Attr_Private		*/
	    0,	/* Obj_Inline			Attr_Public		*/
	    0,	/* Obj_Inline			Attr_Target		*/
	 1441,	/* Obj_Inline			Attr_Equivalence	*/
	 1441,	/* Obj_Inline			Attr_Save		*/
	    0,	/* Obj_Inline			Attr_Pointer		*/
	    0,	/* Obj_Inline			Attr_External		*/
	    0,	/* Obj_Inline			Attr_Intrinsic		*/
	 1441,	/* Obj_Inline			Attr_Data_Init		*/
	    0,	/* Obj_Inline			Attr_Type		*/
	 1441,	/* Obj_Inline			Attr_Co_Array		*/
	    0,	/* Obj_Inline			Attr_Automatic		*/
	 1441	/* Obj_Inline			Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,1441	/* Obj_Inline		 	Attr_Bind		*/
	,1441	/* Obj_Inline		 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************** Obj_Ipa *****************************/
	/* This is a Cray extension.					*/
	/****************************************************************/

	{   0,	/* Obj_Ipa			Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Ipa			Attr_Dimension		*/
	    0,	/* Obj_Ipa			Attr_Explicit_Shp_Arr	*/
	 1441,	/* Obj_Ipa			Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Ipa			Attr_Deferred_Shp_Arr	*/
	 1441,	/* Obj_Ipa			Attr_Assumed_Shp_Arr	*/
	 1441,	/* Obj_Ipa			Attr_Allocatable	*/
	 1441,	/* Obj_Ipa			Attr_Parameter		*/
	 1441,	/* Obj_Ipa			Attr_Intent		*/
	 1441,	/* Obj_Ipa			Attr_Optional		*/
	    0,	/* Obj_Ipa			Attr_Private		*/
	    0,	/* Obj_Ipa			Attr_Public		*/
	    0,	/* Obj_Ipa			Attr_Target		*/
	 1441,	/* Obj_Ipa			Attr_Equivalence	*/
	 1441,	/* Obj_Ipa			Attr_Save		*/
	    0,	/* Obj_Ipa			Attr_Pointer		*/
	    0,	/* Obj_Ipa			Attr_External		*/
	    0,	/* Obj_Ipa			Attr_Intrinsic		*/
	 1441,	/* Obj_Ipa			Attr_Data_Init		*/
	    0,	/* Obj_Ipa			Attr_Type		*/
	 1441,	/* Obj_Ipa			Attr_Co_Array		*/
	    0,	/* Obj_Ipa			Attr_Automatic		*/
	 1441	/* Obj_Ipa			Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,1441	/* Obj_Ipa		 	Attr_Bind		*/
	,1441	/* Obj_Ipa		 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************* Obj_Align_Symbol *********************/
	/* This is an SGI extension.					*/
	/****************************************************************/

	{1441,	/* Obj_Align_Symbol		Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Align_Symbol		Attr_Dimension		*/
	    0,	/* Obj_Align_Symbol		Attr_Explicit_Shp_Arr	*/
	 1441,	/* Obj_Align_Symbol		Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Align_Symbol		Attr_Deferred_Shp_Arr	*/
	 1441,	/* Obj_Align_Symbol		Attr_Assumed_Shp_Arr	*/
	 1441,	/* Obj_Align_Symbol		Attr_Allocatable	*/
	 1441,	/* Obj_Align_Symbol		Attr_Parameter		*/
	 1441,	/* Obj_Align_Symbol		Attr_Intent		*/
	 1441,	/* Obj_Align_Symbol		Attr_Optional		*/
	    0,	/* Obj_Align_Symbol		Attr_Private		*/
	    0,	/* Obj_Align_Symbol		Attr_Public		*/
	    0,	/* Obj_Align_Symbol		Attr_Target		*/
	 1441,	/* Obj_Align_Symbol		Attr_Equivalence	*/
	    0,	/* Obj_Align_Symbol		Attr_Save		*/
	    0,	/* Obj_Align_Symbol		Attr_Pointer		*/
	 1441,	/* Obj_Align_Symbol		Attr_External		*/
	 1441,	/* Obj_Align_Symbol		Attr_Intrinsic		*/
	    0,	/* Obj_Align_Symbol		Attr_Data_Init		*/
	    0,	/* Obj_Align_Symbol		Attr_Type		*/
	 1441,	/* Obj_Align_Symbol		Attr_Co_Array		*/
	    0,	/* Obj_Align_Symbol		Attr_Automatic		*/
	    0	/* Obj_Align_Symbol		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,1441	/* Obj_Align_Symbol	 	Attr_Bind		*/
	,1441	/* Obj_Align_Symbol	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/************************* Obj_Fill_Symbol **********************/
	/* This is an SGI extension.					*/
	/****************************************************************/

	{1441,	/* Obj_Fill_Symbol		Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Fill_Symbol		Attr_Dimension		*/
	    0,	/* Obj_Fill_Symbol		Attr_Explicit_Shp_Arr	*/
	 1441,	/* Obj_Fill_Symbol		Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Fill_Symbol		Attr_Deferred_Shp_Arr	*/
	 1441,	/* Obj_Fill_Symbol		Attr_Assumed_Shp_Arr	*/
	 1441,	/* Obj_Fill_Symbol		Attr_Allocatable	*/
	 1441,	/* Obj_Fill_Symbol		Attr_Parameter		*/
	 1441,	/* Obj_Fill_Symbol		Attr_Intent		*/
	 1441,	/* Obj_Fill_Symbol		Attr_Optional		*/
	    0,	/* Obj_Fill_Symbol		Attr_Private		*/
	    0,	/* Obj_Fill_Symbol		Attr_Public		*/
	    0,	/* Obj_Fill_Symbol		Attr_Target		*/
	 1441,	/* Obj_Fill_Symbol		Attr_Equivalence	*/
	    0,	/* Obj_Fill_Symbol		Attr_Save		*/
	    0,	/* Obj_Fill_Symbol		Attr_Pointer		*/
	 1441,	/* Obj_Fill_Symbol		Attr_External		*/
	 1441,	/* Obj_Fill_Symbol		Attr_Intrinsic		*/
	    0,	/* Obj_Fill_Symbol		Attr_Data_Init		*/
	    0,	/* Obj_Fill_Symbol		Attr_Type		*/
	 1441,	/* Obj_Fill_Symbol		Attr_Co_Array		*/
	    0,	/* Obj_Fill_Symbol		Attr_Automatic		*/
	    0	/* Obj_Fill_Symbol		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,1441	/* Obj_Fill_Symbol	 	Attr_Bind		*/
	,1441	/* Obj_Fill_Symbol	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/************************* Obj_Section_Gp ***********************/
	/* This is an SGI extension.					*/
	/****************************************************************/

	{1441,	/* Obj_Section_Gp		Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Section_Gp		Attr_Dimension		*/
	    0,	/* Obj_Section_Gp		Attr_Explicit_Shp_Arr	*/
	 1441,	/* Obj_Section_Gp		Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Section_Gp		Attr_Deferred_Shp_Arr	*/
	 1441,	/* Obj_Section_Gp		Attr_Assumed_Shp_Arr	*/
	 1441,	/* Obj_Section_Gp		Attr_Allocatable	*/
	 1441,	/* Obj_Section_Gp		Attr_Parameter		*/
	 1441,	/* Obj_Section_Gp		Attr_Intent		*/
	 1441,	/* Obj_Section_Gp		Attr_Optional		*/
	    0,	/* Obj_Section_Gp		Attr_Private		*/
	    0,	/* Obj_Section_Gp		Attr_Public		*/
	    0,	/* Obj_Section_Gp		Attr_Target		*/
	    0,	/* Obj_Section_Gp		Attr_Equivalence	*/
	    0,	/* Obj_Section_Gp		Attr_Save		*/
	    0,	/* Obj_Section_Gp		Attr_Pointer		*/
	 1441,	/* Obj_Section_Gp		Attr_External		*/
	 1441,	/* Obj_Section_Gp		Attr_Intrinsic		*/
	    0,	/* Obj_Section_Gp		Attr_Data_Init		*/
	    0,	/* Obj_Section_Gp		Attr_Type		*/
	 1441,	/* Obj_Section_Gp		Attr_Co_Array		*/
	 1441,	/* Obj_Section_Gp		Attr_Automatic		*/
	    0	/* Obj_Section_Gp		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,1441	/* Obj_Section_Gp	 	Attr_Bind		*/
	,1441	/* Obj_Section_Gp	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/********************** Obj_Section_Non_Gp **********************/
	/* This is an SGI extension.					*/
	/****************************************************************/

	{1441,	/* Obj_Section_Non_Gp		Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Section_Non_Gp		Attr_Dimension		*/
	    0,	/* Obj_Section_Non_Gp		Attr_Explicit_Shp_Arr	*/
	 1441,	/* Obj_Section_Non_Gp		Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Section_Non_Gp		Attr_Deferred_Shp_Arr	*/
	 1441,	/* Obj_Section_Non_Gp		Attr_Assumed_Shp_Arr	*/
	 1441,	/* Obj_Section_Non_Gp		Attr_Allocatable	*/
	 1441,	/* Obj_Section_Non_Gp		Attr_Parameter		*/
	 1441,	/* Obj_Section_Non_Gp		Attr_Intent		*/
	 1441,	/* Obj_Section_Non_Gp		Attr_Optional		*/
	    0,	/* Obj_Section_Non_Gp		Attr_Private		*/
	    0,	/* Obj_Section_Non_Gp		Attr_Public		*/
	    0,	/* Obj_Section_Non_Gp		Attr_Target		*/
	    0,	/* Obj_Section_Non_Gp		Attr_Equivalence	*/
	    0,	/* Obj_Section_Non_Gp		Attr_Save		*/
	    0,	/* Obj_Section_Non_Gp		Attr_Pointer		*/
	 1441,	/* Obj_Section_Non_Gp		Attr_External		*/
	 1441,	/* Obj_Section_Non_Gp		Attr_Intrinsic		*/
	    0,	/* Obj_Section_Non_Gp		Attr_Data_Init		*/
	    0,	/* Obj_Section_Non_Gp		Attr_Type		*/
	 1441,	/* Obj_Section_Non_Gp		Attr_Co_Array		*/
	 1441,	/* Obj_Section_Non_Gp		Attr_Automatic		*/
	    0	/* Obj_Section_Non_Gp		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,1440	/* Obj_Section_Non_Gp	 	Attr_Bind		*/
	,1440	/* Obj_Section_Non_Gp	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/************************ Obj_Ignore_TKR ************************/
	/****************************************************************/

	{   0,	/* Obj_Ignore_TKR		Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Ignore_TKR		Attr_Dimension		*/
	    0,	/* Obj_Ignore_TKR		Attr_Explicit_Shp_Arr	*/
	    0,	/* Obj_Ignore_TKR		Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Ignore_TKR		Attr_Deferred_Shp_Arr	*/
	 1441,	/* Obj_Ignore_TKR		Attr_Assumed_Shp_Arr	*/
	 1441,	/* Obj_Ignore_TKR		Attr_Allocatable	*/
	 1441,	/* Obj_Ignore_TKR		Attr_Parameter		*/
	    0,	/* Obj_Ignore_TKR		Attr_Intent		*/
	    0,	/* Obj_Ignore_TKR		Attr_Optional		*/
	    0,	/* Obj_Ignore_TKR		Attr_Private		*/
	    0,	/* Obj_Ignore_TKR		Attr_Public		*/
	    0,	/* Obj_Ignore_TKR		Attr_Target		*/
	 1441,	/* Obj_Ignore_TKR		Attr_Equivalence	*/
	 1441,	/* Obj_Ignore_TKR		Attr_Save		*/
#ifdef KEY /* Bug 14150 */
	    0,	/* Obj_Ignore_TKR		Attr_Pointer		*/
	    0,	/* Obj_Ignore_TKR		Attr_External		*/
#else /* KEY Bug 14150 */
	 1441,	/* Obj_Ignore_TKR		Attr_Pointer		*/
	 1441,	/* Obj_Ignore_TKR		Attr_External		*/
#endif /* KEY Bug 14150 */
	 1441,	/* Obj_Ignore_TKR		Attr_Intrinsic		*/
	 1441,	/* Obj_Ignore_TKR		Attr_Data_Init		*/
	    0,	/* Obj_Ignore_TKR		Attr_Type		*/
	    0,	/* Obj_Ignore_TKR		Attr_Co_Array		*/
	 1441,	/* Obj_Ignore_TKR		Attr_Automatic		*/
	    0 	/* Obj_Ignore_TKR		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,1441	/* Obj_Ignore_TKR	 	Attr_Bind		*/
	,1441	/* Obj_Ignore_TKR	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/******************** Obj_Optional_Dir	*************************/
	/****************************************************************/

	{   0,	/* Obj_Optional_Dir		Attr_Assumed_Type_Ch	*/
	 1441,	/* Obj_Optional_Dir		Attr_Dimension		*/
	 1441,	/* Obj_Optional_Dir		Attr_Explicit_Shp_Arr	*/
	 1441,	/* Obj_Optional_Dir		Attr_Assumed_Size_Arr	*/
	 1441,	/* Obj_Optional_Dir		Attr_Deferred_Shp_Arr	*/
	 1441,	/* Obj_Optional_Dir		Attr_Assumed_Shp_Arr	*/
	 1441,	/* Obj_Optional_Dir		Attr_Allocatable	*/
	 1441,	/* Obj_Optional_Dir		Attr_Parameter		*/
	 1441,	/* Obj_Optional_Dir		Attr_Intent		*/
	    0,	/* Obj_Optional_Dir		Attr_Optional		*/
	    0,	/* Obj_Optional_Dir		Attr_Private		*/
	    0,	/* Obj_Optional_Dir		Attr_Public		*/
	 1441,	/* Obj_Optional_Dir		Attr_Target		*/
	 1441,	/* Obj_Optional_Dir		Attr_Equivalence	*/
	 1441,	/* Obj_Optional_Dir		Attr_Save		*/
	 1441,	/* Obj_Optional_Dir		Attr_Pointer		*/
	    0,	/* Obj_Optional_Dir		Attr_External		*/
	 1441,	/* Obj_Optional_Dir		Attr_Intrinsic		*/
	 1441,	/* Obj_Optional_Dir		Attr_Data_Init		*/
	    0,	/* Obj_Optional_Dir		Attr_Type		*/
	 1441,	/* Obj_Optional_Dir		Attr_Co_Array		*/
	 1441,	/* Obj_Optional_Dir		Attr_Automatic		*/
	 1441	/* Obj_Optional_Dir		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,1441	/* Obj_Optional_Dir	 	Attr_Bind		*/
	,1441	/* Obj_Optional_Dir	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/************************ Obj_Name ******************************/
	/* This is declared on the C function directive.		*/
	/****************************************************************/

	{   0,	/* Obj_Name			Attr_Assumed_Type_Ch	*/
	 1441,	/* Obj_Name			Attr_Dimension		*/
	 1441,	/* Obj_Name			Attr_Explicit_Shp_Arr	*/
	 1441,	/* Obj_Name			Attr_Assumed_Size_Arr	*/
	 1441,	/* Obj_Name			Attr_Deferred_Shp_Arr	*/
	 1441,	/* Obj_Name			Attr_Assumed_Shp_Arr	*/
	 1441,	/* Obj_Name			Attr_Allocatable	*/
	 1441,	/* Obj_Name			Attr_Parameter		*/
	 1441,	/* Obj_Name			Attr_Intent		*/
	    0,	/* Obj_Name			Attr_Optional		*/
	    0,	/* Obj_Name			Attr_Private		*/
	    0,	/* Obj_Name			Attr_Public		*/
	 1441,	/* Obj_Name			Attr_Target		*/
	 1441,	/* Obj_Name			Attr_Equivalence	*/
	 1441,	/* Obj_Name			Attr_Save		*/
	 1441,	/* Obj_Name			Attr_Pointer		*/
	    0,	/* Obj_Name			Attr_External		*/
	 1441,	/* Obj_Name			Attr_Intrinsic		*/
	 1441,	/* Obj_Name			Attr_Data_Init		*/
	    0,	/* Obj_Name			Attr_Type		*/
	 1441,	/* Obj_Name			Attr_Co_Array		*/
	 1441,	/* Obj_Name			Attr_Automatic		*/
	 1441	/* Obj_Name			Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,1441	/* Obj_Name		 	Attr_Bind		*/
	,1441	/* Obj_Name		 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/************************ Obj_Cri_Ptr ***************************/
	/* From cft77's documentation - A Cray pointer cannot be a	*/
	/* constant, an array, a statement function or an external	*/
	/* function.  It can be in common and be a darg.  A Cray	*/
	/* pointer is considered to be a data type.  It must be		*/
	/* declared to be a pointer, before data initializing.	 	*/
	/****************************************************************/

	{552,	/* Obj_Cri_Ptr			Attr_Assumed_Type_Ch	*/
		/* Cray extension - Would be duplicate typing.		*/

	  552,	/* Obj_Cri_Ptr			Attr_Dimension		*/
	  552,	/* Obj_Cri_Ptr			Attr_Explicit_Shp_Arr	*/
	  552,	/* Obj_Cri_Ptr			Attr_Assumed_Size_Arr	*/
	  552,	/* Obj_Cri_Ptr			Attr_Deferred_Shp_Arr	*/
	  552,	/* Obj_Cri_Ptr			Attr_Assumed_Shp_Arr	*/
	  552,	/* Obj_Cri_Ptr			Attr_Allocatable	*/
	  552,	/* Obj_Cri_Ptr			Attr_Parameter		*/

	  552,	/* Obj_Cri_Ptr			Attr_Intent		*/
	    0,	/* Obj_Cri_Ptr			Attr_Optional		*/
	    0,	/* Obj_Cri_Ptr			Attr_Private		*/
	    0,	/* Obj_Cri_Ptr			Attr_Public		*/
	  552,	/* Obj_Cri_Ptr			Attr_Target		*/
	        /* Cray extension - Do not mix Fortran 90		*/
	  0,	/* Obj_Cri_Ptr			Attr_Equivalence	*/
	  0,	/* Obj_Cri_Ptr			Attr_Save		*/ 
	  552,	/* Obj_Cri_Ptr			Attr_Pointer		*/
		/* Cray extentsion - Do not mix Fortran 90		*/

	  552,	/* Obj_Cri_Ptr			Attr_External		*/
	  552,	/* Obj_Cri_Ptr			Attr_Intrinsic		*/
		/* Can't be a function result.				*/

	  552,	/* Obj_Cri_Ptr			Attr_Data_Init		*/

	  552,	/* Obj_Cri_Ptr			Attr_Type		*/
		/* This would be duplicate typing.			*/

	  552,	/* Obj_Cri_Ptr			Attr_Co_Array		*/
	    0,	/* Obj_Cri_Ptr			Attr_Automatic		*/
	    0	/* Obj_Cri_Ptr			Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 552	/* Obj_Cri_Ptr		 	Attr_Bind		*/
	, 552	/* Obj_Cri_Ptr		 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Cri_Pointee ***********************/
	/* From cft77's documentation - A Cray pointee cannot be a	*/
	/* constant, in common, saved, data initialized or equivalenced	*/
	/* It cannot be a darg or a function result.			*/
	/****************************************************************/

	{ 552,	/* Obj_Cri_Pointee		Attr_Assumed_Type_Ch	*/
		/* This is how to define a Cray character pointer.	*/

	    0,	/* Obj_Cri_Pointee		Attr_Dimension		*/
	    0,	/* Obj_Cri_Pointee		Attr_Explicit_Shp_Arr	*/

	    0,	/* Obj_Cri_Pointee		Attr_Assumed_Size_Arr	*/
		/* If the Assumed size array is a pointee, it cannot	*/
		/* be a dummy argument.	This is a Cray extension.	*/

	  552,	/* Obj_Cri_Pointee		Attr_Deferred_Shp_Arr	*/
	  552,	/* Obj_Cri_Pointee		Attr_Assumed_Shp_Arr	*/
	  552,	/* Obj_Cri_Pointee		Attr_Allocatable	*/
		/* Do not mix Fortran 90 with a Cray extension.		*/

	  552,	/* Obj_Cri_Pointee		Attr_Parameter		*/

	  552,	/* Obj_Cri_Pointee		Attr_Intent		*/
	  552,	/* Obj_Cri_Pointee		Attr_Optional		*/
		/* Can't be a darg.					*/

	    0,	/* Obj_Cri_Pointee		Attr_Private		*/
	    0,	/* Obj_Cri_Pointee		Attr_Public		*/

	  552,	/* Obj_Cri_Pointee		Attr_Target		*/
		/* Do not mix with Fortran 90			 	*/

	  552,	/* Obj_Cri_Pointee		Attr_Equivalence	*/
	  552,	/* Obj_Cri_Pointee		Attr_Save		*/
	
	  552,	/* Obj_Cri_Pointee		Attr_Pointer		*/
		/* Do not mix Fortran 90 with Cray extensions.		*/

	  552,	/* Obj_Cri_Pointee		Attr_External		*/
	  552,	/* Obj_Cri_Pointee		Attr_Intrinsic		*/
	  552,	/* Obj_Cri_Pointee		Attr_Data_Init		*/

	    0,	/* Obj_Cri_Pointee		Attr_Type		*/

	  552,	/* Obj_Cri_Pointee		Attr_Co_Array		*/
	  552,	/* Obj_Cri_Pointee		Attr_Automatic		*/
	    0	/* Obj_Cri_Pointee		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 552	/* Obj_Cri_Pointee	 	Attr_Bind		*/
	, 552	/* Obj_Cri_Pointee	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Cri_Ch_Pointee ********************/
	/* From cft77's documentation - A Cray pointee cannot be a	*/
	/* constant, in common, saved, data initialized or equivalenced	*/
	/* It cannot be a darg or a function result.  A Cray character	*/
	/* pointee cannot be an array.				 	*/
	/****************************************************************/

	{   0,	/* Obj_Cri_Ch_Pointee		Attr_Assumed_Type_Ch	*/
		/* This is how to define a Cray character pointer.	*/

# if defined(_EXTENDED_CRI_CHAR_POINTER)
	    0,	/* Obj_Cri_Ch_Pointee		Attr_Dimension		*/
	    0,	/* Obj_Cri_Ch_Pointee		Attr_Explicit_Shp_Arr	*/
	    0,	/* Obj_Cri_Ch_Pointee		Attr_Assumed_Size_Arr	*/
# else
	1407,	/* Obj_Cri_Ch_Pointee		Attr_Dimension		*/
	1407,	/* Obj_Cri_Ch_Pointee		Attr_Explicit_Shp_Arr	*/
	1407,	/* Obj_Cri_Ch_Pointee		Attr_Assumed_Size_Arr	*/
# endif
	  552,	/* Obj_Cri_Ch_Pointee		Attr_Deferred_Shp_Arr	*/
	  552,	/* Obj_Cri_Ch_Pointee		Attr_Assumed_Shp_Arr	*/
	  552,	/* Obj_Cri_Ch_Pointee		Attr_Allocatable	*/

	  552,	/* Obj_Cri_Ch_Pointee		Attr_Parameter		*/

	  552,	/* Obj_Cri_Ch_Pointee		Attr_Intent		*/
	  552,	/* Obj_Cri_Ch_Pointee		Attr_Optional		*/
		/* Can't be a darg.					*/

	    0,	/* Obj_Cri_Ch_Pointee		Attr_Private		*/
	    0,	/* Obj_Cri_Ch_Pointee		Attr_Public		*/

	  552,	/* Obj_Cri_Ch_Pointee		Attr_Target		*/
		/* Do not mix with Fortran 90				*/

	  552,	/* Obj_Cri_Ch_Pointee		Attr_Equivalence	*/
	  552,	/* Obj_Cri_Ch_Pointee		Attr_Save		*/
	
	  552,	/* Obj_Cri_Ch_Pointee		Attr_Pointer		*/
		/* Do not mix Fortran 90 with Cray extensions.		*/

	  552,	/* Obj_Cri_Ch_Pointee		Attr_External		*/
	  552,	/* Obj_Cri_Ch_Pointee		Attr_Intrinsic		*/
	  552,	/* Obj_Cri_Ch_Pointee		Attr_Data_Init		*/

	    0,	/* Obj_Cri_Ch_Pointee		Attr_Type		*/

	  552,	/* Obj_Cri_Ch_Pointee		Attr_Co_Array		*/
	  552,	/* Obj_Cri_Ch_Pointee		Attr_Automatic		*/
	    0	/* Obj_Cri_Ch_Pointee		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 552	/* Obj_Cri_Ch_Pointee	 	Attr_Bind		*/
	, 552	/* Obj_Cri_Ch_Pointee	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/************************ Obj_Ntry_Func_Result ******************/
	/* The result of a Function, specified on an entry statement.	*/
	/****************************************************************/

	{   0,	/* Obj_Ntry_Func_Result		Attr_Assumed_Type_Ch	*/
		/* Assumed type char can be the result of the current 	*/
		/* function being compiled.	(5.1.1.5)		*/

	    0,	/* Obj_Ntry_Func_Result		Attr_Dimension		*/
	    0,	/* Obj_Ntry_Func_Result		Attr_Explicit_Shp_Arr	*/
		/* legal (5.1.2.4.1)					*/

	  552,	/* Obj_Ntry_Func_Result		Attr_Assumed_Size_Arr	*/
		/* Dargs can't be function results.	(14.1.2)	*/

	    0,	/* Obj_Ntry_Func_Result		Attr_Deferred_Shp_Arr	*/
		/* Func results can be pointers and pointers can be	*/
		/* deferred shape arrays.	12.5.2.2 discussion.	*/

	  552,	/* Obj_Ntry_Func_Result		Attr_Assumed_Shp_Arr	*/
		/* Assumed shape must be a dummy arg.		 	*/
		/* Dargs can't be function results.	(14.1.2)	*/

	  552,	/* Obj_Ntry_Func_Result		Attr_Allocatable	*/
		/* 5.1 constraint					*/

	  552,	/* Obj_Ntry_Func_Result		Attr_Parameter		*/
		/* A constant cannot be a function result.	(14.1.2)*/

	  552,	/* Obj_Ntry_Func_Result		Attr_Intent		*/
	  552,	/* Obj_Ntry_Func_Result		Attr_Optional		*/
		/* 14.1.2						*/

	    0,	/* Obj_Ntry_Func_Result		Attr_Private		*/
	    0,	/* Obj_Ntry_Func_Result		Attr_Public		*/

	    0,	/* Obj_Ntry_Func_Result		Attr_Target		*/
		/* Target may be specified for all function results,	*/
		/* but the function result may be used as a target	*/
		/* only if it is the result of the current function	*/
		/* (7.5.2) pointer assignment			 	*/

	  552,	/* Obj_Ntry_Func_Result		Attr_Equivalence	*/
		/* (5.5.1 constraint)				 	*/

	  552,	/* Obj_Ntry_Func_Result		Attr_Save		*/
		/* 5.2.4 constraint					*/

	    0,	/* Obj_Ntry_Func_Result		Attr_Pointer		*/

	  552,	/* Obj_Ntry_Func_Result		Attr_External		*/
	  552,	/* Obj_Ntry_Func_Result		Attr_Intrinsic		*/
		/* 12.5.2.5						*/

	  552,	/* Obj_Ntry_Func_Result		Attr_Data_Init		*/
		/* 5.2.9 constraint					*/

	    0,	/* Obj_Ntry_Func_Result		Attr_Type		*/

	  552,	/* Obj_Ntry_Func_Result		Attr_Co_Array		*/
	    0,	/* Obj_Ntry_Func_Result		Attr_Automatic		*/
	  552	/* Obj_Ntry_Func_Result		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 552	/* Obj_Ntry_Func_Result	 	Attr_Bind		*/
	, 552	/* Obj_Ntry_Func_Result	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Dummy_Arg	*************************/
	/* The result of a Function, specified on an entry statement.	*/
	/****************************************************************/

	{   0,	/* Obj_Dummy_Arg		Attr_Assumed_Type_Ch	*/
		/* (5.1.1.5) - legal					*/

	    0,	/* Obj_Dummy_Arg		Attr_Dimension		*/
	    0,	/* Obj_Dummy_Arg		Attr_Explicit_Shp_Arr	*/
	    0,	/* Obj_Dummy_Arg		Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Dummy_Arg		Attr_Deferred_Shp_Arr	*/
	    0,	/* Obj_Dummy_Arg		Attr_Assumed_Shp_Arr	*/
		/* 5.1.2.4.1, 5.1.2.4.4, 5.1.2.4.3, 5.1.2.4.2	 */

	  552,	/* Obj_Dummy_Arg		Attr_Allocatable	*/
		/* 5.1 constraint					*/

	  552,	/* Obj_Dummy_Arg		Attr_Parameter		*/
		/* A constant and a darg may not be the same.	(14.1.2)*/

	    0,	/* Obj_Dummy_Arg		Attr_Intent		*/
	    0,	/* Obj_Dummy_Arg		Attr_Optional		*/

	    0,	/* Obj_Dummy_Arg		Attr_Private		*/
	    0,	/* Obj_Dummy_Arg		Attr_Public		*/
		/* These are illegal, but won't issue.	The ENTRY	*/
		/* statement is not allowed in a module.	(11.3)	*/

	    0,	/* Obj_Dummy_Arg		Attr_Target		*/

	  552,	/* Obj_Dummy_Arg		Attr_Equivalence	*/
		/* 5.5.1						*/

	  552,	/* Obj_Dummy_Arg		Attr_Save		*/
		/* 5.2.4						*/

	    0,	/* Obj_Dummy_Arg		Attr_Pointer		*/
	    0,	/* Obj_Dummy_Arg		Attr_External		*/

	  552,	/* Obj_Dummy_Arg		Attr_Intrinsic		*/
		/* Intrinsic procs cannot be dummy procs. (14.1.2)	*/

	  552,	/* Obj_Dummy_Arg		Attr_Data_Init		*/
		/* Illegal - 5.2.9					*/

	    0,	/* Obj_Dummy_Arg		Attr_Type		*/
	    0,	/* Obj_Dummy_Arg		Attr_Co_Array		*/
	  552,	/* Obj_Dummy_Arg		Attr_Automatic		*/
	    0	/* Obj_Dummy_Arg		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 552	/* Obj_Dummy_Arg	 	Attr_Bind		*/
	,   0	/* Obj_Dummy_Arg	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Common_Obj ************************/
	/*	A common-block object must not be a dummy argument, an	*/
	/*	allocatable array, an automatic object, a function name,*/
	/*	an entry name or a result name.	(5.5.2)			*/
	/****************************************************************/

	{  552,	/* Obj_Common_Obj		Attr_Assumed_Type_Ch	*/
		/* Assumed type char must be darg, constant, or func	*/
		/* rslt. (5.1.1.5) - Common cannot be these. (5.5.2)	*/

	    0,	/* Obj_Common_Obj		Attr_Dimension		*/
	    0,	/* Obj_Common_Obj		Attr_Explicit_Shp_Arr	*/

	  552,	/* Obj_Common_Obj		Attr_Assumed_Size_Arr	*/
		/* This is a darg	(5.1.2.4.4)			*/

	    0,	/* Obj_Common_Obj		Attr_Deferred_Shp_Arr	*/

	  552,	/* Obj_Common_Obj		Attr_Assumed_Shp_Arr	*/
		/* This is a darg	(5.1.2.4.2)			*/
		
	  552,	/* Obj_Common_Obj		Attr_Allocatable	*/
		/* 5.5.2						*/

	  552,	/* Obj_Common_Obj		Attr_Parameter		*/
		/* 5.5.2 - Common is a variable.  Variable can't be	*/
		/* constant	(14.1.2)				*/

	  552,	/* Obj_Common_Obj		Attr_Intent		*/
	  552,	/* Obj_Common_Obj		Attr_Optional		*/
		/* 5.5.2 - These are dargs.				*/

	    0,	/* Obj_Common_Obj		Attr_Private		*/
	    0,	/* Obj_Common_Obj		Attr_Public		*/
	    0,	/* Obj_Common_Obj		Attr_Target		*/
	    0,	/* Obj_Common_Obj		Attr_Equivalence	*/

	  552,	/* Obj_Common_Obj		Attr_Save		*/
		/* 5.2.4						*/

	    0,	/* Obj_Common_Obj		Attr_Pointer		*/

	  552,	/* Obj_Common_Obj		Attr_External		*/
	  552,	/* Obj_Common_Obj		Attr_Intrinsic		*/
		/* These are procedures - 5.5.2				*/

	    0,	/* Obj_Common_Obj		Attr_Data_Init		*/
	    0,	/* Obj_Common_Obj		Attr_Type		*/

	    0,	/* Obj_Common_Obj		Attr_Co_Array		*/
	  552,	/* Obj_Common_Obj		Attr_Automatic		*/
	    0	/* Obj_Common_Obj		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 552	/* Obj_Common_Obj	 	Attr_Bind		*/
	, 552	/* Obj_Common_Obj	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Namelist_Obj **********************/
	/* A namelist object must not be an array dummy argument with	*/
	/* a non-constant bound, a variable with a non-constant char	*/
	/* length, an automatic object, a pointer or an allocatable	*/
	/* array. (5.4) constraint					*/
	/****************************************************************/

	{ 552,	/* Obj_Namelist_Obj		Attr_Assumed_Type_Ch	*/
		/* 5.4							*/

	    0,	/* Obj_Namelist_Obj		Attr_Dimension		*/
	    0,	/* Obj_Namelist_Obj		Attr_Explicit_Shp_Arr	*/

	  552,	/* Obj_Namelist_Obj		Attr_Assumed_Size_Arr	*/
	  552,	/* Obj_Namelist_Obj		Attr_Deferred_Shp_Arr	*/
	  552,	/* Obj_Namelist_Obj		Attr_Assumed_Shp_Arr	*/
	  552,	/* Obj_Namelist_Obj		Attr_Allocatable	*/
		/* 5.4							*/

	  552,	/* Obj_Namelist_Obj		Attr_Parameter		*/
		/* 5.4 - A namelist object must be a variable.	A	*/
		/* variable cannot be a constant.	(14.1.2)	*/

	    0,	/* Obj_Namelist_Obj		Attr_Intent		*/
	    0,	/* Obj_Namelist_Obj		Attr_Optional		*/

	    0,	/* Obj_Namelist_Obj		Attr_Private		*/
	    0,	/* Obj_Namelist_Obj		Attr_Public		*/
	    0,	/* Obj_Namelist_Obj		Attr_Target		*/
	    0,	/* Obj_Namelist_Obj		Attr_Equivalence	*/
	    0,	/* Obj_Namelist_Obj		Attr_Save		*/

	  552,	/* Obj_Namelist_Obj		Attr_Pointer		*/
	  552,	/* Obj_Namelist_Obj		Attr_External		*/
	  552,	/* Obj_Namelist_Obj		Attr_Intrinsic		*/
		/* 5.4							*/

	    0,	/* Obj_Namelist_Obj		Attr_Data_Init		*/
	    0,	/* Obj_Namelist_Obj		Attr_Type		*/

	    0,	/* Obj_Namelist_Obj		Attr_Co_Array		*/
	    0,	/* Obj_Namelist_Obj		Attr_Automatic		*/
	    0	/* Obj_Namelist_Obj		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Namelist_Obj	 	Attr_Bind		*/
	,   0	/* Obj_Namelist_Obj	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/************************ Obj_Module_Proc ***********************/
	/* 12.3.2.1 states that a procedure must not have more than	*/
	/* one explicit interface in a scoping unit.	12.3.1 states	*/
	/* that the interface of a module procedure is always an	*/
	/* explicit interface in a scoping unit.	12.3 defines an	*/
	/* interface as the characteristics of the procedure, its	*/
	/* dummy arguments and its function result (if a function).	*/
	/* Since the actual definition of the module procedure is the	*/
	/* explicit interface, it's characteristics cannot be defined	*/
	/* outside of the module procedure definition.			*/
	/* Also 5.1 states that an entity must not be given any		*/
	/* attribute more than once in a scoping unit.			*/
	/****************************************************************/

	{ 552,	/* Obj_Module_Proc		Attr_Assumed_Type_Ch	*/
	  552,	/* Obj_Module_Proc		Attr_Dimension		*/
	  552,	/* Obj_Module_Proc		Attr_Explicit_Shp_Arr	*/
	  552,	/* Obj_Module_Proc		Attr_Assumed_Size_Arr	*/
	  552,	/* Obj_Module_Proc		Attr_Deferred_Shp_Arr	*/
	  552,	/* Obj_Module_Proc		Attr_Assumed_Shp_Arr	*/
	  552,	/* Obj_Module_Proc		Attr_Allocatable	*/
	  552,	/* Obj_Module_Proc		Attr_Parameter		*/
	  552,	/* Obj_Module_Proc		Attr_Intent		*/
	  552,	/* Obj_Module_Proc		Attr_Optional		*/

	    0,	/* Obj_Module_Proc		Attr_Private		*/
	    0,	/* Obj_Module_Proc		Attr_Public		*/
		/* Access is not a characteristic. (12.3) It can be	*/
		/* specified for procedures (5.2.3 constraint).		*/

	  552,	/* Obj_Module_Proc		Attr_Target		*/
	  552,	/* Obj_Module_Proc		Attr_Equivalence	*/
	  552,	/* Obj_Module_Proc		Attr_Save		*/
	  552,	/* Obj_Module_Proc		Attr_Pointer		*/
	  552,	/* Obj_Module_Proc		Attr_External		*/
	  552,	/* Obj_Module_Proc		Attr_Intrinsic		*/
	  552,	/* Obj_Module_Proc		Attr_Data_Init		*/
	  552,	/* Obj_Module_Proc		Attr_Type		*/
	  552,	/* Obj_Module_Proc		Attr_Co_Array		*/
	    0,	/* Obj_Module_Proc		Attr_Automatic		*/
	  552	/* Obj_Module_Proc		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Pointer		 	Attr_Bind		*/
	, 552	/* Obj_Pointer		 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	
	/************************ Obj_Derived_Type **********************/
	/* By 14.1.2, a derived type cannot be a named variable, a	*/
	/* named constant, a construct name, a statement function, an	*/
	/* internal procedure, a dummy procedure, an intrinsic proc,	*/
	/* a generic identifier or a namelist group name.		*/
	/****************************************************************/

	{ 552,	/* Obj_Derived_Type		Attr_Assumed_Type_Ch	*/
	  552,	/* Obj_Derived_Type		Attr_Dimension		*/
	  552,	/* Obj_Derived_Type		Attr_Explicit_Shp_Arr	*/
	  552,	/* Obj_Derived_Type		Attr_Assumed_Size_Arr	*/
	  552,	/* Obj_Derived_Type		Attr_Deferred_Shp_Arr	*/
	  552,	/* Obj_Derived_Type		Attr_Assumed_Shp_Arr	*/
	  552,	/* Obj_Derived_Type		Attr_Allocatable	*/
	  552,	/* Obj_Derived_Type		Attr_Parameter		*/
	  552,	/* Obj_Derived_Type		Attr_Intent		*/
	  552,	/* Obj_Derived_Type		Attr_Optional		*/

	    0,	/* Obj_Derived_Type		Attr_Private		*/
	    0,	/* Obj_Derived_Type		Attr_Public		*/
		/* (5.2.3)						*/

	  552,	/* Obj_Derived_Type		Attr_Target		*/
	  552,	/* Obj_Derived_Type		Attr_Equivalence	*/
	  552,	/* Obj_Derived_Type		Attr_Save		*/
	  552,	/* Obj_Derived_Type		Attr_Pointer		*/
	  552,	/* Obj_Derived_Type		Attr_External		*/
	  552,	/* Obj_Derived_Type		Attr_Intrinsic		*/
	  552,	/* Obj_Derived_Type		Attr_Data_Init		*/
	  552,	/* Obj_Derived_Type		Attr_Type		*/
	  552,	/* Obj_Derived_Type		Attr_Co_Array		*/
	  552,	/* Obj_Derived_Type		Attr_Automatic		*/
	  552	/* Obj_Derived_Type		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 552	/* Obj_Derived_Type	 	Attr_Bind		*/
	, 552	/* Obj_Derived_Type	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Generic_Interface	*****************/
	/* By 14.1.2, a generic interface cannot be a named variable, a */
	/* named constant, a construct name, a statement function, an	*/
	/* internal procedure, a dummy procedure, an intrinsic proc,	*/
	/* a derived type or a namelist group name.			*/
	/****************************************************************/

	{ 552,	/* Obj_Generic_Interface	Attr_Assumed_Type_Ch	*/
	  552,	/* Obj_Generic_Interface	Attr_Dimension		*/
	  552,	/* Obj_Generic_Interface	Attr_Explicit_Shp_Arr	*/
	  552,	/* Obj_Generic_Interface	Attr_Assumed_Size_Arr	*/
	  552,	/* Obj_Generic_Interface	Attr_Deferred_Shp_Arr	*/
	  552,	/* Obj_Generic_Interface	Attr_Assumed_Shp_Arr	*/
	  552,	/* Obj_Generic_Interface	Attr_Allocatable	*/
	  552,	/* Obj_Generic_Interface	Attr_Parameter		*/
	  552,	/* Obj_Generic_Interface	Attr_Intent		*/
	  552,	/* Obj_Generic_Interface	Attr_Optional		*/

	    0,	/* Obj_Generic_Interface	Attr_Private		*/
	    0,	/* Obj_Generic_Interface	Attr_Public		*/
		/* (5.2.3)						*/

	  552,	/* Obj_Generic_Interface	Attr_Target		*/
	  552,	/* Obj_Generic_Interface	Attr_Equivalence	*/
	  552,	/* Obj_Generic_Interface	Attr_Save		*/
	  552,	/* Obj_Generic_Interface	Attr_Pointer		*/
	  552,	/* Obj_Generic_Interface	Attr_External		*/
	    0,	/* Obj_Generic_Interface	Attr_Intrinsic		*/
	  552,	/* Obj_Generic_Interface	Attr_Data_Init		*/
	    0,	/* Obj_Generic_Interface	Attr_Type		*/
	  552,	/* Obj_Generic_Interface	Attr_Co_Array		*/
	  552,	/* Obj_Generic_Interface	Attr_Automatic		*/
	  552	/* Obj_Generic_Interface	Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 552	/* Obj_Generic_Interface 	Attr_Bind		*/
	, 552	/* Obj_Generic_Interface 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Namelist_Grp **********************/
	/* By 14.1.2, a namelist group cannot be a named variable, a	*/
	/* named constant, a construct name, a statement function, an	*/
	/* internal procedure, a dummy procedure, an intrinsic proc,	*/
	/* a derived type or a generic identifier.			*/
	/****************************************************************/

	{ 552,	/* Obj_Namelist_Grp		Attr_Assumed_Type_Ch	*/
	  552,	/* Obj_Namelist_Grp		Attr_Dimension		*/
	  552,	/* Obj_Namelist_Grp		Attr_Explicit_Shp_Arr	*/
	  552,	/* Obj_Namelist_Grp		Attr_Assumed_Size_Arr	*/
	  552,	/* Obj_Namelist_Grp		Attr_Deferred_Shp_Arr	*/
	  552,	/* Obj_Namelist_Grp		Attr_Assumed_Shp_Arr	*/
	  552,	/* Obj_Namelist_Grp		Attr_Allocatable	*/
	  552,	/* Obj_Namelist_Grp		Attr_Parameter		*/
	  552,	/* Obj_Namelist_Grp		Attr_Intent		*/
	  552,	/* Obj_Namelist_Grp		Attr_Optional		*/

	    0,	/* Obj_Namelist_Grp		Attr_Private		*/
	    0,	/* Obj_Namelist_Grp		Attr_Public		*/
		/* (5.2.3)						*/

	  552,	/* Obj_Namelist_Grp		Attr_Target		*/
	  552,	/* Obj_Namelist_Grp		Attr_Equivalence	*/
	  552,	/* Obj_Namelist_Grp		Attr_Save		*/
	  552,	/* Obj_Namelist_Grp		Attr_Pointer		*/
	  552,	/* Obj_Namelist_Grp		Attr_External		*/
	  552,	/* Obj_Namelist_Grp		Attr_Intrinsic		*/
	  552,	/* Obj_Namelist_Grp		Attr_Data_Init		*/
	  552,	/* Obj_Namelist_Grp		Attr_Type		*/
	  552,	/* Obj_Namelist_Grp		Attr_Co_Array		*/
	  552,	/* Obj_Namelist_Grp		Attr_Automatic		*/
	  552	/* Obj_Namelist_Grp		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 552	/* Obj_Namelist_Grp	  	Attr_Bind		*/
	, 552	/* Obj_Namelist_Grp	  	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Stmt_Func	*************************/
	/* By 14.1.2, a statement function cannot be a named variable,  */
	/* a named constant, a construct name, a generic identifier, an */
	/* internal procedure, a dummy procedure, an intrinsic proc,	*/
	/* a derived type or a namelist group.	Also see 12.5.4.	*/
	/****************************************************************/

	{ 552,	/* Obj_Stmt_Func		Attr_Assumed_Type_Ch	*/
	  552,	/* Obj_Stmt_Func		Attr_Dimension		*/
	  552,	/* Obj_Stmt_Func		Attr_Explicit_Shp_Arr	*/
	  552,	/* Obj_Stmt_Func		Attr_Assumed_Size_Arr	*/
	  552,	/* Obj_Stmt_Func		Attr_Deferred_Shp_Arr	*/
	  552,	/* Obj_Stmt_Func		Attr_Assumed_Shp_Arr	*/
	  552,	/* Obj_Stmt_Func		Attr_Allocatable	*/
	  552,	/* Obj_Stmt_Func		Attr_Parameter		*/
	  552,	/* Obj_Stmt_Func		Attr_Intent		*/
	  552,	/* Obj_Stmt_Func		Attr_Optional		*/

	  552,	/* Obj_Stmt_Func		Attr_Private		*/
	  552,	/* Obj_Stmt_Func		Attr_Public		*/
		/* 5.2.3 constraint					*/

	  552,	/* Obj_Stmt_Func		Attr_Target		*/
	  552,	/* Obj_Stmt_Func		Attr_Equivalence	*/
	  552,	/* Obj_Stmt_Func		Attr_Save		*/
	  552,	/* Obj_Stmt_Func		Attr_Pointer		*/
	  552,	/* Obj_Stmt_Func		Attr_External		*/
	  552,	/* Obj_Stmt_Func		Attr_Intrinsic		*/
	  552,	/* Obj_Stmt_Func		Attr_Data_Init		*/

	    0,	/* Obj_Stmt_Func		Attr_Type		*/

	  552,	/* Obj_Stmt_Func		Attr_Co_Array		*/
	  552,	/* Obj_Stmt_Func		Attr_Automatic		*/
	  552	/* Obj_Stmt_Func		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 552	/* Obj_Stmt_Func	 	Attr_Bind		*/
	, 552	/* Obj_Stmt_Func	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Construct	*************************/
	/* By 14.1.2, a construct cannot be a named variable, a named	*/
	/* constant, a statement function, a generic identifier, an	*/
	/* internal procedure, a dummy procedure, an intrinsic proc,	*/
	/* a derived type or a namelist group.			 	*/
	/****************************************************************/

	{ 552,	/* Obj_Construct		Attr_Assumed_Type_Ch	*/
	  552,	/* Obj_Construct		Attr_Dimension		*/
	  552,	/* Obj_Construct		Attr_Explicit_Shp_Arr	*/
	  552,	/* Obj_Construct		Attr_Assumed_Size_Arr	*/
	  552,	/* Obj_Construct		Attr_Deferred_Shp_Arr	*/
	  552,	/* Obj_Construct		Attr_Assumed_Shp_Arr	*/
	  552,	/* Obj_Construct		Attr_Allocatable	*/
	  552,	/* Obj_Construct		Attr_Parameter		*/
	  552,	/* Obj_Construct		Attr_Intent		*/
	  552,	/* Obj_Construct		Attr_Optional		*/

	  552,	/* Obj_Construct		Attr_Private		*/
	  552,	/* Obj_Construct		Attr_Public		*/
		/* 5.2.3 constraint					*/

	  552,	/* Obj_Construct		Attr_Target		*/
	  552,	/* Obj_Construct		Attr_Equivalence	*/
	  552,	/* Obj_Construct		Attr_Save		*/
	  552,	/* Obj_Construct		Attr_Pointer		*/
	  552,	/* Obj_Construct		Attr_External		*/
	  552,	/* Obj_Construct		Attr_Intrinsic		*/
	  552,	/* Obj_Construct		Attr_Data_Init		*/
	  552,	/* Obj_Construct		Attr_Type		*/
	  552,	/* Obj_Construct		Attr_Co_Array		*/
	  552,	/* Obj_Construct		Attr_Automatic		*/
	  552	/* Obj_Construct		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 552	/* Obj_Construct	 	Attr_Bind		*/
	, 552	/* Obj_Construct	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Entry_Func ************************/
	/* This is the name on an ENTRY statement in an external	*/
	/* FUNCTION.	This allows anything legal on a result name to	*/
	/* be specified here.	Then if a different result name is	*/
	/* specified errors are issued at that time saying to use the	*/
	/* result name, not the function name to declare type, shape,	*/
	/* target or pointer.						*/
	/****************************************************************/

	{   0,	/* Obj_Entry_Func		Attr_Assumed_Type_Ch	*/
		/* (5.1.1.5)	The external subprogram being compiled	*/
		/* can be an assumed type character.			*/

	    0,	/* Obj_Entry_Func		Attr_Dimension		*/
	    0,	/* Obj_Entry_Func		Attr_Explicit_Shp_Arr	*/

	  552,	/* Obj_Entry_Func		Attr_Assumed_Size_Arr	*/
		/* This is a darg.  Dargs can't be dummy procedures	*/
		/* or function results.	(14.1.2)			*/

	    0,	/* Obj_Entry_Func		Attr_Deferred_Shp_Arr	*/

	  552,	/* Obj_Entry_Func		Attr_Assumed_Shp_Arr	*/
		/* This is a darg.  Dargs can't be dummy procedures	*/
		/* or function results.	(14.1.2)			*/

	  552,	/* Obj_Entry_Func		Attr_Allocatable	*/
		/* 5.2.6						*/

	  552,	/* Obj_Entry_Func		Attr_Parameter		*/
		/* Constants can't be dargs or dummy procs. (14.1.2)	*/

	  552,	/* Obj_Entry_Func		Attr_Intent		*/
	  552,	/* Obj_Entry_Func		Attr_Optional		*/
		/* This is a darg.	Dargs can't be dummy procedures	*/
		/* or function results.	(14.1.2)			*/

	    0,	/* Obj_Entry_Func		Attr_Private		*/
	    0,	/* Obj_Entry_Func		Attr_Public		*/

	    0,	/* Obj_Entry_Func		Attr_Target		*/
		/* Target may be specified for all function results,	*/
		/* but the function result may be used as a target	*/
		/* only if it is the result of the current function	*/
		/* (7.5.2) pointer assignment			 	*/

	  552,	/* Obj_Entry_Func		Attr_Equivalence	*/
		/* 5.5.1						*/

	  552,	/* Obj_Entry_Func		Attr_Save		*/
		/* 5.2.4						*/

	    0,	/* Obj_Entry_Func		Attr_Pointer		*/

	  552,	/* Obj_Entry_Func		Attr_External		*/
	  552,	/* Obj_Entry_Func		Attr_Intrinsic		*/
		/* 12.5.2.5 constraint					*/

	  552,	/* Obj_Entry_Func		Attr_Data_Init		*/
		/* 5.2.9						*/

	    0,	/* Obj_Entry_Func		Attr_Type		*/

	  552,	/* Obj_Entry_Func		Attr_Co_Array		*/
	    0,  /* Obj_Entry_Func		Attr_Automatic		*/
	  552   /* Obj_Entry_Func		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Entry_Func	 	Attr_Bind		*/
	, 552	/* Obj_Entry_Func	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Entry_Subr ************************/
	/* This is the name on an ENTRY statement in an external	*/
	/* SUBROUTINE.	One just knows that a SUBROUTINE can't have	*/
	/* a type, or a shape, or much of anything else.	(2.2.3)	*/
	/****************************************************************/

	{ 552,	/* Obj_Entry_Subr		Attr_Assumed_Type_Ch	*/
	  552,	/* Obj_Entry_Subr		Attr_Dimension		*/
	  552,	/* Obj_Entry_Subr		Attr_Explicit_Shp_Arr	*/
	  552,	/* Obj_Entry_Subr		Attr_Assumed_Size_Arr	*/
	  552,	/* Obj_Entry_Subr		Attr_Deferred_Shp_Arr	*/
	  552,	/* Obj_Entry_Subr		Attr_Assumed_Shp_Arr	*/
	  552,	/* Obj_Entry_Subr		Attr_Allocatable	*/
	  552,	/* Obj_Entry_Subr		Attr_Parameter		*/
	  552,	/* Obj_Entry_Subr		Attr_Intent		*/
	  552,	/* Obj_Entry_Subr		Attr_Optional		*/

	    0,	/* Obj_Entry_Subr		Attr_Private		*/
	    0,	/* Obj_Entry_Subr		Attr_Public		*/

	  552,	/* Obj_Entry_Subr		Attr_Target		*/
	  552,	/* Obj_Entry_Subr		Attr_Equivalence	*/
	  552,	/* Obj_Entry_Subr		Attr_Save		*/
	  552,	/* Obj_Entry_Subr		Attr_Pointer		*/
	  552,	/* Obj_Entry_Subr		Attr_External		*/
	  552,	/* Obj_Entry_Subr		Attr_Intrinsic		*/
	  552,	/* Obj_Entry_Subr		Attr_Data_Init		*/
	  552,	/* Obj_Entry_Subr		Attr_Type		*/
	  552,	/* Obj_Entry_Subr		Attr_Co_Array		*/
	  552,	/* Obj_Entry_Subr		Attr_Automatic		*/
	  552	/* Obj_Entry_Subr		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Entry_Subr	 	Attr_Bind		*/
	, 552	/* Obj_Entry_Subr	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Intern_Func ***********************/
	/* This is the name on the FUNCTION statement for an internal	*/
	/* FUNCTION.	12.3.2.1 states that a procedure must not have	*/
	/* more than one explicit interface in a scoping unit.	12.3.1  */
	/* states that the interface of an internal procedure is always */
	/* an explicit interface in a scoping unit.	12.3 defines an	*/
	/* interface as the characteristics of the procedure, its	*/
	/* dummy arguments and its function result (if a function).	*/
	/* Since the actual definition of the internal procedure is the */
	/* explicit interface, it's characteristics cannot be defined	*/
	/* outside of the internal procedure definition.		*/
	/* Also 5.1 states that an entity must not be given any		*/
	/* attribute more than once in a scoping unit.		 	*/
	/****************************************************************/

	{  552,	/* Obj_Intern_Func		Attr_Assumed_Type_Ch	*/
	  552,	/* Obj_Intern_Func		Attr_Dimension		*/
	  552,	/* Obj_Intern_Func		Attr_Explicit_Shp_Arr	*/
	  552,	/* Obj_Intern_Func		Attr_Assumed_Size_Arr	*/
	  552,	/* Obj_Intern_Func		Attr_Deferred_Shp_Arr	*/
	  552,	/* Obj_Intern_Func		Attr_Assumed_Shp_Arr	*/
	  552,	/* Obj_Intern_Func		Attr_Allocatable	*/
	  552,	/* Obj_Intern_Func		Attr_Parameter		*/
	  552,	/* Obj_Intern_Func		Attr_Intent		*/
	  552,	/* Obj_Intern_Func		Attr_Optional		*/

	    0,	/* Obj_Intern_Func		Attr_Private		*/
	    0,	/* Obj_Intern_Func		Attr_Public		*/
		/* Access is not a characteristic.(12.3) It can be	*/
		/* specified for procedures (5.2.3 constraint).		*/

	  552,	/* Obj_Intern_Func		Attr_Target		*/
	  552,	/* Obj_Intern_Func		Attr_Equivalence	*/
	  552,	/* Obj_Intern_Func		Attr_Save		*/
	  552,	/* Obj_Intern_Func		Attr_Pointer		*/
	  552,	/* Obj_Intern_Func		Attr_External		*/
	  552,	/* Obj_Intern_Func		Attr_Intrinsic		*/
	  552,	/* Obj_Intern_Func		Attr_Data_Init		*/
	  552,	/* Obj_Intern_Func		Attr_Type		*/
	  552,	/* Obj_Intern_Func		Attr_Co_Array		*/
	  552,	/* Obj_Intern_Func		Attr_Automatic		*/
	  552	/* Obj_Intern_Func		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 552	/* Obj_Intern_Func	 	Attr_Bind		*/
	, 552	/* Obj_Intern_Func	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Intern_Subr ***********************/
	/* This is the name on a SUBROUTINE statement for an internal	*/
	/* SUBROUTINE.	One just knows that a SUBROUTINE can't have	*/
	/* a type, or a shape, or much of anything else.	(2.2.3)	*/
	/****************************************************************/

	{  552,	/* Obj_Intern_Subr		Attr_Assumed_Type_Ch	*/
	  552,	/* Obj_Intern_Subr		Attr_Dimension		*/
	  552,	/* Obj_Intern_Subr		Attr_Explicit_Shp_Arr	*/
	  552,	/* Obj_Intern_Subr		Attr_Assumed_Size_Arr	*/
	  552,	/* Obj_Intern_Subr		Attr_Deferred_Shp_Arr	*/
	  552,	/* Obj_Intern_Subr		Attr_Assumed_Shp_Arr	*/
	  552,	/* Obj_Intern_Subr		Attr_Allocatable	*/
	  552,	/* Obj_Intern_Subr		Attr_Parameter		*/
	  552,	/* Obj_Intern_Subr		Attr_Intent		*/
	  552,	/* Obj_Intern_Subr		Attr_Optional		*/

	    0,	/* Obj_Intern_Subr		Attr_Private		*/
	    0,	/* Obj_Intern_Subr		Attr_Public		*/
		/* Access is not a characteristic. (12.3) It can be	*/
		/* specified for procedures (5.2.3 constraint).		*/

	  552,	/* Obj_Intern_Subr		Attr_Target		*/
	  552,	/* Obj_Intern_Subr		Attr_Equivalence	*/
	  552,	/* Obj_Intern_Subr		Attr_Save		*/
	  552,	/* Obj_Intern_Subr		Attr_Pointer		*/
	  552,	/* Obj_Intern_Subr		Attr_External		*/
	  552,	/* Obj_Intern_Subr		Attr_Intrinsic		*/
	  552,	/* Obj_Intern_Subr		Attr_Data_Init		*/
	  552,	/* Obj_Intern_Subr		Attr_Type		*/
	  552,	/* Obj_Intern_Subr		Attr_Co_Array		*/
	  552,	/* Obj_Intern_Subr		Attr_Automatic		*/
	  552	/* Obj_Intern_Subr		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 552	/* Obj_Intern_Subr	 	Attr_Bind		*/
	, 552	/* Obj_Intern_Subr	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Module_Func ***********************/
	/* This is the name on the FUNCTION statement for a module	*/
	/* FUNCTION.	12.3.2.1 states that a procedure must not have	*/
	/* more than one explicit interface in a scoping unit.	12.3.1  */
	/* states that the interface of a module procedure is always an */
	/* explicit interface in a scoping unit.	12.3 defines an	*/
	/* interface as the characteristics of the procedure, its	*/
	/* dummy arguments and its function result (if a function).	*/
	/* Since the actual definition of the module procedure is the	*/
	/* explicit interface, it's characteristics cannot be defined	*/
	/* outside of the module procedure definition.		 	*/
	/* Also 5.1 states that an entity must not be given any		*/
	/* attribute more than once in a scoping unit.		 	*/
	/****************************************************************/

	{ 552,	/* Obj_Module_Func		Attr_Assumed_Type_Ch	*/
	  552,	/* Obj_Module_Func		Attr_Dimension		*/
	  552,	/* Obj_Module_Func		Attr_Explicit_Shp_Arr	*/
	  552,	/* Obj_Module_Func		Attr_Assumed_Size_Arr	*/
	  552,	/* Obj_Module_Func		Attr_Deferred_Shp_Arr	*/
	  552,	/* Obj_Module_Func		Attr_Assumed_Shp_Arr	*/
	  552,	/* Obj_Module_Func		Attr_Allocatable	*/
	  552,	/* Obj_Module_Func		Attr_Parameter		*/
	  552,	/* Obj_Module_Func		Attr_Intent		*/
	  552,	/* Obj_Module_Func		Attr_Optional		*/

	    0,	/* Obj_Module_Func		Attr_Private		*/
	    0,	/* Obj_Module_Func		Attr_Public		*/
		/* Access is not a characteristic. (12.3) It can be	*/
		/* specified for procedures (5.2.3 constraint).		*/

	  552,	/* Obj_Module_Func		Attr_Target		*/
	  552,	/* Obj_Module_Func		Attr_Equivalence	*/
	  552,	/* Obj_Module_Func		Attr_Save		*/
	  552,	/* Obj_Module_Func		Attr_Pointer		*/
	  552,	/* Obj_Module_Func		Attr_External		*/
	  552,	/* Obj_Module_Func		Attr_Intrinsic		*/
	  552,	/* Obj_Module_Func		Attr_Data_Init		*/
	  552,	/* Obj_Module_Func		Attr_Type		*/
	  552,	/* Obj_Module_Func		Attr_Co_Array		*/
	    0,	/* Obj_Module_Func		Attr_Automatic		*/
	  552 	/* Obj_Module_Func		Attr_Automatic		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Module_Func		Attr_Bind		*/
	, 552	/* Obj_Module_Func		Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Module_Subr ***********************/
	/* This is the name on a SUBROUTINE statement for a module	*/
	/* SUBROUTINE.	One just knows that a SUBROUTINE can't have	*/
	/* a type, or a shape, or much of anything else.	(2.2.3)	*/
	/****************************************************************/

	{ 552,	/* Obj_Module_Subr		Attr_Assumed_Type_Ch	*/
	  552,	/* Obj_Module_Subr		Attr_Dimension		*/
	  552,	/* Obj_Module_Subr		Attr_Explicit_Shp_Arr	*/
	  552,	/* Obj_Module_Subr		Attr_Assumed_Size_Arr	*/
	  552,	/* Obj_Module_Subr		Attr_Deferred_Shp_Arr	*/
	  552,	/* Obj_Module_Subr		Attr_Assumed_Shp_Arr	*/
	  552,	/* Obj_Module_Subr		Attr_Allocatable	*/
	  552,	/* Obj_Module_Subr		Attr_Parameter		*/
	  552,	/* Obj_Module_Subr		Attr_Intent		*/
	  552,	/* Obj_Module_Subr		Attr_Optional		*/

	    0,	/* Obj_Module_Subr		Attr_Private		*/
	    0,	/* Obj_Module_Subr		Attr_Public		*/
		/* Access is not a characteristic. (12.3) It can be	*/
		/* specified for procedures (5.2.3 constraint).		*/

	  552,	/* Obj_Module_Subr		Attr_Target		*/
	  552,	/* Obj_Module_Subr		Attr_Equivalence	*/
	  552,	/* Obj_Module_Subr		Attr_Save		*/
	  552,	/* Obj_Module_Subr		Attr_Pointer		*/
	  552,	/* Obj_Module_Subr		Attr_External		*/
	  552,	/* Obj_Module_Subr		Attr_Intrinsic		*/
	  552,	/* Obj_Module_Subr		Attr_Data_Init		*/
	  552,	/* Obj_Module_Subr		Attr_Type		*/
	  552,	/* Obj_Module_Subr		Attr_Co_Array		*/
	  552,	/* Obj_Module_Subr		Attr_Automatic		*/
	  552	/* Obj_Module_Subr		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Module_Subr	 	Attr_Bind		*/
	, 552	/* Obj_Module_Subr	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},



	/************************ Obj_Sf_Darg	*************************/
	/* This is the dummy argument used in the statement function	*/
	/* definition.	It is defined in 5.1.1.5 and 12.5.4.		*/
	/****************************************************************/

	{ 552,	/* Obj_Sf_Darg			Attr_Assumed_Type_Ch	*/
	  552,	/* Obj_Sf_Darg			Attr_Dimension		*/
	  552,	/* Obj_Sf_Darg			Attr_Explicit_Shp_Arr	*/
	  552,	/* Obj_Sf_Darg			Attr_Assumed_Size_Arr	*/
	  552,	/* Obj_Sf_Darg			Attr_Deferred_Shp_Arr	*/
	  552,	/* Obj_Sf_Darg			Attr_Assumed_Shp_Arr	*/
	  552,	/* Obj_Sf_Darg			Attr_Allocatable	*/
	  552,	/* Obj_Sf_Darg			Attr_Parameter		*/
	    0,	/* Obj_Sf_Darg			Attr_Intent		*/
	    0,	/* Obj_Sf_Darg			Attr_Optional		*/
	    0,	/* Obj_Sf_Darg			Attr_Private		*/
	    0,	/* Obj_Sf_Darg			Attr_Public		*/
	    0,	/* Obj_Sf_Darg			Attr_Target		*/
	    0,	/* Obj_Sf_Darg			Attr_Equivalence	*/
	    0,	/* Obj_Sf_Darg			Attr_Save		*/
	    0,	/* Obj_Sf_Darg			Attr_Pointer		*/
	  552,	/* Obj_Sf_Darg			Attr_External		*/
	  552,	/* Obj_Sf_Darg			Attr_Intrinsic		*/
	    0,	/* Obj_Sf_Darg			Attr_Data_Init		*/
	    0,	/* Obj_Sf_Darg			Attr_Type		*/
	  552,	/* Obj_Sf_Darg			Attr_Co_Array		*/
	    0,	/* Obj_Sf_Darg			Attr_Automatic		*/
	  552	/* Obj_Sf_Darg			Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 552	/* Obj_Sf_Darg		 	Attr_Bind		*/
	, 552	/* Obj_Sf_Darg		 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Sf_Actual_Arg *********************/
	/* This is the actual argument used when a statement function	*/
	/* is referenced.	It is defined in 5.1.1.5 and 12.5.4.	*/
	/****************************************************************/

	{   0,	/* Obj_Sf_Actual_Arg	 	Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Dimension		*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Explicit_Shp_Arr	*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Deferred_Shp_Arr	*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Assumed_Shp_Arr	*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Allocatable	*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Parameter		*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Intent		*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Optional		*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Private		*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Public		*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Target		*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Equivalence	*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Save		*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Pointer		*/
	  759,	/* Obj_Sf_Actual_Arg	 	Attr_External		*/
	  759,	/* Obj_Sf_Actual_Arg	 	Attr_Intrinsic		*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Data_Init		*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Type		*/
	  759,	/* Obj_Sf_Actual_Arg	 	Attr_Co_Array		*/
	    0,	/* Obj_Sf_Actual_Arg	 	Attr_Automatic		*/
	    0	/* Obj_Sf_Actual_Arg	 	Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Sf_Actual_Arg	 	Attr_Bind		*/
	,   0	/* Obj_Sf_Actual_Arg	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Var_Len_Ch ************************/
	/* This is a variable length character (automatic or		*/
	/* adjustable).	This is not called until the length has been	*/
	/* resolved at the end of pass1.  If the length resolves to a	*/
	/* constant, fnd_semantic_err is not called.	If it resolves	*/
	/* to a non-constant, fnd_semantic_err is called with this.	*/
	/* (5.1 discussion)  The specification-expr of a type-param-	*/
	/* value may be a nonconstant expression provided the		*/
	/* specification expression is in an interface body or in the	*/
	/* specification part of a subprogram.	If the data object	*/
	/* being declared depends on the value of such a nonconstant	*/
	/* expression and is not a dummy argument, such an object is	*/
	/* called an automatic data object.  An automatic data object	*/
	/* must not appear in a SAVE or DATA statement nor be declared 	*/
	/* with a SAVE attribute nor be initially defined by an =	*/
	/* initialization-expr.						*/
	/****************************************************************/

	{ 576,	/* Obj_Var_Len_Ch		Attr_Assumed_Type_Ch	*/
		/* Assumed type char cannot be an implicit type.	*/
		/* (5.1.1.5).	This would be retyping. (5.1)		*/
		/* So this means that ATD_TYPED is set.			*/

	    0,	/* Obj_Var_Len_Ch		Attr_Dimension		*/
	    0,	/* Obj_Var_Len_Ch		Attr_Explicit_Shp_Arr	*/
	    0,	/* Obj_Var_Len_Ch		Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Var_Len_Ch		Attr_Deferred_Shp_Arr	*/
	    0,	/* Obj_Var_Len_Ch		Attr_Assumed_Shp_Arr	*/

	    0,	/* Obj_Var_Len_Ch		Attr_Allocatable	*/
		/* Interpretation 79 makes this legal.			*/
	  576,	/* Obj_Var_Len_Ch		Attr_Parameter		*/
		/* 5.1 constraint					*/

	    0,	/* Obj_Var_Len_Ch		Attr_Intent		*/
	    0,	/* Obj_Var_Len_Ch		Attr_Optional		*/

	    0,	/* Obj_Var_Len_Ch		Attr_Private		*/
	    0,	/* Obj_Var_Len_Ch		Attr_Public		*/

	    0,	/* Obj_Var_Len_Ch		Attr_Target		*/

	  576,	/* Obj_Var_Len_Ch		Attr_Equivalence	*/
		/* 5.5.1						*/

	  576,	/* Obj_Var_Len_Ch		Attr_Save		*/
		/* 5.2.4						*/

	    0,	/* Obj_Var_Len_Ch		Attr_Pointer		*/
		/* Interpretation 79 makes this legal.			*/

	    0,	/* Obj_Var_Len_Ch		Attr_External		*/
		/* We issue a better error message in line.	It is	*/
		/* Illegal to have EXTERNAL and var len character.	*/

	    0,	/* Obj_Var_Len_Ch		Attr_Intrinsic		*/
		/* Legal to declare a type for a function result. Can	*/
		/* also declare a type for a specific or generic	*/
		/* intrinsic function. (5.1 discussion)			*/

	  576,	/* Obj_Var_Len_Ch		Attr_Data_Init		*/
		/* 5.1 constraint					*/

	    0,	/* Obj_Var_Len_Ch		Attr_Type		*/
		/* This is okay, because Obj_Type has been checked	*/
		/* when the type was first assigned.			*/

	    0,	/* Obj_Var_Len_Ch		Attr_Co_Array		*/
	  576,	/* Obj_Var_Len_Ch		Attr_Automatic		*/
	    0	/* Obj_Var_Len_Ch		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 576	/* Obj_Var_Len_Ch	 	Attr_Bind		*/
	, 576	/* Obj_Var_Len_Ch	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Var_Len_Arr ***********************/
	/* This is an explicit-shape array.  It can be adjustable or	*/
	/* automatic.	It is not known that this is a variable length	*/
	/* array until the end of pass1.  These originally look like	*/
	/* an explicit shape array with unknown size.			*/
	/* This must be a dummy argument, a function result, or an	*/
	/* automatic object.						*/
	/****************************************************************/

	{   0,	/* Obj_Var_Len_Arr		Attr_Assumed_Type_Ch	*/
		/* They can both be dargs. (5.1.1.5&	5.1.2.4.1)	*/

	    0,	/* Obj_Var_Len_Arr		Attr_Dimension		*/
	    0,	/* Obj_Var_Len_Arr		Attr_Explicit_Shp_Arr	*/
	  582,	/* Obj_Var_Len_Arr		Attr_Assumed_Size_Arr	*/
	  582,	/* Obj_Var_Len_Arr		Attr_Deferred_Shp_Arr	*/
	  582,	/* Obj_Var_Len_Arr		Attr_Assumed_Shp_Arr	*/
		/* These are all illegal, because that would be	*/
		/* defining an object twice as an array.	(5.1)	*/

	582,	/* Obj_Var_Len_Arr		Attr_Allocatable	*/
		/* Allocatable must be	deferred shape.	(5.1)		*/

	  582,	/* Obj_Var_Len_Arr		Attr_Parameter		*/
		/* Shape must be specified before declaring something	*/
		/* as a constant. (5.2.10)				*/

	    0,	/* Obj_Var_Len_Arr		Attr_Intent		*/
	    0,	/* Obj_Var_Len_Arr		Attr_Optional		*/

	    0,	/* Obj_Var_Len_Arr		Attr_Private		*/
	    0,	/* Obj_Var_Len_Arr		Attr_Public		*/
		/* This will eventually error, because variable length	*/
		/* arrays are not allowed in modules. (5.1.2.4.1)	*/

	    0,	/* Obj_Var_Len_Arr		Attr_Target		*/

	  582,	/* Obj_Var_Len_Arr		Attr_Equivalence	*/
		/* 5.5.1 constraint says equivalence may not be a	*/
		/* darg, an automatic object, or a function result.	*/

	  582,	/* Obj_Var_Len_Arr		Attr_Save		*/
		/* 5.2.4						*/

	  582,	/* Obj_Var_Len_Arr		Attr_Pointer		*/
		/* Pointer must be deferred shape (5.2.7)		*/

	  582,	/* Obj_Var_Len_Arr		Attr_External		*/
	  582,	/* Obj_Var_Len_Arr		Attr_Intrinsic		*/
		/* See [Obj_Expl_Shp_Arr] [Attr_External]  comments.	*/

	  582,	/* Obj_Var_Len_Arr		Attr_Data_Init		*/
		/* A constraint in 5.2.9 says a data initialized	*/
		/* object may not be a darg, an automatic object or a   */
		/* function result.					*/

	    0,	/* Obj_Var_Len_Arr		Attr_Type		*/

	    0,	/* Obj_Var_Len_Arr		Attr_Co_Array		*/
	  582,	/* Obj_Var_Len_Arr		Attr_Automatic		*/
	    0 	/* Obj_Var_Len_Arr		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 582	/* Obj_Var_Len_Arr	 	Attr_Bind		*/
	, 582	/* Obj_Var_Len_Arr	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},

	/************************ Obj_Sym_Constant_Arr ******************/
	/* A symbolic constant array is an array where at least one of	*/
	/* the bounds contains a symbolic constant expression.	 	*/
	/* At end pass1, array bounds are resolved.			*/
	/* If they are constant, semantic checking is done, if they	*/
	/* have symbolic constant bounds, fnd_semantic_err is called	*/
	/* again for the object, using Obj_Sym_Constant_Arr.  There is	*/
	/* nothing that can be a symbolic constant array that cannot	*/
	/* be an array with constant bounds.  This is a Cray extension.	*/
	/****************************************************************/

	{   0,	/* Obj_Sym_Constant_Arr		Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Sym_Constant_Arr		Attr_Dimension		*/
	    0,	/* Obj_Sym_Constant_Arr		Attr_Explicit_Shp_Arr	*/
	    0,	/* Obj_Sym_Constant_Arr		Attr_Assumed_Size_Arr	*/
	 1223,	/* Obj_Sym_Constant_Arr		Attr_Deferred_Shp_Arr	*/
	    0,	/* Obj_Sym_Constant_Arr		Attr_Assumed_Shp_Arr	*/
	 1223,	/* Obj_Sym_Constant_Arr		Attr_Allocatable	*/
	 1223,	/* Obj_Sym_Constant_Arr		Attr_Parameter		*/
	    0,	/* Obj_Sym_Constant_Arr		Attr_Intent		*/
	    0,	/* Obj_Sym_Constant_Arr		Attr_Optional		*/
	    0,	/* Obj_Sym_Constant_Arr		Attr_Private		*/
	    0,	/* Obj_Sym_Constant_Arr		Attr_Public		*/
	    0,	/* Obj_Sym_Constant_Arr		Attr_Target		*/
	 1223,	/* Obj_Sym_Constant_Arr		Attr_Equivalence	*/
	    0,	/* Obj_Sym_Constant_Arr		Attr_Save		*/
	 1223,	/* Obj_Sym_Constant_Arr		Attr_Pointer		*/
	 1223,	/* Obj_Sym_Constant_Arr		Attr_External		*/
	 1223,	/* Obj_Sym_Constant_Arr		Attr_Intrinsic		*/
	 1223,	/* Obj_Sym_Constant_Arr		Attr_Data_Init		*/
	    0,	/* Obj_Sym_Constant_Arr		Attr_Type		*/
	 1223,	/* Obj_Sym_Constant_Arr		Attr_Co_Array		*/
	    0,	/* Obj_Sym_Constant_Arr		Attr_Automatic		*/
	 1223	/* Obj_Sym_Constant_Arr		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,1223	/* Obj_Sym_Constant_Arr	 	Attr_Bind		*//*?*/
	,1223	/* Obj_Sym_Constant_Arr	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Interface_Func ********************/
	/* This is the name of the FUNCTION on an interface body.	*/
	/* 12.3.2.1 states that a procedure must not have more than	*/
	/* one explicit interface in a scoping unit.	12.3.1 states	*/
	/* that specifying an external or dummy procedure in an		*/
	/* interface block, causes it to have an explicit interface.	*/
	/* 12.3 defines an interface as the characteristics of the	*/
	/* procedure, its dummy arguments and its function result (if	*/
	/* it's a function).  Since the actual definition of the module */
	/* procedure is the explicit interface, it's characteristics	*/
	/* cannot be defined outside of the module procedure		*/
	/* definition.	Also 5.1 states that an entity must not be	*/
	/* given any attribute more than once in a scoping unit.	*/
	/****************************************************************/

	{ 608,	/* Obj_Interface_Func		Attr_Assumed_Type_Ch	*/
	  608,	/* Obj_Interface_Func		Attr_Dimension		*/
	  608,	/* Obj_Interface_Func		Attr_Explicit_Shp_Arr	*/
	  608,	/* Obj_Interface_Func		Attr_Assumed_Size_Arr	*/
	  608,	/* Obj_Interface_Func		Attr_Deferred_Shp_Arr	*/
	  608,	/* Obj_Interface_Func		Attr_Assumed_Shp_Arr	*/
	  608,	/* Obj_Interface_Func		Attr_Allocatable	*/
	  608,	/* Obj_Interface_Func		Attr_Parameter		*/
	  608,	/* Obj_Interface_Func		Attr_Intent		*/

	    0,	/* Obj_Interface_Func		Attr_Optional		*/
		/* This is a characteristic of the function as a darg	*/
		/* in the host scoping unit, and not a characteristic	*/
		/* of the procedure itself.	(12.2)			*/

	    0,	/* Obj_Interface_Func		Attr_Private		*/
	    0,	/* Obj_Interface_Func		Attr_Public		*/
		/* Access is not a characteristic.  (12.3) It can be	*/
		/* specified for procedures (5.2.3 constraint).		*/

	  608,	/* Obj_Interface_Func		Attr_Target		*/
	  608,	/* Obj_Interface_Func		Attr_Equivalence	*/
	  608,	/* Obj_Interface_Func		Attr_Save		*/
	  608,	/* Obj_Interface_Func		Attr_Pointer		*/
	  608,	/* Obj_Interface_Func		Attr_External		*/
	  608,	/* Obj_Interface_Func		Attr_Intrinsic		*/
	  608,	/* Obj_Interface_Func		Attr_Data_Init		*/
	  608,	/* Obj_Interface_Func		Attr_Type		*/
	  608,	/* Obj_Interface_Func		Attr_Co_Array		*/
	    0,	/* Obj_Interface_Func		Attr_Automatic		*/
	  608	/* Obj_Interface_Func		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Interface_Func	 	Attr_Bind		*/
	, 550	/* Obj_Interface_Func	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Interface_Subr ********************/
	/* This is the name of the SUBROUTINE on an interface body.	*/
	/* This may be a dummy procedure	(12.3.2.1) discussion	*/
	/* Subroutines can't be typed, shaped or anything else.		*/
	/****************************************************************/

	{ 608,	/* Obj_Interface_Subr		Attr_Assumed_Type_Ch	*/
	  608,	/* Obj_Interface_Subr		Attr_Dimension		*/
	  608,	/* Obj_Interface_Subr		Attr_Explicit_Shp_Arr	*/
	  608,	/* Obj_Interface_Subr		Attr_Assumed_Size_Arr	*/
	  608,	/* Obj_Interface_Subr		Attr_Deferred_Shp_Arr	*/
	  608,	/* Obj_Interface_Subr		Attr_Assumed_Shp_Arr	*/
	  608,	/* Obj_Interface_Subr		Attr_Allocatable	*/
	  608,	/* Obj_Interface_Subr		Attr_Parameter		*/
	  608,	/* Obj_Interface_Subr		Attr_Intent		*/

	    0,	/* Obj_Interface_Subr		Attr_Optional		*/
		/* 12.3.2.1						*/

	    0,	/* Obj_Interface_Subr		Attr_Private		*/
	    0,	/* Obj_Interface_Subr		Attr_Public		*/
		/* 5.2.3 allows these to be set for procedures.		*/

	  608,	/* Obj_Interface_Subr		Attr_Target		*/
	  608,	/* Obj_Interface_Subr		Attr_Equivalence	*/
	  608,	/* Obj_Interface_Subr		Attr_Save		*/
	  608,	/* Obj_Interface_Subr		Attr_Pointer		*/
	  608,	/* Obj_Interface_Subr		Attr_External		*/
	  608,	/* Obj_Interface_Subr		Attr_Intrinsic		*/
	  608,	/* Obj_Interface_Subr		Attr_Data_Init		*/
	  608,	/* Obj_Interface_Subr		Attr_Type		*/
	  608,	/* Obj_Interface_Subr		Attr_Co_Array		*/
	  608,	/* Obj_Interface_Subr		Attr_Automatic		*/
	  608	/* Obj_Interface_Subr		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Interface_Subr	 	Attr_Bind		*/
	, 608	/* Obj_Interface_Subr	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Use_Extern_Func ********************/
	/* This is an external function in an expression.		*/
	/***************************************************************/

	{   0,	/* Obj_Use_Extern_Func		Attr_Assumed_Type_Ch	*/
		/* 5.1.1.1 says *(*) must be the current function	*/
		/* being compiled.  It says that the caller must	*/
		/* have a known character length.  So if this was	*/
		/* done, this would be a recursive call, and the char	*/
		/* length wouldn't be known.	Extension		*/
		/* Checked in line, because dummy procs are allowed	*/
		/* to be this, but other things are not.		*/

	    0,	/* Obj_Use_Extern_Func		Attr_Dimension		*/
	    0,	/* Obj_Use_Extern_Func		Attr_Explicit_Shp_Arr	*/

	  628,	/* Obj_Use_Extern_Func		Attr_Assumed_Size_Arr	*/
		/* This is a darg.	(5.1.2.4.4)			*/
		
	    0,	/* Obj_Use_Extern_Func		Attr_Deferred_Shp_Arr	*/

	  628,	/* Obj_Use_Extern_Func		Attr_Assumed_Shp_Arr	*/
		/* This is a darg.	(5.1.2.4.2)			*/

	  628,	/* Obj_Use_Extern_Func		Attr_Allocatable	*/
		/* This can't be a function result. (5.2.6) constraint	*/

	  628,	/* Obj_Use_Extern_Func		Attr_Parameter		*/
		/* Constants can't be functions	(14.1.2)		*/

	  628,	/* Obj_Use_Extern_Func		Attr_Intent		*/
		/* Intent can't be specified for a dummy proc. 5.1.2.3	*/

	    0,	/* Obj_Use_Extern_Func		Attr_Optional		*/
	    0,	/* Obj_Use_Extern_Func		Attr_Private		*/
	    0,	/* Obj_Use_Extern_Func		Attr_Public		*/

	  628,	/* Obj_Use_Extern_Func		Attr_Target		*/
		/* ????							*/

	  628,	/* Obj_Use_Extern_Func		Attr_Equivalence	*/
		/* Must be a variable - 5.5.1				 */

	  628,	/* Obj_Use_Extern_Func		Attr_Save		*/
		/* 5.2.4						*/

	    0,	/* Obj_Use_Extern_Func		Attr_Pointer		*/
	    0,	/* Obj_Use_Extern_Func		Attr_External		*/
	    0,	/* Obj_Use_Extern_Func		Attr_Intrinsic		*/

	  628,	/* Obj_Use_Extern_Func		Attr_Data_Init		*/
		/* 5.1 constraint					*/

	    0,	/* Obj_Use_Extern_Func		Attr_Type		*/
	  628,	/* Obj_Use_Extern_Func		Attr_Co_Array		*/
	    0,	/* Obj_Use_Extern_Func		Attr_Automatic		*/
	  628	/* Obj_Use_Extern_Func		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Use_Extern_Func	 	Attr_Bind		*/
	, 628	/* Obj_Use_Extern_Func	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Use_Extern_Subr *******************/
	/* This is an external subroutine in an expression.		*/
	/* Subroutines can't be typed, ranked or anything else.		*/
	/****************************************************************/

	{ 633,	/* Obj_Use_Extern_Subr		Attr_Assumed_Type_Ch	*/
	  633,	/* Obj_Use_Extern_Subr		Attr_Dimension		*/
	  633,	/* Obj_Use_Extern_Subr		Attr_Explicit_Shp_Arr	*/
	  633,	/* Obj_Use_Extern_Subr		Attr_Assumed_Size_Arr	*/
	  633,	/* Obj_Use_Extern_Subr		Attr_Deferred_Shp_Arr	*/
	  633,	/* Obj_Use_Extern_Subr		Attr_Assumed_Shp_Arr	*/
	  633,	/* Obj_Use_Extern_Subr		Attr_Allocatable	*/
	  633,	/* Obj_Use_Extern_Subr		Attr_Parameter		*/
	  633,	/* Obj_Use_Extern_Subr		Attr_Intent		*/

	    0,	/* Obj_Use_Extern_Subr		Attr_Optional		*/
		/* This can be a dummy proc.	They can be optional.	*/

	    0,	/* Obj_Use_Extern_Subr		Attr_Private		*/
	    0,	/* Obj_Use_Extern_Subr		Attr_Public		*/

	  633,	/* Obj_Use_Extern_Subr		Attr_Target		*/
	  633,	/* Obj_Use_Extern_Subr		Attr_Equivalence	*/
	  633,	/* Obj_Use_Extern_Subr		Attr_Save		*/
	  633,	/* Obj_Use_Extern_Subr		Attr_Pointer		*/

	    0,	/* Obj_Use_Extern_Subr		Attr_External		*/

	    0,	/* Obj_Use_Extern_Subr		Attr_Intrinsic		*/
	  633,	/* Obj_Use_Extern_Subr		Attr_Data_Init		*/
	  633,	/* Obj_Use_Extern_Subr		Attr_Type		*/
	  633,	/* Obj_Use_Extern_Subr		Attr_Co_Array		*/
	  633,	/* Obj_Use_Extern_Subr		Attr_Automatic		*/
	  633	/* Obj_Use_Extern_Subr		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Use_Extern_Subr	 	Attr_Bind		*/
	, 633	/* Obj_Use_Extern_Subr	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Use_In_Expr ***********************/
	/*	This is something referenced in an expression.		*/
	/****************************************************************/

	{   0,	/* Obj_Use_In_Expr		Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Use_In_Expr		Attr_Dimension		*/
	    0,	/* Obj_Use_In_Expr		Attr_Explicit_Shp_Arr	*/
	    0,	/* Obj_Use_In_Expr		Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Use_In_Expr		Attr_Deferred_Shp_Arr	*/
	    0,	/* Obj_Use_In_Expr		Attr_Assumed_Shp_Arr	*/
	    0,	/* Obj_Use_In_Expr		Attr_Allocatable	*/
	    0,	/* Obj_Use_In_Expr		Attr_Parameter		*/
	    0,	/* Obj_Use_In_Expr		Attr_Intent		*/
	    0,	/* Obj_Use_In_Expr		Attr_Optional		*/
	    0,	/* Obj_Use_In_Expr		Attr_Private		*/
	    0,	/* Obj_Use_In_Expr		Attr_Public		*/
	    0,	/* Obj_Use_In_Expr		Attr_Target		*/
	    0,	/* Obj_Use_In_Expr		Attr_Equivalence	*/
	    0,	/* Obj_Use_In_Expr		Attr_Save		*/
	    0,	/* Obj_Use_In_Expr		Attr_Pointer		*/
	    0,	/* Obj_Use_In_Expr		Attr_External		*/
	    0,	/* Obj_Use_In_Expr		Attr_Intrinsic		*/
	    0,	/* Obj_Use_In_Expr		Attr_Data_Init		*/
	    0,	/* Obj_Use_In_Expr		Attr_Type		*/
	    0,	/* Obj_Use_In_Expr		Attr_Co_Array		*/
	    0,	/* Obj_Use_In_Expr		Attr_Automatic		*/
	    0	/* Obj_Use_In_Expr		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Use_In_Expr	 	Attr_Bind		*/
	,   0	/* Obj_Use_In_Expr	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Use_Derived_Type ******************/
	/*	This is a name used to type something as a derived type.*/
	/*	Derived types can't be variables, constants, functions,	*/
	/*	or dummy args.	(14.1.2)				*/
	/****************************************************************/

	{ 644,	/* Obj_Use_Derived_Type		Attr_Assumed_Type_Ch	*/
	  644,	/* Obj_Use_Derived_Type		Attr_Dimension		*/
	  644,	/* Obj_Use_Derived_Type		Attr_Explicit_Shp_Arr	*/
	  644,	/* Obj_Use_Derived_Type		Attr_Assumed_Size_Arr	*/
	  644,	/* Obj_Use_Derived_Type		Attr_Deferred_Shp_Arr	*/
	  644,	/* Obj_Use_Derived_Type		Attr_Assumed_Shp_Arr	*/
	  644,	/* Obj_Use_Derived_Type		Attr_Allocatable	*/
	  644,	/* Obj_Use_Derived_Type		Attr_Parameter		*/
	  644,	/* Obj_Use_Derived_Type		Attr_Intent		*/
	  644,	/* Obj_Use_Derived_Type		Attr_Optional		*/

	    0,	/* Obj_Use_Derived_Type		Attr_Private		*/
	    0,	/* Obj_Use_Derived_Type		Attr_Public		*/
		/* 5.2.3 constraint					*/

	  644,	/* Obj_Use_Derived_Type		Attr_Target		*/
	  644,	/* Obj_Use_Derived_Type		Attr_Equivalence	*/
	  644,	/* Obj_Use_Derived_Type		Attr_Save		*/
	  644,	/* Obj_Use_Derived_Type		Attr_Pointer		*/
	  644,	/* Obj_Use_Derived_Type		Attr_External		*/
	  644,	/* Obj_Use_Derived_Type		Attr_Intrinsic		*/
	  644,	/* Obj_Use_Derived_Type		Attr_Data_Init		*/
	  644,	/* Obj_Use_Derived_Type		Attr_Type		*/
	  644,	/* Obj_Use_Derived_Type		Attr_Co_Array		*/
	  644,	/* Obj_Use_Derived_Type		Attr_Automatic		*/
	  644	/* Obj_Use_Derived_Type		Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Use_Derived_Type	 	Attr_Bind		*/
	, 644	/* Obj_Use_Derived_Type	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/************************ Obj_Use_Spec_Expr *********************/
	/* The rules for a specification expression are as follows:	*/
	/* Each object must be a constant, a dummy arg (that doesn't	*/
	/* have the OPTIONAL or the INTENT (OUT) attributes), in a	*/
	/* common block, accessible by USE or HOST association.		*/
	/* Specific rules are found in 7.1.6.2				*/
	/****************************************************************/

	{   0,	/* Obj_Use_Spec_Expr		Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Use_Spec_Expr		Attr_Dimension		*/
	    0,	/* Obj_Use_Spec_Expr		Attr_Explicit_Shp_Arr	*/
	    0,	/* Obj_Use_Spec_Expr		Attr_Assumed_Size_Arr	*/
	    0,	/* Obj_Use_Spec_Expr		Attr_Deferred_Shp_Arr	*/
	    0,	/* Obj_Use_Spec_Expr		Attr_Assumed_Shp_Arr	*/
	    0,	/* Obj_Use_Spec_Expr		Attr_Allocatable	*/
	    0,	/* Obj_Use_Spec_Expr		Attr_Parameter		*/
	    0,	/* Obj_Use_Spec_Expr		Attr_Intent		*/
	  437,	/* Obj_Use_Spec_Expr		Attr_Optional		*/
	    0,	/* Obj_Use_Spec_Expr		Attr_Private		*/
	    0,	/* Obj_Use_Spec_Expr		Attr_Public		*/
	    0,	/* Obj_Use_Spec_Expr		Attr_Target		*/
	    0,	/* Obj_Use_Spec_Expr		Attr_Equivalence	*/
	    0,	/* Obj_Use_Spec_Expr		Attr_Save		*/
	    0,	/* Obj_Use_Spec_Expr		Attr_Pointer		*/
	    0,	/* Obj_Use_Spec_Expr		Attr_External		*/
	    0,	/* Obj_Use_Spec_Expr		Attr_Intrinsic		*/
	    0,	/* Obj_Use_Spec_Expr		Attr_Data_Init		*/
	    0,	/* Obj_Use_Spec_Expr	 	Attr_Type		*/
	    0,	/* Obj_Use_Spec_Expr	 	Attr_Co_Array		*/
	    0,	/* Obj_Use_Spec_Expr	 	Attr_Automatic		*/
	    0	/* Obj_Use_Spec_Expr	 	Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	,   0	/* Obj_Use_Spec_Expr	 	Attr_Bind		*/
	,   0	/* Obj_Use_Spec_Expr	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	},


	/********************** Obj_Use_Init_Expr ***********************/
	/* The rules for an initialization expression are as follows:	*/
	/* It MUST be a CONSTANT.  It may be one of several INTRINSICS	*/
	/* that are foldable.	(7.1.6.1)				*/
	/****************************************************************/

	{   0,	/* Obj_Use_Init_Expr	 	Attr_Assumed_Type_Ch	*/
	    0,	/* Obj_Use_Init_Expr	 	Attr_Dimension		*/
	    0,	/* Obj_Use_Init_Expr	 	Attr_Explicit_Shp_Arr	*/

	  210,	/* Obj_Use_Init_Expr	 	Attr_Assumed_Size_Arr	*/
		/* Constants can't be dargs.	(5.1.2.4.4, 14.1.2)	*/

	  210,	/* Obj_Use_Init_Expr	 	Attr_Deferred_Shp_Arr	*/
		/* Deferred shape must be allocatable or pointer.	*/
		/* Neither allocatable or pointer can be a constant.	*/

	  210,	/* Obj_Use_Init_Expr	 	Attr_Assumed_Shp_Arr	*/
		/* Constants can't be dargs.	(5.1.2.4.4, 14.1.2)	*/

	  210,	/* Obj_Use_Init_Expr	 	Attr_Allocatable	*/
		/* Illegal - 5.1 constraint				*/

	    0,	/* Obj_Use_Init_Expr	 	Attr_Parameter		*/

	  210,	/* Obj_Use_Init_Expr	 	Attr_Intent		*/
	  210,	/* Obj_Use_Init_Expr	 	Attr_Optional		*/
		/* Constants can't be dargs.	(5.1.2.4.4, 14.1.2)	*/

	    0,	/* Obj_Use_Init_Expr	 	Attr_Private		*/
	    0,	/* Obj_Use_Init_Expr	 	Attr_Public		*/

	  210,	/* Obj_Use_Init_Expr	 	Attr_Target		*/
		/* Constraint 5.2.8 - illegal				*/

	  210,	/* Obj_Use_Init_Expr	 	Attr_Equivalence	*/
		/* Constraint in 5.5.1					*/

	  210,	/* Obj_Use_Init_Expr	 	Attr_Save		*/

	  210,	/* Obj_Use_Init_Expr	 	Attr_Pointer		*/
		/* Illegal 5.2.7					*/

	  210,	/* Obj_Use_Init_Expr	 	Attr_External		*/
	  210,	/* Obj_Use_Init_Expr	 	Attr_Intrinsic		*/
		/* (5.1)						*/

	  210,	/* Obj_Use_Init_Expr	 	Attr_Data_Init		*/
		/* Constants can't be variables. (5.2.9, 14.1.2)	*/

	    0,	/* Obj_Use_Init_Expr	 	Attr_Type		*/

	  210,	/* Obj_Use_Init_Expr	 	Attr_Co_Array		*/
	    0,	/* Obj_Use_Init_Expr	 	Attr_Automatic		*/
	  210	/* Obj_Use_Init_Expr	 	Attr_Volatile		*/
#ifdef KEY /* Bug 14150 */
	, 210	/* Obj_Use_Init_Expr	 	Attr_Bind		*/
	, 210	/* Obj_Use_Init_Expr	 	Attr_Value		*/
#endif /* KEY Bug 14150 */
	}
};


long	dir_msg_num[Obj_Done] [Dir_Done]	= {

	{1459,	/* Obj_Assum_Type_Ch	 	Dir_Auxiliary		*/
	 1459,	/* Obj_Assum_Type_Ch		Dir_Vfunction		*/
	    0,	/* Obj_Assum_Type_Ch		Dir_No_Side_Effects	*/
	    0,	/* Obj_Assum_Type_Ch		Dir_Inline		*/
	 1459,	/* Obj_Assum_Type_Ch		Dir_Symmetric		*/
 	    0,	/* Obj_Assum_Type_Ch	 	Dir_Copy_Assumed_Shape	*/
	 1459,	/* Obj_Assum_Type_Ch	 	Dir_Align_Symbol	*/
	 1459,	/* Obj_Assum_Type_Ch	 	Dir_Fill_Symbol		*/
	 1459,	/* Obj_Assum_Type_Ch	 	Dir_Section_Gp		*/
	 1459,	/* Obj_Assum_Type_Ch	 	Dir_Section_Non_Gp	*/
	    0,	/* Obj_Assum_Type_Ch	 	Dir_Ignore_TKR		*/
	    0,	/* Obj_Assum_Type_Ch	 	Dir_Optional		*/
	    0,	/* Obj_Assum_Type_Ch	 	Dir_Ipa			*/
	    0 	/* Obj_Assum_Type_Ch	 	Dir_Name		*/
	},

	{   0,	/* Obj_Expl_Shp_Arr		Dir_Auxiliary		*/
	 1459,	/* Obj_Expl_Shp_Arr		Dir_Vfunction		*/
	    0, 	/* Obj_Expl_Shp_Arr		Dir_No_Side_Effects	*/
	    0, 	/* Obj_Expl_Shp_Arr		Dir_Inline		*/
	    0,	/* Obj_Expl_Shp_Arr		Dir_Symmetric		*/
	 1459,	/* Obj_Expl_Shp_Arr		Dir_Copy_Assumed_Shape	*/
 	    0,	/* Obj_Expl_Shp_Arr		Dir_Align_Symbol	*/
 	    0,	/* Obj_Expl_Shp_Arr		Dir_Fill_Symbol		*/
 	    0,	/* Obj_Expl_Shp_Arr		Dir_Section_Gp		*/
 	    0,	/* Obj_Expl_Shp_Arr		Dir_Section_Non_Gp	*/
 	    0,	/* Obj_Expl_Shp_Arr		Dir_Ignore_TKR		*/
 	 1459,	/* Obj_Expl_Shp_Arr		Dir_Optional		*/
 	    0,	/* Obj_Expl_Shp_Arr		Dir_Ipa			*/
	 1459	/* Obj_Expl_Shp_Arr		Dir_Name		*/
	},

	{   0,	/* Obj_Assum_Size_Arr		Dir_Auxiliary		*/
	 1459,	/* Obj_Assum_Size_Arr		Dir_Vfunction		*/
	 1459, 	/* Obj_Assum_Size_Arr		Dir_No_Side_Effects	*/
	 1459, 	/* Obj_Assum_Size_Arr		Dir_Inline		*/
	 1459, 	/* Obj_Assum_Size_Arr		Dir_Symmetric		*/
	 1459,	/* Obj_Assum_Size_Arr		Dir_Copy_Assumed_Shape	*/
	 1459,	/* Obj_Assum_Size_Arr		Dir_Align_Symbol	*/
	 1459,	/* Obj_Assum_Size_Arr		Dir_Fill_Symbol		*/
	 1459,	/* Obj_Assum_Size_Arr		Dir_Section_Gp		*/
	 1459,	/* Obj_Assum_Size_Arr		Dir_Section_Non_Gp	*/
	    0,	/* Obj_Assum_Size_Arr		Dir_Ignore_TKR		*/
	 1459,	/* Obj_Assum_Size_Arr		Dir_Optional		*/
	 1459,	/* Obj_Assum_Size_Arr		Dir_Ipa			*/
	 1459 	/* Obj_Assum_Size_Arr		Dir_Name		*/
 	},
 
	{1459,	/* Obj_Defrd_Shp_Arr		Dir_Auxiliary		*/
	 1459,	/* Obj_Defrd_Shp_Arr		Dir_Vfunction		*/
	    0, 	/* Obj_Defrd_Shp_Arr		Dir_No_Side_Effects	*/
	    0, 	/* Obj_Defrd_Shp_Arr		Dir_Inline		*/
	 1459,	/* Obj_Defrd_Shp_Arr		Dir_Symmetric		*/
	    0,	/* Obj_Defrd_Shp_Arr	 	Dir_Copy_Assumed_Shape	*/
	    0,	/* Obj_Defrd_Shp_Arr	 	Dir_Align_Symbol	*/
	    0,	/* Obj_Defrd_Shp_Arr	 	Dir_Fill_Symbol		*/
	    0,	/* Obj_Defrd_Shp_Arr	 	Dir_Section_Gp		*/
	    0,	/* Obj_Defrd_Shp_Arr	 	Dir_Section_Non_Gp	*/
	    0,	/* Obj_Defrd_Shp_Arr	 	Dir_Ignore_TKR		*/
	 1459,	/* Obj_Defrd_Shp_Arr	 	Dir_Optional		*/
	    0,	/* Obj_Defrd_Shp_Arr	 	Dir_Ipa			*/
	 1459 	/* Obj_Defrd_Shp_Arr	 	Dir_Name		*/
	},

	{1459,	/* Obj_Assum_Shp_Arr	 	Dir_Auxiliary		*/
	 1459,	/* Obj_Assum_Shp_Arr	 	Dir_Vfunction		*/
	 1459,	/* Obj_Assum_Shp_Arr	 	Dir_No_Side_Effects	*/
	 1459,	/* Obj_Assum_Shp_Arr	 	Dir_Inline		*/
	 1459,	/* Obj_Assum_Shp_Arr	 	Dir_Symmetric		*/
	    0,	/* Obj_Assum_Shp_Arr	 	Dir_Copy_Assumed_Shape	*/
	 1459,	/* Obj_Assum_Shp_Arr	 	Dir_Align_Symbol	*/
	 1459,	/* Obj_Assum_Shp_Arr	 	Dir_Fill_Symbol		*/
	 1459,	/* Obj_Assum_Shp_Arr	 	Dir_Section_Gp		*/
	 1459,	/* Obj_Assum_Shp_Arr	 	Dir_Section_Non_Gp	*/
	 1459,	/* Obj_Assum_Shp_Arr	 	Dir_Ignore_TKR		*/
	 1459,	/* Obj_Assum_Shp_Arr	 	Dir_Optional		*/
	 1459,	/* Obj_Assum_Shp_Arr	 	Dir_Ipa			*/
	 1459 	/* Obj_Assum_Shp_Arr	 	Dir_Name		*/
	},

	{1459,	/* Obj_Co_Array		Dir_Auxiliary		*/
	 1459,	/* Obj_Co_Array		Dir_Vfunction		*/
	 1459,	/* Obj_Co_Array		Dir_No_Side_Effects	*/
	 1459,	/* Obj_Co_Array		Dir_Inline		*/
	 1459,	/* Obj_Co_Array		Dir_Symmetric		*/
	 1459,	/* Obj_Co_Array		Dir_Copy_Assumed_Shape	*/
	 1459,	/* Obj_Co_Array		Dir_Align_Symbol	*/
	 1459,	/* Obj_Co_Array		Dir_Fill_Symbol		*/
	 1459,	/* Obj_Co_Array		Dir_Section_Gp		*/
	 1459,	/* Obj_Co_Array		Dir_Section_Non_Gp	*/
	    0,	/* Obj_Co_Array		Dir_Ignore_TKR		*/
	 1459,	/* Obj_Co_Array		Dir_Optional		*/
	 1459,	/* Obj_Co_Array		Dir_Ipa			*/
	 1459	/* Obj_Co_Array		Dir_Name		*/
	},

	{1459,	/* Obj_Allocatable		Dir_Auxiliary		*/
	 1459,	/* Obj_Allocatable		Dir_Vfunction		*/
	 1459, 	/* Obj_Allocatable		Dir_No_Side_Effects	*/
	 1459, 	/* Obj_Allocatable		Dir_Inline		*/
	 1459,	/* Obj_Allocatable		Dir_Symmetric		*/
	 1459,	/* Obj_Allocatable		Dir_Copy_Assumed_Shape	*/
	 1459,	/* Obj_Allocatable		Dir_Align_Symbol	*/
	 1459,	/* Obj_Allocatable		Dir_Fill_Symbol		*/
	 1459,	/* Obj_Allocatable		Dir_Section_Gp		*/
	 1459,	/* Obj_Allocatable		Dir_Section_Non_Gp	*/
	 1459,	/* Obj_Allocatable		Dir_Ignore_TKR		*/
	 1459,	/* Obj_Allocatable		Dir_Optional		*/
	 1459,	/* Obj_Allocatable		Dir_Ipa			*/
	 1459 	/* Obj_Allocatable		Dir_Name		*/
	},

#ifdef KEY /* Bug 14150 */
	{1459,	/* Obj_Bind			Dir_Auxiliary		*/
	 1459,	/* Obj_Bind			Dir_Vfunction		*/
	 1459, 	/* Obj_Bind			Dir_No_Side_Effects	*/
	 1459, 	/* Obj_Bind			Dir_Inline		*/
	 1459,	/* Obj_Bind			Dir_Symmetric		*/
	 1459,	/* Obj_Bind			Dir_Copy_Assumed_Shape	*/
	 1459,	/* Obj_Bind			Dir_Align_Symbol	*/
	 1459,	/* Obj_Bind			Dir_Fill_Symbol		*/
	 1459,	/* Obj_Bind			Dir_Section_Gp		*/
	 1459,	/* Obj_Bind			Dir_Section_Non_Gp	*/
	 1459,	/* Obj_Bind			Dir_Ignore_TKR		*/
	 1459,	/* Obj_Bind			Dir_Optional		*/
	 1459,	/* Obj_Bind			Dir_Ipa			*/
	 1459 	/* Obj_Bind			Dir_Name		*/
	},

	{1459,	/* Obj_Value			Dir_Auxiliary		*/
	 1459,	/* Obj_Value			Dir_Vfunction		*/
	 1459, 	/* Obj_Value			Dir_No_Side_Effects	*/
	 1459, 	/* Obj_Value			Dir_Inline		*/
	 1459,	/* Obj_Value			Dir_Symmetric		*/
	 1459,	/* Obj_Value			Dir_Copy_Assumed_Shape	*/
	 1459,	/* Obj_Value			Dir_Align_Symbol	*/
	 1459,	/* Obj_Value			Dir_Fill_Symbol		*/
	 1459,	/* Obj_Value			Dir_Section_Gp		*/
	 1459,	/* Obj_Value			Dir_Section_Non_Gp	*/
	 1459,	/* Obj_Value			Dir_Ignore_TKR		*/
	 1459,	/* Obj_Value			Dir_Optional		*/
	 1459,	/* Obj_Value			Dir_Ipa			*/
	 1459 	/* Obj_Value			Dir_Name		*/
	},
#endif /* KEY Bug 14150 */

	{1459,	/* Obj_Constant			Dir_Auxiliary		*/
	 1459,	/* Obj_Constant			Dir_Vfunction		*/
	 1459, 	/* Obj_Constant			Dir_No_Side_Effects	*/
	 1459, 	/* Obj_Constant			Dir_Inline		*/
	 1459,	/* Obj_Constant			Dir_Symmetric		*/
	 1459,	/* Obj_Constant			Dir_Copy_Assumed_Shape	*/
	 1459,	/* Obj_Constant			Dir_Align_Symbol	*/
	 1459,	/* Obj_Constant			Dir_Fill_Symbol		*/
	 1459,	/* Obj_Constant			Dir_Section_Gp		*/
	 1459,	/* Obj_Constant			Dir_Section_Non_Gp	*/
	 1459,	/* Obj_Constant			Dir_Ignore_TKR		*/
	 1459,	/* Obj_Constant			Dir_Optional		*/
	 1459,	/* Obj_Constant			Dir_Ipa			*/
	 1459	/* Obj_Constant			Dir_Name		*/
	},

	{   0,	/* Obj_Intent			Dir_Auxiliary		*/
	 1459,	/* Obj_Intent			Dir_Vfunction		*/
	 1459, 	/* Obj_Intent			Dir_No_Side_Effects	*/
	 1459,	/* Obj_Intent			Dir_Inline		*/
	 1459,	/* Obj_Intent			Dir_Symmetric		*/
	    0,	/* Obj_Intent			Dir_Copy_Assumed_Shape	*/
	 1459,	/* Obj_Intent			Dir_Align_Symbol	*/
	 1459,	/* Obj_Intent			Dir_Fill_Symbol		*/
	 1459,	/* Obj_Intent			Dir_Section_Gp		*/
	 1459,	/* Obj_Intent			Dir_Section_Non_Gp	*/
	    0,	/* Obj_Intent			Dir_Ignore_TKR		*/
	 1459,	/* Obj_Intent			Dir_Optional		*/
	 1459,	/* Obj_Intent			Dir_Ipa			*/
	 1459 	/* Obj_Intent			Dir_Name		*/
	},

	{   0,	/* Obj_Optional			Dir_Auxiliary		*/
	 1459,	/* Obj_Optional			Dir_Vfunction		*/
	 1459, 	/* Obj_Optional			Dir_No_Side_Effects	*/
	 1459, 	/* Obj_Optional			Dir_Inline		*/
	 1459,	/* Obj_Optional			Dir_Symmetric		*/
	    0,	/* Obj_Optional			Dir_Copy_Assumed_Shape	*/
	 1459,	/* Obj_Optional			Dir_Align_Symbol	*/
	 1459,	/* Obj_Optional			Dir_Fill_Symbol		*/
	 1459,	/* Obj_Optional			Dir_Section_Gp		*/
	 1459,	/* Obj_Optional			Dir_Section_Non_Gp	*/
	    0,	/* Obj_Optional			Dir_Ignore_TKR		*/
	    0,	/* Obj_Optional			Dir_Optional		*/
	 1459,	/* Obj_Optional			Dir_Ipa			*/
	    0	/* Obj_Optional			Dir_Name		*/
	},

	{   0,	/* Obj_Private			Dir_Auxiliary		*/
	    0,	/* Obj_Private			Dir_Vfunction		*/
	    0, 	/* Obj_Private			Dir_No_Side_Effects	*/
	    0,	/* Obj_Private			Dir_Inline		*/
	    0,	/* Obj_Private			Dir_Symmetric		*/
	    0,	/* Obj_Private			Dir_Copy_Assumed_Shape	*/
	    0,	/* Obj_Private			Dir_Align_Symbol	*/
	    0,	/* Obj_Private			Dir_Fill_Symbol		*/
	    0,	/* Obj_Private			Dir_Section_Gp		*/
	    0,	/* Obj_Private			Dir_Section_Non_Gp	*/
	    0,	/* Obj_Private			Dir_Ignore_TKR		*/
	    0,	/* Obj_Private			Dir_Optional		*/
	    0,	/* Obj_Private			Dir_Ipa			*/
	    0 	/* Obj_Private			Dir_Name		*/
	},

	{   0,	/* Obj_Public			Dir_Auxiliary		*/
	    0,	/* Obj_Public			Dir_Vfunction		*/
	    0, 	/* Obj_Public			Dir_No_Side_Effects	*/
	    0,	/* Obj_Public			Dir_Inline		*/
	    0,	/* Obj_Public			Dir_Symmetric		*/
	    0,	/* Obj_Public			Dir_Copy_Assumed_Shape	*/
	    0,	/* Obj_Public			Dir_Align_Symbol	*/
	    0,	/* Obj_Public			Dir_Fill_Symbol		*/
	    0,	/* Obj_Public			Dir_Section_Gp		*/
	    0,	/* Obj_Public			Dir_Section_Non_Gp	*/
	    0,	/* Obj_Public			Dir_Ignore_TKR		*/
	    0,	/* Obj_Public			Dir_Optional		*/
	    0,	/* Obj_Public			Dir_Ipa			*/
	    0 	/* Obj_Public			Dir_Name		*/
	},

	{1459,	/* Obj_Target			Dir_Auxiliary		*/
	 1459,	/* Obj_Target			Dir_Vfunction		*/
	    0, 	/* Obj_Target			Dir_No_Side_Effects	*/
	    0, 	/* Obj_Target			Dir_Inline		*/
	 1459,	/* Obj_Target			Dir_Symmetric		*/
	 1459,	/* Obj_Target			Dir_Copy_Assumed_Shape	*/
	    0,	/* Obj_Target			Dir_Align_Symbol	*/
	    0,	/* Obj_Target			Dir_Fill_Symbol		*/
	    0,	/* Obj_Target			Dir_Section_Gp		*/
	    0,	/* Obj_Target			Dir_Section_Non_Gp	*/
	    0,	/* Obj_Target			Dir_Ignore_TKR		*/
	 1459,	/* Obj_Target			Dir_Optional		*/
	    0,	/* Obj_Target			Dir_Ipa			*/
	 1459 	/* Obj_Target			Dir_Name		*/
	},

	{   0,	/* Obj_Equiv			Dir_Auxiliary		*/
	 1459,	/* Obj_Equiv			Dir_Vfunction		*/
	 1459, 	/* Obj_Equiv			Dir_No_Side_Effects	*/
	 1459, 	/* Obj_Equiv			Dir_Inline		*/
	 1459,	/* Obj_Equiv			Dir_Symmetric		*/
	 1459,	/* Obj_Equiv		 	Dir_Copy_Assumed_Shape	*/
	 1459,	/* Obj_Equiv		 	Dir_Align_Symbol	*/
	 1459,	/* Obj_Equiv		 	Dir_Fill_Symbol		*/
	 1459,	/* Obj_Equiv		 	Dir_Section_Gp		*/
	 1459,	/* Obj_Equiv		 	Dir_Section_Non_Gp	*/
	 1459,	/* Obj_Equiv		 	Dir_Ignore_TKR		*/
	 1459,	/* Obj_Equiv		 	Dir_Optional		*/
	 1459,	/* Obj_Equiv		 	Dir_Ipa			*/
	 1459 	/* Obj_Equiv		 	Dir_Name		*/
	},

	{1459,	/* Obj_Saved		 	Dir_Auxiliary		*/
	 1459,	/* Obj_Saved			Dir_Vfunction		*/
	 1459, 	/* Obj_Saved			Dir_No_Side_Effects	*/
	 1459, 	/* Obj_Saved			Dir_Inline		*/
	 1459, 	/* Obj_Saved			Dir_Symmetric		*/
	 1459,	/* Obj_Saved		 	Dir_Copy_Assumed_Shape	*/
	    0,	/* Obj_Saved		 	Dir_Align_Symbol	*/
	    0,	/* Obj_Saved		 	Dir_Fill_Symbol		*/
	    0,	/* Obj_Saved		 	Dir_Section_Gp		*/
	    0,	/* Obj_Saved		 	Dir_Section_Non_Gp	*/
	 1459,	/* Obj_Saved		 	Dir_Ignore_TKR		*/
	 1459,	/* Obj_Saved		 	Dir_Optional		*/
	 1459,	/* Obj_Saved		 	Dir_Ipa			*/
	 1459 	/* Obj_Saved		 	Dir_Name		*/
	},

	{1459,	/* Obj_Automatic		Dir_Auxiliary		*/
	 1459,	/* Obj_Automatic		Dir_Vfunction		*/
	 1459, 	/* Obj_Automatic		Dir_No_Side_Effects	*/
	 1459, 	/* Obj_Automatic		Dir_Inline		*/
	 1459, 	/* Obj_Automatic		Dir_Symmetric		*/
	 1459,	/* Obj_Automatic		Dir_Copy_Assumed_Shape	*/
	    0,	/* Obj_Automatic		Dir_Align_Symbol	*/
	    0,	/* Obj_Automatic		Dir_Fill_Symbol		*/
	 1459,	/* Obj_Automatic		Dir_Section_Gp		*/
	 1459,	/* Obj_Automatic		Dir_Section_Non_Gp	*/
	 1459,	/* Obj_Automatic		Dir_Ignore_TKR		*/
	 1459,	/* Obj_Automatic		Dir_Optional		*/
	 1459,	/* Obj_Automatic		Dir_Ipa			*/
	 1459 	/* Obj_Automatic		Dir_Name		*/
	},

	{1459,	/* Obj_Pointer			Dir_Auxiliary		*/
	 1459,	/* Obj_Pointer			Dir_Vfunction		*/
	    0, 	/* Obj_Pointer			Dir_No_Side_Effects	*/
	    0, 	/* Obj_Pointer			Dir_Inline		*/
	 1459,	/* Obj_Pointer			Dir_Symmetric		*/
	 1459,	/* Obj_Pointer			Dir_Copy_Assumed_Shape	*/
	    0,	/* Obj_Pointer			Dir_Align_Symbol	*/
	    0,	/* Obj_Pointer			Dir_Fill_Symbol		*/
	    0,	/* Obj_Pointer			Dir_Section_Gp		*/
	    0,	/* Obj_Pointer			Dir_Section_Non_Gp	*/
#ifdef KEY /* Bug 14150 */
	    0,	/* Obj_Pointer			Dir_Ignore_TKR		*/
#else /* KEY Bug 14150 */
	 1459,	/* Obj_Pointer			Dir_Ignore_TKR		*/
#endif /* KEY Bug 14150 */
	 1459,	/* Obj_Pointer			Dir_Optional		*/
	    0,	/* Obj_Pointer			Dir_Ipa			*/
	 1459 	/* Obj_Pointer			Dir_Name		*/
	},

	{1459,	/* Obj_Dcl_Extern		Dir_Auxiliary		*/
	 1459,	/* Obj_Dcl_Extern		Dir_Vfunction		*/
	    0, 	/* Obj_Dcl_Extern		Dir_No_Side_Effects	*/
	    0,	/* Obj_Dcl_Extern		Dir_Inline		*/
	 1459,	/* Obj_Dcl_Extern		Dir_Symmetric		*/
	 1459,	/* Obj_Dcl_Extern		Dir_Copy_Assumed_Shape	*/
	 1459,	/* Obj_Dcl_Extern		Dir_Align_Symbol	*/
	 1459,	/* Obj_Dcl_Extern		Dir_Fill_Symbol		*/
	 1459,	/* Obj_Dcl_Extern		Dir_Section_Gp		*/
	 1459,	/* Obj_Dcl_Extern		Dir_Section_Non_Gp	*/
	 1459,	/* Obj_Dcl_Extern		Dir_Ignore_TKR		*/
	    0,	/* Obj_Dcl_Extern		Dir_Optional		*/
	    0,	/* Obj_Dcl_Extern		Dir_Ipa			*/
	    0 	/* Obj_Dcl_Extern		Dir_Name		*/
	},

	{1459,	/* Obj_Dcl_Intrin		Dir_Auxiliary		*/
	 1459,	/* Obj_Dcl_Intrin		Dir_Vfunction		*/
	 1459, 	/* Obj_Dcl_Intrin		Dir_No_Side_Effects	*/
	 1459, 	/* Obj_Dcl_Intrin		Dir_Inline		*/
	 1459,	/* Obj_Dcl_Intrin		Dir_Symmetric		*/
	 1459,	/* Obj_Dcl_Intrin		Dir_Copy_Assumed_Shape	*/
	 1459,	/* Obj_Dcl_Intrin		Dir_Align_Symbol	*/
	 1459,	/* Obj_Dcl_Intrin		Dir_Fill_Symbol		*/
	 1459,	/* Obj_Dcl_Intrin		Dir_Section_Gp		*/
	 1459,	/* Obj_Dcl_Intrin		Dir_Section_Non_Gp	*/
	 1459,	/* Obj_Dcl_Extern		Dir_Ignore_TKR		*/
	    0,	/* Obj_Dcl_Intrin		Dir_Optional		*/
	 1459,	/* Obj_Dcl_Intrin		Dir_Ipa			*/
	 1459 	/* Obj_Dcl_Intrin		Dir_Name		*/
	},

	{1459,	/* Obj_Data_Init		Dir_Auxiliary		*/
	 1459,	/* Obj_Data_Init		Dir_Vfunction		*/
	 1459,	/* Obj_Data_Init		Dir_No_Side_Effects	*/
	 1459, 	/* Obj_Data_Init		Dir_Inline		*/
	 1459,	/* Obj_Data_Init		Dir_Symmetric		*/
	 1459,	/* Obj_Data_Init		Dir_Copy_Assumed_Shape	*/
	 1459,	/* Obj_Data_Init		Dir_Align_Symbol	*/
	 1459,	/* Obj_Data_Init		Dir_Fill_Symbol		*/
	 1459,	/* Obj_Data_Init		Dir_Section_Gp		*/
	 1459,	/* Obj_Data_Init		Dir_Section_Non_Gp	*/
	 1459,	/* Obj_Data_Init		Dir_Ignore_TKR		*/
	 1459,	/* Obj_Data_Init		Dir_Optional		*/
	 1459,	/* Obj_Data_Init		Dir_Ipa			*/
	 1459 	/* Obj_Data_Init		Dir_Name		*/
	},
 
	{   0,	/* Obj_Typed		 	Dir_Auxiliary		*/
	    0,	/* Obj_Typed			Dir_Vfunction		*/
	    0, 	/* Obj_Typed			Dir_No_Side_Effects	*/
	    0, 	/* Obj_Typed			Dir_Inline		*/
	    0,	/* Obj_Typed			Dir_Symmetric		*/
	    0,	/* Obj_Typed		 	Dir_Copy_Assumed_Shape	*/
	    0,	/* Obj_Typed		 	Dir_Align_Symbol	*/
	    0,	/* Obj_Typed		 	Dir_Fill_Symbol		*/
	    0,	/* Obj_Typed		 	Dir_Section_Gp		*/
	    0,	/* Obj_Typed		 	Dir_Section_Non_Gp	*/
	    0,	/* Obj_Typed		 	Dir_Ignore_TKR		*/
	    0,	/* Obj_Typed		 	Dir_Optional		*/
	    0,	/* Obj_Typed		 	Dir_Ipa			*/
	    0 	/* Obj_Typed		 	Dir_Name		*/
	},

	{   0,	/* Obj_Volatile		 	Dir_Auxiliary		*/
	 1459,	/* Obj_Volatile			Dir_Vfunction		*/
	 1459, 	/* Obj_Volatile			Dir_No_Side_Effects	*/
	 1459, 	/* Obj_Volatile			Dir_Inline		*/
	    0,	/* Obj_Volatile			Dir_Symmetric		*/
	    0,	/* Obj_Volatile		 	Dir_Copy_Assumed_Shape	*/
	    0,	/* Obj_Volatile		 	Dir_Align_Symbol	*/
	    0,	/* Obj_Volatile		 	Dir_Fill_Symbol		*/
	    0,	/* Obj_Volatile		 	Dir_Section_Gp		*/
	    0,	/* Obj_Volatile		 	Dir_Section_Non_Gp	*/
	    0,	/* Obj_Volatile		 	Dir_Ignore_TKR		*/
	 1459,	/* Obj_Volatile		 	Dir_Optional		*/
	 1459,	/* Obj_Volatile		 	Dir_Ipa			*/
	 1459 	/* Obj_Volatile		 	Dir_Name		*/
	},

	{1457,	/* Obj_Copy_Assumed_Shape	Dir_Auxiliary		*/
	 1457,	/* Obj_Copy_Assumed_Shape	Dir_Vfunction		*/
	 1457,	/* Obj_Copy_Assumed_Shape	Dir_No_Side_Effects	*/
	 1457,	/* Obj_Copy_Assumed_Shape	Dir_Inline		*/
	 1457,	/* Obj_Copy_Assumed_Shape	Dir_Symmetric		*/
	 1457,	/* Obj_Copy_Assumed_Shape	Dir_Copy_Assumed_Shape	*/
	 1457,	/* Obj_Copy_Assumed_Shape	Dir_Align_Symbol	*/
	 1457,	/* Obj_Copy_Assumed_Shape	Dir_Fill_Symbol		*/
	 1457,	/* Obj_Copy_Assumed_Shape	Dir_Section_Gp		*/
	 1457,	/* Obj_Copy_Assumed_Shape	Dir_Section_Non_Gp	*/
	    0,	/* Obj_Copy_Assumed_Shape	Dir_Ignore_TKR		*/
	 1457,	/* Obj_Copy_Assumed_Shape	Dir_Optional		*/
	 1457,	/* Obj_Copy_Assumed_Shape	Dir_Ipa			*/
	 1457	/* Obj_Copy_Assumed_Shape	Dir_Name		*/
	},

	{   0,	/* Obj_Auxiliary		Dir_Auxiliary		*/
	 1457,	/* Obj_Auxiliary		Dir_Vfunction		*/
	 1457, 	/* Obj_Auxiliary		Dir_No_Side_Effects	*/
	 1457, 	/* Obj_Auxiliary		Dir_Inline		*/
	 1457,	/* Obj_Auxiliary		Dir_Symmetric		*/
	 1457,	/* Obj_Auxiliary		Dir_Copy_Assumed_Shape	*/
	 1457,	/* Obj_Auxiliary		Dir_Align_Symbol	*/
	 1457,	/* Obj_Auxiliary		Dir_Fill_Symbol		*/
	 1457,	/* Obj_Auxiliary		Dir_Section_Gp		*/
	 1457,	/* Obj_Auxiliary		Dir_Section_Non_Gp	*/
	    0,	/* Obj_Auxiliary		Dir_Ignore_TKR		*/
	 1457,	/* Obj_Auxiliary		Dir_Optional		*/
	 1457,	/* Obj_Auxiliary		Dir_Ipa			*/
	 1457 	/* Obj_Auxiliary		Dir_Name		*/
	},

	{1457,	/* Obj_Vfunction		Dir_Auxiliary		*/
	 1457,	/* Obj_Vfunction		Dir_Vfunction		*/
	    0, 	/* Obj_Vfunction		Dir_No_Side_Effects	*/
	    0,	/* Obj_Vfunction		Dir_Inline		*/
	 1457,	/* Obj_Vfunction		Dir_Symmetric		*/
	 1457,	/* Obj_Vfunction		Dir_Copy_Assumed_Shape	*/
	 1457,	/* Obj_Vfunction		Dir_Align_Symbol	*/
	 1457,	/* Obj_Vfunction		Dir_Fill_Symbol		*/
	 1457,	/* Obj_Vfunction		Dir_Section_Gp		*/
	 1457,	/* Obj_Vfunction		Dir_Section_Non_Gp	*/
	 1457,	/* Obj_Vfunction		Dir_Ignore_TKR		*/
	    0,	/* Obj_Vfunction		Dir_Optional		*/
	    0,	/* Obj_Vfunction		Dir_Ipa			*/
	    0 	/* Obj_Vfunction		Dir_Name		*/
	},

	{1457,	/* Obj_No_Side_Effects		Dir_Auxiliary		*/
	    0,	/* Obj_No_Side_Effects		Dir_Vfunction		*/
	    0, 	/* Obj_No_Side_Effects		Dir_No_Side_Effects	*/
	    0, 	/* Obj_No_Side_Effects		Dir_Inline		*/
	 1457,	/* Obj_No_Side_Effects		Dir_Symmetric		*/
	 1457,	/* Obj_No_Side_Effects		Dir_Copy_Assumed_Shape	*/
	 1457,	/* Obj_No_Side_Effects		Dir_Align_Symbol	*/
	 1457,	/* Obj_No_Side_Effects		Dir_Fill_Symbol		*/
	 1457,	/* Obj_No_Side_Effects		Dir_Section_Gp		*/
	 1457,	/* Obj_No_Side_Effects		Dir_Section_Non_Gp	*/
	 1457,	/* Obj_No_Side_Effects		Dir_Ignore_TKR		*/
	    0,	/* Obj_No_Side_Effects		Dir_Optional		*/
	    0,	/* Obj_No_Side_Effects		Dir_Ipa			*/
	    0 	/* Obj_No_Side_Effects		Dir_Name		*/
	},

	{1457,	/* Obj_Symmetric		Dir_Auxiliary		*/
	 1457,	/* Obj_Symmetric		Dir_Vfunction		*/
	 1457, 	/* Obj_Symmetric		Dir_No_Side_Effects	*/
	 1457, 	/* Obj_Symmetric		Dir_Inline		*/
	 1457,	/* Obj_Symmetric		Dir_Symmetric		*/
	 1457,	/* Obj_Symmetric		Dir_Copy_Assumed_Shape	*/
	 1457,	/* Obj_Symmetric		Dir_Align_Symbol	*/
	 1457,	/* Obj_Symmetric		Dir_Fill_Symbol		*/
	 1457,	/* Obj_Symmetric		Dir_Section_Gp		*/
	 1457,	/* Obj_Symmetric		Dir_Section_Non_Gp	*/
	 1457,	/* Obj_Symmetric		Dir_Ignore_TKR		*/
	 1457,	/* Obj_Symmetric		Dir_Optional		*/
	 1457,	/* Obj_Symmetric		Dir_Ipa			*/
	 1457 	/* Obj_Symmetric		Dir_Name		*/
	},

	{1457,	/* Obj_Inline			Dir_Auxiliary		*/
	    0,	/* Obj_Inline			Dir_Vfunction		*/
	    0, 	/* Obj_Inline			Dir_No_Side_Effects	*/
	    0, 	/* Obj_Inline			Dir_Inline		*/
	 1457,	/* Obj_Inline			Dir_Symmetric		*/
	 1457,	/* Obj_Inline			Dir_Copy_Assumed_Shape	*/
	 1457,	/* Obj_Inline			Dir_Align_Symbol	*/
	 1457,	/* Obj_Inline			Dir_Fill_Symbol		*/
	 1457,	/* Obj_Inline			Dir_Section_Gp		*/
	 1457,	/* Obj_Inline			Dir_Section_Non_Gp	*/
	 1457,	/* Obj_Inline			Dir_Ignore_TKR		*/
	    0,	/* Obj_Inline			Dir_Optional		*/
	    0,	/* Obj_Inline			Dir_Ipa			*/
	    0 	/* Obj_Inline			Dir_Name		*/
	},

	{1457,	/* Obj_Ipa			Dir_Auxiliary		*/
	    0,	/* Obj_Ipa			Dir_Vfunction		*/
	    0, 	/* Obj_Ipa			Dir_No_Side_Effects	*/
	    0, 	/* Obj_Ipa			Dir_Inline		*/
	 1457,	/* Obj_Ipa			Dir_Symmetric		*/
	 1457,	/* Obj_Ipa			Dir_Copy_Assumed_Shape	*/
	 1457,	/* Obj_Ipa			Dir_Align_Symbol	*/
	 1457,	/* Obj_Ipa			Dir_Fill_Symbol		*/
	 1457,	/* Obj_Ipa			Dir_Section_Gp		*/
	 1457,	/* Obj_Ipa			Dir_Section_Non_Gp	*/
	 1457,	/* Obj_Ipa			Dir_Ignore_TKR		*/
	    0,	/* Obj_Ipa			Dir_Optional		*/
	    0,	/* Obj_Ipa			Dir_Ipa			*/
	    0 	/* Obj_Ipa			Dir_Name		*/
	},


	{1457,	/* Obj_Align_Symbol		Dir_Auxiliary		*/
	 1457,	/* Obj_Align_Symbol		Dir_Vfunction		*/
	 1457, 	/* Obj_Align_Symbol		Dir_No_Side_Effects	*/
	 1457, 	/* Obj_Align_Symbol		Dir_Inline		*/
	 1457,	/* Obj_Align_Symbol		Dir_Symmetric		*/
	 1457,	/* Obj_Align_Symbol		Dir_Copy_Assumed_Shape	*/
	 1457,	/* Obj_Align_Symbol		Dir_Align_Symbol	*/
	 1457,	/* Obj_Align_Symbol		Dir_Fill_Symbol		*/
	    0,	/* Obj_Align_Symbol		Dir_Section_Gp		*/
	    0,	/* Obj_Align_Symbol		Dir_Section_Non_Gp	*/
	 1457,	/* Obj_Align_Symbol		Dir_Ignore_TKR		*/
	 1457,	/* Obj_Align_Symbol		Dir_Optional		*/
	 1457,	/* Obj_Align_Symbol		Dir_Ipa			*/
	 1457 	/* Obj_Align_Symbol		Dir_Name		*/
	},

	{1457,	/* Obj_Fill_Symbol		Dir_Auxiliary		*/
	 1457,	/* Obj_Fill_Symbol		Dir_Vfunction		*/
	 1457, 	/* Obj_Fill_Symbol		Dir_No_Side_Effects	*/
	 1457, 	/* Obj_Fill_Symbol		Dir_Inline		*/
	 1457,	/* Obj_Fill_Symbol		Dir_Symmetric		*/
	 1457,	/* Obj_Fill_Symbol		Dir_Copy_Assumed_Shape	*/
	 1457,	/* Obj_Fill_Symbol		Dir_Align_Symbol	*/
	 1457,	/* Obj_Fill_Symbol		Dir_Fill_Symbol		*/
	    0,	/* Obj_Fill_Symbol		Dir_Section_Gp		*/
	    0,	/* Obj_Fill_Symbol		Dir_Section_Non_Gp	*/
	 1457,	/* Obj_Fill_Symbol		Dir_Ignore_TKR		*/
	 1457,	/* Obj_Fill_Symbol		Dir_Optional		*/
	 1457,	/* Obj_Fill_Symbol		Dir_Ipa			*/
	 1457 	/* Obj_Fill_Symbol		Dir_Name		*/
	},

	{1457,	/* Obj_Section_Gp		Dir_Auxiliary		*/
	 1457,	/* Obj_Section_Gp		Dir_Vfunction		*/
	 1457, 	/* Obj_Section_Gp		Dir_No_Side_Effects	*/
	 1457, 	/* Obj_Section_Gp		Dir_Inline		*/
	 1457,	/* Obj_Section_Gp		Dir_Symmetric		*/
	 1457,	/* Obj_Section_Gp		Dir_Copy_Assumed_Shape	*/
	    0,	/* Obj_Section_Gp		Dir_Align_Symbol	*/
	    0,	/* Obj_Section_Gp		Dir_Fill_Symbol		*/
	 1457,	/* Obj_Section_Gp		Dir_Section_Gp		*/
	 1457,	/* Obj_Section_Gp		Dir_Section_Non_Gp	*/
	 1457,	/* Obj_Section_Gp		Dir_Ignore_TKR		*/
	 1457,	/* Obj_Section_Gp		Dir_Optional		*/
	 1457,	/* Obj_Section_Gp		Dir_Ipa			*/
	 1457 	/* Obj_Section_Gp		Dir_Name		*/
	},

	{1457,	/* Obj_Section_Non_Gp		Dir_Auxiliary		*/
	 1457,	/* Obj_Section_Non_Gp		Dir_Vfunction		*/
	 1457, 	/* Obj_Section_Non_Gp		Dir_No_Side_Effects	*/
	 1457, 	/* Obj_Section_Non_Gp		Dir_Inline		*/
	 1457,	/* Obj_Section_Non_Gp		Dir_Symmetric		*/
	 1457,	/* Obj_Section_Non_Gp		Dir_Copy_Assumed_Shape	*/
    	    0,	/* Obj_Section_Non_Gp		Dir_Align_Symbol	*/
	    0,	/* Obj_Section_Non_Gp		Dir_Fill_Symbol		*/
	 1457,	/* Obj_Section_Non_Gp		Dir_Section_Gp		*/
	 1457,	/* Obj_Section_Non_Gp		Dir_Section_Non_Gp	*/
	 1457,	/* Obj_Section_Non_Gp		Dir_Ignore_TKR		*/
	 1457,	/* Obj_Section_Non_Gp		Dir_Optional		*/
	 1457,	/* Obj_Section_Non_Gp		Dir_Ipa			*/
	 1457 	/* Obj_Section_Non_Gp		Dir_Name		*/
	},

	{   0,	/* Obj_Ingore_TKR		Dir_Auxiliary		*/
	 1457,	/* Obj_Ignore_TKR		Dir_Vfunction		*/
	 1457, 	/* Obj_Ignore_TKR		Dir_No_Side_Effects	*/
	 1457, 	/* Obj_Ignore_TKR		Dir_Inline		*/
	 1457,	/* Obj_Ignore_TKR		Dir_Symmetric		*/
	    0,	/* Obj_Ignore_TKR		Dir_Copy_Assumed_Shape	*/
	 1457,	/* Obj_Ignore_TKR		Dir_Align_Symbol	*/
	 1457,	/* Obj_Ignore_TKR		Dir_Fill_Symbol		*/
	 1457,	/* Obj_Ignore_TKR		Dir_Section_Gp		*/
	 1457,	/* Obj_Ignore_TKR		Dir_Section_Non_Gp	*/
	 1457,	/* Obj_Ignore_TKR		Dir_Ignore_TKR		*/
	 1457,	/* Obj_Ignore_TKR		Dir_Optional		*/
	 1457,	/* Obj_Ignore_TKR		Dir_Ipa			*/
	 1457 	/* Obj_Ignore_TKR		Dir_Name		*/
	},

	{1457,	/* Obj_Optional_Dir		Dir_Auxiliary		*/
	    0,	/* Obj_Optional_Dir		Dir_Vfunction		*/
	    0, 	/* Obj_Optional_Dir		Dir_No_Side_Effects	*/
	    0, 	/* Obj_Optional_Dir		Dir_Inline		*/
	 1457,	/* Obj_Optional_Dir		Dir_Symmetric		*/
	 1457,	/* Obj_Optional_Dir		Dir_Copy_Assumed_Shape	*/
	 1457,	/* Obj_Optional_Dir		Dir_Align_Symbol	*/
	 1457,	/* Obj_Optional_Dir		Dir_Fill_Symbol		*/
	 1457,	/* Obj_Optional_Dir		Dir_Section_Gp		*/
	 1457,	/* Obj_Optional_Dir		Dir_Section_Non_Gp	*/
	 1457,	/* Obj_Optional_Dir		Dir_Ignore_TKR		*/
	 1457,	/* Obj_Optional_Dir		Dir_Optional		*/
	    0,	/* Obj_Optional_Dir		Dir_Ipa			*/
	    0 	/* Obj_Optional_Dir		Dir_Name		*/
	},

	{1457,	/* Obj_Name			Dir_Auxiliary		*/
	    0,	/* Obj_Name			Dir_Vfunction		*/
	    0, 	/* Obj_Name			Dir_No_Side_Effects	*/
	    0, 	/* Obj_Name			Dir_Inline		*/
	 1457,	/* Obj_Name			Dir_Symmetric		*/
	 1457,	/* Obj_Name			Dir_Copy_Assumed_Shape	*/
	 1457,	/* Obj_Name			Dir_Align_Symbol	*/
	 1457,	/* Obj_Name			Dir_Fill_Symbol		*/
	 1457,	/* Obj_Name			Dir_Section_Gp		*/
	 1457,	/* Obj_Name			Dir_Section_Non_Gp	*/
	 1457,	/* Obj_Name			Dir_Ignore_TKR		*/
	 1457,	/* Obj_Name			Dir_Optional		*/
	    0,	/* Obj_Name			Dir_Ipa			*/
	 1457 	/* Obj_Name			Dir_Name		*/
	},

	{1458,	/* Obj_Cri_Ptr			Dir_Auxiliary		*/
	 1458,	/* Obj_Cri_Ptr			Dir_Vfunction		*/
	 1458, 	/* Obj_Cri_Ptr			Dir_No_Side_Effects	*/
	 1458, 	/* Obj_Cri_Ptr			Dir_Inline		*/
	    0,	/* Obj_Cri_Ptr			Dir_Symmetric		*/
	 1458,	/* Obj_Cri_Ptr			Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Cri_Ptr			Dir_Align_Symbol	*/
	 1458,	/* Obj_Cri_Ptr			Dir_Fill_Symbol		*/
	 1458,	/* Obj_Cri_Ptr			Dir_Section_Gp		*/
	 1458,	/* Obj_Cri_Ptr			Dir_Section_Non_Gp	*/
	    0,	/* Obj_Cri_Ptr			Dir_Ignore_TKR		*/
	 1458,	/* Obj_Cri_Ptr			Dir_Optional		*/
	 1458,	/* Obj_Cri_Ptr			Dir_Ipa			*/
	 1458 	/* Obj_Cri_Ptr			Dir_Name		*/
	},

	{1458,	/* Obj_Cri_Pointee		Dir_Auxiliary		*/
	 1458,	/* Obj_Cri_Pointee		Dir_Vfunction		*/
	 1458, 	/* Obj_Cri_Pointee		Dir_No_Side_Effects	*/
	 1458, 	/* Obj_Cri_Pointee		Dir_Inline		*/
	 1458,	/* Obj_Cri_Pointee		Dir_Symmetric		*/
	 1458,	/* Obj_Cri_Pointee		Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Cri_Pointee		Dir_Align_Symbol	*/
	 1458,	/* Obj_Cri_Pointee		Dir_Fill_Symbol		*/
	 1458,	/* Obj_Cri_Pointee		Dir_Section_Gp		*/
	 1458,	/* Obj_Cri_Pointee		Dir_Section_Non_Gp	*/
	 1458,	/* Obj_Cri_Pointee		Dir_Ignore_TKR		*/
	 1458,	/* Obj_Cri_Pointee		Dir_Optional		*/
	 1458,	/* Obj_Cri_Pointee		Dir_Ipa			*/
	 1458 	/* Obj_Cri_Pointee		Dir_Name		*/
	},

	{1458,	/* Obj_Cri_Ch_Pointee		Dir_Auxiliary		*/
	 1458,	/* Obj_Cri_Ch_Pointee		Dir_Vfunction		*/
	 1458, 	/* Obj_Cri_Ch_Pointee		Dir_No_Side_Effects	*/
	 1458, 	/* Obj_Cri_Ch_Pointee		Dir_Inline		*/
	 1458,	/* Obj_Cri_Ch_Pointee		Dir_Symmetric		*/
	 1458,	/* Obj_Cri_Ch_Pointee		Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Cri_Ch_Pointee		Dir_Align_Symbol	*/
	 1458,	/* Obj_Cri_Ch_Pointee		Dir_Fill_Symbol		*/
	 1458,	/* Obj_Cri_Ch_Pointee		Dir_Section_Gp		*/
	 1458,	/* Obj_Cri_Ch_Pointee		Dir_Section_Non_Gp	*/
	 1458,	/* Obj_Cri_Ch_Pointee		Dir_Ignore_TKR		*/
	 1458,	/* Obj_Cri_Ch_Pointee		Dir_Optional		*/
	 1458,	/* Obj_Cri_Ch_Pointee		Dir_Ipa			*/
	 1458 	/* Obj_Cri_Ch_Pointee		Dir_Name		*/
	},

	{1458,	/* Obj_Ntry_Func_Result		Dir_Auxiliary		*/
	 1458,	/* Obj_Ntry_Func_Result		Dir_Vfunction		*/
	 1458,	/* Obj_Ntry_Func_Result		Dir_No_Side_Effects	*/
	 1458, 	/* Obj_Ntry_Func_Result		Dir_Inline		*/
	 1458,	/* Obj_Ntry_Func_Result		Dir_Symmetric		*/
	 1458,	/* Obj_Ntry_Func_Result		Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Ntry_Func_Result		Dir_Align_Symbol	*/
	 1458,	/* Obj_Ntry_Func_Result		Dir_Fill_Symbol		*/
	 1458,	/* Obj_Ntry_Func_Result		Dir_Section_Gp		*/
	 1458,	/* Obj_Ntry_Func_Result		Dir_Section_Non_Gp	*/
	 1458,	/* Obj_Ntry_Func_Result		Dir_Ignore_TKR		*/
	 1458,	/* Obj_Ntry_Func_Result		Dir_Optional		*/
	 1458,	/* Obj_Ntry_Func_Result		Dir_Ipa			*/
	 1458 	/* Obj_Ntry_Func_Result		Dir_Name		*/
	},

	{   0,	/* Obj_Dummy_Arg		Dir_Auxiliary		*/
	 1458,	/* Obj_Dummy_Arg		Dir_Vfunction		*/
	 1458, 	/* Obj_Dummy_Arg		Dir_No_Side_Effects	*/
	 1458, 	/* Obj_Dummy_Arg		Dir_Inline		*/
	 1458,	/* Obj_Dummy_Arg		Dir_Symmetric		*/
	    0,	/* Obj_Dummy_Arg		Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Dummy_Arg		Dir_Align_Symbol	*/
	 1458,	/* Obj_Dummy_Arg		Dir_Fill_Symbol		*/
	 1458,	/* Obj_Dummy_Arg		Dir_Section_Gp		*/
	 1458,	/* Obj_Dummy_Arg		Dir_Section_Non_Gp	*/
	    0,	/* Obj_Dummy_Arg		Dir_Ignore_TKR		*/
	    0,	/* Obj_Dummy_Arg		Dir_Optional		*/
	 1458,	/* Obj_Dummy_Arg		Dir_Ipa			*/
	    0 	/* Obj_Dummy_Arg		Dir_Name		*/
	},

	{   0,	/* Obj_Common_Obj		Dir_Auxiliary		*/
	 1458,	/* Obj_Common_Obj		Dir_Vfunction		*/
	 1458, 	/* Obj_Common_Obj		Dir_No_Side_Effects	*/
	 1458, 	/* Obj_Common_Obj		Dir_Inline		*/
	 1458,	/* Obj_Common_Obj		Dir_Symmetric		*/
	 1458,	/* Obj_Common_Obj		Dir_Copy_Assumed_Shape	*/
	    0,	/* Obj_Common_Obj		Dir_Align_Symbol	*/
	    0,	/* Obj_Common_Obj		Dir_Fill_Symbol		*/
	    0,	/* Obj_Common_Obj		Dir_Section_Gp		*/
	    0,	/* Obj_Common_Obj		Dir_Section_Non_Gp	*/
	 1458,	/* Obj_Common_Obj		Dir_Ignore_TKR		*/
	 1458,	/* Obj_Common_Obj		Dir_Optional		*/
	 1458,	/* Obj_Common_Obj		Dir_Ipa			*/
	 1458 	/* Obj_Common_Obj		Dir_Name		*/
	},

	{1458,	/* Obj_Namelist_Obj		Dir_Auxiliary		*/
	 1458,	/* Obj_Namelist_Obj		Dir_Vfunction		*/
	 1458,	/* Obj_Namelist_Obj		Dir_No_Side_Effects	*/
	 1458, 	/* Obj_Namelist_Obj		Dir_Inline		*/
	    0,	/* Obj_Namelist_Obj		Dir_Symmetric		*/
	 1458,	/* Obj_Namelist_Obj		Dir_Copy_Assumed_Shape	*/
	    0,	/* Obj_Namelist_Obj		Dir_Align_Symbol	*/
	    0,	/* Obj_Namelist_Obj		Dir_Fill_Symbol		*/
	    0,	/* Obj_Namelist_Obj		Dir_Section_Gp		*/
	    0,	/* Obj_Namelist_Obj		Dir_Section_Non_Gp	*/
	    0,	/* Obj_Namelist_Obj		Dir_Ignore_TKR		*/
	 1458,	/* Obj_Namelist_Obj		Dir_Optional		*/
	 1458,	/* Obj_Namelist_Obj		Dir_Ipa			*/
	 1458 	/* Obj_Namelist_Obj		Dir_Name		*/
	},

	{1458,	/* Obj_Module_Proc		Dir_Auxiliary		*/
	 1458,	/* Obj_Module_Proc		Dir_Vfunction		*/
	 1458, 	/* Obj_Module_Proc		Dir_No_Side_Effects	*/
	    0,	/* Obj_Module_Proc		Dir_Inline		*/
	 1458,	/* Obj_Module_Proc		Dir_Symmetric		*/
	 1458,	/* Obj_Module_Proc		Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Module_Proc		Dir_Align_Symbol	*/
	 1458,	/* Obj_Module_Proc		Dir_Fill_Symbol		*/
	 1458,	/* Obj_Module_Proc		Dir_Section_Gp		*/
	 1458,	/* Obj_Module_Proc		Dir_Section_Non_Gp	*/
	 1458,	/* Obj_Module_Proc		Dir_Ignore_TKR		*/
	 1458,	/* Obj_Module_Proc		Dir_Optional		*/
	    0,	/* Obj_Module_Proc		Dir_Ipa			*/
	 1458 	/* Obj_Module_Proc		Dir_Name		*/
	},

	{1458,	/* Obj_Derived_Type		Dir_Auxiliary		*/
	 1458,	/* Obj_Derived_Type		Dir_Vfunction		*/
	 1458, 	/* Obj_Derived_Type		Dir_No_Side_Effects	*/
	 1458, 	/* Obj_Derived_Type		Dir_Inline		*/
	 1458,	/* Obj_Derived_Type		Dir_Symmetric		*/
	 1458,	/* Obj_Derived_Type		Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Derived_Type		Dir_Align_Symbol	*/
	 1458,	/* Obj_Derived_Type		Dir_Fill_Symbol		*/
	 1458,	/* Obj_Derived_Type		Dir_Section_Gp		*/
	 1458,	/* Obj_Derived_Type		Dir_Section_Non_Gp	*/
	 1458,	/* Obj_Derived_Type		Dir_Ignore_TKR		*/
	 1458,	/* Obj_Derived_Type		Dir_Optional		*/
	 1458,	/* Obj_Derived_Type		Dir_Ipa			*/
	 1458 	/* Obj_Derived_Type		Dir_Name		*/
	},

	{1458,	/* Obj_Generic_Interface	Dir_Auxiliary		*/
	 1458,	/* Obj_Generic_Interface	Dir_Vfunction		*/
	 1458, 	/* Obj_Generic_Interface	Dir_No_Side_Effects	*/
	    0, 	/* Obj_Generic_Interface	Dir_Inline		*/
	 1458,	/* Obj_Generic_Interface	Dir_Symmetric		*/
	 1458,	/* Obj_Generic_Interface	Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Generic_Interface	Dir_Align_Symbol	*/
	 1458,	/* Obj_Generic_Interface	Dir_Fill_Symbol		*/
	 1458,	/* Obj_Generic_Interface	Dir_Section_Gp		*/
	 1458,	/* Obj_Generic_Interface	Dir_Section_Non_Gp	*/
	 1458,	/* Obj_Generic_Interface	Dir_Ignore_TKR		*/
	 1458,	/* Obj_Generic_Interface	Dir_Optional		*/
	    0,	/* Obj_Generic_Interface	Dir_Ipa			*/
	 1458 	/* Obj_Generic_Interface	Dir_Name		*/
	},

	{1458,	/* Obj_Namelist_Grp		Dir_Auxiliary		*/
	 1458,	/* Obj_Namelist_Grp		Dir_Vfunction		*/
	 1458, 	/* Obj_Namelist_Grp		Dir_No_Side_Effects	*/
	 1458, 	/* Obj_Namelist_Grp		Dir_Inline		*/
	 1458,	/* Obj_Namelist_Grp		Dir_Symmetric		*/
	 1458,	/* Obj_Namelist_Grp		Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Namelist_Grp		Dir_Align_Symbol	*/
	 1458,	/* Obj_Namelist_Grp		Dir_Fill_Symbol		*/
	 1458,	/* Obj_Namelist_Grp		Dir_Section_Gp		*/
	 1458,	/* Obj_Namelist_Grp		Dir_Section_Non_Gp	*/
	 1458,	/* Obj_Namelist_Grp		Dir_Ignore_TKR		*/
	 1458,	/* Obj_Namelist_Grp		Dir_Optional		*/
	 1458,	/* Obj_Namelist_Grp		Dir_Ipa			*/
	 1458 	/* Obj_Namelist_Grp		Dir_Name		*/
	},

	{1458,	/* Obj_Stmt_Func		Dir_Auxiliary		*/
	 1458,	/* Obj_Stmt_Func		Dir_Vfunction		*/
	 1458, 	/* Obj_Stmt_Func		Dir_No_Side_Effects	*/
	 1458, 	/* Obj_Stmt_Func		Dir_Inline		*/
	 1458,	/* Obj_Stmt_Func		Dir_Symmetric		*/
	 1458,	/* Obj_Stmt_Func		Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Stmt_Func		Dir_Align_Symbol	*/
	 1458,	/* Obj_Stmt_Func		Dir_Fill_Symbol		*/
	 1458,	/* Obj_Stmt_Func		Dir_Section_Gp		*/
	 1458,	/* Obj_Stmt_Func		Dir_Section_Non_Gp	*/
	 1458,	/* Obj_Stmt_Func		Dir_Ignore_TKR		*/
	 1458,	/* Obj_Stmt_Func		Dir_Optional		*/
	 1458,	/* Obj_Stmt_Func		Dir_Ipa			*/
	 1458 	/* Obj_Stmt_Func		Dir_Name		*/
	},

	{1458,	/* Obj_Construct		Dir_Auxiliary		*/
	 1458,	/* Obj_Construct		Dir_Vfunction		*/
	 1458, 	/* Obj_Construct		Dir_No_Side_Effects	*/
	 1458, 	/* Obj_Construct		Dir_Inline		*/
	 1458,	/* Obj_Construct		Dir_Symmetric		*/
	 1458,	/* Obj_Construct		Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Construct		Dir_Align_Symbol	*/
	 1458,	/* Obj_Construct		Dir_Fill_Symbol		*/
	 1458,	/* Obj_Construct		Dir_Section_Gp		*/
	 1458,	/* Obj_Construct		Dir_Section_Non_Gp	*/
	 1458,	/* Obj_Construct		Dir_Ignore_TKR		*/
	 1458,	/* Obj_Construct		Dir_Optional		*/
	 1458,	/* Obj_Construct		Dir_Ipa			*/
	 1458 	/* Obj_Construct		Dir_Name		*/
	},

	{1458,	/* Obj_Entry_Func		Dir_Auxiliary		*/
	 1458,	/* Obj_Entry_Func		Dir_Vfunction		*/
	 1458, 	/* Obj_Entry_Func		Dir_No_Side_Effects	*/
	 1458,	/* Obj_Entry_Func		Dir_Inline		*/
	 1458,	/* Obj_Entry_Func		Dir_Symmetric		*/
	 1458,	/* Obj_Entry_Func		Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Entry_Func		Dir_Align_Symbol	*/
	 1458,	/* Obj_Entry_Func		Dir_Fill_Symbol		*/
	 1458,	/* Obj_Entry_Func		Dir_Section_Gp		*/
	 1458,	/* Obj_Entry_Func		Dir_Section_Non_Gp	*/
	 1458,	/* Obj_Entry_Func		Dir_Ignore_TKR		*/
	 1458,	/* Obj_Entry_Func		Dir_Optional		*/
	 1458,	/* Obj_Entry_Func		Dir_Ipa			*/
	 1458 	/* Obj_Entry_Func		Dir_Name		*/
	},

	{1458,	/* Obj_Entry_Subr		Dir_Auxiliary		*/
	 1458,	/* Obj_Entry_Subr		Dir_Vfunction		*/
	 1458, 	/* Obj_Entry_Subr		Dir_No_Side_Effects	*/
	 1458, 	/* Obj_Entry_Subr		Dir_Inline		*/
	 1458,	/* Obj_Entry_Subr		Dir_Symmetric		*/
	 1458,	/* Obj_Entry_Subr		Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Entry_Subr		Dir_Align_Symbol	*/
	 1458,	/* Obj_Entry_Subr		Dir_Fill_Symbol		*/
	 1458,	/* Obj_Entry_Subr		Dir_Section_Gp		*/
	 1458,	/* Obj_Entry_Subr		Dir_Section_Non_Gp	*/
	 1458,	/* Obj_Entry_Subr		Dir_Ignore_TKR		*/
	 1458,	/* Obj_Entry_Subr		Dir_Optional		*/
	 1458,	/* Obj_Entry_Subr		Dir_Ipa			*/
	 1458 	/* Obj_Entry_Subr		Dir_Name		*/
	},

	{1458,	/* Obj_Intern_Func		Dir_Auxiliary		*/
	 1458,	/* Obj_Intern_Func		Dir_Vfunction		*/
	 1458, 	/* Obj_Intern_Func		Dir_No_Side_Effects	*/
	    0, 	/* Obj_Intern_Func		Dir_Inline		*/
	 1458,	/* Obj_Intern_Func		Dir_Symmetric		*/
	 1458,	/* Obj_Intern_Func		Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Intern_Func		Dir_Align_Symbol	*/
	 1458,	/* Obj_Intern_Func		Dir_Fill_Symbol		*/
	 1458,	/* Obj_Intern_Func		Dir_Section_Gp		*/
	 1458,	/* Obj_Intern_Func		Dir_Section_Non_Gp	*/
	 1458,	/* Obj_Intern_Func		Dir_Ignore_TKR		*/
	 1458,	/* Obj_Intern_Func		Dir_Optional		*/
	    0,	/* Obj_Intern_Func		Dir_Ipa			*/
	 1458 	/* Obj_Intern_Func		Dir_Name		*/
	},

	{1458,	/* Obj_Intern_Subr		Dir_Auxiliary		*/
	 1458,	/* Obj_Intern_Subr		Dir_Vfunction		*/
	 1458, 	/* Obj_Intern_Subr		Dir_No_Side_Effects	*/
	    0, 	/* Obj_Intern_Subr		Dir_Inline		*/
	 1458,	/* Obj_Intern_Subr		Dir_Symmetric		*/
	 1458,	/* Obj_Intern_Subr		Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Intern_Subr		Dir_Align_Symbol	*/
	 1458,	/* Obj_Intern_Subr		Dir_Fill_Symbol		*/
	 1458,	/* Obj_Intern_Subr		Dir_Section_Gp		*/
	 1458,	/* Obj_Intern_Subr		Dir_Section_Non_Gp	*/
	 1458,	/* Obj_Intern_Subr		Dir_Ignore_TKR		*/
	 1458,	/* Obj_Intern_Subr		Dir_Optional		*/
	    0,	/* Obj_Intern_Subr		Dir_Ipa			*/
	 1458 	/* Obj_Intern_Subr		Dir_Name		*/
	},

	{1458,	/* Obj_Module_Func		Dir_Auxiliary		*/
	 1458,	/* Obj_Module_Func		Dir_Vfunction		*/
	 1458, 	/* Obj_Module_Func		Dir_No_Side_Effects	*/
	    0, 	/* Obj_Module_Func		Dir_Inline		*/
	 1458,	/* Obj_Module_Func		Dir_Symmetric		*/
	 1458,	/* Obj_Module_Func		Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Module_Func		Dir_Align_Symbol	*/
	 1458,	/* Obj_Module_Func		Dir_Fill_Symbol		*/
	 1458,	/* Obj_Module_Func		Dir_Section_Gp		*/
	 1458,	/* Obj_Module_Func		Dir_Section_Non_Gp	*/
	 1458,	/* Obj_Module_Func		Dir_Ignore_TKR		*/
	 1458,	/* Obj_Module_Func		Dir_Optional		*/
	    0,	/* Obj_Module_Func		Dir_Ipa			*/
	 1458 	/* Obj_Module_Func		Dir_Name		*/
	},

	{1458,	/* Obj_Module_Subr		Dir_Auxiliary		*/
	 1458,	/* Obj_Module_Subr		Dir_Vfunction		*/
	 1458, 	/* Obj_Module_Subr		Dir_No_Side_Effects	*/
	    0, 	/* Obj_Module_Subr		Dir_Inline		*/
	 1458,	/* Obj_Module_Subr		Dir_Symmetric		*/
	 1458,	/* Obj_Module_Subr		Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Module_Subr		Dir_Align_Symbol	*/
	 1458,	/* Obj_Module_Subr		Dir_Fill_Symbol		*/
	 1458,	/* Obj_Module_Subr		Dir_Section_Gp		*/
	 1458,	/* Obj_Module_Subr		Dir_Section_Non_Gp	*/
	 1458,	/* Obj_Module_Subr		Dir_Ignore_TKR		*/
	 1458,	/* Obj_Module_Subr		Dir_Optional		*/
	    0,	/* Obj_Module_Subr		Dir_Ipa			*/
	 1458 	/* Obj_Module_Subr		Dir_Name		*/
	},


	{1458,	/* Obj_Sf_Darg			Dir_Auxiliary		*/
	 1458,	/* Obj_Sf_Darg			Dir_Vfunction		*/
	 1458, 	/* Obj_Sf_Darg			Dir_No_Side_Effects	*/
	 1458, 	/* Obj_Sf_Darg			Dir_Inline		*/
	    0,	/* Obj_Sf_Darg			Dir_Symmetric		*/
	 1458,	/* Obj_Sf_Darg			Dir_Copy_Assumed_Shape	*/
	 1458,	/* Obj_Sf_Darg			Dir_Align_Symbol	*/
	 1458,	/* Obj_Sf_Darg			Dir_Fill_Symbol		*/
	 1458,	/* Obj_Sf_Darg			Dir_Section_Gp		*/
	 1458,	/* Obj_Sf_Darg			Dir_Section_Non_Gp	*/
	    0,	/* Obj_Sf_Darg			Dir_Ignore_TKR		*/
	 1458,	/* Obj_Sf_Darg			Dir_Optional		*/
	 1458,	/* Obj_Sf_Darg			Dir_Ipa			*/
	 1458 	/* Obj_Sf_Darg			Dir_Name		*/
	},

	{   0,	/* Obj_Sf_Actual_Arg		Dir_Auxiliary		*/
	 1463,	/* Obj_Sf_Actual_Arg		Dir_Vfunction		*/
	 1463, 	/* Obj_Sf_Actual_Arg		Dir_No_Side_Effects	*/
	 1463, 	/* Obj_Sf_Actual_Arg		Dir_Inline		*/
	    0,	/* Obj_Sf_Actual_Arg		Dir_Symmetric		*/
	    0,	/* Obj_Sf_Actual_Arg	 	Dir_Copy_Assumed_Shape	*/
	    0,	/* Obj_Sf_Actual_Arg	 	Dir_Align_Symbol	*/
	    0,	/* Obj_Sf_Actual_Arg	 	Dir_Fill_Symbol		*/
	    0,	/* Obj_Sf_Actual_Arg	 	Dir_Section_Gp		*/
	    0,	/* Obj_Sf_Actual_Arg	 	Dir_Section_Non_Gp	*/
	    0,	/* Obj_Sf_Actual_Arg	 	Dir_Ignore_TKR		*/
	 1463,	/* Obj_Sf_Actual_Arg	 	Dir_Optional		*/
	 1463,	/* Obj_Sf_Actual_Arg	 	Dir_Ipa			*/
	 1463 	/* Obj_Sf_Actual_Arg	 	Dir_Name		*/
	},

	{1452,	/* Obj_Var_Len_Ch		Dir_Auxiliary		*/
	 1452,	/* Obj_Var_Len_Ch		Dir_Vfunction		*/
	    0, 	/* Obj_Var_Len_Ch		Dir_No_Side_Effects	*/
	    0, 	/* Obj_Var_Len_Ch		Dir_Inline		*/
	    0,	/* Obj_Var_Len_Ch		Dir_Symmetric		*/
	    0,	/* Obj_Var_Len_Ch		Dir_Copy_Assumed_Shape	*/
	 1452,	/* Obj_Var_Len_Ch		Dir_Align_Symbol	*/
	 1452,	/* Obj_Var_Len_Ch		Dir_Fill_Symbol		*/
	 1452,	/* Obj_Var_Len_Ch		Dir_Section_Gp		*/
	 1452,	/* Obj_Var_Len_Ch		Dir_Section_Non_Gp	*/
	    0,	/* Obj_Var_Len_Ch		Dir_Ignore_TKR		*/
	 1452,	/* Obj_Var_Len_Ch		Dir_Optional		*/
	    0,	/* Obj_Var_Len_Ch		Dir_Ipa			*/
	 1452 	/* Obj_Var_Len_Ch		Dir_Name		*/
	},

	{   0,	/* Obj_Var_Len_Arr		Dir_Auxiliary		*/
	 1464,	/* Obj_Var_Len_Arr		Dir_Vfunction		*/
	    0, 	/* Obj_Var_Len_Arr		Dir_No_Side_Effects	*/
	    0, 	/* Obj_Var_Len_Arr		Dir_Inline		*/
	    0,	/* Obj_Var_Len_Arr		Dir_Symmetric		*/
	 1464,	/* Obj_Var_Len_Arr		Dir_Copy_Assumed_Shape	*/
	    0,	/* Obj_Var_Len_Arr		Dir_Align_Symbol	*/
	    0,	/* Obj_Var_Len_Arr		Dir_Fill_Symbol		*/
	 1464,	/* Obj_Var_Len_Arr		Dir_Section_Gp		*/
	 1464,	/* Obj_Var_Len_Arr		Dir_Section_Non_Gp	*/
	    0,	/* Obj_Var_Len_Arr		Dir_Ignore_TKR		*/
	 1464,	/* Obj_Var_Len_Arr		Dir_Optional		*/
	    0,	/* Obj_Var_Len_Arr		Dir_Ipa			*/
	 1464 	/* Obj_Var_Len_Arr		Dir_Name		*/
	},

	{   0,	/* Obj_Sym_Constant_Arr		Dir_Auxiliary		*/
	 1465,	/* Obj_Sym_Constant_Arr		Dir_Vfunction		*/
	    0, 	/* Obj_Sym_Constant_Arr		Dir_No_Side_Effects	*/
	    0, 	/* Obj_Sym_Constant_Arr		Dir_Inline		*/
	    0,	/* Obj_Sym_Constant_Arr		Dir_Symmetric		*/
	    0,	/* Obj_Sym_Constant_Arr		Dir_Copy_Assumed_Shape	*/
	 1465,	/* Obj_Sym_Constant_Arr		Dir_Align_Symbol	*/
	 1465,	/* Obj_Sym_Constant_Arr		Dir_Fill_Symbol		*/
	 1465,	/* Obj_Sym_Constant_Arr		Dir_Section_Gp		*/
	 1465,	/* Obj_Sym_Constant_Arr		Dir_Section_Non_Gp	*/
	 1465,	/* Obj_Sym_Constant_Arr		Dir_Ignore_TKR		*/
	 1465,	/* Obj_Sym_Constant_Arr		Dir_Optional		*/
	    0,	/* Obj_Sym_Constant_Arr		Dir_Ipa			*/
	 1465	/* Obj_Sym_Constant_Arr		Dir_Name		*/
	},

	{1466,	/* Obj_Interface_Func		Dir_Auxiliary		*/
	 1466,	/* Obj_Interface_Func		Dir_Vfunction		*/
	 1466, 	/* Obj_Interface_Func		Dir_No_Side_Effects	*/
	    0, 	/* Obj_Interface_Func		Dir_Inline		*/
	 1466,	/* Obj_Interface_Func		Dir_Symmetric		*/
	 1466,	/* Obj_Interface_Func		Dir_Copy_Assumed_Shape	*/
	 1466,	/* Obj_Interface_Func		Dir_Align_Symbol	*/
	 1466,	/* Obj_Interface_Func		Dir_Fill_Symbol		*/
	 1466,	/* Obj_Interface_Func		Dir_Section_Gp		*/
	 1466,	/* Obj_Interface_Func		Dir_Section_Non_Gp	*/
	 1466,	/* Obj_Interface_Func		Dir_Ignore_TKR		*/
	 1466,	/* Obj_Interface_Func		Dir_Optional		*/
	    0,	/* Obj_Interface_Func		Dir_Ipa			*/
	 1466 	/* Obj_Interface_Func		Dir_Name		*/
	},

	{1466,	/* Obj_Interface_Subr		Dir_Auxiliary		*/
	 1466,	/* Obj_Interface_Subr		Dir_Vfunction		*/
	 1466, 	/* Obj_Interface_Subr		Dir_No_Side_Effects	*/
	    0, 	/* Obj_Interface_Subr		Dir_Inline		*/
	 1466,	/* Obj_Interface_Subr		Dir_Symmetric		*/
	 1466,	/* Obj_Interface_Subr		Dir_Copy_Assumed_Shape	*/
	 1466,	/* Obj_Interface_Subr		Dir_Align_Symbol	*/
	 1466,	/* Obj_Interface_Subr		Dir_Fill_Symbol		*/
	 1466,	/* Obj_Interface_Subr		Dir_Section_Gp		*/
	 1466,	/* Obj_Interface_Subr		Dir_Section_Non_Gp	*/
	 1466,	/* Obj_Interface_Subr		Dir_Ignore_TKR		*/
	 1466,	/* Obj_Interface_Subr		Dir_Optional		*/
	    0,	/* Obj_Interface_Subr		Dir_Ipa			*/
	 1466 	/* Obj_Interface_Subr		Dir_Name		*/
	},

	{1467,	/* Obj_Use_Extern_Func		Dir_Auxiliary		*/
	    0,	/* Obj_Use_Extern_Func		Dir_Vfunction		*/
	    0, 	/* Obj_Use_Extern_Func		Dir_No_Side_Effects	*/
	    0, 	/* Obj_Use_Extern_Func		Dir_Inline		*/
	 1467,	/* Obj_Use_Extern_Func		Dir_Symmetric		*/
	 1467,	/* Obj_Use_Extern_Func		Dir_Copy_Assumed_Shape	*/
	 1467,	/* Obj_Use_Extern_Func		Dir_Align_Symbol	*/
	 1467,	/* Obj_Use_Extern_Func		Dir_Fill_Symbol		*/
	 1467,	/* Obj_Use_Extern_Func		Dir_Section_Gp		*/
	 1467,	/* Obj_Use_Extern_Func		Dir_Section_Non_Gp	*/
	 1467,	/* Obj_Use_Extern_Func		Dir_Ignore_TKR		*/
	    0,	/* Obj_Use_Extern_Func		Dir_Optional		*/
	    0,	/* Obj_Use_Extern_Func		Dir_Ipa			*/
	    0 	/* Obj_Use_Extern_Func		Dir_Name		*/
	},

	{1468,	/* Obj_Use_Extern_Subr		Dir_Auxiliary		*/
	 1468,	/* Obj_Use_Extern_Subr		Dir_Vfunction		*/
	    0, 	/* Obj_Use_Extern_Subr		Dir_No_Side_Effects	*/
	    0, 	/* Obj_Use_Extern_Subr		Dir_Inline		*/
	 1468,	/* Obj_Use_Extern_Subr		Dir_Symmetric		*/
	 1468,	/* Obj_Use_Extern_Subr		Dir_Copy_Assumed_Shape	*/
	 1468,	/* Obj_Use_Extern_Subr		Dir_Align_Symbol	*/
	 1468,	/* Obj_Use_Extern_Subr		Dir_Fill_Symbol		*/
	 1468,	/* Obj_Use_Extern_Subr		Dir_Section_Gp		*/
	 1468,	/* Obj_Use_Extern_Subr		Dir_Section_Non_Gp	*/
	 1468,	/* Obj_Use_Extern_Subr		Dir_Ignore_TKR		*/
	    0,	/* Obj_Use_Extern_Subr		Dir_Optional		*/
	    0,	/* Obj_Use_Extern_Subr		Dir_Ipa			*/
	    0 	/* Obj_Use_Extern_Subr		Dir_Name		*/
	},

	{   0,	/* Obj_Use_In_Expr		Dir_Auxiliary		*/
	    0,	/* Obj_Use_In_Expr		Dir_Vfunction		*/
	    0, 	/* Obj_Use_In_Expr		Dir_No_Side_Effects	*/
	    0, 	/* Obj_Use_In_Expr		Dir_Inline		*/
	    0,	/* Obj_Use_In_Expr		Dir_Symmetric		*/
	    0,	/* Obj_Use_In_Expr		Dir_Copy_Assumed_Shape	*/
	    0,	/* Obj_Use_In_Expr		Dir_Align_Symbol	*/
	    0,	/* Obj_Use_In_Expr		Dir_Fill_Symbol		*/
	    0,	/* Obj_Use_In_Expr		Dir_Section_Gp		*/
	    0,	/* Obj_Use_In_Expr		Dir_Section_Non_Gp	*/
	    0,	/* Obj_Use_In_Expr		Dir_Ignore_TKR		*/
	    0,	/* Obj_Use_In_Expr		Dir_Optional		*/
	    0,	/* Obj_Use_In_Expr		Dir_Ipa			*/
	    0 	/* Obj_Use_In_Expr		Dir_Name		*/
	},

	{1469,	/* Obj_Use_Derived_Type		Dir_Auxiliary		*/
	 1469,	/* Obj_Use_Derived_Type		Dir_Vfunction		*/
	 1469, 	/* Obj_Use_Derived_Type		Dir_No_Side_Effects	*/
	 1469, 	/* Obj_Use_Derived_Type		Dir_Inline		*/
	 1469,	/* Obj_Use_Derived_Type		Dir_Symmetric		*/
	 1469,	/* Obj_Use_Derived_Type		Dir_Copy_Assumed_Shape	*/
	 1469,	/* Obj_Use_Derived_Type		Dir_Align_Symbol	*/
	 1469,	/* Obj_Use_Derived_Type		Dir_Fill_Symbol		*/
	 1469,	/* Obj_Use_Derived_Type		Dir_Section_Gp		*/
	 1469,	/* Obj_Use_Derived_Type		Dir_Section_Non_Gp	*/
	 1469,	/* Obj_Use_Derived_Type		Dir_Ignore_TKR		*/
	 1469,	/* Obj_Use_Derived_Type		Dir_Optional		*/
	 1469,	/* Obj_Use_Derived_Type		Dir_Ipa			*/
	 1469 	/* Obj_Use_Derived_Type		Dir_Name		*/
	},

	{   0,	/* Obj_Use_Spec_Expr	 	Dir_Auxiliary		*/
	    0,	/* Obj_Use_Spec_Expr		Dir_Vfunction		*/
	    0,	/* Obj_Use_Spec_Expr		Dir_No_Side_Effects	*/
	    0,	/* Obj_Use_Spec_Expr		Dir_Inline		*/
	    0,	/* Obj_Use_Spec_Expr		Dir_Symmetric		*/
	    0,	/* Obj_Use_Spec_Expr	 	Dir_Copy_Assumed_Shape	*/
	    0,	/* Obj_Use_Spec_Expr	 	Dir_Align_Symbol	*/
	    0,	/* Obj_Use_Spec_Expr	 	Dir_Fill_Symbol		*/
	    0,	/* Obj_Use_Spec_Expr	 	Dir_Section_Gp		*/
	    0,	/* Obj_Use_Spec_Expr	 	Dir_Section_Non_Gp	*/
	    0,	/* Obj_Use_Spec_Expr	 	Dir_Ignore_TKR		*/
	    0,	/* Obj_Use_Spec_Expr	 	Dir_Optional		*/
	    0,	/* Obj_Use_Spec_Expr	 	Dir_Ipa			*/
	    0 	/* Obj_Use_Spec_Expr	 	Dir_Name		*/
	},

	{1470,	/* Obj_Use_Init_Expr	 	Dir_Auxiliary		*/
	 1470,	/* Obj_Use_Init_Expr		Dir_Vfunction		*/
	    0, 	/* Obj_Use_Init_Expr		Dir_No_Side_Effects	*/
	    0, 	/* Obj_Use_Init_Expr		Dir_Inline		*/
	    0,	/* Obj_Use_Init_Expr		Dir_Symmetric		*/
	 1470,	/* Obj_Use_Init_Expr	 	Dir_Copy_Assumed_Shape	*/
	 1470,	/* Obj_Use_Init_Expr	 	Dir_Align_Symbol	*/
	 1470,	/* Obj_Use_Init_Expr	 	Dir_Fill_Symbol		*/
	 1470,	/* Obj_Use_Init_Expr	 	Dir_Section_Gp		*/
	 1470,	/* Obj_Use_Init_Expr	 	Dir_Section_Non_Gp	*/
	 1470,	/* Obj_Use_Init_Expr	 	Dir_Ignore_TKR		*/
	 1470,	/* Obj_Use_Init_Expr	 	Dir_Optional		*/
	    0,	/* Obj_Use_Init_Expr	 	Dir_Ipa			*/
	 1470 	/* Obj_Use_Init_Expr	 	Dir_Name		*/
	}
};


long	name_msg_num[Obj_Done] [Name_Done]	= {

	/********************** Obj_Assum_Type_Ch ***********************/
	/* A length type value of star may only be used as a dummy	*/
	/* argument, a constant, or the name of the function result,	*/
	/* in the external function begin compiled.	(5.1.1.5)	*/
	/* Also can be a Cray character pointer.			*/
	/****************************************************************/

	{   0,	/* Obj_Assum_Type_Ch		Name_Variable		*/
	  551,	/* Obj_Assum_Type_Ch		Name_Common_Obj		*/
	  551,	/* Obj_Assum_Type_Ch		Name_Cri_Pointer	*/

# if defined(_EXTENDED_CRI_CHAR_POINTER)
	    0,	/* Obj_Assum_Type_Ch		Name_Cri_Pointee	*/
	    0,	/* Obj_Assum_Type_Ch		Name_Cri_Ch_Pointee	*/
# else
	  625,	/* Obj_Assum_Type_Ch		Name_Cri_Pointee	*/
		/* Must declare type before making it a char pointee. 	*/
		/* This uses a special message, but takes the same	*/
		/* msg args as msg #  551 does.			 	*/

	  551,	/* Obj_Assum_Type_Ch		Name_Cri_Ch_Pointee	*/
# endif
	    0,	/* Obj_Assum_Type_Ch		Name_Func_Result	*/
	    0,	/* Obj_Assum_Type_Ch		Name_Dummy_Arg		*/

	  551,	/* Obj_Assum_Type_Ch		Name_Module_Proc	*/
		/* 12.3.1 says that a module procedure has an explicit	*/
		/* interface.	12.3.2.1 says that a procedure must not	*/
		/* have more than one explicit specific interface in	*/
		/* a given scoping unit.  12.3 details what is in an	*/
		/* interface.						*/

	  551,	/* Obj_Assum_Type_Ch		Name_Derived_Type	*/
		/* 5.1.1.5						*/

	  551,	/* Obj_Assum_Type_Ch		Name_Generic_Interface	*/
		/* If a function result is assumed type character, it 	*/
		/* must be the current function being compiled.	This	*/
		/* can't be a generic interface.  12.3.2.1 constraint	*/

	  551,	/* Obj_Assum_Type_Ch		Name_Namelist_Group	*/
		/* 14.1.2						*/

	  551,	/* Obj_Assum_Type_Ch		Name_Namelist_Group_Obj	*/
		/* 5.4							*/

	  551,	/* Obj_Assum_Type_Ch		Name_Statement_Func	*/
		/* Can't be a character with an unknown len. (12.5.4)	*/

	  551,	/* Obj_Assum_Type_Ch		Name_Construct		*/
		/* 14.1.2						*/

	  551,	/* Obj_Assum_Type_Ch		Name_Intrinsic_Func	*/
	  551,	/* Obj_Assum_Type_Ch		Name_Intrinsic_Subr	*/
		/* Definition says only external functions.		*/

	  551,	/* Obj_Assum_Type_Ch		Name_Module		*/
	  551,	/* Obj_Assum_Type_Ch		Name_Blockdata		*/
	  551,	/* Obj_Assum_Type_Ch		Name_Program		*/
		/* 14.1.2						*/

	    0,	/* Obj_Assum_Type_Ch		Name_Function		*/
	    0,	/* Obj_Assum_Type_Ch		Name_Curr_Func		*/
		/* This is legal, if this is an external function.	*/

	  551, 	/* Obj_Assum_Type_Ch		Name_Curr_Subr		*/

	  551,	/* Obj_Assum_Type_Ch		Name_Internal_Func	*/
	  551	/* Obj_Assum_Type_Ch		Name_Internal_Subr	*/
		/* Definition says only external functions.		*/

	},


	/************************ Obj_Expl_Shp_Arr **********************/
	/* An explict shape array is a named array that is declared	*/
	/* with explicit values for the bounds in each dimension of	*/
	/* the array.	(5.1.2.4.1)  All explicit shape arrays (both	*/
	/* with constant bounds and with non-constant bounds), use this	*/
	/* Obj_... category.	At end pass1, array bounds are resolved.*/
	/* If they are constant, semantic checking is done, if they are	*/
	/* non-constant, fnd_semantic_err is called again for the	*/
	/* object, using Obj_Var_Len_Arr.  There is nothing that can	*/
	/* be a variable length array that cannot be an array with	*/
	/* constant bounds.						*/
	/****************************************************************/

	{   0,	/* Obj_Expl_Shp_Arr		Name_Variable		*/
	    0,	/* Obj_Expl_Shp_Arr		Name_Common_Obj		*/

		/* Cray extension - illegal to be an array.		*/

	  551,	/* Obj_Expl_Shp_Arr		Name_Cri_Pointer	*/
	    0,	/* Obj_Expl_Shp_Arr		Name_Cri_Pointee	*/
# if defined(_EXTENDED_CRI_CHAR_POINTER)
	    0,	/* Obj_Expl_Shp_Arr		Name_Cri_Ch_Pointee	*/
# else
 	 1408,	/* Obj_Expl_Shp_Arr		Name_Cri_Ch_Pointee	*/
# endif
	    0,	/* Obj_Expl_Shp_Arr		Name_Func_Result	*/
	    0,	/* Obj_Expl_Shp_Arr		Name_Dummy_Arg		*/

	  551,	/* Obj_Expl_Shp_Arr		Name_Module_Proc	*/
		/* 12.3.1 says that a module procedure has an explicit	*/
		/* interface.	12.3.2.1 says that a procedure must not	*/
		/* have more than one explicit specific interface in	*/
		/* a given scoping unit.  12.3 details what is in an	*/
		/* interface.						*/

	  551,	/* Obj_Expl_Shp_Arr		Name_Derived_Type	*/
		/* 14.1.2						*/

	  551,	/* Obj_Expl_Shp_Arr		Name_Generic_Interface	*/
		/* Can't add something to an explicit interface.	*/

	  551,	/* Obj_Expl_Shp_Arr		Name_Namelist_Group	*/
		/* 14.1.2						*/

	    0,	/* Obj_Expl_Shp_Arr		Name_Namelist_Group_Obj	*/
		/* 5.4							*/

	  551,	/* Obj_Expl_Shp_Arr		Name_Statement_Func	*/
		/* 12.5.4 constraint - Must be scalar.			*/

	  551,	/* Obj_Expl_Shp_Arr		Name_Construct		*/
		/* 14.1.2						*/

	  551,	/* Obj_Expl_Shp_Arr		Name_Intrinsic_Func	*/
	  551,	/* Obj_Expl_Shp_Arr		Name_Intrinsic_Subr	*/
		/* By exclusion.  5.1 Discussion allows only type.	*/

	  551,	/* Obj_Expl_Shp_Arr		Name_Module		*/
	  551,	/* Obj_Expl_Shp_Arr		Name_Blockdata		*/
	  551,	/* Obj_Expl_Shp_Arr		Name_Program		*/
		/* 14.1.2						*/

	    0,	/* Obj_Expl_Shp_Arr		Name_Function		*/
	    0,	/* Obj_Expl_Shp_Arr		Name_Curr_Func		*/

	  551,	/* Obj_Expl_Shp_Arr		Name_Curr_Subr		*/

	  551,	/* Obj_Expl_Shp_Arr		Name_Internal_Func	*/
	  551	/* Obj_Expl_Shp_Arr		Name_Internal_Subr	*/
		/* 12.3.1 - If a procedure is accessible in a scoping	*/
		/* unit its interface is either explicit or implicit	*/
		/* in that scoping unit.  The interface of an internal	*/
		/* procedure is always explicit.  (12.3.2)  The		*/
		/* interface specification statements may appear in	*/
		/* the procedure definition or an interface block.	*/
		/* But internals can't be specified in an interface	*/
		/* block.  (12.3.2.1 discussion) A procedure may not	*/
		/* have more than one explicit interface per scoping	*/
		/* unit.  So what this means is that all attributes	*/
		/* of an internal function must be specified in the	*/
		/* procedure definition.  When fnd_semantic_err goes	*/
		/* thru the attribute the current procedure being	*/
		/* compiled is Name_Curr_Func (or Name_Curr_Subr) not	*/
		/* Name_Internal_Func (or Name_Internal_Subr).	These	*/
		/* are reserved for references to the procedure		*/
		/* outside of the procedure itself.			*/

	},

	/************************ Obj_Assum_Size_Arr ********************/
	/* This is a dummy argument array.	(5.1.2.4.4)		*/
	/****************************************************************/

	{ 551,	/* Obj_Assum_Size_Arr		Name_Variable		*/
	  551,	/* Obj_Assum_Size_Arr		Name_Common_Obj		*/
		/* Dargs can't be in common.	(5.5.2)		 	*/

	  551,	/* Obj_Assum_Size_Arr		Name_Cri_Pointer	*/
	    0,	/* Obj_Assum_Size_Arr		Name_Cri_Pointee	*/

# if defined(_EXTENDED_CRI_CHAR_POINTER)
	    0,	/* Obj_Assum_Size_Arr		Name_Cri_Ch_Pointee	*/
# else
	1408,	/* Obj_Assum_Size_Arr		Name_Cri_Ch_Pointee	*/
# endif
	  551,	/* Obj_Assum_Size_Arr		Name_Func_Result	*/
		/* 14.1.2						*/

	    0,	/* Obj_Assum_Size_Arr		Name_Dummy_Arg		*/

	  551,	/* Obj_Assum_Size_Arr		Name_Module_Proc	*/
		/* 12.3.1 says that a module procedure has an explicit	*/
		/* interface.	12.3.2.1 says that a procedure must not	*/
		/* have more than one explicit specific interface in	*/
		/* a given scoping unit.  12.3 details what is in an	*/
		/* interface.						*/

	  551,	/* Obj_Assum_Size_Arr		Name_Derived_Type	*/
		/* 14.1.2						*/

	  551,	/* Obj_Assum_Size_Arr		Name_Generic_Interface	*/
		/* Function can't be darg				*/

	  551,	/* Obj_Assum_Size_Arr		Name_Namelist_Group	*/
		/* 5.4							*/

	  551,	/* Obj_Assum_Size_Arr		Name_Namelist_Group_Obj	*/
		/* 5.4							*/

	  551,	/* Obj_Assum_Size_Arr		Name_Statement_Func	*/
		/* 12.5.4 constraint - Stmt function must be scalar.	*/

	  551,	/* Obj_Assum_Size_Arr		Name_Construct		*/
	  551,	/* Obj_Assum_Size_Arr		Name_Intrinsic_Func	*/
	  551,	/* Obj_Assum_Size_Arr		Name_Intrinsic_Subr	*/
	  551,	/* Obj_Assum_Size_Arr		Name_Module		*/
	  551,	/* Obj_Assum_Size_Arr		Name_Blockdata		*/
	  551,	/* Obj_Assum_Size_Arr		Name_Program		*/
	  551,	/* Obj_Assum_Size_Arr		Name_Function		*/
	  551,	/* Obj_Assum_Size_Arr		Name_Curr_Func		*/
	  551, 	/* Obj_Assum_Size_Arr		Name_Curr_Subr		*/
	  551,	/* Obj_Assum_Size_Arr		Name_Internal_Func	*/
	  551	/* Obj_Assum_Size_Arr		Name_Internal_Subr	*/
		/* None of these can be dargs.	14.1.2			*/
	},

	/************************ Obj_Defrd_Shp_Arr *********************/
	/* A deferred shape array is an array pointer or an		*/
	/* allocatable array. (5.1.2.4.3)				*/
	/**************************************************************(*/

	{   0,	/* Obj_Defrd_Shp_Arr		Name_Variable		*/
	    0,	/* Obj_Defrd_Shp_Arr		Name_Common_Obj		*/
		/* Legal - Mentioned in discussion in 5.5.2.3	 	*/

	  551,	/* Obj_Defrd_Shp_Arr		Name_Cri_Pointer	*/
	  551,	/* Obj_Defrd_Shp_Arr		Name_Cri_Pointee	*/
		/* Cray extension - illegal to be an array.		*/

	  551,	/* Obj_Defrd_Shp_Arr		Name_Cri_Ch_Pointee	*/
		/* Cri character pointee must be a scalar.		*/

	    0,	/* Obj_Defrd_Shp_Arr		Name_Func_Result	*/
		/* 12.2.2 says a function result can be a pointer.  A	*/
		/* deferred shape array can also be a pointer.		*/

	    0,	/* Obj_Defrd_Shp_Arr		Name_Dummy_Arg		*/
		/* 5.1.2.4.3 - discussion - legal			*/

	  551,	/* Obj_Defrd_Shp_Arr		Name_Module_Proc	*/
		/* 12.3.1 says that a module procedure has an explicit	*/
		/* interface.	12.3.2.1 says that a procedure must not */
		/* have more than one explicit specific interface in	*/
		/* a given scoping unit.  12.3 details what is in an	*/
		/* interface.						*/

	  551,	/* Obj_Defrd_Shp_Arr		Name_Derived_Type	*/
	  551,	/* Obj_Defrd_Shp_Arr		Name_Generic_Interface	*/
	  551,	/* Obj_Defrd_Shp_Arr		Name_Namelist_Group	*/
	  551,	/* Obj_Defrd_Shp_Arr		Name_Namelist_Group_Obj	*/
		/* 5.4							*/

	  551,	/* Obj_Defrd_Shp_Arr		Name_Statement_Func	*/
		/* 12.5.4 constraint - Stmt function must be scalar.	*/

	  551,	/* Obj_Defrd_Shp_Arr		Name_Construct		*/
		/* 14.1.2						*/

	  551,	/* Obj_Defrd_Shp_Arr		Name_Intrinsic_Func	*/
	  551,	/* Obj_Defrd_Shp_Arr		Name_Intrinsic_Subr	*/
		/* By exclusion.  5.1 Discussion allows only type.	*/

	  551,	/* Obj_Defrd_Shp_Arr		Name_Module		*/
	  551,	/* Obj_Defrd_Shp_Arr		Name_Blockdata		*/
	  551,	/* Obj_Defrd_Shp_Arr		Name_Program		*/
		/* 14.1.2						*/

	    0,	/* Obj_Defrd_Shp_Arr		Name_Function		*/
	    0,	/* Obj_Defrd_Shp_Arr		Name_Curr_Func		*/
		/* 12.2.2 - legal					*/

	  551, 	/* Obj_Defrd_Shp_Arr		Name_Curr_Subr		*/

	  551,	/* Obj_Defrd_Shp_Arr		Name_Internal_Func	*/
		/* Can't redefine something with an explicit interface	*/
		/* See	[Obj_Expl_Shp_Arr, Name_Internal_Subr]		*/

	  551	/* Obj_Defrd_Shp_Arr		Name_Internal_Subr	*/
	},


	/************************ Obj_Assum_Shp_Arr *********************/
	/* This is a non-pointer dummy argument array.	(5.1.2.4.2)	*/
	/****************************************************************/

	{   0,	/* Obj_Assum_Shp_Arr		Name_Variable		*/
	  551,	/* Obj_Assum_Shp_Arr		Name_Common_Obj		*/
		/* Dargs can't be in common.	(5.5.2)		 	*/

	  551,	/* Obj_Assum_Shp_Arr		Name_Cri_Pointer	*/
	  551,	/* Obj_Assum_Shp_Arr		Name_Cri_Pointee	*/
		/* Cray extension - illegal to be an array.		*/

	  551,	/* Obj_Assum_Shp_Arr		Name_Cri_Ch_Pointee	*/
		/* Cri character pointee must be a scalar.		*/

	  551,	/* Obj_Assum_Shp_Arr		Name_Func_Result	*/
		/* Dummy args can't be function results.  (14.1.2)	*/

	    0,	/* Obj_Assum_Shp_Arr		Name_Dummy_Arg		*/

	  551,	/* Obj_Assum_Shp_Arr		Name_Module_Proc	*/
		/* 12.3.1 says that a module procedure has an explicit	*/
		/* interface.	12.3.2.1 says that a procedure must not	*/
		/* have more than one explicit specific interface in	*/
		/* a given scoping unit.  12.3 details what is in an	*/
		/* interface.						*/

	  551,	/* Obj_Assum_Shp_Arr		Name_Derived_Type	*/
	  551,	/* Obj_Assum_Shp_Arr		Name_Generic_Interface	*/
	  551,	/* Obj_Assum_Shp_Arr		Name_Namelist_Group	*/
	  551,	/* Obj_Assum_Shp_Arr		Name_Namelist_Group_Obj	*/
		/* 5.4							*/

	  551,	/* Obj_Assum_Shp_Arr		Name_Statement_Func	*/
	  551,	/* Obj_Assum_Shp_Arr		Name_Construct		*/
	  551,	/* Obj_Assum_Shp_Arr		Name_Intrinsic_Func	*/
	  551,	/* Obj_Assum_Shp_Arr		Name_Intrinsic_Subr	*/
	  551,	/* Obj_Assum_Shp_Arr		Name_Module		*/
	  551,	/* Obj_Assum_Shp_Arr		Name_Blockdata		*/
	  551,	/* Obj_Assum_Shp_Arr		Name_Program		*/
	  551,	/* Obj_Assum_Shp_Arr		Name_Function		*/
	  551,	/* Obj_Assum_Shp_Arr		Name_Curr_Func		*/
	  551,	/* Obj_Assum_Shp_Arr		Name_Curr_Subr		*/
	  551,	/* Obj_Assum_Shp_Arr		Name_Internal_Func	*/
	  551	/* Obj_Assum_Shp_Arr		Name_Internal_Subr	*/
		/* 14.1.2	None of these can be dargs.		*/
	},

	/************************ Obj_Co_Array **********************/
	/* This is a pe dimension [3,4]					*/
	/****************************************************************/

	{   0,	/* Obj_Co_Array		Name_Variable		*/
	    0,	/* Obj_Co_Array		Name_Common_Obj		*/
	  551,	/* Obj_Co_Array		Name_Cri_Pointer	*/
	  551,	/* Obj_Co_Array		Name_Cri_Pointee	*/
	  551,	/* Obj_Co_Array		Name_Cri_Ch_Pointee	*/
	  551,	/* Obj_Co_Array		Name_Func_Result	*/
	    0,	/* Obj_Co_Array		Name_Dummy_Arg		*/
	  551,	/* Obj_Co_Array		Name_Module_Proc	*/
	  551,	/* Obj_Co_Array		Name_Derived_Type	*/
	  551,	/* Obj_Co_Array		Name_Generic_Interface	*/
	  551,	/* Obj_Co_Array		Name_Namelist_Group	*/
	    0,	/* Obj_Co_Array		Name_Namelist_Group_Obj	*/
	  551,	/* Obj_Co_Array		Name_Statement_Func	*/
	  551,	/* Obj_Co_Array		Name_Construct		*/
	  551,	/* Obj_Co_Array		Name_Intrinsic_Func	*/
	  551,	/* Obj_Co_Array		Name_Intrinsic_Subr	*/
	  551,	/* Obj_Co_Array		Name_Module		*/
	  551,	/* Obj_Co_Array		Name_Blockdata		*/
	  551,	/* Obj_Co_Array		Name_Program		*/
	  551,	/* Obj_Co_Array		Name_Function		*/
	  551,	/* Obj_Co_Array		Name_Curr_Func		*/
	  551,	/* Obj_Co_Array		Name_Curr_Subr		*/
	  551,	/* Obj_Co_Array		Name_Internal_Func	*/
	  551	/* Obj_Co_Array		Name_Internal_Subr	*/
	},


	/************************ Obj_Allocatable ***********************/
	/* This must be a deferred shape array and must not be a dummy 	*/
	/* argument or function result. (5.2.6)				*/
	/****************************************************************/

	{   0,	/* Obj_Allocatable		Name_Variable		*/
	  551,	/* Obj_Allocatable		Name_Common_Obj		*/
		/* Allocatable can't be in common. (5.5.2 constraint)	*/

	  551,	/* Obj_Allocatable		Name_Cri_Pointer	*/

	  551,	/* Obj_Allocatable		Name_Cri_Pointee	*/
		/* Cray extensions - do not mix with Fortran 90.	*/

	  551,	/* Obj_Allocatable		Name_Cri_Ch_Pointee	*/
		/* Cri character pointee must be a scalar.		*/

#ifdef KEY /* Bug 6845 */
	    0,	/* Obj_Allocatable		Name_Func_Result	*/
	    0,	/* Obj_Allocatable		Name_Dummy_Arg		*/
#else /* KEY Bug 6845 */
	  551,	/* Obj_Allocatable		Name_Func_Result	*/
	  551,	/* Obj_Allocatable		Name_Dummy_Arg		*/
		/* (5.2.6) - illegal					*/
#endif /* KEY Bug 6845 */

	  551,	/* Obj_Allocatable		Name_Module_Proc	*/
		/* Can't be a function result (5.2.6)		 	*/

	  551,	/* Obj_Allocatable		Name_Derived_Type	*/
	  551,	/* Obj_Allocatable		Name_Generic_Interface	*/
	  551,	/* Obj_Allocatable		Name_Namelist_Group	*/
	  551,	/* Obj_Allocatable		Name_Namelist_Group_Obj	*/
		/* 5.4							*/

	  551,	/* Obj_Allocatable		Name_Statement_Func	*/
	  551,	/* Obj_Allocatable		Name_Construct		*/
	  551,	/* Obj_Allocatable		Name_Intrinsic_Func	*/
	  551,	/* Obj_Allocatable		Name_Intrinsic_Subr	*/
	  551,	/* Obj_Allocatable		Name_Module		*/
	  551,	/* Obj_Allocatable		Name_Blockdata		*/
	  551,	/* Obj_Allocatable		Name_Program		*/
	  551,	/* Obj_Allocatable		Name_Function		*/
#ifdef KEY /* Bug 6845 */
	    0,	/* Obj_Allocatable		Name_Curr_Func		*/
#else /* KEY Bug 6845 */
	  551,	/* Obj_Allocatable		Name_Curr_Func		*/
#endif /* KEY Bug 6845 */
	  551, 	/* Obj_Allocatable		Name_Curr_Subr		*/
	  551,	/* Obj_Allocatable		Name_Internal_Func	*/
	  551	/* Obj_Allocatable		Name_Internal_Subr	*/
	},

#ifdef KEY /* Bug 14150 */
	/************************ Obj_Bind        ***********************/
	/****************************************************************/

	{   0,	/* Obj_Bind			Name_Variable		*/
	  551,	/* Obj_Bind			Name_Common_Obj		*/
	  551,	/* Obj_Bind			Name_Cri_Pointer	*/
	  551,	/* Obj_Bind			Name_Cri_Pointee	*/
	  551,	/* Obj_Bind			Name_Cri_Ch_Pointee	*/
	  551,	/* Obj_Bind			Name_Func_Result	*/
	  551,	/* Obj_Bind			Name_Dummy_Arg		*/
	    0,	/* Obj_Bind			Name_Module_Proc	*/
	  551,	/* Obj_Bind			Name_Derived_Type	*/
	  551,	/* Obj_Bind			Name_Generic_Interface	*/
	  551,	/* Obj_Bind			Name_Namelist_Group	*/
	  551,	/* Obj_Bind			Name_Namelist_Group_Obj	*/
	  551,	/* Obj_Bind			Name_Statement_Func	*/
	  551,	/* Obj_Bind			Name_Construct		*/
	  551,	/* Obj_Bind			Name_Intrinsic_Func	*/
	  551,	/* Obj_Bind			Name_Intrinsic_Subr	*/
	  551,	/* Obj_Bind			Name_Module		*/
	  551,	/* Obj_Bind			Name_Blockdata		*/
	  551,	/* Obj_Bind			Name_Program		*/
	  551,	/* Obj_Bind			Name_Function		*/
	    0,	/* Obj_Bind			Name_Curr_Func		*/
	    0, 	/* Obj_Bind			Name_Curr_Subr		*/
	  551,	/* Obj_Bind			Name_Internal_Func	*/
	  551	/* Obj_Bind			Name_Internal_Subr	*/
	},

	/************************ Obj_Value        ***********************/
	/****************************************************************/

	{ 551,	/* Obj_Value			Name_Variable		*/
	  551,	/* Obj_Value			Name_Common_Obj		*/
	  551,	/* Obj_Value			Name_Cri_Pointer	*/
	  551,	/* Obj_Value			Name_Cri_Pointee	*/
	  551,	/* Obj_Value			Name_Cri_Ch_Pointee	*/
	  551,	/* Obj_Value			Name_Func_Result	*/
	    0,	/* Obj_Value			Name_Dummy_Arg		*/
	  551,	/* Obj_Value			Name_Module_Proc	*/
	  551,	/* Obj_Value			Name_Derived_Type	*/
	  551,	/* Obj_Value			Name_Generic_Interface	*/
	  551,	/* Obj_Value			Name_Namelist_Group	*/
	  551,	/* Obj_Value			Name_Namelist_Group_Obj	*/
	  551,	/* Obj_Value			Name_Statement_Func	*/
	  551,	/* Obj_Value			Name_Construct		*/
	  551,	/* Obj_Value			Name_Intrinsic_Func	*/
	  551,	/* Obj_Value			Name_Intrinsic_Subr	*/
	  551,	/* Obj_Value			Name_Module		*/
	  551,	/* Obj_Value			Name_Blockdata		*/
	  551,	/* Obj_Value			Name_Program		*/
	  551,	/* Obj_Value			Name_Function		*/
	  551,	/* Obj_Value			Name_Curr_Func		*/
	  551, 	/* Obj_Value			Name_Curr_Subr		*/
	  551,	/* Obj_Value			Name_Internal_Func	*/
	  551	/* Obj_Value			Name_Internal_Subr	*/
	},
#endif /* KEY Bug 14150 */


	/************************ Obj_Constant **************************/
	/* The named constant must have its type, shape and any type	*/
	/* parameters specified either by a previous occurrence in a	*/
	/* type declaration stmt in the same scoping unit, or by the	*/
	/* implicit typing rules in effect.	(5.2.10)		*/
	/****************************************************************/

	{ 551,	/* Obj_Constant			Name_Variable		*/
	  551,	/* Obj_Constant			Name_Common_Obj		*/
	  551,	/* Obj_Constant			Name_Cri_Pointer	*/
	  551,	/* Obj_Constant			Name_Cri_Pointee	*/
		/* Cray extensions - illegal				*/
	  551,	/* Obj_Constant			Name_Cri_Ch_Pointee	*/
	  551,	/* Obj_Constant			Name_Func_Result	*/
	  551,	/* Obj_Constant			Name_Dummy_Arg		*/
	  551,	/* Obj_Constant			Name_Module_Proc	*/
	  551,	/* Obj_Constant			Name_Derived_Type	*/
	  551,	/* Obj_Constant			Name_Generic_Interface	*/
	  551,	/* Obj_Constant			Name_Namelist_Group	*/
	  551,	/* Obj_Constant			Name_Namelist_Group_Obj	*/
	  551,	/* Obj_Constant			Name_Statement_Func	*/
	  551,	/* Obj_Constant			Name_Construct		*/
	  551,	/* Obj_Constant			Name_Intrinsic_Func	*/
	  551,	/* Obj_Constant			Name_Intrinsic_Subr	*/
	  551,	/* Obj_Constant			Name_Module		*/
	  551,	/* Obj_Constant			Name_Blockdata		*/
	  551,	/* Obj_Constant			Name_Program		*/
	  551,	/* Obj_Constant			Name_Function		*/
	  551,	/* Obj_Constant			Name_Curr_Func		*/
	  551, 	/* Obj_Constant			Name_Curr_Subr		*/
	  551,	/* Obj_Constant			Name_Internal_Func	*/
	  551	/* Obj_Constant			Name_Internal_Subr	*/
		/* 14.1.2 - These can't be constants.		 	*/
	},


	/************************ Obj_Intent ****************************/
	/* An intent statement may only appear in the specification-	*/
	/* part of a subprogram or interface body.  It must not be a	*/
	/* dummy procedure or dummy pointer.	(5.2.1)			*/
	/***************************************************************/

	{ 551,	/* Obj_Intent			Name_Variable		*/
	  551,	/* Obj_Intent			Name_Common_Obj		*/
		/* 5.5.2						*/

	  551,	/* Obj_Intent			Name_Cri_Pointer	*/
	  551,	/* Obj_Intent			Name_Cri_Pointee	*/
	  551,	/* Obj_Intent			Name_Cri_Ch_Pointee	*/

	  551,	/* Obj_Intent			Name_Func_Result	*/
		/* 14.1.2						*/

	    0,	/* Obj_Intent			Name_Dummy_Arg		*/

	  551,	/* Obj_Intent			Name_Module_Proc	*/
	  551,	/* Obj_Intent			Name_Derived_Type	*/
	  551,	/* Obj_Intent			Name_Generic_Interface	*/
	  551,	/* Obj_Intent			Name_Namelist_Group	*/
	    0,	/* Obj_Intent			Name_Namelist_Group_Obj	*/
	  551,	/* Obj_Intent			Name_Statement_Func	*/
	  551,	/* Obj_Intent			Name_Construct		*/
	  551,	/* Obj_Intent			Name_Intrinsic_Func	*/
	  551,	/* Obj_Intent			Name_Intrinsic_Subr	*/
	  551,	/* Obj_Intent			Name_Module		*/
	  551,	/* Obj_Intent			Name_Blockdata		*/
	  551,	/* Obj_Intent			Name_Program		*/
	  551,	/* Obj_Intent			Name_Function		*/
	  551,	/* Obj_Intent			Name_Curr_Func		*/
	  551, 	/* Obj_Intent			Name_Curr_Subr		*/
	  551,	/* Obj_Intent			Name_Internal_Func	*/
	  551	/* Obj_Intent			Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Optional **************************/
	/* The optional statement may only appear in the specification-	*/
	/* part of a subprogram or interface body.	(5.2.2)		*/
	/****************************************************************/

	{  551,	/* Obj_Optional			Name_Variable		*/

	  551,	/* Obj_Optional			Name_Common_Obj		*/
		/* 5.5.2						*/

	    0,	/* Obj_Optional			Name_Cri_Pointer	*/
	  551,	/* Obj_Optional			Name_Cri_Pointee	*/
	  551,	/* Obj_Optional			Name_Cri_Ch_Pointee	*/
	  551,	/* Obj_Optional			Name_Func_Result	*/
		/* 14.1.2 - can't be a darg.				*/

	    0,	/* Obj_Optional			Name_Dummy_Arg		*/

	  551,	/* Obj_Optional			Name_Module_Proc	*/
	  551,	/* Obj_Optional			Name_Derived_Type	*/
	  551,	/* Obj_Optional			Name_Generic_Interface	*/
	  551,	/* Obj_Optional			Name_Namelist_Group	*/
	    0,	/* Obj_Optional			Name_Namelist_Group_Obj	*/
	  551,	/* Obj_Optional			Name_Statement_Func	*/
	  551,	/* Obj_Optional			Name_Construct		*/
	  551,	/* Obj_Optional			Name_Intrinsic_Func	*/
	  551,	/* Obj_Optional			Name_Intrinsic_Subr	*/
	  551,	/* Obj_Optional			Name_Module		*/
	  551,	/* Obj_Optional			Name_Blockdata		*/
	  551,	/* Obj_Optional			Name_Program		*/
	    0,	/* Obj_Optional			Name_Function		*/
	  551,	/* Obj_Optional			Name_Curr_Func		*/
	  551, 	/* Obj_Optional			Name_Curr_Subr		*/
	  551,	/* Obj_Optional			Name_Internal_Func	*/
	  551	/* Obj_Optional			Name_Internal_Subr	*/
		/* None of these can be dargs.				*/
	},


	/************************ Obj_Private ***************************/
	/* The private statement may only be in the scoping unit of a	*/
	/* module.  Each use-name must be a named variable, procedure,  */
	/* derived type, named constant or namelist group.	(5.2.3)	*/
	/* Some of these that are marked legal, will eventually error,	*/
	/* because the item is not allowed in a module.			*/
	/****************************************************************/

	{   0,	/* Obj_Private			Name_Variable		*/
	    0,	/* Obj_Private			Name_Common_Obj		*/
	    0,	/* Obj_Private			Name_Cri_Pointer	*/
	    0,	/* Obj_Private			Name_Cri_Pointee	*/
	    0,	/* Obj_Private			Name_Cri_Ch_Pointee	*/
	  551,	/* Obj_Private			Name_Func_Result	*/
	    0,	/* Obj_Private			Name_Dummy_Arg		*/
	    0,	/* Obj_Private			Name_Module_Proc	*/
	    0,	/* Obj_Private			Name_Derived_Type	*/
	    0,	/* Obj_Private			Name_Generic_Interface	*/
	    0,	/* Obj_Private			Name_Namelist_Group	*/
	    0,	/* Obj_Private			Name_Namelist_Group_Obj	*/
	  551,	/* Obj_Private			Name_Statement_Func	*/
	  551,	/* Obj_Private			Name_Construct		*/
	    0,	/* Obj_Private			Name_Intrinsic_Func	*/
	    0,	/* Obj_Private			Name_Intrinsic_Subr	*/
	  551,	/* Obj_Private			Name_Module		*/
	  551,	/* Obj_Private			Name_Blockdata		*/
	  551,	/* Obj_Private			Name_Program		*/
	    0,	/* Obj_Private			Name_Function		*/
	  551,	/* Obj_Private			Name_Curr_Func		*/
	  551, 	/* Obj_Private			Name_Curr_Subr		*/
	  551,	/* Obj_Private			Name_Internal_Func	*/
	  551	/* Obj_Private			Name_Internal_Subr	*/
	},


	/************************ Obj_Public ****************************/
	/* The public statement may only be in the scoping unit of a	*/
	/* module.  Each use-name must be a named variable, procedure,	*/
	/* derived type, named constant or namelist group.	(5.2.3)	*/
	/* Some of these that are marked legal, will eventually error,	*/
	/* because the item is not allowed in a module.			*/
	/****************************************************************/

	{   0,	/* Obj_Public			Name_Variable		*/
	    0,	/* Obj_Public			Name_Common_Obj		*/
	    0,	/* Obj_Public			Name_Cri_Pointer	*/
	    0,	/* Obj_Public			Name_Cri_Pointee	*/
	    0,	/* Obj_Public			Name_Cri_Ch_Pointee	*/
	  551,	/* Obj_Public			Name_Func_Result	*/
	    0,	/* Obj_Public			Name_Dummy_Arg		*/
	    0,	/* Obj_Public			Name_Module_Proc	*/
	    0,	/* Obj_Public			Name_Derived_Type	*/
	    0,	/* Obj_Public			Name_Generic_Interface	*/
	    0,	/* Obj_Public			Name_Namelist_Group	*/
	    0,	/* Obj_Public			Name_Namelist_Group_Obj	*/
	  551,	/* Obj_Public			Name_Statement_Func	*/
	  551,	/* Obj_Public			Name_Construct		*/
	    0,	/* Obj_Public			Name_Intrinsic_Func	*/
	    0,	/* Obj_Public			Name_Intrinsic_Subr	*/
	  551,	/* Obj_Public			Name_Module		*/
	  551,	/* Obj_Public			Name_Blockdata		*/
	  551,	/* Obj_Public			Name_Program		*/
	    0,	/* Obj_Public			Name_Function		*/
	  551,	/* Obj_Public			Name_Curr_Func		*/
	  551, 	/* Obj_Public			Name_Curr_Subr		*/
	  551,	/* Obj_Public			Name_Internal_Func	*/
	  551	/* Obj_Public			Name_Internal_Subr	*/
	},


	/************************ Obj_Target ****************************/
	/* This specifies a list of object names that may have	 	*/
	/* pointers associated with them.				*/
	/****************************************************************/

	{   0,	/* Obj_Target			Name_Variable		*/
	    0,	/* Obj_Target			Name_Common_Obj		*/
	  551,	/* Obj_Target			Name_Cri_Pointer	*/
	  551,	/* Obj_Target			Name_Cri_Pointee	*/
	  551,	/* Obj_Target			Name_Cri_Ch_Pointee	*/
		/* Cray extensions - illegal				*/

	    0,	/* Obj_Target			Name_Func_Result	*/
	    0,	/* Obj_Target			Name_Dummy_Arg		*/

	  551,	/* Obj_Target			Name_Module_Proc	*/
		/* 12.3.1 says that a module procedure has an explicit	*/
		/* interface.	12.3.2.1 says that a procedure must not	*/
		/* have more than one explicit specific interface in	*/
		/* a given scoping unit.  12.3 details what is in an	*/
		/* interface.					 	*/

	  551,	/* Obj_Target			Name_Derived_Type	*/
	  551,	/* Obj_Target			Name_Generic_Interface	*/
	  551,	/* Obj_Target			Name_Namelist_Group	*/
	    0,	/* Obj_Target			Name_Namelist_Group_Obj	*/
	  551,	/* Obj_Target			Name_Statement_Func	*/
	  551,	/* Obj_Target			Name_Construct		*/
	  551,	/* Obj_Target			Name_Intrinsic_Func	*/
	  551,	/* Obj_Target			Name_Intrinsic_Subr	*/
	  551,	/* Obj_Target			Name_Module		*/
	  551,	/* Obj_Target			Name_Blockdata		*/
	  551,	/* Obj_Target			Name_Program		*/
	    0,	/* Obj_Target			Name_Function		*/
	    0,	/* Obj_Target			Name_Curr_Func		*/
	  551, 	/* Obj_Target			Name_Curr_Subr		*/
	  551,	/* Obj_Target			Name_Internal_Func	*/
	  551	/* Obj_Target			Name_Internal_Subr	*/
		/* 14.1.2 - None of these can be variables.		*/
	},

	/************************ Obj_Equiv *****************************/
	/* An equivalence object must not be a dummy argument, a	*/
	/* pointer, an allocatable array, and object of a nonsequenced	*/
	/* derived type or of a sequence derived type containing a	*/
	/* pointer at any level of component selection, an automatic	*/
	/* object, a function name, an entry name, a result name, a	*/
	/* named constant, a structure component, or a subobject of any	*/
	/* of the preceeding objects. (5.5.1 constraint)		*/
	/****************************************************************/

	{   0,	/* Obj_Equiv			Name_Variable		*/
	    0,	/* Obj_Equiv			Name_Common_Obj		*/
	    0,	/* Obj_Equiv			Name_Cri_Pointer	*/
	  551,	/* Obj_Equiv			Name_Cri_Pointee	*/
		/* Cray extensions					*/
	  551,	/* Obj_Equiv			Name_Cri_Ch_Pointee	*/

	  551,	/* Obj_Equiv			Name_Func_Result	*/
	  551,	/* Obj_Equiv			Name_Dummy_Arg		*/
	  551,	/* Obj_Equiv			Name_Module_Proc	*/
	  551,	/* Obj_Equiv			Name_Derived_Type	*/
	  551,	/* Obj_Equiv			Name_Generic_Interface	*/
	  551,	/* Obj_Equiv			Name_Namelist_Group	*/
	    0,	/* Obj_Equiv			Name_Namelist_Group_Obj	*/
	  551,	/* Obj_Equiv			Name_Statement_Func	*/
	  551,	/* Obj_Equiv			Name_Construct		*/
	  551,	/* Obj_Equiv			Name_Intrinsic_Func	*/
	  551,	/* Obj_Equiv			Name_Intrinsic_Subr	*/
	  551,	/* Obj_Equiv			Name_Module		*/
	  551,	/* Obj_Equiv			Name_Blockdata		*/
	  551,	/* Obj_Equiv			Name_Program		*/
	  551,	/* Obj_Equiv			Name_Function		*/
	  551,	/* Obj_Equiv			Name_Curr_Func		*/
	  551, 	/* Obj_Equiv			Name_Curr_Subr		*/
	  551,	/* Obj_Equiv			Name_Internal_Func	*/
	  551	/* Obj_Equiv			Name_Internal_Subr	*/
		/* 14.1.2 - None of these can be variables.		*/
	},


	/************************ Obj_Save ******************************/
	/* An object-name must not be a dummy argument name, a	 	*/
	/* procedure name, a function result name, an automatic data	*/
	/* object, or the name of an entity in a common block. (5.2.4)  */
	/* 2.4.3.1 - An object is a variable or a constant.		*/
	/****************************************************************/

	{   0,	/* Obj_Saved			Name_Variable		*/
	  551,	/* Obj_Saved			Name_Common_Obj		*/
	    0,	/* Obj_Saved			Name_Cri_Pointer	*/
	  551,	/* Obj_Saved			Name_Cri_Pointee	*/
	  551,	/* Obj_Saved			Name_Cri_Ch_Pointee	*/
		/* Cray extensions					*/

	  551,	/* Obj_Saved			Name_Func_Result	*/
	  551,	/* Obj_Saved			Name_Dummy_Arg		*/
	  551,	/* Obj_Saved			Name_Module_Proc	*/
	  551,	/* Obj_Saved			Name_Derived_Type	*/
	  551,	/* Obj_Saved			Name_Generic_Interface	*/
	  551,	/* Obj_Saved			Name_Namelist_Group	*/
	    0,	/* Obj_Saved			Name_Namelist_Group_Obj	*/
	  551,	/* Obj_Saved			Name_Statement_Func	*/
	  551,	/* Obj_Saved			Name_Construct		*/
	  551,	/* Obj_Saved			Name_Intrinsic_Func	*/
	  551,	/* Obj_Saved			Name_Intrinsic_Subr	*/
	  551,	/* Obj_Saved			Name_Module		*/
	  551,	/* Obj_Saved			Name_Blockdata		*/
	  551,	/* Obj_Saved			Name_Program		*/
	  551,	/* Obj_Saved			Name_Function		*/
	  551,	/* Obj_Saved			Name_Curr_Func		*/
	  551,	/* Obj_Saved			Name_Curr_Subr		*/
	  551,	/* Obj_Saved			Name_Internal_Func	*/
	  551	/* Obj_Saved			Name_Internal_Subr	*/
		/* None of these can be variables or constants. 14.1.2	*/
	},


	/************************ Obj_Automatic *************************/
	/****************************************************************/

	{   0,	/* Obj_Automatic		Name_Variable		*/
	  551,	/* Obj_Automatic		Name_Common_Obj		*/
	    0,	/* Obj_Automatic		Name_Cri_Pointer	*/
	  551,	/* Obj_Automatic		Name_Cri_Pointee	*/
	  551,	/* Obj_Automatic		Name_Cri_Ch_Pointee	*/
	    0,	/* Obj_Automatic		Name_Func_Result	*/
	  551,	/* Obj_Automatic		Name_Dummy_Arg		*/
	    0,	/* Obj_Automatic		Name_Module_Proc	*/
	  551,	/* Obj_Automatic		Name_Derived_Type	*/
	  551,	/* Obj_Automatic		Name_Generic_Interface	*/
	  551,	/* Obj_Automatic		Name_Namelist_Group	*/
	    0,	/* Obj_Automatic		Name_Namelist_Group_Obj	*/
	  551,	/* Obj_Automatic		Name_Statement_Func	*/
	  551,	/* Obj_Automatic		Name_Construct		*/
	  551,	/* Obj_Automatic		Name_Intrinsic_Func	*/
	  551,	/* Obj_Automatic		Name_Intrinsic_Subr	*/
	  551,	/* Obj_Automatic		Name_Module		*/
	  551,	/* Obj_Automatic		Name_Blockdata		*/
	  551,	/* Obj_Automatic		Name_Program		*/
	    0,	/* Obj_Automatic		Name_Function		*/
	    0,	/* Obj_Automatic		Name_Curr_Func		*/
	  551,	/* Obj_Automatic		Name_Curr_Subr		*/
	    0,	/* Obj_Automatic		Name_Internal_Func	*/
	  551	/* Obj_Automatic		Name_Internal_Subr	*/
	},

	/************************ Obj_Pointer ***************************/
	/* Cannot have the INTENT or PARAMETER attributes.  If it is an	*/
	/* array, it must be a deferred shape one.  (5.2.7 constraints)	*/
	/* Cannot be ALLOCATABLE, EXTERNAL, TARGET, or INTRINSIC. (5.1)	*/
	/****************************************************************/

	{   0,	/* Obj_Pointer			Name_Variable		*/
	    0,	/* Obj_Pointer			Name_Common_Obj		*/
	  551,	/* Obj_Pointer			Name_Cri_Pointer	*/
	  551,	/* Obj_Pointer			Name_Cri_Pointee	*/
	  551,	/* Obj_Pointer			Name_Cri_Ch_Pointee	*/
		/* Cray extensions - illegal				*/

	    0,	/* Obj_Pointer			Name_Func_Result	*/
	    0,	/* Obj_Pointer			Name_Dummy_Arg		*/

	  551,	/* Obj_Pointer			Name_Module_Proc	*/
		/* 12.3.1 says that a module procedure has an explicit	*/
		/* interface.	12.3.2.1 says that a procedure must not	*/
		/* have more than one explicit specific interface in	*/
		/* a given scoping unit.  12.3 details what is in an	*/
		/* interface.						*/

	  551,	/* Obj_Pointer			Name_Derived_Type	*/
	  551,	/* Obj_Pointer			Name_Generic_Interface	*/
	  551,	/* Obj_Pointer			Name_Namelist_Group	*/
	  551,	/* Obj_Pointer			Name_Namelist_Group_Obj	*/
		/* 5.4							*/

	  551,	/* Obj_Pointer			Name_Statement_Func	*/
	  551,	/* Obj_Pointer			Name_Construct		*/
	  551,	/* Obj_Pointer			Name_Intrinsic_Func	*/
	  551,	/* Obj_Pointer			Name_Intrinsic_Subr	*/
	  551,	/* Obj_Pointer			Name_Module		*/
	  551,	/* Obj_Pointer			Name_Blockdata		*/
	  551,	/* Obj_Pointer			Name_Program		*/
		/* 14.1.2						*/

	    0,	/* Obj_Pointer			Name_Function		*/
	    0,	/* Obj_Pointer			Name_Curr_Func		*/

	  551,	/* Obj_Pointer			Name_Curr_Subr		*/
	  551,	/* Obj_Pointer			Name_Internal_Func	*/
	  551	/* Obj_Pointer			Name_Internal_Subr	*/
		/* Must use an explicit interface to make a pointer.	*/
	},


	/************************ Obj_Dcl_Extern ************************/
	/* External-name must be the name of an external procedure, a	*/
	/* darg, or a block data program unit.			 	*/
	/****************************************************************/

	{ 551,	/* Obj_Dcl_Extern		Name_Variable		*/
	  551,	/* Obj_Dcl_Extern		Name_Common_Obj		*/
	  551,	/* Obj_Dcl_Extern		Name_Cri_Pointer	*/
	  551,	/* Obj_Dcl_Extern		Name_Cri_Pointee	*/
	  551,	/* Obj_Dcl_Extern		Name_Cri_Ch_Pointee	*/
		/* Cray extensions - illegal				*/

	  551,	/* Obj_Dcl_Extern		Name_Func_Result	*/

	    0,	/* Obj_Dcl_Extern		Name_Dummy_Arg		*/

	  551,	/* Obj_Dcl_Extern		Name_Module_Proc	*/
	  551,	/* Obj_Dcl_Extern		Name_Derived_Type	*/
	  551,	/* Obj_Dcl_Extern		Name_Generic_Interface	*/

	  551,	/* Obj_Dcl_Extern		Name_Namelist_Group_Obj	*/
	  551,	/* Obj_Dcl_Extern		Name_Namelist_Group	*/
		/* 5.4							*/

	  551,	/* Obj_Dcl_Extern		Name_Statement_Func	*/
	  551,	/* Obj_Dcl_Extern		Name_Construct		*/
	  551,	/* Obj_Dcl_Extern		Name_Intrinsic_Func	*/
	  551,	/* Obj_Dcl_Extern		Name_Intrinsic_Subr	*/
	  551,	/* Obj_Dcl_Extern		Name_Module		*/
		/* 14.1.2						*/

	    0,	/* Obj_Dcl_Extern		Name_Blockdata		*/
		/* 12.3.2.2						*/

	  551,	/* Obj_Dcl_Extern		Name_Program		*/
	    0,	/* Obj_Dcl_Extern		Name_Function		*/
	  551,	/* Obj_Dcl_Extern		Name_Curr_Func		*/
		/* 14.1.2						*/

	    0, 	/* Obj_Dcl_Extern		Name_Curr_Subr		*/
		/* Cray extension					*/

	  551,	/* Obj_Dcl_Extern		Name_Internal_Func	*/
	  551	/* Obj_Dcl_Extern		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Dcl_Intrin ************************/
	/* Intrinsic-name must be the name of an intrinsic procedure.	*/
	/****************************************************************/

	{ 551,	/* Obj_Dcl_Intrin		Name_Variable		*/
	  551,	/* Obj_Dcl_Intrin		Name_Common_Obj		*/
		/* 14.1.2						*/

	  551,	/* Obj_Dcl_Intrin		Name_Cri_Pointer	*/
	  551,	/* Obj_Dcl_Intrin		Name_Cri_Pointee	*/
	  551,	/* Obj_Dcl_Intrin		Name_Cri_Ch_Pointee	*/
	  551,	/* Obj_Dcl_Intrin		Name_Func_Result	*/
	  551,	/* Obj_Dcl_Intrin		Name_Dummy_Arg		*/
	  551,	/* Obj_Dcl_Intrin		Name_Module_Proc	*/
	  551,	/* Obj_Dcl_Intrin		Name_Derived_Type	*/
	    0,	/* Obj_Dcl_Intrin		Name_Generic_Interface	*/
	  551,	/* Obj_Dcl_Intrin		Name_Namelist_Group	*/
	  551,	/* Obj_Dcl_Intrin		Name_Namelist_Group_Obj	*/
		/* 5.4							*/

	  551,	/* Obj_Dcl_Intrin		Name_Statement_Func	*/
	  551,	/* Obj_Dcl_Intrin		Name_Construct		*/
		/* 14.1.2						*/

	    0,	/* Obj_Dcl_Intrin		Name_Intrinsic_Func	*/
	    0,	/* Obj_Dcl_Intrin		Name_Intrinsic_Subr	*/

	  551,	/* Obj_Dcl_Intrin		Name_Module		*/
	  551,	/* Obj_Dcl_Intrin		Name_Blockdata		*/
	  551,	/* Obj_Dcl_Intrin		Name_Program		*/
	    0,	/* Obj_Dcl_Intrin		Name_Function		*/
	  551,	/* Obj_Dcl_Intrin		Name_Curr_Func		*/
	  551,	/* Obj_Dcl_Intrin		Name_Curr_Subr		*/
	  551,	/* Obj_Dcl_Intrin		Name_Internal_Func	*/
	  551	/* Obj_Dcl_Intrin		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Data_Init *************************/
	/* The = initialization-expr must not appear if object-name is	*/
	/* a darg, a function result, an object in a named common block	*/
	/* unless the type declaration is in a block data program unit,	*/
	/* an object in blank common, an allocatable array, a pointer, 	*/
	/* an external name, an intrinsic name, or an automatic object.	*/
	/* (5.1 constraint)						*/
	/****************************************************************/

	{   0,	/* Obj_Data_Init		Name_Variable		*/
	    0,	/* Obj_Data_Init		Name_Common_Obj		*/
	    0,	/* Obj_Data_Init		Name_Cri_Pointer	*/
	  551,	/* Obj_Data_Init		Name_Cri_Pointee	*/
	  551,	/* Obj_Data_Init		Name_Cri_Ch_Pointee	*/
		/* Cray extensions					*/

	  551,	/* Obj_Data_Init		Name_Func_Result	*/
	  551,	/* Obj_Data_Init		Name_Dummy_Arg		*/
	  551,	/* Obj_Data_Init		Name_Module_Proc	*/
	  551,	/* Obj_Data_Init		Name_Derived_Type	*/
	  551,	/* Obj_Data_Init		Name_Generic_Interface	*/
	  551,	/* Obj_Data_Init		Name_Namelist_Group	*/
	    0,	/* Obj_Data_Init		Name_Namelist_Group_Obj	*/
	  551,	/* Obj_Data_Init		Name_Statement_Func	*/
	  551,	/* Obj_Data_Init		Name_Construct		*/
	  551,	/* Obj_Data_Init		Name_Intrinsic_Func	*/
	  551,	/* Obj_Data_Init		Name_Intrinsic_Subr	*/
	  551,	/* Obj_Data_Init		Name_Module		*/
	  551,	/* Obj_Data_Init		Name_Blockdata		*/
	  551,	/* Obj_Data_Init		Name_Program		*/
	  551,	/* Obj_Data_Init		Name_Function		*/
	  551,	/* Obj_Data_Init		Name_Curr_Func		*/
	  551,	/* Obj_Data_Init		Name_Curr_Subr		*/
	  551,	/* Obj_Data_Init		Name_Internal_Func	*/
	  551	/* Obj_Data_Init		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Typed *****************************/
	/* This must be an object (which is a constant or a variable)	*/
	/* (2.4.3.1) or an external function, an intrinsic function, a	*/
	/* function dummy procedure or a statement function.		*/
	/* Actually you can type an internal or module function also,	*/
	/* but strict adherence to the standard forces this typing to	*/
	/* be on the FUNCTION statement, not the type declaration stmt	*/
	/* (These would show up as Name_Function	anyway.)	*/
	/****************************************************************/

	{   0,	/* Obj_Typed			Name_Variable		*/
	    0,	/* Obj_Typed			Name_Common_Obj		*/
	  551,	/* Obj_Typed			Name_Cri_Pointer	*/
		/* Cray extension - This is a type.			*/

	    0,	/* Obj_Typed			Name_Cri_Pointee	*/
	  551,	/* Obj_Typed			Name_Cri_Ch_Pointee	*/
	    0,	/* Obj_Typed			Name_Func_Result	*/
	    0,	/* Obj_Typed			Name_Dummy_Arg		*/

	  551,	/* Obj_Typed			Name_Module_Proc	*/
		/* 12.3.1 says that a module procedure has an explicit	*/
		/* interface.	12.3.2.1 says that a procedure must not	*/
		/* have more than one explicit specific interface in	*/
		/* a given scoping unit.  12.3 details what is in an	*/
		/* interface.					 	*/

	  551,	/* Obj_Typed			Name_Derived_Type	*/
	    0,	/* Obj_Typed			Name_Generic_Interface	*/
	  551,	/* Obj_Typed			Name_Namelist_Group	*/
	    0,	/* Obj_Typed			Name_Namelist_Group_Obj	*/
	  551,	/* Obj_Typed			Name_Statement_Func	*/
	  551,	/* Obj_Typed			Name_Construct		*/
		/* 14.1.2 - Not listed in 2.4.3.1			*/

	    0,	/* Obj_Typed			Name_Intrinsic_Func	*/
	  551,	/* Obj_Typed			Name_Intrinsic_Subr	*/
		/* 2.4.3.1						*/

	  551,	/* Obj_Typed			Name_Module		*/
	  551,	/* Obj_Typed			Name_Blockdata		*/
	  551,	/* Obj_Typed			Name_Program		*/
		/* 14.1.2						*/

	    0,	/* Obj_Typed			Name_Function		*/
	    0,	/* Obj_Typed			Name_Curr_Func		*/

	  551,	/* Obj_Typed			Name_Curr_Subr		*/
	  551,	/* Obj_Typed			Name_Internal_Func	*/
	  551	/* Obj_Typed			Name_Internal_Subr	*/
		/* 14.1.2						*/
	},

	/************************ Obj_Volatile  *************************/
	/* This applies to all variables.                               */
	/****************************************************************/

	{   0,	/* Obj_Volatile			Name_Variable		*/
	    0,	/* Obj_Volatile			Name_Common_Obj		*/
	    0,	/* Obj_Volatile			Name_Cri_Pointer	*/
	    0,	/* Obj_Volatile			Name_Cri_Pointee	*/
	    0,	/* Obj_Volatile			Name_Cri_Ch_Pointee	*/
	  551,	/* Obj_Volatile			Name_Func_Result	*/
	    0,	/* Obj_Volatile			Name_Dummy_Arg		*/
	  551,	/* Obj_Volatile			Name_Module_Proc	*/
	  551,	/* Obj_Volatile			Name_Derived_Type	*/
	  551,	/* Obj_Volatile			Name_Generic_Interface	*/
	  551,	/* Obj_Volatile			Name_Namelist_Group	*/
	    0,	/* Obj_Volatile			Name_Namelist_Group_Obj	*/
	  551,	/* Obj_Volatile			Name_Statement_Func	*/
	  551,	/* Obj_Volatile			Name_Construct		*/
	  551,	/* Obj_Volatile			Name_Intrinsic_Func	*/
	  551,	/* Obj_Volatile			Name_Intrinsic_Subr	*/
	  551,	/* Obj_Volatile			Name_Module		*/
	  551,	/* Obj_Volatile			Name_Blockdata		*/
	  551,	/* Obj_Volatile			Name_Program		*/
	  551,	/* Obj_Volatile			Name_Function		*/
	  551,	/* Obj_Volatile			Name_Curr_Func		*/
	  551,	/* Obj_Volatile			Name_Curr_Subr		*/
	  551,	/* Obj_Volatile			Name_Internal_Func	*/
	  551	/* Obj_Volatile			Name_Internal_Subr	*/
	},


	/********************* Obj_Copy_Assumed_Shape *******************/
	/* This is a non-pointer dummy argument array.	(5.1.2.4.2)	*/
	/****************************************************************/

	{   0,	/* Obj_Copy_Assumed_Shape	Name_Variable		*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Common_Obj		*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Cri_Pointer	*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Cri_Pointee	*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Cri_Ch_Pointee	*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Func_Result	*/
	    0,	/* Obj_Copy_Assumed_Shape	Name_Dummy_Arg		*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Module_Proc	*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Derived_Type	*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Generic_Interface	*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Namelist_Group	*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Namelist_Group_Obj	*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Statement_Func	*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Construct		*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Intrinsic_Func	*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Intrinsic_Subr	*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Module		*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Blockdata		*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Program		*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Function		*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Curr_Func		*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Curr_Subr		*/
	 1442,	/* Obj_Copy_Assumed_Shape	Name_Internal_Func	*/
	 1442 	/* Obj_Copy_Assumed_Shape	Name_Internal_Subr	*/
	},


	/************************ Obj_Auxiliary *************************/
	/* Cray extension - Rules come from the CFT77 manual.		*/
	/* This must be a non-character array.	It can be a dummy arg,  */
	/* but cannot be a function result.	It should not be mixed	*/
	/* with Fortran 90.						*/
	/****************************************************************/

	{   0,	/* Obj_Auxiliary		Name_Variable		*/
	    0,	/* Obj_Auxiliary		Name_Common_Obj		*/
	 1442,	/* Obj_Auxiliary		Name_Cri_Pointer	*/
	 1442,	/* Obj_Auxiliary		Name_Cri_Pointee	*/
	 1442,	/* Obj_Auxiliary		Name_Cri_Ch_Pointee	*/
	 1442,	/* Obj_Auxiliary		Name_Func_Result	*/
	    0,	/* Obj_Auxiliary		Name_Dummy_Arg		*/
	 1442,	/* Obj_Auxiliary		Name_Module_Proc	*/
	 1442,	/* Obj_Auxiliary		Name_Derived_Type	*/
	 1442,	/* Obj_Auxiliary		Name_Generic_Interface	*/
	 1442,	/* Obj_Auxiliary		Name_Namelist_Group	*/
	 1442,	/* Obj_Auxiliary		Name_Namelist_Group_Obj	*/
	 1442,	/* Obj_Auxiliary		Name_Statement_Func	*/
	 1442,	/* Obj_Auxiliary		Name_Construct		*/
	 1442,	/* Obj_Auxiliary		Name_Intrinsic_Func	*/
	 1442,	/* Obj_Auxiliary		Name_Intrinsic_Subr	*/
	 1442,	/* Obj_Auxiliary		Name_Module		*/
	 1442,	/* Obj_Auxiliary		Name_Blockdata		*/
	 1442,	/* Obj_Auxiliary		Name_Program		*/
	 1442,	/* Obj_Auxiliary		Name_Function		*/
	 1442,	/* Obj_Auxiliary		Name_Curr_Func		*/
	 1442, 	/* Obj_Auxiliary		Name_Curr_Subr		*/
	 1442,	/* Obj_Auxiliary		Name_Internal_Func	*/
	 1442	/* Obj_Auxiliary		Name_Internal_Subr	*/
	},


	/************************ Obj_Vfunction *************************/
	/* This is a Cray extension.  It must be an external function,	*/
	/* but cannot be declared EXTERNAL.(From cft77 documentation)	*/
	/****************************************************************/

	{1442,	/* Obj_Vfunction		Name_Variable		*/
	 1442,	/* Obj_Vfunction		Name_Common_Obj		*/
	 1442,	/* Obj_Vfunction		Name_Cri_Pointer	*/
	 1442,	/* Obj_Vfunction		Name_Cri_Pointee	*/
	 1442,	/* Obj_Vfunction		Name_Cri_Ch_Pointee	*/
	 1442,	/* Obj_Vfunction		Name_Func_Result	*/
	 1442,	/* Obj_Vfunction		Name_Dummy_Arg		*/
	 1442,	/* Obj_Vfunction		Name_Module_Proc	*/
	 1442,	/* Obj_Vfunction		Name_Derived_Type	*/
	 1442,	/* Obj_Vfunction		Name_Generic_Interface	*/
	 1442,	/* Obj_Vfunction		Name_Namelist_Group	*/
	 1442,	/* Obj_Vfunction		Name_Namelist_Group_Obj	*/
	 1442,	/* Obj_Vfunction		Name_Statement_Func	*/
	 1442,	/* Obj_Vfunction		Name_Construct		*/
	 1442,	/* Obj_Vfunction		Name_Intrinsic_Func	*/
	 1442,	/* Obj_Vfunction		Name_Intrinsic_Subr	*/
	 1442,	/* Obj_Vfunction		Name_Module		*/
	 1442,	/* Obj_Vfunction		Name_Blockdata		*/
	 1442,	/* Obj_Vfunction		Name_Program		*/
	    0,	/* Obj_Vfunction		Name_Function		*/
	 1442,	/* Obj_Vfunction		Name_Curr_Func		*/
	 1442,	/* Obj_Vfunction		Name_Curr_Subr		*/
	 1442,	/* Obj_Vfunction		Name_Internal_Func	*/
	 1442	/* Obj_Vfunction		Name_Internal_Subr	*/
	},


	/************************ Obj_No_Side_Effects *******************/
	/* This is a Cray extension.	It must be an external function,*/
	/* and must not be a dummy procedure.	(From cft77 doc)	*/
	/****************************************************************/

	{1442,	/* Obj_No_Side_Effects		Name_Variable		*/
	 1442,	/* Obj_No_Side_Effects		Name_Common_Obj		*/
	 1442,	/* Obj_No_Side_Effects		Name_Cri_Pointer	*/
	 1442,	/* Obj_No_Side_Effects		Name_Cri_Pointee	*/
	 1442,	/* Obj_No_Side_Effects		Name_Cri_Ch_Pointee	*/
	 1442,	/* Obj_No_Side_Effects		Name_Func_Result	*/
	 1442,	/* Obj_No_Side_Effects		Name_Dummy_Arg		*/
	 1442,	/* Obj_No_Side_Effects		Name_Module_Proc	*/
	 1442,	/* Obj_No_Side_Effects		Name_Derived_Type	*/
	 1442,	/* Obj_No_Side_Effects		Name_Generic_Interface	*/
	 1442,	/* Obj_No_Side_Effects		Name_Namelist_Group	*/
	 1442,	/* Obj_No_Side_Effects		Name_Namelist_Group_Obj	*/
	 1442,	/* Obj_No_Side_Effects		Name_Statement_Func	*/
	 1442,	/* Obj_No_Side_Effects		Name_Construct		*/
	 1442,	/* Obj_No_Side_Effects		Name_Intrinsic_Func	*/
	 1442,	/* Obj_No_Side_Effects		Name_Intrinsic_Subr	*/
	 1442,	/* Obj_No_Side_Effects		Name_Module		*/
	 1442,	/* Obj_No_Side_Effects		Name_Blockdata		*/
	 1442,	/* Obj_No_Side_Effects		Name_Program		*/
	    0,	/* Obj_No_Side_Effects		Name_Function		*/
	    0,	/* Obj_No_Side_Effects		Name_Curr_Func		*/
	    0,	/* Obj_No_Side_Effects		Name_Curr_Subr		*/
	 1442,	/* Obj_No_Side_Effects		Name_Internal_Func	*/
	 1442	/* Obj_No_Side_Effects		Name_Internal_Subr	*/
	},

	/************************** Obj_Symmetric ***********************/
	/* This is a Cray extension.  It must be a local stack variable	*/
	/****************************************************************/

	{   0,	/* Obj_Symmetric		Name_Variable		*/
	 1442,	/* Obj_Symmetric		Name_Common_Obj		*/
	    0,	/* Obj_Symmetric		Name_Cri_Pointer	*/
	 1442,	/* Obj_Symmetric		Name_Cri_Pointee	*/
	 1442,	/* Obj_Symmetric		Name_Cri_Ch_Pointee	*/
	 1442,	/* Obj_Symmetric		Name_Func_Result	*/
	 1442,	/* Obj_Symmetric		Name_Dummy_Arg		*/
	 1442,	/* Obj_Symmetric		Name_Module_Proc	*/
	 1442,	/* Obj_Symmetric		Name_Derived_Type	*/
	 1442,	/* Obj_Symmetric		Name_Generic_Interface	*/
	 1442,	/* Obj_Symmetric		Name_Namelist_Group	*/
	    0,	/* Obj_Symmetric		Name_Namelist_Group_Obj	*/
	 1442,	/* Obj_Symmetric		Name_Statement_Func	*/
	 1442,	/* Obj_Symmetric		Name_Construct		*/
	 1442,	/* Obj_Symmetric		Name_Intrinsic_Func	*/
	 1442,	/* Obj_Symmetric		Name_Intrinsic_Subr	*/
	 1442,	/* Obj_Symmetric		Name_Module		*/
	 1442,	/* Obj_Symmetric		Name_Blockdata		*/
	 1442,	/* Obj_Symmetric		Name_Program		*/
	 1442,	/* Obj_Symmetric		Name_Function		*/
	 1442,	/* Obj_Symmetric		Name_Curr_Func		*/
	 1442,	/* Obj_Symmetric		Name_Curr_Subr		*/
	 1442,	/* Obj_Symmetric		Name_Internal_Func	*/
	 1442	/* Obj_Symmetric		Name_Internal_Subr	*/
	},


	/**************************** Obj_Inline ************************/
	/* This is a Cray extension.  It must be an external function,  */
	/****************************************************************/

	{1442,	/* Obj_Inline			Name_Variable		*/
	 1442,	/* Obj_Inline			Name_Common_Obj		*/
	 1442,	/* Obj_Inline			Name_Cri_Pointer	*/
	 1442,	/* Obj_Inline			Name_Cri_Pointee	*/
	 1442,	/* Obj_Inline			Name_Cri_Ch_Pointee	*/
	 1442,	/* Obj_Inline			Name_Func_Result	*/
	 1442,	/* Obj_Inline			Name_Dummy_Arg		*/
	    0,	/* Obj_Inline			Name_Module_Proc	*/
	 1442,	/* Obj_Inline			Name_Derived_Type	*/
	    0,	/* Obj_Inline			Name_Generic_Interface	*/
	 1442,	/* Obj_Inline			Name_Namelist_Group	*/
	 1442,	/* Obj_Inline			Name_Namelist_Group_Obj	*/
	 1442,	/* Obj_Inline			Name_Statement_Func	*/
	 1442,	/* Obj_Inline			Name_Construct		*/
	    0,	/* Obj_Inline			Name_Intrinsic_Func	*/
	    0,	/* Obj_Inline			Name_Intrinsic_Subr	*/
	 1442,	/* Obj_Inline			Name_Module		*/
	 1442,	/* Obj_Inline			Name_Blockdata		*/
	 1442,	/* Obj_Inline			Name_Program		*/
	    0,	/* Obj_Inline			Name_Function		*/
	    0,	/* Obj_Inline			Name_Curr_Func		*/
	    0,	/* Obj_Inline			Name_Curr_Subr		*/
	    0,	/* Obj_Inline			Name_Internal_Func	*/
	    0	/* Obj_Inline			Name_Internal_Subr	*/
	},


	/***************************** Obj_Ipa **************************/
	/* This is an extension.  It must be an external function,      */
	/****************************************************************/

	{1442,	/* Obj_Ipa			Name_Variable		*/
	 1442,	/* Obj_Ipa			Name_Common_Obj		*/
	 1442,	/* Obj_Ipa			Name_Cri_Pointer	*/
	 1442,	/* Obj_Ipa			Name_Cri_Pointee	*/
	 1442,	/* Obj_Ipa			Name_Cri_Ch_Pointee	*/
	 1442,	/* Obj_Ipa			Name_Func_Result	*/
	 1442,	/* Obj_Ipa			Name_Dummy_Arg		*/
	    0,	/* Obj_Ipa			Name_Module_Proc	*/
	 1442,	/* Obj_Ipa			Name_Derived_Type	*/
	    0,	/* Obj_Ipa			Name_Generic_Interface	*/
	 1442,	/* Obj_Ipa			Name_Namelist_Group	*/
	 1442,	/* Obj_Ipa			Name_Namelist_Group_Obj	*/
	 1442,	/* Obj_Ipa			Name_Statement_Func	*/
	 1442,	/* Obj_Ipa			Name_Construct		*/
	    0,	/* Obj_Ipa			Name_Intrinsic_Func	*/
	    0,	/* Obj_Ipa			Name_Intrinsic_Subr	*/
	 1442,	/* Obj_Ipa			Name_Module		*/
	 1442,	/* Obj_Ipa			Name_Blockdata		*/
	 1442,	/* Obj_Ipa			Name_Program		*/
	    0,	/* Obj_Ipa			Name_Function		*/
	    0,	/* Obj_Ipa			Name_Curr_Func		*/
	    0,	/* Obj_Ipa			Name_Curr_Subr		*/
	    0,	/* Obj_Ipa			Name_Internal_Func	*/
	    0	/* Obj_Ipa			Name_Internal_Subr	*/
	},


	/*********************** Obj_Align_Symbol ***********************/
	/* This is an SGI extension.					*/
	/****************************************************************/

	{   0,	/* Obj_Align_Symbol		Name_Variable		*/
	    0,	/* Obj_Align_Symbol		Name_Common_Obj		*/
	    0,	/* Obj_Align_Symbol		Name_Cri_Pointer	*/
	    0,	/* Obj_Align_Symbol		Name_Cri_Pointee	*/
	    0,	/* Obj_Align_Symbol		Name_Cri_Ch_Pointee	*/
	 1442,	/* Obj_Align_Symbol		Name_Func_Result	*/
	 1442,	/* Obj_Align_Symbol		Name_Dummy_Arg		*/
	 1442,	/* Obj_Align_Symbol		Name_Module_Proc	*/
	 1442,	/* Obj_Align_Symbol		Name_Derived_Type	*/
	 1442,	/* Obj_Align_Symbol		Name_Generic_Interface	*/
	 1442,	/* Obj_Align_Symbol		Name_Namelist_Group	*/
	    0,	/* Obj_Align_Symbol		Name_Namelist_Group_Obj	*/
	 1442,	/* Obj_Align_Symbol		Name_Statement_Func	*/
	 1442,	/* Obj_Align_Symbol		Name_Construct		*/
	 1442,	/* Obj_Align_Symbol		Name_Intrinsic_Func	*/
	 1442,	/* Obj_Align_Symbol		Name_Intrinsic_Subr	*/
	 1442,	/* Obj_Align_Symbol		Name_Module		*/
	 1442,	/* Obj_Align_Symbol		Name_Blockdata		*/
	 1442,	/* Obj_Align_Symbol		Name_Program		*/
	 1442,	/* Obj_Align_Symbol		Name_Function		*/
	 1442,	/* Obj_Align_Symbol		Name_Curr_Func		*/
	 1442,	/* Obj_Align_Symbol		Name_Curr_Subr		*/
	 1442,	/* Obj_Align_Symbol		Name_Internal_Func	*/
	 1442	/* Obj_Align_Symbol		Name_Internal_Subr	*/
	},

	/*********************** Obj_Fill_Symbol ************************/
	/* This is an SGI extension.					*/
	/****************************************************************/

	{   0,	/* Obj_Fill_Symbol		Name_Variable		*/
	    0,	/* Obj_Fill_Symbol		Name_Common_Obj		*/
	    0,	/* Obj_Fill_Symbol		Name_Cri_Pointer	*/
	    0,	/* Obj_Fill_Symbol		Name_Cri_Pointee	*/
	    0,	/* Obj_Fill_Symbol		Name_Cri_Ch_Pointee	*/
	 1442,	/* Obj_Fill_Symbol		Name_Func_Result	*/
	 1442,	/* Obj_Fill_Symbol		Name_Dummy_Arg		*/
	 1442,	/* Obj_Fill_Symbol		Name_Module_Proc	*/
	 1442,	/* Obj_Fill_Symbol		Name_Derived_Type	*/
	 1442,	/* Obj_Fill_Symbol		Name_Generic_Interface	*/
	 1442,	/* Obj_Fill_Symbol		Name_Namelist_Group	*/
	    0,	/* Obj_Fill_Symbol		Name_Namelist_Group_Obj	*/
	 1442,	/* Obj_Fill_Symbol		Name_Statement_Func	*/
	 1442,	/* Obj_Fill_Symbol		Name_Construct		*/
	 1442,	/* Obj_Fill_Symbol		Name_Intrinsic_Func	*/
	 1442,	/* Obj_Fill_Symbol		Name_Intrinsic_Subr	*/
	 1442,	/* Obj_Fill_Symbol		Name_Module		*/
	 1442,	/* Obj_Fill_Symbol		Name_Blockdata		*/
	 1442,	/* Obj_Fill_Symbol		Name_Program		*/
	 1442,	/* Obj_Fill_Symbol		Name_Function		*/
	 1442,	/* Obj_Fill_Symbol		Name_Curr_Func		*/
	 1442,	/* Obj_Fill_Symbol		Name_Curr_Subr		*/
	 1442,	/* Obj_Fill_Symbol		Name_Internal_Func	*/
	 1442	/* Obj_Fill_Symbol		Name_Internal_Subr	*/
	},

	/*********************** Obj_Section_Gp *************************/
	/* This is an SGI extension.					*/
	/****************************************************************/

	{   0,	/* Obj_Section_Gp		Name_Variable		*/
	    0,	/* Obj_Section_Gp		Name_Common_Obj		*/
	    0,	/* Obj_Section_Gp		Name_Cri_Pointer	*/
	 1442,	/* Obj_Section_Gp		Name_Cri_Pointee	*/
	 1442,	/* Obj_Section_Gp		Name_Cri_Ch_Pointee	*/
	 1442,	/* Obj_Section_Gp		Name_Func_Result	*/
	 1442,	/* Obj_Section_Gp		Name_Dummy_Arg		*/
	 1442,	/* Obj_Section_Gp		Name_Module_Proc	*/
	 1442,	/* Obj_Section_Gp		Name_Derived_Type	*/
	 1442,	/* Obj_Section_Gp		Name_Generic_Interface	*/
	 1442,	/* Obj_Section_Gp		Name_Namelist_Group	*/
	    0,	/* Obj_Section_Gp		Name_Namelist_Group_Obj	*/
	 1442,	/* Obj_Section_Gp		Name_Statement_Func	*/
	 1442,	/* Obj_Section_Gp		Name_Construct		*/
	 1442,	/* Obj_Section_Gp		Name_Intrinsic_Func	*/
	 1442,	/* Obj_Section_Gp		Name_Intrinsic_Subr	*/
	    0,	/* Obj_Section_Gp		Name_Module		*/
	 1442,	/* Obj_Section_Gp		Name_Blockdata		*/
	 1442,	/* Obj_Section_Gp		Name_Program		*/
	 1442,	/* Obj_Section_Gp		Name_Function		*/
	 1442,	/* Obj_Section_Gp		Name_Curr_Func		*/
	 1442,	/* Obj_Section_Gp		Name_Curr_Subr		*/
	 1442,	/* Obj_Section_Gp		Name_Internal_Func	*/
	 1442	/* Obj_Section_Gp		Name_Internal_Subr	*/
	},


	/*********************** Obj_Section_Non_Gp *********************/
	/* This is an SGI extension.					*/
	/****************************************************************/

	{    0,	/* Obj_Section_Non_Gp		Name_Variable		*/
	    0,	/* Obj_Section_Non_Gp		Name_Common_Obj		*/
	    0,	/* Obj_Section_Non_Gp		Name_Cri_Pointer	*/
	 1442,	/* Obj_Section_Non_Gp		Name_Cri_Pointee	*/
	 1442,	/* Obj_Section_Non_Gp		Name_Cri_Ch_Pointee	*/
	 1442,	/* Obj_Section_Non_Gp		Name_Func_Result	*/
	 1442,	/* Obj_Section_Non_Gp		Name_Dummy_Arg		*/
	 1442,	/* Obj_Section_Non_Gp		Name_Module_Proc	*/
	 1442,	/* Obj_Section_Non_Gp		Name_Derived_Type	*/
	 1442,	/* Obj_Section_Non_Gp		Name_Generic_Interface	*/
	 1442,	/* Obj_Section_Non_Gp		Name_Namelist_Group	*/
	    0,	/* Obj_Section_Non_Gp		Name_Namelist_Group_Obj	*/
	 1442,	/* Obj_Section_Non_Gp		Name_Statement_Func	*/
	 1442,	/* Obj_Section_Non_Gp		Name_Construct		*/
	 1442,	/* Obj_Section_Non_Gp		Name_Intrinsic_Func	*/
	 1442,	/* Obj_Section_Non_Gp		Name_Intrinsic_Subr	*/
	    0,	/* Obj_Section_Non_Gp		Name_Module		*/
	 1442,	/* Obj_Section_Non_Gp		Name_Blockdata		*/
	 1442,	/* Obj_Section_Non_Gp		Name_Program		*/
	 1442,	/* Obj_Section_Non_Gp		Name_Function		*/
	 1442,	/* Obj_Section_Non_Gp		Name_Curr_Func		*/
	 1442,	/* Obj_Section_Non_Gp		Name_Curr_Subr		*/
	 1442,	/* Obj_Section_Non_Gp		Name_Internal_Func	*/
	 1442	/* Obj_Section_Non_Gp		Name_Internal_Subr	*/
	},


	/*********************** Obj_Ignore_TKR *************************/
	/****************************************************************/

	{1442,	/* Obj_Ignore_TKR		Name_Variable		*/
	 1442,	/* Obj_Ignore_TKR		Name_Common_Obj		*/
	    0,	/* Obj_Ignore_TKR		Name_Cri_Pointer	*/
	 1442,	/* Obj_Ignore_TKR		Name_Cri_Pointee	*/
	 1442,	/* Obj_Ignore_TKR		Name_Cri_Ch_Pointee	*/
	 1442,	/* Obj_Ignore_TKR		Name_Func_Result	*/
	    0,	/* Obj_Ignore_TKR		Name_Dummy_Arg		*/
	 1442,	/* Obj_Ignore_TKR		Name_Module_Proc	*/
	 1442,	/* Obj_Ignore_TKR		Name_Derived_Type	*/
	 1442,	/* Obj_Ignore_TKR		Name_Generic_Interface	*/
	 1442,	/* Obj_Ignore_TKR		Name_Namelist_Group	*/
	    0,	/* Obj_Ignore_TKR		Name_Namelist_Group_Obj	*/
	 1442,	/* Obj_Ignore_TKR		Name_Statement_Func	*/
	 1442,	/* Obj_Ignore_TKR		Name_Construct		*/
	 1442,	/* Obj_Ignore_TKR		Name_Intrinsic_Func	*/
	 1442,	/* Obj_Ignore_TKR		Name_Intrinsic_Subr	*/
	 1442,	/* Obj_Ignore_TKR		Name_Module		*/
	 1442,	/* Obj_Ignore_TKR		Name_Blockdata		*/
	 1442,	/* Obj_Ignore_TKR		Name_Program		*/
	 1442,	/* Obj_Ignore_TKR		Name_Function		*/
	 1442,	/* Obj_Ignore_TKR		Name_Curr_Func		*/
	 1442,	/* Obj_Ignore_TKR		Name_Curr_Subr		*/
	 1442,	/* Obj_Ignore_TKR		Name_Internal_Func	*/
	 1442	/* Obj_Ignore_TKR		Name_Internal_Subr	*/
	},

	/************************ Obj_Optional_Dir **********************/
	/****************************************************************/

	{1442,	/* Obj_Optional_Dir		Name_Variable		*/
	 1442,	/* Obj_Optional_Dir		Name_Common_Obj		*/
	 1442,	/* Obj_Optional_Dir		Name_Cri_Pointer	*/
	 1442,	/* Obj_Optional_Dir		Name_Cri_Pointee	*/
	 1442,	/* Obj_Optional_Dir		Name_Cri_Ch_Pointee	*/
	 1442,	/* Obj_Optional_Dir		Name_Func_Result	*/
	    0,	/* Obj_Optional_Dir		Name_Dummy_Arg		*/
	    0,	/* Obj_Optional_Dir		Name_Module_Proc	*/
	 1442,	/* Obj_Optional_Dir		Name_Derived_Type	*/
	 1442,	/* Obj_Optional_Dir		Name_Generic_Interface	*/
	 1442,	/* Obj_Optional_Dir		Name_Namelist_Group	*/
	 1442,	/* Obj_Optional_Dir		Name_Namelist_Group_Obj	*/
	 1442,	/* Obj_Optional_Dir		Name_Statement_Func	*/
	 1442,	/* Obj_Optional_Dir		Name_Construct		*/
	 1442,	/* Obj_Optional_Dir		Name_Intrinsic_Func	*/
	 1442,	/* Obj_Optional_Dir		Name_Intrinsic_Subr	*/
	 1442,	/* Obj_Optional_Dir		Name_Module		*/
	    0,	/* Obj_Optional_Dir		Name_Blockdata		*/
	 1442,	/* Obj_Optional_Dir		Name_Program		*/
 	    0,	/* Obj_Optional_Dir		Name_Function		*/
	    0,	/* Obj_Optional_Dir		Name_Curr_Func		*/
	    0, 	/* Obj_Optional_Dir		Name_Curr_Subr		*/
	 1442,	/* Obj_Optional_Dir		Name_Internal_Func	*/
	 1442	/* Obj_Optional_Dir		Name_Internal_Subr	*/
	},

	/************************ Obj_Name ******************************/
	/* This is declared using the !DIR$ C(funcname) directive.	*/
	/****************************************************************/

	{1442,	/* Obj_Name			Name_Variable		*/
	 1442,	/* Obj_Name			Name_Common_Obj		*/
	 1442,	/* Obj_Name			Name_Cri_Pointer	*/
	 1442,	/* Obj_Name			Name_Cri_Pointee	*/
	 1442,	/* Obj_Name			Name_Cri_Ch_Pointee	*/
	 1442,	/* Obj_Name			Name_Func_Result	*/
	    0,	/* Obj_Name			Name_Dummy_Arg		*/
	 1442,	/* Obj_Name			Name_Module_Proc	*/
	 1442,	/* Obj_Name			Name_Derived_Type	*/
	 1442,	/* Obj_Name			Name_Generic_Interface	*/
	 1442,	/* Obj_Name			Name_Namelist_Group	*/
	 1442,	/* Obj_Name			Name_Namelist_Group_Obj	*/
	 1442,	/* Obj_Name			Name_Statement_Func	*/
	 1442,	/* Obj_Name			Name_Construct		*/
	 1442,	/* Obj_Name			Name_Intrinsic_Func	*/
	 1442,	/* Obj_Name			Name_Intrinsic_Subr	*/
	 1442,	/* Obj_Name			Name_Module		*/
	 1442,	/* Obj_Name			Name_Blockdata		*/
	 1442,	/* Obj_Name			Name_Program		*/
	    0,	/* Obj_Name			Name_Function		*/
	 1442,	/* Obj_Name			Name_Curr_Func		*/
	 1442,	/* Obj_Name			Name_Curr_Subr		*/
	 1442,	/* Obj_Name			Name_Internal_Func	*/
	 1442	/* Obj_Name			Name_Internal_Subr	*/
	},

	/************************ Obj_Cri_Ptr ***************************/
	/* From cft77's documentation - A Cray pointer cannot be a	*/
	/* constant, an array, a statement function or an external	*/
	/* function.	It can be in common and be a darg.	A Cray	*/
	/* pointer is considered to be a data type.			*/
	/****************************************************************/

	{   0,	/* Obj_Cri_Ptr			Name_Variable		*/
	    0,	/* Obj_Cri_Ptr			Name_Common_Obj		*/
	    0,	/* Obj_Cri_Ptr			Name_Cri_Pointer	*/
	  553,	/* Obj_Cri_Ptr			Name_Cri_Pointee	*/
	  553,	/* Obj_Cri_Ptr			Name_Cri_Ch_Pointee	*/
	  553,	/* Obj_Cri_Ptr			Name_Func_Result	*/
	    0,	/* Obj_Cri_Ptr			Name_Dummy_Arg		*/
	  553,	/* Obj_Cri_Ptr			Name_Module_Proc	*/
	  553,	/* Obj_Cri_Ptr			Name_Derived_Type	*/
	  553,	/* Obj_Cri_Ptr			Name_Generic_Interface	*/
	  553,	/* Obj_Cri_Ptr			Name_Namelist_Group	*/
	    0,	/* Obj_Cri_Ptr			Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Cri_Ptr			Name_Statement_Func	*/
	  553,	/* Obj_Cri_Ptr			Name_Construct		*/
	  553,	/* Obj_Cri_Ptr			Name_Intrinsic_Func	*/
	  553,	/* Obj_Cri_Ptr			Name_Intrinsic_Subr	*/
	  553,	/* Obj_Cri_Ptr			Name_Module		*/
	  553,	/* Obj_Cri_Ptr			Name_Blockdata		*/
	  553,	/* Obj_Cri_Ptr			Name_Program		*/
	  553,	/* Obj_Cri_Ptr			Name_Function		*/
	  553,	/* Obj_Cri_Ptr			Name_Curr_Func		*/
	  553,	/* Obj_Cri_Ptr			Name_Curr_Subr		*/
	  553,	/* Obj_Cri_Ptr			Name_Internal_Func	*/
	  553	/* Obj_Cri_Ptr			Name_Internal_Subr	*/
	},


	/************************ Obj_Cri_Pointee ***********************/
	/* From cft77's documentation - A Cray pointee cannot be a	*/
	/* constant, in common, saved, data initialized or equivalenced	*/
	/* It cannot be a darg or a function result.			*/
	/****************************************************************/

	{ 553,	/* Obj_Cri_Pointee		Name_Variable		*/
	  553,	/* Obj_Cri_Pointee		Name_Common_Obj		*/
	  553,	/* Obj_Cri_Pointee		Name_Cri_Pointer	*/
	  553,	/* Obj_Cri_Pointee		Name_Cri_Pointee	*/
	  553,	/* Obj_Cri_Pointee		Name_Cri_Ch_Pointee	*/
	  553,	/* Obj_Cri_Pointee		Name_Func_Result	*/
	  553,	/* Obj_Cri_Pointee		Name_Dummy_Arg		*/
	  553,	/* Obj_Cri_Pointee		Name_Module_Proc	*/
	  553,	/* Obj_Cri_Pointee		Name_Derived_Type	*/
	  553,	/* Obj_Cri_Pointee		Name_Generic_Interface	*/
	  553,	/* Obj_Cri_Pointee		Name_Namelist_Group	*/
	  553,	/* Obj_Cri_Pointee		Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Cri_Pointee		Name_Statement_Func	*/
	  553,	/* Obj_Cri_Pointee		Name_Construct		*/
	  553,	/* Obj_Cri_Pointee		Name_Intrinsic_Func	*/
	  553,	/* Obj_Cri_Pointee		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Cri_Pointee		Name_Module		*/
	  553,	/* Obj_Cri_Pointee		Name_Blockdata		*/
	  553,	/* Obj_Cri_Pointee		Name_Program		*/
	  553,	/* Obj_Cri_Pointee		Name_Function		*/
	  553,	/* Obj_Cri_Pointee		Name_Curr_Func		*/
	  553,	/* Obj_Cri_Pointee		Name_Curr_Subr		*/
	  553,	/* Obj_Cri_Pointee		Name_Internal_Func	*/
	  553 	/* Obj_Cri_Pointee		Name_Internal_Subr	*/
	},


	/********************** Obj_Cri_Ch_Pointee **********************/
	/* From cft77's documentation - A Cray pointee cannot be a	*/
	/* constant, in common, saved, data initialized or equivalenced	*/
	/* It cannot be a darg or a function result.	A character	*/
	/* pointee cannot be an array.				 	*/
	/****************************************************************/

	{ 553,	/* Obj_Cri_Ch_Pointee		Name_Variable		*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Common_Obj		*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Cri_Pointer	*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Cri_Pointee	*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Cri_Ch_Pointee	*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Func_Result	*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Dummy_Arg		*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Module_Proc	*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Derived_Type	*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Generic_Interface	*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Namelist_Group	*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Statement_Func	*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Construct		*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Intrinsic_Func	*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Module		*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Blockdata		*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Program		*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Function		*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Curr_Func		*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Curr_Subr		*/
	  553,	/* Obj_Cri_Ch_Pointee		Name_Internal_Func	*/
	  553 	/* Obj_Cri_Ch_Pointee		Name_Internal_Subr	*/
	},


	/************************ Obj_Ntry_Func_Result ******************/
	/* The result of a Function, specified on an entry statement.	*/
	/****************************************************************/

	{ 553,	/* Obj_Ntry_Func_Result		Name_Variable		*/
	  553,	/* Obj_Ntry_Func_Result		Name_Common_Obj		*/
	  553,	/* Obj_Ntry_Func_Result		Name_Cri_Pointer	*/
	  553,	/* Obj_Ntry_Func_Result		Name_Cri_Pointee	*/
	  553,	/* Obj_Ntry_Func_Result		Name_Cri_Ch_Pointee	*/
	    0,	/* Obj_Ntry_Func_Result		Name_Func_Result	*/
	  553,	/* Obj_Ntry_Func_Result		Name_Dummy_Arg		*/
	  553,	/* Obj_Ntry_Func_Result		Name_Module_Proc	*/
	  553,	/* Obj_Ntry_Func_Result		Name_Derived_Type	*/
	  553,	/* Obj_Ntry_Func_Result		Name_Generic_Interface	*/
	  553,	/* Obj_Ntry_Func_Result		Name_Namelist_Group	*/
	    0,	/* Obj_Ntry_Func_Result		Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Ntry_Func_Result		Name_Statement_Func	*/
	  553,	/* Obj_Ntry_Func_Result		Name_Construct		*/
	  553,	/* Obj_Ntry_Func_Result		Name_Intrinsic_Func	*/
	  553,	/* Obj_Ntry_Func_Result		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Ntry_Func_Result		Name_Module		*/
	  553,	/* Obj_Ntry_Func_Result		Name_Blockdata		*/
	  553,	/* Obj_Ntry_Func_Result		Name_Program		*/
	  553,	/* Obj_Ntry_Func_Result		Name_Function		*/
	  553,	/* Obj_Ntry_Func_Result		Name_Curr_Func		*/
	  553,	/* Obj_Ntry_Func_Result		Name_Curr_Subr		*/
	  553,	/* Obj_Ntry_Func_Result		Name_Internal_Func	*/
	  553	/* Obj_Ntry_Func_Result		Name_Internal_Subr	*/
	},


	/************************ Obj_Dummy_Arg *************************/
	/* The result of a Function, specified on an entry statement.	*/
	/****************************************************************/

	{   0,	/* Obj_Dummy_Arg		Name_Variable		*/
	  553,	/* Obj_Dummy_Arg		Name_Common_Obj		*/
		/* 5.5.2						*/

	    0,	/* Obj_Dummy_Arg		Name_Cri_Pointer	*/
	  553,	/* Obj_Dummy_Arg		Name_Cri_Pointee	*/
	  553,	/* Obj_Dummy_Arg		Name_Cri_Ch_Pointee	*/
	  553,	/* Obj_Dummy_Arg		Name_Func_Result	*/
	    0,	/* Obj_Dummy_Arg		Name_Dummy_Arg		*/
	  553,	/* Obj_Dummy_Arg		Name_Module_Proc	*/
	  553,	/* Obj_Dummy_Arg		Name_Derived_Type	*/
	  553,	/* Obj_Dummy_Arg		Name_Generic_Interface	*/
	  553,	/* Obj_Dummy_Arg		Name_Namelist_Group	*/
	    0,	/* Obj_Dummy_Arg		Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Dummy_Arg		Name_Statement_Func	*/
	  553,	/* Obj_Dummy_Arg		Name_Construct		*/
	  553,	/* Obj_Dummy_Arg		Name_Intrinsic_Func	*/
	  553,	/* Obj_Dummy_Arg		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Dummy_Arg		Name_Module		*/
	  553,	/* Obj_Dummy_Arg		Name_Blockdata		*/
	  553,	/* Obj_Dummy_Arg		Name_Program		*/
	    0,	/* Obj_Dummy_Arg		Name_Function		*/
	  553,	/* Obj_Dummy_Arg		Name_Curr_Func		*/
	  553,	/* Obj_Dummy_Arg		Name_Curr_Subr		*/
	  553,	/* Obj_Dummy_Arg		Name_Internal_Func	*/
	  553	/* Obj_Dummy_Arg		Name_Internal_Subr	*/
	},


	/************************ Obj_Common_Obj ************************/
	/* A common-block object must not be a dummy argument, an	*/
	/* allocatable array, an automatic object, a function name,	*/
	/* an entry name or a result name.	(5.5.2)			*/
	/* (R549) An object in common is a variable name.		*/
	/****************************************************************/

	{   0,	/* Obj_Common_Obj		Name_Variable		*/
	  593,	/* Obj_Common_Obj		Name_Common_Obj		*/
	    0,	/* Obj_Common_Obj		Name_Cri_Pointer	*/

	  553,	/* Obj_Common_Obj		Name_Cri_Pointee	*/
	  553,	/* Obj_Common_Obj		Name_Cri_Ch_Pointee	*/
		/* Cray extension					*/

	  553,	/* Obj_Common_Obj		Name_Func_Result	*/
	  553,	/* Obj_Common_Obj		Name_Dummy_Arg		*/
	  553,	/* Obj_Common_Obj		Name_Module_Proc	*/
	  553,	/* Obj_Common_Obj		Name_Derived_Type	*/
	  553,	/* Obj_Common_Obj		Name_Generic_Interface	*/
	  553,	/* Obj_Common_Obj		Name_Namelist_Group	*/
	    0,	/* Obj_Common_Obj		Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Common_Obj		Name_Statement_Func	*/
	  553,	/* Obj_Common_Obj		Name_Construct		*/
	  553,	/* Obj_Common_Obj		Name_Intrinsic_Func	*/
	  553,	/* Obj_Common_Obj		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Common_Obj		Name_Module		*/
	  553,	/* Obj_Common_Obj		Name_Blockdata		*/
	  553,	/* Obj_Common_Obj		Name_Program		*/
	  553,	/* Obj_Common_Obj		Name_Function		*/
	  553,	/* Obj_Common_Obj		Name_Curr_Func		*/
	  553,	/* Obj_Common_Obj		Name_Curr_Subr		*/
	  553,	/* Obj_Common_Obj		Name_Internal_Func	*/
	  553 	/* Obj_Common_Obj		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Namelist_Obj **********************/
	/* A namelist object must not be an array dummy argument with	*/
	/* a non-constant bound, a variable with a non-constant char	*/
	/* length, an automatic object, a pointer or an allocatable	*/
	/* array.	(5.4) constraint				*/
	/* This must be a variable name.	(R544)			*/
	/****************************************************************/

	{   0,	/* Obj_Namelist_Obj		Name_Variable		*/
	    0,	/* Obj_Namelist_Obj		Name_Common_Obj		*/
	    0,	/* Obj_Namelist_Obj		Name_Cri_Pointer	*/
	  553,	/* Obj_Namelist_Obj		Name_Cri_Pointee	*/
	  553,	/* Obj_Namelist_Obj		Name_Cri_Ch_Pointee	*/
	    0,	/* Obj_Namelist_Obj		Name_Func_Result	*/
	    0,	/* Obj_Namelist_Obj		Name_Dummy_Arg		*/

	  553,	/* Obj_Namelist_Obj		Name_Module_Proc	*/
	  553,	/* Obj_Namelist_Obj		Name_Derived_Type	*/
	  553,	/* Obj_Namelist_Obj		Name_Generic_Interface	*/
	  553,	/* Obj_Namelist_Obj		Name_Namelist_Group	*/

	    0,	/* Obj_Namelist_Obj		Name_Namelist_Group_Obj	*/

	  553,	/* Obj_Namelist_Obj		Name_Statement_Func	*/
	  553,	/* Obj_Namelist_Obj		Name_Construct		*/
	  553,	/* Obj_Namelist_Obj		Name_Intrinsic_Func	*/
	  553,	/* Obj_Namelist_Obj		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Namelist_Obj		Name_Module		*/
	  553,	/* Obj_Namelist_Obj		Name_Blockdata		*/
	  553,	/* Obj_Namelist_Obj		Name_Program		*/
	  553,	/* Obj_Namelist_Obj		Name_Function		*/
	  553,	/* Obj_Namelist_Obj		Name_Curr_Func		*/
	  553,	/* Obj_Namelist_Obj		Name_Curr_Subr		*/
	  553,	/* Obj_Namelist_Obj		Name_Internal_Func	*/
	  553	/* Obj_Namelist_Obj		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Module_Proc ***********************/
	/* 12.3.2.1 states that a procedure must not have more than	*/
	/* one explicit interface in a scoping unit.	12.3.1 states	*/
	/* that the interface of a module procedure is always an	*/
	/* explicit interface in a scoping unit.	12.3 defines an	*/
	/* interface as the characteristics of the procedure, its	*/
	/* dummy arguments and its function result (if a function).	*/
	/* Since the actual definition of the module procedure is the	*/
	/* explicit interface, it's characteristics cannot be defined	*/
	/* outside of the module procedure definition.		 	*/
	/* Also 5.1 states that an entity must not be given any		*/
	/* attribute more than once in a scoping unit.			*/
	/****************************************************************/

	{ 553,	/* Obj_Module_Proc		Name_Variable		*/
	  553,	/* Obj_Module_Proc		Name_Common_Obj		*/
	  553,	/* Obj_Module_Proc		Name_Cri_Pointer	*/
	  553,	/* Obj_Module_Proc		Name_Cri_Pointee	*/
	  553,	/* Obj_Module_Proc		Name_Cri_Ch_Pointee	*/
	  553,	/* Obj_Module_Proc		Name_Func_Result	*/
	  553,	/* Obj_Module_Proc		Name_Dummy_Arg		*/

	    0,	/* Obj_Module_Proc		Name_Module_Proc	*/

	  553,	/* Obj_Module_Proc		Name_Derived_Type	*/
		/* 14.1.2						*/

	  553,	/* Obj_Module_Proc		Name_Generic_Interface	*/
		/* 12.3.2.1 discussion - This is legal for the		*/
		/* module procedure to be the same name as the		*/
		/* current interface block, but not the same as any	*/
		/* other interface block.  This is caught by the rtn.	*/

	  553,	/* Obj_Module_Proc		Name_Namelist_Group	*/
	  553,	/* Obj_Module_Proc		Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Module_Proc		Name_Statement_Func	*/
	  553,	/* Obj_Module_Proc		Name_Construct		*/
	    0,	/* Obj_Module_Proc		Name_Intrinsic_Func	*/
	    0,	/* Obj_Module_Proc		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Module_Proc		Name_Module		*/
	  553,	/* Obj_Module_Proc		Name_Blockdata		*/
	  553,	/* Obj_Module_Proc		Name_Program		*/
	  553,	/* Obj_Module_Proc		Name_Function		*/
	  553,	/* Obj_Module_Proc		Name_Curr_Func		*/
	  553,	/* Obj_Module_Proc		Name_Curr_Subr		*/
	  553,	/* Obj_Module_Proc		Name_Internal_Func	*/
	  553	/* Obj_Module_Proc		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Derived_Type **********************/
	/* By 14.1.2, a derived type cannot be a named variable, a	*/
	/* named constant, a construct name, a statement function, an	*/
	/* internal procedure, a dummy procedure, an intrinsic proc,	*/
	/* a generic identifier or a namelist group name.		*/
	/****************************************************************/

	{  553,	/* Obj_Derived_Type		Name_Variable		*/
	  553,	/* Obj_Derived_Type		Name_Common_Obj		*/
	  553,	/* Obj_Derived_Type		Name_Cri_Pointer	*/
	  553,	/* Obj_Derived_Type		Name_Cri_Pointee	*/
	  553,	/* Obj_Derived_Type		Name_Cri_Ch_Pointee	*/
	  553,	/* Obj_Derived_Type		Name_Func_Result	*/
	  553,	/* Obj_Derived_Type		Name_Dummy_Arg		*/
	  553,	/* Obj_Derived_Type		Name_Module_Proc	*/
	  553,	/* Obj_Derived_Type		Name_Derived_Type	*/
	  553,	/* Obj_Derived_Type		Name_Generic_Interface	*/
	  553,	/* Obj_Derived_Type		Name_Namelist_Group	*/
	  553,	/* Obj_Derived_Type		Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Derived_Type		Name_Statement_Func	*/
	  553,	/* Obj_Derived_Type		Name_Construct		*/
	  553,	/* Obj_Derived_Type		Name_Intrinsic_Func	*/
	  553,	/* Obj_Derived_Type		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Derived_Type		Name_Module		*/
	  553,	/* Obj_Derived_Type		Name_Blockdata		*/
	  553,	/* Obj_Derived_Type		Name_Program		*/
	  553,	/* Obj_Derived_Type		Name_Function		*/
	  553,	/* Obj_Derived_Type		Name_Curr_Func		*/
	  553,	/* Obj_Derived_Type		Name_Curr_Subr		*/
	  553,	/* Obj_Derived_Type		Name_Internal_Func	*/
	  553	/* Obj_Derived_Type		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Generic_Interface *****************/
	/* By 14.1.2, a generic interface cannot be a named variable, a */
	/* named constant, a construct name, a statement function, an	*/
	/* internal procedure, a dummy procedure, an intrinsic proc,	*/
	/* a derived type or a namelist group name.			*/
	/****************************************************************/

	{ 553,	/* Obj_Generic_Interface	Name_Variable		*/
	  553,	/* Obj_Generic_Interface	Name_Common_Obj		*/
	  553,	/* Obj_Generic_Interface	Name_Cri_Pointer	*/
	  553,	/* Obj_Generic_Interface	Name_Cri_Pointee	*/
	  553,	/* Obj_Generic_Interface	Name_Cri_Ch_Pointee	*/
	  553,	/* Obj_Generic_Interface	Name_Func_Result	*/
		/* 14.1.2						*/

	  553,	/* Obj_Generic_Interface	Name_Dummy_Arg		*/
		/* ????	Since they can be the same as functions.	*/

	    0,	/* Obj_Generic_Interface	Name_Module_Proc	*/
		/* 12.3.2.1 discussion					*/

	  553,	/* Obj_Generic_Interface	Name_Derived_Type	*/
		/* 14.1.2						*/

	    0,	/* Obj_Generic_Interface	Name_Generic_Interface	*/
		/* 12.3.2.1 discussion					*/

	  553,	/* Obj_Generic_Interface	Name_Namelist_Group	*/
	  553,	/* Obj_Generic_Interface	Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Generic_Interface	Name_Statement_Func	*/
	  553,	/* Obj_Generic_Interface	Name_Construct		*/
	    0,	/* Obj_Generic_Interface	Name_Intrinsic_Func	*/
	    0,	/* Obj_Generic_Interface	Name_Intrinsic_Subr	*/
	  553,	/* Obj_Generic_Interface	Name_Module		*/
	  553,	/* Obj_Generic_Interface	Name_Blockdata		*/
	  553,	/* Obj_Generic_Interface	Name_Program		*/
	    0,	/* Obj_Generic_Interface	Name_Function		*/
	  553,	/* Obj_Generic_Interface	Name_Curr_Func		*/
	  553,	/* Obj_Generic_Interface	Name_Curr_Subr		*/
	  553,	/* Obj_Generic_Interface	Name_Internal_Func	*/
	  553	/* Obj_Generic_Interface	Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Namelist_Grp **********************/
	/* By 14.1.2, a namelist group cannot be a named variable, a	*/
	/* named constant, a construct name, a statement function, an	*/
	/* internal procedure, a dummy procedure, an intrinsic proc,	*/
	/* a derived type or a generic identifier.			*/
	/****************************************************************/

	{ 553,	/* Obj_Namelist_Grp		Name_Variable		*/
	  553,	/* Obj_Namelist_Grp		Name_Common_Obj		*/
	  553,	/* Obj_Namelist_Grp		Name_Cri_Pointer	*/
	  553,	/* Obj_Namelist_Grp		Name_Cri_Pointee	*/
	  553,	/* Obj_Namelist_Grp		Name_Cri_Ch_Pointee	*/
	  553,	/* Obj_Namelist_Grp		Name_Func_Result	*/
	  553,	/* Obj_Namelist_Grp		Name_Dummy_Arg		*/
	  553,	/* Obj_Namelist_Grp		Name_Module_Proc	*/
	  553,	/* Obj_Namelist_Grp		Name_Derived_Type	*/
	  553,	/* Obj_Namelist_Grp		Name_Generic_Interface	*/
		/* 14.1.2						*/

	    0,	/* Obj_Namelist_Grp		Name_Namelist_Group	*/
		/* 5.4 discussion					*/

	  553,	/* Obj_Namelist_Grp		Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Namelist_Grp		Name_Statement_Func	*/
	  553,	/* Obj_Namelist_Grp		Name_Construct		*/
	  553,	/* Obj_Namelist_Grp		Name_Intrinsic_Func	*/
	  553,	/* Obj_Namelist_Grp		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Namelist_Grp		Name_Module		*/
	  553,	/* Obj_Namelist_Grp		Name_Blockdata		*/
	  553,	/* Obj_Namelist_Grp		Name_Program		*/
	  553,	/* Obj_Namelist_Grp		Name_Function		*/
	  553,	/* Obj_Namelist_Grp		Name_Curr_Func		*/
	  553,	/* Obj_Namelist_Grp		Name_Curr_Subr		*/
	  553,	/* Obj_Namelist_Grp		Name_Internal_Func	*/
	  553	/* Obj_Namelist_Grp		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Stmt_Func *************************/
	/* By 14.1.2, a statement function cannot be a named variable,  */
	/* a named constant, a construct name, a generic identifier, an */
	/* internal procedure, a dummy procedure, an intrinsic proc,	*/
	/* a derived type or a namelist group.	Also see 12.5.4.	*/
	/****************************************************************/

	{ 553,	/* Obj_Stmt_Func		Name_Variable		*/
	  553,	/* Obj_Stmt_Func		Name_Common_Obj		*/
	  553,	/* Obj_Stmt_Func		Name_Cri_Pointer	*/
	  553,	/* Obj_Stmt_Func		Name_Cri_Pointee	*/
	  553,	/* Obj_Stmt_Func		Name_Cri_Ch_Pointee	*/
	  553,	/* Obj_Stmt_Func		Name_Func_Result	*/
	  553,	/* Obj_Stmt_Func		Name_Dummy_Arg		*/
	  553,	/* Obj_Stmt_Func		Name_Module_Proc	*/
	  553,	/* Obj_Stmt_Func		Name_Derived_Type	*/
	  553,	/* Obj_Stmt_Func		Name_Generic_Interface	*/
	  553,	/* Obj_Stmt_Func		Name_Namelist_Group	*/
	  553,	/* Obj_Stmt_Func		Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Stmt_Func		Name_Statement_Func	*/
	  553,	/* Obj_Stmt_Func		Name_Construct		*/
	  553,	/* Obj_Stmt_Func		Name_Intrinsic_Func	*/
	  553,	/* Obj_Stmt_Func		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Stmt_Func		Name_Module		*/
	  553,	/* Obj_Stmt_Func		Name_Blockdata		*/
	  553,	/* Obj_Stmt_Func		Name_Program		*/
	  553,	/* Obj_Stmt_Func		Name_Function		*/
	  553,	/* Obj_Stmt_Func		Name_Curr_Func		*/
	  553,	/* Obj_Stmt_Func		Name_Curr_Subr		*/
	  553,	/* Obj_Stmt_Func		Name_Internal_Func	*/
	  553	/* Obj_Stmt_Func		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Construct *************************/
	/* By 14.1.2, a construct cannot be a named variable, a named	*/
	/* constant, a statement function, a generic identifier, an	*/
	/* internal procedure, a dummy procedure, an intrinsic proc,	*/
	/* a derived type or a namelist group.			 	*/
	/****************************************************************/

	{ 553,	/* Obj_Construct		Name_Variable		*/
	  553,	/* Obj_Construct		Name_Common_Obj		*/
	  553,	/* Obj_Construct		Name_Cri_Pointer	*/
	  553,	/* Obj_Construct		Name_Cri_Pointee	*/
	  553,	/* Obj_Construct		Name_Cri_Ch_Pointee	*/
	  553,	/* Obj_Construct		Name_Func_Result	*/
	  553,	/* Obj_Construct		Name_Dummy_Arg		*/
	  553,	/* Obj_Construct		Name_Module_Proc	*/
	  553,	/* Obj_Construct		Name_Derived_Type	*/
	  553,	/* Obj_Construct		Name_Generic_Interface	*/
	  553,	/* Obj_Construct		Name_Namelist_Group	*/
	  553,	/* Obj_Construct		Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Construct		Name_Statement_Func	*/
	  553,	/* Obj_Construct		Name_Construct		*/
	  553,	/* Obj_Construct		Name_Intrinsic_Func	*/
	  553,	/* Obj_Construct		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Construct		Name_Module		*/
	  553,	/* Obj_Construct		Name_Blockdata		*/
	  553,	/* Obj_Construct		Name_Program		*/
	  553,	/* Obj_Construct		Name_Function		*/
	  553,	/* Obj_Construct		Name_Curr_Func		*/
	  553,	/* Obj_Construct		Name_Curr_Subr		*/
	  553,	/* Obj_Construct		Name_Internal_Func	*/
	  553	/* Obj_Construct		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Entry_Func ************************/
	/* This is the name on an ENTRY statement in an external	*/
	/* FUNCTION.	This allows anything legal on a result name to	*/
	/* be specified here.	Then if a different result name is	*/
	/* specified errors are issued at that time saying to use the	*/
	/* result name, not the function name to declare type, shape,	*/
	/* target or pointer.						*/
	/****************************************************************/

	{ 553,	/* Obj_Entry_Func		Name_Variable		*/
	  553,	/* Obj_Entry_Func		Name_Common_Obj		*/
	  553,	/* Obj_Entry_Func		Name_Cri_Pointer	*/
	  553,	/* Obj_Entry_Func		Name_Cri_Pointee	*/
	  553,	/* Obj_Entry_Func		Name_Cri_Ch_Pointee	*/
	  553,	/* Obj_Entry_Func		Name_Func_Result	*/
	  553,	/* Obj_Entry_Func		Name_Dummy_Arg		*/
	  553,	/* Obj_Entry_Func		Name_Module_Proc	*/
	  553,	/* Obj_Entry_Func		Name_Derived_Type	*/
	  553,	/* Obj_Entry_Func		Name_Generic_Interface	*/
	  553,	/* Obj_Entry_Func		Name_Namelist_Group	*/
	  553,	/* Obj_Entry_Func		Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Entry_Func		Name_Statement_Func	*/
	  553,	/* Obj_Entry_Func		Name_Construct		*/
	  553,	/* Obj_Entry_Func		Name_Intrinsic_Func	*/
	  553,	/* Obj_Entry_Func		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Entry_Func		Name_Module		*/
	  553,	/* Obj_Entry_Func		Name_Blockdata		*/
	  553,	/* Obj_Entry_Func		Name_Program		*/
	    0,	/* Obj_Entry_Func		Name_Function		*/
	  553,	/* Obj_Entry_Func		Name_Curr_Func		*/
	  553,	/* Obj_Entry_Func		Name_Curr_Subr		*/
	  553,	/* Obj_Entry_Func		Name_Internal_Func	*/
	  553	/* Obj_Entry_Func		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Entry_Subr ************************/
	/* This is the name on an ENTRY statement in an external	*/
	/* SUBROUTINE.	One just knows that a SUBROUTINE can't have	*/
	/* a type, or a shape, or much of anything else.	(2.2.3)	*/
	/****************************************************************/

	{ 553,	/* Obj_Entry_Subr		Name_Variable		*/
	  553,	/* Obj_Entry_Subr		Name_Common_Obj		*/
	  553,	/* Obj_Entry_Subr		Name_Cri_Pointer	*/
	  553,	/* Obj_Entry_Subr		Name_Cri_Pointee	*/
	  553,	/* Obj_Entry_Subr		Name_Cri_Ch_Pointee	*/
	  553,	/* Obj_Entry_Subr		Name_Func_Result	*/
	  553,	/* Obj_Entry_Subr		Name_Dummy_Arg		*/
	  553,	/* Obj_Entry_Subr		Name_Module_Proc	*/
	  553,	/* Obj_Entry_Subr		Name_Derived_Type	*/
	  553,	/* Obj_Entry_Subr		Name_Generic_Interface	*/
	  553,	/* Obj_Entry_Subr		Name_Namelist_Group	*/
	  553,	/* Obj_Entry_Subr		Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Entry_Subr		Name_Statement_Func	*/
	  553,	/* Obj_Entry_Subr		Name_Construct		*/
	  553,	/* Obj_Entry_Subr		Name_Intrinsic_Func	*/
	  553,	/* Obj_Entry_Subr		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Entry_Subr		Name_Module		*/
	  553,	/* Obj_Entry_Subr		Name_Blockdata		*/
	  553,	/* Obj_Entry_Subr		Name_Program		*/
	  553,	/* Obj_Entry_Subr		Name_Function		*/
	  553,	/* Obj_Entry_Subr		Name_Curr_Func		*/
	  553,	/* Obj_Entry_Subr		Name_Curr_Subr		*/
	  553,	/* Obj_Entry_Subr		Name_Internal_Func	*/
	  553 	/* Obj_Entry_Subr		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Intern_Func ***********************/
	/* This is the name on the FUNCTION statement for an internal	*/
	/* FUNCTION.	12.3.2.1 states that a procedure must not have	*/
	/* more than one explicit interface in a scoping unit.	12.3.1  */
	/* states that the interface of an internal procedure is always */
	/* an explicit interface in a scoping unit.	12.3 defines an	*/
	/* interface as the characteristics of the procedure, its	*/
	/* dummy arguments and its function result (if a function).	*/
	/* Since the actual definition of the internal procedure is the */
	/* explicit interface, it's characteristics cannot be defined	*/
	/* outside of the internal procedure definition.		*/
	/* Also 5.1 states that an entity must not be given any		*/
	/* attribute more than once in a scoping unit.		 	*/
	/* Because of forward referencing, what looks like an		*/
	/* intrinsic may be an internal function.			*/
	/****************************************************************/

	{ 553,	/* Obj_Intern_Func		Name_Variable		*/
	  553,	/* Obj_Intern_Func		Name_Common_Obj		*/
	  553,	/* Obj_Intern_Func		Name_Cri_Pointer	*/
	  553,	/* Obj_Intern_Func		Name_Cri_Pointee	*/
	  553,	/* Obj_Intern_Func		Name_Cri_Ch_Pointee	*/
	  553,	/* Obj_Intern_Func		Name_Func_Result	*/
	  553,	/* Obj_Intern_Func		Name_Dummy_Arg		*/
	  553,	/* Obj_Intern_Func		Name_Module_Proc	*/
	  553,	/* Obj_Intern_Func		Name_Derived_Type	*/
	  553,	/* Obj_Intern_Func		Name_Generic_Interface	*/
	  553,	/* Obj_Intern_Func		Name_Namelist_Group	*/
	  553,	/* Obj_Intern_Func		Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Intern_Func		Name_Statement_Func	*/
	  553,	/* Obj_Intern_Func		Name_Construct		*/
	    0,	/* Obj_Intern_Func		Name_Intrinsic_Func	*/
	    0,	/* Obj_Intern_Func		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Intern_Func		Name_Module		*/
	  553,	/* Obj_Intern_Func		Name_Blockdata		*/
	  553,	/* Obj_Intern_Func		Name_Program		*/
	    0,	/* Obj_Intern_Func		Name_Function		*/
	  553,	/* Obj_Intern_Func		Name_Curr_Func		*/
	  553,	/* Obj_Intern_Func		Name_Curr_Subr		*/
	  553,	/* Obj_Intern_Func		Name_Internal_Func	*/
	  553	/* Obj_Intern_Func		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Intern_Subr ***********************/
	/* This is the name on a SUBROUTINE statement for an internal	*/
	/* SUBROUTINE.	One just knows that a SUBROUTINE can't have	*/
	/* a type, or a shape, or much of anything else.	(2.2.3)	*/
	/* Because of forward referencing, what looks like an		*/
	/* intrinsic may be an internal subroutine.			*/
	/****************************************************************/

	{ 553,	/* Obj_Intern_Subr		Name_Variable		*/
	  553,	/* Obj_Intern_Subr		Name_Common_Obj		*/
	  553,	/* Obj_Intern_Subr		Name_Cri_Pointer	*/
	  553,	/* Obj_Intern_Subr		Name_Cri_Pointee	*/
	  553,	/* Obj_Intern_Subr		Name_Cri_Ch_Pointee	*/
	  553,	/* Obj_Intern_Subr		Name_Func_Result	*/
	  553,	/* Obj_Intern_Subr		Name_Dummy_Arg		*/
	  553,	/* Obj_Intern_Subr		Name_Module_Proc	*/
	  553,	/* Obj_Intern_Subr		Name_Derived_Type	*/
	  553,	/* Obj_Intern_Subr		Name_Generic_Interface	*/
	  553,	/* Obj_Intern_Subr		Name_Namelist_Group	*/
	  553,	/* Obj_Intern_Subr		Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Intern_Subr		Name_Statement_Func	*/
	  553,	/* Obj_Intern_Subr		Name_Construct		*/
	    0,	/* Obj_Intern_Subr		Name_Intrinsic_Func	*/
	    0,	/* Obj_Intern_Subr		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Intern_Subr		Name_Module		*/
	  553,	/* Obj_Intern_Subr		Name_Blockdata		*/
	  553,	/* Obj_Intern_Subr		Name_Program		*/
	  553,	/* Obj_Intern_Subr		Name_Function		*/
	  553,	/* Obj_Intern_Subr		Name_Curr_Func		*/
	  553,	/* Obj_Intern_Subr		Name_Curr_Subr		*/
	  553,	/* Obj_Intern_Subr		Name_Internal_Func	*/
	  553	/* Obj_Intern_Subr		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Module_Func ***********************/
	/* This is the name on the FUNCTION statement for a module	*/
	/* FUNCTION.	12.3.2.1 states that a procedure must not have	*/
	/* more than one explicit interface in a scoping unit.	12.3.1  */
	/* states that the interface of a module procedure is always an */
	/* explicit interface in a scoping unit.	12.3 defines an	*/
	/* interface as the characteristics of the procedure, its	*/
	/* dummy arguments and its function result (if a function).	*/
	/* Since the actual definition of the module procedure is the	*/
	/* explicit interface, it's characteristics cannot be defined	*/
	/* outside of the module procedure definition.		 	*/
	/* Also 5.1 states that an entity must not be given any		*/
	/* attribute more than once in a scoping unit.		 	*/
	/* Because of forward referencing, what looks like an		*/
	/* intrinsic may be a module function.			 	*/
	/****************************************************************/

	{ 553,	/* Obj_Module_Func		Name_Variable		*/
	  553,	/* Obj_Module_Func		Name_Common_Obj		*/
	  553,	/* Obj_Module_Func		Name_Cri_Pointer	*/
	  553,	/* Obj_Module_Func		Name_Cri_Pointee	*/
	  553,	/* Obj_Module_Func		Name_Cri_Ch_Pointee	*/
	  553,	/* Obj_Module_Func		Name_Func_Result	*/
	  553,	/* Obj_Module_Func		Name_Dummy_Arg		*/
	    0,	/* Obj_Module_Func		Name_Module_Proc	*/
		/* This is allowed, because this is actually defining   */
		/* the explicit interface for the module procedure.	*/

	  553,	/* Obj_Module_Func		Name_Derived_Type	*/

	    0,	/* Obj_Module_Func		Name_Generic_Interface	*/
		/* 12.3.2.1						*/

	  553,	/* Obj_Module_Func		Name_Namelist_Group	*/
	  553,	/* Obj_Module_Func		Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Module_Func		Name_Statement_Func	*/
	  553,	/* Obj_Module_Func		Name_Construct		*/
	    0,	/* Obj_Module_Func		Name_Intrinsic_Func	*/
	    0,	/* Obj_Module_Func		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Module_Func		Name_Module		*/
	  553,	/* Obj_Module_Func		Name_Blockdata		*/
	  553,	/* Obj_Module_Func		Name_Program		*/
	    0,	/* Obj_Module_Func		Name_Function		*/
	  553,	/* Obj_Module_Func		Name_Curr_Func		*/
	  553,	/* Obj_Module_Func		Name_Curr_Subr		*/
	  553,	/* Obj_Module_Func		Name_Internal_Func	*/
	  553	/* Obj_Module_Func		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},

	/************************ Obj_Module_Subr ***********************/
	/* This is the name on a SUBROUTINE statement for a module	*/
	/* SUBROUTINE.	One just knows that a SUBROUTINE can't have	*/
	/* a type, or a shape, or much of anything else.	(2.2.3)	*/
	/* Because of forward referencing, what looks like an		*/
	/* intrinsic may be a module subroutine.			*/
	/****************************************************************/

	{ 553,	/* Obj_Module_Subr		Name_Variable		*/
	  553,	/* Obj_Module_Subr		Name_Common_Obj		*/
	  553,	/* Obj_Module_Subr		Name_Cri_Pointer	*/
	  553,	/* Obj_Module_Subr		Name_Cri_Pointee	*/
	  553,	/* Obj_Module_Subr		Name_Cri_Ch_Pointee	*/
	  553,	/* Obj_Module_Subr		Name_Func_Result	*/
	  553,	/* Obj_Module_Subr		Name_Dummy_Arg		*/
	    0,	/* Obj_Module_Subr		Name_Module_Proc	*/
		/* This is allowed, because this is actually defining	*/
		/* the explicit interface for the module procedure.	*/

	  553,	/* Obj_Module_Subr		Name_Derived_Type	*/

	    0,	/* Obj_Module_Subr		Name_Generic_Interface	*/
		/* 12.3.2.1						*/

	  553,	/* Obj_Module_Subr		Name_Namelist_Group	*/
	  553,	/* Obj_Module_Subr		Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Module_Subr		Name_Statement_Func	*/
	  553,	/* Obj_Module_Subr		Name_Construct		*/
	    0,	/* Obj_Module_Subr		Name_Intrinsic_Func	*/
	    0,	/* Obj_Module_Subr		Name_Intrinsic_Subr	*/
	  553,	/* Obj_Module_Subr		Name_Module		*/
	  553,	/* Obj_Module_Subr		Name_Blockdata		*/
	  553,	/* Obj_Module_Subr		Name_Program		*/
	  553,	/* Obj_Module_Subr		Name_Function		*/
	  553,	/* Obj_Module_Subr		Name_Curr_Func		*/
	  553,	/* Obj_Module_Subr		Name_Curr_Subr		*/
	  553,	/* Obj_Module_Subr		Name_Internal_Func	*/
	  553	/* Obj_Module_Subr		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},

	/************************ Obj_Sf_Darg ***************************/
	/* This is the dummy argument used in the statement function	*/
	/* definition.	It is defined in 5.1.1.5 and 12.5.4.		*/
	/****************************************************************/

	{   0,	/* Obj_Sf_Darg			Name_Variable		*/
	    0,	/* Obj_Sf_Darg			Name_Common_Obj		*/
	  553,	/* Obj_Sf_Darg			Name_Cri_Pointer	*/
	    0,	/* Obj_Sf_Darg			Name_Cri_Pointee	*/
	  553,	/* Obj_Sf_Darg			Name_Cri_Ch_Pointee	*/
	    0,	/* Obj_Sf_Darg			Name_Func_Result	*/
	    0,	/* Obj_Sf_Darg			Name_Dummy_Arg		*/
	  553,	/* Obj_Sf_Darg			Name_Module_Proc	*/
	  553,	/* Obj_Sf_Darg			Name_Derived_Type	*/
	  553,	/* Obj_Sf_Darg			Name_Generic_Interface	*/
	  553,	/* Obj_Sf_Darg			Name_Namelist_Group	*/
	    0,	/* Obj_Sf_Darg			Name_Namelist_Group_Obj	*/
	  553,	/* Obj_Sf_Darg			Name_Statement_Func	*/
	  553,	/* Obj_Sf_Darg			Name_Construct		*/
	  553,	/* Obj_Sf_Darg			Name_Intrinsic_Func	*/
	  553,	/* Obj_Sf_Darg			Name_Intrinsic_Subr	*/
	  553,	/* Obj_Sf_Darg			Name_Module		*/
	  553,	/* Obj_Sf_Darg			Name_Blockdata		*/
	  553,	/* Obj_Sf_Darg			Name_Program		*/
	  553,	/* Obj_Sf_Darg			Name_Function		*/
	  553,	/* Obj_Sf_Darg			Name_Curr_Func		*/
	  553,	/* Obj_Sf_Darg			Name_Curr_Subr		*/
	  553,	/* Obj_Sf_Darg			Name_Internal_Func	*/
	  553	/* Obj_Sf_Darg			Name_Internal_Subr	*/
	},

	/************************ Obj_Sf_Actual_Arg *********************/
	/* This is the actual argument used when a statement function	*/
	/* is referenced.	It is defined in 5.1.1.5 and 12.5.4.	*/
	/****************************************************************/

	{   0,	/* Obj_Sf_Actual_Arg		Name_Variable		*/
	    0,	/* Obj_Sf_Actual_Arg		Name_Common_Obj		*/
	    0,	/* Obj_Sf_Actual_Arg		Name_Cri_Pointer	*/
	    0,	/* Obj_Sf_Actual_Arg		Name_Cri_Pointee	*/
	    0,	/* Obj_Sf_Actual_Arg		Name_Cri_Ch_Pointee	*/
	    0,	/* Obj_Sf_Actual_Arg		Name_Func_Result	*/
	    0,	/* Obj_Sf_Actual_Arg		Name_Dummy_Arg		*/
	  760,	/* Obj_Sf_Actual_Arg		Name_Module_Proc	*/
	  760,	/* Obj_Sf_Actual_Arg		Name_Derived_Type	*/
	  760,	/* Obj_Sf_Actual_Arg		Name_Generic_Interface	*/
	  760,	/* Obj_Sf_Actual_Arg		Name_Namelist_Group	*/
	    0,	/* Obj_Sf_Actual_Arg		Name_Namelist_Group_Obj	*/
	  760,	/* Obj_Sf_Actual_Arg		Name_Statement_Func	*/
	  760,	/* Obj_Sf_Actual_Arg		Name_Construct		*/
	  760,	/* Obj_Sf_Actual_Arg		Name_Intrinsic_Func	*/
	  760,	/* Obj_Sf_Actual_Arg		Name_Intrinsic_Subr	*/
	  760,	/* Obj_Sf_Actual_Arg		Name_Module		*/
	  760,	/* Obj_Sf_Actual_Arg		Name_Blockdata		*/
	  760,	/* Obj_Sf_Actual_Arg		Name_Program		*/
	  760,	/* Obj_Sf_Actual_Arg		Name_Function		*/
	  760,	/* Obj_Sf_Actual_Arg		Name_Curr_Func		*/
	  760,	/* Obj_Sf_Actual_Arg		Name_Curr_Subr		*/
	  760,	/* Obj_Sf_Actual_Arg		Name_Internal_Func	*/
	  760	/* Obj_Sf_Actual_Arg		Name_Internal_Subr	*/
	},


	/************************ Obj_Var_Len_Ch ************************/
	/* This is a variable length character (automatic or		*/
	/* adjustable).	This is not called until the length has been	*/
	/* resolved at the end of pass1.  If the length resolves to a	*/
	/* constant, fnd_semantic_err is not called.	If it resolves	*/
	/* to a non-constant, fnd_semantic_err is called with this.	*/
	/* (5.1 discussion)	The specification-expr of a type-param-	*/
	/* value may be a nonconstant expression provided the		*/
	/* specification expression is in an interface body or in the	*/
	/* specification part of a subprogram.	If the data object	*/
	/* being declared depends on the value of such a nonconstant	*/
	/* expression and is not a dummy argument, such an object is	*/
	/* called an automatic data object.  An automatic data object	*/
	/* must not appear in a SAVE or DATA statement nor be declared  */
	/* with a SAVE attribute nor be initially defined by an =	*/
	/* initialization-expr.						*/
	/****************************************************************/

	{    0,	/* Obj_Var_Len_Ch		Name_Variable		*/

	  577,	/* Obj_Var_Len_Ch		Name_Common_Obj		*/
		/*   5.5.2						*/

	  577,	/* Obj_Var_Len_Ch		Name_Cri_Pointer	*/
# if defined(_EXTENDED_CRI_CHAR_POINTER)
	    0,	/* Obj_Var_Len_Ch		Name_Cri_Pointee	*/
	    0,	/* Obj_Var_Len_Ch		Name_Cri_Ch_Pointee	*/
# else
	  577,	/* Obj_Var_Len_Ch		Name_Cri_Pointee	*/
	  577,	/* Obj_Var_Len_Ch		Name_Cri_Ch_Pointee	*/
# endif
		/* Cray extensions					*/

	    0,	/* Obj_Var_Len_Ch		Name_Func_Result	*/
	    0,	/* Obj_Var_Len_Ch		Name_Dummy_Arg		*/

	  577,	/* Obj_Var_Len_Ch		Name_Module_Proc	*/
		/* 12.3.1 says that a module procedure has an explicit  */
		/* interface.	12.3.2.1 says that a procedure must not */
		/* have more than one explicit specific interface in	*/
		/* a given scoping unit.  12.3 details what is in an	*/
		/* interface.						*/

	  577,	/* Obj_Var_Len_Ch		Name_Derived_Type	*/
	  577,	/* Obj_Var_Len_Ch		Name_Generic_Interface	*/

	  577,	/* Obj_Var_Len_Ch		Name_Namelist_Group	*/
		/* 14.1.2						*/

	  577,	/* Obj_Var_Len_Ch		Name_Namelist_Group_Obj	*/
		/*   5.4						*/

	  577,	/* Obj_Var_Len_Ch		Name_Statement_Func	*/
		/* 12.  5.4 constraint - Must be fixed length character.*/

	  577,	/* Obj_Var_Len_Ch		Name_Construct		*/
		/* 14.1.2						*/

	    0,	/* Obj_Var_Len_Ch		Name_Intrinsic_Func	*/
	  577,	/* Obj_Var_Len_Ch		Name_Intrinsic_Subr	*/

	  577,	/* Obj_Var_Len_Ch		Name_Module		*/
	  577,	/* Obj_Var_Len_Ch		Name_Blockdata		*/
	  577,	/* Obj_Var_Len_Ch		Name_Program		*/
		/* 14.1.2						*/

	    0,	/* Obj_Var_Len_Ch		Name_Function		*/
	    0,	/* Obj_Var_Len_Ch		Name_Curr_Func		*/

	  577,	/* Obj_Var_Len_Ch		Name_Curr_Subr		*/
	  577,	/* Obj_Var_Len_Ch		Name_Internal_Func	*/
	  577	/* Obj_Var_Len_Ch		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Var_Len_Arr ***********************/
	/* This is an explicit-shape array.	It can be adjustable or	*/
	/* automatic.	It is not known that this is a variable length	*/
	/* array until the end of pass1.  These originally look like	*/
	/* an explicit shape array with unknown size.			*/
	/* This must be a dummy argument, a function result, or an	*/
	/* automatic object.						*/
	/****************************************************************/

	{   0,	/* Obj_Var_Len_Arr		Name_Variable		*/
	  583,	/* Obj_Var_Len_Arr		Name_Common_Obj		*/
		/*   5.5.2						*/
	  583,	/* Obj_Var_Len_Arr		Name_Cri_Pointer	*/
		/* Cray extension - illegal to be an array.		*/

	    0,	/* Obj_Var_Len_Arr		Name_Cri_Pointee	*/
# if defined(_EXTENDED_CRI_CHAR_POINTER)
	    0,	/* Obj_Var_Len_Arr		Name_Cri_Ch_Pointee	*/
# else
	  583,	/* Obj_Var_Len_Arr		Name_Cri_Ch_Pointee	*/
		/* Cray extension - illegal to be an array.		*/
# endif
		 
	    0,	/* Obj_Var_Len_Arr		Name_Func_Result	*/
	    0,	/* Obj_Var_Len_Arr		Name_Dummy_Arg		*/
	  583,	/* Obj_Var_Len_Arr		Name_Module_Proc	*/
		/* 12.3.1 says that a module procedure has an explicit  */
		/* interface.	12.3.2.1 says that a procedure must not */
		/* have more than one explicit specific interface in	*/
		/* a given scoping unit.  12.3 details what is in an	*/
		/* interface.					 	*/

	  583,	/* Obj_Var_Len_Arr		Name_Derived_Type	*/
	  583,	/* Obj_Var_Len_Arr		Name_Generic_Interface	*/
	  583,	/* Obj_Var_Len_Arr		Name_Namelist_Group	*/
		/* 14.1.2						*/

	  583,	/* Obj_Var_Len_Arr		Name_Namelist_Group_Obj	*/
		/*   5.4						*/

	  583,	/* Obj_Var_Len_Arr		Name_Statement_Func	*/
		/* 12.  5.4 constraint - Must be scalar.		*/

	  583,	/* Obj_Var_Len_Arr		Name_Construct		*/
	  583,	/* Obj_Var_Len_Arr		Name_Intrinsic_Func	*/
	  583,	/* Obj_Var_Len_Arr		Name_Intrinsic_Subr	*/
	  583,	/* Obj_Var_Len_Arr		Name_Module		*/
	  583,	/* Obj_Var_Len_Arr		Name_Blockdata		*/
	  583,	/* Obj_Var_Len_Arr		Name_Program		*/
		/* 14.1.2						*/

	    0,	/* Obj_Var_Len_Arr		Name_Function		*/
	    0,	/* Obj_Var_Len_Arr		Name_Curr_Func		*/

	  583,	/* Obj_Var_Len_Arr		Name_Curr_Subr		*/
	  583,	/* Obj_Var_Len_Arr		Name_Internal_Func	*/
	  583 	/* Obj_Var_Len_Arr		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},

	/************************ Obj_Sym_Constant_Arr ******************/
	/* A symbolic constant array is an array where at least one of	*/
	/* the bounds contains a symbolic constant expression.	 	*/
	/* At end pass1, array bounds are resolved.			*/
	/* If they are constant, semantic checking is done, if they	*/
	/* have symbolic constant bounds, fnd_semantic_err is called	*/
	/* again for the object, using Obj_Sym_Constant_Arr.	There   */
	/* is nothing that can be a symbolic constant array that cannot	*/
	/* be an array with constant bounds.  This is a Cray extension. */
	/****************************************************************/

	{   0,	/* Obj_Sym_Constant_Arr		Name_Variable		*/
	    0,	/* Obj_Sym_Constant_Arr		Name_Common_Obj		*/
	 1224,	/* Obj_Sym_Constant_Arr		Name_Cri_Pointer	*/
	    0,	/* Obj_Sym_Constant_Arr		Name_Cri_Pointee	*/
	    0,	/* Obj_Sym_Constant_Arr		Name_Cri_Ch_Pointee	*/
	    0,	/* Obj_Sym_Constant_Arr		Name_Func_Result	*/
	    0,	/* Obj_Sym_Constant_Arr		Name_Dummy_Arg		*/
	 1224,	/* Obj_Sym_Constant_Arr		Name_Module_Proc	*/
	 1224,	/* Obj_Sym_Constant_Arr		Name_Derived_Type	*/
	 1224,	/* Obj_Sym_Constant_Arr		Name_Generic_Interface	*/
	 1224,	/* Obj_Sym_Constant_Arr		Name_Namelist_Group	*/
	 1224,	/* Obj_Sym_Constant_Arr		Name_Namelist_Group_Obj	*/
	 1224,	/* Obj_Sym_Constant_Arr		Name_Statement_Func	*/
	 1224,	/* Obj_Sym_Constant_Arr		Name_Construct		*/
	 1224,	/* Obj_Sym_Constant_Arr		Name_Intrinsic_Func	*/
	 1224,	/* Obj_Sym_Constant_Arr		Name_Intrinsic_Subr	*/
	 1224,	/* Obj_Sym_Constant_Arr		Name_Module		*/
	 1224,	/* Obj_Sym_Constant_Arr		Name_Blockdata		*/
	 1224,	/* Obj_Sym_Constant_Arr		Name_Program		*/
	    0,	/* Obj_Sym_Constant_Arr		Name_Function		*/
	    0,	/* Obj_Sym_Constant_Arr		Name_Curr_Func		*/
	 1224,	/* Obj_Sym_Constant_Arr		Name_Curr_Subr		*/
	 1224,	/* Obj_Sym_Constant_Arr		Name_Internal_Func	*/
	 1224 	/* Obj_Sym_Constant_Arr		Name_Internal_Subr	*/
	},

	/************************ Obj_Interface_Func ********************/
	/* This is the name of the FUNCTION on an interface body.	*/
	/* 12.3.2.1 states that a procedure must not have more than	*/
	/* one explicit interface in a scoping unit.	12.3.1 states	*/
	/* that specifying an external or dummy procedure in an		*/
	/* interface block, causes it to have an explicit interface.	*/
	/* 12.3 defines an interface as the characteristics of the	*/
	/* procedure, its dummy arguments and its function result (if	*/
	/* it's a function).  Since the actual definition of the module */
	/* procedure is the explicit interface, it's characteristics	*/
	/* cannot be defined outside of the module procedure		*/
	/* definition.	Also 5.1 states that an entity must not be	*/
	/* given any attribute more than once in a scoping unit.	*/
	/****************************************************************/

	{ 609,	/* Obj_Interface_Func		Name_Variable		*/
	  609,	/* Obj_Interface_Func		Name_Common_Obj		*/
	  609,	/* Obj_Interface_Func		Name_Cri_Pointer	*/
	  609,	/* Obj_Interface_Func		Name_Cri_Pointee	*/
	  609,	/* Obj_Interface_Func		Name_Cri_Ch_Pointee	*/
		/* Cray extensions					*/

	  609,	/* Obj_Interface_Func		Name_Func_Result	*/
		/* 14.1.2						*/

	    0,	/* Obj_Interface_Func		Name_Dummy_Arg		*/

	  609,	/* Obj_Interface_Func		Name_Module_Proc	*/
		/* 12.3.1 says that a module procedure has an explicit  */
		/* interface.	12.3.2.1 says that a procedure must not */
		/* have more than one explicit specific interface in	*/
		/* a given scoping unit.  12.3 details what is in an	*/
		/* interface.					 	*/

	  609,	/* Obj_Interface_Func		Name_Derived_Type	*/
		/* 14.1.2						*/

	  609,	/* Obj_Interface_Func		Name_Generic_Interface	*/
		/* 12.3.2.1 discussion	It can be the same name as	*/
		/* the generic interface name, but it has to be the	*/
		/* current one.	This is handled in start_new_subpgm	*/

	  609,	/* Obj_Interface_Func		Name_Namelist_Group	*/
	  609,	/* Obj_Interface_Func		Name_Namelist_Group_Obj	*/
	  609,	/* Obj_Interface_Func		Name_Statement_Func	*/
	  609,	/* Obj_Interface_Func		Name_Construct		*/
	    0,	/* Obj_Interface_Func		Name_Intrinsic_Func	*/
	  609,	/* Obj_Interface_Func		Name_Intrinsic_Subr	*/
	  609,	/* Obj_Interface_Func		Name_Module		*/
	  609,	/* Obj_Interface_Func		Name_Blockdata		*/
	  609,	/* Obj_Interface_Func		Name_Program		*/
	    0,	/* Obj_Interface_Func		Name_Function		*/
	  609,	/* Obj_Interface_Func		Name_Curr_Func		*/
	  609,	/* Obj_Interface_Func		Name_Curr_Subr		*/
	  609,	/* Obj_Interface_Func		Name_Internal_Func	*/
	  609	/* Obj_Interface_Func		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},

	/************************ Obj_Interface_Subr ********************/
	/* This is the name of the SUBROUTINE on an interface body.	*/
	/* This may be a dummy procedure	(12.3.2.1) discussion	*/
	/* Subroutines can't be typed, shaped or anything else.		*/
	/****************************************************************/

	{ 619,	/* Obj_Interface_Subr		Name_Variable		*/
	  619,	/* Obj_Interface_Subr		Name_Common_Obj		*/
	  619,	/* Obj_Interface_Subr		Name_Cri_Pointer	*/
	  619,	/* Obj_Interface_Subr		Name_Cri_Pointee	*/
	  619,	/* Obj_Interface_Subr		Name_Cri_Ch_Pointee	*/
		/* Cray extensions					*/

	  619,	/* Obj_Interface_Subr		Name_Func_Result	*/
		/* 14.1.2						*/

	    0,	/* Obj_Interface_Subr		Name_Dummy_Arg		*/

	  619,	/* Obj_Interface_Subr		Name_Module_Proc	*/
		/* 12.3.1 says that a module procedure has an explicit	*/
		/* interface.	12.3.2.1 says that a procedure must not	*/
		/* have more than one explicit specific interface in	*/
		/* a given scoping unit.  12.3 details what is in an	*/
		/* interface.					 	*/

	  619,	/* Obj_Interface_Subr		Name_Derived_Type	*/
		/* 14.1.2						*/

	  619,	/* Obj_Interface_Subr		Name_Generic_Interface	*/
		/* 12.3.2.1 discussion	It can be the same name as	*/
		/* the generic interface name, but it has to be the	*/
		/* current one.	This is handled in start_new_subpgm	*/

	  619,	/* Obj_Interface_Subr		Name_Namelist_Group	*/
	  619,	/* Obj_Interface_Subr		Name_Namelist_Group_Obj	*/
	  619,	/* Obj_Interface_Subr		Name_Statement_Func	*/
	  619,	/* Obj_Interface_Subr		Name_Construct		*/
	  619,	/* Obj_Interface_Subr		Name_Intrinsic_Func	*/
	    0,	/* Obj_Interface_Subr		Name_Intrinsic_Subr	*/
	  619,	/* Obj_Interface_Subr		Name_Module		*/
	  619,	/* Obj_Interface_Subr		Name_Blockdata		*/
	  619,	/* Obj_Interface_Subr		Name_Program		*/
	  619,	/* Obj_Interface_Subr		Name_Function		*/
	  619,	/* Obj_Interface_Subr		Name_Curr_Func		*/
	  619,	/* Obj_Interface_Subr		Name_Curr_Subr		*/
	  619,	/* Obj_Interface_Subr		Name_Internal_Func	*/
	  619	/* Obj_Interface_Subr		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Use_Extern_Func *******************/
	/* This is an external function in an expression.		*/
	/****************************************************************/

	{ 629,	/* Obj_Use_Extern_Func		Name_Variable		*/
 	  629,	/* Obj_Use_Extern_Func		Name_Common_Obj		*/
	  629,	/* Obj_Use_Extern_Func		Name_Cri_Pointer	*/
	  629,	/* Obj_Use_Extern_Func		Name_Cri_Pointee	*/
	  629,	/* Obj_Use_Extern_Func		Name_Cri_Ch_Pointee	*/
	  629,	/* Obj_Use_Extern_Func		Name_Func_Result	*/
		/* 14.1.2						*/

	    0,	/* Obj_Use_Extern_Func		Name_Dummy_Arg		*/
	    0,	/* Obj_Use_Extern_Func		Name_Module_Proc	*/

	  629,	/* Obj_Use_Extern_Func		Name_Derived_Type	*/
		/* 14.1.2						*/

	    0,	/* Obj_Use_Extern_Func		Name_Generic_Interface	*/
		/* Can also be a function 12.3.2.1			*/

	  629,	/* Obj_Use_Extern_Func		Name_Namelist_Group	*/
	  629,	/* Obj_Use_Extern_Func		Name_Namelist_Group_Obj	*/
	  629,	/* Obj_Use_Extern_Func		Name_Statement_Func	*/
	  629,	/* Obj_Use_Extern_Func		Name_Construct		*/
	    0,	/* Obj_Use_Extern_Func		Name_Intrinsic_Func	*/
	  629,	/* Obj_Use_Extern_Func		Name_Intrinsic_Subr	*/
	  629,	/* Obj_Use_Extern_Func		Name_Module		*/
	  629,	/* Obj_Use_Extern_Func		Name_Blockdata		*/
	  629,	/* Obj_Use_Extern_Func		Name_Program		*/
		/* 14.1.2						*/

	    0,	/* Obj_Use_Extern_Func		Name_Function		*/
	    0,	/* Obj_Use_Extern_Func		Name_Curr_Func		*/

	  629,	/* Obj_Use_Extern_Func		Name_Curr_Subr		*/
	  629,	/* Obj_Use_Extern_Func		Name_Internal_Func	*/
	  629	/* Obj_Use_Extern_Func		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Use_Extern_Subr *******************/
	/* This is an external subroutine in an expression.		*/
	/* Subroutines can't be typed, ranked or anything else.		*/
	/****************************************************************/

	{ 634,	/* Obj_Use_Extern_Subr		Name_Variable		*/
	  634,	/* Obj_Use_Extern_Subr		Name_Common_Obj		*/
	  634,	/* Obj_Use_Extern_Subr		Name_Cri_Pointer	*/
	  634,	/* Obj_Use_Extern_Subr		Name_Cri_Pointee	*/
	  634,	/* Obj_Use_Extern_Subr		Name_Cri_Ch_Pointee	*/
	  634,	/* Obj_Use_Extern_Subr		Name_Func_Result	*/
		/* 14.1.2						*/

	    0,	/* Obj_Use_Extern_Subr		Name_Dummy_Arg		*/
	    0,	/* Obj_Use_Extern_Subr		Name_Module_Proc	*/

	  634,	/* Obj_Use_Extern_Subr		Name_Derived_Type	*/
		/* 14.1.2						*/

	    0,	/* Obj_Use_Extern_Subr		Name_Generic_Interface	*/
		/* Can also be a subroutine 12.3.2.1			*/

	  634,	/* Obj_Use_Extern_Subr		Name_Namelist_Group	*/
	  634,	/* Obj_Use_Extern_Subr		Name_Namelist_Group_Obj	*/
	  634,	/* Obj_Use_Extern_Subr		Name_Statement_Func	*/
	  634,	/* Obj_Use_Extern_Subr		Name_Construct		*/
	  634,	/* Obj_Use_Extern_Subr		Name_Intrinsic_Func	*/
	    0,	/* Obj_Use_Extern_Subr		Name_Intrinsic_Subr	*/
	  458,	/* Obj_Use_Extern_Subr		Name_Module		*/
	  457,	/* Obj_Use_Extern_Subr		Name_Blockdata		*/
	  456,	/* Obj_Use_Extern_Subr		Name_Program		*/
	  335,	/* Obj_Use_Extern_Subr		Name_Function		*/
	  335,	/* Obj_Use_Extern_Subr		Name_Curr_Func		*/
		/* 14.1.2						*/

	    0,	/* Obj_Use_Extern_Subr		Name_Curr_Subr		*/

	  634,	/* Obj_Use_Extern_Subr		Name_Internal_Func	*/
	  634	/* Obj_Use_Extern_Subr		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Use_In_Expr ***********************/
	/*	This is something referenced in an expression.		*/
	/****************************************************************/

	{   0,	/* Obj_Use_In_Expr		Name_Variable		*/
	    0,	/* Obj_Use_In_Expr		Name_Common_Obj		*/
	    0,	/* Obj_Use_In_Expr		Name_Cri_Pointer	*/
	    0,	/* Obj_Use_In_Expr		Name_Cri_Ch_Pointee	*/
	    0,	/* Obj_Use_In_Expr		Name_Cri_Pointee	*/
	    0,	/* Obj_Use_In_Expr		Name_Func_Result	*/
	    0,	/* Obj_Use_In_Expr		Name_Dummy_Arg		*/
	    0,	/* Obj_Use_In_Expr		Name_Module_Proc	*/

	  641,	/* Obj_Use_In_Expr		Name_Derived_Type	*/
		/* 14.1.2						*/

	    0,	/* Obj_Use_In_Expr		Name_Generic_Interface	*/

	  641,	/* Obj_Use_In_Expr		Name_Namelist_Group	*/
		/* 14.1.2						*/

	    0,	/* Obj_Use_In_Expr		Name_Namelist_Group_Obj	*/
	    0,	/* Obj_Use_In_Expr		Name_Statement_Func	*/

	  641,	/* Obj_Use_In_Expr		Name_Construct		*/
		/* 14.1.2						*/

	    0,	/* Obj_Use_In_Expr		Name_Intrinsic_Func	*/
	  641,	/* Obj_Use_In_Expr		Name_Intrinsic_Subr	*/

	  641,	/* Obj_Use_In_Expr		Name_Module		*/
	  641,	/* Obj_Use_In_Expr		Name_Blockdata		*/
	  641,	/* Obj_Use_In_Expr		Name_Program		*/
		/* 14.1.2						*/

	    0,	/* Obj_Use_In_Expr		Name_Function		*/
	    0,	/* Obj_Use_In_Expr		Name_Curr_Func		*/

	  641,	/* Obj_Use_In_Expr		Name_Curr_Subr		*/
		/* 14.1.2						*/

	    0, 	/* Obj_Use_In_Expr		Name_Internal_Func	*/

	  641	/* Obj_Use_In_Expr		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Use_Derived_Type ******************/
	/* This is a name used to type something as a derived type.	*/
	/* Derived types can't be variables, constants, functions,	*/
	/* or dummy args. (14.1.2)					*/
	/****************************************************************/

	{ 645,	/* Obj_Use_Derived_Type		Name_Variable		*/
	  645,	/* Obj_Use_Derived_Type		Name_Common_Obj		*/
	  645,	/* Obj_Use_Derived_Type		Name_Cri_Pointer	*/
	  645,	/* Obj_Use_Derived_Type		Name_Cri_Pointee	*/
	  645,	/* Obj_Use_Derived_Type		Name_Cri_Ch_Pointee	*/
	  645,	/* Obj_Use_Derived_Type		Name_Func_Result	*/
	  645,	/* Obj_Use_Derived_Type		Name_Dummy_Arg		*/
	  645,	/* Obj_Use_Derived_Type		Name_Module_Proc	*/
		/* 14.1.2						*/

	    0,	/* Obj_Use_Derived_Type		Name_Derived_Type	*/

	  645,	/* Obj_Use_Derived_Type		Name_Generic_Interface	*/
	  645,	/* Obj_Use_Derived_Type		Name_Namelist_Group	*/
	  645,	/* Obj_Use_Derived_Type		Name_Namelist_Group_Obj	*/
	  645,	/* Obj_Use_Derived_Type		Name_Statement_Func	*/
	  645,	/* Obj_Use_Derived_Type		Name_Construct		*/
	  645,	/* Obj_Use_Derived_Type		Name_Intrinsic_Func	*/
	  645,	/* Obj_Use_Derived_Type		Name_Intrinsic_Subr	*/
	  645,	/* Obj_Use_Derived_Type		Name_Module		*/
	  645,	/* Obj_Use_Derived_Type		Name_Blockdata		*/
	  645,	/* Obj_Use_Derived_Type		Name_Program		*/
	  645,	/* Obj_Use_Derived_Type		Name_Function		*/
	  645,	/* Obj_Use_Derived_Type		Name_Curr_Func		*/
	  645,	/* Obj_Use_Derived_Type		Name_Curr_Subr		*/
	  645,	/* Obj_Use_Derived_Type		Name_Internal_Func	*/
	  645	/* Obj_Use_Derived_Type		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},


	/************************ Obj_Use_Spec_Expr *********************/
	/* The rules for a specification expression are as follows:	*/
	/* Each object must be a constant, a dummy arg (that doesn't	*/
	/* have the OPTIONAL or the INTENT (OUT) attributes), in a	*/
	/* common block, accessible by USE or HOST association.		*/
	/* Specific rules are found in 7.1.6.2				*/
	/****************************************************************/

	{   0,	/* Obj_Use_Spec_Expr		Name_Variable		*/
	    0,	/* Obj_Use_Spec_Expr		Name_Common_Obj		*/
	  509,	/* Obj_Use_Spec_Expr		Name_Cri_Pointer	*/
	  509,	/* Obj_Use_Spec_Expr		Name_Cri_Pointee	*/
	  509,	/* Obj_Use_Spec_Expr		Name_Cri_Ch_Pointee	*/
	  509,	/* Obj_Use_Spec_Expr		Name_Func_Result	*/
	    0,	/* Obj_Use_Spec_Expr		Name_Dummy_Arg		*/
	  509,	/* Obj_Use_Spec_Expr		Name_Module_Proc	*/

	  509,	/* Obj_Use_Spec_Expr		Name_Derived_Type	*/
		/* 14.1.2						*/

	  509,	/* Obj_Use_Spec_Expr		Name_Generic_Interface	*/

	  509,	/* Obj_Use_Spec_Expr		Name_Namelist_Group	*/
		/* 14.1.2						*/

	    0,	/* Obj_Use_Spec_Expr		Name_Namelist_Group	*/

	  509,	/* Obj_Use_Spec_Expr		Name_Statement_Func	*/

	  509,	/* Obj_Use_Spec_Expr		Name_Construct		*/
		/* 14.1.2						*/

	    0,	/* Obj_Use_Spec_Expr		Name_Intrinsic_Func	*/
	  509,	/* Obj_Use_Spec_Expr		Name_Intrinsic_Subr	*/

	  509,	/* Obj_Use_Spec_Expr		Name_Module		*/
	  509,	/* Obj_Use_Spec_Expr		Name_Blockdata		*/
	  509,	/* Obj_Use_Spec_Expr		Name_Program		*/
		/* 14.1.2						*/

	    0,	/* Obj_Use_Spec_Expr		Name_Function		*/
	  259,	/* Obj_Use_Spec_Expr		Name_Curr_Func		*/
		/* More descriptive msg, but it matches output for 509	*/

	  509,	/* Obj_Use_Spec_Expr		Name_Curr_Subr		*/
		/* 14.1.2						*/

	  509, 	/* Obj_Use_Spec_Expr		Name_Internal_Func	*/

	  509	/* Obj_Use_Spec_Expr		Name_Internal_Subr	*/
		/* 14.1.2						*/
	},

	/********************** Obj_Use_Init_Expr ***********************/
	/* The rules for an initialization expression are as follows:	*/
	/* It MUST be a CONSTANT.  It may be one of several INTRINSICS	*/
	/* that are foldable.	(7.1.6.1)				*/
	/* These are all illegal, because of 14.1.2.			*/
	/****************************************************************/

	{   0,	/* Obj_Use_Init_Expr		Name_Variable		*/
	  276,	/* Obj_Use_Init_Expr		Name_Common_Obj		*/
	  276,	/* Obj_Use_Init_Expr		Name_Cri_Pointer	*/
	  276,	/* Obj_Use_Init_Expr		Name_Cri_Pointee	*/
	  276,	/* Obj_Use_Init_Expr		Name_Cri_Ch_Pointee	*/
	  276,	/* Obj_Use_Init_Expr		Name_Func_Result	*/
	  276,	/* Obj_Use_Init_Expr		Name_Dummy_Arg		*/
	  276,	/* Obj_Use_Init_Expr		Name_Module_Proc	*/
	  276,	/* Obj_Use_Init_Expr		Name_Derived_Type	*/
	  276,	/* Obj_Use_Init_Expr		Name_Generic_Interface	*/
	  276,	/* Obj_Use_Init_Expr		Name_Namelist_Group	*/
	  276,	/* Obj_Use_Init_Expr		Name_Namelist_Group_Obj	*/
	  276,	/* Obj_Use_Init_Expr		Name_Statement_Func	*/
	  276,	/* Obj_Use_Init_Expr		Name_Construct		*/

	    0,	/* Obj_Use_Init_Expr		Name_Intrinsic_Func	*/
	  276,	/* Obj_Use_Init_Expr		Name_Intrinsic_Subr	*/

		/* It can be INTRINSIC, because foldable intrinsics	*/
		/* are allowed through.					*/

	  276,	/* Obj_Use_Init_Expr		Name_Module		*/
	  276,	/* Obj_Use_Init_Expr		Name_Blockdata		*/
	  276,	/* Obj_Use_Init_Expr		Name_Program		*/
	    0,	/* Obj_Use_Init_Expr		Name_Function		*/
	  276,	/* Obj_Use_Init_Expr		Name_Curr_Func		*/
	  276,	/* Obj_Use_Init_Expr		Name_Curr_Subr		*/
	  276, 	/* Obj_Use_Init_Expr		Name_Internal_Func	*/
	  276	/* Obj_Use_Init_Expr		Name_Internal_Subr	*/
	}

};



long	other_msg_num[Obj_Done] [Other_Done]	= {
	

	/********************** Obj_Assum_Type_Ch ***********************/
	/* A length type value of star may only be used as a dummy	*/
	/* argument, a constant, or the name of the function result,	*/
	/* in the external function begin compiled.	(5.1.1.5)	*/
	/****************************************************************/

	{   0,	/* Obj_Assum_Type_Ch	 	Other_Var_Len_Ch	*/
		/* Something can be implicitly typed as a var length	*/
		/* char and then appear in a character statement.(5.3)	*/

	    0,	/* Obj_Assum_Type_Ch	 	Other_Var_Len_Arr	*/
		/* Cannot be a darg, func result or constant. (5.5.2)	*/

	  566,	/* Obj_Assum_Type_Ch	 	Other_Expl_Interface	*/
		/* Only current external function being compiled can	*/
		/* be assumed type character.			 */

	    0,	/* Obj_Assum_Type_Ch	 	Other_Use_Func		*/
		/* Only the current program unit, can be typed as	*/
		/* assumed type character.	Any other function must	*/
		/* have a valid character length.			*/

	  574,	/* Obj_Assum_Type_Ch	 	Other_Use_Subr		*/
	    0,	/* Obj_Assum_Type_Ch	 	Other_Use_Variable	*/
	    0,	/* Obj_Assum_Type_Ch	 	Other_Use_Dummy_Arg	*/
		/* Merge type gets this one.				*/

	  920,	/* Obj_Assum_Type_Ch		Other_Host_Assoc	*/
	  922, 	/* Obj_Assum_Type_Ch		Other_Use_Assoc		*/
	    0, 	/* Obj_Assum_Type_Ch		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Assum_Type_Ch		Other_Not_Visible	*/
	 1134	/* Obj_Assum_Type_Ch		Other_Npes		*/

	},


	/************************ Obj_Expl_Shp_Arr **********************/
	/* An explict shape array is a named array that is declared	*/
	/* with explicit values for the bounds in each dimension of	*/
	/* the array.	(5.1.2.4.1)	All explicit shape arrays (both	*/
	/* with constant bounds and with non-constant bounds), use this	*/
	/* Obj_... category.	At end pass1, array bounds are resolved.*/
	/* If they are constant, semantic checking is done, if they are	*/
	/* non-constant, fnd_semantic_err is called again for the	*/
	/* object, using Obj_Var_Len_Arr.  There is nothing that can	*/
	/* be a variable length array that cannot be an array with	*/
	/* constant bounds.						*/
	/****************************************************************/

	{   0,	/* Obj_Expl_Shp_Arr		Other_Var_Len_Ch	*/

	  562,	/* Obj_Expl_Shp_Arr		Other_Var_Len_Arr	*/
		/* Can't give the DIMENSION attribute twice.	(5.1)	*/

	  566,	/* Obj_Expl_Shp_Arr		Other_Expl_Interface	*/
		/* Cannot add something to an explicit interface.	*/

	  572,	/* Obj_Expl_Shp_Arr		Other_Use_Func		*/
	  574,	/* Obj_Expl_Shp_Arr		Other_Use_Subr		*/
	  559,	/* Obj_Expl_Shp_Arr		Other_Use_Variable	*/
	 1039,	/* Obj_Expl_Shp_Arr		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  920,	/* Obj_Expl_Shp_Arr		Other_Host_Assoc	*/
	  922, 	/* Obj_Expl_Shp_Arr		Other_Use_Assoc		*/
	    0, 	/* Obj_Expl_Shp_Arr		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Expl_Shp_Arr		Other_Not_Visible	*/
	 1134	/* Obj_Expl_Shp_Arr		Other_Npes		*/
	},


	/************************ Obj_Assum_Size_Arr ********************/
	/* This is a dummy argument array.	(5.1.2.4.4)		*/
	/****************************************************************/

	{   0,	/* Obj_Assum_Size_Arr		Other_Var_Len_Ch	*/

	  562,	/* Obj_Assum_Size_Arr		Other_Var_Len_Arr	*/
		/* Illegal - DIMENSION attribute twice.	(5.1)		*/

	  566,	/* Obj_Assum_Size_Arr		Other_Expl_Interface	*/
		/* Can't add things to something with an explicit itrf	*/

	  572,	/* Obj_Assum_Size_Arr		Other_Use_Func		*/
	  574,	/* Obj_Assum_Size_Arr		Other_Use_Subr		*/
	  559,	/* Obj_Assum_Size_Arr		Other_Use_Variable	*/
	 1039,	/* Obj_Assum_Size_Arr		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  920,	/* Obj_Assum_Size_Arr		Other_Host_Assoc	*/
	  922, 	/* Obj_Assum_Size_Arr		Other_Use_Assoc		*/
	    0, 	/* Obj_Assum_Size_Arr		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Assum_Size_Arr		Other_Not_Visible	*/
	 1134	/* Obj_Assum_Size_Arr		Other_Npes		*/
	},


	/************************ Obj_Defrd_Shp_Arr *********************/
	/* A deferred shape array is an array pointer or an		*/
	/* allocatable array. (5.1.2.4.3)				*/
	/****************************************************************/

	{   0,	/* Obj_Defrd_Shp_Arr	 	Other_Var_Len_Ch	*/

	  562,	/* Obj_Defrd_Shp_Arr	 	Other_Var_Len_Arr	*/
		/* Can't give the DIMENSION attribute twice.	(5.1)	*/

	  566,	/* Obj_Defrd_Shp_Arr	 	Other_Expl_Interface	*/

	  572,	/* Obj_Defrd_Shp_Arr	 	Other_Use_Func		*/
	  574,	/* Obj_Defrd_Shp_Arr	 	Other_Use_Subr		*/
	  559,	/* Obj_Defrd_Shp_Arr		Other_Use_Variable	*/
	 1039,	/* Obj_Defrd_Shp_Arr		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  920,	/* Obj_Defrd_Shp_Arr		Other_Host_Assoc	*/
	  922, 	/* Obj_Defrd_Shp_Arr		Other_Use_Assoc		*/
	    0, 	/* Obj_Defrd_Shp_Arr		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Defrd_Shp_Arr		Other_Not_Visible	*/
	 1134	/* Obj_Defrd_Shp_Arr		Other_Npes		*/
	},


	/************************ Obj_Assum_Shp_Arr *********************/
	/* This is a non-pointer dummy argument array.	(5.1.2.4.2)	*/
	/****************************************************************/

	{   0,	/* Obj_Assum_Shp_Arr	 	Other_Var_Len_Ch	*/

	  562,	/* Obj_Assum_Shp_Arr	 	Other_Var_Len_Arr	*/
		/* Can't give the DIMENSION attribute twice.	(5.1)	*/

	  566,	/* Obj_Assum_Shp_Arr	 	Other_Expl_Interface	*/

	  572,	/* Obj_Assum_Shp_Arr	 	Other_Use_Func		*/
	  574,	/* Obj_Assum_Shp_Arr	 	Other_Use_Subr		*/
	  559,	/* Obj_Assum_Shp_Arr		Other_Use_Variable	*/
	 1039,	/* Obj_Assum_Shp_Arr		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  920,	/* Obj_Assum_Shp_Arr		Other_Host_Assoc	*/
	  922, 	/* Obj_Assum_Shp_Arr		Other_Use_Assoc		*/
	    0, 	/* Obj_Assum_Shp_Arr		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Assum_Shp_Arr		Other_Not_Visible	*/
	 1134	/* Obj_Assum_Shp_Arr		Other_Npes		*/
	},

	/************************ Obj_Co_Array **********************/
	/* This is a pe dimension [3,4]					*/
	/****************************************************************/

	{   0,	/* Obj_Co_Array		Other_Var_Len_Ch	*/
	    0,	/* Obj_Co_Array		Other_Var_Len_Arr	*/
	  566,	/* Obj_Co_Array		Other_Expl_Interface	*/
	  572,	/* Obj_Co_Array		Other_Use_Func		*/
	  574,	/* Obj_Co_Array		Other_Use_Subr		*/
	    0,	/* Obj_Co_Array		Other_Use_Variable	*/
	    0,	/* Obj_Co_Array		Other_Use_Dummy_Arg	*/
	    0,	/* Obj_Co_Array		Other_Host_Assoc	*/
	    0,	/* Obj_Co_Array		Other_Use_Assoc		*/
	    0,	/* Obj_Co_Array		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Co_Array		Other_Not_Visible	*/
	 1134	/* Obj_Co_Array		Other_Npes		*/
	},


	/************************ Obj_Allocatable ***********************/
	/* This must be a deferred shape array and must not be a dummy  */
	/* argument or function result. (5.2.6)				*/
	/****************************************************************/

	{   0,	/* Obj_Allocatable		Other_Var_Len_Ch	*/

	  562,	/* Obj_Allocatable		Other_Var_Len_Arr	*/
		/* This must be a deferred shape array.	(5.2.6)		*/

	  566,	/* Obj_Allocatable		Other_Expl_Interface	*/

	  572,	/* Obj_Allocatable		Other_Use_Func		*/
	  574,	/* Obj_Allocatable		Other_Use_Subr		*/
	  559,	/* Obj_Allocatable		Other_Use_Variable	*/
	 1039,	/* Obj_Allocatable		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  920,	/* Obj_Allocatable		Other_Host_Assoc	*/
	  922, 	/* Obj_Allocatable		Other_Use_Assoc		*/
	    0, 	/* Obj_Allocatable		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Allocatable		Other_Not_Visible	*/
	 1134	/* Obj_Allocatable		Other_Npes		*/
	},

#ifdef KEY /* Bug 14150 */
	/************************ Obj_Bind        ***********************/
	/****************************************************************/

	{ 560,	/* Obj_Bind			Other_Var_Len_Ch	*/
	  562,	/* Obj_Bind			Other_Var_Len_Arr	*/
	  566,	/* Obj_Bind			Other_Expl_Interface	*/
	  572,	/* Obj_Bind			Other_Use_Func		*/
	  574,	/* Obj_Bind			Other_Use_Subr		*/
	  559,	/* Obj_Bind			Other_Use_Variable	*/
	 1039,	/* Obj_Bind			Other_Use_Dummy_Arg	*/
	  920,	/* Obj_Bind			Other_Host_Assoc	*/
	  922, 	/* Obj_Bind			Other_Use_Assoc		*/
	 1501, 	/* Obj_Bind			Other_Use_Char_Rslt	*/
	  486,	/* Obj_Bind			Other_Not_Visible	*/
	 1134	/* Obj_Bind			Other_Npes		*/
	},

	/************************ Obj_Value        ***********************/
	/****************************************************************/

	{ 560,	/* Obj_Value			Other_Var_Len_Ch	*/
	  562,	/* Obj_Value			Other_Var_Len_Arr	*/
	  566,	/* Obj_Value			Other_Expl_Interface	*/
	  572,	/* Obj_Value			Other_Use_Func		*/
	  574,	/* Obj_Value			Other_Use_Subr		*/
	  559,	/* Obj_Value			Other_Use_Variable	*/
	    0,	/* Obj_Value			Other_Use_Dummy_Arg	*/
	  920,	/* Obj_Value			Other_Host_Assoc	*/
	  922, 	/* Obj_Value			Other_Use_Assoc		*/
	 1501, 	/* Obj_Value			Other_Use_Char_Rslt	*/
	  486,	/* Obj_Value			Other_Not_Visible	*/
	 1134	/* Obj_Value			Other_Npes		*/
	},
#endif /* KEY Bug 14150 */



	/************************ Obj_Constant **************************/
	/* The named constant must have its type, shape and any type	*/
	/* parameters specified either by a previous occurrence in a	*/
	/* type declaration stmt in the same scoping unit, or by the	*/
	/* implicit typing rules in effect.	(5.2. 10)		*/
	/****************************************************************/

	{ 560,	/* Obj_Constant			Other_Var_Len_Ch	*/
	  562,	/* Obj_Constant			Other_Var_Len_Arr	*/
	  566,	/* Obj_Constant			Other_Expl_Interface	*/
	  572,	/* Obj_Constant			Other_Use_Func		*/
	  574,	/* Obj_Constant			Other_Use_Subr		*/
	    0,	/* Obj_Constant			Other_Use_Variable	*/
	 1039,	/* Obj_Constant			Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  920,	/* Obj_Constant			Other_Host_Assoc	*/
	  922, 	/* Obj_Constant			Other_Use_Assoc		*/
	    0, 	/* Obj_Constant			Other_Use_Char_Rslt	*/
	  486,	/* Obj_Constant			Other_Not_Visible	*/
	 1134	/* Obj_Constant			Other_Npes		*/
	},


	/************************ Obj_Intent ****************************/
	/* An intent statement may only appear in the specification-	*/
	/* part of a subprogram or interface body.  It must not be a	*/
	/* dummy procedure or dummy pointer.	(5.2.1)			*/
	/****************************************************************/

	{   0,	/* Obj_Intent			Other_Var_Len_Ch	*/
	    0,	/* Obj_Intent			Other_Var_Len_Arr	*/

	  566,	/* Obj_Intent			Other_Expl_Interface	*/
	  572,	/* Obj_Intent			Other_Use_Func		*/
	  574,	/* Obj_Intent			Other_Use_Subr		*/
	    0,	/* Obj_Intent			Other_Use_Variable	*/
	    0,	/* Obj_Intent			Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  920,	/* Obj_Intent			Other_Host_Assoc	*/
	  922, 	/* Obj_Intent			Other_Use_Assoc		*/
	    0, 	/* Obj_Intent			Other_Use_Char_Rslt	*/
	  486,	/* Obj_Intent			Other_Not_Visible	*/
	 1134	/* Obj_Intent			Other_Npes		*/
	},


	/************************ Obj_Optional **************************/
	/* The optional statement may only appear in the specification- */
	/* part of a subprogram or interface body.	(5.2.2)		*/
	/****************************************************************/

	{   0,	/* Obj_Optional			Other_Var_Len_Ch	*/
	    0,	/* Obj_Optional			Other_Var_Len_Arr	*/
	    0,	/* Obj_Optional			Other_Expl_Interface	*/
	    0,	/* Obj_Optional			Other_Use_Func		*/
	  574,	/* Obj_Optional			Other_Use_Subr		*/
	    0,	/* Obj_Optional			Other_Use_Variable	*/
	    0,	/* Obj_Optional			Other_Use_Dummy_Arg	*/
	  920,	/* Obj_Optional			Other_Host_Assoc	*/
	  922, 	/* Obj_Optional			Other_Use_Assoc		*/
	    0, 	/* Obj_Optional			Other_Use_Char_Rslt	*/
	  486,	/* Obj_Optional			Other_Not_Visible	*/
	 1134	/* Obj_Optional			Other_Npes		*/
	},


	/************************ Obj_Private ***************************/
	/* The private statement may only be in the scoping unit of a	*/
	/* module.  Each use-name must be a named variable, procedure,  */
	/* derived type, named constant or namelist group.		*/
	/****************************************************************/

	{   0,	/* Obj_Private			Other_Var_Len_Ch	*/
	    0,	/* Obj_Private			Other_Var_Len_Arr	*/
	    0,	/* Obj_Private			Other_Expl_Interface	*/
	    0,	/* Obj_Private			Other_Use_Func		*/
	  574,	/* Obj_Private			Other_Use_Subr		*/
	    0,	/* Obj_Private			Other_Use_Variable	*/
	    0,	/* Obj_Private			Other_Use_Dummy_Arg	*/
	  920,	/* Obj_Private			Other_Host_Assoc	*/
	    0,	/* Obj_Private			Other_Use_Assoc		*/
	    0,	/* Obj_Private			Other_Use_Char_Rslt	*/
	  486,	/* Obj_Private			Other_Not_Visible	*/
	 1134	/* Obj_Private			Other_Npes		*/
	},


	/************************ Obj_Public ****************************/
	/* The public statement may only be in the scoping unit of a	*/
	/* module.  Each use-name must be a named variable, procedure,	*/
	/* derived type, named constant or namelist group.		*/
	/****************************************************************/

	{   0,	/* Obj_Public			Other_Var_Len_Ch	*/
	    0,	/* Obj_Public			Other_Var_Len_Arr	*/
	    0,	/* Obj_Public			Other_Expl_Interface	*/
	    0,	/* Obj_Public			Other_Use_Func		*/
	  574,	/* Obj_Public			Other_Use_Subr		*/
	    0,	/* Obj_Public			Other_Use_Variable	*/
	    0,	/* Obj_Public			Other_Use_Dummy_Arg	*/
	  920,	/* Obj_Public			Other_Host_Assoc	*/
	    0, 	/* Obj_Public			Other_Use_Assoc		*/
	    0, 	/* Obj_Public			Other_Use_Char_Rslt	*/
	  486,	/* Obj_Public			Other_Not_Visible	*/
	 1134	/* Obj_Public			Other_Npes		*/
	},


	/************************ Obj_Target ****************************/
	/* This specifies a list of object names that may have		*/
	/* pointers associated with them.				*/
	/****************************************************************/

	{   0,	/* Obj_Target			Other_Var_Len_Ch	*/
	    0,	/* Obj_Target			Other_Var_Len_Arr	*/

	  566,	/* Obj_Target			Other_Expl_Interface	*/
		/* Can't add to things outside of the explicit itrfc	*/

	    0,	/* Obj_Target			Other_Use_Func		*/
	  574,	/* Obj_Target			Other_Use_Subr		*/
	    0,	/* Obj_Target			Other_Use_Variable	*/
	    0,	/* Obj_Target			Other_Use_Dummy_Arg	*/

	  920,	/* Obj_Target			Other_Host_Assoc	*/
	  922, 	/* Obj_Target			Other_Use_Assoc		*/
	    0, 	/* Obj_Target			Other_Use_Char_Rslt	*/
	  486,	/* Obj_Target			Other_Not_Visible	*/
	 1134	/* Obj_Target			Other_Npes		*/
	},

	/************************ Obj_Equiv *****************************/
	/* An equivalence object must not be a dummy argument, a	*/
	/* pointer, an allocatable array, and object of a nonsequenced	*/
	/* derived type or of a sequence derived type containing a	*/
	/* pointer at any level of component selection, an automatic	*/
	/* object, a function name, an entry name, a result name, a	*/
	/* named constant, a structure component, or a subobject of any */
	/* of the preceeding objects. (5.5.1 constraint)		*/
	/****************************************************************/

	{ 560,	/* Obj_Equiv		 	Other_Var_Len_Ch	*/
	  562,	/* Obj_Equiv		 	Other_Var_Len_Arr	*/
		/* 5.5.1						*/

	  566,	/* Obj_Equiv		 	Other_Expl_Interface	*/
		/* 14.1.2						*/

	  572,	/* Obj_Equiv		 	Other_Use_Func		*/
	  574,	/* Obj_Equiv		 	Other_Use_Subr		*/
	    0,	/* Obj_Equiv			Other_Use_Variable	*/
	    0,	/* Obj_Equiv			Other_Use_Dummy_Arg	*/

	  920,	/* Obj_Equiv			Other_Host_Assoc	*/
	  922, 	/* Obj_Equiv			Other_Use_Assoc		*/
	    0, 	/* Obj_Equiv			Other_Use_Char_Rslt	*/
	  486,	/* Obj_Equiv			Other_Not_Visible	*/
	 1134	/* Obj_Equiv			Other_Npes		*/
	},


	/************************ Obj_Save ******************************/
	/* An object-name must not be a dummy argument name, a		*/
	/* procedure name, a function result name, an automatic data	*/
	/* object, or the name of an entity in a common block. (5.2.4)  */
	/****************************************************************/

	{ 560,	/* Obj_Saved		 	Other_Var_Len_Ch	*/
	  562,	/* Obj_Saved		 	Other_Var_Len_Arr	*/
	  566,	/* Obj_Saved		 	Other_Expl_Interface	*/

	  572,	/* Obj_Saved		 	Other_Use_Func		*/
	  574,	/* Obj_Saved		 	Other_Use_Subr		*/
	    0,	/* Obj_Saved			Other_Use_Variable	*/
	    0,	/* Obj_Saved			Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  920,	/* Obj_Saved			Other_Host_Assoc	*/
	  922, 	/* Obj_Saved			Other_Use_Assoc		*/
	    0, 	/* Obj_Saved			Other_Use_Char_Rslt	*/
	  486,	/* Obj_Saved			Other_Not_Visible	*/
	 1134	/* Obj_Saved			Other_Npes		*/
	},


	/************************ Obj_Automatic *************************/
	/****************************************************************/

	{ 560,	/* Obj_Automatic		Other_Var_Len_Ch	*/
	  562,	/* Obj_Automatic		Other_Var_Len_Arr	*/
	    0,	/* Obj_Automatic		Other_Expl_Interface	*/

	    0,	/* Obj_Automatic		Other_Use_Func		*/
	  574,	/* Obj_Automatic		Other_Use_Subr		*/
	    0,	/* Obj_Automatic		Other_Use_Variable	*/
	 1039,	/* Obj_Automatic		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	    0,	/* Obj_Automatic		Other_Host_Assoc	*/
	  922, 	/* Obj_Automatic		Other_Use_Assoc		*/
	    0, 	/* Obj_Automatic		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Automatic 		Other_Not_Visible	*/
	 1134	/* Obj_Automatic		Other_Npes		*/
	},


	/************************ Obj_Pointer ***************************/
	/* Cannot have the INTENT or PARAMETER attributes.  If it is an	*/
	/* array, it must be a deferred shape one.  (5.2.7 constraints)	*/
	/* Cannot be ALLOCATABLE, EXTERNAL, TARGET, or INTRINSIC. (5.1)	*/
	/****************************************************************/

	{   0,	/* Obj_Pointer			Other_Var_Len_Ch	*/
	  562,	/* Obj_Pointer			Other_Var_Len_Arr	*/
		/* Pointer must be deferred shape array (5.1)		*/

	  566,	/* Obj_Pointer			Other_Expl_Interface	*/
	    0,	/* Obj_Pointer			Other_Use_Func		*/
	  574,	/* Obj_Pointer			Other_Use_Subr		*/
	    0,	/* Obj_Pointer			Other_Use_Variable	*/
	    0,	/* Obj_Pointer			Other_Use_Dummy_Arg	*/

	  920,	/* Obj_Pointer			Other_Host_Assoc	*/
	  922, 	/* Obj_Pointer			Other_Use_Assoc		*/
	    0, 	/* Obj_Pointer			Other_Use_Char_Rslt	*/
	  486,	/* Obj_Pointer			Other_Not_Visible	*/
	 1134	/* Obj_Pointer			Other_Npes		*/
	},


	/************************ Obj_Dcl_Extern ************************/
	/* External-name must be the name of an external procedure, a	*/
	/* darg, or a block data program unit.				*/
	/****************************************************************/

	{ 560,	/* Obj_Dcl_Extern		Other_Var_Len_Ch	*/
	  562,	/* Obj_Dcl_Extern		Other_Var_Len_Arr	*/
		/*	(12.3.1.1) If a procedure has an array-valued	*/
		/* result or a variable length character result, it	*/
		/* must have an explicit interface.	An explicit	*/
		/* interface for an external subprogram must		*/
		/* be specified in an interface block.	(12.3.1)	*/

	  566,	/* Obj_Dcl_Extern		Other_Expl_Interface	*/

	    0,	/* Obj_Dcl_Extern		Other_Use_Func		*/

	  574,	/* Obj_Dcl_Extern		Other_Use_Subr		*/
	    0,	/* Obj_Dcl_Extern		Other_Use_Variable	*/
	    0,	/* Obj_Dcl_Extern		Other_Use_Dummy_Arg	*/

	  920,	/* Obj_Dcl_Extern		Other_Host_Assoc	*/
	  922, 	/* Obj_Dcl_Extern		Other_Use_Assoc		*/
	    0, 	/* Obj_Dcl_Extern		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Dcl_Extern		Other_Not_Visible	*/
	 1134	/* Obj_Dcl_Extern		Other_Npes		*/
	},


	/************************ Obj_Dcl_Intrin ************************/
	/* Intrinsic-name must be the name of an intrinsic procedure.	*/
	/****************************************************************/

	{ 560,	/* Obj_Dcl_Intrin		Other_Var_Len_Ch	*/
	  562,	/* Obj_Dcl_Intrin		Other_Var_Len_Arr	*/
		/* 5.1 discussion limits this to typing.		*/

	  566,	/* Obj_Dcl_Intrin		Other_Expl_Interface	*/

	    0,	/* Obj_Dcl_Intrin		Other_Use_Func		*/
	  574,	/* Obj_Dcl_Intrin		Other_Use_Subr		*/
	  559,	/* Obj_Dcl_Intrin		Other_Use_Variable	*/
	 1039,	/* Obj_Dcl_Intrin		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  920,	/* Obj_Dcl_Intrin		Other_Host_Assoc	*/
	  922, 	/* Obj_Dcl_Intrin		Other_Use_Assoc		*/
	    0, 	/* Obj_Dcl_Intrin		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Dcl_Intrin		Other_Not_Visible	*/
	 1134	/* Obj_Dcl_Intrin		Other_Npes		*/
	},


	/************************ Obj_Data_Init *************************/
	/* The = initialization-expr must not appear if object-name is  */
	/* a darg, a function result, an object in a named common block */
	/* unless the type declaration is in a block data program unit, */
	/* an object in blank common, an allocatable array, a pointer,  */
	/* an external name, an intrinsic name, or an automatic object. */
	/* (5.1 constraint)						*/
	/****************************************************************/

	{ 560,	/* Obj_Data_Init		Other_Var_Len_Ch	*/
	  562,	/* Obj_Data_Init		Other_Var_Len_Arr	*/
		/* 5.2.9						*/

	  566,	/* Obj_Data_Init		Other_Expl_Interface	*/
	  572,	/* Obj_Data_Init		Other_Use_Func		*/
	  574,	/* Obj_Data_Init		Other_Use_Subr		*/
	    0,	/* Obj_Data_Init		Other_Use_Variable	*/
	    0,	/* Obj_Data_Init		Other_Use_Dummy_Arg	*/

	  920,	/* Obj_Data_Init		Other_Host_Assoc	*/
	  922, 	/* Obj_Data_Init		Other_Use_Assoc		*/
	    0, 	/* Obj_Data_Init		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Data_Init		Other_Not_Visible	*/
	 1134	/* Obj_Data_Init		Other_Npes		*/
	},


	/************************ Obj_Typed *****************************/
	/* This must be an object (which is a constant or a variable)	*/
	/* (2.4.3.1) or an external function, an intrinsic function, a  */
	/* function dummy procedure or a statement function.		*/
	/* Actually you can type an internal or module function also,	*/
	/* but strict adherence to the standard forces this typing to	*/
	/* be on the FUNCTION statement, not the type declaration stmt  */
	/****************************************************************/

	{   0,	/* Obj_Typed		 	Other_Var_Len_Ch	*/
	    0,	/* Obj_Typed		 	Other_Var_Len_Arr	*/

	  566,	/* Obj_Typed		 	Other_Expl_Interface	*/
	    0,	/* Obj_Typed		 	Other_Use_Func		*/
	  574,	/* Obj_Typed		 	Other_Use_Subr		*/
	    0,	/* Obj_Typed			Other_Use_Variable	*/
	    0,	/* Obj_Typed			Other_Use_Dummy_Arg	*/
		/* This needs special checking.	Done by merge_type.	*/

	  920,	/* Obj_Typed			Other_Host_Assoc	*/
	  922, 	/* Obj_Typed			Other_Use_Assoc		*/
	    0, 	/* Obj_Typed			Other_Use_Char_Rslt	*/
	  486,	/* Obj_Typed			Other_Not_Visible	*/
	 1134	/* Obj_Typed			Other_Npes		*/
	},


	/************************ Obj_Volatile  *************************/
	/* Volatile applies to any kind of variable.                    */
	/****************************************************************/

	{   0,	/* Obj_Volatile		 	Other_Var_Len_Ch	*/
	    0,	/* Obj_Volatile		 	Other_Var_Len_Arr	*/
	  566,	/* Obj_Volatile		 	Other_Expl_Interface	*/
	  572,	/* Obj_Volatile		 	Other_Use_Func		*/
	  574,	/* Obj_Volatile		 	Other_Use_Subr		*/
	    0,	/* Obj_Volatile			Other_Use_Variable	*/
	    0,	/* Obj_Volatile			Other_Use_Dummy_Arg	*/
#ifdef KEY /* Bug 14110 */
	    0,	/* Obj_Volatile			Other_Host_Assoc	*/
	    0, 	/* Obj_Volatile			Other_Use_Assoc		*/
#else /* KEY Bug 14110 */
	  920,	/* Obj_Volatile			Other_Host_Assoc	*/
	  922, 	/* Obj_Volatile			Other_Use_Assoc		*/
#endif /* KEY Bug 14110 */
	    0, 	/* Obj_Volatile			Other_Use_Char_Rslt	*/
	  486,	/* Obj_Volatile			Other_Not_Visible	*/
	 1134	/* Obj_Volatile			Other_Npes		*/
	},

	/********************** Obj_Copy_Assumed_Shape ******************/
	/* This is a non-pointer dummy argument array.	(5.1.2.4.2)	*/
	/****************************************************************/

	{   0,	/* Obj_Copy_Assumed_Shape	Other_Var_Len_Ch	*/
	 1451,	/* Obj_Copy_Assumed_Shape	Other_Var_Len_Arr	*/
	 1453,	/* Obj_Copy_Assumed_Shape	Other_Expl_Interface	*/
	 1454,	/* Obj_Copy_Assumed_Shape	Other_Use_Func		*/
	 1455,	/* Obj_Copy_Assumed_Shape	Other_Use_Subr		*/
	 1444,	/* Obj_Copy_Assumed_Shape	Other_Use_Variable	*/
	 1443,	/* Obj_Copy_Assumed_Shape	Other_Use_Dummy_Arg	*/
	 1449,	/* Obj_Copy_Assumed_Shape	Other_Host_Assoc	*/
	 1448,	/* Obj_Copy_Assumed_Shape	Other_Use_Assoc		*/
	 1501,	/* Obj_Copy_Assumed_Shape	Other_Use_Char_Rslt	*/
	  486,	/* Obj_Copy_Assumed_Shape	Other_Not_Visible	*/
	 1447 	/* Obj_Copy_Assumed_Shape	Other_Npes		*/
	},


	/************************ Obj_Auxiliary *************************/
	/* Cray extension - Rules come from the CFT77 manual.		*/
	/* This must be a non-character array.	It can be a dummy arg,  */
	/* but cannot be a function result.	It should not be mixed	*/
	/* with Fortran 90.						*/
	/****************************************************************/

	{ 1450,	/* Obj_Auxiliary		Other_Var_Len_Ch	*/
	    0,	/* Obj_Auxiliary		Other_Var_Len_Arr	*/

	 1453,	/* Obj_Auxiliary		Other_Expl_Interface	*/

	 1454,	/* Obj_Auxiliary		Other_Use_Func		*/
	 1455,	/* Obj_Auxiliary		Other_Use_Subr		*/
	    0,	/* Obj_Auxiliary		Other_Use_Variable	*/
	    0,	/* Obj_Auxiliary		Other_Use_Dummy_Arg	*/
		/* cft77 accepts this.					*/

	 1449,	/* Obj_Auxiliary		Other_Host_Assoc	*/
	 1448, 	/* Obj_Auxiliary		Other_Use_Assoc		*/
	    0, 	/* Obj_Auxiliary		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Auxiliary		Other_Not_Visible	*/
	 1447	/* Obj_Auxiliary		Other_Npes		*/
	},

	/************************ Obj_Vfunction *************************/
	/* This is a Cray extension.  It must be an external function,  */
	/* but cannot be declared EXTERNAL.  (From cft77 documentation) */
	/****************************************************************/

	{1450,	/* Obj_Vfunction		Other_Var_Len_Ch	*/
	 1451,	/* Obj_Vfunction		Other_Var_Len_Arr	*/
	 1453,	/* Obj_Vfunction		Other_Expl_Interface	*/
	    0,	/* Obj_Vfunction		Other_Use_Func		*/
	 1455,	/* Obj_Vfunction		Other_Use_Subr		*/
	 1444,	/* Obj_Vfunction		Other_Use_Variable	*/
	 1443,	/* Obj_Vfunction		Other_Use_Dummy_Arg	*/
	 1449,	/* Obj_Vfunction		Other_Host_Assoc	*/
	 1448, 	/* Obj_Vfunction		Other_Use_Assoc		*/
	 1501, 	/* Obj_Vfunction		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Vfunction		Other_Not_Visible	*/
	 1447	/* Obj_Vfunction		Other_Npes		*/
	},


	/************************ Obj_No_Side_Effects *******************/
	/* This is a Cray extension.  It must be an external function,  */
	/* and must not be a dummy procedure.	(From cft77 doc)	*/
	/****************************************************************/

	{ 1450,	/* Obj_No_Side_Effects		Other_Var_Len_Ch	*/
	    0,	/* Obj_No_Side_Effects		Other_Var_Len_Arr	*/
	 1453,	/* Obj_No_Side_Effects		Other_Expl_Interface	*/
	    0,	/* Obj_No_Side_Effects		Other_Use_Func		*/
	    0,	/* Obj_No_Side_Effects		Other_Use_Subr		*/
	 1444,	/* Obj_No_Side_Effects		Other_Use_Variable	*/
	 1443,	/* Obj_No_Side_Effects		Other_Use_Dummy_Arg	*/
	 1449,	/* Obj_No_Side_Effects		Other_Host_Assoc	*/
	 1448, 	/* Obj_No_Side_Effects		Other_Use_Assoc		*/
	 1501, 	/* Obj_No_Side_Effects		Other_Use_Char_Rslt	*/
	  486,	/* Obj_No_Side_Effects		Other_Not_Visible	*/
	 1447	/* Obj_No_Side_Effects		Other_Npes		*/
	},

	/*************************** Obj_Symmetric **********************/
	/* This is a Cray extension.					*/
	/****************************************************************/

	{   0,	/* Obj_Symmetric		Other_Var_Len_Ch	*/
	    0,	/* Obj_Symmetric		Other_Var_Len_Arr	*/
	 1453,	/* Obj_Symmetric		Other_Expl_Interface	*/
	 1454,	/* Obj_Symmetric		Other_Use_Func		*/
	 1455,	/* Obj_Symmetric		Other_Use_Subr		*/
	    0,	/* Obj_Symmetric		Other_Use_Variable	*/
	 1443,	/* Obj_Symmetric		Other_Use_Dummy_Arg	*/
	 1449,	/* Obj_Symmetric		Other_Host_Assoc	*/
	 1448, 	/* Obj_Symmetric		Other_Use_Assoc		*/
	 1501, 	/* Obj_Symmetric		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Symmetric		Other_Not_Visible	*/
	 1447	/* Obj_Symmetric		Other_Npes		*/
	},


	/***************************** Obj_Inline ***********************/
	/* This is a Cray extension.					*/
	/****************************************************************/

	{   0,	/* Obj_Inline			Other_Var_Len_Ch	*/
	    0,	/* Obj_Inline			Other_Var_Len_Arr	*/
	    0,	/* Obj_Inline			Other_Expl_Interface	*/
	    0,	/* Obj_Inline			Other_Use_Func		*/
	    0,	/* Obj_Inline			Other_Use_Subr		*/
	 1444,	/* Obj_Inline			Other_Use_Variable	*/
	 1443,	/* Obj_Inline			Other_Use_Dummy_Arg	*/
	 1449,	/* Obj_Inline			Other_Host_Assoc	*/
	    0, 	/* Obj_Inline	 		Other_Use_Assoc		*/
	 1501, 	/* Obj_Inline	 		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Inline	 		Other_Not_Visible	*/
	 1447	/* Obj_Inline	 		Other_Npes		*/
	},

	/***************************** Obj_Inline ***********************/
	/* This is a Cray extension.					*/
	/****************************************************************/

	{   0,	/* Obj_Ipa			Other_Var_Len_Ch	*/
	    0,	/* Obj_Ipa			Other_Var_Len_Arr	*/
	    0,	/* Obj_Ipa			Other_Expl_Interface	*/
	    0,	/* Obj_Ipa			Other_Use_Func		*/
	    0,	/* Obj_Ipa			Other_Use_Subr		*/
	 1444,	/* Obj_Ipa			Other_Use_Variable	*/
	 1443,	/* Obj_Ipa			Other_Use_Dummy_Arg	*/
	 1449,	/* Obj_Ipa			Other_Host_Assoc	*/
	    0, 	/* Obj_Ipa	 		Other_Use_Assoc		*/
	 1501, 	/* Obj_Ipa	 		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Ipa	 		Other_Not_Visible	*/
	 1447	/* Obj_Ipa	 		Other_Npes		*/
	},

	/********************** Obj_Align_Symbol ************************/
	/* This is an SGI extension.					*/
	/****************************************************************/

	{1450,	/* Obj_Align_Symbol		Other_Var_Len_Ch	*/
	    0,	/* Obj_Align_Symbol		Other_Var_Len_Arr	*/
	 1453,	/* Obj_Align_Symbol		Other_Expl_Interface	*/
	 1454,	/* Obj_Align_Symbol		Other_Use_Func		*/
	 1455,	/* Obj_Align_Symbol		Other_Use_Subr		*/
	    0,	/* Obj_Align_Symbol		Other_Use_Variable	*/
	 1443,	/* Obj_Align_Symbol		Other_Use_Dummy_Arg	*/
	 1449,	/* Obj_Align_Symbol		Other_Host_Assoc	*/
	 1448, 	/* Obj_Align_Symbol		Other_Use_Assoc		*/
	 1501, 	/* Obj_Align_Symbol		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Align_Symbol		Other_Not_Visible	*/
	 1447	/* Obj_Align_Symbol		Other_Npes		*/
	},

	/********************** Obj_Fill_Symbol *************************/
	/* This is an SGI extension.					*/
	/****************************************************************/

	{1450,	/* Obj_Fill_Symbol		Other_Var_Len_Ch	*/
	    0,	/* Obj_Fill_Symbol		Other_Var_Len_Arr	*/
	 1453,	/* Obj_Fill_Symbol		Other_Expl_Interface	*/
	 1454,	/* Obj_Fill_Symbol		Other_Use_Func		*/
	 1455,	/* Obj_Fill_Symbol		Other_Use_Subr		*/
	    0,	/* Obj_Fill_Symbol		Other_Use_Variable	*/
	 1443,	/* Obj_Fill_Symbol		Other_Use_Dummy_Arg	*/
	 1449,	/* Obj_Fill_Symbol		Other_Host_Assoc	*/
	 1448, 	/* Obj_Fill_Symbol		Other_Use_Assoc		*/
	 1501, 	/* Obj_Fill_Symbol		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Fill_Symbol		Other_Not_Visible	*/
	 1447	/* Obj_Fill_Symbol		Other_Npes		*/
	},

	/********************** Obj_Section_Gp **************************/
	/* This is an SGI extension.					*/
	/****************************************************************/

	{1450,	/* Obj_Section_Gp		Other_Var_Len_Ch	*/
	 1450,	/* Obj_Section_Gp		Other_Var_Len_Arr	*/
	 1453,	/* Obj_Section_Gp		Other_Expl_Interface	*/
	 1454,	/* Obj_Section_Gp		Other_Use_Func		*/
	 1455,	/* Obj_Section_Gp		Other_Use_Subr		*/
	    0,	/* Obj_Section_Gp		Other_Use_Variable	*/
	 1443,	/* Obj_Section_Gp		Other_Use_Dummy_Arg	*/
	 1449,	/* Obj_Section_Gp		Other_Host_Assoc	*/
	 1448, 	/* Obj_Section_Gp		Other_Use_Assoc		*/
	 1501, 	/* Obj_Section_Gp		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Section_Gp		Other_Not_Visible	*/
	 1447	/* Obj_Section_Gp		Other_Npes		*/
	},

	/********************** Obj_Section_Non_Gp **********************/
	/* This is an SGI extension.					*/
	/****************************************************************/

	{1450,	/* Obj_Section_Non_Gp		Other_Var_Len_Ch	*/
	 1450,	/* Obj_Section_Non_Gp		Other_Var_Len_Arr	*/
	 1453,	/* Obj_Section_Non_Gp		Other_Expl_Interface	*/
	 1454,	/* Obj_Section_Non_Gp		Other_Use_Func		*/
	 1455,	/* Obj_Section_Non_Gp		Other_Use_Subr		*/
	    0,	/* Obj_Section_Non_Gp		Other_Use_Variable	*/
	 1443,	/* Obj_Section_Non_Gp		Other_Use_Dummy_Arg	*/
	 1449,	/* Obj_Section_Non_Gp		Other_Host_Assoc	*/
	 1448, 	/* Obj_Section_Non_Gp 		Other_Use_Assoc		*/
	 1501, 	/* Obj_Section_Non_Gp 		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Section_Non_Gp 		Other_Not_Visible	*/
	 1447	/* Obj_Section_Non_Gp 		Other_Npes		*/
	},

	/*********************** Obj_Ignore_TKR *************************/
	/****************************************************************/

	{   0,	/* Obj_Ignore_TKR		Other_Var_Len_Ch	*/
	    0,	/* Obj_Ignore_TKR		Other_Var_Len_Arr	*/
	 1453,	/* Obj_Ignore_TKR		Other_Expl_Interface	*/
	 1454,	/* Obj_Ignore_TKR		Other_Use_Func		*/
	 1455,	/* Obj_Ignore_TKR		Other_Use_Subr		*/
	    0,	/* Obj_Ignore_TKR		Other_Use_Variable	*/
	    0,	/* Obj_Ignore_TKR		Other_Use_Dummy_Arg	*/
	 1449,	/* Obj_Ignore_TKR		Other_Host_Assoc	*/
	 1448, 	/* Obj_Ignore_TKR		Other_Use_Assoc		*/
	    0, 	/* Obj_Ignore_TKR		Other_Use_Char_Rslt	*/
 	  486,	/* Obj_Ignore_TKR		Other_Not_Visible	*/
	 1447	/* Obj_Ignore_TKR		Other_Npes		*/
	},

	/************************ Obj_Optional_Dir **********************/
	/****************************************************************/

	{1450,	/* Obj_Optional_Dir		Other_Var_Len_Ch	*/
	 1451,	/* Obj_Optional_Dir		Other_Var_Len_Arr	*/
	    0,	/* Obj_Optional_Dir		Other_Expl_Interface	*/
	    0,	/* Obj_Optional_Dir		Other_Use_Func		*/
	    0,	/* Obj_Optional_Dir		Other_Use_Subr		*/
	 1444,	/* Obj_Optional_Dir		Other_Use_Variable	*/
	 1443,	/* Obj_Optional_Dir		Other_Use_Dummy_Arg	*/
	 1449,	/* Obj_Optional_Dir		Other_Host_Assoc	*/
	 1448, 	/* Obj_Optional_Dir		Other_Use_Assoc		*/
	 1501, 	/* Obj_Optional_Dir		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Optional_Dir		Other_Not_Visible	*/
	 1447	/* Obj_Optional_Dir		Other_Npes		*/
	},

	/************************ Obj_Name ******************************/
	/* This is specified with the !DIR$ C directive.		*/
	/****************************************************************/

	{1450,	/* Obj_Name			Other_Var_Len_Ch	*/
	 1451,	/* Obj_Name			Other_Var_Len_Arr	*/
	 1453,	/* Obj_Name			Other_Expl_Interface	*/
	    0,	/* Obj_Name			Other_Use_Func		*/
	    0,	/* Obj_Name			Other_Use_Subr		*/
	 1444,	/* Obj_Name			Other_Use_Variable	*/
	 1443,	/* Obj_Name			Other_Use_Dummy_Arg	*/
	 1449,	/* Obj_Name			Other_Host_Assoc	*/
	 1448, 	/* Obj_Name			Other_Use_Assoc		*/
	 1501, 	/* Obj_Name			Other_Use_Char_Rslt	*/
	  486,	/* Obj_Name			Other_Not_Visible	*/
	 1447	/* Obj_Name			Other_Npes		*/
	},

	/************************ Obj_Cri_Ptr ***************************/
	/* From cft77's documentation - A Cray pointer cannot be a	*/
	/* constant, an array, a statement function or an external	*/
	/* function.	It can be in common and be a darg.	A Cray	*/
	/* pointer is considered to be a data type.			*/
	/****************************************************************/

	{   0,	/* Obj_Cri_Ptr			Other_Var_Len_Ch	*/
		/* This is legal, because something could be		*/
		/* implicitly typed as a variable length character	*/
		/* and then become a cray pointer.			*/

	  563,	/* Obj_Cri_Ptr			Other_Var_Len_Arr	*/
	  567,	/* Obj_Cri_Ptr			Other_Expl_Interface	*/
	  573,	/* Obj_Cri_Ptr			Other_Use_Func		*/
	  575,	/* Obj_Cri_Ptr			Other_Use_Subr		*/
	  558, 	/* Obj_Cri_Ptr			Other_Use_Variable	*/
	 1038, 	/* Obj_Cri_Ptr			Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  921,	/* Obj_Cri_Ptr			Other_Host_Assoc	*/
	  923, 	/* Obj_Cri_Ptr			Other_Use_Assoc		*/
	  863, 	/* Obj_Cri_Ptr			Other_Use_Char_Rslt	*/
	  486,	/* Obj_Cri_Ptr			Other_Not_Visible	*/
	 1135	/* Obj_Cri_Ptr			Other_Npes		*/
	},


	/************************ Obj_Cri_Pointee ***********************/
	/* From cft77's documentation - A Cray pointee cannot be a	*/
	/* constant, in common, saved, data initialized or equivalenced */
	/* It cannot be a darg or a function result.			*/
	/****************************************************************/

	{ 561,	/* Obj_Cri_Pointee		Other_Var_Len_Ch	*/

	    0,	/* Obj_Cri_Pointee		Other_Var_Len_Arr	*/
		/* cft77 allows this.	It cannot be a dummy argument	*/
		/* nor is it treated like an automatic array.	 */

	  567,	/* Obj_Cri_Pointee		Other_Expl_Interface	*/
	  573,	/* Obj_Cri_Pointee		Other_Use_Func		*/
	  575,	/* Obj_Cri_Pointee		Other_Use_Subr		*/
	  558, 	/* Obj_Cri_Pointee		Other_Use_Variable	*/
	 1038, 	/* Obj_Cri_Pointee		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  921,	/* Obj_Cri_Pointee		Other_Host_Assoc	*/
	  923, 	/* Obj_Cri_Pointee		Other_Use_Assoc		*/
	  863, 	/* Obj_Cri_Pointee		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Cri_Pointee		Other_Not_Visible	*/
	 1135	/* Obj_Cri_Pointee		Other_Npes		*/
	},


	/*********************** Obj_Cri_Ch_Pointee *********************/
	/* From cft77's documentation - A Cray pointee cannot be a	*/
	/* constant, in common, saved, data initialized or equivalenced */
	/* It cannot be a darg or a function result.  A Cray character	*/
	/* pointee cannot be an array.				 	*/
	/****************************************************************/

	{ 561,	/* Obj_Cri_Ch_Pointee		Other_Var_Len_Ch	*/
	  563,	/* Obj_Cri_Ch_Pointee		Other_Var_Len_Arr	*/
	  567,	/* Obj_Cri_Ch_Pointee		Other_Expl_Interface	*/
	  573,	/* Obj_Cri_Ch_Pointee		Other_Use_Func		*/
	  575,	/* Obj_Cri_Ch_Pointee		Other_Use_Subr		*/
	  558, 	/* Obj_Cri_Ch_Pointee		Other_Use_Variable	*/
	 1038, 	/* Obj_Cri_Ch_Pointee		Other_Use_Dummy_Arg	*/
	  921,	/* Obj_Cri_Ch_Pointee		Other_Host_Assoc	*/
	  923, 	/* Obj_Cri_Ch_Pointee		Other_Use_Assoc		*/
	  863, 	/* Obj_Cri_Ch_Pointee		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Cri_Ch_Pointee		Other_Not_Visible	*/
	 1135	/* Obj_Cri_Ch_Pointee		Other_Npes		*/
	},


	/************************ Obj_Ntry_Func_Result ******************/
	/* The result of a Function, specified on an entry statement.	*/
	/****************************************************************/

	{   0,	/* Obj_Ntry_Func_Result		Other_Var_Len_Ch	*/
	    0,	/* Obj_Ntry_Func_Result		Other_Var_Len_Arr	*/

	  567,	/* Obj_Ntry_Func_Result		Other_Expl_Interface	*/
	  573,	/* Obj_Ntry_Func_Result		Other_Use_Func		*/
	  575,	/* Obj_Ntry_Func_Result		Other_Use_Subr		*/
	  558, 	/* Obj_Ntry_Func_Result		Other_Use_Variable	*/
	 1038, 	/* Obj_Ntry_Func_Result		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  921,	/* Obj_Ntry_Func_Result		Other_Host_Assoc	*/
	  923, 	/* Obj_Ntry_Func_Result		Other_Use_Assoc		*/
	  863, 	/* Obj_Ntry_Func_Result		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Ntry_Func_Result		Other_Not_Visible	*/
	 1135	/* Obj_Ntry_Func_Result		Other_Npes		*/
	},


	/************************ Obj_Dummy_Arg *************************/
	/****************************************************************/

	{   0,	/* Obj_Dummy_Arg		Other_Var_Len_Ch	*/
	    0,	/* Obj_Dummy_Arg		Other_Var_Len_Arr	*/
	    0,	/* Obj_Dummy_Arg		Other_Expl_Interface	*/
	    0,	/* Obj_Dummy_Arg		Other_Use_Func		*/

	  575,	/* Obj_Dummy_Arg		Other_Use_Subr		*/

 	    0, 	/* Obj_Dummy_Arg		Other_Use_Variable	*/
 	    0, 	/* Obj_Dummy_Arg		Other_Use_Dummy_Arg	*/
		/* This could be used in a bounds specification		*/
		/* expression.	If it is used in an executable stmt	*/
		/* it's caught by parse_dummy_args.			*/

	  921,	/* Obj_Dummy_Arg		Other_Host_Assoc	*/
	  923, 	/* Obj_Dummy_Arg		Other_Use_Assoc		*/
	    0, 	/* Obj_Dummy_Arg		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Dummy_Arg		Other_Not_Visible	*/
	 1135	/* Obj_Dummy_Arg		Other_Npes		*/
	},


	/************************ Obj_Common_Obj ************************/
	/* A common-block object must not be a dummy argument, an	*/
	/* allocatable array, an automatic object, a function name,	*/
	/* an entry name or a result name.	(5.5.2)			*/
	/****************************************************************/

	{ 561,	/* Obj_Common_Obj		Other_Var_Len_Ch	*/
	  563,	/* Obj_Common_Obj		Other_Var_Len_Arr	*/
	  567,	/* Obj_Common_Obj		Other_Expl_Interface	*/
	  573,	/* Obj_Common_Obj		Other_Use_Func		*/
	  575,	/* Obj_Common_Obj		Other_Use_Subr		*/
	    0, 	/* Obj_Common_Obj 		Other_Use_Variable	*/
	    0, 	/* Obj_Common_Obj		Other_Use_Dummy_Arg	*/
	    0,	/* Obj_Common_Obj		Other_Host_Assoc	*/
	  923, 	/* Obj_Common_Obj		Other_Use_Assoc		*/
	    0, 	/* Obj_Common_Obj		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Common_Obj		Other_Not_Visible	*/
	 1135	/* Obj_Common_Obj		Other_Npes		*/
	},


	/************************ Obj_Namelist_Obj **********************/
	/* A namelist object must not be an array dummy argument with	*/
	/* a non-constant bound, a variable with a non-constant char	*/
	/* length, an automatic object, a pointer or an allocatable	*/
	/* array.	(5.4) constraint				*/
	/****************************************************************/

	{ 561,	/* Obj_Namelist_Obj		Other_Var_Len_Ch	*/
	  563,	/* Obj_Namelist_Obj		Other_Var_Len_Arr	*/
	  567,	/* Obj_Namelist_Obj		Other_Expl_Interface	*/
	  573,	/* Obj_Namelist_Obj		Other_Use_Func		*/
	  575,	/* Obj_Namelist_Obj		Other_Use_Subr		*/
	    0, 	/* Obj_Namelist_Obj		Other_Use_Variable	*/
	    0, 	/* Obj_Namelist_Obj		Other_Use_Dummy_Arg	*/
	    0,	/* Obj_Namelist_Obj		Other_Host_Assoc	*/
	    0, 	/* Obj_Namelist_Obj		Other_Use_Assoc		*/
	    0, 	/* Obj_Namelist_Obj		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Namelist_Obj		Other_Not_Visible	*/
	 1135	/* Obj_Namelist_Obj		Other_Npes		*/
	},

	
	/************************ Obj_Module_Proc ***********************/
	/* 12.3.2.1 states that a procedure must not have more than	*/
	/* one explicit interface in a scoping unit.	12.3.1 states	*/
	/* that the interface of a module procedure is always an	*/
	/* explicit interface in a scoping unit.	12.3 defines an	*/
	/* interface as the characteristics of the procedure, its	*/
	/* dummy arguments and its function result (if a function).	*/
	/* Since the actual definition of the module procedure is the	*/
	/* explicit interface, it's characteristics cannot be defined	*/
	/* outside of the module procedure definition.		 	*/
	/* Also 5.1 states that an entity must not be given any		*/
	/* attribute more than once in a scoping unit.		 	*/
	/****************************************************************/

	{ 561,	/* Obj_Module_Proc		Other_Var_Len_Ch	*/
	  563,	/* Obj_Module_Proc		Other_Var_Len_Arr	*/
	  567,	/* Obj_Module_Proc		Other_Expl_Interface	*/
	  573,	/* Obj_Module_Proc		Other_Use_Func		*/
	  575,	/* Obj_Module_Proc		Other_Use_Subr		*/
	  558, 	/* Obj_Module_Proc		Other_Use_Variable	*/
	 1038, 	/* Obj_Module_Proc		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  921,	/* Obj_Module_Proc		Other_Host_Assoc	*/
	    0,	/* Obj_Module_Proc	Other_Use_Assoc			*/
		/* You can specify something in a MODULE procedure	*/
		/* statement that has been use associated.		*/

	    0,	/* Obj_Module_Proc		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Module_Proc		Other_Not_Visible	*/
	 1135	/* Obj_Module_Proc		Other_Npes		*/
	},


	/************************ Obj_Derived_Type **********************/
	/* By  14.1.2, a derived type cannot be a named variable, a	*/
	/* named constant, a construct name, a statement function, an	*/
	/* internal procedure, a dummy procedure, an intrinsic proc,	*/
	/* a generic identifier or a namelist group name.		*/
	/****************************************************************/

	{ 561,	/* Obj_Derived_Type		Other_Var_Len_Ch	*/
	  563,	/* Obj_Derived_Type		Other_Var_Len_Arr	*/
	  567,	/* Obj_Derived_Type		Other_Expl_Interface	*/
	  573,	/* Obj_Derived_Type		Other_Use_Func		*/
	  575,	/* Obj_Derived_Type		Other_Use_Subr		*/
	  558, 	/* Obj_Derived_Type		Other_Use_Variable	*/
	 1038, 	/* Obj_Derived_Type		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  921,	/* Obj_Derived_Type		Other_Host_Assoc	*/
	  923, 	/* Obj_Derived_Type		Other_Use_Assoc		*/
 	  863, 	/* Obj_Derived_Type		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Derived_Type		Other_Not_Visible	*/
	 1135	/* Obj_Derived_Type		Other_Npes		*/
	},


	/************************ Obj_Generic_Interface *****************/
	/* By 14.1.2, a generic interface cannot be a named variable, a	*/
	/* named constant, a construct name, a statement function, an	*/
	/* internal procedure, a dummy procedure, an intrinsic proc,	*/
	/* a derived type or a namelist group name.			*/
	/****************************************************************/

	{ 561,	/* Obj_Generic_Interface	Other_Var_Len_Ch	*/
	  563,	/* Obj_Generic_Interface	Other_Var_Len_Arr	*/
	    0,	/* Obj_Generic_Interface	Other_Expl_Interface	*/
	  573,	/* Obj_Generic_Interface	Other_Use_Func		*/
	  575,	/* Obj_Generic_Interface	Other_Use_Subr		*/
	  558, 	/* Obj_Generic_Interface	Other_Use_Variable	*/
	 1038, 	/* Obj_Generic_Interface	Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  921,	/* Obj_Generic_Interface	Other_Host_Assoc	*/
	    0, 	/* Obj_Generic_Interface	Other_Use_Assoc		*/
	    0, 	/* Obj_Generic_Interface	Other_Use_Char_Rslt	*/
	  486,	/* Obj_Generic_Interface	Other_Not_Visible	*/
	 1135	/* Obj_Generic_Interface	Other_Npes		*/
	},


	/************************ Obj_Namelist_Grp **********************/
	/* By  14.1.2, a namelist group cannot be a named variable, a	*/
	/* named constant, a construct name, a statement function, an	*/
	/* internal procedure, a dummy procedure, an intrinsic proc,	*/
	/* a derived type or a generic identifier.			*/
	/****************************************************************/

	{ 561,	/* Obj_Namelist_Grp		Other_Var_Len_Ch	*/
	  563,	/* Obj_Namelist_Grp		Other_Var_Len_Arr	*/
	  567,	/* Obj_Namelist_Grp		Other_Expl_Interface	*/
	  573,	/* Obj_Namelist_Grp		Other_Use_Func		*/
	  575,	/* Obj_Namelist_Grp		Other_Use_Subr		*/
	  558, 	/* Obj_Namelist_Grp		Other_Use_Variable	*/
	 1038, 	/* Obj_Namelist_Grp		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  921,	/* Obj_Namelist_Grp		Other_Host_Assoc	*/
	  923, 	/* Obj_Namelist_Grp		Other_Use_Assoc		*/
	  863, 	/* Obj_Namelist_Grp		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Namelist_Grp		Other_Not_Visible	*/
	 1135	/* Obj_Namelist_Grp		Other_Npes		*/
	},


	/************************ Obj_Stmt_Func *************************/
	/* By  14.1.2, a statement function cannot be a named variable, */
	/* a named constant, a construct name, a generic identifier, an	*/
	/* internal procedure, a dummy procedure, an intrinsic proc,	*/
	/* a derived type or a namelist group.	Also see 12.5.4.	*/
	/****************************************************************/

	{ 561,	/* Obj_Stmt_Func		Other_Var_Len_Ch	*/
	  563,	/* Obj_Stmt_Func		Other_Var_Len_Arr	*/
	  567,	/* Obj_Stmt_Func		Other_Expl_Interface	*/
	  573,	/* Obj_Stmt_Func		Other_Use_Func		*/
	  575,	/* Obj_Stmt_Func		Other_Use_Subr		*/
	  558,	/* Obj_Stmt_Func		Other_Use_Variable	*/
	 1038,	/* Obj_Stmt_Func		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  921,	/* Obj_Stmt_Func		Other_Host_Assoc	*/
	  923, 	/* Obj_Stmt_Func		Other_Use_Assoc		*/
	    0, 	/* Obj_Stmt_Func		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Stmt_Func		Other_Not_Visible	*/
	 1135	/* Obj_Stmt_Func		Other_Npes		*/
	},


	/************************ Obj_Construct *************************/
	/* By  14.1.2, a construct cannot be a named variable, a named	*/
	/* constant, a statement function, a generic identifier, an	*/
	/* internal procedure, a dummy procedure, an intrinsic proc,	*/
	/* a derived type or a namelist group.			 	*/
	/****************************************************************/

	{ 561,	/* Obj_Construct		Other_Var_Len_Ch	*/
	  563,	/* Obj_Construct		Other_Var_Len_Arr	*/
	  567,	/* Obj_Construct		Other_Expl_Interface	*/
	  573,	/* Obj_Construct		Other_Use_Func		*/
	  575,	/* Obj_Construct		Other_Use_Subr		*/
	  558, 	/* Obj_Construct		Other_Use_Variable	*/
	 1038, 	/* Obj_Construct		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  921,	/* Obj_Construct		Other_Host_Assoc	*/
	  923, 	/* Obj_Construct		Other_Use_Assoc		*/
	  863, 	/* Obj_Construct		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Construct		Other_Not_Visible	*/
	 1135	/* Obj_Construct		Other_Npes		*/
	},


	/************************ Obj_Entry_Func ************************/
	/* This is the name on an ENTRY statement in an external	*/
	/* FUNCTION.	This allows anything legal on a result name to	*/
	/* be specified here.	Then if a different result name is	*/
	/* specified errors are issued at that time saying to use the	*/
	/* result name, not the function name to declare type, shape,	*/
	/* target or pointer.						*/
	/****************************************************************/

	{   0,	/* Obj_Entry_Func		Other_Var_Len_Ch	*/
	    0,	/* Obj_Entry_Func		Other_Var_Len_Arr	*/
	  567,	/* Obj_Entry_Func		Other_Expl_Interface	*/
	    0,	/* Obj_Entry_Func		Other_Use_Func		*/
	  575,	/* Obj_Entry_Func		Other_Use_Subr		*/
	  558, 	/* Obj_Entry_Func		Other_Use_Variable	*/
	 1038, 	/* Obj_Entry_Func		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  921,	/* Obj_Entry_Func		Other_Host_Assoc	*/
	  923, 	/* Obj_Entry_Func		Other_Use_Assoc		*/
	  863, 	/* Obj_Entry_Func		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Entry_Func		Other_Not_Visible	*/
	 1135	/* Obj_Entry_Func		Other_Npes		*/
	},


	/************************ Obj_Entry_Subr ************************/
	/* This is the name on an ENTRY statement in an external	*/
	/* SUBROUTINE.	One just knows that a SUBROUTINE can't have	*/
	/* a type, or a shape, or much of anything else.	(2.2.3)	*/
	/****************************************************************/

	{ 561,	/* Obj_Entry_Subr		Other_Var_Len_Ch	*/
	  563,	/* Obj_Entry_Subr		Other_Var_Len_Arr	*/
	  567,	/* Obj_Entry_Subr		Other_Expl_Interface	*/
	  573,	/* Obj_Entry_Subr		Other_Use_Func		*/
	    0,	/* Obj_Entry_Subr		Other_Use_Subr		*/
	  558, 	/* Obj_Entry_Subr		Other_Use_Variable	*/
	 1038, 	/* Obj_Entry_Subr		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  921,	/* Obj_Entry_Subr		Other_Host_Assoc	*/
	  923, 	/* Obj_Entry_Subr		Other_Use_Assoc		*/
	  863, 	/* Obj_Entry_Subr		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Entry_Subr		Other_Not_Visible	*/
	 1135	/* Obj_Entry_Subr		Other_Npes		*/
	},


	/************************ Obj_Intern_Func ***********************/
	/* This is the name on the FUNCTION statement for an internal	*/
	/* FUNCTION.	12.3.2.1 states that a procedure must not have	*/
	/* more than one explicit interface in a scoping unit.	12.3.1  */
	/* states that the interface of an internal procedure is always	*/
	/* an explicit interface in a scoping unit.	12.3 defines an	*/
	/* interface as the characteristics of the procedure, its	*/
	/* dummy arguments and its function result (if a function).	*/
	/* Since the actual definition of the internal procedure is the	*/
	/* explicit interface, it's characteristics cannot be defined	*/
	/* outside of the internal procedure definition.		*/
	/* Also 5.1 states that an entity must not be given any		*/
	/* attribute more than once in a scoping unit.		 	*/
	/****************************************************************/

	{ 561,	/* Obj_Intern_Func		Other_Var_Len_Ch	*/
	  563,	/* Obj_Intern_Func		Other_Var_Len_Arr	*/
	  567,	/* Obj_Intern_Func		Other_Expl_Interface	*/
	    0,	/* Obj_Intern_Func		Other_Use_Func		*/
	  575,	/* Obj_Intern_Func		Other_Use_Subr		*/
	    0, 	/* Obj_Intern_Func		Other_Use_Variable	*/
	    0, 	/* Obj_Intern_Func		Other_Use_Dummy_Arg	*/
	    0,	/* Obj_Intern_Func		Other_Host_Assoc	*/
	  923, 	/* Obj_Intern_Func		Other_Use_Assoc		*/
	    0, 	/* Obj_Intern_Func		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Intern_Func		Other_Not_Visible	*/
	 1135	/* Obj_Intern_Func		Other_Npes		*/
	},


	/************************ Obj_Intern_Subr ***********************/
	/* This is the name on a SUBROUTINE statement for an internal	*/
	/* SUBROUTINE.	One just knows that a SUBROUTINE can't have	*/
	/* a type, or a shape, or much of anything else.	(2.2.3)	*/
	/****************************************************************/

	{ 561,	/* Obj_Intern_Subr		Other_Var_Len_Ch	*/
	  563,	/* Obj_Intern_Subr		Other_Var_Len_Arr	*/
	  567,	/* Obj_Intern_Subr		Other_Expl_Interface	*/
	  573,	/* Obj_Intern_Subr		Other_Use_Func		*/
	    0,	/* Obj_Intern_Subr		Other_Use_Subr		*/
	    0, 	/* Obj_Intern_Subr		Other_Use_Variable	*/
	    0, 	/* Obj_Intern_Subr		Other_Use_Dummy_Arg	*/
		/* Allow this, because it could be an actual arg and	*/
		/* we need to issue a meaningful error message.	*/

	    0,	/* Obj_Intern_Subr		Other_Host_Assoc	*/
	  923, 	/* Obj_Intern_Subr		Other_Use_Assoc		*/
	  863, 	/* Obj_Intern_Subr		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Intern_Subr		Other_Not_Visible	*/
	 1135	/* Obj_Intern_Subr		Other_Npes		*/
	},


	/************************ Obj_Module_Func ***********************/
	/* This is the name on the FUNCTION statement for a module	*/
	/* FUNCTION.	12.3.2.1 states that a procedure must not have	*/
	/* more than one explicit interface in a scoping unit.	12.3.1  */
	/* states that the interface of a module procedure is always an */
	/* explicit interface in a scoping unit.	12.3 defines an	*/
	/* interface as the characteristics of the procedure, its	*/
	/* dummy arguments and its function result (if a function).	*/
	/* Since the actual definition of the module procedure is the	*/
	/* explicit interface, it's characteristics cannot be defined	*/
	/* outside of the module procedure definition.		 	*/
	/* Also 5.1 states that an entity must not be given any		*/
	/* attribute more than once in a scoping unit.		 	*/
	/****************************************************************/

	{ 561,	/* Obj_Module_Func		Other_Var_Len_Ch	*/
	  563,	/* Obj_Module_Func		Other_Var_Len_Arr	*/
	  567,	/* Obj_Module_Func		Other_Expl_Interface	*/
	  573,	/* Obj_Module_Func		Other_Use_Func		*/
	  575,	/* Obj_Module_Func		Other_Use_Subr		*/
	  558, 	/* Obj_Module_Func		Other_Use_Variable	*/
	 1038, 	/* Obj_Module_Func		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  921,	/* Obj_Module_Func		Other_Host_Assoc	*/
	  923, 	/* Obj_Module_Func		Other_Use_Assoc		*/
	    0, 	/* Obj_Module_Func		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Module_Func		Other_Not_Visible	*/
	 1135	/* Obj_Module_Func		Other_Npes		*/
	},


	/************************ Obj_Module_Subr ***********************/
	/* This is the name on a SUBROUTINE statement for a module	*/
	/* SUBROUTINE.	One just knows that a SUBROUTINE can't have	*/
	/* a type, or a shape, or much of anything else.	(2.2.3)	*/
	/****************************************************************/

	{ 561,	/* Obj_Module_Subr		Other_Var_Len_Ch	*/
	  563,	/* Obj_Module_Subr		Other_Var_Len_Arr	*/
	  567,	/* Obj_Module_Subr		Other_Expl_Interface	*/
	  573,	/* Obj_Module_Subr		Other_Use_Func		*/
	  575,	/* Obj_Module_Subr		Other_Use_Subr		*/
	  558,	/* Obj_Module_Subr		Other_Use_Variable	*/
	 1038,	/* Obj_Module_Subr		Other_Use_Dummy_Arg	*/
		/* Cannot define something after it has been used.	*/

	  921,	/* Obj_Module_Subr		Other_Host_Assoc	*/
	  923, 	/* Obj_Module_Subr		Other_Use_Assoc		*/
	  863, 	/* Obj_Module_Subr		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Module_Subr		Other_Not_Visible	*/
	 1135	/* Obj_Module_Subr		Other_Npes		*/
	},


	/************************ Obj_Sf_Darg ***************************/
	/* This is the dummy argument used in the statement function	*/
	/* definition.	It is defined in 5.1.1.5 and 12.5.4.		*/
	/****************************************************************/

	{ 561,	/* Obj_Sf_Darg			Other_Var_Len_Ch	*/
	  563,	/* Obj_Sf_Darg			Other_Var_Len_Arr	*/
	  567,	/* Obj_Sf_Darg			Other_Expl_Interface	*/
	  573,	/* Obj_Sf_Darg			Other_Use_Func		*/
	  575,	/* Obj_Sf_Darg			Other_Use_Subr		*/
	    0,	/* Obj_Sf_Darg			Other_Use_Variable	*/
	    0,	/* Obj_Sf_Darg			Other_Use_Dummy_Arg	*/
	    0,	/* Obj_Sf_Darg			Other_Host_Assoc	*/
	    0, 	/* Obj_Sf_Darg			Other_Use_Assoc		*/
	    0, 	/* Obj_Sf_Darg			Other_Use_Char_Rslt	*/
	  486,	/* Obj_Sf_Darg			Other_Not_Visible	*/
	 1135	/* Obj_Sf_Darg			Other_Npes		*/
	},

	/************************ Obj_Sf_Actual_Arg *********************/
	/* This is the actual argument used when a statement function	*/
	/* is referenced.	It is defined in 5.1.1.5 and 12.5.4.	*/
	/****************************************************************/

	{   0,	/* Obj_Sf_Actual_Arg	 	Other_Var_Len_Ch	*/
	    0,	/* Obj_Sf_Actual_Arg	 	Other_Var_Len_Arr	*/
	  761,	/* Obj_Sf_Actual_Arg	 	Other_Expl_Interface	*/
	  761,	/* Obj_Sf_Actual_Arg	 	Other_Use_Func		*/
	  761,	/* Obj_Sf_Actual_Arg	 	Other_Use_Subr		*/
	    0,	/* Obj_Sf_Actual_Arg		Other_Use_Variable	*/
	    0,	/* Obj_Sf_Actual_Arg		Other_Use_Dummy_Arg	*/
	    0,	/* Obj_Sf_Actual_Arg		Other_Host_Assoc	*/
	    0, 	/* Obj_Sf_Actual_Arg		Other_Use_Assoc		*/
	    0, 	/* Obj_Sf_Actual_Arg		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Sf_Actual_Arg		Other_Not_Visible	*/
	0	/* Obj_Sf_Actual_Arg		Other_Npes		*/
	},

	/************************ Obj_Var_Len_Ch ************************/
	/* This is a variable length character (automatic or		*/
	/* adjustable).	This is not called until the length has been	*/
	/* resolved at the end of pass1.  If the length resolves to a	*/
	/* constant, fnd_semantic_err is not called.	If it resolves	*/
	/* to a non-constant, fnd_semantic_err is called with this.	*/
	/* (5.1 discussion)	The specification-expr of a type-param-	*/
	/* value may be a nonconstant expression provided the		*/
	/* specification expression is in an interface body or in the	*/
	/* specification part of a subprogram.	If the data object	*/
	/* being declared depends on the value of such a nonconstant	*/
	/* expression and is not a dummy argument, such an object is	*/
	/* called an automatic data object.  An automatic data object	*/
	/* must not appear in a SAVE or DATA statement nor be declared	*/
	/* with a SAVE attribute nor be initially defined by an =	*/
	/* initialization-expr.						*/
	/****************************************************************/

	{   0,	/* Obj_Var_Len_Ch		Other_Var_Len_Ch	*/
		/* This could be implicitly typed as variable length	*/
		/* character and then be in a character statement.	*/

	    0,	/* Obj_Var_Len_Ch		Other_Var_Len_Arr	*/

	  579,	/* Obj_Var_Len_Ch		Other_Expl_Interface	*/
	    0,	/* Obj_Var_Len_Ch		Other_Use_Func		*/
	  581,	/* Obj_Var_Len_Ch		Other_Use_Subr		*/
	    0,	/* Obj_Var_Len_Ch		Other_Use_Variable	*/
	    0,	/* Obj_Var_Len_Ch		Other_Use_Dummy_Arg	*/

	  924,	/* Obj_Var_Len_Ch		Other_Host_Assoc	*/
	  925, 	/* Obj_Var_Len_Ch		Other_Use_Assoc		*/
	    0, 	/* Obj_Var_Len_Ch		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Var_Len_Ch		Other_Not_Visible	*/
	 1137	/* Obj_Var_Len_Ch		Other_Npes		*/
	},


	/************************ Obj_Var_Len_Arr ***********************/
	/* This is an explicit-shape array.  It can be adjustable or	*/
	/* automatic.	It is not known that this is a variable length	*/
	/* array until the end of pass1.  These originally look like	*/
	/* an explicit shape array with unknown size.			*/
	/* This must be a dummy argument, a function result, or an	*/
	/* automatic object.						*/
	/****************************************************************/

	{   0,	/* Obj_Var_Len_Arr		Other_Var_Len_Ch	*/
	    0,	/* Obj_Var_Len_Arr		Other_Var_Len_Arr	*/

	  579,	/* Obj_Var_Len_Arr		Other_Expl_Interface	*/
	  672,	/* Obj_Var_Len_Arr		Other_Use_Func		*/
	  588,	/* Obj_Var_Len_Arr		Other_Use_Subr		*/
	    0,	/* Obj_Var_Len_Arr		Other_Use_Variable	*/
	    0,	/* Obj_Var_Len_Arr		Other_Use_Dummy_Arg	*/

	  926,	/* Obj_Var_Len_Arr		Other_Host_Assoc	*/
	  927, 	/* Obj_Var_Len_Arr		Other_Use_Assoc		*/
	    0, 	/* Obj_Var_Len_Arr		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Var_Len_Arr		Other_Not_Visible	*/
	 1137	/* Obj_Var_Len_Arr		Other_Npes		*/
	},


	/************************ Obj_Sym_Constant_Arr ******************/
	/* A symbolic constant array is an array where at least one of  */
	/* the bounds contains a symbolic constant expression.	 	*/
	/* At end pass1, array bounds are resolved.			*/
	/* If they are constant, semantic checking is done, if they	*/
	/* have symbolic constant bounds, fnd_semantic_err is called	*/
	/* again for the object, using Obj_Sym_Constant_Arr.  There is	*/
	/* nothing that can be a symbolic constant array that cannot	*/
	/* be an array with constant bounds.  This is a Cray extension.	*/
	/****************************************************************/

	{   0,	/* Obj_Sym_Constant_Arr		Other_Var_Len_Ch	*/
	    0,	/* Obj_Sym_Constant_Arr		Other_Var_Len_Arr	*/

	  579,	/* Obj_Sym_Constant_Arr		Other_Expl_Interface	*/
	    0,	/* Obj_Sym_Constant_Arr		Other_Use_Func		*/
	  588,	/* Obj_Sym_Constant_Arr		Other_Use_Subr		*/
	    0,	/* Obj_Sym_Constant_Arr		Other_Use_Variable	*/
	    0,	/* Obj_Sym_Constant_Arr		Other_Use_Dummy_Arg	*/

	  926,	/* Obj_Sym_Constant_Arr		Other_Host_Assoc	*/
	  927, 	/* Obj_Sym_Constant_Arr		Other_Use_Assoc		*/
	    0, 	/* Obj_Sym_Constant_Arr		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Sym_Constant_Arr		Other_Not_Visible	*/
	 1137	/* Obj_Sym_Constant_Arr		Other_Npes		*/
	},

	/************************ Obj_Interface_Func ********************/
	/* This is the name of the FUNCTION on an interface body.	*/
	/* 12.3.2.1 states that a procedure must not have more than	*/
	/* one explicit interface in a scoping unit.	12.3.1 states	*/
	/* that specifying an external or dummy procedure in an		*/
	/* interface block, causes it to have an explicit interface.	*/
	/* 12.3 defines an interface as the characteristics of the	*/
	/* procedure, its dummy arguments and its function result (if	*/
	/* it's a function).  Since the actual definition of the module */
	/* procedure is the explicit interface, it's characteristics	*/
	/* cannot be defined outside of the module procedure		*/
	/* definition.	Also 5.1 states that an entity must not be	*/
	/* given any attribute more than once in a scoping unit.	*/
	/****************************************************************/

	{ 610,	/* Obj_Interface_Func		Other_Var_Len_Ch	*/
	  611,	/* Obj_Interface_Func		Other_Var_Len_Arr	*/
	  613,	/* Obj_Interface_Func		Other_Expl_Interface	*/
	  616,	/* Obj_Interface_Func		Other_Use_Func		*/
	  617,	/* Obj_Interface_Func		Other_Use_Subr		*/
	  557, 	/* Obj_Interface_Func		Other_Use_Variable	*/
	 1037, 	/* Obj_Interface_Func		Other_Use_Dummy_Arg	*/

	  930,	/* Obj_Interface_Func		Other_Host_Assoc	*/
	  931, 	/* Obj_Interface_Func		Other_Use_Assoc		*/
	    0, 	/* Obj_Interface_Func		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Interface_Func		Other_Not_Visible	*/
	 1136	/* Obj_Interface_Func		Other_Npes		*/
	},


	/************************ Obj_Interface_Subr ********************/
	/* This is the name of the SUBROUTINE on an interface body.	*/
	/* This may be a dummy procedure	(12.3.2.1) discussion	*/
	/* Subroutines can't be typed, shaped or anything else.		*/
	/****************************************************************/

	{ 610,	/* Obj_Interface_Subr		Other_Var_Len_Ch	*/
	  611,	/* Obj_Interface_Subr		Other_Var_Len_Arr	*/
	  613,	/* Obj_Interface_Subr		Other_Expl_Interface	*/
	  616,	/* Obj_Interface_Subr		Other_Use_Func		*/
	  617,	/* Obj_Interface_Subr		Other_Use_Subr		*/
	  557,	/* Obj_Interface_Subr		Other_Use_Variable	*/
	 1037,	/* Obj_Interface_Subr		Other_Use_Dummy_Arg	*/

	  932,	/* Obj_Interface_Subr		Other_Host_Assoc	*/
	  933, 	/* Obj_Interface_Subr		Other_Use_Assoc		*/
	  864, 	/* Obj_Interface_Subr		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Interface_Subr		Other_Not_Visible	*/
	 1136	/* Obj_Interface_Subr		Other_Npes		*/
	},


	/************************ Obj_Use_Extern_Func *******************/
	/* This is an external function in an expression.		*/
	/****************************************************************/

	{   0,	/* Obj_Use_Extern_Func		Other_Var_Len_Ch	*/
	    0,	/* Obj_Use_Extern_Func		Other_Var_Len_Arr	*/
	    0,	/* Obj_Use_Extern_Func		Other_Expl_Interface	*/
	    0,	/* Obj_Use_Extern_Func		Other_Use_Func		*/
	  632,	/* Obj_Use_Extern_Func		Other_Use_Subr		*/
	  557,	/* Obj_Use_Extern_Func		Other_Use_Variable	*/
	 1037,	/* Obj_Use_Extern_Func		Other_Use_Dummy_Arg	*/
	    0,	/* Obj_Use_Extern_Func		Other_Host_Assoc	*/
	    0, 	/* Obj_Use_Extern_Func		Other_Use_Assoc		*/
	    0, 	/* Obj_Use_Extern_Func		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Use_Extern_Func		Other_Not_Visible	*/
	 1136	/* Obj_Use_Extern_Func		Other_Npes		*/
	},


	/************************ Obj_Use_Extern_Subr *******************/
	/* This is an external subroutine in an expression.		*/
	/* Subroutines can't be typed, ranked or anything else.		*/
	/****************************************************************/

	{ 635,	/* Obj_Use_Extern_Subr		Other_Var_Len_Ch	*/
	  636,	/* Obj_Use_Extern_Subr		Other_Var_Len_Arr	*/
	    0,	/* Obj_Use_Extern_Subr		Other_Expl_Interface	*/
	  639,	/* Obj_Use_Extern_Subr		Other_Use_Func		*/
	    0,	/* Obj_Use_Extern_Subr		Other_Use_Subr		*/
	  557,	/* Obj_Use_Extern_Subr		Other_Use_Variable	*/
	 1037,	/* Obj_Use_Extern_Subr		Other_Use_Dummy_Arg	*/
	    0,	/* Obj_Use_Extern_Subr		Other_Host_Assoc	*/
	    0, 	/* Obj_Use_Extern_Subr		Other_Use_Assoc		*/
	  864, 	/* Obj_Use_Extern_Subr		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Use_Extern_Subr		Other_Not_Visible	*/
	 1136	/* Obj_Use_Extern_Subr		Other_Npes		*/
	},


	/************************ Obj_Use_In_Expr ***********************/
	/*	This is something referenced in an expression.		*/
	/****************************************************************/

	{   0,	/* Obj_Use_In_Expr		Other_Var_Len_Ch	*/
	    0,	/* Obj_Use_In_Expr		Other_Var_Len_Arr	*/
	    0,	/* Obj_Use_In_Expr		Other_Expl_Interface	*/
	    0,	/* Obj_Use_In_Expr		Other_Use_Func		*/
	  643,	/* Obj_Use_In_Expr		Other_Use_Subr		*/
	    0,	/* Obj_Use_In_Expr		Other_Use_Variable	*/
	    0,	/* Obj_Use_In_Expr		Other_Use_Dummy_Arg	*/
	    0,	/* Obj_Use_In_Expr		Other_Host_Assoc	*/
	    0, 	/* Obj_Use_In_Expr		Other_Use_Assoc		*/
	    0, 	/* Obj_Use_In_Expr		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Use_In_Expr		Other_Not_Visible	*/
	    0 	/* Obj_Use_In_Expr		Other_Npes		*/
	},


	/************************ Obj_Use_Derived_Type ******************/
	/* This is a name used to type something as a derived type.	*/
	/* Derived types can't be variables, constants, functions,	*/
	/* or dummy args.	( 14.1.2)				*/
	/****************************************************************/

	{ 646,	/* Obj_Use_Derived_Type		Other_Var_Len_Ch	*/
	  647,	/* Obj_Use_Derived_Type		Other_Var_Len_Arr	*/
	  649,	/* Obj_Use_Derived_Type		Other_Expl_Interface	*/
	  652,	/* Obj_Use_Derived_Type		Other_Use_Func		*/
	  653,	/* Obj_Use_Derived_Type		Other_Use_Subr		*/
	  548,	/* Obj_Use_Derived_Type		Other_Use_Variable	*/
	 1040,	/* Obj_Use_Derived_Type		Other_Use_Dummy_Arg	*/
	    0,	/* Obj_Use_Derived_Type		Other_Host_Assoc	*/
	    0, 	/* Obj_Use_Derived_Type		Other_Use_Assoc		*/
	  866, 	/* Obj_Use_Derived_Type		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Use_Derived_Type		Other_Not_Visible	*/
	 1136	/* Obj_Use_Derived_Type		Other_Npes		*/
	},


	/************************ Obj_Use_Spec_Expr *********************/
	/* The rules for a specification expression are as follows:	*/
	/* Each object must be a constant, a dummy arg (that doesn't	*/
	/* have the OPTIONAL or the INTENT (OUT) attributes), in a	*/
	/* common block, accessible by USE or HOST association.		*/
	/* Specific rules are found in 7.1.6.2				*/
	/****************************************************************/

	{   0,	/* Obj_Use_Spec_Expr	 	Other_Var_Len_Ch	*/
	    0,	/* Obj_Use_Spec_Expr	 	Other_Var_Len_Arr	*/
	    0,	/* Obj_Use_Spec_Expr	 	Other_Expl_Interface	*/
	    0,	/* Obj_Use_Spec_Expr	 	Other_Use_Func		*/
	  518,	/* Obj_Use_Spec_Expr	 	Other_Use_Subr		*/
	    0,	/* Obj_Use_Spec_Expr	 	Other_Use_Variable	*/
	    0,	/* Obj_Use_Spec_Expr	 	Other_Use_Dummy_Arg	*/
	    0,	/* Obj_Use_Spec_Expr		Other_Host_Assoc	*/
	    0, 	/* Obj_Use_Spec_Expr		Other_Use_Assoc		*/
	    0, 	/* Obj_Use_Spec_Expr		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Use_Spec_Expr		Other_Not_Visible	*/
	    0	/* Obj_Use_Spec_Expr		Other_Npes		*/
	},

	/********************** Obj_Use_Init_Expr ***********************/
	/* The rules for an initialization expression are as follows:	*/
	/* It MUST be a CONSTANT.  It may be one of several INTRINSICS	*/
	/* that are foldable.	(7.1.6.1)				*/
	/****************************************************************/

	{ 605,	/* Obj_Use_Init_Expr	 	Other_Var_Len_Ch	*/
	  605,	/* Obj_Use_Init_Expr	 	Other_Var_Len_Arr	*/
	  605,	/* Obj_Use_Init_Expr	 	Other_Expl_Interface	*/
	  605,	/* Obj_Use_Init_Expr	 	Other_Use_Func		*/
	  605,	/* Obj_Use_Init_Expr	 	Other_Use_Subr		*/
	    0,	/* Obj_Use_Init_Expr	 	Other_Use_Variable	*/
	    0,	/* Obj_Use_Init_Expr	 	Other_Use_Dummy_Arg	*/
	    0,	/* Obj_Use_Init_Expr		Other_Host_Assoc	*/
	    0, 	/* Obj_Use_Init_Expr		Other_Use_Assoc		*/
	    0, 	/* Obj_Use_Init_Expr		Other_Use_Char_Rslt	*/
	  486,	/* Obj_Use_Init_Expr		Other_Not_Visible	*/
	 1212 	/* Obj_Use_Init_Expr		Other_Npes		*/
	}

};

# ifdef _DEBUG

char	*attr_obj_type_str[Attr_Done]	= {
		"Attr_Assumed_Type_Ch",
		"Attr_Dimension",
		"Attr_Explicit_Shp_Arr",
		"Attr_Assumed_Size_Arr",
		"Attr_Deferred_Shp_Arr",
		"Attr_Assumed_Shp_Arr",
		"Attr_Allocatable",	
		"Attr_Parameter",
		"Attr_Intent",
		"Attr_Optional",
		"Attr_Private",
		"Attr_Public",		
		"Attr_Target",
		"Attr_Equivalence",
		"Attr_Save",
		"Attr_Pointer",
		"Attr_External",
		"Attr_Intrinsic",
		"Attr_Data_Init",
		"Attr_Type",
		"Attr_Co_Array",
		"Attr_Automatic",
		"Attr_Volatile"
#ifdef KEY /* Bug 14150 */
	      , "Attr_Bind"
	      , "Attr_Value"
#endif /* KEY Bug 14150 */
		};

char	*dir_obj_type_str[Dir_Done]	=	{
		"Dir_Auxiliary",
		"Dir_Vfunction",
		"Dir_No_Side_Effects",
		"Dir_Inline",
		"Dir_Symmetric",
		"Dir_Copy_Assumed_Shape",
		"Dir_Align_Symbol",
		"Dir_Fill_Symbol",
		"Dir_Section_Gp",
		"Dir_Section_Non_Gp",
		"Dir_Ignore_TKR",
		"Dir_Optional",
		"Dir_Ipa",
		"Dir_Name"
		};

char	*name_obj_type_str[Name_Done]	= {
		"Name_Variable",
		"Name_Common_Obj",
		"Name_Cri_Pointer",
		"Name_Cri_Pointee",
		"Name_Cri_Ch_Pointee",
		"Name_Func_Result",
		"Name_Dummy_Arg",
		"Name_Module_Proc",
		"Name_Derived_Type",
		"Name_Generic_Interface",
		"Name_Namelist_Group",
		"Name_Namelist_Group_Obj",
		"Name_Statement_Func",
		"Name_Construct",
		"Name_Intrinsic_Func",
		"Name_Intrinsic_Subr",
		"Name_Module",
		"Name_Blockdata",
		"Name_Program",
		"Name_Function",
		"Name_Curr_Func",
		"Name_Curr_Subr",
		"Name_Internal_Func",
		"Name_Internal_Subr"
		};

char	*obj_type_str[Obj_Done]	= {
		"Obj_Assum_Type_Ch",
		"Obj_Expl_Shp_Arr",
		"Obj_Assum_Size_Arr",
		"Obj_Defrd_Shp_Arr",
		"Obj_Assum_Shp_Arr",
		"Obj_Co_Array",
		"Obj_Allocatable",
#ifdef KEY /* Bug 14150 */
		"Obj_Bind",
		"Obj_Value",
#endif /* KEY Bug 14150 */
		"Obj_Constant",
		"Obj_Intent",
		"Obj_Optional",
		"Obj_Private",
		"Obj_Public",
		"Obj_Target",
		"Obj_Equiv",
		"Obj_Saved",
		"Obj_Automatic",
		"Obj_Pointer",
		"Obj_Dcl_Extern",
		"Obj_Dcl_Intrin",
		"Obj_Data_Init",
		"Obj_Typed",
		"Obj_Volatile",

		"Obj_Copy_Assumed_Shape",
		"Obj_Auxiliary",
		"Obj_Vfunction",
		"Obj_No_Side_Effects",
		"Obj_Symmetric",
		"Obj_Inline",
		"Obj_Ipa",
		"Obj_Align_Symbol",
		"Obj_Fill_Symbol",
		"Obj_Section_Gp",
		"Obj_Section_Non_Gp",
		"Obj_Ignore_Type_And_Kind",
		"Obj_Optional_Dir",
		"Obj_Name",
	
		"Obj_Cri_Ptr",
		"Obj_Cri_Pointee",
		"Obj_Cri_Ch_Pointee",
		"Obj_Ntry_Func_Result",
		"Obj_Dummy_Arg",
		"Obj_Common_Obj",
		"Obj_Namelist_Obj",
		"Obj_Module_Proc",
		"Obj_Derived_Type",
		"Obj_Generic_Interface",
		"Obj_Namelist_Grp",
		"Obj_Stmt_Func",
		"Obj_Construct",
		"Obj_Entry_Func",
		"Obj_Entry_Subr",
		"Obj_Intern_Func",
		"Obj_Intern_Subr",
		"Obj_Module_Func",
		"Obj_Module_Subr",
		"Obj_Sf_Darg",

		"Obj_Sf_Actual_Arg",
		"Obj_Var_Len_Ch",
		"Obj_Var_Len_Arr",
		"Obj_Sym_Constant_Array",
		"Obj_Interface_Func",
		"Obj_Interface_Subr",
		"Obj_Use_Extern_Func",
		"Obj_Use_Extern_Subr",
		"Obj_Use_In_Expr",
		"Obj_Use_Derived_Type",
		"Obj_Use_Spec_Expr",
		"Obj_Use_Init_Expr"
		};



char	*other_obj_type_str[Other_Done]	= {
		"Other_Var_Len_Ch",
		"Other_Var_Len_Arr",	
		"Other_Expl_Interface",
		"Other_Use_Func",
		"Other_Use_Subr",
		"Other_Use_Variable",
		"Other_Use_Dummy_Arg",
		"Other_Host_Assoc",
		"Other_Use_Assoc",
		"Other_Use_Char_Rslt",
		"Other_Not_Visible",
		"Other_Npes"
		};
# endif
