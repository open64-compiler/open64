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



/* USMID:  "\n@(#)5.0_pl/headers/s_call.h	5.1	04/29/99 21:22:31\n" */

/***********************************************************\
|* These are the definitions for runtime argument checking *|
|* descriptors. They must remain identical to the defs in  *|
|* argchck.c in libf90. DO NOT CHANGE THESE!!!             *|
\***********************************************************/

enum		arg_type_values	{
				Null_Arg,
				Short_Integer_Arg,
				Long_Integer_Arg,
				Real_Arg,
				Double_Arg,
				Complex_Arg,
				Logical_Arg,
				Character_Arg,
				Pointer_Arg,
				Typeless_Arg,
				Character_Pointer_Arg,
				Label_Arg,
				Subroutine_Arg,
				Null_Function_Arg,
				Short_Integer_Function_Arg,
				Long_Integer_Function_Arg,
				Real_Function_Arg,
				Double_Function_Arg,
				Complex_Function_Arg,
				Logical_Function_Arg,
				Character_Function_Arg,
				Pointer_Function_Arg,
				Typeless_Function_Arg,
				Character_Pointer_Function_Arg,
				Subprogram_Arg,
				Derived_Type_Arg,
				Derived_Type_Function_Arg };

typedef	enum	arg_type_values	arg_type_type;

struct		arg_desc_header	{
				unsigned int		seen_this   :  1;
				unsigned int		f90_flag    :  1;
				unsigned int		num_ck_only :  1;
                                unsigned int		suppress_msg:  1;
                                unsigned int		unused1     :  4;
# ifdef _TARGET64
                                unsigned int		unused2     : 24;
                                unsigned int		unused3     :  8;
# endif
				unsigned int		arg_count   : 24;
				};

typedef struct arg_desc_header arg_desc_header_type;

struct		arg_desc_node	{
				long_type		arg_type;
				long_type		kind;

	/* size is in terms of bytes for character, otherwise it's elements */
        /* If it is assumed-size character, size is elements also. */
				long_type		size;

				long_type		char_len;
				long_type		rank;
				long_type		line;
				char			name[32];
# ifdef _TARGET64
				unsigned int		unused2           : 32;
# endif
				unsigned int		unused1           : 18;
				unsigned int		pgm_unknown       :  1;
				unsigned int		pgm_unit          :  1;
				unsigned int		ignore_tkr        :  1;
				unsigned int		dope_vector       :  1;
				unsigned int		pointer           :  1;
				unsigned int		default_kind      :  1;
				unsigned int		optional          :  1;
				unsigned int		intent_out        :  1;
                                unsigned int		assumed_shape     :  1;
				unsigned int		assumed_size_array:  1;
				unsigned int		assumed_size_char :  1;
                                unsigned int            defineable        :  1;
				unsigned int		array_element     :  1;
				unsigned int		generic_call      :  1;

				long_type		*derived_type_tbl;
				};


long	argchck_suppress_msg[40];
int	num_argchck_suppress_msg = 0;

typedef struct arg_desc_node arg_desc_node_type;

int linear_to_kind_type[Num_Linear_Types] = {
        /* Err_Res           */         0,
        /* Short_Char_Const  */         1,
        /* Short_Typeless_Const */      0,
        /* Typeless_1        */         0,
        /* Typeless_2        */         0,
        /* Typeless_4        */         0,
        /* Typeless_8        */         0,
        /* Long_Typeless     */         0,
        /* Integer_1         */         1,
        /* Integer_2         */         2,
        /* Integer_4         */         4,
        /* Integer_8         */         8,
        /* Real_4            */         4,
        /* Real_8            */         8,
        /* Real_16           */         16,
        /* Complex_4         */         4,
        /* Complex_8         */         8,
        /* Complex_16        */         16,
        /* CRI_Ptr_8         */         0,
        /* Logical_1         */         1,
        /* Logical_2         */         2,
        /* Logical_4         */         4,
        /* Logical_8         */         8,
        /* Character_1       */         1,
        /* Character_2       */         2,
        /* Character_4       */         4,
        /* CRI_Ch_Ptr_8      */         0,
        /* Structure_Type    */         0,
        /* CRI_Parcel_Ptr_8  */         0
        };



/********************************************************\
|* These are the final_arg_work tables that control the *|
|* behavior of argument association. (ie. COPY IN/OUT)  *|
|* act_arg_values are in s_globals.h                    *|
\********************************************************/

enum		dummy_arg_values {
				Unknown_Dummy,
				Scalar_Dummy,
				Sequence_Array_Dummy,
				Scalar_Ptr_Dummy,
				Array_Ptr_Dummy,
				Assumed_Shape_Dummy,
                                Intrin_Dope_Dummy,
				Scalar_Target_Dummy,
				Array_Target_Dummy
				  };

typedef enum	dummy_arg_values	dummy_arg_type;

int	arg_assoc_tbl[MAX_NUM_ACT_TYPES][MAX_NUM_DUMMY_TYPES] = {
	/* Scalar_Expression */				{
			/* Unknown_Dummy        */	COPY_IN,
			/* Scalar_Dummy         */	COPY_IN,
			/* Sequence_Array_Dummy */	ERROR_ASSOC,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	ERROR_ASSOC,
			/* Intrin_Dope_Dummy    */	COPY_IN_MAKE_DV,
			/* Scalar_Target_Dummy  */      ERROR_ASSOC,
			/* Array_Target_Dummy   */	ERROR_ASSOC
							},
	/* Scalar_Var */				{
			/* Unknown_Dummy        */	PASS_ADDRESS,
			/* Scalar_Dummy         */	PASS_ADDRESS,
			/* Sequence_Array_Dummy */	PASS_ADDRESS,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	ERROR_ASSOC,
			/* Intrin_Dope_Dummy    */	MAKE_DV,
			/* Scalar_Target_Dummy  */      MAKE_DV,
			/* Array_Target_Dummy   */	ERROR_ASSOC
							},
	/* Scalar_Tmp_Var */				{
			/* Unknown_Dummy        */	PASS_ADDRESS,
			/* Scalar_Dummy         */	PASS_ADDRESS,
			/* Sequence_Array_Dummy */	PASS_ADDRESS,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	ERROR_ASSOC,
			/* Intrin_Dope_Dummy    */	MAKE_DV,
			/* Scalar_Target_Dummy  */      MAKE_DV,
			/* Array_Target_Dummy   */	ERROR_ASSOC
							},
	/* Array_Elt */					{
			/* Unknown_Dummy        */	PASS_ADDRESS,
			/* Scalar_Dummy         */	PASS_ADDRESS,
			/* Sequence_Array_Dummy */	PASS_ADDRESS,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	ERROR_ASSOC,
			/* Intrin_Dope_Dummy    */	MAKE_DV,
			/* Scalar_Target_Dummy  */      MAKE_DV,
			/* Array_Target_Dummy   */	ERROR_ASSOC
							},
	/* Array_Tmp_Elt */				{
			/* Unknown_Dummy        */	PASS_ADDRESS,
			/* Scalar_Dummy         */	PASS_ADDRESS,
			/* Sequence_Array_Dummy */	PASS_ADDRESS,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	ERROR_ASSOC,
			/* Intrin_Dope_Dummy    */	MAKE_DV,
			/* Scalar_Target_Dummy  */      MAKE_DV,
			/* Array_Target_Dummy   */	ERROR_ASSOC
							},
	/* Scalar_Ptr */				{
			/* Unknown_Dummy        */	PASS_ADDRESS,
			/* Scalar_Dummy         */	PASS_ADDRESS,
			/* Sequence_Array_Dummy */	ERROR_ASSOC,
			/* Scalar_Ptr_Dummy     */	PASS_DV,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	ERROR_ASSOC,
			/* Intrin_Dope_Dummy    */	PASS_DV,
			/* Scalar_Target_Dummy  */      PASS_DV,
			/* Array_Target_Dummy   */	ERROR_ASSOC
							},
	/* Scalar_Tmp_Ptr */				{
			/* Unknown_Dummy        */	PASS_ADDRESS,
			/* Scalar_Dummy         */	PASS_ADDRESS,
			/* Sequence_Array_Dummy */	ERROR_ASSOC,
			/* Scalar_Ptr_Dummy     */	PASS_DV,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	ERROR_ASSOC,
			/* Intrin_Dope_Dummy    */	PASS_DV,
			/* Scalar_Target_Dummy  */      PASS_DV,
			/* Array_Target_Dummy   */	ERROR_ASSOC
							},
	/* Scalar_Constant */				{
			/* Unknown_Dummy        */	COPY_IN,
			/* Scalar_Dummy         */	COPY_IN,
#ifdef KEY /* Bug 14150 */
			/* F2003 allows passing a character string (which
			 * may be const) to a dummy array of character */
			/* Sequence_Array_Dummy */	COPY_IN,
#else /* KEY Bug 14150 */
			/* Sequence_Array_Dummy */	ERROR_ASSOC,
#endif /* KEY Bug 14150 */
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	ERROR_ASSOC,
			/* Intrin_Dope_Dummy    */	COPY_IN_MAKE_DV,
			/* Scalar_Target_Dummy  */      ERROR_ASSOC,
			/* Array_Target_Dummy   */	ERROR_ASSOC
							},
	/* Array_Expr */				{
			/* Unknown_Dummy        */	COPY_IN,
			/* Scalar_Dummy         */	ERROR_ASSOC,
			/* Sequence_Array_Dummy */	COPY_IN,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	COPY_IN_MAKE_DV,
			/* Intrin_Dope_Dummy    */	COPY_IN_MAKE_DV,
			/* Scalar_Target_Dummy  */      ERROR_ASSOC,
			/* Array_Target_Dummy   */	ERROR_ASSOC
							},
	/* Array_Ptr */				{
			/* Unknown_Dummy        */	CHECK_CONTIG_FLAG,
			/* Scalar_Dummy         */	ERROR_ASSOC,
			/* Sequence_Array_Dummy */	CHECK_CONTIG_FLAG,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	PASS_DV,
			/* Assumed_Shape_Dummy  */	PASS_DV_COPY,
			/* Intrin_Dope_Dummy    */	PASS_DV,
			/* Scalar_Target_Dummy  */      ERROR_ASSOC,
			/* Array_Target_Dummy   */	PASS_DV_COPY
							},
	/* Array_Tmp_Ptr */				{
			/* Unknown_Dummy        */	CHECK_CONTIG_FLAG,
			/* Scalar_Dummy         */	ERROR_ASSOC,
			/* Sequence_Array_Dummy */	CHECK_CONTIG_FLAG,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	PASS_DV,
			/* Assumed_Shape_Dummy  */	PASS_DV_COPY,
			/* Intrin_Dope_Dummy    */	PASS_DV,
			/* Scalar_Target_Dummy  */      ERROR_ASSOC,
			/* Array_Target_Dummy   */	PASS_DV_COPY
							},
	/* Whole_Allocatable */				{
			/* Unknown_Dummy        */	PASS_ADDRESS,
			/* Scalar_Dummy         */	ERROR_ASSOC,
			/* Sequence_Array_Dummy */	PASS_ADDRESS,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	PASS_DV_COPY,
			/* Intrin_Dope_Dummy    */	PASS_DV,
			/* Scalar_Target_Dummy  */      ERROR_ASSOC,
			/* Array_Target_Dummy   */	PASS_DV_COPY
							},
	/* Whole_Tmp_Allocatable */			{
			/* Unknown_Dummy        */	PASS_ADDRESS,
			/* Scalar_Dummy         */	ERROR_ASSOC,
			/* Sequence_Array_Dummy */	PASS_ADDRESS,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	PASS_DV_COPY,
			/* Intrin_Dope_Dummy    */	PASS_DV,
			/* Scalar_Target_Dummy  */      ERROR_ASSOC,
			/* Array_Target_Dummy   */	PASS_DV_COPY
							},
	/* Whole_Sequence */				{
			/* Unknown_Dummy        */	PASS_ADDRESS,
			/* Scalar_Dummy         */	ERROR_ASSOC,
			/* Sequence_Array_Dummy */	PASS_ADDRESS,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	MAKE_DV,
			/* Intrin_Dope_Dummy    */	MAKE_DV,
			/* Scalar_Target_Dummy  */      ERROR_ASSOC,
			/* Array_Target_Dummy   */	MAKE_DV
							},
	/* Whole_Tmp_Sequence */			{
			/* Unknown_Dummy        */	PASS_ADDRESS,
			/* Scalar_Dummy         */	ERROR_ASSOC,
			/* Sequence_Array_Dummy */	PASS_ADDRESS,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	MAKE_DV,
			/* Intrin_Dope_Dummy    */	MAKE_DV,
			/* Scalar_Target_Dummy  */      ERROR_ASSOC,
			/* Array_Target_Dummy   */	MAKE_DV
							},
	/* Whole_Ass_Shape */				{
			/* Unknown_Dummy        */	CHECK_CONTIG_FLAG,
			/* Scalar_Dummy         */	ERROR_ASSOC,
			/* Sequence_Array_Dummy */	CHECK_CONTIG_FLAG,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	PASS_DV_COPY,
			/* Intrin_Dope_Dummy    */	PASS_DV,
			/* Scalar_Target_Dummy  */      ERROR_ASSOC,
			/* Array_Target_Dummy   */	PASS_DV_COPY
							},
	/* Whole_Array_Constant */			{
			/* Unknown_Dummy        */	COPY_IN,
			/* Scalar_Dummy         */	ERROR_ASSOC,
			/* Sequence_Array_Dummy */	COPY_IN,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	COPY_IN_MAKE_DV,
			/* Intrin_Dope_Dummy    */	COPY_IN_MAKE_DV,
			/* Scalar_Target_Dummy  */      ERROR_ASSOC,
			/* Array_Target_Dummy   */	ERROR_ASSOC
							},
	/* Sequence_Array_Section */			{
			/* Unknown_Dummy        */	COPY_IN_COPY_OUT,
			/* Scalar_Dummy         */	ERROR_ASSOC,
			/* Sequence_Array_Dummy */	COPY_IN_COPY_OUT,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	MAKE_DV,
			/* Intrin_Dope_Dummy    */	MAKE_DV,
			/* Scalar_Target_Dummy  */      ERROR_ASSOC,
			/* Array_Target_Dummy   */	MAKE_DV
							},
	/* Constant_Array_Section */			{
			/* Unknown_Dummy        */	COPY_IN,
			/* Scalar_Dummy         */	ERROR_ASSOC,
			/* Sequence_Array_Dummy */	COPY_IN,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	COPY_IN_MAKE_DV,
			/* Intrin_Dope_Dummy    */	COPY_IN_MAKE_DV,
			/* Scalar_Target_Dummy  */      ERROR_ASSOC,
			/* Array_Target_Dummy   */	ERROR_ASSOC
							},
	/* Dv_Array_Section */				{
			/* Unknown_Dummy        */	COPY_IN_COPY_OUT,
			/* Scalar_Dummy         */	ERROR_ASSOC,
			/* Sequence_Array_Dummy */	COPY_IN_COPY_OUT,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	MAKE_DV,
			/* Intrin_Dope_Dummy    */	MAKE_DV,
			/* Scalar_Target_Dummy  */      ERROR_ASSOC,
			/* Array_Target_Dummy   */	MAKE_DV
							},
	/* Vector_Subscript_Section */			{
			/* Unknown_Dummy        */	COPY_IN,
			/* Scalar_Dummy         */	ERROR_ASSOC,
			/* Sequence_Array_Dummy */	COPY_IN,
			/* Scalar_Ptr_Dummy     */	ERROR_ASSOC,
			/* Array_Ptr_Dummy      */	ERROR_ASSOC,
			/* Assumed_Shape_Dummy  */	COPY_IN_MAKE_DV,
			/* Intrin_Dope_Dummy    */	COPY_IN_MAKE_DV,
			/* Scalar_Target_Dummy  */      ERROR_ASSOC,
			/* Array_Target_Dummy   */	ERROR_ASSOC
							},

        /* Contig_Section */                            {
                        /* Unknown_Dummy        */      PASS_SECTION_ADDRESS,
                        /* Scalar_Dummy         */      ERROR_ASSOC,
                        /* Sequence_Array_Dummy */      PASS_SECTION_ADDRESS,
                        /* Scalar_Ptr_Dummy     */      ERROR_ASSOC,
                        /* Array_Ptr_Dummy      */      ERROR_ASSOC,
                        /* Assumed_Shape_Dummy  */      MAKE_DV,
                        /* Intrin_Dope_Dummy    */      MAKE_DV,
                        /* Scalar_Target_Dummy  */      ERROR_ASSOC,
                        /* Array_Target_Dummy   */      MAKE_DV
                                                        },

        /* Dv_Contig_Section */                            {
                        /* Unknown_Dummy        */      CHECK_CONTIG_FLAG,
                        /* Scalar_Dummy         */      ERROR_ASSOC,
                        /* Sequence_Array_Dummy */      CHECK_CONTIG_FLAG,
                        /* Scalar_Ptr_Dummy     */      ERROR_ASSOC,
                        /* Array_Ptr_Dummy      */      ERROR_ASSOC,
                        /* Assumed_Shape_Dummy  */      MAKE_DV,
                        /* Intrin_Dope_Dummy    */      MAKE_DV,
                        /* Scalar_Target_Dummy  */      ERROR_ASSOC,
                        /* Array_Target_Dummy   */      MAKE_DV
                                                        },
			};


/******************************************************\
|* globally accessible objects defined in s_call.c    *|
\******************************************************/

arg_strct_type           *arg_info_list;
int                      *arg_list;
int                       arg_list_size          = 0;
int                       arg_info_list_size     = 0;
int                       arg_info_list_base     = NULL_IDX;
int                       arg_info_list_top      = NULL_IDX;

arg_strct_type            init_arg_info;
expr_arg_type		  init_exp_desc;

/**********************************************************************\
|* This variable keeps track of the original sh idx of stmt with call *|
\**********************************************************************/

int			orig_sh_idx = NULL_IDX;
boolean			keep_orig_sh = FALSE;
