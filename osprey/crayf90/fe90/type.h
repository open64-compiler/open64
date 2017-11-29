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



/* USMID:  "\n@(#)5.0_pl/headers/type.h	5.14	10/20/99 12:57:15\n" */

/******************************************************************************\
|**									     **|
|**	This header file is used to intialize tables that are used to 	     **|
|**	define type information.  These tables are processor dependent.      **|
|**									     **|
\******************************************************************************/

# if defined(_TARGET_OS_MAX)

linear_type_type	init_default_linear_type[Num_Fortran_Types] = {

		        Integer_8,	/* Fortran_Integer	  */
		        Logical_8,	/* Fortran_Logical	  */
		        Real_8,		/* Fortran_Real		  */
		        Real_8,		/* Fortran_Double	  */
		        Complex_8,	/* Fortran_Complex	  */
		        Complex_8,	/* Fortran_Double_Complex */
		        Character_1,	/* Fortran_Character	  */
		        Typeless_8	/* Fortran_Typeless	  */
			};

linear_type_type	half_linear_type[Num_Fortran_Types] = {

		        Integer_4,	/* Fortran_Integer	  */
		        Logical_4,	/* Fortran_Logical	  */
		        Real_4,		/* Fortran_Real		  */
		        Real_8,		/* Fortran_Double	  */
		        Complex_4,	/* Fortran_Complex	  */
		        Complex_8,	/* Fortran_Double_Complex */
		        Character_1,	/* Fortran_Character	  */
		        Typeless_4	/* Fortran_Typeless	  */
			};

# else
linear_type_type	init_default_linear_type[Num_Fortran_Types] = {
		        Integer_4,	/* Fortran_Integer	  */
		        Logical_4,	/* Fortran_Logical	  */
		        Real_4,		/* Fortran_Real		  */
		        Real_8,		/* Fortran_Double	  */
		        Complex_4,	/* Fortran_Complex	  */
		        Complex_8,	/* Fortran_Double_Complex */
		        Character_1,	/* Fortran_Character	  */
		        Typeless_4	/* Fortran_Typeless	  */
			};

linear_type_type	half_linear_type[Num_Fortran_Types] = {

		        Err_Res,	/* Fortran_Integer	  */
		        Err_Res,	/* Fortran_Logical	  */
		        Err_Res,	/* Fortran_Real		  */
		        Real_4,		/* Fortran_Double	  */
		        Err_Res,	/* Fortran_Complex	  */
		        Complex_4,	/* Fortran_Double_Complex */
		        Err_Res,	/* Fortran_Character	  */
		        Err_Res		/* Fortran_Typeless	  */
			};

linear_type_type	double_linear_type[Num_Fortran_Types] = {
		        Integer_8,	/* Fortran_Integer	  */
		        Logical_8,	/* Fortran_Logical	  */
		        Real_8,		/* Fortran_Real		  */
		        Real_16,	/* Fortran_Double	  */
		        Complex_8,	/* Fortran_Complex	  */
		        Complex_16,	/* Fortran_Double_Complex */
		        Character_1,	/* Fortran_Character	  */
		        Typeless_8	/* Fortran_Typeless	  */
			};
# endif


int     bit_size_tbl[Num_Linear_Types] = {
        /* Err_Res           */          0,
        /* Short_Char_Const  */         64,
        /* Short_Typeless_Const    */   64,
        /* Typeless_1        */          8,
        /* Typeless_2        */         16,
        /* Typeless_4        */         32,
        /* Typeless_8        */         64,
        /* Long_Typeless     */          0,

# if defined(_TARGET_OS_UNICOS) || defined(_TARGET_OS_MAX) ||            \
     defined(_TARGET_SV2)
        /* Integer_1         */         32,
        /* Integer_2         */         32,
# else
        /* Integer_1         */          8,
        /* Integer_2         */         16,
# endif

        /* Integer_4         */         32,
        /* Integer_8         */         64,

# if defined(_TARGET_OS_UNICOS) && !defined(_TARGET_SV2)
        /* Real_4            */         64,
# else
        /* Real_4            */         32,
# endif

        /* Real_8            */         64,
        /* Real_16           */        128,

# if defined(_TARGET_OS_UNICOS) && !defined(_TARGET_SV2)
        /* Complex_4         */        128,
# else
        /* Complex_4         */         64,
# endif

        /* Complex_8         */        128,

# if defined(_TARGET_OS_MAX)
        /* Complex_16        */        128,
# else
        /* Complex_16        */        256,
# endif

        /* CRI_Ptr_8         */         64,

# if defined(_TARGET_OS_MAX) || defined(_TARGET_SV2)
        /* Logical_1         */         32,
        /* Logical_2         */         32,
        /* Logical_4         */         32,
# elif defined(_TARGET_OS_UNICOS)
        /* Logical_1         */         64,
        /* Logical_2         */         64,
        /* Logical_4         */         64,
# else
        /* Logical_1         */          8,
        /* Logical_2         */         16,
        /* Logical_4         */         32,
# endif

        /* Logical_8         */         64,
        /* Character_1       */          0,
        /* Character_2       */          0,
        /* Character_4       */          0,
        /* CRI_Ch_Ptr_8      */         64,
        /* Structure_Type    */          0,
        /* CRI_Parcel_Ptr_8  */         64
                                                 };


# if defined(_TARGET_OS_UNICOS) && !defined(_TARGET_SV2)

int	storage_bit_kind_tbl[Num_Linear_Types] = {        
	/* Err_Res           */  	0,
        /* Short_Char_Const  */ 	0,
        /* Short_Typeless_Const    */ 	0,
        /* Typeless_1        */ 	1,
        /* Typeless_2        */ 	2,
        /* Typeless_4        */ 	4,
        /* Typeless_8        */ 	8,
        /* Long_Typeless     */ 	0,
        /* Integer_1         */ 	1,
        /* Integer_2         */ 	2,
        /* Integer_4         */ 	4,
        /* Integer_8         */ 	8,
        /* Real_4            */ 	4,
        /* Real_8            */ 	8,
        /* Real_16           */ 	16,
        /* Complex_4         */ 	4,
        /* Complex_8         */ 	8,
        /* Complex_16        */ 	16,
        /* CRI_Ptr_8         */ 	8,
        /* Logical_1         */ 	1,
        /* Logical_2         */ 	2,
        /* Logical_4         */ 	4,
        /* Logical_8         */ 	8,
        /* Character_1       */ 	1,
        /* Character_2       */ 	2,
        /* Character_4       */ 	4,
        /* CRI_Ch_Ptr_8      */ 	8,
        /* Structure_Type    */ 	0,
	/* CRI_Parcel_Ptr_8  */		8
	                                         };

/****************************************************\
|* storage_bit_size_tbl[Complex_4] does not reflect *|
|* the size of a Complex_4 constant in our cn table *|
|* on the t3e. These are stored in two 64 bit words.*|
\****************************************************/

int	storage_bit_size_tbl[Num_Linear_Types] = {        
	/* Err_Res           */  	0,
        /* Short_Char_Const  */ 	64,
        /* Short_Typeless_Const    */ 	64,
        /* Typeless_1        */ 	64,
        /* Typeless_2        */ 	64,
        /* Typeless_4        */ 	64,
        /* Typeless_8        */ 	64,
        /* Long_Typeless     */ 	0,
        /* Integer_1         */ 	64,
        /* Integer_2         */ 	64,
        /* Integer_4         */ 	64,
        /* Integer_8         */ 	64,
        /* Real_4            */ 	64,
        /* Real_8            */ 	64,
        /* Real_16           */ 	128,
        /* Complex_4         */ 	128,
        /* Complex_8         */ 	128,
        /* Complex_16        */ 	256,
        /* CRI_Ptr_8         */ 	64,
        /* Logical_1         */ 	64,
        /* Logical_2         */ 	64,
        /* Logical_4         */ 	64,
        /* Logical_8         */ 	64,
        /* Character_1       */ 	0,
        /* Character_2       */ 	0,
        /* Character_4       */ 	0,
        /* CRI_Ch_Ptr_8      */ 	64,
        /* Structure_Type    */ 	0,
	/* CRI_Parcel_Ptr_8  */		64
	                                         };

int     storage_bit_prec_tbl[Num_Linear_Types] = {
        /* Err_Res           */         0,
        /* Short_Char_Const  */         8,
        /* Short_Typeless_Const    */   64,
        /* Typeless_1        */         64,
        /* Typeless_2        */         64,
        /* Typeless_4        */         64,
        /* Typeless_8        */         64,
        /* Long_Typeless     */         0,
        /* Integer_1         */         32,
        /* Integer_2         */         32,
        /* Integer_4         */         32,
        /* Integer_8         */         64,
        /* Real_4            */         48,
        /* Real_8            */         48,
        /* Real_16           */         96,
        /* Complex_4         */         48,
        /* Complex_8         */         48,
        /* Complex_16        */         96,
        /* CRI_Ptr_8         */         64,
        /* Logical_1         */         64,
        /* Logical_2         */         64,
        /* Logical_4         */         64,
        /* Logical_8         */         64,
        /* Character_1       */         8,
        /* Character_2       */         8,
        /* Character_4       */         8,
        /* CRI_Ch_Ptr_8      */         8,
        /* Structure_Type    */         0,
        /* CRI_Parcel_Ptr_8  */         16
                                                 };

int     register_bit_size_tbl[Num_Linear_Types] = {
        /* Err_Res           */         0,
        /* Short_Char_Const  */         64,
        /* Short_Typeless_Const    */   64,
        /* Typeless_1        */         64,
        /* Typeless_2        */         64,
        /* Typeless_4        */         64,
        /* Typeless_8        */         64,
        /* Long_Typeless     */         0,
        /* Integer_1         */         64,
        /* Integer_2         */         64,
        /* Integer_4         */         64,
        /* Integer_8         */         64,
        /* Real_4            */         64,
        /* Real_8            */         64,
        /* Real_16           */         64,
        /* Complex_4         */         64,
        /* Complex_8         */         64,
        /* Complex_16        */         64,
        /* CRI_Ptr_8         */         64,
        /* Logical_1         */         64,
        /* Logical_2         */         64,
        /* Logical_4         */         64,
        /* Logical_8         */         64,
        /* Character_1       */         0,
        /* Character_2       */         0,
        /* Character_4       */         0,
        /* CRI_Ch_Ptr_8      */         64,
        /* Structure_Type    */         0,
        /* CRI_Parcel_Ptr_8  */         64
                                                 };

int     stride_mult_unit_in_bits[Num_Linear_Types] = {
        /* Err_Res           */         0,
        /* Short_Char_Const  */         8,
        /* Short_Typeless_Const    */   64,
        /* Typeless_1        */         64,
        /* Typeless_2        */         64,
        /* Typeless_4        */         64,
        /* Typeless_8        */         64,
        /* Long_Typeless     */         0,
        /* Integer_1         */         64,
        /* Integer_2         */         64,
        /* Integer_4         */         64,
        /* Integer_8         */         64,
        /* Real_4            */         64,
        /* Real_8            */         64,
        /* Real_16           */         64,
        /* Complex_4         */         64,
        /* Complex_8         */         64,
        /* Complex_16        */         64,
        /* CRI_Ptr_8         */         64,
        /* Logical_1         */         64,
        /* Logical_2         */         64,
        /* Logical_4         */         64,
        /* Logical_8         */         64,
        /* Character_1       */         8,
        /* Character_2       */         16,
        /* Character_4       */         32,
        /* CRI_Ch_Ptr_8      */         64,
        /* Structure_Type    */         64,
        /* CRI_Parcel_Ptr_8  */         64
        };

int     type_alignment_tbl[Num_Linear_Types] = {
        /* Err_Res           */         No_Align,
        /* Short_Char_Const  */         No_Align,
        /* Short_Typeless_Const    */   No_Align,
        /* Typeless_1        */         No_Align,
        /* Typeless_2        */         No_Align,
        /* Typeless_4        */         No_Align,
        /* Typeless_8        */         No_Align,
        /* Long_Typeless     */         No_Align,
        /* Integer_1         */         No_Align,
        /* Integer_2         */         No_Align,
        /* Integer_4         */         No_Align,
        /* Integer_8         */         No_Align,
        /* Real_4            */         No_Align,
        /* Real_8            */         No_Align,
        /* Real_16           */         No_Align,
        /* Complex_4         */         No_Align,
        /* Complex_8         */         No_Align,
        /* Complex_16        */         No_Align,
        /* CRI_Ptr_8         */         No_Align,
        /* Logical_1         */         No_Align,
        /* Logical_2         */         No_Align,
        /* Logical_4         */         No_Align,
        /* Logical_8         */         No_Align,
        /* Character_1       */         No_Align,
        /* Character_2       */         No_Align,
        /* Character_4       */         No_Align,
        /* CRI_Ch_Ptr_8      */         No_Align,
        /* Structure_Type    */         No_Align,
        /* CRI_Parcel_Ptr_8  */         No_Align
        };



# elif defined(_TARGET_OS_MAX)
int	storage_bit_kind_tbl[Num_Linear_Types] = {        
	/* Err_Res           */  	0,
        /* Short_Char_Const  */ 	0,
        /* Short_Typeless_Const    */ 	0,
        /* Typeless_1        */ 	4,
        /* Typeless_2        */ 	4,
        /* Typeless_4        */ 	4,
        /* Typeless_8        */ 	8,
        /* Long_Typeless     */ 	0,
        /* Integer_1         */ 	1,
        /* Integer_2         */ 	2,
        /* Integer_4         */ 	4,
        /* Integer_8         */ 	8,
        /* Real_4            */ 	4,
        /* Real_8            */ 	8,
        /* Real_16           */ 	16,
        /* Complex_4         */ 	4,
        /* Complex_8         */ 	8,
        /* Complex_16        */ 	16,
        /* CRI_Ptr_8         */ 	8,
        /* Logical_1         */ 	1,
        /* Logical_2         */ 	2,
        /* Logical_4         */ 	4,
        /* Logical_8         */ 	8,
        /* Character_1       */ 	1,
        /* Character_2       */ 	2,
        /* Character_4       */ 	4,
        /* CRI_Ch_Ptr_8      */ 	8,
        /* Structure_Type    */ 	0,
	/* CRI_Parcel_Ptr_8  */		8
	                                         };

/****************************************************\
|* storage_bit_size_tbl[Complex_4] does not reflect *|
|* the size of a Complex_4 constant in our cn table *|
|* on the t3e. These are stored in two 64 bit words.*|
\****************************************************/

int	storage_bit_size_tbl[Num_Linear_Types] = {        
	/* Err_Res           */  	0,
        /* Short_Char_Const  */ 	64,
        /* Short_Typeless_Const    */ 	64,
        /* Typeless_1        */ 	32,
        /* Typeless_2        */ 	32,
        /* Typeless_4        */ 	32,
        /* Typeless_8        */ 	64,
        /* Long_Typeless     */ 	0,
        /* Integer_1         */ 	32,
        /* Integer_2         */ 	32,
        /* Integer_4         */ 	32,
        /* Integer_8         */ 	64,
        /* Real_4            */ 	32,
        /* Real_8            */ 	64,
        /* Real_16           */ 	128,
        /* Complex_4         */ 	64,
        /* Complex_8         */ 	128,
        /* Complex_16        */ 	256,
        /* CRI_Ptr_8         */ 	64,
        /* Logical_1         */ 	32,
        /* Logical_2         */ 	32,
        /* Logical_4         */ 	32,
        /* Logical_8         */ 	64,
        /* Character_1       */ 	0,
        /* Character_2       */ 	0,
        /* Character_4       */ 	0,
        /* CRI_Ch_Ptr_8      */ 	64,
        /* Structure_Type    */ 	0,
	/* CRI_Parcel_Ptr_8  */		64
	                                         };

int     storage_bit_prec_tbl[Num_Linear_Types] = {
        /* Err_Res           */         0,
        /* Short_Char_Const  */         8,
        /* Short_Typeless_Const    */   0,
        /* Typeless_1        */         0,
        /* Typeless_2        */         0,
        /* Typeless_4        */         0,
        /* Typeless_8        */         0,
        /* Long_Typeless     */         0,
        /* Integer_1         */         32,
        /* Integer_2         */         32,
        /* Integer_4         */         32,
        /* Integer_8         */         64,
        /* Real_4            */         24,
        /* Real_8            */         48,
        /* Real_16           */         96,
        /* Complex_4         */         24,
        /* Complex_8         */         48,
        /* Complex_16        */         96,
        /* CRI_Ptr_8         */         64,
        /* Logical_1         */         32,
        /* Logical_2         */         32,
        /* Logical_4         */         32,
        /* Logical_8         */         64,
        /* Character_1       */         8,
        /* Character_2       */         8,
        /* Character_4       */         8,
        /* CRI_Ch_Ptr_8      */         8,
        /* Structure_Type    */         0,
        /* CRI_Parcel_Ptr_8  */         8
                                                 };

int     register_bit_size_tbl[Num_Linear_Types] = {
        /* Err_Res           */         0,
        /* Short_Char_Const  */         64,
        /* Short_Typeless_Const    */   64,
        /* Typeless_1        */         32,
        /* Typeless_2        */         32,
        /* Typeless_4        */         32,
        /* Typeless_8        */         64,
        /* Long_Typeless     */         0,
        /* Integer_1         */         32,
        /* Integer_2         */         32,
        /* Integer_4         */         32,
        /* Integer_8         */         64,
        /* Real_4            */         32,
        /* Real_8            */         64,
        /* Real_16           */         64,
        /* Complex_4         */         64,
        /* Complex_8         */         64,
        /* Complex_16        */         64,
        /* CRI_Ptr_8         */         64,
        /* Logical_1         */         64,
        /* Logical_2         */         64,
        /* Logical_4         */         64,
        /* Logical_8         */         64,
        /* Character_1       */         0,
        /* Character_2       */         0,
        /* Character_4       */         0,
        /* CRI_Ch_Ptr_8      */         64,
        /* Structure_Type    */         0,
        /* CRI_Parcel_Ptr_8  */         64
                                                 };

int     stride_mult_unit_in_bits[Num_Linear_Types] = {
        /* Err_Res           */         0,
        /* Short_Char_Const  */         8,
        /* Short_Typeless_Const    */   64,
        /* Typeless_1        */         32,
        /* Typeless_2        */         32,
        /* Typeless_4        */         32,
        /* Typeless_8        */         64,
        /* Long_Typeless     */         0,
        /* Integer_1         */         32,
        /* Integer_2         */         32,
        /* Integer_4         */         32,
        /* Integer_8         */         64,
        /* Real_4            */         32,
        /* Real_8            */         64,
        /* Real_16           */         64,
        /* Complex_4         */         64,
        /* Complex_8         */         64,
        /* Complex_16        */         64,
        /* CRI_Ptr_8         */         64,
        /* Logical_1         */         32,
        /* Logical_2         */         32,
        /* Logical_4         */         32,
        /* Logical_8         */         64,
        /* Character_1       */         8,
        /* Character_2       */         16,
        /* Character_4       */         32,
        /* CRI_Ch_Ptr_8      */         64,
        /* Structure_Type    */         64,
        /* CRI_Parcel_Ptr_8  */         64
        };

int     type_alignment_tbl[Num_Linear_Types] = {
        /* Err_Res           */         No_Align,
        /* Short_Char_Const  */         No_Align,
        /* Short_Typeless_Const    */   No_Align,
        /* Typeless_1        */         No_Align,
        /* Typeless_2        */         No_Align,
        /* Typeless_4        */         No_Align,
        /* Typeless_8        */         No_Align,
        /* Long_Typeless     */         No_Align,
        /* Integer_1         */         No_Align,
        /* Integer_2         */         No_Align,
        /* Integer_4         */         No_Align,
        /* Integer_8         */         No_Align,
        /* Real_4            */         No_Align,
        /* Real_8            */         No_Align,
        /* Real_16           */         No_Align,
        /* Complex_4         */         No_Align,
        /* Complex_8         */         No_Align,
        /* Complex_16        */         No_Align,
        /* CRI_Ptr_8         */         No_Align,
        /* Logical_1         */         No_Align,
        /* Logical_2         */         No_Align,
        /* Logical_4         */         No_Align,
        /* Logical_8         */         No_Align,
        /* Character_1       */         No_Align,
        /* Character_2       */         No_Align,
        /* Character_4       */         No_Align,
        /* CRI_Ch_Ptr_8      */         No_Align,
        /* Structure_Type    */         No_Align,
        /* CRI_Parcel_Ptr_8  */         No_Align
        };



# else
int	storage_bit_kind_tbl[Num_Linear_Types] = {        
	/* Err_Res           */  	0,
        /* Short_Char_Const  */ 	0,
        /* Short_Typeless_Const    */ 	0,
        /* Typeless_1        */ 	1,
        /* Typeless_2        */ 	2,
        /* Typeless_4        */ 	4,
        /* Typeless_8        */ 	8,
        /* Long_Typeless     */ 	0,
        /* Integer_1         */ 	1,
        /* Integer_2         */ 	2,
        /* Integer_4         */ 	4,
        /* Integer_8         */ 	8,
        /* Real_4            */ 	4,
        /* Real_8            */ 	8,
        /* Real_16           */ 	16,
        /* Complex_4         */ 	4,
        /* Complex_8         */ 	8,
        /* Complex_16        */ 	16,
        /* CRI_Ptr_8         */ 	8,
        /* Logical_1         */ 	1,
        /* Logical_2         */ 	2,
        /* Logical_4         */ 	4,
        /* Logical_8         */ 	8,
        /* Character_1       */ 	1,
        /* Character_2       */ 	2,
        /* Character_4       */ 	4,
        /* CRI_Ch_Ptr_8      */ 	8,
        /* Structure_Type    */ 	0,
	/* CRI_Parcel_Ptr_8  */		8
	                                         };

int     storage_bit_size_tbl[Num_Linear_Types] = {
        /* Err_Res           */         0,
        /* Short_Char_Const  */         32,
        /* Short_Typeless_Const    */   32,
        /* Typeless_1        */         32,
        /* Typeless_2        */         32,
        /* Typeless_4        */         32,
        /* Typeless_8        */         64,
        /* Long_Typeless     */         0,
        /* Integer_1         */         32,
        /* Integer_2         */         32,
        /* Integer_4         */         32,
        /* Integer_8         */         64,
        /* Real_4            */         32,
        /* Real_8            */         64,
        /* Real_16           */         128,
        /* Complex_4         */         64,
        /* Complex_8         */         128,
        /* Complex_16        */         256,
        /* CRI_Ptr_8         */         32,
        /* Logical_1         */         32,
        /* Logical_2         */         32,
        /* Logical_4         */         32,
        /* Logical_8         */         64,
        /* Character_1       */         0,
        /* Character_2       */         0,
        /* Character_4       */         0,
        /* CRI_Ch_Ptr_8      */         64,
        /* Structure_Type    */         0,
	/* CRI_Parcel_Ptr_8  */		32
                                                 };

int     storage_bit_prec_tbl[Num_Linear_Types] = {
        /* Err_Res           */         0,
        /* Short_Char_Const  */         8,
        /* Short_Typeless_Const*/       32,
        /* Typeless_1        */         32,
        /* Typeless_2        */         32,
        /* Typeless_4        */         32,
        /* Typeless_8        */         64,
        /* Long_Typeless     */         0,
        /* Integer_1         */         32,
        /* Integer_2         */         32,
        /* Integer_4         */         32,
        /* Integer_8         */         64,
        /* Real_4            */         24,
        /* Real_8            */         48,
        /* Real_16           */         96,
        /* Complex_4         */         24,
        /* Complex_8         */         48,
        /* Complex_16        */         96,
        /* CRI_Ptr_8         */         32,
        /* Logical_1         */         32,
        /* Logical_2         */         32,
        /* Logical_4         */         32,
        /* Logical_8         */         64,
        /* Character_1       */         8,
        /* Character_2       */         8,
        /* Character_4       */         8,
        /* CRI_Ch_Ptr_8      */         8,
        /* Structure_Type    */         0,
        /* CRI_Parcel_Ptr_8    */       8
                                                  };

int     register_bit_size_tbl[Num_Linear_Types] = {
        /* Err_Res           */         0,
        /* Short_Char_Const  */         64,
        /* Short_Typeless_Const    */   64,
        /* Typeless_1        */         32,
        /* Typeless_2        */         32,
        /* Typeless_4        */         32,
        /* Typeless_8        */         64,
        /* Long_Typeless     */         0,
        /* Integer_1         */         32,
        /* Integer_2         */         32,
        /* Integer_4         */         32,
        /* Integer_8         */         64,
        /* Real_4            */         32,
        /* Real_8            */         64,
        /* Real_16           */         64,
        /* Complex_4         */         64,
        /* Complex_8         */         64,
        /* Complex_16        */         64,
        /* CRI_Ptr_8         */         64,
        /* Logical_1         */         64,
        /* Logical_2         */         64,
        /* Logical_4         */         64,
        /* Logical_8         */         64,
        /* Character_1       */         0,
        /* Character_2       */         0,
        /* Character_4       */         0,
        /* CRI_Ch_Ptr_8      */         64,
        /* Structure_Type    */         0,
        /* CRI_Parcel_Ptr_8  */         64
                                                 };

int     stride_mult_unit_in_bits[Num_Linear_Types] = {
        /* Err_Res           */         0,
        /* Short_Char_Const  */         8,
        /* Short_Typeless_Const    */   32,
        /* Typeless_1        */         32,
        /* Typeless_2        */         32,
        /* Typeless_4        */         32,
# ifdef _WHIRL_HOST64_TARGET64
        /* Typeless_8        */         64,
# else
        /* Typeless_8        */         32,
# endif /* _WHIRL_HOST64_TARGET64 */
        /* Long_Typeless     */         0,
        /* Integer_1         */         32,
        /* Integer_2         */         32,
        /* Integer_4         */         32,
# ifdef _WHIRL_HOST64_TARGET64
        /* Integer_8         */         64,
# else
        /* Integer_8         */         32,
# endif /* _WHIRL_HOST64_TARGET64 */
        /* Real_4            */         32,
# ifdef _WHIRL_HOST64_TARGET64
        /* Real_8            */         64,
        /* Real_16           */         64,
        /* Complex_4         */         64,
        /* Complex_8         */         64,
        /* Complex_16        */         64,
        /* CRI_Ptr_8         */         64,
# else
        /* Real_8            */         32,
        /* Real_16           */         32,
        /* Complex_4         */         32,
        /* Complex_8         */         32,
        /* Complex_16        */         32,
        /* CRI_Ptr_8         */         32,
# endif /* _WHIRL_HOST64_TARGET64 */
        /* Logical_1         */         32,
        /* Logical_2         */         32,
        /* Logical_4         */         32,
# ifdef _WHIRL_HOST64_TARGET64
        /* Logical_8         */         64,
# else
        /* Logical_8         */         32,
# endif /* _WHIRL_HOST64_TARGET64 */
        /* Character_1       */         8,
        /* Character_2       */         16,
        /* Character_4       */         32,
# ifdef _WHIRL_HOST64_TARGET64
        /* CRI_Ch_Ptr_8      */         64,
# else
        /* CRI_Ch_Ptr_8      */         32,
# endif /* _WHIRL_HOST64_TARGET64 */
        /* Structure_Type    */         32,
        /* CRI_Parcel_Ptr_8    */       32
                                                };

int     type_alignment_tbl[Num_Linear_Types] = {
        /* Err_Res           */         No_Align,
        /* Short_Char_Const  */         Align_Bit,
        /* Short_Typeless_Const    */   Align_Bit,
        /* Typeless_1        */         Align_8,
        /* Typeless_2        */         Align_16,
        /* Typeless_4        */         Align_32,
        /* Typeless_8        */         Align_64,
        /* Long_Typeless     */         WORD_ALIGN,
        /* Integer_1         */         Align_8,
        /* Integer_2         */         Align_16,
        /* Integer_4         */         Align_32,
        /* Integer_8         */         Align_64,
        /* Real_4            */         Align_32,
        /* Real_8            */         Align_64,
        /* Real_16           */         Align_128,
        /* Complex_4         */         Align_32,
        /* Complex_8         */         Align_64,
        /* Complex_16        */         Align_128,
        /* CRI_Ptr_8         */         Align_64,
        /* Logical_1         */         Align_8,
        /* Logical_2         */         Align_16,
        /* Logical_4         */         Align_32,
        /* Logical_8         */         Align_64,
        /* Character_1       */         Align_Bit,
        /* Character_2       */         Align_Bit,
        /* Character_4       */         Align_Bit,
        /* CRI_Ch_Ptr_8      */         Align_64,
        /* Structure_Type    */         No_Align,
        /* CRI_Parcel_Ptr_8  */         Align_64
        };



# endif


        /* Table used to initialize the type table default.  The first  */
        /* entries in the type table, correspond to the linear_types.   */
        /* This table is read up as longs, and copied to the type table */
        /* in parse_prog_unit.   Then const_one_idx is entered into the */
        /* default Character entry at runtime.                          */
	/* TYP_DECLARED_DBL is set here.                                */

type_tbl_type		type_init_tbl[Num_Linear_Types + 2] = {

		{Integer,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
 		 Err_Res,		0},

		{Character,		0,			NO_Tbl_Idx,
		 Const_Len_Char,	FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Short_Char_Const,	0},

		{Typeless,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Short_Typeless_Const,	TARGET_BITS_PER_WORD},

		{Typeless,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Typeless_1,	        8},

		{Typeless,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Typeless_2,	        16},

		{Typeless,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Typeless_4,	        32},

		{Typeless,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Typeless_8,	        64},

		{Typeless,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Long_Typeless,		0},

		{Integer,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Integer_1,		0},

		{Integer,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Integer_2,		0},

		{Integer,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Integer_4,		0},

		{Integer,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Integer_8,		0},

		{Real,			0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Real_4,		0},

		{Real,			0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Real_8,		0},

		{Real,			0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Real_16,		0},

		{Complex,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Complex_4,		0},

		{Complex,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Complex_8,		0},

		{Complex,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Complex_16,		0},

		{CRI_Ptr,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 CRI_Ptr_8,		TARGET_BITS_PER_WORD},

		{Logical,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Logical_1,		0},

		{Logical,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Logical_2,		0},

		{Logical,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Logical_4,		0},

		{Logical,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Logical_8,		0},

		{Character,		0,			NO_Tbl_Idx,
		 Const_Len_Char,	FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Character_1,		0},

		{Character,		0,			NO_Tbl_Idx,
		 Const_Len_Char,	FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Character_2,		0},

		{Character,		0,			NO_Tbl_Idx,
		 Const_Len_Char,	FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Character_4,		0},

		{CRI_Ch_Ptr,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 CRI_Ch_Ptr_8,		0},

		{Structure,		0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 Structure_Type,	0},

		{CRI_Parcel_Ptr,	0,			NO_Tbl_Idx,
		 Unknown_Char,		FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 FALSE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
		 CRI_Parcel_Ptr_8,	0},

/* this is the DOUBLE_DEFAULT_TYPE entry. keywords "DOUBLE PRECISION" only */

                {Real,			0,                      NO_Tbl_Idx,
                 Unknown_Char,          FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 TRUE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
                 Real_8,	        0},

/* this is the DOUBLE_COMPLEX_DEFAULT_TYPE entry.  "DOUBLE COMPLEX" only */

                {Complex,		0,                      NO_Tbl_Idx,
                 Unknown_Char,          FALSE,			FALSE,
		 FALSE,			FALSE,			FALSE,
		 TRUE,
		 0,			Default_Typed,		NO_Tbl_Idx, 0,
                 Complex_8,	        0},

};
