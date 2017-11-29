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



/* USMID:  "\n@(#)5.0_pl/headers/fold_drive.h	5.2	07/16/99 10:25:58\n" */


# ifdef _TARGET32

int	num_host_wds[Num_Linear_Types] = {
        	/* Err_Res          	*/		0,
        	/* Short_Char_Const 	*/		1,
        	/* Short_Typeless_Const */		1,
        	/* Typeless_1  		*/		1,
        	/* Typeless_2  		*/		1,
        	/* Typeless_4  		*/		1,
        	/* Typeless_8  		*/		2,
        	/* Long_Typeless    	*/		0,
        	/* Integer_1        	*/		1,
        	/* Integer_2        	*/		1,
        	/* Integer_4        	*/		1,
        	/* Integer_8        	*/		2,
        	/* Real_4           	*/		1,
        	/* Real_8           	*/		2,
        	/* Real_16          	*/		4,
        	/* Complex_4        	*/		2,
        	/* Complex_8        	*/		4,
        	/* Complex_16       	*/		8,
        	/* CRI_Ptr_8        	*/		1,
        	/* Logical_1        	*/		1,
        	/* Logical_2        	*/		1,
        	/* Logical_4        	*/		1,
        	/* Logical_8        	*/		2,
        	/* Character_1      	*/		1,
        	/* Character_2      	*/		1,
        	/* Character_4      	*/		1,
        	/* CRI_Ch_Ptr_8     	*/		1,
        	/* Structure_Type   	*/		0,
        	/* CRI_Parcel_Ptr_8 	*/		1,
                };

# else 

int	num_host_wds[Num_Linear_Types] = {
        	/* Err_Res          	*/		0,
        	/* Short_Char_Const 	*/		1,
        	/* Short_Typeless_Const */		1,
        	/* Typeless_1  		*/		1,
        	/* Typeless_2  		*/		1,
        	/* Typeless_4  		*/		1,
        	/* Typeless_8  		*/		1,
        	/* Long_Typeless    	*/		0,
        	/* Integer_1        	*/		1,
        	/* Integer_2        	*/		1,
        	/* Integer_4        	*/		1,
        	/* Integer_8        	*/		1,
        	/* Real_4           	*/		1,
        	/* Real_8           	*/		1,
        	/* Real_16          	*/		2,
# ifdef _WHIRL_HOST64_TARGET64
        	/* Complex_4        	*/		1,
# else
        	/* Complex_4        	*/		2,
# endif /* _WHIRL_HOST64_TARGET64 */
        	/* Complex_8        	*/		2,
        	/* Complex_16       	*/		4,
        	/* CRI_Ptr_8        	*/		1,
        	/* Logical_1        	*/		1,
        	/* Logical_2        	*/		1,
        	/* Logical_4        	*/		1,
        	/* Logical_8        	*/		1,
        	/* Character_1      	*/		1,
        	/* Character_2      	*/		1,
        	/* Character_4      	*/		1,
        	/* CRI_Ch_Ptr_8     	*/		1,
        	/* Structure_Type   	*/		0,
        	/* CRI_Parcel_Ptr_8 	*/		1,
                };
# endif


extern exp_tbl_type     bin_add_tbl[Num_Linear_Types][Num_Linear_Types];
extern long 		input_arith_type[Num_Linear_Types];
