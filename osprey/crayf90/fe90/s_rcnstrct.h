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



/* USMID:  "\n@(#)5.0_pl/headers/s_rcnstrct.h	5.1	04/29/99 21:22:31\n" */

extern exp_tbl_type    bin_add_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    bin_sub_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    mult_div_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    power_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    eq_ne_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    gt_lt_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    and_or_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    asg_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    un_plus_tbl[Num_Linear_Types];
extern exp_tbl_type    not_tbl[Num_Linear_Types];

extern void    (*uminus_folders[Num_Linear_Types]) ();
extern void    (*plus_folders[Num_Linear_Types][Num_Linear_Types]) ();
extern void    (*mult_folders[Num_Linear_Types][Num_Linear_Types]) ();
extern void    (*minus_folders[Num_Linear_Types][Num_Linear_Types]) ();
extern void    (*div_folders[Num_Linear_Types][Num_Linear_Types]) ();
extern void    (*power_folders[Num_Linear_Types][Num_Linear_Types]) ();
extern void    (*eq_folders[Num_Linear_Types][Num_Linear_Types]) ();
extern void    (*lx_folders[Num_Linear_Types][Num_Linear_Types]) ();
