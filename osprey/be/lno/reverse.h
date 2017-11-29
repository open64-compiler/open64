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


// -*-C++-*-

/** Description: 
***
***   This file contains the description of loop reversal functions provided
***   in the LNO phase of the optimizer.  
***
***   Currently only simple cases are handled, i.e. those for which the loop
***   bounds have the form: 
***
*** 	for (i = exp1; i <= exp2; i++) 
***
***   More complex cases will be added if it is deemed worthwhile. 
***
***   Two external functions are provided: 
***
***	BOOL RV_Is_Legal(WN* wn_loop)
***
***	  Returns TRUE if reversing loop 'wn_loop' is legal, FALSE
***       otherwise. 
***
***	void RV_Reverse_Loop(WN* wn_loop)
*** 
***	  Reverse the loop 'wn_loop'.  We assume that the loop has
***	  already been determined to be reversible.
***
***   One other external function is used for testing only: 
*** 
***	void Reverse_Loops(WN* func_nd)
***
***	  Reverse every loop in the 'func_nd' for which loop reversal
***	  is legal.
***
***	BOOL Do_Loop_Is_Backward(WN* wn_loop)
***
***	  Returns TRUE if every do loop index variable in the body of 
***	  the loop 'wn_loop' is provably negative, FALSE otherwise. 
***
***
**/

extern BOOL RV_Is_Legal(WN* wn_loop);
extern void RV_Reverse_Loop(WN* wn_loop);
extern void Reverse_Loops(WN* func_nd);
extern BOOL Do_Loop_Is_Backward(WN* wn_loop); 
extern BOOL Do_Loop_Is_Regular(WN* wn_loop); 


