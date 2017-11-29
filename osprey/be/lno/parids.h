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

/**
*** Description:
***
***	This file contains descriptions of functions used to assign MPNUMs
***	for the subroutines which will be created when the MP lowerer is 
***	invoked on DOACROSSes and PARALLEL REGIONs. 
***
***  Exported types and functions:
***
***	WN* WN_Array_Symbol(WN* wn_ref)
***
***	    For the ILOAD or ISTORE 'wn_ref', returns the node which
***	    contains the symbol of 'wn_ref'.
***
***	void Annotate_For_Mp_Lowering(PU_Info* current_pu, WN* func_nd)
***
***	    Annotate the function 'func_nd' with information about what
***	    functions will be created when the doacrosses and parallel regions
***         are lowered.
***
***	void Print_Prompl_Msgs(PU_Info* current_pu, WN* func_nd) 
***
***	    Print messages for prompf .l file in human readable form.
**/ 

#ifndef parids_INCLUDED
#define parids_INCLUDED "parids.h"

extern WN* WN_Array_Symbol(WN* wn_ref); 

extern void Annotate_For_Mp_Lowering(PU_Info* current_pu, WN* func_nd); 

extern void Print_Prompl_Msgs(PU_Info* current_pu, WN* func_nd); 

#endif /* parids_INCLUDED */

