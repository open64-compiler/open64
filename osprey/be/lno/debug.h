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


/**
*** Description:
***
***   This file contains information about the Whirl Browser, which can be 
***   used to interactively debug programs while running the LNO Phase.  
***
***   To invoke the whirl browser from dbx, type "p debug()".  You can get 
***   a list of available options by typing 'H' at the prompt "WB>". 
***
***  Exported functions: 
***
***   void WB_LNO_Initialize(WN* global_fd, INT sanity_check_level)
***
***	Initializes the whirl browser to operated on the program unit 
***     'global_fd' and sets the 'sanity_check_level'. 
***
***   void WB_LNO_Terminate();
***
***     Makes the whirl browser inoperative. 
***
***   void WB_Set_Sanity_Check_Level(INT sanity_check_level) 
***	
***	Set the sanity check level to value 'sanity_check_level'. 
***	The following are valid values: 
***	  WBC_DISABLE => Disable sanity checking 
***	  WBC_DU_ONLY => Do DU sanity checking only 
***	  WBC_DU_AND_ARRAY => Check DU and array dependences 
***	  WBC_FULL_SNL => Do Full SNL sanity checking 
***	Sanity checking is invoked by using the 'C' command. 
***   
***   const char* WB_Whirl_Symbol(WN* wn, BOOL print_type=FALSE)
***
***	 A printable string of characters for whirl node 'wn'. For 
***	 loads and stores, the symbol is printed, if any.  For do loops,
***	 the symbol of the do loop is printed.  
***
***   extern BOOL WB_Dep_Symbol(WN* wn, char buffer[], INT max_string)
***
***	 Write an abbreviated version of node 'wn' to the first 
***	 'max_string' characters of 'buffer'. 
**/

#ifndef debug_INCLUDED
#define debug_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

#define WBC_MIN		        0
#define WBC_DISABLE		0
#define WBC_DU_ONLY		1
#define WBC_DU_AND_ARRAY	2
#define WBC_FULL_SNL		3
#define WBC_MAX			3
 
extern void WB_LNO_Initialize(WN* global_fd, struct DU_MANAGER* du_mgr, 
  struct ALIAS_MANAGER* alias_mgr, INT sanity_check_level); 
extern void WB_LNO_Terminate(void); 
extern void WB_Set_Sanity_Check_Level(INT sanity_check_level); 
extern const char* WB_Whirl_Symbol(WN* wn); 
extern const char* WB_Whirl_Symbol_Type(WN* wn); 
extern void Lisp_Loops(WN* wn_root, FILE *fp);
extern BOOL WB_Dep_Symbol(WN* wn, char buffer[], INT max_string);

#ifdef __cplusplus
}
#endif


#endif /* debug_INCLUDED */

