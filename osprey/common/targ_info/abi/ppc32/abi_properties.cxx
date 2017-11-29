/*
 * Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
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


//  
//  Generate ABI information
///////////////////////////////////////


//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/abi/x8664/abi_properties.cxx,v $

#include <stddef.h>
#include "abi_properties_gen.h"
#include "targ_isa_registers.h"

static ABI_PROPERTY
  allocatable,
  callee,
  caller,
  func_arg, // argu
  func_val, // return value 
  stack_ptr,
  frame_ptr,
  global_ptr,
  ret_addr;


static void ppc32_32_abi(void)
{
  /* The order is assigned to favor the parameter passing. */
  static const char* integer_names[] = {
    "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7", 
    "8",  "9",  "10", "11", "12", "13", "14", "15", 
    "16", "17", "18", "19", "20", "21", "22", "23", 
    "24", "25", "26", "27", "28", "29", "30", "31" 
  };

  // Floating-Point Registers
  static const char *fp_reg_names[32] = {
      "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7", 
      "8",  "9",  "10", "11", "12", "13", "14", "15", 
      "16", "17", "18", "19", "20", "21", "22", "23", 
      "24", "25", "26", "27", "28", "29", "30", "31" 
  };
  
  // Special Purpose Register
  static const char * sp_reg_name[5] = {
      "fpscr", "xer", "lr", "ctr", "cr",
  };
  
  // ISA_REGISTER_CLASS_integer
  Reg_Names(ISA_REGISTER_CLASS_integer, 0, 31, integer_names);

  Reg_Property(allocatable, ISA_REGISTER_CLASS_integer,
    3, 4, 5, 6, 7, 8, 9, 10,
    11,12,   14,15,16,17,18,
    19,20,21,22,23,24,25,26,
    27,28,29,30,31,
    -1);

  Reg_Property(caller, ISA_REGISTER_CLASS_integer,
    3, 4, 5, 6, 7, 8, 9, 10, 
    11, 12,
    -1);

  Reg_Property(callee, ISA_REGISTER_CLASS_integer,  // nonvolatile
    1,  14, 15, 16, 17, 18, 19, 20,
    21, 22, 23, 24, 25, 26, 27, 28,
    29, 30, 31, 
    -1);
  
  Reg_Property(func_arg, ISA_REGISTER_CLASS_integer,
    3, 4, 5, 6, 7, 8, 9, 10,    
    -1);

  Reg_Property(func_val, ISA_REGISTER_CLASS_integer,
    3, 4, -1);

  Reg_Property(stack_ptr, ISA_REGISTER_CLASS_integer, 
    1, -1);

  Reg_Property(frame_ptr, ISA_REGISTER_CLASS_integer, 
    31, -1);

  Reg_Property(global_ptr, ISA_REGISTER_CLASS_integer, 
    13, -1);
  

  // ISA_REGISTER_CLASS_float
  Reg_Property(allocatable, ISA_REGISTER_CLASS_float,
    0, 1, 2, 3, 4, 5, 6, 7, 
    8, 9, 10,11,12,13,14,15,
    16,17,18,19,20,21,22,23,
    24,25,26,27,28,29,30,31,
    -1);
  
  Reg_Property(caller, ISA_REGISTER_CLASS_float,
    0, 1, 2,  3,  4,  5,  6, 7, 
    8, 9, 10, 11, 12, 13, 
    -1);
  
  Reg_Property(callee, ISA_REGISTER_CLASS_float,
    14,15,
    16,17,18,19,20,21,22,23,
    24,25,26,27,28,29,30,31,
    -1);
  
  Reg_Property(func_arg, ISA_REGISTER_CLASS_float,
    1, 2, 3, 4, 5, 6, 7, 8, 
    -1);
  
  Reg_Property(func_val, ISA_REGISTER_CLASS_float,
    1, -1);

  // ISA_REGISTER_CLASS_special
  Reg_Property(ret_addr, ISA_REGISTER_CLASS_special, 
    2, -1);
}



static void ppc32_64_abi(void)
{
  // assert(0);
}


main()
{
  ABI_Properties_Begin("ppc32");

  allocatable = Create_Reg_Property("allocatable");
  callee      = Create_Reg_Property("callee");
  caller      = Create_Reg_Property("caller");
  func_arg    = Create_Reg_Property("func_arg");
  func_val    = Create_Reg_Property("func_val");
  stack_ptr   = Create_Reg_Property("stack_ptr");
  frame_ptr   = Create_Reg_Property("frame_ptr");
  ret_addr    = Create_Reg_Property("ret_addr");
  global_ptr  = Create_Reg_Property("global_ptr");
  
  Begin_ABI("p32");
  ppc32_32_abi();

  Begin_ABI("p64");
  ppc32_64_abi();

  ABI_Properties_End();
}
