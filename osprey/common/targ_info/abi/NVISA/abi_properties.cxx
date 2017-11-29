/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

/*

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

*/


//  
//  Generate ABI information
///////////////////////////////////////


//  $Revision: 1.22 $
//  $Date: 2001/03/10 01:15:59 $
//  $Author: mtibuild $
//  $Source: /osprey.src/osprey1.0/common/targ_info/abi/ia64/RCS/abi_properties.cxx,v $

#include <stddef.h>
#include "abi_properties_gen.h"
#include "targ_isa_registers.h"

static ABI_PROPERTY
	allocatable,
	func_arg,	// params
	func_val,	// retval
	unused,		// unused dummy result when only using cc
	tid,		// thread id
	ntid,		// number of thread ids
	ctaid,		// cta id
	nctaid,		// number of cta ids
	gridid,		// grid id
	clock;		// cycle counter

#define ISA_REGISTER_CLASS_Last_Reg(x) \
	ISA_REGISTER_CLASS_INFO_Last_Reg(ISA_REGISTER_CLASS_Info(x))

#define MAX_PARAM_REGS 16
#define MAX_RETURN_REGS 4

static const char *i32param_names[MAX_PARAM_REGS] = {
  "%ra1", "%ra2", "%ra3", "%ra4", "%ra5", "%ra6", "%ra7", "%ra8", "%ra9", 
  "%ra10", "%ra11", "%ra12", "%ra13", "%ra14", "%ra15", "%ra16"};
static const char *i32ret_names[MAX_RETURN_REGS] = {
  "%rv1", "%rv2", "%rv3", "%rv4"};
static const char *i64param_names[MAX_PARAM_REGS] = {
  "%rda1", "%rda2", "%rda3", "%rda4", "%rda5", "%rda6", "%rda7","%rda8","%rda9",
  "%rda10", "%rda11", "%rda12", "%rda13", "%rda14", "%rda15", "%rda16"};
static const char *i64ret_names[MAX_RETURN_REGS] = {
  "%rdv1", "%rdv2", "%rdv3", "%rdv4"};
static const char *f32param_names[MAX_PARAM_REGS] = {
  "%fa1", "%fa2", "%fa3", "%fa4", "%fa5", "%fa6", "%fa7", "%fa8", "%fa9", 
  "%fa10", "%fa11", "%fa12", "%fa13", "%fa14", "%fa15", "%fa16"};
static const char *f32ret_names[MAX_RETURN_REGS] = {
  "%fv1", "%fv2", "%fv3", "%fv4"};
static const char *f64param_names[MAX_PARAM_REGS] = {
  "%fda1", "%fda2", "%fda3", "%fda4", "%fda5", "%fda6", "%fda7","%fda8","%fda9",
  "%fda10", "%fda11", "%fda12", "%fda13", "%fda14", "%fda15", "%fda16"};
static const char *f64ret_names[MAX_RETURN_REGS] = {
  "%fdv1", "%fdv2", "%fdv3", "%fdv4"};

main()
{
  ABI_Properties_Begin("nvisa");

  allocatable = Create_Reg_Property("allocatable");
  func_arg = Create_Reg_Property("func_arg");
  func_val = Create_Reg_Property("func_val");
  unused = Create_Reg_Property("unused");
  tid = Create_Reg_Property("tid");
  ntid = Create_Reg_Property("ntid");
  ctaid = Create_Reg_Property("ctaid");
  nctaid = Create_Reg_Property("nctaid");
  gridid = Create_Reg_Property("gridid");
  clock = Create_Reg_Property("clock");


  ///////////////////////////////////////
  Begin_ABI("nvisa");

  // Would be simpler to put these special regs at front,
  // but more user-friendly to put at end so user sees r1,r2,etc
  INT tid_index = ISA_REGISTER_CLASS_Last_Reg(ISA_REGISTER_CLASS_integer16) 
	- 2; // includes last index
  INT ntid_index = tid_index - 3;
  INT ctaid_index = ntid_index - 3;
  INT nctaid_index = ctaid_index - 3;
  INT gridid_index = nctaid_index - 1;

  Set_Reg_Name(ISA_REGISTER_CLASS_integer16, tid_index, "%tid.x");
  Set_Reg_Name(ISA_REGISTER_CLASS_integer16, tid_index+1, "%tid.y");
  Set_Reg_Name(ISA_REGISTER_CLASS_integer16, tid_index+2, "%tid.z");
  Reg_Property_Range (tid, ISA_REGISTER_CLASS_integer16, 
	tid_index, tid_index+2);
  Set_Reg_Name(ISA_REGISTER_CLASS_integer16, ntid_index, "%ntid.x");
  Set_Reg_Name(ISA_REGISTER_CLASS_integer16, ntid_index+1, "%ntid.y");
  Set_Reg_Name(ISA_REGISTER_CLASS_integer16, ntid_index+2, "%ntid.z");
  Reg_Property_Range (ntid, ISA_REGISTER_CLASS_integer16, 
	ntid_index, ntid_index+2);
  Set_Reg_Name(ISA_REGISTER_CLASS_integer16, ctaid_index, "%ctaid.x");
  Set_Reg_Name(ISA_REGISTER_CLASS_integer16, ctaid_index+1, "%ctaid.y");
  Set_Reg_Name(ISA_REGISTER_CLASS_integer16, ctaid_index+2, "%ctaid.z");
  Reg_Property_Range (ctaid, ISA_REGISTER_CLASS_integer16, 
	ctaid_index, ctaid_index+2);
  Set_Reg_Name(ISA_REGISTER_CLASS_integer16, nctaid_index, "%nctaid.x");
  Set_Reg_Name(ISA_REGISTER_CLASS_integer16, nctaid_index+1, "%nctaid.y");
  Set_Reg_Name(ISA_REGISTER_CLASS_integer16, nctaid_index+2, "%nctaid.z");
  Reg_Property_Range (nctaid, ISA_REGISTER_CLASS_integer16, 
	nctaid_index, nctaid_index+2);
  Set_Reg_Name(ISA_REGISTER_CLASS_integer16, gridid_index, "%gridid");
  Reg_Property (gridid, ISA_REGISTER_CLASS_integer16, gridid_index, -1);

  INT clock_index = ISA_REGISTER_CLASS_Last_Reg(ISA_REGISTER_CLASS_integer);
  INT i32param_index = clock_index - MAX_PARAM_REGS;
  INT i32ret_index = i32param_index - MAX_RETURN_REGS;

  Set_Reg_Name(ISA_REGISTER_CLASS_integer, clock_index, "%clock");
  Reg_Property (clock, ISA_REGISTER_CLASS_integer, clock_index, -1);
  Reg_Names(ISA_REGISTER_CLASS_integer, i32param_index, 
	i32param_index+MAX_PARAM_REGS-1, i32param_names);
  Reg_Property_Range (func_arg, ISA_REGISTER_CLASS_integer, 
	i32param_index, i32param_index+MAX_PARAM_REGS-1);
  Reg_Names(ISA_REGISTER_CLASS_integer, i32ret_index, 
	i32ret_index+MAX_RETURN_REGS-1, i32ret_names);
  Reg_Property_Range (func_val, ISA_REGISTER_CLASS_integer, 
	i32ret_index, i32ret_index+MAX_RETURN_REGS-1);

  INT i64param_index = 
    ISA_REGISTER_CLASS_Last_Reg(ISA_REGISTER_CLASS_integer64) - MAX_PARAM_REGS;
  INT i64ret_index = i64param_index - MAX_RETURN_REGS;
  Reg_Names(ISA_REGISTER_CLASS_integer64, i64param_index, 
	i64param_index+MAX_PARAM_REGS-1, i64param_names);
  Reg_Property_Range (func_arg, ISA_REGISTER_CLASS_integer64, 
	i64param_index, i64param_index+MAX_PARAM_REGS-1);
  Reg_Names(ISA_REGISTER_CLASS_integer64, i64ret_index, 
	i64ret_index+MAX_RETURN_REGS-1, i64ret_names);
  Reg_Property_Range (func_val, ISA_REGISTER_CLASS_integer64, 
	i64ret_index, i64ret_index+MAX_RETURN_REGS-1);

  INT f32param_index = 
    ISA_REGISTER_CLASS_Last_Reg(ISA_REGISTER_CLASS_float) - MAX_PARAM_REGS;
  INT f32ret_index = f32param_index - MAX_RETURN_REGS;
  Reg_Names(ISA_REGISTER_CLASS_float, f32param_index, 
	f32param_index+MAX_PARAM_REGS-1, f32param_names);
  Reg_Property_Range (func_arg, ISA_REGISTER_CLASS_float, 
	f32param_index, f32param_index+MAX_PARAM_REGS-1);
  Reg_Names(ISA_REGISTER_CLASS_float, f32ret_index, 
	f32ret_index+MAX_RETURN_REGS-1, f32ret_names);
  Reg_Property_Range (func_val, ISA_REGISTER_CLASS_float, 
	f32ret_index, f32ret_index+MAX_RETURN_REGS-1);

  INT f64param_index = 
    ISA_REGISTER_CLASS_Last_Reg(ISA_REGISTER_CLASS_float64) - MAX_PARAM_REGS;
  INT f64ret_index = f64param_index - MAX_RETURN_REGS;
  Reg_Names(ISA_REGISTER_CLASS_float64, f64param_index, 
	f64param_index+MAX_PARAM_REGS-1, f64param_names);
  Reg_Property_Range (func_arg, ISA_REGISTER_CLASS_float64, 
	f64param_index, f64param_index+MAX_PARAM_REGS-1);
  Reg_Names(ISA_REGISTER_CLASS_float64, f64ret_index, 
	f64ret_index+MAX_RETURN_REGS-1, f64ret_names);
  Reg_Property_Range (func_val, ISA_REGISTER_CLASS_float64, 
	f64ret_index, f64ret_index+MAX_RETURN_REGS-1);

  // for now, no abi so don't need anything defined
  // except allocatable
  Reg_Property_Range (allocatable, ISA_REGISTER_CLASS_integer16, 1,
	gridid_index - 1); // ends before gridid
  Reg_Property_Range (allocatable, ISA_REGISTER_CLASS_integer, 1,
	i32ret_index - 1); // ends before retval 
  Reg_Property_Range (allocatable, ISA_REGISTER_CLASS_integer64, 1,
	i64ret_index - 1);
  Reg_Property_Range (allocatable, ISA_REGISTER_CLASS_float, 1,
	f32ret_index - 1);
  Reg_Property_Range (allocatable, ISA_REGISTER_CLASS_float64, 1,
	f64ret_index - 1);
  Reg_Property_Range (allocatable, ISA_REGISTER_CLASS_predicate, 1, 
	ISA_REGISTER_CLASS_Last_Reg(ISA_REGISTER_CLASS_predicate));

  Set_Reg_Name(ISA_REGISTER_CLASS_integer16, 0, "_");
  Set_Reg_Name(ISA_REGISTER_CLASS_integer, 0, "_");
  Set_Reg_Name(ISA_REGISTER_CLASS_integer64, 0, "_");
  Set_Reg_Name(ISA_REGISTER_CLASS_float, 0, "_");
  Set_Reg_Name(ISA_REGISTER_CLASS_float64, 0, "_");
  Reg_Property(unused, ISA_REGISTER_CLASS_integer, 0, -1);
  Reg_Property(unused, ISA_REGISTER_CLASS_integer16, 0, -1);
  Reg_Property(unused, ISA_REGISTER_CLASS_integer64, 0, -1);
  Reg_Property(unused, ISA_REGISTER_CLASS_float, 0, -1);
  Reg_Property(unused, ISA_REGISTER_CLASS_float64, 0, -1);

  ABI_Properties_End();
}
