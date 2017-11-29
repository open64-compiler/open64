/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

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


#define INCLUDING_IN_REGISTER // we modify register classes and we're
			      // really part of the register implementation

#include "defs.h"
#include "errors.h"
#include "tracing.h"
#include "mempool.h"
#include "config.h"
#include "glob.h"
#include "util.h"
#include "calls.h"
#include "data_layout.h"
#include "tn.h"
#include "targ_sim.h"
#include "op.h"
#include "cg_flags.h"
#include "ipfec_options.h"

#include "targ_isa_registers.h"
#include "targ_abi_properties.h"
#include "targ_isa_operands.h"

#include "register.h"

// REGISTER values are biased by REGISTER_MIN, so apply
// it to get REGISTER value given a machine reg number
#define FIRST_INPUT_REG (32+REGISTER_MIN)
#define FIRST_OUTPUT_REG (127+REGISTER_MIN)
#define LAST_STACKED_REG (127+REGISTER_MIN)
#define FIRST_ROTATING_INTEGER_REG (32+REGISTER_MIN)
#define FIRST_ROTATING_FLOAT_REG (32+REGISTER_MIN)
#define FIRST_ROTATING_PREDICATE_REG (16+REGISTER_MIN)
#define LAST_ROTATING_INTEGER_REG (127+REGISTER_MIN)
#define LAST_ROTATING_FLOAT_REG (127+REGISTER_MIN)
#define LAST_ROTATING_PREDICATE_REG (63+REGISTER_MIN)
#define MAX_NUM_OF_OUTPUT_REGISTERS 8


static INT stacked_callee_next;
static INT stacked_caller_next;
static INT num_output_parameters;
static INT num_caller;
static INT num_rotating;
static REGISTER_SET stacked_callee_used;
REGISTER_SET stacked_caller_used;
static INT stacked_minimum_output;

extern BOOL fat_self_recursive;
extern BOOL can_use_stacked_reg;

//Inserted by ORC
extern INT abi_property;
extern ISA_REGISTER_CLASS reg_class;
extern BOOL need_buffer;
extern INT32 current_lrange;
//End of Insertion


/////////////////////////////////////
void
REGISTER_Init_Stacked(ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  if (REGISTER_Has_Stacked_Registers(rclass)) {
    num_output_parameters = 0;
    num_caller = 0;
    num_rotating = 0;
    //
    // stacked_callee_next points at the last register allocated.
    // stacked_caller_next points at the next register to allocate.
    //
    stacked_callee_next = FIRST_INPUT_REG - 1;
    stacked_caller_next = FIRST_OUTPUT_REG;
    stacked_callee_used = REGISTER_SET_EMPTY_SET;
    stacked_caller_used = REGISTER_SET_EMPTY_SET;

    //
    // This should be reset by GRA to something reasonable.  There
    // shouldn't be much allocation out of the stacked registers
    // before GRA (there'd better not be!), but prevent all output
    // paramaters from being consumed just to be safe.
    //
    stacked_minimum_output = MAX_NUMBER_OF_REGISTER_PARAMETERS;
  }
}

/////////////////////////////////////
void REGISTER_Request_Stacked_Rotating_Register()
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  // Should choose a better balance between callee/caller saved registers!
  
  const INT max_callee = 96 - 8;  // 8 registers required by ABI as output registers?
  INT n_callee = MIN(max_callee, num_rotating);
  INT n_caller = MAX(0, num_rotating - max_callee);
  REGISTER r;

  stacked_callee_next = MAX(stacked_callee_next, (INT) (FIRST_INPUT_REG - 1 + n_callee));
  for (r = FIRST_ROTATING_INTEGER_REG; 
       r <= stacked_callee_next;
       r++) {
    stacked_callee_used = REGISTER_SET_Union1(stacked_callee_used, r);
  }

  stacked_caller_next = MIN(stacked_caller_next, (INT) (FIRST_OUTPUT_REG - n_caller));
  num_caller = FIRST_OUTPUT_REG - stacked_caller_next;
  for (r = FIRST_OUTPUT_REG;
       r > stacked_caller_next;
       r--) {
    stacked_caller_used = REGISTER_SET_Union1(stacked_caller_used, r);
  }

  ISA_REGISTER_CLASS i;
  FOR_ALL_ISA_REGISTER_CLASS(i) {
    if (REGISTER_Has_Rotating_Registers(i)) {  
      REGISTER first = REGISTER_First_Rotating_Registers(i);
      REGISTER last = (i != ISA_REGISTER_CLASS_integer) ? 
	REGISTER_Last_Rotating_Registers(i) : (first + num_rotating - 1);

      REGISTER_CLASS_rotating(i) = REGISTER_SET_Range(first, last);
    } 
  }
}

INT32 Get_Stacked_Callee_Used() {
    INT32 stacked_callee_used = stacked_callee_next - 32;
    return stacked_callee_used;
}
 
INT32 Get_Stacked_Caller_Used() {
    INT32 stacked_caller_used = 128 - stacked_caller_next;
    return stacked_caller_used;
}
 
INT32 Get_Stacked_Callee_Next() {
    return stacked_callee_next;
}
 
INT32 Get_Stacked_Caller_Next() {
    return stacked_caller_next;
}
 
/////////////////////////////////////
REGISTER REGISTER_Request_Stacked_Register(INT has_abi_property,
					   ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  need_buffer = FALSE;
  if (!REGISTER_Has_Stacked_Registers(rclass)) return REGISTER_UNDEFINED;

  if (stacked_callee_next >= stacked_caller_next) return REGISTER_UNDEFINED;
  INT32 cut_num = IPFEC_Stacked_Cut_Num;
  if (fat_self_recursive) {
    if (!can_use_stacked_reg) {
        abi_property = has_abi_property;
        reg_class    = rclass;
        need_buffer  = TRUE;
        return REGISTER_UNDEFINED;
    }
    if ((stacked_callee_next + cut_num) >= stacked_caller_next)
       return REGISTER_UNDEFINED;
  }
  // callee saved are allocated from the front, caller from the
  // rear.  The two pointers meet when the stack is used up.  The
  // caller saved (i.e. output) registers will be renamed at code
  // emission time.  If the "stacked" register class is requested,
  // this indicates that we should choose the best class for performance.
  // Right now, we're hard coding that to caller saved.
  REGISTER result_reg;

  if (has_abi_property == ABI_PROPERTY_callee ||
      has_abi_property == ABI_PROPERTY_frame_ptr) {
    result_reg = ++stacked_callee_next;
    if (has_abi_property != ABI_PROPERTY_frame_ptr) {
      stacked_callee_used = REGISTER_SET_Union1(stacked_callee_used, 
					        result_reg);
    }
  } else if ((has_abi_property == ABI_PROPERTY_caller ||
	     has_abi_property == ABI_PROPERTY_stacked) && num_caller < MAX_NUM_OF_OUTPUT_REGISTERS) {
    result_reg = stacked_caller_next--;
    stacked_caller_used =
      REGISTER_SET_Union1(stacked_caller_used, result_reg);
    num_caller++;
  }
  else
    return REGISTER_UNDEFINED;
  if (fat_self_recursive) { 
      DevWarn(" STACKED REGISTER %d ALLOCATED TO TN %d\n",result_reg,current_lrange);
  }
  return result_reg;
}

/////////////////////////////////////
REGISTER
REGISTER_Allocate_Stacked_Register(INT has_abi_property,
				   ISA_REGISTER_CLASS rclass,
				   REGISTER reg)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  if (!REGISTER_Has_Stacked_Registers(rclass)) return REGISTER_UNDEFINED;

  REGISTER result_reg;

  // These will typically be the input or output parameter registers.  We'll
  // allocate all of the previous registers if we see them out of sequence.
  //
  if (has_abi_property == ABI_PROPERTY_callee ||
      has_abi_property == ABI_PROPERTY_frame_ptr) {
    if (reg <= stacked_callee_next) {
      //
      // already taken care of
      //
      result_reg = reg;
    } else if (reg < stacked_caller_next) {
      //
      // allocate it and those smaller than it
      // don't re-allocate previously allocated,
      // in case they were subsequently marked as unallocatable.
      // (Note that we can have regs that are used as callee,
      // but not allocatable for future uses, so not everything
      // less than callee_next will be in used avail set).
      //
      result_reg = reg;
      for (INT i = stacked_callee_next+1; i <= reg; ++i) {
	stacked_callee_used =
	  REGISTER_SET_Union1(stacked_callee_used, i);
      }
      stacked_callee_next = reg;
    } else {
      // it's not available
      result_reg = REGISTER_UNDEFINED;
    }

    if (   has_abi_property == ABI_PROPERTY_frame_ptr
	&& result_reg != REGISTER_UNDEFINED)
    {
      stacked_callee_used = 
	      REGISTER_SET_Difference1(stacked_callee_used, result_reg);
    }
  } else {
    if (reg > stacked_caller_next) {
      //
      // already taken care of
      //
      result_reg = reg;
    } else if (reg > stacked_callee_next) {
      //
      // Allocate it and those smaller than it.  Note that this code
      // assumes that the supplied reg is always an output parameter.
      // While this is always true, it's not really great style.  Should
      // fix it.  T.L.
      //
      result_reg = reg;
      stacked_caller_next = reg-1;
      num_caller = FIRST_OUTPUT_REG - stacked_caller_next;
      num_output_parameters = FIRST_OUTPUT_REG - reg + 1;
      for (INT i = FIRST_OUTPUT_REG; i > stacked_caller_next; --i) {
	stacked_caller_used =
	  REGISTER_SET_Union1(stacked_caller_used, i);
      }
    } else {
      //
      // it's not available
      //
      result_reg = REGISTER_UNDEFINED;
    }
  }
  return result_reg;
}

/////////////////////////////////////
void
REGISTER_Unallocate_Stacked_Register (
   ISA_REGISTER_CLASS rclass, REGISTER reg)
{
  if ( ! REGISTER_Has_Stacked_Registers(rclass))
	return;
  stacked_callee_used = REGISTER_SET_Difference1(stacked_callee_used, reg);
  stacked_caller_used = REGISTER_SET_Difference1(stacked_caller_used, reg);
}

/////////////////////////////////////
BOOL
REGISTER_Is_Allocatable_Stacked_Register (
   ISA_REGISTER_CLASS rclass, REGISTER reg)
{
  if ( ! REGISTER_Has_Stacked_Registers(rclass)) {
	return FALSE;
  }
  else if (reg <= stacked_callee_next) {
	return REGISTER_SET_MemberP(stacked_callee_used, reg);
  }
  else if (reg >= stacked_caller_next) {
	return REGISTER_SET_MemberP(stacked_caller_used, reg);
  }
  else {
	return FALSE;
  }
}

/////////////////////////////////////
REGISTER_SET
REGISTER_Get_Stacked_Avail_Set(INT has_abi_property, ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  if (REGISTER_Has_Stacked_Registers(rclass)) {
    if (has_abi_property == ABI_PROPERTY_callee) {
      REGISTER_SET avail = stacked_callee_used;
      return avail;
    } else if (has_abi_property == ABI_PROPERTY_caller) {
      return stacked_caller_used;
    } else if (has_abi_property == ABI_PROPERTY_stacked) {
      if (CG_localize_tns && LOCALIZE_using_stacked_regs) {
      	// localize uses callee-save for global tns 
      	// which are not really avail now;
      	// so only return caller-save regs
      	return stacked_caller_used;
      }
      else {
      	REGISTER_SET avail = stacked_callee_used;
      	return REGISTER_SET_Union(avail, stacked_caller_used);
      }
    } else {
      FmtAssert(FALSE, ("REGISTER: Unknown abi property for stacked register"));
    }
  }
  return REGISTER_SET_EMPTY_SET;
}
    
/////////////////////////////////////
BOOL
REGISTER_Is_Stacked_Output(ISA_REGISTER_CLASS rclass, REGISTER reg)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  if ( ! REGISTER_Has_Stacked_Registers(rclass)) {
	return FALSE;
  }
  if (reg > stacked_caller_next) {
    return TRUE;
  }
  return FALSE;
}

    
/////////////////////////////////////
BOOL
REGISTER_Is_Stacked_Local(ISA_REGISTER_CLASS rclass, REGISTER reg)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  if ( ! REGISTER_Has_Stacked_Registers(rclass)) {
	return FALSE;
  }
  if (reg <= stacked_caller_next && reg >= FIRST_INPUT_REG) {
    return TRUE;
  }
  return FALSE;
}

    
/////////////////////////////////////
BOOL
REGISTER_Is_Stacked(ISA_REGISTER_CLASS rclass, REGISTER reg)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  if ( ! REGISTER_Has_Stacked_Registers(rclass)) {
	return FALSE;
  }
  if (reg >= FIRST_INPUT_REG) return TRUE;
  return FALSE;
}

/////////////////////////////////////
BOOL
REGISTER_Is_Rotating(ISA_REGISTER_CLASS rclass, REGISTER reg)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{

  switch(rclass) {
  case ISA_REGISTER_CLASS_integer:
    if (reg >= FIRST_ROTATING_INTEGER_REG && 
	reg <= LAST_ROTATING_INTEGER_REG) return TRUE;
    break;
  case ISA_REGISTER_CLASS_float:
    if (reg >= FIRST_ROTATING_FLOAT_REG &&
	reg <= LAST_ROTATING_FLOAT_REG) return TRUE;
    break;
  case ISA_REGISTER_CLASS_predicate:
    if (reg >= FIRST_ROTATING_PREDICATE_REG &&
	reg <= LAST_ROTATING_PREDICATE_REG) return TRUE;
  }

  return FALSE;
}

/////////////////////////////////////
REGISTER
REGISTER_Translate_Stacked_Output(REGISTER reg)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  REGISTER result;
  if (reg > FIRST_OUTPUT_REG - num_output_parameters) {
    //
    // Output parameters always at end, in reverse order.  
    // Move them to the front, in reverse order.
    //
    result = stacked_callee_next + (FIRST_OUTPUT_REG - reg) + 1;
  }
  else {
    //
    // all others go after 8 argument registers
    //
    INT first_reg = stacked_callee_next + num_output_parameters + 1;
    INT first_caller = stacked_caller_next + 1;
    result = reg - (first_caller - first_reg);
  }
  if (Get_Trace ( TP_EMIT, 16 ))
	fprintf(TFile, "translate stacked reg %d to %d\n", reg, result);
  return result;
}

static char outregname[6] = "out0";

/////////////////////////////////////
char *
REGISTER_Stacked_Output_Name (REGISTER reg)
/////////////////////////////////////
{
	// what if not one of the 8 out regs?
	INT num = FIRST_OUTPUT_REG - reg;
	outregname[3] = '0'+num;
	outregname[4] = '\0';
  	return outregname;
}


/////////////////////////////////////
INT
REGISTER_Number_Stacked_Local (ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  if (rclass != ISA_REGISTER_CLASS_integer) return 0;
  return stacked_callee_next - FIRST_INPUT_REG + 1;
}


/////////////////////////////////////
INT
REGISTER_Number_Stacked_Output (ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  //
  // since the output parameters will start at 120, we cannot just
  // do some math with stacked_caller_next to figure out the number.
  // otherwise, we'll get a minimum of 8 if we have any output paramets
  // no matter how many of them there are.
  // 
  if (rclass != ISA_REGISTER_CLASS_integer) return 0;
  return num_caller;
}


/////////////////////////////////////
INT
REGISTER_Number_Stacked_Rotating (ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  if (rclass != ISA_REGISTER_CLASS_integer) return 0;
  return num_rotating;
}

INT REGISTER_Number_Stacked_Registers_Available (ISA_REGISTER_CLASS rclass)
{
  if (rclass != ISA_REGISTER_CLASS_integer) return 0;
  return stacked_caller_next - stacked_callee_next;
}


/////////////////////////////////////
void REGISTER_Reserve_Rotating_Registers(ISA_REGISTER_CLASS rclass, INT n)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  num_rotating = MAX(num_rotating, n);
  // roundup to next multiple of 8
  if ((num_rotating & 7) != 0) {
    num_rotating &= ~7;
    num_rotating += 8;
  }
}

/////////////////////////////////////
void
REGISTER_Set_Stacked_Output_Minimum(ISA_REGISTER_CLASS rclass, INT num)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  if (REGISTER_Has_Stacked_Registers(rclass)) {
    if (num_output_parameters > num) {
      stacked_minimum_output = num_output_parameters;
      DevWarn("Attempted to set minimum stacked output to %d when %d output parameters already allocated.\n", num, num_output_parameters);
    } else {
      stacked_minimum_output = num;
    }
  }
}


/////////////////////////////////////
REGISTER REGISTER_First_Rotating_Registers(ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  switch (rclass) {
  case ISA_REGISTER_CLASS_integer:
    return FIRST_ROTATING_INTEGER_REG;
  case ISA_REGISTER_CLASS_float:
    return FIRST_ROTATING_FLOAT_REG;
  case ISA_REGISTER_CLASS_predicate:
    return FIRST_ROTATING_PREDICATE_REG;
  }
  return REGISTER_UNDEFINED;
}

/////////////////////////////////////
REGISTER REGISTER_Last_Rotating_Registers(ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  switch (rclass) {
  case ISA_REGISTER_CLASS_integer:
    return LAST_ROTATING_INTEGER_REG;
  case ISA_REGISTER_CLASS_float:
    return LAST_ROTATING_FLOAT_REG;
  case ISA_REGISTER_CLASS_predicate:
    return LAST_ROTATING_PREDICATE_REG;
  }
  return REGISTER_UNDEFINED; 
}


/////////////////////////////////////
REGISTER_SET REGISTER_Get_Requested_Rotating_Registers(ISA_REGISTER_CLASS rclass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  switch (rclass) {
  case ISA_REGISTER_CLASS_integer:
    return REGISTER_SET_Range(FIRST_ROTATING_INTEGER_REG, 
			      FIRST_ROTATING_INTEGER_REG + num_rotating);
  case ISA_REGISTER_CLASS_float:
    return REGISTER_SET_Range(FIRST_ROTATING_FLOAT_REG,
			      LAST_ROTATING_FLOAT_REG);
  case ISA_REGISTER_CLASS_predicate:
    return REGISTER_SET_Range(FIRST_ROTATING_PREDICATE_REG,
			      LAST_ROTATING_PREDICATE_REG);
  }
  return REGISTER_SET_EMPTY_SET;
}


mISA_REGISTER_CLASS Mtype_RegClass_Map[MTYPE_LAST+1];

void Init_Mtype_RegClass_Map(void)
{
  INT i;
  mISA_REGISTER_CLASS * const map = Mtype_RegClass_Map;

  for (i = 0; i <= MTYPE_LAST; ++i) map[i] = ISA_REGISTER_CLASS_UNDEFINED;

  map[MTYPE_B] = ISA_REGISTER_CLASS_predicate;
  map[MTYPE_I1] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_I2] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_I4] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_I8] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_U1] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_U2] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_U4] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_U8] = ISA_REGISTER_CLASS_integer;
  map[MTYPE_F4] = ISA_REGISTER_CLASS_float;
  map[MTYPE_F8] = ISA_REGISTER_CLASS_float;
  map[MTYPE_F10] = ISA_REGISTER_CLASS_float;
  map[MTYPE_F16] = ISA_REGISTER_CLASS_UNDEFINED;
  // bug fix for OSP_87
  map[MTYPE_A8] = ISA_REGISTER_CLASS_branch;
}
