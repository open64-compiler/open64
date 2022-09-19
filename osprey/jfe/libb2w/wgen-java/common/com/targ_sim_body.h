/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
// This file should only be included by <targ>/targ_sim.cxx
// It is for target-independent things that are shared by the targ_sim code.
//

/*
 * This defines the ABI subprogram interface,
 * and is used to determine how parameters and results are passed.
 * We have an array of tables, where each table describes the info
 * for one abi.  The array is indexed by the TARGET_ABI enumeration.
 * The register values are the PREG offsets, so these values can be
 * used in WHIRL.
 */

typedef mUINT8 mDED_PREG_NUM;	/* physical pregs will be in 0..127 range */

/* Preg_Range defines a sequence of preg values */
typedef struct {
	mDED_PREG_NUM first_reg;
	mDED_PREG_NUM last_reg;
	mINT16 skip_value;	/* whether to increment by 1 or 2 */
	/* only need 2 bits, but use 16 so no padding & no purify complaint */
} Preg_Range;

#define PR_first_reg(x)		x.first_reg
#define PR_last_reg(x)		x.last_reg
#define PR_skip_value(x)	x.skip_value

/* Define an interface descriptor: */
typedef struct subprogram_interface {

  mUINT32 flags;			/* Flags -- see masks below */

  /* Registers used for parameters and results */
  Preg_Range int_args;
  Preg_Range flt_args;
  Preg_Range dbl_args;
  Preg_Range int_results;
  Preg_Range flt_results;
  Preg_Range dbl_results;

  /* Argument conversion: */
  mTYPE_ID int_type;	/* Convert to at least this type */
  mTYPE_ID flt_type;	/* Convert to at least this type */
  mTYPE_ID dbl_type;	/* Convert to at least this type */

  /* Argument save area definition: */
  mINT16 min_save_area_bytes;	/* Minimum size to be reserved */
  mINT16 formal_save_area_bytes;	/* save area for formals */
  mINT16 varargs_save_offset;	/* First argument offset (varargs) */

  /* Miscellaneous: */
  mINT16 max_struct_size;	/* Max size of structs in regs */
	/* -1 for no maximum; see IFACE_REG_STRUCTS flag */
  mINT16 max_struct_result;	/* Max size of struct result (in bits) */

  mDED_PREG_NUM slink_reg;	/* Static link register */
  mDED_PREG_NUM pic_call_reg;	/* PIC call address register */

} SIM;

/* Flag masks */
#define SIM_EXTRA_FLT_IN_INT	0x0001	/* Use int regs for extra flts? */
	/* After all available floating point argument registers have
	 * been used, should we use integer registers?
	 */
#define SIM_FLT_AFTER_INT	0x0002	/* Use flt regs after int args? */
	/* Can we use floating point argument registers for floating
	 * point arguments which come after (right of) integer args?
	 */
#define SIM_FIXED_REG_BYTES	0x0008	/* Max bytes in registers? */
	/* The MIPS and SPARC ABIs both reserve enough save area space
	 * for all of the integer argument register contents, but
	 * considering floating point arguments as well actually could
	 * allow more register arguments than will fit in the minimum
	 * space.  Can we actually use more registers than the minimum?
	 */
#define SIM_COORD_INT_FLT	0x0010	/* Coordinate int/flt regs? */
	/* Does the next available integer register get incremented
	 * when we assign a float argument, and vice versa?  (The MIPS
	 * conventions say YES.)
	 */
#define SIM_COORD_MEM_REG	0x0020	/* Coordinate mem and regs? */
	/* Are the registers a map of memory, i.e. does alignment in
	 * memory force equivalent alignment in registers?
	 */
#define SIM_REG_STRUCTS		0x0040	/* Pass structs in regs? */
#define SIM_FLT_RTN_COMPLEX	0x0080	/* Return complex in flt regs? */
#define SIM_CALLER_SAVE_GP	0x0100	/* GP is caller-save? */
#define SIM_FLT_REG_FIELDS	0x0200	/* Float fields in float regs? */
#define SIM_DBL_REG_FIELDS	0x0400	/* Double fields in float regs? */
#define SIM_VARARGS_FLOATS	0x0800	/* Variable float args in fregs? */
#define SIM_RET_ADDR_VIA_ARGS	0x1000	/* if return large structs via addr
					 * in implicit arg, do we use normal 
					 * parameter passing conventions for 
					 * that implicit arg. */
#define SIM_RET_ADDR_VIA_INT_RET 0x2000	/* if return large structs via addr
					 * in implicit arg, do we use integer 
					 * return register rather than normal
					 * parameter passing for that 
					 * implicit arg. */


// gcc and edg have different restrictions on how to do a forward decl
// of an initialized object.
#if (__GNUC__ == 2)
static SIM SIM_Info[];
#else
extern SIM SIM_Info[];
#endif
#define SIM_INFO	SIM_Info[Target_ABI]

#define SIM_varargs_floats	((SIM_INFO.flags & SIM_VARARGS_FLOATS) != 0)
#define SIM_flt_after_int	((SIM_INFO.flags & SIM_FLT_AFTER_INT) != 0)
#define SIM_caller_save_gp	((SIM_INFO.flags & SIM_CALLER_SAVE_GP) != 0)
#define SIM_dbl_fields_in_reg	((SIM_INFO.flags & SIM_DBL_REG_FIELDS) != 0)
#define SIM_return_addr_via_int_return_reg	((SIM_INFO.flags & SIM_RET_ADDR_VIA_INT_RET) != 0)

/*
 *  check for array case where fe doesn't fill in right btype
 */
TYPE_ID
Fix_TY_mtype (TY_IDX ty)
{
    TYPE_ID type = TY_mtype (ty);
    if (type == MTYPE_UNKNOWN && TY_kind (ty) == KIND_ARRAY)
	type = Pointer_Mtype;
    else if (MTYPE_is_complex(type) && TY_kind (ty) == KIND_STRUCT)
	type = MTYPE_M;

    return type;
}

static INT Current_Param_Num = -1;
        /* number of current logical parameter register, start at 0 */
static INT Last_Param_Offset = 0;       /* stack offset */
static INT Last_Fixed_Param = INT_MAX;  /* # of last fixed param (varargs) */


static inline PREG_NUM
Get_Current_Preg_Num (Preg_Range pr)
{
	PREG_NUM i;
	i = PR_first_reg(pr) + (Current_Param_Num * PR_skip_value(pr));
	if (i > PR_last_reg(pr))
		return 0;
	else
		return i;
}

static PLOC Setup_Parameter_Locations (TY_IDX pu_type);
static PLOC Get_Parameter_Location (TY_IDX ty, BOOL is_output);

extern PLOC
Setup_Input_Parameter_Locations (TY_IDX pu_type)
{
    return Setup_Parameter_Locations (pu_type);
}

extern PLOC
Setup_Output_Parameter_Locations (TY_IDX pu_type)
{
    return Setup_Parameter_Locations (pu_type);
}

extern PLOC
Get_Input_Parameter_Location (TY_IDX ty)
{
  return Get_Parameter_Location (ty, FALSE);
}

extern PLOC
Get_Output_Parameter_Location (TY_IDX ty)
{
  return Get_Parameter_Location (ty, TRUE);
}

static PLOC Get_Vararg_Parameter_Location (PLOC prev);

extern PLOC
Get_Vararg_Input_Parameter_Location (PLOC prev)
{
  return Get_Vararg_Parameter_Location (prev);
}

extern PLOC
Get_Vararg_Output_Parameter_Location (PLOC prev)
{
  return Get_Vararg_Parameter_Location (prev);
}

#define IS_INT_PREG(p) \
	((PR_first_reg(SIM_INFO.int_args) <= p) \
	&& (p <= PR_last_reg(SIM_INFO.int_args)))

#define IS_FLT_PREG(p) \
	((PR_first_reg(SIM_INFO.flt_args) <= p) \
	&& (p <= PR_last_reg(SIM_INFO.flt_args)))

/* return position (0'th parameter...8'th parameter) of preg */
#define GET_PREG_POSITION(p,range) \
	((p - PR_first_reg(range)) / PR_skip_value(range))


extern INT32
Get_Preg_Size (PREG_NUM p)
{
	if (IS_FLT_PREG(p))
		return MTYPE_RegisterSize(SIM_INFO.flt_type);
	else
		return MTYPE_RegisterSize(SIM_INFO.int_type);
}

static void Setup_Struct_Parameter_Locations (TY_IDX struct_ty);
static PLOC Get_Struct_Parameter_Location (PLOC prev);

extern void
Setup_Struct_Input_Parameter_Locations (TY_IDX struct_ty)
{
    Setup_Struct_Parameter_Locations (struct_ty);
}

extern void
Setup_Struct_Output_Parameter_Locations (TY_IDX struct_ty)
{
    Setup_Struct_Parameter_Locations (struct_ty);
}

extern PLOC
Get_Struct_Input_Parameter_Location (PLOC prev)
{
  return Get_Struct_Parameter_Location (prev);
}

extern PLOC
Get_Struct_Output_Parameter_Location (PLOC prev)
{
  return Get_Struct_Parameter_Location (prev);
}


static TYPE_ID ploc_parm_mtype;
static INT32 ploc_last_offset;


static PLOC
First_PLOC_Reg (PLOC ploc, TY_IDX parm_ty)
{
	ploc_parm_mtype = Fix_TY_mtype (parm_ty);	/* Target type */
	PLOC first = ploc;
	ploc_last_offset = PLOC_total_size(ploc);
	switch (ploc_parm_mtype) {
	case MTYPE_F10:
		/*
		 * When FP types wider than a register are passed in integer
		 * registers or onto the stack, we need multiple of such
		 * registers or register slots. Treat them as compound types.
		 */
		if (!PLOC_on_stack(ploc) && IS_FLT_PREG(PLOC_reg(ploc)))
			break;
		/*FALLTHROUGH*/
	case MTYPE_M:
		Setup_Struct_Parameter_Locations (parm_ty);
		first = Get_Struct_Parameter_Location (ploc);
		break;
	case MTYPE_C4:
		PLOC_size(first) = MTYPE_RegisterSize(MTYPE_F4);
		break;
	case MTYPE_C8:
	case MTYPE_CQ:
	case MTYPE_FQ:
		PLOC_size(first) = MTYPE_RegisterSize(MTYPE_F8);
		break;
#ifdef TARG_IA64
	case MTYPE_C10:
		PLOC_size(first) = MTYPE_RegisterSize(MTYPE_F10);
		break;
#endif
	}
	return first;
}

extern PLOC
First_Input_PLOC_Reg (PLOC ploc, TY_IDX parm_ty)
{
    return First_PLOC_Reg (ploc, parm_ty);
}

extern PLOC
First_Output_PLOC_Reg (PLOC ploc, TY_IDX parm_ty)
{
    return First_PLOC_Reg (ploc, parm_ty);
}

static PLOC
Next_PLOC_Reg (PLOC prev)
{
	PLOC next = prev;
	switch (ploc_parm_mtype) {
	case MTYPE_M:
		next = Get_Struct_Parameter_Location (prev);
		break;
	case MTYPE_C4:
	case MTYPE_C8:
	case MTYPE_CQ:
	case MTYPE_FQ:
		if (ploc_parm_mtype == MTYPE_C4)
			PLOC_offset(next) += MTYPE_RegisterSize(MTYPE_F4);
		else
			PLOC_offset(next) += MTYPE_RegisterSize(MTYPE_F8);
		if (PLOC_offset(next) == ploc_last_offset) {
			// end reached
			PLOC_size(next) = 0;
		}
	    	PLOC_reg(next) += PR_skip_value(SIM_INFO.flt_args);
		if (PLOC_reg(next) > PR_last_reg(SIM_INFO.flt_args)) {
		    PLOC_reg(next) = 0;
		}
		break;
#ifdef TARG_IA64
	case MTYPE_C10:
		PLOC_offset(next) += MTYPE_RegisterSize(MTYPE_F10);
		if (PLOC_offset(next) == ploc_last_offset)
			PLOC_size(next) = 0;
		PLOC_reg(next) += PR_skip_value(SIM_INFO.flt_args);
		if (PLOC_reg(next) > PR_last_reg(SIM_INFO.flt_args))
			PLOC_reg(next) = 0;
		break;
#endif
	case MTYPE_F10:
		/*
                 * When FP types wider than a register are passed in integer
		 * registers, we need multiple of such registers. Treat them
		 * as compound types.
		 */
                if (PLOC_on_stack(prev) || !IS_FLT_PREG(PLOC_reg(prev))) {
			next = Get_Struct_Parameter_Location (prev);
			break;
		}
		/*FALLTHROUGH*/
	default:
		PLOC_offset(next) = ploc_last_offset;
		PLOC_size(next) = 0; 
	}
	return next;
}

extern PLOC
Next_Input_PLOC_Reg (PLOC prev)
{
	return Next_PLOC_Reg (prev);
}

extern PLOC
Next_Output_PLOC_Reg (PLOC prev)
{
	return Next_PLOC_Reg (prev);
}
