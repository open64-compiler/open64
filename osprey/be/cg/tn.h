/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


//-*-c++-*-
/* ====================================================================
 * ====================================================================
 *
 * Module: tn.h
 * $Revision: 1.21 $
 * $Date: 05/12/05 08:59:09-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.tn.h $
 *
 * Description:
 *
 * Define the TN (temporary name) data structure and relevant utility
 * operations.
 *
 * Exported types:
 *
 *   TN
 *
 *     The Temporary Name (TN) structure describes the operands and result
 *     of OPs. A TN can be used to describe either a register or a constant.
 *     The TN_flags(tn) field is applicable to all TNs and is used to determine
 *     the correct variant of the TN. The TN_size(tn) field is also applicable
 *     to all TNs and gives the size of the operand/result in bytes.
 *     All the other fields are applicable in specific contexts as described 
 *     below.
 *
 *     Register TNs:
 *
 *       The flag TN_is_register(tn) indicates if a TN is a register TN.
 *
 *       The following fields are valid only for register TNs:
 *
 *	   TN_number(tn) 
 *	     numeric id associated with a register TN.
 *
 *	   TN_register_class(tn)
 *	     The TN_register_class(tn) indicates the register class of 
 * 	     the register TN.
 *	
 *	   TN_register(tn)
 *           The TN_register(tn) indicates the register assigned to the
 *	     register TN.
 *
 *	   TN_register_and_class(tn)
 *	     TN_register_class and TN_register combined into a single
 *	     scalar type for efficiency in comparing registers and classes.
 *
 *	   TN_save_reg(tn)
 *	     If the tn is a save-tn, this field gives the register 
 * 	     that the save-tn is saving/restoring. For all other tns,
 *	     this is set to REGISTER_UNDEFINED.
 *
 *	   TN_spill(tn)
 *	     These fields point to spill information for register TNs.
 *
 *	   TN_is_rematerializable(tn)
 *	     If this attribute is true, the <tn> contains a constant
 *	     value. It can be rematerialized using the TN_home field.
 *
 *	   TN_home(tn)
 *	     This field is valid only if TN_is_rematerializable is true.
 *	     It points to the WHIRL node to use in rematerializing the
 *	     constant value of the <tn>.
 *
 *	   TN_is_dedicated(tn)
 *	     This macro indicates that the register TN corresponds to a 
 *           physical register.
 *
 *	   TN_is_float(tn)
 *	     TN is for a floating point register.
 *
 *	   TN_is_fpu_int(tn)
 *	     TN is for an integer value in an FP register.
 *
 *	   TN_is_fcc_register(tn)
 *	     TN is an fcc (FP condition code) register.
 *
 *	   TN_is_save_reg(tn)
 *	     TN is a save-tn for a callee-saved register.
 *
 *
 *    Constant TNs:
 *
 *      The flag TN_is_constant(tn) indicates if a TN is a constant TN. A 
 *      constant TN can be one of the following sub-types:
 * 
 *      1. Integer constant:
 *	  TN_has_value(tn) indicates if a constant TN is an integer
 *	  constant. The value of the constant is given by TN_value(tn).
 *
 *      2. Symbol TN:
 *	  TN_is_symbol(tn) indicates if a constant TN representing a 
 *	  "symbol+offset" expression. The symbol is given by TN_var(tn)
 *	  and the offset by TN_offset(tn). The TN_relocs(tn) field
 *	  indicates additional relocations/operations to be applied 
 *	  to the "symbol+offset".
 *
 *      3. Label TN:
 *	  TN_is_label(tn) indicates if a constant TN represents a label.
 *	  The label is indicated by TN_label(tn). The TN_offset(tn) is
 *	  used to indicate a byte offset from the label.
 *
 *      4. Enum TN:
 *	  TN_is_enum(tn) indicates if a constant TN represents an enum.
 *	  The enum value is indicated by TN_enum(tn).
 *	   
 * Utility functions:
 *
 *   void TN_Allocate_Register( TN *tn, REGISTER reg )
 *	Register TNs may be allocated to a register. This function
 *	sets <tn>'s _register to be <reg>.  This is the act of register
 *	assignment.
 *
 *   BOOL TN_Is_Allocatable( TN *tn )
 *	TRUE for just those TNs that can be assigned a register.  This
 *	excludes all kind of things that must NOT be register allocated,
 *	such as constants, zeros, dedicated TNs. 
 *
 *   OP *TN_Reaching_Value_At_Op(TN *tn, OP *op, DEF_KIND *kind, 
 *				 BOOL reaching_def)
 *      The routine can be used to find (1) the reaching definition of
 *      operand <tn> of <op>, or (2) the following use of the result <tn>
 *	of <op>. The knob is controlled by the flag <reaching_def>.
 *      If <reaching_def> is TRUE, find the reaching definition of the <tn>, 
 *	if <reaching_def> is FALSE, find the reaching use of the <tn>. 
 *	If none is found, return NULL. <kind> determines the definition kind 
 *	property returned by the function, i.e if the value definition 
 *      of <tn> is VAL_KNOWN, VAL_UNKNOWN, VAL_COND_DEF, VAL_COND_USE, .. etc.
 *
 *	Note that not all reaching definitions are found, dominator
 *	information is necessary to handle more cases and it's not
 *	proven to be worth it.
 *
 *   BOOL TN_Value_At_Op( TN *tn, OP *use_op, INT64 *val )
 *	If it can be determined that <tn> has a known integer value, as 
 *	referenced by <use_op>, return the value via the  out parameter <val>. 
 *	<use_op> may be NULL, in which case if <tn> is not constant,
 *	no attempt is made to find a reaching definition. The return 
 *	value indicates if we were able to determine the value.
 *
 *   TN *Build_Dedicated_TN( REGISTER_CLASS rclass, REGISTER reg, INT size )
 *	Create a dedicated TN for register 'reg' in class 'rclass'.
 *	NOTE: Currently this returns the same TN when called multiple
 *	times with the same 'rclass', 'reg' pair. This will eventually
 *	change to build a new TN on each call.
 *	If the 'size' param is 0, then use default dedicated tn for regclass.
 *	Otherwise, for float rclass create separate TN for each float size.
 *	The 'size' is specified in bytes.
 *
 *   void Init_Dedicated_TNs( void )
 *	This routine should be called once per compilation before we
 *	process any PUs. It initializes the dedicated TNs.
 *	NOTE: This will eventually go away, see Build_Dedicated_TN.
 *
 *   BOOL TN_is_dedicated_class_and_reg( TN *tn, UINT16 class_n_reg )
 *	Returns a boolean to indicate if <tn> is a dedicated TN
 *	for the specified class and register.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef tn_INCLUDED
#define tn_INCLUDED

/* Define the type before any includes so they can use it. */
typedef struct tn TN;

#include "tn_list.h"
#include "register.h"
#include "symtab.h"
#include "targ_isa_enums.h"
#include "op.h"
class WN;


/* Define the TN number type: */
typedef INT32 TN_NUM;	/* Individual objects */
typedef mINT32 mTN_NUM;	/* Table components */
#define TN_NUM_MAX	INT32_MAX


/* Define the TN structure: (NOTE: keep the size <= 16 bytes) */

struct tn {
  /* offset 0 */
  union {
    INT64	value;		/* TN_has_value: Integer constant */
    INT64	offset;		/* Offset from symbol (constant) */
    struct {
      mTN_NUM	number;		/* The ID of the register TN */
      CLASS_REG_PAIR save_creg;	/* if save_tn, the corresponding save_reg */
      CLASS_REG_PAIR class_reg; /* Dedicated/allocated register ID
				   and register class (see register.h) */
    } reg_tn;
  } u1;
  /* offset 8 */
  mUINT32	flags;		/* Attribute flags */
#ifdef TARG_NVISA
  /* nvisa doesn't use relocs, needs space info, so overlap space and relocs */
  union {
    mUINT8	relocs;		/* Relocation flags (for symbol TNs) */
    mUINT8	space;		/* memory space associated with tn */
  } urs;
#else
  mUINT8	relocs;		/* Relocation flags (for symbol TNs) */
#endif
  mUINT8	size;		/* Size of the TN in bytes (must be <= 16) */
  /* offset 12 */
  union {
    LABEL_IDX	label;		/* Label constant */
    ISA_ENUM_CLASS_VALUE ecv;	/* Enum constant */
    ST		*var;		/* Symbolic constant (user variable) */
    union {			/* Spill location */
      ST	*spill;		/* ...for register TN */
      WN        *home;		/* Whirl home if rematerializable */
    } u3;
  } u2;
#ifdef TARG_NVISA
  mBOOL from_shared_load;
#endif
};


/* Define the TN_flags masks: */
#define TN_CONSTANT	0x0001	/* Constant value, numeric or label */
#define TN_HAS_VALUE	0x0002	/* Constant numeric value */
#define TN_LABEL	0x0004	/* Constant label value */
#define TN_TAG		0x0008	/* Constant tag value */
#define TN_SYMBOL	0x0010	/* References symbol table element */
#define TN_FLOAT	0x0020	/* => tn for a Floating-Point value */
#define TN_DEDICATED	0x0040	/* Dedicated, register if reg!=NULL */
#define TN_FPU_INT	0x0080	/* Int value in FPU (also TN_FLOAT) */
#define TN_GLOBAL_REG	0x0100	/* TN is a GTN (global register) */
#define TN_IF_CONV_COND 0x0200  /* TN is an if conversion conditinal */
#define TN_REMATERIALIZABLE 0x0400  /* TN is rematerializable from whirl */
#define TN_GRA_HOMEABLE     0x0800  /* TN can be homed by gra */
#define TN_ENUM		    0x1000  /* Constant enum value */
#define TN_GRA_CANNOT_SPLIT 0x2000  /* its live range not to be split by GRA */
#ifdef TARG_X8664
#define TN_PREALLOCATED	    0x4000  /* TN is pre-allocated in LRA */
#define TN_THREAD_SEG_PTR   0x8000  /* TN is pointer to thread-local storage */
#define TN_NO_RENAME        0x10000 /* TN does not get auto renamed */
#endif

#ifdef TARG_IA64
#define TN_TAKE_NAT         0X4000  /* TN (gr,fpr) may take NaT bit */
#endif

#ifdef TARG_NVISA
#define TN_BOOLEAN	0x4000	/* TN is boolean value */
/* overlap ONE_DEF with unused GRA_CANNOT_SPLIT */
#define TN_ONE_DEF	0x2000	/* TN has single definition */
#define TN_MEMORY_SPACE	0x8000	/* TN has memory space */

/* flag memory space associated with tn */
#define TN_GLOBAL_SPACE 0x1 
#define TN_SHARED_SPACE 0x2
#define TN_CONST_SPACE 0x4
#define TN_LOCAL_SPACE 0x8
#define TN_PARAM_SPACE 0x10
#define TN_TEXTURE_SPACE 0x20
#endif

#if defined(TARG_SL)
#define TN_V1BUF_ADDR       0x4000  /* the tn is v1buf address in iload/istore*/
#endif

/* Define the TN_relocs values */

/* the following represent various relocations on symbolic values.  
 * The values are represented by a symbol table element.
 */

typedef enum {
  TN_RELOC_NONE	   = 0x00,
  TN_RELOC_GPREL16   = 0x01,	/* gp-relative reference to symbol */
  TN_RELOC_LOW16	   = 0x02,	/* Bits 0..15 of symbolic val */
  TN_RELOC_HIGH16	   = 0x03,	/* Bits 16..31 of symbol */
  TN_RELOC_HIGHER	   = 0x04,	/* Bits 32..47 of symbol */
  TN_RELOC_HIGHEST   = 0x05,	/* Bits 48..63 of symbol */
  TN_RELOC_GOT_DISP  = 0x06,
  TN_RELOC_GOT_PAGE  = 0x07,
  TN_RELOC_GOT_OFST  = 0x08,
  TN_RELOC_CALL16    = 0x09,
  TN_RELOC_GOT_HI16  = 0x0a,
  TN_RELOC_GOT_LO16  = 0x0b,
  TN_RELOC_CALL_HI16 = 0x0c,
  TN_RELOC_CALL_LO16 = 0x0d,
  TN_RELOC_NEG	     = 0x0e,
  TN_RELOC_GPSUB     = 0x0f,	/* gp - sym (pic2 prolog) */
  TN_RELOC_LO_GPSUB  = 0x10,
  TN_RELOC_HI_GPSUB  = 0x11,
  TN_RELOC_GPIDENT   = 0x12,	/* gp value (pic1 prolog) */
  TN_RELOC_LO_GPIDENT= 0x13,
  TN_RELOC_HI_GPIDENT= 0x14,
  TN_RELOC_IA_IMM14	= 0x20,      /* IA symbols starts at 0x20 */
  TN_RELOC_IA_IMM22	= 0x21,
  TN_RELOC_IA_PCREL	= 0x22,
  TN_RELOC_IA_GPREL22	= 0x23,
  TN_RELOC_IA_LTOFF22	= 0x24,
  TN_RELOC_IA_LTOFF_FPTR= 0x25,
#ifdef TARG_IA64
  TN_RELOC_IA_LTOFF22X	= 0x26,      /*  ltoffx */
                                     /* IA-32 relocations start at 0x40 */
  TN_RELOC_IA32_ALL   = 0x40,        /* All 32 bits of a symbol value. */

  TN_RELOC_IA_TPREL14   = 0x91,      /* For TLS both dynamic and static */
  TN_RELOC_IA_TPREL22,
  TN_RELOC_IA_TPREL64I,
  TN_RELOC_IA_TPREL64MSB,
  TN_RELOC_IA_TPREL64LSB,
  TN_RELOC_IA_LTOFF_TPREL22,
  TN_RELOC_IA_DTPMOD64MSB,
  TN_RELOC_IA_DTPMOD64LSB,
  TN_RELOC_IA_LTOFF_DTPMOD22,
  TN_RELOC_IA_DTPREL14,
  TN_RELOC_IA_DTPREL22,
  TN_RELOC_IA_DTPREL64I,
  TN_RELOC_IA_DTPREL32MSB,
  TN_RELOC_IA_DTPREL32LSB,
  TN_RELOC_IA_DTPREL64MSB,
  TN_RELOC_IA_DTPREL64LSB,
  TN_RELOC_IA_LTOFF_DTPREL22,

#endif
#ifdef TARG_X8664
  TN_RELOC_X8664_PC32 = 0x30,   /* X86-64 symbols start at 0x30 */
  TN_RELOC_X8664_32   = 0x31,   /* X86-64 symbols start at 0x30 */
  TN_RELOC_X8664_64   = 0x32,   
  TN_RELOC_X8664_GOTPCREL   = 0x33,   
  TN_RELOC_X8664_TPOFF32    = 0x34,	 /* thread-local storage (TLS) */
  TN_RELOC_X8664_TPOFF32_seg_reg = 0x35, /* like above, with segment register */
  TN_RELOC_X8664_GOTTPOFF   = 0x36,	 /* TLS with GOT entry */
  TN_RELOC_X8664_GOTNTPOFF  = 0x37,      /* TLS with GOT entry under PIC */
  TN_RELOC_X8664_DTPOFF     = 0x38,      /* Local Dynamic TLS */
  TN_RELOC_X8664_TLSGD      = 0x39,      /* Global Dynamic TLS */
  TN_RELOC_X8664_TLSLD      = 0x3a,      /* Local Dynamic TLS  */

				     /* IA-32 relocations start at 0x40 */
  TN_RELOC_IA32_ALL   = 0x40,	     /* All 32 bits of a symbol value. */
  TN_RELOC_IA32_GOT   = 0x41,
  TN_RELOC_IA32_GLOBAL_OFFSET_TABLE = 0x42,
  TN_RELOC_IA32_GOTOFF              = 0x43   /* 32 bit offset to GOT */
#endif
#if defined(TARG_SL)
TN_RELOC_GPREL_V1 = 0x41,
TN_RELOC_GPREL_V2 = 0x42,
TN_RELOC_GPREL_V4 = 0x43,
TN_RELOC_GPREL_S  = 0x44,
TN_RELOC_GPREL_V1_15 = 0x45,
TN_RELOC_GPREL_V2_15 = 0x46,
TN_RELOC_GPREL_V4_15 = 0x47,
/* 11bit offset from vbuf start address */ 
TN_RELOC_GPREL_SL2_V11 = 0x48,
/* 15bit offset from vbuf start address */
TN_RELOC_GPREL_SL2_V15 = 0x49,
/* 14bit offset from sbuf start address */ 
TN_RELOC_GPREL_SL2_S  = 0x50,
#endif 
} TN_RELOCS;


#define CAN_USE_TN(x)	(x)

/* Define the access functions: */
#define     TN_flags(t)		(CAN_USE_TN(t)->flags)
#define Set_TN_flags(t,x)	(CAN_USE_TN(t)->flags = (x))
/* define TN_is_{constant,register} ahead of time */
#define       TN_is_constant(r)	(TN_flags(r) &   TN_CONSTANT)
#define   Set_TN_is_constant(r)	(TN_flags(r) |=  TN_CONSTANT)
#define       TN_is_register(r)	(!TN_is_constant(r))
extern  BOOL is_str_expand;
inline TN * CAN_USE_REG_TN (const TN *t)
{
        if(!is_str_expand)
	  Is_True(TN_is_register(t), ("not a register tn"));
	return (TN*)t;
}

#ifdef TARG_NVISA
#define     TN_relocs(t)	(CAN_USE_TN(t)->urs.relocs)
#define Set_TN_relocs(t,x)	(CAN_USE_TN(t)->urs.relocs = (x))
#else
#define     TN_relocs(t)	(CAN_USE_TN(t)->relocs)
#define Set_TN_relocs(t,x)	(CAN_USE_TN(t)->relocs = (x))
#endif // TARG_NVISA
#define     TN_size(t)		(CAN_USE_TN(t)->size+0)
#define Set_TN_size(t,x)	(CAN_USE_TN(t)->size = (x))
#define     TN_number(t)	(CAN_USE_REG_TN(t)->u1.reg_tn.number+0)

#ifdef Is_True_On
extern int trace_tn_number_;
extern void set_trace_tn(int n);
extern void reset_trace_tn();
extern void gdb_stop_here();
#endif

inline void  Set_TN_number(TN *t, int x)
{
   (CAN_USE_REG_TN(t)->u1.reg_tn.number = (x));
#ifdef Is_True_On
   if (trace_tn_number_ == x)
      gdb_stop_here();
#endif
}

#define	    TN_class_reg(t)	(CAN_USE_REG_TN(t)->u1.reg_tn.class_reg)
#define	Set_TN_class_reg(t,x)	(CAN_USE_REG_TN(t)->u1.reg_tn.class_reg = (x))
#define     TN_register(t)	\
		(CLASS_REG_PAIR_reg(CAN_USE_REG_TN(t)->u1.reg_tn.class_reg)+0)
#define Set_TN_register(t,x)	\
		(Set_CLASS_REG_PAIR_reg(CAN_USE_REG_TN(t)->u1.reg_tn.class_reg,(x)))
#define     TN_register_class(t) \
		(CLASS_REG_PAIR_rclass(CAN_USE_REG_TN(t)->u1.reg_tn.class_reg))
#define Set_TN_register_class(t,x) \
		(Set_CLASS_REG_PAIR_rclass(CAN_USE_REG_TN(t)->u1.reg_tn.class_reg,(x)))
#define     TN_register_and_class(t) \
		(CLASS_REG_PAIR_class_n_reg(CAN_USE_REG_TN(t)->u1.reg_tn.class_reg)+0)
#define Set_TN_register_and_class(t,x) \
		(Set_CLASS_REG_PAIR_class_n_reg(CAN_USE_REG_TN(t)->u1.reg_tn.class_reg,(x)))
#define     TN_save_creg(t)	(CAN_USE_REG_TN(t)->u1.reg_tn.save_creg)
#define     TN_save_reg(t)	(CLASS_REG_PAIR_reg(TN_save_creg(t))+0)
#define     TN_save_rclass(t)	(CLASS_REG_PAIR_rclass(TN_save_creg(t)))
#define Set_TN_save_creg(t,x)	(CAN_USE_REG_TN(t)->u1.reg_tn.save_creg = (x))
#define     TN_is_save_reg(t)	(!CLASS_REG_PAIR_EqualP(TN_save_creg(t),CLASS_REG_PAIR_undef))
#define     TN_spill(t)		(CAN_USE_TN(t)->u2.u3.spill)
#define Set_TN_spill(t,x)	(CAN_USE_TN(t)->u2.u3.spill = (x))
#define     TN_spill_is_valid(t)(TN_is_register(t) && !(TN_is_rematerializable(t) || TN_is_gra_homeable(t)))
#define     TN_has_spill(t)	(TN_spill_is_valid(t) && (TN_spill(t) != NULL))
#define     TN_value(t)		(CAN_USE_TN(t)->u1.value)
#define Set_TN_value(t,x)	(CAN_USE_TN(t)->u1.value = (x))
#define     TN_offset(t)	(CAN_USE_TN(t)->u1.offset)
#define Set_TN_offset(t,x)	(CAN_USE_TN(t)->u1.offset = (x))
#define     TN_label(t)		(CAN_USE_TN(t)->u2.label)
#define Set_TN_label(t,x)	(CAN_USE_TN(t)->u2.label = (x))
#define     TN_enum(t)		(CAN_USE_TN(t)->u2.ecv)
#define Set_TN_enum(t,x)	(CAN_USE_TN(t)->u2.ecv = (x))
#define     TN_var(t)		(CAN_USE_TN(t)->u2.var)
#define Set_TN_var(t,x)		(CAN_USE_TN(t)->u2.var = (x))
#define     TN_home(t)		(CAN_USE_TN(t)->u2.u3.home)
#define Set_TN_home(t,x)	(CAN_USE_TN(t)->u2.u3.home = (x))

/* Define the TN_flags access functions: */
#define       TN_has_value(r)	(TN_flags(r) &   TN_HAS_VALUE)
#define   Set_TN_has_value(r)	(TN_flags(r) |=  TN_HAS_VALUE)
#define       TN_is_label(r)	(TN_flags(r) &   TN_LABEL)
#define   Set_TN_is_label(r)	(TN_flags(r) |=  TN_LABEL)
#define       TN_is_tag(r)	(TN_flags(r) &   TN_TAG)
#define   Set_TN_is_tag(r)	(TN_flags(r) |=  TN_TAG)
#define       TN_is_symbol(r)	(TN_flags(r) &   TN_SYMBOL)
#define   Set_TN_is_symbol(r)	(TN_flags(r) |=  TN_SYMBOL)
#define       TN_is_enum(r)	(TN_flags(r) &   TN_ENUM)
#define   Set_TN_is_enum(r)	(TN_flags(r) |=  TN_ENUM)

#define       TN_is_float(x)    (TN_flags(x) &   TN_FLOAT)
#define   Set_TN_is_float(x)    (TN_flags(x) |=  TN_FLOAT)
#define       TN_is_fpu_int(x)  (TN_flags(x) &   TN_FPU_INT)
#define   Set_TN_is_fpu_int(x)  (TN_flags(x) |=  TN_FPU_INT)
#define Reset_TN_is_fpu_int(x)  (TN_flags(x) &=  ~TN_FPU_INT)
#define       TN_is_global_reg(x) (TN_flags(x) &   TN_GLOBAL_REG)
#define   Set_TN_is_global_reg(x) (TN_flags(x) |=  TN_GLOBAL_REG)
#define Reset_TN_is_global_reg(x) (TN_flags(x) &= ~TN_GLOBAL_REG)
#define      TN_is_dedicated(r)	(TN_flags(r) &   TN_DEDICATED)
#define  Set_TN_is_dedicated(r)	(TN_flags(r) |=  TN_DEDICATED)
#define Reset_TN_is_dedicated(r) (TN_flags(r)&= ~TN_DEDICATED)

#define      TN_is_if_conv_cond(r)  (TN_flags(r) &   TN_IF_CONV_COND)
#define  Set_TN_is_if_conv_cond(r)  (TN_flags(r) |=  TN_IF_CONV_COND)
#define Reset_TN_is_if_conv_cond(r) (TN_flags(r) &= ~TN_IF_CONV_COND)

#define      TN_is_rematerializable(r)  (TN_flags(r) &   TN_REMATERIALIZABLE)
#define  Set_TN_is_rematerializable(r)  (TN_flags(r) |=  TN_REMATERIALIZABLE)
#define Reset_TN_is_rematerializable(r) (TN_flags(r) &= ~TN_REMATERIALIZABLE)

#define      TN_is_gra_homeable(r)  (TN_flags(r) &   TN_GRA_HOMEABLE)
#define  Set_TN_is_gra_homeable(r)  (TN_flags(r) |=  TN_GRA_HOMEABLE)
#define Reset_TN_is_gra_homeable(r) (TN_flags(r) &= ~TN_GRA_HOMEABLE)

#define      TN_is_gra_cannot_split(r)  (TN_flags(r) &   TN_GRA_CANNOT_SPLIT)
#define  Set_TN_is_gra_cannot_split(r)  (TN_flags(r) |=  TN_GRA_CANNOT_SPLIT)

#if defined(TARG_SL)
#define      TN_is_v1buf_addr(r)    (TN_flags(r) &   TN_V1BUF_ADDR)
#define  Set_TN_is_v1buf_addr(r)    (TN_flags(r) |=  TN_V1BUF_ADDR)
#endif 

#ifdef TARG_IA64
#define        TN_is_take_nat(r)   (TN_flags(r) &   TN_TAKE_NAT)
#define    Set_TN_is_take_nat(r)   (TN_flags(r) |=  TN_TAKE_NAT)
#define  Reset_TN_is_take_nat(r)   (TN_flags(r) &= ~TN_TAKE_NAT) 
#endif

#ifdef TARG_NVISA
#define      TN_is_boolean(r)  	(TN_flags(r) &   TN_BOOLEAN)
#define  Set_TN_is_boolean(r)  	(TN_flags(r) |=  TN_BOOLEAN)
#define Reset_TN_is_boolean(r) 	(TN_flags(r) &= ~TN_BOOLEAN)

#define	     TN_has_one_def(r)	TN_is_gra_cannot_split(r)
#define	 Set_TN_has_one_def(r)	Set_TN_is_gra_cannot_split(r)
#define	Reset_TN_has_one_def(r)	Reset_TN_is_gra_cannot_split(r)
#define      TN_has_memory_space(r)  (TN_flags(r) &   TN_MEMORY_SPACE)
#define  Set_TN_has_memory_space(r)  (TN_flags(r) |=  TN_MEMORY_SPACE)
#define Reset_TN_has_memory_space(r) (TN_flags(r) &= ~TN_MEMORY_SPACE)
#define     TN_memory_space(t)		(CAN_USE_TN(t)->urs.space)
#define Set_TN_memory_space(t,x)	(Set_TN_has_memory_space(t), CAN_USE_TN(t)->urs.space = (x))
#define     TN_in_global_mem(t)	  (TN_memory_space(t) & TN_GLOBAL_SPACE)
#define Set_TN_in_global_mem(t)	  (Set_TN_memory_space(t,TN_GLOBAL_SPACE))
#define     TN_in_shared_mem(t)	  (TN_memory_space(t) & TN_SHARED_SPACE)
#define Set_TN_in_shared_mem(t)	  (Set_TN_memory_space(t,TN_SHARED_SPACE))
#define     TN_in_const_mem(t)	  (TN_memory_space(t) & TN_CONST_SPACE)
#define Set_TN_in_const_mem(t)	  (Set_TN_memory_space(t,TN_CONST_SPACE))
#define     TN_in_local_mem(t)	  (TN_memory_space(t) & TN_LOCAL_SPACE)
#define Set_TN_in_local_mem(t)	  (Set_TN_memory_space(t,TN_LOCAL_SPACE))
#define     TN_in_param_mem(t)	  (TN_memory_space(t) & TN_PARAM_SPACE)
#define Set_TN_in_param_mem(t)	  (Set_TN_memory_space(t,TN_PARAM_SPACE))
#define     TN_in_texture_mem(t)  (TN_memory_space(t) & TN_TEXTURE_SPACE)
#define Set_TN_in_texture_mem(t)  (Set_TN_memory_space(t,TN_TEXTURE_SPACE))

#define TN_from_shared_load(r) (CAN_USE_TN(r)->from_shared_load)
#define Set_TN_from_shared_load(r) (CAN_USE_TN(r)->from_shared_load = TRUE)
#define Reset_TN_from_shared_load(r) (CAN_USE_TN(r)->from_shared_load = FALSE)
#endif

#ifdef TARG_X8664
#define       TN_is_preallocated(r)  (TN_flags(r) &   TN_PREALLOCATED)
#define   Set_TN_is_preallocated(r)  (TN_flags(r) |=  TN_PREALLOCATED)
#define Reset_TN_is_preallocated(r)  (TN_flags(r) &= ~TN_PREALLOCATED)

#define       TN_is_thread_seg_ptr(r)	(TN_flags(r) &   TN_THREAD_SEG_PTR)
#define   Set_TN_is_thread_seg_ptr(r)	(TN_flags(r) |=  TN_THREAD_SEG_PTR)
#define Reset_TN_is_thread_seg_ptr(r)	(TN_flags(r) &= ~TN_THREAD_SEG_PTR)

#define       TN_is_norename(r)	(TN_flags(r) &   TN_NO_RENAME)
#define   Set_TN_is_norename(r)	(TN_flags(r) |=  TN_NO_RENAME)
#define Reset_TN_is_norename(r)	(TN_flags(r) &= ~TN_NO_RENAME)
#endif

/* Macros to check if a TN is a particular dedicated register. */
#define TN_is_sp_reg(r)	   (TN_register_and_class(r) == CLASS_AND_REG_sp)
#define TN_is_gp_reg(r)	   (TN_register_and_class(r) == CLASS_AND_REG_gp)
#define TN_is_ep_reg(r)	   (TN_register_and_class(r) == CLASS_AND_REG_ep)
#define TN_is_fp_reg(r)	   (TN_register_and_class(r) == CLASS_AND_REG_fp)
#define TN_is_ra_reg(r)	   (TN_register_and_class(r) == CLASS_AND_REG_ra)
#define TN_is_zero_reg(r)  (TN_register_and_class(r) == CLASS_AND_REG_zero)
#define TN_is_static_link_reg(r) (TN_register_and_class(r) == CLASS_AND_REG_static_link)
#define TN_is_pfs_reg(r)   (TN_register_and_class(r) == CLASS_AND_REG_pfs)
#define TN_is_lc_reg(r)   (TN_register_and_class(r) == CLASS_AND_REG_lc)
#define TN_is_ec_reg(r)   (TN_register_and_class(r) == CLASS_AND_REG_ec)
#define TN_is_true_pred(r) (TN_register_and_class(r) == CLASS_AND_REG_true)
#define TN_is_fzero_reg(r) (TN_register_and_class(r) == CLASS_AND_REG_fzero)
#define TN_is_fone_reg(r)  (TN_register_and_class(r) == CLASS_AND_REG_fone)

// Check if the TN is either a constant zero or the zero register TN.
// If you know it is a register TN, use TN_is_zero_reg directly.
inline BOOL TN_is_zero (const TN *r) 
{
  return ((TN_has_value(r) && TN_value(r) == 0) || (TN_is_register(r) && TN_is_zero_reg(r)));
}

// Returns TRUE if the TN represents a hardwired registers.
inline BOOL TN_is_const_reg(const TN *r)
{
  return (TN_is_register(r) && 
	  TN_is_dedicated(r) &&
	  (TN_is_zero_reg(r) || 
	   TN_is_true_pred(r) ||
	   TN_is_fzero_reg(r) ||
	   TN_is_fone_reg(r)));
}

/* Define the TN_relocs access functions: */
#define TN_is_reloc_gprel16(r)		(TN_relocs(r) == TN_RELOC_GPREL16)
#define Set_TN_is_reloc_gprel16(r)	Set_TN_relocs(r,TN_RELOC_GPREL16)
#define TN_is_reloc_low16(r)		(TN_relocs(r) == TN_RELOC_LOW16)
#define Set_TN_is_reloc_low16(r)	Set_TN_relocs(r,TN_RELOC_LOW16)
#define TN_is_reloc_high16(r)		(TN_relocs(r) == TN_RELOC_HIGH16)
#define Set_TN_is_reloc_high16(r)	Set_TN_relocs(r,TN_RELOC_HIGH16)
#define TN_is_reloc_higher(r)		(TN_relocs(r) == TN_RELOC_HIGHER)
#define Set_TN_is_reloc_higher(r)	Set_TN_relocs(r,TN_RELOC_HIGHER)
#define TN_is_reloc_highest(r)		(TN_relocs(r) == TN_RELOC_HIGHEST)
#define Set_TN_is_reloc_highest(r)	Set_TN_relocs(r,TN_RELOC_HIGHEST)
#define TN_is_reloc_got_disp(r)		(TN_relocs(r) == TN_RELOC_GOT_DISP)
#define Set_TN_is_reloc_got_disp(r)	Set_TN_relocs(r,TN_RELOC_GOT_DISP)
#define TN_is_reloc_got_page(r)		(TN_relocs(r) == TN_RELOC_GOT_PAGE)
#define Set_TN_is_reloc_got_page(r)	Set_TN_relocs(r,TN_RELOC_GOT_PAGE)
#define TN_is_reloc_got_ofst(r)		(TN_relocs(r) == TN_RELOC_GOT_OFST)
#define Set_TN_is_reloc_got_ofst(r)	Set_TN_relocs(r,TN_RELOC_GOT_OFST)
#define TN_is_reloc_call16(r)		(TN_relocs(r) == TN_RELOC_CALL16)
#define Set_TN_is_reloc_call16(r)	Set_TN_relocs(r,TN_RELOC_CALL16)
#define TN_is_reloc_got_hi16(r)		(TN_relocs(r) == TN_RELOC_GOT_HI16)
#define Set_TN_is_reloc_got_hi16(r)	Set_TN_relocs(r,TN_RELOC_GOT_HI16)
#define TN_is_reloc_got_lo16(r)		(TN_relocs(r) == TN_RELOC_GOT_LO16)
#define Set_TN_is_reloc_got_lo16(r)	Set_TN_relocs(r,TN_RELOC_GOT_LO16)
#define TN_is_reloc_call_hi16(r)	(TN_relocs(r) == TN_RELOC_CALL_HI16)
#define Set_TN_is_reloc_call_hi16(r)	Set_TN_relocs(r,TN_RELOC_CALL_HI16)
#define TN_is_reloc_call_lo16(r)	(TN_relocs(r) == TN_RELOC_CALL_LO16)
#define Set_TN_is_reloc_call_lo16(r)	Set_TN_relocs(r,TN_RELOC_CALL_LO16)
#define TN_is_reloc_neg(r)		(TN_relocs(r) == TN_RELOC_NEG)
#define Set_TN_is_reloc_neg(r)		Set_TN_relocs(r,TN_RELOC_NEG)
#define TN_is_reloc_gpsub(r)		(TN_relocs(r) == TN_RELOC_GPSUB)
#define Set_TN_is_reloc_gpsub(r)	Set_TN_relocs(r,TN_RELOC_GPSUB)
#define TN_is_reloc_lo_gpsub(r)		(TN_relocs(r) == TN_RELOC_LO_GPSUB)
#define Set_TN_is_reloc_lo_gpsub(r)	Set_TN_relocs(r,TN_RELOC_LO_GPSUB)
#define TN_is_reloc_hi_gpsub(r)		(TN_relocs(r) == TN_RELOC_HI_GPSUB)
#define Set_TN_is_reloc_hi_gpsub(r)	Set_TN_relocs(r,TN_RELOC_HI_GPSUB)
#define TN_is_reloc_gpident(r)		(TN_relocs(r) == TN_RELOC_GPIDENT)
#define Set_TN_is_reloc_gpident(r)	Set_TN_relocs(r,TN_RELOC_GPIDENT)
#define TN_is_reloc_lo_gpident(r)	(TN_relocs(r) == TN_RELOC_LO_GPIDENT)
#define Set_TN_is_reloc_lo_gpident(r)	Set_TN_relocs(r,TN_RELOC_LO_GPIDENT)
#define TN_is_reloc_hi_gpident(r)	(TN_relocs(r) == TN_RELOC_HI_GPIDENT)
#define Set_TN_is_reloc_hi_gpident(r)	Set_TN_relocs(r,TN_RELOC_HI_GPIDENT)
#define TN_is_reloc_ia_imm14(r)		(TN_relocs(r) == TN_RELOC_IA_IMM14)
#define Set_TN_is_reloc_ia_imm14(r)	Set_TN_relocs(r,TN_RELOC_IA_IMM14)
#define TN_is_reloc_ia_imm22(r)		(TN_relocs(r) == TN_RELOC_IA_IMM22)
#define Set_TN_is_reloc_ia_imm22(r)	Set_TN_relocs(r,TN_RELOC_IA_IMM22)
#define TN_is_reloc_ia_pcrel(r)		(TN_relocs(r) == TN_RELOC_IA_PCREL)
#define Set_TN_is_reloc_ia_pcrel(r)	Set_TN_relocs(r,TN_RELOC_IA_PCREL)
#define TN_is_reloc_ia_gprel22(r)	(TN_relocs(r) == TN_RELOC_IA_GPREL22)
#define Set_TN_is_reloc_ia_gprel22(r)	Set_TN_relocs(r,TN_RELOC_IA_GPREL22)
#define TN_is_reloc_ia_ltoff22(r)	(TN_relocs(r) == TN_RELOC_IA_LTOFF22)
#define Set_TN_is_reloc_ia_ltoff22(r)	Set_TN_relocs(r,TN_RELOC_IA_LTOFF22)
#define TN_is_reloc_ia_ltoff_fptr(r)	(TN_relocs(r) == TN_RELOC_IA_LTOFF_FPTR)
#define Set_TN_is_reloc_ia_ltoff_fptr(r) Set_TN_relocs(r,TN_RELOC_IA_LTOFF_FPTR)
#ifdef TARG_X8664
#define TN_is_reloc_x8664_pc32(r)	(TN_relocs(r) == TN_RELOC_X8664_PC32)
#define Set_TN_is_reloc_x8664_pc32(r)	Set_TN_relocs(r,TN_RELOC_X8664_PC32)
#define TN_is_reloc_x8664_32(r)	        (TN_relocs(r) == TN_RELOC_X8664_32)
#define Set_TN_is_reloc_x8664_32(r)	Set_TN_relocs(r,TN_RELOC_X8664_32)
#define TN_is_reloc_x8664_gotpcrel(r)   (TN_relocs(r)==TN_RELOC_X8664_GOTPCREL)
#define Set_TN_is_reloc_x8664_gotpcrel(r) Set_TN_relocs(r,TN_RELOC_X8664_GOTPCREL)
#define TN_is_reloc_x8664_64(r)	        (TN_relocs(r) == TN_RELOC_X8664_64)
#define Set_TN_is_reloc_x8664_64(r)	Set_TN_relocs(r,TN_RELOC_X8664_64)
#define TN_is_reloc_x8664_tpoff32(r)	(TN_relocs(r) == TN_RELOC_X8664_TPOFF32)
#define Set_TN_is_reloc_x8664_tpoff32(r) Set_TN_relocs(r,TN_RELOC_X8664_TPOFF32)
#define TN_is_reloc_x8664_tpoff32_seg_reg(r)     (TN_relocs(r) == TN_RELOC_X8664_TPOFF32_seg_reg)
#define Set_TN_is_reloc_x8664_tpoff32_seg_reg(r) Set_TN_relocs(r,TN_RELOC_X8664_TPOFF32_seg_reg)
#endif /* TARG_X8664 */


/* ====================================================================
 *
 * External variables.
 *
 * ====================================================================
 */

/* The register TNs are in a table named TNvec, indexed by their TN 
 * numbers in the range 1..Last_TN.  The first part of the table, the 
 * range 1..Last_Dedicated_TN, consists of TNs for various dedicated 
 * purposes (e.g. stack pointer, zero, physical registers).  It is 
 * followed by TNs for user variables and compiler temporaries, in the 
 * range First_Regular_TN..Last_TN.
 */
extern TN_NUM Last_Dedicated_TN;/* The last dedicated TN number */
extern TN_NUM Last_Distinct_Dedicated_TN;
extern TN_NUM First_Regular_TN;	/* after all the preallocated TNs */
extern TN_NUM Last_TN;		/* The last allocated TN number */
extern TN_NUM First_REGION_TN;	/* The first non-dedicated TN in the current REGION */
extern TN **TN_Vec;		/* Mapping from number to TN */
#define TNvec(i) TN_Vec[i]


// The following are special-purpose TNs required in the compiler for
// specific purposes.  
// NOTE: Don't use these TNs directly in comparisons with other TNs.
//       Instead use the TN_is_xx_reg macros. This allows the 
//       comparisons to work with renaming of dedicated registers.
//
extern  TN *Zero_TN;		// Zero register TN
extern 	TN *FP_TN;		// Frame Pointer
extern  TN *SP_TN;		// Stack Pointer
extern	TN *RA_TN;		// Return address register
extern  TN *Ep_TN;		// Entry point TN
extern	TN *GP_TN;		// Global pointer register

#ifdef TARG_IA64
extern  TN *TP_TN;              // Thread Pointer register
#endif

extern	TN *Pfs_TN;		// Previous Function State TN
extern	TN *LC_TN;		// Loop Counter TN
extern	TN *EC_TN;		// Epilog Counter TN
extern	TN *True_TN;		// TN for true condition (predicate)
extern  TN *FZero_TN;		// Floating zero (0.0) register TN
extern  TN *FOne_TN;		// Floating one (1.0) register TN
#if defined(TARG_SL)
extern  TN *TMP1_TN;              // tmp TN, similar to AT-reg of mips1
extern  TN *TMP2_TN;              // tmp TN, similar to AT-reg of mips1
extern  TN *JA_TN;
extern  TN *LC0_TN;
extern  TN *LC1_TN;
extern  TN *LC2_TN;
extern  TN *LC3_TN;
extern  TN *HI_TN;
extern  TN *Acc0_TN;
extern  TN *Acc1_TN;
extern  TN *Acc2_TN;
extern  TN *Acc3_TN;
extern TN *Addr0_TN;
extern TN *Addr1_TN;
extern TN *Addr2_TN;
extern TN *Addr3_TN;
extern TN *Addr4_TN;
extern TN *Addr5_TN;
extern TN *Addr6_TN;
extern TN *Addr7_TN;
extern TN *Addrsize0_TN;
extern TN *Addrsize1_TN;
extern TN *Addrsize2_TN;
extern TN *Addrsize3_TN;
extern TN *Addrsize4_TN;
extern TN *Addrsize5_TN;
extern TN *Addrsize6_TN;
extern TN *Addrsize7_TN;
extern  PREG_NUM AccPregN ;
extern  PREG_NUM AddPregN;
extern int ACCreg[4];
extern int Addreg[8];

extern  TN* C2_ACC_TN;          // TN for c2 sum4 accumulator
extern  TN* C2_ACC_CTRL_TN;     // TN for c2 sum4 accumulator
extern  TN* C2_MVSEL_TN;        // TN for c2 mvsel internal 
extern  TN* C2_VLCS_TN;         // TN for c2 vlcs internal 
extern  TN* C2_MOVPAT_TN;       // TN for c2 mov pat register
#endif
#ifdef TARG_LOONGSON
extern  TN *HI_TN;		// Hi register used for mul/div
extern  TN *LO_TN;		// Lo register used for mul/div
extern  TN *SL_TN;		// Static link register only for f90
extern  TN *FPSR_TN;
#endif

/* ====================================================================
 * Prototypes of external routines.
 * ====================================================================
 */

/* Intialize the dedicated TNs at the start of the compilation. */
extern  void Init_Dedicated_TNs (void);

/* Initialize the TN data structure at the start of each PU. */ 
extern	void Init_TNs_For_PU (void);
/* Initialize the TN data structure at the start of each REGION. */ 
extern	void Init_TNs_For_REGION (void);

#if defined(TARG_PPC32)
extern TN * Gen_CR_TN (UINT cr);
#endif

/* TN generation: */

/* The following set of routines can be used only for register TNs */

extern TN* Gen_Register_TN (ISA_REGISTER_CLASS rclass, INT size);

#ifdef KEY
extern TN* Gen_Typed_Register_TN (TYPE_ID mtype, INT size);
#endif

extern  TN *Build_Dedicated_TN ( ISA_REGISTER_CLASS rclass, REGISTER reg, INT size);

/*
   TNs_Are_Equivalent
   Returns TRUE if TNs have the same base TN number or that they are
   assigned the same register.
*/
inline BOOL TNs_Are_Equivalent(TN *tn1, TN *tn2) 
{
  if ( (tn1 == tn2 ||
	(TN_register(tn1) != REGISTER_UNDEFINED &&
	 TN_register(tn2) != REGISTER_UNDEFINED &&
	 TN_register_and_class(tn1) == TN_register_and_class(tn2))))
    return TRUE;

  return FALSE;
}

#if defined(TARG_SL)
inline BOOL tn_registers_identical (TN *tn1, TN *tn2)
{
  return ((tn1 == tn2) ||
          ((TN_is_register(tn1) && TN_is_register(tn2) &&
	    (TN_is_dedicated(tn1) || 
	     (TN_register(tn1) != REGISTER_UNDEFINED)) &&           
	    (TN_is_dedicated(tn2) || 
	     (TN_register(tn2) != REGISTER_UNDEFINED)) &&           
	    (TN_register_and_class(tn1) == TN_register_and_class(tn2)))));
}
#endif

/* Build a TN that matches the register class */
inline TN* Build_RCLASS_TN (ISA_REGISTER_CLASS rclass)
{
	return Gen_Register_TN (rclass, 
		(REGISTER_bit_size(rclass, 
		 REGISTER_CLASS_last_register(rclass))/8) );
}

inline TN *Build_TN_Like(TN *tn)
{
  TN *new_tn;
  if (!TN_is_register(tn)) {
	/* assume int rclass? */
	DevWarn("Build_TN_Like called on non-register tn");
	new_tn = Gen_Register_TN (ISA_REGISTER_CLASS_integer, TN_size(tn));
  }
  else {
#if defined(TARG_SL)
    ISA_REGISTER_CLASS rc = TN_register_class(tn);
    if (tn == RA_TN || tn == JA_TN || tn == LC0_TN || tn == LC1_TN || tn == LC2_TN || tn == LC3_TN ||
	tn == HI_TN ) {
      rc = ISA_REGISTER_CLASS_integer;
    }
    new_tn = Gen_Register_TN(rc, TN_size(tn));
#elif defined(TARG_PPC32)
    ISA_REGISTER_CLASS rc = TN_register_class(tn);;
    if (tn == RA_TN) {
      rc = ISA_REGISTER_CLASS_integer;
    }
    new_tn = Gen_Register_TN(rc, TN_size(tn));
#else
    new_tn = Gen_Register_TN(TN_register_class(tn), TN_size(tn) );
#endif
  }
  /* Propogate fpu-int flag... */
  TN_flags(new_tn) |= (TN_flags(tn) & TN_FPU_INT);
  return new_tn;
}

inline TN *Build_TN_Of_Mtype(TYPE_ID mtype)
{
  ISA_REGISTER_CLASS rc;  // register class
  INT rs;                 // register size
#ifdef TARG_SL
  extern void Create_TN_Pair(TN* key, TN* pair);
#ifdef EMULATE_LONGLONG
  if (mtype == MTYPE_I8 || mtype == MTYPE_U8) {
    TYPE_ID new_mtype = (mtype == MTYPE_I8 ? MTYPE_I4 : MTYPE_U4);
    rc = Register_Class_For_Mtype(new_mtype);
    rs = MTYPE_RegisterSize(new_mtype);
    TN *tn1 = Gen_Register_TN (rc, rs);
    TN *tn2 = Gen_Register_TN (rc, rs);
    Create_TN_Pair (tn1, tn2);
    return tn1;
  }
#endif
#ifdef EMULATE_FLOAT_POINT
  if (mtype == MTYPE_F8) {
    rc = Register_Class_For_Mtype(MTYPE_U4);
    rs = MTYPE_RegisterSize(MTYPE_U4);
    TN *tn1 = Gen_Register_TN (rc, rs);
    TN *tn2 = Gen_Register_TN (rc, rs);
    Create_TN_Pair (tn1, tn2);
    return tn1;
  } else if (mtype = MTYPE_F4) {
    rc = Register_Class_For_Mtype(MTYPE_U4);
    rs = MTYPE_RegisterSize(MTYPE_U4);
    return Gen_Register_TN (rc, rs);
  }
#endif
#endif
  rc = Register_Class_For_Mtype(mtype);
  rs = MTYPE_RegisterSize(mtype);
  return Gen_Register_TN (rc, rs);
}

extern	TN *Dup_TN ( TN *tn );	/* Duplicate an existing TN */
extern  TN *Dup_TN_Even_If_Dedicated ( TN *tn ) ; /* Ditto, but for
                                                   * dedicated */

/* Register assignment: */

inline BOOL TN_Is_Allocatable( const TN *tn )
{
  return ! ( TN_is_constant(tn) || TN_is_dedicated(tn));
}

inline void TN_Allocate_Register( TN *tn, REGISTER reg )
{
  Is_True (TN_Is_Allocatable(tn), ("Invalid TN for register allocation"));
  Set_TN_register(tn, reg);
}

inline BOOL TN_is_dedicated_class_and_reg( TN *tn, UINT16 class_n_reg )
{
  return    TN_is_dedicated(tn)
	 && TN_register_and_class(tn) == class_n_reg;
}

#ifdef TARG_SL
inline BOOL TN_is_AccRegister(TN *tn) {
  if (tn == Acc0_TN || tn == Acc1_TN || tn == Acc2_TN || tn == Acc3_TN) {
    return TRUE;
  } 
  return FALSE;
}

inline BOOL TN_is_AddrRegister(TN *tn) {
  if (tn == Addr0_TN || tn == Addr1_TN || tn == Addr2_TN ||
      tn == Addr3_TN || tn == Addr4_TN || tn == Addr5_TN ||
      tn == Addr6_TN || tn == Addr7_TN) {
    return TRUE;
  }
  return FALSE;
}

inline BOOL TN_is_LoopRegister(TN *tn) {
  if (tn == LC0_TN || tn == LC1_TN || tn == LC2_TN || tn == LC3_TN) {
    return TRUE;
  }
  return FALSE;
}

inline BOOL TN_is_AddrSizeRegister(TN *tn) {
  if (tn == Addrsize0_TN || tn == Addrsize1_TN || tn == Addrsize2_TN ||
      tn == Addrsize3_TN || tn == Addrsize4_TN || tn == Addrsize5_TN ||
      tn == Addrsize6_TN || tn == Addrsize7_TN) {
    return TRUE;
  }
  return FALSE;
}

#endif

/* Only the following routines should be used to build constant TNs. */

extern	TN *Gen_Literal_TN ( INT64 val, INT size );

// Produce a literal TN with the given literal value and decide the size automatically.
inline TN * Gen_Literal_TN_Ex ( INT64 val )
{
  if (val >= INT32_MIN && val <= INT32_MAX)
    return Gen_Literal_TN(val, 4);
  else
    return Gen_Literal_TN(val, 8);
}

// normally literals are hashed and reused; this creates unique TN
extern TN *Gen_Unique_Literal_TN (INT64 ivalue, INT size);

inline TN *Gen_Literal_TN_Of_Mtype (INT64 val, TYPE_ID mtype) 
{
	return Gen_Literal_TN (val, MTYPE_is_size_double(mtype) ? 8 : 4);
}
extern TN *Gen_Enum_TN (ISA_ENUM_CLASS_VALUE ecv);

extern  TN *Gen_Symbol_TN ( ST *s, INT64 offset, INT32 relocs);
extern  TN *Gen_Label_TN ( LABEL_IDX lab, INT64 offset );
extern  TN *Gen_Tag_TN ( LABEL_IDX tag);
extern	TN *Gen_Adjusted_TN( TN *tn, INT64 adjust );


/* Trace support: */
#ifdef TARG_IA64
extern char * sPrint_TN ( TN *tn, BOOL verbose, char *buf );
#endif
/* Print TN to a file with given 'fmt'; assume fmt has a %s in it. */
extern	void  fPrint_TN ( FILE *f, const char *fmt, TN *tn);
#pragma mips_frequency_hint NEVER fPrint_TN
/* Print TN to the trace file TFile */
extern	void   Print_TN ( TN *tn, BOOL verbose );
#pragma mips_frequency_hint NEVER Print_TN
/* Print a tn list to a file */
extern	void   Print_TN_List ( FILE *, TN_LIST * );
#pragma mips_frequency_hint NEVER Print_TN_List
/* Print all TNs */
extern	void   Print_TNs ( void );
#pragma mips_frequency_hint NEVER Print_TNs

/* Return the first tn in list which matches the register_class and register of tn0 */
/* If none is found, return NULL. */
extern TN *Find_TN_with_Matching_Register( TN *tn0, TN_LIST *list );

typedef enum {VAL_KNOWN, VAL_UNKNOWN, VAL_COND_DEF, VAL_COND_USE} DEF_KIND;

/* TN value support: */
extern struct op *TN_Reaching_Value_At_Op( TN *tn, struct op *op, DEF_KIND *kind, BOOL reaching_def );
extern BOOL TN_Value_At_Op( TN *tn, struct op *use_op, INT64 *val );

/* Determine whether a given expression involving a constant TN (which
 * may be a symbol TN) is a valid literal operand for the given opcode:
 */
extern BOOL Potential_Immediate_TN_Expr (
  TOP opcode,		/* The operation of interest */
  struct tn *tn1,	/* The primary TN (constant) */
  INT32	disp		/* Displacement from value */
);

BOOL Is_OP_Cond(OP *op);

#include "tn_targ.h"

#endif /* tn_INCLUDED */
