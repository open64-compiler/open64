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


#ifndef REGISTER_INCLUDED
#define REGISTER_INCLUDED

/* ====================================================================
 * ====================================================================
 *
 *  Module: register.h
 *  $Revision: 1.7 $
 *  $Date: 06/03/20 17:31:48-08:00 $
 *  $Author: gautam@jacinth.keyresearch $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.register.h $
 *
 *  Revision history:
 *   07-May-93 - Original Version
 *
 *  Description:
 *
 *      Registers within specific register classes and various
 *      operations on them.
 *
 *  Reserved prefix:
 *
 *      REGISTER
 *
 *  Types:
 *
 *      TYPE REGISTER, mREGISTER
 *
 *          Represents a register within a ISA_REGISTER_CLASS.  The
 *          ISA_REGISTER_CLASS information is not determined from the
 *          REGISTER, but must be supplied in order to perform various
 *          important operations on REGISTERs.  mREGISTER is the most
 *          compact representation of a REGISTER, while REGISTER is
 *          its most cpu efficient representation form.  The two types
 *          may be freely coerced form one to another. REGISTERs are a
 *          small numeric type and may be freely treated as such.
 *          (The first one in any class is always REGISTER_MIN).
 *
 *
 *      TYPE ISA_REGISTER_CLASS, mISA_REGISTER_CLASS (defined by targinfo)
 *
 *          Represents a register class.  Together with a REGISTER,
 *          determines a unique register.  ISA_REGISTER_CLASS's are a
 *          small numeric type and may be freely treated as such.
 *
 *
 *      TYPE ISA_REGISTER_SUBCLASS, mISA_REGISTER_SUBCLASS (defined by targinfo)
 *
 *          Represents a register subclass.  ISA_REGISTER_SUBCLASS's are a
 *          small numeric type and may be freely treated as such.
 *
 *
 *      TYPE REGISTER_SET
 *
 *          A set of REGISTERs.  Notice that REGISTER_SETs are small,
 *          fixed sized objects.  For this reason, we have chosen to
 *          implement them as by-value object (may be a scalar or
 *	    structure depending on the number of registers and word size).
 *	    This has the implication that you may declare and allocate a
 *          REGISTER_SET, 'set' like this:
 *
 *              REGISTER_SET set;
 *
 *	    NOTE: Due to by-value parameter and return semantics whether
 *	    or not the base type is a scalar or a structure is transparent
 *	    to the client except for uses of constants and relational tests.
 *	    Clients should only use the constants and functions defined here.
 *
 *          Functions cannot side effect REGISTER_SETS, instead they
 *          must assign their result.  For example:
 *
 *              set = REGISTER_SET_Intersection(set,...)
 *
 *          REGISTER_SETs may also be directly assigned as a way of
 *          copying them:
 *
 *              set1 = set2;
 *
 *          Now operations on set1 will not effect set2.
 *
 *
 *  REGISTER constants
 *
 *      const REGISTER REGISTER_UNDEFINED
 *
 *          A special register that is out of the range of any
 *          REGISTER_SET.  May be used, e.g. to represent unassigned
 *          REGISTERs.  This is really 0, so you may rely on REGISTERs in
 *          cleared memory being undefined.
 *
 *      const REGISTER REGISTER_MIN
 *
 *          The least register.  This is alway the least member in
 *          every ISA_REGISTER_CLASS that has a member.  Use with
 *          REGISTER_CLASS_last_register (see below) to loop over the
 *          members of a ISA_REGISTER_CLASS.
 *
 *      const INT32 REGISTER_MAX
 *
 *          The largest possible REGISTER.  Use to declare vectors
 *          indexed by REGISTER:
 *
 *              float REGISTER_priority[REGISTER_MAX+1];
 *
 *
 *  Special purpose registers:
 *
 *	CLASS_REG_PAIR     CLASS_REG_PAIR_xxx;
 *      REGISTER           REGISTER_xxx
 *	ISA_REGISTER_CLASS REGISTER_CLASS_xxx
 *	mINT16             CLASS_AND_REG_xxx
 *
 *	    Registers which have a special purpose in the ABI or hardware.
 *	    'xxx' is one of the following:
 *
 *		zero - the read zero / write sink register
 *		ep   - entry point
 *		gp   - global pointer
 *		sp   - stack pointer
 *		fp   - frame pointer
 *		ra   - return address
 *		v0   - integer function return value
 *		undef- undefined class and undefined register
 *		f0   - floating-point function return value for X8664
 *
 *
 *  Initialization:
 *
 *      void REGISTER_Begin(void)
 *
 *          Initialize the package.  Needs to be called once per run
 *          of the be, before any other operations in this module are
 *          used.
 *
 *
 *      void REGISTER_Pu_Begin(void)
 *
 *          Initialize for a new PU.  Needs to be called once per PU
 *          compilation before any other operations in this module are
 *          used.
 *
 *
 *  Miscellaneous:
 *
 *	void REGISTER_Set_Allocatable(
 *	    ISA_REGISTER_CLASS rclass
 *	    REGISTER           reg
 *	    BOOL               is_allocatable
 *	)
 *
 *	    Control whether or not 'reg' in class 'rclass' can be allocated. 
 *	    If 'is_allocatable' is true it can be, otherwise it cannot. 
 *
 *
 *	ISA_REGISTER_CLASS Register_Class_For_Mtype(
 *	    TYPE_ID mtype
 *	)
 *
 *	    Given an MTYPE, return the corresponding register class
 *	    used to hold values of that type. Return 
 *	    ISA_REGISTER_CLASS_UNDEFINED if there is no correspondence.
 *
 *  Mapping from TNs to ISA_REGISTER_CLASSes
 *
 *      void REGISTER_CLASS_OP_Update_Mapping(
 *          struct op *op
 *      )
 *
 *          Update TN -> ISA_REGISTER_CLASS mappings for all TNs
 *          referenced in the given 'op'.
 *
 *
 *      ISA_REGISTER_CLASS TN_register_class(
 *          TN *tn
 *      )
 *
 *          Return the REGISTER_CLASS of the given 'tn'.  This is 
 *	    a field in the TN data structure. So, we now have a direct
 *	    field access to get this.
 *
 *
 *  REGISTER_SET operations
 *
 *	REGISTER_SET REGISTER_CLASS_universe(
 *          ISA_REGISTER_CLASS rclass
 *      )
 *
 *	    The REGISTER_SET containing every member of 'rclass'.
 *
 *
 *	REGISTER_SET REGISTER_CLASS_allocatable(
 *          ISA_REGISTER_CLASS rclass
 *      )
 *
 *	    The REGISTER_SET containing the allocatable members of 'rclass'.
 *
 *
 *	const REGISTER_SET REGISTER_SET_EMPTY_SET
 *
 *          The empty REGISTER_SET.
 *
 *
 *	REGISTER_SET REGISTER_SET_Difference_Range(
 *          REGISTER_SET  set,
 *          REGISTER      low,
 *          REGISTER      high
 *      )
 *
 *          Return Difference('set',{'low':'high'}).  In other words,
 *	    'set', with the elements low..high removed if they were present.
 *
 *
 *	REGISTER_SET REGISTER_SET_Intersection(
 *          REGISTER_SET set1
 *          REGISTER_SET set2
 *      )
 *
 *          Return the intersection of set1, set2.
 *
 *
 *	REGISTER_SET REGISTER_SET_Union(
 *          REGISTER_SET set1,
 *          REGISTER_SET set2
 *      )
 *
 *          Returns the union of the two 'set's.
 *
 *
 *	REGISTER_SET REGISTER_SET_Difference(
 *          REGISTER_SET set1,
 *          REGISTER_SET set2
 *      )
 *
 *          Returns the 'set1' with any members of 'set2' removed.
 *
 *
 *	REGISTER_SET REGISTER_SET_Difference1(
 *          REGISTER_SET set,
 *          REGISTER     reg
 *      )
 *
 *          Return Difference('set',{'reg'})
 *
 *
 *      REGISTER_SET REGISTER_SET_Union1(
 *          REGISTER_SET set,
 *          REGISTER     reg
 *      )
 *
 *          Return Union('set',{'reg'})
 *
 *
 *      REGISTER_SET REGISTER_SET_Intersection1(
 *          REGISTER_SET set,
 *          REGISTER     reg
 *      )
 *
 *          Return Intersection('set',{'reg'})
 *
 *
 *      BOOL REGISTER_SET_MemberP(
 *          REGISTER_SET set,
 *          REGISTER     reg
 *      )
 *
 *          Is 'reg' a member of 'set'?
 *
 *
 *      BOOL REGISTER_SET_IntersectsP(
 *          REGISTER_SET set1,
 *          REGISTER_SET set2
 *      )
 *
 *          Is the intersection of the two 'set's non-empty?
 *
 *
 *      BOOL REGISTER_SET_ContainsP(
 *          REGISTER_SET set1,
 *          REGISTER_SET set2
 *      )
 *
 *          Does set1 contain set2? (Is set2 a subset of set1?)
 *
 *
 *      REGISTER REGISTER_SET_Choose(
 *          REGISTER_SET set
 *      )
 *
 *          Return a member of the given 'set'.  This is deterministic, 
 *          the lowest member is always returned. If the set is empty, 
 *          REGISTER_UNDEFINED is returned.
 *
 *
 *      REGISTER REGISTER_SET_Choose_Next(
 *          REGISTER_SET set,
 *          REGISTER     reg
 *      )
 *
 *          Selects the "next" register in the set, excluding any registers
 *          from the minimum register through 'reg' inclusive.  If there is 
 *          no such member, REGISTER_UNDEFINED is returned.
 *
 *
 *      RESISTER REGISTER_SET_Choose_Range(
 *          REGISTER_SET set,
 *          REGISTER     low,
 *          REGISTER     high
 *      )
 *
 *          Return a member of the given 'set' in the range 'low'..'high'.  
 *          This is deterministic, the lowest member is always returned.  
 *          If there is no such member, REGISTER_UNDEFINED is returned.
 *
 *
 *      REGISTER REGISTER_SET_Choose_Intersection(
 *          REGISTER_SET set1,
 *          REGISTER_SET set2
 *      )
 *
 *          Return a member of the intersection of 'set1' and 'set2'.  
 *          As with the previous Choose functions, this is deterministic, 
 *          picking the least element.  Also retruns REGISTER_UNDEFINED 
 *          to indicate that no element was found.
 *
 *
 *      INT32 REGISTER_SET_Size(
 *          REGISTER_SET set
 *      )
 *
 *          Returns the cardinality of 'set'.  Much more effecient
 *          than looping and counting.
 *
 *
 *      REGISTER_SET REGISTER_SET_Range(UINT low, UINT high)
 *          Returns the register set created from range [low..high]
 *
 *
 *  Looping over the elements in a REGISTER_SET:
 *
 *      Use REGISTER_SET_Choose and REGISTER_SET_Choose_Next.  For
 *      example: *
 *
 *          for ( member = REGISTER_SET_Choose(set);
 *                member != REGISTER_UNDEFINED;
 *                member = REGISTER_SET_Choose_Next(set,member)
 *          ) {
 *            member is a member of the set
 *          }
 *
 *
 *  Machine dependent information:
 *
 *	const char *REGISTER_name(
 *	    ISA_REGISTER_CLASS rclass,
 *          REGISTER           reg
 *	)
 *
 *          Returns the assembly language name of the given register.
 *
 *
 *	BOOL REGISTER_allocatable(
 *          ISA_REGISTER_CLASS rclass,
 *          REGISTER           reg
 *      )
 *
 *	    Returns TRUE if the register can be allocated by the
 *	    register allocators.
 *
 *
 *	INT REGISTER_machine_id(
 *          ISA_REGISTER_CLASS rclass,
 *          REGISTER           reg
 *      )
 *
 *	    Returns the real machine integer id of the given register
 *
 *
 *	INT REGISTER_bit_size(
 *          ISA_REGISTER_CLASS rclass,
 *          REGISTER           reg
 *      )
 *
 *	    Returns the size in bits of the given register.
 *
 *
 *      const ISA_REGISTER_CLASS *REGISTER_CLASS_vec
 *
 *          A vector of the register classes referenced in the current
 *          PU.
 *
 *      const char *REGISTER_CLASS_name(
 *	    ISA_REGISTER_CLASS rclass
 *	)
 *
 *	    Gives the name of the register class.
 *
 *      INT32 REGISTER_CLASS_register_count(
 *          ISA_REGISTER_CLASS rclass
 *      )
 *
 *          Gives the total number of registers in 'rclass'.
 *          This is _NOT_ the number of allocatable registers.
 *
 *      REGISTER REGISTER_CLASS_last_register(
 *          ISA_REGISTER_CLASS rclass
 *      )
 *
 *          Gives the last register in the class.  Use with REGISTER_MIN
 *	    to loop over _ALL_ the registers in 'rclass':
 *
 *              REGISTER reg;
 *
 *              for ( reg = REGISTER_MIN;
 *                    reg <= REGISTER_CLASS_last_register(class);
 *                    ++reg
 *              ) {
 *                  reg is a member of the class
 *              }
 *
 *      REGISTER_SET REGISTER_CLASS_callee_saves(
 *          ISA_REGISTER_CLASS rclass
 *      )
 *      REGISTER_SET REGISTER_CLASS_caller_saves(
 *          ISA_REGISTER_CLASS rclass
 *      )
 *      REGISTER_SET REGISTER_CLASS_function_argument(
 *          ISA_REGISTER_CLASS rclass
 *      )
 *      REGISTER_SET REGISTER_CLASS_function_value(
 *          ISA_REGISTER_CLASS rclass
 *      )
 *      REGISTER_SET REGISTER_CLASS_shrink_wrap(
 *          ISA_REGISTER_CLASS rclass
 *      )
 *      REGISTER_SET REGISTER_CLASS_stacked(
 *          ISA_REGISTER_CLASS rclass
 *      )
 *      REGISTER_SET REGISTER_CLASS_rotating(
 *          ISA_REGISTER_CLASS rclass
 *      )
 *
 *          Returns the REGISTER_SET within the given 'rclass' of a
 *          particular type.
 *
 *
 *	BOOL REGISTER_CLASS_can_store(
 *	    ISA_REGISTER_CLASS rclass
 *	)
 *
 *	    Indicates if the class has operations to save and restore 
 *	    the registers within the class.
 *
 *
 *	BOOL REGISTER_CLASS_multiple_save(
 *	    ISA_REGISTER_CLASS rclass
 *	)
 *
 *	    Indicates if the class has operations to save and restore 
 *	    multiple registers at once within the class.
 *
 *
 *	const char *REGISTER_SUBCLASS_name(
 *	    ISA_REGISTER_SUBCLASS sc
 *	)
 *
 *	    Returns the name of the register subclass.
 *
 *
 *	REGISTER_SET REGISTER_SUBCLASS_members(
 *	    ISA_REGISTER_SUBCLASS sc
 *	)
 *
 *	    Returns the set of registers in the given register subclass.
 *
 *
 *	ISA_REGISTER_CLASS REGISTER_SUBCLASS_register_class(
 *	    ISA_REGISTER_SUBCLASS sc
 *	)
 *
 *	    Returns the register class of the registers in the given
 *	    subclass.
 *
 *
 *	const char *REGISTER_SUBCLASS_reg_name(
 *	    ISA_REGISTER_SUBCLASS sc
 *	    REGISTER reg
 *	)
 *
 *	    Returns the assembly language name of the register
 *	    in the given subclass -- a NULL pointer is returned
 *	    if there is no special name for this use of the register.
 *
 *
 *  CLASS_REG_PAIR utilities
 *	
 *	BOOL CLASS_REG_PAIR_EqualP(
 *	    CLASS_REG_PAIR crp1,
 *	    CLASS_REG_PAIR crp2
 *	)
 *
 *	    Determine if the two registers are equal
 *
 *
 *  Tracing and Printing
 *
 *      void REGISTER_SET_Print(
 *          REGISTER_SET  set,
 *          FILE         *f
 *      )
 *
 *          Prints out the register set to the given file.
 *
 *
 *	void REGISTER_Print(
 *	    ISA_REGISTER_CLASS  rclass,
 *	    REGISTER            reg,
 *	    FILE               *f
 *	)
 *
 *          Prints the assembly language name of the given register.
 *
 *	void CLASS_REG_PAIR_Print(
 *	    CLASS_REG_PAIR crp,
 *	    FILE           *f
 *	)
 *
 *          Prints the assembly language name of the given register.
 *
 *	void REGISTER_Trace(
 *	    ISA_REGISTER_CLASS rclass,
 *	    REGISTER           reg
 *	)
 *
 *	    Print information about the specified register to the trace file.
 *
 *
 *	void REGISTER_CLASS_Trace(
 *	    ISA_REGISTER_CLASS rclass
 *	)
 *
 *	    Print information about the specified class to the trace file.
 *
 *
 *	void REGISTER_CLASS_Trace_All(void)
 *
 *	    Prints information about all classes to the trace file.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static const char register_rcs_id[] = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.register.h $ $Revision: 1.7 $";
#endif /* _KEEP_RCS_ID */

#include "mtypes.h"
#include "targ_isa_registers.h"

struct op;
struct tn;

/* Types
 * =====
 */

typedef UINT   REGISTER;
//typedef mUINT8 mREGISTER;
// Use mUINT16 now because some architecture, e.g. NVISA, can have a large amount 
// of registers.
typedef mUINT16 mREGISTER;

/* define 32-bit structure to hold both the class and register number,
   and a union with an mUINT32 so that we can efficiently compare
   register class-number pairs */
typedef union class_reg_pair {
  mUINT32	class_n_reg;
  struct class_reg_struct {
    mREGISTER 	        reg;
    mISA_REGISTER_CLASS rclass; 
    } class_reg;
} CLASS_REG_PAIR;

/* Define the access macros
 */
#define Set_CLASS_REG_PAIR_class_n_reg(x,cnr)	((x).class_n_reg = (cnr))
#define     CLASS_REG_PAIR_class_n_reg(x)	((x).class_n_reg+0)
#define Set_CLASS_REG_PAIR_rclass(x,rc)		((x).class_reg.rclass = (rc))
#define     CLASS_REG_PAIR_rclass(x)		((ISA_REGISTER_CLASS)(x).class_reg.rclass)
#define Set_CLASS_REG_PAIR_reg(x,r)		((x).class_reg.reg = (r))
#define     CLASS_REG_PAIR_reg(x)		((x).class_reg.reg+0)

// Do not inline this function to avoid a g++ optimizer bug!
extern void Set_CLASS_REG_PAIR(CLASS_REG_PAIR& rp, ISA_REGISTER_CLASS rclass, REGISTER reg);

/* Create a CLASS_REG_PAIR from rclass/reg pair */
#define CREATE_CLASS_N_REG(rclass,reg)	((mUINT16)(((reg)<<8)|(rclass)))
#define CONSTRUCT_CLASS_REG_PAIR(cl_reg_pair,rclass,reg)	\
		(Set_CLASS_REG_PAIR_class_n_reg(cl_reg_pair, \
				CREATE_CLASS_N_REG(rclass,reg))

/* Define register-set type and REGISTER_SET_EMPTY_SET constant based 
 * on max number of registers. We also define a number of macros
 * that are reserved to the implementation.
 *
 * Implementation note: The idea is that where possible, a REGISTER_SET
 * will be implemented as a single word and thus could be nearly
 * always kept in registers and operated on very effeciently.  When 
 * the target has more registers in some class that can be represented 
 * by a single word, we use a structure containing an array of words. 
 */
#if ISA_REGISTER_MAX < 32

typedef UINT32 REGISTER_SET;

#define REGISTER_SET_EMPTY_SET		((REGISTER_SET)0)

/* These are private!!!!
 */
#define REGISTER_SET_WORD		UINT32
#define REGISTER_SET_ELEM(set, idx)	(set)
#define REGISTER_SET_IDX(iv)
#define FOR_REGISTER_SET(init, test, incr) if (0)
#define MAX_REGISTER_SET_IDX		(0)
#define REGISTER_SET_WORD_IDX(bit)	(0)
#define REGISTER_SET_BIT_IDX(bit)	(bit)

#elif ISA_REGISTER_MAX < 64

typedef UINT64 REGISTER_SET;

#define REGISTER_SET_EMPTY_SET		((REGISTER_SET)0)

/* These are private!!!!
 */
#define REGISTER_SET_WORD		UINT64
#define REGISTER_SET_ELEM(set, idx)	(set)
#define REGISTER_SET_IDX(iv)
#define FOR_REGISTER_SET(init, test, incr) if (0)
#define MAX_REGISTER_SET_IDX		(0)
#define REGISTER_SET_WORD_IDX(bit)	(0)
#define REGISTER_SET_BIT_IDX(bit)	(bit)

#else

typedef struct {
  UINT64 v[(ISA_REGISTER_MAX/64)+1];
} REGISTER_SET;

extern const REGISTER_SET REGISTER_SET_EMPTY_SET;

/* These are private!!!!
 */
#define REGISTER_SET_WORD		UINT64
#define REGISTER_SET_ELEM(set, idx)	((set).v[(idx)])
#define REGISTER_SET_IDX(iv)		INT iv
#define FOR_REGISTER_SET(init, test, incr) for ((init); (test); (incr))
#define MAX_REGISTER_SET_IDX		(ISA_REGISTER_MAX/64)
#define REGISTER_SET_WORD_IDX(bit)	((bit) >> 6)
#define REGISTER_SET_BIT_IDX(bit)	((bit) & (64-1))

#endif


/* We'll want to make our tables readonly for our clients, but we need
 * to be able to write them:
 */
#ifdef INCLUDING_IN_REGISTER
#define REGISTER_CLIENT_CONST
#else
#define REGISTER_CLIENT_CONST const
#endif


/* Constants
 */
#define REGISTER_UNDEFINED          ((REGISTER) 0)
#define REGISTER_MIN                ((REGISTER) 1)
#define REGISTER_MAX                ((REGISTER) (ISA_REGISTER_MAX+REGISTER_MIN))


/* Exported data
 * =============
 */

/* This will hold cached information about each register class.  It's
 * not really exported.  See below for exported access macros.
 */
typedef struct {
  mUINT16           reg_machine_id[REGISTER_MAX + 1];
  mUINT8            reg_bit_size[REGISTER_MAX + 1];
  mBOOL             reg_allocatable[REGISTER_MAX + 1];
#if !defined(TARG_NVISA)
  const char       *reg_name[REGISTER_MAX + 1];
#endif

  mBOOL             can_store;
  mBOOL		    multiple_save;
  mUINT16           register_count;
  const char       *name;
  REGISTER_SET      universe;
  REGISTER_SET      allocatable;
  REGISTER_SET      caller_saves;
  REGISTER_SET      callee_saves;
  REGISTER_SET      function_value;
  REGISTER_SET      function_argument;
  REGISTER_SET      shrink_wrap;
  REGISTER_SET	    stacked;
  REGISTER_SET      rotating;
#ifdef TARG_LOONGSON
  REGISTER_SET      assembler_temporary;
#endif
//#ifdef TARG_X8664
//  /* Set of registers that are eight-bit addressable. */
//  REGISTER_SET      eight_bit_regs;
//#endif
} REGISTER_CLASS_INFO;


extern REGISTER_CLIENT_CONST ISA_REGISTER_CLASS
REGISTER_CLASS_vec[ISA_REGISTER_CLASS_MAX + 1];

/* Cached information about each ISA_REGISTER_CLASS:
 */
extern REGISTER_CLIENT_CONST REGISTER_CLASS_INFO
REGISTER_CLASS_info[ISA_REGISTER_CLASS_MAX + 1];

/* Accessing cached information about a ISA_REGISTER_CLASS
 */
#define REGISTER_CLASS_reg_allocatable(x)			\
				(REGISTER_CLASS_info[x].reg_allocatable)
#define REGISTER_CLASS_reg_machine_id(x)			\
				(REGISTER_CLASS_info[x].reg_machine_id)
#define REGISTER_CLASS_reg_name(x)				\
				(REGISTER_CLASS_info[x].reg_name)
#define REGISTER_CLASS_reg_bit_size(x)				\
				(REGISTER_CLASS_info[x].reg_bit_size)
#define REGISTER_CLASS_name(x)	(REGISTER_CLASS_info[x].name)
#define REGISTER_CLASS_register_count(x)			\
                                (REGISTER_CLASS_info[x].register_count)
#define REGISTER_CLASS_universe(x)				\
                                (REGISTER_CLASS_info[x].universe)
#define REGISTER_CLASS_allocatable(x)				\
                                (REGISTER_CLASS_info[x].allocatable)
#define REGISTER_CLASS_caller_saves(x)				\
                                (REGISTER_CLASS_info[x].caller_saves)
#define REGISTER_CLASS_callee_saves(x)				\
                                (REGISTER_CLASS_info[x].callee_saves)
#define REGISTER_CLASS_stacked(x)				\
                                (REGISTER_CLASS_info[x].stacked)
#define REGISTER_CLASS_rotating(x)				\
                                (REGISTER_CLASS_info[x].rotating)
#define REGISTER_CLASS_function_value(x)			\
                                (REGISTER_CLASS_info[x].function_value)
#define REGISTER_CLASS_function_argument(x)			\
                                (REGISTER_CLASS_info[x].function_argument)
#define REGISTER_CLASS_shrink_wrap(x)				\
                                (REGISTER_CLASS_info[x].shrink_wrap)
#define REGISTER_CLASS_can_store(x)				\
                                (REGISTER_CLASS_info[x].can_store)
#define REGISTER_CLASS_multiple_save(x)				\
                                (REGISTER_CLASS_info[x].multiple_save)
#ifdef TARG_LOONGSON
#define REGISTER_CLASS_assembler_temporary(x)               \
                                (REGISTER_CLASS_info[x].assembler_temporary)
#endif
//#ifdef TARG_X8664
//#define REGISTER_CLASS_eight_bit_regs(x)			\
//                                (REGISTER_CLASS_info[x].eight_bit_regs)
//#endif
#define REGISTER_CLASS_last_register(x)				\
			(REGISTER_CLASS_register_count(x) + REGISTER_MIN - 1)

#define REGISTER_machine_id(rclass,reg)				\
                                (REGISTER_CLASS_reg_machine_id(rclass)[reg])
#define REGISTER_allocatable(rclass,reg)			\
				(REGISTER_CLASS_reg_allocatable(rclass)[reg])
#if defined(TARG_NVISA)
/* Because the isa reg_name may not be permanent (when many regs),
 * we have to copy or use it immediately.
 * Rather than alloc space to copy it to the local info struct, 
 * change this to just invoke the isa routine directly
 * (why bother with two copies of the same data?). */
#define REGISTER_name(rclass,reg)				\
	(ISA_REGISTER_CLASS_INFO_Reg_Name(ISA_REGISTER_CLASS_Info(rclass), \
	 reg - REGISTER_MIN))
#else
#define REGISTER_name(rclass,reg)				\
				(REGISTER_CLASS_reg_name(rclass)[reg])
#endif
#define REGISTER_bit_size(rclass,reg)				\
				(REGISTER_CLASS_reg_bit_size(rclass)[reg])


/* Macro to iterate over all members of a REGISTER_SET. */
#define FOR_ALL_REGISTER_SET_members(set,reg)	\
    for (reg = REGISTER_SET_Choose (set);	\
	 reg != REGISTER_UNDEFINED;		\
	 reg = REGISTER_SET_Choose_Next(set,reg))

/* This will hold cached information about each register subclass.  It's
 * not really exported.  See below for exported access macros.
 */
typedef struct {
  const char        *name;
  const char        *reg_name[REGISTER_MAX + 1];
  REGISTER_SET       members;
  ISA_REGISTER_CLASS rclass;
} REGISTER_SUBCLASS_INFO;


/* Cached information about each ISA_REGISTER_SUBCLASS:
 */
extern REGISTER_CLIENT_CONST REGISTER_SUBCLASS_INFO
REGISTER_SUBCLASS_info[ISA_REGISTER_SUBCLASS_MAX + 1];

/* Accessing cached information about a ISA_REGISTER_SUBCLASS
 */
#define REGISTER_SUBCLASS_name(x)			\
				(REGISTER_SUBCLASS_info[x].name)
#define REGISTER_SUBCLASS_members(x)			\
				(REGISTER_SUBCLASS_info[x].members)
#define REGISTER_SUBCLASS_register_class(x)		\
				(REGISTER_SUBCLASS_info[x].rclass)
#define REGISTER_SUBCLASS_reg_name(x,reg)		\
				(REGISTER_SUBCLASS_info[x].reg_name[reg])

/* The various special purpose registers:
 */
extern CLASS_REG_PAIR		CLASS_REG_PAIR_zero;
#define REGISTER_zero		CLASS_REG_PAIR_reg(CLASS_REG_PAIR_zero)
#define REGISTER_CLASS_zero	CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_zero)
#define CLASS_AND_REG_zero	CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_zero)

extern CLASS_REG_PAIR		CLASS_REG_PAIR_ep;
#define REGISTER_ep		CLASS_REG_PAIR_reg(CLASS_REG_PAIR_ep)
#define REGISTER_CLASS_ep	CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_ep)
#define CLASS_AND_REG_ep	CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_ep)

extern CLASS_REG_PAIR		CLASS_REG_PAIR_gp;
#define REGISTER_gp		CLASS_REG_PAIR_reg(CLASS_REG_PAIR_gp)
#define REGISTER_CLASS_gp	CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_gp)
#define CLASS_AND_REG_gp	CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_gp)

extern CLASS_REG_PAIR		CLASS_REG_PAIR_sp;
#define REGISTER_sp		CLASS_REG_PAIR_reg(CLASS_REG_PAIR_sp)
#define REGISTER_CLASS_sp	CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_sp)
#define CLASS_AND_REG_sp	CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_sp)

#ifdef TARG_IA64
extern CLASS_REG_PAIR           CLASS_REG_PAIR_tp;
#define REGISTER_tp             CLASS_REG_PAIR_reg(CLASS_REG_PAIR_tp)
#define REGISTER_CLASS_tp       CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_tp)
#define CLASS_AND_REG_tp        CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_tp)
#endif

extern CLASS_REG_PAIR		CLASS_REG_PAIR_fp;
#define REGISTER_fp		CLASS_REG_PAIR_reg(CLASS_REG_PAIR_fp)
#define REGISTER_CLASS_fp	CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_fp)
#define CLASS_AND_REG_fp	CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_fp)

extern CLASS_REG_PAIR		CLASS_REG_PAIR_ra;
#define REGISTER_ra		CLASS_REG_PAIR_reg(CLASS_REG_PAIR_ra)
#define REGISTER_CLASS_ra	CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_ra)
#define CLASS_AND_REG_ra	CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_ra)

extern CLASS_REG_PAIR		CLASS_REG_PAIR_v0;
#define REGISTER_v0		CLASS_REG_PAIR_reg(CLASS_REG_PAIR_v0)
#define REGISTER_CLASS_v0	CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_v0)
#define CLASS_AND_REG_v0	CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_v0)

#ifdef TARG_X8664
extern CLASS_REG_PAIR		CLASS_REG_PAIR_f0;
#define REGISTER_f0		CLASS_REG_PAIR_reg(CLASS_REG_PAIR_f0)
#define REGISTER_CLASS_f0	CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_f0)
#define CLASS_AND_REG_f0	CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_f0)
#endif

/* The static link may NOT be the same as v0 (i.e. $2) */
extern CLASS_REG_PAIR		CLASS_REG_PAIR_static_link;
#define REGISTER_static_link		CLASS_REG_PAIR_reg(CLASS_REG_PAIR_static_link)
#define REGISTER_CLASS_static_link	CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_static_link)
#define CLASS_AND_REG_static_link	CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_static_link)

extern CLASS_REG_PAIR		CLASS_REG_PAIR_pfs;
#define REGISTER_pfs		CLASS_REG_PAIR_reg(CLASS_REG_PAIR_pfs)
#define REGISTER_CLASS_pfs	CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_pfs)
#define CLASS_AND_REG_pfs	CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_pfs)

extern CLASS_REG_PAIR		CLASS_REG_PAIR_lc;
#define REGISTER_lc		CLASS_REG_PAIR_reg(CLASS_REG_PAIR_lc)
#define REGISTER_CLASS_lc	CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_lc)
#define CLASS_AND_REG_lc	CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_lc)

extern CLASS_REG_PAIR		CLASS_REG_PAIR_ec;
#define REGISTER_ec		CLASS_REG_PAIR_reg(CLASS_REG_PAIR_ec)
#define REGISTER_CLASS_ec	CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_ec)
#define CLASS_AND_REG_ec	CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_ec)

extern CLASS_REG_PAIR		CLASS_REG_PAIR_true;
#define REGISTER_true		CLASS_REG_PAIR_reg(CLASS_REG_PAIR_true)
#define REGISTER_CLASS_true	CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_true)
#define CLASS_AND_REG_true	CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_true)

extern CLASS_REG_PAIR		CLASS_REG_PAIR_fzero;
#define REGISTER_fzero		CLASS_REG_PAIR_reg(CLASS_REG_PAIR_fzero)
#define REGISTER_CLASS_fzero	CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_fzero)
#define CLASS_AND_REG_fzero	CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_fzero)

extern CLASS_REG_PAIR		CLASS_REG_PAIR_fone;
#define REGISTER_fone		CLASS_REG_PAIR_reg(CLASS_REG_PAIR_fone)
#define REGISTER_CLASS_fone	CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_fone)
#define CLASS_AND_REG_fone	CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_fone)

#if defined(TARG_SL)
extern CLASS_REG_PAIR		CLASS_REG_PAIR_k0;
#define REGISTER_k0		CLASS_REG_PAIR_reg(CLASS_REG_PAIR_k0)
#define REGISTER_CLASS_k0	CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_k0)
#define CLASS_AND_REG_k0	CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_k0)
extern CLASS_REG_PAIR           CLASS_REG_PAIR_k1;
#define REGISTER_k1             CLASS_REG_PAIR_reg(CLASS_REG_PAIR_k1)
#define REGISTER_CLASS_k1       CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_k1)
#define CLASS_AND_REG_k1        CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_k1)

//control register ja
extern CLASS_REG_PAIR           CLASS_REG_PAIR_ja;
#define REGISTER_ja             CLASS_REG_PAIR_reg(CLASS_REG_PAIR_ja)
#define REGISTER_CLASS_ja       CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_ja)
#define CLASS_AND_REG_ja        CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_ja)

extern CLASS_REG_PAIR            CLASS_REG_PAIR_lc0;
#define REGISTER_lc0            CLASS_REG_PAIR_reg(CLASS_REG_PAIR_lc0)
#define REGISTER_CLASS_lc0  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_lc0)
#define CLASS_AND_REG_lc0  CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_lc0)

extern CLASS_REG_PAIR            CLASS_REG_PAIR_lc1;
#define REGISTER_lc1            CLASS_REG_PAIR_reg(CLASS_REG_PAIR_lc1)
#define REGISTER_CLASS_lc1  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_lc1)
#define CLASS_AND_REG_lc1  CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_lc1)

extern CLASS_REG_PAIR            CLASS_REG_PAIR_lc2;
#define REGISTER_lc2            CLASS_REG_PAIR_reg(CLASS_REG_PAIR_lc2)
#define REGISTER_CLASS_lc2  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_lc2)
#define CLASS_AND_REG_lc2  CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_lc2)

extern CLASS_REG_PAIR            CLASS_REG_PAIR_lc3;
#define REGISTER_lc3            CLASS_REG_PAIR_reg(CLASS_REG_PAIR_lc3)
#define REGISTER_CLASS_lc3  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_lc3)
#define CLASS_AND_REG_lc3  CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_lc3)

extern CLASS_REG_PAIR      CLASS_REG_PAIR_hi;
#define REGISTER_hi          CLASS_REG_PAIR_reg(CLASS_REG_PAIR_hi)
#define REGISTER_CLASS_hi CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_hi)
#define CLASS_AND_REG_hi  CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_hi)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_acc0;
#define REGISTER_acc0     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_acc0)
#define REGISTER_CLASS_acc0  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_acc0)
#define CLASS_AND_REG_acc0 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_acc0)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_acc1;
#define REGISTER_acc1     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_acc1)
#define REGISTER_CLASS_acc1  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_acc1)
#define CLASS_AND_REG_acc1 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_acc1)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_acc2;
#define REGISTER_acc2     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_acc2)
#define REGISTER_CLASS_acc2  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_acc2)
#define CLASS_AND_REG_acc2 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_acc2)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_acc3;
#define REGISTER_acc3     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_acc3)
#define REGISTER_CLASS_acc3  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_acc3)
#define CLASS_AND_REG_acc3 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_acc3)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_add0;
#define REGISTER_add0     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_add0)
#define REGISTER_CLASS_add0  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_add0)
#define CLASS_AND_REG_add0 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_add0)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_add1;
#define REGISTER_add1     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_add1)
#define REGISTER_CLASS_add1  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_add1)
#define CLASS_AND_REG_add1 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_add1)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_add2;
#define REGISTER_add2     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_add2)
#define REGISTER_CLASS_add2  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_add2)
#define CLASS_AND_REG_add2 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_add2)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_add3;
#define REGISTER_add3     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_add3)
#define REGISTER_CLASS_add3  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_add3)
#define CLASS_AND_REG_add3 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_add3)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_add4;
#define REGISTER_add4     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_add4)
#define REGISTER_CLASS_add4  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_add4)
#define CLASS_AND_REG_add4 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_add4)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_add5;
#define REGISTER_add5     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_add5)
#define REGISTER_CLASS_add5  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_add5)
#define CLASS_AND_REG_add5 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_add5)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_add6;
#define REGISTER_add6     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_add6)
#define REGISTER_CLASS_add6  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_add6)
#define CLASS_AND_REG_add6 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_add6)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_add7;
#define REGISTER_add7     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_add7)
#define REGISTER_CLASS_add7  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_add7)
#define CLASS_AND_REG_add7 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_add7)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_addsize0;
#define REGISTER_addsize0     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_addsize0)
#define REGISTER_CLASS_addsize0  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_addsize0)
#define CLASS_AND_REG_addsize0 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_addsize0)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_addsize1;
#define REGISTER_addsize1     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_addsize1)
#define REGISTER_CLASS_addsize1  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_addsize1)
#define CLASS_AND_REG_addsize1 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_addsize1)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_addsize2;
#define REGISTER_addsize2     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_addsize2)
#define REGISTER_CLASS_addsize2  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_addsize2)
#define CLASS_AND_REG_addsize2 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_addsize2)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_addsize3;
#define REGISTER_addsize3     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_addsize3)
#define REGISTER_CLASS_addsize3  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_addsize3)
#define CLASS_AND_REG_addsize3 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_addsize3)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_addsize4;
#define REGISTER_addsize4     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_addsize4)
#define REGISTER_CLASS_addsize4  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_addsize4)
#define CLASS_AND_REG_addsize4 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_addsize4)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_addsize5;
#define REGISTER_addsize5     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_addsize5)
#define REGISTER_CLASS_addsize5  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_addsize5)
#define CLASS_AND_REG_addsize5 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_addsize5)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_addsize6;
#define REGISTER_addsize6     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_addsize6)
#define REGISTER_CLASS_addsize6  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_addsize6)
#define CLASS_AND_REG_addsize6 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_addsize6)

extern CLASS_REG_PAIR   CLASS_REG_PAIR_addsize7;
#define REGISTER_addsize7     CLASS_REG_PAIR_reg(CLASS_REG_PAIR_addsize7)
#define REGISTER_CLASS_addsize7  CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_addsize7)
#define CLASS_AND_REG_addsize7 CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_addsize7)
#endif // TARG_SL

#ifdef TARG_SL2
extern CLASS_REG_PAIR         CLASS_REG_PAIR_c2acc;
#define REGISTER_c2acc       CLASS_REG_PAIR_reg(CLASS_REG_PAIR_c2acc)
#define REGISTER_CLASS_c2acc CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_c2acc)
#define CLASS_AND_REG_c2acc  CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_c2acc)

extern CLASS_REG_PAIR         CLASS_REG_PAIR_c2cond;
#define REGISTER_c2cond       CLASS_REG_PAIR_reg(CLASS_REG_PAIR_c2cond)
#define REGISTER_CLASS_c2cond CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_c2cond)
#define CLASS_AND_REG_c2cond  CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_c2cond)

extern CLASS_REG_PAIR         CLASS_REG_PAIR_c2mvsel;
#define REGISTER_c2mvsel       CLASS_REG_PAIR_reg(CLASS_REG_PAIR_c2mvsel)
#define REGISTER_CLASS_c2mvsel CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_c2mvsel)
#define CLASS_AND_REG_c2mvsel  CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_c2mvsel)

extern CLASS_REG_PAIR         CLASS_REG_PAIR_c2vlcs;
#define REGISTER_c2vlcs       CLASS_REG_PAIR_reg(CLASS_REG_PAIR_c2vlcs)
#define REGISTER_CLASS_c2vlcs CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_c2vlcs)
#define CLASS_AND_REG_c2vlcs  CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_c2vlcs)

#endif

#ifdef TARG_LOONGSON
extern CLASS_REG_PAIR         CLASS_REG_PAIR_at;
#define REGISTER_at           CLASS_REG_PAIR_reg(CLASS_REG_PAIR_at)
#define REGISTER_CLASS_at     CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_at)
#define CLASS_AND_REG_at      CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_at)

extern CLASS_REG_PAIR         CLASS_REG_PAIR_hi;
#define REGISTER_hi           CLASS_REG_PAIR_reg(CLASS_REG_PAIR_hi)
#define REGISTER_CLASS_hi     CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_hi)
#define CLASS_AND_REG_hi      CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_hi)

extern CLASS_REG_PAIR         CLASS_REG_PAIR_lo;
#define REGISTER_lo           CLASS_REG_PAIR_reg(CLASS_REG_PAIR_lo)
#define REGISTER_CLASS_lo     CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_lo)
#define CLASS_AND_REG_lo      CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_lo)

extern CLASS_REG_PAIR          CLASS_REG_PAIR_fsr;
#define REGISTER_fsr           CLASS_REG_PAIR_reg(CLASS_REG_PAIR_fsr)
#define REGISTER_CLASS_fsr     CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_fsr)
#define CLASS_AND_REG_fsr      CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_fsr)

#endif

extern CLASS_REG_PAIR         CLASS_REG_PAIR_c2movpat;
#define REGISTER_c2movpat       CLASS_REG_PAIR_reg(CLASS_REG_PAIR_c2movpat)
#define REGISTER_CLASS_c2movpat CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_c2movpat)
#define CLASS_AND_REG_c2movpat  CLASS_REG_PAIR_class_n_reg(CLASS_REG_PAIR_c2movpat)

extern const CLASS_REG_PAIR		CLASS_REG_PAIR_undef;

/* Exported functions
 * ==================
 */
extern void
REGISTER_Begin(void);

extern void
REGISTER_Pu_Begin(void);

// possibly reset fp to non-allocatable if need a frame pointer
extern void
REGISTER_Reset_FP (void);

extern void
REGISTER_CLASS_OP_Update_Mapping(
    struct op *op
);


/* The simple functions are defined as inline functions here. The other
 * functions are defined in register.c.
 */

inline BOOL
REGISTER_SET_EqualP(
  REGISTER_SET set1,
  REGISTER_SET set2
)
{
  REGISTER_SET_IDX(i);

  FOR_REGISTER_SET(i = 0, i < MAX_REGISTER_SET_IDX, ++i) {
    if (REGISTER_SET_ELEM(set1, i) != REGISTER_SET_ELEM(set2, i)) {
      return FALSE;
    }
  }
  return    REGISTER_SET_ELEM(set1, MAX_REGISTER_SET_IDX)
         == REGISTER_SET_ELEM(set2, MAX_REGISTER_SET_IDX);
}

inline BOOL
REGISTER_SET_EmptyP(
  REGISTER_SET set
)
{
  REGISTER_SET_IDX(i);

  FOR_REGISTER_SET(i = 0, i < MAX_REGISTER_SET_IDX, ++i) {
    if (REGISTER_SET_ELEM(set, i)) return FALSE;
  }
  return REGISTER_SET_ELEM(set, MAX_REGISTER_SET_IDX) == 0;
}

inline REGISTER_SET
REGISTER_SET_Intersection(
  REGISTER_SET set1,
  REGISTER_SET set2
)
{
  REGISTER_SET result;
  REGISTER_SET_IDX(i);

  FOR_REGISTER_SET(i = 0, i < MAX_REGISTER_SET_IDX, ++i) {
    REGISTER_SET_ELEM(result, i) =
      REGISTER_SET_ELEM(set1, i) & REGISTER_SET_ELEM(set2, i);
  }
  REGISTER_SET_ELEM(result, MAX_REGISTER_SET_IDX) =
      REGISTER_SET_ELEM(set1, MAX_REGISTER_SET_IDX)
    & REGISTER_SET_ELEM(set2, MAX_REGISTER_SET_IDX);

  return result;
}

inline REGISTER_SET
REGISTER_SET_Union(
  REGISTER_SET set1,
  REGISTER_SET set2
)
{
  REGISTER_SET result;
  REGISTER_SET_IDX(i);

  FOR_REGISTER_SET(i = 0, i < MAX_REGISTER_SET_IDX, ++i) {
    REGISTER_SET_ELEM(result, i) =
      REGISTER_SET_ELEM(set1, i) | REGISTER_SET_ELEM(set2, i);
  }
  REGISTER_SET_ELEM(result, MAX_REGISTER_SET_IDX) =
      REGISTER_SET_ELEM(set1, MAX_REGISTER_SET_IDX)
    | REGISTER_SET_ELEM(set2, MAX_REGISTER_SET_IDX);

  return result;
}

inline REGISTER_SET
REGISTER_SET_Difference(
  REGISTER_SET set1,
  REGISTER_SET set2
)
{
  REGISTER_SET result;
  REGISTER_SET_IDX(i);

  FOR_REGISTER_SET(i = 0, i < MAX_REGISTER_SET_IDX, ++i) {
    REGISTER_SET_ELEM(result, i) =
      REGISTER_SET_ELEM(set1, i) & ~REGISTER_SET_ELEM(set2, i);
  }
  REGISTER_SET_ELEM(result, MAX_REGISTER_SET_IDX) =
      REGISTER_SET_ELEM(set1, MAX_REGISTER_SET_IDX)
    & ~REGISTER_SET_ELEM(set2, MAX_REGISTER_SET_IDX);

  return result;
}

inline BOOL
REGISTER_SET_IntersectsP(
  REGISTER_SET set1,
  REGISTER_SET set2
)
{
  REGISTER_SET_IDX(i);

  FOR_REGISTER_SET(i = 0, i < MAX_REGISTER_SET_IDX, ++i) {
    if (REGISTER_SET_ELEM(set1, i) & REGISTER_SET_ELEM(set2, i)) {
      return TRUE;
    }
  }
  return (  REGISTER_SET_ELEM(set1, MAX_REGISTER_SET_IDX)
          & REGISTER_SET_ELEM(set2, MAX_REGISTER_SET_IDX)) != 0;
}

inline BOOL
REGISTER_SET_ContainsP(
  REGISTER_SET set1,
  REGISTER_SET set2
)
{
  REGISTER_SET_IDX(i);

  FOR_REGISTER_SET(i = 0, i < MAX_REGISTER_SET_IDX, ++i) {
    if (REGISTER_SET_ELEM(set2, i) & ~REGISTER_SET_ELEM(set1, i)) {
      return FALSE;
    }
  }
  return (   REGISTER_SET_ELEM(set2, MAX_REGISTER_SET_IDX)
          & ~REGISTER_SET_ELEM(set1, MAX_REGISTER_SET_IDX)) == 0;
}

inline REGISTER_SET
REGISTER_SET_Difference1(
  REGISTER_SET set,
  REGISTER     reg
)
{
  REGISTER_SET result = set;
  INT bit = reg - REGISTER_MIN;
  Is_True((UINT)bit <= (REGISTER_MAX - REGISTER_MIN),
	  ("REGISTER_SET_Difference1: register value out of range"));
  REGISTER_SET_ELEM(result, REGISTER_SET_WORD_IDX(bit)) =
       REGISTER_SET_ELEM(set, REGISTER_SET_WORD_IDX(bit))
    & ~((REGISTER_SET_WORD)1 << REGISTER_SET_BIT_IDX(bit));
  return result;
}

inline REGISTER_SET
REGISTER_SET_Union1(
  REGISTER_SET set,
  REGISTER     reg
)
{
  REGISTER_SET result = set;
  INT bit = reg - REGISTER_MIN;
  Is_True((UINT)bit <= (REGISTER_MAX - REGISTER_MIN),
	  ("REGISTER_SET_Union1: register value out of range"));
  REGISTER_SET_ELEM(result, REGISTER_SET_WORD_IDX(bit)) =
      REGISTER_SET_ELEM(set, REGISTER_SET_WORD_IDX(bit))
    | ((REGISTER_SET_WORD)1 << REGISTER_SET_BIT_IDX(bit));
  return result;
}

inline REGISTER_SET 
REGISTER_SET_Intersection1(
  REGISTER_SET set,
  REGISTER     reg
)
{
  REGISTER_SET result = set;
  INT bit = reg - REGISTER_MIN;
  Is_True((UINT)bit <= (REGISTER_MAX - REGISTER_MIN),
	  ("REGISTER_SET_Intersection1: register value out of range"));
  REGISTER_SET_ELEM(result, REGISTER_SET_WORD_IDX(bit)) =
      REGISTER_SET_ELEM(set, REGISTER_SET_WORD_IDX(bit))
    & ((REGISTER_SET_WORD)1 << REGISTER_SET_BIT_IDX(bit));
  return result;
}

inline BOOL
REGISTER_SET_MemberP(
  REGISTER_SET set,
  REGISTER     reg
)
{
  INT bit = reg - REGISTER_MIN;
  Is_True((UINT)bit <= (REGISTER_MAX - REGISTER_MIN),
	  ("REGISTER_SET_MemberP: register value out of range"));
  return (  REGISTER_SET_ELEM(set, REGISTER_SET_WORD_IDX(bit))
	  & ((REGISTER_SET_WORD)1 << REGISTER_SET_BIT_IDX(bit))) != 0;
}

extern REGISTER_SET
REGISTER_SET_Difference_Range(
  REGISTER_SET   set,
  REGISTER       low,
  REGISTER       high
);

extern REGISTER
REGISTER_SET_Choose(
  REGISTER_SET set
);

extern REGISTER
REGISTER_SET_Choose_Next(
  REGISTER_SET set,
  REGISTER     reg
);

extern REGISTER
REGISTER_SET_Choose_Range(
  REGISTER_SET set,
  REGISTER     low,
  REGISTER     high
);

extern REGISTER
REGISTER_SET_Choose_Intersection(
  REGISTER_SET set1,
  REGISTER_SET set2
);

extern INT32
REGISTER_SET_Size(
  REGISTER_SET set
);

extern void
REGISTER_SET_Print(
  REGISTER_SET  set,
  FILE         *f
);
#pragma mips_frequency_hint NEVER REGISTER_SET_Print

extern void
REGISTER_Print(
  ISA_REGISTER_CLASS  rclass,
  REGISTER            reg,
  FILE               *f
);
#pragma mips_frequency_hint NEVER REGISTER_Print

extern void
CLASS_REG_PAIR_Print(
  CLASS_REG_PAIR crp,
  FILE           *f
);

extern void REGISTER_Trace(
  ISA_REGISTER_CLASS rclass,
  REGISTER           reg
);
#pragma mips_frequency_hint NEVER REGISTER_Trace

extern void REGISTER_CLASS_Trace(
  ISA_REGISTER_CLASS rclass
);
#pragma mips_frequency_hint NEVER REGISTER_CLASS_Trace

extern void REGISTER_CLASS_Trace_All(void);

extern void REGISTER_Set_Allocatable(
  ISA_REGISTER_CLASS rclass,
  REGISTER           reg,
  BOOL               is_allocatable
);

					  
#define CLASS_REG_PAIR_EqualP(crp1,crp2)	\
	  (CLASS_REG_PAIR_class_n_reg(crp1) ==	\
	   CLASS_REG_PAIR_class_n_reg(crp2))


extern REGISTER_SET REGISTER_SET_Range(UINT low, UINT high);

// user wants given register to not be allocatable in file.
extern void Set_Register_Never_Allocatable (char *regname);
extern void Set_Register_Never_Allocatable (PREG_NUM preg);

inline ISA_REGISTER_CLASS Register_Class_For_Mtype(TYPE_ID mtype)
{
  extern mISA_REGISTER_CLASS Mtype_RegClass_Map[MTYPE_LAST+1];
  return   (mtype <= MTYPE_LAST)
	 ? (ISA_REGISTER_CLASS)Mtype_RegClass_Map[mtype] 
	 : ISA_REGISTER_CLASS_UNDEFINED;
}

extern void Init_Mtype_RegClass_Map(void);

#include "register_targ.h"

#endif /* REGISTER_INCLUDED */
